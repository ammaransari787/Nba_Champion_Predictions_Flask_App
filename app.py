from flask import Flask, flash, redirect, render_template, request, session
from flask_session import Session

from helpers import *

import nba_api
from nba_api.stats.static import teams
from nba_api.stats.endpoints import leaguegamefinder
import pandas 
import numpy as np
import statsmodels.api as sm
from sklearn.model_selection import GridSearchCV
from sklearn.linear_model import Ridge, RidgeCV, Lasso, LassoCV, LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import ExtraTreesRegressor, RandomForestRegressor, RandomForestClassifier
from sklearn.feature_selection import SelectFromModel, SelectPercentile, f_regression
from sklearn.pipeline import make_pipeline, make_union
from sklearn.metrics import mean_squared_error
from tpot.builtins import StackingEstimator
from tpot.export_utils import set_param_recursive
import gunicorn

# Configure application
app = Flask(__name__)

# Ensure templates are auto-reloaded
app.config["TEMPLATES_AUTO_RELOAD"] = True

# Configure session to use filesystem (instead of signed cookies)
app.config["SESSION_PERMANENT"] = False
app.config["SESSION_TYPE"] = "filesystem"
Session(app)

@app.route('/', methods = ['GET', 'POST'])
def index():
    if request.method == "POST":
        return apology('apologies', 403)
    else:
        return render_template('index.html')

        # current_year_test_data = pull_from_json("current_year_test_data.json")
        # current_year_test_data["TEAM_NAME"] = current_year_test_data.index

        # json_file = current_year_test_data.to_json(orient='records')

        # json_file = json.loads(json_file)

        # return render_template('data.html', json_file = json_file, name = "Data")
    

@app.route("/data", methods = ["GET", 'POST'])
def data():
    if request.method == "POST":
        return apology('apologies', 403)

    else:
        current_year_test_data = pull_from_json("current_year_test_data.json")
        current_year_test_data["TEAM_NAME"] = current_year_test_data.index

        json_file = current_year_test_data.to_json(orient='records')

        json_file = json.loads(json_file)

        return render_template('test_data.html', json_file = json_file, name = "Data")


@app.route('/logistic', methods = ['GET', 'POST'])
def logistic():
    if request.method == 'POST':
        return apology('apologies', 403)
    
    else:
        if should_we_query_API():
            pull_data()

        X_sk = pull_from_json('X_sk.json')
        y_sm = pull_from_json("y_sk.json", "Series")
        current_year_test_data = pull_from_json("current_year_test_data.json")

        X_sm = X_sk.loc[:,['FG_PCT', 'STL', 'W_PCT']]

        current_year_test_data = current_year_test_data.loc[:,['FG_PCT', 'STL', 'W_PCT']]

        glm_fit_sm = sm.Logit(y_sm, sm.add_constant(X_sm)).fit()
        current_year_predictions = glm_fit_sm.predict(sm.add_constant(current_year_test_data))

        json_file = current_year_predictions.sort_values(ascending=False).to_json()

        return render_template('chart.html', json_file = json_file, name = "Logistic Regression")
    

@app.route('/lasso', methods = ["GET", "POST"])
def lasso():
    if request.method == "POST":
        return apology('apologies', 403)

    else:
        if should_we_query_API():
            pull_data()

        X_sk = pull_from_json('X_sk.json')
        y_sk = pull_from_json("y_sk.json", "Series")
        current_year_test_data = pull_from_json("current_year_test_data.json")

        param_grid = {'C': [0.001, 0.01, 0.1, 1, 10, 100, 1000] }

        glm_model = LogisticRegression(fit_intercept=True, penalty="l1", solver='liblinear') # regularization
        clf = GridSearchCV(glm_model, param_grid)
        clf.fit(X_sk, y_sk)

        optimal_c = clf.best_score_

        glm_model = LogisticRegression(fit_intercept=True, C = optimal_c, penalty="l1", solver='liblinear') # regularization
        glm_fit = glm_model.fit(X_sk, y_sk)
        # print(pandas.Series(glm_fit.coef_[0], index=X_sk.columns))

        ret_df = pandas.Series(data=glm_fit.predict_proba(current_year_test_data)[:, 1], index=current_year_test_data.index)
        json_file = ret_df.sort_values(ascending=False).to_json()

        return render_template('chart.html', json_file = json_file, name = "LASSO Logistic Regression")


@app.route('/stability_selection_to_logistic', methods = ['GET', 'POST'])
def stability_selection_to_logistic():
    if request.method == 'POST':
        return apology('apologies', 403)
    else:
        if should_we_query_API():
            pull_data()

        X_sk = pull_from_json('X_sk.json')
        y_sm = pull_from_json("y_sk.json", "Series")
        current_year_test_data = pull_from_json("current_year_test_data.json")

        X_sm = X_sk.loc[:,["FG_PCT", "AST", "STL", "BLK", "W_PCT"]]

        current_year_test_data = current_year_test_data.loc[:,["FG_PCT", "AST", "STL", "BLK", "W_PCT"]]

        glm_fit_sm = sm.Logit(y_sm, sm.add_constant(X_sm)).fit()
        current_year_predictions = glm_fit_sm.predict(sm.add_constant(current_year_test_data))

        json_file = current_year_predictions.sort_values(ascending=False).to_json()

        return render_template('chart.html', json_file = json_file, name = "Stability Selection to Logistic Regression")


@app.route('/stability_selection_to_lasso', methods = ["GET", "POST"])
def stability_selection_lasso():
    if request.method == "POST":
        return apology('apologies', 403)

    else:
        if should_we_query_API():
            pull_data()

        X_sk = pull_from_json('X_sk.json')
        y_sk = pull_from_json("y_sk.json", "Series")
        current_year_test_data = pull_from_json("current_year_test_data.json")

        X_sk = X_sk.loc[:,["FG_PCT", "AST", "STL", "BLK", "W_PCT"]]
        current_year_test_data = current_year_test_data.loc[:,["FG_PCT", "AST", "STL", "BLK", "W_PCT"]]

        param_grid = {'C': [0.001, 0.01, 0.1, 1, 10, 100, 1000] }
        glm_model = LogisticRegression(fit_intercept=True) # regularization
        clf = GridSearchCV(glm_model, param_grid)
        clf.fit(X_sk, y_sk)
        # print(clf.best_score_)
        optimal_c = clf.best_score_

        glm_model = LogisticRegression(fit_intercept=True, C = optimal_c) # regularization
        glm_fit = glm_model.fit(X_sk, y_sk)

        ret_df = pandas.Series(data=glm_fit.predict_proba(current_year_test_data)[:, 1], index=current_year_test_data.index)
        json_file = ret_df.sort_values(ascending=False).to_json()

        return render_template('chart.html', json_file = json_file, name = "Stability Selection To LASSO Logistic Regression")


@app.route('/k_nearest_neighbors', methods = ["GET", "POST"])
def k_nearest_neightbors():
    if request.method == "POST":
        return apology('apologies', 403)

    else:
        if should_we_query_API():
            pull_data()

        X_sk = pull_from_json('X_sk.json')
        y_sk = pull_from_json("y_sk.json", "Series")
        current_year_test_data = pull_from_json("current_year_test_data.json")

        knn_model = KNeighborsClassifier(n_neighbors=11)
        knn_fit = knn_model.fit(X_sk, y_sk)

        ret_df = pandas.Series(data=knn_fit.predict_proba(current_year_test_data)[:, 1], index=current_year_test_data.index)
        json_file = ret_df.sort_values(ascending=False).to_json()

        return render_template('chart.html', json_file = json_file, name = "K Nearest Neighbors")


@app.route('/random_forest', methods = ["GET", "POST"])
def random_forest():
    if request.method == "POST":
        return apology('apologies', 403)

    else:
        if should_we_query_API():
            pull_data()

        X_sk = pull_from_json('X_sk.json')
        y_sk = pull_from_json("y_sk.json", "Series")
        current_year_test_data = pull_from_json("current_year_test_data.json")

        random_forest_ = RandomForestClassifier(max_depth=5, random_state=42)
        random_forest_fit = random_forest_.fit(X_sk, y_sk)

        ret_df = pandas.Series(data=random_forest_fit.predict_proba(current_year_test_data)[:, 1], index=current_year_test_data.index)
        json_file = ret_df.sort_values(ascending=False).to_json()

        return render_template('chart.html', json_file = json_file, name = "Random Forest")


@app.route('/tpot', methods = ["GET", "POST"])
def tpot():
    if request.method == "POST":
        return apology('apologies', 403)

    else:
        if should_we_query_API():
            pull_data()

        X_sk = pull_from_json('X_sk.json')
        y_sk = pull_from_json("y_sk_tpot.json", "Series")
        current_year_test_data = pull_from_json("current_year_test_data.json")

        # Average CV score on the training set was: -4.439670407988454
        exported_pipeline = make_pipeline(
            SelectPercentile(score_func=f_regression, percentile=18),
            StackingEstimator(estimator=ExtraTreesRegressor(bootstrap=True, max_features=0.35000000000000003, min_samples_leaf=18, min_samples_split=15, n_estimators=100)),
            SelectFromModel(estimator=ExtraTreesRegressor(max_features=0.8500000000000001, n_estimators=100), threshold=0.1),
            RandomForestRegressor(bootstrap=False, max_features=0.8500000000000001, min_samples_leaf=18, min_samples_split=6, n_estimators=100)
        )
        # Fix random state for all the steps in exported pipeline
        set_param_recursive(exported_pipeline.steps, 'random_state', 42)

        tpot_fit = exported_pipeline.fit(X_sk, y_sk)

        ret_df = pandas.Series(data=tpot_fit.predict(current_year_test_data), index=current_year_test_data.index)
        json_file = ret_df.sort_values(ascending=False).to_json()

        return render_template('tpot_chart.html', json_file = json_file, name = "TPOT")


@app.route('/log_transformed_outcome_lasso_regression', methods = ["GET", "POST"])
def log_transformed_outcome_lasso_regression():
    if request.method == "POST":
        return apology('apologies', 403)
    else:
        if should_we_query_API():
            pull_data()

        X_sk = pull_from_json('X_sk.json')
        y_sk = pull_from_json("y_sk_tpot.json", "Series")
        current_year_test_data = pull_from_json("current_year_test_data.json")

        # log transforming playoff wins, then scaling so max is 1 (scaling is actually unnecessary here since I don't need scaling to better consider priors as I did when I calculated this with bayesian linear regression, but just did it anyways bc it's not hurting anything)
        y_sk = np.log(y_sk + 1) / np.log(16 + 1) 
        grid = 10 ** np.linspace(3,-2,100)
        lasso_cv = LassoCV(alphas=grid, cv=10)#,max_iter=100000,)
        lasso_cv.fit(X_sk,y_sk)
        lasso2 = Lasso(alpha=lasso_cv.alpha_)
        lasso2_fit = lasso2.fit(X_sk,y_sk)
        # print(pandas.Series(lasso2_fit.coef_, index=X_sk.columns))

        # converting predictions back to natural scale of playoff wins:
        ret_df = pandas.Series(data=(np.exp(lasso2_fit.predict(current_year_test_data)*np.log(16 + 1))-1), index=current_year_test_data.index)
        json_file = ret_df.sort_values(ascending=False).to_json()
        return render_template('tpot_chart.html', json_file = json_file, name = "log_transformed_outcome LASSO linear regression")
    

@app.route('/bayesian_regression', methods = ["GET", "POST"])
def bayesian_regression():
    if request.method == "POST":
        return apology('apologies', 403)
    else:
        # Returning the shiny app, that can pull data in during season as well
        return render_template("test_shiny_plot.html", name = 'Bayesian Credible Intervals') # 99% credible intervals ggplot 

        # if should_we_query_API():
        #     pull_data()

        # X_sk = pull_from_json('X_sk.json')
        # y_sk = pull_from_json("y_sk_tpot.json", "Series")
        # current_year_test_data = pull_from_json("current_year_test_data.json")

        # # log transforming playoff wins, then scaling so max is 1
        # y_sk = np.log(y_sk + 1) / np.log(16 + 1) 

        # """
        # Commented out pystan code after caching posterior samples,
        # because c++ compiler causes fly.io server to consume 
        # too much memory and crash, both with pystan and cmdstanpy
        # """

        # # Commented out pystan code: 

        # # z_score_PLAYOFF_WINS_list = []
        # # z_score_FT_PCT_list = []
        # # z_score_NUM_MVP_PLAYERS_list = []
        # # z_score_W_PCT_list = []
        # # z_score_E_REB_PCT_list = []

        # # for i in range(len(y_sk)):
        # #     z_score_PLAYOFF_WINS_list.append(y_sk[i])
        # #     z_score_FT_PCT_list.append(X_sk["FT_PCT"][i])
        # #     z_score_NUM_MVP_PLAYERS_list.append(X_sk["NUM_MVP_PLAYERS"][i])
        # #     z_score_W_PCT_list.append(X_sk["W_PCT"][i])
        # #     z_score_E_REB_PCT_list.append(X_sk["E_REB_PCT"][i])

        # # dat_slim_2 = {
        # #     "z_score_PLAYOFF_WINS": z_score_PLAYOFF_WINS_list,
        # #     "z_score_FT_PCT": z_score_FT_PCT_list,
        # #     "z_score_NUM_MVP_PLAYERS": z_score_NUM_MVP_PLAYERS_list,
        # #     "z_score_W_PCT": z_score_W_PCT_list,
        # #     "z_score_E_REB_PCT": z_score_E_REB_PCT_list,
        # # }

        # # stan_code_2 = """
        # # data{
        # #     vector[120] z_score_PLAYOFF_WINS;
        # #     vector[120] z_score_E_REB_PCT;
        # #     vector[120] z_score_W_PCT;
        # #     vector[120] z_score_NUM_MVP_PLAYERS;
        # #     vector[120] z_score_FT_PCT;
        # # }
        # # parameters{
        # #     real a;
        # #     real bFT;
        # #     real bMVP;
        # #     real bWP;
        # #     real bER;
        # #     real<lower=0> sigma;
        # # }
        # # model{
        # #     vector[120] mu;
        # #     sigma ~ exponential( 1 );
        # #     bER ~ normal( 0 , 0.125 );
        # #     bWP ~ normal( 0 , 0.125 );
        # #     bMVP ~ normal( 0 , 0.125 );
        # #     bFT ~ normal( 0 , 0.125 );
        # #     a ~ normal( 0 , 0.2 );
        # #     for ( i in 1:120 ) {
        # #         mu[i] = a + bFT * z_score_FT_PCT[i] + bMVP * z_score_NUM_MVP_PLAYERS[i] + bWP * z_score_W_PCT[i] + bER * z_score_E_REB_PCT[i];
        # #     }
        # #     z_score_PLAYOFF_WINS ~ normal( mu , sigma );
        # # }
        # # """

        # # posterior = stan.build(stan_code_2, data=dat_slim_2, random_seed=1)

        # # fit = posterior.sample(num_chains=4, num_samples=1000)

        # # df = fit.to_frame()
        # # # print(df.describe().T)

        # # df.to_csv('cached_posterior_samples.csv')

        # df = pandas.read_csv("cached_posterior_samples.csv")

        # def sim_(post, test_data):
        #     predictions_ = pandas.DataFrame()
        #     for j in range(len(test_data)):
        #         this_column_ = []
        #         for i in range(len(post)):
        #             this_column_.append(np.random.normal(loc=post['a'][i] + post['bFT'][i]*test_data['FT_PCT'][j] + post['bMVP'][i]*test_data['NUM_MVP_PLAYERS'][j] + post['bWP'][i] * test_data['W_PCT'][j] + post["bER"][i]*test_data["E_REB_PCT"][j], scale=post['sigma'][i], size = 1)[0])
        #         predictions_[f"{test_data.index[j]}"] = this_column_

        #     return predictions_
        
        # sim_playoff_wins = sim_(df, current_year_test_data)

        # sim_mean = pandas.DataFrame()
        
        # sim_mean_list_ = []
        # sim_name_list = []

        # sim_one_point_five_list_ = []
        # sim_ninety_eight_point_five_list_ = []

        # for i in range(len(sim_playoff_wins.axes[1])):
        #     sim_name_list.append(sim_playoff_wins.axes[1][i])
        #     mean_mu = np.mean(sim_playoff_wins[f"{sim_playoff_wins.axes[1][i]}"])
        #     transformed_mean_mu = (np.exp(mean_mu * np.log(17))) - 1
        #     sim_mean_list_.append(transformed_mean_mu)
        #     # sim_mean_list_.append(mean_mu)

        #     # untransformed_one_point_five = np.percentile(sim_playoff_wins[f"{sim_playoff_wins.axes[1][i]}"], 1.5) # 97% credible interval 
        #     untransformed_one_point_five = np.percentile(sim_playoff_wins[f"{sim_playoff_wins.axes[1][i]}"], 0.5) # 99% credible interval 

        #     sim_one_point_five_list_.append(np.exp(untransformed_one_point_five * np.log(17)) - 1)

        #     # untransformed_ninety_eight_point_five = np.percentile(sim_playoff_wins[f"{sim_playoff_wins.axes[1][i]}"], 98.5) # 97% credible interval 
        #     untransformed_ninety_eight_point_five = np.percentile(sim_playoff_wins[f"{sim_playoff_wins.axes[1][i]}"], 99.5) # 99% credible interval 

        #     sim_ninety_eight_point_five_list_.append(np.exp(untransformed_ninety_eight_point_five * np.log(17)) - 1)


        # sim_mean["name"] = sim_name_list

        # sim_mean["mean"] = sim_mean_list_

        # print(sim_mean.sort_values(by='mean', ascending=False))

        # sim_CI = pandas.DataFrame()

        # sim_CI['name'] = sim_name_list

        # # sim_CI['lower_bound_97_credible_interval'] = sim_one_point_five_list_
        # # sim_CI['upper_bound_97_credible_interval'] = sim_ninety_eight_point_five_list_

        # sim_CI['lower_bound_99_credible_interval'] = sim_one_point_five_list_
        # sim_CI['upper_bound_99_credible_interval'] = sim_ninety_eight_point_five_list_

        # # print(sim_CI.sort_values(by= "upper_bound_97_credible_interval", ascending= False))
        # print(sim_CI.sort_values(by= "upper_bound_99_credible_interval", ascending= False))

        # ret_df = pandas.Series(data=sim_mean_list_, index=sim_name_list)
        # json_file = ret_df.sort_values(ascending=False).to_json()

        # return render_template('tpot_chart.html', json_file = json_file, name = "Bayesian Linear Regression")

        # # return render_template("bayes_plot.html") # 99% credible intervals ggplot 
        