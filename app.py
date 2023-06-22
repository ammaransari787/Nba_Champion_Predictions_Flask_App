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
