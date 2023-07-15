big_mac_no_index_df = read.csv("big_mac_no_index.csv")
# view(big_mac_no_index_df)

full_NBA_df <- read.csv("full_nba.csv")

historical_train_data <- read.csv("real_hist_train_data.csv")

historical_train_data_subset <- subset(historical_train_data, select = c(PTS,                      FGM,                       FGA               ,       
                                                                          FG_PCT       ,             FG3M ,                    FG3A              ,       
                                                                          FG3_PCT     ,             FTM,                      FTA                 ,     
                                                                          FT_PCT    ,                OREB            ,          DREB               ,      
                                                                          REB       ,                AST            ,           STL                 ,     
                                                                          BLK     ,                  TOV                       ,PF                   ,    
                                                                          PLUS_MINUS   ,       TEAM_NAME, IS_CHAMPION,   PLAYOFF_WINS,  NUM_ALL_DEFENSIVE_PLAYERS, NUM_ALL_STAR_PLAYERS  ,   
                                                                          NUM_DPOY_PLAYERS ,         NUM_MVP_PLAYERS  ,         W_PCT                  ,  
                                                                          E_OFF_RATING     ,         E_DEF_RATING    ,          E_NET_RATING            , 
                                                                          E_PACE        ,            E_AST_RATIO    ,           E_OREB_PCT               ,
                                                                          E_DREB_PCT   ,             E_REB_PCT      ,           E_TM_TOV_PCT             ,
                                                                          eFG_PCT                ))



games_2223c_df_NEW <- read.csv('games_2223c.csv')


games_2223c_df_NEW_subset <- subset(games_2223c_df_NEW, select = c(PTS,                      FGM,                       FGA               ,       
                                                                         FG_PCT       ,             FG3M ,                    FG3A              ,       
                                                                         FG3_PCT     ,             FTM,                      FTA                 ,     
                                                                         FT_PCT    ,                OREB            ,          DREB               ,      
                                                                         REB       ,                AST            ,           STL                 ,     
                                                                         BLK     ,                  TOV                       ,PF                   ,    
                                                                         PLUS_MINUS   ,       TEAM_NAME,  NUM_ALL_DEFENSIVE_PLAYERS, NUM_ALL_STAR_PLAYERS  ,   
                                                                         NUM_DPOY_PLAYERS ,         NUM_MVP_PLAYERS  ,         W_PCT                  ,  
                                                                         E_OFF_RATING     ,         E_DEF_RATING    ,          E_NET_RATING            , 
                                                                         E_PACE        ,            E_AST_RATIO    ,           E_OREB_PCT               ,
                                                                         E_DREB_PCT   ,             E_REB_PCT      ,           E_TM_TOV_PCT             ,
                                                                         eFG_PCT                ))

OG_NUM_COL = ncol(full_NBA_df)

i = 1
while (i <= OG_NUM_COL) 
{
  placeholder = rep(0, nrow(full_NBA_df))
  full_NBA_df = cbind(full_NBA_df, placeholder)
  z = ncol(full_NBA_df)
  col_name = colnames(full_NBA_df)[i]
  full_NBA_df$placeholder = standardize(full_NBA_df[[i]])
  colnames(full_NBA_df)[z] = glue("z_score_{col_name}")
  i = i + 1
}

full_NBA_df = historical_train_data_subset

OG_NUM_COL = ncol(full_NBA_df)

i = 1
while (i <= OG_NUM_COL) 
{
  if (colnames(full_NBA_df)[i] != 'IS_CHAMPION' && colnames(full_NBA_df)[i] != 'TEAM_NAME')
  {
    placeholder = rep(0, nrow(full_NBA_df))
    full_NBA_df = cbind(full_NBA_df, placeholder)
    z = ncol(full_NBA_df)
    col_name = colnames(full_NBA_df)[i]
    full_NBA_df$placeholder = standardize(full_NBA_df[[i]])
    colnames(full_NBA_df)[z] = glue("z_score_{col_name}")
    i = i + 1
  }
  else
  {
    i = i + 1
  }
}


# games_2223c_df_NEW_subset = games_2223c_df_NEW_subset

OG_NUM_COL = ncol(games_2223c_df_NEW_subset)

i = 1
while (i <= OG_NUM_COL) 
{
  if (colnames(games_2223c_df_NEW_subset)[i] != 'IS_CHAMPION' && colnames(games_2223c_df_NEW_subset)[i] != 'TEAM_NAME')
  {
    placeholder = rep(0, nrow(games_2223c_df_NEW_subset))
    games_2223c_df_NEW_subset = cbind(games_2223c_df_NEW_subset, placeholder)
    z = ncol(games_2223c_df_NEW_subset)
    col_name = colnames(games_2223c_df_NEW_subset)[i]
    games_2223c_df_NEW_subset$placeholder = standardize(games_2223c_df_NEW_subset[[i]])
    colnames(games_2223c_df_NEW_subset)[z] = glue("z_score_{col_name}")
    i = i + 1
  }
  else
  {
    i = i + 1
  }
}


big_mac_w_index_df = read.csv("big_mac_w_index.csv")
# view(big_mac_w_index_df)

games_2223c_df = read.csv('games_2223c_no_index.csv')

# ((FGM + (0.5 * 3PM)) / FGA

big_mac_no_index_df$eFG_PCT = (big_mac_no_index_df$FGM + (0.5 * big_mac_no_index_df$FG3M)) / big_mac_no_index_df$FGA

big_mac_no_index_df$IS_CHAMPION <- ifelse(big_mac_no_index_df$IS_CHAMPION == "True", 1, 0)

# gonna try lasso regression here and see what it says, wayyy after the fact  

predictor_list = c()
counter = 0
i = 1
while (i <= ncol(full_NBA_df))
{
  if (str_sub(colnames(full_NBA_df)[i], 1, 1) == "z"  && colnames(full_NBA_df)[i] != "z_score_PLAYOFF_WINS")
  {
    counter = counter + 1
    predictor_list[counter] = colnames(full_NBA_df)[i]
  }
  i = i + 1
}


#define response variable
# y <- big_mac_no_index_df$IS_CHAMPION
full_NBA_df$IS_CHAMPION_dummy <- ifelse(full_NBA_df$IS_CHAMPION == TRUE, 1, 0)
y <- full_NBA_df$IS_CHAMPION_dummy
y <- full_NBA_df$z_score_PLAYOFF_WINS
y <- full_NBA_df$PLAYOFF_WINS # stopped here 
y <- full_NBA_df_copy$scaled_max_1_PLAYOFF_WINS

#define matrix of predictor variables
# x <- data.matrix(per_game_reg_season_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])
# x <- data.matrix(big_mac_no_index_df[, c('WIN_PCT', "PLUS_MINUS", "PF","TOV","BLK" ,"STL","AST", "REB", "DREB", "OREB", "FT_PCT", "FTA", "FTM", "FG3_PCT", "FG3A", "FG3M", "FG_PCT", "FGA", "FGM", "PTS", "eFG_PCT")])
x <- data.matrix(full_NBA_df_copy[,c("z_score_PTS"             ,          "z_score_FGM"        ,              
                                 "z_score_FGA"           ,            "z_score_FG_PCT"   ,                
                                "z_score_FG3M"          ,            "z_score_FG3A"     ,                
                                "z_score_FG3_PCT"      ,             "z_score_FTM"     ,                 
                                "z_score_FTA"         ,              "z_score_FT_PCT" ,                  
                                "z_score_OREB"       ,               "z_score_DREB"  ,                   
                                "z_score_REB"       ,                "z_score_AST"  ,                    
                                "z_score_STL"      ,                 "z_score_BLK" ,                     
                                 "z_score_TOV"    ,                   "z_score_PF",                       
                                 "z_score_PLUS_MINUS"           ,     "z_score_NUM_ALL_DEFENSIVE_PLAYERS",
                                 "z_score_NUM_ALL_STAR_PLAYERS",      "z_score_NUM_DPOY_PLAYERS",         
                                 "z_score_NUM_MVP_PLAYERS"    ,       "z_score_W_PCT"         ,           
                                 "z_score_E_OFF_RATING"      ,        "z_score_E_DEF_RATING" ,            
                                 "z_score_E_NET_RATING"     ,         "z_score_E_PACE"     ,              
                                 "z_score_E_AST_RATIO"              , "z_score_E_OREB_PCT" ,              
                                 "z_score_E_DREB_PCT"              ,  "z_score_E_REB_PCT" ,               
                                 "z_score_E_TM_TOV_PCT"           ,   "z_score_eFG_PCT")])
# all test? below
# x <- data.matrix(per_game_reg_season_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])
# didn't work ^ 


#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda


#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
best_model$beta


# Stability Selection
stab.glmnet <- stabsel(x = x, y =  y, q = 11, B=300,
                       fitfun = glmnet.lasso,
                       args.fitfun = list(type = "conservative"),
                       cutoff = 0.6)
print(stab.glmnet$selected)
print(sort(stab.glmnet$max))

# doing stabsel with q = sqrt(.8p) as mentioned in paper - I like p//3 (above) better tho (what ISLR recommends for regression RF, which random forest is similar is in spirit to stability selection so makes sense. Paper for stabsel even says q choice isn't critical so makes sense I can pick something else I like better.)
ncol(x) # 34
sqrt(.8*34) # 5.215362

stab.glmnet <- stabsel(x = x, y =  y, q = 6, B=300,
                       fitfun = glmnet.lasso,
                       args.fitfun = list(type = "conservative"),
                       cutoff = 0.6)
print(stab.glmnet$selected)
print(sort(stab.glmnet$max))



# prediciton with initial lasso 

games_2223c_df$eFG_PCT = (games_2223c_df$FGM + (0.5 * games_2223c_df$FG3M)) / games_2223c_df$FGA

new_x <- data.matrix(games_2223c_df_NEW_subset[, c('WIN_PCT', "PLUS_MINUS", "PF","TOV","BLK" ,"STL","AST", "REB", "DREB", "OREB", "FT_PCT", "FTA", "FTM", "FG3_PCT", "FG3A", "FG3M", "FG_PCT", "FGA", "FGM", "PTS", "eFG_PCT")])
og_games_2223c_df_NEW_subset <- subset(games_2223c_df_NEW_subset, select = -c(playoff_predictions))
new_x <- data.matrix(og_games_2223c_df_NEW_subset[,predictor_list])

games_2223c_df_NEW_subset$lasso_predictions_log <- predict (best_model , s = best_lambda ,
                                          newx = new_x)

# games_2223c_df_cbinded_1 = cbind(games_2223c_df, prediction_from_initial_lasso)
# 
# view(games_2223c_df_cbinded_1)

lass_log_ <- subset(games_2223c_df_NEW_subset, select = c(TEAM_NAME, lasso_predictions_log))

lass_log_$projected_playoff_wins <- exp(lass_log_$lasso_predictions_log * log(16 + 1)) - 1

#view(lass_log_)

# Stability Selection lasso 

#define response variable
y <- big_mac_no_index_df$IS_CHAMPION

#define matrix of predictor variables
# x <- data.matrix(per_game_reg_season_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])
# x <- data.matrix(big_mac_no_index_df[, c('WIN_PCT', "PLUS_MINUS", "PF","TOV","BLK" ,"STL","AST", "REB", "DREB", "OREB", "FT_PCT", "FTA", "FTM", "FG3_PCT", "FG3A", "FG3M", "FG_PCT", "FGA", "FGM", "PTS")])
x <- data.matrix(big_mac_no_index_df[, c("STL", "BLK", "FG_PCT", "TOV", "WIN_PCT")])


# all test? below
# x <- data.matrix(per_game_reg_season_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])
# didn't work ^ 


#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda


#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda, family = "binomial")
coef(best_model)
best_model$beta


# prediction_w_stability_selection_lasso : 

new_x <- data.matrix(full_NBA_df[, c("z_score_FT_PCT", "z_score_NUM_ALL_DEFENSIVE_PLAYERS", "z_score_NUM_ALL_STAR_PLAYERS", "z_score_W_PCT", "z_score_E_REB_PCT")])

prediction_from_stability_selection_lasso <- predict (best_model , s = best_lambda ,
                                                      newx = new_x)

games_2223c_df_cbinded_2 = cbind(games_2223c_df, prediction_from_stability_selection_lasso)

view(games_2223c_df_cbinded_2)

# LM using the stability selection 

stabs_lm <- lm(LAYOFF_WINS ~ z_score_FT_PCT + z_score_NUM_ALL_DEFENSIVE_PLAYERS + z_score_NUM_ALL_STAR_PLAYERS + z_score_W_PCT +z_score_E_REB_PCT ,data = full_NBA_df)
summary(stabs_lm)

scaled_outcome_stabs_lm <- lm(z_score_PLAYOFF_WINS ~ z_score_FT_PCT + z_score_NUM_ALL_DEFENSIVE_PLAYERS + z_score_NUM_ALL_STAR_PLAYERS + z_score_W_PCT +z_score_E_REB_PCT ,data = full_NBA_df)
summary(scaled_outcome_stabs_lm)

predict(stabs_lm, games_2223c_df_NEW_subset)

games_2223c_df_NEW_subset$stabs_playoff_predictions = predict(stabs_lm, games_2223c_df_NEW_subset)

# THIS ONE IS LM AFTER STAB SELECTION WITH NEW VARS - CRAZY GOOD ! 
stabs_lm_playoff_predictions_unscaled = subset(games_2223c_df_NEW_subset, select = c(TEAM_NAME, stabs_playoff_predictions))

# lasso version
lasso_playoff_win_predictiosn = subset(games_2223c_df_NEW_subset, select = c(TEAM_NAME, lasso_predictions))


# Testing using scaled log playoff wins as predictor 



# NEW *** Bayesian linear regression and ulam 

dat_slim <-list(
  z_score_PLAYOFF_WINS = full_NBA_df$z_score_PLAYOFF_WINS,
  z_score_FT_PCT = full_NBA_df$z_score_FT_PCT,
  z_score_NUM_ALL_DEFENSIVE_PLAYERS = full_NBA_df$z_score_NUM_ALL_DEFENSIVE_PLAYERS,
  z_score_NUM_ALL_STAR_PLAYERS = full_NBA_df$z_score_NUM_ALL_STAR_PLAYERS,
  z_score_W_PCT = full_NBA_df$z_score_W_PCT,
  z_score_E_REB_PCT = full_NBA_df$z_score_E_REB_PCT
)
str(dat_slim)

# Stability Selection Selected 
bayes_stabs <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dnorm(mu, sigma),
    mu <- a + bFT*z_score_FT_PCT + bDP*z_score_NUM_ALL_DEFENSIVE_PLAYERS + bAS*z_score_NUM_ALL_STAR_PLAYERS + bWP*z_score_W_PCT+ bER*z_score_E_REB_PCT,
    a ~ dnorm(0, 0.2),
    bFT ~ dnorm(0, 0.5),
    bDP ~ dnorm(0, 0.5),
    bAS ~ dnorm(0, 0.5),
    bWP ~ dnorm(0, 0.5),
    bER ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

bayes_robust_stabs <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dstudent(2, mu, sigma),
    mu <- a + bFT*z_score_FT_PCT + bDP*z_score_NUM_ALL_DEFENSIVE_PLAYERS + bAS*z_score_NUM_ALL_STAR_PLAYERS + bWP*z_score_W_PCT+ bER*z_score_E_REB_PCT,
    a ~ dnorm(0, 0.2),
    bFT ~ dnorm(0, 0.5),
    bDP ~ dnorm(0, 0.5),
    bAS ~ dnorm(0, 0.5),
    bWP ~ dnorm(0, 0.5),
    bER ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

precis(bayes_stabs)

plot(precis(bayes_stabs))

PSIS(bayes_stabs)

plot(PSIS(bayes_stabs, pointwise = TRUE)$k)

precis(bayes_robust_stabs)
plot(precis(bayes_robust_stabs))
PSIS(bayes_robust_stabs)

compare(bayes_stabs, bayes_robust_stabs, func = 'PSIS')
plot(compare(bayes_stabs, bayes_robust_stabs, func = 'PSIS'))

# 99% Credible Interval of difference b/w models
18.3 + c(-1,1)*16.4*2.6 # -24.34  60.94

# 97% Credible Interval of Difference 
18.3 + c(-1,1)*16.4*1.881 # -12.5484  49.1484

# 89% Credible Interval of Difference 
18.3 + c(-1,1)*16.4*1.227 # -1.8228 38.4228


# vs an ulam model with all of the ones selected by lasso regression: 
# I'll do compare on them

dat_slim <-list(
  z_score_PLAYOFF_WINS = full_NBA_df$z_score_PLAYOFF_WINS,
  z_score_FT_PCT = full_NBA_df$z_score_FT_PCT,
  z_score_NUM_ALL_DEFENSIVE_PLAYERS = full_NBA_df$z_score_NUM_ALL_DEFENSIVE_PLAYERS,
  z_score_NUM_ALL_STAR_PLAYERS = full_NBA_df$z_score_NUM_ALL_STAR_PLAYERS,
  z_score_W_PCT = full_NBA_df$z_score_W_PCT,
  z_score_E_REB_PCT = full_NBA_df$z_score_E_REB_PCT,
  z_score_E_TM_TOV_PCT = full_NBA_df$z_score_E_TM_TOV_PCT, 
  z_score_E_PACE = full_NBA_df$z_score_E_PACE,
  z_score_NUM_MVP_PLAYERS = full_NBA_df$z_score_NUM_MVP_PLAYERS,
  z_score_FG3M = full_NBA_df$z_score_FG3M,
  z_score_FG3A = full_NBA_df$z_score_FG3A
)

bayes_lasso <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dnorm(mu, sigma),
    mu <- a + bET*z_score_E_TM_TOV_PCT + bEP*z_score_E_PACE + bNM*z_score_NUM_MVP_PLAYERS + bFG3M*z_score_FG3M + bFG3A*z_score_FG3A + bFT*z_score_FT_PCT + bDP*z_score_NUM_ALL_DEFENSIVE_PLAYERS + bAS*z_score_NUM_ALL_STAR_PLAYERS + bWP*z_score_W_PCT+ bER*z_score_E_REB_PCT,
    a ~ dnorm(0, 0.2),
    bET ~ dnorm(0, 0.5),
    bEP ~ dnorm(0, 0.5),
    bNM ~ dnorm(0, 0.5),
    bFG3A ~ dnorm(0, 0.5),
    bFG3M ~ dnorm(0, 0.5),
    bFT ~ dnorm(0, 0.5),
    bDP ~ dnorm(0, 0.5),
    bAS ~ dnorm(0, 0.5),
    bWP ~ dnorm(0, 0.5),
    bER ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

bayes_robust_lasso <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dstudent(2, mu, sigma),
    mu <- a + bET*z_score_E_TM_TOV_PCT + bEP*z_score_E_PACE + bNM*z_score_NUM_MVP_PLAYERS + bFG3M*z_score_FG3M + bFG3A*z_score_FG3A + bFT*z_score_FT_PCT + bDP*z_score_NUM_ALL_DEFENSIVE_PLAYERS + bAS*z_score_NUM_ALL_STAR_PLAYERS + bWP*z_score_W_PCT+ bER*z_score_E_REB_PCT,
    a ~ dnorm(0, 0.2),
    bET ~ dnorm(0, 0.5),
    bEP ~ dnorm(0, 0.5),
    bNM ~ dnorm(0, 0.5),
    bFG3A ~ dnorm(0, 0.5),
    bFG3M ~ dnorm(0, 0.5),
    bFT ~ dnorm(0, 0.5),
    bDP ~ dnorm(0, 0.5),
    bAS ~ dnorm(0, 0.5),
    bWP ~ dnorm(0, 0.5),
    bER ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

plot(precis(bayes_robust_lasso))

compare(bayes_lasso, bayes_robust_lasso, func = 'PSIS')
plot(compare(bayes_lasso, bayes_robust_lasso, func = 'PSIS'))

# 99% credible interval of difference 
23.5 + c(-1, 1) * 16.27 * 2.6 # -18.802  65.802

# 97% credible interval of difference 
23.5 + c(-1, 1) * 16.27 * 1.881 # -7.10387 54.10387

# 89 % Credible Interval
23.5 + c(-1, 1) * 16.27 * 1.227 # 3.53671 43.46329

# So for lasso, definitely better using robust, (unlike stabs)
# at the 89% credible interval. 
# Makes sense, as the k values for lasso were above .8,
# whereas for stabs was around .5 (not above .7)

plot(PSIS(bayes_lasso, pointwise = TRUE)$k)

# Comparing all of them: 

compare(bayes_stabs, bayes_robust_stabs, bayes_lasso, bayes_robust_lasso, func = 'PSIS')
plot(compare(bayes_stabs, bayes_robust_stabs, bayes_lasso, bayes_robust_lasso, func = 'PSIS'))

# Robust lasso is no better than robust stabs:
# makes sense, as stabs ones were the ones with sig parameters
1.5 + c(-1, 1) * 7.27 * 1.227 # -7.42029 10.42029

plot(precis(bayes_robust_stabs))

# 89% 
19.9 + c(-1, 1) * 15.84 * 1.227 # 0.46432 39.33568

# At 89% credible interval, robust lasso selected is slightly better predictions than regular bayes stabs, but not robust stabs


mu <- link(bayes_lasso_v2_bc_log)
mu <- link(bayes_stabs_v2_less)
mu <- link(bayes_stabs_v2_least)

mu <- test_preds_link

set.seed(123)
mu <- test_preds_sim


# summaraize samples across cases
mu_mean <- apply(mu, 2, mean)
# mu_PI <- apply(mu, 2, PI)
mu_PI <- apply(mu, 2, PI, 0.99)

# For my rolling own sim below, but I went with rethinking sim (test_preds_sim)
# mu_mean = mu.mean
# mu_PI = mu.PI

# mu_PI = mu.HPDI

# simulate observations
# again no new data, so uses original data
P_sim <- sim(bayes_robust_stabs, n=1e4)
P_PI <- apply(P_sim, 2, PI)

# # Plot predictions against observed, with a line to show perfect prediction 
# # and line segments for the confidence interval of each prediction 
# plot(mu_mean ~ full_NBA_df$z_score_PLAYOFF_WINS, col = rangi2, ylim=range(mu_PI), xlab="Observed playoffwins", ylab = "Predicted playoff wins")
# abline(a=0, b=1, lty=2)
# for (i in 1:nrow(full_NBA_df)) lines(rep(full_NBA_df$z_score_PLAYOFF_WINS[i],2), mu_PI[,i], col = rangi2)
# 
# identify(x=full_NBA_df$z_score_PLAYOFF_WINS, y=mu_mean, labels = full_NBA_df$TEAM_NAME)


# Plot predictions against observed, with a line to show perfect prediction 
# and line segments for the confidence interval of each prediction 
plot(mu_mean ~ full_NBA_df_copy$scaled_max_1_PLAYOFF_WINS, col = rangi2, ylim=range(mu_PI), xlab="Observed playoffwins", ylab = "Predicted playoff wins")
abline(a=0, b=1, lty=2)
for (i in 1:nrow(full_NBA_df_copy)) lines(rep(full_NBA_df_copy$scaled_max_1_PLAYOFF_WINS[i],2), mu_PI[,i], col = rangi2)

identify(x=full_NBA_df_copy$scaled_max_1_PLAYOFF_WINS, y=mu_mean, labels = full_NBA_df_copy$TEAM_NAME)



length(mu_mean)

test_preds_plus_games_2223_10 <- cbind(games_2223c_df_NEW_subset, mu_mean)

test_preds_plus_games_2223_10 <- cbind(test_preds_plus_games_2223_10, mu_PI[1,])
test_preds_plus_games_2223_10 <- cbind(test_preds_plus_games_2223_10, mu_PI[2,])

# colnames(test_preds_plus_games_2223_7)[75] = 'five_percent'
# colnames(test_preds_plus_games_2223_7)[76] = 'ninety_four_percent'

colnames(test_preds_plus_games_2223_10)[75] = 'point_five_percent'
colnames(test_preds_plus_games_2223_10)[76] = 'ninety_nine_point_five_percent'

# test_preds_subset_8 <- subset(test_preds_plus_games_2223_9, select = c(TEAM_NAME, mu_mean, five_percent, ninety_four_percent))

test_preds_subset_9 <- subset(test_preds_plus_games_2223_10, select = c(TEAM_NAME, mu_mean, point_five_percent, ninety_nine_point_five_percent))


# view(test_preds_subset)

# test_preds_subset_3$natural_scale_playoff_wins <- exp(test_preds_subset_3$mu_mean * log(17)) - 1
# test_preds_subset_3$natural_scale_five_percent <- exp(test_preds_subset_3$five_percent * log(17)) - 1
# test_preds_subset_3$natural_scale_ninety_four_percent <- exp(test_preds_subset_3$ninety_four_percent * log(17)) - 1
# test_preds_subset_3$size_of_credible_interval <- abs(abs(test_preds_subset_3$natural_scale_ninety_four_percent) - abs(test_preds_subset_3$natural_scale_five_percent))

test_preds_subset_9$natural_scale_playoff_wins <- exp(test_preds_subset_9$mu_mean * log(17)) - 1
test_preds_subset_9$natural_scale_point_five_percent <- exp(test_preds_subset_9$point_five_percent * log(17)) - 1
test_preds_subset_9$natural_scale_ninety_nine_point_five_percent <- exp(test_preds_subset_9$ninety_nine_point_five_percent * log(17)) - 1
test_preds_subset_9$size_of_credible_interval <- abs(abs(test_preds_subset_9$natural_scale_ninety_nine_point_five_percent) - abs(test_preds_subset_9$natural_scale_point_five_percent))


# link predictions (also see above where i went after making link, back up to mu)

dat_slim <-list(
  z_score_FT_PCT = games_2223c_df_NEW_subset$z_score_FT_PCT,  
  z_score_NUM_ALL_DEFENSIVE_PLAYERS = games_2223c_df_NEW_subset$z_score_NUM_ALL_DEFENSIVE_PLAYERS,  
  z_score_W_PCT = games_2223c_df_NEW_subset$z_score_W_PCT,  
  z_score_E_TM_TOV_PCT = games_2223c_df_NEW_subset$z_score_E_TM_TOV_PCT,  
  z_score_NUM_MVP_PLAYERS = games_2223c_df_NEW_subset$z_score_NUM_MVP_PLAYERS, 
  z_score_FG3A = games_2223c_df_NEW_subset$z_score_FG3A,  
  z_score_FGA = games_2223c_df_NEW_subset$z_score_FGA,  
  z_score_FTM = games_2223c_df_NEW_subset$z_score_FTM,  
  z_score_STL = games_2223c_df_NEW_subset$z_score_STL, 
  z_score_E_AST_RATIO = games_2223c_df_NEW_subset$z_score_E_AST_RATIO,  
  z_score_E_OREB_PCT = games_2223c_df_NEW_subset$z_score_E_OREB_PCT,  
  z_score_E_REB_PCT = games_2223c_df_NEW_subset$z_score_E_REB_PCT  
)


test_preds_link <- link(bayes_lasso_v2_bc_log, data = data.frame(
                                                             z_score_FT_PCT = games_2223c_df_NEW_subset$z_score_FT_PCT,  
                                                             z_score_NUM_ALL_DEFENSIVE_PLAYERS = games_2223c_df_NEW_subset$z_score_NUM_ALL_DEFENSIVE_PLAYERS,  
                                                             z_score_W_PCT = games_2223c_df_NEW_subset$z_score_W_PCT,  
                                                             z_score_E_TM_TOV_PCT = games_2223c_df_NEW_subset$z_score_E_TM_TOV_PCT,  
                                                             z_score_NUM_MVP_PLAYERS = games_2223c_df_NEW_subset$z_score_NUM_MVP_PLAYERS, 
                                                             z_score_FG3A = games_2223c_df_NEW_subset$z_score_FG3A,  
                                                             z_score_FGA = games_2223c_df_NEW_subset$z_score_FGA,  
                                                             z_score_FTM = games_2223c_df_NEW_subset$z_score_FTM,  
                                                             z_score_STL = games_2223c_df_NEW_subset$z_score_STL, 
                                                             z_score_E_AST_RATIO = games_2223c_df_NEW_subset$z_score_E_AST_RATIO,  
                                                             z_score_E_OREB_PCT = games_2223c_df_NEW_subset$z_score_E_OREB_PCT,  
                                                             z_score_E_REB_PCT = games_2223c_df_NEW_subset$z_score_E_REB_PCT ))

# doing bayes less now here: 

dat_link <-list(
  z_score_FT_PCT = games_2223c_df_NEW_subset$z_score_FT_PCT,
  z_score_NUM_MVP_PLAYERS = games_2223c_df_NEW_subset$z_score_NUM_MVP_PLAYERS,
  z_score_W_PCT = games_2223c_df_NEW_subset$z_score_W_PCT,
  z_score_E_REB_PCT = games_2223c_df_NEW_subset$z_score_E_REB_PCT
)

test_preds_link <- link(bayes_stabs_v2_less, data = dat_link)

set.seed(123)
test_preds_sim <- sim(bayes_stabs_v2_less, data = dat_link, n = 1e5)
test_preds_sim <- sim(bayes_lasso_v2_bc_log, data = dat_slim, n = 1e5)

# Rolling own sim 

post <- extract.samples(bayes_stabs_v2_less, n = 10000)

# test.function <- `[[`

# sim.pw <- sapply(games_2223c_df_NEW_subset, function(test_data) rnorm(n = nrow(post), mean = post$a + post$bFT * test_data[,45]+ post$bNM * test_data[,58]+ post$bWP * test_data[,59] + post$bER * test_data[,67] , sd = post$sigma))

# def sim_(post, test_data):
#   predictions_ = pd.DataFrame()
#   for j in range(len(test_data)):
#     this_column_ = []
#     for i in range(len(post)):
#       this_column_.append(np.random.normal(loc=post['a'][i] + post['bFT'][i]*test_data['z_score_FT_PCT'][j] + post['bMVP'][i]*test_data['z_score_NUM_MVP_PLAYERS'][j] + post['bWP'][i] * test_data['z_score_W_PCT'][j] + post["bER"][i]*test_data["z_score_E_REB_PCT"][j], scale=post['sigma'][i], size = 1)[0])
#     predictions_[f"{test_data['TEAM_NAME'][j]}"] = this_column_

#   return predictions_

sim_ <- function(post, test_data) {
  predictions_ = list()
  
  for (j in 1:nrow(test_data))
  {
    this_column = rep(0, length(post$a))
    for (i in 1:length(post$a))
    {
      this_column[i] <- rnorm(1, mean = ( post$a[i] + post$bFT[i]*test_data$z_score_FT_PCT[j] + post$bMVP[i]*test_data$z_score_NUM_MVP_PLAYERS[j] + post$bWP[i] * test_data$z_score_W_PCT[j] + post$bER[i]*test_data$z_score_E_REB_PCT[j] ), sd = post$sigma)
    }
    col_name = glue("{test_data$TEAM_NAME[j]}")
    predictions_[[col_name]] = this_column
  }
  return(as.data.frame(predictions_))
}

set.seed(123)

sim_pw <- sim_(post, games_2223c_df_NEW_subset)


mu.mean <- apply(sim_pw, 2, mean)
mu.PI <- apply(sim_pw, 2, PI, prob = 0.99)
mu.HPDI <- apply(sim_pw, 2, HPDI, prob = 0.97)

# NOTE: This model (less) (test_preds_subset_2) is actually really good. 
# I need to in some way split into west and east before finals.
# Teams need to have better chances based on the weakness of their opponents. 
# TODO NEW Figure out a way to bring that into the model! 

# TODO NEW ALSO use 97 PI instead of 89 and use that in my df

# TODO NEW Use sim here with PI like I did in Python script !!!


# TODO NEW ALSO REMEMBER TO COME BACK AND USE A ZERO INFLATED! 




# log playoff wins ? 

full_NBA_df_copy = full_NBA_df

full_NBA_df_copy$log_PLAYOFF_WINS <- log(full_NBA_df_copy$PLAYOFF_WINS + 1)
# 
# full_NBA_df_copy$z_score_log_PLAYOFF_WINS <- standardize(full_NBA_df_copy$log_PLAYOFF_WINS)
# 
full_NBA_df_copy$scaled_max_1_PLAYOFF_WINS <- full_NBA_df_copy$log_PLAYOFF_WINS / max(full_NBA_df_copy$log_PLAYOFF_WINS)



dat_slim <-list(
  z_score_PLAYOFF_WINS = full_NBA_df_copy$scaled_max_1_PLAYOFF_WINS,
  z_score_FT_PCT = full_NBA_df_copy$z_score_FT_PCT,
  z_score_NUM_ALL_DEFENSIVE_PLAYERS = full_NBA_df_copy$z_score_NUM_ALL_DEFENSIVE_PLAYERS,
  z_score_NUM_ALL_STAR_PLAYERS = full_NBA_df_copy$z_score_NUM_ALL_STAR_PLAYERS,
  z_score_W_PCT = full_NBA_df_copy$z_score_W_PCT,
  z_score_E_REB_PCT = full_NBA_df_copy$z_score_E_REB_PCT
)
str(dat_slim)

bayes_stabs_first_log <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dnorm(mu, sigma),
    mu <- a + bFT*z_score_FT_PCT + bDP*z_score_NUM_ALL_DEFENSIVE_PLAYERS + bAS*z_score_NUM_ALL_STAR_PLAYERS + bWP*z_score_W_PCT+ bER*z_score_E_REB_PCT,
    a ~ dnorm(0, 0.2),
    bFT ~ dnorm(0, 0.125),
    bDP ~ dnorm(0, 0.125),
    bAS ~ dnorm(0, 0.125),
    bWP ~ dnorm(0, 0.125),
    bER ~ dnorm(0, 0.125),
    # sigma ~ dexp(1)
    sigma ~ dunif(0,3)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

plot(PSIS(bayes_stabs_first_log, pointwise = TRUE)$k)

compare(bayes_stabs_first_log, bayes_robust_lasso)

plot(compare(bayes_stabs_first_log, bayes_robust_lasso, func = "PSIS"))

# Considering the priors:

# I'll adapt this section below, from text, to this situation.
# What about those slopes? 
# What would a very strong effect of water and shade look like? 
# How big could those slopes be in theory? 
# The range of both water and shade is 2 - 
# from -1 to 1 is 2 units. 
# To take us from the theoretical minimum of zero blooms on one end to the observed maximum of 1 - 
# a range of 1 unit - on the other would require a slope of 0.5 from either variable - 
# 0.5 x 2 = 1. 
# So if we assign a standard deviation of .25 to each, 
# then 95% of the prior slopes are from -0.5 to 0.5, 
# so either variable could in principle account for the entire range, 
# but it would be unlikely. 
# Remember, the goals here are to assign weakly informative priors to discourage overfitting - 
# impossibly large effects should be assigned low prior probability - 
# and also to force ourselves to think about waht the model means. 

# Considering the priors:

# I'll adapt this section below, from text, to this situation.
# What about those slopes? 
# What would a very strong effect of water and shade look like? 
# How big could those slopes be in theory? 
# The range of both water and shade is 4 - 
# from -2 to 2 is 4 units. 
# To take us from the theoretical minimum of zero blooms on one end to the observed maximum of 1 - 
# a range of 1 unit - on the other would require a slope of 0.5 from either variable - 
# 0.25 x 4 = 1. 
# So if we assign a standard deviation of .125 to each, 
# then 95% of the prior slopes are from -0.5 to 0.5, 
# so either variable could in principle account for the entire range, 
# but it would be unlikely. 
# Remember, the goals here are to assign weakly informative priors to discourage overfitting - 
# impossibly large effects should be assigned low prior probability - 
# and also to force ourselves to think about waht the model means. 


# Couple new stabs to test after switching to the better log version: 


# First one with more: 

# z_score_FT_PCT z_score_NUM_ALL_DEFENSIVE_PLAYERS 
# 10                                20 
# z_score_NUM_MVP_PLAYERS                     z_score_W_PCT 
# 23                                24 
# z_score_E_REB_PCT 
# 32 

dat_slim <-list(
  z_score_PLAYOFF_WINS = full_NBA_df_copy$scaled_max_1_PLAYOFF_WINS,
  z_score_FT_PCT = full_NBA_df_copy$z_score_FT_PCT,
  z_score_NUM_ALL_DEFENSIVE_PLAYERS = full_NBA_df_copy$z_score_NUM_ALL_DEFENSIVE_PLAYERS,
  z_score_NUM_MVP_PLAYERS = full_NBA_df_copy$z_score_NUM_MVP_PLAYERS,
  z_score_W_PCT = full_NBA_df_copy$z_score_W_PCT,
  z_score_E_REB_PCT = full_NBA_df_copy$z_score_E_REB_PCT
)
str(dat_slim)

bayes_stabs_v2_more <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dnorm(mu, sigma),
    mu <- a + bFT*z_score_FT_PCT + bDP*z_score_NUM_ALL_DEFENSIVE_PLAYERS + bMVP*z_score_NUM_MVP_PLAYERS + bWP*z_score_W_PCT+ bER*z_score_E_REB_PCT,
    a ~ dnorm(0, 0.2),
    bFT ~ dnorm(0, 0.125),
    bDP ~ dnorm(0, 0.125),
    bMVP ~ dnorm(0, 0.125),
    bWP ~ dnorm(0, 0.125),
    bER ~ dnorm(0, 0.125),
    # sigma ~ dexp(1)
    sigma ~ dunif(0,3)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

plot(compare(bayes_stabs_v2_more, bayes_stabs_first_log, func = "PSIS"))

compare(bayes_stabs_v2_more, bayes_stabs_first_log, func = "PSIS")

# No real difference even at the 89% credible interval level b/w first log and v2
2.8 + c(-1,1)*4.25*1.227 # -2.41475  8.01475

plot(PSIS(bayes_robust_stabs_v2_more, pointwise = TRUE)$k)


bayes_robust_stabs_v2_more <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dstudent(2, mu, sigma),
    mu <- a + bFT*z_score_FT_PCT + bDP*z_score_NUM_ALL_DEFENSIVE_PLAYERS + bMVP*z_score_NUM_MVP_PLAYERS + bWP*z_score_W_PCT+ bER*z_score_E_REB_PCT,
    a ~ dnorm(0, 0.2),
    bFT ~ dnorm(0, 0.125),
    bDP ~ dnorm(0, 0.125),
    bMVP ~ dnorm(0, 0.125),
    bWP ~ dnorm(0, 0.125),
    bER ~ dnorm(0, 0.125),
    # sigma ~ dexp(1)
    sigma ~ dunif(0,3)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

plot(precis(bayes_robust_stabs_v2_more))
plot(precis(bayes_stabs_first_log))

compare(bayes_stabs_first_log, bayes_stabs_v2_more, bayes_robust_stabs_v2_more, func = "PSIS")

plot(compare(bayes_stabs_first_log, bayes_stabs_v2_more, bayes_robust_stabs_v2_more, func = "PSIS"))


# Then one with less: 
dat_slim <-list(
  z_score_PLAYOFF_WINS = full_NBA_df_copy$scaled_max_1_PLAYOFF_WINS,
  z_score_FT_PCT = full_NBA_df_copy$z_score_FT_PCT,
  z_score_NUM_MVP_PLAYERS = full_NBA_df_copy$z_score_NUM_MVP_PLAYERS,
  z_score_W_PCT = full_NBA_df_copy$z_score_W_PCT,
  z_score_E_REB_PCT = full_NBA_df_copy$z_score_E_REB_PCT
)
str(dat_slim)

set.seed(1)
bayes_stabs_v2_less <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dnorm(mu, sigma),
    mu <- a + bFT*z_score_FT_PCT + bMVP*z_score_NUM_MVP_PLAYERS + bWP*z_score_W_PCT+ bER*z_score_E_REB_PCT,
    a ~ dnorm(0, 0.2),
    bFT ~ dnorm(0, 0.125),
    bMVP ~ dnorm(0, 0.125),
    bWP ~ dnorm(0, 0.125),
    bER ~ dnorm(0, 0.125),
    sigma ~ dexp(1)
    # sigma ~ dunif(0,3)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

plot(precis(bayes_stabs_v2_less))

PSIS(bayes_stabs_v2_less)

compare(bayes_stabs_v2_less, bayes_stabs_v2_more, bayes_stabs_first_log, func = 'PSIS')

plot(compare(bayes_stabs_v2_less, bayes_stabs_v2_more, bayes_stabs_first_log, func = 'PSIS'))


# Then one with LEAST (q = sqrt(.8p))

dat_slim <-list(
  z_score_PLAYOFF_WINS = full_NBA_df_copy$scaled_max_1_PLAYOFF_WINS,
  z_score_FT_PCT = full_NBA_df_copy$z_score_FT_PCT,
  z_score_W_PCT = full_NBA_df_copy$z_score_W_PCT
)
str(dat_slim)

bayes_stabs_v2_least <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dnorm(mu, sigma),
    mu <- a + bFT*z_score_FT_PCT + bWP*z_score_W_PCT,
    a ~ dnorm(0, 0.2),
    bFT ~ dnorm(0, 0.125),
    bWP ~ dnorm(0, 0.125),
    # sigma ~ dexp(1)
    sigma ~ dunif(0,3)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

PSIS(bayes_stabs_v2_least)

plot(compare(bayes_stabs_v2_least, bayes_stabs_v2_less, bayes_stabs_v2_more, bayes_stabs_first_log, func = 'PSIS'))

compare(bayes_stabs_v2_least, bayes_stabs_v2_less, bayes_stabs_v2_more, bayes_stabs_first_log, func = 'PSIS')

# Can't say at 89% credible interval that lest is worse than less
5.7 + c(-1, 1)*6.88*1.227 # -2.74176 14.14176

compare(bayes_lasso_v2_bc_log, bayes_stabs_v2_least, bayes_stabs_v2_less, bayes_stabs_v2_more, bayes_stabs_first_log, func = 'PSIS')
plot(compare(bayes_lasso_v2_bc_log, bayes_stabs_v2_least, bayes_stabs_v2_less, bayes_stabs_v2_more, bayes_stabs_first_log, func = 'PSIS'))

# Can't say at 89% credible interval that lest is worse than lasso
7.7 + c(-1,1)*9.84*1.227 # -4.37368 19.77368


# Then lasso (after rerunning lasso): 

# 
# z_score_FGA                       -0.006314892
# z_score_FG3A                       0.018623265
# z_score_FTM                        0.005935783
# z_score_FT_PCT                     0.072229985
# z_score_STL                        0.004762903
# z_score_NUM_ALL_DEFENSIVE_PLAYERS  0.030821863
# z_score_NUM_MVP_PLAYERS            0.023833481
# z_score_W_PCT                      0.217454757
# z_score_E_AST_RATIO               -0.032551989
# z_score_E_OREB_PCT                 0.012940642
# z_score_E_REB_PCT                  0.031892255
# z_score_E_TM_TOV_PCT               0.017268137



# Python selected variables: 
# FGA                         -0.005895
# FG3A                         0.018403
# FTM                          0.005853
# FT_PCT                       0.071320
# STL                          0.004379
# NUM_ALL_DEFENSIVE_PLAYERS    0.030422
# NUM_MVP_PLAYERS              0.023517
# W_PCT                        0.216456
# E_AST_RATIO                 -0.031873
# E_OREB_PCT                   0.012418
# E_REB_PCT                    0.031756
# E_TM_TOV_PCT                 0.016816

# Wow, exactly the same between Python and R lasso - nice! 



dat_slim <-list(
  z_score_PLAYOFF_WINS = full_NBA_df_copy$scaled_max_1_PLAYOFF_WINS,
  z_score_FT_PCT = full_NBA_df_copy$z_score_FT_PCT,  
  z_score_NUM_ALL_DEFENSIVE_PLAYERS = full_NBA_df_copy$z_score_NUM_ALL_DEFENSIVE_PLAYERS,  
  z_score_W_PCT = full_NBA_df_copy$z_score_W_PCT,  
  z_score_E_TM_TOV_PCT = full_NBA_df_copy$z_score_E_TM_TOV_PCT,  
  z_score_NUM_MVP_PLAYERS = full_NBA_df_copy$z_score_NUM_MVP_PLAYERS, 
  z_score_FG3A = full_NBA_df_copy$z_score_FG3A,  
  z_score_FGA = full_NBA_df_copy$z_score_FGA,  
  z_score_FTM = full_NBA_df_copy$z_score_FTM,  
  z_score_STL = full_NBA_df_copy$z_score_STL, 
  z_score_E_AST_RATIO = full_NBA_df_copy$z_score_E_AST_RATIO,  
  z_score_E_OREB_PCT = full_NBA_df_copy$z_score_E_OREB_PCT,  
  z_score_E_REB_PCT = full_NBA_df_copy$z_score_E_REB_PCT  
)

bayes_lasso_v2_bc_log <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dnorm(mu, sigma),
    mu <- a + bFT*z_score_FT_PCT + bDP*z_score_NUM_ALL_DEFENSIVE_PLAYERS + bWP*z_score_W_PCT + bET*z_score_E_TM_TOV_PCT + bNM*z_score_NUM_MVP_PLAYERS + bFG3A*z_score_FG3A + bFGA*z_score_FGA + bFTM*z_score_FTM + bSTL*z_score_STL + bEAR*z_score_E_AST_RATIO + bEORP*z_score_E_OREB_PCT + bER*z_score_E_REB_PCT,
    a ~ dnorm(0, 0.2),
    bFT ~ dnorm(0, 0.125),
    bDP ~ dnorm(0, 0.125),
    bWP ~ dnorm(0, 0.125),
    bET ~ dnorm(0, 0.125),
    bNM ~ dnorm(0, 0.125),
    bFG3A ~ dnorm(0, 0.125),
    bFGA ~ dnorm(0, 0.125),
    bFTM ~ dnorm(0, 0.125),
    bSTL ~ dnorm(0, 0.125),
    bEAR ~ dnorm(0, 0.125),
    bEORP ~ dnorm(0, 0.125),
    bER ~ dnorm(0, 0.125),
    sigma ~ dexp(1)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

plot(precis(bayes_lasso_v2_bc_log))

PSIS(bayes_lasso_v2_bc_log)

plot(PSIS(bayes_lasso_v2_bc_log, pointwise = TRUE)$k)
# Not crazy high, just .5

# I'll just do a robust one in case and compare them to all 
bayes_robust_lasso_v2_bc_log <- ulam(
  alist(
    z_score_PLAYOFF_WINS ~ dstudent(2, mu, sigma),
    mu <- a + bFT*z_score_FT_PCT + bDP*z_score_NUM_ALL_DEFENSIVE_PLAYERS + bWP*z_score_W_PCT + bET*z_score_E_TM_TOV_PCT + bNM*z_score_NUM_MVP_PLAYERS + bFG3A*z_score_FG3A + bFGA*z_score_FGA + bFTM*z_score_FTM + bSTL*z_score_STL + bEAR*z_score_E_AST_RATIO + bEORP*z_score_E_OREB_PCT + bER*z_score_E_REB_PCT,
    a ~ dnorm(0, 0.2),
    bFT ~ dnorm(0, 0.125),
    bDP ~ dnorm(0, 0.125),
    bWP ~ dnorm(0, 0.125),
    bET ~ dnorm(0, 0.125),
    bNM ~ dnorm(0, 0.125),
    bFG3A ~ dnorm(0, 0.125),
    bFGA ~ dnorm(0, 0.125),
    bFTM ~ dnorm(0, 0.125),
    bSTL ~ dnorm(0, 0.125),
    bEAR ~ dnorm(0, 0.125),
    bEORP ~ dnorm(0, 0.125),
    bER ~ dnorm(0, 0.125),
    # sigma ~ dexp(1)
    sigma ~ dunif(0, 3)
  ), data = dat_slim, chains = 4, cores = 4, log_lik = TRUE
)

precis()

plot(compare(bayes_robust_lasso_v2_bc_log, bayes_lasso_v2_bc_log,bayes_stabs_first_log ,bayes_stabs_v2_less,bayes_stabs_v2_more ,func = "PSIS"))

compare(bayes_robust_lasso_v2_bc_log, bayes_lasso_v2_bc_log,bayes_stabs_first_log ,bayes_stabs_v2_less,bayes_stabs_v2_more ,func = "PSIS")

# 99% credibility interval of diff between top and second models
2.4  + c(-1, 1)*7.11*2.6 # -16.086  20.886

# 97% credibility interval 
2.4  + c(-1, 1)*7.11*1.881 # -10.97391  15.77391

# 89% credibility interval 
2.4  + c(-1, 1)*7.11*1.227 # -6.32397 11.12397

# Virtually no difference between top 3 models here 

# 89% credibility interval of diff between #1 and #4 
5.9 + c(-1, 1) * 6.35  * 1.227 # -1.89145 13.69145

# Top 4 models all have 0 in the 89% credible interval of difference as well 

# TODO Now to input these into python app - prob do LASSO first - DONE with LASSO! Next, pyMC? 

# Stability Selction logistic 

stability_selection_logistic = glm(data = big_mac_no_index_df, formula = IS_CHAMPION ~ STL + BLK + WIN_PCT + TOV + FG_PCT, family = binomial(link='logit') )

odds_stabs_logistic_2 <- predict(stability_selection_logistic, games_2223c_df, type = "response")

games_2223c_df_cbinded_4 = cbind(games_2223c_df, odds_stabs_logistic_2)

view(games_2223c_df_cbinded_4)


# using previous prediction model 


# rename colnames to be correct 

copy_games_2223c_df = games_2223c_df

# colnames(copy_games_2223c_df[25]) = 'pctWins'
# 
# colnames(copy_games_2223c_df[6]) = 'pctFG'
# 
# colnames(copy_games_2223c_df[17]) = 'stl'

copy_games_2223c_df$pctWins = copy_games_2223c_df$WIN_PCT

copy_games_2223c_df$pctFG = copy_games_2223c_df$FG_PCT

copy_games_2223c_df$stl = copy_games_2223c_df$STL

odds_prediction_model <- predict(prediction_model, copy_games_2223c_df, type = "response")

games_2223c_df_cbinded_5 = cbind(games_2223c_df, odds_prediction_model)

view(games_2223c_df_cbinded_5)


# adding the new year to it 

view(per_game_reg_season_test_year_2022)

per_game_reg_season_after_fact_2022 = per_game_reg_season_test_year_2022


per_game_reg_season_after_fact_2022$isNBAChampion = ifelse(per_game_reg_season_after_fact_2022$teamName == "Warriors", 1, 0)

view(per_game_reg_season_after_fact_2022)

per_game_reg_season_after_fact_2022 = subset(per_game_reg_season_after_fact_2022, select = -c(odds, odds.1, odds_new, odds_new_2))


# view(per_game_reg_season_current)

copyper_game_reg_season_current = subset(per_game_reg_season_current, select = -c(modeSearch, idTeam, nameTeam, cityTeam))


after_fact_current = rbind(copyper_game_reg_season_current, per_game_reg_season_after_fact_2022 )




# now do all the lasso etc w after_fact_current 

# gonna try lasso regression here and see what it says, wayyy after the fact  

#define response variable
y <- after_fact_current$isNBAChampion

#define matrix of predictor variables
x <- data.matrix(after_fact_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])
# all test? below
# x <- data.matrix(per_game_reg_season_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])
# didn't work ^ 


#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda


#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda, family = "binomial")
coef(best_model)
best_model$beta



# prediciton with new initial lasso 
copy_games_2223c_df$pctFG3 = copy_games_2223c_df$FG3_PCT
copy_games_2223c_df$pctFT = copy_games_2223c_df$FT_PCT
copy_games_2223c_df$oreb = copy_games_2223c_df$OREB
copy_games_2223c_df$dreb = copy_games_2223c_df$DREB
copy_games_2223c_df$treb = copy_games_2223c_df$REB
copy_games_2223c_df$ast = copy_games_2223c_df$AST
copy_games_2223c_df$tov = copy_games_2223c_df$TOV
copy_games_2223c_df$blk = copy_games_2223c_df$BLK
copy_games_2223c_df$pts = copy_games_2223c_df$PTS
copy_games_2223c_df$ftm = copy_games_2223c_df$FTM

new_x <- data.matrix(copy_games_2223c_df[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])

prediction_from_new_initial_lasso <- predict (best_model , s = best_lambda ,
                                              newx = new_x)

copy_games_2223c_df_cbinded_1 = cbind(copy_games_2223c_df, prediction_from_new_initial_lasso)

view(copy_games_2223c_df_cbinded_1)



# Stability Selection w this dataset 


# Stability Selection
stab.glmnet <- stabsel(x = x, y =  y, q = 4, B=300,
                       fitfun = glmnet.lasso,
                       args.fitfun = list(type = "conservative"),
                       cutoff = 0.6)
print(stab.glmnet$selected)
print(sort(stab.glmnet$max))


# pctFG, blk, stl, ast, pctWins way below these 


# new stability selection lasso 

#define response variable
y <- after_fact_current$isNBAChampion

#define matrix of predictor variables
# x <- data.matrix(per_game_reg_season_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])
# x <- data.matrix(big_mac_no_index_df[, c('WIN_PCT', "PLUS_MINUS", "PF","TOV","BLK" ,"STL","AST", "REB", "DREB", "OREB", "FT_PCT", "FTA", "FTM", "FG3_PCT", "FG3A", "FG3M", "FG_PCT", "FGA", "FGM", "PTS")])
x <- data.matrix(after_fact_current[, c("pctFG", "blk", "stl", "ast", "pctWins")])


# all test? below
# x <- data.matrix(per_game_reg_season_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])
# didn't work ^ 


#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda


#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda, family = "binomial")
coef(best_model)
best_model$beta


# prediction_w_stability_selection_lasso : 

new_x <- data.matrix(copy_games_2223c_df[, c("pctFG", "blk", "stl", "ast", "pctWins")])

new_new_prediction_from_stability_selection_lasso <- predict (best_model , s = best_lambda ,
                                                              newx = new_x)

new_games_2223c_df_cbinded_3 = cbind(copy_games_2223c_df, new_new_prediction_from_stability_selection_lasso)

view(new_games_2223c_df_cbinded_3)


# new stability selection logisitic 

# TODO 

# prediction_model <- glm(formula = isNBAChampion ~ pctWins + pctFG + stl, data = per_game_reg_season_current, family = binomial())

stability_selection_logistic = glm(formula = isNBAChampion ~ pctFG  + tov + stl + dreb + pctWins + pts + ftm, data = after_fact_current, family = binomial(link = 'logit'))

summary(stability_selection_logistic)

PseudoR2(stability_selection_logistic, which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

exp(stability_selection_logistic$coefficients)
exp(confint(stability_selection_logistic))

anova(stability_selection_logistic, test = "Chisq")

# prediction w this logistic: 

odds_logistic_new <- predict(stability_selection_logistic, copy_games_2223c_df, type = "response")

copy_games_2223c_df_cbinded_5 = cbind(copy_games_2223c_df, odds_logistic_new)

view(copy_games_2223c_df_cbinded_5)




# ISLR version of lasso ? # STILL NEED TO FIX, TODO 



y <- after_fact_current$isNBAChampion
# x <- model.matrix(complete_preinterview_cases$Progress_6_Complete_2yrs ~ . -1, complete_preinterview_cases[, c("App_year", "career_level", "UG_school_selectivity", "UG_major_byGroup", "UG_major_bySchool", "UG_major_minor_STEM", "UG_GPA", "UG_sports", "UG_PellGrant", "LIC_served_self_report", "LIC_served_rating", "Leadership_role", "family_responsibility", "Time_to_complete_app", "days_between_app_opening_and_application_start","days_between_app_opening_and_application_submission")])
# x <- model.matrix(complete_preinterview_cases$Progress_6_Complete_2yrs ~ . -1, complete_preinterview_cases[, c("career_level", "UG_school_selectivity", "UG_major_byGroup", "UG_major_bySchool", "UG_major_minor_STEM", "UG_GPA", "UG_sports", "UG_PellGrant", "Leadership_role", "family_responsibility", "Time_to_complete_app", "days_between_app_opening_and_application_start","days_between_app_opening_and_application_submission")])
# x <- model.matrix(complete_preinterview_cases$Progress_6_Complete_2yrs ~ . -1, complete_preinterview_cases[, c("career_level", "UG_school_selectivity", "UG_major_byGroup", "UG_major_bySchool", "UG_major_minor_STEM", "UG_GPA", "UG_sports", "UG_PellGrant", "LIC_served_rating", "Leadership_role", "family_responsibility", "Time_to_complete_app", "days_between_app_opening_and_application_start","days_between_app_opening_and_application_submission")])

x <- model.matrix(after_fact_current$isNBAChampion ~ . -1, after_fact_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])


# One hot encoding instead of dummy encoding: 
#x <- model.matrix(complete_preinterview_cases$Progress_6_Complete_2yrs ~ . -1, data =complete_preinterview_cases[, c("career_level", "UG_school_selectivity", "UG_major_byGroup", "UG_major_bySchool", "UG_major_minor_STEM", "UG_GPA", "UG_sports", "UG_PellGrant", "LIC_served_self_report", "LIC_served_rating", "Leadership_role", "family_responsibility", "Time_to_complete_app", "days_between_app_opening_and_application_start","days_between_app_opening_and_application_submission")],  contrasts.arg = lapply(complete_preinterview_cases[, c("career_level", "UG_school_selectivity", "UG_major_byGroup", "UG_major_bySchool", "UG_major_minor_STEM", "UG_sports", "UG_PellGrant", "LIC_served_self_report", "LIC_served_rating", "Leadership_role", "family_responsibility")], contrasts, contrasts=FALSE))




set.seed (123)
train <- sample (1: nrow (x), nrow (x) / 2)
test <- (-train)
y.test <- y[test]

grid <- 10^ seq (10, -2, length = 100)
ridge.mod <- glmnet (x, y, alpha = 1, lambda = grid, family = 'binomial')
dim ( coef (ridge.mod))

set.seed (123)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1, family = 'binomial')
plot (cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.mod <- glmnet (x[train , ], y[train], alpha = 1,
                     lambda = grid, thresh = 1e-12, family = 'binomial')
# ridge.pred <- predict (ridge.mod , s = 4, newx = x[test , ])
# mean ((ridge.pred - y.test)^2)

ridge.pred <- predict (ridge.mod , s = bestlam ,
                       newx = x[test , ])
mean ((ridge.pred - y.test)^2)

out <- glmnet (x, y, alpha = 1, family = 'binomial')
predict (out , type = "coefficients", s = bestlam)





# K Nearest Neighbors from ISLR 

train.x = data.matrix(after_fact_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])

train.x = scale(train.x)

test.x = data.matrix(copy_games_2223c_df[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])

test.x = scale(test.x)

true_values = subset(after_fact_current, subset = after_fact_current$isNBAChampion == 1 )

class.labels = after_fact_current$isNBAChampion

true_values$descriptionNBAFinalsAppearance = ifelse(is.na(true_values$descriptionNBAFinalsAppearance), "Champ", true_values$descriptionNBAFinalsAppearance)

set.seed (1)

knn.pred <- knn (train.x, test.x, class.labels , k = 7,prob = TRUE)
prob_w_copy_games_2223c_df <- cbind(copy_games_2223c_df, attributes(.Last.value)$prob)

view(prob_w_copy_games_2223c_df)

view(knn.pred)

sqrt(nrow(after_fact_current)) / 2 # = 6.708204

# with straight pandas dataset

y <- big_mac_no_index_df$IS_CHAMPION

x <- data.matrix(big_mac_no_index_df[, c('WIN_PCT', "PLUS_MINUS", "PF","TOV","BLK" ,"STL","AST", "REB", "DREB", "OREB", "FT_PCT", "FTA", "FTM", "FG3_PCT", "FG3A", "FG3M", "FG_PCT", "FGA", "FGM", "PTS")])

scaled_x = scale(x)


new_x <- data.matrix(games_2223c_df[, c('WIN_PCT', "PLUS_MINUS", "PF","TOV","BLK" ,"STL","AST", "REB", "DREB", "OREB", "FT_PCT", "FTA", "FTM", "FG3_PCT", "FG3A", "FG3M", "FG_PCT", "FGA", "FGM", "PTS")])

scaled_new_x = scale(new_x)

set.seed (1)

knn.pred_2 <- knn (scaled_x, scaled_new_x, y , k = 11, l = .5, prob = TRUE)
prob_w_games223c_df <- cbind(games_2223c_df, attributes(.Last.value)$prob)

view(knn.pred_2)

view(prob_w_games223c_df)

sqrt(nrow(big_mac_no_index_df)) / 2 # = 6-7





# KNN for 2021 

old_train.x = data.matrix(per_game_reg_season_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])

old_train.x = scale(old_train.x)

old_test.x = data.matrix(per_game_playoffs_test_year_2022[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])

old_test.x = scale(old_test.x)

old_class.labels = per_game_reg_season_current$isNBAChampion

set.seed (1)

old_knn.pred <- knn (old_train.x, old_test.x, old_class.labels , k = 11, prob = TRUE)
prob_w_per_game_playoffs_test_year_2022_df <- cbind(per_game_playoffs_test_year_2022, attributes(.Last.value)$prob)

view(prob_w_per_game_playoffs_test_year_2022_df)

view(old_knn.pred)

sqrt(nrow(per_game_reg_season_current)) / 2 # = 6.123724


# correlations

# x <- data.matrix(big_mac_no_index_df[, c('WIN_PCT', "PLUS_MINUS", "PF","TOV","BLK" ,"STL","AST", "REB", "DREB", "OREB", "FT_PCT", "FTA", "FTM", "FG3_PCT", "FG3A", "FG3M", "FG_PCT", "FGA", "FGM", "PTS")])
# 
# scaled_x = scale(x)
# 
# cor(scaled_x)


# LDA from ISLR

scaled_after_fact_current = scale(after_fact_current[,-c(1,2,13,33,34)])
# view(after_fact_current)

scaled_copy_games_2223c_df = scale(copy_games_2223c_df[,-c(1,22)])
# view(copy_games_2223c_df)

lda.fit = lda(formula = isNBAChampion ~ pctWins+ pctFG+ pctFG3+pctFT+oreb +dreb+treb+ ast+ tov+ stl+ blk+ pts+ ftm, data = scaled_after_fact_current)

lda.fit

lda.pred <- predict (lda.fit , scaled_copy_games_2223c_df)

posterior_wcopy_games_2223c_df = cbind(copy_games_2223c_df, lda.pred[["posterior"]])

view(posterior_wcopy_games_2223c_df)

x_w_copy_games_2223c_df = cbind(copy_games_2223c_df,lda.pred[["x"]])

view(x_w_copy_games_2223c_df)



# QDA from ISLR 
# TODO prob scale this 
# says too small for QDA - can only do number of smalles case - 1 (6 champs, so 5 columns allowed)

# pctFG, blk, stl, ast, pctWins

# lasso + stabs - stl blk fg_pct, win_pct, TOV, DREB

after_fact_current$isNBAChampion = as.factor(after_fact_current$isNBAChampion)
# can cahnge to numeric if necessary 

nrow(subset(after_fact_current, subset = after_fact_current$isNBAChampion == '1'))

qda.fit = qda(formula = isNBAChampion ~ pctWins+ pctFG + stl + dreb + fgm, data = after_fact_current)

qda.pred <- predict (qda.fit , copy_games_2223c_df)

qda_posterior_wcopy_games_2223c_df = cbind(copy_games_2223c_df, qda.pred[["posterior"]])

view(qda_posterior_wcopy_games_2223c_df)

x_qda_w_copy_games_2223c_df = cbind(copy_games_2223c_df,qda.pred[["x"]])

view(x_qda_w_copy_games_2223c_df)



# Naive bayes - ISLR

nb.fit = naiveBayes(formula = isNBAChampion ~ pctWins+ pctFG+ pctFG3+pctFT+oreb +dreb+treb+ ast+ tov+ stl+ blk+ pts+ ftm, data = after_fact_current)

nb.preds <- predict (nb.fit , copy_games_2223c_df, type = "raw")

nb_w_copy_games_2223c_df = cbind(copy_games_2223c_df, nb.preds)

view(nb_w_copy_games_2223c_df)

# Naive bayes says nuggets, pretty much only 1 


# Testing naive bayes of 2021 

old_nb.fit = naiveBayes(formula = isNBAChampion ~ pctWins+ pctFG+ pctFG3+pctFT+oreb +dreb+treb+ ast+ tov+ stl+ blk+ pts+ ftm, data = per_game_reg_season_current)

old_nb.preds <- predict (old_nb.fit , per_game_reg_season_test_year_2022, type = "raw")

old_nb_w_per_game_reg_season_test_year_2022_df = cbind(per_game_reg_season_test_year_2022, old_nb.preds)

view(old_nb_w_per_game_reg_season_test_year_2022_df)



# Testing QDA of 2021 
# TODO prob scale this 

old_qda.fit = qda(formula = isNBAChampion ~ pctWins+ pctFG + stl + blk, data = per_game_reg_season_current)

old_qda.pred <- predict (old_qda.fit , per_game_reg_season_test_year_2022)

old_qda_posterior_w_per_game_reg_season_test_year_2022 = cbind(per_game_reg_season_test_year_2022, old_qda.pred[["posterior"]])

view(old_qda_posterior_w_per_game_reg_season_test_year_2022)



# testing LDA of 2021
# TODO scale this, as inabove example 

old_lda.fit = lda(formula = isNBAChampion ~ pctWins+ pctFG+ pctFG3+pctFT+oreb +dreb+treb+ ast+ tov+ stl+ blk+ pts+ ftm, data = per_game_reg_season_current)

old_lda.pred <- predict (old_lda.fit , per_game_reg_season_test_year_2022)

old_lda_posterior_w_per_game_reg_season_test_year_2022 = cbind(per_game_reg_season_test_year_2022, old_lda.pred[["posterior"]])

view(old_lda_posterior_w_per_game_reg_season_test_year_2022)



# Random Forest w straight pandas - this ones good ! OOB error rate = 3.33
copy_big_mac_no_index_df = big_mac_no_index_df

copy_big_mac_no_index_df$IS_CHAMPION = as.factor(copy_big_mac_no_index_df$IS_CHAMPION)

set.seed (1)
# pandas_rf.pk <- randomForest(pkPerCapita ~ gunOwnership + violentRate + dem_new_percent + rep_new_percent + totalGuns + PovertyRate, tester , mtry = 3, importance = TRUE)
pandas_rf.pk = randomForest(formula = IS_CHAMPION ~ WIN_PCT+ PLUS_MINUS+ PF+TOV+BLK +STL+AST+ REB+ DREB+ OREB+ FT_PCT+ FTA+ FTM+ FG3_PCT+ FG3A+ FG3M+ FG_PCT+ FGA+ FGM+ PTS + eFG_PCT, data = copy_big_mac_no_index_df, mtry = 5, importance = TRUE, proximity = TRUE )

print(pandas_rf.pk)

yhat.rf <- predict (pandas_rf.pk, newdata = games_2223c_df, "prob")
# pk.test <- copy_games_2223c_df[, "isNBAChampion"] # added this to book code in case copying just this section 
# mean ((yhat.rf - pk.test)^2) # 5.013699 - so random forests yielded an improvement over bagging in this case 

rf_cbinded_w_games_2223c_df = cbind(games_2223c_df, yhat.rf)

view(rf_cbinded_w_games_2223c_df)

importance(pandas_rf.pk) # bigger numbers = more important predictor 
varImpPlot(pandas_rf.pk)


# Bagging w straight pandas # OOB error rate = 4 % 
copy_big_mac_no_index_df = big_mac_no_index_df

copy_big_mac_no_index_df$IS_CHAMPION = as.factor(copy_big_mac_no_index_df$IS_CHAMPION)

set.seed (1)
# pandas_bag.pk <- randomForest(pkPerCapita ~ gunOwnership + violentRate + dem_new_percent + rep_new_percent + totalGuns + PovertyRate, tester , mtry = 3, importance = TRUE)
pandas_bag.pk = randomForest(formula = IS_CHAMPION ~ WIN_PCT+ PLUS_MINUS+ PF+TOV+BLK +STL+AST+ REB+ DREB+ OREB+ FT_PCT+ FTA+ FTM+ FG3_PCT+ FG3A+ FG3M+ FG_PCT+ FGA+ FGM+ PTS, data = copy_big_mac_no_index_df, mtry = 20, importance = TRUE, proximity = TRUE )

print(pandas_bag.pk)

yhat.bag <- predict (pandas_bag.pk, newdata = games_2223c_df, "prob")
# pk.test <- copy_games_2223c_df[, "isNBAChampion"] # added this to book code in case copying just this section 
# mean ((yhat.bag - pk.test)^2) # 5.013699 - so random forests yielded an improvement over bagging in this case 

bag_cbinded_w_games_2223c_df = cbind(games_2223c_df, yhat.bag)

view(bag_cbinded_w_games_2223c_df)

importance(pandas_bag.pk) # bigger numbers = more important predictor 
varImpPlot(pandas_bag.pk)


# Random Forest 

set.seed (1)
# rf.pk <- randomForest(pkPerCapita ~ gunOwnership + violentRate + dem_new_percent + rep_new_percent + totalGuns + PovertyRate, tester , mtry = 3, importance = TRUE)
rf.pk = randomForest(formula = isNBAChampion ~ pctWins+ pctFG+ pctFG3+pctFT+oreb +dreb+treb+ ast+ tov+ stl+ blk+ pts+ ftm, data = after_fact_current, mtry = 4, importance = TRUE, proximity = TRUE )

print(rf.pk)

yhat.rf <- predict (rf.pk, newdata = copy_games_2223c_df, "prob")
# pk.test <- copy_games_2223c_df[, "isNBAChampion"] # added this to book code in case copying just this section 
# mean ((yhat.rf - pk.test)^2) # 5.013699 - so random forests yielded an improvement over bagging in this case 

rf_cbinded_w_copy_games_2223c_df = cbind(copy_games_2223c_df, yhat.rf)

view(rf_cbinded_w_copy_games_2223c_df)

importance(rf.pk) # bigger numbers = more important predictor 
varImpPlot(rf.pk)


# Bagging : selects Kings lol 

set.seed (1)
# rf.pk <- randomForest(pkPerCapita ~ gunOwnership + violentRate + dem_new_percent + rep_new_percent + totalGuns + PovertyRate, tester , mtry = 3, importance = TRUE)
bag.pk = randomForest(formula = isNBAChampion ~ pctWins+ pctFG+ pctFG3+pctFT+oreb +dreb+treb+ ast+ tov+ stl+ blk+ pts+ ftm, data = after_fact_current, mtry = 13, importance = TRUE, proximity = TRUE)

print(bag.pk)

yhat.bag <- predict (bag.pk, newdata = copy_games_2223c_df)
# pk.test <- copy_games_2223c_df[, "isNBAChampion"] # added this to book code in case copying just this section 
# mean ((yhat.rf - pk.test)^2) # 5.013699 - so random forests yielded an improvement over bagging in this case 

bag_cbinded_w_copy_games_2223c_df = cbind(copy_games_2223c_df, yhat.bag)

view(bag_cbinded_w_copy_games_2223c_df)

importance(bag.pk) # bigger numbers = more important predictor 
varImpPlot(bag.pk)


# Bagging on old data (so in general, bagging is looking WORSE than random forest)

per_game_reg_season_current$isNBAChampion = as.factor(per_game_reg_season_current$isNBAChampion)

set.seed (1)
old_bag.pk = randomForest(formula = isNBAChampion ~ pctWins+ pctFG+ pctFG3+pctFT+oreb +dreb+treb+ ast+ tov+ stl+ blk+ pts+ ftm, data = per_game_reg_season_current, mtry = 13, importance = TRUE, proximity = TRUE)

print(old_bag.pk)

old_yhat.bag <- predict (old_bag.pk, newdata = per_game_reg_season_test_year_2022, "prob")

old_bag_cbinded_w_per_game_reg_season_test_year_2022 = cbind(per_game_reg_season_test_year_2022, old_yhat.bag)

view(old_bag_cbinded_w_per_game_reg_season_test_year_2022)

importance(old_bag.pk) # bigger numbers = more important predictor 
varImpPlot(old_bag.pk)

# Random Forest on old data 

# TODO scale these before gleaning anything from feature importance here 

set.seed (1)
old_rf.pk = randomForest(formula = isNBAChampion ~ pctWins+ pctFG+ pctFG3+pctFT+oreb +dreb+treb+ ast+ tov+ stl+ blk+ pts+ ftm, data = per_game_reg_season_current, mtry = 4, importance = TRUE, proximity = TRUE)

print(old_rf.pk)

old_yhat.rf <- predict (old_rf.pk, newdata = per_game_reg_season_test_year_2022, "prob")

old_rf_cbinded_w_per_game_reg_season_test_year_2022 = cbind(per_game_reg_season_test_year_2022, old_yhat.rf)

view(old_rf_cbinded_w_per_game_reg_season_test_year_2022)

importance(old_rf.pk) # bigger numbers = more important predictor 
varImpPlot(old_rf.pk)


# Support Vector Machines 

# TODO 


