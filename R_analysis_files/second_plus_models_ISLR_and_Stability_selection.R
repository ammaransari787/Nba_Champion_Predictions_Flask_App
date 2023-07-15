big_mac_no_index_df = read.csv("big_mac_no_index.csv")
# view(big_mac_no_index_df)


big_mac_w_index_df = read.csv("big_mac_w_index.csv")
# view(big_mac_w_index_df)

games_2223c_df = read.csv('games_2223c_no_index.csv')

# ((FGM + (0.5 * 3PM)) / FGA

big_mac_no_index_df$eFG_PCT = (big_mac_no_index_df$FGM + (0.5 * big_mac_no_index_df$FG3M)) / big_mac_no_index_df$FGA

big_mac_no_index_df$IS_CHAMPION <- ifelse(big_mac_no_index_df$IS_CHAMPION == "True", 1, 0)

# gonna try lasso regression here and see what it says, wayyy after the fact  

#define response variable
y <- big_mac_no_index_df$IS_CHAMPION

#define matrix of predictor variables
# x <- data.matrix(per_game_reg_season_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])
x <- data.matrix(big_mac_no_index_df[, c('WIN_PCT', "PLUS_MINUS", "PF","TOV","BLK" ,"STL","AST", "REB", "DREB", "OREB", "FT_PCT", "FTA", "FTM", "FG3_PCT", "FG3A", "FG3M", "FG_PCT", "FGA", "FGM", "PTS", "eFG_PCT")])
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


# Stability Selection
stab.glmnet <- stabsel(x = x, y =  y, q = 7, B=300,
                       fitfun = glmnet.lasso,
                       args.fitfun = list(type = "conservative"),
                       cutoff = 0.6)
print(stab.glmnet$selected)
print(sort(stab.glmnet$max))

# prediciton with initial lasso 

games_2223c_df$eFG_PCT = (games_2223c_df$FGM + (0.5 * games_2223c_df$FG3M)) / games_2223c_df$FGA

new_x <- data.matrix(games_2223c_df[, c('WIN_PCT', "PLUS_MINUS", "PF","TOV","BLK" ,"STL","AST", "REB", "DREB", "OREB", "FT_PCT", "FTA", "FTM", "FG3_PCT", "FG3A", "FG3M", "FG_PCT", "FGA", "FGM", "PTS", "eFG_PCT")])

prediction_from_initial_lasso <- predict (best_model , s = best_lambda ,
                       newx = new_x)

games_2223c_df_cbinded_1 = cbind(games_2223c_df, prediction_from_initial_lasso)

view(games_2223c_df_cbinded_1)



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

new_x <- data.matrix(games_2223c_df[, c("STL", "BLK", "FG_PCT", "TOV", "WIN_PCT")])

prediction_from_stability_selection_lasso <- predict (best_model , s = best_lambda ,
                                          newx = new_x)

games_2223c_df_cbinded_2 = cbind(games_2223c_df, prediction_from_stability_selection_lasso)

view(games_2223c_df_cbinded_2)



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


