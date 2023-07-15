library(purrr)
library(nbastatR)
library(tidyverse)
library(glmnet)
library(stabs)
library(class)
library(MASS)
library (e1071)
library (randomForest)

Sys.setenv(VROOM_CONNECTION_SIZE=500072)
# assign_nba_players()

# players_careers(players = c("LeBron James"))
# 
# 
# # my_table <- bref_players_stats()
# 
# 
# players_careers(players = c("Stephen Curry"))

assign_nba_teams()

# my_tibble <- teams_annual_stats(all_active_teams = T, season_types = c("Regular Season", "Playoffs"), modes = c("Totals", "PerGame"))


per_game_playoffs <- teams_annual_stats(all_active_teams = T, season_types = c("Playoffs"), modes = c("PerGame"))
per_game_reg_season <- teams_annual_stats(all_active_teams = T, season_types = c("Regular Season"), modes = c("PerGame"))

totals_playoffs <- teams_annual_stats(all_active_teams = T, season_types = c("Playoffs"), modes = c("Totals"))
totals_reg_season <- teams_annual_stats(all_active_teams = T, season_types = c("Regular Season"), modes = c("Totals"))


per_game_playoffs_current <- per_game_playoffs %>%
  filter(slugSeason == "2020-21" | slugSeason == "2019-20" | slugSeason == "2018-19" | slugSeason == "2017-18" | slugSeason == "2016-17")

per_game_playoffs_current$isNBAChampion[is.na(per_game_playoffs_current$isNBAChampion)] <- 0
  

# prediction_model <- glm(formula = isNBAChampion ~ pctFG3 + tov + pts + blk + stl + ast + oreb + dreb + fg3a + fga + pctFG + fgm, data = per_game_playoffs_current, family = binomial())

# prediction_model <- glm(formula = isNBAChampion ~ pts + blk + stl + pctFG + ast + tov, data = per_game_playoffs_current, family = binomial())

#prediction_model <- glm(formula = isNBAChampion ~ pctFG + stl + pts, data = per_game_playoffs_current, family = binomial())

summary(prediction_model)

PseudoR2(prediction_model, which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

exp(prediction_model$coefficients)
exp(confint(prediction_model))

anova(prediction_model, test = "Chisq")


# testobject <- data.frame(salary_decile = "50", age_ventile = "10p", yrs_of_service = "2__2-3", ORG_CODE = "Org_1026")
# warriorstestobject <- data.frame(pts = 111.0, stl = 8.8, pctFG =.469)
# predict(prediction_model, warriorstestobject, type = "response")
# 
# celticstestobject <- data.frame(pts = 111.8, stl = 7.2, pctFG = .466)
# predict(prediction_model, celticstestobject, type = "response")

# warriors are going to win. According to per_game_playoffs_current. need to also check with reg season. 



per_game_playoffs_test_year_2022 <- per_game_playoffs %>%
  filter(slugSeason == "2021-22")




odds <- predict(prediction_model, per_game_playoffs_test_year_2022, type = "response")

per_game_playoffs_test_year_2022 <- cbind(per_game_playoffs_test_year_2022, odds)

ggplot(per_game_playoffs_test_year_2022, aes(x= reorder(nameTeam, odds), y = odds)) + geom_col() + coord_flip() 


# now do it for regular season


per_game_reg_season_current <- per_game_reg_season %>%
  filter(slugSeason == "2020-21" | slugSeason == "2019-20" | slugSeason == "2018-19" | slugSeason == "2017-18" | slugSeason == "2016-17")

# per_game_reg_season_current$isNBAChampion[is.na(per_game_reg_season_current$isNBAChampion)] <- 0

per_game_reg_season_current$isNBAChampion <- ifelse(is.na(per_game_reg_season_current$isNBAChampion), 0, 1)

per_game_reg_season_current$isNBAChampion

# prediction_model <- glm(formula = isNBAChampion ~ pctWins+pctFG +stl, data = per_game_reg_season_current, family = binomial())

# this one below strong but had Warning message:glm.fit: fitted probabilities numerically 0 or 1 occurred
prediction_model <- glm(formula = isNBAChampion ~  pctWins + pctFG + stl + dreb + blk, data = per_game_reg_season_current, family = binomial())

# wait maybe this doesn't just translate over- testing usnig actual lasso model # LASSO CHANGE ONE OF this one below strong but had Warning message:glm.fit: fitted probabilities numerically 0 or 1 occurred
prediction_model <- glm(formula = isNBAChampion ~  pctWins + pctFG + stl + pts + blk, data = per_game_reg_season_current, family = binomial())

# prediction_model <- glm(formula = isNBAChampion ~  dreb+ pctWins + pctFG +stl, data = per_game_reg_season_current, family = binomial())

# this one below is the one i screenshotted 
prediction_model <- glm(formula = isNBAChampion ~  pctWins + pctFG + stl + dreb, data = per_game_reg_season_current, family = binomial())

# keep getting back to this one (see first one above) - remarkably accurate + lean, all signs point to these being most important attributes
prediction_model <- glm(formula = isNBAChampion ~ pctWins + pctFG + stl, data = per_game_reg_season_current, family = binomial())

# stabs above .5: 
prediction_model <- glm(formula = isNBAChampion ~ blk + pctFG + stl, data = per_game_reg_season_current, family = binomial())

# stabs above .4: 
prediction_model <- glm(formula = isNBAChampion ~ blk + pctWins + pctFG + stl + ast, data = per_game_reg_season_current, family = binomial())

summary(prediction_model)

PseudoR2(prediction_model, which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

exp(prediction_model$coefficients)
exp(confint(prediction_model))

anova(prediction_model, test = "Chisq")


# ok, here I'm going to test lasso and see what it finds sunglasses_emoji 

# gonna try lasso regression here and see what it says, wayyy after the fact  

#define response variable
y <- per_game_reg_season_current$isNBAChampion

#define matrix of predictor variables
x <- data.matrix(per_game_reg_season_current[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")])
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

#vif(backwards_2)
#wanna try predicting/maybe roc? idk, maybe rmse? 



# testobject <- data.frame(salary_decile = "50", age_ventile = "10p", yrs_of_service = "2__2-3", ORG_CODE = "Org_1026")
# warriorstestobject <- data.frame(pts = 111.0, stl = 8.8, pctFG =.469)
# predict(prediction_model, warriorstestobject, type = "response")
# 
# celticstestobject <- data.frame(pts = 111.8, stl = 7.2, pctFG = .466)
# predict(prediction_model, celticstestobject, type = "response")

# warriors are going to win. According to per_game_reg_season_current. need to also check with reg season. 

per_game_reg_season_test_year_2022 <- per_game_reg_season %>%
  filter(slugSeason == "2021-22")

odds <- predict(prediction_model, per_game_reg_season_test_year_2022, type = "response")

odds <- predict(best_model, s = best_lambda, newx = data.matrix(per_game_reg_season_test_year_2022[, c('pctWins', "pctFG", "pctFG3","pctFT","oreb" ,"dreb","treb", "ast", "tov", "stl", "blk", "pts", "ftm")]))

per_game_reg_season_test_year_2022 <- cbind(per_game_reg_season_test_year_2022, odds)

ggplot(per_game_reg_season_test_year_2022, aes(x= reorder(nameTeam, odds), y = odds)) + geom_col() + coord_flip() 

predict(prediction_model, per_game_reg_season_test_year_2022[per_game_reg_season_test_year_2022$nameTeam == "Golden State Warriors", ], type = "response")

# testsh2 

per_game_reg_season_test_year_2022_X <- per_game_reg_season_test_year_2022# [c("nameTeam", "pts", "stl","pctFG")]
per_game_reg_season_test_year_2022_X

# per_game_reg_season_test_year_2022_X$isNBAChampion <- ifelse(is.na(per_game_reg_season_test_year_2022_X$isNBAChampion), 0, 1)

odds <- predict(prediction_model, per_game_reg_season_test_year_2022_X, type = "response")


per_game_reg_season_test_year_2022 <- cbind(per_game_reg_season_test_year_2022_X, odds)

ggplot(per_game_reg_season_test_year_2022_X, aes(x= reorder(nameTeam, odds), y = odds)) + geom_col() + coord_flip() 

ggplot(data = per_game_reg_season_test_year_2022, aes(x= reorder(nameTeam, odds_new_2), y = odds_new_2)) + geom_col() + coord_flip() 

per_game_reg_season_test_year_2022 = subset(per_game_reg_season_test_year_2022, select =-c(per_game_reg_season_test_year_2022$odds))

# end testsh2 
summary(prediction_model)
# so the good one DOES NOT include BLKS, i believe, but double check  

