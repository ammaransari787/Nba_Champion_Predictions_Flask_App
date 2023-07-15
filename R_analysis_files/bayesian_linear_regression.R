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
