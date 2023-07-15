# Nba_Champion_Predictions_Flask_App

Link to live app: https://nba-champion-2023-predicted-probabilities.fly.dev/


This project initially began as a way to practice statistical techniques I was learning using real world data that I found relevant. I kept building on it as I learned new topics, coming back to it and adapting the project to incorporate the new techniques, programming languages, or statistical paradigms I wanted to explore more fully. 

I have used this project to practice and develop my statistics skills, working through techniques from the books Discovering Statistics Using R by Andy Field, an Introduction to Statistical Learning with Applications in R by James, Witten, Hastie, and Tibshirani, and Statistical Rethinking: A Bayesian Course With Applications in R and STAN by Richard McElreath. 

Additionally, when I wanted to practice performing these techniques I had been learning in R in Python instead, I rewrote all R code in Python, as a means to teach myself Python and pandas. 

I also used it to further my learning on web development using the Python Flask framework, as well as explore other techniques I used in various analytics and machine learning competitions, such as Stability Selection (https://rss.onlinelibrary.wiley.com/doi/10.1111/j.1467-9868.2010.00740.x) and  AutoML with TPOT (http://epistasislab.github.io/tpot/). 

I additionally incorporated techniques for optimizing the app and making it more computationally efficient, such as caching and data stores such as flat files or databases, as part of my final project of CS 3C: Advanced Data Structures and Algorithms in Python, at Foothill College. 

While the project initially began as a vehicle for learning, several of the later additions, in particular the newest portion using Bayesian Linear Regression, are remarkably accurate. By simply using weakly informative priors that keep the results in the realm of possible values and perform regularization of the model against overfitting, and cross-validation techniques for assessing various models as recommended in Statistical Rethinking to decide between feature setts proposed by various feature selection procedures such as LASSO regression and Logistic Regression, along with transformation of the continuous predictor variable, Playoff Wins, to be on the log transformed scale (because there is a natural maximum for amount of playoff wins for any team), a Bayesian Linear Regression performed extremely well. It is also easy to communicate results in the Bayesian Paradigm, by easily being able to see which teams have the potential to reach the required amount of wins within their 99% percent credible interval. 

I plan to continue working on the app and keep it live for the regular season next year and evaluate its performance. I also plan on adding features as well, such as incorporating conference ranking as an ordinal predictor to be able to account for stronger or weaker conferences and therefore easier paths to more wins, as well as web scraping coach win record data to potentially include as a predictor, as this information is not found in the NBA api I used to source data. 
