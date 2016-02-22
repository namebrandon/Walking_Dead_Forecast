library(xts)
library(forecast)
library(Matrix)

#Windows
setwd('C:/Users/Brandon/Desktop/clean_for_GitHub')

#OS X
#setwd('/Users/brandon')


# Read data file (obtained from https://en.wikipedia.org/wiki/The_Walking_Dead_%28TV_series%29 )
project_data <- read.csv("walking_dead_viewers_wikipedia.csv",stringsAsFactors=FALSE)

# Convert AirDate column from character to date object
project_data$Airdate <- as.Date(project_data$Airdate, "%m/%d/%Y")


# Convert the type of show variable and sunday night football variables
# to be factor-level variables instead of characters.
project_data$Type <- as.factor(project_data$Type)
project_data$sunday_night_football <- as.factor(project_data$sunday_night_football)


# create time series object out of viewers and the episode airdates
ts_data <- xts(project_data$Viewers, order.by = project_data$Airdate)

# plot the data
plot(ts_data)

# run an auto.arima on the time series to get a starting point
# Note the suggestion of (2,1,2) model as well as AIC of 239.85
auto.arima(ts_data)

# turn our factor level variables into a sparse matrix to be used with regression
# esentially this is "One hot" encoding the variables.
encoded_regressors <- sparse.model.matrix(Viewers~Type+sunday_night_football, data = project_data)

# create a time series out of of encoded categorical variables
regressors <- xts(encoded_regressors[,-1], order.by = project_data$Airdate)

# re-run auto.arima to see if our model changed
# note that it suggests (0,1,1), and our AIC has improved! 239.85 -> 197.07
auto.arima(ts_data,xreg=regressors)

# here's an Arima model function we can use to test sasonality model
# 
# The unusual thing about this TV show is that each season has 2 premieres and 2 finale's
# Each 8-episode arc is a mini-season. I'll refer to the first mini-season as A and the second
# mini season as B. So Season 6A = 8 episodes, Season 6B = 8 episodes, Season 6 total = 16 episodes.
#
# Note that our AIC has dropped even further! 
# 
Arima(ts_data, xreg=regressors, order = c(0,1,1), seasonal = list(order = c(1,1,0), period = 8))


#Since we have one data point for Season 6B (the season airing right now)
#we're going to train our model on all of our data, except the single season 6B data point

# kill the last data point
train_data <- head(ts_data,-1)
# kill the last regressor data points
train_regressors <- head(regressors,-1)

# fit our model to our new data which has no Season 6B information

model_fit <- Arima(train_data, xreg=train_regressors, order = c(0,1,1), seasonal = list(order = c(1,1,0), period = 8))


# When we predict the new season, we need the regressor information for the upcoming episodes
# so I just created a file with these values as it was easier than making the data in R directly.
# This is for the future, season 6B episodes.
season_6_regressors <- read.csv("season_6_regressors.csv",stringsAsFactors=FALSE)

# We convert the episode Airdate column to a time object - These are the future episode airdates
season_6_regressors$Airdate <- as.Date(season_6_regressors$Airdate, "%m/%d/%Y")

# We grab the columns needed for the regression and create a time series out of it
# using the airdate for future episodes
test_regressors <- xts(season_6_regressors[,2:4], order.by = season_6_regressors$Airdate)

# We apply our model from seasons 1-5 and ask it to predict the next 8 episodes of Season 6
# using our new regessor data
our_forecast <- predict(model_fit, newxreg=test_regressors, n.ahead = 8)

# we get output of our forecast and store it as a new time series
forecasted_viewers <- xts(our_forecast$pred,season_6_regressors$Airdate)

# plot the actual (including our single data point from Season 6B)
# as well as our predictions

plot.xts(ts_data,main="Viewership (millions) - Actual vs Predicted (Red)")
lines(forecasted_viewers, col=c("red"))

# get an output of the actual counts we predicted / forecasted.
forecasted_viewers



