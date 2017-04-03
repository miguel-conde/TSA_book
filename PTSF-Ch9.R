## 9 - NEURAL NETWORKS


# 9.1 - Neural Networks for Forecasting Time Series -----------------------


# 9.2 - The Neural Network Model ------------------------------------------

# A neural network consists of 3 types of layers:
#
# 1. An input layer accepts the input values of predictors (numerical or binary
# 2. One or more hidden layers create derived variables.
# 3. An output layer produces predicted values (numeric or binary)
#
# Hidden layer nodes are derived variables. They're a weighted sum of the inputs
# to which some monotone function, called activation function, is applied. 
# Common functions are linear, exponential and s-shaped functions as the logit
# and hyperbolic tangent.
#
# Linear regression, logistic regression and AR models can be written as NN that
# lack hidden layers.
# A linear activation function corresponds to a linear regression model y_t,
# an exponential activation function, to the linear regression model log(y_t)and
# a logit activation function to the logistic regression model log(odds_t).


# 9.3 - Pre-processing ----------------------------------------------------

# Derived Predictors: lagged versions of the time series, lagged versions of
# external information, seasonal dummies and a time index to capture trend.
#
# Options:
# a. caret::avNNet()
# b. forecast:nnetar() (nn ar) -> better when the predictors are lagged 
#    versions of the time series.
#
# Removing rows with NAs: nnetar does it automatically.
#
# Scaling: nnetar does it automatically.
#
# Removing trend and seasonality: some people say this is a must, others not.
# Use any method (regression, smoothing,...) but differencing.


# 9.4 - User input --------------------------------------------------------

# Number of input layers: equal tothe number of predictors.
# Number of hidden layers: tipically 1-2 layers is sufficient.
# Number of nodes per hidden layers: requires trial and error.
# Choice of activation function: the most common are s-shaped (sigmoid) 
#                                functions.
# Number of output nodes: depends on the number of forecast to generate.



# 9.5 - Forecasting with Neural Nets in R ---------------------------------

# When working with time series, working with caret::avNNet() can be tedious:
# not only all the pre-processing must be done "by hand" but also when you
# want to forecast more than 1-step ahead, some of the predictors in the 
# validation period need to be forecasted values.
# forecast::nnetar() does all this in an automatic manner, but it can be 
# used only if all the predictors are llaged versions of the time series 
# itself. To include other predictors, such as seasonal dummies, you'll need
# to use caret::avNNet().
#
# forecast::nnetar() has 4 arguments: size, p, P and size.
#
# repeats: number of nn's to fit (default:20)
#
# Neural Network Auto Regression NNAR(p, P, size) 
#
# p: number of non-seasonal lags. If not specified, nnetar() chooses p based
#    on the best-fit AR(p).
# P: number of sesaonal lags (default: 1)
# size: number of nodes in the single hidden layer that nnetar() considers.
#       (default: int((p+P+1)/2) )
#
# Activation functions:
# Input to hidden: logit function
# Hidden to output: linear (lineout = TRUE) for output with continuous range,
#                   or logit (lineout = FALSE) for binary outcomes.
# Output: logit to ensure the output is a "probability".


# 9.6 - Example: forecasting Amtrak Ridership -----------------------------

library(readxl)
Amtrak_data <- 
  read_excel("./DATA/AmtrakPassengersMonthly T-Competition.xls", 
             sheet = "Data")
ridership_ts <- ts(Amtrak_data$Ridership, 
                   start = c(1991,1), end = c(2004, 3),
                   frequency = 12)
plot(ridership_ts,  
     xlab = "Time", ylab = "Ridership",
     bty = "l")

## Preparing the data
#
# The anual seasonality can be addressed in several ways:
# 1. 11 non seasonal lags and 1 seasonal lag
# 2. Include 11 dummy variables
# 3. Deseasonalize using some common method
#
# Here we'll apply the 1st option

# Validation period: last 36 months.
# There are 159 months in the time series, dropping 12 months due to lag
# variables and 36 for the validation period, the training period contains
# 111 months.
nTotal <- length(ridership_ts)
nValid <- 36
nLag <- 12
nTrain <- nTotal - nValid 
startTrain <- c(1991,1)
startValid <- startTrain + c(0, nTrain)
f <- 12

train_ts <- ts(ridership_ts[1:nTrain], 
               start = startTrain, frequency = f)
valid_ts <- ts(ridership_ts[(nTrain+1):nTotal], 
               start = startValid, frequency = f)

## Neural Network Output
#
library(forecast)

set.seed(201)
ridership.nnetar <- nnetar(train_ts, repeats = 20, p = 11, P = 1, size = 7)
summary(ridership.nnetar$model[[1]]) # 1st of 20 fitted nn

ridership.nnetar.pred <- forecast(ridership.nnetar, h = nValid)
accuracy(ridership.nnetar.pred, valid_ts)

plot(train_ts,
     ylim = c(1300, 2900), 
     ylab = "Ridership", xlab = "time",
     xlim = c(1991, 2006.25),
     bty = "l", xaxt = "n", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership.nnetar.pred$fitted, lwd = 2, col = "blue")
lines(ridership.nnetar.pred$mean, lwd = 2, col = "blue", lty = 2)
lines(valid_ts)

# The nn is overfit to the training set