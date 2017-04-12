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
     ylim = c(1300, 2200), 
     ylab = "Ridership", xlab = "time",
     xlim = c(1991, 2006.25),
     bty = "l", xaxt = "n", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership.nnetar.pred$fitted, lwd = 2, col = "blue")
lines(ridership.nnetar.pred$mean, lwd = 2, col = "blue", lty = 2)
lines(valid_ts)

# The nn is overfit to the training set

## Approach 2: deseasonalize
stl_train_ts <- stl(train_ts, s.window = "periodic")
noise_train_ts <- train_ts - stl_train_ts$time.series[,"trend"] - stl_train_ts$time.series[,"seasonal"]
noise_train_ts 

set.seed(201)
ridership.nnetar <- nnetar(noise_train_ts, repeats = 20, p = 11, P = 1, size = 7)
summary(ridership.nnetar$model[[1]]) # 1st of 20 fitted nn

ridership.nnetar.pred <- forecast(ridership.nnetar, h = nValid)$mean + snaive(train_ts, h = nValid)$mean
accuracy(ridership.nnetar.pred, valid_ts)

plot(train_ts,
     ylim = c(1300, 2200), 
     ylab = "Ridership", xlab = "time",
     xlim = c(1991, 2006.25),
     bty = "l", xaxt = "n", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership.nnetar.pred, lwd = 2, col = "blue")
#lines(ridership.nnetar.pred$mean, lwd = 2, col = "blue", lty = 2)
lines(valid_ts)


# 9.7 - PROBLEMS ----------------------------------------------------------

# Forecasting Australian Wine Sales
library(readxl)
AW <- read_excel("D:/PROYECTOS DATA SCIENCE/TSA_book/DATA/AustralianWines.xls")
AW$`Source: Website` <- NULL

AW_ts <- ts(AW[,-1], 
            start = c(1980, 1), end = c(1994, 12),
            frequency = 12)
plot(AW_ts,
     xlab = "", ylab = "Sales", 
     main = "Australian Wines monthly sales", bty = "n", type = "l")

# labs <- c(unlist(lapply(1980:1994, paste, "Jan")), "")
# at_seq <- seq(1980, 1995, 1)
# axis(side = 1, at = at_seq, labels = labs, las = 2)

# We want to obtain short-term forecasts (2-3 months ahead) for each of the 6
# series, and this task will be repeated every month.

# 2. Use neural networks to forecast fortified wine sales, as follows:
#
# Partition the data using the period until December 1993 as the training
# period.
nTotal <- nrow(AW_ts)

trainStart <- c(1980, 1)
nTrain <- (1993 - 1980 + 1) * 12 
trainEnd   <- trainStart + c(0, nTrain-1)

validStart <- trainEnd + c(0, 1)
nValid <- nTotal - nTrain
validEnd <- validStart + c(0, nValid-1)

fort_train_ts <- window(AW_ts[,"Fortified"],
                        start = trainStart, end = trainEnd)
fort_valid_ts <- window(AW_ts[,"Fortified"],
                        start = validStart, end = validEnd)

# Run a neural network using R's nnetar with 11 non-seasonal lags (i.e., p = 11)
# Leave all other arguments at their default.
library(forecast)
fort.nnetar <- nnetar(y = fort_train_ts, p = 11)

# (a) Create a time plot for the actual and forecasted series over the training
#     period. Create also a time plot of the forecast errors for the training
#     period. Interpret what you see in the plots.
plot(fort_train_ts,
     xlab = "", ylab = "Sales", 
     main = "Fortified Wines monthly sales", 
     bty = "n", type = "l", xaxt = "n")

labs <- c(unlist(lapply(1980:1994, paste, "Jan")), "")
at_seq <- seq(1980, 1995, 1)
axis(side = 1, at = at_seq, labels = labs, las = 2)

lines(fitted(fort.nnetar), lty = 1, col = "blue")

plot(residuals(fort.nnetar),
     xlab = "", ylab = "Sales", 
     main = "Fortified Wines monthly sales", 
     bty = "l", type = "l", xaxt = "n")

axis(side = 1, at = at_seq, labels = labs, las = 2)
abline(h = 0, lty = 2)

# (b) Use the neural network to forecast sales in each month in the validation
#     period (January to December 1994)
fort.nnetar.pred <- forecast(fort.nnetar, h = 12)
fort.nnetar.pred

plot(fort_train_ts,
     xlim = c(1980, 1995),
     xlab = "", ylab = "Sales", 
     main = "Fortified Wines monthly sales", 
     bty = "l", type = "l", xaxt = "n")

labs <- c(unlist(lapply(1980:1995, paste, "Jan")), "")
at_seq <- seq(1980, 1996, 1)
axis(side = 1, at = at_seq, labels = labs, las = 2)

lines(fitted(fort.nnetar), lty = 1, lwd = 2, col = "blue")
lines(fort_valid_ts, lty = 2)
lines(fort.nnetar.pred$mean, lty = 2, lwd = 2, col = "blue")

# 3 - Compare your neural network to an exponential smoothing model used to
#     forecast fortified wine sales.
#
# (a) Use R's ets function to automatically select and fit an exponential
#     smoothing model to the training period until Dec 93. Which model did
#     ets fit?

fort.ets <- ets(fort_train_ts)
fort.ets

# (b) Use this exponential smoothing model to forecast sales for each month in 
#     1994.
fort.ets.pred <- forecast(fort.ets, h = 12)
fort.ets.pred

plot(fort.ets.pred,
     xlim = c(1980, 1995),
     xlab = "", ylab = "Sales", 
     main = "Fortified Wines monthly sales", 
     bty = "l", type = "l", xaxt = "n")

labs <- c(unlist(lapply(1980:1995, paste, "Jan")), "")
at_seq <- seq(1980, 1996, 1)
axis(side = 1, at = at_seq, labels = labs, las = 2)

lines(fitted(fort.ets), lty = 1, lwd = 2, col = "blue")
lines(fort_valid_ts, lty = 2)

# (c) How does the nn compare to the exponential smoothing model in terms of
#     predictive performance in the training period? In the validation period?
accuracy(fort.nnetar.pred, fort_valid_ts)
accuracy(fort.ets.pred, fort_valid_ts)


## PROBATINA
library(readxl)
PET_PRI_SPT_S1_M <- read_excel("D:/PROYECTOS DATA SCIENCE/TSA_book/DATA/PET_PRI_SPT_S1_M.xls")

numNAs <- sum(is.na(PET_PRI_SPT_S1_M[,3]))
EBSP_FOB <- PET_PRI_SPT_S1_M[(numNAs+1):nrow(PET_PRI_SPT_S1_M), c(1,3)]
names(EBSP_FOB)[2] <- "EBSP_FOB"

start_EBSP_FOB_ts <- c(1986, 5)
end_EBSP_FOB_ts   <- c(2016, 6)
f <- 12

EBSP_FOB_ts <- ts(EBSP_FOB[, 2], 
                  start = start_EBSP_FOB_ts, end = end_EBSP_FOB_ts,
                  frequency = f)
plot(EBSP_FOB_ts,
     main = "Europe Brent Spot Price FOB",
     ylab = "Dollars per Barrel", bty = "l")

nTotal <- nrow(EBSP_FOB)
nTrain <- nTotal * 0.8
nValid <- nTotal - nTrain

startTrain <- start_EBSP_FOB_ts
endTrain   <- startTrain + c(0, nTrain-1)
startValid <- endTrain + c(0, 1)
endValid   <- end_EBSP_FOB_ts

EBSP_FOB_train_ts <- ts(EBSP_FOB[1:nTrain, 2], 
                        start = startTrain, end = endTrain,
                        frequency = f)
EBSP_FOB_valid_ts <- ts(EBSP_FOB[(nTrain+1):nTotal, 2], 
                        start = startValid, end = endValid,
                        frequency = f)
plot(EBSP_FOB_valid_ts, xlim = c(1986, 2017), ylim = c(0, 140), col = "red")
lines(EBSP_FOB_train_ts)

# ETS
EBSP_FOB_ets <- ets(EBSP_FOB_train_ts)
EBSP_FOB_ets.pred <- forecast(EBSP_FOB_ets, h = nValid)

plot(EBSP_FOB_ets.pred,
     xlim = c(1986, 2017),
     main = "Europe Brent Spot Price FOB",
     ylab = "Dollars per Barrel", bty = "l")

lines(fitted(EBSP_FOB_ets), lty = 1, lwd = 2, col = "blue")
lines(EBSP_FOB_valid_ts, lty = 2)

# NNETAR
EBSP_FOB_nnetar <- nnetar(EBSP_FOB_train_ts)
EBSP_FOB_nnetar.pred <- forecast(EBSP_FOB_nnetar, h = nValid)

plot(EBSP_FOB_nnetar.pred,
     xlim = c(1986, 2017),
     main = "Europe Brent Spot Price FOB",
     ylab = "Dollars per Barrel", bty = "l", lty = 2, lwd = 2)

lines(fitted(EBSP_FOB_nnetar), lty = 1, lwd = 2, col = "blue")
lines(EBSP_FOB_valid_ts, lty = 2)

## LSTM
makeVarsTS <- function(tsSet, lags = c(1)) {
  xy <- tsSet
  
  for (l in lags) {
    xy <- cbind(xy, lag(tsSet, -l))
  }
  xy <- as.data.frame(xy)
  names(xy) <- c("Y", paste0("lag_", lags))
  xy <- cbind(xy, Y = xy[, "Y"])
  xy[, 1] <- NULL
  
  xy <- xy[(length(lags)+1):length(time((tsSet))),]
  
  scales <- list()
  xy <- sapply(xy, function(X) {
    X <- scale(X)
    scales <<- cbind(scales, c(attr(X, "scaled:center"), attr(X, "scaled:scale")))
    X
  })
  
  scales <- as.data.frame(scales)
  names(scales) <- names(xy)
  row.names(scales) <- c("center", "scale")
  
  return(list(xy = as.data.frame(xy), scales = scales))
}

lags <- 1:4
xy_train <- makeVarsTS(EBSP_FOB_train_ts, lags = lags)
xy_valid <- makeVarsTS(EBSP_FOB_valid_ts, lags = lags)

# Prepare Data <samples, time steps, features>
t <- time(EBSP_FOB_train_ts)
t <- t[(length(lags)+1):length(t)]
# y_train <- array(data = xy_train$xy[, "Y"], 
#                  dim = c(1, nTrain - length(lags), 1),
#                  dimnames = list("Sample1", t, "Y"))
y_train <- array(data = unlist(xy_train$xy[, "Y"]), 
                 dim = c(nrow(xy_train$xy), 
                         1,
                         1),
                 dimnames = list(t, "Y", names(ncol(xy_train$xy))))
# x_train <- array(data = unlist(xy_train$xy[, names(xy_train$xy)[-1]]),
#                  dim = c(1, nTrain - length(lags), ncol(xy_train$xy)- 1),
#                  dimnames = list("Sample1", t, 
#                                  names(xy_train$xy)[-1]))
x_train <- array(data = unlist(xy_train$xy[, lags]), 
                 dim = c(nrow(xy_train$xy), 
                         1,
                         ncol(xy_train$xy)-1),
                 dimnames = list(t, "Sample1", names(ncol(xy_train$xy))))

library(rnn)

EBSP_FOB_lstm <- trainr(Y = y_train,
                        X = x_train, learningrate = 0.1,
                        network_type = "lstm",
                        num_epochs = 4)
EBSP_FOB_lstm.pred <- as.numeric(predictr(EBSP_FOB_lstm, X = x_train))
EBSP_FOB_lstm.pred <- EBSP_FOB_lstm.pred * unlist(xy_train$scales[2,1]) + unlist(xy_train$scales[1,1])
EBSP_FOB_lstm.pred <- ts(EBSP_FOB_lstm.pred,
                          start = startTrain + c(0, max(lags)+1), frequency = f)

plot(EBSP_FOB_lstm.pred,
     xlim = c(1986, 2017), ylim = c(0, 150),
     main = "Europe Brent Spot Price FOB",
     ylab = "Dollars per Barrel", bty = "l", lty = 2, lwd = 2)

lines(EBSP_FOB_train_ts, lty = 1, lwd = 2, col = "blue")
lines(EBSP_FOB_valid_ts, lty = 2)

## RSNNS http://software-tecnico-libre.es/es/articulo-por-tema/todas-las-secciones/todos-los-temas/todos-los-articulos/redes-neuronales-recurrentes-y-series-de-tiempo
makeVarsTS_2 <- function(tsSet, lags = c(1)) {
  xy <- tsSet
  
  xy <- scale(xy)
  scales <- c(attr(xy, "scaled:center"), attr(xy, "scaled:scale"))
  xy <- ts(xy, start = start(xy), end = end(xy), frequency = frequency(xy))
  
  out <- xy
  for (l in lags) {
    out <- cbind(out, lag(xy[, 1], -l))
  }
  out <- as.data.frame(out)
  names(out) <- c("Y", paste0("lag_", lags))
  
  out <- out[(length(lags)+1):length(time((tsSet))),]
  
  names(scales) <- c("center", "scale")
  
  return(list(xy = as.data.frame(out), scales = scales))
}

lags <- 1:4
xy_train <- makeVarsTS_2(EBSP_FOB_train_ts, lags = lags)
xy_valid <- makeVarsTS_2(EBSP_FOB_valid_ts, lags = lags)

pacman::p_load("RSNNS")

inTrain <- xy_train$xy[, 2:(length(lags)+1)]
outTrain <- xy_train$xy[, 1]
EBSP_FOB_rsnns <- elman(inTrain,
                        outTrain,
                        # size = c(3, 2),
                        # size = (length(lags)-1),
                        size = c(length(lags), length(lags)-1),
                        learnFuncParams = c(0.1),
                        maxit = 5000)
EBSP_FOB_rsnns
summary(EBSP_FOB_rsnns)

plotIterativeError(EBSP_FOB_rsnns) 

# WITH TRAIN
EBSP_FOB_rsnns.pred <- predict(EBSP_FOB_rsnns, inTrain) 

EBSP_FOB_rsnns.pred <- EBSP_FOB_rsnns.pred * xy_train$scales["scale"] + xy_train$scales["center"]
EBSP_FOB_rsnns.pred <- ts(EBSP_FOB_rsnns.pred,
                         start = startTrain + c(0, max(lags)+1), frequency = f)
accuracy(outTrain, EBSP_FOB_rsnns.pred)

plot(EBSP_FOB_rsnns.pred,
     xlim = c(1986, 2017), ylim = c(0, 150),
     main = "Europe Brent Spot Price FOB",
     ylab = "Dollars per Barrel", bty = "l", lty = 2, lwd = 2)

lines(EBSP_FOB_train_ts, lty = 1, lwd = 2, col = "blue")
lines(EBSP_FOB_valid_ts, lty = 2)

# WITH TEST
inValid <- xy_valid$xy[, 2:(length(lags)+1)]
outValid <- xy_valid$xy[, 1]
EBSP_FOB_rsnns.pred <- predict(EBSP_FOB_rsnns, inValid) 

EBSP_FOB_rsnns.pred <- EBSP_FOB_rsnns.pred * xy_valid$scales["scale"] + xy_valid$scales["center"]
EBSP_FOB_rsnns.pred <- ts(EBSP_FOB_rsnns.pred,
                          start = startValid + c(0, max(lags)+1), frequency = f)
accuracy(outValid, EBSP_FOB_rsnns.pred)

lines(EBSP_FOB_rsnns.pred, lty = 2, col = "red")

## Probatina 2
set.seed(42)
library(quantmod)
getSymbols('^GSPC', from='1990-01-01')
GSPC <- adjustOHLC(GSPC, symbol.name='^GSPC')
GSPC <- to.monthly(GSPC, indexAt='lastof')
Target <- scale(Cl(GSPC))
scales <- c(attr(Target, "scaled:center"), attr(Target, "scaled:scale"))
names(scales) <- c("center", "scale")
f <- 12

# Calculate some technical indicators
periods <- c(1, 2, 3, 6, 9, 12)

Lags <- data.frame(lapply(periods, function(x) Lag(Target, x))) 

allData <- na.omit(cbind(Lags, Tgt = as.numeric(Target)))
dates <- as.Date(rownames(allData))
require(lubridate)
allData_ts <- ts(allData, 
                 start = c(year(dates[1]), 
                           month(dates[1])),
                 end = c(year(dates[length(dates)]),
                         month(dates[length(dates)])),
                 frequency = f)
allData_zoo <- as.zoo(allData_ts) 

nTotal <- nrow(allData_zoo)
nTrain <- as.integer(0.8*nTotal)
nValid <- nTotal - nTrain

startTrain <- start(allData_ts)
endTrain   <- startTrain + c(0, (nTrain-1))
startValid <- endTrain + c(0, 1)
endValid   <- startValid + c(0, nValid-1)

tgtTrain <- window(allData_zoo[, "Tgt"], start = startTrain, end = endTrain)
tgtValid <- window(allData_zoo[, "Tgt"], start = startValid, end = endValid)

lagsTrain <- window(allData_zoo[, 1:(length(periods)-1)],
                    start = startTrain, end = endTrain)
lagsValid <- window(allData_zoo[, 1:(length(periods)-1)],
                    start = startValid, end = endValid)

GSPC_rsnns <- elman(x = lagsTrain,
                    y = tgtTrain,
                    # size = c(ncol(Lags), ncol(Lags)-1),
                    # size = c(4),
                    size = c(4),
                    learnFuncParams = c(0.001),
                    maxit = 10000)
GSPC_rsnns
summary(GSPC_rsnns)

plotIterativeError(GSPC_rsnns) 

plot((c(tgtTrain, tgtValid)*scales["scale"])+scales["center"], 
     main = "Google (GSPC)", xlab = "Time", ylab = "US $", lty = 1, lwd = 1)

# WITH TRAIN
GSPC_rsnns.tr_pred <- ts(predict(GSPC_rsnns, lagsTrain)*scales["scale"]+scales["center"], 
                         start = startTrain, end = endTrain, frequency = f)
accuracy(GSPC_rsnns.tr_pred, tgtTrain*scales["scale"]+scales["center"])

lines(GSPC_rsnns.tr_pred, col = "green")

# With valid
GSPC_rsnns.va_pred <- ts(predict(GSPC_rsnns, lagsValid)*scales["scale"]+scales["center"],
                         start = startValid, end = endValid, frequency = f)
accuracy(GSPC_rsnns.va_pred, tgtValid*scales["scale"]+scales["center"])

lines(GSPC_rsnns.va_pred, lty = 1, col = "blue")
