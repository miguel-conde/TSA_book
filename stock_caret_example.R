## https://www.r-bloggers.com/time-series-cross-validation-5/

# Load the dataset, adjust, and convert to monthly returns
set.seed(42)
library(quantmod)
getSymbols('^GSPC', from='1990-01-01')
GSPC <- adjustOHLC(GSPC, symbol.name='^GSPC')
GSPC <- to.monthly(GSPC, indexAt='lastof')
Target <- ClCl(GSPC)

# Calculate some technical indicators
periods <- c(3, 6, 9, 12)

Lags <- data.frame(lapply(c(1:2, periods), function(x) Lag(Target, x)))

EMAs <- data.frame(lapply(periods, function(x) {
  out <- EMA(Target, x)
  names(out) <- paste('EMA', x, sep='.')
  return(out)
}))

RSIs <- data.frame(lapply(periods, function(x) {
  out <- RSI(Cl(GSPC), x)
  names(out) <- paste('RSI', x, sep='.')
  return(out)
}))

# DVIs <- data.frame(lapply(periods, function(x) {
#   out <- DVI(Cl(GSPC), x)
#   out <- out$dvi
#   names(out) <- paste('DVI', x, sep='.')
#   return(out)
# }))

MACDs <- MACD(Cl(GSPC))

#dat <- data.frame(Next(Target), Lags, EMAs, RSIs, DVIs)
dat <- data.frame(Next(Target), Lags, EMAs, RSIs, MACDs)
dat <- na.omit(dat)

# Create a summary function to calculate trade costs and cumulative profit 
# in the test set
mySummary <- function (data, lev = NULL, model = NULL) {
  positions <- sign(data[, "pred"])
  trades <- abs(c(1,diff(positions)))
  profits <- positions*data[, "obs"] + trades*0.01
  profit <- prod(1+profits)
  names(profit) <- 'profit'
  return(profit)
}

#Build the model!
library(caret)

model <- train(dat[,-1], dat[,1], method = 'rf', 
               metric = 'profit', maximize = TRUE,
               trControl = trainControl(
                 method = 'timeslice',
                 initialWindow = 12, fixedWindow = TRUE, 
                 horizon = 12, summaryFunction = mySummary,
                 verboseIter = TRUE))
model

myData <- data.frame(pred = predict(model, dat[,-1]),
                     obs  = dat[, 1])
mySummary(myData)
