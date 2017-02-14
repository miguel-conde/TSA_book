### 3. PERFORMANCE EVALUATION


# 3.1 DATA PARTITIONING ---------------------------------------------------

library(readxl)
Amtrak_data <- 
  read_excel("~/PROYECTOS DATA SCIENCE/TSA_book/DATA/AmtrakPassengersMonthly T-Competition.xls", 
             sheet = "Data")
ridership_ts <- ts(Amtrak_data$Ridership, 
                   start = c(1991,1), end = c(2004, 3),
                   frequency = 12)
nValid <- 36
nTrain <- length(ridership_ts) - nValid
train_ts <- window(ridership_ts, 
                   start = c(1991, 1), end = c(1991, nTrain))
valid_ts <- window(ridership_ts, 
                   start = c(1991, nTrain+1), end = c(1991, nTrain+nValid))

library(forecast)
ridership_lm <- tslm(train_ts ~ trend + I(trend^2))
ridership_lm_pred <- forecast(ridership_lm, h = nValid, level = 0)

plot(ridership_lm_pred, 
     ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership_lm$fitted.values, lwd = 2)
lines(valid_ts)

# 3.2 NAIVE FORECASTS -----------------------------------------------------


# 3.3 MEASURING PREDICTIVE ACCURACY ---------------------------------------

accuracy(ridership_lm_pred$mean, valid_ts)


# 3.4 EVALUATING FORECAST UNCERTAINTY -------------------------------------


# 3.4.1 DISTRIBUTION OF FORECAST ERRORS
names(ridership_lm_pred)
# Training period resuduals (= forecast errors)
ridership_lm_pred$residuals
# Validation period residuals (= forecast errors)
valid_ts - ridership_lm_pred$mean

hist(ridership_lm_pred$residuals, 
     ylab = "Frequency", xlab = "Forecast Error",
     bty = "l", main = "")

# PREDICTION INTERVALS
# If forecast errors were normally distributed (level = 95):
ridership_lm_pred <- forecast(ridership_lm, h = nValid, level = 95)

plot(ridership_lm_pred, 
     ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", flty = 2, main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership_lm$fitted.values, lwd = 2)
lines(valid_ts)

# But normally they aren't normally distributed (see histogrem above)
# A 90% prediction interval can be constructed by using the 5th and 95th
# percentiles from the sample of forecast errors
q95 <- quantile(ridership_lm_pred$residuals, probs = c(0.05, 0.95))
q95

ridership_lm_pred <- forecast(ridership_lm, h = nValid, level = 0)

plot(ridership_lm_pred, 
     ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", flty = 2, main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership_lm$fitted.values + q95["95%"], lwd = 2, lty = 2)
lines(ridership_lm$fitted.values, lwd = 2)
lines(ridership_lm$fitted.values + q95["5%"], lwd = 2, lty = 2)

q95 <- quantile(valid_ts - ridership_lm_pred$mean, probs = c(0.05, 0.95))
lines(ridership_lm_pred$mean + q95["95%"], lwd = 2, lty = 2, col = "blue")
lines(valid_ts)
lines(ridership_lm_pred$mean + q95["5%"], lwd = 2, lty = 2, col = "blue")

# PREDICTION CONES
# Model 1
ridership_ts_ets_AAN <- ets(ridership_ts, model = "AAN") 
# Model 2
ridership_ts_ets_MMN   <- ets(ridership_ts, model = "MMN", damped = FALSE) 
# Model 3
ridership_ts_ets_MMdN <- ets(ridership_ts, model = "MMN", damped = TRUE)

ridership_ts_ets_AAN_pred <- forecast(ridership_ts_ets_AAN, h = 115,
                                      level = c(0.2, 0.4, 0.6, 0.8))
ridership_ts_ets_MMN_pred <- forecast(ridership_ts_ets_MMN, h = 115,
                                      level = c(0.2, 0.4, 0.6, 0.8))
ridership_ts_ets_MMdN_pred <- forecast(ridership_ts_ets_MMdN, h = 115,
                                      level = c(0.2, 0.4, 0.6, 0.8))

par(mfrow = c(1,3))
plot(ridership_ts_ets_AAN_pred,
     xlab = "Time")
plot(ridership_ts_ets_MMN_pred,
     xlab = "Time")
plot(ridership_ts_ets_MMdN_pred,
     xlab = "Time")
par(mfrow = c(1,1))


# 3.5 ADVANCED DATA PARTITIONING: Roll-forward validation -----------------

# Fixed training and validation periods:
# TRAINING PERIOD: t = 1, 2,..., n      - Models are built on this period
# VALIADATION PERIOD: t = n+1, n+2, ... - Models performaces are assesed on this period

# Roll-forward validation period: multiple training-validation partitions
# created by moving the partitioning one period at a time.
# Example: In the monthly ridership example we can create multiple data
# partitions by rolling forward one month at a time:
library(caret)
tps <- createTimeSlices(ridership_ts,
                        initialWindow = 123, 
                        horizon = 12,
                        fixedWindow = TRUE)

# Partitions
for (part in 1:length(tps$train))
{
  trPartIdx  <- tps$train[[part]]
  tstPartIdx <- tps$test[[part]]
  
  # Training period
  print(window(ridership_ts, 
               start = head(time(ridership_ts)[trPartIdx], 1),
               end   = tail(time(ridership_ts)[trPartIdx], 1)))
  # Validation period
  print(window(ridership_ts, 
               start = head(time(ridership_ts)[tstPartIdx], 1),
               end   = tail(time(ridership_ts)[tstPartIdx], 1)))
}

partList <- list()

for (part in 1:length(tps$train))
{
  trPartIdx  <- tps$train[[part]]
  tstPartIdx <- tps$test[[part]]
  
  item <- list()
  # Training period
  item[["train"]] <- window(ridership_ts, 
               start = head(time(ridership_ts)[trPartIdx], 1),
               end   = tail(time(ridership_ts)[trPartIdx], 1))
  # Validation period
  item[["test"]] <- window(ridership_ts, 
               start = head(time(ridership_ts)[tstPartIdx], 1),
               end   = tail(time(ridership_ts)[tstPartIdx], 1))
  partList[[paste0("Partition_", part)]] <- item
}

createRollForwardPartitions <- function(x, timeSlicesList) {
  partList <- list()
  
  for (part in 1:length(timeSlicesList$train))
  {
    trPartIdx  <- timeSlicesList$train[[part]]
    tstPartIdx <- timeSlicesList$test[[part]]
    
    item <- list()
    # Training period
    item[["train"]] <- window(x, 
                              start = head(time(x)[trPartIdx], 1),
                              end   = tail(time(x)[trPartIdx], 1))
    # Validation period
    item[["test"]] <- window(x, 
                             start = head(time(x)[tstPartIdx], 1),
                             end   = tail(time(x)[tstPartIdx], 1))
    partList[[paste0("Partition_", part)]] <- item
  }
  return(partList)
}

ridership_RF <- createRollForwardPartitions(ridership_ts, tps)

start(ridership_RF$Partition_1$train)
end(ridership_RF$Partition_1$train)
start(ridership_RF$Partition_1$test)
end(ridership_RF$Partition_1$test)

start(ridership_RF$Partition_2$train)
end(ridership_RF$Partition_2$train)
start(ridership_RF$Partition_2$test)
end(ridership_RF$Partition_2$test)

######### Esta es la buena
partList <- list()

initTrainingPeriod <- 123
for(trainingPeriod in initTrainingPeriod:(length(ridership_ts)-1)) {
  tps <- createTimeSlices(ridership_ts,
                          initialWindow = trainingPeriod, 
                          horizon = length(ridership_ts) - trainingPeriod,
                          fixedWindow = TRUE)
  item <- list()
  item$train <- tps$train[[1]]
  item$test <- tps$test[[1]]
  
  partList[[paste0("Partition_", 
                   trainingPeriod - initTrainingPeriod + 1)]] <- item
}

lapply(partList, function(x) {
  list(train = window(ridership_ts, 
                      start = head(time(ridership_ts)[x[[1]]], 1),
                      end   = tail(time(ridership_ts)[x[[1]]], 1)),
       test = window(ridership_ts, 
                     start = head(time(ridership_ts)[x[[2]]], 1),
                     end   = tail(time(ridership_ts)[x[[2]]], 1)))
})


# 3.6 EXAMPLE: COMPARING TWO MODELS ---------------------------------------

### FIXED PARTITIONING
# Validation period = Apr 2001 - Mar 2004
fixed_nValid <- 36
fixed_nTrain <- length(ridership_ts) - fixed_nValid

train_ts <- window(ridership_ts, 
                   start = c(1991, 1), end = c(1991, fixed_nTrain))
test_ts  <- window(ridership_ts, 
                   start = c(1991, fixed_nTrain + 1), 
                   end = c(1991, fixed_nTrain + fixed_nValid))

naive_pred  <-  naive(train_ts, h = fixed_nValid)
snaive_pred <- snaive(train_ts, h = fixed_nValid)

accuracy(naive_pred,  valid_ts)
accuracy(snaive_pred, valid_ts)

### ROLL-FORWARD PARTITIONING
fixed_nValid <- 36
fixed_nTrain <- length(ridership_ts) - fixed_nValid

# Roll-forward one month ahead
stepsAhead <- 1
# Number of stepsAhead forecasts
numStepsAheadFC <- fixed_nValid - stepsAhead + 1
  
error         <- rep(0, numStepsAheadFC)
percent_error <- rep(0, numStepsAheadFC)

for (j in fixed_nTrain:(fixed_nTrain + fixed_nValid - stepsAhead)) {
  train_ts <- window(ridership_ts,
                     start = c(1991, 1),
                     end   = c(1991, j))
  valid_ts <- window(ridership_ts,
                     start = c(1991, j + stepsAhead),
                     end   = c(1991, j + stepsAhead))
  
  naive_pred <- naive(train_ts, h = stepsAhead)
  
  error[j - fixed_nTrain + 1] <- valid_ts - naive_pred$mean[stepsAhead]
  percent_error[j - fixed_nTrain + 1] <- error[j - fixed_nTrain + 1] / valid_ts
}

# MAE
mean(abs(error))

# RMSE
sqrt(mean(error^2))

# MAPE
mean(abs(percent_error))

# FUNCTION for k stepsAhead

RFP_performance <- function(x, fixed_nValid = 36, 
                            start = c(1991, 1), stepsAhead = 1,
                            fcFUN = naive) {
  
  fixed_nTrain <- length(x) - fixed_nValid
  
  # Number of stepsAhead forecasts
  numStepsAheadFC <- fixed_nValid - stepsAhead + 1
  
  error         <- rep(0, numStepsAheadFC)
  percent_error <- rep(0, numStepsAheadFC)
  
  for (j in fixed_nTrain:(fixed_nTrain + fixed_nValid - stepsAhead)) {
    train_ts <- window(x,
                       start = start,
                       end   = c(1991, j))
    valid_ts <- window(x,
                       start = c(1991, j + stepsAhead),
                       end   = c(1991, j + stepsAhead))
    
    fc <- fcFUN(train_ts, h = stepsAhead)
    
    error[j - fixed_nTrain + 1] <- valid_ts - fc$mean[stepsAhead]
    percent_error[j - fixed_nTrain + 1] <- error[j - fixed_nTrain + 1] / valid_ts
  }
  
  return(c(MAE  = mean(abs(error)), 
           RMSE = sqrt(mean(error^2)),
           MAPE = mean(abs(percent_error))))
}

# Test it
RFP_performance(ridership_ts, fixed_nValid = 36, 
                start = c(1991, 1), stepsAhead = 1,
                fcFUN = naive)

# Naive forecasting
naive_perf <- 
  t(sapply(1:36, function(x) {
  RFP_performance(ridership_ts, fixed_nValid = 36, 
                  start = c(1991, 1), stepsAhead = x,
                  fcFUN = naive)
}))

naive_perf

apply(naive_perf, 2, mean)

# Seasonal naive forecasting
snaive_perf <- 
  t(sapply(1:36, function(x) {
    RFP_performance(ridership_ts, fixed_nValid = 36, 
                    start = c(1991, 1), stepsAhead = x,
                    fcFUN = snaive)
  }))

snaive_perf

apply(snaive_perf, 2, mean)



# 3.7 Problems ------------------------------------------------------------

## 3.7.1
library(readxl)
SouvenirSales <- read_excel("~/PROYECTOS DATA SCIENCE/TSA_book/DATA/SouvenirSales.xls")
SS_ts <- ts(SouvenirSales$Sales, 
            start = c(1995, 1, 1), end = c(2001, 12, 1),
            frequency = 12)
plot(SS_ts,
     xlab = "Date", ylab = "Sales ($)", 
     main = "Souvenir Sales",
     bty = "l")

library(ggplot2)
plot <- ggplot(SouvenirSales, aes(x = 1:nrow(SouvenirSales), y = Sales)) + 
  geom_line() +
  xlab("Date") +
  ylab("Sales($)") 

plot
plot + scale_y_log10()
plot + scale_y_continuous(trans='log10')
plot + coord_trans(y = "log10")

# Partition the data into the training and validation (the last 12 months of 
# data, year 2001) periods.
nValid <- 12
nTrain <- length(SS_ts) - nValid
ts_start <- start(train_ts)
train_ts <- window(SS_ts, 
                   start = c(ts_start[1], 1), 
                   end = c(ts_start[1], nTrain))
valid_ts <- window(SS_ts, 
                   start = c(ts_start[1], nTrain+1), 
                   end = c(ts_start[1], nTrain+nValid))
start(train_ts)
end(train_ts)
start(valid_ts)
end(valid_ts)

# c) What is the naive forecast for the validation period? (assume that you 
# must provide forescasts for 12 months ahead)
library(forecast)
naive_pred  <-  naive(train_ts, h = 12)
naive_pred
summary(naive_pred)

# d) Compute the RMSE and MAPE for the naive forecast
accuracy(naive_pred)
error = naive_pred$mean - valid_ts

RMSE = sqrt(sum(error^2))
RMSE

percent_error <- error / valid_ts
MAPE <- mean(abs(percent_error))
MAPE

# Plot a histogram of the forecast error that result from the naive forecasts
# (for the validation period). 
hist(error)
hist(percent_error)

# Plot also a time plot for the naive forecasts and the actual sales numbers 
# in the validation period
plot(valid_ts,
     xlab = "Date", ylab = "Sales ($)", 
     main = "Souvenir Sales",
     bty = "l")
lines(naive_pred$mean, col = "red")


plot(naive_pred, xlab = "Date", ylab = "Sales ($)", 
     main = "Souvenir Sales",
     bty = "l")
lines(valid_ts, col = "red")
legend("topleft",  
       c("Train", "Forecast", "Validation"), fill = c("black", "blue", "red"))


args(plot.forecast)
plot(naive_pred, plot.conf = FALSE, ylim = c(0,120000))
lines(valid_ts, col = "red")

# f) The analyst found a forecasting model that gives satisfactory 
# performance on the validation set. What must she do to use the forecasting
# model for generating forecasts for year 2002?

# Let's suppose the other model is a snaive:
snaive_pred  <-  snaive(train_ts, h = 12)
snaive_pred
summary(snaive_pred)

s_error = snaive_pred$mean - valid_ts

s_RMSE = sqrt(sum(s_error^2))
s_RMSE

s_percent_error <- s_error / valid_ts
s_MAPE <- mean(abs(s_percent_error))
s_MAPE

hist(s_error)
hist(s_percent_error)

plot(snaive_pred)
lines(valid_ts, col = "red")

# We now must recalculate the chosen model on all the data:
snaive_pred_all  <-  snaive(SS_ts, h = 12)
snaive_pred_all
summary(snaive_pred_all)
plot(snaive_pred_all)
legend("topleft",
       c("Data 1995-2001", "Forecast 2002"), fill = c("black", "blue"))

