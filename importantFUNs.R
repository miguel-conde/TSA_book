### IMPORTANT FUNCTIONS

## Package stats
# ts()
Amtrak_data <- 
  read_excel("~/PROYECTOS DATA SCIENCE/TSA_book/DATA/AmtrakPassengersMonthly T-Competition.xls", 
             sheet = "Data")
ridership_ts <- ts(Amtrak_data$Ridership, 
                   start = c(1991,1), end = c(2004, 3),
                   frequency = 12)
# window()
ridership_ts_zoom <- window(ridership_ts, start = c(1991,1), end = c(2000, 12))

# plot.ts()
plot(ridership_ts,
     xlab = "Time", ylab = "Ridership",
     bty = "l")

## Package forecast
library(forecast)

# tslm()
nValid <- 36
nTrain <- length(ridership_ts) - nValid
train_ts <- window(ridership_ts, 
                   start = c(1991, 1), end = c(1991, nTrain))
valid_ts <- window(ridership_ts, 
                   start = c(1991, nTrain+1), end = c(1991, nTrain+nValid))

ridership_lm <- tslm(ridership_ts ~ trend + I(trend^2))

train_lm_expo <- tslm(train_ts ~ trend, lambda = 0) # Multiplicative seasonality
train_lm_expo_pred <- forecast(train_lm_expo, h = nValid, level = 0)

train_lm_linear <- tslm(train_ts ~ trend, lambda = 1) # Additive seasonality
train_lm_linear_pred <- forecast(train_lm_linear, h = nValid, level = 0)

train_lm_trend_season <- tslm(train_ts ~ trend + I(trend^2) + season)
summary(train_lm_trend_season)

# forecast()
ridership_lm <- tslm(train_ts ~ trend + I(trend^2))
ridership_lm_pred <- forecast(ridership_lm, h = nValid, level = 0)

# plot.forecast()
plot(ridership_lm_pred, 
     ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership_lm$fitted.values, lwd = 2)
lines(valid_ts)

# naive()
naive(train_ts)

# Seasonal naive snaive()
snaive(train_ts)

# rwf()
rwf(train_ts)

# accuracy()
accuracy(ridership_lm_pred$mean, valid_ts)

# Simple Moving Average Smoother - ma()
# Centered Moving Average
ma_CMA <- ma(ridership_ts, order = 12)

plot(ridership_ts,
     ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n",
     main="")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))
lines(ma_CMA, lwd = 2)

# zoo::rollmean()
# Trailing Moving Average
# TMA
library(zoo)
ma_TMA <- rollmean(ridership_ts, k = 12, align = "right")
lines(ma_TMA, lwd = 2, lty = 2)
legend(1994, 2300, c("Ridership", "CMA", "TMA"),
       lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = "n")

# Differencing diff()
diff(ridership_ts, lag = 1)

# ets()
ses <- ets(train_ts, model = "ANN", alpha = 0.2)
summary(ses)
ses.pred <- forecast(ses, h = nValid, level = 0)
ses.pred

ses_opt <- ets(partitions$train_ts, model = "ANN")
summary(ses_opt)

hwin <- ets(train_ts, model = "MAA")

ets(partitions$train_ts, restrict = FALSE, allow.multiplicative.trend = TRUE)

# msts()
bike_hourly_df <- read.csv("./DATA/BikeSharing_hour.csv")
# 1 month (07 2012): 31 days * 24 hours/day = 744 hours
jul12 <- bike_hourly_df$cnt[13004:13747]
# msts() sets up the time series with 2 seasonal periods: hours per day and 
# hours per week
bike_hourly_msts <- msts(jul12, 
                         seasonal.periods = c(24, 168), 
                         start = c(0,1))

# dshw()
# Training and validation sets
nTotal <- length(jul12)
nTrain <- 21 * 24 # 21 days of hourly data
nValid <- nTotal - nTrain # 10 days of hourly data
yTrain_msts <- window(bike_hourly_msts, start = c(0,1), end = c(0, nTrain))
yValid_msts <- window(bike_hourly_msts, 
                      start = c(0, nTrain + 1), end = c(0, nTotal))

# Fit a double seasoned holt-winter's model
bike_hourly_dshw_pred <- dshw(yTrain_msts, h = nValid)
# This is to correct a minor error in forecast package v7.1
bike_hourly_dshw_pred_mean <- msts(bike_hourly_dshw_pred$mean,
                                   seasonal.periods = c(24, 168),
                                   start = c(0, nTrain + 1))
accuracy(bike_hourly_dshw_pred_mean, yValid_msts)

## stl()
library(readxl)
sep11_data <- 
  read_excel("./DATA/Sept11Travel.xls")
sep11_ts <- ts(sep11_data$`Air RPM (000s)`, 
               start = c(1990,1), end = c(2004, 4),
               frequency = 12)
preSep11_ts <- window(sep11_ts, start = start(sep11_ts),
                      end = c(2001, 8))
postSep11_ts <- window(sep11_ts, start = c(2001, 9),
                       end = end(sep11_ts))

hwin_preSep11_stl <- stl(preSep11_ts / 1000000, s.window = "periodic")
hwin_preSep11_noseason <- preSep11_ts/1000000 - hwin_preSep11_stl$time.series[, "seasonal"]
plot(hwin_preSep11_noseason, ylim = c(0, 70))


## Package caret
library(caret)
tps <- createTimeSlices(ridership_ts,
                        initialWindow = 123, 
                        horizon = 12,
                        fixedWindow = TRUE)