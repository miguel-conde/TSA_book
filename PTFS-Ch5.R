### 5. SMOOTHING METHODS

# Here we'll describe methods for forecasting time series that rely on
# smoothing.
# Smoothing is based on averaging values over multiple periods to reduce
# noise (= high frequencies, so we'll use low pass filters).
# We start with:
# - The moving average (MA)
# - Simple exponential smoothing
# Both are suitable for forecasting series that contain:
# - No trend
# - Nor seasonality.
# Smoothing methods are:
# - Data driven
# - Able to adapt to changes in the series' pattern over time.
# - Highly automatizable, though the user must specify smoothing constants


# 5.2 Moving Average (MA) -------------------------------------------------

# Centered moving average (CMA): powerful for visualizing trends, not good for 
# forecasting as it is not causal
# Trailing moving average (TMA): less powerful for visualizing trends but useful
# for forecasting

# CMA window width in a series is straightforward: the length of a
# seasonal cycle.
library(readxl)
Amtrak_data <- 
  read_excel("~/PROYECTOS DATA SCIENCE/TSA_book/DATA/AmtrakPassengersMonthly T-Competition.xls", 
             sheet = "Data")
ridership_ts <- ts(Amtrak_data$Ridership, 
                   start = c(1991,1), end = c(2004, 3),
                   frequency = 12)

library(forecast)
ma_CMA <- ma(ridership_ts, order = 12)

plot(ridership_ts,
     ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n",
     main="")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))
lines(ma_CMA, lwd = 2)


# TMA
library(zoo)
ma_TMA <- rollmean(ridership_ts, k = 12, align = "right")
lines(ma_TMA, lwd = 2, lty = 2)
legend(1994, 2300, c("Ridership", "CMA", "TMA"),
       lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = "n")

# Next we illustrate a 12-month TMA forecaster.
# Validation period: last 36 months
# Window: 12
# The forecasts for all months in the validation period are identical
# because the method assumes that information is known only until march 2001. 
# In other words: the validation forecasts are NOT roll-forward next month 
# forecasts
partitions <- tsDataPartition(ridership_ts, nValid = 36)

ma_TMA <- rollmean(partitions$train_ts, k = 12,align = "right")
last_ma <- tail(ma_TMA, 1)

ma_TMA_pred <- ts(rep(last_ma, nValid), 
                  start = end(partitions$train_ts) + c(0 ,1),
                  end = end(partitions$train_ts) + c(0, length(partitions$valid_ts)),
                  freq = 12)

plot(partitions$train_ts,
     ylab = "Ridership", xlab = "Time",
     xlim = c(1991, 2006.25),
     ylim = c(min(ridership_ts), max(ridership_ts)),
     bty = "l", xaxt = "n",
     main="")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ma_TMA, lwd = 2, col = "blue")
lines(ma_TMA_pred, lwd = 2, col = "blue", lty = 2)
lines(partitions$valid_ts)
abline(v = end(partitions$train_ts))
abline(v = end(partitions$valid_ts))

# In general the TMA can be used for forecasting only in series that lack
# - Trend
# - Seasonality
# Methods for de-trending and deseasonalizing:
# - Regression models
# - Advanced exponential smoothing
# - Differencing



# 5.3 Differencing --------------------------------------------------------

# First difference = lag-1 difference: y_t - y_(t-1)
# lag-k difference: y_t - y_(t-k)

# De-trending
# Lag-1 differencing is useful for removing a trend.
# For quadratic and exponential trends, often another round of lag-1
# differencing must be applied in order to remove the trend.

lag_1_ridership_ts <- diff(ridership_ts, lag = 1)

old_par <- par()
par(mfrow = c(2,1))
plot(ridership_ts,
     ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n",
     main="Ridership")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))

plot(lag_1_ridership_ts,
     ylab = "Lag-1", xlab = "Time",
     bty = "l", xaxt = "n",
     main="Difference-1")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))
par(old_par)

# Deseasonalizing
# For removing a seasonal pattern wit M seasons, we difference at lag M
# lag-7 difference for removing a day-of-week pattern in daily data
# lag-12 difference for year pattern in monthly data

lag_12_ridership_ts <- diff(ridership_ts, lag = 12)

old_par <- par()
par(mfrow = c(2,1))
plot(ridership_ts,
     ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n",
     main="Ridership")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))

plot(lag_1_ridership_ts,
     ylab = "Lag-12", xlab = "Time",
     bty = "l", xaxt = "n",
     main="Difference-12")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))
par(old_par)

# Removing Trend and Seasonality
# We difference twice. In our example:
# Differencing the original series at lag-12 to remove seasonality
# Differencing the differenced series at lag-1 to remove trend
lag_12_1_ridership_ts <- diff(diff(ridership_ts, lag = 12), 1)

old_par <- par()
par(mfrow = c(2,1))
plot(ridership_ts,
     ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n",
     main="Ridership")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))

plot(lag_12_1_ridership_ts,
     ylab = "Lag-12-1", xlab = "Time",
     bty = "l", xaxt = "n",
     main="Difference-12-1")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))
par(old_par)


# Simple Exponential Smoothing (SES) --------------------------------------
# The idea is similar to forecasting with a TMA, except that instead of
# taking a simple average over the w most recent values, we take a _weighted
# average of all_ past values, so that the weights decrease exponentially
# into the past.
# The idea is to give more weight to recent information, yet not completely
# ignore older information.
#
# Like the MA, simple exponential smoothing (SES) should only be used for 
# forecasting series that have no trend nor seasonality.
#
# F_(t+1) = ay_t + a(1-a)y_(t-1) + a(1-a)^2y_(t-2) + ...
# a = smoothing constant, 0 < a < 1
# A very useful formulation is:
# F_(t+1) = F_t + ae_t
# where e_t is the forecast error at time t: e_t = y_t - F_t
# We can see:
# - SAS is an "active learner"
# - Advantages in terms of data storage and computation times
#
# As the series is assumed to lack trend and seasonality, forecasts into the
# future rely only on info that is availabble at the time of prediction.
# Hence, the k-step-ahead forecast is F_(t+k) = F_(t+1)
#
# alpha values close to 1 indicates fast learning whereas a value close to
# 0 indicates slow learning. The alpha value that optimizes 1-step-ahead 
# predictive accuracy can be used to determine the degree of local versus
# global nature of the level.
#
# forecast::ets()
# error,trend, seasonality
# ANN = Additive error, No trend, No seasonality
lag_12_1_ridership_ts <- diff(diff(ridership_ts, lag = 12), 1)
nValid <- 36
partitions <- tsDataPartition(lag_12_1_ridership_ts, nValid = nValid)

ses <- ets(partitions$train_ts, model = "ANN", alpha = 0.2)
summary(ses)
ses.pred <- forecast(ses, h = nValid, level = 0)
ses.pred

plot(ses.pred,
     ylab = "Ridership (Twice-Differenced)", xlab = "Time",
     xlim = c(1991, 2006.25),
     ylim = c(min(lag_12_1_ridership_ts), max(lag_12_1_ridership_ts)),
     bty = "l", xaxt = "n",
     flty = 2, main="")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(partitions$valid_ts)
abline(v = end(partitions$train_ts), col = "red")
abline(v = end(partitions$valid_ts), col = "red")

# Now we'll let ets() to choose the optimal alpha
ses_opt <- ets(partitions$train_ts, model = "ANN")
summary(ses_opt)

# Compare ses and ses_opt models:
accuracy(ses.pred, partitions$valid_ts)

ses.pred.opt <- forecast(ses_opt, h = nValid, level = 0)
accuracy(ses.pred.opt, partitions$valid_ts)

ses_opt

# The optimally chosen alpha is virtually zero; taht means that the level 
# is global
# The global level (aldo the initial state) is 1.6583. Because the level is 
# global, it is the model's forecast for each month in the validation 
# period.
#
# MA and SES: both are approximately equal if the window width of the MA 
# is equal to w = (2/alpha) - 1


# Advanced Exponential Smoothing (AES) ------------------------------------
# MA and SES can be used to forecast series with just a level and noise.
# If the series shows, in addition, a trend and/or seasonality, we can first 
# remove those components.
# Another solution is to use AES, which can capture trend and seasonality.

## SERIES with an ADDITIVE TREND
# For this kind of series we can use DOUBLE EXPONENTIAL SMOOTHING (DES), aka
# HOLTS's LINEAR TREND model.
# The trend in this model is not assumed to be global, but rather it can 
# change over time. 
# - The local trend is estimated from the data and is updated as more data 
#   becomes available.
# - Similar to SES, the level is also estimated from the data and is updated 
#   as more data becomes available.
# - The k-step-ahead forecast is given by combining:
#      * The level estimate at time t (L_t)
#      * And the trend estimate (assumed additive) at time t (T_t)
#
#                F_(t+k) = L_t + k x T_t
#
# Now, in the presence of a trend, the k-steps ahead forecast are no longer
# identical because the level and the trend are updated through:
#
#                L_t = alpha x y_t + (1 - alpha) x (L_(t-1) + T_(t-1))
#                T_t = beta x (L_t - L_(t-1)) + (1 - beta) x T_(t-1)
#
# Type of errors
# Additive error
#    y_(t+1) = L_t + T_t + e
#    (e_t = constant for all t = e)
# Multiplicative error
#    y_(t+1) = (L_t + T_t) x (1 + e_t)
#    The error act as a % increase 
#
## SERIES with a MULTIPLICATIVE TREND
#  Now:
#        F_(t+k) = L_t x (T_t)^k
# and:
#        L_t = alpha x y_t + (1 - alpha) x (L_(t-1) + T_(t-1))
#        T_t = beta x (L_t / L_(t-1)) + (1 - beta) x T_(t-1)
# The additive error takes the form:
#    y_(t+1) = L_t x T_t + e
# and the multiplicative:
#    y_(t+1) = (L_t x T_t) x (1 + e_t)
#


## SERIES with a TREND and SEASONALITY
# For this kind of series we can use the HOLT-WINTER's EXPONENTIAL SMOOTHING
# method.


## EXAMPLE: our raw Amtrak ridership data contains both a trend and 
# monthly seasonality
plot(ridership_ts,
     ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n",
     main="")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))

# Let's build an ets forecasting model assuming aditive trend and seasonality
# and including a multiplicative error (i.e., ETS = "MAA")
nValid <- 36
partitions <- tsDataPartition(ridership_ts, nValid = nValid)

hwin <- ets(partitions$train_ts, model = "MAA")
hwin_pred <- forecast(hwin, h = nValid, level = 0)

plot(hwin_pred,
     ylab = "Ridership", xlab = "Time",
     xlim = c(1991, 2006.25),
     ylim = c(min(ridership_ts), max(ridership_ts)),
     bty = "l", xaxt = "n",
     flty = 2, main="")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(partitions$valid_ts)
abline(v = end(partitions$train_ts), col = "red")
abline(v = end(partitions$valid_ts), col = "red")


hwin
head(hwin$states, 1)
tail(hwin$states, 1)

plot(1:12, head(hwin$states[, paste0("s", 1:12)], 1), type = "l",
     xlab = "States", ylab = "Ridership")
lines(1:12, tail(hwin$states[, paste0("s", 1:12)], 1), type = "l", col = "red")
abline(h = 0, col = "black", lty = 2)


plot(hwin$states[,"l"] + hwin$states[,"s1"], col = "blue", lty = 2)
lines(train_ts)
