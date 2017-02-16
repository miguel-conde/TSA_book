### 7. REGRESSION-BASED MODELS: AUTOCORRELATION & EXTERNAL INFORMATION
# OBJECTIVES:
#   - How to use a regression model to quantify the correlation between
#     neighbouring values in a time series (autocorrelation).
#   - Useful for:
#        * Improving forecast accuracy using info contained in the 
#          autocorrelation (beyond trend and seasonality).
#        * Evaluating the predictability of a series (wether a series is a
#          "random walk" or not)
library(readxl)
Amtrak_data <- 
  read_excel("./DATA/AmtrakPassengersMonthly T-Competition.xls", 
             sheet = "Data")
ridership_ts <- ts(Amtrak_data$Ridership, 
                   start = c(1991,1), end = c(2004, 3),
                   frequency = 12)
plot(ridership_ts)

## 7.1 AUTOCORRELATION
# lag-1 autocorrelation
library(zoo)
ridership_zoo <- zoo(ridership_ts)
h <- -1
lag_ridership_zoo <- cbind(ridership_zoo, lag(ridership_zoo, k = h))
names(lag_ridership_zoo) <- paste0("lag_", 0:1)
head(lag_ridership_zoo)
cor(lag_ridership_zoo, use = "complete.obs")

acf(ridership_ts, plot=FALSE)$acf[2]
library(forecast)
Acf(ridership_ts, plot = FALSE)$acf[2]

# 12 lags autocorrrelation
ridership_24_ts <- window(ridership_ts, start = start(ridership_ts),
                         end = start(ridership_ts) + c(0, 23))
Acf(ridership_24_ts,lag.max = 12, main = "")
Acf(ridership_24_ts,lag.max = 12, main = "", plot = FALSE)

#lag_ridership_24_ts <- ridership_24_ts
lag_ridership_24_ts <- sapply(0:-12, function(x) {
  out <- cbind(ridership_24_ts, lag(ridership_24_ts, k = x))
  dimnames(out)[[2]]  <- paste0("lag_", c(0, -x))
  out
})
names(lag_ridership_24_ts) <- paste0("lag_", 0:12)

my_acf <- sapply(lag_ridership_24_ts, function(x) {
  cor(x[, 1], x[, 2], use = "complete.obs")
  })
plot(my_acf, type = "h")
abline(h = 0)

Acf(ridership_24_ts,lag.max = 12, main = "", lwd = 5, col = "lightblue")
lines(my_acf[-1], type = "h", lty = 20, col ="red")

## Some typical autocorrelation behaviors
# Strong autocorrelation at multiples of a lag larger than 1 =>
# => cyclical pattern (e.g., high ac at 12, 24, 36 => anual seasonality)
Acf(ridership_24_ts,lag.max = 12, main = "", lwd = 5, col = "lightblue")

## Positive lag-1 ac ("stickiness"): consecutive values move generally in
# the same direction

## Negative lag-1 ac: swings in the series, consecutive values move 
# generally in the opposite direction or high values are followed by low ones

# Example
Acf(ridership_24_ts,lag.max = 12, main = "", lwd = 5, col = "lightblue")
# The strongest ac is negative and at lag 6: biannual pattern, with 6-month
# witches from high to low ridership (high summer, low winter)
plot(window(ridership_ts, start = start(ridership_ts),
            end = start(ridership_ts) + c(0, 47)))

# AC of residual series: if we have right modeled the seasonal pattern, the
# residual series should show no ac at the season lag.
nValid <- 36
nTrain <- length(ridership_ts) - nValid
train_ts <- window(ridership_ts, 
                   start = c(1991, 1), end = c(1991, nTrain))
valid_ts <- window(ridership_ts, 
                   start = c(1991, nTrain+1), end = c(1991, nTrain+nValid))

train_lm_trend_season <- tslm(train_ts ~ trend + I(trend^2) + season)
summary(train_lm_trend_season)

Acf(residuals(train_lm_trend_season),lag.max = 12, main = "", lwd = 5, 
    col = "lightblue")
# It's clear that the 6-month (nor 12-month) cyclical behaiour no longer
# dominates the series of residuals: the regression model captured them right
# The still high lag-1 ac is valuable info that can be used to improve
# forecasting

## IMPROVING FORECASTS BY CAPTURING AUTOCORRELATION: AR and ARIMA MODELS

## AR MODELS
# Regression models whose predictors are the past values of the series
# x_t = beta_0 + beta_1 x y_t-1 + beta_2 x y_t-1 + error_t

# 2 approaches to take advante of autocorrelation:
# - Directly buiding the ac into the regression model using ARIMA models
# - Constructing a simple 2nd-oreder level forecasting model on the residual
#   series

# AR as a 2nd-layer Model
# 1 - Generate a k-stp-ahead Forecast F_t+k using some forecasting method
# 2 - Generate k-step-ahead of the forecast error e_t+k using an AR 
#     (or other) model
# 3 - Improve the initial model adjusting it acoording to the second model:
#              Improved F_t+k = F_t+k + e_t+k
# 4 - Examine the acf of the series of residuals-of-residuals to test if
#     we have indeed accounted for the ac in the series and that no more
#     info remains in the series.

# To fit a AR model:
# a - Examine acf
# b - Choose the order of the AR model according to the lags in which ac
#     appears. Often, if ac exists at lag-1 and higher, an AR(1) is 
#     sufficient. The reason is that if neighboring values are correlated,
#     then the relationship can propagate to values that are at k (>1) 
#     periods apart.
# c - To fit an AR(p) model we use an ARIMA model of order (p,0,0)
#     (p, d, q) refers to:
#         p: number of AR terms in the model (AR order)
#         q: number of MA terms in the model (MA order)
#         d: number of integrated terms (number of times the series id
#            differenced before an ARMA model is applied)
#
# Reference: https://www.otexts.org/fpp/8
#

# Example. AR(1) model to the Amtrak ridership residual series
train_res_arima <- Arima(residuals(train_lm_trend_season),
                         order = c(1, 0, 0))
summary(train_res_arima)
train_res_arima_pred <- forecast(train_res_arima)

plot(residuals(train_lm_trend_season), 
     xlim = c(1991, 2006.25), ylim = c(-250, 250),
     ylab = "Residuals", xlab = "Time", xaxt = "n")
axis(1, at = seq(1991, 2006, 1), labels = format( seq(1991, 2006, 1)))
lines(fitted(train_res_arima_pred), lwd = 2, col = "blue")
abline(v = 2001.25, col = "red", lty = 2)
abline(v = 2004.25, col = "red", lty = 3)

Acf(residuals(train_res_arima),lag.max = 12, main = "", lwd = 5, 
    col = "lightblue")


# Now we have the model, let's make sonme forecasting
# 1st level model (cuadratic trend + season) forecasting
train_lm_trend_season_pred <- forecast(train_lm_trend_season, h = nValid, level = 0)

plot(train_lm_trend_season_pred, xlab = "Time", ylab = "Ridership", bty = "l",
     xaxt = "n", main = "", flty = 2,
     xlim = c(1991, 2006.25), ylim = c(1300, 2600))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train_lm_trend_season_pred$fitted, lwd = 2, col = "blue")
lines(valid_ts)
abline(v = 2001.25, col = "red", lty = 3)
abline(v = 2004.25, col = "red", lty = 3)

# 2nd order level model, 1st level model + AR(1) model
train_res_arima_pred <- forecast(train_res_arima, h = nValid, level = 0)

plot(ridership_ts, xlab = "Time", ylab = "Ridership", bty = "l",
     xaxt = "n", main = "",
     xlim = c(1991, 2006.25), ylim = c(1300, 2600))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
abline(v = 2001.25, col = "red", lty = 3)
abline(v = 2004.25, col = "red", lty = 3)

fitted_v <- fitted(train_lm_trend_season_pred) + fitted(train_res_arima_pred)
fitted_m <- train_lm_trend_season_pred$mean + train_res_arima_pred$mean

lines(fitted_v, lwd = 2, col = "blue")
lines(fitted_m, lwd = 2, col = "blue", lty = 2)

# 1st order accuracy
accuracy(fitted(train_lm_trend_season_pred), train_ts)
accuracy(train_lm_trend_season_pred$mean, valid_ts)

# 2nd order accuracy
accuracy(fitted_v, train_ts)
accuracy(fitted_m, valid_ts)

# Improving forecasts via an aditional AR layer is useful for SHORT TERM
# FORECASTING.

## ARIMA MODELS






## EVALUATING PREDICTABILITY
library(tseries)
adf.test(ridership_ts)
# So we reject H_0 (H_0: ridership_ts is not stationary) and turn to H_a:
# ridership_ts is stationary (i.e., it's not a random walk)

Acf(diff(ridership_ts)) # some ac out of the thresholds => no random walk


library(quantmod)
getSymbols('^GSPC', from='1990-01-01')
GSPC <- adjustOHLC(GSPC, symbol.name='^GSPC')
GSPC <- to.monthly(GSPC, indexAt='lastof')
adf.test(Cl(GSPC)) # Non stationary
adf.test(ClCl(GSPC)[-1]) # Stationary


Acf(diff(Cl(GSPC))) # some ac out of the thresholds => not a random walk
Acf(ClCl(GSPC)) # no ac out of the thresholds => random walk
