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

# 7.1 AUTOCORRELATION -----------------------------------------------------
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
# switches from high to low ridership (high summer, low winter)
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
# It's clear that the 6-month (nor 12-month) cyclical behaviour no longer
# dominates the series of residuals: the regression model captured them right
# The still high lag-1 ac is valuable info that can be used to improve
# forecasting


# 7.2 IMPROVING FORECASTS BY CAPTURING AUTOCORRELATION: AR and ARIMA ------

## AR MODELS
# Regression models whose predictors are the past values of the series
# x_t = beta_0 + beta_1 x y_t-1 + beta_2 x y_t-1 + error_t

# 2 approaches to take advante of autocorrelation:
# - Directly buiding the ac into the regression model using ARIMA models
# - Constructing a simple 2nd-order level forecasting model on the residual
#   series

# AR as a 2nd-layer Model
# 1 - Generate a k-step-ahead Forecast F_t+k using some forecasting method
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

# An ARIMA(p, d, q) model directly models:
# - The autocorrelation of the series at lags 1,2,...,p
#   AR(p):  y_t = beta_0 + beta_1 x y_t-1 + beta_2 x y_t-1 + error_t
# - The autocorrelation of the forecast errors (called "moving average") upto
#   lag q:
#   MA(q): y_t = theta_1 x error_t-1 + theta_2 x error_t-2 +...+ theta_q x error_t-q
#
# AR and ARMA models can only be fitted to data with NO TREND NOR SEASONALITY
#
# - To remove trend a preliminary differencing step is incorporated 
#   I(d): d = 0 means no differencing, d = 1 removes a lineal trenf, d = 2 a
#         quadratic one, etc.
#
#   Seasonal ARIMA models incorporate a preliminary differencing step to 
#   remove trend.
# 


# 7.3 EVALUATING PREDICTABILITY -------------------------------------------

# Before forecast: is a time series predictable? (i.e., its past predicts its
# future?)
# To answer, we can test if the time series is a random walk (a special case of
# AR(1)):
#         y_t = beta_0 + y_t-1 + error_t
#
# To test if a series is a random walk:
# - Fit an AR(1) model
# - Test the hypothesis H_0: beta_1 = 1.
# - If rejected, the series is not a random walk and we can attempt to predict
#   it (beyond a naive forecast).

# Example

summary(train_res_arima)

# The slope coefficient `ar1` is more than 5 standard errors away from 1,
# indicating that it is not a random walk.

# Example
library(quantmod)
getSymbols('^GSPC', from='1990-01-01')
GSPC <- adjustOHLC(GSPC, symbol.name='^GSPC')
summary(Arima(Cl(GSPC), order = c(1, 0, 0)))

# The coefficient is sufficiently close to 1 (around 1 standard error away)
# indicating that it is a random walk

summary(Arima(ClCl(GSPC), order = c(1, 0, 0)))
# The slope coefficient `ar1` is more than 5 standard errors away from 1,
# indicating that it is not a random walk.

## Another approach: 
#  Examine the lag-1 differenced series. As a random walk is a constant plus 
#  a random term, the ACF of a lag-1 differenced series should indicate that 
#  autocorrelations at all lags should be aprox. zero (all the bars within the 
#  thresholds)


## A more advance way; Augmented Dicker-Fuller test, tseries::adf.test()


## Example
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


Acf(diff(Cl(GSPC))) # none ac out of the thresholds =>  a random walk
Acf(diff(ClCl(GSPC))) # lag-1 ac out of the thresholds => not a random walk


# 7.4 INCLUDING EXTERNAL INFORMATION --------------------------------------

# We've seen linear regression models capturing trend, seasonality and
# autocorrelation (via autoregresive models).
# Now we'll learn that they can capture aditional types of patterns: outliers,
# special events, interventions and policy changes and correlations with
# other series.

## OUTLIERS
#  - Fit the model with and without outliers and analyse how predictive
#    performance is affected.
#    * If dropping the outliers has not much effect: fit the model with the 
#      outliers.
#    * Else:
#           + Remove these periods and fit the model without them
#             Regressions models can do it directly.
#             Smoothing models: replace the outliers with forecasts or
#             imputed values (e.g., using a centered moving average)
#           + OR: label the outliers usisng a dummy variable

## SPECIAL EVENTS

## INTERVENTIONS

## CORRELATED EXTERNAL SERIES
#
#  Two step procedure:
#
# 1 - Remove trend and seasonality from both series - the series of interest
#     y_t and the external series x_t. Let y*_t and x*_t the resulting series.
# 2 - Fit a regression model of y*_t with predictors that are lags of y*_t 
#     and / or x*_t
#
# Note #1: Lagged or forecasted predictors
#          The model should include EITHER lagged versions of the external
#          series x_t-1, x_t-2,... OR their forecasts when they are not
#          available at the time of prediction.
#
# Note #2: Data availability at time of prediction
#          It is adventageous to update the model as new information arrives.
#          This assumes that the model is trained only on data available at
#          the time of prediction. The training period must therefore take
#          into account data availability and the forecast horizon.


## Example 3: Forecasting BIKE RENTALS
library(lubridate)

bike.df <- read.csv("./DATA/BikeSharing_day.csv")

bike.df$Date       <- as.Date(bike.df$dteday, format = "%Y-%m-%d")
bike.df$Month      <- month(bike.df$Date, label = TRUE)
bike.df$DOW        <- wday(bike.df$Date,  label = TRUE)
bike.df$WorkingDay <- factor(bike.df$workingday, 
                             levels = c(0, 1),
                             labels = c("Not_Working", "Working"))
bike.df$Weather    <- factor(bike.df$weathersit, 
                             levels = c(1, 2, 3),
                             labels = c("Clear", "Mist", "Rain_Snow"))

Month.dummies              <- model.matrix(~ 0 + Month, data = bike.df)
DOW.dummies                <- model.matrix(~ 0 + DOW,   data = bike.df)
WorkingDay_Weather.dummies <- model.matrix(~ 0 + WorkingDay:Weather, 
                                           data = bike.df)

colnames(Month.dummies) <- gsub("\\\\.", "", gsub("Month", "", colnames(Month.dummies)))
colnames(DOW.dummies)   <- gsub("\\\\.", "", gsub("DOW",   "", colnames(DOW.dummies)))
colnames(WorkingDay_Weather.dummies) <- gsub("WorkingDay", "", 
                                             colnames(WorkingDay_Weather.dummies))
colnames(WorkingDay_Weather.dummies) <- gsub("Weather", "", 
                                             colnames(WorkingDay_Weather.dummies))
colnames(WorkingDay_Weather.dummies) <- gsub(":", "_", 
                                             colnames(WorkingDay_Weather.dummies))

x <- as.data.frame(cbind(Month.dummies[, -12],
                         DOW.dummies[, -7],
                         WorkingDay_Weather.dummies[, -6]))
y <- bike.df$cnt

nTotal <- length(y)
nValid <- 90
nTrain <- nTotal - nValid

xTrain <- x[1:nTrain, ]
xValid <- x[(nTrain+1):nTotal, ]
yTrain <- y[1:nTrain]
yValid <- y[(nTrain+1):nTotal]

yTrain.ts <- ts(yTrain)

f <- paste("yTrain.ts", paste(c("trend", colnames(xTrain)),  collapse = "+"), 
           sep = "~")
f
(formula <- as.formula(f))

bike.tslm <- tslm(formula, data = xTrain, lambda = 1)

bike.tslm.pred <- forecast(bike.tslm, newdata = xValid)

plot(bike.tslm.pred, ylim = c(0, 9000),
     xlab = "Days", ylab ="Daily Bike Rentals")

summary(bike.tslm)
mCoef <- coef(bike.tslm)

# The number of daily bike rentals on a clear non-working day on average will
# be 3220.48 higher than on a rainy/snowy non-working day
as.numeric(mCoef["Not_Working_Clear"] - mCoef["Not_Working_Rain_Snow"])


## Example 4: Forecasting Walmart Sales by Store and by Department
#
# We want to quantify the effect of holidays on the average weekly sales from
# one store-department pais (Store 1 and Dpt 6). We'll use the complete
# 143 weeks.
walmart_data <- read.csv("./DATA/Walmart_train.csv")
str(walmart_data)
one_pair <- subset(walmart_data, (Store == 1 & Dept == 6),
                   select = c(Date, Weekly_Sales, IsHoliday))

nTrain <- nrow(one_pair)

walmart_data <- read.csv("./DATA/Walmart_test.csv")
one_pair_tst <- subset(walmart_data, (Store == 1 & Dept == 6),
                       select = c(Date, IsHoliday))
nTest <- nrow(one_pair_tst) 

rm(walmart_data)

yTrain.ts <- ts(one_pair$Weekly_Sales[1:nTrain], start = c(2010, 5), 
                frequency = 52)
plot(yTrain.ts)

stl.run <- stl(yTrain.ts, s.window = "periodic")
plot(stl.run) # Watch week of year seasonality

Acf(one_pair$Weekly_Sales) # Watch relevant lag-1 and lag-4 acs

# To fit an ARIMA model (to capture autocorrelation) we must first 
# deseasonalize the weekly sales before including external info.
#
# After deseasonalize, we fit an ARIMA model to the deseasonalized series 
# that includes IsHoliday as an aditional predictor.
#
# We finally examine the coeficient of Is Holiday to quantify its effect


# 1 - USING STLM
#
xTrain <- data.frame(IsHoliday = one_pair$IsHoliday)
stlm.reg.fit <- stlm(yTrain.ts, 
                     s.window = "periodic", 
                     xreg = xTrain, 
                     method = "arima")
stlm.reg.fit$model

## 2 - EQUIVALENT ALTERNATIVE APPROACH TO stlm()
#
# Ia. Deseasonalize, approach 1
seasonal.comp <- stl.run$time.series[, "seasonal"]
deseasonalized.ts <- yTrain.ts - seasonal.comp

# Ib Deseasonalize, approach 2
deseasonalized.ts <- seasadj(stl.run)

# II - Fit an ARIMA model with external predictors to the deseasonalized
# series
arima.fit.deas <- auto.arima(deseasonalized.ts, xreg = xTrain)

# III - Make a forecast using the ARIMA model
xTest <- data.frame(IsHoliday = one_pair_tst$IsHoliday)
arima.fit.deas.pred <- forecast(arima.fit.deas, xreg = xTest, h = nTest)

# IV Forecast the seasonal component from the stl decomposition using a
# seasonal naive forecast
seasonal.comp.pred <- snaive(seasonal.comp, h = nTest)

# V - Add the ARIMA model's forecast of the deseasonalized series to the
# sesasonal naive forecast of the seasonal component
alt.forecast <- arima.fit.deas.pred$mean + seasonal.comp.pred$mean


# Plot forecast
stlm.reg.pred <- forecast(stlm.reg.fit, xreg = xTest, h = nTest)
plot(stlm.reg.pred, xlab = "Year", ylab = "Weekly Sales")

## Check results are equal
stlm.reg.pred$mean - alt.forecast

## Analysis
# The ARIMA models are 
stlm.reg.fit$model
arima.fit.deas

# The external predictor, IsHoliday, turns out to be statistically 
# insignificant: its coefficient
coef(arima.fit.deas)["IsHoliday"]

# is only 
coef(arima.fit.deas)["IsHoliday"]/arima.fit.deas$var.coef[3,3]
# standard errors (s.e.) away from zero. Hence, we conclude that once
# weekly seasonality is accounted for, the holiday information does not
# contribute further information about weekly sales in this store-department 
# pair



# 7.5 - PROBLEMS ----------------------------------------------------------

## 1 - Analysis of Canadian Manufacturing Workers Work-Hours.

CanadianWorkHours <- read_excel("D:/PROYECTOS DATA SCIENCE/TSA_book/DATA/CanadianWorkHours.xls")

CWH_ts <- ts(CanadianWorkHours$`Hours per week`, 
             start = c(1966), end = c(2000),
             frequency = 1)
plot(CWH_ts,
     xlab = "Year", ylab = "Hours per Week", 
     main = "Canadian Hours per Week",
     bty = "l")

# a) If we computed the autocorrelation of this series, would the lag-1
#    autocorrelation exhibit negative, positive or no autocorrelation? How can 
#    you see this from the plot?

# There are two clear trends (upto and from 1989, more or less). Usually a trend 
# in the data will show in the correlogram as a slow decay in the 
# autocorrelations, due to similar values in the series occurring close together 
# in time. So, lag-1 should be significative and positive

# b) Compute de autocorrelation and produce an ACF plot. Verify your answer to 
#    the previous question.
acf(CWH_ts)
Acf(CWH_ts)

## 2 - Forecasting Wal-Mart Stock
library(readxl)
WalMartStock <- read_excel("D:/PROYECTOS DATA SCIENCE/TSA_book/DATA/WalMartStock.xls")

WMS_ts <- ts(WalMartStock$Close, 
             start = c(2001, 2),
             frequency = 365)
plot(WMS_ts,
     xlab = "Time", ylab = "Closing Price", 
     main = "Wal-Mart Daily Closing Prices",
     bty = "n")

library(forecast)
Acf(WMS_ts, lag.max = 10)
Acf(diff(WMS_ts), lag.max = 10)

WMS_AR1   <-  Arima(WMS_ts,       order = c(1, 0, 0))
WMS_AR1

WMS_AR1_D <-  Arima(diff(WMS_ts), order = c(1, 0, 0))
WMS_AR1_D

# a) Create a time plot of the differenced series
plot(diff(WMS_ts),
     xlab = "Time", ylab = "Diff Closing Price", 
     main = "Wal-Mart Diff Daily Closing Prices",
     bty = "n")

# b) Which of the following is/are relevant for testing wether this stock is a
#    random walk?

# - The autocorrelation of the closing price series

# - The AR(1) slope coefficient for the closing price series
# YES
summary(WMS_AR1)
(1 - 0.9558) / 0.0187 > qnorm(0.975) # ar1 more than 1.96 ses away from 1, it's not a rw
(1 - 0.9558) / 0.0187 > qnorm(0.995) # ar1 less than 2.58 ses away from 1, it's a rw
# ar1 less than 5 standard errors away from 1, it's a rw

# - The AR(1) constant coefficient for the closing price series

# - The autocorrelation of the differenced series
# YES
#  As a random walk is a constant plus 
#  a random term, the ACF of a lag-1 differenced series should indicate that 
#  autocorrelations at all lags should be aprox. zero (all the bars within the 
#  thresholds)

# - The AR(1) slope coefficient for the differenced series

# - The AR(1) constant coefficient for the differenced series

# As a plus:
adf.test(WMS_ts)       # Non stationary => could be a rw
adf.test(diff(WMS_ts)) # Stationary => can't be a rw

# c) Does the AR(1) model for the Close price series indicate that this is a 
# random walk?
# With a confidence level of 95% (~2   s.e.'s): it's not a rw
# With a confidence level of 99% (~2.5 s.e.'s): it's a rw
# With a confidence level of 5 s.e.'s: it's a rw

# d) What are the implications of finding that a series is a random walk?

# If the series is not a random walk then we can attempt to predict
# it (beyond a naive forecast)

# If it's a rw:

# - It is impossible to obtain useful forecasts of the series
# OK
# - The series is random
# The changes in the series from one period to the other are ranodm
# OK

# 3 - Souvenir Sales
SouvenirSales <- read_excel("D:/PROYECTOS DATA SCIENCE/TSA_book/DATA/SouvenirSales.xls")

SS_ts <- ts(SouvenirSales$Sales, 
             start = c(1995, 1),
             frequency = 12)
plot(SS_ts,
     xlab = "Time", ylab = "Sales", 
     main = "Souvenir Sales",
     bty = "n", type = "o")

##
library(lubridate)
SS_df <- SouvenirSales[, c("Date", "Sales")]
SS_df$Month <- month(as.Date(SS_df$Date, format = "%Y-%m-%d"), label = TRUE)

Month.dummies <- model.matrix(~ 0 + Month, data = SS_df)
colnames(Month.dummies) <- gsub("Month", "", colnames(Month.dummies))

nTotal <- length(SS_ts)
nValid <- 12
nTrain <- nTotal - nValid

x <- as.data.frame(Month.dummies[, -12])
y <- log(SS_df$Sales)

SS_train_y <- y[1:nTrain]
SS_valid_y <- y[(nTrain+1):nTotal]

SS_train_x <- x[1:nTrain, ]
SS_valid_x <- x[(nTrain+1):(nTrain+nValid), ]


# a) Run a regression model with log(Sales) as the output variable and with a 
#    linear trend and monthly predictors. Use this model to forecast the sales
#    in February 2002
yTrain.ts <- ts((SS_train_y), start = start(SS_ts), freq = 12)

f <- paste("yTrain.ts", paste(c("trend", colnames(SS_train_x)),  collapse = "+"), 
           sep = "~")
f
(formula <- as.formula(f))

SS.tslm <- tslm(formula, data = SS_train_x, lambda = 1)
summary(SS.tslm)

SS.tslm.pred <- forecast(SS.tslm, newdata = SS_valid_x)

# b) Create en ACF plot until lag-15 for the forecast errors.
Acf(residuals(SS.tslm), lag.max = 15)

# Now fit an AR model with lag-2 ARIMA(2,0,0) to the forecasts errors
SS.AR2.FE <- Arima(residuals(SS.tslm), c(2,0,0))
summary(SS.AR2.FE)

# b.i) Examining the ACF plot and the estimated coefficients of the AR(2)
# model (and their statistical significance), what can we learn about the 
# regression forecasts?

# if we had right modeled the seasonal pattern, the residual series should 
# show no ac at the season lag: and that's the case.
# ar1 and ar2 are aprox. 6-7 s.e.'s from 1 => the residuals series is not a random walk

# b.ii)
SS.AR2.FE.pred <- forecast(SS.AR2.FE, h = nValid)

fitted2 <- exp(fitted(SS.tslm) + fitted(SS.AR2.FE))
pred2   <- exp(SS.tslm.pred$mean + SS.AR2.FE.pred$mean)

plot(SS_ts,
     xlab = "Time", ylab = "Sales", 
     main = "Souvenir Sales",
     bty = "n", type = "o")
abline(v = 2001, col = "red", lty = 3)
abline(v = 2002, col = "red", lty = 3)

lines(fitted2, lwd = 2, col = "blue")
lines(pred2, lwd = 2, col = "blue", lty = 2)

accuracy(fitted2, SS_train_y)
accuracy(pred2, SS_valid_y)

## 5 - Shipments of Household Appliances.
library(readxl)
SHA <- read_excel("D:/PROYECTOS DATA SCIENCE/TSA_book/DATA/ApplianceShipments.xls")

SHA_ts <- ts(SHA$Shipments, 
             start = c(1985, 1),
             frequency = 4)
plot(SHA_ts,
     xlab = "Time", ylab = "Quarterly Shipments (Millions US$)", 
     main = "Quarterly Shipments \nof U.S. household appliances",
     xaxt = "n", bty = "n", type = "o")

labs <- unlist(lapply(1985:1989, paste, c("Q1", "Q2", "Q3", "Q4")))
at_seq <- seq(1985, 1989.75, 0.25)

axis(1, at = at_seq, labels = FALSE)
text(at_seq, par("usr")[3] - 50, srt = 45, adj = 1,
     labels = labs, xpd = TRUE, cex = 0.8)

# If we compute the autocorrelation of the series, which lag (>0) is most
# likely to have the largest value (in absolute value)? Create an ACF plot
# and compare it with your answer
library(forecast)
Acf(SHA_ts)
Acf(diff(SHA_ts))

## 5 - Forecasting Australian Wines Sales
library(readxl)
AW <- read_excel("D:/PROYECTOS DATA SCIENCE/TSA_book/DATA/AustralianWines.xls")
AW$`Source: Website` <- NULL

AW_ts <- ts(AW$Fortified, 
             start = c(1980, 1), end = c(1994, 12),
             frequency = 12)
plot(AW_ts,
     xlab = "", ylab = "Sales", 
     main = "Fortified monthly sales", bty = "n", type = "l", xaxt = "n")

labs <- c(unlist(lapply(1980:1994, paste, "Jan")), "")
at_seq <- seq(1980, 1995, 1)
axis(side = 1, at = at_seq, labels = labs, las = 2)

# a) Focus on fortified wine alone and produce as accurate as possible
#    forecasts for the next 2 months.

# Start by partitioning the data using the period until dec-1993 as the 
# training period

nValid <- 12
nTotal <- length(AW_ts)
nTrain <- nTotal - nValid

AW_train_ts <- window(AW_ts, start = start(AW_ts), 
                      end = c(start(AW_ts)[1], nTrain))
AW_valid_ts <- window(AW_ts, start = c(end(AW_train_ts)[1], 
                                       end(AW_train_ts)[2]+1),
                      end = end(AW_ts))

# Fit a regression model to sales with a linear trend and seasonality
library(forecast)
AW_lm_trend_season <- tslm(AW_train_ts ~ trend + season)
summary(AW_lm_trend_season)

# i. Create the "actual vs. forecast" plot. What can you say about model fit?
AW_lm_trend_season_pred <- forecast(AW_lm_trend_season, h = nValid)
AW_lm_trend_season_pred

abline(v = 1994, col = "red", lty = 3)
abline(v = 1995, col = "red", lty = 3)

lines(fitted(AW_lm_trend_season), lwd = 2, col = "blue")
lines(AW_lm_trend_season_pred$mean, lwd = 2, col = "blue", lty = 2)

# ii. Use the regression model to forecast sales in january and february 94
window(AW_lm_trend_season_pred$mean, start = c(1994,1), end = c(1994,2))

# b) Create anACF plot for the residuals from the above model until lag-12
Acf(residuals(AW_lm_trend_season), lag.max = 12)

# i. Examining this plot, which of the following statements are reasonable?

# There is a strong correlation between sales on the same calendar month
# The model does not capture the seasonality well (if we had right modeled the 
# seasonal pattern, the residual series should show no ac at the season lag)

# ii. How can you handle the above effect without adding another layer to your
# model?

# The other approach is directly building the ac into the regression model 
# using ARIMA models

#### 

names(AW) <- c("Month", "Fortified", "Red", "Rose", "Sparkling", "Sweet_White", "Dry_White")

AW_train_ts2 <- window(AW_train_ts, 
                       start = c(start(AW_ts)[1],start(AW_ts)[2]+1), 
                      end = end(AW_train_ts))

m <- tslm(AW_train_ts2 ~ trend + season + Red + Rose + Sparkling + Sweet_White + Dry_White, 
          data = AW[1:(nTrain-1),3:7])
summary(m)

## 6 - Forecasting Weekly Sales at Walmart

WTrain <- read.csv("D:/PROYECTOS DATA SCIENCE/TSA_book/DATA/Walmart_train.csv")
WTest  <- read.csv("D:/PROYECTOS DATA SCIENCE/TSA_book/DATA/Walmart_test.csv")
WFeat  <- read.csv("D:/PROYECTOS DATA SCIENCE/TSA_book/DATA/Walmart_features.csv")

# Subset Department #27 of Walmart Store 1
WTrain <- subset(WTrain, subset = (Store == 1 & Dept == 27))
WTest  <- subset(WTest,  subset = (Store == 1 & Dept == 27))
WFeat  <- subset(WFeat,  subset = (Store == 1))

dim(WTrain) ; dim(WTest); dim(WFeat)

# Merge train and test sets
WTrain <- merge(WTrain, WFeat[1:143, ], by = c("Store", "Date", "IsHoliday"))
WTest  <- merge(WTest,  WFeat[144:182, ], by = c("Store", "Date", "IsHoliday"))

# We are interested in creating a forecasting model for weekly sales for the
# next 52 weeks

# (a) - Recreate the time plot of the weekly sales data. Which systematic
#       patterns appear in this series?

WS_ts <- ts(WTrain$Weekly_Sales, 
            start = c(2010, 2), frequency = 52)
plot(WS_ts,
     xlab = "", ylab = "Sales", 
     main = "Weekly Sales in Department #27 of Walmart Store 1", 
     bty = "n", type = "l", xaxt = "n")

labs <- format(as.Date(WTrain$Date[1]) %m+% months(0:32))
at_seq = seq((2010+2/52), (2010+nrow(WTrain)/52), length.out = 33)

axis(1, at = at_seq, labels = FALSE)
text(at_seq, par("usr")[3] - 150, srt = 45, adj = 1,
     labels = labs, xpd = TRUE, cex = 0.8)

# (b) Create time plots of the other numerical series (Temperature, Fuel_Price,
#     CPI and Unemployement)

par(mfrow = c(2, 2))

# TEMPERATURE
WS_Temp_ts <- ts(WTrain$Temperature, 
                 start = c(2010, 2), frequency = 52)
plot(WS_Temp_ts,
     xlab = "", ylab = "Temperature (K)", 
     main = "Weekly Temperature in Department #27 of Walmart Store 1", 
     bty = "n", type = "l", xaxt = "n")

axis(1, at = at_seq, labels = FALSE)
text(at_seq, par("usr")[3] - 1.5, srt = 45, adj = 1,
     labels = labs, xpd = TRUE, cex = 0.8)

# FUEL PRICE
WS_Fuel_ts <- ts(WTrain$Fuel_Price, 
                 start = c(2010, 2), frequency = 52)
plot(WS_Fuel_ts,
     xlab = "", ylab = "Fuel Price ($)", 
     main = "Weekly Fuel Price in Department #27 of Walmart Store 1", 
     bty = "n", type = "l", xaxt = "n")

axis(1, at = at_seq, labels = FALSE)
text(at_seq, par("usr")[3] - 0.05, srt = 45, adj = 1,
     labels = labs, xpd = TRUE, cex = 0.8)

# CPI
WS_CPI_ts <- ts(WTrain$CPI, 
                 start = c(2010, 2), frequency = 52)
plot(WS_CPI_ts,
     xlab = "", ylab = "CPI", 
     main = "Weekly CPI in Department #27 of Walmart Store 1", 
     bty = "n", type = "l", xaxt = "n")

axis(1, at = at_seq, labels = FALSE)
text(at_seq, par("usr")[3] - 0.5, srt = 45, adj = 1,
     labels = labs, xpd = TRUE, cex = 0.8)

# UNEMPLOYEMENT
WS_Une_ts <- ts(WTrain$Unemployment, 
                start = c(2010, 2), frequency = 52)
plot(WS_Une_ts,
     xlab = "", ylab = "Unemployment", 
     main = "Weekly Unemployment in Department #27 of Walmart Store 1", 
     bty = "n", type = "l", xaxt = "n")

axis(1, at = at_seq, labels = FALSE)
text(at_seq, par("usr")[3] - 0.05, srt = 45, adj = 1,
     labels = labs, xpd = TRUE, cex = 0.8)

par(mfrow = c(1, 1))

# Also create scatter plots of the sales series against each of these four
# series (each point in the scatter plot will be a week). From the charts,
# which of the four series would potentially be useful as external predictors
# in a regression model for forecasting sales?
par(mfrow = c(2, 2))
plot(WTrain$Weekly_Sales, WTrain$Temperature,
     xlab = "Weekly Sales", ylab = "Temperature(K)")
plot(WTrain$Weekly_Sales, WTrain$Fuel_Price,
     xlab = "Weekly Sales", ylab = "Fuel Price")
plot(WTrain$Weekly_Sales, WTrain$CPI,
     xlab = "Weekly Sales", ylab = "CPI")
plot(WTrain$Weekly_Sales, WTrain$Unemployment,
     xlab = "Weekly Sales", ylab = "Unemployment")
par(mfrow = c(1, 1))

cor(WTrain$Weekly_Sales, WTrain$Temperature)
cor(WTrain$Weekly_Sales, WTrain$Fuel_Price)
cor(WTrain$Weekly_Sales, WTrain$CPI)
cor(WTrain$Weekly_Sales, WTrain$Unemployment)

intVars <- c("Weekly_Sales", "Temperature", "Fuel_Price", "CPI", "Unemployment")
cor(WTrain[, intVars])

# Fuel_Price, CPI and Unemployement are strongly corralated. Only one of them
# should be used as predictor.
# Weekly_Sales is mostly correlated to Temperature, which, in turn, has not
# much to do with Fuel_Price, CPI or Unemployement.
#
# I should use Temperature or Temperature and Fuel_price

library(psych)
pairs.panels(WTrain[, intVars])

## (c) The period to forecast is November 2, 2012 to July 26, 2013. For which
#      of the series does the file include data for the forecasting period. For
#      which of the series is there no data for this period? Explain why.
sapply(WTest[, intVars[intVars != "Weekly_Sales"]], function(x) {
  sum(is.na(x))
})

# (d) How is it possible that we have Temperature for the prediction period?

# (e) Treat the first 91 weeks (until Oct 28, 2011) as the training period, 
#     and the next 52 weeks as the validation period.
#     Create naive forecasts for the validation period. 
nTotal <- nrow(WTrain)
nTrain <- 91
nValid <- nTotal - nTrain

train_start <- start(WS_ts)
train_end   <- start(WS_ts) + c(0, nTrain-1)
valid_start <- end(train_ts) + c(0, 1)
valid_end   <- end(WS_ts)

train_ts <- window(WS_ts, start = train_start, 
                   end = train_end)
valid_ts <- window(WS_ts, start = valid_start,
                   end = valid_end)
library(forecast)
naive_pred <- naive(train_ts, h = nValid)
snaive_pred <- snaive(train_ts, h = nValid)

# Create a time plot of these forecasts and a plot of the forecast error series.
plot(naive_pred, bty = "n")
lines(fitted(naive_pred), lty = 1, col = "blue")
lines(valid_ts, lty = 2, col = "red")

resid <- ts(c(residuals(naive_pred), valid_ts - naive_pred$mean),
            start = start(train_ts), end = end(valid_ts), freq = 52)
plot(resid, bty = "n")
abline(v = 2010 + nTrain/52, lty = 2, col = "red")
abline(v = 2010 + nTotal/52, lty = 2, col = "red")

plot(snaive_pred, bty = "n")
lines(fitted(naive_pred), lty = 1, col = "blue")
lines(valid_ts, lty = 2, col = "red")

resid <- ts(c(residuals(snaive_pred), valid_ts - snaive_pred$mean),
            start = start(train_ts), end = end(valid_ts), freq = 52)
plot(resid, bty = "n")
abline(v = 2010 + nTrain/52, lty = 2, col = "red")
abline(v = 2010 + nTotal/52, lty = 2, col = "red")


# Compute the average error and RMSE
accuracy(naive_pred,  valid_ts)
accuracy(snaive_pred, valid_ts)


error <- valid_ts - naive_pred$mean
# ME
mean(error)
# RMSE
sqrt(mean(error^2))

error <- valid_ts - snaive_pred$mean
# ME
mean(error)
# RMSE
sqrt(mean(error^2))

## (g) If we wanted to include Temperature and Fuel_Price as perdictors in the 
#      regression model, in what form can they be included?

# 1 - Remove trend and seasonality from both series - the series of interest
#     y_t and the external series x_t. Let y*_t and x*_t the resulting series.

# Remove trend and seasonality from objective and external series
# I. Deseasonalize
stl.sales <- stl(WS_ts, s.window = "periodic")
sales.seasonal.comp <- stl.sales$time.series[, "seasonal"]
sales.trend.comp    <- stl.sales$time.series[, "trend"]
deseasonalized.sales.ts <- WS_ts - sales.seasonal.comp - sales.trend.comp
deseasonalized.sales.train.ts <- window(deseasonalized.sales.ts,
                                        start = train_start,
                                        end   = train_end)
deseasonalized.sales.valid.ts <- window(deseasonalized.sales.ts,
                                        start = valid_start,
                                        end   = valid_end)

stl.temp <- stl(WS_Temp_ts, s.window = "periodic")
temp.seasonal.comp <- stl.temp$time.series[, "seasonal"]
temp.trend.comp    <- stl.temp$time.series[, "trend"]
deseasonalized.temp.ts <- WS_Temp_ts - temp.seasonal.comp - temp.trend.comp
deseasonalized.temp.train.ts <- window(deseasonalized.temp.ts,
                                        start = train_start,
                                        end   = train_end)
deseasonalized.temp.valid.ts <- window(deseasonalized.temp.ts,
                                        start = valid_start,
                                        end   = valid_end)

stl.fuel <- stl(WS_Fuel_ts, s.window = "periodic")
fuel.seasonal.comp <- stl.fuel$time.series[, "seasonal"]
fuel.trend.comp    <- stl.fuel$time.series[, "trend"]
deseasonalized.fuel.ts <- WS_Fuel_ts - fuel.seasonal.comp - temp.trend.fuel
deseasonalized.fuel.train.ts <- window(deseasonalized.fuel.ts,
                                       start = train_start,
                                       end   = train_end)
deseasonalized.fuel.valid.ts <- window(deseasonalized.fuel.ts,
                                        start = valid_start,
                                        end   = valid_end)

# 2 - Fit a regression model of y*_t with predictors that are lags of y*_t 
#     and / or x*_t

# Decide which lags to use
Ccf(deseasonalized.sales.train.ts, 
    deseasonalized.temp.train.ts, lag.max = 60) # -4, -9, -24
Ccf(deseasonalized.sales.train.ts, 
    deseasonalized.fuel.train.ts, lag.max = 60) # -4, -11  

# Build train and valid external regressors data frames

# train regressors
des.temp_A_ts <- lag(deseasonalized.temp.train.ts, -4)
des.temp_B_ts <- lag(deseasonalized.temp.train.ts, -9)
des.fuel_A_ts <- lag(deseasonalized.fuel.train.ts, -4)
des.fuel_B_ts <- lag(deseasonalized.fuel.train.ts, -11)

x_train_df <- as.data.frame(cbind(deseasonalized.sales.train.ts, 
                                  des.temp_A_ts, 
                                  des.temp_B_ts, 
                                  des.fuel_A_ts,
                                  des.fuel_B_ts))[1:nTrain, -1]

# valid regressors
tmp_ts <- ts(c(deseasonalized.temp.train.ts, deseasonalized.temp.valid.ts),
             start = train_start, end = valid_end, freq = 52)
des.temp_A_ts <- window(lag(tmp_ts, -4), 
                     start = valid_start, end = valid_end)
des.temp_B_ts <- window(lag(tmp_ts, -9), 
                     start = valid_start, end = valid_end)


tmp_ts <- ts(c(deseasonalized.fuel.train.ts, deseasonalized.fuel.valid.ts),
             start = train_start, end = valid_end, freq = 52)
des.fuel_A_ts <- window(lag(tmp_ts, -4), 
                     start = valid_start, end = valid_end)
des.fuel_B_ts <- window(lag(tmp_ts, -11), 
                        start = valid_start, end = valid_end)

x_valid_df <- as.data.frame(cbind(deseasonalized.sales.valid.ts, 
                                  des.temp_A_ts, 
                                  des.temp_B_ts, 
                                  des.fuel_A_ts,
                                  des.fuel_B_ts))[1:nValid, -1]

# Fit the model
# II - Fit an ARIMA model with external predictors to the deseasonalized
# series

arima.fit.deas <- auto.arima(deseasonalized.sales.train.ts, xreg = x_train_df)

# III - Make a forecast using the ARIMA model
arima.fit.deas.pred <- forecast(arima.fit.deas, xreg = x_valid_df, h = nValid)

# IV Forecast the seasonal component from the stl decomposition using a
# seasonal naive forecast
snaive_pred

# V - Add the ARIMA model's forecast of the deseasonalized series to the
# sesasonal naive forecast of the seasonal component
alt.forecast <- arima.fit.deas.pred$mean + snaive_pred$mean


# Plot forecast
plot(WS_ts, xlab = "Year", ylab = "Weekly Sales", bty = "n")
lines(alt.forecast, lty = 2, col = "red")
abline(v = 2010 + nTrain/52, lty = 2, col = "red")
abline(v = 2010 + nTotal/52, lty = 2, col = "red")

accuracy(alt.forecast, valid_ts)

## OPTION 2
# Must remove NAs...
train_ts2 <- window(train_ts, 
                    start = train_start + c(0, 11))

f <- paste("train_ts2", 
           paste(c("trend", "season", colnames(x_train_df)),  collapse = "+"), 
           sep = "~")
f
(formula <- as.formula(f))

Wsales.tslm <- tslm(formula, data = na.exclude(x_train_df))

Wsales.tslm.pred <- forecast(Wsales.tslm, newdata = x_valid_df)

plot(Wsales.tslm.pred, bty = "n")
lines(valid_ts)
abline(v = 2010 + nTrain/52, lty = 2, col = "red")
abline(v = 2010 + nTotal/52, lty = 2, col = "red")

accuracy(Wsales.tslm.pred, valid_ts)
