### 6. REGRESSION-BASED MODELS: CAPTURING TREND AND SEASONALITY

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

## 6.1 MODEL WITH TREND

# 6.1.1 LINEAR TREND
train_lm <- tslm(train_ts ~ trend)

plot(train_ts, xlab = "Time", ylab = "Ridership", bty = "l")
lines(train_lm$fitted.values, lwd = 2)

train_lm_pred <- forecast(train_lm, h = nValid, level = 0)

plot(train_lm_pred, xlab = "Time", ylab = "Ridership", bty = "l",
     xaxt = "n", main = "", flty = 2,
     xlim = c(1991, 2006.25), ylim = c(1300, 2200))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train_lm_pred$fitted, lwd = 2, col = "blue")
lines(valid_ts)
abline(v = 2001.25, col = "red", lty = 3)
abline(v = 2004.25, col = "red", lty = 3)

summary(train_lm)

# 6.1.2 EXPONENTIAL TREND
train_lm_expo <- tslm(train_ts ~ trend, lambda = 0) # Multiplicative seasonality
train_lm_expo_pred <- forecast(train_lm_expo, h = nValid, level = 0)

train_lm_linear <- tslm(train_ts ~ trend, lambda = 1) # Additive seasonality
train_lm_linear_pred <- forecast(train_lm_linear, h = nValid, level = 0)

plot(train_lm_expo_pred, xlab = "Time", ylab = "Ridership", bty = "l",
     xaxt = "n", main = "", flty = 2,
     xlim = c(1991, 2006.25), ylim = c(1300, 2200))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train_lm_expo_pred$fitted, lwd = 2, col = "blue")
lines(train_lm_linear_pred$fitted, lwd = 2, col = "black")
lines(train_lm_linear_pred$mean, lwd = 2, col = "black")
lines(valid_ts)
abline(v = 2001.25, col = "red", lty = 3)
abline(v = 2004.25, col = "red", lty = 3)

# 6.1.2 POLYNOMIAL TREND
train_lm_poly <- tslm(train_ts ~ trend + I(trend^2))
summary(train_lm_poly)

train_lm_poly_pred <- forecast(train_lm_poly, h = nValid, level = 0)

plot(train_lm_poly_pred, xlab = "Time", ylab = "Ridership", bty = "l",
     xaxt = "n", main = "", flty = 2,
     xlim = c(1991, 2006.25), ylim = c(1300, 2200))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train_lm_poly_pred$fitted, lwd = 2, col = "blue")
lines(valid_ts)
abline(v = 2001.25, col = "red", lty = 3)
abline(v = 2004.25, col = "red", lty = 3)

## 6.2 MODEL WITH SEASONALITY
train_lm_season <- tslm(train_ts ~ season)
summary(train_lm_season)

train_lm_season$xlevels

train_lm_season_pred <- forecast(train_lm_season, h = nValid, level = 0)

plot(train_lm_season_pred, xlab = "Time", ylab = "Ridership", bty = "l",
     xaxt = "n", main = "", flty = 2,
     xlim = c(1991, 2006.25), ylim = c(1300, 2200))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train_lm_season_pred$fitted, lwd = 2, col = "blue")
lines(valid_ts)
abline(v = 2001.25, col = "red", lty = 3)
abline(v = 2004.25, col = "red", lty = 3)

plot(train_lm_season_pred$residuals, xlab = "Time", ylab = "Residuals", bty = "l",
     xaxt = "n", main = "", flty = 2,
     xlim = c(1991, 2006.25), ylim = c(-400, 400))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid_ts - train_lm_season_pred$mean, lwd = 2, col = "blue")
abline(v = 2001.25, col = "red", lty = 3)
abline(v = 2004.25, col = "red", lty = 3)


## 6.2 MODEL WITH TREND AND SEASONALITY
train_lm_trend_season <- tslm(train_ts ~ trend + I(trend^2) + season)
summary(train_lm_trend_season)

train_lm_trend_season_pred <- forecast(train_lm_trend_season, h = nValid, level = 0)

plot(train_lm_trend_season_pred, xlab = "Time", ylab = "Ridership", bty = "l",
     xaxt = "n", main = "", flty = 2,
     xlim = c(1991, 2006.25), ylim = c(1300, 2600))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train_lm_trend_season_pred$fitted, lwd = 2, col = "blue")
lines(valid_ts)
abline(v = 2001.25, col = "red", lty = 3)
abline(v = 2004.25, col = "red", lty = 3)

plot(train_lm_trend_season_pred$residuals, xlab = "Time", ylab = "Residuals", bty = "l",
     xaxt = "n", main = "", flty = 2,
     xlim = c(1991, 2006.25), ylim = c(-400, 400))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid_ts - train_lm_trend_season_pred$mean, lwd = 2, col = "blue")
abline(v = 2001.25, col = "red", lty = 3)
abline(v = 2004.25, col = "red", lty = 3)

## PROBLEMS
## Problem 1 - Impact of September 11 on Air Travel in the USA
# https://www.rita.dot.gov/bts/sites/rita.dot.gov.bts/files/publications/estimated_impacts_of_9_11_on_us_travel/index.html

# The purpose of this study is to provide a greater understanding
# of the passenger travel behavior patterns of persons making longdistance
# trips before and after 9/11. 

# (a)
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

plot(preSep11_ts, ylab = "Air RPM (000s)", 
     main = "Pre-Sep11 Air Travel", xaxt = "n", bty = "l")
lPreSep11_ts <- length(time(preSep11_ts))
selLabs <- seq(1, lPreSep11_ts, 12)
whereLabs <- time(preSep11_ts)[selLabs]
preSepLabs <- as.Date(time(preSep11_ts))[selLabs]
axis(1, at = whereLabs, 
    labels = preSepLabs, 
    las = 1, cex.axis = .75)
# There are level, linear trend, seasonality and noise

# (b)
hwin_preSep11 <- ets(preSep11_ts/1000000, model = "MAM")
plot(hwin_preSep11)

plot(hwin_preSep11$states[,"l"]) # level
plot(hwin_preSep11$states[,"b"]) # trend
plot(hwin_preSep11$states[,"s1"]) # seasonal ???

head(hwin_preSep11$states, 1)
tail(hwin_preSep11$states, 1)

seasonalPartPreSep11_ts <- hwin_preSep11$states[,"s1"]
plot(seasonalPartPreSep11_ts)

plot(preSep11_ts/1000000 - seasonalPartPreSep11_ts, ylim = c(0, 70))
plot(hwin_preSep11$states[,"l"], ylim = c(0, 70)) # level

# MUCH BETTER:
hwin_preSep11_stl <- stl(preSep11_ts / 1000000, s.window = "periodic")
hwin_preSep11_noseason <- preSep11_ts/1000000 - hwin_preSep11_stl$time.series[, "seasonal"]
plot(hwin_preSep11_noseason, ylim = c(0, 70))


# (c) lambda = 1 means MULTIPLICATIVE seasonality
preSep11_lm_trend_season <- tslm(preSep11_ts ~ trend + season, lambda = 0)

# (d)
# i
summary(preSep11_lm_trend_season)
# ii
forecast(preSep11_lm_trend_season)$fitted[1]

# (e)
sep11_air_ts <- ts(sep11_data$`Air RPM (000s)`, 
                   start = c(1990,1), end = c(2004, 4),
                   frequency = 12)
preSep11_air_ts <- window(sep11_air_ts, start = start(sep11_air_ts),
                          end = c(2001, 8))
postSep11_air_ts <- window(sep11_air_ts, start = c(2001, 9),
                           end = end(sep11_air_ts))

sep11_rail_ts <- ts(sep11_data$`Rail PM`, 
                   start = c(1990,1), end = c(2004, 4),
                   frequency = 12)
preSep11_rail_ts <- window(sep11_rail_ts, start = start(sep11_rail_ts),
                          end = c(2001, 8))
postSep11_rail_ts <- window(sep11_rail_ts, start = c(2001, 9),
                           end = end(sep11_rail_ts))

sep11_auto_ts <- ts(sep11_data$`VMT (billions)`, 
                    start = c(1990,1), end = c(2004, 4),
                    frequency = 12)
preSep11_auto_ts <- window(sep11_auto_ts, start = start(sep11_auto_ts),
                           end = c(2001, 8))
postSep11_auto_ts <- window(sep11_auto_ts, start = c(2001, 9),
                            end = end(sep11_auto_ts))
plot(preSep11_air_ts)
plot(preSep11_rail_ts)
plot(preSep11_auto_ts)

preSep11_lm_air_trend_season <- tslm(preSep11_air_ts ~ trend + season)
#preSep11_lm_rail_trend_season <- tslm(preSep11_rail_ts ~ trend +  I(trend^2) + season)
preSep11_lm_rail_trend_season <- tslm(preSep11_rail_ts ~ season)
preSep11_lm_auto_trend_season <- tslm(preSep11_auto_ts ~ trend + season)

h = length(postSep11_air_ts)

preSep11_lm_air_pred <- forecast(preSep11_lm_air_trend_season, h = h)
plot(preSep11_lm_air_pred, main ="Air Travel", xlim = c(2001, 2004.25))
lines(preSep11_lm_air_pred$fitted, lwd = 2, col = "blue")
lines(postSep11_air_ts, lwd = 2, col = "red")

preSep11_lm_rail_pred <- forecast(preSep11_lm_rail_trend_season, h = h)
plot(preSep11_lm_rail_pred, main = "Rail Travel", xlim = c(2001, 2004.25))
lines(preSep11_lm_rail_pred$fitted, lwd = 2, col = "blue")
lines(postSep11_rail_ts, lwd = 2, col = "red")

preSep11_lm_auto_pred <- forecast(preSep11_lm_auto_trend_season, h = h)
plot(preSep11_lm_auto_pred, main = "Auto Travel", xlim = c(2001, 2004.25))
lines(preSep11_lm_auto_pred$fitted, lwd = 2, col = "blue")
lines(postSep11_auto_ts, lwd = 2, col = "red")

## Problem 3 - Modeling Toys "R" Us Revenues

tru_data <- 
  read_excel("~/PROYECTOS DATA SCIENCE/TSA_book/DATA/ToysRUsRevenues.xls")
tru_ts <- ts(tru_data$`Revenue(in million $)`, 
             start = c(1992,1), end = c(1995, 4),
             frequency = 4)
plot(tru_ts, 
     main = "Quarterly Revenues of Toys \"R\" Us", 
     ylab = "Revenue ($Millions)", bty = "l")

# (a)
nValid <- 2
nTrain <- length(tru_ts) - nValid
train_tru_ts <- window(tru_ts, start = start(tru_ts),
                       end = c(1992, nTrain))
valid_tru_ts <- window(tru_ts, start = c(1992, nTrain + 1),
                       end = end(tru_ts))

tru_lm_trend_season <- tslm(train_tru_ts ~ trend + season)

# (b)
summary(tru_lm_trend_season)

tru_lm_pred <- forecast(tru_lm_trend_season, h = nValid)
accuracy(tru_lm_pred, valid_tru_ts)

# i
# ii
# iii - Avg Sales Q3 - Avg Sales Q1 = -15.11
# iv - Q4: +2101.73
