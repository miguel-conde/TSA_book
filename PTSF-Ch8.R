## 8. FORECASTING BINARY OUTCOMES


# 8.1. Forecasting Binary Outcomes ----------------------------------------

# - Will an event occur or not in a future time?
# - In which direction will a time series move in a future time?
# - Will a numerical time series cross or not a given threshold in a future time?

# It's likely that in all three scenarios the binary outcome is relate to
# measurements in previoues periods.

# We'll use logistic regression in this chapter and neural networks in the next 
# one. But many other classificators can be used instead,

# External information can also be used in our models.


# 8.2 Naive forecasts and performance evaluation --------------------------

# Naive benchmarks: the binary value in the previous period; or the "majority-
# -class" forecast.

# Performace; classification / confussion matrix and asociates indicators; and
# ROC curves.


# 8.3 Logistic regression -------------------------------------------------

# When used with time series, logistic regression can incorporate trend,
# seasonality, lag variables and external information.
#
# log(odds) = log(p / (1 - p)) = beta_0 + beta_1 * x_1 + ... + beta_n * x_n
#
# The funcion log(odds) = log(p / (1 - p)) is known as the logit function in p.

# All we've studied before about linear regression can be applied to logistic
# regression.

# Daily rainfall amounts in Melbourne from Jan 1, 2000 and Oct 31, 2011
rainy_data <- read.csv("./Data/IDCJAC0009_086282_1800_Data.csv")
names(rainy_data)[6] <- "Rainfall"
iniD <- which(rainy_data$Year == 2000 & rainy_data$Month == 1 & rainy_data$Day == 1)
endD <- which(rainy_data$Year == 2011 & rainy_data$Month == 10 & rainy_data$Day == 31) 
rainy_data <- rainy_data[iniD:endD, ]
rainy_data <- subset(rainy_data,  
                     select = c("Year", "Month", "Day", "Rainfall"))

nTotal <- nrow(rainy_data)
nTrain <- which(rainy_data$Year == 2009 & rainy_data$Month == 12 & rainy_data$Day == 31)
nValid <- nTotal - nTrain

train_ts <- ts(rainy_data[1:nTrain, "Rainfall", drop = FALSE], 
               start = c(2000, 1), freq = 365)
iniTime <- c(2000, 1)
plot(window(train_ts, start = iniTime, end = iniTime + c(0,365)))

library(data.table)
rainy_data_dt <- as.data.table(rainy_data)

month_rainfall_dt <- rainy_data_dt[,  sum(Rainfall), by = Month]
names(month_rainfall_dt) <- c("Month", "Month_Rainfall") 

plot(ts(month_rainfall_dt$Month_Rainfall), bty = "n",
     xaxt = "n")
axis(1, at = 1:12, labels = month.abb)

# Logistic regression model

## Adding predictors
rainy_data <- read.csv("./Data/IDCJAC0009_086282_1800_Data.csv")
names(rainy_data)[6] <- "Rainfall"

f <- 365
trainStart <- c(2000, 1)
trainEnd   <- trainStart + c(0, nTrain)
validStart <- trainEnd + c(0, 1)
validEnd   <- validStart + c(0, nValid)

rainy_ts <- ts(rainy_data[, "Rainfall", drop = FALSE], 
               start = c(1970, 1), freq = f)

# Rainfall lag-1
rainy_ts <- cbind(rainy_ts, lag(rainy_ts, -1))

# Rain?
Rain <- ts((ifelse(rainy_data$Rainfall > 0, 1, 0)), 
           start = c(1970, 1), freq = f)
rainy_ts <- cbind(rainy_ts, Rain)

# Lag1
Lag1 <- lag(Rain, -1)
rainy_ts <- cbind(rainy_ts, Lag1)

dimnames(rainy_ts)[[2]] <- c("Rainfall", "Rainfall_lag1", "Rain", "Lag1")

# Build train and validations sets
train_ts <- window(rainy_ts, start = trainStart, end = trainEnd)
valid_ts <- window(rainy_ts, start = validStart, end = validEnd)


# t
t_train <- ts(1:nTrain, start = trainStart, freq = f)
train_ts <- cbind(train_ts, t_train)

t_valid <- ts(1:nValid, start = validStart, freq = f)
valid_ts <- cbind(valid_ts, t_valid)

# Seasonal_sine
Seasonal_sine <- ts(sin(2*pi*t_train / f), start = trainStart, freq = f)
train_ts <- cbind(train_ts, Seasonal_sine)

Seasonal_sine <- ts(sin(2*pi*t_valid / f), start = validStart, freq = f)
valid_ts <- cbind(valid_ts, Seasonal_sine)

# Seasonal_cosine
Seasonal_cosine <- ts(cos(2*pi*t_train / f), start = trainStart, freq = f)
train_ts <- cbind(train_ts, Seasonal_cosine)

Seasonal_cosine <- ts(cos(2*pi*t_valid / f), start = validStart, freq = f)
valid_ts <- cbind(valid_ts, Seasonal_cosine)

# Good names

dimnames(train_ts)[[2]] <- c("Rainfall", "Rainfall_lag1", "Rain", "Lag1", "t",
                     "Seasonal_sine", "Seasonal_cosine")
dimnames(valid_ts)[[2]] <- c("Rainfall", "Rainfall_lag1", "Rain", "Lag1", "t",
                             "Seasonal_sine", "Seasonal_cosine")

# Build logistic regression model
rainy_lrm <- glm(formula = Rain ~ Lag1 + Seasonal_sine + Seasonal_cosine,
                 family = "binomial", data = as.data.frame(train_ts))

# Predictive performance
library(caret)
rainy_lrm_train_pred <- ifelse(predict(rainy_lrm, train_ts, type = "response") > 0.5, 1, 0)
confusionMatrix(data = rainy_lrm_train_pred, reference = train_ts[,"Rain"])

rainy_lrm_valid_pred <- ifelse(predict(rainy_lrm, valid_ts, type = "response") > 0.5, 1, 0)
confusionMatrix(data = rainy_lrm_valid_pred, reference = valid_ts[,"Rain"])

# The model does not improve a naive forecast


# 8.5 PROBLEMS ------------------------------------------------------------

## Predicting powdery mildew in mango
# 
# The epidemic tipically occurs in the 3rd and 4th week of March, and hence the 
# outbreak status is known by the end of March of a given year.
library(readxl)
PME <- read_excel("./DATA/PowderyMildewEpidemic.xls")
PME
names(PME) <- make.names(names(PME))
target <-   names(PME)[2]
features <- names(PME)[3:4]

# 1 - In order for the model to serve as a forewarming system for farmers, what
#     rquirements must be satisfied regarding data availability?
#
# Year t-1 Outbreak, Max temperature and Relative humidity must be known before
# the year t possible outbreak (3rd week of march) 
#
# 2 - Write an equation for the model.

# log(odds(Outbreak)_t) = 
# beta_0 + beta_11 * max_temp_(t-1) + beta_12 * rel_humidity_(t-1) +... + 
#          beta_n1 * xmax_temp(t-n) + beta_n2 * rel_humidity_(t-n)

# 3 - Create a scatterplot of the 2 predictors, using different hue for
#     epidemic and non epidemic markers. Does ther appear to be a relationship
#     between epidemic status and the 2 predictors?

library(ggplot2)

gplot <- ggplot(data = PME, mapping = aes(x = Max.temp, y = Rel.humidity))
gplot + geom_point(mapping = aes(color = Outbreak, size = Outbreak))

# 4 - Compute naive forecasts of epidemic status for years 1995-1997 using
#     next-year forecasts (F_(t+1) = F_t). What is the naive forecast for year 
#     2000? Summarize the results for this 4 years in a classification matrix.

n_forecast <- data.frame(Year = PME$Year, 
                         Obs  = PME$Outbreak,
                         n_forecast = c(NA, PME$Outbreak[1:11]))
n_forecast

caret::confusionMatrix(data = n_forecast$n_forecast, reference = n_forecast$Obs)

# 5 - Partition the data into training and validation periods, so that years
#     1987-1994 are the training period. Fit a logistic regression to the 
#     training period using the two predictors, and report the outbreak
#     probability as well as a forecast for year 1995 (usea thhreshold of 0.5).
nTrain <- 1994 -1987 + 1
nTotal <- nrow(PME)
nValid <- nTotal - nTrain

PME$Outbreak <- ifelse(PME$Outbreak == "Yes", 1, 0)
PME$Lag1.Max.temp <- c(NA, PME$Max.temp[1:(nTotal-1)])
PME$Lag1.Rel.humidity <- c(NA, PME$Rel.humidity[1:(nTotal-1)])

train_df <- PME[1:nTrain, ]
valid_df <- PME[(nTrain+1):nTotal, ]

pme_lrm <- glm(Outbreak ~ Lag1.Max.temp + Lag1.Rel.humidity, 
               data = train_df,
               family = "binomial")
summary(pme_lrm)

ifelse(predict(pme_lrm, 
               valid_df[valid_df$Year == 1995, ], 
               type = "response") > 0.5,
       1, 0)

# 6 - Generate outbreak forecasts for years 1996, 1997, and 2000 by
#     repeatedly moving the training period forward. For example, to forecast
#     year 1996, partition the data so that years 1987-1995 are the training 
#     period. Then fit the logistic regression model and use it to generate
#     a forecast (use threshold 0.5)
out <- c()
for (i in 0:3) {
  train_df <- PME[1:nTrain + i, ]
  valid_df <- PME[(nTrain + i + 1):nTotal, ]
  
  pme_lrm <- glm(Outbreak ~ Lag1.Max.temp + Lag1.Rel.humidity, 
                 data = train_df,
                 family = "binomial")
  summary(pme_lrm)
  
  p <- ifelse(predict(pme_lrm, 
                 valid_df[1, ], 
                 type = "response") > 0.5,
         1, 0)
  out <- c(out, as.numeric(p))
  print(as.numeric(p))
}
names(out) <- c("1995", "1996", "1997", "2000")
out <- factor(out, levels = c(1, 0), labels = c("yes", "no"))
out

# 7 - 
caret::confusionMatrix(out, factor(PME$Outbreak[9:12], 
                                   levels = c(1, 0), labels = c("yes", "no")))

# 8 -

# 9 -
predict(pme_lrm, PME[9:12,], type = "response")
# 10 - 