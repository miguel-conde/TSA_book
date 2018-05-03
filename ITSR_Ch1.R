
# The classic air passenger data set --------------------------------------

data(AirPassengers)
AP <- AirPassengers
AP


class(AP)

start(AP)
end(AP)
frequency(AP)

plot(AP)

# the seasonal effect can be ‘removed’ by aggregation of the data to the 
# annual level, which can be achieved in R using the aggregate function
aggregate(AP)
plot(aggregate(AP), ylab="Annual passengers/1000’s")

# summary of the values for each season can be viewed using a boxplot, with 
# the cycle function to extract the seasons for each item of data.
cycle(AP)
boxplot(AP ~ cycle(AP), names=month.abb)


# Electricity, beer and chocolate data ------------------------------------
# https://github.com/kaybenleroll/dublin_r_workshops
www = "https://raw.githubusercontent.com/kaybenleroll/dublin_r_workshops/master/ws_timeseries_201309/cbe.dat"
# www <- "./DATA/cbe.dat"
cbe = read.table(www, header=T)

cbe[1:4,]
class(cbe)

ts(1:120, start=c(1990, 1), end=c(1993, 8), freq=12)
ts(1:120, start=c(1991, 7), end=c(1993, 8), freq=12)


elec.ts = ts(cbe[,3], start=1958, freq=12)
beer.ts = ts(cbe[,2], start=1958, freq=12)
choc.ts = ts(cbe[,1], start=1958, freq=12)


head(cbind(elec.ts, beer.ts, choc.ts))
plot(cbind(elec.ts, beer.ts, choc.ts),
     main="Chocolate, Beer, and Electricity Production: 1958-1990")


ap.elec = ts.intersect(AP, elec.ts)
start(ap.elec); end(ap.elec)
ap.elec[1:3, ]


ap = ap.elec[,1]; elec = ap.elec[,2]
layout(1:2)
plot(ap, main="", ylab="Air passengers / 1000’s")
plot(elec, main="", ylab="Electricity production / MkWh")

layout(1:1)
plot(as.vector(ap), as.vector(elec),
       xlab="Air passengers / 1000’s",
       ylab="Electricity production / MWh")
abline(reg=lm(elec ~ ap))

cor(ap,elec)

# Any series that has a trend will tend to be correlated to any other series 
# that has a trend. For this reason, it is preferable to remove trends and 
# seasonal effects before comparing multiple series.

# The term non-stationary is used to describe a time series that has underlying
# trends or seasonal effects.


# Quarterly exchange rate data --------------------------------------------

# The exchange rates, for British pounds sterling to New Zealand dollars
# for the period January 1991 to March 2000 are used next. The
# data are mean values taken over quarterly periods of three months, with the
# first quarter being January to March and the last quarter being October to
# December.
www = "https://raw.githubusercontent.com/kaybenleroll/dublin_r_workshops/master/ws_timeseries_201309/pounds_nz.dat"
z = read.table(www, header=T)

z[1:4,]

z.ts = ts(z, st=1991, fr=4)
plot(z.ts, xlab="time / years",
     ylab="Quarterly exchange rate in $NZ / pound")


dummy.ts = ts(st=c(1992, 1), end=c(1996, 1), fr=4)
z.92_96 = ts.intersect(z.ts, dummy.ts)[,1]
dummy.ts = ts(st=c(1996, 1), end=c(1998, 1), fr=4)
z.96_98 = ts.intersect(z.ts, dummy.ts)[,1]

layout(1:2)
plot(z.92_96,
     ylab="Exchange rate in $NZ/pound", xlab="time/years")
plot(z.96_98,
     ylab="Exchange rate in $NZ/pound", xlab="time/years")
layout(1:1)


# Global temperature series -----------------------------------------------

www = "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt"
global = read.table (www)
#global.ts = ts(global, st=c(1856,1), end=c(2005,12), fr=12)
global.ts = ts(global[,2], st=c(1850,1), fr=12)
global.annual = aggregate(global.ts, FUN=mean)
layout(1:2)
plot(global.ts); plot(global.annual)
layout(1:1)

dummy.ts = ts(st=c(1970, 1), end=end(global.ts), fr=12)
new.series = ts.intersect(global.ts, dummy.ts)[,1]
new.time = time(new.series)
plot(new.series); abline(reg=lm(new.series ~ new.time))


# The correlogram ---------------------------------------------------------

x <- ap
y <- elec
n <- length(x)

# Mean, variance, covariance. Not biased estimators.
mean(x)
sum(x)/n

var(x)

mean((x - mean(x))^2)
sum((x - mean(x))^2)/n

sum((x - mean(x))^2)/(n - 1) # NOT BIASED

cov(x, y)

sum((x - mean(x)) * (y - mean(y))) / (n - 1)

# Correlation
# Just a standarization of the covariance
cor(x, y)

cov(x, y) / sd(x) / sd(y)

# Autocorrelation
cor(x[1:(n-1)], x[2:n]) # lag 1
cor(x[1:(n-2)], x[3:n]) # lag 2

sapply(0:(n-1), function(l) {
  cor(x[1:(n-l)], x[(1+l):n])
})

cov(x[1:(n-1)], x[2:n]) / sd(x) / sd(x)

acf(x, plot = FALSE)$acf

# This is what they're using (it's a BIASED version)
mean((x[1:(n-1)] - mean(x)) * (x[2:n] - mean(x))) / sd(x) / sd(x)

sapply(0:(n-1), function(l) {
  mean((x[1:(n-l)] - mean(x)) * (x[(1+l):n] - mean(x))) / sd(x) / sd(x)
})

# In a time series analysis, a biased estimate often has to be tolerated 
# because the observations are serially dependent. In most cases this does 
# not present a problem as the sample size n is usually great


acf(x)
acf(x)$acf
acf(x)$acf[2] # lag 1


## Second order properties
mean(x)
acf(x, plot = FALSE)$acf * sd(x) * sd(x) # Autocovariance


acf(AP)


# Decomposition -----------------------------------------------------------

# decompose(): methods based on moving averages
# stl(): methods based on polynomials

plot(decompose(elec.ts))

elec.decom = decompose(elec.ts, type = "mult")
plot(elec.decom)

trend <- elec.decom$trend
seasonal <- elec.decom$seasonal

ts.plot(cbind(trend, trend*seasonal), lty = 1:2)


# Forecasting -------------------------------------------------------------

# One-step ahead predictions for the exchange rate data
z.hw = HoltWinters(z.ts, beta=0, gamma=0)
z.hw$alpha

plot(z.hw)

# Four-year ahead forecasts for the air passenger data
AP.hw = HoltWinters(AP, seasonal="mult")
plot(AP.hw)

AP.predict = predict(AP.hw, n.ahead=4*12)
ts.plot(AP, AP.predict, lty=1:2)



# Exercises ---------------------------------------------------------------

## 1. Carry out the following exploratory time series analysis using the 
#  global temperature series

www = "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt"
global = read.table (www)
global.ts = ts(global[,2], st=c(1850,1), fr=12)

# a) Produce a time plot of the data. Plot the aggregated annual mean
#    series, and a boxplot which summarises the observed values for each
#    season, and comment on the plots.

global.annual <- aggregate(global.ts, FUN = mean)

layout(1:2)
plot(global.ts)
plot(global.annual)
layout(1:1)

global.seasonal <- aggregate(global.ts, nfrequency = 4, FUN = mean)
boxplot(global.seasonal ~ cycle(global.seasonal), 
        names = c("Winter", "Spring", "Summer", "Autumn"))
abline(h = 0, lty = 2)

# b) Decompose the series into the components: trend, seasonal effect and
#    residuals, and plot the decomposed series. Produce a plot of the trend
#    with a superimposed seasonal effect.
global.decomp <- decompose(global.ts, type = "mult")
plot(global.decomp)

trend    <- global.decomp$trend
seasonal <- global.decomp$seasonal

ts.plot(cbind(trend, trend*seasonal), lty = 1:2)

ts.plot(window(cbind(trend, trend*seasonal), 
               start = c(1970,1), end = end(global.ts)),
        lty = 1:2)
