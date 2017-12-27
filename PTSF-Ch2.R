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

library(forecast)
ridership_lm <- tslm(ridership_ts ~ trend + I(trend^2))

par(mfrow = c(2,1))
plot(ridership_ts, 
     xlab = "Time", ylab = "Ridership",
     bty = "l")
lines(ridership_lm$fitted.values, lwd = 2)

ridership_ts_zoom <- window(ridership_ts, start = c(1991,1), end = c(2000, 12))
plot(ridership_ts_zoom, 
     xlab = "Time", ylab = "Ridership",
     bty = "l")
par(mfrow = c(1,1))


### PLOTTING TS WITH GGPLOT
library(ggplot2)
plot <- ggplot(Amtrak_data, aes(x = as.Date(Month), y = Ridership)) + 
  geom_line() +
  xlab("Date") +
  ylab("Ridership")

plot
plot + scale_y_log10()
plot + scale_y_continuous(trans='log10')
plot + coord_trans(y = "log10")
plot + scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year")


# Plotting Time Series with ggplot2 and ggfortify
# http://rpubs.com/sinhrks/plot_ts



# Problem 2.6.1 -----------------------------------------------------------

library(readxl)
Sept11Travel <- 
  read_excel("./DATA/Sept11Travel.xls")

# Load pre-event time series
Rail_ts <- ts(Sept11Travel$`Rail PM`, 
                   start = c(1990,1,1), end = c(2001, 8, 1),
                   frequency = 12)
Air_ts  <- ts(Sept11Travel$`Air RPM (000s)`, 
              start = c(1990,1,1), end = c(2001, 8, 1),
              frequency = 12)
Auto_ts <- ts(Sept11Travel$`VMT (billions)`, 
              start = c(1990,1,1), end = c(2001, 8, 1),
              frequency = 12)

### PLOTS
par(mfrow = c(3, 1))

## PLOT RAIL
plot(Rail_ts,
     xlab = "Date", ylab = "Miles", 
     main = "Rail passenger miles",
     bty = "l")
abline(v=2001.75, col = "red")

Rail_ts_lm <- tslm(Rail_ts ~ trend + I(trend^2) + I(trend^3))
lines(Rail_ts_lm$fitted.values, lwd = 2)

## PLOT AIR
plot(Air_ts,
     xlab = "Date", ylab = "Miles", 
     main = "Actual airline revenue passenger miles",
     bty = "l")
abline(v=2001.75, col = "red")

Air_ts_lm <- tslm(Air_ts ~ trend)
lines(Air_ts_lm$fitted.values, lwd = 2)

## PLOT AUTO
plot(Auto_ts,
     xlab = "Date", ylab = "Miles", 
     main = "Vehicle miles traveled",
     bty = "l")
abline(v=2001.75, col = "red")

Auto_ts_lm <- tslm(Auto_ts ~ trend)
lines(Auto_ts_lm$fitted.values, lwd = 2)

par(mfrow = c(1,1))

## Try zoom in
# F_11S <- 2001+254/365
Air_ts_zoom <- window(Air_ts, start = c(1999,1), end = c(2000,12))
plot(Air_ts_zoom,
     xlab = "Date", ylab = "Miles", 
     main = "Actual airline revenue passenger miles",
     bty = "l")

## Change the scale
plot(Air_ts_zoom,
     log = "y",
     xlab = "Date", ylab = "Miles", 
     main = "Actual airline revenue passenger miles",
     bty = "l",
     xy.lines = T)


## Adding trend lines
# (done before)

## Supressing seasonality
# Plotting at a cruder time scale
Air_ts_years <- window(Air_ts, frequency = 1)
plot(Air_ts_years,
     xlab = "Date", ylab = "Miles", 
     main = "Actual airline revenue passenger miles",
     bty = "l")

# Plot separate plots for each season
years <- c(1990, 1992, 1994, 1996, 1998, 2000)

par(mfrow = c(3,2))
for (year in years) {
  year_window <- window(Air_ts, start = c(year,1), end = c(year, 12))
  plot(year_window,
       xlab = "Date", ylab = "Miles", 
       main = paste("Actual airline revenue passenger miles", year),
       bty = "l")
}
par(mfrow = c(1,1))

# Moving averages
plot(Air_ts,
     xlab = "Date", ylab = "Miles", 
     main = "Actual airline revenue passenger miles",
     bty = "l")
abline(v=2001.75, col = "red")

# 2-sided MA
lines(stats::filter(x= Air_ts, 
                    method = "convolution", 
                    filter = rep(1,12)/12,
                    sides = 2),
      col = "red")


### END PLOTS

# Problem 2.6.2 -----------------------------------------------------------
DepartmentStoreSales <- read_excel("./DATA/DepartmentStoreSales.xls", 
                                   sheet = "Sheet1")
DSS_ts <- ts(DepartmentStoreSales$Sales, 
              # start = c(1990,1,1), end = c(2001, 8, 1),
              frequency = 4)
plot(DSS_ts,
     xlab = "Year", ylab = "Sales ($)", 
     main = "Quarterly Sales",
     bty = "l")

# b) Level, trend, seasonality

# Problem 2.6.3 -----------------------------------------------------------
ApplianceShipments <- read_excel("./DATA/ApplianceShipments.xls")

ApplianceShipments$Quarter <- paste0(substr(ApplianceShipments$Quarter, 4, 7),
                                     "-", 
                                     substr(ApplianceShipments$Quarter, 1, 2))
ApplianceShipments <- ApplianceShipments[order(ApplianceShipments$Quarter), ]

AS_ts <- ts(ApplianceShipments$Shipments, 
             # start = c(1990,1,1), end = c(2001, 8, 1),
             frequency = 4)
plot(AS_ts,
     xlab = "Year", ylab = "Shipments ($)", 
     main = "Quarterly Shipments",
     bty = "l")

# b) Level, trend seasonality

# Problem 2.6.4 -----------------------------------------------------------
CanadianWorkHours <- read_excel("./DATA/CanadianWorkHours.xls")

CWH_ts <- ts(CanadianWorkHours$`Hours per week`, 
             start = c(1966), end = c(2000),
             frequency = 1)
plot(CWH_ts,
     xlab = "Year", ylab = "Hours per Week", 
     main = "Canadian Hours per Week",
     bty = "l")

# b) Level, trend 

# Problem 2.6.5 -----------------------------------------------------------
SouvenirSales <- read_excel("./DATA/SouvenirSales.xls")

SS_ts <- ts(SouvenirSales$Sales, 
             start = c(1995, 1, 1), end = c(2001, 12, 1),
             frequency = 12)
plot(SS_ts,
     xlab = "Date", ylab = "Sales ($)", 
     main = "Souvenir Sales",
     bty = "l")
SS_ts_lm <- tslm(SS_ts ~ I(exp(trend)))
lines(SS_ts_lm$fitted.values)

# b)  
plot(SS_ts,
     xlab = "Date", ylab = "Sales ($)", 
     main = "Souvenir Sales - log(y)",
     log = "y",
     bty = "l")
plot(SS_ts,
     xlab = "Date", ylab = "Sales ($)", 
     main = "Souvenir Sales - log(xy)",
     log = "xy",
     bty = "l")

# WITH GGPLOT
library(ggplot2)
plot <- ggplot(SouvenirSales, aes(x = 1:nrow(SouvenirSales), y = Sales)) + 
  geom_line() +
  xlab("Date") +
  ylab("Sales($)") 
#+
 # scale_x_date(date_labels = "%Y", date_breaks = "1 year")

plot
plot + scale_y_log10()
plot + scale_y_continuous(trans='log10')
plot + coord_trans(y = "log10")
plot + coord_trans(x = "log10", y = "log10")

plot + stat_smooth()
plot + stat_smooth() + scale_y_continuous(trans='log10')
plot + stat_smooth() + scale_x_continuous(trans='log10')
plot + stat_smooth() + scale_y_continuous(trans='log10') + scale_x_continuous(trans='log10')


# Problem 2.6.6 -----------------------------------------------------------
ShampooSales <- read_excel("./DATA/ShampooSales.xls")

ShS_ts <- ts(ShampooSales$`Shampoo Sales`, 
            start = c(1995, 1), end = c(1997, 12),
            frequency = 12)
plot(ShS_ts,
     xlab = "Date", ylab = "Sales ($)", 
     main = "Shampoo Sales",
     bty = "l")
ShS_ts_lm <- tslm(ShS_ts ~ I(trend^2))
lines(ShS_ts_lm$fitted.values)

# Seasonality ?
# Plot separate plots for each season
years <- c(1995, 1996, 1997)

par(mfrow = c(3,1))
for (year in years) {
  year_window <- window(ShS_ts, start = c(year,1), end = c(year, 12))
  plot(year_window,
       xlab = "Date", ylab = "Sales ($)", 
       main = paste("Shampoo Sales", year),
       bty = "l")
}
par(mfrow = c(1,1))
