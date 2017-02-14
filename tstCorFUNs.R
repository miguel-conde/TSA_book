source("corFUNs.R")
source("dftFUNs.R")

N <- 1000

x <- makeExpFunc(N, level = 0, slope = 0, 
                 A = 1, T = 100, ph = .6*pi,
                 noise = NULL) +
  makeExpFunc(N, level = 0, slope = 0, 
              A = 3, T = 50, ph = .6*pi,
              noise = NULL) + 
  makeExpFunc(N, level = 0, slope = 0, 
              A = 2, T = 25, ph = .6*pi,
              noise = NULL)
plot(1:N,
     x,
     type = "l")


y <- makeExpFunc(N, level = 0, slope = 0, 
                 A = 7, T = 20, ph = .6*pi,
                 noise = NULL) +
  makeExpFunc(N, level = 0, slope = 0, 
              A = 2, T = 80, ph = .6*pi,
              noise = NULL) + 
  makeExpFunc(N, level = 0, slope = 0, 
              A = 4, T = 160, ph = .6*pi,
              noise = NULL)
plot(1:N,
     y,
     type = "l")

R_xy(x, y, 0)
R_xy(x, y, 1)
R_xy(x, y, 2)
R_xy(x, y, 3)
R_xy(x, y, 4)
R_xy(x, y, 5)

R_xy_F(x, y)


gamma_xy(x, y, 0)
gamma_xy(x, y, 1)
gamma_xy(x, y, 2)
gamma_xy(x, y, 3)
gamma_xy(x, y, 4)
gamma_xy(x, y, 5)

gamma_xy_F(x, y)

crossCorr(x, y, 0)
crossCorr(x, y, 1)
crossCorr(x, y, 2)
crossCorr(x, y, 3)
crossCorr(x, y, 4)
crossCorr(x, y, 5)

crossCorr_F(x, y)
ccf(x, y, plot = FALSE)

plot(crossCorr_F(x, y), type = "l", col = "blue")
lines(ccf(x, y, plot = FALSE)$acf[27:53], type = "l", col = "red")

crossCorr(x, y, 0, type ="covariance")
crossCorr(x, y, 1, type ="covariance")
crossCorr(x, y, 2, type ="covariance")
crossCorr(x, y, 3, type ="covariance")
crossCorr(x, y, 4, type ="covariance")
crossCorr(x, y, 5, type ="covariance")


## ----
R_x(x, 0)
R_x(x, 1)
R_x(x, 2)
R_x(x, 3)
R_x(x, 4)
R_x(x, 5)

R_x_F(x)

autoCorr(x, 0)
autoCorr(x, 1)
autoCorr(x, 2)
autoCorr(x, 3)
autoCorr(x, 4)
autoCorr(x, 5)

autoCorr(x, 0, type ="covariance")
autoCorr(x, 1, type ="covariance")
autoCorr(x, 2, type ="covariance")
autoCorr(x, 3, type ="covariance")
autoCorr(x, 4, type ="covariance")
autoCorr(x, 5, type ="covariance")

autoCorr_F(x)
autoCorr_F(x, type = "covariance")

acf(x, plot = FALSE)
acf(x, type = "covariance", plot = FALSE)

plot(autoCorr_F(x), type = "l", col = "blue")
lines(acf(x, plot = FALSE)$acf, type = "l", col = "red")
