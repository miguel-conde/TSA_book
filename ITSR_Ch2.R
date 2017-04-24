## 2. Basic Models

# White noise: ideal residual error series
# Models that contain white noise
# - Random Walk (RW)
# - Autoregressive (AR) models (RW is an special case of AR models) 

# 2.1 White Noise ---------------------------------------------------------

# 2.1.1 - Residual error time series: e_t = y_t - F_t
#
# If a model explains all the features in the data, the residual time series 
# would be uncorralated so that the correlogram of residuals series would
# exhibit no obvious pattern.
#
# 2.1.2 - Definition:
# A residual time series e_t is white noise if the variables e_1, e_2,...,e_n
# are independent and identically dsitributed (iid) with a mean of zero.
#
# Corollary:
# - The variables all have the same variance, so that cor(e_i, e_j) = 0 for
#   all i <> j
# 
# If, aditionally, the variables follow a Normal distribution, the series is 
# called aGaussian white noise.
# 
# 2.1.3 - Simulation
#
# Gaussian N(0, 1) white noise series of length 100
set.seed(1)
e <- rnorm(100)
plot(e, type = "l",
     main = "Time plot of simulated\n Gaussian white noise series",
     ylab = "e", xlab = "time")

hist(rnorm(100), prob = TRUE)
x <- seq(-3, 3, len = 1000)
points(x, dnorm(x), type = "l")

# 2.1.4 - 2nd order propoerties and the correlogram
# acf = rho_k = cov(e_t, e_(t+k)) / sigma(e_t) / sigma(e_(t+k))
# cov(X, Y) = E[(X - E(X)) * (Y - E(Y))] = E[XY - YE(X) - XE(Y) + E(X)E(Y)] =
# <e_t * e_(t+k)> - <e_(t+k)><e_t> - <e_t><e_(t+k) + <e_t><e_(t+k) =
# <e_t * e_(t+k)> - <e_t><e_(t+k) = {iid} = 
#  - if k = 0: <(e_t)^2> - <e_t>^2 = Var[e_t]
#  - if k <> 0: <e_t><e_(t+k) - <e_t><e_(t+k)> = 0
#
# So:
# - rho_k = 1 if k = 0
# - rho_k = 0 if k <> 0
par(mfrow = c(3,3))
for (i in 1:9) {
  acf(rnorm(100))
}
par(mfrow = c(1, 1))

# 2.1.5 - Fitting a white noise model
# Aftr fitting an appropriate time series model, the residual series should
# be white noise: the correlogram usually provides sufficient evidence to 
# support this conjeture.

# 2.2 Random Walks (RWs)
# 2.2.1 - Intro
#
# 2.2.2 - Definition
#
# A time series {y_t} is a RW if and only if:
#  x_t = x_(t-1) = e_t
# where {e_t} is a white noise.
#
# Observation:
# From the definition follows: x_t = e_t + e_(t-1) + e_(t-2) + ... + e_1

# 2.2.3 - The backward shift operator
#
# Definition
# 
# The backward shift (or lag) operator B is defined as B(x_t) = x_(t-1)
# 
# Generalizing: B^n(x_t) = x_(t-n)

# 2.2.4 - RW 2nd order properties

# 2.2.5 - Derivation of 2nd order properties

# 2.2.6 - The difference operator

# 2.2.7 - Simulation
x <- e <- rnorm(1000)
for( t in 2:1000) {
  x[t] <- x[t-1] + e[t]
}
plot(x, type = "l")

acf(x)
acf(diff(x))
