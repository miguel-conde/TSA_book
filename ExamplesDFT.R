N <- 100000

wn <- rnorm(N, 10, 2)

acf(wn)
acf(wn, type = "correlation")
acf(wn, type = "covariance")

acf_corr <- acf(wn, type = "correlation", plot = FALSE)
acf_cov  <- acf(wn, type = "covariance", plot = FALSE)
acfs <- data.frame(Autocorrelation_lag = acf_corr$lag,
                   Autocorrelation = acf_corr$acf,
                   Autocovariance_lag  = acf_cov$lag, 
                   Autocovariance  = acf_cov$acf)
acfs

var_wn <- acf_cov$acf[1]
mu_2_wn <- mean(wn)

E_x_th_xt <- var_wn*acf_corr$acf + mu_2_wn
E_x_th_xt
plot(E_x_th_xt, type = "h")

acf_corr$acf - E_x_th_xt

# ------------
N <- 100000
n <- 1:N

set.seed(123)
wn <- rnorm(N, 0, 2)

T <- 50 
fs <- (0:(N/2 - 1)) / N

hid_tone <- wn + cos(2*n*pi/T) + cos(2*n*pi/(T/5))
plot.ts(hid_tone[1:200])
abline(h=0, col = "blue")

acf_corr_ht <- acf(hid_tone, type = "correlation")

periodHT <- abs(fft(hid_tone))^2/N
plot(fs, periodHT[1:(N/2)], type = "o")


s_periodHT <- 4/N*periodHT[1:(N/2)]
plot(fs, s_periodHT, type = "o")

# Continua:
s_periodHT[1]
fs[1]

# Alterna del coseno
idx_f <- which(s_periodHT == max(s_periodHT))
s_periodHT[idx_f]
1/fs[idx_f]

s_periodHT[order(s_periodHT, decreasing = TRUE)][1:10]
1/fs[order(s_periodHT, decreasing = TRUE)][1:10]
#----------------
N <- 500
n <- 1:N
T <- 50
ph <- .6*pi
set.seed(1000) # so you can reproduce these results
x = 2*cos(2*pi*n/T + ph) + rnorm(N,0,5)
plot.ts(x)


I = abs(fft(x))^2/N # the periodogram
P = (4/N)*I[1:(N/2)] # the scaled periodogram
f = 0:249/500 # frequencies
plot(f, P, type="l", xlab="Frequency", ylab="Scaled Periodogram")

P[1:20]
f[1:20]

P[11]
f[11]

1/f[11]

#--------
# Load the dataset, adjust, and convert to monthly returns
set.seed(42)
library(quantmod)
getSymbols('^GSPC', from='1990-01-01')
GSPC <- adjustOHLC(GSPC, symbol.name='^GSPC')
GSPC <- to.monthly(GSPC, indexAt='lastof')
plot(ClCl(GSPC))
summary(ClCl(GSPC))
hist(ClCl(GSPC)[-1], breaks = 20, freq = FALSE)
lines(density(ClCl(GSPC)[-1]))

Target <- as.numeric(ClCl(GSPC)[-1])

N <- length(Target)

I = abs(fft(Target))^2/N # the periodogram
P = (4/N)*I[1:(N/2)] # the scaled periodogram
f = (0:(N/2 - 1)) / N # frequencies
plot(f, P, type="l", xlab="Frequency", ylab="Scaled Periodogram")

P[order(P, decreasing = TRUE)][1:10]
1/f[order(P, decreasing = TRUE)][1:10]

plot(1/f[2:20], P[2:20], type="o", xlab="Period", ylab="Scaled Periodogram")

plot(1/f[order(P, decreasing = TRUE)][2:8], 
     P[order(P, decreasing = TRUE)][2:8]/sum(P)*100, 
     type="h", xlab="Period", ylab="Scaled Periodogram")
# -----
x  <- 1:4
N <- length(x)

dft <- fft(x)
dft

idft <- fft(dft, inverse = TRUE) / N
idft

#---
periodogram <- function(x, ordered = FALSE) {
  N <- length(x)
  
  T_0 <- N
  f_0 <- 1/T_0
  
  # The periodogram
  I <- abs(fft(x))^2 / N
  
  # The scaled periodogram
  P <- (4/N) * I[1:(N/2)]
  
  # Frequencies
  k     <- 0:(N/2 - 1)
  freqs <- k * f_0
  
  if (ordered == TRUE) {
    idxOrder <- order(P, decreasing = TRUE)
    P <- P[idxOrder]
    freqs <- freqs[idxOrder]
  }
  
  return(list(periodogram = P, frequencies = freqs, periods = 1/freqs))
}

p1 <- periodogram(as.numeric(ClCl(GSPC)[-1]))
p1

plot(p1$frequencies, p1$periodogram, type = "h")
plot(p1$periods[2:10], p1$periodogram[2:10], type = "h")
plot(p1$frequencies[2:10], p1$periodogram[2:10], type = "h")

p2 <- periodogram(as.numeric(ClCl(GSPC)[-1]), ordered = TRUE)
p2

plot(p2$periods[2:10], p2$periodogram[2:10], type = "h")
plot(p2$frequencies[2:10], p2$periodogram[2:10], type = "h")
plot(p2$periods[-1], p2$periodogram[-1], type = "h")

##
N <- 500
n <- 1:N
T <- 50
ph <- .6*pi
set.seed(1000) # so you can reproduce these results
x = 2*cos(2*pi*n/T + ph) + rnorm(N,0,5)

p3 <- periodogram(x, ordered = TRUE)
p3

plot(p3$periods[1:10], p3$periodogram[1:10], type = "h")
