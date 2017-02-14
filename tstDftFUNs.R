source("dftFUNs.R")

## Test dft()
dft0(1:4)$dft
dft(1:4)$dft
fft(1:4) / 4

idft(dft(1:4))




N <- 500
n <- 1:N
T <- 50
ph <- .6*pi
set.seed(1000) # so you can reproduce these results
x = 2*cos(2*pi*n/T + ph) + rnorm(N,0,5)

dft0(x)$dft[178]
fft(x)[178]/N
dft(x)$dft[178]

dft0(x)$dft[25]
fft(x)[25]/N
dft(x)$dft[25]


idft(dft(x))

# Test dftPower()
pow <- dftPower(x)
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")

x <- 1:4
sum(abs(x)^2)/length(x)
pow <- dftPower(x)
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")

x <- 1:5
sum(abs(x)^2)/length(x)
pow <- dftPower(x)
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")

x <- 1:6
sum(abs(x)^2)/length(x)
pow <- dftPower(x)
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")

x <- 1:7
sum(abs(x)^2)/length(x)
pow <- dftPower(x)
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")


N <- 500
n <- 1:N
T <- 50
ph <- .6*pi
set.seed(1000) # so you can reproduce these results
x = 2*cos(2*pi*n/T + ph) + rnorm(N,0,5)

# 1-sided
sum(abs(x)^2)/length(x)
pow <- dftPower(x)
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")
plot.dftp(pow)

pow <- dftPower(x, make_percent = TRUE, make_order = FALSE)
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")
plot.dftp(pow)

pow <- dftPower(x, make_percent = FALSE, make_order = TRUE)
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")

pow <- dftPower(x, make_percent = TRUE, make_order = TRUE)
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")

# 2-sided
sum(abs(x)^2)/length(x)
pow <- dftPower(x, type ="2-sided")
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")

pow <- dftPower(x, make_percent = TRUE, make_order = FALSE, type ="2-sided")
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")

pow <- dftPower(x, make_percent = FALSE, make_order = TRUE, type ="2-sided")
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")

pow <- dftPower(x, make_percent = TRUE, make_order = TRUE, type ="2-sided")
pow
plot(pow$dft$f_ks[1:length(pow$spectrum)], pow$spectrum, type = "h")

# Test odd & even functions
is.even(3)
is.even(4)

is.odd(3)
is.odd(4)


###


plot(1:100,
     makeExpFunc(100, level = 0, slope = 0, 
                 A = 1, T = 50, ph = .6*pi,
                 noise = NULL),
     type = "l")
plot(1:100,
     makeExpFunc(100, level = 10, slope = 0, 
                 A = 1, T = 50, ph = .6*pi,
                 noise = NULL),
     type = "l")
plot(1:100,
     makeExpFunc(100, level = 0, slope = 0.1, 
                 A = 1, T = 50, ph = .6*pi,
                 noise = NULL),
     type = "l")
plot(1:100,
     makeExpFunc(100, level = 10, slope = 0.1, 
                 A = 1, T = 50, ph = .6*pi,
                 noise = NULL),
     type = "l")
plot(1:100,
     makeExpFunc(100, level = 10, slope = 0.1, 
                 A = 1, T = 50, ph = .6*pi,
                 noise = c(mean = 1, sd = 2)),
     type = "l")


x <- makeExpFunc(100, level = 0, slope = 0, 
                 A = 1, T = 33, ph = .6*pi,
                 noise = c(mean = 1, sd = 2)) +
  makeExpFunc(100, level = 0, slope = 0, 
              A = 3, T = 39, ph = .6*pi,
              noise = c(mean = 1, sd = 2)) + 
  makeExpFunc(100, level = 0, slope = 0, 
              A = 2, T = 51, ph = .6*pi,
              noise = c(mean = 1, sd = 2))
plot(1:100,
     x,
     type = "l")

pow <- dftPower(x, make_percent = TRUE, make_order = TRUE)
pow
plot.dftp(pow)


x <- makeExpFunc(100, level = 0, slope = 0, 
                 A = 1, T = 100, ph = .6*pi,
                 noise = NULL) +
  makeExpFunc(100, level = 0, slope = 0, 
              A = 3, T = 50, ph = .6*pi,
              noise = NULL) + 
  makeExpFunc(100, level = 0, slope = 0, 
              A = 2, T = 25, ph = .6*pi,
              noise = NULL)
plot(1:100,
     x,
     type = "l")

pow <- dftPower(x, make_percent = TRUE, make_order = TRUE)
pow
plot.dftp(pow)


###
# Autocovarianza
gamma_h <- acf(x, type = "covariance", plot = FALSE, lag.max = length(x)-1)
gamma_h

R_x <-  (as.numeric(gamma_h$acf) + mean(x)^2)

g_f <- dft(R_x)
g_f$dft
dftPower(x, type = "2-sided")$spectrum


# With my functions
R_x <- autoCorr_F(x, h_max = length(x)-1, type = "covariance") + mean(x)^2
g_f <- dft(R_x)
head(g_f$dft)
head(dftPower(x, type = "2-sided")$spectrum)


g_f <- dft(R_x_F(x))
head(g_f$dft)
head(dftPower(x, type = "2-sided")$spectrum)

###
N <- length(x)
M <- 2^10 # zero-pad total length
freq <- seq(0, 0.5, by = 1/M)

x.zp <- c(x, rep(0, M-N))
S.pgram <- (1/N)*abs(fft(x.zp)[1:(M/2+1)])^2

plot(freq, S.pgram, type='l', log='y', xlab = "Frequency", ylab = "Spectrum")


##

stepFUN(-1)
stepFUN(0)
stepFUN(1)
stepFUN(1.7)
stepFUN(T)
stepFUN("asd")

impulseFUN(-1)
impulseFUN(0)
impulseFUN(1)
impulseFUN(1.7)
impulseFUN(T)
impulseFUN("asd")


plot(-10:10, sapply(-10:10, stepFUN), type = "h", lty = 1, ylim = c(0,2))
lines(-10:10, sapply(-10:10, stepFUN), type = "p")

plot(-10:10, sapply(-10:10, impulseFUN), type = "h", lty = 1, ylim = c(0,2))
lines(-10:10, sapply(-10:10, impulseFUN), type = "p")


####
set.seed(2435)

random_walk <- rw(100)
plot(random_walk, type = "l")
mean(random_walk)

random_walk <- rw(1000)
plot(random_walk, type = "l")
mean(random_walk)

random_walk <- rw(10000)
plot(random_walk, type = "l")
mean(random_walk)


