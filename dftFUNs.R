

## SLOW but STEP-BY-STEP Discrete Fourier Transform
dft0 <- function(x) {
  N <- length(x)
  Ns <- 1:N
  ks <- Ns - 1
  w_0 <- 2*pi/N
  w_ks <- w_0*ks
  T_ks <- N/ks
  f_ks <- ks/N
  names(w_ks) <- names(T_ks) <- names(f_ks) <- paste0("k_", ks)
  
  out <- c()
  for (idx_k in 1:length(ks)) {
    out <- c(out, a_k(x, w_ks[idx_k]))
  }
  names(out) <- paste0("a_", ks)
  
  out <- list(dft = out, w_ks = w_ks, T_ks = T_ks, f_ks = f_ks, N = N)
  out
}

X_k <- function(x, w_k) {
  N <- length(x)
  Ns <- 1:N
  
  out <- 0
  for (n in Ns) {
    out <- out + x[n] * exp(-1i * w_k * (n - 1))
  }
  
  return(out)
}

a_k <- function(x, w_k) {
  N <- length(x)
  
  out <- (1 / N) * X_k(x, w_k)
  
  return(out)
}

## fft (Fast Fourier Transform) based Discrete Fourier Transform
dft <- function(x) {
  N <- length(x)
  Ns <- 1:N
  ks <- Ns - 1
  w_0 <- 2*pi/N
  w_ks <- w_0*ks
  T_ks <- N/ks
  f_ks <- ks/N
  names(w_ks) <- names(T_ks) <- names(f_ks) <- paste0("k_", ks)

  out <- fft(x) / N
  names(out) <- paste0("a_", ks)
  
  out <- list(dft = out, w_ks = w_ks, T_ks = T_ks, f_ks = f_ks, N = N)
  out
}

## Inverse Discrete Fourier Transform
idft <- function(ft) {
  out <- fft(ft$dft, inverse = TRUE)
  names(out) <- paste("x_0", 0:(length(out)-1))
  out
}

dftPower <- function(x,
                     make_percent = FALSE, make_order = FALSE,
                     type = "1-sided") {
  ft <- dft(x)
  
  if (type == "1-sided")
    out <- dftPower1S(ft)
  else
    out <- dftPower2S(ft)
  
  if (make_percent == TRUE) {
    out$spectrum <- out$spectrum / out$totalPower * 100
  }
  
  if (make_order == TRUE) {
    idxOrder <- order(out$spectrum, decreasing = TRUE)
    
    out$spectrum <- out$spectrum[idxOrder]
    out$dft$w_ks <- out$dft$w_ks[idxOrder]
    out$dft$T_ks <- out$dft$T_ks[idxOrder]
    out$dft$f_ks <- out$dft$f_ks[idxOrder]
  }
  
  out$percent <- make_percent
  out$order <- make_order
  
  class(out) <- "dftp"
  out
}

dftPower2S <- function(ft) {
  out <- list(dft = ft,
              spectrum = Mod(ft$dft)^2,
              totalPower = sum(Mod(ft$dft)^2))
  out
}

dftPower1S <- function(ft) {
  
  spectrum <- Mod(ft$dft)^2
  
  k_pi <- ft$N / 2
  
  idx_dc <- 1
  last_idx <- as.integer(k_pi) + 1
  if(is.even(ft$N)) {
    idx_X2 <- (idx_dc+1):(last_idx - 1)
    idx_X1 <- last_idx
    
    spectrum <- c(spectrum[idx_dc], 2*spectrum[idx_X2], spectrum[idx_X1])
  }
  else
  {
    idx_X2 <- (idx_dc+1):last_idx
    spectrum <- c(spectrum[idx_dc], 2*spectrum[idx_X2])
  }
  
  out <- list(dft = ft,
              spectrum = spectrum,
              totalPower = sum(spectrum))
  out
}


is.even <- function(x) x %% 2 == 0

is.odd <- function(x) !is.even(x)


## Plot DFT Periodogram
plot.dftp <- function(dftP, xtype = "frequency", thr = 0,
                     type = "h", ...) {
  
  if(! xtype %in% c("frequency", "period", "angular")) {
    stop("xtype must be one of: frequency, period, angular")
  }
  
  if (xtype == "frequency") {
    x <- dftP$dft$f_ks[1:length(dftP$spectrum)]
  }
  else
  {
    if (xtype == "period") {
      x <- dftP$dft$T_ks[1:length(dftP$spectrum)]
    }
    else
    {
      x <- dftP$dft$w_ks[1:length(dftP$spectrum)]
    }
  }
  
  Ylab = ifelse(dftP$percent == TRUE, "% Power", "Power")
  
  plot(x = x[dftP$spectrum > thr], 
       y = dftP$spectrum[dftP$spectrum > thr], 
       type = type,
       xlab = xtype,
       ylab = Ylab, 
       ...)
}

##
makeExpFunc <- function(N, level = 0, slope = 0, A = 0, T = 50, ph = 0, 
                        noise = NULL,
                        seed = 123) {
  n <- 1:N
  
  x_level = rep(level, N)
  
  x_trend = n * slope
  
  x_stational = A*cos(2*pi*n/T + ph)
  
  if (!is.null(noise)) {
    set.seed(seed)
    x_noise = rnorm(N, noise["mean"], noise["sd"])
  }
  else
    x_noise = rep(0, N)
  
  x_level + x_trend + x_stational + x_noise
}

## Random Walk
rw <- function(n = 100, x_0 = 0, drift = 0, mean = 0, sd = 1) {

  out <- rep(0, n + 1)
  out[1] <- x_0
  for (i in 2:(n+1)) {
    out[i] <- out[i - 1] + drift + rnorm(n = 1, mean = mean, sd = sd)
  }
  
  out[2:(n+1)]
}

rw2 <- function(n = 100, x_0 = 0, drift = 0, mean = 0, sd = 1) {

  # x[n] = x_0 + n x drift + cumsum_1^n(rnorm)
  out <- x_0 + (1:n) * drift + cumsum(rnorm(n, mean = mean, sd = sd))
  
  out

}

stepFUN <- function(n) {
  
  if (is.numeric(n)) {
    m <- as.integer(n)
    if (m - n != 0)
      stop(sprintf("n = %s is not an integer", as.character(n)))
  }
  else {
    stop(sprintf("n = %s is not an integer", as.character(n)))
  }
  
  ifelse(m < 0, 0, 1)
}

impulseFUN <- function(n) {
  stepFUN(n) - stepFUN(n-1)
}
