require(zoo)


# R_xy --------------------------------------------------------------------

## R_xy
#  This function estimates E[X_t x Y_(t+h)]
R_xy <- function(x, y, h = 1) {
  if (!is.vector(x) | !is.vector(y)) 
    stop('x and y must be vectors')
  if (!is.numeric(x) | !is.numeric(y)) 
    stop('x and y must be numeric')
  if (!is.numeric(h))
    stop('h must be numeric')
  if (1 != length(h))
    stop('h must be a single number')
  if(length(x) != length(y)) 
    stop("x and y lengths must be equal")
  if(h < 0 | h > (length(x) - 1)) 
    stop("h must be between 1 and length(x) - 1")
  
  y_lag <- zoo(y) 
  y_lag <- lag(y_lag, h)
  
  # if (h >= 1) x <- x[-(1:h)]
  if (h >= 1) x <- x[-((length(x) - h + 1):length(x))]
  
  mean(x * y_lag)
}

R_xy_F <- function(x, y, h_max = round(10*log10(length(x)/2))) {
  sapply(0:h_max, function(k) {
    R_xy(x, y, k)
  })
}


# COVARIANCE --------------------------------------------------------------

## COVARIANCE X, Y
# This function estimates E[ (X_t - E(X_t))] x [(Y_(t+h) - E(Y_(t+h)))] ]
gamma_xy <- function(x, y, h = 1) {
  
  if (!is.vector(x) | !is.vector(y)) 
    stop('x and y must be vectors')
  if (!is.numeric(x) | !is.numeric(y)) 
    stop('x and y must be numeric')
  if (!is.numeric(h))
    stop('h must be numeric')
  if (1 != length(h))
    stop('h must be a single number')
  if(length(x) != length(y)) 
    stop("x and y lengths must be equal")
  if(h < 0 | h > (length(x) - 1)) 
    stop("h must be between 1 and length(x) - 1")
  
  y_lag <- zoo(y) 
  y_lag <- lag(y_lag, h)
  
  # if (h >= 1) x <- x[-(1:h)]
  if (h >= 1) x <- x[-((length(x) - h + 1):length(x))]
  
  mean( (x - mean(x)) * (y_lag - mean(y_lag)))
}

gamma_xy_F <- function(x, y, h_max = round(10*log10(length(x)/2))) {
  sapply(0:h_max, function(k) {
    gamma_xy(x, y, k)
  })
}


# CROSS CORRELATION -------------------------------------------------------

## CROSS CORRELATION X, Y
#  This function estimates COVARIANCE(X, Y) / [sigma_X x sigma_Y]
crossCorr <- function(x, y, h, type = "correlation") {
  
  if(!type %in% c("correlation", "covariance"))
    stop("type must be 'correlation' or 'covariance'")
  
  out <- gamma_xy(x, y, h)
  
  if (type == "correlation") {
    sigma_x <- sqrt(gamma_xy(x, x, 0))
    sigma_y <- sqrt(gamma_xy(y, y, 0))
    out <- out / sigma_x / sigma_y
  }
  
  out
}

crossCorr_F <- function(x, y, 
                        h_max = round(10*log10(length(x)/2)), 
                        type = "correlation") {
  sapply(0:h_max, function(k) {
    crossCorr(x, y, k, type = type)
  })
}


# AUTO R_x ----------------------------------------------------------------

## AUTO R_x
R_x <- function(x, h = round(10*log10(length(x)))) {
  if (!is.vector(x)) 
    stop('x must be a vector')
  if (!is.numeric(x)) 
    stop('x must be numeric')
  if (!is.numeric(h))
    stop('h must be numeric')
  if (1 != length(h))
    stop('h must be a single number')
  if(h < 0 | h > (length(x) - 1)) 
    stop("h must be between 1 and length(x) - 1")
  
  R_xy(x, x, h)
}

R_x_F <- function(x, h_max = round(10*log10(length(x)))) {
  sapply(0:h_max, function(k) {
    R_x(x, k)
  })
}


# AUTO COVARIANCE ---------------------------------------------------------

## AUTO COVARIANCE
gamma_x <- function(x, h = round(10*log10(length(x)))) {
  if (!is.vector(x)) 
    stop('x must be a vector')
  if (!is.numeric(x)) 
    stop('x must be numeric')
  if (!is.numeric(h))
    stop('h must be numeric')
  if (1 != length(h))
    stop('h must be a single number')
  if(h < 0 | h > (length(x) - 1)) 
    stop("h must be between 1 and length(x) - 1")
  
  gamma_xy(x, x, h)
}

gamma_x_F <- function(x, h_max = round(10*log10(length(x)))) {
  sapply(0:h_max, function(k) {
    gamma_x(x, k)
  })
}


# AUTO CORRELATION --------------------------------------------------------

## AUTO CORRELATION
autoCorr <- function(x, h, type = "correlation") {
  if(!type %in% c("correlation", "covariance"))
    stop("type must be 'correlation' or 'covariance'")
  
  crossCorr(x, x, h, type)
}

autoCorr_F <- function(x, 
                       h_max = round(10*log10(length(x))), 
                       type = "correlation") {
  sapply(0:h_max, function(k) {
    autoCorr(x, k, type = type)
  })
}