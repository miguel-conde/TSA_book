if(!require("signal")) {
  install.packages("signal")
  library(signal)
}


H_EMA_filter <- function(EMA_Period = 30) {
  
  if(EMA_Period <= 1) {
    stop("EMA_Period must be > 1")
  }
  
  K <- 2/(EMA_Period + 1)
  
  poly_num <- c(K, 0)
  poly_den <- c(1, -(1-K))
  
  return(freqz(poly_num, poly_den, 100))
  
}

# Convenience function to draw multiple plots
hPlot <- function(H, ...){
  text <- deparse(substitute(H))  # get the name of the filter for the title
  # c <- substr(text,4,4)
  
  par(mfrow = c(2, 1))
  plot(H$f,Mod(H$h),
       col="red",
       ylim=c(0,1),
       xlab="Normalized Frequency",
       ylab="Magnitude",
       main=paste("Filter H",text,"(z)",sep=""),
       ...
  )
  plot(H$f,Arg(H$h),
       col="red",
       # ylim=c(0,1),
       xlab="Normalized Frequency",
       ylab="Phase",
       main=paste("Filter H",text,"(z)",sep=""),
       ...
  )
  par(mfrow = c(2,2))
}

H_EMA_1 <- H_EMA_filter(EMA_Period = 10) 

H_EMA_1
plot(H_EMA_1)
freqz_plot(H_EMA_1)

hPlot(H_EMA_1, type ="o")


H_MACD_filter <- function(fastP = 12, slowP = 26, sigP = 9){
  
  if(fastP <= 1) {
    stop("fastP must be > 1")
  }
  if(slowP <= 1) {
    stop("slowP must be > 1")
  }
  if(sigP <= 1) {
    stop("sigP must be > 1")
  }
  
  H_fast_EMA <- H_EMA_filter(EMA_Period = fastP) 
  H_slow_EMA <- H_EMA_filter(EMA_Period = slowP) 
  H_sig_EMA  <- H_EMA_filter(EMA_Period = sigP) 
  
  H_MACD   <- list(h = H_fast_EMA$h - H_slow_EMA$h,
                   f = H_fast_EMA$f)
  class(H_MACD)   <- "freqz"
  
  H_signal <- list(h = H_sig_EMA$h * H_MACD$h,
                   f = H_sig_EMA$f)
  class(H_signal) <- "freqz"
  
  return(list(H_MACD = H_MACD, H_Signal = H_signal))
}

H_MACD_1 <- H_MACD_filter()

hPlot(H_MACD_1$H_MACD, type ="l")
hPlot(H_MACD_1$H_Signal, type ="l")


library(manipulate)
manipulate(hPlot(H_MACD_filter(fastP, slowP, sigP)$H_MACD, type ="l"),
           fastP = slider(2, 100, initial = 12),
           slowP = slider(2, 100, initial = 26),
           sigP  = slider(2, 100, initial =  9))
manipulate(hPlot(H_MACD_filter(fastP, slowP, sigP)$H_Signal, type ="l"),
           fastP = slider(2, 100, initial = 12),
           slowP = slider(2, 100, initial = 26),
           sigP  = slider(2, 100, initial =  9))
