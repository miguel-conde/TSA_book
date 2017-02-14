#' Title
#'
#' @param x 
#' @param nValid 
#' @param ts_start 
#'
#' @return
#' @export
#'
#' @examples
tsDataPartition <- function(x, nValid = frequency(x), ts_start = start(x)) {

  nTrain   <- length(x) - nValid

  # train_ts <- window(x, 
  #                    start = c(ts_start[1], 1), 
  #                    end   = c(ts_start[1], nTrain))
  # valid_ts <- window(x, 
  #                    start = c(ts_start[1], nTrain+1), 
  #                    end   = c(ts_start[1], nTrain+nValid))
  
  train_ts <- window(x, 
                     start = ts_start, 
                     end   = c(ts_start[1], nTrain + (ts_start[2] - 1)))
  valid_ts <- window(x, 
                     start = c(ts_start[1], nTrain + (ts_start[2] - 1) + 1), 
                     end   = c(ts_start[1], nTrain + (ts_start[2] - 1) + nValid))
  
  return(list(train_ts = train_ts, valid_ts = valid_ts))
}

# FUNCTION for k stepsAhead

#' Title
#'
#' @param x 
#' @param fixed_nValid 
#' @param start 
#' @param stepsAhead 
#' @param fcFUN 
#'
#' @return
#' @export
#'
#' @examples
RFP_performance <- function(x, fixed_nValid = frequency(x), 
                            start = stats::start(x), stepsAhead = 1,
                            fcFUN = naive) {
  
  # cat(sprintf("fixed_nValid = %d \nstart[1] = %d, start[2] = %d\n",
  #             fixed_nValid, start[1], start[2]))
  
  fixed_nTrain <- length(x) - fixed_nValid
  
  # Number of stepsAhead forecasts
  numStepsAheadFC <- fixed_nValid - stepsAhead + 1
  
  error         <- rep(0, numStepsAheadFC)
  percent_error <- rep(0, numStepsAheadFC)
  
  for (j in fixed_nTrain:(fixed_nTrain + fixed_nValid - stepsAhead)) {
    train_ts <- window(x,
                       start = start,
                       end   = c(start[1], j))
    valid_ts <- window(x,
                       start = c(start[1], j + stepsAhead),
                       end   = c(start[1], j + stepsAhead))
    
    fc <- fcFUN(train_ts, h = stepsAhead)
    
    error[j - fixed_nTrain + 1] <- valid_ts - fc$mean[stepsAhead]
    percent_error[j - fixed_nTrain + 1] <- error[j - fixed_nTrain + 1] / valid_ts
  }
  
  return(c(MAE  = mean(abs(error)), 
           RMSE = sqrt(mean(error^2)),
           MAPE = mean(abs(percent_error))))
}

# Test it
RFP_performance(ridership_ts, fixed_nValid = 36, 
                start = c(1991, 1), stepsAhead = 1,
                fcFUN = naive)
RFP_performance(ridership_ts)

#' Title
#'
#' @param x 
#' @param fixed_nValid 
#' @param start 
#' @param fcFUN 
#' @param stat 
#'
#' @return
#' @export
#'
#' @examples
kStepsPerf <- function(x, fixed_nValid = frequency(x), 
                       start = stats::start(x), 
                       fcFUN = naive, stat = mean) {
  fc_perf <- 
    t(sapply(1:fixed_nValid, function(n) {
      RFP_performance(x, fixed_nValid, 
                      start, stepsAhead = n,
                      fcFUN)
    }))
  
  out <- rbind(fc_perf, apply(fc_perf, 2, stat))
  
  row.names(out) <- c(1:fixed_nValid, deparse(substitute(stat)))
  
  out
}

kStepsPerf(ridership_ts)
kStepsPerf(ridership_ts, fixed_nValid = 36)
kStepsPerf(ridership_ts, stat = sd)

#' Title
#'
#' @param x 
#' @param initTrainingPeriod 
#'
#' @return
#' @export
#'
#' @examples
rfPartition <- function(x, initTrainingPeriod) {
  
  if(!R.utils::isPackageLoaded("caret")) {
    if(!require(caret))
      stop("Package 'caret' not found")
  }
  
  partList <- list()

  for(trainingPeriod in initTrainingPeriod:(length(x) - 1)) {
    tps <- createTimeSlices(x,
                            initialWindow = trainingPeriod, 
                            horizon       = length(x) - trainingPeriod,
                            fixedWindow   = TRUE)
    item       <- list()
    item$train <- tps$train[[1]]
    item$test  <- tps$test[[1]]
    
    partList[[paste0("Partition_", 
                     trainingPeriod - initTrainingPeriod + 1)]] <- item
  }
  
  lapply(partList, function(n) {
    list(train = window(x, 
                        start = head(time(ridership_ts)[n[[1]]], 1),
                        end   = tail(time(ridership_ts)[n[[1]]], 1)),
         test  = window(x, 
                        start = head(time(ridership_ts)[n[[2]]], 1),
                        end   = tail(time(ridership_ts)[n[[2]]], 1)))
  })
}

rfPartition(ridership_ts, initTrainingPeriod = 123)


