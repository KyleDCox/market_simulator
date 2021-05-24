moving_average <- function(DAYS, SERIES){
  total <- length(SERIES)
  ma <- rep(NA, total)
  ma_len <- DAYS - 1
  for(i in DAYS: total){
    ma[i] <- mean(SERIES[i: (i - ma_len)], na.rm = T)
  }
  return(ma)
}

ema <- function(DAYS, SERIES){
  total <- length(SERIES)
  SERIES_SHORT <- SERIES[1: (total - 1)]
  sma <- moving_average(DAYS, SERIES)[1: (total - 1)]
  weight <- 2 / (DAYS + 1)
  return((SERIES_SHORT - sma) * weight + sma)
}

macd <- function(FAST, SLOW, SIGNAL, SERIES){
  ema_fast <- ema(FAST, SERIES)
  ema_slow <- ema(SLOW, SERIES)
  MACD <- ema_slow - ema_fast
  MACD_signal <- ema(SIGNAL, MACD)
  
  total <- length(SERIES)
  
  buffer_NAs <- function(TOT, SER){c(rep(NA, TOT - length(SER)), SER)}
  
  MACD <- buffer_NAs(total, MACD)
  MACD_signal <- buffer_NAs(total, MACD_signal)
  
  output <- as.data.frame(cbind(MACD, MACD_signal))
  colnames(output) <- c("macd", "macd_signal")
  return(output)
  
}
