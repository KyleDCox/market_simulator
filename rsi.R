rsi <- function(DAYS, CHANGE){
  
  U <- CHANGE
  U[which(U < 0)] <- 0
  
  D <- -CHANGE
  D[which(D < 0)] <- 0
  
  wilder_smooth <- function(DAYS, SER){
    total <- length(SER)
    out <- rep(NA, total)
    out[DAYS] <- mean(SER[1: DAYS], na.rm = T)
    for(i in (DAYS + 1): total){
      out[i] <- SER[i] / DAYS + out[i - 1] * (DAYS - 1) / DAYS
    }
    return(out)
  }
  
  AU <- wilder_smooth(DAYS, U)
  AD <- wilder_smooth(DAYS, D)
  
  RS = AU / AD
  
  return(100 - (100 / (1 + RS)))
}
