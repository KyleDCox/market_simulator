derivative <- function(SERIES){
  total <- length(SERIES)
  d <- rep(NA, total)
  
  for(i in 2: total){
    d[i] <- SERIES[i] - SERIES[i-1]
  }
  return(d)
}
