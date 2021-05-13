# this is a monte carlo simulation for leveraged ETF investing
# question: ARE leveraged ETFs a good long-term play
# Leveraged ETFs offer *daily* leveraged performance relative to their baselines
# This means they get hit harder on bearish runs, but outperform the market in bull runs
# The path is more important than the destination (compounding value)
# Because they are leveraged, they are more sensitive to the effects of compounding

sp500 <- read.csv("HistoricalPrices.csv")
sp500 <- janitor::clean_names(sp500)

sp500$date <- lubridate::mdy(sp500$date)

prev_day <- c(sp500$close[2:nrow(sp500)], 1)

sp500$change <- (sp500$close - prev_day) / prev_day
sp500 <- sp500[1:(nrow(sp500) - 1), ]
sp500$intraday_change <- (sp500$high - sp500$low) / sp500$high

library(ggplot2)
library(viridis)

# plot past 30 years of S&P 500
ggplot(sp500, aes(x=date, y=close)) + 
  geom_line() +
  theme_bw()

ggplot(sp500, aes(x=date, y=change)) + 
  geom_col() +
  theme_bw()

ggplot(sp500, aes(x=date, y=intraday_change*sign(change))) + 
  geom_col() +
  theme_bw()

ggplot(sp500, aes(x=change, y=intraday_change*sign(change))) + 
  geom_point(alpha=0.1) +
  theme_bw()

ggplot(sp500, aes(x=change)) + 
  geom_freqpoly() +
  theme_bw()

ggplot(sp500, aes(x=intraday_change)) + 
  geom_freqpoly() +
  theme_bw()


### Simulation

start <- 100
num_it <- nrow(sp500)
num_sim <- 10
leverages <- c(1, 2, 3)

sim <- as.data.frame(NULL)
for(i in 1:num_sim){
  price <- start
  for(j in 1:num_it){
    for(k in leverages){
      sim <- rbind(sim, c(price, j, i, k))
      price <- price*(1+k*sample(sp500$change, 1))
    }
  }
}

colnames(sim) <- c("price", "time", "chain", "leverage")

ggplot(sim, aes(x = time, y = price, color = as.factor(chain))) + 
  geom_line() + 
  theme_bw()

