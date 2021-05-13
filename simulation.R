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
  geom_density() +
  theme_bw()

ggplot(sp500, aes(x=intraday_change)) + 
  geom_density() +
  theme_bw()

# It is unlikely to see any drop larger than 10% day-to-day
# It is unlikely to see an intra-day drop larger than 10%
# Setting a trailing stop loss to 10% could safely allow for normal fluctuation
# This would cut losses only during a crash or prolonged bear market
# However, with a stop loss, it can be difficult to know when to re-enter the market

# Note, this trailing stop loss will need to be multiplied by the leverage for leveraged ETFs
# e.g. for TQQQ (which has a 3x leverage),
# if you want to persist until the market declines 10%,
# then set the trailing stop loss to 30%

# Because re-entering the market is challenging, I do not add stop losses to the model
# Additionally, the model assumes daily changes are independent
# In reality, the change in one day may affect the change in the next
# Test this with a linear regression

### Simulation
start <- 100
num_it <- nrow(sp500)
num_sim <- 20
leverages <- c(1, 2, 3)

library(progress)
pb <- progress_bar$new(
  format = "  simulating [:bar] :current/:total (:percent) | eta: :eta",
  total = num_it*num_sim, clear = FALSE, width= 60)

sim <- as.data.frame(NULL)
for(i in 1:num_sim){
  prices <- rep(start, length(leverages))
  change <- 0
  for(j in 1:num_it){
    sim <- rbind(sim, c(prices, j, i, change))
    change <- sample(sp500$change, 1)
    prices <- prices*(1+leverages*change)
    pb$tick()
  }
}

colnames(sim) <- c("1", "2", "3", "day", "chain", "change")

sim$divergence <- (sim$`3` - sim$`1`) / sim$`1`

ggplot(sim, aes(x = day, y = divergence, color = as.factor(chain))) + 
  geom_smooth() + 
  theme_bw()

write.csv(sim, "simulated_data.csv", quote = F, row.names = F)

# Is the divergence proportional to the average change?
# Or is the divergence proportional to the average sign(change)?


library(reshape2)
sim <- melt(sim, measure.vars = c("1", "2", "3"), variable.name = "leverage")

ggplot(sim, aes(x = day, y = value, color = as.factor(chain))) + 
  geom_smooth() + 
  facet_wrap(~leverage) +
  theme_bw() +
  scale_y_log10()

ggplot(sim, aes(x = day, y = value, color = as.factor(chain))) + 
  geom_smooth() + 
  facet_wrap(~leverage) +
  theme_bw() +
  scale_y_log10()
