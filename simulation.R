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
library(reshape2)
library(progress)

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
num_days <- nrow(sp500)
num_sim <- 10
leverages <- c(1, 2, 3)

day_n <- function(N, change, leverage){
  leveraged_change <- 1 + leverage * change[1: N]
  return(prod(leveraged_change))
}

# every day
pb <- progress_bar$new(
  format = "  simulating [:bar] :current/:total (:percent) | eta: :eta",
  total = length(leverages)*num_sim, clear = FALSE, width= 60)

sim <- as.data.frame(NULL)

for(i in 1:num_sim){
  change <- sample(sp500$change, num_days, replace = T)
  chain <- as.data.frame(cbind(i, 1:num_days, change))
  for(L in leverages){
    lev <- sapply(1: num_days, day_n, change = chain$change, leverage = L)
    chain <- cbind(chain, start * lev)
    pb$tick()
  }
  colnames(chain) <- c("chain", "day", "change", "1", "2", "3")
  sim <- rbind(sim, chain)
}
sim$divergence3 <- (sim$`3` - sim$`1`) / sim$`1`
sim$divergence2 <- (sim$`2` - sim$`1`) / sim$`1`

ggplot(sim, aes(x = day, y = divergence3, color = as.factor(chain))) + 
  geom_smooth() + 
  theme_bw()

ggplot(sim, aes(x = day, y = divergence2, color = as.factor(chain))) + 
  geom_smooth() + 
  theme_bw()

write.csv(sim, "simulated_data.csv", quote = F, row.names = F)

# Is the divergence proportional to the average change?
# Or is the divergence proportional to the average sign(change)?


sim <- melt(sim, measure.vars = c("1", "2", "3"), variable.name = "leverage")

ggplot(sim, aes(x = day, y = value, color = as.factor(chain))) + 
  geom_smooth() + 
  facet_wrap(~leverage) +
  theme_bw() +
  scale_y_log10()

ggplot(sim, aes(x = day, y = value, color = as.factor(chain))) + 
  #geom_smooth() + 
  geom_line() +
  facet_wrap(~leverage) +
  theme_bw() +
  scale_y_log10()


# only beginning-end change
num_sim <- 1e5
pb <- progress_bar$new(
  format = "  simulating [:bar] :current/:total (:percent) | eta: :eta",
  total = length(leverages)*num_sim, clear = FALSE, width= 60)

sim <- as.data.frame(NULL)

for(i in 1:num_sim){
  change <- sample(sp500$change, num_days, replace = T)
  chain <- as.data.frame(i)
  for(L in leverages){
    lev <- day_n(num_days, change, L)
    chain <- cbind(chain, start * lev)
    pb$tick()
  }
  colnames(chain) <- c("chain", "1", "2", "3")
  sim <- rbind(sim, chain)
}

sim$divergence3 <- (sim$`3` - sim$`1`) / sim$`1`
sim$divergence2 <- (sim$`2` - sim$`1`) / sim$`1`
write.csv(sim, "simulated_endpoints.csv", quote = F, row.names = F)



ggplot(sim, aes(y=divergence2)) + 
  geom_boxplot(outlier.shape = NA) +
  theme_bw()

ggplot(sim, aes(y=divergence3)) + 
  geom_boxplot(outlier.shape = NA) +
  theme_bw()

quantile(sim$divergence2)
quantile(sim$divergence3)
mean(sim$divergence2 > 0)
mean(sim$divergence3 > 0)
mean(sim$`2` < sim$`3`)

# it seems to be a safer bet to go for 2x leverage, rather than 3x leverage
# best strategy is probably a mix of both for a high-risk portfolio

# check these probabilities for different time lengths?