library(data.table)
library(lubridate)
library(here)
library(glue)
library(fst)

source(here('src/factor/scale.R'))
source(here('src/factor/ic.R'))
source(here('src/factor/residualise.R'))
source(here('src/risk/covar.R'))

# prepare data ------------------------------------------------------------

# general data
prices <- read_fst(here('src/data/clean/prices_m.fst'), as.data.table=TRUE)
universe <- read_fst(here('src/data/clean/universe_m.fst'), as.data.table=TRUE)
alpha <- read_fst(here('src/data/clean/alpha_m.fst'), as.data.table=TRUE)
controls <- read_fst(here('src/data/clean/controls_m.fst'), as.data.table=TRUE)

# data for covariance estimation
prices_daily <- read_fst(here('src/data/clean/prices_d_RAW.fst'), as.data.table=TRUE)
risk <- read_fst(here('src/data/clean/risk.fst'), as.data.table=TRUE)

# get forward month return
setorder(prices, date)
prices[, id := cumsum(fcoalesce(date - shift(date) >= 45, FALSE)), by = symbol]
prices[, return_1m := shift(close, -1) / close - 1, by = .(symbol, id)]
prices <- na.omit(prices, by=c('return_1m'))

# join on universe
data <- prices[universe, on=.(date, symbol), nomatch = NULL]
data <- alpha[data, on=.(date, symbol)][date >= '2000-06-30' & date <= '2025-03-31']
data <- controls[, .(date, symbol, vol, beta)][data, on=.(date, symbol)]

# fill NA values
data[, vol := fill.na(vol), by = date]
data[, alpha := fill.na(alpha), by = date]

data[, expected_return := alpha * vol * 0.02]
data[, `:=`(id = NULL)]

# create beta 1 portfolio with alpha tilt
rebalance_dates <- unique(data$date)
portfolio_history <- list()

for (month in rebalance_dates) {
  rebalance_data <- data[date == month]
  rebalance_data[, alpha_weight := exp(alpha)]
  
  low_beta <- rebalance_data[beta < 1]
  high_beta <- rebalance_data[beta >= 1]
  
  # solve for alpha tilted beta 1 returns
  low_beta[, w := alpha_weight / sum(alpha_weight)]
  high_beta[, w := alpha_weight / sum(alpha_weight)]
  
  beta_low <- sum(low_beta$w * low_beta$beta)
  beta_high <- sum(high_beta$w * high_beta$beta)
  
  x <- (1 - beta_low) / (beta_high - beta_low)
  low_beta[, final_weight := (1 - x) * w]
  high_beta[, final_weight := x * w]
  
  weights <- rbind(
    low_beta[, .(symbol, weight = final_weight)],
    high_beta[, .(symbol, weight = final_weight)]
  )
  
  # get weights
  ret <- rebalance_data[, .(symbol, return_1m)][weights, on=.(symbol), nomatch=NULL]
  portfolio_history[[length(portfolio_history) + 1]] <- data.table(date = as.Date(month), return = sum(ret$return_1m * ret$weight, na.rm=TRUE))

  cat(glue('Portfolio completed for {as.Date(month)}'), "\n")
}

library(ggplot2)

universe_prices <- prices[universe, on=.(date, symbol), nomatch = NULL]
universe_returns <- universe_prices[, .(benchmark = sum(weight * return_1m, na.rm=TRUE)), by = date]

portfolio_dt <- rbindlist(portfolio_history)
portfolio_dt <- portfolio_dt[universe_returns, on=.(date), nomatch=NULL]
portfolio_dt[, cum_return := cumprod(1 + return)]
portfolio_dt[, cum_benchmark := cumprod(1 + benchmark)]

portfolio_vs_benchmark <- ggplot(portfolio_dt, aes(x = date)) +
  geom_line(aes(y = cum_return)) +
  geom_line(aes(y = cum_benchmark), linetype='dashed') +
  theme_minimal(base_family = 'Courier New') +
  labs(x = '', y = 'Growth of $1') +
  scale_x_date(date_breaks='2 years', date_labels='%Y') +
  scale_y_continuous(breaks=seq(-1, 15, by = 1))

ggsave(here('src/research/output/10-naive-portfolio-vs-benchmark.png'), portfolio_vs_benchmark, width=10.5, height=5.33)
