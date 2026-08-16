library(data.table)
library(ggplot2)
library(here)
library(fst)

source(here('src/factor/scale.R'))
source(here('src/factor/ic.R'))
source(here('src/factor/residualise.R'))

prices <- read_fst(here('src/data/clean/prices_m.fst'), as.data.table=TRUE)
universe <- read_fst(here('src/data/clean/universe_m.fst'))
setorder(prices, date)

# two definitions of momentum - 12m1m, so year's return excluding the most recent month
# and 6m1m, the half year return excluding the most recent month
prices[, id := cumsum(fcoalesce(date - shift(date) > 45, FALSE)), by = symbol]
prices[, mom_12m1m := shift(close, 2) / shift(close, 12) - 1, by = .(symbol, id)]
prices[, mom_6m1m := shift(close, 2) / shift(close, 6) - 1, by = .(symbol, id)]

prices[, return_1m := shift(close, -1) / close - 1, by = .(symbol, id)]
prices[, return_3m := shift(close, -3) / close - 1, by = .(symbol, id)]

# fill na factors with the average value that month
factors <- c('mom_12m1m', 'mom_6m1m')
prices <- prices[universe, on=.(date, symbol), nomatch = NULL]
prices <- prices[, (factors) := lapply(.SD, fill.na), .SDcols=factors, by=date]

prices <- na.omit(prices)

# rank and scale factors
prices[, (factors) := lapply(.SD, winsorise), .SDcols=factors, by=date]
prices[, (factors) := lapply(.SD, rank.scale), .SDcols=factors, by=date]

# initial ic evaluation ---------------------------------------------------

mom_raw <- ic.backtest(
  prices,
  c('return_1m', 'return_3m'),
  c('mom_6m1m', 'mom_12m1m')
)

mom_ic <- mom_raw$ic
setorder(mom_ic, date)
mom_ic[, rolling_ic := frollmean(ic, 12), by = .(factor, horizon)]

mom_raw <- ggplot(mom_ic, aes(x = date, y = ic, color = factor, fill = factor)) +
  geom_col(position='dodge', alpha=0.15, linewidth=0) +
  geom_line(aes(y = rolling_ic), linewidth=1) +
  theme_minimal(base_size=13, base_family='Courier New') +
  facet_wrap(~horizon, ncol=1)

ggsave(here('src/research/output/05-raw-mom-ic.png'), mom_raw, width=10.5, height=5.33)

print(mom_raw$tstat)


# residualise versus controls ---------------------------------------------

controls <- read_fst(here('src/data/clean/controls_m.fst'), as.data.table=TRUE)

resid_mom <- residualise.factors(
  prices, controls, c('mom_12m1m', 'mom_6m1m'), c('beta', 'size', 'vol')
)

resid_mom[, (factors) := lapply(.SD, winsorise), .SDcols=factors, by=date]
resid_mom[, (factors) := lapply(.SD, rank.scale), .SDcols=factors, by=date]
resid_mom[, (factors) := lapply(.SD, normalise), .SDcols=factors, by=date]

prices[, `:=`(mom_12m1m = NULL, mom_6m1m = NULL)]

# merge on new residual momentum signals and fill nas with mean values again
prices <- resid_mom[prices, on=.(date, symbol)]
prices <- prices[, (factors) := lapply(.SD, fill.na), .SDcols=factors, by=date]

mom_residualised <- ic.backtest(
  prices,
  c('return_1m', 'return_3m'),
  c('mom_6m1m', 'mom_12m1m')
)

mom_ic <- mom_residualised$ic
setorder(mom_ic, date)
mom_ic[, rolling_ic := frollmean(ic, 12), by = .(factor, horizon)]

resid_mom <- ggplot(mom_ic, aes(x = date, y = ic, color = factor, fill = factor)) +
  geom_col(position='dodge', alpha=0.15, linewidth=0) +
  geom_line(aes(y = rolling_ic), linewidth=1) +
  theme_minimal(base_size=13, base_family='Courier New') +
  facet_wrap(~horizon, ncol=1)

ggsave(here('src/research/output/06-resid-mom-ic.png'), resid_mom, width=10.5, height=5.33)

print(mom_residualised$tstat)

# create momentum composite, top minus bottom -----------------------------

prices[, mom := (mom_12m1m + mom_6m1m) / 2]
prices[, mom := normalise(rank.scale(mom)), by = date]

mom_composite <- ic.backtest(
  prices,
  c('return_1m', 'return_3m'),
  c('mom')
)

print(mom_composite$tstat)

library(dplyr)

prices[, mom_bucket := ntile(mom, 10), by = date]
top_minus_bottom <- prices[, .(
  return = mean(return_1m[mom_bucket == 10], na.rm=TRUE) - mean(return_1m[mom_bucket == 1], na.rm=TRUE)
), by = date]

ggplot(top_minus_bottom, aes(x = return)) +
  geom_histogram() +
  theme_minimal(base_family='Courier New')

setorder(top_minus_bottom, date)
top_minus_bottom[, cum_return := cumprod(1 + return) - 1]

mom_ls <- ggplot(top_minus_bottom, aes(x = date, y = cum_return + 1)) +
  geom_line() +
  theme_minimal(base_family='Courier New', base_size=12) +
  labs(x = '', y = 'Growth of $1')

ggsave(here('src/research/output/07-mom-top-minus-bottom.png'), mom_ls, width=10.5, height=5.33)

# save down factor --------------------------------------------------------

mom <- prices[, .(date, symbol, mom)]

write_fst(mom, here('src/data/clean/momentum_m.fst'))