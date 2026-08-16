library(data.table)
library(ggplot2)
library(here)
library(fst)

# pull in factor utils
source(here('src/factor/scale.R'))
source(here('src/factor/ic.R'))
source(here('src/factor/residualise.R'))

prices <- read_fst(here('src/data/clean/prices_m.fst'), as.data.table=TRUE)
fundamentals <- read_fst(here('src/data/clean/fundamentals_m.fst'), as.data.table=TRUE)
universe <- read_fst(here('src/data/clean/universe_m.fst'), as.data.table=TRUE)

# get forward returns
setorder(prices, date)
prices[, id := cumsum(fcoalesce(date - shift(date) >= 45, FALSE)), by = symbol]

prices[, return_1m := shift(close, -1) / close - 1, by = .(symbol, id)]
prices[, return_3m := shift(close, -3) / close - 1, by = .(symbol, id)]


# initial factor evaluation -----------------------------------------------

# winsorise and scale raw factors first
factors <- c('ebitdaev', 'ep', 'sp', 'bp')

fundamentals[, (factors) := lapply(.SD, winsorise), .SDcols = factors]
fundamentals[, (factors) := lapply(.SD, rank.scale), .SDcols = factors]

data_01 <- prices[fundamentals, on=.(date, symbol)]
data_01 <- data_01[universe, on=.(date, symbol), nomatch = NULL]
data_01 <- na.omit(data_01)

# loop over factors and return a list of data.tables
result <- ic.backtest(
  data_01,
  c("return_1m", "return_3m"),
  factors
)

setorder(result$ic, date, factor)
result$ic[, rolling_ic := frollmean(ic, 12), by = .(factor, horizon)]

ic_raw <- ggplot(result$ic, aes(x = date, y = ic, fill = factor, color = factor)) +
  geom_col(position='dodge', alpha = 0.35, linewidth=0) +
  geom_line(aes(y = rolling_ic)) +
  theme_minimal(base_size=13, base_family='Courier New') +
  labs(x = '', y = 'IC') +
  facet_wrap(~horizon, ncol=1) +
  scale_x_date(date_breaks="4 years", date_labels="%Y")

ggsave(here('src/research/output/01-ic-raw.png'), ic_raw, width=10.5, height=5.33)

print(result$tstat)

# consider the cross-sectional correlation of these factors
corr_hist <- cross.sectional.correlation(data_01, factors)

ic_corr <- ggplot(corr_hist, aes(x = mean_corr)) +
  geom_histogram() +
  theme_minimal(base_size = 13, base_family='Courier New') +
  labs(x = 'Mean pairwise correlation', y='')

ggsave(here('src/research/output/03-ic-corr.png'), ic_corr, width=10.5, height=5.33)

# does controlling for sector or industry improve ic? ---------------------

data_02 <- prices[fundamentals, on=.(date, symbol)]
data_02 <- data_02[universe, on=.(date, symbol), nomatch = NULL]
metadata <- read_fst(here('src/data/clean/metadata.fst'), as.data.table=TRUE)
data_02 <- metadata[data_02, on=.(symbol)]
data_02 <- na.omit(data_02)

# de-mean by sector
demean <- function(x) {
  x - mean(x, na.rm=TRUE)
}

data_02[, (factors) := lapply(.SD, demean), by = .(date, sector), .SDcols = factors]

result_sector <- ic.backtest(
  na.omit(data_02),
  c("return_1m", "return_3m"),
  factors
)

setorder(result_sector$ic, date, factor)
result_sector$ic[, rolling_ic := frollmean(ic, 12), by = .(factor, horizon)]

print(result_sector$tstat)

# how about by industry?

data_03 <- prices[fundamentals, on=.(date, symbol)]
data_03 <- data_03[universe, on=.(date, symbol), nomatch = NULL]
data_03 <- metadata[data_03, on=.(symbol)]
data_03 <- na.omit(data_03)
data_03[, (factors) := lapply(.SD, demean), by = .(date, industry), .SDcols = factors]

result_industry <- ic.backtest(
  na.omit(data_03),
  c("return_1m", "return_3m"),
  factors
)

print(result_industry$tstat)

# controlling for beta, size and volatility -------------------------------

controls <- read_fst(here('src/data/clean/controls_m.fst'), as.data.table=TRUE)

data_04 <- prices[fundamentals, on=.(date, symbol)]
data_04 <- metadata[data_04, on=.(symbol)]
data_04 <- na.omit(data_04)

# de-mean by sector
data_04[, (factors) := lapply(.SD, demean), by = .(date, sector), .SDcols = factors]

# find residualised factors that have been de-meaned
residual_factors <- residualise.factors(data_04, controls, c('ebitdaev', 'ep', 'bp', 'sp'), c('beta', 'size', 'vol'))
data_04[, `:=` (ebitdaev = NULL, ep = NULL, bp = NULL, sp = NULL)]
data_04 <- data_04[residual_factors, on=.(symbol, date)]
data_04 <- data_04[universe, on=.(symbol, date), nomatch = NULL]
data_04 <- na.omit(data_04)

result_controlled <- ic.backtest(
  data_04,
  c("return_1m", "return_3m"),
  factors
)

print(result_controlled$tstat)

# creating a value composite ----------------------------------------------

"
the value composite is simply an average of the three significant factors identified,
ebitda/ev, e/p and s/p. the factors are de-meaned at a sector level (which improves performance),
and then regressed vs beta, size and volatility to remove bets.
"

data_final <- prices[fundamentals, on=.(date, symbol)]
data_final <- metadata[data_final, on=.(symbol)]
data_final <- data_final[universe, on=.(date, symbol), nomatch=0]

# de-mean at the sector level
data_final[, (factors) := lapply(.SD, demean), by = .(date, sector), .SDcols=factors]

# residualise vs beta, vol and size
residualised_factors <- residualise.factors(data_final, controls, c('ebitdaev', 'ep', 'bp', 'sp'), c('beta', 'vol', 'size'))
data_final[, `:=`(ebitdaev = NULL, ep = NULL, bp = NULL, sp = NULL)]

data_final <- residualised_factors[data_final, on=.(date, symbol)]
data_final <- data_final[date >= '1999-09-30' & date <= '2025-03-31']

# fill any nas (gaps in controls, etc.) with the average value, and composite
data_final[, (factors) := lapply(.SD, fill.na), by = date, .SDcols = factors]
data_final[, value := (ebitdaev + ep + sp) / 3]
data_final[, value := rank.scale(value), by = date]
data_final[, value := normalise(value), by = date]

# consider the performance of the composite
result_composite <- ic.backtest(
  data_final,
  c("return_1m", "return_3m"),
  c("value")
)

ic_1m <- result_composite$ic[horizon == 'return_1m']
ic_1m[, rolling_ic := frollmean(ic, 12)]

ic_value <- ggplot(ic_1m, aes(x = date, y = ic)) +
  geom_col(alpha=0.25, width=10) +
  geom_line(aes(y = rolling_ic), size=1, color='red') +
  labs(x = '', y = 'IC') +
  scale_x_date(date_breaks="2 years", date_labels="%Y") +
  theme_minimal(base_size=12, base_family="Courier New")

ggsave(here('src/research/output/02-ic-value.png'), ic_value, width=10.5, height=5.33)

value <- data_final[, .(date, symbol, value)]
write_fst(value, here('src/data/clean/value_m.fst'))

# rank portfolio for value composite --------------------------------------

library(dplyr)

data_final[, value_bucket := ntile(value, 10), by = date]
top_minus_bottom <- data_final[, .(
  return = mean(return_1m[value_bucket == 10], na.rm=TRUE) - mean(return_1m[value_bucket == 1], na.rm=TRUE)
), by = date]
top_minus_bottom[, cum_return := cumprod(1 + return) - 1]

top_minus_bottom_p <- ggplot(top_minus_bottom, aes(x = date, y = cum_return + 1)) +
  geom_line() +
  theme_minimal(base_size=12, base_family='Courier New') +
  scale_x_date(date_breaks="2 years", date_labels="%Y") +
  scale_y_continuous(breaks=seq(1, 10, by=1)) +
  labs(x = '', y='Growth of $1') 

ggsave(here('src/research/output/04-top-minus-bottom.png'), top_minus_bottom_p, width=10.5, height=5.33)
