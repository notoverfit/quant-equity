#' we calculate daily series of log market cap, volatility and beta

library(data.table)
library(lubridate)
library(here)
library(fst)

prices <- fread(here('src/data/us_prices_RAW.csv'))
fundamentals <- fread(here('src/data/us_fundamentals_RAW.csv'))
symbol_map <- read_fst(here('src/data/symbol_map_RAW.fst'), as.data.table=TRUE)
constituents <- read_fst(here('src/data/clean/universe_d_RAW.fst'), as.data.table=TRUE)

#' merge on symbols, and get market cap for size residualisation
prices <- prices[symbol_map, on=.(ticker), nomatch = NULL]
fundamentals <- fundamentals[symbol_map, on=.(ticker), nomatch = NULL]

prices <- prices[, .(symbol, date, close = closeadj)]
fundamentals <- fundamentals[, .(symbol, date, size = log(marketcap))]

#' calculate volatility and market beta
setorder(prices, date)
setorder(fundamentals, date)
prices[, id := cumsum(fcoalesce(date - shift(date) > 7, FALSE)), by = symbol]
prices[, log_ret := log(close / shift(close)), by = .(symbol, id)]
prices[, simp_ret := close / shift(close) - 1, by = .(symbol, id)]

constituent_prices <- prices[constituents, on=.(date, symbol), nomatch = NULL]
benchmark_returns <- constituent_prices[, .(
  universe = weighted.mean(simp_ret, weight, na.rm=TRUE)
), by = date]

#' merge benchmark returns onto prices
prices_weak <- prices[symbol %in% constituents$symbol]
setorder(prices_weak, date)

prices_weak <- prices_weak[benchmark_returns, on=.(date)]
prices_weak[, vol := frollsd(log_ret, 20, align="right") * sqrt(252), by = .(symbol, id)]
prices_weak[, beta := {
  e_market_stock = frollmean(simp_ret * universe, 60)
  e_market = frollmean(universe, 60)
  e_stock = frollmean(simp_ret, 60)
  e_market2 = frollmean(universe^2, 60)
  
  (e_market_stock - e_market * e_stock) / (e_market2 - e_market^2)
}, by = .(symbol, id)]

#' get month-end residual factors
prices_weak[, `:=`(
  vol = shift(vol),
  beta = shift(beta)
), by = .(symbol, id)]


prices_weak <- prices_weak[fundamentals, on=.(date, symbol)]
prices_weak <- na.omit(prices_weak, by = c('vol', 'beta'))
prices_weak[, month := ceiling_date(date, "months") - days(1)]
prices_weak <- prices_weak[, .SD[.N], by = .(symbol, month)][, .(symbol, date = month, vol, beta, size)]

#' save down monthly residual factors
write_fst(prices_weak, here('src/data/clean/controls_m.fst'))
