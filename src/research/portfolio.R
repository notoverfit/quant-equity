library(data.table)
library(lubridate)
library(quadprog)
library(Matrix)
library(here)
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
data <- controls[, .(date, symbol, vol)][data, on=.(date, symbol)]

# fill NA values
data[, vol := fill.na(vol), by = date]
data[, alpha := fill.na(alpha), by = date]

data[, expected_return := alpha * vol * 0.02]
data[, `:=`(id = NULL)]