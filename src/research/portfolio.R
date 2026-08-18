library(data.table)
library(lubridate)
library(Matrix)
library(here)
library(glue)
library(osqp)
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
data <- prices[universe, on=.(date, symbol)]
setnames(data, "weight", "benchmark_weight")
data <- alpha[data, on=.(date, symbol)][date >= '2000-06-30' & date <= '2025-03-31']
data <- controls[, .(date, symbol, vol, beta)][data, on=.(date, symbol)]

# fill NA values
data[, vol := fill.na(vol), by = date]
data[, alpha := fill.na(alpha), by = date]
data[, beta := fill.na(beta), by = date]

data[, expected_return := alpha * (vol / sqrt(12)) * 0.02]
data[, `:=`(id = NULL)]

# create beta 1 portfolio with alpha tilt
rebalance_dates <- unique(data$date)

factors <- c("mkt", "smb", "hml")
lambda <- 20
max_active <- 0.01

portfolio_weights <- lapply(rebalance_dates, function(month) {
  cat(glue("Optimising for {month}..."), "\n")  
  
  # rm has F, B and D
  # F is the factor covariance matrix
  # B is the betas of the stocks to the risk factors
  # D is the idiosyncratic risk
  rm <- factor.risk(
    prices_daily, risk,
    month %m-% months(3), month,
    factors
  )
  
  month_data <- data[date == month]
  
  # stocks we can optimise
  opt <- month_data[
    symbol %in% rownames(rm$B) &
      complete.cases(expected_return, beta, benchmark_weight)
  ]
  
  # everything else is fixed at benchmark
  fixed <- month_data[!symbol %in% opt$symbol]
  
  B <- rm$B[opt$symbol, , drop = FALSE]
  D <- rm$D[opt$symbol]
  mu <- opt$expected_return
  beta <- opt$beta
  benchmark_weight <- opt$benchmark_weight
  
  N <- nrow(B)
  K <- ncol(B)
  
  # need to exclude any benchmark weighted stocks
  weight_target <- 1 - sum(fixed$benchmark_weight)
  beta_target <- 1 - sum(fixed$benchmark_weight * fixed$beta)
  
  # 1/2 t(x) %*% P %*% x + t(q) %*% x
  # x is the weight, P is the covariance matrix, q is the expected return
  # l is the lower bound and u is the upper bound
  model <- osqp(
    P = bdiag(
      lambda * Diagonal(x = D),
      lambda * Matrix(rm$F, sparse = TRUE)
    ),
    q = c(-mu, rep(0, K)),
    A = rbind(
      cbind(Matrix(t(B), sparse = TRUE), -Diagonal(K)),
      cbind(Matrix(1, 1, N, sparse = TRUE), Matrix(0, 1, K, sparse = TRUE)),
      cbind(Matrix(beta, 1, N, sparse = TRUE), Matrix(0, 1, K, sparse = TRUE)),
      cbind(Diagonal(N), Matrix(0, N, K, sparse = TRUE))
    ),
    l = c(
      rep(0, K),
      weight_target,
      beta_target,
      pmax(0, benchmark_weight - max_active)
    ),
    u = c(
      rep(0, K),
      weight_target,
      beta_target,
      benchmark_weight + max_active
    ),
    pars = osqpSettings(verbose = FALSE)
  )
  
  solution <- data.table(
    date = month,
    symbol = opt$symbol,
    weight = model$Solve()$x[seq_len(N)]
  )
  
  solution <- rbind(
    solution,
    fixed[, .(date = month, symbol, weight = benchmark_weight)]
  )
  
  data.table(
    date = month,
    symbol = rownames(B),
    weight = model$Solve()$x[seq_len(N)]
  )
})

portfolio_weights <- rbindlist(portfolio_weights)
portfolio_data <- data[portfolio_weights, on=.(date, symbol), nomatch=NULL]

# assess portfolio performance --------------------------------------------

library(ggplot2)

portfolio_returns <- portfolio_data[, .(return = sum(return_1m  * weight, na.rm=TRUE)), by = date]
portfolio_returns[, cum_return := cumprod(1 + return)]
ggplot(portfolio_returns, aes(x = date, y = cum_return)) +
  geom_line()
