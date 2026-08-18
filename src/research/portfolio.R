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
lambda <- 25
max_active <- 0.0015

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
  
  rbind(
    solution,
    fixed[, .(date = month, symbol, weight = benchmark_weight)]
  )
})

portfolio_weights <- rbindlist(portfolio_weights)
portfolio_data <- data[portfolio_weights, on=.(date, symbol), nomatch=NULL]

# assess portfolio performance --------------------------------------------

library(ggplot2)
library(patchwork)

benchmark <- data[, .(benchmark = sum(benchmark_weight * return_1m, na.rm=TRUE)), by = date]

portfolio_returns <- portfolio_data[, .(return = sum(return_1m  * weight, na.rm=TRUE)), by = date]
portfolio_returns[, cum_return := cumprod(1 + return)]
portfolio_returns <- benchmark[portfolio_returns, on=.(date)]
portfolio_returns[, cum_benchmark := cumprod(1 + benchmark)]

portfolio_ls <- ggplot(portfolio_returns, aes(x = date, y = cum_return)) +
  geom_line(color = 'red') +
  geom_line(aes(y = cum_benchmark), linetype='dashed') +
  theme_minimal(base_family = 'Courier New') +
  labs(x = '', y = 'Growth of $1') + 
  scale_y_continuous(breaks=seq(1, 10, by = 1))

ggsave(here('src/research/output/10-portfolio-ls.png'), portfolio_ls, width=10.5, height=5.33)

# cumulative active returns
portfolio_returns[, active_return := return - benchmark]
portfolio_returns[, cum_active := cumprod(1 + active_return)]
ggplot(portfolio_returns, aes(x = date, y = cum_active)) +
  geom_line() +
  labs(x = '', y = 'Growth of active $1') +
  theme_minimal(base_family = 'Courier New')

# rolling tracking return
portfolio_returns[, rolling_te := frollsd(active_return * sqrt(12), 12)]
portfolio_returns[, beta := frollapply(1:.N, 36, function(i) cov(return[i], benchmark[i]) / var(benchmark[i]))]
portfolio_returns[, rolling_active := frollprod(1 + active_return, 12) - 1]
tracking_error <- ggplot(portfolio_returns[!is.na(rolling_te)], aes(x = date, y = rolling_te * 100)) +
  geom_line() +
  labs(x = '', y = 'Tracking error (%, rolling 12-month)') +
  theme_minimal(base_family = 'Courier New', base_size=10) +
  scale_x_date(date_breaks='2 years', date_labels='%Y')

beta <- ggplot(portfolio_returns[!is.na(beta)], aes(x = date, y = beta)) +
  geom_ribbon(
    aes(ymin = 1, ymax = pmax(beta, 1)),
    fill = "green", alpha = 0.2
  ) +
  geom_ribbon(
    aes(ymin = pmin(beta, 1), ymax = 1),
    fill = "red", alpha = 0.2
  ) +
  geom_hline(yintercept = 1, linetype='dashed') +
  geom_line() +
  labs(x = "", y = "Rolling beta (36-month)") +
  theme_minimal(base_family = "Courier New", base_size=10) + 
  scale_x_date(date_breaks='2 years', date_labels='%Y')

rolling_active <- ggplot(portfolio_returns[!is.na(rolling_active)], aes(x = date, y = rolling_active * 100)) +
  geom_line() +
  labs(x = '', y = 'Active returns (%, rolling 12-month)') +
  scale_y_continuous(breaks=seq(-10, 20, by = 2.5)) +
  scale_x_date(date_breaks='2 years', date_labels='%Y') +
  theme_minimal(base_family = 'Courier New', base_size = 10)

portfolio_attributes <- tracking_error / beta / rolling_active

ggsave(here('src/research/output/11-portfolio-attributes.png'), portfolio_attributes, width=10.5, height=10)
