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

# the below parameters were tweaked and configured but weren't grid searched
lambda <- 25
max_active <- 0.0015
turnover_penalty <- 0.005

portfolio_weights <- vector("list", length(rebalance_dates))

for (j in seq_along(rebalance_dates)) {
  month <- rebalance_dates[j]
  cat(glue("Optimising for {month}..."), "\n")
  
  # rm contains the risk covariance matrix F, the betas to risk factors B and stock-specific variance D
  rm <- factor.risk(prices_daily, risk, month %m-% months(3), month, factors)
  month_data <- data[date == month]
  
  opt <- month_data[symbol %in% rownames(rm$B) & complete.cases(expected_return, beta, benchmark_weight)]
  fixed <- month_data[!symbol %in% opt$symbol]
  
  B <- rm$B[opt$symbol, , drop=FALSE]
  D <- rm$D[opt$symbol]
  mu <- opt$expected_return
  beta <- opt$beta
  benchmark_weight <- opt$benchmark_weight
  
  N <- nrow(B)
  K <- ncol(B)
  
  weight_target <- 1 - sum(fixed$benchmark_weight)
  beta_target <- 1 - sum(fixed$benchmark_weight * fixed$beta)
  
  if (j == 1) {
    prev_weight <- benchmark_weight
  } else {
    prev <- portfolio_weights[[j - 1]]
    prev_weight <- prev$weight[match(opt$symbol, prev$symbol)]
    prev_weight[is.na(prev_weight)] <- 0
  }

  model <- osqp(
    P = bdiag(lambda * Diagonal(x=D), lambda * Matrix(rm$F, sparse=TRUE), Matrix(0, N, N, sparse=TRUE)),
    # expected return, factor exposure and turnover penalty
    q = c(-mu, rep(0, K), rep(turnover_penalty, N)),
    A = rbind(
      # constrain portfolio factor exposures for covariance t(B) %*% w  - z = 0
      cbind(Matrix(t(B), sparse=TRUE), -Diagonal(K), Matrix(0, K, N, sparse=TRUE)),
      # weight target t(1) %*% = weight_target
      cbind(Matrix(1, 1, N, sparse=TRUE), Matrix(0, 1, K + N, sparse=TRUE)),
      # beta target t(B) %*% w = beta_target
      cbind(Matrix(beta, 1, N, sparse=TRUE), Matrix(0, 1, K + N, sparse=TRUE)),
      # per-stock active exposure target
      cbind(Diagonal(N), Matrix(0, N, K + N, sparse=TRUE)),
      # diagonals to extract w + t and w - t, which constrain the turnover
      cbind(Diagonal(N), Matrix(0, N, K, sparse=TRUE), -Diagonal(N)),
      cbind(-Diagonal(N), Matrix(0, N, K, sparse=TRUE), -Diagonal(N))
    ),
    l = c(rep(0, K), weight_target, beta_target, pmax(0, benchmark_weight - max_active), rep(-Inf, 2 * N)),
    u = c(rep(0, K), weight_target, beta_target, benchmark_weight + max_active, prev_weight, -prev_weight),
    pars = osqpSettings(verbose=FALSE)
  )
  
  result <- model$Solve()
  
  portfolio_weights[[j]] <- rbind(
    data.table(date=month, symbol=opt$symbol, weight=result$x[seq_len(N)]),
    fixed[, .(date=month, symbol, weight=benchmark_weight)]
  )
}

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

# rolling tracking return
portfolio_returns[, rolling_te := frollsd(active_return * sqrt(12), 12)]
portfolio_returns[, beta := frollapply(1:.N, 36, function(i) cov(return[i], benchmark[i]) / var(benchmark[i]))]
portfolio_returns[, rolling_active := frollprod(1 + active_return, 12) - 1]
portfolio_data[, prev_weight := shift(weight), by = symbol]
turnover <- portfolio_data[, .(turnover = 0.5 * sum(abs(weight - prev_weight), na.rm = TRUE)), by = date]
turnover[, rolling_turnover := frollmean(turnover, 12)]

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

turnover_plot <- ggplot(turnover[turnover != 0], aes(x = date, y = rolling_turnover * 100)) +
  geom_line() +
  labs(x = '', y = 'One-way turnover (%)') +
  scale_y_continuous(breaks=seq(0, 100, by = 1)) +
  scale_x_date(date_breaks='2 years', date_labels='%Y') +
  theme_minimal(base_family = 'Courier New', base_size = 10)

portfolio_attributes <- tracking_error / beta / rolling_active / turnover_plot

ggsave(here('src/research/output/11-portfolio-attributes.png'), portfolio_attributes, width=10.5, height=13)
