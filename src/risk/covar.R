factor.risk <- function(return_dt, risk_dt, start_date, end_date, factors) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  missing <- c(
    setdiff(c("date", "symbol", "return"), names(return_dt)),
    setdiff(c("date", factors), names(risk_dt))
  )
  
  if (length(missing)) {
    stop("Missing required columns: ",
         paste(unique(missing), collapse = ", "))
  }
  
  # restrict estimation window
  returns <- return_dt[date >= start_date & date <= end_date]
  risk    <- risk_dt[date >= start_date & date <= end_date]
  
  data <- returns[risk, on = "date", nomatch = NULL]
  
  # factor covariance matrix
  F <- cov(
    risk[, ..factors],
    use = "complete.obs"
  )
  
  # estimate stock factor loadings + idiosyncratic variance
  estimates <- data[, {
    X <- cbind(1, as.matrix(.SD))
    y <- return
    
    ok <- complete.cases(X, y)
    X <- X[ok, , drop = FALSE]
    y <- y[ok]
    
    if (length(y) <= ncol(X)) {
      list(
        beta = list(rep(NA_real_, length(factors))),
        resid_var = NA_real_
      )
    } else {
      fit <- .lm.fit(X, y)
      
      list(
        beta = list(fit$coefficients[-1]),
        resid_var = sum(fit$residuals^2) /
          (length(y) - fit$rank)
      )
    }
  }, by = symbol, .SDcols = factors]
  
  estimates <- estimates[!is.na(resid_var)]
  
  if (!nrow(estimates)) {
    stop("No stocks had enough valid observations.")
  }
  
  # B: N x K factor exposures
  B <- do.call(rbind, estimates$beta)
  dimnames(B) <- list(estimates$symbol, factors)
  
  # D is diagonal, so only store its diagonal
  D <- pmax(estimates$resid_var, 1e-8)
  names(D) <- estimates$symbol
  
  list(
    B = B,
    F = F,
    D = D
  )
}

covar.mat <- function(return_dt, risk_dt, start_date, end_date, factors) {
  
  model <- factor.risk(
    return_dt,
    risk_dt,
    start_date,
    end_date,
    factors
  )
  
  Sigma <- model$B %*% model$F %*% t(model$B) +
    diag(model$D)
  
  Sigma <- (Sigma + t(Sigma)) / 2
  
  dimnames(Sigma) <- list(
    rownames(model$B),
    rownames(model$B)
  )
  
  Sigma
}