residualise.factors <- function(factors_dt, controls_dt, factors, controls) {
  required_factors <- c("date", "symbol", factors)
  required_controls <- c("date", "symbol", controls)
  
  # check for required columns
  missing_cols <- c(
    setdiff(required_factors, names(factors_dt)),
    setdiff(required_controls, names(controls_dt))
  )
  
  if (length(missing_cols)) {
    stop(
      "Missing required columns: ",
      paste(unique(missing_cols), collapse = ", ")
    )
  }
  
  # merge on controls to factors
  data <- merge(
    factors_dt[, required_factors, with = FALSE],
    controls_dt[, required_controls, with = FALSE],
    by = c("date", "symbol")
  )
  
  data[, {
    X <- as.matrix(.SD[, controls, with = FALSE])

    # no null controls here
    valid_x <- apply(X, 1L, function(x) all(is.finite(x)))
    
    X <- cbind(intercept = 1, X)
    
    residuals <- lapply(factors, function(factor) {
      y <- .SD[[factor]]
      
      valid <- valid_x & is.finite(y)
      
      # ensure any invalid results are still kept but return as NA
      out <- rep(NA_real_, .N)
      
      if (sum(valid) > ncol(X)) {
        out[valid] <- .lm.fit(
          X[valid, , drop = FALSE],
          y[valid]
        )$residuals
      }
      
      out
    })
    
    names(residuals) <- factors
    
    c(
      list(symbol = symbol),
      residuals
    )
  },
  by = date,
  .SDcols = c(factors, controls)]
}