# ic history
ic.backtest <- function(dt, return_cols, factor_cols) {
  required_cols <- c("date", return_cols, factor_cols)
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols)) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  ic <- rbindlist(lapply(factor_cols, function(f) {
    rbindlist(lapply(return_cols, function(r) {
      dt[, .(
        factor = f,
        horizon = r,
        ic = cor(get(f), get(r), method = "spearman", use = "complete.obs")
      ), by = date]
    }))
  }))
  
  tstat <- ic[, .(
    mean_ic = mean(ic, na.rm = TRUE),
    t_stat = mean(ic, na.rm = TRUE) /
      (sd(ic, na.rm = TRUE) / sqrt(sum(!is.na(ic))))
  ), by = .(factor, horizon)]
  
  list(ic = ic, tstat = tstat)
}

# pairwise cross-sectional correlation
cross.sectional.correlation <- function(dt, factor_cols) {
  missing_cols <- setdiff(c("date", factor_cols), names(dt))
  
  if (length(missing_cols))
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  
  dt[, {
    x <- cor(.SD, method = "spearman", use = "pairwise.complete.obs")
    .(mean_corr = mean(x[upper.tri(x)], na.rm = TRUE))
  }, by = date, .SDcols = factor_cols]
}
