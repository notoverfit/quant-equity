winsorise <- function(x, p = 0.01) {
  bounds <- quantile(x, probs = c(p, 1-p), na.rm=TRUE)
  pmax(pmin(x, bounds[2]), bounds[1])
}

rank_scale <- function(x) {
  valid <- !is.na(x)
  out <- rep(NA_real_, length(x))
  
  r <- frank(x[valid], ties.method = "average")
  n <- length(r)
  
  out[valid] <- (r - 0.5) / n
  out
}

normalise <- function(x) {
  qnorm(x)
}
