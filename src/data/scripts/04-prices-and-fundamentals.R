#' clean prices and fundamentals

library(data.table)
library(lubridate)
library(here)
library(fst)

prices <- fread(here('src/data/us_prices_RAW.csv'))
fundamentals <- fread(here('src/data/us_fundamentals_RAW.csv'))
constituents <- read_fst(here('src/data/clean/universe_m.fst'), as.data.table=TRUE)
symbol_map <- read_fst(here('src/data/symbol_map_RAW.fst'), as.data.table=TRUE)

#' initial clean-up of columns, merge on symbols, reduce to universe
prices[, date := as.Date(date)]
fundamentals[, date := as.Date(date)]
prices <- prices[symbol_map, on=.(ticker), nomatch = NULL]
fundamentals <- fundamentals[symbol_map, on=.(ticker), nomatch = NULL]

start_date <- '1999-06-30'
end_date <- '2025-06-30'

#' filter by dates and inverse fundamental ratios
prices <- prices[date >= start_date & date <= end_date]
fundamentals <- fundamentals[date >= start_date & date <= end_date]

setorder(prices, date)
setorder(fundamentals, date)

prices[, month := ceiling_date(date, "months") - days(1)]
fundamentals[, month := ceiling_date(date, "months") - days(1)]
prices <- prices[symbol %in% constituents$symbol]
fundamentals <- fundamentals[symbol %in% constituents$symbol]

monthly_prices <- prices[, .SD[.N], by = .(symbol, month)][, .(date = month, symbol, close = closeadj, volume)]
monthly_fundamentals <- fundamentals[, .SD[.N], by = .(symbol, month)][, .(date = month, symbol, ebitdaev = 1 / evebitda, ep = 1 / pe, sp = 1 / ps, bp = 1 / pb)]

#' get infinity and na fundamental factors, cast them to average value for the month
factors <- c('ebitdaev', 'ep', 'sp', 'bp')

for (factor in factors) {
  monthly_fundamentals[, (factor) := {
      x <- get(factor)
      x[!is.finite(x)] <- mean(x[is.finite(x)], na.rm = TRUE)
      x
    }, by = date
  ]
}

#' save down
write_fst(monthly_prices, here('src/data/clean/prices_m.fst'))
write_fst(monthly_fundamentals, here('src/data/clean/fundamentals_m.fst'))
