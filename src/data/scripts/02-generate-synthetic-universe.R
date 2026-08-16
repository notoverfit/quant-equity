#' the synthetic universe used in this project is the top 2000 companies
#' by market cap (domestic common stock, no ADRs) at the end-of-month in june.

library(data.table)
library(lubridate)
library(uuid)
library(here)
library(fst)

invalid_tickers <- c("APDN")

#' pull in daily fundamentals data
fundamentals <- fread(here('src/data/us_fundamentals_RAW.csv'))
symbol_map <- read_fst(here('src/data/symbol_map_RAW.fst'), as.data.table=TRUE)

#' now get june-end rebalances
fundamentals <- fundamentals[!(ticker %in% invalid_tickers)]
fundamentals <- fundamentals[symbol_map, on=.(ticker)]
market_cap <- fundamentals[, .(symbol, date, marketcap)]

setorder(market_cap, date, symbol)
market_cap <- market_cap[!is.na(date) & !is.na(marketcap)]
market_cap[, date := as.Date(date)]

june_points <- market_cap[month(date) == 6]
june_points[, year := year(date)]

setorder(june_points, date)
last_date_of_month <- june_points[, .SD[.N], by = year][, .(year, last_date_of_month = date)]
june_points <- june_points[last_date_of_month, on=.(year)]
june_points <- june_points[date == last_date_of_month]

constituents <- june_points[order(-marketcap), head(.SD, 2000), by = year][order(date)]
constituents[, weight := marketcap / sum(marketcap), by = date]

#' get monthly constituents
start_date <- min(constituents$date)
end_date <- max(constituents$date)

month_ends <- seq(start_date, end_date, by = "month")
month_ends <- data.table(date = ceiling_date(month_ends, "month") - days(1))
month_ends[, rebal_year := year(date) - (month(date) < 6)]

constituents[, rebal_year := year(date)]
monthly_constituents <- constituents[month_ends, on = "rebal_year", allow.cartesian=TRUE][, .(date = i.date, symbol, weight)]

write_fst(monthly_constituents, here('src/data/clean/universe_m.fst'))

#' get daily constituents
day_ends <- data.table(date = seq(start_date, end_date, by = "day"))
day_ends[, rebal_year := year(date) - (month(date) < 6)]

daily_constituents <- constituents[day_ends, on = "rebal_year", allow.cartesian=TRUE][, .(date = i.date, symbol, weight)]
write_fst(daily_constituents, here('src/data/clean/universe_d_RAW.fst'))
