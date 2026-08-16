library(data.table)
library(here)
library(fst)

source(here('src/factor/scale.R'))
source(here('src/factor/ic.R'))
source(here('src/factor/residualise.R'))

# valid rate for testing
start_date <- '2000-06-30'
end_date <- '2025-03-31'

prices <- read_fst(here('src/data/clean/prices_m.fst'), as.data.table=TRUE)
universe <- read_fst(here('src/data/clean/universe_m.fst'), as.data.table=TRUE)
value <- read_fst(here('src/data/clean/value_m.fst'), as.data.table=TRUE)
momentum <- read_fst(here('src/data/clean/momentum_m.fst'), as.data.table=TRUE)

# merge on value and momentum factors to prices
data <- momentum[value[prices, on=.(date, symbol)], on=.(date, symbol)]
data <- data[date >= start_date]

# get forward month returns
setorder(data, date)
data[, id := cumsum(fcoalesce(date - shift(date) > 45, FALSE)), by = symbol]
data[, return_1m := shift(close, -1) / close - 1, by = .(symbol, id)]
data <- data[date <= end_date]

data <- data[universe, on=.(date, symbol), nomatch=NULL]

# define alpha composite 
data[, alpha := fcoalesce((mom + value) / 2, mom, value)]
data[, alpha := normalise(rank.scale(winsorise(alpha))), by = date]

# get ic for alpha composite
alpha_ic <- data[!is.na(return_1m) & !is.na(alpha), .(ic = cor(alpha, return_1m, method='spearman')), by = date]
alpha_ic[, rolling_ic := frollmean(ic, 12)]

raw_alpha <- ggplot(alpha_ic, aes(x = date)) +
  geom_col(aes(y = ic), alpha=0.15, width=8, linewidth=0) +
  geom_line(aes(y = rolling_ic), size=1) +
  theme_minimal() +
  labs(x = '', y = 'IC') +
  scale_x_date(date_breaks="2 years", date_labels="%Y")

ggsave(here('src/research/output/08-raw-alpha-ic.png'), raw_alpha)

mean(alpha_ic$ic) * sqrt(nrow(alpha_ic)) / sd(alpha_ic$ic)

# alpha top minus-bottom returns ------------------------------------------

library(dplyr)

data <- na.omit(data, by = 'return_1m')

data[, alpha_bucket := ntile(alpha, 10), by = date]
alpha_ls_raw <- data[, .(return = mean(return_1m[alpha_bucket == 10], na.rm=TRUE) - mean(return_1m[alpha_bucket == 1], na.rm=TRUE)), by = date]

alpha_ls_raw[, cum := cumprod(1 + return)]

p_alpha_ls <- ggplot(alpha_ls_raw, aes(x = date, y = cum)) +
  annotate(
    "rect",
    xmin = as.Date("2018-01-01"),
    xmax = as.Date("2020-12-31"),
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.15
  ) +
  geom_line() +
  theme_minimal() +
  labs(x = "", y = "Growth of $1") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

ggsave(here('src/research/output/09-alpha-ls.png'), p_alpha_ls)

# residualise value vs momentum -------------------------------------------

# value is negatively correlated with momentum due to having price in it's divisor. 
# can we get a better composite if we combine value residualised by momentum and momentum?

data[, value_resid := {
  model <- .lm.fit(cbind(1, mom), value)
  model$residuals
}, by = date]

# now we can have independent bets
data[, alpha_v2 := fcoalesce((mom + value_resid) / 2, mom, value_resid)]
data[, alpha_v2 := normalise(rank.scale(winsorise(alpha_v2))), by = date]

alpha_ic_v2 <- data[!is.na(return_1m) & !is.na(alpha_v2), .(ic = cor(alpha_v2, return_1m, method='spearman')), by = date]

print(mean(alpha_ic_v2$ic))
print(mean(alpha_ic$ic))

# clearly not much difference
alpha <- data[, .(date, symbol, alpha = alpha_v2)]
write_fst(alpha, here('src/data/clean/alpha_m.fst'))
