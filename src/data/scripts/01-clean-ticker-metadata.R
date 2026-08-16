#' apply symbology to metadata, and get dommestic common stock

library(data.table)
library(uuid)
library(here)
library(fst)

metadata <- fread(here('src/data/ticker_metadata_RAW.csv'))
metadata <- metadata[category %in% c('Domestic Common Stock', 'Domestic Common Stock Primary Class')]
metadata

#' first, create a unique symbology for each ticker
ticker_set <- unique(metadata$ticker)
used_symbols <- c()
ticker_to_symbol <- list()

for (ticker in ticker_set) {
  symbol <- UUIDgenerate()
  while (symbol %in% used_symbols) symbol <- UUIDgenerate()
  used_symbols <- c(used_symbols, symbol)
  ticker_to_symbol[[length(ticker_to_symbol) + 1]] <- data.table(ticker = ticker, symbol = symbol)
}

symbol_map <- rbindlist(ticker_to_symbol)
write_fst(symbol_map, here('src/data/symbol_map_RAW.fst'))

#' merge on symbols and get required columns
metadata <- metadata[symbol_map, on=.(ticker)]
metadata <- metadata[, .(symbol, sector, industry = famaindustry)]
metadata <- unique(metadata, by=c('symbol'))

write_fst(metadata, here('src/data/clean/metadata.fst'))
