library(data.table)

risk_data <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
temp_zip <- tempfile(fileext = ".zip")
download.file(risk_data, destfile = temp_zip, mode = "wb")
unzip(temp_zip, exdir = tempdir())

file <- file.path(tempdir(), "F-F_Research_Data_Factors_daily.CSV")
ff <- as.data.table(read.csv(file, skip = 3))[1:nrow(ff)-1]

# clean up columns
setnames(ff, c('date', 'mkt', 'smb', 'hml', 'rf'))
ff[, date := as.Date(date, format='%Y%m%d')]

factors <- c('mkt', 'smb', 'hml', 'rf')
ff[, (factors) := lapply(.SD, function(x) x / 100), .SDcols=factors]

write_fst(ff, here('src/data/clean/risk.fst'))
