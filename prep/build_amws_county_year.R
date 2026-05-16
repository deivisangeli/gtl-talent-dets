###############################################################################
# Aggregate AMWS 1955 births to a county-year panel (n_amws per geoid x year).
#
# Inputs
#   output/amws_1955_us_geocoded_final.csv  (lineid -> geoid)
#   output/amws_1955_cleaned.csv           (lineid -> date string)
#
# Output
#   output/amws_1955_county_year.csv  (GEOID, year, n_amws)
#
# Birth-year parsing: 1-4 digit number at end of `date` string.
#   - 4 digit -> as is
#   - 0..40   -> 1900 + yy
#   - 41..99  -> 1800 + yy
# A row is only counted as a real birth year if `date` contains a month token
# (Jan..Dec). Without a month, the trailing number is almost always a degree
# year, not a birth year (spot-checked May 2026). ~150 such rows are dropped.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

geo <- fread("output/amws_1955_us_geocoded_final.csv",
             select = c("lineid", "geoid"))
cln <- fread("output/amws_1955_cleaned.csv",
             select = c("lineid", "date"))

d <- merge(geo, cln, by = "lineid", all.x = TRUE)

# Parse trailing year token, anchored to end-of-string
yr_raw <- suppressWarnings(as.integer(
  regmatches(d$date,
             regexpr("[0-9]{1,4}\\s*$", d$date)) |>
    (\(x) sub("\\s+$", "", x))()
))
# regmatches drops non-matching rows -> align back to length(d$date)
has_tok <- regexpr("[0-9]{1,4}\\s*$", d$date) > 0
yr_full <- rep(NA_integer_, nrow(d))
yr_full[has_tok] <- yr_raw

birth_year <- rep(NA_integer_, nrow(d))
birth_year[!is.na(yr_full) & yr_full >= 1000] <- yr_full[!is.na(yr_full) & yr_full >= 1000]
k19 <- !is.na(yr_full) & yr_full >= 0  & yr_full <= 40
k18 <- !is.na(yr_full) & yr_full >= 41 & yr_full <= 99
birth_year[k19] <- 1900L + yr_full[k19]
birth_year[k18] <- 1800L + yr_full[k18]

has_month <- grepl("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec",
                   d$date, ignore.case = TRUE)

d[, `:=`(birth_year = birth_year, has_month = has_month)]

drop_no_month <- sum(!is.na(d$birth_year) & !d$has_month)
cat(sprintf("dropping %d rows without a month token (degree-year leakage)\n",
            drop_no_month))

panel <- d[!is.na(birth_year) & has_month, .N,
           by = .(GEOID = as.integer(geoid), year = birth_year)]
setnames(panel, "N", "n_amws")
setorder(panel, GEOID, year)

cat(sprintf("Aggregated %d county-year rows, %d counties, years %d-%d, total births %d\n",
            nrow(panel),
            uniqueN(panel$GEOID),
            min(panel$year), max(panel$year),
            sum(panel$n_amws)))

fwrite(panel, "output/amws_1955_county_year.csv")
cat("wrote output/amws_1955_county_year.csv\n")
