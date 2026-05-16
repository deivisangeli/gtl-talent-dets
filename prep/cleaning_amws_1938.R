###############################################################################
# Clean AMWS edition 6 (1938) birthplaces.
#
# Input: input/amws_1938.dta  -- 23,743 rows. Columns:
#   lineid, page, last, address_legacy, field_legacy,
#   a            <- biographical blob (city, state, Month DD, YY. then career)
#   init, AMSname
#
# Output: output/amws_1938_cleaned.csv
#   (lineid, AMSname, birthplace_orig, city, state, country, date, birth_year,
#    has_month, flag)
#
# Year rule: YY <= 25 -> 1900 + YY; YY 26..99 -> 1800 + YY.
# (Edition published 1938; min sane working scientist age ~15 -> 1923 birth.)
###############################################################################

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(stringr)
})

source("state_alias.R")
source("../paths.R")

df <- read_dta(file.path(AMWS_INPUT, "amws_1938.dta"))

# Fix encoding
char_cols <- names(df)[sapply(df, is.character)]
for (col in char_cols) {
  df[[col]] <- iconv(df[[col]], from = "latin1", to = "UTF-8", sub = "?")
}

d <- as.data.table(df)
setnames(d, c("lineid", "page", "last", "address_legacy", "field_legacy",
              "blob", "init", "AMSname"))
cat("rows read:", nrow(d), "\n")

# ---- Pre-clean blob (same logic as 1955 cleaner) ---------------------------
preclean <- function(bp) {
  if (is.na(bp)) return(NA_character_)
  bp <- gsub("(\\S)-\\s+(\\S)", "\\1\\2", bp)   # de-hyphenate line breaks
  bp <- gsub("@", "", bp)                       # stray @
  bp <- gsub("\\s+", " ", bp)
  trimws(bp)
}
d[, blob_clean := vapply(blob, preclean, character(1))]

# ---- Truncate at first degree/career token to isolate place+date prefix ----
# 1938 entries record births as either "City, State, Month DD, YY" OR plain
# "City, State, YY" (no month). Both forms come before the first degree token.
DEGREE_RX <- paste0("\\b(",
  "A\\.?B|B\\.?A|B\\.?S|B\\.?E|M\\.?A|M\\.?S|M\\.?D|D\\.?Sc|",
  "Ph\\.?\\s?D|Sc\\.?D|LL\\.?D|D\\.?Eng|A\\.?M|",
  "Asst|Prof|Instr|Fellow|Research|RESEARCH|PROF|ASST|INSTR|",
  "Lecturer|lecturer|Teacher|teacher",
  ")\\b")
trunc_pos <- regexpr(DEGREE_RX, d$blob_clean, perl = TRUE)
d[, prefix := ifelse(trunc_pos > 0,
                     substr(blob_clean, 1, trunc_pos - 1),
                     blob_clean)]
d[, prefix := trimws(prefix)]
d[, prefix := gsub("[;\\.\\s]+$", "", prefix)]

# ---- Extract date within prefix (month-day-year first, then year-only) -----
MONTHS <- paste0(
  "(January|February|March|April|May|June|July|August|September|October|",
  "November|December|Jan|Feb|Mar|Apr|Aug|Sept|Sep|Oct|Nov|Dec)")
DATE_RX <- paste0("\\b", MONTHS, "\\.?\\s+\\d{1,2}\\s*[,.]?\\s*(\\d{2,4})\\b")

prefix_vec <- d$prefix
m <- regexpr(DATE_RX, prefix_vec, ignore.case = TRUE, perl = TRUE)
date_len <- attr(m, "match.length")
has_month <- m > 0

date_raw  <- rep("", nrow(d))
place_raw <- prefix_vec
date_raw[has_month]  <- substr(prefix_vec[has_month],
                                m[has_month],
                                m[has_month] + date_len[has_month] - 1)
place_raw[has_month] <- trimws(substr(prefix_vec[has_month],
                                       1, m[has_month] - 1))

# Year-only fallback: trailing ", YY" at end of prefix (no month token).
YEAR_ONLY_RX <- ",\\s*(\\d{2,4})\\s*$"
mY <- regexpr(YEAR_ONLY_RX, prefix_vec, perl = TRUE)
yY_len <- attr(mY, "match.length")
has_year_only <- !has_month & mY > 0
date_raw[has_year_only]  <- substr(prefix_vec[has_year_only],
                                    mY[has_year_only],
                                    mY[has_year_only] + yY_len[has_year_only] - 1)
place_raw[has_year_only] <- trimws(substr(prefix_vec[has_year_only],
                                           1, mY[has_year_only] - 1))

# Strip trailing punctuation on place_raw
place_raw <- trimws(gsub("[;,.]\\s*$", "", place_raw))

d[, has_month     := has_month]
d[, has_year_only := has_year_only]
d[, date_raw      := date_raw]
d[, place_raw     := place_raw]

# ---- Year extraction -------------------------------------------------------
yr_int <- suppressWarnings(as.integer(
  regmatches(d$date_raw,
             regexpr("\\d{1,4}\\s*$", d$date_raw))))
has_yr <- regexpr("\\d{1,4}\\s*$", d$date_raw) > 0
yr_full <- rep(NA_integer_, nrow(d))
yr_full[has_yr] <- yr_int

birth_year <- rep(NA_integer_, nrow(d))
k4    <- !is.na(yr_full) & yr_full >= 1000
k1900 <- !is.na(yr_full) & yr_full >= 0  & yr_full <= 25
k1800 <- !is.na(yr_full) & yr_full >= 26 & yr_full <= 99
birth_year[k4]    <- yr_full[k4]
birth_year[k1900] <- 1900L + yr_full[k1900]
birth_year[k1800] <- 1800L + yr_full[k1800]
d[, birth_year := birth_year]

# ---- Split place_raw into (city, state, country) ---------------------------
# Strategy: split on commas. Walk right-to-left until a token normalizes to a
# US state; everything left is the city.
split_place <- function(s) {
  if (is.na(s) || !nzchar(s)) {
    return(list(city = "", state = "", country = "", flag = "missing_place"))
  }
  toks <- trimws(strsplit(s, ",")[[1]])
  toks <- toks[nzchar(toks)]
  if (length(toks) == 0)
    return(list(city = "", state = "", country = "", flag = "missing_place"))

  # Walk from right, find first token that normalizes to USPS
  for (j in seq.int(length(toks), 1)) {
    usps <- normalize_state(toks[j])
    if (!is.na(usps)) {
      city <- paste(toks[seq_len(j - 1)], collapse = ", ")
      return(list(city = trimws(city), state = usps, country = "USA",
                  flag = "ok"))
    }
  }
  # No US state token -> foreign. Country = last token, city = everything else.
  city <- if (length(toks) > 1) paste(toks[-length(toks)], collapse = ", ") else ""
  list(city = trimws(city), state = "",
       country = toks[length(toks)], flag = "foreign")
}

parsed <- lapply(d$place_raw, split_place)
d[, city    := vapply(parsed, `[[`, character(1), "city")]
d[, state   := vapply(parsed, `[[`, character(1), "state")]
d[, country := vapply(parsed, `[[`, character(1), "country")]
d[, flag    := vapply(parsed, `[[`, character(1), "flag")]
d[has_month == FALSE & has_year_only == FALSE & flag == "ok",
  flag := "ok_no_date"]
d[has_month == FALSE & has_year_only == FALSE & flag != "ok",
  flag := "no_date_found"]

# ---- Output ----------------------------------------------------------------
out <- d[, .(lineid, AMSname,
             birthplace_orig = blob,
             city, state, country, date = date_raw,
             birth_year, has_month, flag)]

fwrite(out, file.path(AMWS_OUTPUT, "amws_1938_cleaned.csv"))
cat("wrote amws_1938_cleaned.csv\n")

cat("\n=== diagnostics ===\n")
cat("total rows:           ", nrow(out), "\n")
cat("with city:            ", sum(nzchar(out$city)), "\n")
cat("with state (US):      ", sum(out$country == "USA" & nzchar(out$state)), "\n")
cat("US rows:              ", sum(out$country == "USA"), "\n")
cat("foreign rows:         ", sum(out$country != "USA" & out$country != ""), "\n")
cat("with birth_year:      ", sum(!is.na(out$birth_year)), "\n")
cat("with month token:     ", sum(out$has_month), "\n\n")

cat("flag distribution:\n")
print(out[, .N, by = flag][order(-N)])

cat("\ntop 15 country values (incl. residuals):\n")
print(head(out[, .N, by = country][order(-N)], 15))

cat("\nsample 10 OK rows:\n")
set.seed(1)
ok <- out[flag == "ok"][sample(.N, 10)]
print(ok[, .(birthplace_orig = substr(birthplace_orig, 1, 60),
             city, state, country, date, birth_year)])

cat("\nsample 10 'foreign' rows:\n")
fr <- out[flag == "foreign"]
if (nrow(fr) > 0) {
  set.seed(2)
  print(fr[sample(.N, min(10, .N))][, .(
    birthplace_orig = substr(birthplace_orig, 1, 60),
    city, state, country)])
}

cat("\nsample 10 'missing_place' or 'no_date_found' rows:\n")
bad <- out[flag %in% c("missing_place", "no_date_found")]
if (nrow(bad) > 0) {
  set.seed(3)
  print(bad[sample(.N, min(10, .N))][, .(
    birthplace_orig = substr(birthplace_orig, 1, 80),
    city, state, country, flag)])
}
