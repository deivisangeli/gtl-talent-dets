###############################################################################
# Clean AMWS edition 1 (1906) birthplaces.
#
# Input: input/amws_1906.xlsx  -- 4,109 rows, 10 cols, no header (first row is
# data). Cols (positional):
#   1 lineid, 2 amsname, 3 address_legacy, 4 field_legacy,
#   5 birthplace_blob ("Bregenz, Austria, March 6, 47"),
#   6 biography_blob,
#   7 city_raw ("Bregenz"),
#   8 state_or_country_raw ("Austria"; "Ont, Canada" forms split on comma),
#   9 month_day_raw ("March 6"),
#  10 year_yy (47)
#
# Output: output/amws_1906_cleaned.csv
#   (lineid, birthplace_orig, city, state, country, date, birth_year, flag)
#
# Year rule: every 2-digit YY -> 1800 + YY  (edition published 1906; even the
# youngest scientist would be born well before 1900).
###############################################################################

suppressPackageStartupMessages({
  library(readxl)
  library(data.table)
  library(stringr)
})

repo_root <- Sys.getenv("GTL_REPO", unset = "")
if (!nzchar(repo_root)) {
  stop("GTL_REPO is not set. Set it in .Renviron to the repository root.")
}

source(file.path(repo_root, "prep", "state_alias.R"))
source(file.path(repo_root, "paths.R"))

raw <- as.data.table(read_excel(file.path(AMWS_INPUT, "amws_1906.xlsx"),
                                sheet = 1, col_names = FALSE))
setnames(raw, paste0("c", 1:10))

d <- raw[, .(
  lineid           = as.integer(c1),
  amsname          = c2,
  address_legacy   = c3,
  field_legacy     = c4,
  birthplace_orig  = c5,
  biography_blob   = c6,
  city_raw         = c7,
  st_country_raw   = c8,
  month_day_raw    = c9,
  year_yy_raw      = c10
)]

cat("rows read:", nrow(d), "\n")

# ---- City: trim, fix encoding ----------------------------------------------
fix_enc <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "?")
  trimws(gsub("\\s+", " ", x))
}
d[, city := fix_enc(city_raw)]

# ---- State / country split --------------------------------------------------
# col 8 forms seen:
#   "NY", "MA", ..., USPS 2-letter (US)
#   "Austria", "England", "Germany", ... (non-US, single token country)
#   "Ont, Canada", "B. C, Canada", "N. S, Canada", "Que" (province), "Can"
parse_state_country <- function(s) {
  if (is.na(s) || !nzchar(trimws(s))) {
    return(list(state = "", country = "", flag = "missing_state"))
  }
  s <- trimws(s)

  # Case 1: comma inside -> "<province>, <country>"
  if (grepl(",", s)) {
    parts <- strsplit(s, ",")[[1]]
    prov <- trimws(parts[1])
    ctry <- trimws(parts[length(parts)])
    # Canadian provinces -> CA-XX style state token kept verbatim; country=Canada
    return(list(state = prov, country = ctry, flag = "ok_subnational"))
  }

  # Case 2: token in USPS list
  usps <- normalize_state(s)
  if (!is.na(usps)) {
    return(list(state = usps, country = "USA", flag = "ok"))
  }

  # Case 3: short Canadian province aliases without country tag
  if (s %in% c("Que", "Ont", "B. C", "B.C", "N. S", "N.S", "N. B", "N.B",
               "Man", "Sask", "Alta", "P. E. I", "Nfld", "Yukon", "Can")) {
    return(list(state = s, country = "Canada", flag = "ok_subnational"))
  }

  # Case 4: foreign country single token
  list(state = "", country = s, flag = "foreign")
}

parsed <- lapply(d$st_country_raw, parse_state_country)
d[, state   := vapply(parsed, `[[`, character(1), "state")]
d[, country := vapply(parsed, `[[`, character(1), "country")]
d[, flag    := vapply(parsed, `[[`, character(1), "flag")]

# ---- Birth year (every YY -> 1800 + YY) ------------------------------------
d[, birth_year := suppressWarnings(as.integer(year_yy_raw))]
d[is.na(birth_year),
  birth_year := suppressWarnings(as.integer(str_match(
    birthplace_orig,
    "(?i)\\b(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)\\.?\\s+\\d{1,2}\\.\\s*(\\d{2})\\b"
  )[, 2]))]
d[!is.na(birth_year), birth_year := 1800L + birth_year]

# ---- Date string (Month DD, YY) --------------------------------------------
d[, date := ifelse(!is.na(month_day_raw) & !is.na(year_yy_raw),
                   paste0(month_day_raw, ", ", year_yy_raw), "")]

# ---- Final select ----------------------------------------------------------
out <- d[, .(lineid, birthplace_orig, city, state, country,
             date, birth_year, flag)]

# Drop rows with no lineid (should be 0)
n_no_id <- sum(is.na(out$lineid))
cat("rows missing lineid:", n_no_id, "\n")

fwrite(out, file.path(AMWS_OUTPUT, "amws_1906_cleaned.csv"))
cat("wrote amws_1906_cleaned.csv\n")

cat("\n=== diagnostics ===\n")
cat("total rows:           ", nrow(out), "\n")
cat("with city:            ", sum(nzchar(out$city)), "\n")
cat("with state (US):      ", sum(out$country == "USA" & nzchar(out$state)), "\n")
cat("US rows:              ", sum(out$country == "USA"), "\n")
cat("foreign rows:         ", sum(out$country != "USA" & out$country != ""), "\n")
cat("with birth_year:      ", sum(!is.na(out$birth_year)), "\n\n")

cat("country distribution (top 15):\n")
print(head(out[, .N, by = country][order(-N)], 15))

cat("\nflag distribution:\n")
print(out[, .N, by = flag][order(-N)])
