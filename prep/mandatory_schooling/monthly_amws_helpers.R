suppressPackageStartupMessages({library(data.table); library(stringr)})

monthly_geoid <- function(x) {
  n <- suppressWarnings(as.integer(x))
  out <- rep(NA_character_, length(n))
  ok <- !is.na(n) & n >= 1000L & n <= 56999L
  out[ok] <- sprintf('%05d', n[ok])
  out
}
monthly_kept <- function(x) as.character(x) %in% c('TRUE', 'True', 'true', '1')
monthly_contaminated <- function(text) {
  text[is.na(text)] <- ''
  see <- as.integer(regexpr('\\bsee\\s+prev(?:ious)?(?:\\s+edition)?\\b', text, ignore.case=TRUE, perl=TRUE))
  birth <- as.integer(regexpr('(^|[[:space:],.;])b[[:space:]]+', text, ignore.case=TRUE, perl=TRUE))
  see > 0L & (birth < 0L | birth > see)
}

# Require an explicit named month, day and year in the birth segment.
# Two-digit years are checked against the canonical year, never guessed.
parse_amws_month <- function(text, year, early = FALSE) {
  text <- as.character(text)
  if (!length(text)) return(data.table(month=integer(),month_status=character(),birth_segment=character()))
  text[is.na(text)] <- ''
  if (early) text <- str_split_fixed(text,
    regex(';|\\b(?:[ABMD]\\s*\\.?\\s*[ABS]\\b|Ph\\s*\\.?\\s*D\\b|m\\s*\\.\\s*[0-9]|c\\s*\\.\\s*[0-9])', ignore_case=TRUE), 2)[,1]
  month_pat <- '(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:t(?:ember)?)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)'
  pattern <- paste0('(?i)\\b(', month_pat, ')[.,]?\\s*([0-9]{1,2})\\s*(?:[.,/ -]|\\s)\\s*([0-9]{4}|[0-9]{2})(?![0-9A-Za-z])')
  matches <- str_match_all(text, pattern)
  rbindlist(lapply(seq_along(text), function(i) {
    m <- matches[[i]]
    # Cape May is a place name, whereas two explicit month/day strings are ambiguous.
    tokens <- str_count(text[i], regex(paste0('\\b',month_pat,'[.,]?\\s*[0-9]'),ignore_case=TRUE))
    status <- if (!nrow(m)) 'missing_or_unparseable_date' else if (nrow(m) != 1L || tokens > 1L) 'ambiguous_dates' else 'assigned'
    month <- NA_integer_
    if (status == 'assigned') {
      month <- match(tolower(substr(m[1,2],1,3)), tolower(month.abb))
      y <- as.integer(m[1,4]); day <- as.integer(m[1,3])
      if (is.na(year[i]) || (nchar(m[1,4]) == 4L && y != year[i]) ||
          (nchar(m[1,4]) == 2L && y != year[i] %% 100L)) status <- 'conflicting_birth_year'
      leap <- !is.na(year[i]) && year[i] %% 4L == 0L && (year[i] %% 100L != 0L || year[i] %% 400L == 0L)
      max_day <- c(31L, if (leap) 29L else 28L,
                   31L,30L,31L,30L,31L,31L,30L,31L,30L,31L)
      if (is.na(month) || day < 1L || day > max_day[month]) status <- 'invalid_birth_date'
    }
    data.table(month = if (status == 'assigned') month else NA_integer_, month_status = status,
               birth_segment = text[i])
  }))
}

monthly_rate <- function(count, denominator) {
  out <- rep(NA_real_, length(count))
  ok <- !is.na(denominator) & denominator > 0
  out[ok] <- 1000 * count[ok] / denominator[ok]
  out
}

# Preserve the upstream county estimates and national population universe.
# Validate them against the raw USA row instead of silently recomputing shares.
validate_gapminder_annual <- function(annual, file, expected_years=1915:1950) {
  required <- c('year','population','county_births_estimate_year',
                'us_births_year','us_pop_year','us_birth_rate_year')
  missing <- setdiff(required,names(annual))
  if (length(missing)) stop('Annual Gapminder columns missing: ',paste(missing,collapse=', '))
  national <- unique(annual[,.(year,us_births_year,us_pop_year,us_birth_rate_year)])
  if (anyDuplicated(national$year) || !setequal(national$year,expected_years)) {
    stop('Require one consistent national denominator for each selected year')
  }
  setorder(national,year)
  raw <- fread(file,header=TRUE)
  usa <- raw[geo=='usa']
  years <- as.character(national$year)
  if (nrow(usa)!=1L || !all(years %in% names(usa))) stop('Require one Gapminder USA row with all target years')
  national[, gapminder_us_births := as.numeric(unlist(usa[,years,with=FALSE],use.names=FALSE))]
  if (anyNA(national) || any(national$us_pop_year<=0) || any(national$us_births_year<=0) ||
      any(national$gapminder_us_births != national$us_births_year)) {
    stop('Gapminder raw births disagree with annual panel, or national inputs are invalid; rebuild upstream annual inputs')
  }
  if (!isTRUE(all.equal(national$us_birth_rate_year,national$us_births_year/national$us_pop_year,tolerance=1e-10))) {
    stop('Annual national birth rates do not match the existing national population denominator')
  }
  expected <- annual$population*annual$us_birth_rate_year
  if (!isTRUE(all.equal(annual$county_births_estimate_year,expected,tolerance=1e-10))) {
    stop('Annual county estimates do not reconcile to population times national birth rate')
  }
  national
}

verify_monthly_rates <- function(count, denominator, rate) {
  expected <- monthly_rate(count,denominator)
  if (!isTRUE(all.equal(rate,expected,tolerance=1e-10))) stop('Monthly rates do not match their denominators')
  invisible(TRUE)
}

read_occurrence_births <- function(root, label) {
  ds <- if (label == 'early') '1915_1941_cVS' else '1939_1959_cVS'
  code <- if (label == 'early') 'AF1U001' else 'AGVA001'
  files <- sort(list.files(file.path(root,label,'csv'), '_(county|state)[.]csv$', recursive=TRUE, full.names=TRUE))
  expected <- if (label == 'early') 54L else 24L
  if (length(files) != expected) stop('Incomplete ', label, ' extract; run download_nhgis_births.R')
  rbindlist(lapply(files, function(f) {
    x <- fread(f, colClasses='character')
    x <- x[grepl('^[0-9]{4}$', YEAR)] # csv_header includes a second label row
    if (!code %in% names(x)) stop('NT001 column missing: ', f)
    births <- suppressWarnings(as.numeric(x[[code]]))
    bad <- nzchar(x[[code]]) & !is.na(x[[code]]) & (is.na(births) | births < 0)
    if (any(bad)) stop('Unexpected birth sentinel/value in ', f)
    county <- grepl('_county[.]csv$', f)
    data.table(GISJOIN=x$GISJOIN, year=as.integer(x$YEAR), state_name=x$STATE,
      county_name=if (county) x$COUNTY else NA_character_,
      STATEA=x$STATEA, COUNTYA=if (county) x$COUNTYA else NA_character_,
      geography=if (county) 'county' else 'state', annual_recorded_births=births,
      nhgis_dataset=ds, nhgis_table='NT001', nhgis_variable=code, nhgis_file=basename(f))
  }))
}

# No fuzzy or many-to-one matches. Historic NHGIS IDs must correspond to an
# explicit ID in the project's nominal population crosswalk and county universe.
match_birth_counties <- function(births, reference, universe) {
  ref <- unique(reference[, .(GISJOIN, GEOID)])
  ref[, reference_candidates := uniqueN(GEOID), by=GISJOIN]
  ref <- ref[, .(GEOID=if (uniqueN(GEOID)==1L) GEOID[1] else NA_character_,
                 reference_candidates=uniqueN(GEOID)), by=GISJOIN]
  out <- merge(births, ref, by='GISJOIN', all.x=TRUE, sort=FALSE)
  out[, county_match_status := fifelse(is.na(reference_candidates), 'unmatched_historical_id',
    fifelse(reference_candidates > 1L, 'ambiguous_historical_id',
      fifelse(GEOID %in% universe, 'matched_nominal_id', 'outside_county_universe')))]
  # Reject duplicate target counties within an extract-year even if each ID matched.
  out[, target_matches := .N, by=.(nhgis_dataset, year, GEOID)]
  out[county_match_status == 'matched_nominal_id' & target_matches > 1L,
      county_match_status := 'ambiguous_target_county']
  out[GISJOIN == 'G3609610' | (GISJOIN == 'G3600610' & nhgis_dataset == '1915_1941_cVS' & year >= 1939L),
      county_match_status := 'aggregate_new_york_city_not_borough']
  out
}

choose_birth_vintage <- function(x) {
  # Deliberately no fallback to the early vintage in 1939+ even if late is blank.
  x[(nhgis_dataset == '1915_1941_cVS' & year <= 1938L) |
      (nhgis_dataset == '1939_1959_cVS' & year >= 1939L & year <= 1950L)]
}
