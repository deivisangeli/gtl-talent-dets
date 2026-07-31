###############################################################################
# Pilot general regex rules for AMWS Ed86 rows excluded because birth_city,
# birth_year, or birth_country is missing.
#
# The script evaluates rules against the manually reviewed sample of 200 rows
# and performs a count-only dry run on all missing-information rows. It never
# changes the canonical AMWS files.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..",
                                       ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "prep", "amws", "state_alias.R"))

local_dropbox <- file.path("C:/Users", Sys.info()[["user"]],
                           "Globtalent Dropbox", "gtl_talent_dets")
if (!dir.exists(TALENT_DETS_DATA_DIR) && dir.exists(local_dropbox)) {
  TALENT_DETS_DATA_DIR <- normalizePath(local_dropbox, winslash = "/")
  DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | str_to_upper(str_trim(x)) == "NA", "", x)
}

normalize_text <- function(x) {
  blank_na(x) |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

token_key <- function(x) {
  normalize_text(x) |>
    str_to_lower() |>
    str_replace_all("[^\\p{L}0-9]+", " ") |>
    str_squish()
}

normalize_city_key <- function(x) {
  token_key(x) |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_remove("\\s+(city|town|township|village|borough|cdp)$") |>
    str_squish()
}

normalize_digits <- function(x) {
  chartr("OoIiLlSsZzBb|", "0011115522881", x)
}

amws_year <- function(x) {
  x <- normalize_digits(x)
  if (!str_detect(x, "^([0-9]{2}|[0-9]{4})$")) return("")
  value <- suppressWarnings(as.integer(x))
  if (nchar(x) == 2L) value <- ifelse(value <= 86L, 1900L + value,
                                      1800L + value)
  if (is.na(value) || value < 1800L || value > 1986L) "" else as.character(value)
}

PROVINCE_ALIAS <- c(
  "ab" = "AB", "alta" = "AB", "alberta" = "AB",
  "bc" = "BC", "british columbia" = "BC",
  "man" = "MB", "manitoba" = "MB",
  "nb" = "NB", "new brunswick" = "NB",
  "ns" = "NS", "nova scotia" = "NS",
  "ont" = "ON", "ontario" = "ON",
  "que" = "QC", "quebec" = "QC",
  "sask" = "SK", "saskatchewan" = "SK",
  "pei" = "PE", "prince edward island" = "PE"
)

COUNTRY_ALIAS <- c(
  "arg" = "Argentina", "argentina" = "Argentina",
  "australia" = "Australia", "austria" = "Austria",
  "belg" = "Belgium", "belgium" = "Belgium",
  "brazil" = "Brazil", "brasil" = "Brazil",
  "can" = "Canada", "canada" = "Canada",
  "china" = "China", "denmark" = "Denmark",
  "eng" = "England", "england" = "England",
  "estonia" = "Estonia", "france" = "France",
  "ger" = "Germany", "germany" = "Germany",
  "greece" = "Greece", "holland" = "Netherlands",
  "netherlands" = "Netherlands", "hungary" = "Hungary",
  "india" = "India", "indonesia" = "Indonesia",
  "ireland" = "Ireland", "israel" = "Israel",
  "italy" = "Italy", "japan" = "Japan", "korea" = "Korea",
  "mex" = "Mexico", "mexico" = "Mexico",
  "norway" = "Norway", "pakistan" = "Pakistan",
  "poland" = "Poland", "portugal" = "Portugal",
  "romania" = "Romania", "rumania" = "Romania",
  "scotland" = "Scotland", "south vietnam" = "South Vietnam",
  "s vietnam" = "South Vietnam", "svietnam" = "South Vietnam",
  "spain" = "Spain", "sweden" = "Sweden",
  "switzerland" = "Switzerland", "taiwan" = "Taiwan",
  "turkey" = "Turkey", "uk" = "United Kingdom",
  "united kingdom" = "United Kingdom", "us" = "USA",
  "usa" = "USA", "united states" = "USA",
  "ussr" = "USSR", "wales" = "Wales", "yugoslavia" = "Yugoslavia"
)

state_alias_clean <- STATE_ALIAS
names(state_alias_clean) <- vapply(names(STATE_ALIAS), token_key, character(1))
state_alias_clean <- state_alias_clean[!duplicated(names(state_alias_clean))]
names(PROVINCE_ALIAS) <- vapply(names(PROVINCE_ALIAS), token_key, character(1))
names(COUNTRY_ALIAS) <- vapply(names(COUNTRY_ALIAS), token_key, character(1))

lookup_location_alias <- function(key) {
  key <- token_key(key)
  state <- unname(state_alias_clean[key])
  if (!is.na(state) && nzchar(state)) {
    return(list(kind = "us_state", code = state, country = "USA"))
  }
  province <- unname(PROVINCE_ALIAS[key])
  if (!is.na(province) && nzchar(province)) {
    return(list(kind = "canada_province", code = province,
                country = "Canada"))
  }
  country <- unname(COUNTRY_ALIAS[key])
  if (!is.na(country) && nzchar(country)) {
    return(list(kind = "country", code = "", country = country))
  }
  NULL
}

empty_location <- function(source = "") {
  list(city = "", state = "", country = "", year_after_suffix = "",
       rule = "", source = source, evidence = "", city_validated = FALSE)
}

tokenize_location <- function(x) {
  str_split(token_key(x), " ", simplify = FALSE)[[1]]
}

parse_location_tokens <- function(x, source, gazetteer_keys = NULL) {
  original <- normalize_text(x)
  tokens <- tokenize_location(original)
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) return(empty_location(source))

  alias_at <- function(start, max_words = 3L) {
    if (start > length(tokens)) return(NULL)
    for (n_word in rev(seq_len(min(max_words, length(tokens) - start + 1L)))) {
      key <- paste(tokens[start:(start + n_word - 1L)], collapse = " ")
      hit <- lookup_location_alias(key)
      if (!is.null(hit)) return(c(hit, list(n_word = n_word, key = key)))
    }
    NULL
  }

  leading <- alias_at(1L)
  if (!is.null(leading)) {
    next_start <- 1L + leading$n_word
    next_alias <- alias_at(next_start)
    if (is.null(next_alias)) {
      year <- ""
      if (next_start <= length(tokens) &&
          str_detect(tokens[[next_start]], "^([0-9]{2}|[0-9]{4})$")) {
        year <- amws_year(tokens[[next_start]])
      }
      rule <- switch(
        leading$kind,
        us_state = "R_LOCATION_SUFFIX_US_STATE",
        canada_province = "R_LOCATION_SUFFIX_CA_PROVINCE",
        country = "R_COUNTRY_EXACT_TOKEN"
      )
      return(list(
        city = "", state = leading$code, country = leading$country,
        year_after_suffix = year, rule = rule, source = source,
        evidence = original, city_validated = FALSE
      ))
    }
  }

  max_start <- min(length(tokens), 8L)
  for (start in seq.int(2L, max_start)) {
    hit <- alias_at(start)
    if (is.null(hit)) next
    city_tokens <- tokens[seq_len(start - 1L)]
    if (length(city_tokens) > 5L) next
    city <- str_to_title(paste(city_tokens, collapse = " "))
    city_key <- normalize_city_key(city)
    if (!str_detect(city_key, "^[a-z][a-z '’-]{2,59}$")) next

    year_pos <- start + hit$n_word
    year <- ""
    if (year_pos <= length(tokens) &&
        str_detect(tokens[[year_pos]], "^([0-9]{2}|[0-9]{4})$")) {
      year <- amws_year(tokens[[year_pos]])
    }

    city_validated <- FALSE
    if (hit$kind == "us_state" && !is.null(gazetteer_keys)) {
      city_validated <- paste(hit$code, city_key, sep = "\r") %in%
        gazetteer_keys
    }
    rule <- switch(
      hit$kind,
      us_state = "R_LOCATION_SUFFIX_US_STATE",
      canada_province = "R_LOCATION_SUFFIX_CA_PROVINCE",
      country = "R_LOCATION_SUFFIX_COUNTRY"
    )
    return(list(
      city = city, state = hit$code, country = hit$country,
      year_after_suffix = year, rule = rule, source = source,
      evidence = original, city_validated = city_validated
    ))
  }
  empty_location(source)
}

MONTH_RX <- paste0(
  "(?:Jan(?:uary)?|Feb(?:ruary)?|Fcb|Mar(?:ch)?|Apr(?:il)?|Api|May|",
  "Jun(?:e)?|Jul(?:y|v)?|Aug(?:ust)?|Sep(?:t)?|Sept|Scpt|",
  "Oct(?:ober)?|Oet|Nov(?:ember)?|Dec(?:ember)?|Dee|Dcc)"
)
DIGIT_RX <- "[0-9OoIiLlSsZzBb|]"

normalize_month <- function(x) {
  key <- str_to_lower(normalize_text(x))
  case_when(
    str_detect(key, "^jan") ~ "Jan",
    str_detect(key, "^(feb|fcb)") ~ "Feb",
    str_detect(key, "^mar") ~ "Mar",
    str_detect(key, "^(apr|api)") ~ "Apr",
    key == "may" ~ "May",
    str_detect(key, "^jun") ~ "June",
    str_detect(key, "^jul") ~ "July",
    str_detect(key, "^aug") ~ "Aug",
    str_detect(key, "^(sep|scpt)") ~ "Sept",
    str_detect(key, "^(oct|oet)") ~ "Oct",
    str_detect(key, "^nov") ~ "Nov",
    str_detect(key, "^(dec|dee|dcc)") ~ "Dec",
    TRUE ~ ""
  )
}

parse_day <- function(x) {
  x <- normalize_digits(x)
  if (!str_detect(x, "^[0-9]{1,2}$")) return(NA_integer_)
  value <- suppressWarnings(as.integer(x))
  if (is.na(value) || value < 1L || value > 31L) NA_integer_ else value
}

extract_date_candidate <- function(raw_text) {
  raw <- str_sub(normalize_text(raw_text), 1L, 220L)
  standard_pattern <- paste0(
    "\\b(", MONTH_RX, ")\\.?\\s*[,;]?\\s*(", DIGIT_RX,
    "{1,2})\\s*[,.;_'’‘\\-^ ]+\\s*(", DIGIT_RX, "{2,4})"
  )
  m <- str_match(raw, regex(standard_pattern, ignore_case = TRUE))
  loc <- str_locate(raw, regex(standard_pattern, ignore_case = TRUE))
  if (!is.na(m[1, 1])) {
    day <- parse_day(m[1, 3])
    year <- amws_year(m[1, 4])
    if (!is.na(day) && nzchar(year)) {
      return(list(
        found = TRUE, start = loc[1, 1], end = loc[1, 2],
        birth_date = paste0(normalize_month(m[1, 2]), " ", day, ", ",
                            str_sub(year, 3, 4)),
        birth_year = year, rule = "R_DATE_MDY_FLEX",
        evidence = m[1, 1]
      ))
    }
  }

  compact_pattern <- paste0(
    "\\b(", MONTH_RX, ")\\.?\\s*[,;]?\\s*(", DIGIT_RX,
    "{3,4})"
  )
  m <- str_match(raw, regex(compact_pattern, ignore_case = TRUE))
  loc <- str_locate(raw, regex(compact_pattern, ignore_case = TRUE))
  if (!is.na(m[1, 1])) {
    compact <- normalize_digits(m[1, 3])
    day <- parse_day(str_sub(compact, 1L, nchar(compact) - 2L))
    year <- amws_year(str_sub(compact, -2L))
    if (!is.na(day) && nzchar(year)) {
      return(list(
        found = TRUE, start = loc[1, 1], end = loc[1, 2],
        birth_date = paste0(normalize_month(m[1, 2]), " ", day, ", ",
                            str_sub(year, 3, 4)),
        birth_year = year, rule = "R_DATE_MDY_COMPACT",
        evidence = m[1, 1]
      ))
    }
  }

  month_year_pattern <- paste0(
    "\\b(", MONTH_RX, ")\\b[^\\p{L}0-9]{1,3}(", DIGIT_RX,
    "{2}|1[89][0-9]{2})(?=\\s+(?:m|c)\\b|\\s+[A-Z]{4,})"
  )
  m <- str_match(raw, regex(month_year_pattern, ignore_case = FALSE))
  loc <- str_locate(raw, regex(month_year_pattern, ignore_case = FALSE))
  if (!is.na(m[1, 1])) {
    year <- amws_year(m[1, 3])
    if (nzchar(year)) {
      return(list(
        found = TRUE, start = loc[1, 1], end = loc[1, 2],
        birth_date = paste0(normalize_month(m[1, 2]), " ", year),
        birth_year = year, rule = "R_MONTH_YEAR_BIRTH_ONLY",
        evidence = m[1, 1]
      ))
    }
  }

  list(found = FALSE, start = NA_integer_, end = NA_integer_,
       birth_date = "", birth_year = "", rule = "", evidence = "")
}

location_segments_before_date <- function(raw_text, date_start) {
  raw <- str_sub(normalize_text(raw_text), 1L, 220L)
  if (is.na(date_start) || date_start <= 1L) return(character())
  prefix <- str_sub(raw, 1L, date_start - 1L) |>
    str_replace("[,.;:\\s]+$", "") |>
    normalize_text()
  candidates <- character()

  b_hits <- str_locate_all(prefix, regex("b\\s+(?=[A-Z])"))[[1]]
  if (nrow(b_hits)) {
    candidates <- c(candidates,
                    str_sub(prefix, b_hits[nrow(b_hits), 2] + 1L))
  }

  commas <- str_locate_all(prefix, fixed(","))[[1]]
  if (nrow(commas) >= 2L) {
    candidates <- c(candidates,
                    str_sub(prefix, commas[nrow(commas) - 1L, 2] + 1L))
  }
  if (nrow(commas) >= 1L) {
    candidates <- c(candidates,
                    str_sub(prefix, commas[nrow(commas), 2] + 1L))
  }
  candidates <- c(candidates, prefix)
  unique(normalize_text(candidates[nzchar(normalize_text(candidates))]))
}

choose_raw_location <- function(raw_text, date, gazetteer_keys) {
  if (!date$found) return(empty_location("raw_birth_clause"))
  segments <- location_segments_before_date(raw_text, date$start)
  if (!length(segments)) return(empty_location("raw_birth_clause"))
  parsed <- lapply(segments, parse_location_tokens,
                   source = "raw_birth_clause",
                   gazetteer_keys = gazetteer_keys)

  # The first segment is the text after an explicit/glued birth marker when
  # such a marker exists. Prefer it over comma-derived fallbacks so a person's
  # name cannot be reinterpreted as a foreign birthplace city.
  explicit_b <- str_detect(
    str_sub(normalize_text(raw_text), 1L, date$start),
    "b\\s+(?=[A-Z])"
  )
  if (explicit_b && nzchar(parsed[[1]]$country) &&
      (!nzchar(parsed[[1]]$city) || parsed[[1]]$city_validated)) {
    return(parsed[[1]])
  }
  scores <- vapply(parsed, function(x) {
    10L * nzchar(x$country) + 5L * nzchar(x$state) +
      20L * (nzchar(x$city) && x$city_validated) -
      nchar(x$city) / 100
  }, numeric(1))
  if (!length(scores) || max(scores) <= 0) return(empty_location("raw_birth_clause"))
  parsed[[which.max(scores)]]
}

proposal_for_row <- function(row, gazetteer_keys) {
  old_city <- blank_na(row$birth_city_old)
  old_year <- blank_na(row$birth_year_old)
  old_state <- blank_na(row$birth_state_old)
  old_country <- blank_na(row$birth_country_old)
  missing_city <- identical(blank_na(row$missing_birth_city), "TRUE")
  missing_year <- identical(blank_na(row$missing_birth_year), "TRUE")
  missing_country <- identical(blank_na(row$missing_birth_country), "TRUE")

  old_loc <- parse_location_tokens(row$birth_place_old, "old_birth_place",
                                   gazetteer_keys)
  date <- extract_date_candidate(row$raw_text_adjusted)
  raw_loc <- choose_raw_location(row$raw_text_adjusted, date, gazetteer_keys)

  loc <- old_loc
  if ((!nzchar(loc$country) && nzchar(raw_loc$country)) ||
      (!nzchar(loc$state) && nzchar(raw_loc$state)) ||
      (!nzchar(loc$city) && nzchar(raw_loc$city))) {
    loc <- raw_loc
  }

  # A US/Canadian suffix following an unvalidated city fragment is too broad
  # to automate (for example OCR prose containing "la" or "nm"). State-only
  # clauses and exact gazetteer city/state pairs remain eligible.
  unsafe_north_america_city <-
    loc$rule %in% c("R_LOCATION_SUFFIX_US_STATE",
                    "R_LOCATION_SUFFIX_CA_PROVINCE") &&
    nzchar(loc$city) && !loc$city_validated
  if (unsafe_north_america_city) loc <- empty_location(loc$source)

  # Do not reinterpret a populated city whose text is itself a full US state
  # name as a state-only birthplace (notably New York OCR fragments).
  if (!nzchar(loc$city) && nzchar(old_city) &&
      loc$rule == "R_LOCATION_SUFFIX_US_STATE" &&
      str_count(normalize_city_key(old_city), " ") >= 1L &&
      str_starts(token_key(loc$evidence), normalize_city_key(old_city))) {
    loc <- empty_location(loc$source)
  }

  regex_city <- ""
  city_rule <- ""
  if (missing_city && nzchar(loc$city) && loc$city_validated) {
    regex_city <- loc$city
    city_rule <- ifelse(loc$source == "raw_birth_clause",
                        "R_BIRTH_CLAUSE_STRICT", loc$rule)
  }

  regex_country <- ""
  country_rule <- ""
  if (missing_country && nzchar(loc$country)) {
    regex_country <- loc$country
    country_rule <- loc$rule
  }

  regex_state <- ""
  state_rule <- ""
  state_is_useful <- missing_country || nzchar(regex_city)
  if (!nzchar(old_state) && state_is_useful && nzchar(loc$state)) {
    regex_state <- loc$state
    state_rule <- loc$rule
  }

  regex_year <- ""
  year_rule <- ""
  regex_date <- ""
  if (missing_year) {
    if (date$found &&
        (nzchar(raw_loc$country) ||
           str_detect(str_sub(normalize_text(row$raw_text_adjusted), 1L,
                              date$start), "b\\s"))) {
      regex_year <- date$birth_year
      regex_date <- date$birth_date
      year_rule <- date$rule
    } else if (nzchar(old_loc$year_after_suffix)) {
      regex_year <- old_loc$year_after_suffix
      regex_date <- old_loc$year_after_suffix
      year_rule <- "R_YEAR_AFTER_RECOGNIZED_PLACE"
    }
  }

  evidence <- paste(unique(c(
    if (nzchar(regex_city) || nzchar(regex_state) || nzchar(regex_country)) {
      loc$evidence
    },
    if (nzchar(regex_year)) date$evidence
  )), collapse = " | ")

  tibble(
    regex_birth_date = regex_date,
    regex_birth_year = regex_year,
    regex_birth_city = regex_city,
    regex_birth_state = regex_state,
    regex_birth_country = regex_country,
    birth_year_rule = year_rule,
    birth_city_rule = city_rule,
    birth_state_rule = state_rule,
    birth_country_rule = country_rule,
    regex_evidence = evidence
  )
}

canonical_state <- function(x) {
  x <- blank_na(x)
  if (!nzchar(x)) return("")
  us <- normalize_state(x)
  if (!is.na(us) && nzchar(us)) return(us)
  province <- unname(PROVINCE_ALIAS[token_key(x)])
  ifelse(is.na(province), str_to_upper(token_key(x)), province)
}

canonical_country <- function(x) {
  x <- blank_na(x)
  if (!nzchar(x)) return("")
  out <- unname(COUNTRY_ALIAS[token_key(x)])
  ifelse(is.na(out), str_to_title(token_key(x)), out)
}

values_match <- function(field, actual, expected) {
  actual <- blank_na(actual)
  expected <- blank_na(expected)
  if (!nzchar(actual) && !nzchar(expected)) return(TRUE)
  if (field == "birth_city") {
    return(identical(normalize_city_key(actual), normalize_city_key(expected)))
  }
  if (field == "birth_state") {
    return(identical(canonical_state(actual), canonical_state(expected)))
  }
  if (field == "birth_country") {
    return(identical(canonical_country(actual), canonical_country(expected)))
  }
  identical(actual, expected)
}

data_dir <- file.path(TALENT_DETS_DATA_DIR, "Data")
pilot_root <- file.path(data_dir, "intermediary", "amws",
                        "manual_missing_birth_info_pilot_20260713")
review_file <- file.path(
  pilot_root, "amws_ed86_missing_birth_info_sample200_reviews.csv"
)
full_file <- file.path(data_dir, "processed", "amws", "amws_ed86.csv")
gaz_file <- file.path(DATA_INPUT, "2024_Gaz_place_national.txt")
output_candidates <- file.path(
  pilot_root, "amws_ed86_missing_birth_info_regex_pilot_candidates.csv"
)
output_events <- file.path(
  pilot_root, "amws_ed86_missing_birth_info_regex_pilot_events.csv"
)
output_summary <- file.path(
  pilot_root, "amws_ed86_missing_birth_info_regex_pilot_summary.csv"
)

stopifnot(file.exists(review_file), file.exists(full_file), file.exists(gaz_file))

gazetteer <- read_tsv(gaz_file,
                      col_types = cols(.default = col_character()),
                      show_col_types = FALSE,
                      locale = locale(encoding = "UTF-8"), progress = FALSE) |>
  rename_with(trimws) |>
  transmute(state = USPS, city_key = normalize_city_key(NAME)) |>
  filter(nzchar(state), nzchar(city_key)) |>
  distinct()
gazetteer_keys <- paste(gazetteer$state, gazetteer$city_key, sep = "\r")

reviews <- read_csv(review_file, col_types = cols(.default = col_character()),
                    show_col_types = FALSE, progress = FALSE) |>
  mutate(across(everything(), blank_na))
if (nrow(reviews) != 200L ||
    n_distinct(paste(reviews$doc_id, reviews$lineid, sep = "\r")) != 200L) {
  stop("The reviewed pilot must contain 200 unique doc_id + lineid rows.")
}
if (sum(reviews$recovery_status %in%
        c("fully_recoverable", "partially_recoverable")) != 38L) {
  stop("Expected 38 positive recoverable rows in the reviewed pilot.")
}

proposals <- bind_rows(lapply(seq_len(nrow(reviews)), function(i) {
  proposal_for_row(as.list(reviews[i, ]), gazetteer_keys)
}))
candidates <- bind_cols(reviews, proposals)

field_specs <- tribble(
  ~field, ~actual_col, ~expected_col, ~rule_col,
  "birth_year", "regex_birth_year", "birth_year_proposed", "birth_year_rule",
  "birth_city", "regex_birth_city", "birth_city_proposed", "birth_city_rule",
  "birth_state", "regex_birth_state", "birth_state_proposed", "birth_state_rule",
  "birth_country", "regex_birth_country", "birth_country_proposed",
  "birth_country_rule"
)

events <- bind_rows(lapply(seq_len(nrow(field_specs)), function(j) {
  spec <- field_specs[j, ]
  tibble(
    sample_id = candidates$sample_id,
    batch_id = candidates$batch_id,
    doc_id = candidates$doc_id,
    lineid = candidates$lineid,
    recovery_status = candidates$recovery_status,
    field = spec$field,
    regex_value = candidates[[spec$actual_col]],
    manual_value = candidates[[spec$expected_col]],
    rule_id = candidates[[spec$rule_col]],
    regex_evidence = candidates$regex_evidence
  ) |>
    filter(nzchar(regex_value)) |>
    rowwise() |>
    mutate(
      comparison = if_else(
        values_match(field, regex_value, manual_value), "match",
        if_else(nzchar(manual_value), "mismatch", "false_positive")
      )
    ) |>
    ungroup()
}))

expected_long <- bind_rows(lapply(seq_len(nrow(field_specs)), function(j) {
  spec <- field_specs[j, ]
  tibble(
    sample_id = candidates$sample_id,
    field = spec$field,
    manual_value = candidates[[spec$expected_col]],
    regex_value = candidates[[spec$actual_col]]
  ) |>
    filter(nzchar(manual_value)) |>
    rowwise() |>
    mutate(captured = values_match(field, regex_value, manual_value)) |>
    ungroup()
}))

rule_independent_rows <- events |>
  filter(comparison == "match") |>
  summarise(independent_match_rows = n_distinct(sample_id), .by = rule_id)

canonical_geographic_rules <- c(
  "R_COUNTRY_EXACT_TOKEN",
  "R_LOCATION_SUFFIX_US_STATE",
  "R_LOCATION_SUFFIX_CA_PROVINCE",
  "R_LOCATION_SUFFIX_COUNTRY"
)

rule_summary <- events |>
  count(rule_id, comparison, name = "n") |>
  tidyr::complete(
    rule_id,
    comparison = c("match", "mismatch", "false_positive"),
    fill = list(n = 0L)
  ) |>
  tidyr::pivot_wider(names_from = comparison, values_from = n, values_fill = 0) |>
  left_join(rule_independent_rows, by = "rule_id") |>
  mutate(
    match = ifelse(is.na(match), 0L, match),
    mismatch = ifelse(is.na(mismatch), 0L, mismatch),
    false_positive = ifelse(is.na(false_positive), 0L, false_positive),
    independent_match_rows = ifelse(is.na(independent_match_rows), 0L,
                                    independent_match_rows),
    event_rows = match + mismatch + false_positive,
    precision = if_else(event_rows > 0L, match / event_rows, NA_real_),
    promotion_status = if_else(
      mismatch == 0L & false_positive == 0L &
        (independent_match_rows >= 2L |
           (rule_id %in% canonical_geographic_rules &
              independent_match_rows >= 1L & match >= 2L)),
      "promote", "diagnostic_only"
    )
  ) |>
  arrange(desc(promotion_status), desc(match), rule_id)

promoted_rules <- rule_summary |>
  filter(promotion_status == "promote") |>
  pull(rule_id)

candidates <- candidates |>
  mutate(
    regex_birth_year_promoted = if_else(
      birth_year_rule %in% promoted_rules, regex_birth_year, ""
    ),
    regex_birth_city_promoted = if_else(
      birth_city_rule %in% promoted_rules, regex_birth_city, ""
    ),
    regex_birth_state_promoted = if_else(
      birth_state_rule %in% promoted_rules, regex_birth_state, ""
    ),
    regex_birth_country_promoted = if_else(
      birth_country_rule %in% promoted_rules, regex_birth_country, ""
    )
  ) |>
  rowwise() |>
  mutate(
    regex_proposed_required_n = sum(c(
      missing_birth_city == "TRUE" && nzchar(regex_birth_city),
      missing_birth_year == "TRUE" && nzchar(regex_birth_year),
      missing_birth_country == "TRUE" && nzchar(regex_birth_country)
    )),
    regex_all_required_match = all(c(
      if (missing_birth_city == "TRUE") {
        values_match("birth_city", regex_birth_city, birth_city_proposed)
      } else TRUE,
      if (missing_birth_year == "TRUE") {
        values_match("birth_year", regex_birth_year, birth_year_proposed)
      } else TRUE,
      if (missing_birth_country == "TRUE") {
        values_match("birth_country", regex_birth_country,
                     birth_country_proposed)
      } else TRUE
    )),
    regex_would_make_final_eligible =
      missing_birth_city != "TRUE" || nzchar(regex_birth_city),
    regex_would_make_final_eligible = regex_would_make_final_eligible &
      (missing_birth_year != "TRUE" || nzchar(regex_birth_year)) &
      (missing_birth_country != "TRUE" || nzchar(regex_birth_country)),
    promoted_proposed_required_n = sum(c(
      missing_birth_city == "TRUE" && nzchar(regex_birth_city_promoted),
      missing_birth_year == "TRUE" && nzchar(regex_birth_year_promoted),
      missing_birth_country == "TRUE" && nzchar(regex_birth_country_promoted)
    )),
    promoted_all_required_match = all(c(
      if (missing_birth_city == "TRUE") {
        values_match("birth_city", regex_birth_city_promoted,
                     birth_city_proposed)
      } else TRUE,
      if (missing_birth_year == "TRUE") {
        values_match("birth_year", regex_birth_year_promoted,
                     birth_year_proposed)
      } else TRUE,
      if (missing_birth_country == "TRUE") {
        values_match("birth_country", regex_birth_country_promoted,
                     birth_country_proposed)
      } else TRUE
    )),
    promoted_would_make_final_eligible =
      (missing_birth_city != "TRUE" || nzchar(regex_birth_city_promoted)) &
      (missing_birth_year != "TRUE" || nzchar(regex_birth_year_promoted)) &
      (missing_birth_country != "TRUE" ||
         nzchar(regex_birth_country_promoted))
  ) |>
  ungroup()

promoted_expected_long <- bind_rows(lapply(seq_len(nrow(field_specs)),
                                            function(j) {
  spec <- field_specs[j, ]
  promoted_col <- paste0(spec$actual_col, "_promoted")
  tibble(
    sample_id = candidates$sample_id,
    field = spec$field,
    manual_value = candidates[[spec$expected_col]],
    regex_value = candidates[[promoted_col]]
  ) |>
    filter(nzchar(manual_value)) |>
    rowwise() |>
    mutate(captured = values_match(field, regex_value, manual_value)) |>
    ungroup()
}))

# Count-only full-corpus dry run. Only rows with at least one missing required
# field are evaluated, and no canonical file is written.
skip_full_scan <- str_to_lower(Sys.getenv(
  "AMWS_ED86_REGEX_PILOT_SKIP_FULL_SCAN", unset = "false"
)) %in% c("true", "1", "yes")

if (skip_full_scan) {
  full <- tibble()
  full_rule_counts <- tibble(rule_id = character(),
                             candidate_events = integer())
} else {
full <- read_csv(full_file, col_types = cols(.default = col_character()),
                 show_col_types = FALSE, progress = FALSE) |>
  mutate(across(everything(), blank_na)) |>
  mutate(
    missing_birth_city = !nzchar(str_trim(birth_city)),
    missing_birth_year = !nzchar(str_trim(birth_year)),
    missing_birth_country = !nzchar(str_trim(birth_country))
  ) |>
  filter(missing_birth_city | missing_birth_year | missing_birth_country) |>
  transmute(
    doc_id, lineid, entry_instance,
    birth_place_old = birth_place, birth_date_old = birth_date,
    birth_year_old = birth_year, birth_city_old = birth_city,
    birth_state_old = birth_state, birth_country_old = birth_country,
    raw_text_adjusted,
    missing_birth_city = as.character(missing_birth_city),
    missing_birth_year = as.character(missing_birth_year),
    missing_birth_country = as.character(missing_birth_country)
  )

full_proposals <- bind_rows(lapply(seq_len(nrow(full)), function(i) {
  proposal_for_row(as.list(full[i, ]), gazetteer_keys)
}))
full_candidates <- bind_cols(full, full_proposals)
full_rule_counts <- bind_rows(lapply(seq_len(nrow(field_specs)), function(j) {
  spec <- field_specs[j, ]
  tibble(rule_id = full_candidates[[spec$rule_col]],
         value = full_candidates[[spec$actual_col]]) |>
    filter(nzchar(rule_id), nzchar(value)) |>
    count(rule_id, name = "candidate_events")
})) |>
  group_by(rule_id) |>
  summarise(candidate_events = sum(candidate_events), .groups = "drop")
}

rule_summary <- rule_summary |>
  left_join(full_rule_counts, by = "rule_id") |>
  mutate(candidate_events = ifelse(is.na(candidate_events), 0L,
                                   candidate_events))

overall_summary <- bind_rows(
  tibble(metric = "sample_rows", value = "200"),
  tibble(metric = "positive_rows", value = "38"),
  tibble(metric = "negative_control_rows", value = "162"),
  tibble(metric = "regex_event_rows", value = as.character(nrow(events))),
  tibble(metric = "regex_matches", value = as.character(sum(events$comparison == "match"))),
  tibble(metric = "regex_mismatches", value = as.character(sum(events$comparison == "mismatch"))),
  tibble(metric = "regex_false_positives",
         value = as.character(sum(events$comparison == "false_positive"))),
  tibble(metric = "manual_expected_field_values",
         value = as.character(nrow(expected_long))),
  tibble(metric = "manual_expected_field_values_captured",
         value = as.character(sum(expected_long$captured))),
  tibble(metric = "manual_expected_field_values_captured_by_promoted_rules",
         value = as.character(sum(promoted_expected_long$captured))),
  tibble(metric = "fully_recoverable_rows_made_final_eligible",
         value = as.character(sum(candidates$recovery_status == "fully_recoverable" &
                                    candidates$promoted_would_make_final_eligible &
                                    candidates$promoted_all_required_match))),
  tibble(metric = "partially_recoverable_rows_with_any_required_capture",
         value = as.character(sum(candidates$recovery_status == "partially_recoverable" &
                                    candidates$promoted_proposed_required_n > 0L))),
  tibble(metric = "promoted_rule_count",
         value = as.character(length(promoted_rules))),
  tibble(metric = "promoted_rules", value = paste(promoted_rules, collapse = ";")),
  tibble(metric = "full_missing_universe_rows", value = as.character(nrow(full))),
  tibble(metric = "canonical_input_md5", value = unname(tools::md5sum(full_file))),
  tibble(metric = "manual_review_md5", value = unname(tools::md5sum(review_file)))
)

summary_output <- bind_rows(
  overall_summary |>
    mutate(summary_type = "overall", rule_id = "", .before = 1),
  rule_summary |>
    transmute(
      summary_type = "rule", rule_id,
      metric = paste0(
        "events=", event_rows,
        ";matches=", match,
        ";independent_match_rows=", independent_match_rows,
        ";mismatches=", mismatch,
        ";false_positives=", false_positive,
        ";precision=", sprintf("%.6f", precision),
        ";full_scan_events=", candidate_events,
        ";status=", promotion_status
      ),
      value = promotion_status
    )
)

write_excel_csv(candidates, output_candidates, na = "")
write_excel_csv(events, output_events, na = "")
write_excel_csv(summary_output, output_summary, na = "")

cat("Sample rows:", nrow(candidates), "\n")
cat("Regex events:", nrow(events), "\n")
cat("Matches:", sum(events$comparison == "match"), "\n")
cat("Mismatches:", sum(events$comparison == "mismatch"), "\n")
cat("False positives:", sum(events$comparison == "false_positive"), "\n")
cat("Promoted rules:", paste(promoted_rules, collapse = ", "), "\n")
cat("Candidates:", output_candidates, "\n")
cat("Events:", output_events, "\n")
cat("Summary:", output_summary, "\n")
