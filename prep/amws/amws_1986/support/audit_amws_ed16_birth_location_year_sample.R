###############################################################################
# Audit birth_city, birth_state, and birth_year against raw_text_adjusted.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv
#
# Writes:
#   output/amws/regex_all_docs/
#     audit_1000_birth_location_year.csv
#     audit_1000_birth_location_year.xlsx
#     audit_1000_birth_location_year_summary.csv
#
# Environment overrides:
#   AMWS_LOCATION_YEAR_AUDIT_INPUT_FILE
#   AMWS_LOCATION_YEAR_AUDIT_OUTPUT_DIR
#   AMWS_LOCATION_YEAR_AUDIT_OUTPUT_STEM
#   AMWS_LOCATION_YEAR_AUDIT_TEXT_COL
#   AMWS_LOCATION_YEAR_AUDIT_SAMPLE_N
#   AMWS_LOCATION_YEAR_AUDIT_SEED
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(writexl)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..", ".."),
                             winslash = "/", mustWork = TRUE)
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
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
}

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed)) {
    stop("Environment variable ", name, " must be an integer; got: ", value)
  }
  parsed
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x), "", x)
}

normalize_text <- function(x) {
  blank_na(x) |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

strip_edge_punct <- function(x) {
  normalize_text(x) |>
    str_replace("^[,;:.\\s'\"’“”\\-_*\\^]+", "") |>
    str_replace("[,;:.\\s'\"’“”\\-_*\\^]+$", "") |>
    normalize_text()
}

norm_key <- function(x) {
  normalize_text(x) |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "")
}

normalize_ocr_digits <- function(x) {
  chartr("OoIiLlSsZzBb|", "0011115522881", x)
}

raw_looks_corrupt <- function(raw_text) {
  raw_text <- normalize_text(raw_text)
  nchar(raw_text) < 25L ||
    str_detect(raw_text,
               regex("[\\^<>]|[A-Za-z][0-9][A-Za-z]|[0-9][A-Za-z][0-9]"))
}

MONTH_RX <- paste(
  "January", "February", "March", "April", "September", "October",
  "November", "December", "June", "July", "August", "Sept", "Sep",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct",
  "Nov", "Dec", "Dee", "Dcc", "Oet", "Mav", "Juiy", "Julv",
  sep = "|"
)

DATE_RX <- paste0(
  "\\b(", MONTH_RX, ")\\.?\\s*,?\\s+",
  "[0-9OISZLB|]{1,2}",
  "(?:\\s*[,.'’`-]?\\s*[0-9OISZLB|]{1,4}[A-Za-z]?)?\\b"
)

OCR_NOV_RX <- "\\bN[o0][*v]\\s*J?[A-Za-z0-9]{2,4}\\b"

find_first_date <- function(x) {
  x <- normalize_text(x)
  loc <- str_locate(x, regex(DATE_RX, ignore_case = TRUE))
  if (!is.na(loc[1, 1])) {
    return(tibble(
      value = str_sub(x, loc[1, 1], loc[1, 2]) |> normalize_text(),
      start = loc[1, 1],
      end = loc[1, 2],
      rule_id = "date_flex_month"
    ))
  }

  loc <- str_locate(x, regex(OCR_NOV_RX, ignore_case = TRUE))
  if (!is.na(loc[1, 1])) {
    return(tibble(
      value = str_sub(x, loc[1, 1], loc[1, 2]) |> normalize_text(),
      start = loc[1, 1],
      end = loc[1, 2],
      rule_id = "date_ocr_november"
    ))
  }

  tibble(value = "", start = NA_integer_, end = NA_integer_, rule_id = "")
}

find_birth_marker <- function(raw_text) {
  marker_patterns <- c(
    spaced = "(?:^|[,.;:\\s])b\\s+(?=[A-Z])",
    h_spaced = "(?i)(?:^|[,.;:])\\s*h\\s+(?=[A-Z])",
    glued = "(?:[,.;:'’*])\\s*b_?\\s*(?=[A-Z])",
    underscore = "(?:^|[,.;:\\s])b_+\\s*(?=[A-Z])"
  )
  hits <- bind_rows(lapply(names(marker_patterns), function(rule_id) {
    loc <- str_locate(raw_text, regex(marker_patterns[[rule_id]]))
    if (is.na(loc[1, 1])) return(NULL)
    tibble(rule_id = rule_id, start = loc[1, 1], end = loc[1, 2])
  }))
  if (!nrow(hits)) {
    return(tibble(rule_id = "", start = NA_integer_, end = NA_integer_))
  }
  slice(arrange(hits, start), 1)
}

extract_raw_birth <- function(raw_text) {
  raw_text <- normalize_text(raw_text)
  marker <- find_birth_marker(raw_text)
  if (!nzchar(marker$rule_id[[1]])) {
    return(tibble(
      expected_birth_place = "",
      expected_birth_date = "",
      raw_birth_rule = "no_birth_marker"
    ))
  }

  after_birth <- str_sub(raw_text, marker$end[[1]] + 1) |> normalize_text()
  date <- find_first_date(after_birth)
  if (!nzchar(date$value[[1]])) {
    birth_segment <- str_split(after_birth, ";", n = 2)[[1]][1]
    birth_segment <- str_split(birth_segment, "\\s+Educ\\b|\\s+Edue\\b|\\s+Prof Exp\\b|\\s+Mem\\b", n = 2)[[1]][1]
    return(tibble(
      expected_birth_place = strip_edge_punct(birth_segment),
      expected_birth_date = "",
      raw_birth_rule = "birth_marker_no_date"
    ))
  }

  tibble(
    expected_birth_place = str_sub(after_birth, 1, date$start[[1]] - 1) |>
      strip_edge_punct(),
    expected_birth_date = date$value[[1]],
    raw_birth_rule = paste(marker$rule_id[[1]], date$rule_id[[1]], sep = "+")
  )
}

amws_century_year <- function(yy) {
  yy <- as.integer(yy)
  ifelse(yy <= 86L, 1900L + yy, 1800L + yy)
}

parse_year_from_date <- function(birth_date) {
  x <- normalize_text(birth_date)
  if (!nzchar(x)) return(NA_integer_)
  x <- str_replace_all(x, "\\bDee\\b|\\bDcc\\b", "Dec")
  x <- str_replace_all(x, "\\bOet\\b", "Oct")
  x <- str_replace_all(x, "\\bMav\\b", "May")
  x <- str_replace(x, "[,;:.\\s]+$", "")
  token <- str_match(x, "([0-9OoIiLlSsZzBb|]{2,4})[A-Za-z]?$")[, 2]
  if (is.na(token) || !nzchar(token)) return(NA_integer_)
  normalized <- normalize_ocr_digits(token)
  if (!str_detect(normalized, "^[0-9]+$")) return(NA_integer_)

  if (nchar(normalized) == 2L) {
    year <- amws_century_year(normalized)
  } else if (nchar(normalized) == 4L) {
    year <- as.integer(normalized)
    if (!is.na(year) && (year < 1800L || year > 1986L)) {
      compact_day <- suppressWarnings(as.integer(str_sub(normalized, 1, 2)))
      compact_yy <- str_sub(normalized, 3, 4)
      if (!is.na(compact_day) && compact_day >= 1L && compact_day <= 31L) {
        year <- amws_century_year(compact_yy)
      }
    }
  } else if (nchar(normalized) == 3L) {
    compact_day <- suppressWarnings(as.integer(str_sub(normalized, 1, 1)))
    compact_yy <- str_sub(normalized, 2, 3)
    year <- if (!is.na(compact_day) && compact_day >= 1L && compact_day <= 9L) {
      amws_century_year(compact_yy)
    } else {
      NA_integer_
    }
  } else {
    year <- NA_integer_
  }

  if (is.na(year) || year < 1800L || year > 1986L) NA_integer_ else as.integer(year)
}

strip_location_punct <- function(x) {
  normalize_text(x) |>
    str_replace("^[,;:.\\s'\"’“”\\-_*\\^]+", "") |>
    str_replace("[,;:.\\s'\"’“”\\-_*\\^]+$", "") |>
    normalize_text()
}

location_key <- function(x) {
  strip_location_punct(x) |>
    str_to_lower() |>
    str_replace_all("\\s+", " ")
}

CANADA_PROVINCE_ALIAS <- c(
  "ab" = "AB", "alta" = "AB", "alberta" = "AB",
  "bc" = "BC", "b c" = "BC", "b. c" = "BC", "b.c" = "BC",
  "british columbia" = "BC",
  "man" = "MB", "manitoba" = "MB",
  "nb" = "NB", "n b" = "NB", "n. b" = "NB", "n.b" = "NB",
  "new brunswick" = "NB",
  "nfld" = "NL", "newfoundland" = "NL",
  "ns" = "NS", "n s" = "NS", "n. s" = "NS", "n.s" = "NS",
  "nova scotia" = "NS",
  "ont" = "ON", "ontario" = "ON",
  "pei" = "PE", "p e i" = "PE", "p. e. i" = "PE",
  "prince edward island" = "PE",
  "que" = "QC", "quc" = "QC", "quebec" = "QC",
  "sask" = "SK", "saskatchewan" = "SK"
)

normalize_canada_province <- function(x) {
  key <- location_key(x)
  out <- unname(CANADA_PROVINCE_ALIAS[key])
  ifelse(is.na(out), "", out)
}

COUNTRY_ALIAS <- c(
  "afghanistan" = "Afghanistan",
  "africa" = "Africa",
  "arg" = "Argentina", "argentina" = "Argentina",
  "australia" = "Australia",
  "austria" = "Austria",
  "belgium" = "Belgium",
  "brazil" = "Brazil",
  "can" = "Canada", "canada" = "Canada",
  "china" = "China",
  "cuba" = "Cuba",
  "czech" = "Czechoslovakia", "czechoslovakia" = "Czechoslovakia",
  "denmark" = "Denmark",
  "egypt" = "Egypt",
  "eng" = "England", "england" = "England",
  "france" = "France",
  "gcr" = "Germany", "ger" = "Germany", "germany" = "Germany",
  "wger" = "West Germany", "w ger" = "West Germany",
  "greece" = "Greece",
  "holland" = "Netherlands", "neth" = "Netherlands",
  "netherlands" = "Netherlands",
  "hong kong" = "Hong Kong",
  "hungary" = "Hungary",
  "india" = "India",
  "indonesia" = "Indonesia",
  "iran" = "Iran",
  "iraq" = "Iraq",
  "ireland" = "Ireland",
  "israel" = "Israel",
  "italy" = "Italy",
  "japan" = "Japan",
  "korea" = "Korea",
  "latvia" = "Latvia",
  "lebanon" = "Lebanon",
  "lithuania" = "Lithuania",
  "mex" = "Mexico", "mexico" = "Mexico",
  "norway" = "Norway",
  "nz" = "New Zealand", "new zealand" = "New Zealand",
  "pakistan" = "Pakistan", "wpakistan" = "Pakistan",
  "palestine" = "Palestine",
  "philippines" = "Philippines",
  "poland" = "Poland",
  "portugal" = "Portugal",
  "romania" = "Romania",
  "russia" = "Russia",
  "scotland" = "Scotland",
  "south africa" = "South Africa", "safrica" = "South Africa",
  "spain" = "Spain",
  "sweden" = "Sweden",
  "switz" = "Switzerland", "switzerland" = "Switzerland",
  "syria" = "Syria",
  "taiwan" = "Taiwan",
  "turkey" = "Turkey",
  "uk" = "United Kingdom", "united kingdom" = "United Kingdom",
  "ussr" = "USSR",
  "wales" = "Wales",
  "yugoslavia" = "Yugoslavia"
)

normalize_country <- function(x) {
  key <- location_key(x)
  out <- unname(COUNTRY_ALIAS[key])
  ifelse(is.na(out), "", out)
}

split_trailing_region <- function(token) {
  token <- strip_location_punct(token)
  if (!nzchar(token)) return(character())
  if (!is.na(normalize_state(token)) ||
      nzchar(normalize_canada_province(token)) ||
      nzchar(normalize_country(token))) {
    return(token)
  }

  words <- str_split(token, "\\s+")[[1]]
  if (length(words) < 2L) return(token)
  max_suffix <- min(3L, length(words) - 1L)
  for (n_suffix in seq_len(max_suffix)) {
    suffix_start <- length(words) - n_suffix + 1L
    suffix <- paste(words[suffix_start:length(words)], collapse = " ")
    if (!is.na(normalize_state(suffix)) ||
        nzchar(normalize_canada_province(suffix)) ||
        nzchar(normalize_country(suffix))) {
      prefix <- paste(words[seq_len(suffix_start - 1L)], collapse = " ")
      return(c(prefix, suffix))
    }
  }
  token
}

split_birth_place_tokens <- function(place) {
  place <- normalize_text(place)
  if (!nzchar(place)) return(character())
  comma_place <- str_replace_all(place, "\\s+\\.\\s+", ", ")
  comma_place <- str_replace_all(comma_place, "\\.\\s*$", "")
  tokens <- str_split(comma_place, "\\s*,\\s*")[[1]]
  if (length(tokens) == 1L && str_detect(tokens[1], "\\.")) {
    tokens <- str_split(tokens[1], "\\s*\\.\\s*")[[1]]
  }
  tokens <- strip_location_punct(tokens)
  tokens <- unlist(lapply(tokens[nzchar(tokens)], split_trailing_region),
                   use.names = FALSE)
  tokens[nzchar(tokens)]
}

city_from_place <- function(place) {
  tokens <- split_birth_place_tokens(place)
  if (!length(tokens)) return("")
  state_tokens <- normalize_state_vec(tokens)
  province_tokens <- vapply(tokens, normalize_canada_province, character(1))
  country_tokens <- vapply(tokens, normalize_country, character(1))
  state_idx <- which(!is.na(state_tokens))
  province_idx <- which(!is.na(province_tokens) & nzchar(province_tokens))
  country_idx <- which(!is.na(country_tokens) & nzchar(country_tokens))
  cut_idx <- min(c(state_idx, province_idx, country_idx, length(tokens) + 1L))
  if (cut_idx == length(tokens) + 1L && length(tokens) == 1L) return("")
  if (cut_idx <= 1L) return("")
  city <- paste(tokens[seq_len(cut_idx - 1L)], collapse = ", ")
  if (!nzchar(city) || nchar(city) > 70L) "" else city
}

state_from_place <- function(place) {
  tokens <- split_birth_place_tokens(place)
  if (!length(tokens)) return("")
  state_tokens <- normalize_state_vec(tokens)
  state_idx <- which(!is.na(state_tokens))
  if (length(state_idx)) return(state_tokens[[tail(state_idx, 1L)]])
  province_tokens <- vapply(tokens, normalize_canada_province, character(1))
  province_idx <- which(!is.na(province_tokens) & nzchar(province_tokens))
  if (length(province_idx)) return(province_tokens[[tail(province_idx, 1L)]])
  ""
}

compatible_text <- function(actual, expected) {
  actual_key <- norm_key(actual)
  expected_key <- norm_key(expected)
  if (!nzchar(actual_key) && !nzchar(expected_key)) return(TRUE)
  if (!nzchar(actual_key) || !nzchar(expected_key)) return(FALSE)
  actual_key == expected_key ||
    str_detect(actual_key, fixed(expected_key)) ||
    str_detect(expected_key, fixed(actual_key))
}

classify_text_column <- function(actual, expected, raw_text) {
  actual <- normalize_text(actual)
  expected <- normalize_text(expected)
  if (nzchar(expected)) {
    if (!nzchar(actual)) {
      if (raw_looks_corrupt(raw_text)) return("unclear_raw")
      return("incorrect")
    }
    if (compatible_text(actual, expected)) return("correct")
    if (raw_looks_corrupt(raw_text)) return("unclear_raw")
    return("incorrect")
  }
  if (nzchar(actual)) return("unclear_raw")
  if (raw_looks_corrupt(raw_text)) return("unclear_raw")
  "not_present_in_raw"
}

classify_year_column <- function(actual_year, expected_date, raw_text) {
  expected_year <- parse_year_from_date(expected_date)
  actual_year <- suppressWarnings(as.integer(actual_year))
  if (!is.na(expected_year)) {
    if (is.na(actual_year)) return("incorrect")
    if (identical(actual_year, expected_year)) return("correct")
    return("incorrect")
  }
  if (!is.na(actual_year)) return("unclear_raw")
  if (raw_looks_corrupt(raw_text)) return("unclear_raw")
  "not_present_in_raw"
}

audit_one <- function(row, text_col) {
  raw_text <- normalize_text(row[[text_col]][[1]])
  raw_birth <- extract_raw_birth(raw_text)
  expected_city <- city_from_place(raw_birth$expected_birth_place[[1]])
  expected_state <- state_from_place(raw_birth$expected_birth_place[[1]])
  expected_year <- parse_year_from_date(raw_birth$expected_birth_date[[1]])

  audit_birth_city <- classify_text_column(
    row$birth_city[[1]], expected_city, raw_text
  )
  audit_birth_state <- classify_text_column(
    row$birth_state[[1]], expected_state, raw_text
  )
  audit_birth_year <- classify_year_column(
    row$birth_year[[1]], raw_birth$expected_birth_date[[1]], raw_text
  )

  bad_cols <- c(
    if (audit_birth_city == "incorrect") "birth_city",
    if (audit_birth_state == "incorrect") "birth_state",
    if (audit_birth_year == "incorrect") "birth_year"
  )
  audit_entry <- if (length(bad_cols)) "incorrect" else "correct"
  note <- if (length(bad_cols)) {
    paste("incorrect:", paste(bad_cols, collapse = ","))
  } else if (raw_looks_corrupt(raw_text)) {
    paste0(text_col, " is noisy but no recoverable city/state/year error was detected")
  } else {
    "no city/state/year issue detected"
  }

  tibble(
    expected_birth_place = raw_birth$expected_birth_place[[1]],
    expected_birth_date = raw_birth$expected_birth_date[[1]],
    expected_birth_year = expected_year,
    expected_birth_city = expected_city,
    expected_birth_state = expected_state,
    raw_birth_rule = raw_birth$raw_birth_rule[[1]],
    audit_birth_city = audit_birth_city,
    audit_birth_state = audit_birth_state,
    audit_birth_year = audit_birth_year,
    audit_entry = audit_entry,
    audit_note = note
  )
}

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
input_file <- env_chr(
  "AMWS_LOCATION_YEAR_AUDIT_INPUT_FILE",
  file.path(default_output_dir,
            "amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv")
)
output_dir <- env_chr("AMWS_LOCATION_YEAR_AUDIT_OUTPUT_DIR",
                      default_output_dir)
output_stem <- env_chr("AMWS_LOCATION_YEAR_AUDIT_OUTPUT_STEM",
                       "audit_1000_birth_location_year")
audit_text_col <- env_chr("AMWS_LOCATION_YEAR_AUDIT_TEXT_COL",
                          "raw_text_adjusted")
sample_n <- env_int("AMWS_LOCATION_YEAR_AUDIT_SAMPLE_N", 1000L)
seed <- env_int("AMWS_LOCATION_YEAR_AUDIT_SEED", 20260707L)

if (basename(output_stem) != output_stem) {
  stop("AMWS_LOCATION_YEAR_AUDIT_OUTPUT_STEM must be a file stem.")
}

input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input <- read_csv(input_file, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c("doc_id", "lineid", "raw_text", "birth_place",
                   "birth_city", "birth_state", "birth_year")
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input table is missing required columns: ",
       paste(missing_cols, collapse = ", "))
}
if (!audit_text_col %in% names(input)) {
  stop("AMWS_LOCATION_YEAR_AUDIT_TEXT_COL is not present in input: ",
       audit_text_col)
}

if (sample_n > nrow(input)) sample_n <- nrow(input)
set.seed(seed)
sample_idx <- sample.int(nrow(input), sample_n, replace = FALSE)

sampled <- input[sample_idx, ] |>
  arrange(doc_id, suppressWarnings(as.integer(lineid))) |>
  mutate(audit_id = row_number(),
         audit_text_col = .env$audit_text_col,
         .before = 1)

audited_bits <- bind_rows(lapply(seq_len(nrow(sampled)), function(i) {
  audit_one(sampled[i, ], audit_text_col)
}))

audited <- bind_cols(sampled, audited_bits) |>
  select(
    audit_id, audit_text_col, doc_id, lineid,
    any_of(c("source_lineid", "entry_instance", "lineid_original_key")),
    raw_text, any_of("raw_text_adjusted"),
    birth_place, birth_city, birth_state, birth_country, birth_year,
    expected_birth_place, expected_birth_date, expected_birth_year,
    expected_birth_city, expected_birth_state,
    audit_birth_city, audit_birth_state, audit_birth_year, audit_entry,
    raw_birth_rule, audit_note,
    everything()
  )

summary_rows <- bind_rows(
  tibble(metric = "sample_n", value = nrow(audited)),
  tibble(metric = "unique_doc_lineid",
         value = n_distinct(paste(audited$doc_id, audited$lineid))),
  tibble(metric = "city_state_year_problem_n",
         value = sum(audited$audit_entry == "incorrect")),
  tibble(metric = "city_state_year_problem_pct",
         value = 100 * mean(audited$audit_entry == "incorrect")),
  tibble(metric = "birth_city_incorrect_n",
         value = sum(audited$audit_birth_city == "incorrect")),
  tibble(metric = "birth_state_incorrect_n",
         value = sum(audited$audit_birth_state == "incorrect")),
  tibble(metric = "birth_year_incorrect_n",
         value = sum(audited$audit_birth_year == "incorrect")),
  tibble(metric = "all_three_correct_n",
         value = sum(audited$audit_birth_city == "correct" &
                       audited$audit_birth_state == "correct" &
                       audited$audit_birth_year == "correct")),
  tibble(metric = "birth_city_adequate_n",
         value = sum(audited$audit_birth_city %in%
                       c("correct", "not_present_in_raw"))),
  tibble(metric = "birth_state_adequate_n",
         value = sum(audited$audit_birth_state %in%
                       c("correct", "not_present_in_raw"))),
  tibble(metric = "birth_year_adequate_n",
         value = sum(audited$audit_birth_year %in%
                       c("correct", "not_present_in_raw"))),
  tibble(metric = "all_three_adequate_n",
         value = sum(audited$audit_birth_city %in%
                       c("correct", "not_present_in_raw") &
                       audited$audit_birth_state %in%
                         c("correct", "not_present_in_raw") &
                       audited$audit_birth_year %in%
                         c("correct", "not_present_in_raw")))
)

by_column <- bind_rows(
  audited |> count(audit_birth_city, name = "n") |>
    transmute(metric = paste0("audit_birth_city:", audit_birth_city),
              value = n),
  audited |> count(audit_birth_state, name = "n") |>
    transmute(metric = paste0("audit_birth_state:", audit_birth_state),
              value = n),
  audited |> count(audit_birth_year, name = "n") |>
    transmute(metric = paste0("audit_birth_year:", audit_birth_year),
              value = n)
)

summary <- bind_rows(summary_rows, by_column) |>
  mutate(value = as.numeric(value))

sample_csv <- file.path(output_dir, paste0(output_stem, ".csv"))
sample_xlsx <- file.path(output_dir, paste0(output_stem, ".xlsx"))
summary_csv <- file.path(output_dir, paste0(output_stem, "_summary.csv"))

readr::write_excel_csv(audited, sample_csv, na = "")
writexl::write_xlsx(list(audit = audited, summary = summary), sample_xlsx)
readr::write_excel_csv(summary, summary_csv, na = "")

cat("Input:", input_file, "\n")
cat("Text column:", audit_text_col, "\n")
cat("Output stem:", output_stem, "\n")
cat("Seed:", seed, "\n")
cat("Rows sampled:", nrow(audited), "\n")
cat("City/state/year problems:", sum(audited$audit_entry == "incorrect"), "\n")
cat("Wrote audit CSV:", sample_csv, "\n")
cat("Wrote audit XLSX:", sample_xlsx, "\n")
cat("Wrote summary:", summary_csv, "\n")
