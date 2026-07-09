###############################################################################
# Apply all curated AMWS Ed16 expanded birth-place corrections.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded.csv
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_corrections.csv
#     amws_ed16_expanded_birth_place_manual_corrections.csv
#
# Writes:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv
#     amws_ed16_expanded_all_corrections_applied_log.csv
#     amws_ed16_expanded_all_corrections_summary.csv
#
# Environment overrides:
#   AMWS_ED16_ALL_CORRECTIONS_INPUT_FILE
#   AMWS_ED16_ALL_CORRECTIONS_OUTPUT_DIR
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."),
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

normalize_ocr_digits <- function(x) {
  chartr("OoIiLlSsZzBb|", "0011115522881", x)
}

amws_century_year <- function(yy) {
  yy <- as.integer(yy)
  ifelse(yy <= 86L, 1900L + yy, 1800L + yy)
}

parse_year_from_date <- function(birth_date) {
  x <- normalize_text(birth_date)
  if (!nzchar(x)) return("")
  token <- str_match(x, "([0-9OoIiLlSsZzBb|]{2,4})[A-Za-z]?$")[, 2]
  if (is.na(token) || !nzchar(token)) return("")
  normalized <- normalize_ocr_digits(token)
  if (!str_detect(normalized, "^[0-9]+$")) return("")
  if (nchar(normalized) == 2L) {
    year <- amws_century_year(normalized)
  } else if (nchar(normalized) == 4L) {
    year <- as.integer(normalized)
  } else {
    return("")
  }
  if (is.na(year) || year < 1800L || year > 1986L) "" else as.character(year)
}

city_from_place <- function(place) {
  place <- normalize_text(place)
  if (!nzchar(place)) return("")
  if (str_detect(place, ",")) {
    city <- str_split_fixed(place, ",", 2)[, 1]
  } else if (str_detect(place, "\\.\\s*[A-Za-z0-9' ]{1,20}$")) {
    city <- str_split_fixed(place, "\\.", 2)[, 1]
  } else {
    city <- place
  }
  normalize_text(city)
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

has_city_format_symbol <- function(x) {
  x <- normalize_text(x)
  ascii_symbol <- str_detect(
    x,
    "[[:cntrl:]<>\\^_*=+/\\\\|\\[\\]{}#@&%$~;:!?()]"
  )
  unicode_symbol <- str_detect(x, fixed(intToUtf8(0xFFFD))) |
    str_detect(x, fixed(intToUtf8(0x00C3))) |
    str_detect(x, fixed(intToUtf8(0x00C2))) |
    str_detect(x, fixed(intToUtf8(0x00AE))) |
    str_detect(x, fixed(intToUtf8(0x2122))) |
    str_detect(x, fixed(intToUtf8(0x00A9))) |
    str_detect(x, fixed(intToUtf8(0x2022)))
  nzchar(x) & (ascii_symbol | unicode_symbol)
}

birth_location_format_reasons <- function(birth_city, birth_state, birth_year) {
  city <- normalize_text(birth_city)
  state <- normalize_text(birth_state)
  year <- normalize_text(birth_year)
  year_int <- suppressWarnings(as.integer(year))

  city_bad_symbol <- has_city_format_symbol(city)
  city_has_digit <- nzchar(city) & str_detect(city, "[0-9]")
  city_has_separator_debris <- nzchar(city) & str_detect(city, "[,.]")
  city_has_section_text <- nzchar(city) & str_detect(
    city,
    regex(
      "\\b(Educ|Edue|Prof|Univ|Dept|Chemistry|Physics|Biology|Engineering|Medicine|Research|PHARMACEUTICS|SCIENCE|SYSTEM|SYSTEMS|MATH|MATHEMATICS|BIOCHEMISTRY|GENETICS|GEOLOGY|PATHOLOGY)\\b",
      ignore_case = TRUE
    )
  )
  city_has_date_marker <- nzchar(city) & str_detect(
    city,
    regex(
      "\\b(Jan|Feb|Mar|Apr|May|Jun|June|Jul|July|Aug|Sep|Sept|Oct|Nov|Dec|Dee|Dcc|Oet|Mav|Julv|Juiy)\\b",
      ignore_case = TRUE
    )
  )
  city_too_long <- nchar(city) > 45L
  city_bad_edge <- nzchar(city) &
    city != str_replace_all(city, "^[[:punct:] ]+|[[:punct:] ]+$", "")
  state_malformed <- nzchar(state) & !str_detect(state, "^[A-Z]{2}$")
  year_malformed <- nzchar(year) &
    (!str_detect(year, "^[0-9]{4}$") |
       is.na(year_int) |
       year_int < 1800L |
       year_int > 1986L)

  vapply(seq_along(city), function(i) {
    reasons <- c(
      if (city_bad_symbol[[i]]) "birth_city_symbols",
      if (city_has_digit[[i]]) "birth_city_digits",
      if (city_has_separator_debris[[i]]) "birth_city_punct_or_separator",
      if (city_has_section_text[[i]]) "birth_city_section_text",
      if (city_has_date_marker[[i]]) "birth_city_date_text",
      if (city_too_long[[i]]) "birth_city_too_long",
      if (city_bad_edge[[i]]) "birth_city_edge_punct",
      if (state_malformed[[i]]) "birth_state_not_clean_code",
      if (year_malformed[[i]]) "birth_year_not_plausible_4digit"
    )
    paste(reasons, collapse = "; ")
  }, character(1))
}

add_birth_location_format_flags <- function(data) {
  reasons <- birth_location_format_reasons(
    data$birth_city,
    data$birth_state,
    data$birth_year
  )
  data |>
    mutate(
      birth_location_format_problem = nzchar(reasons),
      birth_location_format_problem_reason = reasons
    )
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

is_safe_location_token <- function(x) {
  x <- strip_location_punct(x)
  nzchar(x) &&
    nchar(x) <= 35L &&
    !str_detect(x, "[0-9:;*^<>\\\\|]|\\b(Educ|Prof|Univ|Dept|Mailing|Res|Mem)\\b")
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
  tokens[nzchar(tokens)]
}

parse_birth_location <- function(place) {
  tokens <- split_birth_place_tokens(place)
  if (!length(tokens)) {
    return(tibble(birth_state = "", birth_country = "",
                  birth_location_parse_flag = "missing_place"))
  }

  state_tokens <- normalize_state_vec(tokens)
  state_idx <- which(!is.na(state_tokens))
  if (length(state_idx)) {
    j <- tail(state_idx, 1L)
    if (j == length(tokens) ||
        all(str_detect(tokens[(j + 1L):length(tokens)], "^[0-9]{2,4}$"))) {
      return(tibble(birth_state = state_tokens[[j]], birth_country = "USA",
                    birth_location_parse_flag = "us_state"))
    }
  }

  province_tokens <- vapply(tokens, normalize_canada_province, character(1))
  province_idx <- which(!is.na(province_tokens) & nzchar(province_tokens))
  country_tokens <- vapply(tokens, normalize_country, character(1))

  if (length(province_idx)) {
    j <- tail(province_idx, 1L)
    has_canada <- any(country_tokens == "Canada", na.rm = TRUE)
    if (j == length(tokens) || has_canada) {
      return(tibble(birth_state = province_tokens[[j]],
                    birth_country = "Canada",
                    birth_location_parse_flag = "canada_province"))
    }
  }

  country_idx <- which(!is.na(country_tokens) & nzchar(country_tokens))
  if (length(country_idx)) {
    j <- tail(country_idx, 1L)
    flag <- if (length(tokens) == 1L) "country_only" else "foreign_country"
    return(tibble(birth_state = "", birth_country = country_tokens[[j]],
                  birth_location_parse_flag = flag))
  }

  last_token <- tail(tokens, 1L)
  if (is_safe_location_token(last_token) && length(tokens) > 1L) {
    return(tibble(birth_state = "", birth_country = last_token,
                  birth_location_parse_flag = "foreign_country"))
  }

  tibble(birth_state = "", birth_country = "",
         birth_location_parse_flag = "unparsed")
}

month_regex <- paste(
  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "June", "Jul", "July",
    "Aug", "Sep", "Sept", "Oct", "Nov", "Dec", "Dee", "Dcc", "Oet",
    "Jon", "Mav", "N°v"),
  collapse = "|"
)

normalize_month <- function(x) {
  x |>
    str_replace_all("\\bDee\\b", "Dec") |>
    str_replace_all("\\bDcc\\b", "Dec") |>
    str_replace_all("\\bOet\\b", "Oct") |>
    str_replace_all("\\bJon\\b", "Jan") |>
    str_replace_all("\\bMav\\b", "May") |>
    str_replace_all("N°v", "Nov")
}

clean_place <- function(x) {
  x <- normalize_month(normalize_text(x))
  x <- str_replace_all(x, "[;:]+$", "")
  x <- str_replace_all(x, "\\s*\\.\\s*", ", ")
  x <- str_replace_all(x, "\\s*,\\s*", ", ")
  x <- str_replace_all(x, "\\bW'is\\b", "Wis")
  x <- str_replace_all(x, "(?<=, )111\\b", "Ill")
  x <- str_replace_all(x, ",+$", "")
  normalize_text(x)
}

strip_leading_markers <- function(x) {
  x <- normalize_text(x)
  old <- "__not_same__"
  patterns <- c(
    "^(?:US citizen|nat US|Can citizen|Br citizen)\\b\\s*[.;:]?\\s*",
    "^(?:div|wid|single)\\b\\s*[.;:]?\\s*",
    "^(?:m|in|:n)\\s*[0-9A-Za-z, ]{0,18}[.;:]\\s*",
    "^c\\s*[0-9Il|]{1,3}[.;:]?\\s*"
  )
  while (!identical(x, old)) {
    old <- x
    for (pattern in patterns) {
      x <- str_replace(x, pattern, "") |> normalize_text()
    }
  }
  x
}

valid_day <- function(x) {
  day <- suppressWarnings(as.integer(normalize_ocr_digits(x)))
  !is.na(day) && day >= 1L && day <= 31L
}

valid_year_token <- function(x) {
  if (is.na(x) || !nzchar(x)) return(FALSE)
  str_detect(normalize_ocr_digits(x), "^[0-9]{2}$|^[0-9]{4}$")
}

valid_place <- function(place) {
  place <- normalize_text(place)
  has_place_shape <- str_detect(
    place,
    "^[A-Za-z][A-Za-z .,'’()-]*(, [A-Za-z][A-Za-z .,'’()-]*)+$"
  )
  has_blocked_residue <- str_detect(
    place,
    paste0(
      "[0-9:;*^<>\\\\|]|\\b(",
      paste(c("US", "citizen", "O'tizen", "Prnf", "Prof", "rxp", "CHEM",
              "BIOLOGY", "PHYS", "SCIENCE", "ENGINEERING", "MATHEMATICS",
              "GENETICS", "NUTRITION", "SPACE", "Educ", "Mailing", "Dept"),
            collapse = "|"),
      ")\\b"
    )
  )
  has_place_shape && !has_blocked_residue && nchar(place) <= 65L
}

normalize_place_match_key <- function(x) {
  normalize_text(x) |>
    str_replace_all("\\b(111|III)\\b", "Ill") |>
    str_replace_all("\\s*\\.\\s*", ", ") |>
    str_replace_all("[^A-Za-z0-9]+", " ") |>
    str_to_lower() |>
    normalize_text()
}

place_matches_old_parse <- function(old_place, new_place) {
  old_key <- normalize_place_match_key(old_place)
  new_key <- normalize_place_match_key(new_place)
  old_without_name_prefix <- str_replace(
    old_key,
    "^[a-z][a-z ]{0,30}(?: jr| sr)? b ",
    ""
  )
  nzchar(old_key) &&
    nzchar(new_key) &&
    (str_starts(old_key, fixed(new_key)) ||
       str_starts(old_without_name_prefix, fixed(new_key)))
}

clean_field <- function(x) {
  normalize_text(x) |>
    str_replace_all("[;:'’‘`]+$", "") |>
    str_replace_all("\\s*\\.\\s*", ", ") |>
    str_replace_all("\\s*,\\s*", ", ") |>
    str_replace_all("[, ]+$", "") |>
    normalize_text()
}

valid_field <- function(field) {
  field <- normalize_text(field)
  nzchar(field) &&
    str_detect(field, "[A-Z]{3}") &&
    nchar(field) <= 100L &&
    !str_detect(
      field,
      "[;:]|\\b(Univ|College|Dept|Mailing|Address|citizen)\\b|\\bm\\s*[0-9]|\\bc\\s*[0-9]"
    )
}

extract_strict_regex_candidate <- function(raw_text_adjusted) {
  raw <- normalize_text(raw_text_adjusted)
  birth_match <- str_match(
    raw,
    "(?:^|[ ,.;])b\\s*([^;]{2,140});\\s*(.*)$"
  )
  if (is.na(birth_match[1, 2])) {
    return(tibble(
      regex_apply = FALSE,
      birth_place_new = "",
      birth_date_new = "",
      birth_year_new = "",
      birth_city_new = "",
      field_new = "",
      regex_rule_id = "no_birth_semicolon",
      regex_note = "No clear b ... ; birth segment."
    ))
  }

  birth_segment <- normalize_month(normalize_text(birth_match[1, 2]))
  remainder <- normalize_text(birth_match[1, 3])
  date_attempted <- str_detect(birth_segment, paste0("\\b(", month_regex, ")\\b"))
  date_match <- str_match(
    birth_segment,
    paste0(
      "^(.*?)[, .]+\\b(", month_regex,
      ")\\.?\\s*([A-Za-z0-9°'’|]{1,4})(?:[,. ]+([A-Za-z0-9|]{2,4}))?\\.?$"
    )
  )

  birth_date_new <- ""
  if (!is.na(date_match[1, 2]) &&
      valid_day(date_match[1, 4]) &&
      valid_year_token(date_match[1, 5])) {
    birth_place_new <- clean_place(date_match[1, 2])
    month <- normalize_month(date_match[1, 3])
    day <- as.integer(normalize_ocr_digits(date_match[1, 4]))
    year_token <- normalize_ocr_digits(date_match[1, 5])
    birth_date_new <- paste0(month, " ", day, ", ", year_token)
  } else {
    birth_place_new <- clean_place(birth_segment)
  }

  cleaned_remainder <- strip_leading_markers(remainder)
  field_match <- str_match(
    cleaned_remainder,
    "^([A-Z][A-Z0-9 &/,()'\\-.]{2,120}?)(?:\\s+Educ\\b|\\s+Edue\\b|\\s+Educ:|\\s+Edue:|\\s+Prof Exp\\b|\\s+Mem\\b)"
  )
  field_new <- if (is.na(field_match[1, 2])) "" else clean_field(field_match[1, 2])
  birth_year_new <- parse_year_from_date(birth_date_new)
  birth_city_new <- city_from_place(birth_place_new)

  bad_date <- date_attempted && !nzchar(birth_date_new)
  regex_apply <- valid_place(birth_place_new) &&
    valid_field(field_new) &&
    !bad_date

  note <- if (regex_apply) {
    "High-precision rule: b birth segment before semicolon; field before Educ/section header."
  } else if (bad_date) {
    "Skipped: month-like date residue is present but not safely parseable."
  } else if (!valid_place(birth_place_new)) {
    "Skipped: extracted birthplace did not pass conservative place-shape checks."
  } else if (!valid_field(field_new)) {
    "Skipped: extracted field did not pass conservative field checks."
  } else {
    "Skipped by conservative rule."
  }

  tibble(
    regex_apply = regex_apply,
    birth_place_new = if (regex_apply) birth_place_new else "",
    birth_date_new = if (regex_apply) birth_date_new else "",
    birth_year_new = if (regex_apply) birth_year_new else "",
    birth_city_new = if (regex_apply) birth_city_new else "",
    field_new = if (regex_apply) field_new else "",
    regex_rule_id = "b_semicolon_field_before_educ_strict",
    regex_note = note
  )
}

correction_cols <- c("birth_place", "birth_date", "birth_year", "birth_city",
                     "field")

make_correction_rows <- function(data, source, priority, rule_col,
                                 confidence_col, note_col) {
  data |>
    transmute(
      doc_id, lineid,
      correction_source = source,
      correction_priority = priority,
      correction_rule = .data[[rule_col]],
      correction_confidence = .data[[confidence_col]],
      correction_note = .data[[note_col]],
      birth_place_new, birth_date_new, birth_year_new, birth_city_new, field_new
    )
}

apply_corrections <- function(input, corrections) {
  corrected <- input
  if (!nrow(corrections)) return(corrected)
  idx <- match(paste(corrections$doc_id, corrections$lineid),
               paste(corrected$doc_id, corrected$lineid))
  corrected$birth_place[idx] <- corrections$birth_place_new
  corrected$birth_date[idx] <- corrections$birth_date_new
  corrected$birth_year[idx] <- corrections$birth_year_new
  corrected$birth_city[idx] <- corrections$birth_city_new
  corrected$field[idx] <- corrections$field_new
  corrected
}

csv_text_cols <- cols(.default = col_character())

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_ALL_CORRECTIONS_OUTPUT_DIR",
                      default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input_file <- env_chr(
  "AMWS_ED16_ALL_CORRECTIONS_INPUT_FILE",
  file.path(output_dir,
            "amws_ed16_entries_regex_parsed_birth_year_city_expanded.csv")
)
input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

old_regex_csv <- file.path(
  output_dir,
  "amws_ed16_entries_regex_parsed_birth_year_city_expanded_corrections.csv"
)
manual_csv <- file.path(output_dir,
                        "amws_ed16_expanded_birth_place_manual_corrections.csv")
corrected_csv <- file.path(
  output_dir,
  "amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv"
)
applied_log_csv <- file.path(output_dir,
                             "amws_ed16_expanded_all_corrections_applied_log.csv")
strict_regex_log_csv <- file.path(
  output_dir,
  "amws_ed16_expanded_all_corrections_strict_regex_candidates.csv"
)
summary_csv <- file.path(output_dir,
                         "amws_ed16_expanded_all_corrections_summary.csv")

input <- read_csv(input_file, col_types = csv_text_cols, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
old_regex <- read_csv(old_regex_csv, col_types = csv_text_cols,
                      show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
manual <- read_csv(manual_csv, col_types = csv_text_cols,
                   show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_input_cols <- c("doc_id", "lineid", correction_cols)
missing_input <- setdiff(required_input_cols, names(input))
if (length(missing_input)) {
  stop("Expanded input is missing required columns: ",
       paste(missing_input, collapse = ", "))
}

if (n_distinct(paste(input$doc_id, input$lineid)) != nrow(input)) {
  stop("Expanded input has duplicated doc_id + lineid.")
}

required_old_cols <- c(
  "doc_id", "lineid", "correction_rule", "correction_confidence",
  "correction_note", "birth_place_new", "birth_date_new", "birth_year_new",
  "birth_city_new", "field_new"
)
missing_old <- setdiff(required_old_cols, names(old_regex))
if (length(missing_old)) {
  stop("Old regex corrections are missing required columns: ",
       paste(missing_old, collapse = ", "))
}

required_manual_cols <- c(
  "doc_id", "lineid", "birth_place_new", "birth_date_new",
  "birth_year_new", "birth_city_new", "field_new", "manual_action",
  "manual_confidence", "manual_note", "raw_text_adjusted",
  "birth_place_old", "birth_date_old", "birth_year_old", "birth_city_old",
  "field_old"
)
missing_manual <- setdiff(required_manual_cols, names(manual))
if (length(missing_manual)) {
  stop("Manual corrections table is missing required columns: ",
       paste(missing_manual, collapse = ", "))
}

dup_old <- old_regex |> count(doc_id, lineid) |> filter(n > 1L)
if (nrow(dup_old)) {
  stop("Old regex corrections have duplicated doc_id + lineid: ", nrow(dup_old))
}

dup_manual <- manual |> count(doc_id, lineid) |> filter(n > 1L)
if (nrow(dup_manual)) {
  stop("Manual corrections table has duplicated doc_id + lineid: ", nrow(dup_manual))
}

old_regex_rows <- old_regex |>
  filter(correction_confidence %in% c("high", "medium")) |>
  make_correction_rows(
    source = "old_regex_corrections",
    priority = 10L,
    rule_col = "correction_rule",
    confidence_col = "correction_confidence",
    note_col = "correction_note"
  )

manual_rows <- manual |>
  filter(manual_action == "correct",
         manual_confidence %in% c("high", "medium")) |>
  make_correction_rows(
    source = "manual_dee_batches",
    priority = 30L,
    rule_col = "manual_action",
    confidence_col = "manual_confidence",
    note_col = "manual_note"
  )

pending <- manual |>
  filter(manual_action == "review_pending") |>
  arrange(doc_id, suppressWarnings(as.integer(lineid)))

strict_regex_candidates <- bind_cols(
  pending |>
    select(doc_id, lineid, birth_place_old, birth_date_old, birth_year_old,
           birth_city_old, field_old, raw_text_adjusted),
  bind_rows(lapply(pending$raw_text_adjusted, extract_strict_regex_candidate))
) |>
  mutate(
    place_matches_old = mapply(place_matches_old_parse,
                               birth_place_old, birth_place_new),
    regex_apply = regex_apply & place_matches_old,
    birth_place_new = ifelse(regex_apply, birth_place_new, ""),
    birth_date_new = ifelse(regex_apply, birth_date_new, ""),
    birth_year_new = ifelse(regex_apply, birth_year_new, ""),
    birth_city_new = ifelse(regex_apply, birth_city_new, ""),
    field_new = ifelse(regex_apply, field_new, ""),
    regex_note = ifelse(
      regex_apply | place_matches_old,
      regex_note,
      "Skipped: extracted birthplace does not match this row's old birth_place parse."
    ),
    regex_confidence = ifelse(regex_apply, "high", ""),
    birth_city_expected_from_place = vapply(birth_place_new, city_from_place,
                                            character(1))
  )

strict_regex_rows <- strict_regex_candidates |>
  filter(regex_apply) |>
  make_correction_rows(
    source = "strict_pending_regex",
    priority = 20L,
    rule_col = "regex_rule_id",
    confidence_col = "regex_confidence",
    note_col = "regex_note"
  )

candidate_rows <- bind_rows(old_regex_rows, strict_regex_rows, manual_rows)

unmatched <- anti_join(candidate_rows, input |> select(doc_id, lineid),
                       by = c("doc_id", "lineid"))
if (nrow(unmatched)) {
  stop("Corrections contain keys not present in expanded input: ", nrow(unmatched))
}

bad_empty_place <- candidate_rows |>
  filter(!nzchar(normalize_text(birth_place_new)))
if (nrow(bad_empty_place)) {
  stop("Applied correction candidates include empty birth_place_new rows: ",
       nrow(bad_empty_place))
}

effective_rows <- candidate_rows |>
  arrange(doc_id, lineid, desc(correction_priority), correction_source) |>
  group_by(doc_id, lineid) |>
  mutate(
    candidate_count_for_key = n(),
    overridden_candidate_count = n() - 1L
  ) |>
  slice(1L) |>
  ungroup()

applied_log <- effective_rows |>
  left_join(input |>
              select(doc_id, lineid,
                     birth_place_old_actual = birth_place,
                     birth_date_old_actual = birth_date,
                     birth_year_old_actual = birth_year,
                     birth_city_old_actual = birth_city,
                     field_old_actual = field),
            by = c("doc_id", "lineid")) |>
  mutate(
    changed_birth_place = normalize_text(birth_place_old_actual) !=
      normalize_text(birth_place_new),
    changed_birth_date = normalize_text(birth_date_old_actual) !=
      normalize_text(birth_date_new),
    changed_birth_year = normalize_text(birth_year_old_actual) !=
      normalize_text(birth_year_new),
    changed_birth_city = normalize_text(birth_city_old_actual) !=
      normalize_text(birth_city_new),
    changed_field = normalize_text(field_old_actual) != normalize_text(field_new)
  )

corrected <- apply_corrections(input, effective_rows)

location_bits <- bind_rows(lapply(corrected$birth_place, parse_birth_location))
corrected <- corrected |>
  select(-any_of(c("birth_state", "birth_country",
                   "birth_location_parse_flag",
                   "birth_location_format_problem",
                   "birth_location_format_problem_reason"))) |>
  bind_cols(location_bits) |>
  add_birth_location_format_flags() |>
  relocate(birth_state, birth_country, birth_location_parse_flag,
           birth_location_format_problem,
           birth_location_format_problem_reason,
           .after = birth_city)

if (nrow(corrected) != nrow(input)) {
  stop("Corrected row count changed.")
}
if (!identical(paste(corrected$doc_id, corrected$lineid),
               paste(input$doc_id, input$lineid))) {
  stop("Corrected output changed row order or keys.")
}
if (n_distinct(paste(corrected$doc_id, corrected$lineid)) != nrow(corrected)) {
  stop("Corrected output has duplicated doc_id + lineid.")
}

format_problem_reason_values <- corrected$birth_location_format_problem_reason[
  corrected$birth_location_format_problem
]
format_problem_reason_counts <- if (length(format_problem_reason_values)) {
  reasons <- unlist(strsplit(format_problem_reason_values, "; ", fixed = TRUE),
                    use.names = FALSE)
  as_tibble(sort(table(reasons), decreasing = TRUE), .name_repair = "minimal") |>
    transmute(metric = paste0("birth_location_format_problem_reason:", reasons),
              value = as.numeric(n))
} else {
  tibble(metric = character(), value = numeric())
}

summary <- bind_rows(
  tibble(metric = "input_rows", value = nrow(input)),
  tibble(metric = "old_regex_candidate_rows", value = nrow(old_regex_rows)),
  tibble(metric = "manual_candidate_rows", value = nrow(manual_rows)),
  tibble(metric = "strict_pending_regex_candidate_rows",
         value = nrow(strict_regex_rows)),
  tibble(metric = "candidate_rows_total", value = nrow(candidate_rows)),
  tibble(metric = "applied_unique_rows", value = nrow(effective_rows)),
  tibble(metric = "overridden_candidate_rows",
         value = sum(applied_log$overridden_candidate_count)),
  effective_rows |>
    count(correction_source, name = "value") |>
    transmute(metric = paste0("applied_source:", correction_source), value),
  candidate_rows |>
    count(correction_source, name = "value") |>
    transmute(metric = paste0("candidate_source:", correction_source), value),
  tibble(metric = "changed_birth_place",
         value = sum(applied_log$changed_birth_place)),
  tibble(metric = "changed_birth_date",
         value = sum(applied_log$changed_birth_date)),
  tibble(metric = "changed_birth_year",
         value = sum(applied_log$changed_birth_year)),
  tibble(metric = "changed_birth_city",
         value = sum(applied_log$changed_birth_city)),
  tibble(metric = "changed_field",
         value = sum(applied_log$changed_field)),
  tibble(metric = "birth_state_nonempty",
         value = sum(nzchar(corrected$birth_state))),
  tibble(metric = "birth_country_nonempty",
         value = sum(nzchar(corrected$birth_country))),
  tibble(metric = "birth_location_format_problem",
         value = sum(corrected$birth_location_format_problem)),
  tibble(metric = "birth_city_format_problem",
         value = sum(str_detect(corrected$birth_location_format_problem_reason,
                                "birth_city_"))),
  tibble(metric = "birth_state_format_problem",
         value = sum(str_detect(corrected$birth_location_format_problem_reason,
                                "birth_state_"))),
  tibble(metric = "birth_year_format_problem",
         value = sum(str_detect(corrected$birth_location_format_problem_reason,
                                "birth_year_"))),
  format_problem_reason_counts,
  corrected |>
    count(birth_location_parse_flag, name = "value") |>
    transmute(metric = paste0("birth_location_parse_flag:",
                              birth_location_parse_flag),
              value),
  corrected |>
    filter(nzchar(birth_country)) |>
    count(birth_country, sort = TRUE, name = "value") |>
    slice_head(n = 30L) |>
    transmute(metric = paste0("top_birth_country:", birth_country), value),
  corrected |>
    filter(birth_country == "USA", nzchar(birth_state)) |>
    count(birth_state, sort = TRUE, name = "value") |>
    slice_head(n = 30L) |>
    transmute(metric = paste0("top_usa_birth_state:", birth_state), value)
) |>
  mutate(value = as.numeric(value))

write_excel_csv(strict_regex_candidates, strict_regex_log_csv, na = "")
write_excel_csv(corrected, corrected_csv, na = "")
write_excel_csv(applied_log, applied_log_csv, na = "")
write_excel_csv(summary, summary_csv, na = "")

cat("Expanded input:", input_file, "\n")
cat("Applied unique rows:", nrow(effective_rows), "\n")
cat("Wrote corrected output:", corrected_csv, "\n")
cat("Wrote applied log:", applied_log_csv, "\n")
cat("Wrote strict regex candidate log:", strict_regex_log_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
