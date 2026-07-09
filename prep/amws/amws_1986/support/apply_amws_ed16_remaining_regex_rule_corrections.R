###############################################################################
# Apply conservative regex-rule corrections to remaining AMWS Ed16 expanded rows.
#
# This script uses the manual-review diagnosis sample to target a narrow,
# reproducible class of remaining parsing errors:
#   - birth_place absorbed demographic/status/field text;
#   - the real birthplace is visible immediately after the AMWS birth marker;
#   - field text is visible before Educ/Edue/Prof Exp/Mem.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_manual_corrected.csv
#     amws_ed16_expanded_birth_place_manual_corrections.csv
#
# Writes:
#   output/amws/regex_all_docs/
#     amws_ed16_expanded_birth_place_regex_rule_corrections.csv
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_manual_regex_corrected.csv
#     amws_ed16_expanded_birth_place_regex_rule_apply_summary.csv
#
# Environment overrides:
#   AMWS_ED16_MANUAL_BP_OUTPUT_DIR
#   AMWS_ED16_REGEX_RULE_INPUT_FILE
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
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..", ".."),
                             winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "paths.R"))

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
  } else {
    city <- place
  }
  normalize_text(city)
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

extract_regex_candidate <- function(raw_text_adjusted) {
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

csv_text_cols <- cols(.default = col_character())

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_MANUAL_BP_OUTPUT_DIR", default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input_file <- env_chr(
  "AMWS_ED16_REGEX_RULE_INPUT_FILE",
  file.path(
    output_dir,
    "amws_ed16_entries_regex_parsed_birth_year_city_expanded_manual_corrected.csv"
  )
)
input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

manual_csv <- file.path(output_dir,
                        "amws_ed16_expanded_birth_place_manual_corrections.csv")
regex_log_csv <- file.path(
  output_dir,
  "amws_ed16_expanded_birth_place_regex_rule_corrections.csv"
)
corrected_csv <- file.path(
  output_dir,
  "amws_ed16_entries_regex_parsed_birth_year_city_expanded_manual_regex_corrected.csv"
)
summary_csv <- file.path(
  output_dir,
  "amws_ed16_expanded_birth_place_regex_rule_apply_summary.csv"
)

input <- read_csv(input_file, col_types = csv_text_cols, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
manual <- read_csv(manual_csv, col_types = csv_text_cols, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_input_cols <- c("doc_id", "lineid", "birth_place", "birth_date",
                         "birth_year", "birth_city", "field")
missing_input <- setdiff(required_input_cols, names(input))
if (length(missing_input)) {
  stop("Input is missing required columns: ", paste(missing_input, collapse = ", "))
}

required_manual_cols <- c(
  "doc_id", "lineid", "source_lineid", "entry_instance",
  "manual_target_reason", "birth_place_word_n",
  "has_dee_date_in_birth_place", "birth_place_old", "birth_date_old",
  "birth_year_old", "birth_city_old", "field_old", "raw_text_adjusted",
  "manual_action"
)
missing_manual <- setdiff(required_manual_cols, names(manual))
if (length(missing_manual)) {
  stop("Manual corrections table is missing required columns: ",
       paste(missing_manual, collapse = ", "))
}

dup_manual <- manual |>
  count(doc_id, lineid) |>
  filter(n > 1L)
if (nrow(dup_manual)) {
  stop("Manual corrections table has duplicated doc_id + lineid: ", nrow(dup_manual))
}

pending <- manual |>
  filter(manual_action == "review_pending") |>
  arrange(doc_id, suppressWarnings(as.integer(lineid)))

regex_candidates <- bind_cols(
  pending |>
    select(doc_id, lineid, source_lineid, entry_instance, manual_target_reason,
           birth_place_word_n, has_dee_date_in_birth_place,
           birth_place_old, birth_date_old, birth_year_old, birth_city_old,
           field_old, raw_text_adjusted),
  bind_rows(lapply(pending$raw_text_adjusted, extract_regex_candidate))
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

to_apply <- regex_candidates |>
  filter(regex_apply)

if (nrow(to_apply)) {
  empty_place <- to_apply |> filter(!nzchar(normalize_text(birth_place_new)))
  if (nrow(empty_place)) {
    stop("Regex candidates marked for application include empty birthplace rows.")
  }

  city_mismatch <- to_apply |>
    filter(normalize_text(birth_city_new) != normalize_text(birth_city_expected_from_place))
  if (nrow(city_mismatch)) {
    stop("Regex candidates include birth_city values inconsistent with birth_place.")
  }

  year_mismatch <- to_apply |>
    mutate(parsed_year = vapply(birth_date_new, parse_year_from_date, character(1))) |>
    filter(nzchar(birth_date_new),
           normalize_text(parsed_year) != normalize_text(birth_year_new))
  if (nrow(year_mismatch)) {
    stop("Regex candidates include birth_year values inconsistent with birth_date.")
  }
}

unmatched <- anti_join(to_apply, input |> select(doc_id, lineid),
                       by = c("doc_id", "lineid"))
if (nrow(unmatched)) {
  stop("Regex corrections contain keys not present in input: ", nrow(unmatched))
}

corrected <- input
if (nrow(to_apply)) {
  idx <- match(paste(to_apply$doc_id, to_apply$lineid),
               paste(corrected$doc_id, corrected$lineid))
  corrected$birth_place[idx] <- to_apply$birth_place_new
  corrected$birth_date[idx] <- to_apply$birth_date_new
  corrected$birth_year[idx] <- to_apply$birth_year_new
  corrected$birth_city[idx] <- to_apply$birth_city_new
  corrected$field[idx] <- to_apply$field_new
}

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

summary <- bind_rows(
  tibble(metric = "input_rows", value = nrow(input)),
  tibble(metric = "manual_pending_rows", value = nrow(pending)),
  tibble(metric = "regex_candidate_rows", value = nrow(regex_candidates)),
  tibble(metric = "regex_applied_rows", value = nrow(to_apply)),
  regex_candidates |>
    count(regex_rule_id, regex_apply, name = "value") |>
    transmute(metric = paste0("rule:", regex_rule_id, ":apply:", regex_apply),
              value),
  tibble(metric = "changed_birth_place",
         value = sum(normalize_text(to_apply$birth_place_old) !=
                       normalize_text(to_apply$birth_place_new))),
  tibble(metric = "changed_birth_date",
         value = sum(normalize_text(to_apply$birth_date_old) !=
                       normalize_text(to_apply$birth_date_new))),
  tibble(metric = "changed_birth_year",
         value = sum(normalize_text(to_apply$birth_year_old) !=
                       normalize_text(to_apply$birth_year_new))),
  tibble(metric = "changed_birth_city",
         value = sum(normalize_text(to_apply$birth_city_old) !=
                       normalize_text(to_apply$birth_city_new))),
  tibble(metric = "changed_field",
         value = sum(normalize_text(to_apply$field_old) !=
                       normalize_text(to_apply$field_new)))
) |>
  mutate(value = as.numeric(value))

write_excel_csv(regex_candidates, regex_log_csv, na = "")
write_excel_csv(corrected, corrected_csv, na = "")
write_excel_csv(summary, summary_csv, na = "")

cat("Input:", input_file, "\n")
cat("Manual correction table:", manual_csv, "\n")
cat("Pending rows scanned:", nrow(pending), "\n")
cat("Regex-applied rows:", nrow(to_apply), "\n")
cat("Wrote regex candidate log:", regex_log_csv, "\n")
cat("Wrote corrected output:", corrected_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
