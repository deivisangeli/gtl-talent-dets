###############################################################################
# Apply explicit manual AMWS Ed16 expanded birth-place corrections.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded.csv
#     amws_ed16_expanded_birth_place_manual_corrections.csv
#
# Writes:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_manual_corrected.csv
#     amws_ed16_expanded_birth_place_manual_applied_log.csv
#     amws_ed16_expanded_birth_place_manual_apply_summary.csv
#
# Environment overrides:
#   AMWS_ED16_MANUAL_BP_INPUT_FILE
#   AMWS_ED16_MANUAL_BP_OUTPUT_DIR
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
  if (!nzchar(x)) return(NA_integer_)
  x <- str_replace(x, "[,;:.\\s]+$", "")
  token <- str_match(x, "([0-9OoIiLlSsZzBb|]{2,4})[A-Za-z]?$")[, 2]
  if (is.na(token) || !nzchar(token)) return(NA_integer_)
  normalized <- normalize_ocr_digits(token)
  if (!str_detect(normalized, "^[0-9]+$")) return(NA_integer_)
  if (nchar(normalized) == 2L) {
    year <- amws_century_year(normalized)
  } else if (nchar(normalized) == 4L) {
    year <- as.integer(normalized)
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

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
input_file <- env_chr(
  "AMWS_ED16_MANUAL_BP_INPUT_FILE",
  file.path(default_output_dir,
            "amws_ed16_entries_regex_parsed_birth_year_city_expanded.csv")
)
output_dir <- env_chr("AMWS_ED16_MANUAL_BP_OUTPUT_DIR", default_output_dir)

input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

manual_csv <- file.path(output_dir,
                        "amws_ed16_expanded_birth_place_manual_corrections.csv")
corrected_csv <- file.path(
  output_dir,
  "amws_ed16_entries_regex_parsed_birth_year_city_expanded_manual_corrected.csv"
)
applied_log_csv <- file.path(
  output_dir,
  "amws_ed16_expanded_birth_place_manual_applied_log.csv"
)
summary_csv <- file.path(
  output_dir,
  "amws_ed16_expanded_birth_place_manual_apply_summary.csv"
)

csv_text_cols <- cols(.default = col_character())

input <- read_csv(input_file, col_types = csv_text_cols, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
manual <- read_csv(manual_csv, col_types = csv_text_cols, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_input_cols <- c("doc_id", "lineid", "birth_place", "birth_date",
                         "birth_year", "birth_city", "field")
missing_input <- setdiff(required_input_cols, names(input))
if (length(missing_input)) {
  stop("Expanded input is missing required columns: ",
       paste(missing_input, collapse = ", "))
}

required_manual_cols <- c(
  "doc_id", "lineid", "birth_place_new", "birth_date_new",
  "birth_year_new", "birth_city_new", "field_new", "manual_action",
  "manual_confidence", "manual_note"
)
missing_manual <- setdiff(required_manual_cols, names(manual))
if (length(missing_manual)) {
  stop("Manual corrections table is missing required columns: ",
       paste(missing_manual, collapse = ", "))
}

allowed_actions <- c("review_pending", "correct", "review_unclear", "no_change")
bad_actions <- setdiff(unique(manual$manual_action), allowed_actions)
if (length(bad_actions)) {
  stop("Invalid manual_action values: ", paste(bad_actions, collapse = ", "))
}

allowed_conf <- c("", "high", "medium", "low")
bad_conf <- setdiff(unique(manual$manual_confidence), allowed_conf)
if (length(bad_conf)) {
  stop("Invalid manual_confidence values: ", paste(bad_conf, collapse = ", "))
}

dup_manual <- manual |>
  count(doc_id, lineid) |>
  filter(n > 1L)
if (nrow(dup_manual)) {
  stop("Manual corrections table has duplicated doc_id + lineid: ", nrow(dup_manual))
}

to_apply <- manual |>
  filter(manual_action == "correct",
         manual_confidence %in% c("high", "medium"))

if (nrow(to_apply)) {
  empty_place <- to_apply |> filter(!nzchar(normalize_text(birth_place_new)))
  if (nrow(empty_place)) {
    stop("Correct rows must have nonempty birth_place_new. Bad rows: ",
         nrow(empty_place))
  }

  year_check <- to_apply |>
    mutate(
      parsed_year = vapply(birth_date_new, parse_year_from_date, integer(1)),
      supplied_year = suppressWarnings(as.integer(birth_year_new)),
      year_mismatch = nzchar(birth_year_new) &
        !is.na(parsed_year) & supplied_year != parsed_year
    ) |>
    filter(year_mismatch)
  if (nrow(year_check)) {
    stop("birth_year_new is inconsistent with birth_date_new for ",
         nrow(year_check), " rows.")
  }
}

unmatched <- anti_join(to_apply, input |> select(doc_id, lineid),
                       by = c("doc_id", "lineid"))
if (nrow(unmatched)) {
  stop("Manual corrections contain keys not present in expanded input: ",
       nrow(unmatched))
}

corrected <- input
applied_log <- to_apply |>
  left_join(input |>
              select(doc_id, lineid,
                     birth_place_old_actual = birth_place,
                     birth_date_old_actual = birth_date,
                     birth_year_old_actual = birth_year,
                     birth_city_old_actual = birth_city,
                     field_old_actual = field),
            by = c("doc_id", "lineid")) |>
  mutate(
    birth_city_expected_from_place = vapply(birth_place_new, city_from_place,
                                            character(1)),
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
if (n_distinct(paste(corrected$doc_id, corrected$lineid)) != nrow(corrected)) {
  stop("Corrected output has duplicated doc_id + lineid.")
}
if (!identical(paste(corrected$doc_id, corrected$lineid),
               paste(input$doc_id, input$lineid))) {
  stop("Corrected output changed row order or keys.")
}

summary <- bind_rows(
  tibble(metric = "input_rows", value = nrow(input)),
  tibble(metric = "manual_rows", value = nrow(manual)),
  tibble(metric = "applied_rows", value = nrow(to_apply)),
  manual |> count(manual_action, name = "value") |>
    transmute(metric = paste0("manual_action:", manual_action), value),
  manual |> count(manual_confidence, name = "value") |>
    transmute(metric = paste0("manual_confidence:", manual_confidence), value),
  tibble(metric = "changed_birth_place",
         value = sum(applied_log$changed_birth_place)),
  tibble(metric = "changed_birth_date",
         value = sum(applied_log$changed_birth_date)),
  tibble(metric = "changed_birth_year",
         value = sum(applied_log$changed_birth_year)),
  tibble(metric = "changed_birth_city",
         value = sum(applied_log$changed_birth_city)),
  tibble(metric = "changed_field",
         value = sum(applied_log$changed_field))
) |>
  mutate(value = as.numeric(value))

readr::write_excel_csv(corrected, corrected_csv, na = "")
readr::write_excel_csv(applied_log, applied_log_csv, na = "")
readr::write_excel_csv(summary, summary_csv, na = "")

cat("Expanded input:", input_file, "\n")
cat("Manual corrections:", manual_csv, "\n")
cat("Applied rows:", nrow(to_apply), "\n")
cat("Wrote corrected output:", corrected_csv, "\n")
cat("Wrote applied log:", applied_log_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
