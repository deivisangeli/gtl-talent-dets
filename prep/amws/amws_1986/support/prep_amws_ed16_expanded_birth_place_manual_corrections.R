###############################################################################
# Prepare the manual-correction table for long/contaminated birth_place values
# in the AMWS edition 16 expanded regex output.
#
# This script only selects rows for manual curation. It does not change the
# expanded data. If the manual-corrections CSV already exists, existing manual
# columns are preserved and joined onto the freshly selected target rows.
#
# Inputs:
#   <Dropbox root>/output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded.csv
#
# Outputs:
#   <Dropbox root>/output/amws/regex_all_docs/
#     amws_ed16_expanded_birth_place_manual_corrections.csv
#     amws_ed16_expanded_birth_place_manual_selection_summary.csv
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
  DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  AMWS_INPUT <- file.path(DATA_INPUT, "amws")
  AMWS_OUTPUT <- file.path(DATA_OUTPUT, "amws")
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

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
input_file <- env_chr(
  "AMWS_ED16_MANUAL_BP_INPUT_FILE",
  file.path(default_output_dir,
            "amws_ed16_entries_regex_parsed_birth_year_city_expanded.csv")
)
output_dir <- env_chr("AMWS_ED16_MANUAL_BP_OUTPUT_DIR", default_output_dir)

input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

manual_csv <- file.path(output_dir,
                        "amws_ed16_expanded_birth_place_manual_corrections.csv")
summary_csv <- file.path(output_dir,
                         "amws_ed16_expanded_birth_place_manual_selection_summary.csv")

input <- read_csv(input_file, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c(
  "doc_id", "lineid", "source_lineid", "entry_instance",
  "raw_text", "raw_text_adjusted", "birth_place", "birth_date", "birth_year",
  "birth_city", "field"
)
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input table is missing required columns: ",
       paste(missing_cols, collapse = ", "))
}

selected <- input |>
  mutate(
    birth_place_old = normalize_text(birth_place),
    birth_place_word_n = str_count(birth_place_old, regex("[A-Za-z0-9]+")),
    has_dee_date_in_birth_place = str_detect(
      birth_place_old,
      regex("\\bDee\\.?\\s*[0-9]", ignore_case = FALSE)
    ),
    manual_target_reason = case_when(
      birth_place_word_n >= 4L & has_dee_date_in_birth_place ~
        "birth_place_4plus_words_and_dee_date",
      birth_place_word_n >= 4L ~
        "birth_place_4plus_words",
      birth_place_word_n >= 3L & has_dee_date_in_birth_place ~
        "birth_place_3plus_words_and_dee_date",
      TRUE ~ ""
    )
  ) |>
  filter(nzchar(birth_place_old), nzchar(manual_target_reason)) |>
  transmute(
    doc_id, lineid, source_lineid, entry_instance,
    manual_target_reason,
    birth_place_word_n,
    has_dee_date_in_birth_place,
    birth_place_old,
    birth_date_old = birth_date,
    birth_year_old = birth_year,
    birth_city_old = birth_city,
    field_old = field,
    birth_place_new = "",
    birth_date_new = "",
    birth_year_new = "",
    birth_city_new = "",
    field_new = "",
    manual_action = "review_pending",
    manual_confidence = "",
    manual_note = "",
    raw_text_adjusted,
    raw_text
  ) |>
  arrange(doc_id, as.integer(lineid))

if (anyDuplicated(paste(selected$doc_id, selected$lineid))) {
  stop("Selected manual target rows have duplicated doc_id + lineid.")
}

manual_cols <- c(
  "birth_place_new", "birth_date_new", "birth_year_new", "birth_city_new",
  "field_new", "manual_action", "manual_confidence", "manual_note"
)

if (file.exists(manual_csv)) {
  existing <- read_csv(manual_csv, show_col_types = FALSE) |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
  missing_existing <- setdiff(c("doc_id", "lineid", manual_cols), names(existing))
  if (length(missing_existing)) {
    stop("Existing manual corrections CSV is missing required columns: ",
         paste(missing_existing, collapse = ", "))
  }
  existing_manual <- existing |>
    select(doc_id, lineid, all_of(manual_cols)) |>
    distinct(doc_id, lineid, .keep_all = TRUE)

  selected <- selected |>
    select(-all_of(manual_cols)) |>
    left_join(existing_manual, by = c("doc_id", "lineid")) |>
    mutate(
      across(all_of(manual_cols), ~ ifelse(is.na(.x), "", .x)),
      manual_action = ifelse(nzchar(manual_action), manual_action,
                             "review_pending")
    ) |>
    select(
      doc_id, lineid, source_lineid, entry_instance,
      manual_target_reason, birth_place_word_n, has_dee_date_in_birth_place,
      birth_place_old, birth_date_old, birth_year_old, birth_city_old,
      field_old, birth_place_new, birth_date_new, birth_year_new,
      birth_city_new, field_new, manual_action, manual_confidence,
      manual_note, raw_text_adjusted, raw_text
    )
}

summary <- bind_rows(
  tibble(metric = "input_rows", value = nrow(input)),
  tibble(metric = "selected_rows", value = nrow(selected)),
  tibble(metric = "selected_unique_doc_lineid",
         value = n_distinct(paste(selected$doc_id, selected$lineid))),
  tibble(metric = "word_ge_4_rows",
         value = sum(selected$birth_place_word_n >= 4L)),
  tibble(metric = "word_ge_3_dee_rows",
         value = sum(selected$birth_place_word_n >= 3L &
                       selected$has_dee_date_in_birth_place)),
  selected |>
    count(manual_target_reason, name = "value") |>
    transmute(metric = paste0("reason:", manual_target_reason), value),
  selected |>
    count(manual_action, name = "value") |>
    transmute(metric = paste0("manual_action:", manual_action), value)
) |>
  mutate(value = as.numeric(value))

readr::write_excel_csv(selected, manual_csv, na = "")
readr::write_excel_csv(summary, summary_csv, na = "")

cat("Input:", input_file, "\n")
cat("Selected rows:", nrow(selected), "\n")
cat("Wrote manual corrections table:", manual_csv, "\n")
cat("Wrote selection summary:", summary_csv, "\n")
