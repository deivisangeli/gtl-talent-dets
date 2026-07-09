###############################################################################
# Prepare AMWS Ed16 location-format manual correction chunks.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv
#
# Writes:
#   output/amws/regex_all_docs/manual_location_format_batches/
#     amws_ed16_location_format_manual_master.csv
#     in/location_format_chunk_01.csv ... location_format_chunk_04.csv
#     amws_ed16_location_format_manual_prep_summary.csv
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

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_LOCATION_FORMAT_OUTPUT_DIR",
                      default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input_file <- env_chr(
  "AMWS_ED16_LOCATION_FORMAT_INPUT_FILE",
  file.path(output_dir,
            "amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv")
)
input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

batch_root <- file.path(output_dir, "manual_location_format_batches")
in_dir <- file.path(batch_root, "in")
out_dir <- file.path(batch_root, "out")
dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

master_csv <- file.path(batch_root,
                        "amws_ed16_location_format_manual_master.csv")
summary_csv <- file.path(batch_root,
                         "amws_ed16_location_format_manual_prep_summary.csv")

csv_text_cols <- cols(.default = col_character())
input <- read_csv(input_file, col_types = csv_text_cols,
                  show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c(
  "doc_id", "lineid", "source_lineid", "entry_instance", "raw_text",
  "raw_text_adjusted", "birth_place", "birth_date", "birth_year",
  "birth_city", "birth_state", "birth_country", "field",
  "birth_location_format_problem", "birth_location_format_problem_reason"
)
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input is missing required columns: ", paste(missing_cols, collapse = ", "))
}

flagged <- input |>
  filter(birth_location_format_problem %in% c("TRUE", "true", "1", TRUE)) |>
  arrange(doc_id, suppressWarnings(as.integer(lineid))) |>
  mutate(
    review_id = row_number(),
    location_format_chunk = sprintf("%02d", ((review_id - 1L) %% 4L) + 1L),
    manual_action = "review_pending",
    manual_confidence = "",
    birth_city_new = "",
    birth_state_new = "",
    birth_year_new = "",
    manual_note = "",
    .before = 1
  ) |>
  transmute(
    review_id,
    location_format_chunk,
    doc_id,
    lineid,
    source_lineid,
    entry_instance,
    raw_text_adjusted,
    raw_text,
    birth_place,
    birth_date,
    birth_city_old = birth_city,
    birth_state_old = birth_state,
    birth_country_old = birth_country,
    birth_year_old = birth_year,
    field,
    birth_location_format_problem_reason,
    birth_city_new,
    birth_state_new,
    birth_year_new,
    manual_action,
    manual_confidence,
    manual_note
  )

if (n_distinct(paste(flagged$doc_id, flagged$lineid)) != nrow(flagged)) {
  stop("Flagged rows have duplicated doc_id + lineid.")
}

readr::write_excel_csv(flagged, master_csv, na = "")

chunk_files <- flagged |>
  group_split(location_format_chunk)
for (chunk in chunk_files) {
  chunk_id <- unique(chunk$location_format_chunk)
  chunk_file <- file.path(in_dir,
                          paste0("location_format_chunk_", chunk_id, ".csv"))
  readr::write_excel_csv(chunk, chunk_file, na = "")
}

summary <- bind_rows(
  tibble(metric = "input_file", value = input_file),
  tibble(metric = "flagged_rows", value = as.character(nrow(flagged))),
  tibble(metric = "flagged_unique_doc_lineid",
         value = as.character(n_distinct(paste(flagged$doc_id, flagged$lineid)))),
  flagged |>
    count(location_format_chunk, name = "value") |>
    transmute(metric = paste0("chunk_rows:", location_format_chunk),
              value = as.character(value))
)
readr::write_excel_csv(summary, summary_csv, na = "")

cat("Input:", input_file, "\n")
cat("Flagged rows:", nrow(flagged), "\n")
cat("Wrote master:", master_csv, "\n")
cat("Wrote chunks to:", in_dir, "\n")
cat("Wrote summary:", summary_csv, "\n")
