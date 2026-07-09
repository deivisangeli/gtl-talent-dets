###############################################################################
# Aggregate AMWS Ed16 location-format manual correction chunk outputs.
#
# Reads:
#   output/amws/regex_all_docs/manual_location_format_batches/
#     amws_ed16_location_format_manual_master.csv
#     out/location_format_chunk_*.csv
#
# Writes:
#   output/amws/regex_all_docs/manual_location_format_batches/
#     amws_ed16_location_format_manual_corrections.csv
#     amws_ed16_location_format_manual_aggregate_summary.csv
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

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_LOCATION_FORMAT_OUTPUT_DIR",
                      default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

batch_root <- file.path(output_dir, "manual_location_format_batches")
out_dir <- file.path(batch_root, "out")
master_csv <- file.path(batch_root,
                        "amws_ed16_location_format_manual_master.csv")
corrections_csv <- file.path(batch_root,
                             "amws_ed16_location_format_manual_corrections.csv")
summary_csv <- file.path(batch_root,
                         "amws_ed16_location_format_manual_aggregate_summary.csv")

csv_text_cols <- cols(.default = col_character())
master <- read_csv(master_csv, col_types = csv_text_cols,
                   show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

manual_cols <- c("birth_city_new", "birth_state_new", "birth_year_new",
                 "manual_action", "manual_confidence", "manual_note")
required_cols <- c("review_id", "location_format_chunk", "doc_id", "lineid",
                   manual_cols)

files <- sort(list.files(out_dir, pattern = "\\.csv$", full.names = TRUE))
if (!length(files)) {
  stop("No correction chunk outputs found in: ", out_dir)
}

chunk_rows <- bind_rows(lapply(files, function(path) {
  x <- read_csv(path, col_types = csv_text_cols, show_col_types = FALSE) |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols)) {
    stop("Output chunk is missing required columns: ", basename(path), " -> ",
         paste(missing_cols, collapse = ", "))
  }
  x |>
    select(all_of(required_cols)) |>
    mutate(batch_output_file = basename(path))
}))

allowed_actions <- c("correct", "review_unclear", "no_change")
bad_actions <- setdiff(unique(chunk_rows$manual_action), allowed_actions)
if (length(bad_actions)) {
  stop("Invalid manual_action values: ", paste(bad_actions, collapse = ", "))
}

allowed_confidence <- c("high", "medium", "low", "")
bad_conf <- setdiff(unique(chunk_rows$manual_confidence), allowed_confidence)
if (length(bad_conf)) {
  stop("Invalid manual_confidence values: ", paste(bad_conf, collapse = ", "))
}

dup_keys <- chunk_rows |>
  count(doc_id, lineid) |>
  filter(n > 1L)
if (nrow(dup_keys)) {
  stop("Duplicate doc_id + lineid across output chunks: ", nrow(dup_keys))
}

unmatched <- anti_join(chunk_rows, master |> select(doc_id, lineid),
                       by = c("doc_id", "lineid"))
if (nrow(unmatched)) {
  stop("Output chunks contain keys not present in master: ", nrow(unmatched))
}

missing_outputs <- anti_join(master |> select(doc_id, lineid), chunk_rows,
                             by = c("doc_id", "lineid"))
if (nrow(missing_outputs)) {
  stop("Output chunks do not cover all master rows. Missing: ",
       nrow(missing_outputs))
}

bad_correct_note <- chunk_rows |>
  filter(manual_action == "correct") |>
  filter(!nzchar(str_trim(birth_city_new)) &
           !nzchar(str_trim(birth_state_new)) &
           !nzchar(str_trim(birth_year_new)) &
           !nzchar(str_trim(manual_note)))
if (nrow(bad_correct_note)) {
  stop("Correct rows with all new fields empty must explain why in manual_note: ",
       nrow(bad_correct_note))
}

corrections <- master |>
  select(-all_of(manual_cols)) |>
  left_join(chunk_rows |> select(-batch_output_file),
            by = c("review_id", "location_format_chunk", "doc_id", "lineid")) |>
  arrange(as.integer(review_id))

readr::write_excel_csv(corrections, corrections_csv, na = "")

summary <- bind_rows(
  tibble(metric = "output_files", value = length(files)),
  tibble(metric = "correction_rows", value = nrow(corrections)),
  tibble(metric = "correction_unique_doc_lineid",
         value = n_distinct(paste(corrections$doc_id, corrections$lineid))),
  corrections |>
    count(manual_action, name = "value") |>
    transmute(metric = paste0("manual_action:", manual_action), value),
  corrections |>
    count(manual_confidence, name = "value") |>
    transmute(metric = paste0("manual_confidence:", manual_confidence), value)
) |>
  mutate(value = as.numeric(value))
readr::write_excel_csv(summary, summary_csv, na = "")

cat("Aggregated files:", length(files), "\n")
cat("Rows:", nrow(corrections), "\n")
cat("Wrote corrections:", corrections_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
