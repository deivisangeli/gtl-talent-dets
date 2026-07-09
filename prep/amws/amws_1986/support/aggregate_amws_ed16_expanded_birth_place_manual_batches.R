###############################################################################
# Aggregate AMWS Ed16 manual birth-place batch outputs into the master
# manual-corrections CSV.
#
# Reads:
#   output/amws/regex_all_docs/amws_ed16_expanded_birth_place_manual_corrections.csv
#   output/amws/regex_all_docs/manual_birth_place_batches/out/*.csv
#
# Writes:
#   output/amws/regex_all_docs/amws_ed16_expanded_birth_place_manual_corrections.csv
#   output/amws/regex_all_docs/amws_ed16_expanded_birth_place_manual_aggregate_summary.csv
#
# Environment overrides:
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

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_MANUAL_BP_OUTPUT_DIR", default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

manual_csv <- file.path(output_dir,
                        "amws_ed16_expanded_birth_place_manual_corrections.csv")
out_dir <- file.path(output_dir, "manual_birth_place_batches", "out")
summary_csv <- file.path(output_dir,
                         "amws_ed16_expanded_birth_place_manual_aggregate_summary.csv")

csv_text_cols <- cols(.default = col_character())

master <- read_csv(manual_csv, col_types = csv_text_cols, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

manual_cols <- c(
  "birth_place_new", "birth_date_new", "birth_year_new", "birth_city_new",
  "field_new", "manual_action", "manual_confidence", "manual_note"
)
required_out_cols <- c("doc_id", "lineid", "source_lineid", "entry_instance",
                       manual_cols)

files <- sort(list.files(out_dir, pattern = "\\.csv$", full.names = TRUE))
if (!length(files)) {
  summary <- bind_rows(
    tibble(metric = "batch_output_files", value = 0),
    master |> count(manual_action, name = "value") |>
      transmute(metric = paste0("manual_action:", manual_action), value)
  ) |>
    mutate(value = as.numeric(value))
  readr::write_excel_csv(summary, summary_csv, na = "")
  cat("No batch output CSVs found in:", out_dir, "\n")
  quit(save = "no", status = 0)
}

batch_rows <- bind_rows(lapply(files, function(path) {
  x <- read_csv(path, col_types = csv_text_cols, show_col_types = FALSE) |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
  missing_cols <- setdiff(required_out_cols, names(x))
  if (length(missing_cols)) {
    stop("Batch output is missing required columns: ", basename(path), " -> ",
         paste(missing_cols, collapse = ", "))
  }
  x |>
    select(all_of(required_out_cols)) |>
    mutate(batch_output_file = basename(path))
}))

allowed_actions <- c("review_pending", "correct", "review_unclear", "no_change")
bad_actions <- setdiff(unique(batch_rows$manual_action), allowed_actions)
if (length(bad_actions)) {
  stop("Invalid manual_action values in batch outputs: ",
       paste(bad_actions, collapse = ", "))
}

allowed_conf <- c("", "high", "medium", "low")
bad_conf <- setdiff(unique(batch_rows$manual_confidence), allowed_conf)
if (length(bad_conf)) {
  stop("Invalid manual_confidence values in batch outputs: ",
       paste(bad_conf, collapse = ", "))
}

bad_correct <- batch_rows |>
  filter(manual_action == "correct",
         !nzchar(str_trim(birth_place_new)))
if (nrow(bad_correct)) {
  stop("Batch outputs mark rows as correct with empty birth_place_new: ",
       nrow(bad_correct))
}

dup_keys <- batch_rows |>
  count(doc_id, lineid) |>
  filter(n > 1L)
if (nrow(dup_keys)) {
  stop("Duplicate doc_id + lineid keys across batch outputs: ", nrow(dup_keys))
}

unmatched <- anti_join(batch_rows, master |> select(doc_id, lineid),
                       by = c("doc_id", "lineid"))
if (nrow(unmatched)) {
  stop("Batch outputs contain keys not present in master table: ", nrow(unmatched))
}

updated <- master |>
  left_join(batch_rows |> select(doc_id, lineid, all_of(manual_cols)),
            by = c("doc_id", "lineid"),
            suffix = c("", "_batch")) |>
  mutate(
    birth_place_new = ifelse(!is.na(birth_place_new_batch), birth_place_new_batch,
                             birth_place_new),
    birth_date_new = ifelse(!is.na(birth_date_new_batch), birth_date_new_batch,
                            birth_date_new),
    birth_year_new = ifelse(!is.na(birth_year_new_batch), birth_year_new_batch,
                            birth_year_new),
    birth_city_new = ifelse(!is.na(birth_city_new_batch), birth_city_new_batch,
                            birth_city_new),
    field_new = ifelse(!is.na(field_new_batch), field_new_batch, field_new),
    manual_action = ifelse(!is.na(manual_action_batch), manual_action_batch,
                           manual_action),
    manual_confidence = ifelse(!is.na(manual_confidence_batch),
                               manual_confidence_batch, manual_confidence),
    manual_note = ifelse(!is.na(manual_note_batch), manual_note_batch,
                         manual_note)
  ) |>
  select(-ends_with("_batch"))

readr::write_excel_csv(updated, manual_csv, na = "")

summary <- bind_rows(
  tibble(metric = "batch_output_files", value = length(files)),
  tibble(metric = "batch_output_rows", value = nrow(batch_rows)),
  tibble(metric = "batch_output_unique_doc_lineid",
         value = n_distinct(paste(batch_rows$doc_id, batch_rows$lineid))),
  updated |> count(manual_action, name = "value") |>
    transmute(metric = paste0("manual_action:", manual_action), value),
  updated |> count(manual_confidence, name = "value") |>
    transmute(metric = paste0("manual_confidence:", manual_confidence), value)
) |>
  mutate(value = as.numeric(value))

readr::write_excel_csv(summary, summary_csv, na = "")

cat("Aggregated batch output files:", length(files), "\n")
cat("Aggregated batch rows:", nrow(batch_rows), "\n")
cat("Updated master corrections:", manual_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
