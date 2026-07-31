###############################################################################
# Aggregate directed-QA outputs and isolate disagreements for adjudication.
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
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | str_to_upper(str_trim(x)) == "NA", "", x)
}

qa_root <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "intermediary", "amws",
  "manual_bad_ocr_birth_city_full_rollout_20260710", "qa"
)
master_csv <- file.path(qa_root, "amws_ed86_bad_ocr_qa_master.csv")
results_csv <- file.path(qa_root, "amws_ed86_bad_ocr_qa_results.csv")
disagreements_csv <- file.path(qa_root,
                               "amws_ed86_bad_ocr_qa_disagreements.csv")
summary_csv <- file.path(qa_root, "amws_ed86_bad_ocr_qa_summary.csv")
out_dir <- file.path(qa_root, "out")

csv_text_cols <- cols(.default = col_character())
read_text_csv <- function(path) {
  read_csv(path, col_types = csv_text_cols, show_col_types = FALSE) |>
    mutate(across(everything(), blank_na))
}
master <- read_text_csv(master_csv)
files <- sort(list.files(out_dir, pattern = "\\.csv$", full.names = TRUE))
if (length(files) != n_distinct(master$qa_batch_id)) {
  stop("Expected ", n_distinct(master$qa_batch_id), " QA outputs; found ",
       length(files), ".")
}

qa_cols <- c(
  "qa_decision", "qa_manual_action", "qa_birth_city_new",
  "qa_birth_state_new", "qa_birth_country_new",
  "qa_location_inference_basis", "qa_location_inference_note",
  "qa_confidence", "qa_note", "qa_reviewer_id"
)
keys <- c("qa_id", "global_review_id", "doc_id", "lineid")
outputs <- bind_rows(lapply(files, function(path) {
  x <- read_text_csv(path)
  missing <- setdiff(c(keys, qa_cols), names(x))
  if (length(missing)) stop(basename(path), " missing: ",
                            paste(missing, collapse = ", "))
  x |> select(all_of(c(keys, qa_cols))) |>
    mutate(qa_output_file = basename(path))
}))

if (nrow(outputs) != nrow(master) || n_distinct(outputs$qa_id) != nrow(master)) {
  stop("QA outputs do not cover the master exactly once.")
}
if (nrow(anti_join(outputs, master, by = keys)) ||
    nrow(anti_join(master, outputs, by = keys))) {
  stop("QA output keys differ from the master.")
}

results <- master |>
  select(-all_of(qa_cols)) |>
  left_join(outputs, by = keys) |>
  arrange(as.integer(qa_id))
write_excel_csv(results, results_csv, na = "")

disagreements <- results |>
  filter(qa_decision %in% c("revise", "escalate")) |>
  mutate(
    adjudication_decision = "review_pending",
    adjudicated_manual_action = "",
    adjudicated_birth_city_new = "",
    adjudicated_birth_state_new = "",
    adjudicated_birth_country_new = "",
    adjudicated_location_inference_basis = "",
    adjudicated_location_inference_note = "",
    adjudicated_confidence = "",
    adjudication_note = "",
    adjudicator_id = ""
  )
write_excel_csv(disagreements, disagreements_csv, na = "")

summary <- bind_rows(
  tibble(metric = "qa_rows", value = as.character(nrow(results))),
  tibble(metric = "qa_unique_keys", value = as.character(n_distinct(results$qa_id))),
  results |> count(qa_decision, name = "n") |>
    transmute(metric = paste0("qa_decision:", qa_decision),
              value = as.character(n)),
  tibble(metric = "disagreement_rows", value = as.character(nrow(disagreements)))
)
write_excel_csv(summary, summary_csv, na = "")

cat("QA rows:", nrow(results), "\n")
cat("Agree:", sum(results$qa_decision == "agree"), "\n")
cat("Revise:", sum(results$qa_decision == "revise"), "\n")
cat("Escalate:", sum(results$qa_decision == "escalate"), "\n")
cat("Wrote disagreements:", disagreements_csv, "\n")
