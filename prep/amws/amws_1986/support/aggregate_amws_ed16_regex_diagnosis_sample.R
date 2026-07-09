###############################################################################
# Aggregate AMWS Ed16 regex-diagnosis sample reviews.
#
# Reads:
#   output/amws/regex_all_docs/manual_birth_place_regex_sample/
#     amws_ed16_remaining_regex_diagnosis_sample.csv
#     out/*.csv
#
# Writes:
#   output/amws/regex_all_docs/manual_birth_place_regex_sample/
#     amws_ed16_remaining_regex_diagnosis_sample_reviewed.csv
#     regex_diagnosis_summary.csv
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

csv_text_cols <- cols(.default = col_character())

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_MANUAL_BP_OUTPUT_DIR", default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

sample_root <- file.path(output_dir, "manual_birth_place_regex_sample")
sample_csv <- file.path(sample_root,
                        "amws_ed16_remaining_regex_diagnosis_sample.csv")
reviewed_csv <- file.path(sample_root,
                          "amws_ed16_remaining_regex_diagnosis_sample_reviewed.csv")
summary_csv <- file.path(sample_root, "regex_diagnosis_summary.csv")
out_dir <- file.path(sample_root, "out")

sample <- read_csv(sample_csv, col_types = csv_text_cols, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

manual_cols <- c(
  "birth_place_new", "birth_date_new", "birth_year_new", "birth_city_new",
  "field_new", "manual_action", "manual_confidence", "manual_note",
  "error_category", "regex_correctable", "regex_rule_suggestion",
  "regex_review_note"
)
required_out_cols <- c(
  "regex_sample_id", "doc_id", "lineid", "source_lineid", "entry_instance",
  manual_cols
)

files <- sort(list.files(out_dir, pattern = "\\.csv$", full.names = TRUE))
if (!length(files)) {
  stop("No reviewed sample output CSVs found in: ", out_dir)
}

reviewed_rows <- bind_rows(lapply(files, function(path) {
  x <- read_csv(path, col_types = csv_text_cols, show_col_types = FALSE) |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
  missing_cols <- setdiff(required_out_cols, names(x))
  if (length(missing_cols)) {
    stop("Sample output is missing required columns: ", basename(path), " -> ",
         paste(missing_cols, collapse = ", "))
  }
  x |>
    select(all_of(required_out_cols)) |>
    mutate(sample_output_file = basename(path))
}))

allowed_actions <- c("correct", "review_unclear", "no_change")
bad_actions <- setdiff(unique(reviewed_rows$manual_action), allowed_actions)
if (length(bad_actions)) {
  stop("Invalid manual_action values in sample outputs: ",
       paste(bad_actions, collapse = ", "))
}

allowed_conf <- c("high", "medium", "low")
bad_conf <- setdiff(unique(reviewed_rows$manual_confidence), allowed_conf)
if (length(bad_conf)) {
  stop("Invalid manual_confidence values in sample outputs: ",
       paste(bad_conf, collapse = ", "))
}

allowed_regex <- c("yes", "no", "maybe")
bad_regex <- setdiff(unique(str_to_lower(reviewed_rows$regex_correctable)),
                     allowed_regex)
if (length(bad_regex)) {
  stop("Invalid regex_correctable values in sample outputs: ",
       paste(bad_regex, collapse = ", "))
}

bad_correct <- reviewed_rows |>
  filter(manual_action == "correct", !nzchar(str_trim(birth_place_new)))
if (nrow(bad_correct)) {
  stop("Sample outputs mark rows as correct with empty birth_place_new: ",
       nrow(bad_correct))
}

dup_keys <- reviewed_rows |>
  count(doc_id, lineid) |>
  filter(n > 1L)
if (nrow(dup_keys)) {
  stop("Duplicate doc_id + lineid keys across sample outputs: ", nrow(dup_keys))
}

unmatched <- anti_join(reviewed_rows, sample |> select(doc_id, lineid),
                       by = c("doc_id", "lineid"))
if (nrow(unmatched)) {
  stop("Sample outputs contain keys not present in sample: ", nrow(unmatched))
}

missing_review <- anti_join(sample |> select(doc_id, lineid), reviewed_rows,
                            by = c("doc_id", "lineid"))

updated <- sample |>
  left_join(reviewed_rows |> select(doc_id, lineid, all_of(manual_cols),
                                    sample_output_file),
            by = c("doc_id", "lineid"),
            suffix = c("", "_review"))

for (col_name in manual_cols) {
  review_col <- paste0(col_name, "_review")
  updated[[col_name]] <- ifelse(!is.na(updated[[review_col]]),
                                updated[[review_col]],
                                updated[[col_name]])
}

updated <- updated |>
  mutate(sample_output_file = ifelse(is.na(sample_output_file), "",
                                     sample_output_file)) |>
  select(-ends_with("_review"))

write_excel_csv(updated, reviewed_csv, na = "")

summary <- bind_rows(
  tibble(metric = "sample_rows", value = nrow(sample)),
  tibble(metric = "sample_output_files", value = length(files)),
  tibble(metric = "sample_output_rows", value = nrow(reviewed_rows)),
  tibble(metric = "sample_output_unique_doc_lineid",
         value = n_distinct(paste(reviewed_rows$doc_id, reviewed_rows$lineid))),
  tibble(metric = "sample_missing_review_rows", value = nrow(missing_review)),
  updated |> count(manual_action, name = "value") |>
    transmute(metric = paste0("manual_action:", manual_action), value),
  updated |> count(manual_confidence, name = "value") |>
    transmute(metric = paste0("manual_confidence:", manual_confidence), value),
  updated |> count(error_category, name = "value") |>
    transmute(metric = paste0("error_category:", error_category), value),
  updated |> count(regex_correctable, name = "value") |>
    transmute(metric = paste0("regex_correctable:", regex_correctable), value)
) |>
  mutate(value = as.numeric(value))

write_excel_csv(summary, summary_csv, na = "")

cat("Sample rows:", nrow(sample), "\n")
cat("Reviewed rows:", nrow(reviewed_rows), "\n")
cat("Missing review rows:", nrow(missing_review), "\n")
cat("Wrote reviewed sample:", reviewed_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
