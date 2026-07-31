###############################################################################
# Rebuild Data/processed/amws/amws_ed86.xlsx from corrected amws_ed86.csv.
# Set AMWS_ED86_XLSX_COMMIT=TRUE to back up and replace the official workbook.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(openxlsx)
  library(readr)
  library(stringr)
  library(tibble)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..",
                                       ".."), winslash = "/", mustWork = TRUE)
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

env_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = ifelse(default, "true", "false")))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be a Boolean flag; received: ", value)
  }
  value %in% c("true", "1", "yes")
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | str_to_upper(str_trim(x)) == "NA", "", x)
}

data_dir <- file.path(TALENT_DETS_DATA_DIR, "Data")
processed_dir <- file.path(data_dir, "processed", "amws")
audit_dir <- file.path(
  data_dir, "intermediary", "amws",
  "manual_bad_ocr_birth_city_full_rollout_20260710", "apply_20260711"
)
source_csv <- file.path(processed_dir, "amws_ed86.csv")
official_xlsx <- file.path(processed_dir, "amws_ed86.xlsx")
candidate_xlsx <- file.path(audit_dir,
                            "amws_ed86_synced_candidate.xlsx")
backup_xlsx <- file.path(audit_dir,
                         "amws_ed86_before_bad_ocr_sync_20260711.xlsx")
summary_csv <- file.path(audit_dir, "amws_ed86_xlsx_sync_summary.csv")
commit <- env_flag("AMWS_ED86_XLSX_COMMIT", FALSE)

if (!file.exists(source_csv) || !file.exists(official_xlsx)) {
  stop("Corrected CSV or official XLSX is missing.")
}
source_hash <- unname(tools::md5sum(source_csv))
old_xlsx_hash <- unname(tools::md5sum(official_xlsx))
source <- read_csv(
  source_csv,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  progress = FALSE
) |>
  mutate(across(everything(), blank_na))

key <- paste(source$doc_id, source$lineid, source$entry_instance, sep = "\r")
if (nrow(source) != 94809L || anyDuplicated(key)) {
  stop("Corrected CSV row count or key uniqueness does not match expectations.")
}

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
workbook <- createWorkbook()
addWorksheet(workbook, "amws_ed86")
writeData(workbook, "amws_ed86", source)
freezePane(workbook, "amws_ed86", firstRow = TRUE)
saveWorkbook(workbook, candidate_xlsx, overwrite = TRUE)

if (!file.exists(candidate_xlsx) || file.info(candidate_xlsx)$size <= 0L ||
    !identical(getSheetNames(candidate_xlsx), "amws_ed86")) {
  stop("The candidate workbook was not generated correctly.")
}
candidate_hash <- unname(tools::md5sum(candidate_xlsx))

committed <- FALSE
backup_hash <- ""
final_hash <- old_xlsx_hash
if (commit) {
  if (file.exists(backup_xlsx)) {
    stop("Backup already exists; refusing to overwrite: ", backup_xlsx)
  }
  if (unname(tools::md5sum(source_csv)) != source_hash) {
    stop("Corrected CSV changed during workbook generation.")
  }
  if (!file.copy(official_xlsx, backup_xlsx, overwrite = FALSE)) {
    stop("Could not create XLSX backup.")
  }
  backup_hash <- unname(tools::md5sum(backup_xlsx))
  if (backup_hash != old_xlsx_hash) stop("XLSX backup hash mismatch.")
  if (!file.copy(candidate_xlsx, official_xlsx, overwrite = TRUE)) {
    stop("Could not replace official XLSX.")
  }
  final_hash <- unname(tools::md5sum(official_xlsx))
  if (final_hash != candidate_hash) {
    file.copy(backup_xlsx, official_xlsx, overwrite = TRUE)
    stop("Final XLSX hash mismatch; backup was restored.")
  }
  committed <- TRUE
}

summary <- tribble(
  ~metric, ~value,
  "commit_requested", as.character(commit),
  "committed", as.character(committed),
  "source_csv", source_csv,
  "official_xlsx", official_xlsx,
  "candidate_xlsx", candidate_xlsx,
  "backup_xlsx", ifelse(committed, backup_xlsx, ""),
  "rows", as.character(nrow(source)),
  "columns", as.character(ncol(source)),
  "sheet_name", "amws_ed86",
  "source_csv_md5", source_hash,
  "old_xlsx_md5", old_xlsx_hash,
  "backup_xlsx_md5", backup_hash,
  "candidate_xlsx_md5", candidate_hash,
  "final_xlsx_md5", final_hash
)
write_excel_csv(summary, summary_csv, na = "")

cat("Rows:", nrow(source), "\n")
cat("Committed:", committed, "\n")
cat("Workbook:", official_xlsx, "\n")
cat("Summary:", summary_csv, "\n")
