#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), "..", "..", ".."), mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

panel_root <- file.path(DATA_OUTPUT, "land_grants", "faculty_longitudinal")
staging_root <- file.path(panel_root, "staging")
stopifnot(dir.exists(staging_root))

add_columns <- function(path, defaults) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  changed <- FALSE
  for (name in names(defaults)) {
    if (!name %in% names(x)) {
      x[[name]] <- rep(defaults[[name]], nrow(x))
      changed <- TRUE
    }
  }
  if (changed) write.csv(x, path, row.names = FALSE, na = "")
  changed
}

changed_files <- character()
page_log_columns <- c(
  "batch_id", "research_agent", "page_review_id", "college_id", "event_id",
  "college", "academic_year_start", "academic_year_label", "source_id",
  "source_page", "page_title", "visual_review_status", "n_included_rows",
  "n_excluded_rows", "reviewer_notes", "reviewed_at"
)
for (batch_dir in list.dirs(staging_root, recursive = FALSE, full.names = TRUE)) {
  roster_path <- file.path(batch_dir, "roster_entries.csv")
  coverage_path <- file.path(batch_dir, "coverage.csv")
  summary_path <- file.path(batch_dir, "batch_summary.csv")
  page_log_path <- file.path(batch_dir, "manual_page_log.csv")
  if (file.exists(roster_path) && add_columns(roster_path, c(
    transcription_method = "", page_visually_verified = "FALSE", verification_notes = ""
  ))) changed_files <- c(changed_files, roster_path)
  if (file.exists(coverage_path) && add_columns(coverage_path, c(
    roster_pages_reviewed = "", manual_verification_status = ""
  ))) changed_files <- c(changed_files, coverage_path)
  if (file.exists(summary_path) && add_columns(summary_path, c(
    pages_visually_reviewed = "", rows_visually_verified = ""
  ))) changed_files <- c(changed_files, summary_path)
  if (!file.exists(page_log_path)) {
    write.csv(setNames(as.data.frame(matrix(nrow = 0, ncol = length(page_log_columns))), page_log_columns),
              page_log_path, row.names = FALSE)
    changed_files <- c(changed_files, page_log_path)
  }
}

cat(sprintf("Manual-verification schema added to %d staging files.\n", length(changed_files)))
