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
assignments <- read.csv(file.path(panel_root, "college_batch_assignments.csv"), stringsAsFactors = FALSE)
manifest <- read.csv(file.path(panel_root, "college_year_work_manifest.csv"), stringsAsFactors = FALSE)
legacy <- read.csv(file.path(panel_root, "legacy_opening_roster_entries.csv"), stringsAsFactors = FALSE)
legacy_sources <- read.csv(file.path(panel_root, "legacy_sources.csv"), stringsAsFactors = FALSE)
reconciliation <- read.csv(file.path(panel_root, "legacy_reconciliation.csv"), stringsAsFactors = FALSE)

batch_dirs <- file.path(staging_root, sprintf("batch_%02d", 1:19))
all_headers_contain <- function(file_name, required) {
  all(vapply(batch_dirs, function(batch_dir) {
    path <- file.path(batch_dir, file_name)
    file.exists(path) && all(required %in% names(read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE, nrows = 0
    )))
  }, logical(1)))
}

checks <- c(
  colleges = nrow(assignments) == 57L && !anyDuplicated(assignments$college_id),
  batches = length(unique(assignments$batch_id)) == 19L,
  batch_size = all(table(assignments$batch_id) == 3L),
  target_years = nrow(manifest) == 3693L,
  manifest_unique = !anyDuplicated(paste(manifest$college_id, manifest$academic_year_start)),
  manifest_bounds = all(manifest$academic_year_start >= manifest$opening_or_transition_year & manifest$academic_year_start <= 1950L),
  legacy_rows = nrow(legacy) == 338L,
  legacy_sources = nrow(legacy_sources) == 124L && all(nzchar(legacy_sources$source_url)),
  reconciliation_rows = nrow(reconciliation) == 338L,
  staging_dirs = all(dir.exists(batch_dirs)),
  manual_roster_schema = all_headers_contain("roster_entries.csv", c(
    "transcription_method", "page_visually_verified", "verification_notes"
  )),
  manual_coverage_schema = all_headers_contain("coverage.csv", c(
    "roster_pages_reviewed", "manual_verification_status"
  )),
  manual_page_log_schema = all_headers_contain("manual_page_log.csv", c(
    "page_review_id", "source_id", "source_page", "visual_review_status",
    "n_included_rows", "n_excluded_rows", "reviewer_notes"
  )),
  manual_summary_schema = all_headers_contain("batch_summary.csv", c(
    "institutions_expected", "pages_visually_reviewed", "rows_visually_verified"
  ))
)

report <- data.frame(check = names(checks), passed = unname(checks), stringsAsFactors = FALSE)
write.csv(report, file.path(panel_root, "scaffold_validation.csv"), row.names = FALSE)
print(report, row.names = FALSE)
if (!all(checks)) stop("Faculty scaffold validation failed.")
