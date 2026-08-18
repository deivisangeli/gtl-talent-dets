#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), "..", "..", ".."), mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

trailing <- commandArgs(trailingOnly = TRUE)
get_arg <- function(prefix, default = "") {
  hit <- grep(paste0("^", prefix, "="), trailing, value = TRUE)
  if (length(hit)) sub(paste0("^", prefix, "="), "", hit[[1]]) else default
}
batch_id <- get_arg("--batch")
decision <- get_arg("--decision")
notes <- get_arg("--notes")
if (!grepl("^batch_[0-9]{2}$", batch_id)) stop("Supply --batch=batch_NN.")
if (!decision %in% c("accepted", "needs_revision")) stop("Decision must be accepted or needs_revision.")

panel_root <- file.path(DATA_OUTPUT, "land_grants", "faculty_longitudinal")
summary_path <- file.path(panel_root, "staging", batch_id, "batch_summary.csv")
checks_path <- file.path(panel_root, "reviews", batch_id, "automated_checks.csv")
stopifnot(file.exists(summary_path), file.exists(checks_path))

summary <- read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE)
checks <- read.csv(checks_path, stringsAsFactors = FALSE, check.names = FALSE)
coverage_path <- file.path(panel_root, "staging", batch_id, "coverage.csv")
roster_path <- file.path(panel_root, "staging", batch_id, "roster_entries.csv")
audit_path <- file.path(panel_root, "reviews", batch_id, "primary_page_audit.csv")
coverage <- read.csv(coverage_path, stringsAsFactors = FALSE, check.names = FALSE)
roster <- read.csv(roster_path, stringsAsFactors = FALSE, check.names = FALSE)
if (nrow(summary) != 1L) stop("Batch summary must contain exactly one row before review.")
if (decision == "accepted" && (!nrow(checks) || !all(checks$passed))) {
  stop("Cannot accept a batch with failed automated checks.")
}
if (decision == "accepted" && any(coverage$coverage_status == "source_found_not_processed")) {
  stop("Cannot accept a batch while accessible located sources remain unprocessed.")
}
if (decision == "accepted" && nrow(roster) && any(roster$scope_decision == "review")) {
  stop("Cannot accept a batch with unresolved roster scope decisions.")
}
if (decision == "accepted") {
  if (!file.exists(audit_path)) stop("Cannot accept a batch before the primary page audit is prepared.")
  audit <- read.csv(audit_path, stringsAsFactors = FALSE, check.names = FALSE)
  required_audit <- c("college_id", "audit_result", "auditor_notes", "all_rows_visually_verified")
  missing_audit <- setdiff(required_audit, names(audit))
  if (length(missing_audit)) stop("Primary page audit lacks: ", paste(missing_audit, collapse = ", "))
  expected_colleges <- unique(roster$college_id[roster$scope_decision == "include"])
  if (!setequal(unique(audit$college_id), expected_colleges)) stop("Primary page audit does not cover every institution with included rows.")
  if (!nrow(audit) || any(is.na(audit$audit_result) | audit$audit_result != "pass")) stop("Cannot accept while page-audit rows are pending or failed.")
  if (any(is.na(audit$all_rows_visually_verified) |
          toupper(trimws(audit$all_rows_visually_verified)) != "TRUE")) stop("Audited pages contain rows not visually verified.")
  if (any(is.na(audit$auditor_notes) | !nzchar(trimws(audit$auditor_notes)))) stop("Every page-audit decision requires reviewer notes.")
}

summary$review_status <- decision
summary$reviewed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
summary$review_notes <- notes
write.csv(summary, summary_path, row.names = FALSE, na = "")
cat(sprintf("Set %s review_status to %s.\n", batch_id, decision))
