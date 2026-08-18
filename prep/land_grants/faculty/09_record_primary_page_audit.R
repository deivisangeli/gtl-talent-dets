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
get_arg <- function(prefix) {
  hit <- grep(paste0("^", prefix, "="), trailing, value = TRUE)
  if (length(hit)) sub(paste0("^", prefix, "="), "", hit[[1]]) else ""
}
batch_id <- get_arg("--batch")
audit_page_id <- get_arg("--audit-page-id")
result <- get_arg("--result")
notes <- get_arg("--notes")
if (!grepl("^batch_[0-9]{2}$", batch_id)) stop("Supply --batch=batch_NN.")
if (!nzchar(audit_page_id)) stop("Supply --audit-page-id=...")
if (!result %in% c("pass", "fail")) stop("--result must be pass or fail.")
if (!nzchar(trimws(notes))) stop("--notes must document the page comparison.")

audit_path <- file.path(
  DATA_OUTPUT, "land_grants", "faculty_longitudinal", "reviews", batch_id,
  "primary_page_audit.csv"
)
stopifnot(file.exists(audit_path))
audit <- read.csv(audit_path, stringsAsFactors = FALSE, check.names = FALSE)
idx <- which(audit$audit_page_id == audit_page_id)
if (length(idx) != 1L) stop("Audit page ID must match exactly one row.")
audit$audit_result[idx] <- result
audit$auditor_notes[idx] <- notes
audit$audited_at[idx] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
write.csv(audit, audit_path, row.names = FALSE, na = "")
cat(sprintf("Recorded %s for %s in %s.\n", result, audit_page_id, batch_id))
