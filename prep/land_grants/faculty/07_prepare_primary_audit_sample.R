#!/usr/bin/env Rscript

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), "..", "..", ".."), mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

trailing <- commandArgs(trailingOnly = TRUE)
batch_arg <- grep("^--batch=", trailing, value = TRUE)
batch_id <- if (length(batch_arg)) sub("^--batch=", "", batch_arg[[1]]) else ""
if (!grepl("^batch_[0-9]{2}$", batch_id)) stop("Supply --batch=batch_NN.")

panel_root <- file.path(DATA_OUTPUT, "land_grants", "faculty_longitudinal")
batch_dir <- file.path(panel_root, "staging", batch_id)
review_dir <- file.path(panel_root, "reviews", batch_id)
dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)

all_roster <- read.csv(file.path(batch_dir, "roster_entries.csv"), stringsAsFactors = FALSE, check.names = FALSE)
page_log <- read.csv(file.path(batch_dir, "manual_page_log.csv"), stringsAsFactors = FALSE, check.names = FALSE)
required <- c(
  "scope_decision", "source_id", "source_page", "college_id", "college",
  "academic_year_start", "academic_year_label", "person_name_normalized",
  "role_raw", "confidence", "verification_notes", "page_visually_verified"
)
missing <- setdiff(required, names(all_roster))
if (length(missing)) stop("Roster lacks: ", paste(missing, collapse = ", "))
roster <- all_roster[all_roster$scope_decision == "include", , drop = FALSE]
excluded <- all_roster[all_roster$scope_decision == "exclude", , drop = FALSE]
page_required <- c(
  "college_id", "college", "academic_year_start", "academic_year_label",
  "source_id", "source_page", "visual_review_status", "reviewer_notes"
)
page_missing <- setdiff(page_required, names(page_log))
if (length(page_missing)) stop("Manual page log lacks: ", paste(page_missing, collapse = ", "))
if (!nrow(page_log)) stop("No manually reviewed pages to audit.")

roster$page_key <- if (nrow(roster)) paste(
  roster$college_id, roster$source_id, roster$source_page, sep = "\u241f"
) else character()
excluded$page_key <- if (nrow(excluded)) paste(
  excluded$college_id, excluded$source_id, excluded$source_page, sep = "\u241f"
) else character()
page_log$page_key <- paste(page_log$college_id, page_log$source_id, page_log$source_page, sep = "\u241f")
pages <- do.call(rbind, lapply(seq_len(nrow(page_log)), function(i) {
  page <- page_log[i, , drop = FALSE]
  x <- roster[roster$page_key == page$page_key[[1]], , drop = FALSE]
  x_excluded <- excluded[excluded$page_key == page$page_key[[1]], , drop = FALSE]
  roster_uncertain <- nrow(x) && (
    any(tolower(x$confidence) == "low", na.rm = TRUE) || any(grepl(
      "uncertain|ambiguous|illegible|unclear", tolower(x$verification_notes)
    ), na.rm = TRUE)
  )
  page_uncertain <- grepl(
    "uncertain|ambiguous|illegible|unclear", tolower(page$reviewer_notes[[1]])
  )
  data.frame(
    page_key = page$page_key[[1]],
    college_id = page$college_id[[1]],
    college = page$college[[1]],
    academic_year_start = as.integer(page$academic_year_start[[1]]),
    academic_year_label = page$academic_year_label[[1]],
    source_id = page$source_id[[1]],
    source_page = page$source_page[[1]],
    n_included_rows = nrow(x),
    n_excluded_rows = nrow(x_excluded),
    roster_entry_ids = if (nrow(x)) paste(x$roster_entry_id, collapse = ";") else "",
    names_on_page = if (nrow(x)) paste(x$person_name_normalized, collapse = " | ") else "",
    roles_on_page = if (nrow(x)) paste(x$role_raw, collapse = " | ") else "",
    excluded_lines_on_page = if (nrow(x_excluded)) paste(
      paste(x_excluded$person_name_raw, x_excluded$role_raw, sep = ": "), collapse = " | "
    ) else "",
    all_rows_visually_verified = page$visual_review_status[[1]] == "complete" &&
      (!nrow(x) || all(!is.na(x$page_visually_verified) &
        toupper(trimws(x$page_visually_verified)) == "TRUE")),
    uncertainty_trigger = isTRUE(roster_uncertain) || isTRUE(page_uncertain),
    stringsAsFactors = FALSE
  )
}))
row.names(pages) <- NULL

# Ten percent of distinct pages per institution, minimum five, spread over time.
page_groups_by_college <- split(seq_len(nrow(pages)), pages$college_id)
sampled <- unique(unlist(lapply(page_groups_by_college, function(ii) {
  ii <- ii[order(pages$academic_year_start[ii], pages$source_id[ii], pages$source_page[ii])]
  n <- min(length(ii), max(5L, ceiling(0.10 * length(ii))))
  ii[unique(round(seq(1, length(ii), length.out = n)))]
})))
selected <- sort(unique(c(sampled, which(pages$uncertainty_trigger))))
audit <- pages[selected, , drop = FALSE]
audit$audit_page_id <- sprintf("%s_page_%04d", batch_id, seq_len(nrow(audit)))
audit$audit_reason <- ifelse(audit$uncertainty_trigger, "uncertainty_plus_sample", "stratified_10pct_sample")
audit$audit_result <- "pending"
audit$auditor_notes <- ""
audit$audited_at <- ""

out_path <- file.path(review_dir, "primary_page_audit.csv")
if (file.exists(out_path)) {
  prior <- read.csv(out_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (all(c("page_key", "audit_result", "auditor_notes", "audited_at") %in% names(prior))) {
    idx <- match(audit$page_key, prior$page_key)
    keep <- !is.na(idx) & prior$audit_result[idx] %in% c("pass", "fail")
    audit$audit_result[keep] <- prior$audit_result[idx[keep]]
    audit$auditor_notes[keep] <- prior$auditor_notes[idx[keep]]
    audit$audited_at[keep] <- prior$audited_at[idx[keep]]
  }
}

audit <- audit[, c(
  "audit_page_id", "page_key", "audit_reason", "college_id", "college",
  "academic_year_start", "academic_year_label", "source_id", "source_page",
  "n_included_rows", "n_excluded_rows", "roster_entry_ids", "names_on_page", "roles_on_page",
  "excluded_lines_on_page",
  "all_rows_visually_verified", "uncertainty_trigger", "audit_result",
  "auditor_notes", "audited_at"
)]
write.csv(audit, out_path, row.names = FALSE, na = "")
cat(sprintf("Wrote %d page-audit rows from %d distinct roster pages to %s.\n",
            nrow(audit), nrow(pages), out_path))
