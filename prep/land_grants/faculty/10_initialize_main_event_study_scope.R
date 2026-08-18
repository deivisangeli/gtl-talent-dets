#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), "..", "..", ".."), mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

source_root <- file.path(DATA_OUTPUT, "land_grants", "faculty_longitudinal")
panel_root <- file.path(DATA_OUTPUT, "land_grants", "faculty_main_event_study_20")
staging_root <- file.path(panel_root, "staging")

assignment_path <- file.path(source_root, "college_batch_assignments.csv")
manifest_path <- file.path(source_root, "college_year_work_manifest.csv")
stopifnot(file.exists(assignment_path), file.exists(manifest_path))

main_event_ids <- c(
  7L, 8L, 11L, 14L, 15L, 16L, 17L, 19L, 20L, 23L,
  25L, 26L, 31L, 32L, 33L, 36L, 39L, 40L, 42L, 43L
)

# Workload-balanced batches frozen for the main-event-study research run.
batch_map <- data.frame(
  event_id = c(
    16L, 42L, 26L,
    17L, 25L, 43L,
    11L, 19L, 36L,
    20L, 39L, 31L,
    7L, 14L, 33L,
    15L, 23L, 40L,
    8L, 32L
  ),
  batch_number = c(
    rep(1:6, each = 3), rep(7L, 2)
  ),
  stringsAsFactors = FALSE
)

if (!setequal(batch_map$event_id, main_event_ids) || anyDuplicated(batch_map$event_id)) {
  stop("The frozen batch map must contain each of the 20 main-event-study institutions exactly once.")
}

assignments_all <- read.csv(assignment_path, stringsAsFactors = FALSE, check.names = FALSE)
manifest_all <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)

assignments <- assignments_all[assignments_all$event_id %in% main_event_ids, ]
assignments <- merge(
  assignments[, setdiff(names(assignments), c("batch_id", "batch_number", "wave", "research_slot"))],
  batch_map,
  by = "event_id",
  all.x = TRUE,
  sort = FALSE
)
assignments$batch_id <- sprintf("batch_%02d", assignments$batch_number)
assignments$wave <- ceiling(assignments$batch_number / 3)
assignments$research_slot <- ((assignments$batch_number - 1L) %% 3L) + 1L
assignments <- assignments[order(assignments$batch_number, assignments$event_id), ]
assignments <- assignments[, c(
  "batch_id", "batch_number", "wave", "research_slot", "college_id", "event_id",
  "college", "experiment_year", "opening_or_transition_year", "target_college_years",
  "coverage_status", "estimated_workload"
)]

if (nrow(assignments) != 20L || anyDuplicated(assignments$event_id)) {
  stop("Expected 20 unique scoped assignments.")
}
batch_sizes <- table(assignments$batch_id)
if (!identical(as.integer(batch_sizes), c(rep(3L, 6), 2L))) {
  stop("Expected six batches of three institutions and one batch of two.")
}

manifest <- merge(
  manifest_all[, setdiff(names(manifest_all), c("batch_id", "wave", "research_slot"))],
  assignments[, c("event_id", "batch_id", "wave", "research_slot")],
  by = "event_id",
  all = FALSE,
  sort = FALSE
)
manifest <- manifest[order(manifest$batch_id, manifest$college_id, manifest$academic_year_start), ]
manifest <- manifest[, c(
  "batch_id", "wave", "research_slot", "college_id", "event_id", "college",
  "experiment_year", "opening_or_transition_year", "academic_year_start",
  "academic_year_label", "assignment_status"
)]

expected_years <- sum(assignments$target_college_years)
if (nrow(manifest) != expected_years) {
  stop("Scoped manifest year count does not match the assignment totals.")
}

schemas <- list(
  sources = c(
    "batch_id", "research_agent", "college_id", "event_id", "college", "source_id",
    "source_title", "source_type", "repository", "source_url", "local_path",
    "file_sha256", "academic_year_start", "academic_year_end", "roster_pages",
    "access_date", "extraction_method", "source_status", "notes"
  ),
  roster_entries = c(
    "batch_id", "research_agent", "roster_entry_id", "college_id", "event_id",
    "college", "academic_year_start", "academic_year_label", "person_name_raw",
    "person_name_normalized", "role_raw", "discipline_raw", "rank_normalized",
    "discipline_normalized", "division", "appointment_status", "scope_decision",
    "teaching_evidence", "source_id", "source_page", "confidence", "raw_roster_line",
    "transcription_method", "page_visually_verified", "verification_notes", "notes"
  ),
  coverage = c(
    "batch_id", "research_agent", "college_id", "event_id", "college",
    "academic_year_start", "academic_year_label", "source_ids", "expected_roster_count",
    "found_roster_count", "roster_pages_reviewed", "manual_verification_status",
    "coverage_status", "gap_reason", "next_source_candidate", "review_notes"
  ),
  manual_page_log = c(
    "batch_id", "research_agent", "page_review_id", "college_id", "event_id",
    "college", "academic_year_start", "academic_year_label", "source_id",
    "source_page", "page_title", "visual_review_status", "n_included_rows",
    "n_excluded_rows", "reviewer_notes", "reviewed_at"
  ),
  identity_proposals = c(
    "batch_id", "research_agent", "proposal_id", "college_id", "person_name_normalized",
    "local_person_key", "candidate_faculty_id", "link_scope", "evidence", "confidence",
    "researcher_recommendation", "primary_decision", "decision_notes"
  ),
  batch_summary = c(
    "batch_id", "research_agent", "institutions_expected", "institutions_completed",
    "target_college_years", "college_years_complete", "college_years_likely_complete",
    "college_years_partial", "college_years_not_found", "roster_entries",
    "pages_visually_reviewed", "rows_visually_verified", "unresolved_questions",
    "review_status", "submitted_at", "reviewed_at", "review_notes"
  )
)

write_empty_csv <- function(path, columns) {
  if (!file.exists(path) || file.info(path)$size == 0L) {
    empty <- setNames(as.data.frame(matrix(nrow = 0, ncol = length(columns))), columns)
    write.csv(empty, path, row.names = FALSE)
  }
}

dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
write.csv(assignments, file.path(panel_root, "college_batch_assignments.csv"), row.names = FALSE, na = "")
write.csv(manifest, file.path(panel_root, "college_year_work_manifest.csv"), row.names = FALSE, na = "")

legacy_roster_path <- file.path(source_root, "legacy_opening_roster_entries.csv")
legacy_coverage_path <- file.path(source_root, "legacy_opening_coverage.csv")
legacy_sources_path <- file.path(source_root, "legacy_sources.csv")
legacy_roster <- if (file.exists(legacy_roster_path)) read.csv(legacy_roster_path, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
legacy_coverage <- if (file.exists(legacy_coverage_path)) read.csv(legacy_coverage_path, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
legacy_sources <- if (file.exists(legacy_sources_path)) read.csv(legacy_sources_path, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()

if (nrow(legacy_roster)) {
  legacy_roster <- legacy_roster[legacy_roster$event_id %in% main_event_ids, ]
  legacy_roster$batch_id <- assignments$batch_id[match(legacy_roster$event_id, assignments$event_id)]
}
if (nrow(legacy_coverage)) {
  legacy_coverage <- legacy_coverage[legacy_coverage$event_id %in% main_event_ids, ]
  legacy_coverage$batch_id <- assignments$batch_id[match(legacy_coverage$event_id, assignments$event_id)]
}

used_legacy_source_ids <- character()
if (nrow(legacy_roster)) {
  used_legacy_source_ids <- unique(c(legacy_roster$primary_source_id, legacy_roster$secondary_source_id))
  used_legacy_source_ids <- used_legacy_source_ids[nzchar(used_legacy_source_ids)]
}
if (nrow(legacy_sources)) legacy_sources <- legacy_sources[legacy_sources$source_id %in% used_legacy_source_ids, ]
write.csv(legacy_sources, file.path(panel_root, "legacy_sources.csv"), row.names = FALSE, na = "")

for (batch_id in assignments$batch_id[!duplicated(assignments$batch_id)]) {
  batch_dir <- file.path(staging_root, batch_id)
  dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)
  batch_targets <- manifest[manifest$batch_id == batch_id, ]
  write.csv(batch_targets, file.path(batch_dir, "targets.csv"), row.names = FALSE, na = "")
  for (schema_name in names(schemas)) {
    write_empty_csv(file.path(batch_dir, paste0(schema_name, ".csv")), schemas[[schema_name]])
  }
  write.csv(legacy_roster[legacy_roster$batch_id == batch_id, ],
            file.path(batch_dir, "legacy_seed.csv"), row.names = FALSE, na = "")
  write.csv(legacy_coverage[legacy_coverage$batch_id == batch_id, ],
            file.path(batch_dir, "legacy_coverage_seed.csv"), row.names = FALSE, na = "")
}

cat(sprintf(
  "Initialized %d main-event-study institutions, %d batches, and %d college-years at %s\n",
  nrow(assignments), length(unique(assignments$batch_id)), nrow(manifest), panel_root
))
