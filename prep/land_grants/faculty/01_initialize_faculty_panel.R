#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), "..", "..", ".."), mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

faculty_input <- file.path(DATA_INPUT, "land_grants")
panel_root <- file.path(DATA_OUTPUT, "land_grants", "faculty_longitudinal")
staging_root <- file.path(panel_root, "staging")
raw_root <- file.path(TALENT_DETS_DATA_DIR, "raw", "land_grants", "faculty_rosters")

coverage_path <- file.path(faculty_input, "andrews_founding_faculty_coverage.csv")
event_path <- file.path(DATA_OUTPUT, "land_grants", "andrews_event_county_units_1850_1920.csv")
stopifnot(file.exists(coverage_path), file.exists(event_path))

dir.create(panel_root, recursive = TRUE, showWarnings = FALSE)
dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_root, recursive = TRUE, showWarnings = FALSE)

coverage <- read.csv(coverage_path, stringsAsFactors = FALSE, check.names = FALSE)
events <- read.csv(event_path, stringsAsFactors = FALSE, colClasses = c(GEOID = "character"), check.names = FALSE)
events <- events[events$sample_role == "treated", c("event_id", "college", "experiment_year", "college_type", "GEOID", "county", "state")]

targets <- merge(
  coverage,
  events,
  by = c("college", "experiment_year"),
  all.x = TRUE,
  sort = FALSE,
  suffixes = c("_legacy", "_event")
)
if (nrow(targets) != 57L || anyNA(targets$event_id) || anyDuplicated(targets$event_id)) {
  stop("Expected exactly 57 uniquely matched treated colleges.")
}

targets$college_id <- sprintf("andrews_event_%03d", as.integer(targets$event_id))
targets$opening_or_transition_year <- as.integer(targets$opening_or_transition_year)
targets$target_college_years <- 1950L - targets$opening_or_transition_year + 1L
targets$difficulty_multiplier <- ifelse(
  targets$coverage_status == "not_found", 1.50,
  ifelse(targets$coverage_status == "partial", 1.25,
         ifelse(targets$coverage_status == "likely_complete", 1.10, 1.00))
)
targets$estimated_workload <- targets$target_college_years * targets$difficulty_multiplier

# Deterministic three-pass snake assignment. Sorting the 57 colleges by
# estimated workload and reversing the second pass creates 19 batches of three
# while balancing both long histories and difficult first-search cases.
ord <- order(-targets$estimated_workload, targets$college)
targets <- targets[ord, ]
batch_sequence <- c(1:19, 19:1, 1:19)
targets$batch_id_longitudinal <- batch_sequence
targets$wave <- ceiling(targets$batch_id_longitudinal / 3)
targets$research_slot <- ((targets$batch_id_longitudinal - 1L) %% 3L) + 1L
targets <- targets[order(targets$batch_id_longitudinal, -targets$estimated_workload, targets$college), ]

if (any(table(targets$batch_id_longitudinal) != 3L)) {
  stop("Every longitudinal research batch must contain exactly three colleges.")
}

academic_label <- function(year) sprintf("%d-%02d", year, (year + 1L) %% 100L)
manifest <- do.call(rbind, lapply(seq_len(nrow(targets)), function(i) {
  years <- seq.int(targets$opening_or_transition_year[[i]], 1950L)
  data.frame(
    batch_id = sprintf("batch_%02d", targets$batch_id_longitudinal[[i]]),
    wave = targets$wave[[i]],
    research_slot = targets$research_slot[[i]],
    college_id = targets$college_id[[i]],
    event_id = targets$event_id[[i]],
    college = targets$college[[i]],
    experiment_year = targets$experiment_year[[i]],
    opening_or_transition_year = targets$opening_or_transition_year[[i]],
    academic_year_start = years,
    academic_year_label = academic_label(years),
    assignment_status = "not_started",
    stringsAsFactors = FALSE
  )
}))

if (nrow(manifest) != 3693L) stop("Expected 3,693 target college-years.")

college_assignments <- targets[, c(
  "batch_id_longitudinal", "wave", "research_slot", "college_id", "event_id",
  "college", "experiment_year", "opening_or_transition_year",
  "target_college_years", "coverage_status", "estimated_workload"
)]
names(college_assignments)[names(college_assignments) == "batch_id_longitudinal"] <- "batch_number"
college_assignments$batch_id <- sprintf("batch_%02d", college_assignments$batch_number)
college_assignments <- college_assignments[, c(
  "batch_id", "batch_number", "wave", "research_slot", "college_id", "event_id",
  "college", "experiment_year", "opening_or_transition_year",
  "target_college_years", "coverage_status", "estimated_workload"
)]

write.csv(college_assignments, file.path(panel_root, "college_batch_assignments.csv"), row.names = FALSE, na = "")
write.csv(manifest, file.path(panel_root, "college_year_work_manifest.csv"), row.names = FALSE, na = "")

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
    write.csv(setNames(as.data.frame(matrix(nrow = 0, ncol = length(columns))), columns), path, row.names = FALSE)
  }
}

for (batch_number in 1:19) {
  batch_id <- sprintf("batch_%02d", batch_number)
  batch_dir <- file.path(staging_root, batch_id)
  dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)
  batch_targets <- manifest[manifest$batch_id == batch_id, ]
  write.csv(batch_targets, file.path(batch_dir, "targets.csv"), row.names = FALSE, na = "")
  for (schema_name in names(schemas)) {
    write_empty_csv(file.path(batch_dir, paste0(schema_name, ".csv")), schemas[[schema_name]])
  }
}

cat(sprintf("Initialized %d colleges, %d batches, and %d college-years at %s\n",
            nrow(targets), length(unique(targets$batch_id_longitudinal)), nrow(manifest), panel_root))
