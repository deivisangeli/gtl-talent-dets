#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), "..", "..", ".."), mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

legacy_root <- file.path(DATA_INPUT, "land_grants")
panel_root <- file.path(DATA_OUTPUT, "land_grants", "faculty_longitudinal")
assignment_path <- file.path(panel_root, "college_batch_assignments.csv")
faculty_path <- file.path(legacy_root, "andrews_founding_faculty.csv")
coverage_path <- file.path(legacy_root, "andrews_founding_faculty_coverage.csv")
stopifnot(file.exists(assignment_path), file.exists(faculty_path), file.exists(coverage_path))

assignments <- read.csv(assignment_path, stringsAsFactors = FALSE, check.names = FALSE)
legacy <- read.csv(faculty_path, stringsAsFactors = FALSE, check.names = FALSE)
legacy_coverage <- read.csv(coverage_path, stringsAsFactors = FALSE, check.names = FALSE)
names(legacy)[names(legacy) == "batch_id"] <- "legacy_batch_id"
names(legacy_coverage)[names(legacy_coverage) == "batch_id"] <- "legacy_batch_id"

legacy <- merge(
  legacy,
  assignments[, c("batch_id", "college_id", "event_id", "college", "experiment_year")],
  by = c("college", "experiment_year"), all.x = TRUE, sort = FALSE
)
legacy_coverage <- merge(
  legacy_coverage,
  assignments[, c("batch_id", "college_id", "event_id", "college", "experiment_year")],
  by = c("college", "experiment_year"), all.x = TRUE, sort = FALSE
)
if (nrow(legacy) != 338L || anyNA(legacy$college_id)) stop("Legacy faculty mapping failed.")
if (nrow(legacy_coverage) != 57L || anyNA(legacy_coverage$college_id)) stop("Legacy coverage mapping failed.")

source_rows <- list()
source_lookup <- new.env(parent = emptyenv())
source_counter <- 0L

register_source <- function(title, type, url, page) {
  if (!nzchar(trimws(url))) return("")
  key <- paste(title, type, url, page, sep = "\u241f")
  if (!exists(key, envir = source_lookup, inherits = FALSE)) {
    source_counter <<- source_counter + 1L
    source_id <- sprintf("legacy_source_%03d", source_counter)
    assign(key, source_id, envir = source_lookup)
    source_rows[[length(source_rows) + 1L]] <<- data.frame(
      source_id = source_id,
      source_title = title,
      source_type = type,
      source_url = url,
      source_page = page,
      stringsAsFactors = FALSE
    )
  }
  get(key, envir = source_lookup, inherits = FALSE)
}

primary_ids <- character(nrow(legacy))
secondary_ids <- character(nrow(legacy))
for (i in seq_len(nrow(legacy))) {
  primary_ids[[i]] <- register_source(
    legacy$source_1_title[[i]], legacy$source_1_type[[i]],
    legacy$source_1_url[[i]], legacy$source_1_page[[i]]
  )
  secondary_ids[[i]] <- register_source(
    legacy$source_2_title[[i]], legacy$source_2_type[[i]],
    legacy$source_2_url[[i]], legacy$source_2_page[[i]]
  )
}

year_from_label <- suppressWarnings(as.integer(sub("^.*?([0-9]{4}).*$", "\\1", legacy$roster_academic_year)))
year_from_label[is.na(year_from_label)] <- as.integer(legacy$opening_or_transition_year[is.na(year_from_label)])

seed <- data.frame(
  legacy_roster_entry_id = sprintf("legacy_roster_%04d", seq_len(nrow(legacy))),
  batch_id = legacy$batch_id,
  college_id = legacy$college_id,
  event_id = legacy$event_id,
  college = legacy$college,
  experiment_year = legacy$experiment_year,
  opening_or_transition_year = legacy$opening_or_transition_year,
  academic_year_start = year_from_label,
  academic_year_label = legacy$roster_academic_year,
  person_name_raw = legacy$person_name_raw,
  person_name_normalized = legacy$person_name_normalized,
  role_raw = legacy$role_raw,
  discipline_raw = legacy$discipline,
  appointment_type = legacy$appointment_type,
  teaching_evidence = legacy$teaching_evidence,
  primary_source_id = primary_ids,
  secondary_source_id = secondary_ids,
  confidence = legacy$confidence,
  notes = legacy$notes,
  stringsAsFactors = FALSE
)

reconciliation <- data.frame(
  legacy_roster_entry_id = seed$legacy_roster_entry_id,
  matched_roster_entry_id = "",
  reconciliation_status = "pending",
  discrepancy_type = "",
  reviewer_notes = "",
  stringsAsFactors = FALSE
)

coverage_seed <- legacy_coverage[, c(
  "batch_id", "college_id", "event_id", "college", "experiment_year",
  "opening_or_transition_year", "roster_academic_year", "expected_roster_count",
  "found_roster_count", "coverage_status", "gap_reason", "next_source_candidate", "review_notes"
)]

write.csv(do.call(rbind, source_rows), file.path(panel_root, "legacy_sources.csv"), row.names = FALSE, na = "")
write.csv(seed, file.path(panel_root, "legacy_opening_roster_entries.csv"), row.names = FALSE, na = "")
write.csv(coverage_seed, file.path(panel_root, "legacy_opening_coverage.csv"), row.names = FALSE, na = "")
write.csv(reconciliation, file.path(panel_root, "legacy_reconciliation.csv"), row.names = FALSE, na = "")

staging_root <- file.path(panel_root, "staging")
for (batch_id in unique(assignments$batch_id)) {
  batch_dir <- file.path(staging_root, batch_id)
  if (!dir.exists(batch_dir)) next
  write.csv(seed[seed$batch_id == batch_id, ], file.path(batch_dir, "legacy_seed.csv"), row.names = FALSE, na = "")
  write.csv(coverage_seed[coverage_seed$batch_id == batch_id, ],
            file.path(batch_dir, "legacy_coverage_seed.csv"), row.names = FALSE, na = "")
}

cat(sprintf("Prepared %d legacy roster rows, %d unique source records, and %d coverage rows.\n",
            nrow(seed), length(source_rows), nrow(coverage_seed)))
