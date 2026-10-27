#!/usr/bin/env Rscript
# Build the benchmark-year faculty-count panel for the 20 main-event-study
# colleges from the web-research staging files.
#
# Input (Dropbox): output/land_grants/faculty_counts_main20_<date>/staging/*.csv,
#                  faculty_main_event_study_20/college_batch_assignments.csv,
#                  faculty_longitudinal/legacy_opening_coverage.csv
# Output (Dropbox): faculty_count_observations.csv, faculty_count_panel.csv,
#                   coverage_summary.csv in the same release directory.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), "..", "..", ".."), mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

release_id <- "faculty_counts_main20_20261022"
release_root <- file.path(DATA_OUTPUT, "land_grants", release_id)
staging_root <- file.path(release_root, "staging")
assignment_path <- file.path(DATA_OUTPUT, "land_grants", "faculty_main_event_study_20",
                             "college_batch_assignments.csv")
legacy_path <- file.path(DATA_OUTPUT, "land_grants", "faculty_longitudinal",
                         "legacy_opening_coverage.csv")
stopifnot(dir.exists(staging_root), file.exists(assignment_path), file.exists(legacy_path))

max_offset <- 2L
last_benchmark <- 1950L

observation_cols <- c(
  "event_id", "college", "benchmark_year", "observed_year", "observed_academic_year",
  "faculty_total", "faculty_collegiate", "faculty_preparatory", "count_definition",
  "source_family", "source_title", "source_url", "page", "raw_text", "confidence", "notes"
)
# Within a year, national series come first so the panel stays on one
# definition where it can; the college's own records fill what they miss.
family_rank <- c(
  commissioner_report = 1L, biennial_survey = 1L, land_grant_statistics = 2L,
  ace_auc = 3L, other_directory = 4L, catalog = 5L, legacy_roster = 5L,
  institutional_history = 6L, other = 7L
)
confidence_rank <- c(high = 1L, medium = 2L, low = 3L)

###############################################################################
# Benchmarks: the opening year, then every year ending in 0 or 5 to 1950
###############################################################################

assignments <- read.csv(assignment_path, stringsAsFactors = FALSE, check.names = FALSE)
assignments <- assignments[order(assignments$event_id),
                           c("event_id", "college", "experiment_year", "opening_or_transition_year")]
stopifnot(nrow(assignments) == 20L, !anyDuplicated(assignments$event_id))

benchmarks <- do.call(rbind, lapply(seq_len(nrow(assignments)), function(i) {
  opening <- assignments$opening_or_transition_year[i]
  years <- sort(unique(c(opening, seq(ceiling((opening + 1) / 5) * 5, last_benchmark, by = 5))))
  data.frame(assignments[i, ], benchmark_year = years, row.names = NULL)
}))

###############################################################################
# Observations: staged web research plus the legacy opening-roster counts
###############################################################################

staging_files <- list.files(staging_root, pattern = "\\.csv$", full.names = TRUE)
stopifnot(length(staging_files) > 0L)
staged <- do.call(rbind, lapply(staging_files, function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
                encoding = "UTF-8")
  missing_cols <- setdiff(observation_cols, names(x))
  if (length(missing_cols)) stop(basename(path), " lacks columns: ", paste(missing_cols, collapse = ", "))
  x <- x[, observation_cols]
  x$staging_file <- basename(path)
  x
}))

legacy <- read.csv(legacy_path, stringsAsFactors = FALSE, check.names = FALSE)
legacy <- legacy[legacy$event_id %in% assignments$event_id & !is.na(legacy$found_roster_count), ]
legacy_obs <- data.frame(
  event_id = as.character(legacy$event_id),
  college = legacy$college,
  benchmark_year = as.character(legacy$opening_or_transition_year),
  observed_year = substr(legacy$roster_academic_year, 1, 4),
  observed_academic_year = legacy$roster_academic_year,
  faculty_total = as.character(legacy$found_roster_count),
  faculty_collegiate = "",
  faculty_preparatory = "",
  count_definition = "Unique included people on the first-search opening roster",
  source_family = "legacy_roster",
  source_title = "Legacy opening/transition faculty roster (faculty_longitudinal)",
  source_url = "output/land_grants/faculty_longitudinal/legacy_opening_roster_entries.csv",
  page = "",
  raw_text = "",
  # A partial roster is a known undercount; it stays as evidence but fills no cell.
  confidence = ifelse(legacy$coverage_status == "complete", "high",
                      ifelse(legacy$coverage_status == "likely_complete", "medium", "low")),
  notes = paste0("legacy coverage_status=", legacy$coverage_status),
  staging_file = "legacy_opening_coverage.csv",
  stringsAsFactors = FALSE
)

obs <- rbind(staged, legacy_obs)
obs[] <- lapply(obs, function(x) { x[is.na(x)] <- ""; trimws(x) })
for (col in c("event_id", "benchmark_year", "observed_year",
              "faculty_total", "faculty_collegiate", "faculty_preparatory")) {
  obs[[col]] <- suppressWarnings(as.integer(obs[[col]]))
}
obs$college <- assignments$college[match(obs$event_id, assignments$event_id)]
obs <- obs[order(obs$event_id, obs$observed_year, family_rank[obs$source_family]), ]
obs$observation_id <- sprintf("obs_%04d", seq_len(nrow(obs)))
obs <- obs[, c("observation_id", setdiff(names(obs), "observation_id"))]

###############################################################################
# Panel: one count per college x benchmark
###############################################################################

usable <- obs[!is.na(obs$event_id) & !is.na(obs$observed_year) &
                !is.na(obs$faculty_total) & obs$faculty_total > 0L &
                !(obs$source_family == "legacy_roster" & obs$confidence == "low"), ]

pick_cell <- function(event_id, benchmark_year) {
  cand <- usable[usable$event_id == event_id &
                   abs(usable$observed_year - benchmark_year) <= max_offset, ]
  if (!nrow(cand)) return(NULL)
  offset <- abs(cand$observed_year - benchmark_year)
  # Low-confidence rows (combined campuses, garbled lines) only fill a cell when
  # nothing better exists. Otherwise the nearest year wins, since young colleges
  # grow fast enough that two years matter more than the series; the source
  # family breaks ties.
  ord <- order(cand$confidence == "low", offset, family_rank[cand$source_family],
               confidence_rank[cand$confidence])
  best <- cand[ord[1], ]
  same_year <- cand[cand$observed_year == best$observed_year, ]
  data.frame(
    status = if (best$observed_year == benchmark_year) "observed" else "substitute_within_2y",
    faculty_total = best$faculty_total,
    faculty_collegiate = best$faculty_collegiate,
    faculty_preparatory = best$faculty_preparatory,
    observed_year = best$observed_year,
    observed_academic_year = best$observed_academic_year,
    year_offset = best$observed_year - benchmark_year,
    source_family = best$source_family,
    confidence = best$confidence,
    count_definition = best$count_definition,
    source_title = best$source_title,
    source_url = best$source_url,
    page = best$page,
    observation_id = best$observation_id,
    n_candidates = nrow(cand),
    n_source_families = length(unique(cand$source_family)),
    same_year_min = min(same_year$faculty_total),
    same_year_max = max(same_year$faculty_total),
    stringsAsFactors = FALSE
  )
}

empty_cell <- data.frame(
  status = "not_found", faculty_total = NA_integer_, faculty_collegiate = NA_integer_,
  faculty_preparatory = NA_integer_, observed_year = NA_integer_, observed_academic_year = "",
  year_offset = NA_integer_, source_family = "", confidence = "", count_definition = "",
  source_title = "", source_url = "", page = "", observation_id = "", n_candidates = 0L,
  n_source_families = 0L, same_year_min = NA_integer_, same_year_max = NA_integer_,
  stringsAsFactors = FALSE
)

panel <- do.call(rbind, lapply(seq_len(nrow(benchmarks)), function(i) {
  cell <- pick_cell(benchmarks$event_id[i], benchmarks$benchmark_year[i])
  if (is.null(cell)) cell <- empty_cell
  cbind(benchmarks[i, ], cell, row.names = NULL)
}))

coverage <- do.call(rbind, lapply(split(panel, panel$event_id), function(x) {
  found <- x[x$status != "not_found", ]
  data.frame(
    event_id = x$event_id[1], college = x$college[1],
    opening_or_transition_year = x$opening_or_transition_year[1],
    n_benchmarks = nrow(x),
    n_observed = sum(x$status == "observed"),
    n_substitute = sum(x$status == "substitute_within_2y"),
    n_not_found = sum(x$status == "not_found"),
    share_covered = round(nrow(found) / nrow(x), 3),
    missing_benchmarks = paste(x$benchmark_year[x$status == "not_found"], collapse = " "),
    source_families = paste(sort(unique(found$source_family)), collapse = " | "),
    stringsAsFactors = FALSE
  )
}))

write.csv(obs, file.path(release_root, "faculty_count_observations.csv"), row.names = FALSE, na = "")
write.csv(panel, file.path(release_root, "faculty_count_panel.csv"), row.names = FALSE, na = "")
write.csv(coverage, file.path(release_root, "coverage_summary.csv"), row.names = FALSE, na = "")

cat(sprintf("%d observations from %d staging files; %d of %d college-benchmarks covered (%d exact, %d within %d years)\n",
            nrow(obs), length(staging_files), sum(panel$status != "not_found"), nrow(panel),
            sum(panel$status == "observed"), sum(panel$status == "substitute_within_2y"), max_offset))
cat("wrote", release_root, "\n")
