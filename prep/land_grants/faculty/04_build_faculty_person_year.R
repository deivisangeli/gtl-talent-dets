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
roster_path <- file.path(panel_root, "faculty_roster_entries_accepted.csv")
coverage_path <- file.path(panel_root, "faculty_coverage_accepted.csv")
identity_path <- file.path(panel_root, "faculty_identity_proposals_accepted.csv")
stopifnot(file.exists(roster_path), file.exists(coverage_path), file.exists(identity_path))

roster <- read.csv(roster_path, stringsAsFactors = FALSE, check.names = FALSE)
coverage <- read.csv(coverage_path, stringsAsFactors = FALSE, check.names = FALSE)
identity <- read.csv(identity_path, stringsAsFactors = FALSE, check.names = FALSE)

collapse_values <- function(x) {
  x <- unique(trimws(x[!is.na(x) & nzchar(trimws(x))]))
  paste(sort(x), collapse = " | ")
}

slug <- function(x) {
  x <- iconv(tolower(x), from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

empty_outputs <- function() {
  person_year_cols <- c(
    "faculty_id", "college_id", "event_id", "college", "academic_year_start",
    "academic_year_label", "person_name_canonical", "person_name_variants",
    "roles", "ranks", "disciplines", "divisions", "appointment_statuses",
    "source_ids", "source_pages", "transcription_methods",
    "all_pages_visually_verified", "confidence", "first_year_at_college",
    "last_year_at_college", "new_hire_observed", "departure_observed"
  )
  registry_cols <- c(
    "faculty_id", "person_name_canonical", "person_name_variants", "college_ids",
    "colleges", "first_year_observed", "last_year_observed", "identity_scope"
  )
  counts_cols <- c(
    "college_id", "event_id", "college", "academic_year_start", "academic_year_label",
    "coverage_status", "faculty_count_observation", "n_faculty",
    "n_new_hires_observed", "n_departures_observed", "n_with_known_discipline"
  )
  write.csv(setNames(as.data.frame(matrix(nrow = 0, ncol = length(person_year_cols))), person_year_cols),
            file.path(panel_root, "faculty_person_year.csv"), row.names = FALSE)
  write.csv(setNames(as.data.frame(matrix(nrow = 0, ncol = length(registry_cols))), registry_cols),
            file.path(panel_root, "faculty_person_registry.csv"), row.names = FALSE)
  write.csv(setNames(as.data.frame(matrix(nrow = 0, ncol = length(counts_cols))), counts_cols),
            file.path(panel_root, "faculty_college_year_counts.csv"), row.names = FALSE)
}

if (!nrow(roster)) {
  empty_outputs()
  cat("No accepted roster entries; wrote empty canonical panel headers.\n")
  quit(save = "no", status = 0)
}

required_roster <- c(
  "college_id", "event_id", "college", "academic_year_start", "academic_year_label",
  "person_name_raw", "person_name_normalized", "role_raw", "scope_decision",
  "source_id", "source_page", "transcription_method", "page_visually_verified",
  "confidence"
)
missing_roster <- setdiff(required_roster, names(roster))
if (length(missing_roster)) stop("Roster input lacks: ", paste(missing_roster, collapse = ", "))

roster <- roster[roster$scope_decision == "include", ]
if (!nrow(roster)) {
  empty_outputs()
  cat("No included roster entries; wrote empty canonical panel headers.\n")
  quit(save = "no", status = 0)
}

decision_map <- data.frame()
if (nrow(identity)) {
  accepted <- identity[
    tolower(identity$primary_decision) %in% c("accept", "accepted") & nzchar(trimws(identity$candidate_faculty_id)),
  ]
  if (nrow(accepted)) {
    accepted$key <- paste(accepted$college_id, accepted$person_name_normalized, sep = "\u241f")
    if (anyDuplicated(accepted$key)) stop("Duplicate accepted identity decisions for a college/name key.")
    decision_map <- accepted[, c("key", "candidate_faculty_id")]
  }
}

roster$key <- paste(roster$college_id, roster$person_name_normalized, sep = "\u241f")
roster$faculty_id <- paste0("local_", roster$college_id, "_", slug(roster$person_name_normalized))
if (nrow(decision_map)) {
  idx <- match(roster$key, decision_map$key)
  use <- !is.na(idx)
  roster$faculty_id[use] <- decision_map$candidate_faculty_id[idx[use]]
}

optional <- c(
  rank_normalized = "", discipline_normalized = "", division = "",
  appointment_status = "", discipline_raw = ""
)
for (name in names(optional)) if (!name %in% names(roster)) roster[[name]] <- optional[[name]]

group_key <- paste(roster$faculty_id, roster$college_id, roster$academic_year_start, sep = "\u241f")
groups <- split(seq_len(nrow(roster)), group_key)
person_year <- do.call(rbind, lapply(groups, function(ii) {
  x <- roster[ii, ]
  confidence_rank <- c(high = 1L, medium = 2L, low = 3L)
  conf <- names(which.max(tapply(confidence_rank[x$confidence], x$confidence, max)))
  normalized_names <- trimws(x$person_name_normalized[nzchar(trimws(x$person_name_normalized))])
  data.frame(
    faculty_id = x$faculty_id[[1]],
    college_id = x$college_id[[1]],
    event_id = x$event_id[[1]],
    college = x$college[[1]],
    academic_year_start = as.integer(x$academic_year_start[[1]]),
    academic_year_label = x$academic_year_label[[1]],
    person_name_canonical = if (length(normalized_names)) normalized_names[[1]] else x$person_name_raw[[1]],
    person_name_variants = collapse_values(c(x$person_name_raw, x$person_name_normalized)),
    roles = collapse_values(x$role_raw),
    ranks = collapse_values(x$rank_normalized),
    disciplines = collapse_values(c(x$discipline_normalized, x$discipline_raw)),
    divisions = collapse_values(x$division),
    appointment_statuses = collapse_values(x$appointment_status),
    source_ids = collapse_values(x$source_id),
    source_pages = collapse_values(paste(x$source_id, x$source_page, sep = ": ")),
    transcription_methods = collapse_values(x$transcription_method),
    all_pages_visually_verified = all(toupper(trimws(x$page_visually_verified)) == "TRUE"),
    confidence = conf,
    stringsAsFactors = FALSE
  )
}))
row.names(person_year) <- NULL

faculty_college_key <- paste(person_year$faculty_id, person_year$college_id, sep = "\u241f")
first_year <- ave(person_year$academic_year_start, faculty_college_key, FUN = min)
last_year <- ave(person_year$academic_year_start, faculty_college_key, FUN = max)
person_year$first_year_at_college <- as.integer(first_year)
person_year$last_year_at_college <- as.integer(last_year)

# Only infer entry/exit when the adjacent year's roster is sufficiently complete.
# Gaps, partial rosters, and source failures are not evidence of absence.
adequate_status <- c("complete", "likely_complete")
coverage$status_key <- paste(coverage$college_id, coverage$academic_year_start, sep = "\u241f")
coverage_status_map <- setNames(coverage$coverage_status, coverage$status_key)
present_key <- paste(person_year$faculty_id, person_year$college_id, person_year$academic_year_start, sep = "\u241f")
prior_status_key <- paste(person_year$college_id, person_year$academic_year_start - 1L, sep = "\u241f")
next_status_key <- paste(person_year$college_id, person_year$academic_year_start + 1L, sep = "\u241f")
prior_person_key <- paste(person_year$faculty_id, person_year$college_id, person_year$academic_year_start - 1L, sep = "\u241f")
next_person_key <- paste(person_year$faculty_id, person_year$college_id, person_year$academic_year_start + 1L, sep = "\u241f")
person_year$new_hire_observed <- unname(coverage_status_map[prior_status_key]) %in% adequate_status &
  !prior_person_key %in% present_key
person_year$departure_observed <- unname(coverage_status_map[next_status_key]) %in% adequate_status &
  !next_person_key %in% present_key
person_year <- person_year[order(person_year$college_id, person_year$academic_year_start, person_year$faculty_id), ]

registry_groups <- split(seq_len(nrow(person_year)), person_year$faculty_id)
registry <- do.call(rbind, lapply(registry_groups, function(ii) {
  x <- person_year[ii, ]
  data.frame(
    faculty_id = x$faculty_id[[1]],
    person_name_canonical = x$person_name_canonical[[1]],
    person_name_variants = collapse_values(x$person_name_variants),
    college_ids = collapse_values(x$college_id),
    colleges = collapse_values(x$college),
    first_year_observed = min(x$academic_year_start),
    last_year_observed = max(x$academic_year_start),
    identity_scope = if (length(unique(x$college_id)) > 1L) "cross_college_reviewed" else "within_college",
    stringsAsFactors = FALSE
  )
}))
row.names(registry) <- NULL

count_key <- paste(person_year$college_id, person_year$academic_year_start, sep = "\u241f")
count_groups <- split(seq_len(nrow(person_year)), count_key)
counts <- do.call(rbind, lapply(count_groups, function(ii) {
  x <- person_year[ii, ]
  data.frame(
    college_id = x$college_id[[1]],
    event_id = x$event_id[[1]],
    college = x$college[[1]],
    academic_year_start = x$academic_year_start[[1]],
    academic_year_label = x$academic_year_label[[1]],
    n_faculty = nrow(x),
    n_new_hires_observed = sum(x$new_hire_observed),
    n_departures_observed = sum(x$departure_observed),
    n_with_known_discipline = sum(nzchar(trimws(x$disciplines))),
    stringsAsFactors = FALSE
  )
}))

coverage_small <- coverage[, intersect(c(
  "college_id", "event_id", "college", "academic_year_start", "academic_year_label",
  "coverage_status"
), names(coverage)), drop = FALSE]
if (nrow(coverage_small)) {
  observed_counts <- counts[, c(
    "college_id", "academic_year_start", "n_faculty", "n_new_hires_observed",
    "n_departures_observed", "n_with_known_discipline"
  )]
  counts <- merge(
    coverage_small, observed_counts,
    by = c("college_id", "academic_year_start"), all.x = TRUE, sort = FALSE
  )
  full_zero <- counts$coverage_status %in% adequate_status & is.na(counts$n_faculty)
  count_cols <- c("n_faculty", "n_new_hires_observed", "n_departures_observed", "n_with_known_discipline")
  for (name in count_cols) counts[[name]][full_zero] <- 0L
  counts$faculty_count_observation <- ifelse(
    counts$coverage_status %in% adequate_status,
    "observed_roster",
    ifelse(counts$coverage_status == "partial" & !is.na(counts$n_faculty),
           "observed_lower_bound", "missing")
  )
} else {
  counts$coverage_status <- ""
  counts$faculty_count_observation <- "missing"
}
counts <- counts[, c(
  "college_id", "event_id", "college", "academic_year_start", "academic_year_label",
  "coverage_status", "faculty_count_observation", "n_faculty",
  "n_new_hires_observed", "n_departures_observed", "n_with_known_discipline"
)]
counts <- counts[order(counts$college_id, counts$academic_year_start), ]

write.csv(person_year, file.path(panel_root, "faculty_person_year.csv"), row.names = FALSE, na = "")
write.csv(registry, file.path(panel_root, "faculty_person_registry.csv"), row.names = FALSE, na = "")
write.csv(counts, file.path(panel_root, "faculty_college_year_counts.csv"), row.names = FALSE, na = "")

cat(sprintf("Built %d person-years for %d faculty IDs across %d college-years.\n",
            nrow(person_year), nrow(registry), nrow(counts)))
