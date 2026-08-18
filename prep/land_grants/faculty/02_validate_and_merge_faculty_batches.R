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
assignment_path <- file.path(panel_root, "college_batch_assignments.csv")
manifest_path <- file.path(panel_root, "college_year_work_manifest.csv")
stopifnot(file.exists(assignment_path), file.exists(manifest_path), dir.exists(staging_root))

assignments <- read.csv(assignment_path, stringsAsFactors = FALSE, check.names = FALSE)
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)

required <- list(
  sources = c("batch_id", "college_id", "source_id", "source_title", "source_type", "source_url", "source_status"),
  roster_entries = c("batch_id", "roster_entry_id", "college_id", "academic_year_start", "person_name_raw", "person_name_normalized", "role_raw", "scope_decision", "teaching_evidence", "source_id", "source_page", "confidence", "transcription_method", "page_visually_verified", "verification_notes"),
  coverage = c("batch_id", "college_id", "academic_year_start", "source_ids", "found_roster_count", "roster_pages_reviewed", "manual_verification_status", "coverage_status", "gap_reason"),
  manual_page_log = c("batch_id", "page_review_id", "college_id", "academic_year_start", "source_id", "source_page", "visual_review_status", "n_included_rows", "n_excluded_rows", "reviewer_notes", "reviewed_at"),
  identity_proposals = c("batch_id", "proposal_id", "college_id", "local_person_key", "link_scope", "confidence"),
  batch_summary = c("batch_id", "institutions_expected", "institutions_completed", "review_status")
)

allowed_coverage <- c(
  "complete", "likely_complete", "partial", "source_found_not_processed",
  "source_located_inaccessible", "not_found"
)
allowed_scope <- c("include", "exclude", "review")
allowed_confidence <- c("high", "medium", "low")
allowed_final_source_status <- c(
  "manually_transcribed", "source_located_inaccessible", "not_roster_bearing",
  "duplicate_source"
)
accepted_batches <- character()
validation_rows <- list()

has_content_quality_issue <- function(roster) {
  if (!nrow(roster)) return(logical())
  included <- roster$scope_decision == "include"
  name <- trimws(roster$person_name_normalized)
  role <- trimws(roster$role_raw)
  bad_name_glyph <- grepl("[0-9\\\\^|<>]", name)
  honorific_as_name <- grepl(
    "^(lt|miss|mrs|mr|dr|prof|rev|capt|captain|major|colonel)\\.?\\s+",
    tolower(name)
  )
  split_word_name <- grepl("\\b[[:alpha:]]\\s+[[:lower:]]{2,3}\\s+[[:lower:]]{2,3}\\b", name) |
    grepl("\\b[[:alpha:]]\\s+[[:lower:]]\\s+[[:lower:]]{2,}\\b", name)
  mojibake_name <- grepl("â|€|™|�", name)
  status_as_name <- grepl(
    "\\b(vacant|vacancy|retired|resigned|appointed|elected|deceased|died|to be filled|position open)\\b",
    tolower(name)
  )
  rank_as_name <- grepl(
    "^(professor|instructor|assistant|associate|dean|president|faculty|officers?|staff)\\b",
    tolower(name)
  )
  too_few_letters <- nchar(gsub("[^[:alpha:]]", "", name)) < 3L
  bad_role_glyph <- grepl("[\\\\^|<>]", role)
  broken_role_word <- grepl("[[:alpha:]]+-\\s+[[:lower:]]", role)
  mojibake_role <- grepl("â|€|™|�", role)
  included & (!nzchar(name) | bad_name_glyph | honorific_as_name | split_word_name |
                mojibake_name | status_as_name | rank_as_name | too_few_letters |
                bad_role_glyph | broken_role_word | mojibake_role)
}

read_stage <- function(batch_dir, name) {
  path <- file.path(batch_dir, paste0(name, ".csv"))
  if (!file.exists(path)) stop("Missing staging file: ", path)
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  missing_cols <- setdiff(required[[name]], names(x))
  if (length(missing_cols)) stop("Missing columns in ", path, ": ", paste(missing_cols, collapse = ", "))
  x
}

data_names <- setdiff(names(required), "batch_summary")
all_data <- setNames(lapply(data_names, function(x) list()), data_names)

for (batch_id in unique(assignments$batch_id)) {
  batch_dir <- file.path(staging_root, batch_id)
  summary <- read_stage(batch_dir, "batch_summary")
  is_accepted <- nrow(summary) == 1L && identical(summary$review_status[[1]], "accepted")
  errors <- character()

  if (is_accepted) {
    stage <- lapply(data_names, function(name) read_stage(batch_dir, name))
    names(stage) <- data_names
    expected_colleges <- assignments$college_id[assignments$batch_id == batch_id]
    expected_years <- manifest[manifest$batch_id == batch_id, c("college_id", "academic_year_start")]

    if (length(expected_colleges) != 3L) errors <- c(errors, "batch does not contain exactly three assigned colleges")
    if (!setequal(unique(stage$coverage$college_id), expected_colleges)) errors <- c(errors, "coverage college IDs do not match assignment")
    coverage_key <- paste(stage$coverage$college_id, stage$coverage$academic_year_start, sep = "|")
    expected_key <- paste(expected_years$college_id, expected_years$academic_year_start, sep = "|")
    if (anyDuplicated(coverage_key)) errors <- c(errors, "duplicate college-year coverage rows")
    if (!setequal(coverage_key, expected_key)) errors <- c(errors, "coverage does not span every assigned college-year")
    if (any(!stage$coverage$coverage_status %in% allowed_coverage)) errors <- c(errors, "invalid coverage status")
    if (any(stage$coverage$coverage_status == "source_found_not_processed")) errors <- c(errors, "accepted batch retains accessible unprocessed sources")
    source_required <- stage$coverage$coverage_status %in% c(
      "complete", "likely_complete", "partial", "source_found_not_processed",
      "source_located_inaccessible"
    )
    if (any(source_required & !nzchar(trimws(stage$coverage$source_ids)))) errors <- c(errors, "covered or located years lack source IDs")
    source_tokens <- unique(trimws(unlist(strsplit(stage$coverage$source_ids[source_required], "[|;]", fixed = FALSE))))
    source_tokens <- source_tokens[nzchar(source_tokens)]
    if (any(!source_tokens %in% stage$sources$source_id)) errors <- c(errors, "coverage references missing source IDs")
    inaccessible <- stage$coverage$coverage_status == "source_located_inaccessible"
    if (any(inaccessible & !nzchar(trimws(stage$coverage$gap_reason)))) errors <- c(errors, "inaccessible sources lack obstacle documentation")
    covered_status <- stage$coverage$coverage_status %in% c("complete", "likely_complete", "partial")
    pages_reviewed <- suppressWarnings(as.integer(stage$coverage$roster_pages_reviewed))
    if (any(covered_status & (is.na(pages_reviewed) | pages_reviewed < 1L))) errors <- c(errors, "covered years lack manual page-review counts")
    manual_status_ok <- !is.na(stage$coverage$manual_verification_status) &
      ((covered_status & stage$coverage$manual_verification_status %in% c("complete", "partial")) |
         (!covered_status & stage$coverage$manual_verification_status == "not_applicable"))
    if (any(!manual_status_ok)) errors <- c(errors, "invalid coverage manual-verification status")
    page_log_key <- paste(stage$manual_page_log$college_id, stage$manual_page_log$source_id,
                          stage$manual_page_log$source_page, sep = "|")
    if (anyDuplicated(stage$manual_page_log$page_review_id)) errors <- c(errors, "duplicate manual page-review IDs")
    if (anyDuplicated(page_log_key)) errors <- c(errors, "duplicate manual page keys")
    manual_page_year_key <- paste(stage$manual_page_log$college_id,
                                  stage$manual_page_log$academic_year_start, sep = "|")
    if (nrow(stage$manual_page_log) && any(!manual_page_year_key %in% expected_key)) errors <- c(errors, "manual page log contains out-of-target years")
    if (nrow(stage$manual_page_log) && any(!stage$manual_page_log$source_id %in% stage$sources$source_id)) errors <- c(errors, "manual page log references missing source IDs")
    used_source_idx <- match(unique(stage$manual_page_log$source_id), stage$sources$source_id)
    used_source_ranges <- if (length(used_source_idx)) stage$sources$roster_pages[used_source_idx] else character()
    if (length(used_source_ranges) && any(is.na(used_source_ranges) | !nzchar(trimws(used_source_ranges)) |
                                          !grepl("[0-9]", used_source_ranges))) errors <- c(errors, "used sources lack full numeric roster-page ranges")
    if (nrow(stage$manual_page_log) && any(is.na(stage$manual_page_log$visual_review_status) |
                                          stage$manual_page_log$visual_review_status != "complete")) errors <- c(errors, "manual page log retains incomplete visual reviews")
    if (nrow(stage$manual_page_log) && any(is.na(stage$manual_page_log$reviewer_notes) |
                                          !nzchar(trimws(stage$manual_page_log$reviewer_notes)))) errors <- c(errors, "manual page log lacks reviewer notes")
    if (nrow(stage$manual_page_log) && any(is.na(stage$manual_page_log$reviewed_at) |
                                          !nzchar(trimws(stage$manual_page_log$reviewed_at)))) errors <- c(errors, "manual page log lacks review timestamps")
    valid_logged_page_locator <- grepl(
      "(pdf|printed|scan|image|page|p{1,2}\\.?)[^0-9]{0,12}[0-9]",
      tolower(stage$manual_page_log$source_page)
    )
    if (nrow(stage$manual_page_log) && any(!valid_logged_page_locator)) errors <- c(errors, "manual page log lacks image-page locators")
    logged_by_year <- if (nrow(stage$manual_page_log)) table(paste(
      stage$manual_page_log$college_id, stage$manual_page_log$academic_year_start, sep = "|"
    )) else integer()
    logged_counts <- as.integer(logged_by_year[coverage_key])
    logged_counts[is.na(logged_counts)] <- 0L
    if (anyNA(pages_reviewed) || !all(pages_reviewed == logged_counts)) errors <- c(errors, "coverage page counts do not match manual page log")
    if (nrow(stage$roster_entries) && any(!stage$roster_entries$scope_decision %in% allowed_scope)) errors <- c(errors, "invalid scope decision")
    if (nrow(stage$roster_entries) && any(stage$roster_entries$scope_decision == "review")) errors <- c(errors, "accepted batch retains unresolved scope decisions")
    if (nrow(stage$roster_entries) && any(!stage$roster_entries$confidence %in% allowed_confidence)) errors <- c(errors, "invalid roster confidence")
    if (anyDuplicated(stage$sources$source_id)) errors <- c(errors, "duplicate source IDs")
    if (nrow(stage$sources) && any(!stage$sources$source_status %in% allowed_final_source_status)) errors <- c(errors, "source inventory retains nonfinal statuses")
    nonmanual_source <- stage$sources$source_status != "manually_transcribed"
    if (any(nonmanual_source & (is.na(stage$sources$notes) | !nzchar(trimws(stage$sources$notes))))) errors <- c(errors, "nonmanual source dispositions lack notes")
    manual_source_ids <- stage$sources$source_id[stage$sources$source_status == "manually_transcribed"]
    if (length(manual_source_ids) && any(!manual_source_ids %in% stage$manual_page_log$source_id)) errors <- c(errors, "manually transcribed sources lack page logs")
    if (nrow(stage$manual_page_log) && any(!stage$manual_page_log$source_id %in% manual_source_ids)) errors <- c(errors, "page logs reference sources not marked manually transcribed")
    if (anyDuplicated(stage$roster_entries$roster_entry_id)) errors <- c(errors, "duplicate roster entry IDs")
    if (nrow(stage$roster_entries)) {
      included <- stage$roster_entries$scope_decision == "include"
      decided <- stage$roster_entries$scope_decision %in% c("include", "exclude")
      if (any(included & !nzchar(trimws(stage$roster_entries$teaching_evidence)))) errors <- c(errors, "included entries lack teaching evidence")
      bad_page <- decided & (is.na(stage$roster_entries$source_page) | !nzchar(trimws(stage$roster_entries$source_page)))
      valid_page_locator <- grepl(
        "(pdf|printed|scan|image|page|p{1,2}\\.?)[^0-9]{0,12}[0-9]",
        tolower(stage$roster_entries$source_page)
      )
      bad_page <- bad_page | (decided & !valid_page_locator)
      if (any(bad_page)) errors <- c(errors, "included entries lack page locators")
      bad_method <- decided & (is.na(stage$roster_entries$transcription_method) | stage$roster_entries$transcription_method != "manual_visual")
      if (any(bad_method)) errors <- c(errors, "included entries were not manually transcribed")
      bad_visual <- decided & (is.na(stage$roster_entries$page_visually_verified) |
        toupper(trimws(stage$roster_entries$page_visually_verified)) != "TRUE")
      if (any(bad_visual)) errors <- c(errors, "included entries were not visually page-verified")
      bad_verification_notes <- decided & (is.na(stage$roster_entries$verification_notes) |
        !nzchar(trimws(stage$roster_entries$verification_notes)))
      if (any(bad_verification_notes)) errors <- c(errors, "included entries lack manual-verification notes")
      roster_page_key <- paste(stage$roster_entries$college_id[included],
                               stage$roster_entries$source_id[included],
                               stage$roster_entries$source_page[included], sep = "|")
      if (any(!roster_page_key %in% page_log_key)) errors <- c(errors, "included roster entries do not resolve to the manual page log")
      actual_included_by_page <- if (length(roster_page_key)) table(roster_page_key) else integer()
      actual_included_count <- as.integer(actual_included_by_page[page_log_key])
      actual_included_count[is.na(actual_included_count)] <- 0L
      logged_included_count <- suppressWarnings(as.integer(stage$manual_page_log$n_included_rows))
      logged_excluded_count <- suppressWarnings(as.integer(stage$manual_page_log$n_excluded_rows))
      excluded <- stage$roster_entries$scope_decision == "exclude"
      excluded_page_key <- if (any(excluded)) paste(
        stage$roster_entries$college_id[excluded], stage$roster_entries$source_id[excluded],
        stage$roster_entries$source_page[excluded], sep = "|"
      ) else character()
      actual_excluded_by_page <- if (length(excluded_page_key)) table(excluded_page_key) else integer()
      actual_excluded_count <- as.integer(actual_excluded_by_page[page_log_key])
      actual_excluded_count[is.na(actual_excluded_count)] <- 0L
      if (anyNA(logged_included_count) || anyNA(logged_excluded_count) ||
          !all(logged_included_count == actual_included_count) ||
          !all(logged_excluded_count == actual_excluded_count)) {
        errors <- c(errors, "manual page-log line counts are inconsistent")
      }
      if (any(!stage$roster_entries$source_id %in% stage$sources$source_id)) errors <- c(errors, "roster entry references missing source ID")
      if (any(has_content_quality_issue(stage$roster_entries))) errors <- c(errors, "included entries fail the OCR/content-quality screen")
    }
    included <- if (nrow(stage$roster_entries)) stage$roster_entries$scope_decision == "include" else logical()
    count_by_year <- if (any(included)) {
      included_key <- paste(
        stage$roster_entries$college_id[included],
        stage$roster_entries$academic_year_start[included], sep = "|"
      )
      tapply(stage$roster_entries$person_name_normalized[included], included_key,
             function(x) length(unique(trimws(x))))
    } else integer()
    calculated_count <- as.integer(count_by_year[coverage_key])
    calculated_count[is.na(calculated_count)] <- 0L
    reported_count <- suppressWarnings(as.integer(stage$coverage$found_roster_count))
    if (anyNA(reported_count) || !all(reported_count == calculated_count)) {
      errors <- c(errors, "coverage found_roster_count does not equal unique included people")
    }

    if (!length(errors)) {
      accepted_batches <- c(accepted_batches, batch_id)
      for (name in names(stage)) all_data[[name]][[length(all_data[[name]]) + 1L]] <- stage[[name]]
    }
  }

  validation_rows[[length(validation_rows) + 1L]] <- data.frame(
    batch_id = batch_id,
    review_status = if (nrow(summary)) summary$review_status[[1]] else "missing",
    accepted_for_merge = is_accepted && !length(errors),
    validation_errors = paste(errors, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

validation <- do.call(rbind, validation_rows)
write.csv(validation, file.path(panel_root, "batch_validation.csv"), row.names = FALSE, na = "")

bind_or_empty <- function(parts, columns) {
  if (!length(parts)) {
    return(setNames(as.data.frame(matrix(nrow = 0, ncol = length(columns))), columns))
  }
  do.call(rbind, parts)
}

for (name in names(all_data)) {
  write.csv(bind_or_empty(all_data[[name]], required[[name]]), file.path(panel_root, paste0("faculty_", name, "_accepted.csv")), row.names = FALSE, na = "")
}

cat(sprintf("Validated %d batches; %d accepted and merged.\n", nrow(validation), length(accepted_batches)))
