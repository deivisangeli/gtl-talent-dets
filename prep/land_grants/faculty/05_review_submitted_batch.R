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
batch_id <- if (length(batch_arg)) sub("^--batch=", "", batch_arg[[1]]) else Sys.getenv("FACULTY_BATCH_ID")
if (!grepl("^batch_[0-9]{2}$", batch_id)) stop("Supply --batch=batch_NN or FACULTY_BATCH_ID=batch_NN.")

panel_root <- file.path(DATA_OUTPUT, "land_grants", "faculty_longitudinal")
batch_dir <- file.path(panel_root, "staging", batch_id)
review_dir <- file.path(panel_root, "reviews", batch_id)
dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)

read_stage <- function(name) {
  path <- file.path(batch_dir, paste0(name, ".csv"))
  if (!file.exists(path)) stop("Missing ", path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

targets <- read_stage("targets")
sources <- read_stage("sources")
roster <- read_stage("roster_entries")
coverage <- read_stage("coverage")
page_log <- read_stage("manual_page_log")
identity <- read_stage("identity_proposals")
summary <- read_stage("batch_summary")
legacy <- read_stage("legacy_seed")

required_manual_columns <- c("transcription_method", "page_visually_verified", "verification_notes")
missing_manual_columns <- setdiff(required_manual_columns, names(roster))
if (length(missing_manual_columns)) {
  stop("Roster lacks manual-verification columns: ", paste(missing_manual_columns, collapse = ", "))
}

included <- if (nrow(roster)) roster$scope_decision == "include" else logical()
decided <- if (nrow(roster)) roster$scope_decision %in% c("include", "exclude") else logical()
name_normalized <- if (nrow(roster)) trimws(roster$person_name_normalized) else character()
role_text <- if (nrow(roster)) trimws(roster$role_raw) else character()
valid_page_locator <- if (nrow(roster)) grepl(
  "(pdf|printed|scan|image|page|p{1,2}\\.?)[^0-9]{0,12}[0-9]",
  tolower(roster$source_page)
) else logical()
bad_name_glyph <- grepl("[0-9\\\\^|<>]", name_normalized)
honorific_as_name <- grepl(
  "^(lt|miss|mrs|mr|dr|prof|rev|capt|captain|major|colonel)\\.?\\s+",
  tolower(name_normalized)
)
split_word_name <- grepl("\\b[[:alpha:]]\\s+[[:lower:]]{2,3}\\s+[[:lower:]]{2,3}\\b", name_normalized) |
  grepl("\\b[[:alpha:]]\\s+[[:lower:]]\\s+[[:lower:]]{2,}\\b", name_normalized)
mojibake_name <- grepl("â|€|™|�", name_normalized)
status_as_name <- grepl(
  "\\b(vacant|vacancy|retired|resigned|appointed|elected|deceased|died|to be filled|position open)\\b",
  tolower(name_normalized)
)
rank_as_name <- grepl(
  "^(professor|instructor|assistant|associate|dean|president|faculty|officers?|staff)\\b",
  tolower(name_normalized)
)
too_few_letters <- nchar(gsub("[^[:alpha:]]", "", name_normalized)) < 3L
bad_role_glyph <- grepl("[\\\\^|<>]", role_text)
broken_role_word <- grepl("[[:alpha:]]+-\\s+[[:lower:]]", role_text)
mojibake_role <- grepl("â|€|™|�", role_text)
content_flag <- included & (
  !nzchar(name_normalized) | bad_name_glyph | honorific_as_name | split_word_name |
    mojibake_name | status_as_name | rank_as_name | too_few_letters |
    bad_role_glyph | broken_role_word | mojibake_role
)

content_quality_flags <- if (any(content_flag)) {
  out <- roster[content_flag, , drop = FALSE]
  out$content_quality_reason <- vapply(which(content_flag), function(i) {
    reasons <- character()
    if (!nzchar(name_normalized[[i]])) reasons <- c(reasons, "missing_normalized_name")
    if (bad_name_glyph[[i]]) reasons <- c(reasons, "name_has_digit_or_ocr_glyph")
    if (honorific_as_name[[i]]) reasons <- c(reasons, "honorific_in_normalized_name")
    if (split_word_name[[i]]) reasons <- c(reasons, "possible_split_word_in_name")
    if (mojibake_name[[i]]) reasons <- c(reasons, "mojibake_in_name")
    if (status_as_name[[i]]) reasons <- c(reasons, "status_or_vacancy_text_as_name")
    if (rank_as_name[[i]]) reasons <- c(reasons, "rank_or_header_text_as_name")
    if (too_few_letters[[i]]) reasons <- c(reasons, "too_few_name_letters")
    if (bad_role_glyph[[i]]) reasons <- c(reasons, "role_has_ocr_glyph")
    if (broken_role_word[[i]]) reasons <- c(reasons, "role_has_linebreak_hyphenation")
    if (mojibake_role[[i]]) reasons <- c(reasons, "mojibake_in_role")
    paste(reasons, collapse = ";")
  }, character(1))
  out
} else {
  data.frame(
    roster_entry_id = character(), college_id = character(), academic_year_start = integer(),
    person_name_normalized = character(), role_raw = character(),
    content_quality_reason = character(), stringsAsFactors = FALSE
  )
}

target_key <- paste(targets$college_id, targets$academic_year_start, sep = "|")
coverage_key <- if (nrow(coverage)) paste(coverage$college_id, coverage$academic_year_start, sep = "|") else character()
roster_count_by_year <- if (nrow(roster) && any(included)) {
  included_key <- paste(roster$college_id[included], roster$academic_year_start[included], sep = "|")
  tapply(roster$person_name_normalized[included], included_key, function(x) length(unique(trimws(x))))
} else {
  integer()
}
calculated_roster_count <- as.integer(roster_count_by_year[coverage_key])
calculated_roster_count[is.na(calculated_roster_count)] <- 0L
reported_roster_count <- suppressWarnings(as.integer(coverage$found_roster_count))
covered_status <- coverage$coverage_status %in% c("complete", "likely_complete", "partial")
pages_reviewed <- suppressWarnings(as.integer(coverage$roster_pages_reviewed))
roster_page_key <- if (nrow(roster) && any(included)) paste(
  roster$college_id[included], roster$source_id[included], roster$source_page[included], sep = "|"
) else character()
excluded_page_key <- if (nrow(roster) && any(roster$scope_decision == "exclude")) paste(
  roster$college_id[roster$scope_decision == "exclude"],
  roster$source_id[roster$scope_decision == "exclude"],
  roster$source_page[roster$scope_decision == "exclude"], sep = "|"
) else character()
page_log_key <- if (nrow(page_log)) paste(
  page_log$college_id, page_log$source_id, page_log$source_page, sep = "|"
) else character()
logged_pages_by_year <- if (nrow(page_log)) {
  table(paste(page_log$college_id, page_log$academic_year_start, sep = "|"))
} else integer()
logged_page_count <- as.integer(logged_pages_by_year[coverage_key])
logged_page_count[is.na(logged_page_count)] <- 0L
actual_included_by_page <- if (length(roster_page_key)) table(roster_page_key) else integer()
actual_included_count <- as.integer(actual_included_by_page[page_log_key])
actual_included_count[is.na(actual_included_count)] <- 0L
logged_included_count <- suppressWarnings(as.integer(page_log$n_included_rows))
logged_excluded_count <- suppressWarnings(as.integer(page_log$n_excluded_rows))
actual_excluded_by_page <- if (length(excluded_page_key)) table(excluded_page_key) else integer()
actual_excluded_count <- as.integer(actual_excluded_by_page[page_log_key])
actual_excluded_count[is.na(actual_excluded_count)] <- 0L
valid_logged_page_locator <- if (nrow(page_log)) grepl(
  "(pdf|printed|scan|image|page|p{1,2}\\.?)[^0-9]{0,12}[0-9]",
  tolower(page_log$source_page)
) else logical()
used_source_idx <- match(unique(page_log$source_id), sources$source_id)
used_source_ranges <- if (length(used_source_idx)) sources$roster_pages[used_source_idx] else character()
allowed_final_source_status <- c(
  "manually_transcribed", "source_located_inaccessible", "not_roster_bearing",
  "duplicate_source"
)
manual_source_ids <- sources$source_id[sources$source_status == "manually_transcribed"]
source_required <- if (nrow(coverage)) coverage$coverage_status %in% c(
  "complete", "likely_complete", "partial", "source_found_not_processed",
  "source_located_inaccessible"
) else logical()
coverage_source_tokens <- if (any(source_required)) {
  unique(trimws(unlist(strsplit(coverage$source_ids[source_required], "[|;]", fixed = FALSE))))
} else character()
coverage_source_tokens <- coverage_source_tokens[nzchar(coverage_source_tokens)]

checks <- c(
  exactly_three_institutions = length(unique(targets$college_id)) == 3L,
  coverage_one_row_per_target = !anyDuplicated(coverage_key) && setequal(target_key, coverage_key),
  coverage_counts_match_unique_included_people = length(reported_roster_count) == length(calculated_roster_count) &&
    !anyNA(reported_roster_count) && all(reported_roster_count == calculated_roster_count),
  covered_years_have_manual_page_review = !nrow(coverage) || all(
    !covered_status | (!is.na(pages_reviewed) & pages_reviewed > 0L)
  ),
  coverage_manual_status_valid = !nrow(coverage) || all(
    !is.na(coverage$manual_verification_status) &
      ((covered_status & coverage$manual_verification_status %in% c("complete", "partial")) |
         (!covered_status & coverage$manual_verification_status == "not_applicable"))
  ),
  manual_page_ids_unique = !nrow(page_log) || !anyDuplicated(page_log$page_review_id),
  manual_page_keys_unique = !nrow(page_log) || !anyDuplicated(page_log_key),
  manual_pages_in_target = !nrow(page_log) || all(
    paste(page_log$college_id, page_log$academic_year_start, sep = "|") %in% target_key
  ),
  manual_pages_have_sources = !nrow(page_log) || all(page_log$source_id %in% sources$source_id),
  used_sources_document_full_roster_range = !length(used_source_ranges) || all(
    !is.na(used_source_ranges) & nzchar(trimws(used_source_ranges)) & grepl("[0-9]", used_source_ranges)
  ),
  manual_pages_visually_complete = !nrow(page_log) || all(
    !is.na(page_log$visual_review_status) & page_log$visual_review_status == "complete"
  ),
  manual_pages_have_review_notes = !nrow(page_log) || all(
    !is.na(page_log$reviewer_notes) & nzchar(trimws(page_log$reviewer_notes))
  ),
  manual_pages_have_timestamps = !nrow(page_log) || all(
    !is.na(page_log$reviewed_at) & nzchar(trimws(page_log$reviewed_at))
  ),
  manual_pages_have_image_locators = !nrow(page_log) || all(valid_logged_page_locator),
  manual_page_line_counts_valid = !nrow(page_log) || (
    !anyNA(logged_included_count) && !anyNA(logged_excluded_count) &&
      all(logged_included_count == actual_included_count) &&
      all(logged_excluded_count == actual_excluded_count)
  ),
  included_roster_pages_resolve = !length(roster_page_key) || all(roster_page_key %in% page_log_key),
  coverage_page_counts_match_log = length(pages_reviewed) == length(logged_page_count) &&
    !anyNA(pages_reviewed) && all(pages_reviewed == logged_page_count),
  source_ids_unique = !anyDuplicated(sources$source_id),
  source_statuses_final = !nrow(sources) || all(sources$source_status %in% allowed_final_source_status),
  nonmanual_sources_document_disposition = !nrow(sources) || all(
    sources$source_status == "manually_transcribed" |
      (!is.na(sources$notes) & nzchar(trimws(sources$notes)))
  ),
  manually_transcribed_sources_have_page_logs = !length(manual_source_ids) || all(
    manual_source_ids %in% page_log$source_id
  ),
  logged_sources_are_manually_transcribed = !nrow(page_log) || all(
    page_log$source_id %in% manual_source_ids
  ),
  roster_entry_ids_unique = !anyDuplicated(roster$roster_entry_id),
  roster_years_in_target = !nrow(roster) || all(paste(roster$college_id, roster$academic_year_start, sep = "|") %in% target_key),
  included_rows_have_source = !nrow(roster) || all(roster$source_id[roster$scope_decision == "include"] %in% sources$source_id),
  included_rows_have_evidence = !nrow(roster) || all(nzchar(trimws(roster$teaching_evidence[roster$scope_decision == "include"]))),
  included_rows_have_names = !nrow(roster) || all(nzchar(trimws(roster$person_name_raw[roster$scope_decision == "include"]))),
  included_rows_have_page_locators = !nrow(roster) || all(
    !is.na(roster$source_page[decided]) & nzchar(trimws(roster$source_page[decided])) &
      valid_page_locator[decided]
  ),
  included_rows_manually_transcribed = !nrow(roster) || all(
    !is.na(roster$transcription_method[decided]) & roster$transcription_method[decided] == "manual_visual"
  ),
  included_rows_visually_verified = !nrow(roster) || all(
    !is.na(roster$page_visually_verified[decided]) &
      toupper(trimws(roster$page_visually_verified[decided])) == "TRUE"
  ),
  included_rows_have_verification_notes = !nrow(roster) || all(
    !is.na(roster$verification_notes[decided]) & nzchar(trimws(roster$verification_notes[decided]))
  ),
  included_rows_pass_content_screen = !any(content_flag),
  covered_years_have_source_ids = !nrow(coverage) || all(!source_required | nzchar(trimws(coverage$source_ids))),
  coverage_source_ids_resolve = !length(coverage_source_tokens) || all(coverage_source_tokens %in% sources$source_id),
  inaccessible_years_document_obstacle = !nrow(coverage) || all(
    coverage$coverage_status != "source_located_inaccessible" |
      nzchar(trimws(coverage$gap_reason))
  ),
  complete_years_have_sources = !nrow(coverage) || all(
    coverage$coverage_status[coverage$coverage_status %in% c("complete", "likely_complete")] %in% c("complete", "likely_complete") &
      nzchar(trimws(coverage$source_ids[coverage$coverage_status %in% c("complete", "likely_complete")]))
  ),
  summary_submitted = nrow(summary) == 1L && summary$review_status[[1]] == "submitted"
)

classify_source <- function(type, title) {
  x <- tolower(paste(type, title))
  if (grepl("catalog|catalogue|register|yearbook|annual report|regent|trustee", x)) return("tier_1_contemporary_official")
  if (grepl("official|archive|institutional|university history|library", x)) return("tier_2_official_retrospective")
  if (grepl("historical book|state history|local history|government|education report", x)) return("tier_3_historical_government")
  if (grepl("journal|newspaper", x)) return("tier_4_contemporary_secondary")
  "unclassified"
}

if (nrow(sources)) {
  sources$source_priority_tier <- mapply(classify_source, sources$source_type, sources$source_title)
  source_summary <- as.data.frame(table(sources$source_priority_tier), stringsAsFactors = FALSE)
  names(source_summary) <- c("source_priority_tier", "n_sources")
} else {
  source_summary <- data.frame(source_priority_tier = character(), n_sources = integer())
}
source_status_summary <- if (nrow(sources)) {
  out <- as.data.frame(table(sources$source_status), stringsAsFactors = FALSE)
  names(out) <- c("source_status", "n_sources")
  out
} else data.frame(source_status = character(), n_sources = integer())

manual_page_summary <- if (nrow(page_log)) {
  aggregate(
    list(
      pages_reviewed = rep(1L, nrow(page_log)),
      included_lines = suppressWarnings(as.integer(page_log$n_included_rows)),
      excluded_lines = suppressWarnings(as.integer(page_log$n_excluded_rows))
    ),
    by = list(college_id = page_log$college_id, college = page_log$college),
    FUN = sum, na.rm = TRUE
  )
} else data.frame(
  college_id = character(), college = character(), pages_reviewed = integer(),
  included_lines = integer(), excluded_lines = integer()
)

normalize_name <- function(x) {
  x <- iconv(tolower(x), from = "", to = "ASCII//TRANSLIT")
  x <- gsub("\\b(rev|dr|prof|mr|mrs|miss)\\b", " ", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

legacy_reconciliation <- legacy
if (nrow(legacy)) {
  roster_norm <- if (nrow(roster)) normalize_name(roster$person_name_normalized) else character()
  roster_college <- if (nrow(roster)) roster$college_id else character()
  legacy_reconciliation$opening_roster_match <- vapply(seq_len(nrow(legacy)), function(i) {
    any(roster_college == legacy$college_id[[i]] & roster_norm == normalize_name(legacy$person_name_normalized[[i]]))
  }, logical(1))
} else {
  legacy_reconciliation$opening_roster_match <- logical()
}

coverage_summary <- if (nrow(coverage)) {
  out <- as.data.frame(table(coverage$coverage_status), stringsAsFactors = FALSE)
  names(out) <- c("coverage_status", "n_college_years")
  out
} else {
  data.frame(coverage_status = character(), n_college_years = integer())
}

confidence_summary <- if (nrow(roster)) {
  out <- as.data.frame(table(roster$confidence, roster$scope_decision), stringsAsFactors = FALSE)
  names(out) <- c("confidence", "scope_decision", "n_entries")
  out[out$n_entries > 0, ]
} else {
  data.frame(confidence = character(), scope_decision = character(), n_entries = integer())
}

check_report <- data.frame(check = names(checks), passed = unname(checks), stringsAsFactors = FALSE)
write.csv(check_report, file.path(review_dir, "automated_checks.csv"), row.names = FALSE)
write.csv(source_summary, file.path(review_dir, "source_priority_summary.csv"), row.names = FALSE)
write.csv(source_status_summary, file.path(review_dir, "source_status_summary.csv"), row.names = FALSE)
write.csv(manual_page_summary, file.path(review_dir, "manual_page_summary.csv"), row.names = FALSE)
write.csv(coverage_summary, file.path(review_dir, "coverage_summary.csv"), row.names = FALSE)
write.csv(confidence_summary, file.path(review_dir, "confidence_scope_summary.csv"), row.names = FALSE)
write.csv(legacy_reconciliation, file.path(review_dir, "legacy_reconciliation_review.csv"), row.names = FALSE, na = "")
write.csv(content_quality_flags, file.path(review_dir, "content_quality_flags.csv"), row.names = FALSE, na = "")

review_summary <- c(
  paste0("batch_id=", batch_id),
  paste0("institutions=", length(unique(targets$college_id))),
  paste0("target_college_years=", nrow(targets)),
  paste0("sources=", nrow(sources)),
  paste0("manual_pages=", nrow(page_log)),
  paste0("roster_entries=", nrow(roster)),
  paste0("identity_proposals=", nrow(identity)),
  paste0("content_quality_flags=", nrow(content_quality_flags)),
  paste0("automated_checks_passed=", sum(checks), "/", length(checks)),
  paste0("legacy_rows_matched=", if (nrow(legacy_reconciliation)) sum(legacy_reconciliation$opening_roster_match) else 0L,
         "/", nrow(legacy_reconciliation)),
  "primary_decision=pending"
)
writeLines(review_summary, file.path(review_dir, "review_summary.txt"))

cat(paste(review_summary, collapse = "\n"), "\n")
if (!all(checks)) quit(save = "no", status = 2L)
