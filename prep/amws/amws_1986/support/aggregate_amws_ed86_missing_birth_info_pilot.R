###############################################################################
# Validate and aggregate the four AMWS Ed86 missing-birth-information pilot
# batches. This script writes audit outputs only and never modifies canonical
# AMWS data.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..",
                                       ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

local_dropbox <- file.path("C:/Users", Sys.info()[["user"]],
                           "Globtalent Dropbox", "gtl_talent_dets")
if (!dir.exists(TALENT_DETS_DATA_DIR) && dir.exists(local_dropbox)) {
  TALENT_DETS_DATA_DIR <- normalizePath(local_dropbox, winslash = "/")
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | str_to_upper(str_trim(x)) == "NA", "", x)
}
filled <- function(x) nzchar(str_trim(blank_na(x)))

pilot_root <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "intermediary", "amws",
  "manual_missing_birth_info_pilot_20260713"
)
out_dir <- file.path(pilot_root, "out")
master_file <- file.path(pilot_root,
                         "amws_ed86_missing_birth_info_sample200_master.csv")
aggregate_file <- file.path(
  pilot_root, "amws_ed86_missing_birth_info_sample200_reviews.csv"
)
summary_file <- file.path(
  pilot_root, "amws_ed86_missing_birth_info_sample200_review_summary.csv"
)

master <- read_csv(master_file, col_types = cols(.default = col_character()),
                   show_col_types = FALSE, progress = FALSE) |>
  mutate(across(everything(), blank_na))

review_cols <- c(
  "sample_id", "batch_id", "doc_id", "lineid", "entry_instance",
  "birth_place_proposed", "birth_date_proposed", "birth_year_proposed",
  "birth_city_proposed", "birth_state_proposed", "birth_country_proposed",
  "recovery_status", "manual_confidence", "evidence_basis",
  "other_issue_flag", "manual_note", "reviewer_id"
)
key_cols <- c("sample_id", "batch_id", "doc_id", "lineid", "entry_instance")
files <- sort(list.files(
  out_dir,
  pattern = "^amws_ed86_missing_birth_info_batch_[0-9]{2}_reviews\\.csv$",
  full.names = TRUE
))
if (length(files) != 4L) {
  stop("Expected four reviewed batch files, found ", length(files), ".")
}

reviews <- bind_rows(lapply(files, function(path) {
  x <- read_csv(path, col_types = cols(.default = col_character()),
                show_col_types = FALSE, progress = FALSE) |>
    mutate(across(everything(), blank_na))
  missing <- setdiff(review_cols, names(x))
  if (length(missing)) {
    stop(basename(path), " is missing columns: ", paste(missing, collapse = ", "))
  }
  if (nrow(x) != 50L || ncol(x) != length(review_cols)) {
    stop(basename(path), " must contain exactly 50 rows and the review schema.")
  }
  x |> select(all_of(review_cols)) |>
    mutate(review_file = basename(path))
}))

if (nrow(reviews) != 200L ||
    n_distinct(paste(reviews$doc_id, reviews$lineid, sep = "\r")) != 200L ||
    any((reviews |> count(batch_id))$n != 50L)) {
  stop("Review row count, uniqueness, or batch size validation failed.")
}
if (nrow(anti_join(reviews, master, by = key_cols)) ||
    nrow(anti_join(master, reviews, by = key_cols))) {
  stop("Reviewed keys do not match the sample master.")
}

allowed_status <- c("fully_recoverable", "partially_recoverable",
                    "not_recoverable", "review_unclear")
allowed_confidence <- c("high", "medium", "low")
allowed_basis <- c("ocr_explicit", "ocr_fragment",
                   "state_or_province_suffix", "mixed", "no_evidence",
                   "ambiguous")
if (length(setdiff(unique(reviews$recovery_status), allowed_status)) ||
    length(setdiff(unique(reviews$manual_confidence), allowed_confidence)) ||
    length(setdiff(unique(reviews$evidence_basis), allowed_basis)) ||
    any(!filled(reviews$manual_note)) || any(!filled(reviews$reviewer_id))) {
  stop("Invalid or missing review metadata.")
}

reviewed <- master |>
  select(-birth_place_proposed, -birth_date_proposed, -birth_year_proposed,
         -birth_city_proposed, -birth_state_proposed, -birth_country_proposed,
         -recovery_status, -manual_confidence, -evidence_basis,
         -other_issue_flag, -manual_note, -reviewer_id) |>
  left_join(reviews, by = key_cols) |>
  mutate(
    other_issue_flag_raw = other_issue_flag,
    other_issue_flag = if_else(
      !filled(other_issue_flag_raw) |
        str_to_upper(str_trim(other_issue_flag_raw)) == "FALSE",
      "FALSE", "TRUE"
    ),
    other_issue_detail = case_when(
      !filled(other_issue_flag_raw) ~ "",
      str_to_upper(str_trim(other_issue_flag_raw)) %in% c("TRUE", "FALSE") ~ "",
      TRUE ~ other_issue_flag_raw
    ),
    recovered_birth_city = missing_birth_city == "TRUE" &
      filled(birth_city_proposed),
    recovered_birth_year = missing_birth_year == "TRUE" &
      filled(birth_year_proposed),
    recovered_birth_country = missing_birth_country == "TRUE" &
      filled(birth_country_proposed),
    missing_required_n = (missing_birth_city == "TRUE") +
      (missing_birth_year == "TRUE") + (missing_birth_country == "TRUE"),
    recovered_required_n = recovered_birth_city + recovered_birth_year +
      recovered_birth_country,
    final_eligible_after_proposal =
      filled(if_else(missing_birth_city == "TRUE", birth_city_proposed,
                     birth_city_old)) &
      filled(if_else(missing_birth_year == "TRUE", birth_year_proposed,
                     birth_year_old)) &
      filled(if_else(missing_birth_country == "TRUE", birth_country_proposed,
                     birth_country_old)) &
      manual_confidence %in% c("high", "medium")
  )

if (any(reviewed$missing_birth_city != "TRUE" &
        filled(reviewed$birth_city_proposed)) ||
    any(reviewed$missing_birth_year != "TRUE" &
        filled(reviewed$birth_year_proposed)) ||
    any(reviewed$missing_birth_country != "TRUE" &
        filled(reviewed$birth_country_proposed))) {
  stop("A review proposed a replacement for a nonmissing required field.")
}
if (any(filled(reviewed$birth_state_proposed) &
        filled(reviewed$birth_state_old))) {
  stop("A review proposed a state where the old state was already nonmissing.")
}

full <- reviewed$recovery_status == "fully_recoverable"
partial <- reviewed$recovery_status == "partially_recoverable"
none <- reviewed$recovery_status == "not_recoverable"
unclear <- reviewed$recovery_status == "review_unclear"
if (any(full & (!reviewed$final_eligible_after_proposal |
                !reviewed$manual_confidence %in% c("high", "medium"))) ||
    any(partial & (reviewed$recovered_required_n == 0L |
                   reviewed$recovered_required_n >= reviewed$missing_required_n)) ||
    any(none & reviewed$recovered_required_n != 0L) ||
    any(unclear & reviewed$manual_confidence != "low")) {
  stop("Recovery statuses are inconsistent with the proposed values.")
}
if (any(reviewed$final_eligible_after_proposal != full)) {
  stop("Final eligibility must match fully_recoverable exactly.")
}

eligible_universe_n <- 14483L
sample_n <- nrow(reviewed)
full_n <- sum(full)
partial_n <- sum(partial)
exact_ci <- binom.test(full_n, sample_n, conf.level = 0.95)$conf.int
full_share <- full_n / sample_n

summary <- bind_rows(
  tibble(metric = "sample_rows", value = as.character(sample_n)),
  tibble(metric = "sample_unique_doc_lineid",
         value = as.character(n_distinct(paste(reviewed$doc_id,
                                              reviewed$lineid)))),
  tibble(metric = "batch_count", value = as.character(n_distinct(reviewed$batch_id))),
  tibble(metric = "batch_size", value = "50"),
  tibble(metric = "eligible_universe_rows", value = as.character(eligible_universe_n)),
  tibble(metric = "fully_recoverable_rows", value = as.character(full_n)),
  tibble(metric = "partially_recoverable_rows", value = as.character(partial_n)),
  tibble(metric = "fully_recoverable_share", value = sprintf("%.6f", full_share)),
  tibble(metric = "fully_recoverable_exact_ci95_low",
         value = sprintf("%.6f", exact_ci[[1]])),
  tibble(metric = "fully_recoverable_exact_ci95_high",
         value = sprintf("%.6f", exact_ci[[2]])),
  tibble(metric = "projected_fully_recoverable_rows",
         value = as.character(round(eligible_universe_n * full_share))),
  tibble(metric = "projected_fully_recoverable_ci95_low",
         value = as.character(round(eligible_universe_n * exact_ci[[1]]))),
  tibble(metric = "projected_fully_recoverable_ci95_high",
         value = as.character(round(eligible_universe_n * exact_ci[[2]]))),
  tibble(metric = "rows_with_other_issue_flag",
         value = as.character(sum(reviewed$other_issue_flag == "TRUE"))),
  tibble(metric = "fully_recoverable_with_other_issue_flag",
         value = as.character(sum(full & reviewed$other_issue_flag == "TRUE"))),
  reviewed |> count(recovery_status, name = "n") |>
    transmute(metric = paste0("status:", recovery_status), value = as.character(n)),
  reviewed |> count(manual_confidence, name = "n") |>
    transmute(metric = paste0("confidence:", manual_confidence), value = as.character(n)),
  reviewed |> count(missing_fields, recovery_status, name = "n") |>
    transmute(metric = paste0("pattern_status:", missing_fields, ":",
                              recovery_status), value = as.character(n)),
  reviewed |> count(batch_id, recovery_status, name = "n") |>
    transmute(metric = paste0("batch_status:", batch_id, ":", recovery_status),
              value = as.character(n)),
  tibble(metric = "master_md5", value = unname(tools::md5sum(master_file))),
  tibble(metric = "review_file_count", value = as.character(length(files)))
)

write_excel_csv(reviewed, aggregate_file, na = "")
write_excel_csv(summary, summary_file, na = "")

cat("Reviewed rows:", nrow(reviewed), "\n")
cat("Fully recoverable:", full_n, "\n")
cat("Partially recoverable:", partial_n, "\n")
cat("Projected fully recoverable:", round(eligible_universe_n * full_share), "\n")
cat("95% exact projection interval:",
    round(eligible_universe_n * exact_ci[[1]]), "to",
    round(eligible_universe_n * exact_ci[[2]]), "\n")
cat("Aggregate:", aggregate_file, "\n")
cat("Summary:", summary_file, "\n")
