# Append male and female cutoff flags to saved AMWS predictions, without new inference.
# Run after build_amws_gender_inference.R; requires only data.table.
suppressPackageStartupMessages(library(data.table))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), "..", ".."),
                winslash = "/", mustWork = TRUE)
} else normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/")
source(file.path(repo_root, "paths.R"))
stopifnot(dir.exists(TALENT_DETS_DATA_DIR))
out_dir <- file.path(AMWS_OUTPUT, "gender_inference")
record_file <- file.path(out_dir, "amws_gender_records.csv")
thresholds <- c(.95, .90, .85, .80)
male_flag_cols <- c("male_95", "male_90", "male_85", "male_80")
female_flag_cols <- c("female_95", "female_90", "female_85", "female_80")
flag_cols <- c(male_flag_cols, female_flag_cols)
overall_file <- file.path(out_dir, "amws_gender_threshold_coverage.csv")
edition_file <- file.path(out_dir, "amws_gender_threshold_edition_coverage.csv")
checksum_file <- file.path(out_dir, "amws_gender_threshold_flags_checksums.csv")
validation_file <- file.path(out_dir, "amws_gender_threshold_flags_validation.csv")
owned_reports <- c(overall_file, edition_file, checksum_file, validation_file)

# Treat original fields as text to retain GEOID zero padding and numeric strings.
# Base CSV parsing correctly decodes repeated literal quotes in OCR text.
read_records <- function(path) as.data.table(read.csv(
  path, colClasses = "character", stringsAsFactors = FALSE, na.strings = "",
  check.names = FALSE, encoding = "UTF-8"))
before <- read_records(record_file)
stopifnot(nrow(before) > 0L, !anyDuplicated(before$record_id),
          all(c("proportion_male", "proportion_female", "first_name", "classification_final",
                "validated_gender", "validation_source_url", "validation_notes", "GEOID") %in% names(before)))
original_cols <- setdiff(names(before), female_flag_cols)
output_cols <- c(names(before), setdiff(flag_cols, names(before)))
augmented <- copy(before)
report_columns <- lapply(c(overall_file, edition_file), function(path) {
  if (file.exists(path)) names(fread(path, nrows = 0L)) else character()
})

manifest <- fread(file.path(out_dir, "amws_gender_source_checksums.csv"))
protected_files <- unique(c(
  manifest$path,
  list.files(out_dir, recursive = TRUE, full.names = TRUE),
  list.files(file.path(AMWS_OUTPUT, "gender_pilot"), full.names = TRUE),
  list.files(DATA_OUTPUT, pattern = "^us_panel_county_amws.*\\.csv$", full.names = TRUE),
  list.files(file.path(TALENT_DETS_DATA_DIR, "results", "gi_bill", "cohort_comparison"),
             recursive = TRUE, full.names = TRUE)))
protected_files <- setdiff(protected_files, c(record_file, owned_reports))
stopifnot(all(file.exists(protected_files)))
protected_md5_before <- tools::md5sum(protected_files)
record_md5_before <- unname(tools::md5sum(record_file))

male <- as.numeric(before$proportion_male)
female <- as.numeric(before$proportion_female)
has_name <- !is.na(before$first_name) & nzchar(before$first_name)
usable <- has_name & !is.na(male) & !is.na(female)
stopifnot(all(is.na(male) == is.na(female)),
          all(is.na(male) | (is.finite(male) & male >= 0 & male <= 1)),
          all(is.na(female) | (is.finite(female) & female >= 0 & female <= 1)),
          all(abs(male[usable] + female[usable] - 1) < 1e-8))
documented_male <- !is.na(before$validated_gender) & before$validated_gender == "male"
correction_row <- which(documented_male)
stopifnot(length(correction_row) == 1L,
          before$record_id[correction_row] == "1906:19:17",
          before$birth_year[correction_row] == "1868",
          before$classification_final[correction_row] == "male",
          before$classification_raw[correction_row] == "female",
          male[correction_row] == 0, female[correction_row] == 1,
          !is.na(before$validation_source_url[correction_row]),
          !is.na(before$validation_notes[correction_row]),
          all(is.na(before$validated_gender) | documented_male))

threshold_flag <- function(proportion, available, cutoff,
                           override = rep(FALSE, length(proportion)), override_value = 1L) {
  out <- rep(NA_integer_, length(proportion))
  out[available] <- as.integer(proportion[available] >= cutoff)
  out[override] <- override_value
  out
}
# Inclusive boundaries, initial/missing predictions, and documented override.
for (cutoff in thresholds) {
  male_test <- threshold_flag(c(cutoff, cutoff - .00001, NA, 0), c(TRUE, TRUE, FALSE, TRUE),
                             cutoff, c(FALSE, FALSE, FALSE, TRUE))
  female_test <- threshold_flag(c(cutoff, cutoff - .00001, NA, 1), c(TRUE, TRUE, FALSE, TRUE),
                               cutoff, c(FALSE, FALSE, FALSE, TRUE), override_value = 0L)
  stopifnot(identical(male_test, c(1L, 0L, NA_integer_, 1L)),
            identical(female_test, c(1L, 0L, NA_integer_, 0L)))
}

summarize_threshold <- function(labels, flags, female_flags, cutoff, flag_name,
                                female_flag_name, group = NULL) {
  z <- data.table(edition = before$edition, label = labels, flag = flags, female_flag = female_flags)
  out <- z[, .(men = sum(label == "male"), women = sum(label == "female"),
                ambiguous = sum(label == "ambiguous"), unmatched = sum(label == "unmatched"),
                unreadable = sum(label == "unreadable"), total_records = .N,
                male_flag_zero = sum(flag == 0L, na.rm = TRUE),
                male_flag_missing = sum(is.na(flag)),
                female_flag_zero = sum(female_flag == 0L, na.rm = TRUE),
                female_flag_missing = sum(is.na(female_flag))), by = group]
  out[, `:=`(cutoff = cutoff, flag_column = flag_name, classified = men + women,
              unresolved = ambiguous + unmatched + unreadable, female_flag_column = female_flag_name)]
  out[, `:=`(male_flag_share = men / total_records, coverage = classified / total_records,
              female_flag_share = women / total_records)]
  stopifnot(all(out$classified + out$unresolved == out$total_records),
            all(out$men + out$male_flag_zero + out$male_flag_missing == out$total_records),
            all(out$male_flag_missing == out$unmatched + out$unreadable),
            all(out$women + out$female_flag_zero + out$female_flag_missing == out$total_records),
            all(out$female_flag_missing == out$unmatched + out$unreadable))
  out
}
overall_parts <- edition_parts <- vector("list", length(thresholds))
for (i in seq_along(thresholds)) {
  cutoff <- thresholds[i]
  flag <- threshold_flag(male, usable, cutoff, documented_male)
  female_flag <- threshold_flag(female, usable, cutoff, documented_male, override_value = 0L)
  if (male_flag_cols[i] %in% names(before)) {
    stopifnot(identical(as.integer(before[[male_flag_cols[i]]]), flag))
  }
  set(augmented, j = male_flag_cols[i], value = flag)
  set(augmented, j = female_flag_cols[i], value = female_flag)
  labels <- rep("unmatched", nrow(before))
  labels[!has_name] <- "unreadable"
  labels[usable] <- "ambiguous"
  labels[!is.na(female_flag) & female_flag == 1L] <- "female"
  labels[!is.na(flag) & flag == 1L] <- "male"
  stopifnot(all(is.na(flag) | flag %in% c(0L, 1L)),
            all(flag[correction_row] == 1L),
            identical(is.na(flag), !usable),
            all(is.na(female_flag) | female_flag %in% c(0L, 1L)),
            identical(is.na(female_flag), !usable),
            all(female_flag[correction_row] == 0L),
            all((flag + female_flag)[usable] <= 1L))
  if (cutoff == .95) {
    stopifnot(identical(labels, before$classification_final),
              identical(!is.na(flag) & flag == 1L, before$classification_final == "male"),
              identical(!is.na(female_flag) & female_flag == 1L,
                        before$classification_final == "female"))
  }
  overall_parts[[i]] <- summarize_threshold(labels, flag, female_flag, cutoff,
                                           male_flag_cols[i], female_flag_cols[i])
  edition_parts[[i]] <- summarize_threshold(labels, flag, female_flag, cutoff,
                                           male_flag_cols[i], female_flag_cols[i], "edition")
}
for (sex_cols in list(male_flag_cols, female_flag_cols)) {
  for (i in 1:3) {
    high <- augmented[[sex_cols[i]]]
    low <- augmented[[sex_cols[i + 1L]]]
    stopifnot(identical(is.na(high), is.na(low)), all(low[usable] >= high[usable]))
  }
}
overall <- rbindlist(overall_parts)
by_edition <- rbindlist(edition_parts)
setorder(by_edition, -cutoff, edition)
for (i in seq_along(report_columns)) {
  report <- if (i == 1L) overall else by_edition
  setcolorder(report, c(report_columns[[i]], setdiff(names(report), report_columns[[i]])))
}
count_cols <- c("men", "women", "ambiguous", "unmatched", "unreadable", "total_records",
                "classified", "unresolved", "male_flag_zero", "male_flag_missing",
                "female_flag_zero", "female_flag_missing")
for (cutoff_value in thresholds) {
  stopifnot(all(colSums(by_edition[cutoff == cutoff_value, ..count_cols]) ==
                  unlist(overall[cutoff == cutoff_value, ..count_cols])))
}
stopifnot(all(overall$total_records == nrow(before)),
          all(overall$male_flag_missing == sum(!usable)),
          all(overall$female_flag_missing == sum(!usable)),
          all(overall$unmatched == sum(has_name & !usable)),
          all(overall$unreadable == sum(!has_name)))
baseline_file <- Sys.getenv("AMWS_GENDER_BASELINE", unset = "")
if (nzchar(baseline_file)) {
  baseline <- read_records(baseline_file)
  idx <- match(baseline$record_id, augmented$record_id)
  stopifnot(!anyNA(idx))
  for (col in intersect(flag_cols, names(baseline))) {
    stopifnot(identical(as.integer(baseline[[col]]), augmented[[col]][idx]))
  }
}

# Validate a staged CSV before changing the target. Keep a recoverable backup
# until the installed CSV and protected files have also passed verification.
stage <- tempfile("amws_gender_flags_stage_", tmpdir = out_dir, fileext = ".csv")
backup <- tempfile("amws_gender_flags_backup_", tmpdir = out_dir, fileext = ".csv")
installed <- FALSE
backed_up <- FALSE
verify_record_file <- function(path) {
  out <- read_records(path)
  stopifnot(nrow(out) == nrow(before),
            identical(names(out), output_cols),
            identical(as.list(out[, ..original_cols]), as.list(before[, ..original_cols])))
  for (nm in flag_cols) stopifnot(identical(as.integer(out[[nm]]), augmented[[nm]]))
  invisible(TRUE)
}
setcolorder(augmented, output_cols)
tryCatch({
  fwrite(augmented, stage, na = "", qmethod = "double")
  verify_record_file(stage)
  stopifnot(identical(record_md5_before, unname(tools::md5sum(record_file))),
            identical(protected_md5_before, tools::md5sum(protected_files)))
  if (!file.rename(record_file, backup)) stop("Could not back up the record CSV; target unchanged.")
  backed_up <- TRUE
  if (!file.rename(stage, record_file)) stop("Could not install the validated record CSV.")
  installed <- TRUE
  verify_record_file(record_file)
  protected_md5_after <- tools::md5sum(protected_files)
  stopifnot(identical(protected_md5_before, protected_md5_after))
  fwrite(overall, overall_file, na = "")
  fwrite(by_edition, edition_file, na = "")
  checksums <- data.table(path = protected_files, role = "protected",
                          md5_before = unname(protected_md5_before),
                          md5_after = unname(protected_md5_after), unchanged = TRUE)
  checksums <- rbind(checksums, data.table(
    path = record_file, role = "augmented_record_csv", md5_before = record_md5_before,
    md5_after = unname(tools::md5sum(record_file)),
    unchanged = record_md5_before == unname(tools::md5sum(record_file))))
  fwrite(checksums, checksum_file)
  validation <- data.table(
    check = c("run_time", "population", "cutoffs", "original_column_count",
              "original_values_and_order_unchanged", "geoids_preserved_as_text",
              "male_95_matches_existing_final_classification", "inclusive_boundaries",
              "flags_nested", "missing_flags_per_cutoff", "documented_override_record_id",
              "documented_override_all_flags", "coverage_totals_reconciled", "protected_files",
              "protected_files_unchanged", "flag_zero_meaning", "male_flag_share_denominator",
              "coverage_definition", "inference_rerun", "precision"),
    result = as.character(c(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), nrow(before),
                            paste(thresholds, collapse = ";"), length(original_cols),
                            TRUE, TRUE, TRUE, TRUE, TRUE, sum(!usable),
                            before$record_id[correction_row], TRUE, TRUE, length(protected_files),
                            TRUE, "Prediction exists but sex-specific cutoff not met; does not imply opposite sex",
                            paste("All", nrow(before), "included records"), "Male plus female classifications / all records",
                            FALSE, "Unmeasured; reference proportions are not accuracy")))
  validation <- rbind(validation, data.table(
    check = c("female_95_matches_existing_final_classification", "existing_male_flags_unchanged",
              "sex_flags_mutually_exclusive", "female_missing_flags_per_cutoff",
              "documented_override_female_flags", "female_flag_share_denominator"),
    result = c("TRUE", "TRUE", "TRUE", as.character(sum(!usable)), "All four female flags are 0",
               paste("All", nrow(before), "included records"))))
  fwrite(validation, validation_file)
  unlink(backup)
  backed_up <- FALSE
}, error = function(e) {
  if (backed_up) {
    if (installed) unlink(record_file)
    if (!file.rename(backup, record_file)) {
      stop("Could not restore original CSV. Recover it from: ", backup, "\nOriginal error: ",
           conditionMessage(e))
    }
    backed_up <- FALSE
  }
  stop(conditionMessage(e), call. = FALSE)
}, finally = {
  if (file.exists(stage)) unlink(stage)
})
cat("\nOverall threshold coverage:\n")
print(overall[, .(cutoff, men, women, ambiguous, unmatched, unreadable, total_records,
                  male_flag_share, female_flag_share, coverage)])
cat("\nCoverage by edition:\n")
print(by_edition[, .(cutoff, edition, men, women, male_flag_share, female_flag_share, coverage)])
cat("\nGenerated four male and four female flags; all prior values/order and", length(protected_files),
    "protected files preserved.\nSaved to:", out_dir, "\n")
