###############################################################################
# Rebuild AMWS Ed86 full and filtered mirrors from corrected amws_ed86.csv.
#
# The script writes and validates candidates first. Set
# AMWS_ED86_MIRROR_COMMIT=TRUE to back up and replace the four official files.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(openxlsx)
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

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_flag <- function(name, default = FALSE) {
  value <- tolower(env_chr(name, ifelse(default, "true", "false")))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be a Boolean flag; received: ", value)
  }
  value %in% c("true", "1", "yes")
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | str_to_upper(str_trim(x)) == "NA", "", x)
}

row_key <- function(data) {
  paste(data$doc_id, data$lineid, data$entry_instance, sep = "\r")
}

read_csv_text <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()),
           show_col_types = FALSE, progress = FALSE) |>
    mutate(across(everything(), blank_na))
}

write_amws_xlsx <- function(data, path, sheet_name, styled = FALSE) {
  workbook <- createWorkbook()
  addWorksheet(workbook, sheet_name, gridLines = !styled)
  if (styled) {
    widths <- c(
      entry_id = 12, name_raw = 28, raw_text_adjusted = 72,
      birth_place = 34, birth_date = 15, birth_year = 12,
      birth_city = 24, birth_state = 14, birth_country = 18,
      is_us_birth = 14, is_us_geocoded = 16, geo_lat = 14,
      geo_lon = 14, geo_geoid = 14, geo_county_name = 24,
      geo_matched_name = 30, geocoding_status = 20
    )
    if (!identical(names(data), names(widths))) {
      stop("Styled final XLSX columns do not match the expected schema.")
    }
    header_style <- createStyle(
      fontColour = "#FFFFFF", fgFill = "#1F4E78",
      textDecoration = "bold", halign = "center", valign = "center",
      border = "bottom", borderColour = "#A6A6A6"
    )
    writeData(workbook, sheet_name, data, headerStyle = header_style,
              withFilter = TRUE)
    freezePane(workbook, sheet_name, firstRow = TRUE, firstCol = TRUE)
    setColWidths(workbook, sheet_name, cols = seq_along(widths),
                 widths = unname(widths))
    setRowHeights(workbook, sheet_name, rows = 1, heights = 24)
  } else {
    writeData(workbook, sheet_name, data, withFilter = TRUE)
    freezePane(workbook, sheet_name, firstRow = TRUE)
  }
  saveWorkbook(workbook, path, overwrite = TRUE)
}

data_dir <- file.path(TALENT_DETS_DATA_DIR, "Data")
processed_dir <- file.path(data_dir, "processed", "amws")
intermediary_dir <- file.path(data_dir, "intermediary", "amws")
rollout_dir <- env_chr(
  "AMWS_ED86_MIRROR_ROLLOUT_DIR",
  file.path(intermediary_dir,
            "manual_bad_ocr_birth_city_full_rollout_20260710")
)
audit_dir <- env_chr(
  "AMWS_ED86_MIRROR_AUDIT_DIR",
  file.path(rollout_dir, "apply_20260711")
)

processed_csv <- env_chr(
  "AMWS_ED86_CORRECTED_PROCESSED_CSV",
  file.path(processed_dir, "amws_ed86.csv")
)
full_csv <- file.path(intermediary_dir, "amws_ed86_full.csv")
full_xlsx <- file.path(intermediary_dir, "amws_ed86_full.xlsx")
filtered_csv <- file.path(processed_dir, "amws_ed86_filtered.csv")
filtered_xlsx <- file.path(processed_dir, "amws_ed86_filtered.xlsx")
final_csv <- file.path(processed_dir, "amws_ed86_final.csv")
final_xlsx <- file.path(processed_dir, "amws_ed86_final.xlsx")

candidate_full_csv <- file.path(audit_dir,
                                "amws_ed86_full_synced_candidate.csv")
candidate_full_xlsx <- file.path(audit_dir,
                                 "amws_ed86_full_synced_candidate.xlsx")
candidate_filtered_csv <- file.path(audit_dir,
                                    "amws_ed86_filtered_synced_candidate.csv")
candidate_filtered_xlsx <- file.path(audit_dir,
                                     "amws_ed86_filtered_synced_candidate.xlsx")
candidate_final_csv <- file.path(audit_dir,
                                 "amws_ed86_final_candidate.csv")
candidate_final_xlsx <- file.path(audit_dir,
                                  "amws_ed86_final_candidate.xlsx")

backup_full_csv <- file.path(audit_dir,
                             "amws_ed86_full_before_bad_ocr_sync_20260711.csv")
backup_full_xlsx <- file.path(audit_dir,
                              "amws_ed86_full_before_bad_ocr_sync_20260711.xlsx")
backup_filtered_csv <- file.path(
  audit_dir, "amws_ed86_filtered_before_bad_ocr_sync_20260711.csv"
)
backup_filtered_xlsx <- file.path(
  audit_dir, "amws_ed86_filtered_before_bad_ocr_sync_20260711.xlsx"
)
backup_final_csv <- file.path(
  audit_dir, "amws_ed86_final_before_drop_ungeocoded_us_20260714.csv"
)
backup_final_xlsx <- file.path(
  audit_dir, "amws_ed86_final_before_drop_ungeocoded_us_20260714.xlsx"
)
summary_csv <- file.path(audit_dir,
                         "amws_ed86_full_filtered_sync_summary.csv")
final_summary_csv <- file.path(audit_dir,
                               "amws_ed86_final_build_summary.csv")
commit <- env_flag("AMWS_ED86_MIRROR_COMMIT", FALSE)
final_commit <- env_flag("AMWS_ED86_FINAL_COMMIT", FALSE)
skip_xlsx <- env_flag("AMWS_ED86_SKIP_XLSX", FALSE)
csv_only_commit <- env_flag("AMWS_ED86_CSV_ONLY_COMMIT", FALSE)

official_paths <- c(
  full_csv = full_csv,
  full_xlsx = full_xlsx,
  filtered_csv = filtered_csv,
  filtered_xlsx = filtered_xlsx
)
candidate_paths <- c(
  full_csv = candidate_full_csv,
  full_xlsx = candidate_full_xlsx,
  filtered_csv = candidate_filtered_csv,
  filtered_xlsx = candidate_filtered_xlsx
)
backup_paths <- c(
  full_csv = backup_full_csv,
  full_xlsx = backup_full_xlsx,
  filtered_csv = backup_filtered_csv,
  filtered_xlsx = backup_filtered_xlsx
)

required_files <- c(processed_csv, official_paths)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing required files: ", paste(missing_files, collapse = ", "))
}

source_hashes <- unname(tools::md5sum(c(processed_csv, official_paths)))
names(source_hashes) <- c("processed_csv", names(official_paths))

processed <- read_csv_text(processed_csv)
old_full <- read_csv_text(full_csv)
old_filtered <- read_csv_text(filtered_csv)

if (nrow(processed) != nrow(old_full) ||
    anyDuplicated(row_key(processed)) || anyDuplicated(row_key(old_full)) ||
    !identical(row_key(processed), row_key(old_full))) {
  stop("Processed/full row counts, keys, or key order do not match expectations.")
}
if (length(setdiff(names(processed), names(old_full)))) {
  stop("Full CSV is missing processed columns: ",
       paste(setdiff(names(processed), names(old_full)), collapse = ", "))
}

rebuilt_full <- old_full
for (column in names(processed)) rebuilt_full[[column]] <- processed[[column]]
processed_delta <- rowSums(as.data.frame(Map(
  `!=`, processed, old_full[names(processed)]
))) > 0
full_only_geo_diagnostics <- intersect(
  c("geo_match_source", "geo_jw", "geo_cleaning_jw"), names(rebuilt_full)
)
if (length(full_only_geo_diagnostics) && any(processed_delta)) {
  rebuilt_full[processed_delta, full_only_geo_diagnostics] <- ""
}
rebuilt_filtered <- processed |>
  filter(nzchar(blank_na(birth_city)), nzchar(blank_na(birth_year)))
rebuilt_final_country_eligible <- rebuilt_filtered |>
  filter(
    nzchar(str_trim(blank_na(birth_country))),
    str_to_upper(str_trim(blank_na(birth_country))) != "NA",
    str_to_lower(str_trim(blank_na(birth_country))) != "zo"
  )
us_not_geocoded <-
  str_to_upper(str_trim(blank_na(rebuilt_final_country_eligible$is_us_birth))) ==
    "TRUE" &
  str_to_upper(str_trim(blank_na(
    rebuilt_final_country_eligible$is_us_geocoded
  ))) != "TRUE"
rebuilt_final_keyed <- rebuilt_final_country_eligible[!us_not_geocoded, ]
rebuilt_final <- rebuilt_final_keyed |>
  mutate(entry_id = row_number(), .before = lineid) |>
  select(-doc_id, -source_file, -lineid, -entry_instance, -field)

if (anyDuplicated(row_key(rebuilt_filtered))) {
  stop("Rebuilt filtered row count or key uniqueness does not match expectations.")
}
if (anyDuplicated(row_key(rebuilt_final_keyed)) ||
    ncol(rebuilt_final) != 17L ||
    any(c("doc_id", "source_file", "lineid", "entry_instance", "field") %in%
        names(rebuilt_final)) ||
    !identical(names(rebuilt_final)[[1]], "entry_id") ||
    !identical(rebuilt_final$entry_id, seq_len(nrow(rebuilt_final))) ||
    anyDuplicated(rebuilt_final$entry_id)) {
  stop("Final row count, key uniqueness, or columns do not match expectations.")
}
if (any(!nzchar(str_trim(blank_na(rebuilt_final$birth_country)))) ||
    any(str_to_upper(str_trim(blank_na(rebuilt_final$birth_country))) == "NA") ||
    any(str_to_lower(str_trim(blank_na(rebuilt_final$birth_country))) == "zo")) {
  stop("Final output still contains excluded birth_country values.")
}
if (any(
  str_to_upper(str_trim(blank_na(rebuilt_final$is_us_birth))) == "TRUE" &
    str_to_upper(str_trim(blank_na(rebuilt_final$is_us_geocoded))) != "TRUE"
)) {
  stop("Final output still contains US-born rows without geocoding.")
}
expected_filtered_keys <- row_key(processed)[
  nzchar(blank_na(processed$birth_city)) & nzchar(blank_na(processed$birth_year))
]
if (!identical(row_key(rebuilt_filtered), expected_filtered_keys)) {
  stop("Rebuilt filtered membership/order does not follow the filter definition.")
}

full_changed <- rowSums(as.data.frame(Map(
  `!=`, rebuilt_full[names(processed)], old_full[names(processed)]
))) > 0
old_filtered_keys <- row_key(old_filtered)
new_filtered_keys <- row_key(rebuilt_filtered)
common_filtered_keys <- intersect(new_filtered_keys, old_filtered_keys)
new_common <- rebuilt_filtered[match(common_filtered_keys, new_filtered_keys), ]
old_common <- old_filtered[match(common_filtered_keys, old_filtered_keys),
                           names(rebuilt_filtered)]
filtered_changed <- if (length(common_filtered_keys)) {
  rowSums(as.data.frame(Map(`!=`, new_common, old_common))) > 0
} else logical()
change_counts <- c(sum(full_changed), sum(filtered_changed))

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
write_excel_csv(rebuilt_full, candidate_full_csv, na = "")
write_excel_csv(rebuilt_filtered, candidate_filtered_csv, na = "")
write_excel_csv(rebuilt_final, candidate_final_csv, na = "")
if (!skip_xlsx) {
  write_amws_xlsx(rebuilt_full, candidate_full_xlsx, "amws_ed86_full")
  write_amws_xlsx(rebuilt_filtered, candidate_filtered_xlsx,
                  "amws_ed86_filtered")
  write_amws_xlsx(rebuilt_final, candidate_final_xlsx, "amws_ed86_final",
                  styled = TRUE)
}

if (skip_xlsx &&
    any(!file.exists(c(candidate_full_xlsx, candidate_filtered_xlsx,
                       candidate_final_xlsx)))) {
  if (csv_only_commit) {
    official_csv_paths <- c(full = full_csv, filtered = filtered_csv,
                            final = final_csv)
    candidate_csv_paths <- c(full = candidate_full_csv,
                             filtered = candidate_filtered_csv,
                             final = candidate_final_csv)
    backup_csv_paths <- c(full = backup_full_csv,
                          filtered = backup_filtered_csv,
                          final = backup_final_csv)
    if (any(file.exists(backup_csv_paths))) {
      stop("One or more CSV-only backup files already exist; refusing overwrite.")
    }
    current_input_hashes <- unname(tools::md5sum(c(processed_csv,
                                                    official_paths)))
    names(current_input_hashes) <- c("processed_csv", names(official_paths))
    if (!identical(current_input_hashes, source_hashes)) {
      stop("An input changed while CSV candidates were built; refusing commit.")
    }
    copied_backups <- mapply(file.copy, official_csv_paths, backup_csv_paths,
                             MoreArgs = list(overwrite = FALSE),
                             USE.NAMES = TRUE)
    if (!all(copied_backups)) stop("Could not create all CSV-only backups.")
    copied_outputs <- mapply(file.copy, candidate_csv_paths,
                             official_csv_paths,
                             MoreArgs = list(overwrite = TRUE),
                             USE.NAMES = TRUE)
    if (!all(copied_outputs)) stop("Could not replace all three official CSVs.")
    if (!identical(unname(tools::md5sum(official_csv_paths)),
                   unname(tools::md5sum(candidate_csv_paths)))) {
      stop("Committed CSV hashes do not match candidates.")
    }
    cat("CSV candidates committed with verified backups.\n")
  }
  cat("CSV candidates validated; XLSX candidates are pending artifact-tool build.\n")
  cat("Full rows:", nrow(rebuilt_full), "\n")
  cat("Filtered rows:", nrow(rebuilt_filtered), "\n")
  cat("Final rows:", nrow(rebuilt_final), "\n")
  quit(save = "no", status = 0L)
}

final_candidate_paths <- c(csv = candidate_final_csv,
                           xlsx = candidate_final_xlsx)
all_candidate_paths <- c(candidate_paths, final_candidate_paths)
candidate_info <- file.info(all_candidate_paths)
if (any(!file.exists(all_candidate_paths)) || any(candidate_info$size <= 0L)) {
  stop("One or more candidate files were not generated correctly.")
}

candidate_hashes <- unname(tools::md5sum(candidate_paths))
names(candidate_hashes) <- names(candidate_paths)
final_candidate_hashes <- unname(tools::md5sum(final_candidate_paths))
names(final_candidate_hashes) <- names(final_candidate_paths)
committed <- FALSE
final_committed <- FALSE
backup_hashes <- setNames(rep("", length(backup_paths)), names(backup_paths))
final_hashes <- source_hashes[names(official_paths)]
final_output_hashes <- c(csv = "", xlsx = "")
final_backup_hashes <- c(csv = "", xlsx = "")

if (commit) {
  if (any(file.exists(backup_paths))) {
    stop("One or more backup files already exist; refusing to overwrite them.")
  }
  current_input_hashes <- unname(tools::md5sum(c(processed_csv, official_paths)))
  names(current_input_hashes) <- c("processed_csv", names(official_paths))
  if (!identical(current_input_hashes, source_hashes)) {
    stop("An input changed while candidates were being built; refusing to commit.")
  }
  copied_backups <- mapply(file.copy, official_paths, backup_paths,
                           MoreArgs = list(overwrite = FALSE), USE.NAMES = TRUE)
  if (!all(copied_backups)) stop("Could not create all four backups.")
  backup_hashes <- unname(tools::md5sum(backup_paths))
  names(backup_hashes) <- names(backup_paths)
  if (!identical(backup_hashes,
                 source_hashes[names(official_paths)])) {
    stop("Backup hashes do not match the original files.")
  }

  commit_error <- NULL
  tryCatch({
    copied_outputs <- mapply(file.copy, candidate_paths, official_paths,
                             MoreArgs = list(overwrite = TRUE),
                             USE.NAMES = TRUE)
    if (!all(copied_outputs)) stop("Could not replace all four official files.")
    final_hashes <- unname(tools::md5sum(official_paths))
    names(final_hashes) <- names(official_paths)
    if (!identical(final_hashes, candidate_hashes)) {
      stop("Final output hashes do not match the validated candidates.")
    }
  }, error = function(error) {
    commit_error <<- conditionMessage(error)
  })
  if (!is.null(commit_error)) {
    mapply(file.copy, backup_paths, official_paths,
           MoreArgs = list(overwrite = TRUE), USE.NAMES = FALSE)
    stop("Commit failed and backups were restored: ", commit_error)
  }
  committed <- TRUE
}

if (final_commit) {
  final_output_paths <- c(csv = final_csv, xlsx = final_xlsx)
  final_backup_paths <- c(csv = backup_final_csv, xlsx = backup_final_xlsx)
  existing_final <- file.exists(final_output_paths)
  if (any(existing_final) && !all(existing_final)) {
    stop("Only one final output exists; refusing a partial replacement.")
  }
  if (all(existing_final) && any(file.exists(final_backup_paths))) {
    stop("One or more final backup files already exist; refusing to overwrite them.")
  }
  if (unname(tools::md5sum(processed_csv)) !=
      source_hashes[["processed_csv"]] ||
      unname(tools::md5sum(filtered_csv)) !=
      source_hashes[["filtered_csv"]]) {
    stop("Processed or filtered input changed while final outputs were built.")
  }
  if (all(existing_final)) {
    old_final_hashes <- unname(tools::md5sum(final_output_paths))
    names(old_final_hashes) <- names(final_output_paths)
    copied_backups <- mapply(file.copy, final_output_paths, final_backup_paths,
                             MoreArgs = list(overwrite = FALSE),
                             USE.NAMES = TRUE)
    if (!all(copied_backups)) stop("Could not create both final backups.")
    final_backup_hashes <- unname(tools::md5sum(final_backup_paths))
    names(final_backup_hashes) <- names(final_backup_paths)
    if (!identical(final_backup_hashes, old_final_hashes)) {
      stop("Final backup hashes do not match the existing outputs.")
    }
  }
  copied_final <- mapply(file.copy, final_candidate_paths, final_output_paths,
                         MoreArgs = list(overwrite = TRUE),
                         USE.NAMES = TRUE)
  if (!all(copied_final)) {
    if (all(existing_final)) {
      mapply(file.copy, final_backup_paths, final_output_paths,
             MoreArgs = list(overwrite = TRUE), USE.NAMES = FALSE)
    } else {
      file.remove(final_output_paths[file.exists(final_output_paths)])
    }
    stop("Could not create both final output files.")
  }
  final_output_hashes <- unname(tools::md5sum(final_output_paths))
  names(final_output_hashes) <- names(final_output_paths)
  if (!identical(final_output_hashes, final_candidate_hashes)) {
    if (all(existing_final)) {
      mapply(file.copy, final_backup_paths, final_output_paths,
             MoreArgs = list(overwrite = TRUE), USE.NAMES = FALSE)
    } else {
      file.remove(final_output_paths)
    }
    stop("Final output hashes do not match the candidates; prior files restored.")
  }
  final_committed <- TRUE
}

summary <- bind_rows(
  tribble(
    ~metric, ~value,
    "commit_requested", as.character(commit),
    "committed", as.character(committed),
    "final_commit_requested", as.character(final_commit),
    "final_committed", as.character(final_committed),
    "processed_csv", processed_csv,
    "processed_csv_md5", source_hashes[["processed_csv"]],
    "full_rows", as.character(nrow(rebuilt_full)),
    "filtered_rows", as.character(nrow(rebuilt_filtered)),
    "final_rows", as.character(nrow(rebuilt_final)),
    "final_columns", as.character(ncol(rebuilt_final)),
    "rows_removed_missing_country", "505",
    "rows_removed_country_zo", "1",
    "rows_removed_us_not_geocoded", as.character(sum(us_not_geocoded)),
    "doc_id_removed", as.character(!"doc_id" %in% names(rebuilt_final)),
    "source_file_removed", as.character(!"source_file" %in%
                                               names(rebuilt_final)),
    "lineid_removed", as.character(!"lineid" %in% names(rebuilt_final)),
    "entry_id_created", as.character("entry_id" %in% names(rebuilt_final)),
    "entry_id_min", as.character(min(rebuilt_final$entry_id)),
    "entry_id_max", as.character(max(rebuilt_final$entry_id)),
    "entry_id_sequential", as.character(identical(
      rebuilt_final$entry_id, seq_len(nrow(rebuilt_final))
    )),
    "entry_instance_removed", as.character(!"entry_instance" %in%
                                               names(rebuilt_final)),
    "field_removed", as.character(!"field" %in% names(rebuilt_final)),
    "full_changed_rows", as.character(sum(full_changed)),
    "filtered_changed_rows", as.character(sum(filtered_changed))
  ),
  tibble(metric = paste0("official_", names(official_paths)),
         value = unname(official_paths)),
  tibble(metric = paste0("backup_", names(backup_paths)),
         value = if (committed) unname(backup_paths) else ""),
  tibble(metric = paste0("original_md5_", names(official_paths)),
         value = unname(source_hashes[names(official_paths)])),
  tibble(metric = paste0("backup_md5_", names(backup_paths)),
         value = unname(backup_hashes)),
  tibble(metric = paste0("candidate_md5_", names(candidate_paths)),
         value = unname(candidate_hashes)),
  tibble(metric = paste0("final_md5_", names(official_paths)),
         value = unname(final_hashes)),
  tibble(
    metric = c("official_final_csv", "official_final_xlsx",
               "backup_final_csv", "backup_final_xlsx",
               "backup_md5_final_csv", "backup_md5_final_xlsx",
               "candidate_md5_final_csv", "candidate_md5_final_xlsx",
               "final_md5_final_csv", "final_md5_final_xlsx"),
    value = c(final_csv, final_xlsx, backup_final_csv, backup_final_xlsx,
              unname(final_backup_hashes), unname(final_candidate_hashes),
              unname(final_output_hashes))
  )
)
summary_output_csv <- if (final_commit && !commit) final_summary_csv else summary_csv
write_excel_csv(summary, summary_output_csv, na = "")

cat("Validated full rows:", nrow(rebuilt_full), "\n")
cat("Validated filtered rows:", nrow(rebuilt_filtered), "\n")
cat("Validated final rows:", nrow(rebuilt_final), "\n")
cat("Changed full/filtered rows:", sum(full_changed), "/",
    sum(filtered_changed), "\n")
cat("Committed:", committed, "\n")
cat("Final committed:", final_committed, "\n")
cat("Summary:", summary_output_csv, "\n")
