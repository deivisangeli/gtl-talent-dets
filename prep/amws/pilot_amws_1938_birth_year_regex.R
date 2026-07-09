###############################################################################
# Pilot a conservative birth-year regex for AMWS 1938.
#
# Reads:
#   output/amws/amws_birth_year_parse_audit_sample_50_by_edition_current_classified.csv
#
# Writes:
#   output/amws/amws_1938_birth_year_regex_pilot.csv
#   output/amws/amws_1938_birth_year_regex_pilot_summary.csv
#
# This is diagnostic only. It does not modify cleaned AMWS files.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                             winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "paths.R"))

local_dropbox <- file.path("C:/Users", Sys.info()[["user"]],
                           "Globtalent Dropbox", "gtl_talent_dets")
if (!dir.exists(TALENT_DETS_DATA_DIR) && dir.exists(local_dropbox)) {
  TALENT_DETS_DATA_DIR <- normalizePath(local_dropbox, winslash = "/")
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  AMWS_OUTPUT <- file.path(DATA_OUTPUT, "amws")
}

sample_file <- file.path(
  AMWS_OUTPUT,
  "amws_birth_year_parse_audit_sample_50_by_edition_current_classified.csv"
)
if (!file.exists(sample_file)) {
  stop("Missing classified audit sample: ", sample_file)
}

normalize_year_1938 <- function(year_chr) {
  year_int <- suppressWarnings(as.integer(year_chr))
  out <- rep(NA_integer_, length(year_int))
  k4 <- !is.na(year_int) & year_int >= 1000L
  k1900 <- !is.na(year_int) & year_int >= 0L & year_int <= 25L
  k1800 <- !is.na(year_int) & year_int >= 26L & year_int <= 99L
  out[k4] <- year_int[k4]
  out[k1900] <- 1900L + year_int[k1900]
  out[k1800] <- 1800L + year_int[k1800]
  out
}

first_capture <- function(pattern, text, ignore.case = TRUE) {
  match <- regexec(pattern, text, ignore.case = ignore.case, perl = TRUE)
  parts <- regmatches(text, match)[[1]]
  if (!length(parts) || length(parts) < 2L) return(NA_character_)
  parts[length(parts)]
}

extract_birth_year_regex <- function(raw_text) {
  text <- as.character(raw_text)
  if (is.na(text) || !nzchar(trimws(text))) {
    return(list(year = NA_integer_, note = "empty_raw_text"))
  }
  text <- gsub("\\s+", " ", text)
  text_head <- substr(text, 1, 220)

  # Month/date first. OCR alternatives are included only for common AMWS 1938
  # confusions observed in audited rows.
  months <- paste0(
    "(January|February|March|April|May|June|July|August|September|",
    "October|November|December|Jan|Feb|Mar|Apr|Aug|Sept|Sep|Oct|",
    "Nov|Dec|Nor|Dee|Doc|Dae)"
  )
  date_rx <- paste0(
    "\\b", months,
    "\\.?\\s*[,.]?\\s*\\.?\\s*[0-9A-Za-z]{1,2}",
    "\\s*[,.]?\\s*([0-9]{2,4})\\b"
  )
  year_chr <- first_capture(date_rx, text_head)
  if (!is.na(year_chr)) {
    year <- normalize_year_1938(year_chr)
    return(list(year = year, note = "month_day_year_regex"))
  }

  # Only use a trailing year fallback before degree/career tokens. This avoids
  # capturing degree years like "B.S., 24" or "Chem.E, 28".
  degree_rx <- paste0(
    "\\b(",
    "A\\.?\\s?B|B\\.?\\s?A|B\\.?\\s?S|B\\.?\\s?E|B\\s?S|A\\s?B|",
    "M\\.?\\s?A|M\\.?\\s?S|M\\.?\\s?D|Ph\\.?\\s?D|D\\.?\\s?Sc|",
    "Sc\\.?\\s?D|LL\\.?\\s?D|D\\.?\\s?Eng|Chem\\.?\\s?E|",
    "C\\.?\\s?E|D\\.?\\s?V\\.?\\s?M|Ph\\.?G|Lit\\.?B|",
    "Cand\\.?\\s?Mag|Grad|Asst|Prof|Instr|Fellow|Research|",
    "RESEARCH|PROF|ASST|INSTR|Lecturer|lecturer|Teacher|teacher",
    ")\\b"
  )
  degree_pos <- regexpr(degree_rx, text_head, ignore.case = TRUE, perl = TRUE)
  prefix <- if (degree_pos > 0) {
    substr(text_head, 1, degree_pos - 1L)
  } else {
    text_head
  }
  prefix <- trimws(gsub("[;,.\\s]+$", "", prefix))
  year_chr <- first_capture("(?:^|[, ])\\s*([0-9]{2,4})\\s*$", prefix,
                            ignore.case = FALSE)
  if (!is.na(year_chr)) {
    year <- normalize_year_1938(year_chr)
    return(list(year = year, note = "trailing_prefix_year_regex"))
  }

  list(year = NA_integer_, note = "no_regex_year")
}

sample <- fread(sample_file, colClasses = "character")
required_cols <- c("edition", "lineid", "name", "raw_text",
                   "parsed_birth_year", "audit_status")
missing_cols <- setdiff(required_cols, names(sample))
if (length(missing_cols)) {
  stop("Sample missing required columns: ", paste(missing_cols, collapse = ", "))
}

pilot <- sample[edition == "1938"]
if (nrow(pilot) == 0L) stop("No 1938 rows found in sample.")

parsed <- lapply(pilot$raw_text, extract_birth_year_regex)
pilot[, current_birth_year := suppressWarnings(as.integer(parsed_birth_year))]
pilot[, regex_birth_year := vapply(parsed, `[[`, integer(1), "year")]
pilot[, regex_note := vapply(parsed, `[[`, character(1), "note")]
pilot[, changed := fifelse(
  is.na(regex_birth_year) & is.na(current_birth_year), FALSE,
  is.na(regex_birth_year) != is.na(current_birth_year) |
    regex_birth_year != current_birth_year
)]
pilot[, false_positive := audit_status == "correct" &
        !is.na(regex_birth_year) &
        regex_birth_year != current_birth_year]
pilot[, corrected_known_error := audit_status == "incorrect" &
        !is.na(regex_birth_year) &
        regex_birth_year != current_birth_year]
pilot[, regex_missed_current_correct := audit_status == "correct" &
        is.na(regex_birth_year)]
pilot[, raw_text_start := substr(raw_text, 1, 160)]

out_file <- file.path(AMWS_OUTPUT, "amws_1938_birth_year_regex_pilot.csv")
fwrite(
  pilot[, .(
    lineid, name, raw_text, raw_text_start,
    current_birth_year, regex_birth_year, audit_status,
    changed, false_positive, corrected_known_error,
    regex_missed_current_correct, regex_note, audit_note
  )],
  out_file
)

summary <- data.table(
  metric = c(
    "sample_rows",
    "classified_correct_rows",
    "classified_incorrect_rows",
    "correct_rows_exact_match",
    "correct_rows_regex_na",
    "false_positive_rows",
    "known_error_rows_corrected_by_regex",
    "known_error_rows_not_corrected_by_regex",
    "rows_with_regex_change"
  ),
  value = c(
    nrow(pilot),
    pilot[audit_status == "correct", .N],
    pilot[audit_status == "incorrect", .N],
    pilot[audit_status == "correct" &
            !is.na(regex_birth_year) &
            regex_birth_year == current_birth_year, .N],
    pilot[regex_missed_current_correct == TRUE, .N],
    pilot[false_positive == TRUE, .N],
    pilot[corrected_known_error == TRUE, .N],
    pilot[audit_status == "incorrect" & corrected_known_error != TRUE, .N],
    pilot[changed == TRUE, .N]
  )
)
summary_file <- file.path(
  AMWS_OUTPUT,
  "amws_1938_birth_year_regex_pilot_summary.csv"
)
fwrite(summary, summary_file)

cat("Wrote pilot comparison:", out_file, "\n")
cat("Wrote pilot summary:", summary_file, "\n")
print(summary)

changed_rows <- pilot[changed == TRUE, .(
  lineid, name, current_birth_year, regex_birth_year,
  audit_status, false_positive, corrected_known_error, regex_note,
  raw_text_start
)]
if (nrow(changed_rows)) {
  cat("\nRows where regex differs from current parser:\n")
  print(changed_rows, nrows = Inf)
}
