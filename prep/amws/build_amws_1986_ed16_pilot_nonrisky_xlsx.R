###############################################################################
# Build the final XLSX for the AMWS edition 16 (1986) pilot.
#
# Inputs:
#   <Dropbox root>/output/amws/transcription_runs/
#     amws16_A_0_200_precleaning_first10_rawxlsx_4agents/
#       amws_entries_regex_parsed.csv
#       regex_gap_audit/regex_test_results.csv
#
# Output:
#   <same run directory>/regex_gap_audit/
#     amws_1986_ed16_pilot_nonrisky_corrected.xlsx
#
# Scope:
#   Consolidates the stage-2 parsed table and applies only safe recoveries
#   proposed by the stage-3 missing-value audit. Basic conservative
#   normalizations belong to the stage-2 parser, not this final export.
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
  DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  AMWS_INPUT <- file.path(DATA_INPUT, "amws")
  AMWS_OUTPUT <- file.path(DATA_OUTPUT, "amws")
}

run_id <- "amws16_A_0_200_precleaning_first10_rawxlsx_4agents"
run_dir <- file.path(DATA_OUTPUT, "amws", "transcription_runs", run_id)
audit_dir <- file.path(run_dir, "regex_gap_audit")

parsed_file <- file.path(run_dir, "amws_entries_regex_parsed.csv")
test_file <- file.path(audit_dir, "regex_test_results.csv")
output_xlsx <- file.path(audit_dir,
                         "amws_1986_ed16_pilot_nonrisky_corrected.xlsx")
correction_log_file <- file.path(audit_dir,
                                 "amws_1986_ed16_pilot_nonrisky_corrections.csv")

if (!file.exists(parsed_file)) {
  stop("Parsed input not found: ", parsed_file)
}
if (!file.exists(test_file)) {
  stop("Regex test input not found: ", test_file)
}

normalize_blank <- function(x) {
  x <- ifelse(is.na(x), "", x)
  str_squish(x)
}

safe_birth_date <- function(x) {
  x <- normalize_blank(x)
  str_detect(
    x,
    regex("^(Jan|Feb|Mar|Apr|May|Jun|June|Jul|July|Aug|Sep|Sept|Oct|Nov|Dec)\\.?\\s+[0-9]{1,2},\\s*[0-9]{2,4}$")
  )
}

safe_birth_place <- function(x) {
  x <- normalize_blank(x)
  nzchar(x) &&
    !str_detect(x, regex("\\b111\\b")) &&
    !str_detect(x, "[\\^<>?«»]") &&
    !str_detect(x, regex("^[A-Z]\\*?[0-9]|MMLNOCh|Stanvanfr", ignore_case = TRUE))
}

safe_field <- function(x) {
  x <- normalize_blank(x)
  nzchar(x) &&
    !str_detect(x, "[\\^<>?«»\\\\]") &&
    !str_detect(x, "[0-9]") &&
    !str_detect(x, regex("^CAL\\s+CHEMISTRY$")) &&
    !str_detect(x, regex("\\b(citizen|PhD|Univ|Dept|Prof|Mailing)\\b",
                        ignore_case = TRUE))
}

parsed_raw <- read_csv(parsed_file, show_col_types = FALSE)

if (!"birth_place" %in% names(parsed_raw) && "birthplace" %in% names(parsed_raw)) {
  parsed_raw <- parsed_raw |>
    mutate(birth_place = birthplace)
}

parsed <- parsed_raw |>
  mutate(
    across(c(birth_place, birth_date, field), normalize_blank),
    lineid = as.integer(lineid),
    global_lineid = as.integer(global_lineid)
  )

tests <- read_csv(test_file, show_col_types = FALSE) |>
  mutate(
    global_lineid = as.integer(global_lineid),
    needs_manual_review = as.logical(needs_manual_review),
    proposed_value = normalize_blank(proposed_value)
  )

work <- parsed |>
  transmute(
    lineid = lineid,
    birth_place = birth_place,
    birth_date = birth_date,
    field = field,
    raw_text = raw_text
  )

log_rows <- list()

record_change <- function(lineid, field_name, old_value, new_value, rule) {
  if (identical(old_value, new_value)) {
    return(invisible(NULL))
  }
  log_rows[[length(log_rows) + 1]] <<- tibble(
    lineid = lineid,
    field = field_name,
    old_value = old_value,
    new_value = new_value,
    rule = rule
  )
}

apply_value <- function(lineid, field_name, new_value, rule) {
  row <- which(work$lineid == lineid)
  if (length(row) != 1) {
    stop("Expected exactly one row for lineid ", lineid)
  }
  old_value <- work[[field_name]][row]
  if (nzchar(old_value) || !nzchar(new_value)) {
    return(invisible(NULL))
  }
  record_change(lineid, field_name, old_value, new_value, rule)
  work[[field_name]][row] <<- new_value
}

safe_tests <- tests |>
  filter(!needs_manual_review)

for (i in seq_len(nrow(safe_tests))) {
  row <- safe_tests[i, ]
  target <- row$target_field
  value <- row$proposed_value
  output_field <- dplyr::recode(
    target,
    "birthplace" = "birth_place",
    "birth_date" = "birth_date",
    "field" = "field"
  )

  if (target == "birthplace") {
    if (!safe_birth_place(value)) next
  } else if (target == "birth_date") {
    if (!safe_birth_date(value)) next
  } else if (target == "field") {
    if (!safe_field(value)) next
  }

  apply_value(row$global_lineid, output_field, value, row$pattern_id)
}

if (nrow(work) != nrow(parsed)) {
  stop("Output row count changed.")
}
if (!identical(work$lineid, seq_len(nrow(work)))) {
  stop("Output lineid is not sequential 1:n.")
}
if (!identical(names(work), c("lineid", "birth_place", "birth_date", "field",
                              "raw_text"))) {
  stop("Output columns are not the requested schema.")
}
if (any(str_detect(work$birth_place, regex("\\b111\\b")))) {
  stop("Uncorrected 111 remains in birth_place.")
}

correction_log <- if (length(log_rows)) bind_rows(log_rows) else {
  tibble(lineid = integer(), field = character(), old_value = character(),
         new_value = character(), rule = character())
}

write_csv(correction_log, correction_log_file)

wb <- createWorkbook()
addWorksheet(wb, "entries")
writeData(wb, "entries", work)

header_style <- createStyle(textDecoration = "bold", fgFill = "#D9EAF7",
                            border = "Bottom")
addStyle(wb, "entries", header_style, rows = 1, cols = 1:5,
         gridExpand = TRUE)
freezePane(wb, "entries", firstRow = TRUE)
setColWidths(wb, "entries", cols = 1, widths = 10)
setColWidths(wb, "entries", cols = 2:4, widths = c(28, 16, 42))
setColWidths(wb, "entries", cols = 5, widths = 80)
raw_style <- createStyle(wrapText = TRUE, valign = "top")
addStyle(wb, "entries", raw_style, rows = 2:(nrow(work) + 1), cols = 5,
         gridExpand = TRUE)
addFilter(wb, "entries", rows = 1, cols = 1:5)

saveWorkbook(wb, output_xlsx, overwrite = TRUE)

read_back <- openxlsx::read.xlsx(output_xlsx, sheet = "entries")
if (nrow(read_back) != nrow(work)) {
  stop("XLSX read-back row count mismatch.")
}
if (!identical(names(read_back), names(work))) {
  stop("XLSX read-back columns mismatch.")
}

cat("output_xlsx:", output_xlsx, "\n")
cat("rows:", nrow(work), "\n")
cat("complete_rows:", sum(work$birth_place != "" &
                            work$birth_date != "" &
                            work$field != ""), "\n")
cat("incomplete_rows:", sum(!(work$birth_place != "" &
                                work$birth_date != "" &
                                work$field != "")), "\n")
cat("corrections:", nrow(correction_log), "\n")
cat("correction_log:", correction_log_file, "\n")
