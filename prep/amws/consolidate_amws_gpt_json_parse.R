###############################################################################
# Consolidate AMWS GPT/Codex JSON batch outputs.
#
# Inputs, by default:
#   <Dropbox root>/output/amws/transcription_runs/
#     amws16_A_0_200_regex_only_pages1_191/gpt54_mini_codex_parse/
#       source_rows_for_consolidation.json
#       batch_01_output.json ... batch_45_output.json
#
# Outputs:
#   <run dir>/amws_entries_gpt54_mini_codex_parsed.json
#   <run dir>/amws_entries_gpt54_mini_codex_parsed.csv
#   <run dir>/amws_entries_gpt54_mini_codex_parsed.xlsx
#   <run dir>/gpt54_mini_codex_run_summary.csv
#   <run dir>/gpt54_mini_codex_validation_issues.json
#
# Environment overrides:
#   AMWS_GPT_PARSE_RUN_DIR       Full directory containing batch JSONs.
#   AMWS_GPT_PARSE_RUN_ID        Run id under output/amws/transcription_runs.
#   AMWS_GPT_PARSE_SUBDIR        Subdirectory under RUN_ID; default gpt54_mini_codex_parse.
#   AMWS_GPT_PARSE_SOURCE_FILE   Source rows JSON; default source_rows_for_consolidation.json.
#   AMWS_GPT_PARSE_OUTPUT_PREFIX Output prefix; default amws_entries_gpt54_mini_codex_parsed.
#   AMWS_GPT_PARSE_BATCH_COUNT   Expected number of batches; default detect from files.
#   AMWS_GPT_PARSE_BATCH_SIZE    Expected full batch size; default 60.
#   AMWS_GPT_PARSE_STRICT        TRUE/FALSE; default TRUE. Stop when validation issues exist.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(openxlsx)
  library(readr)
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

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_int <- function(name, default = NA_integer_) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    return(default)
  }
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed)) {
    stop("Environment variable ", name, " must be an integer; got: ", value)
  }
  parsed
}

env_bool <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    return(default)
  }
  value <- tolower(value)
  if (value %in% c("true", "t", "1", "yes", "y")) {
    return(TRUE)
  }
  if (value %in% c("false", "f", "0", "no", "n")) {
    return(FALSE)
  }
  stop("Environment variable ", name, " must be TRUE/FALSE; got: ", value)
}

read_text_with_fallback <- function(path) {
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  text <- rawToChar(raw)
  utf8 <- iconv(text, from = "UTF-8", to = "UTF-8", sub = NA)
  if (!is.na(utf8)) {
    return(list(text = enc2utf8(utf8), encoding = "utf-8"))
  }
  cp1252 <- iconv(text, from = "CP1252", to = "UTF-8", sub = NA)
  if (is.na(cp1252)) {
    stop("Could not decode JSON as UTF-8 or CP1252: ", path)
  }
  list(text = enc2utf8(cp1252), encoding = "cp1252")
}

read_json_fallback <- function(path, simplify_vector = TRUE) {
  decoded <- read_text_with_fallback(path)
  parsed <- jsonlite::fromJSON(decoded$text, simplifyVector = simplify_vector)
  list(data = parsed, encoding = decoded$encoding)
}

append_issue <- function(issues, type, batch = NA_integer_, lineid = NA_integer_,
                         detail = "") {
  issues[[length(issues) + 1]] <- list(
    type = type,
    batch = if (is.na(batch)) NULL else batch,
    lineid = if (is.na(lineid)) NULL else lineid,
    detail = detail
  )
  issues
}

nonempty_n <- function(x) {
  sum(nzchar(trimws(ifelse(is.na(x), "", as.character(x)))))
}

default_run_id <- "amws16_A_0_200_regex_only_pages1_191"
run_id <- env_chr("AMWS_GPT_PARSE_RUN_ID", default_run_id)
subdir <- env_chr("AMWS_GPT_PARSE_SUBDIR", "gpt54_mini_codex_parse")
run_dir <- env_chr(
  "AMWS_GPT_PARSE_RUN_DIR",
  file.path(DATA_OUTPUT, "amws", "transcription_runs", run_id, subdir)
)
run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)

source_file <- env_chr(
  "AMWS_GPT_PARSE_SOURCE_FILE",
  file.path(run_dir, "source_rows_for_consolidation.json")
)
source_file <- normalizePath(source_file, winslash = "/", mustWork = TRUE)

output_prefix <- env_chr("AMWS_GPT_PARSE_OUTPUT_PREFIX",
                         "amws_entries_gpt54_mini_codex_parsed")
batch_count_env <- env_int("AMWS_GPT_PARSE_BATCH_COUNT", NA_integer_)
batch_size <- env_int("AMWS_GPT_PARSE_BATCH_SIZE", 60L)
strict <- env_bool("AMWS_GPT_PARSE_STRICT", TRUE)

source_rows <- read_json_fallback(source_file, simplify_vector = TRUE)$data %>%
  as_tibble() %>%
  mutate(lineid = as.integer(lineid))

required_source_cols <- c("lineid", "batch_id", "prefix_text", "raw_text")
missing_source_cols <- setdiff(required_source_cols, names(source_rows))
if (length(missing_source_cols)) {
  stop("source_rows_for_consolidation.json is missing columns: ",
       paste(missing_source_cols, collapse = ", "))
}

batch_files <- list.files(run_dir, pattern = "^batch_[0-9]{2}_output\\.json$",
                          full.names = TRUE)
batch_ids_detected <- as.integer(sub("^batch_([0-9]{2})_output\\.json$", "\\1",
                                     basename(batch_files)))
if (length(batch_files) == 0) {
  stop("No batch output JSON files found in: ", run_dir)
}

batch_count <- if (is.na(batch_count_env)) {
  max(batch_ids_detected)
} else {
  batch_count_env
}

issues <- list()
encodings <- character()
parsed_batches <- list()

for (batch_id in seq_len(batch_count)) {
  batch_file <- file.path(run_dir, sprintf("batch_%02d_output.json", batch_id))
  if (!file.exists(batch_file)) {
    issues <- append_issue(issues, "missing_batch", batch_id,
                           detail = batch_file)
    next
  }

  parsed <- tryCatch(
    read_json_fallback(batch_file, simplify_vector = FALSE),
    error = function(e) e
  )
  if (inherits(parsed, "error")) {
    issues <- append_issue(issues, "json_parse_error", batch_id,
                           detail = conditionMessage(parsed))
    next
  }

  encodings <- c(encodings, parsed$encoding)
  batch_objects <- parsed$data
  if (!is.list(batch_objects)) {
    issues <- append_issue(issues, "batch_not_json_array", batch_id)
    next
  }

  for (idx in seq_along(batch_objects)) {
    object_names <- names(batch_objects[[idx]])
    required_names <- c("lineid", "birth_place", "birth_date", "field",
                        "confidence", "notes")
    if (!setequal(object_names, required_names) ||
        length(object_names) != length(required_names)) {
      issues <- append_issue(
        issues, "bad_keys", batch_id,
        detail = paste(sort(object_names), collapse = ",")
      )
    }
    confidence <- batch_objects[[idx]][["confidence"]]
    if (!isTRUE(confidence %in% c("high", "medium", "low"))) {
      lineid_value <- suppressWarnings(as.integer(batch_objects[[idx]][["lineid"]]))
      issues <- append_issue(issues, "bad_confidence", batch_id, lineid_value,
                             detail = as.character(confidence))
    }
  }

  batch_df <- bind_rows(batch_objects) %>%
    mutate(
      batch_file = sprintf("batch_%02d", batch_id),
      lineid = suppressWarnings(as.integer(lineid)),
      across(c(birth_place, birth_date, field, confidence, notes),
             ~ ifelse(is.na(.x), "", as.character(.x)))
    )

  expected_start <- (batch_id - 1L) * batch_size + 1L
  expected_end <- min(batch_id * batch_size, nrow(source_rows))
  expected_ids <- seq.int(expected_start, expected_end)

  if (expected_start > nrow(source_rows)) {
    issues <- append_issue(issues, "batch_beyond_source_rows", batch_id)
  } else if (!identical(sort(batch_df$lineid), expected_ids)) {
    issues <- append_issue(
      issues, "bad_batch_ids", batch_id,
      detail = paste0("got ", min(batch_df$lineid, na.rm = TRUE), "-",
                      max(batch_df$lineid, na.rm = TRUE), "; expected ",
                      expected_start, "-", expected_end)
    )
  }

  parsed_batches[[length(parsed_batches) + 1]] <- batch_df
}

parsed_rows <- bind_rows(parsed_batches)

if (nrow(parsed_rows) != nrow(source_rows)) {
  issues <- append_issue(
    issues, "bad_total_n",
    detail = paste0("got ", nrow(parsed_rows), "; expected ", nrow(source_rows))
  )
}

duplicate_lineids <- parsed_rows$lineid[duplicated(parsed_rows$lineid)]
if (length(duplicate_lineids)) {
  issues <- append_issue(
    issues, "duplicate_lineids",
    detail = paste(head(unique(duplicate_lineids), 50), collapse = ",")
  )
}

missing_lineids <- setdiff(source_rows$lineid, parsed_rows$lineid)
if (length(missing_lineids)) {
  issues <- append_issue(
    issues, "missing_lineids",
    detail = paste(head(missing_lineids, 50), collapse = ",")
  )
}

issues_file <- file.path(run_dir, "gpt54_mini_codex_validation_issues.json")
jsonlite::write_json(issues, issues_file, pretty = TRUE, auto_unbox = TRUE,
                     null = "null")

if (length(issues) && strict) {
  stop("Validation failed with ", length(issues),
       " issue(s). See: ", issues_file)
}

consolidated <- source_rows %>%
  select(lineid, batch_id, prefix_text, raw_text) %>%
  left_join(
    parsed_rows %>%
      select(batch_file, lineid, birth_place, birth_date, field, confidence,
             notes),
    by = "lineid"
  ) %>%
  transmute(
    batch_file,
    batch_id,
    lineid,
    raw_text,
    prefix_text,
    birth_place = ifelse(is.na(birth_place), "", birth_place),
    birth_date = ifelse(is.na(birth_date), "", birth_date),
    field = ifelse(is.na(field), "", field),
    confidence = ifelse(is.na(confidence), "", confidence),
    notes = ifelse(is.na(notes), "", notes)
  ) %>%
  arrange(lineid)

confidence_counts <- table(factor(consolidated$confidence,
                                  levels = c("high", "medium", "low")))
encoding_counts <- table(factor(encodings, levels = c("utf-8", "cp1252")))

summary_tbl <- tibble(
  metric = c(
    "run_dir",
    "source_file",
    "input_rows",
    "parsed_rows",
    "unique_lineids",
    "detected_batch_files",
    "expected_batch_count",
    "birth_place_nonempty",
    "birth_date_nonempty",
    "field_nonempty",
    "all_three_nonempty",
    "confidence_high",
    "confidence_medium",
    "confidence_low",
    "batch_files_utf8",
    "batch_files_cp1252",
    "validation_issues"
  ),
  value = as.character(c(
    run_dir,
    source_file,
    nrow(source_rows),
    nrow(consolidated),
    n_distinct(consolidated$lineid),
    length(batch_files),
    batch_count,
    nonempty_n(consolidated$birth_place),
    nonempty_n(consolidated$birth_date),
    nonempty_n(consolidated$field),
    sum(nzchar(trimws(consolidated$birth_place)) &
          nzchar(trimws(consolidated$birth_date)) &
          nzchar(trimws(consolidated$field))),
    unname(confidence_counts[["high"]]),
    unname(confidence_counts[["medium"]]),
    unname(confidence_counts[["low"]]),
    unname(encoding_counts[["utf-8"]]),
    unname(encoding_counts[["cp1252"]]),
    length(issues)
  ))
)

json_file <- file.path(run_dir, paste0(output_prefix, ".json"))
csv_file <- file.path(run_dir, paste0(output_prefix, ".csv"))
xlsx_file <- file.path(run_dir, paste0(output_prefix, ".xlsx"))
summary_file <- file.path(run_dir, "gpt54_mini_codex_run_summary.csv")

jsonlite::write_json(consolidated, json_file, pretty = TRUE,
                     auto_unbox = TRUE, na = "null")
readr::write_excel_csv(consolidated, csv_file, na = "")
readr::write_excel_csv(summary_tbl, summary_file, na = "")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "parsed_entries", gridLines = FALSE)
openxlsx::addWorksheet(wb, "run_summary", gridLines = FALSE)

openxlsx::writeDataTable(
  wb, "parsed_entries", consolidated,
  tableName = "parsed_entries",
  tableStyle = "TableStyleMedium2",
  withFilter = TRUE
)
openxlsx::freezePane(wb, "parsed_entries", firstRow = TRUE)
openxlsx::setColWidths(
  wb, "parsed_entries", cols = seq_along(consolidated),
  widths = c(12, 18, 10, 75, 75, 30, 18, 36, 14, 45)
)
wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
openxlsx::addStyle(
  wb, "parsed_entries", style = wrap_style,
  rows = 1:(nrow(consolidated) + 1L),
  cols = seq_along(consolidated),
  gridExpand = TRUE,
  stack = TRUE
)

openxlsx::writeDataTable(
  wb, "run_summary", summary_tbl,
  tableName = "run_summary",
  tableStyle = "TableStyleMedium4",
  withFilter = FALSE
)
openxlsx::freezePane(wb, "run_summary", firstRow = TRUE)
openxlsx::setColWidths(wb, "run_summary", cols = 1:2, widths = c(32, 90))

openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)

message("Wrote JSON: ", json_file)
message("Wrote CSV: ", csv_file)
message("Wrote XLSX: ", xlsx_file)
message("Wrote summary: ", summary_file)
message("Wrote validation issues: ", issues_file)
message("Rows: ", nrow(consolidated))
message("Validation issues: ", length(issues))
