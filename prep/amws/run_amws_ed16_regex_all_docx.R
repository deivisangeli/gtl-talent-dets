###############################################################################
# Run the regex-only AMWS edition 16 pipeline across all main DOCX files.
#
# Step 1: segment raw verbete text from DOCX files using the existing
#         regex-only segmentation script.
# Step 2: parse birth_place, birth_date, and field using the current reusable
#         regex parser rules.
#
# Outputs:
#   <Dropbox root>/output/amws/regex_all_docs/
#     amws_ed16_entries_regex_raw.csv
#     amws_ed16_entries_regex_parsed.csv
#     amws_ed16_entries_regex_parsed.xlsx
#     amws_ed16_doc_run_summary.csv
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tibble)
  library(writexl)
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

env_bool <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  value <- tolower(value)
  if (value %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (value %in% c("false", "f", "0", "no", "n")) return(FALSE)
  stop("Environment variable ", name, " must be TRUE/FALSE; got: ", value)
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x), "", x)
}

normalize_text <- function(x) {
  blank_na(x) |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

strip_edge_punct <- function(x) {
  normalize_text(x) |>
    str_replace("^[,;:.\\s'\"’“”\\-_*\\^]+", "") |>
    str_replace("[,;:.\\s'\"’“”\\-_*\\^]+$", "") |>
    normalize_text()
}

MONTH_RX <- paste(
  "January", "February", "March", "April", "September", "October",
  "November", "December", "June", "July", "August", "Sept", "Sep",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct",
  "Nov", "Dec",
  sep = "|"
)

DATE_RX <- paste0(
  "\\b(", MONTH_RX, ")\\.?\\s*,?\\s+",
  "[0-9OISZLB]{1,2}",
  "(?:\\s*[,.'’`-]?\\s*[0-9OISZLB]{1,4}[A-Za-z]?)?\\b"
)

OCR_NOV_RX <- "\\bN[o0][*v]\\s*J?[A-Za-z0-9]{2,4}\\b"

find_first_date <- function(x) {
  x <- normalize_text(x)
  loc <- str_locate(x, regex(DATE_RX, ignore_case = TRUE))
  if (!is.na(loc[1, 1])) {
    return(tibble(
      date = str_sub(x, loc[1, 1], loc[1, 2]) |> normalize_text(),
      start = loc[1, 1],
      end = loc[1, 2],
      rule_id = "R_DATE_FLEX_MONTH"
    ))
  }

  loc <- str_locate(x, regex(OCR_NOV_RX, ignore_case = TRUE))
  if (!is.na(loc[1, 1])) {
    return(tibble(
      date = str_sub(x, loc[1, 1], loc[1, 2]) |> normalize_text(),
      start = loc[1, 1],
      end = loc[1, 2],
      rule_id = "R_DATE_OCR_NOVEMBER"
    ))
  }

  tibble(date = "", start = NA_integer_, end = NA_integer_, rule_id = "")
}

find_birth_marker <- function(raw_text) {
  marker_patterns <- c(
    "R_BIRTH_MARKER_SPACED" =
      "(?:^|[,.;:\\s])b\\s+(?=[A-Z])",
    "R_BIRTH_MARKER_H_SPACED" =
      "(?i)(?:^|[,.;:])\\s*h\\s+(?=[A-Z])",
    "R_BIRTH_MARKER_GLUED" =
      "(?:[,.;:'’*])\\s*b_?\\s*(?=[A-Z])",
    "R_BIRTH_MARKER_UNDERSCORE" =
      "(?:^|[,.;:\\s])b_+\\s*(?=[A-Z])"
  )

  hits <- bind_rows(lapply(names(marker_patterns), function(rule_id) {
    loc <- str_locate(raw_text, regex(marker_patterns[[rule_id]]))
    if (is.na(loc[1, 1])) return(NULL)
    tibble(rule_id = rule_id, start = loc[1, 1], end = loc[1, 2])
  }))

  if (!nrow(hits)) {
    return(tibble(rule_id = "", start = NA_integer_, end = NA_integer_))
  }
  hits |> arrange(start) |> slice(1)
}

looks_like_entry_name <- function(raw_text) {
  str_detect(
    raw_text,
    regex("^[A-Z][A-Z0-9'’(). -]{1,55},\\s+[A-Z][A-Za-z0-9'’(). -]{1,95}")
  )
}

extract_name_raw <- function(raw_text) {
  raw_text <- normalize_text(raw_text)
  marker <- find_birth_marker(raw_text)
  if (nzchar(marker$rule_id[[1]]) && marker$start[[1]] > 1) {
    return(strip_edge_punct(str_sub(raw_text, 1, marker$start[[1]] - 1)))
  }

  marker_loc <- str_locate(
    raw_text,
    regex("\\b(?:see\\s+previous\\s+edition|see\\s+previous|deceased)\\b",
          ignore_case = TRUE)
  )
  if (!is.na(marker_loc[1, 1])) {
    prefix <- str_sub(raw_text, 1, marker_loc[1, 1] - 1)
    parts <- str_split(prefix, ",")[[1]] |> str_trim()
    parts <- parts[nzchar(parts)]
    if (length(parts) >= 2) {
      name_parts <- parts[1:2]
      if (length(parts) >= 3 &&
          str_detect(parts[3], regex("^(JR|SR|II|III|IV)\\b",
                                    ignore_case = TRUE))) {
        name_parts <- parts[1:3]
      }
      return(strip_edge_punct(paste(name_parts, collapse = ", ")))
    }
  }

  if (looks_like_entry_name(raw_text)) {
    parts <- str_split(raw_text, ",")[[1]] |> str_trim()
    parts <- parts[nzchar(parts)]
    if (length(parts) >= 2) {
      return(strip_edge_punct(paste(parts[1:2], collapse = ", ")))
    }
  }

  ""
}

normalize_birth_date <- function(x) {
  x <- normalize_text(x)
  x <- str_replace(x, regex("\\b([0-9]{1,4})m\\b$"), "\\1")
  x <- str_replace(x, regex("^([A-Z][a-z]+),\\s*([0-9]{1,2}),\\s*([0-9]{2,4})$"),
                   "\\1 \\2, \\3")
  x <- str_replace(x, regex("^([A-Z][a-z]+)\\s+([0-9]{1,2})\\.\\s*([0-9]{2,4})$"),
                   "\\1 \\2, \\3")
  x <- str_replace(x, regex("^([A-Z][a-z]+)\\s+([0-9]{1,2})\\s+([0-9]{2,4})$"),
                   "\\1 \\2, \\3")
  x <- str_replace(x, regex("^([A-Z][a-z]+)\\s+([0-9]{1,2}),([0-9]{2,4})$"),
                   "\\1 \\2, \\3")
  str_squish(x)
}

normalize_birth_place <- function(x) {
  x <- normalize_text(x)
  x <- str_replace_all(x, "\\s*,\\s*", ", ")
  str_squish(x)
}

strip_demographic_prefix <- function(x) {
  x <- normalize_text(x)
  old <- NA_character_
  while (!identical(old, x)) {
    old <- x
    x <- x |>
      str_replace(regex("^[,;:.\\s'\"\\-_*]+"), "") |>
      str_replace(regex("^(?:US\\s+citizen|Can\\s+citizen|nat\\s+US)\\b[,;:.\\s-]*",
                        ignore_case = TRUE), "") |>
      str_replace(regex("^(?:wid|div|sep)\\b[,;:.\\s-]*",
                        ignore_case = TRUE), "") |>
      str_replace(regex("^(?:m|c)\\s*[0-9OISZLB]{0,4}[-]?[A-Za-z]?\\b[,;:.\\s-]*",
                        ignore_case = TRUE), "") |>
      str_replace(regex("^[0-9]{1,4}[A-Za-z]?\\s+[0-9]{0,3}[A-Za-z]?\\s+(?=[A-Z])"), "") |>
      str_replace(regex("^[0-9]{1,4}[A-Za-z]?\\s+(?=[A-Z])"), "") |>
      str_replace(regex("^c(?=[A-Z])"), "") |>
      str_replace(regex("^[A-Z]{1,3}\\s*[0-9]{1,2}\\s+(?=[A-Z])"), "") |>
      str_replace(regex("^[^A-Za-z]{1,16}"), "")
  }
  strip_edge_punct(x)
}

normalize_field <- function(x) {
  x <- normalize_text(x)
  x <- str_replace(x, regex("^ail,\\s*"), "")
  x <- str_replace_all(x, "\\s+", " ")
  strip_edge_punct(x)
}

clean_field_candidate <- function(x) {
  x <- strip_demographic_prefix(x)
  x <- x |>
    str_replace("\\\\B.*$", "") |>
    str_replace(regex("\\bA\\s+EOu.*$", ignore_case = TRUE), "") |>
    str_replace(regex("^A.{1,4}'(?=[A-Z])"), "") |>
    str_replace(regex("^[A-Z][^A-Z]{1,4}[A-Z]'(?=[A-Z])"), "") |>
    str_replace(regex("^c(?=[A-Z])"), "") |>
    strip_edge_punct()
  x
}

safe_field_candidate <- function(field) {
  field <- normalize_text(field)
  nzchar(field) &&
    nchar(field) <= 140 &&
    !str_detect(field, regex("\\b(Educ|Prof\\s+Exp|Mailing\\s*Add|Univ|PhD|Col|Dept|Assoc|Professor|prof|res)\\b",
                            ignore_case = TRUE)) &&
    !str_detect(field, "[0-9]") &&
    !str_detect(field, "[\\^?\\\\<>\\u25a0]") &&
    !str_detect(field, regex("^[0-9]+\\.?\\s*$"))
}

extract_field_from_source <- function(field_source) {
  field_source <- clean_field_candidate(field_source)
  if (!nzchar(field_source)) return("")

  stop_loc <- str_locate(
    field_source,
    regex("\\b(Educ|Prof\\s+Exp|Concurrent\\s+Pos|Honors\\s*&\\s*Awards|Mem|Res|Mailing\\s*Add|Exp)\\b\\s*[:;.!\\-\\u25a0]?",
          ignore_case = TRUE)
  )
  if (!is.na(stop_loc[1, 1])) {
    field <- str_sub(field_source, 1, stop_loc[1, 1] - 1)
  } else {
    field <- str_split(field_source, "\\.\\s+", n = 2)[[1]][1]
  }

  field <- normalize_field(toupper(clean_field_candidate(field)))
  if (!safe_field_candidate(field)) return("")
  field
}

extract_previous_edition_field <- function(raw_text, name_raw) {
  marker <- str_locate(
    raw_text,
    regex("\\b(?:see\\s+previous(?:\\s+edition)?|deceased)\\b",
          ignore_case = TRUE)
  )
  if (is.na(marker[1, 1])) return("")

  prefix <- str_sub(raw_text, 1, marker[1, 1] - 1)
  original_prefix <- prefix
  if (nzchar(name_raw) && str_starts(prefix, fixed(name_raw))) {
    prefix <- str_sub(prefix, nchar(name_raw) + 1)
  }
  if (!nzchar(strip_edge_punct(prefix))) {
    parts <- str_split(original_prefix, ",")[[1]] |> str_trim()
    parts <- parts[nzchar(parts)]
    if (length(parts) >= 2) {
      prefix <- parts[length(parts)]
    }
  }
  strip_edge_punct(prefix)
}

extract_field_before_educ <- function(raw_text, name_raw) {
  if (!str_detect(raw_text, regex("\\bEduc\\b", ignore_case = TRUE))) {
    return("")
  }
  before_educ <- str_replace(raw_text, regex("\\bEduc\\b.*$", ignore_case = TRUE),
                             "")
  if (nzchar(name_raw) && str_starts(before_educ, fixed(name_raw))) {
    before_educ <- str_sub(before_educ, nchar(name_raw) + 1)
  }
  candidates <- c(
    str_match(before_educ, regex("(?:^|[;,.]\\s*)(?:US\\s+citizen;?\\s*)?(?:nat\\s+US;?\\s*)?(?:m|c)\\s*[0-9OISZLB]{0,4}[-]?[A-Za-z]?\\b[,;:.\\s-]*(.+)$",
                                ignore_case = TRUE))[1, 2],
    str_match(before_educ, regex("(?:^|[;,.]\\s*)c(?=[A-Z])(.+)$",
                                ignore_case = TRUE))[1, 2],
    str_match(before_educ, regex("[,.-]\\s*([^,.;-]{4,120})\\s*$",
                                ignore_case = TRUE))[1, 2],
    before_educ
  )
  candidates <- candidates[!is.na(candidates)]
  for (candidate in candidates) {
    field <- extract_field_from_source(candidate)
    if (nzchar(field)) return(field)
  }
  ""
}

extract_birth <- function(raw_text) {
  marker <- find_birth_marker(raw_text)
  if (!nzchar(marker$rule_id[[1]])) {
    return(tibble(
      birth_place = "",
      birth_date = "",
      after_birth_date = "",
      birth_rule_id = "",
      birth_flag = "no_birth"
    ))
  }

  after_birth <- str_sub(raw_text, marker$end[[1]] + 1) |> normalize_text()
  date <- find_first_date(after_birth)
  if (!nzchar(date$date[[1]])) {
    return(tibble(
      birth_place = strip_edge_punct(str_split(after_birth, ";", n = 2)[[1]][1]),
      birth_date = "",
      after_birth_date = after_birth,
      birth_rule_id = marker$rule_id[[1]],
      birth_flag = "no_birth_date"
    ))
  }

  tibble(
    birth_place = str_sub(after_birth, 1, date$start[[1]] - 1) |>
      strip_edge_punct() |>
      normalize_birth_place(),
    birth_date = normalize_birth_date(date$date[[1]]),
    after_birth_date = str_sub(after_birth, date$end[[1]] + 1) |>
      normalize_text(),
    birth_rule_id = paste(marker$rule_id[[1]], date$rule_id[[1]], sep = "+"),
    birth_flag = "ok"
  )
}

parse_entry_regex <- function(raw_text) {
  raw_text <- normalize_text(raw_text)
  name_raw <- extract_name_raw(raw_text)
  birth <- extract_birth(raw_text)

  previous_field <- extract_previous_edition_field(raw_text, name_raw)
  field <- ""
  field_rule_id <- ""
  if (nzchar(previous_field)) {
    field <- extract_field_from_source(previous_field)
    if (nzchar(field)) field_rule_id <- "R_FIELD_BEFORE_SEE_PREVIOUS"
  }

  if (!nzchar(field)) {
    field <- extract_field_from_source(birth$after_birth_date[[1]])
    if (nzchar(field)) field_rule_id <- "R_FIELD_AFTER_DATE"
  }

  if (!nzchar(field)) {
    field <- extract_field_before_educ(raw_text, name_raw)
    if (nzchar(field)) field_rule_id <- "R_FIELD_BEFORE_EDUC"
  }

  parse_flags <- c()
  if (birth$birth_flag[[1]] != "ok") {
    parse_flags <- c(parse_flags, birth$birth_flag[[1]])
  }
  if (!nzchar(field)) parse_flags <- c(parse_flags, "no_field")
  if (!length(parse_flags)) parse_flags <- "ok"

  tibble(
    name_raw = name_raw,
    birth_place = birth$birth_place[[1]],
    birth_date = birth$birth_date[[1]],
    field = field,
    regex_birth_rule_id = birth$birth_rule_id[[1]],
    regex_field_rule_id = field_rule_id,
    regex_parse_flag = paste(parse_flags, collapse = ";")
  )
}

read_entries_workbook <- function(path) {
  sheets <- readxl::excel_sheets(path)
  sheet <- if ("entries" %in% sheets) "entries" else sheets[[1]]
  readxl::read_xlsx(path, sheet = sheet) |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
}

run_step1 <- function(docx_file, run_id, force) {
  run_dir <- file.path(DATA_OUTPUT, "amws", "transcription_runs", run_id)
  raw_file <- file.path(run_dir, "amws_entries_raw_combined.xlsx")
  merge_file <- file.path(run_dir, "regex_contextual_merges.csv")
  if (file.exists(raw_file) && !force) {
    return(tibble(run_id = run_id, step1_status = "skipped_existing",
                  step1_exit_status = 0L, raw_file = raw_file,
                  merge_file = merge_file))
  }

  step1_script <- file.path(repo_root, "prep", "amws",
                            "build_amws_1986_ed16_first10_regex_raw_xlsx.R")
  env_names <- c(
    "AMWS_REGEX_INPUT_FILE",
    "AMWS_REGEX_RUN_ID",
    "AMWS_REGEX_PROCESS_FULL_DOC",
    "AMWS_REGEX_COMBINE_WITH_FIRST10"
  )
  old_env <- Sys.getenv(env_names, unset = NA_character_)
  names(old_env) <- env_names
  on.exit({
    for (env_name in env_names) {
      if (is.na(old_env[[env_name]])) {
        Sys.unsetenv(env_name)
      } else {
        Sys.setenv(structure(old_env[[env_name]], names = env_name))
      }
    }
  }, add = TRUE)

  Sys.setenv(
    AMWS_REGEX_INPUT_FILE = normalizePath(docx_file, winslash = "/", mustWork = TRUE),
    AMWS_REGEX_RUN_ID = run_id,
    AMWS_REGEX_PROCESS_FULL_DOC = "TRUE",
    AMWS_REGEX_COMBINE_WITH_FIRST10 = "FALSE"
  )
  status <- system2("Rscript", step1_script)
  if (!file.exists(raw_file)) {
    return(tibble(run_id = run_id, step1_status = "raw_missing",
                  step1_exit_status = as.integer(status), raw_file = raw_file,
                  merge_file = merge_file))
  }
  tibble(run_id = run_id, step1_status = "ok",
         step1_exit_status = as.integer(status), raw_file = raw_file,
         merge_file = merge_file)
}

parse_run <- function(docx_file, doc_id, run_id, raw_file) {
  run_dir <- dirname(raw_file)
  raw <- read_entries_workbook(raw_file) |>
    mutate(
      doc_id = doc_id,
      source_file = normalizePath(docx_file, winslash = "/", mustWork = TRUE),
      run_id = run_id,
      raw_text = normalize_text(raw_text)
    )

  if (!"lineid" %in% names(raw)) {
    raw <- raw |> mutate(lineid = row_number(), .before = 1)
  }

  parsed <- bind_rows(lapply(raw$raw_text, parse_entry_regex))
  out <- bind_cols(
    raw |> select(doc_id, source_file, run_id, everything()),
    parsed
  ) |>
    select(doc_id, source_file, run_id, lineid, raw_text,
           birth_place, birth_date, field, name_raw,
           regex_birth_rule_id, regex_field_rule_id, regex_parse_flag,
           everything())

  parsed_csv <- file.path(run_dir, "amws_entries_regex_parsed.csv")
  parsed_xlsx <- file.path(run_dir, "amws_entries_regex_parsed.xlsx")
  summary_csv <- file.path(run_dir, "amws_entries_regex_summary.csv")

  summary <- tibble(
    doc_id = doc_id,
    run_id = run_id,
    n_entries = nrow(out),
    raw_text_empty = sum(!nzchar(out$raw_text)),
    duplicated_lineid = anyDuplicated(out$lineid) > 0,
    birth_place_nonempty = sum(nzchar(out$birth_place)),
    birth_date_nonempty = sum(nzchar(out$birth_date)),
    field_nonempty = sum(nzchar(out$field)),
    no_birth = sum(str_detect(out$regex_parse_flag, fixed("no_birth"))),
    no_birth_date = sum(str_detect(out$regex_parse_flag, fixed("no_birth_date"))),
    no_field = sum(str_detect(out$regex_parse_flag, fixed("no_field")))
  )

  readr::write_excel_csv(out, parsed_csv, na = "")
  writexl::write_xlsx(list(entries = out, summary = summary), parsed_xlsx)
  readr::write_excel_csv(summary, summary_csv, na = "")

  list(raw = raw, parsed = out, summary = summary)
}

input_dir <- file.path(TALENT_DETS_DATA_DIR, "Data", "amws ed 16")
if (!dir.exists(input_dir)) stop("Input directory not found: ", input_dir)

docx_files <- list.files(input_dir, pattern = "\\.docx$", full.names = TRUE)
docx_files <- docx_files[
  !str_detect(basename(docx_files), regex("precleaning|^16_Index_", ignore_case = TRUE))
]
docx_files <- sort(normalizePath(docx_files, winslash = "/", mustWork = TRUE))

force_step1 <- env_bool("AMWS_REGEX_ALL_DOCS_FORCE_STEP1", TRUE)
output_dir <- Sys.getenv(
  "AMWS_REGEX_ALL_DOCS_OUTPUT_DIR",
  unset = file.path(DATA_OUTPUT, "amws", "regex_all_docs")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

all_raw <- list()
all_parsed <- list()
summaries <- list()
step1_statuses <- list()
contextual_merges <- list()

cat("DOCX files:", length(docx_files), "\n")
for (i in seq_along(docx_files)) {
  docx_file <- docx_files[[i]]
  stem <- tools::file_path_sans_ext(basename(docx_file))
  doc_id <- paste0("amws", stem)
  run_id <- paste0(doc_id, "_regex_all_docs")
  cat(sprintf("[%02d/%02d] %s\n", i, length(docx_files), basename(docx_file)))

  step1 <- run_step1(docx_file, run_id, force_step1)
  step1_statuses[[i]] <- mutate(step1, doc_id = doc_id, source_file = docx_file)
  if (!identical(step1$step1_status[[1]], "ok") &&
      !identical(step1$step1_status[[1]], "skipped_existing")) {
    warning("Skipping parse for failed raw extraction: ", basename(docx_file))
    next
  }

  parsed <- parse_run(docx_file, doc_id, run_id, step1$raw_file[[1]])
  all_raw[[length(all_raw) + 1L]] <- parsed$raw
  all_parsed[[length(all_parsed) + 1L]] <- parsed$parsed
  summaries[[length(summaries) + 1L]] <- parsed$summary

  if (file.exists(step1$merge_file[[1]])) {
    merges <- readr::read_csv(step1$merge_file[[1]],
                              col_types = readr::cols(.default = "c"),
                              show_col_types = FALSE) |>
      mutate(
        doc_id = doc_id,
        source_file = normalizePath(docx_file, winslash = "/", mustWork = TRUE),
        run_id = run_id,
        .before = 1
      )
    contextual_merges[[length(contextual_merges) + 1L]] <- merges
  }
}

raw_all <- bind_rows(all_raw)
parsed_all <- bind_rows(all_parsed)
summary_all <- bind_rows(summaries)
step1_all <- bind_rows(step1_statuses)
contextual_merges_all <- bind_rows(contextual_merges)
if (!nrow(parsed_all)) {
  status_csv <- file.path(output_dir, "amws_ed16_doc_run_summary.csv")
  readr::write_excel_csv(step1_all, status_csv, na = "")
  stop("No documents were parsed. Step-1 status written to: ", status_csv)
}
doc_summary <- step1_all |>
  select(doc_id, source_file, run_id, step1_status, step1_exit_status, raw_file) |>
  left_join(summary_all, by = c("doc_id", "run_id"))

raw_csv <- file.path(output_dir, "amws_ed16_entries_regex_raw.csv")
parsed_csv <- file.path(output_dir, "amws_ed16_entries_regex_parsed.csv")
parsed_xlsx <- file.path(output_dir, "amws_ed16_entries_regex_parsed.xlsx")
summary_csv <- file.path(output_dir, "amws_ed16_doc_run_summary.csv")
contextual_merges_csv <- file.path(output_dir,
                                   "amws_ed16_regex_contextual_merges.csv")

readr::write_excel_csv(raw_all, raw_csv, na = "")
readr::write_excel_csv(parsed_all, parsed_csv, na = "")
readr::write_excel_csv(contextual_merges_all, contextual_merges_csv, na = "")
writexl::write_xlsx(
  list(entries = parsed_all, doc_summary = doc_summary,
       contextual_merges = contextual_merges_all),
  parsed_xlsx
)
readr::write_excel_csv(doc_summary, summary_csv, na = "")

cat("Wrote raw CSV:", raw_csv, "\n")
cat("Wrote parsed CSV:", parsed_csv, "\n")
cat("Wrote parsed XLSX:", parsed_xlsx, "\n")
cat("Wrote summary:", summary_csv, "\n")
cat("Wrote contextual merge audit:", contextual_merges_csv, "\n")
cat("Total parsed entries:", nrow(parsed_all), "\n")
