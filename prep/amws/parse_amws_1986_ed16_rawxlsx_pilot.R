###############################################################################
# Regex parser for the AMWS edition 16 (1986) raw-XLSX pilot.
#
# Input:
#   <Dropbox root>/output/amws/transcription_runs/
#     amws16_A_0_200_precleaning_first10_rawxlsx_4agents/
#     amws_entries_raw_combined.xlsx
#
# Outputs:
#   <same run directory>/amws_entries_regex_parsed.csv
#   <same run directory>/amws_entries_regex_audit_flags.csv
#
# Scope:
#   Parse birth_place, birth_date, and AMWS discipline field from verbetes that
#   were already segmented manually by agents. This script does not split
#   entries and does not modify the raw XLSX. Conservative field normalizations
#   live here so the final XLSX consolidation does not repeat them.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
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
input_file <- file.path(run_dir, "amws_entries_raw_combined.xlsx")
parsed_file <- file.path(run_dir, "amws_entries_regex_parsed.csv")
audit_file <- file.path(run_dir, "amws_entries_regex_audit_flags.csv")

if (!file.exists(input_file)) {
  stop("Input workbook not found: ", input_file)
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
  "[0-9OISZLB]{1,2}\\s*[,.'’`-]?\\s*",
  "[0-9OISZLB]{1,4}[A-Za-z]?\\b"
)

OCR_NOV_RX <- "\\bN[o0][*v]\\s*J?[A-Za-z0-9]{2,4}\\b"

normalize_text <- function(x) {
  x |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

strip_edge_punct <- function(x) {
  x <- blank_na(x)
  x |>
    str_replace("^[,;:.\\s'\"\\-_*]+", "") |>
    str_replace("[,;:.\\s'\"\\-_*]+$", "") |>
    normalize_text()
}

blank_na <- function(x) {
  x <- if_else(is.na(x), "", x)
  x
}

normalize_birth_place <- function(x) {
  x <- blank_na(x) |> normalize_text()
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_replace(x, regex("(^|,\\s*)111\\b", ignore_case = TRUE),
                   "\\1Ill")
  x <- str_replace(x, regex("\\bWashington\\s+DC\\b"), "Washington, DC")
  x <- str_replace(x, regex("\\bDadeviUe\\s+Ala\\b"), "Dadeville, Ala")
  x <- str_replace(x, regex("^([A-Za-z][A-Za-z .'-]+)\\.\\s+([A-Za-z]{2,})$"),
                   "\\1, \\2")
  x <- str_replace(x, regex("\\bBrun,wick\\b"), "Brunswick")
  x <- str_replace(x, "\u2019\\s+", ", ")
  x <- str_replace_all(x, "\\s*,\\s*", ", ")
  str_squish(x)
}

normalize_birth_date <- function(x) {
  x <- blank_na(x) |> normalize_text()
  x <- str_replace(x, regex("^([A-Z][a-z]+)\\s+II\\.\\s*([0-9]{2,4})$"),
                   "\\1 11, \\2")
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

normalize_field <- function(x) {
  x <- blank_na(x) |> normalize_text()
  x <- str_replace(x, regex("^ail,\\s*"), "")
  x <- str_replace(x, regex("^4ECOLOGY$"), "ECOLOGY")
  x <- str_replace(x, regex("^c\\s*4ECOLOGY$", ignore_case = TRUE), "ECOLOGY")
  x <- str_replace_all(x, "SED1MENTOLOGY", "SEDIMENTOLOGY")
  x <- str_replace_all(x, "IMMUNOCHEM1STRY", "IMMUNOCHEMISTRY")
  x <- str_replace(x, regex("^[A-Z]{1,3}\\s*[0-9]{1,2}\\s+(?=[A-Z])"), "")
  x <- str_replace(x, regex("^1\\s+LECTRON\\s+"), "ELECTRON ")
  x <- str_replace(x, regex("^LS\\s+citizen,\\s*m\\s+[0-9]+;\\s*c\\s+[0-9]+\\.\\s*",
                           ignore_case = TRUE), "")
  x <- str_replace(x, regex("^UScitizen;\\s*m\\s+[0-9]+;\\s*c\\s+[0-9]+\\s*",
                           ignore_case = TRUE), "")
  x <- str_replace(x, regex("^[0-9]{2}\\.\\s+"), "")
  x <- str_replace(x, regex("^A\u00bbI'(?=SOLID STATE PHYSICS)"), "")
  str_squish(x)
}

find_first_date <- function(x) {
  x <- blank_na(x) |> normalize_text()
  loc <- str_locate(x, regex(DATE_RX, ignore_case = TRUE))
  if (!is.na(loc[1, 1])) {
    return(tibble(
      date = str_sub(x, loc[1, 1], loc[1, 2]) |> normalize_text(),
      start = loc[1, 1],
      end = loc[1, 2],
      rule_id = "date_flex_month"
    ))
  }

  loc <- str_locate(x, regex(OCR_NOV_RX, ignore_case = TRUE))
  if (!is.na(loc[1, 1])) {
    return(tibble(
      date = str_sub(x, loc[1, 1], loc[1, 2]) |> normalize_text(),
      start = loc[1, 1],
      end = loc[1, 2],
      rule_id = "date_ocr_november"
    ))
  }

  tibble(date = "", start = NA_integer_, end = NA_integer_, rule_id = "")
}

find_birth_marker <- function(raw_text) {
  marker_patterns <- c(
    "birth_marker_spaced" = "(?i)(?:^|[,.;:\\s])(?:b|h)\\s+(?=[A-Z])",
    "birth_marker_glued" = "(?i)(?:\\.|['\\u2019*])b[_\\s]*(?=[A-Z])",
    "birth_marker_underscore" = "(?i)(?:^|[,.;:\\s])b_+\\s*(?=[A-Z])"
  )

  hits <- bind_rows(lapply(names(marker_patterns), function(rule_id) {
    loc <- str_locate(raw_text, regex(marker_patterns[[rule_id]]))
    if (is.na(loc[1, 1])) {
      return(NULL)
    }
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
    regex("^[A-Z][A-Z0-9'’(). -]{1,45},\\s+[A-Z][A-Za-z0-9'’(). -]{1,90}")
  )
}

extract_name_raw <- function(raw_text) {
  raw_text <- normalize_text(raw_text)

  birth_match <- str_match(raw_text, "^\\s*(.*?)[,\\.]\\s*b\\s+(.+)$")
  if (!is.na(birth_match[1, 1])) {
    return(strip_edge_punct(birth_match[1, 2]))
  }

  marker <- str_locate(
    raw_text,
    regex("\\b(?:see\\s+previous\\s+edition|deceased)\\b",
          ignore_case = TRUE)
  )
  if (!is.na(marker[1, 1])) {
    prefix <- str_sub(raw_text, 1, marker[1, 1] - 1)
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

  educ_marker <- str_locate(raw_text, regex("\\bEduc\\s*[:;.!-]",
                                            ignore_case = TRUE))
  if (!is.na(educ_marker[1, 1]) && looks_like_entry_name(raw_text)) {
    prefix <- str_sub(raw_text, 1, educ_marker[1, 1] - 1)
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

  fallback <- str_match(raw_text, "^\\s*([^.;]{3,120})")[1, 2]
  strip_edge_punct(blank_na(fallback))
}

extract_birth <- function(raw_text) {
  marker <- find_birth_marker(raw_text)

  if (!nzchar(marker$rule_id[[1]])) {
    return(tibble(
      birthplace = "",
      birth_date = "",
      after_birth_date = "",
      birth_flag = "no_birth"
    ))
  }

  after_birth <- str_sub(raw_text, marker$end[[1]] + 1) |> normalize_text()
  date <- find_first_date(after_birth)

  if (!nzchar(date$date[[1]])) {
    place <- str_split(after_birth, ";|\\.\\s+[A-Z]{2,}", n = 2)[[1]][1] |>
      strip_edge_punct()
    return(tibble(
      birthplace = place,
      birth_date = "",
      after_birth_date = after_birth,
      birth_flag = "no_birth_date"
    ))
  }

  tibble(
    birthplace = str_sub(after_birth, 1, date$start[[1]] - 1) |>
      strip_edge_punct(),
    birth_date = date$date[[1]],
    after_birth_date = str_sub(after_birth, date$end[[1]] + 1) |>
      normalize_text(),
    birth_flag = "ok"
  )
}

strip_demographic_prefix <- function(x) {
  x <- blank_na(x) |> normalize_text()
  old <- NA_character_
  while (!identical(old, x)) {
    old <- x
    x <- x |>
      str_replace(regex("^[,;:.\\s'\"\\-_*]+"), "") |>
      str_replace(regex("^(?:US\\s+citizen|nat\\s+US)\\b[,;:.\\s-]*",
                        ignore_case = TRUE), "") |>
      str_replace(regex("^US[A-Za-z]{2,};\\s*[0-9OISZLB]{0,4}\\.?\\s*",
                        ignore_case = TRUE), "") |>
      str_replace(regex("^(?:wid|div|sep)\\b[,;:.\\s-]*",
                        ignore_case = TRUE), "") |>
      str_replace(regex("^(?:m|c)\\s*[0-9OISZLB]{0,4}[-]?[A-Za-z]?\\b[,;:.\\s-]*",
                        ignore_case = TRUE), "") |>
      str_replace(regex("^c(?=[A-Z])", ignore_case = TRUE), "") |>
      str_replace(regex("^[A-Z]{1,3}\\s*[0-9]{1,2}\\s+(?=[A-Z])"), "") |>
      str_replace(regex("^[^A-Za-z]{1,12}"), "")
  }
  x |> strip_edge_punct()
}

clean_field_candidate <- function(x) {
  x <- strip_demographic_prefix(x)
  x <- x |>
    str_replace("\\\\B.*$", "") |>
    str_replace(regex("\\bA\\s+EOu.*$", ignore_case = TRUE), "") |>
    str_replace(regex("^A.{1,4}'(?=[A-Z])"), "") |>
    str_replace(regex("^[A-Z][^A-Z]{1,4}[A-Z]'?"), "") |>
    str_replace(regex("^c(?=[A-Z])", ignore_case = TRUE), "") |>
    strip_edge_punct() |>
    str_replace_all("\\s+", " ")
  x
}

safe_field_candidate <- function(field) {
  field <- blank_na(field) |> normalize_text()
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
  if (!nzchar(field_source)) {
    return("")
  }

  stop_loc <- str_locate(
    field_source,
    regex("\\b(Educ|Prof\\s+Exp|Concurrent\\s+Pos|Honors\\s*&\\s*Awards|Mem|Res|Mailing\\s*Add)\\s*[:;.!\\-\\u25a0]?",
          ignore_case = TRUE)
  )
  if (!is.na(stop_loc[1, 1])) {
    field <- str_sub(field_source, 1, stop_loc[1, 1] - 1)
  } else {
    field <- str_split(field_source, "\\.\\s+", n = 2)[[1]][1]
  }

  field <- clean_field_candidate(field)
  field <- normalize_field(toupper(field))
  if (!safe_field_candidate(field)) {
    return("")
  }
  field
}

extract_field_before_educ <- function(raw_text) {
  if (!str_detect(raw_text, regex("\\bEduc\\b", ignore_case = TRUE))) {
    return("")
  }
  before_educ <- str_replace(raw_text, regex("\\bEduc\\b.*$", ignore_case = TRUE),
                             "")
  candidates <- c(
    str_match(before_educ, regex("(?:^|[;,.]\\s*)(?:US\\s+citizen;?\\s*)?(?:nat\\s+US;?\\s*)?(?:m|c)\\s*[0-9OISZLB]{0,4}[-]?[A-Za-z]?\\b[,;:.\\s-]*(.+)$",
                                ignore_case = TRUE))[1, 2],
    str_match(before_educ, regex("(?:^|[;,.]\\s*)c(?=[A-Z])(.+)$",
                                ignore_case = TRUE))[1, 2],
    str_match(before_educ, regex("[,.-]\\s*([^,.;-]{4,120})\\s*$",
                                ignore_case = TRUE))[1, 2]
  )
  candidates <- candidates[!is.na(candidates)]
  for (candidate in candidates) {
    field <- extract_field_from_source(candidate)
    if (nzchar(field)) {
      return(field)
    }
  }
  ""
}

extract_previous_edition_field <- function(raw_text, name_raw) {
  marker <- str_locate(
    raw_text,
    regex("\\b(?:see\\s+previous(?:\\s+edition)?|deceased)\\b",
          ignore_case = TRUE)
  )
  if (is.na(marker[1, 1]) || !nzchar(name_raw)) {
    return("")
  }

  prefix <- str_sub(raw_text, 1, marker[1, 1] - 1)
  field <- str_sub(prefix, nchar(name_raw) + 1) |>
    strip_edge_punct()
  field
}

extract_field <- function(raw_text, name_raw, birth_info) {
  previous_field <- extract_previous_edition_field(raw_text, name_raw)
  if (nzchar(previous_field)) {
    field <- extract_field_from_source(previous_field)
    if (nzchar(field)) {
      return(field)
    }
  }

  field_source <- birth_info$after_birth_date[[1]]
  if (!nzchar(field_source)) {
    if (birth_info$birth_flag[[1]] == "no_birth" &&
        !looks_like_entry_name(raw_text)) {
      return("")
    }
    field_source <- raw_text
    if (nzchar(name_raw) && str_starts(field_source, fixed(name_raw))) {
      field_source <- str_sub(field_source, nchar(name_raw) + 1)
    }
  }

  field_source <- strip_demographic_prefix(field_source)
  if (!nzchar(field_source)) {
    return(extract_field_before_educ(raw_text))
  }

  field <- extract_field_from_source(field_source)
  if (!nzchar(field)) {
    field <- extract_field_before_educ(raw_text)
  }

  field
}

make_parse_flag <- function(birthplace, birth_date, field, birth_flag) {
  flags <- character()
  if (birth_flag == "no_birth") {
    flags <- c(flags, "no_birth")
  }
  if (birth_flag == "no_birth_date") {
    flags <- c(flags, "no_birth_date")
  }
  if (birth_flag != "no_birth" && !nzchar(birthplace)) {
    flags <- c(flags, "no_birth_place")
  }
  if (!nzchar(field)) {
    flags <- c(flags, "no_field")
  }
  if (!length(flags)) {
    "ok"
  } else {
    paste(flags, collapse = "_")
  }
}

input <- read_excel(input_file, sheet = "entries", col_types = "text") |>
  as_tibble()

required_cols <- c("batch_id", "lineid", "raw_text")
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input workbook is missing required columns: ",
       paste(missing_cols, collapse = ", "))
}

parsed_rows <- lapply(seq_len(nrow(input)), function(i) {
  raw_text <- normalize_text(input$raw_text[i])
  name_raw <- extract_name_raw(raw_text)
  birth <- extract_birth(raw_text)
  birth_place <- normalize_birth_place(birth$birthplace)
  birth_date <- normalize_birth_date(birth$birth_date)
  field <- normalize_field(extract_field(raw_text, name_raw, birth))
  tibble(
    batch_id = input$batch_id[i],
    batch_lineid = suppressWarnings(as.integer(input$lineid[i])),
    global_lineid = i,
    lineid = i,
    raw_text = raw_text,
    name_raw = name_raw,
    birth_place = birth_place,
    birth_date = birth_date,
    field = field,
    parse_flag = make_parse_flag(
      birth_place,
      birth_date,
      field,
      birth$birth_flag
    )
  )
})

parsed <- bind_rows(parsed_rows)

if (nrow(parsed) != nrow(input)) {
  stop("Parsed row count does not match input row count.")
}
if (any(!nzchar(parsed$raw_text))) {
  stop("Parsed output has empty raw_text values.")
}
if (anyDuplicated(parsed$global_lineid)) {
  stop("global_lineid is not unique.")
}
if (!identical(parsed$lineid, seq_len(nrow(parsed)))) {
  stop("lineid is not sequential 1:n.")
}
if (!"birth_place" %in% names(parsed)) {
  stop("Parsed output is missing birth_place.")
}
if (any(str_detect(parsed$birth_place, regex("\\b111\\b")))) {
  stop("Uncorrected 111 remains in birth_place.")
}

audit <- parsed |>
  filter(parse_flag != "ok")

write_csv(parsed, parsed_file)
write_csv(audit, audit_file)

cat("input workbook:", input_file, "\n")
cat("parsed output:", parsed_file, "\n")
cat("audit flags:", audit_file, "\n")
cat("rows read:", nrow(input), "\n")
cat("rows written:", nrow(parsed), "\n")
cat("empty birth_place:", sum(!nzchar(parsed$birth_place)), "\n")
cat("empty birth_date:", sum(!nzchar(parsed$birth_date)), "\n")
cat("empty field:", sum(!nzchar(parsed$field)), "\n\n")

cat("parse flag distribution:\n")
print(sort(table(parsed$parse_flag), decreasing = TRUE))

cat("\nfirst 20 parsed rows:\n")
print(
  parsed |>
    select(global_lineid, batch_id, batch_lineid, lineid, name_raw, birth_place,
           birth_date, field, parse_flag) |>
    head(20),
  n = 20
)
