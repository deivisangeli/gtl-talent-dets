###############################################################################
# Compare a reusable regex parser against GPT/Codex AMWS consolidated outputs.
#
# Default inputs:
#   <Dropbox root>/output/amws/consolidated_docs/
#     amws16_A_000_200/amws_entries_parsed.csv
#     amws16_A_200_400/amws_entries_parsed.csv
#
# Default outputs:
#   <Dropbox root>/output/amws/regex_vs_gpt_sample/
#     amws_regex_vs_gpt_sample_100.csv
#     amws_regex_vs_gpt_sample_100.xlsx
#     amws_regex_vs_gpt_summary.csv
#     amws_regex_parser_rules.csv
#
# Environment overrides:
#   AMWS_REGEX_VS_GPT_INPUT_FILES  Semicolon-separated consolidated CSV files.
#   AMWS_REGEX_VS_GPT_DOC_IDS      Semicolon-separated doc ids matching inputs.
#   AMWS_REGEX_VS_GPT_OUTPUT_DIR   Output directory.
#   AMWS_REGEX_VS_GPT_SAMPLE_N     Total sample size; default 100.
#   AMWS_REGEX_VS_GPT_SEED         Sampling seed; default 20260630.
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

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_int <- function(name, default) {
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

default_input_files <- file.path(
  DATA_OUTPUT, "amws", "consolidated_docs",
  c("amws16_A_000_200", "amws16_A_200_400"),
  "amws_entries_parsed.csv"
)
input_files <- strsplit(
  env_chr("AMWS_REGEX_VS_GPT_INPUT_FILES",
          paste(default_input_files, collapse = ";")),
  ";",
  fixed = TRUE
)[[1]]
input_files <- normalizePath(input_files, winslash = "/", mustWork = TRUE)

doc_ids <- strsplit(
  env_chr("AMWS_REGEX_VS_GPT_DOC_IDS",
          "amws16_A_000_200;amws16_A_200_400"),
  ";",
  fixed = TRUE
)[[1]]
if (length(doc_ids) != length(input_files)) {
  stop("AMWS_REGEX_VS_GPT_DOC_IDS must match number of input files.")
}

output_dir <- env_chr(
  "AMWS_REGEX_VS_GPT_OUTPUT_DIR",
  file.path(DATA_OUTPUT, "amws", "regex_vs_gpt_sample")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

sample_n_total <- env_int("AMWS_REGEX_VS_GPT_SAMPLE_N", 100L)
sample_seed <- env_int("AMWS_REGEX_VS_GPT_SEED", 20260630L)
sample_per_doc <- floor(sample_n_total / length(input_files))
sample_remainder <- sample_n_total %% length(input_files)

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
  if (!nzchar(field_source)) {
    return("")
  }

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
  if (!safe_field_candidate(field)) {
    return("")
  }
  field
}

extract_previous_edition_field <- function(raw_text, name_raw) {
  marker <- str_locate(
    raw_text,
    regex("\\b(?:see\\s+previous(?:\\s+edition)?|deceased)\\b",
          ignore_case = TRUE)
  )
  if (is.na(marker[1, 1])) {
    return("")
  }
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
    if (nzchar(field)) {
      return(field)
    }
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
    if (nzchar(field)) {
      field_rule_id <- "R_FIELD_BEFORE_SEE_PREVIOUS"
    }
  }

  if (!nzchar(field)) {
    field <- extract_field_from_source(birth$after_birth_date[[1]])
    if (nzchar(field)) {
      field_rule_id <- "R_FIELD_AFTER_DATE"
    }
  }

  if (!nzchar(field)) {
    field <- extract_field_before_educ(raw_text, name_raw)
    if (nzchar(field)) {
      field_rule_id <- "R_FIELD_BEFORE_EDUC"
    }
  }

  parse_flags <- c()
  if (birth$birth_flag[[1]] != "ok") parse_flags <- c(parse_flags, birth$birth_flag[[1]])
  if (!nzchar(field)) parse_flags <- c(parse_flags, "no_field")
  if (!length(parse_flags)) parse_flags <- "ok"

  tibble(
    regex_name_raw = name_raw,
    regex_birth_place = birth$birth_place[[1]],
    regex_birth_date = birth$birth_date[[1]],
    regex_field = field,
    regex_birth_rule_id = birth$birth_rule_id[[1]],
    regex_field_rule_id = field_rule_id,
    regex_parse_flag = paste(parse_flags, collapse = ";")
  )
}

norm_cmp <- function(x, case = FALSE) {
  x <- normalize_text(x)
  x <- str_replace_all(x, "\\s*,\\s*", ", ")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_replace(x, "[.;:,\\s]+$", "")
  if (case) {
    x <- toupper(x)
  }
  x
}

field_match <- function(a, b) norm_cmp(a, case = TRUE) == norm_cmp(b, case = TRUE)
text_match <- function(a, b) norm_cmp(a, case = FALSE) == norm_cmp(b, case = FALSE)
nonempty <- function(x) nzchar(normalize_text(x))

read_doc <- function(path, doc_id) {
  readr::read_csv(path, show_col_types = FALSE) |>
    mutate(
      doc_id = doc_id,
      source_file = path,
      source_lineid = as.integer(lineid),
      across(c(raw_text, birth_place, birth_date, field), ~ normalize_text(.x))
    )
}

all_docs <- bind_rows(Map(read_doc, input_files, doc_ids))
required_cols <- c("doc_id", "source_lineid", "raw_text", "birth_place",
                   "birth_date", "field")
missing_cols <- setdiff(required_cols, names(all_docs))
if (length(missing_cols)) {
  stop("Input consolidated files are missing columns: ",
       paste(missing_cols, collapse = ", "))
}

set.seed(sample_seed)
sampled <- bind_rows(lapply(seq_along(doc_ids), function(i) {
  this_n <- sample_per_doc + ifelse(i <= sample_remainder, 1L, 0L)
  doc_rows <- all_docs |>
    filter(doc_id == doc_ids[[i]])
  doc_rows |>
    slice_sample(n = min(this_n, nrow(doc_rows)))
})) |>
  arrange(doc_id, source_lineid) |>
  mutate(sample_lineid = row_number())

regex_parsed <- bind_rows(lapply(sampled$raw_text, parse_entry_regex))

comparison <- bind_cols(
  sampled |>
    transmute(
      sample_lineid,
      doc_id,
      source_lineid,
      raw_text,
      gpt_birth_place = birth_place,
      gpt_birth_date = birth_date,
      gpt_field = field,
      gpt_confidence = confidence,
      gpt_notes = notes
    ),
  regex_parsed
) |>
  mutate(
    match_birth_place = text_match(regex_birth_place, gpt_birth_place),
    match_birth_date = text_match(regex_birth_date, gpt_birth_date),
    match_field = field_match(regex_field, gpt_field),
    match_all_three = match_birth_place & match_birth_date & match_field,
    regex_all_three_nonempty = nonempty(regex_birth_place) &
      nonempty(regex_birth_date) & nonempty(regex_field),
    gpt_all_three_nonempty = nonempty(gpt_birth_place) &
      nonempty(gpt_birth_date) & nonempty(gpt_field),
    divergence_type = case_when(
      match_all_three ~ "all_three_match",
      !nonempty(regex_birth_place) & nonempty(gpt_birth_place) ~
        "regex_missing_gpt_birth_place",
      nonempty(regex_birth_place) & !nonempty(gpt_birth_place) ~
        "regex_extra_birth_place",
      !nonempty(regex_birth_date) & nonempty(gpt_birth_date) ~
        "regex_missing_gpt_birth_date",
      nonempty(regex_birth_date) & !nonempty(gpt_birth_date) ~
        "regex_extra_birth_date",
      !nonempty(regex_field) & nonempty(gpt_field) ~
        "regex_missing_gpt_field",
      nonempty(regex_field) & !nonempty(gpt_field) ~
        "regex_extra_field",
      TRUE ~ "both_present_different"
    )
  )

metric_row <- function(metric, value) tibble(metric = metric, value = as.character(value))
count_nonempty <- function(x) sum(nonempty(x))
match_rate <- function(x) sprintf("%.1f%%", 100 * mean(x))

summary_tbl <- bind_rows(
  metric_row("sample_seed", sample_seed),
  metric_row("sample_rows", nrow(comparison)),
  metric_row("docs", paste(doc_ids, collapse = ";")),
  metric_row("regex_birth_place_nonempty", count_nonempty(comparison$regex_birth_place)),
  metric_row("gpt_birth_place_nonempty", count_nonempty(comparison$gpt_birth_place)),
  metric_row("regex_birth_date_nonempty", count_nonempty(comparison$regex_birth_date)),
  metric_row("gpt_birth_date_nonempty", count_nonempty(comparison$gpt_birth_date)),
  metric_row("regex_field_nonempty", count_nonempty(comparison$regex_field)),
  metric_row("gpt_field_nonempty", count_nonempty(comparison$gpt_field)),
  metric_row("regex_all_three_nonempty", sum(comparison$regex_all_three_nonempty)),
  metric_row("gpt_all_three_nonempty", sum(comparison$gpt_all_three_nonempty)),
  metric_row("birth_place_matches", sum(comparison$match_birth_place)),
  metric_row("birth_place_match_rate", match_rate(comparison$match_birth_place)),
  metric_row("birth_date_matches", sum(comparison$match_birth_date)),
  metric_row("birth_date_match_rate", match_rate(comparison$match_birth_date)),
  metric_row("field_matches", sum(comparison$match_field)),
  metric_row("field_match_rate", match_rate(comparison$match_field)),
  metric_row("all_three_matches", sum(comparison$match_all_three)),
  metric_row("all_three_match_rate", match_rate(comparison$match_all_three)),
  metric_row("regex_missing_gpt_birth_place",
             sum(!nonempty(comparison$regex_birth_place) & nonempty(comparison$gpt_birth_place))),
  metric_row("regex_missing_gpt_birth_date",
             sum(!nonempty(comparison$regex_birth_date) & nonempty(comparison$gpt_birth_date))),
  metric_row("regex_missing_gpt_field",
             sum(!nonempty(comparison$regex_field) & nonempty(comparison$gpt_field))),
  metric_row("regex_extra_birth_place",
             sum(nonempty(comparison$regex_birth_place) & !nonempty(comparison$gpt_birth_place))),
  metric_row("regex_extra_birth_date",
             sum(nonempty(comparison$regex_birth_date) & !nonempty(comparison$gpt_birth_date))),
  metric_row("regex_extra_field",
             sum(nonempty(comparison$regex_field) & !nonempty(comparison$gpt_field))),
  metric_row(
    "recommendation",
    if (mean(comparison$match_all_three) >= 0.9 &&
        sum(comparison$regex_all_three_nonempty) >= sum(comparison$gpt_all_three_nonempty)) {
      "regex_competitive_on_sample"
    } else {
      "gpt_strategy_preferred_on_sample"
    }
  )
)

divergences <- comparison |>
  filter(!match_all_three) |>
  select(sample_lineid, doc_id, source_lineid, divergence_type,
         raw_text, starts_with("gpt_"), starts_with("regex_"),
         starts_with("match_"))

rules_tbl <- tribble(
  ~rule_id, ~target_field, ~description,
  "R_BIRTH_MARKER_SPACED", "birth_place,birth_date",
  "Lowercase birth marker b followed by whitespace and a capitalized place.",
  "R_BIRTH_MARKER_H_SPACED", "birth_place,birth_date",
  "Narrow OCR h-as-b marker only after punctuation, avoiding initials such as H M.",
  "R_BIRTH_MARKER_GLUED", "birth_place,birth_date",
  "Lowercase birth marker b glued to a capitalized place after punctuation, avoiding surname initials.",
  "R_BIRTH_MARKER_UNDERSCORE", "birth_place,birth_date",
  "Lowercase OCR variant b_ before a capitalized place.",
  "R_DATE_FLEX_MONTH", "birth_date",
  "Month name or abbreviation followed by day and optional year, with narrow OCR cleanup.",
  "R_DATE_OCR_NOVEMBER", "birth_date",
  "Narrow OCR recovery for corrupted November strings.",
  "R_FIELD_AFTER_DATE", "field",
  "Field after birth date and demographic fragments, stopping before AMWS section markers.",
  "R_FIELD_BEFORE_EDUC", "field",
  "Field before Educ marker, including entries that start directly with the field.",
  "R_FIELD_BEFORE_SEE_PREVIOUS", "field",
  "Field before see previous/deceased marker; birth fields intentionally blank."
)

csv_file <- file.path(output_dir, "amws_regex_vs_gpt_sample_100.csv")
xlsx_file <- file.path(output_dir, "amws_regex_vs_gpt_sample_100.xlsx")
summary_file <- file.path(output_dir, "amws_regex_vs_gpt_summary.csv")
rules_file <- file.path(output_dir, "amws_regex_parser_rules.csv")

readr::write_excel_csv(comparison, csv_file, na = "")
readr::write_excel_csv(summary_tbl, summary_file, na = "")
readr::write_excel_csv(rules_tbl, rules_file, na = "")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "sample_comparison", gridLines = FALSE)
openxlsx::addWorksheet(wb, "summary", gridLines = FALSE)
openxlsx::addWorksheet(wb, "regex_rules", gridLines = FALSE)
openxlsx::addWorksheet(wb, "divergences", gridLines = FALSE)

openxlsx::writeDataTable(wb, "sample_comparison", comparison,
                         tableName = "sample_comparison")
openxlsx::writeDataTable(wb, "summary", summary_tbl,
                         tableName = "summary")
openxlsx::writeDataTable(wb, "regex_rules", rules_tbl,
                         tableName = "regex_rules")
openxlsx::writeDataTable(wb, "divergences", divergences,
                         tableName = "divergences")

for (sheet in names(wb)) {
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = 1:80, widths = "auto")
}
openxlsx::setColWidths(wb, "sample_comparison",
                       cols = which(names(comparison) == "raw_text"),
                       widths = 80)
if (nrow(divergences)) {
  openxlsx::setColWidths(wb, "divergences",
                         cols = which(names(divergences) == "raw_text"),
                         widths = 80)
}
openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)

cat("Wrote sample CSV: ", csv_file, "\n", sep = "")
cat("Wrote sample XLSX: ", xlsx_file, "\n", sep = "")
cat("Wrote summary: ", summary_file, "\n", sep = "")
cat("Wrote regex rules: ", rules_file, "\n", sep = "")
cat("Rows: ", nrow(comparison), "\n", sep = "")
cat("All-three matches: ", sum(comparison$match_all_three), "/",
    nrow(comparison), "\n", sep = "")
cat("Recommendation: ",
    summary_tbl$value[summary_tbl$metric == "recommendation"], "\n", sep = "")
