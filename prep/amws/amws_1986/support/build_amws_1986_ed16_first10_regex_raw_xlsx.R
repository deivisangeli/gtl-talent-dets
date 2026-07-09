###############################################################################
# Regex-only raw-entry segmentation for the AMWS edition 16 (1986).
#
# Default input:
#   <Dropbox root>/Data/amws ed 16/16_A_0_200_precleaning.docx
#
# Outputs:
#   <Dropbox root>/output/amws/transcription_runs/
#     amws16_A_0_200_first10_regex_only/
#       amws_entries_raw_regex_only.xlsx
#       amws_entries_raw_combined.xlsx
#       regex_segmentation_audit.csv
#       run_summary.csv
#       comparison_to_agent_raw.csv
#
# Environment overrides:
#   AMWS_REGEX_INPUT_FILE            Full path to a DOCX input.
#   AMWS_REGEX_INPUT_BASENAME        DOCX file name under Data/amws ed 16.
#   AMWS_REGEX_RUN_ID                Output run id under output/amws/transcription_runs.
#   AMWS_REGEX_PAGE_START            Source-page start relative to anchor; default 1.
#   AMWS_REGEX_PAGE_END              Source-page end relative to anchor; default 10.
#   AMWS_REGEX_PROCESS_FULL_DOC      TRUE/FALSE; process all pages after anchor.
#   AMWS_REGEX_START_ANCHOR          Regex for first paragraph to keep.
#   AMWS_REGEX_AUTO_ANCHOR           TRUE/FALSE; fallback to first plausible entry.
#   AMWS_REGEX_COMBINE_WITH_FIRST10  TRUE/FALSE; legacy combine for 16_A_0_200 only.
#
# Scope:
#   Reads DOCX directly and uses only regex rules to build participant-level
#   raw_text rows. It does not parse birth/date/field columns.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tibble)
  library(writexl)
  library(xml2)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..", ".."),
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

env_chr <- function(name, default) {
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

env_bool <- function(name, default) {
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

default_input_basename <- "16_A_0_200_precleaning.docx"
input_basename <- env_chr("AMWS_REGEX_INPUT_BASENAME",
                          default_input_basename)
default_input_file <- file.path(TALENT_DETS_DATA_DIR, "Data", "amws ed 16",
                                input_basename)
input_file <- env_chr("AMWS_REGEX_INPUT_FILE", default_input_file)
input_file <- normalizePath(input_file, winslash = "/", mustWork = FALSE)
input_stem <- tools::file_path_sans_ext(basename(input_file))
default_input <- basename(input_file) == default_input_basename

baseline_run_id <- "amws16_A_0_200_precleaning_first10_rawxlsx_4agents"
baseline_run_dir <- file.path(DATA_OUTPUT, "amws", "transcription_runs",
                              baseline_run_id)
baseline_raw_file <- file.path(baseline_run_dir, "amws_entries_raw_combined.xlsx")

page_start <- env_int("AMWS_REGEX_PAGE_START", 1L)
page_end <- env_int("AMWS_REGEX_PAGE_END", 10L)
process_full_doc <- env_bool("AMWS_REGEX_PROCESS_FULL_DOC", FALSE)
if (process_full_doc) {
  page_end <- .Machine$integer.max
}
if (page_start < 1L || page_end < page_start) {
  stop("Invalid page range: ", page_start, "-", page_end)
}

include_first_next_page <- env_bool(
  "AMWS_REGEX_INCLUDE_FIRST_NEXT_PAGE",
  default_input && page_start == 1L && page_end == 10L
)
skip_first_page_overflow <- env_bool(
  "AMWS_REGEX_SKIP_FIRST_PAGE_OVERFLOW",
  FALSE
)

default_run_id <- if (default_input && page_start == 1L && page_end == 10L) {
  "amws16_A_0_200_first10_regex_only"
} else if (process_full_doc) {
  sprintf("%s_regex_only", input_stem)
} else {
  sprintf("%s_pages%d_%d_regex_only", input_stem, page_start, page_end)
}
run_id <- Sys.getenv("AMWS_REGEX_RUN_ID", unset = default_run_id)
run_dir <- file.path(DATA_OUTPUT, "amws", "transcription_runs", run_id)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

raw_xlsx <- file.path(run_dir, "amws_entries_raw_regex_only.xlsx")
parser_xlsx <- file.path(run_dir, "amws_entries_raw_combined.xlsx")
audit_file <- file.path(run_dir, "regex_segmentation_audit.csv")
duplicate_file <- file.path(run_dir, "regex_exact_duplicate_drops.csv")
summary_file <- file.path(run_dir, "run_summary.csv")
comparison_file <- file.path(run_dir, "comparison_to_agent_raw.csv")

if (!file.exists(input_file)) {
  stop("Input DOCX not found: ", input_file)
}

normalize_text <- function(x) {
  x |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("([[:alnum:]])-\\s+([[:alnum:]])", "\\1\\2") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

MONTH_RX <- paste(
  "January", "February", "March", "April", "September", "October",
  "November", "December", "June", "July", "August", "Sept", "Sep",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct",
  "Nov", "Dec",
  sep = "|"
)

NAME_TOKEN <- "[A-Z0-9*^'\\u2019(). -]"
GIVEN_TOKEN <- "[A-Za-z0-9*^'\\u2019()., -]"
SUFFIX_RX <- "JR|SR|II|III|IV"

ENTRY_START_BIRTH <- paste0(
  "^\\s*",
  "[A-Z]", NAME_TOKEN, "{1,70},\\s+",
  "[A-Z0-9]", GIVEN_TOKEN, "{0,120}",
  "(?:,\\s*(?:", SUFFIX_RX, "))?",
  "[,\\.]?\\s*(?:b|h|b_)\\b"
)

ENTRY_START_SEE_PREVIOUS <- paste0(
  "^\\s*",
  "[A-Z]", NAME_TOKEN, "{1,70},\\s+",
  "[A-Z0-9]", GIVEN_TOKEN, "{0,120}",
  "\\b.*\\b(?:see\\s+previous(?:\\s+edition)?|deceased)\\b"
)

ENTRY_START_NO_BIRTH_FIELD <- paste0(
  "^\\s*",
  "[A-Z]", NAME_TOKEN, "{1,70},\\s+",
  "[A-Z0-9]", GIVEN_TOKEN, "{0,120},?\\s+",
  "[A-Z][A-Za-z ,&*/-]{5,140}[\\.;:]\\s*Educ\\b"
)

ENTRY_START_NO_COMMA_BIRTH <- paste0(
  "^\\s*",
  "[A-Z]", NAME_TOKEN, "{2,80}\\s+",
  "[A-Z]", GIVEN_TOKEN, "{1,80},\\s*(?:b|h)\\b"
)

DAMAGED_WITH_MONTH_AND_EDUC <- paste0(
  "^\\s*.{0,170}\\b(", MONTH_RX, ")\\b.{0,240}(?:\\bEduc\\b|£duc)"
)

DAMAGED_WITH_MONTH <- paste0(
  "^\\s*.{0,170}(?:^|[,.;:'\\u2019*_\\s])(?:b|h|b_)[_\\s]*",
  ".{0,120}\\b(", MONTH_RX, ")\\b"
)

DAMAGED_GLUE_BIRTH_MONTH <- paste0(
  "^\\s*.{0,170}(?:^|[,.;:'\\u2019*_\\s])(?:b|h|b_)",
  "[_\\sA-Za-z.,]{0,90}\\b(", MONTH_RX, ")\\b"
)

DAMAGED_WITH_FIELD_AND_EDUC <- paste0(
  "^\\s*.{0,180}\\b[A-Z][A-Za-z ,&*/-]{5,120}[\\.;:]\\s*(?:Educ\\b|£duc)"
)

DAMAGED_POUND_EDUC <- "^\\s*.{0,240}£duc"

DAMAGED_SEE_PREVIOUS <- "^\\s*.{5,180}\\bsee\\s+previous\\b"

ISOLATED_DAMAGED_NAME <- "^\\s*[A-Z][A-Z]{2,}(?:[ -][A-Z][A-Z]{2,}){0,2}\\s*$"

DASH_PROF_DAMAGED_START <- "^\\s*-\\s*prof\\s+internal\\s+med\\b"

CONTINUATION_START <- paste0(
  "^\\s*(Educ|Prof\\s+Exp|Concurrent\\s+Pos|Honors\\s*&\\s*Awards|",
  "Mem|Res|Mailing\\s*Add|[0-9]{2,};|[-,;:.])\\b"
)

NOISE_RX <- paste0(
  "^\\s*(A|[0-9]+/[A-Z0-9'\\u2019.-]+|",
  "[A-Z0-9'\\u2019.-]+/[0-9]+|cs;\\s*et\\^cl)\\s*$"
)

SAFE_EMBEDDED_PATTERNS <- c(
  paste0("(?<=[0-9]{5})(?=[A-Z][A-Z]+\\s+[A-Z][A-Z]+,\\s*(?:b|h)\\b)"),
  paste0("(?<=[0-9]{5})\\s*(?=.{0,170}\\b(", MONTH_RX, ")\\b)"),
  paste0("\\s*(?=[A-Z][^\\s]{0,110}\\.b[^\\s,;.]+\\.\\s+(", MONTH_RX, ")\\b)"),
  paste0(
    "(?<=[0-9]{5})\\s*(?=.{1,140}\\b(", MONTH_RX,
    ")\\b.{0,260}(?:\\bEduc\\b|£duc))"
  ),
  paste0(
    "\\s+(?=",
    "(?![A-Z][A-Z. ]{0,25},\\s*(?:", SUFFIX_RX, ")\\s*,?\\s*(?:b|h)\\b)",
    "[A-Z][A-Z0-9'\\u2019().* -]{1,55},\\s+",
    "[A-Z0-9][A-Za-z0-9'\\u2019()., *-]{0,120}",
    "(?:[,\\.]\\s*(?:b|h)\\b|\\bsee\\s+previous(?:\\s+edition)?\\b|\\bdeceased\\b)",
    ")"
  )
)

read_docx_paragraphs <- function(path) {
  tmp <- tempfile("amws16_regex_docx_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  unzip(path, files = "word/document.xml", exdir = tmp)
  doc <- read_xml(file.path(tmp, "word", "document.xml"))
  ns <- xml_ns(doc)
  paragraphs <- xml_find_all(doc, ".//w:p", ns)

  paragraph_text <- function(p) {
    paste(xml_text(xml_find_all(p, ".//w:t", ns)), collapse = "")
  }
  has_page_break <- function(p) {
    length(xml_find_all(
      p,
      ".//w:lastRenderedPageBreak | .//w:br[@w:type='page']",
      ns
    )) > 0
  }

  tibble(
    paragraph_id = seq_along(paragraphs),
    detected_page = cumsum(vapply(paragraphs, has_page_break, logical(1))) + 1L,
    text = vapply(paragraphs, paragraph_text, character(1))
  ) |>
    mutate(text = normalize_text(text)) |>
    filter(nzchar(text))
}

split_safe_embedded_entries <- function(x) {
  marked <- x
  for (pat in SAFE_EMBEDDED_PATTERNS) {
    marked <- str_replace_all(marked, regex(pat), "\n<<<ENTRY>>>")
  }
  pieces <- unlist(str_split(marked, fixed("<<<ENTRY>>>")), use.names = FALSE)
  pieces <- normalize_text(pieces)
  pieces[nzchar(pieces)]
}

repair_piece_sequence <- function(pieces) {
  if (nrow(pieces) == 0L) {
    return(pieces)
  }

  out <- list()
  i <- 1L
  while (i <= nrow(pieces)) {
    if (i < nrow(pieces) &&
        str_detect(pieces$text[i], regex("^\\*\\s*no$", ignore_case = TRUE)) &&
        str_detect(pieces$text[i + 1L], regex(ENTRY_START_BIRTH))) {
      out[[length(out) + 1L]] <- pieces[i + 1L, ] |>
        mutate(
          source_piece = pieces$source_piece[i],
          was_embedded_split =
            was_embedded_split | pieces$was_embedded_split[i],
          text = normalize_text(paste(pieces$text[i], pieces$text[i + 1L]))
        )
      i <- i + 2L
      next
    }

    if (i + 2L <= nrow(pieces) &&
        str_detect(pieces$text[i], regex("^[A-Z][A-Z]+,\\s*$")) &&
        str_detect(pieces$text[i + 1L],
                   regex("^[A-Z]\\([A-Z]+\\)\\s*$")) &&
        str_detect(pieces$text[i + 2L],
                   regex(paste0(
                     "^[A-Z]\\([A-Z]+\\),\\s*(?:", SUFFIX_RX,
                     ")\\s*,?\\s*(?:b|h)\\b"
                   )))) {
      out[[length(out) + 1L]] <- pieces[i, ] |>
        mutate(
          source_piece = pieces$source_piece[i],
          was_embedded_split =
            was_embedded_split |
            pieces$was_embedded_split[i + 1L] |
            pieces$was_embedded_split[i + 2L],
          text = normalize_text(paste(
            pieces$text[i],
            pieces$text[i + 1L],
            pieces$text[i + 2L]
          ))
        )
      i <- i + 3L
      next
    }

    out[[length(out) + 1L]] <- pieces[i, ]
    i <- i + 1L
  }

  bind_rows(out) |>
    mutate(piece_id = row_number(), .before = paragraph_id)
}

find_start_paragraph <- function(paragraphs) {
  explicit_anchor <- Sys.getenv("AMWS_REGEX_START_ANCHOR", unset = "")
  auto_anchor <- env_bool("AMWS_REGEX_AUTO_ANCHOR", !default_input)

  if (nzchar(explicit_anchor)) {
    first_entry <- which(str_detect(paragraphs$text, regex(explicit_anchor)))[1]
    if (!is.na(first_entry)) {
      return(list(index = first_entry, rule = "explicit_anchor",
                  pattern = explicit_anchor))
    }
    if (!auto_anchor) {
      stop("Could not find explicit start anchor: ", explicit_anchor)
    }
    warning("Explicit start anchor not found; falling back to auto anchor: ",
            explicit_anchor)
  }

  if (default_input && !auto_anchor) {
    first_entry <- which(str_detect(paragraphs$text, regex("^AABOE,\\s")))[1]
    if (is.na(first_entry)) {
      stop("Could not find pilot start entry: AABOE")
    }
    return(list(index = first_entry, rule = "legacy_aaboe_anchor",
                pattern = "^AABOE,\\s"))
  }

  strong_patterns <- c(
    ENTRY_START_BIRTH,
    ENTRY_START_NO_COMMA_BIRTH,
    ENTRY_START_SEE_PREVIOUS,
    ENTRY_START_NO_BIRTH_FIELD,
    DAMAGED_GLUE_BIRTH_MONTH,
    DAMAGED_WITH_MONTH_AND_EDUC
  )
  matches <- lapply(seq_along(strong_patterns), function(i) {
    hit <- which(str_detect(paragraphs$text,
                            regex(strong_patterns[[i]],
                                  ignore_case = TRUE)))[1]
    if (is.na(hit)) {
      return(NULL)
    }
    tibble(pattern_id = i, index = hit, pattern = strong_patterns[[i]])
  }) |>
    bind_rows()
  if (nrow(matches)) {
    match <- matches |>
      arrange(index, pattern_id) |>
      slice(1)
    return(list(index = match$index[[1]], rule = "auto_anchor",
                pattern = match$pattern[[1]]))
  }

  stop("Could not find an automatic start entry anchor in DOCX: ", input_file)
}

build_page_pieces <- function(paragraphs, page_start, page_end,
                              include_first_next_page = FALSE,
                              skip_first_page_overflow = FALSE) {
  start_anchor <- find_start_paragraph(paragraphs)
  first_entry <- start_anchor$index

  base_page <- paragraphs$detected_page[first_entry]
  page_rows <- paragraphs |>
    slice(first_entry:n()) |>
    mutate(
      source_page = detected_page - base_page + 1L,
      start_anchor_rule = start_anchor$rule,
      start_anchor_pattern = start_anchor$pattern,
      start_anchor_paragraph_id = paragraphs$paragraph_id[first_entry]
    )

  if (skip_first_page_overflow) {
    first_page_paragraph <- min(
      page_rows$paragraph_id[page_rows$source_page == page_start],
      na.rm = TRUE
    )
    if (is.finite(first_page_paragraph)) {
      page_rows <- page_rows |>
        filter(!(source_page == page_start &
                   paragraph_id == first_page_paragraph))
    }
  }

  if (include_first_next_page) {
    first_next_paragraph <- min(
      page_rows$paragraph_id[page_rows$source_page == page_end + 1L],
      na.rm = TRUE
    )
    page_rows <- page_rows |>
      filter(
        source_page >= page_start,
        source_page <= page_end |
          (is.finite(first_next_paragraph) &
             source_page == page_end + 1L &
             paragraph_id == first_next_paragraph)
      )
  } else {
    page_rows <- page_rows |>
      filter(source_page >= page_start, source_page <= page_end)
  }

  if (nrow(page_rows) == 0L) {
    stop("No DOCX text found for page range ", page_start, "-", page_end)
  }

  pieces <- bind_rows(lapply(seq_len(nrow(page_rows)), function(i) {
    pieces <- split_safe_embedded_entries(page_rows$text[i])
    tibble(
      paragraph_id = page_rows$paragraph_id[i],
      source_page = page_rows$source_page[i],
      source_piece = seq_along(pieces),
      was_embedded_split = seq_along(pieces) > 1L,
      start_anchor_rule = page_rows$start_anchor_rule[i],
      start_anchor_pattern = page_rows$start_anchor_pattern[i],
      start_anchor_paragraph_id = page_rows$start_anchor_paragraph_id[i],
      text = pieces
    )
  }))

  repair_piece_sequence(pieces)
}

classify_piece <- function(text) {
  text <- normalize_text(text)

  if (str_detect(text, regex(NOISE_RX))) {
    return(tibble(rule = "noise", starts_entry = FALSE))
  }
  if (str_detect(text, regex(ENTRY_START_BIRTH))) {
    suffix <- str_detect(
      text,
      regex(paste0(",\\s*(?:", SUFFIX_RX, ")\\s*,?\\s*(?:b|h)\\b"))
    )
    return(tibble(
      rule = ifelse(suffix, "suffix_birth_start", "strong_birth_start"),
      starts_entry = TRUE
    ))
  }
  if (str_detect(text, regex(ENTRY_START_SEE_PREVIOUS, ignore_case = TRUE))) {
    return(tibble(rule = "see_previous_start", starts_entry = TRUE))
  }
  if (str_detect(text, regex(ENTRY_START_NO_COMMA_BIRTH))) {
    return(tibble(rule = "no_comma_birth_start", starts_entry = TRUE))
  }
  if (str_detect(text, regex(ENTRY_START_NO_BIRTH_FIELD))) {
    return(tibble(rule = "no_birth_field_start", starts_entry = TRUE))
  }
  if (!str_detect(text, regex(CONTINUATION_START, ignore_case = TRUE)) &&
      str_detect(text, regex(DAMAGED_SEE_PREVIOUS, ignore_case = TRUE))) {
    return(tibble(rule = "damaged_see_previous_start", starts_entry = TRUE))
  }
  if (!str_detect(text, regex(CONTINUATION_START, ignore_case = TRUE)) &&
      str_detect(text, regex(DAMAGED_WITH_MONTH_AND_EDUC, ignore_case = TRUE))) {
    return(tibble(rule = "damaged_month_educ_start", starts_entry = TRUE))
  }
  if (!str_detect(text, regex(CONTINUATION_START, ignore_case = TRUE)) &&
      str_detect(text, regex(DAMAGED_WITH_MONTH, ignore_case = TRUE))) {
    return(tibble(rule = "damaged_month_start", starts_entry = TRUE))
  }
  if (!str_detect(text, regex(CONTINUATION_START, ignore_case = TRUE)) &&
      str_detect(text, regex(DAMAGED_GLUE_BIRTH_MONTH, ignore_case = TRUE))) {
    return(tibble(rule = "damaged_glue_birth_month_start", starts_entry = TRUE))
  }
  if (!str_detect(text, regex(CONTINUATION_START, ignore_case = TRUE)) &&
      str_detect(text, regex(DAMAGED_WITH_FIELD_AND_EDUC))) {
    return(tibble(rule = "damaged_field_educ_start", starts_entry = TRUE))
  }
  if (!str_detect(text, regex(CONTINUATION_START, ignore_case = TRUE)) &&
      str_detect(text, regex(DAMAGED_POUND_EDUC))) {
    return(tibble(rule = "damaged_pound_educ_start", starts_entry = TRUE))
  }
  if (str_detect(text, regex(DASH_PROF_DAMAGED_START, ignore_case = TRUE))) {
    return(tibble(rule = "dash_prof_damaged_start", starts_entry = TRUE))
  }
  if (str_detect(text, regex(ISOLATED_DAMAGED_NAME))) {
    return(tibble(rule = "isolated_damaged_name_start", starts_entry = TRUE))
  }
  if (str_detect(text, regex(CONTINUATION_START, ignore_case = TRUE))) {
    return(tibble(rule = "continued_previous", starts_entry = FALSE))
  }

  tibble(rule = "continued_previous", starts_entry = FALSE)
}

is_incomplete_birth_stub <- function(current) {
  if (is.null(current)) {
    return(FALSE)
  }

  text <- normalize_text(paste(current$text, collapse = " "))
  if (!nzchar(text) || nchar(text) > 260L) {
    return(FALSE)
  }

  has_birth_signal <-
    str_detect(text, regex(ENTRY_START_BIRTH)) ||
    str_detect(text, regex(ENTRY_START_NO_COMMA_BIRTH)) ||
    str_detect(text, regex(DAMAGED_WITH_MONTH, ignore_case = TRUE)) ||
    str_detect(text, regex(DAMAGED_GLUE_BIRTH_MONTH, ignore_case = TRUE))

  has_later_section <- str_detect(
    text,
    regex(
      "\\b(Educ|Prof\\s+Exp|Concurrent\\s+Pos|Honors\\s*&\\s*Awards|Mem|Res|Mailing\\s*Add)\\b|£duc",
      ignore_case = TRUE
    )
  )

  has_birth_signal && !has_later_section
}

is_field_educ_continuation <- function(row) {
  row$rule %in% c("no_birth_field_start", "damaged_field_educ_start") &&
    !str_detect(row$text, regex(ENTRY_START_BIRTH)) &&
    !str_detect(row$text, regex(ENTRY_START_NO_COMMA_BIRTH))
}

segment_entries <- function(pieces) {
  classified <- bind_cols(
    pieces,
    bind_rows(lapply(pieces$text, classify_piece))
  )

  entries <- list()
  orphan_rows <- list()
  contextual_merges <- list()
  current <- NULL

  flush_current <- function(current) {
    if (is.null(current)) {
      return(NULL)
    }
    raw_text <- normalize_text(paste(current$text, collapse = " "))
    if (!nzchar(raw_text)) {
      return(NULL)
    }
    tibble(
      source_page_start = min(current$source_page),
      source_page_end = max(current$source_page),
      paragraph_ids = paste(current$paragraph_id, collapse = ";"),
      source_pieces = paste(current$source_piece, collapse = ";"),
      boundary_flag = paste(unique(current$rule), collapse = ";"),
      start_rule = current$rule[[1]],
      raw_text = raw_text,
      dropped = FALSE
    )
  }

  for (i in seq_len(nrow(classified))) {
    row <- classified[i, ]

    if (row$rule == "noise") {
      orphan_rows[[length(orphan_rows) + 1L]] <- tibble(
        source_page_start = row$source_page,
        source_page_end = row$source_page,
        paragraph_ids = as.character(row$paragraph_id),
        source_pieces = as.character(row$source_piece),
        boundary_flag = "orphan_noise",
        start_rule = "noise",
        raw_text = row$text,
        dropped = TRUE
      )
      next
    }

    if (row$starts_entry) {
      if (is_incomplete_birth_stub(current) &&
          is_field_educ_continuation(row)) {
        current_text_before <- normalize_text(paste(current$text, collapse = " "))
        contextual_merges[[length(contextual_merges) + 1L]] <- tibble(
          previous_paragraph_ids = paste(current$paragraph_id, collapse = ";"),
          continuation_paragraph_id = as.character(row$paragraph_id),
          previous_source_pieces = paste(current$source_piece, collapse = ";"),
          continuation_source_piece = as.character(row$source_piece),
          previous_start_rule = current$rule[[1]],
          continuation_original_rule = row$rule,
          merge_rule = "field_educ_continuation_after_birth_stub",
          previous_text = current_text_before,
          continuation_text = row$text
        )
        current <- bind_rows(
          current,
          tibble(
            source_page = row$source_page,
            paragraph_id = row$paragraph_id,
            source_piece = row$source_piece,
            rule = "field_educ_continuation_after_birth_stub",
            text = row$text
          )
        )
        next
      }

      flushed <- flush_current(current)
      if (!is.null(flushed)) {
        entries[[length(entries) + 1L]] <- flushed
      }
      current <- tibble(
        source_page = row$source_page,
        paragraph_id = row$paragraph_id,
        source_piece = row$source_piece,
        rule = row$rule,
        text = row$text
      )
      next
    }

    if (is.null(current)) {
      orphan_rows[[length(orphan_rows) + 1L]] <- tibble(
        source_page_start = row$source_page,
        source_page_end = row$source_page,
        paragraph_ids = as.character(row$paragraph_id),
        source_pieces = as.character(row$source_piece),
        boundary_flag = "orphan_continuation",
        start_rule = row$rule,
        raw_text = row$text,
        dropped = TRUE
      )
      next
    }

    current <- bind_rows(
      current,
      tibble(
        source_page = row$source_page,
        paragraph_id = row$paragraph_id,
        source_piece = row$source_piece,
        rule = row$rule,
        text = row$text
      )
    )
  }

  flushed <- flush_current(current)
  if (!is.null(flushed)) {
    entries[[length(entries) + 1L]] <- flushed
  }

  kept <- if (length(entries)) bind_rows(entries) else {
    tibble(
      source_page_start = integer(),
      source_page_end = integer(),
      paragraph_ids = character(),
      source_pieces = character(),
      boundary_flag = character(),
      start_rule = character(),
      raw_text = character(),
      dropped = logical()
    )
  }

  dropped <- if (length(orphan_rows)) bind_rows(orphan_rows) else kept[0, ]

  kept <- kept |>
    mutate(lineid = row_number(), .before = source_page_start) |>
    select(lineid, source_page_start, source_page_end, paragraph_ids,
           source_pieces, boundary_flag, start_rule, raw_text, dropped)

  dropped <- dropped |>
    mutate(lineid = NA_integer_, .before = source_page_start) |>
    select(lineid, source_page_start, source_page_end, paragraph_ids,
           source_pieces, boundary_flag, start_rule, raw_text, dropped)

  list(
    entries = kept,
    audit = bind_rows(kept, dropped),
    candidates = classified,
    contextual_merges = if (length(contextual_merges)) {
      bind_rows(contextual_merges)
    } else {
      tibble(
        previous_paragraph_ids = character(),
        continuation_paragraph_id = character(),
        previous_source_pieces = character(),
        continuation_source_piece = character(),
        previous_start_rule = character(),
        continuation_original_rule = character(),
        merge_rule = character(),
        previous_text = character(),
        continuation_text = character()
      )
    }
  )
}

write_entries_workbook <- function(entries, path) {
  write_xlsx(list(entries = entries), path = path)
}

combine_with_first10_run <- function(current_entries) {
  combine_legacy <- env_bool(
    "AMWS_REGEX_COMBINE_WITH_FIRST10",
    default_input && page_start > 1L
  )
  if (!combine_legacy) {
    return(NULL)
  }
  if (page_start <= 1L) {
    return(NULL)
  }

  first10_run_id <- "amws16_A_0_200_first10_regex_only"
  first10_file <- file.path(
    DATA_OUTPUT, "amws", "transcription_runs", first10_run_id,
    "amws_entries_raw_regex_only.xlsx"
  )
  if (!file.exists(first10_file)) {
    warning("First-10 regex workbook not found; skipping full DOCX combine: ",
            first10_file)
    return(NULL)
  }

  combined_run_id <- "amws16_A_0_200_regex_only_pages1_191"
  current_run_id <- run_id
  combined_run_dir <- file.path(
    DATA_OUTPUT, "amws", "transcription_runs", combined_run_id
  )
  dir.create(combined_run_dir, recursive = TRUE, showWarnings = FALSE)

  first10_entries <- read_excel(first10_file, sheet = "entries",
                                col_types = "text") |>
    as_tibble() |>
    transmute(batch_id = "regex_pages_1_10", raw_text = raw_text)

  current_batch_id <- sprintf("regex_pages_%d_%d", page_start, page_end)
  current_entries <- current_entries |>
    transmute(batch_id = current_batch_id, raw_text = raw_text)

  combined <- bind_rows(first10_entries, current_entries) |>
    mutate(lineid = row_number(), .after = batch_id) |>
    select(batch_id, lineid, raw_text)

  combined_file <- file.path(combined_run_dir, "amws_entries_raw_combined.xlsx")
  write_entries_workbook(combined, combined_file)

  combined_summary <- tibble(
    run_id = combined_run_id,
    source_file = input_file,
    first_run = first10_run_id,
    second_run = current_run_id,
    total_raw_entries = nrow(combined),
    first_run_entries = nrow(first10_entries),
    second_run_entries = nrow(current_entries),
    status = "complete",
    notes = "Combined regex-only raw_text workbook for source pages 1-191."
  )
  write_csv(combined_summary,
            file.path(combined_run_dir, "run_summary.csv"))

  combined_file
}

compare_to_baseline <- function(entries) {
  if (!file.exists(baseline_raw_file)) {
    return(tibble(
      metric = c("baseline_found", "regex_rows"),
      value = c("FALSE", as.character(nrow(entries)))
    ))
  }

  baseline <- read_excel(baseline_raw_file, sheet = "entries",
                         col_types = "text") |>
    as_tibble() |>
    mutate(raw_text_norm = normalize_text(raw_text))

  regex <- entries |>
    mutate(raw_text_norm = normalize_text(raw_text))

  baseline_blob <- paste(regex$raw_text_norm, collapse = "\n")
  baseline_name_key <- function(x) {
    m <- str_match(
      x,
      regex(paste0(
        "^(.{5,180}?)(?:[,\\.]\\s+(?:b|h)\\b|",
        "\\bsee\\s+previous|\\bdeceased\\b|",
        "\\.\\s+[A-Z][A-Z ,&-]{4,80}\\b)"
      ), ignore_case = TRUE)
    )[, 2]
    ifelse(is.na(m), str_sub(x, 1, 80), normalize_text(m))
  }
  baseline_keys <- baseline_name_key(baseline$raw_text_norm)
  baseline_keys_visible <- vapply(
    baseline_keys,
    function(key) str_detect(baseline_blob, fixed(key)),
    logical(1)
  )

  tibble(
    metric = c(
      "baseline_found",
      "baseline_rows",
      "regex_rows",
      "regex_exact_in_baseline",
      "baseline_exact_in_regex",
      "baseline_keys_visible_in_regex"
    ),
    value = c(
      "TRUE",
      as.character(nrow(baseline)),
      as.character(nrow(regex)),
      as.character(sum(regex$raw_text_norm %in% baseline$raw_text_norm)),
      as.character(sum(baseline$raw_text_norm %in% regex$raw_text_norm)),
      as.character(sum(baseline_keys_visible))
    )
  )
}

paragraphs <- read_docx_paragraphs(input_file)
pieces <- build_page_pieces(
  paragraphs = paragraphs,
  page_start = page_start,
  page_end = page_end,
  include_first_next_page = include_first_next_page,
  skip_first_page_overflow = skip_first_page_overflow
)
segmented <- segment_entries(pieces)

entries_audit_raw <- segmented$entries
entries_with_duplicate_flags <- entries_audit_raw |>
  mutate(
    original_lineid = lineid,
    raw_text_norm = normalize_text(raw_text),
    exact_duplicate = duplicated(raw_text_norm)
  )
duplicate_drops <- entries_with_duplicate_flags |>
  filter(exact_duplicate) |>
  select(original_lineid, source_page_start, source_page_end, paragraph_ids,
         source_pieces, boundary_flag, start_rule, raw_text)
entries_audit <- entries_with_duplicate_flags |>
  filter(!exact_duplicate) |>
  select(-lineid, -raw_text_norm, -exact_duplicate) |>
  mutate(lineid = row_number(), .before = source_page_start) |>
  select(lineid, original_lineid, source_page_start, source_page_end,
         paragraph_ids, source_pieces, boundary_flag, start_rule, raw_text,
         dropped)
entries <- entries_audit |>
  transmute(lineid = lineid, raw_text = raw_text)

parser_entries <- entries |>
  mutate(batch_id = "regex_only", .before = lineid)

audit <- segmented$audit
candidates <- segmented$candidates |>
  select(piece_id, paragraph_id, source_page, source_piece,
         was_embedded_split, rule, starts_entry, text)
contextual_merges <- segmented$contextual_merges
comparison <- if (default_input && page_start == 1L && page_end == 10L) {
  compare_to_baseline(entries)
} else {
  tibble(
    metric = c("baseline_found", "baseline_applicable", "regex_rows"),
    value = c(
      as.character(file.exists(baseline_raw_file)),
      "FALSE",
      as.character(nrow(entries))
    )
  )
}

if (nrow(entries) == 0L) {
  stop("Regex segmentation produced no entries.")
}
if (!identical(entries$lineid, seq_len(nrow(entries)))) {
  stop("lineid is not sequential 1:n.")
}
if (any(!nzchar(entries$raw_text))) {
  stop("Output contains empty raw_text values.")
}

write_entries_workbook(entries, raw_xlsx)
write_entries_workbook(parser_entries, parser_xlsx)
write_csv(audit, audit_file)
write_csv(duplicate_drops, duplicate_file)
write_csv(comparison, comparison_file)
write_csv(candidates, file.path(run_dir, "regex_start_candidates.csv"))
write_csv(contextual_merges, file.path(run_dir, "regex_contextual_merges.csv"))
combined_full_file <- combine_with_first10_run(entries)
source_pages_processed <- if (nrow(pieces)) {
  sprintf("%d-%d relative to anchor", min(pieces$source_page),
          max(pieces$source_page))
} else {
  ""
}
pages_requested_label <- if (process_full_doc) {
  sprintf("%d-end relative to anchor", page_start)
} else {
  sprintf("%d-%d relative to anchor", page_start, page_end)
}
anchor_rule <- if ("start_anchor_rule" %in% names(pieces)) {
  pieces$start_anchor_rule[[1]]
} else {
  ""
}
anchor_paragraph_id <- if ("start_anchor_paragraph_id" %in% names(pieces)) {
  pieces$start_anchor_paragraph_id[[1]]
} else {
  NA_integer_
}

summary <- tibble(
  run_id = run_id,
  source_file = input_file,
  source_mtime_after = file.info(input_file)$mtime,
  source_size_after = file.info(input_file)$size,
  pages_requested = pages_requested_label,
  source_pages_processed = source_pages_processed,
  page_basis = "word_detected_page_break_relative_to_anchor",
  start_anchor_rule = anchor_rule,
  start_anchor_paragraph_id = anchor_paragraph_id,
  include_first_next_page = include_first_next_page,
  skip_first_page_overflow = skip_first_page_overflow,
  paragraphs_read = nrow(paragraphs),
  pilot_pieces = nrow(pieces),
  start_candidates = sum(candidates$starts_entry),
  raw_entries_before_exact_dedup = nrow(entries_audit_raw),
  exact_duplicate_drops = nrow(duplicate_drops),
  contextual_field_educ_merges = nrow(contextual_merges),
  raw_entries = nrow(entries),
  dropped_rows = sum(audit$dropped),
  suffix_start_entries =
    sum(entries_audit$start_rule == "suffix_birth_start", na.rm = TRUE),
  damaged_start_entries =
    sum(str_detect(entries_audit$start_rule, "^damaged"), na.rm = TRUE),
  status = "complete",
  combined_full_file = ifelse(is.null(combined_full_file), "",
                              combined_full_file),
  notes = paste(
    "Regex-only two-pass segmentation from DOCX;",
    "baseline is used only for first-10 validation, not for output construction."
  )
)
write_csv(summary, summary_file)

cat("input:", input_file, "\n")
cat("run_dir:", run_dir, "\n")
cat("raw_xlsx:", raw_xlsx, "\n")
cat("parser_xlsx:", parser_xlsx, "\n")
cat("audit:", audit_file, "\n")
cat("summary:", summary_file, "\n")
cat("comparison:", comparison_file, "\n")
cat("paragraphs read:", nrow(paragraphs), "\n")
cat("page pieces:", nrow(pieces), "\n")
cat("start candidates:", sum(candidates$starts_entry), "\n")
cat("entries:", nrow(entries), "\n")
cat("dropped rows:", sum(audit$dropped), "\n")
if (!is.null(combined_full_file)) {
  cat("combined_full_file:", combined_full_file, "\n")
}
print(comparison)
