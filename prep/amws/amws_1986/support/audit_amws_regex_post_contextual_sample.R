###############################################################################
# Audit a random sample from the post-contextual-merge AMWS regex consolidated
# file.
#
# Inputs:
#   <Dropbox root>/output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city.csv
#
# Outputs:
#   <Dropbox root>/output/amws/regex_all_docs/
#     audit_1000_regex_entries_post_contextual.csv
#     audit_1000_regex_entries_post_contextual.xlsx
#     audit_1000_regex_entries_post_contextual_summary.csv
#
# Environment overrides:
#   AMWS_REGEX_AUDIT_INPUT_FILE
#   AMWS_REGEX_AUDIT_OUTPUT_DIR
#   AMWS_REGEX_AUDIT_OUTPUT_STEM
#   AMWS_REGEX_AUDIT_TEXT_COL
#   AMWS_REGEX_AUDIT_SAMPLE_N
#   AMWS_REGEX_AUDIT_SEED
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(writexl)
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

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed)) {
    stop("Environment variable ", name, " must be an integer; got: ", value)
  }
  parsed
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

norm_key <- function(x) {
  normalize_text(x) |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "")
}

normalize_ocr_digits <- function(x) {
  chartr("OoIiLlSsZzBb", "001111552288", x)
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
      value = str_sub(x, loc[1, 1], loc[1, 2]) |> normalize_text(),
      start = loc[1, 1],
      end = loc[1, 2],
      rule_id = "date_flex_month"
    ))
  }

  loc <- str_locate(x, regex(OCR_NOV_RX, ignore_case = TRUE))
  if (!is.na(loc[1, 1])) {
    return(tibble(
      value = str_sub(x, loc[1, 1], loc[1, 2]) |> normalize_text(),
      start = loc[1, 1],
      end = loc[1, 2],
      rule_id = "date_ocr_november"
    ))
  }

  tibble(value = "", start = NA_integer_, end = NA_integer_, rule_id = "")
}

find_birth_marker <- function(raw_text) {
  marker_patterns <- c(
    spaced = "(?:^|[,.;:\\s])b\\s+(?=[A-Z])",
    h_spaced = "(?i)(?:^|[,.;:])\\s*h\\s+(?=[A-Z])",
    glued = "(?:[,.;:'’*])\\s*b_?\\s*(?=[A-Z])",
    underscore = "(?:^|[,.;:\\s])b_+\\s*(?=[A-Z])"
  )
  hits <- bind_rows(lapply(names(marker_patterns), function(rule_id) {
    loc <- str_locate(raw_text, regex(marker_patterns[[rule_id]]))
    if (is.na(loc[1, 1])) return(NULL)
    tibble(rule_id = rule_id, start = loc[1, 1], end = loc[1, 2])
  }))
  if (!nrow(hits)) {
    return(tibble(rule_id = "", start = NA_integer_, end = NA_integer_))
  }
  slice(arrange(hits, start), 1)
}

raw_looks_corrupt <- function(raw_text) {
  raw_text <- normalize_text(raw_text)
  nchar(raw_text) < 25L ||
    str_detect(raw_text, regex("[\\^<>]|[A-Za-z][0-9][A-Za-z]|[0-9][A-Za-z][0-9]"))
}

extract_raw_birth <- function(raw_text) {
  raw_text <- normalize_text(raw_text)
  marker <- find_birth_marker(raw_text)
  if (!nzchar(marker$rule_id[[1]])) {
    return(tibble(
      expected_birth_place = "",
      expected_birth_date = "",
      after_birth_date = "",
      raw_birth_rule = "no_birth_marker"
    ))
  }

  after_birth <- str_sub(raw_text, marker$end[[1]] + 1) |> normalize_text()
  date <- find_first_date(after_birth)
  if (!nzchar(date$value[[1]])) {
    return(tibble(
      expected_birth_place = strip_edge_punct(str_split(after_birth, ";", n = 2)[[1]][1]),
      expected_birth_date = "",
      after_birth_date = after_birth,
      raw_birth_rule = "birth_marker_no_date"
    ))
  }

  tibble(
    expected_birth_place = str_sub(after_birth, 1, date$start[[1]] - 1) |>
      strip_edge_punct(),
    expected_birth_date = date$value[[1]],
    after_birth_date = str_sub(after_birth, date$end[[1]] + 1) |>
      normalize_text(),
    raw_birth_rule = paste(marker$rule_id[[1]], date$rule_id[[1]], sep = "+")
  )
}

clean_field_source <- function(x) {
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
      str_replace(regex("^c(?=[A-Z])"), "")
  }
  strip_edge_punct(x)
}

safe_field <- function(x) {
  x <- normalize_text(x)
  nzchar(x) &&
    nchar(x) <= 140L &&
    !str_detect(x, regex("\\b(Educ|Prof\\s+Exp|Mailing\\s*Add|Univ|PhD|Col|Dept|Assoc|Professor|res)\\b",
                         ignore_case = TRUE)) &&
    !str_detect(x, "[0-9]") &&
    !str_detect(x, "[\\^?\\\\<>]")
}

extract_raw_field <- function(raw_text, after_birth_date) {
  raw_text <- normalize_text(raw_text)
  previous_loc <- str_locate(raw_text, regex("\\b(?:see\\s+previous(?:\\s+edition)?|deceased)\\b",
                                             ignore_case = TRUE))
  if (!is.na(previous_loc[1, 1])) {
    source <- str_sub(raw_text, 1, previous_loc[1, 1] - 1)
    parts <- str_split(source, ",")[[1]] |> str_trim()
    parts <- parts[nzchar(parts)]
    source <- if (length(parts) >= 2L) parts[length(parts)] else source
  } else {
    source <- after_birth_date
  }

  source <- clean_field_source(source)
  if (!nzchar(source)) {
    return(tibble(expected_field = "", raw_field_rule = "no_field_source"))
  }

  stop_loc <- str_locate(
    source,
    regex("\\b(Educ|Prof\\s+Exp|Concurrent\\s+Pos|Honors\\s*&\\s*Awards|Mem|Res|Mailing\\s*Add|Exp)\\b\\s*[:;.!\\-]?",
          ignore_case = TRUE)
  )
  candidate <- if (!is.na(stop_loc[1, 1])) {
    str_sub(source, 1, stop_loc[1, 1] - 1)
  } else {
    str_split(source, "\\.\\s+", n = 2)[[1]][1]
  }
  candidate <- candidate |>
    clean_field_source() |>
    str_to_upper() |>
    strip_edge_punct()

  if (!safe_field(candidate)) {
    return(tibble(expected_field = "", raw_field_rule = "unsafe_field_candidate"))
  }
  tibble(expected_field = candidate, raw_field_rule = "field_before_section")
}

amws_century_year <- function(yy) {
  yy <- as.integer(yy)
  ifelse(yy <= 86L, 1900L + yy, 1800L + yy)
}

parse_year_from_date <- function(birth_date) {
  x <- normalize_text(birth_date)
  if (!nzchar(x)) return(NA_integer_)
  x <- str_replace(x, "[,;:.\\s]+$", "")
  token <- str_match(x, "([0-9OoIiLlSsZzBb]{2,4})[A-Za-z]?$")[, 2]
  if (is.na(token) || !nzchar(token)) return(NA_integer_)
  normalized <- normalize_ocr_digits(token)
  if (!str_detect(normalized, "^[0-9]+$")) return(NA_integer_)

  if (nchar(normalized) == 2L) {
    year <- amws_century_year(normalized)
  } else if (nchar(normalized) == 4L) {
    year <- as.integer(normalized)
    if (!is.na(year) && (year < 1800L || year > 1986L)) {
      compact_day <- suppressWarnings(as.integer(str_sub(normalized, 1, 2)))
      compact_yy <- str_sub(normalized, 3, 4)
      if (!is.na(compact_day) && compact_day >= 1L && compact_day <= 31L) {
        year <- amws_century_year(compact_yy)
      }
    }
  } else if (nchar(normalized) == 3L) {
    compact_day <- suppressWarnings(as.integer(str_sub(normalized, 1, 1)))
    compact_yy <- str_sub(normalized, 2, 3)
    year <- if (!is.na(compact_day) && compact_day >= 1L && compact_day <= 9L) {
      amws_century_year(compact_yy)
    } else {
      NA_integer_
    }
  } else {
    year <- NA_integer_
  }

  if (is.na(year) || year < 1800L || year > 1986L) NA_integer_ else as.integer(year)
}

state_or_region_tokens <- c(
  "Ala", "Alaska", "Ariz", "Ark", "Calif", "Colo", "Conn", "Del",
  "DC", "Fla", "Ga", "Hawaii", "Idaho", "Ill", "111", "Ind", "Iowa",
  "Kans", "Ky", "La", "Maine", "Md", "Mass", "Mich", "Minn", "Miss",
  "Mo", "Mont", "Nebr", "Nev", "NH", "NJ", "NMex", "NY", "NC", "NDak",
  "Ohio", "Okla", "Ore", "Pa", "RI", "SC", "SDak", "Tenn", "Tex",
  "Utah", "Vt", "Va", "Wash", "WVa", "Wis", "Wyo", "Ont", "Que",
  "BC", "NS", "NB", "Alta", "Sask", "Man", "PEI", "PR"
)

broad_place_tokens <- c(
  state_or_region_tokens,
  "Africa", "Argentina", "Austria", "Belgium", "Brazil", "Canada",
  "China", "CZ", "Denmark", "Egypt", "England", "Eng", "France", "Germany",
  "Ger", "Gcr", "Greece", "Hungary", "India", "Iran", "Iraq", "Ireland",
  "Israel", "Italy", "Japan", "Korea", "Lebanon", "Mexico", "Mex",
  "Netherlands", "Norway", "Pakistan", "Palestine", "Poland", "Portugal",
  "Russia", "Scotland", "Spain", "Sweden", "Switzerland", "Syria",
  "Taiwan", "Turkey", "UK", "US", "USA", "USSR", "Wales"
)

clean_city_candidate <- function(x) {
  x <- strip_edge_punct(x)
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_replace(
    x,
    regex(paste0("\\s+(", paste(state_or_region_tokens, collapse = "|"),
                 ")$"), ignore_case = TRUE),
    ""
  )
  strip_edge_punct(x)
}

parse_city_from_place <- function(birth_place) {
  x <- normalize_text(birth_place)
  if (!nzchar(x)) return("")
  x <- str_replace_all(x, "\\s*,\\s*", ", ") |> strip_edge_punct()
  if (str_detect(x, regex(paste0("^(", paste(broad_place_tokens,
                                             collapse = "|"), ")$"),
                           ignore_case = TRUE))) {
    return("")
  }
  if (str_detect(x, ",")) {
    city <- str_split_fixed(x, ",", 2)[, 1]
  } else if (str_detect(x, "\\.\\s*[A-Za-z0-9]{1,15}$")) {
    city <- str_split_fixed(x, "\\.", 2)[, 1]
  } else {
    city <- x
  }
  city <- clean_city_candidate(city)
  if (str_detect(city, regex(paste0("^(", paste(broad_place_tokens,
                                                collapse = "|"), ")$"),
                             ignore_case = TRUE))) {
    return("")
  }
  if (!nzchar(city) || nchar(city) < 3L || nchar(city) > 70L) "" else city
}

compatible_text <- function(actual, expected) {
  actual_key <- norm_key(actual)
  expected_key <- norm_key(expected)
  if (!nzchar(actual_key) && !nzchar(expected_key)) return(TRUE)
  if (!nzchar(actual_key) || !nzchar(expected_key)) return(FALSE)
  actual_key == expected_key ||
    str_detect(actual_key, fixed(expected_key)) ||
    str_detect(expected_key, fixed(actual_key))
}

classify_text_column <- function(actual, expected, raw_text) {
  actual <- normalize_text(actual)
  expected <- normalize_text(expected)
  if (nzchar(expected)) {
    if (!nzchar(actual)) {
      if (raw_looks_corrupt(raw_text)) return("unclear_raw")
      return("incorrect")
    }
    if (compatible_text(actual, expected)) return("correct")
    if (raw_looks_corrupt(raw_text)) return("unclear_raw")
    return("incorrect")
  }
  if (nzchar(actual)) return("unclear_raw")
  if (raw_looks_corrupt(raw_text)) return("unclear_raw")
  "not_present_in_raw"
}

classify_year_column <- function(actual_year, expected_date, raw_text) {
  expected_year <- parse_year_from_date(expected_date)
  actual_year <- suppressWarnings(as.integer(actual_year))
  if (!is.na(expected_year)) {
    if (is.na(actual_year)) return("incorrect")
    if (identical(actual_year, expected_year)) return("correct")
    return("incorrect")
  }
  if (!is.na(actual_year)) return("unclear_raw")
  if (raw_looks_corrupt(raw_text)) return("unclear_raw")
  "not_present_in_raw"
}

ENTRY_START_RX <- paste0(
  "\\b[A-Z][A-Z'’\\- ]{2,45},\\s+",
  "[A-Z][A-Za-z'’().\\- ]{1,85},?\\s+",
  "(?:b\\s+[A-Z]|see\\s+previous(?:\\s+edition)?|deceased\\b)"
)

detect_multi_entry <- function(raw_text) {
  raw_text <- normalize_text(raw_text)
  locs <- str_locate_all(raw_text, regex(ENTRY_START_RX, ignore_case = FALSE))[[1]]
  if (!nrow(locs)) {
    return(tibble(entry_start_count = 0L, audit_multi_entry = "no",
                  audit_multi_entry_evidence = ""))
  }
  extra <- locs[locs[, "start"] > 25L, , drop = FALSE]
  if (!nrow(extra)) {
    return(tibble(entry_start_count = nrow(locs), audit_multi_entry = "no",
                  audit_multi_entry_evidence = ""))
  }
  evidence <- str_sub(raw_text, extra[1, "start"], min(nchar(raw_text),
                                                       extra[1, "start"] + 160L))
  tibble(entry_start_count = nrow(locs), audit_multi_entry = "yes",
         audit_multi_entry_evidence = evidence)
}

audit_one <- function(row, text_col = "raw_text") {
  raw_text <- normalize_text(row[[text_col]][[1]])
  raw_birth <- extract_raw_birth(raw_text)
  raw_field <- extract_raw_field(raw_text, raw_birth$after_birth_date[[1]])
  multi <- detect_multi_entry(raw_text)

  expected_year <- parse_year_from_date(raw_birth$expected_birth_date[[1]])
  expected_city <- parse_city_from_place(raw_birth$expected_birth_place[[1]])

  audit_birth_place <- classify_text_column(
    row$birth_place[[1]], raw_birth$expected_birth_place[[1]], raw_text
  )
  audit_birth_date <- classify_text_column(
    row$birth_date[[1]], raw_birth$expected_birth_date[[1]], raw_text
  )
  audit_birth_year <- classify_year_column(
    row$birth_year[[1]], raw_birth$expected_birth_date[[1]], raw_text
  )
  audit_birth_city <- classify_text_column(
    row$birth_city[[1]], expected_city, raw_text
  )
  audit_field <- classify_text_column(
    row$field[[1]], raw_field$expected_field[[1]], raw_text
  )

  bad_cols <- c(
    if (audit_birth_place == "incorrect") "birth_place",
    if (audit_birth_date == "incorrect") "birth_date",
    if (audit_birth_year == "incorrect") "birth_year",
    if (audit_birth_city == "incorrect") "birth_city",
    if (audit_field == "incorrect") "field"
  )
  audit_entry <- if (length(bad_cols) || multi$audit_multi_entry[[1]] == "yes") {
    "incorrect"
  } else {
    "correct"
  }

  note_parts <- c()
  if (length(bad_cols)) {
    note_parts <- c(note_parts, paste("incorrect:", paste(bad_cols, collapse = ",")))
  }
  if (multi$audit_multi_entry[[1]] == "yes") {
    note_parts <- c(note_parts, paste0(text_col, " contains another entry-start pattern"))
  }
  if (!length(note_parts) && raw_looks_corrupt(raw_text)) {
    note_parts <- paste0(text_col, " is noisy but no recoverable parser error was detected")
  }
  if (!length(note_parts)) note_parts <- "no substantive parsing issue detected"

  bind_cols(
    tibble(
      expected_birth_place = raw_birth$expected_birth_place[[1]],
      expected_birth_date = raw_birth$expected_birth_date[[1]],
      expected_birth_year = expected_year,
      expected_birth_city = expected_city,
      expected_field = raw_field$expected_field[[1]],
      raw_birth_rule = raw_birth$raw_birth_rule[[1]],
      raw_field_rule = raw_field$raw_field_rule[[1]],
      audit_birth_place = audit_birth_place,
      audit_birth_date = audit_birth_date,
      audit_birth_year = audit_birth_year,
      audit_birth_city = audit_birth_city,
      audit_field = audit_field,
      audit_entry = audit_entry
    ),
    multi,
    tibble(audit_note = paste(note_parts, collapse = " | "))
  )
}

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
input_file <- env_chr(
  "AMWS_REGEX_AUDIT_INPUT_FILE",
  file.path(default_output_dir,
            "amws_ed16_entries_regex_parsed_birth_year_city.csv")
)
output_dir <- env_chr("AMWS_REGEX_AUDIT_OUTPUT_DIR", default_output_dir)
output_stem <- env_chr("AMWS_REGEX_AUDIT_OUTPUT_STEM",
                       "audit_1000_regex_entries_post_contextual")
audit_text_col <- env_chr("AMWS_REGEX_AUDIT_TEXT_COL", "raw_text")
sample_n <- env_int("AMWS_REGEX_AUDIT_SAMPLE_N", 1000L)
seed <- env_int("AMWS_REGEX_AUDIT_SEED", 20260705L)

if (basename(output_stem) != output_stem) {
  stop("AMWS_REGEX_AUDIT_OUTPUT_STEM must be a file stem, not a path: ",
       output_stem)
}

input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input <- read_csv(input_file, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c(
  "doc_id", "lineid", "raw_text", "birth_place", "birth_date", "field",
  "birth_year", "birth_city", "regex_parse_flag", "birth_year_parse_flag",
  "birth_city_parse_flag"
)
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input table is missing required columns: ",
       paste(missing_cols, collapse = ", "))
}
if (!audit_text_col %in% names(input)) {
  stop("AMWS_REGEX_AUDIT_TEXT_COL is not present in input: ", audit_text_col)
}

if (sample_n > nrow(input)) sample_n <- nrow(input)
set.seed(seed)
sample_idx <- sample.int(nrow(input), sample_n, replace = FALSE)

sampled <- input[sample_idx, ] |>
  arrange(doc_id, as.integer(lineid)) |>
  mutate(audit_id = row_number(),
         audit_text_col = .env$audit_text_col,
         .before = 1)

audited_bits <- bind_rows(lapply(seq_len(nrow(sampled)), function(i) {
  audit_one(sampled[i, ], audit_text_col)
}))

audited <- bind_cols(sampled, audited_bits) |>
  select(
    audit_id, audit_text_col, doc_id, lineid,
    any_of(c("source_lineid", "entry_instance", "lineid_original_key")),
    raw_text, any_of("raw_text_adjusted"),
    birth_place, birth_date, birth_year, birth_city, field,
    regex_parse_flag, birth_year_parse_flag, birth_city_parse_flag,
    expected_birth_place, expected_birth_date, expected_birth_year,
    expected_birth_city, expected_field,
    audit_birth_place, audit_birth_date, audit_birth_year, audit_birth_city,
    audit_field, audit_entry, audit_multi_entry, entry_start_count,
    audit_multi_entry_evidence, raw_birth_rule, raw_field_rule, audit_note,
    everything()
  )

summary_rows <- bind_rows(
  tibble(metric = "sample_n", value = nrow(audited)),
  tibble(metric = "unique_doc_lineid", value = n_distinct(paste(audited$doc_id,
                                                                audited$lineid))),
  tibble(metric = "substantive_parsing_problem_n",
         value = sum(audited$audit_entry == "incorrect")),
  tibble(metric = "substantive_parsing_problem_pct",
         value = 100 * mean(audited$audit_entry == "incorrect")),
  tibble(metric = "multi_entry_n",
         value = sum(audited$audit_multi_entry == "yes")),
  tibble(metric = "multi_entry_pct",
         value = 100 * mean(audited$audit_multi_entry == "yes")),
  tibble(metric = "birth_place_incorrect_n",
         value = sum(audited$audit_birth_place == "incorrect")),
  tibble(metric = "birth_date_incorrect_n",
         value = sum(audited$audit_birth_date == "incorrect")),
  tibble(metric = "birth_year_incorrect_n",
         value = sum(audited$audit_birth_year == "incorrect")),
  tibble(metric = "birth_city_incorrect_n",
         value = sum(audited$audit_birth_city == "incorrect")),
  tibble(metric = "field_incorrect_n",
         value = sum(audited$audit_field == "incorrect")),
  tibble(metric = "birth_place_present_in_text_incorrect_n",
         value = sum(audited$audit_birth_place == "incorrect")),
  tibble(metric = "birth_date_present_in_text_incorrect_n",
         value = sum(audited$audit_birth_date == "incorrect")),
  tibble(metric = "field_present_in_text_incorrect_n",
         value = sum(audited$audit_field == "incorrect"))
)

by_column <- bind_rows(
  audited |> count(audit_birth_place, name = "n") |>
    transmute(metric = paste0("audit_birth_place:", audit_birth_place),
              value = n),
  audited |> count(audit_birth_date, name = "n") |>
    transmute(metric = paste0("audit_birth_date:", audit_birth_date),
              value = n),
  audited |> count(audit_birth_year, name = "n") |>
    transmute(metric = paste0("audit_birth_year:", audit_birth_year),
              value = n),
  audited |> count(audit_birth_city, name = "n") |>
    transmute(metric = paste0("audit_birth_city:", audit_birth_city),
              value = n),
  audited |> count(audit_field, name = "n") |>
    transmute(metric = paste0("audit_field:", audit_field),
              value = n),
  audited |> count(audit_multi_entry, name = "n") |>
    transmute(metric = paste0("audit_multi_entry:", audit_multi_entry),
              value = n)
)

summary <- bind_rows(summary_rows, by_column) |>
  mutate(value = as.numeric(value))

sample_csv <- file.path(output_dir, paste0(output_stem, ".csv"))
sample_xlsx <- file.path(output_dir, paste0(output_stem, ".xlsx"))
summary_csv <- file.path(output_dir, paste0(output_stem, "_summary.csv"))

readr::write_excel_csv(audited, sample_csv, na = "")
writexl::write_xlsx(list(audit = audited, summary = summary), sample_xlsx)
readr::write_excel_csv(summary, summary_csv, na = "")

cat("Input:", input_file, "\n")
cat("Text column:", audit_text_col, "\n")
cat("Output stem:", output_stem, "\n")
cat("Seed:", seed, "\n")
cat("Rows sampled:", nrow(audited), "\n")
cat("Substantive parsing problems:",
    sum(audited$audit_entry == "incorrect"), "\n")
cat("Multi-entry audited text rows:",
    sum(audited$audit_multi_entry == "yes"), "\n")
cat("Wrote audit CSV:", sample_csv, "\n")
cat("Wrote audit XLSX:", sample_xlsx, "\n")
cat("Wrote summary:", summary_csv, "\n")
