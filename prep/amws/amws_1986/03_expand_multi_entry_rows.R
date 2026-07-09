###############################################################################
# Expand residual multi-entry rows in the AMWS edition 16 regex consolidated
# output.
#
# This is a post-processing step. It does not overwrite the canonical
# consolidated file. It detects additional biographical markers inside raw_text,
# creates one extra row per high-confidence additional entry, replicates the
# original raw_text, and adds raw_text_adjusted starting at the additional
# marker for the new row.
#
# Input:
#   <Dropbox root>/output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city.csv
#
# Outputs:
#   <Dropbox root>/output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded.csv
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded.xlsx
#     amws_ed16_multi_entry_expansion_audit.csv
#     amws_ed16_multi_entry_expansion_audit.xlsx
#
# Environment overrides:
#   AMWS_REGEX_MULTI_INPUT_FILE
#   AMWS_REGEX_MULTI_OUTPUT_DIR
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
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."),
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
  "Jan", "Feb", "Mar", "Apr", "May", "Mai", "Jun", "Jul", "Aug", "Oct",
  "Nov", "Dec",
  sep = "|"
)

DATE_RX <- paste0(
  "\\b(", MONTH_RX, ")\\.?\\s*,?\\s+",
  "[0-9OISZLB|]{1,2}",
  "(?:\\s*[,.'’`-]?\\s*[0-9OISZLB|]{1,4}[A-Za-z]?)?\\b"
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

normalize_ocr_digits <- function(x) {
  chartr("OoIiLlSsZzBb|", "0011115522881", x)
}

amws_century_year <- function(yy) {
  yy <- as.integer(yy)
  ifelse(yy <= 86L, 1900L + yy, 1800L + yy)
}

parse_birth_year_one <- function(birth_date) {
  x <- normalize_text(birth_date)
  if (!nzchar(x)) {
    return(tibble(birth_year = NA_integer_,
                  birth_year_parse_flag = "no_birth_date"))
  }

  x <- str_replace(x, "[,;:.\\s]+$", "")
  year_token <- str_match(x, "([0-9OoIiLlSsZzBb]{2,4})[A-Za-z]?$")[, 2]
  if (is.na(year_token) || !nzchar(year_token)) {
    return(tibble(birth_year = NA_integer_,
                  birth_year_parse_flag = "no_year_token"))
  }

  normalized <- normalize_ocr_digits(year_token)
  if (!str_detect(normalized, "^[0-9]+$")) {
    return(tibble(birth_year = NA_integer_,
                  birth_year_parse_flag = "year_ocr_unresolved"))
  }

  if (nchar(normalized) == 2L) {
    year <- amws_century_year(normalized)
    flag <- "ok_2digit_amws"
  } else if (nchar(normalized) == 4L) {
    year <- as.integer(normalized)
    flag <- "ok_4digit"
    if (!is.na(year) && (year < 1800L || year > 1986L)) {
      compact_day <- suppressWarnings(as.integer(str_sub(normalized, 1, 2)))
      compact_yy <- str_sub(normalized, 3, 4)
      if (str_detect(year_token, "[A-Za-z]")) {
        year <- amws_century_year(str_sub(normalized, 1, 2))
        flag <- "ok_2digit_ocr_suffix"
      } else if (!is.na(compact_day) && compact_day >= 1L && compact_day <= 31L) {
        year <- amws_century_year(compact_yy)
        flag <- "ok_compact_day_year"
      }
    }
  } else if (nchar(normalized) == 3L) {
    compact_day <- suppressWarnings(as.integer(str_sub(normalized, 1, 1)))
    compact_yy <- str_sub(normalized, 2, 3)
    if (!is.na(compact_day) && compact_day >= 1L && compact_day <= 9L) {
      year <- amws_century_year(compact_yy)
      flag <- "ok_compact_day_year"
    } else {
      year <- as.integer(normalized)
      flag <- "year_token_implausible_length"
    }
  } else {
    year <- as.integer(normalized)
    flag <- "year_token_implausible_length"
  }

  if (is.na(year) || year < 1800L || year > 1986L) {
    return(tibble(birth_year = NA_integer_,
                  birth_year_parse_flag = "year_implausible"))
  }

  tibble(birth_year = as.integer(year), birth_year_parse_flag = flag)
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
  "Israel", "Italy", "Japan", "Korea", "Lebanon", "Mexico", "Mex", "Netherlands",
  "Norway", "Pakistan", "Palestine", "Poland", "Portugal", "Russia",
  "Scotland", "Spain", "Sweden", "Switzerland", "Syria", "Taiwan",
  "Turkey", "UK", "US", "USA", "USSR", "Wales"
)

bad_place_markers <- regex(
  paste(
    "\\bEduc\\b", "\\bProf\\s+Exp\\b", "\\bConcurrent\\s+Pos\\b",
    "\\bHonors\\b", "\\bMailing\\s+Add\\b", "\\bPhD\\b", "\\bUniv\\b",
    "\\bMem\\b", "\\bRes\\b", "\\bAssoc\\b", "\\bSCIENCE\\b",
    "\\bCHEMISTRY\\b", "\\bPHYSICS\\b", "\\bMEDICINE\\b",
    sep = "|"
  ),
  ignore_case = TRUE
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

parse_birth_city_one <- function(birth_place) {
  x <- normalize_text(birth_place)
  if (!nzchar(x)) {
    return(tibble(birth_city = "", birth_city_parse_flag = "no_birth_place"))
  }

  x <- str_replace_all(x, "\\s*,\\s*", ", ")
  x <- strip_edge_punct(x)

  if (nchar(x) > 140L || str_detect(x, bad_place_markers)) {
    return(tibble(birth_city = "",
                  birth_city_parse_flag = "birth_place_contaminated"))
  }

  if (str_detect(x, ",")) {
    city <- str_split_fixed(x, ",", 2)[, 1]
    city <- clean_city_candidate(city)
    flag <- "ok_before_comma"
  } else if (str_detect(x, "\\.\\s*[A-Za-z0-9]{1,15}$")) {
    city <- str_split_fixed(x, "\\.", 2)[, 1]
    city <- clean_city_candidate(city)
    flag <- "ok_before_period"
  } else {
    city <- clean_city_candidate(x)
    flag <- "single_component"
  }

  if (!nzchar(city) || nchar(city) < 3L ||
      str_detect(city, regex("^[^A-Za-z]+$"))) {
    return(tibble(birth_city = "",
                  birth_city_parse_flag = "city_empty_after_cleaning"))
  }

  if (str_detect(city, regex(paste0("^(", paste(state_or_region_tokens,
                                             collapse = "|"), ")$"),
                           ignore_case = TRUE)) ||
      (flag == "single_component" &&
       str_detect(city, regex(paste0("^(", paste(broad_place_tokens,
                                                collapse = "|"), ")$"),
                              ignore_case = TRUE)))) {
    return(tibble(birth_city = "",
                  birth_city_parse_flag = "place_only_no_city"))
  }

  if (nchar(city) > 70L || str_detect(city, bad_place_markers)) {
    return(tibble(birth_city = "",
                  birth_city_parse_flag = "city_contaminated"))
  }

  tibble(birth_city = city, birth_city_parse_flag = flag)
}

add_birth_year_city_one <- function(row) {
  bind_cols(
    row,
    parse_birth_year_one(row$birth_date[[1]]),
    parse_birth_city_one(row$birth_place[[1]])
  )
}

BIO_MARKER_RX <- paste(
  "(?:^|[,.;:'’\\s])(?:b_?|h)\\s+(?=[A-Z])",
  "\\bsee\\s+previous(?:\\s+edition)?\\b",
  "\\bdeceased\\b",
  sep = "|"
)

DEGREE_OR_SECTION_RX <- regex(
  paste(
    "\\bEduc\\b", "\\bBA\\b", "\\bBS\\b", "\\bMS\\b", "\\bMA\\b",
    "\\bMD\\b", "\\bPhD\\b", "\\bDPhil\\b", "\\bProf\\s+Exp\\b",
    "\\bMem\\b", "\\bRes\\b", "\\bMailing\\s+Add\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

FIELD_BEFORE_EDUC_RX <- regex(
  "\\b[A-Z][A-Z ,&;/\\-]{5,100}\\.?\\s+Educ\\b",
  ignore_case = FALSE
)

marker_core_start <- function(raw_text, start, end) {
  matched <- str_sub(raw_text, start, end)
  local <- str_locate(
    matched,
    regex("(?:b_?|h)\\s+|see\\s+previous(?:\\s+edition)?|deceased",
          ignore_case = TRUE)
  )
  if (is.na(local[1, 1])) start else start + local[1, 1] - 1L
}

marker_type_from_text <- function(marker_text) {
  marker_text <- normalize_text(marker_text)
  case_when(
    str_detect(marker_text, regex("^b_?(?:\\s+)?$", ignore_case = TRUE)) ~ "birth_b",
    str_detect(marker_text, regex("^h(?:\\s+)?$", ignore_case = TRUE)) ~ "birth_h",
    str_detect(marker_text, regex("^see\\s+previous", ignore_case = TRUE)) ~ "see_previous",
    str_detect(marker_text, regex("^deceased", ignore_case = TRUE)) ~ "deceased",
    TRUE ~ "unknown"
  )
}

find_bio_markers <- function(raw_text) {
  raw_text <- normalize_text(raw_text)
  locs <- str_locate_all(raw_text, regex(BIO_MARKER_RX, ignore_case = TRUE))[[1]]
  if (!nrow(locs)) {
    return(tibble(marker_start = integer(), marker_end = integer(),
                  marker_text = character(), marker_type = character()))
  }

  markers <- lapply(seq_len(nrow(locs)), function(i) {
    core_start <- marker_core_start(raw_text, locs[i, "start"], locs[i, "end"])
    marker_text <- str_sub(raw_text, core_start, locs[i, "end"]) |>
      normalize_text()
    tibble(
      marker_start = as.integer(core_start),
      marker_end = as.integer(locs[i, "end"]),
      marker_text = marker_text,
      marker_type = marker_type_from_text(marker_text)
    )
  }) |>
    bind_rows() |>
    distinct(marker_start, .keep_all = TRUE) |>
    arrange(marker_start)

  markers
}

classify_marker_candidate <- function(raw_text, marker_row, first_marker_start) {
  raw_text <- normalize_text(raw_text)
  marker_start <- marker_row$marker_start[[1]]
  if (is.na(first_marker_start) || marker_start <= first_marker_start + 25L) {
    return(NULL)
  }

  local_after <- str_sub(raw_text, marker_start,
                         min(nchar(raw_text), marker_start + 520L))
  local_short <- str_sub(raw_text, marker_start,
                         min(nchar(raw_text), marker_start + 180L))
  preceding <- str_sub(raw_text, max(1L, marker_start - 100L),
                       marker_start - 1L)

  has_date <- nzchar(find_first_date(local_short)$date[[1]])
  has_month <- str_detect(local_short, regex(paste0("\\b(", MONTH_RX, ")\\b"),
                                             ignore_case = TRUE))
  has_degree_or_section <- str_detect(local_after, DEGREE_OR_SECTION_RX)
  has_field_educ <- str_detect(local_after, FIELD_BEFORE_EDUC_RX)
  has_demographic_marker <- str_detect(
    local_short,
    regex("(?:^|[,;:.\\s])(?:m|c)\\s*[0-9OISZLB]{0,4}\\b",
          ignore_case = TRUE)
  )
  starts_in_address_context <- str_detect(
    preceding,
    regex("\\b(Mailing\\s+Add|Res|Mem|Concurrent\\s+Pos)\\b.{0,80}$",
          ignore_case = TRUE)
  )

  confidence <- "discard"
  reason <- ""
  if (marker_row$marker_type[[1]] %in% c("birth_b", "birth_h")) {
    if (((has_date || has_month) &&
         (has_degree_or_section || has_field_educ || has_demographic_marker)) ||
        (has_degree_or_section && has_field_educ)) {
      confidence <- "high"
      reason <- "birth_marker_with_biographic_and_education_or_section_evidence"
    } else if (has_date || (has_degree_or_section && has_field_educ)) {
      confidence <- "review"
      reason <- "partial_evidence_after_birth_marker"
    } else {
      reason <- "birth_marker_without_supporting_evidence"
    }
  } else if (marker_row$marker_type[[1]] %in% c("see_previous", "deceased")) {
    if (!starts_in_address_context && has_degree_or_section) {
      confidence <- "review"
      reason <- "status_marker_with_supporting_evidence"
    } else {
      reason <- "status_marker_without_supporting_evidence"
    }
  }

  tibble(
    marker_start = marker_start,
    marker_end = marker_row$marker_end[[1]],
    marker_text = marker_row$marker_text[[1]],
    marker_type = marker_row$marker_type[[1]],
    confidence = confidence,
    reason = reason,
    has_date = has_date,
    has_month = has_month,
    has_degree_or_section = has_degree_or_section,
    has_field_educ = has_field_educ,
    has_demographic_marker = has_demographic_marker,
    starts_in_address_context = starts_in_address_context,
    evidence = local_after
  )
}

detect_extra_entries <- function(row) {
  raw_text <- normalize_text(row$raw_text[[1]])
  markers <- find_bio_markers(raw_text)
  if (!nrow(markers)) {
    return(tibble())
  }

  first_marker_start <- markers$marker_start[[1]]
  candidates <- bind_rows(lapply(seq_len(nrow(markers)), function(i) {
    classify_marker_candidate(raw_text, markers[i, ], first_marker_start)
  }))
  if (!nrow(candidates)) {
    return(tibble())
  }

  candidates |>
    filter(confidence != "discard") |>
    mutate(
      doc_id = row$doc_id[[1]],
      source_lineid = as.integer(row$lineid[[1]]),
      raw_text = raw_text,
      .before = marker_start
    )
}

parse_adjusted_row <- function(base_row, raw_text_adjusted, raw_text_for_parse,
                               instance) {
  parsed <- parse_entry_regex(raw_text_for_parse)
  parsed <- bind_cols(parsed, parse_birth_year_one(parsed$birth_date[[1]]),
                      parse_birth_city_one(parsed$birth_place[[1]]))

  base_row |>
    mutate(
      source_lineid = as.integer(lineid),
      entry_instance = instance,
      lineid_original_key = paste0(source_lineid, ".", entry_instance),
      raw_text_adjusted = raw_text_adjusted,
      name_raw = parsed$name_raw[[1]],
      birth_place = parsed$birth_place[[1]],
      birth_date = parsed$birth_date[[1]],
      field = parsed$field[[1]],
      regex_birth_rule_id = parsed$regex_birth_rule_id[[1]],
      regex_field_rule_id = parsed$regex_field_rule_id[[1]],
      regex_parse_flag = parsed$regex_parse_flag[[1]],
      birth_year = ifelse(is.na(parsed$birth_year[[1]]), "",
                          as.character(parsed$birth_year[[1]])),
      birth_year_parse_flag = parsed$birth_year_parse_flag[[1]],
      birth_city = parsed$birth_city[[1]],
      birth_city_parse_flag = parsed$birth_city_parse_flag[[1]],
      multi_entry_expanded = TRUE,
      multi_entry_marker = "",
      multi_entry_evidence = "",
      multi_entry_confidence = "high"
    )
}

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
input_file <- env_chr(
  "AMWS_REGEX_MULTI_INPUT_FILE",
  file.path(default_output_dir,
            "amws_ed16_entries_regex_parsed_birth_year_city.csv")
)
output_dir <- env_chr("AMWS_REGEX_MULTI_OUTPUT_DIR", default_output_dir)

input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input <- read_csv(input_file, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c("doc_id", "source_file", "run_id", "lineid", "raw_text",
                   "birth_place", "birth_date", "field", "name_raw",
                   "regex_birth_rule_id", "regex_field_rule_id",
                   "regex_parse_flag", "batch_id", "birth_year",
                   "birth_year_parse_flag", "birth_city",
                   "birth_city_parse_flag")
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input table is missing required columns: ",
       paste(missing_cols, collapse = ", "))
}

input <- input |>
  mutate(
    lineid = as.integer(lineid),
    raw_text = normalize_text(raw_text)
  )

candidate_rows <- bind_rows(lapply(seq_len(nrow(input)), function(i) {
  detect_extra_entries(input[i, ])
}))

audit <- if (nrow(candidate_rows)) {
  candidate_rows |>
    group_by(doc_id, source_lineid) |>
    arrange(marker_start, .by_group = TRUE) |>
    mutate(
      candidate_instance = row_number() + 1L,
      expands_row = confidence == "high",
      evidence_short = str_sub(evidence, 1, 260)
    ) |>
    ungroup() |>
    select(doc_id, source_lineid, candidate_instance, marker_start, marker_end,
           marker_text, marker_type, confidence, expands_row, reason,
           has_date, has_month, has_degree_or_section, has_field_educ,
           has_demographic_marker,
           starts_in_address_context, evidence_short, raw_text)
} else {
  tibble(
    doc_id = character(),
    source_lineid = integer(),
    candidate_instance = integer(),
    marker_start = integer(),
    marker_end = integer(),
    marker_text = character(),
    marker_type = character(),
    confidence = character(),
    expands_row = logical(),
    reason = character(),
    has_date = logical(),
    has_month = logical(),
    has_degree_or_section = logical(),
    has_field_educ = logical(),
    has_demographic_marker = logical(),
    starts_in_address_context = logical(),
    evidence_short = character(),
    raw_text = character()
  )
}

input_with_meta <- input |>
  mutate(
    source_lineid = lineid,
    entry_instance = 1L,
    lineid_original_key = paste0(source_lineid, ".1"),
    raw_text_adjusted = raw_text,
    multi_entry_expanded = FALSE,
    multi_entry_marker = "",
    multi_entry_evidence = "",
    multi_entry_confidence = ""
  )

expanded_extra <- if (nrow(audit)) {
  high <- audit |> filter(expands_row)
  bind_rows(lapply(seq_len(nrow(high)), function(i) {
    cand <- high[i, ]
    base <- input |> filter(doc_id == cand$doc_id[[1]],
                            lineid == cand$source_lineid[[1]])
    if (nrow(base) != 1L) {
      stop("Could not find one base row for ",
           cand$doc_id[[1]], " / ", cand$source_lineid[[1]])
    }
    adjusted <- str_sub(base$raw_text[[1]], cand$marker_start[[1]]) |>
      normalize_text()
    later_markers <- audit |>
      filter(doc_id == cand$doc_id[[1]],
             source_lineid == cand$source_lineid[[1]],
             marker_start > cand$marker_start[[1]])
    next_marker_start <- if (nrow(later_markers)) {
      min(later_markers$marker_start)
    } else {
      NA_integer_
    }
    parse_end <- if (is.na(next_marker_start)) {
      nchar(base$raw_text[[1]])
    } else {
      next_marker_start - 1L
    }
    adjusted_parse <- str_sub(base$raw_text[[1]], cand$marker_start[[1]],
                              parse_end) |>
      normalize_text()
    row <- parse_adjusted_row(base, adjusted, adjusted_parse,
                              cand$candidate_instance[[1]])
    row |>
      mutate(
        multi_entry_marker = cand$marker_text[[1]],
        multi_entry_evidence = cand$evidence_short[[1]],
        multi_entry_confidence = cand$confidence[[1]]
      )
  }))
} else {
  input_with_meta[0, ]
}

expanded <- bind_rows(input_with_meta, expanded_extra) |>
  arrange(doc_id, source_lineid, entry_instance) |>
  group_by(doc_id) |>
  mutate(lineid = row_number()) |>
  ungroup() |>
  select(doc_id, source_file, run_id, lineid, source_lineid, entry_instance,
         lineid_original_key, raw_text, raw_text_adjusted,
         birth_place, birth_date, birth_year, birth_city, field, name_raw,
         regex_birth_rule_id, regex_field_rule_id, regex_parse_flag,
         birth_year_parse_flag, birth_city_parse_flag, batch_id,
         multi_entry_expanded, multi_entry_marker, multi_entry_confidence,
         multi_entry_evidence, everything())

if (anyDuplicated(paste(expanded$doc_id, expanded$lineid))) {
  stop("Expanded output has duplicated doc_id + lineid.")
}

if (any(expanded$entry_instance == 1L &
        expanded$raw_text_adjusted != expanded$raw_text)) {
  stop("Original rows should have raw_text_adjusted equal to raw_text.")
}

new_rows <- expanded |> filter(entry_instance > 1L)
if (nrow(new_rows) &&
    any(!str_starts(new_rows$raw_text_adjusted, fixed(new_rows$multi_entry_marker)))) {
  stop("At least one added row does not start raw_text_adjusted at marker.")
}

expected_n <- nrow(input) + sum(audit$expands_row)
if (nrow(expanded) != expected_n) {
  stop("Expanded row count mismatch: expected ", expected_n,
       ", got ", nrow(expanded))
}

expanded_csv <- file.path(
  output_dir,
  "amws_ed16_entries_regex_parsed_birth_year_city_expanded.csv"
)
expanded_xlsx <- file.path(
  output_dir,
  "amws_ed16_entries_regex_parsed_birth_year_city_expanded.xlsx"
)
audit_csv <- file.path(output_dir, "amws_ed16_multi_entry_expansion_audit.csv")
audit_xlsx <- file.path(output_dir, "amws_ed16_multi_entry_expansion_audit.xlsx")

readr::write_excel_csv(expanded, expanded_csv, na = "")
writexl::write_xlsx(list(entries = expanded), expanded_xlsx)
readr::write_excel_csv(audit, audit_csv, na = "")
writexl::write_xlsx(list(audit = audit), audit_xlsx)

cat("Input:", input_file, "\n")
cat("Original rows:", nrow(input), "\n")
cat("Candidate markers:", nrow(audit), "\n")
cat("High-confidence expansions:", sum(audit$expands_row), "\n")
cat("Expanded rows:", nrow(expanded), "\n")
cat("Wrote expanded CSV:", expanded_csv, "\n")
cat("Wrote expanded XLSX:", expanded_xlsx, "\n")
cat("Wrote audit CSV:", audit_csv, "\n")
cat("Wrote audit XLSX:", audit_xlsx, "\n")
