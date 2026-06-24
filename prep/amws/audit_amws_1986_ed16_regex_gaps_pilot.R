###############################################################################
# Audit regex gaps for the AMWS edition 16 (1986) raw-XLSX pilot.
#
# Input:
#   <Dropbox root>/output/amws/transcription_runs/
#     amws16_A_0_200_precleaning_first10_rawxlsx_4agents/
#     amws_entries_regex_parsed.csv
#
# Outputs:
#   <same run directory>/regex_gap_audit/
#     regex_gap_audit_cases.csv
#     regex_pattern_inventory.csv
#     regex_test_results.csv
#     proposed_regex_rules.R
#     audit_summary.md
#
# Scope:
#   Non-destructive audit of rows where birth_place, birth_date, or field is
#   missing. This script proposes regex rules; it does not edit the parser or
#   source AMWS files.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
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
parsed_file <- file.path(run_dir, "amws_entries_regex_parsed.csv")
audit_dir <- file.path(run_dir, "regex_gap_audit")

cases_file <- file.path(audit_dir, "regex_gap_audit_cases.csv")
patterns_file <- file.path(audit_dir, "regex_pattern_inventory.csv")
tests_file <- file.path(audit_dir, "regex_test_results.csv")
rules_file <- file.path(audit_dir, "proposed_regex_rules.R")
summary_file <- file.path(audit_dir, "audit_summary.md")

if (!file.exists(parsed_file)) {
  stop("Parsed input not found: ", parsed_file)
}
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

MONTH_RX <- paste(
  "January", "February", "March", "April", "September", "October",
  "November", "December", "June", "July", "August", "Sept", "Sep",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct",
  "Nov", "Dec",
  sep = "|"
)

DATE_RX <- paste0(
  "\\b(", MONTH_RX, ")\\.?\\s*,?\\s*",
  "[0-9OISZLB]{1,2}\\s*[,.'\\u2019`-]?\\s*",
  "[0-9OISZLB]{1,4}[A-Za-z]?\\b"
)

OCR_NOV_RX <- "\\bN[o0][*v]\\s*J?[A-Za-z0-9]{2,4}\\b"

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

strip_edge_punct <- function(x) {
  x <- normalize_text(x)
  x |>
    str_replace("^[,;:.\\s'\"\\-_*]+", "") |>
    str_replace("[,;:.\\s'\"\\-_*]+$", "") |>
    normalize_text()
}

clean_field_candidate <- function(x) {
  x <- normalize_text(x)
  old <- NA_character_
  while (!identical(old, x)) {
    old <- x
    x <- x |>
      str_replace(regex("^[,;:.\\s'\"\\-_*]+"), "") |>
      str_replace(regex("^(?:US\\s+citizen|nat\\s+US)\\b[,;:.\\s-]*",
                        ignore_case = TRUE), "") |>
      str_replace(regex("^(?:wid|div|sep)\\b[,;:.\\s-]*",
                        ignore_case = TRUE), "") |>
      str_replace(regex("^(?:m|c)\\s*[0-9OISZLB]{0,4}[-]?[A-Za-z]?\\b[,;:.\\s-]*",
                        ignore_case = TRUE), "") |>
      str_replace(regex("^[A-Z]{1,3}\\s*[0-9]{1,2}\\s+"), "") |>
      str_replace(regex("^[^A-Za-z]{1,12}"), "")
  }
  x |>
    str_replace(regex("^[A-Z]{1,3}\\s*[0-9]{1,2}\\s+"), "") |>
    strip_edge_punct()
}

find_first_date <- function(x) {
  loc <- str_locate(x, regex(DATE_RX, ignore_case = TRUE))
  if (!is.na(loc[1, 1])) {
    return(tibble(
      date = str_sub(x, loc[1, 1], loc[1, 2]) |> normalize_text(),
      start = loc[1, 1],
      end = loc[1, 2],
      rule_id = "P_DATE_FLEX_MONTH"
    ))
  }

  loc <- str_locate(x, regex(OCR_NOV_RX, ignore_case = TRUE))
  if (!is.na(loc[1, 1])) {
    return(tibble(
      date = str_sub(x, loc[1, 1], loc[1, 2]) |> normalize_text(),
      start = loc[1, 1],
      end = loc[1, 2],
      rule_id = "P_DATE_OCR_NOVEMBER"
    ))
  }

  tibble(date = "", start = NA_integer_, end = NA_integer_, rule_id = "")
}

find_birth_marker <- function(raw_text) {
  marker_patterns <- c(
    "P_BIRTH_MARKER_SPACED" = "(?i)(?:^|[,.;:\\s])(?:b|h)\\s+(?=[A-Z])",
    "P_BIRTH_MARKER_GLUED" = "(?i)(?:\\.|['\\u2019*])b[_\\s]*(?=[A-Z])",
    "P_BIRTH_MARKER_UNDERSCORE" = "(?i)(?:^|[,.;:\\s])b_+\\s*(?=[A-Z])"
  )

  hits <- lapply(names(marker_patterns), function(rule_id) {
    loc <- str_locate(raw_text, regex(marker_patterns[[rule_id]]))
    if (is.na(loc[1, 1])) {
      return(NULL)
    }
    tibble(rule_id = rule_id, start = loc[1, 1], end = loc[1, 2])
  })

  hits <- bind_rows(hits)
  if (!nrow(hits)) {
    return(tibble(rule_id = "", start = NA_integer_, end = NA_integer_))
  }
  hits |> arrange(start) |> slice(1)
}

propose_birth_from_marker <- function(raw_text) {
  marker <- find_birth_marker(raw_text)
  if (!nzchar(marker$rule_id[[1]])) {
    return(tibble(
      birthplace = "", birth_date = "", rule_id = "",
      evidence = "", date_end = NA_integer_
    ))
  }

  after_marker <- str_sub(raw_text, marker$end[[1]] + 1)
  date <- find_first_date(after_marker)
  if (!nzchar(date$date[[1]])) {
    return(tibble(
      birthplace = "", birth_date = "", rule_id = "",
      evidence = "", date_end = NA_integer_
    ))
  }

  place <- str_sub(after_marker, 1, date$start[[1]] - 1) |> strip_edge_punct()
  if (!nzchar(place)) {
    return(tibble(
      birthplace = "", birth_date = date$date[[1]],
      rule_id = "P_BIRTH_DATE_ONLY_AFTER_MARKER",
      evidence = str_sub(after_marker, 1, min(nchar(after_marker), 90)),
      date_end = marker$end[[1]] + date$end[[1]]
    ))
  }

  tibble(
    birthplace = place,
    birth_date = date$date[[1]],
    rule_id = marker$rule_id[[1]],
    evidence = str_sub(after_marker, 1, min(nchar(after_marker), 110)),
    date_end = marker$end[[1]] + date$end[[1]]
  )
}

propose_birth_from_current_place <- function(current_birthplace) {
  date <- find_first_date(current_birthplace)
  if (!nzchar(date$date[[1]])) {
    return(tibble(
      birthplace = "", birth_date = "", rule_id = "",
      evidence = "", date_end = NA_integer_
    ))
  }

  tibble(
    birthplace = str_sub(current_birthplace, 1, date$start[[1]] - 1) |>
      strip_edge_punct(),
    birth_date = date$date[[1]],
    rule_id = "P_BIRTH_DATE_SWALLOWED_IN_PLACE",
    evidence = current_birthplace,
    date_end = NA_integer_
  )
}

propose_birth_from_opening_place_date <- function(raw_text) {
  prefix <- str_split(raw_text, ";", n = 2)[[1]][1] |> normalize_text()
  date <- find_first_date(prefix)
  if (!nzchar(date$date[[1]])) {
    return(tibble(
      birthplace = "", birth_date = "", rule_id = "",
      evidence = "", date_end = NA_integer_
    ))
  }

  place <- str_sub(prefix, 1, date$start[[1]] - 1) |>
    str_replace(regex("^[^A-Z]*"), "") |>
    strip_edge_punct()

  if (!nzchar(place)) {
    return(tibble(
      birthplace = "", birth_date = "", rule_id = "",
      evidence = "", date_end = NA_integer_
    ))
  }

  tibble(
    birthplace = place,
    birth_date = date$date[[1]],
    rule_id = "P_BIRTH_OPENING_PLACE_DATE_NO_MARKER",
    evidence = prefix,
    date_end = date$end[[1]]
  )
}

best_birth_candidate <- function(raw_text, current_birthplace) {
  marker <- propose_birth_from_marker(raw_text)
  if (nzchar(marker$rule_id[[1]])) {
    return(marker)
  }

  swallowed <- propose_birth_from_current_place(current_birthplace)
  if (nzchar(swallowed$rule_id[[1]])) {
    return(swallowed)
  }

  opening <- propose_birth_from_opening_place_date(raw_text)
  if (nzchar(opening$rule_id[[1]])) {
    return(opening)
  }

  tibble(
    birthplace = "", birth_date = "", rule_id = "",
    evidence = "", date_end = NA_integer_
  )
}

extract_text_after_date <- function(raw_text, birth_date) {
  if (!nzchar(birth_date)) {
    return("")
  }
  loc <- str_locate(raw_text, fixed(birth_date))
  if (is.na(loc[1, 1])) {
    loc <- str_locate(raw_text, regex(DATE_RX, ignore_case = TRUE))
  }
  if (is.na(loc[1, 1])) {
    loc <- str_locate(raw_text, regex(OCR_NOV_RX, ignore_case = TRUE))
  }
  if (is.na(loc[1, 1])) {
    return("")
  }
  str_sub(raw_text, loc[1, 2] + 1) |> normalize_text()
}

propose_field_after_date <- function(raw_text, birth_date) {
  after_date <- extract_text_after_date(raw_text, birth_date)
  if (!nzchar(after_date)) {
    return(tibble(field = "", rule_id = "", evidence = ""))
  }

  source <- clean_field_candidate(after_date)
  if (!nzchar(source)) {
    return(tibble(field = "", rule_id = "", evidence = ""))
  }

  stop_loc <- str_locate(
    source,
    regex("\\b(Educ|Prof\\s+Exp|Concurrent\\s+Pos|Honors\\s*&\\s*Awards|Mem|Res|Mailing\\s*Add|Exp)\\s*[:;.!\\-\\u25a0]?",
          ignore_case = TRUE)
  )
  if (!is.na(stop_loc[1, 1])) {
    field <- str_sub(source, 1, stop_loc[1, 1] - 1)
  } else {
    field <- str_split(source, "\\.\\s+", n = 2)[[1]][1]
  }

  field <- field |>
    str_replace("\\\\B.*$", "") |>
    str_replace(regex("\\bA\\s+EOu.*$", ignore_case = TRUE), "") |>
    clean_field_candidate() |>
    str_replace(regex("^A.{1,4}'(?=[A-Z])"), "") |>
    str_replace(regex("^[A-Z][^A-Z]{1,4}[A-Z]'?"), "") |>
    strip_edge_punct() |>
    str_replace_all("\\s+", " ")

  if (!nzchar(field) || nchar(field) > 120 ||
      str_detect(field, regex("\\b(Univ|PhD|Col|prof|res|Dept)\\b",
                             ignore_case = TRUE)) ||
      str_detect(field, "[0-9\\^?\\\\\u25a0]")) {
    return(tibble(field = "", rule_id = "", evidence = source))
  }

  tibble(
    field = toupper(field),
    rule_id = "P_FIELD_AFTER_DATE_DEMOGRAPHICS",
    evidence = str_sub(source, 1, min(nchar(source), 120))
  )
}

propose_field_without_birth <- function(raw_text) {
  source <- raw_text

  marker <- find_birth_marker(source)
  if (nzchar(marker$rule_id[[1]])) {
    return(tibble(field = "", rule_id = "", evidence = ""))
  }

  if (str_detect(source, regex("see\\s+previous", ignore_case = TRUE))) {
    prefix <- str_replace(source, regex("see\\s+previous(?:\\s+edition)?.*$",
                                       ignore_case = TRUE), "")
    field <- str_match(prefix, "[-,]\\s*([^,.;-]{4,90})\\s*,?\\s*$")[1, 2]
    field <- clean_field_candidate(field)
    if (nzchar(field)) {
      return(tibble(
        field = toupper(field),
        rule_id = "P_FIELD_BEFORE_SEE_PREVIOUS",
        evidence = prefix
      ))
    }
  }

  if (!str_detect(source, regex("\\bEduc\\b", ignore_case = TRUE))) {
    return(tibble(field = "", rule_id = "", evidence = ""))
  }

  before_educ <- str_replace(source, regex("\\bEduc\\b.*$", ignore_case = TRUE),
                             "")
  before_educ <- str_replace(before_educ, regex("^.{0,80}?\\b(?:m|c)\\s*[0-9OISZLB]{0,4}[-]?[A-Za-z]?\\b[,;:.\\s-]*",
                                               ignore_case = TRUE), "")
  before_educ <- str_replace(before_educ, regex("^.*?[,\\.]\\s*"), "")
  field <- before_educ |> clean_field_candidate()

  if (!nzchar(field) || nchar(field) > 120 ||
      str_detect(field, regex("\\b(PhD|Univ|Col|prof|res|Dept)\\b",
                             ignore_case = TRUE))) {
    return(tibble(field = "", rule_id = "", evidence = before_educ))
  }

  tibble(
    field = toupper(field),
    rule_id = "P_FIELD_BEFORE_EDUC_NO_BIRTH",
    evidence = str_sub(before_educ, 1, min(nchar(before_educ), 120))
  )
}

best_field_candidate <- function(raw_text, current_birth_date, proposed_birth_date) {
  date <- ifelse(nzchar(current_birth_date), current_birth_date,
                 proposed_birth_date)

  from_date <- propose_field_after_date(raw_text, date)
  if (nzchar(from_date$rule_id[[1]])) {
    return(from_date)
  }

  from_no_birth <- propose_field_without_birth(raw_text)
  if (nzchar(from_no_birth$rule_id[[1]])) {
    return(from_no_birth)
  }

  tibble(field = "", rule_id = "", evidence = "")
}

classify_case <- function(row, birth_candidate, field_candidate) {
  raw_text <- row$raw_text[[1]]
  missing_birthplace <- !nzchar(row$birthplace[[1]])
  missing_birth_date <- !nzchar(row$birth_date[[1]])
  missing_field <- !nzchar(row$field[[1]])

  recoverable <- (missing_birthplace && nzchar(birth_candidate$birthplace[[1]])) ||
    (missing_birth_date && nzchar(birth_candidate$birth_date[[1]])) ||
    (missing_field && nzchar(field_candidate$field[[1]]))

  if (recoverable) {
    return("recoverable_regex")
  }

  if (str_detect(raw_text, regex("see\\s+previous|deceased",
                                ignore_case = TRUE))) {
    return("true_missing")
  }

  if ((missing_birthplace || missing_birth_date) && !missing_field &&
      !nzchar(find_birth_marker(raw_text)$rule_id[[1]])) {
    return("true_missing")
  }

  if (missing_birthplace && !missing_birth_date &&
      str_detect(raw_text, regex("\\bb\\s+", ignore_case = TRUE))) {
    return("true_missing")
  }

  if (str_detect(raw_text, regex("^\\s*(-|[?.]|[a-z])")) ||
      !str_detect(raw_text, ",")) {
    return("segmentation_problem")
  }

  if (str_detect(raw_text, regex("[\\^<>*]|[A-Za-z][0-9][A-Za-z]|[0-9][A-Za-z][0-9]"))) {
    return("ocr_corruption")
  }

  "manual_review"
}

make_evidence <- function(row, birth_candidate, field_candidate) {
  evidence <- c()
  if (nzchar(birth_candidate$evidence[[1]])) {
    evidence <- c(evidence, paste0(birth_candidate$rule_id[[1]], ": ",
                                   birth_candidate$evidence[[1]]))
  }
  if (nzchar(field_candidate$evidence[[1]])) {
    evidence <- c(evidence, paste0(field_candidate$rule_id[[1]], ": ",
                                   field_candidate$evidence[[1]]))
  }
  if (!length(evidence)) {
    evidence <- str_sub(row$raw_text[[1]], 1, min(nchar(row$raw_text[[1]]), 160))
  }
  paste(evidence, collapse = " | ")
}

make_recommended_action <- function(case_class) {
  dplyr::case_when(
    case_class == "recoverable_regex" ~
      "Add candidate regex to parser after reviewing false-positive risk.",
    case_class == "true_missing" ~
      "Leave missing; raw_text does not contain the missing value.",
    case_class == "segmentation_problem" ~
      "Review entry segmentation before field extraction.",
    case_class == "ocr_corruption" ~
      "Route to manual/model-assisted review; regex evidence is unsafe.",
    TRUE ~ "Manual review."
  )
}

parsed_raw <- read_csv(parsed_file, show_col_types = FALSE)

if (!"birthplace" %in% names(parsed_raw) && "birth_place" %in% names(parsed_raw)) {
  parsed_raw <- parsed_raw |>
    mutate(birthplace = birth_place)
}

parsed <- parsed_raw |>
  mutate(
    across(c(birthplace, birth_date, field), ~ ifelse(is.na(.x), "", .x)),
    raw_text = normalize_text(raw_text)
  )

required_cols <- c("global_lineid", "lineid", "batch_id", "raw_text",
                   "birthplace", "birth_date", "field")
missing_cols <- setdiff(required_cols, names(parsed))
if (length(missing_cols)) {
  stop("Parsed table is missing required columns: ",
       paste(missing_cols, collapse = ", "))
}

gap_rows <- parsed |>
  filter(!nzchar(birthplace) | !nzchar(birth_date) | !nzchar(field))

audited <- lapply(seq_len(nrow(gap_rows)), function(i) {
  row <- gap_rows[i, ]
  birth_candidate <- best_birth_candidate(row$raw_text[[1]],
                                          row$birthplace[[1]])
  proposed_date_for_field <- birth_candidate$birth_date[[1]]
  field_candidate <- best_field_candidate(
    row$raw_text[[1]],
    row$birth_date[[1]],
    proposed_date_for_field
  )
  case_class <- classify_case(row, birth_candidate, field_candidate)

  tibble(
    global_lineid = row$global_lineid,
    lineid = row$lineid,
    batch_id = row$batch_id,
    missing_birthplace = !nzchar(row$birthplace[[1]]),
    missing_birth_date = !nzchar(row$birth_date[[1]]),
    missing_field = !nzchar(row$field[[1]]),
    current_birthplace = row$birthplace,
    current_birth_date = row$birth_date,
    current_field = row$field,
    proposed_birthplace = birth_candidate$birthplace[[1]],
    proposed_birth_date = birth_candidate$birth_date[[1]],
    proposed_field = field_candidate$field[[1]],
    birth_rule_id = birth_candidate$rule_id[[1]],
    field_rule_id = field_candidate$rule_id[[1]],
    case_class = case_class,
    evidence_text = make_evidence(row, birth_candidate, field_candidate),
    recommended_action = make_recommended_action(case_class),
    raw_text = row$raw_text
  )
}) |> bind_rows()

audit_cases <- audited |>
  select(global_lineid, lineid, batch_id, missing_birthplace,
         missing_birth_date, missing_field, current_birthplace,
         current_birth_date, current_field, case_class, evidence_text,
         recommended_action, raw_text)

risky_pattern_ids <- c(
  "P_BIRTH_OPENING_PLACE_DATE_NO_MARKER",
  "P_FIELD_BEFORE_EDUC_NO_BIRTH"
)

test_rows <- bind_rows(
  audited |>
    filter(missing_birthplace, nzchar(proposed_birthplace)) |>
    transmute(
      global_lineid, lineid, pattern_id = birth_rule_id,
      target_field = "birthplace", old_value = current_birthplace,
      proposed_value = proposed_birthplace, changed = TRUE,
      needs_manual_review = case_class != "recoverable_regex" |
        birth_rule_id %in% risky_pattern_ids,
      raw_text
    ),
  audited |>
    filter(missing_birth_date, nzchar(proposed_birth_date)) |>
    transmute(
      global_lineid, lineid, pattern_id = birth_rule_id,
      target_field = "birth_date", old_value = current_birth_date,
      proposed_value = proposed_birth_date, changed = TRUE,
      needs_manual_review = case_class != "recoverable_regex" |
        birth_rule_id %in% risky_pattern_ids,
      raw_text
    ),
  audited |>
    filter(missing_field, nzchar(proposed_field)) |>
    transmute(
      global_lineid, lineid, pattern_id = field_rule_id,
      target_field = "field", old_value = current_field,
      proposed_value = proposed_field, changed = TRUE,
      needs_manual_review = case_class != "recoverable_regex" |
        field_rule_id %in% risky_pattern_ids,
      raw_text
    )
) |>
  arrange(global_lineid, target_field)

pattern_meta <- tribble(
  ~pattern_id, ~target_field, ~priority, ~case_class, ~regex, ~description, ~false_positive_risk, ~do_not_match_examples,
  "P_BIRTH_MARKER_SPACED", "birthplace,birth_date", "safe", "recoverable_regex",
  "(?i)(?:^|[,.;:\\s])(?:b|h)\\s+(?=[A-Z]) ... DATE_RX",
  "Capture birth blocks after spaced b/h markers, including likely OCR h for b.",
  "May capture non-birth h only if followed by place/date; require date within same clause.",
  "Do not use without a following month/date.",
  "P_BIRTH_MARKER_GLUED", "birthplace,birth_date", "moderate", "recoverable_regex",
  "(?i)(?:\\.|['\\u2019*])b[_\\s]*(?=[A-Z]) ... DATE_RX",
  "Capture b glued to names or punctuation, such as .bMinn or NORMAN'b.",
  "Could capture OCR debris; require month/date and plausible place before date.",
  "Do not use when the following text has no month/date.",
  "P_BIRTH_MARKER_UNDERSCORE", "birthplace,birth_date", "moderate", "recoverable_regex",
  "(?i)(?:^|[,.;:\\s])b[_\\s]*(?=[A-Z]) ... DATE_RX",
  "Capture b_ or b directly attached to a place.",
  "Moderate OCR risk; require a date within the next short span.",
  "Do not use for field words beginning with b.",
  "P_BIRTH_DATE_SWALLOWED_IN_PLACE", "birthplace,birth_date", "safe", "recoverable_regex",
  "current_birthplace contains DATE_RX or OCR_NOV_RX",
  "Split a parsed birthplace that swallowed the date.",
  "Low when applied only to rows with missing birth_date.",
  "Do not overwrite non-empty birth_date.",
  "P_BIRTH_OPENING_PLACE_DATE_NO_MARKER", "birthplace,birth_date", "risky", "recoverable_regex",
  "^<opening text before semicolon> contains place + DATE_RX",
  "Recover place/date when segmentation lost the name and b marker.",
  "High; often indicates segmentation problems.",
  "Do not apply automatically to production data.",
  "P_BIRTH_DATE_ONLY_AFTER_MARKER", "birth_date", "safe", "true_missing",
  "b DATE_RX with no place before the date",
  "Recognize rows where birth date is present but birthplace is truly absent.",
  "Low; used to avoid inventing a birthplace.",
  "Do not create a birthplace from the date.",
  "P_FIELD_AFTER_DATE_DEMOGRAPHICS", "field", "moderate", "recoverable_regex",
  "<birth_date>; demographic prefixes; FIELD before Educ/Prof Exp/etc.",
  "Recover fields after birth date and demographic fragments.",
  "May retain OCR debris in badly corrupted fields; cap length and stop at section markers.",
  "Do not use if candidate contains education/institution terms.",
  "P_FIELD_BEFORE_EDUC_NO_BIRTH", "field", "risky", "recoverable_regex",
  "No birth block; FIELD before Educ",
  "Recover field-only entries that start directly with discipline and education.",
  "High when name/segmentation is corrupted.",
  "Do not use when candidate contains PhD, Univ, Col, Dept, or career text.",
  "P_FIELD_BEFORE_SEE_PREVIOUS", "field", "moderate", "recoverable_regex",
  "FIELD before see previous",
  "Recover fields in previous-edition rows, including truncated see previous text.",
  "Moderate when name is OCR-corrupted.",
  "Do not use if field fragment is not readable."
)

covered <- test_rows |>
  filter(nzchar(pattern_id)) |>
  group_by(pattern_id) |>
  summarise(
    covered_lineids = paste(unique(global_lineid), collapse = ";"),
    .groups = "drop"
  )

pattern_inventory <- pattern_meta |>
  inner_join(covered, by = "pattern_id") |>
  arrange(pattern_id)

if (!nrow(pattern_inventory)) {
  stop("No regex patterns covered any audited rows.")
}

proposed_rules <- c(
  "###############################################################################",
  "# Proposed AMWS regex-gap rules.",
  "#",
  "# Generated by audit_amws_1986_ed16_regex_gaps_pilot.R.",
  "# These functions are proposals only. They do not read or write project data.",
  "###############################################################################",
  "",
  "suppressPackageStartupMessages({",
  "  library(dplyr)",
  "  library(stringr)",
  "  library(tibble)",
  "})",
  "",
  "amws_month_rx <- paste(",
  "  'January', 'February', 'March', 'April', 'September', 'October',",
  "  'November', 'December', 'June', 'July', 'August', 'Sept', 'Sep',",
  "  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Oct',",
  "  'Nov', 'Dec',",
  "  sep = '|'",
  ")",
  "",
  "amws_date_rx <- paste0(",
  "  '\\\\b(', amws_month_rx, ')\\\\.?\\\\s*,?\\\\s*',",
  "  \"[0-9OISZLB]{1,2}\\\\s*[,.;\\\\u2019`]?\\\\s*\",",
  "  '[0-9OISZLB]{1,4}[A-Za-z]?\\\\b'",
  ")",
  "",
  "amws_ocr_nov_rx <- '\\\\bN[o0][*v]\\\\s*J?[A-Za-z0-9]{2,4}\\\\b'",
  "",
  "amws_normalize_text <- function(x) {",
  "  x <- ifelse(is.na(x), '', x)",
  "  x |> str_replace_all('\\\\u00a0', ' ') |> str_replace_all('\\\\s+', ' ') |> str_trim()",
  "}",
  "",
  "amws_strip_edge_punct <- function(x) {",
  "  x |> amws_normalize_text() |>",
  "    str_replace(\"^[,;:.\\\\s\\\"\\\\-_*]+\", \"\") |>",
  "    str_replace(\"[,;:.\\\\s\\\"\\\\-_*]+$\", \"\") |>",
  "    amws_normalize_text()",
  "}",
  "",
  "amws_find_first_date <- function(x) {",
  "  loc <- str_locate(x, regex(amws_date_rx, ignore_case = TRUE))",
  "  rule_id <- 'P_DATE_FLEX_MONTH'",
  "  if (is.na(loc[1, 1])) {",
  "    loc <- str_locate(x, regex(amws_ocr_nov_rx, ignore_case = TRUE))",
  "    rule_id <- 'P_DATE_OCR_NOVEMBER'",
  "  }",
  "  if (is.na(loc[1, 1])) {",
  "    return(tibble(date = '', start = NA_integer_, end = NA_integer_, rule_id = ''))",
  "  }",
  "  tibble(date = str_sub(x, loc[1, 1], loc[1, 2]) |> amws_normalize_text(),",
  "         start = loc[1, 1], end = loc[1, 2], rule_id = rule_id)",
  "}",
  "",
  "extract_birth_marker_variant <- function(raw_text) {",
  "  raw_text <- amws_normalize_text(raw_text)",
  "  marker_rx <- c(",
  "    P_BIRTH_MARKER_SPACED = '(?i)(?:^|[,.;:\\\\s])(?:b|h)\\\\s+(?=[A-Z])',",
  "    P_BIRTH_MARKER_GLUED = \"(?i)(?:\\\\.|['\\\\u2019*])b[_\\\\s]*(?=[A-Z])\",",
  "    P_BIRTH_MARKER_UNDERSCORE = '(?i)(?:^|[,.;:\\\\s])b_+\\\\s*(?=[A-Z])'",
  "  )",
  "  hits <- bind_rows(lapply(names(marker_rx), function(rule_id) {",
  "    loc <- str_locate(raw_text, regex(marker_rx[[rule_id]]))",
  "    if (is.na(loc[1, 1])) return(NULL)",
  "    tibble(rule_id = rule_id, start = loc[1, 1], end = loc[1, 2])",
  "  }))",
  "  if (!nrow(hits)) {",
  "    return(tibble(birthplace = '', birth_date = '', rule_id = ''))",
  "  }",
  "  marker <- hits |> arrange(start) |> slice(1)",
  "  after_marker <- str_sub(raw_text, marker$end[[1]] + 1)",
  "  date <- amws_find_first_date(after_marker)",
  "  if (!nzchar(date$date[[1]])) {",
  "    return(tibble(birthplace = '', birth_date = '', rule_id = ''))",
  "  }",
  "  place <- str_sub(after_marker, 1, date$start[[1]] - 1) |> amws_strip_edge_punct()",
  "  tibble(birthplace = place, birth_date = date$date[[1]], rule_id = marker$rule_id[[1]])",
  "}",
  "",
  "extract_birth_date_swallowed_in_place <- function(current_birthplace) {",
  "  date <- amws_find_first_date(current_birthplace)",
  "  if (!nzchar(date$date[[1]])) {",
  "    return(tibble(birthplace = '', birth_date = '', rule_id = ''))",
  "  }",
  "  tibble(birthplace = str_sub(current_birthplace, 1, date$start[[1]] - 1) |> amws_strip_edge_punct(),",
  "         birth_date = date$date[[1]],",
  "         rule_id = 'P_BIRTH_DATE_SWALLOWED_IN_PLACE')",
  "}",
  "",
  "extract_field_after_noisy_demographics <- function(raw_text, birth_date) {",
  "  raw_text <- amws_normalize_text(raw_text)",
  "  loc <- str_locate(raw_text, fixed(birth_date))",
  "  if (is.na(loc[1, 1])) return(tibble(field = '', rule_id = ''))",
  "  source <- str_sub(raw_text, loc[1, 2] + 1) |> amws_normalize_text()",
  "  old <- NA_character_",
  "  while (!identical(old, source)) {",
  "    old <- source",
  "    source <- source |>",
  "      str_replace(regex(\"^[,;:.\\\\s\\\"\\\\-_*]+\"), \"\") |>",
  "      str_replace(regex('^(?:US\\\\s+citizen|nat\\\\s+US)\\\\b[,;:.\\\\s-]*', ignore_case = TRUE), '') |>",
  "      str_replace(regex('^(?:wid|div|sep)\\\\b[,;:.\\\\s-]*', ignore_case = TRUE), '') |>",
  "      str_replace(regex('^(?:m|c)\\\\s*[0-9OISZLB]{0,4}[-]?[A-Za-z]?\\\\b[,;:.\\\\s-]*', ignore_case = TRUE), '') |>",
  "      str_replace(regex('^[A-Z]{1,3}\\\\s*[0-9]{1,2}\\\\s+'), '')",
  "  }",
  "  stop_loc <- str_locate(source, regex('\\\\b(Educ|Prof\\\\s+Exp|Concurrent\\\\s+Pos|Honors\\\\s*&\\\\s*Awards|Mem|Res|Mailing\\\\s*Add|Exp)\\\\s*[:;.!-]?', ignore_case = TRUE))",
  "  field <- if (!is.na(stop_loc[1, 1])) str_sub(source, 1, stop_loc[1, 1] - 1) else str_split(source, '\\\\.\\\\s+', n = 2)[[1]][1]",
  "  field <- field |> str_replace('\\\\\\\\B.*$', '') |> amws_strip_edge_punct() |> toupper()",
  "  field <- str_replace(field, regex(\"^A.{1,4}'(?=[A-Z])\"), \"\")",
  "  field <- str_replace(field, regex(\"^[A-Z][^A-Z]{1,4}[A-Z]'?\"), \"\")",
  "  field <- amws_strip_edge_punct(field)",
  "  if (!nzchar(field) || nchar(field) > 120 ||",
  "      str_detect(field, regex(\"\\\\b(Univ|PhD|Col|prof|res|Dept)\\\\b\", ignore_case = TRUE)) ||",
  "      str_detect(field, \"[0-9\\\\^?\\\\\\\\\\\\u25a0]\")) field <- ''",
  "  tibble(field = field, rule_id = ifelse(nzchar(field), 'P_FIELD_AFTER_DATE_DEMOGRAPHICS', ''))",
  "}",
  "",
  "extract_field_before_see_previous <- function(raw_text) {",
  "  prefix <- str_replace(raw_text, regex('see\\\\s+previous(?:\\\\s+edition)?.*$', ignore_case = TRUE), '')",
  "  field <- str_match(prefix, '[-,]\\\\s*([^,.;-]{4,90})\\\\s*,?\\\\s*$')[1, 2]",
  "  field <- amws_strip_edge_punct(field) |> toupper()",
  "  tibble(field = ifelse(nzchar(field), field, ''),",
  "         rule_id = ifelse(nzchar(field), 'P_FIELD_BEFORE_SEE_PREVIOUS', ''))",
  "}"
)

write_csv(audit_cases, cases_file)
write_csv(pattern_inventory, patterns_file)
write_csv(test_rows, tests_file)
writeLines(proposed_rules, rules_file, useBytes = TRUE)

case_counts <- audit_cases |>
  count(case_class, name = "n") |>
  arrange(desc(n))

recoverable_counts <- test_rows |>
  count(target_field, name = "n") |>
  arrange(target_field)

examples_true_missing <- audit_cases |>
  filter(case_class == "true_missing") |>
  select(global_lineid, lineid, evidence_text) |>
  head(6)

examples_recoverable <- audited |>
  filter(case_class == "recoverable_regex") |>
  transmute(
    global_lineid, lineid,
    proposed = paste(
      na.omit(c(
        ifelse(nzchar(proposed_birthplace), paste0("birthplace=", proposed_birthplace), NA),
        ifelse(nzchar(proposed_birth_date), paste0("birth_date=", proposed_birth_date), NA),
        ifelse(nzchar(proposed_field), paste0("field=", proposed_field), NA)
      )),
      collapse = "; "
    ),
    evidence_text
  ) |>
  head(10)

summary_lines <- c(
  "# AMWS Regex Gap Audit Summary",
  "",
  paste0("- Run id: `", run_id, "`"),
  paste0("- Parsed rows: ", nrow(parsed)),
  paste0("- Gap rows audited: ", nrow(audit_cases)),
  paste0("- Rows with missing birthplace: ", sum(audit_cases$missing_birthplace)),
  paste0("- Rows with missing birth_date: ", sum(audit_cases$missing_birth_date)),
  paste0("- Rows with missing field: ", sum(audit_cases$missing_field)),
  "",
  "## Case Classes",
  "",
  paste0("- ", case_counts$case_class, ": ", case_counts$n),
  "",
  "## Recoverable Values Proposed",
  "",
  if (nrow(recoverable_counts)) {
    paste0("- ", recoverable_counts$target_field, ": ", recoverable_counts$n)
  } else {
    "- No recoverable values proposed."
  },
  "",
  "## True Missing Examples",
  "",
  if (nrow(examples_true_missing)) {
    paste0(
      "- global_lineid ", examples_true_missing$global_lineid,
      " / lineid ", examples_true_missing$lineid,
      ": ", str_sub(examples_true_missing$evidence_text, 1, 180)
    )
  } else {
    "- None."
  },
  "",
  "## Recoverable Regex Examples",
  "",
  if (nrow(examples_recoverable)) {
    paste0(
      "- global_lineid ", examples_recoverable$global_lineid,
      " / lineid ", examples_recoverable$lineid,
      ": ", examples_recoverable$proposed,
      " | evidence: ", str_sub(examples_recoverable$evidence_text, 1, 180)
    )
  } else {
    "- None."
  },
  "",
  "## Remaining Manual/OCR/Segmentation Cases",
  "",
  paste0(
    "- Not recoverable by proposed regex without review: ",
    sum(audit_cases$case_class != "recoverable_regex")
  )
)

writeLines(summary_lines, summary_file, useBytes = TRUE)

if (nrow(audit_cases) != nrow(gap_rows)) {
  stop("Audit case count does not match gap row count.")
}
if (any(!nzchar(pattern_inventory$covered_lineids))) {
  stop("At least one proposed regex has no covered lineids.")
}
invisible(parse(rules_file))

cat("parsed input:", parsed_file, "\n")
cat("audit dir:", audit_dir, "\n")
cat("gap rows audited:", nrow(audit_cases), "\n")
cat("case class distribution:\n")
print(case_counts)
cat("\nrecoverable values proposed:\n")
print(recoverable_counts)
cat("\noutputs:\n")
cat(cases_file, "\n")
cat(patterns_file, "\n")
cat(tests_file, "\n")
cat(rules_file, "\n")
cat(summary_file, "\n")
