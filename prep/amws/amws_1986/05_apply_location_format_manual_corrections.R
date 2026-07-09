###############################################################################
# Apply AMWS Ed16 manual location-format corrections.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv
#   output/amws/regex_all_docs/manual_location_format_batches/
#     amws_ed16_location_format_manual_corrections.csv
#
# Writes:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv
#     amws_ed16_location_format_manual_applied_log.csv
#     amws_ed16_location_format_manual_apply_summary.csv
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
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
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

has_city_format_symbol <- function(x) {
  x <- normalize_text(x)
  ascii_symbol <- str_detect(
    x,
    "[[:cntrl:]<>\\^_*=+/\\\\|\\[\\]{}#@&%$~;:!?()]"
  )
  unicode_symbol <- str_detect(x, fixed(intToUtf8(0xFFFD))) |
    str_detect(x, fixed(intToUtf8(0x00C3))) |
    str_detect(x, fixed(intToUtf8(0x00C2))) |
    str_detect(x, fixed(intToUtf8(0x00AE))) |
    str_detect(x, fixed(intToUtf8(0x2122))) |
    str_detect(x, fixed(intToUtf8(0x00A9))) |
    str_detect(x, fixed(intToUtf8(0x2022)))
  nzchar(x) & (ascii_symbol | unicode_symbol)
}

birth_location_format_reasons <- function(birth_city, birth_state, birth_year) {
  city <- normalize_text(birth_city)
  state <- normalize_text(birth_state)
  year <- normalize_text(birth_year)
  year_int <- suppressWarnings(as.integer(year))

  city_bad_symbol <- has_city_format_symbol(city)
  city_has_digit <- nzchar(city) & str_detect(city, "[0-9]")
  city_has_separator_debris <- nzchar(city) & str_detect(city, "[,.]")
  city_has_section_text <- nzchar(city) & str_detect(
    city,
    regex(
      "\\b(Educ|Edue|Prof|Univ|Dept|Chemistry|Physics|Biology|Engineering|Medicine|Research|PHARMACEUTICS|SCIENCE|SYSTEM|SYSTEMS|MATH|MATHEMATICS|BIOCHEMISTRY|GENETICS|GEOLOGY|PATHOLOGY)\\b",
      ignore_case = TRUE
    )
  )
  city_has_date_marker <- nzchar(city) & str_detect(
    city,
    regex(
      "\\b(Jan|Feb|Mar|Apr|May|Jun|June|Jul|July|Aug|Sep|Sept|Oct|Nov|Dec|Dee|Dcc|Oet|Mav|Julv|Juiy)\\b",
      ignore_case = TRUE
    )
  )
  city_too_long <- nchar(city) > 45L
  city_bad_edge <- nzchar(city) &
    city != str_replace_all(city, "^[[:punct:] ]+|[[:punct:] ]+$", "")
  state_malformed <- nzchar(state) & !str_detect(state, "^[A-Z]{2}$")
  year_malformed <- nzchar(year) &
    (!str_detect(year, "^[0-9]{4}$") |
       is.na(year_int) |
       year_int < 1800L |
       year_int > 1986L)

  vapply(seq_along(city), function(i) {
    reasons <- c(
      if (city_bad_symbol[[i]]) "birth_city_symbols",
      if (city_has_digit[[i]]) "birth_city_digits",
      if (city_has_separator_debris[[i]]) "birth_city_punct_or_separator",
      if (city_has_section_text[[i]]) "birth_city_section_text",
      if (city_has_date_marker[[i]]) "birth_city_date_text",
      if (city_too_long[[i]]) "birth_city_too_long",
      if (city_bad_edge[[i]]) "birth_city_edge_punct",
      if (state_malformed[[i]]) "birth_state_not_clean_code",
      if (year_malformed[[i]]) "birth_year_not_plausible_4digit"
    )
    paste(reasons, collapse = "; ")
  }, character(1))
}

add_birth_location_format_flags <- function(data) {
  reasons <- birth_location_format_reasons(
    data$birth_city,
    data$birth_state,
    data$birth_year
  )
  data |>
    mutate(
      birth_location_format_problem = nzchar(reasons),
      birth_location_format_problem_reason = reasons
    )
}

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_LOCATION_FORMAT_OUTPUT_DIR",
                      default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input_file <- env_chr(
  "AMWS_ED16_LOCATION_FORMAT_INPUT_FILE",
  file.path(output_dir,
            "amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv")
)
input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

batch_root <- file.path(output_dir, "manual_location_format_batches")
corrections_file <- env_chr(
  "AMWS_ED16_LOCATION_FORMAT_CORRECTIONS_FILE",
  file.path(batch_root, "amws_ed16_location_format_manual_corrections.csv")
)
corrections_file <- normalizePath(corrections_file, winslash = "/",
                                  mustWork = TRUE)

applied_log_csv <- file.path(output_dir,
                             "amws_ed16_location_format_manual_applied_log.csv")
summary_csv <- file.path(output_dir,
                         "amws_ed16_location_format_manual_apply_summary.csv")

csv_text_cols <- cols(.default = col_character())
input <- read_csv(input_file, col_types = csv_text_cols,
                  show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
corrections <- read_csv(corrections_file, col_types = csv_text_cols,
                        show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_input_cols <- c("doc_id", "lineid", "birth_city", "birth_state",
                         "birth_year")
missing_input <- setdiff(required_input_cols, names(input))
if (length(missing_input)) {
  stop("Input missing required columns: ", paste(missing_input, collapse = ", "))
}

required_correction_cols <- c(
  "doc_id", "lineid", "birth_city_new", "birth_state_new", "birth_year_new",
  "manual_action", "manual_confidence", "manual_note"
)
missing_corrections <- setdiff(required_correction_cols, names(corrections))
if (length(missing_corrections)) {
  stop("Corrections missing required columns: ",
       paste(missing_corrections, collapse = ", "))
}

if (n_distinct(paste(input$doc_id, input$lineid)) != nrow(input)) {
  stop("Input has duplicated doc_id + lineid.")
}
if (n_distinct(paste(corrections$doc_id, corrections$lineid)) !=
    nrow(corrections)) {
  stop("Corrections have duplicated doc_id + lineid.")
}

to_apply <- corrections |>
  filter(manual_action == "correct",
         manual_confidence %in% c("high", "medium"))

unmatched <- anti_join(to_apply, input |> select(doc_id, lineid),
                       by = c("doc_id", "lineid"))
if (nrow(unmatched)) {
  stop("Corrections contain keys not present in input: ", nrow(unmatched))
}

bad_year <- to_apply |>
  filter(nzchar(birth_year_new),
         !str_detect(birth_year_new, "^[0-9]{4}$") |
           as.integer(birth_year_new) < 1800L |
           as.integer(birth_year_new) > 1986L)
if (nrow(bad_year)) {
  stop("Corrections include malformed birth_year_new rows: ", nrow(bad_year))
}

bad_state <- to_apply |>
  filter(nzchar(birth_state_new),
         !str_detect(birth_state_new, "^[A-Z]{2}$"))
if (nrow(bad_state)) {
  stop("Corrections include malformed birth_state_new rows: ", nrow(bad_state))
}

corrected <- input
idx <- match(paste(to_apply$doc_id, to_apply$lineid),
             paste(corrected$doc_id, corrected$lineid))
corrected$birth_city[idx] <- to_apply$birth_city_new
corrected$birth_state[idx] <- to_apply$birth_state_new
corrected$birth_year[idx] <- to_apply$birth_year_new
corrected <- add_birth_location_format_flags(corrected)

if (nrow(corrected) != nrow(input)) {
  stop("Corrected row count changed.")
}
if (!identical(paste(corrected$doc_id, corrected$lineid),
               paste(input$doc_id, input$lineid))) {
  stop("Corrected output changed row order or keys.")
}

applied_log <- to_apply |>
  left_join(input |>
              select(doc_id, lineid,
                     birth_city_old_actual = birth_city,
                     birth_state_old_actual = birth_state,
                     birth_year_old_actual = birth_year),
            by = c("doc_id", "lineid")) |>
  mutate(
    changed_birth_city = normalize_text(birth_city_old_actual) !=
      normalize_text(birth_city_new),
    changed_birth_state = normalize_text(birth_state_old_actual) !=
      normalize_text(birth_state_new),
    changed_birth_year = normalize_text(birth_year_old_actual) !=
      normalize_text(birth_year_new)
  )

summary <- bind_rows(
  tibble(metric = "input_rows", value = nrow(input)),
  tibble(metric = "correction_rows", value = nrow(corrections)),
  tibble(metric = "applied_rows", value = nrow(to_apply)),
  tibble(metric = "manual_action:correct",
         value = sum(corrections$manual_action == "correct")),
  tibble(metric = "manual_action:review_unclear",
         value = sum(corrections$manual_action == "review_unclear")),
  tibble(metric = "manual_action:no_change",
         value = sum(corrections$manual_action == "no_change")),
  tibble(metric = "changed_birth_city",
         value = sum(applied_log$changed_birth_city)),
  tibble(metric = "changed_birth_state",
         value = sum(applied_log$changed_birth_state)),
  tibble(metric = "changed_birth_year",
         value = sum(applied_log$changed_birth_year)),
  tibble(metric = "remaining_birth_location_format_problem",
         value = sum(corrected$birth_location_format_problem)),
  tibble(metric = "remaining_birth_city_format_problem",
         value = sum(str_detect(corrected$birth_location_format_problem_reason,
                                "birth_city_"))),
  tibble(metric = "remaining_birth_state_format_problem",
         value = sum(str_detect(corrected$birth_location_format_problem_reason,
                                "birth_state_"))),
  tibble(metric = "remaining_birth_year_format_problem",
         value = sum(str_detect(corrected$birth_location_format_problem_reason,
                                "birth_year_")))
) |>
  mutate(value = as.numeric(value))

readr::write_excel_csv(corrected, input_file, na = "")
readr::write_excel_csv(applied_log, applied_log_csv, na = "")
readr::write_excel_csv(summary, summary_csv, na = "")

cat("Applied rows:", nrow(to_apply), "\n")
cat("Remaining format problems:",
    sum(corrected$birth_location_format_problem), "\n")
cat("Updated output:", input_file, "\n")
cat("Wrote applied log:", applied_log_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
