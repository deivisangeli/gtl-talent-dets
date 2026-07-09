###############################################################################
# Pilot OpenAI API curation for remaining AMWS Ed16 expanded rows.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_expanded_birth_place_regex_rule_corrections.csv
#
# Writes:
#   output/amws/regex_all_docs/manual_birth_place_gpt_nano_pilot/
#     gpt_nano_pilot_input_50.csv
#     gpt_nano_pilot_output_50_raw.jsonl
#     gpt_nano_pilot_output_50.csv
#     gpt_nano_pilot_summary.csv
#     gpt_nano_pilot_errors.csv
#
# Environment overrides:
#   AMWS_GPT_NANO_MODEL
#   AMWS_GPT_NANO_SAMPLE_N
#   AMWS_GPT_NANO_SAMPLE_FILE
#   AMWS_GPT_NANO_SEED
#   AMWS_GPT_NANO_WORKERS
#   AMWS_GPT_NANO_OUTPUT_DIR
#   AMWS_GPT_NANO_OUTPUT_PREFIX
#   AMWS_GPT_NANO_RENVIRON
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(furrr)
  library(future)
  library(httr2)
  library(jsonlite)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
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
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
}

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed <= 0L) {
    stop("Environment variable ", name, " must be a positive integer; got: ", value)
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

scalar_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) default else as.character(x[[1]])
}

scalar_int <- function(x, default = NA_integer_) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) default else as.integer(x[[1]])
}

parse_birth_year <- function(birth_date) {
  x <- normalize_text(birth_date)
  if (!nzchar(x)) return("")
  token <- str_match(x, "\\b[A-Za-z]{3,9}\\.?\\s+[0-9?]{1,2}[, .]+([0-9]{2,4})\\D*$")[, 2]
  if (is.na(token) || !nzchar(token)) return("")
  if (nchar(token) == 2L) {
    yy <- as.integer(token)
    year <- ifelse(yy <= 86L, 1900L + yy, 1800L + yy)
  } else if (nchar(token) == 4L) {
    year <- as.integer(token)
  } else {
    return("")
  }
  if (is.na(year) || year < 1800L || year > 1986L) "" else as.character(year)
}

city_from_place <- function(place) {
  place <- normalize_text(place)
  if (!nzchar(place)) return("")
  if (str_detect(place, ",")) {
    normalize_text(str_split_fixed(place, ",", 2)[, 1])
  } else if (str_detect(place, "\\.\\s*[A-Za-z][A-Za-z .]{1,20}\\.?$")) {
    normalize_text(str_split_fixed(place, "\\.", 2)[, 1])
  } else {
    normalize_text(place)
  }
}

ensure_col <- function(data, col, default = "") {
  if (!col %in% names(data)) data[[col]] <- default
  data
}

pricing_for_model <- function(model) {
  if (model == "gpt-5.4-mini") return(list(input = 0.75, output = 4.50))
  if (model == "gpt-5.4-nano") return(list(input = 0.20, output = 1.25))
  if (model == "gpt-5-mini") return(list(input = 0.25, output = 2.00))
  if (model == "gpt-5-nano") return(list(input = 0.05, output = 0.40))
  list(input = NA_real_, output = NA_real_)
}

default_renviron <- file.path(Sys.getenv("USERPROFILE"),
                              "OneDrive", "Documentos", ".Renviron")
renviron_file <- env_chr("AMWS_GPT_NANO_RENVIRON", default_renviron)
if (file.exists(renviron_file)) {
  readRenviron(renviron_file)
}

api_key <- env_chr("OPENAI_API_KEY", env_chr("chatGPT_API_KEY"))
if (!nzchar(api_key)) {
  stop("No API key found. Expected OPENAI_API_KEY or chatGPT_API_KEY after reading: ",
       renviron_file)
}

model <- env_chr("AMWS_GPT_NANO_MODEL", "gpt-5.4-nano")
sample_n <- env_int("AMWS_GPT_NANO_SAMPLE_N", 50L)
seed <- env_int("AMWS_GPT_NANO_SEED", 20260706L)
workers <- env_int("AMWS_GPT_NANO_WORKERS", 20L)
sample_file <- env_chr("AMWS_GPT_NANO_SAMPLE_FILE")
output_prefix <- env_chr("AMWS_GPT_NANO_OUTPUT_PREFIX", "gpt_nano_pilot")

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
input_log_csv <- file.path(
  default_output_dir,
  "amws_ed16_expanded_birth_place_regex_rule_corrections.csv"
)
input_log_csv <- normalizePath(input_log_csv, winslash = "/", mustWork = TRUE)

pilot_dir <- env_chr(
  "AMWS_GPT_NANO_OUTPUT_DIR",
  file.path(default_output_dir, "manual_birth_place_gpt_nano_pilot")
)
dir.create(pilot_dir, recursive = TRUE, showWarnings = FALSE)
pilot_dir <- normalizePath(pilot_dir, winslash = "/", mustWork = TRUE)

input_csv <- file.path(pilot_dir, sprintf("%s_input_%d.csv", output_prefix, sample_n))
raw_jsonl <- file.path(pilot_dir, sprintf("%s_output_%d_raw.jsonl", output_prefix, sample_n))
output_csv <- file.path(pilot_dir, sprintf("%s_output_%d.csv", output_prefix, sample_n))
summary_csv <- file.path(pilot_dir, sprintf("%s_summary.csv", output_prefix))
errors_csv <- file.path(pilot_dir, sprintf("%s_errors.csv", output_prefix))
comparison_csv <- file.path(pilot_dir, sprintf("%s_vs_manual_classification_summary.csv",
                                               output_prefix))

source_rows <- read_csv(input_log_csv, col_types = cols(.default = col_character()),
                        show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

remaining <- source_rows |>
  filter(regex_apply != "TRUE") |>
  arrange(doc_id, suppressWarnings(as.integer(lineid)))

if (nrow(remaining) < sample_n) {
  stop("Requested ", sample_n, " rows, but only ", nrow(remaining), " remain.")
}

if (nzchar(sample_file)) {
  sample_file <- normalizePath(sample_file, winslash = "/", mustWork = TRUE)
  pilot_input <- read_csv(sample_file, col_types = cols(.default = col_character()),
                          show_col_types = FALSE) |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
  sample_n <- nrow(pilot_input)
} else {
  set.seed(seed)
  pilot_input <- remaining |>
    slice_sample(n = sample_n)
}

for (col in c("source_lineid", "entry_instance", "manual_target_reason",
              "has_dee_date_in_birth_place", "birth_place_old",
              "birth_date_old", "birth_year_old", "birth_city_old",
              "field_old", "regex_rule_id", "raw_text_adjusted",
              "likely_manual_correctable", "reason", "review_note")) {
  pilot_input <- ensure_col(pilot_input, col)
}

pilot_input <- pilot_input |>
  arrange(doc_id, suppressWarnings(as.integer(lineid))) |>
  mutate(
    gpt_pilot_id = if ("review_id" %in% names(pilot_input) &&
                       all(nzchar(review_id))) review_id else as.character(row_number())
  ) |>
  select(gpt_pilot_id, any_of("review_id"), doc_id, lineid, source_lineid,
         entry_instance, manual_target_reason, birth_place_word_n,
         has_dee_date_in_birth_place, birth_place_old, birth_date_old,
         birth_year_old, birth_city_old, field_old, regex_rule_id,
         raw_text_adjusted, likely_manual_correctable, reason, review_note)

if (nrow(pilot_input) != sample_n) {
  stop("Pilot input size mismatch.")
}
if (n_distinct(paste(pilot_input$doc_id, pilot_input$lineid)) != nrow(pilot_input)) {
  stop("Pilot input has duplicate doc_id + lineid keys.")
}

write_excel_csv(pilot_input, input_csv, na = "")

system_prompt <- paste(
  "You curate American Men and Women of Science OCR parser rows.",
  "Use only the visible text in raw_text_adjusted and the old parser values provided for this row.",
  "Do not use external biographical knowledge. Do not use nearby rows unless they are part of the same raw_text_adjusted string.",
  "",
  "Task: extract or correct birth_place_new, birth_date_new, birth_year_new, birth_city_new, field_new, manual_action, manual_confidence, and manual_note.",
  "",
  "Clean AMWS entry structure:",
  "A normal entry often looks like: NAME, b BIRTH_PLACE, BIRTH_DATE; demographic markers. FIELD. Educ: ...",
  "Birthplace starts immediately after the birth marker 'b' and ends before the birth date, demographic markers, field text, or section headers.",
  "Birth date usually follows birthplace and usually has month + day + year, such as 'Jan 6, 18', 'Oct 18, 38', or 'May 28 39'.",
  "Field is the discipline phrase after birthplace/date/demographic markers and before section headers such as Educ, Edue, Prof Exp, Mem, Res, or Mailing Add.",
  "Demographic/status markers are not field: m 43, c 3, US citizen, nat US, Can citizen, div, wid.",
  "Education, degrees, institutions, jobs, addresses, memberships, and research descriptions are not field.",
  "",
  "Clean examples:",
  "Example A: BARD, ENZO, b Luis Palacios, Santa Fe, Argentina, Oct 18, 38m 65r MOLECULAR BIOLOGY, MICROBIOLOGY. Educ:",
  "=> birth_place_new='Luis Palacios, Santa Fe, Argentina'; birth_date_new='Oct 18, 38'; birth_year_new='1938'; birth_city_new='Luis Palacios'; field_new='MOLECULAR BIOLOGY, MICROBIOLOGY'.",
  "Example B: MCNULTY, IRVING BAZIL, b Salt Lake City, Utah, Jan 6, 18; m 43; c 3. PLANT PHYSIOLOGY. Educ:",
  "=> birth_place_new='Salt Lake City, Utah'; birth_date_new='Jan 6, 18'; birth_year_new='1918'; birth_city_new='Salt Lake City'; field_new='PLANT PHYSIOLOGY'.",
  "Example C: EDINGTON, JEFFREY WILLIAM, b Newcastle upon Tyne, Eng, May 28 39. MATERIALS SCIENCE. Educ:",
  "=> birth_place_new='Newcastle upon Tyne, Eng'; birth_date_new='May 28, 39'; birth_year_new='1939'; birth_city_new='Newcastle upon Tyne'; field_new='MATERIALS SCIENCE'.",
  "",
  "Common OCR/date problems:",
  "Dee or Dcc can mean Dec only when followed by a date number. Oet can mean Oct only when followed by a date number.",
  "Mav can mean May only when clearly a month. Juiy or Julv can mean July only when clearly a month.",
  "No, before a date may be OCR for Nov only if the date context is clear.",
  "111 can mean Ill only when clearly Illinois, such as East St Louis, 111.",
  "Do not normalize OCR if the correction is not directly supported by the row.",
  "",
  "Common parsing failures to fix:",
  "If birthplace includes field, such as 'Mexico City, Mex. CIVIL ENGINEERING. Educ:', set birth_place_new='Mexico City, Mex' and field_new='CIVIL ENGINEERING'.",
  "If birthplace includes date and field, such as 'Dallas. Tea. No, 8. 42 m 68. c 3 CHEMICAL ENGINEERING. Educ:', extract field_new='CHEMICAL ENGINEERING' but use review_unclear if the place or month cannot be safely interpreted.",
  "If date is glued to a demographic marker, such as 'Oct 18, 38m 65r MOLECULAR BIOLOGY, MICROBIOLOGY. Educ:', use birth_date_new='Oct 18, 38' and field_new='MOLECULAR BIOLOGY, MICROBIOLOGY'.",
  "If field is before Educ, such as 'BIOCHEMISTRY. Educ:', use field_new='BIOCHEMISTRY'.",
  "If raw_text_adjusted contains another ' b ' later, extract only the entry matching the provided old parser values and current doc_id/lineid context. If you cannot identify the target entry, use review_unclear.",
  "",
  "Conservative correction rules:",
  "Use correct only when birth_place_new is a clean, defensible place string directly visible in raw_text_adjusted.",
  "Do not use correct if birth_place_new would contain OCR debris, symbols, partial words, education text, field text, address text, institutional text, or uncertain fragments.",
  "Do not treat a single corrupted token such as 'Kam' or 'Onevt' as a safe birthplace. Use review_unclear.",
  "Do not use correct only because field_new is recoverable. If no reliable birthplace can be recovered and birth_place_old is wrong, use review_unclear.",
  "If birth_place is clearly recoverable but birth_date or field is corrupted, correct the clean birth_place and city, leave unrecoverable fields blank, and explain the blank fields in manual_note.",
  "Do not extract field_new from Res, Mem, Mailing Add, Prof Exp, employment titles, society memberships, grants, awards, or research descriptions.",
  "If multiple entries are visible and the target entry cannot be aligned to the old parser values, use review_unclear instead of correcting a clearer neighboring entry.",
  "",
  "City rule: derive birth_city_new from birth_place_new. If birth_place_new has commas, city is before the first comma. If it uses period separators, city is the location before the state/country suffix.",
  "Preserve multiword cities: Salt Lake City, New York, East St Louis, Los Angeles, Newcastle upon Tyne, White River Junction.",
  "",
  "Year rule: derive birth_year_new from birth_date_new when a year is visible. Two-digit years 00-86 map to 1900-1986; 87-99 map to 1887-1899. Four-digit years stay four-digit if plausible. Leave blank if no year is visible.",
  "",
  "manual_action:",
  "Use correct only when the corrected birthplace and field are clearly supported by raw_text_adjusted. Date/year may be blank if no date/year is visible; explain that in manual_note.",
  "Use no_change only when all old parser values are already acceptable, especially birth_place_old is a clean birthplace and not field/education text.",
  "Use review_unclear when OCR corruption, missing separators, multiple entries, or row alignment make correction unsafe.",
  "If only field is visible but birthplace/date are missing or birth_place_old is wrong, use review_unclear, not no_change.",
  "",
  "manual_confidence: high means all corrected values are directly visible and internally consistent; medium means correction is probably right but OCR punctuation/spelling is imperfect; low is only for review_unclear or very uncertain partial evidence.",
  "Never return manual_action='correct' with manual_confidence='low'. If confidence is low, use manual_action='review_unclear'.",
  "For review_unclear rows, leave uncertain corrected fields blank. Include short visible partials only if useful for review. Explain the blocker in manual_note.",
  "Return JSON only, conforming exactly to the provided schema.",
  sep = "\n"
)

json_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = list("birth_place_new", "birth_date_new", "birth_year_new",
                  "birth_city_new", "field_new", "manual_action",
                  "manual_confidence", "manual_note"),
  properties = list(
    birth_place_new = list(type = "string"),
    birth_date_new = list(type = "string"),
    birth_year_new = list(type = "string"),
    birth_city_new = list(type = "string"),
    field_new = list(type = "string"),
    manual_action = list(type = "string",
                         enum = list("correct", "no_change", "review_unclear")),
    manual_confidence = list(type = "string",
                             enum = list("high", "medium", "low")),
    manual_note = list(type = "string")
  )
)

build_user_prompt <- function(row) {
  paste0(
    "Review this single AMWS row and return one JSON object.\n\n",
    "Use the old parser values to identify which part of raw_text_adjusted corresponds to this row.\n",
    "Do not correct a different person if raw_text_adjusted contains multiple entries.\n\n",
    "Keys:\n",
    "gpt_pilot_id: ", row$gpt_pilot_id, "\n",
    "doc_id: ", row$doc_id, "\n",
    "lineid: ", row$lineid, "\n\n",
    "Old parser values:\n",
    "birth_place_old: ", row$birth_place_old, "\n",
    "birth_date_old: ", row$birth_date_old, "\n",
    "birth_year_old: ", row$birth_year_old, "\n",
    "birth_city_old: ", row$birth_city_old, "\n",
    "field_old: ", row$field_old, "\n\n",
    "raw_text_adjusted:\n",
    row$raw_text_adjusted
  )
}

extract_output_text <- function(parsed) {
  direct <- parsed$output_text
  if (!is.null(direct) && length(direct) && nzchar(as.character(direct[[1]]))) {
    return(as.character(direct[[1]]))
  }
  output <- parsed$output
  if (is.null(output) || !length(output)) return("")
  for (item in output) {
    content <- item$content
    if (is.null(content) || !length(content)) next
    for (part in content) {
      text <- part$text
      if (!is.null(text) && length(text) && nzchar(as.character(text[[1]]))) {
        return(as.character(text[[1]]))
      }
    }
  }
  ""
}

api_parse_one <- function(row) {
  start_time <- Sys.time()
  request_body <- list(
    model = model,
    input = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = build_user_prompt(row))
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "amws_manual_correction",
        strict = TRUE,
        schema = json_schema
      )
    )
  )

  tryCatch({
    response <- request("https://api.openai.com/v1/responses") |>
      req_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(request_body, auto_unbox = TRUE, null = "null") |>
      req_timeout(180) |>
      req_retry(max_tries = 4) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()

    status <- resp_status(response)
    body_text <- resp_body_string(response)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    parsed <- fromJSON(body_text, simplifyVector = FALSE)

    if (status >= 300L) {
      message <- scalar_chr(parsed$error$message, body_text)
      return(list(
        ok = FALSE, status = status, elapsed = elapsed, parsed = parsed,
        result = NULL, error = message
      ))
    }

    output_text <- extract_output_text(parsed)
    if (!nzchar(output_text)) {
      return(list(
        ok = FALSE, status = status, elapsed = elapsed, parsed = parsed,
        result = NULL, error = "API response did not contain output text."
      ))
    }

    result <- fromJSON(output_text, simplifyVector = FALSE)
    list(ok = TRUE, status = status, elapsed = elapsed, parsed = parsed,
         result = result, error = "")
  }, error = function(e) {
    list(ok = FALSE, status = NA_integer_,
         elapsed = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
         parsed = NULL, result = NULL, error = conditionMessage(e))
  })
}

old_plan <- plan()
on.exit(plan(old_plan), add = TRUE)
plan(multisession, workers = workers)

rows <- split(pilot_input, seq_len(nrow(pilot_input)))
api_results <- future_map(rows, api_parse_one,
                          .options = furrr_options(seed = TRUE))

raw_lines <- map2_chr(seq_along(api_results), api_results, function(i, res) {
  row <- pilot_input[i, , drop = FALSE]
  toJSON(list(
    gpt_pilot_id = row$gpt_pilot_id,
    doc_id = row$doc_id,
    lineid = row$lineid,
    ok = res$ok,
    status = res$status,
    error = res$error,
    api_response = res$parsed
  ), auto_unbox = TRUE, null = "null")
})
writeLines(raw_lines, raw_jsonl, useBytes = TRUE)

allowed_actions <- c("correct", "no_change", "review_unclear")
allowed_conf <- c("high", "medium", "low")

output <- map2_dfr(seq_along(api_results), api_results, function(i, res) {
  row <- pilot_input[i, , drop = FALSE]
  usage <- res$parsed$usage
  result <- res$result
  if (is.null(result)) result <- list()

  out <- tibble(
    gpt_pilot_id = row$gpt_pilot_id,
    doc_id = row$doc_id,
    lineid = row$lineid,
    source_lineid = row$source_lineid,
    entry_instance = row$entry_instance,
    birth_place_old = row$birth_place_old,
    birth_date_old = row$birth_date_old,
    birth_year_old = row$birth_year_old,
    birth_city_old = row$birth_city_old,
    field_old = row$field_old,
    likely_manual_correctable = row$likely_manual_correctable,
    manual_classification_reason = row$reason,
    manual_classification_note = row$review_note,
    raw_text_adjusted = row$raw_text_adjusted,
    birth_place_new = scalar_chr(result$birth_place_new),
    birth_date_new = scalar_chr(result$birth_date_new),
    birth_year_new = scalar_chr(result$birth_year_new),
    birth_city_new = scalar_chr(result$birth_city_new),
    field_new = scalar_chr(result$field_new),
    manual_action = scalar_chr(result$manual_action),
    manual_confidence = scalar_chr(result$manual_confidence),
    manual_note = scalar_chr(result$manual_note),
    api_model = model,
    api_ok = res$ok,
    api_status = scalar_chr(res$status),
    api_elapsed_sec = res$elapsed,
    input_tokens = scalar_int(usage$input_tokens),
    output_tokens = scalar_int(usage$output_tokens),
    total_tokens = scalar_int(usage$total_tokens),
    api_error = scalar_chr(res$error)
  )

  schema_ok <- out$api_ok &&
    out$manual_action %in% allowed_actions &&
    out$manual_confidence %in% allowed_conf
  city_expected <- city_from_place(out$birth_place_new)
  year_expected <- parse_birth_year(out$birth_date_new)
  out |>
    mutate(
      schema_ok = schema_ok,
      birth_city_expected_from_place = city_expected,
      birth_city_consistent = manual_action == "review_unclear" ||
        !nzchar(birth_place_new) ||
        normalize_text(birth_city_new) == normalize_text(city_expected),
      birth_year_expected_from_date = year_expected,
      birth_year_consistent = manual_action == "review_unclear" ||
        !nzchar(birth_date_new) ||
        !nzchar(year_expected) ||
        normalize_text(birth_year_new) == normalize_text(year_expected)
    )
})

write_excel_csv(output, output_csv, na = "")

errors <- output |>
  filter(!api_ok | !schema_ok | !birth_city_consistent | !birth_year_consistent)
write_excel_csv(errors, errors_csv, na = "")

pricing <- pricing_for_model(model)
pricing_input_per_m <- pricing$input
pricing_output_per_m <- pricing$output
total_input_tokens <- sum(output$input_tokens, na.rm = TRUE)
total_output_tokens <- sum(output$output_tokens, na.rm = TRUE)
estimated_cost_usd <- total_input_tokens / 1e6 * pricing_input_per_m +
  total_output_tokens / 1e6 * pricing_output_per_m

metric_row <- function(metric, value) {
  tibble(metric = metric, value = as.character(value))
}

summary <- bind_rows(
  metric_row("model", model),
  metric_row("sample_n", nrow(output)),
  metric_row("workers", workers),
  metric_row("api_ok_rows", sum(output$api_ok)),
  metric_row("schema_ok_rows", sum(output$schema_ok)),
  metric_row("error_rows", nrow(errors)),
  output |> count(manual_action, name = "value") |>
    transmute(metric = paste0("manual_action:", manual_action),
              value = as.character(value)),
  output |> count(manual_confidence, name = "value") |>
    transmute(metric = paste0("manual_confidence:", manual_confidence),
              value = as.character(value)),
  metric_row("birth_place_new_nonempty", sum(nzchar(output$birth_place_new))),
  metric_row("birth_date_new_nonempty", sum(nzchar(output$birth_date_new))),
  metric_row("birth_year_new_nonempty", sum(nzchar(output$birth_year_new))),
  metric_row("birth_city_new_nonempty", sum(nzchar(output$birth_city_new))),
  metric_row("field_new_nonempty", sum(nzchar(output$field_new))),
  metric_row("input_tokens", total_input_tokens),
  metric_row("output_tokens", total_output_tokens),
  metric_row("total_tokens", sum(output$total_tokens, na.rm = TRUE)),
  metric_row("pricing_input_per_1m", pricing_input_per_m),
  metric_row("pricing_output_per_1m", pricing_output_per_m),
  metric_row("estimated_standard_cost_usd", sprintf("%.6f", estimated_cost_usd)),
  metric_row("input_file", input_csv),
  metric_row("output_file", output_csv),
  metric_row("raw_jsonl_file", raw_jsonl),
  metric_row("errors_file", errors_csv)
)
write_excel_csv(summary, summary_csv, na = "")

if ("likely_manual_correctable" %in% names(output) &&
    any(nzchar(output$likely_manual_correctable))) {
  comparison <- bind_rows(
    output |>
      count(likely_manual_correctable, manual_action, name = "value") |>
      transmute(metric = paste0("manual_correctable:", likely_manual_correctable,
                                ":action:", manual_action),
                value = as.character(value)),
    metric_row("manual_correctable_yes",
               sum(output$likely_manual_correctable == "yes")),
    metric_row("manual_correctable_no",
               sum(output$likely_manual_correctable == "no")),
    metric_row("correct_among_manual_correctable_yes",
               sum(output$likely_manual_correctable == "yes" &
                     output$manual_action == "correct")),
    metric_row("review_unclear_among_manual_correctable_no",
               sum(output$likely_manual_correctable == "no" &
                     output$manual_action == "review_unclear")),
    metric_row("potential_false_positive_correct_when_manual_no",
               sum(output$likely_manual_correctable == "no" &
                     output$manual_action == "correct")),
    metric_row("potential_false_negative_unclear_when_manual_yes",
               sum(output$likely_manual_correctable == "yes" &
                     output$manual_action == "review_unclear"))
  )
  write_excel_csv(comparison, comparison_csv, na = "")
}

cat("Pilot input:", input_csv, "\n")
cat("Pilot output:", output_csv, "\n")
cat("Raw JSONL:", raw_jsonl, "\n")
cat("Summary:", summary_csv, "\n")
cat("Errors:", errors_csv, "\n")
cat("Rows:", nrow(output), "\n")
cat("API OK rows:", sum(output$api_ok), "\n")
cat("Schema OK rows:", sum(output$schema_ok), "\n")
cat("Error rows:", nrow(errors), "\n")
cat("Input tokens:", total_input_tokens, "\n")
cat("Output tokens:", total_output_tokens, "\n")
cat("Estimated standard cost USD:", sprintf("%.6f", estimated_cost_usd), "\n")
if (file.exists(comparison_csv)) {
  cat("Comparison:", comparison_csv, "\n")
}
