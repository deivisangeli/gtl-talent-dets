###############################################################################
# Prepare a reproducible pilot for manual AMWS 1986 birth-city OCR corrections.
#
# Reads:
#   Data/intermediary/amws/amws_ed86_full.csv
#
# Writes (without modifying the input):
#   Data/intermediary/amws/manual_bad_ocr_birth_city_pilot_20260710/
#     amws_ed86_bad_ocr_sample100_master.csv
#     amws_ed86_bad_ocr_sample100_prep_summary.csv
#     in/amws_ed86_bad_ocr_batch_01.csv
#     in/amws_ed86_bad_ocr_batch_02.csv
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
}

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_int <- function(name, default) {
  value <- env_chr(name, as.character(default))
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed)) stop(name, " must be an integer; received: ", value)
  parsed
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | str_to_upper(str_trim(x)) == "NA", "", x)
}

normalize_text <- function(x) {
  blank_na(x) |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

# Letters, whitespace, and ordinary place-name punctuation are allowed by the
# historical default.  Later review rounds can supply a stricter pattern while
# retaining the same auditable batch contract.
suspicious_pattern <- env_chr(
  "AMWS_ED86_BAD_OCR_SUSPICIOUS_PATTERN",
  "[^\\p{L}\\s.,'’‘()&/\\-]"
)

has_suspicious_birth_city_character <- function(x) {
  city <- normalize_text(x)
  nzchar(city) & str_detect(city, suspicious_pattern)
}

extract_suspicious_characters <- function(x) {
  chars <- str_extract_all(normalize_text(x), suspicious_pattern)[[1]]
  if (!length(chars)) return("")
  paste(unique(chars), collapse = " | ")
}

data_dir <- file.path(TALENT_DETS_DATA_DIR, "Data")
input_file <- env_chr(
  "AMWS_ED86_BAD_OCR_PILOT_INPUT_FILE",
  file.path(data_dir, "intermediary", "amws", "amws_ed86_full.csv")
)
input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

sample_n <- env_int("AMWS_ED86_BAD_OCR_PILOT_SAMPLE_N", 100L)
random_seed <- env_int("AMWS_ED86_BAD_OCR_PILOT_SEED", 20260710L)
expected_eligible_n <- env_int("AMWS_ED86_BAD_OCR_EXPECTED_ELIGIBLE_N", 1342L)
pilot_id_start <- env_int("AMWS_ED86_BAD_OCR_PILOT_ID_START", 1L)
batch_id_start <- env_int("AMWS_ED86_BAD_OCR_PILOT_BATCH_ID_START", 1L)
batch_size <- env_int("AMWS_ED86_BAD_OCR_PILOT_BATCH_SIZE", 50L)
pilot_label <- env_chr("AMWS_ED86_BAD_OCR_PILOT_LABEL", "sample100")
exclude_file <- env_chr("AMWS_ED86_BAD_OCR_PILOT_EXCLUDE_FILE", "")
if (sample_n < 1L || batch_size < 1L || pilot_id_start < 1L ||
    batch_id_start < 1L) {
  stop("Sample, batch, and starting IDs must be positive integers.")
}

pilot_root <- env_chr(
  "AMWS_ED86_BAD_OCR_PILOT_DIR",
  file.path(data_dir, "intermediary", "amws",
            "manual_bad_ocr_birth_city_pilot_20260710")
)
in_dir <- file.path(pilot_root, "in")
out_dir <- file.path(pilot_root, "out")
dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

master_csv <- file.path(
  pilot_root,
  paste0("amws_ed86_bad_ocr_", pilot_label, "_master.csv")
)
summary_csv <- file.path(
  pilot_root,
  paste0("amws_ed86_bad_ocr_", pilot_label, "_prep_summary.csv")
)

csv_text_cols <- cols(.default = col_character())
input <- read_csv(input_file, col_types = csv_text_cols,
                  show_col_types = FALSE) |>
  mutate(across(everything(), blank_na))

required_cols <- c(
  "doc_id", "lineid", "source_lineid", "entry_instance", "name_raw",
  "field", "raw_text_adjusted", "birth_place", "birth_date", "birth_year",
  "birth_city", "birth_state", "birth_country",
  "birth_location_format_problem", "birth_location_format_problem_reason",
  "geocoding_status"
)
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input is missing required columns: ", paste(missing_cols, collapse = ", "))
}

if (n_distinct(paste(input$doc_id, input$lineid)) != nrow(input)) {
  stop("Input has duplicated doc_id + lineid keys.")
}

eligible <- input |>
  mutate(
    birth_city_normalized = normalize_text(birth_city),
    is_bad_ocr_birth_city = has_suspicious_birth_city_character(birth_city),
    suspicious_chars = vapply(birth_city, extract_suspicious_characters,
                              character(1))
  ) |>
  filter(is_bad_ocr_birth_city) |>
  arrange(doc_id, suppressWarnings(as.integer(lineid)), lineid)

if (expected_eligible_n > 0L && nrow(eligible) != expected_eligible_n) {
  stop("Expected ", expected_eligible_n,
       " suspicious birth-city rows, found ", nrow(eligible), ".")
}

excluded_keys <- tibble(doc_id = character(), lineid = character())
if (nzchar(exclude_file)) {
  exclude_file <- normalizePath(exclude_file, winslash = "/", mustWork = TRUE)
  excluded <- read_csv(exclude_file, col_types = csv_text_cols,
                       show_col_types = FALSE) |>
    mutate(across(everything(), blank_na))
  missing_exclude_cols <- setdiff(c("doc_id", "lineid"), names(excluded))
  if (length(missing_exclude_cols)) {
    stop("Exclusion file is missing required columns: ",
         paste(missing_exclude_cols, collapse = ", "))
  }
  excluded_keys <- excluded |>
    distinct(doc_id, lineid)
}

available <- eligible |>
  anti_join(excluded_keys, by = c("doc_id", "lineid"))
if (nrow(available) < sample_n) {
  stop("Requested ", sample_n, " rows but only ", nrow(available),
       " eligible non-excluded rows are available.")
}

RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion",
        sample.kind = "Rejection")
set.seed(random_seed)
sample_index <- sample.int(nrow(available), size = sample_n, replace = FALSE)

pilot <- available[sample_index, , drop = FALSE] |>
  mutate(
    pilot_id = sprintf("%03d", pilot_id_start + row_number() - 1L),
    batch_id = sprintf(
      "%02d",
      batch_id_start + ((row_number() - 1L) %/% batch_size)
    ),
    manual_target_reason = "birth_city_bad_ocr_character",
    birth_city_new = "",
    birth_state_new = "",
    birth_country_new = "",
    location_inference_basis = "",
    location_inference_note = "",
    manual_action = "review_pending",
    manual_confidence = "",
    manual_note = "",
    agent_id = ""
  ) |>
  transmute(
    pilot_id,
    batch_id,
    doc_id,
    lineid,
    source_lineid,
    entry_instance,
    manual_target_reason,
    suspicious_chars,
    name_raw,
    field_old = field,
    raw_text_adjusted,
    birth_place_old = birth_place,
    birth_date_old = birth_date,
    birth_year_old = birth_year,
    birth_city_old = birth_city,
    birth_state_old = birth_state,
    birth_country_old = birth_country,
    birth_location_format_problem,
    birth_location_format_problem_reason,
    geocoding_status,
    birth_city_new,
    birth_state_new,
    birth_country_new,
    location_inference_basis,
    location_inference_note,
    manual_action,
    manual_confidence,
    manual_note,
    agent_id
  )

if (nrow(pilot) != sample_n ||
    n_distinct(paste(pilot$doc_id, pilot$lineid)) != sample_n) {
  stop("Pilot sample must contain exactly ", sample_n,
       " unique doc_id + lineid keys.")
}

batch_counts <- pilot |>
  count(batch_id, name = "n")
expected_batch_n <- ceiling(sample_n / batch_size)
if (nrow(batch_counts) != expected_batch_n ||
    sum(batch_counts$n) != sample_n ||
    any(batch_counts$n > batch_size)) {
  stop("Pilot batch sizes do not match the requested configuration.")
}

write_excel_csv(pilot, master_csv, na = "")
for (batch in split(pilot, pilot$batch_id)) {
  batch_id <- unique(batch$batch_id)
  batch_file <- file.path(
    in_dir,
    paste0("amws_ed86_bad_ocr_batch_", batch_id, ".csv")
  )
  write_excel_csv(batch, batch_file, na = "")
}

input_info <- file.info(input_file)
summary <- bind_rows(
  tibble(metric = "input_file", value = input_file),
  tibble(metric = "input_rows", value = as.character(nrow(input))),
  tibble(metric = "input_md5", value = unname(tools::md5sum(input_file))),
  tibble(metric = "input_size_bytes", value = as.character(input_info$size)),
  tibble(metric = "eligible_rows", value = as.character(nrow(eligible))),
  tibble(metric = "suspicious_pattern", value = suspicious_pattern),
  tibble(metric = "expected_eligible_rows",
         value = as.character(expected_eligible_n)),
  tibble(metric = "exclude_file", value = exclude_file),
  tibble(metric = "excluded_unique_keys",
         value = as.character(nrow(excluded_keys))),
  tibble(metric = "available_after_exclusion",
         value = as.character(nrow(available))),
  tibble(metric = "rng_kind", value = "Mersenne-Twister/Inversion/Rejection"),
  tibble(metric = "random_seed", value = as.character(random_seed)),
  tibble(metric = "pilot_label", value = pilot_label),
  tibble(metric = "pilot_id_start", value = as.character(pilot_id_start)),
  tibble(metric = "batch_id_start", value = as.character(batch_id_start)),
  tibble(metric = "batch_size", value = as.character(batch_size)),
  tibble(metric = "sample_rows", value = as.character(nrow(pilot))),
  batch_counts |>
    transmute(metric = paste0("batch_rows:", batch_id), value = as.character(n))
)
write_excel_csv(summary, summary_csv, na = "")

cat("Input:", input_file, "\n")
cat("Eligible suspicious rows:", nrow(eligible), "\n")
cat("Available after exclusion:", nrow(available), "\n")
cat("Sample rows:", nrow(pilot), "\n")
cat("Wrote master:", master_csv, "\n")
cat("Wrote batches to:", in_dir, "\n")
cat("Wrote summary:", summary_csv, "\n")
