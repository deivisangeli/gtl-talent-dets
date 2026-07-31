###############################################################################
# Validate and aggregate AMWS 1986 bad-OCR birth-city pilot batches.
#
# Reads:
#   Data/intermediary/amws/manual_bad_ocr_birth_city_pilot_20260710/
#     amws_ed86_bad_ocr_sample100_master.csv
#     out/amws_ed86_bad_ocr_batch_01.csv
#     out/amws_ed86_bad_ocr_batch_02.csv
#
# Writes (without modifying any AMWS source dataset):
#   amws_ed86_bad_ocr_sample100_corrections.csv
#   amws_ed86_bad_ocr_sample100_aggregate_summary.csv
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

env_flag <- function(name, default = FALSE) {
  value <- str_to_lower(env_chr(name, ifelse(default, "true", "false")))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be a boolean; received: ", value)
  }
  value %in% c("true", "1", "yes")
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

suspicious_pattern <- "[^\\p{L}\\s.,'’‘()&/\\-]"

has_suspicious_birth_city_character <- function(x) {
  city <- normalize_text(x)
  nzchar(city) & str_detect(city, suspicious_pattern)
}

normalize_place_key <- function(x) {
  normalize_text(x) |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}

data_dir <- file.path(TALENT_DETS_DATA_DIR, "Data")
pilot_label <- env_chr("AMWS_ED86_BAD_OCR_PILOT_LABEL", "sample100")
batch_size <- env_int("AMWS_ED86_BAD_OCR_PILOT_BATCH_SIZE", 50L)
require_inference_metadata <- env_flag(
  "AMWS_ED86_BAD_OCR_PILOT_REQUIRE_INFERENCE_METADATA",
  FALSE
)
pilot_root <- env_chr(
  "AMWS_ED86_BAD_OCR_PILOT_DIR",
  file.path(data_dir, "intermediary", "amws",
            "manual_bad_ocr_birth_city_pilot_20260710")
)
pilot_root <- normalizePath(pilot_root, winslash = "/", mustWork = TRUE)
out_dir <- file.path(pilot_root, "out")
master_csv <- file.path(pilot_root,
                        paste0("amws_ed86_bad_ocr_", pilot_label,
                               "_master.csv"))
corrections_csv <- file.path(
  pilot_root,
  paste0("amws_ed86_bad_ocr_", pilot_label, "_corrections.csv")
)
summary_csv <- file.path(
  pilot_root,
  paste0("amws_ed86_bad_ocr_", pilot_label, "_aggregate_summary.csv")
)

csv_text_cols <- cols(.default = col_character())
master <- read_csv(master_csv, col_types = csv_text_cols,
                   show_col_types = FALSE) |>
  mutate(across(everything(), blank_na))

manual_cols <- c(
  "birth_city_new", "birth_state_new", "birth_country_new",
  "location_inference_basis", "location_inference_note",
  "manual_action", "manual_confidence", "manual_note", "agent_id"
)
inference_cols <- c("location_inference_basis", "location_inference_note")
key_cols <- c("pilot_id", "batch_id", "doc_id", "lineid", "entry_instance")
required_output_cols <- c(key_cols, setdiff(manual_cols, inference_cols))

files <- sort(list.files(
  out_dir,
  pattern = "^amws_ed86_bad_ocr_batch_[0-9]{2}\\.csv$",
  full.names = TRUE
))
expected_batch_n <- ceiling(nrow(master) / batch_size)
if (length(files) != expected_batch_n) {
  stop("Expected exactly ", expected_batch_n, " batch outputs in ", out_dir,
       "; found ", length(files), ".")
}

batch_rows <- bind_rows(lapply(files, function(path) {
  x <- read_csv(path, col_types = csv_text_cols, show_col_types = FALSE) |>
    mutate(across(everything(), blank_na))
  missing_cols <- setdiff(required_output_cols, names(x))
  if (length(missing_cols)) {
    stop("Output batch is missing required columns: ", basename(path),
         " -> ", paste(missing_cols, collapse = ", "))
  }
  if (!"location_inference_basis" %in% names(x)) {
    x$location_inference_basis <- "not_inferred"
  }
  if (!"location_inference_note" %in% names(x)) {
    x$location_inference_note <- x$manual_note
  }
  x |>
    select(all_of(c(key_cols, manual_cols))) |>
    mutate(batch_output_file = basename(path))
}))

if (nrow(batch_rows) != nrow(master)) {
  stop("Expected ", nrow(master), " reviewed rows, found ",
       nrow(batch_rows), ".")
}

batch_counts <- batch_rows |>
  count(batch_id, name = "n")
if (sum(batch_counts$n) != nrow(master) || any(batch_counts$n > batch_size)) {
  stop("Reviewed batch sizes do not match the configured master.")
}

duplicate_keys <- batch_rows |>
  count(across(all_of(key_cols))) |>
  filter(n > 1L)
if (nrow(duplicate_keys)) {
  stop("Duplicate pilot keys across output batches: ", nrow(duplicate_keys))
}

unmatched <- anti_join(batch_rows, master, by = key_cols)
if (nrow(unmatched)) {
  stop("Output batches contain keys not present in the pilot master: ",
       nrow(unmatched))
}

missing_outputs <- anti_join(master, batch_rows, by = key_cols)
if (nrow(missing_outputs)) {
  stop("Output batches do not cover all pilot rows. Missing: ",
       nrow(missing_outputs))
}

allowed_actions <- c("correct", "review_unclear", "no_change")
bad_actions <- setdiff(unique(batch_rows$manual_action), allowed_actions)
if (length(bad_actions)) {
  stop("Invalid manual_action values: ", paste(bad_actions, collapse = ", "))
}

allowed_confidence <- c("high", "medium", "low")
bad_confidence <- setdiff(unique(batch_rows$manual_confidence),
                          allowed_confidence)
if (length(bad_confidence)) {
  stop("Invalid or missing manual_confidence values: ",
       paste(bad_confidence, collapse = ", "))
}

allowed_inference_basis <- c(
  "ocr_explicit", "ocr_fragment", "gazetteer_unique",
  "agent_geographic_knowledge", "mixed", "not_inferred"
)
bad_inference_basis <- setdiff(unique(batch_rows$location_inference_basis),
                               allowed_inference_basis)
if (length(bad_inference_basis)) {
  stop("Invalid or missing location_inference_basis values: ",
       paste(bad_inference_basis, collapse = ", "))
}

if (any(!nzchar(str_trim(batch_rows$manual_note)))) {
  stop("Every reviewed row must include a nonempty manual_note.")
}
if (any(!nzchar(str_trim(batch_rows$agent_id)))) {
  stop("Every reviewed row must include a nonempty agent_id.")
}

reviewed <- master |>
  select(-any_of(manual_cols)) |>
  left_join(batch_rows |> select(-batch_output_file), by = key_cols)

correct_rows <- reviewed |>
  filter(manual_action == "correct")
if (any(!nzchar(normalize_text(correct_rows$birth_city_new)))) {
  stop("Every correct row must include a nonempty birth_city_new.")
}
if (any(!nzchar(normalize_text(correct_rows$birth_country_new)))) {
  stop("Every correct row must include a nonempty birth_country_new.")
}
if (any(has_suspicious_birth_city_character(correct_rows$birth_city_new))) {
  stop("Correct rows still contain suspicious birth_city_new characters.")
}
if (any(!nzchar(normalize_text(correct_rows$location_inference_note)))) {
  stop("Every correct row must include a location_inference_note.")
}
if (require_inference_metadata &&
    any(correct_rows$location_inference_basis == "not_inferred" &
        (normalize_text(correct_rows$birth_state_new) !=
           normalize_text(correct_rows$birth_state_old) |
         normalize_text(correct_rows$birth_country_new) !=
           normalize_text(correct_rows$birth_country_old)))) {
  stop("State/country changes cannot use location_inference_basis = not_inferred.")
}
if (any(correct_rows$location_inference_basis ==
          "agent_geographic_knowledge" &
        correct_rows$manual_confidence == "high")) {
  stop("Pure agent geographic inference cannot have high confidence.")
}

correct_changed <- normalize_text(correct_rows$birth_city_new) !=
    normalize_text(correct_rows$birth_city_old) |
  normalize_text(correct_rows$birth_state_new) !=
    normalize_text(correct_rows$birth_state_old) |
  normalize_text(correct_rows$birth_country_new) !=
    normalize_text(correct_rows$birth_country_old)
if (any(!correct_changed)) {
  stop("Every correct row must change at least one target field.")
}

bad_state <- correct_rows |>
  filter(nzchar(normalize_text(birth_state_new)),
         !str_detect(normalize_text(birth_state_new), "^[A-Z]{2}$"))
if (nrow(bad_state)) {
  stop("Correct rows contain malformed birth_state_new values: ",
       nrow(bad_state))
}

no_change_rows <- reviewed |>
  filter(manual_action == "no_change")
no_change_matches <- normalize_text(no_change_rows$birth_city_new) ==
    normalize_text(no_change_rows$birth_city_old) &
  normalize_text(no_change_rows$birth_state_new) ==
    normalize_text(no_change_rows$birth_state_old) &
  normalize_text(no_change_rows$birth_country_new) ==
    normalize_text(no_change_rows$birth_country_old)
if (any(!no_change_matches)) {
  stop("no_change rows must repeat the three old values in the new columns.")
}
if (any(no_change_rows$location_inference_basis != "not_inferred")) {
  stop("no_change rows must use location_inference_basis = not_inferred.")
}

unclear_rows <- reviewed |>
  filter(manual_action == "review_unclear")
unclear_blank <- !nzchar(normalize_text(unclear_rows$birth_city_new)) &
  !nzchar(normalize_text(unclear_rows$birth_state_new)) &
  !nzchar(normalize_text(unclear_rows$birth_country_new))
if (any(!unclear_blank)) {
  stop("review_unclear rows must leave the three new-value columns blank.")
}
if (any(unclear_rows$location_inference_basis != "not_inferred")) {
  stop("review_unclear rows must use location_inference_basis = not_inferred.")
}

us_state_codes <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
  "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
  "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
  "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
  "UT", "VT", "VA", "WA", "WV", "WI", "WY", "PR"
)
canadian_province_codes <- c(
  "AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC",
  "SK", "YT"
)

bad_us_country <- correct_rows |>
  filter(birth_state_new %in% us_state_codes,
         normalize_text(birth_country_new) != "USA")
if (nrow(bad_us_country)) {
  stop("US state codes must have birth_country_new = USA: ",
       nrow(bad_us_country))
}

bad_canada_country <- correct_rows |>
  filter(birth_state_new %in% canadian_province_codes,
         normalize_text(birth_country_new) != "Canada")
if (nrow(bad_canada_country)) {
  stop("Canadian province codes must have birth_country_new = Canada: ",
       nrow(bad_canada_country))
}
missing_us_canada_state <- correct_rows |>
  filter(birth_country_new %in% c("USA", "Canada"),
         !nzchar(normalize_text(birth_state_new)))
if (nrow(missing_us_canada_state)) {
  stop("USA/Canada correct rows require a state/province code: ",
       nrow(missing_us_canada_state))
}

gazetteer_unique_rows <- correct_rows |>
  filter(location_inference_basis == "gazetteer_unique")
if (nrow(gazetteer_unique_rows)) {
  gazetteer_file <- file.path(TALENT_DETS_DATA_DIR, "input",
                              "2024_Gaz_place_national.txt")
  geonames_file <- file.path(TALENT_DETS_DATA_DIR, "input",
                             "geonames_US.txt")
  if (!file.exists(gazetteer_file) || !file.exists(geonames_file)) {
    stop("Gazetteer validation requires 2024_Gaz_place_national.txt and ",
         "geonames_US.txt in the Dropbox input directory.")
  }

  census_places <- read_tsv(
    gazetteer_file,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    transmute(
      city_name = str_remove(
        NAME,
        regex("\\s+(city|town|village|CDP|borough|municipality)$",
              ignore_case = TRUE)
      ),
      state_code = USPS
    )

  geonames_cols <- c(
    "geonameid", "name", "asciiname", "alternatenames", "latitude",
    "longitude", "feature_class", "feature_code", "country_code", "cc2",
    "admin1_code", "admin2_code", "admin3_code", "admin4_code", "population",
    "elevation", "dem", "timezone", "modification_date"
  )
  geonames_places <- read_tsv(
    geonames_file,
    col_names = geonames_cols,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE,
    quote = ""
  ) |>
    filter(country_code == "US", feature_class == "P") |>
    transmute(city_name = if_else(nzchar(asciiname), asciiname, name),
              state_code = admin1_code)

  us_place_reference <- bind_rows(census_places, geonames_places) |>
    mutate(city_key = normalize_place_key(city_name)) |>
    filter(nzchar(city_key), state_code %in% us_state_codes) |>
    distinct(city_key, state_code)

  gazetteer_checks <- lapply(seq_len(nrow(gazetteer_unique_rows)), function(i) {
    row <- gazetteer_unique_rows[i, , drop = FALSE]
    key <- normalize_place_key(row$birth_city_new)
    states <- us_place_reference |>
      filter(city_key == key) |>
      pull(state_code) |>
      unique()
    tibble(
      pilot_id = row$pilot_id,
      matched_state_n = length(states),
      matched_state = ifelse(length(states) == 1L, states[[1]], ""),
      proposed_state = row$birth_state_new
    )
  }) |>
    bind_rows()

  bad_gazetteer <- gazetteer_checks |>
    filter(matched_state_n != 1L | matched_state != proposed_state)
  if (nrow(bad_gazetteer)) {
    stop("gazetteer_unique rows are not unique or disagree with the proposed ",
         "state: ", paste(bad_gazetteer$pilot_id, collapse = ", "))
  }
}

corrections <- reviewed |>
  arrange(as.integer(pilot_id))
write_excel_csv(corrections, corrections_csv, na = "")

summary <- bind_rows(
  tibble(metric = "reviewed_rows", value = as.character(nrow(corrections))),
  tibble(metric = "reviewed_unique_keys",
         value = as.character(n_distinct(paste(corrections$doc_id,
                                               corrections$lineid)))),
  batch_counts |>
    transmute(metric = paste0("batch_rows:", batch_id), value = as.character(n)),
  corrections |>
    count(manual_action, name = "n") |>
    transmute(metric = paste0("manual_action:", manual_action),
              value = as.character(n)),
  corrections |>
    count(manual_confidence, name = "n") |>
    transmute(metric = paste0("manual_confidence:", manual_confidence),
              value = as.character(n)),
  corrections |>
    count(location_inference_basis, name = "n") |>
    transmute(metric = paste0("location_inference_basis:",
                              location_inference_basis),
              value = as.character(n)),
  tibble(
    metric = c(
      "changed_birth_city",
      "changed_birth_state",
      "changed_birth_country",
      "added_birth_state",
      "added_birth_country",
      "proposed_correct_rows_with_suspicious_city"
    ),
    value = as.character(c(
      sum(corrections$manual_action == "correct" &
            normalize_text(corrections$birth_city_new) !=
              normalize_text(corrections$birth_city_old)),
      sum(corrections$manual_action == "correct" &
            normalize_text(corrections$birth_state_new) !=
              normalize_text(corrections$birth_state_old)),
      sum(corrections$manual_action == "correct" &
            normalize_text(corrections$birth_country_new) !=
              normalize_text(corrections$birth_country_old)),
      sum(corrections$manual_action == "correct" &
            !nzchar(normalize_text(corrections$birth_state_old)) &
            nzchar(normalize_text(corrections$birth_state_new))),
      sum(corrections$manual_action == "correct" &
            !nzchar(normalize_text(corrections$birth_country_old)) &
            nzchar(normalize_text(corrections$birth_country_new))),
      sum(corrections$manual_action == "correct" &
            has_suspicious_birth_city_character(corrections$birth_city_new))
    ))
  )
)
write_excel_csv(summary, summary_csv, na = "")

cat("Reviewed rows:", nrow(corrections), "\n")
cat("Correct:", sum(corrections$manual_action == "correct"), "\n")
cat("Review unclear:", sum(corrections$manual_action == "review_unclear"), "\n")
cat("No change:", sum(corrections$manual_action == "no_change"), "\n")
cat("Wrote corrections:", corrections_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
