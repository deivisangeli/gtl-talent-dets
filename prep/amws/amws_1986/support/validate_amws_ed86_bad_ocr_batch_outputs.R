###############################################################################
# Validate selected AMWS 1986 bad-OCR manual batch outputs without writing data.
#
# Environment:
#   AMWS_ED86_BAD_OCR_PILOT_DIR       batch root containing in/ and out/
#   AMWS_ED86_BAD_OCR_VALIDATE_BATCHES comma-separated batch IDs (e.g. 01,02)
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
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
manual_cols <- c(
  "birth_city_new", "birth_state_new", "birth_country_new",
  "location_inference_basis", "location_inference_note", "manual_action",
  "manual_confidence", "manual_note", "agent_id"
)
allowed_actions <- c("correct", "review_unclear", "no_change")
allowed_confidence <- c("high", "medium", "low")
allowed_basis <- c(
  "ocr_explicit", "ocr_fragment", "gazetteer_unique",
  "agent_geographic_knowledge", "mixed", "not_inferred"
)
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

pilot_root <- normalizePath(
  env_chr("AMWS_ED86_BAD_OCR_PILOT_DIR"),
  winslash = "/",
  mustWork = TRUE
)
batch_ids <- str_split(
  env_chr("AMWS_ED86_BAD_OCR_VALIDATE_BATCHES"),
  "\\s*,\\s*"
)[[1]]
batch_ids <- batch_ids[nzchar(batch_ids)]
if (!length(batch_ids)) stop("No batch IDs supplied for validation.")

csv_text_cols <- cols(.default = col_character())
summaries <- lapply(batch_ids, function(batch_id) {
  batch_id <- sprintf("%02d", as.integer(batch_id))
  file_name <- paste0("amws_ed86_bad_ocr_batch_", batch_id, ".csv")
  input_file <- file.path(pilot_root, "in", file_name)
  output_file <- file.path(pilot_root, "out", file_name)
  input <- read_csv(input_file, col_types = csv_text_cols,
                    show_col_types = FALSE) |>
    mutate(across(everything(), blank_na))
  output <- read_csv(output_file, col_types = csv_text_cols,
                     show_col_types = FALSE) |>
    mutate(across(everything(), blank_na))

  if (!identical(names(input), names(output))) {
    stop("Batch ", batch_id, " changed the column schema.")
  }
  if (nrow(input) != nrow(output)) {
    stop("Batch ", batch_id, " changed the row count.")
  }
  context_cols <- setdiff(names(input), manual_cols)
  if (!identical(input[context_cols], output[context_cols])) {
    stop("Batch ", batch_id, " changed context columns or row order.")
  }
  if (any(!output$manual_action %in% allowed_actions)) {
    stop("Batch ", batch_id, " has invalid manual_action values.")
  }
  if (any(!output$manual_confidence %in% allowed_confidence)) {
    stop("Batch ", batch_id, " has invalid manual_confidence values.")
  }
  if (any(!output$location_inference_basis %in% allowed_basis)) {
    stop("Batch ", batch_id, " has invalid inference-basis values.")
  }
  if (any(!nzchar(str_trim(output$manual_note))) ||
      any(!nzchar(str_trim(output$agent_id)))) {
    stop("Batch ", batch_id, " has blank notes or agent IDs.")
  }

  correct <- output |> filter(manual_action == "correct")
  if (nrow(correct)) {
    if (any(!nzchar(normalize_text(correct$birth_city_new)))) {
      stop("Batch ", batch_id, " has correct rows with blank cities.")
    }
    if (any(str_detect(correct$birth_city_new, suspicious_pattern))) {
      stop("Batch ", batch_id, " has corrected cities with suspicious characters.")
    }
    if (any(!nzchar(normalize_text(correct$location_inference_note))) ||
        any(str_to_lower(normalize_text(correct$location_inference_note)) %in%
              c("high", "medium", "low"))) {
      stop("Batch ", batch_id, " has invalid inference notes.")
    }
    if (any(nzchar(correct$birth_state_new) &
            !str_detect(correct$birth_state_new, "^[A-Z]{2}$"))) {
      stop("Batch ", batch_id, " has malformed state codes.")
    }
    if (any(correct$birth_state_new %in% us_state_codes &
            correct$birth_country_new != "USA")) {
      stop("Batch ", batch_id, " has US state/country inconsistencies.")
    }
    if (any(correct$birth_state_new %in% canadian_province_codes &
            correct$birth_country_new != "Canada")) {
      stop("Batch ", batch_id, " has Canadian province/country inconsistencies.")
    }
    if (any(correct$location_inference_basis ==
              "agent_geographic_knowledge" &
            correct$manual_confidence == "high")) {
      stop("Batch ", batch_id, " assigns high confidence to pure agent knowledge.")
    }
  }

  unclear <- output |> filter(manual_action == "review_unclear")
  if (nrow(unclear) &&
      any(nzchar(normalize_text(unclear$birth_city_new)) |
          nzchar(normalize_text(unclear$birth_state_new)) |
          nzchar(normalize_text(unclear$birth_country_new)) |
          unclear$location_inference_basis != "not_inferred")) {
    stop("Batch ", batch_id, " has malformed review_unclear rows.")
  }

  no_change <- output |> filter(manual_action == "no_change")
  if (nrow(no_change)) {
    source_rows <- input[match(no_change$pilot_id, input$pilot_id), ]
    if (any(no_change$birth_city_new != source_rows$birth_city_old |
            no_change$birth_state_new != source_rows$birth_state_old |
            no_change$birth_country_new != source_rows$birth_country_old |
            no_change$location_inference_basis != "not_inferred")) {
      stop("Batch ", batch_id, " has malformed no_change rows.")
    }
  }

  output |>
    count(manual_action, name = "n") |>
    mutate(batch_id = batch_id, .before = 1)
})

summary <- bind_rows(summaries)
print(summary, n = Inf)
cat("Validated batches:", paste(batch_ids, collapse = ", "), "\n")
