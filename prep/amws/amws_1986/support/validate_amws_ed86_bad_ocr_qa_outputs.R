###############################################################################
# Validate selected directed-QA outputs without modifying data.
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

qa_cols <- c(
  "qa_decision", "qa_manual_action", "qa_birth_city_new",
  "qa_birth_state_new", "qa_birth_country_new",
  "qa_location_inference_basis", "qa_location_inference_note",
  "qa_confidence", "qa_note", "qa_reviewer_id"
)
allowed_decisions <- c("agree", "revise", "escalate")
allowed_actions <- c("correct", "review_unclear", "no_change")
allowed_confidence <- c("high", "medium", "low")
allowed_basis <- c(
  "ocr_explicit", "ocr_fragment", "gazetteer_unique",
  "agent_geographic_knowledge", "mixed", "not_inferred"
)
us_codes <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
  "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
  "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
  "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
  "UT", "VT", "VA", "WA", "WV", "WI", "WY", "PR"
)
canada_codes <- c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON",
                  "PE", "QC", "SK", "YT")

qa_root <- normalizePath(env_chr("AMWS_ED86_BAD_OCR_QA_DIR"),
                         winslash = "/", mustWork = TRUE)
qa_ids <- str_split(env_chr("AMWS_ED86_BAD_OCR_VALIDATE_QA_BATCHES"),
                    "\\s*,\\s*")[[1]]
qa_ids <- qa_ids[nzchar(qa_ids)]
if (!length(qa_ids)) stop("No QA batch IDs supplied.")
csv_text_cols <- cols(.default = col_character())

summaries <- lapply(qa_ids, function(qa_id) {
  file_name <- paste0("amws_ed86_bad_ocr_qa_", qa_id, ".csv")
  input <- read_csv(file.path(qa_root, "in", file_name),
                    col_types = csv_text_cols, show_col_types = FALSE) |>
    mutate(across(everything(), blank_na))
  output <- read_csv(file.path(qa_root, "out", file_name),
                     col_types = csv_text_cols, show_col_types = FALSE) |>
    mutate(across(everything(), blank_na))
  if (!identical(names(input), names(output)) || nrow(input) != nrow(output)) {
    stop(qa_id, " changed QA schema or row count.")
  }
  if (!identical(input[setdiff(names(input), qa_cols)],
                 output[setdiff(names(output), qa_cols)])) {
    stop(qa_id, " changed non-QA fields or row order.")
  }
  if (any(!output$qa_decision %in% allowed_decisions) ||
      any(!nzchar(str_trim(output$qa_note))) ||
      any(!nzchar(str_trim(output$qa_reviewer_id)))) {
    stop(qa_id, " has invalid decisions, notes, or reviewer IDs.")
  }
  if (any(output$qa_reviewer_id == output$agent_id)) {
    stop(qa_id, " assigns the same primary and QA reviewer ID.")
  }

  agree <- output |> filter(qa_decision == "agree")
  if (nrow(agree) && any(
    agree$qa_manual_action != agree$manual_action |
      agree$qa_birth_city_new != agree$birth_city_new |
      agree$qa_birth_state_new != agree$birth_state_new |
      agree$qa_birth_country_new != agree$birth_country_new |
      agree$qa_location_inference_basis != agree$location_inference_basis |
      agree$qa_confidence != agree$manual_confidence
  )) stop(qa_id, " has agree rows that do not repeat primary values.")
  if (nrow(agree) &&
      any(!nzchar(normalize_text(agree$qa_location_inference_note)))) {
    stop(qa_id, " has agree rows with blank independent inference notes.")
  }

  revise <- output |> filter(qa_decision == "revise")
  if (nrow(revise)) {
    if (any(!revise$qa_manual_action %in% allowed_actions) ||
        any(!revise$qa_confidence %in% allowed_confidence) ||
        any(!revise$qa_location_inference_basis %in% allowed_basis)) {
      stop(qa_id, " has invalid revised action/confidence/basis values.")
    }
    revised_correct <- revise |> filter(qa_manual_action == "correct")
    if (nrow(revised_correct)) {
      if (any(!nzchar(normalize_text(revised_correct$qa_birth_city_new))) ||
          any(!nzchar(normalize_text(
            revised_correct$qa_location_inference_note)))) {
        stop(qa_id, " has incomplete revised corrections.")
      }
      if (any(nzchar(revised_correct$qa_birth_state_new) &
              !str_detect(revised_correct$qa_birth_state_new, "^[A-Z]{2}$"))) {
        stop(qa_id, " has malformed revised state codes.")
      }
      if (any(revised_correct$qa_birth_state_new %in% us_codes &
              revised_correct$qa_birth_country_new != "USA") ||
          any(revised_correct$qa_birth_state_new %in% canada_codes &
              revised_correct$qa_birth_country_new != "Canada")) {
        stop(qa_id, " has revised state/country inconsistencies.")
      }
    }
    revised_unclear <- revise |> filter(qa_manual_action == "review_unclear")
    if (nrow(revised_unclear) && any(
      nzchar(normalize_text(revised_unclear$qa_birth_city_new)) |
        nzchar(normalize_text(revised_unclear$qa_birth_state_new)) |
        nzchar(normalize_text(revised_unclear$qa_birth_country_new)) |
        revised_unclear$qa_location_inference_basis != "not_inferred"
    )) stop(qa_id, " has malformed revised review_unclear rows.")
  }

  escalate <- output |> filter(qa_decision == "escalate")
  if (nrow(escalate) && any(
    nzchar(normalize_text(escalate$qa_manual_action)) |
      nzchar(normalize_text(escalate$qa_birth_city_new)) |
      nzchar(normalize_text(escalate$qa_birth_state_new)) |
      nzchar(normalize_text(escalate$qa_birth_country_new)) |
      nzchar(normalize_text(escalate$qa_location_inference_basis)) |
      nzchar(normalize_text(escalate$qa_location_inference_note)) |
      nzchar(normalize_text(escalate$qa_confidence))
  )) stop(qa_id, " has nonblank proposal fields on escalated rows.")

  output |> count(qa_decision, name = "n") |>
    mutate(qa_batch_id = qa_id, .before = 1)
})

print(bind_rows(summaries), n = Inf)
cat("Validated QA batches:", paste(qa_ids, collapse = ", "), "\n")
