###############################################################################
# Adjudicate QA disagreements and write final global AMWS bad-OCR overrides.
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

rollout_root <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "intermediary", "amws",
  "manual_bad_ocr_birth_city_full_rollout_20260710"
)
qa_root <- file.path(rollout_root, "qa")
primary_csv <- file.path(rollout_root,
                         "amws_ed86_bad_ocr_all_primary_corrections.csv")
qa_results_csv <- file.path(qa_root, "amws_ed86_bad_ocr_qa_results.csv")
disagreements_csv <- file.path(qa_root,
                               "amws_ed86_bad_ocr_qa_disagreements.csv")
adjudications_csv <- file.path(qa_root,
                               "amws_ed86_bad_ocr_qa_adjudications.csv")
final_csv <- file.path(rollout_root,
                       "amws_ed86_bad_ocr_all_corrections.csv")
summary_csv <- file.path(rollout_root,
                         "amws_ed86_bad_ocr_all_corrections_summary.csv")

csv_text_cols <- cols(.default = col_character())
read_text_csv <- function(path) {
  read_csv(path, col_types = csv_text_cols, show_col_types = FALSE) |>
    mutate(across(everything(), blank_na))
}
primary <- read_text_csv(primary_csv)
qa_results <- read_text_csv(qa_results_csv)
disagreements <- read_text_csv(disagreements_csv)

decisions <- tribble(
  ~global_review_id, ~adjudication_decision,
  "1219", "accept_qa",
  "0406", "accept_qa",
  "0685", "accept_qa",
  "0919", "accept_qa",
  "1006", "accept_qa",
  "0418", "accept_qa",
  "0107", "accept_qa",
  "0231", "accept_qa",
  "1173", "custom",
  "1335", "accept_qa",
  "0853", "accept_qa",
  "1254", "custom",
  "0645", "accept_qa",
  "1082", "accept_qa",
  "0209", "custom",
  "0479", "custom",
  "0752", "custom",
  "0605", "custom",
  "0366", "custom",
  "0843", "custom",
  "0267", "custom",
  "0765", "custom",
  "0779", "custom",
  "1080", "custom",
  "0295", "accept_primary",
  "0119", "custom",
  "1180", "custom",
  "0995", "custom",
  "0811", "custom",
  "0437", "custom",
  "1292", "custom"
)

custom <- tribble(
  ~global_review_id, ~custom_action, ~custom_city, ~custom_state,
  ~custom_country, ~custom_basis, ~custom_inference_note, ~custom_confidence,
  ~custom_note,
  "1173", "correct", "Grand Rapids", "", "", "ocr_fragment",
  "Grand R... strongly supports Grand Rapids, but no jurisdiction is legible.",
  "medium", "Retain the recoverable city without forcing MI or USA.",
  "1254", "correct", "Birmingham", "", "", "ocr_fragment",
  "Birminghan is an obvious OCR form of Birmingham; jurisdiction is ambiguous.",
  "medium", "Correct city only because Birmingham may refer to multiple countries.",
  "0209", "review_unclear", "", "", "", "not_inferred",
  "Nr* does not identify a unique birthplace.", "low",
  "Reject unsupported New York inference.",
  "0479", "review_unclear", "", "", "", "not_inferred",
  "Fa... does not uniquely support Fargo or North Dakota.", "low",
  "Reject unsupported Fargo inference.",
  "0752", "review_unclear", "", "", "", "not_inferred",
  "A... plus a damaged Pennsylvania token is not a unique city.", "low",
  "Reject unsupported Allentown inference.",
  "0605", "review_unclear", "", "", "", "not_inferred",
  "Tim... does not uniquely identify Trikala or Greece.", "low",
  "Reject unsupported international inference.",
  "0366", "review_unclear", "", "", "", "not_inferred",
  "P... A... does not uniquely identify Perth Amboy.", "low",
  "Reject unsupported city-state reconstruction.",
  "0843", "correct", "Mount Vernon", "", "", "ocr_fragment",
  "Ml Jen... vi is consistent with Mount Vernon; state is not recoverable.",
  "medium", "Retain city only because multiple Mount Vernons exist.",
  "0267", "review_unclear", "", "", "", "not_inferred",
  "St j... in a merged row does not establish Saint John, NB.", "low",
  "Merged-entry ambiguity remains unresolved.",
  "0765", "review_unclear", "", "", "", "not_inferred",
  "The short Vkh... fragment does not uniquely identify Wichita.", "low",
  "Reject unsupported Wichita inference.",
  "0779", "review_unclear", "", "", "", "not_inferred",
  "Kimpnln could support multiple Kingston/Kinston-like names.", "low",
  "City and state remain ambiguous.",
  "1080", "correct", "Columbus", "", "", "ocr_fragment",
  "Columbu* clearly supports Columbus; the state suffix is unreadable.",
  "medium", "Retain city only because Columbus occurs in many states.",
  "0119", "correct", "Gibsons", "", "", "ocr_fragment",
  "GibsonN supports Gibsons; BC appears only in later career context.",
  "medium", "Retain city only and leave jurisdiction blank.",
  "1180", "review_unclear", "", "", "", "not_inferred",
  "Td... and G... do not uniquely identify Tifton, Georgia.", "low",
  "Reject unsupported Tifton inference.",
  "0995", "correct", "Frederick", "", "", "ocr_fragment",
  "Frcdet... supports Frederick; Maryland appears only in later employment.",
  "medium", "Retain city only and leave jurisdiction blank.",
  "0811", "review_unclear", "", "", "", "not_inferred",
  "An S-initial fragment cannot uniquely support San Francisco.", "low",
  "Reject context-driven San Francisco inference.",
  "0437", "review_unclear", "", "", "", "not_inferred",
  "Ch... before a date does not uniquely identify Chicago.", "low",
  "Reject unsupported Chicago inference.",
  "1292", "correct", "Easton", "", "", "ocr_explicit",
  "Easton is explicit, but N1 conflicts with the geographic references.",
  "medium", "Retain city only and leave the unresolved jurisdiction blank."
)

if (!setequal(decisions$global_review_id, disagreements$global_review_id) ||
    nrow(decisions) != nrow(disagreements)) {
  stop("Adjudication decision table does not cover every disagreement once.")
}

adjudications <- disagreements |>
  select(-starts_with("adjudicat")) |>
  left_join(decisions, by = "global_review_id") |>
  left_join(custom, by = "global_review_id") |>
  mutate(
    adjudicated_manual_action = case_when(
      adjudication_decision == "accept_primary" ~ manual_action,
      adjudication_decision == "accept_qa" ~ qa_manual_action,
      TRUE ~ custom_action
    ),
    adjudicated_birth_city_new = case_when(
      adjudication_decision == "accept_primary" ~ birth_city_new,
      adjudication_decision == "accept_qa" ~ qa_birth_city_new,
      TRUE ~ custom_city
    ),
    adjudicated_birth_state_new = case_when(
      adjudication_decision == "accept_primary" ~ birth_state_new,
      adjudication_decision == "accept_qa" ~ qa_birth_state_new,
      TRUE ~ custom_state
    ),
    adjudicated_birth_country_new = case_when(
      adjudication_decision == "accept_primary" ~ birth_country_new,
      adjudication_decision == "accept_qa" ~ qa_birth_country_new,
      TRUE ~ custom_country
    ),
    adjudicated_location_inference_basis = case_when(
      adjudication_decision == "accept_primary" ~ location_inference_basis,
      adjudication_decision == "accept_qa" ~ qa_location_inference_basis,
      TRUE ~ custom_basis
    ),
    adjudicated_location_inference_note = case_when(
      adjudication_decision == "accept_primary" ~ location_inference_note,
      adjudication_decision == "accept_qa" ~ qa_location_inference_note,
      TRUE ~ custom_inference_note
    ),
    adjudicated_confidence = case_when(
      adjudication_decision == "accept_primary" ~ manual_confidence,
      adjudication_decision == "accept_qa" ~ qa_confidence,
      TRUE ~ custom_confidence
    ),
    adjudication_note = case_when(
      adjudication_decision == "accept_primary" ~
        "Primary proposal retained after adjudication.",
      adjudication_decision == "accept_qa" ~ qa_note,
      TRUE ~ custom_note
    ),
    adjudicator_id = "root"
  )
write_excel_csv(adjudications, adjudications_csv, na = "")

manual_cols <- c(
  "birth_city_new", "birth_state_new", "birth_country_new",
  "location_inference_basis", "location_inference_note", "manual_action",
  "manual_confidence", "manual_note", "agent_id"
)
qa_meta <- qa_results |>
  select(global_review_id, qa_decision, qa_reviewer_id, qa_note)
adj_meta <- adjudications |>
  select(global_review_id, adjudication_decision,
         starts_with("adjudicated_"), adjudication_note, adjudicator_id)

final <- primary |>
  rename_with(~ paste0("primary_", .x), all_of(manual_cols)) |>
  left_join(qa_meta, by = "global_review_id") |>
  left_join(adj_meta, by = "global_review_id") |>
  mutate(across(c(qa_decision, qa_reviewer_id, qa_note,
                  adjudication_decision, adjudication_note, adjudicator_id),
                blank_na)) |>
  mutate(
    final_source = case_when(
      qa_decision == "" ~ "primary_not_sampled_for_qa",
      qa_decision == "agree" ~ "qa_agree_primary",
      adjudication_decision == "accept_primary" ~ "adjudicated_primary",
      adjudication_decision == "accept_qa" ~ "adjudicated_qa",
      adjudication_decision == "custom" ~ "adjudicated_custom",
      TRUE ~ "invalid"
    ),
    manual_action = if_else(
      qa_decision %in% c("revise", "escalate"),
      adjudicated_manual_action, primary_manual_action
    ),
    birth_city_new = if_else(
      qa_decision %in% c("revise", "escalate"),
      adjudicated_birth_city_new, primary_birth_city_new
    ),
    birth_state_new = if_else(
      qa_decision %in% c("revise", "escalate"),
      adjudicated_birth_state_new, primary_birth_state_new
    ),
    birth_country_new = if_else(
      qa_decision %in% c("revise", "escalate"),
      adjudicated_birth_country_new, primary_birth_country_new
    ),
    location_inference_basis = if_else(
      qa_decision %in% c("revise", "escalate"),
      adjudicated_location_inference_basis,
      primary_location_inference_basis
    ),
    location_inference_note = if_else(
      qa_decision %in% c("revise", "escalate"),
      adjudicated_location_inference_note,
      primary_location_inference_note
    ),
    manual_confidence = if_else(
      qa_decision %in% c("revise", "escalate"),
      adjudicated_confidence, primary_manual_confidence
    ),
    manual_note = if_else(
      qa_decision %in% c("revise", "escalate"),
      adjudication_note, primary_manual_note
    ),
    agent_id = primary_agent_id
  )

if (nrow(final) != 1342L ||
    n_distinct(paste(final$doc_id, final$lineid)) != 1342L ||
    any(final$final_source == "invalid")) {
  stop("Final table has invalid coverage or unresolved QA rows.")
}

allowed_actions <- c("correct", "review_unclear", "no_change")
if (any(!final$manual_action %in% allowed_actions)) {
  stop("Final table has invalid manual_action values.")
}
correct <- final |> filter(manual_action == "correct")
if (any(!nzchar(normalize_text(correct$birth_city_new))) ||
    any(nzchar(correct$birth_state_new) &
        !str_detect(correct$birth_state_new, "^[A-Z]{2}$"))) {
  stop("Final corrections have blank cities or malformed state codes.")
}
unclear <- final |> filter(manual_action == "review_unclear")
if (nrow(unclear) && any(
  nzchar(normalize_text(unclear$birth_city_new)) |
    nzchar(normalize_text(unclear$birth_state_new)) |
    nzchar(normalize_text(unclear$birth_country_new)) |
    unclear$location_inference_basis != "not_inferred"
)) stop("Final review_unclear rows are malformed.")

write_excel_csv(final, final_csv, na = "")
summary <- bind_rows(
  tibble(metric = "final_rows", value = as.character(nrow(final))),
  tibble(metric = "qa_rows", value = as.character(nrow(qa_results))),
  tibble(metric = "adjudicated_rows", value = as.character(nrow(adjudications))),
  final |> count(manual_action, name = "n") |>
    transmute(metric = paste0("manual_action:", manual_action),
              value = as.character(n)),
  final |> count(manual_confidence, name = "n") |>
    transmute(metric = paste0("manual_confidence:", manual_confidence),
              value = as.character(n)),
  final |> count(location_inference_basis, name = "n") |>
    transmute(metric = paste0("location_inference_basis:",
                              location_inference_basis),
              value = as.character(n)),
  final |> count(final_source, name = "n") |>
    transmute(metric = paste0("final_source:", final_source),
              value = as.character(n)),
  tibble(
    metric = c("correct_with_state", "correct_with_country"),
    value = as.character(c(
      sum(final$manual_action == "correct" & nzchar(final$birth_state_new)),
      sum(final$manual_action == "correct" & nzchar(final$birth_country_new))
    ))
  )
)
write_excel_csv(summary, summary_csv, na = "")

cat("Final rows:", nrow(final), "\n")
cat("Correct:", sum(final$manual_action == "correct"), "\n")
cat("Review unclear:", sum(final$manual_action == "review_unclear"), "\n")
cat("No change:", sum(final$manual_action == "no_change"), "\n")
cat("Wrote final overrides:", final_csv, "\n")
