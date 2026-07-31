###############################################################################
# Combine AMWS bad-OCR primary corrections and prepare directed QA batches.
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

data_dir <- file.path(TALENT_DETS_DATA_DIR, "Data", "intermediary", "amws")
rollout_root <- file.path(data_dir,
                          "manual_bad_ocr_birth_city_full_rollout_20260710")
round2_root <- file.path(data_dir,
                         "manual_bad_ocr_birth_city_pilot_round2_20260710")
rollout_file <- file.path(
  rollout_root,
  "amws_ed86_bad_ocr_all_rollout1292_corrections.csv"
)
round2_file <- file.path(
  round2_root,
  "amws_ed86_bad_ocr_sample050_round2_corrections.csv"
)
qa_root <- file.path(rollout_root, "qa")
qa_in_dir <- file.path(qa_root, "in")
qa_out_dir <- file.path(qa_root, "out")
dir.create(qa_in_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_out_dir, recursive = TRUE, showWarnings = FALSE)

global_primary_csv <- file.path(
  rollout_root,
  "amws_ed86_bad_ocr_all_primary_corrections.csv"
)
qa_master_csv <- file.path(qa_root, "amws_ed86_bad_ocr_qa_master.csv")
qa_summary_csv <- file.path(qa_root, "amws_ed86_bad_ocr_qa_prep_summary.csv")

csv_text_cols <- cols(.default = col_character())
read_text_csv <- function(path) {
  read_csv(path, col_types = csv_text_cols, show_col_types = FALSE) |>
    mutate(across(everything(), blank_na))
}

rollout <- read_text_csv(rollout_file) |>
  mutate(
    primary_source = "rollout1292",
    primary_source_review_id = pilot_id,
    primary_source_batch_id = batch_id
  )
round2 <- read_text_csv(round2_file) |>
  mutate(
    primary_source = "round2_accepted",
    primary_source_review_id = pilot_id,
    primary_source_batch_id = batch_id
  )

if (!setequal(names(rollout), names(round2))) {
  stop("Rollout and round-2 correction schemas differ.")
}

primary <- bind_rows(rollout, round2) |>
  arrange(doc_id, suppressWarnings(as.integer(lineid)), lineid) |>
  mutate(global_review_id = sprintf("%04d", row_number()), .before = 1)

if (nrow(primary) != 1342L ||
    n_distinct(paste(primary$doc_id, primary$lineid)) != 1342L) {
  stop("Global primary table must contain exactly 1,342 unique keys.")
}

write_excel_csv(primary, global_primary_csv, na = "")

primary <- primary |>
  mutate(
    changed_state = normalize_text(birth_state_new) !=
      normalize_text(birth_state_old),
    changed_country = normalize_text(birth_country_new) !=
      normalize_text(birth_country_old),
    changed_geo = changed_state | changed_country,
    qa_medium = manual_action == "correct" & manual_confidence == "medium",
    qa_sensitive_basis = manual_action == "correct" &
      location_inference_basis %in% c(
        "mixed", "gazetteer_unique", "agent_geographic_knowledge"
      ),
    qa_nonexplicit_geo = manual_action == "correct" & changed_geo &
      location_inference_basis != "ocr_explicit",
    qa_mandatory = qa_medium | qa_sensitive_basis | qa_nonexplicit_geo
  )

high_pool <- which(
  primary$manual_action == "correct" &
    primary$manual_confidence == "high" &
    !primary$qa_mandatory
)
unclear_pool <- which(primary$manual_action == "review_unclear")

RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion",
        sample.kind = "Rejection")
set.seed(20260719)
high_sample <- if (length(high_pool)) {
  sample(high_pool, size = ceiling(0.10 * length(high_pool)), replace = FALSE)
} else integer()
unclear_sample <- if (length(unclear_pool)) {
  sample(unclear_pool, size = ceiling(0.10 * length(unclear_pool)),
         replace = FALSE)
} else integer()

primary$qa_random_high <- seq_len(nrow(primary)) %in% high_sample
primary$qa_random_unclear <- seq_len(nrow(primary)) %in% unclear_sample

qa <- primary |>
  filter(qa_mandatory | qa_random_high | qa_random_unclear) |>
  mutate(
    qa_selection_reason = vapply(seq_len(n()), function(i) {
      reasons <- c(
        if (qa_medium[[i]]) "medium_confidence",
        if (qa_sensitive_basis[[i]]) "sensitive_inference_basis",
        if (qa_nonexplicit_geo[[i]]) "nonexplicit_state_country_change",
        if (qa_random_high[[i]]) "random_10pct_remaining_high",
        if (qa_random_unclear[[i]]) "random_10pct_review_unclear"
      )
      paste(reasons, collapse = ";")
    }, character(1)),
    assigned_qa_worker = case_when(
      str_detect(agent_id, "rollout_worker_1") ~ "worker_2",
      str_detect(agent_id, "rollout_worker_2") ~ "worker_3",
      str_detect(agent_id, "rollout_worker_3") ~ "worker_1",
      TRUE ~ "worker_1"
    )
  )

set.seed(20260720)
qa <- qa[sample.int(nrow(qa)), , drop = FALSE] |>
  group_by(assigned_qa_worker) |>
  mutate(
    qa_worker_row = row_number(),
    qa_batch_number = ((qa_worker_row - 1L) %/% 50L) + 1L,
    qa_batch_id = paste0(assigned_qa_worker, "_",
                         sprintf("%02d", qa_batch_number))
  ) |>
  ungroup() |>
  arrange(assigned_qa_worker, qa_batch_number, qa_worker_row) |>
  mutate(
    qa_id = sprintf("%04d", row_number()),
    qa_decision = "review_pending",
    qa_manual_action = "",
    qa_birth_city_new = "",
    qa_birth_state_new = "",
    qa_birth_country_new = "",
    qa_location_inference_basis = "",
    qa_location_inference_note = "",
    qa_confidence = "",
    qa_note = "",
    qa_reviewer_id = ""
  )

write_excel_csv(qa, qa_master_csv, na = "")
for (batch in split(qa, qa$qa_batch_id)) {
  batch_id <- unique(batch$qa_batch_id)
  write_excel_csv(
    batch,
    file.path(qa_in_dir,
              paste0("amws_ed86_bad_ocr_qa_", batch_id, ".csv")),
    na = ""
  )
}

summary <- bind_rows(
  tibble(metric = "global_primary_rows", value = as.character(nrow(primary))),
  tibble(metric = "qa_rows", value = as.character(nrow(qa))),
  tibble(metric = "qa_mandatory_rows", value = as.character(sum(primary$qa_mandatory))),
  tibble(metric = "remaining_high_pool", value = as.character(length(high_pool))),
  tibble(metric = "sampled_remaining_high", value = as.character(length(high_sample))),
  tibble(metric = "review_unclear_pool", value = as.character(length(unclear_pool))),
  tibble(metric = "sampled_review_unclear", value = as.character(length(unclear_sample))),
  qa |>
    count(assigned_qa_worker, name = "n") |>
    transmute(metric = paste0("qa_rows:", assigned_qa_worker),
              value = as.character(n)),
  qa |>
    distinct(qa_batch_id) |>
    count(name = "n") |>
    transmute(metric = "qa_batch_count", value = as.character(n))
)
write_excel_csv(summary, qa_summary_csv, na = "")

cat("Global primary rows:", nrow(primary), "\n")
cat("Directed QA rows:", nrow(qa), "\n")
cat("QA batches:", n_distinct(qa$qa_batch_id), "\n")
cat("Wrote global primary:", global_primary_csv, "\n")
cat("Wrote QA master:", qa_master_csv, "\n")
