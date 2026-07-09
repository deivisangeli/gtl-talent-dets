###############################################################################
# Sample remaining AMWS Ed16 manual targets for regex-diagnosis review.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_expanded_birth_place_manual_corrections.csv
#
# Writes:
#   output/amws/regex_all_docs/manual_birth_place_regex_sample/
#     amws_ed16_remaining_regex_diagnosis_sample.csv
#     sample_manifest.csv
#     in/batch_0001.csv, ...
#
# Environment overrides:
#   AMWS_ED16_MANUAL_BP_OUTPUT_DIR
#   AMWS_ED16_REGEX_SAMPLE_SEED
#   AMWS_ED16_REGEX_SAMPLE_FRACTION
#   AMWS_ED16_REGEX_SAMPLE_BATCH_SIZE
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

env_num <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  parsed <- suppressWarnings(as.numeric(value))
  if (is.na(parsed) || parsed <= 0 || parsed > 1) {
    stop("Environment variable ", name, " must be in (0, 1]; got: ", value)
  }
  parsed
}

csv_text_cols <- cols(.default = col_character())

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_MANUAL_BP_OUTPUT_DIR", default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

seed <- env_int("AMWS_ED16_REGEX_SAMPLE_SEED", 20260706L)
sample_fraction <- env_num("AMWS_ED16_REGEX_SAMPLE_FRACTION", 0.10)
batch_size <- env_int("AMWS_ED16_REGEX_SAMPLE_BATCH_SIZE", 25L)

manual_csv <- file.path(output_dir,
                        "amws_ed16_expanded_birth_place_manual_corrections.csv")
sample_root <- file.path(output_dir, "manual_birth_place_regex_sample")
in_dir <- file.path(sample_root, "in")
out_dir <- file.path(sample_root, "out")
dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sample_csv <- file.path(sample_root,
                        "amws_ed16_remaining_regex_diagnosis_sample.csv")
manifest_csv <- file.path(sample_root, "sample_manifest.csv")
summary_csv <- file.path(sample_root, "sample_summary.csv")

manual <- read_csv(manual_csv, col_types = csv_text_cols, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c(
  "doc_id", "lineid", "source_lineid", "entry_instance",
  "manual_target_reason", "birth_place_word_n",
  "has_dee_date_in_birth_place", "birth_place_old", "birth_date_old",
  "birth_year_old", "birth_city_old", "field_old", "raw_text_adjusted",
  "raw_text", "manual_action"
)
missing_cols <- setdiff(required_cols, names(manual))
if (length(missing_cols)) {
  stop("Manual corrections table is missing required columns: ",
       paste(missing_cols, collapse = ", "))
}

pending <- manual |>
  filter(manual_action == "review_pending") |>
  mutate(
    birth_place_word_n_int = suppressWarnings(as.integer(birth_place_word_n)),
    word_band = case_when(
      is.na(birth_place_word_n_int) ~ "unknown",
      birth_place_word_n_int <= 5L ~ "04_05",
      birth_place_word_n_int <= 9L ~ "06_09",
      birth_place_word_n_int <= 19L ~ "10_19",
      birth_place_word_n_int <= 49L ~ "20_49",
      TRUE ~ "50_plus"
    ),
    sample_stratum = paste(manual_target_reason, word_band,
                           has_dee_date_in_birth_place, sep = "|")
  ) |>
  arrange(sample_stratum, doc_id, suppressWarnings(as.integer(lineid)))

if (!nrow(pending)) {
  stop("No review_pending rows available for regex-diagnosis sampling.")
}

target_n <- ceiling(nrow(pending) * sample_fraction)
strata <- pending |>
  count(sample_stratum, name = "stratum_n") |>
  mutate(
    raw_alloc = stratum_n / sum(stratum_n) * target_n,
    alloc = pmax(1L, floor(raw_alloc)),
    alloc = pmin(alloc, stratum_n),
    frac = raw_alloc - floor(raw_alloc)
  )

while (sum(strata$alloc) < target_n) {
  candidates <- strata |>
    filter(alloc < stratum_n) |>
    arrange(desc(frac), sample_stratum)
  if (!nrow(candidates)) break
  idx <- match(candidates$sample_stratum[1], strata$sample_stratum)
  strata$alloc[idx] <- strata$alloc[idx] + 1L
}
while (sum(strata$alloc) > target_n) {
  candidates <- strata |>
    filter(alloc > 1L) |>
    arrange(frac, desc(stratum_n), sample_stratum)
  if (!nrow(candidates)) break
  idx <- match(candidates$sample_stratum[1], strata$sample_stratum)
  strata$alloc[idx] <- strata$alloc[idx] - 1L
}

set.seed(seed)
sampled <- pending |>
  left_join(strata |> select(sample_stratum, alloc), by = "sample_stratum") |>
  group_by(sample_stratum) |>
  group_modify(~ {
    n_take <- unique(.x$alloc)
    .x[sample.int(nrow(.x), size = n_take), , drop = FALSE]
  }) |>
  ungroup() |>
  arrange(sample_stratum, doc_id, suppressWarnings(as.integer(lineid))) |>
  mutate(
    regex_sample_id = row_number(),
    regex_sample_batch_id = ceiling(regex_sample_id / batch_size),
    error_category = "",
    regex_correctable = "",
    regex_rule_suggestion = "",
    regex_review_note = "",
    birth_place_new = "",
    birth_date_new = "",
    birth_year_new = "",
    birth_city_new = "",
    field_new = "",
    manual_action = "review_pending",
    manual_confidence = "",
    manual_note = ""
  )

if (nrow(sampled) != target_n) {
  stop("Sample size mismatch: expected ", target_n, ", got ", nrow(sampled))
}
if (n_distinct(paste(sampled$doc_id, sampled$lineid)) != nrow(sampled)) {
  stop("Sample contains duplicated doc_id + lineid keys.")
}

sample_cols <- c(
  "regex_sample_id", "regex_sample_batch_id",
  "doc_id", "lineid", "source_lineid", "entry_instance",
  "manual_target_reason", "birth_place_word_n",
  "has_dee_date_in_birth_place", "word_band", "sample_stratum",
  "birth_place_old", "birth_date_old", "birth_year_old", "birth_city_old",
  "field_old", "birth_place_new", "birth_date_new", "birth_year_new",
  "birth_city_new", "field_new", "manual_action", "manual_confidence",
  "manual_note", "error_category", "regex_correctable",
  "regex_rule_suggestion", "regex_review_note", "raw_text_adjusted", "raw_text"
)

sampled <- sampled |> select(all_of(sample_cols))
write_excel_csv(sampled, sample_csv, na = "")

batch_ids <- sort(unique(sampled$regex_sample_batch_id))
manifest <- bind_rows(lapply(batch_ids, function(id) {
  rows <- sampled |> filter(regex_sample_batch_id == id)
  batch_file <- file.path(in_dir, sprintf("sample_batch_%04d.csv", id))
  write_excel_csv(rows, batch_file, na = "")
  tibble(
    regex_sample_batch_id = id,
    batch_file = normalizePath(batch_file, winslash = "/", mustWork = TRUE),
    row_n = nrow(rows)
  )
}))
write_excel_csv(manifest, manifest_csv, na = "")

summary <- bind_rows(
  tibble(metric = "pending_rows", value = nrow(pending)),
  tibble(metric = "sample_fraction", value = sample_fraction),
  tibble(metric = "sample_rows", value = nrow(sampled)),
  tibble(metric = "sample_unique_doc_lineid",
         value = n_distinct(paste(sampled$doc_id, sampled$lineid))),
  tibble(metric = "sample_batches", value = nrow(manifest)),
  sampled |> count(manual_target_reason, name = "value") |>
    transmute(metric = paste0("reason:", manual_target_reason), value),
  sampled |> count(word_band, name = "value") |>
    transmute(metric = paste0("word_band:", word_band), value)
) |>
  mutate(value = as.numeric(value))
write_excel_csv(summary, summary_csv, na = "")

cat("Manual corrections:", manual_csv, "\n")
cat("Pending rows:", nrow(pending), "\n")
cat("Sample rows:", nrow(sampled), "\n")
cat("Sample batches:", nrow(manifest), "\n")
cat("Sample root:", sample_root, "\n")
