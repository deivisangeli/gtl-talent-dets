###############################################################################
# Prepare a reproducible manual pilot for AMWS Ed86 rows excluded from the
# final dataset because birth_city, birth_year, or birth_country is missing.
#
# This script never modifies the canonical AMWS files.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..",
                                       ".."), winslash = "/", mustWork = TRUE)
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

data_dir <- file.path(TALENT_DETS_DATA_DIR, "Data")
input_file <- file.path(data_dir, "processed", "amws", "amws_ed86.csv")
pilot_root <- file.path(
  data_dir, "intermediary", "amws",
  "manual_missing_birth_info_pilot_20260713"
)
in_dir <- file.path(pilot_root, "in")
out_dir <- file.path(pilot_root, "out")
dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sample_n <- 200L
batch_size <- 50L
random_seed <- 20260713L

input <- read_csv(input_file, col_types = cols(.default = col_character()),
                  show_col_types = FALSE, progress = FALSE) |>
  mutate(across(everything(), blank_na))

required_cols <- c(
  "doc_id", "lineid", "entry_instance", "name_raw", "field",
  "raw_text_adjusted", "birth_place", "birth_date", "birth_year",
  "birth_city", "birth_state", "birth_country", "geocoding_status"
)
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input is missing required columns: ", paste(missing_cols, collapse = ", "))
}
if (nrow(input) != 94809L ||
    n_distinct(paste(input$doc_id, input$lineid, sep = "\r")) != nrow(input)) {
  stop("Unexpected input row count or duplicated doc_id + lineid keys.")
}

eligible <- input |>
  mutate(
    missing_birth_city = !nzchar(str_trim(birth_city)),
    missing_birth_year = !nzchar(str_trim(birth_year)),
    missing_birth_country = !nzchar(str_trim(birth_country)),
    missing_fields = paste0(
      if_else(missing_birth_city, "birth_city;", ""),
      if_else(missing_birth_year, "birth_year;", ""),
      if_else(missing_birth_country, "birth_country;", "")
    ) |>
      str_remove(";$")
  ) |>
  filter(missing_birth_city | missing_birth_year | missing_birth_country) |>
  arrange(doc_id, suppressWarnings(as.integer(lineid)), lineid)

if (nrow(eligible) != 14483L) {
  stop("Expected 14,483 eligible rows, found ", nrow(eligible), ".")
}

RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion",
        sample.kind = "Rejection")
set.seed(random_seed)
sample_index <- sample.int(nrow(eligible), size = sample_n, replace = FALSE)

sampled <- eligible[sample_index, , drop = FALSE] |>
  mutate(
    sample_id = sprintf("%03d", row_number()),
    batch_id = sprintf("%02d", (row_number() - 1L) %/% batch_size + 1L)
  ) |>
  transmute(
    sample_id, batch_id, doc_id, lineid, entry_instance, name_raw,
    missing_birth_city, missing_birth_year, missing_birth_country,
    missing_fields,
    field_old = field,
    raw_text_adjusted,
    birth_place_old = birth_place,
    birth_date_old = birth_date,
    birth_year_old = birth_year,
    birth_city_old = birth_city,
    birth_state_old = birth_state,
    birth_country_old = birth_country,
    geocoding_status,
    birth_place_proposed = "",
    birth_date_proposed = "",
    birth_year_proposed = "",
    birth_city_proposed = "",
    birth_state_proposed = "",
    birth_country_proposed = "",
    recovery_status = "review_pending",
    manual_confidence = "",
    evidence_basis = "",
    other_issue_flag = "",
    manual_note = "",
    reviewer_id = ""
  )

if (nrow(sampled) != sample_n ||
    n_distinct(paste(sampled$doc_id, sampled$lineid, sep = "\r")) != sample_n ||
    any((sampled |> count(batch_id))$n != batch_size)) {
  stop("Sample or batch validation failed.")
}

master_file <- file.path(pilot_root,
                         "amws_ed86_missing_birth_info_sample200_master.csv")
write_excel_csv(sampled, master_file, na = "")

for (batch in split(sampled, sampled$batch_id)) {
  batch_id <- unique(batch$batch_id)
  write_excel_csv(
    batch,
    file.path(in_dir,
              paste0("amws_ed86_missing_birth_info_batch_", batch_id, ".csv")),
    na = ""
  )
}

pattern_counts <- eligible |>
  count(missing_fields, name = "eligible_rows") |>
  arrange(desc(eligible_rows))
sample_pattern_counts <- sampled |>
  count(missing_fields, name = "sample_rows")

summary <- bind_rows(
  tibble(metric = "input_file", value = normalizePath(input_file, winslash = "/")),
  tibble(metric = "input_md5", value = unname(tools::md5sum(input_file))),
  tibble(metric = "input_rows", value = as.character(nrow(input))),
  tibble(metric = "eligible_rows", value = as.character(nrow(eligible))),
  tibble(metric = "random_seed", value = as.character(random_seed)),
  tibble(metric = "rng_kind", value = "Mersenne-Twister/Inversion/Rejection"),
  tibble(metric = "sample_rows", value = as.character(nrow(sampled))),
  tibble(metric = "batch_size", value = as.character(batch_size)),
  tibble(metric = "batch_count", value = as.character(n_distinct(sampled$batch_id))),
  pattern_counts |>
    transmute(metric = paste0("eligible_pattern:", missing_fields),
              value = as.character(eligible_rows)),
  sample_pattern_counts |>
    transmute(metric = paste0("sample_pattern:", missing_fields),
              value = as.character(sample_rows))
)
write_excel_csv(summary,
                file.path(pilot_root,
                          "amws_ed86_missing_birth_info_sample200_prep_summary.csv"),
                na = "")

cat("Eligible rows:", nrow(eligible), "\n")
cat("Sample rows:", nrow(sampled), "\n")
cat("Batches:", n_distinct(sampled$batch_id), "\n")
cat("Pilot root:", pilot_root, "\n")
