###############################################################################
# Split AMWS Ed16 expanded birth-place manual-correction targets into batch CSVs.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_expanded_birth_place_manual_corrections.csv
#
# Writes:
#   output/amws/regex_all_docs/manual_birth_place_batches/in/
#     batch_0001.csv, ...
#   output/amws/regex_all_docs/manual_birth_place_batches/batch_manifest.csv
#
# Environment overrides:
#   AMWS_ED16_MANUAL_BP_OUTPUT_DIR
#   AMWS_ED16_MANUAL_BP_BATCH_SIZE
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

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_MANUAL_BP_OUTPUT_DIR", default_output_dir)
batch_size <- env_int("AMWS_ED16_MANUAL_BP_BATCH_SIZE", 25L)

output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
manual_csv <- file.path(output_dir,
                        "amws_ed16_expanded_birth_place_manual_corrections.csv")
batch_root <- file.path(output_dir, "manual_birth_place_batches")
in_dir <- file.path(batch_root, "in")
out_dir <- file.path(batch_root, "out")
dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

targets <- read_csv(manual_csv, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c(
  "doc_id", "lineid", "source_lineid", "entry_instance",
  "manual_target_reason", "birth_place_word_n",
  "has_dee_date_in_birth_place", "birth_place_old", "birth_date_old",
  "birth_year_old", "birth_city_old", "field_old", "birth_place_new",
  "birth_date_new", "birth_year_new", "birth_city_new", "field_new",
  "manual_action", "manual_confidence", "manual_note", "raw_text_adjusted"
)
missing_cols <- setdiff(required_cols, names(targets))
if (length(missing_cols)) {
  stop("Manual corrections table is missing required columns: ",
       paste(missing_cols, collapse = ", "))
}

ordered_targets <- targets |>
  mutate(
    batch_priority = case_when(
      has_dee_date_in_birth_place == "TRUE" |
        has_dee_date_in_birth_place == TRUE ~ 1L,
      suppressWarnings(as.integer(birth_place_word_n)) >= 6L ~ 2L,
      TRUE ~ 3L
    )
  ) |>
  arrange(batch_priority, desc(suppressWarnings(as.integer(birth_place_word_n))),
          doc_id, suppressWarnings(as.integer(lineid))) |>
  mutate(batch_id = ceiling(row_number() / batch_size))

pending <- ordered_targets |>
  filter(manual_action == "review_pending")

if (!nrow(pending)) {
  manifest <- tibble(batch_id = integer(), batch_file = character(),
                     row_n = integer(), priority_min = integer(),
                     priority_max = integer())
  readr::write_excel_csv(manifest, file.path(batch_root, "batch_manifest.csv"),
                         na = "")
  cat("No review_pending rows to batch.\n")
  quit(save = "no", status = 0)
}

batch_ids <- sort(unique(pending$batch_id))
manifest <- bind_rows(lapply(batch_ids, function(id) {
  rows <- pending |> filter(batch_id == id)
  batch_file <- file.path(in_dir, sprintf("batch_%04d.csv", id))
  readr::write_excel_csv(
    rows |>
      select(batch_id, all_of(required_cols)),
    batch_file,
    na = ""
  )
  tibble(
    batch_id = id,
    batch_file = normalizePath(batch_file, winslash = "/", mustWork = TRUE),
    row_n = nrow(rows),
    priority_min = min(rows$batch_priority),
    priority_max = max(rows$batch_priority)
  )
}))

readr::write_excel_csv(manifest, file.path(batch_root, "batch_manifest.csv"),
                       na = "")

cat("Manual corrections:", manual_csv, "\n")
cat("Pending rows:", nrow(pending), "\n")
cat("Batch size:", batch_size, "\n")
cat("Batches:", nrow(manifest), "\n")
cat("Batch input dir:", in_dir, "\n")
cat("Batch output dir:", out_dir, "\n")
