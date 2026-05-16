# prep_manual_fix_batches.R
#
# Combine the unmatched (1,408) and agent-cleaning-suspect (378) rows from the
# US geocoder into one set, write per-batch JSONL inputs for subagent manual
# repair. One batch = 5 rows.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(jsonlite)
})
source("../paths.R")

out_root <- AMWS_OUTPUT
batch_dir <- file.path(out_root, "manual_fix")
in_dir   <- file.path(batch_dir, "in")
out_dir  <- file.path(batch_dir, "out")
dir.create(in_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

um <- read_csv(file.path(out_root, "amws_1955_us_unmatched.csv"),
               show_col_types = FALSE) |>
  mutate(source_set = "unmatched")
su <- read_csv(file.path(out_root, "amws_1955_us_geocoded_suspects.csv"),
               show_col_types = FALSE) |>
  transmute(lineid, birthplace_orig, city, state, source_set = "suspect")

stopifnot(length(intersect(um$lineid, su$lineid)) == 0)

all <- bind_rows(um |> select(lineid, birthplace_orig, city, state, source_set),
                 su) |>
  arrange(lineid)
cat("total rows for manual fix:", nrow(all), "\n")
cat("  unmatched:", sum(all$source_set == "unmatched"), "\n")
cat("  suspect:  ", sum(all$source_set == "suspect"),  "\n")

batch_size <- 5
n <- nrow(all)
n_batches <- ceiling(n / batch_size)
cat("batches (5 rows each):", n_batches, "\n")

for (b in seq_len(n_batches)) {
  rng <- ((b - 1) * batch_size + 1):min(b * batch_size, n)
  rows <- all[rng, ]
  lines <- vapply(seq_len(nrow(rows)), function(i) {
    toJSON(list(
      lineid          = unbox(rows$lineid[i]),
      birthplace_orig = unbox(rows$birthplace_orig[i] %||% ""),
      cleaned_city    = unbox(rows$city[i] %||% ""),
      cleaned_state   = unbox(rows$state[i] %||% ""),
      source_set      = unbox(rows$source_set[i])
    ), auto_unbox = FALSE)
  }, character(1))
  writeLines(lines, file.path(in_dir, sprintf("%05d.jsonl", b)), useBytes = TRUE)
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a) || length(a) == 0) b else a
cat("wrote", n_batches, "batches to", in_dir, "\n")
