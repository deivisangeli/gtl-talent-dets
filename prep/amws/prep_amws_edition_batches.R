###############################################################################
# Write per-batch JSONL inputs for AMWS edition manual-fix subagent dispatch.
# Env: AMWS_EDITION = "1906" or "1938"
#
# Source rows: prep/output/amws_<ED>_us_unmatched.csv
# Output:      prep/output/manual_fix_<ED>/in/NNNNN.jsonl (5 rows each)
###############################################################################
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(jsonlite)
})
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
ED <- Sys.getenv("AMWS_EDITION", unset = "1906")
stopifnot(ED %in% c("1906", "1938"))

out_root  <- AMWS_OUTPUT
batch_dir <- file.path(out_root, paste0("manual_fix_", ED))
in_dir    <- file.path(batch_dir, "in")
out_dir   <- file.path(batch_dir, "out")
dir.create(in_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

um <- read_csv(file.path(out_root, sprintf("amws_%s_us_unmatched.csv", ED)),
               show_col_types = FALSE) |>
  arrange(lineid)
cat("edition:", ED, "rows for manual fix:", nrow(um), "\n")

batch_size <- 5
n_batches  <- ceiling(nrow(um) / batch_size)
cat("batches (5/each):", n_batches, "\n")

`%||%` <- function(a, b) if (is.null(a) || (length(a)==1 && is.na(a)) || length(a)==0) b else a

for (b in seq_len(n_batches)) {
  rng <- ((b - 1) * batch_size + 1):min(b * batch_size, nrow(um))
  rows <- um[rng, ]
  lines <- vapply(seq_len(nrow(rows)), function(i) {
    toJSON(list(
      lineid          = unbox(as.integer(rows$lineid[i])),
      birthplace_orig = unbox(rows$birthplace_orig[i] %||% ""),
      cleaned_city    = unbox(rows$city[i] %||% ""),
      cleaned_state   = unbox(rows$state[i] %||% "")
    ), auto_unbox = FALSE)
  }, character(1))
  writeLines(lines, file.path(in_dir, sprintf("%05d.jsonl", b)),
             useBytes = TRUE)
}
cat("wrote", n_batches, "batches to", in_dir, "\n")
