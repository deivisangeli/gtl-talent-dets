suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(readr)
})
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

out_dir <- file.path(AMWS_OUTPUT, "amws_1955_batches", "out")
in_dir  <- file.path(AMWS_OUTPUT, "amws_1955_batches", "in")

files <- sort(list.files(out_dir, pattern = "^\\d{5}\\.jsonl$", full.names = TRUE))
cat("Batch files found:", length(files), "\n\n")

rows <- do.call(rbind, lapply(files, function(f) {
  lines <- readLines(f, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  do.call(rbind, lapply(lines, function(l) as.data.frame(fromJSON(l), stringsAsFactors = FALSE)))
}))
cat("Total cleaned rows:", nrow(rows), "\n\n")

cat("=== flag counts ===\n")
print(table(rows$flag, useNA = "ifany"))

cat("\n=== country counts (top 15) ===\n")
print(sort(table(rows$country), decreasing = TRUE)[1:15])

cat("\n=== nat counts ===\n")
print(table(nzchar(rows$nat), useNA = "ifany"))
cat("\nNon-empty nat values:\n")
print(table(rows$nat[nzchar(rows$nat)]))

cat("\n=== state counts for USA (top 15) ===\n")
us <- rows[rows$country == "USA", ]
print(sort(table(us$state), decreasing = TRUE)[1:15])

cat("\n=== rows with empty city (potential losses) ===\n")
empty_city <- rows[!nzchar(rows$city), ]
cat("count:", nrow(empty_city), "\n")
if (nrow(empty_city) > 0) {
  print(empty_city[, c("lineid", "city", "country", "date", "nat", "flag")])
}

cat("\n=== rows with empty date (no_date or related) ===\n")
empty_date <- rows[!nzchar(rows$date), ]
cat("count:", nrow(empty_date), "\n")
if (nrow(empty_date) > 0 && nrow(empty_date) <= 20) {
  print(empty_date[, c("lineid", "city", "country", "date", "nat", "flag")])
}

cat("\n=== sanity check: nat field cross-referenced with original ===\n")
raw_tsv <- read_tsv(file.path(AMWS_OUTPUT, "amws_1955_batches", "amws_1955_raw.tsv"), show_col_types = FALSE)
nat_rows <- rows[nzchar(rows$nat), ]
mismatches <- 0
for (i in seq_len(nrow(nat_rows))) {
  lid <- nat_rows$lineid[i]
  orig <- raw_tsv$birthplace_orig[raw_tsv$lineid == lid]
  if (length(orig) == 0) next
  has_nat <- grepl("\\bnat\\b", orig, ignore.case = TRUE)
  if (!has_nat) {
    mismatches <- mismatches + 1
    cat(sprintf("  MISMATCH lineid=%d: nat='%s' but orig='%s'\n",
                lid, nat_rows$nat[i], orig))
  }
}
cat(sprintf("Total nat rows: %d. Mismatches (nat output but no 'nat' in original): %d\n",
            nrow(nat_rows), mismatches))

cat("\n=== 12 random rows ===\n")
set.seed(1)
samp <- sample(nrow(rows), 12)
samp_rows <- rows[samp, c("lineid", "city", "state", "country", "date", "nat", "flag")]
for (i in seq_len(nrow(samp_rows))) {
  lid <- samp_rows$lineid[i]
  orig <- raw_tsv$birthplace_orig[raw_tsv$lineid == lid]
  cat(sprintf("[%d] orig: %s\n", lid, orig))
  cat(sprintf("     -> city=%s | st=%s | ctry=%s | date=%s | nat=%s | flag=%s\n",
              samp_rows$city[i], samp_rows$state[i], samp_rows$country[i],
              samp_rows$date[i], samp_rows$nat[i], samp_rows$flag[i]))
}
