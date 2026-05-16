suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(readr)
})

source("../paths.R")

batch_dir <- file.path(AMWS_OUTPUT, "amws_1955_batches")
out_dir   <- file.path(batch_dir, "out")
raw_tsv   <- file.path(batch_dir, "amws_1955_raw.tsv")
agg_csv   <- file.path(AMWS_OUTPUT, "amws_1955_cleaned.csv")

files <- sort(list.files(out_dir, pattern = "^\\d{5}\\.jsonl$", full.names = TRUE))
cat("batches:", length(files), "\n")

read_jsonl <- function(f) {
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  lines <- iconv(lines, from = "UTF-8", to = "UTF-8", sub = "?")
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) return(NULL)
  # some agents wrote multiple JSON objects on one line without separator
  lines <- unlist(strsplit(paste(lines, collapse = "\n"), "(?<=\\})\\s*(?=\\{)", perl = TRUE))
  lines <- lines[nzchar(trimws(lines))]
  do.call(rbind, lapply(lines, function(l) {
    j <- fromJSON(l)
    data.frame(
      lineid  = as.integer(j$lineid %||% NA),
      city    = as.character(j$city    %||% ""),
      state   = as.character(j$state   %||% ""),
      country = as.character(j$country %||% ""),
      date    = as.character(j$date    %||% ""),
      nat     = as.character(j$nat     %||% ""),
      flag    = as.character(j$flag    %||% ""),
      stringsAsFactors = FALSE
    )
  }))
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

rows <- do.call(rbind, lapply(files, read_jsonl))
cat("rows from jsonl:", nrow(rows), "\n")

raw <- read_tsv(raw_tsv, show_col_types = FALSE)
cat("rows in raw input:", nrow(raw), "\n")

merged <- raw |>
  left_join(rows, by = "lineid")

write_csv(merged, agg_csv)
cat("wrote", agg_csv, "with", nrow(merged), "rows\n")

cat("\n=== country counts (top 20) ===\n")
print(sort(table(merged$country, useNA = "ifany"), decreasing = TRUE)[1:20])

us <- merged |> filter(country == "USA")
cat("\nUS rows:", nrow(us), "\n")
cat("US distinct (city, state):", n_distinct(us[, c("city", "state")]), "\n")

cat("\n=== top 25 US (city, state) ===\n")
print(us |> count(city, state, sort = TRUE) |> head(25))

cat("\nUS empty city:", sum(!nzchar(us$city)), "\n")
cat("US empty state:", sum(!nzchar(us$state)), "\n")
cat("US flag != '' :\n")
print(table(us$flag[nzchar(us$flag)]))
