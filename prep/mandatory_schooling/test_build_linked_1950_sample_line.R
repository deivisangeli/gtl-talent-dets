# Small end-to-end test for 1950 selection, reciprocal links, and row-year weights.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

root <- file.path(tempdir(), "linked_1950_sample_line_fixture")
dir.create(root, showWarnings = FALSE)
base <- file.path(root, "base")
dir.create(base, showWarnings = FALSE)
dir.create(file.path(root, "out"), showWarnings = FALSE)
output <- file.path(root, "out", "output")
source_path <- file.path(root, "sample.csv.gz")

rows <- data.frame(
  YEAR = c(1940L, 1940L, 1940L, 1950L, 1950L, 1950L),
  SERIAL = 1:6, GQ = 1L, PERNUM = 1L, VERSIONHIST = 1L,
  HISTID = c("a40", "b40", "x40", "a50", "b50", "c50"),
  HIK = c("a", "b", "x", "a", "b", "c"),
  LINK1900 = 0L, LINK1910 = 0L, LINK1920 = 0L,
  LINK1930 = 0L, LINK1940 = c(1L, 1L, 1L, 1L, 1L, 0L),
  LINK1950 = c(1L, 1L, 0L, 1L, 1L, 1L),
  VERSIONHIK = "1.0-2.0", BPL = c(1L, 2L, 9L, 1L, 2L, 3L),
  OCC1950 = c(7L, 999L, 999L, 7L, 999L, 61L),
  birth_bpl = c("001", "002", "unknown", "001", "002", "003"),
  scientist_occ1950 = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),
  stringsAsFactors = FALSE)

con <- dbConnect(duckdb())
dbWriteTable(con, "fixture", rows)
invisible(dbExecute(con, sprintf("COPY fixture TO '%s' (FORMAT PARQUET,
  PARTITION_BY (birth_bpl), FILENAME_PATTERN 'part_{i}')",
                       gsub("\\\\", "/", base))))
dbDisconnect(con, shutdown = TRUE)

dictionary <- data.frame(
  name = names(rows), label = names(rows), concept = "Fixture",
  description = "Synthetic fixture field", parquet_type = "fixture")
utils::write.csv(dictionary, file.path(base, "variable_dictionary.csv"),
                 row.names = FALSE)

sample <- rows[rows$HISTID %in% c("b40", "a50", "b50", "c50"),
               c("YEAR", "SERIAL", "GQ", "PERNUM", "VERSIONHIST", "HISTID",
                 "HIK", paste0("LINK", seq(1900, 1950, 10)), "VERSIONHIK")]
sample$SLWT <- ifelse(sample$YEAR == 1940L, 20, 5)
sample$SLREC <- 2L
sample$VETWWII <- NA_integer_
sample$VETWWI <- 0L
sample <- sample[, c("YEAR", "SERIAL", "GQ", "PERNUM", "SLWT", "SLREC",
                     "VERSIONHIST", "VETWWII", "VETWWI", "HISTID", "HIK",
                     paste0("LINK", seq(1900, 1950, 10)), "VERSIONHIK")]
connection <- gzfile(source_path, "wt")
utils::write.csv(sample, connection, row.names = FALSE, na = "")
close(connection)

Sys.setenv(IPUMS_1950_SAMPLE_LINE_SOURCE = source_path,
           IPUMS_LINKED_1950_PARQUET_DIR = base,
           IPUMS_1950_SAMPLE_LINE_PARQUET_DIR = output,
           IPUMS_1950_SAMPLE_LINE_EXPECTED_COUNT = "3")
source("prep/mandatory_schooling/build_linked_1950_sample_line.R", local = TRUE)
main()

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
glob <- gsub("\\\\", "/", file.path(output, "birth_bpl=*", "*.parquet"))
actual <- dbGetQuery(con, sprintf("SELECT YEAR,HISTID,HIK,SLWT,SLREC,
  birth_bpl,scientist_occ1950 FROM read_parquet('%s',hive_partitioning=true)
  ORDER BY HISTID", glob))
stopifnot(nrow(actual) == 5L,
          identical(sort(actual$HISTID), sort(c("a40", "a50", "b40", "b50", "c50"))))
by_id <- setNames(seq_len(nrow(actual)), actual$HISTID)
stopifnot(is.na(actual$SLWT[by_id[["a40"]]]),
          is.na(actual$SLREC[by_id[["a40"]]]),
          actual$SLWT[by_id[["b40"]]] == 20,
          actual$SLWT[by_id[["a50"]]] == 5,
          actual$SLWT[by_id[["b50"]]] == 5,
          actual$SLWT[by_id[["c50"]]] == 5,
          actual$birth_bpl[by_id[["a40"]]] == "001",
          isTRUE(actual$scientist_occ1950[by_id[["a50"]]]),
          file.exists(file.path(output, "_manifest.json")),
          file.exists(file.path(output, "field_dictionary.csv")))
manifest <- jsonlite::read_json(file.path(output, "_manifest.json"))
stopifnot(manifest$flagged_1940_links == 2L,
          manifest$matched_1940_links == 2L,
          manifest$flagged_links_without_1940_row == 0L)
message("1950 sample-line linked fixture passed")
