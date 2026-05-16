###############################################################################
# Concatenate all enrollment-research agent outputs into a single TSV with
# columns: school, state_abbr, founding_year, year10_seats, year20_seats,
# year30_seats, year10_year_used, ..., year10_url, ..., confidence, notes.
#
# Output: prep/output/elite_high_schools_enrollment_v2.tsv
###############################################################################
suppressPackageStartupMessages({
  library(jsonlite); library(data.table)
})
source("../paths.R")
out_root <- file.path(SCHOOLS_OUTPUT, "enrollment_research", "out")
files <- list.files(out_root, pattern = "\\.jsonl$", full.names = TRUE)
cat("agent output files:", length(files), "\n")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a) || identical(a, "")) b else a
chr <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "" else as.character(x)
int <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_integer_)
  v <- suppressWarnings(as.integer(x))
  if (length(v) == 0) NA_integer_ else v
}

rows <- lapply(files, function(f) {
  txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!nzchar(trimws(txt))) return(NULL)
  j <- tryCatch(fromJSON(txt), error = function(e) NULL)
  if (is.null(j)) { cat("BAD JSON:", f, "\n"); return(NULL) }
  data.table(
    school            = chr(j$school),
    state_abbr        = chr(j$state_abbr),
    founding_year     = int(j$founding_year),
    year10_seats      = int(j$year10_seats),
    year10_year_used  = int(j$year10_year_used),
    year10_url        = chr(j$year10_url),
    year20_seats      = int(j$year20_seats),
    year20_year_used  = int(j$year20_year_used),
    year20_url        = chr(j$year20_url),
    year30_seats      = int(j$year30_seats),
    year30_year_used  = int(j$year30_year_used),
    year30_url        = chr(j$year30_url),
    confidence        = chr(j$confidence),
    notes             = chr(j$notes)
  )
})
rows <- rbindlist(rows, fill = TRUE)
rows <- unique(rows, by = c("school","state_abbr"))
fwrite(rows, file.path(SCHOOLS_OUTPUT, "elite_high_schools_enrollment_v2.tsv"), sep = "\t")

cat("\n--- Coverage of v2 enrollment ---\n")
cat("schools:", nrow(rows), "\n")
cat("  with year10:", sum(!is.na(rows$year10_seats)), "\n")
cat("  with year20:", sum(!is.na(rows$year20_seats)), "\n")
cat("  with year30:", sum(!is.na(rows$year30_seats)), "\n")
cat("  by confidence:\n"); print(rows[, .N, by = confidence])
