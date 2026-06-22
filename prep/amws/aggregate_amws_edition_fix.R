###############################################################################
# Aggregate AMWS edition manual-fix outputs + re-geocode.
# Env: AMWS_EDITION = "1906" or "1938"
#
# Reads:
#   output/manual_fix_<ED>/out/*.jsonl     (agent decisions)
#   output/amws_<ED>_cleaned.csv          (deterministic cleaning step)
#
# Writes:
#   output/manual_fix_<ED>_results.csv     (concatenated agent decisions)
#   output/amws_<ED>_cleaned_corrected.csv (cleaned + overrides applied)
#
# Then re-runs geocode_amws_edition.R on the corrected cleaned file via
# Sys.setenv(AMWS_EDITION = ED, AMWS_CLEANED_OVERRIDE = "corrected").
#
# Final output (after re-geocode):
#   output/amws_<ED>_us_geocoded_final.csv
#   output/amws_<ED>_us_still_unmatched.csv
###############################################################################

suppressPackageStartupMessages({
  library(jsonlite); library(dplyr); library(readr); library(data.table)
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

out_root   <- AMWS_OUTPUT
batch_out  <- file.path(out_root, paste0("manual_fix_", ED), "out")
cleaned_in <- file.path(out_root, sprintf("amws_%s_cleaned.csv", ED))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

read_jsonl <- function(f) {
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  lines <- iconv(lines, from = "UTF-8", to = "UTF-8", sub = "?")
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) return(NULL)
  lines <- unlist(strsplit(paste(lines, collapse = "\n"),
                            "(?<=\\})\\s*(?=\\{)", perl = TRUE))
  lines <- lines[nzchar(trimws(lines))]
  do.call(rbind, lapply(lines, function(l) {
    j <- fromJSON(l)
    data.frame(
      lineid     = as.integer(j$lineid %||% NA),
      city       = as.character(j$city       %||% ""),
      state      = as.character(j$state      %||% ""),
      country    = as.character(j$country    %||% ""),
      confidence = as.character(j$confidence %||% ""),
      notes      = as.character(j$notes      %||% ""),
      stringsAsFactors = FALSE
    )
  }))
}

files <- sort(list.files(batch_out, pattern = "^\\d{5}\\.jsonl$",
                         full.names = TRUE))
cat("edition:", ED, "batches:", length(files), "\n")

rows <- do.call(rbind, lapply(files, read_jsonl))
cat("agent rows:", nrow(rows), "\n")
write_csv(rows, file.path(out_root, sprintf("manual_fix_%s_results.csv", ED)))

# Apply overrides into the cleaned CSV
cleaned <- read_csv(cleaned_in, show_col_types = FALSE)
keys <- match(rows$lineid, cleaned$lineid)
ok <- !is.na(keys)
cleaned$city[keys[ok]]    <- rows$city[ok]
cleaned$state[keys[ok]]   <- rows$state[ok]
cleaned$country[keys[ok]] <- rows$country[ok]
# Add confidence/notes columns for downstream propagation
cleaned$mf_confidence <- ""
cleaned$mf_notes      <- ""
cleaned$mf_confidence[keys[ok]] <- rows$confidence[ok]
cleaned$mf_notes[keys[ok]]      <- rows$notes[ok]

write_csv(cleaned, file.path(out_root,
                              sprintf("amws_%s_cleaned_corrected.csv", ED)))
cat("wrote corrected cleaned CSV with", sum(ok), "overrides\n")
