###############################################################################
# Aggregate independent audit results (1906 + 1938) into an accuracy report.
#
# Reads:
#   output/audit_<ED>/out/*.jsonl      (audit verdicts)
#   output/amws_<ED>_audit_sample.csv  (100 sampled rows with our derived data)
#
# Writes:
#   output/amws_<ED>_audit_results.csv  (per-row sample + verdict + notes)
#   output/amws_all_audit_report.md     (overall accuracy summary)
###############################################################################
suppressPackageStartupMessages({
  library(jsonlite); library(readr); library(dplyr); library(data.table)
})
source("../paths.R")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

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
      lineid      = as.integer(j$lineid       %||% NA),
      verdict     = as.character(j$verdict    %||% ""),
      audit_city  = as.character(j$audit_city %||% ""),
      audit_state = as.character(j$audit_state %||% ""),
      notes       = as.character(j$notes      %||% ""),
      stringsAsFactors = FALSE
    )
  }))
}

out_root <- AMWS_OUTPUT
report <- list()

for (ED in c("1906", "1938")) {
  audit_out <- file.path(out_root, paste0("audit_", ED), "out")
  samp_file <- file.path(out_root, sprintf("amws_%s_audit_sample.csv", ED))
  if (!dir.exists(audit_out) || !file.exists(samp_file)) {
    cat("[skip", ED, "]\n"); next
  }
  files <- sort(list.files(audit_out, pattern = "^\\d{5}\\.jsonl$",
                           full.names = TRUE))
  if (length(files) == 0) { cat("[", ED, "no audit files yet]\n"); next }
  rows <- do.call(rbind, lapply(files, read_jsonl))
  samp <- read_csv(samp_file, show_col_types = FALSE)

  merged <- merge(samp, rows, by = "lineid", all.x = TRUE)
  merged$verdict[is.na(merged$verdict) | merged$verdict == ""] <- "missing"
  write_csv(merged, file.path(out_root, sprintf("amws_%s_audit_results.csv", ED)))

  tot <- nrow(merged)
  by_v <- merged |> count(verdict)
  by_src <- merged |>
    group_by(match_source) |>
    summarise(n = n(),
              correct = sum(verdict == "correct"),
              wrong   = sum(verdict == "wrong"),
              uncertain = sum(verdict == "uncertain"),
              accuracy = correct / n)

  cat("\n=== EDITION", ED, "===\n")
  cat("audited:", tot, "  correct:", sum(merged$verdict == "correct"),
      " wrong:", sum(merged$verdict == "wrong"),
      " uncertain:", sum(merged$verdict == "uncertain"), "\n")
  cat("accuracy (correct/total):",
      round(100 * sum(merged$verdict == "correct") / tot, 1), "%\n")
  cat("\nby match_source:\n")
  print(by_src)

  report[[ED]] <- merged
}

cat("\nDone.\n")
