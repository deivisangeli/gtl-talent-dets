suppressPackageStartupMessages({library(jsonlite); library(data.table)})
out_dir <- "prep/output/enrollment_research/out"
files <- list.files(out_dir, pattern="\\.jsonl$", full.names=TRUE)
rows <- lapply(files, function(f) {
  j <- tryCatch(fromJSON(paste(readLines(f, warn=FALSE), collapse="\n")),
                 error=function(e) NULL)
  if (is.null(j)) return(NULL)
  to_int <- function(x) if (is.null(x) || x=="" || is.na(x)) NA_integer_ else as.integer(x)
  data.table(school=j$school, state=j$state_abbr,
             y10=to_int(j$year10_seats), y20=to_int(j$year20_seats),
             y30=to_int(j$year30_seats), conf=j$confidence)
})
d <- rbindlist(rows, fill=TRUE)
cat("Total schools:", nrow(d), "\n")
cat("\nBy confidence level:\n"); print(d[, .N, by=conf][order(-N)])
cat("\nCoverage:\n")
cat("  any year10:", sum(!is.na(d$y10)), "\n")
cat("  any year20:", sum(!is.na(d$y20)), "\n")
cat("  any year30:", sum(!is.na(d$y30)), "\n")
cat("  all three:", sum(!is.na(d$y10) & !is.na(d$y20) & !is.na(d$y30)), "\n")
cat("  exactly zero:", sum(is.na(d$y10) & is.na(d$y20) & is.na(d$y30)), "\n")
cat("\n--- 5 high-confidence rows ---\n")
print(d[conf=="high"][, .(school, state, y10, y20, y30)])
cat("\n--- 5 medium-confidence rows ---\n")
print(head(d[conf=="medium"][, .(school, state, y10, y20, y30)], 5))
cat("\n--- 5 low-confidence rows ---\n")
print(head(d[conf=="low"][, .(school, state, y10, y20, y30)], 5))
