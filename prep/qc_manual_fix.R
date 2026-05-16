# qc_manual_fix.R
#
# Random 100-row QC sample of manual-fixed rows after aggregation.
# Writes a CSV that pairs birthplace_orig with the agent's repair + final geoid.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})
source("../paths.R")

out_dir <- AMWS_OUTPUT

final  <- read_csv(file.path(out_dir, "amws_1955_us_geocoded_final.csv"),
                   show_col_types = FALSE)
fixes  <- read_csv(file.path(out_dir, "manual_fix_results.csv"),
                   show_col_types = FALSE)

fixed_rows <- final |> filter(lineid %in% fixes$lineid)
cat("manual-fixed rows that successfully geocoded:", nrow(fixed_rows), "\n")

set.seed(42)
qc <- fixed_rows |>
  slice_sample(n = min(100, nrow(fixed_rows))) |>
  select(lineid, birthplace_orig, city, state, county_name, geoid,
         confidence, match_source, notes) |>
  arrange(confidence, lineid)

write_csv(qc, file.path(out_dir, "amws_1955_manual_fix_qc100.csv"))

cat("\n=== QC sample (100 rows) written to amws_1955_manual_fix_qc100.csv ===\n")
cat("\nconfidence dist in QC sample:\n")
print(table(qc$confidence, useNA = "ifany"))

cat("\n=== still_unmatched preview ===\n")
unmatched <- read_csv(file.path(out_dir, "amws_1955_us_still_unmatched.csv"),
                      show_col_types = FALSE)
cat("count:", nrow(unmatched), "\n")
print(unmatched |> count(state, sort = TRUE) |> head(20) |> as.data.frame())
