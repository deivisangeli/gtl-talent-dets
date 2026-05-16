###############################################################################
# Sample 100 random rows per edition from the final geocoded table, split into
# 20 batches of 5 rows each, and write JSONL for the independent audit agents.
# Env: AMWS_EDITION ("1906" or "1938"). Seed = 42 (reproducible).
###############################################################################
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(jsonlite)
})
source("../paths.R")
ED <- Sys.getenv("AMWS_EDITION", unset = "1906")
stopifnot(ED %in% c("1906", "1938"))

out_root <- AMWS_OUTPUT
geo_file <- file.path(out_root, sprintf("amws_%s_us_geocoded_final.csv", ED))
audit_dir <- file.path(out_root, paste0("audit_", ED))
in_dir    <- file.path(audit_dir, "in")
out_dir   <- file.path(audit_dir, "out")
dir.create(in_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

g <- read_csv(geo_file, show_col_types = FALSE)
cat("edition:", ED, "final geocoded rows:", nrow(g), "\n")
set.seed(42)
samp <- g[sample(nrow(g), min(100, nrow(g))), ] |>
  arrange(lineid)
write_csv(samp, file.path(out_root, sprintf("amws_%s_audit_sample.csv", ED)))

`%||%` <- function(a, b) if (is.null(a) || length(a)==0 || is.na(a)) b else a

for (b in seq_len(20)) {
  rng <- ((b - 1) * 5 + 1):min(b * 5, nrow(samp))
  rows <- samp[rng, ]
  lines <- vapply(seq_len(nrow(rows)), function(i) {
    toJSON(list(
      lineid          = unbox(as.integer(rows$lineid[i])),
      birthplace_orig = unbox(rows$birthplace_orig[i] %||% ""),
      our_city        = unbox(rows$city[i]            %||% ""),
      our_state       = unbox(rows$state[i]           %||% ""),
      our_county      = unbox(rows$county_name[i]     %||% ""),
      our_geoid       = unbox(as.character(rows$geoid[i]) %||% ""),
      our_match_src   = unbox(rows$match_source[i]    %||% "")
    ), auto_unbox = FALSE)
  }, character(1))
  writeLines(lines, file.path(in_dir, sprintf("%05d.jsonl", b)), useBytes = TRUE)
}
cat("wrote 20 audit batches to", in_dir, "\n")
