suppressPackageStartupMessages({
  library(data.table)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..", ".."),
                             winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

local_dropbox <- file.path("C:/Users", Sys.info()[["user"]],
                           "Globtalent Dropbox", "gtl_talent_dets")
if (!dir.exists(TALENT_DETS_DATA_DIR) && dir.exists(local_dropbox)) {
  TALENT_DETS_DATA_DIR <- normalizePath(local_dropbox, winslash = "/")
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
}

output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
input_file <- file.path(output_dir,
                        "amws_ed16_all_countries_geocoded_us_only_regex_enhanced.csv")
audit_root <- file.path(output_dir,
                        "manual_audit_sample1000_birth_city_year_regex_enhanced_20260709")
in_dir <- file.path(audit_root, "batches", "in")
out_dir <- file.path(audit_root, "batches", "out")
dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260709L)
needed <- c(
  "doc_id", "lineid", "source_file", "name_raw", "birth_place", "birth_date",
  "birth_city", "birth_state", "birth_country", "birth_year",
  "birth_year_parse_flag", "birth_city_parse_flag",
  "birth_location_parse_flag", "birth_location_format_problem",
  "raw_text_adjusted"
)
data <- fread(input_file, colClasses = "character", select = needed,
              showProgress = FALSE)
data[is.na(data)] <- ""
if (nrow(data) < 1000L) stop("Input has fewer than 1000 rows.")

sample_idx <- sample.int(nrow(data), 1000L)
sample <- data[sample_idx]
sample[, audit_id := sprintf("audit_%04d", seq_len(.N))]
setcolorder(sample, c("audit_id", setdiff(names(sample), "audit_id")))

sample_file <- file.path(audit_root, "sample1000_input.csv")
fwrite(sample, sample_file, bom = TRUE)

batch_size <- 50L
sample[, batch_id := sprintf("batch_%03d", ceiling(seq_len(.N) / batch_size))]
manifest <- sample[, .(n = .N), by = batch_id][order(batch_id)]
manifest[, input_file := file.path(in_dir, paste0(batch_id, ".csv"))]
manifest[, output_file := file.path(out_dir, paste0(batch_id, "_reviewed.csv"))]

for (bid in manifest$batch_id) {
  fwrite(sample[batch_id == bid][, batch_id := NULL],
         file.path(in_dir, paste0(bid, ".csv")),
         bom = TRUE)
}
fwrite(manifest, file.path(audit_root, "batch_manifest.csv"), bom = TRUE)

cat("Audit root:", audit_root, "\n")
cat("Sample:", sample_file, "\n")
cat("Batches:", nrow(manifest), "\n")
