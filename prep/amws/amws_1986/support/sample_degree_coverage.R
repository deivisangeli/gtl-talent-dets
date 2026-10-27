# Reproducible full-edition degree audit sample. Source data are read-only.
suppressPackageStartupMessages(library(data.table))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])),
                          "..", "..", "..", ".."), winslash = "/")
} else normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/")
source(file.path(repo_root, "paths.R"))
local_dropbox <- file.path("C:/Users", Sys.info()[["user"]],
                          "Globtalent Dropbox", "gtl_talent_dets")
if (!dir.exists(TALENT_DETS_DATA_DIR) && dir.exists(local_dropbox)) {
  TALENT_DETS_DATA_DIR <- normalizePath(local_dropbox, winslash = "/")
}
input <- file.path(TALENT_DETS_DATA_DIR, "Data", "intermediary", "amws",
                   "amws_ed86_full.csv")
out_dir <- file.path(TALENT_DETS_DATA_DIR, "output", "amws", "education_pilot")
before <- unname(tools::md5sum(input))
# readr preserves embedded doubled quotes in OCR text through a CSV round trip.
data <- as.data.table(readr::read_csv(
  input, col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE, progress = FALSE
))
stopifnot(nrow(readr::problems(data)) == 0L)
stopifnot(nrow(data) >= 1000L,
          !anyDuplicated(data[, .(doc_id, lineid, entry_instance)]))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(42)
idx <- sample.int(nrow(data), 1000L, replace = FALSE)
sample <- copy(data[idx])
sample[, `:=`(sample_id = seq_len(.N), source_row = idx)]
setcolorder(sample, c("sample_id", "source_row", setdiff(names(sample),
                                                       c("sample_id", "source_row"))))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
output <- file.path(out_dir, "sample_1000.csv")
manifest_file <- file.path(out_dir, "sample_manifest.json")
if (file.exists(output)) {
  stopifnot(file.exists(manifest_file))
  old_manifest <- jsonlite::read_json(manifest_file, simplifyVector = TRUE)
  stopifnot(identical(old_manifest$source_md5, before))
  old <- readr::read_csv(output, col_types = readr::cols(.default = readr::col_character()),
                        show_col_types = FALSE, progress = FALSE)
  stopifnot(identical(as.integer(old$source_row), idx),
            identical(old$raw_text_adjusted, sample$raw_text_adjusted))
} else fwrite(sample, output, na = "")
manifest <- list(source = normalizePath(input, winslash = "/"),
                 source_md5 = before, population_n = nrow(data), sample_n = 1000L,
                 seed = 42L, rng = RNGkind(), R_version = R.version.string,
                 sampling = "uniform without replacement, original file order",
                 key = c("doc_id", "lineid", "entry_instance"))
if (!file.exists(manifest_file)) {
  jsonlite::write_json(manifest, manifest_file, auto_unbox = TRUE, pretty = TRUE)
}
stopifnot(identical(before, unname(tools::md5sum(input))))
cat("Sample:", output, "\nPopulation:", nrow(data), "\nMD5:", before, "\n")
