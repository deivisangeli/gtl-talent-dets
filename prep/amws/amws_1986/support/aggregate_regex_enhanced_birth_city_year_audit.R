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

audit_root <- file.path(DATA_OUTPUT, "amws", "regex_all_docs",
                        "manual_audit_sample1000_birth_city_year_regex_enhanced_20260709")
manifest_file <- file.path(audit_root, "batch_manifest.csv")
sample_file <- file.path(audit_root, "sample1000_input.csv")
reviewed_file <- file.path(audit_root, "sample1000_reviewed.csv")
summary_file <- file.path(audit_root, "sample1000_summary.csv")

manifest <- fread(manifest_file, colClasses = "character", showProgress = FALSE)
sample <- fread(sample_file, colClasses = "character", showProgress = FALSE)
sample[is.na(sample)] <- ""

review_files <- manifest$output_file
missing_files <- review_files[!file.exists(review_files)]
if (length(missing_files)) {
  stop("Missing reviewed batch files:\n", paste(missing_files, collapse = "\n"))
}

reviewed <- rbindlist(lapply(review_files, function(path) {
  x <- fread(path, colClasses = "character", showProgress = FALSE)
  x[, source_review_file := basename(path)]
  x
}), fill = TRUE)
reviewed[is.na(reviewed)] <- ""

required_cols <- c(
  "audit_id", "doc_id", "lineid",
  "birth_city_audit_status", "birth_year_audit_status",
  "raw_birth_city", "raw_birth_year", "audit_note"
)
missing_cols <- setdiff(required_cols, names(reviewed))
if (length(missing_cols)) {
  stop("Reviewed data missing columns: ", paste(missing_cols, collapse = ", "))
}
if (nrow(reviewed) != 1000L) stop("Expected 1000 reviewed rows, got ", nrow(reviewed))
dup <- reviewed[, .N, by = audit_id][N > 1L]
if (nrow(dup)) stop("Duplicated audit_id in reviewed data.")
missing_ids <- setdiff(sample$audit_id, reviewed$audit_id)
extra_ids <- setdiff(reviewed$audit_id, sample$audit_id)
if (length(missing_ids) || length(extra_ids)) {
  stop("Reviewed audit_ids do not match sample.")
}

allowed <- c("correct", "incorrect", "raw_missing", "unclear")
bad_city <- setdiff(unique(reviewed$birth_city_audit_status), allowed)
bad_year <- setdiff(unique(reviewed$birth_year_audit_status), allowed)
if (length(bad_city) || length(bad_year)) {
  stop("Invalid audit status values.")
}

reviewed[, city_effective_correct :=
           birth_city_audit_status == "correct" |
           (birth_city_audit_status == "raw_missing" & trimws(birth_city) == "")]
reviewed[, year_effective_correct :=
           birth_year_audit_status == "correct" |
           (birth_year_audit_status == "raw_missing" & trimws(birth_year) == "")]
reviewed[, both_effective_correct := city_effective_correct & year_effective_correct]

summary <- rbindlist(list(
  data.table(metric = "reviewed_rows", value = nrow(reviewed)),
  data.table(metric = "birth_city_correct_or_correctly_blank",
             value = sum(reviewed$city_effective_correct)),
  data.table(metric = "birth_city_incorrect",
             value = sum(reviewed$birth_city_audit_status == "incorrect")),
  data.table(metric = "birth_city_raw_missing",
             value = sum(reviewed$birth_city_audit_status == "raw_missing")),
  data.table(metric = "birth_city_unclear",
             value = sum(reviewed$birth_city_audit_status == "unclear")),
  data.table(metric = "birth_year_correct_or_correctly_blank",
             value = sum(reviewed$year_effective_correct)),
  data.table(metric = "birth_year_incorrect",
             value = sum(reviewed$birth_year_audit_status == "incorrect")),
  data.table(metric = "birth_year_raw_missing",
             value = sum(reviewed$birth_year_audit_status == "raw_missing")),
  data.table(metric = "birth_year_unclear",
             value = sum(reviewed$birth_year_audit_status == "unclear")),
  data.table(metric = "both_correct_or_correctly_blank",
             value = sum(reviewed$both_effective_correct))
))
summary[, percent := round(100 * as.numeric(value) / nrow(reviewed), 2)]
summary[metric == "reviewed_rows", percent := 100]

setorder(reviewed, audit_id)
fwrite(reviewed, reviewed_file, bom = TRUE)
fwrite(summary, summary_file, bom = TRUE)

cat("Reviewed:", reviewed_file, "\n")
cat("Summary:", summary_file, "\n")
print(summary)
