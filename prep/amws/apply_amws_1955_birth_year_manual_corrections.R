###############################################################################
# Apply explicit AMWS 1955 birth-year manual corrections.
#
# Reads:
#   output/amws/amws_1955_split.csv
#
# Writes:
#   output/amws/amws_1955_split_corrected.csv
#   output/amws/amws_1955_birth_year_manual_corrections_applied_log.csv
#   output/amws/amws_1955_birth_year_manual_corrections_summary.csv
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
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
  AMWS_OUTPUT <- file.path(DATA_OUTPUT, "amws")
}

input_file <- file.path(AMWS_OUTPUT, "amws_1955_split.csv")
output_file <- file.path(AMWS_OUTPUT, "amws_1955_split_corrected.csv")
log_file <- file.path(
  AMWS_OUTPUT,
  "amws_1955_birth_year_manual_corrections_applied_log.csv"
)
summary_file <- file.path(
  AMWS_OUTPUT,
  "amws_1955_birth_year_manual_corrections_summary.csv"
)

if (!file.exists(input_file)) stop("Missing input file: ", input_file)

corrections <- data.table(
  lineid = 12343L,
  date_raw_new = "Jan. 25, 25",
  birth_year_new = 1925L,
  correction_note = paste(
    "External sources identify this as Edward E. David Jr., born",
    "Wilmington, N.C., Jan. 25, 1925; AMWS/OCR row read Jan. 23, 35."
  )
)

input <- fread(input_file)
required_cols <- c("lineid", "last", "first", "birthplace_orig",
                   "date_raw", "birth_year")
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input missing required columns: ", paste(missing_cols, collapse = ", "))
}
if (uniqueN(input$lineid) != nrow(input)) {
  stop("Input has duplicated lineid.")
}

to_apply <- merge(
  corrections,
  input[, .(
    lineid, last, first, birthplace_orig,
    date_raw_old = date_raw,
    birth_year_old = birth_year
  )],
  by = "lineid",
  all.x = TRUE
)
if (any(is.na(to_apply$birth_year_old))) {
  stop("At least one correction lineid was not found in input.")
}

bad_year <- corrections[
  is.na(birth_year_new) |
    birth_year_new < 1800L |
    birth_year_new > 1955L
]
if (nrow(bad_year)) stop("Invalid birth_year_new in corrections.")

corrected <- copy(input)
idx <- match(corrections$lineid, corrected$lineid)
corrected$date_raw[idx] <- corrections$date_raw_new
corrected$birth_year[idx] <- corrections$birth_year_new

applied_log <- to_apply[, .(
  edition = 1955L,
  lineid,
  last,
  first,
  birthplace_orig,
  date_raw_old,
  birth_year_old,
  date_raw_new,
  birth_year_new,
  changed_date_raw = trimws(date_raw_old) != trimws(date_raw_new),
  changed_birth_year = birth_year_old != birth_year_new,
  correction_note
)]

birth_year_int <- suppressWarnings(as.integer(corrected$birth_year))
summary <- data.table(
  metric = c(
    "input_rows",
    "manual_correction_rows",
    "changed_date_raw",
    "changed_birth_year",
    "birth_year_after_edition_remaining",
    "min_birth_year",
    "max_birth_year"
  ),
  value = c(
    nrow(corrected),
    nrow(corrections),
    applied_log[changed_date_raw == TRUE, .N],
    applied_log[changed_birth_year == TRUE, .N],
    sum(!is.na(birth_year_int) & birth_year_int > 1955L),
    min(birth_year_int, na.rm = TRUE),
    max(birth_year_int, na.rm = TRUE)
  )
)

fwrite(corrected, output_file)
fwrite(applied_log, log_file)
fwrite(summary, summary_file)

cat("Applied AMWS 1955 birth-year manual corrections:",
    nrow(corrections), "\n")
cat("Changed birth_year:", applied_log[changed_birth_year == TRUE, .N], "\n")
cat("Updated:", output_file, "\n")
cat("Wrote log:", log_file, "\n")
cat("Wrote summary:", summary_file, "\n")
