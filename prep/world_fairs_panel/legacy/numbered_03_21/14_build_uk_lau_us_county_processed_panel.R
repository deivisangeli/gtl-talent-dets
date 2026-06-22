###############################################################################
# Build a processed UK LAU + US county inventor panel.
#
# The US side is restricted to counties whose population source is NHGIS for
# every year from 1800 through 1960. The UK side is kept as-is from the processed
# UK LAU panel.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/14_build_uk_lau_us_county_processed_panel.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
})

###############################################################################
# Paths
###############################################################################

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
}

source(file.path(repo_root, "paths.R"))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(
      user_data_dir, winslash = "/", mustWork = TRUE
    )
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

DATA_PROCESSED <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed")

uk_file <- file.path(
  DATA_PROCESSED,
  "uk_lau_inventor_panel_1801_1960_census_population.csv"
)
us_file <- file.path(
  DATA_PROCESSED,
  "us_county_inventor_panel_1800_1960_census_population.csv"
)
out_file <- file.path(
  DATA_PROCESSED,
  "uk_lau_us_county_inventor_panel_1800_1960_nhgis_us.csv"
)
qc_file <- file.path(
  DATA_PROCESSED,
  "uk_lau_us_county_inventor_panel_1800_1960_nhgis_us_qc.csv"
)

required_files <- c(uk_file, us_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

###############################################################################
# Read and filter
###############################################################################

cat("Reading processed UK LAU panel...\n")
uk <- fread(uk_file, na.strings = c("", "NA"))

cat("Reading processed US county panel...\n")
us <- fread(us_file, na.strings = c("", "NA"))

if (!identical(names(uk), names(us))) {
  stop("UK and US processed panels do not have identical schemas.")
}

us_years <- 1800L:1960L
eligible_us_counties <- us[, .(
  n_years = .N,
  first_year = min(year, na.rm = TRUE),
  last_year = max(year, na.rm = TRUE),
  all_nhgis = all(population_source == "nhgis"),
  n_nhgis = sum(population_source == "nhgis", na.rm = TRUE),
  n_hyde = sum(population_source == "hyde", na.rm = TRUE),
  n_manual = sum(population_source == "manual", na.rm = TRUE)
), by = .(unit_id, GEOID)][
  first_year == min(us_years) &
    last_year == max(us_years) &
    n_years == length(us_years) &
    all_nhgis == TRUE
]

us_filtered <- us[unit_id %in% eligible_us_counties$unit_id]

combined <- rbindlist(list(uk, us_filtered), use.names = TRUE)
setorder(combined, unit_type, iso3, unit_id, year)

###############################################################################
# Validation and write
###############################################################################

expected_uk_rows <- nrow(uk)
expected_us_counties <- 376L
expected_us_rows <- expected_us_counties * length(us_years)
expected_total_rows <- expected_uk_rows + expected_us_rows

duplicate_keys <- combined[, .N, by = .(unit_id, year)][N > 1]

if (nrow(eligible_us_counties) != expected_us_counties) {
  stop(
    "Unexpected number of eligible US NHGIS-only counties: expected ",
    expected_us_counties, ", got ", nrow(eligible_us_counties)
  )
}
if (nrow(us_filtered) != expected_us_rows) {
  stop(
    "Unexpected number of filtered US rows: expected ",
    expected_us_rows, ", got ", nrow(us_filtered)
  )
}
if (nrow(combined) != expected_total_rows) {
  stop(
    "Unexpected combined row count: expected ",
    expected_total_rows, ", got ", nrow(combined)
  )
}
if (nrow(duplicate_keys) > 0L) {
  stop("Duplicate unit_id-year keys found: ", nrow(duplicate_keys))
}
if (us_filtered[population_source != "nhgis", .N] > 0L) {
  stop("Filtered US panel contains non-NHGIS population rows.")
}
if (uk[, uniqueN(unit_id)] != 348L || min(uk$year) != 1801L || max(uk$year) != 1960L) {
  stop("UK panel does not match expected 348 LAUs over 1801-1960.")
}

qc <- rbindlist(list(
  data.table(
    panel = "uk_lau",
    rows = nrow(uk),
    units = uniqueN(uk$unit_id),
    years_min = min(uk$year),
    years_max = max(uk$year),
    nhgis_only_us_filter = NA,
    missing_population = sum(is.na(uk$population)),
    zero_population = sum(uk$population == 0, na.rm = TRUE),
    total_inventors = sum(uk$n_inventors, na.rm = TRUE),
    total_stem = sum(uk$n_stem, na.rm = TRUE)
  ),
  data.table(
    panel = "us_county_nhgis_only",
    rows = nrow(us_filtered),
    units = uniqueN(us_filtered$unit_id),
    years_min = min(us_filtered$year),
    years_max = max(us_filtered$year),
    nhgis_only_us_filter = TRUE,
    missing_population = sum(is.na(us_filtered$population)),
    zero_population = sum(us_filtered$population == 0, na.rm = TRUE),
    total_inventors = sum(us_filtered$n_inventors, na.rm = TRUE),
    total_stem = sum(us_filtered$n_stem, na.rm = TRUE)
  ),
  data.table(
    panel = "combined",
    rows = nrow(combined),
    units = uniqueN(combined$unit_id),
    years_min = min(combined$year),
    years_max = max(combined$year),
    nhgis_only_us_filter = TRUE,
    missing_population = sum(is.na(combined$population)),
    zero_population = sum(combined$population == 0, na.rm = TRUE),
    total_inventors = sum(combined$n_inventors, na.rm = TRUE),
    total_stem = sum(combined$n_stem, na.rm = TRUE)
  )
), use.names = TRUE)

fwrite(combined, out_file)
fwrite(qc, qc_file)

cat("\nCompleted processed UK LAU + US county panel.\n")
cat("Output: ", out_file, "\n", sep = "")
cat("QC: ", qc_file, "\n", sep = "")
cat("Rows: ", nrow(combined), "\n", sep = "")
cat("Units: ", uniqueN(combined$unit_id), "\n", sep = "")
cat("Eligible US NHGIS-only counties: ", nrow(eligible_us_counties), "\n", sep = "")
