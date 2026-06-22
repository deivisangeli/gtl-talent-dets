###############################################################################
# Export the US county inventor panel to the shared processed-data folder.
#
# The output mirrors the processed UK LAU panel schema, but uses US counties
# as the fixed geographic unit.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/13_export_us_county_inventor_panel_processed.R
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
dir.create(DATA_PROCESSED, recursive = TRUE, showWarnings = FALSE)

us_county_file <- file.path(DATA_OUTPUT, "us_panel_county_stem_year_1800.csv")
out_file <- file.path(
  DATA_PROCESSED,
  "us_county_inventor_panel_1800_1960_census_population.csv"
)

if (!file.exists(us_county_file)) {
  stop("Missing required file: ", us_county_file)
}

###############################################################################
# Build processed panel
###############################################################################

cat("Reading US annual county inventor panel...\n")
us <- fread(us_county_file, na.strings = c("", "NA"))

required_columns <- c(
  "GEOID", "year", "population", "population_source",
  "n_inventors", "n_stem", "any_stem", "log1p_n_inventors",
  "log1p_n_stem", "inv_per_100k", "stem_per_100k",
  "allsci_per_1000_pop", "stem_per_1000_pop",
  "lon_county", "lat_county"
)
missing_columns <- setdiff(required_columns, names(us))
if (length(missing_columns) > 0L) {
  stop(
    "Missing required columns in ", us_county_file, ":\n",
    paste(missing_columns, collapse = "\n")
  )
}

us[, GEOID := sprintf("%05d", as.integer(gsub("[^0-9]", "", as.character(GEOID))))]
us[, year := as.integer(year)]
us <- us[year >= 1800L & year <= 1960L]

us[, `:=`(
  n_inventors = fifelse(is.na(n_inventors), 0L, as.integer(n_inventors)),
  n_stem = fifelse(is.na(n_stem), 0L, as.integer(n_stem))
)]
us[, n_nonstem := n_inventors - n_stem]

panel <- us[, .(
  unit_type = "us_county",
  unit_id = paste0("US_COUNTY_", GEOID),
  GEOID,
  lau_id = NA_character_,
  city_geonameid = NA_integer_,
  place_name = GEOID,
  place_name_ascii = GEOID,
  country = "United States",
  iso3 = "USA",
  lat = lat_county,
  lon = lon_county,
  year,
  n_inventors,
  n_stem,
  n_nonstem,
  any_inventor = as.integer(n_inventors > 0L),
  any_stem = as.integer(n_stem > 0L),
  log1p_n_inventors,
  log1p_n_stem,
  population,
  population_original = population,
  population_source,
  population_interp_status = fifelse(
    year %% 10L == 0L,
    "decennial_knot_or_source_year",
    "linear_interpolation_between_decades"
  ),
  inventors_per_100k_pop = inv_per_100k,
  stem_per_100k_pop = stem_per_100k,
  inventors_per_1000_pop = allsci_per_1000_pop,
  stem_per_1000_pop,
  match_status = "matched",
  match_method = "us_county_geoid",
  match_distance_km = NA_real_,
  match_needs_review = FALSE,
  source_panel = "us_panel_county_stem_year_1800",
  population_interpolated = year %% 10L != 0L,
  n_source_units = NA_integer_,
  share_area_weighted = NA_real_
)]

setorder(panel, unit_id, year)

###############################################################################
# Validation and write
###############################################################################

expected_years <- 1800L:1960L
expected_rows <- uniqueN(panel$unit_id) * length(expected_years)
duplicate_keys <- panel[, .N, by = .(unit_id, year)][N > 1]
unit_periods <- panel[, .N, by = unit_id]

if (nrow(panel) != expected_rows) {
  stop("Panel is not balanced: expected ", expected_rows, " rows, got ", nrow(panel))
}
if (nrow(duplicate_keys) > 0L) {
  stop("Duplicate unit_id-year keys found: ", nrow(duplicate_keys))
}
if (any(unit_periods$N != length(expected_years))) {
  stop("At least one county does not have a complete 1800-1960 panel.")
}
if (any(panel$n_nonstem != panel$n_inventors - panel$n_stem, na.rm = TRUE)) {
  stop("n_nonstem identity failed.")
}
if (any(is.na(panel$population) | panel$population < 0)) {
  stop("Missing or negative population found.")
}
zero_pop_with_counts <- panel[
  population == 0 & (n_inventors > 0L | n_stem > 0L)
]
if (nrow(zero_pop_with_counts) > 0L) {
  cat(
    "Warning: inventor counts found in county-years with zero population; ",
    "rates remain missing for these rows: ",
    nrow(zero_pop_with_counts), "\n",
    sep = ""
  )
}

rate_tolerance <- 1e-8
rate_check <- panel[population > 0, .(
  max_inv_100k_error = max(abs(inventors_per_100k_pop - 100000 * n_inventors / population), na.rm = TRUE),
  max_stem_100k_error = max(abs(stem_per_100k_pop - 100000 * n_stem / population), na.rm = TRUE),
  max_inv_1000_error = max(abs(inventors_per_1000_pop - 1000 * n_inventors / population), na.rm = TRUE),
  max_stem_1000_error = max(abs(stem_per_1000_pop - 1000 * n_stem / population), na.rm = TRUE)
)]
if (any(unlist(rate_check) > rate_tolerance)) {
  print(rate_check)
  stop("Population rate validation failed.")
}

fwrite(panel, out_file)

cat("\nCompleted US county processed inventor panel export.\n")
cat("Output: ", out_file, "\n", sep = "")
cat("Rows: ", nrow(panel), "\n", sep = "")
cat("Counties: ", uniqueN(panel$unit_id), "\n", sep = "")
cat("Years: ", min(panel$year), "-", max(panel$year), "\n", sep = "")
cat("Total inventors: ", sum(panel$n_inventors), "\n", sep = "")
cat("Total STEM: ", sum(panel$n_stem), "\n", sep = "")
