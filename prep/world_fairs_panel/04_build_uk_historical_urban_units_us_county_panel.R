###############################################################################
# Build a combined UK historical urban-unit + US county inventor panel.
#
# Inputs:
#   Data/processed/uk_historical_urban_units_inventor_panel_1801_1960_census_population.csv
#   Data/processed/us_county_inventor_panel_1800_1960_census_population.csv
#
# Output:
#   Data/processed/uk_historical_urban_units_us_county_inventor_panel_1800_1960_nhgis_us.csv
#   Data/processed/uk_historical_urban_units_us_county_inventor_panel_1800_1960_nhgis_us_qc.csv
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/04_build_uk_historical_urban_units_us_county_panel.R
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

uk_file <- file.path(
  DATA_PROCESSED,
  "uk_historical_urban_units_inventor_panel_1801_1960_census_population.csv"
)
us_file <- file.path(
  DATA_PROCESSED,
  "us_county_inventor_panel_1800_1960_census_population.csv"
)
county_population_file <- file.path(DATA_OUTPUT, "county_population.csv")
out_file <- file.path(
  DATA_PROCESSED,
  "uk_historical_urban_units_us_county_inventor_panel_1800_1960_nhgis_us.csv"
)
qc_file <- file.path(
  DATA_PROCESSED,
  "uk_historical_urban_units_us_county_inventor_panel_1800_1960_nhgis_us_qc.csv"
)

required_files <- c(uk_file, us_file, county_population_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

###############################################################################
# Helpers
###############################################################################

add_missing_columns <- function(data, all_cols) {
  missing_cols <- setdiff(all_cols, names(data))
  for (col in missing_cols) {
    data[, (col) := NA]
  }
  setcolorder(data, all_cols)
  data
}

summarise_panel <- function(data, panel_name, nhgis_only_us_filter) {
  data.table(
    panel = panel_name,
    rows = nrow(data),
    units = uniqueN(data$unit_id),
    years_min = min(data$year, na.rm = TRUE),
    years_max = max(data$year, na.rm = TRUE),
    nhgis_only_us_filter = nhgis_only_us_filter,
    missing_population = sum(is.na(data$population)),
    zero_population = sum(data$population == 0, na.rm = TRUE),
    total_inventors = sum(data$n_inventors, na.rm = TRUE),
    total_stem = sum(data$n_stem, na.rm = TRUE)
  )
}

interp_no_extrapolate <- function(year, population, years_out) {
  ok <- !is.na(year) & !is.na(population)
  year <- as.integer(year[ok])
  population <- as.numeric(population[ok])
  if (length(unique(year)) == 0L) {
    return(rep(NA_real_, length(years_out)))
  }
  if (length(unique(year)) == 1L) {
    return(fifelse(years_out == year[[1L]], population[[1L]], NA_real_))
  }
  approx(
    x = year,
    y = population,
    xout = years_out,
    method = "linear",
    rule = 1,
    ties = sum
  )$y
}

replace_us_population_with_nhgis_only <- function(us, county_population_file, years) {
  out <- copy(us)
  out[, GEOID := sprintf(
    "%05d",
    as.integer(gsub("[^0-9]", "", as.character(GEOID)))
  )]

  county_population <- fread(county_population_file, na.strings = c("", "NA"))
  required_pop_cols <- c("GEOID", "decade", "population", "source")
  missing_pop_cols <- setdiff(required_pop_cols, names(county_population))
  if (length(missing_pop_cols) > 0L) {
    stop(
      "Missing required columns in ", county_population_file, ":\n",
      paste(missing_pop_cols, collapse = "\n")
    )
  }

  county_population[, `:=`(
    GEOID = sprintf(
      "%05d",
      as.integer(gsub("[^0-9]", "", as.character(GEOID)))
    ),
    decade = as.integer(decade),
    population = suppressWarnings(as.numeric(population))
  )]
  nhgis_knots <- county_population[
    source == "nhgis" &
      decade %in% years &
      !is.na(population),
    .(GEOID, year = decade, population_nhgis = population)
  ]

  annual_pop <- merge(
    unique(out[, .(unit_id, GEOID, year)]),
    nhgis_knots,
    by = c("GEOID", "year"),
    all.x = TRUE,
    sort = FALSE
  )
  annual_pop[, population_nhgis_interp := interp_no_extrapolate(
    year = year[!is.na(population_nhgis)],
    population = population_nhgis[!is.na(population_nhgis)],
    years_out = year
  ), by = unit_id]
  annual_pop[, `:=`(
    population = population_nhgis_interp,
    population_original = population_nhgis,
    population_source = fifelse(!is.na(population_nhgis_interp), "nhgis", NA_character_),
    population_interp_status = fcase(
      !is.na(population_nhgis), "nhgis_decennial_knot",
      is.na(population_nhgis) & !is.na(population_nhgis_interp),
      "linear_interpolation_between_nhgis_decades",
      default = "missing_no_nhgis_interpolation"
    ),
    population_interpolated = is.na(population_nhgis) & !is.na(population_nhgis_interp)
  )]

  out[
    annual_pop,
    on = .(unit_id, GEOID, year),
    `:=`(
      population = i.population,
      population_original = i.population_original,
      population_source = i.population_source,
      population_interp_status = i.population_interp_status,
      population_interpolated = i.population_interpolated
    )
  ]
  out[, `:=`(
    inventors_per_100k_pop = fifelse(
      !is.na(population) & population > 0,
      1e5 * n_inventors / population,
      NA_real_
    ),
    stem_per_100k_pop = fifelse(
      !is.na(population) & population > 0,
      1e5 * n_stem / population,
      NA_real_
    ),
    inventors_per_1000_pop = fifelse(
      !is.na(population) & population > 0,
      1000 * n_inventors / population,
      NA_real_
    ),
    stem_per_1000_pop = fifelse(
      !is.na(population) & population > 0,
      1000 * n_stem / population,
      NA_real_
    )
  )]

  list(
    panel = out,
    nhgis_knots = nhgis_knots,
    annual_population = annual_pop
  )
}

###############################################################################
# Read and filter
###############################################################################

cat("Reading UK historical urban-unit panel...\n")
uk <- fread(uk_file, na.strings = c("", "NA"))

cat("Reading US county panel...\n")
us <- fread(us_file, na.strings = c("", "NA"))

required_common <- c(
  "unit_type", "unit_id", "year", "n_inventors", "n_stem", "n_nonstem",
  "any_inventor", "any_stem", "population", "population_source",
  "iso3", "country"
)
missing_uk <- setdiff(required_common, names(uk))
missing_us <- setdiff(required_common, names(us))
if (length(missing_uk) > 0L) {
  stop("Missing required UK columns:\n", paste(missing_uk, collapse = "\n"))
}
if (length(missing_us) > 0L) {
  stop("Missing required US columns:\n", paste(missing_us, collapse = "\n"))
}

uk[, year := as.integer(year)]
us[, year := as.integer(year)]

us_years <- 1800L:1960L
us_nhgis_only <- replace_us_population_with_nhgis_only(
  us,
  county_population_file,
  us_years
)
us_filtered <- us_nhgis_only$panel

###############################################################################
# Harmonize and combine
###############################################################################

all_cols <- union(names(uk), names(us_filtered))
uk_harmonized <- copy(uk)
us_harmonized <- copy(us_filtered)

add_missing_columns(uk_harmonized, all_cols)
add_missing_columns(us_harmonized, all_cols)

combined <- rbindlist(
  list(uk_harmonized, us_harmonized),
  use.names = TRUE,
  fill = TRUE
)

setorder(combined, iso3, unit_type, unit_id, year)

###############################################################################
# Validation
###############################################################################

cat("Validating combined panel...\n")

expected_uk_units <- uniqueN(uk$unit_id)
expected_uk_rows <- expected_uk_units * length(1801L:1960L)
expected_us_counties <- 3143L
expected_us_rows <- expected_us_counties * length(us_years)
expected_total_rows <- expected_uk_rows + expected_us_rows
expected_total_units <- expected_uk_units + expected_us_counties

duplicate_keys <- combined[, .N, by = .(unit_id, year)][N > 1L]

if (nrow(uk) != expected_uk_rows) {
  stop("Unexpected UK row count: expected ", expected_uk_rows, ", got ", nrow(uk))
}
if (min(uk$year, na.rm = TRUE) != 1801L || max(uk$year, na.rm = TRUE) != 1960L) {
  stop("UK panel does not span 1801-1960.")
}
if (uniqueN(us_filtered$unit_id) != expected_us_counties) {
  stop(
    "Unexpected US county count: expected ",
    expected_us_counties, ", got ", uniqueN(us_filtered$unit_id)
  )
}
if (nrow(us_filtered) != expected_us_rows) {
  stop("Unexpected US rows: expected ", expected_us_rows, ", got ", nrow(us_filtered))
}
if (us_filtered[!is.na(population_source) & population_source != "nhgis", .N] > 0L) {
  stop("US panel contains non-NHGIS population source rows.")
}
if (nrow(combined) != expected_total_rows) {
  stop("Unexpected combined row count: expected ", expected_total_rows, ", got ", nrow(combined))
}
if (uniqueN(combined$unit_id) != expected_total_units) {
  stop(
    "Unexpected combined unit count: expected ",
    expected_total_units, ", got ", uniqueN(combined$unit_id)
  )
}
if (nrow(duplicate_keys) > 0L) {
  stop("Duplicate unit_id-year keys found: ", nrow(duplicate_keys))
}
if (any(combined$n_nonstem != combined$n_inventors - combined$n_stem, na.rm = TRUE)) {
  stop("n_nonstem identity failed.")
}
if (any(combined$any_inventor != as.integer(combined$n_inventors > 0L), na.rm = TRUE)) {
  stop("any_inventor identity failed.")
}
if (any(combined$any_stem != as.integer(combined$n_stem > 0L), na.rm = TRUE)) {
  stop("any_stem identity failed.")
}
if (any(combined$population < 0, na.rm = TRUE)) {
  stop("Negative population found.")
}

###############################################################################
# QC and write
###############################################################################

qc <- rbindlist(list(
  summarise_panel(uk, "uk_historical_urban_unit", NA),
  summarise_panel(us_filtered, "us_county_nhgis_only_population", FALSE),
  summarise_panel(combined, "combined", FALSE)
), use.names = TRUE)

qc_by_type <- combined[, .(
  rows = .N,
  units = uniqueN(unit_id),
  years_min = min(year, na.rm = TRUE),
  years_max = max(year, na.rm = TRUE),
  missing_population = sum(is.na(population)),
  zero_population = sum(population == 0, na.rm = TRUE),
  total_inventors = sum(n_inventors, na.rm = TRUE),
  total_stem = sum(n_stem, na.rm = TRUE)
), by = .(iso3, unit_type)][order(iso3, unit_type)]

qc <- rbindlist(list(
  qc,
  qc_by_type[, .(
    panel = paste0(iso3, "_", unit_type),
    rows,
    units,
    years_min,
    years_max,
    nhgis_only_us_filter = fifelse(iso3 == "USA", FALSE, NA),
    missing_population,
    zero_population,
    total_inventors,
    total_stem
  )]
), use.names = TRUE)

fwrite(combined, out_file)
fwrite(qc, qc_file)

cat("\nCompleted UK historical urban-unit + US county panel.\n")
cat("Output: ", out_file, "\n", sep = "")
cat("QC: ", qc_file, "\n", sep = "")
cat("Rows: ", nrow(combined), "\n", sep = "")
cat("Units: ", uniqueN(combined$unit_id), "\n", sep = "")
cat("UK units: ", uniqueN(uk$unit_id), "\n", sep = "")
cat("US counties: ", uniqueN(us_filtered$unit_id), "\n", sep = "")
cat("US rows with NHGIS-based population: ", us_filtered[!is.na(population), .N], "\n", sep = "")
cat("US rows missing NHGIS-based population: ", us_filtered[is.na(population), .N], "\n", sep = "")
cat("Years: ", min(combined$year), "-", max(combined$year), "\n", sep = "")
