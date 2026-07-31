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

uk_filename <-
  "uk_historical_urban_units_inventor_panel_1801_1960_census_population.csv"
us_filename <- "us_county_inventor_panel_1800_1960_census_population.csv"
uk_file_candidates <- c(
  file.path(DATA_PROCESSED, uk_filename),
  file.path(DATA_PROCESSED, "worlds_fairs", uk_filename)
)
us_file_candidates <- c(
  file.path(DATA_PROCESSED, us_filename),
  file.path(DATA_PROCESSED, "worlds_fairs", us_filename)
)
uk_file <- uk_file_candidates[file.exists(uk_file_candidates)][1L]
us_file <- us_file_candidates[file.exists(us_file_candidates)][1L]
if (is.na(uk_file)) uk_file <- uk_file_candidates[[1L]]
if (is.na(us_file)) us_file <- us_file_candidates[[1L]]
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

occupation_panel_cols <- c(
  "population_implied_1801",
  "agri_share_1801", "trade_share_1801", "other_share_1801",
  "occupation_share_coverage_1801", "population_density_1801",
  "population_density_area_coverage_1801"
)
swing_population_panel_cols <- c(
  "population_knot", "population_knot_available",
  "population_swing_implied", "population_swing_used",
  "population_swing_geometry_coverage", "population_swing_density_coverage",
  "population_swing_growth_outlier", "population_swing_exclusion_reason"
)

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
  has_occupation_shares <- all(occupation_panel_cols %chin% names(data))
  occupation_rows <- if (has_occupation_shares) {
    sum(!is.na(data$agri_share_1801))
  } else {
    0L
  }
  occupation_units <- if (has_occupation_shares) {
    uniqueN(data$unit_id[!is.na(data$agri_share_1801)])
  } else {
    0L
  }
  mean_occupation_coverage <- if (
    has_occupation_shares &&
      any(!is.na(data$occupation_share_coverage_1801))
  ) {
    mean(data$occupation_share_coverage_1801, na.rm = TRUE)
  } else {
    NA_real_
  }
  density_rows <- if (has_occupation_shares) {
    sum(!is.na(data$population_density_1801))
  } else {
    0L
  }
  density_units <- if (has_occupation_shares) {
    uniqueN(data$unit_id[!is.na(data$population_density_1801)])
  } else {
    0L
  }
  mean_density_area_coverage <- if (
    has_occupation_shares &&
      any(!is.na(data$population_density_area_coverage_1801))
  ) {
    mean(data$population_density_area_coverage_1801, na.rm = TRUE)
  } else {
    NA_real_
  }
  swing_rows <- if ("population_swing_used" %chin% names(data)) {
    sum(data$population_swing_used == TRUE, na.rm = TRUE)
  } else {
    0L
  }
  swing_units <- if ("population_swing_used" %chin% names(data)) {
    uniqueN(data$unit_id[data$population_swing_used == TRUE])
  } else {
    0L
  }

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
    total_stem = sum(data$n_stem, na.rm = TRUE),
    occupation_share_rows_1801 = occupation_rows,
    occupation_share_units_1801 = occupation_units,
    mean_occupation_share_coverage_1801 = mean_occupation_coverage,
    population_density_rows_1801 = density_rows,
    population_density_units_1801 = density_units,
    mean_population_density_area_coverage_1801 = mean_density_area_coverage,
    swing_population_knots_used = swing_rows,
    units_using_swing_population = swing_units
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
missing_occupation_uk <- setdiff(occupation_panel_cols, names(uk))
if (length(missing_occupation_uk) > 0L) {
  stop(
    "UK panel is missing 1801 occupation-share columns:\n",
    paste(missing_occupation_uk, collapse = "\n")
  )
}
missing_swing_population_uk <- setdiff(swing_population_panel_cols, names(uk))
if (length(missing_swing_population_uk) > 0L) {
  stop(
    "UK panel is missing Swing population columns:\n",
    paste(missing_swing_population_uk, collapse = "\n")
  )
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

uk_share_missing_count <- uk[, rowSums(is.na(.SD)),
                             .SDcols = c(
                               "agri_share_1801", "trade_share_1801",
                               "other_share_1801"
                             )]
if (any(!uk_share_missing_count %in% c(0L, 3L))) {
  stop("UK occupation shares must be jointly observed or jointly missing.")
}
if (uk[
      !is.na(agri_share_1801),
      any(
        agri_share_1801 < 0 | agri_share_1801 > 1 |
          trade_share_1801 < 0 | trade_share_1801 > 1 |
          other_share_1801 < 0 | other_share_1801 > 1 |
          abs(agri_share_1801 + trade_share_1801 + other_share_1801 - 1) > 1e-6
      )
    ]) {
  stop("Invalid UK 1801 occupation shares in the combined-panel input.")
}
if (uk[
      !is.na(occupation_share_coverage_1801),
      any(occupation_share_coverage_1801 < 0 |
          occupation_share_coverage_1801 > 1 + 1e-6)
    ]) {
  stop("UK occupation-share coverage must lie in [0, 1].")
}
if (uk[
      !is.na(population_implied_1801),
      any(!is.finite(population_implied_1801) | population_implied_1801 <= 0)
    ]) {
  stop("UK implied 1801 population must be finite and positive.")
}
if (uk[
      !is.na(population_density_1801),
      any(!is.finite(population_density_1801) | population_density_1801 < 0)
    ]) {
  stop("UK 1801 population density must be finite and non-negative.")
}
if (uk[
      !is.na(population_density_area_coverage_1801),
      any(population_density_area_coverage_1801 < 0 |
          population_density_area_coverage_1801 > 1 + 1e-6)
    ]) {
  stop("UK population-density area coverage must lie in [0, 1].")
}
occupation_static_check <- uk[, lapply(.SD, uniqueN),
                              by = unit_id,
                              .SDcols = occupation_panel_cols]
if (occupation_static_check[
      , any(unlist(.SD, use.names = FALSE) != 1L),
      .SDcols = occupation_panel_cols
    ]) {
  stop("UK 1801 demographic columns are not time invariant by unit.")
}
if (combined[
      iso3 == "USA",
      any(!is.na(population_implied_1801) |
          !is.na(agri_share_1801) |
          !is.na(trade_share_1801) |
          !is.na(other_share_1801) |
          !is.na(occupation_share_coverage_1801) |
          !is.na(population_density_1801) |
          !is.na(population_density_area_coverage_1801))
    ]) {
  stop("US rows must have missing UK-only 1801 demographic variables.")
}
if (combined[
      iso3 == "USA",
      any(!is.na(population_swing_implied) |
          !is.na(population_swing_geometry_coverage) |
          !is.na(population_swing_density_coverage) |
          !is.na(population_swing_growth_outlier) |
          !is.na(population_swing_exclusion_reason))
    ]) {
  stop("US rows must have missing UK-only Swing population fields.")
}
if (uk[
      population_swing_used == TRUE,
      any(is.na(population_swing_implied) |
          population_swing_growth_outlier == TRUE |
          population_swing_geometry_coverage < 0.95 |
          population_swing_density_coverage < 0.95)
    ]) {
  stop("UK panel contains an invalid used Swing population knot.")
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
  total_stem = sum(n_stem, na.rm = TRUE),
  occupation_share_rows_1801 = sum(!is.na(agri_share_1801)),
  occupation_share_units_1801 = uniqueN(unit_id[!is.na(agri_share_1801)]),
  mean_occupation_share_coverage_1801 = if (
    any(!is.na(occupation_share_coverage_1801))
  ) {
    mean(occupation_share_coverage_1801, na.rm = TRUE)
  } else {
    NA_real_
  },
  population_density_rows_1801 = sum(!is.na(population_density_1801)),
  population_density_units_1801 = uniqueN(
    unit_id[!is.na(population_density_1801)]
  ),
  mean_population_density_area_coverage_1801 = if (
    any(!is.na(population_density_area_coverage_1801))
  ) {
    mean(population_density_area_coverage_1801, na.rm = TRUE)
  } else {
    NA_real_
  },
  swing_population_knots_used = sum(
    population_swing_used == TRUE, na.rm = TRUE
  ),
  units_using_swing_population = uniqueN(
    unit_id[population_swing_used == TRUE]
  )
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
    total_stem,
    occupation_share_rows_1801,
    occupation_share_units_1801,
    mean_occupation_share_coverage_1801,
    population_density_rows_1801,
    population_density_units_1801,
    mean_population_density_area_coverage_1801,
    swing_population_knots_used,
    units_using_swing_population
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
