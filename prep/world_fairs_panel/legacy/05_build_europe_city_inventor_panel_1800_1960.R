###############################################################################
# Build a balanced European city-year inventor/scientist outcome panel.
#
# Inputs:
#   output/discovery_science_city_year_us_europe.csv
#   output/europe_city_population_stadester_1800_1960.csv
#
# Outputs:
#   output/europe_city_inventor_panel_1800_1960.csv
#   output/europe_city_inventor_panel_1800_1960_qc.csv
#
# The panel is balanced over the European city universe in the Stadester
# population panel: one row per city_geonameid x year, 1800-1960.
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
})

initial_time <- Sys.time()

repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = NA), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "paths.R"))

###############################################################################
# Paths and constants
###############################################################################

scientist_file <- file.path(DATA_OUTPUT, "discovery_science_city_year_us_europe.csv")
population_file <- file.path(DATA_OUTPUT, "europe_city_population_stadester_1800_1960.csv")
us_county_file <- file.path(DATA_OUTPUT, "us_panel_county_stem_year_1800.csv")

out_file <- file.path(DATA_OUTPUT, "europe_city_inventor_panel_1800_1960.csv")
out_balanced_rates <- file.path(
  DATA_OUTPUT,
  "europe_city_inventor_panel_1800_1960_balanced_rates.csv"
)
out_us_europe <- file.path(DATA_OUTPUT, "us_europe_inventor_panel_1800_1960.csv")
out_us_europe_balanced_rates <- file.path(
  DATA_OUTPUT,
  "us_europe_inventor_panel_1800_1960_balanced_rates.csv"
)
out_us_europe_qc <- file.path(DATA_OUTPUT, "us_europe_inventor_panel_1800_1960_qc.csv")
out_xlsx <- file.path(DATA_OUTPUT, "europe_city_inventor_panel_1800_1960.xlsx")
out_qc <- file.path(DATA_OUTPUT, "europe_city_inventor_panel_1800_1960_qc.csv")

years_keep <- 1800L:1960L
excel_max_rows <- 1048576L

stopifnot(file.exists(scientist_file))
stopifnot(file.exists(population_file))
stopifnot(file.exists(us_county_file))

###############################################################################
# Load balanced city-year skeleton with Stadester population
###############################################################################

cat("Reading balanced European city-year population skeleton...\n")
pop <- fread(population_file)
pop[, city_geonameid := as.integer(city_geonameid)]
pop[, year := as.integer(year)]

pop <- pop[year %in% years_keep]

pop_dups <- pop[, .N, by = .(city_geonameid, year)][N > 1]
if (nrow(pop_dups) > 0) {
  stop("Population panel has duplicate city_geonameid-year keys.")
}

expected_rows <- uniqueN(pop$city_geonameid) * length(years_keep)
if (nrow(pop) != expected_rows) {
  stop(
    "Population skeleton is not balanced: got ", nrow(pop),
    " rows, expected ", expected_rows, "."
  )
}

cat("Population skeleton cities:", uniqueN(pop$city_geonameid), "\n")
cat("Population skeleton rows:", nrow(pop), "\n")

###############################################################################
# Load and aggregate Europe Discovery/Science counts
###############################################################################

cat("Reading Europe Discovery/Science city-year counts...\n")
sci <- fread(scientist_file)
sci[, city_geonameid := as.integer(city_geonameid)]
sci[, year := as.integer(year)]

sci_eu <- sci[
  continent == "EU" &
    year %in% years_keep
]

if (nrow(sci_eu) == 0) {
  stop("No European Discovery/Science rows found for 1800-1960.")
}

sci_agg <- sci_eu[, .(
  n_inventors = sum(n_scientists, na.rm = TRUE),
  n_stem = sum(n_stem, na.rm = TRUE),
  n_nonstem = sum(n_nonstem, na.rm = TRUE),
  scientist_mean_distance_to_city_km = ifelse(
    sum(n_scientists, na.rm = TRUE) > 0,
    weighted.mean(mean_distance_to_city_km, n_scientists, na.rm = TRUE),
    NA_real_
  ),
  scientist_max_distance_to_city_km = max(max_distance_to_city_km, na.rm = TRUE),
  n_scientist_geocode_needs_review = sum(n_needs_review, na.rm = TRUE)
), by = .(city_geonameid, year)]

sci_agg[
  !is.finite(scientist_max_distance_to_city_km),
  scientist_max_distance_to_city_km := NA_real_
]

cat("Europe source city-year rows:", nrow(sci_eu), "\n")
cat("Europe source cities with scientists:", uniqueN(sci_eu$city_geonameid), "\n")
cat("Europe source total Discovery/Science:", sum(sci_eu$n_scientists), "\n")
cat("Europe source total STEM:", sum(sci_eu$n_stem), "\n")

missing_source_cities <- setdiff(unique(sci_agg$city_geonameid), unique(pop$city_geonameid))
if (length(missing_source_cities) > 0) {
  stop(
    "Scientist city IDs missing from population skeleton: ",
    paste(head(missing_source_cities, 20), collapse = ", ")
  )
}

###############################################################################
# Merge counts into balanced panel and compute outcomes
###############################################################################

cat("Building balanced inventor outcome panel...\n")
panel <- merge(
  pop,
  sci_agg,
  by = c("city_geonameid", "year"),
  all.x = TRUE,
  sort = FALSE
)

count_cols <- c("n_inventors", "n_stem", "n_nonstem",
                "n_scientist_geocode_needs_review")
for (v in count_cols) {
  panel[is.na(get(v)), (v) := 0]
}

panel[, any_inventor := as.integer(n_inventors > 0)]
panel[, any_stem := as.integer(n_stem > 0)]
panel[, log1p_n_inventors := log1p(n_inventors)]
panel[, log1p_n_stem := log1p(n_stem)]

panel[, inventors_per_100k_pop := fifelse(
  !is.na(city_population_stadester_interp) &
    city_population_stadester_interp > 0,
  1e5 * n_inventors / city_population_stadester_interp,
  NA_real_
)]
panel[, stem_per_100k_pop := fifelse(
  !is.na(city_population_stadester_interp) &
    city_population_stadester_interp > 0,
  1e5 * n_stem / city_population_stadester_interp,
  NA_real_
)]
panel[, inventors_per_1000_pop := fifelse(
  !is.na(city_population_stadester_interp) &
    city_population_stadester_interp > 0,
  1000 * n_inventors / city_population_stadester_interp,
  NA_real_
)]
panel[, stem_per_1000_pop := fifelse(
  !is.na(city_population_stadester_interp) &
    city_population_stadester_interp > 0,
  1000 * n_stem / city_population_stadester_interp,
  NA_real_
)]

out_cols <- c(
  "city_geonameid", "city", "city_ascii", "country", "iso3",
  "lat_city", "lon_city", "year",
  "n_inventors", "n_stem", "n_nonstem", "any_inventor", "any_stem",
  "log1p_n_inventors", "log1p_n_stem",
  "city_population_stadester", "city_population_stadester_interp",
  "city_population_stadester_interp_status",
  "inventors_per_100k_pop", "stem_per_100k_pop",
  "inventors_per_1000_pop", "stem_per_1000_pop",
  "match_status", "match_method", "stadester_source", "stadester_key",
  "stadester_name", "stadester_country", "stadester_lat", "stadester_lon",
  "match_distance_km", "match_needs_review",
  "scientist_mean_distance_to_city_km", "scientist_max_distance_to_city_km",
  "n_scientist_geocode_needs_review"
)
panel <- panel[, ..out_cols]
setorder(panel, iso3, city_ascii, year)

###############################################################################
# QC
###############################################################################

cat("\n=== QC ===\n")
panel_dups <- panel[, .N, by = .(city_geonameid, year)][N > 1]
city_periods <- panel[, .N, by = city_geonameid]

cat("Panel rows:", nrow(panel), "\n")
cat("Panel cities:", uniqueN(panel$city_geonameid), "\n")
cat("Panel years:", min(panel$year), "-", max(panel$year), "\n")
cat("Expected rows:", expected_rows, "\n")
cat("Duplicate city-year keys:", nrow(panel_dups), "\n")
cat("Cities with complete 161-year panel:", sum(city_periods$N == length(years_keep)), "\n")
cat("Cities with incomplete panel:", sum(city_periods$N != length(years_keep)), "\n")
cat("Panel total Discovery/Science:", sum(panel$n_inventors), "\n")
cat("Panel total STEM:", sum(panel$n_stem), "\n")
cat("City-years with positive Discovery/Science:", sum(panel$n_inventors > 0), "\n")
cat("Cities with any Discovery/Science:", uniqueN(panel[n_inventors > 0]$city_geonameid), "\n")
cat(
  "City-years with nonmissing interpolated population:",
  sum(!is.na(panel$city_population_stadester_interp)), "\n"
)
cat(
  "City-years with missing interpolated population:",
  sum(is.na(panel$city_population_stadester_interp)), "\n"
)

stopifnot(nrow(panel) == expected_rows)
stopifnot(nrow(panel_dups) == 0)
stopifnot(all(city_periods$N == length(years_keep)))
stopifnot(sum(panel$n_inventors) == sum(sci_eu$n_scientists))
stopifnot(sum(panel$n_stem) == sum(sci_eu$n_stem))
stopifnot(sum(panel$n_nonstem) == sum(sci_eu$n_nonstem))

###############################################################################
# Balanced sample for the main rate outcome
###############################################################################

rate_complete_cities <- panel[, .(
  n_years = .N,
  n_nonmissing_rate = sum(!is.na(inventors_per_100k_pop)),
  n_nonmissing_population = sum(!is.na(city_population_stadester_interp))
), by = city_geonameid][
  n_years == length(years_keep) &
    n_nonmissing_rate == length(years_keep) &
    n_nonmissing_population == length(years_keep),
  city_geonameid
]

panel_balanced_rates <- panel[city_geonameid %in% rate_complete_cities]
setorder(panel_balanced_rates, iso3, city_ascii, year)

balanced_rate_expected_rows <-
  uniqueN(panel_balanced_rates$city_geonameid) * length(years_keep)
balanced_rate_dups <- panel_balanced_rates[
  , .N, by = .(city_geonameid, year)
][N > 1]
balanced_rate_periods <- panel_balanced_rates[, .N, by = city_geonameid]

cat("\n=== Balanced-rate panel QC ===\n")
cat("Balanced-rate rows:", nrow(panel_balanced_rates), "\n")
cat("Balanced-rate cities:", uniqueN(panel_balanced_rates$city_geonameid), "\n")
cat("Balanced-rate expected rows:", balanced_rate_expected_rows, "\n")
cat("Balanced-rate duplicate city-year keys:", nrow(balanced_rate_dups), "\n")
cat(
  "Balanced-rate missing inventors_per_100k_pop:",
  sum(is.na(panel_balanced_rates$inventors_per_100k_pop)), "\n"
)
cat(
  "Balanced-rate missing interpolated population:",
  sum(is.na(panel_balanced_rates$city_population_stadester_interp)), "\n"
)
cat(
  "Balanced-rate total Discovery/Science:",
  sum(panel_balanced_rates$n_inventors), "\n"
)
cat(
  "Balanced-rate share of full Discovery/Science:",
  round(100 * sum(panel_balanced_rates$n_inventors) / sum(panel$n_inventors), 2),
  "%\n",
  sep = ""
)

stopifnot(nrow(panel_balanced_rates) == balanced_rate_expected_rows)
stopifnot(nrow(balanced_rate_dups) == 0)
stopifnot(all(balanced_rate_periods$N == length(years_keep)))
stopifnot(!any(is.na(panel_balanced_rates$inventors_per_100k_pop)))
stopifnot(!any(is.na(panel_balanced_rates$city_population_stadester_interp)))
stopifnot(identical(sort(unique(panel_balanced_rates$year)), years_keep))

qc_overall <- data.table(
  section = "overall",
  group_key = "overall",
  group_label = "overall",
  cities = uniqueN(panel$city_geonameid),
  rows = nrow(panel),
  city_years_with_inventors = sum(panel$n_inventors > 0),
  total_inventors = sum(panel$n_inventors),
  total_stem = sum(panel$n_stem),
  population_interp_nonmissing_rows =
    sum(!is.na(panel$city_population_stadester_interp)),
  population_interp_missing_rows =
    sum(is.na(panel$city_population_stadester_interp))
)

qc_balanced_rates <- data.table(
  section = "balanced_rates",
  group_key = "inventors_per_100k_pop",
  group_label = "complete_1800_1960",
  cities = uniqueN(panel_balanced_rates$city_geonameid),
  rows = nrow(panel_balanced_rates),
  city_years_with_inventors = sum(panel_balanced_rates$n_inventors > 0),
  total_inventors = sum(panel_balanced_rates$n_inventors),
  total_stem = sum(panel_balanced_rates$n_stem),
  population_interp_nonmissing_rows =
    sum(!is.na(panel_balanced_rates$city_population_stadester_interp)),
  population_interp_missing_rows =
    sum(is.na(panel_balanced_rates$city_population_stadester_interp)),
  retained_inventor_share =
    sum(panel_balanced_rates$n_inventors) / sum(panel$n_inventors)
)

qc_country <- panel[, .(
  cities = uniqueN(city_geonameid),
  rows = .N,
  city_years_with_inventors = sum(n_inventors > 0),
  total_inventors = sum(n_inventors),
  total_stem = sum(n_stem),
  population_interp_nonmissing_rows =
    sum(!is.na(city_population_stadester_interp)),
  population_interp_missing_rows =
    sum(is.na(city_population_stadester_interp))
), by = .(group_key = iso3, group_label = country)]
qc_country[, section := "country"]

qc_match <- panel[, .(
  cities = uniqueN(city_geonameid),
  rows = .N,
  city_years_with_inventors = sum(n_inventors > 0),
  total_inventors = sum(n_inventors),
  total_stem = sum(n_stem),
  population_interp_nonmissing_rows =
    sum(!is.na(city_population_stadester_interp)),
  population_interp_missing_rows =
    sum(is.na(city_population_stadester_interp))
), by = .(group_key = match_method, group_label = match_status)]
qc_match[, section := "match_method"]

qc <- rbindlist(
  list(qc_overall, qc_balanced_rates, qc_country, qc_match),
  use.names = TRUE,
  fill = TRUE
)
setcolorder(qc, c("section", "group_key", "group_label"))
setorder(qc, section, group_key)

cat("\nTop countries by Discovery/Science births:\n")
print(qc_country[order(-total_inventors)][1:min(20, nrow(qc_country))])

cat("\nMatch-method QC:\n")
print(qc_match[order(-cities)])

###############################################################################
# Combined US-county + Europe-city panel
###############################################################################

cat("\nReading US annual county inventor panel...\n")
us <- fread(us_county_file)
us[, GEOID := sprintf("%05s", as.character(GEOID))]
us[, year := as.integer(year)]
us <- us[year %in% years_keep]

us_dups <- us[, .N, by = .(GEOID, year)][N > 1]
us_periods <- us[, .N, by = GEOID]
us_expected_rows <- uniqueN(us$GEOID) * length(years_keep)

cat("\n=== US annual county panel QC ===\n")
cat("US rows:", nrow(us), "\n")
cat("US counties:", uniqueN(us$GEOID), "\n")
cat("US expected rows:", us_expected_rows, "\n")
cat("US duplicate GEOID-year keys:", nrow(us_dups), "\n")
cat("US counties with complete 161-year panel:", sum(us_periods$N == length(years_keep)), "\n")
cat("US missing inv_per_100k:", sum(is.na(us$inv_per_100k)), "\n")

stopifnot(nrow(us) == us_expected_rows)
stopifnot(nrow(us_dups) == 0)
stopifnot(all(us_periods$N == length(years_keep)))

eu_common <- panel[, .(
  unit_type = "europe_city",
  unit_id = paste0("GEONAMES_", city_geonameid),
  GEOID = NA_character_,
  city_geonameid = as.integer(city_geonameid),
  place_name = city,
  place_name_ascii = city_ascii,
  country,
  iso3,
  lat = lat_city,
  lon = lon_city,
  year,
  n_inventors,
  n_stem,
  n_nonstem,
  any_inventor,
  any_stem,
  log1p_n_inventors,
  log1p_n_stem,
  population = city_population_stadester_interp,
  population_original = city_population_stadester,
  population_source = "stadester",
  population_interp_status = city_population_stadester_interp_status,
  inventors_per_100k_pop,
  stem_per_100k_pop,
  inventors_per_1000_pop,
  stem_per_1000_pop,
  match_status,
  match_method,
  match_distance_km,
  match_needs_review,
  source_panel = "europe_city_inventor_panel_1800_1960"
)]

us_common <- us[, .(
  unit_type = "us_county",
  unit_id = paste0("US_COUNTY_", GEOID),
  GEOID,
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
  n_nonstem = n_inventors - n_stem,
  any_inventor = as.integer(n_inventors > 0),
  any_stem = as.integer(n_stem > 0),
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
  match_needs_review = NA,
  source_panel = "us_panel_county_stem_year_1800"
)]

combined <- rbindlist(list(eu_common, us_common), use.names = TRUE, fill = TRUE)
setorder(combined, unit_type, iso3, place_name_ascii, year)

combined_dups <- combined[, .N, by = .(unit_id, year)][N > 1]
combined_periods <- combined[, .N, by = unit_id]
combined_expected_rows <- uniqueN(combined$unit_id) * length(years_keep)

complete_rate_units <- combined[, .(
  n_years = .N,
  n_nonmissing_rate = sum(!is.na(inventors_per_100k_pop)),
  n_nonmissing_population = sum(!is.na(population))
), by = unit_id][
  n_years == length(years_keep) &
    n_nonmissing_rate == length(years_keep) &
    n_nonmissing_population == length(years_keep),
  unit_id
]
combined_balanced_rates <- combined[unit_id %in% complete_rate_units]
setorder(combined_balanced_rates, unit_type, iso3, place_name_ascii, year)

combined_balanced_dups <- combined_balanced_rates[, .N, by = .(unit_id, year)][N > 1]
combined_balanced_periods <- combined_balanced_rates[, .N, by = unit_id]
combined_balanced_expected_rows <-
  uniqueN(combined_balanced_rates$unit_id) * length(years_keep)

cat("\n=== Combined US-Europe panel QC ===\n")
cat("Combined rows:", nrow(combined), "\n")
cat("Combined units:", uniqueN(combined$unit_id), "\n")
cat("Combined expected rows:", combined_expected_rows, "\n")
cat("Combined duplicate unit-year keys:", nrow(combined_dups), "\n")
cat("Combined missing inventors_per_100k_pop:", sum(is.na(combined$inventors_per_100k_pop)), "\n")
cat("Combined total Discovery/Science:", sum(combined$n_inventors), "\n")

cat("\n=== Combined balanced-rate panel QC ===\n")
cat("Combined balanced-rate rows:", nrow(combined_balanced_rates), "\n")
cat("Combined balanced-rate units:", uniqueN(combined_balanced_rates$unit_id), "\n")
cat("Combined balanced-rate expected rows:", combined_balanced_expected_rows, "\n")
cat("Combined balanced-rate duplicate unit-year keys:", nrow(combined_balanced_dups), "\n")
cat(
  "Combined balanced-rate missing inventors_per_100k_pop:",
  sum(is.na(combined_balanced_rates$inventors_per_100k_pop)), "\n"
)
cat(
  "Combined balanced-rate total Discovery/Science:",
  sum(combined_balanced_rates$n_inventors), "\n"
)

stopifnot(nrow(combined) == combined_expected_rows)
stopifnot(nrow(combined_dups) == 0)
stopifnot(all(combined_periods$N == length(years_keep)))
stopifnot(nrow(combined_balanced_rates) == combined_balanced_expected_rows)
stopifnot(nrow(combined_balanced_dups) == 0)
stopifnot(all(combined_balanced_periods$N == length(years_keep)))
stopifnot(!any(is.na(combined_balanced_rates$inventors_per_100k_pop)))
stopifnot(!any(is.na(combined_balanced_rates$population)))

qc_combined_overall <- combined[, .(
  section = "overall",
  group_key = "overall",
  group_label = "overall",
  units = uniqueN(unit_id),
  rows = .N,
  unit_years_with_inventors = sum(n_inventors > 0),
  total_inventors = sum(n_inventors),
  total_stem = sum(n_stem),
  rate_nonmissing_rows = sum(!is.na(inventors_per_100k_pop)),
  rate_missing_rows = sum(is.na(inventors_per_100k_pop))
)]

qc_combined_balanced <- combined_balanced_rates[, .(
  section = "balanced_rates",
  group_key = "inventors_per_100k_pop",
  group_label = "complete_1800_1960",
  units = uniqueN(unit_id),
  rows = .N,
  unit_years_with_inventors = sum(n_inventors > 0),
  total_inventors = sum(n_inventors),
  total_stem = sum(n_stem),
  rate_nonmissing_rows = sum(!is.na(inventors_per_100k_pop)),
  rate_missing_rows = sum(is.na(inventors_per_100k_pop))
)]

qc_combined_type <- combined[, .(
  units = uniqueN(unit_id),
  rows = .N,
  unit_years_with_inventors = sum(n_inventors > 0),
  total_inventors = sum(n_inventors),
  total_stem = sum(n_stem),
  rate_nonmissing_rows = sum(!is.na(inventors_per_100k_pop)),
  rate_missing_rows = sum(is.na(inventors_per_100k_pop))
), by = .(unit_type)]
qc_combined_type[, `:=`(
  section = "unit_type",
  group_key = unit_type,
  group_label = unit_type
)]
qc_combined_type[, unit_type := NULL]

qc_combined_type_balanced <- combined_balanced_rates[, .(
  units = uniqueN(unit_id),
  rows = .N,
  unit_years_with_inventors = sum(n_inventors > 0),
  total_inventors = sum(n_inventors),
  total_stem = sum(n_stem),
  rate_nonmissing_rows = sum(!is.na(inventors_per_100k_pop)),
  rate_missing_rows = sum(is.na(inventors_per_100k_pop))
), by = .(unit_type)]
qc_combined_type_balanced[, `:=`(
  section = "balanced_rates_by_unit_type",
  group_key = unit_type,
  group_label = unit_type
)]
qc_combined_type_balanced[, unit_type := NULL]

qc_combined <- rbindlist(
  list(
    qc_combined_overall,
    qc_combined_balanced,
    qc_combined_type,
    qc_combined_type_balanced
  ),
  use.names = TRUE,
  fill = TRUE
)
setorder(qc_combined, section, group_key)

cat("\nCombined balanced-rate distribution by unit type:\n")
print(qc_combined_type_balanced[order(group_key)])

###############################################################################
# Export
###############################################################################

fwrite(panel, out_file)
fwrite(panel_balanced_rates, out_balanced_rates)
fwrite(qc, out_qc)
fwrite(combined, out_us_europe)
fwrite(combined_balanced_rates, out_us_europe_balanced_rates)
fwrite(qc_combined, out_us_europe_qc)

if (nrow(panel) <= excel_max_rows) {
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(as.data.frame(panel), out_xlsx)
    cat("Wrote Excel file:", out_xlsx, "\n")
  } else {
    cat("Skipped Excel file: writexl is not installed.\n")
  }
} else {
  cat(
    "Skipped Excel file: panel has ", nrow(panel),
    " rows, above Excel's ", excel_max_rows, " row limit.\n",
    sep = ""
  )
}

cat("\nWrote:\n")
cat("  ", out_file, "\n", sep = "")
cat("  ", out_balanced_rates, "\n", sep = "")
cat("  ", out_qc, "\n", sep = "")
cat("  ", out_us_europe, "\n", sep = "")
cat("  ", out_us_europe_balanced_rates, "\n", sep = "")
cat("  ", out_us_europe_qc, "\n", sep = "")

final_time <- Sys.time() - initial_time
cat("\nRan in", round(as.numeric(final_time, units = "mins"), 2), "minutes.\n")
