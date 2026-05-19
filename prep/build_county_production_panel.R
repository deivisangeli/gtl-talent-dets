###############################################################################
# Project: GTL Talent Determinants
# Goal: Build unified county-level manufacturing and farming production panel
###############################################################################

rm(list = ls())

library("tidyverse")
library("arrow")

source("raw_paths.R")

###############################################################################
# Paths
###############################################################################

manufacturing_parquet_dir <- raw_dir("nhgis_manufacturing", "parquet")
farming_parquet_dir <- raw_dir("nhgis_farming", "parquet")

###############################################################################
# Helpers
###############################################################################

find_latest_parquet <- function(directory, pattern) {
 files <- list.files(directory, pattern = pattern, full.names = TRUE)
 files <- files[!str_detect(basename(files), "_with_total")]

 if (length(files) == 0) {
  stop("No parquet file matching pattern '", pattern, "' in ", directory)
 }

 files[which.max(file.info(files)$mtime)]
}

first_non_missing <- function(x) {
 x <- x[!is.na(x)]
 if (length(x) == 0) NA else x[1]
}

as_numeric_col <- function(x) {
 suppressWarnings(as.numeric(x))
}

interpolate_cpi <- function(years, cpi_table) {
 stats::approx(
  x = cpi_table$year,
  y = cpi_table$cpi_1967_100,
  xout = years,
  rule = 2,
  ties = "ordered"
 )$y
}

flatten_value_vars <- function(value_vars) {
 unlist(value_vars, use.names = FALSE)
}

compute_value <- function(data, value_vars) {
 value_vars <- flatten_value_vars(value_vars)

 missing_vars <- setdiff(value_vars, names(data))
 if (length(missing_vars) > 0) {
  stop("Missing expected production variable(s): ", paste(missing_vars, collapse = ", "))
 }

 if (length(value_vars) == 1) {
  as_numeric_col(data[[value_vars]])
 } else {
  rowSums(as.data.frame(map(data[value_vars], as_numeric_col)), na.rm = TRUE)
 }
}

extract_domain_panel <- function(spec, crosswalk) {
 parquet_file <- find_latest_parquet(spec$parquet_dir, spec$file_pattern)
 data <- read_parquet(parquet_file)

 production_value <- compute_value(data, spec$value_vars[[1]])

 data %>%
  mutate(
   year = spec$year,
   production_type = spec$production_type,
   production_value = production_value,
   source_dataset = spec$source_dataset,
   source_variable = paste(flatten_value_vars(spec$value_vars[[1]]), collapse = " + "),
   production_label = spec$production_label,
   production_construction = spec$production_construction,
   source_file = parquet_file
  ) %>%
  select(
   GISJOIN,
   state_nhgis = STATE,
   county_nhgis = COUNTY,
   statea_nhgis = STATEA,
   countya_nhgis = COUNTYA,
   year,
   production_type,
   production_value,
   source_dataset,
   source_variable,
   production_label,
   production_construction,
   source_file
  ) %>%
  left_join(crosswalk, by = "GISJOIN")
}

###############################################################################
# GISJOIN -> current/nominal FIPS GEOID crosswalk
###############################################################################

# This is the same nominal county file used in prep/cleaning_county.R. Rows with
# missing COUNTYFP are historical counties that do not map cleanly to a current
# 5-digit county GEOID and are audited separately.
county_crosswalk <- read_csv(
 manual_input_path("nhgis0001_ts_nominal_county.csv"),
 show_col_types = FALSE
) %>%
 transmute(
  GISJOIN,
  statefp = STATEFP,
  countyfp = COUNTYFP,
  GEOID = if_else(!is.na(STATEFP) & !is.na(COUNTYFP),
                  paste0(STATEFP, COUNTYFP),
                  NA_character_),
  state_crosswalk = STATE,
  county_crosswalk = COUNTY
 ) %>%
 distinct(GISJOIN, .keep_all = TRUE)

###############################################################################
# Production variable map
###############################################################################

manufacturing_1840_vars <- c(
 paste0("ADA", str_pad(7:36, width = 3, pad = "0")),
 "ADA040",
 "ADA041"
)

production_specs <- tribble(
 ~production_type, ~year, ~source_dataset, ~parquet_dir, ~file_pattern, ~value_vars, ~production_label, ~production_construction,
 "manufacturing", 1840L, "1840_cMfg", manufacturing_parquet_dir, "ds8_1840_county[.]parquet$", list(manufacturing_1840_vars), "Value of manufacturing products", "Sum of 1840_cMfg NT5 variables explicitly labeled Manufacturing Products; excludes market gardens, fisheries, forests, and mining products.",
 "manufacturing", 1870L, "1870_cMfg", manufacturing_parquet_dir, "ds20_1870_county[.]parquet$", list("ANW001"), "Value of manufacturing output", "Direct NHGIS total.",
 "manufacturing", 1890L, "1890_cPHAM", manufacturing_parquet_dir, "ds27_1890_county[.]parquet$", list("AVJ001"), "Value of products, including custom work and repairing", "Direct NHGIS total.",
 "manufacturing", 1900L, "1900_cPHAM", manufacturing_parquet_dir, "ds31_1900_county[.]parquet$", list("AZI001"), "Value of all products of manufacturing establishments", "Direct NHGIS total.",
 "manufacturing", 1920L, "1920_cPHAM", manufacturing_parquet_dir, "ds43_1920_county[.]parquet$", list("A8A001"), "Value of products of manufacturing establishments", "Direct NHGIS total.",
 "farming", 1840L, "1840_cAg", farming_parquet_dir, "ds6_1840_county[.]parquet$", list("ACB001"), "Estimated value of agricultural output", "Direct NHGIS total.",
 "farming", 1870L, "1870_cAg", farming_parquet_dir, "ds16_1870_county[.]parquet$", list("AJX001"), "Estimated value of all farm productions", "Direct NHGIS total.",
 "farming", 1880L, "1880_cAg", farming_parquet_dir, "ds22_1880_county[.]parquet$", list("AOF001"), "Estimated value of all farm products", "Direct NHGIS total.",
 "farming", 1890L, "1890_cAg", farming_parquet_dir, "ds26_1890_county[.]parquet$", list("AUL001"), "Estimated value of farm products", "Direct NHGIS total.",
 "farming", 1900L, "1900_cAg", farming_parquet_dir, "ds30_1900_county[.]parquet$", list("AWX001"), "Value of farm products not fed to livestock", "Direct NHGIS total; concept is narrower than some earlier all-products totals.",
 "farming", 1920L, "1920_cAg", farming_parquet_dir, "ds210_1920_county[.]parquet$", list(c("AB4R001", paste0("AB4E", str_pad(1:5, width = 3, pad = "0")))), "Total value of all crops plus livestock products", "Constructed as total crop value plus value of dairy, poultry/eggs, honey/wax, wool, and mohair products."
)

###############################################################################
# CPI deflator
###############################################################################

# Minneapolis Fed historical CPI series, 1967 = 100. Values before 1913 are
# historical estimates compiled from earlier BLS/Historical Statistics series.
# Real values are expressed in 1900 dollars.
cpi_deflator <- tribble(
 ~year, ~cpi_1967_100,
 1800L, 51.0,
 1810L, 47.0,
 1820L, 42.0,
 1830L, 32.0,
 1840L, 30.0,
 1850L, 25.0,
 1860L, 27.0,
 1870L, 38.0,
 1880L, 29.0,
 1890L, 27.0,
 1900L, 25.0,
 1910L, 28.0,
 1920L, 60.2
) %>%
 mutate(
  cpi_base_1900 = cpi_1967_100[year == 1900L],
  real_1900_factor = cpi_base_1900 / cpi_1967_100,
  cpi_source = "Federal Reserve Bank of Minneapolis historical CPI, 1967=100"
 )

###############################################################################
# Build long and wide panels
###############################################################################

production_long_all <- map_dfr(
 seq_len(nrow(production_specs)),
 \(i) extract_domain_panel(production_specs[i, ], county_crosswalk)
)

production_unmatched <- production_long_all %>%
 filter(is.na(GEOID)) %>%
 arrange(production_type, year, state_nhgis, county_nhgis)

production_long <- production_long_all %>%
 filter(!is.na(GEOID)) %>%
 mutate(
  cpi_1967_100 = interpolate_cpi(year, cpi_deflator),
  cpi_base_1900 = cpi_deflator$cpi_1967_100[cpi_deflator$year == 1900L],
  real_1900_factor = cpi_base_1900 / cpi_1967_100,
  production_value_real_1900 = production_value * real_1900_factor,
  cpi_source = "Federal Reserve Bank of Minneapolis historical CPI, 1967=100"
 ) %>%
 arrange(GEOID, year, production_type)

production_wide <- production_long %>%
 select(
  GEOID,
  year,
  state_nhgis,
  county_nhgis,
  production_type,
  production_value,
  production_value_real_1900,
  cpi_1967_100,
  cpi_base_1900,
  real_1900_factor,
  source_dataset,
  source_variable,
  production_label,
  production_construction
 ) %>%
 pivot_wider(
  names_from = production_type,
  values_from = c(
   production_value,
   production_value_real_1900,
   source_dataset,
   source_variable,
   production_label,
   production_construction
  ),
  names_glue = "{production_type}_{.value}"
 ) %>%
 group_by(GEOID, year) %>%
 summarise(
  state_nhgis = first_non_missing(state_nhgis),
  county_nhgis = first_non_missing(county_nhgis),
  manufacturing_output_value = first_non_missing(manufacturing_production_value),
  farming_output_value = first_non_missing(farming_production_value),
  manufacturing_output_value_real_1900 = first_non_missing(manufacturing_production_value_real_1900),
  farming_output_value_real_1900 = first_non_missing(farming_production_value_real_1900),
  cpi_1967_100 = first_non_missing(cpi_1967_100),
  cpi_base_1900 = first_non_missing(cpi_base_1900),
  real_1900_factor = first_non_missing(real_1900_factor),
  manufacturing_source_dataset = first_non_missing(manufacturing_source_dataset),
  farming_source_dataset = first_non_missing(farming_source_dataset),
  manufacturing_source_variable = first_non_missing(manufacturing_source_variable),
  farming_source_variable = first_non_missing(farming_source_variable),
  manufacturing_production_label = first_non_missing(manufacturing_production_label),
  farming_production_label = first_non_missing(farming_production_label),
  manufacturing_production_construction = first_non_missing(manufacturing_production_construction),
  farming_production_construction = first_non_missing(farming_production_construction),
  .groups = "drop"
 ) %>%
 mutate(
  has_manufacturing_output = !is.na(manufacturing_output_value),
  has_farming_output = !is.na(farming_output_value)
 ) %>%
 arrange(GEOID, year)

###############################################################################
# Export
###############################################################################

write_csv(production_wide, output_file_path("county_production_panel.csv"))
write_parquet(production_wide, output_file_path("county_production_panel.parquet"))

write_csv(production_long, output_file_path("county_production_panel_long.csv"))
write_parquet(production_long, output_file_path("county_production_panel_long.parquet"))

write_csv(production_unmatched, output_file_path("county_production_unmatched_historical_counties.csv"))

production_variable_map <- production_specs %>%
 transmute(
  production_type,
  year,
  source_dataset,
  source_variable = map_chr(value_vars, \(x) paste(flatten_value_vars(x), collapse = " + ")),
  production_label,
  production_construction
 )

write_csv(production_variable_map, output_file_path("county_production_variable_map.csv"))
write_csv(cpi_deflator, output_file_path("county_production_cpi_deflator.csv"))

message("Saved wide panel: ", output_file_path("county_production_panel.csv"))
message("Saved long panel: ", output_file_path("county_production_panel_long.csv"))
message("Saved unmatched county audit: ", output_file_path("county_production_unmatched_historical_counties.csv"))
message("Saved CPI deflator: ", output_file_path("county_production_cpi_deflator.csv"))
