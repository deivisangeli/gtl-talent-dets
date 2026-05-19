###############################################################################
# Project: GTL Talent Determinants
# Goal: Clean county-level frontier/TFE covariates
###############################################################################

rm(list = ls())

library("tidyverse")

source("raw_paths.R")

###############################################################################
# Inputs
###############################################################################

county_covariates_raw <- raw_file_path("county_tpe_covariates.csv")
county_production_path <- output_file_path("county_production_panel.csv")
county_hyde_landuse_path <- output_file_path("county_hyde_landuse.csv")
county_inventor_rates_path <- output_file_path("county_inventor_rates_hyde.csv")

required_vars <- c(
 "gisjoin",
 "year",
 "frontier100kmL6",
 "wsexrat",
 "POSTOFFICE",
 "fb_shr",
 "existcanal"
)

county_covariates <- read_csv(county_covariates_raw, show_col_types = FALSE)
county_production <- read_csv(county_production_path, show_col_types = FALSE)
county_hyde_landuse <- read_csv(county_hyde_landuse_path, show_col_types = FALSE)
county_inventor_rates <- read_csv(county_inventor_rates_path, show_col_types = FALSE)

missing_vars <- setdiff(required_vars, names(county_covariates))
if (length(missing_vars) > 0) {
 stop("Missing required variable(s): ", paste(missing_vars, collapse = ", "))
}

required_production_vars <- c(
 "GEOID",
 "year",
 "state_nhgis",
 "county_nhgis",
 "manufacturing_output_value",
 "farming_output_value",
 "manufacturing_output_value_real_1900",
 "farming_output_value_real_1900",
 "cpi_1967_100",
 "cpi_base_1900",
 "real_1900_factor",
 "manufacturing_source_dataset",
 "farming_source_dataset",
 "has_manufacturing_output",
 "has_farming_output"
)

missing_production_vars <- setdiff(required_production_vars, names(county_production))
if (length(missing_production_vars) > 0) {
 stop("Missing required production variable(s): ",
      paste(missing_production_vars, collapse = ", "))
}

production_duplicate_keys <- county_production %>%
 count(GEOID, year, name = "n") %>%
 filter(n > 1)

if (nrow(production_duplicate_keys) > 0) {
 stop("Production panel has duplicate GEOID-year keys.")
}

required_hyde_landuse_vars <- c(
 "GEOID",
 "year",
 "cropland_km2",
 "grazeland_km2"
)

missing_hyde_landuse_vars <- setdiff(
 required_hyde_landuse_vars,
 names(county_hyde_landuse)
)
if (length(missing_hyde_landuse_vars) > 0) {
 stop("Missing required HYDE land-use variable(s): ",
      paste(missing_hyde_landuse_vars, collapse = ", "))
}

hyde_landuse_duplicate_keys <- county_hyde_landuse %>%
 count(GEOID, year, name = "n") %>%
 filter(n > 1)

if (nrow(hyde_landuse_duplicate_keys) > 0) {
 stop("HYDE land-use panel has duplicate GEOID-year keys.")
}

required_county_inventor_rate_vars <- c(
 "GEOID",
 "year",
 "hyde_population",
 "n_inventors",
 "inventors_per_100k_hyde"
)

missing_county_inventor_rate_vars <- setdiff(
 required_county_inventor_rate_vars,
 names(county_inventor_rates)
)
if (length(missing_county_inventor_rate_vars) > 0) {
 stop("Missing required county inventor rate variable(s): ",
      paste(missing_county_inventor_rate_vars, collapse = ", "))
}

county_inventor_rate_duplicate_keys <- county_inventor_rates %>%
 count(GEOID, year, name = "n") %>%
 filter(n > 1)

if (nrow(county_inventor_rate_duplicate_keys) > 0) {
 stop("County inventor rates panel has duplicate GEOID-year keys.")
}

###############################################################################
# Build total frontier exposure
###############################################################################

# The panel is decennial. Each row with frontier100kmL6 == 1 contributes 10
# years of frontier exposure to the county-level total.
frontier_years <- county_covariates %>%
 group_by(gisjoin) %>%
 summarise(
  frontier_years = sum(frontier100kmL6 == 1, na.rm = TRUE) * 10,
  .groups = "drop"
 )

###############################################################################
# Clean county-year covariates
###############################################################################

county_covariates_clean <- county_covariates %>%
 select(-any_of("frontier_years")) %>%
 mutate(
  GEOID = if_else(
   str_detect(gisjoin, "^G[0-9]{7}$"),
   paste0(str_sub(gisjoin, 2, 3), str_sub(gisjoin, 5, 7)),
   NA_character_
  )
 ) %>%
 left_join(frontier_years, by = "gisjoin") %>%
 transmute(
  GEOID,
  gisjoin,
  year,
  frontier100kmL6,
  frontier_years,
  sex_ratio = wsexrat,
  post_offices = POSTOFFICE,
  immigrant_share = fb_shr,
  canal_access = existcanal
 ) %>%
 arrange(GEOID, year)

production_for_merge <- county_production %>%
 select(
  GEOID,
  year,
  manufacturing_output_value,
  farming_output_value,
  manufacturing_output_value_real_1900,
  farming_output_value_real_1900,
  cpi_1967_100,
  cpi_base_1900,
  real_1900_factor,
  manufacturing_source_dataset,
  farming_source_dataset,
  has_manufacturing_output,
  has_farming_output
 )

production_unmatched_to_covariates <- county_production %>%
 anti_join(
  county_covariates_clean %>% distinct(GEOID, year),
  by = c("GEOID", "year")
 ) %>%
 select(
  GEOID,
  year,
  state_nhgis,
  county_nhgis,
  manufacturing_output_value,
  farming_output_value,
  has_manufacturing_output,
  has_farming_output
 ) %>%
 arrange(year, GEOID)

county_covariates_clean <- county_covariates_clean %>%
 left_join(production_for_merge, by = c("GEOID", "year")) %>%
 arrange(GEOID, year)

if (nrow(county_covariates_clean) != nrow(county_covariates)) {
 stop("Unexpected row count change after merging production data.")
}

hyde_landuse_for_merge <- county_hyde_landuse %>%
 select(GEOID, year, cropland_km2, grazeland_km2)

hyde_landuse_unmatched_to_covariates <- county_hyde_landuse %>%
 anti_join(
  county_covariates_clean %>% distinct(GEOID, year),
  by = c("GEOID", "year")
 ) %>%
 arrange(year, GEOID)

covariates_unmatched_to_hyde_landuse <- county_covariates_clean %>%
 filter(year >= 1800, year <= 1930) %>%
 anti_join(
  county_hyde_landuse %>% distinct(GEOID, year),
  by = c("GEOID", "year")
 ) %>%
 select(GEOID, gisjoin, year) %>%
 arrange(year, GEOID)

county_covariates_clean <- county_covariates_clean %>%
 left_join(hyde_landuse_for_merge, by = c("GEOID", "year")) %>%
 arrange(GEOID, year)

if (nrow(county_covariates_clean) != nrow(county_covariates)) {
 stop("Unexpected row count change after merging HYDE land-use data.")
}

inventor_rates_for_merge <- county_inventor_rates %>%
 select(GEOID, year, hyde_population, n_inventors, inventors_per_100k_hyde)

inventor_rates_unmatched_to_covariates <- county_inventor_rates %>%
 filter(year %in% unique(county_covariates_clean$year)) %>%
 anti_join(
  county_covariates_clean %>% distinct(GEOID, year),
  by = c("GEOID", "year")
 ) %>%
 arrange(year, GEOID)

covariates_unmatched_to_inventor_rates <- county_covariates_clean %>%
 filter(year >= 1800, year <= 1930) %>%
 anti_join(
  county_inventor_rates %>% distinct(GEOID, year),
  by = c("GEOID", "year")
 ) %>%
 select(GEOID, gisjoin, year) %>%
 arrange(year, GEOID)

county_covariates_clean <- county_covariates_clean %>%
 left_join(inventor_rates_for_merge, by = c("GEOID", "year")) %>%
 mutate(
  n_inventors = if_else(
   year >= 1800 & year <= 1930,
   replace_na(n_inventors, 0L),
   NA_integer_
  ),
  inventors_per_100k_hyde = if_else(
   !is.na(hyde_population) & hyde_population > 0,
   1e5 * n_inventors / hyde_population,
   NA_real_
  )
 ) %>%
 arrange(GEOID, year)

if (nrow(county_covariates_clean) != nrow(county_covariates)) {
 stop("Unexpected row count change after merging inventor rates.")
}

###############################################################################
# Validation
###############################################################################

if (nrow(county_covariates_clean) != nrow(county_covariates)) {
 stop("Unexpected row count change while cleaning county covariates.")
}

frontier_years_check <- county_covariates_clean %>%
 group_by(gisjoin) %>%
 summarise(n_frontier_years = n_distinct(frontier_years), .groups = "drop")

if (any(frontier_years_check$n_frontier_years != 1)) {
 stop("frontier_years is not constant within gisjoin.")
}

if (any(is.na(county_covariates_clean$n_inventors[
 county_covariates_clean$year >= 1800 & county_covariates_clean$year <= 1930
]))) {
 stop("n_inventors has missing values in 1800-1930.")
}

###############################################################################
# Export
###############################################################################

write_csv(
 county_covariates_clean,
 output_file_path("county_tpe_covariates_clean.csv"),
 na = ""
)

write_csv(
 production_unmatched_to_covariates,
 output_file_path("county_production_unmatched_to_tfe_covariates.csv"),
 na = ""
)

write_csv(
 hyde_landuse_unmatched_to_covariates,
 output_file_path("county_hyde_landuse_unmatched_to_tfe_covariates.csv"),
 na = ""
)

write_csv(
 covariates_unmatched_to_hyde_landuse,
 output_file_path("county_tfe_covariates_unmatched_to_hyde_landuse.csv"),
 na = ""
)

write_csv(
 inventor_rates_unmatched_to_covariates,
 output_file_path("county_inventor_rates_unmatched_to_tfe_covariates.csv"),
 na = ""
)

write_csv(
 covariates_unmatched_to_inventor_rates,
 output_file_path("county_tfe_covariates_unmatched_to_inventor_rates.csv"),
 na = ""
)

message(
 "Saved clean county covariates: ",
 output_file_path("county_tpe_covariates_clean.csv")
)

message(
 "Saved production merge audit: ",
 output_file_path("county_production_unmatched_to_tfe_covariates.csv")
)

message(
 "Saved HYDE land-use merge audit: ",
 output_file_path("county_hyde_landuse_unmatched_to_tfe_covariates.csv")
)

message(
 "Saved TFE covariates without HYDE land-use audit: ",
 output_file_path("county_tfe_covariates_unmatched_to_hyde_landuse.csv")
)

message(
 "Saved inventor rates merge audit: ",
 output_file_path("county_inventor_rates_unmatched_to_tfe_covariates.csv")
)

message(
 "Saved TFE covariates without inventor rates audit: ",
 output_file_path("county_tfe_covariates_unmatched_to_inventor_rates.csv")
)
