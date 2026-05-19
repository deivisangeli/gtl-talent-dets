###############################################################################
# Project: GTL Talent Determinants
# Goal: Build county-level inventor rates using Wikipedia and HYDE population
###############################################################################

rm(list = ls())

library("tidyverse")
library("data.table")
library("sf")
library("tigris")

source("raw_paths.R")

###############################################################################
# Parameters and inputs
###############################################################################

years <- seq(1800, 2000, by = 10)
county_hyde_population_path <- output_file_path("county_hyde_population.csv")

county_hyde_population <- read_csv(
 county_hyde_population_path,
 show_col_types = FALSE
)

required_population_vars <- c("GEOID", "year", "hyde_population")
missing_population_vars <- setdiff(
 required_population_vars,
 names(county_hyde_population)
)

if (length(missing_population_vars) > 0) {
 stop("Missing required HYDE population variable(s): ",
      paste(missing_population_vars, collapse = ", "))
}

missing_population_years <- setdiff(years, unique(county_hyde_population$year))
if (length(missing_population_years) > 0) {
 stop(
  "county_hyde_population.csv does not cover required year(s): ",
  paste(missing_population_years, collapse = ", "),
  ". Run prep/build_county_hyde_population.R first."
 )
}

population_duplicate_keys <- county_hyde_population %>%
 count(GEOID, year, name = "n") %>%
 filter(n > 1)

if (nrow(population_duplicate_keys) > 0) {
 stop("HYDE population panel has duplicate GEOID-year keys.")
}

###############################################################################
# County polygons
###############################################################################

tigris_cache_path <- tigris_cache_dir()
options(tigris_use_cache = TRUE, tigris_cache_dir = tigris_cache_path)

counties_sf <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 st_transform(4326) %>%
 filter(as.integer(STATEFP) <= 56) %>%
 select(GEOID, geometry)

###############################################################################
# Wikipedia inventor counts
###############################################################################

raw_wikipedia <- fread(
 ensure_wikipedia_csv(),
 select = c("wikidata_code", "birth", "bplo1", "bpla1", "level1_main_occ")
)

inventors_clean <- raw_wikipedia %>%
 as_tibble() %>%
 drop_na(birth, bplo1, bpla1) %>%
 filter(
  level1_main_occ == "Discovery/Science",
  birth >= 1800
 ) %>%
 mutate(year = floor(birth / 10) * 10) %>%
 filter(year %in% years) %>%
 select(wikidata_code, year, bplo1, bpla1)

inventors_sf <- inventors_clean %>%
 st_as_sf(coords = c("bplo1", "bpla1"), crs = 4326, remove = FALSE)

inventors_county <- st_join(
 inventors_sf,
 counties_sf["GEOID"],
 join = st_within
) %>%
 st_drop_geometry() %>%
 filter(!is.na(GEOID))

inventors_agg <- inventors_county %>%
 count(GEOID, year, name = "n_inventors")

###############################################################################
# Balanced county-decade panel
###############################################################################

county_inventor_rates <- expand_grid(
 GEOID = counties_sf$GEOID,
 year = years
) %>%
 left_join(
  county_hyde_population %>%
   filter(year %in% years) %>%
   select(GEOID, year, hyde_population),
  by = c("GEOID", "year")
 ) %>%
 left_join(inventors_agg, by = c("GEOID", "year")) %>%
 mutate(
  n_inventors = replace_na(n_inventors, 0L),
  inventors_per_100k_hyde = if_else(
   !is.na(hyde_population) & hyde_population > 0,
   1e5 * n_inventors / hyde_population,
   NA_real_
  )
 ) %>%
 arrange(GEOID, year)

###############################################################################
# Validation and export
###############################################################################

expected_rows <- n_distinct(counties_sf$GEOID) * length(years)
if (nrow(county_inventor_rates) != expected_rows) {
 stop("Unexpected row count in county inventor rates panel.")
}

duplicate_keys <- county_inventor_rates %>%
 count(GEOID, year, name = "n") %>%
 filter(n > 1)

if (nrow(duplicate_keys) > 0) {
 stop("County inventor rates panel has duplicate GEOID-year keys.")
}

if (any(is.na(county_inventor_rates$n_inventors))) {
 stop("n_inventors has missing values.")
}

write_csv(
 county_inventor_rates,
 output_file_path("county_inventor_rates_hyde.csv"),
 na = ""
)

coverage_summary <- county_inventor_rates %>%
 group_by(year) %>%
 summarise(
  n_counties = n(),
  inventors_total = sum(n_inventors),
  counties_with_hyde_population = sum(!is.na(hyde_population)),
  counties_with_rate = sum(!is.na(inventors_per_100k_hyde)),
  .groups = "drop"
 )

write_csv(
 coverage_summary,
 output_file_path("county_inventor_rates_hyde_coverage.csv"),
 na = ""
)

message(
 "Saved county inventor rates: ",
 output_file_path("county_inventor_rates_hyde.csv")
)

message(
 "Saved county inventor rates coverage: ",
 output_file_path("county_inventor_rates_hyde_coverage.csv")
)
