###############################################################################
# Project: Determinants of Talent Production
# Goal: Cleaning pipeline for STEM-focused analysis
#
# Outputs:
#   output/data_final_stem.csv      — country-year panel, STEM scientists only
#   output/data_final_allsci.csv    — country-year panel, all Discovery/Science
#   output/crossverified_with_stem.csv — individual-level with stem dummy
###############################################################################

rm(list = ls())

# Packages
library("tidyverse")
library("sf")
library("rnaturalearth")

# Recording Initial Time
initial_time <- Sys.time()

###############################################################################
# Source STEM helper (defines stem_occ, stem_regex, add_stem_dummy)
###############################################################################
source("stem_labels.R")

###############################################################################
# Read raw data
###############################################################################
raw_data <- read.csv("input/cross-verified-database.csv")

###############################################################################
# Filter: Discovery/Science, birth >= 1800, drop NA on birth/bplo1/bpla1
###############################################################################
data_clean <- raw_data %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  filter(birth >= 1800) %>%
  drop_na(birth, bplo1, bpla1)

###############################################################################
# Reclassify occupations and add stem dummy
###############################################################################
data_clean <- add_stem_dummy(data_clean)

###############################################################################
# Adding country from coordinates
# (adapted from cleaning.R; we select only needed columns before the sf join
#  to avoid the person-name / country-name column conflict)
###############################################################################
world <- ne_countries(returnclass = "sf")

data_clean_geo <- data_clean %>%
  select(wikidata_code, birth, death, bplo1, bpla1, citizenship_1_b,
         level3_occ, stem)

points <- st_as_sf(data_clean_geo, coords = c("bplo1", "bpla1"), crs = 4326, remove = FALSE)

sf::sf_use_s2(FALSE)
points_with_country <- st_join(points, world[, c("iso_a3", "name")], join = st_within, left = TRUE)
points_with_country <- points_with_country %>%
  mutate(
    iso_a3 = case_when(
      iso_a3 == "-99" & grepl("France",      name, ignore.case = TRUE) ~ "FRA",
      iso_a3 == "-99" & grepl("Norway",      name, ignore.case = TRUE) ~ "NOR",
      iso_a3 == "-99" & grepl("Denmark",     name, ignore.case = TRUE) ~ "DNK",
      iso_a3 == "-99" & grepl("Netherlands", name, ignore.case = TRUE) ~ "NLD",
      TRUE ~ iso_a3
    )
  )
sf::sf_use_s2(TRUE)

data_clean_final <- points_with_country %>%
  mutate(country = name, iso3 = iso_a3) %>%
  st_drop_geometry() %>%
  select(wikidata_code, birth, death, citizenship_1_b, country, iso3,
         level3_occ, stem)

###############################################################################
# Export individual-level file with stem dummy
###############################################################################
write.csv(
  data_clean_final %>%
    select(wikidata_code, birth, death, citizenship_1_b, country, iso3,
           level3_occ, stem),
  "output/crossverified_with_stem.csv",
  row.names = FALSE
)

###############################################################################
# Aggregate: STEM only (stem == 1)
###############################################################################
data_clean_stem_agg <- data_clean_final %>%
  filter(stem == 1, !is.na(iso3), !is.na(birth)) %>%
  transmute(iso3, country, year = as.integer(birth)) %>%
  count(iso3, country, year, name = "n_stem")

###############################################################################
# Aggregate: all Discovery/Science
###############################################################################
data_clean_allsci_agg <- data_clean_final %>%
  filter(!is.na(iso3), !is.na(birth)) %>%
  transmute(iso3, country, year = as.integer(birth)) %>%
  count(iso3, country, year, name = "n_allsci")

###############################################################################
# Demographics (Gapminder)
###############################################################################
new_births      <- read.csv("input/new_births_total_number_estimated.csv")
child_mortality <- read.csv("input/child_mortality_0_5_year_olds_dying_per_1000_born.csv")
child_deaths    <- read.csv("input/number_of_child_deaths.csv")

to_long <- function(df, value_name) {
  df %>%
    mutate(iso3 = toupper(geo)) %>%
    pivot_longer(
      cols = matches("^X?\\d{4}$"),
      names_to = "year",
      values_to = value_name,
      names_transform = list(year = ~ as.integer(sub("^X", "", .x)))
    ) %>%
    transmute(iso3, country = name, year, !!value_name := .data[[value_name]])
}

nb_long <- to_long(new_births,      "births")
cm_long <- to_long(child_mortality, "mort_per_1000")
cd_long <- to_long(child_deaths,    "child_deaths")

demo_long <- nb_long %>%
  left_join(cm_long, by = c("iso3", "year")) %>%
  left_join(cd_long, by = c("iso3", "year")) %>%
  mutate(
    cohort_from_mortality = if_else(
      !is.na(births) & !is.na(mort_per_1000),
      pmax(births - round(births * (mort_per_1000 / 1000)), 0),
      NA_real_
    ),
    cohort_from_deaths = if_else(
      !is.na(births) & !is.na(child_deaths),
      pmax(births - child_deaths, 0),
      NA_real_
    )
  )

###############################################################################
# Build data_final_stem.csv
###############################################################################
final_stem <- data_clean_stem_agg %>%
  left_join(
    demo_long %>% select(iso3, year, births, cohort_from_mortality, cohort_from_deaths),
    by = c("iso3", "year")
  ) %>%
  mutate(
    rate_per_1000_mort   = if_else(coalesce(cohort_from_mortality, 0) > 0,
                                   1e3 * n_stem / cohort_from_mortality, NA_real_),
    rate_per_1000_deaths = if_else(coalesce(cohort_from_deaths, 0) > 0,
                                   1e3 * n_stem / cohort_from_deaths,    NA_real_),
    rate_per_100k_births = if_else(coalesce(births, 0) > 0,
                                   1e5 * n_stem / births,                NA_real_)
  ) %>%
  select(country, iso3, year, births, n_stem,
         rate_per_1000_mort, rate_per_1000_deaths, rate_per_100k_births)

###############################################################################
# Build data_final_allsci.csv
###############################################################################
final_allsci <- data_clean_allsci_agg %>%
  left_join(
    demo_long %>% select(iso3, year, births, cohort_from_mortality, cohort_from_deaths),
    by = c("iso3", "year")
  ) %>%
  mutate(
    rate_per_1000_mort   = if_else(coalesce(cohort_from_mortality, 0) > 0,
                                   1e3 * n_allsci / cohort_from_mortality, NA_real_),
    rate_per_1000_deaths = if_else(coalesce(cohort_from_deaths, 0) > 0,
                                   1e3 * n_allsci / cohort_from_deaths,    NA_real_),
    rate_per_100k_births = if_else(coalesce(births, 0) > 0,
                                   1e5 * n_allsci / births,                NA_real_)
  ) %>%
  select(country, iso3, year, births, n_allsci,
         rate_per_1000_mort, rate_per_1000_deaths, rate_per_100k_births)

###############################################################################
# Export
###############################################################################
write.csv(final_stem,   "output/data_final_stem.csv",   row.names = FALSE)
write.csv(final_allsci, "output/data_final_allsci.csv", row.names = FALSE)

###############################################################################
# Summary
###############################################################################
cat("=== STEM cleaning complete ===\n")
cat("Discovery/Science individuals (birth >= 1800, coords present):", nrow(data_clean_final), "\n")
cat("  of which STEM (stem == 1):", sum(data_clean_final$stem, na.rm = TRUE), "\n")
cat("  STEM share:", round(100 * mean(data_clean_final$stem, na.rm = TRUE), 1), "%\n\n")
cat("Country-year observations written:\n")
cat("  data_final_stem.csv:    ", nrow(final_stem),   "rows\n")
cat("  data_final_allsci.csv:  ", nrow(final_allsci), "rows\n")
cat("  crossverified_with_stem.csv:", nrow(data_clean_final), "rows\n\n")

final_time <- Sys.time() - initial_time
print(paste("This code ran in", round(final_time, 1), "seconds."))
