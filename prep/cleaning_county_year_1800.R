###############################################################################
# Project: Elite High Schools and Scientific Advance
# Goal:    Year-level county panel 1800-2000 (no decade collapse).
#
# Inputs
#   - Raw cross-verified database (path overridable via ELITE_RAW_PATH).
#   - 2020 county shapefile from local tigris cache.
#   - prep/output/county_population.csv (decennial, with manual patches).
#   - prep/input/new_births_total_number_estimated.csv (annual US births).
#
# Outputs
#   - output/us_panel_county_stem_year_1800.csv
#   - output/us_panel_county_stem_year_1800_nyc_merged.csv
#
# Differences from cleaning_county_1800.R (the decade-level companion):
#   - Time unit is `year` (1 row per county-year) instead of `decade`.
#   - Population is linearly interpolated between decennial Census values.
#     Cells outside the available decennial range remain NA.
#   - The births denominator uses annual US births directly from the
#     Gapminder series, scaled to county level by county pop / US pop in
#     the same year.
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("tidyverse")
  library("sf")
  library("data.table")
  library("readr")
})

initial_time <- Sys.time()
sf::sf_use_s2(FALSE)

source("stem_labels.R")
source("../paths.R")

###############################################################################
# Paths
###############################################################################

raw_path     <- Sys.getenv("ELITE_RAW_PATH",
                           unset = file.path(DATA_INPUT, "cross-verified-database.csv"))
pop_path     <- file.path(DATA_OUTPUT, "county_population.csv")
births_path  <- file.path(DATA_INPUT,  "new_births_total_number_estimated.csv")
county_shp   <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "tigris", "tigris", "Cache", "cb_2020_us_county_20m.shp"
)

if (!file.exists(county_shp)) {
  stop("Cached county shapefile not found at: ", county_shp)
}
if (!file.exists(pop_path)) {
  stop("Decennial population panel not found. Run build_county_population.R first.")
}

###############################################################################
# 1. Births: load and classify
###############################################################################

raw_data <- as_tibble(
  fread(
    raw_path,
    select = c(
      "wikidata_code", "birth", "death", "bplo1", "bpla1",
      "citizenship_1_b", "level1_main_occ", "level3_main_occ", "level3_all_occ"
    ),
    showProgress = TRUE
  )
)

# Keep every Wikipedia person with a usable birth year and coordinates so we
# can produce parallel aggregates for all-Wikipedia, all-science, and STEM.
data_clean <- raw_data %>%
  drop_na(birth, bplo1, bpla1) %>%
  filter(birth >= 1800, birth <= 2000) %>%
  mutate(
    year   = as.integer(birth),
    is_sci = as.integer(level1_main_occ == "Discovery/Science")
  ) %>%
  add_stem_dummy() %>%
  mutate(stem = if_else(is_sci == 1L, as.integer(stem), 0L)) %>%
  select(wikidata_code, year, death, bplo1, bpla1,
         citizenship_1_b, level3_occ, is_sci, stem)

cat("All Wikipedia births kept:    ", nrow(data_clean), "\n", sep = "")
cat("Discovery/Science births:     ", sum(data_clean$is_sci), "\n", sep = "")
cat("STEM share of scientific:     ",
    round(sum(data_clean$stem) / sum(data_clean$is_sci), 3), "\n", sep = "")

###############################################################################
# 2. County geometry
###############################################################################

counties_sf <- st_read(county_shp, quiet = TRUE) %>%
  st_transform(5070) %>%
  select(GEOID, NAME, STATEFP, COUNTYFP, geometry) %>%
  filter(as.integer(STATEFP) <= 56) %>%
  mutate(GEOID = str_pad(as.character(GEOID), 5, "left", "0"))

county_centroids <- counties_sf %>%
  st_centroid() %>%
  st_transform(4326) %>%
  mutate(
    lon_county = st_coordinates(.)[, 1],
    lat_county = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(GEOID, lon_county, lat_county)

###############################################################################
# 3. Spatial join and aggregation by GEOID x year
###############################################################################

inventors_sf <- data_clean %>%
  st_as_sf(coords = c("bplo1", "bpla1"), crs = 4326, remove = FALSE) %>%
  st_transform(5070)

inventors_county <- st_join(inventors_sf, counties_sf["GEOID"], join = st_within) %>%
  st_drop_geometry() %>%
  filter(!is.na(GEOID))

cat("Individuals assigned to US counties: ", nrow(inventors_county), "\n",
    sep = "")

all_wiki_agg <- inventors_county %>% count(GEOID, year, name = "n_all_wiki")
allsci_agg   <- inventors_county %>% filter(is_sci == 1) %>%
                  count(GEOID, year, name = "n_inventors")
stem_agg     <- inventors_county %>% filter(stem == 1) %>%
                  count(GEOID, year, name = "n_stem")

###############################################################################
# 4. Population: decennial -> annual via linear interpolation
###############################################################################

pop_decennial <- read_csv(pop_path, show_col_types = FALSE) %>%
  mutate(
    GEOID  = str_pad(as.character(GEOID), 5, "left", "0"),
    decade = as.integer(decade)
  ) %>%
  rename(population_source_decennial = source) %>%
  arrange(GEOID, decade)

interp_one <- function(df) {
  if (sum(!is.na(df$population)) < 2) {
    df$population_year <- df$population
    return(df)
  }
  df$population_year <- approx(
    x = df$decade, y = df$population, xout = df$year,
    method = "linear", rule = 1
  )$y
  df
}

# Build the year skeleton and join decennial pop, then interpolate per county.
year_skeleton <- expand_grid(
  GEOID = counties_sf$GEOID,
  year  = seq(1800L, 2000L, by = 1L)
)

pop_at_year <- year_skeleton %>%
  left_join(
    pop_decennial %>%
      transmute(GEOID, decade, population, population_source_decennial),
    by = c("GEOID" = "GEOID", "year" = "decade")
  ) %>%
  mutate(decade = year) %>%   # keep the matched decade value where present
  group_by(GEOID) %>%
  group_modify(~ {
    df <- .x
    df$decade <- ifelse(is.na(df$population), NA_integer_, df$year)
    # Build a temporary frame that fully captures the decennial knots.
    knots <- pop_decennial %>%
      filter(GEOID == .y$GEOID) %>%
      transmute(year = decade, population_decade = population,
                source_decade = population_source_decennial) %>%
      filter(year >= 1800, year <= 2000)
    full <- df %>% select(year) %>%
      left_join(knots, by = "year")
    if (sum(!is.na(full$population_decade)) >= 2) {
      full$population_year <- approx(
        x = full$year, y = full$population_decade,
        xout = full$year, method = "linear", rule = 1
      )$y
    } else if (sum(!is.na(full$population_decade)) == 1) {
      full$population_year <- full$population_decade
    } else {
      full$population_year <- NA_real_
    }
    full %>% transmute(year, population = population_year,
                       population_source = source_decade)
  }) %>%
  ungroup()

# Tag each year with the source of the floor-decennial knot it inherits
# from (year 1825 -> source of the 1820 knot, year 1849 -> source of the
# 1840 knot, year 1850 -> source of the 1850 knot). This keeps the source
# label informative across interpolated years so downstream filters can
# drop "hyde-derived" years cleanly.
pop_at_year <- pop_at_year %>%
  mutate(floor_decade = as.integer(floor(year / 10) * 10)) %>%
  left_join(
    pop_decennial %>%
      transmute(GEOID, floor_decade = decade,
                source_floor = population_source_decennial),
    by = c("GEOID", "floor_decade")
  ) %>%
  mutate(population_source = source_floor) %>%
  select(-floor_decade, -source_floor)

###############################################################################
# 5. Annual US births and birth rate
###############################################################################

us_births_year <- read_csv(births_path, show_col_types = FALSE) %>%
  filter(geo == "usa") %>%
  select(-geo, -name) %>%
  pivot_longer(everything(), names_to = "year", values_to = "us_births_year") %>%
  mutate(year = as.integer(year)) %>%
  filter(year >= 1800, year <= 2000)

us_pop_year <- pop_at_year %>%
  group_by(year) %>%
  summarise(us_pop_year = sum(population, na.rm = TRUE), .groups = "drop")

###############################################################################
# 6. Build the year panel
###############################################################################

panel <- year_skeleton %>%
  left_join(pop_at_year,      by = c("GEOID", "year")) %>%
  left_join(all_wiki_agg,     by = c("GEOID", "year")) %>%
  left_join(allsci_agg,       by = c("GEOID", "year")) %>%
  left_join(stem_agg,         by = c("GEOID", "year")) %>%
  left_join(us_births_year,   by = "year") %>%
  left_join(us_pop_year,      by = "year") %>%
  left_join(county_centroids, by = "GEOID") %>%
  mutate(
    n_all_wiki                   = replace_na(n_all_wiki, 0L),
    n_inventors                  = replace_na(n_inventors, 0L),
    n_stem                       = replace_na(n_stem, 0L),
    any_all_wiki                 = as.integer(n_all_wiki > 0),
    any_allsci                   = as.integer(n_inventors > 0),
    any_stem                     = as.integer(n_stem > 0),
    any_all_wiki_pct             = 100 * any_all_wiki,
    any_allsci_pct               = 100 * any_allsci,
    any_stem_pct                 = 100 * any_stem,
    log1p_n_all_wiki             = log1p(n_all_wiki),
    log1p_n_inventors            = log1p(n_inventors),
    log1p_n_stem                 = log1p(n_stem),
    us_birth_rate_year           = if_else(us_pop_year > 0,
                                           us_births_year / us_pop_year,
                                           NA_real_),
    county_births_estimate_year  = if_else(!is.na(population),
                                           population * us_birth_rate_year,
                                           NA_real_),
    inv_per_100k = if_else(!is.na(population) & population > 0,
                           1e5 * n_inventors / population, NA_real_),
    stem_per_100k = if_else(!is.na(population) & population > 0,
                            1e5 * n_stem / population, NA_real_),
    all_wiki_per_1000_pop = if_else(!is.na(population) & population > 0,
                                    1000 * n_all_wiki / population, NA_real_),
    allsci_per_1000_pop = if_else(!is.na(population) & population > 0,
                                  1000 * n_inventors / population, NA_real_),
    stem_per_1000_pop = if_else(!is.na(population) & population > 0,
                                1000 * n_stem / population, NA_real_),
    all_wiki_per_1000_births = if_else(
      !is.na(county_births_estimate_year) & county_births_estimate_year > 0,
      1000 * n_all_wiki / county_births_estimate_year, NA_real_),
    allsci_per_1000_births = if_else(
      !is.na(county_births_estimate_year) & county_births_estimate_year > 0,
      1000 * n_inventors / county_births_estimate_year, NA_real_),
    stem_per_1000_births = if_else(
      !is.na(county_births_estimate_year) & county_births_estimate_year > 0,
      1000 * n_stem / county_births_estimate_year, NA_real_),
    stem_over_allwiki_pct = if_else(n_all_wiki > 0,
                                    100 * n_stem / n_all_wiki, NA_real_)
  ) %>%
  arrange(GEOID, year)

panel_cols <- c(
  "GEOID", "year",
  "population", "population_source",
  "us_births_year", "us_pop_year",
  "us_birth_rate_year", "county_births_estimate_year",
  "n_all_wiki", "n_inventors", "n_stem",
  "any_all_wiki", "any_allsci", "any_stem",
  "any_all_wiki_pct", "any_allsci_pct", "any_stem_pct",
  "inv_per_100k", "stem_per_100k",
  "all_wiki_per_1000_pop", "allsci_per_1000_pop", "stem_per_1000_pop",
  "all_wiki_per_1000_births", "allsci_per_1000_births", "stem_per_1000_births",
  "stem_over_allwiki_pct",
  "log1p_n_all_wiki", "log1p_n_inventors", "log1p_n_stem",
  "lon_county", "lat_county"
)
panel <- panel %>% select(all_of(panel_cols))

###############################################################################
# 7. NYC-merged variant
###############################################################################

nyc_boroughs <- c("36005", "36047", "36061", "36081", "36085")
nyc_geoid    <- "36000"

panel_nyc <- panel %>%
  filter(GEOID %in% nyc_boroughs) %>%
  group_by(year) %>%
  summarise(
    GEOID                       = nyc_geoid,
    population                  = sum(population, na.rm = TRUE),
    population_source           = "merged_nyc",
    us_births_year              = first(us_births_year),
    us_pop_year                 = first(us_pop_year),
    us_birth_rate_year          = first(us_birth_rate_year),
    n_all_wiki                  = sum(n_all_wiki, na.rm = TRUE),
    n_inventors                 = sum(n_inventors, na.rm = TRUE),
    n_stem                      = sum(n_stem, na.rm = TRUE),
    lon_county                  = mean(lon_county, na.rm = TRUE),
    lat_county                  = mean(lat_county, na.rm = TRUE),
    .groups                     = "drop"
  ) %>%
  mutate(
    population = if_else(population == 0, NA_real_, population),
    any_all_wiki = as.integer(n_all_wiki > 0),
    any_allsci   = as.integer(n_inventors > 0),
    any_stem     = as.integer(n_stem > 0),
    any_all_wiki_pct = 100 * any_all_wiki,
    any_allsci_pct   = 100 * any_allsci,
    any_stem_pct     = 100 * any_stem,
    log1p_n_all_wiki  = log1p(n_all_wiki),
    log1p_n_inventors = log1p(n_inventors),
    log1p_n_stem      = log1p(n_stem),
    county_births_estimate_year = if_else(!is.na(population),
                                          population * us_birth_rate_year,
                                          NA_real_),
    inv_per_100k  = if_else(!is.na(population) & population > 0,
                            1e5 * n_inventors / population, NA_real_),
    stem_per_100k = if_else(!is.na(population) & population > 0,
                            1e5 * n_stem / population, NA_real_),
    all_wiki_per_1000_pop = if_else(!is.na(population) & population > 0,
                                    1000 * n_all_wiki / population, NA_real_),
    allsci_per_1000_pop = if_else(!is.na(population) & population > 0,
                                  1000 * n_inventors / population, NA_real_),
    stem_per_1000_pop = if_else(!is.na(population) & population > 0,
                                1000 * n_stem / population, NA_real_),
    all_wiki_per_1000_births = if_else(
      !is.na(county_births_estimate_year) & county_births_estimate_year > 0,
      1000 * n_all_wiki / county_births_estimate_year, NA_real_),
    allsci_per_1000_births = if_else(
      !is.na(county_births_estimate_year) & county_births_estimate_year > 0,
      1000 * n_inventors / county_births_estimate_year, NA_real_),
    stem_per_1000_births = if_else(
      !is.na(county_births_estimate_year) & county_births_estimate_year > 0,
      1000 * n_stem / county_births_estimate_year, NA_real_),
    stem_over_allwiki_pct = if_else(n_all_wiki > 0,
                                    100 * n_stem / n_all_wiki, NA_real_)
  ) %>%
  select(all_of(panel_cols))

panel_nyc_merged <- bind_rows(
  panel %>% filter(!GEOID %in% nyc_boroughs),
  panel_nyc
) %>% arrange(GEOID, year)

###############################################################################
# 8. Export
###############################################################################

write_csv(panel,            file.path(DATA_OUTPUT, "us_panel_county_stem_year_1800.csv"))
write_csv(panel_nyc_merged, file.path(DATA_OUTPUT, "us_panel_county_stem_year_1800_nyc_merged.csv"))

cat("\n=== Year-level panel complete ===\n")
cat("Standard panel rows:        ", nrow(panel), "\n", sep = "")
cat("NYC-merged panel rows:      ", nrow(panel_nyc_merged), "\n", sep = "")
cat("Year range:                 ", min(panel$year), " to ", max(panel$year),
    "\n", sep = "")
cat("Counties with any STEM:     ",
    panel %>% filter(n_stem > 0) %>% pull(GEOID) %>% n_distinct(),
    "\n", sep = "")

elapsed <- difftime(Sys.time(), initial_time, units = "mins")
cat("Runtime (minutes): ", round(as.numeric(elapsed), 2), "\n", sep = "")
