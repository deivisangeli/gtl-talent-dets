###############################################################################
# Project: Elite High Schools and Scientific Advance
# Goal:    Build the analysis-ready county-decade panel(s) from 1800 onward.
#
# Inputs
#   - Raw cross-verified database (births, occupations, coordinates).
#     Path overridable via env var ELITE_RAW_PATH so the script can read
#     from a sibling repo without copying the 1.1 GB file.
#   - 2020 county shapefile from the local tigris cache.
#   - Unified county-decade population panel: prep/output/county_population.csv
#     (built by prep/build_county_population.R; combines NHGIS, HYDE, and
#     hand-curated 1790-1840 Census patches for the high-access counties).
#   - National annual US-births series:
#     prep/input/new_births_total_number_estimated.csv
#
# Outputs
#   - output/us_panel_county_1800.csv               all-science count panel
#   - output/us_panel_county_stem_1800.csv          STEM count panel,
#                                                   boroughs separate
#   - output/us_panel_county_stem_1800_nyc_merged.csv
#                                                   STEM count panel,
#                                                   five NYC boroughs
#                                                   collapsed into the
#                                                   synthetic GEOID "36000"
#                                                   (treated as one labor
#                                                   market and one elite-
#                                                   educational ecosystem)
#
# All derived columns the analysis needs are computed here:
#   - population (taken from the unified pop panel; column population_source
#     records the source of each cell)
#   - n_stem, any_stem, any_stem_pct, n_inventors
#   - us_births_in_decade, us_pop_decade, us_birth_rate_decade
#   - county_births_estimate_decade
#       = population * us_birth_rate_decade
#   - stem_per_1000_pop      = 1000 * n_stem / population
#   - stem_per_1000_births   = 1000 * n_stem / county_births_estimate_decade
#
# Convention: GEOID is always a five-digit zero-padded string.
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
  stop("Unified population panel not found. Run build_county_population.R first.")
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

# Keep every Wikipedia person with a usable birth year and birth coordinates;
# downstream code carries three indicators per row so we can aggregate
# (i) all-Wikipedia, (ii) Discovery/Science (all scientific), and
# (iii) the hard-STEM subset of Discovery/Science.
data_clean <- raw_data %>%
  drop_na(birth, bplo1, bpla1) %>%
  filter(birth >= 1800, birth <= 2000) %>%
  mutate(
    decade  = floor(birth / 10) * 10,
    is_sci  = as.integer(level1_main_occ == "Discovery/Science")
  ) %>%
  add_stem_dummy() %>%
  mutate(
    # stem dummy is only meaningful for Discovery/Science; zero it out for
    # non-science rows so the level3 reclassification regex doesn't pick up
    # stray matches (e.g. an academic flagged as "historian").
    stem = if_else(is_sci == 1L, as.integer(stem), 0L)
  ) %>%
  select(wikidata_code, birth, death, bplo1, bpla1,
         citizenship_1_b, decade, level3_occ, is_sci, stem)

cat("All Wikipedia births kept:    ", nrow(data_clean), "\n", sep = "")
cat("Discovery/Science births:     ", sum(data_clean$is_sci), "\n", sep = "")
cat("STEM share of scientific:     ",
    round(sum(data_clean$stem) / sum(data_clean$is_sci), 3), "\n", sep = "")

###############################################################################
# 2. County geometry from local cache
###############################################################################

counties_sf <- st_read(county_shp, quiet = TRUE) %>%
  st_transform(5070) %>%
  select(GEOID, NAME, STATEFP, COUNTYFP, geometry) %>%
  filter(as.integer(STATEFP) <= 56) %>%
  mutate(GEOID = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"))

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
# 3. Assign births to counties
###############################################################################

inventors_sf <- data_clean %>%
  st_as_sf(coords = c("bplo1", "bpla1"), crs = 4326, remove = FALSE) %>%
  st_transform(5070)

inventors_county <- st_join(inventors_sf, counties_sf["GEOID"], join = st_within) %>%
  st_drop_geometry() %>%
  filter(!is.na(GEOID))

cat("Individuals assigned to US counties: ", nrow(inventors_county), "\n",
    sep = "")

###############################################################################
# 4. Aggregate counts by county x decade
###############################################################################

all_wiki_agg <- inventors_county %>%
  count(GEOID, decade, name = "n_all_wiki")

allsci_agg <- inventors_county %>%
  filter(is_sci == 1) %>%
  count(GEOID, decade, name = "n_inventors")

stem_agg <- inventors_county %>%
  filter(stem == 1) %>%
  count(GEOID, decade, name = "n_stem")

###############################################################################
# 5. Population (unified panel)
###############################################################################

pop_panel <- read_csv(pop_path, show_col_types = FALSE) %>%
  mutate(
    GEOID  = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade = as.integer(decade)
  ) %>%
  rename(population_source = source)

###############################################################################
# 6. National birth-rate series for the births denominator
#
# US annual births are converted to a per-decade total. The per-decade US
# birth rate is then defined as us_births_in_decade / sum(US county
# population in that decade). Each county's estimated births are
# population * birth_rate. This applies the national birth rate to each
# county's population — captures temporal variation in fertility but not
# cross-county variation in birth rates. The Gapminder series starts at
# 1800, which matches the panel start.
###############################################################################

us_births_decade <- read_csv(births_path, show_col_types = FALSE) %>%
  filter(geo == "usa") %>%
  select(-geo, -name) %>%
  pivot_longer(everything(), names_to = "year", values_to = "us_births_year") %>%
  mutate(
    year   = as.integer(year),
    decade = (year %/% 10L) * 10L
  ) %>%
  group_by(decade) %>%
  summarise(us_births_in_decade = sum(us_births_year, na.rm = TRUE),
            .groups = "drop")

us_pop_decade <- pop_panel %>%
  group_by(decade) %>%
  summarise(us_pop_decade = sum(population, na.rm = TRUE), .groups = "drop")

###############################################################################
# 7. Build the 1800-2000 balanced panel
###############################################################################

panel_skeleton <- expand_grid(
  GEOID  = counties_sf$GEOID,
  decade = seq(1800L, 2000L, by = 10L)
)

panel <- panel_skeleton %>%
  left_join(pop_panel,        by = c("GEOID", "decade")) %>%
  left_join(all_wiki_agg,     by = c("GEOID", "decade")) %>%
  left_join(allsci_agg,       by = c("GEOID", "decade")) %>%
  left_join(stem_agg,         by = c("GEOID", "decade")) %>%
  left_join(us_births_decade, by = "decade") %>%
  left_join(us_pop_decade,    by = "decade") %>%
  left_join(county_centroids, by = "GEOID") %>%
  mutate(
    n_all_wiki                    = replace_na(n_all_wiki, 0L),
    n_inventors                   = replace_na(n_inventors, 0L),
    n_stem                        = replace_na(n_stem, 0L),
    any_all_wiki                  = as.integer(n_all_wiki > 0),
    any_allsci                    = as.integer(n_inventors > 0),
    any_stem                      = as.integer(n_stem > 0),
    any_all_wiki_pct              = 100 * any_all_wiki,
    any_allsci_pct                = 100 * any_allsci,
    any_stem_pct                  = 100 * any_stem,
    log1p_n_all_wiki              = log1p(n_all_wiki),
    log1p_n_inventors             = log1p(n_inventors),
    log1p_n_stem                  = log1p(n_stem),
    us_birth_rate_decade          = if_else(us_pop_decade > 0,
                                            us_births_in_decade / us_pop_decade,
                                            NA_real_),
    county_births_estimate_decade = if_else(!is.na(population),
                                            population * us_birth_rate_decade,
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
      !is.na(county_births_estimate_decade) & county_births_estimate_decade > 0,
      1000 * n_all_wiki / county_births_estimate_decade, NA_real_),
    allsci_per_1000_births = if_else(
      !is.na(county_births_estimate_decade) & county_births_estimate_decade > 0,
      1000 * n_inventors / county_births_estimate_decade, NA_real_),
    stem_per_1000_births = if_else(
      !is.na(county_births_estimate_decade) & county_births_estimate_decade > 0,
      1000 * n_stem / county_births_estimate_decade, NA_real_),
    stem_within_notable_share        = if_else(n_inventors > 0,
                                               n_stem / n_inventors, NA_real_),
    stem_within_notable_share_zero   = replace_na(stem_within_notable_share, 0),
    stem_within_notable_share_smooth = if_else(n_inventors > 0,
                                               (n_stem + 0.5) / (n_inventors + 1),
                                               0),
    stem_within_notable_pct        = 100 * stem_within_notable_share,
    stem_within_notable_pct_zero   = 100 * stem_within_notable_share_zero,
    stem_within_notable_pct_smooth = 100 * stem_within_notable_share_smooth,
    # Share of STEM among all Wikipedia-notable births in the county-decade.
    # Denominator is n_all_wiki (any occupation), not n_inventors. NA where
    # no one in the county-decade has a Wikipedia entry.
    stem_over_allwiki_pct = if_else(n_all_wiki > 0,
                                    100 * n_stem / n_all_wiki, NA_real_)
  ) %>%
  arrange(GEOID, decade)

panel_allsci <- panel %>%
  select(GEOID, decade, population, population_source,
         n_inventors, any_allsci,
         inv_per_100k, log1p_n_inventors, lon_county, lat_county)

panel_stem_cols <- c(
  "GEOID", "decade",
  "population", "population_source",
  "us_births_in_decade", "us_pop_decade",
  "us_birth_rate_decade", "county_births_estimate_decade",
  "n_all_wiki", "n_inventors", "n_stem",
  "any_all_wiki", "any_allsci", "any_stem",
  "any_all_wiki_pct", "any_allsci_pct", "any_stem_pct",
  "inv_per_100k", "stem_per_100k",
  "all_wiki_per_1000_pop", "allsci_per_1000_pop", "stem_per_1000_pop",
  "all_wiki_per_1000_births", "allsci_per_1000_births", "stem_per_1000_births",
  "stem_within_notable_share", "stem_within_notable_share_zero",
  "stem_within_notable_share_smooth",
  "stem_within_notable_pct", "stem_within_notable_pct_zero",
  "stem_within_notable_pct_smooth",
  "stem_over_allwiki_pct",
  "log1p_n_all_wiki", "log1p_n_inventors", "log1p_n_stem",
  "lon_county", "lat_county"
)
panel_stem <- panel %>% select(all_of(panel_stem_cols))

###############################################################################
# 8. NYC-merged variant
#
# Substantively NYC is one labor market and one elite-educational ecosystem;
# the staggered borough-school timing (Hunter 1869, Stuyvesant 1904, Brooklyn
# Tech 1922, Bronx Sci 1938) does not isolate clean treatment-introduction
# effects because Brooklyn and the Bronx already had substantial pre-existing
# infrastructure tied to Manhattan. The merged variant treats the 5 boroughs
# as a single unit (synthetic GEOID "36000") whose first high-access school
# is Hunter HS (1869).
#
# Aggregation: counts are summed; population is summed; the births
# denominator is recomputed as merged_population * us_birth_rate_decade so
# the rate definitions stay consistent. Outcomes derived from those (per-pop,
# per-births, share, log1p) are recomputed from the aggregates.
###############################################################################

nyc_boroughs <- c("36005", "36047", "36061", "36081", "36085")
nyc_geoid    <- "36000"

panel_nyc <- panel_stem %>%
  filter(GEOID %in% nyc_boroughs) %>%
  group_by(decade) %>%
  summarise(
    GEOID                = nyc_geoid,
    population           = sum(population, na.rm = TRUE),
    population_source    = "merged_nyc",
    us_births_in_decade  = first(us_births_in_decade),
    us_pop_decade        = first(us_pop_decade),
    us_birth_rate_decade = first(us_birth_rate_decade),
    n_all_wiki           = sum(n_all_wiki, na.rm = TRUE),
    n_inventors          = sum(n_inventors, na.rm = TRUE),
    n_stem               = sum(n_stem, na.rm = TRUE),
    lon_county           = mean(lon_county, na.rm = TRUE),
    lat_county           = mean(lat_county, na.rm = TRUE),
    .groups              = "drop"
  ) %>%
  mutate(
    # If every borough was NA at this decade, sum() returned 0 — restore NA
    # so downstream filters treat the cell as missing.
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
    county_births_estimate_decade = if_else(!is.na(population),
                                            population * us_birth_rate_decade,
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
      !is.na(county_births_estimate_decade) & county_births_estimate_decade > 0,
      1000 * n_all_wiki / county_births_estimate_decade, NA_real_),
    allsci_per_1000_births = if_else(
      !is.na(county_births_estimate_decade) & county_births_estimate_decade > 0,
      1000 * n_inventors / county_births_estimate_decade, NA_real_),
    stem_per_1000_births = if_else(
      !is.na(county_births_estimate_decade) & county_births_estimate_decade > 0,
      1000 * n_stem / county_births_estimate_decade, NA_real_),
    stem_within_notable_share        = if_else(n_inventors > 0,
                                               n_stem / n_inventors, NA_real_),
    stem_within_notable_share_zero   = replace_na(stem_within_notable_share, 0),
    stem_within_notable_share_smooth = if_else(n_inventors > 0,
                                               (n_stem + 0.5) / (n_inventors + 1),
                                               0),
    stem_within_notable_pct        = 100 * stem_within_notable_share,
    stem_within_notable_pct_zero   = 100 * stem_within_notable_share_zero,
    stem_within_notable_pct_smooth = 100 * stem_within_notable_share_smooth,
    stem_over_allwiki_pct = if_else(n_all_wiki > 0,
                                    100 * n_stem / n_all_wiki, NA_real_)
  ) %>%
  select(all_of(panel_stem_cols))

panel_stem_nyc_merged <- bind_rows(
  panel_stem %>% filter(!GEOID %in% nyc_boroughs),
  panel_nyc
) %>%
  arrange(GEOID, decade)

###############################################################################
# 9. Export
###############################################################################

write_csv(panel_allsci,          file.path(DATA_OUTPUT, "us_panel_county_1800.csv"))
write_csv(panel_stem,            file.path(DATA_OUTPUT, "us_panel_county_stem_1800.csv"))
write_csv(panel_stem_nyc_merged, file.path(DATA_OUTPUT, "us_panel_county_stem_1800_nyc_merged.csv"))

cat("\n=== 1800+ county panel complete ===\n")
cat("Standard panel rows:        ", nrow(panel_stem), "\n", sep = "")
cat("NYC-merged panel rows:      ", nrow(panel_stem_nyc_merged), "\n", sep = "")
cat("Decade range:               ", min(panel_stem$decade), " to ",
    max(panel_stem$decade), "\n", sep = "")
cat("Counties with any allsci:   ",
    panel_stem %>% filter(n_inventors > 0) %>% pull(GEOID) %>% n_distinct(),
    "\n", sep = "")
cat("Counties with any STEM:     ",
    panel_stem %>% filter(n_stem > 0) %>% pull(GEOID) %>% n_distinct(),
    "\n", sep = "")

cat("\n=== 1800 cells for known high-access counties (standard panel) ===\n")
panel_stem %>%
  filter(decade == 1800L,
         GEOID %in% c("06075","11001","24005","24510","36005","36047",
                      "36061","36081","36085","39061","42101")) %>%
  select(GEOID, population, population_source,
         n_stem, county_births_estimate_decade) %>%
  print()

cat("\n=== 1800 cell for synthetic NYC (merged panel) ===\n")
panel_stem_nyc_merged %>%
  filter(GEOID == nyc_geoid, decade == 1800L) %>%
  select(GEOID, population, population_source,
         n_stem, county_births_estimate_decade) %>%
  print()

elapsed <- difftime(Sys.time(), initial_time, units = "mins")
cat("Runtime (minutes): ", round(as.numeric(elapsed), 2), "\n", sep = "")
