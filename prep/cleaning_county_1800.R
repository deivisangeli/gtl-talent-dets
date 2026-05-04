###############################################################################
# Project: Determinants of Talent Production
# Goal: Build a county-decade panel from 1800 onward for all-science and STEM
#
# Uses:
#   - raw cross-verified database for birth coordinates and occupations
#   - cached 2020 county shapefile from the local tigris cache
#   - NHGIS county population for 1850-2000 where available
#
# Outputs:
#   output/us_panel_county_1800.csv
#   output/us_panel_county_stem_1800.csv
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

###############################################################################
# Paths
###############################################################################

raw_path <- "input/cross-verified-database.csv"
pop_path <- "input/nhgis0001_ts_nominal_county.csv"
county_shp <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "tigris", "tigris", "Cache", "cb_2020_us_county_20m.shp"
)

if (!file.exists(county_shp)) {
  stop("Cached county shapefile not found at: ", county_shp)
}

###############################################################################
# 1. Load and classify the raw data using only required columns
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

data_clean <- raw_data %>%
  drop_na(birth, bplo1, bpla1) %>%
  filter(level1_main_occ == "Discovery/Science",
         birth >= 1800, birth <= 2000) %>%
  mutate(decade = floor(birth / 10) * 10) %>%
  add_stem_dummy() %>%
  select(wikidata_code, birth, death, bplo1, bpla1,
         citizenship_1_b, decade, level3_occ, stem)

cat("Discovery/Science births kept:", nrow(data_clean), "\n")
cat("STEM share in individual data:", round(mean(data_clean$stem, na.rm = TRUE), 3), "\n")

###############################################################################
# 2. County geometry from local cache
###############################################################################

counties_sf <- st_read(county_shp, quiet = TRUE) %>%
  st_transform(5070) %>%
  select(GEOID, NAME, STATEFP, COUNTYFP, geometry) %>%
  filter(as.integer(STATEFP) <= 56)

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

cat("Individuals assigned to US counties:", nrow(inventors_county), "\n")

###############################################################################
# 4. Aggregate all-science and STEM counts by county x decade
###############################################################################

allsci_agg <- inventors_county %>%
  count(GEOID, decade, name = "n_inventors")

stem_agg <- inventors_county %>%
  filter(stem == 1) %>%
  count(GEOID, decade, name = "n_stem")

###############################################################################
# 5. Historical county population (1850-2000 only)
###############################################################################

pop_raw <- read_csv(pop_path, show_col_types = FALSE)

pop_long <- pop_raw %>%
  select(GISJOIN, STATE, STATEFP, COUNTY, COUNTYFP, starts_with("A00AA")) %>%
  pivot_longer(
    cols = starts_with("A00AA"),
    names_to = "decade",
    values_to = "population"
  ) %>%
  mutate(
    decade = as.integer(str_extract(decade, "[0-9]{4}")),
    STATEFP = str_pad(STATEFP, 2, pad = "0"),
    COUNTYFP = str_pad(COUNTYFP, 3, pad = "0"),
    GEOID = paste0(STATEFP, COUNTYFP)
  ) %>%
  filter(decade >= 1850, decade <= 2000) %>%
  select(GEOID, decade, population)

###############################################################################
# 6. Build the 1800-2000 balanced panel
###############################################################################

panel_skeleton <- expand_grid(
  GEOID = counties_sf$GEOID,
  decade = seq(1800, 2000, by = 10)
)

panel <- panel_skeleton %>%
  left_join(pop_long, by = c("GEOID", "decade")) %>%
  left_join(allsci_agg, by = c("GEOID", "decade")) %>%
  left_join(stem_agg, by = c("GEOID", "decade")) %>%
  mutate(
    n_inventors = replace_na(n_inventors, 0L),
    n_stem = replace_na(n_stem, 0L),
    any_allsci = as.integer(n_inventors > 0),
    any_stem = as.integer(n_stem > 0),
    inv_per_100k = if_else(!is.na(population) & population > 0,
                           1e5 * n_inventors / population,
                           NA_real_),
    stem_per_100k = if_else(!is.na(population) & population > 0,
                            1e5 * n_stem / population,
                            NA_real_),
    # Share of notable Wikipedia people in the county-decade who are
    # classified as STEM. This is NOT a share of births.
    stem_within_notable_share        = if_else(n_inventors > 0, n_stem / n_inventors, NA_real_),
    stem_within_notable_share_zero   = replace_na(stem_within_notable_share, 0),
    stem_within_notable_share_smooth = if_else(n_inventors > 0,
                                               (n_stem + 0.5) / (n_inventors + 1),
                                               0),
    stem_within_notable_pct        = 100 * stem_within_notable_share,
    stem_within_notable_pct_zero   = 100 * stem_within_notable_share_zero,
    stem_within_notable_pct_smooth = 100 * stem_within_notable_share_smooth,
    any_stem_pct = 100 * any_stem,
    log1p_n_stem = log1p(n_stem),
    log1p_n_inventors = log1p(n_inventors)
  ) %>%
  left_join(county_centroids, by = "GEOID") %>%
  arrange(GEOID, decade)

panel_allsci <- panel %>%
  select(GEOID, decade, population, n_inventors, any_allsci,
         inv_per_100k, log1p_n_inventors, lon_county, lat_county)

panel_stem <- panel %>%
  select(
    GEOID, decade, population, n_inventors, n_stem, any_allsci, any_stem,
    inv_per_100k, stem_per_100k,
    stem_within_notable_share, stem_within_notable_share_zero,
    stem_within_notable_share_smooth,
    stem_within_notable_pct, stem_within_notable_pct_zero,
    stem_within_notable_pct_smooth,
    any_stem_pct, log1p_n_inventors, log1p_n_stem,
    lon_county, lat_county
  )

###############################################################################
# 7. Export
###############################################################################

write_csv(panel_allsci, "output/us_panel_county_1800.csv")
write_csv(panel_stem, "output/us_panel_county_stem_1800.csv")

cat("\n=== 1800+ county panel complete ===\n")
cat("Rows:", nrow(panel_stem), "\n")
cat("Decade range:", min(panel_stem$decade), "to", max(panel_stem$decade), "\n")
cat("Counties with any all-science births:",
    panel_stem %>% filter(n_inventors > 0) %>% pull(GEOID) %>% n_distinct(), "\n")
cat("Counties with any STEM births:",
    panel_stem %>% filter(n_stem > 0) %>% pull(GEOID) %>% n_distinct(), "\n")
cat("Mean STEM-within-notable share where n_inventors > 0:",
    round(mean(panel_stem$stem_within_notable_share, na.rm = TRUE), 3), "\n")

elapsed <- difftime(Sys.time(), initial_time, units = "mins")
cat("Runtime (minutes):", round(as.numeric(elapsed), 2), "\n")
