###############################################################################
# Project: Determinants of Talent Production via Scientific Infrastructure
# Goal: Add STEM dummy to the US county panel
#
# Reads from local cross-verified-database.csv (no download needed).
# Re-uses tigris county shapefiles from cache.
# Outputs:
#   output/us_panel_county_stem.csv  — county x decade panel with n_stem
#                                      merged alongside n_inventors (all sci)
###############################################################################

rm(list = ls())

library("tidyverse")
library("sf")
library("terra")
library("FNN")
library("tigris")

options(tigris_use_cache = TRUE)
initial_time <- Sys.time()

###############################################################################
# 1. Load and classify data
###############################################################################
source("stem_labels.R")

raw_data <- read_csv("input/cross-verified-database.csv", show_col_types = FALSE)

data_clean <- raw_data %>%
  drop_na(birth, bplo1, bpla1) %>%
  filter(level1_main_occ == "Discovery/Science",
         birth >= 1800, birth <= 2000) %>%
  mutate(decade = floor(birth / 10) * 10) %>%
  add_stem_dummy()

cat("Discovery/Science individuals with coords:", nrow(data_clean), "\n")
cat("  STEM:", sum(data_clean$stem), sprintf("(%.1f%%)\n", 100*mean(data_clean$stem)))

###############################################################################
# 2. US county shapefiles (from tigris cache)
###############################################################################
counties_sf <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_transform(4326) %>%
  select(GEOID, NAME, STATEFP, COUNTYFP, geometry) %>%
  filter(as.integer(STATEFP) <= 56)

###############################################################################
# 3. Assign all individuals to counties (point-in-polygon)
###############################################################################
inventors_sf <- data_clean %>%
  st_as_sf(coords = c("bplo1", "bpla1"), crs = 4326)

inventors_county <- st_join(inventors_sf, counties_sf["GEOID"], join = st_within) %>%
  st_drop_geometry() %>%
  filter(!is.na(GEOID))

cat("Individuals assigned to US counties:", nrow(inventors_county), "\n")

###############################################################################
# 4. Aggregate: all Discovery/Science and STEM separately
###############################################################################
allsci_agg <- inventors_county %>%
  count(GEOID, decade, name = "n_inventors")

stem_agg <- inventors_county %>%
  filter(stem == 1) %>%
  count(GEOID, decade, name = "n_stem")

###############################################################################
# 5. Load existing panel and merge stem counts onto it
###############################################################################
panel_base <- read_csv("output/us_panel_county.csv", show_col_types = FALSE)

panel_stem <- panel_base %>%
  left_join(stem_agg, by = c("GEOID", "decade")) %>%
  mutate(
    n_stem       = replace_na(n_stem, 0),
    stem_per_100k = ifelse(!is.na(population) & population > 0,
                           1e5 * n_stem / population,
                           NA_real_),
    stem_share   = ifelse(n_inventors > 0, n_stem / n_inventors, NA_real_)
  )

###############################################################################
# 6. Export
###############################################################################
write.csv(panel_stem, "output/us_panel_county_stem.csv", row.names = FALSE)

cat("\n=== County stem panel complete ===\n")
cat("Rows:", nrow(panel_stem), "\n")
cat("Counties with any STEM inventor:",
    panel_stem %>% filter(n_stem > 0) %>% pull(GEOID) %>% n_distinct(), "\n")
cat("Mean STEM share (where n_inventors > 0):",
    round(mean(panel_stem$stem_share, na.rm=TRUE), 3), "\n")

final_time <- Sys.time() - initial_time
print(paste("Ran in", round(final_time, 1), "seconds."))
