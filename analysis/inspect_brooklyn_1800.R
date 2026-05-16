suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
})
source("../paths.R")

# Brooklyn (Kings County NY) 2020 boundary
counties_shp <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "tigris", "tigris", "Cache", "cb_2020_us_county_20m.shp"
)
counties_sf <- st_read(counties_shp, quiet=TRUE) %>%
  filter(GEOID == "36047") %>%
  st_transform(4326)

cv <- read_csv(file.path(DATA_INPUT, "cross-verified-database.csv"), show_col_types=FALSE)

cat("Total rows in CV: ", nrow(cv), "\n")

# Get people with birth coords
cat("Sample citizenship values: ", paste(head(unique(cv$citizenship_1_b), 20), collapse=" | "), "\n")
cat("Sample early years bpla1/bplo1 non-NA: ",
    cv %>% filter(birth >= 1800, birth <= 1819, !is.na(bpla1)) %>% nrow(), "\n\n")

stem <- read_csv(file.path(DATA_OUTPUT, "crossverified_with_stem.csv"), show_col_types=FALSE) %>%
  select(wikidata_code, stem)

cv_brooklyn <- cv %>%
  filter(!is.na(bpla1), !is.na(bplo1),
         birth >= 1800, birth <= 1819) %>%
  left_join(stem, by = "wikidata_code")

cat("US births 1800-1819 with coords: ", nrow(cv_brooklyn), "\n")

# Spatial join to Kings County
pts <- cv_brooklyn %>%
  st_as_sf(coords = c("bplo1","bpla1"), crs = 4326)
in_kings <- st_join(pts, counties_sf, join=st_within) %>%
  filter(!is.na(GEOID))

cat("Born in Kings County 1800-1819 (all): ", nrow(in_kings), "\n")
cat("Born in Kings County 1800-1819 with stem flag set: ",
    sum(in_kings$stem == 1, na.rm=TRUE), "\n\n")

cat("=== STEM-coded people born in Kings County by decade ===\n")
in_kings %>%
  st_drop_geometry() %>%
  filter(stem == 1) %>%
  mutate(decade = (birth %/% 10) * 10) %>%
  arrange(decade, birth) %>%
  select(name, birth, decade, level1_main_occ, level2_main_occ, level3_main_occ) %>%
  print(n = 30)
