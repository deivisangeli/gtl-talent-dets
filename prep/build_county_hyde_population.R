###############################################################################
# Build a county-decade panel of HYDE population for the US, 1800-2000.
#
# Source: HYDE 3.2/3.3 popc_<decade>AD.asc rasters in the GTL Dropbox.
# Counties: 2020 TIGER cb_2020_us_county_20m shapefile (50 states + DC).
#
# Approach: zonal sum of raster pixel values within each county polygon.
# popc rasters store absolute population counts per pixel, so zonal sum
# is the total population of the county at that decade.
#
# Output: prep/output/county_hyde_population.csv
#   columns: GEOID, decade, hyde_population
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("tidyverse")
  library("sf")
  library("terra")
  library("exactextractr")
})

initial_time <- Sys.time()

source("../paths.R")

hyde_dir <- file.path(TALENT_DETS_DATA_DIR, "input", "hyde_pop_asc")

county_shp <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "tigris", "tigris", "Cache", "cb_2020_us_county_20m.shp"
)

stopifnot(dir.exists(hyde_dir))
stopifnot(file.exists(county_shp))

decades <- seq(1800L, 2000L, by = 10L)

###############################################################################
# Load county polygons in WGS84 to match HYDE
###############################################################################

counties_sf <- st_read(county_shp, quiet = TRUE) %>%
  filter(as.integer(STATEFP) <= 56) %>%
  select(GEOID, geometry) %>%
  st_transform(4326)

cat("Counties loaded: ", nrow(counties_sf), "\n", sep = "")

###############################################################################
# Loop over decades, extract zonal population sums via exactextractr
# (much faster than terra::extract for this raster size).
###############################################################################

extract_one <- function(d) {
  fp <- file.path(hyde_dir, sprintf("popc_%dAD.asc", d))
  if (!file.exists(fp)) {
    warning("Missing raster: ", fp)
    return(NULL)
  }
  r <- rast(fp)
  crs(r) <- "EPSG:4326"

  zsum <- exact_extract(r, counties_sf, fun = "sum", progress = FALSE)
  tibble(
    GEOID  = counties_sf$GEOID,
    decade = d,
    hyde_population = as.numeric(zsum)
  )
}

panel_list <- list()
for (d in decades) {
  cat("  decade ", d, "\n", sep = "")
  panel_list[[as.character(d)]] <- extract_one(d)
}
panel_hyde <- bind_rows(panel_list)

cat("\nRows: ", nrow(panel_hyde), "\n", sep = "")
cat("Distinct counties: ", n_distinct(panel_hyde$GEOID), "\n", sep = "")
cat("Decades: ", paste(sort(unique(panel_hyde$decade)), collapse = ", "), "\n", sep = "")

###############################################################################
# Diagnostics: NA / zero counts per decade
###############################################################################

diag_decade <- panel_hyde %>%
  group_by(decade) %>%
  summarise(
    n_counties     = n(),
    n_NA           = sum(is.na(hyde_population)),
    n_zero         = sum(hyde_population == 0, na.rm = TRUE),
    n_positive     = sum(hyde_population > 0, na.rm = TRUE),
    sum_population = sum(hyde_population, na.rm = TRUE),
    .groups = "drop"
  )
cat("\n=== HYDE population diagnostics by decade ===\n")
print(diag_decade)

write_csv(diag_decade,
          file.path(DATA_OUTPUT, "county_hyde_population_diagnostics.csv"))

###############################################################################
# Cross-check against existing NHGIS-derived us_panel_county_stem_1800.csv
###############################################################################

nhgis_panel <- read_csv(file.path(DATA_OUTPUT, "us_panel_county_stem_1800.csv"),
                        show_col_types = FALSE) %>%
  mutate(
    GEOID  = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade = as.integer(decade)
  ) %>%
  select(GEOID, decade, population_nhgis = population)

merged <- panel_hyde %>%
  mutate(GEOID = str_pad(GEOID, width = 5, side = "left", pad = "0")) %>%
  left_join(nhgis_panel, by = c("GEOID", "decade"))

compare_decade <- merged %>%
  filter(!is.na(population_nhgis), hyde_population > 0) %>%
  group_by(decade) %>%
  summarise(
    n             = n(),
    cor_spearman  = cor(population_nhgis, hyde_population, method = "spearman"),
    ratio_mean    = mean(population_nhgis / hyde_population, na.rm = TRUE),
    ratio_median  = median(population_nhgis / hyde_population, na.rm = TRUE),
    .groups       = "drop"
  )
cat("\n=== HYDE vs NHGIS comparison (overlapping decades) ===\n")
print(compare_decade)

write_csv(compare_decade,
          file.path(DATA_OUTPUT, "county_hyde_population_vs_nhgis.csv"))

###############################################################################
# Export
###############################################################################

write_csv(panel_hyde, file.path(DATA_OUTPUT, "county_hyde_population.csv"))

cat("\nDone in ", round(as.numeric(Sys.time() - initial_time, units = "mins"), 2),
    " minutes. Output: prep/output/county_hyde_population.csv\n", sep = "")
