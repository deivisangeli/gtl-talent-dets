###############################################################################
# Project: GTL Talent Determinants
# Goal: Aggregate HYDE cropland and grazeland rasters to US counties
###############################################################################

rm(list = ls())

library("tidyverse")
library("sf")
library("terra")
library("tigris")

source("raw_paths.R")

###############################################################################
# Paths and parameters
###############################################################################

det_dir <- require_det_dir()
hyde_landuse_dir <- file.path(det_dir, "input", "hyde")

landuse_specs <- tribble(
 ~landuse, ~directory, ~file_prefix, ~output_var,
 "cropland", "cropland", "cropland", "cropland_km2",
 "grazeland", "grazeland", "grazeland", "grazeland_km2"
)

years <- seq(1800, 1930, by = 10)

###############################################################################
# County polygons
###############################################################################

tigris_cache_path <- tigris_cache_dir()
options(tigris_use_cache = TRUE, tigris_cache_dir = tigris_cache_path)

counties_sf <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 filter(as.integer(STATEFP) <= 56) %>%
 select(GEOID, NAME, STATEFP, COUNTYFP, geometry)

###############################################################################
# Helpers
###############################################################################

landuse_file_path <- function(directory, file_prefix, year) {
 file.path(
  hyde_landuse_dir,
  directory,
  paste0(file_prefix, "_", year, "AD.asc")
 )
}

aggregate_landuse_year <- function(spec, year, counties_vect) {
 raster_path <- landuse_file_path(spec$directory, spec$file_prefix, year)

 if (!file.exists(raster_path)) {
  stop("Missing HYDE land-use raster: ", raster_path)
 }

 message("Processing ", spec$landuse, " ", year)

 land_pct <- rast(raster_path)
 land_pct <- crop(land_pct, county_raster)

 cell_area_km2 <- terra::cellSize(land_pct, unit = "km")
 land_area_km2 <- (land_pct / 100) * cell_area_km2
 names(land_area_km2) <- spec$output_var

 zonal_sum <- terra::zonal(
  land_area_km2,
  county_raster,
  fun = "sum",
  na.rm = TRUE,
  as.raster = FALSE
 )

 zonal_sum %>%
  as_tibble() %>%
  rename(county_index = county_index) %>%
  left_join(county_lookup, by = "county_index") %>%
  transmute(
  GEOID,
  year = year,
  !!spec$output_var := .data[[spec$output_var]]
  )
}

###############################################################################
# Aggregate rasters to counties
###############################################################################

r_ref <- rast(landuse_file_path("cropland", "cropland", years[1]))
counties_vect <- counties_sf %>%
 st_transform(crs(r_ref)) %>%
 mutate(county_index = row_number()) %>%
 vect()

county_lookup <- tibble(
 county_index = counties_vect$county_index,
 GEOID = counties_vect$GEOID
)

# Rasterize counties onto the HYDE grid once, assigning each HYDE cell to the
# county containing the cell center. This matches the repo's existing HYDE
# cell-index workflow and is fast enough for all decades.
county_template <- crop(r_ref, counties_vect)
county_raster <- terra::rasterize(
 counties_vect,
 county_template,
 field = "county_index"
)
names(county_raster) <- "county_index"

landuse_panels <- vector("list", nrow(landuse_specs))

for (i in seq_len(nrow(landuse_specs))) {
 spec <- landuse_specs[i, ]

 landuse_panels[[i]] <- map_dfr(
  years,
  \(year) aggregate_landuse_year(spec, year, counties_vect)
 )
}

county_hyde_landuse <- reduce(
 landuse_panels,
 full_join,
 by = c("GEOID", "year")
) %>%
 arrange(GEOID, year)

###############################################################################
# Validation and export
###############################################################################

duplicate_keys <- county_hyde_landuse %>%
 count(GEOID, year, name = "n") %>%
 filter(n > 1)

if (nrow(duplicate_keys) > 0) {
 stop("HYDE land-use panel has duplicate GEOID-year keys.")
}

expected_years <- years
observed_years <- sort(unique(county_hyde_landuse$year))
if (!identical(observed_years, expected_years)) {
 stop("Unexpected HYDE land-use years in output.")
}

write_csv(
 county_hyde_landuse,
 output_file_path("county_hyde_landuse.csv"),
 na = ""
)

message(
 "Saved county HYDE land-use panel: ",
 output_file_path("county_hyde_landuse.csv")
)
