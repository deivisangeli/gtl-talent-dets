###############################################################################
# Project: Determinants of Inventors
# Author: GTF
# Goal: Downloading and cleaning the Wikipedia Database
###############################################################################

# Clean the working environment
rm(list = ls())

# Packages
library("tidyverse")
library("ggplot2")
library("tidygeocoder")
library("sf")
library("rnaturalearth")
library("readr")
library("data.table")
library("dataverse")
library("R.utils")
library("geodata")
library("terra")
library("FNN")
library("tigris")

# Recording Initial Time
initial_time <- Sys.time()

# Parameters
options(timeout = 1000)

###############################################################################
# 1. Wikipedia inventors database
###############################################################################
# Downloading the database
url_wikipedia <- "https://data.sciencespo.fr/api/access/datafile/4432?format=original"
temp_file <- tempfile(fileext = ".gz")
download.file(url_wikipedia, temp_file, mode = "wb")
raw_data <- fread(temp_file)

# We are interested in the year of birth of individuals, so if this is not in the database
# I'll just drop the observation. The same for bplo1 and bpla1
# Filtering for birth year and occupation and selecting variables we will use
data_clean <- raw_data %>%
 drop_na(birth, bplo1, bpla1) %>%
 filter(level1_main_occ == "Discovery/Science") %>%
 filter(birth >= 1800 & birth <= 2000) %>%
 mutate(decade = floor(birth / 10) * 10) %>%
 select(wikidata_code, birth, death, bplo1, bpla1, citizenship_1_b, decade)

###############################################################################
# 2. US County shapefiles (for spatial assignment)
###############################################################################
# Download all US counties from TIGER via tigris
# resolution = "20m" is sufficient and faster than "500k"
counties_sf <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 st_transform(4326) %>%
 select(GEOID, NAME, STATEFP, COUNTYFP, geometry) %>%
 filter(as.integer(STATEFP) <= 56)

# County centroids (for distance-based facility assignment later)
county_centroids <- counties_sf %>%
 st_centroid() %>%
 mutate(
  lon_county = st_coordinates(.)[, 1],
  lat_county = st_coordinates(.)[, 2]
 ) %>%
 st_drop_geometry()

###############################################################################
# 3. Historical county population (1850-2000, decennial)
###############################################################################
# Read the population CSV
pop_raw <- read_csv("input/nhgis0001_ts_nominal_county.csv")

# Pivot to long: one row per county x decade
pop_long <- pop_raw %>%
 select(GISJOIN, STATE, STATEFP, COUNTY, COUNTYFP,
        starts_with("A00AA")) %>%
 pivot_longer(
  cols      = starts_with("A00AA"),
  names_to  = "decade",
  values_to = "population"
 ) %>%
 mutate(
  decade    = as.integer(str_extract(decade, "[0-9]{4}")),
  # Pad FIPS codes to match tigris GEOID
  STATEFP   = str_pad(STATEFP,  2, pad = "0"),
  COUNTYFP  = str_pad(COUNTYFP, 3, pad = "0"),
  GEOID     = paste0(STATEFP, COUNTYFP)
 ) %>%
 filter(decade >= 1850 & decade <= 2000)

###############################################################################
# 4. Assign inventors to counties (point-in-polygon)
###############################################################################

inventors_sf <- data_clean %>%
 st_as_sf(coords = c("bplo1", "bpla1"), crs = 4326)

# Each inventor gets the GEOID of the county they were born in
inventors_county <- st_join(inventors_sf, counties_sf["GEOID"],
                            join = st_within) %>%
 st_drop_geometry() %>%
 filter(!is.na(GEOID))   # drops inventors born outside US counties

# Aggregate: n_inventors by county x decade
inventors_agg <- inventors_county %>%
 count(GEOID, decade, name = "n_inventors")

###############################################################################
# 5. Build balanced panel: county x decade
###############################################################################

# All county x decade combinations
panel_skeleton <- expand_grid(
 GEOID  = counties_sf$GEOID,
 decade = seq(1870, 2000, by = 10)
)

# Now creating the panel with the important information
panel <- panel_skeleton %>%
 left_join(pop_long %>% select(GEOID, decade, population),
           by = c("GEOID", "decade")) %>%
 left_join(inventors_agg,
           by = c("GEOID", "decade")) %>%
 mutate(
  n_inventors  = replace_na(n_inventors, 0),
  inv_per_100k = ifelse(!is.na(population) & population > 0,
                        1e5 * n_inventors / population,
                        NA_real_)
 ) %>%
 left_join(county_centroids %>% select(GEOID, lon_county, lat_county),
           by = "GEOID")

###############################################################################
# 8. Map - inventor density by county, aggregate over all decades
###############################################################################

# Aggregate inv_per_100k over time per county (mean across decades)
panel_agg <- panel %>%
 filter(!is.na(inv_per_100k)) %>%
 group_by(GEOID) %>%
 summarise(inv_per_100k_mean = mean(inv_per_100k, na.rm = TRUE),
           .groups = "drop")

# Join to county geometries
map_agg <- counties_sf %>%
 left_join(panel_agg, by = "GEOID")

# Plot
plot_agg <- ggplot() +
 geom_sf(data = map_agg,
         aes(fill = inv_per_100k_mean),
         color = "grey60",
         linewidth = 0.05) +
 scale_fill_viridis_c(
  option   = "plasma",
  trans    = "log1p",
  na.value = "grey85",
  name     = "Inventors\nper 100k"
 ) +
 coord_sf(xlim = c(-125, -65), ylim = c(24, 50), expand = FALSE) +
 theme_void(base_size = 12) +
 theme(
  legend.position  = c(0.92, 0.25),
  legend.key.width = unit(0.4, "cm"),
  plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
  plot.subtitle    = element_text(color = "grey40", size = 10, hjust = 0.5),
  plot.caption     = element_text(color = "grey60", size = 8,  hjust = 1)
 ) +
 labs(
  title    = "Inventor density across US counties, 1850-2000",
  subtitle = "Mean inventors per 100,000 inhabitants across all decades (log scale)",
  caption  = "Sources: Wikipedia (SciencesPo), NHGIS, TIGER/Line"
 )

print(plot_agg)

###############################################################################
# Exporting
###############################################################################
write.csv(panel, "output/us_panel_county.csv", row.names = FALSE)

ggsave("output/county_density_aggregate.png", plot = plot_agg,
       width = 10, height = 6, dpi = 300)
