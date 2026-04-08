###############################################################################
# Project: Determinants of Inventors
# Author: Lucas Mattos
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

# Recording Initial Time
initial_time <- Sys.time()

# Parameters
options(timeout = 600)

###############################################################################
# Importing the Database
###############################################################################
# Wikipedia
## Getting the URL for the SciencePo website with the database
url_wikipedia <- "https://data.sciencespo.fr/api/access/datafile/4432?format=original"
## Downloading a tempfile for the compressed folder
temp_file <- tempfile(fileext = ".gz")
download.file(url_wikipedia, temp_file, mode = "wb")
## Now reading the data, detecting the CSV format from the zip compression
raw_data <- fread(temp_file)


# Population from Hyde (already treated)
hyde_dir <- "input/hyde_pop_asc"
hyde_files <- list.files(hyde_dir,pattern = "^popc_.*\\.asc$",full.names = TRUE)
## Function to exctract year (decade) from the file
get_year <- function(x) as.integer(gsub(".*([0-9]{4})AD.*", "\\1", x))
## Final Dataframe
hyde_index <- tibble(file = hyde_files,decade = get_year(hyde_files))

###############################################################################
# Cleaning the Database
###############################################################################
# We are interested in the year of birth of individuals, so if this is not in the database
# I'll just drop the observation. The same for bplo1 and bpla1
data_clean <- raw_data %>%
 drop_na(birth, bplo1, bpla1)

# Selecting the variables I think will be useful and filtering for Discovery/Science
# Let's check the levels
#unique(data_clean$level1_main_occ)
#unique(data_clean$level2_main_occ)
#unique(data_clean$level3_main_occ)

# Filtering for birth year and occupation and selecting variables we will use
## I used 1800 because is the minimum we have in the births database
data_clean <- data_clean %>%
 filter(level1_main_occ == "Discovery/Science") %>%
 filter(birth>= 1800 & birth <=2000) %>%
 mutate(decade = floor(birth / 10) * 10) %>%
 select(wikidata_code,birth, death,bplo1, bpla1, citizenship_1_b, decade)

###############################################################################
# Adding geography from the database
###############################################################################
# Getting a vectorized spatial object
points_vect <- vect(data_clean, geom = c("bplo1", "bpla1"), crs = "EPSG:4326" )

# Getting the reference of the unit of Hyde to match with coordinates
r_ref <- rast(hyde_index$file[1])
points_vect$cell_id <- terra::cellFromXY(r_ref,terra::crds(points_vect))

# Now creating our full vector
points_vect$population <- NA_real_

for (d in sort(unique(points_vect$decade))) {
 message("Processing decade: ", d)
 r <- rast(hyde_index$file[hyde_index$decade == d])
 idx <- which(points_vect$decade == d)
 # Matching
 points_vect$population[idx] <- terra::extract(r,points_vect[idx], ID = FALSE)[, 1]
}

###############################################################################
# Joining with the dataset of births
###############################################################################
data_final <- as.data.frame(points_vect) %>%
 left_join(data_clean %>% select("wikidata_code","bplo1", "bpla1"), by="wikidata_code") %>%
 filter(population != 0)

#Now getting the country and coordinates from the Hyde data
cell_centers <- as.data.frame(terra::xyFromCell(r_ref, unique(data_final$cell_id)))

# Getting the columns
colnames(cell_centers) <- c("lon_cell", "lat_cell")
cell_centers$cell_id <- unique(data_final$cell_id)

# sf object
cells_sf <- st_as_sf(cell_centers, coords = c("lon_cell", "lat_cell"), crs = 4326,
 remove = FALSE)

world <- ne_countries(returnclass = "sf")

sf::sf_use_s2(FALSE)
cells_sf <- st_join(
 cells_sf,
 world[, c("iso_a3", "name")],
 join = st_intersects,
 left = TRUE
)
sf::sf_use_s2(TRUE)

# Joining the data to the previous data frame
data_final_loc <- data_final %>%
 left_join(st_drop_geometry(cells_sf) %>%
   rename(country = name, iso3 = iso_a3),
  by = "cell_id")

# Aggregation
agg_cell_birth <- data_final_loc %>%
 count(cell_id, birth, decade, country,lat_cell, lon_cell, population,name = "n_inventors") %>%
 drop_na() %>%
 mutate(inventors_per_100k = 1e5 * n_inventors / population)

###############################################################################
# Exporting
###############################################################################
write.csv(agg_cell_birth, "output/agg_hyde.csv", row.names = FALSE)
