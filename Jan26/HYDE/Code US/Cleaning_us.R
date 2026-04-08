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
library("FNN")

# Recording Initial Time
initial_time <- Sys.time()

# Parameters
options(timeout = 600)

###############################################################################
# Load Databases
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
hyde_dir <- "C:/Users/lucas/OneDrive/Documentos/Mestrado EESP/GTF/Project/Input/hyde_pop_asc"
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
# Filtering for birth year and occupation and selecting variables we will use
data_clean <- raw_data %>%
 drop_na(birth, bplo1, bpla1) %>% 
 filter(level1_main_occ == "Discovery/Science") %>%
 filter(birth>= 1800 & birth <=2000) %>%
 mutate(decade = floor(birth / 10) * 10) %>%
 select(wikidata_code,birth, death,bplo1, bpla1, citizenship_1_b, decade)

###############################################################################
# Restrict HYDE grid to US (FAST raster mask)
###############################################################################

# Reference raster defines the fixed HYDE grid geometry
r_ref <- rast(hyde_index$file[1])

# Load US boundary
us_sf <- ne_countries(scale="medium", returnclass="sf") %>%
 filter(admin=="United States of America")
us <- vect(us_sf)

# Mask raster to US
us_mask <- mask(r_ref, us)

# Extract raster cell indices corresponding to US territory
us_cells <- which(!is.na(values(us_mask)))

#plot(us, add = TRUE, border = "red")

###############################################################################
# Build population panel: cell × decade
###############################################################################

# For each HYDE raster:
# - read population values
# - index directly by US cell IDs (no spatial extraction)
# - store population by cell and decade

pop_list <- vector("list", nrow(hyde_index))

for (i in seq_len(nrow(hyde_index))) {
 
 d <- hyde_index$decade[i]
 message("Processing decade: ", d)
 
 r <- rast(hyde_index$file[i])   
 
 vals <- terra::values(r)[us_cells]
 
 pop_list[[i]] <- tibble(
  cell_id = us_cells,
  population = vals,
  decade = d)}


pop_panel <- bind_rows(pop_list)

###############################################################################
# Assign inventors to HYDE cells
###############################################################################

# Convert inventor birth coordinates to spatial points
points_vect <- vect(data_clean, geom = c("bplo1", "bpla1"), crs = "EPSG:4326")

# Map each inventor to HYDE raster cell
points_vect$cell_id <- terra::cellFromXY(r_ref, terra::crds(points_vect))

# Aggregate inventor counts by cell × decade
inventors_cell <- data_clean %>%
 mutate(cell_id = points_vect$cell_id) %>%
 count(cell_id, decade, name = "n_inventors")

###############################################################################
# Merge population and inventors to create balanced panel
###############################################################################
panel <- pop_panel %>%
 left_join(inventors_cell, by = c("cell_id", "decade")) %>%
 mutate(n_inventors = replace_na(n_inventors, 0)) %>%
 mutate(inv_per_100k = ifelse(population>0,1e5 * n_inventors / population,0))

# Recover cell centroids from raster geometry
xy <- terra::xyFromCell(r_ref, panel$cell_id)
panel$lon_cell <- xy[,1]
panel$lat_cell <- xy[,2]

###############################################################################
# Identify zero-population cells with inventors (problematic observations)
###############################################################################

# Now treating
bad <- panel %>% filter(population == 0 & n_inventors > 0)
good <- panel %>% filter(population > 0)

# For each decade:
# - find nearest populated cell (k=1)
# - move inventor counts accordingly
reassigned <- vector("list", length(unique(bad$decade)))
k <- 1

for (d in unique(bad$decade)) {
 
 message("Fixing decade ", d)
 
 bad_d  <- bad  %>% filter(decade == d)
 good_d <- good %>% filter(decade == d)
 
 if(nrow(bad_d) == 0) next
 
 nn <- FNN::get.knnx(
  data = as.matrix(good_d[,c("lon_cell","lat_cell")]),
  query = as.matrix(bad_d[,c("lon_cell","lat_cell")]),
  k = 1
 )
 
 bad_d$target_cell <- good_d$cell_id[nn$nn.index[,1]]
 bad_d$dist_km <- nn$nn.dist[,1]
 
 reassigned[[k]] <- bad_d
 k <- k + 1
}

reassigned <- bind_rows(reassigned)

# Aggregate moved inventors by destination cell
moves <- reassigned %>%
 group_by(target_cell, decade) %>%
 summarise(moved = sum(n_inventors), .groups="drop")

# Creating the object
panel_fixed <- panel

panel_fixed$n_inventors[panel_fixed$cell_id %in% bad$cell_id &
                         panel_fixed$decade %in% bad$decade] <- 0

panel_fixed <- panel_fixed %>%
 left_join(moves, by = c("cell_id" = "target_cell", "decade")) %>%
 mutate(
  moved = replace_na(moved,0),
  n_inventors = n_inventors + moved
 ) %>%
 select(-moved)

panel_fixed <- panel_fixed %>%
 mutate(inv_per_100k = ifelse(population > 0,
                              1e5 * n_inventors / population,
                              0))


###############################################################################
# Now Plotting
###############################################################################
us_sf <- ne_countries(scale="medium", returnclass="sf") %>%
 filter(admin=="United States of America")

pairs <- reassigned %>%
 left_join(panel %>% select(cell_id, lon_cell, lat_cell),
           by=c("target_cell"="cell_id")) %>%
 rename(lon_new = lon_cell.y, lat_new = lat_cell.y, lon_cell=lon_cell.x,
        lat_cell=lat_cell.x)

plot_diff <- ggplot() +
 geom_sf(data=us_sf, fill="grey95") +
 geom_segment(data=pairs,
              aes(x=lon_cell,y=lat_cell,
                  xend=lon_new,yend=lat_new),
              alpha=.4) +
 geom_point(data=pairs,
            aes(lon_cell,lat_cell),
            color="red",size=1) +
 geom_point(data=pairs,
            aes(lon_new,lat_new),
            color="blue",size=1) +
 coord_sf(xlim=c(-130,-60),ylim=c(24,50)) +
 theme_minimal() +
 labs(title="Red = original zero-pop cells; Blue = reassigned populated cells")

###############################################################################
# Exporting
###############################################################################
write.csv(panel, "Output\\us_panel.csv", row.names = FALSE)
write.csv(panel_fixed, "Output\\us_panel_fixed.csv", row.names = FALSE)

ggsave(filename = "Results/fixing_error.png",plot= plot_diff, width = 8,
       height = 6, dpi = 300)
