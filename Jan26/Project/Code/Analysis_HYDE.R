###############################################################################
# Project: Determinants of Talent Production via Scientific Infrastructure 
# Author: Lucas Mattos and Deivis Angeli (GTF)
# Goal: Creating Outputs for Analysis
###############################################################################

# Clean the working environment
rm(list = ls())

# Packages
library("tidyverse")
library("ggplot2")
library("stargazer")
library("fixest")
library("patchwork")
library("ggplotify")
library("modelsummary")
library("did")
library("tidygeocoder")
library("sf")
library("rnaturalearth")
library("fixest")

# Recording Initial Time
initial_time <- Sys.time()

###############################################################################
# Uploading databases
###############################################################################
data <- read_csv("Output/agg_hyde.csv", show_col_types = FALSE)
facilities <- read_delim("Output/scientific_facilities.csv",delim = ";",
                         locale = locale( decimal_mark = ".", grouping_mark = ""),
                         show_col_types = FALSE)

###############################################################################
# Ajusting the data
###############################################################################
# Filling the data with observations for every year in every country
countries_keep <- facilities %>%
 distinct(country) %>%
 pull(country)

data_full <- data %>%
 rename(year = birth) %>%
 #filter(country %in% countries_keep) %>%
 select(-decade) %>%
 group_by(cell_id,lat_cell,lon_cell, country) %>%
 complete(
  year = 1800:2015,
  fill = list(
   n_inventors = 0,
   inventors_per_100k = 0,
   population = 0
  )
 ) %>%
 ungroup()

# Decade dataframe 
data_decade <- data_full %>%
 mutate(decade = floor(year / 10) * 10) %>%
 group_by(cell_id,lat_cell,lon_cell, decade, country) %>%
 summarise(inventors_per_100k  = mean(inventors_per_100k, na.rm = TRUE),
           .groups = "drop") %>%
 rename(year = decade)

###############################################################################
# Now dealing with the facilities database
###############################################################################
# Converting to sf object to measure distance
## Cells (unique, not panel)
cells_sf <- data_full %>%
 select(cell_id, lat_cell, lon_cell) %>%
 distinct() %>%
 st_as_sf(
  coords = c("lon_cell", "lat_cell"),
  crs = 4326
 )

## Facilities
fac_sf <- facilities %>%
 st_as_sf(
  coords = c("lon", "lat"),
  crs = 4326
 )

# Converting to meters
cells_sf <- st_transform(cells_sf, 3857)
fac_sf   <- st_transform(fac_sf, 3857)

# Defining radius
radius_km <- 100
radius_m  <- radius_km * 1000

# Finding distance between facilities and cells
dist_mat <- st_distance(cells_sf, fac_sf)

# Long format
cell_event_year <- as.data.frame(dist_mat) %>%
 mutate(cell_id = cells_sf$cell_id) %>%
 tidyr::pivot_longer( -cell_id, names_to  = "fac_idx", values_to = "dist_m") %>%
 mutate(fac_idx = as.integer(gsub("V", "", fac_idx)))

# Joining with the facilities construction year
cell_event_year <- cell_event_year %>%
 left_join(facilities %>%mutate(fac_idx = row_number()) %>%
   select(fac_idx, year), by = "fac_idx")

# Applying the treatmend of the distance radius
cell_event_year <- cell_event_year %>%
 filter(as.numeric(dist_m) <= radius_m) %>%
 group_by(cell_id) %>%
 summarise(event_year = min(year, na.rm = TRUE),.groups = "drop")

###############################################################################
# Creating the panels
###############################################################################
# Yearly panel
data_full_es <- data_full %>%
 left_join(cell_event_year, by = "cell_id") %>%
 mutate(event_year = if_else(is.na(event_year), 0L, event_year))

# Decade panel
cell_event_decade <- cell_event_year %>%
 mutate(event_decade = floor(event_year / 10) * 10) %>%
 select(cell_id, event_decade)

data_full_es_decade <- data_decade %>%
 left_join(cell_event_decade, by = "cell_id") %>%
 mutate(event_year = if_else(is.na(event_decade), 0L, event_decade))


###############################################################################
# Event Studies
###############################################################################
# Creating function
ESgraph<-function(data, type, window, control, title_add = NULL){
 out <- att_gt(yname = "inventors_per_100k",
               tname = "year",
               idname = "cell_id",
               gname = "event_year",
               data = data,
               control_group = control,
               est_method = "dr",
               base_period="universal",
               cores=8)
 
 es <- aggte(out, type = type,
             na.rm= TRUE,
             min_e = -window,
             max_e = window)
 
 plot=ggdid(es) + labs(x="Relative Time", y="Effect",
                       title = if (!is.null(title_add)) {
                        paste0("Average Effect by Lenght of Exposure — ", title_add)
                       } else {
                        "Average Effect by Lenght of Exposure"
                       }
 )
 print(plot)
}

# All Sample
decade_200 <-ESgraph(data=data_full_es_decade, type = "dynamic", window = 80,
                     control= "notyettreated",
                     title_add = paste0("All Sample ", radius_km, "km"))


# Specific By country
## US
data_us <- data_full_es_decade %>%
 filter(country == "United States of America")

plot_us_200 <- ESgraph(data=data_us, type = "dynamic", window = 80,
                        control= "notyettreated",
                        title_add = paste0("US ", radius_km, "km"))


# USSR
data_ussr <- data_full_es_decade %>%
 filter(country %in% c("Russia", "Armenia", "Georgia", "Ukraine", "Kazakhstan"))

plot_ussr_200 <- ESgraph(data=data_ussr, type = "dynamic", window = 80,
                       control= "notyettreated",
                       title_add = paste0("USSR ", radius_km, "km"))


# UK
data_west_eur<- data_full_es_decade %>%
 filter(country %in% c("United Kingdom", "Netherlands", "Italy", "Germany",
                       "Spain", "Portugal", "Belgium", "France", "Austria",
                       "Switzerland"))

plot_west_100 <- ESgraph(data=data_west_eur, type = "dynamic", window = 80,
                       control= "notyettreated",
                       title_add = paste0("Western Europe ", radius_km, "km"))

###############################################################################
# Saving
###############################################################################
ggsave(filename = "Results/ES_all_100.png",plot= decade_100, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "Results/ES_us_100.png",plot= plot_us_100, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "Results/ES_ussr_100.png",plot= plot_ussr_100, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "Results/ES_westeur_100.png",plot= plot_west_100, width = 8,
       height = 6, dpi = 300)

#
ggsave(filename = "Results/ES_all_200.png",plot= decade_200, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "Results/ES_us_200.png",plot= plot_us_200, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "Results/ES_ussr_200.png",plot= plot_ussr_200, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "Results/ES_westeur_200.png",plot= plot_west_200, width = 8,
       height = 6, dpi = 300)
