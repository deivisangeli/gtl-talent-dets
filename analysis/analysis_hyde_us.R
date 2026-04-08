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
library("rnaturalearthdata")

# Recording Initial Time
initial_time <- Sys.time()

###############################################################################
# Uploading databases
###############################################################################
#data <- read_csv("../prep/output/us_panel.csv", show_col_types = FALSE)
data_full <- read_csv("../prep/output/us_panel_fixed.csv", show_col_types = FALSE)
facilities <- read_delim("../prep/output/facilities_us.csv", delim = ";",
                         locale = locale( decimal_mark = ".", grouping_mark = ""),
                         show_col_types = FALSE)

facilities_alt <- read_delim("../prep/output/facilities_us_alt.csv", delim = ";",
                         locale = locale( decimal_mark = ".", grouping_mark = ""),
                         show_col_types = FALSE)
###############################################################################
# Adjusting facilities in the decade format
###############################################################################

facilities <- facilities %>%
 filter(!is.na(year)) %>%
 mutate(decade_std = floor(year / 10) * 10,
  decade_shift = ifelse(year %% 10 >= 7, floor(year / 10) * 10 + 10,
                        floor(year / 10) * 10))

facilities_alt <- facilities_alt %>%
 filter(!is.na(year)) %>%
 mutate(decade_std = floor(year / 10) * 10,
        decade_shift = ifelse(year %% 10 >= 7, floor(year / 10) * 10 + 10,
                              floor(year / 10) * 10))
###############################################################################
# Creating the spatial objects
###############################################################################

# for HYDE cells
cells_sf <- data_full %>%
 select(cell_id, lat_cell, lon_cell) %>%
 distinct() %>%
 st_as_sf(coords = c("lon_cell","lat_cell"), crs = 4326) %>%
 st_transform(3857)

# for facilities
fac_sf <- facilities %>%
 filter(!is.na(year)) %>%
 st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
 st_transform(3857)

fac_sf_alt <- facilities_alt %>%
 filter(!is.na(year)) %>%
 st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
 st_transform(3857)

# Distance Matrix
dist_mat <- st_distance(cells_sf, fac_sf)
dist_mat_alt <- st_distance(cells_sf, fac_sf_alt)

# Creating dataframe
cell_event_decade <- as.data.frame(dist_mat) %>%
 mutate(cell_id = cells_sf$cell_id) %>%
 pivot_longer( -cell_id, names_to = "fac_idx",values_to = "dist_m") %>%
 mutate(fac_idx = as.integer(gsub("V","",fac_idx)))

cell_event_decade_alt <- as.data.frame(dist_mat_alt) %>%
 mutate(cell_id = cells_sf$cell_id) %>%
 pivot_longer( -cell_id, names_to = "fac_idx",values_to = "dist_m") %>%
 mutate(fac_idx = as.integer(gsub("V","",fac_idx)))

# Joining facilities
cell_event_decade <- cell_event_decade %>%
 left_join(facilities %>%mutate(fac_idx = row_number()) %>%
   select(fac_idx, decade_std, decade_shift),by = "fac_idx")

cell_event_decade_alt <- cell_event_decade_alt %>%
 left_join(facilities_alt %>%mutate(fac_idx = row_number()) %>%
            select(fac_idx, decade_std, decade_shift),by = "fac_idx")

###############################################################################
# Apply radius + earliest decade
###############################################################################

radius_m <- 50000

cell_event_decade <- cell_event_decade %>%
 filter(as.numeric(dist_m) <= radius_m) %>%
 group_by(cell_id) %>%
 summarise(g_std   = min(decade_std), g_shift = min(decade_shift),.groups = "drop")

cell_event_decade_alt <- cell_event_decade_alt %>%
 filter(as.numeric(dist_m) <= radius_m) %>%
 group_by(cell_id) %>%
 summarise(g_std   = min(decade_std), g_shift = min(decade_shift),.groups = "drop")

# Panel
data_full_es_decade <- data_full %>%
 left_join(cell_event_decade, by = "cell_id") %>%
 mutate(g_std   = ifelse(is.na(g_std), 0, g_std),
  g_shift = ifelse(is.na(g_shift), 0, g_shift))

data_full_es_decade_alt <- data_full %>%
 left_join(cell_event_decade_alt, by = "cell_id") %>%
 mutate(g_std   = ifelse(is.na(g_std), 0, g_std),
        g_shift = ifelse(is.na(g_shift), 0, g_shift))
###############################################################################
# Event Studies
###############################################################################
# Creating function
ESgraph<-function(data, type, window, control, treat_var, title_add = NULL){
 out <- att_gt(yname = "inv_per_100k",
               tname = "decade",
               idname = "cell_id",
               gname = treat_var,
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
                        paste0("Average Effect by Lenght of Exposure - ", title_add)}
                       else {"Average Effect by Lenght of Exposure"} ) +
  coord_cartesian(ylim = c(-20, 20))
 print(plot)
}

decade_50_std <-ESgraph(data=data_full_es_decade, type = "dynamic", window = 100,
                     control= "notyettreated", treat_var = "g_std",
                     title_add = paste0(radius_m/1000, "km - Standard Decade"))

decade_50_alt <-ESgraph(data=data_full_es_decade, type = "dynamic", window = 100,
                    control= "notyettreated", treat_var = "g_shift",
                    title_add = paste0(radius_m/1000, "km - Alternative Decade"))

es_alt_facilities_std <-ESgraph(data=data_full_es_decade_alt, type = "dynamic", window = 100,
                                control= "notyettreated", treat_var = "g_std",
                                title_add = paste0(radius_m/1000, "km - Standard Decade - Selected Facilities"))

es_alt_facilities_alt <-ESgraph(data=data_full_es_decade_alt, type = "dynamic", window = 100,
                        control= "notyettreated", treat_var = "g_shift",
                        title_add = paste0(radius_m/1000, "km - Alternative Decade - Selected Facilities"))

###############################################################################
# Means Plot
###############################################################################
plot_df <- data_full_es_decade %>%
 mutate(cohort = ifelse(g_std == 0, "Never treated", as.character(g_std)))

mean_df <- plot_df %>%
 group_by(decade, cohort) %>%
 summarise(mean_inv = mean(inv_per_100k, na.rm = TRUE),.groups = "drop")

mean_df %>%
 filter(decade == 1850 & cohort ==1930)

plot_means_full <- ggplot(mean_df,
       aes(x = decade, y = mean_inv, group = cohort,
           color = cohort)) +
 geom_line(linewidth = 0.9, alpha = 0.8) +
 geom_point(size = 1.5) +
 labs( y = "Inventors per 100k",
  title = "Average Inventors by Treatment Cohort",
  subtitle = paste0(radius_m/1000, "km radius - cohorts defined by first exposure decade"),
  color = "Treatment cohort") +
 theme_minimal(base_size = 13)

plot_means_focus <- ggplot(mean_df,
                          aes(x = decade, y = mean_inv, group = cohort,
                              color = cohort)) +
 geom_line(linewidth = 0.9, alpha = 0.8) +
 geom_point(size = 1.5) +
 labs( y = "Inventors per 100k",
       title = "Average Inventors by Treatment Cohort",
       subtitle = paste0(radius_m/1000, "km radius - cohorts defined by first exposure decade"),
       color = "Treatment cohort") +
 theme_minimal(base_size = 13)  +
 coord_cartesian(ylim = c(0, 50))

# Temp
fac_1930 <- fac_sf %>%
 filter(decade_std == 1990)

cells_sf <- data_full_es_decade %>%
 filter(g_std == 1990)%>%
 select(cell_id, decade, inv_per_100k, lon_cell, lat_cell) %>%
 st_as_sf(coords = c("lon_cell", "lat_cell"),
          crs = 4326,
          remove = FALSE)

cells_m <- st_transform(cells_sf, 5070)   # US Albers
fac_m   <- st_transform(fac_1930, 5070)

fac_buf <- st_buffer(fac_m, 50000)

cell_fac_matches <- st_join(
 cells_m,
 fac_buf,
 join = st_within
)

facility_means <- cell_fac_matches %>%
 st_drop_geometry() %>%
 group_by(facility, decade) %>%   # <-- decade, not decade_std
 summarise(
  mean_inv = mean(inv_per_100k, na.rm = TRUE),
  n_cells = n(),
  .groups = "drop"
 )

facility_means %>% filter(decade ==1910)

ggplot(facility_means,
       aes(x = decade,
           y = mean_inv,
           color = facility)) +
 geom_line(linewidth = 1) +
 geom_point(size = 2) +
 theme_minimal(base_size = 13) +
 labs(
  title = "Inventors per 100k - 1990 Facilities",
  subtitle = "Cells within 50km radius",
  y = "Inventors per 100k",
  color = "Facility"
 )

###############################################################################
# Understanding the spikes
target_facilities <- facilities %>%
 filter(facility %in% c("Jet Propulsion Laboratory (JPL)",
                        "Lawrence Berkeley National Laboratory")) %>%
 mutate(fac_idx = row_number())

# Now using the cell_event_decade dataframe only for the two
cell_fac <- as.data.frame(dist_mat) %>%
 mutate(cell_id = cells_sf$cell_id) %>%
 pivot_longer(-cell_id, names_to = "fac_idx", values_to = "dist_m") %>%
 mutate(fac_idx = as.integer(gsub("V","",fac_idx))) %>%
 left_join(target_facilities, by = "fac_idx") %>%
 filter(!is.na(facility)) %>%
 filter(as.numeric(dist_m) <= radius_m)

# Now doing to the full data
facility_means <- data_full %>%
 left_join(cell_fac %>% select(cell_id, facility), by = "cell_id") %>%
 filter(!is.na(facility)) %>%
 group_by(facility, decade) %>%
 summarise(mean_inv = mean(inv_per_100k, na.rm = TRUE),
           .groups = "drop")

# Plotting
ggplot(facility_means,
       aes(x = decade, y = mean_inv, color = facility)) +
 geom_line(linewidth = 1) +
 geom_point(size = 2) +
 theme_minimal(base_size = 13) +
 labs(
  title = "Mean Inventors per 100k within 50km of Facilities from the 1930 Group",
  #subtitle = "Jet Propulsion Laboratory vs Lawrence Berkeley National Laboratory",
  x = "Decade",
  y = "Inventors per 100k",
  color = "Facility"
 )

###############################################################################
# Map Evidence
###############################################################################
# Converting facilities into an sf object
fac_sf <- facilities %>%
 st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Get US map as sf
us_sf <- ne_countries(country = "United States of America", scale = "medium",
                   returnclass = "sf")

# Now will create the HYDE cells from the size and the central coordinate
res <- 5/60
half <- res / 2

# Build HYDE grid polygons from cell centers
hyde_cells <- data_full %>%
 distinct(cell_id, lat_cell, lon_cell) %>%
 rowwise() %>%
 mutate(geometry = list(st_polygon(list(matrix(c(
    lon_cell - half, lat_cell - half,
    lon_cell + half, lat_cell - half,
    lon_cell + half, lat_cell + half,
    lon_cell - half, lat_cell + half,
    lon_cell - half, lat_cell - half), ncol = 2, byrow = TRUE))))) %>%
 ungroup() %>%
 st_as_sf(crs = 4326)

cell_stats <- data_full_es_decade %>%
 group_by(cell_id) %>%
 summarise(
  treated = any(g_std > 0, na.rm = TRUE),
  inv_per_100k_mean = mean(inv_per_100k, na.rm = TRUE),
  inv_per_100k_median = median(inv_per_100k, na.rm = TRUE),
  .groups = "drop"
 ) %>%
 mutate(inv_plot = ifelse(inv_per_100k_mean == 0, NA, inv_per_100k_mean) )

hyde_cells <- hyde_cells %>%
 left_join(cell_stats, by = "cell_id")

# Creating map projections
fac_proj   <- st_transform(fac_sf, 5070)
hyde_proj  <- st_transform(hyde_cells, 5070)
us_proj    <- st_transform(us_sf, 5070)

# Create 50km buffers
fac_buffer <- st_buffer(fac_proj, dist = 50000)

# Convert back to coordinaates for ggplot
fac_ll    <- st_transform(fac_proj, 4326)
buffer_ll <- st_transform(fac_buffer, 4326)
hyde_ll   <- st_transform(hyde_proj, 4326)
us_ll     <- st_transform(us_proj, 4326)

# Plot of the treated
hyde_ll <- hyde_ll %>%
 mutate(treat_label = ifelse(treated, "Treated HYDE cell", "Untreated HYDE cell"))

plot_treatment <- ggplot() +
 geom_sf(data = us_ll, fill = "gray95", color = "gray70") +
 geom_sf(data = hyde_ll, aes(fill = treat_label), color = NA,
  alpha = 0.5) +
 geom_sf(data = buffer_ll, aes(color = "50km radius"),
  fill = NA,linewidth = 0.3 ) +
 geom_sf( data = fac_ll,aes(shape = "Facility"), size = 1) +
 scale_fill_manual( values = c( "Treated HYDE cell" = "blue",
   "Untreated HYDE cell" = "red"),
  name = "Grid cells" ) + scale_color_manual(
  values = c("50km radius" = "black"),
  name = "") +
 scale_shape_manual( values = c("Facility" = 19),
  name = "") +
 coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
 theme_minimal()

plot_treatment

# Plot of the density
plot_decade_density_all <- ggplot() +
 geom_sf(data = us_ll, fill = "gray95", color = "gray70") +
 geom_sf(data = hyde_ll, aes(fill = inv_plot), color = NA) +
 geom_sf(data = buffer_ll, fill = NA, linewidth = .6) +
 geom_sf(data = fac_ll, size = 1) +
 scale_fill_viridis_c(option = "plasma",
                      trans = "log10", na.value = "transparent") +
 coord_sf(xlim = c(-125,-65), ylim = c(25,50)) +
 theme_minimal() +
 labs(title = "Inventors per 100k - All Decades (Mean) - In Log",
  fill = "Log (Inv / 100k)")

###############################################################################
# Saving Results
###############################################################################
ggsave(filename = "results/treatment_map.png", plot=plot_treatment, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "results/density.png", plot= plot_decade_density_all, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "results/ES_US_full_std.png", plot= decade_50_std, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "results/ES_US_full_alt.png", plot= decade_50_alt, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "results/ES_US_selected_std.png", plot= es_alt_facilities_std, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "results/ES_US_selected_alt.png", plot= es_alt_facilities_alt, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "results/plot_means.png", plot= plot_means_full, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "results/plot_means_focus.png", plot= plot_means_focus, width = 8,
       height = 6, dpi = 300)
