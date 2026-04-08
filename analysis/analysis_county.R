###############################################################################
# Project: Determinants of Talent Production via Scientific Infrastructure
# Author: Lucas Mattos and Deivis Angeli (GTF)
# Goal: Creating Outputs for Analysis - County version
###############################################################################

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
library("rnaturalearthdata")
library("tigris")
library("viridis")

initial_time <- Sys.time()
options(tigris_use_cache = TRUE)

###############################################################################
# Uploading databases
###############################################################################

data_full <- read_csv("../prep/output/us_panel_county.csv", show_col_types = FALSE)

facilities <- read_delim("../prep/output/facilities_us.csv", delim = ";",
                         locale = locale(decimal_mark = ".", grouping_mark = ""),
                         show_col_types = FALSE)

facilities_alt <- read_delim("../prep/output/facilities_us_alt.csv", delim = ";",
                             locale = locale(decimal_mark = ".", grouping_mark = ""),
                             show_col_types = FALSE)

###############################################################################
# Adjusting facilities in the decade format
###############################################################################

facilities <- facilities %>%
 filter(!is.na(year)) %>%
 mutate(
  decade_std   = floor(year / 10) * 10,
  decade_shift = ifelse(year %% 10 >= 7,
                        floor(year / 10) * 10 + 10,
                        floor(year / 10) * 10)
 )

facilities_alt <- facilities_alt %>%
 filter(!is.na(year)) %>%
 mutate(
  decade_std   = floor(year / 10) * 10,
  decade_shift = ifelse(year %% 10 >= 7,
                        floor(year / 10) * 10 + 10,
                        floor(year / 10) * 10)
 )

###############################################################################
# Creating the spatial objects
###############################################################################
# Making NA_s as 0
data_full <- data_full %>%
 mutate(inv_per_100k = replace_na(inv_per_100k, 0))


# County centroids as sf (replaces HYDE cells)
counties_sf_analysis <- data_full %>%
 select(GEOID, lat_county, lon_county) %>%
 distinct() %>%
 st_as_sf(coords = c("lon_county", "lat_county"), crs = 4326) %>%
 st_transform(3857)

# Facilities
fac_sf <- facilities %>%
 st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
 st_transform(3857)

fac_sf_alt <- facilities_alt %>%
 st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
 st_transform(3857)

# Distance matrices: county centroids x facilities
dist_mat     <- st_distance(counties_sf_analysis, fac_sf)
dist_mat_alt <- st_distance(counties_sf_analysis, fac_sf_alt)

# Long format
county_event_decade <- as.data.frame(dist_mat) %>%
 mutate(GEOID = counties_sf_analysis$GEOID) %>%
 pivot_longer(-GEOID, names_to = "fac_idx", values_to = "dist_m") %>%
 mutate(fac_idx = as.integer(gsub("V", "", fac_idx)))

county_event_decade_alt <- as.data.frame(dist_mat_alt) %>%
 mutate(GEOID = counties_sf_analysis$GEOID) %>%
 pivot_longer(-GEOID, names_to = "fac_idx", values_to = "dist_m") %>%
 mutate(fac_idx = as.integer(gsub("V", "", fac_idx)))

# Join facility decade info
county_event_decade <- county_event_decade %>%
 left_join(facilities %>% mutate(fac_idx = row_number()) %>%
            select(fac_idx, decade_std, decade_shift),
           by = "fac_idx")

county_event_decade_alt <- county_event_decade_alt %>%
 left_join(facilities_alt %>% mutate(fac_idx = row_number()) %>%
            select(fac_idx, decade_std, decade_shift),
           by = "fac_idx")

###############################################################################
# Apply radius + earliest decade
###############################################################################

radius_m <- 50000

county_event_decade <- county_event_decade %>%
 filter(as.numeric(dist_m) <= radius_m) %>%
 group_by(GEOID) %>%
 summarise(g_std = min(decade_std), g_shift = min(decade_shift),
           .groups = "drop")

county_event_decade_alt <- county_event_decade_alt %>%
 filter(as.numeric(dist_m) <= radius_m) %>%
 group_by(GEOID) %>%
 summarise(g_std = min(decade_std), g_shift = min(decade_shift),
           .groups = "drop")

# Merge into panel
data_full_es_decade <- data_full %>%
 left_join(county_event_decade, by = "GEOID") %>%
 mutate(g_std   = ifelse(is.na(g_std),   0, g_std),
        g_shift = ifelse(is.na(g_shift), 0, g_shift))

data_full_es_decade_alt <- data_full %>%
 left_join(county_event_decade_alt, by = "GEOID") %>%
 mutate(g_std   = ifelse(is.na(g_std),   0, g_std),
        g_shift = ifelse(is.na(g_shift), 0, g_shift))

###############################################################################
# Event Studies
###############################################################################

ESgraph <- function(data, type, window, control, treat_var, title_add = NULL) {

 out <- att_gt(
  yname         = "inv_per_100k",
  tname         = "decade",
  idname        = "GEOID",          # county FIPS replaces cell_id
  gname         = treat_var,
  data          = data,
  control_group = control,
  est_method    = "dr",
  base_period   = "universal",
  cores         = 4
 )

 es <- aggte(out, type = type, na.rm = TRUE,
             min_e = -window, max_e = window)

 plot <- ggdid(es) +
  labs(x = "Relative Time", y = "Effect",
       title = if (!is.null(title_add)) {
        paste0("Average Effect by Length of Exposure - ", title_add)
       } else {
        "Average Effect by Length of Exposure"
       }) +
  coord_cartesian(ylim = c(-5, 5))

 print(plot)
 return(plot)
}

# Note: GEOID must be numeric for att_gt - coerce before passing
data_full_es_decade     <- data_full_es_decade     %>% mutate(GEOID = as.numeric(GEOID))
data_full_es_decade_alt <- data_full_es_decade_alt %>% mutate(GEOID = as.numeric(GEOID))

decade_50_std <- ESgraph(
 data      = data_full_es_decade, type = "dynamic", window = 70,
 control   = "notyettreated",     treat_var = "g_std",
 title_add = paste0(radius_m / 1000, "km - Standard Decade"))

decade_50_alt <- ESgraph(
 data      = data_full_es_decade, type = "dynamic", window = 70,
 control   = "notyettreated",     treat_var = "g_shift",
 title_add = paste0(radius_m / 1000, "km - Alternative Decade"))

es_alt_facilities_std <- ESgraph(
 data      = data_full_es_decade_alt, type = "dynamic", window = 70,
 control   = "notyettreated",         treat_var = "g_std",
 title_add = paste0(radius_m / 1000, "km - Standard Decade - Selected Facilities"))

es_alt_facilities_alt <- ESgraph(
 data      = data_full_es_decade_alt, type = "dynamic", window = 70,
 control   = "notyettreated",         treat_var = "g_shift",
 title_add = paste0(radius_m / 1000, "km - Alternative Decade - Selected Facilities"))

###############################################################################
# Means Plot
###############################################################################

plot_df <- data_full_es_decade %>%
 mutate(cohort = ifelse(g_std == 0, "Never treated", as.character(g_std)))

mean_df <- plot_df %>%
 group_by(decade, cohort) %>%
 summarise(mean_inv = mean(inv_per_100k, na.rm = TRUE), .groups = "drop")

plot_means_full <- ggplot(mean_df,
                          aes(x = decade, y = mean_inv,
                              group = cohort, color = cohort)) +
 geom_line(linewidth = 0.9, alpha = 0.8) +
 geom_point(size = 1.5) +
 labs(y        = "Inventors per 100k",
      title    = "Average Inventors by Treatment Cohort",
      subtitle = paste0(radius_m / 1000,
                        "km radius - cohorts defined by first exposure decade"),
      color    = "Treatment cohort") +
 theme_minimal(base_size = 13)

plot_means_focus <- plot_means_full +
 coord_cartesian(ylim = c(0, 50))

###############################################################################
# Map Evidence
###############################################################################

# County polygons
counties_map <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 st_transform(4326) %>%
 select(GEOID, geometry) %>%
 filter(as.integer(substr(GEOID, 1, 2)) <= 56) %>%
 mutate(GEOID = as.numeric(GEOID))

# Facilities - use a clear name, never overwrite it
fac_sf_plot <- facilities %>%
 st_as_sf(coords = c("lon", "lat"), crs = 4326)

# US outline
us_sf <- ne_countries(country = "United States of America",
                      scale = "medium", returnclass = "sf")

# County-level stats
county_stats <- data_full_es_decade %>%
 group_by(GEOID) %>%
 summarise(
  treated             = any(g_std > 0, na.rm = TRUE),
  inv_per_100k_mean   = mean(inv_per_100k, na.rm = TRUE),
  inv_per_100k_median = median(inv_per_100k, na.rm = TRUE),
  .groups = "drop"
 ) %>%
 mutate(inv_plot = ifelse(inv_per_100k_mean == 0, NA, inv_per_100k_mean))

counties_plot <- counties_map %>%
 left_join(county_stats, by = "GEOID") %>%
 mutate(treat_label = ifelse(treated, "Treated county", "Untreated county"))

# Project everything to US Albers (5070) for buffering
fac_proj      <- st_transform(fac_sf_plot,   5070)
counties_proj <- st_transform(counties_plot, 5070)
us_proj       <- st_transform(us_sf,         5070)
fac_buffer    <- st_buffer(fac_proj, dist = 50000)

# Back to 4326 for ggplot - all distinct names
fac_4326      <- st_transform(fac_proj,      4326)
buffer_4326   <- st_transform(fac_buffer,    4326)
counties_4326 <- st_transform(counties_proj, 4326)
us_4326       <- st_transform(us_proj,       4326)

# Treatment map
plot_treatment <- ggplot() +
 geom_sf(data = us_4326,       fill = "gray95", color = "gray70") +
 geom_sf(data = counties_4326, aes(fill = treat_label),
         color = "grey80", linewidth = 0.05, alpha = 0.6) +
 geom_sf(data = buffer_4326,   aes(color = "50km radius"),
         fill = NA, linewidth = 0.3) +
 geom_sf(data = fac_4326,      aes(shape = "Facility"), size = 0.5) +
 scale_fill_manual(
  values = c("Treated county" = "blue", "Untreated county" = "red"),
  name   = "Counties") +
 scale_color_manual(values = c("50km radius" = "black"), name = "") +
 scale_shape_manual(values = c("Facility" = 19),         name = "") +
 coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
 theme_minimal()

plot_treatment

# Density map
plot_decade_density_all <- ggplot() +
 geom_sf(data = us_4326,       fill = "gray95", color = "gray70") +
 geom_sf(data = counties_4326, aes(fill = inv_plot),
         color = "grey80", linewidth = 0.05) +
 geom_sf(data = buffer_4326,   fill = NA, linewidth = 0.6) +
 geom_sf(data = fac_4326,      size = 1.5) +
 scale_fill_viridis_c(option   = "plasma",
                      trans    = "log10",
                      na.value = "transparent") +
 coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
 theme_minimal() +
 labs(title = "Inventors per 100k - All Decades (Mean) - Log scale",
      fill  = "Log (Inv / 100k)")

plot_decade_density_all

###############################################################################
# Saving Results
###############################################################################

dir.create("results", showWarnings = FALSE)

ggsave("results/treatment_map.png",      plot = plot_treatment,
       width = 8, height = 6, dpi = 300)
ggsave("results/density.png",            plot = plot_decade_density_all,
       width = 8, height = 6, dpi = 300)
ggsave("results/ES_county_full_std.png", plot = decade_50_std,
       width = 8, height = 6, dpi = 300)
ggsave("results/ES_county_full_alt.png", plot = decade_50_alt,
       width = 8, height = 6, dpi = 300)
ggsave("results/ES_county_sel_std.png",  plot = es_alt_facilities_std,
       width = 8, height = 6, dpi = 300)
ggsave("results/ES_county_sel_alt.png",  plot = es_alt_facilities_alt,
       width = 8, height = 6, dpi = 300)
ggsave("results/plot_means.png",         plot = plot_means_full,
       width = 8, height = 6, dpi = 300)
ggsave("results/plot_means_focus.png",   plot = plot_means_focus,
       width = 8, height = 6, dpi = 300)

message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
