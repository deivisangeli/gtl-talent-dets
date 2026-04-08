###############################################################################
# Project: Determinants of Talent Production via Scientific Infrastructure
# Author: GTF
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
# Facilities using the standard decade definition
facilities <- facilities %>%
 filter(!is.na(year)) %>%
 mutate(
  decade_std   = floor(year / 10) * 10,
  decade_shift = ifelse(year %% 10 >= 7,
                        floor(year / 10) * 10 + 10,
                        floor(year / 10) * 10)
 )

# Facilities using the alternative decade definition
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

# Replacing NA
data_full <- data_full %>%
 mutate(inv_per_100k = replace_na(inv_per_100k, 0))

# Full county POLYGONS (not centroids) for intersection-based treatment
counties_poly <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 st_transform(3857) %>%
 select(GEOID, geometry) %>%
 filter(as.integer(substr(GEOID, 1, 2)) <= 56)

# Facilities
fac_sf <- facilities %>%
 st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
 st_transform(3857)

# Alternative
fac_sf_alt <- facilities_alt %>%
 st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
 st_transform(3857)

###############################################################################
# Apply radius via polygon intersection
###############################################################################

# Criando raio 50km
radius_m <- 50000

# Buffer each facility by 50km
fac_buf     <- st_buffer(fac_sf,     dist = radius_m)
fac_buf_alt <- st_buffer(fac_sf_alt, dist = radius_m)

# Add facility index and decade info before joining
fac_buf <- fac_buf %>%
 mutate(fac_idx = row_number()) %>%
 select(fac_idx, decade_std, decade_shift)

fac_buf_alt <- fac_buf_alt %>%
 mutate(fac_idx = row_number()) %>%
 select(fac_idx, decade_std, decade_shift)

# Spatial join: a county is treated if its polygon intersects the buffer
# st_intersects catches any overlap - even a corner of the county touching the buffer
county_fac <- st_join(counties_poly, fac_buf,     join = st_intersects)
county_fac_alt <- st_join(counties_poly, fac_buf_alt, join = st_intersects)

# Keep earliest treatment decade per county
county_event_decade <- county_fac %>%
 st_drop_geometry() %>%
 filter(!is.na(fac_idx)) %>%
 group_by(GEOID) %>%
 summarise(g_std   = min(decade_std),
           g_shift = min(decade_shift),
           .groups = "drop")

county_event_decade_alt <- county_fac_alt %>%
 st_drop_geometry() %>%
 filter(!is.na(fac_idx)) %>%
 group_by(GEOID) %>%
 summarise(g_std   = min(decade_std),
           g_shift = min(decade_shift),
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

# Creating the Event Study Plot function
ESgraph <- function(data, type, window, control, treat_var, title_add = NULL) {

 out <- att_gt(
  yname         = "inv_per_100k",
  tname         = "decade",
  idname        = "GEOID",
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

# Just making sure the variable is numeric
data_full_es_decade     <- data_full_es_decade     %>% mutate(GEOID = as.numeric(GEOID))
data_full_es_decade_alt <- data_full_es_decade_alt %>% mutate(GEOID = as.numeric(GEOID))

# Creating the event studies
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
options(timeout = 1000)

# County polygons - keep GEOID as character to match data_full_es_decade
counties_map <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 st_transform(4326) %>%
 select(GEOID, geometry) %>%
 filter(as.integer(substr(GEOID, 1, 2)) <= 56)

# Facilities
fac_sf_plot <- facilities %>%
 st_as_sf(coords = c("lon", "lat"), crs = 4326)

# US outline
us_sf <- ne_countries(country = "United States of America",
                      scale = "medium", returnclass = "sf")

# County-level stats - coerce GEOID to character to be safe
county_stats <- data_full_es_decade %>%
 mutate(GEOID = as.character(GEOID)) %>%
 group_by(GEOID) %>%
 summarise(
  treated             = any(g_std > 0, na.rm = TRUE),
  inv_per_100k_mean   = mean(inv_per_100k, na.rm = TRUE),
  inv_per_100k_median = median(inv_per_100k, na.rm = TRUE),
  .groups = "drop"
 ) %>%
 mutate(
  inv_plot    = ifelse(inv_per_100k_mean == 0, NA, inv_per_100k_mean),
  treat_label = ifelse(treated, "Treated county", "Untreated county")
 )

# Join - both sides now character
counties_plot <- counties_map %>%
 left_join(county_stats, by = "GEOID") %>%
 mutate(treat_label = replace_na(treat_label, "Untreated county"))

# Project to US Albers for buffering
fac_proj      <- st_transform(fac_sf_plot,   5070)
counties_proj <- st_transform(counties_plot, 5070)
us_proj       <- st_transform(us_sf,         5070)
fac_buffer    <- st_buffer(fac_proj, dist = 50000)

# Back to 4326 for plotting
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

print(plot_treatment)

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
# Top 10 counties
###############################################################################

# Get county names
counties_names <- tigris::counties(cb = TRUE, year = 2020) %>%
 st_drop_geometry() %>%
 select(GEOID, NAME, STATEFP) %>%
 mutate(
  GEOID = as.numeric(GEOID))

# This is important to check the code and the state because there are counties names
# in different states
states <- tigris::states(cb = TRUE, year = 2020) %>%
 st_drop_geometry() %>%
 select(STATEFP, STUSPS)

counties_names <- counties_names %>%
 left_join(states, by = "STATEFP") %>%
 mutate(GEOID = as.character(GEOID))

# Top 10 counties by mean inventors per 100k
top_counties <- county_stats %>%
 arrange(desc(inv_per_100k_mean)) %>%
 slice_head(n = 10) %>%
 select(GEOID, mean_inv = inv_per_100k_mean)

# Now join to our ranking
top_counties_named <- top_counties %>%
 left_join(counties_names, by = "GEOID") %>%
 select(GEOID, NAME, mean_inv)

top_counties_named

###############################################################################
# Fermilab Analysis
###############################################################################

# The counties we will be selecting based on the official document
sample_counties <- c(
 "17043", # treated: Weston, IL area
 "26161", # Ann Arbor, MI
 "36103", # Brookhaven / Upton, NY
 "08031", # Denver, CO
 "55025", # Madison, WI
 "06017"  # Sierra Foothills / Sacramento area
)

treated_geoid <- "17043"   # Weston county
treat_decade  <- 1970      # Because is 1969 the construction date

# Creating the sample
data_weston_sample <- data_full %>%
 filter(GEOID %in% sample_counties) %>%
 mutate(
  g_weston = ifelse(GEOID == treated_geoid, treat_decade, 0),
  GEOID = as.numeric(GEOID)
 )

# Running event study
weston_es <- ESgraph(
 data      = data_weston_sample,
 type      = "dynamic",
 window    = 70,
 control   = "notyettreated",
 treat_var = "g_weston",
 title_add = "Weston vs. other shortlisted counties"
)

# Now the plot of the series
# build plotting data
plot_weston_ts <- data_full %>%
 filter(GEOID %in% sample_counties) %>%
 mutate(
  # replace NA with 0
  inv_per_100k = replace_na(inv_per_100k, 0),

  # County labels
  county_label = case_when(
   GEOID == "17043" ~ "DuPage, IL (Weston)",
   GEOID == "26161" ~ "Washtenaw, MI (Ann Arbor)",
   GEOID == "36103" ~ "Suffolk, NY (Brookhaven)",
   GEOID == "08031" ~ "Denver, CO",
   GEOID == "55025" ~ "Dane, WI (Madison)",
   GEOID == "06017" ~ "El Dorado, CA (Sierra Foothills)",
   TRUE ~ GEOID
  ),
  treated_status = ifelse(GEOID == treated_geoid, "Treated", "Control")
 )

# Plot
ggplot(plot_weston_ts, aes(x = decade, y = inv_per_100k, color = county_label)) +
 geom_line(linewidth = 1) +
 geom_point(size = 2) +
 geom_vline(xintercept = treat_decade, linetype = "dashed") +
 labs(
  x = "Decade",
  y = "Inventors per 100k",
  color = "County",
  title = "Historical inventors per 100k for shortlisted counties",
  subtitle = "Weston treated county versus other shortlisted candidate counties"
 ) +
 theme_minimal() +
 theme(legend.position = "bottom")

#######################################################################################
# Alternative, using nearby counties:
counties_poly <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 st_transform(3857) %>%
 select(GEOID, NAME, STATEFP, geometry) %>%
 filter(as.integer(substr(GEOID, 1, 2)) <= 56)

# Using the codes again
treated_seed  <- "17043"
control_seeds <- c("26161", "36103", "08031", "55025", "06017")
sample_seeds  <- c(treated_seed, control_seeds)

# Now get the counties spatial object
seed_poly <- counties_poly %>%
 filter(GEOID %in% sample_seeds)

# Neighbors counties
touch_list <- st_touches(seed_poly, counties_poly)

neighbors_df <- lapply(seq_along(touch_list), function(i) {
 tibble(
  seed_geoid     = seed_poly$GEOID[i],
  neighbor_geoid = counties_poly$GEOID[touch_list[[i]]]
 )
}) %>%
 bind_rows()

# Neighbors that will be treated
treated_neighbors <- neighbors_df %>%
 filter(seed_geoid == treated_seed) %>%
 pull(neighbor_geoid) %>%
 unique()

# Neighbors that will be control
control_neighbors <- neighbors_df %>%
 filter(seed_geoid %in% control_seeds) %>%
 pull(neighbor_geoid) %>%
 unique()

# Now the groups
treated_group_expanded <- unique(c(treated_seed, treated_neighbors))
control_group_expanded <- unique(c(control_seeds, control_neighbors))

# If there is overlpa (in this case there isn't)
overlap_counties <- intersect(treated_group_expanded, control_group_expanded)

# Final groups
treated_final <- setdiff(treated_group_expanded, overlap_counties)
control_final <- setdiff(control_group_expanded, overlap_counties)

sample_final <- c(treated_final, control_final)
treat_decade <- 1970

# Final dataset
data_weston_border_sample <- data_full %>%
 filter(GEOID %in% sample_final) %>%
 mutate(
  g_weston_border = ifelse(GEOID %in% treated_final, treat_decade, 0),
  GEOID = as.numeric(GEOID)
 )

# Making the event study
weston_border_es <- ESgraph(
 data      = data_weston_border_sample,
 type      = "dynamic",
 window    = 70,
 control   = "notyettreated",
 treat_var = "g_weston_border",
 title_add = "Weston vs shortlisted counties + border-connected neighbors"
)


###############################################################################
# Saving Results
###############################################################################
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

ggsave("results/plot_fermilab.png",   plot = weston_es,
       width = 8, height = 6, dpi = 300)

ggsave("results/plot_fermilab_bordercounties.png",   plot = weston_border_es,
       width = 8, height = 6, dpi = 300)
