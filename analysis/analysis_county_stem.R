###############################################################################
# Project: Determinants of Talent Production via Scientific Infrastructure
# Author: Lucas Mattos and Deivis Angeli (GTF)
# Goal: County event studies — STEM scientists only
#
# Mirrors analysis_border.R but uses us_panel_county_stem.csv (STEM-only counts)
# and stem_per_100k as the outcome variable.
# All figures saved with stem_ prefix for direct comparison.
###############################################################################

rm(list = ls())

library("tidyverse")
library("ggplot2")
library("fixest")
library("did")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("tigris")
library("viridis")

initial_time <- Sys.time()
options(tigris_use_cache = TRUE)

###############################################################################
# Load data
###############################################################################

data_full <- read_csv("../prep/output/us_panel_county_stem.csv",
                      show_col_types = FALSE) %>%
  mutate(stem_per_100k = replace_na(stem_per_100k, 0))

facilities <- read_delim("../prep/output/facilities_us.csv", delim = ";",
                         locale = locale(decimal_mark = ".", grouping_mark = ""),
                         show_col_types = FALSE)

facilities_alt <- read_delim("../prep/output/facilities_us_alt.csv", delim = ";",
                             locale = locale(decimal_mark = ".", grouping_mark = ""),
                             show_col_types = FALSE)

###############################################################################
# Adjust facilities to decade format
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
# Spatial objects: county polygons and facility buffers
###############################################################################

counties_poly <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_transform(3857) %>%
  select(GEOID, geometry) %>%
  filter(as.integer(substr(GEOID, 1, 2)) <= 56)

fac_sf <- facilities %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3857)

fac_sf_alt <- facilities_alt %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3857)

###############################################################################
# 50km buffer treatment assignment (polygon intersection)
###############################################################################

radius_m <- 50000

fac_buf <- st_buffer(fac_sf, dist = radius_m) %>%
  mutate(fac_idx = row_number()) %>%
  select(fac_idx, decade_std, decade_shift)

fac_buf_alt <- st_buffer(fac_sf_alt, dist = radius_m) %>%
  mutate(fac_idx = row_number()) %>%
  select(fac_idx, decade_std, decade_shift)

county_fac     <- st_join(counties_poly, fac_buf,     join = st_intersects)
county_fac_alt <- st_join(counties_poly, fac_buf_alt, join = st_intersects)

county_event_decade <- county_fac %>%
  st_drop_geometry() %>%
  filter(!is.na(fac_idx)) %>%
  group_by(GEOID) %>%
  summarise(g_std = min(decade_std), g_shift = min(decade_shift), .groups = "drop")

county_event_decade_alt <- county_fac_alt %>%
  st_drop_geometry() %>%
  filter(!is.na(fac_idx)) %>%
  group_by(GEOID) %>%
  summarise(g_std = min(decade_std), g_shift = min(decade_shift), .groups = "drop")

data_full_es_decade <- data_full %>%
  left_join(county_event_decade, by = "GEOID") %>%
  mutate(g_std   = ifelse(is.na(g_std),   0, g_std),
         g_shift = ifelse(is.na(g_shift), 0, g_shift))

data_full_es_decade_alt <- data_full %>%
  left_join(county_event_decade_alt, by = "GEOID") %>%
  mutate(g_std   = ifelse(is.na(g_std),   0, g_std),
         g_shift = ifelse(is.na(g_shift), 0, g_shift))

###############################################################################
# Event study function (outcome: stem_per_100k)
###############################################################################

ESgraph <- function(data, type, window, control, treat_var, title_add = NULL) {
  out <- att_gt(
    yname         = "stem_per_100k",
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
    labs(x = "Relative Time", y = "Effect on STEM scientists per 100k",
         title = if (!is.null(title_add)) {
           paste0("STEM: Average Effect by Length of Exposure - ", title_add)
         } else {
           "STEM: Average Effect by Length of Exposure"
         }) +
    coord_cartesian(ylim = c(-5, 5))
  print(plot)
  return(plot)
}

data_full_es_decade     <- data_full_es_decade     %>% mutate(GEOID = as.numeric(GEOID))
data_full_es_decade_alt <- data_full_es_decade_alt %>% mutate(GEOID = as.numeric(GEOID))

stem_decade_std <- ESgraph(
  data      = data_full_es_decade, type = "dynamic", window = 70,
  control   = "notyettreated",     treat_var = "g_std",
  title_add = paste0(radius_m / 1000, "km - Standard Decade"))

stem_decade_alt <- ESgraph(
  data      = data_full_es_decade, type = "dynamic", window = 70,
  control   = "notyettreated",     treat_var = "g_shift",
  title_add = paste0(radius_m / 1000, "km - Alternative Decade"))

stem_alt_facilities_std <- ESgraph(
  data      = data_full_es_decade_alt, type = "dynamic", window = 70,
  control   = "notyettreated",         treat_var = "g_std",
  title_add = paste0(radius_m / 1000, "km - Standard Decade - Selected Facilities"))

stem_alt_facilities_alt <- ESgraph(
  data      = data_full_es_decade_alt, type = "dynamic", window = 70,
  control   = "notyettreated",         treat_var = "g_shift",
  title_add = paste0(radius_m / 1000, "km - Alternative Decade - Selected Facilities"))

###############################################################################
# Means plot
###############################################################################

mean_df <- data_full_es_decade %>%
  mutate(cohort = ifelse(g_std == 0, "Never treated", as.character(g_std))) %>%
  group_by(decade, cohort) %>%
  summarise(mean_stem = mean(stem_per_100k, na.rm = TRUE), .groups = "drop")

plot_means_full <- ggplot(mean_df,
                          aes(x = decade, y = mean_stem, group = cohort, color = cohort)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 1.5) +
  labs(y     = "STEM scientists per 100k",
       title = "Average STEM Scientists by Treatment Cohort",
       subtitle = paste0(radius_m / 1000,
                         "km radius — cohorts defined by first exposure decade"),
       color = "Treatment cohort") +
  theme_minimal(base_size = 13)

plot_means_focus <- plot_means_full + coord_cartesian(ylim = c(0, 20))

###############################################################################
# Map evidence
###############################################################################
options(timeout = 1000)

counties_map <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_transform(4326) %>%
  select(GEOID, geometry) %>%
  filter(as.integer(substr(GEOID, 1, 2)) <= 56)

fac_sf_plot <- facilities %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

us_sf <- ne_countries(country = "United States of America",
                      scale = "medium", returnclass = "sf")

county_stats <- data_full_es_decade %>%
  mutate(GEOID = as.character(GEOID)) %>%
  group_by(GEOID) %>%
  summarise(
    treated               = any(g_std > 0, na.rm = TRUE),
    stem_per_100k_mean    = mean(stem_per_100k, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    stem_plot   = ifelse(stem_per_100k_mean == 0, NA, stem_per_100k_mean),
    treat_label = ifelse(treated, "Treated county", "Untreated county")
  )

counties_plot <- counties_map %>%
  left_join(county_stats, by = "GEOID") %>%
  mutate(treat_label = replace_na(treat_label, "Untreated county"))

fac_proj      <- st_transform(fac_sf_plot,   5070)
counties_proj <- st_transform(counties_plot, 5070)
us_proj       <- st_transform(us_sf,         5070)
fac_buffer    <- st_buffer(fac_proj, dist = 50000)

fac_4326      <- st_transform(fac_proj,      4326)
buffer_4326   <- st_transform(fac_buffer,    4326)
counties_4326 <- st_transform(counties_proj, 4326)
us_4326       <- st_transform(us_proj,       4326)

plot_treatment <- ggplot() +
  geom_sf(data = us_4326,       fill = "gray95", color = "gray70") +
  geom_sf(data = counties_4326, aes(fill = treat_label),
          color = "grey80", linewidth = 0.05, alpha = 0.6) +
  geom_sf(data = buffer_4326,   aes(color = "50km radius"),
          fill = NA, linewidth = 0.3) +
  geom_sf(data = fac_4326,      aes(shape = "Facility"), size = 0.5) +
  scale_fill_manual(values = c("Treated county" = "blue", "Untreated county" = "red"),
                    name = "Counties") +
  scale_color_manual(values = c("50km radius" = "black"), name = "") +
  scale_shape_manual(values = c("Facility" = 19), name = "") +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  theme_minimal() +
  labs(title = "Treatment assignment (STEM outcome)")

plot_density <- ggplot() +
  geom_sf(data = us_4326,       fill = "gray95", color = "gray70") +
  geom_sf(data = counties_4326, aes(fill = stem_plot),
          color = "grey80", linewidth = 0.05) +
  geom_sf(data = buffer_4326,   fill = NA, linewidth = 0.6) +
  geom_sf(data = fac_4326,      size = 1.5) +
  scale_fill_viridis_c(option   = "plasma",
                       trans    = "log10",
                       na.value = "transparent") +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  theme_minimal() +
  labs(title = "STEM scientists per 100k — All Decades (Mean) — Log scale",
       fill  = "Log (STEM / 100k)")

###############################################################################
# Fermilab / Weston county analysis
###############################################################################

sample_counties <- c(
  "17043",  # DuPage, IL (Weston — treated)
  "26161",  # Washtenaw, MI (Ann Arbor)
  "36103",  # Suffolk, NY (Brookhaven)
  "08031",  # Denver, CO
  "55025",  # Dane, WI (Madison)
  "06017"   # El Dorado, CA (Sierra Foothills)
)

treated_geoid <- "17043"
treat_decade  <- 1970

data_weston_sample <- data_full %>%
  filter(GEOID %in% sample_counties) %>%
  mutate(
    g_weston = ifelse(GEOID == treated_geoid, treat_decade, 0),
    GEOID    = as.numeric(GEOID)
  )

weston_es <- ESgraph(
  data      = data_weston_sample,
  type      = "dynamic",
  window    = 70,
  control   = "notyettreated",
  treat_var = "g_weston",
  title_add = "Weston vs. other shortlisted counties"
)

# Time-series plot for Weston and control counties
plot_weston_ts <- data_full %>%
  filter(GEOID %in% sample_counties) %>%
  mutate(
    stem_per_100k = replace_na(stem_per_100k, 0),
    county_label = case_when(
      GEOID == "17043" ~ "DuPage, IL (Weston)",
      GEOID == "26161" ~ "Washtenaw, MI (Ann Arbor)",
      GEOID == "36103" ~ "Suffolk, NY (Brookhaven)",
      GEOID == "08031" ~ "Denver, CO",
      GEOID == "55025" ~ "Dane, WI (Madison)",
      GEOID == "06017" ~ "El Dorado, CA (Sierra Foothills)",
      TRUE ~ GEOID
    )
  )

p_weston_ts <- ggplot(plot_weston_ts,
                      aes(x = decade, y = stem_per_100k, color = county_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = treat_decade, linetype = "dashed") +
  labs(
    x = "Decade", y = "STEM scientists per 100k",
    color = "County",
    title = "Historical STEM scientists per 100k — Fermilab shortlisted counties",
    subtitle = "Dashed line = Fermilab construction decade (1970)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Border counties expansion
counties_poly_named <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_transform(3857) %>%
  select(GEOID, NAME, STATEFP, geometry) %>%
  filter(as.integer(substr(GEOID, 1, 2)) <= 56)

seed_poly <- counties_poly_named %>%
  filter(GEOID %in% sample_counties)

touch_list <- st_touches(seed_poly, counties_poly_named)

neighbors_df <- lapply(seq_along(touch_list), function(i) {
  tibble(
    seed_geoid     = seed_poly$GEOID[i],
    neighbor_geoid = counties_poly_named$GEOID[touch_list[[i]]]
  )
}) %>%
  bind_rows()

treated_neighbors <- neighbors_df %>%
  filter(seed_geoid == treated_geoid) %>%
  pull(neighbor_geoid) %>% unique()

control_neighbors <- neighbors_df %>%
  filter(seed_geoid %in% sample_counties[sample_counties != treated_geoid]) %>%
  pull(neighbor_geoid) %>% unique()

treated_final <- unique(c(treated_geoid, treated_neighbors))
control_final <- setdiff(unique(c(sample_counties[sample_counties != treated_geoid],
                                   control_neighbors)),
                          treated_final)

data_weston_border_sample <- data_full %>%
  filter(GEOID %in% c(treated_final, control_final)) %>%
  mutate(
    g_weston_border = ifelse(GEOID %in% treated_final, treat_decade, 0),
    GEOID           = as.numeric(GEOID)
  )

weston_border_es <- ESgraph(
  data      = data_weston_border_sample,
  type      = "dynamic",
  window    = 70,
  control   = "notyettreated",
  treat_var = "g_weston_border",
  title_add = "Weston vs. shortlisted counties + border-connected neighbors"
)

###############################################################################
# Save outputs
###############################################################################

dir.create("results", showWarnings = FALSE)

ggsave("results/stem_treatment_map.png",         plot_treatment,         width = 8, height = 6, dpi = 300)
ggsave("results/stem_density.png",               plot_density,           width = 8, height = 6, dpi = 300)
ggsave("results/stem_ES_county_full_std.png",    stem_decade_std,        width = 8, height = 6, dpi = 300)
ggsave("results/stem_ES_county_full_alt.png",    stem_decade_alt,        width = 8, height = 6, dpi = 300)
ggsave("results/stem_ES_county_sel_std.png",     stem_alt_facilities_std, width = 8, height = 6, dpi = 300)
ggsave("results/stem_ES_county_sel_alt.png",     stem_alt_facilities_alt, width = 8, height = 6, dpi = 300)
ggsave("results/stem_plot_means.png",            plot_means_full,        width = 8, height = 6, dpi = 300)
ggsave("results/stem_plot_means_focus.png",      plot_means_focus,       width = 8, height = 6, dpi = 300)
ggsave("results/stem_fermilab_ES.png",           weston_es,              width = 8, height = 6, dpi = 300)
ggsave("results/stem_fermilab_ts.png",           p_weston_ts,            width = 8, height = 6, dpi = 300)
ggsave("results/stem_fermilab_border_ES.png",    weston_border_es,       width = 8, height = 6, dpi = 300)

cat("All STEM county figures saved to results/ with stem_ prefix.\n")
message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
