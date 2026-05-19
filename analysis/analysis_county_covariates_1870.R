###############################################################################
# Project: Determinants of Talent Production via Scientific Infrastructure
# Goal: County event studies with fixed pre-treatment covariate means through 1870
###############################################################################

rm(list = ls())

library("tidyverse")
library("did")
library("sf")

source("../prep/raw_paths.R")

initial_time <- Sys.time()

###############################################################################
# Paths
###############################################################################

results_subdir <- "county_facilities_covariates_1870"

results_subdir_path <- function(...) {
 results_file_path(results_subdir, ...)
}

event_study_y_limits <- c(-10, 10)

###############################################################################
# Helpers
###############################################################################

as_geoid <- function(x) {
 str_pad(as.character(x), width = 5, side = "left", pad = "0")
}

complete_control_counties <- function(data, controls) {
 data %>%
  distinct(GEOID, across(all_of(controls))) %>%
  filter(if_all(all_of(controls), ~ !is.na(.x))) %>%
  pull(GEOID)
}

extract_dynamic_att <- function(es, spec_name, sample_name, timing_name) {
 tibble(
  spec = spec_name,
  sample = sample_name,
  timing = timing_name,
  event_time = es$egt,
  estimate = es$att.egt,
  se = es$se.egt,
  ci_low = estimate - 1.96 * se,
  ci_high = estimate + 1.96 * se
 )
}

ESgraph <- function(data, controls, spec_name, sample_name, timing_name,
                    treat_var, title_add, window = 70) {
 analysis_counties <- complete_control_counties(data, controls)

 data_es <- data %>%
  filter(GEOID %in% analysis_counties) %>%
  mutate(GEOID = as.numeric(GEOID))

 xformla <- as.formula(paste("~", paste(controls, collapse = " + ")))

 out <- att_gt(
  yname = "inv_per_100k",
  tname = "decade",
  idname = "GEOID",
  gname = treat_var,
  xformla = xformla,
  data = data_es,
  control_group = "notyettreated",
  est_method = "dr",
  base_period = "universal",
  cores = 4
 )

 es <- aggte(
  out,
  type = "dynamic",
  na.rm = TRUE,
  min_e = -window,
  max_e = window
 )

 n_treated_counties <- n_distinct(data_es$GEOID[data_es[[treat_var]] > 0])
 n_never_treated_counties <- n_distinct(data_es$GEOID[data_es[[treat_var]] == 0])

 plot <- ggdid(es) +
  labs(
   x = "Relative Time",
   y = "Effect",
   title = paste0("Average Effect by Length of Exposure - ", title_add)
  ) +
  annotate(
   "label",
   x = Inf,
   y = Inf,
   hjust = 1.05,
   vjust = 1.15,
   label = paste0(
    "Treated counties: ", n_treated_counties,
    "\nControl counties: ", n_never_treated_counties
   ),
   size = 3.4,
  label.size = 0.2,
  fill = "white",
  alpha = 0.85
 ) +
  coord_cartesian(ylim = event_study_y_limits)

 list(
  out = out,
  es = es,
  plot = plot,
  n_counties = n_distinct(data_es$GEOID),
  n_rows = nrow(data_es),
  n_treated_counties = n_treated_counties,
  n_never_treated_counties = n_never_treated_counties
 )
}

run_spec <- function(data_full_es_decade, data_full_es_decade_alt,
                     controls, spec_name) {
 runs <- list(
  full_std = list(
   data = data_full_es_decade,
   sample = "full_facilities",
   timing = "standard_decade",
   treat_var = "g_std",
   title = "50km - Standard Decade"
  ),
  full_shift = list(
   data = data_full_es_decade,
   sample = "full_facilities",
   timing = "shifted_decade",
   treat_var = "g_shift",
   title = "50km - Alternative Decade"
  ),
  selected_std = list(
   data = data_full_es_decade_alt,
   sample = "selected_facilities",
   timing = "standard_decade",
   treat_var = "g_std",
   title = "50km - Standard Decade - Selected Facilities"
  ),
  selected_shift = list(
   data = data_full_es_decade_alt,
   sample = "selected_facilities",
   timing = "shifted_decade",
   treat_var = "g_shift",
   title = "50km - Alternative Decade - Selected Facilities"
  )
 )

 imap(runs, function(run, run_name) {
  result <- ESgraph(
   data = run$data,
   controls = controls,
   spec_name = spec_name,
   sample_name = run$sample,
   timing_name = run$timing,
   treat_var = run$treat_var,
   title_add = paste0(run$title, " - ", str_to_title(spec_name), " Controls")
  )

  ggsave(
   filename = results_subdir_path(
    paste0("ES_county_cov1870_", spec_name, "_", run_name, ".png")
   ),
   plot = result$plot,
   width = 8,
   height = 6,
   dpi = 300
  )

  result
 })
}

###############################################################################
# Load data
###############################################################################

data_full <- read_csv(
 output_file_path("us_panel_county.csv"),
 col_types = cols(GEOID = col_character(), .default = col_guess())
) %>%
 mutate(
  GEOID = as_geoid(GEOID),
  inv_per_100k = replace_na(inv_per_100k, 0)
 )

facilities <- read_delim(
 output_file_path("facilities_us.csv"),
 delim = ";",
 locale = locale(decimal_mark = ".", grouping_mark = ""),
 show_col_types = FALSE
)

facilities_alt <- read_delim(
 output_file_path("facilities_us_alt.csv"),
 delim = ";",
 locale = locale(decimal_mark = ".", grouping_mark = ""),
 show_col_types = FALSE
)

county_covariates <- read_csv(
 output_file_path("county_tpe_covariates_clean.csv"),
 col_types = cols(GEOID = col_character(), .default = col_guess())
) %>%
 mutate(GEOID = as_geoid(GEOID))

county_demographics <- read_csv(
 output_file_path("county_nhgis_demographics_panel.csv"),
 col_types = cols(GEOID = col_character(), .default = col_guess())
) %>%
 mutate(GEOID = as_geoid(GEOID))

# Build fixed pre-treatment controls. These are county-level means over all
# available observations through 1870, so they stay predetermined for every
# facility event in the panel.
###############################################################################

mean_na <- function(x) {
 if (all(is.na(x))) {
  NA_real_
 } else {
  mean(x, na.rm = TRUE)
 }
}

covariates_pre1870 <- county_covariates %>%
 filter(year <= 1870) %>%
 group_by(GEOID) %>%
 transmute(
  GEOID,
  frontier100kmL6_mean_pre1870 = mean_na(frontier100kmL6),
  cropland_km2_mean_pre1870 = mean_na(cropland_km2),
  grazeland_km2_mean_pre1870 = mean_na(grazeland_km2),
  canal_access_mean_pre1870 = mean_na(canal_access),
  sex_ratio_mean_pre1870 = mean_na(sex_ratio),
  post_offices_mean_pre1870 = mean_na(post_offices),
  manufacturing_output_real_1900_million_mean_pre1870 =
   mean_na(manufacturing_output_value_real_1900) / 1e6,
  farming_output_real_1900_million_mean_pre1870 =
   mean_na(farming_output_value_real_1900) / 1e6,
  immigrant_share_mean_pre1870 = mean_na(immigrant_share),
  hyde_population_thousand_mean_pre1870 = mean_na(hyde_population) / 1e3,
  inventors_per_100k_hyde_mean_pre1870 = mean_na(inventors_per_100k_hyde)
 ) %>%
 distinct(GEOID, .keep_all = TRUE) %>%
 ungroup()

demographics_pre1870 <- county_demographics %>%
 filter(year <= 1870) %>%
 group_by(GEOID) %>%
 summarise(
  slave_share_mean_pre1870 = mean_na(slave_share),
  illiterate_share_mean_pre1870 = mean_na(illiterate_share_total_population),
  .groups = "drop"
 )

controls_fixed <- data_full %>%
 distinct(GEOID) %>%
 left_join(covariates_pre1870, by = "GEOID") %>%
 left_join(demographics_pre1870, by = "GEOID")

control_sets <- list(
 baseline = c(
  "frontier100kmL6_mean_pre1870",
  "cropland_km2_mean_pre1870",
  "grazeland_km2_mean_pre1870",
  "canal_access_mean_pre1870"
 ),
 extended = c(
  "frontier100kmL6_mean_pre1870",
  "cropland_km2_mean_pre1870",
  "grazeland_km2_mean_pre1870",
  "canal_access_mean_pre1870",
  "sex_ratio_mean_pre1870",
  "post_offices_mean_pre1870",
  "manufacturing_output_real_1900_million_mean_pre1870",
  "farming_output_real_1900_million_mean_pre1870",
  "immigrant_share_mean_pre1870",
  "slave_share_mean_pre1870"
 ),
 full = c(
  "frontier100kmL6_mean_pre1870",
  "cropland_km2_mean_pre1870",
  "grazeland_km2_mean_pre1870",
  "canal_access_mean_pre1870",
  "sex_ratio_mean_pre1870",
  "post_offices_mean_pre1870",
  "manufacturing_output_real_1900_million_mean_pre1870",
  "farming_output_real_1900_million_mean_pre1870",
  "immigrant_share_mean_pre1870",
  "slave_share_mean_pre1870",
  "illiterate_share_mean_pre1870",
  "hyde_population_thousand_mean_pre1870",
  "inventors_per_100k_hyde_mean_pre1870"
 )
)

missing_controls <- imap_dfr(control_sets, function(controls, spec_name) {
 controls_fixed %>%
  summarise(across(all_of(controls), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "control", values_to = "missing_count") %>%
  mutate(
   spec = spec_name,
   total_counties = n_distinct(controls_fixed$GEOID),
   missing_pct = 100 * missing_count / total_counties,
   .before = 1
  )
})

###############################################################################
# Treatment timing
###############################################################################

facilities <- facilities %>%
 filter(!is.na(year)) %>%
 mutate(
  decade_std = floor(year / 10) * 10,
  decade_shift = ifelse(
   year %% 10 >= 7,
   floor(year / 10) * 10 + 10,
   floor(year / 10) * 10
  )
 )

facilities_alt <- facilities_alt %>%
 filter(!is.na(year)) %>%
 mutate(
  decade_std = floor(year / 10) * 10,
  decade_shift = ifelse(
   year %% 10 >= 7,
   floor(year / 10) * 10 + 10,
   floor(year / 10) * 10
  )
 )

counties_sf_analysis <- data_full %>%
 select(GEOID, lat_county, lon_county) %>%
 distinct() %>%
 st_as_sf(coords = c("lon_county", "lat_county"), crs = 4326) %>%
 st_transform(3857)

fac_sf <- facilities %>%
 st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
 st_transform(3857)

fac_sf_alt <- facilities_alt %>%
 st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
 st_transform(3857)

dist_mat <- st_distance(counties_sf_analysis, fac_sf)
dist_mat_alt <- st_distance(counties_sf_analysis, fac_sf_alt)

county_event_decade <- as.data.frame(dist_mat) %>%
 mutate(GEOID = counties_sf_analysis$GEOID) %>%
 pivot_longer(-GEOID, names_to = "fac_idx", values_to = "dist_m") %>%
 mutate(fac_idx = as.integer(gsub("V", "", fac_idx))) %>%
 left_join(
  facilities %>%
   mutate(fac_idx = row_number()) %>%
   select(fac_idx, decade_std, decade_shift),
  by = "fac_idx"
 )

county_event_decade_alt <- as.data.frame(dist_mat_alt) %>%
 mutate(GEOID = counties_sf_analysis$GEOID) %>%
 pivot_longer(-GEOID, names_to = "fac_idx", values_to = "dist_m") %>%
 mutate(fac_idx = as.integer(gsub("V", "", fac_idx))) %>%
 left_join(
  facilities_alt %>%
   mutate(fac_idx = row_number()) %>%
   select(fac_idx, decade_std, decade_shift),
  by = "fac_idx"
 )

radius_m <- 50000

county_event_decade <- county_event_decade %>%
 filter(as.numeric(dist_m) <= radius_m) %>%
 group_by(GEOID) %>%
 summarise(
  g_std = min(decade_std),
  g_shift = min(decade_shift),
  .groups = "drop"
 )

county_event_decade_alt <- county_event_decade_alt %>%
 filter(as.numeric(dist_m) <= radius_m) %>%
 group_by(GEOID) %>%
 summarise(
  g_std = min(decade_std),
  g_shift = min(decade_shift),
  .groups = "drop"
 )

data_full_es_decade <- data_full %>%
 left_join(county_event_decade, by = "GEOID") %>%
 left_join(controls_fixed, by = "GEOID") %>%
 mutate(
  g_std = ifelse(is.na(g_std), 0, g_std),
  g_shift = ifelse(is.na(g_shift), 0, g_shift)
 )

data_full_es_decade_alt <- data_full %>%
 left_join(county_event_decade_alt, by = "GEOID") %>%
 left_join(controls_fixed, by = "GEOID") %>%
 mutate(
  g_std = ifelse(is.na(g_std), 0, g_std),
  g_shift = ifelse(is.na(g_shift), 0, g_shift)
 )

###############################################################################
# Run event studies
###############################################################################

results <- imap(
 control_sets,
 \(controls, spec_name) run_spec(
  data_full_es_decade = data_full_es_decade,
  data_full_es_decade_alt = data_full_es_decade_alt,
  controls = controls,
  spec_name = spec_name
 )
)

sample_summary <- imap_dfr(results, function(spec_results, spec_name) {
 imap_dfr(spec_results, function(result, run_name) {
  tibble(
   spec = spec_name,
   run = run_name,
   n_counties = result$n_counties,
   n_rows = result$n_rows,
   n_treated_counties = result$n_treated_counties,
   n_never_treated_counties = result$n_never_treated_counties
  )
 })
})

dynamic_att_summary <- imap_dfr(results, function(spec_results, spec_name) {
 imap_dfr(spec_results, function(result, run_name) {
  run_parts <- str_split_fixed(run_name, "_", 2)
  extract_dynamic_att(
   result$es,
   spec_name = spec_name,
   sample_name = run_parts[, 1],
   timing_name = run_parts[, 2]
  )
 })
})

###############################################################################
# Export audits
###############################################################################

write_csv(
 missing_controls,
 results_subdir_path("county_facilities_cov1870_missing_controls.csv"),
 na = ""
)

write_csv(
 sample_summary,
 results_subdir_path("county_facilities_cov1870_sample_summary.csv"),
 na = ""
)

write_csv(
 dynamic_att_summary,
 results_subdir_path("county_facilities_cov1870_attgt_summary.csv"),
 na = ""
)

message("Saved outputs in: ", results_subdir_path())
message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
