###############################################################################
# Project: Determinants of Talent Production via Elite High Schools
# Goal: County event studies with 50 km elite-school exposure
#
# Exposure rule:
#   A county is treated if its county polygon intersects a 50 km buffer around an
#   elite school's county-centroid proxy location. Treatment timing is the first
#   full decade after the earliest nearby school opening.
#
# Outcomes:
#   stem_share_pct  = 100 * STEM share, with missing shares set to zero
#   stem_per_100k   = STEM notable births per 100k county population
#   any_stem_pct    = 100 * 1[n_stem > 0]
#
# Grouped comparison:
#   Counties are split by whether their first nearby elite-school exposure is
#   historically high-access or low-access. Each group is compared to counties
#   with no elite-school exposure within 50 km.
#
# Outputs:
#   results/elite_radius50_treatment.csv
#   results/elite_radius50_att_summary.csv
#   results/elite_radius50_dynamic.csv
#   results/elite_radius50_pretrend_leads.csv
#   results/elite_radius50_any_dynamic.png
#   results/elite_radius50_high_low_dynamic.png
#   results/elite_radius50_summary.txt
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("tidyverse")
  library("did")
  library("ggplot2")
  library("sf")
})

initial_time <- Sys.time()
theme_set(theme_minimal(base_size = 13))

radius_m     <- 50000
window_years <- 40L

###############################################################################
# Load data
###############################################################################

panel <- read_csv("../prep/output/us_panel_county_stem.csv", show_col_types = FALSE) %>%
  mutate(
    GEOID          = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade         = as.integer(decade),
    population     = replace_na(population, 0),
    n_inventors    = replace_na(n_inventors, 0),
    n_stem         = replace_na(n_stem, 0),
    stem_share_pct = 100 * replace_na(stem_share, 0),
    stem_per_100k  = replace_na(stem_per_100k, 0),
    any_stem_pct   = 100 * as.numeric(n_stem > 0)
  )

schools <- read_csv("../prep/output/elite_high_schools_national_1800_1930.csv",
                    show_col_types = FALSE) %>%
  mutate(
    county_geoid       = str_pad(as.character(county_geoid), width = 5, side = "left", pad = "0"),
    founding_year_used = as.integer(founding_year_used)
  )

first_panel_decade <- min(panel$decade, na.rm = TRUE)

###############################################################################
# 50 km treatment assignment
###############################################################################

county_pts <- panel %>%
  distinct(GEOID, lat_county, lon_county) %>%
  st_as_sf(coords = c("lon_county", "lat_county"), crs = 4326, remove = FALSE) %>%
  st_transform(3857)

school_pts <- schools %>%
  distinct(
    school, county_geoid, county_name, state_abbr, founding_year_used,
    poor_access_historical, lat_county, lon_county
  ) %>%
  mutate(
    g_full_post_open = if_else(
      founding_year_used %% 10L == 0L,
      founding_year_used,
      (founding_year_used %/% 10L) * 10L + 10L
    )
  ) %>%
  st_as_sf(coords = c("lon_county", "lat_county"), crs = 4326, remove = FALSE) %>%
  st_transform(3857)

dist_mat <- st_distance(county_pts, school_pts)
hit_idx  <- which(units::drop_units(dist_mat) <= radius_m, arr.ind = TRUE)

county_school_exposure <- tibble(
  GEOID      = county_pts$GEOID[hit_idx[, 1]],
  school_row = hit_idx[, 2]
) %>%
  bind_cols(st_drop_geometry(school_pts)[.$school_row, ]) %>%
  select(-school_row)

county_lookup <- panel %>%
  distinct(GEOID, lat_county, lon_county)

radius_treatment <- county_school_exposure %>%
  group_by(GEOID) %>%
  summarise(
    n_nearby_schools = n_distinct(school),
    first_exposure_year = min(founding_year_used, na.rm = TRUE),
    first_exposure_schools = paste(
      sort(unique(school[founding_year_used == first_exposure_year])),
      collapse = "; "
    ),
    first_exposure_access = if_else(
      any(poor_access_historical[founding_year_used == first_exposure_year] == "high"),
      "high",
      "low"
    ),
    g_any = min(g_full_post_open, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    county_school_exposure %>%
      filter(poor_access_historical == "high") %>%
      group_by(GEOID) %>%
      summarise(
        g_high = min(g_full_post_open, na.rm = TRUE),
        first_high_school = paste(
          sort(unique(school[g_full_post_open == min(g_full_post_open, na.rm = TRUE)])),
          collapse = "; "
        ),
        .groups = "drop"
      ),
    by = "GEOID"
  ) %>%
  left_join(
    county_school_exposure %>%
      filter(poor_access_historical == "low") %>%
      group_by(GEOID) %>%
      summarise(
        g_low = min(g_full_post_open, na.rm = TRUE),
        first_low_school = paste(
          sort(unique(school[g_full_post_open == min(g_full_post_open, na.rm = TRUE)])),
          collapse = "; "
        ),
        .groups = "drop"
      ),
    by = "GEOID"
  ) %>%
  left_join(county_lookup, by = "GEOID") %>%
  mutate(
    treated_in_first_panel_decade = g_any <= first_panel_decade
  )

write_csv(radius_treatment, "results/elite_radius50_treatment.csv")

###############################################################################
# Build panels
###############################################################################

panel_base <- panel %>%
  left_join(radius_treatment, by = "GEOID") %>%
  mutate(GEOID_num = as.numeric(GEOID))

panel_any <- panel_base %>%
  mutate(
    g_spec = case_when(
      is.na(g_any) ~ 0L,
      g_any <= first_panel_decade ~ NA_integer_,
      TRUE ~ g_any
    )
  ) %>%
  filter(!is.na(g_spec))

panel_high <- panel_base %>%
  filter(is.na(first_exposure_access) | first_exposure_access == "high") %>%
  mutate(
    g_spec = case_when(
      is.na(g_any) ~ 0L,
      g_any <= first_panel_decade ~ NA_integer_,
      TRUE ~ g_any
    )
  ) %>%
  filter(!is.na(g_spec))

panel_low <- panel_base %>%
  filter(is.na(first_exposure_access) | first_exposure_access == "low") %>%
  mutate(
    g_spec = case_when(
      is.na(g_any) ~ 0L,
      g_any <= first_panel_decade ~ NA_integer_,
      TRUE ~ g_any
    )
  ) %>%
  filter(!is.na(g_spec))

###############################################################################
# Estimation helpers
###############################################################################

run_att <- function(data, outcome) {
  suppressWarnings(
    att_gt(
      yname         = outcome,
      tname         = "decade",
      idname        = "GEOID_num",
      gname         = "g_spec",
      data          = data,
      control_group = "notyettreated",
      est_method    = "dr",
      base_period   = "universal",
      cores         = 4
    )
  )
}

tidy_simple <- function(att_obj, data, spec_label, outcome_label) {
  agg <- suppressWarnings(aggte(att_obj, type = "simple", na.rm = TRUE))
  tibble(
    spec             = spec_label,
    outcome          = outcome_label,
    treated_counties = n_distinct(data$GEOID[data$g_spec > 0]),
    control_counties = n_distinct(data$GEOID[data$g_spec == 0]),
    first_cohort     = min(data$g_spec[data$g_spec > 0], na.rm = TRUE),
    last_cohort      = max(data$g_spec[data$g_spec > 0], na.rm = TRUE),
    overall_att      = agg$overall.att,
    overall_se       = agg$overall.se,
    p_value          = 2 * (1 - pnorm(abs(agg$overall.att / agg$overall.se)))
  )
}

tidy_dynamic <- function(att_obj, spec_label, outcome_label, window = window_years) {
  dyn <- suppressWarnings(
    aggte(att_obj, type = "dynamic", na.rm = TRUE,
          min_e = -window, max_e = window)
  )

  tibble(
    spec       = spec_label,
    outcome    = outcome_label,
    event_time = dyn$egt,
    att        = dyn$att.egt,
    se         = dyn$se.egt
  ) %>%
    filter(!is.na(att), !is.na(se)) %>%
    mutate(
      conf_low  = att - 1.96 * se,
      conf_high = att + 1.96 * se
    )
}

estimate_spec <- function(data, spec_label, outcome_name, outcome_label) {
  att <- run_att(data, outcome_name)
  list(
    att_obj  = att,
    summary  = tidy_simple(att, data, spec_label, outcome_label),
    dynamic  = tidy_dynamic(att, spec_label, outcome_label)
  )
}

###############################################################################
# Run all specs x outcomes
###############################################################################

specs <- list(
  list(name = "Any nearby elite-school exposure", data = panel_any),
  list(name = "High-access first nearby exposure", data = panel_high),
  list(name = "Low-access first nearby exposure", data = panel_low)
)

outcomes <- list(
  list(var = "stem_share_pct", label = "STEM share (pct points)"),
  list(var = "stem_per_100k",  label = "STEM births per 100k"),
  list(var = "any_stem_pct",   label = "Any STEM birth (pct points)")
)

results <- list()

for (spec in specs) {
  for (outcome in outcomes) {
    key <- paste(spec$name, outcome$label, sep = " | ")
    results[[key]] <- estimate_spec(spec$data, spec$name, outcome$var, outcome$label)
  }
}

att_summary <- bind_rows(lapply(results, `[[`, "summary"))
dynamic_tbl <- bind_rows(lapply(results, `[[`, "dynamic"))

pretrend_leads <- dynamic_tbl %>%
  filter(event_time < 0) %>%
  group_by(spec, outcome) %>%
  summarise(
    lead_m40 = att[event_time == -40][1],
    lead_m30 = att[event_time == -30][1],
    lead_m20 = att[event_time == -20][1],
    se_m40   = se[event_time == -40][1],
    se_m30   = se[event_time == -30][1],
    se_m20   = se[event_time == -20][1],
    .groups = "drop"
  )

write_csv(att_summary,  "results/elite_radius50_att_summary.csv")
write_csv(dynamic_tbl,  "results/elite_radius50_dynamic.csv")
write_csv(pretrend_leads, "results/elite_radius50_pretrend_leads.csv")

###############################################################################
# Plots
###############################################################################

plot_any <- dynamic_tbl %>%
  filter(spec == "Any nearby elite-school exposure") %>%
  ggplot(aes(x = event_time, y = att)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              fill = "#8ecae6", alpha = 0.22) +
  geom_line(color = "#023047", linewidth = 0.9) +
  geom_point(color = "#023047", size = 1.8) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(-window_years, window_years, by = 10)) +
  labs(
    x = "Years relative to first full post-opening decade",
    y = "ATT",
    title = "50 km elite-school exposure: dynamic effects",
    subtitle = "Main spec using any nearby elite-school exposure"
  )

plot_high_low <- dynamic_tbl %>%
  filter(spec %in% c("High-access first nearby exposure",
                     "Low-access first nearby exposure")) %>%
  ggplot(aes(x = event_time, y = att, color = spec, fill = spec)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.6) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(-window_years, window_years, by = 10)) +
  scale_color_manual(values = c(
    "High-access first nearby exposure" = "#2a9d8f",
    "Low-access first nearby exposure"  = "#bc4749"
  )) +
  scale_fill_manual(values = c(
    "High-access first nearby exposure" = "#2a9d8f",
    "Low-access first nearby exposure"  = "#bc4749"
  )) +
  labs(
    x = "Years relative to first full post-opening decade",
    y = "ATT",
    color = NULL,
    fill = NULL,
    title = "50 km elite-school exposure: high- vs low-access splits",
    subtitle = "Each treated group is compared to counties with no nearby elite-school exposure"
  )

ggsave("results/elite_radius50_any_dynamic.png", plot_any,
       width = 8.5, height = 10, dpi = 300)
ggsave("results/elite_radius50_high_low_dynamic.png", plot_high_low,
       width = 8.5, height = 10, dpi = 300)

###############################################################################
# Text summary
###############################################################################

support_tbl <- bind_rows(
  tibble(
    spec = "Any nearby elite-school exposure",
    treated_counties = n_distinct(panel_any$GEOID[panel_any$g_spec > 0]),
    control_counties = n_distinct(panel_any$GEOID[panel_any$g_spec == 0])
  ),
  tibble(
    spec = "High-access first nearby exposure",
    treated_counties = n_distinct(panel_high$GEOID[panel_high$g_spec > 0]),
    control_counties = n_distinct(panel_high$GEOID[panel_high$g_spec == 0])
  ),
  tibble(
    spec = "Low-access first nearby exposure",
    treated_counties = n_distinct(panel_low$GEOID[panel_low$g_spec > 0]),
    control_counties = n_distinct(panel_low$GEOID[panel_low$g_spec == 0])
  )
)

high_counties <- radius_treatment %>%
  filter(first_exposure_access == "high", g_any > first_panel_decade) %>%
  arrange(g_any) %>%
  transmute(
    GEOID,
    first_exposure_year,
    treated_cohort = g_any,
    first_exposure_schools
  )

sink("results/elite_radius50_summary.txt")
cat("=== 50 km elite-school exposure event studies ===\n\n")
cat("School point proxy: school county centroid from the elite-school file.\n")
cat("Treated counties: county centroids within 50 km of that school-centroid proxy.\n")
cat("Treatment timing: first full post-opening decade.\n\n")

cat("=== Support ===\n")
print(support_tbl)
cat("\nCounties ever exposed within 50 km:", nrow(radius_treatment), "\n")
cat("Dropped because treated in or before 1870:", sum(radius_treatment$treated_in_first_panel_decade), "\n")
cat("Nearby high-access treated counties after 1870:", nrow(high_counties), "\n\n")

cat("=== ATT summary ===\n")
print(att_summary)
cat("\n=== Pre-period leads (-40, -30, -20) ===\n")
print(pretrend_leads)
cat("\n=== Nearby high-access treated counties ===\n")
print(high_counties)
cat("\nRuntime (minutes):", round(as.numeric(difftime(Sys.time(), initial_time, units = "mins")), 2), "\n")
sink()

cat("Elite-school 50 km analysis complete. Outputs written to analysis/results/.\n")
