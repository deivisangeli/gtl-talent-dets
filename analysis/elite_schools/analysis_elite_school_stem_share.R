###############################################################################
# Project: Determinants of Talent Production via Elite High Schools
# Goal: County event studies for the STEM share outcome
#
# Outcome:
#   stem_share_pct = 100 * STEM share among county-decade notable scientist births
#                    with missing shares set to zero when a county-decade has
#                    no notable scientist births in the data.
#
# Treatment timing:
#   first full post-opening decade after a county's first elite-school opening.
#   This is more conservative than coding the opening decade itself as fully
#   treated in a birth-decade panel.
#
# Outputs:
#   results/elite_school_county_treatment.csv
#   results/elite_stem_share_att_summary.csv
#   results/elite_stem_share_dynamic_any.csv
#   results/elite_stem_share_dynamic_high_low.csv
#   results/elite_stem_share_any_dynamic.png
#   results/elite_stem_share_high_low_dynamic.png
#   results/elite_stem_share_summary.txt
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("tidyverse")
  library("did")
  library("ggplot2")
})
args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repo_root <- if (basename(cwd) == "analysis") dirname(cwd) else if (basename(dirname(cwd)) == "analysis") dirname(dirname(cwd)) else cwd
}
source(file.path(repo_root, "paths.R"))

results_dir <- file.path(TALENT_DETS_DATA_DIR, "results", "elite_schools", "elite_school_stem_share_legacy")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

initial_time <- Sys.time()
theme_set(theme_minimal(base_size = 13))

window_years <- 40L

###############################################################################
# Load data
###############################################################################

panel <- read_csv(file.path(DATA_OUTPUT, "us_panel_county_stem.csv"), show_col_types = FALSE) %>%
  mutate(
    GEOID          = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade         = as.integer(decade),
    n_inventors    = replace_na(n_inventors, 0),
    stem_share_pct = 100 * replace_na(stem_share, 0)
  )

schools <- read_csv(file.path(SCHOOLS_OUTPUT, "elite_high_schools_national_1800_1930.csv"),
                    show_col_types = FALSE) %>%
  mutate(
    county_geoid       = str_pad(as.character(county_geoid), width = 5, side = "left", pad = "0"),
    founding_year_used = as.integer(founding_year_used)
  )

first_panel_decade <- min(panel$decade, na.rm = TRUE)

###############################################################################
# County treatment file
###############################################################################

county_treatment <- schools %>%
  group_by(county_geoid) %>%
  summarise(
    county_name = first(county_name),
    state_abbr  = first(state_abbr),
    n_schools   = n(),
    first_open_year = min(founding_year_used, na.rm = TRUE),
    first_school = paste(sort(unique(school[founding_year_used == first_open_year])),
                         collapse = "; "),
    first_open_access = if_else(
      any(poor_access_historical[founding_year_used == first_open_year] == "high"),
      "high",
      "low"
    ),
    first_high_year = {
      high_years <- founding_year_used[poor_access_historical == "high"]
      if (length(high_years) > 0) min(high_years, na.rm = TRUE) else NA_integer_
    },
    first_low_year = {
      low_years <- founding_year_used[poor_access_historical == "low"]
      if (length(low_years) > 0) min(low_years, na.rm = TRUE) else NA_integer_
    },
    .groups = "drop"
  ) %>%
  mutate(
    first_open_decade = (first_open_year %/% 10L) * 10L,
    # First full decade after the school exists.
    g_full_post_open = if_else(
      first_open_year %% 10L == 0L,
      first_open_year,
      (first_open_year %/% 10L) * 10L + 10L
    ),
    first_high_full_post = if_else(
      is.na(first_high_year),
      NA_integer_,
      if_else(first_high_year %% 10L == 0L,
              first_high_year,
              (first_high_year %/% 10L) * 10L + 10L)
    ),
    first_low_full_post = if_else(
      is.na(first_low_year),
      NA_integer_,
      if_else(first_low_year %% 10L == 0L,
              first_low_year,
              (first_low_year %/% 10L) * 10L + 10L)
    ),
    treated_in_first_panel_decade = g_full_post_open <= first_panel_decade
  )

write_csv(county_treatment, file.path(results_dir, "elite_school_county_treatment.csv"))

###############################################################################
# Build panels for event-study designs
###############################################################################

panel_base <- panel %>%
  left_join(county_treatment, by = c("GEOID" = "county_geoid")) %>%
  mutate(GEOID_num = as.numeric(GEOID))

panel_any <- panel_base %>%
  mutate(
    g_any = case_when(
      is.na(g_full_post_open) ~ 0L,
      g_full_post_open <= first_panel_decade ~ NA_integer_,
      TRUE ~ g_full_post_open
    )
  ) %>%
  filter(!is.na(g_any))

panel_high <- panel_base %>%
  filter(is.na(first_open_access) | first_open_access == "high") %>%
  mutate(
    g_high = case_when(
      is.na(g_full_post_open) ~ 0L,
      g_full_post_open <= first_panel_decade ~ NA_integer_,
      TRUE ~ g_full_post_open
    )
  ) %>%
  filter(!is.na(g_high))

panel_low <- panel_base %>%
  filter(is.na(first_open_access) | first_open_access == "low") %>%
  mutate(
    g_low = case_when(
      is.na(g_full_post_open) ~ 0L,
      g_full_post_open <= first_panel_decade ~ NA_integer_,
      TRUE ~ g_full_post_open
    )
  ) %>%
  filter(!is.na(g_low))

###############################################################################
# Helper functions
###############################################################################

run_att <- function(data, gname) {
  suppressWarnings(
    att_gt(
      yname         = "stem_share_pct",
      tname         = "decade",
      idname        = "GEOID_num",
      gname         = gname,
      data          = data,
      control_group = "notyettreated",
      est_method    = "dr",
      base_period   = "universal",
      cores         = 4
    )
  )
}

tidy_dynamic <- function(att_obj, label, window = window_years) {
  dyn <- suppressWarnings(
    aggte(att_obj, type = "dynamic", na.rm = TRUE,
          min_e = -window, max_e = window)
  )

  tibble(
    spec       = label,
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

tidy_simple <- function(att_obj, data, gname, label) {
  agg <- suppressWarnings(aggte(att_obj, type = "simple", na.rm = TRUE))
  tibble(
    spec             = label,
    treated_counties = n_distinct(data$GEOID[data[[gname]] > 0]),
    control_counties = n_distinct(data$GEOID[data[[gname]] == 0]),
    first_cohort     = min(data[[gname]][data[[gname]] > 0], na.rm = TRUE),
    last_cohort      = max(data[[gname]][data[[gname]] > 0], na.rm = TRUE),
    overall_att      = agg$overall.att,
    overall_se       = agg$overall.se,
    p_value          = 2 * (1 - pnorm(abs(agg$overall.att / agg$overall.se)))
  )
}

###############################################################################
# Estimate event studies
###############################################################################

att_any  <- run_att(panel_any,  "g_any")
att_high <- run_att(panel_high, "g_high")
att_low  <- run_att(panel_low,  "g_low")

dyn_any  <- tidy_dynamic(att_any,  "Any elite-school opening")
dyn_high <- tidy_dynamic(att_high, "High-access first opening")
dyn_low  <- tidy_dynamic(att_low,  "Low-access first opening")

att_summary <- bind_rows(
  tidy_simple(att_any,  panel_any,  "g_any",  "Any elite-school opening"),
  tidy_simple(att_high, panel_high, "g_high", "High-access first opening"),
  tidy_simple(att_low,  panel_low,  "g_low",  "Low-access first opening")
)

write_csv(att_summary, file.path(results_dir, "elite_stem_share_att_summary.csv"))
write_csv(dyn_any, file.path(results_dir, "elite_stem_share_dynamic_any.csv"))
write_csv(bind_rows(dyn_high, dyn_low), file.path(results_dir, "elite_stem_share_dynamic_high_low.csv"))

###############################################################################
# Plots
###############################################################################

plot_any <- ggplot(dyn_any, aes(x = event_time, y = att)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              fill = "#8ecae6", alpha = 0.25) +
  geom_line(color = "#023047", linewidth = 0.9) +
  geom_point(color = "#023047", size = 2) +
  scale_x_continuous(breaks = seq(-window_years, window_years, by = 10)) +
  labs(
    x = "Years relative to first full post-opening decade",
    y = "Effect on STEM share (percentage points)",
    title = "Elite-school openings and county STEM share",
    subtitle = "Treatment = county's first elite high school; outcome = STEM share of notable scientist births"
  )

dyn_compare <- bind_rows(dyn_high, dyn_low)

plot_high_low <- ggplot(dyn_compare,
                        aes(x = event_time, y = att, color = spec, fill = spec)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(-window_years, window_years, by = 10)) +
  scale_color_manual(values = c(
    "High-access first opening" = "#2a9d8f",
    "Low-access first opening"  = "#bc4749"
  )) +
  scale_fill_manual(values = c(
    "High-access first opening" = "#2a9d8f",
    "Low-access first opening"  = "#bc4749"
  )) +
  labs(
    x = "Years relative to first full post-opening decade",
    y = "Effect on STEM share (percentage points)",
    color = NULL,
    fill = NULL,
    title = "STEM-share event study by historical access",
    subtitle = "Treated counties split by whether the first elite-school opening was historically high- or low-access"
  )

ggsave(file.path(results_dir, "elite_stem_share_any_dynamic.png"), plot_any,
       width = 8, height = 5.5, dpi = 300)
ggsave(file.path(results_dir, "elite_stem_share_high_low_dynamic.png"), plot_high_low,
       width = 8, height = 5.5, dpi = 300)

###############################################################################
# Text summary
###############################################################################

high_counties <- county_treatment %>%
  filter(first_open_access == "high", g_full_post_open > first_panel_decade) %>%
  arrange(g_full_post_open) %>%
  transmute(
    county = paste0(county_name, ", ", state_abbr),
    first_open_year,
    first_school,
    treated_cohort = g_full_post_open
  )

sink(file.path(results_dir, "elite_stem_share_summary.txt"))
cat("=== County STEM-share event study: elite high school openings ===\n\n")
cat("Outcome: stem_share_pct = 100 * stem_share, with missing shares set to zero.\n")
cat("Treatment timing: first full post-opening decade after a county's first elite-school opening.\n")
cat("Control group: not-yet-treated counties.\n\n")

cat("=== Sample construction ===\n")
cat("Counties in county panel:", n_distinct(panel$GEOID), "\n")
cat("Counties with at least one identified elite school:", nrow(county_treatment), "\n")
cat("Dropped because treated in or before first panel decade:", sum(county_treatment$treated_in_first_panel_decade), "\n")
cat("Usable treated counties in main event study:", n_distinct(panel_any$GEOID[panel_any$g_any > 0]), "\n")
cat("Usable high-access treated counties:", n_distinct(panel_high$GEOID[panel_high$g_high > 0]), "\n")
cat("Usable low-access treated counties:", n_distinct(panel_low$GEOID[panel_low$g_low > 0]), "\n")
cat("County-decades with positive notable-scientist births:", sum(panel$n_inventors > 0), "of", nrow(panel), "\n\n")

cat("=== Overall ATT estimates (percentage points) ===\n")
print(att_summary)
cat("\n=== High-access treated counties used in grouped comparison ===\n")
print(high_counties)

elapsed <- difftime(Sys.time(), initial_time, units = "mins")
cat("\nRuntime (minutes):", round(as.numeric(elapsed), 2), "\n")
sink()

cat("Elite-school STEM-share analysis complete. Outputs written to Dropbox results/elite_schools/.\n")
