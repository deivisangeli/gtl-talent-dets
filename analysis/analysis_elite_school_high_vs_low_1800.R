###############################################################################
# Project: Determinants of Talent Production via Elite High Schools
# Goal: Compare high-access versus low-access elite-school exposures
#
# Estimand:
#   Dynamic difference in outcomes for high-access counties relative to
#   low-access counties, among counties that are ever exposed to an elite
#   school in the core school sample.
#
# Identification:
#   Event time is defined by first exposure to any elite school.
#   Low-access counties provide the baseline dynamic path.
#
# Outputs:
#   results/elite_school_event_studies/core_1800_panel_high_vs_low_county_only*/
#     elite_high_vs_low_county_treatment_core.csv
#     elite_high_vs_low_dynamic.csv
#     elite_high_vs_low_dynamic_cohort_trends.csv
#     elite_high_vs_low_summary.csv
#     elite_high_vs_low_raw_means.csv
#     elite_high_vs_low_event_study.png
#     elite_high_vs_low_event_study_cohort_trends.png
#     elite_high_vs_low_raw_means.png
#     elite_high_vs_low_summary.txt
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("tidyverse")
  library("fixest")
  library("ggplot2")
})

initial_time <- Sys.time()
theme_set(theme_minimal(base_size = 13))

window_years <- 60L
school_age <- as.integer(Sys.getenv("ELITE_SCHOOL_AGE", unset = "14"))
event_grid <- seq(-window_years, window_years, by = 10L)
results_root <- file.path("results", "elite_school_event_studies")
default_results_subdir <- if (school_age == 14L) {
  "core_1800_panel_high_vs_low_county_only"
} else {
  paste0("core_1800_panel_high_vs_low_county_only_age", school_age)
}
results_subdir <- Sys.getenv("ELITE_RESULTS_SUBDIR", unset = default_results_subdir)
results_dir <- file.path(results_root, results_subdir)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Helpers
###############################################################################

first_full_exposure_decade <- function(open_year, school_age = 14L) {
  as.integer(10 * ceiling((open_year - school_age) / 10))
}

build_ever_treated_panel <- function(panel, treatment_df) {
  first_panel_decade <- min(panel$decade, na.rm = TRUE)

  panel %>%
    left_join(treatment_df, by = c("GEOID" = "county_geoid")) %>%
    mutate(
      GEOID_num = as.numeric(GEOID),
      g = as.integer(g_any),
      access_group = case_when(
        first_exposure_access == "high" ~ "High access",
        first_exposure_access == "low" ~ "Low access",
        TRUE ~ NA_character_
      ),
      high_access = if_else(access_group == "High access", 1L, 0L, missing = 0L),
      rel_time = decade - g
    ) %>%
    filter(!is.na(g), g > first_panel_decade, !is.na(access_group)) %>%
    filter(rel_time %in% event_grid) %>%
    mutate(
      rel_time_f = factor(rel_time, levels = event_grid),
      access_group = factor(access_group, levels = c("Low access", "High access"))
    )
}

run_high_vs_low <- function(data, outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ i(rel_time_f, ref = '-10') + ",
      "i(rel_time_f, high_access, ref = '-10') | GEOID_num + decade"
    )
  )

  feols(fml, data = data, cluster = ~GEOID_num, warn = FALSE)
}

run_high_vs_low_trend <- function(data, outcome) {
  trend_origin <- min(data$decade, na.rm = TRUE)
  data_trend <- data %>%
    mutate(
      cohort = factor(g),
      trend = decade - trend_origin
    )

  fml <- as.formula(
    paste0(
      outcome,
      " ~ i(rel_time_f, ref = '-10') + ",
      "i(rel_time_f, high_access, ref = '-10') | ",
      "GEOID_num + decade + cohort[trend]"
    )
  )

  feols(fml, data = data_trend, cluster = ~GEOID_num, warn = FALSE)
}

tidy_high_vs_low <- function(model, design_label, outcome_label) {
  ct <- as.data.frame(coeftable(model)) %>%
    rownames_to_column("term") %>%
    as_tibble() %>%
    filter(str_detect(term, "high_access")) %>%
    transmute(
      design = design_label,
      outcome = outcome_label,
      event_time = as.integer(str_extract(term, "-?\\d+")),
      att = Estimate,
      se = `Std. Error`
    )

  ref_row <- tibble(
    design = design_label,
    outcome = outcome_label,
    event_time = -10L,
    att = 0,
    se = 0
  )

  bind_rows(ct, ref_row) %>%
    arrange(event_time) %>%
    mutate(
      conf_low = att - 1.96 * se,
      conf_high = att + 1.96 * se
    )
}

make_summary <- function(dynamic_tbl, panel_tbl) {
  support <- panel_tbl %>%
    summarise(
      sample_counties = n_distinct(GEOID),
      high_access_counties = n_distinct(GEOID[access_group == "High access"]),
      low_access_counties = n_distinct(GEOID[access_group == "Low access"]),
      first_cohort = min(g, na.rm = TRUE),
      last_cohort = max(g, na.rm = TRUE)
    )

  out <- dynamic_tbl %>%
    group_by(design, outcome) %>%
    summarise(
      lead_m60 = att[event_time == -60][1],
      se_m60 = se[event_time == -60][1],
      lead_m40 = att[event_time == -40][1],
      se_m40 = se[event_time == -40][1],
      lead_m20 = att[event_time == -20][1],
      se_m20 = se[event_time == -20][1],
      ref_m10 = att[event_time == -10][1],
      post_0 = att[event_time == 0][1],
      se_0 = se[event_time == 0][1],
      post_avg_0_60 = mean(att[event_time >= 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    bind_cols(support[rep(1, nrow(.)), ])

  out
}

make_raw_means <- function(data, design_label, outcome, outcome_label) {
  group_means <- data %>%
    group_by(rel_time, access_group) %>%
    summarise(
      mean_outcome = mean(.data[[outcome]], na.rm = TRUE),
      counties = n_distinct(GEOID),
      .groups = "drop"
    )

  wide <- group_means %>%
    select(rel_time, access_group, mean_outcome) %>%
    pivot_wider(names_from = access_group, values_from = mean_outcome) %>%
    mutate(raw_diff_high_minus_low = `High access` - `Low access`)

  group_means %>%
    mutate(
      design = design_label,
      outcome = outcome_label
    ) %>%
    left_join(
      wide %>% select(rel_time, raw_diff_high_minus_low),
      by = "rel_time"
    ) %>%
    rename(event_time = rel_time)
}

###############################################################################
# Load panel and school list
###############################################################################

panel <- read_csv("../prep/output/us_panel_county_stem_1800.csv", show_col_types = FALSE) %>%
  mutate(
    GEOID = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade = as.integer(decade)
  )

schools <- read_csv("../prep/output/elite_high_schools_core_1800_1930.csv",
                    show_col_types = FALSE) %>%
  mutate(
    county_geoid = str_pad(as.character(county_geoid), width = 5, side = "left", pad = "0"),
    founding_year_used = as.integer(founding_year_used),
    g_full = first_full_exposure_decade(founding_year_used, school_age)
  )

###############################################################################
# Treatment files
###############################################################################

county_treatment <- schools %>%
  group_by(county_geoid) %>%
  summarise(
    county_name = first(county_name),
    state_abbr = first(state_abbr),
    n_schools = n(),
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
    g_any = min(g_full, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(county_treatment, file.path(results_dir, "elite_high_vs_low_county_treatment_core.csv"))

###############################################################################
# Estimate differential event studies
###############################################################################

designs <- list(
  list(name = "County of school", treatment = county_treatment)
)

outcomes <- list(
  list(var = "stem_share_zero_pct", label = "STEM share zero-filled (pp)"),
  list(var = "stem_share_smooth_pct", label = "STEM share smoothed (pp)"),
  list(var = "n_stem", label = "STEM births (count)"),
  list(var = "any_stem_pct", label = "Any STEM birth (pp)")
)

dynamic_results <- list()
dynamic_trend_results <- list()
summary_results <- list()
raw_mean_results <- list()

for (design in designs) {
  design_panel <- build_ever_treated_panel(panel, design$treatment)

  for (outcome in outcomes) {
    outcome_panel <- design_panel %>%
      filter(!is.na(.data[[outcome$var]]))

    if (nrow(outcome_panel) == 0) {
      next
    }

    mod <- run_high_vs_low(outcome_panel, outcome$var)
    mod_trend <- run_high_vs_low_trend(outcome_panel, outcome$var)
    dyn <- tidy_high_vs_low(mod, design$name, outcome$label)
    dyn_trend <- tidy_high_vs_low(mod_trend, design$name, outcome$label) %>%
      mutate(estimator = "Differential ES + cohort linear trends", .before = 1)

    key <- paste(design$name, outcome$label, sep = " | ")
    dynamic_results[[key]] <- dyn
    dynamic_trend_results[[key]] <- dyn_trend
    summary_results[[key]] <- make_summary(dyn, outcome_panel)
    raw_mean_results[[key]] <- make_raw_means(outcome_panel, design$name, outcome$var, outcome$label)
  }
}

dynamic_tbl <- bind_rows(dynamic_results)
dynamic_trend_tbl <- bind_rows(dynamic_trend_results)
summary_tbl <- bind_rows(summary_results)
raw_means_tbl <- bind_rows(raw_mean_results)

write_csv(dynamic_tbl, file.path(results_dir, "elite_high_vs_low_dynamic.csv"))
write_csv(dynamic_trend_tbl, file.path(results_dir, "elite_high_vs_low_dynamic_cohort_trends.csv"))
write_csv(summary_tbl, file.path(results_dir, "elite_high_vs_low_summary.csv"))
write_csv(raw_means_tbl, file.path(results_dir, "elite_high_vs_low_raw_means.csv"))

###############################################################################
# Plots
###############################################################################

plot_dyn <- dynamic_tbl %>%
  ggplot(aes(x = event_time, y = att)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              fill = "#2a9d8f", alpha = 0.12, color = NA) +
  geom_line(color = "#023047", linewidth = 0.9) +
  geom_point(color = "#023047", size = 1.5) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = event_grid) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "High-access minus low-access ATT",
    title = "High-access versus low-access county-of-school event studies",
    subtitle = "Ever-treated counties only; low-access counties define the baseline path"
  )

plot_dyn_trend <- dynamic_trend_tbl %>%
  ggplot(aes(x = event_time, y = att)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              fill = "#e9c46a", alpha = 0.16, color = NA) +
  geom_line(color = "#8d5524", linewidth = 0.9) +
  geom_point(color = "#8d5524", size = 1.5) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = event_grid) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "High-access minus low-access ATT",
    title = "High-access versus low-access county-of-school event studies",
    subtitle = "Differential event study with cohort-specific linear trends"
  )

plot_raw <- raw_means_tbl %>%
  ggplot(aes(x = event_time, y = mean_outcome, color = access_group, linetype = access_group)) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = event_grid) +
  scale_color_manual(values = c(
    "Low access" = "#bc4749",
    "High access" = "#023047"
  )) +
  scale_linetype_manual(values = c(
    "Low access" = "dashed",
    "High access" = "solid"
  )) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "Raw mean outcome",
    color = NULL,
    linetype = NULL,
    title = "Raw means for high-access and low-access counties",
    subtitle = "Ever-treated counties only"
  )

ggsave(file.path(results_dir, "elite_high_vs_low_event_study.png"), plot_dyn,
       width = 8.5, height = 10, dpi = 300)
ggsave(file.path(results_dir, "elite_high_vs_low_event_study_cohort_trends.png"), plot_dyn_trend,
       width = 8.5, height = 10, dpi = 300)
ggsave(file.path(results_dir, "elite_high_vs_low_raw_means.png"), plot_raw,
       width = 8.5, height = 10, dpi = 300)

###############################################################################
# Text summary
###############################################################################

sink(file.path(results_dir, "elite_high_vs_low_summary.txt"))
cat("=== High-access versus low-access elite-school event studies ===\n\n")
cat("Panel decades:", min(panel$decade), "to", max(panel$decade), "\n")
cat("School sample: core elite-school list only.\n")
cat("Treatment timing: first fully exposed birth decade assuming school age ", school_age, ".\n", sep = "")
cat("Design: county-of-school treatment only.\n")
cat("Comparison group: low-access counties among ever-treated counties only.\n")
cat("Interpretation: coefficients are high-access minus low-access by event time,\n")
cat("relative to the event-time -10 decade.\n\n")

cat("=== Summary ===\n")
print(summary_tbl)

elapsed <- difftime(Sys.time(), initial_time, units = "mins")
cat("\nRuntime (minutes):", round(as.numeric(elapsed), 2), "\n")
sink()

cat("High-vs-low access event study complete. Outputs written to ", results_dir, ".\n", sep = "")
