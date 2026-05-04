###############################################################################
# Project: Determinants of Talent Production via Elite High Schools
# Goal: Compare estimators on the 1800+ panel and add raw-means event-time plots
#
# Estimators:
#   - Callaway & Sant'Anna (did::att_gt), doubly robust, not-yet-treated controls
#   - Borusyak, Jaravel, Spiess (didimputation::did_imputation)
#   - Wooldridge ETWFE (etwfe::etwfe + emfx), using never-treated reference
#
# Scope:
#   - Any exposure only
#   - County-of-school and 25 km designs
#   - Main outcomes:
#       n_stem
#       any_stem_pct
#       stem_share_zero_pct
#
# Outputs:
#   results/elite_school_event_studies/current_1800_panel_25km/
#     elite_school_1800_estimator_summary.csv
#     elite_school_1800_estimator_dynamic.csv
#     elite_school_1800_raw_means.csv
#     elite_school_1800_estimator_notes.txt
#     elite_school_1800_estimator_compare.png
#     elite_school_1800_raw_means.png
#     elite_school_1800_raw_means_diff.png
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("tidyverse")
  library("did")
  library("didimputation")
  library("etwfe")
  library("ggplot2")
})

initial_time <- Sys.time()
theme_set(theme_minimal(base_size = 13))

window_years <- 60L
event_grid <- seq(-60L, 60L, by = 10L)
post_grid <- event_grid[event_grid >= 0]
pre_grid <- event_grid[event_grid < 0]
radius_label <- "25 km radius"
results_root <- file.path("results", "elite_school_event_studies")
results_dir <- file.path(results_root, "current_1800_panel_25km")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Helpers
###############################################################################

build_any_panel <- function(panel, treatment_df) {
  first_panel_decade <- min(panel$decade, na.rm = TRUE)

  panel %>%
    left_join(treatment_df, by = c("GEOID" = "county_geoid")) %>%
    mutate(
      GEOID_num = as.numeric(GEOID),
      g = case_when(
        is.na(g_any) ~ 0L,
        g_any <= first_panel_decade ~ NA_integer_,
        TRUE ~ as.integer(g_any)
      )
    ) %>%
    filter(!is.na(g), g == 0L | g > first_panel_decade)
}

run_cs_dynamic <- function(data, outcome, design_label, outcome_label) {
  out <- suppressWarnings(
    att_gt(
      yname = outcome,
      tname = "decade",
      idname = "GEOID_num",
      gname = "g",
      data = data,
      control_group = "notyettreated",
      est_method = "dr",
      base_period = "universal",
      cores = 4
    )
  )

  dyn <- suppressWarnings(
    aggte(out, type = "dynamic", na.rm = TRUE,
          min_e = -window_years, max_e = window_years)
  )

  tibble(
    design = design_label,
    outcome = outcome_label,
    estimator = "Callaway-Sant'Anna DR",
    event_time = as.integer(dyn$egt),
    att = dyn$att.egt,
    se = dyn$se.egt
  ) %>%
    filter(event_time %in% event_grid, !is.na(att), !is.na(se))
}

run_bjs_dynamic <- function(data, outcome, design_label, outcome_label) {
  out <- suppressWarnings(
    did_imputation(
      data = data,
      yname = outcome,
      gname = "g",
      tname = "decade",
      idname = "GEOID_num",
      horizon = post_grid,
      pretrends = pre_grid,
      cluster_var = "GEOID_num"
    )
  )

  tibble(
    design = design_label,
    outcome = outcome_label,
    estimator = "BJS imputation",
    event_time = as.integer(out$term),
    att = out$estimate,
    se = out$std.error
  ) %>%
    filter(event_time %in% event_grid)
}

run_bjs_trend_dynamic <- function(data, outcome, design_label, outcome_label) {
  trend_origin <- min(data$decade, na.rm = TRUE)
  data_trend <- data %>%
    mutate(
      cohort = factor(g),
      trend = decade - trend_origin
    )

  first_stage_fml <- ~ 0 | GEOID_num + decade + cohort[trend]

  out <- suppressWarnings(
    did_imputation(
      data = data_trend,
      yname = outcome,
      gname = "g",
      tname = "decade",
      idname = "GEOID_num",
      horizon = post_grid,
      pretrends = pre_grid,
      first_stage = first_stage_fml,
      cluster_var = "GEOID_num"
    )
  )

  tibble(
    design = design_label,
    outcome = outcome_label,
    estimator = "BJS + cohort linear trends",
    event_time = as.integer(out$term),
    att = out$estimate,
    se = out$std.error
  ) %>%
    filter(event_time %in% event_grid)
}

run_etwfe_dynamic <- function(data, outcome, design_label, outcome_label) {
  mod <- suppressWarnings(
    etwfe(
      fml = as.formula(paste0(outcome, " ~ 1")),
      tvar = decade,
      gvar = g,
      data = data,
      ivar = GEOID_num,
      cgroup = "never",
      gref = 0
    )
  )

  fx <- suppressWarnings(
    emfx(mod, type = "event", window = c(window_years, window_years),
         post_only = FALSE)
  )

  tibble(as.data.frame(fx)) %>%
    transmute(
      design = design_label,
      outcome = outcome_label,
      estimator = "Wooldridge ETWFE",
      event_time = as.integer(event),
      att = estimate,
      se = std.error
    ) %>%
    filter(event_time %in% event_grid)
}

ensure_reference_zero <- function(dynamic_tbl, ref_event = -10L) {
  refs <- dynamic_tbl %>%
    distinct(design, outcome, estimator) %>%
    anti_join(
      dynamic_tbl %>%
        filter(event_time == ref_event) %>%
        distinct(design, outcome, estimator),
      by = c("design", "outcome", "estimator")
    ) %>%
    mutate(
      event_time = ref_event,
      att = 0,
      se = 0
    )

  bind_rows(dynamic_tbl, refs) %>%
    arrange(design, outcome, estimator, event_time)
}

make_summary <- function(dynamic_tbl) {
  dynamic_tbl %>%
    group_by(design, outcome, estimator) %>%
    summarise(
      lead_m60 = att[event_time == -60][1],
      lead_m40 = att[event_time == -40][1],
      lead_m20 = att[event_time == -20][1],
      post_0 = att[event_time == 0][1],
      post_avg_0_60 = mean(att[event_time %in% post_grid], na.rm = TRUE),
      .groups = "drop"
    )
}

make_raw_means <- function(data, outcome, design_label, outcome_label) {
  control_means <- data %>%
    group_by(decade) %>%
    summarise(
      control_mean = mean(.data[[outcome]][g == 0L | g > decade], na.rm = TRUE),
      control_n = sum(g == 0L | g > decade),
      .groups = "drop"
    )

  data %>%
    filter(g > 0L) %>%
    mutate(event_time = decade - g) %>%
    filter(event_time %in% event_grid) %>%
    left_join(control_means, by = "decade") %>%
    group_by(event_time) %>%
    summarise(
      treated_mean = mean(.data[[outcome]], na.rm = TRUE),
      control_mean = mean(control_mean, na.rm = TRUE),
      raw_diff = treated_mean - control_mean,
      treated_obs = n(),
      mean_control_n = mean(control_n, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      design = design_label,
      outcome = outcome_label
    ) %>%
    select(design, outcome, event_time, treated_mean, control_mean,
           raw_diff, treated_obs, mean_control_n)
}

###############################################################################
# Load data
###############################################################################

panel <- read_csv("../prep/output/us_panel_county_stem_1800.csv", show_col_types = FALSE) %>%
  mutate(
    GEOID = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade = as.integer(decade)
  )

county_treatment <- read_csv(
  file.path(results_dir, "elite_county_1800_treatment.csv"),
  show_col_types = FALSE
)
radius_treatment <- read_csv(
  file.path(results_dir, "elite_radius25_1800_treatment.csv"),
  show_col_types = FALSE
)

designs <- list(
  list(name = "County of school", treatment = county_treatment),
  list(name = radius_label, treatment = radius_treatment)
)

outcomes <- list(
  list(var = "n_stem", label = "STEM births (count)"),
  list(var = "any_stem_pct", label = "Any STEM birth (pp)"),
  list(var = "stem_share_zero_pct", label = "STEM share zero-filled (pp)")
)

###############################################################################
# Estimate and collect
###############################################################################

dynamic_results <- list()
raw_mean_results <- list()

for (design in designs) {
  d <- build_any_panel(panel, design$treatment)

  for (outcome in outcomes) {
    key <- paste(design$name, outcome$label, sep = " | ")

    dynamic_results[[paste(key, "CS", sep = " | ")]] <-
      run_cs_dynamic(d, outcome$var, design$name, outcome$label)

    dynamic_results[[paste(key, "BJS", sep = " | ")]] <-
      run_bjs_dynamic(d, outcome$var, design$name, outcome$label)

    dynamic_results[[paste(key, "BJS trends", sep = " | ")]] <-
      run_bjs_trend_dynamic(d, outcome$var, design$name, outcome$label)

    dynamic_results[[paste(key, "ETWFE", sep = " | ")]] <-
      run_etwfe_dynamic(d, outcome$var, design$name, outcome$label)

    raw_mean_results[[key]] <-
      make_raw_means(d, outcome$var, design$name, outcome$label)
  }
}

dynamic_tbl <- bind_rows(dynamic_results) %>%
  ensure_reference_zero(ref_event = -10L) %>%
  mutate(
    conf_low = att - 1.96 * se,
    conf_high = att + 1.96 * se
  )

summary_tbl <- make_summary(dynamic_tbl)
raw_means_tbl <- bind_rows(raw_mean_results)

write_csv(dynamic_tbl, file.path(results_dir, "elite_school_1800_estimator_dynamic.csv"))
write_csv(summary_tbl, file.path(results_dir, "elite_school_1800_estimator_summary.csv"))
write_csv(raw_means_tbl, file.path(results_dir, "elite_school_1800_raw_means.csv"))

###############################################################################
# Plots
###############################################################################

plot_estimators <- dynamic_tbl %>%
  filter(design == "County of school") %>%
  ggplot(aes(x = event_time, y = att, color = estimator, fill = estimator)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = event_grid) +
  scale_color_manual(values = c(
    "Callaway-Sant'Anna DR" = "#023047",
    "BJS imputation" = "#bc4749",
    "BJS + cohort linear trends" = "#8d5524",
    "Wooldridge ETWFE" = "#2a9d8f"
  )) +
  scale_fill_manual(values = c(
    "Callaway-Sant'Anna DR" = "#023047",
    "BJS imputation" = "#bc4749",
    "BJS + cohort linear trends" = "#8d5524",
    "Wooldridge ETWFE" = "#2a9d8f"
  )) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "ATT",
    color = NULL,
    fill = NULL,
    title = "Estimator comparison: county-of-school design",
    subtitle = "1800+ panel, any exposure only; when the -10 decade is normalized out it is shown at zero"
  )

raw_means_levels <- raw_means_tbl %>%
  pivot_longer(
    cols = c(treated_mean, control_mean),
    names_to = "series",
    values_to = "mean_value"
  ) %>%
  mutate(
    series = recode(
      series,
      treated_mean = "Treated mean",
      control_mean = "Control mean"
    )
  )

plot_raw_levels <- raw_means_levels %>%
  ggplot(aes(x = event_time, y = mean_value, color = series, linetype = series)) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_grid(outcome ~ design, scales = "free_y") +
  scale_x_continuous(breaks = event_grid) +
  scale_color_manual(values = c(
    "Treated mean" = "#023047",
    "Control mean" = "#bc4749"
  )) +
  scale_linetype_manual(values = c(
    "Treated mean" = "solid",
    "Control mean" = "dashed"
  )) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "Mean outcome level",
    color = NULL,
    linetype = NULL,
    title = "Raw means event-time levels",
    subtitle = "Treated and same-decade never-treated or not-yet-treated controls"
  )

plot_raw_diff <- raw_means_tbl %>%
  ggplot(aes(x = event_time, y = raw_diff, color = design)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = event_grid) +
  scale_color_manual(values = c(
    "County of school" = "#023047",
    "25 km radius" = "#bc4749"
  )) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "Raw treated minus control mean",
    color = NULL,
    title = "Raw means event-time benchmark",
    subtitle = "Controls are never-treated or not-yet-treated counties within the same calendar decade"
  )

ggsave(file.path(results_dir, "elite_school_1800_estimator_compare.png"), plot_estimators,
       width = 8.5, height = 10, dpi = 300)
ggsave(file.path(results_dir, "elite_school_1800_raw_means.png"), plot_raw_levels,
       width = 11, height = 10, dpi = 300)
ggsave(file.path(results_dir, "elite_school_1800_raw_means_diff.png"), plot_raw_diff,
       width = 8.5, height = 10, dpi = 300)

###############################################################################
# Notes
###############################################################################

sink(file.path(results_dir, "elite_school_1800_estimator_notes.txt"))
cat("=== Estimator comparison on the 1800+ panel ===\n\n")
cat("Current main estimator in analysis_elite_school_1800.R:\n")
cat("- did::att_gt with est_method = 'dr', base_period = 'universal',\n")
cat("  and not-yet-treated controls.\n\n")

cat("Alternative estimators added here:\n")
cat("- BJS imputation via didimputation::did_imputation\n")
cat("- BJS imputation with cohort-specific linear first-stage trends\n")
cat("- Wooldridge ETWFE via etwfe::etwfe + emfx\n\n")

cat("Important comparability note:\n")
cat("- Callaway-Sant'Anna and both BJS specifications are using not-yet-treated logic.\n")
cat("- The Wooldridge implementation here uses never-treated counties as the\n")
cat("  reference group (gref = 0) to recover a full event-time path.\n\n")
cat("- To align event-time reporting with the universal-base CS output, the\n")
cat("  normalized -10 decade is shown explicitly at zero if an estimator\n")
cat("  does not return it.\n\n")

cat("=== Summary table ===\n")
print(summary_tbl)

cat("\nRuntime (minutes):", round(as.numeric(difftime(Sys.time(), initial_time, units = 'mins')), 2), "\n")
sink()

cat("Estimator comparison complete. Outputs written to ", results_dir, ".\n", sep = "")
