###############################################################################
# Project: Determinants of Talent Production via Elite High Schools
# Goal: Event studies on the new 1800+ county birth panel
#
# Main timing correction:
#   Treatment is assigned to the first fully exposed birth decade assuming
#   secondary-school exposure begins at a configurable entry age.
#
# Design:
#   County-of-school treatment only
#
# Outcomes:
#   - stem_share_zero_pct
#   - stem_share_smooth_pct
#   - n_stem
#   - any_stem_pct
#
# Outputs:
#   results/elite_school_event_studies/core_1800_panel_county_only*/
#     elite_county_1800_treatment_core.csv
#     elite_school_1800_att_summary.csv
#     elite_school_1800_pretrend_leads.csv
#     elite_school_1800_dynamic.csv
#     elite_school_1800_cohort_trend_dynamic.csv
#     elite_school_1800_cohort_trend_summary.csv
#     elite_school_1800_summary.txt
#     elite_school_1800_main_dynamics.png
#     elite_school_1800_control_group_compare.png
#     elite_school_1800_main_dynamics_cohort_trends.png
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("tidyverse")
  library("did")
  library("didimputation")
  library("ggplot2")
})

initial_time <- Sys.time()
theme_set(theme_minimal(base_size = 13))

window_years <- 60L
school_age <- as.integer(Sys.getenv("ELITE_SCHOOL_AGE", unset = "14"))
results_root <- file.path("results", "elite_school_event_studies")
default_results_subdir <- if (school_age == 14L) {
  "core_1800_panel_county_only"
} else {
  paste0("core_1800_panel_county_only_age", school_age)
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

add_group_trend_controls <- function(data, g_var = "g", trend_var = "trend",
                                     prefix = "trend_g_") {
  positive_groups <- sort(unique(data[[g_var]][data[[g_var]] > 0L]))
  data_out <- data
  trend_cols <- character(length(positive_groups))

  for (i in seq_along(positive_groups)) {
    group_value <- positive_groups[[i]]
    col_name <- paste0(prefix, group_value)
    data_out[[col_name]] <- ifelse(data_out[[g_var]] == group_value,
                                   data_out[[trend_var]], 0)
    trend_cols[[i]] <- col_name
  }

  list(data = data_out, trend_cols = trend_cols)
}

run_att <- function(data, outcome, control_group) {
  suppressWarnings(
    att_gt(
      yname = outcome,
      tname = "decade",
      idname = "GEOID_num",
      gname = "g",
      data = data,
      control_group = control_group,
      est_method = "dr",
      base_period = "universal",
      cores = 4
    )
  )
}

tidy_simple <- function(att_obj, data, design_label, spec_label, outcome_label) {
  agg <- suppressWarnings(aggte(att_obj, type = "simple", na.rm = TRUE))
  tibble(
    design = design_label,
    spec = spec_label,
    outcome = outcome_label,
    sample_counties = n_distinct(data$GEOID),
    treated_counties = n_distinct(data$GEOID[data$g > 0]),
    never_treated_counties = n_distinct(data$GEOID[data$g == 0]),
    first_cohort = min(data$g[data$g > 0], na.rm = TRUE),
    last_cohort = max(data$g[data$g > 0], na.rm = TRUE),
    overall_att = agg$overall.att,
    overall_se = agg$overall.se,
    p_value = 2 * (1 - pnorm(abs(agg$overall.att / agg$overall.se)))
  )
}

tidy_dynamic <- function(att_obj, design_label, spec_label, outcome_label,
                         window = window_years) {
  dyn <- suppressWarnings(
    aggte(att_obj, type = "dynamic", na.rm = TRUE,
          min_e = -window, max_e = window)
  )

  tibble(
    design = design_label,
    spec = spec_label,
    outcome = outcome_label,
    event_time = dyn$egt,
    att = dyn$att.egt,
    se = dyn$se.egt
  ) %>%
    filter(!is.na(att), !is.na(se)) %>%
    mutate(
      conf_low = att - 1.96 * se,
      conf_high = att + 1.96 * se
    )
}

run_bjs_trend_dynamic <- function(data, outcome, design_label, spec_label,
                                  outcome_label) {
  event_grid <- seq(-window_years, window_years, by = 10L)
  post_grid <- event_grid[event_grid >= 0]
  pre_grid <- event_grid[event_grid < 0]
  trend_origin <- min(data$decade, na.rm = TRUE)

  data_trend <- data %>%
    mutate(
      cohort = factor(g),
      trend = decade - trend_origin
    )

  trend_setup <- add_group_trend_controls(data_trend)
  data_trend <- trend_setup$data
  first_stage_rhs <- paste(c("0", trend_setup$trend_cols), collapse = " + ")
  first_stage_fml <- as.formula(
    paste0("~ ", first_stage_rhs, " | GEOID_num + decade")
  )

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
    spec = spec_label,
    outcome = outcome_label,
    event_time = as.integer(out$term),
    att = out$estimate,
    se = out$std.error
  ) %>%
    filter(event_time %in% event_grid) %>%
    mutate(
      conf_low = att - 1.96 * se,
      conf_high = att + 1.96 * se
    )
}

build_spec_panel <- function(panel, treatment_df, access = NULL) {
  data <- panel %>%
    left_join(treatment_df, by = c("GEOID" = "county_geoid")) %>%
    mutate(GEOID_num = as.numeric(GEOID))

  if (!is.null(access)) {
    data <- data %>%
      filter(is.na(first_exposure_access) | first_exposure_access == access)
  }

  first_panel_decade <- min(data$decade, na.rm = TRUE)

  data %>%
    mutate(
      g = case_when(
        is.na(g_any) ~ 0L,
        g_any <= first_panel_decade ~ NA_integer_,
        TRUE ~ g_any
      )
    ) %>%
    filter(!is.na(g))
}

restrict_to_ever_treated <- function(data) {
  data %>%
    filter(g > 0L)
}

###############################################################################
# Load panel and school list
###############################################################################

panel <- read_csv("../prep/output/us_panel_county_stem_1800.csv", show_col_types = FALSE) %>%
  mutate(
    GEOID = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade = as.integer(decade),
    log1p_population = log1p(population)
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

write_csv(county_treatment, file.path(results_dir, "elite_county_1800_treatment_core.csv"))

###############################################################################
# Estimate all designs x specs x outcomes
###############################################################################

designs <- list(
  list(name = "County of school", treatment = county_treatment)
)

specs <- list(
  list(name = "Any exposure", access = NULL),
  list(name = "High-access first exposure", access = "high"),
  list(name = "Low-access first exposure", access = "low")
)

control_setups <- list(
  list(
    name = "Never-treated only",
    control_group = "nevertreated",
    ever_treated_only = FALSE
  ),
  list(
    name = "Not-yet + never-treated",
    control_group = "notyettreated",
    ever_treated_only = FALSE
  ),
  list(
    name = "Strict not-yet-treated only",
    control_group = "notyettreated",
    ever_treated_only = TRUE
  )
)

outcomes <- list(
  list(var = "stem_share_zero_pct", label = "STEM share zero-filled (pp)"),
  list(var = "stem_share_smooth_pct", label = "STEM share smoothed (pp)"),
  list(var = "n_stem", label = "STEM births (count)"),
  list(var = "any_stem_pct", label = "Any STEM birth (pp)"),
  list(var = "log1p_population", label = "County population (log1p)")
)

results <- list()
cohort_trend_results <- list()

for (control_setup in control_setups) {
  control_name <- control_setup$name
  control_group_name <- control_setup$control_group

  for (design in designs) {
    for (spec in specs) {
      spec_panel <- build_spec_panel(panel, design$treatment, spec$access)

      if (control_setup$ever_treated_only) {
        spec_panel <- restrict_to_ever_treated(spec_panel)
      }

      if (nrow(spec_panel) == 0) {
        next
      }

      for (outcome in outcomes) {
        outcome_panel <- spec_panel %>%
          filter(!is.na(.data[[outcome$var]]))

        if (nrow(outcome_panel) == 0) {
          next
        }

        att_obj <- tryCatch(
          run_att(outcome_panel, outcome$var, control_group_name),
          error = function(e) NULL
        )

        if (is.null(att_obj)) {
          next
        }

        key <- paste(control_name, design$name, spec$name, outcome$label, sep = " | ")
        results[[key]] <- list(
          summary = tidy_simple(att_obj, outcome_panel, design$name, spec$name, outcome$label) %>%
            mutate(
              control_setup = control_name,
              control_group = control_group_name,
              school_sample = "core",
              .before = 1
            ),
          dynamic = tidy_dynamic(att_obj, design$name, spec$name, outcome$label) %>%
            mutate(
              control_setup = control_name,
              control_group = control_group_name,
              school_sample = "core",
              .before = 1
            )
        )

        if (control_name == "Not-yet + never-treated") {
          trend_key <- paste(design$name, spec$name, outcome$label, sep = " | ")
          trend_dyn <- tryCatch(
            run_bjs_trend_dynamic(
              outcome_panel,
              outcome$var,
              design$name,
              spec$name,
              outcome$label
            ),
            error = function(e) NULL
          )

          if (!is.null(trend_dyn)) {
            cohort_trend_results[[trend_key]] <- trend_dyn %>%
              mutate(
                school_sample = "core",
                estimator = "BJS + cohort linear trends",
                .before = 1
              )
          }
        }
      }
    }
  }
}

att_summary <- bind_rows(lapply(results, `[[`, "summary"))
dynamic_tbl <- bind_rows(lapply(results, `[[`, "dynamic"))
cohort_trend_dynamic <- bind_rows(cohort_trend_results)

pretrend_leads <- dynamic_tbl %>%
  filter(event_time < 0) %>%
  group_by(school_sample, control_setup, control_group, design, spec, outcome) %>%
  summarise(
    lead_m60 = att[event_time == -60][1],
    se_m60 = se[event_time == -60][1],
    lead_m40 = att[event_time == -40][1],
    se_m40 = se[event_time == -40][1],
    lead_m20 = att[event_time == -20][1],
    se_m20 = se[event_time == -20][1],
    .groups = "drop"
  )

cohort_trend_summary <- cohort_trend_dynamic %>%
  filter(event_time < 0 | event_time >= 0) %>%
  group_by(school_sample, estimator, design, spec, outcome) %>%
  summarise(
    lead_m60 = att[event_time == -60][1],
    se_m60 = se[event_time == -60][1],
    lead_m40 = att[event_time == -40][1],
    se_m40 = se[event_time == -40][1],
    lead_m20 = att[event_time == -20][1],
    se_m20 = se[event_time == -20][1],
    post_0 = att[event_time == 0][1],
    se_0 = se[event_time == 0][1],
    post_avg_0_60 = mean(att[event_time >= 0 & event_time <= window_years], na.rm = TRUE),
    .groups = "drop"
  )

write_csv(att_summary, file.path(results_dir, "elite_school_1800_att_summary.csv"))
write_csv(dynamic_tbl, file.path(results_dir, "elite_school_1800_dynamic.csv"))
write_csv(pretrend_leads, file.path(results_dir, "elite_school_1800_pretrend_leads.csv"))
write_csv(cohort_trend_dynamic, file.path(results_dir, "elite_school_1800_cohort_trend_dynamic.csv"))
write_csv(cohort_trend_summary, file.path(results_dir, "elite_school_1800_cohort_trend_summary.csv"))

###############################################################################
# Plots
###############################################################################

plot_main <- dynamic_tbl %>%
  filter(
    control_setup == "Not-yet + never-treated",
    design == "County of school",
    outcome == "STEM share zero-filled (pp)",
    spec %in% c("Any exposure", "High-access first exposure", "Low-access first exposure")
  ) %>%
  ggplot(aes(x = event_time, y = att, color = spec, fill = spec)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.4) +
  scale_x_continuous(breaks = seq(-60, 60, by = 10)) +
  scale_color_manual(values = c(
    "Any exposure" = "#023047",
    "High-access first exposure" = "#2a9d8f",
    "Low-access first exposure" = "#bc4749"
  )) +
  scale_fill_manual(values = c(
    "Any exposure" = "#023047",
    "High-access first exposure" = "#2a9d8f",
    "Low-access first exposure" = "#bc4749"
  )) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "ATT on STEM share (pp)",
    color = NULL,
    fill = NULL,
    title = "County-of-school event study on county STEM share",
    subtitle = "Main CS spec: not-yet-treated plus never-treated controls"
  )

ggsave(file.path(results_dir, "elite_school_1800_main_dynamics.png"), plot_main,
       width = 8.5, height = 5.75, dpi = 300)

plot_main_trends <- cohort_trend_dynamic %>%
  filter(
    design == "County of school",
    outcome == "STEM share zero-filled (pp)",
    spec %in% c("Any exposure", "High-access first exposure", "Low-access first exposure")
  ) %>%
  ggplot(aes(x = event_time, y = att, color = spec, fill = spec)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.4) +
  scale_x_continuous(breaks = seq(-60, 60, by = 10)) +
  scale_color_manual(values = c(
    "Any exposure" = "#023047",
    "High-access first exposure" = "#2a9d8f",
    "Low-access first exposure" = "#bc4749"
  )) +
  scale_fill_manual(values = c(
    "Any exposure" = "#023047",
    "High-access first exposure" = "#2a9d8f",
    "Low-access first exposure" = "#bc4749"
  )) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "ATT on STEM share (pp)",
    color = NULL,
    fill = NULL,
    title = "County-of-school event study on county STEM share",
    subtitle = "BJS imputation with cohort-specific linear trends"
  )

ggsave(file.path(results_dir, "elite_school_1800_main_dynamics_cohort_trends.png"),
       plot_main_trends, width = 8.5, height = 5.75, dpi = 300)

plot_controls <- dynamic_tbl %>%
  filter(
    design == "County of school",
    spec == "Any exposure",
    outcome %in% c("STEM births (count)", "County population (log1p)")
  ) %>%
  ggplot(aes(x = event_time, y = att, color = control_setup, fill = control_setup)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.3) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(-60, 60, by = 10)) +
  scale_color_manual(values = c(
    "Never-treated only" = "#bc4749",
    "Not-yet + never-treated" = "#023047",
    "Strict not-yet-treated only" = "#2a9d8f"
  )) +
  scale_fill_manual(values = c(
    "Never-treated only" = "#bc4749",
    "Not-yet + never-treated" = "#023047",
    "Strict not-yet-treated only" = "#2a9d8f"
  )) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "ATT",
    color = NULL,
    fill = NULL,
    title = "Control-group comparison for county-of-school event studies",
    subtitle = "Any-exposure dynamics for STEM births and county population"
  )

ggsave(file.path(results_dir, "elite_school_1800_control_group_compare.png"), plot_controls,
       width = 8.5, height = 8.5, dpi = 300)

plot_any_trends <- cohort_trend_dynamic %>%
  filter(
    design == "County of school",
    spec == "Any exposure",
    outcome %in% c("STEM births (count)", "County population (log1p)")
  ) %>%
  ggplot(aes(x = event_time, y = att)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              fill = "#2a9d8f", alpha = 0.12, color = NA) +
  geom_line(color = "#023047", linewidth = 0.9) +
  geom_point(color = "#023047", size = 1.5) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(-60, 60, by = 10)) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "ATT",
    title = "County-of-school event studies with cohort-specific linear trends",
    subtitle = "Any exposure: STEM births and county population"
  )

ggsave(file.path(results_dir, "elite_school_1800_any_exposure_cohort_trends.png"),
       plot_any_trends, width = 8.5, height = 8.5, dpi = 300)

###############################################################################
# Text summary
###############################################################################

support_tbl <- att_summary %>%
  distinct(
    school_sample, control_setup, control_group, design, spec,
    sample_counties, treated_counties, never_treated_counties,
    first_cohort, last_cohort
  ) %>%
  arrange(control_setup, design, spec)

sink(file.path(results_dir, "elite_school_1800_summary.txt"))
cat("=== Elite-school event studies on the 1800+ county panel ===\n\n")
cat("Panel decades:", min(panel$decade), "to", max(panel$decade), "\n")
cat("Treatment timing: first fully exposed birth decade assuming school age ", school_age, ".\n", sep = "")
cat("Design: county-of-school treatment only.\n")
cat("School treatment sample: core elite-school list only.\n")
cat("Main outcomes: zero-filled share, smoothed share, STEM count, extensive margin,\n")
cat("and county population on the log1p scale.\n\n")
cat("Control setups:\n")
cat("- Never-treated only: att_gt(control_group = 'nevertreated') on the full county sample.\n")
cat("- Not-yet + never-treated: att_gt(control_group = 'notyettreated') on the full county sample.\n")
cat("- Strict not-yet-treated only: att_gt(control_group = 'notyettreated') after dropping never-treated counties.\n\n")
cat("Trend-adjusted alternative:\n")
cat("- BJS imputation with cohort-specific linear trends in the first stage.\n\n")

cat("=== Support by design and spec ===\n")
print(support_tbl)

cat("\n=== ATT summary ===\n")
print(att_summary)

cat("\n=== Pre-period leads ===\n")
print(pretrend_leads)

cat("\n=== Cohort-linear-trend event-study summary ===\n")
print(cohort_trend_summary)

elapsed <- difftime(Sys.time(), initial_time, units = "mins")
cat("\nRuntime (minutes):", round(as.numeric(elapsed), 2), "\n")
sink()

cat("Elite-school 1800+ analysis complete. Outputs written to ", results_dir, ".\n", sep = "")
