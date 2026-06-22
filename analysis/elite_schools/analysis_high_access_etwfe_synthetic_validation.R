###############################################################################
# Synthetic-data validation for the high-access ETWFE estimators.
#
# Goal: confirm the production helpers in etwfe_high_access_helpers.R recover
# a known dynamic ATT under two scenarios.
#
# Scenarios
#   1. No selection on slope.
#      Treated and control units share the same distribution of
#      county-specific linear trends. Plain ETWFE and the detrended
#      ETWFE are both expected to recover the truth.
#   2. Selection on slope.
#      High-access counties have a strictly higher mean trend than
#      controls. Plain ETWFE is expected to be biased; the detrended
#      ETWFE must still recover the truth.
#
# DGP
#   y_it = alpha_i + lambda_t + beta_i * t_dec + tau(e_it) + eps_it
#   where t_dec = (decade - 1800) / 10, e_it = decade - g_i for
#   treated cohorts, tau(e) = 0 for e < 0 and tau(e) = 1 + 0.05 * e
#   for e >= 0. Low-access cohorts get tau == 0 (clean control).
#
# Pass / fail
#   The detrended estimator must satisfy max |estimate - truth| <= 0.20
#   at all event times in [-60, 60] in BOTH scenarios. Plain ETWFE must
#   satisfy the same bound only in Scenario 1.
###############################################################################

rm(list = ls())

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repo_root <- if (basename(cwd) == "analysis") dirname(cwd) else if (basename(dirname(cwd)) == "analysis") dirname(dirname(cwd)) else cwd
}
source(file.path(repo_root, "analysis", "elite_schools", "etwfe_high_access_helpers.R"))
source(file.path(repo_root, "paths.R"))
suppressPackageStartupMessages(library("ggplot2"))
set.seed(2026L)

theme_set(theme_minimal(base_size = 13))

window_years <- 60L
event_grid   <- seq(-window_years, window_years, by = 10L)

results_dir <- file.path(
  TALENT_DETS_DATA_DIR, "results", "elite_schools", "elite_school_event_studies",
  "synthetic_high_access_validation"
)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Truth and DGP
###############################################################################

tau_intercept <- 1.0
tau_slope     <- 0.05

true_tau <- function(event_time) {
  if_else(event_time < 0, 0, tau_intercept + tau_slope * event_time)
}

simulate_panel <- function(
    n_high     = 500L,
    n_low      = 1000L,
    n_never    = 4000L,
    decades    = seq(1800L, 2000L, by = 10L),
    cohort_pool = c(1830L, 1850L, 1870L, 1890L, 1910L),
    alpha_sd   = 2.0,
    beta_mu_high  = 0.0,
    beta_mu_other = 0.0,
    beta_sd    = 0.05,
    lambda_intercept = 0.5,
    lambda_slope     = 0.01,
    sigma_eps  = 0.5
) {
  total_n <- n_high + n_low + n_never
  group   <- c(rep("high", n_high), rep("low", n_low), rep("never", n_never))

  alpha_i <- rnorm(total_n, 0, alpha_sd)
  beta_mu <- ifelse(group == "high", beta_mu_high, beta_mu_other)
  beta_i  <- rnorm(total_n, beta_mu, beta_sd)

  cohort_i <- rep(0L, total_n)
  cohort_i[group == "high"] <- sample(cohort_pool, n_high, replace = TRUE)
  cohort_i[group == "low"]  <- sample(cohort_pool, n_low,  replace = TRUE)

  decade_min <- min(decades)

  expand_grid(county = seq_len(total_n), decade = decades) %>%
    arrange(county, decade) %>%
    mutate(
      group     = group[county],
      alpha     = alpha_i[county],
      beta      = beta_i[county],
      g_unit    = cohort_i[county],
      t_dec     = (decade - decade_min) / 10,
      lambda    = lambda_intercept + lambda_slope * (decade - 1900),
      eps       = rnorm(n(), 0, sigma_eps),
      tau_truth = case_when(
        group == "high" & decade >= g_unit ~ tau_intercept + tau_slope * (decade - g_unit),
        TRUE                                ~ 0
      ),
      y = alpha + lambda + beta * t_dec + tau_truth + eps,
      GEOID                  = sprintf("%05d", county),
      first_exposure_access  = case_when(
        group == "high" ~ "high",
        group == "low"  ~ "low",
        TRUE             ~ NA_character_
      ),
      g_any = if_else(g_unit == 0L, NA_integer_, g_unit)
    )
}

###############################################################################
# Run one scenario
###############################################################################

run_scenario <- function(scenario_name, beta_mu_high) {
  cat("\n==== Scenario:", scenario_name,
      "(beta_mu_high =", beta_mu_high, ") ====\n")

  panel_full <- simulate_panel(beta_mu_high = beta_mu_high)

  treatment_tbl <- panel_full %>%
    distinct(GEOID, first_exposure_access, g_any) %>%
    rename(county_geoid = GEOID) %>%
    filter(!is.na(first_exposure_access))

  panel_for_helpers <- panel_full %>%
    select(GEOID, decade, y) %>%
    distinct()

  specs <- list(
    list(label   = "Spec A: high vs all others",
         short   = "spec_A",
         builder = build_panel_spec_A),
    list(label   = "Spec B: high vs low-access only",
         short   = "spec_B",
         builder = build_panel_spec_B)
  )

  dyn_list <- list()

  for (spec in specs) {
    spec_panel <- spec$builder(panel_for_helpers, treatment_tbl)

    dyn_list[[paste(spec$short, "plain")]] <-
      compute_dynamic(
        spec_panel, "y", spec$label, "y (synthetic)",
        "Wooldridge ETWFE",
        window_years, event_grid, detrend = FALSE
      ) %>%
      mutate(scenario = scenario_name, .before = 1)

    dyn_list[[paste(spec$short, "trend")]] <-
      compute_dynamic(
        spec_panel, "y", spec$label, "y (synthetic)",
        "Wooldridge ETWFE + county pre-period trend",
        window_years, event_grid, detrend = TRUE
      ) %>%
      mutate(scenario = scenario_name, .before = 1)
  }

  bind_rows(dyn_list)
}

scenarios <- list(
  list(name = "no_selection_on_slope",
       beta_mu_high = 0.0,
       require_plain_pass  = TRUE,
       require_detrend_pass = TRUE),
  list(name = "selection_on_slope",
       beta_mu_high = 0.20,
       require_plain_pass   = FALSE,
       require_detrend_pass = TRUE)
)

dyn_all <- bind_rows(lapply(scenarios, function(sc) {
  run_scenario(sc$name, sc$beta_mu_high)
}))

###############################################################################
# Compare to truth
###############################################################################

truth_tbl <- tibble(event_time = event_grid, truth = true_tau(event_grid))

merged <- dyn_all %>%
  left_join(truth_tbl, by = "event_time") %>%
  mutate(deviation = att - truth)

write_csv(merged,
          file.path(results_dir, "synthetic_etwfe_truth_vs_estimate.csv"))

tolerance <- 0.20

check_tbl <- merged %>%
  group_by(scenario, spec, estimator) %>%
  summarise(
    max_abs_deviation = max(abs(deviation), na.rm = TRUE),
    max_abs_dev_pre   = max(abs(deviation[event_time <  0]), na.rm = TRUE),
    max_abs_dev_post  = max(abs(deviation[event_time >= 0]), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(passes = max_abs_deviation <= tolerance)

write_csv(check_tbl,
          file.path(results_dir, "synthetic_etwfe_pass_fail.csv"))

cat("\n=== Pass / fail check (tolerance =", tolerance, ") ===\n")
print(check_tbl)

###############################################################################
# Plot estimates vs truth
###############################################################################

p <- merged %>%
  ggplot(aes(x = event_time, y = att,
             color = estimator, fill = estimator)) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4,
             linetype = "dashed") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  geom_line(aes(x = event_time, y = truth),
            color = "black", linewidth = 0.5, linetype = "dotted",
            inherit.aes = FALSE,
            data = function(d) distinct(d, scenario, spec, event_time, truth)) +
  facet_grid(scenario ~ spec) +
  scale_x_continuous(breaks = event_grid) +
  scale_color_manual(values = c(
    "Wooldridge ETWFE"                            = "#023047",
    "Wooldridge ETWFE + county pre-period trend"  = "#bc4749"
  )) +
  scale_fill_manual(values = c(
    "Wooldridge ETWFE"                            = "#023047",
    "Wooldridge ETWFE + county pre-period trend"  = "#bc4749"
  )) +
  labs(
    x     = "Event time (years from treatment)",
    y     = "Estimate of ATT(e)",
    color = NULL, fill = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(results_dir, "synthetic_etwfe_estimates_vs_truth.png"),
       p, width = 11.5, height = 8, dpi = 300)

###############################################################################
# Hard pass / fail
###############################################################################

required_failures <- list()

for (sc in scenarios) {
  sc_check <- check_tbl %>% filter(scenario == sc$name)

  detrend <- sc_check %>%
    filter(estimator == "Wooldridge ETWFE + county pre-period trend")
  if (sc$require_detrend_pass && any(!detrend$passes)) {
    required_failures[[length(required_failures) + 1]] <-
      paste0("Detrend ETWFE failed in scenario '", sc$name, "'.")
  }

  plain <- sc_check %>%
    filter(estimator == "Wooldridge ETWFE")
  if (sc$require_plain_pass && any(!plain$passes)) {
    required_failures[[length(required_failures) + 1]] <-
      paste0("Plain ETWFE failed in scenario '", sc$name, "'.")
  }
}

if (length(required_failures) > 0) {
  cat("\n!!! Validation FAILED !!!\n")
  for (m in required_failures) cat("- ", m, "\n", sep = "")
  cat("\nSee ", file.path(results_dir, "synthetic_etwfe_truth_vs_estimate.csv"),
      "\n", sep = "")
  stop("Synthetic validation did not pass.")
}

cat("\nSynthetic validation PASSED. Plot and CSVs in ",
    results_dir, ".\n", sep = "")
