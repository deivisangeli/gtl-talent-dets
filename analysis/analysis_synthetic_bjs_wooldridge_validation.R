###############################################################################
# Project: Determinants of Talent Production via Elite High Schools
# Goal: Validate BJS and Wooldridge implementations on synthetic staggered data
#
# Synthetic DGP:
#   - Counties treated in different decades, plus a never-treated group
#   - Untreated potential outcomes have cohort-specific linear pre-trends
#   - Treatment raises the slope after treatment by a known amount
#
# Validation targets:
#   1. BJS without cohort trends
#   2. BJS with cohort-specific linear trends
#   3. Wooldridge ETWFE without cohort trends
#   4. Wooldridge ETWFE with cohort-specific linear trends
#
# Outputs:
#   results/elite_school_event_studies/synthetic_bjs_wooldridge_validation/
#     synthetic_truth.csv
#     synthetic_dynamic.csv
#     synthetic_manual_benchmark.csv
#     synthetic_rmse.csv
#     synthetic_wooldridge_wrapper_check.csv
#     synthetic_event_study_validation.png
#     synthetic_wooldridge_wrapper_check.png
#     synthetic_summary.txt
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("tidyverse")
  library("etwfe")
  library("fixest")
  library("ggplot2")
})

initial_time <- Sys.time()
theme_set(theme_minimal(base_size = 13))
set.seed(20260501)

results_root <- file.path("analysis", "results", "elite_school_event_studies")
results_dir <- file.path(results_root, "synthetic_bjs_wooldridge_validation")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

window_years <- 60L
event_grid <- seq(-window_years, window_years, by = 10L)
post_grid <- event_grid[event_grid >= 0]
pre_grid <- event_grid[event_grid < 0]

n_counties <- 600L
decades <- seq(1800L, 2000L, by = 10L)
treated_cohorts <- c(1840L, 1860L, 1880L, 1900L, 1920L)
cohort_values <- c(0L, treated_cohorts)
cohort_probs <- c(0.30, rep(0.14, length(treated_cohorts)))
post_slope_shift <- 1.5
noise_sd <- 6.0

###############################################################################
# Helpers
###############################################################################

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

run_bjs_dynamic <- function(data, outcome, trend_adjusted = FALSE) {
  data_model <- data

  if (trend_adjusted) {
    trend_origin <- min(data_model$decade, na.rm = TRUE)
    data_model <- data_model %>%
      mutate(
        cohort = factor(g),
        trend = decade - trend_origin
      )

    trend_setup <- add_group_trend_controls(data_model)
    data_model <- trend_setup$data
    first_stage_rhs <- paste(c("0", trend_setup$trend_cols), collapse = " + ")
    first_stage_fml <- as.formula(
      paste0(outcome, " ~ ", first_stage_rhs, " | county_id + decade")
    )

    trend_label <- "Cohort linear trends"
  } else {
    first_stage_fml <- as.formula(
      paste0(outcome, " ~ 0 | county_id + decade")
    )

    trend_label <- "No cohort trends"
  }

  first_stage_mod <- feols(
    first_stage_fml,
    data = data_model %>% filter(g == 0L | decade < g),
    warn = FALSE,
    fixef.rm = "none"
  )

  resid_name <- paste0(outcome, "_bjs_resid")
  data_model[[resid_name]] <- data_model[[outcome]] - predict(
    first_stage_mod,
    newdata = data_model
  )

  data_model %>%
    filter(g > 0L, !is.na(.data[[resid_name]])) %>%
    mutate(event_time = decade - g) %>%
    filter(event_time %in% event_grid) %>%
    group_by(event_time) %>%
    summarise(
      att = mean(.data[[resid_name]], na.rm = TRUE),
      se = sd(.data[[resid_name]], na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      estimator = "BJS",
      trend_spec = trend_label,
      conf_low = att - 1.96 * se,
      conf_high = att + 1.96 * se
    ) %>%
    select(estimator, trend_spec, event_time, att, se, conf_low, conf_high)
}

aggregate_wooldridge_event <- function(mod, data, trend_label, term_prefix = "Dtreat",
                                       g_var = "g", t_var = "decade_f") {
  coef_names <- names(coef(mod))
  beta <- coef(mod)
  vc <- as.matrix(vcov(mod))

  support <- data %>%
    filter(g > 0L) %>%
    mutate(event_time = decade - g) %>%
    filter(event_time %in% event_grid) %>%
    count(g, decade, event_time, name = "n_obs")

  map_dfr(event_grid, function(e) {
    cells <- support %>%
      filter(event_time == e) %>%
      mutate(term = paste0(term_prefix, ":", g_var, "::", g, ":", t_var, "::", decade))

    if (nrow(cells) == 0) {
      return(tibble())
    }

    cells <- cells %>%
      filter(term %in% coef_names)

    if (nrow(cells) == 0) {
      return(tibble())
    }

    w <- cells$n_obs / sum(cells$n_obs)
    idx <- match(cells$term, coef_names)
    att <- sum(w * beta[idx])
    v_sub <- vc[idx, idx, drop = FALSE]
    se <- sqrt(as.numeric(t(w) %*% v_sub %*% w))

    tibble(
      estimator = "Wooldridge ETWFE",
      trend_spec = trend_label,
      event_time = e,
      att = att,
      se = se,
      conf_low = att - 1.96 * se,
      conf_high = att + 1.96 * se
    )
  })
}

run_wooldridge_direct <- function(data, outcome, trend_adjusted = FALSE) {
  trend_origin <- min(data$decade, na.rm = TRUE)

  data_model <- data %>%
    mutate(
      Dtreat = as.integer(g > 0L),
      cohort = factor(g),
      decade_f = factor(decade),
      trend = decade - trend_origin
    )

  if (trend_adjusted) {
    fml <- as.formula(
      paste0(
        outcome,
        " ~ Dtreat:i(g, decade_f, ref = 0) | county_id + decade + cohort[trend]"
      )
    )
    trend_label <- "Cohort linear trends"
  } else {
    fml <- as.formula(
      paste0(
        outcome,
        " ~ Dtreat:i(g, decade_f, ref = 0) | county_id + decade"
      )
    )
    trend_label <- "No cohort trends"
  }

  mod <- suppressWarnings(
    feols(fml, data = data_model, cluster = ~county_id, warn = FALSE)
  )

  aggregate_wooldridge_event(mod, data_model, trend_label)
}

run_wooldridge_residualized <- function(data, outcome, trend_adjusted = FALSE) {
  trend_origin <- min(data$decade, na.rm = TRUE)

  data_model <- data %>%
    mutate(
      cohort = factor(g),
      trend = decade - trend_origin
    )

  untreated <- data_model %>%
    filter(g == 0L | decade < g)

  if (trend_adjusted) {
    trend_setup <- add_group_trend_controls(data_model)
    data_model <- trend_setup$data
    untreated <- data_model %>%
      filter(g == 0L | decade < g)
    first_stage_rhs <- paste(c("0", trend_setup$trend_cols), collapse = " + ")
    first_stage_fml <- as.formula(
      paste0(outcome, " ~ ", first_stage_rhs, " | county_id + decade")
    )
    trend_label <- "Cohort linear trends"
  } else {
    first_stage_fml <- as.formula(
      paste0(outcome, " ~ 0 | county_id + factor(decade)")
    )
    trend_label <- "No cohort trends"
  }

  trend_mod <- feols(
    first_stage_fml,
    data = untreated,
    warn = FALSE,
    fixef.rm = "none"
  )

  resid_name <- paste0(outcome, "_resid")
  data_model[[resid_name]] <- data_model[[outcome]] - predict(trend_mod,
                                                              newdata = data_model)

  data_model %>%
    filter(g > 0L) %>%
    mutate(event_time = decade - g) %>%
    filter(event_time %in% event_grid) %>%
    group_by(event_time) %>%
    summarise(
      att = mean(.data[[resid_name]], na.rm = TRUE),
      se = sd(.data[[resid_name]], na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      estimator = "Wooldridge ETWFE",
      trend_spec = trend_label,
      conf_low = att - 1.96 * se,
      conf_high = att + 1.96 * se
    ) %>%
    select(estimator, trend_spec, event_time, att, se, conf_low, conf_high)
}

run_wooldridge_wrapper <- function(data, outcome) {
  mod <- suppressWarnings(
    etwfe(
      fml = as.formula(paste0(outcome, " ~ 1")),
      tvar = decade,
      gvar = g,
      data = data,
      ivar = county_id,
      cgroup = "never",
      gref = 0,
      vcov = ~county_id
    )
  )

  fx <- suppressWarnings(
    emfx(mod, type = "event", post_only = FALSE)
  )

  tibble(as.data.frame(fx)) %>%
    transmute(
      event_time = as.integer(event),
      att_wrapper = estimate,
      se_wrapper = std.error
    ) %>%
    filter(event_time %in% event_grid)
}

run_manual_trend_benchmark <- function(data, outcome) {
  trend_origin <- min(data$decade, na.rm = TRUE)

  data_model <- data %>%
    mutate(
      cohort = factor(g),
      trend = decade - trend_origin
    )

  untreated <- data_model %>%
    filter(g == 0L | decade < g)

  mod <- feols(
    as.formula(paste0(outcome, " ~ 0 | county_id + factor(decade) + cohort[trend]")),
    data = untreated
  )

  data_model %>%
    mutate(
      y0_hat = predict(mod, newdata = data_model),
      event_time = if_else(g > 0L, decade - g, NA_integer_),
      att_hat = .data[[outcome]] - y0_hat
    ) %>%
    filter(!is.na(event_time), event_time %in% event_grid) %>%
    group_by(event_time) %>%
    summarise(
      att = mean(att_hat),
      true_att = mean(true_att),
      error = mean(att_hat - true_att),
      .groups = "drop"
    ) %>%
    mutate(
      estimator = "Manual untreated-only benchmark",
      trend_spec = "Cohort linear trends"
    ) %>%
    select(estimator, trend_spec, event_time, att, true_att, error)
}

###############################################################################
# Simulate panel
###############################################################################

county_tbl <- tibble(
  county_id = seq_len(n_counties),
  g = sample(cohort_values, size = n_counties, replace = TRUE, prob = cohort_probs),
  alpha_i = rnorm(n_counties, mean = 0, sd = 4)
) %>%
  mutate(
    cohort_rank = match(g, cohort_values) - 1L,
    untreated_slope = 0.20 + 0.18 * cohort_rank
  )

panel <- crossing(
  county_tbl,
  decade = decades
) %>%
  mutate(
    t_index = (decade - min(decades)) / 10,
    common_time = 0.4 * t_index + 0.8 * sin(t_index / 3),
    event_step = if_else(g > 0L, (decade - g) / 10, NA_real_),
    true_att = if_else(g > 0L & event_step >= 0, post_slope_shift * event_step, 0),
    y0 = alpha_i + common_time + untreated_slope * t_index + rnorm(n(), sd = noise_sd),
    y = y0 + true_att
  )

truth_tbl <- tibble(
  event_time = event_grid,
  true_att = if_else(event_time >= 0L, post_slope_shift * (event_time / 10), 0)
)

write_csv(truth_tbl, file.path(results_dir, "synthetic_truth.csv"))

###############################################################################
# Estimate and compare
###############################################################################

bjs_no_trends <- run_bjs_dynamic(panel, "y", trend_adjusted = FALSE)
bjs_trends <- run_bjs_dynamic(panel, "y", trend_adjusted = TRUE)
wool_no_trends <- run_wooldridge_residualized(panel, "y", trend_adjusted = FALSE)
wool_trends <- run_wooldridge_residualized(panel, "y", trend_adjusted = TRUE)

dynamic_tbl <- bind_rows(
  bjs_no_trends,
  bjs_trends,
  wool_no_trends,
  wool_trends
) %>%
  left_join(truth_tbl, by = "event_time") %>%
  mutate(error = att - true_att)

write_csv(dynamic_tbl, file.path(results_dir, "synthetic_dynamic.csv"))

manual_benchmark <- run_manual_trend_benchmark(panel, "y")
write_csv(manual_benchmark, file.path(results_dir, "synthetic_manual_benchmark.csv"))

wrapper_check <- run_wooldridge_wrapper(panel, "y") %>%
  left_join(
    wool_no_trends %>%
      select(event_time, att_direct = att, se_direct = se),
    by = "event_time"
  ) %>%
  mutate(
    abs_diff = abs(att_wrapper - att_direct),
    abs_se_diff = abs(se_wrapper - se_direct)
  )

write_csv(wrapper_check, file.path(results_dir, "synthetic_wooldridge_wrapper_check.csv"))

rmse_tbl <- dynamic_tbl %>%
  mutate(period_group = if_else(event_time < 0L, "pre", "post")) %>%
  group_by(estimator, trend_spec, period_group) %>%
  summarise(
    rmse = sqrt(mean(error^2, na.rm = TRUE)),
    max_abs_error = max(abs(error), na.rm = TRUE),
    mean_error = mean(error, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(rmse_tbl, file.path(results_dir, "synthetic_rmse.csv"))

###############################################################################
# Plots
###############################################################################

plot_position <- position_dodge(width = 1.2)

plot_validation <- dynamic_tbl %>%
  ggplot(
    aes(
      x = event_time,
      y = att,
      color = estimator,
      fill = estimator,
      linetype = estimator,
      shape = estimator,
      group = estimator
    )
  ) +
  geom_hline(yintercept = 0, color = "gray55", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = 0.4, linetype = "dashed") +
  geom_ribbon(
    aes(ymin = conf_low, ymax = conf_high),
    alpha = 0.12,
    color = NA,
    position = plot_position
  ) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 2.5,
    alpha = 0.45,
    position = plot_position
  ) +
  geom_line(linewidth = 0.9, position = plot_position) +
  geom_point(size = 1.4, position = plot_position) +
  geom_line(
    data = truth_tbl,
    aes(x = event_time, y = true_att),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  facet_wrap(~ trend_spec, ncol = 1) +
  scale_x_continuous(breaks = event_grid) +
  scale_color_manual(values = c(
    "BJS" = "#023047",
    "Wooldridge ETWFE" = "#bc4749"
  )) +
  scale_fill_manual(values = c(
    "BJS" = "#023047",
    "Wooldridge ETWFE" = "#bc4749"
  )) +
  scale_linetype_manual(values = c(
    "BJS" = "solid",
    "Wooldridge ETWFE" = "dashed"
  )) +
  scale_shape_manual(values = c(
    "BJS" = 16,
    "Wooldridge ETWFE" = 17
  )) +
  labs(
    x = "Years relative to treatment",
    y = "ATT",
    color = NULL,
    fill = NULL,
    linetype = NULL,
    shape = NULL,
    title = "Synthetic validation of BJS and Wooldridge event studies",
    subtitle = "Black dashed line is the exact treatment effect in the DGP"
  )

ggsave(file.path(results_dir, "synthetic_event_study_validation.png"),
       plot_validation, width = 9, height = 8.5, dpi = 300)

plot_wrapper <- wrapper_check %>%
  ggplot(aes(x = event_time)) +
  geom_hline(yintercept = 0, color = "gray55", linewidth = 0.4) +
  geom_line(aes(y = att_wrapper, color = "Wrapper"), linewidth = 0.9) +
  geom_point(aes(y = att_wrapper, color = "Wrapper"), size = 1.3) +
  geom_line(aes(y = att_direct, color = "Direct"), linewidth = 0.9, linetype = "dashed") +
  geom_point(aes(y = att_direct, color = "Direct"), size = 1.3) +
  scale_x_continuous(breaks = event_grid) +
  scale_color_manual(values = c("Wrapper" = "#1d3557", "Direct" = "#e76f51")) +
  labs(
    x = "Years relative to treatment",
    y = "ATT",
    color = NULL,
    title = "Wooldridge wrapper check",
    subtitle = "Wrapper ETWFE versus direct saturated fixest aggregation"
  )

ggsave(file.path(results_dir, "synthetic_wooldridge_wrapper_check.png"),
       plot_wrapper, width = 9, height = 4.8, dpi = 300)

###############################################################################
# Text summary
###############################################################################

sink(file.path(results_dir, "synthetic_summary.txt"))
cat("=== Synthetic validation of BJS and Wooldridge ===\n\n")
cat("Counties:", n_counties, "\n")
cat("Decades:", min(decades), "to", max(decades), "\n")
cat("Treated cohorts:", paste(treated_cohorts, collapse = ", "), "\n")
cat("Never-treated share:", cohort_probs[[1]], "\n")
cat("Post-treatment slope shift in truth:", post_slope_shift, "per decade\n")
cat("Noise SD:", noise_sd, "\n\n")
cat("Untreated potential outcomes include cohort-specific linear trends.\n")
cat("Correctly specified trend-adjusted estimators should recover zero pre-period effects\n")
cat("and the true post-treatment slope shift path.\n\n")
cat("BJS and Wooldridge pre/post periods are residualized event-cell means from\n")
cat("the same untreated first-stage model for each trend specification. This keeps\n")
cat("the validation plot on the raw ATT scale and avoids event-reference normalization.\n\n")
cat("=== RMSE relative to exact truth ===\n")
print(rmse_tbl)
cat("\n=== Manual untreated-only benchmark ===\n")
print(manual_benchmark)
cat("\n=== Wooldridge wrapper versus direct check ===\n")
print(wrapper_check)
elapsed <- difftime(Sys.time(), initial_time, units = "mins")
cat("\nRuntime (minutes):", round(as.numeric(elapsed), 2), "\n")
sink()

cat("Synthetic validation complete. Outputs written to ", results_dir, ".\n", sep = "")
