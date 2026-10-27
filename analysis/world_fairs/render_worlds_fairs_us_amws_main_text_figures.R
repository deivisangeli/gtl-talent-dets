###############################################################################
# Render the paired USA world's-fairs figures used in project/main.tex.
#
# The left/right panels use validated standard- and alternative-decade
# single-fair results and share y-axis limits within each outcome.
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
}

source(file.path(repo_root, "paths.R"))
source(file.path(
  repo_root,
  "analysis",
  "world_fairs",
  "worlds_fairs_us_amws_helpers.R"
))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users",
    Sys.info()[["user"]],
    "Globtalent Dropbox",
    "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(
      user_data_dir,
      winslash = "/",
      mustWork = TRUE
    )
  }
}

timing_specs <- tribble(
  ~profile_name, ~timing, ~timing_label,
  "robust_m30_pop_m10_balanced_oldest_treatment_m20_single_fair",
  "standard_decade", "standard decade",
  paste0(
    "robust_m30_pop_m10_balanced_oldest_treatment_m20_",
    "single_fair_g_shift"
  ),
  "alternative_decade", "alternative decade"
)
extended_pre_timing_specs <- tribble(
  ~profile_name, ~timing, ~timing_label,
  "robust_m50_pop_m10_balanced_oldest_treatment_m20_single_fair",
  "standard_decade", "standard decade",
  paste0(
    "robust_m50_pop_m10_balanced_oldest_treatment_m20_",
    "single_fair_g_shift"
  ),
  "alternative_decade", "alternative decade"
)

outcomes <- c(
  "n_amws",
  "amws_per_1000_births",
  "county_births_estimate"
)
outcome_labels <- c(
  n_amws = "AMWS scientists born",
  amws_per_1000_births = "AMWS scientists born per 1,000 births",
  county_births_estimate = "County births estimate"
)
event_times <- seq(-30L, 50L, 10L)
extended_pre_event_times <- seq(-50L, 50L, 10L)
figures_dir <- file.path(repo_root, "project", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

load_validated_timing_result <- function(profile_name, timing, timing_label,
                                         selected_outcomes,
                                         selected_event_times) {
  profile <- world_fairs_us_amws_profile(profile_name)
  results_dir <- file.path(
    TALENT_DETS_DATA_DIR,
    "results",
    "worlds_fair",
    profile$results_subdir
  )
  validation_file <- file.path(results_dir, "run_validation.csv")
  dynamic_file <- file.path(results_dir, "all_dynamic_att.csv")
  hosted_dir <- file.path(results_dir, "hosted")
  required_files <- c(
    validation_file,
    dynamic_file,
    file.path(hosted_dir, "sample_summary.csv"),
    file.path(hosted_dir, "treatment_assignment.csv")
  )
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0L) {
    stop("Missing manuscript-figure inputs:\n", paste(missing_files, collapse = "\n"))
  }

  validation <- read_csv(validation_file, show_col_types = FALSE)
  if (!all(validation$passed)) {
    stop("Profile failed validation: ", profile_name)
  }

  dynamic <- read_csv(dynamic_file, show_col_types = FALSE) %>%
    filter(
      treatment_spec == "hosted",
      is.na(distance_bin_km),
      outcome %in% selected_outcomes
    ) %>%
    mutate(timing = timing, timing_label = timing_label)
  expected_grid <- tidyr::crossing(
    outcome = selected_outcomes,
    event_time = selected_event_times
  )
  observed_grid <- dynamic %>% distinct(outcome, event_time)
  if (nrow(anti_join(expected_grid, observed_grid, by = c("outcome", "event_time"))) > 0L ||
      nrow(dynamic) != nrow(expected_grid)) {
    stop("Incomplete hosted event-time grid for profile: ", profile_name)
  }
  reference_rows <- dynamic %>% filter(event_time == -10L)
  if (nrow(reference_rows) != length(selected_outcomes) ||
      any(abs(reference_rows$estimate) > .Machine$double.eps^0.5)) {
    stop("Reference period is not normalized for profile: ", profile_name)
  }

  sample_summary <- read_csv(
    file.path(hosted_dir, "sample_summary.csv"),
    show_col_types = FALSE
  )
  treatment_assignment <- read_csv(
    file.path(hosted_dir, "treatment_assignment.csv"),
    show_col_types = FALSE
  )

  list(
    profile_name = profile_name,
    timing = timing,
    timing_label = timing_label,
    dynamic = dynamic,
    n_events = n_distinct(
      treatment_assignment$first_parent_fair_id,
      na.rm = TRUE
    ),
    n_treated_units = sample_summary$n_treated_eligible[[1L]],
    n_control_units = sample_summary$n_never_treated_controls[[1L]]
  )
}

timing_results <- pmap(timing_specs, function(profile_name, timing, timing_label) {
  load_validated_timing_result(
    profile_name,
    timing,
    timing_label,
    selected_outcomes = outcomes,
    selected_event_times = event_times
  )
})

all_dynamic <- map_dfr(timing_results, "dynamic")
rendered_files <- character()

for (outcome in outcomes) {
  outcome_data <- all_dynamic %>% filter(.data$outcome == .env$outcome)
  limits <- range(
    c(outcome_data$ci_low, outcome_data$ci_high, 0),
    na.rm = TRUE
  )
  padding <- diff(limits) * 0.03
  if (!is.finite(padding) || padding == 0) padding <- 1
  y_limits <- limits + c(-padding, padding)

  for (timing_result in timing_results) {
    plot_data <- timing_result$dynamic %>%
      filter(.data$outcome == .env$outcome)
    plot <- plot_dynamic_att(
      plot_data,
      title = paste0(
        outcome_labels[[outcome]],
        " — World's fairs, hosted counties ",
        "(CSDID + log population, lead20, single fair, ",
        timing_result$timing_label,
        ", ref e=-10)"
      ),
      n_events = timing_result$n_events,
      n_treated_units = timing_result$n_treated_units,
      n_control_units = timing_result$n_control_units,
      event_time_min = min(event_times),
      event_time_max = max(event_times),
      y_limits = y_limits
    )
    output_file <- file.path(
      figures_dir,
      paste0("WF_ES_", outcome, "_", timing_result$timing, ".png")
    )
    ggsave(output_file, plot, width = 8, height = 6, dpi = 300)
    rendered_files <- c(rendered_files, output_file)
  }
}

if (length(rendered_files) != 2L * length(outcomes) ||
    any(!file.exists(rendered_files))) {
  stop("Expected six main-text figures; rendered ", length(rendered_files), ".")
}

message(
  "Rendered ", length(rendered_files),
  " paired main-text figures in: ", figures_dir
)

extended_pre_results <- pmap(
  extended_pre_timing_specs,
  function(profile_name, timing, timing_label) {
    load_validated_timing_result(
      profile_name,
      timing,
      timing_label,
      selected_outcomes = "amws_per_1000_births",
      selected_event_times = extended_pre_event_times
    )
  }
)
extended_pre_dynamic <- map_dfr(extended_pre_results, "dynamic")
extended_pre_limits <- range(
  c(
    extended_pre_dynamic$ci_low,
    extended_pre_dynamic$ci_high,
    0
  ),
  na.rm = TRUE
)
extended_pre_padding <- diff(extended_pre_limits) * 0.03
if (!is.finite(extended_pre_padding) || extended_pre_padding == 0) {
  extended_pre_padding <- 1
}
extended_pre_y_limits <- extended_pre_limits +
  c(-extended_pre_padding, extended_pre_padding)
extended_pre_rendered_files <- character()

for (timing_result in extended_pre_results) {
  plot <- plot_dynamic_att(
    timing_result$dynamic,
    title = paste0(
      outcome_labels[["amws_per_1000_births"]],
      " — World's fairs, hosted counties ",
      "(CSDID + log population, lead20, single fair, e=[-50,+50], ",
      timing_result$timing_label,
      ", ref e=-10)"
    ),
    n_events = timing_result$n_events,
    n_treated_units = timing_result$n_treated_units,
    n_control_units = timing_result$n_control_units,
    event_time_min = min(extended_pre_event_times),
    event_time_max = max(extended_pre_event_times),
    y_limits = extended_pre_y_limits
  )
  output_file <- file.path(
    figures_dir,
    paste0(
      "WF_ES_amws_per_1000_births_lead20_m50_",
      timing_result$timing,
      ".png"
    )
  )
  ggsave(output_file, plot, width = 8, height = 6, dpi = 300)
  extended_pre_rendered_files <- c(extended_pre_rendered_files, output_file)
}

if (length(extended_pre_rendered_files) != 2L ||
    any(!file.exists(extended_pre_rendered_files))) {
  stop(
    "Expected two extended-preperiod robustness figures; rendered ",
    length(extended_pre_rendered_files), "."
  )
}

message(
  "Rendered ", length(extended_pre_rendered_files),
  " extended-preperiod robustness figures in: ", figures_dir
)

load_validated_per_cohort_result <- function(profile_name, timing, timing_label) {
  profile <- world_fairs_us_amws_profile(profile_name)
  per_cohort_dir <- file.path(
    TALENT_DETS_DATA_DIR,
    "results",
    "worlds_fair",
    profile$results_subdir,
    "per_cohort_figure5"
  )
  validation_file <- file.path(per_cohort_dir, "run_validation.csv")
  dynamic_file <- file.path(per_cohort_dir, "all_dynamic_att_per_cohort.csv")
  status_file <- file.path(per_cohort_dir, "all_model_status_per_cohort.csv")
  required_files <- c(validation_file, dynamic_file, status_file)
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0L) {
    stop(
      "Missing Figure 5 per-cohort inputs:\n",
      paste(missing_files, collapse = "\n")
    )
  }

  validation <- read_csv(validation_file, show_col_types = FALSE)
  if (!all(validation$passed)) {
    stop("Per-cohort profile failed validation: ", profile_name)
  }

  dynamic <- read_csv(dynamic_file, show_col_types = FALSE) %>%
    filter(
      treatment_spec == "hosted",
      is.na(distance_bin_km),
      outcome == "amws_per_1000_births"
    ) %>%
    mutate(timing = timing, timing_label = timing_label)
  status <- read_csv(status_file, show_col_types = FALSE) %>%
    filter(
      treatment_spec == "hosted",
      is.na(distance_bin_km),
      outcome == "amws_per_1000_births"
    )

  expected <- if (timing == "standard_decade") {
    tribble(
      ~cohort, ~fair_cohort, ~n_treated_units, ~event_time_max,
      1860L, 1880L, 4L, 100L,
      1870L, 1890L, 1L, 90L,
      1880L, 1900L, 1L, 80L
    )
  } else {
    tribble(
      ~cohort, ~fair_cohort, ~n_treated_units, ~event_time_max,
      1860L, 1880L, 2L, 100L,
      1870L, 1890L, 2L, 90L,
      1880L, 1900L, 1L, 80L,
      1890L, 1910L, 1L, 70L
    )
  }
  observed <- status %>%
    transmute(
      cohort = as.integer(cohort),
      fair_cohort = as.integer(fair_cohort),
      n_treated_units = as.integer(n_treated_units),
      event_time_max = as.integer(event_time_max)
    ) %>%
    arrange(cohort)
  if (!identical(observed, expected) ||
      any(status$n_control_units != 687L) ||
      any(status$event_time_min != -30L) ||
      any(!status$ok)) {
    stop("Unexpected Figure 5 per-cohort sample for profile: ", profile_name)
  }

  expected_grid <- expected %>%
    transmute(
      cohort,
      event_time = map2(
        cohort,
        event_time_max,
        ~ seq(-30L, .y, 10L)
      )
    ) %>%
    unnest(event_time)
  observed_grid <- dynamic %>%
    transmute(cohort = as.integer(cohort), event_time = as.integer(event_time)) %>%
    distinct()
  if (!identical(arrange(observed_grid, cohort, event_time), expected_grid) ||
      nrow(dynamic) != nrow(expected_grid)) {
    stop("Incomplete Figure 5 per-cohort grid for profile: ", profile_name)
  }
  reference_rows <- dynamic %>% filter(event_time == -10L)
  if (nrow(reference_rows) != nrow(expected) ||
      any(abs(reference_rows$estimate) > .Machine$double.eps^0.5)) {
    stop("Per-cohort reference period is not normalized for profile: ", profile_name)
  }

  list(
    profile_name = profile_name,
    timing = timing,
    timing_label = timing_label,
    dynamic = dynamic,
    n_events = sum(expected$n_treated_units),
    n_cohorts = nrow(expected)
  )
}

per_cohort_results <- pmap(
  timing_specs,
  load_validated_per_cohort_result
)
per_cohort_dynamic <- map_dfr(per_cohort_results, "dynamic")
per_cohort_limits <- range(
  c(per_cohort_dynamic$ci_low, per_cohort_dynamic$ci_high, 0),
  na.rm = TRUE
)
per_cohort_padding <- diff(per_cohort_limits) * 0.03
if (!is.finite(per_cohort_padding) || per_cohort_padding == 0) {
  per_cohort_padding <- 1
}
per_cohort_y_limits <- per_cohort_limits +
  c(-per_cohort_padding, per_cohort_padding)
per_cohort_rendered_files <- character()

for (timing_result in per_cohort_results) {
  plot_data <- timing_result$dynamic %>%
    group_by(cohort) %>%
    mutate(cohort_event_time_max = max(event_time)) %>%
    ungroup() %>%
    mutate(
      period = factor(
        if_else(event_time < 0, "Pre", "Post"),
        levels = c("Pre", "Post")
      ),
      cohort_label = paste0(
        "g_birth = ", cohort,
        " (fair g=", fair_cohort,
        ", n_t=", n_treated_units,
        ", n_c=", n_control_units,
        ", e_max=", cohort_event_time_max, ")"
      )
    )
  cohort_label_levels <- plot_data %>%
    distinct(cohort, cohort_label) %>%
    arrange(cohort) %>%
    pull(cohort_label)
  plot_data <- plot_data %>%
    mutate(cohort_label = factor(cohort_label, levels = cohort_label_levels))

  plot <- ggplot(
    plot_data,
    aes(x = event_time, y = estimate, ymin = ci_low, ymax = ci_high)
  ) +
    geom_vline(xintercept = 0, linetype = "dotted", colour = "gray60") +
    geom_point(aes(colour = period), size = 1.2, na.rm = TRUE) +
    geom_errorbar(aes(colour = period), width = 1.5, na.rm = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~ cohort_label, nrow = 1, scales = "fixed") +
    scale_x_continuous(breaks = seq(-30L, 100L, 10L)) +
    scale_color_manual(
      drop = FALSE,
      values = c(Pre = "#e87d72", Post = "#56bcc2"),
      breaks = c("Pre", "Post"),
      labels = c("Pre", "Post")
    ) +
    coord_cartesian(
      xlim = c(-30L, 100L),
      ylim = per_cohort_y_limits
    ) +
    labs(
      x = "Relative Time",
      y = "Effect",
      colour = NULL,
      title = str_wrap(
        paste0(
          outcome_labels[["amws_per_1000_births"]],
          " — World's fairs by treatment cohort ",
          "(CSDID + log population, lead20, single fair, ",
          timing_result$timing_label, ", ref e=-10)"
        ),
        100
      ),
      subtitle = paste0(
        "Events: ", timing_result$n_events,
        " (same sample as pooled Figure 5)"
      )
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(color = "darkgray", face = "bold", size = 11),
      plot.subtitle = element_text(color = "darkgray", size = 10),
      axis.title = element_text(color = "darkgray", face = "bold", size = 11),
      strip.text = element_text(color = "darkgray", face = "bold", size = 9),
      strip.background = element_rect(fill = "white", color = "white"),
      legend.position = "bottom"
    )

  output_file <- file.path(
    figures_dir,
    paste0(
      "WF_ES_percohort_amws_per_1000_births_",
      timing_result$timing,
      ".png"
    )
  )
  ggsave(
    output_file,
    plot,
    width = 3.5 * timing_result$n_cohorts + 2,
    height = 4.5,
    dpi = 300
  )
  per_cohort_rendered_files <- c(per_cohort_rendered_files, output_file)
}

if (length(per_cohort_rendered_files) != 2L ||
    any(!file.exists(per_cohort_rendered_files))) {
  stop(
    "Expected two Figure 5 per-cohort appendix figures; rendered ",
    length(per_cohort_rendered_files), "."
  )
}

message(
  "Rendered ", length(per_cohort_rendered_files),
  " Figure 5 per-cohort appendix figures in: ", figures_dir
)
