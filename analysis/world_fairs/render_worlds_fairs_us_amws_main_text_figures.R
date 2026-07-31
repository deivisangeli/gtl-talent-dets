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
  "robust_m30_pop_m10_balanced_oldest_single_fair",
  "standard_decade", "standard decade",
  "robust_m30_pop_m10_balanced_oldest_single_fair_g_shift",
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
figures_dir <- file.path(repo_root, "project", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

timing_results <- pmap(timing_specs, function(profile_name, timing, timing_label) {
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
      outcome %in% outcomes
    ) %>%
    mutate(timing = timing, timing_label = timing_label)
  expected_grid <- tidyr::crossing(outcome = outcomes, event_time = event_times)
  observed_grid <- dynamic %>% distinct(outcome, event_time)
  if (nrow(anti_join(expected_grid, observed_grid, by = c("outcome", "event_time"))) > 0L ||
      nrow(dynamic) != nrow(expected_grid)) {
    stop("Incomplete hosted event-time grid for profile: ", profile_name)
  }
  reference_rows <- dynamic %>% filter(event_time == -10L)
  if (nrow(reference_rows) != length(outcomes) ||
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
        "(CSDID + log population, single fair, ",
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
