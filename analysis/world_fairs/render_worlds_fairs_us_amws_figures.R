###############################################################################
# Render USA-only AMWS world's-fairs figures from existing result CSVs.
# This script does not estimate models or modify any CSV output.
#
# Run from the repository root or analysis/:
#   Rscript analysis/world_fairs/render_worlds_fairs_us_amws_figures.R
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
  if (basename(repo_root) == "world_fairs" && basename(dirname(repo_root)) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, "..", ".."), winslash = "/")
  }
  if (basename(repo_root) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, ".."), winslash = "/")
  }
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
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(user_data_dir, winslash = "/", mustWork = TRUE)
  }
}

profile <- world_fairs_us_amws_profile()
message("WORLD_FAIRS_PROFILE: ", profile$name)

results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results",
  "worlds_fair",
  profile$results_subdir
)
dynamic_file <- file.path(results_dir, "all_dynamic_att.csv")
if (!file.exists(dynamic_file)) stop("Missing dynamic ATT file: ", dynamic_file)

outcome_labels <- c(
  n_amws = "AMWS scientists born",
  log1p_n_amws = "log(1 + AMWS scientists born)",
  amws_per_100k = "AMWS scientists born per 100k population",
  amws_per_1000_births = "AMWS scientists born per 1,000 births",
  population = "County population",
  county_births_estimate = "County births estimate"
)
event_times <- seq(profile$event_time_min, profile$event_time_max, 10L)
plot_estimator_label <- value_or(
  profile$plot_estimator_label,
  "CSDID, ref e=-10"
)
dynamic_att <- read_csv(dynamic_file, show_col_types = FALSE)

required_columns <- c(
  "treatment_spec", "distance_bin_km", "outcome", "event_time",
  "estimate", "ci_low", "ci_high"
)
missing_columns <- setdiff(required_columns, names(dynamic_att))
if (length(missing_columns) > 0L) {
  stop("Dynamic ATT file is missing columns: ", paste(missing_columns, collapse = ", "))
}
if (!all(dynamic_att$event_time %in% event_times)) {
  stop(
    "Dynamic ATT file contains event times outside ",
    min(event_times), " to +", max(event_times), "."
  )
}

analysis_keys <- dynamic_att %>%
  distinct(treatment_spec, distance_bin_km)
expected_figures <- nrow(analysis_keys) * n_distinct(dynamic_att$outcome)
rendered_files <- character()

for (key_index in seq_len(nrow(analysis_keys))) {
  spec_label <- analysis_keys$treatment_spec[[key_index]]
  bin_label <- analysis_keys$distance_bin_km[[key_index]]
  output_dir <- world_fairs_output_dir(results_dir, spec_label, bin_label)
  sample_file <- file.path(output_dir, "sample_summary.csv")
  treatment_file <- file.path(output_dir, "treatment_assignment.csv")
  required_files <- c(sample_file, treatment_file)
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0L) {
    stop("Missing rendering inputs:\n", paste(missing_files, collapse = "\n"))
  }

  sample_summary <- read_csv(sample_file, show_col_types = FALSE)
  treatment_assignment <- read_csv(treatment_file, show_col_types = FALSE)
  n_events <- n_distinct(treatment_assignment$first_parent_fair_id, na.rm = TRUE)
  n_treated_units <- sample_summary$n_treated_eligible[[1L]]
  n_control_units <- sample_summary$n_never_treated_controls[[1L]]
  treatment_label <- world_fairs_treatment_label(spec_label, bin_label)

  analysis_data <- dynamic_att %>%
    filter(
      treatment_spec == spec_label,
      if (is.na(bin_label)) is.na(distance_bin_km) else distance_bin_km == bin_label
    )

  for (outcome in unique(analysis_data$outcome)) {
    plot_data <- analysis_data %>% filter(.data$outcome == .env$outcome)
    if (nrow(plot_data) != length(event_times) ||
        !setequal(plot_data$event_time, event_times)) {
      stop("Incomplete event-time grid for ", spec_label, " / ", bin_label, " / ", outcome)
    }

    plot <- plot_dynamic_att(
      plot_data,
      title = paste0(
        outcome_labels[[outcome]],
        " — World's fairs, ",
        treatment_label,
        " (", plot_estimator_label, ")"
      ),
      n_events = n_events,
      n_treated_units = n_treated_units,
      n_control_units = n_control_units,
      event_time_min = min(event_times),
      event_time_max = max(event_times)
    )

    output_file <- file.path(
      output_dir,
      paste0("ES_", sanitize_filename(outcome), ".png")
    )
    ggsave(output_file, plot, width = 8, height = 6, dpi = 300)
    rendered_files <- c(rendered_files, output_file)
  }
}

if (length(rendered_files) != expected_figures || any(!file.exists(rendered_files))) {
  stop(
    "Expected ", expected_figures, " rendered figures; produced ",
    length(rendered_files), "."
  )
}

message("Rendered ", length(rendered_files), " figures without re-estimating models.")
