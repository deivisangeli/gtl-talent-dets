###############################################################################
# Shared sample construction for the AMWS land-grants temporal-support event
# studies.
#
# 03_run_baseline_population_controls.R (pooled) and
# 07_run_csdid_per_cohort_max_e.R (per cohort) both source this file so that
# they estimate on exactly the same events: the per-cohort figures are a
# decomposition of the pooled figures, so the treated counts in the cohort
# facets must sum to the pooled event count.
#
# Any change to the balance rule or the event-retention rule belongs here, once.
#
# Requires output_file_path() from prep/raw_paths.R, sourced by the caller.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

pad_geoid <- function(x) {
  str_pad(as.character(as.integer(x)), 5, pad = "0")
}

# The two county-decade panels and the event-unit file. The alternative panel
# bins births ending in 7-9 forward one decade, matching the g_shift treatment
# rule, so it carries a 1960 decade that the standard panel does not have.
read_temporal_support_inputs <- function() {
  read_padded <- function(name) {
    read_csv(
      output_file_path("land_grants", name),
      show_col_types = FALSE
    ) %>%
      mutate(GEOID = pad_geoid(GEOID))
  }

  list(
    panel = read_padded("amws_temporal_support_county_decade_1830_1950.csv"),
    panel_alt = read_padded("amws_temporal_support_county_decade_1830_1950_alt.csv"),
    units = read_padded("andrews_event_county_units_1850_1920.csv")
  )
}

# One stack per timing definition: each event's selected county is treated and
# its runner-ups are never-treated controls. Baseline covariates are measured in
# the decade before treatment (g - 10).
build_stack <- function(units, panel_src, timing_var, timing_name) {
  stack_units <- units %>%
    mutate(
      treatment_decade = .data[[timing_var]],
      control_decade = treatment_decade - 10L,
      g = if_else(sample_role == "treated", treatment_decade, 0L),
      timing = timing_name,
      stack_unit_id = paste(event_id, GEOID, sample_role, sep = "_"),
      stack_unit_num = dense_rank(stack_unit_id)
    )

  baseline <- stack_units %>%
    select(stack_unit_id, GEOID, control_decade) %>%
    left_join(
      panel_src %>% select(GEOID, control_decade = decade,
                           population_baseline = population,
                           baseline_population_source = population_source,
                           births_baseline = county_births_estimate),
      by = c("GEOID", "control_decade")
    ) %>%
    mutate(
      log_population_baseline = if_else(
        population_baseline > 0, log(population_baseline), NA_real_
      )
    )

  stack_units %>%
    left_join(baseline, by = c("stack_unit_id", "GEOID", "control_decade")) %>%
    inner_join(panel_src, by = "GEOID") %>%
    arrange(event_id, sample_role, GEOID, decade)
}

# Keep only events whose treated county has a cohort in `cohorts_filter`.
filter_cohorts <- function(stack_panel, cohorts_filter) {
  if (is.null(cohorts_filter)) return(stack_panel)
  treated_events_in_cohorts <- stack_panel %>%
    filter(sample_role == "treated", treatment_decade %in% cohorts_filter) %>%
    distinct(event_id) %>%
    pull(event_id)
  stack_panel %>% filter(event_id %in% treated_events_in_cohorts)
}

# Relative-time balance: a stack unit is kept only if it has non-missing
# population at every requested event time, measured against its OWN event's
# treatment decade. Events that lose their treated county are dropped entirely.
filter_balanced_event_time <- function(stack_panel, event_times) {
  stack_panel %>%
    mutate(.et = decade - treatment_decade) %>%
    group_by(stack_unit_num) %>%
    mutate(.has_full = all(event_times %in% .et[!is.na(population)])) %>%
    ungroup() %>%
    filter(.has_full) %>%
    group_by(event_id) %>%
    mutate(.has_treated = any(sample_role == "treated")) %>%
    ungroup() %>%
    filter(.has_treated) %>%
    select(-.et, -.has_full, -.has_treated)
}

# The estimation sample for one outcome/covariate pair. An event survives only
# if both its treated county and at least one of its own runner-ups still have
# usable rows, so a treated county whose runner-ups all failed the balance
# filter is not silently carried by other events' controls.
prepare_event_study_sample <- function(data, outcome, control) {
  d <- data %>%
    select(stack_unit_num, GEOID, decade, g, event_id, sample_role,
           all_of(outcome), all_of(control)) %>%
    rename(y = all_of(outcome), x = all_of(control)) %>%
    filter(!is.na(y), is.finite(y), !is.na(x), is.finite(x))

  retained <- d %>%
    group_by(event_id) %>%
    summarise(
      has_treated = any(g > 0),
      has_control = any(g == 0),
      .groups = "drop"
    ) %>%
    filter(has_treated, has_control) %>%
    select(event_id)

  semi_join(d, retained, by = "event_id")
}
