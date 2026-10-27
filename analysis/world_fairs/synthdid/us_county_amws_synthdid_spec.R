###############################################################################
# Shared specification for the non-pooled US county AMWS synthetic-DiD design.
###############################################################################

wf_us_sd_profile_specs <- function() {
  data.table::data.table(
    decade_definition = c("standard", "alternative"),
    profile_name = c(
      "robust_m30_pop_m10_balanced_oldest_treatment_m20_single_fair",
      paste0("robust_m30_pop_m10_balanced_oldest_treatment_m20_",
             "single_fair_g_shift")
    ),
    results_subdir = c(
      paste0("worlds_fairs_us_amws_event_studies_fair_cohorts_1840_1900_",
             "treatment_m20_e_m30_p50_balanced_oldest_logpop_m10_single_fair"),
      paste0("worlds_fairs_us_amws_event_studies_g_shift_treatment_m20_",
             "e_m30_p50_balanced_oldest_logpop_m10_single_fair")
    )
  )
}

wf_us_sd_expected_timing <- function() {
  data.table::data.table(
    decade_definition = rep(c("standard", "alternative"), each = 6L),
    GEOID = rep(c("21111", "22071", "36029", "36047", "39061", "47037"), 2L),
    treatment_cohort = c(
      1860L, 1860L, 1860L, 1880L, 1860L, 1870L,
      1860L, 1860L, 1870L, 1890L, 1870L, 1880L
    )
  )
}

wf_us_sd_variant_id <- function() {
  variant <- Sys.getenv("US_WF_SYNTHDID_VARIANT", unset = "fixed_m30")
  allowed <- c("fixed_m30", "max_pre")
  if (!variant %in% allowed) {
    stop("US_WF_SYNTHDID_VARIANT must be one of: ",
         paste(allowed, collapse = ", "), ".")
  }
  variant
}

wf_us_sd_expected_windows <- function(variant = wf_us_sd_variant_id()) {
  x <- wf_us_sd_expected_timing()
  if (identical(variant, "fixed_m30")) {
    x[, event_time_min := -30L]
  } else if (identical(variant, "max_pre")) {
    x[, event_time_min := c(
      -60L, -40L, -30L, -80L, -60L, -70L,
      -60L, -40L, -40L, -90L, -70L, -80L
    )]
  } else {
    stop("Unknown synthetic-DiD variant: ", variant, ".")
  }
  x[, `:=`(
    variant = variant,
    event_time_max = 50L,
    n_pre_periods = as.integer(-event_time_min / 10L),
    n_post_periods = 6L
  )]
  x[, total_periods := n_pre_periods + n_post_periods]
  setcolorder(x, c(
    "variant", "decade_definition", "GEOID", "treatment_cohort",
    "event_time_min", "event_time_max", "n_pre_periods",
    "n_post_periods", "total_periods"
  ))
  x[]
}

wf_us_sd_model_window <- function(definition, geoid, treatment_cohort,
                                  variant = wf_us_sd_variant_id()) {
  timing_value <- as.integer(treatment_cohort)
  x <- wf_us_sd_expected_windows(variant)[
    decade_definition == definition & GEOID == geoid &
      treatment_cohort == timing_value
  ]
  if (nrow(x) != 1L) {
    stop("Expected one window for ", definition, "/", geoid, "/g=",
         treatment_cohort, "; found ", nrow(x), ".")
  }
  x[, event_times := list(seq(event_time_min, event_time_max, by = 10L))]
  x[]
}

wf_us_sd_expected_eligible_donors <- function(
    variant = wf_us_sd_variant_id()) {
  x <- wf_us_sd_expected_timing()[, .(decade_definition, GEOID)]
  if (identical(variant, "fixed_m30")) {
    x[, expected_eligible_donors := c(
      876L, 884L, 886L, 1498L, 875L, 1154L,
      876L, 884L, 1159L, 1836L, 1147L, 1503L
    )]
  } else if (identical(variant, "max_pre")) {
    x[, expected_eligible_donors := c(
      367L, 681L, 886L, 360L, 367L, 367L,
      367L, 681L, 886L, 360L, 367L, 367L
    )]
  } else {
    stop("Unknown synthetic-DiD variant: ", variant, ".")
  }
  x[]
}

wf_us_sd_event_times <- function(definition = NULL, geoid = NULL,
                                 treatment_cohort = NULL,
                                 variant = wf_us_sd_variant_id()) {
  empty <- vapply(list(definition, geoid, treatment_cohort), is.null, logical(1))
  if (identical(variant, "fixed_m30") && all(empty)) {
    return(seq(-30L, 50L, by = 10L))
  }
  if (any(empty)) {
    stop("definition, geoid, and treatment_cohort are required for max_pre windows.")
  }
  wf_us_sd_model_window(definition, geoid, treatment_cohort, variant)$event_times[[1L]]
}

wf_us_sd_plot_event_times <- function(variant = wf_us_sd_variant_id()) {
  if (identical(variant, "fixed_m30")) {
    seq(-30L, 50L, by = 10L)
  } else if (identical(variant, "max_pre")) {
    seq(-90L, 50L, by = 10L)
  } else {
    stop("Unknown synthetic-DiD variant: ", variant, ".")
  }
}

wf_us_sd_matching_event_times <- function() c(-30L, -20L, -10L)
wf_us_sd_reference_event_time <- function() -10L
wf_us_sd_treatment_start_event_time <- function() 0L
wf_us_sd_fair_event_time <- function() 20L
wf_us_sd_donor_count <- function() 30L
wf_us_sd_donor_min_distance_km <- function() 50
wf_us_sd_outcome_name <- function() "amws_per_1000_births"

wf_us_sd_matching_specification <- function() paste0(
  "equal_z_euclidean_log1p_amws_per_1000_births_lags_m30_m20_m10_",
  "plus_log1p_population_cagr_m30_m10_plus_log_population_density_m10"
)

wf_us_sd_output_root <- function(data_root, variant = wf_us_sd_variant_id()) {
  suffix <- switch(
    variant,
    fixed_m30 = "us_county_amws_figure5_lead20_e0_nonpooled",
    max_pre = "us_county_amws_figure5_lead20_e0_nonpooled_max_pre",
    stop("Unknown synthetic-DiD variant: ", variant, ".")
  )
  file.path(data_root, "results", "worlds_fair", "synthdid", suffix)
}

wf_us_sd_legacy_output_root <- function(data_root) file.path(
  data_root, "results", "worlds_fair", "synthdid",
  "us_county_amws_figure5_lead20_nonpooled"
)

wf_us_sd_decade <- function(year, definition) {
  year <- as.integer(year)
  if (identical(definition, "standard")) {
    return(as.integer(floor(year / 10) * 10))
  }
  if (identical(definition, "alternative")) {
    return(as.integer(floor((year + 3L) / 10) * 10))
  }
  stop("Unknown decade definition: ", definition, ".")
}

wf_us_sd_model_id <- function(definition, geoid, treatment_cohort) paste0(
  definition, "_", geoid, "_g", as.integer(treatment_cohort)
)

wf_us_sd_feature_spec <- function() data.table::data.table(
  feature_index = 1:5,
  feature = c(
    "transformed_outcome_m30", "transformed_outcome_m20",
    "transformed_outcome_m10", "transformed_population_growth",
    "transformed_density"
  ),
  feature_group = c(
    "outcome_lag", "outcome_lag", "outcome_lag",
    "population_growth", "density"
  ),
  source_column = c(
    "outcome_m30", "outcome_m20", "outcome_m10",
    "population_cagr_m30_m10", "population_density_m10"
  )
)
