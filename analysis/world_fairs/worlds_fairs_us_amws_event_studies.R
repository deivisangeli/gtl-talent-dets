###############################################################################
# USA-only world's-fairs event studies using the consolidated AMWS outcomes.
#
# Run from the repository root or analysis/:
#   Rscript analysis/world_fairs/worlds_fairs_us_amws_event_studies.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(did)
  library(sf)
  library(tigris)
})

initial_time <- Sys.time()
options(timeout = 1000, tigris_use_cache = TRUE)
sf_use_s2(FALSE)

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
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

profile <- world_fairs_us_amws_profile()
message("WORLD_FAIRS_PROFILE: ", profile$name)

panel_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
fairs_file <- file.path(
  DATA_INPUT,
  "worlds_fairs",
  "worlds_fairs_1790_1960_with_visits_venues.csv"
)
results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results",
  "worlds_fair",
  profile$results_subdir
)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(panel_file, fairs_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

classification_year_min <- 1790L
classification_year_max <- 1961L
treated_event_year_min <- profile$treated_event_year_min
treated_event_year_max <- profile$treated_event_year_max
panel_year_min <- 1800L
panel_year_max <- 1960L
event_times <- seq(profile$event_time_min, profile$event_time_max, 10L)
visits_threshold <- 100000
bin_breaks <- c(-1e-9, 2, 4, 6, 8, 10)
bin_labels <- c("0-2", "2-4", "4-6", "6-8", "8-10")
analysis_bin_labels <- c(bin_labels, "0-10")
cores <- suppressWarnings(as.integer(Sys.getenv("WORLD_FAIRS_CORES", unset = "4")))
if (is.na(cores) || cores < 1L) cores <- 1L

outcomes <- c(
  "n_amws",
  "log1p_n_amws",
  "amws_per_100k",
  "amws_per_1000_births",
  "population",
  "county_births_estimate"
)
population_control <- isTRUE(value_or(profile$population_control, FALSE))
balance_controls_calendar <- isTRUE(value_or(
  profile$balance_controls_calendar,
  value_or(profile$balance_controls_oldest, FALSE)
))
control_sample_source_profile_name <- as.character(value_or(
  profile$control_sample_source_profile,
  ""
))
reuse_control_sample <- nzchar(control_sample_source_profile_name)
control_sample_source_label <- if (reuse_control_sample) {
  control_sample_source_profile_name
} else {
  profile$name
}
control_sample_source_results_dir <- NA_character_
if (reuse_control_sample) {
  if (identical(control_sample_source_profile_name, profile$name)) {
    stop("A profile cannot inherit its control sample from itself.")
  }
  control_sample_source_profile <- world_fairs_us_amws_profile(
    control_sample_source_profile_name
  )
  control_sample_source_results_dir <- file.path(
    TALENT_DETS_DATA_DIR,
    "results",
    "worlds_fair",
    control_sample_source_profile$results_subdir
  )
  if (!dir.exists(control_sample_source_results_dir)) {
    stop(
      "Missing conventional control-sample results directory: ",
      control_sample_source_results_dir
    )
  }
}
est_method <- value_or(profile$est_method, "dr")
treatment_cohort_shift <- as.integer(value_or(profile$treatment_cohort_shift, 0L))
treatment_timing <- as.character(value_or(
  profile$treatment_timing,
  "standard_decade"
))
if (!treatment_timing %in% c("standard_decade", "alternative_decade")) {
  stop("Unsupported treatment timing: ", treatment_timing)
}
single_fair_event_window <- isTRUE(value_or(profile$single_fair_event_window, FALSE))
support_vars <- outcomes
if (population_control) support_vars <- unique(c(support_vars, "population"))
plot_estimator_label <- value_or(
  profile$plot_estimator_label,
  "CSDID, ref e=-10"
)
outcome_labels <- c(
  n_amws = "AMWS scientists born",
  log1p_n_amws = "log(1 + AMWS scientists born)",
  amws_per_100k = "AMWS scientists born per 100k population",
  amws_per_1000_births = "AMWS scientists born per 1,000 births",
  population = "County population",
  county_births_estimate = "County births estimate"
)

message("Reading and validating consolidated AMWS panel...")
panel_year <- fread(panel_file, na.strings = c("", "NA")) %>% as_tibble()
required_panel_cols <- c(
  "GEOID", "year", "population", "county_births_estimate_year",
  "n_amws_1906_1955_dedup", "n_amws_1986", "n_amws"
)
missing_panel_cols <- setdiff(required_panel_cols, names(panel_year))
if (length(missing_panel_cols) > 0L) {
  stop("AMWS panel is missing columns: ", paste(missing_panel_cols, collapse = ", "))
}

panel_year <- panel_year %>%
  transmute(
    GEOID = pad_geoid(GEOID),
    year = as.integer(year),
    population = as.numeric(population),
    county_births_estimate_year = as.numeric(county_births_estimate_year),
    n_amws_1906_1955_dedup = as.numeric(n_amws_1906_1955_dedup),
    n_amws_1986 = as.numeric(n_amws_1986),
    n_amws = as.numeric(n_amws)
  ) %>%
  filter(year >= panel_year_min, year <= panel_year_max, !is.na(GEOID))

duplicate_panel_keys <- panel_year %>% count(GEOID, year) %>% filter(n > 1L)
if (nrow(duplicate_panel_keys) > 0L) stop("Duplicate GEOID-year rows in AMWS panel.")
if (any(panel_year$n_amws != panel_year$n_amws_1906_1955_dedup + panel_year$n_amws_1986)) {
  stop("AMWS component counts do not reconcile.")
}
if (sum(panel_year$n_amws_1986, na.rm = TRUE) <= 0) {
  stop("The consolidated AMWS panel contains no edition-1986 observations.")
}

panel_decade <- panel_year %>%
  # Bin AMWS births (numerator), population, and the births denominator onto the
  # SAME decade grid as the treatment cohort. For standard profiles this reduces
  # to standard_decade(); for alternative-decade (g_shift) profiles it shifts
  # birth years ending in 7-9 forward one decade, matching the treatment timing.
  mutate(decade = event_decade(year, treatment_timing)) %>%
  group_by(GEOID, decade) %>%
  summarise(
    n_amws_1906_1955_dedup = sum(n_amws_1906_1955_dedup, na.rm = TRUE),
    n_amws_1986 = sum(n_amws_1986, na.rm = TRUE),
    n_amws = sum(n_amws, na.rm = TRUE),
    population = mean_or_na(population),
    county_births_estimate = sum_or_na(county_births_estimate_year),
    .groups = "drop"
  ) %>%
  mutate(
    unit_id = paste0("US_COUNTY_", GEOID),
    log1p_n_amws = log1p(n_amws),
    amws_per_100k = if_else(population > 0, 1e5 * n_amws / population, NA_real_),
    amws_per_1000_births = if_else(
      county_births_estimate > 0,
      1000 * n_amws / county_births_estimate,
      NA_real_
    )
  )

input_validation <- tibble(
  metric = c(
    "panel_rows", "panel_counties", "panel_min_year", "panel_max_year",
    "amws_1906_1955_dedup", "amws_1986", "amws_total",
    "duplicate_geoid_year_rows", "decade_rows_missing_population",
    "decade_rows_missing_births_denominator"
  ),
  value = c(
    nrow(panel_year), n_distinct(panel_year$GEOID), min(panel_year$year),
    max(panel_year$year), sum(panel_year$n_amws_1906_1955_dedup),
    sum(panel_year$n_amws_1986), sum(panel_year$n_amws),
    nrow(duplicate_panel_keys), sum(!is.finite(panel_decade$population)),
    sum(!is.finite(panel_decade$county_births_estimate))
  )
)
write_csv(input_validation, file.path(results_dir, "input_validation.csv"))

message("Building USA county geometries...")
geometry_result <- build_us_target_geometries(panel_decade)
us_targets <- geometry_result$targets
write_csv(geometry_result$missing, file.path(results_dir, "geoid_missing_tigris_2020.csv"))
panel_decade <- panel_decade %>%
  semi_join(sf::st_drop_geometry(us_targets) %>% select(GEOID), by = "GEOID")

message("Loading conservative USA fair venues...")
venue_data <- load_us_venue_data(
  fairs_file,
  classification_year_min,
  classification_year_max,
  visits_threshold
)
write_csv(venue_data$audit, file.path(results_dir, "venue_quality_audit.csv"))

message("Building treatment assignments...")
exposures <- list(
  hosted = build_host_exposure(
    us_targets, venue_data$all,
    treated_event_year_min, treated_event_year_max, classification_year_max,
    timing = treatment_timing
  ),
  hosted_visits_100k = build_host_exposure(
    us_targets, venue_data$visits_100k,
    treated_event_year_min, treated_event_year_max, classification_year_max,
    timing = treatment_timing
  ),
  venue_distance = build_distance_exposure(
    us_targets, venue_data$all, bin_breaks, bin_labels,
    treated_event_year_min, treated_event_year_max, classification_year_max,
    timing = treatment_timing
  ),
  venue_distance_visits_100k = build_distance_exposure(
    us_targets, venue_data$visits_100k, bin_breaks, bin_labels,
    treated_event_year_min, treated_event_year_max, classification_year_max,
    timing = treatment_timing
  )
)

specifications <- list(
  hosted = list(type = "hosted", bins = NA_character_),
  hosted_visits_100k = list(type = "hosted", bins = NA_character_),
  venue_distance = list(type = "distance", bins = analysis_bin_labels),
  venue_distance_visits_100k = list(type = "distance", bins = analysis_bin_labels)
)

all_model_status <- list()
all_dynamic_att <- list()
all_simple_att <- list()
all_treated_counts <- list()
all_support <- list()
all_sample_summaries <- list()
all_control_balance <- list()
all_control_sample_identity <- list()
all_baseline_population <- list()
all_single_fair_audits <- list()
all_single_fair_details <- list()
run_warnings <- character()
result_index <- 0L

for (spec_label in names(specifications)) {
  spec <- specifications[[spec_label]]
  exposure <- exposures[[spec_label]]
  all_event_audit <- if (str_starts(spec_label, "hosted")) {
    exposures$hosted$match_audit
  } else {
    exposures$venue_distance$match_audit
  }
  spec_dir <- file.path(results_dir, spec_label)
  dir.create(spec_dir, recursive = TRUE, showWarnings = FALSE)

  write_csv(exposure$match_audit, file.path(spec_dir, "venue_match_audit.csv"))
  write_csv(exposure$first_exposure, file.path(spec_dir, "first_exposure.csv"))
  write_csv(exposure$never_units, file.path(spec_dir, "never_treated_units.csv"))
  write_csv(
    exposure$always_units,
    file.path(spec_dir, paste0("always_treated_pre_", treated_event_year_min, ".csv"))
  )
  write_csv(
    exposure$future_units,
    file.path(spec_dir, paste0("future_treated_after_", treated_event_year_max, ".csv"))
  )

  bins <- spec$bins
  if (spec$type == "hosted") bins <- NA_character_

  for (bin_label in bins) {
    result_index <- result_index + 1L
    analysis_label <- if (is.na(bin_label)) spec_label else paste(spec_label, bin_label, sep = "__")
    output_dir <- if (is.na(bin_label)) spec_dir else file.path(
      spec_dir,
      paste0("bin_", str_replace_all(bin_label, "-", "_"), "km")
    )
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    treated_assigned_unfiltered <- exposure$first_exposure %>%
      filter(exposure_status == "treated")
    if (!is.na(bin_label)) {
      if (bin_label == "0-10") {
        treated_assigned_unfiltered <- treated_assigned_unfiltered %>%
          filter(distance_bin_km %in% bin_labels)
      } else {
        treated_assigned_unfiltered <- treated_assigned_unfiltered %>%
          filter(distance_bin_km == bin_label)
      }
    }
    treated_assigned_unfiltered <- treated_assigned_unfiltered %>%
      mutate(
        fair_cohort = first_exposure_decade,
        g = first_exposure_decade + treatment_cohort_shift
      ) %>%
      distinct(unit_id, .keep_all = TRUE)

    if (single_fair_event_window) {
      single_fair_eligibility <- build_single_fair_window_eligibility(
        treated_assigned_unfiltered,
        all_event_audit,
        event_times,
        timing = treatment_timing
      )
      treated_assigned <- single_fair_eligibility$eligible
      write_csv(
        treated_assigned_unfiltered,
        file.path(output_dir, "treatment_assignment_before_single_fair_filter.csv")
      )
      write_csv(
        single_fair_eligibility$audit,
        file.path(output_dir, "single_fair_eligibility_audit.csv")
      )
      write_csv(
        single_fair_eligibility$detail,
        file.path(output_dir, "single_fair_event_detail.csv")
      )
      all_single_fair_audits[[analysis_label]] <-
        single_fair_eligibility$audit %>%
        mutate(treatment_spec = spec_label, distance_bin_km = bin_label)
      all_single_fair_details[[analysis_label]] <-
        single_fair_eligibility$detail %>%
        mutate(treatment_spec = spec_label, distance_bin_km = bin_label)
    } else {
      treated_assigned <- treated_assigned_unfiltered
    }

    eligibility <- build_treatment_eligibility(
      treated_assigned,
      panel_decade,
      support_vars,
      event_times
    )
    treated <- eligibility$eligible
    candidate_controls <- exposure$never_units %>% mutate(g = 0L)
    treated_cohorts <- sort(unique(treated$g))
    reference_cohort <- if (length(treated_cohorts) > 0L) {
      min(treated_cohorts)
    } else {
      NA_integer_
    }
    if (balance_controls_calendar && length(treated_cohorts) > 0L) {
      control_eligibility <- build_calendar_balanced_never_controls(
        candidate_controls,
        panel_decade,
        support_vars,
        event_times,
        treated_cohorts
      )
      controls <- control_eligibility$eligible %>% mutate(g = 0L)
    } else {
      control_eligibility <- list(
        audit = candidate_controls %>%
          mutate(
            eligible = TRUE,
            observed_periods = NA_integer_,
            required_periods = NA_integer_,
            required_calendar_decades = "",
            missing_calendar_decades = ""
          ),
        detail = tibble(),
        eligible = candidate_controls,
        required_decades = integer()
      )
      controls <- candidate_controls
    }

    if (reuse_control_sample) {
      source_output_dir <- world_fairs_output_dir(
        control_sample_source_results_dir,
        spec_label,
        bin_label
      )
      source_control_file <- file.path(
        source_output_dir,
        "balanced_never_treated_controls.csv"
      )
      if (!file.exists(source_control_file)) {
        stop("Missing conventional control sample: ", source_control_file)
      }
      source_controls <- read_csv(source_control_file, show_col_types = FALSE) %>%
        transmute(
          unit_id = as.character(unit_id),
          GEOID = pad_geoid(GEOID)
        )
      if (anyDuplicated(source_controls$unit_id) > 0L) {
        stop("Duplicate unit IDs in conventional control sample: ", source_control_file)
      }

      control_sample_identity_audit <- source_controls %>%
        left_join(
          candidate_controls %>%
            transmute(unit_id, GEOID, is_current_never_treated = TRUE),
          by = c("unit_id", "GEOID")
        ) %>%
        left_join(
          control_eligibility$eligible %>%
            transmute(unit_id, calendar_support_complete = TRUE),
          by = "unit_id"
        ) %>%
        mutate(
          is_current_never_treated = replace_na(is_current_never_treated, FALSE),
          calendar_support_complete = replace_na(calendar_support_complete, FALSE),
          control_sample_source_profile = control_sample_source_profile_name,
          identity_ok = is_current_never_treated & calendar_support_complete
        )
      if (!all(control_sample_identity_audit$identity_ok)) {
        stop(
          "Conventional controls are not valid calendar-balanced never-treated ",
          "units for ", analysis_label, "."
        )
      }

      controls <- candidate_controls %>%
        semi_join(source_controls, by = c("unit_id", "GEOID")) %>%
        mutate(g = 0L)
      if (!setequal(controls$unit_id, source_controls$unit_id)) {
        stop("Failed to reproduce the conventional control IDs for ", analysis_label, ".")
      }
    } else {
      control_sample_identity_audit <- controls %>%
        transmute(
          unit_id,
          GEOID,
          is_current_never_treated = TRUE,
          calendar_support_complete = TRUE,
          control_sample_source_profile = profile$name,
          identity_ok = TRUE
        )
    }

    control_eligibility$audit <- control_eligibility$audit %>%
      mutate(
        selected_as_control = unit_id %in% controls$unit_id,
        control_sample_source_profile = control_sample_source_label
      )

    write_csv(treated_assigned, file.path(output_dir, "treatment_assignment_before_support.csv"))
    write_csv(eligibility$audit, file.path(output_dir, "treatment_eligibility_audit.csv"))
    write_csv(eligibility$detail, file.path(output_dir, "treatment_eligibility_detail.csv"))
    write_csv(treated, file.path(output_dir, "treatment_assignment.csv"))
    write_csv(control_eligibility$audit, file.path(output_dir, "control_balance_audit.csv"))
    write_csv(control_eligibility$detail, file.path(output_dir, "control_balance_detail.csv"))
    write_csv(controls, file.path(output_dir, "balanced_never_treated_controls.csv"))
    write_csv(
      control_sample_identity_audit,
      file.path(output_dir, "control_sample_identity_audit.csv")
    )

    baseline_population_audit <- map_dfr(sort(unique(treated$g)), function(cohort) {
      baseline_decade <- cohort - 10L
      bind_rows(
        treated %>% filter(g == cohort) %>% transmute(unit_id, GEOID, sample_role = "treated"),
        controls %>% transmute(unit_id, GEOID, sample_role = "never_treated")
      ) %>%
        mutate(cohort = cohort, baseline_decade = baseline_decade) %>%
        left_join(
          panel_decade %>%
            select(GEOID, baseline_decade = decade, population_baseline = population),
          by = c("GEOID", "baseline_decade")
        ) %>%
        mutate(
          log_population_baseline = if_else(
            population_baseline > 0,
            log(population_baseline),
            NA_real_
          )
        )
    })
    write_csv(
      baseline_population_audit,
      file.path(output_dir, "baseline_population_audit.csv")
    )

    analysis_units <- bind_rows(
      treated %>% select(unit_id, GEOID, g),
      controls %>% select(unit_id, GEOID, g)
    ) %>% distinct(unit_id, .keep_all = TRUE)

    panel_es <- panel_decade %>%
      semi_join(analysis_units, by = c("unit_id", "GEOID")) %>%
      left_join(analysis_units, by = c("unit_id", "GEOID")) %>%
      mutate(unit_num = as.integer(factor(unit_id)))

    support <- build_support_by_event_time(
      panel_decade,
      treated,
      controls,
      outcomes,
      event_times,
      spec_label,
      bin_label
    )
    write_csv(support, file.path(output_dir, "support_by_event_time.csv"))
    all_support[[analysis_label]] <- support

    treated_counts <- treated %>%
      count(g, name = "n_treated_eligible") %>%
      full_join(
        treated_assigned %>% count(g, name = "n_treated_assigned"),
        by = "g"
      ) %>%
      mutate(
        treatment_spec = spec_label,
        distance_bin_km = bin_label,
        n_treated_eligible = replace_na(n_treated_eligible, 0L),
        n_treated_assigned = replace_na(n_treated_assigned, 0L)
      ) %>%
      relocate(treatment_spec, distance_bin_km)
    write_csv(treated_counts, file.path(output_dir, "treated_counts_by_cohort.csv"))
    all_treated_counts[[analysis_label]] <- treated_counts

    sample_summary <- tibble(
      treatment_spec = spec_label,
      distance_bin_km = bin_label,
      n_treated_before_single_fair_filter = n_distinct(
        treated_assigned_unfiltered$unit_id
      ),
      n_treated_assigned = n_distinct(treated_assigned$unit_id),
      n_treated_excluded_multiple_fairs =
        n_distinct(treated_assigned_unfiltered$unit_id) -
        n_distinct(treated_assigned$unit_id),
      n_treated_eligible = n_distinct(treated$unit_id),
      n_treated_excluded_for_support = n_distinct(treated_assigned$unit_id) - n_distinct(treated$unit_id),
      n_never_treated_controls = n_distinct(controls$unit_id),
      n_always_treated_excluded = nrow(exposure$always_units),
      n_future_treated_excluded = nrow(exposure$future_units),
      event_time_min = min(event_times),
      event_time_max = max(event_times),
      single_fair_event_window = single_fair_event_window,
      control_calendar_min = if (length(control_eligibility$required_decades) > 0L) {
        min(control_eligibility$required_decades)
      } else {
        NA_integer_
      },
      control_calendar_max = if (length(control_eligibility$required_decades) > 0L) {
        max(control_eligibility$required_decades)
      } else {
        NA_integer_
      },
      control_sample_source_profile = control_sample_source_label
    )
    write_csv(sample_summary, file.path(output_dir, "sample_summary.csv"))
    all_sample_summaries[[analysis_label]] <- sample_summary
    all_control_balance[[analysis_label]] <- control_eligibility$audit %>%
      mutate(treatment_spec = spec_label, distance_bin_km = bin_label)
    all_control_sample_identity[[analysis_label]] <-
      control_sample_identity_audit %>%
      mutate(treatment_spec = spec_label, distance_bin_km = bin_label)
    all_baseline_population[[analysis_label]] <- baseline_population_audit %>%
      mutate(treatment_spec = spec_label, distance_bin_km = bin_label)

    model_results <- list()
    for (outcome in outcomes) {
      message("Running ", analysis_label, " / ", outcome, "...")
      model_results[[outcome]] <- withCallingHandlers(
        run_amws_event_study(
          panel_es,
          outcome,
          spec_label,
          bin_label,
          min(event_times),
          max(event_times),
          cores,
          population_control = population_control,
          est_method = est_method
        ),
        warning = function(w) {
          run_warnings <<- c(run_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    }

    model_status <- map_dfr(model_results, model_status_row)
    successful <- keep(model_results, ~ isTRUE(.x$ok))
    dynamic_att <- imap_dfr(
      successful,
      ~ extract_dynamic_att(.x$dynamic, .x$outcome, spec_label, bin_label)
    )
    simple_att <- imap_dfr(
      successful,
      ~ extract_simple_att(.x$simple, .x$outcome, spec_label, bin_label)
    )

    write_csv(model_status, file.path(output_dir, "model_status.csv"))
    write_csv(dynamic_att, file.path(output_dir, "dynamic_att.csv"))
    write_csv(simple_att, file.path(output_dir, "simple_att.csv"))
    all_model_status[[analysis_label]] <- model_status
    all_dynamic_att[[analysis_label]] <- dynamic_att
    all_simple_att[[analysis_label]] <- simple_att

    if (nrow(dynamic_att) > 0L) {
      n_events <- n_distinct(treated$first_parent_fair_id, na.rm = TRUE)
      n_treated_units <- n_distinct(treated$unit_id)
      n_control_units <- n_distinct(controls$unit_id)
      treatment_label <- world_fairs_treatment_label(spec_label, bin_label)
      for (outcome in unique(dynamic_att$outcome)) {
        plot_data <- dynamic_att %>% filter(.data$outcome == .env$outcome)
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
        ggsave(
          file.path(output_dir, paste0("ES_", sanitize_filename(outcome), ".png")),
          plot,
          width = 8,
          height = 6,
          dpi = 300
        )
      }
    }

    notes <- c(
      paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      paste0("Treatment specification: ", spec_label),
      paste0("Distance bin: ", ifelse(is.na(bin_label), "hosted county", paste0(bin_label, " km"))),
      paste0("AMWS panel: ", panel_file),
      paste0("Fairs: ", fairs_file),
      "AMWS combines deduplicated 1906/1938/1955 editions with edition 1986.",
      paste0(
        "Treated cohorts: g = ", profile$treated_cohort_min,
        " through ", profile$treated_cohort_max, "."
      ),
      paste0("Treated support requires e = ", paste(event_times, collapse = ", "), "."),
      paste0(
        "Estimator: did::att_gt, ", est_method,
        ", universal base, never-treated controls."
      ),
      paste0("Population control: ", population_control, "."),
      paste0("Treatment cohort shift from fair decade: ", treatment_cohort_shift, "."),
      paste0(
        "Single-fair requirement within the event window: ",
        single_fair_event_window, "."
      ),
      if (single_fair_event_window) {
        paste0(
          "Single-fair count uses all catalogued fairs and distinct parent_fair_id ",
          "within e=[", min(event_times), ",", max(event_times), "]."
        )
      } else {
        "Single-fair count not applied."
      },
      paste0("Controls balanced over full calendar support: ", balance_controls_calendar, "."),
      paste0(
        "Required control calendar decades: ",
        paste(control_eligibility$required_decades, collapse = ","), "."
      ),
      paste0("Control sample source profile: ", control_sample_source_label, "."),
      "Unbalanced panels are allowed; e = -10 is the normalized reference period.",
      paste0("Successful models: ", sum(model_status$ok), " / ", nrow(model_status))
    )
    writeLines(notes, file.path(output_dir, "notes.txt"))
  }
}

combined_status <- bind_rows(all_model_status)
combined_dynamic <- bind_rows(all_dynamic_att)
combined_simple <- bind_rows(all_simple_att)
combined_treated <- bind_rows(all_treated_counts)
combined_support <- bind_rows(all_support)
combined_samples <- bind_rows(all_sample_summaries)
combined_control_balance <- bind_rows(all_control_balance)
combined_control_sample_identity <- bind_rows(all_control_sample_identity)
combined_baseline_population <- bind_rows(all_baseline_population)
combined_single_fair_audit <- bind_rows(all_single_fair_audits)
combined_single_fair_detail <- bind_rows(all_single_fair_details)

write_csv(combined_status, file.path(results_dir, "all_model_status.csv"))
write_csv(combined_dynamic, file.path(results_dir, "all_dynamic_att.csv"))
write_csv(combined_simple, file.path(results_dir, "all_simple_att.csv"))
write_csv(combined_treated, file.path(results_dir, "all_treated_counts_by_cohort.csv"))
write_csv(combined_support, file.path(results_dir, "all_support_by_event_time.csv"))
write_csv(combined_samples, file.path(results_dir, "all_sample_summary.csv"))
write_csv(combined_control_balance, file.path(results_dir, "all_control_balance_audit.csv"))
write_csv(
  combined_control_sample_identity,
  file.path(results_dir, "all_control_sample_identity_audit.csv")
)
write_csv(combined_baseline_population, file.path(results_dir, "all_baseline_population_audit.csv"))
if (single_fair_event_window) {
  write_csv(
    combined_single_fair_audit,
    file.path(results_dir, "all_single_fair_eligibility_audit.csv")
  )
  write_csv(
    combined_single_fair_detail,
    file.path(results_dir, "all_single_fair_event_detail.csv")
  )
}
warning_summary <- tibble(warning = run_warnings) %>%
  count(warning, sort = TRUE, name = "n_occurrences")
write_csv(warning_summary, file.path(results_dir, "warning_summary.csv"))

expected_models <- nrow(combined_samples) * length(outcomes)
core_status <- combined_status %>%
  filter(
    treatment_spec %in% c("hosted", "hosted_visits_100k") |
      distance_bin_km == "0-10"
  )
core_counts <- combined_treated %>%
  filter(
    (treatment_spec %in% c("hosted", "hosted_visits_100k") &
       is.na(distance_bin_km)) |
      distance_bin_km == "0-10"
  ) %>%
  group_by(treatment_spec, distance_bin_km) %>%
  summarise(
    n_treated_assigned = sum(n_treated_assigned),
    n_treated_eligible = sum(n_treated_eligible),
    .groups = "drop"
  )
expected_core_counts <- tibble(
  treatment_spec = c(
    "hosted", "hosted_visits_100k", "venue_distance",
    "venue_distance_visits_100k"
  ),
  distance_bin_km = c(NA_character_, NA_character_, "0-10", "0-10"),
  expected_assigned = profile$expected_assigned,
  expected_eligible = profile$expected_eligible
)
expected_core_models <- nrow(expected_core_counts) * length(outcomes)
core_count_validation <- expected_core_counts %>%
  left_join(core_counts, by = c("treatment_spec", "distance_bin_km")) %>%
  mutate(
    assigned_ok = n_treated_assigned == expected_assigned,
    eligible_ok = n_treated_eligible == expected_eligible
  )
write_csv(
  core_count_validation,
  file.path(results_dir, "core_treated_count_validation.csv")
)
if (!is.null(profile$expected_core_controls)) {
  expected_control_counts <- expected_core_counts %>%
    select(treatment_spec, distance_bin_km) %>%
    mutate(expected_controls = profile$expected_core_controls)
  core_control_validation <- expected_control_counts %>%
    left_join(
      combined_samples %>%
        filter(
          (treatment_spec %in% c("hosted", "hosted_visits_100k") &
             is.na(distance_bin_km)) |
            distance_bin_km == "0-10"
        ) %>%
        select(treatment_spec, distance_bin_km, n_never_treated_controls),
      by = c("treatment_spec", "distance_bin_km")
    ) %>%
    mutate(controls_ok = n_never_treated_controls == expected_controls)
} else {
  core_control_validation <- tibble(
    treatment_spec = character(),
    distance_bin_km = character(),
    expected_controls = integer(),
    n_never_treated_controls = integer(),
    controls_ok = logical()
  )
}
write_csv(core_control_validation, file.path(results_dir, "core_control_count_validation.csv"))
control_support_validation <- combined_support %>%
  left_join(
    combined_samples %>%
      select(treatment_spec, distance_bin_km, n_never_treated_controls),
    by = c("treatment_spec", "distance_bin_km")
  ) %>%
  mutate(
    fixed_control_pool_available =
      n_control_units == n_never_treated_controls
  )
write_csv(
  control_support_validation,
  file.path(results_dir, "control_support_validation.csv")
)
dynamic_grid <- combined_dynamic %>%
  count(treatment_spec, distance_bin_km, outcome, name = "n_event_times")
reference_rows <- combined_dynamic %>% filter(event_time == -10)
treated_cohorts <- combined_treated %>%
  filter(n_treated_assigned > 0L) %>%
  distinct(g) %>%
  pull(g)
run_validation <- tibble(
  check = c(
    "expected_model_count",
    "amws_1986_present",
    "dynamic_event_times_in_range",
    "dynamic_grid_complete",
    "reference_period_normalized",
    "treated_cohorts_in_range",
    "core_assigned_counts_match",
    "core_eligible_counts_match",
    "core_models_successful",
    "all_models_successful",
    "balanced_control_counts_match",
    "fixed_control_pool_available",
    "baseline_population_complete",
    "single_fair_audit_valid",
    "single_fair_assignment_counts_match",
    "calendar_control_support_explicit",
    "inherited_control_ids_valid"
  ),
  passed = c(
    nrow(combined_status) == expected_models,
    sum(panel_year$n_amws_1986) > 0,
    all(combined_dynamic$event_time %in% event_times),
    nrow(dynamic_grid) == expected_models &&
      all(dynamic_grid$n_event_times == length(event_times)),
    nrow(reference_rows) == expected_models &&
      all(reference_rows$estimate == 0) && all(is.na(reference_rows$se)),
    length(treated_cohorts) > 0L &&
      all(treated_cohorts >= profile$treated_cohort_min) &&
      all(treated_cohorts <= profile$treated_cohort_max),
    all(core_count_validation$assigned_ok),
    all(core_count_validation$eligible_ok),
    nrow(core_status) == expected_core_models && all(core_status$ok),
    nrow(combined_status) == expected_models && all(combined_status$ok),
    nrow(core_control_validation) == 0L || all(core_control_validation$controls_ok),
    !balance_controls_calendar ||
      (nrow(control_support_validation) > 0L &&
         all(control_support_validation$fixed_control_pool_available)),
    !population_control ||
      (nrow(combined_baseline_population) > 0L &&
         all(is.finite(combined_baseline_population$log_population_baseline))),
    !single_fair_event_window ||
      (nrow(combined_single_fair_audit) > 0L &&
         all(combined_single_fair_audit$n_distinct_fairs_in_window >= 1L) &&
         all(
           combined_single_fair_audit$single_fair_eligible ==
             (combined_single_fair_audit$n_distinct_fairs_in_window == 1L)
         )),
    !single_fair_event_window ||
      sum(combined_single_fair_audit$single_fair_eligible) ==
        sum(combined_samples$n_treated_assigned),
    !balance_controls_calendar ||
      all(
        is.finite(combined_samples$control_calendar_min) &
          is.finite(combined_samples$control_calendar_max) &
          combined_samples$control_calendar_min <=
            combined_samples$control_calendar_max
      ),
    !reuse_control_sample ||
      (nrow(combined_control_sample_identity) ==
         sum(combined_samples$n_never_treated_controls) &&
         all(combined_control_sample_identity$identity_ok))
  ),
  detail = c(
    paste(nrow(combined_status), "of", expected_models),
    as.character(sum(panel_year$n_amws_1986)),
    paste(sort(unique(combined_dynamic$event_time)), collapse = ","),
    paste(nrow(dynamic_grid), "models with", length(event_times), "event times"),
    paste(nrow(reference_rows), "normalized rows"),
    paste(sort(treated_cohorts), collapse = ","),
    paste(core_count_validation$n_treated_assigned, collapse = ","),
    paste(core_count_validation$n_treated_eligible, collapse = ","),
    paste(sum(core_status$ok), "of", nrow(core_status)),
    paste(sum(combined_status$ok), "of", nrow(combined_status)),
    if (nrow(core_control_validation) == 0L) "not configured" else
      paste(core_control_validation$n_never_treated_controls, collapse = ","),
    paste(
      sum(control_support_validation$fixed_control_pool_available),
      "of", nrow(control_support_validation), "support cells"
    ),
    paste(
      sum(is.finite(combined_baseline_population$log_population_baseline)),
      "of", nrow(combined_baseline_population)
    ),
    if (!single_fair_event_window) "not configured" else paste(
      sum(combined_single_fair_audit$single_fair_eligible),
      "of", nrow(combined_single_fair_audit), "assignments retained"
    ),
    if (!single_fair_event_window) "not configured" else paste(
      sum(combined_single_fair_audit$single_fair_eligible),
      "audit rows vs", sum(combined_samples$n_treated_assigned),
      "assigned treated rows"
    ),
    if (!balance_controls_calendar) "not configured" else paste(
      paste(
        combined_samples$control_calendar_min,
        combined_samples$control_calendar_max,
        sep = "-"
      ),
      collapse = ","
    ),
    if (!reuse_control_sample) "not configured" else paste(
      nrow(combined_control_sample_identity),
      "inherited IDs from", control_sample_source_profile_name
    )
  )
)
write_csv(run_validation, file.path(results_dir, "run_validation.csv"))

elapsed <- difftime(Sys.time(), initial_time, units = "mins")
root_notes <- c(
  "USA-only AMWS world's-fairs event studies.",
  paste0("Profile: ", profile$name),
  paste0(
    "Treatment cohorts: ", profile$treated_cohort_min,
    " through ", profile$treated_cohort_max
  ),
  paste0("Event-time window: [", min(event_times), ", ", max(event_times), "]"),
  paste0("Population control: ", population_control, "; estimator: ", est_method),
  paste0("Treatment timing: ", treatment_timing),
  paste0("Treatment cohort shift from fair decade: ", treatment_cohort_shift),
  paste0("Single-fair requirement within event window: ", single_fair_event_window),
  paste0("Controls balanced over full calendar support: ", balance_controls_calendar),
  paste0("Control sample source profile: ", control_sample_source_label),
  paste0("Results directory: ", results_dir),
  paste0("Elapsed minutes: ", round(as.numeric(elapsed), 2)),
  paste0("Models successful: ", sum(combined_status$ok), " / ", nrow(combined_status)),
  paste0("Validation checks passed: ", sum(run_validation$passed), " / ", nrow(run_validation))
)
writeLines(root_notes, file.path(results_dir, "notes.txt"))

if (!all(run_validation$passed)) {
  stop("USA AMWS world's-fairs validation failed; see ", file.path(results_dir, "run_validation.csv"))
}

message("Completed USA AMWS world's-fairs event studies in: ", results_dir)
