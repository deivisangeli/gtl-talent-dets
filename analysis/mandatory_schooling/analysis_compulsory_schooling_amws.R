###############################################################################
# Compulsory schooling laws and AMWS scientist births (US counties, 1800-1960)
#
# This is the AMWS-outcome counterpart to analysis_compulsory_schooling.R.
# It uses the same consolidated annual AMWS data as the USA world's-fairs
# pipeline, aggregated to standard county decades by the AMWS prep pipeline.
#
# Treatment: decade in which a state passed its first compulsory-attendance
# law. All counties in a state share treatment timing; missing law dates are
# coded as never treated. Identification is therefore between states adopting
# in different decades.
#
# Estimator: fixest::sunab event study with county and decade fixed effects,
# state-clustered standard errors, and e = -10 as the reference period.
# Both levels and cohort-linear-detrended specifications are estimated for:
#   - AMWS births per 1,000 population (primary)
#   - AMWS birth counts
#   - log(1 + AMWS birth counts)
#   - AMWS births per 1,000 estimated births
#
# Profiles (set with COMPULSORY_AMWS_PROFILE):
#   main                 Existing unbalanced e = -50,...,+50 analysis (default)
#   balanced_m40_p50     Same counties at every e = -40,...,+50 event time
#
# The balanced profile retains eligible counties' other available decades in
# estimation. This keeps the reported relative-time composition fixed while
# preserving the calendar-time support needed to identify e = +50.
###############################################################################

rm(list = ls())

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/", mustWork = TRUE
  )
} else {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repo_root <- if (basename(cwd) == "analysis") {
    dirname(cwd)
  } else if (basename(dirname(cwd)) == "analysis") {
    dirname(dirname(cwd))
  } else {
    cwd
  }
}

source(file.path(repo_root, "paths.R"))

# Match the portability fallback used by the USA world's-fairs AMWS analysis.
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(
      user_data_dir, winslash = "/", mustWork = TRUE
    )
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
    SCHOOLS_OUTPUT <- file.path(DATA_OUTPUT, "elite_schools")
  }
}

source(file.path(
  repo_root, "analysis", "elite_schools", "etwfe_high_access_helpers.R"
))

suppressPackageStartupMessages({
  library("fixest")
  library("ggplot2")
  library("ggrepel")
  library("tidyverse")
})

initial_time <- Sys.time()
theme_set(theme_minimal(base_size = 13))

profile_name <- tolower(trimws(Sys.getenv(
  "COMPULSORY_AMWS_PROFILE", unset = "main"
)))
profiles <- list(
  main = list(
    event_grid = seq(-50L, 50L, by = 10L),
    balance_event_time = FALSE,
    results_subdir = "compulsory_schooling_amws"
  ),
  balanced_m40_p50 = list(
    event_grid = seq(-40L, 50L, by = 10L),
    balance_event_time = TRUE,
    results_subdir = "compulsory_schooling_amws_balanced_m40_p50",
    expected_counties = 1609L,
    expected_states = 29L
  )
)
if (!profile_name %in% names(profiles)) {
  stop(
    "Unknown COMPULSORY_AMWS_PROFILE: ", profile_name,
    ". Expected one of: ", paste(names(profiles), collapse = ", ")
  )
}
profile <- profiles[[profile_name]]
event_grid <- profile$event_grid
message("COMPULSORY_AMWS_PROFILE: ", profile_name)

panel_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_decade.csv")
laws_file <- file.path(DATA_INPUT, "compulsory_schooling_laws.csv")
schools_file <- file.path(
  SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"
)
results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results", "mandatory_schooling", profile$results_subdir
)

required_files <- c(panel_file, laws_file, schools_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

panel_year_min <- 1800L
panel_year_max <- 1960L
min_pre <- 3L
ref_event <- -10L

outcome_labels <- c(
  amws_per_1000_pop = "AMWS scientists born per 1,000 population",
  n_amws = "AMWS scientists born",
  log1p_n_amws = "log(1 + AMWS scientists born)",
  amws_per_1000_births = "AMWS scientists born per 1,000 estimated births"
)
outcome_slugs <- c(
  amws_per_1000_pop = "per_1000_population",
  n_amws = "count",
  log1p_n_amws = "log1p_count",
  amws_per_1000_births = "per_1000_births"
)

###############################################################################
# Input validation and common analysis sample
###############################################################################

required_panel_cols <- c(
  "GEOID", "decade", "population", "population_source",
  "county_births_estimate_decade", "n_amws_1906_1955_dedup",
  "n_amws_1986", "n_amws", names(outcome_labels)
)

panel_raw <- read_csv(
  panel_file,
  col_types = cols(GEOID = col_character()),
  show_col_types = FALSE
)
missing_panel_cols <- setdiff(required_panel_cols, names(panel_raw))
if (length(missing_panel_cols) > 0L) {
  stop("AMWS panel is missing columns: ", paste(missing_panel_cols, collapse = ", "))
}

panel_raw <- panel_raw %>%
  transmute(
    GEOID = str_pad(as.character(.data$GEOID), 5L, "left", "0"),
    decade = as.integer(.data$decade),
    population = as.numeric(.data$population),
    population_source = as.character(.data$population_source),
    county_births_estimate_decade = as.numeric(
      .data$county_births_estimate_decade
    ),
    n_amws_1906_1955_dedup = as.numeric(.data$n_amws_1906_1955_dedup),
    n_amws_1986 = as.numeric(.data$n_amws_1986),
    n_amws = as.numeric(.data$n_amws),
    amws_per_1000_pop = as.numeric(.data$amws_per_1000_pop),
    log1p_n_amws = as.numeric(.data$log1p_n_amws),
    amws_per_1000_births = as.numeric(.data$amws_per_1000_births)
  )

duplicate_keys <- panel_raw %>% count(.data$GEOID, .data$decade) %>% filter(n > 1L)
if (nrow(duplicate_keys) > 0L) stop("Duplicate GEOID-decade rows in AMWS panel.")

if (any(panel_raw$n_amws < 0, na.rm = TRUE)) stop("Negative AMWS counts found.")
if (any(
  panel_raw$n_amws !=
    panel_raw$n_amws_1906_1955_dedup + panel_raw$n_amws_1986,
  na.rm = TRUE
)) {
  stop("AMWS component counts do not reconcile.")
}

amws_totals <- panel_raw %>%
  summarise(
    early = sum(.data$n_amws_1906_1955_dedup),
    ed1986 = sum(.data$n_amws_1986),
    total = sum(.data$n_amws)
  )
expected_totals <- c(early = 49587, ed1986 = 62704, total = 112291)
if (!identical(as.numeric(amws_totals[1, ]), as.numeric(expected_totals))) {
  stop(
    "AMWS totals differ from the validated world-fairs panel: expected ",
    paste(expected_totals, collapse = "/"), ", got ",
    paste(as.numeric(amws_totals[1, ]), collapse = "/")
  )
}

allowed_population_sources <- c("nhgis", "manual", "merged_nyc")
unexpected_sources <- setdiff(
  unique(panel_raw$population_source), allowed_population_sources
)
if (length(unexpected_sources) > 0L) {
  stop("Unexpected population sources: ", paste(unexpected_sources, collapse = ", "))
}

required_law_cols <- c(
  "state", "state_abbr", "state_fips", "compulsory_law_year"
)
laws <- read_csv(
  laws_file,
  col_types = cols(state_fips = col_character()),
  show_col_types = FALSE
)
missing_law_cols <- setdiff(required_law_cols, names(laws))
if (length(missing_law_cols) > 0L) {
  stop("Law file is missing columns: ", paste(missing_law_cols, collapse = ", "))
}

laws <- laws %>%
  transmute(
    state = as.character(.data$state),
    state_abbr = as.character(.data$state_abbr),
    state_fips = str_pad(as.character(.data$state_fips), 2L, "left", "0"),
    compulsory_law_year = as.integer(.data$compulsory_law_year),
    g_compulsory = as.integer(
      (.data$compulsory_law_year %/% 10L) * 10L
    )
  )
if (anyDuplicated(laws$state_fips)) stop("Duplicate state FIPS rows in law file.")

panel <- panel_raw %>%
  filter(.data$decade >= panel_year_min, .data$decade <= panel_year_max) %>%
  mutate(state_fips = substr(.data$GEOID, 1L, 2L)) %>%
  left_join(laws, by = "state_fips") %>%
  mutate(
    g_compulsory = if_else(
      is.na(.data$g_compulsory), 0L, as.integer(.data$g_compulsory)
    )
  )

common_finite <- is.finite(panel$population) & panel$population > 0 &
  is.finite(panel$county_births_estimate_decade) &
  panel$county_births_estimate_decade > 0
for (outcome in names(outcome_labels)) {
  common_finite <- common_finite & is.finite(panel[[outcome]])
}
panel <- panel[common_finite, , drop = FALSE]

pre_counts <- panel %>%
  mutate(is_pre = .data$g_compulsory == 0L | .data$decade < .data$g_compulsory) %>%
  group_by(.data$GEOID) %>%
  summarise(n_pre = sum(.data$is_pre), .groups = "drop")

panel <- panel %>%
  left_join(pre_counts, by = "GEOID") %>%
  filter(.data$n_pre >= min_pre) %>%
  mutate(g = .data$g_compulsory)

if (nrow(panel) == 0L) stop("No observations remain after sample restrictions.")
if (any(panel$n_pre < min_pre)) stop("Minimum pre-period restriction failed.")
if (any(
  panel %>% group_by(.data$state_fips) %>%
    summarise(n_g = n_distinct(.data$g_compulsory), .groups = "drop") %>%
    pull(.data$n_g) != 1L
)) {
  stop("Treatment timing is not constant within state.")
}

panel_before_balance <- panel
requested_support <- panel_before_balance %>%
  filter(.data$g_compulsory > 0L) %>%
  mutate(event_time = .data$decade - .data$g_compulsory) %>%
  group_by(.data$GEOID) %>%
  summarise(
    n_required_event_times_observed = sum(event_grid %in% .data$event_time),
    observed_event_times = paste(
      sort(intersect(event_grid, unique(.data$event_time))), collapse = ";"
    ),
    missing_event_times = paste(
      setdiff(event_grid, unique(.data$event_time)), collapse = ";"
    ),
    full_event_window = all(event_grid %in% .data$event_time),
    .groups = "drop"
  )

balance_audit <- panel_before_balance %>%
  distinct(
    .data$GEOID, .data$state_fips, .data$state_abbr, .data$state,
    .data$g_compulsory, .data$n_pre
  ) %>%
  left_join(requested_support, by = "GEOID") %>%
  mutate(
    n_required_event_times_observed = replace_na(
      .data$n_required_event_times_observed, 0L
    ),
    observed_event_times = replace_na(.data$observed_event_times, ""),
    missing_event_times = if_else(
      .data$g_compulsory == 0L,
      paste(event_grid, collapse = ";"),
      replace_na(.data$missing_event_times, paste(event_grid, collapse = ";"))
    ),
    full_event_window = replace_na(.data$full_event_window, FALSE),
    retained_by_profile = if (isTRUE(profile$balance_event_time)) {
      .data$g_compulsory > 0L & .data$full_event_window
    } else {
      TRUE
    },
    exclusion_reason = case_when(
      .data$retained_by_profile ~ "retained",
      .data$g_compulsory == 0L ~ "never_treated_no_relative_event_window",
      TRUE ~ "incomplete_required_event_window"
    )
  ) %>%
  arrange(.data$retained_by_profile, .data$state_fips, .data$GEOID)

if (isTRUE(profile$balance_event_time)) {
  eligible_geoids <- balance_audit %>%
    filter(.data$retained_by_profile) %>%
    pull(.data$GEOID)
  panel <- panel_before_balance %>%
    filter(.data$GEOID %in% eligible_geoids)
}

declared_window <- panel %>%
  filter(.data$g_compulsory > 0L) %>%
  mutate(event_time = .data$decade - .data$g_compulsory) %>%
  filter(.data$event_time %in% event_grid)

if (isTRUE(profile$balance_event_time)) {
  balanced_counts <- declared_window %>%
    count(.data$GEOID, name = "n_event_times")
  if (nrow(balanced_counts) != n_distinct(panel$GEOID) ||
      any(balanced_counts$n_event_times != length(event_grid))) {
    stop("Balanced profile does not have every event time for every county.")
  }

  balanced_counties <- n_distinct(panel$GEOID)
  balanced_states <- n_distinct(panel$state_fips)
  if (balanced_counties != profile$expected_counties ||
      balanced_states != profile$expected_states) {
    stop(
      "Balanced profile sample differs from validated support: expected ",
      profile$expected_counties, " counties / ", profile$expected_states,
      " states; got ", balanced_counties, " / ", balanced_states, "."
    )
  }
}

###############################################################################
# Sun-Abraham event studies: levels and cohort-specific linear detrending
###############################################################################

extract_cohort_event_study <- function(
    mod, model_data, outcome_name, outcome_label, specification) {
  term_pattern <- "^decade::(-?[0-9]+):cohort::([0-9]+)$"

  raw_ct <- as.data.frame(summary(mod, agg = FALSE)$coeftable)
  raw_ct$term <- rownames(raw_ct)
  raw_cells <- raw_ct %>%
    as_tibble() %>%
    mutate(
      parsed = str_match(.data$term, term_pattern),
      event_time = as.integer(.data$parsed[, 2L]),
      cohort = as.integer(.data$parsed[, 3L])
    ) %>%
    filter(
      !is.na(.data$event_time), !is.na(.data$cohort),
      .data$event_time %in% event_grid
    ) %>%
    transmute(
      cohort = .data$cohort,
      event_time = .data$event_time,
      att_raw = .data$Estimate,
      se_raw = .data$`Std. Error`
    )

  collinear_terms <- tibble(
    term = if (is.null(mod$collin.var)) character() else mod$collin.var
  ) %>%
    mutate(
      parsed = str_match(.data$term, term_pattern),
      event_time = as.integer(.data$parsed[, 2L]),
      cohort = as.integer(.data$parsed[, 3L])
    ) %>%
    filter(
      !is.na(.data$event_time), !is.na(.data$cohort),
      .data$event_time %in% event_grid
    ) %>%
    distinct(.data$cohort, .data$event_time) %>%
    mutate(was_collinear = TRUE)

  cohort_counts <- model_data %>%
    filter(.data$g_compulsory > 0L) %>%
    distinct(
      .data$GEOID, .data$state_fips,
      cohort = .data$g_compulsory
    ) %>%
    group_by(.data$cohort) %>%
    summarise(
      n_counties_cohort = n_distinct(.data$GEOID),
      n_states_cohort = n_distinct(.data$state_fips),
      .groups = "drop"
    )

  cohort_grid <- crossing(
    cohort = sort(unique(model_data$g_compulsory[model_data$g_compulsory > 0L])),
    event_time = event_grid
  ) %>%
    left_join(raw_cells, by = c("cohort", "event_time")) %>%
    left_join(collinear_terms, by = c("cohort", "event_time")) %>%
    left_join(cohort_counts, by = "cohort") %>%
    mutate(
      was_collinear = replace_na(.data$was_collinear, FALSE),
      support_status = case_when(
        .data$event_time == ref_event ~ "reference",
        is.finite(.data$att_raw) & is.finite(.data$se_raw) ~ "estimated",
        .data$was_collinear ~ "collinear",
        TRUE ~ "not_returned"
      ),
      identified = .data$support_status %in% c("reference", "estimated"),
      att = if_else(
        .data$support_status == "reference", 0, .data$att_raw
      ),
      se = if_else(
        .data$support_status == "reference", 0, .data$se_raw
      ),
      conf_low = .data$att - 1.96 * .data$se,
      conf_high = .data$att + 1.96 * .data$se,
      calendar_decade = .data$cohort + .data$event_time,
      outcome = outcome_name,
      outcome_label = outcome_label,
      specification = specification,
      specification_label = if_else(
        specification == "levels", "Levels", "Cohort-linear detrended"
      ),
      n_obs = nobs(mod),
      n_counties = n_distinct(model_data$GEOID),
      n_states = n_distinct(model_data$state_fips),
      .before = 1L
    ) %>%
    select(-"att_raw", -"se_raw", -"was_collinear")

  unexpected <- cohort_grid %>%
    filter(.data$support_status == "not_returned")
  if (nrow(unexpected) > 0L) {
    stop(
      "Sun-Abraham did not return or flag ", nrow(unexpected),
      " requested cohort/event cells for ", outcome_name, " / ",
      specification, "."
    )
  }
  cohort_grid
}

run_event_study <- function(outcome, specification) {
  outcome_name <- outcome
  outcome_label <- unname(outcome_labels[[outcome_name]])
  model_data <- panel
  model_outcome <- outcome_name
  if (specification == "detrended") {
    model_data <- detrend_cohort(
      model_data, outcome_name, min_pre_obs = min_pre
    )
    if (!all(model_data$detrended)) {
      stop("At least one cohort could not be detrended for ", outcome_name)
    }
    model_outcome <- "y_resid"
  }

  model_data <- model_data %>%
    mutate(
      g_sa = if_else(
        .data$g_compulsory == 0L, 10000L,
        as.integer(.data$g_compulsory)
      )
    )

  fml <- as.formula(paste0(
    model_outcome,
    " ~ sunab(g_sa, decade, ref.p = ", ref_event, ") | GEOID + decade"
  ))
  mod <- suppressMessages(suppressWarnings(
    feols(fml, data = model_data, cluster = ~state_fips, warn = FALSE)
  ))

  ct <- as.data.frame(summary(mod, agg = "period")$coeftable)
  ct$term <- rownames(ct)
  dynamic <- ct %>%
    as_tibble() %>%
    mutate(
      event_time = as.integer(str_extract(.data$term, "(?<=::)-?\\d+"))
    ) %>%
    filter(.data$event_time %in% event_grid) %>%
    transmute(
      event_time = .data$event_time,
      att = .data$Estimate,
      se = .data$`Std. Error`
    )

  dynamic <- ensure_reference_zero(dynamic, ref_event = ref_event) %>%
    arrange(.data$event_time)
  if (anyDuplicated(dynamic$event_time)) {
    stop(
      "Duplicate event-time estimates for ", outcome_name, " / ",
      specification
    )
  }
  if (!identical(dynamic$event_time, event_grid)) {
    stop(
      "Incomplete event grid for ", outcome_name, " / ", specification,
      ": got ", paste(dynamic$event_time, collapse = ", ")
    )
  }
  if (any(!is.finite(dynamic$att)) || any(!is.finite(dynamic$se))) {
    stop("Non-finite estimates for ", outcome_name, " / ", specification)
  }
  ref_row <- dynamic %>% filter(.data$event_time == ref_event)
  if (nrow(ref_row) != 1L || ref_row$att != 0 || ref_row$se != 0) {
    stop("Reference period was not normalized to zero.")
  }

  pooled <- dynamic %>%
    mutate(
      outcome = outcome_name,
      outcome_label = outcome_label,
      specification = specification,
      specification_label = if_else(
        specification == "levels", "Levels", "Cohort-linear detrended"
      ),
      conf_low = .data$att - 1.96 * .data$se,
      conf_high = .data$att + 1.96 * .data$se,
      n_obs = nobs(mod),
      n_counties = n_distinct(model_data$GEOID),
      n_states = n_distinct(model_data$state_fips),
      .before = 1L
    )

  cohort <- if (isTRUE(profile$balance_event_time)) {
    extract_cohort_event_study(
      mod, model_data, outcome_name, outcome_label, specification
    )
  } else {
    tibble()
  }
  list(pooled = pooled, cohort = cohort)
}

specifications <- c("levels", "detrended")
model_results <- crossing(
  outcome = names(outcome_labels),
  specification = specifications
) %>%
  mutate(
    result = map2(
      .data$outcome, .data$specification, run_event_study
    )
  )

event_results <- model_results %>%
  transmute(estimates = map(.data$result, "pooled")) %>%
  select("estimates") %>%
  unnest("estimates")

cohort_event_results <- model_results %>%
  transmute(estimates = map(.data$result, "cohort")) %>%
  select("estimates") %>%
  unnest("estimates")

if (nrow(event_results) !=
    length(outcome_labels) * length(specifications) * length(event_grid)) {
  stop("Unexpected number of event-study coefficient rows.")
}

write_csv(
  event_results,
  file.path(results_dir, "compulsory_amws_event_study_coefficients.csv")
)

###############################################################################
# Treatment-cohort event studies for the balanced profile
###############################################################################

if (isTRUE(profile$balance_event_time)) {
  cohort_results_dir <- file.path(results_dir, "per_cohort")
  dir.create(cohort_results_dir, recursive = TRUE, showWarnings = FALSE)

  expected_cohorts <- seq(1850L, 1910L, by = 10L)
  observed_cohorts <- sort(unique(cohort_event_results$cohort))
  if (!identical(observed_cohorts, expected_cohorts)) {
    stop(
      "Unexpected treatment cohorts in cohort event studies: ",
      paste(observed_cohorts, collapse = ", ")
    )
  }

  expected_cohort_rows <- length(expected_cohorts) * length(event_grid) *
    length(outcome_labels) * length(specifications)
  if (nrow(cohort_event_results) != expected_cohort_rows) {
    stop(
      "Unexpected cohort-grid size: expected ", expected_cohort_rows,
      ", got ", nrow(cohort_event_results), "."
    )
  }
  if (anyDuplicated(cohort_event_results[c(
    "outcome", "specification", "cohort", "event_time"
  )])) {
    stop("Duplicate outcome/specification/cohort/event-time rows.")
  }

  reference_rows <- cohort_event_results %>%
    filter(.data$support_status == "reference")
  expected_reference_rows <- length(expected_cohorts) *
    length(outcome_labels) * length(specifications)
  if (nrow(reference_rows) != expected_reference_rows ||
      any(reference_rows$att != 0) || any(reference_rows$se != 0)) {
    stop("Cohort reference periods were not normalized to exact zeros.")
  }

  unsupported_rows <- cohort_event_results %>%
    filter(!.data$identified)
  expected_unsupported <- tibble(
    cohort = c(1850L, 1850L, 1850L, 1910L),
    event_time = c(-40L, -30L, -20L, 50L)
  )
  observed_unsupported <- unsupported_rows %>%
    distinct(.data$cohort, .data$event_time)
  if (!identical(
    arrange(observed_unsupported, .data$cohort, .data$event_time),
    arrange(expected_unsupported, .data$cohort, .data$event_time)
  )) {
    stop("Unexpected unsupported cohort/event-time pattern.")
  }
  if (nrow(unsupported_rows) !=
      nrow(expected_unsupported) * length(outcome_labels) *
        length(specifications)) {
    stop("Unexpected number of unsupported cohort/event-time rows.")
  }

  identified_rows <- cohort_event_results %>%
    filter(.data$identified)
  if (any(!is.finite(identified_rows$att)) ||
      any(!is.finite(identified_rows$se)) ||
      any(!is.finite(identified_rows$conf_low)) ||
      any(!is.finite(identified_rows$conf_high))) {
    stop("Non-finite identified cohort estimates found.")
  }

  cohort_event_results <- cohort_event_results %>%
    arrange(
      .data$outcome, .data$specification,
      .data$cohort, .data$event_time
    )
  write_csv(
    cohort_event_results,
    file.path(
      cohort_results_dir,
      "compulsory_amws_cohort_event_study_coefficients.csv"
    ),
    na = ""
  )

  cohort_support <- cohort_event_results %>%
    group_by(
      .data$outcome, .data$specification, .data$cohort,
      .data$n_counties_cohort, .data$n_states_cohort
    ) %>%
    summarise(
      requested_event_times = n(),
      identified_event_times = sum(.data$identified),
      estimated_event_times = sum(.data$support_status == "estimated"),
      reference_event_times = sum(.data$support_status == "reference"),
      unsupported_event_times = sum(!.data$identified),
      unsupported_event_time_values = paste(
        .data$event_time[!.data$identified], collapse = ";"
      ),
      .groups = "drop"
    ) %>%
    arrange(.data$outcome, .data$specification, .data$cohort)
  write_csv(
    cohort_support,
    file.path(cohort_results_dir, "compulsory_amws_cohort_event_support.csv")
  )

  cohort_label_lookup <- cohort_event_results %>%
    distinct(
      .data$cohort, .data$n_counties_cohort, .data$n_states_cohort
    ) %>%
    arrange(.data$cohort) %>%
    mutate(
      cohort_label = paste0(
        "g = ", .data$cohort, "\n",
        .data$n_counties_cohort, " counties; ",
        .data$n_states_cohort,
        if_else(.data$n_states_cohort == 1L, " state", " states")
      )
    )

  for (outcome in names(outcome_labels)) {
    plot_data <- cohort_event_results %>%
      filter(.data$outcome == .env$outcome, .data$identified) %>%
      left_join(cohort_label_lookup, by = c(
        "cohort", "n_counties_cohort", "n_states_cohort"
      )) %>%
      mutate(
        cohort_label = factor(
          .data$cohort_label,
          levels = cohort_label_lookup$cohort_label
        ),
        specification_label = factor(
          .data$specification_label,
          levels = c("Levels", "Cohort-linear detrended")
        ),
        period = factor(
          if_else(.data$event_time < 0L, "Pre", "Post"),
          levels = c("Pre", "Post")
        )
      )

    p_cohort <- ggplot(
      plot_data,
      aes(
        x = .data$event_time, y = .data$att,
        ymin = .data$conf_low, ymax = .data$conf_high,
        color = .data$period
      )
    ) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
      geom_errorbar(width = 1) +
      geom_point(size = 1.7) +
      facet_grid(specification_label ~ cohort_label, scales = "fixed") +
      scale_x_continuous(breaks = event_grid) +
      scale_color_manual(
        values = c(Pre = "#e87d72", Post = "#56bcc2"),
        drop = FALSE
      ) +
      labs(
        title = "Compulsory schooling laws and AMWS births by treatment cohort",
        subtitle = paste0(
          outcome_labels[[outcome]],
          " | Sun-Abraham cohort cells | Ref: e=", ref_event,
          " | State-clustered SEs"
        ),
        caption = paste0(
          "Balanced e=-40 to +50 sample. Blank boundary cells are collinear: ",
          "g=1850 at e=-40,-30,-20; g=1910 at e=+50."
        ),
        x = "Years relative to compulsory-law passage",
        y = "Estimated effect",
        color = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 9),
        legend.position = "bottom"
      )

    ggsave(
      file.path(
        cohort_results_dir,
        paste0(
          "compulsory_amws_cohort_event_study_",
          outcome_slugs[[outcome]], ".png"
        )
      ),
      p_cohort, width = 24, height = 8, dpi = 300
    )
  }

  cohort_notes <- c(
    "Balanced compulsory-schooling AMWS treatment-cohort event studies.",
    "",
    paste0(
      "Estimator: unaggregated Sun-Abraham cohort-by-event-time cells from ",
      "the same models used for the pooled balanced event studies."
    ),
    paste0(
      "Sample: ", n_distinct(panel$GEOID), " counties in ",
      n_distinct(panel$state_fips), " states; requested event window [",
      min(event_grid), ", ", max(event_grid), "]; reference e = ",
      ref_event, "."
    ),
    paste0(
      "There are no never-treated states in the balanced sample, so these ",
      "figures retain the pooled Sun-Abraham comparison structure rather ",
      "than applying the college-allocation never-treated CSDID routine."
    ),
    paste0(
      "Unsupported collinear boundary cells: g=1850 at e=-40,-30,-20; ",
      "g=1910 at e=+50."
    ),
    paste0("Generated: ", Sys.Date(), ".")
  )
  writeLines(
    cohort_notes,
    file.path(cohort_results_dir, "notes.txt")
  )
}

###############################################################################
# Sample and treatment-support diagnostics
###############################################################################

sample_summary <- tibble(
  metric = c(
    "profile", "balance_definition",
    "input_rows", "input_counties", "input_min_decade", "input_max_decade",
    "duplicate_geoid_decade_rows", "amws_1906_1955_dedup",
    "amws_1986", "amws_total", "component_reconciliation_failures",
    "pre_balance_rows", "pre_balance_counties", "pre_balance_states",
    "estimation_rows", "estimation_counties", "estimation_states",
    "treated_states", "never_treated_states", "minimum_pre_periods",
    "event_time_min", "event_time_max", "reference_event_time",
    "declared_window_rows", "declared_window_counties",
    "declared_window_states", "required_event_times_per_county"
  ),
  value = as.character(c(
    profile_name,
    if (isTRUE(profile$balance_event_time)) {
      "complete_relative_event_time"
    } else {
      "unbalanced_relative_event_time"
    },
    nrow(panel_raw), n_distinct(panel_raw$GEOID), min(panel_raw$decade),
    max(panel_raw$decade), nrow(duplicate_keys), amws_totals$early,
    amws_totals$ed1986, amws_totals$total, 0L,
    nrow(panel_before_balance), n_distinct(panel_before_balance$GEOID),
    n_distinct(panel_before_balance$state_fips), nrow(panel),
    n_distinct(panel$GEOID), n_distinct(panel$state_fips),
    n_distinct(panel$state_fips[panel$g_compulsory > 0L]),
    n_distinct(panel$state_fips[panel$g_compulsory == 0L]),
    min_pre, min(event_grid), max(event_grid), ref_event,
    nrow(declared_window), n_distinct(declared_window$GEOID),
    n_distinct(declared_window$state_fips), length(event_grid)
  ))
)
write_csv(
  sample_summary,
  file.path(results_dir, "compulsory_amws_sample_summary.csv")
)
write_csv(
  balance_audit,
  file.path(results_dir, "compulsory_amws_balance_audit.csv")
)

treatment_support <- panel %>%
  distinct(
    .data$state_fips, .data$state_abbr, .data$state,
    .data$g_compulsory, .data$GEOID, .data$decade
  ) %>%
  group_by(
    .data$state_fips, .data$state_abbr, .data$state, .data$g_compulsory
  ) %>%
  summarise(
    n_counties = n_distinct(.data$GEOID),
    n_observations = n(),
    min_decade = min(.data$decade),
    max_decade = max(.data$decade),
    .groups = "drop"
  ) %>%
  arrange(.data$g_compulsory, .data$state_fips)
write_csv(
  treatment_support,
  file.path(results_dir, "compulsory_amws_treatment_support.csv")
)

event_support <- declared_window %>%
  group_by(.data$event_time) %>%
  summarise(
    treated_observations = n(),
    treated_counties = n_distinct(.data$GEOID),
    treated_states = n_distinct(.data$state_fips),
    .groups = "drop"
  ) %>%
  complete(
    event_time = event_grid,
    fill = list(
      treated_observations = 0L, treated_counties = 0L, treated_states = 0L
    )
  ) %>%
  arrange(.data$event_time)
if (isTRUE(profile$balance_event_time)) {
  if (any(event_support$treated_counties != n_distinct(panel$GEOID)) ||
      any(event_support$treated_states != n_distinct(panel$state_fips)) ||
      any(event_support$treated_observations != n_distinct(panel$GEOID))) {
    stop("Reported event-time support is not constant in the balanced profile.")
  }
}
write_csv(
  event_support,
  file.path(results_dir, "compulsory_amws_event_support.csv")
)

###############################################################################
# Event-study figures
###############################################################################

for (outcome in names(outcome_labels)) {
  plot_data <- event_results %>% filter(.data$outcome == .env$outcome)
  p <- ggplot(plot_data, aes(x = .data$event_time, y = .data$att)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    geom_errorbar(
      aes(ymin = .data$conf_low, ymax = .data$conf_high), width = 1
    ) +
    geom_point(size = 2) +
    facet_wrap(~specification_label, ncol = 2L) +
    scale_x_continuous(breaks = event_grid) +
    labs(
      title = "Compulsory schooling laws and AMWS scientist births",
      subtitle = paste0(
        outcome_labels[[outcome]],
        " | Sun-Abraham | Ref: e=", ref_event, " | State-clustered SEs",
        if (isTRUE(profile$balance_event_time)) {
          paste0(" | Balanced e=", min(event_grid), " to +", max(event_grid))
        } else {
          ""
        }
      ),
      x = "Years relative to compulsory-law passage",
      y = "Estimated effect"
    ) +
    theme_minimal(base_size = 13)

  ggsave(
    file.path(
      results_dir,
      paste0("compulsory_amws_event_study_", outcome_slugs[[outcome]], ".png")
    ),
    p, width = 12, height = 6
  )
}

###############################################################################
# Outcome-invariant diagnostic: elite-school timing versus compulsory laws
###############################################################################

elite_schools <- read_csv(
  schools_file,
  col_types = cols(county_geoid = col_character()),
  show_col_types = FALSE
) %>%
  filter(.data$crit_high_access_strict == "yes") %>%
  mutate(
    county_geoid = str_pad(as.character(.data$county_geoid), 5L, "left", "0"),
    g_elite = as.integer((.data$founding_year_used %/% 10L) * 10L),
    state_fips = substr(.data$county_geoid, 1L, 2L)
  ) %>%
  select(
    "state", "state_abbr", "state_fips", "county_geoid", "school",
    "founding_year_used", "g_elite"
  )

elite_with_law <- elite_schools %>%
  left_join(
    laws %>% select("state_fips", "g_compulsory"),
    by = "state_fips"
  ) %>%
  mutate(elite_before_law = .data$g_elite < .data$g_compulsory)

corr_raw <- cor(
  elite_with_law$g_elite,
  elite_with_law$g_compulsory,
  use = "complete.obs"
)
write_csv(
  elite_with_law %>% mutate(raw_timing_correlation = corr_raw),
  file.path(results_dir, "compulsory_vs_elite_timing.csv")
)

p_corr <- ggplot(
  elite_with_law,
  aes(
    x = .data$g_compulsory,
    y = .data$g_elite,
    label = paste0(.data$state_abbr, ": ", .data$school)
  )
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 3, color = "#2166ac") +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  scale_x_continuous(breaks = seq(1850, 1920, by = 10)) +
  scale_y_continuous(breaks = seq(1830, 1940, by = 10)) +
  labs(
    title = "Elite school opening vs. compulsory-attendance law",
    subtitle = paste0(
      "Strict high-access schools; raw timing correlation = ",
      sprintf("%.3f", corr_raw)
    ),
    x = "Decade state passed compulsory-attendance law",
    y = "Decade elite school opened",
    caption = paste0(
      "Elite schools: crit_high_access_strict == 'yes'. ",
      "Law dates: Lleras-Muney (2002)."
    )
  )
ggsave(
  file.path(results_dir, "compulsory_vs_elite_correlation.png"),
  p_corr, width = 9, height = 7
)

elapsed <- difftime(Sys.time(), initial_time, units = "secs")
cat("\n=== Mandatory schooling with AMWS outcomes ===\n")
cat("Profile:", profile_name, "\n")
cat("AMWS births:", amws_totals$total, "\n")
cat("Estimation rows:", nrow(panel), "\n")
cat("Counties:", n_distinct(panel$GEOID), "\n")
cat("States:", n_distinct(panel$state_fips), "\n")
cat("Raw elite/law timing correlation:", round(corr_raw, 3), "\n")
cat("Completed in", round(as.numeric(elapsed), 1), "seconds.\n")
cat("Results in:", results_dir, "\n")
