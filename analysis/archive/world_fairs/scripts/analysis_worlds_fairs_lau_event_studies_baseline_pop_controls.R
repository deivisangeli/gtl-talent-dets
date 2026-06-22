###############################################################################
# Project: GTL Talent Determinants
# Goal: LAU-level event studies using world's fairs as treatment, controlling
#       for baseline population in 1800
#
# Main sample:
#   - European LAUs from the Europe-only LAU inventor panel
#
# Treatment:
#   - first world's fair in the unit between 1800 and 1900, inclusive
#   - outcome panel covers 1700-1960, so 1700-1799 is an added pre-period
#   - multi-year fair entries use the first listed year
#   - timing mirrors scientific-facilities scripts:
#       g_std   = floor(event_year / 10) * 10
#       g_shift = years ending in 7-9 shifted to the next decade
#
# Run from analysis/:
#   Rscript analysis_worlds_fairs_lau_event_studies_baseline_pop_controls.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(did)
  library(sf)
})

initial_time <- Sys.time()
options(timeout = 1000)

source("../paths.R")

results_dir <- file.path("results", "worlds_fairs_lau_event_studies_baseline_pop_controls")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Helpers
###############################################################################

value_or <- function(x, default) {
  if (is.null(x)) default else x
}

mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(NA_character_)
  }

  as.character(x[[1]])
}

sanitize_filename <- function(x) {
  str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

parse_first_year <- function(year_text, year_start) {
  parsed <- suppressWarnings(as.integer(str_extract(as.character(year_text), "[0-9]{4}")))
  if_else(is.na(as.integer(year_start)), parsed, as.integer(year_start))
}

standard_decade <- function(year) {
  as.integer(floor(year / 10) * 10)
}

shifted_decade <- function(year) {
  as.integer(ifelse(year %% 10 >= 7, floor(year / 10) * 10 + 10, floor(year / 10) * 10))
}

extract_dynamic_att <- function(es, outcome, sample_name, timing, control_spec) {
  tibble(
    outcome = outcome,
    sample = sample_name,
    timing = timing,
    control_spec = control_spec,
    control_group = "notyettreated",
    event_time = es$egt,
    estimate = es$att.egt,
    se = es$se.egt,
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se
  )
}

extract_simple_att <- function(ag, outcome, sample_name, timing, control_spec) {
  tibble(
    outcome = outcome,
    sample = sample_name,
    timing = timing,
    control_spec = control_spec,
    control_group = "notyettreated",
    estimate = ag$overall.att,
    se = ag$overall.se,
    p_value = 2 * (1 - pnorm(abs(estimate / se))),
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se
  )
}

dynamic_y_limits <- function(dynamic_att) {
  y_values <- dynamic_att %>%
    select(estimate, ci_low, ci_high) %>%
    unlist(use.names = FALSE)

  y_values <- y_values[is.finite(y_values)]
  max_abs <- max(abs(y_values), na.rm = TRUE)

  if (!is.finite(max_abs) || max_abs == 0) {
    max_abs <- 1
  }

  c(-1.1 * max_abs, 1.1 * max_abs)
}

control_title <- function(control_spec) {
  switch(
    control_spec,
    population_level = "Control: population_baseline_1800",
    population_log = "Control: log_population_baseline_1800",
    paste0("Control: ", control_spec)
  )
}

plot_dynamic_event_study <- function(es, outcome, sample_name, timing,
                                     control_spec, y_limits) {
  did::ggdid(es) +
    labs(
      x = "Relative Time",
      y = "Effect",
      title = str_wrap(
        paste(
          "World's fairs event study -",
          sample_name,
          timing,
          control_title(control_spec),
          outcome
        ),
        width = 72
      )
    ) +
    coord_cartesian(ylim = y_limits)
}

check_balanced_panel <- function(data, id_col, time_col) {
  id_col <- rlang::ensym(id_col)
  time_col <- rlang::ensym(time_col)

  n_times <- data %>% summarise(n = n_distinct(!!time_col)) %>% pull(n)
  counts <- data %>% count(!!id_col, name = "n_periods")

  list(
    is_balanced = all(counts$n_periods == n_times),
    n_units = nrow(counts),
    n_periods = n_times,
    min_periods_per_unit = min(counts$n_periods, na.rm = TRUE),
    max_periods_per_unit = max(counts$n_periods, na.rm = TRUE)
  )
}

complete_control_units <- function(data, controls) {
  data %>%
    distinct(unit_num, across(all_of(controls))) %>%
    filter(if_all(all_of(controls), ~ !is.na(.x))) %>%
    pull(unit_num)
}

prepare_controls_for_spec <- function(data, controls) {
  analysis_units <- complete_control_units(data, controls)

  data <- data %>%
    filter(unit_num %in% analysis_units)

  formula_vars <- controls[
    map_lgl(data[controls], ~ n_distinct(.x, na.rm = TRUE) > 1)
  ]

  list(data = data, formula_vars = formula_vars)
}

run_event_study <- function(data, outcome, sample_name, timing, gname,
                            controls, control_spec, window = 70, cores = 4) {
  data_es <- data %>%
    select(unit_num, unit_id, decade, all_of(gname), all_of(outcome), all_of(controls)) %>%
    rename(g = all_of(gname), y = all_of(outcome)) %>%
    mutate(
      unit_num = as.numeric(unit_num),
      decade = as.numeric(decade),
      g = as.numeric(g),
      y = as.numeric(y)
    ) %>%
    filter(!is.na(y), is.finite(y))

  control_prep <- prepare_controls_for_spec(data_es, controls)
  data_es <- control_prep$data

  if (nrow(data_es) == 0) {
    return(list(
      ok = FALSE,
      outcome = outcome,
      sample = sample_name,
      timing = timing,
      control_spec = control_spec,
      formula_vars = list(control_prep$formula_vars),
      error = "No rows after outcome and control missingness filters."
    ))
  }

  first_period <- min(data_es$decade, na.rm = TRUE)
  first_period_treated_units <- data_es %>%
    filter(g == first_period) %>%
    distinct(unit_id) %>%
    pull(unit_id)

  data_es <- data_es %>%
    filter(g == 0 | g > first_period)

  if (n_distinct(data_es$g[data_es$g > 0]) == 0) {
    return(list(
      ok = FALSE,
      outcome = outcome,
      sample = sample_name,
      timing = timing,
      control_spec = control_spec,
      formula_vars = list(control_prep$formula_vars),
      error = "No treated cohorts in estimation sample after dropping first-period-treated units.",
      n_first_period_treated_units_dropped = length(first_period_treated_units)
    ))
  }

  if (n_distinct(data_es$y, na.rm = TRUE) < 2) {
    return(list(
      ok = FALSE,
      outcome = outcome,
      sample = sample_name,
      timing = timing,
      control_spec = control_spec,
      formula_vars = list(control_prep$formula_vars),
      error = "Outcome has insufficient variation.",
      n_first_period_treated_units_dropped = length(first_period_treated_units)
    ))
  }

  tryCatch(
    {
      xformla <- if (length(control_prep$formula_vars) > 0) {
        as.formula(paste("~", paste(control_prep$formula_vars, collapse = " + ")))
      } else {
        ~ 1
      }

      out <- did::att_gt(
        yname = "y",
        tname = "decade",
        idname = "unit_num",
        gname = "g",
        xformla = xformla,
        data = data_es,
        control_group = "notyettreated",
        est_method = "dr",
        base_period = "universal",
        cores = cores
      )

      es <- did::aggte(
        out,
        type = "dynamic",
        na.rm = TRUE,
        min_e = -window,
        max_e = window
      )

      simple <- did::aggte(out, type = "simple", na.rm = TRUE)

      list(
        ok = TRUE,
        outcome = outcome,
        sample = sample_name,
        timing = timing,
        control_spec = control_spec,
        out = out,
        es = es,
        simple = simple,
        n_rows = nrow(data_es),
        n_units = n_distinct(data_es$unit_num),
        n_treated_units = n_distinct(data_es$unit_num[data_es$g > 0]),
        n_control_units = n_distinct(data_es$unit_num[data_es$g == 0]),
        n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
        n_first_period_treated_units_dropped = length(first_period_treated_units),
        min_decade = min(data_es$decade, na.rm = TRUE),
        max_decade = max(data_es$decade, na.rm = TRUE),
        formula_vars = list(control_prep$formula_vars),
        error = NA_character_
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        outcome = outcome,
        sample = sample_name,
        timing = timing,
        control_spec = control_spec,
        n_rows = nrow(data_es),
        n_units = n_distinct(data_es$unit_num),
        n_treated_units = n_distinct(data_es$unit_num[data_es$g > 0]),
        n_control_units = n_distinct(data_es$unit_num[data_es$g == 0]),
        n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
        n_first_period_treated_units_dropped = length(first_period_treated_units),
        min_decade = min(data_es$decade, na.rm = TRUE),
        max_decade = max(data_es$decade, na.rm = TRUE),
        formula_vars = list(control_prep$formula_vars),
        error = conditionMessage(e)
      )
    }
  )
}

###############################################################################
# Load data
###############################################################################

panel_file <- file.path(DATA_OUTPUT, "europe_lau_inventor_panel_1700_1960_balanced_rates.csv")
fairs_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia_geocoded.xlsx")
lau_boundaries_file <- file.path(DATA_OUTPUT, "europe_lau_boundaries_2024.gpkg")

if (!file.exists(panel_file)) {
  stop("Missing inventor panel: ", panel_file)
}

if (!file.exists(fairs_file)) {
  stop("Missing geocoded fairs file: ", fairs_file)
}

if (!file.exists(lau_boundaries_file)) {
  stop("Missing GISCO LAU boundaries file: ", lau_boundaries_file)
}

panel_year <- read_csv(panel_file, show_col_types = FALSE) %>%
  mutate(
    unit_id = as.character(unit_id),
    unit_type = as.character(unit_type),
    GEOID = as.character(GEOID),
    lau_id = as.character(lau_id),
    city_geonameid = suppressWarnings(as.integer(city_geonameid)),
    year = as.integer(year)
  ) %>%
  filter(year >= 1700, year <= 1960)

fairs_raw <- read_xlsx(fairs_file) %>%
  mutate(
    fair_row_id = row_number(),
    event_year = parse_first_year(Year, year_start),
    geonameid = suppressWarnings(as.integer(geonameid)),
    matched_country_iso3 = as.character(matched_country_iso3),
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    in_treatment_window = !is.na(event_year) & event_year >= 1800 & event_year <= 1900
  )

###############################################################################
# Build fair-to-panel-unit matches
###############################################################################

europe_lau_units <- panel_year %>%
  filter(unit_type == "europe_lau") %>%
  distinct(
    lau_id,
    europe_unit_id = unit_id,
    europe_place_name = place_name,
    europe_country = country,
    europe_iso3 = iso3
  ) %>%
  filter(!is.na(lau_id))

lau_poly <- st_read(lau_boundaries_file, quiet = TRUE) %>%
  st_transform(4326) %>%
  select(GISCO_ID, CNTR_CODE, LAU_NAME)

europe_fairs_spatial <- fairs_raw %>%
  filter(
    in_treatment_window,
    matched_country_iso3 != "USA",
    !is.na(lon),
    !is.na(lat)
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_join(lau_poly, join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  select(fair_row_id, fair_lau_id = GISCO_ID, fair_lau_iso2 = CNTR_CODE, fair_lau_name = LAU_NAME) %>%
  left_join(europe_lau_units, by = c("fair_lau_id" = "lau_id"))

fair_match_audit <- fairs_raw %>%
  left_join(europe_fairs_spatial, by = "fair_row_id") %>%
  mutate(
    assigned_unit_type = case_when(
      matched_country_iso3 != "USA" & !is.na(europe_unit_id) ~ "europe_lau",
      TRUE ~ NA_character_
    ),
    assigned_unit_id = case_when(
      assigned_unit_type == "europe_lau" ~ europe_unit_id,
      TRUE ~ NA_character_
    ),
    assigned_lau_id = if_else(assigned_unit_type == "europe_lau", fair_lau_id, NA_character_),
    match_status = case_when(
      is.na(event_year) ~ "missing_event_year",
      !in_treatment_window ~ "outside_1800_1900",
      matched_country_iso3 == "USA" ~ "usa_excluded_from_europe_panel",
      matched_country_iso3 != "USA" & is.na(fair_lau_id) ~ "europe_no_lau_spatial_match",
      matched_country_iso3 != "USA" & is.na(europe_unit_id) ~ "europe_lau_not_in_panel",
      !is.na(assigned_unit_id) ~ "matched_to_panel",
      TRUE ~ "unmatched"
    )
  ) %>%
  select(
    fair_row_id,
    Year,
    City,
    Country,
    Fair_name,
    event_year,
    in_treatment_window,
    lat,
    lon,
    geonameid,
    matched_name,
    matched_country_iso3,
    assigned_unit_type,
    assigned_unit_id,
    assigned_lau_id,
    fair_lau_iso2,
    fair_lau_name,
    europe_place_name,
    europe_country,
    europe_iso3,
    match_status
  )

treatment_assignment <- fair_match_audit %>%
  filter(match_status == "matched_to_panel") %>%
  group_by(assigned_unit_type, assigned_unit_id) %>%
  summarise(
    event_year = min(event_year, na.rm = TRUE),
    first_fair_name = Fair_name[which.min(event_year)][[1]],
    first_fair_city = City[which.min(event_year)][[1]],
    first_fair_country = Country[which.min(event_year)][[1]],
    n_fairs_1800_1900 = n(),
    fair_years_1800_1900 = paste(sort(unique(event_year)), collapse = ";"),
    fair_names_1800_1900 = paste(unique(Fair_name), collapse = " | "),
    .groups = "drop"
  ) %>%
  transmute(
    unit_type = assigned_unit_type,
    unit_id = assigned_unit_id,
    event_year,
    g_std = standard_decade(event_year),
    g_shift = shifted_decade(event_year),
    first_fair_name,
    first_fair_city,
    first_fair_country,
    n_fairs_1800_1900,
    fair_years_1800_1900,
    fair_names_1800_1900
  )

write_csv(
  fair_match_audit,
  file.path(results_dir, "worlds_fairs_lau_baseline_pop_controls_match_audit.csv")
)
write_csv(
  treatment_assignment,
  file.path(results_dir, "worlds_fairs_lau_baseline_pop_controls_treatment_assignment.csv")
)

###############################################################################
# Aggregate annual outcome panel to decades
###############################################################################

panel_decade <- panel_year %>%
  mutate(decade = standard_decade(year)) %>%
  group_by(
    unit_type,
    unit_id,
    GEOID,
    lau_id,
    city_geonameid,
    place_name,
    place_name_ascii,
    country,
    iso3,
    lat,
    lon,
    decade
  ) %>%
  summarise(
    n_inventors = sum(n_inventors, na.rm = TRUE),
    n_stem = sum(n_stem, na.rm = TRUE),
    n_nonstem = sum(n_nonstem, na.rm = TRUE),
    population = mean_or_na(population),
    source_panel = first_nonmissing(source_panel),
    .groups = "drop"
  ) %>%
  mutate(
    any_inventor = as.integer(n_inventors > 0),
    any_stem = as.integer(n_stem > 0),
    log1p_n_inventors = log1p(n_inventors),
    log1p_n_stem = log1p(n_stem),
    inventors_per_100k_pop = if_else(population > 0, 100000 * n_inventors / population, NA_real_),
    stem_per_100k_pop = if_else(population > 0, 100000 * n_stem / population, NA_real_)
  ) %>%
  left_join(treatment_assignment %>% select(unit_id, event_year, g_std, g_shift), by = "unit_id") %>%
  mutate(
    event_year = replace_na(event_year, 0L),
    g_std = replace_na(g_std, 0L),
    g_shift = replace_na(g_shift, 0L),
    unit_num = as.integer(factor(unit_id))
  )

baseline_population <- panel_decade %>%
  filter(decade == 1800) %>%
  transmute(
    unit_id,
    population_baseline_1800 = population,
    log_population_baseline_1800 = if_else(
      population_baseline_1800 > 0,
      log(population_baseline_1800),
      NA_real_
    )
  )

panel_decade <- panel_decade %>%
  left_join(baseline_population, by = "unit_id")

###############################################################################
# Validation summaries
###############################################################################

balance_europe <- check_balanced_panel(panel_decade, unit_id, decade)

sample_summary <- panel_decade %>%
  summarise(
    sample = "europe_lau",
    n_rows = n(),
    n_units = n_distinct(unit_id),
    n_periods = n_distinct(decade),
    min_decade = min(decade),
    max_decade = max(decade),
    is_balanced = balance_europe$is_balanced,
    min_periods_per_unit = balance_europe$min_periods_per_unit,
    max_periods_per_unit = balance_europe$max_periods_per_unit,
    missing_inventors_per_100k_pop = sum(is.na(inventors_per_100k_pop)),
    missing_stem_per_100k_pop = sum(is.na(stem_per_100k_pop)),
    missing_population_baseline_1800 = sum(is.na(population_baseline_1800)),
    missing_log_population_baseline_1800 = sum(is.na(log_population_baseline_1800)),
    total_inventors = sum(n_inventors, na.rm = TRUE),
    total_stem = sum(n_stem, na.rm = TRUE),
    n_treated_std = n_distinct(unit_id[g_std > 0]),
    n_treated_shift = n_distinct(unit_id[g_shift > 0])
  )

control_sets <- list(
  population_level = "population_baseline_1800",
  population_log = "log_population_baseline_1800"
)

missing_controls <- imap_dfr(control_sets, function(controls, control_spec) {
  panel_decade %>%
    distinct(unit_id, across(all_of(controls))) %>%
    summarise(missing_units = sum(if_any(all_of(controls), is.na)), .groups = "drop") %>%
    mutate(sample = "europe_lau", control_spec = control_spec) %>%
    mutate(control = paste(controls, collapse = " + "))
}) %>%
  select(sample, control_spec, control, missing_units)

event_distribution <- bind_rows(
  panel_decade %>%
    distinct(unit_id, g_std) %>%
    count(g_std, name = "n_units") %>%
    mutate(sample = "europe_lau", timing = "standard_decade", cohort = as.character(g_std)) %>%
    select(sample, timing, cohort, n_units),
  panel_decade %>%
    distinct(unit_id, g_shift) %>%
    count(g_shift, name = "n_units") %>%
    mutate(sample = "europe_lau", timing = "alternative_decade", cohort = as.character(g_shift)) %>%
    select(sample, timing, cohort, n_units)
)

write_csv(sample_summary, file.path(results_dir, "worlds_fairs_lau_baseline_pop_controls_sample_summary.csv"))
write_csv(missing_controls, file.path(results_dir, "worlds_fairs_lau_baseline_pop_controls_missing_controls.csv"))
write_csv(
  event_distribution,
  file.path(results_dir, "worlds_fairs_lau_baseline_pop_controls_event_distribution.csv")
)

###############################################################################
# Event studies
###############################################################################

outcomes <- c(
  "inventors_per_100k_pop",
  "stem_per_100k_pop",
  "n_inventors",
  "log1p_n_inventors",
  "n_stem",
  "log1p_n_stem"
)

specs <- list(
  list(
    sample = "europe_lau",
    timing = "standard_decade",
    gname = "g_std",
    data = panel_decade
  ),
  list(
    sample = "europe_lau",
    timing = "alternative_decade",
    gname = "g_shift",
    data = panel_decade
  )
)

model_results <- list()

for (control_spec in names(control_sets)) {
  for (spec in specs) {
    for (outcome in outcomes) {
      key <- paste(outcome, spec$sample, spec$timing, control_spec, sep = "__")
      message(
        "Running baseline-population controlled world's fairs event study: ",
        outcome,
        " | ",
        spec$sample,
        " | ",
        spec$timing,
        " | ",
        control_spec
      )

      model_results[[key]] <- run_event_study(
        data = spec$data,
        outcome = outcome,
        sample_name = spec$sample,
        timing = spec$timing,
        gname = spec$gname,
        controls = control_sets[[control_spec]],
        control_spec = control_spec,
        window = 70,
        cores = 4
      )
    }
  }
}

successful_models <- keep(model_results, "ok")

if (length(successful_models) == 0) {
  model_status_failed <- imap_dfr(
    model_results,
    ~ tibble(
      outcome = .x$outcome,
      sample = .x$sample,
      timing = .x$timing,
      control_spec = .x$control_spec,
      control_group = "notyettreated",
      ok = .x$ok,
      n_rows = value_or(.x$n_rows, NA_integer_),
      n_units = value_or(.x$n_units, NA_integer_),
      n_treated_units = value_or(.x$n_treated_units, NA_integer_),
      n_control_units = value_or(.x$n_control_units, NA_integer_),
      n_treated_cohorts = value_or(.x$n_treated_cohorts, NA_integer_),
      n_first_period_treated_units_dropped = value_or(
        .x$n_first_period_treated_units_dropped,
        NA_integer_
      ),
      min_decade = value_or(.x$min_decade, NA_real_),
      max_decade = value_or(.x$max_decade, NA_real_),
      formula_vars = paste(unlist(value_or(.x$formula_vars, list(character()))), collapse = " + "),
      error = value_or(.x$error, NA_character_)
    )
  )
  write_csv(
    model_status_failed,
    file.path(results_dir, "worlds_fairs_lau_baseline_pop_controls_model_status.csv")
  )
  stop("No world's-fairs event-study model ran successfully.")
}

dynamic_att <- imap_dfr(
  successful_models,
  ~ extract_dynamic_att(.x$es, .x$outcome, .x$sample, .x$timing, .x$control_spec)
)

simple_att <- imap_dfr(
  successful_models,
  ~ extract_simple_att(.x$simple, .x$outcome, .x$sample, .x$timing, .x$control_spec)
)

model_status <- imap_dfr(
  model_results,
  ~ tibble(
    outcome = .x$outcome,
    sample = .x$sample,
    timing = .x$timing,
    control_spec = .x$control_spec,
    control_group = "notyettreated",
    ok = .x$ok,
    n_rows = value_or(.x$n_rows, NA_integer_),
    n_units = value_or(.x$n_units, NA_integer_),
    n_treated_units = value_or(.x$n_treated_units, NA_integer_),
    n_control_units = value_or(.x$n_control_units, NA_integer_),
    n_treated_cohorts = value_or(.x$n_treated_cohorts, NA_integer_),
    n_first_period_treated_units_dropped = value_or(
      .x$n_first_period_treated_units_dropped,
      NA_integer_
    ),
    min_decade = value_or(.x$min_decade, NA_real_),
    max_decade = value_or(.x$max_decade, NA_real_),
    formula_vars = paste(unlist(value_or(.x$formula_vars, list(character()))), collapse = " + "),
    error = value_or(.x$error, NA_character_)
  )
)

write_csv(
  dynamic_att,
  file.path(results_dir, "worlds_fairs_lau_baseline_pop_controls_dynamic_att.csv")
)
write_csv(
  simple_att,
  file.path(results_dir, "worlds_fairs_lau_baseline_pop_controls_simple_att.csv")
)
write_csv(
  model_status,
  file.path(results_dir, "worlds_fairs_lau_baseline_pop_controls_model_status.csv")
)

y_limits_by_outcome <- dynamic_att %>%
  group_by(outcome) %>%
  summarise(y_limits = list(dynamic_y_limits(pick(everything()))), .groups = "drop")

for (model in successful_models) {
  y_limits <- y_limits_by_outcome %>%
    filter(outcome == model$outcome) %>%
    pull(y_limits) %>%
    pluck(1)

  plot_es <- plot_dynamic_event_study(
    model$es,
    model$outcome,
    model$sample,
    model$timing,
    model$control_spec,
    y_limits
  )

  ggsave(
    file.path(
      results_dir,
      paste0(
        "ES_worlds_fairs_lau_",
        sanitize_filename(model$outcome),
        "_",
        sanitize_filename(model$sample),
        "_",
        sanitize_filename(model$timing),
        "_",
        sanitize_filename(model$control_spec),
        ".png"
      )
    ),
    plot_es,
    width = 8,
    height = 6,
    dpi = 300
  )
}

hyphen_year_check <- fair_match_audit %>%
  filter(str_detect(Year, "[-\u2013\u2014]")) %>%
  select(fair_row_id, Year, City, Country, Fair_name, event_year, match_status)

write_csv(
  hyphen_year_check,
  file.path(results_dir, "worlds_fairs_lau_baseline_pop_controls_multi_year_fair_year_check.csv")
)

notes <- c(
  "World's fairs LAU-level event studies with baseline population controls",
  "",
  paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("DATA_INPUT: ", DATA_INPUT),
  paste0("DATA_OUTPUT: ", DATA_OUTPUT),
  paste0("Panel: ", panel_file),
  paste0("Fairs: ", fairs_file),
  paste0("GISCO LAU boundaries: ", lau_boundaries_file),
  "Outcome panel window: 1700-1960.",
  "Treatment window: fair event_year between 1800 and 1900, inclusive.",
  "Multi-year entries use the first listed year via year_start / first 4-digit year.",
  "US events are excluded because the outcome panel is Europe-only.",
  "European events are assigned to GISCO LAU polygons by point-in-polygon.",
  "Units treated in the first outcome period are recorded but dropped from did estimation because they have no pre-period.",
  "Timing variables: g_std = floor(event_year / 10) * 10; g_shift shifts years ending in 7-9 to next decade.",
  "Baseline control definition: population of the unit in 1800.",
  "Control specifications: population_level uses population_baseline_1800; population_log uses log_population_baseline_1800.",
  "Estimator: did::att_gt with xformla controls, control_group = notyettreated, est_method = dr, base_period = universal.",
  paste0("Outcomes: ", paste(outcomes, collapse = ", ")),
  paste0("Successful models: ", sum(model_status$ok), " / ", nrow(model_status)),
  paste0(
    "Treatment fair rows matched to panel: ",
    sum(fair_match_audit$match_status == "matched_to_panel" & fair_match_audit$in_treatment_window)
  ),
  paste0(
    "Treated units in treatment assignment: ",
    n_distinct(treatment_assignment$unit_id)
  ),
  paste0(
    "Elapsed minutes: ",
    round(difftime(Sys.time(), initial_time, units = "mins"), 1)
  )
)

writeLines(notes, file.path(results_dir, "notes.txt"))

message("Saved world's fairs LAU event-study outputs in: ", results_dir)
message(
  "Done. Elapsed: ",
  round(difftime(Sys.time(), initial_time, units = "mins"), 1),
  " min"
)
