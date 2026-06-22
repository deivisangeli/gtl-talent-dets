###############################################################################
# Project: GTL Talent Determinants
# Goal: Controlled AMWS event studies from 1900 using Andrews college site-selection pairs
###############################################################################

rm(list = ls())

library("tidyverse")
library("ggplot2")
library("did")
library("sf")
library("tigris")
library("readxl")

initial_time <- Sys.time()

###############################################################################
# Paths
###############################################################################

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]

if (length(file_arg) > 0) {
 script_path <- normalizePath(
  sub("^--file=", "", file_arg[[1]]),
  winslash = "/",
  mustWork = TRUE
 )
 repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
 cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
 repo_root <- if (basename(cwd) == "analysis") dirname(cwd) else if (basename(dirname(cwd)) == "analysis") dirname(dirname(cwd)) else cwd
}

source(file.path(repo_root, "prep", "raw_paths.R"))

options(tigris_use_cache = TRUE, tigris_cache_dir = tigris_cache_dir())

results_subdir <- "amws_county_pairs_all_colleges_controls_from_1900"

results_subdir_path <- function(...) {
 out_dir <- results_file_path("land_grants", results_subdir)
 dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
 file.path(out_dir, ...)
}

###############################################################################
# Helpers
###############################################################################

as_geoid <- function(x) {
 str_pad(as.character(as.integer(x)), width = 5, side = "left", pad = "0")
}

mean_or_na <- function(x) {
 if (all(is.na(x))) {
  return(NA_real_)
 }

 mean(x, na.rm = TRUE)
}

normalize_county <- function(x) {
 x %>%
  iconv(to = "ASCII//TRANSLIT") %>%
  tolower() %>%
  str_replace_all("&", "and") %>%
  str_replace_all("\\bexperiment station\\b", " ") %>%
  str_replace_all("\\bexperiment\\b", " ") %>%
  str_replace_all(
   "\\bcounty\\b|\\bparish\\b|\\bborough\\b|\\bcensus area\\b|\\bmunicipality\\b|\\bcity and borough\\b|\\bcity\\b",
   " "
  ) %>%
  str_replace_all("[^a-z0-9]+", " ") %>%
  str_squish()
}

sanitize_filename <- function(x) {
 str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

extract_dynamic_att <- function(es, outcome, spec_name, timing_name) {
 tibble(
  outcome = outcome,
  spec = spec_name,
  timing = timing_name,
  event_time = es$egt,
  estimate = es$att.egt,
  se = es$se.egt,
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

add_sample_annotation <- function(plot, n_events, n_treated_units,
                                  n_control_units) {
 plot +
  annotate(
   "label",
   x = Inf,
   y = Inf,
   hjust = 1.05,
   vjust = 1.15,
   label = paste0(
    "Events: ", n_events,
    "\nTreated: ", n_treated_units,
    "\nControls: ", n_control_units
   ),
   size = 3.1,
   fill = "white",
   alpha = 0.9
  )
}

plot_dynamic_event_study <- function(es, outcome, spec_name, timing_name,
                                     y_limits, n_events, n_treated_units,
                                     n_control_units) {
 plot <- did::ggdid(es) +
  labs(
   x = "Relative Time",
   y = "Effect",
   title = str_wrap(
    paste(
     "AMWS event study - Andrews county pairs - all college types -",
     outcome,
     str_to_title(spec_name), "controls",
     timing_name
    ),
    width = 72
   )
  ) +
  coord_cartesian(ylim = y_limits)

 add_sample_annotation(plot, n_events, n_treated_units, n_control_units)
}

plot_group_att <- function(out, outcome, spec_name, timing_name,
                           n_events, n_treated_units, n_control_units) {
 plot <- did::ggdid(out) +
  labs(
   title = str_wrap(
    paste(
     "Group-specific ATT - AMWS - Andrews county pairs - all college types -",
     outcome,
     str_to_title(spec_name), "controls",
     timing_name
    ),
    width = 72
   )
  )

 add_sample_annotation(plot, n_events, n_treated_units, n_control_units)
}

complete_control_units <- function(data, controls) {
 if (length(controls) == 0) {
  return(unique(data$stack_unit_num))
 }

 data %>%
  distinct(stack_unit_num, across(all_of(controls))) %>%
  filter(if_all(all_of(controls), ~ !is.na(.x))) %>%
  pull(stack_unit_num)
}

prepare_controls_for_spec <- function(data, controls) {
 analysis_units <- complete_control_units(data, controls)

 data <- data %>%
  filter(stack_unit_num %in% analysis_units)

 retained_events <- data %>%
  group_by(event_id) %>%
  summarise(
   has_treated = any(g > 0),
   has_control = any(g == 0),
   .groups = "drop"
  ) %>%
  filter(has_treated, has_control) %>%
  select(event_id)

 data <- data %>%
  semi_join(retained_events, by = "event_id")

 formula_vars <- controls[
  map_lgl(data[controls], ~ n_distinct(.x, na.rm = TRUE) > 1)
 ]

 list(data = data, formula_vars = formula_vars)
}

run_event_study <- function(data, outcome, controls, spec_name, timing_name,
                            window = 70) {
 data_es <- data %>%
  select(stack_unit_num, GEOID, decade, g, sample_role, event_id,
         all_of(outcome), all_of(controls)) %>%
  rename(y = all_of(outcome)) %>%
  filter(!is.na(y))

 control_prep <- prepare_controls_for_spec(data_es, controls)
 data_es <- control_prep$data

 if (nrow(data_es) == 0) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   spec = spec_name,
   timing = timing_name,
   n_rows = 0L,
   n_units = 0L,
   n_events = 0L,
   n_estimable_events_did = NA_integer_,
   n_already_treated_events = NA_integer_,
   n_treated_units = 0L,
   n_control_units = 0L,
   min_decade = NA_real_,
   max_decade = NA_real_,
   formula_vars = list(control_prep$formula_vars),
   error = "No rows after outcome and control missingness filters."
  ))
 }

 if (n_distinct(data_es$g[data_es$g > 0]) == 0) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   spec = spec_name,
   timing = timing_name,
   n_rows = nrow(data_es),
   n_units = n_distinct(data_es$stack_unit_num),
   n_events = n_distinct(data_es$event_id),
   n_estimable_events_did = NA_integer_,
   n_already_treated_events = NA_integer_,
   n_treated_units = n_distinct(data_es$stack_unit_num[data_es$g > 0]),
   n_control_units = n_distinct(data_es$stack_unit_num[data_es$g == 0]),
   min_decade = min(data_es$decade, na.rm = TRUE),
   max_decade = max(data_es$decade, na.rm = TRUE),
   formula_vars = list(control_prep$formula_vars),
   error = "No treated cohorts in estimation sample."
  ))
 }

 if (n_distinct(data_es$y, na.rm = TRUE) < 2) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   spec = spec_name,
   timing = timing_name,
   n_rows = nrow(data_es),
   n_units = n_distinct(data_es$stack_unit_num),
   n_events = n_distinct(data_es$event_id),
   n_estimable_events_did = NA_integer_,
   n_already_treated_events = NA_integer_,
   n_treated_units = n_distinct(data_es$stack_unit_num[data_es$g > 0]),
   n_control_units = n_distinct(data_es$stack_unit_num[data_es$g == 0]),
   min_decade = min(data_es$decade, na.rm = TRUE),
   max_decade = max(data_es$decade, na.rm = TRUE),
   formula_vars = list(control_prep$formula_vars),
   error = "Outcome has insufficient variation."
  ))
 }

 xformla <- if (length(control_prep$formula_vars) > 0) {
  as.formula(paste("~", paste(control_prep$formula_vars, collapse = " + ")))
 } else {
  ~ 1
 }

 tryCatch(
  {
   out <- did::att_gt(
    yname = "y",
    tname = "decade",
    idname = "stack_unit_num",
    gname = "g",
    xformla = xformla,
    data = data_es,
    control_group = "nevertreated",
    est_method = "reg",
    base_period = "universal",
    allow_unbalanced_panel = TRUE,
    cores = 4
   )

   es <- did::aggte(
    out,
    type = "dynamic",
    na.rm = TRUE,
    min_e = -window,
    max_e = window
   )

   list(
    ok = TRUE,
    outcome = outcome,
    spec = spec_name,
    timing = timing_name,
    out = out,
    es = es,
    n_rows = nrow(data_es),
    n_units = n_distinct(data_es$stack_unit_num),
    n_events = n_distinct(data_es$event_id),
    n_estimable_events_did = n_distinct(
     data_es$event_id[data_es$g > min(data_es$decade, na.rm = TRUE)]
    ),
    n_already_treated_events = n_distinct(
     data_es$event_id[
      data_es$g > 0 & data_es$g <= min(data_es$decade, na.rm = TRUE)
     ]
    ),
    n_treated_units = n_distinct(data_es$stack_unit_num[data_es$g > 0]),
    n_control_units = n_distinct(data_es$stack_unit_num[data_es$g == 0]),
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
    spec = spec_name,
    timing = timing_name,
    n_rows = NA_integer_,
    n_units = NA_integer_,
    n_events = NA_integer_,
    n_estimable_events_did = NA_integer_,
    n_already_treated_events = NA_integer_,
    n_treated_units = NA_integer_,
    n_control_units = NA_integer_,
    min_decade = NA_real_,
    max_decade = NA_real_,
    formula_vars = list(control_prep$formula_vars),
    error = conditionMessage(e)
   )
  }
 )
}

summarise_missing_controls <- function(data, controls, spec_name, timing_name) {
 data %>%
  distinct(stack_unit_num, across(all_of(controls))) %>%
  summarise(across(all_of(controls), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "control", values_to = "missing_units") %>%
  mutate(
   spec = spec_name,
   timing = timing_name,
   total_units = n_distinct(data$stack_unit_num),
   missing_pct = 100 * missing_units / total_units,
   .before = 1
  )
}

nearest_control <- function(units, source_data, value_var, source_year_var,
                            rule = c("prior", "production", "prior_pre1870")) {
 rule <- match.arg(rule)

 candidates <- units %>%
  select(stack_unit_id, GEOID, experiment_year, event_treatment_decade) %>%
  left_join(source_data, by = "GEOID") %>%
  filter(!is.na(.data[[value_var]])) %>%
  filter(
   case_when(
    rule == "production" ~
     source_year < event_treatment_decade |
      (experiment_year == 1839 & source_year == 1840),
    rule == "prior_pre1870" ~
     source_year < event_treatment_decade & source_year < 1870,
    rule == "prior" ~ source_year < event_treatment_decade,
    TRUE ~ FALSE
   )
  )

 candidates %>%
  group_by(stack_unit_id) %>%
  slice_max(source_year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
   stack_unit_id,
   !!value_var := .data[[value_var]],
   !!source_year_var := source_year
  )
}

###############################################################################
# Load AMWS panel and controls
###############################################################################

panel_year <- read_csv(
 output_file_path("us_panel_county_amws_combined_year.csv"),
 show_col_types = FALSE
) %>%
 mutate(
  GEOID = as_geoid(GEOID),
  year = as.integer(year),
  decade = floor(year / 10) * 10
 ) %>%
 filter(year >= 1900, decade >= 1900)

panel <- panel_year %>%
 group_by(GEOID, decade) %>%
 summarise(
  n_amws = sum(replace_na(n_amws, 0), na.rm = TRUE),
  population = mean_or_na(population),
  county_births_estimate = sum(
   replace_na(county_births_estimate_year, 0),
   na.rm = TRUE
  ),
  .groups = "drop"
 ) %>%
 mutate(
  amws_per_1000_births = if_else(
   county_births_estimate > 0,
   1000 * n_amws / county_births_estimate,
   NA_real_
  ),
  amws_per_100k = if_else(
   population > 0,
   100000 * n_amws / population,
   NA_real_
  ),
  log1p_n_amws = log1p(n_amws)
 )

county_covariates_raw <- read_csv(
 output_file_path("county_tpe_covariates_clean.csv"),
 show_col_types = FALSE
) %>%
 mutate(GEOID = as_geoid(GEOID))

covariate_controls <- county_covariates_raw %>%
 transmute(
  GEOID,
  source_year = year,
  frontier100kmL6,
  cropland_km2,
  grazeland_km2,
  canal_access,
  sex_ratio,
  post_offices,
  manufacturing_output_real_1900_million =
   manufacturing_output_value_real_1900 / 1e6,
  farming_output_real_1900_million =
   farming_output_value_real_1900 / 1e6,
  immigrant_share,
  hyde_population_thousand = hyde_population / 1e3
 )

county_demographics_raw <- read_csv(
 output_file_path("county_nhgis_demographics_panel.csv"),
 show_col_types = FALSE
) %>%
 mutate(GEOID = as_geoid(GEOID))

demographic_controls <- county_demographics_raw %>%
 transmute(
  GEOID,
  source_year = year,
  slave_share,
  illiterate_share_total_population
 )

pairs_path <- raw_file_path("andrews_2023_county_pairs_long.xlsx")

pairs_long <- readxl::read_excel(pairs_path, sheet = "county_pairs_long") %>%
 mutate(across(c(college, college_type, selected_county, selected_state,
                 runner_up_county, runner_up_state_assumed,
                 runner_up_match_status), as.character))

###############################################################################
# County lookup
###############################################################################

counties_lookup <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 st_drop_geometry() %>%
 select(GEOID, NAME, STATEFP) %>%
 filter(as.integer(STATEFP) <= 56)

states_lookup <- tigris::states(cb = TRUE, year = 2020) %>%
 st_drop_geometry() %>%
 select(STATEFP, STUSPS)

lookup <- counties_lookup %>%
 left_join(states_lookup, by = "STATEFP") %>%
 transmute(
  GEOID = as_geoid(GEOID),
  county_norm = normalize_county(NAME),
  state_abbr = STUSPS
 )

state_lookup <- tibble(
 state = state.name,
 state_abbr = state.abb
)

###############################################################################
# Build stacked experiment-county units
###############################################################################

event_lookup <- pairs_long %>%
 distinct(college, experiment_year, college_type, selected_county, selected_state) %>%
 arrange(experiment_year, college, selected_state, selected_county) %>%
 mutate(
  event_id = row_number(),
  g_std = floor(experiment_year / 10) * 10,
  g_shift = if_else(
   experiment_year %% 10 >= 7,
   floor(experiment_year / 10) * 10 + 10,
   floor(experiment_year / 10) * 10
  )
 )

treated_units <- event_lookup %>%
 left_join(state_lookup, by = c("selected_state" = "state")) %>%
 mutate(county_norm = normalize_county(selected_county)) %>%
 left_join(lookup, by = c("county_norm", "state_abbr")) %>%
 transmute(
  event_id,
  college,
  experiment_year,
  college_type,
  GEOID,
  county = selected_county,
  state = selected_state,
  sample_role = "treated",
  g_std,
  g_shift
 )

runner_unresolved_rows <- pairs_long %>%
 filter(runner_up_match_status != "matched_same_state") %>%
 distinct(college, experiment_year, runner_up_county, runner_up_state_assumed,
          runner_up_match_status)

runner_units <- pairs_long %>%
 filter(runner_up_match_status == "matched_same_state") %>%
 left_join(
  event_lookup,
  by = c("college", "experiment_year", "college_type",
         "selected_county", "selected_state")
 ) %>%
 distinct(event_id, college, experiment_year, college_type,
          runner_up_county, runner_up_state_assumed, g_std, g_shift) %>%
 left_join(state_lookup, by = c("runner_up_state_assumed" = "state")) %>%
 mutate(county_norm = normalize_county(runner_up_county)) %>%
 left_join(lookup, by = c("county_norm", "state_abbr")) %>%
 transmute(
  event_id,
  college,
  experiment_year,
  college_type,
  GEOID,
  county = runner_up_county,
  state = runner_up_state_assumed,
  sample_role = "runner_up",
  g_std = 0,
  g_shift = 0
 )

if (any(is.na(treated_units$GEOID))) {
 unmatched <- treated_units %>%
  filter(is.na(GEOID)) %>%
  distinct(college, experiment_year, county, state)
 stop(
  "Some treated counties could not be matched to GEOID: ",
  paste(
   paste(unmatched$college, unmatched$county, unmatched$state, sep = " | "),
   collapse = "; "
  )
 )
}

if (any(is.na(runner_units$GEOID))) {
 unmatched <- runner_units %>%
  filter(is.na(GEOID)) %>%
  distinct(college, experiment_year, county, state)
 stop(
  "Some runner-up counties could not be matched to GEOID: ",
  paste(
   paste(unmatched$college, unmatched$county, unmatched$state, sep = " | "),
   collapse = "; "
  )
 )
}

first_panel_decade <- min(panel$decade, na.rm = TRUE)
last_panel_decade <- max(panel$decade, na.rm = TRUE)

control_source_specs <- tribble(
 ~control, ~source_year_var, ~source_table, ~rule,
 "frontier100kmL6", "frontier100kmL6_control_year", "covariate", "prior",
 "cropland_km2", "cropland_km2_control_year", "covariate", "prior",
 "grazeland_km2", "grazeland_km2_control_year", "covariate", "prior",
 "canal_access", "canal_access_control_year", "covariate", "prior",
 "sex_ratio", "sex_ratio_control_year", "covariate", "prior",
 "post_offices", "post_offices_control_year", "covariate", "prior",
 "manufacturing_output_real_1900_million",
 "manufacturing_output_control_year", "covariate", "production",
 "farming_output_real_1900_million",
 "farming_output_control_year", "covariate", "production",
 "immigrant_share", "immigrant_share_control_year", "covariate", "prior",
 "hyde_population_thousand", "hyde_population_control_year", "covariate",
 "prior",
 "slave_share", "slave_share_control_year", "demographic", "prior_pre1870",
 "illiterate_share_total_population",
 "illiterate_share_control_year", "demographic", "prior"
)

build_nearest_controls <- function(stack_units) {
 control_tables <- list(
  covariate = covariate_controls,
  demographic = demographic_controls
 )

 nearest_list <- pmap(
  control_source_specs,
  function(control, source_year_var, source_table, rule) {
   nearest_control(
    units = stack_units,
    source_data = control_tables[[source_table]] %>%
     select(GEOID, source_year, all_of(control)),
    value_var = control,
    source_year_var = source_year_var,
    rule = rule
   )
  }
 )

 reduce(nearest_list, full_join, by = "stack_unit_id")
}

build_stacked_panel <- function(timing_var, timing_name) {
 event_sample <- event_lookup %>%
  mutate(event_treatment_decade = .data[[timing_var]]) %>%
  filter(
   event_treatment_decade >= 1890,
   event_treatment_decade <= last_panel_decade
  ) %>%
  select(event_id, event_treatment_decade)

 treated_sample <- treated_units %>%
  semi_join(event_sample, by = "event_id") %>%
  left_join(event_sample, by = "event_id") %>%
  mutate(g = event_treatment_decade)

 runner_sample <- runner_units %>%
  semi_join(event_sample, by = "event_id") %>%
  left_join(event_sample, by = "event_id") %>%
  mutate(g = 0)

 stack_units <- bind_rows(treated_sample, runner_sample) %>%
  arrange(event_id, desc(sample_role == "treated"), GEOID) %>%
  distinct(event_id, GEOID, sample_role, .keep_all = TRUE) %>%
  mutate(
   timing = timing_name,
   control_year = event_treatment_decade - 10,
   stack_unit_id = paste(timing_name, event_id, GEOID, sample_role, sep = "_"),
   stack_unit_num = dense_rank(stack_unit_id)
  )

 nearest_controls <- build_nearest_controls(stack_units)

 stack_units %>%
  left_join(nearest_controls, by = "stack_unit_id") %>%
  left_join(panel, by = "GEOID") %>%
  arrange(event_id, sample_role, GEOID, decade)
}

panel_std <- build_stacked_panel("g_std", "standard_decade")
panel_shift <- build_stacked_panel("g_shift", "alternative_decade")

nearest_control_source_audit <- bind_rows(panel_std, panel_shift) %>%
 distinct(
  timing,
  event_id,
  stack_unit_id,
  stack_unit_num,
  sample_role,
  GEOID,
  college,
  experiment_year,
  event_treatment_decade,
  control_year,
  frontier100kmL6,
  frontier100kmL6_control_year,
  cropland_km2,
  cropland_km2_control_year,
  grazeland_km2,
  grazeland_km2_control_year,
  canal_access,
  canal_access_control_year,
  sex_ratio,
  sex_ratio_control_year,
  post_offices,
  post_offices_control_year,
  manufacturing_output_real_1900_million,
  manufacturing_output_control_year,
  farming_output_real_1900_million,
  farming_output_control_year,
  immigrant_share,
  immigrant_share_control_year,
  hyde_population_thousand,
  hyde_population_control_year,
  slave_share,
  slave_share_control_year,
  illiterate_share_total_population,
  illiterate_share_control_year
 ) %>%
 mutate(
  frontier100kmL6_year_distance_to_treatment =
   event_treatment_decade - frontier100kmL6_control_year,
  cropland_km2_year_distance_to_treatment =
   event_treatment_decade - cropland_km2_control_year,
  grazeland_km2_year_distance_to_treatment =
   event_treatment_decade - grazeland_km2_control_year,
  canal_access_year_distance_to_treatment =
   event_treatment_decade - canal_access_control_year,
  sex_ratio_year_distance_to_treatment =
   event_treatment_decade - sex_ratio_control_year,
  post_offices_year_distance_to_treatment =
   event_treatment_decade - post_offices_control_year,
  manufacturing_output_year_distance_to_treatment =
   event_treatment_decade - manufacturing_output_control_year,
  farming_output_year_distance_to_treatment =
   event_treatment_decade - farming_output_control_year,
  immigrant_share_year_distance_to_treatment =
   event_treatment_decade - immigrant_share_control_year,
  hyde_population_year_distance_to_treatment =
   event_treatment_decade - hyde_population_control_year,
  slave_share_year_distance_to_treatment =
   event_treatment_decade - slave_share_control_year,
  illiterate_share_year_distance_to_treatment =
   event_treatment_decade - illiterate_share_control_year
 )

nearest_control_source_distribution <- nearest_control_source_audit %>%
 select(timing, ends_with("_control_year")) %>%
 pivot_longer(
  cols = ends_with("_control_year"),
  names_to = "control",
  values_to = "source_year"
 ) %>%
 mutate(control = str_remove(control, "_control_year$")) %>%
 group_by(timing, control, source_year) %>%
 summarise(n_units = n(), .groups = "drop") %>%
 arrange(timing, control, source_year)

###############################################################################
# Control specifications and event studies
###############################################################################

control_sets <- list(
 baseline = c(
  "frontier100kmL6",
  "cropland_km2",
  "grazeland_km2",
  "canal_access"
 ),
 extended = c(
  "frontier100kmL6",
  "cropland_km2",
  "grazeland_km2",
  "canal_access",
  "sex_ratio",
  "post_offices",
  "manufacturing_output_real_1900_million",
  "farming_output_real_1900_million",
  "immigrant_share",
  "slave_share"
 ),
 full = c(
  "frontier100kmL6",
  "cropland_km2",
  "grazeland_km2",
  "canal_access",
  "sex_ratio",
  "post_offices",
  "manufacturing_output_real_1900_million",
  "farming_output_real_1900_million",
  "immigrant_share",
  "slave_share",
  "illiterate_share_total_population",
  "hyde_population_thousand"
 )
)

outcomes <- c(
 "amws_per_1000_births",
 "n_amws",
 "log1p_n_amws",
 "amws_per_100k"
)

missing_controls <- imap_dfr(control_sets, function(controls, spec_name) {
 bind_rows(
  summarise_missing_controls(panel_std, controls, spec_name, "standard_decade"),
  summarise_missing_controls(panel_shift, controls, spec_name,
                             "alternative_decade")
 )
})

window_years <- 70
model_results <- list()

for (spec_name in names(control_sets)) {
 for (timing_name in c("standard_decade", "alternative_decade")) {
  panel_timing <- if (timing_name == "standard_decade") {
   panel_std
  } else {
   panel_shift
  }

  for (outcome in outcomes) {
   message(
    "Running controlled AMWS event study: ",
    outcome, " | ", spec_name, " | ", timing_name
   )

   model_results[[paste(outcome, spec_name, timing_name, sep = "__")]] <-
    run_event_study(
     data = panel_timing,
     outcome = outcome,
     controls = control_sets[[spec_name]],
     spec_name = spec_name,
     timing_name = timing_name,
     window = window_years
    )
  }
 }
}

successful_models <- keep(model_results, "ok")

if (length(successful_models) == 0) {
 stop("No controlled AMWS event-study model ran successfully.")
}

dynamic_att <- imap_dfr(
 successful_models,
 ~ extract_dynamic_att(.x$es, .x$outcome, .x$spec, .x$timing)
)

model_status <- imap_dfr(
 model_results,
 ~ tibble(
  model = .y,
  outcome = .x$outcome,
  spec = .x$spec,
  timing = .x$timing,
  ok = .x$ok,
  n_rows = ifelse(isTRUE(.x$ok), .x$n_rows, NA_integer_),
  n_units = ifelse(isTRUE(.x$ok), .x$n_units, NA_integer_),
  n_events = ifelse(isTRUE(.x$ok), .x$n_events, NA_integer_),
  n_estimable_events_did =
   ifelse(isTRUE(.x$ok), .x$n_estimable_events_did, NA_integer_),
  n_already_treated_events =
   ifelse(isTRUE(.x$ok), .x$n_already_treated_events, NA_integer_),
  n_treated_units = ifelse(isTRUE(.x$ok), .x$n_treated_units, NA_integer_),
  n_control_units = ifelse(isTRUE(.x$ok), .x$n_control_units, NA_integer_),
  min_decade = ifelse(isTRUE(.x$ok), .x$min_decade, NA_real_),
  max_decade = ifelse(isTRUE(.x$ok), .x$max_decade, NA_real_),
  formula_vars = paste(.x$formula_vars[[1]], collapse = ", "),
  error = .x$error
 )
)

###############################################################################
# Export outputs
###############################################################################

for (model in successful_models) {
 model_att <- dynamic_att %>%
  filter(outcome == model$outcome, spec == model$spec)

 y_limits <- dynamic_y_limits(model_att)

 plot_es <- plot_dynamic_event_study(
  model$es,
  model$outcome,
  model$spec,
  model$timing,
  y_limits,
  model$n_events,
  model$n_treated_units,
  model$n_control_units
 )

 plot_group <- plot_group_att(
  model$out,
  model$outcome,
  model$spec,
  model$timing,
  model$n_events,
  model$n_treated_units,
  model$n_control_units
 )

 safe_outcome <- sanitize_filename(model$outcome)
 safe_spec <- sanitize_filename(model$spec)
 safe_timing <- sanitize_filename(model$timing)

 ggsave(
  filename = results_subdir_path(
   paste0("ES_amws_county_pairs_all_colleges_controls_", safe_spec, "_",
          safe_outcome, "_", safe_timing, ".png")
  ),
  plot = plot_es,
  width = 8,
  height = 6,
  dpi = 300
 )

 ggsave(
  filename = results_subdir_path(
   paste0("ggdid_amws_county_pairs_all_colleges_controls_", safe_spec, "_",
          safe_outcome, "_", safe_timing, ".png")
  ),
  plot = plot_group,
  width = 8,
  height = 6,
  dpi = 300
 )
}

write_csv(
 dynamic_att,
 results_subdir_path("amws_county_pairs_all_colleges_controls_dynamic_att.csv"),
 na = ""
)

write_csv(
 model_status,
 results_subdir_path("amws_county_pairs_all_colleges_controls_model_status.csv"),
 na = ""
)

sample_summary <- model_status %>%
 filter(ok) %>%
 select(outcome, spec, timing, n_events, n_estimable_events_did,
        n_already_treated_events, n_units, n_rows, n_treated_units,
        n_control_units, min_decade, max_decade)

write_csv(
 sample_summary,
 results_subdir_path("amws_county_pairs_all_colleges_controls_sample_summary.csv"),
 na = ""
)

write_csv(
 missing_controls,
 results_subdir_path("amws_county_pairs_all_colleges_controls_missing_controls.csv"),
 na = ""
)

write_csv(
 model_status %>%
  select(outcome, spec, timing, formula_vars),
 results_subdir_path("amws_county_pairs_all_colleges_controls_formula_vars.csv"),
 na = ""
)

write_csv(
 nearest_control_source_audit,
 results_subdir_path("amws_county_pairs_all_colleges_controls_source_years.csv"),
 na = ""
)

write_csv(
 nearest_control_source_distribution,
 results_subdir_path(
  "amws_county_pairs_all_colleges_controls_source_year_distribution.csv"
 ),
 na = ""
)

event_distribution <- bind_rows(
 event_lookup %>%
  count(college_type, name = "n_events") %>%
  mutate(distribution = "college_type", value = college_type) %>%
  select(distribution, value, n_events),
 event_lookup %>%
  count(g_std, name = "n_events") %>%
  mutate(distribution = "standard_decade", value = as.character(g_std)) %>%
  select(distribution, value, n_events),
 event_lookup %>%
  count(g_shift, name = "n_events") %>%
  mutate(distribution = "alternative_decade", value = as.character(g_shift)) %>%
  select(distribution, value, n_events)
)

event_details <- event_lookup %>%
 left_join(
  treated_units %>% select(event_id, treated_GEOID = GEOID),
  by = "event_id"
 ) %>%
 mutate(
  retained_standard_from_1900 =
   g_std >= 1890 & g_std <= last_panel_decade,
  retained_alternative_from_1900 =
   g_shift >= 1890 & g_shift <= last_panel_decade,
  already_treated_standard_from_1900 =
   retained_standard_from_1900 & g_std <= first_panel_decade,
  already_treated_alternative_from_1900 =
   retained_alternative_from_1900 & g_shift <= first_panel_decade,
  estimable_standard_decade =
   g_std > first_panel_decade & g_std <= last_panel_decade,
  estimable_alternative_decade =
   g_shift > first_panel_decade & g_shift <= last_panel_decade
 )

write_csv(
 event_distribution,
 results_subdir_path(
  "amws_county_pairs_all_colleges_controls_event_distribution.csv"
 ),
 na = ""
)

write_csv(
 event_details,
 results_subdir_path("amws_county_pairs_all_colleges_controls_events.csv"),
 na = ""
)

notes_lines <- c(
 "Controlled AMWS event study from 1900 using Andrews high-quality college site-selection experiments",
 paste0("Source workbook: ", pairs_path),
 paste0("Generated on: ", Sys.Date()),
 "",
 paste0("AMWS panel file: ",
        output_file_path("us_panel_county_amws_combined_year.csv")),
 paste0("AMWS panel decades used: ", first_panel_decade, "-",
        last_panel_decade),
 "Event retention rule: keep Andrews events with treatment decade g >= 1890.",
 "DID estimability: did::att_gt drops cohorts already treated in the first observed panel decade.",
 "With this 1900+ panel, g = 1890 and g = 1900 events are retained for audit but are not DID-estimable cohorts.",
 "Estimation method: did::att_gt with est_method = 'reg'.",
 paste0("Dynamic window: +/-", window_years, " years"),
 paste0("Original Andrews events: ", nrow(event_lookup)),
 paste0("Unresolved runner-up rows excluded: ", nrow(runner_unresolved_rows)),
 "Control timing: nearest non-missing pre-treatment decade for every covariate.",
 "Production controls use the nearest non-missing pre-treatment decade, with 1840 allowed for the 1839 event.",
 "Slave share uses the nearest non-missing pre-treatment value before 1870; 1870+ slave-share zeros are not used.",
 "Each control specification uses complete cases for that specification after applying the nearest-control rules.",
 "",
 "Outcomes:",
 paste0("- ", outcomes),
 "",
 "Control specifications:",
 imap_chr(
  control_sets,
  ~ paste0("- ", .y, ": ", paste(.x, collapse = ", "))
 ),
 "",
 "Sample summary:",
 capture.output(print(sample_summary, n = Inf)),
 "",
 "Model status:",
 capture.output(print(model_status, n = Inf)),
 "",
 "Events not estimable under standard decade:",
 event_details %>%
  filter(!estimable_standard_decade) %>%
  transmute(line = paste0(
   "- ", college, " | ", experiment_year, " | g_std=", g_std,
   " | ", selected_county, ", ", selected_state
  )) %>%
  pull(line),
 "",
 "Events not estimable under alternative decade:",
 event_details %>%
  filter(!estimable_alternative_decade) %>%
  transmute(line = paste0(
   "- ", college, " | ", experiment_year, " | g_shift=", g_shift,
   " | ", selected_county, ", ", selected_state
  )) %>%
  pull(line)
)

writeLines(
 notes_lines,
 con = results_subdir_path("amws_county_pairs_all_colleges_controls_notes.txt")
)

message("Saved controlled AMWS county-pairs event-study outputs in: ",
        results_subdir_path("."))
message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
