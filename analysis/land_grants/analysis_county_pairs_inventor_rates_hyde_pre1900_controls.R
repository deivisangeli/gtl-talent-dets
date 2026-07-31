###############################################################################
# Project: GTL Talent Determinants
# Goal: Controlled university event studies using HYDE inventor rates
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

results_subdir <- "county_pairs_hyde_pre1900_controls"

results_subdir_path <- function(...) {
 results_file_path("land_grants", results_subdir, ...)
}

###############################################################################
# Helpers
###############################################################################

as_geoid <- function(x) {
 str_pad(as.character(as.integer(x)), width = 5, side = "left", pad = "0")
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

collapse_unique <- function(x) {
 paste(sort(unique(na.omit(x))), collapse = "; ")
}

extract_dynamic_att <- function(es, spec_name, timing_name) {
 tibble(
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

complete_control_units <- function(data, controls) {
 data %>%
  distinct(stack_unit_id, across(all_of(controls))) %>%
  filter(if_all(all_of(controls), ~ !is.na(.x))) %>%
  pull(stack_unit_id)
}

prepare_controls_for_spec <- function(data, controls) {
 analysis_units <- complete_control_units(data, controls)
 data <- data %>%
  filter(stack_unit_id %in% analysis_units)

 retained_treated_events <- data %>%
  filter(sample_role == "treated") %>%
  distinct(event_id)

 data <- data %>%
  semi_join(retained_treated_events, by = "event_id")

 formula_vars <- controls[
  map_lgl(data[controls], ~ n_distinct(.x, na.rm = TRUE) > 1)
 ]

 list(
  data = data,
  formula_vars = formula_vars,
  missing_indicator_vars = character()
 )
}

add_sample_annotation <- function(plot, n_events, n_treated_units,
                                  n_control_units) {
 if (is.null(n_events) || is.null(n_treated_units) ||
     is.null(n_control_units)) {
  return(plot)
 }

 plot +
  annotate(
   "label",
   x = Inf,
   y = Inf,
   hjust = 1.05,
   vjust = 1.15,
   label = paste0(
    "Events: ", n_events,
    "\n",
    "Treated: ", n_treated_units,
    "\nControls: ", n_control_units
   ),
   size = 3.2,
   fill = "white",
   alpha = 0.9
  )
}

plot_dynamic_event_study <- function(es, title_add, y_limits,
                                     n_events = NULL,
                                     n_treated_units = NULL,
                                     n_control_units = NULL) {
 plot <- did::ggdid(es) +
  labs(
   x = "Relative Time",
   y = "Effect",
   title = str_wrap(
    paste0("Average Effect by Length of Exposure - ", title_add),
    width = 62
   )
  ) +
  coord_cartesian(ylim = y_limits)

 add_sample_annotation(plot, n_events, n_treated_units, n_control_units)
}

plot_group_att <- function(out, spec_name, timing_name,
                           n_events, n_treated_units, n_control_units) {
 plot <- did::ggdid(out) +
  labs(
   title = str_wrap(
    "Group-specific ATT by cohort - Selected vs runner-up counties",
    width = 54
   ),
   subtitle = str_wrap(
    paste0(
     "HYDE inventor rate, pre-1900 university foundations, ",
     str_to_title(spec_name), " controls, ",
     str_replace_all(timing_name, "_", " ")
    ),
    width = 68
   )
  )

 add_sample_annotation(plot, n_events, n_treated_units, n_control_units)
}

run_event_study <- function(data, controls, spec_name, timing_name,
                            treat_var, title_add, y_limits = NULL,
                            window = 70) {
 control_prep <- prepare_controls_for_spec(data, controls)
 data_es <- control_prep$data

 xformla <- if (length(control_prep$formula_vars) > 0) {
  as.formula(paste("~", paste(control_prep$formula_vars, collapse = " + ")))
 } else {
  ~ 1
 }

 out <- did::att_gt(
  yname = "inv_per_100k",
  tname = "decade",
  idname = "stack_unit_id",
  gname = treat_var,
  xformla = xformla,
  data = data_es,
  control_group = "nevertreated",
  est_method = "reg",
  base_period = "universal",
  cores = 4
 )

 es <- did::aggte(
  out,
  type = "dynamic",
  na.rm = TRUE,
  min_e = -window,
  max_e = window
 )

 if (is.null(y_limits)) {
  y_limits <- dynamic_y_limits(extract_dynamic_att(es, spec_name, timing_name))
 }

 plot <- plot_dynamic_event_study(es, title_add, y_limits)

 list(
  out = out,
  es = es,
  plot = plot,
  n_units = n_distinct(data_es$stack_unit_id),
  n_rows = nrow(data_es),
  n_events = n_distinct(data_es$event_id),
  n_treated_units = n_distinct(data_es$stack_unit_id[data_es[[treat_var]] > 0]),
  n_control_units = n_distinct(data_es$stack_unit_id[data_es[[treat_var]] == 0]),
  formula_vars = list(control_prep$formula_vars),
  missing_indicator_vars = list(control_prep$missing_indicator_vars)
 )
}

summarise_missing_controls <- function(data, controls, spec_name, timing_name) {
 data %>%
  distinct(stack_unit_id, across(all_of(controls))) %>%
  summarise(across(all_of(controls), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "control", values_to = "missing_units") %>%
  mutate(
   spec = spec_name,
   timing = timing_name,
   total_units = n_distinct(data$stack_unit_id),
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
# Load data
###############################################################################

panel <- read_csv(
 output_file_path("county_inventor_rates_hyde.csv"),
 show_col_types = FALSE
) %>%
 transmute(
  GEOID = as_geoid(GEOID),
  decade = year,
  inv_per_100k = replace_na(inventors_per_100k_hyde, 0)
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

events_pre1900 <- pairs_long %>%
 filter(experiment_year < 1900)

events_1900_or_later <- pairs_long %>%
 filter(experiment_year >= 1900)

event_lookup <- events_pre1900 %>%
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

runner_unresolved_rows <- events_pre1900 %>%
 filter(!runner_up_match_status %in% c("matched_same_state", "matched_cross_state")) %>%
 distinct(college, experiment_year, runner_up_county, runner_up_state_assumed,
          runner_up_match_status)

runner_units <- events_pre1900 %>%
 filter(runner_up_match_status %in% c("matched_same_state", "matched_cross_state")) %>%
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

stack_units_base <- bind_rows(treated_units, runner_units) %>%
 arrange(event_id, desc(sample_role == "treated"), GEOID) %>%
 distinct(event_id, GEOID, .keep_all = TRUE) %>%
 mutate(stack_unit_id = row_number(), .before = 1)

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
 "hyde_population_thousand", "hyde_population_control_year", "covariate", "prior",
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

build_stacked_panel <- function(timing_name, treat_var) {
 event_timing <- event_lookup %>%
  transmute(
   event_id,
   event_treatment_decade = .data[[treat_var]],
   control_year = event_treatment_decade - 10
  )

 stack_units <- stack_units_base %>%
  select(-g_std, -g_shift) %>%
  left_join(event_timing, by = "event_id") %>%
  mutate(
   g = if_else(sample_role == "treated", event_treatment_decade, 0),
   timing = timing_name
  )

 nearest_controls <- build_nearest_controls(stack_units)

 stack_units %>%
  left_join(nearest_controls, by = "stack_unit_id") %>%
  left_join(panel, by = "GEOID") %>%
  mutate(stack_unit_id = as.numeric(stack_unit_id))
}

panel_std <- build_stacked_panel("standard_decade", "g_std")
panel_shift <- build_stacked_panel("alternative_decade", "g_shift")

nearest_control_source_audit <- bind_rows(panel_std, panel_shift) %>%
 distinct(
  timing,
  event_id,
  stack_unit_id,
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
 select(
  timing,
  ends_with("_control_year")
 ) %>%
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
# Control specifications
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

missing_controls <- imap_dfr(control_sets, function(controls, spec_name) {
 bind_rows(
  summarise_missing_controls(panel_std, controls, spec_name, "standard_decade"),
  summarise_missing_controls(panel_shift, controls, spec_name, "alternative_decade")
 )
})

###############################################################################
# Run event studies
###############################################################################

window_decades <- 70

raw_results <- imap(control_sets, function(controls, spec_name) {
 std_initial <- run_event_study(
  data = panel_std,
  controls = controls,
  spec_name = spec_name,
  timing_name = "standard_decade",
  treat_var = "g",
  title_add = paste0("HYDE inventor rate - ", str_to_title(spec_name),
                     " Controls - Standard Decade"),
  window = window_decades
 )

 shift_initial <- run_event_study(
  data = panel_shift,
  controls = controls,
  spec_name = spec_name,
  timing_name = "alternative_decade",
  treat_var = "g",
  title_add = paste0("HYDE inventor rate - ", str_to_title(spec_name),
                     " Controls - Alternative Decade"),
  window = window_decades
 )

 spec_att <- bind_rows(
  extract_dynamic_att(std_initial$es, spec_name, "standard_decade"),
  extract_dynamic_att(shift_initial$es, spec_name, "alternative_decade")
 )

 spec_y_limits <- dynamic_y_limits(spec_att)

 std_initial$plot <- plot_dynamic_event_study(
 std_initial$es,
  paste0("HYDE inventor rate - ", str_to_title(spec_name),
         " Controls - Standard Decade"),
  spec_y_limits,
  n_events = std_initial$n_events,
  n_treated_units = std_initial$n_treated_units,
  n_control_units = std_initial$n_control_units
 )
 std_initial$y_min <- spec_y_limits[[1]]
 std_initial$y_max <- spec_y_limits[[2]]

 shift_initial$plot <- plot_dynamic_event_study(
 shift_initial$es,
  paste0("HYDE inventor rate - ", str_to_title(spec_name),
         " Controls - Alternative Decade"),
  spec_y_limits,
  n_events = shift_initial$n_events,
  n_treated_units = shift_initial$n_treated_units,
  n_control_units = shift_initial$n_control_units
 )
 shift_initial$y_min <- spec_y_limits[[1]]
 shift_initial$y_max <- spec_y_limits[[2]]

 list(
  standard_decade = std_initial,
  alternative_decade = shift_initial
 )
})

###############################################################################
# Export outputs
###############################################################################

walk2(raw_results, names(raw_results), function(spec_results, spec_name) {
 walk2(spec_results, names(spec_results), function(result, timing_name) {
  timing_suffix <- if_else(timing_name == "standard_decade", "std", "alt")

  ggsave(
   filename = results_subdir_path(
    paste0("ES_county_pairs_hyde_pre1900_", spec_name, "_",
           timing_suffix, ".png")
   ),
   plot = result$plot,
   width = 8,
   height = 6,
   dpi = 300
  )

  ggsave(
   filename = results_subdir_path(
    paste0("ggdid_county_pairs_hyde_pre1900_", spec_name, "_",
           timing_suffix, ".png")
   ),
   plot = plot_group_att(
    result$out,
    spec_name,
    timing_name,
    result$n_events,
    result$n_treated_units,
    result$n_control_units
   ),
   width = 8,
   height = 6,
   dpi = 300
  )
 })
})

dynamic_att_summary <- imap_dfr(raw_results, function(spec_results, spec_name) {
 imap_dfr(spec_results, function(result, timing_name) {
  extract_dynamic_att(result$es, spec_name, timing_name)
 })
})

sample_summary <- imap_dfr(raw_results, function(spec_results, spec_name) {
 imap_dfr(spec_results, function(result, timing_name) {
  tibble(
   spec = spec_name,
   timing = timing_name,
   n_events = result$n_events,
   n_units = result$n_units,
   n_rows = result$n_rows,
   n_treated_units = result$n_treated_units,
   n_control_units = result$n_control_units,
   y_min = result$y_min,
   y_max = result$y_max
  )
 })
})

formula_summary <- imap_dfr(raw_results, function(spec_results, spec_name) {
 imap_dfr(spec_results, function(result, timing_name) {
  tibble(
   spec = spec_name,
   timing = timing_name,
   formula_vars = paste(result$formula_vars[[1]], collapse = ", "),
   missing_indicator_vars = paste(
    result$missing_indicator_vars[[1]],
    collapse = ", "
   )
  )
 })
})

write_csv(
 dynamic_att_summary,
 results_subdir_path("county_pairs_hyde_pre1900_controls_dynamic_att.csv"),
 na = ""
)

write_csv(
 sample_summary,
 results_subdir_path("county_pairs_hyde_pre1900_controls_sample_summary.csv"),
 na = ""
)

write_csv(
 missing_controls,
 results_subdir_path("county_pairs_hyde_pre1900_controls_missing_controls.csv"),
 na = ""
)

write_csv(
 formula_summary,
 results_subdir_path("county_pairs_hyde_pre1900_controls_formula_vars.csv"),
 na = ""
)

write_csv(
 nearest_control_source_audit,
 results_subdir_path("county_pairs_hyde_pre1900_controls_source_years.csv"),
 na = ""
)

write_csv(
 nearest_control_source_distribution,
 results_subdir_path("county_pairs_hyde_pre1900_controls_source_year_distribution.csv"),
 na = ""
)

summary_lines <- c(
 "County pairs event-study sample: HYDE inventor rate, pre-1900 university foundations with controls",
 paste0("Source workbook: ", pairs_path),
 paste0("Generated on: ", Sys.Date()),
 "",
 paste0("Panel decades: ", min(panel$decade), "-", max(panel$decade)),
 paste0("Outcome: inventors_per_100k_hyde"),
 "Estimation method: did::att_gt with est_method = 'reg'.",
 paste0("Dynamic window: +/-", window_decades, " years"),
 "Control timing: nearest non-missing pre-treatment decade for every covariate.",
 "Production controls use the nearest non-missing pre-treatment decade, with 1840 allowed for the 1839 event.",
 "Slave share uses the nearest non-missing pre-treatment value before 1870; 1870+ slave-share zeros are not used.",
 "Each control specification uses complete cases for that specification after applying the nearest-control rules.",
 "",
 paste0("Unique treated university events before 1900: ", nrow(event_lookup)),
 paste0("Event-year range used: ",
        min(event_lookup$experiment_year, na.rm = TRUE), "-",
        max(event_lookup$experiment_year, na.rm = TRUE)),
 paste0("Unique treated university events excluded at or after 1900: ",
        n_distinct(events_1900_or_later$college,
                   events_1900_or_later$experiment_year,
                   events_1900_or_later$selected_county,
                   events_1900_or_later$selected_state)),
 paste0("Stacked experiment-county units before control missingness filter: ",
        n_distinct(stack_units_base$stack_unit_id)),
 paste0("Unresolved runner-up rows excluded: ", nrow(runner_unresolved_rows)),
 "",
 "Sample summary by spec and timing:",
 sample_summary %>%
  transmute(
   line = paste0(
    "- ", spec, " / ", timing,
    ": events=", n_events,
    ", units=", n_units,
    ", treated=", n_treated_units,
    ", controls=", n_control_units,
    ", y=[", round(y_min, 3), ", ", round(y_max, 3), "]"
   )
  ) %>%
  pull(line),
 "",
 "Formula variables by spec and timing are saved in county_pairs_hyde_pre1900_controls_formula_vars.csv.",
 "",
 "Earliest treated events:",
 event_lookup %>%
  arrange(experiment_year, college) %>%
  transmute(
   line = paste0(
    "- ", college, ": ", selected_county, ", ", selected_state,
    " | experiment_year=", experiment_year,
    " | g_std=", g_std,
    " | std_control_year=", g_std - 10,
    " | g_shift=", g_shift,
    " | shift_control_year=", g_shift - 10
   )
  ) %>%
  slice_head(n = 10) %>%
  pull(line)
)

writeLines(
 summary_lines,
 con = results_subdir_path("county_pairs_hyde_pre1900_controls_es_sample.txt")
)

message("Saved controlled HYDE inventor-rate university event-study outputs in: ",
        results_subdir_path("."))
message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
