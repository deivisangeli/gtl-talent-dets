###############################################################################
# Project: GTL Talent Determinants
# Goal: AMWS event studies using Andrews college site-selection experiments
#
# Balanced-panel specification:
# - all Andrews college types
# - county-decade AMWS panel balanced over 1850-1950
# - treatment cohorts 1880-1910
# - at least three pre-treatment and four post-treatment decades
# - did::att_gt with never-treated controls, DR, universal base period
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

results_subdir <- "amws_county_pairs_all_colleges_balanced_1850_1950"

control_group <- "nevertreated"
est_method <- "dr"
analysis_min_decade <- 1850L
analysis_max_decade <- 1950L
treatment_min_cohort <- 1880L
treatment_max_cohort <- 1910L
pre_event_window <- 30L
post_event_window <- 40L
analysis_decades <- seq(analysis_min_decade, analysis_max_decade, by = 10L)

results_subdir_path <- function(...) {
 out_dir <- results_file_path("land_grants", "event_studies", results_subdir)
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

extract_dynamic_att <- function(es, outcome, timing_name) {
 tibble(
  outcome = outcome,
  timing = timing_name,
  control_group = control_group,
  est_method = est_method,
  analysis_min_decade = analysis_min_decade,
  analysis_max_decade = analysis_max_decade,
  treatment_min_cohort = treatment_min_cohort,
  treatment_max_cohort = treatment_max_cohort,
  min_event_time = -pre_event_window,
  max_event_time = post_event_window,
  event_time = es$egt,
  estimate = es$att.egt,
  se = es$se.egt,
  ci_low = estimate - 1.96 * se,
  ci_high = estimate + 1.96 * se
 )
}

extract_simple_att <- function(ag, outcome, timing_name) {
 tibble(
  outcome = outcome,
  timing = timing_name,
  control_group = control_group,
  est_method = est_method,
  analysis_min_decade = analysis_min_decade,
  analysis_max_decade = analysis_max_decade,
  treatment_min_cohort = treatment_min_cohort,
  treatment_max_cohort = treatment_max_cohort,
  min_event_time = -pre_event_window,
  max_event_time = post_event_window,
  estimate = ag$overall.att,
  se = ag$overall.se,
  p_value = 2 * (1 - pnorm(abs(estimate / se))),
  ci_low = estimate - 1.96 * se,
  ci_high = estimate + 1.96 * se
 )
}

plot_y_limits <- function(plot_obj, padding = 0.12) {
 built <- ggplot_build(plot_obj)
 y_values <- built$data %>%
  map(~ {
   y_cols <- intersect(c("y", "ymin", "ymax", "yend"), names(.x))
   if (length(y_cols) == 0) {
    return(numeric())
   }
   unlist(.x[y_cols], use.names = FALSE)
  }) %>%
  unlist(use.names = FALSE)

 y_values <- y_values[is.finite(y_values)]

 if (length(y_values) == 0) {
  return(c(-1, 1))
 }

 y_range <- range(c(y_values, 0), na.rm = TRUE)
 y_span <- diff(y_range)

 if (!is.finite(y_span) || y_span == 0) {
  y_span <- max(abs(y_range), 1)
 }

 y_range + c(-padding, padding) * y_span
}

sanitize_filename <- function(x) {
 str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

format_sample_annotation <- function(model) {
 paste(
  paste0("Events: ", model$n_events),
  paste0("Treated: ", model$n_treated_units),
  paste0("Controls: ", model$n_control_units),
  sep = "\n"
 )
}

plot_dynamic_event_study <- function(es, outcome, timing_name,
                                     sample_annotation) {
 plot_base <- did::ggdid(es) +
  annotate(
   "label",
   x = Inf,
   y = Inf,
   label = sample_annotation,
   hjust = 1.05,
   vjust = 1.05,
   size = 3,
   linewidth = 0.2,
   alpha = 0.9
  ) +
  labs(
   x = "Relative Time",
   y = "Effect",
   title = str_wrap(
    paste(
     "AMWS event study - Andrews county pairs - all college types -",
     outcome,
     timing_name
    ),
    width = 72
   )
  )

 plot_scaled <- suppressMessages(
  plot_base +
   scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
 )

 plot_scaled +
  coord_cartesian(ylim = plot_y_limits(plot_base), clip = "off")
}

run_event_study <- function(data, outcome, timing_name,
                            min_event_time = -30,
                            max_event_time = 40,
                            cores = 4) {
 data_es <- data %>%
  select(stack_unit_num, GEOID, decade, g, sample_role, event_id,
         all_of(outcome)) %>%
  rename(y = all_of(outcome)) %>%
  filter(!is.na(y), is.finite(y))

 if (n_distinct(data_es$g[data_es$g > 0]) == 0) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   timing = timing_name,
   error = "No treated cohorts in estimation sample."
  ))
 }

 if (n_distinct(data_es$y, na.rm = TRUE) < 2) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   timing = timing_name,
   error = "Outcome has insufficient variation."
  ))
 }

 tryCatch(
  {
   out <- did::att_gt(
    yname = "y",
    tname = "decade",
    idname = "stack_unit_num",
    gname = "g",
    data = data_es,
    control_group = control_group,
    est_method = est_method,
    base_period = "universal",
    allow_unbalanced_panel = FALSE,
    cores = cores
   )

   es <- did::aggte(
    out,
    type = "dynamic",
    na.rm = TRUE,
    min_e = min_event_time,
    max_e = max_event_time
   )

   simple <- did::aggte(
    out,
    type = "simple",
    na.rm = TRUE,
    min_e = 0,
    max_e = max_event_time
   )

   list(
    ok = TRUE,
    outcome = outcome,
    timing = timing_name,
    out = out,
    es = es,
    simple = simple,
    n_rows = nrow(data_es),
    n_units = n_distinct(data_es$stack_unit_num),
    n_events = n_distinct(data_es$event_id),
    n_treated_units = n_distinct(data_es$stack_unit_num[data_es$g > 0]),
    n_control_units = n_distinct(data_es$stack_unit_num[data_es$g == 0]),
    min_decade = min(data_es$decade, na.rm = TRUE),
    max_decade = max(data_es$decade, na.rm = TRUE),
    error = NA_character_
   )
  },
  error = function(e) {
   list(
    ok = FALSE,
    outcome = outcome,
    timing = timing_name,
    error = conditionMessage(e)
   )
  }
 )
}

###############################################################################
# Load AMWS county-year panel and aggregate to county-decade
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
 filter(decade %in% analysis_decades)

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

complete_geoids <- panel %>%
 count(GEOID, name = "n_decades") %>%
 filter(n_decades == length(analysis_decades)) %>%
 pull(GEOID)

panel <- panel %>%
 filter(GEOID %in% complete_geoids)

if (n_distinct(panel$decade) != length(analysis_decades)) {
 stop("The balanced AMWS panel does not contain all requested decades.")
}

###############################################################################
# Load Andrews county pairs
###############################################################################

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
 filter(!runner_up_match_status %in% c("matched_same_state", "matched_cross_state")) %>%
 distinct(college, experiment_year, runner_up_county, runner_up_state_assumed,
          runner_up_match_status)

runner_units <- pairs_long %>%
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

first_panel_decade <- min(panel$decade, na.rm = TRUE)
last_panel_decade <- max(panel$decade, na.rm = TRUE)

build_stack_panel <- function(timing_var, timing_name) {
 event_sample <- event_lookup %>%
  mutate(treatment_decade = .data[[timing_var]]) %>%
  filter(
   treatment_decade >= treatment_min_cohort,
   treatment_decade <= treatment_max_cohort,
   treatment_decade - pre_event_window >= first_panel_decade,
   treatment_decade + post_event_window <= last_panel_decade
  ) %>%
  select(event_id, treatment_decade)

 treated_sample <- treated_units %>%
  semi_join(event_sample, by = "event_id") %>%
  left_join(event_sample, by = "event_id") %>%
  mutate(g = treatment_decade)

 runner_sample <- runner_units %>%
  semi_join(event_sample, by = "event_id") %>%
  left_join(event_sample, by = "event_id") %>%
  mutate(g = 0)

 stack_units <- bind_rows(treated_sample, runner_sample) %>%
  filter(GEOID %in% complete_geoids) %>%
  distinct(event_id, GEOID, sample_role, .keep_all = TRUE) %>%
  mutate(
   timing = timing_name,
   stack_unit_id = paste(event_id, GEOID, sample_role, sep = "_"),
   stack_unit_num = dense_rank(stack_unit_id)
  )

 event_with_support <- stack_units %>%
  group_by(event_id) %>%
  summarise(
   has_treated = any(sample_role == "treated"),
   n_runner_up = n_distinct(GEOID[sample_role == "runner_up"]),
   .groups = "drop"
  ) %>%
  filter(has_treated, n_runner_up > 0)

 stack_units <- stack_units %>%
  semi_join(event_with_support, by = "event_id") %>%
  mutate(stack_unit_num = dense_rank(stack_unit_id))

 panel %>%
  inner_join(stack_units, by = "GEOID") %>%
  arrange(event_id, sample_role, GEOID, decade)
}

panel_es_std <- build_stack_panel("g_std", "standard_decade")
panel_es_shift <- build_stack_panel("g_shift", "alternative_decade")

###############################################################################
# Event studies
###############################################################################

outcomes <- c(
 "amws_per_1000_births",
 "n_amws",
 "log1p_n_amws",
 "amws_per_100k"
)

model_results <- list()

for (timing_name in c("standard_decade", "alternative_decade")) {
 panel_timing <- if (timing_name == "standard_decade") {
  panel_es_std
 } else {
  panel_es_shift
 }

 for (outcome in outcomes) {
  message("Running AMWS event study: ", outcome, " | ", timing_name)

  model_results[[paste(outcome, timing_name, sep = "__")]] <- run_event_study(
   data = panel_timing,
   outcome = outcome,
   timing_name = timing_name,
   min_event_time = -pre_event_window,
   max_event_time = post_event_window
  )
 }
}

successful_models <- keep(model_results, "ok")
failed_models <- discard(model_results, "ok")

if (length(successful_models) == 0) {
 stop("No event-study model ran successfully.")
}

dynamic_att <- imap_dfr(
 successful_models,
 ~ extract_dynamic_att(.x$es, .x$outcome, .x$timing)
)

simple_att <- imap_dfr(
 successful_models,
 ~ extract_simple_att(.x$simple, .x$outcome, .x$timing)
)

model_status <- imap_dfr(
 model_results,
 ~ tibble(
  model = .y,
  outcome = .x$outcome,
  timing = .x$timing,
  ok = .x$ok,
  n_rows = ifelse(isTRUE(.x$ok), .x$n_rows, NA_integer_),
  n_units = ifelse(isTRUE(.x$ok), .x$n_units, NA_integer_),
  n_events = ifelse(isTRUE(.x$ok), .x$n_events, NA_integer_),
  n_treated_units = ifelse(isTRUE(.x$ok), .x$n_treated_units, NA_integer_),
  n_control_units = ifelse(isTRUE(.x$ok), .x$n_control_units, NA_integer_),
  min_decade = ifelse(isTRUE(.x$ok), .x$min_decade, NA_real_),
  max_decade = ifelse(isTRUE(.x$ok), .x$max_decade, NA_real_),
  error = .x$error
 )
)

###############################################################################
# Export plots and tables
###############################################################################

for (model in successful_models) {
 plot_es <- plot_dynamic_event_study(
  model$es,
  model$outcome,
  model$timing,
  format_sample_annotation(model)
 )

 plot_group <- did::ggdid(model$out) +
  labs(
   title = str_wrap(
    paste(
     "Group-specific ATT - AMWS - Andrews county pairs - all college types -",
     model$outcome,
     model$timing
    ),
    width = 72
   )
  )

 safe_outcome <- sanitize_filename(model$outcome)
 safe_timing <- sanitize_filename(model$timing)

 ggsave(
  filename = results_subdir_path(
   paste0("ES_amws_county_pairs_all_colleges_", safe_outcome, "_",
          safe_timing, ".png")
  ),
  plot = plot_es,
  width = 8,
  height = 6,
  dpi = 300
 )

 ggsave(
  filename = results_subdir_path(
   paste0("ggdid_amws_county_pairs_all_colleges_", safe_outcome, "_",
          safe_timing, ".png")
  ),
  plot = plot_group,
  width = 8,
  height = 6,
  dpi = 300
 )
}

write_csv(
 dynamic_att,
 results_subdir_path("amws_county_pairs_all_colleges_dynamic_att.csv"),
 na = ""
)

write_csv(
 simple_att,
 results_subdir_path("amws_county_pairs_all_colleges_simple_att.csv"),
 na = ""
)

write_csv(
 model_status,
 results_subdir_path("amws_county_pairs_all_colleges_model_status.csv"),
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

university_allocations_by_decade <- event_lookup %>%
 count(decade = g_std, name = "n_universities") %>%
 arrange(decade)

stopifnot(sum(university_allocations_by_decade$n_universities) == nrow(event_lookup))

write_csv(
 university_allocations_by_decade,
 results_subdir_path("university_allocations_by_decade.csv"),
 na = ""
)

plot_university_allocations_by_decade <- ggplot(
 university_allocations_by_decade,
 aes(x = decade, y = n_universities)
) +
 geom_col(fill = "#2f7786", width = 8) +
 geom_text(
  aes(label = n_universities),
  vjust = -0.3,
  size = 3.5
 ) +
 scale_x_continuous(
  breaks = university_allocations_by_decade$decade,
  labels = as.character(university_allocations_by_decade$decade)
 ) +
 scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
 labs(
  x = "Decade",
  y = "Number of universities allocated",
  title = "Universities allocated by decade"
 ) +
 theme_classic() +
 theme(
  plot.title = element_text(
   color = "darkgray",
   face = "bold",
   size = 12
  ),
  axis.title = element_text(
   color = "darkgray",
   face = "bold",
   size = 12
  ),
  axis.text.x = element_text(angle = 45, hjust = 1)
 )

ggsave(
 filename = results_subdir_path("university_allocations_by_decade.png"),
 plot = plot_university_allocations_by_decade,
 width = 8,
 height = 6,
 dpi = 300
)

write_csv(
 event_distribution,
 results_subdir_path("amws_county_pairs_all_colleges_event_distribution.csv"),
 na = ""
)

event_details <- event_lookup %>%
 left_join(
  treated_units %>% select(event_id, treated_GEOID = GEOID),
  by = "event_id"
 ) %>%
 mutate(
  candidate_standard_decade =
   g_std >= treatment_min_cohort &
    g_std <= treatment_max_cohort &
    g_std - pre_event_window >= first_panel_decade &
    g_std + post_event_window <= last_panel_decade,
  candidate_alternative_decade =
   g_shift >= treatment_min_cohort &
    g_shift <= treatment_max_cohort &
    g_shift - pre_event_window >= first_panel_decade &
    g_shift + post_event_window <= last_panel_decade,
  estimable_standard_decade = event_id %in% unique(panel_es_std$event_id),
  estimable_alternative_decade = event_id %in% unique(panel_es_shift$event_id)
 )

write_csv(
 event_details,
 results_subdir_path("amws_county_pairs_all_colleges_events.csv"),
 na = ""
)

sample_summary <- bind_rows(
 tibble(
  timing = "standard_decade",
  original_events = nrow(event_lookup),
  candidate_events = sum(event_details$candidate_standard_decade),
  estimable_events = n_distinct(panel_es_std$event_id),
  complete_counties = length(complete_geoids),
  treated_units = n_distinct(panel_es_std$stack_unit_num[panel_es_std$g > 0]),
  runner_up_units = n_distinct(panel_es_std$stack_unit_num[panel_es_std$g == 0]),
  stack_units = n_distinct(panel_es_std$stack_unit_num),
  panel_rows = nrow(panel_es_std),
  first_panel_decade = first_panel_decade,
  last_panel_decade = last_panel_decade,
  treatment_min_cohort = treatment_min_cohort,
  treatment_max_cohort = treatment_max_cohort,
  pre_event_window = pre_event_window,
  post_event_window = post_event_window,
  unresolved_runner_up_rows = nrow(runner_unresolved_rows)
 ),
 tibble(
  timing = "alternative_decade",
  original_events = nrow(event_lookup),
  candidate_events = sum(event_details$candidate_alternative_decade),
  estimable_events = n_distinct(panel_es_shift$event_id),
  complete_counties = length(complete_geoids),
  treated_units = n_distinct(panel_es_shift$stack_unit_num[panel_es_shift$g > 0]),
  runner_up_units = n_distinct(panel_es_shift$stack_unit_num[panel_es_shift$g == 0]),
  stack_units = n_distinct(panel_es_shift$stack_unit_num),
  panel_rows = nrow(panel_es_shift),
  first_panel_decade = first_panel_decade,
  last_panel_decade = last_panel_decade,
  treatment_min_cohort = treatment_min_cohort,
  treatment_max_cohort = treatment_max_cohort,
  pre_event_window = pre_event_window,
  post_event_window = post_event_window,
  unresolved_runner_up_rows = nrow(runner_unresolved_rows)
 )
)

write_csv(
 sample_summary,
 results_subdir_path("amws_county_pairs_all_colleges_sample_summary.csv"),
 na = ""
)

notes_lines <- c(
 "AMWS event study using Andrews high-quality college site-selection experiments",
 paste0("Source workbook: ", pairs_path),
 paste0("Generated on: ", Sys.Date()),
 "",
paste0("AMWS panel file: ",
        output_file_path("us_panel_county_amws_combined_year.csv")),
 paste0("AMWS panel decades used: ", first_panel_decade, "-",
        last_panel_decade),
 paste0("Treatment cohorts used: ", treatment_min_cohort, "-",
        treatment_max_cohort),
 paste0("Dynamic window: -", pre_event_window, " to +",
        post_event_window, " years"),
 paste0("Control group: ", control_group),
 paste0("Estimator: did::att_gt est_method = ", est_method,
        ", base_period = universal, allow_unbalanced_panel = FALSE"),
 paste0("Original Andrews events: ", nrow(event_lookup)),
 paste0("Unresolved runner-up rows excluded: ", nrow(runner_unresolved_rows)),
 "",
 "Event-study outcomes:",
 paste0("- ", outcomes),
 "",
 "Sample summary:",
 capture.output(print(sample_summary)),
 "",
 "Event distribution:",
 capture.output(print(event_distribution, n = Inf)),
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
 con = results_subdir_path("amws_county_pairs_all_colleges_notes.txt")
)

message("Saved AMWS county-pairs event-study outputs in: ",
        results_subdir_path("."))
message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
