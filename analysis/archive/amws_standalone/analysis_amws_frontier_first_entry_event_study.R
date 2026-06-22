###############################################################################
# Project: GTL Talent Determinants
# Goal: AMWS event study using first county frontier entry as treatment
###############################################################################

rm(list = ls())

library("tidyverse")
library("ggplot2")
library("did")

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

results_subdir <- "amws_frontier_first_entry_event_study"

results_subdir_path <- function(...) {
 out_dir <- file.path(repo_root, "analysis", "results", "amws", results_subdir)
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

extract_dynamic_att <- function(es, outcome) {
 tibble(
  outcome = outcome,
  control_group = "nevertreated",
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

sanitize_filename <- function(x) {
 str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

value_or <- function(x, default) {
 if (is.null(x)) default else x
}

plot_dynamic_event_study <- function(es, outcome, y_limits) {
 did::ggdid(es) +
  labs(
   x = "Relative Time",
   y = "Effect",
   title = str_wrap(
    paste(
     "AMWS event study - first county frontier entry -",
     outcome
    ),
    width = 72
   )
  ) +
  coord_cartesian(ylim = y_limits)
}

run_event_study <- function(data, outcome, window = 70) {
 data_es <- data %>%
  select(unit_id, GEOID, decade, g, treatment_status, all_of(outcome)) %>%
  rename(y = all_of(outcome)) %>%
  filter(!is.na(y))

 if (n_distinct(data_es$g[data_es$g > 0]) == 0) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   error = "No treated cohorts in estimation sample."
  ))
 }

 if (n_distinct(data_es$g[data_es$g == 0]) == 0) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   error = "No never-treated controls in estimation sample."
  ))
 }

 if (n_distinct(data_es$y, na.rm = TRUE) < 2) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   error = "Outcome has insufficient variation."
  ))
 }

 tryCatch(
  {
   out <- did::att_gt(
    yname = "y",
    tname = "decade",
    idname = "unit_id",
    gname = "g",
    data = data_es,
    control_group = "nevertreated",
    est_method = "dr",
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
    out = out,
    es = es,
    n_rows = nrow(data_es),
    n_units = n_distinct(data_es$unit_id),
    n_treated_units = n_distinct(data_es$unit_id[data_es$g > 0]),
    n_control_units = n_distinct(data_es$unit_id[data_es$g == 0]),
    n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
    min_decade = min(data_es$decade, na.rm = TRUE),
    max_decade = max(data_es$decade, na.rm = TRUE),
    error = NA_character_
   )
  },
  error = function(e) {
   list(
    ok = FALSE,
    outcome = outcome,
    n_rows = nrow(data_es),
    n_units = n_distinct(data_es$unit_id),
    n_treated_units = n_distinct(data_es$unit_id[data_es$g > 0]),
    n_control_units = n_distinct(data_es$unit_id[data_es$g == 0]),
    n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
    min_decade = min(data_es$decade, na.rm = TRUE),
    max_decade = max(data_es$decade, na.rm = TRUE),
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
 filter(year >= 1840, decade >= 1840)

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

first_panel_decade <- min(panel$decade, na.rm = TRUE)
last_panel_decade <- max(panel$decade, na.rm = TRUE)

###############################################################################
# Build frontier first-entry treatment
###############################################################################

frontier_panel <- read_csv(
 output_file_path("county_tpe_covariates_clean.csv"),
 show_col_types = FALSE
) %>%
 mutate(
  GEOID = as_geoid(GEOID),
  year = as.integer(year),
  frontier = frontier100kmL6 == 1
 )

first_frontier <- frontier_panel %>%
 filter(frontier) %>%
 group_by(GEOID) %>%
 summarise(
  first_frontier_year = min(year, na.rm = TRUE),
  n_frontier_decades = n(),
  .groups = "drop"
 )

all_amws_counties <- panel %>%
 distinct(GEOID)

frontier_events_all <- all_amws_counties %>%
 left_join(first_frontier, by = "GEOID") %>%
 mutate(
  ever_frontier = !is.na(first_frontier_year),
  g_raw = if_else(ever_frontier, as.numeric(first_frontier_year), 0),
  treatment_status = case_when(
   !ever_frontier ~ "never_frontier",
   first_frontier_year <= first_panel_decade ~ "removed_always_treated",
   first_frontier_year > last_panel_decade ~ "outside_panel",
   TRUE ~ "treated_estimable"
  )
 )

frontier_events <- frontier_events_all %>%
 filter(treatment_status %in% c("treated_estimable", "never_frontier")) %>%
 mutate(
  g = if_else(treatment_status == "never_frontier", 0, as.numeric(g_raw)),
  unit_id = dense_rank(GEOID)
 )

if (!all(frontier_events$g == 0 | frontier_events$g > first_panel_decade)) {
 stop("Estimation sample includes always-treated counties.")
}

analysis_panel <- panel %>%
 inner_join(
  frontier_events %>%
   select(GEOID, unit_id, g, treatment_status, first_frontier_year),
  by = "GEOID"
 ) %>%
 arrange(GEOID, decade)

###############################################################################
# Event studies
###############################################################################

outcomes <- c(
 "amws_per_1000_births",
 "n_amws",
 "log1p_n_amws",
 "amws_per_100k"
)

window_years <- 70

model_results <- list()

for (outcome in outcomes) {
 message("Running AMWS frontier event study: ", outcome)

 model_results[[outcome]] <- run_event_study(
  data = analysis_panel,
  outcome = outcome,
  window = window_years
 )
}

successful_models <- keep(model_results, "ok")
failed_models <- discard(model_results, "ok")

if (length(successful_models) == 0) {
 stop("No frontier event-study model ran successfully.")
}

dynamic_att <- imap_dfr(
 successful_models,
 ~ extract_dynamic_att(.x$es, .x$outcome)
)

model_status <- imap_dfr(
 model_results,
 ~ tibble(
  outcome = .x$outcome,
  control_group = "nevertreated",
  ok = .x$ok,
  n_rows = value_or(.x$n_rows, NA_integer_),
  n_units = value_or(.x$n_units, NA_integer_),
  n_treated_units = value_or(.x$n_treated_units, NA_integer_),
  n_control_units = value_or(.x$n_control_units, NA_integer_),
  n_treated_cohorts = value_or(.x$n_treated_cohorts, NA_integer_),
  min_decade = value_or(.x$min_decade, NA_real_),
  max_decade = value_or(.x$max_decade, NA_real_),
  error = value_or(.x$error, NA_character_)
 )
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
  y_limits
 )

 safe_outcome <- sanitize_filename(model$outcome)

 ggsave(
  filename = results_subdir_path(
   paste0("ES_amws_frontier_first_entry_", safe_outcome, ".png")
  ),
  plot = plot_es,
  width = 9,
  height = 6,
  dpi = 300
 )

 ggsave(
  filename = results_subdir_path(
   paste0("ggdid_amws_frontier_first_entry_", safe_outcome, ".png")
  ),
  plot = did::ggdid(model$es),
  width = 9,
  height = 6,
  dpi = 300
 )
}

###############################################################################
# Export diagnostics
###############################################################################

write_csv(
 dynamic_att,
 results_subdir_path("amws_frontier_first_entry_dynamic_att.csv"),
 na = ""
)

write_csv(
 model_status,
 results_subdir_path("amws_frontier_first_entry_model_status.csv"),
 na = ""
)

event_distribution <- bind_rows(
 frontier_events_all %>%
  count(treatment_status, name = "n_counties") %>%
  mutate(distribution = "treatment_status", value = treatment_status) %>%
  select(distribution, value, n_counties),
 frontier_events %>%
  filter(g > 0) %>%
  count(g, name = "n_counties") %>%
  mutate(distribution = "treated_cohort", value = as.character(g)) %>%
  select(distribution, value, n_counties)
)

write_csv(
 event_distribution,
 results_subdir_path("amws_frontier_first_entry_event_distribution.csv"),
 na = ""
)

event_details <- frontier_events %>%
 select(
  GEOID,
  unit_id,
  treatment_status,
  g,
  first_frontier_year,
  n_frontier_decades
 ) %>%
 arrange(treatment_status, g, GEOID)

write_csv(
 event_details,
 results_subdir_path("amws_frontier_first_entry_events.csv"),
 na = ""
)

sample_summary <- tibble(
 total_amws_counties = nrow(frontier_events_all),
 ever_frontier_counties = sum(frontier_events_all$ever_frontier),
 never_frontier_controls = sum(frontier_events_all$treatment_status == "never_frontier"),
 removed_always_treated = sum(frontier_events_all$treatment_status == "removed_always_treated"),
 outside_panel = sum(frontier_events_all$treatment_status == "outside_panel"),
 treated_estimable = sum(frontier_events_all$treatment_status == "treated_estimable"),
 estimation_units = n_distinct(analysis_panel$unit_id),
 estimation_treated_units = n_distinct(analysis_panel$unit_id[analysis_panel$g > 0]),
 estimation_control_units = n_distinct(analysis_panel$unit_id[analysis_panel$g == 0]),
 treated_cohorts = n_distinct(analysis_panel$g[analysis_panel$g > 0]),
 panel_rows = nrow(analysis_panel),
 first_panel_decade = first_panel_decade,
 last_panel_decade = last_panel_decade,
 window_years = window_years
)

removed_always_distribution <- frontier_events_all %>%
 filter(treatment_status == "removed_always_treated") %>%
 count(g_raw, name = "n_counties") %>%
 arrange(g_raw)

write_csv(
 sample_summary,
 results_subdir_path("amws_frontier_first_entry_sample_summary.csv"),
 na = ""
)

notes_lines <- c(
 "AMWS event study using first county frontier entry as treatment",
 paste0("Generated on: ", Sys.Date()),
 "",
 paste0("AMWS panel file: ",
        output_file_path("us_panel_county_amws_combined_year.csv")),
 paste0("Frontier covariate file: ",
        output_file_path("county_tpe_covariates_clean.csv")),
 paste0("AMWS panel decades used: ", first_panel_decade, "-",
        last_panel_decade),
 paste0("Dynamic window: +/-", window_years, " years"),
 "Treatment: first decade where frontier100kmL6 == 1.",
 "Always-treated removal: drop counties with g <= first panel decade.",
 "Control group: never-treated counties only.",
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
"Removed always-treated cohort distribution:",
 capture.output(print(removed_always_distribution, n = Inf))
)

writeLines(
 notes_lines,
 con = results_subdir_path("amws_frontier_first_entry_notes.txt")
)

message("Saved AMWS frontier event-study outputs in: ",
        results_subdir_path("."))
message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
