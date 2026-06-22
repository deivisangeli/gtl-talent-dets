###############################################################################
# Project: GTL Talent Determinants
# Goal: Event study for pre-1900 university siting pairs using HYDE inventor rate
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
 script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]),
                              winslash = "/", mustWork = TRUE)
 repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
 cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
 repo_root <- if (basename(cwd) == "analysis") dirname(cwd) else if (basename(dirname(cwd)) == "analysis") dirname(dirname(cwd)) else cwd
}

source(file.path(repo_root, "prep", "raw_paths.R"))

options(tigris_use_cache = TRUE, tigris_cache_dir = tigris_cache_dir())

results_subdir <- "county_pairs_hyde_pre1900"

results_subdir_path <- function(...) {
 results_file_path("land_grants", results_subdir, ...)
}

###############################################################################
# Helpers
###############################################################################

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

extract_dynamic_att <- function(es, timing_name) {
 tibble(
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

plot_dynamic_event_study <- function(es, title_add, y_limits) {
 did::ggdid(es) +
  labs(
   x = "Relative Time",
   y = "Effect",
   title = paste0("Average Effect by Length of Exposure - ", title_add)
  ) +
  coord_cartesian(ylim = y_limits)
}

run_event_study <- function(data, type, window, control, treat_var,
                            title_add = NULL) {
 out <- did::att_gt(
  yname = "inv_per_100k",
  tname = "decade",
  idname = "GEOID",
  gname = treat_var,
  data = data,
  control_group = control,
  est_method = "dr",
  base_period = "universal",
  cores = 4
 )

 es <- did::aggte(
  out,
  type = type,
  na.rm = TRUE,
  min_e = -window,
  max_e = window
 )

 list(out = out, es = es, title_add = title_add)
}

###############################################################################
# Load panel and Andrews pairs
###############################################################################

panel <- read_csv(
 output_file_path("county_inventor_rates_hyde.csv"),
 show_col_types = FALSE
) %>%
 transmute(
  GEOID = as.numeric(GEOID),
  decade = year,
  inv_per_100k = replace_na(inventors_per_100k_hyde, 0)
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
  GEOID = as.numeric(GEOID),
  county_norm = normalize_county(NAME),
  state_abbr = STUSPS
 )

state_lookup <- tibble(
 state = state.name,
 state_abbr = state.abb
)

###############################################################################
# Build treated and control county sets
###############################################################################

events_pre1900 <- pairs_long %>%
 filter(experiment_year < 1900)

events_1900_or_later <- pairs_long %>%
 filter(experiment_year >= 1900)

treated_events <- events_pre1900 %>%
 distinct(college, experiment_year, college_type, selected_county, selected_state) %>%
 left_join(state_lookup, by = c("selected_state" = "state")) %>%
 mutate(
  county_norm = normalize_county(selected_county),
  g_std = floor(experiment_year / 10) * 10,
  g_shift = if_else(
   experiment_year %% 10 >= 7,
   floor(experiment_year / 10) * 10 + 10,
   floor(experiment_year / 10) * 10
  )
 ) %>%
 left_join(lookup, by = c("county_norm", "state_abbr"))

runner_unresolved_rows <- events_pre1900 %>%
 filter(runner_up_match_status != "matched_same_state") %>%
 distinct(college, experiment_year, runner_up_county, runner_up_state_assumed,
          runner_up_match_status)

runner_counties <- events_pre1900 %>%
 filter(runner_up_match_status == "matched_same_state") %>%
 distinct(runner_up_county, runner_up_state_assumed) %>%
 left_join(state_lookup, by = c("runner_up_state_assumed" = "state")) %>%
 mutate(county_norm = normalize_county(runner_up_county)) %>%
 left_join(lookup, by = c("county_norm", "state_abbr")) %>%
 transmute(
  GEOID,
  county = runner_up_county,
  state = runner_up_state_assumed,
  g_std = 0,
  g_shift = 0
 )

if (any(is.na(treated_events$GEOID))) {
 unmatched <- treated_events %>%
  filter(is.na(GEOID)) %>%
  distinct(college, experiment_year, selected_county, selected_state)
 stop(
  "Some treated counties could not be matched to GEOID: ",
  paste(
   paste(unmatched$college, unmatched$selected_county,
         unmatched$selected_state, sep = " | "),
   collapse = "; "
  )
 )
}

if (any(is.na(runner_counties$GEOID))) {
 stop("Some runner-up counties could not be matched to GEOID.")
}

treated_counties <- treated_events %>%
 group_by(GEOID) %>%
 summarise(
  first_experiment_year = min(experiment_year, na.rm = TRUE),
  college = collapse_unique(college[experiment_year == first_experiment_year]),
  college_type = collapse_unique(college_type[experiment_year == first_experiment_year]),
  county = first(selected_county),
  state = first(selected_state),
  g_std = min(g_std, na.rm = TRUE),
  g_shift = min(g_shift, na.rm = TRUE),
  .groups = "drop"
 )

###############################################################################
# Merge to county panel
###############################################################################

first_panel_decade <- min(panel$decade, na.rm = TRUE)

treated_std <- treated_counties %>%
 filter(g_std >= first_panel_decade)

treated_shift <- treated_counties %>%
 filter(g_shift >= first_panel_decade)

treated_before_panel_std <- treated_counties %>%
 filter(g_std < first_panel_decade)

treated_before_panel_shift <- treated_counties %>%
 filter(g_shift < first_panel_decade)

sample_counties_std <- bind_rows(
 treated_std %>% mutate(sample_role = "treated"),
 runner_counties %>% mutate(sample_role = "runner_up")
) %>%
 arrange(desc(sample_role == "treated")) %>%
 distinct(GEOID, .keep_all = TRUE)

sample_counties_shift <- bind_rows(
 treated_shift %>% mutate(sample_role = "treated"),
 runner_counties %>% mutate(sample_role = "runner_up")
) %>%
 arrange(desc(sample_role == "treated")) %>%
 distinct(GEOID, .keep_all = TRUE)

panel_es_std <- panel %>%
 inner_join(
  sample_counties_std %>%
   select(GEOID, sample_role, g_std),
  by = "GEOID"
 ) %>%
 mutate(g_std = if_else(is.na(g_std), 0, g_std))

panel_es_shift <- panel %>%
 inner_join(
  sample_counties_shift %>%
   select(GEOID, sample_role, g_shift),
  by = "GEOID"
 ) %>%
 mutate(g_shift = if_else(is.na(g_shift), 0, g_shift))

###############################################################################
# Event studies
###############################################################################

window_decades <- 70

es_std <- run_event_study(
 data = panel_es_std,
 type = "dynamic",
 window = window_decades,
 control = "nevertreated",
 treat_var = "g_std",
 title_add = "Selected vs runner-up counties - HYDE inventor rate - Standard Decade"
)

es_shift <- run_event_study(
 data = panel_es_shift,
 type = "dynamic",
 window = window_decades,
 control = "nevertreated",
 treat_var = "g_shift",
 title_add = "Selected vs runner-up counties - HYDE inventor rate - Alternative Decade"
)

dynamic_att_summary <- bind_rows(
 extract_dynamic_att(es_std$es, "standard_decade"),
 extract_dynamic_att(es_shift$es, "alternative_decade")
)

event_study_y_limits <- dynamic_y_limits(dynamic_att_summary)

plot_es_std <- plot_dynamic_event_study(
 es_std$es,
 es_std$title_add,
 event_study_y_limits
)

plot_es_shift <- plot_dynamic_event_study(
 es_shift$es,
 es_shift$title_add,
 event_study_y_limits
)

plot_ggdid_std <- did::ggdid(es_std$out) +
 labs(
  title = "Group-specific ATT by cohort - Selected vs runner-up counties",
  subtitle = "HYDE inventor rate, pre-1900 university foundations, standard decade"
 )

plot_ggdid_shift <- did::ggdid(es_shift$out) +
 labs(
  title = "Group-specific ATT by cohort - Selected vs runner-up counties",
  subtitle = "HYDE inventor rate, pre-1900 university foundations, alternative decade"
 )

###############################################################################
# Export outputs
###############################################################################

ggsave(
 filename = results_subdir_path("ES_county_pairs_hyde_pre1900_std.png"),
 plot = plot_es_std,
 width = 8,
 height = 6,
 dpi = 300
)

ggsave(
 filename = results_subdir_path("ES_county_pairs_hyde_pre1900_alt.png"),
 plot = plot_es_shift,
 width = 8,
 height = 6,
 dpi = 300
)

ggsave(
 filename = results_subdir_path("ggdid_county_pairs_hyde_pre1900_std.png"),
 plot = plot_ggdid_std,
 width = 8,
 height = 6,
 dpi = 300
)

ggsave(
 filename = results_subdir_path("ggdid_county_pairs_hyde_pre1900_alt.png"),
 plot = plot_ggdid_shift,
 width = 8,
 height = 6,
 dpi = 300
)

write_csv(
 dynamic_att_summary,
 results_subdir_path("county_pairs_hyde_pre1900_dynamic_att.csv"),
 na = ""
)

summary_lines <- c(
 "County pairs event-study sample: HYDE inventor rate, pre-1900 university foundations",
 paste0("Source workbook: ", pairs_path),
 paste0("Generated on: ", Sys.Date()),
 "",
 paste0("Panel decades: ", min(panel$decade), "-", max(panel$decade)),
 paste0("Outcome: inventors_per_100k_hyde"),
 paste0("Dynamic window: +/-", window_decades, " years"),
 paste0("Event-study y-axis limits: ",
        paste(round(event_study_y_limits, 3), collapse = " to ")),
 "",
 paste0("Unique treated university events before 1900: ",
        n_distinct(treated_events$college, treated_events$experiment_year,
                   treated_events$selected_county, treated_events$selected_state)),
 paste0("Event-year range used: ",
        min(treated_events$experiment_year, na.rm = TRUE), "-",
        max(treated_events$experiment_year, na.rm = TRUE)),
 paste0("Unique treated university events excluded at or after 1900: ",
        n_distinct(events_1900_or_later$college,
                   events_1900_or_later$experiment_year,
                   events_1900_or_later$selected_county,
                   events_1900_or_later$selected_state)),
 paste0("Treated counties retained (standard decade): ",
        n_distinct(treated_std$GEOID)),
 paste0("Treated counties retained (alternative decade): ",
        n_distinct(treated_shift$GEOID)),
 paste0("Runner-up counties used: ", n_distinct(runner_counties$GEOID)),
 paste0("Dropped before-panel treated counties (standard decade): ",
        nrow(treated_before_panel_std)),
 paste0("Dropped before-panel treated counties (alternative decade): ",
        nrow(treated_before_panel_shift)),
 paste0("Unresolved runner-up rows excluded: ", nrow(runner_unresolved_rows)),
 "",
 "Earliest retained treated cohorts (standard decade):",
 treated_std %>%
  arrange(first_experiment_year, county, state) %>%
  transmute(line = paste0("- ", college, ": ", county, ", ", state,
                          " | experiment_year=", first_experiment_year,
                          " | g_std=", g_std,
                          " | g_shift=", g_shift)) %>%
  slice_head(n = 10) %>%
  pull(line)
)

writeLines(
 summary_lines,
 con = results_subdir_path("county_pairs_hyde_pre1900_es_sample.txt")
)

message("Saved HYDE inventor-rate university event-study outputs in: ",
        results_subdir_path("."))
message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
