###############################################################################
# Project: Determinants of Talent Production via Scientific Infrastructure
# Author: GTF
# Goal: Event study using selected and runner-up counties from college siting
###############################################################################

rm(list = ls())

library("tidyverse")
library("ggplot2")
library("did")
library("sf")
library("tigris")

source("../prep/raw_paths.R")

initial_time <- Sys.time()
options(tigris_use_cache = TRUE, tigris_cache_dir = tigris_cache_dir())

###############################################################################
# Helpers
###############################################################################

normalize_county <- function(x) {
 x %>%
  iconv(to = "ASCII//TRANSLIT") %>%
  tolower() %>%
  str_replace_all("&", "and") %>%
  str_replace_all("\\bcounty\\b|\\bparish\\b|\\bborough\\b|\\bcensus area\\b|\\bmunicipality\\b|\\bcity and borough\\b|\\bcity\\b", " ") %>%
  str_replace_all("[^a-z0-9]+", " ") %>%
  str_squish()
}

ESgraph <- function(data, type, window, control, treat_var, title_add = NULL) {

 out <- att_gt(
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

 es <- aggte(
  out,
  type = type,
  na.rm = TRUE,
  min_e = -window,
  max_e = window
 )

 plot <- ggdid(es) +
  labs(
   x = "Relative Time",
   y = "Effect",
   title = if (!is.null(title_add)) {
    paste0("Average Effect by Length of Exposure - ", title_add)
   } else {
    "Average Effect by Length of Exposure"
   }
  ) +
  coord_cartesian(ylim = c(-5, 5))

 list(out = out, es = es, plot = plot)
}

###############################################################################
# Uploading databases
###############################################################################

panel <- read_csv(output_file_path("us_panel_county.csv"), show_col_types = FALSE)

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

treated_pre1870 <- pairs_long %>%
 distinct(college, experiment_year, selected_county, selected_state) %>%
 filter(experiment_year < 1870)

treated_counties <- pairs_long %>%
 distinct(college, experiment_year, college_type, selected_county, selected_state) %>%
 filter(experiment_year >= 1870) %>%
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

runner_unresolved_rows <- pairs_long %>%
 filter(experiment_year >= 1870, runner_up_match_status != "matched_same_state") %>%
 distinct(college, experiment_year, runner_up_county, runner_up_state_assumed,
          runner_up_match_status)

runner_counties <- pairs_long %>%
 filter(experiment_year >= 1870, runner_up_match_status == "matched_same_state") %>%
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

if (any(is.na(treated_counties$GEOID))) {
 stop("Some treated counties could not be matched to GEOID.")
}

if (any(is.na(runner_counties$GEOID))) {
 stop("Some runner-up counties could not be matched to GEOID.")
}

treated_counties <- treated_counties %>%
 transmute(
  GEOID,
  college,
  experiment_year,
  college_type,
  county = selected_county,
  state = selected_state,
  g_std,
  g_shift
 )

###############################################################################
# Merge to county panel
###############################################################################

panel <- panel %>%
 mutate(
  GEOID = as.numeric(GEOID),
  inv_per_100k = replace_na(inv_per_100k, 0)
 )

first_panel_decade <- min(panel$decade, na.rm = TRUE)

treated_std <- treated_counties %>%
 filter(g_std > first_panel_decade)

treated_shift <- treated_counties %>%
 filter(g_shift > first_panel_decade)

treated_first_period_std <- treated_counties %>%
 filter(g_std <= first_panel_decade)

treated_first_period_shift <- treated_counties %>%
 filter(g_shift <= first_panel_decade)

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

es_std <- ESgraph(
 data = panel_es_std,
 type = "dynamic",
 window = window_decades,
 control = "nevertreated",
 treat_var = "g_std",
 title_add = "Selected vs runner-up counties - Standard Decade"
)

es_shift <- ESgraph(
 data = panel_es_shift,
 type = "dynamic",
 window = window_decades,
 control = "nevertreated",
 treat_var = "g_shift",
 title_add = "Selected vs runner-up counties - Alternative Decade"
)

plot_ggdid_std <- ggdid(es_std$out) +
 labs(
  title = "Group-specific ATT by cohort - Selected vs runner-up counties",
  subtitle = "Standard decade timing"
 )

plot_ggdid_shift <- ggdid(es_shift$out) +
 labs(
  title = "Group-specific ATT by cohort - Selected vs runner-up counties",
  subtitle = "Alternative decade timing"
 )

###############################################################################
# Saving outputs
###############################################################################

ggsave(
 filename = results_file_path("ES_county_pairs_std.png"),
 plot = es_std$plot,
 width = 8,
 height = 6,
 dpi = 300
)

ggsave(
 filename = results_file_path("ES_county_pairs_alt.png"),
 plot = es_shift$plot,
 width = 8,
 height = 6,
 dpi = 300
)

ggsave(
 filename = results_file_path("ggdid_county_pairs_std.png"),
 plot = plot_ggdid_std,
 width = 8,
 height = 6,
 dpi = 300
)

ggsave(
 filename = results_file_path("ggdid_county_pairs_alt.png"),
 plot = plot_ggdid_shift,
 width = 8,
 height = 6,
 dpi = 300
)

summary_lines <- c(
 "County pairs event-study sample",
 paste0("Source workbook: ", pairs_path),
 paste0("Generated on: ", Sys.Date()),
 "",
 paste0("Treated counties retained (standard decade): ", n_distinct(treated_std$GEOID)),
 paste0("Treated counties retained (alternative decade): ", n_distinct(treated_shift$GEOID)),
 paste0("Runner-up counties used: ", n_distinct(runner_counties$GEOID)),
 paste0("Dropped pre-1870 experiments: ", nrow(treated_pre1870)),
 paste0("Dropped first-period treated counties (standard decade): ", nrow(treated_first_period_std)),
 paste0("Dropped first-period treated counties (alternative decade): ", nrow(treated_first_period_shift)),
 paste0("Unresolved runner-up rows excluded: ", nrow(runner_unresolved_rows)),
 paste0("Panel decades: ", min(panel$decade), "-", max(panel$decade)),
 "",
 "Earliest retained treated cohorts (standard decade):",
 treated_std %>%
  arrange(experiment_year, county, state) %>%
  transmute(line = paste0("- ", college, ": ", county, ", ", state,
                          " | experiment_year=", experiment_year,
                          " | g_std=", g_std,
                          " | g_shift=", g_shift)) %>%
  slice_head(n = 10) %>%
  pull(line)
)

writeLines(summary_lines, con = results_file_path("county_pairs_es_sample.txt"))

message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
