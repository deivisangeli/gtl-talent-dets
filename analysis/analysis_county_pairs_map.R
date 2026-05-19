###############################################################################
# Project: Determinants of Talent Production via Scientific Infrastructure
# Author: GTF
# Goal: Map selected and runner-up counties from college site selection pairs
###############################################################################

rm(list = ls())

library("tidyverse")
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("tigris")

source("../prep/raw_paths.R")

initial_time <- Sys.time()

tigris_cache_path <- tigris_cache_dir()
options(tigris_use_cache = TRUE, tigris_cache_dir = tigris_cache_path)

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

state_lookup <- tibble(
 state = state.name,
 state_abbr = state.abb
)

###############################################################################
# Uploading databases
###############################################################################

pairs_path <- raw_file_path("andrews_2023_county_pairs_long.xlsx")

pairs_long <- readxl::read_excel(pairs_path, sheet = "county_pairs_long") %>%
 mutate(across(c(selected_county, selected_state, runner_up_county,
                 runner_up_state_assumed, runner_up_match_status), as.character))

###############################################################################
# County role tables
###############################################################################

treated_counties <- pairs_long %>%
 distinct(selected_county, selected_state) %>%
 transmute(
  county = selected_county,
  state = selected_state,
  map_role = "Treated county",
  role_rank = 2L
 )

untreated_counties <- pairs_long %>%
 filter(runner_up_match_status == "matched_same_state") %>%
 distinct(runner_up_county, runner_up_state_assumed) %>%
 transmute(
  county = runner_up_county,
  state = runner_up_state_assumed,
  map_role = "Untreated county",
  role_rank = 1L
 )

county_roles <- bind_rows(untreated_counties, treated_counties) %>%
 left_join(state_lookup, by = "state") %>%
 mutate(county_norm = normalize_county(county)) %>%
 arrange(role_rank) %>%
 group_by(state, county) %>%
 slice_tail(n = 1) %>%
 ungroup() %>%
 select(county, state, state_abbr, county_norm, map_role)

unresolved_runner_ups <- pairs_long %>%
 filter(runner_up_match_status != "matched_same_state") %>%
 distinct(college, runner_up_county, runner_up_state_assumed, runner_up_match_status) %>%
 arrange(college, runner_up_county)

###############################################################################
# Spatial objects
###############################################################################

counties_map <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 st_transform(4326) %>%
 select(GEOID, NAME, STATEFP, geometry) %>%
 filter(as.integer(substr(GEOID, 1, 2)) <= 56)

states_map <- tigris::states(cb = TRUE, year = 2020) %>%
 st_drop_geometry() %>%
 select(STATEFP, STUSPS)

us_sf <- ne_countries(
 country = "United States of America",
 scale = "medium",
 returnclass = "sf"
)

counties_plot <- counties_map %>%
 left_join(states_map, by = "STATEFP") %>%
 mutate(
  state_abbr = STUSPS,
  county_norm = normalize_county(NAME)
 ) %>%
 left_join(
  county_roles %>% select(state_abbr, county_norm, map_role),
  by = c("state_abbr", "county_norm")
 )

mapped_counties <- counties_plot %>%
 filter(!is.na(map_role))

###############################################################################
# Plot
###############################################################################

plot_treatment <- ggplot() +
 geom_sf(data = us_sf, fill = "gray95", color = "gray70") +
 geom_sf(
  data = mapped_counties,
  aes(fill = map_role),
  color = "grey80",
  linewidth = 0.05,
  alpha = 0.75
 ) +
 scale_fill_manual(
  values = c("Treated county" = "blue", "Untreated county" = "red"),
  name = "Counties"
 ) +
 coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
 theme_minimal() +
 labs(
  title = "Selected and runner-up counties in college site selection experiments",
  subtitle = paste0(
   n_distinct(treated_counties$county, treated_counties$state), " treated counties; ",
   n_distinct(untreated_counties$county, untreated_counties$state), " runner-up counties; ",
   nrow(unresolved_runner_ups), " unresolved runner-up counties excluded"
  ),
  caption = "Source: Andrews (2023) Appendix Table A1, Census Gazetteer, TIGER/Line"
 )

###############################################################################
# Saving results
###############################################################################

ggsave(
 filename = results_file_path("county_pairs_treatment_map.png"),
 plot = plot_treatment,
 width = 8,
 height = 6,
 dpi = 300
)

notes_path <- results_file_path("county_pairs_treatment_map_notes.txt")
notes_lines <- c(
 "County pairs treatment map",
 paste0("Source workbook: ", pairs_path),
 paste0("Generated on: ", Sys.Date()),
 "",
 paste0("Treated counties mapped: ",
        n_distinct(treated_counties$county, treated_counties$state)),
 paste0("Runner-up counties mapped: ",
        n_distinct(untreated_counties$county, untreated_counties$state)),
 paste0("Unresolved runner-up counties excluded: ", nrow(unresolved_runner_ups)),
 "",
 "Excluded unresolved runner-up counties:"
)

if (nrow(unresolved_runner_ups) > 0) {
 notes_lines <- c(
  notes_lines,
  unresolved_runner_ups %>%
   transmute(line = paste0(
    "- ", college, ": ", runner_up_county, ", ",
    runner_up_state_assumed, " (", runner_up_match_status, ")"
   )) %>%
   pull(line)
 )
}

writeLines(notes_lines, con = notes_path)

message("Saved map to: ", results_file_path("county_pairs_treatment_map.png"))
message("Saved notes to: ", notes_path)
message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
