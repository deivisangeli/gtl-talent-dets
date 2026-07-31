###############################################################################
# Build the Andrews-AMWS event-study panel with calendar support 1830-1950.
# AMWS counts are constructed independently of population availability.
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(readxl)
  library(sf)
  library(stringr)
  library(tidyr)
  library(tigris)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "prep", "raw_paths.R"))

options(tigris_use_cache = TRUE, tigris_cache_dir = tigris_cache_dir())

analysis_min_year <- 1830L
analysis_max_year <- 1959L
event_year_min <- 1850L
event_year_max <- 1920L

normalize_geoid <- function(x) {
  y <- suppressWarnings(as.integer(x))
  ifelse(is.na(y), NA_character_, sprintf("%05d", y))
}

normalize_name <- function(x) {
  x %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    tolower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("\\bexperiment station\\b|\\bexperiment\\b", " ") %>%
    str_replace_all(
      "\\bcounty\\b|\\bparish\\b|\\bborough\\b|\\bcensus area\\b|\\bmunicipality\\b|\\bcity and borough\\b|\\bcity\\b",
      " "
    ) %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

has_value <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & x != "" & !toupper(x) %in% c("NA", "N/A")
}

kept_flag <- function(x) as.character(x) %in% c("TRUE", "True", "true", "1")

first_pos <- function(pattern, text) {
  as.integer(regexpr(pattern, as.character(text), ignore.case = TRUE, perl = TRUE))
}

is_see_previous_contaminated <- function(text) {
  text <- as.character(text)
  text[is.na(text)] <- ""
  see_pos <- first_pos("\\bsee\\s+prev(?:ious)?(?:\\s+edition)?\\b", text)
  birth_pos <- first_pos("(^|[[:space:],.;])b[[:space:]]+", text)
  see_pos > 0L & (birth_pos < 0L | birth_pos > see_pos)
}

collapse_source <- function(x) {
  x <- sort(unique(na.omit(x)))
  if (length(x) == 0L) "missing" else paste(x, collapse = "|")
}

###############################################################################
# Events and county mapping
###############################################################################

pairs_path <- raw_file_path("andrews_2023_county_pairs_long.xlsx")
pairs <- read_excel(pairs_path, sheet = "county_pairs_long")

events <- pairs %>%
  distinct(college, experiment_year, college_type, selected_county, selected_state) %>%
  filter(between(experiment_year, event_year_min, event_year_max)) %>%
  arrange(experiment_year, college, selected_state, selected_county) %>%
  mutate(
    event_id = row_number(),
    g_std = floor(experiment_year / 10) * 10L,
    g_shift = if_else(
      experiment_year %% 10 >= 7,
      floor(experiment_year / 10) * 10L + 10L,
      floor(experiment_year / 10) * 10L
    )
  )

county_lookup <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_drop_geometry() %>%
  select(GEOID, county_name = NAME, STATEFP) %>%
  filter(as.integer(STATEFP) <= 56)

state_lookup <- tigris::states(cb = TRUE, year = 2020) %>%
  st_drop_geometry() %>%
  select(STATEFP, state_name = NAME)

lookup <- county_lookup %>%
  left_join(state_lookup, by = "STATEFP") %>%
  transmute(
    GEOID = normalize_geoid(GEOID),
    county_norm = normalize_name(county_name),
    state_norm = normalize_name(state_name)
  )

treated_units <- events %>%
  transmute(
    event_id, college, experiment_year, college_type, g_std, g_shift,
    sample_role = "treated", county = selected_county, state = selected_state,
    county_norm = normalize_name(selected_county),
    state_norm = normalize_name(selected_state)
  ) %>%
  left_join(lookup, by = c("county_norm", "state_norm"))

runner_unresolved <- pairs %>%
  inner_join(events %>% select(event_id, college, experiment_year), by = c("college", "experiment_year")) %>%
  filter(!runner_up_match_status %in% c("matched_same_state", "matched_cross_state")) %>%
  distinct(event_id, college, experiment_year, runner_up_county,
           runner_up_state_assumed, runner_up_match_status)

runner_units <- pairs %>%
  filter(runner_up_match_status %in% c("matched_same_state", "matched_cross_state")) %>%
  inner_join(
    events %>% select(event_id, college, experiment_year, college_type, g_std, g_shift),
    by = c("college", "experiment_year", "college_type")
  ) %>%
  transmute(
    event_id, college, experiment_year, college_type, g_std, g_shift,
    sample_role = "runner_up", county = runner_up_county,
    state = runner_up_state_assumed,
    county_norm = normalize_name(runner_up_county),
    state_norm = normalize_name(runner_up_state_assumed)
  ) %>%
  left_join(lookup, by = c("county_norm", "state_norm")) %>%
  distinct(event_id, GEOID, sample_role, .keep_all = TRUE)

units <- bind_rows(treated_units, runner_units) %>%
  select(event_id, college, experiment_year, college_type, g_std, g_shift,
         sample_role, GEOID, county, state)

if (any(is.na(units$GEOID))) {
  bad <- units %>% filter(is.na(GEOID)) %>% distinct(event_id, sample_role, county, state)
  stop("Unresolved county mappings: ", paste(capture.output(print(bad)), collapse = " "))
}

event_support <- units %>%
  group_by(event_id) %>%
  summarise(
    has_treated = any(sample_role == "treated"),
    n_controls = n_distinct(GEOID[sample_role == "runner_up"]),
    .groups = "drop"
  )

if (nrow(events) != 57L || any(!event_support$has_treated) || any(event_support$n_controls < 1L)) {
  stop("Expected 57 events, each with one treated unit and at least one control.")
}

###############################################################################
# AMWS county-year counts independent of population
###############################################################################

early <- fread(output_file_path("amws", "amws_combined_us_geocoded.csv"))
early <- early[kept_flag(kept) & has_value(birth_year) & has_value(geoid)]
early[, `:=`(year = suppressWarnings(as.integer(birth_year)), GEOID = normalize_geoid(geoid))]
early <- early[between(year, analysis_min_year, analysis_max_year) & !is.na(GEOID)]
early_cy <- early[, .(n_amws_1906_1955_dedup = .N), by = .(GEOID, year)]

ed86_path <- file.path(require_det_dir(), "Data", "processed", "amws", "amws_ed86_final.csv")
ed86 <- fread(ed86_path)
required_ed86 <- c("birth_year", "birth_country", "geo_geoid", "raw_text_adjusted")
if (length(setdiff(required_ed86, names(ed86)))) stop("Missing required columns in ", ed86_path)
ed86[, `:=`(
  year = suppressWarnings(as.integer(birth_year)),
  GEOID = normalize_geoid(geo_geoid),
  see_previous_contaminated = is_see_previous_contaminated(raw_text_adjusted)
)]
ed86 <- ed86[
  birth_country == "USA" & has_value(birth_year) & has_value(geo_geoid) &
    between(year, analysis_min_year, analysis_max_year) & !is.na(GEOID) &
    see_previous_contaminated != TRUE
]
ed86_cy <- ed86[, .(n_amws_1986 = .N), by = .(GEOID, year)]

###############################################################################
# NHGIS/manual-only annual population interpolation
###############################################################################

relevant_geoids <- sort(unique(units$GEOID))
years <- seq(analysis_min_year, analysis_max_year)

pop_knots <- read_csv(output_file_path("county_population.csv"), show_col_types = FALSE) %>%
  mutate(GEOID = normalize_geoid(GEOID), decade = as.integer(decade)) %>%
  filter(GEOID %in% relevant_geoids, source %in% c("nhgis", "manual"),
         !is.na(population), population > 0) %>%
  select(GEOID, year = decade, population, source)

interpolate_population <- function(geoid) {
  knots <- pop_knots %>% filter(GEOID == geoid) %>% arrange(year)
  out <- tibble(GEOID = geoid, year = years)
  if (nrow(knots) >= 2L) {
    out$population <- approx(knots$year, knots$population, xout = years,
                             method = "linear", rule = 1)$y
  } else if (nrow(knots) == 1L) {
    out$population <- ifelse(years == knots$year, knots$population, NA_real_)
  } else {
    out$population <- NA_real_
  }
  out %>%
    left_join(knots %>% select(year, observed_source = source), by = "year") %>%
    mutate(
      population_source = case_when(
        !is.na(observed_source) ~ observed_source,
        !is.na(population) ~ "nhgis_interpolated",
        TRUE ~ "missing"
      )
    ) %>%
    select(-observed_source)
}

population_year <- purrr::map_dfr(relevant_geoids, interpolate_population)

birth_rate <- fread(output_file_path("us_panel_county_stem_year_1800.csv"))[
  year %in% years,
  .(us_birth_rate_year = first(na.omit(us_birth_rate_year))),
  by = year
]

skeleton <- CJ(GEOID = relevant_geoids, year = years, unique = TRUE)
panel_year <- merge(skeleton, as.data.table(population_year), by = c("GEOID", "year"), all.x = TRUE)
panel_year <- merge(panel_year, early_cy, by = c("GEOID", "year"), all.x = TRUE)
panel_year <- merge(panel_year, ed86_cy, by = c("GEOID", "year"), all.x = TRUE)
panel_year <- merge(panel_year, birth_rate, by = "year", all.x = TRUE)
panel_year[is.na(n_amws_1906_1955_dedup), n_amws_1906_1955_dedup := 0L]
panel_year[is.na(n_amws_1986), n_amws_1986 := 0L]
panel_year[, n_amws := n_amws_1906_1955_dedup + n_amws_1986]
panel_year[, county_births_estimate_year := population * us_birth_rate_year]
panel_year[, decade := floor(year / 10) * 10L]

panel_decade <- panel_year[, .(
  n_amws_1906_1955_dedup = sum(n_amws_1906_1955_dedup),
  n_amws_1986 = sum(n_amws_1986),
  n_amws = sum(n_amws),
  population = if (all(is.na(population))) NA_real_ else mean(population, na.rm = TRUE),
  county_births_estimate = if (all(is.na(county_births_estimate_year))) NA_real_ else sum(county_births_estimate_year, na.rm = TRUE),
  population_source = collapse_source(population_source)
), by = .(GEOID, decade)]

panel_decade[, `:=`(
  log1p_n_amws = log1p(n_amws),
  amws_per_100k = fifelse(population > 0, 1e5 * n_amws / population, NA_real_),
  amws_per_1000_births = fifelse(county_births_estimate > 0, 1000 * n_amws / county_births_estimate, NA_real_)
)]

###############################################################################
# Outputs and audits
###############################################################################

panel_out <- output_file_path("land_grants", "amws_temporal_support_county_decade_1830_1950.csv")
units_out <- output_file_path("land_grants", "andrews_event_county_units_1850_1920.csv")
audit_out <- output_file_path("land_grants", "amws_temporal_support_build_audit.csv")
unresolved_out <- output_file_path("land_grants", "andrews_runner_unresolved_1850_1920.csv")

fwrite(panel_decade, panel_out)
write_csv(units, units_out, na = "")
write_csv(runner_unresolved, unresolved_out, na = "")

audit <- tibble(
  metric = c(
    "events", "treated_units", "control_event_county_units", "stacked_units",
    "physical_counties", "calendar_decades", "count_panel_rows",
    "ed86_source_file", "ed86_valid_rows_1830_1959", "unresolved_runner_rows"
  ),
  value = as.character(c(
    n_distinct(units$event_id),
    sum(units$sample_role == "treated"),
    sum(units$sample_role == "runner_up"),
    nrow(units),
    n_distinct(units$GEOID),
    n_distinct(panel_decade$decade),
    nrow(units) * n_distinct(panel_decade$decade),
    basename(ed86_path),
    nrow(ed86),
    nrow(runner_unresolved)
  ))
)
write_csv(audit, audit_out, na = "")

cat("wrote", panel_out, "\n")
cat("wrote", units_out, "\n")
cat("events:", n_distinct(units$event_id), " treated:", sum(units$sample_role == "treated"),
    " controls:", sum(units$sample_role == "runner_up"), " stacked units:", nrow(units), "\n")
cat("county-decade rows:", nrow(panel_decade), " count panel rows:", nrow(units) * n_distinct(panel_decade$decade), "\n")
