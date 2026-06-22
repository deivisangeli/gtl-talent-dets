###############################################################################
# Project: GTL Talent Determinants
# Goal: UK urban-unit event studies using distance to world's fair venues.
#
# Treatment:
#   - First exposure to a realized world's fair venue within distance bins:
#       0-2, 2-4, 4-6, 6-8, 8-10 km.
#   - Distance is measured from the harmonized urban-unit polygon to the venue.
#   - Greater London is excluded before treatment/control classification.
#   - Controls are units never exposed within 10 km over the full fair period
#     available in the consolidated file, 1790-1961.
#   - Units first exposed before 1840 are always-treated and excluded.
#   - Units first exposed after 1910 are future-treated and excluded.
#
# Run from analysis/:
#   Rscript analysis_worlds_fairs_uk_urban_unit_venue_distance_event_studies_no_london.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(did)
  library(sf)
})

initial_time <- Sys.time()
options(timeout = 1000)
sf_use_s2(FALSE)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
  if (basename(repo_root) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, ".."), winslash = "/", mustWork = TRUE)
  }
}

source(file.path(repo_root, "paths.R"))

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
  }
}

###############################################################################
# Paths and constants
###############################################################################

data_processed <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed")
results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results",
  "worlds_fairs_uk_urban_unit_venue_distance_event_studies_no_london_events_1840_1910"
)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
panel_file <- file.path(
  data_processed,
  "uk_historical_urban_units_inventor_panel_1801_1960_census_population.csv"
)
fairs_file <- file.path(
  DATA_INPUT,
  "worlds_fairs",
  "worlds_fairs_1790_1960_with_visits_venues.csv"
)
boundary_gpkg <- file.path(
  gbr_dir,
  "raw",
  "historical_boundaries",
  "uk_historical_districts_1921_1961.gpkg"
)
lau_gpkg <- file.path(
  TALENT_DETS_DATA_DIR,
  "raw",
  "gisco",
  "lau",
  "LAU_RG_01M_2019_4326.gpkg"
)

required_files <- c(panel_file, fairs_file, boundary_gpkg, lau_gpkg)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

greater_london_id <- "GBR_HIST_URBAN_GREATER_LONDON"
target_types <- c("Urban District", "Municipal Borough", "County Borough")
bin_breaks <- c(-1e-9, 2, 4, 6, 8, 10)
bin_labels <- c("0-2", "2-4", "4-6", "6-8", "8-10")
bin_dirs <- c(
  "0-2" = "bin_0_2km",
  "2-4" = "bin_2_4km",
  "4-6" = "bin_4_6km",
  "6-8" = "bin_6_8km",
  "8-10" = "bin_8_10km"
)
classification_year_min <- 1790L
classification_year_max <- 1961L
treated_event_year_min <- 1840L
treated_event_year_max <- 1910L
panel_year_min <- 1801L
panel_year_max <- 1960L
event_window <- 50L
control_group_name <- "nevertreated"

outcomes <- c(
  "inventors_per_100k_pop",
  "stem_per_100k_pop",
  "n_inventors",
  "log1p_n_inventors",
  "n_stem",
  "log1p_n_stem"
)

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
  if (length(x) == 0L) {
    return(NA_character_)
  }
  as.character(x[[1L]])
}

standard_decade <- function(year) {
  as.integer(floor(year / 10) * 10)
}

sanitize_filename <- function(x) {
  str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

extract_dynamic_att <- function(es, outcome, bin_label) {
  tibble(
    outcome = outcome,
    distance_bin_km = bin_label,
    control_group = control_group_name,
    event_time = es$egt,
    estimate = es$att.egt,
    se = es$se.egt,
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se
  )
}

extract_simple_att <- function(ag, outcome, bin_label) {
  tibble(
    outcome = outcome,
    distance_bin_km = bin_label,
    control_group = control_group_name,
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

plot_dynamic_event_study <- function(es, outcome, bin_label, y_limits) {
  did::ggdid(es) +
    labs(
      x = "Relative time (years)",
      y = "Effect",
      title = str_wrap(
        paste(
          "World's fairs venue-distance event study",
          paste0("bin ", bin_label, " km"),
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

build_target_geometries <- function() {
  districts_1921 <- st_read(boundary_gpkg, layer = "districts_1921", quiet = TRUE) %>%
    st_transform(27700) %>%
    st_make_valid()

  lau <- st_read(lau_gpkg, quiet = TRUE) %>%
    st_transform(27700) %>%
    st_make_valid()

  greater_london_lau <- lau[lau$CNTR_CODE == "UK" & grepl("^E090000", lau$LAU_ID), ]
  if (nrow(greater_london_lau) != 33L) {
    stop("Expected 33 Greater London LAUs, found ", nrow(greater_london_lau))
  }

  greater_london_sf <- st_sf(
    target_unit_id = greater_london_id,
    target_unit_name = "Greater London",
    target_area_type = "Greater London",
    target_boundary_id = greater_london_id,
    geometry = st_sfc(st_union(greater_london_lau), crs = 27700)
  )

  base_targets <- districts_1921[districts_1921$boundary_type %in% target_types, ]
  base_targets$target_row <- seq_len(nrow(base_targets))

  london_intersections <- suppressWarnings(st_intersection(
    base_targets[, c("target_row", "boundary_id", "boundary_name", "boundary_type")],
    greater_london_sf[, c("target_unit_id")]
  ))

  if (nrow(london_intersections) > 0L) {
    london_intersections$overlap_area_m2 <- as.numeric(st_area(london_intersections))
    london_overlap_rows <- unique(
      london_intersections$target_row[london_intersections$overlap_area_m2 > 1000]
    )
  } else {
    london_overlap_rows <- integer()
  }

  base_targets <- base_targets[!(base_targets$target_row %in% london_overlap_rows), ]

  targets <- st_sf(
    target_unit_id = paste0("GBR_HIST_URBAN_", base_targets$boundary_id),
    target_unit_name = base_targets$boundary_name,
    target_area_type = base_targets$boundary_type,
    target_boundary_id = base_targets$boundary_id,
    geometry = st_geometry(base_targets)
  ) %>%
    st_make_valid()

  targets[targets$target_unit_id != greater_london_id, ]
}

load_conservative_venues <- function() {
  fairs <- fread(fairs_file, na.strings = c("", "NA")) %>%
    as_tibble()

  if (!"parent_fair_id" %in% names(fairs)) {
    fairs$parent_fair_id <- fairs$fair_id
  }
  if (!"venue_seq" %in% names(fairs)) {
    fairs$venue_seq <- 1L
  }

  fairs <- fairs %>%
    mutate(
      fair_id = as.integer(fair_id),
      parent_fair_id = as.integer(parent_fair_id),
      venue_seq = as.integer(venue_seq),
      year_start = as.integer(year_start),
      host_matched_country_iso3 = as.character(host_matched_country_iso3),
      venue_longitude = as.numeric(venue_longitude),
      venue_latitude = as.numeric(venue_latitude),
      venue_coordinates_note = as.character(venue_coordinates_note)
    )

  venue_audit <- fairs %>%
    filter(
      year_start >= classification_year_min,
      year_start <= classification_year_max,
      host_matched_country_iso3 == "GBR"
    ) %>%
    mutate(
      has_venue_coordinates = !is.na(venue_longitude) & !is.na(venue_latitude),
      excluded_no_venue_coordinates = !has_venue_coordinates,
      excluded_low_quality_venue_coordinates =
        has_venue_coordinates &
          str_detect(
            venue_coordinates_note,
            fixed("automated geocoding returned no reliable coordinate")
          ),
      venue_used_conservative =
        has_venue_coordinates & !excluded_low_quality_venue_coordinates
    )

  venues <- venue_audit %>%
    filter(venue_used_conservative) %>%
    select(
      fair_id,
      parent_fair_id,
      venue_seq,
      year_start,
      City,
      Country,
      Fair_name,
      host_matched_name,
      host_admin1_name,
      venue,
      venue_longitude,
      venue_latitude,
      venue_coordinates_source_title,
      venue_coordinates_note
    )

  list(venues = venues, audit = venue_audit)
}

build_distance_exposure <- function(targets_sf, venues) {
  target_dt <- as_tibble(st_drop_geometry(targets_sf)) %>%
    select(target_unit_id, target_unit_name, target_area_type, target_boundary_id)

  venue_points <- venues %>%
    st_as_sf(coords = c("venue_longitude", "venue_latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(27700)

  distance_matrix <- matrix(
    as.numeric(st_distance(targets_sf, venue_points)),
    nrow = nrow(targets_sf),
    ncol = nrow(venues)
  )

  hit_index <- which(distance_matrix <= 10000, arr.ind = TRUE)
  if (nrow(hit_index) == 0L) {
    return(list(
      distance_audit = tibble(),
      first_exposure = tibble(),
      never_units = target_dt,
      always_units = tibble()
    ))
  }

  distance_audit <- tibble(
    target_row = hit_index[, 1],
    venue_row = hit_index[, 2],
    distance_km = distance_matrix[hit_index] / 1000
  ) %>%
    bind_cols(target_dt[.$target_row, ]) %>%
    bind_cols(venues[.$venue_row, ]) %>%
    mutate(
      distance_bin_km = cut(
        distance_km,
        breaks = bin_breaks,
        labels = bin_labels,
        include.lowest = TRUE,
        right = TRUE
      )
    ) %>%
    arrange(target_unit_id, year_start, distance_km, fair_id)

  first_exposure <- distance_audit %>%
    group_by(target_unit_id) %>%
    slice(1L) %>%
    ungroup() %>%
    transmute(
      unit_id = target_unit_id,
      target_unit_id,
      target_unit_name,
      target_area_type,
      target_boundary_id,
      first_exposure_year = year_start,
      first_exposure_decade = standard_decade(first_exposure_year),
      distance_bin_km = as.character(distance_bin_km),
      first_distance_km = distance_km,
      first_fair_id = fair_id,
      first_parent_fair_id = parent_fair_id,
      first_venue_seq = venue_seq,
      first_fair_name = Fair_name,
      first_fair_city = City,
      first_fair_venue = venue,
      exposure_status = case_when(
        first_exposure_year < treated_event_year_min ~ "always_treated_pre_1840",
        first_exposure_year >= treated_event_year_min &
          first_exposure_year <= treated_event_year_max ~ "treated",
        first_exposure_year > treated_event_year_max &
          first_exposure_year <= classification_year_max ~ "future_treated_after_1910",
        TRUE ~ "outside_classification_window"
      )
    )

  exposed_units <- first_exposure %>% distinct(target_unit_id)
  never_units <- target_dt %>%
    anti_join(exposed_units, by = "target_unit_id") %>%
    mutate(unit_id = target_unit_id, exposure_status = "never_treated")

  list(
    distance_audit = distance_audit,
    first_exposure = first_exposure,
    never_units = never_units,
    always_units = first_exposure %>% filter(exposure_status == "always_treated_pre_1840"),
    future_units = first_exposure %>% filter(exposure_status == "future_treated_after_1910")
  )
}

run_event_study <- function(data, outcome, bin_label, window = event_window, cores = 4) {
  data_es <- data %>%
    select(unit_num, unit_id, decade, g, all_of(outcome)) %>%
    rename(y = all_of(outcome)) %>%
    mutate(
      unit_num = as.numeric(unit_num),
      decade = as.numeric(decade),
      g = as.numeric(g),
      y = as.numeric(y)
    )

  all_periods <- sort(unique(data_es$decade))
  complete_units <- data_es %>%
    group_by(unit_num) %>%
    summarise(
      n_periods = n_distinct(decade),
      n_valid_y = sum(!is.na(y) & is.finite(y)),
      .groups = "drop"
    ) %>%
    filter(n_periods == length(all_periods), n_valid_y == length(all_periods)) %>%
    pull(unit_num)

  data_es <- data_es %>%
    filter(unit_num %in% complete_units, !is.na(y), is.finite(y))

  if (n_distinct(data_es$g[data_es$g > 0]) == 0L) {
    return(list(
      ok = FALSE,
      outcome = outcome,
      distance_bin_km = bin_label,
      n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - length(complete_units),
      error = "No treated cohorts in estimation sample."
    ))
  }

  if (n_distinct(data_es$unit_num[data_es$g == 0]) == 0L) {
    return(list(
      ok = FALSE,
      outcome = outcome,
      distance_bin_km = bin_label,
      n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - length(complete_units),
      error = "No never-treated control units in estimation sample."
    ))
  }

  if (n_distinct(data_es$y, na.rm = TRUE) < 2L) {
    return(list(
      ok = FALSE,
      outcome = outcome,
      distance_bin_km = bin_label,
      n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - length(complete_units),
      error = "Outcome has insufficient variation."
    ))
  }

  tryCatch(
    {
      out <- did::att_gt(
        yname = "y",
        tname = "decade",
        idname = "unit_num",
        gname = "g",
        data = data_es,
        control_group = control_group_name,
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
        distance_bin_km = bin_label,
        out = out,
        es = es,
        simple = simple,
        n_rows = nrow(data_es),
        n_units = n_distinct(data_es$unit_num),
        n_treated_units = n_distinct(data_es$unit_num[data_es$g > 0]),
        n_control_units = n_distinct(data_es$unit_num[data_es$g == 0]),
        n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
        n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - length(complete_units),
        min_decade = min(data_es$decade, na.rm = TRUE),
        max_decade = max(data_es$decade, na.rm = TRUE),
        error = NA_character_
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        outcome = outcome,
        distance_bin_km = bin_label,
        n_rows = nrow(data_es),
        n_units = n_distinct(data_es$unit_num),
        n_treated_units = n_distinct(data_es$unit_num[data_es$g > 0]),
        n_control_units = n_distinct(data_es$unit_num[data_es$g == 0]),
        n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
        n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - length(complete_units),
        min_decade = min(data_es$decade, na.rm = TRUE),
        max_decade = max(data_es$decade, na.rm = TRUE),
        error = conditionMessage(e)
      )
    }
  )
}

###############################################################################
# Load panel and treatment geography
###############################################################################

message("Reading panel...")
panel_year <- fread(panel_file, na.strings = c("", "NA")) %>%
  as_tibble() %>%
  mutate(
    unit_id = as.character(unit_id),
    target_unit_id = as.character(target_unit_id),
    year = as.integer(year)
  ) %>%
  filter(
    year >= panel_year_min,
    year <= panel_year_max,
    target_unit_id != greater_london_id
  )

eligible_units <- panel_year %>%
  filter(year >= treated_event_year_min, year <= panel_year_max) %>%
  group_by(target_unit_id) %>%
  summarise(has_any_population = any(!is.na(population)), .groups = "drop") %>%
  filter(has_any_population)

panel_year <- panel_year %>%
  semi_join(eligible_units, by = "target_unit_id")

message("Building harmonized urban-unit geometries...")
targets_sf <- build_target_geometries() %>%
  semi_join(eligible_units, by = "target_unit_id")

if (any(targets_sf$target_unit_id == greater_london_id) ||
    any(panel_year$target_unit_id == greater_london_id)) {
  stop("Greater London is present after the exclusion step.")
}

###############################################################################
# Build venue-distance treatment assignment
###############################################################################

message("Building conservative venue-distance treatment assignment...")
venue_data <- load_conservative_venues()
venues <- venue_data$venues
venue_quality_audit <- venue_data$audit
exposure <- build_distance_exposure(targets_sf, venues)

first_exposure <- exposure$first_exposure
never_units <- exposure$never_units
always_units <- exposure$always_units
future_units <- exposure$future_units
distance_audit <- exposure$distance_audit

write_csv(venue_quality_audit, file.path(results_dir, "venue_quality_audit.csv"))
write_csv(distance_audit, file.path(results_dir, "venue_distance_match_audit_all_bins.csv"))
write_csv(first_exposure, file.path(results_dir, "first_exposure_all_bins.csv"))
write_csv(never_units, file.path(results_dir, "never_treated_units.csv"))
write_csv(always_units, file.path(results_dir, "always_treated_pre_1840_units.csv"))
write_csv(future_units, file.path(results_dir, "future_treated_after_1910_units.csv"))

###############################################################################
# Aggregate annual outcome panel to decades
###############################################################################

message("Aggregating annual panel to decades...")
panel_decade_base <- panel_year %>%
  mutate(decade = standard_decade(year)) %>%
  group_by(
    unit_type,
    unit_id,
    GEOID,
    lau_id,
    city_geonameid,
    target_unit_id,
    target_area_type,
    target_boundary_id,
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
    inventors_per_100k_pop = if_else(
      population > 0,
      100000 * n_inventors / population,
      NA_real_
    ),
    stem_per_100k_pop = if_else(
      population > 0,
      100000 * n_stem / population,
      NA_real_
    )
  )

balance <- check_balanced_panel(panel_decade_base, unit_id, decade)

###############################################################################
# Run event studies by distance bin
###############################################################################

root_sample_summary <- list()

for (bin_label in bin_labels) {
  message("Running bin ", bin_label, " km...")
  bin_dir <- file.path(results_dir, bin_dirs[[bin_label]])
  dir.create(bin_dir, recursive = TRUE, showWarnings = FALSE)

  treated_units <- first_exposure %>%
    filter(
      exposure_status == "treated",
      distance_bin_km == bin_label,
      first_exposure_year >= treated_event_year_min,
      first_exposure_year <= treated_event_year_max
    ) %>%
    transmute(
      unit_id,
      target_unit_id,
      event_year = first_exposure_year,
      g = first_exposure_decade,
      distance_bin_km,
      first_distance_km,
      first_fair_id,
      first_parent_fair_id,
      first_venue_seq,
      first_fair_name,
      first_fair_city,
      first_fair_venue
    )

  controls <- never_units %>%
    transmute(unit_id, target_unit_id, g = 0L)

  analysis_units <- bind_rows(
    treated_units %>% select(unit_id, target_unit_id, g),
    controls
  )

  panel_decade <- panel_decade_base %>%
    semi_join(analysis_units, by = c("unit_id", "target_unit_id")) %>%
    left_join(analysis_units, by = c("unit_id", "target_unit_id")) %>%
    mutate(unit_num = as.integer(factor(unit_id)))

  if (any(panel_decade$target_unit_id == greater_london_id)) {
    stop("Greater London found in estimation panel for bin ", bin_label)
  }

  event_distribution <- bind_rows(
    panel_decade %>%
      distinct(unit_id, g) %>%
      count(g, name = "n_units") %>%
      mutate(distance_bin_km = bin_label, cohort = as.character(g)) %>%
      select(distance_bin_km, cohort, n_units),
    tibble(
      distance_bin_km = bin_label,
      cohort = "always_treated_pre_1840_excluded",
      n_units = nrow(always_units)
    ),
    tibble(
      distance_bin_km = bin_label,
      cohort = "future_treated_after_1910_excluded",
      n_units = nrow(future_units)
    )
  )

  sample_summary <- panel_decade %>%
    summarise(
      distance_bin_km = bin_label,
      n_rows = n(),
      n_units = n_distinct(unit_id),
      n_periods = n_distinct(decade),
      min_decade = min(decade, na.rm = TRUE),
      max_decade = max(decade, na.rm = TRUE),
      is_balanced = balance$is_balanced,
      min_periods_per_unit = balance$min_periods_per_unit,
      max_periods_per_unit = balance$max_periods_per_unit,
      n_treated_units = n_distinct(unit_id[g > 0]),
      n_control_units = n_distinct(unit_id[g == 0]),
      n_always_treated_excluded = nrow(always_units),
      n_future_treated_after_1910_excluded = nrow(future_units),
      missing_inventors_per_100k_pop = sum(is.na(inventors_per_100k_pop)),
      missing_stem_per_100k_pop = sum(is.na(stem_per_100k_pop)),
      total_inventors = sum(n_inventors, na.rm = TRUE),
      total_stem = sum(n_stem, na.rm = TRUE),
      .groups = "drop"
    )

  model_results <- list()
  for (outcome in outcomes) {
    message("  outcome: ", outcome)
    model_results[[outcome]] <- run_event_study(
      data = panel_decade,
      outcome = outcome,
      bin_label = bin_label,
      window = event_window,
      cores = 4
    )
  }

  successful_models <- keep(model_results, "ok")

  model_status <- imap_dfr(
    model_results,
    ~ tibble(
      outcome = .x$outcome,
      distance_bin_km = .x$distance_bin_km,
      control_group = control_group_name,
      ok = .x$ok,
      n_rows = value_or(.x$n_rows, NA_integer_),
      n_units = value_or(.x$n_units, NA_integer_),
      n_treated_units = value_or(.x$n_treated_units, NA_integer_),
      n_control_units = value_or(.x$n_control_units, NA_integer_),
      n_treated_cohorts = value_or(.x$n_treated_cohorts, NA_integer_),
      n_units_dropped_for_incomplete_outcome = value_or(
        .x$n_units_dropped_for_incomplete_outcome,
        NA_integer_
      ),
      min_decade = value_or(.x$min_decade, NA_real_),
      max_decade = value_or(.x$max_decade, NA_real_),
      error = value_or(.x$error, NA_character_)
    )
  )

  if (length(successful_models) > 0L) {
    dynamic_att <- imap_dfr(
      successful_models,
      ~ extract_dynamic_att(.x$es, .x$outcome, .x$distance_bin_km)
    )
    simple_att <- imap_dfr(
      successful_models,
      ~ extract_simple_att(.x$simple, .x$outcome, .x$distance_bin_km)
    )
  } else {
    dynamic_att <- tibble()
    simple_att <- tibble()
  }

  write_csv(treated_units, file.path(bin_dir, "treatment_assignment.csv"))
  write_csv(
    distance_audit %>% filter(distance_bin_km == bin_label),
    file.path(bin_dir, "venue_distance_match_audit.csv")
  )
  write_csv(event_distribution, file.path(bin_dir, "event_distribution.csv"))
  write_csv(sample_summary, file.path(bin_dir, "sample_summary.csv"))
  write_csv(model_status, file.path(bin_dir, "model_status.csv"))
  write_csv(dynamic_att, file.path(bin_dir, "dynamic_att.csv"))
  write_csv(simple_att, file.path(bin_dir, "simple_att.csv"))

  if (nrow(dynamic_att) > 0L) {
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
        model$distance_bin_km,
        y_limits
      )

      ggsave(
        file.path(
          bin_dir,
          paste0("ES_", sanitize_filename(model$outcome), "_bin_", sanitize_filename(bin_label), "km.png")
        ),
        plot_es,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  }

  notes <- c(
    "World's fairs UK urban-unit venue-distance event study",
    "",
    paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Distance bin: ", bin_label, " km"),
    paste0("Panel: ", panel_file),
    paste0("Fairs: ", fairs_file),
    paste0("Control group: ", control_group_name),
    "Greater London excluded before treatment/control classification.",
    "Distance is polygon-to-venue; venues inside polygons have distance 0.",
    "Venue coordinates use conservative quality filter.",
    "Always-treated units are units first exposed before 1840.",
    "Future-treated units are units first exposed after 1910 and before or during 1961.",
    "Decade aggregation follows existing worlds-fairs event-study scripts.",
    paste0("Dynamic window: -", event_window, " to +", event_window, " years."),
    paste0("Treated units: ", sample_summary$n_treated_units),
    paste0("Never-treated control units: ", sample_summary$n_control_units),
    paste0("Always-treated units excluded: ", nrow(always_units)),
    paste0("Future-treated units after 1910 excluded: ", nrow(future_units)),
    paste0("Successful models: ", sum(model_status$ok), " / ", nrow(model_status))
  )
  writeLines(notes, file.path(bin_dir, "notes.txt"))

  root_sample_summary[[bin_label]] <- sample_summary
}

all_sample_summary <- bind_rows(root_sample_summary)
write_csv(all_sample_summary, file.path(results_dir, "sample_summary_all_bins.csv"))

all_event_distribution <- map_dfr(bin_labels, function(bin_label) {
  bin_file <- file.path(results_dir, bin_dirs[[bin_label]], "event_distribution.csv")
  read_csv(bin_file, show_col_types = FALSE)
})
write_csv(all_event_distribution, file.path(results_dir, "event_distribution_all_bins.csv"))

root_notes <- c(
  "World's fairs UK urban-unit venue-distance event studies, excluding London",
  "",
  paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("TALENT_DETS_DATA_DIR: ", TALENT_DETS_DATA_DIR),
  paste0("Panel: ", panel_file),
  paste0("Fairs: ", fairs_file),
  paste0("Results directory: ", results_dir),
  "Distance bins: 0-2, 2-4, 4-6, 6-8, 8-10 km.",
  paste0("Exposure classification window: ", classification_year_min, "-", classification_year_max, "."),
  paste0("Included treated event window: ", treated_event_year_min, "-", treated_event_year_max, "."),
  "Control group is strictly never treated within 10 km over 1790-1961.",
  "Greater London is excluded before treatment and control classification.",
  "Venue coordinates with low-quality automated geocoding notes are excluded.",
  paste0("Elapsed minutes: ", round(difftime(Sys.time(), initial_time, units = "mins"), 1))
)
writeLines(root_notes, file.path(results_dir, "notes.txt"))

message("Saved results in: ", results_dir)
message(
  "Done. Elapsed: ",
  round(difftime(Sys.time(), initial_time, units = "mins"), 1),
  " min"
)
