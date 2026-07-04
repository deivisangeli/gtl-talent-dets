###############################################################################
# Project: GTL Talent Determinants
# Goal: Pooled UK historical urban-unit + US county event studies using
#       distance to the first Crystal Palace world's fair venue.
#
# Treatment:
#   - Exposure to The Great Exhibition of 1851 at Crystal Palace, London,
#     within distance bins:
#       0-2, 2-4, 4-6, 6-8, 8-10, 10-12, 12-14, 14-16, 16-18, 18-20 km.
#   - Controls are units never exposed within 20 km to this event.
#   - Units first exposed before 1840 are always-treated and excluded.
#   - Units first exposed after 1910 are future-treated and excluded.
#   - Greater London is included as an outcome unit using the Nomis/ONS 1921
#     boundary definition selected by >=50% overlap with 1911 Greater London.
#
# Run from analysis/ or repo root:
#   Rscript analysis/world_fairs/worlds_fairs_venue_distance_crystal_palace_1851_0_20km.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(did)
  library(sf)
  library(tigris)
})

initial_time <- Sys.time()
options(timeout = 1000, tigris_use_cache = TRUE)
sf_use_s2(FALSE)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
  if (basename(repo_root) == "world_fairs" && basename(dirname(repo_root)) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, "..", ".."), winslash = "/", mustWork = TRUE)
  }
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
    TALENT_DETS_DATA_DIR <- normalizePath(user_data_dir, winslash = "/", mustWork = TRUE)
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
  "worlds_fair",
  "worlds_fairs_uk_us_venue_distance_crystal_palace_1851_event_studies_0_20km"
)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
panel_file <- file.path(
  data_processed,
  "uk_historical_urban_units_us_county_inventor_panel_1800_1960_nhgis_us.csv"
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
greater_london_crosswalk_file <- file.path(
  gbr_dir,
  "raw",
  "arcgis_english_admin_boundaries_1911",
  "greater_london_1911_to_nomis_1921_crosswalk.csv"
)

required_files <- c(panel_file, fairs_file, boundary_gpkg, greater_london_crosswalk_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

greater_london_id <- "GBR_HIST_URBAN_GREATER_LONDON"
target_types <- c("Urban District", "Municipal Borough", "County Borough")
crystal_palace_1851_fair_id <- 23L
crystal_palace_1851_latitude <- 51.50241
crystal_palace_1851_longitude <- -0.17049
crystal_palace_1851_coordinate_source <- "Geograph/Read the Plaque - Great Exhibition site marker, Hyde Park"
crystal_palace_1851_coordinate_note <- paste(
  "Manual override for the 1851 Hyde Park Crystal Palace site.",
  "The consolidated fair file geocodes the name Crystal Palace to the later Sydenham site;",
  "this specification uses the Hyde Park Great Exhibition site marker instead."
)
max_treatment_distance_km <- 20
max_treatment_distance_m <- max_treatment_distance_km * 1000
aggregate_bin_label <- "0-20"
bin_breaks <- c(-1e-9, seq(2, max_treatment_distance_km, by = 2))
bin_labels <- paste(seq(0, max_treatment_distance_km - 2, by = 2),
                    seq(2, max_treatment_distance_km, by = 2),
                    sep = "-")
bin_dirs <- c(
  "0-2" = "bin_0_2km",
  "2-4" = "bin_2_4km",
  "4-6" = "bin_4_6km",
  "6-8" = "bin_6_8km",
  "8-10" = "bin_8_10km",
  "10-12" = "bin_10_12km",
  "12-14" = "bin_12_14km",
  "14-16" = "bin_14_16km",
  "16-18" = "bin_16_18km",
  "18-20" = "bin_18_20km",
  "0-20" = "bin_0_20km"
)
analysis_bin_labels <- c(bin_labels, aggregate_bin_label)
classification_year_min <- 1790L
classification_year_max <- 1961L
treated_event_year_min <- 1840L
treated_event_year_max <- 1910L
panel_year_min <- 1800L
panel_year_max <- 1960L
event_window <- 50L
control_group_name <- "nevertreated"

filter_outcomes <- function(default_outcomes) {
  env <- Sys.getenv("WORLD_FAIRS_OUTCOMES", unset = "")
  if (env == "") return(default_outcomes)

  requested <- trimws(strsplit(env, ",", fixed = TRUE)[[1]])
  requested <- requested[requested != ""]
  bad <- setdiff(requested, default_outcomes)
  if (length(bad) > 0L) {
    stop("Unknown WORLD_FAIRS_OUTCOMES: ", paste(bad, collapse = ", "))
  }
  requested
}

outcomes <- filter_outcomes(c(
  "inventors_per_100k_pop",
  "stem_per_100k_pop",
  "n_inventors",
  "log1p_n_inventors",
  "n_stem",
  "log1p_n_stem",
  "population",
  "log_population"
))

###############################################################################
# Helpers
###############################################################################

value_or <- function(x, default) {
  if (is.null(x)) default else x
}

mean_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0L) return(NA_character_)
  as.character(x[[1L]])
}

standard_decade <- function(year) {
  as.integer(floor(year / 10) * 10)
}

sanitize_filename <- function(x) {
  str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

pad_geoid <- function(x) {
  x_chr <- as.character(x)
  x_chr <- if_else(
    is.na(x_chr) | x_chr == "",
    NA_character_,
    str_pad(str_replace(x_chr, "\\.0$", ""), 5, pad = "0")
  )
  x_chr
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
  if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
  c(-1.1 * max_abs, 1.1 * max_abs)
}

n_distinct_nonmissing <- function(x) {
  length(unique(x[!is.na(x) & x != ""]))
}

summarise_effective_sample <- function(data_es, outcome, bin_label) {
  event_id <- coalesce(
    as.character(data_es$first_parent_fair_id),
    as.character(data_es$first_fair_id)
  )

  tibble(
    outcome = outcome,
    distance_bin_km = bin_label,
    n_events = n_distinct_nonmissing(event_id[data_es$g > 0]),
    n_treated_gbr = n_distinct(data_es$unit_id[data_es$g > 0 & data_es$iso3 == "GBR"]),
    n_treated_usa = n_distinct(data_es$unit_id[data_es$g > 0 & data_es$iso3 == "USA"]),
    n_control_gbr = n_distinct(data_es$unit_id[data_es$g == 0 & data_es$iso3 == "GBR"]),
    n_control_usa = n_distinct(data_es$unit_id[data_es$g == 0 & data_es$iso3 == "USA"])
  )
}

format_sample_annotation <- function(sample_summary) {
  paste(
    paste0("Events: ", sample_summary$n_events),
    paste0("Treated UK: ", sample_summary$n_treated_gbr),
    paste0("Treated US: ", sample_summary$n_treated_usa),
    paste0("Control UK: ", sample_summary$n_control_gbr),
    paste0("Control US: ", sample_summary$n_control_usa),
    sep = "\n"
  )
}

plot_dynamic_event_study <- function(es, outcome, bin_label, y_limits, sample_annotation) {
  did::ggdid(es) +
    annotate(
      "label",
      x = Inf,
      y = Inf,
      label = sample_annotation,
      hjust = 1.05,
      vjust = 1.05,
      size = 3,
      label.size = 0.2,
      alpha = 0.9
    ) +
    labs(
      x = "Relative time (years)",
      y = "Effect",
      title = str_wrap(
        paste(
          "World's fairs pooled UK+US venue-distance event study",
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

build_uk_target_geometries <- function() {
  districts_1921 <- st_read(boundary_gpkg, layer = "districts_1921", quiet = TRUE) %>%
    st_transform(27700) %>%
    st_make_valid()

  greater_london_boundary_ids <- read_csv(
    greater_london_crosswalk_file,
    show_col_types = FALSE,
    col_types = cols(.default = col_guess(), nomis_1921_id = col_character())
  ) %>%
    filter(in_greater_london_1911_main == TRUE) %>%
    pull(nomis_1921_id) %>%
    unique()

  if (length(greater_london_boundary_ids) == 0L) {
    stop("No Greater London 1911 main units found in crosswalk.")
  }

  greater_london_components <- districts_1921[
    districts_1921$boundary_id %in% greater_london_boundary_ids,
  ]

  missing_london_ids <- setdiff(greater_london_boundary_ids, greater_london_components$boundary_id)
  if (length(missing_london_ids) > 0L) {
    stop(
      "Greater London crosswalk ids missing from districts_1921:\n",
      paste(missing_london_ids, collapse = "\n")
    )
  }

  greater_london_sf <- st_sf(
    unit_id = greater_london_id,
    target_unit_id = greater_london_id,
    target_unit_name = "Greater London",
    target_area_type = "Greater London",
    target_boundary_id = greater_london_id,
    geo_country_iso3 = "GBR",
    GEOID = NA_character_,
    geometry = st_sfc(st_union(greater_london_components), crs = 27700)
  )

  base_targets <- districts_1921[districts_1921$boundary_type %in% target_types, ]
  base_targets <- base_targets[!(base_targets$boundary_id %in% greater_london_boundary_ids), ]

  base_targets_sf <- st_sf(
    unit_id = paste0("GBR_HIST_URBAN_", base_targets$boundary_id),
    target_unit_id = paste0("GBR_HIST_URBAN_", base_targets$boundary_id),
    target_unit_name = base_targets$boundary_name,
    target_area_type = base_targets$boundary_type,
    target_boundary_id = base_targets$boundary_id,
    geo_country_iso3 = "GBR",
    GEOID = NA_character_,
    geometry = st_geometry(base_targets)
  )

  bind_rows(base_targets_sf, greater_london_sf) %>%
    st_make_valid()
}

build_us_target_geometries <- function(panel_year) {
  us_units <- panel_year %>%
    filter(iso3 == "USA") %>%
    distinct(unit_id, GEOID, place_name) %>%
    mutate(GEOID = pad_geoid(GEOID)) %>%
    filter(!is.na(GEOID))

  counties_poly <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
    st_transform(5070) %>%
    select(GEOID, NAMELSAD, STATEFP, geometry) %>%
    filter(as.integer(STATEFP) <= 56) %>%
    mutate(GEOID = as.character(GEOID)) %>%
    inner_join(us_units, by = "GEOID")

  missing_us <- anti_join(us_units, st_drop_geometry(counties_poly), by = "GEOID")
  if (nrow(missing_us) > 0L) {
    stop(
      "US panel counties without tigris geometry:\n",
      paste(head(missing_us$GEOID, 20), collapse = ", ")
    )
  }

  counties_poly %>%
    transmute(
      unit_id,
      target_unit_id = unit_id,
      target_unit_name = NAMELSAD,
      target_area_type = "US County",
      target_boundary_id = GEOID,
      geo_country_iso3 = "USA",
      GEOID,
      geometry
    ) %>%
    st_make_valid()
}

load_conservative_venues <- function() {
  fairs <- fread(fairs_file, na.strings = c("", "NA")) %>%
    as_tibble()

  if (!"parent_fair_id" %in% names(fairs)) fairs$parent_fair_id <- fairs$fair_id
  if (!"venue_seq" %in% names(fairs)) fairs$venue_seq <- 1L

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
      host_matched_country_iso3 %in% c("GBR", "USA")
    ) %>%
    mutate(
      has_venue_coordinates = !is.na(venue_longitude) & !is.na(venue_latitude),
      excluded_no_venue_coordinates = !has_venue_coordinates,
      excluded_low_quality_venue_coordinates =
        has_venue_coordinates &
          str_detect(
            coalesce(venue_coordinates_note, ""),
            fixed("automated geocoding returned no reliable coordinate")
          ),
      venue_used_conservative =
        has_venue_coordinates & !excluded_low_quality_venue_coordinates
    )

  venue_audit <- venue_audit %>%
    filter(fair_id == crystal_palace_1851_fair_id) %>%
    mutate(
      venue_longitude = crystal_palace_1851_longitude,
      venue_latitude = crystal_palace_1851_latitude,
      venue_coordinates_source_title = crystal_palace_1851_coordinate_source,
      venue_coordinates_note = crystal_palace_1851_coordinate_note,
      has_venue_coordinates = TRUE,
      excluded_no_venue_coordinates = FALSE,
      excluded_low_quality_venue_coordinates = FALSE,
      venue_used_conservative = TRUE
    )

  if (nrow(venue_audit) != 1L) {
    stop(
      "Expected exactly one Crystal Palace 1851 venue row for fair_id ",
      crystal_palace_1851_fair_id,
      "; found ",
      nrow(venue_audit)
    )
  }
  if (!isTRUE(venue_audit$venue_used_conservative[[1L]])) {
    stop("Crystal Palace 1851 venue row is not usable under conservative coordinate rules.")
  }

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
      host_matched_country_iso3,
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

build_distance_exposure_one_country <- function(targets_sf, venues_country) {
  target_dt <- as_tibble(st_drop_geometry(targets_sf)) %>%
    select(
      unit_id,
      target_unit_id,
      target_unit_name,
      target_area_type,
      target_boundary_id,
      geo_country_iso3,
      GEOID
    )

  if (nrow(venues_country) == 0L) {
    return(list(
      distance_audit = tibble(),
      first_exposure = tibble(),
      never_units = target_dt %>% mutate(exposure_status = "never_treated"),
      always_units = tibble(),
      future_units = tibble()
    ))
  }

  venue_points <- venues_country %>%
    st_as_sf(coords = c("venue_longitude", "venue_latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(st_crs(targets_sf))

  distance_matrix <- matrix(
    as.numeric(st_distance(targets_sf, venue_points)),
    nrow = nrow(targets_sf),
    ncol = nrow(venues_country)
  )

  hit_index <- which(distance_matrix <= max_treatment_distance_m, arr.ind = TRUE)
  if (nrow(hit_index) == 0L) {
    return(list(
      distance_audit = tibble(),
      first_exposure = tibble(),
      never_units = target_dt %>% mutate(exposure_status = "never_treated"),
      always_units = tibble(),
      future_units = tibble()
    ))
  }

  distance_audit <- tibble(
    target_row = hit_index[, 1],
    venue_row = hit_index[, 2],
    distance_km = distance_matrix[hit_index] / 1000
  ) %>%
    bind_cols(target_dt[.$target_row, ]) %>%
    bind_cols(venues_country[.$venue_row, ]) %>%
    mutate(
      distance_bin_km = cut(
        distance_km,
        breaks = bin_breaks,
        labels = bin_labels,
        include.lowest = TRUE,
        right = TRUE
      )
    ) %>%
    arrange(geo_country_iso3, unit_id, year_start, distance_km, fair_id)

  first_exposure <- distance_audit %>%
    group_by(unit_id) %>%
    slice(1L) %>%
    ungroup() %>%
    transmute(
      unit_id,
      target_unit_id,
      target_unit_name,
      target_area_type,
      target_boundary_id,
      geo_country_iso3,
      GEOID,
      first_exposure_year = year_start,
      first_exposure_decade = standard_decade(first_exposure_year),
      distance_bin_km = as.character(distance_bin_km),
      first_distance_km = distance_km,
      first_fair_id = fair_id,
      first_parent_fair_id = parent_fair_id,
      first_venue_seq = venue_seq,
      first_fair_name = Fair_name,
      first_fair_city = City,
      first_fair_country = Country,
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

  exposed_units <- first_exposure %>% distinct(unit_id)
  never_units <- target_dt %>%
    anti_join(exposed_units, by = "unit_id") %>%
    mutate(exposure_status = "never_treated")

  list(
    distance_audit = distance_audit,
    first_exposure = first_exposure,
    never_units = never_units,
    always_units = first_exposure %>% filter(exposure_status == "always_treated_pre_1840"),
    future_units = first_exposure %>% filter(exposure_status == "future_treated_after_1910")
  )
}

build_distance_exposure <- function(uk_targets, us_targets, venues) {
  uk_exposure <- build_distance_exposure_one_country(
    uk_targets,
    venues %>% filter(host_matched_country_iso3 == "GBR")
  )
  us_exposure <- build_distance_exposure_one_country(
    us_targets,
    venues %>% filter(host_matched_country_iso3 == "USA")
  )

  list(
    distance_audit = bind_rows(uk_exposure$distance_audit, us_exposure$distance_audit),
    first_exposure = bind_rows(uk_exposure$first_exposure, us_exposure$first_exposure),
    never_units = bind_rows(uk_exposure$never_units, us_exposure$never_units),
    always_units = bind_rows(uk_exposure$always_units, us_exposure$always_units),
    future_units = bind_rows(uk_exposure$future_units, us_exposure$future_units)
  )
}

run_event_study <- function(data, outcome, bin_label, window = event_window, cores = 4) {
  data_es <- data %>%
    select(
      unit_num,
      unit_id,
      iso3,
      decade,
      g,
      first_parent_fair_id,
      first_fair_id,
      all_of(outcome)
    ) %>%
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

  effective_sample_summary <- summarise_effective_sample(data_es, outcome, bin_label)

  if (n_distinct(data_es$g[data_es$g > 0]) == 0L) {
    return(list(
      ok = FALSE,
      outcome = outcome,
      distance_bin_km = bin_label,
      effective_sample_summary = effective_sample_summary,
      n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - length(complete_units),
      error = "No treated cohorts in estimation sample."
    ))
  }

  if (n_distinct(data_es$unit_num[data_es$g == 0]) == 0L) {
    return(list(
      ok = FALSE,
      outcome = outcome,
      distance_bin_km = bin_label,
      effective_sample_summary = effective_sample_summary,
      n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - length(complete_units),
      error = "No never-treated control units in estimation sample."
    ))
  }

  if (n_distinct(data_es$y, na.rm = TRUE) < 2L) {
    return(list(
      ok = FALSE,
      outcome = outcome,
      distance_bin_km = bin_label,
      effective_sample_summary = effective_sample_summary,
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
        effective_sample_summary = effective_sample_summary,
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
        effective_sample_summary = effective_sample_summary,
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

message("Reading combined UK+US panel...")
panel_year <- fread(panel_file, na.strings = c("", "NA")) %>%
  as_tibble() %>%
  mutate(
    unit_id = as.character(unit_id),
    GEOID = pad_geoid(GEOID),
    target_unit_id = if_else(
      iso3 == "USA" | is.na(target_unit_id) | target_unit_id == "",
      unit_id,
      as.character(target_unit_id)
    ),
    target_area_type = if_else(
      iso3 == "USA" | is.na(target_area_type) | target_area_type == "",
      "US County",
      as.character(target_area_type)
    ),
    target_boundary_id = if_else(
      iso3 == "USA" | is.na(target_boundary_id) | target_boundary_id == "",
      coalesce(GEOID, unit_id),
      as.character(target_boundary_id)
    ),
    year = as.integer(year)
  ) %>%
  filter(
    iso3 %in% c("GBR", "USA"),
    year >= panel_year_min,
    year <= panel_year_max
  )

eligible_units <- panel_year %>%
  filter(year >= treated_event_year_min, year <= panel_year_max) %>%
  group_by(unit_id, iso3) %>%
  summarise(has_any_population = any(!is.na(population)), .groups = "drop") %>%
  filter(has_any_population)

panel_year <- panel_year %>%
  semi_join(eligible_units, by = c("unit_id", "iso3"))

message("Building UK historical urban-unit geometries...")
uk_targets <- build_uk_target_geometries() %>%
  semi_join(eligible_units %>% filter(iso3 == "GBR"), by = "unit_id")

message("Building US county geometries...")
us_targets <- build_us_target_geometries(panel_year) %>%
  semi_join(eligible_units %>% filter(iso3 == "USA"), by = "unit_id")

###############################################################################
# Build venue-distance treatment assignment
###############################################################################

message("Building conservative venue-distance treatment assignment...")
venue_data <- load_conservative_venues()
venues <- venue_data$venues
venue_quality_audit <- venue_data$audit
exposure <- build_distance_exposure(uk_targets, us_targets, venues)

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
    log_population = if_else(population > 0, log(population), NA_real_),
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

###############################################################################
# Run event studies by distance bin
###############################################################################

root_sample_summary <- list()
root_country_summary <- list()

for (bin_label in analysis_bin_labels) {
  message("Running bin ", bin_label, " km...")
  bin_dir <- file.path(results_dir, bin_dirs[[bin_label]])
  dir.create(bin_dir, recursive = TRUE, showWarnings = FALSE)

  treated_units <- first_exposure %>%
    filter(
      exposure_status == "treated",
      if (bin_label == aggregate_bin_label) {
        distance_bin_km %in% bin_labels
      } else {
        distance_bin_km == bin_label
      },
      first_exposure_year >= treated_event_year_min,
      first_exposure_year <= treated_event_year_max
    ) %>%
    transmute(
      unit_id,
      target_unit_id,
      geo_country_iso3,
      event_year = first_exposure_year,
      g = first_exposure_decade,
      distance_bin_km = bin_label,
      source_distance_bin_km = distance_bin_km,
      first_distance_km,
      first_fair_id = as.character(first_fair_id),
      first_parent_fair_id = as.character(first_parent_fair_id),
      first_venue_seq,
      first_fair_name,
      first_fair_city,
      first_fair_country,
      first_fair_venue
    )

  controls <- never_units %>%
    transmute(
      unit_id,
      target_unit_id,
      geo_country_iso3,
      g = 0L,
      first_parent_fair_id = NA_character_,
      first_fair_id = NA_character_
    )

  analysis_units <- bind_rows(
    treated_units %>%
      select(unit_id, target_unit_id, geo_country_iso3, g, first_parent_fair_id, first_fair_id),
    controls
  )

  panel_decade <- panel_decade_base %>%
    semi_join(analysis_units, by = c("unit_id", "target_unit_id")) %>%
    left_join(analysis_units, by = c("unit_id", "target_unit_id")) %>%
    mutate(unit_num = as.integer(factor(unit_id)))

  balance <- check_balanced_panel(panel_decade, unit_id, decade)

  event_distribution <- bind_rows(
    panel_decade %>%
      distinct(unit_id, iso3, g) %>%
      count(iso3, g, name = "n_units") %>%
      mutate(distance_bin_km = bin_label, cohort = as.character(g)) %>%
      select(distance_bin_km, iso3, cohort, n_units),
    always_units %>%
      count(geo_country_iso3, name = "n_units") %>%
      transmute(
        distance_bin_km = bin_label,
        iso3 = geo_country_iso3,
        cohort = "always_treated_pre_1840_excluded",
        n_units
      ),
    future_units %>%
      count(geo_country_iso3, name = "n_units") %>%
      transmute(
        distance_bin_km = bin_label,
        iso3 = geo_country_iso3,
        cohort = "future_treated_after_1910_excluded",
        n_units
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

  country_summary <- panel_decade %>%
    group_by(iso3) %>%
    summarise(
      distance_bin_km = bin_label,
      n_rows = n(),
      n_units = n_distinct(unit_id),
      n_treated_units = n_distinct(unit_id[g > 0]),
      n_control_units = n_distinct(unit_id[g == 0]),
      total_inventors = sum(n_inventors, na.rm = TRUE),
      total_stem = sum(n_stem, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    relocate(distance_bin_km)

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
      n_events = value_or(.x$effective_sample_summary$n_events, NA_integer_),
      n_treated_gbr = value_or(.x$effective_sample_summary$n_treated_gbr, NA_integer_),
      n_treated_usa = value_or(.x$effective_sample_summary$n_treated_usa, NA_integer_),
      n_control_gbr = value_or(.x$effective_sample_summary$n_control_gbr, NA_integer_),
      n_control_usa = value_or(.x$effective_sample_summary$n_control_usa, NA_integer_),
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

  effective_sample_summary <- imap_dfr(
    model_results,
    ~ .x$effective_sample_summary
  )
  distance_audit_bin <- if (bin_label == aggregate_bin_label) {
    distance_audit %>%
      filter(distance_bin_km %in% bin_labels) %>%
      mutate(analysis_distance_bin_km = bin_label)
  } else {
    distance_audit %>%
      filter(distance_bin_km == bin_label) %>%
      mutate(analysis_distance_bin_km = bin_label)
  }

  write_csv(treated_units, file.path(bin_dir, "treatment_assignment.csv"))
  write_csv(
    distance_audit_bin,
    file.path(bin_dir, "venue_distance_match_audit.csv")
  )
  write_csv(event_distribution, file.path(bin_dir, "event_distribution.csv"))
  write_csv(sample_summary, file.path(bin_dir, "sample_summary.csv"))
  write_csv(country_summary, file.path(bin_dir, "sample_summary_by_country.csv"))
  write_csv(effective_sample_summary, file.path(bin_dir, "effective_sample_summary_by_outcome.csv"))
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
        y_limits,
        format_sample_annotation(model$effective_sample_summary)
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
    "World's fairs pooled UK+US venue-distance event study: Crystal Palace 1851 only",
    "",
    paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Distance bin: ", bin_label, " km"),
    paste0("Panel: ", panel_file),
    paste0("Fairs: ", fairs_file),
    paste0("Selected fair_id: ", crystal_palace_1851_fair_id),
    paste0(
      "Selected venue coordinates: ",
      crystal_palace_1851_latitude,
      ", ",
      crystal_palace_1851_longitude
    ),
    paste0("Selected venue coordinate source: ", crystal_palace_1851_coordinate_source),
    paste0("Control group: ", control_group_name),
    "Greater London included using Nomis/ONS 1921 districts selected by >=50% overlap with 1911 Greater London.",
    "The Crystal Palace 1851 venue is used to classify exposure for Greater London and nearby units.",
    "US counties use tigris 2020 cartographic-boundary counties.",
    "Distance is polygon-to-venue; venues inside polygons have distance 0.",
    "Venue coordinates use conservative quality filter.",
    paste0("Maximum exposure distance: ", max_treatment_distance_km, " km."),
    "Always-treated units are units first exposed before 1840.",
    "Future-treated units are units first exposed after 1910 and before or during 1961.",
    paste0("Dynamic window: -", event_window, " to +", event_window, " years."),
    paste0("Treated units: ", sample_summary$n_treated_units),
    paste0("Never-treated control units: ", sample_summary$n_control_units),
    paste0("Always-treated units excluded: ", nrow(always_units)),
    paste0("Future-treated units after 1910 excluded: ", nrow(future_units)),
    paste0("Successful models: ", sum(model_status$ok), " / ", nrow(model_status))
  )
  writeLines(notes, file.path(bin_dir, "notes.txt"))

  root_sample_summary[[bin_label]] <- sample_summary
  root_country_summary[[bin_label]] <- country_summary
}

all_sample_summary <- bind_rows(root_sample_summary)
write_csv(all_sample_summary, file.path(results_dir, "sample_summary_all_bins.csv"))

all_country_summary <- bind_rows(root_country_summary)
write_csv(all_country_summary, file.path(results_dir, "sample_summary_by_country_all_bins.csv"))

all_event_distribution <- map_dfr(analysis_bin_labels, function(bin_label) {
  bin_file <- file.path(results_dir, bin_dirs[[bin_label]], "event_distribution.csv")
  read_csv(bin_file, show_col_types = FALSE)
})
write_csv(all_event_distribution, file.path(results_dir, "event_distribution_all_bins.csv"))

root_notes <- c(
  "World's fairs pooled UK historical urban-unit + US county venue-distance event studies: Crystal Palace 1851 only",
  "",
  paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("TALENT_DETS_DATA_DIR: ", TALENT_DETS_DATA_DIR),
  paste0("Panel: ", panel_file),
  paste0("Fairs: ", fairs_file),
  paste0("Selected fair_id: ", crystal_palace_1851_fair_id),
  paste0(
    "Selected venue coordinates: ",
    crystal_palace_1851_latitude,
    ", ",
    crystal_palace_1851_longitude
  ),
  paste0("Selected venue coordinate source: ", crystal_palace_1851_coordinate_source),
  paste0("Results directory: ", results_dir),
  paste0(
    "Distance specifications: ",
    paste(bin_labels, collapse = ", "),
    ", and cumulative ",
    aggregate_bin_label,
    " km."
  ),
  paste0("Exposure classification window: ", classification_year_min, "-", classification_year_max, "."),
  paste0("Included treated event window: ", treated_event_year_min, "-", treated_event_year_max, "."),
  paste0(
    "Control group is strictly never treated within ",
    max_treatment_distance_km,
    " km of Crystal Palace 1851 over ",
    classification_year_min,
    "-",
    classification_year_max,
    "."
  ),
  "Greater London is included using Nomis/ONS 1921 districts selected by >=50% overlap with 1911 Greater London.",
  "US counties remain in the panel but no US event is selected in this Crystal Palace-only specification.",
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
