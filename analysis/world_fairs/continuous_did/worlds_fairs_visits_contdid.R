###############################################################################
# Project: GTL Talent Determinants
# Goal: Continuous-treatment event studies for world's fairs using contdid.
#
# Treatment dose:
#   - first_fair_visits / 100,000 for the first fair that exposes a unit.
#   - The dose is fixed across all periods, including pre-treatment periods, as
#     required by contdid.
#
# Specifications:
#   - hosted: first realized fair venue with visits inside the unit polygon.
#   - 0-10 km: first realized fair venue with visits within 10 km of the unit.
#
# Run from analysis/ or repo root:
#   Rscript analysis/world_fairs/continuous_did/worlds_fairs_visits_contdid.R
#
# Useful smoke-test env vars:
#   CONTDID_OUTCOMES=log1p_n_stem CONTDID_BITERS=25 Rscript ...
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(sf)
  library(tigris)
})

if (!requireNamespace("contdid", quietly = TRUE)) {
  stop(
    "Package 'contdid' is required. Install it with install.packages('contdid') ",
    "or remotes::install_github('bcallaway11/contdid')."
  )
}

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
    file.path(dirname(script_path), "..", "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
  if (basename(repo_root) == "continuous_did" &&
      basename(dirname(repo_root)) == "world_fairs" &&
      basename(dirname(dirname(repo_root))) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, "..", "..", ".."), winslash = "/", mustWork = TRUE)
  }
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
  "continuous_did",
  "worlds_fairs_uk_us_visits_contdid_with_london_events_1840_1910"
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
classification_year_min <- 1790L
classification_year_max <- 1961L
treated_event_year_min <- 1840L
treated_event_year_max <- 1910L
panel_year_min <- 1800L
panel_year_max <- 1960L
max_distance_m <- 10000
event_window <- as.integer(Sys.getenv("CONTDID_EVENT_WINDOW", unset = "50"))
biters <- as.integer(Sys.getenv("CONTDID_BITERS", unset = "1000"))
degree <- as.integer(Sys.getenv("CONTDID_DEGREE", unset = "1"))
num_knots <- as.integer(Sys.getenv("CONTDID_NUM_KNOTS", unset = "0"))
cl <- as.integer(Sys.getenv("CONTDID_CL", unset = "1"))
cband <- tolower(Sys.getenv("CONTDID_CBAND", unset = "false")) %in% c("1", "true", "yes", "y")
control_group_name <- "nevertreated"
dose_scale <- 100000
contdid_internal_dose_scale <- as.numeric(
  Sys.getenv("CONTDID_INTERNAL_DOSE_SCALE", unset = "100000000")
)

if (is.na(event_window) || event_window < 0L) stop("CONTDID_EVENT_WINDOW must be non-negative.")
if (event_window %% 10L != 0L) stop("CONTDID_EVENT_WINDOW must be a multiple of 10 for decennial data.")
if (is.na(biters) || biters < 1L) stop("CONTDID_BITERS must be positive.")
if (is.na(degree) || degree < 1L) stop("CONTDID_DEGREE must be positive.")
if (is.na(num_knots) || num_knots < 0L) stop("CONTDID_NUM_KNOTS must be non-negative.")
if (is.na(cl) || cl < 1L) stop("CONTDID_CL must be positive.")
if (is.na(contdid_internal_dose_scale) || contdid_internal_dose_scale <= 0) {
  stop("CONTDID_INTERNAL_DOSE_SCALE must be positive.")
}
event_window_periods <- as.integer(event_window / 10L)
acrt_rescale_to_100k_visits <- dose_scale / contdid_internal_dose_scale

default_outcomes <- c(
  "inventors_per_100k_pop",
  "stem_per_100k_pop",
  "n_inventors",
  "log1p_n_inventors",
  "n_stem",
  "log1p_n_stem",
  "population",
  "log_population"
)

outcomes <- {
  env <- Sys.getenv("CONTDID_OUTCOMES", unset = "")
  if (env == "") {
    default_outcomes
  } else {
    requested <- str_split(env, ",", simplify = FALSE)[[1]] %>%
      str_trim() %>%
      discard(~ .x == "")
    bad <- setdiff(requested, default_outcomes)
    if (length(bad) > 0L) stop("Unknown CONTDID_OUTCOMES: ", paste(bad, collapse = ", "))
    requested
  }
}

available_specs <- c("hosted", "0-10")
selected_specs <- {
  env <- Sys.getenv("CONTDID_SPECS", unset = "")
  if (env == "") {
    available_specs
  } else {
    requested <- str_split(env, ",", simplify = FALSE)[[1]] %>%
      str_trim() %>%
      discard(~ .x == "")
    bad <- setdiff(requested, available_specs)
    if (length(bad) > 0L) stop("Unknown CONTDID_SPECS: ", paste(bad, collapse = ", "))
    requested
  }
}

spec_dirs <- c("hosted" = "hosted", "0-10" = "bin_0_10km")

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
  if_else(
    is.na(x_chr) | x_chr == "",
    NA_character_,
    str_pad(str_replace(x_chr, "\\.0$", ""), 5, pad = "0")
  )
}

n_distinct_nonmissing <- function(x) {
  length(unique(x[!is.na(x) & x != ""]))
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

load_conservative_visit_venues <- function() {
  fairs <- fread(fairs_file, na.strings = c("", "NA")) %>%
    as_tibble()

  if (!"visits" %in% names(fairs)) stop("Missing required visits column in fairs file: ", fairs_file)
  if (!"parent_fair_id" %in% names(fairs)) fairs$parent_fair_id <- fairs$fair_id
  if (!"venue_seq" %in% names(fairs)) fairs$venue_seq <- 1L
  if (!"visits_measure" %in% names(fairs)) fairs$visits_measure <- NA_character_

  fairs <- fairs %>%
    mutate(
      fair_id = as.integer(fair_id),
      parent_fair_id = as.integer(parent_fair_id),
      venue_seq = as.integer(venue_seq),
      year_start = as.integer(year_start),
      visits_num = suppressWarnings(as.numeric(visits)),
      host_matched_country_iso3 = as.character(host_matched_country_iso3),
      venue_longitude = as.numeric(venue_longitude),
      venue_latitude = as.numeric(venue_latitude),
      venue_coordinates_note = as.character(venue_coordinates_note)
    )

  fair_visits_by_parent <- fairs %>%
    group_by(parent_fair_id) %>%
    summarise(
      fair_visits = {
        values <- visits_num[!is.na(visits_num)]
        if (length(values) == 0L) NA_real_ else max(values)
      },
      fair_visits_measure = first_nonmissing(visits_measure),
      .groups = "drop"
    )

  fairs <- fairs %>%
    left_join(fair_visits_by_parent, by = "parent_fair_id")

  venue_audit <- fairs %>%
    filter(
      year_start >= classification_year_min,
      year_start <= classification_year_max,
      host_matched_country_iso3 %in% c("GBR", "USA")
    ) %>%
    mutate(
      fair_has_visits = !is.na(fair_visits),
      fair_visits_positive = fair_has_visits & fair_visits > 0,
      excluded_missing_visits = !fair_has_visits,
      excluded_nonpositive_visits = fair_has_visits & fair_visits <= 0,
      has_venue_coordinates = !is.na(venue_longitude) & !is.na(venue_latitude),
      excluded_no_venue_coordinates = !has_venue_coordinates,
      excluded_low_quality_venue_coordinates =
        has_venue_coordinates &
          str_detect(
            coalesce(venue_coordinates_note, ""),
            fixed("automated geocoding returned no reliable coordinate")
          ),
      venue_coordinates_used_conservative =
        has_venue_coordinates & !excluded_low_quality_venue_coordinates,
      venue_used_conservative =
        fair_visits_positive & venue_coordinates_used_conservative
    )

  venues <- venue_audit %>%
    filter(venue_used_conservative) %>%
    select(any_of(c(
      "fair_id",
      "parent_fair_id",
      "venue_seq",
      "year_start",
      "City",
      "Country",
      "Fair_name",
      "visits",
      "visits_num",
      "fair_visits",
      "fair_visits_measure",
      "visits_measure",
      "host_matched_country_iso3",
      "host_matched_name",
      "host_admin1_name",
      "venue",
      "venue_longitude",
      "venue_latitude",
      "venue_coordinates_source_title",
      "venue_coordinates_note"
    )))

  list(venues = venues, audit = venue_audit)
}

classify_exposure <- function(first_exposure_year) {
  case_when(
    first_exposure_year < treated_event_year_min ~ "always_treated_pre_1840",
    first_exposure_year >= treated_event_year_min &
      first_exposure_year <= treated_event_year_max ~ "treated",
    first_exposure_year > treated_event_year_max &
      first_exposure_year <= classification_year_max ~ "future_treated_after_1910",
    TRUE ~ "outside_classification_window"
  )
}

empty_exposure <- function(targets_sf) {
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

  list(
    audit = tibble(),
    first_exposure = tibble(),
    never_units = target_dt %>% mutate(exposure_status = "never_treated"),
    always_units = tibble(),
    future_units = tibble()
  )
}

build_host_exposure_one_country <- function(targets_sf, venues_country) {
  if (nrow(venues_country) == 0L) return(empty_exposure(targets_sf))

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

  venue_points <- venues_country %>%
    st_as_sf(coords = c("venue_longitude", "venue_latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(st_crs(targets_sf))

  hit_index <- which(st_intersects(targets_sf, venue_points, sparse = FALSE), arr.ind = TRUE)
  if (nrow(hit_index) == 0L) return(empty_exposure(targets_sf))

  audit <- tibble(
    target_row = hit_index[, 1],
    venue_row = hit_index[, 2]
  ) %>%
    bind_cols(target_dt[.$target_row, ]) %>%
    bind_cols(venues_country[.$venue_row, ]) %>%
    mutate(treatment_spec = "hosted", hosted_unit = TRUE) %>%
    arrange(geo_country_iso3, unit_id, year_start, fair_id, venue_seq)

  first_exposure <- audit %>%
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
      treatment_spec = "hosted",
      first_fair_id = fair_id,
      first_parent_fair_id = parent_fair_id,
      first_venue_seq = venue_seq,
      first_fair_name = Fair_name,
      first_fair_city = City,
      first_fair_country = Country,
      first_fair_venue = venue,
      first_fair_visits = fair_visits,
      first_fair_visits_measure = fair_visits_measure,
      exposure_status = classify_exposure(first_exposure_year)
    )

  exposed_units <- first_exposure %>% distinct(unit_id)
  never_units <- target_dt %>%
    anti_join(exposed_units, by = "unit_id") %>%
    mutate(exposure_status = "never_treated")

  list(
    audit = audit,
    first_exposure = first_exposure,
    never_units = never_units,
    always_units = first_exposure %>% filter(exposure_status == "always_treated_pre_1840"),
    future_units = first_exposure %>% filter(exposure_status == "future_treated_after_1910")
  )
}

build_distance_exposure_one_country <- function(targets_sf, venues_country) {
  if (nrow(venues_country) == 0L) return(empty_exposure(targets_sf))

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

  venue_points <- venues_country %>%
    st_as_sf(coords = c("venue_longitude", "venue_latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(st_crs(targets_sf))

  distance_matrix <- matrix(
    as.numeric(st_distance(targets_sf, venue_points)),
    nrow = nrow(targets_sf),
    ncol = nrow(venues_country)
  )

  hit_index <- which(distance_matrix <= max_distance_m, arr.ind = TRUE)
  if (nrow(hit_index) == 0L) return(empty_exposure(targets_sf))

  audit <- tibble(
    target_row = hit_index[, 1],
    venue_row = hit_index[, 2],
    distance_km = distance_matrix[hit_index] / 1000
  ) %>%
    bind_cols(target_dt[.$target_row, ]) %>%
    bind_cols(venues_country[.$venue_row, ]) %>%
    mutate(treatment_spec = "0-10", distance_bin_km = "0-10") %>%
    arrange(geo_country_iso3, unit_id, year_start, distance_km, fair_id, venue_seq)

  first_exposure <- audit %>%
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
      treatment_spec = "0-10",
      distance_bin_km = "0-10",
      first_distance_km = distance_km,
      first_fair_id = fair_id,
      first_parent_fair_id = parent_fair_id,
      first_venue_seq = venue_seq,
      first_fair_name = Fair_name,
      first_fair_city = City,
      first_fair_country = Country,
      first_fair_venue = venue,
      first_fair_visits = fair_visits,
      first_fair_visits_measure = fair_visits_measure,
      exposure_status = classify_exposure(first_exposure_year)
    )

  exposed_units <- first_exposure %>% distinct(unit_id)
  never_units <- target_dt %>%
    anti_join(exposed_units, by = "unit_id") %>%
    mutate(exposure_status = "never_treated")

  list(
    audit = audit,
    first_exposure = first_exposure,
    never_units = never_units,
    always_units = first_exposure %>% filter(exposure_status == "always_treated_pre_1840"),
    future_units = first_exposure %>% filter(exposure_status == "future_treated_after_1910")
  )
}

bind_country_exposure <- function(uk_exposure, us_exposure) {
  list(
    audit = bind_rows(uk_exposure$audit, us_exposure$audit),
    first_exposure = bind_rows(uk_exposure$first_exposure, us_exposure$first_exposure),
    never_units = bind_rows(uk_exposure$never_units, us_exposure$never_units),
    always_units = bind_rows(uk_exposure$always_units, us_exposure$always_units),
    future_units = bind_rows(uk_exposure$future_units, us_exposure$future_units)
  )
}

summarise_effective_sample <- function(data_es, outcome, spec_label) {
  event_id <- coalesce(
    as.character(data_es$first_parent_fair_id),
    as.character(data_es$first_fair_id)
  )

  tibble(
    outcome = outcome,
    treatment_spec = spec_label,
    n_events = n_distinct_nonmissing(event_id[data_es$g > 0]),
    n_treated_gbr = n_distinct(data_es$unit_id[data_es$g > 0 & data_es$iso3 == "GBR"]),
    n_treated_usa = n_distinct(data_es$unit_id[data_es$g > 0 & data_es$iso3 == "USA"]),
    n_control_gbr = n_distinct(data_es$unit_id[data_es$g == 0 & data_es$iso3 == "GBR"]),
    n_control_usa = n_distinct(data_es$unit_id[data_es$g == 0 & data_es$iso3 == "USA"])
  )
}

extract_overall <- function(model, outcome, spec_label, parameter_label, estimate_scale = 1) {
  if (!is.null(model$overall_att)) {
    estimate <- as.numeric(model$overall_att$overall.att)
    se <- as.numeric(model$overall_att$overall.se)
  } else if (!is.null(model$overall_results)) {
    estimate <- as.numeric(model$overall_results$att)
    se <- as.numeric(model$overall_results$se)
  } else {
    estimate <- NA_real_
    se <- NA_real_
  }

  estimate <- estimate * estimate_scale
  se <- se * abs(estimate_scale)
  crit <- qnorm(0.975)
  tibble(
    outcome = outcome,
    treatment_spec = spec_label,
    parameter = parameter_label,
    estimate = estimate,
    se = se,
    p_value = 2 * (1 - pnorm(abs(estimate / se))),
    ci_low = estimate - crit * se,
    ci_high = estimate + crit * se
  )
}

extract_dynamic <- function(model, outcome, spec_label, parameter_label, estimate_scale = 1) {
  if (!is.null(model$event_study)) {
    event_time <- 10 * as.numeric(model$event_study$egt)
    estimate <- as.numeric(model$event_study$att.egt)
    se <- as.numeric(model$event_study$se.egt)
    crit <- model$event_study$crit.val.egt
    if (is.null(crit) || all(is.na(crit))) crit <- qnorm(0.975)
    crit <- as.numeric(crit)
  } else if (!is.null(model$dyn_results)) {
    event_time <- 10 * as.numeric(model$dyn_results$e)
    estimate <- as.numeric(model$dyn_results$att.e)
    se <- as.numeric(model$dyn_results$se)
    crit <- qnorm(0.975)
  } else {
    return(tibble())
  }

  estimate <- estimate * estimate_scale
  se <- se * abs(estimate_scale)
  tibble(
    outcome = outcome,
    treatment_spec = spec_label,
    parameter = parameter_label,
    event_time = event_time,
    estimate = estimate,
    se = se,
    crit = crit,
    ci_low = estimate - crit * se,
    ci_high = estimate + crit * se
  ) %>%
    filter(
      event_time >= -event_window,
      event_time <= event_window
    )
}

empty_overall_results <- function() {
  tibble(
    outcome = character(),
    treatment_spec = character(),
    parameter = character(),
    estimate = numeric(),
    se = numeric(),
    p_value = numeric(),
    ci_low = numeric(),
    ci_high = numeric()
  )
}

empty_dynamic_results <- function() {
  tibble(
    outcome = character(),
    treatment_spec = character(),
    parameter = character(),
    event_time = numeric(),
    estimate = numeric(),
    se = numeric(),
    crit = numeric(),
    ci_low = numeric(),
    ci_high = numeric()
  )
}

dynamic_y_limits <- function(dynamic_tbl) {
  y_values <- dynamic_tbl %>%
    select(estimate, ci_low, ci_high) %>%
    unlist(use.names = FALSE)
  y_values <- y_values[is.finite(y_values)]
  max_abs <- max(abs(y_values), na.rm = TRUE)
  if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
  c(-1.1 * max_abs, 1.1 * max_abs)
}

plot_dynamic <- function(dynamic_tbl, outcome, spec_label, parameter_label) {
  plot_data <- dynamic_tbl %>%
    filter(outcome == !!outcome, treatment_spec == !!spec_label, parameter == !!parameter_label)
  y_limits <- dynamic_y_limits(plot_data)

  ggplot(plot_data, aes(x = event_time, y = estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey45") +
    geom_vline(xintercept = -10, linewidth = 0.35, linetype = "dashed", color = "grey55") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 1.2, color = "#3f5f75") +
    geom_point(size = 2, color = "#153e5c") +
    scale_x_continuous(breaks = seq(-event_window, event_window, by = 10)) +
    coord_cartesian(ylim = y_limits) +
    labs(
      x = "Relative time (years)",
      y = if_else(parameter_label == "acrt", "ACRT per 100,000 visits", "ATTo"),
      title = str_wrap(
        paste("World's fairs visits contdid", spec_label, toupper(parameter_label), outcome),
        width = 72
      )
    ) +
    theme_minimal(base_size = 12)
}

prepare_contdid_data <- function(data, outcome) {
  all_decades <- sort(unique(data$decade))
  data_es <- data %>%
    mutate(
      period = match(decade, all_decades),
      g_period = if_else(g > 0, match(g, all_decades), 0L)
    ) %>%
    select(
      unit_num,
      unit_id,
      iso3,
      decade,
      period,
      g_period,
      dose_visits_100k,
      dose_for_contdid_atto,
      dose_for_contdid_acrt,
      first_parent_fair_id,
      first_fair_id,
      all_of(outcome)
    ) %>%
    rename(y = all_of(outcome)) %>%
    mutate(
      unit_num = as.numeric(unit_num),
      decade = as.numeric(decade),
      period = as.numeric(period),
      g_period = as.numeric(g_period),
      dose_visits_100k = as.numeric(dose_visits_100k),
      dose_for_contdid_atto = as.numeric(dose_for_contdid_atto),
      dose_for_contdid_acrt = as.numeric(dose_for_contdid_acrt),
      y = as.numeric(y)
    )

  all_periods <- sort(unique(data_es$period))
  complete_units <- data_es %>%
    group_by(unit_num) %>%
    summarise(
      n_periods = n_distinct(period),
      n_valid_y = sum(!is.na(y) & is.finite(y)),
      n_distinct_g = n_distinct(g_period),
      n_distinct_dose = n_distinct(dose_visits_100k),
      .groups = "drop"
    ) %>%
    filter(
      n_periods == length(all_periods),
      n_valid_y == length(all_periods),
      n_distinct_g == 1L,
      n_distinct_dose == 1L
    ) %>%
    pull(unit_num)

  data_es %>%
    filter(unit_num %in% complete_units, !is.na(y), is.finite(y))
}

run_contdid_model <- function(data, outcome, spec_label, parameter_label, spec_dir) {
  target_parameter <- if_else(parameter_label == "acrt", "slope", "level")
  data_es <- prepare_contdid_data(data, outcome)
  if (parameter_label == "acrt") {
    valid_acrt_cohorts <- data_es %>%
      filter(g_period > 0) %>%
      group_by(g_period) %>%
      summarise(n_positive_doses = n_distinct(dose_for_contdid_acrt), .groups = "drop") %>%
      filter(n_positive_doses >= 2L) %>%
      pull(g_period)

    data_es <- data_es %>%
      filter(g_period == 0 | g_period %in% valid_acrt_cohorts)
  }
  effective_sample_summary <- summarise_effective_sample(data_es, outcome, spec_label)

  fail_result <- function(error) {
    list(
      ok = FALSE,
      outcome = outcome,
      treatment_spec = spec_label,
      parameter = parameter_label,
      effective_sample_summary = effective_sample_summary,
      n_rows = nrow(data_es),
      n_units = n_distinct(data_es$unit_num),
      n_treated_units = n_distinct(data_es$unit_num[data_es$g_period > 0]),
      n_control_units = n_distinct(data_es$unit_num[data_es$g_period == 0]),
      n_treated_cohorts = n_distinct(data_es$g_period[data_es$g_period > 0]),
      n_positive_doses = n_distinct(data_es$dose_visits_100k[data_es$g_period > 0]),
      n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - n_distinct(data_es$unit_num),
      min_decade = if (nrow(data_es) == 0L) NA_real_ else min(data_es$decade, na.rm = TRUE),
      max_decade = if (nrow(data_es) == 0L) NA_real_ else max(data_es$decade, na.rm = TRUE),
      error = error
    )
  }

  if (n_distinct(data_es$g_period[data_es$g_period > 0]) == 0L) {
    return(fail_result("No treated cohorts in estimation sample."))
  }
  if (n_distinct(data_es$unit_num[data_es$g_period == 0]) == 0L) {
    return(fail_result("No never-treated control units in estimation sample."))
  }
  if (any(is.na(data_es$dose_visits_100k[data_es$g_period > 0])) ||
      any(data_es$dose_visits_100k[data_es$g_period > 0] <= 0)) {
    return(fail_result("Treated units contain missing or non-positive doses."))
  }
  if (parameter_label == "acrt" &&
      n_distinct(data_es$dose_visits_100k[data_es$g_period > 0]) < 2L) {
    return(fail_result("ACRT requires at least two distinct positive doses."))
  }
  if (parameter_label == "acrt" &&
      any(data_es$dose_for_contdid_acrt[data_es$g_period > 0] > 1, na.rm = TRUE)) {
    return(fail_result(
      paste0(
        "Internal contdid dose exceeds 1; increase CONTDID_INTERNAL_DOSE_SCALE above ",
        max(data_es$dose_visits_100k[data_es$g_period > 0], na.rm = TRUE) * dose_scale,
        "."
      )
    ))
  }
  if (n_distinct(data_es$y, na.rm = TRUE) < 2L) {
    return(fail_result("Outcome has insufficient variation."))
  }

  dname <- if_else(parameter_label == "acrt", "dose_for_contdid_acrt", "dose_for_contdid_atto")
  estimate_scale <- if_else(parameter_label == "acrt", acrt_rescale_to_100k_visits, 1)

  tryCatch(
    {
      model <- contdid::cont_did(
        yname = "y",
        dname = dname,
        gname = "g_period",
        tname = "period",
        idname = "unit_num",
        data = data_es,
        target_parameter = target_parameter,
        aggregation = "eventstudy",
        treatment_type = "continuous",
        control_group = control_group_name,
        base_period = "universal",
        bstrap = TRUE,
        cband = cband,
        biters = biters,
        cl = cl,
        degree = degree,
        num_knots = num_knots,
        min_e = -event_window_periods,
        max_e = event_window_periods
      )

      model_dir <- file.path(spec_dir, "contdid_objects")
      dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(
        model,
        file.path(
          model_dir,
          paste0("contdid_", parameter_label, "_", sanitize_filename(outcome), "_", sanitize_filename(spec_label), ".rds")
        )
      )

      list(
        ok = TRUE,
        outcome = outcome,
        treatment_spec = spec_label,
        parameter = parameter_label,
        model = model,
        overall = extract_overall(model, outcome, spec_label, parameter_label, estimate_scale),
        dynamic = extract_dynamic(model, outcome, spec_label, parameter_label, estimate_scale),
        effective_sample_summary = effective_sample_summary,
        n_rows = nrow(data_es),
        n_units = n_distinct(data_es$unit_num),
        n_treated_units = n_distinct(data_es$unit_num[data_es$g_period > 0]),
        n_control_units = n_distinct(data_es$unit_num[data_es$g_period == 0]),
        n_treated_cohorts = n_distinct(data_es$g_period[data_es$g_period > 0]),
        n_positive_doses = n_distinct(data_es$dose_visits_100k[data_es$g_period > 0]),
        n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - n_distinct(data_es$unit_num),
        min_decade = min(data_es$decade, na.rm = TRUE),
        max_decade = max(data_es$decade, na.rm = TRUE),
        error = NA_character_
      )
    },
    error = function(e) {
      fail_result(conditionMessage(e))
    }
  )
}

run_spec <- function(spec_label, exposure, panel_decade_base) {
  spec_dir <- file.path(results_dir, spec_dirs[[spec_label]])
  dir.create(spec_dir, recursive = TRUE, showWarnings = FALSE)

  first_exposure <- exposure$first_exposure
  never_units <- exposure$never_units
  always_units <- exposure$always_units
  future_units <- exposure$future_units

  treated_units <- first_exposure %>%
    filter(
      exposure_status == "treated",
      first_exposure_year >= treated_event_year_min,
      first_exposure_year <= treated_event_year_max,
      !is.na(first_fair_visits),
      first_fair_visits > 0
    ) %>%
    transmute(
      unit_id,
      target_unit_id,
      geo_country_iso3,
      event_year = first_exposure_year,
      g = first_exposure_decade,
      treatment_spec = spec_label,
      dose_visits = first_fair_visits,
      dose_visits_100k = first_fair_visits / dose_scale,
      dose_for_contdid_atto = 1,
      dose_for_contdid_acrt = first_fair_visits / contdid_internal_dose_scale,
      first_fair_id = as.character(first_fair_id),
      first_parent_fair_id = as.character(first_parent_fair_id),
      first_venue_seq,
      first_fair_name,
      first_fair_city,
      first_fair_country,
      first_fair_venue,
      first_fair_visits,
      first_fair_visits_measure,
      first_distance_km = if ("first_distance_km" %in% names(.)) first_distance_km else NA_real_
    )

  controls <- never_units %>%
    transmute(
      unit_id,
      target_unit_id,
      geo_country_iso3,
      g = 0L,
      dose_visits = 0,
      dose_visits_100k = 0,
      dose_for_contdid_atto = 0,
      dose_for_contdid_acrt = 0,
      first_parent_fair_id = NA_character_,
      first_fair_id = NA_character_
    )

  analysis_units <- bind_rows(
    treated_units %>%
      select(
        unit_id,
        target_unit_id,
        geo_country_iso3,
        g,
        dose_visits,
        dose_visits_100k,
        dose_for_contdid_atto,
        dose_for_contdid_acrt,
        first_parent_fair_id,
        first_fair_id
      ),
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
      mutate(treatment_spec = spec_label, cohort = as.character(g)) %>%
      select(treatment_spec, iso3, cohort, n_units),
    always_units %>%
      count(geo_country_iso3, name = "n_units") %>%
      transmute(
        treatment_spec = spec_label,
        iso3 = geo_country_iso3,
        cohort = "always_treated_pre_1840_excluded",
        n_units
      ),
    future_units %>%
      count(geo_country_iso3, name = "n_units") %>%
      transmute(
        treatment_spec = spec_label,
        iso3 = geo_country_iso3,
        cohort = "future_treated_after_1910_excluded",
        n_units
      )
  )

  sample_summary <- panel_decade %>%
    summarise(
      treatment_spec = spec_label,
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
      n_treated_cohorts = n_distinct(g[g > 0]),
      n_always_treated_excluded = nrow(always_units),
      n_future_treated_after_1910_excluded = nrow(future_units),
      min_dose_visits = min(dose_visits[g > 0], na.rm = TRUE),
      median_dose_visits = median(dose_visits[g > 0], na.rm = TRUE),
      max_dose_visits = max(dose_visits[g > 0], na.rm = TRUE),
      n_positive_doses = n_distinct(dose_visits_100k[g > 0]),
      total_inventors = sum(n_inventors, na.rm = TRUE),
      total_stem = sum(n_stem, na.rm = TRUE),
      .groups = "drop"
    )

  country_summary <- panel_decade %>%
    group_by(iso3) %>%
    summarise(
      treatment_spec = spec_label,
      n_rows = n(),
      n_units = n_distinct(unit_id),
      n_treated_units = n_distinct(unit_id[g > 0]),
      n_control_units = n_distinct(unit_id[g == 0]),
      total_inventors = sum(n_inventors, na.rm = TRUE),
      total_stem = sum(n_stem, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    relocate(treatment_spec)

  write_csv(treated_units, file.path(spec_dir, "treatment_assignment.csv"))
  write_csv(exposure$audit, file.path(spec_dir, "treatment_match_audit.csv"))
  write_csv(event_distribution, file.path(spec_dir, "event_distribution.csv"))
  write_csv(sample_summary, file.path(spec_dir, "sample_summary.csv"))
  write_csv(country_summary, file.path(spec_dir, "sample_summary_by_country.csv"))

  model_results <- list()
  for (outcome in outcomes) {
    message("  ", spec_label, " outcome: ", outcome, " ATTo")
    model_results[[paste(spec_label, outcome, "atto", sep = "__")]] <- run_contdid_model(
      data = panel_decade,
      outcome = outcome,
      spec_label = spec_label,
      parameter_label = "atto",
      spec_dir = spec_dir
    )
    message("  ", spec_label, " outcome: ", outcome, " ACRT")
    model_results[[paste(spec_label, outcome, "acrt", sep = "__")]] <- run_contdid_model(
      data = panel_decade,
      outcome = outcome,
      spec_label = spec_label,
      parameter_label = "acrt",
      spec_dir = spec_dir
    )
  }

  model_status <- imap_dfr(
    model_results,
    ~ tibble(
      outcome = .x$outcome,
      treatment_spec = .x$treatment_spec,
      parameter = .x$parameter,
      ok = .x$ok,
      n_rows = value_or(.x$n_rows, NA_integer_),
      n_units = value_or(.x$n_units, NA_integer_),
      n_treated_units = value_or(.x$n_treated_units, NA_integer_),
      n_control_units = value_or(.x$n_control_units, NA_integer_),
      n_treated_cohorts = value_or(.x$n_treated_cohorts, NA_integer_),
      n_positive_doses = value_or(.x$n_positive_doses, NA_integer_),
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

  successful_models <- keep(model_results, "ok")
  overall_all <- map_dfr(successful_models, "overall")
  dynamic_all <- map_dfr(successful_models, "dynamic")
  if (nrow(overall_all) == 0L) overall_all <- empty_overall_results()
  if (nrow(dynamic_all) == 0L) dynamic_all <- empty_dynamic_results()

  overall_atto <- overall_all %>% filter(parameter == "atto")
  overall_acrt <- overall_all %>% filter(parameter == "acrt")
  dynamic_atto <- dynamic_all %>% filter(parameter == "atto")
  dynamic_acrt <- dynamic_all %>% filter(parameter == "acrt")
  effective_sample_summary <- map_dfr(model_results, "effective_sample_summary") %>%
    distinct()

  write_csv(model_status, file.path(spec_dir, "model_status.csv"))
  write_csv(effective_sample_summary, file.path(spec_dir, "effective_sample_summary_by_outcome.csv"))
  write_csv(overall_atto, file.path(spec_dir, "contdid_overall_atto.csv"))
  write_csv(dynamic_atto, file.path(spec_dir, "contdid_dynamic_atto.csv"))
  write_csv(overall_acrt, file.path(spec_dir, "contdid_overall_acrt.csv"))
  write_csv(dynamic_acrt, file.path(spec_dir, "contdid_dynamic_acrt.csv"))
  write_csv(overall_all, file.path(spec_dir, "contdid_overall_all_parameters.csv"))
  write_csv(dynamic_all, file.path(spec_dir, "contdid_dynamic_all_parameters.csv"))

  for (parameter_label in c("atto", "acrt")) {
    dynamic_tbl <- dynamic_all %>% filter(parameter == parameter_label)
    if (nrow(dynamic_tbl) > 0L) {
      for (outcome in unique(dynamic_tbl$outcome)) {
        ggsave(
          file.path(
            spec_dir,
            paste0(
              "CONTDID_event_study_",
              parameter_label,
              "_",
              sanitize_filename(outcome),
              "_",
              sanitize_filename(spec_label),
              ".png"
            )
          ),
          plot_dynamic(dynamic_tbl, outcome, spec_label, parameter_label),
          width = 8,
          height = 6,
          dpi = 300
        )
      }
    }
  }

  notes <- c(
    paste0("World's fairs contdid visits event study: ", spec_label),
    "",
    paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Panel: ", panel_file),
    paste0("Fairs: ", fairs_file),
    "Dose: first fair visits / 100,000, fixed across periods.",
    paste0("Control group: ", control_group_name),
    paste0("Event window: +/-", event_window, " years."),
    "Dynamic outputs are post-filtered to this event window because contdid can return event-study points outside min_e/max_e.",
    paste0("Spline degree: ", degree),
    paste0("Spline knots: ", num_knots),
    paste0("Internal ACRT dose scale: visits / ", contdid_internal_dose_scale),
    paste0("ACRT output scale multiplier: ", acrt_rescale_to_100k_visits),
    paste0("Bootstrap iterations: ", biters),
    paste0("Uniform confidence bands: ", cband),
    paste0("Outcomes: ", paste(outcomes, collapse = ", ")),
    paste0("Treated units: ", sample_summary$n_treated_units),
    paste0("Never-treated control units: ", sample_summary$n_control_units),
    paste0("Always-treated units excluded: ", nrow(always_units)),
    paste0("Future-treated units after 1910 excluded: ", nrow(future_units)),
    paste0("Successful models: ", sum(model_status$ok), " / ", nrow(model_status))
  )
  writeLines(notes, file.path(spec_dir, "notes.txt"))

  list(
    sample_summary = sample_summary,
    country_summary = country_summary,
    model_status = model_status,
    effective_sample_summary = effective_sample_summary,
    overall_atto = overall_atto,
    dynamic_atto = dynamic_atto,
    overall_acrt = overall_acrt,
    dynamic_acrt = dynamic_acrt,
    overall_all = overall_all,
    dynamic_all = dynamic_all
  )
}

###############################################################################
# Load panel and build treatment exposure
###############################################################################

message("Reading UK+US panel...")
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

message("Loading venue data with positive visits...")
venue_data <- load_conservative_visit_venues()
venues <- venue_data$venues
write_csv(venue_data$audit, file.path(results_dir, "venue_quality_audit.csv"))

message("Building hosted exposure...")
hosted_exposure <- bind_country_exposure(
  build_host_exposure_one_country(uk_targets, venues %>% filter(host_matched_country_iso3 == "GBR")),
  build_host_exposure_one_country(us_targets, venues %>% filter(host_matched_country_iso3 == "USA"))
)
write_csv(hosted_exposure$audit, file.path(results_dir, "hosted_match_audit.csv"))
write_csv(hosted_exposure$first_exposure, file.path(results_dir, "first_exposure_hosted.csv"))
write_csv(hosted_exposure$never_units, file.path(results_dir, "never_treated_units_hosted.csv"))
write_csv(hosted_exposure$always_units, file.path(results_dir, "always_treated_pre_1840_units_hosted.csv"))
write_csv(hosted_exposure$future_units, file.path(results_dir, "future_treated_after_1910_units_hosted.csv"))

message("Building 0-10 km exposure...")
distance_exposure <- bind_country_exposure(
  build_distance_exposure_one_country(uk_targets, venues %>% filter(host_matched_country_iso3 == "GBR")),
  build_distance_exposure_one_country(us_targets, venues %>% filter(host_matched_country_iso3 == "USA"))
)
write_csv(distance_exposure$audit, file.path(results_dir, "venue_distance_match_audit_0_10km.csv"))
write_csv(distance_exposure$first_exposure, file.path(results_dir, "first_exposure_0_10km.csv"))
write_csv(distance_exposure$never_units, file.path(results_dir, "never_treated_units_0_10km.csv"))
write_csv(distance_exposure$always_units, file.path(results_dir, "always_treated_pre_1840_units_0_10km.csv"))
write_csv(distance_exposure$future_units, file.path(results_dir, "future_treated_after_1910_units_0_10km.csv"))

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
    inventors_per_100k_pop = if_else(population > 0, 100000 * n_inventors / population, NA_real_),
    stem_per_100k_pop = if_else(population > 0, 100000 * n_stem / population, NA_real_)
  )

###############################################################################
# Run contdid by specification
###############################################################################

root_results <- list()
if ("hosted" %in% selected_specs) {
  message("Running hosted contdid models...")
  root_results[["hosted"]] <- run_spec("hosted", hosted_exposure, panel_decade_base)
}
if ("0-10" %in% selected_specs) {
  message("Running 0-10 km contdid models...")
  root_results[["0-10"]] <- run_spec("0-10", distance_exposure, panel_decade_base)
}

root_suffix <- if (identical(selected_specs, available_specs)) "_all_specs" else "_selected_specs"

all_sample_summary <- map_dfr(root_results, "sample_summary")
all_country_summary <- map_dfr(root_results, "country_summary")
all_model_status <- map_dfr(root_results, "model_status")
all_effective_sample_summary <- map_dfr(root_results, "effective_sample_summary")
all_overall_atto <- map_dfr(root_results, "overall_atto")
all_dynamic_atto <- map_dfr(root_results, "dynamic_atto")
all_overall_acrt <- map_dfr(root_results, "overall_acrt")
all_dynamic_acrt <- map_dfr(root_results, "dynamic_acrt")
all_overall <- map_dfr(root_results, "overall_all")
all_dynamic <- map_dfr(root_results, "dynamic_all")

write_csv(all_sample_summary, file.path(results_dir, paste0("sample_summary", root_suffix, ".csv")))
write_csv(all_country_summary, file.path(results_dir, paste0("sample_summary_by_country", root_suffix, ".csv")))
write_csv(all_model_status, file.path(results_dir, paste0("model_status", root_suffix, ".csv")))
write_csv(
  all_effective_sample_summary,
  file.path(results_dir, paste0("effective_sample_summary_by_outcome", root_suffix, ".csv"))
)
write_csv(all_overall_atto, file.path(results_dir, paste0("contdid_overall_atto", root_suffix, ".csv")))
write_csv(all_dynamic_atto, file.path(results_dir, paste0("contdid_dynamic_atto", root_suffix, ".csv")))
write_csv(all_overall_acrt, file.path(results_dir, paste0("contdid_overall_acrt", root_suffix, ".csv")))
write_csv(all_dynamic_acrt, file.path(results_dir, paste0("contdid_dynamic_acrt", root_suffix, ".csv")))
write_csv(all_overall, file.path(results_dir, paste0("contdid_overall_all_parameters", root_suffix, ".csv")))
write_csv(all_dynamic, file.path(results_dir, paste0("contdid_dynamic_all_parameters", root_suffix, ".csv")))

notes <- c(
  "World's fairs contdid visits event studies",
  "",
  paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("TALENT_DETS_DATA_DIR: ", TALENT_DETS_DATA_DIR),
  paste0("Panel: ", panel_file),
  paste0("Fairs: ", fairs_file),
  paste0("Results directory: ", results_dir),
  "Dose: first fair visits / 100,000, fixed across periods, including pre-treatment periods.",
  "ATTo uses an internal binary positive-exposure dose because contdid's event-study level path is documented as binarized exposure.",
  paste0("ACRT internal dose: visits / ", contdid_internal_dose_scale, "."),
  "ACRT units: marginal response per 100,000 visits.",
  paste0("Control group: ", control_group_name),
  paste0("Selected specs: ", paste(selected_specs, collapse = ", ")),
  paste0("Included treated event window: ", treated_event_year_min, "-", treated_event_year_max, "."),
  paste0("Exposure classification window: ", classification_year_min, "-", classification_year_max, "."),
  "Always-treated units are first exposed before 1840 and excluded.",
  "Future-treated units are first exposed after 1910 through 1961 and excluded.",
  paste0("Event window: +/-", event_window, " years."),
  "Dynamic outputs are post-filtered to this event window because contdid can return event-study points outside min_e/max_e.",
  paste0("Spline degree: ", degree),
  paste0("Spline knots: ", num_knots),
  paste0("ACRT output scale multiplier: ", acrt_rescale_to_100k_visits),
  paste0("Bootstrap iterations: ", biters),
  paste0("Uniform confidence bands: ", cband),
  paste0("Outcomes: ", paste(outcomes, collapse = ", ")),
  paste0("Successful models: ", sum(all_model_status$ok), " / ", nrow(all_model_status)),
  paste0("Elapsed minutes: ", round(difftime(Sys.time(), initial_time, units = "mins"), 1))
)
writeLines(notes, file.path(results_dir, "notes.txt"))

message("Saved results in: ", results_dir)
message("Done. Elapsed: ", round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
