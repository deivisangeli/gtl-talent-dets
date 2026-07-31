###############################################################################
# Helpers for USA-only AMWS world's-fairs event studies.
###############################################################################

value_or <- function(x, default) {
  if (is.null(x) || length(x) == 0L) default else x
}

world_fairs_us_amws_profile <- function(
    profile = Sys.getenv("WORLD_FAIRS_PROFILE", unset = "main")) {
  profiles <- list(
    main = list(
      name = "main",
      results_subdir = "worlds_fairs_us_amws_event_studies_1840_1910",
      treated_event_year_min = 1840L,
      treated_event_year_max = 1910L,
      treated_cohort_min = 1840L,
      treated_cohort_max = 1910L,
      event_time_min = -20L,
      event_time_max = 50L,
      expected_assigned = c(15L, 15L, 42L, 46L),
      expected_eligible = c(13L, 13L, 34L, 37L)
    ),
    robust_m30 = list(
      name = "robust_m30",
      results_subdir = paste0(
        "worlds_fairs_us_amws_event_studies_",
        "cohorts_1840_1900_e_m30_p50"
      ),
      treated_event_year_min = 1840L,
      # Years 1900--1909 all map to treatment cohort g = 1900.
      treated_event_year_max = 1909L,
      treated_cohort_min = 1840L,
      treated_cohort_max = 1900L,
      event_time_min = -30L,
      event_time_max = 50L,
      expected_assigned = c(15L, 15L, 42L, 46L),
      expected_eligible = c(10L, 10L, 30L, 33L)
    ),
    robust_m30_pop_m10_balanced_oldest = list(
      name = "robust_m30_pop_m10_balanced_oldest",
      results_subdir = paste0(
        "worlds_fairs_us_amws_event_studies_",
        "cohorts_1840_1900_e_m30_p50_balanced_oldest_logpop_m10"
      ),
      treated_event_year_min = 1840L,
      # Years 1900--1909 all map to treatment cohort g = 1900.
      treated_event_year_max = 1909L,
      treated_cohort_min = 1840L,
      treated_cohort_max = 1900L,
      event_time_min = -30L,
      event_time_max = 50L,
      expected_assigned = c(15L, 15L, 42L, 46L),
      expected_eligible = c(10L, 10L, 30L, 33L),
      expected_core_controls = c(1164L, 687L, 1140L, 676L),
      balance_controls_calendar = TRUE,
      population_control = TRUE,
      est_method = "reg",
      plot_estimator_label = "CSDID + log population, ref e=-10"
    ),
    robust_m30_pop_m10_balanced_oldest_g_shift = list(
      name = "robust_m30_pop_m10_balanced_oldest_g_shift",
      results_subdir = paste0(
        "worlds_fairs_us_amws_event_studies_",
        "g_shift_e_m30_p50_balanced_oldest_logpop_m10"
      ),
      treated_event_year_min = 1840L,
      treated_event_year_max = 1909L,
      treated_cohort_min = 1840L,
      # Fairs in 1907--1909 map to the shifted cohort g = 1910.
      treated_cohort_max = 1910L,
      treatment_timing = "alternative_decade",
      event_time_min = -30L,
      event_time_max = 50L,
      expected_assigned = c(15L, 15L, 42L, 46L),
      expected_eligible = c(12L, 12L, 32L, 35L),
      expected_core_controls = c(1164L, 687L, 1140L, 676L),
      balance_controls_calendar = TRUE,
      population_control = TRUE,
      est_method = "reg",
      plot_estimator_label = paste0(
        "CSDID + log population, alternative-decade timing, ref e=-10"
      )
    ),
    robust_m30_pop_m10_balanced_oldest_single_fair = list(
      name = "robust_m30_pop_m10_balanced_oldest_single_fair",
      results_subdir = paste0(
        "worlds_fairs_us_amws_event_studies_",
        "cohorts_1840_1900_e_m30_p50_balanced_oldest_logpop_m10_",
        "single_fair"
      ),
      treated_event_year_min = 1840L,
      # Years 1900--1909 all map to treatment cohort g = 1900.
      treated_event_year_max = 1909L,
      treated_cohort_min = 1840L,
      treated_cohort_max = 1900L,
      event_time_min = -30L,
      event_time_max = 50L,
      expected_assigned = c(11L, 11L, 29L, 30L),
      expected_eligible = c(7L, 7L, 20L, 21L),
      expected_core_controls = c(1164L, 687L, 1140L, 676L),
      balance_controls_calendar = TRUE,
      control_sample_source_profile = "robust_m30_pop_m10_balanced_oldest",
      population_control = TRUE,
      single_fair_event_window = TRUE,
      est_method = "reg",
      plot_estimator_label = paste0(
        "CSDID + log population, single fair in e=[-30,+50], ",
        "standard-decade timing, ref e=-10"
      )
    ),
    robust_m30_pop_m10_balanced_oldest_single_fair_g_shift = list(
      name = "robust_m30_pop_m10_balanced_oldest_single_fair_g_shift",
      results_subdir = paste0(
        "worlds_fairs_us_amws_event_studies_",
        "g_shift_e_m30_p50_balanced_oldest_logpop_m10_single_fair"
      ),
      treated_event_year_min = 1840L,
      treated_event_year_max = 1909L,
      treated_cohort_min = 1840L,
      treated_cohort_max = 1910L,
      treatment_timing = "alternative_decade",
      event_time_min = -30L,
      event_time_max = 50L,
      expected_assigned = c(11L, 11L, 29L, 30L),
      expected_eligible = c(8L, 8L, 21L, 22L),
      expected_core_controls = c(1164L, 687L, 1140L, 676L),
      balance_controls_calendar = TRUE,
      control_sample_source_profile =
        "robust_m30_pop_m10_balanced_oldest_g_shift",
      population_control = TRUE,
      single_fair_event_window = TRUE,
      est_method = "reg",
      plot_estimator_label = paste0(
        "CSDID + log population, single fair in e=[-30,+50], ",
        "alternative-decade timing, ref e=-10"
      )
    ),
    robust_m30_pop_m10_balanced_oldest_treatment_m20 = list(
      name = "robust_m30_pop_m10_balanced_oldest_treatment_m20",
      results_subdir = paste0(
        "worlds_fairs_us_amws_event_studies_",
        "fair_cohorts_1840_1900_treatment_m20_e_m30_p50_",
        "balanced_oldest_logpop_m10"
      ),
      treated_event_year_min = 1840L,
      # Years 1900--1909 all map to fair cohort 1900, then to treatment g=1880.
      treated_event_year_max = 1909L,
      treated_cohort_min = 1820L,
      treated_cohort_max = 1880L,
      treatment_cohort_shift = -20L,
      event_time_min = -30L,
      event_time_max = 50L,
      expected_assigned = c(15L, 15L, 42L, 46L),
      expected_eligible = c(8L, 8L, 19L, 23L),
      expected_core_controls = c(687L, 372L, 669L, 364L),
      balance_controls_calendar = TRUE,
      population_control = TRUE,
      est_method = "reg",
      plot_estimator_label = paste0(
        "CSDID + log population, g = fair decade - 20, ref e=-10"
      )
    )
  )

  if (!profile %in% names(profiles)) {
    stop(
      "Unknown WORLD_FAIRS_PROFILE: ", profile,
      ". Expected one of: ", paste(names(profiles), collapse = ", ")
    )
  }
  profiles[[profile]]
}

standard_decade <- function(year) {
  as.integer(floor(as.numeric(year) / 10) * 10)
}

alternative_decade <- function(year) {
  year <- as.integer(year)
  standard_decade(year) + dplyr::if_else(
    !is.na(year) & year %% 10L >= 7L,
    10L,
    0L
  )
}

event_decade <- function(year, timing = "standard_decade") {
  if (identical(timing, "standard_decade")) return(standard_decade(year))
  if (identical(timing, "alternative_decade")) return(alternative_decade(year))
  stop(
    "Unknown treatment timing: ", timing,
    ". Expected standard_decade or alternative_decade."
  )
}

pad_geoid <- function(x) {
  out <- suppressWarnings(as.integer(as.character(x)))
  ifelse(is.na(out), NA_character_, sprintf("%05d", out))
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sum_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

sanitize_filename <- function(x) {
  stringr::str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

build_us_target_geometries <- function(panel_decade) {
  us_units <- panel_decade %>%
    dplyr::distinct(GEOID) %>%
    dplyr::mutate(
      GEOID = pad_geoid(GEOID),
      unit_id = paste0("US_COUNTY_", GEOID)
    ) %>%
    dplyr::filter(!is.na(GEOID))

  counties_poly <- tigris::counties(
    cb = TRUE,
    resolution = "20m",
    year = 2020,
    class = "sf"
  ) %>%
    sf::st_transform(5070) %>%
    dplyr::select(GEOID, NAMELSAD, STATEFP, geometry) %>%
    dplyr::filter(as.integer(STATEFP) <= 56) %>%
    dplyr::mutate(GEOID = as.character(GEOID)) %>%
    dplyr::inner_join(us_units, by = "GEOID")

  missing_us <- us_units %>%
    dplyr::anti_join(sf::st_drop_geometry(counties_poly), by = "GEOID")

  targets <- counties_poly %>%
    dplyr::transmute(
      unit_id,
      target_unit_id = unit_id,
      target_unit_name = NAMELSAD,
      target_area_type = "US County",
      target_boundary_id = GEOID,
      geo_country_iso3 = "USA",
      GEOID,
      geometry
    ) %>%
    sf::st_make_valid()

  list(targets = targets, missing = missing_us)
}

load_us_venue_data <- function(
    fairs_file,
    classification_year_min,
    classification_year_max,
    visits_threshold) {
  fairs <- data.table::fread(fairs_file, na.strings = c("", "NA")) %>%
    tibble::as_tibble()

  if (!"parent_fair_id" %in% names(fairs)) fairs$parent_fair_id <- fairs$fair_id
  if (!"venue_seq" %in% names(fairs)) fairs$venue_seq <- 1L
  if (!"visits" %in% names(fairs)) {
    stop("Missing required visits column in fairs file: ", fairs_file)
  }

  fairs <- fairs %>%
    dplyr::mutate(
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

  fair_visits <- fairs %>%
    dplyr::group_by(parent_fair_id) %>%
    dplyr::summarise(
      fair_visits = {
        values <- visits_num[!is.na(visits_num)]
        if (length(values) == 0L) NA_real_ else max(values)
      },
      .groups = "drop"
    )

  audit <- fairs %>%
    dplyr::left_join(fair_visits, by = "parent_fair_id") %>%
    dplyr::filter(
      year_start >= classification_year_min,
      year_start <= classification_year_max,
      host_matched_country_iso3 == "USA"
    ) %>%
    dplyr::mutate(
      fair_has_visits = !is.na(fair_visits),
      fair_visits_ge_threshold = fair_has_visits & fair_visits >= visits_threshold,
      has_venue_coordinates = !is.na(venue_longitude) & !is.na(venue_latitude),
      excluded_no_venue_coordinates = !has_venue_coordinates,
      excluded_low_quality_venue_coordinates =
        has_venue_coordinates &
        stringr::str_detect(
          dplyr::coalesce(venue_coordinates_note, ""),
          stringr::fixed("automated geocoding returned no reliable coordinate")
        ),
      venue_coordinates_used_conservative =
        has_venue_coordinates & !excluded_low_quality_venue_coordinates,
      venue_used_all = venue_coordinates_used_conservative,
      venue_used_visits_100k =
        venue_coordinates_used_conservative & fair_visits_ge_threshold
    )

  venue_columns <- c(
    "fair_id", "parent_fair_id", "venue_seq", "year_start", "City",
    "Country", "Fair_name", "host_matched_country_iso3",
    "host_matched_name", "host_admin1_name", "venue", "fair_visits",
    "venue_longitude", "venue_latitude", "venue_coordinates_source_title",
    "venue_coordinates_note"
  )

  list(
    audit = audit,
    all = audit %>%
      dplyr::filter(venue_used_all) %>%
      dplyr::select(dplyr::all_of(venue_columns)),
    visits_100k = audit %>%
      dplyr::filter(venue_used_visits_100k) %>%
      dplyr::select(dplyr::all_of(venue_columns))
  )
}

empty_exposure <- function(target_dt, audit_name) {
  out <- list(
    first_exposure = tibble::tibble(),
    never_units = target_dt %>% dplyr::mutate(exposure_status = "never_treated"),
    always_units = tibble::tibble(),
    future_units = tibble::tibble()
  )
  out[[audit_name]] <- tibble::tibble()
  out
}

classify_first_exposure <- function(audit, treated_event_year_min, treated_event_year_max,
                                    classification_year_max, distance = FALSE,
                                    timing = "standard_decade") {
  always_status <- paste0("always_treated_pre_", treated_event_year_min)
  future_status <- paste0("future_treated_after_", treated_event_year_max)
  first <- audit %>%
    dplyr::group_by(unit_id) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()

  common <- first %>%
    dplyr::transmute(
      unit_id,
      target_unit_id,
      target_unit_name,
      target_area_type,
      target_boundary_id,
      geo_country_iso3,
      GEOID,
      first_exposure_year = year_start,
      first_exposure_decade = event_decade(first_exposure_year, timing),
      first_fair_id = fair_id,
      first_parent_fair_id = parent_fair_id,
      first_venue_seq = venue_seq,
      first_fair_name = Fair_name,
      first_fair_city = City,
      first_fair_country = Country,
      first_fair_venue = venue,
      exposure_status = dplyr::case_when(
        first_exposure_year < treated_event_year_min ~ always_status,
        first_exposure_year >= treated_event_year_min &
          first_exposure_year <= treated_event_year_max ~ "treated",
        first_exposure_year > treated_event_year_max &
          first_exposure_year <= classification_year_max ~ future_status,
        TRUE ~ "outside_classification_window"
      )
    )

  if (distance) {
    common <- common %>%
      dplyr::left_join(
        first %>%
          dplyr::select(
            unit_id,
            distance_bin_km,
            first_distance_km = distance_km
          ),
        by = "unit_id"
      ) %>%
      dplyr::mutate(distance_bin_km = as.character(distance_bin_km))
  }
  common
}

build_host_exposure <- function(targets_sf, venues, treated_event_year_min,
                                treated_event_year_max, classification_year_max,
                                timing = "standard_decade") {
  target_dt <- targets_sf %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble()

  if (nrow(venues) == 0L) return(empty_exposure(target_dt, "match_audit"))

  venue_points <- venues %>%
    sf::st_as_sf(
      coords = c("venue_longitude", "venue_latitude"),
      crs = 4326,
      remove = FALSE
    ) %>%
    sf::st_transform(sf::st_crs(targets_sf))

  hit_index <- which(sf::st_intersects(targets_sf, venue_points, sparse = FALSE),
                     arr.ind = TRUE)
  if (nrow(hit_index) == 0L) return(empty_exposure(target_dt, "match_audit"))

  match_audit <- tibble::tibble(
    target_row = hit_index[, 1],
    venue_row = hit_index[, 2]
  ) %>%
    dplyr::bind_cols(target_dt[hit_index[, 1], ]) %>%
    dplyr::bind_cols(venues[hit_index[, 2], ]) %>%
    dplyr::mutate(hosted_unit = TRUE) %>%
    dplyr::arrange(unit_id, year_start, fair_id, venue_seq)

  first_exposure <- classify_first_exposure(
    match_audit,
    treated_event_year_min,
    treated_event_year_max,
    classification_year_max,
    distance = FALSE,
    timing = timing
  )

  never_units <- target_dt %>%
    dplyr::anti_join(first_exposure %>% dplyr::distinct(unit_id), by = "unit_id") %>%
    dplyr::mutate(exposure_status = "never_treated")

  list(
    match_audit = match_audit,
    first_exposure = first_exposure,
    never_units = never_units,
    always_units = first_exposure %>%
      dplyr::filter(exposure_status == paste0("always_treated_pre_", treated_event_year_min)),
    future_units = first_exposure %>%
      dplyr::filter(exposure_status == paste0("future_treated_after_", treated_event_year_max))
  )
}

build_distance_exposure <- function(targets_sf, venues, bin_breaks, bin_labels,
                                    treated_event_year_min, treated_event_year_max,
                                    classification_year_max,
                                    timing = "standard_decade") {
  target_dt <- targets_sf %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble()

  if (nrow(venues) == 0L) return(empty_exposure(target_dt, "match_audit"))

  venue_points <- venues %>%
    sf::st_as_sf(
      coords = c("venue_longitude", "venue_latitude"),
      crs = 4326,
      remove = FALSE
    ) %>%
    sf::st_transform(sf::st_crs(targets_sf))

  distance_matrix <- matrix(
    as.numeric(sf::st_distance(targets_sf, venue_points)),
    nrow = nrow(targets_sf),
    ncol = nrow(venues)
  )
  hit_index <- which(distance_matrix <= 10000, arr.ind = TRUE)
  if (nrow(hit_index) == 0L) return(empty_exposure(target_dt, "match_audit"))

  match_audit <- tibble::tibble(
    target_row = hit_index[, 1],
    venue_row = hit_index[, 2],
    distance_km = distance_matrix[hit_index] / 1000
  ) %>%
    dplyr::bind_cols(target_dt[hit_index[, 1], ]) %>%
    dplyr::bind_cols(venues[hit_index[, 2], ]) %>%
    dplyr::mutate(
      distance_bin_km = cut(
        distance_km,
        breaks = bin_breaks,
        labels = bin_labels,
        include.lowest = TRUE,
        right = TRUE
      )
    ) %>%
    dplyr::arrange(unit_id, year_start, distance_km, fair_id, venue_seq)

  first_exposure <- classify_first_exposure(
    match_audit,
    treated_event_year_min,
    treated_event_year_max,
    classification_year_max,
    distance = TRUE,
    timing = timing
  )

  never_units <- target_dt %>%
    dplyr::anti_join(first_exposure %>% dplyr::distinct(unit_id), by = "unit_id") %>%
    dplyr::mutate(exposure_status = "never_treated")

  list(
    match_audit = match_audit,
    first_exposure = first_exposure,
    never_units = never_units,
    always_units = first_exposure %>%
      dplyr::filter(exposure_status == paste0("always_treated_pre_", treated_event_year_min)),
    future_units = first_exposure %>%
      dplyr::filter(exposure_status == paste0("future_treated_after_", treated_event_year_max))
  )
}

build_single_fair_window_eligibility <- function(treated, all_event_audit,
                                                  event_times,
                                                  timing = "standard_decade") {
  if (nrow(treated) == 0L) {
    return(list(
      audit = treated %>%
        dplyr::mutate(
          fair_window_start = integer(),
          fair_window_end = integer(),
          n_distinct_fairs_in_window = integer(),
          fair_parent_ids_in_window = character(),
          fair_years_in_window = character(),
          fair_names_in_window = character(),
          single_fair_eligible = logical()
        ),
      detail = tibble::tibble(),
      eligible = treated
    ))
  }

  required_audit_columns <- c(
    "unit_id", "parent_fair_id", "fair_id", "year_start", "Fair_name",
    "City"
  )
  missing_audit_columns <- setdiff(required_audit_columns, names(all_event_audit))
  if (length(missing_audit_columns) > 0L) {
    stop(
      "Single-fair audit is missing columns: ",
      paste(missing_audit_columns, collapse = ", ")
    )
  }

  unit_windows <- treated %>%
    dplyr::distinct(unit_id, g) %>%
    dplyr::mutate(
      fair_window_start = g + min(event_times),
      fair_window_end = g + max(event_times)
    )

  detail <- all_event_audit %>%
    dplyr::mutate(
      fair_event_decade = event_decade(year_start, timing)
    ) %>%
    dplyr::inner_join(unit_windows, by = "unit_id") %>%
    dplyr::filter(
      fair_event_decade >= fair_window_start,
      fair_event_decade <= fair_window_end
    ) %>%
    dplyr::arrange(unit_id, year_start, parent_fair_id, fair_id) %>%
    dplyr::group_by(
      unit_id, g, fair_window_start, fair_window_end, parent_fair_id
    ) %>%
    dplyr::summarise(
      fair_id = dplyr::first(fair_id),
      fair_year = dplyr::first(year_start),
      fair_decade = dplyr::first(fair_event_decade),
      fair_name = dplyr::first(Fair_name),
      fair_city = dplyr::first(City),
      n_matching_venue_rows = dplyr::n(),
      .groups = "drop"
    )

  event_summary <- detail %>%
    dplyr::arrange(unit_id, fair_year, parent_fair_id) %>%
    dplyr::group_by(unit_id, g, fair_window_start, fair_window_end) %>%
    dplyr::summarise(
      n_distinct_fairs_in_window = dplyr::n_distinct(parent_fair_id),
      fair_parent_ids_in_window = paste(parent_fair_id, collapse = ";"),
      fair_years_in_window = paste(fair_year, collapse = ";"),
      fair_names_in_window = paste(fair_name, collapse = " | "),
      .groups = "drop"
    )

  audit <- treated %>%
    dplyr::left_join(event_summary, by = c("unit_id", "g")) %>%
    dplyr::mutate(
      fair_window_start = dplyr::coalesce(fair_window_start, g + min(event_times)),
      fair_window_end = dplyr::coalesce(fair_window_end, g + max(event_times)),
      n_distinct_fairs_in_window = tidyr::replace_na(
        n_distinct_fairs_in_window, 0L
      ),
      fair_parent_ids_in_window = tidyr::replace_na(
        fair_parent_ids_in_window, ""
      ),
      fair_years_in_window = tidyr::replace_na(fair_years_in_window, ""),
      fair_names_in_window = tidyr::replace_na(fair_names_in_window, ""),
      single_fair_eligible = n_distinct_fairs_in_window == 1L
    )

  list(
    audit = audit,
    detail = detail,
    eligible = treated %>%
      dplyr::semi_join(
        audit %>% dplyr::filter(single_fair_eligible),
        by = c("unit_id", "g")
      )
  )
}

build_treatment_eligibility <- function(treated, panel_decade, outcomes, event_times) {
  if (nrow(treated) == 0L) {
    return(list(
      audit = tibble::tibble(),
      detail = tibble::tibble(),
      eligible = treated
    ))
  }

  detail <- tidyr::crossing(
    treated %>%
      dplyr::select(
        unit_id, GEOID, g, dplyr::everything()
      ) %>%
      dplyr::distinct(),
    event_time = event_times
  ) %>%
    dplyr::mutate(decade = g + event_time) %>%
    dplyr::left_join(
      panel_decade %>%
        dplyr::select(GEOID, decade, dplyr::all_of(outcomes)),
      by = c("GEOID", "decade")
    )

  outcome_matrix <- as.matrix(detail[, outcomes, drop = FALSE])
  detail$row_present <- rowSums(is.finite(outcome_matrix)) == length(outcomes)

  audit <- detail %>%
    dplyr::group_by(unit_id, GEOID, g) %>%
    dplyr::summarise(
      eligible = all(row_present),
      observed_periods = sum(row_present),
      required_periods = dplyr::n(),
      missing_event_times = paste(event_time[!row_present], collapse = ";"),
      .groups = "drop"
    ) %>%
    dplyr::left_join(treated, by = c("unit_id", "GEOID", "g"))

  list(
    audit = audit,
    detail = detail,
    eligible = audit %>% dplyr::filter(eligible)
  )
}

build_calendar_balanced_never_controls <- function(
    controls, panel_decade, support_vars, event_times, treated_cohorts) {
  if (nrow(controls) == 0L) {
    return(list(
      audit = tibble::tibble(),
      detail = tibble::tibble(),
      eligible = controls,
      required_decades = integer()
    ))
  }

  treated_cohorts <- sort(unique(as.integer(treated_cohorts)))
  treated_cohorts <- treated_cohorts[is.finite(treated_cohorts)]
  if (length(treated_cohorts) == 0L) {
    stop("Calendar control balance requires at least one treated cohort.")
  }
  required_decades <- sort(unique(as.integer(
    outer(treated_cohorts, as.integer(event_times), `+`)
  )))
  calendar_decade_min <- min(required_decades)
  calendar_decade_max <- max(required_decades)

  detail <- tidyr::crossing(
    controls %>%
      dplyr::select(unit_id, GEOID, dplyr::everything()) %>%
      dplyr::distinct(),
    decade = required_decades
  ) %>%
    dplyr::mutate(
      calendar_decade_min = .env$calendar_decade_min,
      calendar_decade_max = .env$calendar_decade_max
    ) %>%
    dplyr::left_join(
      panel_decade %>%
        dplyr::select(GEOID, decade, dplyr::all_of(support_vars)),
      by = c("GEOID", "decade")
    )

  support_matrix <- as.matrix(detail[, support_vars, drop = FALSE])
  detail$row_present <- rowSums(is.finite(support_matrix)) == length(support_vars)

  audit <- detail %>%
    dplyr::group_by(unit_id, GEOID, calendar_decade_min, calendar_decade_max) %>%
    dplyr::summarise(
      eligible = all(row_present),
      observed_periods = sum(row_present),
      required_periods = dplyr::n(),
      required_calendar_decades = paste(decade, collapse = ";"),
      missing_calendar_decades = paste(decade[!row_present], collapse = ";"),
      .groups = "drop"
    ) %>%
    dplyr::left_join(controls, by = c("unit_id", "GEOID"))

  list(
    audit = audit,
    detail = detail,
    eligible = audit %>% dplyr::filter(eligible),
    required_decades = required_decades
  )
}

build_support_by_event_time <- function(panel_decade, treated, controls, outcomes,
                                        event_times, spec_label, bin_label) {
  if (nrow(treated) == 0L) return(tibble::tibble())
  cohorts <- sort(unique(treated$g))
  rows <- vector("list", length(outcomes) * length(cohorts) * length(event_times))
  idx <- 0L

  for (outcome in outcomes) {
    for (cohort in cohorts) {
      cohort_units <- treated %>% dplyr::filter(g == cohort) %>% dplyr::pull(GEOID)
      for (event_time in event_times) {
        idx <- idx + 1L
        calendar_decade <- cohort + event_time
        observed <- panel_decade %>%
          dplyr::filter(decade == calendar_decade, is.finite(.data[[outcome]]))
        rows[[idx]] <- tibble::tibble(
          treatment_spec = spec_label,
          distance_bin_km = bin_label,
          outcome = outcome,
          cohort = cohort,
          event_time = event_time,
          calendar_decade = calendar_decade,
          n_treated_units = sum(cohort_units %in% observed$GEOID),
          n_control_units = sum(controls$GEOID %in% observed$GEOID)
        )
      }
    }
  }
  dplyr::bind_rows(rows[seq_len(idx)])
}

extract_dynamic_att <- function(es, outcome, spec_label, bin_label) {
  tibble::tibble(
    treatment_spec = spec_label,
    distance_bin_km = bin_label,
    outcome = outcome,
    event_time = es$egt,
    estimate = es$att.egt,
    se = es$se.egt,
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se
  )
}

extract_simple_att <- function(ag, outcome, spec_label, bin_label) {
  tibble::tibble(
    treatment_spec = spec_label,
    distance_bin_km = bin_label,
    outcome = outcome,
    estimate = ag$overall.att,
    se = ag$overall.se,
    p_value = 2 * (1 - stats::pnorm(abs(estimate / se))),
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se
  )
}

run_amws_event_study <- function(data, outcome, spec_label, bin_label,
                                 event_time_min, event_time_max, cores,
                                 population_control = FALSE,
                                 est_method = "dr") {
  data_es <- data %>%
    dplyr::transmute(
      unit_num = as.numeric(.data$unit_num),
      GEOID = .data$GEOID,
      decade = as.numeric(.data$decade),
      g = as.numeric(.data$g),
      y = as.numeric(.data[[outcome]]),
      x = if (population_control) log(as.numeric(.data$population)) else 1
    ) %>%
    dplyr::filter(is.finite(y), is.finite(x))

  base_result <- list(
    outcome = outcome,
    treatment_spec = spec_label,
    distance_bin_km = bin_label,
    n_rows = nrow(data_es),
    n_units = dplyr::n_distinct(data_es$unit_num),
    n_treated_units = dplyr::n_distinct(data_es$unit_num[data_es$g > 0]),
    n_control_units = dplyr::n_distinct(data_es$unit_num[data_es$g == 0]),
    n_treated_cohorts = dplyr::n_distinct(data_es$g[data_es$g > 0])
  )

  fail <- function(message) c(base_result, list(ok = FALSE, error = message))
  if (base_result$n_treated_units == 0L) return(fail("No eligible treated units."))
  if (base_result$n_control_units == 0L) return(fail("No never-treated controls."))
  if (dplyr::n_distinct(data_es$y) < 2L) return(fail("Outcome has insufficient variation."))

  tryCatch(
    {
      model <- did::att_gt(
        yname = "y",
        tname = "decade",
        idname = "unit_num",
        gname = "g",
        xformla = if (population_control) ~ x else ~ 1,
        data = data_es,
        panel = TRUE,
        allow_unbalanced_panel = TRUE,
        control_group = "nevertreated",
        est_method = est_method,
        base_period = "universal",
        cores = cores
      )
      dynamic <- did::aggte(
        model,
        type = "dynamic",
        min_e = event_time_min,
        max_e = event_time_max,
        na.rm = TRUE
      )
      simple <- did::aggte(
        model,
        type = "simple",
        min_e = 0,
        max_e = event_time_max,
        na.rm = TRUE
      )
      c(base_result, list(
        ok = TRUE,
        error = NA_character_,
        model = model,
        dynamic = dynamic,
        simple = simple
      ))
    },
    error = function(e) fail(conditionMessage(e))
  )
}

model_status_row <- function(result) {
  tibble::tibble(
    treatment_spec = result$treatment_spec,
    distance_bin_km = result$distance_bin_km,
    outcome = result$outcome,
    ok = result$ok,
    n_rows = value_or(result$n_rows, NA_integer_),
    n_units = value_or(result$n_units, NA_integer_),
    n_treated_units = value_or(result$n_treated_units, NA_integer_),
    n_control_units = value_or(result$n_control_units, NA_integer_),
    n_treated_cohorts = value_or(result$n_treated_cohorts, NA_integer_),
    error = value_or(result$error, NA_character_)
  )
}

world_fairs_treatment_label <- function(spec_label, bin_label = NA_character_) {
  switch(
    spec_label,
    hosted = "hosted counties",
    hosted_visits_100k = "hosted counties, visits >= 100,000",
    venue_distance = paste0("venue distance ", bin_label, " km"),
    venue_distance_visits_100k = paste0(
      "venue distance ", bin_label, " km, visits >= 100,000"
    ),
    spec_label
  )
}

world_fairs_output_dir <- function(results_dir, spec_label,
                                   bin_label = NA_character_) {
  if (is.na(bin_label) || !nzchar(bin_label)) {
    return(file.path(results_dir, spec_label))
  }
  file.path(
    results_dir,
    spec_label,
    paste0("bin_", stringr::str_replace_all(bin_label, "-", "_"), "km")
  )
}

plot_dynamic_att <- function(data, title, n_events, n_treated_units,
                             n_control_units, event_time_min = -20,
                             event_time_max = 50, y_limits = NULL) {
  plot_data <- data %>%
    dplyr::mutate(
      period = factor(
        ifelse(event_time < 0, "Pre", "Post"),
        levels = c("Pre", "Post")
      )
    )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = event_time,
      y = estimate,
      ymin = ci_low,
      ymax = ci_high
    )
  ) +
    ggplot2::geom_point(ggplot2::aes(colour = period), size = 1.5, na.rm = TRUE) +
    ggplot2::geom_errorbar(
      ggplot2::aes(colour = period),
      width = 0.1,
      na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = seq(event_time_min, event_time_max, 10)) +
    ggplot2::scale_color_manual(
      drop = FALSE,
      values = c(Pre = "#e87d72", Post = "#56bcc2"),
      breaks = c("Pre", "Post"),
      labels = c("Pre", "Post")
    ) +
    ggplot2::labs(
      x = "Relative Time",
      y = "Effect",
      colour = NULL,
      title = stringr::str_wrap(title, 72)
    ) +
    ggplot2::coord_cartesian(
      xlim = c(event_time_min, event_time_max),
      ylim = y_limits
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        color = "darkgray",
        face = "bold",
        size = 12
      ),
      axis.title = ggplot2::element_text(
        color = "darkgray",
        face = "bold",
        size = 12
      ),
      legend.position = "bottom"
    ) +
    ggplot2::annotate(
      "label",
      x = Inf,
      y = Inf,
      hjust = 1.05,
      vjust = 1.05,
      label = sprintf(
        "Events: %d\nTreated units: %d\nControl units: %d",
        n_events,
        n_treated_units,
        n_control_units
      ),
      size = 3,
      label.padding = grid::unit(0.4, "lines"),
      label.r = grid::unit(0, "lines")
    )
}
