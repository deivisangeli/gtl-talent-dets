###############################################################################
# Shared helpers for world's-fairs synthetic DiD analyses.
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(synthdid)
  library(sf)
  library(tigris)
})

wf_value_or <- function(x, default) {
  if (is.null(x)) default else x
}

wf_mean_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

wf_first_nonmissing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0L) return(NA_character_)
  as.character(x[[1L]])
}

wf_standard_decade <- function(year) {
  as.integer(floor(year / 10) * 10)
}

wf_sanitize_filename <- function(x) {
  str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

wf_pad_geoid <- function(x) {
  x_chr <- as.character(x)
  if_else(
    is.na(x_chr) | x_chr == "",
    NA_character_,
    str_pad(str_replace(x_chr, "\\.0$", ""), 5, pad = "0")
  )
}

wf_bool_env <- function(name, default = FALSE) {
  default_chr <- if (default) "true" else "false"
  tolower(Sys.getenv(name, unset = default_chr)) %in% c("1", "true", "yes", "y")
}

wf_int_env <- function(name, default) {
  value <- as.integer(Sys.getenv(name, unset = as.character(default)))
  if (is.na(value) || value < 0L) {
    stop(name, " must be a non-negative integer.")
  }
  value
}

wf_outcomes <- function(default_outcomes) {
  env <- Sys.getenv("SYNTHDID_OUTCOMES", unset = "")
  if (env == "") return(default_outcomes)
  requested <- str_split(env, ",", simplify = FALSE)[[1]] %>%
    str_trim() %>%
    discard(~ .x == "")
  bad <- setdiff(requested, default_outcomes)
  if (length(bad) > 0L) {
    stop("Unknown SYNTHDID_OUTCOMES: ", paste(bad, collapse = ", "))
  }
  requested
}

wf_selected_bins <- function(available_bins) {
  env <- Sys.getenv("SYNTHDID_BINS", unset = "")
  if (env == "") return(available_bins)
  requested <- str_split(env, ",", simplify = FALSE)[[1]] %>%
    str_trim() %>%
    discard(~ .x == "")
  bad <- setdiff(requested, available_bins)
  if (length(bad) > 0L) {
    stop("Unknown SYNTHDID_BINS: ", paste(bad, collapse = ", "))
  }
  requested
}

wf_build_uk_target_geometries <- function(boundary_gpkg, greater_london_crosswalk_file,
                                          greater_london_id, target_types) {
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

wf_build_us_target_geometries <- function(panel_year) {
  us_units <- panel_year %>%
    filter(iso3 == "USA") %>%
    distinct(unit_id, GEOID, place_name) %>%
    mutate(GEOID = wf_pad_geoid(GEOID)) %>%
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

wf_load_conservative_venues <- function(fairs_file, classification_year_min,
                                        classification_year_max,
                                        visits_threshold = NULL) {
  fairs <- fread(fairs_file, na.strings = c("", "NA")) %>%
    as_tibble()

  if (!"parent_fair_id" %in% names(fairs)) fairs$parent_fair_id <- fairs$fair_id
  if (!"venue_seq" %in% names(fairs)) fairs$venue_seq <- 1L
  if (!is.null(visits_threshold) && !"visits" %in% names(fairs)) {
    stop("Missing required visits column in fairs file: ", fairs_file)
  }

  fairs <- fairs %>%
    mutate(
      fair_id = as.integer(fair_id),
      parent_fair_id = as.integer(parent_fair_id),
      venue_seq = as.integer(venue_seq),
      year_start = as.integer(year_start),
      visits_num = if ("visits" %in% names(.)) suppressWarnings(as.numeric(visits)) else NA_real_,
      host_matched_country_iso3 = as.character(host_matched_country_iso3),
      venue_longitude = as.numeric(venue_longitude),
      venue_latitude = as.numeric(venue_latitude),
      venue_coordinates_note = as.character(venue_coordinates_note)
    )

  if (!is.null(visits_threshold)) {
    fair_visits_by_parent <- fairs %>%
      group_by(parent_fair_id) %>%
      summarise(
        fair_visits = {
          values <- visits_num[!is.na(visits_num)]
          if (length(values) == 0L) NA_real_ else max(values)
        },
        .groups = "drop"
      )
    fairs <- fairs %>% left_join(fair_visits_by_parent, by = "parent_fair_id")
  } else {
    fairs <- fairs %>% mutate(fair_visits = NA_real_)
    if (!"visits_measure" %in% names(fairs)) fairs$visits_measure <- NA_character_
  }

  venue_audit <- fairs %>%
    filter(
      year_start >= classification_year_min,
      year_start <= classification_year_max,
      host_matched_country_iso3 %in% c("GBR", "USA")
    ) %>%
    mutate(
      fair_has_visits = !is.na(fair_visits),
      fair_visits_ge_threshold = if (is.null(visits_threshold)) TRUE else fair_has_visits & fair_visits >= visits_threshold,
      excluded_missing_visits = if (is.null(visits_threshold)) FALSE else !fair_has_visits,
      excluded_below_visits_threshold = if (is.null(visits_threshold)) FALSE else fair_has_visits & fair_visits < visits_threshold,
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
        fair_visits_ge_threshold & venue_coordinates_used_conservative
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

wf_classify_exposure <- function(first_exposure_year, treated_event_year_min,
                                 treated_event_year_max, classification_year_max) {
  case_when(
    first_exposure_year < treated_event_year_min ~ "always_treated_pre_1840",
    first_exposure_year >= treated_event_year_min &
      first_exposure_year <= treated_event_year_max ~ "treated",
    first_exposure_year > treated_event_year_max &
      first_exposure_year <= classification_year_max ~ "future_treated_after_1910",
    TRUE ~ "outside_classification_window"
  )
}

wf_build_distance_exposure_one_country <- function(targets_sf, venues_country,
                                                   max_distance_m, bin_breaks,
                                                   bin_labels,
                                                   treated_event_year_min,
                                                   treated_event_year_max,
                                                   classification_year_max) {
  target_dt <- as_tibble(st_drop_geometry(targets_sf)) %>%
    select(unit_id, target_unit_id, target_unit_name, target_area_type,
           target_boundary_id, geo_country_iso3, GEOID)

  if (nrow(venues_country) == 0L) {
    return(list(
      audit = tibble(),
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

  hit_index <- which(distance_matrix <= max_distance_m, arr.ind = TRUE)
  if (nrow(hit_index) == 0L) {
    return(list(
      audit = tibble(),
      first_exposure = tibble(),
      never_units = target_dt %>% mutate(exposure_status = "never_treated"),
      always_units = tibble(),
      future_units = tibble()
    ))
  }

  audit <- tibble(
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
      first_exposure_decade = wf_standard_decade(first_exposure_year),
      distance_bin_km = as.character(distance_bin_km),
      first_distance_km = distance_km,
      first_fair_id = fair_id,
      first_parent_fair_id = parent_fair_id,
      first_venue_seq = venue_seq,
      first_fair_name = Fair_name,
      first_fair_city = City,
      first_fair_country = Country,
      first_fair_venue = venue,
      first_fair_visits = if ("fair_visits" %in% names(audit)) fair_visits else NA_real_,
      exposure_status = wf_classify_exposure(
        first_exposure_year,
        treated_event_year_min,
        treated_event_year_max,
        classification_year_max
      )
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

wf_build_host_exposure_one_country <- function(targets_sf, venues_country,
                                               treatment_spec,
                                               treated_event_year_min,
                                               treated_event_year_max,
                                               classification_year_max) {
  target_dt <- as_tibble(st_drop_geometry(targets_sf)) %>%
    select(unit_id, target_unit_id, target_unit_name, target_area_type,
           target_boundary_id, geo_country_iso3, GEOID)

  if (nrow(venues_country) == 0L) {
    return(list(
      audit = tibble(),
      first_exposure = tibble(),
      never_units = target_dt %>% mutate(exposure_status = "never_treated"),
      always_units = tibble(),
      future_units = tibble()
    ))
  }

  venue_points <- venues_country %>%
    st_as_sf(coords = c("venue_longitude", "venue_latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(st_crs(targets_sf))

  hit_index <- which(st_intersects(targets_sf, venue_points, sparse = FALSE), arr.ind = TRUE)
  if (nrow(hit_index) == 0L) {
    return(list(
      audit = tibble(),
      first_exposure = tibble(),
      never_units = target_dt %>% mutate(exposure_status = "never_treated"),
      always_units = tibble(),
      future_units = tibble()
    ))
  }

  audit <- tibble(
    target_row = hit_index[, 1],
    venue_row = hit_index[, 2]
  ) %>%
    bind_cols(target_dt[.$target_row, ]) %>%
    bind_cols(venues_country[.$venue_row, ]) %>%
    mutate(treatment_spec = treatment_spec, hosted_unit = TRUE) %>%
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
      first_exposure_decade = wf_standard_decade(first_exposure_year),
      treatment_spec = treatment_spec,
      first_fair_id = fair_id,
      first_parent_fair_id = parent_fair_id,
      first_venue_seq = venue_seq,
      first_fair_name = Fair_name,
      first_fair_city = City,
      first_fair_country = Country,
      first_fair_venue = venue,
      first_fair_visits = if ("fair_visits" %in% names(audit)) fair_visits else NA_real_,
      exposure_status = wf_classify_exposure(
        first_exposure_year,
        treated_event_year_min,
        treated_event_year_max,
        classification_year_max
      )
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

wf_bind_country_exposure <- function(uk_exposure, us_exposure) {
  list(
    audit = bind_rows(uk_exposure$audit, us_exposure$audit),
    first_exposure = bind_rows(uk_exposure$first_exposure, us_exposure$first_exposure),
    never_units = bind_rows(uk_exposure$never_units, us_exposure$never_units),
    always_units = bind_rows(uk_exposure$always_units, us_exposure$always_units),
    future_units = bind_rows(uk_exposure$future_units, us_exposure$future_units)
  )
}

wf_fit_one_cohort <- function(data, outcome, spec_label, cohort, event_window,
                              att_placebo_replications,
                              curve_placebo_replications,
                              rng_seed) {
  data_es <- data %>%
    select(unit_id, iso3, decade, g, all_of(outcome)) %>%
    rename(y = all_of(outcome)) %>%
    mutate(
      unit_id = as.character(unit_id),
      decade = as.integer(decade),
      g = as.integer(g),
      y = as.numeric(y)
    )

  stack_min <- max(min(data_es$decade, na.rm = TRUE), cohort - event_window)
  stack_max <- min(max(data_es$decade, na.rm = TRUE), cohort + event_window)

  treated_units <- data_es %>%
    distinct(unit_id, g) %>%
    filter(g == cohort) %>%
    pull(unit_id)

  control_units <- data_es %>%
    distinct(unit_id, g) %>%
    filter(g == 0L | g > stack_max) %>%
    pull(unit_id)

  stack <- data_es %>%
    filter(
      decade >= stack_min,
      decade <= stack_max,
      unit_id %in% c(treated_units, control_units)
    ) %>%
    mutate(treatment = as.integer(unit_id %in% treated_units & decade >= cohort))

  all_periods <- sort(unique(stack$decade))
  complete_units <- stack %>%
    group_by(unit_id) %>%
    summarise(
      n_periods = n_distinct(decade),
      n_valid_y = sum(!is.na(y) & is.finite(y)),
      .groups = "drop"
    ) %>%
    filter(n_periods == length(all_periods), n_valid_y == length(all_periods)) %>%
    pull(unit_id)

  stack <- stack %>%
    filter(unit_id %in% complete_units, !is.na(y), is.finite(y))

  n_treated <- n_distinct(stack$unit_id[stack$unit_id %in% treated_units])
  n_control <- n_distinct(stack$unit_id[stack$unit_id %in% control_units])

  fail_result <- function(error) {
    list(
      ok = FALSE,
      status = tibble(
        outcome = outcome,
        spec_label = spec_label,
        cohort = cohort,
        ok = FALSE,
        n_rows = nrow(stack),
        n_units = n_distinct(stack$unit_id),
        n_treated_units = n_treated,
        n_control_units = n_control,
        stack_min_decade = stack_min,
        stack_max_decade = stack_max,
        error = error
      )
    )
  }

  if (n_treated == 0L) return(fail_result("No treated units in complete stack."))
  if (n_control == 0L) return(fail_result("No eligible controls in complete stack."))
  if (length(all_periods[all_periods < cohort]) == 0L) return(fail_result("No pre-treatment periods."))
  if (length(all_periods[all_periods >= cohort]) == 0L) return(fail_result("No post-treatment periods."))
  if (n_distinct(stack$y, na.rm = TRUE) < 2L) return(fail_result("Outcome has insufficient variation."))

  tryCatch({
    setup <- synthdid::panel.matrices(
      stack %>%
        select(unit_id, decade, y, treatment) %>%
        as.data.frame(),
      unit = "unit_id",
      time = "decade",
      outcome = "y",
      treatment = "treatment",
      treated.last = TRUE
    )

    estimate_obj <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
    att <- as.numeric(estimate_obj)
    att_se <- if (att_placebo_replications > 0L) {
      tryCatch(
        sqrt(as.numeric(vcov(
          estimate_obj,
          method = "placebo",
          replications = att_placebo_replications
        ))),
        error = function(e) NA_real_
      )
    } else {
      NA_real_
    }

    time_periods <- as.integer(colnames(setup$Y))
    post_periods <- time_periods[(setup$T0 + 1L):length(time_periods)]
    curve <- as.numeric(synthdid::synthdid_effect_curve(estimate_obj))
    if (length(curve) != length(post_periods)) {
      post_periods <- seq_len(length(curve)) + cohort
    }

    curve_tbl <- tibble(
      outcome = outcome,
      spec_label = spec_label,
      cohort = cohort,
      decade = as.integer(post_periods),
      event_time = as.integer(post_periods - cohort),
      effect = curve,
      n_treated_units = nrow(setup$Y) - setup$N0,
      n_control_units = setup$N0
    )

    placebo_curve <- tibble()
    if (curve_placebo_replications > 0L && setup$N0 > (nrow(setup$Y) - setup$N0)) {
      n1 <- nrow(setup$Y) - setup$N0
      control_index <- seq_len(setup$N0)
      y_control <- setup$Y[control_index, , drop = FALSE]

      placebo_curve <- map_dfr(seq_len(curve_placebo_replications), function(rep_id) {
        set.seed(rng_seed + as.integer(cohort) * 1000L + rep_id)
        placebo_treated <- sample(control_index, size = n1, replace = FALSE)
        placebo_controls <- setdiff(control_index, placebo_treated)
        y_placebo <- rbind(
          y_control[placebo_controls, , drop = FALSE],
          y_control[placebo_treated, , drop = FALSE]
        )
        n0_placebo <- length(placebo_controls)
        est_placebo <- tryCatch(
          synthdid::synthdid_estimate(y_placebo, n0_placebo, setup$T0),
          error = function(e) NULL
        )
        if (is.null(est_placebo)) return(tibble())

        placebo_effect <- as.numeric(synthdid::synthdid_effect_curve(est_placebo))
        if (length(placebo_effect) != length(post_periods)) return(tibble())

        tibble(
          outcome = outcome,
          spec_label = spec_label,
          cohort = cohort,
          placebo_rep = rep_id,
          decade = as.integer(post_periods),
          event_time = as.integer(post_periods - cohort),
          placebo_effect = placebo_effect,
          n_treated_units = n1
        )
      })
    }

    weights_omega <- tryCatch(
      synthdid::synthdid_controls(estimate_obj, mass = 1, weight.type = "omega") %>%
        as.data.frame() %>%
        rownames_to_column("control_unit_id") %>%
        as_tibble() %>%
        rename(weight = 2) %>%
        mutate(outcome = outcome, spec_label = spec_label, cohort = cohort, weight_type = "omega", .before = 1),
      error = function(e) tibble()
    )

    weights_lambda <- tryCatch(
      synthdid::synthdid_controls(estimate_obj, mass = 1, weight.type = "lambda") %>%
        as.data.frame() %>%
        rownames_to_column("pre_period") %>%
        as_tibble() %>%
        rename(weight = 2) %>%
        mutate(outcome = outcome, spec_label = spec_label, cohort = cohort, weight_type = "lambda", .before = 1),
      error = function(e) tibble()
    )

    list(
      ok = TRUE,
      status = tibble(
        outcome = outcome,
        spec_label = spec_label,
        cohort = cohort,
        ok = TRUE,
        n_rows = nrow(stack),
        n_units = n_distinct(stack$unit_id),
        n_treated_units = n_treated,
        n_control_units = n_control,
        stack_min_decade = stack_min,
        stack_max_decade = stack_max,
        error = NA_character_
      ),
      att = tibble(
        outcome = outcome,
        spec_label = spec_label,
        cohort = cohort,
        estimate = att,
        se = att_se,
        ci_low = att - 1.96 * att_se,
        ci_high = att + 1.96 * att_se,
        p_value = if_else(is.finite(att_se) & att_se > 0, 2 * (1 - pnorm(abs(att / att_se))), NA_real_),
        se_method = if (att_placebo_replications > 0L) "placebo" else "not_computed",
        se_replications = if (att_placebo_replications > 0L) att_placebo_replications else NA_integer_,
        n_treated_units = n_treated,
        n_control_units = n_control,
        stack_min_decade = stack_min,
        stack_max_decade = stack_max
      ),
      curve = curve_tbl,
      placebo_curve = placebo_curve,
      control_weights = weights_omega,
      time_weights = weights_lambda
    )
  }, error = function(e) fail_result(conditionMessage(e)))
}

wf_aggregate_curves <- function(curve_tbl, placebo_tbl) {
  if (nrow(curve_tbl) == 0L) return(tibble())

  observed <- curve_tbl %>%
    group_by(outcome, spec_label, event_time) %>%
    summarise(
      estimate = weighted.mean(effect, w = n_treated_units, na.rm = TRUE),
      n_cohorts = n_distinct(cohort),
      n_treated_weight = sum(n_treated_units, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(placebo_tbl) == 0L) {
    return(observed %>% mutate(se = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }

  placebo_agg <- placebo_tbl %>%
    group_by(outcome, spec_label, placebo_rep, event_time) %>%
    summarise(
      placebo_estimate = weighted.mean(placebo_effect, w = n_treated_units, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(outcome, spec_label, event_time) %>%
    summarise(se = sd(placebo_estimate, na.rm = TRUE), .groups = "drop")

  observed %>%
    left_join(placebo_agg, by = c("outcome", "spec_label", "event_time")) %>%
    mutate(
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se
    )
}

wf_plot_dynamic <- function(dynamic_tbl, outcome, spec_label, title_prefix) {
  plot_data <- dynamic_tbl %>%
    filter(outcome == !!outcome, spec_label == !!spec_label)

  ggplot(plot_data, aes(x = event_time, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey45") +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.18, fill = "#1f78b4") +
    geom_line(linewidth = 0.7, color = "#1f78b4") +
    geom_point(size = 2, color = "#1f78b4") +
    labs(
      x = "Relative time (years)",
      y = "Synthetic DiD effect",
      title = str_wrap(paste(title_prefix, spec_label, outcome), width = 78),
      caption = "Bands are pointwise 95% intervals from placebo curves where available."
    ) +
    theme_minimal(base_size = 12)
}

wf_plot_att_by_spec <- function(simple_att, outcome, title_prefix) {
  plot_data <- simple_att %>%
    filter(outcome == !!outcome) %>%
    mutate(spec_label = factor(spec_label, levels = unique(spec_label)))

  ggplot(plot_data, aes(x = spec_label, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, na.rm = TRUE, color = "#2f4f4f") +
    geom_point(size = 2.4, color = "#1f78b4") +
    labs(
      x = "Specification",
      y = "Weighted mean cohort ATT",
      title = str_wrap(paste(title_prefix, "ATT by specification", outcome), width = 78),
      caption = "Bars show approximate 95% intervals using weighted cohort SEs."
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}

wf_run_stacked_synthdid_for_spec <- function(panel_decade, treated_units, never_units,
                                             first_exposure, always_units,
                                             future_units, results_dir,
                                             spec_label, spec_dir,
                                             outcomes, event_window,
                                             title_prefix,
                                             extra_audit = NULL) {
  spec_out_dir <- file.path(results_dir, spec_dir)
  dir.create(spec_out_dir, recursive = TRUE, showWarnings = FALSE)

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

  panel_spec <- panel_decade %>%
    semi_join(analysis_units, by = c("unit_id", "target_unit_id")) %>%
    left_join(analysis_units, by = c("unit_id", "target_unit_id")) %>%
    mutate(unit_num = as.integer(factor(unit_id)))

  write_csv(treated_units, file.path(spec_out_dir, "treatment_assignment.csv"))
  if (!is.null(extra_audit)) write_csv(extra_audit, file.path(spec_out_dir, "treatment_match_audit.csv"))

  sample_summary <- panel_spec %>%
    summarise(
      spec_label = spec_label,
      n_rows = n(),
      n_units = n_distinct(unit_id),
      n_periods = n_distinct(decade),
      min_decade = min(decade, na.rm = TRUE),
      max_decade = max(decade, na.rm = TRUE),
      n_treated_units = n_distinct(unit_id[g > 0]),
      n_control_units = n_distinct(unit_id[g == 0]),
      n_treated_cohorts = n_distinct(g[g > 0]),
      n_always_treated_excluded = nrow(always_units),
      n_future_treated_after_1910_excluded = nrow(future_units),
      .groups = "drop"
    )
  write_csv(sample_summary, file.path(spec_out_dir, "sample_summary.csv"))

  cohorts <- sort(unique(panel_spec$g[panel_spec$g > 0]))
  att_reps <- wf_int_env("SYNTHDID_ATT_PLACEBO_REPLICATIONS", 10L)
  curve_reps <- wf_int_env("SYNTHDID_CURVE_PLACEBO_REPLICATIONS", 10L)
  rng_seed <- as.integer(Sys.getenv("SYNTHDID_SEED", unset = "20260702"))

  model_results <- list()
  for (outcome in outcomes) {
    message("  outcome: ", outcome)
    for (cohort in cohorts) {
      model_results[[paste(outcome, cohort, sep = "__")]] <- wf_fit_one_cohort(
        data = panel_spec,
        outcome = outcome,
        spec_label = spec_label,
        cohort = cohort,
        event_window = event_window,
        att_placebo_replications = att_reps,
        curve_placebo_replications = curve_reps,
        rng_seed = rng_seed
      )
    }
  }

  model_status <- map_dfr(model_results, "status")
  cohort_att <- map_dfr(keep(model_results, "ok"), "att")
  cohort_curve <- map_dfr(keep(model_results, "ok"), "curve")
  placebo_curve <- map_dfr(keep(model_results, "ok"), "placebo_curve")
  control_weights <- map_dfr(keep(model_results, "ok"), "control_weights")
  time_weights <- map_dfr(keep(model_results, "ok"), "time_weights")

  dynamic_att <- wf_aggregate_curves(cohort_curve, placebo_curve)

  simple_att <- cohort_att %>%
    group_by(outcome, spec_label) %>%
    summarise(
      estimate = weighted.mean(estimate, w = n_treated_units, na.rm = TRUE),
      se = if (all(is.na(se))) {
        NA_real_
      } else {
        sqrt(sum((n_treated_units * se)^2, na.rm = TRUE)) / sum(n_treated_units, na.rm = TRUE)
      },
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se,
      n_cohorts = n_distinct(cohort),
      n_treated_weight = sum(n_treated_units, na.rm = TRUE),
      .groups = "drop"
    )

  write_csv(model_status, file.path(spec_out_dir, "model_status.csv"))
  write_csv(cohort_att, file.path(spec_out_dir, "synthdid_cohort_att.csv"))
  write_csv(cohort_curve, file.path(spec_out_dir, "synthdid_cohort_effect_curve.csv"))
  write_csv(placebo_curve, file.path(spec_out_dir, "synthdid_placebo_effect_curve.csv"))
  write_csv(dynamic_att, file.path(spec_out_dir, "synthdid_dynamic_att.csv"))
  write_csv(simple_att, file.path(spec_out_dir, "synthdid_simple_att.csv"))
  write_csv(control_weights, file.path(spec_out_dir, "synthdid_control_weights.csv"))
  write_csv(time_weights, file.path(spec_out_dir, "synthdid_time_weights.csv"))

  if (nrow(dynamic_att) > 0L) {
    for (outcome in unique(dynamic_att$outcome)) {
      ggsave(
        file.path(spec_out_dir, paste0("SDID_event_study_", wf_sanitize_filename(outcome), "_", wf_sanitize_filename(spec_label), ".png")),
        wf_plot_dynamic(dynamic_att, outcome, spec_label, title_prefix),
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  }

  if (nrow(simple_att) > 0L) {
    for (outcome in unique(simple_att$outcome)) {
      ggsave(
        file.path(spec_out_dir, paste0("SDID_att_", wf_sanitize_filename(outcome), "_", wf_sanitize_filename(spec_label), ".png")),
        wf_plot_att_by_spec(simple_att, outcome, title_prefix),
        width = 8,
        height = 5.5,
        dpi = 300
      )
    }
  }

  list(
    sample_summary = sample_summary,
    model_status = model_status,
    cohort_att = cohort_att,
    dynamic_att = dynamic_att,
    simple_att = simple_att
  )
}

run_worlds_fairs_synthdid <- function(spec_type = c("venue_distance", "ever_hosted"),
                                      visits_threshold = NULL,
                                      results_subdir,
                                      repo_root) {
  spec_type <- match.arg(spec_type)
  initial_time <- Sys.time()
  options(timeout = 1000, tigris_use_cache = TRUE)
  sf_use_s2(FALSE)

  source(file.path(repo_root, "paths.R"))
  if (!dir.exists(TALENT_DETS_DATA_DIR)) {
    user_data_dir <- file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets")
    if (dir.exists(user_data_dir)) {
      TALENT_DETS_DATA_DIR <- normalizePath(user_data_dir, winslash = "/", mustWork = TRUE)
      DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
      DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
    }
  }

  data_processed <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed")
  results_dir <- file.path(TALENT_DETS_DATA_DIR, "results", "worlds_fair", "synthdid", results_subdir)
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

  gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
  panel_file <- file.path(
    data_processed,
    "uk_historical_urban_units_us_county_inventor_panel_1800_1960_nhgis_us.csv"
  )
  fairs_file <- file.path(DATA_INPUT, "worlds_fairs", "worlds_fairs_1790_1960_with_visits_venues.csv")
  boundary_gpkg <- file.path(gbr_dir, "raw", "historical_boundaries", "uk_historical_districts_1921_1961.gpkg")
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
  event_window <- wf_int_env("SYNTHDID_EVENT_WINDOW", 50L)
  max_distance_m <- 10000
  bin_breaks <- c(-1e-9, 2, 4, 6, 8, 10)
  bin_labels <- c("0-2", "2-4", "4-6", "6-8", "8-10")
  analysis_bin_labels <- c(bin_labels, "0-10")
  bin_dirs <- c(
    "0-2" = "bin_0_2km",
    "2-4" = "bin_2_4km",
    "4-6" = "bin_4_6km",
    "6-8" = "bin_6_8km",
    "8-10" = "bin_8_10km",
    "0-10" = "bin_0_10km"
  )
  selected_bin_labels <- wf_selected_bins(analysis_bin_labels)
  selected_bins_active <- spec_type == "venue_distance" &&
    !identical(selected_bin_labels, analysis_bin_labels)
  root_suffix <- if (selected_bins_active) "_selected_bins" else "_all_specs"
  outcomes <- wf_outcomes(c(
    "inventors_per_100k_pop",
    "stem_per_100k_pop",
    "n_inventors",
    "log1p_n_inventors",
    "n_stem",
    "log1p_n_stem",
    "population",
    "log_population"
  ))

  message("Reading UK+US panel...")
  panel_year <- fread(panel_file, na.strings = c("", "NA")) %>%
    as_tibble() %>%
    mutate(
      unit_id = as.character(unit_id),
      GEOID = wf_pad_geoid(GEOID),
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
    filter(iso3 %in% c("GBR", "USA"), year >= panel_year_min, year <= panel_year_max)

  eligible_units <- panel_year %>%
    filter(year >= treated_event_year_min, year <= panel_year_max) %>%
    group_by(unit_id, iso3) %>%
    summarise(has_any_population = any(!is.na(population)), .groups = "drop") %>%
    filter(has_any_population)

  panel_year <- panel_year %>% semi_join(eligible_units, by = c("unit_id", "iso3"))

  reuse_treatment <- wf_bool_env("SYNTHDID_REUSE_TREATMENT", FALSE)
  exposure_files <- c(
    first_exposure = file.path(
      results_dir,
      if (spec_type == "venue_distance") "first_exposure_all_bins.csv" else "first_exposure_hosted.csv"
    ),
    never_units = file.path(results_dir, "never_treated_units.csv"),
    always_units = file.path(results_dir, "always_treated_pre_1840_units.csv"),
    future_units = file.path(results_dir, "future_treated_after_1910_units.csv"),
    audit = file.path(
      results_dir,
      if (spec_type == "venue_distance") "venue_distance_match_audit_all_bins.csv" else "hosted_match_audit.csv"
    )
  )

  if (reuse_treatment && all(file.exists(exposure_files))) {
    message("Reusing saved treatment exposure files...")
    first_exposure <- read_csv(exposure_files[["first_exposure"]], show_col_types = FALSE)
    never_units <- read_csv(exposure_files[["never_units"]], show_col_types = FALSE)
    always_units <- read_csv(exposure_files[["always_units"]], show_col_types = FALSE)
    future_units <- read_csv(exposure_files[["future_units"]], show_col_types = FALSE)
    exposure <- list(
      audit = read_csv(exposure_files[["audit"]], show_col_types = FALSE),
      first_exposure = first_exposure,
      never_units = never_units,
      always_units = always_units,
      future_units = future_units
    )
  } else {
    message("Building UK historical urban-unit geometries...")
    uk_targets <- wf_build_uk_target_geometries(
      boundary_gpkg,
      greater_london_crosswalk_file,
      greater_london_id,
      target_types
    ) %>%
      semi_join(eligible_units %>% filter(iso3 == "GBR"), by = "unit_id")

    message("Building US county geometries...")
    us_targets <- wf_build_us_target_geometries(panel_year) %>%
      semi_join(eligible_units %>% filter(iso3 == "USA"), by = "unit_id")

    message("Loading venue data...")
    venue_data <- wf_load_conservative_venues(
      fairs_file,
      classification_year_min,
      classification_year_max,
      visits_threshold = visits_threshold
    )
    venues <- venue_data$venues
    write_csv(venue_data$audit, file.path(results_dir, "venue_quality_audit.csv"))

    message("Building treatment exposure...")
    if (spec_type == "venue_distance") {
      uk_exposure <- wf_build_distance_exposure_one_country(
        uk_targets,
        venues %>% filter(host_matched_country_iso3 == "GBR"),
        max_distance_m,
        bin_breaks,
        bin_labels,
        treated_event_year_min,
        treated_event_year_max,
        classification_year_max
      )
      us_exposure <- wf_build_distance_exposure_one_country(
        us_targets,
        venues %>% filter(host_matched_country_iso3 == "USA"),
        max_distance_m,
        bin_breaks,
        bin_labels,
        treated_event_year_min,
        treated_event_year_max,
        classification_year_max
      )
    } else {
      treatment_spec <- "hosted"
      uk_exposure <- wf_build_host_exposure_one_country(
        uk_targets,
        venues %>% filter(host_matched_country_iso3 == "GBR"),
        treatment_spec,
        treated_event_year_min,
        treated_event_year_max,
        classification_year_max
      )
      us_exposure <- wf_build_host_exposure_one_country(
        us_targets,
        venues %>% filter(host_matched_country_iso3 == "USA"),
        treatment_spec,
        treated_event_year_min,
        treated_event_year_max,
        classification_year_max
      )
    }

    exposure <- wf_bind_country_exposure(uk_exposure, us_exposure)
    first_exposure <- exposure$first_exposure
    never_units <- exposure$never_units
    always_units <- exposure$always_units
    future_units <- exposure$future_units

    write_csv(exposure$audit, exposure_files[["audit"]])
    write_csv(first_exposure, exposure_files[["first_exposure"]])
    write_csv(never_units, exposure_files[["never_units"]])
    write_csv(always_units, exposure_files[["always_units"]])
    write_csv(future_units, exposure_files[["future_units"]])
  }

  message("Aggregating annual panel to decades...")
  panel_decade_base <- panel_year %>%
    mutate(decade = wf_standard_decade(year)) %>%
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
      population = wf_mean_or_na(population),
      source_panel = wf_first_nonmissing(source_panel),
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

  root_results <- list()
  title_prefix <- paste(
    "World's fairs",
    if (spec_type == "venue_distance") "venue-distance stacked synthdid" else "ever-hosted stacked synthdid"
  )

  if (spec_type == "venue_distance") {
    for (bin_label in selected_bin_labels) {
      message("Running bin ", bin_label, "...")
      treated_units <- first_exposure %>%
        filter(
          exposure_status == "treated",
          if (bin_label == "0-10") distance_bin_km %in% bin_labels else distance_bin_km == bin_label,
          first_exposure_year >= treated_event_year_min,
          first_exposure_year <= treated_event_year_max
        ) %>%
        transmute(
          unit_id,
          target_unit_id,
          geo_country_iso3,
          event_year = first_exposure_year,
          g = first_exposure_decade,
          spec_label = bin_label,
          source_distance_bin_km = distance_bin_km,
          first_distance_km,
          first_fair_id = as.character(first_fair_id),
          first_parent_fair_id = as.character(first_parent_fair_id),
          first_fair_name,
          first_fair_city,
          first_fair_country,
          first_fair_venue
        )

      audit_bin <- if (bin_label == "0-10") {
        exposure$audit %>% filter(distance_bin_km %in% bin_labels)
      } else {
        exposure$audit %>% filter(distance_bin_km == bin_label)
      }

      root_results[[bin_label]] <- wf_run_stacked_synthdid_for_spec(
        panel_decade = panel_decade_base,
        treated_units = treated_units,
        never_units = never_units,
        first_exposure = first_exposure,
        always_units = always_units,
        future_units = future_units,
        results_dir = results_dir,
        spec_label = bin_label,
        spec_dir = bin_dirs[[bin_label]],
        outcomes = outcomes,
        event_window = event_window,
        title_prefix = title_prefix,
        extra_audit = audit_bin
      )
    }
  } else {
    message("Running hosted spec...")
    treated_units <- first_exposure %>%
      filter(
        exposure_status == "treated",
        first_exposure_year >= treated_event_year_min,
        first_exposure_year <= treated_event_year_max
      ) %>%
      transmute(
        unit_id,
        target_unit_id,
        geo_country_iso3,
        event_year = first_exposure_year,
        g = first_exposure_decade,
        spec_label = "hosted",
        first_fair_id = as.character(first_fair_id),
        first_parent_fair_id = as.character(first_parent_fair_id),
        first_fair_name,
        first_fair_city,
        first_fair_country,
        first_fair_venue
      )

    root_results[["hosted"]] <- wf_run_stacked_synthdid_for_spec(
      panel_decade = panel_decade_base,
      treated_units = treated_units,
      never_units = never_units,
      first_exposure = first_exposure,
      always_units = always_units,
      future_units = future_units,
      results_dir = results_dir,
      spec_label = "hosted",
      spec_dir = "hosted",
      outcomes = outcomes,
      event_window = event_window,
      title_prefix = title_prefix,
      extra_audit = exposure$audit
    )
  }

  all_sample_summary <- map_dfr(root_results, "sample_summary")
  all_model_status <- map_dfr(root_results, "model_status")
  all_cohort_att <- map_dfr(root_results, "cohort_att")
  all_dynamic_att <- map_dfr(root_results, "dynamic_att")
  all_simple_att <- map_dfr(root_results, "simple_att")

  write_csv(all_sample_summary, file.path(results_dir, paste0("sample_summary", root_suffix, ".csv")))
  write_csv(all_model_status, file.path(results_dir, paste0("model_status", root_suffix, ".csv")))
  write_csv(all_cohort_att, file.path(results_dir, paste0("synthdid_cohort_att", root_suffix, ".csv")))
  write_csv(all_dynamic_att, file.path(results_dir, paste0("synthdid_dynamic_att", root_suffix, ".csv")))
  write_csv(all_simple_att, file.path(results_dir, paste0("synthdid_simple_att", root_suffix, ".csv")))

  if (nrow(all_simple_att) > 0L) {
    for (outcome in unique(all_simple_att$outcome)) {
      ggsave(
        file.path(results_dir, paste0("SDID_att_by_spec_", wf_sanitize_filename(outcome), root_suffix, ".png")),
        wf_plot_att_by_spec(all_simple_att, outcome, title_prefix),
        width = 9,
        height = 5.5,
        dpi = 300
      )
    }
  }

  notes <- c(
    paste0("World's fairs stacked synthdid: ", spec_type),
    "",
    paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("TALENT_DETS_DATA_DIR: ", TALENT_DETS_DATA_DIR),
    paste0("Panel: ", panel_file),
    paste0("Fairs: ", fairs_file),
    paste0("Results directory: ", results_dir),
    paste0("Visits threshold: ", if (is.null(visits_threshold)) "none" else visits_threshold),
    paste0("Event window: +/-", event_window, " years."),
    paste0("Selected bins: ", if (spec_type == "venue_distance") paste(selected_bin_labels, collapse = ", ") else "hosted"),
    paste0("Outcomes: ", paste(outcomes, collapse = ", ")),
    paste0("ATT placebo replications: ", wf_int_env("SYNTHDID_ATT_PLACEBO_REPLICATIONS", 10L)),
    paste0("Curve placebo replications: ", wf_int_env("SYNTHDID_CURVE_PLACEBO_REPLICATIONS", 10L)),
    "Estimator: cohort-specific synthdid stacks aggregated by treated-unit counts.",
    "Controls by cohort: never-treated and not-yet-treated units not treated inside the stack window.",
    paste0("Elapsed minutes: ", round(difftime(Sys.time(), initial_time, units = "mins"), 1))
  )
  writeLines(notes, file.path(results_dir, "notes.txt"))

  message("Saved results in: ", results_dir)
  message("Done. Elapsed: ", round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
}
