###############################################################################
# Build event-specific donor pools for the six Figure 5 US county world fairs.
#
# Each standard/alternative timing case receives 30 donors selected with the
# preferred UK matching rule: three pre-period outcome lags, population growth,
# and baseline density, all z-scored on that event's eligible donor universe.
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(tigris)
})

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]), winslash = "/", mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", "..", ".."),
    winslash = "/", mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE
  )
}
source(file.path(repo_root, "paths.R"))
source(file.path(
  repo_root, "analysis", "world_fairs", "synthdid",
  "us_county_amws_synthdid_spec.R"
))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  candidate <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(candidate)) TALENT_DETS_DATA_DIR <- candidate
}
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  stop("Data root does not exist: ", TALENT_DETS_DATA_DIR)
}

profiles <- wf_us_sd_profile_specs()
variant <- wf_us_sd_variant_id()
profiles[, source_results_dir := file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", results_subdir
)]
panel_file <- file.path(
  TALENT_DETS_DATA_DIR, "output", "us_panel_county_amws_combined_year.csv"
)
results_root <- wf_us_sd_output_root(TALENT_DETS_DATA_DIR, variant)
results_dir <- file.path(results_root, "donor_pools")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

candidate_file <- file.path(results_dir, "donor_candidate_audit.csv")
pairs_file <- file.path(results_dir, "donor_pool_pairs.csv")
summary_file <- file.path(results_dir, "donor_pool_summary.csv")
columns_file <- file.path(results_dir, "donor_pool_matching_columns.csv")
components_file <- file.path(results_dir, "donor_pool_matching_components.csv")
overlap_file <- file.path(results_dir, "donor_pool_standard_alternative_overlap.csv")
manifest_file <- file.path(results_dir, "treated_event_manifest.csv")
qc_file <- file.path(results_dir, "run_validation.csv")
input_manifest_file <- file.path(results_dir, "input_manifest.csv")

required_files <- c(panel_file)
for (root in profiles$source_results_dir) {
  required_files <- c(required_files, file.path(root, c(
    "run_validation.csv", "venue_quality_audit.csv",
    "hosted/treatment_assignment.csv", "hosted/never_treated_units.csv"
  )))
}
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing required inputs:\n", paste(missing_files, collapse = "\n"))
}

coerce_logical <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("true", "1", "yes", "y")
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sum_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

assert_unique <- function(x, columns, label) {
  if (anyDuplicated(x[, ..columns])) {
    stop(label, " is not unique by ", paste(columns, collapse = ", "), ".")
  }
}

build_decade_panel <- function(panel_year, definition) {
  x <- copy(panel_year)
  x[, decade := wf_us_sd_decade(year, definition)]
  x <- x[, .(
    n_amws = sum_or_na(n_amws),
    population = mean_or_na(population),
    county_births_estimate = sum_or_na(county_births_estimate_year)
  ), by = .(GEOID, decade)]
  x[, outcome_value := fifelse(
    is.finite(county_births_estimate) & county_births_estimate > 0,
    1000 * n_amws / county_births_estimate,
    NA_real_
  )]
  assert_unique(x, c("GEOID", "decade"), paste(definition, "decade panel"))
  x[]
}

build_event_covariates <- function(
    ids, panel, treatment_cohort, area_lookup, required_event_times) {
  required_decades <- treatment_cohort + required_event_times
  x <- panel[GEOID %in% ids & decade %in% required_decades]
  x[, event_time := as.integer(decade - treatment_cohort)]

  completeness <- x[, .(
    observed_periods = .N,
    complete_window = .N == length(required_event_times) &&
      all(required_event_times %in% event_time) &&
      all(is.finite(outcome_value)) &&
      all(is.finite(population) & population > 0) &&
      all(is.finite(county_births_estimate) & county_births_estimate > 0)
  ), by = GEOID]

  lag_wide <- dcast(
    x[event_time %in% wf_us_sd_matching_event_times(),
      .(GEOID, event_time, outcome_value)],
    GEOID ~ event_time, value.var = "outcome_value"
  )
  lag_names <- c("-30" = "outcome_m30", "-20" = "outcome_m20",
                 "-10" = "outcome_m10")
  for (old in names(lag_names)) {
    if (!old %in% names(lag_wide)) lag_wide[, (old) := NA_real_]
  }
  setnames(lag_wide, names(lag_names), unname(lag_names))

  population_values <- dcast(
    x[event_time %in% c(-30L, -10L), .(GEOID, event_time, population)],
    GEOID ~ event_time, value.var = "population"
  )
  for (old in c("-30", "-10")) {
    if (!old %in% names(population_values)) {
      population_values[, (old) := NA_real_]
    }
  }
  setnames(
    population_values, c("-30", "-10"),
    c("population_m30", "population_m10")
  )

  out <- data.table(GEOID = ids)
  out <- merge(out, completeness, by = "GEOID", all.x = TRUE, sort = FALSE)
  out <- merge(out, lag_wide, by = "GEOID", all.x = TRUE, sort = FALSE)
  out <- merge(out, population_values, by = "GEOID", all.x = TRUE, sort = FALSE)
  out <- merge(out, area_lookup, by = "GEOID", all.x = TRUE, sort = FALSE)
  out[is.na(observed_periods), `:=`(observed_periods = 0L, complete_window = FALSE)]
  out[, population_cagr_m30_m10 := fifelse(
    is.finite(population_m30) & population_m30 > 0 &
      is.finite(population_m10) & population_m10 > 0,
    (population_m10 / population_m30)^(1 / 20) - 1,
    NA_real_
  )]
  out[, population_density_m10 := fifelse(
    is.finite(population_m10) & population_m10 > 0 &
      is.finite(fixed_2020_land_area_km2) & fixed_2020_land_area_km2 > 0,
    population_m10 / fixed_2020_land_area_km2,
    NA_real_
  )]
  out[, valid_matching_covariates :=
    complete_window &
      is.finite(outcome_m30) & outcome_m30 >= 0 &
      is.finite(outcome_m20) & outcome_m20 >= 0 &
      is.finite(outcome_m10) & outcome_m10 >= 0 &
      is.finite(population_cagr_m30_m10) & population_cagr_m30_m10 > -1 &
      is.finite(population_density_m10) & population_density_m10 > 0]
  out[]
}

message("Reading and validating Figure 5 source results...")
source_objects <- list()
treated_manifests <- list()
for (i in seq_len(nrow(profiles))) {
  profile <- profiles[i]
  definition <- profile$decade_definition
  root <- profile$source_results_dir
  validation <- fread(file.path(root, "run_validation.csv"))
  validation[, passed := coerce_logical(passed)]
  if (!nrow(validation) || any(!validation$passed)) {
    stop("Source Figure 5 profile failed validation: ", profile$profile_name)
  }

  treated <- fread(file.path(root, "hosted", "treatment_assignment.csv"))
  treated[, `:=`(
    GEOID = sprintf("%05d", as.integer(GEOID)),
    g = as.integer(g),
    first_parent_fair_id = as.integer(first_parent_fair_id),
    first_exposure_year = as.integer(first_exposure_year),
    eligible = coerce_logical(eligible)
  )]
  treated <- treated[eligible == TRUE]
  expected <- wf_us_sd_expected_timing()[decade_definition == definition]
  timing_check <- merge(
    treated[, .(GEOID, treatment_cohort = g)], expected,
    by = c("GEOID", "treatment_cohort"), all = TRUE
  )
  if (nrow(treated) != 6L || nrow(timing_check) != 6L ||
      anyNA(timing_check$decade_definition)) {
    stop("Unexpected treated sample for ", definition, " timing.")
  }
  assert_unique(treated, "GEOID", paste(definition, "treated assignments"))
  treated[, `:=`(
    decade_definition = definition,
    treatment_cohort = g,
    model_id = wf_us_sd_model_id(definition, GEOID, g)
  )]

  never <- fread(file.path(root, "hosted", "never_treated_units.csv"))
  never[, GEOID := sprintf("%05d", as.integer(GEOID))]
  assert_unique(never, "GEOID", paste(definition, "never-host counties"))
  if (nrow(never) != 3071L) {
    stop("Expected 3,071 never-host counties under ", definition,
         " timing; found ", nrow(never), ".")
  }

  venues <- fread(file.path(root, "venue_quality_audit.csv"))
  venues[, `:=`(
    parent_fair_id = as.integer(parent_fair_id),
    year_start = as.integer(year_start),
    venue_used_all = coerce_logical(venue_used_all),
    venue_latitude = as.numeric(venue_latitude),
    venue_longitude = as.numeric(venue_longitude)
  )]

  source_objects[[definition]] <- list(
    treated = treated, never = never, venues = venues,
    profile_name = profile$profile_name, source_results_dir = root
  )
  treated_manifests[[definition]] <- treated
}
treated_manifest <- rbindlist(treated_manifests, fill = TRUE, use.names = TRUE)
treated_manifest <- merge(
  treated_manifest,
  wf_us_sd_expected_windows(variant),
  by = c("decade_definition", "GEOID", "treatment_cohort"),
  all.x = TRUE, sort = FALSE
)
if (anyNA(treated_manifest$event_time_min) ||
    anyNA(treated_manifest$event_time_max)) {
  stop("Failed to attach the ", variant, " model windows to treated events.")
}
treated_manifest[, `:=`(
  first_model_decade = treatment_cohort + event_time_min,
  last_model_decade = treatment_cohort + event_time_max,
  reference_event_time = wf_us_sd_reference_event_time(),
  treatment_start_event_time = wf_us_sd_treatment_start_event_time(),
  fair_event_time = wf_us_sd_fair_event_time()
)]
setorder(treated_manifest, decade_definition, treatment_cohort, GEOID)

standard_never <- source_objects$standard$never$GEOID
alternative_never <- source_objects$alternative$never$GEOID
if (!identical(sort(standard_never), sort(alternative_never))) {
  stop("Standard and alternative timing must use the same never-host universe.")
}

message("Reading and aggregating the consolidated AMWS panel...")
panel_year <- fread(
  panel_file, na.strings = c("", "NA"),
  select = c("GEOID", "year", "population", "county_births_estimate_year", "n_amws")
)
panel_year[, `:=`(
  GEOID = sprintf("%05d", as.integer(GEOID)),
  year = as.integer(year),
  population = as.numeric(population),
  county_births_estimate_year = as.numeric(county_births_estimate_year),
  n_amws = as.numeric(n_amws)
)]
if (anyDuplicated(panel_year[, .(GEOID, year)])) {
  stop("The consolidated AMWS panel has duplicate GEOID-year rows.")
}
panels <- lapply(
  c("standard", "alternative"),
  function(definition) build_decade_panel(panel_year, definition)
)
names(panels) <- c("standard", "alternative")

message("Loading fixed 2020 county geometry and land area...")
county_sf <- tigris::counties(
  cb = TRUE, resolution = "20m", year = 2020, class = "sf",
  progress_bar = FALSE
)
county_sf <- county_sf[as.integer(county_sf$STATEFP) <= 56, ]
county_sf$GEOID <- as.character(county_sf$GEOID)
county_sf <- st_transform(st_make_valid(county_sf), 5070)
area_lookup <- as.data.table(st_drop_geometry(county_sf))[, .(
  GEOID,
  county_name_2020 = NAMELSAD,
  fixed_2020_land_area_km2 = as.numeric(ALAND) / 1e6
)]
assert_unique(area_lookup, "GEOID", "2020 county area lookup")

all_candidates <- list()
all_pairs <- list()
all_summaries <- list()
all_columns <- list()
all_components <- list()
feature_spec <- wf_us_sd_feature_spec()
feature_columns <- feature_spec$feature
matching_specification <- wf_us_sd_matching_specification()
tolerance <- 1e-10

for (definition in c("standard", "alternative")) {
  source <- source_objects[[definition]]
  panel <- panels[[definition]]
  never_ids <- source$never$GEOID
  for (event_index in seq_len(nrow(source$treated))) {
    treated <- source$treated[event_index]
    model_id <- treated$model_id
    g <- treated$treatment_cohort
    window <- wf_us_sd_model_window(definition, treated$GEOID, g, variant)
    event_times <- window$event_times[[1L]]
    message("Building donor pool: ", model_id)

    candidate_covariates <- build_event_covariates(
      never_ids, panel, g, area_lookup, event_times
    )
    treated_covariates <- build_event_covariates(
      treated$GEOID, panel, g, area_lookup, event_times
    )
    if (nrow(treated_covariates) != 1L ||
        !treated_covariates$valid_matching_covariates) {
      stop("Treated matching covariates are incomplete for ", model_id, ".")
    }
    if (identical(variant, "max_pre")) {
      prior_decade <- g + min(event_times) - 10L
      prior_row <- panel[GEOID == treated$GEOID & decade == prior_decade]
      prior_supported <- nrow(prior_row) == 1L &&
        is.finite(prior_row$outcome_value) &&
        is.finite(prior_row$population) && prior_row$population > 0 &&
        is.finite(prior_row$county_births_estimate) &&
        prior_row$county_births_estimate > 0
      if (prior_supported) {
        stop("max_pre does not start at the earliest supported decade for ",
             model_id, ".")
      }
    }

    event_venues <- source$venues[
      parent_fair_id == treated$first_parent_fair_id &
        venue_used_all == TRUE & is.finite(venue_latitude) &
        is.finite(venue_longitude)
    ]
    if (!nrow(event_venues)) {
      stop("No reviewed event venues found for ", model_id, ".")
    }
    venue_points <- st_as_sf(
      event_venues, coords = c("venue_longitude", "venue_latitude"),
      crs = 4326, remove = FALSE
    )
    venue_points <- st_transform(venue_points, 5070)
    candidate_geometry <- county_sf[county_sf$GEOID %in% never_ids, ]
    distance_matrix <- st_distance(candidate_geometry, venue_points)
    distance_lookup <- data.table(
      GEOID = candidate_geometry$GEOID,
      distance_from_event_venue_km = apply(
        as.matrix(distance_matrix), 1L,
        function(x) min(as.numeric(x))
      ) / 1000
    )
    candidate_covariates <- merge(
      candidate_covariates, distance_lookup, by = "GEOID",
      all.x = TRUE, sort = FALSE
    )
    candidate_covariates <- merge(
      candidate_covariates,
      source$never[, .(GEOID, donor_county_name = target_unit_name)],
      by = "GEOID", all.x = TRUE, sort = FALSE
    )
    candidate_covariates[, outside_distance_exclusion :=
      is.finite(distance_from_event_venue_km) &
        distance_from_event_venue_km >= wf_us_sd_donor_min_distance_km()]
    candidate_covariates[, eligible_donor :=
      valid_matching_covariates & outside_distance_exclusion]
    candidate_covariates[, exclusion_reason := fcase(
      !valid_matching_covariates, "invalid_matching_covariates",
      !is.finite(distance_from_event_venue_km), "missing_venue_distance",
      distance_from_event_venue_km < wf_us_sd_donor_min_distance_km(),
        "within_event_venue_exclusion",
      default = "eligible"
    )]

    eligible <- candidate_covariates[eligible_donor == TRUE]
    expected_eligible <- wf_us_sd_expected_eligible_donors(variant)[
      decade_definition == definition & GEOID == treated$GEOID,
      expected_eligible_donors
    ]
    if (length(expected_eligible) != 1L || nrow(eligible) != expected_eligible) {
      stop("Eligible donor count changed for ", model_id, ": expected ",
           expected_eligible, ", found ", nrow(eligible), ".")
    }
    if (nrow(eligible) < wf_us_sd_donor_count()) {
      stop("Fewer than 30 eligible donors for ", model_id, ".")
    }

    eligible[, `:=`(
      transformed_outcome_m30 = log1p(outcome_m30),
      transformed_outcome_m20 = log1p(outcome_m20),
      transformed_outcome_m10 = log1p(outcome_m10),
      transformed_population_growth = log1p(population_cagr_m30_m10),
      transformed_density = log(population_density_m10)
    )]
    treated_features <- c(
      log1p(treated_covariates$outcome_m30),
      log1p(treated_covariates$outcome_m20),
      log1p(treated_covariates$outcome_m10),
      log1p(treated_covariates$population_cagr_m30_m10),
      log(treated_covariates$population_density_m10)
    )
    names(treated_features) <- feature_columns
    donor_features <- as.matrix(eligible[, ..feature_columns])
    feature_mean <- colMeans(donor_features)
    feature_sd <- apply(donor_features, 2L, sd)
    if (any(!is.finite(feature_mean)) || any(!is.finite(feature_sd)) ||
        any(feature_sd <= 0) || any(!is.finite(treated_features))) {
      stop("All five matching columns must be finite and non-degenerate for ",
           model_id, ".")
    }
    donor_z <- sweep(sweep(donor_features, 2L, feature_mean, "-"),
                     2L, feature_sd, "/")
    treated_z <- (treated_features - feature_mean) / feature_sd
    z_gap <- sweep(donor_z, 2L, treated_z, "-")
    feature_GEOIDs <- data.table::copy(eligible$GEOID)
    eligible[, `:=`(
      outcome_lag_distance_component = sqrt(rowSums(z_gap[, 1:3, drop = FALSE]^2)),
      growth_distance_component = abs(z_gap[, 4L]),
      density_distance_component = abs(z_gap[, 5L]),
      match_distance = sqrt(rowSums(z_gap^2))
    )]
    setorder(eligible, match_distance, GEOID)
    eligible[, donor_rank := seq_len(.N)]
    eligible[, selected_donor := donor_rank <= wf_us_sd_donor_count()]

    score_columns <- c(
      "GEOID", feature_columns,
      "outcome_lag_distance_component", "growth_distance_component",
      "density_distance_component", "match_distance", "donor_rank",
      "selected_donor"
    )
    candidate_covariates <- merge(
      candidate_covariates, eligible[, ..score_columns],
      by = "GEOID", all.x = TRUE, sort = FALSE
    )
    candidate_covariates[, `:=`(
      matching_specification = matching_specification,
      decade_definition = definition,
      model_id = model_id,
      treated_GEOID = treated$GEOID,
      treated_county_name = treated$target_unit_name,
      fair_parent_id = treated$first_parent_fair_id,
      fair_name = treated$first_fair_name,
      fair_year = treated$first_exposure_year,
      treatment_cohort = g,
      variant = variant,
      event_time_min = window$event_time_min,
      event_time_max = window$event_time_max,
      n_pre_periods = window$n_pre_periods,
      n_post_periods = window$n_post_periods,
      total_periods = window$total_periods,
      reference_event_time = wf_us_sd_reference_event_time(),
      treatment_start_event_time = wf_us_sd_treatment_start_event_time(),
      fair_event_time = wf_us_sd_fair_event_time(),
      donor_min_distance_km = wf_us_sd_donor_min_distance_km()
    )]

    selected <- candidate_covariates[selected_donor == TRUE]
    if (nrow(selected) != wf_us_sd_donor_count() ||
        anyDuplicated(selected$GEOID) ||
        any(selected$distance_from_event_venue_km + tolerance <
              wf_us_sd_donor_min_distance_km())) {
      stop("Selected donor-pool validation failed for ", model_id, ".")
    }
    setnames(selected, "GEOID", "donor_GEOID")

    columns_audit <- copy(feature_spec)
    columns_audit[, `:=`(
      matching_specification = matching_specification,
      decade_definition = definition,
      model_id = model_id,
      treated_GEOID = treated$GEOID,
      treatment_cohort = g,
      variant = variant,
      event_time_min = window$event_time_min,
      event_time_max = window$event_time_max,
      n_pre_periods = window$n_pre_periods,
      n_post_periods = window$n_post_periods,
      eligible_donors = nrow(eligible),
      donor_mean = as.numeric(feature_mean),
      donor_sd = as.numeric(feature_sd),
      treated_value = as.numeric(treated_features),
      treated_z = as.numeric(treated_z),
      used_in_distance = TRUE
    )]

    component_long <- data.table(
      donor_GEOID = rep(feature_GEOIDs, times = nrow(feature_spec)),
      feature = rep(feature_spec$feature, each = nrow(eligible)),
      feature_group = rep(feature_spec$feature_group, each = nrow(eligible)),
      donor_value = as.numeric(donor_features),
      donor_z = as.numeric(donor_z),
      treated_z = rep(as.numeric(treated_z), each = nrow(eligible)),
      z_gap = as.numeric(z_gap)
    )
    component_long <- component_long[
      donor_GEOID %in% selected$donor_GEOID
    ]
    component_long <- merge(
      component_long,
      selected[, .(donor_GEOID, donor_rank, donor_county_name)],
      by = "donor_GEOID", all.x = TRUE, sort = FALSE
    )
    component_long[, `:=`(
      matching_specification = matching_specification,
      decade_definition = definition,
      model_id = model_id,
      treated_GEOID = treated$GEOID,
      treatment_cohort = g
    )]

    summary_row <- data.table(
      matching_specification = matching_specification,
      decade_definition = definition,
      model_id = model_id,
      treated_GEOID = treated$GEOID,
      treated_county_name = treated$target_unit_name,
      fair_parent_id = treated$first_parent_fair_id,
      fair_name = treated$first_fair_name,
      fair_year = treated$first_exposure_year,
      treatment_cohort = g,
      variant = variant,
      event_time_min = window$event_time_min,
      event_time_max = window$event_time_max,
      n_pre_periods = window$n_pre_periods,
      n_post_periods = window$n_post_periods,
      total_periods = window$total_periods,
      reference_event_time = wf_us_sd_reference_event_time(),
      treatment_start_event_time = wf_us_sd_treatment_start_event_time(),
      fair_event_time = wf_us_sd_fair_event_time(),
      candidate_never_hosts = nrow(candidate_covariates),
      complete_window_candidates = candidate_covariates[complete_window == TRUE, .N],
      candidates_excluded_invalid_covariates = candidate_covariates[
        exclusion_reason == "invalid_matching_covariates", .N
      ],
      candidates_excluded_within_50km = candidate_covariates[
        exclusion_reason == "within_event_venue_exclusion", .N
      ],
      eligible_donors = nrow(eligible),
      selected_donors = nrow(selected),
      nearest_match_distance = min(selected$match_distance),
      thirtieth_match_distance = max(selected$match_distance),
      minimum_selected_venue_distance_km =
        min(selected$distance_from_event_venue_km),
      treated_outcome_m30 = treated_covariates$outcome_m30,
      treated_outcome_m20 = treated_covariates$outcome_m20,
      treated_outcome_m10 = treated_covariates$outcome_m10,
      treated_population_cagr_m30_m10 =
        treated_covariates$population_cagr_m30_m10,
      treated_population_density_m10 =
        treated_covariates$population_density_m10
    )

    key <- model_id
    all_candidates[[key]] <- candidate_covariates
    all_pairs[[key]] <- selected
    all_summaries[[key]] <- summary_row
    all_columns[[key]] <- columns_audit
    all_components[[key]] <- component_long
  }
}

candidate_audit <- rbindlist(all_candidates, fill = TRUE, use.names = TRUE)
selected_pairs <- rbindlist(all_pairs, fill = TRUE, use.names = TRUE)
pool_summary <- rbindlist(all_summaries, fill = TRUE, use.names = TRUE)
matching_columns <- rbindlist(all_columns, fill = TRUE, use.names = TRUE)
matching_components <- rbindlist(all_components, fill = TRUE, use.names = TRUE)

setorder(candidate_audit, decade_definition, treatment_cohort, treated_GEOID,
         selected_donor, donor_rank, GEOID)
setorder(selected_pairs, decade_definition, treatment_cohort, treated_GEOID,
         donor_rank)
setorder(pool_summary, decade_definition, treatment_cohort, treated_GEOID)
setorder(matching_columns, decade_definition, treatment_cohort, treated_GEOID,
         feature_index)
setorder(matching_components, decade_definition, treatment_cohort,
         treated_GEOID, donor_rank, feature)

standard_pairs <- selected_pairs[decade_definition == "standard", .(
  treated_GEOID, donor_GEOID, standard_rank = donor_rank
)]
alternative_pairs <- selected_pairs[decade_definition == "alternative", .(
  treated_GEOID, donor_GEOID, alternative_rank = donor_rank
)]
pair_overlap <- merge(
  standard_pairs, alternative_pairs,
  by = c("treated_GEOID", "donor_GEOID"), all = TRUE
)
overlap_summary <- pair_overlap[, .(
  standard_donors = sum(!is.na(standard_rank)),
  alternative_donors = sum(!is.na(alternative_rank)),
  common_donors = sum(!is.na(standard_rank) & !is.na(alternative_rank)),
  union_donors = .N,
  jaccard_overlap = sum(!is.na(standard_rank) & !is.na(alternative_rank)) / .N
), by = treated_GEOID]
setorder(overlap_summary, treated_GEOID)

expected_models <- 12L
expected_pairs <- expected_models * wf_us_sd_donor_count()
component_check <- matching_components[, .(
  recomputed_distance = sqrt(sum(z_gap^2))
), by = .(model_id, donor_GEOID)]
component_check <- merge(
  component_check,
  selected_pairs[, .(model_id, donor_GEOID, match_distance)],
  by = c("model_id", "donor_GEOID")
)
max_component_error <- max(abs(
  component_check$recomputed_distance - component_check$match_distance
))
rank_check <- selected_pairs[, .(
  ranks_ok = identical(as.integer(donor_rank), seq_len(.N)),
  distances_nondecreasing = all(diff(match_distance) >= -tolerance)
), by = model_id]

legacy_pool_match <- TRUE
legacy_pool_detail <- "Not applicable to max_pre"
if (identical(variant, "fixed_m30")) {
  legacy_pairs_file <- file.path(
    wf_us_sd_legacy_output_root(TALENT_DETS_DATA_DIR),
    "donor_pools", "donor_pool_pairs.csv"
  )
  if (!file.exists(legacy_pairs_file)) {
    legacy_pool_match <- FALSE
    legacy_pool_detail <- "Legacy donor_pool_pairs.csv is missing"
  } else {
    legacy_pairs <- fread(legacy_pairs_file, na.strings = c("", "NA"))
    legacy_pairs[, `:=`(
      donor_GEOID = sprintf("%05d", as.integer(donor_GEOID)),
      donor_rank = as.integer(donor_rank)
    )]
    comparison_columns <- c("model_id", "donor_GEOID", "donor_rank")
    legacy_pool_match <- identical(
      legacy_pairs[, ..comparison_columns],
      selected_pairs[, ..comparison_columns]
    ) && max(abs(
      legacy_pairs$match_distance - selected_pairs$match_distance
    )) <= 1e-12
    legacy_pool_detail <- if (legacy_pool_match) {
      "All 360 donor IDs, ranks, and matching distances reproduce legacy"
    } else {
      "Corrected fixed-window donor selections differ from legacy"
    }
  }
}

validation <- data.table(
  check = c(
    "source_profiles_validated", "treated_models_match_figure5",
    "never_host_universe_identical", "eligible_donor_counts_match",
    "selected_pool_rows_complete", "selected_donors_unique_within_model",
    "selected_donors_outside_50km", "five_matching_columns_per_model",
    "matching_components_reproduce_distance", "donor_ranks_deterministic",
    "fixed_window_donor_pools_reproduce_legacy"
  ),
  passed = c(
    TRUE,
    nrow(treated_manifest) == expected_models &&
      uniqueN(treated_manifest$model_id) == expected_models,
    identical(sort(standard_never), sort(alternative_never)),
    all(pool_summary$eligible_donors ==
          wf_us_sd_expected_eligible_donors(variant)[
            pool_summary,
            on = .(decade_definition, GEOID = treated_GEOID),
            x.expected_eligible_donors
          ]),
    nrow(selected_pairs) == expected_pairs &&
      all(pool_summary$selected_donors == wf_us_sd_donor_count()),
    selected_pairs[, all(uniqueN(donor_GEOID) == wf_us_sd_donor_count()),
                   by = model_id][, all(V1)],
    all(selected_pairs$distance_from_event_venue_km + tolerance >=
          wf_us_sd_donor_min_distance_km()),
    matching_columns[, all(.N == 5L & used_in_distance), by = model_id][, all(V1)],
    is.finite(max_component_error) && max_component_error <= 1e-8,
    all(rank_check$ranks_ok & rank_check$distances_nondecreasing),
    legacy_pool_match
  ),
  detail = c(
    "Both Figure 5 run_validation.csv files pass",
    paste(nrow(treated_manifest), "of", expected_models),
    paste(length(standard_never), "never-host GEOIDs"),
    paste(range(pool_summary$eligible_donors), collapse = "-"),
    paste(nrow(selected_pairs), "of", expected_pairs),
    paste(uniqueN(selected_pairs$model_id), "validated pools"),
    paste0("minimum ", round(min(selected_pairs$distance_from_event_venue_km), 3), " km"),
    paste(nrow(matching_columns), "rows"),
    format(max_component_error, scientific = TRUE),
    paste(nrow(rank_check), "validated pools"),
    legacy_pool_detail
  )
)

input_manifest <- data.table(
  path = normalizePath(required_files, winslash = "/", mustWork = TRUE),
  bytes = as.numeric(file.info(required_files)$size),
  md5 = unname(tools::md5sum(required_files))
)

fwrite(candidate_audit, candidate_file, na = "")
fwrite(selected_pairs, pairs_file, na = "")
fwrite(pool_summary, summary_file, na = "")
fwrite(matching_columns, columns_file, na = "")
fwrite(matching_components, components_file, na = "")
fwrite(overlap_summary, overlap_file, na = "")
fwrite(treated_manifest, manifest_file, na = "")
fwrite(validation, qc_file, na = "")
fwrite(input_manifest, input_manifest_file, na = "")

notes <- c(
  "US county AMWS non-pooled synthetic-DiD donor pools",
  paste0("Variant: ", variant, "."),
  paste0("Matching specification: ", matching_specification, "."),
  paste0("Outcome used for matching: ", wf_us_sd_outcome_name(), "."),
  paste0("Event-window range across cases: e=[",
         min(pool_summary$event_time_min), ",+",
         max(pool_summary$event_time_max), "]."),
  paste0("Reference e=", wf_us_sd_reference_event_time(),
         "; treatment begins e=", wf_us_sd_treatment_start_event_time(),
         "; fair occurs e=+", wf_us_sd_fair_event_time(), "."),
  paste0("Matching lags: e=", paste(wf_us_sd_matching_event_times(), collapse = ","), "."),
  "Population CAGR spans e=-30 to e=-10; density is measured at e=-10.",
  paste0("Selected donors per event: ", wf_us_sd_donor_count(), "."),
  paste0("Minimum distance from any reviewed event venue: ",
         wf_us_sd_donor_min_distance_km(), " km."),
  paste0("Candidate controls are never-host counties with complete ", variant,
         " event-specific support."),
  "Fixed 2020 Census land area (ALAND) supplies the density denominator.",
  paste0("Results: ", normalizePath(results_dir, winslash = "/", mustWork = TRUE))
)
writeLines(notes, file.path(results_dir, "notes.txt"))

if (any(!validation$passed)) {
  print(validation)
  stop("US county AMWS donor-pool validation failed; see ", qc_file)
}

message("US county AMWS donor pools complete.")
message("  Models: ", nrow(pool_summary))
message("  Selected pairs: ", nrow(selected_pairs))
message("  Eligible donors: ", min(pool_summary$eligible_donors), "--",
        max(pool_summary$eligible_donors))
message("  Outputs: ", results_dir)
