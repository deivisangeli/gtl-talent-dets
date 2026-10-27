###############################################################################
# Build treated-LGD-specific donor pools for the fixed-1911 England and Wales
# Wikipedia synthetic-DiD analysis. Treatment can be the LGD's own first fair
# (the original design) or its first fair venue within a polygon-based radius.
#
# Two matching specifications are available, selected with
# SYNTHDID_MATCHING_SPEC and defined in ukds_1911_lgd_matching_spec.R.
#
# pooled_rate_density (default, historical layout):
#   - pooled annualized pre-treatment Wikipedia Discovery/Science births per
#     100,000 population;
#   - population density in the completed decade before treatment.
#
# lagged_rate_growth_density (written under match_v2_lagged/):
#   - one lagged-outcome column per pre-treatment decade bin, each the
#     annualized Wikipedia Discovery/Science rate per 100,000 population-year;
#   - the annualized pre-treatment population growth rate (CAGR) from the
#     first to the last pre-treatment bin;
#   - population density in the completed decade before treatment.
#
# balanced3_rate_growth_density (written under match_v3_balanced/):
#   - exactly three lagged-outcome columns, at T-30, T-20 and T-10 decades
#     relative to the fair decade T, so the fair decade itself never enters
#     donor selection;
#   - a uniform 20-year population growth rate (CAGR) over T-30 to T-10;
#   - population density at T-10, which is the existing baseline decade.
#   Five columns for every treated unit, so the equal-per-column weighting is
#   comparable across events. This specification implies the balanced
#   4-pre/3-post event window and restricts the fair decade to [1880, 1920].
#
# In both cases the distance is an equal-weight Euclidean distance over
# columns z-scored on the eligible-donor mean and standard deviation, and the
# treated unit is standardized with those same donor moments.
#
# Population level is retained for balance diagnostics but is not part of the
# matching distance. Candidate controls must never host a matched fair and
# must be at least 50 km from the treated LGD's first-fair venue by default.
#
# Run from the repository root:
#   Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
#
# Optional sensitivity overrides:
#   SYNTHDID_DONOR_COUNT=30
#   SYNTHDID_DONOR_MIN_DISTANCE_KM=50
#   SYNTHDID_COHORT_LEAD_YEARS=0
#   SYNTHDID_TREATMENT_MODE=host
#   SYNTHDID_TREATMENT_RADIUS_KM=50
#   SYNTHDID_MATCHING_SPEC=pooled_rate_density
#   SYNTHDID_EVENT_WINDOW=full
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})

sf_use_s2(FALSE)

###############################################################################
# Paths and configuration
###############################################################################

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
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/", mustWork = TRUE
  )
}
source(file.path(repo_root, "paths.R"))
source(file.path(
  repo_root, "analysis", "world_fairs", "synthdid",
  "ukds_1911_lgd_matching_spec.R"
))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  candidate <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(candidate)) TALENT_DETS_DATA_DIR <- candidate
}

processed_dir <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "processed", "worlds_fairs",
  "ukds_1911_boundary_crosswalk"
)
base_results_dir <- file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", "synthdid",
  "ukds_1911_lgd_wikipedia_donor_pools"
)

treatment_mode <- tolower(trimws(Sys.getenv(
  "SYNTHDID_TREATMENT_MODE", unset = "host"
)))
treatment_radius_km <- suppressWarnings(as.numeric(Sys.getenv(
  "SYNTHDID_TREATMENT_RADIUS_KM", unset = "50"
)))
if (!treatment_mode %in% c("host", "radius"))
  stop("SYNTHDID_TREATMENT_MODE must be host or radius.")
if (!is.finite(treatment_radius_km) || treatment_radius_km <= 0)
  stop("SYNTHDID_TREATMENT_RADIUS_KM must be positive.")
treatment_mode_value <- treatment_mode
treatment_radius_value <- if (treatment_mode == "radius") {
  treatment_radius_km
} else {
  NA_real_
}
radius_tag <- paste0(
  format(treatment_radius_km, trim = TRUE, scientific = FALSE), "km"
)
treatment_results_dir <- if (treatment_mode == "host") {
  base_results_dir
} else {
  file.path(base_results_dir, paste0("radius_", radius_tag))
}

cohort_lead_years <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_COHORT_LEAD_YEARS", unset = "0"
)))
if (is.na(cohort_lead_years) || cohort_lead_years < 0L)
  stop("SYNTHDID_COHORT_LEAD_YEARS must be a non-negative integer.")
matching_spec_id <- wf_matching_spec_id()
matching_specification <- wf_matching_spec_string(matching_spec_id)
matching_uses_lags <- wf_matching_spec_uses_lags(matching_spec_id)
matching_fixed_lags <- wf_matching_spec_fixed_lags(matching_spec_id)
event_window_id <- wf_event_window_id(matching_spec_id)
event_window <- wf_event_window_spec(event_window_id)
matching_results_dir <- wf_matching_spec_dir(
  treatment_results_dir, matching_spec_id
)
results_dir <- if (cohort_lead_years == 0L) matching_results_dir else file.path(
  matching_results_dir, paste0("cohort_lead_", cohort_lead_years, "y")
)

panel_files <- c(
  standard = file.path(
    processed_dir,
    "ukds_1911_lgd_world_fairs_panel_decade_standard_1851_1961.csv"
  ),
  alternative = file.path(
    processed_dir,
    "ukds_1911_lgd_world_fairs_panel_decade_alternative_1851_1961.csv"
  )
)
geometry_file <- file.path(
  processed_dir, "ukds_1911_lgd_analysis_geometries.gpkg"
)
fair_audit_file <- file.path(
  processed_dir, "ukds_1911_lgd_fair_treatment_audit.csv"
)

candidate_output <- file.path(results_dir, "donor_candidate_audit.csv")
selected_output <- file.path(results_dir, "donor_pool_pairs.csv")
summary_output <- file.path(results_dir, "donor_pool_summary.csv")
overlap_output <- file.path(
  results_dir, "donor_pool_standard_alternative_overlap.csv"
)
qc_output <- file.path(results_dir, "donor_pool_qc.csv")
timing_output <- file.path(results_dir, "cohort_timing_status.csv")
radius_match_output <- file.path(results_dir, "radius_lgd_fair_match_audit.csv")
radius_assignment_output <- file.path(
  results_dir, "radius_first_exposure_assignment.csv"
)
matching_columns_output <- file.path(
  results_dir, "donor_pool_matching_columns.csv"
)
matching_components_output <- file.path(
  results_dir, "donor_pool_matching_components.csv"
)

required <- c(panel_files, geometry_file, fair_audit_file)
missing <- required[!file.exists(required)]
if (length(missing)) stop("Missing inputs:\n", paste(missing, collapse = "\n"))
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

donor_count <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_DONOR_COUNT", unset = "30"
)))
donor_min_distance_km <- suppressWarnings(as.numeric(Sys.getenv(
  "SYNTHDID_DONOR_MIN_DISTANCE_KM", unset = "50"
)))
if (is.na(donor_count) || donor_count < 2L)
  stop("SYNTHDID_DONOR_COUNT must be an integer of at least two.")
if (!is.finite(donor_min_distance_km) || donor_min_distance_km < 0)
  stop("SYNTHDID_DONOR_MIN_DISTANCE_KM must be non-negative.")

analysis_crs <- 27700L
expected_lgds <- 1834L
tolerance <- 1e-10
# The balanced window fixes the number of matched lags, so a pool is only
# built when the complete window is available.
minimum_pre_periods <- if (matching_fixed_lags)
  length(event_window$matched_lag_offsets) else 2L

###############################################################################
# Helpers
###############################################################################

assert_unique <- function(x, columns, label) {
  if (anyDuplicated(x[, ..columns]))
    stop(label, " is not unique by ", paste(columns, collapse = ", "))
}

safe_ratio <- function(x, y) {
  ifelse(is.finite(x) & is.finite(y) & x > 0 & y > 0,
         pmax(x, y) / pmin(x, y), NA_real_)
}

standard_decade <- function(year) as.integer(floor(as.numeric(year) / 10) * 10)
alternative_decade <- function(year) {
  year <- as.integer(year)
  standard_decade(year) + ifelse(!is.na(year) & year %% 10L >= 7L, 10L, 0L)
}

min_distance_to_venues <- function(target_geometry, venue_points) {
  distances <- st_distance(target_geometry, venue_points)
  distance_matrix <- matrix(
    as.numeric(distances), nrow = nrow(target_geometry),
    ncol = nrow(venue_points)
  )
  if (ncol(distance_matrix) == 1L) {
    distance_matrix[, 1L] / 1000
  } else {
    apply(distance_matrix, 1L, min) / 1000
  }
}

build_radius_exposure <- function(targets_sf, fair_rows, radius_km) {
  venues <- copy(fair_rows[
    is.finite(venue_latitude) & is.finite(venue_longitude)
  ])
  if (!nrow(venues)) stop("No valid fair venues for radius treatment.")
  if ("analysis_lgd_id" %in% names(venues))
    setnames(venues, "analysis_lgd_id", "host_analysis_lgd_id")
  host_rename <- intersect(
    c("population_lgd_name", "population_lgd_type", "first_fair_year",
      "treatment_status"),
    names(venues)
  )
  if (length(host_rename))
    setnames(venues, host_rename, paste0("host_", host_rename))
  venue_points <- st_as_sf(
    venues, coords = c("venue_longitude", "venue_latitude"),
    crs = 4326, remove = FALSE
  )
  venue_points <- st_transform(venue_points, st_crs(targets_sf))
  distance_matrix <- matrix(
    as.numeric(st_distance(targets_sf, venue_points)) / 1000,
    nrow = nrow(targets_sf), ncol = nrow(venue_points)
  )
  hits <- which(distance_matrix <= radius_km + tolerance, arr.ind = TRUE)
  if (!nrow(hits)) stop("No LGDs lie within the requested fair radius.")

  target_rows <- as.data.table(st_drop_geometry(targets_sf))[hits[, 1L], .(
    analysis_lgd_id = as.character(analysis_lgd_id),
    population_lgd_name,
    population_lgd_type
  )]
  audit <- cbind(
    target_rows,
    data.table(distance_km = distance_matrix[hits]),
    venues[hits[, 2L]]
  )
  setorder(audit, analysis_lgd_id, event_year, distance_km,
           parent_fair_id, venue_seq)

  first <- audit[, .SD[1L], by = analysis_lgd_id]
  first <- first[, .(
    analysis_lgd_id,
    first_fair_year = event_year,
    first_distance_km = distance_km,
    first_fair_id = fair_id,
    first_parent_fair_id = parent_fair_id,
    first_fair_name = Fair_name,
    first_fair_city = City,
    first_fair_venue = venue,
    first_venue_latitude = venue_latitude,
    first_venue_longitude = venue_longitude,
    first_fair_match_status = fair_match_status
  )]
  target_lookup <- as.data.table(st_drop_geometry(targets_sf))[, .(
    analysis_lgd_id = as.character(analysis_lgd_id),
    population_lgd_name,
    population_lgd_type
  )]
  assignment <- merge(
    target_lookup, first, by = "analysis_lgd_id", all.x = TRUE, sort = FALSE
  )
  assignment[, treatment_status := fifelse(
    is.na(first_fair_year), "never_within_radius",
    fifelse(first_fair_year > 1870L, "treated_post1870",
            "pretreated_by1870")
  )]
  setorder(assignment, treatment_status, first_fair_year, analysis_lgd_id)
  list(audit = audit, assignment = assignment)
}

###############################################################################
# Fixed target geometry and treatment venue lookup
###############################################################################

message("Reading fixed-1911 LGD geometry and fair venues...")
targets <- st_read(
  geometry_file, layer = "fixed_1911_lgds", quiet = TRUE
)
targets <- st_transform(st_make_valid(targets), analysis_crs)
targets$analysis_lgd_id <- as.character(targets$analysis_lgd_id)
targets$area_km2 <- as.numeric(st_area(targets)) / 1e6

if (nrow(targets) != expected_lgds || any(!is.finite(targets$area_km2)) ||
    any(targets$area_km2 <= 0)) {
  stop("The fixed-1911 LGD geometry is incomplete or has invalid area.")
}
assert_unique(
  as.data.table(st_drop_geometry(targets)), "analysis_lgd_id",
  "Fixed-1911 LGD geometry"
)
area_lookup <- as.data.table(st_drop_geometry(targets))[, .(
  analysis_lgd_id,
  fixed_1911_area_km2 = area_km2
)]

fair_audit <- fread(fair_audit_file, na.strings = c("", "NA"))
fair_audit[, `:=`(
  analysis_lgd_id = as.character(analysis_lgd_id),
  event_year = as.integer(event_year),
  first_fair_year = as.integer(first_fair_year),
  venue_latitude = as.numeric(venue_latitude),
  venue_longitude = as.numeric(venue_longitude)
)]

if (treatment_mode == "radius") {
  radius_exposure <- build_radius_exposure(
    targets, fair_audit, treatment_radius_km
  )
  treatment_lookup <- radius_exposure$assignment[, .(
    analysis_lgd_id,
    analysis_first_fair_year = first_fair_year,
    analysis_treatment_status = treatment_status
  )]
  radius_treated_ids <- treatment_lookup[
    analysis_treatment_status == "treated_post1870", analysis_lgd_id
  ]
  first_venues <- radius_exposure$audit[
    analysis_lgd_id %in% radius_treated_ids,
    .SD[event_year == min(event_year)], by = analysis_lgd_id
  ]
  setnames(first_venues, "analysis_lgd_id", "treated_lgd_id")
  fwrite(radius_exposure$audit, radius_match_output, na = "")
  fwrite(radius_exposure$assignment, radius_assignment_output, na = "")
} else {
  treatment_lookup <- unique(fair_audit[
    !is.na(analysis_lgd_id), .(
      analysis_lgd_id,
      analysis_first_fair_year = first_fair_year,
      analysis_treatment_status = treatment_status
    )
  ])
  missing_hosts <- setdiff(targets$analysis_lgd_id, treatment_lookup$analysis_lgd_id)
  treatment_lookup <- rbind(
    treatment_lookup,
    data.table(
      analysis_lgd_id = as.character(missing_hosts),
      analysis_first_fair_year = NA_integer_,
      analysis_treatment_status = "never_host"
    )
  )
  first_venues <- copy(fair_audit[
    fair_match_status == "matched_unique_lgd" &
      treatment_status == "treated_post1870" &
      event_year == first_fair_year &
      is.finite(venue_latitude) & is.finite(venue_longitude)
  ])
  setnames(first_venues, "analysis_lgd_id", "treated_lgd_id")
}

status_counts <- treatment_lookup[, .N, by = analysis_treatment_status]
expected_treated <- status_counts[
  analysis_treatment_status == "treated_post1870", N
]
expected_pretreated <- status_counts[
  analysis_treatment_status == "pretreated_by1870", N
]
candidate_control_status <- if (treatment_mode == "radius") {
  "never_within_radius"
} else {
  "never_host"
}
expected_never <- status_counts[
  analysis_treatment_status == candidate_control_status, N
]
if (nrow(treatment_lookup) != expected_lgds ||
    anyDuplicated(treatment_lookup$analysis_lgd_id) ||
    sum(c(expected_treated, expected_pretreated, expected_never)) !=
      expected_lgds) {
  stop("Treatment assignment does not partition all fixed LGDs.")
}
if (treatment_mode == "host" &&
    !identical(c(expected_treated, expected_pretreated, expected_never),
               c(11L, 4L, 1819L))) {
  stop("Host treatment counts changed unexpectedly.")
}
if (treatment_mode == "radius" && treatment_radius_km == 50 &&
    !identical(c(expected_treated, expected_pretreated, expected_never),
               c(293L, 644L, 897L))) {
  stop("The 50 km treatment counts changed unexpectedly.")
}
if (uniqueN(first_venues$treated_lgd_id) != expected_treated)
  stop("Every treated LGD must have a first-exposure venue.")

###############################################################################
# Donor-pool construction
###############################################################################

all_candidates <- list()
all_selected <- list()
all_summaries <- list()
all_timing <- list()
all_matching_columns <- list()
all_matching_components <- list()

for (definition in names(panel_files)) {
  message("Building ", definition, "-decade donor pools...")
  panel <- fread(panel_files[[definition]], na.strings = c("", "NA"))
  panel[, `:=`(
    analysis_lgd_id = as.character(analysis_lgd_id),
    decade = as.integer(decade),
    n_inventors = as.integer(n_inventors),
    population = as.numeric(population),
    first_fair_year = as.integer(first_fair_year),
    g_standard = as.integer(g_standard),
    g_alternative = as.integer(g_alternative)
  )]
  if (nrow(panel) != expected_lgds * 12L ||
      anyDuplicated(panel[, .(analysis_lgd_id, decade)])) {
    stop(definition, " decade panel is not a balanced 1,834 by 12 grid.")
  }
  unit_names <- unique(panel[, .(
    analysis_lgd_id, population_lgd_name, population_lgd_type
  )])
  status <- merge(
    treatment_lookup, unit_names, by = "analysis_lgd_id",
    all.x = TRUE, sort = FALSE
  )
  treated <- status[analysis_treatment_status == "treated_post1870", .(
    treated_lgd_id = analysis_lgd_id,
    treated_lgd_name = population_lgd_name,
    treated_lgd_type = population_lgd_type,
    treated_first_fair_year = analysis_first_fair_year
  )]
  treated[, fair_treatment_decade := if (definition == "standard") {
    standard_decade(treated_first_fair_year)
  } else {
    alternative_decade(treated_first_fair_year)
  }]
  assert_unique(treated, "treated_lgd_id", paste(definition, "treated LGDs"))
  never_ids <- status[
    analysis_treatment_status == candidate_control_status, analysis_lgd_id
  ]

  for (treated_index in seq_len(nrow(treated))) {
    treated_row <- treated[treated_index]
    treated_id <- treated_row$treated_lgd_id
    effective_cohort_year <- treated_row$treated_first_fair_year -
      cohort_lead_years
    treatment_decade <- if (definition == "standard") {
      standard_decade(effective_cohort_year)
    } else {
      alternative_decade(effective_cohort_year)
    }
    baseline_decade <- treatment_decade - 10L
    available_pre_periods <- panel[decade < treatment_decade, uniqueN(decade)]
    # The balanced window needs three pre-fair bins on the left and a post
    # window reaching T+30 on the right, which pins the fair decade to the
    # range carried by the window specification.
    window_admits <- wf_event_window_admits(treatment_decade, event_window)
    timing_status <- if (available_pre_periods == 0L) {
      "skipped_no_pre_period"
    } else if (available_pre_periods < minimum_pre_periods) {
      "skipped_insufficient_pre_periods"
    } else if (!window_admits) {
      "skipped_outside_balanced_window"
    } else {
      "donor_pool_built"
    }

    timing_key <- paste(definition, treated_id, sep = "|")
    all_timing[[timing_key]] <- data.table(
      treatment_mode = treatment_mode_value,
      treatment_radius_km = treatment_radius_value,
      matching_specification = matching_specification,
      decade_definition = definition,
      treated_lgd_id = treated_id,
      treated_lgd_name = treated_row$treated_lgd_name,
      treated_lgd_type = treated_row$treated_lgd_type,
      treated_first_fair_year = treated_row$treated_first_fair_year,
      fair_treatment_decade = treated_row$fair_treatment_decade,
      cohort_lead_years = cohort_lead_years,
      effective_cohort_year = effective_cohort_year,
      treatment_decade = treatment_decade,
      baseline_decade = baseline_decade,
      available_pre_periods = available_pre_periods,
      minimum_pre_periods_required = minimum_pre_periods,
      event_window = event_window_id,
      event_window_min_treatment_decade = event_window$min_treatment_decade,
      event_window_max_treatment_decade = event_window$max_treatment_decade,
      window_admits_treated_decade = window_admits,
      model_eligible = available_pre_periods >= minimum_pre_periods &&
        window_admits,
      timing_status = timing_status
    )
    if (available_pre_periods < minimum_pre_periods || !window_admits) next

    first_event_venues <- first_venues[
      treated_lgd_id == treated_id &
        event_year == treated_row$treated_first_fair_year
    ]
    if (!nrow(first_event_venues))
      stop("No first-treatment venue for ", treated_row$treated_lgd_name, ".")
    venue_points <- st_as_sf(
      first_event_venues,
      coords = c("venue_longitude", "venue_latitude"),
      crs = 4326, remove = FALSE
    )
    venue_points <- st_transform(venue_points, analysis_crs)
    distance_lookup <- data.table(
      analysis_lgd_id = targets$analysis_lgd_id,
      distance_from_first_fair_venue_km = min_distance_to_venues(
        targets, venue_points
      )
    )

    pre_treatment <- panel[decade < treatment_decade, .(
      cumulative_pre_treatment_scientists = sum(n_inventors),
      pre_treatment_person_years = sum(population * n_years_in_bin),
      pre_treatment_bins = .N
    ), by = analysis_lgd_id]
    pre_treatment[, pre_treatment_scientist_rate_per_100k_year :=
      1e5 * cumulative_pre_treatment_scientists /
        pre_treatment_person_years]

    # The lagged specification matches on the pre-treatment outcome path rather
    # than on one pooled scalar. Bins are the same decade bins the synthdid
    # models are estimated on, and the annualization matches the estimator's
    # outcome definition exactly.
    pre_bins <- if (matching_fixed_lags) {
      # Exactly the three strictly pre-fair bins: T-30, T-20, T-10. The fair
      # decade is a pre period for estimation but is deliberately kept out of
      # donor selection.
      sort(as.integer(treatment_decade + event_window$matched_lag_offsets))
    } else {
      sort(unique(panel[decade < treatment_decade, decade]))
    }
    lag_columns <- paste0("lag_rate_", pre_bins)
    transformed_lag_columns <- paste0("transformed_", lag_columns)
    growth_span_years <- as.integer(max(pre_bins) - min(pre_bins))
    if (matching_fixed_lags) {
      if (!identical(length(pre_bins),
                     length(event_window$matched_lag_offsets)) ||
          max(pre_bins) != baseline_decade ||
          !all(pre_bins %in% panel$decade)) {
        stop("Balanced matched lags are unavailable for ",
             treated_row$treated_lgd_name, " under ", definition, ".")
      }
    }
    if (matching_uses_lags) {
      if (length(pre_bins) < minimum_pre_periods || growth_span_years <= 0L) {
        stop("Cannot build lagged matching covariates for ",
             treated_row$treated_lgd_name, " under ", definition, ".")
      }
      if (max(pre_bins) != baseline_decade) {
        stop("The last pre-treatment bin must equal the baseline decade for ",
             treated_row$treated_lgd_name, " under ", definition, ".")
      }
      lag_wide <- dcast(
        panel[decade %in% pre_bins, .(
          analysis_lgd_id, decade,
          lag_rate = 1e5 * n_inventors / (population * n_years_in_bin)
        )],
        analysis_lgd_id ~ decade, value.var = "lag_rate"
      )
      setnames(lag_wide, as.character(pre_bins), lag_columns)
      if (nrow(lag_wide) != expected_lgds ||
          !all(lag_columns %in% names(lag_wide))) {
        stop("Lagged outcome reshape failed for ", definition, ".")
      }
      growth_inputs <- merge(
        panel[decade == min(pre_bins), .(
          analysis_lgd_id, pre_treatment_population_first = population
        )],
        panel[decade == max(pre_bins), .(
          analysis_lgd_id, pre_treatment_population_last = population
        )],
        by = "analysis_lgd_id", all = TRUE, sort = FALSE
      )
      growth_inputs[, pre_treatment_population_cagr := fifelse(
        is.finite(pre_treatment_population_first) &
          pre_treatment_population_first > 0 &
          is.finite(pre_treatment_population_last) &
          pre_treatment_population_last > 0,
        (pre_treatment_population_last / pre_treatment_population_first)^
          (1 / growth_span_years) - 1,
        NA_real_
      )]
      growth_inputs[, pre_treatment_population_growth_span_years :=
        growth_span_years]
      pre_treatment <- merge(
        pre_treatment, lag_wide, by = "analysis_lgd_id", all = TRUE,
        sort = FALSE
      )
      pre_treatment <- merge(
        pre_treatment, growth_inputs, by = "analysis_lgd_id", all = TRUE,
        sort = FALSE
      )
    }
    baseline <- panel[decade == baseline_decade, .(
      analysis_lgd_id,
      candidate_lgd_name = population_lgd_name,
      candidate_lgd_type = population_lgd_type,
      baseline_population = population
    )]
    baseline <- merge(
      baseline,
      treatment_lookup[, .(
        analysis_lgd_id,
        candidate_treatment_status = analysis_treatment_status
      )],
      by = "analysis_lgd_id", all.x = TRUE, sort = FALSE
    )
    if (nrow(baseline) != expected_lgds)
      stop("Missing baseline decade ", baseline_decade, " for ", definition, ".")

    covariates <- merge(
      baseline, pre_treatment, by = "analysis_lgd_id", all.x = TRUE,
      sort = FALSE
    )
    covariates <- merge(
      covariates, area_lookup, by = "analysis_lgd_id", all.x = TRUE,
      sort = FALSE
    )
    covariates <- merge(
      covariates, distance_lookup, by = "analysis_lgd_id", all.x = TRUE,
      sort = FALSE
    )
    covariates[, baseline_population_density_per_km2 :=
      baseline_population / fixed_1911_area_km2]

    treated_covariates <- covariates[analysis_lgd_id == treated_id]
    if (nrow(treated_covariates) != 1L ||
        any(!is.finite(unlist(treated_covariates[, .(
          cumulative_pre_treatment_scientists,
          pre_treatment_person_years,
          pre_treatment_scientist_rate_per_100k_year,
          baseline_population,
          baseline_population_density_per_km2
        )])))) {
      stop("Treated matching covariates are incomplete for ",
           treated_row$treated_lgd_name, ".")
    }
    if (matching_uses_lags) {
      treated_lag_values <- unlist(
        treated_covariates[, ..lag_columns], use.names = FALSE
      )
      if (any(!is.finite(treated_lag_values)) ||
          any(treated_lag_values < 0) ||
          !is.finite(treated_covariates$pre_treatment_population_cagr) ||
          treated_covariates$pre_treatment_population_cagr <= -1) {
        stop("Treated lagged matching covariates are incomplete for ",
             treated_row$treated_lgd_name, ".")
      }
    }

    candidates <- covariates[analysis_lgd_id %in% never_ids]
    candidates[, `:=`(
      treatment_mode = treatment_mode_value,
      treatment_radius_km = treatment_radius_value,
      matching_specification = matching_specification,
      decade_definition = definition,
      treated_lgd_id = treated_id,
      treated_lgd_name = treated_row$treated_lgd_name,
      treated_lgd_type = treated_row$treated_lgd_type,
      treated_first_fair_year = treated_row$treated_first_fair_year,
      fair_treatment_decade = treated_row$fair_treatment_decade,
      cohort_lead_years = cohort_lead_years,
      effective_cohort_year = effective_cohort_year,
      treatment_decade = treatment_decade,
      baseline_decade = baseline_decade,
      available_pre_periods = available_pre_periods,
      first_fair_parent_ids = paste(
        sort(unique(first_event_venues$parent_fair_id)), collapse = ";"
      ),
      first_fair_names = paste(
        sort(unique(first_event_venues$Fair_name)), collapse = " | "
      ),
      first_fair_venue_names = paste(
        sort(unique(first_event_venues$venue)), collapse = " | "
      ),
      first_fair_venue_latitudes = paste(
        sort(unique(first_event_venues$venue_latitude)), collapse = ";"
      ),
      first_fair_venue_longitudes = paste(
        sort(unique(first_event_venues$venue_longitude)), collapse = ";"
      ),
      treated_pre_treatment_scientists =
        treated_covariates$cumulative_pre_treatment_scientists,
      treated_pre_treatment_person_years =
        treated_covariates$pre_treatment_person_years,
      treated_pre_treatment_scientist_rate_per_100k_year =
        treated_covariates$pre_treatment_scientist_rate_per_100k_year,
      treated_baseline_population = treated_covariates$baseline_population,
      treated_fixed_1911_area_km2 = treated_covariates$fixed_1911_area_km2,
      treated_baseline_population_density_per_km2 =
        treated_covariates$baseline_population_density_per_km2
    )]
    if (matching_uses_lags) {
      candidates[, `:=`(
        treated_pre_treatment_population_first =
          treated_covariates$pre_treatment_population_first,
        treated_pre_treatment_population_last =
          treated_covariates$pre_treatment_population_last,
        treated_pre_treatment_population_cagr =
          treated_covariates$pre_treatment_population_cagr,
        pre_treatment_population_growth_span_years = growth_span_years,
        pre_treatment_bins_matched = length(pre_bins)
      )]
    }
    candidates[, valid_matching_covariates :=
      !is.na(cumulative_pre_treatment_scientists) &
      is.finite(pre_treatment_person_years) &
      pre_treatment_person_years > 0 &
      is.finite(pre_treatment_scientist_rate_per_100k_year) &
      pre_treatment_scientist_rate_per_100k_year >= 0 &
      is.finite(baseline_population) & baseline_population > 0 &
      is.finite(fixed_1911_area_km2) & fixed_1911_area_km2 > 0 &
      is.finite(baseline_population_density_per_km2) &
      baseline_population_density_per_km2 > 0]
    if (matching_uses_lags) {
      lag_valid <- Reduce(`&`, lapply(lag_columns, function(column) {
        values <- candidates[[column]]
        is.finite(values) & values >= 0
      }))
      candidates[, valid_matching_covariates := valid_matching_covariates &
        lag_valid &
        is.finite(pre_treatment_population_first) &
        pre_treatment_population_first > 0 &
        is.finite(pre_treatment_population_last) &
        pre_treatment_population_last > 0 &
        is.finite(pre_treatment_population_cagr) &
        pre_treatment_population_cagr > -1]
    }
    candidates[, outside_distance_exclusion :=
      is.finite(distance_from_first_fair_venue_km) &
      distance_from_first_fair_venue_km >= donor_min_distance_km]
    candidates[, eligible_donor :=
      valid_matching_covariates & outside_distance_exclusion]
    candidates[, exclusion_reason := fifelse(
      !valid_matching_covariates, "invalid_matching_covariates",
      fifelse(
        !is.finite(distance_from_first_fair_venue_km),
        "missing_venue_distance",
        fifelse(
          distance_from_first_fair_venue_km < donor_min_distance_km,
          "within_first_fair_venue_exclusion", "eligible"
        )
      )
    )]

    eligible <- candidates[eligible_donor == TRUE]
    if (nrow(eligible) < donor_count)
      stop("Only ", nrow(eligible), " eligible donors for ",
           treated_row$treated_lgd_name, " under ", definition, ".")

    component_long <- NULL
    columns_audit <- NULL
    if (matching_uses_lags) {
      # One column per pre-treatment bin, plus population growth and density.
      # Every column is z-scored on the eligible-donor moments and enters the
      # Euclidean distance with equal weight, so the outcome path share of the
      # distance rises with the number of pre-treatment bins.
      eligible[, (transformed_lag_columns) := lapply(.SD, log1p),
               .SDcols = lag_columns]
      eligible[, `:=`(
        transformed_population_growth = log1p(pre_treatment_population_cagr),
        transformed_density = log(baseline_population_density_per_km2)
      )]
      feature_columns <- c(
        transformed_lag_columns,
        "transformed_population_growth", "transformed_density"
      )
      feature_groups <- c(
        rep("outcome_lag", length(transformed_lag_columns)),
        "population_growth", "density"
      )
      feature_sources <- c(lag_columns, "pre_treatment_population_cagr",
                           "baseline_population_density_per_km2")
      treated_features <- c(
        log1p(unlist(treated_covariates[, ..lag_columns], use.names = FALSE)),
        log1p(treated_covariates$pre_treatment_population_cagr),
        log(treated_covariates$baseline_population_density_per_km2)
      )
      names(treated_features) <- feature_columns
      donor_features <- as.matrix(eligible[, ..feature_columns])
      feature_mean <- colMeans(donor_features)
      feature_sd <- apply(donor_features, 2L, sd)
      feature_usable <- is.finite(feature_mean) & is.finite(feature_sd) &
        feature_sd > 0 & is.finite(treated_features)
      drop_reason <- fifelse(
        feature_usable, "",
        fifelse(!is.finite(treated_features), "treated_value_not_finite",
                fifelse(!is.finite(feature_sd) | feature_sd <= 0,
                        "zero_donor_variance", "donor_mean_not_finite"))
      )
      if (sum(feature_usable) < 2L) {
        stop("Fewer than two usable matching columns for ",
             treated_row$treated_lgd_name, " under ", definition, ".")
      }
      used_columns <- feature_columns[feature_usable]
      used_groups <- feature_groups[feature_usable]
      donor_z <- sweep(
        sweep(donor_features[, feature_usable, drop = FALSE], 2L,
              feature_mean[feature_usable], "-"),
        2L, feature_sd[feature_usable], "/"
      )
      treated_z <- (treated_features[feature_usable] -
                      feature_mean[feature_usable]) /
        feature_sd[feature_usable]
      z_gap <- sweep(donor_z, 2L, treated_z, "-")
      group_component <- function(group) {
        index <- which(used_groups == group)
        if (!length(index)) return(rep(0, nrow(z_gap)))
        sqrt(rowSums(z_gap[, index, drop = FALSE]^2))
      }
      if (!all(c("transformed_population_growth", "transformed_density") %in%
                 used_columns)) {
        stop("Population growth and density must both enter the distance for ",
             treated_row$treated_lgd_name, " under ", definition, ".")
      }
      eligible[, `:=`(
        transformed_population_growth_z =
          (transformed_population_growth -
             feature_mean[["transformed_population_growth"]]) /
          feature_sd[["transformed_population_growth"]],
        donor_density_z = (transformed_density -
          feature_mean[["transformed_density"]]) /
          feature_sd[["transformed_density"]],
        treated_population_growth_z =
          treated_z[["transformed_population_growth"]],
        treated_density_z = treated_z[["transformed_density"]],
        outcome_lag_distance_component = group_component("outcome_lag"),
        growth_distance_component = group_component("population_growth"),
        density_distance_component = group_component("density"),
        n_matching_columns = sum(feature_usable),
        n_matching_columns_outcome_lags = sum(used_groups == "outcome_lag")
      )]
      eligible[, match_distance := sqrt(rowSums(z_gap^2))]

      columns_audit <- data.table(
        matching_specification = matching_specification,
        decade_definition = definition,
        treated_lgd_id = treated_id,
        treated_lgd_name = treated_row$treated_lgd_name,
        treatment_decade = treatment_decade,
        baseline_decade = baseline_decade,
        available_pre_periods = available_pre_periods,
        eligible_donors = nrow(eligible),
        feature_index = seq_along(feature_columns),
        feature = feature_columns,
        feature_group = feature_groups,
        feature_source_column = feature_sources,
        donor_mean = as.numeric(feature_mean),
        donor_sd = as.numeric(feature_sd),
        treated_value = as.numeric(treated_features),
        treated_z = fifelse(
          feature_usable,
          (as.numeric(treated_features) - as.numeric(feature_mean)) /
            as.numeric(feature_sd),
          NA_real_
        ),
        used_in_distance = feature_usable,
        drop_reason = drop_reason
      )
      component_long <- data.table(
        analysis_lgd_id = rep(eligible$analysis_lgd_id,
                              times = length(used_columns)),
        feature = rep(used_columns, each = nrow(eligible)),
        feature_group = rep(used_groups, each = nrow(eligible)),
        donor_value = as.numeric(
          donor_features[, feature_usable, drop = FALSE]
        ),
        donor_z = as.numeric(donor_z),
        treated_z = rep(as.numeric(treated_z), each = nrow(eligible)),
        z_gap = as.numeric(z_gap)
      )
    } else {
      eligible[, `:=`(
        transformed_scientist_rate =
          log1p(pre_treatment_scientist_rate_per_100k_year),
        transformed_density = log(baseline_population_density_per_km2)
      )]
      donor_scientist_rate_mean <- mean(eligible$transformed_scientist_rate)
      donor_scientist_rate_sd <- sd(eligible$transformed_scientist_rate)
      donor_density_mean <- mean(eligible$transformed_density)
      donor_density_sd <- sd(eligible$transformed_density)
      if (!is.finite(donor_scientist_rate_sd) ||
          donor_scientist_rate_sd <= 0 ||
          !is.finite(donor_density_sd) || donor_density_sd <= 0) {
        stop("Cannot standardize matching variables for ",
             treated_row$treated_lgd_name, " under ", definition, ".")
      }

      treated_scientist_rate_z <- (
        log1p(
          treated_covariates$pre_treatment_scientist_rate_per_100k_year
        ) - donor_scientist_rate_mean
      ) / donor_scientist_rate_sd
      treated_density_z <- (
        log(treated_covariates$baseline_population_density_per_km2) -
          donor_density_mean
      ) / donor_density_sd
      eligible[, `:=`(
        donor_scientist_rate_z =
          (transformed_scientist_rate - donor_scientist_rate_mean) /
            donor_scientist_rate_sd,
        donor_density_z =
          (transformed_density - donor_density_mean) / donor_density_sd,
        treated_scientist_rate_z = treated_scientist_rate_z,
        treated_density_z = treated_density_z
      )]
      eligible[, `:=`(
        scientist_rate_distance_component = abs(
          donor_scientist_rate_z - treated_scientist_rate_z
        ),
        density_distance_component = abs(donor_density_z - treated_density_z)
      )]
      eligible[, match_distance := sqrt(
        scientist_rate_distance_component^2 + density_distance_component^2
      )]
    }
    eligible[, `:=`(
      scientist_rate_plus_one_ratio = safe_ratio(
        pre_treatment_scientist_rate_per_100k_year + 1,
        treated_covariates$pre_treatment_scientist_rate_per_100k_year + 1
      ),
      density_ratio = safe_ratio(
        baseline_population_density_per_km2,
        treated_covariates$baseline_population_density_per_km2
      ),
      population_ratio = safe_ratio(
        baseline_population, treated_covariates$baseline_population
      ),
      donor_to_treated_population_ratio =
        baseline_population / treated_covariates$baseline_population,
      absolute_log_population_difference = abs(
        log(baseline_population) - log(treated_covariates$baseline_population)
      )
    )]
    setorder(eligible, match_distance, analysis_lgd_id)
    eligible[, donor_rank := seq_len(.N)]
    eligible[, selected_donor := donor_rank <= donor_count]

    scored_columns <- if (matching_uses_lags) c(
      "analysis_lgd_id", "transformed_population_growth", "transformed_density",
      "transformed_population_growth_z", "donor_density_z",
      "treated_population_growth_z", "treated_density_z",
      "outcome_lag_distance_component", "growth_distance_component",
      "density_distance_component", "n_matching_columns",
      "n_matching_columns_outcome_lags", "match_distance",
      "scientist_rate_plus_one_ratio", "density_ratio", "population_ratio",
      "donor_to_treated_population_ratio",
      "absolute_log_population_difference", "donor_rank", "selected_donor"
    ) else c(
      "analysis_lgd_id", "transformed_scientist_rate", "transformed_density",
      "donor_scientist_rate_z", "donor_density_z",
      "treated_scientist_rate_z",
      "treated_density_z", "scientist_rate_distance_component",
      "density_distance_component", "match_distance",
      "scientist_rate_plus_one_ratio", "density_ratio", "population_ratio",
      "donor_to_treated_population_ratio",
      "absolute_log_population_difference", "donor_rank", "selected_donor"
    )
    candidates <- merge(
      candidates, eligible[, ..scored_columns],
      by = "analysis_lgd_id", all.x = TRUE, sort = FALSE
    )
    candidates[is.na(selected_donor), selected_donor := FALSE]
    candidates[, `:=`(
      donor_count_requested = donor_count,
      donor_min_distance_km = donor_min_distance_km,
      donor_scientist_rate_transform_mean = if (matching_uses_lags)
        NA_real_ else donor_scientist_rate_mean,
      donor_scientist_rate_transform_sd = if (matching_uses_lags)
        NA_real_ else donor_scientist_rate_sd,
      donor_density_transform_mean = if (matching_uses_lags)
        feature_mean[["transformed_density"]] else donor_density_mean,
      donor_density_transform_sd = if (matching_uses_lags)
        feature_sd[["transformed_density"]] else donor_density_sd
    )]
    setnames(candidates, "analysis_lgd_id", "donor_lgd_id")
    setnames(candidates, c(
      "candidate_lgd_name", "candidate_lgd_type",
      "candidate_treatment_status", "cumulative_pre_treatment_scientists",
      "pre_treatment_person_years", "pre_treatment_bins",
      "pre_treatment_scientist_rate_per_100k_year",
      "baseline_population", "fixed_1911_area_km2",
      "baseline_population_density_per_km2"
    ), c(
      "donor_lgd_name", "donor_lgd_type", "donor_treatment_status",
      "donor_pre_treatment_scientists",
      "donor_pre_treatment_person_years",
      "donor_pre_treatment_bins",
      "donor_pre_treatment_scientist_rate_per_100k_year",
      "donor_baseline_population",
      "donor_fixed_1911_area_km2",
      "donor_baseline_population_density_per_km2"
    ))

    selected <- candidates[selected_donor == TRUE]
    if (nrow(selected) != donor_count ||
        anyDuplicated(selected$donor_lgd_id) ||
        any(selected$donor_treatment_status != candidate_control_status) ||
        any(selected$distance_from_first_fair_venue_km + tolerance <
              donor_min_distance_km)) {
      stop("Selected donor-pool validation failed for ",
           treated_row$treated_lgd_name, " under ", definition, ".")
    }

    summary_row <- selected[, .(
      treatment_mode = treatment_mode_value,
      treatment_radius_km = treatment_radius_value,
      matching_specification = matching_specification,
      decade_definition = definition,
      treated_lgd_id = treated_id,
      treated_lgd_name = treated_row$treated_lgd_name,
      treated_lgd_type = treated_row$treated_lgd_type,
      treated_first_fair_year = treated_row$treated_first_fair_year,
      fair_treatment_decade = treated_row$fair_treatment_decade,
      cohort_lead_years = cohort_lead_years,
      effective_cohort_year = effective_cohort_year,
      treatment_decade = treatment_decade,
      baseline_decade = baseline_decade,
      available_pre_periods = available_pre_periods,
      candidate_never_exposed_lgds = nrow(candidates),
      candidates_excluded_within_venue_distance = candidates[
        exclusion_reason == "within_first_fair_venue_exclusion", .N
      ],
      candidates_excluded_invalid_covariates = candidates[
        exclusion_reason == "invalid_matching_covariates", .N
      ],
      eligible_donors = candidates[eligible_donor == TRUE, .N],
      selected_donors = .N,
      treated_pre_treatment_scientists =
        unique(treated_pre_treatment_scientists),
      treated_pre_treatment_person_years =
        unique(treated_pre_treatment_person_years),
      treated_pre_treatment_scientist_rate_per_100k_year =
        unique(treated_pre_treatment_scientist_rate_per_100k_year),
      selected_donor_scientists_min = min(donor_pre_treatment_scientists),
      selected_donor_scientists_median = median(donor_pre_treatment_scientists),
      selected_donor_scientists_max = max(donor_pre_treatment_scientists),
      selected_donor_scientist_rate_min =
        min(donor_pre_treatment_scientist_rate_per_100k_year),
      selected_donor_scientist_rate_median =
        median(donor_pre_treatment_scientist_rate_per_100k_year),
      selected_donor_scientist_rate_max =
        max(donor_pre_treatment_scientist_rate_per_100k_year),
      treated_baseline_population = unique(treated_baseline_population),
      selected_donor_population_min = min(donor_baseline_population),
      selected_donor_population_median = median(donor_baseline_population),
      selected_donor_population_max = max(donor_baseline_population),
      selected_donor_population_ratio_median = median(population_ratio),
      selected_donor_population_ratio_max = max(population_ratio),
      selected_donor_share_within_2x_population = mean(population_ratio <= 2),
      treated_population_within_selected_range =
        unique(treated_baseline_population) >= min(donor_baseline_population) &
        unique(treated_baseline_population) <= max(donor_baseline_population),
      treated_baseline_density_per_km2 =
        unique(treated_baseline_population_density_per_km2),
      selected_donor_density_min =
        min(donor_baseline_population_density_per_km2),
      selected_donor_density_median =
        median(donor_baseline_population_density_per_km2),
      selected_donor_density_max =
        max(donor_baseline_population_density_per_km2),
      selected_donor_distance_from_venue_min_km =
        min(distance_from_first_fair_venue_km),
      selected_donor_distance_from_venue_median_km =
        median(distance_from_first_fair_venue_km),
      nearest_match_distance = min(match_distance),
      thirtieth_match_distance = max(match_distance),
      mean_match_distance = mean(match_distance)
    )][1L]

    key <- paste(definition, treated_id, sep = "|")
    if (matching_uses_lags) {
      summary_row[, `:=`(
        n_matching_columns = unique(selected$n_matching_columns),
        n_matching_columns_outcome_lags =
          unique(selected$n_matching_columns_outcome_lags),
        pre_treatment_population_growth_span_years = growth_span_years,
        treated_pre_treatment_population_cagr =
          unique(selected$treated_pre_treatment_population_cagr),
        selected_donor_population_cagr_min =
          min(selected$pre_treatment_population_cagr),
        selected_donor_population_cagr_median =
          median(selected$pre_treatment_population_cagr),
        selected_donor_population_cagr_max =
          max(selected$pre_treatment_population_cagr),
        outcome_lag_component_median = median(
          selected$outcome_lag_distance_component
        ),
        growth_component_median = median(selected$growth_distance_component),
        density_component_median = median(selected$density_distance_component)
      )]
      all_matching_columns[[key]] <- columns_audit
      components <- component_long[
        analysis_lgd_id %in% selected$donor_lgd_id
      ]
      setnames(components, "analysis_lgd_id", "donor_lgd_id")
      components <- merge(
        components, selected[, .(donor_lgd_id, donor_lgd_name, donor_rank)],
        by = "donor_lgd_id", all.x = TRUE, sort = FALSE
      )
      components[, `:=`(
        matching_specification = matching_specification,
        decade_definition = definition,
        treated_lgd_id = treated_id,
        treated_lgd_name = treated_row$treated_lgd_name,
        treatment_decade = treatment_decade
      )]
      setorder(components, donor_rank, feature)
      setcolorder(components, c(
        "matching_specification", "decade_definition", "treated_lgd_id",
        "treated_lgd_name", "treatment_decade", "donor_lgd_id",
        "donor_lgd_name", "donor_rank", "feature", "feature_group"
      ))
      all_matching_components[[key]] <- components
    }
    all_candidates[[key]] <- candidates
    all_selected[[key]] <- selected
    all_summaries[[key]] <- summary_row
  }
}

candidate_audit <- rbindlist(all_candidates, fill = TRUE, use.names = TRUE)
selected_pairs <- rbindlist(all_selected, fill = TRUE, use.names = TRUE)
pool_summary <- rbindlist(all_summaries, fill = TRUE, use.names = TRUE)
timing_status <- rbindlist(all_timing, fill = TRUE, use.names = TRUE)
matching_columns_audit <- if (length(all_matching_columns))
  rbindlist(all_matching_columns, fill = TRUE, use.names = TRUE) else
  data.table()
matching_components_audit <- if (length(all_matching_components))
  rbindlist(all_matching_components, fill = TRUE, use.names = TRUE) else
  data.table()

if (nrow(timing_status) != 2L * expected_treated ||
    anyDuplicated(timing_status[, .(decade_definition, treated_lgd_id)])) {
  stop("Cohort timing status is not one row per treated LGD and definition.")
}
eligible_model_count <- timing_status[model_eligible == TRUE, .N]
skipped_no_pre_count <- timing_status[
  timing_status == "skipped_no_pre_period", .N
]
skipped_insufficient_pre_count <- timing_status[
  timing_status == "skipped_insufficient_pre_periods", .N
]
skipped_outside_window_count <- timing_status[
  timing_status == "skipped_outside_balanced_window", .N
]
# The balanced window trims both tails of the treated-decade distribution, so
# the full-window count assertions do not apply to it.
if (event_window$balanced && treatment_mode == "host") {
  expected_balanced_pools <- if (cohort_lead_years == 0L) 19L else
    if (cohort_lead_years == 20L) 10L else NA_integer_
  if (!is.na(expected_balanced_pools) &&
      eligible_model_count != expected_balanced_pools) {
    stop("The balanced window should retain ", expected_balanced_pools,
         " host pools at a ", cohort_lead_years, "-year lead, not ",
         eligible_model_count, ".")
  }
}
if (!event_window$balanced && cohort_lead_years == 0L &&
    eligible_model_count != 2L * expected_treated) {
  stop("The zero-lead specification should retain every model definition.")
}
if (!event_window$balanced && cohort_lead_years == 20L &&
    treatment_mode == "host" &&
    (eligible_model_count != 15L || skipped_no_pre_count != 1L ||
     skipped_insufficient_pre_count != 6L)) {
  stop("Unexpected host-treatment timing counts for the 20-year lead.")
}
if (!event_window$balanced && cohort_lead_years == 20L &&
    treatment_mode == "radius" &&
    treatment_radius_km == 50 &&
    (eligible_model_count != 403L || skipped_no_pre_count != 5L ||
     skipped_insufficient_pre_count != 178L)) {
  stop("Unexpected 50 km treatment timing counts for the 20-year lead.")
}

# Enforce the declared one-row-per-pool summary interface.
pool_summary <- unique(
  pool_summary,
  by = c("decade_definition", "treated_lgd_id")
)
if (nrow(pool_summary) != eligible_model_count)
  stop("Pool summary is not one row per timing definition and treated LGD.")

pool_summary[, support_distance_rank_desc := frank(
  -thirtieth_match_distance, ties.method = "first"
), by = decade_definition]
pool_summary[, support_distance_percentile_within_treated := frank(
  thirtieth_match_distance, ties.method = "average"
) / .N, by = decade_definition]

setorder(candidate_audit, decade_definition, treated_first_fair_year,
         treated_lgd_id, selected_donor, donor_rank, donor_lgd_id)
setorder(selected_pairs, decade_definition, treated_first_fair_year,
         treated_lgd_id, donor_rank)
setorder(pool_summary, decade_definition, treated_first_fair_year,
         treated_lgd_id)
setorder(timing_status, decade_definition, treatment_decade,
         treated_lgd_id)

###############################################################################
# Standard-versus-alternative pool overlap and final QC
###############################################################################

common_treated_ids <- intersect(
  selected_pairs[decade_definition == "standard", unique(treated_lgd_id)],
  selected_pairs[decade_definition == "alternative", unique(treated_lgd_id)]
)
standard_pairs <- selected_pairs[
  decade_definition == "standard" & treated_lgd_id %in% common_treated_ids, .(
  treated_lgd_id, donor_lgd_id, standard_rank = donor_rank
)]
alternative_pairs <- selected_pairs[
  decade_definition == "alternative" & treated_lgd_id %in% common_treated_ids, .(
  treated_lgd_id, donor_lgd_id, alternative_rank = donor_rank
)]
pair_overlap <- merge(
  standard_pairs, alternative_pairs,
  by = c("treated_lgd_id", "donor_lgd_id"), all = TRUE
)
overlap_summary <- pair_overlap[, .(
  standard_donors = sum(!is.na(standard_rank)),
  alternative_donors = sum(!is.na(alternative_rank)),
  common_donors = sum(!is.na(standard_rank) & !is.na(alternative_rank)),
  union_donors = .N,
  jaccard_similarity =
    sum(!is.na(standard_rank) & !is.na(alternative_rank)) / .N,
  mean_absolute_rank_change_common = if (
    any(!is.na(standard_rank) & !is.na(alternative_rank))
  ) mean(abs(standard_rank - alternative_rank), na.rm = TRUE) else NA_real_
), by = treated_lgd_id]
treated_names <- unique(selected_pairs[
  treated_lgd_id %in% common_treated_ids, .(
  treated_lgd_id, treated_lgd_name, treated_lgd_type,
  treated_first_fair_year
)])
overlap_summary <- merge(
  unique(treated_names), overlap_summary,
  by = "treated_lgd_id", all.x = TRUE, sort = FALSE
)
setorder(overlap_summary, treated_first_fair_year, treated_lgd_id)

expected_pool_rows <- eligible_model_count * donor_count
if (nrow(selected_pairs) != expected_pool_rows ||
    uniqueN(selected_pairs[, .(decade_definition, treated_lgd_id)]) !=
      eligible_model_count ||
    selected_pairs[, anyDuplicated(.SD), .SDcols = c(
      "decade_definition", "treated_lgd_id", "donor_lgd_id"
    )] ||
    any(selected_pairs$donor_treatment_status != candidate_control_status) ||
    any(selected_pairs$distance_from_first_fair_venue_km + tolerance <
          donor_min_distance_km) ||
    any(!is.finite(selected_pairs$match_distance))) {
  stop("Final selected donor-pair validation failed.")
}

ranking_check <- selected_pairs[, .(
  correct_ranks = identical(donor_rank, seq_len(.N)),
  nondecreasing_distance = all(diff(match_distance) >= -tolerance)
), by = .(decade_definition, treated_lgd_id)]
if (any(!ranking_check$correct_ranks) ||
    any(!ranking_check$nondecreasing_distance)) {
  stop("Selected donor rankings are not deterministic and nondecreasing.")
}

density_identity_error <- selected_pairs[, max(abs(
  donor_baseline_population_density_per_km2 -
    donor_baseline_population / donor_fixed_1911_area_km2
))]
if (!is.finite(density_identity_error) || density_identity_error > tolerance)
  stop("Population-density identity failed.")

scientist_rate_identity_error <- selected_pairs[, max(abs(
  donor_pre_treatment_scientist_rate_per_100k_year -
    1e5 * donor_pre_treatment_scientists /
      donor_pre_treatment_person_years
))]
if (!is.finite(scientist_rate_identity_error) ||
    scientist_rate_identity_error > tolerance) {
  stop("Pooled pre-treatment scientist-rate identity failed.")
}

match_distance_identity_error <- if (matching_uses_lags) {
  selected_pairs[, max(abs(
    match_distance - sqrt(
      outcome_lag_distance_component^2 + growth_distance_component^2 +
        density_distance_component^2
    )
  ))]
} else {
  selected_pairs[, max(abs(
    match_distance - sqrt(
      scientist_rate_distance_component^2 + density_distance_component^2
    )
  ))]
}
if (!is.finite(match_distance_identity_error) ||
    match_distance_identity_error > tolerance) {
  stop(if (matching_uses_lags) "Three-component" else "Two-component",
       " matching-distance identity failed.")
}

# The per-feature component audit must reproduce the selected-pair distance.
component_identity_error <- NA_real_
if (matching_uses_lags) {
  recomputed <- matching_components_audit[, .(
    recomputed_distance = sqrt(sum(z_gap^2))
  ), by = .(decade_definition, treated_lgd_id, donor_lgd_id)]
  recomputed <- merge(
    recomputed,
    selected_pairs[, .(decade_definition, treated_lgd_id, donor_lgd_id,
                       match_distance)],
    by = c("decade_definition", "treated_lgd_id", "donor_lgd_id"),
    all = TRUE, sort = FALSE
  )
  if (nrow(recomputed) != nrow(selected_pairs) ||
      any(!is.finite(recomputed$recomputed_distance))) {
    stop("Matching-component audit does not cover every selected pair.")
  }
  component_identity_error <- recomputed[, max(abs(
    recomputed_distance - match_distance
  ))]
  if (!is.finite(component_identity_error) ||
      component_identity_error > 1e-8) {
    stop("Matching-component audit does not reproduce the match distance.")
  }
  if (matching_fixed_lags) {
    expected_columns <- length(event_window$matched_lag_offsets) + 2L
    used_column_counts <- matching_columns_audit[
      used_in_distance == TRUE, .N, by = .(decade_definition, treated_lgd_id)
    ]
    if (any(used_column_counts$N != expected_columns))
      stop("The balanced specification must match on exactly ",
           expected_columns, " columns in every pool.")
    lag_offsets <- matching_columns_audit[
      feature_group == "outcome_lag",
      .(offset = as.integer(sub("^transformed_lag_rate_", "", feature)) -
          treatment_decade)
    ]
    if (!setequal(unique(lag_offsets$offset), event_window$matched_lag_offsets))
      stop("Balanced matched lags are not at the declared window offsets.")
    expected_span <- as.integer(diff(range(event_window$matched_lag_offsets)))
    span_years <- unique(pool_summary$pre_treatment_population_growth_span_years)
    if (length(span_years) != 1L || span_years != expected_span)
      stop("The balanced population CAGR must span ", expected_span,
           " years in every pool.")
    if (matching_columns_audit[feature_group == "population_growth" &
                               !is.finite(treated_value), .N])
      stop("Balanced population-growth covariate is not finite.")
  } else {
    used_column_counts <- matching_columns_audit[
      used_in_distance == TRUE, .N,
      by = .(decade_definition, treated_lgd_id, available_pre_periods)
    ]
    if (any(used_column_counts$N !=
              used_column_counts$available_pre_periods + 2L))
      stop("Each pool must match on one column per pre-treatment bin plus two.")
  }
}

radius_match_rows <- if (treatment_mode == "radius")
  nrow(radius_exposure$audit) else NA_integer_
radius_exposed_lgds <- if (treatment_mode == "radius")
  uniqueN(radius_exposure$audit$analysis_lgd_id) else NA_integer_
radius_max_match_distance_km <- if (treatment_mode == "radius")
  max(radius_exposure$audit$distance_km) else NA_real_
radius_outside_ew_first_exposures <- if (treatment_mode == "radius")
  radius_exposure$assignment[
    first_fair_match_status == "outside_fixed_ew_lgds", .N
  ] else NA_integer_
if (treatment_mode == "radius" && treatment_radius_km == 50 &&
    (radius_match_rows != 8319L || radius_exposed_lgds != 937L ||
     radius_max_match_distance_km > treatment_radius_km + tolerance ||
     radius_outside_ew_first_exposures != 0L)) {
  stop("The 50 km radius exposure audit failed validation.")
}

legacy_balanced_pool_match <- NA
if (treatment_mode == "host" &&
    identical(matching_spec_id, "balanced3_rate_growth_density_e0")) {
  legacy_dir <- file.path(treatment_results_dir, "match_v3_balanced")
  if (cohort_lead_years != 0L) {
    legacy_dir <- file.path(
      legacy_dir, paste0("cohort_lead_", cohort_lead_years, "y")
    )
  }
  legacy_file <- file.path(legacy_dir, "donor_pool_pairs.csv")
  if (!file.exists(legacy_file)) {
    stop("Legacy balanced donor pools are required for the e0 audit: ",
         legacy_file)
  }
  legacy_pairs <- fread(legacy_file, na.strings = c("", "NA"))
  comparison_keys <- c(
    "decade_definition", "treated_lgd_id", "donor_lgd_id", "donor_rank"
  )
  setorderv(legacy_pairs, comparison_keys)
  comparison_pairs <- copy(selected_pairs)
  setorderv(comparison_pairs, comparison_keys)
  legacy_balanced_pool_match <- identical(
    legacy_pairs[, ..comparison_keys], comparison_pairs[, ..comparison_keys]
  ) && max(abs(
    legacy_pairs$match_distance - comparison_pairs$match_distance
  )) <= 1e-12
  if (!legacy_balanced_pool_match) {
    stop("Corrected e0 donor IDs, ranks, or distances differ from legacy v3.")
  }
}

qc <- data.table(
  metric = c(
    "treatment_mode", "treatment_radius_km", "candidate_control_status",
    "radius_match_rows", "radius_exposed_lgds",
    "radius_max_match_distance_km", "radius_outside_ew_first_exposures",
    "donor_count_requested", "donor_min_distance_km",
    "cohort_lead_years", "minimum_pre_periods_required",
    "treated_lgds", "pretreated_lgds", "never_exposed_lgds",
    "timing_definitions", "treated_definition_models_requested",
    "treated_definition_pools", "skipped_no_pre_period_models",
    "skipped_insufficient_pre_period_models",
    "candidate_audit_rows", "selected_pair_rows",
    "selected_pairs_expected", "selected_pair_duplicate_keys",
    "selected_non_never_exposed_donors", "selected_within_distance_exclusion",
    "matching_specification", "matching_specification_id",
    "event_window", "event_window_min_treatment_decade",
    "event_window_max_treatment_decade",
    "skipped_outside_balanced_window_models",
    "matching_columns_min", "matching_columns_max",
    "matching_columns_dropped", "matching_component_rows",
    "max_matching_component_identity_error",
    "selected_missing_match_distance",
    "max_density_identity_error", "max_scientist_rate_identity_error",
    "max_match_distance_identity_error",
    "minimum_eligible_donors_any_pool", "maximum_eligible_donors_any_pool",
    "minimum_standard_alternative_common_donors",
    "maximum_standard_alternative_common_donors",
    "legacy_balanced_pool_ids_ranks_distances_match"
  ),
  value = as.character(c(
    treatment_mode, treatment_radius_value, candidate_control_status,
    radius_match_rows, radius_exposed_lgds, radius_max_match_distance_km,
    radius_outside_ew_first_exposures,
    donor_count, donor_min_distance_km, cohort_lead_years,
    minimum_pre_periods, expected_treated, expected_pretreated,
    expected_never, 2L, nrow(timing_status), nrow(pool_summary),
    skipped_no_pre_count, skipped_insufficient_pre_count, nrow(candidate_audit),
    nrow(selected_pairs), expected_pool_rows,
    anyDuplicated(selected_pairs[, .(
      decade_definition, treated_lgd_id, donor_lgd_id
    )]),
    selected_pairs[donor_treatment_status != candidate_control_status, .N],
    selected_pairs[distance_from_first_fair_venue_km <
                     donor_min_distance_km, .N],
    matching_specification, matching_spec_id,
    event_window_id, event_window$min_treatment_decade,
    event_window$max_treatment_decade, skipped_outside_window_count,
    if (nrow(matching_columns_audit)) matching_columns_audit[
      used_in_distance == TRUE, .N,
      by = .(decade_definition, treated_lgd_id)
    ][, min(N)] else NA_integer_,
    if (nrow(matching_columns_audit)) matching_columns_audit[
      used_in_distance == TRUE, .N,
      by = .(decade_definition, treated_lgd_id)
    ][, max(N)] else NA_integer_,
    if (nrow(matching_columns_audit))
      matching_columns_audit[used_in_distance == FALSE, .N] else NA_integer_,
    nrow(matching_components_audit), component_identity_error,
    selected_pairs[!is.finite(match_distance), .N], density_identity_error,
    scientist_rate_identity_error, match_distance_identity_error,
    min(pool_summary$eligible_donors), max(pool_summary$eligible_donors),
    min(overlap_summary$common_donors), max(overlap_summary$common_donors),
    legacy_balanced_pool_match
  ))
)

fwrite(candidate_audit, candidate_output, na = "")
fwrite(selected_pairs, selected_output, na = "")
fwrite(pool_summary, summary_output, na = "")
fwrite(overlap_summary, overlap_output, na = "")
fwrite(qc, qc_output, na = "")
fwrite(timing_status, timing_output, na = "")
if (matching_uses_lags) {
  fwrite(matching_columns_audit, matching_columns_output, na = "")
  fwrite(matching_components_audit, matching_components_output, na = "")
}

message("Wikipedia donor-pool construction complete.")
message("  Matching specification: ", matching_spec_id)
message("  Event window: ", event_window_id)
message("  Treatment mode: ", treatment_mode,
        if (treatment_mode == "radius") paste0(" (", radius_tag, ")") else "")
message("  Cohort lead: ", cohort_lead_years, " years")
message("  Timing definitions requested: ", nrow(timing_status))
message("  Feasible donor pools: ", nrow(pool_summary))
message("  Selected pairs: ", nrow(selected_pairs))
message("  Eligible donors per pool: ", min(pool_summary$eligible_donors),
        "--", max(pool_summary$eligible_donors))
message("  Largest 30th-neighbor distances (weakest relative support):")
print(pool_summary[
  order(-thirtieth_match_distance),
  .(decade_definition, treated_lgd_name, treatment_decade,
    thirtieth_match_distance, selected_donor_population_ratio_median,
    selected_donor_population_ratio_max)
][1:6])
message("  Selected pairs: ", selected_output)
message("  Candidate audit: ", candidate_output)
message("  Pool summary: ", summary_output)
message("  Timing overlap: ", overlap_output)
message("  QC: ", qc_output)
message("  Cohort timing status: ", timing_output)
if (matching_uses_lags) {
  message("  Matching columns: ", matching_columns_output)
  message("  Matching components: ", matching_components_output)
}
