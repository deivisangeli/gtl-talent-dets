###############################################################################
# State-level compulsory-schooling C&S helpers.
#
# Treatment varies only by state, so this design takes the state as the unit:
# AMWS births, population and estimated births are SUMMED over every valid
# county-period of the county panels, and the rates are recomputed from the
# sums, never averaged. The estimation primitives in annual_csdid_helpers.R are
# reused unchanged; only the readers, labels and output checks differ.
#
# Three consequences of the unit change worth keeping in mind:
#   * states are unweighted -- each counts once in att_gt and in the cohort
#     aggregation, so DC weighs as much as New York;
#   * a state's aggregate covers whichever of its counties exist in a period,
#     so composition grows as territories organise (state_coverage.csv reports
#     it for every target window; nothing is filtered on it);
#   * clustering on state_id is resampling units, and with ~12 treated states
#     the bootstrap rests on few clusters.
###############################################################################

state_outcomes <- c(
  amws_per_1000_pop = "AMWS scientists born per 1,000 population",
  n_amws = "AMWS scientists born",
  log1p_n_amws = "log(1 + AMWS scientists born)",
  amws_per_1000_births = "AMWS scientists born per 1,000 estimated births",
  population = "State population",
  births_estimate_year = "Estimated state births"
)

state_figure_file_count <- function() 4L * length(state_outcomes)

STATE_SEED_BASE <- 20260923L

# Pinned target-state counts, calibrated on the built panels (2026-09-23). A
# state is a target once its aggregate exists in every event period, so these
# could exceed the county design's jurisdictions; in every block they equal
# them, with the same states. NA would skip the check.
state_expected <- list(
  annual = list(pre20 = 12L, pre40 = 9L),
  period5 = list(
    post20 = list(standard = list(pre20 = 14L, pre40 = 11L),
                  alt = list(pre20 = 12L, pre40 = 9L)),
    post30 = list(standard = list(pre20 = 12L, pre40 = 9L),
                  alt = list(pre20 = 11L, pre40 = 9L)))
)

# ---------------------------------------------------------------------------
# Target windows
#
# early      : laws of 1850-1880 (1875 under the post30 cap), every target
#              followed over the whole event grid -- the balanced design.
# all_events : every adopter through the last comparison period. Every state
#              had a law by 1920 and a not-yet-treated donor must adopt at
#              least 15 years after the comparison period, so nothing after
#              1905 has any donor (did truncates its panel there on its own),
#              and 1905 itself has only Mississippi. A cell resting on one
#              donor state has a degenerate influence function and no standard
#              error, so the horizon stops at the last period with at least
#              STATE_MIN_DONOR_STATES donors: 1904 annually (FL, SC, MS). A
#              cohort g is identified only to e = last - g, so the pooled
#              curve is RAGGED: each point averages the cohorts identified
#              there. Later adopters get no post-law cell and stay donors.
# ---------------------------------------------------------------------------
STATE_MIN_DONOR_STATES <- 2L

state_windows <- list(
  early = list(ragged = FALSE, results_dir = "compulsory_schooling_amws_state_csdid",
               seed_offset = 0L, label = NULL),
  all_events = list(ragged = TRUE, results_dir = "compulsory_schooling_amws_state_csdid_all_events",
                    seed_offset = 500000L, label = "all events to the donor horizon")
)

# Pinned target-state counts for the all_events window, calibrated 2026-09-23.
# Horizons do not change them: under a ragged grid post30 only lengthens the
# curve. alt bins DE (1907) and MO (1905) into the 1905 bin, which alt keeps.
state_expected_all_events <- list(
  annual = list(pre20 = 22L, pre40 = 16L),
  period5 = list(
    post20 = list(standard = list(pre20 = 22L, pre40 = 16L),
                  alt = list(pre20 = 24L, pre40 = 18L)),
    post30 = list(standard = list(pre20 = 22L, pre40 = 16L),
                  alt = list(pre20 = 24L, pre40 = 18L)))
)

# Not-yet-treated donor states observed in each period: states with a valid
# row whose law comes more than `anticipation` years later. did keeps only
# t < max(g) - anticipation when no unit is never-treated.
state_donor_counts <- function(panel, anticipation = 14L) {
  d <- panel[valid_outcomes == TRUE & !is.na(g)]
  latest_g <- max(d$g)
  periods <- sort(unique(d$year))
  periods <- periods[periods < latest_g - anticipation]
  data.table::data.table(year = periods, donor_states = vapply(periods, function(t)
    data.table::uniqueN(d[year == t & g > t + anticipation, id]), integer(1L)))
}

# The latest period with at least `min_donor_states` donors: 1904 annually,
# 1900 for standard bins (FL and SC bin to 1915, leaving MS alone in 1920) and
# 1905 for alt bins (FL, SC and MS all bin to 1920).
state_last_comparison <- function(panel, anticipation = 14L,
                                  min_donor_states = STATE_MIN_DONOR_STATES) {
  donors <- state_donor_counts(panel, anticipation)
  max(donors[donor_states >= min_donor_states, year])
}

# Profiles carry the state count in `jurisdictions`; `counties` is NA so the
# county-count check inside build_annual_sample never fires. all_events targets
# every cohort that has at least its law period inside the donor horizon.
state_window_profiles <- function(profiles, window, expected_early, expected_all,
                                  last_comparison) {
  for (nm in names(profiles)) {
    profiles[[nm]]$county_design_jurisdictions <- profiles[[nm]]$jurisdictions
    profiles[[nm]]$counties <- NA_integer_
    if (window == "all_events") {
      profiles[[nm]]$target_max <- last_comparison
      profiles[[nm]]$jurisdictions <- expected_all[[nm]]
    } else {
      profiles[[nm]]$jurisdictions <- expected_early[[nm]]
    }
  }
  profiles
}

state_annual_profiles <- function(window = "early", last_comparison = NA_integer_) {
  state_window_profiles(annual_profiles, window, state_expected$annual,
                        state_expected_all_events$annual, last_comparison)
}

state_period5_profiles <- function(convention, horizon, window = "early",
                                   last_comparison = NA_integer_) {
  state_window_profiles(period5_profiles_for(convention, horizon), window,
                        state_expected$period5[[horizon]][[convention]],
                        state_expected_all_events$period5[[horizon]][[convention]],
                        last_comparison)
}

state_annual_cadence <- function(window = "early") {
  csdid_cadence(label = "Annual C&S, state panel",
                seed_base = STATE_SEED_BASE + state_windows[[window]]$seed_offset)
}

state_period5_cadence <- function(horizon, window = "early") {
  cadence <- period5_cadence(horizon)
  cadence$label <- "5-year-bin C&S, state panel"
  cadence$seed_base <- STATE_SEED_BASE + 100000L +
    match(horizon, names(period5_horizons)) * 1000L + state_windows[[window]]$seed_offset
  cadence
}

# annual_support flags every cell without donors. In a ragged run the cells
# after the last comparison period have none by construction; they are
# relabelled so that any OTHER unsupported cell still stops the run.
state_support <- function(panel, weights, grid, anticipation, step, last_comparison, ragged) {
  support <- annual_support(panel, weights, grid, anticipation, step)
  if (ragged) support[year > last_comparison & status != "reference",
                      status := "beyond_donor_horizon"]
  support
}

state_allowed_support <- c("supported", "reference", "beyond_donor_horizon")

# Mark the cohort cells did never estimated because they lie past the donor
# horizon, and insist that every other cell is estimated.
state_mark_beyond <- function(cells, last_comparison) {
  cells[year > last_comparison, status := "beyond_donor_horizon"]
  stopifnot(all(is.na(cells[status == "beyond_donor_horizon", att])),
            all(is.finite(cells[status != "beyond_donor_horizon", att])),
            all(is.finite(cells[status != "beyond_donor_horizon", se])))
  cells
}

# Caption line describing how the pooled curve's composition changes.
state_ragged_note <- function(dynamic, post, last_comparison) {
  at <- function(e) dynamic[event_time == e, n_units][1L]
  sprintf(paste("Ragged: fewer than %d donor states after %d, so the pooled estimate covers",
                "%d states at e = 0 and %d at e = +%d."),
          STATE_MIN_DONOR_STATES, last_comparison, at(0L), at(post), post)
}

# Sum a county panel to state x period. `panel` carries GEOID, `time_col`,
# population, county_births_estimate_year and the AMWS counts; five-year panels
# also carry n_years, since their values are annual averages within a bin, and
# totals reconcile only once weighted by it. A county-period is dropped when any
# summed quantity is non-finite or a denominator is not positive, exactly the
# county design's valid_outcomes rule; the AMWS births dropped with it are
# reported, never silently lost.
aggregate_county_panel_to_state <- function(panel, time_col = "year") {
  counts <- c("n_amws", "n_amws_1906_1955_dedup", "n_amws_1986")
  stopifnot(all(c("GEOID", time_col, "population", "county_births_estimate_year",
                  counts) %in% names(panel)))
  d <- data.table::copy(panel)
  if (!"n_years" %in% names(d)) d[, n_years := 1L]
  data.table::setnames(d, time_col, "year")
  d[, state_fips := substr(GEOID, 1L, 2L)]
  sums <- c("population", "county_births_estimate_year", counts)
  d[, valid := Reduce(`&`, lapply(.SD, is.finite)) & population > 0 &
      county_births_estimate_year > 0, .SDcols = sums]
  dropped <- d[valid == FALSE, .(dropped_rows = .N,
                                 dropped_amws_births = sum(n_amws * n_years, na.rm = TRUE)),
               by = state_fips]
  state <- d[valid == TRUE, c(list(n_counties = .N, n_years = n_years[1L],
                                   n_years_consistent = data.table::uniqueN(n_years) == 1L),
                              lapply(.SD, sum)),
             by = .(state_fips, year), .SDcols = sums]
  stopifnot(all(state$n_years_consistent))
  state[, n_years_consistent := NULL]
  data.table::setnames(state, "county_births_estimate_year", "births_estimate_year")
  state[, `:=`(amws_per_1000_pop = 1000 * n_amws / population,
               amws_per_1000_births = 1000 * n_amws / births_estimate_year,
               log1p_n_amws = log1p(n_amws))]
  stopifnot(
    max(abs(state$n_amws - state$n_amws_1906_1955_dedup - state$n_amws_1986)) < 1e-9,
    abs(sum(state$n_amws * state$n_years) + sum(dropped$dropped_amws_births) -
          sum(d$n_amws * d$n_years, na.rm = TRUE)) < 1e-6,
    !anyDuplicated(state[, .(state_fips, year)]))
  data.table::setorder(state, state_fips, year)
  list(panel = state[], dropped = dropped)
}

# State analogue of csdid_attach_treatment. GEOID becomes the two-digit state
# code so build_annual_sample's grouping and target columns work unchanged.
# state_id stays a column distinct from id (with the same values): did drops a
# clustervar equal to idname by name, which would break fit_annual_csdid's
# cluster check.
state_attach_treatment <- function(panel, laws, bin_law = as.integer) {
  panel <- merge(panel, laws[, .(state_fips, state_abbr, state,
                                g = bin_law(compulsory_law_year))],
                 by = "state_fips", all.x = TRUE, sort = FALSE)
  panel[, valid_outcomes := Reduce(`&`, lapply(.SD, is.finite)) &
          population > 0 & births_estimate_year > 0, .SDcols = names(state_outcomes)]
  panel[, `:=`(GEOID = state_fips, id = as.integer(state_fips),
               state_id = as.integer(state_fips), event_time = year - g)]
  data.table::setorder(panel, id, year)
  panel[]
}

state_county_columns <- c("GEOID", "population", "population_source",
                          "county_births_estimate_year", "n_amws",
                          "n_amws_1906_1955_dedup", "n_amws_1986")

read_state_annual_inputs <- function(data_root) {
  panel_path <- file.path(data_root, "output", "us_panel_county_amws_combined_year.csv")
  laws_path <- file.path(data_root, "input", "compulsory_schooling_laws.csv")
  county <- data.table::fread(panel_path, colClasses = c(GEOID = "character"))
  laws <- csdid_read_laws(laws_path)
  required <- c(state_county_columns, "year")
  stopifnot(all(required %in% names(county)))
  county <- county[, ..required]
  county[, GEOID := sprintf("%05d", as.integer(GEOID))]
  stopifnot(!anyDuplicated(county[, .(GEOID, year)]),
            all(county$n_amws == county$n_amws_1906_1955_dedup + county$n_amws_1986),
            all(county$n_amws >= 0), sum(county$n_amws) == 112291,
            sum(county$n_amws_1906_1955_dedup) == 49587,
            sum(county$n_amws_1986) == 62704,
            all(county$population_source %in% c("nhgis", "manual", "merged_nyc")))
  aggregated <- aggregate_county_panel_to_state(county, "year")
  panel <- state_attach_treatment(aggregated$panel, laws)
  list(panel = panel, paths = c(panel = panel_path, laws = laws_path),
       convention = NA_character_, dropped = aggregated$dropped,
       input_totals = data.table::data.table(
         county_rows = nrow(county), counties = data.table::uniqueN(county$GEOID),
         county_amws_total = sum(county$n_amws), state_rows = nrow(panel),
         states = data.table::uniqueN(panel$id), state_amws_total = sum(panel$n_amws)))
}

# Sums the complete bins of the county five-year panel, so each state bin
# covers the counties with a complete bin. Partial edge bins are dropped before
# aggregation, as in the county design.
read_state_period5_inputs <- function(data_root, convention) {
  stopifnot(length(convention) == 1L, convention %in% period5_conventions)
  panel_path <- file.path(data_root, "output", paste0(
    "us_panel_county_amws_combined_period5", period5_suffix(convention), ".csv"))
  laws_path <- file.path(data_root, "input", "compulsory_schooling_laws.csv")
  county <- data.table::fread(panel_path, colClasses = c(GEOID = "character"))
  laws <- csdid_read_laws(laws_path)
  required <- c(state_county_columns, "period", "n_years", "complete_period")
  stopifnot(all(required %in% names(county)))
  county <- county[, ..required]
  county[, GEOID := sprintf("%05d", as.integer(GEOID))]
  stopifnot(
    !anyDuplicated(county[, .(GEOID, period)]),
    max(abs(county$n_amws - (county$n_amws_1906_1955_dedup + county$n_amws_1986))) < 1e-9,
    all(county$n_amws >= 0),
    abs(sum(county$n_amws * county$n_years) - 112291) < 1e-6,
    abs(sum(county$n_amws_1906_1955_dedup * county$n_years) - 49587) < 1e-6,
    abs(sum(county$n_amws_1986 * county$n_years) - 62704) < 1e-6,
    all(county$population_source %in% c("nhgis", "manual", "merged_nyc")))
  partial_bins <- county[complete_period == FALSE, .N]
  county <- county[complete_period == TRUE]
  county[, complete_period := NULL]
  aggregated <- aggregate_county_panel_to_state(county, "period")
  panel <- state_attach_treatment(aggregated$panel, laws,
    bin_law = function(y) period5_bin(y, convention))
  cohort_mapping <- laws[, .(state_fips, state_abbr, state, compulsory_law_year,
                             g_standard = period5_bin(compulsory_law_year, "standard"),
                             g_alt = period5_bin(compulsory_law_year, "alt"))]
  cohort_mapping[, `:=`(binning_convention = convention,
                        g = period5_bin(compulsory_law_year, convention))]
  data.table::setorder(cohort_mapping, state_fips)
  list(panel = panel, paths = c(panel = panel_path, laws = laws_path),
       convention = convention, cohort_mapping = cohort_mapping,
       partial_bin_rows = partial_bins, dropped = aggregated$dropped,
       input_totals = data.table::data.table(
         county_rows = nrow(county), counties = data.table::uniqueN(county$GEOID),
         county_amws_total = sum(county$n_amws * county$n_years),
         state_rows = nrow(panel), states = data.table::uniqueN(panel$id),
         state_amws_total = sum(panel$n_amws * panel$n_years)))
}

# How much each target state's county composition moves inside its event
# window. Reported, not filtered: a growing county count means the state
# aggregate covers more territory at the end of the window than at the start.
state_coverage_audit <- function(panel, targets, grid, ref) {
  window <- panel[id %in% targets$id & event_time %in% grid]
  window[, .(
    state_abbr = state_abbr[1L], g = g[1L],
    counties_at_start = n_counties[event_time == min(grid)],
    counties_at_reference = n_counties[event_time == ref],
    counties_at_end = n_counties[event_time == max(grid)],
    min_counties = min(n_counties), max_counties = max(n_counties),
    population_at_reference = population[event_time == ref],
    population_ratio_end_to_start = population[event_time == max(grid)] /
      population[event_time == min(grid)],
    composition_changes = sum(diff(n_counties[order(event_time)]) != 0L)
  ), by = .(id, state_fips)][order(g, state_fips)]
}

state_code_manifest <- function(runner) {
  paths <- file.path(repo_root, c(
    file.path("analysis", "mandatory_schooling", c(
      runner, "state_csdid_helpers.R", "annual_csdid_helpers.R",
      "csdid_figure_helpers.R", "period5_csdid_helpers.R", "test_state_csdid.R")),
    file.path("prep", "amws", "period5_bins.R")))
  data.table::data.table(file = basename(paths), md5 = unname(tools::md5sum(paths)))
}

# Which specifications a profile runs, and why the other is skipped. Mirrors
# the five-year county runner: a two-point linear fit interpolates exactly, so
# a cohort trend is not identified from the two unexposed bins of pre20.
period5_specification_status <- function(profile, cadence) {
  both <- c("levels", "detrended")
  requested <- csdid_profile_specifications(profile)
  unexposed <- sum(seq.int(-profile$pre, profile$post, by = cadence$step) <= cadence$ref)
  data.table::data.table(
    specification = both,
    status = data.table::fifelse(both %in% requested, "estimated", "skipped"),
    reason = data.table::fifelse(both %in% requested, "",
      sprintf("insufficient unexposed bins (%d; three needed for a cohort trend)", unexposed)),
    unexposed_bins_including_reference = unexposed
  )
}
