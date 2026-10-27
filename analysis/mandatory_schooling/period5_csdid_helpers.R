###############################################################################
# Five-year-bin configuration for the compulsory-schooling C&S event studies.
# Estimation primitives are shared with the annual design; only the cadence,
# the profiles and the input reader differ.
#
# The 14-year exposure allowance carries over unchanged. did applies
# `anticipation` in the units of the time variable, so anticipation = 14 on the
# five-year grid reproduces the annual rule exactly:
#   * the universal base period is the latest bin strictly before g - 14, which
#     on a five-year grid is the bin at event time -15, as in the annual design;
#   * a not-yet-treated donor needs g_donor > max(t_base, t_event) + 14, and
#     since every bin is a multiple of five that means its law bin starts at
#     least 15 years after the comparison bin -- the annual condition exactly.
# The direct unconditional DiD check in the runner re-derives both rules from
# the panel and would fail if this mapping were wrong.
###############################################################################

# ---------------------------------------------------------------------------
# Post-treatment horizons
#
# The not-yet-treated donor pool is finite: every state had a compulsory law by
# 1920, and a comparison at calendar bin t needs a donor whose law bin starts at
# least 15 years later. The latest law bin is 1920 (MS), so 1905 is the last
# usable comparison bin -- did finds this on its own and truncates its processed
# panel there. A cohort g can therefore be followed only to event time 1905 - g:
#
#   g       1850  1860  1865  1870  1875  1880
#   max e   +55   +45   +40   +35   +30   +25
#
# Reaching horizon P requires the target window to satisfy g <= 1905 - P. A
# capped cohort is not discarded: it becomes an eligible not-yet-treated donor,
# which is what it is.
#
# The bound is an inequality, not an equality. post20 keeps the paper's
# substantive 1850-1880 window, which is tighter than its own ceiling of 1885
# would allow -- widening it to 1885 would pull in the raw-1888 adopters and
# change the estimand, not just the horizon. post30's 1875 is the ceiling
# binding. post20 also keeps its directory suffix and seed base so its
# published results are reproduced exactly.
# ---------------------------------------------------------------------------
PERIOD5_LAST_COMPARISON_BIN <- 1905L

period5_horizons <- list(
  post20 = list(post = 20L, target_max = 1880L, seed_base = 20260916L),
  post30 = list(post = 30L, target_max = 1875L, seed_base = 20260946L)
)

# The widest target window any horizon uses, so a shorter-horizon run can report
# what its cap gave up relative to the design as a whole.
period5_widest_target_max <- function() {
  max(vapply(period5_horizons, function(h) h$target_max, integer(1L)))
}

period5_cadence <- function(horizon = "post20") {
  stopifnot(length(horizon) == 1L, horizon %in% names(period5_horizons))
  h <- period5_horizons[[horizon]]
  csdid_cadence(
    step = 5L,
    anticipation = 14L,
    label = "5-year-bin C&S",
    shade_label = "anticipation window (bins -10 and -5)",
    x_label = "Five-year bin relative to statewide law effect",
    dir_suffix = sprintf("_post%d_ref15", h$post),
    exposure_note = paste(
      "births up to 14 years before the law may be exposed, so the reference bin",
      "is e = -15 and bins -10 and -5 are not placebo leads"
    ),
    seed_base = h$seed_base
  )
}

# pre20 has only two unexposed bins (-20 and -15). A two-point line interpolates
# exactly, which would force the detrended estimate to zero at both, so pre20
# runs levels only. The unexposed-bin count does not depend on the horizon, so
# this holds for every horizon. Expected sample sizes are pinned per horizon and
# convention after the calibration run; NA skips the check.
period5_profiles <- list(
  pre20 = list(pre = 20L, specifications = "levels"),
  pre40 = list(pre = 40L, specifications = c("levels", "detrended"))
)

# Calibrated on the built panels. The two conventions differ in sample because
# binning moves whole states between cohorts: the raw-1883 adopters (IL, MT,
# ND, RI, SD) bin to 1880 under `standard` and so enter the 1850-1880 target
# window, but to 1885 under `alt` and so do not; WA (1878) and WI (1879) move
# the other way, from the 1875 cohort to 1880. See cohort_bin_mapping.csv.
#
# The post30 cap costs the two conventions different things. Under `standard` it
# removes exactly the raw-1883 adopters, leaving the annual paper's own target
# set; under `alt` it removes WA and WI, whose raw law years are inside the
# 1850-1880 window, which is a genuine loss.
period5_expected <- list(
  post20 = list(
    standard = list(pre20 = c(counties = 429L, jurisdictions = 14L),
                    pre40 = c(counties = 287L, jurisdictions = 11L)),
    alt = list(pre20 = c(counties = 310L, jurisdictions = 12L),
               pre40 = c(counties = 193L, jurisdictions = 9L))),
  post30 = list(
    standard = list(pre20 = c(counties = 322L, jurisdictions = 12L),
                    pre40 = c(counties = 195L, jurisdictions = 9L)),
    alt = list(pre20 = c(counties = 280L, jurisdictions = 11L),
               pre40 = c(counties = 193L, jurisdictions = 9L)))
)

# Pin the horizon and the calibrated target counts onto the profile list.
period5_profiles_for <- function(convention, horizon = "post20") {
  stopifnot(length(convention) == 1L, convention %in% period5_conventions,
            length(horizon) == 1L, horizon %in% names(period5_horizons))
  h <- period5_horizons[[horizon]]
  stopifnot(h$target_max <= PERIOD5_LAST_COMPARISON_BIN - h$post)
  expected <- period5_expected[[horizon]][[convention]]
  profiles <- period5_profiles
  for (nm in names(profiles)) {
    profiles[[nm]]$post <- h$post
    profiles[[nm]]$target_max <- h$target_max
    profiles[[nm]]$counties <- unname(expected[[nm]]["counties"])
    profiles[[nm]]$jurisdictions <- unname(expected[[nm]]["jurisdictions"])
  }
  profiles
}

read_period5_inputs <- function(data_root, convention) {
  stopifnot(length(convention) == 1L, convention %in% period5_conventions)
  panel_path <- file.path(data_root, "output", paste0(
    "us_panel_county_amws_combined_period5", period5_suffix(convention), ".csv"))
  laws_path <- file.path(data_root, "input", "compulsory_schooling_laws.csv")
  panel <- data.table::fread(panel_path, colClasses = c(GEOID = "character"))
  laws <- csdid_read_laws(laws_path)
  required <- unique(c("GEOID", "period", "n_years", "complete_period", "population",
                       "population_source", "county_births_estimate_year",
                       "n_amws_1906_1955_dedup", "n_amws_1986", names(annual_outcomes)))
  stopifnot(all(required %in% names(panel)))
  panel <- panel[, ..required]
  panel[, GEOID := sprintf("%05d", as.integer(GEOID))]
  stopifnot(!anyDuplicated(panel[, .(GEOID, period)]))
  # Bin values are annual averages, so totals reconcile only once each bin is
  # weighted by the years that produced it. Check that before trusting anything.
  stopifnot(
    max(abs(panel$n_amws - (panel$n_amws_1906_1955_dedup + panel$n_amws_1986))) < 1e-9,
    all(panel$n_amws >= 0),
    all(panel$population_source %in% c("nhgis", "manual", "merged_nyc")))
  supplied_totals <- panel[, .(rows = .N, counties = data.table::uniqueN(GEOID),
                               amws_total = sum(n_amws * n_years))]
  # Reconcile with the current release's annual source, not a historic total.
  annual_totals <- data.table::fread(file.path(data_root,
    "output/amws/amws_consolidated_county_year_summary.csv"))
  source_total <- function(requested_metric) {
    value <- annual_totals[metric == requested_metric, value]
    stopifnot(length(value) == 1L)
    as.numeric(value)
  }
  stopifnot(abs(sum(panel$n_amws * panel$n_years) - source_total("total_amws_births_in_panel")) < 1e-6,
    abs(sum(panel$n_amws_1906_1955_dedup * panel$n_years) - source_total("amws_1906_1955_dedup_births_in_panel")) < 1e-6,
    abs(sum(panel$n_amws_1986 * panel$n_years) - source_total("amws_1986_births_in_panel")) < 1e-6)
  # A partial bin averages over a shorter, different window; never pooled with
  # complete bins. Under `alt` these are the 1800 and 1960 edge bins only.
  partial_bins <- panel[complete_period == FALSE, .N]
  panel <- panel[complete_period == TRUE]
  data.table::setnames(panel, "period", "year")
  panel <- csdid_attach_treatment(panel, laws,
    bin_law = function(y) period5_bin(y, convention))
  cohort_mapping <- laws[, .(state_fips, state_abbr, state, compulsory_law_year,
                             g_standard = period5_bin(compulsory_law_year, "standard"),
                             g_alt = period5_bin(compulsory_law_year, "alt"))]
  cohort_mapping[, `:=`(binning_convention = convention,
                        g = period5_bin(compulsory_law_year, convention))]
  data.table::setorder(cohort_mapping, state_fips)
  list(panel = panel, paths = c(panel = panel_path, laws = laws_path),
       convention = convention, cohort_mapping = cohort_mapping,
       supplied_totals = supplied_totals, partial_bin_rows = partial_bins,
       input_totals = panel[, .(rows = .N, counties = data.table::uniqueN(id),
                                amws_total = sum(n_amws * n_years))])
}
