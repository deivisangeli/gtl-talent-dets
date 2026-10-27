###############################################################################
# Project: GTL talent determinants - mandatory schooling pipeline.
# Goal:    Describe how AMWS scientists per 1,000 estimated births is
#          distributed ACROSS COUNTIES within 50-year birth-cohort intervals.
#
# The pipeline already uses amws_per_1000_births as its headline outcome, but
# only ever shows its spatial pattern (the county maps) or its event-study
# path. This script shows the shape of the thing being averaged.
#
# For the same figure restricted to the states whose compulsory-schooling law
# took effect after 1900, see
# plot_amws_per_1000_births_distribution_late_treated.R.
#
# READ THIS BEFORE READING THE FIGURE. It is descriptive, not causal:
#
#   1. county_births_estimate_year is NOT measured births. It is county
#      population x the NATIONAL crude birth rate for that year
#      (prep/cleaning_county_year_1800.R:254-259), so every county in a given
#      year shares one birth rate and the denominator inherits the national
#      age structure.
#   2. Interval rates are a RATIO OF SUMS - numerator and denominator are
#      summed over the interval first, then divided. They are not the mean of
#      the annual county rates. The two differ sharply in the early period,
#      where most county-years carry a zero numerator.
#   3. The three intervals are NOT a like-for-like time series. AMWS coverage
#      is three editions (1906/1938/1955 deduplicated, plus 1986 which is not
#      deduplicated against them), so an 1800s cohort and a 1900s cohort are
#      observed through different books. The rising level across intervals is
#      partly coverage, not only talent production.
#   4. 1950-1960 is deliberately EXCLUDED. It is an 11-year remainder, not a
#      50-year interval, and 84.5% of its counties are zero because people
#      born after 1950 were too young for even the 1986 edition. Plotting it
#      would show right-censoring, not talent. 1900-1949 is already mildly
#      censored at its own top end for the same reason.
#   5. Counties are restricted to the contiguous US plus DC, matching the
#      universe of the existing county maps. The panel carries Hawaii; the
#      maps drop it through the geometry join, which a histogram has no
#      equivalent of, so it is dropped explicitly here.
#   6. Counties below AMWS_DIST_MIN_BIRTHS estimated births are FLAGGED, not
#      filtered. Their rates are arithmetically real but substantively thin -
#      two scientists over 212 estimated births reads as 9.44 per 1,000 - and
#      they dominate the far right tail in 1850-1899.
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/plot_amws_per_1000_births_distribution.R
#
# Input:
#   output/us_panel_county_amws_combined_year.csv
#     (built by prep/amws/build_amws_combined_county_year.R)
#
# Output (results/mandatory_schooling/amws_birth_rate_distribution/):
#   amws_per_1000_births_distribution_50yr.png
#   amws_per_1000_births_distribution_50yr.pdf
#   amws_per_1000_births_county_by_period.csv
#   amws_per_1000_births_period_summary.csv
#   validation_summary.csv
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

# ---------------------------------------------------------------- paths ---- #

helpers_dir <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1L]), winslash = "/"))
  } else {
    file.path(Sys.getenv("GTL_REPO", unset = getwd()), "analysis", "mandatory_schooling")
  }
})
source(file.path(helpers_dir, "amws_birth_rate_distribution_helpers.R"))

repo_root <- amws_dist_resolve_repo_root()
data_root <- amws_dist_resolve_data_root(repo_root)

source(file.path(
  repo_root, "analysis", "mandatory_schooling", "amws_county_map_helpers.R"
))
source(file.path(
  repo_root, "analysis", "mandatory_schooling", "annual_csdid_helpers.R"
))

PANEL_FILE <- file.path(
  data_root, "output", "us_panel_county_amws_combined_year.csv"
)
OUT_DIR <- file.path(
  data_root, "results", "mandatory_schooling", "amws_birth_rate_distribution"
)
OUTPUT_STEM <- file.path(OUT_DIR, "amws_per_1000_births_distribution_50yr")

if (!file.exists(PANEL_FILE)) {
  stop(
    "No annual AMWS panel at ", PANEL_FILE,
    ".\nRun prep/amws/build_amws_combined_county_year.R first."
  )
}
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

ledger <- amws_dist_check_ledger()
expect <- ledger$expect

# ------------------------------------------------------------- constants -- #

# The expected_* columns are validated aggregates that
# amws_map_build_period_rates() asserts against and stops on drift, the same
# guard the county-map scripts carry. Frozen from the 2026-09-21 run of
# us_panel_county_amws_combined_year.csv (AMWS total 112,291).
periods <- data.table(
  period     = c("1800-1849", "1850-1899", "1900-1949"),
  start_year = c(1800L, 1850L, 1900L),
  end_year   = c(1849L, 1899L, 1949L),
  expected_amws = c(537, 19616, 90548),
  expected_estimated_births = c(24011350.1567424, 85230462.5154683, 145805100.488625),
  expected_counties = c(1181L, 2671L, 3084L),
  expected_positive_counties = c(207L, 1868L, 2862L)
)

# --------------------------------------------------------------- compute -- #

panel <- amws_map_read_panel(PANEL_FILE)
period_rates <- amws_map_build_period_rates(
  panel, periods, eligible_state_fips = AMWS_DIST_CONUS_FIPS
)
period_rates[, group := "all"]
period_rates[, thin_denominator := estimated_births < AMWS_DIST_MIN_BIRTHS]

expect(
  "Three 50-year intervals present",
  identical(sort(as.character(unique(period_rates$period))), sort(periods$period)),
  paste(unique(as.character(period_rates$period)), collapse = ", ")
)
expect(
  "County rates are finite and nonnegative",
  all(is.finite(period_rates$amws_per_1000_births)) &&
    all(period_rates$amws_per_1000_births >= 0),
  ""
)
expect(
  "Non-CONUS counties excluded",
  !any(substr(period_rates$GEOID, 1L, 2L) %in% c("02", "15")),
  paste0(
    "Hawaii county-years in the raw 1800-1949 panel: ",
    nrow(panel[substr(GEOID, 1L, 2L) == "15" & year >= 1800L & year <= 1949L])
  )
)
expect(
  "One row per county-interval",
  !anyDuplicated(period_rates[, .(period, GEOID)]),
  ""
)

period_summary <- amws_dist_summarise(period_rates)
period_summary[, group := NULL]
period_summary <- merge(
  periods[, .(period, start_year, end_year)], period_summary,
  by = "period", sort = FALSE
)
setorder(period_summary, start_year)

expect(
  "Pooled rate reproduces from the interval totals",
  all(abs(
    period_summary$pooled_rate_per_1000 -
      1000 * period_summary$total_amws / period_summary$total_estimated_births
  ) < 1e-9),
  ""
)
expect(
  "Interval AMWS totals do not exceed the validated panel total",
  sum(period_summary$total_amws) <= 112291,
  paste0(
    "Sum over 1800-1949 CONUS: ",
    format(sum(period_summary$total_amws), big.mark = ","), " of 112,291"
  )
)

# ------------------------------------------------------------- write CSVs -- #

county_out <- period_rates[, .(
  GEOID, state_fips, period, n_amws, estimated_births, amws_per_1000_births,
  thin_denominator
)]
setorder(county_out, period, GEOID)

fwrite(county_out, file.path(OUT_DIR, "amws_per_1000_births_county_by_period.csv"))
fwrite(period_summary, file.path(OUT_DIR, "amws_per_1000_births_period_summary.csv"))

# ---------------------------------------------------------------- figure -- #

groups <- data.frame(
  group = "all", label = "All CONUS counties",
  colour = "#315a7d", style = "fill", stringsAsFactors = FALSE
)

annotation <- period_summary[, .(
  period,
  label = sprintf(
    "pooled %.3f   county mean %.3f   median %.3f\nzero-rate counties %.1f%% of %s",
    pooled_rate_per_1000, county_mean, county_median,
    100 * zero_share, format(n_counties, big.mark = ",", trim = TRUE)
  )
)]

overall_max <- max(period_rates$amws_per_1000_births)
total_above_cap <- sum(period_summary$n_above_cap)
total_above_cap_thin <- sum(period_summary$n_above_cap_thin)

figure <- amws_dist_build_figure(
  period_rates = period_rates,
  period_summary = period_summary,
  groups = groups,
  period_levels = period_summary$period,
  title = "AMWS scientists born per 1,000 estimated births: distribution across counties",
  subtitle = paste0(
    "Contiguous United States and DC, by 50-year birth cohort | county rate = ",
    "1,000 x (AMWS births in interval) / (estimated births in interval)"
  ),
  caption = paste(
    sprintf(
      "Rates above %.1f are shown in the final bin: %d county-intervals, observed maximum %.2f; %d of them have under %s estimated births.",
      AMWS_DIST_RATE_CAP, total_above_cap, overall_max, total_above_cap_thin,
      format(AMWS_DIST_MIN_BIRTHS, big.mark = ",", trim = TRUE)
    ),
    paste(
      "Births are estimated as county population x the national crude birth rate,",
      "not measured births. The intervals are not"
    ),
    paste(
      "comparable as a time series: AMWS coverage comes from different editions by",
      "cohort. 1950-1960 is excluded as an"
    ),
    "11-year, right-censored remainder.",
    sep = "\n"
  ),
  x_label = unname(annual_outcomes[["amws_per_1000_births"]]),
  annotation = annotation
)

figure_files <- amws_dist_save_figure(figure, OUTPUT_STEM)
expect(
  "Figure written as PNG and PDF",
  all(file.exists(figure_files)) && all(file.size(figure_files) > 0),
  paste(basename(figure_files), file.size(figure_files), collapse = "; ")
)

# ---------------------------------------------------------------- report -- #

validation <- ledger$results()
fwrite(validation, file.path(OUT_DIR, "validation_summary.csv"))

cat("\nAMWS per-1,000-births county distribution, 50-year intervals:\n\n")
print(as.data.frame(period_summary), row.names = FALSE, digits = 4)

cat("\nValidation:\n")
print(as.data.frame(validation), row.names = FALSE)

cat(
  "\nDescriptive only. The denominator is estimated, not measured, and the",
  "\nthree intervals are observed through different AMWS editions - read the",
  "\nlevel shift across intervals as coverage plus talent, not talent alone.\n"
)
message("\nSaved to: ", OUT_DIR)

if (!all(validation$validation_passed)) {
  stop(sum(!validation$validation_passed), " validation check(s) FAILED.")
}
