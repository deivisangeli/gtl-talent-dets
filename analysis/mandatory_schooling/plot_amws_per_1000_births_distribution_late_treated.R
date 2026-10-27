###############################################################################
# Project: GTL talent determinants - mandatory schooling pipeline.
# Goal:    The county distribution of AMWS scientists per 1,000 estimated
#          births, restricted to the 16 states whose first compulsory-schooling
#          law took effect after 1900, against the rest of the contiguous US.
#
# These 16 states carry essentially all of the pipeline's modern
# treatment-timing variation, so the shape of the outcome there is what the
# compulsory-schooling event studies are actually averaging over.
#
# Companion to plot_amws_per_1000_births_distribution.R, which draws the same
# figure for all CONUS counties. Both share
# amws_birth_rate_distribution_helpers.R.
#
# READ THIS BEFORE READING THE FIGURE. It is descriptive, not causal:
#
#   1. THIS IS LARGELY A SOUTH-VERSUS-REST CONTRAST, NOT A LAW CONTRAST.
#      Eleven of the 16 late adopters are the CLS-coded South and they cluster
#      1913-1920. Law-adoption decade is nearly collinear with region, and the
#      gradient is flat within the non-South. The level gap below should be
#      read as regional composition first. Nothing in this figure identifies
#      an effect of a compulsory-schooling law on anything.
#   2. The treatment date is Lingwall (2014), Table 6.2: the first year a law
#      was in effect STATEWIDE, not the year of enactment. Alabama's statute is
#      enacted 1915 and effective 1917. The 16-state set is the same under
#      either reading, and under every law vintage the repo holds - the ordered
#      law years straddle the cut as 1899 (AZ) -> 1902 (IA, MD), so no state
#      sits near enough to move. The vintages disagree about how far post-1900
#      the Southern adopters are, never about who they are.
#   3. county_births_estimate_year is NOT measured births. It is county
#      population x the NATIONAL crude birth rate for that year
#      (prep/cleaning_county_year_1800.R:254-259).
#   4. Interval rates are a RATIO OF SUMS, not the mean of annual county rates.
#   5. The three intervals are NOT a like-for-like time series. AMWS coverage
#      is three editions (1906/1938/1955 deduplicated, plus 1986 which is not
#      deduplicated against them), so the rising level across intervals is
#      partly coverage, not only talent production.
#   6. 1800-1849 is kept although it is 95.1% zeros on 35 scientists across 614
#      counties. That these states had almost no AMWS-producing counties before
#      1850 is the finding, not a defect - but the panel is near-degenerate and
#      should not be read as a distribution.
#   7. Counties below AMWS_DIST_MIN_BIRTHS estimated births are FLAGGED, not
#      filtered. In 1850-1899 every late-adopter county above the plotted cap
#      is one of them; the maximum, 9.44, is 2 scientists over 212 estimated
#      births.
#   8. 1950-1960 is excluded as an 11-year, right-censored remainder.
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/plot_amws_per_1000_births_distribution_late_treated.R
#
# Inputs:
#   output/us_panel_county_amws_combined_year.csv
#     (built by prep/amws/build_amws_combined_county_year.R)
#   input/compulsory_schooling_laws.csv  (Lingwall 2014, Table 6.2)
#
# Output (results/mandatory_schooling/amws_birth_rate_distribution/):
#   amws_per_1000_births_distribution_50yr_late_treated.png
#   amws_per_1000_births_distribution_50yr_late_treated.pdf
#   amws_per_1000_births_county_by_period_late_treated.csv
#   amws_per_1000_births_period_summary_late_treated.csv
#   validation_summary_late_treated.csv
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
LAWS_FILE <- file.path(data_root, "input", "compulsory_schooling_laws.csv")
OUT_DIR <- file.path(
  data_root, "results", "mandatory_schooling", "amws_birth_rate_distribution"
)
OUTPUT_STEM <- file.path(
  OUT_DIR, "amws_per_1000_births_distribution_50yr_late_treated"
)

missing_inputs <- c(PANEL_FILE, LAWS_FILE)[!file.exists(c(PANEL_FILE, LAWS_FILE))]
if (length(missing_inputs)) {
  stop(
    "Missing input(s):\n  ", paste(missing_inputs, collapse = "\n  "),
    "\nRun prep/amws/build_amws_combined_county_year.R first."
  )
}
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

ledger <- amws_dist_check_ledger()
expect <- ledger$expect

# ------------------------------------------------- treatment-state groups -- #

# csdid_read_laws() is the pipeline's only guard on the law VINTAGE - it greps
# the source string for Lingwall (2014), Table 6.2. Read through it rather than
# hardcoding years, which is how the project's figure volume once sat on the
# pre-Lingwall dates undetected.
laws <- csdid_read_laws(LAWS_FILE)

late_treated <- laws[is.finite(compulsory_law_year) & compulsory_law_year > 1900L]
inclusive_cut <- laws[is.finite(compulsory_law_year) & compulsory_law_year >= 1900L]

EXPECTED_LATE_STATES <- c(
  "IA", "MD", "MO", "DE", "OK", "NC", "TN", "LA",
  "TX", "AL", "AR", "GA", "VA", "FL", "SC", "MS"
)
expect(
  "Post-1900 set matches the validated 16 states",
  nrow(late_treated) == 16L && setequal(late_treated$state_abbr, EXPECTED_LATE_STATES),
  paste(sort(late_treated$state_abbr), collapse = " ")
)
# The repo uses `> 1900` here and `>= 1900` in describe_cls_policy_environment.R.
# They agree only because no jurisdiction has a law year of 1900 or 1901; assert
# it so a future revision landing in that gap is caught rather than absorbed.
expect(
  "Strict and inclusive 1900 cuts select the same states",
  setequal(late_treated$state_abbr, inclusive_cut$state_abbr),
  paste0(
    "years straddling the cut: ",
    max(laws[is.finite(compulsory_law_year) & compulsory_law_year < 1900L,
             compulsory_law_year]),
    " -> ", min(late_treated$compulsory_law_year)
  )
)

late_fips <- intersect(AMWS_DIST_CONUS_FIPS, late_treated$state_fips)
early_fips <- setdiff(AMWS_DIST_CONUS_FIPS, late_treated$state_fips)

expect(
  "Groups partition CONUS and DC with no overlap",
  !length(intersect(late_fips, early_fips)) &&
    setequal(union(late_fips, early_fips), AMWS_DIST_CONUS_FIPS) &&
    length(late_fips) == 16L && length(early_fips) == 33L,
  paste0("late states: ", length(late_fips), "; early states: ", length(early_fips))
)

# ------------------------------------------------------------- constants -- #

# The expected_* columns are validated aggregates that
# amws_map_build_period_rates() asserts against and stops on drift, the same
# guard the county-map scripts carry. Frozen from the 2026-09-21 run of
# us_panel_county_amws_combined_year.csv (AMWS total 112,291). Per interval
# these sum across the two groups to the national script's totals.
base_periods <- data.table(
  period     = c("1800-1849", "1850-1899", "1900-1949"),
  start_year = c(1800L, 1850L, 1900L),
  end_year   = c(1849L, 1899L, 1949L)
)

group_specs <- list(
  late = list(
    label = "Law in effect after 1900 (16 states)",
    colour = "#315a7d",
    style = "fill",
    fips = late_fips,
    periods = copy(base_periods)[, `:=`(
      expected_amws = c(35, 3691, 18476),
      expected_estimated_births = c(
        9136351.965877259, 29938676.440598514, 47390248.561931871
      ),
      expected_counties = c(614L, 1254L, 1443L),
      expected_positive_counties = c(30L, 752L, 1305L)
    )]
  ),
  early = list(
    label = "Law in effect by 1900 (33 states and DC)",
    colour = "#bc4749",
    style = "outline",
    fips = early_fips,
    periods = copy(base_periods)[, `:=`(
      expected_amws = c(502, 15925, 72072),
      expected_estimated_births = c(
        14874998.19086514, 55291786.07486974, 98414851.92669329
      ),
      expected_counties = c(567L, 1417L, 1641L),
      expected_positive_counties = c(177L, 1116L, 1557L)
    )]
  )
)

# --------------------------------------------------------------- compute -- #

panel <- amws_map_read_panel(PANEL_FILE)

period_rates <- rbindlist(lapply(names(group_specs), function(key) {
  spec <- group_specs[[key]]
  rates <- amws_map_build_period_rates(
    panel, spec$periods, eligible_state_fips = spec$fips
  )
  rates[, group := key]
  rates[]
}))
period_rates[, thin_denominator := estimated_births < AMWS_DIST_MIN_BIRTHS]

expect(
  "Three 50-year intervals present for both groups",
  nrow(unique(period_rates[, .(group, period)])) == 6L,
  paste(sort(unique(as.character(period_rates$period))), collapse = ", ")
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
  ""
)
expect(
  "One row per county-interval-group",
  !anyDuplicated(period_rates[, .(group, period, GEOID)]),
  ""
)
expect(
  "No county appears in both groups",
  !nrow(period_rates[, .(groups = uniqueN(group)), by = .(period, GEOID)][groups > 1L]),
  ""
)

period_summary <- amws_dist_summarise(period_rates)
period_summary <- merge(
  base_periods, period_summary, by = "period", sort = FALSE
)
setorder(period_summary, start_year, group)

# The national script reports these same totals over all CONUS counties; the
# two groups must reconstruct them exactly, or the partition has leaked.
national_totals <- data.table(
  period = c("1800-1849", "1850-1899", "1900-1949"),
  national_amws = c(537, 19616, 90548),
  national_counties = c(1181L, 2671L, 3084L)
)
reconstructed <- period_summary[, .(
  amws = sum(total_amws), counties = sum(n_counties)
), by = period]
reconstructed <- merge(reconstructed, national_totals, by = "period")
expect(
  "Groups reconstruct the national interval totals",
  all(reconstructed$amws == reconstructed$national_amws) &&
    all(reconstructed$counties == reconstructed$national_counties),
  paste0(
    "AMWS ", paste(reconstructed$amws, collapse = "/"),
    " vs ", paste(reconstructed$national_amws, collapse = "/")
  )
)
expect(
  "Pooled rate reproduces from the interval totals",
  all(abs(
    period_summary$pooled_rate_per_1000 -
      1000 * period_summary$total_amws / period_summary$total_estimated_births
  ) < 1e-9),
  ""
)

# ------------------------------------------------------------- write CSVs -- #

county_out <- period_rates[, .(
  GEOID, state_fips, group, period, n_amws, estimated_births,
  amws_per_1000_births, thin_denominator
)]
setorder(county_out, period, group, GEOID)

fwrite(county_out, file.path(
  OUT_DIR, "amws_per_1000_births_county_by_period_late_treated.csv"
))
fwrite(period_summary, file.path(
  OUT_DIR, "amws_per_1000_births_period_summary_late_treated.csv"
))

# ---------------------------------------------------------------- figure -- #

groups <- data.frame(
  group = names(group_specs),
  label = vapply(group_specs, function(s) s$label, character(1)),
  colour = vapply(group_specs, function(s) s$colour, character(1)),
  style = vapply(group_specs, function(s) s$style, character(1)),
  stringsAsFactors = FALSE
)

annotation <- period_summary[, {
  ordered <- .SD[match(names(group_specs), group)]
  .(label = paste(
    sprintf(
      "%-9s pooled %.3f   mean %.3f   median %.3f   zero %.1f%% of %s",
      c("post-1900", "by 1900"),
      ordered$pooled_rate_per_1000, ordered$county_mean, ordered$county_median,
      100 * ordered$zero_share,
      format(ordered$n_counties, big.mark = ",", trim = TRUE)
    ),
    collapse = "\n"
  ))
}, by = period]

overall_max <- max(period_rates$amws_per_1000_births)
total_above_cap <- sum(period_summary$n_above_cap)
total_above_cap_thin <- sum(period_summary$n_above_cap_thin)
late_gap_1900 <- period_summary[
  period == "1900-1949",
  pooled_rate_per_1000[group == "early"] / pooled_rate_per_1000[group == "late"]
]

figure <- amws_dist_build_figure(
  period_rates = period_rates,
  period_summary = period_summary,
  groups = groups,
  period_levels = base_periods$period,
  title = paste0(
    "AMWS scientists born per 1,000 estimated births: ",
    "late-adopting states against the rest"
  ),
  subtitle = paste0(
    "Counties of the 16 states whose compulsory-schooling law took effect after ",
    "1900 (Lingwall 2014, Table 6.2), by 50-year birth cohort"
  ),
  caption = paste(
    sprintf(
      "Late adopters run below the rest of the country throughout, by %.2fx in 1900-1949. This is largely a South-versus-rest contrast:",
      late_gap_1900
    ),
    paste(
      "11 of the 16 are the CLS-coded South, so adoption timing is nearly collinear",
      "with region. Nothing here identifies a law effect."
    ),
    sprintf(
      "Rates above %.1f are shown in the final bin: %d county-intervals, observed maximum %.2f; %d of them have under %s estimated births.",
      AMWS_DIST_RATE_CAP, total_above_cap, overall_max, total_above_cap_thin,
      format(AMWS_DIST_MIN_BIRTHS, big.mark = ",", trim = TRUE)
    ),
    paste(
      "Births are estimated as county population x the national crude birth rate.",
      "The intervals are not comparable as a time series:"
    ),
    paste(
      "AMWS coverage comes from different editions by cohort. 1950-1960 is excluded",
      "as an 11-year, right-censored remainder."
    ),
    sep = "\n"
  ),
  x_label = unname(annual_outcomes[["amws_per_1000_births"]]),
  annotation = annotation
)

figure_files <- amws_dist_save_figure(figure, OUTPUT_STEM, width = 10, height = 9.5)
expect(
  "Figure written as PNG and PDF",
  all(file.exists(figure_files)) && all(file.size(figure_files) > 0),
  paste(basename(figure_files), file.size(figure_files), collapse = "; ")
)

# ---------------------------------------------------------------- report -- #

validation <- ledger$results()
fwrite(validation, file.path(OUT_DIR, "validation_summary_late_treated.csv"))

cat("\nLate-adopting states (law in effect after 1900):\n  ")
cat(paste(sort(late_treated$state_abbr), collapse = " "), "\n")
cat("\nAMWS per-1,000-births county distribution by group, 50-year intervals:\n\n")
print(as.data.frame(period_summary), row.names = FALSE, digits = 4)

cat("\nValidation:\n")
print(as.data.frame(validation), row.names = FALSE)

cat(
  "\nDescriptive only. Eleven of the sixteen late adopters are the CLS-coded",
  "\nSouth, so this contrast is regional composition before it is anything",
  "\nabout schooling law. Do not read the gap as an effect.\n"
)
message("\nSaved to: ", OUT_DIR)

if (!all(validation$validation_passed)) {
  stop(sum(!validation$validation_passed), " validation check(s) FAILED.")
}
