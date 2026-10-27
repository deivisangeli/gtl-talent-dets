###############################################################################
# County x five-year panel combining the consolidated AMWS editions with the
# annually interpolated population and birth denominators, 1800-1960.
#
# Built under both binning conventions (see prep/amws/period5_bins.R). Unlike
# the decade builder this reads only the annual combined panel: that file
# already carries interpolated population, its source, and the estimated county
# births, so no decennial merge and no Virginia independent-city fallback is
# needed.
#
# Aggregation is to ANNUAL AVERAGES within each bin: counts and estimated
# births are bin totals divided by the number of contributing years, and
# population is the bin mean. Rates are recomputed from the aggregated
# numerator and denominator; a rate is never itself averaged.
###############################################################################
suppressPackageStartupMessages(library(data.table))

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/", mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE
  )
}
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "prep", "amws", "period5_bins.R"))

# paths.R still defaults to a coauthor's home directory. Fall back to this
# machine's Dropbox folder when that default is absent and no override is set,
# matching what the annual analysis runner already does.
if (!dir.exists(TALENT_DETS_DATA_DIR) && !nzchar(Sys.getenv("TALENT_DETS_DATA_DIR"))) {
  TALENT_DETS_DATA_DIR <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  AMWS_OUTPUT <- file.path(DATA_OUTPUT, "amws")
}
stopifnot(dir.exists(DATA_OUTPUT), dir.exists(AMWS_OUTPUT))

normalize_geoid <- function(x) sprintf("%05d", suppressWarnings(as.integer(x)))

annual_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")

annual <- fread(annual_file)
required_annual <- c(
  "GEOID", "year", "population", "population_source",
  "county_births_estimate_year", "n_amws_1906_1955_dedup", "n_amws_1986", "n_amws"
)
missing_annual <- setdiff(required_annual, names(annual))
if (length(missing_annual)) {
  stop("Missing annual columns: ", paste(missing_annual, collapse = ", "))
}
annual <- annual[, ..required_annual]
annual[, `:=`(GEOID = normalize_geoid(GEOID), year = as.integer(year))]
annual <- annual[year >= 1800L & year <= 1960L]
setorder(annual, GEOID, year)

if (anyDuplicated(annual[, .(GEOID, year)])) stop("Annual panel is not unique by GEOID-year")
if (annual[, any(n_amws != n_amws_1906_1955_dedup + n_amws_1986)]) {
  stop("AMWS components do not reconcile in the annual panel")
}
expected_totals <- annual[, .(
  early = sum(n_amws_1906_1955_dedup, na.rm = TRUE),
  ed1986 = sum(n_amws_1986, na.rm = TRUE),
  total = sum(n_amws, na.rm = TRUE)
)]

# A bin's value is the mean over its years, so a bin missing any denominator
# year is left missing rather than averaged over a shorter, different window.
mean_or_na <- function(x) if (all(is.finite(x))) mean(x) else NA_real_
sum_or_na <- function(x) if (all(is.finite(x))) sum(x) else NA_real_

build_period5_panel <- function(dt, convention) {
  d <- copy(dt)
  d[, period := period5_bin(year, convention)]
  out <- d[, {
    years <- .N
    births <- sum_or_na(county_births_estimate_year)
    pop <- mean_or_na(population)
    early <- sum(n_amws_1906_1955_dedup, na.rm = TRUE) / years
    ed1986 <- sum(n_amws_1986, na.rm = TRUE) / years
    total <- sum(n_amws, na.rm = TRUE) / years
    .(
      n_years = years,
      complete_period = years == 5L,
      first_year = min(year),
      last_year = max(year),
      population = pop,
      population_source = population_source[1L],
      population_source_mixed = uniqueN(population_source) > 1L,
      county_births_estimate_year = if (is.na(births)) NA_real_ else births / years,
      n_amws_1906_1955_dedup = early,
      n_amws_1986 = ed1986,
      n_amws = total
    )
  }, by = .(GEOID, period)]

  if (out[, max(abs(n_amws - (n_amws_1906_1955_dedup + n_amws_1986)))] > 1e-9) {
    stop("AMWS components do not reconcile after ", convention, " five-year aggregation")
  }
  out[, `:=`(
    amws_per_1000_pop = fifelse(
      is.finite(population) & population > 0, 1000 * n_amws / population, NA_real_
    ),
    amws_per_1000_births = fifelse(
      is.finite(county_births_estimate_year) & county_births_estimate_year > 0,
      1000 * n_amws / county_births_estimate_year, NA_real_
    ),
    log1p_n_amws = log1p(n_amws)
  )]
  setorder(out, GEOID, period)
  out
}

for (convention in period5_conventions) {
  out <- build_period5_panel(annual, convention)

  actual_totals <- out[, .(
    early = sum(n_amws_1906_1955_dedup * n_years),
    ed1986 = sum(n_amws_1986 * n_years),
    total = sum(n_amws * n_years)
  )]
  if (max(abs(as.numeric(expected_totals) - as.numeric(actual_totals))) > 1e-6) {
    stop("Annual and five-year AMWS totals do not reconcile for ", convention)
  }

  suffix <- period5_suffix(convention)
  output_file <- file.path(
    DATA_OUTPUT, paste0("us_panel_county_amws_combined_period5", suffix, ".csv")
  )
  summary_file <- file.path(
    AMWS_OUTPUT, paste0("amws_consolidated_county_period5", suffix, "_summary.csv")
  )
  fwrite(out, output_file)

  summary_dt <- data.table(
    metric = c(
      "binning_convention", "panel_rows", "panel_counties",
      "panel_min_period", "panel_max_period",
      "complete_periods", "incomplete_periods", "mixed_population_source_rows",
      "n_amws_1906_1955_dedup", "n_amws_1986", "n_amws_total",
      "missing_population_rows", "missing_births_rows"
    ),
    value = c(
      convention, nrow(out), uniqueN(out$GEOID),
      min(out$period), max(out$period),
      out[complete_period == TRUE, .N], out[complete_period == FALSE, .N],
      out[population_source_mixed == TRUE, .N],
      actual_totals$early, actual_totals$ed1986, actual_totals$total,
      out[!is.finite(population), .N], out[!is.finite(county_births_estimate_year), .N]
    )
  )
  fwrite(summary_dt, summary_file)

  cat("wrote", output_file, "\n")
  cat("wrote", summary_file, "\n")
  cat(
    convention, "rows:", nrow(out), " counties:", uniqueN(out$GEOID),
    " periods:", min(out$period), "-", max(out$period),
    " AMWS births:", round(actual_totals$total), "\n"
  )
}
