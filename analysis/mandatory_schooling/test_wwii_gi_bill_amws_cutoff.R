# Synthetic design tests; --integration validates the saved real-data run without rebuilding inputs.
arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
root <- if (length(arg)) normalizePath(file.path(dirname(sub('^--file=', '', arg[1])), '../..'),
  winslash = '/') else Sys.getenv('GTL_REPO', getwd())
source(file.path(root, 'analysis/mandatory_schooling/wwii_gi_bill_amws_helpers.R'))
assert_error <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), 'error'))
d <- CJ(GEOID = sprintf('%05d', 1:30), year = 1923:1930, month = 1:12)
d[, county := as.integer(GEOID)]
d[, monthly_allocated_births := 100000 * county]
d[, x := 12 * (year - 1927) + month - 12]
d[, rate := 20 + county / 10 + month / 10 + 4 * (x <= 0) +
    .03 * x * (x <= 0) + .05 * x * (x > 0) + ((county * 11 + month * 7 + year * 13) %% 17 - 8) / 100]
d[, n_amws_male_80 := round(rate * monthly_allocated_births / 1000)]
d[, `:=`(n_amws_female_80 = 0, n_amws_unclassified_80 = 0,
          n_amws = n_amws_male_80, n_amws_1906_1955_dedup = n_amws_male_80, n_amws_1986 = 0)]
base <- gi_balance(d, 1925:1930)
stopifnot(nrow(base$data) == 30 * 72, all(base$audit$retained))
m <- gi_period_data(base$data, 'monthly'); q <- gi_period_data(base$data, 'quarterly')
stopifnot(all(m[year == 1927 & season == 12, x] == 0),
  all(m[year == 1928 & season == 1, x] == 1),
  all(q[year == 1927 & season == 4, x] == 0),
  all(q[year == 1928 & season == 1, x] == 1), all(m$earlier == (m$x <= 0)),
  nrow(q) == 30 * 24, sum(q$births) == sum(m$births),
  sum(q$n_amws_male_80) == sum(m$n_amws_male_80), all(q$n_amws_female_80 == 0))
fit <- gi_fit(m, 'male')
stopifnot(abs(fit$estimates$estimate - 4) < .01, fit$estimates$df == 29,
          fit$estimates$estimate > 0, nobs(fit$fit) == nrow(m))
# Quarter-centering implies the mean monthly pre-side bin is x_m = -1 at x_q = 0.
# This changes the estimand by the difference in slopes, even with identical input counts.
quarter_fit <- gi_fit(q, 'male')
stopifnot(abs(quarter_fit$estimates$estimate - (4 - .03 + .05)) < .02)
actual <- copy(q); actual[, quarterly_allocated_births := births]
stopifnot(gi_reconcile_quarters(base$data, actual)$count_mismatches == 0)
bad <- copy(actual); bad[1, n_amws_male_80 := n_amws_male_80 + 1]
assert_error(gi_reconcile_quarters(base$data, bad))
bad <- copy(d)
bad <- bad[!(GEOID == '00001' & year == 1926 & month == 1)]
bad[GEOID == '00002' & year == 1926 & month == 1, monthly_allocated_births := NA_real_]
bad[GEOID == '00003' & year == 1926 & month == 1, monthly_allocated_births := 0]
bad[GEOID == '00004' & year == 1926 & month == 1, n_amws_male_80 := -1]
bad[GEOID == '00005' & year == 1926 & month == 1, n_amws_male_80 := 0]
bad[GEOID == '00006' & year == 1923 & month == 1, monthly_allocated_births := NA_real_]
balanced <- gi_balance(bad, 1925:1930)
expanded <- gi_balance(bad, 1923:1930)
common <- intersect(balanced$data$GEOID, expanded$data$GEOID)
stopifnot(nrow(balanced$data) == 26 * 72, !any(sprintf('%05d', 1:4) %in% balanced$data$GEOID),
  balanced$data[GEOID == '00005' & year == 1926 & month == 1, n_amws_male_80] == 0,
  '00006' %in% balanced$data$GEOID, !'00006' %in% common,
  uniqueN(common) == 25)
gap <- gi_balance(d, setdiff(1925:1930, 1928))$data
stopifnot(!any(gap$year == 1928), nrow(gap) == 30 * 60, nrow(gi_period_data(gap, 'quarterly')) == 30 * 20)
message('Synthetic tests passed: centering/sign, trend recovery, quarter aggregation, sample exclusions, zeros, common support.')

if ('--integration' %in% commandArgs(TRUE)) {
  paths <- gi_paths(root); out <- paths$results
  e <- fread(file.path(out, 'estimates.csv'))
  specs <- fread(file.path(out, 'specifications.csv'))
  stopifnot(nrow(e) == 60, !anyDuplicated(e$model_id), setequal(e$model_id, specs$model_id),
    all(e$df == pmin(e$counties, e$cohort_clusters) - 1),
    all(abs(e$p_value - 2 * pt(-abs(e$estimate / e$std_error), e$df)) < 1e-10),
    all(abs(e$ci_lower - (e$estimate - qt(.975, e$df) * e$std_error)) < 1e-10),
    all(abs(e$ci_upper - (e$estimate + qt(.975, e$df) * e$std_error)) < 1e-10))
  b <- e[window == 'baseline' & support == 'window_specific' & weighting == 'births' & seasonal]
  stopifnot(all(b$counties == 3051), all(b[resolution == 'monthly', observations] == 219672),
    all(b[resolution == 'quarterly', observations] == 73224),
    all(abs(b$estimated_births - 16588557) < 1))
  # Counts change with a published AMWS source refresh. Reconcile the primary
  # male/female/unclassified models with the current balanced raw panel instead
  # of asserting the previous data vintage's scientist totals.
  current <- gi_read_inputs(paths$files)
  baseline_panel <- gi_balance(current$monthly, gi_windows$baseline$years)$data
  current_totals <- vapply(gi_count_columns, function(column) sum(baseline_panel[[column]]), integer(1L))
  stopifnot(identical(b[resolution == 'monthly'][order(match(outcome, gi_groups)), assigned_scientists],
                     unname(current_totals)),
    identical(b[resolution == 'quarterly'][order(match(outcome, gi_groups)), assigned_scientists],
              unname(current_totals)))
  rm(current, baseline_panel); invisible(gc())
  p <- fread(file.path(out, 'cohort_summaries.csv'))
  for (id in e$model_id) {
    fit <- readRDS(file.path(out, 'models', paste0(id, '.rds')))
    row <- e[model_id == id]
    stopifnot(abs(coef(fit)[['earlier']] - row$estimate) < 1e-12,
      nobs(fit) == row$observations, nrow(p[model_id == id]) == row$cohort_clusters,
      sum(p[model_id == id, assigned_scientists]) == row$assigned_scientists,
      abs(sum(p[model_id == id, estimated_births]) - row$estimated_births) < 1e-6)
  }
  audit <- fread(file.path(out, 'county_sample_audit.csv'))
  stopifnot(nrow(audit) == 3092 * 4, all(audit[retained == TRUE, invalid_birth_cells] == 0),
    all(audit[retained == TRUE, invalid_count_cells] == 0))
  annual <- fread(file.path(out, 'audit_annual.csv'))
  stopifnot(nrow(annual) == 8, all(annual$assigned == annual$male_assigned +
    annual$female_assigned + annual$unclassified_assigned))
  manifest <- fread(file.path(out, 'input_manifest.csv'))
  stopifnot(identical(unname(tools::md5sum(manifest$path)), manifest$md5))
  message('Integration checks passed: all 60 saved models, inference, baseline support, audits and unchanged input hashes.')
}
