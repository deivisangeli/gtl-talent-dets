# Independent Gapminder quarterly outcome panel. Run from any directory.
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
repo_root <- if (length(script)) {
  normalizePath(file.path(dirname(script[1]), '../..'), winslash = '/')
} else Sys.getenv('GTL_REPO', getwd())
source(file.path(repo_root, 'paths.R'))
source(file.path(repo_root, 'prep/mandatory_schooling/monthly_amws_helpers.R'))
source(file.path(repo_root, 'prep/mandatory_schooling/amws_panel_gender_helpers.R'))
source(file.path(repo_root, 'prep/mandatory_schooling/quarterly_amws_helpers.R'))
if (length(commandArgs(TRUE))) stop('Usage: build_amws_county_quarter.R (1800-1960 Gapminder only)')
dest <- file.path(DATA_OUTPUT, 'mandatory_schooling')
stage_root <- tempfile('gender_quarterly_stage_', tmpdir = dest)
dir.create(stage_root)
annual_file <- file.path(dest, 'county_gapminder_annual_births_1800_1960.csv')
monthly_file <- file.path(dest, 'county_amws_monthly_gapminder_1800_1960.csv')
monthly_audit <- file.path(dest, 'monthly_gapminder_1800_1960_audits')
unassigned_file <- file.path(monthly_audit, 'scientists_unassigned_month.csv')
source_annual_file <- file.path(DATA_OUTPUT, 'us_panel_county_amws_combined_year.csv')
gapminder_file <- file.path(DATA_INPUT, 'new_births_total_number_estimated.csv')
inputs <- c(annual_file, monthly_file, unassigned_file, source_annual_file, gapminder_file)
if (any(!file.exists(inputs))) stop('Build the validated full-period monthly Gapminder panel first')
before <- tools::md5sum(inputs)
annual <- fread(annual_file, colClasses = c(GEOID = 'character', state_id = 'character'))
if (!'gender_inference_threshold' %in% names(annual) || any(annual$gender_inference_threshold != 0.8)) {
  stop('Rebuild the full-period monthly panel with saved gender flags first')
}
amws_validate_gender_annual(annual)
universe <- sort(unique(annual$GEOID))
years <- sort(unique(annual$year))
if (length(universe) != 3092L || !identical(years, 1800:1960) ||
    anyDuplicated(annual[, .(GEOID, year)]) || nrow(annual) != length(universe) * length(years) ||
    anyNA(annual[, .(GEOID, year)]) || any(!grepl('^[0-9]{5}$', annual$GEOID))) {
  stop('Unexpected full-period annual county grid')
}
national <- validate_gapminder_annual(annual, gapminder_file, years)
setorder(annual, GEOID, year)

source_annual <- fread(source_annual_file, select = c('GEOID', 'year',
  'n_amws_1906_1955_dedup', 'n_amws_1986'))
source_annual[, GEOID := monthly_geoid(GEOID)]
source_annual <- source_annual[GEOID %in% universe & year %in% years]
if (anyDuplicated(source_annual[, .(GEOID, year)])) stop('Duplicate source annual keys')
setnames(source_annual, c('n_amws_1906_1955_dedup', 'n_amws_1986'),
         c('source_early', 'source_1986'))
annual <- merge(annual, source_annual, by = c('GEOID', 'year'), all.x = TRUE)
if (any(annual$annual_panel_present != !is.na(annual$source_early)) ||
    any(annual[annual_panel_present == TRUE,
      n_early_year != source_early | n_1986_year != source_1986])) stop('Annual source totals changed')

unassigned <- fread(unassigned_file, colClasses = c(GEOID = 'character'))
if (any(!is.na(unassigned$month)) || any(!unassigned$GEOID %in% universe) ||
    any(!unassigned$year %in% years) || any(!unassigned$source %in% c('1906_1955_dedup', '1986')) ||
    anyDuplicated(unassigned[, .(source, record_id)])) stop('Invalid unassigned-month audit')
u <- unassigned[, .(audit_early = sum(source == '1906_1955_dedup'),
                     audit_1986 = sum(source == '1986')), by = .(GEOID, year)]
annual <- merge(annual, u, by = c('GEOID', 'year'), all.x = TRUE)
for (col in c('audit_early', 'audit_1986')) set(annual, which(is.na(annual[[col]])), col, 0L)
if (any(annual$audit_early != annual$n_early_unassigned |
        annual$audit_1986 != annual$n_1986_unassigned)) stop('Unassigned-source totals changed')

message('Reading validated monthly counts and denominators...')
monthly <- fread(monthly_file, select = c('GEOID', 'year', 'month',
  'n_amws_1906_1955_dedup', 'n_amws_1986', 'n_amws', 'monthly_allocated_births', amws_gender_period_cols()),
  colClasses = c(GEOID = 'character'))
if (nrow(monthly) != 5973744L || !setequal(monthly$GEOID, universe) ||
    !setequal(monthly$year, years) || any(monthly[, .N, by = .(GEOID, year)]$N != 12L)) {
  stop('Unexpected full-period monthly county grid')
}
counts <- aggregate_amws_quarters(monthly)
rm(monthly); invisible(gc())
if (nrow(counts) != 1991248L) stop('Unexpected number of quarters')

audit <- file.path(stage_root, 'quarterly_gapminder_1800_1960_audits')
dir.create(audit, showWarnings = FALSE)
save_audit <- function(x, name) {
  x <- copy(x)
  order_cols <- intersect(c('GEOID', 'year', 'quarter', 'state_id', 'source', 'record_id'), names(x))
  if (nrow(x) && length(order_cols)) setorderv(x, order_cols)
  fwrite(x, file.path(audit, paste0(name, '.csv')), na = '')
}
output_name <- 'county_amws_quarterly_gapminder_1800_1960.csv'
output <- file.path(stage_root, output_name)
batches <- split(universe, ceiling(seq_along(universe) / 100L))
reconciliations <- vector('list', length(batches))
gender_reconciliations <- vector('list', length(batches))
rows <- scientists <- 0L
internal <- c('source_early', 'source_1986', 'audit_early', 'audit_1986')
for (i in seq_along(batches)) {
  a <- annual[GEOID %in% batches[[i]]]
  panel_a <- copy(a); panel_a[, (internal) := NULL]
  q <- amws_quarter_batch(panel_a, counts[GEOID %in% batches[[i]]])
  if (anyDuplicated(q[, .(GEOID, year, quarter)]) ||
      nrow(q) != length(batches[[i]]) * 644L || any(q[, .N, by = GEOID]$N != 644L) ||
      any(!q$quarter %in% 1:4)) stop('Quarterly grid failed')
  verify_monthly_rates(q$n_amws, q$quarterly_allocated_births, q$amws_per_1000_births)
  verify_monthly_rates(q$n_amws, q$population, q$amws_per_1000_pop)
  recon <- q[, .(quarterly_birth_sum = if (all(is.na(quarterly_allocated_births))) NA_real_
                  else sum(quarterly_allocated_births),
    early_quarter_sum = sum(n_amws_1906_1955_dedup),
    late_quarter_sum = sum(n_amws_1986)), by = .(GEOID, year)]
  recon <- merge(recon, a[, .(GEOID, year, annual_panel_present, annual_births = county_births_estimate_year,
    n_early_assigned, n_1986_assigned, n_early_unassigned, n_1986_unassigned,
    n_early_year, n_1986_year, source_early, source_1986)], by = c('GEOID', 'year'))
  recon[, `:=`(early_reconciles = early_quarter_sum + n_early_unassigned == n_early_year &
                 (!annual_panel_present | early_quarter_sum + n_early_unassigned == source_early),
    late_reconciles = late_quarter_sum + n_1986_unassigned == n_1986_year &
                 (!annual_panel_present | late_quarter_sum + n_1986_unassigned == source_1986))]
  if (any(!recon$early_reconciles | !recon$late_reconciles) ||
      any(recon$early_quarter_sum != recon$n_early_assigned | recon$late_quarter_sum != recon$n_1986_assigned) ||
      !isTRUE(all.equal(recon$quarterly_birth_sum, recon$annual_births, tolerance = 1e-10))) {
    stop('Quarterly annual reconciliation failed')
  }
  reconciliations[[i]] <- recon
  gender_reconciliations[[i]] <- amws_gender_reconciliation(q, a, 'quarterly')
  fwrite(q, output, na = '', append = i > 1L)
  rows <- rows + nrow(q); scientists <- scientists + sum(q$n_amws)
  if (i %% 5L == 0L || i == length(batches)) message('Wrote counties ', min(i * 100L, length(universe)), '/', length(universe))
}
if (rows != 1991248L || scientists != sum(annual$n_early_assigned + annual$n_1986_assigned)) {
  stop('Quarterly total rows/counts failed')
}
save_audit(rbindlist(reconciliations), 'quarterly_annual_reconciliation')
save_audit(rbindlist(gender_reconciliations), 'gender_quarterly_annual_reconciliation')
# Retain the original CSV verbatim: re-encoding OCR quote runs can change their
# interpretation on import. Every record here is unassigned to any quarter.
if (!file.copy(unassigned_file, file.path(audit, 'scientists_unassigned_quarter.csv'),
               overwrite = TRUE)) stop('Could not preserve the unassigned-scientist audit')
for (name in c('scientist_gender_assignments', 'scientists_no_saved_gender_inference',
               'gender_classification_coverage', 'gender_coverage_state_year',
               'no_saved_inference_denominator_coverage')) {
  if (!file.copy(file.path(monthly_audit, paste0(name, '.csv')), file.path(audit, paste0(name, '.csv')),
                 overwrite = TRUE)) stop('Could not copy gender provenance audit: ', name)
}
if (!file.copy(file.path(monthly_audit, 'scientists_unassigned_month_by_gender.csv'),
               file.path(audit, 'scientists_unassigned_quarter_by_gender.csv'), overwrite = TRUE)) {
  stop('Could not copy the gender-specific unassigned audit')
}
save_audit(annual[, .(GEOID, first_nominal_year, historical_name_variants, historical_names,
  known_geography_change, late_nominal_coverage, geography_caution, geography_note,
  geography_source)][!duplicated(GEOID)], 'county_geography_flags')
coverage <- annual[, .(county_years = .N, observed_estimated_births = sum(!is.na(county_births_estimate_year)),
  zero_births = sum(county_births_estimate_year == 0, na.rm = TRUE),
  missing_births = sum(is.na(county_births_estimate_year)),
  scientists = sum(n_early_year + n_1986_year),
  assigned_scientists = sum(n_early_assigned + n_1986_assigned),
  unassigned_scientists = sum(n_unassigned_month),
  usable_denominators = sum(county_births_estimate_year > 0, na.rm = TRUE)), by = .(state_id, year)]
old_coverage <- fread(file.path(monthly_audit, 'coverage_state_year.csv'), colClasses = c(state_id = 'character'))
setorder(coverage, state_id, year); setorder(old_coverage, state_id, year)
if (!isTRUE(all.equal(as.list(coverage), as.list(old_coverage[, names(coverage), with = FALSE])))) {
  stop('Quarterly denominator/scientist coverage differs from monthly coverage')
}
coverage[, `:=`(quarter_rows = county_years * 4L, usable_quarters = usable_denominators * 4L)]
save_audit(coverage, 'coverage_state_year')
save_audit(coverage[, lapply(.SD, sum), by = year,
  .SDcols = setdiff(names(coverage), c('state_id', 'year'))], 'coverage_year')
save_audit(national, 'gapminder_national_birth_verification')
manifest_inputs <- c(inputs, file.path(monthly_audit, 'coverage_state_year.csv'))
save_audit(data.table(file = normalizePath(manifest_inputs, winslash = '/'),
  md5 = unname(tools::md5sum(manifest_inputs))), 'input_manifest')
if (!identical(before, tools::md5sum(inputs))) stop('Input hashes changed during quarterly build')
message('Validating staged quarterly output against every original panel column...')
amws_verify_staged_panel(output, file.path(dest, output_name), rows)
amws_install_staged(stage_root, dest)
message('Wrote ', rows, ' rows; ', length(universe), ' counties; ', scientists,
        ' assigned scientist births; ', nrow(unassigned), ' unassigned; ',
        sum(annual$county_births_estimate_year > 0, na.rm = TRUE), ' usable county-year denominators.')
