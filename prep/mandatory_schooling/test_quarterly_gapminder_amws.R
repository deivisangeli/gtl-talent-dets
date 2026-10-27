script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
repo_root <- if (length(script)) {
  normalizePath(file.path(dirname(script[1]), '../..'), winslash = '/')
} else Sys.getenv('GTL_REPO', getwd())
source(file.path(repo_root, 'paths.R'))
source(file.path(repo_root, 'prep/mandatory_schooling/monthly_amws_helpers.R'))
source(file.path(repo_root, 'prep/mandatory_schooling/quarterly_amws_helpers.R'))
expect_failure <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), 'error'))
stopifnot(identical(amws_calendar_quarter(c(1:12, NA_integer_)),
                    c(rep(1:4, each = 3L), NA_integer_)))
expect_failure(amws_calendar_quarter(c(0, 13)))
expect_failure(amws_calendar_quarter(1.5))
annual <- data.table(GEOID = sprintf('%05d', 1001:1005), year = 1800L,
  population = c(1000, 1000, NA, 0, 1000),
  county_births_estimate_year = c(1200, 1200, NA, 0, -12),
  birth_month_completeness = c(.7, NA, .8, NA, NA),
  birth_month_complete = c(FALSE, TRUE, FALSE, TRUE, TRUE),
  monthly_allocation = 'annual_estimate_divided_by_12')
m <- annual[rep(seq_len(.N), each = 12L)]
m[, `:=`(month = rep(1:12, times = nrow(annual)),
  n_amws_1906_1955_dedup = 0L, n_amws_1986 = 0L,
  monthly_allocated_births = county_births_estimate_year / 12)]
m[GEOID == '01001' & month %in% c(1, 3, 4, 12), n_amws_1906_1955_dedup := 1:4]
m[GEOID == '01001' & month %in% c(2, 7), n_amws_1986 := 5:6]
m[GEOID == '01003' & month == 4L, n_amws_1986 := 1L]
m[, n_amws := n_amws_1906_1955_dedup + n_amws_1986]
counts <- aggregate_amws_quarters(m)
q <- amws_quarter_batch(annual, counts)
stopifnot(nrow(q) == 20L, identical(q[GEOID == '01001', n_amws], c(8L, 3L, 6L, 4L)),
  identical(as.character(q[GEOID == '01001', quarter_start]),
            c('1800-01-01', '1800-04-01', '1800-07-01', '1800-10-01')),
  all(q[GEOID == '01002', amws_per_1000_births] == 0),
  all(is.na(q[GEOID %in% c('01003', '01004', '01005'), amws_per_1000_births])),
  all(q[GEOID == '01005', quarterly_allocated_births] == -3),
  all(is.na(q[GEOID == '01004', amws_per_1000_pop])),
  all(q[GEOID == '01001', birth_month_completeness] == .7),
  sum(q$n_amws) == sum(m$n_amws),
  !any(c('month', 'month_start', 'monthly_allocation', 'monthly_allocated_births',
          'county_births_estimate_month') %in% names(q)))
verify_monthly_rates(q$n_amws, q$quarterly_allocated_births, q$amws_per_1000_births)
verify_monthly_rates(q$n_amws, q$population, q$amws_per_1000_pop)
expect_failure(aggregate_amws_quarters(rbind(m, m[1])))
expect_failure(aggregate_amws_quarters(m[-1]))
bad <- copy(m); bad[1, n_amws := 99L]
expect_failure(aggregate_amws_quarters(bad))
bad <- copy(m); bad[1, monthly_allocated_births := NA_real_]
expect_failure(aggregate_amws_quarters(bad))
bad <- copy(counts); bad[1, monthly_birth_sum := 99]
expect_failure(amws_quarter_batch(annual, bad))
message('Quarter boundary, aggregation, zero/missing/nonpositive rates and invalid-input tests passed.')

if ('--integration' %in% commandArgs(TRUE)) {
  dest <- file.path(DATA_OUTPUT, 'mandatory_schooling')
  existing_panels <- file.path(dest, c('county_amws_monthly_1915_1950.csv',
    'county_nhgis_annual_births_1915_1950.csv', 'county_amws_monthly_gapminder_1915_1950.csv',
    'county_gapminder_annual_births_1915_1950.csv', 'county_amws_monthly_gapminder_1800_1960.csv',
    'county_gapminder_annual_births_1800_1960.csv'))
  existing_audits <- unlist(lapply(file.path(dest, c('monthly_1915_1950_audits',
    'monthly_gapminder_1915_1950_audits', 'monthly_gapminder_1800_1960_audits')),
    function(path) sort(list.files(path, full.names = TRUE))))
  protected <- c(existing_panels, existing_audits,
    file.path(DATA_OUTPUT, c('us_panel_county_amws_combined_year.csv', 'us_panel_county_stem_year_1800.csv')),
    file.path(AMWS_OUTPUT, 'amws_combined_us_geocoded.csv'),
    file.path(TALENT_DETS_DATA_DIR, 'Data/processed/amws/amws_ed86_final.csv'),
    file.path(DATA_INPUT, 'new_births_total_number_estimated.csv'))
  stopifnot(all(file.exists(protected)))
  before <- tools::md5sum(protected)
  run_builder <- function() {
    previous <- getwd(); on.exit(setwd(previous)); setwd(tempdir())
    out <- system2(file.path(R.home('bin'), 'Rscript.exe'),
      shQuote(file.path(repo_root, 'prep/mandatory_schooling/build_amws_county_quarter.R')),
      stdout = TRUE, stderr = TRUE)
    status <- attr(out, 'status')
    if (!is.null(status) && status != 0L) stop(paste(out, collapse = '\n'))
    message(paste(out, collapse = '\n'))
  }
  run_builder()
  audit <- file.path(dest, 'quarterly_gapminder_1800_1960_audits')
  output <- file.path(dest, 'county_amws_quarterly_gapminder_1800_1960.csv')
  outputs <- c(output, sort(list.files(audit, full.names = TRUE)))
  first <- tools::md5sum(outputs)
  run_builder()
  stopifnot(identical(first, tools::md5sum(outputs)), identical(before, tools::md5sum(protected)))

  quarterly <- fread(output, colClasses = c(GEOID = 'character', state_id = 'character'))
  stopifnot(nrow(quarterly) == 1991248L, length(unique(quarterly$GEOID)) == 3092L,
    !anyDuplicated(quarterly[, .(GEOID, year, quarter)]),
    all(quarterly[, .N, by = GEOID]$N == 644L), all(quarterly$quarter %in% 1:4),
    identical(sort(unique(quarterly$year)), 1800:1960),
    !is.unsorted(paste(quarterly$GEOID, sprintf('%04d%d', quarterly$year, quarterly$quarter))),
    all(as.IDate(quarterly$quarter_start) == as.IDate(sprintf('%04d-%02d-01',
      quarterly$year, (quarterly$quarter - 1L) * 3L + 1L))))
  verify_monthly_rates(quarterly$n_amws, quarterly$quarterly_allocated_births, quarterly$amws_per_1000_births)
  verify_monthly_rates(quarterly$n_amws, quarterly$population, quarterly$amws_per_1000_pop)
  stopifnot(isTRUE(all.equal(quarterly$quarterly_allocated_births,
    quarterly$county_births_estimate_year / 4, tolerance = 1e-10)),
    identical(quarterly$county_births_estimate_quarter, quarterly$quarterly_allocated_births),
    all(quarterly[n_amws == 0 & quarterly_allocated_births > 0, amws_per_1000_births] == 0),
    all(is.na(quarterly[is.na(quarterly_allocated_births) | quarterly_allocated_births <= 0,
                         amws_per_1000_births])), sum(quarterly$n_amws) == 111701L)
  a <- fread(file.path(dest, 'county_gapminder_annual_births_1800_1960.csv'),
             colClasses = c(GEOID = 'character', state_id = 'character'))
  expected <- a[rep(seq_len(.N), each = 4L)]
  expected[, quarter := rep(1:4, times = nrow(a))]
  setorder(expected, GEOID, year, quarter)
  carried <- setdiff(names(a), c('monthly_allocated_births', 'county_births_estimate_month',
    'monthly_allocation', 'n_early_existing', 'n_1986_existing', 'early_reconciles', 'late_reconciles'))
  stopifnot(identical(as.list(expected[, ..carried]), as.list(quarterly[, ..carried])))
  rm(expected); invisible(gc())
  m <- fread(file.path(dest, 'county_amws_monthly_gapminder_1800_1960.csv'),
    select = c('GEOID', 'year', 'month', 'n_amws_1906_1955_dedup', 'n_amws_1986',
      'n_amws', 'monthly_allocated_births'), colClasses = c(GEOID = 'character'))
  expected_counts <- aggregate_amws_quarters(m)
  count_cols <- c('GEOID', 'year', 'quarter', 'n_amws_1906_1955_dedup', 'n_amws_1986', 'n_amws')
  stopifnot(identical(as.list(expected_counts[, ..count_cols]), as.list(quarterly[, ..count_cols])),
    isTRUE(all.equal(expected_counts$monthly_birth_sum, quarterly$quarterly_allocated_births, tolerance = 1e-10)))
  recon <- fread(file.path(audit, 'quarterly_annual_reconciliation.csv'))
  stopifnot(nrow(recon) == 497812L, all(recon$early_reconciles), all(recon$late_reconciles),
    isTRUE(all.equal(recon$quarterly_birth_sum, recon$annual_births, tolerance = 1e-10)))
  unknown <- fread(file.path(audit, 'scientists_unassigned_quarter.csv'), colClasses = c(GEOID = 'character'))
  monthly_unknown <- fread(file.path(dest, 'monthly_gapminder_1800_1960_audits/scientists_unassigned_month.csv'),
                           colClasses = c(GEOID = 'character'))
  setorderv(monthly_unknown, c('GEOID', 'year', 'source', 'record_id'))
  setorderv(unknown, c('GEOID', 'year', 'source', 'record_id'))
  stopifnot(nrow(unknown) == 2284L, all(is.na(unknown$month)),
    identical(as.list(unknown), as.list(monthly_unknown)),
    identical(unname(tools::md5sum(file.path(audit, 'scientists_unassigned_quarter.csv'))),
              unname(tools::md5sum(file.path(dest,
                'monthly_gapminder_1800_1960_audits/scientists_unassigned_month.csv')))))
  for (name in c('coverage_year', 'coverage_state_year')) {
    cov <- fread(file.path(audit, paste0(name, '.csv')))
    old <- fread(file.path(dest, 'monthly_gapminder_1800_1960_audits', paste0(name, '.csv')))
    sort_cols <- intersect(c('year', 'state_id'), names(cov))
    setorderv(cov, sort_cols); setorderv(old, sort_cols)
    stopifnot(identical(as.list(cov[, names(old), with = FALSE]), as.list(old)),
      all(cov$quarter_rows == cov$county_years * 4L), all(cov$usable_quarters == cov$usable_denominators * 4L))
  }
  message('Quarterly integration passed: 1,991,248 unique rows; exact monthly source aggregation; unchanged annual metadata; denominators and rates; annual/unassigned reconciliation; coverage; deterministic rerun; existing outputs and sources unchanged.')
}
