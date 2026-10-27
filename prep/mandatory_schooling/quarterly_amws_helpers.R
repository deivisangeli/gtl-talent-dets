# Shared quarterly aggregation and validation; uses the established rate rules.
suppressPackageStartupMessages(library(data.table))

amws_calendar_quarter <- function(month) {
  if (any(!is.na(month) & (month != as.integer(month) | !month %in% 1:12))) {
    stop('Birth months must be integers from 1 to 12')
  }
  as.integer((month - 1L) %/% 3L + 1L)
}

aggregate_amws_quarters <- function(monthly) {
  required <- c('GEOID', 'year', 'month', 'n_amws_1906_1955_dedup', 'n_amws_1986',
                'n_amws', 'monthly_allocated_births')
  if (!all(required %in% names(monthly))) stop('Missing monthly aggregation fields')
  if (anyNA(monthly[, .(GEOID, year, month)]) ||
      anyDuplicated(monthly[, .(GEOID, year, month)])) stop('Invalid or duplicate monthly keys')
  gender_cols <- if (exists('amws_gender_period_cols')) intersect(amws_gender_period_cols(), names(monthly)) else character()
  if (length(gender_cols) && length(gender_cols) != 9L) stop('Incomplete monthly gender-count schema')
  count_cols <- c('n_amws_1906_1955_dedup', 'n_amws_1986', 'n_amws', gender_cols)
  for (col in count_cols) {
    x <- monthly[[col]]
    if (anyNA(x) || any(!is.finite(x) | x < 0 | x != floor(x))) stop('Invalid scientist counts')
  }
  if (any(monthly$n_amws != monthly$n_amws_1906_1955_dedup + monthly$n_amws_1986)) {
    stop('Monthly source counts do not sum to the combined count')
  }
  if (any(!is.na(monthly$monthly_allocated_births) &
          !is.finite(monthly$monthly_allocated_births))) stop('Nonfinite monthly births')
  monthly <- copy(monthly)
  monthly[, quarter := amws_calendar_quarter(month)]
  counts <- monthly[, .(
    months = .N,
    n_amws_1906_1955_dedup = sum(n_amws_1906_1955_dedup),
    n_amws_1986 = sum(n_amws_1986), n_amws = sum(n_amws),
    missing_birth_months = sum(is.na(monthly_allocated_births)),
    monthly_birth_sum = if (all(is.na(monthly_allocated_births))) NA_real_
                       else sum(monthly_allocated_births)), by = .(GEOID, year, quarter)]
  if (any(counts$months != 3L) || any(!counts$missing_birth_months %in% c(0L, 3L))) {
    stop('Each quarter requires three months with consistent denominator availability')
  }
  counts[, c('months', 'missing_birth_months') := NULL]
  if (length(gender_cols)) {
    split_counts <- monthly[, lapply(.SD, sum), by = .(GEOID, year, quarter), .SDcols = gender_cols]
    counts <- merge(counts, split_counts, by = c('GEOID', 'year', 'quarter'))
  }
  setorder(counts, GEOID, year, quarter)
  counts
}

amws_quarter_batch <- function(annual, counts) {
  out <- copy(annual)[rep(seq_len(.N), each = 4L)]
  out[, quarter := rep(1:4, times = nrow(annual))]
  out <- merge(out, counts, by = c('GEOID', 'year', 'quarter'), all.x = TRUE)
  if (anyNA(out$n_amws)) stop('Quarterly counts do not cover the annual grid')
  obsolete <- intersect(c('monthly_allocated_births', 'county_births_estimate_month',
    'monthly_allocation', 'n_early_existing', 'n_1986_existing',
    'early_reconciles', 'late_reconciles'), names(out))
  out[, (obsolete) := NULL]
  out[, `:=`(quarter_start = as.IDate(sprintf('%04d-%02d-01', year, (quarter - 1L) * 3L + 1L)),
    quarterly_allocated_births = county_births_estimate_year / 4,
    county_births_estimate_quarter = county_births_estimate_year / 4,
    quarterly_allocation = 'annual_estimate_divided_by_4')]
  if (!isTRUE(all.equal(out$monthly_birth_sum, out$quarterly_allocated_births,
                       tolerance = 1e-10, check.attributes = FALSE))) {
    stop('Quarterly denominators differ from the sum of three monthly denominators')
  }
  out[, `:=`(amws_per_1000_births = monthly_rate(n_amws, quarterly_allocated_births),
             amws_per_1000_pop = monthly_rate(n_amws, population))]
  out[, monthly_birth_sum := NULL]
  if (exists('amws_gender_period_cols') && all(amws_gender_period_cols() %in% names(out))) {
    out <- amws_add_gender_rates(out, 'quarterly_allocated_births')
  }
  setorder(out, GEOID, year, quarter)
  out
}
