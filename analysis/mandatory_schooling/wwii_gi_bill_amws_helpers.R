# WWII/GI Bill birth-cohort reduced forms. Counts always use total-birth denominators.
suppressPackageStartupMessages({ library(data.table); library(fixest); library(ggplot2) })

gi_groups <- c('male', 'female', 'unclassified')
gi_count_columns <- paste0('n_amws_', gi_groups, '_80')
gi_windows <- list(
  baseline = list(years = 1925:1930, label = '1925-1930'),
  expanded = list(years = 1923:1930, label = '1923-1930'),
  narrow = list(years = 1926:1929, label = '1926-1929'),
  exclude_1928 = list(years = setdiff(1925:1930, 1928L), label = '1925-1930, omit 1928'))
gi_ssc <- function() fixest::ssc(K.adj = TRUE, K.fixef = 'nonnested',
                                G.adj = TRUE, G.df = 'min', t.df = 'min')

gi_repo_root <- function() {
  arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
  root <- if (length(arg)) file.path(dirname(sub('^--file=', '', arg[1])), '../..') else
    Sys.getenv('GTL_REPO', unset = getwd())
  normalizePath(root, winslash = '/', mustWork = TRUE)
}

gi_paths <- function(repo_root) {
  e <- new.env(parent = baseenv())
  sys.source(file.path(repo_root, 'paths.R'), envir = e)
  if (!dir.exists(e$TALENT_DETS_DATA_DIR))
    stop('Set TALENT_DETS_DATA_DIR to the local Dropbox project directory.')
  root <- normalizePath(e$TALENT_DETS_DATA_DIR, winslash = '/')
  input <- file.path(root, 'output/mandatory_schooling')
  list(root = root, results = file.path(root, 'results/mandatory_schooling/wwii_gi_bill_amws_cutoff'),
       files = c(monthly = file.path(input, 'county_amws_monthly_gapminder_1800_1960.csv'),
                 quarterly = file.path(input, 'county_amws_quarterly_gapminder_1800_1960.csv'),
                 annual = file.path(input, 'county_gapminder_annual_births_1800_1960.csv')))
}

gi_read_inputs <- function(files) {
  if (any(!file.exists(files))) stop('Missing extended AMWS panel input.')
  message('Reading selected columns from the extended monthly and quarterly panels...')
  counts <- c('n_amws', 'n_amws_1906_1955_dedup', 'n_amws_1986', gi_count_columns)
  m <- fread(files[['monthly']], select = c('GEOID', 'year', 'month',
    'monthly_allocated_births', counts, 'gender_inference_threshold'),
    colClasses = c(GEOID = 'character'))[year %in% 1923:1930]
  q <- fread(files[['quarterly']], select = c('GEOID', 'year', 'quarter',
    'quarterly_allocated_births', counts, 'gender_inference_threshold'),
    colClasses = c(GEOID = 'character'))[year %in% 1923:1930]
  a <- fread(files[['annual']], colClasses = c(GEOID = 'character', state_id = 'character'))[
    year %in% 1923:1930]
  for (d in list(m, q, a)) {
    if (anyNA(d$gender_inference_threshold) || any(d$gender_inference_threshold != .8))
      stop('Saved 0.8 gender classification is required.')
    if (anyNA(d[, .(GEOID, year)]) || any(!grepl('^[0-9]{5}$', d$GEOID)))
      stop('Invalid county/year identifiers.')
  }
  if (any(!m$month %in% 1:12) || any(!q$quarter %in% 1:4) ||
      anyDuplicated(m[, .(GEOID, year, month)]) ||
      anyDuplicated(q[, .(GEOID, year, quarter)]) || anyDuplicated(a[, .(GEOID, year)]))
    stop('Invalid or duplicate panel keys.')
  good <- apply(as.matrix(m[, ..gi_count_columns]), 1, function(z)
    all(is.finite(z) & z >= 0 & z == floor(z)))
  if (any(m[good, n_amws != n_amws_male_80 + n_amws_female_80 + n_amws_unclassified_80]))
    stop('Gender counts do not partition total counts.')
  list(monthly = m, quarterly = q, annual = a)
}

gi_balance <- function(monthly, years, universe = unique(monthly$GEOID)) {
  d <- copy(monthly[year %in% years])
  expected <- length(years) * 12L
  d[, invalid_count := !Reduce(`&`, lapply(.SD, function(z)
    is.finite(z) & z >= 0 & z == floor(z))), .SDcols = gi_count_columns]
  audit <- d[, .(observed_cells = .N, unique_cells = uniqueN(12L * year + month),
    invalid_birth_cells = sum(!is.finite(monthly_allocated_births) | monthly_allocated_births <= 0),
    invalid_count_cells = sum(invalid_count)), by = GEOID]
  audit <- merge(data.table(GEOID = sort(universe)), audit, by = 'GEOID', all.x = TRUE)
  for (col in c('observed_cells', 'unique_cells', 'invalid_birth_cells', 'invalid_count_cells'))
    set(audit, which(is.na(audit[[col]])), col, 0L)
  audit[, expected_cells := expected]
  audit[, retained := observed_cells == expected & unique_cells == expected &
          invalid_birth_cells == 0L & invalid_count_cells == 0L]
  audit[, reason := fifelse(observed_cells != expected | unique_cells != expected, 'incomplete_grid',
    fifelse(invalid_birth_cells > 0L, 'invalid_birth_denominator',
      fifelse(invalid_count_cells > 0L, 'invalid_count', 'retained')))]
  d[, invalid_count := NULL]
  list(data = d[GEOID %in% audit[retained == TRUE, GEOID]], audit = audit)
}

gi_period_data <- function(monthly, resolution) {
  d <- copy(monthly)
  if (resolution == 'monthly') {
    d[, `:=`(season = month, births = monthly_allocated_births,
              x = 12L * (year - 1927L) + month - 12L)]
    d[, cohort_id := 12L * year + month]
    d[, cohort_date := as.Date(sprintf('%04d-%02d-01', year, month))]
  } else if (resolution == 'quarterly') {
    d[, quarter := as.integer(ceiling(month / 3))]
    sumcols <- c('monthly_allocated_births', 'n_amws', 'n_amws_1906_1955_dedup',
                 'n_amws_1986', gi_count_columns)
    d <- d[, lapply(.SD, sum), by = .(GEOID, year, quarter), .SDcols = sumcols]
    d[, `:=`(season = quarter, births = monthly_allocated_births,
              x = 4L * (year - 1927L) + quarter - 4L)]
    d[, cohort_id := 4L * year + quarter]
    d[, cohort_date := as.Date(sprintf('%04d-%02d-01', year, 3L * quarter - 2L))]
  } else stop('Unknown resolution.')
  d[, earlier := as.integer(x <= 0)]
  d[, `:=`(trend_earlier = x * earlier, trend_later = x * (1L - earlier))]
  setorder(d, GEOID, cohort_id)
  d
}

gi_reconcile_quarters <- function(monthly, quarterly) {
  expected <- gi_period_data(monthly, 'quarterly')
  cols <- c('n_amws', 'n_amws_1906_1955_dedup', 'n_amws_1986', gi_count_columns)
  actual <- quarterly[year %in% monthly$year & GEOID %in% monthly$GEOID]
  setorder(actual, GEOID, year, quarter)
  if (nrow(expected) != nrow(actual) ||
      !identical(expected[, .(GEOID, year, quarter)], actual[, .(GEOID, year, quarter)]) ||
      any(as.matrix(expected[, ..cols]) != as.matrix(actual[, ..cols])))
    stop('Existing quarterly counts/keys differ from retained monthly sums.')
  difference <- abs(expected$births - actual$quarterly_allocated_births)
  if (any(!is.finite(difference)) || any(difference > 1e-8 * pmax(1, expected$births)))
    stop('Quarterly denominators differ from retained monthly sums.')
  data.table(check = 'quarterly_reconciliation', observations = nrow(expected),
             max_denominator_difference = max(difference), count_mismatches = 0L)
}

gi_fit <- function(period, group, weighting = 'births', seasonal = TRUE) {
  d <- copy(period)
  d[, y := 1000 * get(paste0('n_amws_', group, '_80')) / births]
  d[, weight := if (weighting == 'births') births else 1]
  formula <- as.formula(paste('y ~ earlier + trend_earlier + trend_later | GEOID',
                             if (seasonal) '+ season' else ''))
  warnings <- character()
  fit <- withCallingHandlers(fixest::feols(formula, data = d, weights = ~weight,
    vcov = ~GEOID + cohort_id, ssc = gi_ssc(), fixef.rm = 'none', notes = FALSE),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning') })
  if (nobs(fit) != nrow(d) || !'earlier' %in% names(coef(fit)) || length(fit$collin.var))
    stop('Estimation dropped observations or a required regressor.')
  ct <- coeftable(fit)
  df <- degrees_freedom(fit, 't')
  nc <- uniqueN(d$GEOID); nt <- uniqueN(d$cohort_id)
  if (df != min(nc, nt) - 1L) stop('Unexpected two-way cluster t degrees of freedom.')
  se <- ct['earlier', 2]; beta <- ct['earlier', 1]
  if (!is.finite(se) || se <= 0) stop('Cutoff standard error is unavailable/nonpositive.')
  ci <- beta + c(-1, 1) * qt(.975, df) * se
  row <- data.table(outcome = group, weighting = weighting, seasonal = seasonal,
    estimate = beta, std_error = se, ci_lower = ci[1], ci_upper = ci[2],
    t_statistic = ct['earlier', 3], p_value = ct['earlier', 4], df = df,
    observations = nrow(d), counties = nc, cohort_clusters = nt,
    estimated_births = sum(d$births), assigned_scientists = sum(d[[paste0('n_amws_', group, '_80')]]),
    zero_cell_share = mean(d[[paste0('n_amws_', group, '_80')]] == 0),
    slope_earlier = coef(fit)[['trend_earlier']], slope_later = coef(fit)[['trend_later']],
    warnings = paste(unique(warnings), collapse = ' | '))
  # Remove fitted nuisance effects, then restore one fixed weighted mean. This
  # yields cohort points and lines on the same standardized rate scale.
  trend <- beta * d$earlier + coef(fit)[['trend_earlier']] * d$trend_earlier +
    coef(fit)[['trend_later']] * d$trend_later
  nuisance <- fitted(fit) - trend
  constant <- weighted.mean(nuisance, d$weight)
  d[, `:=`(adjusted = y - nuisance + constant,
            adjusted_fit = trend + constant, residual = residuals(fit))]
  cohorts <- d[, .(raw_rate = weighted.mean(y, weight),
    adjusted_rate = weighted.mean(adjusted, weight), fitted_rate = weighted.mean(adjusted_fit, weight),
    residual_mean = weighted.mean(residual, weight), estimated_births = sum(births),
    assigned_scientists = sum(get(paste0('n_amws_', group, '_80'))), zero_cell_share = mean(y == 0)),
    by = .(year, season, cohort_id, cohort_date, x, earlier)]
  setorder(cohorts, cohort_id)
  row[, cohort_residual_lag1 := cor(head(cohorts$residual_mean, -1), tail(cohorts$residual_mean, -1))]
  lines <- rbindlist(lapply(c(0L, 1L), function(side) {
    xr <- if (side == 1L) seq(min(d$x), 0, length.out = 120) else seq(0, max(d$x), length.out = 120)
    data.table(x = xr, earlier = side, fitted_rate = constant + beta * side +
      xr * if (side == 1L) coef(fit)[['trend_earlier']] else coef(fit)[['trend_later']],
      extrapolated = if (side == 1L) FALSE else xr < min(d[earlier == 0L, x]))
  }))
  list(fit = fit, estimates = row, cohorts = cohorts, lines = lines)
}

gi_plot_cutoff <- function(points, lines, raw = FALSE, title = '') {
  d <- copy(points)
  d[, plot_rate := if (raw) raw_rate else adjusted_rate]
  d[, outcome := factor(outcome, levels = gi_groups,
                        labels = c('Male (primary)', 'Female (placebo)', 'Unclassified (placebo)'))]
  l <- copy(lines)
  l[, outcome := factor(outcome, levels = gi_groups,
                        labels = c('Male (primary)', 'Female (placebo)', 'Unclassified (placebo)'))]
  p <- ggplot(d, aes(x, plot_rate)) +
    geom_vline(xintercept = .5, linetype = 'dashed', color = '#65717B') +
    geom_point(size = 1.5, color = '#23658A') +
    facet_wrap(~outcome, ncol = 1, scales = 'free_y') +
    labs(title = title, x = 'Birth-cohort bins from last earlier-born bin (x = 0)',
         y = 'Scientists per 1,000 total estimated births',
         subtitle = if (raw) 'Raw birth-weighted cohort rates; descriptive points' else
           'County- and season-adjusted cohort means; separate fitted trends',
         caption = 'Dashed boundary: between x = 0 and x = 1. Reported coefficient is earlier minus later at x = 0.') +
    theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(),
      plot.title = element_text(face = 'bold'), strip.text = element_text(face = 'bold'),
      plot.caption = element_text(size = 8, hjust = 0))
  if (!raw) p <- p + geom_line(data = l, aes(x, fitted_rate, group = interaction(earlier, extrapolated),
                                            linetype = extrapolated),
                              inherit.aes = FALSE, color = '#B55B21', linewidth = .65) +
    scale_linetype_manual(values = c('FALSE' = 'solid', 'TRUE' = 'dotted'), guide = 'none')
  p
}

gi_save_plot <- function(plot, stem, height = 7) {
  ggsave(paste0(stem, '.png'), plot, width = 8, height = height, dpi = 180, bg = 'white')
  ggsave(paste0(stem, '.pdf'), plot, width = 8, height = height, device = grDevices::cairo_pdf)
}

gi_measurement_audits <- function(annual, monthly, ids) {
  a <- copy(annual[GEOID %in% ids]); m <- monthly[GEOID %in% ids]
  annual_summary <- a[, .(counties = .N, estimated_births = sum(county_births_estimate_year),
    national_births = unique(us_births_year), national_population = unique(us_pop_year),
    assigned = sum(n_early_assigned + n_1986_assigned),
    unassigned = sum(n_unassigned_month), early_assigned = sum(n_early_assigned),
    late_assigned = sum(n_1986_assigned),
    male_assigned = sum(n_early_assigned_male_80 + n_1986_assigned_male_80),
    female_assigned = sum(n_early_assigned_female_80 + n_1986_assigned_female_80),
    unclassified_assigned = sum(n_early_assigned_unclassified_80 + n_1986_assigned_unclassified_80),
    known_geography_change_counties = sum(known_geography_change),
    late_nominal_coverage_counties = sum(late_nominal_coverage)), by = year]
  setorder(annual_summary, year)
  annual_summary[, `:=`(births_pct_change = 100 * (estimated_births / shift(estimated_births) - 1),
    completeness = assigned / (assigned + unassigned),
    classified_share_assigned = (male_assigned + female_assigned) / assigned,
    early_share_assigned = early_assigned / assigned)]
  gender <- rbindlist(lapply(gi_groups, function(g) {
    s <- paste0('_', g, '_80')
    z <- a[, .(assigned = sum(get(paste0('n_early_assigned', s)) + get(paste0('n_1986_assigned', s))),
      unassigned = sum(get(paste0('n_unassigned_month', s)))), by = year]
    z[, `:=`(outcome = g, completeness = assigned / (assigned + unassigned))]; z
  }))
  cohort <- m[, .(births = sum(monthly_allocated_births),
    male = 1000 * sum(n_amws_male_80) / sum(monthly_allocated_births),
    female = 1000 * sum(n_amws_female_80) / sum(monthly_allocated_births),
    unclassified = 1000 * sum(n_amws_unclassified_80) / sum(monthly_allocated_births)), by = .(year, month)]
  setorder(cohort, year, month)
  decjan <- cohort[month == 1L & year > min(year)]
  dec <- cohort[month == 12L, .(year = year + 1L, previous_dec_births = births,
    previous_dec_male = male, previous_dec_female = female, previous_dec_unclassified = unclassified)]
  decjan <- merge(decjan, dec, by = 'year')
  decjan[, `:=`(births_pct_change = 100 * (births / previous_dec_births - 1),
    male_rate_change = male - previous_dec_male, female_rate_change = female - previous_dec_female,
    unclassified_rate_change = unclassified - previous_dec_unclassified)]
  geography <- unique(a[, .(GEOID, first_nominal_year, known_geography_change,
                            late_nominal_coverage, geography_caution, geography_note)])
  list(annual = annual_summary, gender = gender, december_january = decjan, geography = geography)
}
