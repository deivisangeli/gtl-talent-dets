# Run from any directory; see wwii_gi_bill_amws_cutoff.md.
arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
root <- if (length(arg)) normalizePath(file.path(dirname(sub('^--file=', '', arg[1])), '../..'),
  winslash = '/') else Sys.getenv('GTL_REPO', getwd())
source(file.path(root, 'analysis/mandatory_schooling/wwii_gi_bill_amws_helpers.R'))
render_only <- identical(commandArgs(TRUE), '--render-only')
if (length(commandArgs(TRUE)) && !render_only) stop('Usage: estimate_wwii_gi_bill_amws_cutoff.R [--render-only]')
paths <- gi_paths(root)
out <- paths$results
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out, 'figures'), showWarnings = FALSE)
dir.create(file.path(out, 'models'), showWarnings = FALSE)
before <- tools::md5sum(paths$files)
if (render_only) {
  manifest_file <- file.path(out, 'input_manifest.csv')
  if (!file.exists(manifest_file)) stop('Rendering requires the original estimation input manifest.')
  original <- fread(manifest_file)
  if (!identical(unname(before), original$md5[match(names(paths$files), original$input)]))
    stop('Inputs changed since estimation; rerun the models before rendering.')
} else {
  fwrite(data.table(input = names(paths$files), path = unname(paths$files),
    md5 = unname(before), bytes = file.info(paths$files)$size), file.path(out, 'input_manifest.csv'))
}
inputs <- gi_read_inputs(paths$files)
universe <- sort(unique(inputs$annual$GEOID))
samples <- lapply(gi_windows, function(w) gi_balance(inputs$monthly, w$years, universe))
common <- Reduce(intersect, lapply(samples, function(s) s$audit[retained == TRUE, GEOID]))
if (!length(common)) stop('No counties are usable in all core windows.')
fwrite(data.table(GEOID = sort(common)), file.path(out, 'common_counties.csv'))
exclusions <- rbindlist(lapply(names(samples), function(w) {
  d <- copy(samples[[w]]$audit); d[, `:=`(window = w, retained_common = retained & GEOID %in% common)]; d
}))
fwrite(exclusions, file.path(out, 'county_sample_audit.csv'))

specifications <- CJ(window = names(gi_windows), support = c('window_specific', 'common'),
                     resolution = c('monthly', 'quarterly'), outcome = gi_groups)
specifications[, `:=`(weighting = 'births', seasonal = TRUE)]
extra <- CJ(window = 'baseline', support = 'window_specific',
            resolution = c('monthly', 'quarterly'), outcome = gi_groups,
            variant = c('equal_weights', 'no_seasonality'))
extra[, `:=`(weighting = fifelse(variant == 'equal_weights', 'equal', 'births'),
              seasonal = variant != 'no_seasonality')]
extra[, variant := NULL]
specifications <- rbindlist(list(specifications, extra), use.names = TRUE)
specifications[, model_id := paste(window, support, resolution, outcome, weighting,
  fifelse(seasonal, 'seasonal', 'no_seasonal'), sep = '__')]
fwrite(specifications, file.path(out, 'specifications.csv'))
reconciliation <- rbindlist(lapply(names(samples), function(w) {
  d <- gi_reconcile_quarters(samples[[w]]$data, inputs$quarterly); d[, window := w]; d
}))
fwrite(reconciliation, file.path(out, 'quarterly_reconciliation.csv'))

rows <- points <- curves <- vector('list', nrow(specifications))
if (!render_only) for (i in seq_len(nrow(specifications))) {
  s <- specifications[i]
  message(sprintf('Model %d/%d: %s', i, nrow(specifications), s$model_id))
  m <- samples[[s$window]]$data
  if (s$support == 'common') m <- m[GEOID %in% common]
  d <- gi_period_data(m, s$resolution)
  result <- gi_fit(d, s$outcome, s$weighting, s$seasonal)
  tags <- c('window', 'support', 'resolution', 'model_id')
  for (tag in tags) {
    set(result$estimates, j = tag, value = s[[tag]])
    set(result$cohorts, j = tag, value = s[[tag]])
    set(result$lines, j = tag, value = s[[tag]])
  }
  for (tag in c('outcome', 'weighting', 'seasonal')) {
    set(result$cohorts, j = tag, value = s[[tag]])
    set(result$lines, j = tag, value = s[[tag]])
  }
  rows[[i]] <- result$estimates; points[[i]] <- result$cohorts; curves[[i]] <- result$lines
  saveRDS(result$fit, file.path(out, 'models', paste0(s$model_id, '.rds')), compress = TRUE)
}
estimates <- if (render_only) fread(file.path(out, 'estimates.csv')) else rbindlist(rows)
cohorts <- if (render_only) fread(file.path(out, 'cohort_summaries.csv')) else rbindlist(points)
lines <- if (render_only) fread(file.path(out, 'fitted_lines.csv')) else rbindlist(curves)
if (nrow(estimates) != 60 || !setequal(estimates$model_id, specifications$model_id))
  stop('Rendering requires the complete fixed core model set.')
fwrite(estimates, file.path(out, 'estimates.csv'))
fwrite(cohorts, file.path(out, 'cohort_summaries.csv'))
fwrite(lines, file.path(out, 'fitted_lines.csv'))
fwrite(estimates[, .(model_id, warnings, cohort_residual_lag1)], file.path(out, 'estimation_diagnostics.csv'))
fwrite(estimates[, .(model_id, window, support, resolution, outcome, observations, counties,
  cohort_clusters, estimated_births, assigned_scientists, zero_cell_share)], file.path(out, 'sample_summary.csv'))

for (r in c('monthly', 'quarterly')) {
  for (w in names(gi_windows)) {
    p <- cohorts[resolution == r & window == w & support == 'window_specific' & weighting == 'births' & seasonal]
    l <- lines[resolution == r & window == w & support == 'window_specific' & weighting == 'births' & seasonal]
    gi_save_plot(gi_plot_cutoff(p, l, title = paste('WWII/GI Bill cutoff:', r, gi_windows[[w]]$label)),
      file.path(out, 'figures', paste0(w, '_', r, '_adjusted')), height = 8.5)
    if (w == 'baseline') {
      gi_save_plot(gi_plot_cutoff(p[outcome == 'male'], l[outcome == 'male'],
        title = paste('Male scientists:', r, '1925-1930')),
        file.path(out, 'figures', paste0('baseline_', r, '_male')), height = 4.2)
      gi_save_plot(gi_plot_cutoff(p[outcome != 'male'], l[outcome != 'male'],
        title = paste('Placebo outcomes:', r, '1925-1930')),
        file.path(out, 'figures', paste0('baseline_', r, '_placebos')), height = 6.5)
      gi_save_plot(gi_plot_cutoff(p, l, raw = TRUE,
        title = paste('Raw cohort rates:', r, '1925-1930')),
        file.path(out, 'figures', paste0('baseline_', r, '_raw')), height = 8.5)
    }
  }
}

audits <- gi_measurement_audits(inputs$annual, inputs$monthly, common)
for (name in names(audits)) fwrite(audits[[name]], file.path(out, paste0('audit_', name, '.csv')))
theme <- theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(),
  plot.title = element_text(face = 'bold'))
denominator_plot <- ggplot(audits$annual, aes(year, estimated_births / 1e6)) +
  geom_line(color = '#23658A') + geom_point(color = '#23658A') +
  geom_vline(xintercept = 1927.5, linetype = 'dashed') + theme +
  labs(title = 'Annual birth denominators on the common county sample',
       x = 'Birth year', y = 'Total estimated births (millions)',
       caption = 'Each annual county denominator is allocated equally across months; annual changes can shift January rates.')
gi_save_plot(denominator_plot, file.path(out, 'figures/diagnostic_denominators'), height = 4.5)
completeness_plot <- ggplot(audits$gender, aes(year, 100 * completeness, color = outcome)) +
  geom_line() + geom_point() + geom_vline(xintercept = 1927.5, linetype = 'dashed') + theme +
  labs(title = 'Birth-month assignment completeness by gender classification',
       x = 'Birth year', y = 'Assigned months (% of annual records)', color = 'Outcome')
gi_save_plot(completeness_plot, file.path(out, 'figures/diagnostic_completeness'), height = 4.5)
composition <- melt(audits$annual, id.vars = 'year', measure.vars =
  c('classified_share_assigned', 'early_share_assigned'), variable.name = 'series', value.name = 'share')
composition[, series := fifelse(series == 'classified_share_assigned',
  'Classified male or female', 'Early editions (1906-1955)')]
composition_plot <- ggplot(composition, aes(year, 100 * share, color = series)) +
  geom_line() + geom_point() + geom_vline(xintercept = 1927.5, linetype = 'dashed') + theme +
  labs(title = 'Gender classification and AMWS source composition',
       x = 'Birth year', y = 'Share of assigned-month records (%)', color = '')
gi_save_plot(composition_plot, file.path(out, 'figures/diagnostic_composition'), height = 4.5)

baseline <- estimates[window == 'baseline' & support == 'window_specific' & weighting == 'births' & seasonal]
writeLines(c('# AMWS WWII/GI Bill cutoff reduced forms', '',
  'Outcome: gender-specific scientists per 1,000 total estimated births; saved 0.8 inferred-gender flags.',
  'Coefficient: earlier-born minus later-born fitted rate at x = 0, the last earlier-born bin.',
  'Baseline: 1925-1930; birth weights; county and season fixed effects; separate linear trends.',
  'Inference: county and national birth-cohort two-way clustering; t df = min(clusters)-1.', '',
  capture.output(print(baseline[, .(resolution, outcome, estimate, std_error, ci_lower, ci_upper, p_value)])), '',
  'Placebo outcomes are diagnostics; unclassified records may contain men.',
  'The denominator is total births, not male or female births. Counts include assigned birth months only.',
  'There are 72 monthly / 24 quarterly baseline national cohorts. Conventional clustering does not',
  'automatically address serially correlated national cohort shocks or discrete-trend approximation bias.',
  'Cohort residual lag-one correlations are descriptive and are saved in estimation_diagnostics.csv;',
  'they do not supply an alternative confidence interval or a formal serial-dependence test.',
  'Exclude-1928 estimates extrapolate across a gap. Annual denominators divided by 12 can create',
  'December/January rate changes. AMWS directory selection, date assignment, inferred gender,',
  'career censoring, geography and non-deduplication of 1986 against early editions limit interpretation.',
  'No service first stage is estimated. Exposure bundles WWII service and GI Bill/other veteran benefits;',
  'these reduced forms do not isolate an education subsidy or effects per veteran.'),
  file.path(out, 'results_note.md'))
after <- tools::md5sum(paths$files)
if (!identical(before, after)) stop('Input panel hashes changed during estimation.')
manifest <- data.table(input = names(paths$files), path = unname(paths$files),
  md5 = unname(after), bytes = file.info(paths$files)$size)
fwrite(manifest, file.path(out, 'input_manifest.csv'))
writeLines(c(capture.output(sessionInfo()), paste('Analysis timestamp:', format(Sys.time(), tz = 'America/Sao_Paulo'))),
           file.path(out, 'session_info.txt'))
message('Completed ', nrow(estimates), ' models. Results: ', out)
print(baseline[, .(resolution, outcome, estimate, std_error, p_value, counties, cohort_clusters)])
