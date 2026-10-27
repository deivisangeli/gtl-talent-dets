# Export saved estimates only. Self-contained tables and inline PGFPlots allow
# the Codex single-file compiler to build the report without auxiliary files.
arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
root <- if (length(arg)) normalizePath(file.path(dirname(sub('^--file=', '', arg[1])), '../..'),
  winslash = '/') else Sys.getenv('GTL_REPO', getwd())
source(file.path(root, 'analysis/mandatory_schooling/wwii_gi_bill_amws_helpers.R'))
if (length(commandArgs(TRUE))) stop('Usage: export_wwii_gi_bill_amws_cutoff_to_project.R')
paths <- gi_paths(root); out <- paths$results
project <- file.path(root, 'project')
prefix <- 'wwii_gi_bill_amws_reduced_form'
dir.create(file.path(project, 'tables'), showWarnings = FALSE)
dir.create(file.path(project, 'figures'), showWarnings = FALSE)
required <- c('estimates.csv', 'cohort_summaries.csv', 'fitted_lines.csv', 'county_sample_audit.csv',
              'audit_annual.csv', 'audit_gender.csv', 'audit_december_january.csv',
              'audit_geography.csv', 'input_manifest.csv', 'estimation_diagnostics.csv')
if (any(!file.exists(file.path(out, required)))) stop('Run the cutoff estimator before exporting.')
e <- fread(file.path(out, 'estimates.csv'))
p <- fread(file.path(out, 'cohort_summaries.csv'))
l <- fread(file.path(out, 'fitted_lines.csv'))
sample <- fread(file.path(out, 'county_sample_audit.csv'), colClasses = c(GEOID = 'character'))
a <- fread(file.path(out, 'audit_annual.csv'))
gender <- fread(file.path(out, 'audit_gender.csv'))
dj <- fread(file.path(out, 'audit_december_january.csv'))
diag <- fread(file.path(out, 'estimation_diagnostics.csv'))
input_manifest <- fread(file.path(out, 'input_manifest.csv'))
if (nrow(e) != 60 || anyDuplicated(e$model_id) || any(!is.finite(e$estimate)) ||
    !identical(unname(tools::md5sum(input_manifest$path)), input_manifest$md5))
  stop('Incomplete estimates or changed inputs; rerun estimation.')
baseline <- e[window == 'baseline' & support == 'window_specific' & weighting == 'births' & seasonal]
if (nrow(baseline) != 6) stop('Expected six baseline rows.')

tex_escape <- function(x) {
  x <- as.character(x)
  for (z in c('&', '%', '$', '#', '_', '{', '}')) x <- gsub(z, paste0('\\', z), x, fixed = TRUE)
  x
}
num <- function(x, digits = 4) formatC(x, format = 'f', digits = digits)
integer_fmt <- function(x) format(round(x), big.mark = ',', scientific = FALSE, trim = TRUE)
p_fmt <- function(x) ifelse(x < .001, '$<0.001$', num(x, 3))
ci_fmt <- function(lo, hi) paste0('[', num(lo), ', ', num(hi), ']')
outcome_label <- c(male = 'Male', female = 'Female', unclassified = 'Unclassified')
resolution_label <- c(monthly = 'Monthly', quarterly = 'Quarterly')
exports <- list()
record_export <- function(destination, sources) {
  src <- normalizePath(file.path(out, sources), winslash = '/', mustWork = TRUE)
  exports[[length(exports) + 1L]] <<- data.table(destination = normalizePath(destination, winslash = '/'),
    source = src, source_md5 = unname(tools::md5sum(src)),
    source_modified = format(file.info(src)$mtime, tz = 'America/Sao_Paulo'),
    exported = format(Sys.time(), tz = 'America/Sao_Paulo'))
}
table_tex <- function(name, caption, header, rows, note, sources = 'estimates.csv', align = NULL) {
  if (is.null(align)) align <- paste0('l', paste(rep('r', length(header) - 1L), collapse = ''))
  text <- c('\\begin{table}[H]', '\\centering', '\\small', paste0('\\caption{', caption, '}'),
    paste0('\\label{tab:', prefix, '_', name, '}'), '\\setlength{\\tabcolsep}{4pt}',
    paste0('\\begin{tabular}{', align, '}'), '\\toprule',
    paste0(paste(header, collapse = ' & '), ' \\\\'), '\\midrule',
    vapply(rows, function(row) paste0(paste(row, collapse = ' & '), ' \\\\'), character(1)),
    '\\bottomrule', '\\end{tabular}', '\\par\\medskip',
    paste0('\\begin{minipage}{\\linewidth}\\footnotesize\\textit{Notes:} ', note, '\\end{minipage}'),
    '\\end{table}')
  dest <- file.path(project, 'tables', paste0(prefix, '_', name, '.tex'))
  writeLines(text, dest); record_export(dest, sources)
  text
}
coordinates <- function(x, y) paste(sprintf('(%.8f,%.10f)', x, y), collapse = ' ')
axis_tex <- function(series, title, xlabel, ylabel, height = 4.3, vertical = NULL, legend = FALSE) {
  values <- unlist(lapply(series, function(z) z$y))
  yr <- range(values, finite = TRUE); pad <- max(diff(yr) * .1, .001)
  yr <- yr + c(-pad, pad)
  xr <- range(unlist(lapply(series, function(z) z$x)), finite = TRUE)
  x_options <- if (xlabel == 'Birth year') paste0('xtick={',
    paste(seq(ceiling(xr[1]), floor(xr[2])), collapse = ','), '},xticklabels={',
    paste(seq(ceiling(xr[1]), floor(xr[2])), collapse = ','), '},') else ''
  code <- c('\\begin{tikzpicture}', paste0('\\begin{axis}[width=0.97\\linewidth,height=', height,
    'cm,scale only axis=false,grid=major,grid style={gray!15},',
    'tick label style={font=\\scriptsize},label style={font=\\scriptsize},title style={font=\\small},',
    'scaled y ticks=false,/pgf/number format/fixed,/pgf/number format/precision=4,', x_options,
    'ylabel={', ylabel, '},xlabel={', xlabel, '},title={', title, '},',
    'xmin=', num(xr[1], 8), ',xmax=', num(xr[2], 8), ',ymin=', num(yr[1], 10), ',ymax=', num(yr[2], 10),
    ',legend style={font=\\scriptsize,at={(0.5,-0.32)},anchor=north,legend columns=2},unbounded coords=jump]'))
  for (s in series) {
    code <- c(code, paste0('\\addplot[', s$style, '] coordinates {', coordinates(s$x, s$y), '};'))
    if (legend && !is.null(s$label)) code <- c(code, paste0('\\addlegendentry{', s$label, '}'))
  }
  if (!is.null(vertical)) code <- c(code, paste0('\\draw[dashed,gray] (axis cs:', vertical, ',',
    num(yr[1], 10), ') -- (axis cs:', vertical, ',', num(yr[2], 10), ');'))
  c(code, '\\end{axis}', '\\end{tikzpicture}')
}
figure_tex <- function(name, caption, panels, note, sources) {
  text <- c('\\begin{figure}[H]', '\\centering',
    unlist(lapply(panels, function(z) c(z, '\\par\\smallskip'))),
    paste0('\\caption{', caption, '}'), paste0('\\label{fig:', prefix, '_', name, '}'),
    paste0('\\begin{minipage}{\\linewidth}\\footnotesize\\textit{Notes:} ', note, '\\end{minipage}'),
    '\\end{figure}')
  dest <- file.path(project, 'figures', paste0(prefix, '_', name, '.tex'))
  writeLines(text, dest); record_export(dest, sources)
  text
}
cutoff_figure <- function(window, resolution, outcomes, raw = FALSE, name) {
  plot_window <- window; plot_resolution <- resolution
  panels <- lapply(outcomes, function(g) {
    id <- e[window == plot_window & resolution == plot_resolution & outcome == g &
             support == 'window_specific' & weighting == 'births' & seasonal, model_id]
    if (length(id) != 1) stop('Ambiguous plot model selection.')
    z <- p[model_id == id]; fitline <- l[model_id == id]
    series <- list(list(x = z$x, y = if (raw) z$raw_rate else z$adjusted_rate,
      style = 'only marks,mark=*,mark size=1.15pt,color=blue!65!black'))
    if (!raw) for (side in c(1L, 0L)) {
      ll <- fitline[earlier == side]
      series <- c(series, list(list(x = ll$x, y = ll$fitted_rate,
        style = if (side == 1L) 'no marks,color=orange!80!black,thick' else
          'no marks,color=orange!80!black,thick,densely dotted')))
    }
    axis_tex(series, paste0(outcome_label[g], ' -- ', resolution_label[resolution]),
      'Birth-cohort bins from last earlier-born bin', 'Scientists / 1,000 total births',
      height = if (length(outcomes) == 1) 6 else 4.3, vertical = .5)
  })
  method <- if (raw) 'Points are raw birth-weighted cohort rates. No descriptive line is presented as a regression estimate.' else
    'Points remove fitted county and seasonal effects and restore their sample birth-weighted mean. Lines show the corresponding separate fitted trends; the dotted later-born line includes extrapolation to $x=0$.'
  figure_tex(name, paste0(if (raw) 'Raw cohort rates: ' else 'Adjusted cutoff comparison: ',
    resolution_label[resolution], ', ', gi_windows[[window]]$label), panels,
    paste(method, 'The dashed boundary is between $x=0$ and $x=1$; the reported coefficient compares the fitted earlier and later rates at $x=0$. Vertical scales differ by outcome.'),
    c('cohort_summaries.csv', 'fitted_lines.csv', 'estimates.csv'))
}

inference_note <- paste('All outcomes use total estimated births as the denominator and saved 0.8 gender flags.',
  'Coefficients are earlier-born minus later-born fitted rates at $x=0$. Baseline models use birth weights,',
  'county and seasonal fixed effects, and separate linear trends. Standard errors cluster separately by county',
  'and national birth cohort; intervals and $p$-values use a $t$ distribution with the smaller cluster count minus one degrees of freedom.')

main <- baseline[outcome == 'male'][order(match(resolution, c('monthly', 'quarterly')))]
main_rows <- lapply(c('Estimate', 'Standard error', '95% interval', '$p$-value', 'Observations',
                    'Counties', 'Birth-cohort clusters', 'Estimated births covered'), function(label) {
  values <- switch(label, 'Estimate' = num(main$estimate), 'Standard error' = num(main$std_error),
    '95% interval' = ci_fmt(main$ci_lower, main$ci_upper), '$p$-value' = p_fmt(main$p_value),
    'Observations' = integer_fmt(main$observations), 'Counties' = integer_fmt(main$counties),
    'Birth-cohort clusters' = integer_fmt(main$cohort_clusters),
    'Estimated births covered' = integer_fmt(main$estimated_births))
  c(if (label == '95% interval') tex_escape(label) else label, values)
})
main_table <- table_tex('main', 'Male baseline reduced-form estimates, 1925--1930',
  c('Statistic', 'Monthly', 'Quarterly'), main_rows, inference_note)
placebo <- baseline[outcome != 'male'][order(match(outcome, gi_groups), match(resolution, c('monthly', 'quarterly')))]
placebo_rows <- lapply(seq_len(nrow(placebo)), function(i) {
  z <- placebo[i]
  c(outcome_label[z$outcome], resolution_label[z$resolution], num(z$estimate), num(z$std_error),
    ci_fmt(z$ci_lower, z$ci_upper), p_fmt(z$p_value))
})
placebo_table <- table_tex('placebos', 'Female and unclassified baseline placebo estimates',
  c('Outcome', 'Bins', '$\\widehat\\beta$', 'SE', tex_escape('95% interval'), '$p$'), placebo_rows,
  paste(inference_note, sprintf('Each monthly model has %s observations and each quarterly model has %s, in %s counties. Unclassified records may include men.',
    integer_fmt(main$observations[1]), integer_fmt(main$observations[2]), integer_fmt(main$counties[1]))),
  align = 'llrrrr')

main_text <- sprintf(paste0('The male monthly estimate is %s scientists per 1,000 total estimated births ',
  '(95\\%% interval %s; $p=%s$). The quarterly estimate is %s (95\\%% interval %s; $p=%s$). ',
  'Both cover %s counties and approximately %s estimated births. ',
  'The monthly and quarterly coefficients evaluate resolution-specific last-earlier-bin intercepts; ',
  'aggregation and centering can therefore change the fitted contrast.'),
  num(main$estimate[1]), ci_fmt(main$ci_lower[1], main$ci_upper[1]), num(main$p_value[1], 3),
  num(main$estimate[2]), ci_fmt(main$ci_lower[2], main$ci_upper[2]), num(main$p_value[2], 3),
  integer_fmt(main$counties[1]), integer_fmt(main$estimated_births[1]))
main_text <- paste(main_text, if (all(main$ci_lower <= 0 & main$ci_upper >= 0))
  'Both baseline 95\\% intervals include zero.' else
  'The intervals above summarize uncertainty under the specified clustering assumptions.')
placebo_text <- paste(vapply(c('female', 'unclassified'), function(g) {
  z <- placebo[outcome == g]
  sprintf('%s coefficients are %s monthly and %s quarterly, in the same units. These are diagnostics rather than evidence that these groups were wholly unexposed.',
    outcome_label[g], num(z[resolution == 'monthly', estimate]), num(z[resolution == 'quarterly', estimate]))
}, character(1)), collapse = ' ')
u <- placebo[outcome == 'unclassified' & resolution == 'monthly']
placebo_text <- paste(placebo_text, if (u$ci_upper < 0 || u$ci_lower > 0)
  'The unclassified monthly 95\\% interval excludes zero, underscoring the need to examine classification coverage.' else
  'The unclassified monthly 95\\% interval includes zero.')

doc <- c('% Generated from saved cutoff analysis outputs; regenerate with the report exporter.',
  '% Self-contained: no external figures, tables, or bibliography are required for compilation.',
  '\\documentclass[11pt]{article}', '\\usepackage[a4paper,margin=1in]{geometry}',
  '\\usepackage{amsmath,booktabs,float,microtype}', '\\usepackage{pgfplots}',
  '\\pgfplotsset{compat=1.18}', '\\usepackage[colorlinks=true,linkcolor=black,urlcolor=blue]{hyperref}',
  '\\setlength{\\parindent}{0pt}', '\\setlength{\\parskip}{0.5em}',
  '\\begin{document}', '\\begin{center}',
  '{\\LARGE\\bfseries World War II, the GI Bill, and AMWS Scientists}\\\\[0.4em]',
  '{\\large Monthly and quarterly birth-cohort reduced forms}\\\\[0.3em]',
  paste0('{\\small Exported ', format(Sys.Date(), '%Y-%m-%d'), '}'), '\\end{center}',
  '\\section{Main results}',
  'This analysis adapts the birth-cohort boundary described in the project strategy note: December 1927 versus January 1928, or 1927Q4 versus 1928Q1. It follows the cohort comparison in \\href{https://www.nber.org/papers/w33164}{Abramitzky, Kowalski, Perez, and Price, NBER Working Paper 33164}, with county outcomes and the adaptations below.',
  'The extended AMWS panel supplies assigned scientist birth months, inferred gender, and county birth estimates. Male counts at the saved 0.8 classification threshold are primary; all three outcomes divide by total estimated births. Annual county births equal county population times the national Gapminder birth rate and are allocated equally across months. Neither male-specific fertility nor monthly birth seasonality is observed. Quarterly counts and denominators are sums of the retained monthly cells.',
  'For resolution $r$, the baseline weighted least-squares model is',
  '\\begin{equation}',
  'Y_{cr}=\\alpha_c+\\beta_r D_r+\\gamma_r x_rD_r+\\delta_r x_r(1-D_r)+\\lambda_{s(r)}+\\varepsilon_{cr},',
  '\\end{equation}',
  'where $Y_{cr}$ is scientists per 1,000 total estimated births; $\\alpha_c$ is a county effect and $\\lambda_{s(r)}$ a month- or quarter-of-year effect. For months, $x_m=12(y-1927)+(m-12)$; for quarters, $x_q=4(y-1927)+(q-4)$. In both cases $D_r=\\mathbf{1}\\{x_r\\leq0\\}$ and the adjacent bins are zero and one. The coefficient compares the two fitted rates at $x_r=0$, rather than their raw adjacent-bin means.',
  'The baseline retains counties with all 72 monthly cells in January 1925--December 1930, finite positive denominators, and valid nonnegative counts for every outcome. Zero counts remain in the sample. Birth denominators are regression weights. Standard errors cluster separately by county and national birth cohort; the baseline has only 72 monthly or 24 quarterly cohort clusters.',
  main_text, main_table, '\\clearpage',
  cutoff_figure('baseline', 'monthly', 'male', name = 'baseline_monthly_male'),
  cutoff_figure('baseline', 'quarterly', 'male', name = 'baseline_quarterly_male'),
  '\\clearpage', '\\section{Placebos}',
  'Female and unclassified scientist counts use the identical county sample, total-birth denominators, weights, and specifications. Unclassified records can include men, so their estimates also diagnose changes in classification coverage. A nonzero placebo can reflect measurement or broader cohort changes; a small placebo does not establish causal identification.',
  placebo_text, placebo_table, '\\clearpage',
  cutoff_figure('baseline', 'monthly', c('female', 'unclassified'), name = 'baseline_monthly_placebos'),
  '\\clearpage', cutoff_figure('baseline', 'quarterly', c('female', 'unclassified'), name = 'baseline_quarterly_placebos'),
  '\\clearpage', '\\appendix', '\\section{Robustness estimates}',
  'The core checks expand the window to 1923--1930, narrow it to 1926--1929, and omit all 1928 births from 1925--1930. Every window rebuilds the balanced county sample and is repeated on the intersection of usable counties. Equal observation weights and omission of seasonal effects are separate baseline checks. Equal weights give equal county influence within each balanced panel. Excluding 1928 creates a gap and increases reliance on extrapolation.')
for (r in c('monthly', 'quarterly')) for (g in gi_groups) {
  z <- e[resolution == r & outcome == g]
  setorder(z, window, support, weighting, seasonal)
  robust_rows <- lapply(seq_len(nrow(z)), function(i) {
    row <- z[i]
    variant <- if (row$weighting == 'equal') 'Equal weights' else if (!row$seasonal) 'No seasonal FE' else 'Birth weights + seasonal FE'
    c(gi_windows[[row$window]]$label, if (row$support == 'common') 'Common' else 'Window',
      variant, num(row$estimate), ci_fmt(row$ci_lower, row$ci_upper), p_fmt(row$p_value))
  })
  doc <- c(doc, table_tex(paste0('robustness_', r, '_', g),
    paste(outcome_label[g], '--', resolution_label[r], 'robustness estimates'),
    c('Window', 'Sample', 'Variant', '$\\widehat\\beta$', tex_escape('95% interval'), '$p$'), robust_rows,
    paste(inference_note, 'Window and common-county rows are intentionally both retained, even when the samples coincide.'),
    align = 'lllrrr'), '\\clearpage')
}
doc <- c(doc, '\\subsection{Alternative-window cutoff plots}')
for (w in setdiff(names(gi_windows), 'baseline')) for (r in c('monthly', 'quarterly')) {
  doc <- c(doc, cutoff_figure(w, r, gi_groups, name = paste0(w, '_', r, '_adjusted')), '\\clearpage')
}
doc <- c(doc, '\\section{Raw cohort rates and sample retention}')
for (r in c('monthly', 'quarterly')) doc <- c(doc,
  cutoff_figure('baseline', r, gi_groups, raw = TRUE, name = paste0('baseline_', r, '_raw')), '\\clearpage')
sample_rows <- lapply(names(gi_windows), function(w) {
  z <- sample[window == w]; row <- e[window == w & resolution == 'monthly' &
    outcome == 'male' & support == 'window_specific' & weighting == 'births' & seasonal]
  c(gi_windows[[w]]$label, integer_fmt(nrow(z)), integer_fmt(sum(z$retained)),
    integer_fmt(sum(z$retained_common)), integer_fmt(sum(!z$retained)), integer_fmt(row$estimated_births))
})
doc <- c(doc, table_tex('sample_retention', 'Balanced sample retention by analysis window',
  c('Window', 'Candidate', 'Retained', 'Common', 'Excluded', 'Est. births'), sample_rows,
  'Eligibility is assessed in required monthly cells before quarterly aggregation. Every outcome uses the same eligible counties. Exclusion flags and county identifiers are saved in the county sample audit.',
  c('county_sample_audit.csv', 'estimates.csv')))
zero_rows <- lapply(seq_len(nrow(baseline)), function(i) {
  z <- baseline[i]
  c(outcome_label[z$outcome], resolution_label[z$resolution], integer_fmt(z$assigned_scientists),
    paste0(num(100 * z$zero_cell_share, 2), '\\%'), num(z$cohort_residual_lag1, 3))
})
doc <- c(doc, table_tex('sparsity_inference', 'Baseline sparsity and national-cohort residual diagnostics',
  c('Outcome', 'Bins', 'Assigned scientists', 'Zero cells', 'Residual lag 1'), zero_rows,
  'Lag 1 is the correlation of successive national-cohort weighted residual means. It is descriptive, not a formal test or a correction for serial dependence. County and cohort clustering does not automatically address serially correlated national shocks or approximation bias from discrete cohorts.',
  'estimates.csv', align = 'llrrr'), '\\clearpage', '\\section{Measurement diagnostics}')
annual_rows <- lapply(seq_len(nrow(a)), function(i) {
  z <- a[i]
  c(as.character(z$year), integer_fmt(z$estimated_births), integer_fmt(z$assigned),
    integer_fmt(z$unassigned), num(100 * z$completeness, 2),
    num(100 * z$classified_share_assigned, 2), num(100 * z$early_share_assigned, 2))
})
doc <- c(doc, table_tex('annual_measurement', 'Annual denominators, assignment and composition: common counties',
  c('Year', 'Est. births', 'Assigned', 'Unassigned', tex_escape(c('Assigned %', 'Classified %', 'Early %'))),
  annual_rows, 'Annual audit fields are aggregated once per county-year. Classified means male or female; early means deduplicated 1906--1955 records. Unassigned-month records are not included in regression numerators.',
  'audit_annual.csv'))
dj_rows <- lapply(seq_len(nrow(dj)), function(i) {
  z <- dj[i]
  c(as.character(z$year), num(z$births_pct_change, 2), num(z$male_rate_change),
    num(z$female_rate_change), num(z$unclassified_rate_change))
})
doc <- c(doc, table_tex('december_january', 'Raw December-to-January changes on the common sample',
  c('January year', tex_escape('Births change %'), 'Male rate', 'Female rate', 'Unclassified rate'), dj_rows,
  'Changes are January minus the preceding December. Rate differences are scientists per 1,000 total estimated births. These raw descriptive comparisons are not fitted discontinuities or additional placebo regressions.',
  'audit_december_january.csv'))
for (kind in c('denominators', 'completeness', 'composition')) {
  series <- switch(kind,
    denominators = list(list(x = a$year, y = a$estimated_births / 1e6,
      style = 'mark=*,mark size=1.5pt,color=blue!65!black', label = 'Estimated births')),
    completeness = lapply(seq_along(gi_groups), function(i) {
      z <- gender[outcome == gi_groups[i]]; setorder(z, year)
      list(x = z$year, y = 100 * z$completeness,
        style = paste0('mark=*,mark size=1.5pt,color=', c('blue!65!black', 'orange!80!black', 'green!50!black')[i]),
        label = outcome_label[gi_groups[i]])
    }),
    composition = list(list(x = a$year, y = 100 * a$classified_share_assigned,
      style = 'mark=*,color=blue!65!black', label = 'Classified male or female'),
      list(x = a$year, y = 100 * a$early_share_assigned,
        style = 'mark=*,color=orange!80!black', label = 'Early editions')))
  title <- switch(kind, denominators = 'Annual birth denominators',
    completeness = 'Birth-month assignment completeness', composition = 'Classification and source composition')
  ylabel <- if (kind == 'denominators') 'Estimated births (millions)' else 'Share (percent)'
  panel <- axis_tex(series, title, 'Birth year', ylabel, height = 6, vertical = 1927.5, legend = kind != 'denominators')
  doc <- c(doc, '\\clearpage', figure_tex(paste0('diagnostic_', kind), title, list(panel),
    'All series use the same common county sample. The dashed line marks the 1927/1928 boundary. Denominators are annual estimates allocated equally across months; composition and completeness can change independently of service exposure.',
    if (kind == 'completeness') 'audit_gender.csv' else 'audit_annual.csv'))
}
geography <- fread(file.path(out, 'audit_geography.csv'))
doc <- c(doc, '\\clearpage', '\\subsection{Interpretation and data limitations}',
  sprintf('The common sample contains %s counties flagged for a known geography change and %s with late nominal population coverage. All counties retain the upstream caution that historical boundaries are not fully harmonized to modern scientist geocodes. County-level flags are retained in the exported audit.',
    integer_fmt(uniqueN(geography[known_geography_change == TRUE, GEOID])),
    integer_fmt(uniqueN(geography[late_nominal_coverage == TRUE, GEOID]))),
  'AMWS outcomes preserve directory selection and career censoring. The 1986 edition is not deduplicated against early editions. A zero cell means no assigned scientist record, not a demonstrated absence of scientists. Gender is inferred, and unclassified records must not be interpreted as a distinct untreated sex.',
  'A causal interpretation requires smooth counterfactual scientist rates and stable recording, date assignment, geography, and denominator construction around the boundary. Monthly dates do not establish a sharper legal discontinuity. The panel supplies no service status, benefit use, or service first stage, so estimates cannot be converted into effects per veteran. Any exposure effect bundles WWII service with GI Bill and other veteran benefits and does not isolate an education subsidy.',
  'There are only 72 monthly or 24 quarterly baseline national cohorts. Thousands of county cells do not create independent treatment cohorts. Conventional two-way clustered intervals remain sensitive to national serial dependence and trend approximation in a discrete running variable. The robustness checks and placebos support assessment of the design rather than resolving these assumptions.',
  '\\end{document}')

# Export reusable rendered assets in addition to the inline figures in the report.
assets <- list.files(file.path(out, 'figures'), pattern = '\\.(png|pdf)$', full.names = TRUE)
for (asset in assets) {
  dest <- file.path(project, 'figures', paste0(prefix, '_', basename(asset)))
  if (!file.copy(asset, dest, overwrite = TRUE)) stop('Failed to export figure: ', asset)
  record_export(dest, file.path('figures', basename(asset)))
}
document <- file.path(project, paste0(prefix, '.tex'))
writeLines(doc, document, useBytes = TRUE)
record_export(document, required)
manifest <- rbindlist(exports)
fwrite(manifest, file.path(project, 'tables', paste0(prefix, '_manifest.csv')))
message('Exported self-contained report: ', document)
message('Exported ', length(assets), ' rendered figure assets and ', uniqueN(manifest$destination), ' report/table/figure files.')
