# Check that the report contains the intended models and matches saved outputs.
arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
root <- if (length(arg)) normalizePath(file.path(dirname(sub('^--file=', '', arg[1])), '../..'),
  winslash = '/') else Sys.getenv('GTL_REPO', getwd())
source(file.path(root, 'analysis/mandatory_schooling/wwii_gi_bill_amws_helpers.R'))
out <- gi_paths(root)$results
project <- file.path(root, 'project')
prefix <- 'wwii_gi_bill_amws_reduced_form'
document <- file.path(project, paste0(prefix, '.tex'))
text <- readLines(document)
joined <- paste(text, collapse = '\n')
e <- fread(file.path(out, 'estimates.csv'))
p <- fread(file.path(out, 'cohort_summaries.csv'))
manifest <- fread(file.path(project, 'tables', paste0(prefix, '_manifest.csv')))
stopifnot(all(file.exists(manifest$destination)), all(file.exists(manifest$source)),
  identical(unname(tools::md5sum(manifest$source)), manifest$source_md5),
  uniqueN(manifest$destination) == 62,
  sum(grepl('\\begin{table}', text, fixed = TRUE)) == 12,
  sum(grepl('\\begin{figure}', text, fixed = TRUE)) == 15,
  sum(grepl('\\section{Main results}', text, fixed = TRUE)) == 1,
  sum(grepl('\\section{Placebos}', text, fixed = TRUE)) == 1,
  sum(grepl('\\appendix', text, fixed = TRUE)) == 1,
  !grepl('\\input{', joined, fixed = TRUE), !grepl('\\includegraphics', joined, fixed = TRUE))

# Baseline table entries must correspond to the primary male outcome, in the
# declared monthly/quarterly order, with the rounded saved confidence intervals.
b <- e[window == 'baseline' & support == 'window_specific' & weighting == 'births' & seasonal & outcome == 'male']
b <- b[order(match(resolution, c('monthly', 'quarterly')))]
fmt <- function(x) formatC(x, digits = 4, format = 'f')
main <- readLines(file.path(project, 'tables', paste0(prefix, '_main.tex')))
stopifnot(any(grepl(paste0('Estimate & ', paste(fmt(b$estimate), collapse = ' & ')), main, fixed = TRUE)),
  any(grepl(paste0('Standard error & ', paste(fmt(b$std_error), collapse = ' & ')), main, fixed = TRUE)))
for (r in c('monthly', 'quarterly')) for (g in gi_groups) {
  rows <- e[resolution == r & outcome == g]
  tab <- paste(readLines(file.path(project, 'tables', paste0(prefix, '_robustness_', r, '_', g, '.tex'))), collapse = '\n')
  stopifnot(sum(grepl(' & ', strsplit(tab, '\n', fixed = TRUE)[[1]], fixed = TRUE)) == 11)
  for (i in seq_len(nrow(rows))) {
    z <- rows[i]
    stopifnot(grepl(paste0(' & ', fmt(z$estimate), ' & [', fmt(z$ci_lower), ', ', fmt(z$ci_upper), ']'), tab, fixed = TRUE))
  }
}

# Every reported cutoff figure's first panel must use its own window/resolution
# rather than accidentally reusing a baseline or another outcome's coordinates.
for (w in names(gi_windows)) for (r in c('monthly', 'quarterly')) {
  id <- e[window == w & resolution == r & outcome == 'male' &
    support == 'window_specific' & weighting == 'births' & seasonal, model_id]
  z <- p[model_id == id][order(cohort_id)]
  name <- if (w == 'baseline') paste0('baseline_', r, '_male') else paste0(w, '_', r, '_adjusted')
  fig <- readLines(file.path(project, 'figures', paste0(prefix, '_', name, '.tex')))
  coordinate <- sprintf('(%.8f,%.10f)', z$x[1], z$adjusted_rate[1])
  first <- fig[grep('\\addplot[only marks', fig, fixed = TRUE)[1]]
  stopifnot(grepl(coordinate, first, fixed = TRUE))
}
for (r in c('monthly', 'quarterly')) for (g in c('female', 'unclassified')) {
  id <- e[window == 'baseline' & resolution == r & outcome == g &
    support == 'window_specific' & weighting == 'births' & seasonal, model_id]
  z <- p[model_id == id][order(cohort_id)]
  fig <- readLines(file.path(project, 'figures', paste0(prefix, '_baseline_', r, '_placebos.tex')))
  coordinate <- sprintf('(%.8f,%.10f)', z$x[1], z$adjusted_rate[1])
  stopifnot(any(grepl(coordinate, fig, fixed = TRUE)))
}
assets <- unique(manifest[grepl('\\.(pdf|png)$', destination), .(destination, source)])
stopifnot(nrow(assets) == 34,
  identical(unname(tools::md5sum(assets$destination)), unname(tools::md5sum(assets$source))))
# Existing report is tracked in the project repository and must remain unchanged.
status <- system2('git', c('-C', shQuote(project), 'diff', '--exit-code', '--', 'wwII_gi_bill.tex'),
                  stdout = FALSE, stderr = FALSE)
stopifnot(status == 0)
message('Report checks passed: primary/placebo selection, all robustness coefficients, inline plot data, 34 figure assets, provenance and preserved existing report.')
