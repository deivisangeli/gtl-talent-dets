# Distribution of the Gapminder monthly birth denominator, 1800-1960.
# Run from any working directory. Figures stay in Dropbox results.
args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep('^--file=', args, value = TRUE)
repo_root <- if (length(script_arg)) {
  normalizePath(file.path(dirname(sub('^--file=', '', script_arg[1])), '../..'), winslash = '/')
} else Sys.getenv('GTL_REPO', unset = getwd())
source(file.path(repo_root, 'paths.R'))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})

input <- file.path(DATA_OUTPUT, 'mandatory_schooling',
                   'county_gapminder_annual_births_1800_1960.csv')
d <- fread(input, select = c('GEOID', 'year', 'county_births_estimate_month'),
           colClasses = list(character = 'GEOID'))
stopifnot(!anyDuplicated(d[, .(GEOID, year)]), nrow(d) == 3092L * 161L,
          identical(sort(unique(d$year)), 1800:1960))
x <- d$county_births_estimate_month
stopifnot(all(is.na(x) | is.finite(x)), all(is.na(x) | x > 0))
x <- x[!is.na(x)]
# Every observed county-year contributes twelve identical monthly values.
# Thus the annual table gives exactly the same histogram shares as the panel.
observed_months <- length(x) * 12L
missing_months <- (nrow(d) - length(x)) * 12L
cutoff <- unname(quantile(x, .99))
fmt <- function(z) format(round(z), big.mark = ',', scientific = FALSE, trim = TRUE)

make_bins <- function(values, breaks, transform = identity) {
  h <- hist(values, breaks = breaks, plot = FALSE, include.lowest = TRUE)
  data.table(lower = transform(head(h$breaks, -1)),
             upper = transform(tail(h$breaks, -1)),
             share = h$counts / length(x) * 100)
}
linear_bins <- make_bins(x[x <= cutoff], seq(0, cutoff, length.out = 51))
log_bins <- make_bins(log10(x), seq(log10(min(x)) - 1e-9,
                                    log10(max(x)) + 1e-9, length.out = 51),
                      function(z) 10^z)
stopifnot(abs(sum(log_bins$share) - 100) < 1e-8,
          sum(linear_bins$share) >= 98.9, sum(linear_bins$share) <= 99.1)

chart_theme <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 11, color = '#526174'),
        axis.title = element_text(size = 12),
        plot.margin = margin(12, 16, 12, 12))
make_plot <- function(bins) {
  ggplot(bins, aes(xmin = lower, xmax = upper, ymin = 0, ymax = share)) +
    geom_rect(fill = '#2D708E', color = 'white', linewidth = .2) +
    scale_y_continuous(labels = function(z) paste0(z, '%'),
                       expand = expansion(mult = c(0, .06))) +
    labs(x = 'Estimated births per county-month', y = 'Share of observed county-months') +
    chart_theme
}
p1 <- make_plot(linear_bins) +
  scale_x_continuous(breaks = seq(0, 750, 150), limits = c(0, cutoff),
                     expand = expansion(mult = c(0, .01))) +
  labs(title = 'Central 99% of observations',
       subtitle = sprintf('Linear scale. Values above %.1f births omitted from this view.', cutoff))
p2 <- make_plot(log_bins) +
  scale_x_log10(breaks = c(.01, .1, 1, 10, 100, 1000, 10000),
               labels = c('0.01', '0.1', '1', '10', '100', '1,000', '10,000'),
               expand = expansion(mult = c(.01, .01))) +
  labs(title = 'Full distribution',
       subtitle = 'Logarithmic horizontal scale. Equal-width bins in log10 births.')

draw <- function() {
  grid::grid.newpage()
  grid::grid.text('Estimated monthly births across US counties, 1800-1960',
                 x = .035, y = .955, just = 'left',
                 gp = grid::gpar(fontsize = 21, fontface = 'bold', col = '#1B2938'))
  grid::grid.text(sprintf('%s observed county-months   |   Median %.1f   |   Mean %.1f',
                          fmt(observed_months), median(x), mean(x)),
                 x = .035, y = .905, just = 'left',
                 gp = grid::gpar(fontsize = 13, col = '#526174'))
  print(p1, vp = grid::viewport(x = .25, y = .49, width = .49, height = .72))
  print(p2, vp = grid::viewport(x = .75, y = .49, width = .49, height = .72))
  note <- sprintf(paste0('Source: Gapminder county estimates in the AMWS panel. Annual county births divided by 12; all months weighted equally.\n',
                         '%s county-months with unavailable birth estimates excluded. There are no observed zero or negative birth denominators.'),
                  fmt(missing_months))
  grid::grid.text(note, x = .035, y = .055, just = 'left',
                 gp = grid::gpar(fontsize = 11, col = '#526174', lineheight = 1.4))
}
dest <- file.path(TALENT_DETS_DATA_DIR, 'results', 'mandatory_schooling')
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
stem <- file.path(dest, 'monthly_births_histogram_gapminder_1800_1960')
png(paste0(stem, '.png'), width = 2700, height = 1350, res = 180)
draw()
dev.off()
pdf(paste0(stem, '.pdf'), width = 15, height = 7.5, useDingbats = FALSE)
draw()
dev.off()
cat(sprintf('Saved PNG and PDF to %s\nObserved months: %s; missing months: %s; median: %.4f; mean: %.4f\n',
            dest, fmt(observed_months), fmt(missing_months), median(x), mean(x)))
