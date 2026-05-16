###############################################################################
# For each of the 4 treated counties, plot AMWS combined births vs Wikipedia
# STEM births over time, each normalized so its 1900 value = 100.
# Output: analysis/results/event_study_yearly_1860_1910/normalized_1900_<unit>.png
###############################################################################
suppressPackageStartupMessages({
  library(data.table); library(ggplot2)
})
source("paths.R")

events <- data.table(
  GEOID      = c("36061",         "11001",          "39061"),
  unit_label = c("NY_Manhattan",  "Washington_DC",  "Hamilton_Co_OH"),
  pretty     = c("NY Manhattan (Hunter HS 1869)",
                 "Washington DC (Dunbar HS 1870)",
                 "Hamilton Co OH (Walnut Hills 1895)"),
  event_year = c(1869, 1870, 1895)
)

p <- fread(file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv"))
p[, GEOID := sprintf("%05d", as.integer(GEOID))]
p <- p[year >= 1840 & year <= 1930]

out_dir <- "analysis/results/event_study_yearly_1860_1910"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Normalize to mean over a centered window around 1900. Use a 21-year window
# (1890-1910) because the Wikipedia STEM series has zeros in some counties for
# the narrow 1898-1902 window (Baltimore Co and Hamilton Co both have 0 STEM
# births in those 5 years).
norm_window <- 1890:1910

for (i in seq_len(nrow(events))) {
  geo  <- events$GEOID[i]
  unit <- events$unit_label[i]
  ev   <- events$event_year[i]
  d <- p[GEOID == geo, .(year, n_amws, n_stem)]
  base_amws <- mean(d[year %in% norm_window]$n_amws, na.rm = TRUE)
  base_stem <- mean(d[year %in% norm_window]$n_stem, na.rm = TRUE)
  if (base_amws == 0 || base_stem == 0) {
    cat("WARN", unit, "base zero — amws:", base_amws, "stem:", base_stem, "\n")
  }
  d[, AMWS_combined := 100 * n_amws / base_amws]
  d[, Wikipedia_STEM := 100 * n_stem / base_stem]
  d_long <- melt(d, id.vars = "year",
                 measure.vars = c("AMWS_combined", "Wikipedia_STEM"),
                 variable.name = "source", value.name = "index")

  # Okabe-Ito: blue (#0072B2) + vermillion (#D55E00). Raw lines only.
  pl <- ggplot(d_long, aes(year, index, color = source)) +
    geom_hline(yintercept = 100, color = "grey70", linetype = "dotted") +
    geom_vline(xintercept = ev, color = "grey40", linetype = "dashed") +
    geom_line(linewidth = 0.7) +
    scale_color_manual(values = c(AMWS_combined  = "#0072B2",
                                  Wikipedia_STEM = "#D55E00"),
                       name = NULL) +
    labs(x = "Birth year",
         y = "Births  (index: 1890-1910 mean = 100)",
         subtitle = sprintf("%s  |  school opens %d  |  raw 1890-1910 mean: AMWS=%.2f  STEM=%.2f",
                            events$pretty[i], ev, base_amws, base_stem)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  ggsave(file.path(out_dir, sprintf("normalized_1900_%s.png", unit)),
         pl, width = 9, height = 5, dpi = 140)
  cat("wrote", unit, "\n")
}
