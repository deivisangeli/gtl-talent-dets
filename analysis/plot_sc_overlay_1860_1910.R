suppressPackageStartupMessages({library(data.table); library(ggplot2)})
events <- data.table(unit = c("NY_Manhattan","Washington_DC","Hamilton_Co_OH"),
                     event_year = c(1869, 1870, 1895))
sc <- fread("analysis/results/event_study_yearly_1860_1910/sc/all_sc_estimates.csv")
sc <- merge(sc, events, by = "unit")
sc[, e := year - event_year]
sc[, label := sprintf("%s (g=%d)", unit, event_year)]

for (oc in c("n_amws", "amws_per_1000_pop", "n_stem", "stem_per_1000_pop",
             "population")) {
  d <- sc[outcome == oc]
  if (nrow(d) == 0) next
  p <- ggplot(d, aes(e, gap, color = label)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = -0.5, color = "grey60", linetype = "dashed") +
    geom_line(linewidth = 0.7) + geom_point(size = 1.2) +
    # Okabe-Ito 4-color palette
    scale_color_manual(values = c("#0072B2","#D55E00","#009E73","#CC79A7")) +
    labs(title = sprintf("Synthetic-control gap (treated - synth) - %s", oc),
         subtitle = "pre: 1860 to event-1  |  post: event to event+20",
         x = "Event time (years since school opening)",
         y = paste("gap:", oc), color = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(sprintf("analysis/results/event_study_yearly_1860_1910/sc/sc_gap_overlay_%s.png", oc),
         p, width = 9, height = 5.5, dpi = 140)
}
cat("overlay plots written\n")
