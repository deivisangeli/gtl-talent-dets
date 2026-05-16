###############################################################################
# Per-event synthetic control for the 4 events 1860-1910.
# Same panel as the yearly event study.
#
# For each treated unit, fit SC weights on each outcome separately (AMWS and
# Wikipedia STEM, in counts and per-1000-pop). Pre-period: 1860 -> event-1.
# Post: event -> event+20. Donor pool: top 100 counties by 1860 Census pop
# minus the 4 treated counties, the contaminated set, and the other 4 NYC
# boroughs (so they don't act as donors for Manhattan).
#
# Output: analysis/results/event_study_yearly_1860_1910/sc/<unit>/
###############################################################################
suppressPackageStartupMessages({
  library(data.table); library(Synth); library(ggplot2)
})
source("paths.R")

out_root <- "analysis/results/event_study_yearly_1860_1910/sc"
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

events <- data.table(
  GEOID      = c("36061",        "11001",         "39061"),
  unit_label = c("NY_Manhattan", "Washington_DC", "Hamilton_Co_OH"),
  event_year = c(1869,           1870,            1895)
)
# Baltimore city + Baltimore Co merged and dropped (pre-1860 BCC/Western HS)
contaminate <- c("25025","06075","24510","24005","42101")
NYC_OTHER <- c("36005","36047","36081","36085")  # other boroughs excluded as donors

# ---- Load panel ------------------------------------------------------------
p <- fread(file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv"))
p[, GEOID := sprintf("%05d", as.integer(GEOID))]
p <- p[year >= 1840 & year <= 1925]   # +20y post for Hamilton (event 1895)

# ---- Donor pool: top 100 counties by 1860 population -----------------------
pop1860 <- p[year == 1860, .(GEOID, population)]
setorder(pop1860, -population)
top100_donors <- pop1860[1:100]$GEOID
# Exclude treated, contaminated, and other NYC boroughs from donors
donors <- setdiff(top100_donors,
                  c(events$GEOID, contaminate, NYC_OTHER))
cat("Donor pool size after exclusions:", length(donors), "\n")

# Numeric unit IDs (Synth requires integer)
units_keep <- c(events$GEOID, donors)
p_sc <- p[GEOID %in% units_keep,
          .(GEOID, year, n_amws, amws_per_1000_pop,
            n_stem, stem_per_1000_pop, population)]
# stable integer id per GEOID; Synth needs a unit-names character column too
ids <- data.table(GEOID = unique(p_sc$GEOID))
ids[, unit_id := .I]
p_sc <- merge(p_sc, ids, by = "GEOID")
p_sc[, unit_name := GEOID]
# Replace NA/Inf with 0 so Synth doesn't crash
p_sc[!is.finite(amws_per_1000_pop), amws_per_1000_pop := 0]
p_sc[!is.finite(n_amws),            n_amws            := 0]
p_sc[!is.finite(stem_per_1000_pop), stem_per_1000_pop := 0]
p_sc[!is.finite(n_stem),            n_stem            := 0]
# Synth wants a plain data.frame, integer unit_id, integer year
p_sc[, unit_id := as.integer(unit_id)]
p_sc[, year    := as.integer(year)]
setDF(p_sc)

run_sc <- function(treated_geoid, ev_year, label) {
  cat("\n--- SC:", label, "(", treated_geoid, ", event", ev_year, ") ---\n")
  treated_id <- ids[GEOID == treated_geoid]$unit_id
  donor_ids  <- ids[GEOID %in% donors]$unit_id

  pre_yrs  <- 1860:(ev_year - 1)
  post_yrs <- ev_year:(ev_year + 20)
  all_yrs  <- min(pre_yrs):max(post_yrs)
  d_unit   <- as.data.table(p_sc)[unit_id %in% c(treated_id, donor_ids) &
                                  year %in% all_yrs]
  setDF(d_unit)

  out_dir <- file.path(out_root, label)
  dir.create(out_dir, showWarnings = FALSE)

  results_list <- list()
  for (outcome in c("n_amws", "amws_per_1000_pop",
                    "n_stem", "stem_per_1000_pop",
                    "population")) {
    cat("  outcome:", outcome, "\n")
    # Synth dataprep
    dp <- tryCatch(
      dataprep(foo = d_unit,
               predictors = NULL,
               dependent = outcome,
               unit.variable = "unit_id",
               time.variable = "year",
               special.predictors = lapply(pre_yrs, function(y)
                 list(outcome, y, "mean")),
               treatment.identifier = treated_id,
               controls.identifier  = donor_ids,
               time.predictors.prior = pre_yrs,
               time.optimize.ssr     = pre_yrs,
               time.plot             = all_yrs,
               unit.names.variable   = "unit_name"),
      error = function(e) e)
    if (inherits(dp, "error")) {
      cat("    dataprep error:", conditionMessage(dp), "\n"); next
    }
    sc <- tryCatch(synth(dp, verbose = FALSE), error = function(e) e)
    if (inherits(sc, "error")) {
      cat("    synth error:", conditionMessage(sc), "\n"); next
    }
    y1 <- as.numeric(dp$Y1plot)
    y0 <- as.numeric(dp$Y0plot %*% sc$solution.w)
    res <- data.table(year = all_yrs, treated = y1, synth = y0,
                      gap = y1 - y0, outcome = outcome, unit = label)
    fwrite(res, file.path(out_dir, sprintf("sc_%s.csv", outcome)))

    # weights
    w <- data.table(donor_id = as.integer(rownames(sc$solution.w)),
                    weight = as.numeric(sc$solution.w))
    w <- merge(w, ids, by.x = "donor_id", by.y = "unit_id")
    setorder(w, -weight)
    fwrite(w[weight > 1e-4], file.path(out_dir, sprintf("weights_%s.csv", outcome)))
    cat("    top 5 donor weights:\n"); print(head(w[weight > 1e-4], 5))

    pl <- ggplot(res, aes(year)) +
      geom_line(aes(y = treated, color = "Treated"), linewidth = 0.8) +
      geom_line(aes(y = synth,   color = "Synthetic"),
                linewidth = 0.8, linetype = "dashed") +
      geom_vline(xintercept = ev_year - 0.5, color = "grey40",
                 linetype = "dotted") +
      scale_color_manual(values = c("Treated"   = "#0072B2",   # Okabe-Ito blue
                                    "Synthetic" = "#D55E00"),  # vermillion
                         name = NULL) +
      labs(title = sprintf("SC: %s  |  event %d", label, ev_year),
           subtitle = sprintf("Outcome: %s  |  pre: 1860-%d  |  donors: top 100 by 1860 pop",
                            outcome, ev_year - 1),
           x = "Year", y = outcome) +
      theme_minimal(base_size = 11) + theme(legend.position = "bottom")
    ggsave(file.path(out_dir, sprintf("sc_%s.png", outcome)),
           pl, width = 8, height = 5, dpi = 140)
    results_list[[outcome]] <- res
  }
  rbindlist(results_list, fill = TRUE)
}

all_sc <- list()
for (i in seq_len(nrow(events))) {
  all_sc[[i]] <- run_sc(events$GEOID[i], events$event_year[i], events$unit_label[i])
}
combined <- rbindlist(all_sc, fill = TRUE)
fwrite(combined, file.path(out_root, "all_sc_estimates.csv"))
cat("\nAll SC done. Results in", out_root, "\n")
