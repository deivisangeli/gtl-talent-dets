###############################################################################
# Yearly event study, 1860-1910 elite-school openings.
# Same panel/specs as the decade study; this version is at the year grain
# and runs BOTH the AMWS combined outcomes and the Wikipedia STEM outcomes.
#
# Estimator: Callaway-Sant'Anna (did::att_gt + aggte) with population (level)
# at 1860 as time-invariant covariate.
#
# Three panel specs:
#   (i)   drop_nyc        — all 5 NYC boroughs removed; 2 events
#   (ii)  merge_nyc       — 5 NYC boroughs aggregated to synthetic GEOID 36000,
#                            treated at Hunter HS 1869; 3 events
#   (iii) boroughs_sep    — each borough its own unit; Manhattan treated 1869,
#                            other boroughs controls; 3 events
#
# Outcomes:
#   AMWS  : n_amws, amws_per_1000_pop, amws_per_1000_births
#   Wiki  : n_stem, stem_per_1000_pop, stem_per_1000_births
# Controls: counties with at least one low-access (private tuition) school
#           founded <= 1910.
# Window:   -10 / +20 years (10 pre, 20 post).
# Outputs:  Dropbox results/elite_schools/event_study_yearly_1860_1910/<spec>/<outcome>.png+.csv
###############################################################################
suppressPackageStartupMessages({
  library(data.table); library(did); library(ggplot2)
})
args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repo_root <- if (basename(cwd) == "analysis") dirname(cwd) else if (basename(dirname(cwd)) == "analysis") dirname(dirname(cwd)) else cwd
}
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "analysis", "elite_schools", "amws_timing_helpers.R"))
timing <- elite_timing_config()

events <- data.table(
  GEOID      = c("36061",        "11001",         "39061"),
  unit_label = c("NY Manhattan", "Washington DC", "Hamilton Co OH (Cincinnati)"),
  school     = c("Hunter HS",    "Dunbar HS",     "Walnut Hills HS"),
  opening_year = c(1869L,        1870L,           1895L)
)
events[, event_year := elite_event_year(opening_year, timing)]
# Baltimore city + Baltimore Co are merged and treated as contaminated:
# the metro already had Baltimore City College (1839) + Western HS (1844)
# before 1860, so McDonogh (1873, Owings Mills) is a marginal addition
# rather than first treatment. Merging avoids the city/county split-noise too.
contaminate <- c("25025",   # Suffolk MA (Boston Latin, 1635)
                 "06075",   # San Francisco (Lowell HS, 1856 — pre-1860)
                 "24510",   # Baltimore city (BCC 1839, Western 1844)
                 "24005",   # Baltimore Co (merged with city as one metro)
                 "42101")   # Philadelphia (Central HS 1836, Girls' HS 1848)
NYC_BOROUGHS <- c("36005","36047","36061","36081","36085")

# ---- Control counties: those with at least one LOW-ACCESS school -----------
# Low-access = private_tuition_dominant secondary school in the 1800-1940 frame
# that was founded <= 1910 (active during the event window).
schools <- fread(file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"))
low_access <- schools[access_model_historical_prelim == "private_tuition_dominant" &
                       crit_secondary_school == "yes" &
                       crit_in_frame_1800_1940 == "yes" &
                       founding_year_used <= 1910]
low_access_counties <- sprintf("%05d", unique(low_access$county_geoid))
cat("Low-access (private tuition) counties:", length(low_access_counties), "\n")

out_root <- elite_results_dir(
  TALENT_DETS_DATA_DIR, "event_study_yearly_1860_1910", timing
)
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

p <- fread(file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv"))
p[, GEOID := sprintf("%05d", as.integer(GEOID))]
p <- p[year >= min(events$event_year) - 10L &
       year <= max(events$event_year) + 20L]
p <- p[!GEOID %in% contaminate]

# Time-invariant covariate: pre-treatment population at 1820.
panel_full <- fread(file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv"))
panel_full[, GEOID := sprintf("%05d", as.integer(GEOID))]
pop1820 <- panel_full[year == 1820, .(GEOID, log_pop_1820 = log1p(population))]
p <- merge(p, pop1820, by = "GEOID", all.x = TRUE)

event_map <- setNames(events$event_year, events$GEOID)

build_panel <- function(spec) {
  d <- copy(p)
  # Restrict controls to low-access-school counties (controls keep their
  # original GEOIDs; treated keep theirs; everything else dropped)
  treated_geoids <- names(event_map)
  if (spec == "drop_nyc") {
    keep_treated  <- setdiff(treated_geoids, "36061")
    keep_controls <- setdiff(low_access_counties,
                             c(treated_geoids, NYC_BOROUGHS))
    d <- d[GEOID %in% c(keep_treated, keep_controls)]
    treat_map <- event_map[keep_treated]
  } else if (spec == "merge_nyc") {
    keep_treated_geo  <- setdiff(treated_geoids, "36061")  # plus synthetic 36000 below
    keep_controls <- setdiff(low_access_counties,
                             c(treated_geoids, NYC_BOROUGHS))
    d <- d[GEOID %in% c(keep_treated_geo, keep_controls, NYC_BOROUGHS)]
    d_nyc <- d[GEOID %in% NYC_BOROUGHS,
               .(GEOID = "36000",
                 population = sum(population, na.rm = TRUE),
                 county_births_estimate_year =
                   sum(county_births_estimate_year, na.rm = TRUE),
                 log_pop_1820 = log1p(sum(expm1(log_pop_1820), na.rm = TRUE)),
                 n_amws = sum(n_amws, na.rm = TRUE),
                 n_stem = sum(n_stem, na.rm = TRUE)),
               by = year]
    d_nyc[, amws_per_1000_pop := ifelse(population > 0,
                                         1000 * n_amws / population, NA_real_)]
    d_nyc[, amws_per_1000_births := ifelse(county_births_estimate_year > 0,
                                            1000 * n_amws / county_births_estimate_year,
                                            NA_real_)]
    d_nyc[, stem_per_1000_pop := ifelse(population > 0,
                                         1000 * n_stem / population, NA_real_)]
    d_nyc[, stem_per_1000_births := ifelse(county_births_estimate_year > 0,
                                            1000 * n_stem / county_births_estimate_year,
                                            NA_real_)]
    keep_cols <- c("GEOID","year","population","county_births_estimate_year",
                   "n_amws","amws_per_1000_pop","amws_per_1000_births",
                   "n_stem","stem_per_1000_pop","stem_per_1000_births",
                   "log_pop_1820")
    d <- rbind(d[!GEOID %in% NYC_BOROUGHS, ..keep_cols], d_nyc, fill = TRUE)
    treat_map <- c(event_map[setdiff(names(event_map), "36061")],
                   "36000" = unname(event_map[["36061"]]))
  } else if (spec == "boroughs_sep") {
    keep_treated  <- treated_geoids
    keep_controls <- setdiff(low_access_counties, treated_geoids)
    d <- d[GEOID %in% c(keep_treated, keep_controls)]
    treat_map <- event_map
  } else stop("unknown spec")
  d[, g := treat_map[GEOID]]
  d[is.na(g), g := 0L]      # never-treated = control
  d[, id := as.integer(factor(GEOID))]
  d
}

run_cs <- function(d, outcome, label) {
  # CS requires non-NA outcome and covariate
  d_use <- d[is.finite(get(outcome)) & !is.na(log_pop_1820)]
  sample_counts <- d_use[, .(
    n_events = uniqueN(GEOID[g != 0L]),
    n_treated_units = uniqueN(GEOID[g != 0L]),
    n_control_units = uniqueN(GEOID[g == 0L])
  )]
  # Drop singletons
  out <- tryCatch(
    att_gt(yname        = outcome,
           tname        = "year",
           idname       = "id",
           gname        = "g",
           xformla      = ~ log_pop_1820,
           data         = as.data.frame(d_use),
           control_group= "nevertreated",
           est_method   = "reg",
           panel        = TRUE,
           allow_unbalanced_panel = TRUE,
           bstrap       = FALSE),
    error = function(e) e)
  if (inherits(out, "error")) {
    cat("  CS failed for", outcome, ":", conditionMessage(out), "\n")
    return(NULL)
  }
  agg <- tryCatch(
    aggte(out, type = "dynamic", min_e = -10, max_e = 20,
          balance_e = NULL, na.rm = TRUE),
    error = function(e) e)
  if (inherits(agg, "error")) {
    cat("  aggte failed for", outcome, ":", conditionMessage(agg), "\n")
    return(NULL)
  }
  df <- data.table(e   = agg$egt,
                   est = agg$att.egt,
                   se  = agg$se.egt)
  df[, lo := est - 1.96 * se]
  df[, hi := est + 1.96 * se]
  df[, outcome := outcome]; df[, spec := label]
  list(estimates = df, sample_counts = sample_counts)
}

add_sample_annotation <- function(plot, sample_counts, size = 3) {
  plot + annotate(
    "label",
    x = Inf, y = Inf, hjust = 1.05, vjust = 1.05,
    label = sprintf(
      "Events: %d\nTreated units: %d\nControl units: %d",
      sample_counts$n_events,
      sample_counts$n_treated_units,
      sample_counts$n_control_units
    ),
    size = size,
    label.padding = grid::unit(0.4, "lines"),
    label.r = grid::unit(0, "lines")
  )
}

plot_event <- function(df, label, outcome, sample_counts) {
  df <- df[e >= -10 & e <= 20]
  df <- rbind(df, data.table(e = -1L, est = 0, se = 0,
                             lo = 0, hi = 0,
                             outcome = outcome, spec = label), fill = TRUE)
  setorder(df, e)
  plot <- ggplot(df, aes(e, est)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_vline(xintercept = -0.5, color = "grey60", linetype = "dashed") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, fill = "#0072B2") +
    geom_line(color = "#0072B2", group = 1) +
    geom_point(color = "#0072B2", size = 1.8) +
    scale_x_continuous(breaks = seq(-10, 20, 5)) +
    labs(x = "Years relative to treatment timing",
         y = sprintf("Effect on %s  (vs e=-1)", outcome),
         title = NULL,
         subtitle = sprintf("timing: %s  |  covariate: log pop 1820",
                            timing$label)) +
    theme_minimal(base_size = 11)
  add_sample_annotation(plot, sample_counts)
}

OUTCOMES <- c("n_amws", "amws_per_1000_pop", "amws_per_1000_births",
              "n_stem", "stem_per_1000_pop", "stem_per_1000_births",
              "population")

run_spec <- function(spec) {
  cat("\n=== spec:", spec, "===\n")
  d <- build_panel(spec)
  treat_map <- switch(spec,
    drop_nyc     = event_map[setdiff(names(event_map), "36061")],
    merge_nyc    = c(event_map[setdiff(names(event_map), "36061")],
                     "36000" = unname(event_map[["36061"]])),
    boroughs_sep = event_map)
  n_units    <- length(treat_map)
  n_controls <- uniqueN(d[g == 0L]$GEOID)
  cat("  treated units:", n_units, " | panel rows:", nrow(d),
      " | counties:", uniqueN(d$GEOID), "\n")
  cat("  control units (never-treated):", n_controls, "\n")

  out_dir <- file.path(out_root, spec); dir.create(out_dir, showWarnings = FALSE)
  res_all <- list()
  for (oc in OUTCOMES) {
    fit <- run_cs(d, oc, spec)
    if (is.null(fit)) next
    res <- fit$estimates
    res_all[[oc]] <- res
    fwrite(res, file.path(out_dir, sprintf("event_study_%s.csv", oc)))
    pl <- plot_event(res, spec, oc, fit$sample_counts)
    ggsave(file.path(out_dir, sprintf("event_study_%s.png", oc)),
           pl, width = 8, height = 5, dpi = 140)
  }

  # ---- Raw treated-vs-control means by calendar year ----------------------
  for (oc in OUTCOMES) {
    rm_oc <- d[, .(treated = mean(get(oc)[g != 0L], na.rm = TRUE),
                   control = mean(get(oc)[g == 0L], na.rm = TRUE)), by = year]
    rm_long <- melt(rm_oc, id.vars = "year",
                    measure.vars = c("treated","control"),
                    variable.name = "group", value.name = "mean")
    pl <- ggplot(rm_long, aes(year, mean, color = group)) +
      geom_line(linewidth = 0.7) +
      geom_vline(xintercept = unique(treat_map), color = "grey50",
                 linetype = "dashed") +
      scale_color_manual(values = c(treated = "#0072B2", control = "#D55E00"),
                         name = NULL) +
      labs(x = "Year", y = sprintf("Mean %s per county-year", oc),
           subtitle = sprintf("Raw means | N treated: %d | N controls: %d | dashed lines = event years",
                              n_units, n_controls)) +
      theme_minimal(base_size = 11) + theme(legend.position = "bottom")
    ggsave(file.path(out_dir, sprintf("raw_means_%s.png", oc)),
           pl, width = 8, height = 5, dpi = 140)
  }
  rbindlist(res_all, fill = TRUE)
}

results <- list()
for (spec in c("drop_nyc", "merge_nyc", "boroughs_sep")) {
  results[[spec]] <- run_spec(spec)
}
all <- rbindlist(results, fill = TRUE)
all[, `:=`(timing_mode = timing$mode, school_age = timing$school_age)]
fwrite(all, file.path(out_root, "all_event_study_estimates.csv"))
fwrite(events, file.path(out_root, "treatment_events.csv"))
cat("\nAll specs done (Callaway-Sant'Anna). Results in", out_root, "\n")
