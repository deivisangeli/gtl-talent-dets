###############################################################################
# Decennial elite-school event studies, common AMWS/Wikipedia support 1800-1960
#
# Estimators:
#   - Callaway-Sant'Anna: high-access vs low-access, log 1820 population
#   - Sun-Abraham/Wooldridge ETWFE: high vs never, low vs never, high vs low
#
# Timings: first fully treated birth decade under opening and opening-14 rules.
# Geographies: drop_nyc, merge_nyc, boroughs_sep.
###############################################################################
suppressPackageStartupMessages({
  library(data.table)
  library(did)
  library(fixest)
  library(ggplot2)
  library(patchwork)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg)) {
  script_path <- normalizePath(
    sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/", mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE
  )
}
source(file.path(repo_root, "paths.R"))

normalize_geoid <- function(x) sprintf("%05d", suppressWarnings(as.integer(x)))
NYC_BOROUGHS <- c("36005", "36047", "36061", "36081", "36085")
EVENT_GRID <- seq(-50L, 50L, by = 10L)
REF_EVENT <- -10L
NEVER_G <- 10000L

tag <- trimws(Sys.getenv(
  "ELITE_RESULTS_TAG",
  unset = "countyfix_amws1986_20260719_decennial_1800_1960"
))
if (!nzchar(tag)) stop("ELITE_RESULTS_TAG cannot be empty")

timing_env <- trimws(Sys.getenv("ELITE_TREATMENT_TIMING", unset = ""))
timing_modes <- if (nzchar(timing_env)) timing_env else c("exposure14", "opening")
if (any(!timing_modes %in% c("exposure14", "opening"))) {
  stop("ELITE_TREATMENT_TIMING must be exposure14 or opening")
}

geo_env <- trimws(Sys.getenv("ELITE_GEO_SPEC", unset = ""))
geo_specs <- if (nzchar(geo_env)) geo_env else c(
  "drop_nyc", "merge_nyc", "boroughs_sep"
)
if (any(!geo_specs %in% c("drop_nyc", "merge_nyc", "boroughs_sep"))) {
  stop("ELITE_GEO_SPEC must be drop_nyc, merge_nyc, or boroughs_sep")
}

source_root <- file.path(
  TALENT_DETS_DATA_DIR, "results", "elite_schools",
  "event_study_decennial_1800_1960", tag
)
delivery_root <- file.path(
  TALENT_DETS_DATA_DIR, "results", "elite_schools",
  "event_study_figures", tag
)
dir.create(source_root, recursive = TRUE, showWarnings = FALSE)
dir.create(delivery_root, recursive = TRUE, showWarnings = FALSE)

panel_base <- fread(file.path(
  DATA_OUTPUT, "us_panel_county_amws_combined_decade.csv"
))
required_panel <- c(
  "GEOID", "decade", "population", "population_source",
  "county_births_estimate_decade", "n_amws", "amws_per_1000_pop",
  "amws_per_1000_births", "n_stem", "stem_per_1000_pop",
  "stem_per_1000_births"
)
missing_panel <- setdiff(required_panel, names(panel_base))
if (length(missing_panel)) {
  stop("Missing decennial panel columns: ", paste(missing_panel, collapse = ", "))
}
panel_base[, `:=`(
  GEOID = normalize_geoid(GEOID),
  decade = as.integer(decade)
)]
panel_base <- panel_base[
  decade >= 1800L & decade <= 1960L &
    population_source != "hyde" & is.finite(population) & population > 0
]

schools_base <- fread(file.path(
  SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"
))
schools_base[, `:=`(
  county_geoid = normalize_geoid(county_geoid),
  founding_year_used = as.integer(founding_year_used)
)]

cohort_decade <- function(opening_year, timing_mode) {
  age <- if (identical(timing_mode, "exposure14")) 14L else 0L
  as.integer(10L * ceiling((as.integer(opening_year) - age) / 10))
}

timing_label <- function(mode) {
  if (identical(mode, "exposure14")) {
    "first fully exposed birth decade (opening - 14)"
  } else {
    "first birth decade fully after school opening"
  }
}

recompute_rates <- function(p) {
  p[, `:=`(
    amws_per_1000_pop = fifelse(
      population > 0, 1000 * n_amws / population, NA_real_
    ),
    amws_per_1000_births = fifelse(
      county_births_estimate_decade > 0,
      1000 * n_amws / county_births_estimate_decade,
      NA_real_
    ),
    stem_per_1000_pop = fifelse(
      population > 0, 1000 * n_stem / population, NA_real_
    ),
    stem_per_1000_births = fifelse(
      county_births_estimate_decade > 0,
      1000 * n_stem / county_births_estimate_decade,
      NA_real_
    )
  )]
  p
}

build_geo_panel <- function(geo_spec) {
  keep <- c(
    "GEOID", "decade", "population", "population_source",
    "county_births_estimate_decade", "n_amws", "n_stem",
    "amws_per_1000_pop", "amws_per_1000_births",
    "stem_per_1000_pop", "stem_per_1000_births"
  )
  p <- copy(panel_base[, ..keep])
  if (identical(geo_spec, "drop_nyc")) {
    p <- p[!GEOID %in% NYC_BOROUGHS]
  } else if (identical(geo_spec, "boroughs_sep")) {
    p <- p[GEOID != "36005"]
  } else {
    nyc <- p[GEOID %in% NYC_BOROUGHS, .(
      GEOID = "36000",
      population = sum(population, na.rm = TRUE),
      population_source = "merged_nyc",
      county_births_estimate_decade = sum(
        county_births_estimate_decade, na.rm = TRUE
      ),
      n_amws = sum(n_amws, na.rm = TRUE),
      n_stem = sum(n_stem, na.rm = TRUE)
    ), by = decade]
    p <- rbindlist(
      list(p[!GEOID %in% NYC_BOROUGHS], nyc),
      fill = TRUE, use.names = TRUE
    )
  }
  p <- recompute_rates(p)
  pop1820 <- p[decade == 1820L, .(
    GEOID, log_pop_1820 = log1p(population)
  )]
  p <- merge(p, pop1820, by = "GEOID", all.x = TRUE)
  p[, id := as.integer(factor(GEOID))]
  setorder(p, GEOID, decade)
  p
}

build_treatment <- function(geo_spec, timing_mode) {
  s <- copy(schools_base)
  contaminated <- unique(s[
    as.character(contaminates_county) %chin% c("yes", "TRUE", "1"),
    county_geoid
  ])
  s <- s[!county_geoid %in% c(contaminated, "06075")]
  if (identical(geo_spec, "drop_nyc")) {
    s <- s[!county_geoid %in% NYC_BOROUGHS]
  } else if (identical(geo_spec, "boroughs_sep")) {
    s <- s[county_geoid != "36005"]
  } else {
    s[county_geoid %in% NYC_BOROUGHS, county_geoid := "36000"]
  }

  access <- s[, .(
    has_high = any(crit_high_access_strict == "yes"),
    first_high_year = if (any(crit_high_access_strict == "yes")) {
      min(founding_year_used[crit_high_access_strict == "yes"], na.rm = TRUE)
    } else NA_integer_,
    first_low_year = if (any(crit_high_access_strict == "no")) {
      min(founding_year_used[crit_high_access_strict == "no"], na.rm = TRUE)
    } else NA_integer_,
    first_any_year = min(founding_year_used, na.rm = TRUE),
    schools = paste(sort(unique(school)), collapse = "; ")
  ), by = .(GEOID = county_geoid)]

  high <- access[has_high == TRUE, .(
    GEOID, opening_year = first_high_year, schools
  )]
  low <- access[has_high == FALSE, .(
    GEOID, opening_year = first_any_year, schools
  )]
  high[, g := cohort_decade(opening_year, timing_mode)]
  low[, g := cohort_decade(opening_year, timing_mode)]

  expected <- c(drop_nyc = 5L, merge_nyc = 6L, boroughs_sep = 7L)
  if (nrow(high) != expected[[geo_spec]]) {
    stop(
      "Unexpected high-access count for ", geo_spec, ": ", nrow(high),
      " (expected ", expected[[geo_spec]], ")"
    )
  }
  list(
    schools = s,
    all_school_geoids = unique(access$GEOID),
    high = high[g > 1800L & g <= 1960L],
    low = low,
    low_treated = low[g > 1800L & g <= 1960L]
  )
}

sample_label <- function(counts) {
  sprintf(
    "Events: %d\nTreated units: %d\nControl units: %d",
    counts$n_events, counts$n_treated_units, counts$n_control_units
  )
}

add_sample_annotation <- function(plot, counts, size = 3) {
  plot + annotate(
    "label", x = Inf, y = Inf, hjust = 1.05, vjust = 1.05,
    label = sample_label(counts), size = size,
    label.padding = grid::unit(0.4, "lines"),
    label.r = grid::unit(0, "lines")
  )
}

ensure_reference <- function(d, value_col, se_col) {
  if (!(REF_EVENT %in% d$e)) {
    d <- rbindlist(list(
      d,
      data.table(e = REF_EVENT, value = 0, se_value = 0)
    ), fill = TRUE)
    setnames(d, c("value", "se_value"), c(value_col, se_col), skip_absent = TRUE)
  }
  setorder(d, e)
  d
}

CS_OUTCOMES <- data.table(
  variable = c(
    "n_amws", "amws_per_1000_pop", "amws_per_1000_births",
    "n_stem", "stem_per_1000_pop", "stem_per_1000_births", "population"
  ),
  label = c(
    "AMWS births (count)", "AMWS / 1000 pop", "AMWS / 1000 births",
    "Wikipedia STEM (count)", "Wikipedia STEM / 1000 pop",
    "Wikipedia STEM / 1000 births", "County population"
  )
)

SA_OUTCOMES <- data.table(
  variable = c("n_amws", "amws_per_1000_pop", "n_stem", "stem_per_1000_pop"),
  label = c(
    "AMWS births (count)", "AMWS / 1000 pop",
    "Wikipedia STEM (count)", "Wikipedia STEM / 1000 pop"
  ),
  log_count = c(TRUE, FALSE, TRUE, FALSE)
)

save_and_copy <- function(plot, source_file, delivery_file, width, height, dpi) {
  dir.create(dirname(source_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(delivery_file), recursive = TRUE, showWarnings = FALSE)
  ggsave(source_file, plot, width = width, height = height, dpi = dpi)
  if (!file.copy(source_file, delivery_file, overwrite = TRUE)) {
    stop("Could not copy figure to ", delivery_file)
  }
}

run_cs <- function(panel, treatment, timing_mode, geo_spec) {
  high <- treatment$high
  low <- treatment$low
  d <- panel[GEOID %in% c(high$GEOID, low$GEOID)]
  d <- merge(d, high[, .(GEOID, g)], by = "GEOID", all.x = TRUE)
  d[is.na(g), g := 0L]
  d[, id := as.integer(factor(GEOID))]

  source_dir <- file.path(
    source_root, "callaway_santanna", timing_mode, geo_spec
  )
  delivery_dir <- file.path(
    delivery_root, "callaway_santanna", timing_mode, geo_spec
  )
  estimates <- list()
  supports <- list()
  for (i in seq_len(nrow(CS_OUTCOMES))) {
    outcome <- CS_OUTCOMES$variable[i]
    label <- CS_OUTCOMES$label[i]
    d_use <- d[is.finite(get(outcome)) & is.finite(log_pop_1820)]
    counts <- d_use[, .(
      n_events = uniqueN(GEOID[g > 0L]),
      n_treated_units = uniqueN(GEOID[g > 0L]),
      n_control_units = uniqueN(GEOID[g == 0L])
    )]
    fit <- did::att_gt(
      yname = outcome, tname = "decade", idname = "id", gname = "g",
      xformla = ~ log_pop_1820, data = as.data.frame(d_use),
      control_group = "nevertreated", est_method = "reg",
      panel = TRUE, allow_unbalanced_panel = TRUE,
      bstrap = FALSE, cband = FALSE, base_period = "universal"
    )
    agg <- did::aggte(
      fit, type = "dynamic", min_e = -50L, max_e = 50L,
      balance_e = NULL, na.rm = TRUE, bstrap = FALSE, cband = FALSE
    )
    est <- data.table(
      e = as.integer(agg$egt), estimate = as.numeric(agg$att.egt),
      se = as.numeric(agg$se.egt)
    )[e %in% EVENT_GRID]
    if (!(REF_EVENT %in% est$e)) {
      est <- rbind(est, data.table(e = REF_EVENT, estimate = 0, se = 0))
    }
    est[e == REF_EVENT, `:=`(estimate = 0, se = 0)]
    est[, `:=`(
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se,
      outcome = outcome, outcome_label = label,
      timing_mode = timing_mode, geography = geo_spec,
      n_events = counts$n_events,
      n_treated_units = counts$n_treated_units,
      n_control_units = counts$n_control_units
    )]
    setorder(est, e)
    estimates[[outcome]] <- est

    support <- rbindlist(lapply(EVENT_GRID, function(event_time) {
      data.table(
        e = event_time,
        n_events = uniqueN(d_use[g > 0L & decade == g + event_time, GEOID])
      )
    }))
    support[, `:=`(
      outcome = outcome, timing_mode = timing_mode, geography = geo_spec
    )]
    supports[[outcome]] <- support

    plot <- ggplot(est, aes(e, estimate)) +
      geom_hline(yintercept = 0, color = "grey60") +
      geom_vline(xintercept = -5, color = "grey60", linetype = "dashed") +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2,
                  fill = "#0072B2") +
      geom_line(color = "#0072B2", linewidth = 0.6) +
      geom_point(color = "#0072B2", size = 1.8) +
      scale_x_continuous(breaks = EVENT_GRID) +
      labs(
        x = "Years relative to treatment decade",
        y = sprintf("Effect on %s (vs e=-10)", outcome),
        subtitle = sprintf(
          "timing: %s | geography: %s | covariate: log pop 1820",
          timing_label(timing_mode), geo_spec
        )
      ) +
      theme_minimal(base_size = 11)
    plot <- add_sample_annotation(plot, counts)
    filename <- sprintf("event_study_%s.png", outcome)
    save_and_copy(
      plot, file.path(source_dir, filename), file.path(delivery_dir, filename),
      width = 8, height = 5, dpi = 150
    )
  }
  out_est <- rbindlist(estimates, fill = TRUE)
  out_support <- rbindlist(supports, fill = TRUE)
  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  fwrite(out_est, file.path(source_dir, "event_study_estimates.csv"))
  fwrite(out_support, file.path(source_dir, "event_time_support.csv"))
  list(estimates = out_est, support = out_support)
}

build_sa_sample <- function(panel, treatment, spec_code) {
  high <- treatment$high
  low <- treatment$low
  low_treated <- treatment$low_treated
  never <- setdiff(unique(panel$GEOID), treatment$all_school_geoids)
  if (identical(spec_code, "A")) {
    treated <- high
    controls <- never
  } else if (identical(spec_code, "B")) {
    treated <- low_treated
    controls <- never
  } else {
    treated <- high
    controls <- low$GEOID
  }
  p <- panel[GEOID %in% c(treated$GEOID, controls)]
  p <- merge(p, treated[, .(GEOID, g)], by = "GEOID", all.x = TRUE)
  p[is.na(g), g := 0L]
  required <- SA_OUTCOMES$variable
  p <- p[complete.cases(p[, ..required])]
  p
}

run_sa_spec <- function(panel, treatment, spec_code, spec_label,
                        timing_mode, geo_spec) {
  p <- build_sa_sample(panel, treatment, spec_code)
  model_results <- lapply(seq_len(nrow(SA_OUTCOMES)), function(i) {
    outcome <- SA_OUTCOMES$variable[i]
    label <- SA_OUTCOMES$label[i]
    dt <- copy(p)
    dt[, yy := if (SA_OUTCOMES$log_count[i]) log1p(get(outcome)) else get(outcome)]
    dt[, g_sa := fifelse(g == 0L, NEVER_G, as.integer(g))]
    mod <- feols(
      yy ~ sunab(g_sa, decade, ref.p = REF_EVENT) | GEOID + decade,
      data = dt, cluster = ~ GEOID, warn = FALSE
    )
    used <- dt[obs(mod)]
    ip <- iplot(mod, only.params = TRUE)$prms
    est <- data.table(
      spec = spec_label, spec_code = spec_code, outcome = label,
      outcome_variable = outcome,
      e = as.integer(round(ip$x)), estimate = ip$y,
      ci_low = ip$ci_low, ci_high = ip$ci_high
    )[e %in% EVENT_GRID]
    if (!(REF_EVENT %in% est$e)) {
      est <- rbind(est, data.table(
        spec = spec_label, spec_code = spec_code, outcome = label,
        outcome_variable = outcome, e = REF_EVENT,
        estimate = 0, ci_low = 0, ci_high = 0
      ), fill = TRUE)
    }
    est[e == REF_EVENT, `:=`(estimate = 0, ci_low = 0, ci_high = 0)]
    est[, `:=`(timing_mode = timing_mode, geography = geo_spec)]
    setorder(est, e)
    counts <- used[, .(
      n_events = uniqueN(GEOID[g > 0L]),
      n_treated_units = uniqueN(GEOID[g > 0L]),
      n_control_units = uniqueN(GEOID[g == 0L])
    )]
    est[, `:=`(
      n_events = counts$n_events,
      n_treated_units = counts$n_treated_units,
      n_control_units = counts$n_control_units
    )]
    support <- rbindlist(lapply(EVENT_GRID, function(event_time) {
      data.table(
        e = event_time,
        n_events = uniqueN(used[g > 0L & decade == g + event_time, GEOID])
      )
    }))
    support[, `:=`(
      spec = spec_label, outcome = label,
      timing_mode = timing_mode, geography = geo_spec
    )]
    list(estimates = est, counts = counts, support = support)
  })
  counts <- unique(rbindlist(lapply(model_results, `[[`, "counts")))
  if (nrow(counts) != 1L) {
    stop("Sun-Abraham sample counts differ across outcomes for ", spec_label)
  }
  list(
    estimates = rbindlist(lapply(model_results, `[[`, "estimates"), fill = TRUE),
    counts = counts,
    support = rbindlist(lapply(model_results, `[[`, "support"), fill = TRUE)
  )
}

make_sa_panel <- function(d, title) {
  ggplot(d, aes(e, estimate)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_vline(xintercept = -5, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), size = 0.28) +
    scale_x_continuous(breaks = EVENT_GRID) +
    labs(x = "Years relative to treatment decade", y = "Coefficient", title = title) +
    theme_minimal(base_size = 9) +
    theme(plot.title = element_text(size = 9))
}

run_sa <- function(panel, treatment, timing_mode, geo_spec) {
  specs <- list(
    A = "A. high-access vs never-treated",
    B = "B. low-access vs never-treated",
    C = "C. high-access vs low-access"
  )
  fits <- lapply(names(specs), function(code) {
    run_sa_spec(panel, treatment, code, specs[[code]], timing_mode, geo_spec)
  })
  names(fits) <- names(specs)

  source_dir <- file.path(source_root, "sun_abraham", timing_mode, geo_spec)
  delivery_dir <- file.path(delivery_root, "sun_abraham", timing_mode, geo_spec)
  all_est <- rbindlist(lapply(fits, `[[`, "estimates"), fill = TRUE)
  all_support <- rbindlist(lapply(fits, `[[`, "support"), fill = TRUE)
  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  fwrite(all_est, file.path(source_dir, "event_study_estimates_threespec.csv"))
  fwrite(all_support, file.path(source_dir, "event_time_support.csv"))

  outcome_levels <- SA_OUTCOMES$label
  all_est[, outcome := factor(outcome, levels = outcome_levels)]
  combined_plots <- list()
  for (code in names(specs)) {
    d_spec <- all_est[spec_code == code]
    counts <- fits[[code]]$counts
    annotation_data <- data.table(
      outcome = factor(outcome_levels[2L], levels = outcome_levels),
      e = Inf, estimate = Inf, label = sample_label(counts)
    )
    focused <- ggplot(d_spec, aes(e, estimate)) +
      geom_hline(yintercept = 0, color = "grey50") +
      geom_vline(xintercept = -5, linetype = "dashed", color = "grey50") +
      geom_pointrange(aes(ymin = ci_low, ymax = ci_high), size = 0.35) +
      facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
      geom_label(
        data = annotation_data, aes(e, estimate, label = label),
        inherit.aes = FALSE, hjust = 1.05, vjust = 1.05, size = 3,
        label.padding = grid::unit(0.4, "lines"),
        label.r = grid::unit(0, "lines")
      ) +
      scale_x_continuous(breaks = EVENT_GRID) +
      labs(
        x = "Years relative to treatment decade", y = "Coefficient",
        title = specs[[code]],
        subtitle = sprintf("timing: %s | geography: %s",
                           timing_label(timing_mode), geo_spec)
      ) +
      theme_minimal(base_size = 11)
    filename <- sprintf("event_study_%s.png", gsub(
      "[^A-Za-z0-9]+", "_", specs[[code]]
    ))
    save_and_copy(
      focused, file.path(source_dir, filename), file.path(delivery_dir, filename),
      width = 10, height = 7, dpi = 150
    )

    for (oc_label in outcome_levels) {
      p <- make_sa_panel(
        d_spec[as.character(outcome) == oc_label],
        sprintf("%s\n%s", specs[[code]], oc_label)
      )
      if (identical(oc_label, tail(outcome_levels, 1L))) {
        p <- add_sample_annotation(p, counts, size = 2.4)
      }
      combined_plots[[paste(code, oc_label)]] <- p
    }
  }
  combined <- wrap_plots(combined_plots, ncol = 4) +
    plot_annotation(
      title = "Elite high schools — decennial event study, AMWS vs Wikipedia STEM",
      subtitle = sprintf(
        "1800-1960 common sample, +/-50 years; timing: %s; geography: %s",
        timing_label(timing_mode), geo_spec
      )
    )
  save_and_copy(
    combined,
    file.path(source_dir, "event_study_threespec.png"),
    file.path(delivery_dir, "event_study_threespec.png"),
    width = 18, height = 12, dpi = 160
  )
  list(estimates = all_est, support = all_support)
}

all_cs <- list()
all_sa <- list()
figure_manifest <- list()
for (timing_mode in timing_modes) {
  for (geo_spec in geo_specs) {
    cat("\n===", timing_mode, "|", geo_spec, "===\n")
    panel <- build_geo_panel(geo_spec)
    treatment <- build_treatment(geo_spec, timing_mode)
    cat(
      "high-access events:", nrow(treatment$high),
      " low-access counties:", nrow(treatment$low),
      " panel counties:", uniqueN(panel$GEOID), "\n"
    )
    combo <- paste(timing_mode, geo_spec, sep = "__")
    all_cs[[combo]] <- run_cs(panel, treatment, timing_mode, geo_spec)
    all_sa[[combo]] <- run_sa(panel, treatment, timing_mode, geo_spec)

    cs_dir <- file.path(delivery_root, "callaway_santanna", timing_mode, geo_spec)
    sa_dir <- file.path(delivery_root, "sun_abraham", timing_mode, geo_spec)
    figure_manifest[[length(figure_manifest) + 1L]] <- rbindlist(list(
      data.table(
        estimator = "callaway_santanna", timing_mode, geography = geo_spec,
        path = list.files(cs_dir, pattern = "^event_study_.*\\.png$", full.names = TRUE)
      ),
      data.table(
        estimator = "sun_abraham", timing_mode, geography = geo_spec,
        path = list.files(sa_dir, pattern = "^event_study_.*\\.png$", full.names = TRUE)
      )
    ))
  }
}

manifest <- rbindlist(figure_manifest, fill = TRUE)
manifest[, bytes := file.info(path)$size]
if (length(timing_modes) == 2L && length(geo_specs) == 3L && nrow(manifest) != 66L) {
  stop("Expected 66 figures; found ", nrow(manifest))
}
fwrite(manifest, file.path(delivery_root, "figure_manifest.csv"))

cs_est <- rbindlist(lapply(all_cs, `[[`, "estimates"), fill = TRUE)
cs_support <- rbindlist(lapply(all_cs, `[[`, "support"), fill = TRUE)
sa_est <- rbindlist(lapply(all_sa, `[[`, "estimates"), fill = TRUE)
sa_support <- rbindlist(lapply(all_sa, `[[`, "support"), fill = TRUE)
fwrite(cs_est, file.path(source_root, "callaway_santanna_estimates.csv"))
fwrite(cs_support, file.path(source_root, "callaway_santanna_event_support.csv"))
fwrite(sa_est, file.path(source_root, "sun_abraham_estimates.csv"))
fwrite(sa_support, file.path(source_root, "sun_abraham_event_support.csv"))

cat("\nWrote", nrow(manifest), "figures to", delivery_root, "\n")
