# Isolated calendar-balanced appendix runs; existing outputs are untouched.
# Rscript analysis/report_companion_calendar_appendices.R [college|facilities|--test]
args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
repo_root <- normalizePath(file.path(dirname(script), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "paths.R"))
suppressPackageStartupMessages(library(data.table))
source(file.path(repo_root, "analysis/scientific_facilities/amws_facilities_annual_helpers.R"))
source(file.path(repo_root, "analysis/scientific_facilities/amws_facilities_decennial_helpers.R"))

calendar_audit <- function(panel, grid, outcomes, positive = character()) {
  p <- as.data.table(copy(panel))
  stopifnot(!anyDuplicated(p[, .(GEOID, decade)]))
  p <- p[decade %in% grid]
  p[, usable := TRUE]
  for (o in outcomes) p[, usable := usable & is.finite(get(o)) &
    if (o %in% positive) get(o) > 0 else get(o) >= 0]
  a <- p[, .(observed_decades = .N, usable_decades = sum(usable),
    missing_or_invalid_decades = paste(setdiff(grid, decade[usable]), collapse = ";")), by = GEOID]
  a[, included := observed_decades == length(grid) & usable_decades == length(grid)]
  a
}

test_calendar <- function() {
  p <- CJ(GEOID = c("00001", "00002", "00003"), decade = seq(1830L, 1850L, 10L))
  p[, `:=`(amws_per_1000_births = 1, county_births_estimate = 10)]
  p <- p[!(GEOID == "00002" & decade == 1840L)]
  p[GEOID == "00003" & decade == 1850L, county_births_estimate := 0]
  a <- calendar_audit(p, seq(1830L, 1850L, 10L),
    c("amws_per_1000_births", "county_births_estimate"), "county_births_estimate")
  stopifnot(identical(a[included == TRUE, GEOID], "00001"))
  annual <- CJ(GEOID = c("00001", "00002"), year = 1860:1949)
  annual[, `:=`(n_amws = 1, population = 100, county_births_estimate_year = 10)]
  annual <- annual[!(GEOID == "00002" & year == 1888)]
  b <- sfd_build_panel(annual)
  a <- calendar_audit(b$panel, seq(1860L, 1940L, 10L),
    c("amws_per_1000_births", "county_births_estimate", "population"),
    c("county_births_estimate", "population"))
  stopifnot(identical(a[included == TRUE, GEOID], "00001"),
    is.na(b$panel[GEOID == "00002" & decade == 1880L, n_amws]))
  assignment <- data.table(GEOID = c("00001", "00002"),
    first_exposure_year = c(1880L, NA_integer_), role = c("target", "unexposed_through_1949"))
  d <- sfd_model_data(b$panel, assignment, "amws_per_1000_births", "standard_decade")
  s <- sfd_settings(); s$pre <- 50L
  f <- sfd_estimate(d, "nevertreated", s, estimate = FALSE)
  stopifnot(f$cells[g == 1880L & event_time == -50L, status] == "outside_calendar_window",
    f$cells[g == 1880L & event_time == -20L, status] == "available")
  cat("Calendar coverage, invalid denominators, missing annual blocks and unsupported leads: passed.\n")
}

new_run <- function(pipeline, family, inputs) {
  tag <- Sys.getenv("REPORT_APPENDIX_RESULTS_TAG",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  stopifnot(grepl("^[A-Za-z0-9_-]+$", tag))
  out <- file.path(TALENT_DETS_DATA_DIR, "results", pipeline, "event_studies", family, tag)
  if (dir.exists(out)) stop("Refusing to overwrite: ", out)
  dir.create(out, recursive = TRUE)
  inputs <- unique(c(inputs, script))
  fwrite(data.table(path = normalizePath(inputs, winslash = "/", mustWork = TRUE),
    md5 = unname(tools::md5sum(inputs))), file.path(out, "input_manifest.csv"))
  writeLines(c("Calendar balance only; no full event-time coverage requirement.",
    "Requested event window -50 to +70 years; unsupported comparisons are not imputed.",
    paste("R:", R.version.string)), file.path(out, "notes.txt"))
  cat("Results directory:", out, "\n"); flush.console()
  out
}

run_college <- function() {
  Sys.setenv(DET_DIR = TALENT_DETS_DATA_DIR)
  source(file.path(repo_root, "prep/raw_paths.R"))
  helper <- file.path(repo_root, "analysis/land_grants/temporal_support_1830_1950/_sample_helpers.R")
  legacy <- file.path(repo_root, "analysis/land_grants/temporal_support_1830_1950/03_run_baseline_population_controls.R")
  source(helper)
  # Import only estimator functions, without executing the original main script.
  for (expr in parse(legacy)) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% c("renormalize_dynamic_at", "run_controlled")) eval(expr)
  }
  min_event_time <- -50L; max_event_time <- 70L; reference_event_time <- 0L
  environment(run_controlled) <- environment()
  environment(renormalize_dynamic_at) <- environment()
  inputs <- read_temporal_support_inputs()
  input_paths <- file.path(DATA_OUTPUT, "land_grants", c(
    "amws_temporal_support_county_decade_1830_1950.csv",
    "amws_temporal_support_county_decade_1830_1950_alt.csv",
    "andrews_event_county_units_1850_1920.csv"))
  out <- new_run("land_grants", "amws_county_pairs_calendar_balanced_m50_p70",
    c(input_paths, helper, legacy))
  panels <- list(standard_decade = inputs$panel, alternative_decade = inputs$panel_alt)
  audits <- lapply(panels, calendar_audit, grid = seq(1830L, 1950L, 10L),
    outcomes = c("amws_per_1000_births", "county_births_estimate"), positive = "county_births_estimate")
  kept <- Reduce(intersect, lapply(audits, function(a) a[included == TRUE, GEOID]))
  for (timing in names(audits)) {
    audits[[timing]][, `:=`(timing = timing, retained_in_both_timings = GEOID %in% kept)]
  }
  fwrite(rbindlist(audits), file.path(out, "calendar_sample_audit.csv"))
  if (!length(kept)) stop("No counties satisfy both calendar definitions.")
  suppressPackageStartupMessages(library(did))
  dynamics <- list(); statuses <- list(); supports <- list(); retained <- list(); cells_all <- list()
  for (timing in names(panels)) {
    p <- panels[[timing]][panels[[timing]]$GEOID %in% kept &
      panels[[timing]]$decade %in% seq(1830L, 1950L, 10L), ]
    stopifnot(all(table(p$GEOID) == 13L))
    original_stack <- build_stack(inputs$units, p,
      if (timing == "standard_decade") "g_std" else "g_shift", timing)
    sample <- prepare_event_study_sample(original_stack, "amws_per_1000_births", "births_baseline")
    stack <- dplyr::filter(original_stack, event_id %in% unique(sample$event_id))
    ua <- as.data.table(copy(inputs$units))
    ua[, `:=`(timing = timing, calendar_county_retained = GEOID %in% kept,
      event_retained = event_id %in% unique(stack$event_id))]
    retained[[timing]] <- ua
    for (outcome in c("amws_per_1000_births", "county_births_estimate")) {
      cat("Estimating college:", timing, outcome, "\n"); flush.console()
      set.seed(20261002L)
      fit <- run_controlled(stack, outcome, "births_baseline", "births_level", timing)
      if (!isTRUE(fit$ok)) stop(fit$error)
      key <- paste(timing, outcome, sep = "__")
      z <- as.data.table(fit$dynamic_renorm)
      z[, `:=`(model = key, outcome = outcome, timing = timing,
        spec = "births_level", control = "births_baseline")]
      dynamics[[key]] <- z
      statuses[[key]] <- data.table(outcome, timing, ok = TRUE,
        n_events = fit$n_events, n_units = fit$n_units, n_rows = fit$n_rows)
      cells <- data.table(g = fit$att$group, calendar_decade = fit$att$t, estimate = fit$att$att)
      cells[, `:=`(event_time = calendar_decade - g, outcome = outcome, timing = timing,
        estimable = is.finite(estimate))]
      cells_all[[key]] <- cells
      data_es <- as.data.table(prepare_event_study_sample(stack, outcome, "births_baseline"))
      supports[[key]] <- rbindlist(lapply(seq(-50L, 70L, 10L), function(e) {
        groups <- cells[event_time == e & estimable == TRUE, unique(g)]
        units <- data_es[g %in% groups & g > 0]
        data.table(outcome, timing, event_time = e, n_cohorts = length(groups),
          cohort_years = paste(sort(groups), collapse = ";"),
          n_treated_units = uniqueN(units$stack_unit_num),
          n_treated_counties = uniqueN(units$GEOID),
          n_events = uniqueN(units$event_id))
      }))
    }
  }
  fwrite(rbindlist(dynamics), file.path(out, "baseline_pop_dynamic_att.csv"))
  fwrite(rbindlist(statuses), file.path(out, "baseline_pop_model_status.csv"))
  fwrite(rbindlist(supports), file.path(out, "event_time_support.csv"))
  fwrite(rbindlist(cells_all), file.path(out, "cohort_time_att.csv"))
  fwrite(rbindlist(retained), file.path(out, "event_sample_audit.csv"))
  writeLines(c(readLines(file.path(out, "notes.txt")),
    paste("Shared calendar-complete counties:", length(kept)),
    "Calendar decade labels 1830 through 1950 under each timing definition; alternative 1960 bin excluded.",
    "Outcome/covariate-complete counties retained jointly for both outcomes and timings.",
    "Selected counties and surviving own runner-ups; never-treated controls; CS-DID regression estimator.",
    "Baseline births at g-10; dynamic estimates renormalized to e=0 using original influence functions.",
    "Pointwise 95% intervals; per-point support counts describe dynamic ATT before renormalization."),
    file.path(out, "notes.txt"))
  writeLines("complete", file.path(out, "status.txt"))
  out
}

run_facilities <- function() {
  source_dir <- file.path(TALENT_DETS_DATA_DIR,
    "results/scientific_facilities/event_studies/amws_scientific_facilities_decennial_pre1944_end1949/decennial_20261002")
  panel_path <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
  assignment_path <- file.path(source_dir, "treatment_assignment.csv")
  out <- new_run("scientific_facilities", "amws_facilities_calendar_balanced_m50_p70",
    c(panel_path, assignment_path, file.path(repo_root, "analysis/scientific_facilities",
      c("amws_facilities_annual_helpers.R", "amws_facilities_decennial_helpers.R"))))
  s <- sfd_settings(); s$pre <- 50L
  annual <- sfa_read_panel(panel_path, s)
  b <- sfd_build_panel(annual)
  a <- calendar_audit(b$panel, seq(1860L, 1940L, 10L),
    c("amws_per_1000_births", "county_births_estimate", "population"),
    c("county_births_estimate", "population"))
  assignment <- fread(assignment_path, colClasses = c(GEOID = "character"))
  a[, matched_geography := GEOID %in% assignment$GEOID]
  a[, retained := included & matched_geography]
  fwrite(a, file.path(out, "calendar_sample_audit.csv"))
  fwrite(b$audit, file.path(out, "block_completeness.csv"))
  kept <- a[retained == TRUE, GEOID]
  if (!length(kept)) stop("No facilities counties satisfy complete calendar coverage.")
  p <- b$panel[GEOID %in% kept]
  assignment <- assignment[GEOID %in% kept]
  stopifnot(all(p$annual_rows == 10L), all(p$count_complete & p$births_complete & p$population_complete),
    all(table(p$GEOID) == 9L))
  fwrite(p, file.path(out, "county_decade_outcomes.csv"))
  fwrite(assignment, file.path(out, "treatment_assignment.csv"))
  suppressPackageStartupMessages(library(did))
  exports <- list(dynamic_att = list(), post_att_summary = list(), support_estimation = list(),
    cohort_time_att = list(), county_contributions = list(), aggregation_validation = list())
  timings <- c("standard_decade", "alternative_decade")
  for (ti in seq_along(timings)) {
    timing <- timings[ti]
    cat("Estimating facilities:", timing, "\n"); flush.console()
    # Same bootstrap seed convention as original outcome index 4, control index 1.
    s$seed <- sfd_settings()$seed + 4L * 100L + ti * 20L + 10L
    d <- sfd_model_data(p, assignment, "amws_per_1000_births", timing)
    fit <- sfd_estimate(d, "nevertreated", s)
    if (any(fit$cells$status == "failed_estimation")) {
      fwrite(fit$cells, file.path(out, paste0("failed_cells_", timing, ".csv")))
      stop("Failed facilities comparisons; no replacement estimator used.")
    }
    fit$grid <- fit$grid[fit$grid <= 70L]
    result <- sfd_aggregate(fit, d, "unbalanced", s)
    pieces <- list(dynamic_att = result$dynamic, post_att_summary = result$summaries,
      support_estimation = fit$cells, cohort_time_att = result$cells,
      county_contributions = result$annotation$participants,
      aggregation_validation = result$dynamic[, .(event_time, estimate, manual_att,
        difference = estimate - manual_att)])
    for (n in names(pieces)) {
      z <- pieces[[n]]
      z[, `:=`(outcome = "amws_per_1000_births", timing = timing,
        control_group = "nevertreated", balance = "calendar_balanced_event_unbalanced")]
      exports[[n]][[timing]] <- z
    }
  }
  for (n in names(exports)) fwrite(rbindlist(exports[[n]], fill = TRUE), file.path(out, paste0(n, ".csv")))
  writeLines(c(readLines(file.path(out, "notes.txt")),
    paste("Calendar-complete counties with matched geography:", length(kept)),
    "Complete annual inputs in all nine calendar decades 1860-1869 through 1940-1949.",
    "Existing saved 50km assignment; openings through 1943; never-treated unexposed through 1949.",
    "Standard and shifted treatment timing; common calendar-decade outcomes; reference e=-10.",
    "Original two-period DR estimator and cohort-size aggregation; 1000 common county multiplier draws.",
    "Shifted references may contain exposed years, as in the original specification."),
    file.path(out, "notes.txt"))
  writeLines("complete", file.path(out, "status.txt"))
  out
}

if (sys.nframe() == 0L) {
  cli <- commandArgs(TRUE)
  test_calendar()
  if (!"--test" %in% cli) {
    targets <- if (length(cli)) cli else c("college", "facilities")
    stopifnot(all(targets %in% c("college", "facilities")))
    for (target in targets) {
      path <- if (target == "college") run_college() else run_facilities()
      cat("COMPLETE", target, path, "\n")
    }
  }
}
