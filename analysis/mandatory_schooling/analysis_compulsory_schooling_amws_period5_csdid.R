###############################################################################
# Five-year-bin AMWS births and compulsory-schooling laws: early adopters,
# 1850-1880. Outcomes and denominators are annual averages within calendar
# five-year bins, under both binning conventions:
#
#   standard : 5 * floor(y / 5)                     bin B collects B .. B+4
#   alt      : 5 * floor(y / 5) + 5 * (y %% 5 >= 3) bin B collects B-2 .. B+2
#
# The rule is applied to the outcome, its denominators and the law year alike.
# Estimation reuses the annual C&S primitives with step = 5 and the unchanged
# 14-year exposure allowance, which keeps the reference at event time -15.
# The annual pipeline and its results are untouched.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(did)
})

period5_repo_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    candidate <- normalizePath(sub("^--file=", "", file_arg[1L]), winslash = "/")
    if (basename(candidate) == "analysis_compulsory_schooling_amws_period5_csdid.R") {
      return(normalizePath(file.path(dirname(candidate), "../.."), winslash = "/"))
    }
  }
  candidate <- Sys.getenv("GTL_REPO", unset = getwd())
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (parent == candidate) stop("Cannot resolve repository root; set GTL_REPO.")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
}

repo_root <- period5_repo_root()
source(file.path(repo_root, "prep", "amws", "period5_bins.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "annual_csdid_helpers.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "csdid_figure_helpers.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "annual_csdid_population_controls.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "period5_csdid_helpers.R"))

PERIOD5_RUNNER <- "analysis_compulsory_schooling_amws_period5_csdid.R"
PERIOD5_EXTRA_CODE <- c("prep/amws/period5_bins.R",
                        "prep/amws/build_amws_combined_county_period5.R",
                        "analysis/mandatory_schooling/period5_csdid_helpers.R")

period5_data_root <- function() {
  config <- new.env()
  sys.source(file.path(repo_root, "paths.R"), config)
  root <- config$TALENT_DETS_DATA_DIR
  if (!dir.exists(root) && !nzchar(Sys.getenv("TALENT_DETS_DATA_DIR"))) {
    root <- file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets")
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

period5_grid_length <- function(profile, cadence) {
  (profile$pre + profile$post) %/% cadence$step + 1L
}

# Which specifications this profile runs, and why the other is not reported.
# A two-point linear fit interpolates exactly, so a cohort trend is not
# identified from the two unexposed bins the pre20 window leaves.
period5_specification_status <- function(profile, cadence) {
  both <- c("levels", "detrended")
  requested <- csdid_profile_specifications(profile)
  unexposed <- sum(seq.int(-profile$pre, profile$post, by = cadence$step) <= cadence$ref)
  data.table(
    specification = both,
    status = fifelse(both %in% requested, "estimated", "skipped"),
    reason = fifelse(both %in% requested, "",
                     sprintf("insufficient unexposed bins (%d; three needed for a cohort trend)",
                             unexposed)),
    unexposed_bins_including_reference = unexposed
  )
}

run_period5_profile <- function(inputs, profile_name, results_root, cadence, profiles) {
  profile <- profiles[[profile_name]]
  anticipation <- cadence$anticipation
  step <- cadence$step
  ref <- cadence$ref
  biters <- 1000L
  specifications <- csdid_profile_specifications(profile)
  output_dir <- file.path(results_root, paste0(profile_name, cadence$dir_suffix))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "models"), showWarnings = FALSE)
  dir.create(file.path(output_dir, "per_cohort"), showWarnings = FALSE)
  sample <- build_annual_sample(inputs$panel, profile, anticipation = anticipation, step = step)
  fwrite(sample$audit, file.path(output_dir, "sample_inclusion_audit.csv"))
  fwrite(sample$targets, file.path(output_dir, "treated_counties.csv"))
  fwrite(sample$weights, file.path(output_dir, "cohort_weights.csv"))
  fwrite(inputs$cohort_mapping, file.path(output_dir, "cohort_bin_mapping.csv"))
  fwrite(period5_specification_status(profile, cadence),
         file.path(output_dir, "specification_status.csv"))
  support <- annual_support(sample$panel, sample$weights, sample$grid, anticipation, step)
  fwrite(support, file.path(output_dir, "comparison_support.csv"))
  if (any(!support$status %in% c("supported", "reference"))) {
    stop("Unsupported requested comparison: see ", file.path(output_dir, "comparison_support.csv"))
  }
  stopifnot(all(support$treated_year_counties == support$treated_base_counties))
  # Law bins the horizon cap moved out of the target set, relative to the widest
  # window the design ever uses. They are not discarded: they stay in the panel
  # as not-yet-treated donors, which is what they are.
  capped <- sample$audit[!is.na(g) & g > profile$target_max & g <= period5_widest_target_max(),
                         .(counties = .N, role = paste(sort(unique(role)), collapse = "/")),
                         by = g][order(g)]
  terminal <- support[event_time == profile$post]
  summary <- data.table(
    binning_convention = inputs$convention,
    profile = profile_name, pre_years = profile$pre, post_years = profile$post,
    target_max_cohort = profile$target_max,
    cohorts_excluded_by_cap = if (nrow(capped))
      paste0(capped$g, "(", capped$counties, "co,", capped$role, ")", collapse = ";") else "",
    terminal_control_states = min(terminal$control_year_states),
    terminal_control_counties = min(terminal$control_year_counties),
    step_years = step, anticipation_years = anticipation, reference_event = ref,
    unexposed_pre_bins_including_reference = sum(sample$grid <= ref),
    specifications = paste(specifications, collapse = ";"),
    target_counties = nrow(sample$targets),
    target_jurisdictions = uniqueN(sample$targets$state_id),
    target_states_excluding_dc = uniqueN(sample$targets[state_id != 11L, state_id]),
    donor_counties = uniqueN(sample$panel[target == FALSE, id]),
    estimation_counties = uniqueN(sample$panel$id), estimation_input_rows = nrow(sample$panel),
    supplied_rows = inputs$supplied_totals$rows,
    dropped_partial_bin_rows = inputs$partial_bin_rows,
    window_rows = nrow(sample$window),
    window_amws_births = sum(sample$window$n_amws * sample$window$n_years),
    window_zero_share = mean(sample$window$n_amws == 0),
    min_post_paired_control_states = min(support[event_time == profile$post, control_paired_states])
  )
  fwrite(summary, file.path(output_dir, "sample_summary.csv"))
  cohort_labels <- sample$targets[, .(cohort_label = paste0(g[1L], ": ",
    paste(sort(unique(state_abbr)), collapse = ", "))), by = g]
  pooled_all <- list(); cells_all <- list(); checks_all <- list(); trends_all <- list()
  warning_log <- list()
  combinations <- CJ(outcome_name = names(annual_outcomes), specification = specifications)
  # Keep model order fixed so each run is reproducible, including separate
  # conventions and profiles.
  for (i in seq_len(nrow(combinations))) {
    outcome <- combinations$outcome_name[i]
    spec <- combinations$specification[i]
    model_seed <- cadence$seed_base +
      match(inputs$convention, period5_conventions) * 10000L +
      match(profile_name, names(profiles)) * 100L + i
    set.seed(model_seed)
    started <- Sys.time()
    message(format(started, "%H:%M:%S"), " ", inputs$convention, " / ", profile_name,
            " / ", outcome, " / ", spec)
    model_panel <- copy(sample$panel)
    if (spec == "detrended") {
      detrended <- annual_detrend(model_panel, outcome, profile$pre, anticipation, step)
      model_panel <- detrended$panel
      tr <- detrended$trends
      tr[, `:=`(outcome_name = outcome, specification = spec)]
      trends_all[[length(trends_all) + 1L]] <- tr
    } else {
      model_panel[, model_y := get(outcome)]
    }
    fitted <- fit_annual_csdid(model_panel, anticipation, biters)
    fit <- fitted$fit
    agg_warnings <- character()
    estimates <- withCallingHandlers(
      aggregate_annual_targets(fit, sample$weights, sample$grid, anticipation, biters, step),
      warning = function(w) {
        agg_warnings <<- c(agg_warnings, conditionMessage(w)); invokeRestart("muffleWarning")
      })
    warnings <- unique(c(fitted$warnings, agg_warnings))
    warning_log[[i]] <- data.table(outcome_name = outcome, specification = spec,
                                  warning = if (length(warnings)) warnings else "none")
    stopifnot(all(is.finite(estimates$cells$att)), all(is.finite(estimates$dynamic$att)))
    # Verify the earliest unexposed lead, the first bin inside the anticipation
    # window, the treatment bin, and the long-run bin.
    check_grid <- CJ(g = c(min(sample$weights$g), max(sample$weights$g)),
                     event_time = c(-profile$pre, ref + step, 0L, profile$post))
    check_grid[, manual_att := mapply(function(gg, ee) manual_annual_did(
      model_panel, gg, ee, anticipation, step), g, event_time)]
    check_grid <- merge(check_grid, estimates$cells[, .(g, event_time, att)],
                        by = c("g", "event_time"))
    check_grid[, `:=`(absolute_error = abs(att - manual_att), outcome_name = outcome,
                      specification = spec, validation = "direct_unconditional_did")]
    if (any(check_grid$absolute_error > 1e-7 * pmax(1, abs(check_grid$att)))) {
      fwrite(check_grid, file.path(output_dir, "failed_direct_did_check.csv"))
      stop("C&S estimates disagree with direct unconditional DiD.")
    }
    checks_all[[i]] <- check_grid
    for (kind in c("dynamic", "cells")) {
      d <- estimates[[kind]]
      d[, `:=`(binning_convention = inputs$convention, profile = profile_name,
               outcome_name = outcome, specification = spec,
               specification_label = if (spec == "levels") "Levels" else "Cohort-linear detrended",
               reference_event = ref, anticipation_years = anticipation,
               step_years = step, bootstrap_iterations = biters, seed = model_seed)]
    }
    pooled_all[[i]] <- estimates$dynamic
    cells_all[[i]] <- estimates$cells
    saveRDS(list(fit = fit, aggregation = estimates$aggregation,
                 cohort_weights = sample$weights, weighted_validation = estimates$weighted_validation,
                 seed = model_seed, input_md5 = tools::md5sum(inputs$paths),
                 binning_convention = inputs$convention, warnings = warnings),
            file.path(output_dir, "models", paste0(outcome, "_", spec, ".rds")),
            compress = "gzip")
    # Persist completed specifications even if a later model fails.
    fwrite(rbindlist(pooled_all), file.path(output_dir, "event_study_coefficients.csv"))
    fwrite(rbindlist(cells_all), file.path(output_dir, "per_cohort", "cohort_coefficients.csv"))
    fwrite(rbindlist(checks_all), file.path(output_dir, "direct_did_validation.csv"))
    fwrite(rbindlist(warning_log), file.path(output_dir, "estimation_warnings.csv"))
    if (length(trends_all)) fwrite(rbindlist(trends_all), file.path(output_dir, "detrending_fits.csv"))
    message("Completed in ", round(as.numeric(difftime(Sys.time(), started, units = "secs"))), "s")
    rm(fitted, fit, estimates, model_panel)
    invisible(gc())
  }
  pooled <- rbindlist(pooled_all)
  cells <- merge(rbindlist(cells_all), cohort_labels, by = "g")
  stopifnot(nrow(pooled) == csdid_specification_count(profile) * length(sample$grid),
            nrow(cells) == csdid_specification_count(profile) * length(sample$grid) *
              nrow(sample$weights))
  fwrite(cells, file.path(output_dir, "per_cohort", "cohort_coefficients.csv"))
  figure_counts <- csdid_figure_counts(sample$targets, sample$audit, sample$weights)
  plot_csdid_results(pooled, output_dir, profile$pre, figure_counts, FALSE, NULL,
                     cadence, profile$post)
  plot_csdid_results(cells, file.path(output_dir, "per_cohort"), profile$pre, figure_counts,
                     TRUE, NULL, cadence, profile$post)
  writeLines(c(
    paste("Binning convention:", inputs$convention),
    if (inputs$convention == "standard")
      "Bins are 5 * floor(year / 5); bin B collects years B to B+4."
    else
      "Bins are 5 * floor(year / 5) + 5 * (year %% 5 >= 3); bin B collects years B-2 to B+2.",
    "The same rule bins the outcome, its denominators and the statewide law year.",
    "Outcomes are ANNUAL AVERAGES within a bin. Rates are recomputed from aggregated",
    "numerators and denominators, never averaged. Partial edge bins are dropped.",
    paste("Profile:", profile_name),
    paste0("Post-treatment horizon: +", profile$post, " years (",
           profile$post %/% 5L, " bins after the law bin)."),
    paste0("The last usable comparison bin is ", PERIOD5_LAST_COMPARISON_BIN,
           ": every state had a compulsory law by 1920, and a"),
    "not-yet-treated donor must start at least 15 years after the comparison bin, so the",
    "latest law bin (1920) leaves 1905 as the final bin with any donor at all. A cohort g",
    paste0("can therefore be followed only to event time ", PERIOD5_LAST_COMPARISON_BIN,
           " - g; did truncates there on its own."),
    paste0("Targets are capped at binned law year <= ", profile$target_max,
           " so every retained cohort reaches +", profile$post, "."),
    "A capped cohort is not discarded: it stays in the panel as a not-yet-treated donor.",
    sprintf("The e=+%d cell has the thinnest donor pool of the curve (%d %s, %d counties);",
            profile$post, min(terminal$control_year_states),
            if (min(terminal$control_year_states) == 1L) "state" else "states",
            min(terminal$control_year_counties)),
    "read it with more caution than the rest of the curve.",
    sprintf("Target laws took statewide effect in %d-%d, inclusive, by BINNED law year.",
            if (is.null(profile$target_min)) 1850L else profile$target_min,
            profile$target_max),
    "Treatment dates are from Lingwall (2014), dissertation Table 6.2.",
    "Treatment is the first year a compulsory-attendance law was in effect in every county.",
    "Missing dates are excluded, never converted to never-treated status.",
    "The 14-year exposure allowance is unchanged; did applies it in years, so the",
    "base period is the latest bin before g-14, which on this grid is e=-15, and a",
    "donor law bin must start at least 15 years after the comparison bin.",
    "Event bins e=-10 and e=-5 are inside the anticipation window, not placebo leads.",
    "Treated counties have every requested event bin; controls may have calendar gaps.",
    "Every eligible county keeps its other available bins in estimation.",
    "Only selected early-adopter effects are aggregated, with equal weight per target county.",
    "Joint influence functions include uncertainty in cohort weights.",
    "State-clustered multiplier bootstrap: 1000 draws; 95% pointwise intervals.",
    paste0("Base seed ", cadence$seed_base,
           "; deterministic convention/profile/model offsets are in coefficient exports."),
    paste("Specifications run:", paste(specifications, collapse = ", ")),
    if (identical(specifications, "levels"))
      "Detrended omitted: only two unexposed bins, too few to identify a cohort trend."
    else
      "Detrending uses only e=-pre,...,-15; inference is conditional on estimated trends.",
    "Population is a bin mean of the interpolated annual series; births are annual estimates.",
    paste("Generated:", Sys.time())
  ), file.path(output_dir, "notes.txt"))
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))
  fwrite(data.table(input = names(inputs$paths), path = unname(inputs$paths),
                    md5 = unname(tools::md5sum(inputs$paths))),
         file.path(output_dir, "input_manifest.csv"))
  summary
}

verify_period5_outputs <- function(results_root, profile_names, cadence, profiles, convention) {
  reports <- list()
  for (nm in profile_names) {
    profile <- profiles[[nm]]
    expected_models <- csdid_specification_count(profile)
    n_grid <- period5_grid_length(profile, cadence)
    d <- file.path(results_root, paste0(nm, cadence$dir_suffix))
    pooled <- fread(file.path(d, "event_study_coefficients.csv"))
    cells <- fread(file.path(d, "per_cohort", "cohort_coefficients.csv"))
    support <- fread(file.path(d, "comparison_support.csv"))
    targets <- fread(file.path(d, "treated_counties.csv"))
    audit <- fread(file.path(d, "sample_inclusion_audit.csv"))
    weights <- fread(file.path(d, "cohort_weights.csv"))
    direct <- fread(file.path(d, "direct_did_validation.csv"))
    stopifnot(
      is.na(profile$counties) || nrow(targets) == profile$counties,
      is.na(profile$jurisdictions) || uniqueN(targets$state_id) == profile$jurisdictions,
      nrow(pooled) == expected_models * n_grid,
      nrow(cells) == nrow(pooled) * nrow(weights),
      !anyDuplicated(pooled[, .(outcome_name, specification, event_time)]),
      !anyDuplicated(cells[, .(outcome_name, specification, g, event_time)]),
      all(is.finite(pooled$att)), all(is.finite(pooled$se)),
      all(is.finite(cells$att)), all(is.finite(cells$se)),
      all(pooled[event_time == cadence$ref, att == 0 & se == 0]),
      all(support$status %in% c("supported", "reference")),
      max(direct$absolute_error) < 1e-7,
      all(pooled$specification %in% csdid_profile_specifications(profile)))
    trends_path <- file.path(d, "detrending_fits.csv")
    if ("detrended" %in% csdid_profile_specifications(profile)) {
      trends <- fread(trends_path)
      stopifnot(all(trends$fitting_max_event <= cadence$ref), all(trends$fitting_years >= 3L))
    } else {
      stopifnot(!file.exists(trends_path))
    }
    models <- list.files(file.path(d, "models"), pattern = "[.]rds$", full.names = TRUE)
    stopifnot(length(models) == expected_models)
    metadata <- rbindlist(lapply(models, function(path) {
      m <- readRDS(path)
      ids <- as.integer(rownames(m$fit$inffunc))
      dp <- m$fit$DIDparams
      stopifnot(identical(as.integer(dp$cluster_vector), ids %/% 1000L),
                identical(dp$cluster_vector_var, "state_id"),
                dp$anticipation == cadence$anticipation, dp$base_period == "universal",
                dp$control_group == "notyettreated", dp$bstrap, !dp$cband,
                dp$biters == 1000L, nrow(m$fit$inffunc) == m$fit$n,
                m$binning_convention == convention,
                max(abs(m$weighted_validation$att - m$weighted_validation$manual_att)) < 1e-8)
      data.table(model = basename(path), counties = m$fit$n,
                 state_clusters = uniqueN(dp$cluster_vector),
                 processed_rows = nrow(dp$data),
                 processed_min_year = min(dp$data$year), processed_max_year = max(dp$data$year),
                 seed = m$seed, bootstrap_iterations = dp$biters)
    }))
    fwrite(metadata, file.path(d, "model_metadata.csv"))
    figure_counts <- csdid_figure_counts(targets, audit, weights)
    summary <- fread(file.path(d, "sample_summary.csv"))
    stopifnot(figure_counts$treated_counties == summary$target_counties,
              figure_counts$treated_states == summary$target_jurisdictions,
              figure_counts$control_counties == summary$donor_counties)
    plot_csdid_results(pooled, d, profile$pre, figure_counts, FALSE, NULL, cadence, profile$post)
    plot_csdid_results(cells, file.path(d, "per_cohort"), profile$pre, figure_counts,
                       TRUE, NULL, cadence, profile$post)
    figures <- c(list.files(d, pattern = "[.](png|pdf)$", full.names = TRUE),
                 list.files(file.path(d, "per_cohort"), pattern = "[.](png|pdf)$", full.names = TRUE))
    stopifnot(length(figures) == annual_figure_file_count(), all(file.info(figures)$size > 0))
    reports[[nm]] <- data.table(binning_convention = convention, profile = nm,
      models = nrow(metadata), pooled_rows = nrow(pooled), cohort_rows = nrow(cells),
      target_counties = nrow(targets), target_jurisdictions = uniqueN(targets$state_id),
      max_direct_did_error = max(direct$absolute_error), figures = length(figures),
      validation_passed = TRUE)
  }
  report <- rbindlist(reports)
  # Scoped by horizon: results_root is per convention, so a shared name would
  # leave only whichever horizon ran last.
  fwrite(report, file.path(results_root,
                           paste0("validation_summary", cadence$dir_suffix, ".csv")))
  fwrite(annual_control_code_manifest(PERIOD5_RUNNER, PERIOD5_EXTRA_CODE),
         file.path(results_root, "code_manifest.csv"))
  print(report)
  invisible(report)
}

main <- function() {
  data_root <- period5_data_root()
  binning <- tolower(trimws(Sys.getenv("COMPULSORY_AMWS_PERIOD5_BINNING", unset = "both")))
  if (!binning %in% c("both", period5_conventions)) {
    stop("Binning must be both, standard, or alt.")
  }
  conventions <- if (binning == "both") period5_conventions else binning
  horizon <- tolower(trimws(Sys.getenv("COMPULSORY_AMWS_PERIOD5_HORIZON", unset = "both")))
  if (!horizon %in% c("both", names(period5_horizons))) {
    stop("Horizon must be both, post20, or post30.")
  }
  horizons <- if (horizon == "both") names(period5_horizons) else horizon
  requested <- tolower(trimws(Sys.getenv("COMPULSORY_AMWS_PERIOD5_PROFILE", unset = "both")))
  if (!requested %in% c("both", names(period5_profiles))) {
    stop("Profile must be both, pre20, or pre40.")
  }
  profile_names <- if (requested == "both") names(period5_profiles) else requested
  control <- tolower(trimws(Sys.getenv("COMPULSORY_AMWS_PERIOD5_CONTROL", unset = "none")))
  if (!control %in% c("none", "both", names(annual_population_controls))) {
    stop("Control must be none, both, baseline_log_population, or preperiod_population_cagr.")
  }
  verify_only <- "--verify-outputs" %in% commandArgs(TRUE)
  summaries <- list()
  for (convention in conventions) {
    inputs <- read_period5_inputs(data_root, convention)
    results_root <- file.path(data_root, "results", "mandatory_schooling",
                              "compulsory_schooling_amws_period5_csdid", convention)
    for (hz in horizons) {
      cadence <- period5_cadence(hz)
      profiles <- period5_profiles_for(convention, hz)
      # Validate nesting even when just one profile is requested.
      s20 <- build_annual_sample(inputs$panel, profiles$pre20,
                                 anticipation = cadence$anticipation, step = cadence$step)
      s40 <- build_annual_sample(inputs$panel, profiles$pre40,
                                 anticipation = cadence$anticipation, step = cadence$step)
      stopifnot(all(s40$targets$id %in% s20$targets$id))
      rm(s20, s40)
      if (control != "none") {
        controls <- if (control == "both") names(annual_population_controls) else control
        for (nm in profile_names) for (cv in controls) {
          if (!verify_only) {
            run_annual_population_control(inputs, nm, cv, results_root, cadence, profiles,
                                          seed_base = cadence$seed_base + 1L,
                                          runner = PERIOD5_RUNNER,
                                          extra_code = PERIOD5_EXTRA_CODE)
          }
          verify_annual_population_control(inputs, nm, cv, results_root, cadence, profiles,
                                           runner = PERIOD5_RUNNER,
                                           extra_code = PERIOD5_EXTRA_CODE)
        }
        next
      }
      if (!verify_only) {
        summaries[[paste(convention, hz)]] <- rbindlist(lapply(profile_names, function(nm)
          run_period5_profile(inputs, nm, results_root, cadence, profiles)))
      }
      verify_period5_outputs(results_root, profile_names, cadence, profiles, convention)
    }
  }
  if (length(summaries)) print(rbindlist(summaries))
  message("Results: ", file.path(data_root, "results", "mandatory_schooling",
                                 "compulsory_schooling_amws_period5_csdid"))
}

if (sys.nframe() == 0L) main()
