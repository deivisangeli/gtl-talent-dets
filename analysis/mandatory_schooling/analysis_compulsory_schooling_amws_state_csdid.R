###############################################################################
# State-level AMWS births and compulsory-schooling laws: early adopters,
# 1850-1880. The companion of the county C&S runners, with the state as the
# unit: counts and denominators are summed over every valid county-period and
# rates recomputed from the sums. Both cadences are run:
#
#   annual  : pre20 / pre40, through +20           (as the annual county runner)
#   period5 : standard / alt x post20 / post30     (as the five-year county runner)
#
# Estimation is identical to the county designs -- not-yet-treated donors,
# anticipation 14, universal base at e = -15, 1000-draw multiplier bootstrap
# clustered on state -- except that states are unweighted. County runners and
# their results are untouched.
#
# Two target windows (state_csdid_helpers.R): `early`, the 1850-1880 laws on a
# balanced event grid, and `all_events`, every adopter through 1905 on a RAGGED
# grid -- nothing after 1905 has a not-yet-treated donor, so a cohort g is
# identified only to e = 1905 - g and the pooled curve's composition changes.
#
# Env vars: COMPULSORY_AMWS_STATE_WINDOW (early|all_events|both; default early),
#           COMPULSORY_AMWS_STATE_CADENCE (all|annual|period5),
#           COMPULSORY_AMWS_STATE_PROFILE (both|pre20|pre40),
#           COMPULSORY_AMWS_STATE_BINNING (both|standard|alt),
#           COMPULSORY_AMWS_STATE_HORIZON (both|post20|post30).
# Flag: --verify-outputs re-checks and re-plots finished runs without refitting.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(did)
})

STATE_RUNNER <- "analysis_compulsory_schooling_amws_state_csdid.R"

state_repo_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    candidate <- normalizePath(sub("^--file=", "", file_arg[1L]), winslash = "/")
    if (basename(candidate) == STATE_RUNNER) {
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

repo_root <- state_repo_root()
source(file.path(repo_root, "prep", "amws", "period5_bins.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "annual_csdid_helpers.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "csdid_figure_helpers.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "period5_csdid_helpers.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "state_csdid_helpers.R"))

state_data_root <- function() {
  config <- new.env()
  sys.source(file.path(repo_root, "paths.R"), config)
  root <- config$TALENT_DETS_DATA_DIR
  if (!dir.exists(root) && !nzchar(Sys.getenv("TALENT_DETS_DATA_DIR"))) {
    root <- file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets")
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

state_grid_length <- function(profile, cadence) {
  (profile$pre + profile$post) %/% cadence$step + 1L
}

# The cohort-weight column is n_counties inside the shared helpers; on disk it
# is named for what it counts here.
read_state_weights <- function(path) {
  weights <- fread(path)
  setnames(weights, "n_states", "n_counties")
  weights
}

run_state_profile <- function(inputs, profile_name, profiles, cadence, output_dir,
                              window = "early") {
  profile <- profiles[[profile_name]]
  anticipation <- cadence$anticipation
  step <- cadence$step
  ref <- cadence$ref
  biters <- 1000L
  ragged <- state_windows[[window]]$ragged
  # Only the ragged window has a donor horizon; the balanced one reports every
  # grid cell it targets, however thin, exactly as the county design does.
  last_comparison <- if (ragged) state_last_comparison(inputs$panel, anticipation) else Inf
  target_min <- if (is.null(profile$target_min)) 1850L else profile$target_min
  target_max <- if (is.null(profile$target_max)) 1880L else profile$target_max
  period5 <- !is.na(inputs$convention)
  specifications <- csdid_profile_specifications(profile)
  dir.create(file.path(output_dir, "models"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "per_cohort"), showWarnings = FALSE)
  sample <- build_annual_sample(inputs$panel, profile, check_expected = FALSE,
                                anticipation = anticipation, step = step)
  if (!is.na(profile$jurisdictions) && nrow(sample$targets) != profile$jurisdictions) {
    stop(sprintf("Expected %d target states for %s, found %d.", profile$jurisdictions,
                 profile_name, nrow(sample$targets)))
  }
  fwrite(sample$audit, file.path(output_dir, "sample_inclusion_audit.csv"))
  fwrite(sample$targets, file.path(output_dir, "treated_states.csv"))
  fwrite(setnames(copy(sample$weights), "n_counties", "n_states"),
         file.path(output_dir, "cohort_weights.csv"))
  coverage <- state_coverage_audit(sample$panel, sample$targets, sample$grid, ref)
  fwrite(coverage, file.path(output_dir, "state_coverage.csv"))
  if (period5) {
    fwrite(inputs$cohort_mapping, file.path(output_dir, "cohort_bin_mapping.csv"))
    fwrite(period5_specification_status(profile, cadence),
           file.path(output_dir, "specification_status.csv"))
  }
  support <- state_support(sample$panel, sample$weights, sample$grid, anticipation, step,
                           last_comparison, ragged)
  fwrite(support, file.path(output_dir, "comparison_support.csv"))
  if (any(!support$status %in% state_allowed_support)) {
    stop("Unsupported requested comparison: see ", file.path(output_dir, "comparison_support.csv"))
  }
  stopifnot(all(support$treated_year_counties == support$treated_base_counties),
            !ragged || all(support[status == "supported", control_year_states] >=
                             STATE_MIN_DONOR_STATES))
  # The terminal event's donors, among the cohorts identified that far.
  terminal <- support[event_time == profile$post & status == "supported"]
  post_supported <- support[event_time >= 0L & status == "supported"]
  horizon <- support[status %in% c("supported", "reference"), .(max_e = max(event_time)), by = g]
  summary <- data.table(
    window = window,
    cadence = if (period5) "period5" else "annual",
    binning_convention = inputs$convention,
    profile = profile_name, pre_years = profile$pre, post_years = profile$post,
    target_max_cohort = target_max,
    last_comparison_period = if (ragged) last_comparison else NA_integer_,
    max_identified_event_by_cohort = paste0(horizon$g, ":", horizon$max_e, collapse = ";"),
    step_years = step, anticipation_years = anticipation, reference_event = ref,
    unexposed_pre_periods_including_reference = sum(sample$grid <= ref),
    specifications = paste(specifications, collapse = ";"),
    target_states = nrow(sample$targets),
    target_states_excluding_dc = uniqueN(sample$targets[state_id != 11L, state_id]),
    target_state_abbr = paste(sort(sample$targets$state_abbr), collapse = ";"),
    county_design_target_jurisdictions = profile$county_design_jurisdictions,
    donor_states = uniqueN(sample$panel[target == FALSE, id]),
    estimation_states = uniqueN(sample$panel$id), estimation_input_rows = nrow(sample$panel),
    window_rows = nrow(sample$window),
    window_amws_births = sum(sample$window$n_amws * sample$window$n_years),
    window_zero_share = mean(sample$window$n_amws == 0),
    target_states_with_composition_change = sum(coverage$composition_changes > 0L),
    terminal_control_states = min(terminal$control_year_states),
    min_post_paired_control_states = min(terminal$control_paired_states),
    min_post_control_states = min(post_supported$control_year_states),
    post_cells_le2_donor_states = sum(post_supported$control_year_states <= 2L)
  )
  fwrite(summary, file.path(output_dir, "sample_summary.csv"))
  cohort_labels <- sample$targets[, .(cohort_label = paste0(g[1L], ": ",
    paste(sort(unique(state_abbr)), collapse = ", "))), by = g]
  pooled_all <- list(); cells_all <- list(); checks_all <- list(); trends_all <- list()
  warning_log <- list()
  combinations <- CJ(outcome_name = names(state_outcomes), specification = specifications)
  # Keep model order fixed so each run is reproducible, including separate
  # cadences, conventions and profiles.
  for (i in seq_len(nrow(combinations))) {
    outcome <- combinations$outcome_name[i]
    spec <- combinations$specification[i]
    model_seed <- cadence$seed_base +
      (if (period5) match(inputs$convention, period5_conventions) * 10000L else 0L) +
      match(profile_name, names(profiles)) * 100L + i
    set.seed(model_seed)
    started <- Sys.time()
    message(format(started, "%H:%M:%S"), " ", cadence$label, " / ",
            if (period5) paste0(inputs$convention, " / ") else "", profile_name,
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
      aggregate_annual_targets(fit, sample$weights, sample$grid, anticipation, biters, step,
                               ragged = ragged, max_year = last_comparison),
      warning = function(w) {
        agg_warnings <<- c(agg_warnings, conditionMessage(w)); invokeRestart("muffleWarning")
      })
    warnings <- unique(c(fitted$warnings, agg_warnings))
    warning_log[[i]] <- data.table(outcome_name = outcome, specification = spec,
                                  warning = if (length(warnings)) warnings else "none")
    state_mark_beyond(estimates$cells, last_comparison)
    stopifnot(all(is.finite(estimates$dynamic$att)))
    # Earliest unexposed lead, first period inside the anticipation window, the
    # treatment period and the long-run period, for the first and last cohorts,
    # wherever a donor exists.
    check_grid <- CJ(g = c(min(sample$weights$g), max(sample$weights$g)),
                     event_time = c(-profile$pre, ref + step, 0L, profile$post))
    check_grid <- check_grid[g + event_time <= last_comparison]
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
      d[, `:=`(unit = "state", binning_convention = inputs$convention, profile = profile_name,
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
  }
  pooled <- rbindlist(pooled_all)
  cells <- merge(rbindlist(cells_all), cohort_labels, by = "g")
  n_models <- csdid_specification_count(profile, state_outcomes)
  stopifnot(nrow(pooled) == n_models * length(sample$grid),
            nrow(cells) == n_models * length(sample$grid) * nrow(sample$weights))
  fwrite(cells, file.path(output_dir, "per_cohort", "cohort_coefficients.csv"))
  plot_state_figures(pooled, cells, output_dir, profile, cadence, ragged,
                     csdid_figure_counts(sample$targets, sample$audit, sample$weights),
                     target_min, target_max, last_comparison)
  writeLines(c(
    if (ragged) c(
      sprintf("WINDOW: ALL EVENTS TO %d (ragged). Every state had a law by 1920 and a",
              last_comparison),
      "not-yet-treated donor must adopt at least 15 years after the comparison period.",
      sprintf("Periods after %d have fewer than %d donor states (one donor gives no standard",
              last_comparison, STATE_MIN_DONOR_STATES),
      sprintf("error), so a cohort g is identified only to e = %d - g.", last_comparison),
      "Each pooled point averages the cohorts identified there, weights renormalised",
      "over them; n_cohorts and n_units in event_study_coefficients.csv give the",
      "composition. Cohort cells past the horizon are status beyond_donor_horizon.",
      "Late cohorts' post-law cells are identified off the 1906-1920 adopters, almost",
      "all Southern, so they inherit the region collinearity of Southern adoption.",
      sprintf("The thinnest post-law cell has %d donor state(s); %d post cells have <= 2.",
              summary$min_post_control_states, summary$post_cells_le2_donor_states)
    ) else "WINDOW: EARLY (balanced event grid).",
    "UNIT: STATE. AMWS births, population and estimated births are summed over every",
    "valid county-period of the county panel; rates are recomputed from the sums.",
    "A state's aggregate covers whichever of its counties exist in a period, so",
    "composition grows as territories organise; state_coverage.csv reports it for",
    "each target window, and nothing is filtered on it.",
    if (period5) c(
      paste("Binning convention:", inputs$convention),
      if (inputs$convention == "standard")
        "Bins are 5 * floor(year / 5); bin B collects years B to B+4."
      else
        "Bins are 5 * floor(year / 5) + 5 * (year %% 5 >= 3); bin B collects years B-2 to B+2.",
      "Values are ANNUAL AVERAGES within a bin; only counties with a complete bin enter it.",
      paste0("Post-treatment horizon: +", profile$post, "; the last usable comparison bin is ",
             PERIOD5_LAST_COMPARISON_BIN, ". Capped cohorts stay in as not-yet-treated donors.")
    ) else "Annual outcomes; population is interpolated annually, births are annual estimates.",
    paste("Profile:", profile_name),
    sprintf("Target laws took statewide effect in %d-%d, inclusive%s.",
            target_min, target_max, if (period5) ", by BINNED law year" else ""),
    "Treatment dates are from Lingwall (2014), dissertation Table 6.2.",
    "Treatment is the first year a compulsory-attendance law was in effect in every county.",
    "Missing dates are excluded, never converted to never-treated status.",
    sprintf("14-year exposure allowance; reference is e=%d. Periods inside the", ref),
    "anticipation window are not placebo leads.",
    "Target states have every requested event period; donors may have calendar gaps.",
    "C&S not-yet-treated comparisons, anticipation=14, universal base period.",
    "States are UNWEIGHTED: equal weight per state in att_gt and in the cohort aggregation.",
    "Joint influence functions include uncertainty in cohort weights.",
    "Multiplier bootstrap clustered on state (= unit): 1000 draws; 95% pointwise intervals.",
    sprintf("Few clusters: %d target states; a single-state cohort's cells rest on one cluster.",
            nrow(sample$targets)),
    sprintf("The e=+%d period has %d donor state(s) at its thinnest.",
            profile$post, min(terminal$control_year_states)),
    paste0("Base seed ", cadence$seed_base, "; deterministic offsets are in coefficient exports."),
    paste("Specifications run:", paste(specifications, collapse = ", ")),
    if (identical(specifications, "levels"))
      "Detrended omitted: only two unexposed bins, too few to identify a cohort trend."
    else
      sprintf("Detrending uses only e=-pre,...,%d; inference is conditional on estimated trends.", ref),
    paste("Generated:", Sys.time())
  ), file.path(output_dir, "notes.txt"))
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))
  fwrite(data.table(input = names(inputs$paths), path = unname(inputs$paths),
                    md5 = unname(tools::md5sum(inputs$paths))),
         file.path(output_dir, "input_manifest.csv"))
  summary
}

plot_state_figures <- function(pooled, cells, output_dir, profile, cadence, ragged, counts,
                               target_min, target_max, last_comparison) {
  targets_text <- sprintf("Targets: laws adopted in %d-%d.", target_min, target_max)
  note <- if (ragged) state_ragged_note(
    pooled[outcome_name == outcome_name[1L] & specification == specification[1L]],
    profile$post, last_comparison)
  plot_csdid_results(pooled, output_dir, profile$pre, counts, FALSE, NULL, cadence,
                     profile$post, state_outcomes, "state", targets_text, note)
  plot_csdid_results(cells, file.path(output_dir, "per_cohort"), profile$pre, counts, TRUE,
                     NULL, cadence, profile$post, state_outcomes, "state", targets_text, note)
}

verify_state_outputs <- function(results_root, profile_names, profiles, cadence, convention,
                                 window = "early") {
  ragged <- state_windows[[window]]$ragged
  reports <- list()
  for (nm in profile_names) {
    profile <- profiles[[nm]]
    expected_models <- csdid_specification_count(profile, state_outcomes)
    n_grid <- state_grid_length(profile, cadence)
    d <- file.path(results_root, paste0(nm, cadence$dir_suffix))
    pooled <- fread(file.path(d, "event_study_coefficients.csv"))
    cells <- fread(file.path(d, "per_cohort", "cohort_coefficients.csv"))
    support <- fread(file.path(d, "comparison_support.csv"))
    targets <- fread(file.path(d, "treated_states.csv"))
    audit <- fread(file.path(d, "sample_inclusion_audit.csv"))
    weights <- read_state_weights(file.path(d, "cohort_weights.csv"))
    direct <- fread(file.path(d, "direct_did_validation.csv"))
    stopifnot(
      is.na(profile$jurisdictions) || nrow(targets) == profile$jurisdictions,
      !anyDuplicated(targets$id), all(targets$id == targets$state_id),
      nrow(pooled) == expected_models * n_grid,
      nrow(cells) == nrow(pooled) * nrow(weights),
      !anyDuplicated(pooled[, .(outcome_name, specification, event_time)]),
      !anyDuplicated(cells[, .(outcome_name, specification, g, event_time)]),
      all(is.finite(pooled$att)), all(is.finite(pooled$se)),
      all(is.finite(cells[status != "beyond_donor_horizon", att])),
      all(is.finite(cells[status != "beyond_donor_horizon", se])),
      all(is.na(cells[status == "beyond_donor_horizon", att])),
      ragged || !any(cells$status == "beyond_donor_horizon"),
      all(pooled[event_time == cadence$ref, att == 0 & se == 0]),
      all(support$status %in% state_allowed_support),
      max(direct$absolute_error) < 1e-7,
      all(pooled$specification %in% csdid_profile_specifications(profile)))
    if (ragged) {
      # Cohorts only ever drop out of the pooled curve after the law.
      stopifnot(all(pooled[event_time >= 0L,
                           .(ok = all(diff(n_cohorts[order(event_time)]) <= 0L)),
                           by = .(outcome_name, specification)]$ok),
                all(pooled[event_time <= 0L, n_units] == nrow(targets)))
    }
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
      # The unit is the state, so each unit is its own cluster.
      stopifnot(identical(as.integer(dp$cluster_vector), ids),
                identical(dp$cluster_vector_var, "state_id"),
                dp$anticipation == cadence$anticipation, dp$base_period == "universal",
                dp$control_group == "notyettreated", dp$bstrap, !dp$cband,
                dp$biters == 1000L, nrow(m$fit$inffunc) == m$fit$n,
                identical(m$binning_convention, convention),
                max(abs(m$weighted_validation$att - m$weighted_validation$manual_att)) < 1e-8)
      data.table(model = basename(path), states = m$fit$n,
                 state_clusters = uniqueN(dp$cluster_vector),
                 processed_rows = nrow(dp$data),
                 processed_min_year = min(dp$data$year), processed_max_year = max(dp$data$year),
                 seed = m$seed, bootstrap_iterations = dp$biters, unique_warnings = length(m$warnings))
    }))
    fwrite(metadata, file.path(d, "model_metadata.csv"))
    figure_counts <- csdid_figure_counts(targets, audit, weights)
    summary <- fread(file.path(d, "sample_summary.csv"))
    stopifnot(figure_counts$treated_states == summary$target_states,
              figure_counts$control_states == summary$donor_states)
    plot_state_figures(pooled, cells, d, profile, cadence, ragged, figure_counts,
                       if (is.null(profile$target_min)) 1850L else profile$target_min,
                       summary$target_max_cohort, summary$last_comparison_period)
    figures <- c(list.files(d, pattern = "[.](png|pdf)$", full.names = TRUE),
                 list.files(file.path(d, "per_cohort"), pattern = "[.](png|pdf)$", full.names = TRUE))
    stopifnot(length(figures) == state_figure_file_count(), all(file.info(figures)$size > 0))
    reports[[nm]] <- data.table(window = window, binning_convention = convention, profile = nm,
      models = nrow(metadata), pooled_rows = nrow(pooled), cohort_rows = nrow(cells),
      target_states = nrow(targets), donor_states = summary$donor_states,
      max_direct_did_error = max(direct$absolute_error), figures = length(figures),
      validation_passed = TRUE)
  }
  report <- rbindlist(reports)
  fwrite(report, file.path(results_root, paste0("validation_summary", cadence$dir_suffix, ".csv")))
  fwrite(state_code_manifest(STATE_RUNNER), file.path(results_root, "code_manifest.csv"))
  print(report)
  invisible(report)
}

# Run (unless verify-only) and verify one cadence/convention/horizon block.
run_state_block <- function(inputs, profiles, profile_names, cadence, results_root, verify_only,
                            window) {
  # Validate nesting even when just one profile is requested.
  s20 <- build_annual_sample(inputs$panel, profiles$pre20, check_expected = FALSE,
                             anticipation = cadence$anticipation, step = cadence$step)
  s40 <- build_annual_sample(inputs$panel, profiles$pre40, check_expected = FALSE,
                             anticipation = cadence$anticipation, step = cadence$step)
  stopifnot(all(s40$targets$id %in% s20$targets$id))
  summaries <- NULL
  if (!verify_only) {
    dir.create(results_root, recursive = TRUE, showWarnings = FALSE)
    fwrite(inputs$panel, file.path(results_root, "state_panel.csv"))
    fwrite(inputs$dropped, file.path(results_root, "dropped_county_periods.csv"))
    summaries <- rbindlist(lapply(profile_names, function(nm) run_state_profile(
      inputs, nm, profiles, cadence, file.path(results_root, paste0(nm, cadence$dir_suffix)),
      window)))
  }
  verify_state_outputs(results_root, profile_names, profiles, cadence, inputs$convention, window)
  summaries
}

choose <- function(var, allowed, default = "both") {
  value <- tolower(trimws(Sys.getenv(var, unset = default)))
  if (!value %in% c(default, allowed)) {
    stop(var, " must be one of: ", paste(c(default, allowed), collapse = ", "))
  }
  if (value == default) allowed else value
}

main <- function() {
  data_root <- state_data_root()
  window <- tolower(trimws(Sys.getenv("COMPULSORY_AMWS_STATE_WINDOW", unset = "early")))
  if (!window %in% c("both", names(state_windows))) {
    stop("COMPULSORY_AMWS_STATE_WINDOW must be one of: early, all_events, both")
  }
  windows <- if (window == "both") names(state_windows) else window
  cadences <- choose("COMPULSORY_AMWS_STATE_CADENCE", c("annual", "period5"), "all")
  profile_names <- choose("COMPULSORY_AMWS_STATE_PROFILE", names(annual_profiles))
  conventions <- choose("COMPULSORY_AMWS_STATE_BINNING", period5_conventions)
  horizons <- choose("COMPULSORY_AMWS_STATE_HORIZON", names(period5_horizons))
  verify_only <- "--verify-outputs" %in% commandArgs(TRUE)
  results_base <- file.path(data_root, "results", "mandatory_schooling")
  window_root <- function(w) file.path(results_base, state_windows[[w]]$results_dir)
  summaries <- list()
  if ("annual" %in% cadences) {
    inputs <- read_state_annual_inputs(data_root)
    print(inputs$input_totals)
    for (w in windows) {
      summaries[[paste(w, "annual")]] <- run_state_block(
        inputs, state_annual_profiles(w, state_last_comparison(inputs$panel)), profile_names,
        state_annual_cadence(w),
        file.path(window_root(w), "annual"), verify_only, w)
    }
  }
  if ("period5" %in% cadences) {
    for (convention in conventions) {
      inputs <- read_state_period5_inputs(data_root, convention)
      print(inputs$input_totals)
      for (w in windows) for (hz in horizons) {
        summaries[[paste(w, convention, hz)]] <- run_state_block(
          inputs, state_period5_profiles(convention, hz, w, state_last_comparison(inputs$panel)),
          profile_names,
          state_period5_cadence(hz, w), file.path(window_root(w), "period5", convention),
          verify_only, w)
      }
    }
  }
  if (length(summaries)) print(rbindlist(summaries, fill = TRUE))
  message("Results: ", paste(vapply(windows, window_root, character(1L)), collapse = ", "))
}

if (sys.nframe() == 0L) main()
