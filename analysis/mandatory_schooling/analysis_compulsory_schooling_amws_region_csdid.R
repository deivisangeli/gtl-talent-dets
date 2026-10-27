###############################################################################
# AMWS births and compulsory-schooling laws: the county C&S event studies of
# project/mandatory_schooling.tex, re-estimated with region x birth-year
# effects. Regions are the CLS South (region 3) and everything else; since every
# 1850-1880 target is non-Southern, each target is compared with non-Southern
# not-yet-treated donors only (see region_csdid_helpers.R for why that is the
# C&S region x time estimator).
#
# Target sets, profiles, anticipation, reference period, controls and seeds'
# structure are those of the published annual and five-year (post30) runs; the
# donor restriction is the only change. Non-Southern donors run out around
# 1890, so late cohorts are identified only to e = last_comparison - g and the
# pooled curves are ragged. The published runs and their results are untouched.
#
#   COMPULSORY_AMWS_REGION_CADENCE  annual | period5 | both (default)
#   COMPULSORY_AMWS_REGION_PROFILE  pre20 | pre40 | both (default)
#   COMPULSORY_AMWS_REGION_BINNING  standard | alt | both (default; period5 only)
#   COMPULSORY_AMWS_REGION_CONTROL  none | baseline_log_population |
#                                   preperiod_population_cagr | all (default)
#   --verify-outputs  re-check completed runs and redraw figures only
#   --resume          skip models already saved with the same inputs and code
#
# Run from anywhere:
#   Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws_region_csdid.R
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(did)
})

region_repo_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    candidate <- normalizePath(sub("^--file=", "", file_arg[1L]), winslash = "/")
    if (basename(candidate) == "analysis_compulsory_schooling_amws_region_csdid.R") {
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

repo_root <- region_repo_root()
source(file.path(repo_root, "prep", "amws", "period5_bins.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "annual_csdid_helpers.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "csdid_figure_helpers.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "annual_csdid_population_controls.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "period5_csdid_helpers.R"))
source(file.path(repo_root, "analysis", "mandatory_schooling", "region_csdid_helpers.R"))

REGION_RUNNER <- "analysis_compulsory_schooling_amws_region_csdid.R"
REGION_RESULTS_DIR <- "compulsory_schooling_amws_region_csdid"
REGION_SEED_BASE <- 20260925L
# The volume reports the five-year design at the +30 horizon only.
REGION_PERIOD5_HORIZON <- "post30"
REGION_CONTROLS <- c("none", names(annual_population_controls))
REGION_BITERS <- 1000L

region_data_root <- function() {
  config <- new.env()
  sys.source(file.path(repo_root, "paths.R"), config)
  root <- config$TALENT_DETS_DATA_DIR
  if (!dir.exists(root) && !nzchar(Sys.getenv("TALENT_DETS_DATA_DIR"))) {
    root <- file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets")
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

region_code_manifest <- function() {
  paths <- file.path(repo_root, c(
    file.path("analysis", "mandatory_schooling", c(
      REGION_RUNNER, "region_csdid_helpers.R", "annual_csdid_helpers.R",
      "csdid_figure_helpers.R", "annual_csdid_population_controls.R",
      "period5_csdid_helpers.R", "test_region_csdid.R")),
    file.path("prep", "amws", "period5_bins.R")))
  data.table(file = basename(paths), md5 = unname(tools::md5sum(paths)))
}

# One block per estimation grid: the annual panel, or one five-year convention.
region_block <- function(data_root, cadence_name, convention = NA_character_) {
  results <- file.path(data_root, "results", "mandatory_schooling", REGION_RESULTS_DIR)
  if (cadence_name == "annual") {
    list(name = "annual", convention = NA_character_,
         inputs = read_annual_inputs(data_root),
         cadence = csdid_cadence(label = "Annual C&S, region x birth year",
                                 seed_base = REGION_SEED_BASE),
         profiles = annual_profiles, root = file.path(results, "annual"),
         targets_text = "Targets: laws adopted in 1850-1880.")
  } else {
    cadence <- period5_cadence(REGION_PERIOD5_HORIZON)
    cadence$label <- "5-year-bin C&S, region x birth year"
    cadence$seed_base <- REGION_SEED_BASE + 100000L +
      match(convention, period5_conventions) * 10000L
    profiles <- period5_profiles_for(convention, REGION_PERIOD5_HORIZON)
    list(name = "period5", convention = convention,
         inputs = read_period5_inputs(data_root, convention),
         cadence = cadence, profiles = profiles,
         root = file.path(results, "period5", convention),
         targets_text = sprintf("Targets: binned law years 1850-%d.",
                                profiles[[1L]]$target_max))
  }
}

region_output_dir <- function(block, profile_name, control) {
  base <- file.path(block$root, paste0(profile_name, block$cadence$dir_suffix))
  if (control == "none") base else file.path(base, control)
}

region_block_label <- function(block) {
  if (is.na(block$convention)) block$name else paste(block$name, block$convention)
}

region_figure_label <- function(control, pre) {
  if (control == "none") REGION_LABEL else
    paste(REGION_LABEL, "|", annual_control_label(control, pre))
}

# Everything that defines a run's sample, shared by the estimation and the
# verification passes so the two cannot drift apart.
region_design <- function(block, profile_name) {
  cadence <- block$cadence
  profile <- block$profiles[[profile_name]]
  full <- build_annual_sample(block$inputs$panel, profile,
                              anticipation = cadence$anticipation, step = cadence$step)
  sample <- region_restrict_sample(full)
  last <- region_last_comparison(sample$panel, cadence$anticipation)
  support <- region_support(sample$panel, sample$weights, sample$grid,
                            cadence$anticipation, cadence$step, last)
  list(profile = profile, full = full, sample = sample, last = last, support = support)
}

region_check_events <- function(profile, cadence) {
  unique(c(-profile$pre, cadence$ref + cadence$step, 0L, profile$post))
}

run_region_spec <- function(block, profile_name, control) {
  cadence <- block$cadence
  ref <- cadence$ref; step <- cadence$step; anticipation <- cadence$anticipation
  design <- region_design(block, profile_name)
  profile <- design$profile; sample <- design$sample; last <- design$last
  output <- region_output_dir(block, profile_name, control)
  dir.create(file.path(output, "models"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output, "per_cohort"), showWarnings = FALSE)
  fwrite(sample$audit, file.path(output, "sample_inclusion_audit.csv"))
  fwrite(sample$targets, file.path(output, "treated_counties.csv"))
  fwrite(sample$weights, file.path(output, "cohort_weights.csv"))
  fwrite(region_assignment(sample$audit), file.path(output, "region_assignment.csv"))
  fwrite(region_donor_counts(sample$panel, anticipation), file.path(output, "region_donor_states.csv"))
  fwrite(design$support, file.path(output, "comparison_support.csv"))
  region_check_support(design$support, file.path(output, "comparison_support.csv"))
  if (!is.na(block$convention)) {
    fwrite(block$inputs$cohort_mapping, file.path(output, "cohort_bin_mapping.csv"))
  }
  audit <- NULL
  if (control != "none") {
    audit <- annual_covariate_audit(block$inputs$panel, sample, profile$pre, control, cadence)
    csupport <- annual_control_support(sample, audit, cadence)
    csupport[year > last & status != "reference", status := "beyond_region_donor_horizon"]
    fwrite(audit, file.path(output, "covariate_audit.csv"))
    fwrite(csupport, file.path(output, "control_comparison_support.csv"))
    if (any(csupport[status == "supported", !control_regression_full_rank])) {
      stop("Rank-deficient covariate regression inside the donor horizon; see ",
           file.path(output, "control_comparison_support.csv"))
    }
  }
  units <- unique(sample$panel[, .(id, g, state_id)])
  setorder(units, id)
  specifications <- csdid_profile_specifications(profile)
  combinations <- CJ(outcome_name = names(annual_outcomes), specification = specifications)
  input_md5 <- tools::md5sum(block$inputs$paths)
  code_manifest <- region_code_manifest()
  for (i in seq_len(nrow(combinations))) {
    outcome <- combinations$outcome_name[i]; spec <- combinations$specification[i]
    model_path <- file.path(output, "models", paste0(outcome, "_", spec, ".rds"))
    model_seed <- cadence$seed_base + match(profile_name, names(block$profiles)) * 1000L +
      match(control, REGION_CONTROLS) * 100L + i
    if ("--resume" %in% commandArgs(TRUE) && file.exists(model_path)) {
      existing <- readRDS(model_path)
      if (identical(existing$input_md5, input_md5) && existing$seed == model_seed &&
          identical(existing$last_comparison, last) &&
          identical(as.data.frame(existing$code_manifest), as.data.frame(code_manifest))) {
        message("Resume: verified completed model ", basename(model_path))
        next
      }
    }
    started <- Sys.time()
    message(format(started, "%H:%M:%S"), " ", region_block_label(block), " / ", profile_name,
            " / ", control, " / ", outcome, " / ", spec)
    p <- copy(sample$panel); trends <- NULL
    if (spec == "detrended") {
      tr <- annual_detrend(p, outcome, profile$pre, anticipation, step)
      p <- tr$panel; trends <- tr$trends
    } else p[, model_y := get(outcome)]
    # Nothing after the donor horizon is reported, so did never sees it: this
    # keeps one-donor-state cells (and their thin covariate fits) out entirely.
    # Each 2x2 uses only its base and comparison years, so in-horizon cells are
    # unchanged by the cut.
    p <- p[year <= last]
    events <- region_check_events(profile, cadence)
    if (control == "none") {
      set.seed(model_seed)
      fitted <- fit_annual_csdid(p, anticipation, REGION_BITERS)
      agg_warnings <- character()
      estimates <- withCallingHandlers(
        aggregate_annual_targets(fitted$fit, sample$weights, sample$grid, anticipation,
                                 REGION_BITERS, step, ragged = TRUE, max_year = last),
        warning = function(w) {
          agg_warnings <<- c(agg_warnings, conditionMessage(w)); invokeRestart("muffleWarning")
        })
      cells <- region_mark_beyond(copy(estimates$cells), last)
      dynamic <- estimates$dynamic
      checks <- CJ(g = range(sample$weights$g), event_time = events)[g + event_time <= last]
      checks[, manual_att := mapply(function(gg, ee) manual_annual_did(p, gg, ee, anticipation, step),
                                    g, event_time)]
      checks <- merge(checks, cells[, .(g, event_time, att)], by = c("g", "event_time"))
      checks[, `:=`(absolute_error = abs(att - manual_att), validation = "direct_unconditional_did")]
      if (any(checks$absolute_error > 1e-7 * pmax(1, abs(checks$att)))) {
        fwrite(checks, file.path(output, "failed_direct_did_check.csv"))
        stop("C&S estimates disagree with direct unconditional DiD on the restricted pool.")
      }
      model <- list(kind = "unadjusted", fit = fitted$fit, aggregation = estimates$aggregation,
                    weighted_validation = estimates$weighted_validation,
                    warnings = unique(c(fitted$warnings, agg_warnings)))
    } else {
      fits <- list(); checks <- list(); warnings <- character()
      for (gg in sample$weights$g) {
        cp <- annual_control_panel(p, audit, gg)
        fitted <- fit_annual_csdid(cp, anticipation = anticipation, xformla = ~covariate,
                                   est_method = "reg", bstrap = FALSE)
        compact <- region_compact_cohort_fit(fitted$fit, gg, sample$grid, fitted$warnings, last)
        if (!identical(is.finite(compact$att), gg + sample$grid <= last)) {
          fwrite(data.table(g = gg, event_time = sample$grid, att = compact$att),
                 file.path(output, "failed_cohort_cells.csv"))
          stop("Controlled cohort ", gg, " is not estimated exactly inside the donor horizon.")
        }
        ev <- unique(c(events, ref))
        ev <- ev[gg + ev <= last]
        ck <- data.table(g = gg, event_time = ev)
        ck[, manual_att := vapply(event_time, function(ee) manual_annual_reg_did(cp, gg, ee, cadence),
                                  numeric(1L))]
        ck[, att := compact$att[match(event_time, sample$grid)]]
        ck[, `:=`(absolute_error = abs(att - manual_att), validation = "direct_outcome_regression_did")]
        stopifnot(all(ck$absolute_error < 1e-7))
        checks[[as.character(gg)]] <- ck
        fits[[as.character(gg)]] <- compact
        warnings <- c(warnings, fitted$warnings)
        message("  cohort ", gg, ": ", compact$n, " counties; ",
                sum(is.finite(compact$att)), " of ", length(sample$grid), " cells in horizon")
        rm(fitted, cp); invisible(gc(FALSE))
      }
      aggregate <- region_aggregate_controlled(fits, units, sample$weights, sample$grid,
                                               model_seed, REGION_BITERS, ref)
      # Copy: the stored aggregation must stay exactly what re-aggregation gives.
      cells <- region_mark_beyond(copy(aggregate$cells), last)
      dynamic <- copy(aggregate$dynamic)
      checks <- rbindlist(checks)
      model <- list(kind = "controlled", cohort_fits = fits, aggregation = aggregate,
                    units = units, warnings = unique(warnings))
    }
    if (!all(is.finite(dynamic$att)) || !all(is.finite(dynamic$se))) {
      stop("A pooled event time has no cohort inside the donor horizon (last comparison ",
           last, "); shorten the grid rather than report it.")
    }
    checks[, `:=`(outcome_name = outcome, specification = spec)]
    model <- c(model, list(dynamic = dynamic, cells = cells, checks = checks, trends = trends,
      cohort_weights = sample$weights, outcome_name = outcome, specification = spec,
      profile = profile_name, control_specification = control, seed = model_seed,
      last_comparison = last, input_md5 = input_md5, code_manifest = code_manifest))
    saveRDS(model, model_path, compress = "gzip")
    message("Completed in ", round(as.numeric(difftime(Sys.time(), started, units = "secs"))), "s")
    rm(model); invisible(gc(FALSE))
  }
  terminal <- design$support[event_time == profile$post & status == "supported"]
  summary <- data.table(
    block = region_block_label(block), profile = profile_name, control_specification = control,
    region = sample$region, last_comparison_period = last,
    min_donor_states = REGION_MIN_DONOR_STATES,
    target_counties = nrow(sample$targets), target_jurisdictions = uniqueN(sample$targets$state_id),
    events = nrow(sample$weights),
    cohorts_reaching_post = sum(sample$weights$g + profile$post <= last),
    donor_counties = uniqueN(sample$panel[target == FALSE, id]),
    donor_states = uniqueN(sample$panel[target == FALSE, state_id]),
    excluded_southern_donor_counties = sample$audit[role == "excluded_other_region", .N],
    excluded_southern_donor_states = uniqueN(sample$audit[role == "excluded_other_region", state_fips]),
    terminal_min_control_states = if (nrow(terminal)) min(terminal$control_year_states) else NA_integer_,
    step_years = step, anticipation_years = anticipation, reference_event = ref,
    specifications = paste(specifications, collapse = ";"))
  fwrite(summary, file.path(output, "sample_summary.csv"))
  fwrite(data.table(input = names(block$inputs$paths), path = unname(block$inputs$paths),
                    md5 = unname(input_md5)), file.path(output, "input_manifest.csv"))
  fwrite(code_manifest, file.path(output, "code_manifest.csv"))
  writeLines(capture.output(sessionInfo()), file.path(output, "session_info.txt"))
  writeLines(c(
    paste("Block:", region_block_label(block), "| Profile:", profile_name,
          "| Control:", control),
    "Region x birth-year effects: regions are the CLS South (region 3: AL AR FL GA LA MS NC",
    "SC TN TX VA) and everything else. C&S region x time effects are parallel trends",
    "conditional on region; every target is non-Southern (CLS code DC in the Northeast),",
    "so each target is compared with NON-SOUTHERN not-yet-treated donors only. Unadjusted,",
    "this is exactly region x year fixed effects; with a population covariate the outcome",
    "regression is fitted within the non-Southern pool (region-specific slope).",
    sprintf(paste("Donor horizon: %d is the last period with at least %d non-Southern donor",
                  "states; nothing later is estimated (the panel is cut there)."),
            last, REGION_MIN_DONOR_STATES),
    "A cohort g is identified only to e = last - g, so pooled points are RAGGED: each averages",
    "the cohorts identified there, weights renormalised by target counties (n_cohorts and",
    "n_units in event_study_coefficients.csv).",
    sprintf("%d of %d target cohorts reach e = +%d.",
            sum(sample$weights$g + profile$post <= last), nrow(sample$weights), profile$post),
    "Target sets, anticipation, reference period and controls are those of the published run.",
    "Treatment dates are from Lingwall (2014), dissertation Table 6.2.",
    "Treatment is the first year a compulsory-attendance law was in effect in every county.",
    sprintf("Reference %d; anticipation %d years; %s.", ref, anticipation, cadence$exposure_note),
    if (control == "none") "C&S not-yet-treated comparisons, universal base period." else
      paste(annual_control_label(control, profile$pre),
            "(fixed focal-cohort covariate, fitted per focal cohort)."),
    "Detrending uses only e = -pre..-15; inference is conditional on fitted trends.",
    "State-clustered multiplier bootstrap: 1000 draws; 95% pointwise intervals.",
    "Joint county influence functions include cohort-share uncertainty.",
    paste("Generated:", Sys.time())), file.path(output, "notes.txt"))
  invisible(summary)
}

verify_region_spec <- function(block, profile_name, control) {
  cadence <- block$cadence
  ref <- cadence$ref
  design <- region_design(block, profile_name)
  profile <- design$profile; sample <- design$sample; last <- design$last
  output <- region_output_dir(block, profile_name, control)
  support <- fread(file.path(output, "comparison_support.csv"))
  stopifnot(all(support$status %in% region_allowed_support))
  paths <- list.files(file.path(output, "models"), pattern = "[.]rds$", full.names = TRUE)
  stopifnot(length(paths) == csdid_specification_count(profile))
  pooled <- list(); cells <- list(); checks <- list(); trends <- list(); metadata <- list()
  warnings <- list()
  input_md5 <- tools::md5sum(block$inputs$paths)
  southern_ids <- sample$audit[role == "excluded_other_region", id]
  for (i in seq_along(paths)) {
    m <- readRDS(paths[i])
    m$dynamic <- copy(m$dynamic); m$cells <- copy(m$cells); m$checks <- copy(m$checks)
    stopifnot(identical(m$input_md5, input_md5), m$last_comparison == last,
              m$profile == profile_name, m$control_specification == control,
              identical(as.data.frame(m$cohort_weights), as.data.frame(sample$weights)))
    if (control == "none") {
      dp <- m$fit$DIDparams
      ids <- as.integer(rownames(m$fit$inffunc))
      stopifnot(m$kind == "unadjusted",
        identical(as.integer(dp$cluster_vector), sample$panel$state_id[match(ids, sample$panel$id)]),
        dp$anticipation == cadence$anticipation, dp$base_period == "universal",
        dp$control_group == "notyettreated", dp$biters == REGION_BITERS,
        max(dp$data$year) <= last, !any(dp$data$id %in% southern_ids),
        max(abs(m$weighted_validation$att - m$weighted_validation$manual_att), na.rm = TRUE) < 1e-8)
      metadata[[i]] <- data.table(model = basename(paths[i]), counties = m$fit$n,
        state_clusters = uniqueN(dp$cluster_vector), processed_rows = nrow(dp$data),
        processed_max_year = max(dp$data$year), seed = m$seed)
    } else {
      agg <- region_aggregate_controlled(m$cohort_fits, m$units, m$cohort_weights,
                                         sample$grid, m$seed, REGION_BITERS, ref)
      stopifnot(m$kind == "controlled", isTRUE(all.equal(agg, m$aggregation, tolerance = 1e-12)))
      for (f in m$cohort_fits) {
        stopifnot(f$metadata$est_method == "reg", f$metadata$max_year <= last,
                  f$metadata$control_group == "notyettreated",
                  f$metadata$base_period == "universal")
      }
      metadata[[i]] <- rbindlist(lapply(m$cohort_fits, function(f) data.table(
        model = basename(paths[i]), g = f$g, counties = f$n,
        state_clusters = uniqueN(f$clusters), processed_rows = f$metadata$processed_rows,
        processed_max_year = f$metadata$max_year, seed = m$seed)))
    }
    label <- if (m$specification == "levels") "Levels" else "Cohort-linear detrended"
    for (d in list(m$dynamic, m$cells)) {
      d[, `:=`(block = region_block_label(block), profile = profile_name,
               outcome_name = m$outcome_name, specification = m$specification,
               specification_label = label, control_specification = control,
               region_design = "cls_south_excluded", last_comparison_period = last,
               reference_event = ref, anticipation_years = cadence$anticipation,
               step_years = cadence$step, bootstrap_iterations = REGION_BITERS, seed = m$seed)]
    }
    stopifnot(all(is.finite(m$dynamic$att)), all(is.finite(m$dynamic$se)),
              all(m$dynamic[event_time == ref, att == 0 & se == 0]),
              identical(is.finite(m$cells$att), m$cells$year <= last),
              all(is.finite(m$cells[year <= last, se])),
              max(m$checks$absolute_error) < 1e-7)
    pooled[[i]] <- m$dynamic; cells[[i]] <- m$cells; checks[[i]] <- m$checks
    if (!is.null(m$trends)) {
      stopifnot(all(m$trends$fitting_max_event <= ref))
      trends[[i]] <- copy(m$trends)[, `:=`(outcome_name = m$outcome_name,
                                           specification = m$specification)]
    }
    warnings[[i]] <- data.table(model = basename(paths[i]),
                                warning = if (length(m$warnings)) m$warnings else "none")
    rm(m); invisible(gc(FALSE))
  }
  pooled <- rbindlist(pooled); cells <- rbindlist(cells); checks <- rbindlist(checks)
  labels <- sample$targets[, .(cohort_label = paste0(g[1L], ": ",
    paste(sort(unique(state_abbr)), collapse = ", "))), by = g]
  cells <- merge(cells, labels, by = "g")
  n_models <- csdid_specification_count(profile)
  stopifnot(nrow(pooled) == n_models * length(sample$grid),
            nrow(cells) == nrow(pooled) * nrow(sample$weights),
            !anyDuplicated(pooled[, .(outcome_name, specification, event_time)]),
            !anyDuplicated(cells[, .(outcome_name, specification, g, event_time)]))
  # Composition depends on the calendar alone, so it must agree across models.
  composition <- unique(pooled[, .(event_time, n_cohorts, n_units)])
  stopifnot(!anyDuplicated(composition$event_time))
  fwrite(pooled, file.path(output, "event_study_coefficients.csv"))
  fwrite(cells, file.path(output, "per_cohort", "cohort_coefficients.csv"))
  fwrite(checks, file.path(output, "direct_did_validation.csv"))
  if (length(trends)) fwrite(rbindlist(trends), file.path(output, "detrending_fits.csv"))
  fwrite(rbindlist(metadata, fill = TRUE), file.path(output, "model_metadata.csv"))
  fwrite(rbindlist(warnings), file.path(output, "estimation_warnings.csv"))
  fwrite(composition, file.path(output, "pooled_composition.csv"))
  if (control == "none") {
    counts <- csdid_figure_counts(sample$targets, sample$audit, sample$weights)
  } else {
    audit <- fread(file.path(output, "covariate_audit.csv"))
    pool <- unique(audit[eligible & !target, .(id, state_fips)])
    stopifnot(!any(pool$id %in% sample$audit[role == "excluded_other_region", id]))
    counts <- list(treated_counties = nrow(sample$targets),
                   treated_states = uniqueN(sample$targets$state_id),
                   control_counties = nrow(pool), control_states = uniqueN(pool$state_fips),
                   events = nrow(sample$weights))
  }
  note <- region_ragged_note(composition, profile$post, last)
  label <- region_figure_label(control, profile$pre)
  plot_csdid_results(pooled, output, profile$pre, counts, FALSE, label, cadence, profile$post,
                     targets_text = block$targets_text, note = note)
  plot_csdid_results(cells, file.path(output, "per_cohort"), profile$pre, counts, TRUE, label,
                     cadence, profile$post, targets_text = block$targets_text, note = note)
  figures <- c(list.files(output, pattern = "[.](png|pdf)$", full.names = TRUE),
               list.files(file.path(output, "per_cohort"), pattern = "[.](png|pdf)$",
                          full.names = TRUE))
  stopifnot(length(figures) == annual_figure_file_count(), all(file.info(figures)$size > 0))
  report <- data.table(block = region_block_label(block), profile = profile_name,
    control_specification = control, last_comparison_period = last, models = length(paths),
    pooled_rows = nrow(pooled), cohort_rows = nrow(cells),
    target_counties = counts$treated_counties, target_jurisdictions = counts$treated_states,
    control_pool_counties = counts$control_counties, control_pool_states = counts$control_states,
    units_at_e0 = composition[event_time == 0L, n_units],
    units_at_post = composition[event_time == profile$post, n_units],
    cohorts_at_post = composition[event_time == profile$post, n_cohorts],
    max_direct_did_error = max(checks$absolute_error), figures = length(figures),
    validation_passed = TRUE)
  fwrite(report, file.path(output, "validation_summary.csv"))
  fwrite(region_code_manifest(), file.path(output, "verification_code_manifest.csv"))
  report
}

region_env_choice <- function(name, allowed, all_value) {
  value <- tolower(trimws(Sys.getenv(name, unset = all_value)))
  if (!value %in% c(all_value, allowed)) {
    stop(name, " must be one of: ", paste(c(all_value, allowed), collapse = ", "))
  }
  if (value == all_value) allowed else value
}

main <- function() {
  data_root <- region_data_root()
  cadences <- region_env_choice("COMPULSORY_AMWS_REGION_CADENCE", c("annual", "period5"), "both")
  profile_names <- region_env_choice("COMPULSORY_AMWS_REGION_PROFILE", c("pre20", "pre40"), "both")
  conventions <- region_env_choice("COMPULSORY_AMWS_REGION_BINNING", period5_conventions, "both")
  controls <- region_env_choice("COMPULSORY_AMWS_REGION_CONTROL", REGION_CONTROLS, "all")
  verify_only <- "--verify-outputs" %in% commandArgs(TRUE)
  reports <- list()
  for (cadence_name in cadences) {
    for (convention in if (cadence_name == "annual") NA_character_ else conventions) {
      block <- region_block(data_root, cadence_name, convention)
      for (nm in profile_names) for (cv in controls) {
        if (!verify_only) run_region_spec(block, nm, cv)
        reports[[length(reports) + 1L]] <- verify_region_spec(block, nm, cv)
        print(reports[[length(reports)]])
      }
      rm(block); invisible(gc())
    }
  }
  print(rbindlist(reports))
  message("Results: ", file.path(data_root, "results", "mandatory_schooling", REGION_RESULTS_DIR))
}

if (sys.nframe() == 0L) main()
