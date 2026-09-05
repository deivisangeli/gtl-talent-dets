###############################################################################
# Annual AMWS births and compulsory-schooling laws: early adopters, 1850-1880.
# Default: both pre20 and pre40 profiles, through +20, anticipation = 14 years.
# Existing decennial analyses and results are preserved.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(did)
})

annual_repo_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    candidate <- normalizePath(sub("^--file=", "", file_arg[1L]), winslash = "/")
    if (basename(candidate) == "analysis_compulsory_schooling_amws_annual_csdid.R") {
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

repo_root <- annual_repo_root()
source(file.path(repo_root, "analysis", "mandatory_schooling", "annual_csdid_helpers.R"))

annual_data_root <- function() {
  config <- new.env()
  sys.source(file.path(repo_root, "paths.R"), config)
  root <- config$TALENT_DETS_DATA_DIR
  if (!dir.exists(root) && !nzchar(Sys.getenv("TALENT_DETS_DATA_DIR"))) {
    root <- file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets")
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

plot_annual_results <- function(coefficients, output_dir, pre, per_cohort = FALSE) {
  for (outcome in names(annual_outcomes)) {
    d <- coefficients[outcome_name == outcome]
    d[, specification_label := factor(specification_label,
      levels = c("Levels", "Cohort-linear detrended"))]
    p <- ggplot(d, aes(event_time, att)) +
      annotate("rect", xmin = -14.5, xmax = -0.5, ymin = -Inf, ymax = Inf,
               fill = "#f0c674", alpha = 0.22) +
      geom_hline(yintercept = 0, color = "grey45", linetype = "dashed") +
      geom_vline(xintercept = c(-15, 0), color = "grey50", linetype = "dotted") +
      geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.35,
                    color = "#315a7d", linewidth = 0.35, na.rm = TRUE) +
      geom_point(size = 1.15, color = "#173e60", na.rm = TRUE) +
      scale_x_continuous(breaks = sort(unique(c(seq(-pre, 20, 10), -15))),
                         limits = c(-pre - 0.7, 20.7)) +
      labs(title = "Compulsory schooling and AMWS scientist births",
           subtitle = paste0(annual_outcomes[[outcome]], " | Annual C&S | ",
                             pre, " pre-law years | Reference: -15"),
           x = "Birth year relative to law passage", y = "Estimated effect",
           caption = paste0("Shaded: potentially exposed birth cohorts (-14 to -1). ",
                            "95% pointwise intervals; state-clustered multiplier bootstrap.\n",
                            "Targets: laws adopted in 1850-1880. Trends use only unexposed years; ",
                            "detrended inference is conditional on fitted trends.")) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(), plot.caption = element_text(hjust = 0),
            strip.text = element_text(face = "bold"))
    if (per_cohort) {
      p <- p + facet_grid(cohort_label ~ specification_label, scales = "free_y")
      height <- 2.05 * uniqueN(d$g) + 2
    } else {
      p <- p + facet_wrap(~specification_label, ncol = 2)
      height <- 6
    }
    stem <- file.path(output_dir, paste0(if (per_cohort) "cohort_" else "pooled_", outcome))
    ggsave(paste0(stem, ".png"), p, width = 13, height = height, dpi = 160,
           limitsize = FALSE, bg = "white")
    ggsave(paste0(stem, ".pdf"), p, width = 13, height = height, limitsize = FALSE)
  }
}

run_annual_profile <- function(inputs, profile_name, results_root) {
  profile <- annual_profiles[[profile_name]]
  anticipation <- 14L
  biters <- 1000L
  output_dir <- file.path(results_root, paste0(profile_name, "_post20_a14"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "models"), showWarnings = FALSE)
  dir.create(file.path(output_dir, "per_cohort"), showWarnings = FALSE)
  sample <- build_annual_sample(inputs$panel, profile)
  fwrite(sample$audit, file.path(output_dir, "sample_inclusion_audit.csv"))
  fwrite(sample$targets, file.path(output_dir, "treated_counties.csv"))
  fwrite(sample$weights, file.path(output_dir, "cohort_weights.csv"))
  support <- annual_support(sample$panel, sample$weights, sample$grid, anticipation)
  fwrite(support, file.path(output_dir, "comparison_support.csv"))
  if (any(!support$status %in% c("supported", "reference"))) {
    stop("Unsupported requested comparison: see ", file.path(output_dir, "comparison_support.csv"))
  }
  stopifnot(all(support$treated_year_counties == support$treated_base_counties))
  summary <- data.table(
    profile = profile_name, pre_years = profile$pre, post_years = profile$post,
    anticipation_years = anticipation, reference_event = -15L,
    unexposed_pre_years_including_reference = profile$pre - anticipation,
    target_counties = nrow(sample$targets),
    target_jurisdictions = uniqueN(sample$targets$state_id),
    target_states_excluding_dc = uniqueN(sample$targets[state_id != 11L, state_id]),
    donor_counties = uniqueN(sample$panel[target == FALSE, id]),
    estimation_counties = uniqueN(sample$panel$id), estimation_input_rows = nrow(sample$panel),
    window_rows = nrow(sample$window), window_amws_births = sum(sample$window$n_amws),
    window_zero_share = mean(sample$window$n_amws == 0),
    min_post20_paired_control_states = min(support[event_time == 20L, control_paired_states])
  )
  fwrite(summary, file.path(output_dir, "sample_summary.csv"))
  cohort_labels <- sample$targets[, .(cohort_label = paste0(g[1L], ": ",
    paste(sort(unique(state_abbr)), collapse = ", "))), by = g]
  pooled_all <- list(); cells_all <- list(); checks_all <- list(); trends_all <- list()
  warning_log <- list()
  combinations <- CJ(outcome_name = names(annual_outcomes), specification = c("levels", "detrended"))
  # Keep model order fixed so each run is reproducible, including separate profiles.
  for (i in seq_len(nrow(combinations))) {
    outcome <- combinations$outcome_name[i]
    spec <- combinations$specification[i]
    model_seed <- 20260905L + match(profile_name, names(annual_profiles)) * 100L + i
    set.seed(model_seed)
    started <- Sys.time()
    message(format(started, "%H:%M:%S"), " ", profile_name, " / ", outcome, " / ", spec)
    model_panel <- copy(sample$panel)
    if (spec == "detrended") {
      detrended <- annual_detrend(model_panel, outcome, profile$pre, anticipation)
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
      aggregate_annual_targets(fit, sample$weights, sample$grid, anticipation, biters),
      warning = function(w) {
        agg_warnings <<- c(agg_warnings, conditionMessage(w)); invokeRestart("muffleWarning")
      })
    warnings <- unique(c(fitted$warnings, agg_warnings))
    warning_log[[i]] <- data.table(outcome_name = outcome, specification = spec,
                                  warning = if (length(warnings)) warnings else "none")
    # All target cells must retain their original support and finite estimates.
    stopifnot(all(is.finite(estimates$cells$att)), all(is.finite(estimates$dynamic$att)))
    # Verify unexposed leads, the potentially exposed interval, and long-run cells.
    check_grid <- CJ(g = c(min(sample$weights$g), max(sample$weights$g)),
                     event_time = c(-profile$pre, -14L, 0L, 20L))
    check_grid[, manual_att := mapply(function(gg, ee) manual_annual_did(
      model_panel, gg, ee, anticipation), g, event_time)]
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
      d[, `:=`(profile = profile_name, outcome_name = outcome, specification = spec,
               specification_label = if (spec == "levels") "Levels" else "Cohort-linear detrended",
               reference_event = -15L, anticipation_years = anticipation,
               bootstrap_iterations = biters, seed = model_seed)]
    }
    pooled_all[[i]] <- estimates$dynamic
    cells_all[[i]] <- estimates$cells
    saveRDS(list(fit = fit, aggregation = estimates$aggregation,
                 cohort_weights = sample$weights, weighted_validation = estimates$weighted_validation,
                 seed = model_seed, input_md5 = tools::md5sum(inputs$paths),
                 warnings = warnings),
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
  stopifnot(nrow(pooled) == 8L * length(sample$grid),
            nrow(cells) == 8L * length(sample$grid) * nrow(sample$weights))
  fwrite(cells, file.path(output_dir, "per_cohort", "cohort_coefficients.csv"))
  plot_annual_results(pooled, output_dir, profile$pre)
  plot_annual_results(cells, file.path(output_dir, "per_cohort"), profile$pre, TRUE)
  writeLines(c(
    paste("Profile:", profile_name),
    "Annual AMWS birth cohorts; target laws adopted in 1850-1880, inclusive.",
    "Exact law dates are provisional inputs, not independently verified historical facts.",
    "Missing dates are excluded, never converted to never-treated status.",
    "14-year exposure allowance is an explicit assumption; reference is e=-15.",
    "Birth cohorts e=-14,...,-1 may already be exposed and are not placebo leads.",
    "Treated counties have every requested event year; controls may have calendar gaps.",
    "All eligible counties' other available years are retained in estimation.",
    "C&S not-yet-treated comparisons use anticipation=14 and universal base periods.",
    "Only selected early-adopter effects are aggregated, with equal weight per target county.",
    "Joint influence functions include uncertainty in cohort weights.",
    "State-clustered multiplier bootstrap: 1000 draws; 95% pointwise intervals.",
    "Base seed 20260905; deterministic profile/model offsets are in coefficient exports.",
    "Detrending uses only e=-pre,...,-15; inference is conditional on estimated trends.",
    "Annual county population is interpolated; annual county births use estimated denominators.",
    paste("Generated:", Sys.time())
  ), file.path(output_dir, "notes.txt"))
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))
  fwrite(data.table(input = names(inputs$paths), path = unname(inputs$paths),
                    md5 = unname(tools::md5sum(inputs$paths))), file.path(output_dir, "input_manifest.csv"))
  summary
}

verify_annual_outputs <- function(results_root, profiles) {
  reports <- list()
  for (nm in profiles) {
    profile <- annual_profiles[[nm]]
    d <- file.path(results_root, paste0(nm, "_post20_a14"))
    pooled <- fread(file.path(d, "event_study_coefficients.csv"))
    cells <- fread(file.path(d, "per_cohort", "cohort_coefficients.csv"))
    support <- fread(file.path(d, "comparison_support.csv"))
    targets <- fread(file.path(d, "treated_counties.csv"))
    weights <- fread(file.path(d, "cohort_weights.csv"))
    direct <- fread(file.path(d, "direct_did_validation.csv"))
    trends <- fread(file.path(d, "detrending_fits.csv"))
    stopifnot(nrow(targets) == profile$counties,
      uniqueN(targets$state_id) == profile$jurisdictions,
      nrow(pooled) == 8L * (profile$pre + 21L),
      nrow(cells) == nrow(pooled) * nrow(weights),
      !anyDuplicated(pooled[, .(outcome_name, specification, event_time)]),
      !anyDuplicated(cells[, .(outcome_name, specification, g, event_time)]),
      all(is.finite(pooled$att)), all(is.finite(pooled$se)),
      all(is.finite(cells$att)), all(is.finite(cells$se)),
      all(pooled[event_time == -15L, att == 0 & se == 0]),
      all(support$status %in% c("supported", "reference")),
      all(trends$fitting_max_event <= -15L),
      max(direct$absolute_error) < 1e-7)
    models <- list.files(file.path(d, "models"), pattern = "[.]rds$", full.names = TRUE)
    stopifnot(length(models) == 8L)
    metadata <- rbindlist(lapply(models, function(path) {
      m <- readRDS(path)
      ids <- as.integer(rownames(m$fit$inffunc))
      dp <- m$fit$DIDparams
      stopifnot(identical(as.integer(dp$cluster_vector), ids %/% 1000L),
                identical(dp$cluster_vector_var, "state_id"),
                dp$anticipation == 14L, dp$base_period == "universal",
                dp$control_group == "notyettreated", dp$bstrap, !dp$cband,
                dp$biters == 1000L, nrow(m$fit$inffunc) == m$fit$n,
                max(abs(m$weighted_validation$att - m$weighted_validation$manual_att)) < 1e-8)
      data.table(model = basename(path), seed = m$seed, counties = m$fit$n,
        processed_rows = nrow(dp$data), processed_min_year = min(dp$data$year),
        processed_max_year = max(dp$data$year), state_clusters = uniqueN(dp$cluster_vector),
        bootstrap_iterations = dp$biters, unique_warnings = length(m$warnings),
        maximum_aggregation_error = max(abs(m$weighted_validation$att - m$weighted_validation$manual_att)),
        cluster_alignment_valid = TRUE)
    }))
    fwrite(metadata, file.path(d, "model_metadata.csv"))
    summary <- fread(file.path(d, "sample_summary.csv"))
    # Compatibility with completed runs made before the metadata label clarified
    # the distinction between supplied rows and did's internal time truncation.
    if ("estimation_rows" %in% names(summary)) setnames(summary, "estimation_rows", "estimation_input_rows")
    fwrite(summary, file.path(d, "sample_summary.csv"))
    plot_annual_results(pooled, d, profile$pre)
    plot_annual_results(cells, file.path(d, "per_cohort"), profile$pre, TRUE)
    figures <- list.files(d, pattern = "[.](png|pdf)$", recursive = TRUE, full.names = TRUE)
    stopifnot(length(figures) == 16L, all(file.info(figures)$size > 0))
    reports[[nm]] <- data.table(profile = nm, models = nrow(metadata),
      pooled_rows = nrow(pooled), cohort_rows = nrow(cells),
      target_counties = nrow(targets), target_jurisdictions = uniqueN(targets$state_id),
      supported_comparisons = nrow(support),
      max_direct_did_error = max(direct$absolute_error),
      max_aggregation_error = max(metadata$maximum_aggregation_error),
      figures = length(figures), validation_passed = TRUE)
    invisible(gc())
  }
  report <- rbindlist(reports)
  fwrite(report, file.path(results_root, "validation_summary.csv"))
  code_paths <- file.path(repo_root, "analysis", "mandatory_schooling", c(
    "analysis_compulsory_schooling_amws_annual_csdid.R", "annual_csdid_helpers.R", "test_annual_csdid.R"))
  fwrite(data.table(file = basename(code_paths), md5 = unname(tools::md5sum(code_paths))),
         file.path(results_root, "code_manifest.csv"))
  print(report)
}

main <- function() {
  data_root <- annual_data_root()
  inputs <- read_annual_inputs(data_root)
  requested <- tolower(trimws(Sys.getenv("COMPULSORY_AMWS_ANNUAL_PROFILE", unset = "both")))
  if (!requested %in% c("both", names(annual_profiles))) stop("Profile must be both, pre20, or pre40.")
  # Validate nesting even when just one profile is requested.
  s20 <- build_annual_sample(inputs$panel, annual_profiles$pre20)
  s40 <- build_annual_sample(inputs$panel, annual_profiles$pre40)
  stopifnot(all(s40$targets$id %in% s20$targets$id))
  rm(s20, s40)
  profiles <- if (requested == "both") names(annual_profiles) else requested
  results_root <- file.path(data_root, "results", "mandatory_schooling",
                            "compulsory_schooling_amws_annual_csdid")
  if ("--verify-outputs" %in% commandArgs(TRUE)) {
    verify_annual_outputs(results_root, profiles)
    return(invisible(NULL))
  }
  summaries <- lapply(profiles, function(nm) run_annual_profile(inputs, nm, results_root))
  verify_annual_outputs(results_root, profiles)
  print(rbindlist(summaries))
  message("Results: ", results_root)
}

if (sys.nframe() == 0L) main()
