# Fixed, focal-cohort population covariates for annual C&S outcome regression.
# Sourced by the annual runner after annual_csdid_helpers.R.
annual_population_controls <- c(
  baseline_log_population = "Control: log population at -15 | C&S outcome regression",
  preperiod_population_cagr = "Control: population CAGR, -%d to -15 (%d years) | C&S outcome regression"
)

annual_control_label <- function(control, pre) {
  if (control == "preperiod_population_cagr") {
    sprintf(annual_population_controls[[control]], pre, pre - 15L)
  } else annual_population_controls[[control]]
}

annual_covariate_audit <- function(population_panel, sample, pre, control,
                                   cadence = csdid_cadence()) {
  ref <- cadence$ref
  stopifnot(control %in% names(annual_population_controls), pre > -ref)
  units <- unique(sample$panel[, .(id, GEOID, state_id, state_fips, state_abbr, g, target)])
  rbindlist(lapply(sample$weights$g, function(gg) {
    endpoint <- population_panel[year == gg + ref,
      .(id, population_end = population, population_end_source = population_source)]
    startpoint <- population_panel[year == gg - pre,
      .(id, population_start = population, population_start_source = population_source)]
    d <- merge(merge(units, endpoint, by = "id", all.x = TRUE),
               startpoint, by = "id", all.x = TRUE)
    d[, `:=`(focal_g = gg, control_specification = control,
              baseline_year = gg + ref, cagr_start_year = gg - pre,
              cagr_years = pre + ref)]
    d[, end_ok := is.finite(population_end) & population_end > 0]
    d[, start_ok := is.finite(population_start) & population_start > 0]
    d[, covariate := NA_real_]
    if (control == "baseline_log_population") {
      d[end_ok == TRUE, covariate := log(population_end)]
    } else {
      d[end_ok & start_ok, covariate := 100 * expm1(
        (log(population_end) - log(population_start)) / cagr_years)]
    }
    # Earlier adopters cannot be unexposed controls at this cohort's reference.
    d[, status := fcase(g < gg, "excluded_earlier_cohort",
      !end_ok, "excluded_missing_baseline_population",
      !is.finite(covariate), "excluded_missing_start_population",
      g == gg, "target", default = "eligible_control")]
    d[, eligible := status %in% c("target", "eligible_control")]
    stopifnot(all(d[g == gg, eligible]),
              d[g == gg, .N] == sample$weights[g == gg, n_counties])
    d
  }))
}

annual_control_panel <- function(panel, audit, gg) {
  merge(panel, audit[focal_g == gg & eligible, .(id, covariate)], by = "id")
}

annual_control_support <- function(sample, audit, cadence = csdid_cadence()) {
  ref <- cadence$ref
  rbindlist(lapply(sample$weights$g, function(gg) {
    p <- annual_control_panel(sample$panel, audit, gg)
    support <- annual_support(p, sample$weights[g == gg], sample$grid,
                              cadence$anticipation, cadence$step)
    diagnostics <- rbindlist(lapply(sample$grid, function(ee) {
      base <- gg + ref
      threshold <- max(base, gg + ee) + cadence$anticipation
      z <- p[g > threshold & g != gg & year %in% c(base, gg + ee)]
      per_year <- z[, .(rank = qr(cbind(1, covariate))$rank,
                         control_min = min(covariate), control_max = max(covariate)), by = year]
      treated <- p[g == gg & year == base, covariate]
      data.table(event_time = ee,
        control_regression_full_rank = nrow(per_year) == ifelse(ee == ref, 1L, 2L) &&
          all(per_year$rank == 2L),
        target_outside_control_range = if (nrow(per_year)) sum(
          treated < max(per_year$control_min) | treated > min(per_year$control_max)) else NA_integer_)
    }))
    merge(support, diagnostics, by = "event_time")
  }))
}

# Independent point-estimate check: fit control outcomes separately in the two
# calendar years, then evaluate their change at the fixed treated covariates.
manual_annual_reg_did <- function(panel, gg, ee, cadence = csdid_cadence()) {
  ref <- cadence$ref
  if (ee == ref) return(0)
  base <- gg + ref; tt <- gg + ee
  threshold <- max(base, tt) + cadence$anticipation
  b <- panel[year == base]; t <- panel[year == tt]
  cb <- b[g != gg & g > threshold]; ct <- t[g != gg & g > threshold]
  rb <- lm(model_y ~ covariate, data = cb)
  rt <- lm(model_y ~ covariate, data = ct)
  target_x <- rbind(b[g == gg, .(covariate)], t[g == gg, .(covariate)])
  mean(t[g == gg, model_y]) - mean(b[g == gg, model_y]) -
    mean(predict(rt, target_x) - predict(rb, target_x))
}

# Keep the requested target columns and county IFs, rather than saving large
# matrices of irrelevant donor-cohort effects. Original ids and normalization
# are retained so these compact fits fully reproduce the joint inference.
compact_annual_cohort_fit <- function(fit, gg, grid, warnings) {
  positions <- match(paste(gg, gg + grid), paste(fit$group, fit$t))
  stopifnot(!anyNA(positions))
  dp <- fit$DIDparams
  list(g = gg, event_time = grid, att = fit$att[positions],
    inffunc = as.matrix(fit$inffunc[, positions, drop = FALSE]), n = fit$n,
    ids = as.integer(rownames(fit$inffunc)), clusters = as.integer(dp$cluster_vector),
    warnings = warnings,
    metadata = list(est_method = dp$est_method, xformla = paste(deparse(dp$xformla), collapse = " "),
      anticipation = dp$anticipation, base_period = dp$base_period,
      control_group = dp$control_group, allow_unbalanced_panel = dp$allow_unbalanced_panel,
      processed_rows = nrow(dp$data), min_year = min(dp$data$year), max_year = max(dp$data$year),
      did_version = as.character(packageVersion("did"))))
}

aggregate_annual_controlled <- function(fits, units, weights, grid, seed, biters = 1000L,
                                        ref = -15L) {
  stopifnot(!anyDuplicated(units$id), !anyDuplicated(weights$g),
            setequal(vapply(fits, function(f) f$g, integer(1L)), weights$g),
            sum(weights$n_counties) == sum(units$g %in% weights$g))
  n <- nrow(units); k <- length(grid)
  joint <- matrix(0, n, nrow(weights) * k)
  effects <- matrix(NA_real_, nrow(weights), k)
  cells <- vector("list", nrow(weights))
  for (j in seq_len(nrow(weights))) {
    gg <- weights$g[j]
    f <- fits[[which(vapply(fits, function(z) z$g == gg, logical(1L)))]]
    ids <- match(f$ids, units$id)
    stopifnot(!anyNA(ids), !anyDuplicated(ids), length(ids) == f$n,
      identical(f$clusters, as.integer(units$state_id[ids])),
      identical(as.integer(f$event_time), as.integer(grid)), nrow(f$inffunc) == f$n)
    columns <- (j - 1L) * k + seq_len(k)
    # Each fitted IF is normalized by its own processed county count.
    joint[ids, columns] <- f$inffunc * n / f$n
    effects[j, ] <- f$att
    cells[[j]] <- data.table(g = gg, event_time = grid, year = gg + grid, att = f$att)
  }
  complete <- apply(effects, 2L, function(x) all(is.finite(x)))
  pooled <- as.numeric(crossprod(weights$weight, effects))
  pooled[!complete] <- NA_real_
  pooled_if <- matrix(0, n, k)
  p_target <- sum(weights$n_counties) / n
  for (j in seq_len(nrow(weights))) {
    columns <- (j - 1L) * k + seq_len(k)
    pooled_if <- pooled_if + weights$weight[j] * joint[, columns, drop = FALSE]
    # Delta method for estimated target cohort shares: I(G=g)/P(target)
    # times (ATT_g - ATT_pooled); summing over cohorts centers this term.
    in_cohort <- units$g == weights$g[j]
    pooled_if[in_cohort, ] <- sweep(pooled_if[in_cohort, , drop = FALSE],
                                   2L, (effects[j, ] - pooled) / p_target, "+")
  }
  all_if <- cbind(joint, pooled_if)
  all_att <- c(as.vector(t(effects)), pooled)
  finite <- is.finite(all_att) & apply(all_if, 2L, function(x) all(is.finite(x)))
  se <- rep(NA_real_, length(all_att))
  zero <- finite & vapply(seq_along(all_att), function(j) all(abs(all_if[, j]) < 1e-12), logical(1L))
  se[zero] <- 0
  nonzero <- which(finite & !zero)
  set.seed(seed)
  if (length(nonzero)) {
    params <- list(idname = "id", tname = "year", clustervars = "state_id",
      biters = biters, alp = 0.05, panel = FALSE, allow_unbalanced_panel = TRUE,
      true_repeated_cross_sections = FALSE, cluster_vector = units$state_id)
    se[nonzero] <- did:::mboot(all_if[, nonzero, drop = FALSE], params, return_V = FALSE)$se
  }
  if (any(finite & !is.finite(se))) stop("Missing bootstrap inference for a finite controlled effect.")
  cells <- rbindlist(cells)
  cells[, se := head(se, nrow(cells))]
  dynamic <- data.table(event_time = grid, att = pooled, se = tail(se, k))
  for (d in list(cells, dynamic)) {
    d[, status := fifelse(is.finite(att), "estimated", "incomplete_cohort_support")]
    d[event_time == ref & is.finite(att), `:=`(att = 0, se = 0, status = "reference")]
    d[, `:=`(conf_low = att - qnorm(.975) * se, conf_high = att + qnorm(.975) * se)]
  }
  list(cells = cells, dynamic = dynamic, influence = pooled_if,
       units = units, seed = seed, biters = biters,
       weight_influence_included = TRUE)
}

annual_control_code_manifest <- function(
    runner = "analysis_compulsory_schooling_amws_annual_csdid.R",
    extra = character()) {
  # `extra` entries are repo-relative so cadence files outside this folder
  # (the five-year bin rule lives in prep/amws) can still be hashed.
  paths <- c(file.path(repo_root, "analysis", "mandatory_schooling", c(
    runner, "annual_csdid_helpers.R", "csdid_figure_helpers.R",
    "annual_csdid_population_controls.R")),
    if (length(extra)) file.path(repo_root, extra))
  data.table(file = basename(paths), md5 = unname(tools::md5sum(paths)))
}

run_annual_population_control <- function(inputs, profile_name, control, results_root,
                                          cadence = csdid_cadence(),
                                          profiles = annual_profiles,
                                          seed_base = 20260906L,
                                          runner = "analysis_compulsory_schooling_amws_annual_csdid.R",
                                          extra_code = character()) {
  profile <- profiles[[profile_name]]
  ref <- cadence$ref
  specifications <- csdid_profile_specifications(profile)
  sample <- build_annual_sample(inputs$panel, profile,
                                anticipation = cadence$anticipation, step = cadence$step)
  output <- file.path(results_root, paste0(profile_name, cadence$dir_suffix), control)
  dir.create(file.path(output, "models"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output, "per_cohort"), showWarnings = FALSE)
  audit <- annual_covariate_audit(inputs$panel, sample, profile$pre, control, cadence)
  support <- annual_control_support(sample, audit, cadence)
  fwrite(audit, file.path(output, "covariate_audit.csv"))
  fwrite(support, file.path(output, "comparison_support.csv"))
  fwrite(sample$targets, file.path(output, "treated_counties.csv"))
  fwrite(sample$weights, file.path(output, "cohort_weights.csv"))
  if (any(!support$status %in% c("supported", "reference") | !support$control_regression_full_rank)) {
    stop("Unsupported controlled comparison; see comparison_support.csv in ", output)
  }
  control_pool <- unique(audit[eligible & !target, .(id, state_fips)])
  summary <- data.table(profile = profile_name, control_specification = control,
    target_counties = nrow(sample$targets), target_jurisdictions = uniqueN(sample$targets$state_id),
    events = nrow(sample$weights), control_counties = nrow(control_pool),
    control_states = uniqueN(control_pool$state_fips),
    min_control_states = min(support$control_year_states, support$control_base_states),
    comparisons_requiring_extrapolation = sum(support$target_outside_control_range > 0L))
  fwrite(summary, file.path(output, "sample_summary.csv"))
  units <- unique(sample$panel[, .(id, g, state_id)])
  setorder(units, id)
  combinations <- CJ(outcome_name = names(annual_outcomes), specification = specifications)
  input_md5 <- tools::md5sum(inputs$paths)
  code_manifest <- annual_control_code_manifest(runner, extra_code)
  for (i in seq_len(nrow(combinations))) {
    outcome <- combinations$outcome_name[i]; spec <- combinations$specification[i]
    model_path <- file.path(output, "models", paste0(outcome, "_", spec, ".rds"))
    model_seed <- seed_base + match(profile_name, names(profiles)) * 1000L +
      match(control, names(annual_population_controls)) * 100L + i
    if ("--resume" %in% commandArgs(TRUE) && file.exists(model_path)) {
      existing <- readRDS(model_path)
      stopifnot(identical(existing$input_md5, input_md5),
                identical(as.data.frame(existing$code_manifest), as.data.frame(code_manifest)),
                existing$seed == model_seed)
      message("Resume: verified completed model ", basename(model_path))
      next
    }
    message(format(Sys.time(), "%H:%M:%S"), " ", profile_name, " / ", control, " / ", outcome, " / ", spec)
    p <- copy(sample$panel); trends <- NULL
    if (spec == "detrended") {
      tr <- annual_detrend(p, outcome, profile$pre, cadence$anticipation, cadence$step)
      p <- tr$panel; trends <- tr$trends
    } else p[, model_y := get(outcome)]
    fits <- list(); checks <- list()
    for (gg in sample$weights$g) {
      cp <- annual_control_panel(p, audit, gg)
      fitted <- fit_annual_csdid(cp, anticipation = cadence$anticipation,
                                 xformla = ~covariate, est_method = "reg", bstrap = FALSE)
      compact <- compact_annual_cohort_fit(fitted$fit, gg, sample$grid, fitted$warnings)
      if (any(!is.finite(compact$att))) {
        fwrite(data.table(g = gg, event_time = sample$grid, att = compact$att),
               file.path(output, "failed_cohort_cells.csv"))
        stop("A requested controlled cohort effect is not estimable; no reweighting performed.")
      }
      checks[[as.character(gg)]] <- data.table(g = gg,
        event_time = unique(c(-profile$pre, ref, ref + cadence$step, 0L, profile$post)))
      checks[[as.character(gg)]][, manual_att := vapply(event_time,
        function(ee) manual_annual_reg_did(cp, gg, ee, cadence), numeric(1L))]
      checks[[as.character(gg)]][, att := compact$att[match(event_time, sample$grid)]]
      checks[[as.character(gg)]][, absolute_error := abs(att - manual_att)]
      stopifnot(all(checks[[as.character(gg)]]$absolute_error < 1e-7))
      fits[[as.character(gg)]] <- compact
      message("  cohort ", gg, ": ", compact$n, " counties; target cells verified")
      rm(fitted, cp); invisible(gc(FALSE))
    }
    aggregate <- aggregate_annual_controlled(fits, units, sample$weights, sample$grid,
                                             model_seed, ref = ref)
    saveRDS(list(cohort_fits = fits, aggregation = aggregate, cohort_weights = sample$weights,
      checks = rbindlist(checks), trends = trends, outcome_name = outcome, specification = spec,
      profile = profile_name, control_specification = control, seed = model_seed,
      input_md5 = input_md5, code_manifest = code_manifest), model_path, compress = "gzip")
  }
  fwrite(data.table(input = names(inputs$paths), path = unname(inputs$paths), md5 = unname(input_md5)),
         file.path(output, "input_manifest.csv"))
  fwrite(code_manifest, file.path(output, "code_manifest.csv"))
  writeLines(capture.output(sessionInfo()), file.path(output, "session_info.txt"))
  writeLines(c(annual_control_label(control, profile$pre),
    "Population is interpolated between census observations; CAGR is percent per year.",
    "Fixed covariates use the focal treatment cohort's calendar dates for every county.",
    "C&S outcome regression retains the single-county DC cohort; it is not doubly robust.",
    sprintf("Reference %d; anticipation %d; not-yet-treated donor eligibility uses Lingwall law dates.",
            ref, cadence$anticipation),
    paste0("Law dates are Lingwall (2014), Table 6.2; ", cadence$exposure_note, "."),
    "Detrending uses original profile cohort trends; inference is conditional on those fitted trends.",
    "County-count aggregation includes cohort-share uncertainty and joint county influence functions.",
    "State-clustered multiplier bootstrap: 1000 shared draws; 95% pointwise intervals.",
    "The control-pool box counts the union of eligible later-adopter counties across focal cohorts.",
    "Control availability varies by comparison; range extrapolation is audited, not trimmed.",
    "Compact RDS fits retain all requested cohort effects, county IFs, ids, normalization and metadata.",
    paste("Generated:", Sys.time())), file.path(output, "notes.txt"))
  invisible(summary)
}

verify_annual_population_control <- function(inputs, profile_name, control, results_root,
                                             cadence = csdid_cadence(),
                                             profiles = annual_profiles,
                                             runner = "analysis_compulsory_schooling_amws_annual_csdid.R",
                                             extra_code = character()) {
  profile <- profiles[[profile_name]]
  ref <- cadence$ref
  expected_models <- csdid_specification_count(profile)
  sample <- build_annual_sample(inputs$panel, profile,
                                anticipation = cadence$anticipation, step = cadence$step)
  output <- file.path(results_root, paste0(profile_name, cadence$dir_suffix), control)
  audit <- fread(file.path(output, "covariate_audit.csv"))
  support <- fread(file.path(output, "comparison_support.csv"))
  stopifnot(all(support$status %in% c("supported", "reference")),
            all(support$control_regression_full_rank),
            all(audit[g == focal_g, eligible]))
  expected_audit <- annual_covariate_audit(inputs$panel, sample, profile$pre, control, cadence)
  stopifnot(isTRUE(all.equal(audit$covariate, expected_audit$covariate, tolerance = 1e-12)),
            identical(audit$id, expected_audit$id), identical(audit$focal_g, expected_audit$focal_g))
  paths <- list.files(file.path(output, "models"), pattern = "[.]rds$", full.names = TRUE)
  stopifnot(length(paths) == expected_models)
  pooled <- list(); cells <- list(); checks <- list(); trends <- list(); metadata <- list(); warnings <- list()
  for (i in seq_along(paths)) {
    m <- readRDS(paths[i])
    stopifnot(identical(m$input_md5, tools::md5sum(inputs$paths)),
              identical(as.data.frame(m$cohort_weights), as.data.frame(sample$weights)),
              m$control_specification == control, m$profile == profile_name,
              m$aggregation$biters == 1000L, m$aggregation$weight_influence_included)
    agg <- aggregate_annual_controlled(m$cohort_fits, m$aggregation$units,
      m$cohort_weights, sample$grid, m$seed, ref = ref)
    stopifnot(isTRUE(all.equal(agg, m$aggregation, tolerance = 1e-12)),
      all(is.finite(agg$dynamic$att)), all(is.finite(agg$dynamic$se)),
      all(is.finite(agg$cells$att)), all(is.finite(agg$cells$se)),
      max(m$checks$absolute_error) < 1e-7)
    for (d in list(agg$dynamic, agg$cells, m$checks)) {
      d[, `:=`(profile = profile_name, outcome_name = m$outcome_name, specification = m$specification,
        specification_label = if (m$specification == "levels") "Levels" else "Cohort-linear detrended",
        control_specification = control, estimator = "C&S outcome regression",
        reference_event = ref, anticipation_years = cadence$anticipation,
        bootstrap_iterations = 1000L, seed = m$seed)]
    }
    pooled[[i]] <- agg$dynamic; cells[[i]] <- agg$cells; checks[[i]] <- m$checks
    if (!is.null(m$trends)) {
      stopifnot(all(m$trends$fitting_max_event <= ref))
      m$trends[, `:=`(outcome_name = m$outcome_name, specification = m$specification)]
      trends[[i]] <- m$trends
    }
    for (f in m$cohort_fits) {
      stopifnot(f$metadata$est_method == "reg", f$metadata$anticipation == cadence$anticipation,
        f$metadata$base_period == "universal", f$metadata$control_group == "notyettreated")
      metadata[[length(metadata) + 1L]] <- data.table(model = basename(paths[i]), g = f$g,
        counties = f$n, state_clusters = uniqueN(f$clusters), processed_rows = f$metadata$processed_rows,
        processed_min_year = f$metadata$min_year, processed_max_year = f$metadata$max_year,
        seed = m$seed, bootstrap_iterations = 1000L, cluster_alignment_valid = TRUE)
      warnings[[length(warnings) + 1L]] <- data.table(model = basename(paths[i]), g = f$g,
        warning = if (length(f$warnings)) f$warnings else "none")
    }
  }
  pooled <- rbindlist(pooled); cells <- rbindlist(cells); checks <- rbindlist(checks)
  labels <- sample$targets[, .(cohort_label = paste0(g[1L], ": ",
    paste(sort(unique(state_abbr)), collapse = ", "))), by = g]
  cells <- merge(cells, labels, by = "g")
  stopifnot(nrow(pooled) == expected_models * length(sample$grid),
    nrow(cells) == nrow(pooled) * nrow(sample$weights),
    !anyDuplicated(pooled[, .(outcome_name, specification, event_time)]),
    !anyDuplicated(cells[, .(outcome_name, specification, g, event_time)]),
    all(pooled[event_time == ref, att == 0 & se == 0]))
  fwrite(pooled, file.path(output, "event_study_coefficients.csv"))
  fwrite(cells, file.path(output, "per_cohort", "cohort_coefficients.csv"))
  fwrite(checks, file.path(output, "direct_did_validation.csv"))
  # A levels-only profile has no trends to write; an empty file would only be
  # mistaken for a failed fit.
  if (length(trends)) fwrite(rbindlist(trends), file.path(output, "detrending_fits.csv"))
  fwrite(rbindlist(metadata), file.path(output, "model_metadata.csv"))
  fwrite(rbindlist(warnings), file.path(output, "estimation_warnings.csv"))
  pool <- unique(audit[eligible & !target, .(id, state_fips)])
  counts <- list(treated_counties = nrow(sample$targets), treated_states = uniqueN(sample$targets$state_id),
    control_counties = nrow(pool), control_states = uniqueN(pool$state_fips), events = nrow(sample$weights))
  label <- annual_control_label(control, profile$pre)
  plot_csdid_results(pooled, output, profile$pre, counts, FALSE, label, cadence, profile$post)
  plot_csdid_results(cells, file.path(output, "per_cohort"), profile$pre, counts, TRUE, label,
                     cadence, profile$post)
  figures <- list.files(output, pattern = "[.](png|pdf)$", recursive = TRUE, full.names = TRUE)
  stopifnot(length(figures) == annual_figure_file_count(), all(file.info(figures)$size > 0))
  report <- data.table(profile = profile_name, control_specification = control, models = length(paths),
    pooled_rows = nrow(pooled), cohort_rows = nrow(cells),
    target_counties = counts$treated_counties, target_jurisdictions = counts$treated_states,
    control_pool_counties = counts$control_counties, control_pool_states = counts$control_states,
    min_control_states = min(support$control_year_states, support$control_base_states),
    max_direct_did_error = max(checks$absolute_error), figures = length(figures), validation_passed = TRUE)
  fwrite(report, file.path(output, "validation_summary.csv"))
  fwrite(annual_control_code_manifest(runner, extra_code),
         file.path(output, "verification_code_manifest.csv"))
  print(report)
  invisible(report)
}
