###############################################################################
# US county world fairs: pooled Dube-Zipperer synthetic-DiD analysis.
#
# The observed case studies are the validated fixed e=[-30,+50] fits. For each
# event, the 30 originally matched donors define the in-space placebo identities.
# Every pseudo-treated county is then matched anew to 30 controls drawn from the
# event's broad eligible never-host universe, using the same five pre-treatment
# matching variables and the same 50 km fair-venue exclusion as the observed fit.
#
# Optional overrides:
#   US_WF_DZ_CORES=8
#   US_WF_DZ_RANDOMIZATION_DRAWS=1000
#   US_WF_DZ_BASE_SEED=20260902
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(parallel)
  if (!requireNamespace("synthdid", quietly = TRUE)) {
    stop("Package 'synthdid' is required.")
  }
  library(synthdid)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_arg)) stop("Run this file with Rscript.")
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]),
                             winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."),
                           winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "paths.R"))
source(file.path(dirname(script_path), "us_county_amws_synthdid_spec.R"))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  candidate <- file.path("C:/Users", Sys.info()[["user"]],
                         "Globtalent Dropbox", "gtl_talent_dets")
  if (dir.exists(candidate)) TALENT_DETS_DATA_DIR <- candidate
}
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  stop("Set TALENT_DETS_DATA_DIR to the Dropbox project root.")
}

variant <- "fixed_m30"
results_root <- wf_us_sd_output_root(TALENT_DETS_DATA_DIR, variant)
donor_dir <- file.path(results_root, "donor_pools")
model_dir <- file.path(results_root, "models_amws_per_1000_births")
results_dir <- file.path(results_root, "pooled_dz")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

panel_file <- file.path(
  TALENT_DETS_DATA_DIR, "output", "us_panel_county_amws_combined_year.csv"
)
input_files <- c(
  panel = panel_file,
  donor_validation = file.path(donor_dir, "run_validation.csv"),
  donor_candidates = file.path(donor_dir, "donor_candidate_audit.csv"),
  donor_pairs = file.path(donor_dir, "donor_pool_pairs.csv"),
  treated_manifest = file.path(donor_dir, "treated_event_manifest.csv"),
  model_validation = file.path(model_dir, "run_validation.csv"),
  event_effects = file.path(model_dir, "synthdid_event_effects.csv"),
  event_paths = file.path(model_dir, "synthdid_event_gap_paths.csv"),
  control_weights = file.path(model_dir, "synthdid_control_weights.csv")
)
if (any(!file.exists(input_files))) {
  stop("Missing required input(s): ",
       paste(input_files[!file.exists(input_files)], collapse = "; "))
}

donor_count <- wf_us_sd_donor_count()
event_times <- wf_us_sd_event_times(variant = variant)
reference_event_time <- wf_us_sd_reference_event_time()
treatment_start_event_time <- wf_us_sd_treatment_start_event_time()
fair_event_time <- wf_us_sd_fair_event_time()
randomization_draws <- suppressWarnings(as.integer(Sys.getenv(
  "US_WF_DZ_RANDOMIZATION_DRAWS", unset = "1000"
)))
base_seed <- suppressWarnings(as.integer(Sys.getenv(
  "US_WF_DZ_BASE_SEED", unset = "20260902"
)))
detected_cores <- parallel::detectCores(logical = FALSE)
if (!is.finite(detected_cores)) detected_cores <- 2L
cores <- suppressWarnings(as.integer(Sys.getenv(
  "US_WF_DZ_CORES",
  unset = as.character(max(1L, min(8L, detected_cores - 1L)))
)))
if (!is.finite(cores) || cores < 1L) cores <- 1L
if (!is.finite(randomization_draws) || randomization_draws < 1L) {
  stop("US_WF_DZ_RANDOMIZATION_DRAWS must be positive.")
}
if (!is.finite(base_seed)) stop("US_WF_DZ_BASE_SEED must be an integer.")

matching_specification <- wf_us_sd_matching_specification()
placebo_design <- paste0(
  "original_30_event_donors_as_pseudo_treated_identities__",
  "fresh_30_nearest_controls_from_broad_event_eligible_universe"
)
feature_columns <- wf_us_sd_feature_spec()$feature

coerce_logical <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("true", "1", "yes", "y")
}
mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

build_decade_panel <- function(panel_year, definition) {
  x <- copy(panel_year)
  x[, decade := wf_us_sd_decade(year, definition)]
  x <- x[, .(
    n_amws = sum_or_na(n_amws),
    population = mean_or_na(population),
    county_births_estimate = sum_or_na(county_births_estimate_year)
  ), by = .(GEOID, decade)]
  x[, outcome_value := fifelse(
    is.finite(county_births_estimate) & county_births_estimate > 0,
    1000 * n_amws / county_births_estimate,
    NA_real_
  )]
  x[]
}

fit_gap_path <- function(Y, N0 = 30L, T0 = 3L) {
  noise_level <- suppressWarnings(sd(apply(
    Y[seq_len(N0), seq_len(T0), drop = FALSE], 1L, diff
  )))
  noise_floor_used <- !is.finite(noise_level) || noise_level <= 0
  if (noise_floor_used) noise_level <- 1e-8
  fit <- tryCatch(
    synthdid::synthdid_estimate(Y, N0, T0, noise.level = noise_level),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(ok = FALSE, error = conditionMessage(fit)))
  }
  weights <- attr(fit, "weights")
  omega <- as.numeric(weights$omega)
  lambda <- as.numeric(weights$lambda)
  if (length(omega) != N0 || any(!is.finite(omega)) ||
      abs(sum(omega) - 1) > 1e-8) {
    return(list(ok = FALSE, error = "Invalid donor weights."))
  }
  if (length(lambda) != T0 || any(!is.finite(lambda)) ||
      abs(sum(lambda) - 1) > 1e-8) {
    return(list(ok = FALSE, error = "Invalid time weights."))
  }
  controls <- Y[seq_len(N0), , drop = FALSE]
  treated <- as.numeric(Y[nrow(Y), ])
  synthetic <- as.numeric(crossprod(omega, controls))
  gap <- treated - synthetic
  pre <- seq_len(T0)
  post <- (T0 + 1L):ncol(Y)
  gap_adjusted <- gap - gap[[T0]]
  rmspe_pre <- sqrt(mean(gap[pre]^2))
  rmspe_post <- sqrt(mean(gap[post]^2))
  mean_abs_pre_panel <- mean(abs(Y[, pre, drop = FALSE]))
  standardized <- if (rmspe_pre > 0) {
    mean(gap_adjusted[post]) / rmspe_pre
  } else if (abs(mean(gap_adjusted[post])) <= 1e-15) {
    0
  } else {
    sign(mean(gap_adjusted[post])) * Inf
  }
  list(
    ok = TRUE, error = NA_character_, omega = omega, lambda = lambda,
    noise_level = noise_level, noise_floor_used = noise_floor_used,
    treated = treated, synthetic = synthetic, gap = gap,
    gap_adjusted = gap_adjusted,
    tau = mean(gap_adjusted[post]),
    att_synthdid = as.numeric(fit),
    rmspe_pre = rmspe_pre,
    rmspe_post = rmspe_post,
    nrmspe_pre = if (is.finite(mean_abs_pre_panel) && mean_abs_pre_panel > 0)
      rmspe_pre / mean_abs_pre_panel else NA_real_,
    standardized = standardized,
    post_pre_ratio = if (is.finite(rmspe_pre) && rmspe_pre > 0)
      rmspe_post / rmspe_pre else NA_real_
  )
}

observed_rank <- function(observed, placebos) {
  placebos <- placebos[!is.na(placebos)]
  if (is.na(observed) || !length(placebos)) return(NA_real_)
  (1 + sum(placebos <= observed)) / (length(placebos) + 1)
}

self_rank <- function(x) {
  good <- !is.na(x)
  out <- rep(NA_real_, length(x))
  if (sum(good) < 2L) return(out)
  out[good] <- rank(x[good], ties.method = "max") / sum(good)
  out
}

randomization_critical_value <- function(x, centre, alpha = 0.05) {
  deviations <- sort(abs(x[is.finite(x)] - centre), decreasing = TRUE)
  n <- length(deviations)
  if (!n || !is.finite(centre)) return(NA_real_)
  tail_count <- ceiling(alpha * (n + 1L) - 1L)
  tail_count <- min(n, max(1L, tail_count))
  deviations[[tail_count]]
}

validation_ok <- function(file) {
  x <- fread(file)
  x[, passed := coerce_logical(passed)]
  nrow(x) > 0L && all(x$passed)
}
if (!validation_ok(input_files[["donor_validation"]]) ||
    !validation_ok(input_files[["model_validation"]])) {
  stop("The fixed-window donor or model stage did not pass validation.")
}

message("Reading observed fixed-window fits and matching inputs...")
manifest <- fread(input_files[["treated_manifest"]], na.strings = c("", "NA"))
manifest[, `:=`(
  GEOID = sprintf("%05d", as.integer(GEOID)),
  treatment_cohort = as.integer(treatment_cohort),
  first_exposure_year = as.integer(first_exposure_year)
)]
setorder(manifest, decade_definition, treatment_cohort, GEOID)

donor_pairs <- fread(input_files[["donor_pairs"]], na.strings = c("", "NA"))
donor_pairs[, `:=`(
  donor_GEOID = sprintf("%05d", as.integer(donor_GEOID)),
  treated_GEOID = sprintf("%05d", as.integer(treated_GEOID)),
  donor_rank = as.integer(donor_rank)
)]
setorder(donor_pairs, model_id, donor_rank)

candidate_audit <- fread(input_files[["donor_candidates"]],
                         na.strings = c("", "NA"))
candidate_audit[, `:=`(
  GEOID = sprintf("%05d", as.integer(GEOID)),
  treated_GEOID = sprintf("%05d", as.integer(treated_GEOID)),
  eligible_donor = coerce_logical(eligible_donor)
)]

observed_events <- fread(input_files[["event_effects"]],
                         na.strings = c("", "NA"))
observed_events[, treated_GEOID := sprintf("%05d", as.integer(treated_GEOID))]
observed_paths <- fread(input_files[["event_paths"]],
                        na.strings = c("", "NA"))
observed_paths[, treated_GEOID := sprintf("%05d", as.integer(treated_GEOID))]
observed_weights <- fread(input_files[["control_weights"]],
                          na.strings = c("", "NA"))
observed_weights[, `:=`(
  treated_GEOID = sprintf("%05d", as.integer(treated_GEOID)),
  donor_GEOID = sprintf("%05d", as.integer(donor_GEOID))
)]

if (nrow(manifest) != 12L || nrow(observed_events) != 12L ||
    nrow(donor_pairs) != 360L ||
    any(observed_events$variant != variant) ||
    any(observed_events$n_pre_periods != 3L) ||
    any(observed_events$n_post_periods != 6L)) {
  stop("Unexpected fixed-window US synthetic-DiD inputs.")
}
if (candidate_audit[eligible_donor == TRUE,
                    any(!is.finite(as.matrix(.SD))),
                    .SDcols = feature_columns]) {
  stop("Eligible matching features contain non-finite values.")
}

panel_year <- fread(
  input_files[["panel"]], na.strings = c("", "NA"),
  select = c("GEOID", "year", "population",
             "county_births_estimate_year", "n_amws")
)
panel_year[, `:=`(
  GEOID = sprintf("%05d", as.integer(GEOID)), year = as.integer(year),
  population = as.numeric(population),
  county_births_estimate_year = as.numeric(county_births_estimate_year),
  n_amws = as.numeric(n_amws)
)]
panels <- lapply(c("standard", "alternative"), function(definition) {
  build_decade_panel(panel_year, definition)
})
names(panels) <- c("standard", "alternative")
rm(panel_year); gc()

event_manifest <- manifest[, .(
  event_key = model_id, model_id, decade_definition,
  treated_GEOID = GEOID, treated_county_name = target_unit_name,
  fair_name = first_fair_name, fair_year = first_exposure_year,
  treatment_cohort
)]
setorder(event_manifest, decade_definition, treatment_cohort, treated_GEOID)

build_event_context <- function(event) {
  eligible <- copy(candidate_audit[
    model_id == event$model_id & eligible_donor == TRUE
  ])
  setorder(eligible, GEOID)
  if (nrow(eligible) < donor_count + 1L || anyDuplicated(eligible$GEOID)) {
    stop("Invalid broad eligible universe for ", event$model_id, ".")
  }
  decades <- event$treatment_cohort + event_times
  outcome_wide <- dcast(
    panels[[event$decade_definition]][
      GEOID %chin% eligible$GEOID & decade %in% decades,
      .(GEOID, decade, outcome_value)
    ],
    GEOID ~ decade, value.var = "outcome_value"
  )
  outcome_wide <- outcome_wide[match(eligible$GEOID, GEOID)]
  if (nrow(outcome_wide) != nrow(eligible) ||
      anyNA(outcome_wide) ||
      !identical(outcome_wide$GEOID, eligible$GEOID)) {
    stop("Incomplete broad eligible outcome panel for ", event$model_id, ".")
  }
  outcome <- as.matrix(outcome_wide[, -"GEOID"])
  storage.mode(outcome) <- "double"
  colnames(outcome) <- as.character(decades)
  features <- as.matrix(eligible[, ..feature_columns])
  storage.mode(features) <- "double"
  pseudo_ids <- donor_pairs[model_id == event$model_id][order(donor_rank),
                                                        donor_GEOID]
  pseudo_indices <- match(pseudo_ids, eligible$GEOID)
  if (length(pseudo_ids) != donor_count || anyNA(pseudo_indices) ||
      anyDuplicated(pseudo_ids)) {
    stop("Original 30 placebo identities are invalid for ", event$model_id, ".")
  }
  list(
    ids = eligible$GEOID,
    names = eligible$donor_county_name,
    venue_distance = eligible$distance_from_event_venue_km,
    features = features, outcome = outcome, decades = decades,
    pseudo_ids = pseudo_ids, pseudo_indices = pseudo_indices
  )
}

# These globals are updated once for each event and exported to workers.
fit_one_pseudo <- function(pseudo_position) {
  pseudo_index <- context_pseudo_indices[[pseudo_position]]
  candidates <- setdiff(seq_along(context_ids), pseudo_index)
  donor_features <- context_features[candidates, , drop = FALSE]
  feature_mean <- colMeans(donor_features)
  feature_sd <- apply(donor_features, 2L, sd)
  if (any(!is.finite(feature_mean)) || any(!is.finite(feature_sd)) ||
      any(feature_sd <= 0) ||
      any(!is.finite(context_features[pseudo_index, ]))) {
    return(list(ok = FALSE, pseudo_position = pseudo_position,
                pseudo_index = pseudo_index,
                error = "Non-finite or degenerate matching feature."))
  }
  donor_z <- sweep(sweep(donor_features, 2L, feature_mean, "-"),
                   2L, feature_sd, "/")
  pseudo_z <- (context_features[pseudo_index, ] - feature_mean) / feature_sd
  distance <- sqrt(rowSums(sweep(donor_z, 2L, pseudo_z, "-")^2))
  ord <- order(distance, context_ids[candidates])
  selected <- candidates[ord[seq_len(donor_count)]]
  selected_distance <- distance[ord[seq_len(donor_count)]]
  Y <- rbind(context_outcome[selected, , drop = FALSE],
             context_outcome[pseudo_index, , drop = FALSE])
  fit <- fit_gap_path(Y, donor_count, 3L)
  if (!fit$ok) {
    return(list(ok = FALSE, pseudo_position = pseudo_position,
                pseudo_index = pseudo_index, selected = selected,
                match_distance = selected_distance, error = fit$error))
  }
  c(list(
    ok = TRUE, pseudo_position = pseudo_position,
    pseudo_index = pseudo_index, selected = selected,
    match_distance = selected_distance
  ), fit)
}

cluster <- NULL
if (cores > 1L) {
  cluster <- makeCluster(min(cores, donor_count))
  clusterEvalQ(cluster, suppressPackageStartupMessages(library(synthdid)))
  clusterExport(cluster, c("fit_gap_path", "fit_one_pseudo", "donor_count"),
                envir = environment())
}
on.exit(if (!is.null(cluster)) stopCluster(cluster), add = TRUE)

message("Rematching and estimating 360 placebo case studies...")
context_outputs <- vector("list", nrow(event_manifest))
for (i in seq_len(nrow(event_manifest))) {
  event <- event_manifest[i]
  context <- build_event_context(event)
  context_ids <- context$ids
  context_names <- context$names
  context_venue_distance <- context$venue_distance
  context_features <- context$features
  context_outcome <- context$outcome
  context_pseudo_indices <- context$pseudo_indices
  if (!is.null(cluster)) {
    clusterExport(
      cluster,
      c("context_ids", "context_features", "context_outcome",
        "context_pseudo_indices"),
      envir = environment()
    )
    fits <- parLapplyLB(cluster, seq_len(donor_count), fit_one_pseudo)
  } else {
    fits <- lapply(seq_len(donor_count), fit_one_pseudo)
  }
  context_outputs[[i]] <- list(
    event = event, context = context, fits = fits
  )
  message(sprintf(
    "  [%d/12] %s: %d successful, %d failed",
    i, event$model_id, sum(vapply(fits, `[[`, logical(1L), "ok")),
    sum(!vapply(fits, `[[`, logical(1L), "ok"))
  ))
}
if (!is.null(cluster)) {
  stopCluster(cluster)
  cluster <- NULL
}

message("Assembling event, placebo, and rematched-pool outputs...")
placebo_stats_list <- list()
placebo_paths_list <- list()
pool_pairs_list <- list()
pool_qc_list <- list()
for (i in seq_along(context_outputs)) {
  out <- context_outputs[[i]]
  event <- out$event
  context <- out$context
  fits <- out$fits
  placebo_stats_list[[i]] <- rbindlist(lapply(fits, function(z) {
    pseudo_id <- context$ids[[z$pseudo_index]]
    pseudo_name <- context$names[[z$pseudo_index]]
    if (!z$ok) return(data.table(
      event_key = event$model_id, decade_definition = event$decade_definition,
      treated_GEOID = event$treated_GEOID,
      placebo_treated_GEOID = pseudo_id,
      placebo_treated_county_name = pseudo_name,
      successful = FALSE, error = z$error
    ))
    data.table(
      event_key = event$model_id, decade_definition = event$decade_definition,
      treated_GEOID = event$treated_GEOID,
      treated_county_name = event$treated_county_name,
      fair_name = event$fair_name, fair_year = event$fair_year,
      treatment_cohort = event$treatment_cohort,
      placebo_treated_GEOID = pseudo_id,
      placebo_treated_county_name = pseudo_name,
      original_donor_rank = z$pseudo_position,
      successful = TRUE, error = NA_character_,
      eligible_rematched_controls = length(context$ids) - 1L,
      tau_event_effect = z$tau, att_synthdid = z$att_synthdid,
      rmspe_pre = z$rmspe_pre, rmspe_post = z$rmspe_post,
      nrmspe_pre = z$nrmspe_pre,
      standardized_effect = z$standardized,
      post_pre_rmspe_ratio = z$post_pre_ratio,
      synthdid_noise_level = z$noise_level,
      synthdid_noise_floor_used = z$noise_floor_used
    )
  }), fill = TRUE)
  placebo_paths_list[[i]] <- rbindlist(lapply(fits, function(z) {
    if (!z$ok) return(NULL)
    data.table(
      event_key = event$model_id,
      decade_definition = event$decade_definition,
      treated_GEOID = event$treated_GEOID,
      placebo_treated_GEOID = context$ids[[z$pseudo_index]],
      treatment_cohort = event$treatment_cohort,
      decade = context$decades, event_time = event_times,
      gap = z$gap, gap_adjusted = z$gap_adjusted,
      standardized_gap = if (z$rmspe_pre > 0) {
        z$gap_adjusted / z$rmspe_pre
      } else ifelse(abs(z$gap_adjusted) <= 1e-15, 0,
                    sign(z$gap_adjusted) * Inf)
    )
  }), fill = TRUE)
  pool_pairs_list[[i]] <- rbindlist(lapply(fits, function(z) {
    if (is.null(z$selected)) return(NULL)
    data.table(
      event_key = event$model_id,
      decade_definition = event$decade_definition,
      treated_GEOID = event$treated_GEOID,
      placebo_treated_GEOID = context$ids[[z$pseudo_index]],
      placebo_original_donor_rank = z$pseudo_position,
      rematched_donor_GEOID = context$ids[z$selected],
      rematched_donor_county_name = context$names[z$selected],
      rematched_donor_rank = seq_along(z$selected),
      match_distance = z$match_distance,
      distance_from_fair_venue_km = context$venue_distance[z$selected],
      synthdid_weight = if (z$ok) z$omega else NA_real_
    )
  }), fill = TRUE)
  fit_ok <- vapply(fits, `[[`, logical(1L), "ok")
  selected_counts <- vapply(fits, function(z) length(z$selected), integer(1L))
  pool_qc_list[[i]] <- data.table(
    event_key = event$model_id,
    decade_definition = event$decade_definition,
    treated_GEOID = event$treated_GEOID,
    broad_eligible_donors = length(context$ids),
    placebo_identities_requested = donor_count,
    placebo_fits_successful = sum(fit_ok),
    placebo_fits_failed = sum(!fit_ok),
    donors_per_attempt_min = min(selected_counts),
    donors_per_attempt_max = max(selected_counts),
    minimum_selected_venue_distance_km = min(
      context$venue_distance[unique(unlist(lapply(fits, `[[`, "selected")))]
    ),
    maximum_weight_sum_error = max(vapply(fits[fit_ok], function(z) {
      abs(sum(z$omega) - 1)
    }, numeric(1L)))
  )
}

placebo_stats <- rbindlist(placebo_stats_list, fill = TRUE)
placebo_paths <- rbindlist(placebo_paths_list, fill = TRUE)
pool_pairs <- rbindlist(pool_pairs_list, fill = TRUE)
pool_qc <- rbindlist(pool_qc_list, fill = TRUE)
setorder(placebo_stats, event_key, original_donor_rank)
setorder(placebo_paths, event_key, placebo_treated_GEOID, event_time)
setorder(pool_pairs, event_key, placebo_original_donor_rank,
         rematched_donor_rank)

if (any(placebo_stats$successful != TRUE)) {
  print(placebo_stats[successful == FALSE, .N, by = .(event_key, error)])
}
if (nrow(placebo_stats) != 360L || any(placebo_stats$successful != TRUE) ||
    nrow(placebo_paths) != 360L * length(event_times) ||
    nrow(pool_pairs) != 360L * donor_count) {
  stop("Not all 360 rematched placebo fits completed successfully.")
}

event_effects <- copy(observed_events)
setnames(event_effects,
         c("event_effect", "normalized_pre_rmspe"),
         c("tau_event_effect", "nrmspe_pre"))
event_effects[, `:=`(
  standardized_effect = fifelse(pre_rmspe > 0,
                                tau_event_effect / pre_rmspe, NA_real_),
  post_pre_rmspe_ratio = fifelse(pre_rmspe > 0,
                                post_rmspe / pre_rmspe, NA_real_),
  n_placebos = NA_integer_, placebo_p_value = NA_real_,
  percentile_rank = NA_real_, placebo_pool_procedure = placebo_design
)]
for (i in seq_len(nrow(event_effects))) {
  p <- placebo_stats[event_key == event_effects$model_id[[i]]]
  event_effects[i, `:=`(
    n_placebos = sum(!is.na(p$standardized_effect)),
    placebo_p_value = (1 + sum(abs(p$standardized_effect) >=
      abs(standardized_effect))) / (nrow(p) + 1),
    percentile_rank = observed_rank(standardized_effect,
                                    p$standardized_effect)
  )]
}

event_horizons <- merge(
  observed_paths[, .(
    model_id, decade_definition, treated_GEOID, treated_county_name,
    fair_name, fair_year, treatment_cohort, decade, event_time, period,
    gap, gap_adjusted
  )],
  event_effects[, .(model_id, pre_rmspe)],
  by = "model_id", all.x = TRUE, sort = FALSE
)
event_horizons[, `:=`(
  standardized_effect = fifelse(pre_rmspe > 0,
                                gap_adjusted / pre_rmspe, NA_real_),
  percentile_rank = NA_real_, n_placebos = NA_integer_
)]
for (i in seq_len(nrow(event_horizons))) {
  row <- event_horizons[i]
  values <- placebo_paths[
    event_key == row$model_id & event_time == row$event_time,
    standardized_gap
  ]
  event_horizons[i, n_placebos := sum(!is.na(values))]
  if (row$event_time != reference_event_time) {
    event_horizons[i, percentile_rank := observed_rank(
      row$standardized_effect, values
    )]
  }
}
setorder(event_horizons, decade_definition, model_id, event_time)

# Diagnose observed donor reuse without constraining the estimation.
donor_usage <- observed_weights[, .(
  events_used = sum(weight > 1e-12),
  cumulative_weight = sum(weight),
  maximum_weight = max(weight)
), by = .(decade_definition, donor_GEOID)]
setorder(donor_usage, decade_definition, -cumulative_weight, donor_GEOID)
donor_overlap <- rbindlist(lapply(c("standard", "alternative"), function(defn) {
  mids <- sort(unique(observed_weights[decade_definition == defn, model_id]))
  rbindlist(lapply(mids, function(m1) {
    rbindlist(lapply(mids, function(m2) {
      a <- observed_weights[model_id == m1, .(donor_GEOID, w1 = weight)]
      b <- observed_weights[model_id == m2, .(donor_GEOID, w2 = weight)]
      z <- merge(a, b, by = "donor_GEOID", all = TRUE)
      z[is.na(w1), w1 := 0]
      z[is.na(w2), w2 := 0]
      data.table(decade_definition = defn, model_id_1 = m1,
                 model_id_2 = m2,
                 weighted_overlap = sum(pmin(z$w1, z$w2)))
    }))
  }))
}))

# Progressive trimming uses only observed pre-treatment normalized RMSPE.
event_effects[, fit_order := frank(nrmspe_pre, ties.method = "first"),
              by = decade_definition]
max_drop <- 4L
observed_trim <- rbindlist(lapply(c("standard", "alternative"), function(defn) {
  events <- event_effects[decade_definition == defn][order(nrmspe_pre)]
  rbindlist(lapply(0:max_drop, function(drop_k) {
    keep <- events[seq_len(nrow(events) - drop_k)]
    dropped <- if (drop_k) {
      events[(nrow(events) - drop_k + 1L):nrow(events), treated_county_name]
    } else character(0)
    data.table(
      decade_definition = defn,
      worst_fitting_events_dropped = drop_k,
      dropped_event_names = paste(dropped, collapse = "; "),
      n_events = nrow(keep),
      max_nrmspe_pre_retained = max(keep$nrmspe_pre),
      pooled_att = mean(keep$tau_event_effect),
      mean_percentile_rank = mean(keep$percentile_rank),
      mean_standardized_effect = mean(keep$standardized_effect)
    )
  }))
}))

observed_dynamic_trim <- rbindlist(lapply(
  c("standard", "alternative"), function(defn) {
    events <- event_effects[decade_definition == defn][order(nrmspe_pre)]
    rbindlist(lapply(0:max_drop, function(drop_k) {
      keep_ids <- events[seq_len(nrow(events) - drop_k), model_id]
      dropped <- if (drop_k) {
        events[(nrow(events) - drop_k + 1L):nrow(events), treated_county_name]
      } else character(0)
      event_horizons[model_id %chin% keep_ids, .(
        reference_event_time = reference_event_time,
        n_events = uniqueN(model_id),
        pooled_effect = mean(gap_adjusted),
        effect_sd_across_events = sd(gap_adjusted),
        mean_percentile_rank = if (event_time[[1L]] == reference_event_time)
          NA_real_ else mean(percentile_rank),
        dropped_event_names = paste(dropped, collapse = "; "),
        max_nrmspe_pre_retained = max(
          events[model_id %chin% keep_ids, nrmspe_pre]
        )
      ), by = event_time][, `:=`(
        decade_definition = defn,
        worst_fitting_events_dropped = drop_k
      )]
    }))
  }
))
setorder(observed_dynamic_trim, decade_definition,
         worst_fitting_events_dropped, event_time)

message("Running ", randomization_draws,
        " joint randomization draws per decade definition...")
draw_rows <- list()
trim_inference <- list()
horizon_inference <- list()
pooled_rows <- list()
for (definition_index in seq_along(c("standard", "alternative"))) {
  defn <- c("standard", "alternative")[[definition_index]]
  events <- event_effects[decade_definition == defn][order(nrmspe_pre)]
  event_keys <- events$model_id
  n_events <- nrow(events)
  stats_lookup <- lapply(event_keys, function(key) {
    z <- copy(placebo_stats[event_key == key & successful == TRUE])
    z[, self_percentile_rank := self_rank(standardized_effect)]
    setkey(z, placebo_treated_GEOID)
    z
  })
  names(stats_lookup) <- event_keys
  path_lookup <- lapply(event_keys, function(key) {
    z <- dcast(placebo_paths[event_key == key],
               placebo_treated_GEOID ~ event_time,
               value.var = "gap_adjusted")
    setkey(z, placebo_treated_GEOID)
    z
  })
  names(path_lookup) <- event_keys
  rank_path_lookup <- lapply(event_keys, function(key) {
    z <- dcast(placebo_paths[event_key == key],
               placebo_treated_GEOID ~ event_time,
               value.var = "standardized_gap")
    for (column in setdiff(names(z), "placebo_treated_GEOID")) {
      z[, (column) := self_rank(get(column))]
    }
    setkey(z, placebo_treated_GEOID)
    z
  })
  names(rank_path_lookup) <- event_keys

  draw_att <- array(NA_real_, c(randomization_draws, max_drop + 1L,
                                length(event_times)))
  draw_rank <- array(NA_real_, c(randomization_draws, max_drop + 1L,
                                 length(event_times)))
  draw_scalar_att <- matrix(NA_real_, randomization_draws, max_drop + 1L)
  draw_scalar_rank <- matrix(NA_real_, randomization_draws, max_drop + 1L)
  draw_summary <- vector("list", randomization_draws)
  set.seed(base_seed + definition_index * 7919L)
  for (b in seq_len(randomization_draws)) {
    universe <- unique(unlist(lapply(stats_lookup, function(z) {
      z$placebo_treated_GEOID
    }), use.names = FALSE))
    assigned <- rep(NA_character_, n_events)
    for (attempt in 1:100) {
      permutation <- sample(universe)
      position <- setNames(seq_along(permutation), permutation)
      used <- character(0)
      assigned[] <- NA_character_
      for (k in sample(seq_len(n_events))) {
        eligible_ids <- setdiff(
          stats_lookup[[event_keys[[k]]]]$placebo_treated_GEOID, used
        )
        if (!length(eligible_ids)) break
        pick <- eligible_ids[[which.min(position[eligible_ids])]]
        assigned[[k]] <- pick
        used <- c(used, pick)
      }
      if (!anyNA(assigned)) break
    }
    if (anyNA(assigned)) {
      stop("Could not construct a unique joint placebo assignment for ", defn)
    }
    tau <- ranks <- nfit <- numeric(n_events)
    paths <- rank_paths <- matrix(NA_real_, n_events, length(event_times))
    for (k in seq_len(n_events)) {
      stat <- stats_lookup[[event_keys[[k]]]][assigned[[k]]]
      tau[[k]] <- stat$tau_event_effect
      ranks[[k]] <- stat$self_percentile_rank
      nfit[[k]] <- stat$nrmspe_pre
      paths[k, ] <- unlist(
        path_lookup[[event_keys[[k]]]][assigned[[k]], -1L],
        use.names = FALSE
      )
      rank_paths[k, ] <- unlist(
        rank_path_lookup[[event_keys[[k]]]][assigned[[k]], -1L],
        use.names = FALSE
      )
    }
    fit_order <- order(nfit)
    for (drop_k in 0:max_drop) {
      keep <- fit_order[seq_len(n_events - drop_k)]
      draw_att[b, drop_k + 1L, ] <- colMeans(paths[keep, , drop = FALSE])
      draw_rank[b, drop_k + 1L, ] <- colMeans(
        rank_paths[keep, , drop = FALSE], na.rm = TRUE
      )
      draw_scalar_att[b, drop_k + 1L] <- mean(tau[keep])
      draw_scalar_rank[b, drop_k + 1L] <- mean(ranks[keep])
    }
    draw_summary[[b]] <- data.table(
      decade_definition = defn, draw = b,
      assigned_placebo_GEOIDs = paste(assigned, collapse = ";"),
      pooled_att = draw_scalar_att[b, 1L],
      mean_percentile_rank = draw_scalar_rank[b, 1L]
    )
  }
  draw_table <- rbindlist(draw_summary)
  draw_rows[[defn]] <- draw_table
  observed_defn_trim <- observed_trim[decade_definition == defn]
  trim_inference[[defn]] <- rbindlist(lapply(0:max_drop, function(drop_k) {
    observed <- observed_defn_trim[
      worst_fitting_events_dropped == drop_k
    ]
    null_att <- draw_scalar_att[, drop_k + 1L]
    null_rank <- draw_scalar_rank[, drop_k + 1L]
    centre_att <- mean(null_att)
    centre_rank <- mean(null_rank)
    data.table(
      decade_definition = defn,
      worst_fitting_events_dropped = drop_k,
      randomization_draws_used = randomization_draws,
      randomization_null_mean_att = centre_att,
      randomization_null_mean_rank = centre_rank,
      randomization_p_att = (1 + sum(abs(null_att - centre_att) >=
        abs(observed$pooled_att - centre_att))) / (randomization_draws + 1),
      randomization_p_rank = (1 + sum(abs(null_rank - centre_rank) >=
        abs(observed$mean_percentile_rank - centre_rank))) /
        (randomization_draws + 1)
    )
  }))
  observed_defn_dynamic <- observed_dynamic_trim[decade_definition == defn]
  horizon_inference[[defn]] <- rbindlist(lapply(0:max_drop, function(drop_k) {
    rbindlist(lapply(seq_along(event_times), function(h) {
      observed <- observed_defn_dynamic[
        worst_fitting_events_dropped == drop_k &
          event_time == event_times[[h]]
      ]
      null_att <- draw_att[, drop_k + 1L, h]
      null_rank <- draw_rank[, drop_k + 1L, h]
      is_reference <- event_times[[h]] == reference_event_time
      centre_att <- mean(null_att)
      centre_rank <- mean(null_rank, na.rm = TRUE)
      critical <- if (!is_reference) {
        randomization_critical_value(null_att, centre_att)
      } else NA_real_
      data.table(
        decade_definition = defn,
        worst_fitting_events_dropped = drop_k,
        event_time = event_times[[h]],
        reference_event_time = reference_event_time,
        omitted_reference_period = is_reference,
        randomization_draws_used = randomization_draws,
        null_mean = centre_att,
        null_lower = quantile(null_att, .025, names = FALSE),
        null_upper = quantile(null_att, .975, names = FALSE),
        randomization_critical_95 = critical,
        effect_ci_lower = if (!is_reference)
          observed$pooled_effect - centre_att - critical else NA_real_,
        effect_ci_upper = if (!is_reference)
          observed$pooled_effect - centre_att + critical else NA_real_,
        confidence_level = if (!is_reference) .95 else NA_real_,
        confidence_method = if (!is_reference)
          "finite_sample_joint_randomization_test_inversion" else
          "omitted_reference_period",
        randomization_p = if (!is_reference)
          (1 + sum(abs(null_att - centre_att) >=
            abs(observed$pooled_effect - centre_att))) /
            (randomization_draws + 1) else NA_real_,
        rank_null_mean = if (!is_reference) centre_rank else NA_real_,
        rank_null_lower = if (!is_reference)
          quantile(null_rank, .025, names = FALSE, na.rm = TRUE) else NA_real_,
        rank_null_upper = if (!is_reference)
          quantile(null_rank, .975, names = FALSE, na.rm = TRUE) else NA_real_,
        rank_randomization_p = if (!is_reference)
          (1 + sum(abs(null_rank - centre_rank) >=
            abs(observed$mean_percentile_rank - centre_rank), na.rm = TRUE)) /
            (sum(is.finite(null_rank)) + 1) else NA_real_
      )
    }))
  }))
  observed0 <- observed_defn_trim[worst_fitting_events_dropped == 0L]
  centre_att <- mean(draw_scalar_att[, 1L])
  centre_rank <- mean(draw_scalar_rank[, 1L])
  pooled_rows[[defn]] <- data.table(
    decade_definition = defn, n_events = n_events,
    pooled_att = observed0$pooled_att,
    pooled_att_sd_across_events = sd(events$tau_event_effect),
    mean_percentile_rank = observed0$mean_percentile_rank,
    mean_standardized_effect = observed0$mean_standardized_effect,
    mean_rmspe_pre = mean(events$pre_rmspe),
    mean_nrmspe_pre = mean(events$nrmspe_pre),
    pooled_statistic = "mean_placebo_percentile_rank",
    randomization_draws = randomization_draws,
    randomization_mean_rank = centre_rank,
    randomization_sd_rank = sd(draw_scalar_rank[, 1L]),
    randomization_mean_att = centre_att,
    randomization_p_rank_two_sided = (1 + sum(
      abs(draw_scalar_rank[, 1L] - centre_rank) >=
        abs(observed0$mean_percentile_rank - centre_rank)
    )) / (randomization_draws + 1),
    randomization_p_rank_one_sided_positive = (1 + sum(
      draw_scalar_rank[, 1L] >= observed0$mean_percentile_rank
    )) / (randomization_draws + 1),
    randomization_p_att_two_sided = (1 + sum(
      abs(draw_scalar_att[, 1L] - centre_att) >=
        abs(observed0$pooled_att - centre_att)
    )) / (randomization_draws + 1),
    matching_specification = matching_specification,
    placebo_pool_procedure = placebo_design,
    outcome = wf_us_sd_outcome_name(),
    treated_weighting = "equal_treated_county",
    effect_estimand = "mean_e0_through_e50_gap_minus_final_pre_gap",
    event_screen = "retain_all_six_events"
  )
}

randomization_table <- rbindlist(draw_rows)
trim_inference_table <- rbindlist(trim_inference)
horizon_inference_table <- rbindlist(horizon_inference)
pooled_att <- rbindlist(pooled_rows)
trimming <- merge(
  observed_trim, trim_inference_table,
  by = c("decade_definition", "worst_fitting_events_dropped"),
  all.x = TRUE, sort = FALSE
)
dynamic_trim <- merge(
  observed_dynamic_trim, horizon_inference_table,
  by = c("decade_definition", "worst_fitting_events_dropped",
         "event_time", "reference_event_time"),
  all.x = TRUE, sort = FALSE
)
dynamic_trim[, `:=`(
  event_time_decades = event_time / 10L,
  effect_lower = pooled_effect - effect_sd_across_events,
  effect_upper = pooled_effect + effect_sd_across_events
)]
dynamic <- copy(dynamic_trim[worst_fitting_events_dropped == 0L])
dynamic[, c("worst_fitting_events_dropped", "dropped_event_names",
            "max_nrmspe_pre_retained") := NULL]
for (object_name in c("pooled_att", "trimming", "dynamic", "dynamic_trim")) {
  object <- get(object_name)
  object[, definition_order := match(
    decade_definition, c("standard", "alternative")
  )]
  ordering <- c("definition_order", intersect(
    c("worst_fitting_events_dropped", "event_time"), names(object)
  ))
  setorderv(object, ordering)
  object[, definition_order := NULL]
  assign(object_name, object)
}

post_dynamic_check <- dynamic[event_time >= treatment_start_event_time,
                              .(post_path_mean = mean(pooled_effect)),
                              by = decade_definition]
post_dynamic_check <- merge(
  post_dynamic_check,
  pooled_att[, .(decade_definition, pooled_att)],
  by = "decade_definition"
)
reference_error <- dynamic[event_time == reference_event_time,
                           max(abs(pooled_effect))]
rank_reference_ok <- dynamic[event_time == reference_event_time,
                             all(is.na(mean_percentile_rank) &
                                   is.na(rank_null_lower) &
                                   is.na(rank_null_upper))]
trim_order_ok <- trimming[, all(n_events == 6L -
  worst_fitting_events_dropped), by = decade_definition][, all(V1)]
pool_qc[, `:=`(
  donor_pool_rebuilt_for_each_placebo = TRUE,
  pseudo_identities_restricted_to_original_30 = TRUE,
  minimum_required_venue_distance_km = wf_us_sd_donor_min_distance_km()
)]
pool_exclusion_checks <- pool_pairs[, .(
  actual_treated_excluded = all(rematched_donor_GEOID != treated_GEOID),
  pseudo_treated_excluded_from_own_pool = all(
    rematched_donor_GEOID != placebo_treated_GEOID
  )
), by = event_key]
pool_qc <- merge(pool_qc, pool_exclusion_checks, by = "event_key",
                 all.x = TRUE, sort = FALSE)
pool_qc[, all_checks_pass :=
  placebo_fits_successful == donor_count & placebo_fits_failed == 0L &
    donors_per_attempt_min == donor_count &
    donors_per_attempt_max == donor_count &
    minimum_selected_venue_distance_km >=
      wf_us_sd_donor_min_distance_km() - 1e-8 &
    maximum_weight_sum_error < 1e-8 & actual_treated_excluded &
    pseudo_treated_excluded_from_own_pool]

qc <- data.table(
  check = c(
    "twelve_observed_events", "three_hundred_sixty_rematched_placebos",
    "thirty_unique_controls_per_placebo", "venue_exclusion_preserved",
    "weights_sum_to_one", "thirty_rank_statistics_per_event",
    "six_events_pooled_per_definition",
    "one_thousand_joint_draws_per_definition", "reference_period_normalized",
    "reference_rank_and_interval_omitted", "post_path_mean_matches_pooled_att",
    "progressive_trimming_six_through_two"
  ),
  passed = c(
    nrow(event_effects) == 12L,
    nrow(placebo_stats) == 360L && all(placebo_stats$successful),
    nrow(pool_pairs) == 10800L &&
      pool_pairs[, all(uniqueN(rematched_donor_GEOID) == donor_count),
                 by = .(event_key, placebo_treated_GEOID)][, all(V1)] &&
      pool_pairs[, all(placebo_treated_GEOID != rematched_donor_GEOID)],
    all(pool_qc$minimum_selected_venue_distance_km >=
          wf_us_sd_donor_min_distance_km() - 1e-8),
    all(pool_qc$maximum_weight_sum_error < 1e-8),
    placebo_stats[, all(sum(!is.na(standardized_effect)) == donor_count),
                  by = event_key][, all(V1)],
    nrow(pooled_att) == 2L && all(pooled_att$n_events == 6L),
    randomization_table[, all(.N == randomization_draws),
                        by = decade_definition][, all(V1)],
    is.finite(reference_error) && reference_error <= 1e-10,
    rank_reference_ok,
    max(abs(post_dynamic_check$post_path_mean -
              post_dynamic_check$pooled_att)) <= 1e-10,
    trim_order_ok && nrow(trimming) == 10L &&
      all(range(trimming$n_events) == c(2L, 6L))
  ),
  detail = c(
    paste(nrow(event_effects), "observed event fits"),
    paste(nrow(placebo_stats), "successful rematched placebo fits"),
    paste(nrow(pool_pairs), "event-placebo-control rows"),
    paste0("minimum ", round(min(pool_qc$minimum_selected_venue_distance_km), 3),
           " km"),
    format(max(pool_qc$maximum_weight_sum_error), scientific = TRUE),
    paste0("30 per event; ",
           placebo_stats[synthdid_noise_floor_used == TRUE, .N],
           " exact-zero-pre-fit cases use signed infinite ratios"),
    paste(pooled_att$n_events, collapse = ","),
    paste(randomization_draws, "draws each"),
    format(reference_error, scientific = TRUE),
    as.character(rank_reference_ok),
    format(max(abs(post_dynamic_check$post_path_mean -
                     post_dynamic_check$pooled_att)), scientific = TRUE),
    "drop 0 through 4; retain 6 through 2 events"
  )
)

input_manifest <- data.table(
  path = normalizePath(input_files, winslash = "/", mustWork = TRUE),
  bytes = as.numeric(file.info(input_files)$size),
  md5 = unname(tools::md5sum(input_files))
)

message("Writing pooled DZ outputs to: ", results_dir)
fwrite(event_effects, file.path(results_dir, "dz_event_effects.csv"), na = "")
fwrite(observed_paths, file.path(results_dir, "dz_event_gap_paths.csv"), na = "")
fwrite(event_horizons, file.path(results_dir, "dz_event_horizon_effects.csv"), na = "")
fwrite(placebo_stats, file.path(results_dir, "dz_placebo_events.csv"), na = "")
fwrite(placebo_paths, file.path(results_dir, "dz_placebo_gap_paths.csv"), na = "")
fwrite(pool_pairs, file.path(results_dir, "dz_rematched_placebo_pool_pairs.csv"), na = "")
fwrite(pool_qc, file.path(results_dir, "dz_rematched_placebo_pool_qc.csv"), na = "")
fwrite(donor_usage, file.path(results_dir, "dz_observed_donor_usage.csv"), na = "")
fwrite(donor_overlap, file.path(results_dir, "dz_observed_donor_overlap.csv"), na = "")
fwrite(pooled_att, file.path(results_dir, "dz_pooled_att.csv"), na = "")
fwrite(dynamic, file.path(results_dir, "dz_pooled_dynamic_effects.csv"), na = "")
fwrite(dynamic_trim, file.path(results_dir, "dz_pooled_dynamic_by_trim.csv"), na = "")
fwrite(trimming, file.path(results_dir, "dz_progressive_trimming.csv"), na = "")
fwrite(randomization_table, file.path(results_dir, "dz_randomization_draws.csv"), na = "")
fwrite(qc, file.path(results_dir, "dz_qc.csv"), na = "")
fwrite(input_manifest, file.path(results_dir, "input_manifest.csv"), na = "")
writeLines(c(
  "US county AMWS pooled Dube-Zipperer synthetic-DiD analysis",
  "Observed design: fixed e=[-30,+50], three pre-treatment and six post-treatment decades.",
  "Six treated fairs are pooled separately under standard and alternative decades.",
  "The outcome is AMWS scientists born per 1,000 county births.",
  paste0("Matching specification: ", matching_specification, "."),
  paste0("Placebo design: ", placebo_design, "."),
  "For each event, the original 30 donors supply the placebo-treated identities.",
  "Every placebo receives a newly ranked 30-control pool from the broad event-eligible never-host universe.",
  "When the selected placebo panel has zero pre-period noise, synthdid uses a 1e-8 numerical noise floor.",
  "A zero pre-RMSPE with a nonzero post effect is retained as a signed infinite standardized effect for ranking.",
  "The actual treated county and the pseudo-treated county are excluded from each placebo donor pool.",
  "All donor matching and progressive trimming use pre-treatment information only.",
  "Event time e=-10 is the normalized reference; treatment begins at e=0; the fair occurs at e=+20.",
  "The pooled effect is the equal-weighted mean; evidence is the mean event-specific placebo percentile rank.",
  paste0("Joint randomization draws per decade definition: ", randomization_draws, "."),
  "Joint assignments prohibit the same pseudo-treated county from representing two events in one draw.",
  "Progressive trimming removes the worst normalized pre-RMSPE event and stops with two events retained."
), file.path(results_dir, "notes.txt"), useBytes = TRUE)

if (any(!pool_qc$all_checks_pass) || any(!qc$passed)) {
  print(pool_qc[all_checks_pass == FALSE])
  print(qc[passed == FALSE])
  stop("US pooled DZ validation failed; inspect dz_qc.csv.")
}

message("US pooled DZ analysis complete.")
message("  Observed events: ", nrow(event_effects))
message("  Rematched placebo fits: ", nrow(placebo_stats))
message("  Joint draws: ", randomization_draws, " per decade definition")
message("  Outputs: ", results_dir)
