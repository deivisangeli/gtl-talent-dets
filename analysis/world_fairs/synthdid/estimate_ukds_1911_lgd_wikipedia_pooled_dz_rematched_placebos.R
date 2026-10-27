###############################################################################
# UK world fairs: pooled synthetic DiD with a freshly matched donor pool for
# every in-space placebo.
#
# The observed lead-20 treated-event fits are read from the validated baseline
# Dube-Zipperer output.  In contrast to that baseline, a never-host LGD used as
# a pseudo-treated unit is matched to its own nearest 30 never-host controls,
# after applying the same 50 km spatial exclusion around the pseudo unit.  The
# five matching variables and the balanced 3-pre/4-post window are unchanged.
#
# Outputs are deliberately isolated under pooled_dz_rematched_placebos.  This
# script never modifies the baseline pooled_dz results or project/main.tex.
#
# Optional overrides:
#   SYNTHDID_DZ_REMATCHED_CORES=8
#   SYNTHDID_DZ_RANDOMIZATION_DRAWS=1000
#   SYNTHDID_BASE_SEED=20260819
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  if (!requireNamespace("synthdid", quietly = TRUE)) {
    stop("Package 'synthdid' is required.")
  }
  library(synthdid)
  library(parallel)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_arg)) stop("Run this file with Rscript.")
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]),
                            winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."),
                           winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "paths.R"))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  candidate <- file.path("C:/Users", Sys.info()[["user"]],
                         "Globtalent Dropbox", "gtl_talent_dets")
  if (dir.exists(candidate)) TALENT_DETS_DATA_DIR <- candidate
}
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  stop("Set TALENT_DETS_DATA_DIR to the Dropbox project root.")
}

processed_dir <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "processed", "worlds_fairs",
  "ukds_1911_boundary_crosswalk"
)
root_results <- file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", "synthdid",
  "ukds_1911_lgd_wikipedia_donor_pools", "match_v4_balanced_e0"
)
baseline_dir <- file.path(root_results, "pooled_dz")
lead_dir <- file.path(root_results, "cohort_lead_20y")
results_dir <- file.path(root_results, "pooled_dz_rematched_placebos")
checkpoint_dir <- file.path(results_dir, "checkpoints")
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

panel_files <- c(
  standard = file.path(processed_dir,
    "ukds_1911_lgd_world_fairs_panel_decade_standard_1851_1961.csv"),
  alternative = file.path(processed_dir,
    "ukds_1911_lgd_world_fairs_panel_decade_alternative_1851_1961.csv")
)
geometry_file <- file.path(processed_dir,
                           "ukds_1911_lgd_analysis_geometries.gpkg")
audit_file <- file.path(lead_dir, "donor_candidate_audit.csv")
baseline_files <- c(
  event_effects = file.path(baseline_dir, "dz_event_effects.csv"),
  event_paths = file.path(baseline_dir, "dz_event_gap_paths.csv"),
  pooled_att = file.path(baseline_dir, "dz_pooled_att.csv")
)
required <- c(panel_files, geometry_file, audit_file, baseline_files)
if (any(!file.exists(required))) {
  stop("Missing required input(s): ", paste(required[!file.exists(required)],
                                             collapse = "; "))
}

donor_count <- 30L
minimum_distance_km <- 50
event_times <- c(-30L, -20L, -10L, 0L, 10L, 20L, 30L)
reference_event_time <- -10L
randomization_draws <- as.integer(Sys.getenv(
  "SYNTHDID_DZ_RANDOMIZATION_DRAWS", unset = "1000"
))
base_seed <- as.integer(Sys.getenv("SYNTHDID_BASE_SEED", unset = "20260819"))
detected <- parallel::detectCores(logical = FALSE)
if (!is.finite(detected)) detected <- 2L
cores <- as.integer(Sys.getenv(
  "SYNTHDID_DZ_REMATCHED_CORES",
  unset = as.character(max(1L, min(8L, detected - 1L)))
))
cores <- max(1L, cores)
matching_specification <- paste0(
  "equal_z_euclidean_log1p_3_balanced_pre_treatment_wikipedia_rate_lags_",
  "plus_log1p_20y_pre_treatment_population_cagr_plus_log_baseline_",
  "population_density_treatment_e0"
)
placebo_design <- paste0(
  "fresh_30_nearest_never_host_donors_per_pseudo_treated_lgd_",
  "with_50km_polygon_to_pseudo_centroid_exclusion"
)

if (!is.finite(randomization_draws) || randomization_draws < 1L) {
  stop("SYNTHDID_DZ_RANDOMIZATION_DRAWS must be positive.")
}

fit_gap_path <- function(Y, N0 = 30L, T0 = 3L) {
  fit <- tryCatch(synthdid::synthdid_estimate(Y, N0, T0),
                  error = function(e) e)
  if (inherits(fit, "error")) {
    return(list(ok = FALSE, error = conditionMessage(fit)))
  }
  weights <- attr(fit, "weights")
  omega <- as.numeric(weights$omega)
  if (length(omega) != N0 || any(!is.finite(omega)) ||
      abs(sum(omega) - 1) > 1e-8) {
    return(list(ok = FALSE, error = "Invalid donor weights."))
  }
  controls <- Y[seq_len(N0), , drop = FALSE]
  synthetic <- as.numeric(crossprod(omega, controls))
  treated <- as.numeric(Y[nrow(Y), ])
  gap <- treated - synthetic
  pre <- seq_len(T0)
  post <- (T0 + 1L):ncol(Y)
  reference_gap <- gap[[T0]]
  gap_adjusted <- gap - reference_gap
  rmspe_pre <- sqrt(mean(gap[pre]^2))
  mean_abs_pre <- mean(abs(treated[pre]))
  mean_abs_pre_panel <- mean(abs(Y[, pre, drop = FALSE]))
  list(
    ok = TRUE, error = NA_character_, omega = omega,
    treated = treated, synthetic = synthetic, gap = gap,
    gap_adjusted = gap_adjusted,
    tau = mean(gap_adjusted[post]), att_synthdid = as.numeric(fit),
    mean_post_gap = mean(gap[post]), mean_pre_gap = mean(gap[pre]),
    reference_pre_gap = reference_gap, rmspe_pre = rmspe_pre,
    mean_abs_pre = mean_abs_pre,
    mean_abs_pre_panel = mean_abs_pre_panel,
    pre_all_zero = is.finite(mean_abs_pre) && mean_abs_pre <= 0,
    nrmspe_pre = if (is.finite(mean_abs_pre_panel) && mean_abs_pre_panel > 0)
      rmspe_pre / mean_abs_pre_panel else NA_real_,
    rmspe_post = sqrt(mean(gap[post]^2)),
    post_pre_ratio = if (is.finite(rmspe_pre) && rmspe_pre > 0)
      sqrt(mean(gap[post]^2)) / rmspe_pre else NA_real_,
    standardized = if (is.finite(rmspe_pre) && rmspe_pre > 0)
      mean(gap_adjusted[post]) / rmspe_pre else NA_real_
  )
}

plus_one_p <- function(observed, null) {
  null <- null[is.finite(null)]
  if (!is.finite(observed) || !length(null)) return(NA_real_)
  (1 + sum(abs(null) >= abs(observed))) / (length(null) + 1)
}
observed_rank <- function(observed, placebos) {
  placebos <- placebos[is.finite(placebos)]
  if (!is.finite(observed) || !length(placebos)) return(NA_real_)
  (1 + sum(placebos <= observed)) / (length(placebos) + 1)
}
self_rank <- function(x) {
  good <- is.finite(x)
  out <- rep(NA_real_, length(x))
  n <- sum(good)
  if (n < 2L) return(out)
  # For unit i, compare against all other pseudo units and add the treated slot.
  out[good] <- (1 + rank(x[good], ties.method = "max") - 1) / n
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

message("Reading baseline treated-event results and decade panels...")
observed_events <- fread(baseline_files[["event_effects"]])[
  cohort_lead_years == 20L &
    model_id %chin% c("lead20_standard", "lead20_alternative")
]
observed_paths <- fread(baseline_files[["event_paths"]])[
  model_id %chin% c("lead20_standard", "lead20_alternative")
]
baseline_att <- fread(baseline_files[["pooled_att"]])[
  model_id %chin% c("lead20_standard", "lead20_alternative")
]
if (nrow(observed_events) != 8L || any(observed_events$n_donors != donor_count) ||
    any(observed_events$n_pre_periods != 3L) ||
    any(observed_events$n_post_periods != 4L)) {
  stop("Unexpected baseline lead-20 treated-event sample.")
}

panels <- lapply(panel_files, function(f) {
  x <- fread(f, na.strings = c("", "NA"))
  x[, `:=`(analysis_lgd_id = as.character(analysis_lgd_id),
           decade = as.integer(decade), population = as.numeric(population),
           n_inventors = as.numeric(n_inventors),
           n_years_in_bin = as.numeric(n_years_in_bin))]
  x[, outcome_value := 1e5 * n_inventors / (population * n_years_in_bin)]
  x[]
})
names(panels) <- names(panel_files)

audit <- fread(audit_file, na.strings = c("", "NA"))
never_lookup <- unique(audit[
  donor_treatment_status == "never_host" & valid_matching_covariates == TRUE,
  .(analysis_lgd_id = as.character(donor_lgd_id),
    lgd_name = donor_lgd_name, lgd_type = donor_lgd_type)
])
if (anyDuplicated(never_lookup$analysis_lgd_id) || nrow(never_lookup) != 1819L) {
  stop("Expected exactly 1,819 unique valid never-host LGDs.")
}
setorder(never_lookup, analysis_lgd_id)
never_ids <- never_lookup$analysis_lgd_id

message("Building pseudo-unit representative points and spatial exclusions...")
lgd_sf <- st_read(geometry_file, layer = "fixed_1911_lgds", quiet = TRUE)
lgd_sf$analysis_lgd_id <- as.character(lgd_sf$analysis_lgd_id)
lgd_sf <- lgd_sf[match(never_ids, lgd_sf$analysis_lgd_id), ]
if (anyNA(lgd_sf$analysis_lgd_id) || !identical(lgd_sf$analysis_lgd_id, never_ids)) {
  stop("Never-host LGDs could not be aligned to the fixed-1911 geometries.")
}
centres <- suppressWarnings(st_centroid(lgd_sf))
inside_matrix <- st_within(centres, lgd_sf, sparse = FALSE)
inside <- diag(inside_matrix)
if (any(!inside)) {
  centres[!inside, ] <- suppressWarnings(st_point_on_surface(lgd_sf[!inside, ]))
}
distance_cache <- file.path(checkpoint_dir, "never_host_polygon_to_pseudo_point_km.rds")
geometry_md5 <- unname(tools::md5sum(geometry_file))
distance_method <- "polygon_to_own_centroid_with_point_on_surface_fallback_v1"
if (file.exists(distance_cache)) {
  cached <- readRDS(distance_cache)
  if (identical(cached$ids, never_ids) && identical(cached$geometry_md5,
                                                   geometry_md5) &&
      identical(cached$distance_method, distance_method)) {
    distance_km <- cached$distance_km
  } else {
    distance_km <- NULL
  }
} else distance_km <- NULL
if (is.null(distance_km)) {
  distance_km <- unclass(st_distance(lgd_sf, centres)) / 1000
  saveRDS(list(ids = never_ids, geometry_md5 = geometry_md5,
               distance_method = distance_method,
               distance_km = distance_km), distance_cache, compress = FALSE)
}
if (!all(dim(distance_km) == c(length(never_ids), length(never_ids))) ||
    any(!is.finite(distance_km))) stop("Invalid spatial-distance matrix.")

build_context <- function(definition, treatment_decade) {
  panel <- panels[[definition]]
  decades <- treatment_decade + event_times
  sample <- panel[analysis_lgd_id %chin% never_ids & decade %in% decades]
  outcome_wide <- dcast(sample, analysis_lgd_id ~ decade,
                        value.var = "outcome_value")
  population_wide <- dcast(sample, analysis_lgd_id ~ decade,
                           value.var = "population")
  outcome_wide <- outcome_wide[match(never_ids, analysis_lgd_id)]
  population_wide <- population_wide[match(never_ids, analysis_lgd_id)]
  if (anyNA(outcome_wide) || anyNA(population_wide)) {
    stop("Incomplete pseudo-unit panel for ", definition, " ", treatment_decade)
  }
  outcome_matrix <- as.matrix(outcome_wide[, -"analysis_lgd_id"])
  storage.mode(outcome_matrix) <- "double"
  colnames(outcome_matrix) <- as.character(decades)
  pop_matrix <- as.matrix(population_wide[, -"analysis_lgd_id"])
  storage.mode(pop_matrix) <- "double"
  area_km2 <- as.numeric(st_area(lgd_sf)) / 1e6
  growth <- (pop_matrix[, 3L] / pop_matrix[, 1L])^(1 / 20) - 1
  density <- pop_matrix[, 3L] / area_km2
  features <- cbind(
    log1p(outcome_matrix[, 1L]), log1p(outcome_matrix[, 2L]),
    log1p(outcome_matrix[, 3L]), log1p(growth), log(density)
  )
  colnames(features) <- c(
    paste0("log1p_lag_rate_", decades[1:3]),
    "log1p_population_cagr", "log_population_density"
  )
  if (any(!is.finite(features))) {
    stop("Non-finite matching feature in ", definition, " ", treatment_decade)
  }
  list(outcome = outcome_matrix, features = features, decades = decades)
}

# Worker state is updated once per event context, avoiding repeated data copies
# inside each pseudo-treatment fit.
fit_one_pseudo <- function(pseudo_index) {
  eligible <- which(distance_km[, pseudo_index] >= minimum_distance_km)
  eligible <- eligible[eligible != pseudo_index]
  if (length(eligible) < donor_count) {
    return(list(ok = FALSE, pseudo_index = pseudo_index,
                error = "Fewer than 30 spatially eligible donors."))
  }
  donor_features <- context_features[eligible, , drop = FALSE]
  feature_mean <- colMeans(donor_features)
  feature_sd <- apply(donor_features, 2L, sd)
  usable <- is.finite(feature_mean) & is.finite(feature_sd) & feature_sd > 0 &
    is.finite(context_features[pseudo_index, ])
  if (sum(usable) < 2L || !all(usable[4:5])) {
    return(list(ok = FALSE, pseudo_index = pseudo_index,
                error = "Insufficient non-degenerate matching features."))
  }
  donor_z <- sweep(sweep(donor_features[, usable, drop = FALSE], 2L,
                         feature_mean[usable], "-"),
                   2L, feature_sd[usable], "/")
  pseudo_z <- (context_features[pseudo_index, usable] -
                 feature_mean[usable]) / feature_sd[usable]
  match_distance <- sqrt(rowSums(sweep(donor_z, 2L, pseudo_z, "-")^2))
  ord <- order(match_distance, never_ids[eligible])
  selected <- eligible[ord[seq_len(donor_count)]]
  selected_distance <- match_distance[ord[seq_len(donor_count)]]
  Y <- rbind(context_outcome[selected, , drop = FALSE],
             context_outcome[pseudo_index, , drop = FALSE])
  result <- fit_gap_path(Y, donor_count, 3L)
  if (!result$ok) {
    return(list(
      ok = FALSE, pseudo_index = pseudo_index, error = result$error,
      eligible_count = length(eligible), selected = selected,
      match_distance = selected_distance, omega = rep(NA_real_, donor_count)
    ))
  }
  list(
    ok = TRUE, pseudo_index = pseudo_index, error = NA_character_,
    eligible_count = length(eligible), selected = selected,
    match_distance = selected_distance, omega = result$omega,
    tau = result$tau, att_synthdid = result$att_synthdid,
    mean_post_gap = result$mean_post_gap,
    mean_pre_gap = result$mean_pre_gap,
    reference_pre_gap = result$reference_pre_gap,
    rmspe_pre = result$rmspe_pre,
    mean_abs_pre = result$mean_abs_pre,
    mean_abs_pre_panel = result$mean_abs_pre_panel,
    pre_all_zero = result$pre_all_zero, nrmspe_pre = result$nrmspe_pre,
    rmspe_post = result$rmspe_post, post_pre_ratio = result$post_pre_ratio,
    standardized = result$standardized,
    treated = result$treated, synthetic = result$synthetic,
    gap = result$gap, gap_adjusted = result$gap_adjusted
  )
}

event_manifest <- unique(observed_events[, .(
  model_id, cohort_lead_years, decade_definition, treated_lgd_id,
  treated_lgd_name, treated_lgd_type, treated_first_fair_year,
  effective_cohort_year, treatment_decade, treatment_start_decade,
  reference_event_time
)])
event_manifest[, event_key := paste(model_id, treated_lgd_id, sep = "__")]
setorder(event_manifest, model_id, treatment_decade, treated_lgd_id)

input_signature <- unname(tools::md5sum(c(panel_files, geometry_file,
                                         baseline_files[1:2])))
cluster <- NULL
if (cores > 1L) {
  cluster <- makeCluster(cores)
  clusterEvalQ(cluster, suppressPackageStartupMessages(library(synthdid)))
  clusterExport(cluster, c(
    "distance_km", "minimum_distance_km", "donor_count", "never_ids",
    "fit_gap_path", "fit_one_pseudo"
  ), envir = environment())
}
on.exit(if (!is.null(cluster)) stopCluster(cluster), add = TRUE)

context_outputs <- vector("list", nrow(event_manifest))
for (i in seq_len(nrow(event_manifest))) {
  event <- event_manifest[i]
  checkpoint_file <- file.path(checkpoint_dir, paste0(event$event_key, ".rds"))
  metadata <- list(
    version = 4L, input_signature = input_signature,
    event_key = event$event_key, donor_count = donor_count,
    minimum_distance_km = minimum_distance_km,
    matching_specification = matching_specification
  )
  reuse <- FALSE
  if (file.exists(checkpoint_file)) {
    saved <- readRDS(checkpoint_file)
    reuse <- identical(saved$metadata, metadata)
  }
  if (reuse) {
    message(sprintf("[%d/%d] Reusing checkpoint: %s", i,
                    nrow(event_manifest), event$event_key))
    context_outputs[[i]] <- saved$output
    next
  }
  message(sprintf("[%d/%d] Rematching and fitting 1,819 placebos: %s",
                  i, nrow(event_manifest), event$event_key))
  context <- build_context(event$decade_definition, event$treatment_decade)
  context_features <- context$features
  context_outcome <- context$outcome
  pseudo_pre_sd <- apply(context_outcome[, 1:3, drop = FALSE], 1L, sd)
  pseudo_positive_pre_variation <- is.finite(pseudo_pre_sd) & pseudo_pre_sd > 0
  pseudo_indices <- seq_along(never_ids)
  message("  attempting every valid never-host pseudo treatment: ",
          length(pseudo_indices))
  if (!is.null(cluster)) {
    clusterExport(cluster, c("context_features", "context_outcome"),
                  envir = environment())
    fits <- parLapplyLB(cluster, pseudo_indices, fit_one_pseudo)
  } else {
    fits <- lapply(pseudo_indices, fit_one_pseudo)
  }
  output <- list(event = as.list(event), decades = context$decades, fits = fits,
                 pseudo_pre_sd = pseudo_pre_sd,
                 pseudo_positive_pre_variation =
                   pseudo_positive_pre_variation)
  saveRDS(list(metadata = metadata, output = output), checkpoint_file,
          compress = FALSE)
  context_outputs[[i]] <- output
  message("  successful fits: ", sum(vapply(fits, `[[`, logical(1L), "ok")),
          "; failed: ", sum(!vapply(fits, `[[`, logical(1L), "ok")))
}
if (!is.null(cluster)) { stopCluster(cluster); cluster <- NULL }

message("Assembling rematched placebo outputs...")
placebo_stats_list <- list()
placebo_paths_list <- list()
pool_pairs_list <- list()
pool_qc_list <- list()
pseudo_audit_list <- list()
for (i in seq_along(context_outputs)) {
  out <- context_outputs[[i]]
  event <- event_manifest[i]
  fits <- out$fits
  stats <- rbindlist(lapply(fits, function(z) {
    idx <- z$pseudo_index
    if (!z$ok) return(data.table(
      event_key = event$event_key, model_id = event$model_id,
      decade_definition = event$decade_definition,
      treated_lgd_id = event$treated_lgd_id,
      treated_lgd_name = event$treated_lgd_name,
      treatment_decade = event$treatment_decade,
      placebo_lgd_id = never_ids[idx], placebo_lgd_name = never_lookup$lgd_name[idx],
      successful = FALSE, error = z$error
    ))
    data.table(
      event_key = event$event_key, model_id = event$model_id,
      cohort_lead_years = 20L, decade_definition = event$decade_definition,
      treated_lgd_id = event$treated_lgd_id,
      treated_lgd_name = event$treated_lgd_name,
      treatment_decade = event$treatment_decade,
      placebo_lgd_id = never_ids[idx], placebo_lgd_name = never_lookup$lgd_name[idx],
      placebo_lgd_type = never_lookup$lgd_type[idx], successful = TRUE,
      error = NA_character_, eligible_rematched_donors = z$eligible_count,
      tau_event_effect = z$tau, att_synthdid = z$att_synthdid,
      mean_post_gap = z$mean_post_gap, mean_pre_gap = z$mean_pre_gap,
      reference_pre_gap = z$reference_pre_gap, rmspe_pre = z$rmspe_pre,
      mean_abs_pre_outcome = z$mean_abs_pre,
      mean_abs_pre_outcome_panel = z$mean_abs_pre_panel,
      nrmspe_pre = z$nrmspe_pre, placebo_pre_all_zero = z$pre_all_zero,
      rmspe_post = z$rmspe_post,
      post_pre_rmspe_ratio = z$post_pre_ratio,
      standardized_effect = z$standardized
    )
  }), fill = TRUE, use.names = TRUE)
  paths <- rbindlist(lapply(fits, function(z) {
    if (!z$ok) return(NULL)
    idx <- z$pseudo_index
    data.table(
      event_key = event$event_key, model_id = event$model_id,
      treated_lgd_id = event$treated_lgd_id,
      treatment_decade = event$treatment_decade,
      placebo_lgd_id = never_ids[idx], placebo_lgd_name = never_lookup$lgd_name[idx],
      decade = out$decades, event_time = event_times,
      reference_event_time = reference_event_time,
      period = fifelse(event_times < 0L, "pre", "post"),
      placebo_outcome = z$treated, synthetic_outcome = z$synthetic,
      gap = z$gap, gap_adjusted = z$gap_adjusted,
      standardized_gap = if (is.finite(z$rmspe_pre) && z$rmspe_pre > 0)
        z$gap_adjusted / z$rmspe_pre else NA_real_
    )
  }), fill = TRUE, use.names = TRUE)
  pairs <- rbindlist(lapply(fits, function(z) {
    if (is.null(z$selected) || length(z$selected) != donor_count) return(NULL)
    idx <- z$pseudo_index
    data.table(
      event_key = event$event_key, model_id = event$model_id,
      decade_definition = event$decade_definition,
      treated_lgd_id = event$treated_lgd_id,
      treatment_decade = event$treatment_decade,
      placebo_lgd_id = never_ids[idx], placebo_lgd_name = never_lookup$lgd_name[idx],
      donor_rank = seq_len(donor_count), donor_lgd_id = never_ids[z$selected],
      donor_lgd_name = never_lookup$lgd_name[z$selected],
      polygon_to_pseudo_point_km = distance_km[cbind(z$selected, idx)],
      match_distance = z$match_distance, synthdid_weight = z$omega
    )
  }), fill = TRUE, use.names = TRUE)
  placebo_stats_list[[event$event_key]] <- stats
  placebo_paths_list[[event$event_key]] <- paths
  pool_pairs_list[[event$event_key]] <- pairs
  pseudo_audit_list[[event$event_key]] <- data.table(
    event_key = event$event_key, model_id = event$model_id,
    decade_definition = event$decade_definition,
    treated_lgd_id = event$treated_lgd_id,
    treated_lgd_name = event$treated_lgd_name,
    treatment_decade = event$treatment_decade,
    placebo_lgd_id = never_ids, placebo_lgd_name = never_lookup$lgd_name,
    valid_matching_covariates = TRUE,
    pseudo_pre_treatment_sd = out$pseudo_pre_sd,
    positive_pseudo_pre_treatment_variation =
      out$pseudo_positive_pre_variation
  )
  pool_qc_list[[event$event_key]] <- data.table(
    event_key = event$event_key, model_id = event$model_id,
    decade_definition = event$decade_definition,
    treated_lgd_id = event$treated_lgd_id,
    treated_lgd_name = event$treated_lgd_name,
    treatment_decade = event$treatment_decade,
    placebo_candidates = length(never_ids),
    placebo_fits_requested = length(fits),
    placebo_fits_successful = sum(stats$successful),
    placebo_fits_failed = sum(!stats$successful),
    rematched_pairs = nrow(pairs), donors_per_attempt_min = if (nrow(pairs))
      min(pairs[, .N, by = placebo_lgd_id]$N) else NA_integer_,
    donors_per_attempt_max = if (nrow(pairs))
      max(pairs[, .N, by = placebo_lgd_id]$N) else NA_integer_,
    minimum_selected_spatial_distance_km = if (nrow(pairs))
      min(pairs$polygon_to_pseudo_point_km) else NA_real_,
    maximum_donor_weight_sum_error = if (nrow(pairs)) {
      weight_sums <- pairs[is.finite(synthdid_weight),
                           .(weight_sum = sum(synthdid_weight)),
                           by = placebo_lgd_id]$weight_sum
      if (length(weight_sums)) max(abs(weight_sums - 1)) else NA_real_
    } else NA_real_
  )
}
placebo_stats <- rbindlist(placebo_stats_list, fill = TRUE, use.names = TRUE)
placebo_paths <- rbindlist(placebo_paths_list, fill = TRUE, use.names = TRUE)
pool_pairs <- rbindlist(pool_pairs_list, fill = TRUE, use.names = TRUE)
pool_qc <- rbindlist(pool_qc_list, fill = TRUE, use.names = TRUE)
pseudo_audit <- rbindlist(pseudo_audit_list, fill = TRUE, use.names = TRUE)

if (nrow(pool_pairs) != nrow(placebo_stats) * donor_count ||
    any(pool_pairs$polygon_to_pseudo_point_km < minimum_distance_km - 1e-8)) {
  stop("Rematched-placebo fit or donor-pool validation failed.")
}
pseudo_audit <- merge(
  pseudo_audit,
  placebo_stats[, .(event_key, placebo_lgd_id, fit_successful = successful,
                    fit_error = error)],
  by = c("event_key", "placebo_lgd_id"), all.x = TRUE, sort = FALSE
)
pseudo_audit[, fit_attempted := TRUE]
pseudo_audit[, exclusion_reason := fifelse(
  fit_successful == TRUE, "", "synthetic_did_solver_failure"
)]

# Event-level observed ranks use the full independently rematched placebo
# distribution for that treated-event context.
event_effects <- copy(observed_events)
event_effects[, c("n_placebos", "placebo_p_value", "percentile_rank") := NULL]
event_effects[, `:=`(n_placebos = NA_integer_, placebo_p_value = NA_real_,
                     percentile_rank = NA_real_,
                     placebo_pool_procedure = placebo_design)]
for (i in seq_len(nrow(event_effects))) {
  ev <- event_effects[i]
  p <- placebo_stats[event_key == paste(ev$model_id, ev$treated_lgd_id,
                                        sep = "__")]
  event_effects[i, `:=`(
    n_placebos = sum(is.finite(p$standardized_effect)),
    placebo_p_value = plus_one_p(ev$standardized_effect, p$standardized_effect),
    percentile_rank = observed_rank(ev$standardized_effect,
                                    p$standardized_effect)
  )]
}

event_horizons <- merge(
  observed_paths[, .(model_id, treated_lgd_id, treated_lgd_name, decade,
                     event_time, reference_event_time, period, gap,
                     gap_adjusted)],
  event_effects[, .(model_id, treated_lgd_id, rmspe_pre)],
  by = c("model_id", "treated_lgd_id"), all.x = TRUE, sort = FALSE
)
event_horizons[, `:=`(
  standardized_effect = fifelse(rmspe_pre > 0, gap_adjusted / rmspe_pre,
                                NA_real_),
  percentile_rank = NA_real_, n_placebos = NA_integer_
)]
for (i in seq_len(nrow(event_horizons))) {
  ev <- event_horizons[i]
  key <- paste(ev$model_id, ev$treated_lgd_id, sep = "__")
  vals <- placebo_paths[event_key == key & event_time == ev$event_time,
                        standardized_gap]
  event_horizons[i, n_placebos := sum(is.finite(vals))]
  if (ev$event_time != reference_event_time) {
    event_horizons[i, percentile_rank := observed_rank(
      ev$standardized_effect, vals
    )]
  }
}
setorder(event_horizons, model_id, treated_lgd_id, event_time)

# Observed trimming paths are unchanged on the effect scale; only their ranks
# and inference change because the placebo procedure changes.
event_effects[, fit_order := frank(nrmspe_pre, ties.method = "first"),
              by = model_id]
observed_trim <- rbindlist(lapply(unique(event_effects$model_id), function(mid) {
  evs <- event_effects[model_id == mid][order(nrmspe_pre)]
  rbindlist(lapply(0:(nrow(evs) - 1L), function(drop_k) {
    keep <- evs[seq_len(nrow(evs) - drop_k)]
    dropped <- if (drop_k) evs[(nrow(evs) - drop_k + 1L):nrow(evs),
                               treated_lgd_name] else character(0)
    data.table(
      model_id = mid, worst_fitting_events_dropped = drop_k,
      dropped_event_names = paste(dropped, collapse = "; "),
      n_events = nrow(keep), max_nrmspe_pre_retained = max(keep$nrmspe_pre),
      pooled_att = mean(keep$tau_event_effect),
      mean_percentile_rank = mean(keep$percentile_rank),
      mean_standardized_effect = mean(keep$standardized_effect)
    )
  }))
}))

observed_dynamic_trim <- rbindlist(lapply(unique(event_effects$model_id),
  function(mid) {
    evs <- event_effects[model_id == mid][order(nrmspe_pre)]
    rbindlist(lapply(0:(nrow(evs) - 1L), function(drop_k) {
      keep_ids <- evs[seq_len(nrow(evs) - drop_k), treated_lgd_id]
      dropped <- if (drop_k) evs[(nrow(evs) - drop_k + 1L):nrow(evs),
                                 treated_lgd_name] else character(0)
      z <- event_horizons[model_id == mid & treated_lgd_id %chin% keep_ids,
        .(reference_event_time = unique(reference_event_time),
          n_events = uniqueN(treated_lgd_id),
          pooled_effect = mean(gap_adjusted),
          effect_sd_across_events = sd(gap_adjusted),
          mean_percentile_rank = if (event_time[1L] == reference_event_time[1L])
            NA_real_ else mean(percentile_rank),
          dropped_event_names = paste(dropped, collapse = "; "),
          max_nrmspe_pre_retained = max(evs[treated_lgd_id %chin% keep_ids,
                                             nrmspe_pre])),
        by = event_time]
      z[, `:=`(model_id = mid, worst_fitting_events_dropped = drop_k)]
      z
    }))
  }))
setcolorder(observed_dynamic_trim, c(
  "model_id", "worst_fitting_events_dropped", "event_time",
  setdiff(names(observed_dynamic_trim), c("model_id",
    "worst_fitting_events_dropped", "event_time"))
))
setorder(observed_dynamic_trim, model_id, worst_fitting_events_dropped,
         event_time)

message("Running ", randomization_draws,
        " joint randomization draws for each decade definition...")
draw_rows <- list()
horizon_inference <- list()
trim_inference <- list()
pooled_rows <- list()
for (model_index in seq_along(unique(event_effects$model_id))) {
  mid <- unique(event_effects$model_id)[model_index]
  evs <- event_effects[model_id == mid][order(nrmspe_pre)]
  event_keys <- paste(evs$model_id, evs$treated_lgd_id, sep = "__")
  stats_lookup <- lapply(event_keys, function(key) {
    z <- placebo_stats[event_key == key & successful == TRUE]
    z[, self_percentile_rank := self_rank(standardized_effect)]
    setkey(z, placebo_lgd_id)
    z
  })
  names(stats_lookup) <- event_keys
  path_lookup <- lapply(event_keys, function(key) {
    z <- dcast(placebo_paths[event_key == key],
               placebo_lgd_id ~ event_time, value.var = "gap_adjusted")
    setkey(z, placebo_lgd_id)
    z
  })
  names(path_lookup) <- event_keys
  rank_path_lookup <- lapply(event_keys, function(key) {
    z <- dcast(placebo_paths[event_key == key],
               placebo_lgd_id ~ event_time, value.var = "standardized_gap")
    for (nm in setdiff(names(z), "placebo_lgd_id")) z[, (nm) := self_rank(get(nm))]
    setkey(z, placebo_lgd_id)
    z
  })
  names(rank_path_lookup) <- event_keys
  n_events <- nrow(evs)
  draw_att <- array(NA_real_, c(randomization_draws, n_events, 7L))
  draw_rank <- array(NA_real_, c(randomization_draws, n_events, 7L))
  draw_summary <- vector("list", randomization_draws)
  set.seed(base_seed + model_index * 7919L)
  for (b in seq_len(randomization_draws)) {
    universe <- unique(unlist(lapply(stats_lookup, function(z)
      z$placebo_lgd_id), use.names = FALSE))
    permutation <- sample(universe)
    position <- setNames(seq_along(permutation), permutation)
    assigned <- rep(NA_character_, n_events)
    used <- character(0)
    for (k in sample(seq_len(n_events))) {
      eligible_ids <- setdiff(stats_lookup[[event_keys[k]]]$placebo_lgd_id,
                              used)
      if (!length(eligible_ids)) next
      pick <- eligible_ids[which.min(position[eligible_ids])]
      assigned[k] <- pick
      used <- c(used, pick)
    }
    if (anyNA(assigned)) stop("Incomplete joint placebo assignment for ", mid)
    tau <- ranks <- svals <- nfit <- numeric(n_events)
    paths <- rank_paths <- matrix(NA_real_, n_events, 7L)
    for (k in seq_len(n_events)) {
      stat <- stats_lookup[[event_keys[k]]][assigned[k]]
      tau[k] <- stat$tau_event_effect
      ranks[k] <- stat$self_percentile_rank
      svals[k] <- stat$standardized_effect
      nfit[k] <- stat$nrmspe_pre
      paths[k, ] <- unlist(path_lookup[[event_keys[k]]][assigned[k], -1L],
                           use.names = FALSE)
      rank_paths[k, ] <- unlist(
        rank_path_lookup[[event_keys[k]]][assigned[k], -1L],
        use.names = FALSE
      )
    }
    ord <- order(nfit)
    for (drop_k in 0:(n_events - 1L)) {
      keep <- ord[seq_len(n_events - drop_k)]
      draw_att[b, drop_k + 1L, ] <- colMeans(paths[keep, , drop = FALSE])
      draw_rank[b, drop_k + 1L, ] <- colMeans(rank_paths[keep, , drop = FALSE],
                                             na.rm = TRUE)
    }
    draw_summary[[b]] <- data.table(
      model_id = mid, draw = b, n_assigned_events = n_events,
      assigned_placebo_lgd_ids = paste(assigned, collapse = ";"),
      mean_percentile_rank = mean(ranks), pooled_att = mean(tau),
      mean_standardized_effect = mean(svals)
    )
  }
  draw_table <- rbindlist(draw_summary)
  draw_rows[[mid]] <- draw_table
  obs_trim <- observed_trim[model_id == mid]
  trim_inference[[mid]] <- rbindlist(lapply(0:(n_events - 1L), function(drop_k) {
    obs <- obs_trim[worst_fitting_events_dropped == drop_k]
    rank_null <- apply(draw_rank[, drop_k + 1L, , drop = FALSE], 1L,
                       function(x) mean(x, na.rm = TRUE))
    # Scalar rank uses event-level post-average standardized effects, not the
    # average of horizon ranks; recover it by the same observed-fit ordering.
    scalar_rank <- vapply(seq_len(randomization_draws), function(b) {
      ids <- strsplit(draw_table$assigned_placebo_lgd_ids[b], ";",
                      fixed = TRUE)[[1L]]
      vals <- vapply(seq_len(n_events), function(k)
        stats_lookup[[event_keys[k]]][ids[k]]$self_percentile_rank, numeric(1L))
      nfits <- vapply(seq_len(n_events), function(k)
        stats_lookup[[event_keys[k]]][ids[k]]$nrmspe_pre, numeric(1L))
      keep <- order(nfits)[seq_len(n_events - drop_k)]
      mean(vals[keep])
    }, numeric(1L))
    scalar_att <- vapply(seq_len(randomization_draws), function(b) {
      ids <- strsplit(draw_table$assigned_placebo_lgd_ids[b], ";",
                      fixed = TRUE)[[1L]]
      vals <- vapply(seq_len(n_events), function(k)
        stats_lookup[[event_keys[k]]][ids[k]]$tau_event_effect, numeric(1L))
      nfits <- vapply(seq_len(n_events), function(k)
        stats_lookup[[event_keys[k]]][ids[k]]$nrmspe_pre, numeric(1L))
      mean(vals[order(nfits)[seq_len(n_events - drop_k)]])
    }, numeric(1L))
    centre_rank <- mean(scalar_rank)
    centre_att <- mean(scalar_att)
    data.table(
      model_id = mid, worst_fitting_events_dropped = drop_k,
      randomization_draws_used = randomization_draws,
      randomization_null_mean = centre_rank,
      randomization_null_mean_att = centre_att,
      randomization_p_rank = (1 + sum(abs(scalar_rank - centre_rank) >=
        abs(obs$mean_percentile_rank - centre_rank))) / (randomization_draws + 1),
      randomization_p_att = (1 + sum(abs(scalar_att - centre_att) >=
        abs(obs$pooled_att - centre_att))) / (randomization_draws + 1)
    )
  }))
  obs_dyn <- observed_dynamic_trim[model_id == mid]
  horizon_inference[[mid]] <- rbindlist(lapply(0:(n_events - 1L), function(drop_k) {
    rbindlist(lapply(seq_along(event_times), function(h) {
      obs <- obs_dyn[worst_fitting_events_dropped == drop_k &
                       event_time == event_times[h]]
      va <- draw_att[, drop_k + 1L, h]
      vr <- draw_rank[, drop_k + 1L, h]
      is_reference <- event_times[h] == reference_event_time
      ca <- mean(va); cr <- mean(vr, na.rm = TRUE)
      critical <- if (!is_reference)
        randomization_critical_value(va, ca) else NA_real_
      data.table(
        model_id = mid, worst_fitting_events_dropped = drop_k,
        event_time = event_times[h], reference_event_time = reference_event_time,
        omitted_reference_period = is_reference,
        randomization_draws_used = randomization_draws,
        null_mean = ca, null_lower = quantile(va, .025, names = FALSE),
        null_upper = quantile(va, .975, names = FALSE),
        randomization_critical_95 = critical,
        effect_ci_lower = if (!is_reference) obs$pooled_effect - ca - critical else NA_real_,
        effect_ci_upper = if (!is_reference) obs$pooled_effect - ca + critical else NA_real_,
        confidence_level = if (!is_reference) .95 else NA_real_,
        confidence_method = if (!is_reference)
          "finite_sample_joint_randomization_test_inversion_rematched_placebos" else
          "omitted_reference_period",
        randomization_p = if (!is_reference) (1 + sum(abs(va - ca) >=
          abs(obs$pooled_effect - ca))) / (randomization_draws + 1) else NA_real_,
        rank_null_mean = if (!is_reference) cr else NA_real_,
        rank_null_lower = if (!is_reference) quantile(vr, .025, names = FALSE,
                                                       na.rm = TRUE) else NA_real_,
        rank_null_upper = if (!is_reference) quantile(vr, .975, names = FALSE,
                                                       na.rm = TRUE) else NA_real_,
        rank_randomization_p = if (!is_reference) (1 + sum(abs(vr - cr) >=
          abs(obs$mean_percentile_rank - cr), na.rm = TRUE)) /
          (sum(is.finite(vr)) + 1) else NA_real_
      )
    }))
  }))
  obs0 <- obs_trim[worst_fitting_events_dropped == 0L]
  centre_rank <- mean(draw_table$mean_percentile_rank)
  centre_att <- mean(draw_table$pooled_att)
  pooled_rows[[mid]] <- data.table(
    model_id = mid, cohort_lead_years = 20L,
    decade_definition = sub("lead20_", "", mid), n_events = n_events,
    pooled_att = obs0$pooled_att,
    pooled_att_sd_across_events = sd(evs$tau_event_effect),
    pooled_att_synthdid = mean(evs$att_synthdid),
    mean_percentile_rank = obs0$mean_percentile_rank,
    mean_standardized_effect = obs0$mean_standardized_effect,
    mean_rmspe_pre = mean(evs$rmspe_pre), mean_nrmspe_pre = mean(evs$nrmspe_pre),
    pooled_statistic = "mean_placebo_percentile_rank",
    observed_mean_percentile_rank = obs0$mean_percentile_rank,
    randomization_draws = randomization_draws,
    randomization_mean = centre_rank,
    randomization_sd = sd(draw_table$mean_percentile_rank),
    randomization_centre_used = centre_rank,
    randomization_mean_att = centre_att,
    randomization_p_two_sided = (1 + sum(abs(draw_table$mean_percentile_rank -
      centre_rank) >= abs(obs0$mean_percentile_rank - centre_rank))) /
      (randomization_draws + 1),
    randomization_p_one_sided_positive = (1 + sum(
      draw_table$mean_percentile_rank >= obs0$mean_percentile_rank)) /
      (randomization_draws + 1),
    randomization_p_att_two_sided = (1 + sum(abs(draw_table$pooled_att -
      centre_att) >= abs(obs0$pooled_att - centre_att))) /
      (randomization_draws + 1),
    mean_assigned_events = n_events, incomplete_assignment_draws = 0L,
    matching_specification = matching_specification,
    placebo_pool_procedure = placebo_design,
    outcome = unique(evs$outcome), treated_weighting = "equal_treated_lgd",
    effect_estimand = "mean_post_gap_minus_final_pre_period_gap",
    event_screen = "retain_all_estimated_events"
  )
}

randomization_table <- rbindlist(draw_rows)
horizon_table <- rbindlist(horizon_inference)
trim_inference_table <- rbindlist(trim_inference)
pooled_att <- rbindlist(pooled_rows)
trimming <- merge(observed_trim, trim_inference_table,
                  by = c("model_id", "worst_fitting_events_dropped"),
                  all.x = TRUE, sort = FALSE)
dynamic_trim <- merge(
  observed_dynamic_trim, horizon_table,
  by = c("model_id", "worst_fitting_events_dropped", "event_time",
         "reference_event_time"), all.x = TRUE, sort = FALSE
)
dynamic_trim[, `:=`(
  event_time_decades = event_time / 10L,
  effect_lower = pooled_effect - effect_sd_across_events,
  effect_upper = pooled_effect + effect_sd_across_events
)]
dynamic <- dynamic_trim[worst_fitting_events_dropped == 0L]
dynamic[, c("worst_fitting_events_dropped", "dropped_event_names",
            "max_nrmspe_pre_retained") := NULL]

rematched_summary <- event_effects[, .(
  rematched_placebos_per_event_min = min(n_placebos),
  rematched_placebos_per_event_max = max(n_placebos),
  rematched_mean_rank = mean(percentile_rank)
), by = model_id]
rematched_summary <- merge(
  rematched_summary,
  pooled_att[, .(model_id, pooled_effect = pooled_att,
                 rematched_rank_p = randomization_p_two_sided,
                 rematched_att_p = randomization_p_att_two_sided)],
  by = "model_id", all.x = TRUE, sort = FALSE
)
comparison <- merge(
  baseline_att[, .(model_id, baseline_placebos_per_event = 30L,
                   baseline_mean_rank = mean_percentile_rank,
                   baseline_rank_p = randomization_p_two_sided,
                   baseline_att_p = randomization_p_att_two_sided)],
  rematched_summary, by = "model_id", all.x = TRUE, sort = FALSE
)

qc <- merge(
  pool_qc[, .(
    placebo_candidates = sum(placebo_candidates),
    placebo_fits_requested = sum(placebo_fits_requested),
    placebo_fits_successful = sum(placebo_fits_successful),
    placebo_fits_failed = sum(placebo_fits_failed),
    rematched_pairs = sum(rematched_pairs),
    donors_per_attempt_min = min(donors_per_attempt_min),
    donors_per_attempt_max = max(donors_per_attempt_max),
    minimum_selected_spatial_distance_km = min(minimum_selected_spatial_distance_km),
    maximum_donor_weight_sum_error = max(maximum_donor_weight_sum_error)
  ), by = model_id],
  pooled_att[, .(model_id, n_events, randomization_draws,
                 incomplete_assignment_draws)], by = "model_id", all.x = TRUE
)
qc[, `:=`(
  valid_never_host_candidates_per_event = 1819L,
  donor_pool_recomputed_for_each_placebo = TRUE,
  pseudo_point_centroid_fallback_count = sum(!inside),
  minimum_required_spatial_distance_km = minimum_distance_km,
  expected_rematched_pairs = placebo_fits_requested * donor_count
)]
qc[, `:=`(
  all_checks_pass = placebo_fits_successful + placebo_fits_failed ==
      placebo_fits_requested &
    donors_per_attempt_min == donor_count & donors_per_attempt_max == donor_count &
    rematched_pairs == expected_rematched_pairs &
    minimum_selected_spatial_distance_km >= minimum_distance_km - 1e-8 &
    maximum_donor_weight_sum_error < 1e-8 &
    incomplete_assignment_draws == 0L
)]
if (!all(qc$all_checks_pass)) stop("Final rematched-placebo QC failed.")

message("Writing rematched-placebo results to: ", results_dir)
fwrite(event_effects, file.path(results_dir, "dz_event_effects.csv"))
fwrite(observed_paths, file.path(results_dir, "dz_event_gap_paths.csv"))
fwrite(event_horizons, file.path(results_dir, "dz_event_horizon_effects.csv"))
fwrite(placebo_stats, file.path(results_dir, "dz_placebo_events.csv"))
fwrite(placebo_paths, file.path(results_dir, "dz_placebo_gap_paths.csv"))
fwrite(pool_pairs, file.path(results_dir, "dz_rematched_placebo_pool_pairs.csv"))
fwrite(pool_qc, file.path(results_dir, "dz_rematched_placebo_pool_qc.csv"))
fwrite(pseudo_audit, file.path(results_dir, "dz_rematched_placebo_candidate_audit.csv"))
fwrite(pooled_att, file.path(results_dir, "dz_pooled_att.csv"))
fwrite(dynamic, file.path(results_dir, "dz_pooled_dynamic_effects.csv"))
fwrite(dynamic_trim, file.path(results_dir, "dz_pooled_dynamic_by_trim.csv"))
fwrite(trimming, file.path(results_dir, "dz_progressive_trimming.csv"))
fwrite(randomization_table, file.path(results_dir, "dz_randomization_draws.csv"))
fwrite(comparison, file.path(results_dir, "dz_baseline_comparison.csv"))
fwrite(qc, file.path(results_dir, "dz_qc.csv"))
writeLines(c(
  paste0("generated_at=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("cores=", cores), paste0("randomization_draws=", randomization_draws),
  paste0("valid_never_host_candidates_per_event=", length(never_ids)),
  paste0("successful_placebos_per_event_range=",
         min(pool_qc$placebo_fits_successful), "-",
         max(pool_qc$placebo_fits_successful)),
  paste0("event_contexts=", nrow(event_manifest)),
  paste0("placebo_fits=", nrow(placebo_stats)),
  paste0("placebo_design=", placebo_design)
), file.path(results_dir, "run_manifest.txt"))
message("Done.")
