###############################################################################
# Estimate non-pooled synthetic-DiD case studies for the six Figure 5 US fairs.
#
# Each standard/alternative timing case uses its own fixed 30-county donor pool.
# Event paths are treated-minus-synthetic gaps normalized to e=-10, with
# treatment beginning at e=0 and the fair occurring at e=+20. In-space inference treats
# every donor once against the remaining 29 donors; the actual treated county is
# excluded from every placebo fit.
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  if (!requireNamespace("synthdid", quietly = TRUE)) {
    stop("Package 'synthdid' is required for this analysis.")
  }
  library(synthdid)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]), winslash = "/", mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", "..", ".."),
    winslash = "/", mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE
  )
}
source(file.path(repo_root, "paths.R"))
source(file.path(
  repo_root, "analysis", "world_fairs", "synthdid",
  "us_county_amws_synthdid_spec.R"
))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  candidate <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(candidate)) TALENT_DETS_DATA_DIR <- candidate
}
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  stop("Data root does not exist: ", TALENT_DETS_DATA_DIR)
}

variant <- wf_us_sd_variant_id()
panel_file <- file.path(
  TALENT_DETS_DATA_DIR, "output", "us_panel_county_amws_combined_year.csv"
)
results_root <- wf_us_sd_output_root(TALENT_DETS_DATA_DIR, variant)
donor_dir <- file.path(results_root, "donor_pools")
results_dir <- file.path(results_root, "models_amws_per_1000_births")
figure_dir <- file.path(results_dir, "figures")
fit_figure_dir <- file.path(figure_dir, "fits")
dir.create(fit_figure_dir, recursive = TRUE, showWarnings = FALSE)

donor_validation_file <- file.path(donor_dir, "run_validation.csv")
donor_pairs_file <- file.path(donor_dir, "donor_pool_pairs.csv")
donor_summary_file <- file.path(donor_dir, "donor_pool_summary.csv")
treated_manifest_file <- file.path(donor_dir, "treated_event_manifest.csv")
required_files <- c(
  panel_file, donor_validation_file, donor_pairs_file, donor_summary_file,
  treated_manifest_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing required inputs:\n", paste(missing_files, collapse = "\n"))
}

output_files <- list(
  status = file.path(results_dir, "synthdid_model_status.csv"),
  effects = file.path(results_dir, "synthdid_event_effects.csv"),
  paths = file.path(results_dir, "synthdid_event_gap_paths.csv"),
  horizon = file.path(results_dir, "synthdid_event_horizon_inference.csv"),
  placebo_effects = file.path(results_dir, "synthdid_placebo_event_effects.csv"),
  placebo_paths = file.path(results_dir, "synthdid_placebo_gap_paths.csv"),
  placebo_status = file.path(results_dir, "synthdid_placebo_status.csv"),
  control_weights = file.path(results_dir, "synthdid_control_weights.csv"),
  time_weights = file.path(results_dir, "synthdid_time_weights.csv"),
  fit_paths = file.path(results_dir, "synthdid_fit_paths.csv"),
  models = file.path(results_dir, "synthdid_models.rds"),
  validation = file.path(results_dir, "run_validation.csv"),
  manifest = file.path(results_dir, "input_manifest.csv")
)

coerce_logical <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("true", "1", "yes", "y")
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sum_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

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

fit_gap_path <- function(Y, N0, T0) {
  fit <- tryCatch(
    synthdid::synthdid_estimate(Y, N0, T0),
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
    return(list(ok = FALSE, error = "Invalid synthetic-control weights."))
  }
  if (length(lambda) != T0 || any(!is.finite(lambda)) ||
      abs(sum(lambda) - 1) > 1e-8) {
    return(list(ok = FALSE, error = "Invalid pre-treatment time weights."))
  }
  controls <- Y[seq_len(N0), , drop = FALSE]
  treated <- as.numeric(Y[nrow(Y), ])
  synthetic <- as.numeric(crossprod(omega, controls))
  gap <- treated - synthetic
  pre <- seq_len(T0)
  post <- (T0 + 1L):length(gap)
  reference_gap <- gap[[T0]]
  gap_adjusted <- gap - reference_gap
  rmspe_pre <- sqrt(mean(gap[pre]^2))
  rmspe_post <- sqrt(mean(gap[post]^2))
  mean_abs_pre_panel <- mean(abs(Y[, pre, drop = FALSE]))
  list(
    ok = TRUE,
    fit = fit,
    omega = omega,
    lambda = lambda,
    treated = treated,
    synthetic = synthetic,
    gap = gap,
    gap_adjusted = gap_adjusted,
    reference_gap = reference_gap,
    synthdid_att = as.numeric(fit),
    event_effect = mean(gap_adjusted[post]),
    pre_rmspe = rmspe_pre,
    post_rmspe = rmspe_post,
    normalized_pre_rmspe = if (
      is.finite(mean_abs_pre_panel) && mean_abs_pre_panel > 0
    ) rmspe_pre / mean_abs_pre_panel else NA_real_,
    mean_abs_pre_panel = mean_abs_pre_panel
  )
}

empty_model_result <- function(meta, error) {
  list(
    status = data.table(
      model_id = meta$model_id,
      decade_definition = meta$decade_definition,
      treated_GEOID = meta$GEOID,
      treated_county_name = meta$target_unit_name,
      fair_name = meta$first_fair_name,
      fair_year = meta$first_exposure_year,
      treatment_cohort = meta$treatment_cohort,
      model_status = "estimation_failed",
      error = error,
      n_control_units = wf_us_sd_donor_count(),
      n_treated_units = 1L,
      n_pre_periods = as.integer(meta$n_pre_periods),
      n_post_periods = as.integer(meta$n_post_periods),
      successful_placebos = 0L,
      minimum_placebos_required = 29L
    ),
    effects = data.table(), paths = data.table(), horizon = data.table(),
    placebo_effects = data.table(), placebo_paths = data.table(),
    placebo_status = data.table(), control_weights = data.table(),
    time_weights = data.table(), fit_paths = data.table(), model = NULL
  )
}

estimate_one_model <- function(index, manifest, donor_pairs, panels) {
  meta <- manifest[index]
  model_id <- meta$model_id
  definition <- meta$decade_definition
  g <- as.integer(meta$treatment_cohort)
  event_times <- seq(
    as.integer(meta$event_time_min), as.integer(meta$event_time_max), by = 10L
  )
  decades <- g + event_times
  treatment_start <- g + wf_us_sd_treatment_start_event_time()
  donors <- donor_pairs[model_id == meta$model_id][order(donor_rank)]
  if (nrow(donors) != wf_us_sd_donor_count() ||
      anyDuplicated(donors$donor_GEOID) ||
      !identical(as.integer(donors$donor_rank),
                 seq_len(wf_us_sd_donor_count()))) {
    return(empty_model_result(meta, "Invalid fixed donor pool."))
  }

  unit_ids <- c(donors$donor_GEOID, meta$GEOID)
  sample <- panels[[definition]][
    GEOID %in% unit_ids & decade %in% decades,
    .(unit_id = GEOID, decade, outcome_value)
  ]
  sample[, treatment := as.integer(
    unit_id == meta$GEOID & decade >= treatment_start
  )]
  expected_rows <- (wf_us_sd_donor_count() + 1L) * length(event_times)
  if (nrow(sample) != expected_rows || anyNA(sample) ||
      any(!is.finite(sample$outcome_value)) ||
      anyDuplicated(sample[, .(unit_id, decade)])) {
    return(empty_model_result(meta, paste0(
      "Incomplete or duplicate 31-by-", length(event_times), " model panel."
    )))
  }

  setup <- tryCatch(
    synthdid::panel.matrices(
      as.data.frame(sample), unit = "unit_id", time = "decade",
      outcome = "outcome_value", treatment = "treatment",
      treated.last = TRUE
    ),
    error = function(e) e
  )
  if (inherits(setup, "error")) {
    return(empty_model_result(meta, conditionMessage(setup)))
  }
  if (setup$N0 != wf_us_sd_donor_count() ||
      setup$T0 != as.integer(meta$n_pre_periods) ||
      nrow(setup$Y) != 31L ||
      ncol(setup$Y) != as.integer(meta$total_periods) ||
      rownames(setup$Y)[nrow(setup$Y)] != meta$GEOID) {
    return(empty_model_result(meta, "Unexpected synthdid matrix geometry."))
  }

  observed <- fit_gap_path(setup$Y, setup$N0, setup$T0)
  if (!observed$ok) return(empty_model_result(meta, observed$error))
  model_decades <- as.integer(colnames(setup$Y))
  model_event_times <- model_decades - g
  donor_matrix <- setup$Y[seq_len(setup$N0), , drop = FALSE]
  donor_ids <- rownames(donor_matrix)

  placebo_status <- vector("list", setup$N0)
  placebo_effects <- vector("list", setup$N0)
  placebo_paths <- vector("list", setup$N0)
  for (placebo_index in seq_len(setup$N0)) {
    placebo_id <- donor_ids[[placebo_index]]
    placebo_controls <- setdiff(seq_len(setup$N0), placebo_index)
    placebo_Y <- rbind(
      donor_matrix[placebo_controls, , drop = FALSE],
      donor_matrix[placebo_index, , drop = FALSE]
    )
    placebo_fit <- fit_gap_path(placebo_Y, length(placebo_controls), setup$T0)
    if (!placebo_fit$ok) {
      placebo_status[[placebo_index]] <- data.table(
        model_id = model_id,
        placebo_treated_GEOID = placebo_id,
        successful = FALSE,
        error = placebo_fit$error
      )
      next
    }
    placebo_status[[placebo_index]] <- data.table(
      model_id = model_id,
      placebo_treated_GEOID = placebo_id,
      successful = TRUE,
      error = NA_character_
    )
    placebo_effects[[placebo_index]] <- data.table(
      model_id = model_id,
      decade_definition = definition,
      treated_GEOID = meta$GEOID,
      placebo_treated_GEOID = placebo_id,
      event_effect = placebo_fit$event_effect,
      synthdid_att = placebo_fit$synthdid_att,
      pre_rmspe = placebo_fit$pre_rmspe,
      post_rmspe = placebo_fit$post_rmspe,
      normalized_pre_rmspe = placebo_fit$normalized_pre_rmspe,
      pre_rmspe_ratio_to_treated = fifelse(
        observed$pre_rmspe > 0,
        placebo_fit$pre_rmspe / observed$pre_rmspe,
        NA_real_
      )
    )
    placebo_paths[[placebo_index]] <- data.table(
      model_id = model_id,
      decade_definition = definition,
      treated_GEOID = meta$GEOID,
      placebo_treated_GEOID = placebo_id,
      decade = model_decades,
      event_time = model_event_times,
      gap = placebo_fit$gap,
      gap_adjusted = placebo_fit$gap_adjusted
    )
  }
  model_placebo_status <- rbindlist(placebo_status, fill = TRUE)
  model_placebo_effects <- rbindlist(placebo_effects, fill = TRUE)
  model_placebo_paths <- rbindlist(placebo_paths, fill = TRUE)
  successful_placebos <- model_placebo_status[successful == TRUE, .N]

  scalar_placebos <- model_placebo_effects$event_effect
  scalar_null <- as.numeric(quantile(
    scalar_placebos, probs = c(0.025, 0.975), na.rm = TRUE, type = 8
  ))

  horizon <- data.table(
    model_id = model_id,
    decade_definition = definition,
    treated_GEOID = meta$GEOID,
    treated_county_name = meta$target_unit_name,
    fair_name = meta$first_fair_name,
    fair_year = meta$first_exposure_year,
    treatment_cohort = g,
    variant = meta$variant,
    event_time_min = meta$event_time_min,
    event_time_max = meta$event_time_max,
    n_pre_periods = meta$n_pre_periods,
    n_post_periods = meta$n_post_periods,
    decade = model_decades,
    event_time = model_event_times,
    effect = observed$gap_adjusted
  )
  horizon_stats <- model_placebo_paths[, {
    q <- as.numeric(quantile(
      gap_adjusted, probs = c(0.025, 0.975), na.rm = TRUE, type = 8
    ))
    .(
      successful_placebos = .N,
      placebo_null_mean = mean(gap_adjusted),
      placebo_null_lower = q[[1L]],
      placebo_null_upper = q[[2L]]
    )
  }, by = event_time]
  horizon <- merge(horizon, horizon_stats, by = "event_time", all.x = TRUE)
  horizon[event_time == wf_us_sd_reference_event_time(), `:=`(
    placebo_null_lower = NA_real_, placebo_null_upper = NA_real_
  )]
  setorder(horizon, event_time)

  paths <- horizon[, .(
    model_id, decade_definition, treated_GEOID, treated_county_name,
    fair_name, fair_year, treatment_cohort, decade, event_time,
    period = fifelse(
      event_time < wf_us_sd_treatment_start_event_time(), "pre", "post"
    ),
    treated_outcome = observed$treated,
    synthetic_outcome = observed$synthetic,
    gap = observed$gap,
    reference_gap = observed$reference_gap,
    gap_adjusted = observed$gap_adjusted
  )]

  effects <- data.table(
    model_id = model_id,
    decade_definition = definition,
    treated_GEOID = meta$GEOID,
    treated_county_name = meta$target_unit_name,
    fair_parent_id = meta$first_parent_fair_id,
    fair_name = meta$first_fair_name,
    fair_year = meta$first_exposure_year,
    treatment_cohort = g,
    variant = meta$variant,
    event_time_min = meta$event_time_min,
    event_time_max = meta$event_time_max,
    reference_event_time = wf_us_sd_reference_event_time(),
    treatment_start_event_time = wf_us_sd_treatment_start_event_time(),
    fair_event_time = wf_us_sd_fair_event_time(),
    event_effect = observed$event_effect,
    synthdid_att_diagnostic = observed$synthdid_att,
    pre_rmspe = observed$pre_rmspe,
    post_rmspe = observed$post_rmspe,
    normalized_pre_rmspe = observed$normalized_pre_rmspe,
    mean_abs_pre_panel = observed$mean_abs_pre_panel,
    placebo_null_lower = scalar_null[[1L]],
    placebo_null_upper = scalar_null[[2L]],
    successful_placebos = successful_placebos,
    fit_restricted_placebos_ratio_le_2 = model_placebo_effects[
      pre_rmspe_ratio_to_treated <= 2, .N
    ],
    fit_restricted_placebos_ratio_le_5 = model_placebo_effects[
      pre_rmspe_ratio_to_treated <= 5, .N
    ],
    n_control_units = setup$N0,
    n_treated_units = 1L,
    n_pre_periods = setup$T0,
    n_post_periods = ncol(setup$Y) - setup$T0
  )

  control_weights <- data.table(
    model_id = model_id,
    decade_definition = definition,
    treated_GEOID = meta$GEOID,
    donor_GEOID = donor_ids,
    weight = observed$omega
  )
  control_weights <- merge(
    control_weights,
    donors[, .(donor_GEOID, donor_rank, match_distance)],
    by = "donor_GEOID", all.x = TRUE, sort = FALSE
  )
  setorder(control_weights, -weight, donor_GEOID)

  pre_decades <- model_decades[seq_len(setup$T0)]
  time_weights <- data.table(
    model_id = model_id,
    decade_definition = definition,
    treated_GEOID = meta$GEOID,
    decade = pre_decades,
    event_time = pre_decades - g,
    weight = observed$lambda
  )
  fit_paths <- paths[, .(
    model_id, decade_definition, treated_GEOID, treated_county_name,
    fair_name, fair_year, treatment_cohort, decade, event_time, period,
    treated_outcome, synthetic_outcome, gap, gap_adjusted
  )]

  status <- data.table(
    model_id = model_id,
    decade_definition = definition,
    treated_GEOID = meta$GEOID,
    treated_county_name = meta$target_unit_name,
    fair_name = meta$first_fair_name,
    fair_year = meta$first_exposure_year,
    treatment_cohort = g,
    variant = meta$variant,
    model_status = "estimated",
    error = NA_character_,
    n_control_units = setup$N0,
    n_treated_units = 1L,
    n_pre_periods = setup$T0,
    n_post_periods = ncol(setup$Y) - setup$T0,
    successful_placebos = successful_placebos,
    minimum_placebos_required = 29L
  )

  list(
    status = status,
    effects = effects,
    paths = paths,
    horizon = horizon,
    placebo_effects = model_placebo_effects,
    placebo_paths = model_placebo_paths,
    placebo_status = model_placebo_status,
    control_weights = control_weights,
    time_weights = time_weights,
    fit_paths = fit_paths,
    model = list(
      estimate = observed$fit,
      setup = setup,
      metadata = as.list(meta),
      event_effect = observed$event_effect
    )
  )
}

message("Validating donor-pool stage...")
donor_validation <- fread(donor_validation_file)
donor_validation[, passed := coerce_logical(passed)]
if (!nrow(donor_validation) || any(!donor_validation$passed)) {
  stop("Donor-pool stage failed validation; rerun the builder first.")
}

donor_pairs <- fread(donor_pairs_file, na.strings = c("", "NA"))
donor_summary <- fread(donor_summary_file, na.strings = c("", "NA"))
manifest <- fread(treated_manifest_file, na.strings = c("", "NA"))
donor_pairs[, `:=`(
  donor_GEOID = sprintf("%05d", as.integer(donor_GEOID)),
  treated_GEOID = sprintf("%05d", as.integer(treated_GEOID)),
  donor_rank = as.integer(donor_rank)
)]
manifest[, `:=`(
  GEOID = sprintf("%05d", as.integer(GEOID)),
  treatment_cohort = as.integer(treatment_cohort),
  first_exposure_year = as.integer(first_exposure_year),
  first_parent_fair_id = as.integer(first_parent_fair_id),
  event_time_min = as.integer(event_time_min),
  event_time_max = as.integer(event_time_max),
  n_pre_periods = as.integer(n_pre_periods),
  n_post_periods = as.integer(n_post_periods),
  total_periods = as.integer(total_periods)
)]
if (nrow(manifest) != 12L || uniqueN(manifest$model_id) != 12L ||
    nrow(donor_pairs) != 360L ||
    any(manifest$variant != variant) ||
    any(donor_pairs$matching_specification !=
          wf_us_sd_matching_specification())) {
  stop("Unexpected donor-pool or treated-event manifest.")
}
setorder(manifest, decade_definition, treatment_cohort, GEOID)

message("Reading standard and alternative AMWS panels...")
panel_year <- fread(
  panel_file, na.strings = c("", "NA"),
  select = c("GEOID", "year", "population", "county_births_estimate_year", "n_amws")
)
panel_year[, `:=`(
  GEOID = sprintf("%05d", as.integer(GEOID)),
  year = as.integer(year), population = as.numeric(population),
  county_births_estimate_year = as.numeric(county_births_estimate_year),
  n_amws = as.numeric(n_amws)
)]
panels <- lapply(
  c("standard", "alternative"),
  function(definition) build_decade_panel(panel_year, definition)
)
names(panels) <- c("standard", "alternative")

cores <- suppressWarnings(as.integer(Sys.getenv("WORLD_FAIRS_CORES", unset = "1")))
if (is.na(cores) || cores < 1L) cores <- 1L
cores <- min(cores, nrow(manifest))
message("Estimating 12 non-pooled models with ", cores, " worker(s)...")
indices <- seq_len(nrow(manifest))
if (cores == 1L) {
  model_results <- lapply(
    indices, estimate_one_model,
    manifest = manifest, donor_pairs = donor_pairs, panels = panels
  )
} else {
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(data.table)
      library(synthdid)
    })
    NULL
  })
  parallel::clusterExport(
    cl,
    c(
      "estimate_one_model", "empty_model_result", "fit_gap_path",
      "wf_us_sd_donor_count", "wf_us_sd_treatment_start_event_time",
      "wf_us_sd_reference_event_time", "wf_us_sd_fair_event_time"
    ),
    envir = environment()
  )
  model_results <- parallel::parLapply(
    cl, indices, estimate_one_model,
    manifest = manifest, donor_pairs = donor_pairs, panels = panels
  )
  parallel::stopCluster(cl)
  on.exit(NULL, add = FALSE)
}

bind_result <- function(name) rbindlist(
  lapply(model_results, `[[`, name), fill = TRUE, use.names = TRUE
)
status <- bind_result("status")
effects <- bind_result("effects")
paths <- bind_result("paths")
horizon <- bind_result("horizon")
placebo_effects <- bind_result("placebo_effects")
placebo_paths <- bind_result("placebo_paths")
placebo_status <- bind_result("placebo_status")
control_weights <- bind_result("control_weights")
time_weights <- bind_result("time_weights")
fit_paths <- bind_result("fit_paths")
model_objects <- lapply(model_results, `[[`, "model")
names(model_objects) <- status$model_id

setorder(status, decade_definition, treatment_cohort, treated_GEOID)
setorder(effects, decade_definition, treatment_cohort, treated_GEOID)
setorder(paths, decade_definition, treatment_cohort, treated_GEOID, event_time)
setorder(horizon, decade_definition, treatment_cohort, treated_GEOID, event_time)
setorder(placebo_effects, model_id, placebo_treated_GEOID)
setorder(placebo_paths, model_id, placebo_treated_GEOID, event_time)
setorder(placebo_status, model_id, placebo_treated_GEOID)
setorder(control_weights, model_id, -weight, donor_GEOID)
setorder(time_weights, model_id, event_time)
setorder(fit_paths, model_id, event_time)

message("Rendering non-pooled diagnostic figures...")
figure_paths <- merge(
  horizon,
  effects[, .(model_id, event_effect)],
  by = "model_id", all.x = TRUE
)
figure_paths[, facet_label := paste0(
  treated_county_name, " (", fair_year, ")\n",
  "g=", treatment_cohort, "; T0=", n_pre_periods,
  "; tau=", sprintf("%.3f", event_effect)
)]
global_y <- range(c(
  figure_paths$effect, figure_paths$placebo_null_lower,
  figure_paths$placebo_null_upper
), finite = TRUE)
global_pad <- max(diff(global_y) * 0.05, 0.01)
global_y <- global_y + c(-global_pad, global_pad)

theme_case <- theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 8.5),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, size = 8),
    plot.margin = margin(10, 12, 10, 12)
  )

for (definition in c("standard", "alternative")) {
  plot_data <- figure_paths[decade_definition == definition]
  plot_data[, facet_label := factor(
    facet_label,
    levels = unique(facet_label[order(treatment_cohort, treated_GEOID)])
  )]
  p <- ggplot(plot_data, aes(event_time, effect)) +
    geom_ribbon(
      aes(ymin = placebo_null_lower, ymax = placebo_null_upper),
      fill = "grey75", alpha = 0.7, na.rm = TRUE
    ) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.35) +
    geom_vline(
      xintercept = wf_us_sd_reference_event_time(),
      colour = "grey35", linewidth = 0.4, linetype = "longdash"
    ) +
    geom_vline(
      xintercept = wf_us_sd_treatment_start_event_time(),
      colour = "#b2182b", linewidth = 0.45, linetype = "dashed"
    ) +
    geom_vline(
      xintercept = wf_us_sd_fair_event_time(),
      colour = "#2166ac", linewidth = 0.45, linetype = "dotted"
    ) +
    geom_line(colour = "black", linewidth = 0.55) +
    geom_point(colour = "black", size = 1.35) +
    facet_wrap(~ facet_label, ncol = 2) +
    scale_x_continuous(
      breaks = wf_us_sd_plot_event_times(variant),
      limits = range(wf_us_sd_plot_event_times(variant))
    ) +
    scale_y_continuous(limits = global_y) +
    labs(
      title = paste0(
        "US world's fairs: non-pooled synthetic-DiD case studies (",
        definition, " decades)"
      ),
      subtitle = paste0(
        "AMWS scientists per 1,000 county births; 30 event-specific donors; ",
        "e=-10 normalized to zero"
      ),
      x = "Event time relative to lead-20 cohort",
      y = "Treated minus synthetic gap, relative to e=-10",
      caption = paste0(
        "Grey bands are 2.5th-97.5th percentile in-space placebo null ",
        "envelopes, not confidence intervals.\nGrey long-dash: reference ",
        "e=-10. Red dashed: treatment starts at e=0. Blue dotted: fair ",
        "at e=+20. No effects are pooled."
      )
    ) +
    theme_case
  ggsave(
    file.path(figure_dir, paste0(
      "us_amws_nonpooled_synthdid_", definition, "_event_paths.png"
    )),
    p, width = 10.5, height = 9.0, dpi = 220
  )
}

forest_data <- copy(effects)
forest_data[, case_label := paste0(
  treated_county_name, " (", fair_year, "; g=", treatment_cohort, ")"
)]
forest_data[, case_label := factor(
  case_label,
  levels = rev(unique(case_label[order(decade_definition, fair_year, treated_GEOID)]))
)]
forest_plot <- ggplot(forest_data, aes(event_effect, case_label)) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.4) +
  geom_errorbar(
    aes(xmin = placebo_null_lower, xmax = placebo_null_upper),
    width = 0.15, colour = "grey45", orientation = "y"
  ) +
  geom_point(size = 1.8, colour = "black") +
  facet_wrap(~ decade_definition, scales = "free_y") +
  labs(
    title = "US world's fairs: event-specific synthetic-DiD effects",
    subtitle = "Mean e=0 to +50 gap relative to e=-10; no pooling",
    x = "Event-specific effect (AMWS scientists per 1,000 births)", y = NULL,
    caption = "Whiskers are event-specific 2.5th-97.5th percentile placebo null envelopes, not confidence intervals."
  ) +
  theme_case
ggsave(
  file.path(figure_dir, "us_amws_nonpooled_synthdid_event_effects.png"),
  forest_plot, width = 10.2, height = 6.4, dpi = 220
)

for (mid in unique(fit_paths$model_id)) {
  fp <- fit_paths[model_id == mid]
  effect_row <- effects[model_id == mid]
  fit_long <- melt(
    fp,
    id.vars = c("event_time", "treated_county_name", "fair_year"),
    measure.vars = c("treated_outcome", "synthetic_outcome"),
    variable.name = "series", value.name = "outcome_value"
  )
  fit_long[, series := factor(
    series,
    levels = c("treated_outcome", "synthetic_outcome"),
    labels = c("Treated county", "Synthetic control")
  )]
  fit_plot <- ggplot(fit_long, aes(event_time, outcome_value, colour = series)) +
    geom_vline(
      xintercept = wf_us_sd_reference_event_time(),
      colour = "grey35", linewidth = 0.35, linetype = "longdash"
    ) +
    geom_vline(
      xintercept = wf_us_sd_treatment_start_event_time(),
      colour = "#b2182b", linewidth = 0.4, linetype = "dashed"
    ) +
    geom_vline(
      xintercept = wf_us_sd_fair_event_time(),
      colour = "#2166ac", linewidth = 0.4, linetype = "dotted"
    ) +
    geom_line(linewidth = 0.7) + geom_point(size = 1.5) +
    scale_colour_manual(values = c(
      "Treated county" = "black", "Synthetic control" = "#377eb8"
    )) +
    scale_x_continuous(
      breaks = wf_us_sd_plot_event_times(variant),
      limits = range(wf_us_sd_plot_event_times(variant))
    ) +
    labs(
      title = paste0(
        effect_row$treated_county_name, " (", effect_row$fair_year, ")"
      ),
      subtitle = paste0(
        effect_row$decade_definition, " decades; pre-RMSPE=",
        sprintf("%.4f", effect_row$pre_rmspe), "; tau=",
        sprintf("%.4f", effect_row$event_effect)
      ),
      x = "Event time relative to lead-20 cohort",
      y = "AMWS scientists per 1,000 county births",
      colour = NULL
    ) +
    theme_case + theme(legend.position = "bottom")
  ggsave(
    file.path(fit_figure_dir, paste0(mid, "_fit.png")),
    fit_plot, width = 7.4, height = 4.8, dpi = 200
  )
}

weight_sums <- control_weights[, .(weight_sum = sum(weight)), by = model_id]
time_weight_sums <- time_weights[, .(weight_sum = sum(weight)), by = model_id]
reference_error <- paths[event_time == wf_us_sd_reference_event_time(),
                         max(abs(gap_adjusted))]
gap_identity_error <- paths[, max(abs(
  gap - (treated_outcome - synthetic_outcome)
))]
effect_recomputed <- paths[period == "post", .(
  recomputed_effect = mean(gap_adjusted)
), by = model_id]
effect_recomputed <- merge(
  effect_recomputed, effects[, .(model_id, event_effect)], by = "model_id"
)
effect_error <- max(abs(
  effect_recomputed$recomputed_effect - effect_recomputed$event_effect
))
placebos_by_model <- placebo_status[, .(
  successful_placebos = sum(successful == TRUE)
), by = model_id]
expected_windows <- wf_us_sd_expected_windows(variant)
expected_windows[, model_id := wf_us_sd_model_id(
  decade_definition, GEOID, treatment_cohort
)]
geometry_check <- merge(
  status[, .(model_id, observed_n_pre = n_pre_periods,
             observed_n_post = n_post_periods)],
  expected_windows[, .(model_id, expected_n_pre = n_pre_periods,
                       expected_n_post = n_post_periods,
                       expected_total = total_periods)],
  by = "model_id", all = TRUE
)
grid_check <- horizon[, .(
  observed_total = .N,
  observed_min = min(event_time),
  observed_max = max(event_time),
  regular_decades = all(diff(event_time) == 10L)
), by = model_id]
grid_check <- merge(
  grid_check,
  expected_windows[, .(model_id, expected_total = total_periods,
                       expected_min = event_time_min,
                       expected_max = event_time_max)],
  by = "model_id", all = TRUE
)

validation <- data.table(
  check = c(
    "donor_stage_validated", "all_12_models_estimated",
    "model_matrix_dimensions", "minimum_29_placebos_per_model",
    "control_weights_sum_to_one", "time_weights_sum_to_one",
    "gap_identity", "reference_period_normalized",
    "event_effect_reproduces_post_mean", "event_time_grid_complete",
    "actual_treated_excluded_from_placebos", "horizon_null_envelope_complete"
  ),
  passed = c(
    all(donor_validation$passed),
    nrow(status) == 12L && all(status$model_status == "estimated") &&
      nrow(effects) == 12L,
    nrow(geometry_check) == 12L &&
      all(geometry_check$observed_n_pre == geometry_check$expected_n_pre &
            geometry_check$observed_n_post == geometry_check$expected_n_post &
            geometry_check$expected_total ==
              geometry_check$expected_n_pre + geometry_check$expected_n_post) &&
      all(status$n_control_units == 30L & status$n_treated_units == 1L),
    nrow(placebos_by_model) == 12L &&
      all(placebos_by_model$successful_placebos >= 29L),
    nrow(weight_sums) == 12L && max(abs(weight_sums$weight_sum - 1)) <= 1e-8,
    nrow(time_weight_sums) == 12L &&
      max(abs(time_weight_sums$weight_sum - 1)) <= 1e-8,
    is.finite(gap_identity_error) && gap_identity_error <= 1e-10,
    is.finite(reference_error) && reference_error <= 1e-10,
    is.finite(effect_error) && effect_error <= 1e-10,
    nrow(grid_check) == 12L &&
      all(grid_check$observed_total == grid_check$expected_total &
            grid_check$observed_min == grid_check$expected_min &
            grid_check$observed_max == grid_check$expected_max &
            grid_check$regular_decades),
    placebo_status[manifest[, .(model_id, actual_GEOID = GEOID)],
      on = "model_id", all(placebo_treated_GEOID != actual_GEOID)],
    horizon[event_time != wf_us_sd_reference_event_time(),
      all(is.finite(placebo_null_lower) & is.finite(placebo_null_upper))]
  ),
  detail = c(
    paste(sum(donor_validation$passed), "of", nrow(donor_validation)),
    paste(status[model_status == "estimated", .N], "of 12"),
    paste0("31 units; T0=", paste(range(status$n_pre_periods), collapse = "-"),
           " and T1=", paste(range(status$n_post_periods), collapse = "-")),
    paste(range(placebos_by_model$successful_placebos), collapse = "-"),
    format(max(abs(weight_sums$weight_sum - 1)), scientific = TRUE),
    format(max(abs(time_weight_sums$weight_sum - 1)), scientific = TRUE),
    format(gap_identity_error, scientific = TRUE),
    format(reference_error, scientific = TRUE),
    format(effect_error, scientific = TRUE),
    paste(nrow(horizon), "rows"),
    "Actual treated GEOID is absent from every in-space placebo",
    paste(horizon[event_time != wf_us_sd_reference_event_time(), .N],
          "non-reference rows")
  )
)

input_manifest <- data.table(
  path = normalizePath(required_files, winslash = "/", mustWork = TRUE),
  bytes = as.numeric(file.info(required_files)$size),
  md5 = unname(tools::md5sum(required_files))
)

fwrite(status, output_files$status, na = "")
fwrite(effects, output_files$effects, na = "")
fwrite(paths, output_files$paths, na = "")
fwrite(horizon, output_files$horizon, na = "")
fwrite(placebo_effects, output_files$placebo_effects, na = "")
fwrite(placebo_paths, output_files$placebo_paths, na = "")
fwrite(placebo_status, output_files$placebo_status, na = "")
fwrite(control_weights, output_files$control_weights, na = "")
fwrite(time_weights, output_files$time_weights, na = "")
fwrite(fit_paths, output_files$fit_paths, na = "")
saveRDS(model_objects, output_files$models)
fwrite(validation, output_files$validation, na = "")
fwrite(input_manifest, output_files$manifest, na = "")

notes <- c(
  "US county AMWS non-pooled synthetic-DiD case studies",
  paste0("Variant: ", variant, "."),
  paste0("Matching specification: ", wf_us_sd_matching_specification(), "."),
  "Twelve independent models: six Figure 5 events under each decade definition.",
  paste0("Each model uses one treated county, 30 event-specific donors, and ",
         paste(range(status$n_pre_periods), collapse = "-"),
         " pre-treatment periods plus six post-treatment periods."),
  "Event time e=-10 is the final pre-period and is normalized to zero.",
  "Treatment starts at e=0; the fair occurs at e=+20.",
  "The primary event effect is the mean adjusted gap over e=0 through e=+50.",
  "The synthdid package ATT is retained as a diagnostic, not the primary estimand.",
  "Inference treats each donor once against the other 29 donors; the actual treated county is excluded.",
  "Placebo bands in figures are null envelopes, not confidence intervals.",
  paste0("WORLD_FAIRS_CORES used: ", cores, "."),
  paste0("Results: ", normalizePath(results_dir, winslash = "/", mustWork = TRUE))
)
writeLines(notes, file.path(results_dir, "notes.txt"))

if (any(!validation$passed)) {
  print(validation)
  stop("US county AMWS synth-DiD validation failed; see ",
       output_files$validation)
}

message("US county AMWS non-pooled synthetic-DiD complete.")
message("  Models estimated: ", nrow(effects))
message("  Successful placebos per model: ",
        paste(range(placebos_by_model$successful_placebos), collapse = "--"))
message("  Figures: ", figure_dir)
message("  Outputs: ", results_dir)
