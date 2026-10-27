###############################################################################
# Estimate treated-LGD-specific synthetic-DiD models using the fixed donor
# pools built by build_ukds_1911_lgd_wikipedia_donor_pools.R.
#
# Primary outcome:
#   Annualized Wikipedia Discovery/Science births per 100,000 population
#   = 100000 * n_inventors / (population * n_years_in_bin)
#
# The standard and alternative decade definitions are estimated separately.
# A positive cohort lead moves treatment to cohorts born before the fair.
# Models with fewer than two pre-treatment bins are retained in the status
# audit but skipped because the synthdid estimator cannot fit them.
# Models with no pre-treatment outcome variation are reported as infeasible;
# they are not assigned numerical fallbacks or altered donor pools.
#
# Run from the repository root:
#   Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R
#
# Optional sensitivity overrides:
#   SYNTHDID_PLACEBO_REPLICATIONS=200
#   SYNTHDID_BASE_SEED=20260819
#   SYNTHDID_COHORT_LEAD_YEARS=0
#   SYNTHDID_TREATMENT_MODE=host
#   SYNTHDID_TREATMENT_RADIUS_KM=50
#   SYNTHDID_SHARD_COUNT=1
#   SYNTHDID_SHARD_INDEX=1
#   SYNTHDID_CHECKPOINT_ONLY=FALSE
#   SYNTHDID_MATCHING_SPEC=pooled_rate_density
#   SYNTHDID_EVENT_WINDOW=full
#
# Balanced windows use seven bins from T-30 to T+30 around the effective cohort
# decade T. The legacy window starts treatment at T+10; the corrected e0 window
# starts at T and therefore has three pre and four post periods.
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

###############################################################################
# Paths and configuration
###############################################################################

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
  "ukds_1911_lgd_matching_spec.R"
))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  candidate <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(candidate)) TALENT_DETS_DATA_DIR <- candidate
}

processed_dir <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "processed", "worlds_fairs",
  "ukds_1911_boundary_crosswalk"
)
base_donor_results_dir <- file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", "synthdid",
  "ukds_1911_lgd_wikipedia_donor_pools"
)
treatment_mode <- tolower(trimws(Sys.getenv(
  "SYNTHDID_TREATMENT_MODE", unset = "host"
)))
treatment_radius_km <- suppressWarnings(as.numeric(Sys.getenv(
  "SYNTHDID_TREATMENT_RADIUS_KM", unset = "50"
)))
if (!treatment_mode %in% c("host", "radius"))
  stop("SYNTHDID_TREATMENT_MODE must be host or radius.")
if (!is.finite(treatment_radius_km) || treatment_radius_km <= 0)
  stop("SYNTHDID_TREATMENT_RADIUS_KM must be positive.")
radius_tag <- paste0(
  format(treatment_radius_km, trim = TRUE, scientific = FALSE), "km"
)
treatment_donor_results_dir <- if (treatment_mode == "host") {
  base_donor_results_dir
} else {
  file.path(base_donor_results_dir, paste0("radius_", radius_tag))
}
cohort_lead_years <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_COHORT_LEAD_YEARS", unset = "0"
)))
if (is.na(cohort_lead_years) || cohort_lead_years < 0L)
  stop("SYNTHDID_COHORT_LEAD_YEARS must be a non-negative integer.")
matching_spec_id <- wf_matching_spec_id()
expected_matching_specification <- wf_matching_spec_string(matching_spec_id)
event_window_id <- wf_event_window_id(matching_spec_id)
event_window <- wf_event_window_spec(event_window_id)
matching_donor_results_dir <- wf_matching_spec_dir(
  treatment_donor_results_dir, matching_spec_id
)
donor_results_dir <- if (cohort_lead_years == 0L) {
  matching_donor_results_dir
} else {
  file.path(matching_donor_results_dir,
            paste0("cohort_lead_", cohort_lead_years, "y"))
}
results_dir <- file.path(donor_results_dir, "models_wikipedia_rate")
fit_figure_dir <- file.path(results_dir, "figures", "fits")
dynamic_figure_dir <- file.path(results_dir, "figures", "dynamic")

panel_files <- c(
  standard = file.path(
    processed_dir,
    "ukds_1911_lgd_world_fairs_panel_decade_standard_1851_1961.csv"
  ),
  alternative = file.path(
    processed_dir,
    "ukds_1911_lgd_world_fairs_panel_decade_alternative_1851_1961.csv"
  )
)
donor_pairs_file <- file.path(donor_results_dir, "donor_pool_pairs.csv")
donor_summary_file <- file.path(donor_results_dir, "donor_pool_summary.csv")
timing_status_file <- file.path(donor_results_dir, "cohort_timing_status.csv")

status_file <- file.path(results_dir, "synthdid_model_status.csv")
att_file <- file.path(results_dir, "synthdid_att.csv")
dynamic_file <- file.path(results_dir, "synthdid_dynamic_effects.csv")
placebo_att_file <- file.path(results_dir, "synthdid_placebo_att.csv")
placebo_dynamic_file <- file.path(results_dir, "synthdid_placebo_dynamic.csv")
placebo_status_file <- file.path(results_dir, "synthdid_placebo_status.csv")
control_weights_file <- file.path(results_dir, "synthdid_control_weights.csv")
time_weights_file <- file.path(results_dir, "synthdid_time_weights.csv")
fit_paths_file <- file.path(results_dir, "synthdid_fit_paths.csv")
models_file <- file.path(results_dir, "synthdid_models.rds")
qc_file <- file.path(results_dir, "synthdid_qc.csv")
forest_file <- file.path(results_dir, "figures", "synthdid_att_forest.png")
forest_cohort_dir <- file.path(results_dir, "figures", "forest_by_first_fair_year")
checkpoint_dir <- file.path(results_dir, "checkpoints")

required <- c(panel_files, donor_pairs_file, donor_summary_file)
if (cohort_lead_years != 0L || file.exists(timing_status_file))
  required <- c(required, timing_status_file)
missing <- required[!file.exists(required)]
if (length(missing)) stop("Missing inputs:\n", paste(missing, collapse = "\n"))
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fit_figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dynamic_figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(forest_file), recursive = TRUE, showWarnings = FALSE)
dir.create(forest_cohort_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

placebo_replications <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_PLACEBO_REPLICATIONS", unset = "200"
)))
base_seed <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_BASE_SEED", unset = "20260819"
)))
if (is.na(placebo_replications) || placebo_replications <= 0L)
  stop("SYNTHDID_PLACEBO_REPLICATIONS must be a positive integer.")
if (is.na(base_seed) || base_seed <= 0L)
  stop("SYNTHDID_BASE_SEED must be a positive integer.")
shard_count <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_SHARD_COUNT", unset = "1"
)))
shard_index <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_SHARD_INDEX", unset = "1"
)))
checkpoint_only <- tolower(Sys.getenv(
  "SYNTHDID_CHECKPOINT_ONLY", unset = "false"
)) %in% c("1", "true", "yes", "y")
if (is.na(shard_count) || shard_count < 1L || is.na(shard_index) ||
    shard_index < 1L || shard_index > shard_count)
  stop("Invalid SYNTHDID_SHARD_COUNT/SYNTHDID_SHARD_INDEX configuration.")
if (shard_count > 1L && !checkpoint_only)
  stop("Multi-process shards must use SYNTHDID_CHECKPOINT_ONLY=TRUE.")

expected_donors <- 30L
candidate_control_status <- if (treatment_mode == "radius") {
  "never_within_radius"
} else {
  "never_host"
}
minimum_successful_dynamic_placebos <- ceiling(0.80 * placebo_replications)
tolerance <- 1e-8
outcome_name <- "discovery_science_births_per_100k_population_year"

###############################################################################
# Helpers
###############################################################################

model_slug <- function(definition, name, treated_id) {
  prefix <- if (cohort_lead_years == 0L) "" else
    paste0("lead", cohort_lead_years, "_")
  radius_prefix <- if (treatment_mode == "radius") paste0(
    "radius", radius_tag, "_"
  ) else ""
  id_suffix <- if (treatment_mode == "radius") paste0("_", treated_id) else ""
  x <- tolower(paste0(
    radius_prefix, prefix, definition, "_", name, id_suffix
  ))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

extract_weights <- function(estimate, weight_type, id_name) {
  raw <- synthdid::synthdid_controls(
    estimate, mass = 1, weight.type = weight_type
  )
  out <- as.data.table(as.data.frame(raw), keep.rownames = id_name)
  setnames(out, setdiff(names(out), id_name)[[1L]], "weight")
  out[, weight := as.numeric(weight)]
  out
}

make_status_row <- function(model_meta, status, error = NA_character_,
                            noise_level = NA_real_, pre_distinct = NA_integer_,
                            pre_control_distinct = NA_integer_,
                            n0 = NA_integer_, n1 = NA_integer_,
                            t0 = NA_integer_, t1 = NA_integer_,
                            dynamic_placebos_successful = NA_integer_,
                            att_se_available = FALSE) {
  data.table(
    model_id = model_meta$model_id,
    treatment_mode = treatment_mode,
    treatment_radius_km = if (treatment_mode == "radius")
      treatment_radius_km else NA_real_,
    decade_definition = model_meta$decade_definition,
    treated_lgd_id = model_meta$treated_lgd_id,
    treated_lgd_name = model_meta$treated_lgd_name,
    treated_lgd_type = model_meta$treated_lgd_type,
    treated_first_fair_year = model_meta$treated_first_fair_year,
    fair_treatment_decade = model_meta$fair_treatment_decade,
    cohort_lead_years = model_meta$cohort_lead_years,
    effective_cohort_year = model_meta$effective_cohort_year,
    treatment_decade = model_meta$treatment_decade,
    baseline_decade = model_meta$baseline_decade,
    matching_specification = if (
      "matching_specification" %in% names(model_meta)
    ) model_meta$matching_specification else NA_character_,
    outcome = outcome_name,
    model_status = status,
    error = error,
    n_control_units = n0,
    n_treated_units = n1,
    n_pre_periods = t0,
    n_post_periods = t1,
    min_decade = 1850L,
    max_decade = 1960L,
    raw_preperiod_noise_level = noise_level,
    preperiod_distinct_values_all_units = pre_distinct,
    preperiod_distinct_values_controls = pre_control_distinct,
    placebo_replications_requested = placebo_replications,
    dynamic_placebos_successful = dynamic_placebos_successful,
    minimum_dynamic_placebos_required = minimum_successful_dynamic_placebos,
    dynamic_inference_complete = !is.na(dynamic_placebos_successful) &
      dynamic_placebos_successful >= minimum_successful_dynamic_placebos,
    att_se_available = att_se_available,
    model_seed = model_meta$model_seed,
    thirtieth_match_distance = if (
      "thirtieth_match_distance" %in% names(model_meta)
    ) model_meta$thirtieth_match_distance else NA_real_,
    selected_donor_population_ratio_median =
      if ("selected_donor_population_ratio_median" %in% names(model_meta))
        model_meta$selected_donor_population_ratio_median else NA_real_,
    selected_donor_population_ratio_max =
      if ("selected_donor_population_ratio_max" %in% names(model_meta))
        model_meta$selected_donor_population_ratio_max else NA_real_
  )
}

###############################################################################
# Inputs and model manifest
###############################################################################

message("Matching specification: ", matching_spec_id)
message("Event window: ", event_window_id)
message("Reading donor pools and fixed-LGD decade panels...")
donor_pairs <- fread(donor_pairs_file, na.strings = c("", "NA"))
donor_summary <- fread(donor_summary_file, na.strings = c("", "NA"))
if (!"matching_specification" %in% names(donor_summary)) {
  donor_summary[, matching_specification :=
    "legacy_log1p_cumulative_scientists_plus_log_density"]
}
matching_specifications <- unique(donor_summary$matching_specification)
if (length(matching_specifications) != 1L ||
    is.na(matching_specifications) || !nzchar(matching_specifications)) {
  stop("Donor summaries must identify exactly one matching specification.")
}
matching_specification <- matching_specifications[[1L]]
if (!identical(matching_specification, expected_matching_specification)) {
  stop("Donor pools were built with matching specification\n  ",
       matching_specification, "\nbut SYNTHDID_MATCHING_SPEC requests\n  ",
       expected_matching_specification,
       "\nRebuild the donor pools before estimating.")
}
timing_manifest <- if (file.exists(timing_status_file)) {
  fread(timing_status_file, na.strings = c("", "NA"))
} else {
  # Backward-compatible manifest for the original zero-lead donor outputs.
  unique(donor_pairs[, .(
    decade_definition,
    treated_lgd_id,
    treated_lgd_name,
    treated_lgd_type,
    treated_first_fair_year,
    fair_treatment_decade = treatment_decade,
    cohort_lead_years = 0L,
    effective_cohort_year = treated_first_fair_year,
    treatment_decade,
    baseline_decade,
    available_pre_periods = as.integer((treatment_decade - 1850L) / 10L),
    minimum_pre_periods_required = 2L,
    model_eligible = TRUE,
    timing_status = "donor_pool_built"
  )])
}

expected_treated_lgds <- uniqueN(timing_manifest$treated_lgd_id)
expected_requested_models <- 2L * expected_treated_lgds
if (nrow(timing_manifest) != expected_requested_models ||
    anyDuplicated(timing_manifest[, .(decade_definition, treated_lgd_id)]) ||
    !setequal(timing_manifest$decade_definition,
              c("standard", "alternative"))) {
  stop("The cohort timing manifest is not one row per LGD and definition.")
}
if (any(timing_manifest$cohort_lead_years != cohort_lead_years))
  stop("Cohort lead in timing manifest does not match the requested lead.")
if ("treatment_mode" %in% names(timing_manifest) &&
    any(timing_manifest$treatment_mode != treatment_mode))
  stop("Treatment mode in timing manifest does not match the request.")
if ("matching_specification" %in% names(timing_manifest) &&
    any(timing_manifest$matching_specification != matching_specification)) {
  stop("Matching specification differs between timing and donor manifests.")
}
if (treatment_mode == "radius" &&
    "treatment_radius_km" %in% names(timing_manifest) &&
    any(abs(timing_manifest$treatment_radius_km - treatment_radius_km) >
          tolerance)) {
  stop("Treatment radius in timing manifest does not match the request.")
}

model_period_count <- if (event_window$balanced)
  event_window$n_pre_periods + event_window$n_post_periods else 12L
expected_eligible_models <- timing_manifest[model_eligible == TRUE, .N]
expected_skipped_models <- expected_requested_models - expected_eligible_models
if (event_window$balanced) {
  if (treatment_mode == "host") {
    expected_balanced_models <- if (cohort_lead_years == 0L) 19L else
      if (cohort_lead_years == 20L) 10L else NA_integer_
    if (!is.na(expected_balanced_models) &&
        expected_eligible_models != expected_balanced_models)
      stop("The balanced host manifest should contain ",
           expected_balanced_models, " eligible models at a ",
           cohort_lead_years, "-year lead, not ", expected_eligible_models, ".")
  }
} else {
  if (cohort_lead_years == 0L &&
      expected_eligible_models != expected_requested_models)
    stop("Every zero-lead model should be timing-eligible.")
  if (treatment_mode == "host" && cohort_lead_years == 20L &&
      expected_eligible_models != 15L)
    stop("The host 20-year lead manifest should contain 15 eligible models.")
  if (treatment_mode == "radius" && treatment_radius_km == 50 &&
      cohort_lead_years == 20L && expected_eligible_models != 403L)
    stop("The 50 km 20-year lead manifest should contain 403 eligible models.")
}

if (nrow(donor_pairs) != expected_eligible_models * expected_donors ||
    uniqueN(donor_pairs[, .(decade_definition, treated_lgd_id)]) !=
      expected_eligible_models ||
    donor_pairs[, any(.N != expected_donors),
                by = .(decade_definition, treated_lgd_id)]$V1 |> any()) {
  stop("The selected donor-pair input does not contain 30 donors per eligible model.")
}

setorder(timing_manifest, decade_definition, treatment_decade, treated_lgd_id)
timing_manifest[, model_number := seq_len(.N)]
timing_manifest[, model_id := model_slug(
  decade_definition, treated_lgd_name, treated_lgd_id
)]
timing_manifest[, model_seed := base_seed + model_number * 1000L]
if (anyDuplicated(timing_manifest$model_id)) stop("Duplicate model IDs.")

model_manifest <- merge(
  timing_manifest[model_eligible == TRUE],
  donor_summary[, .(
    decade_definition, treated_lgd_id, matching_specification,
    thirtieth_match_distance,
    selected_donor_population_ratio_median,
    selected_donor_population_ratio_max
  )],
  by = c("decade_definition", "treated_lgd_id"), all.x = TRUE,
  sort = FALSE
)
setorder(model_manifest, decade_definition, treatment_decade, treated_lgd_id)
if (nrow(model_manifest) != expected_eligible_models ||
    anyDuplicated(model_manifest$model_id)) {
  stop("Unexpected model manifest.")
}
run_model_manifest <- model_manifest[
  ((model_number - 1L) %% shard_count) + 1L == shard_index
]
if (!nrow(run_model_manifest))
  stop("The requested shard contains no eligible models.")

panel_inputs <- lapply(names(panel_files), function(definition) {
  x <- fread(panel_files[[definition]], na.strings = c("", "NA"))
  x[, `:=`(
    analysis_lgd_id = as.character(analysis_lgd_id),
    decade = as.integer(decade),
    n_inventors = as.numeric(n_inventors),
    population = as.numeric(population),
    n_years_in_bin = as.integer(n_years_in_bin)
  )]
  x[, outcome_value :=
    1e5 * n_inventors / (population * n_years_in_bin)]
  if (any(!is.finite(x$outcome_value)) || any(x$n_years_in_bin <= 0L))
    stop("Invalid annualized Wikipedia outcome in ", definition, " panel.")
  x
})
names(panel_inputs) <- names(panel_files)

input_md5 <- tools::md5sum(c(
  donor_pairs_file, donor_summary_file, timing_status_file, panel_files
))
run_fingerprint <- paste(
  treatment_mode,
  if (treatment_mode == "radius") treatment_radius_km else "host",
  cohort_lead_years, placebo_replications, base_seed,
  paste(names(input_md5), unname(input_md5), collapse = "|"),
  sep = "::"
)

###############################################################################
# Estimate each treated-LGD model
###############################################################################

model_status <- list()
att_results <- list()
dynamic_results <- list()
placebo_att_results <- list()
placebo_dynamic_results <- list()
placebo_status_results <- list()
control_weight_results <- list()
time_weight_results <- list()
fit_path_results <- list()
model_objects <- list()

skipped_manifest <- timing_manifest[model_eligible == FALSE]
if (nrow(skipped_manifest)) {
  for (skipped_index in seq_len(nrow(skipped_manifest))) {
    skipped_meta <- skipped_manifest[skipped_index]
    skipped_error <- if (
      skipped_meta$timing_status == "skipped_no_pre_period"
    ) {
      "No pre-treatment decade is available after applying the cohort lead."
    } else {
      "Only one pre-treatment decade is available; synthdid requires at least two."
    }
    model_status[[skipped_meta$model_id]] <- make_status_row(
      skipped_meta,
      status = skipped_meta$timing_status,
      error = skipped_error,
      n0 = 0L,
      n1 = 1L,
      t0 = skipped_meta$available_pre_periods,
      t1 = model_period_count - skipped_meta$available_pre_periods
    )
  }
}

for (model_index in seq_len(nrow(run_model_manifest))) {
  meta <- run_model_manifest[model_index]
  definition <- meta$decade_definition
  treated_id <- meta$treated_lgd_id
  checkpoint_file <- file.path(
    checkpoint_dir, paste0(meta$model_id, ".rds")
  )
  if (file.exists(checkpoint_file)) {
    checkpoint <- tryCatch(readRDS(checkpoint_file), error = function(e) NULL)
    if (!is.null(checkpoint) &&
        identical(checkpoint$run_fingerprint, run_fingerprint) &&
        identical(checkpoint$model_id, meta$model_id)) {
      model_status[[meta$model_id]] <- checkpoint$status
      att_results[[meta$model_id]] <- checkpoint$att
      dynamic_results[[meta$model_id]] <- checkpoint$dynamic
      placebo_att_results[[meta$model_id]] <- checkpoint$placebo_att
      placebo_dynamic_results[[meta$model_id]] <- checkpoint$placebo_dynamic
      placebo_status_results[[meta$model_id]] <- checkpoint$placebo_status
      control_weight_results[[meta$model_id]] <- checkpoint$control_weights
      time_weight_results[[meta$model_id]] <- checkpoint$time_weights
      fit_path_results[[meta$model_id]] <- checkpoint$fit_path
      model_objects[[meta$model_id]] <- checkpoint$model_object
      message("Resumed ", meta$model_id, " from checkpoint (",
              model_index, "/", nrow(run_model_manifest), ").")
      next
    }
  }
  donors <- donor_pairs[
    decade_definition == definition & treated_lgd_id == treated_id
  ][order(donor_rank)]
  if (nrow(donors) != expected_donors ||
      any(donors$donor_treatment_status != candidate_control_status)) {
    stop("Invalid donor pool for model ", meta$model_id, ".")
  }

  panel <- panel_inputs[[definition]]
  unit_ids <- c(donors$donor_lgd_id, treated_id)
  sample <- panel[analysis_lgd_id %in% unit_ids, .(
    unit_id = analysis_lgd_id,
    unit_name = population_lgd_name,
    decade,
    n_inventors,
    population,
    n_years_in_bin,
    outcome_value
  )]
  # The balanced window keeps only the seven bins around the fair decade and
  # treats the fair decade itself as the last pre-treatment period.
  window_decades <- wf_event_window_decades(meta$treatment_decade, event_window)
  treatment_start <- wf_event_window_treatment_start(
    meta$treatment_decade, event_window
  )
  if (!is.null(window_decades)) {
    if (!all(window_decades %in% sample$decade))
      stop("The balanced window is not fully observed for ", meta$model_id, ".")
    sample <- sample[decade %in% window_decades]
  }
  sample[, treatment := as.integer(
    unit_id == treated_id & decade >= treatment_start
  )]
  expected_periods <- if (is.null(window_decades)) 12L else
    length(window_decades)
  if (nrow(sample) != (expected_donors + 1L) * expected_periods ||
      uniqueN(sample$unit_id) != expected_donors + 1L ||
      any(!is.finite(sample$outcome_value))) {
    stop("Incomplete model sample for ", meta$model_id, ".")
  }
  identity_error <- sample[, max(abs(
    outcome_value - 1e5 * n_inventors / (population * n_years_in_bin)
  ))]
  if (identity_error > tolerance)
    stop("Annualized outcome identity failed for ", meta$model_id, ".")

  setup <- synthdid::panel.matrices(
    as.data.frame(sample[, .(unit_id, decade, outcome_value, treatment)]),
    unit = "unit_id", time = "decade", outcome = "outcome_value",
    treatment = "treatment", treated.last = TRUE
  )
  time_periods <- as.integer(colnames(setup$Y))
  pre_periods <- time_periods[seq_len(setup$T0)]
  post_periods <- time_periods[(setup$T0 + 1L):length(time_periods)]
  if (setup$N0 != expected_donors || nrow(setup$Y) - setup$N0 != 1L ||
      setup$T0 != sum(time_periods < treatment_start)) {
    stop("Unexpected synthdid matrix layout for ", meta$model_id, ".")
  }
  if (event_window$balanced &&
      (setup$T0 != event_window$n_pre_periods ||
       length(time_periods) - setup$T0 != event_window$n_post_periods)) {
    stop("The balanced window must give ", event_window$n_pre_periods,
         " pre and ", event_window$n_post_periods, " post periods for ",
         meta$model_id, ".")
  }

  pre_all <- setup$Y[, seq_len(setup$T0), drop = FALSE]
  pre_controls <- setup$Y[seq_len(setup$N0), seq_len(setup$T0), drop = FALSE]
  raw_noise <- sd(apply(pre_controls, 1L, diff))
  pre_distinct <- uniqueN(as.vector(pre_all))
  pre_control_distinct <- uniqueN(as.vector(pre_controls))

  if (!is.finite(raw_noise) || raw_noise <= tolerance || pre_distinct <= 1L) {
    model_status[[meta$model_id]] <- make_status_row(
      meta,
      status = "infeasible_zero_preperiod_variation",
      error = "Treated unit and selected donors have no informative pre-treatment outcome variation.",
      noise_level = raw_noise,
      pre_distinct = pre_distinct,
      pre_control_distinct = pre_control_distinct,
      n0 = setup$N0,
      n1 = nrow(setup$Y) - setup$N0,
      t0 = setup$T0,
      t1 = ncol(setup$Y) - setup$T0
    )
    next
  }

  message("Estimating ", definition, " model for ", meta$treated_lgd_name,
          " (", model_index, "/", nrow(run_model_manifest), "; shard ",
          shard_index, "/", shard_count, ")...")
  fit <- tryCatch(
    synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    model_status[[meta$model_id]] <- make_status_row(
      meta, status = "estimation_failed", error = conditionMessage(fit),
      noise_level = raw_noise, pre_distinct = pre_distinct,
      pre_control_distinct = pre_control_distinct,
      n0 = setup$N0, n1 = 1L, t0 = setup$T0,
      t1 = ncol(setup$Y) - setup$T0
    )
    next
  }

  set.seed(meta$model_seed + 1L)
  att_se <- tryCatch(
    sqrt(as.numeric(vcov(
      fit, method = "placebo", replications = placebo_replications
    ))),
    error = function(e) NA_real_
  )
  att <- as.numeric(fit)

  observed_curve <- as.numeric(synthdid::synthdid_effect_curve(fit))
  if (length(observed_curve) != length(post_periods)) {
    model_status[[meta$model_id]] <- make_status_row(
      meta, status = "estimation_failed",
      error = "Observed effect-curve length does not match post-treatment periods.",
      noise_level = raw_noise, pre_distinct = pre_distinct,
      pre_control_distinct = pre_control_distinct,
      n0 = setup$N0, n1 = 1L, t0 = setup$T0,
      t1 = ncol(setup$Y) - setup$T0,
      att_se_available = is.finite(att_se)
    )
    next
  }

  omega <- extract_weights(fit, "omega", "donor_lgd_id")
  lambda <- extract_weights(fit, "lambda", "pre_period")
  lambda[, pre_period := as.integer(pre_period)]

  # synthdid_controls() reports only the requested positive mass. Restore the
  # complete donor/pre-period support so omitted entries are explicit zeros.
  omega <- merge(
    data.table(donor_lgd_id = rownames(setup$Y)[seq_len(setup$N0)]),
    omega,
    by = "donor_lgd_id",
    all.x = TRUE,
    sort = FALSE
  )
  omega[is.na(weight), weight := 0]
  lambda <- merge(
    data.table(pre_period = pre_periods),
    lambda,
    by = "pre_period",
    all.x = TRUE,
    sort = FALSE
  )
  lambda[is.na(weight), weight := 0]
  if (abs(sum(omega$weight) - 1) > tolerance ||
      abs(sum(lambda$weight) - 1) > tolerance) {
    stop("Synthdid weights do not sum to one for ", meta$model_id, ".")
  }

  donor_matrix <- setup$Y[seq_len(setup$N0), , drop = FALSE]
  omega_lookup <- setNames(omega$weight, omega$donor_lgd_id)
  ordered_omega <- omega_lookup[rownames(donor_matrix)]
  if (anyNA(ordered_omega))
    stop("Cannot align donor weights for ", meta$model_id, ".")
  synthetic_path <- as.numeric(crossprod(ordered_omega, donor_matrix))
  treated_path <- as.numeric(setup$Y[nrow(setup$Y), ])
  gap_path <- treated_path - synthetic_path
  pre_rmspe <- sqrt(mean(gap_path[seq_len(setup$T0)]^2))
  post_mean_gap <- mean(gap_path[(setup$T0 + 1L):length(gap_path)])

  placebo_att <- list()
  placebo_dynamic <- list()
  placebo_status <- list()
  control_index <- seq_len(setup$N0)
  for (rep_id in seq_len(placebo_replications)) {
    rep_seed <- meta$model_seed + 10000L + rep_id
    set.seed(rep_seed)
    placebo_treated <- sample(control_index, 1L)
    placebo_controls <- setdiff(control_index, placebo_treated)
    y_placebo <- rbind(
      donor_matrix[placebo_controls, , drop = FALSE],
      donor_matrix[placebo_treated, , drop = FALSE]
    )
    placebo_fit <- tryCatch(
      synthdid::synthdid_estimate(
        y_placebo, length(placebo_controls), setup$T0
      ),
      error = function(e) e
    )
    if (inherits(placebo_fit, "error")) {
      placebo_status[[rep_id]] <- data.table(
        model_id = meta$model_id, placebo_replication = rep_id,
        placebo_seed = rep_seed, successful = FALSE,
        error = conditionMessage(placebo_fit)
      )
      next
    }
    placebo_curve <- as.numeric(
      synthdid::synthdid_effect_curve(placebo_fit)
    )
    if (length(placebo_curve) != length(post_periods)) {
      placebo_status[[rep_id]] <- data.table(
        model_id = meta$model_id, placebo_replication = rep_id,
        placebo_seed = rep_seed, successful = FALSE,
        error = "Placebo curve length mismatch."
      )
      next
    }
    placebo_status[[rep_id]] <- data.table(
      model_id = meta$model_id, placebo_replication = rep_id,
      placebo_seed = rep_seed, successful = TRUE, error = NA_character_
    )
    placebo_att[[rep_id]] <- data.table(
      model_id = meta$model_id,
      decade_definition = definition,
      treated_lgd_id = treated_id,
      treated_lgd_name = meta$treated_lgd_name,
      cohort_lead_years = cohort_lead_years,
      effective_cohort_year = meta$effective_cohort_year,
      treatment_decade = meta$treatment_decade,
      placebo_replication = rep_id,
      placebo_treated_lgd_id = rownames(donor_matrix)[placebo_treated],
      placebo_att = as.numeric(placebo_fit)
    )
    placebo_dynamic[[rep_id]] <- data.table(
      model_id = meta$model_id,
      decade_definition = definition,
      treated_lgd_id = treated_id,
      treated_lgd_name = meta$treated_lgd_name,
      cohort_lead_years = cohort_lead_years,
      effective_cohort_year = meta$effective_cohort_year,
      treatment_decade = meta$treatment_decade,
      placebo_replication = rep_id,
      decade = post_periods,
      event_time = post_periods - meta$treatment_decade,
      event_time_decades = (post_periods - meta$treatment_decade) / 10,
      placebo_effect = placebo_curve
    )
  }
  model_placebo_status <- rbindlist(placebo_status, fill = TRUE)
  model_placebo_att <- rbindlist(placebo_att, fill = TRUE)
  model_placebo_dynamic <- rbindlist(placebo_dynamic, fill = TRUE)
  successful_placebos <- model_placebo_status[successful == TRUE, .N]
  dynamic_inference_complete <-
    successful_placebos >= minimum_successful_dynamic_placebos

  dynamic <- data.table(
    model_id = meta$model_id,
    decade_definition = definition,
    treated_lgd_id = treated_id,
    treated_lgd_name = meta$treated_lgd_name,
    treated_lgd_type = meta$treated_lgd_type,
    treated_first_fair_year = meta$treated_first_fair_year,
    fair_treatment_decade = meta$fair_treatment_decade,
    cohort_lead_years = cohort_lead_years,
    effective_cohort_year = meta$effective_cohort_year,
    treatment_decade = meta$treatment_decade,
    outcome = outcome_name,
    decade = post_periods,
    event_time = post_periods - meta$treatment_decade,
    event_time_decades = (post_periods - meta$treatment_decade) / 10,
    effect = observed_curve
  )
  if (dynamic_inference_complete) {
    curve_se <- model_placebo_dynamic[, .(
      dynamic_placebo_se = sd(placebo_effect),
      successful_placebo_replications = uniqueN(placebo_replication)
    ), by = .(decade, event_time, event_time_decades)]
    dynamic <- merge(
      dynamic, curve_se,
      by = c("decade", "event_time", "event_time_decades"),
      all.x = TRUE, sort = FALSE
    )
    dynamic[, `:=`(
      ci_low = effect - 1.96 * dynamic_placebo_se,
      ci_high = effect + 1.96 * dynamic_placebo_se,
      dynamic_inference_complete = TRUE
    )]
  } else {
    dynamic[, `:=`(
      dynamic_placebo_se = NA_real_,
      successful_placebo_replications = successful_placebos,
      ci_low = NA_real_, ci_high = NA_real_,
      dynamic_inference_complete = FALSE
    )]
  }
  setorder(dynamic, decade)

  att_results[[meta$model_id]] <- data.table(
    model_id = meta$model_id,
    decade_definition = definition,
    treated_lgd_id = treated_id,
    treated_lgd_name = meta$treated_lgd_name,
    treated_lgd_type = meta$treated_lgd_type,
    treated_first_fair_year = meta$treated_first_fair_year,
    fair_treatment_decade = meta$fair_treatment_decade,
    cohort_lead_years = cohort_lead_years,
    effective_cohort_year = meta$effective_cohort_year,
    treatment_decade = meta$treatment_decade,
    outcome = outcome_name,
    matching_specification = meta$matching_specification,
    estimate = att,
    se = att_se,
    ci_low = att - 1.96 * att_se,
    ci_high = att + 1.96 * att_se,
    p_value = ifelse(
      is.finite(att_se) && att_se > 0,
      2 * (1 - pnorm(abs(att / att_se))), NA_real_
    ),
    se_method = "placebo_vcov",
    se_replications = placebo_replications,
    n_control_units = setup$N0,
    n_treated_units = 1L,
    n_pre_periods = setup$T0,
    n_post_periods = ncol(setup$Y) - setup$T0,
    pre_rmspe = pre_rmspe,
    post_mean_treated_minus_synthetic = post_mean_gap,
    thirtieth_match_distance = meta$thirtieth_match_distance,
    selected_donor_population_ratio_median =
      meta$selected_donor_population_ratio_median,
    selected_donor_population_ratio_max =
      meta$selected_donor_population_ratio_max
  )
  dynamic_results[[meta$model_id]] <- dynamic
  placebo_att_results[[meta$model_id]] <- model_placebo_att
  placebo_dynamic_results[[meta$model_id]] <- model_placebo_dynamic
  placebo_status_results[[meta$model_id]] <- model_placebo_status

  omega[, `:=`(
    model_id = meta$model_id,
    decade_definition = definition,
    treated_lgd_id = treated_id,
    treated_lgd_name = meta$treated_lgd_name,
    cohort_lead_years = cohort_lead_years,
    effective_cohort_year = meta$effective_cohort_year,
    treatment_decade = meta$treatment_decade,
    outcome = outcome_name,
    weight_type = "omega"
  )]
  omega <- merge(
    omega,
    donors[, .(
      donor_lgd_id, donor_lgd_name, donor_rank, match_distance,
      distance_from_first_fair_venue_km, population_ratio
    )],
    by = "donor_lgd_id", all.x = TRUE, sort = FALSE
  )
  setorder(omega, -weight, donor_lgd_id)
  control_weight_results[[meta$model_id]] <- omega

  lambda[, `:=`(
    model_id = meta$model_id,
    decade_definition = definition,
    treated_lgd_id = treated_id,
    treated_lgd_name = meta$treated_lgd_name,
    cohort_lead_years = cohort_lead_years,
    effective_cohort_year = meta$effective_cohort_year,
    treatment_decade = meta$treatment_decade,
    outcome = outcome_name,
    weight_type = "lambda"
  )]
  setorder(lambda, pre_period)
  time_weight_results[[meta$model_id]] <- lambda

  fit_path <- data.table(
    model_id = meta$model_id,
    decade_definition = definition,
    treated_lgd_id = treated_id,
    treated_lgd_name = meta$treated_lgd_name,
    treated_lgd_type = meta$treated_lgd_type,
    treated_first_fair_year = meta$treated_first_fair_year,
    fair_treatment_decade = meta$fair_treatment_decade,
    cohort_lead_years = cohort_lead_years,
    effective_cohort_year = meta$effective_cohort_year,
    treatment_decade = meta$treatment_decade,
    treatment_start_decade = treatment_start,
    event_window = event_window_id,
    outcome = outcome_name,
    decade = time_periods,
    event_time = time_periods - meta$treatment_decade,
    period = ifelse(time_periods < treatment_start, "pre", "post"),
    treated_outcome = treated_path,
    synthetic_outcome = synthetic_path,
    treated_minus_synthetic = gap_path,
    pre_rmspe = pre_rmspe
  )
  fit_path_results[[meta$model_id]] <- fit_path

  model_status[[meta$model_id]] <- make_status_row(
    meta, status = "estimated", noise_level = raw_noise,
    pre_distinct = pre_distinct,
    pre_control_distinct = pre_control_distinct,
    n0 = setup$N0, n1 = 1L, t0 = setup$T0,
    t1 = ncol(setup$Y) - setup$T0,
    dynamic_placebos_successful = successful_placebos,
    att_se_available = is.finite(att_se)
  )
  model_objects[[meta$model_id]] <- list(
    estimate = fit, setup = setup, metadata = as.list(meta),
    outcome = outcome_name, annualization =
      "100000 * n_inventors / (population * n_years_in_bin)"
  )

  plot_fit_data <- melt(
    fit_path,
    id.vars = c("decade", "treatment_decade", "treatment_start_decade"),
    measure.vars = c("treated_outcome", "synthetic_outcome"),
    variable.name = "series", value.name = "value"
  )
  plot_fit_data[, series := factor(
    series,
    levels = c("treated_outcome", "synthetic_outcome"),
    labels = c("Treated LGD", "Synthetic control")
  )]
  fit_plot <- ggplot(
    plot_fit_data, aes(x = decade, y = value, color = series, linetype = series)
  ) +
    geom_vline(xintercept = treatment_start, color = "grey55",
               linewidth = 0.5, linetype = "dashed") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.7) +
    scale_color_manual(values = c("Treated LGD" = "#1b6ca8",
                                  "Synthetic control" = "#c44e52")) +
    scale_linetype_manual(values = c("Treated LGD" = "solid",
                                     "Synthetic control" = "dashed")) +
    labs(
      title = paste0(meta$treated_lgd_name, ": treated vs synthetic"),
      subtitle = paste0(definition, " decades; fair = ",
                        meta$treated_first_fair_year, "; cohort treatment = ",
                        meta$treatment_decade, "; treatment starts ",
                        treatment_start, "; pre-RMSPE = ",
                        format(round(pre_rmspe, 3), nsmall = 3)),
      x = "Decade",
      y = "Annual Discovery/Science births per 100,000",
      color = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      plot.title.position = "plot",
      plot.title = element_text(margin = margin(b = 4)),
      plot.subtitle = element_text(margin = margin(b = 8)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.margin = margin(t = 12, r = 14, b = 12, l = 24)
    )
  ggsave(
    file.path(fit_figure_dir, paste0(meta$model_id, "_fit.png")),
    fit_plot, width = 7.8, height = 5.0, dpi = 180
  )

  dynamic_plot <- ggplot(dynamic, aes(x = event_time_decades, y = effect)) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.5) +
    geom_ribbon(
      aes(ymin = ci_low, ymax = ci_high),
      fill = "#1b6ca8", alpha = 0.18, na.rm = TRUE
    ) +
    geom_line(color = "#1b6ca8", linewidth = 0.8) +
    geom_point(color = "#1b6ca8", size = 1.8) +
    labs(
      title = paste0(meta$treated_lgd_name, ": post-treatment effects"),
      subtitle = paste0(definition, " decades; ", cohort_lead_years,
                        "-year lead; placebo intervals (", successful_placebos,
                        " successful replications)"),
      x = "Decades since treatment",
      y = "Effect per 100,000 population per year"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(margin = margin(b = 4)),
      plot.subtitle = element_text(margin = margin(b = 8)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.margin = margin(t = 12, r = 14, b = 12, l = 24)
    )
  ggsave(
    file.path(dynamic_figure_dir, paste0(meta$model_id, "_dynamic.png")),
    dynamic_plot, width = 7.8, height = 5.0, dpi = 180
  )
  saveRDS(list(
    run_fingerprint = run_fingerprint,
    model_id = meta$model_id,
    status = model_status[[meta$model_id]],
    att = att_results[[meta$model_id]],
    dynamic = dynamic_results[[meta$model_id]],
    placebo_att = placebo_att_results[[meta$model_id]],
    placebo_dynamic = placebo_dynamic_results[[meta$model_id]],
    placebo_status = placebo_status_results[[meta$model_id]],
    control_weights = control_weight_results[[meta$model_id]],
    time_weights = time_weight_results[[meta$model_id]],
    fit_path = fit_path_results[[meta$model_id]],
    model_object = model_objects[[meta$model_id]]
  ), checkpoint_file)
}

if (checkpoint_only) {
  message("Checkpoint-only shard complete: ", shard_index, "/", shard_count,
          "; eligible models visited: ", nrow(run_model_manifest), ".")
  quit(save = "no", status = 0L)
}

###############################################################################
# Assemble outputs, validation, and summary figure
###############################################################################

status <- rbindlist(model_status, fill = TRUE, use.names = TRUE)
att <- rbindlist(att_results, fill = TRUE, use.names = TRUE)
dynamic <- rbindlist(dynamic_results, fill = TRUE, use.names = TRUE)
placebo_att <- rbindlist(placebo_att_results, fill = TRUE, use.names = TRUE)
placebo_dynamic <- rbindlist(placebo_dynamic_results, fill = TRUE, use.names = TRUE)
placebo_status <- rbindlist(placebo_status_results, fill = TRUE, use.names = TRUE)
control_weights <- rbindlist(control_weight_results, fill = TRUE, use.names = TRUE)
time_weights <- rbindlist(time_weight_results, fill = TRUE, use.names = TRUE)
fit_paths <- rbindlist(fit_path_results, fill = TRUE, use.names = TRUE)

setorder(status, decade_definition, treatment_decade, treated_lgd_id)
setorder(att, decade_definition, treatment_decade, treated_lgd_id)
setorder(dynamic, decade_definition, treatment_decade, treated_lgd_id, decade)
setorder(placebo_att, model_id, placebo_replication)
setorder(placebo_dynamic, model_id, placebo_replication, decade)
setorder(placebo_status, model_id, placebo_replication)
setorder(control_weights, model_id, -weight, donor_lgd_id)
setorder(time_weights, model_id, pre_period)
setorder(fit_paths, model_id, decade)

successful_models <- status[model_status == "estimated", .N]
infeasible_models <- status[
  model_status == "infeasible_zero_preperiod_variation", .N
]
failed_models <- status[model_status == "estimation_failed", .N]
skipped_no_pre_models <- status[model_status == "skipped_no_pre_period", .N]
skipped_insufficient_pre_models <- status[
  model_status == "skipped_insufficient_pre_periods", .N
]
# The balanced window adds a third skip reason, so the skip total is taken over
# every timing-skip status rather than the two the full window can produce.
skipped_outside_window_models <- status[
  model_status == "skipped_outside_balanced_window", .N
]
skipped_models <- skipped_no_pre_models + skipped_insufficient_pre_models +
  skipped_outside_window_models

expected_successful_models <- successful_models
expected_infeasible_models <- infeasible_models
# Per-reason expectations are pinned only for the full window, whose treated
# decade distribution is known. The balanced window trims both tails, so its
# skip mix is validated against the timing manifest instead.
expected_skipped_no_pre_models <- if (event_window$balanced)
  skipped_no_pre_models else if (
  treatment_mode == "host" && cohort_lead_years == 20L
) 1L else if (cohort_lead_years == 0L) 0L else skipped_no_pre_models
expected_skipped_insufficient_pre_models <- if (event_window$balanced)
  skipped_insufficient_pre_models else if (
  treatment_mode == "host" && cohort_lead_years == 20L
) 6L else if (cohort_lead_years == 0L) 0L else
  skipped_insufficient_pre_models
expected_pattern_ok <-
  successful_models + infeasible_models == expected_eligible_models &&
  skipped_models == expected_skipped_models &&
  skipped_no_pre_models == expected_skipped_no_pre_models &&
  skipped_insufficient_pre_models == expected_skipped_insufficient_pre_models
if (nrow(status) != expected_requested_models || !expected_pattern_ok ||
    failed_models != 0L) {
  fwrite(status, status_file, na = "")
  stop("Unexpected model outcome pattern; status audit written.")
}
if (nrow(att) != expected_successful_models ||
    any(!is.finite(att$estimate)) || any(!is.finite(att$se)) ||
    any(att$n_control_units != expected_donors) ||
    any(status[model_status == "estimated", dynamic_inference_complete] == FALSE)) {
  stop("Successful model estimates or inference are incomplete.")
}

weight_qc <- control_weights[, .(weight_sum = sum(weight)), by = model_id]
time_weight_qc <- time_weights[, .(weight_sum = sum(weight)), by = model_id]
if (nrow(weight_qc) != expected_successful_models ||
    nrow(time_weight_qc) != expected_successful_models ||
    nrow(control_weights) != expected_successful_models * expected_donors ||
    nrow(fit_paths) != expected_successful_models * model_period_count ||
    nrow(placebo_status) != expected_successful_models * placebo_replications ||
    max(abs(weight_qc$weight_sum - 1)) > tolerance ||
    max(abs(time_weight_qc$weight_sum - 1)) > tolerance ||
    anyDuplicated(control_weights[, .(model_id, donor_lgd_id)])) {
  stop("Control/time weight validation failed.")
}
forest_data <- merge(
  status[, .(
    model_id, decade_definition, treated_lgd_id, treated_lgd_name,
    treated_lgd_type, treated_first_fair_year, effective_cohort_year,
    cohort_lead_years, model_status
  )],
  att[, .(model_id, estimate, ci_low, ci_high)],
  by = "model_id", all.x = TRUE, sort = FALSE
)
forest_data[, plot_label := if (treatment_mode == "radius") {
  paste0(treated_lgd_name, " (", treated_lgd_type, "; ", treated_lgd_id, ")")
} else {
  treated_lgd_name
}]
forest_data[, status_plot_label := fcase(
  model_status == "skipped_no_pre_period", "skipped: no pre-period",
  model_status == "skipped_insufficient_pre_periods",
    "skipped: fewer than two pre-periods",
  model_status == "infeasible_zero_preperiod_variation",
    "infeasible: zero pre-period variation",
  default = NA_character_
)]
build_forest_plot <- function(plot_data, plot_title) {
  plot_data <- copy(plot_data)
  label_order <- unique(plot_data[
    order(treated_first_fair_year, treated_lgd_name, treated_lgd_id), plot_label
  ])
  plot_data[, plot_label := factor(plot_label, levels = rev(label_order))]
  finite_limits <- c(plot_data$ci_low, plot_data$ci_high)
  finite_limits <- finite_limits[is.finite(finite_limits)]
  x_limits <- if (length(finite_limits)) range(finite_limits) else c(-1, 1)
  x_padding <- max(diff(x_limits) * 0.08, 0.02)
  x_limits <- x_limits + c(-x_padding, x_padding)
  ggplot(plot_data, aes(y = plot_label)) +
    geom_vline(xintercept = 0, color = "grey55", linewidth = 0.5,
               linetype = "dashed") +
    geom_errorbar(
      data = plot_data[model_status == "estimated"],
      aes(xmin = ci_low, xmax = ci_high), width = 0.18,
      orientation = "y", color = "#1b6ca8"
    ) +
    geom_point(
      data = plot_data[model_status == "estimated"],
      aes(x = estimate), color = "#1b6ca8", size = 2
    ) +
    geom_text(
      data = plot_data[model_status != "estimated"],
      aes(x = x_limits[[1L]], label = status_plot_label),
      hjust = 0, color = "#8c2d2d", size = 2.8
    ) +
    facet_wrap(~ decade_definition, nrow = 1L) +
    coord_cartesian(xlim = x_limits, clip = "off") +
    labs(
      title = plot_title,
      subtitle = paste0(
        "Exposure begins ", cohort_lead_years,
        " years before each fair; annualized Wikipedia Discovery/Science births per 100,000"
      ),
      x = "Synthetic-DiD ATT with 95% placebo interval", y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title.position = "plot", panel.spacing.x = unit(1.2, "lines"))
}

if (treatment_mode == "host") {
  forest_plot <- build_forest_plot(
    forest_data,
    paste0("LGD-specific synthetic-DiD estimates: ", cohort_lead_years,
           "-year cohort lead")
  )
  ggsave(forest_file, forest_plot, width = 11, height = 6.8, dpi = 200)
  forest_output_label <- forest_file
} else {
  forest_manifest <- rbindlist(lapply(
    sort(unique(forest_data$treated_first_fair_year)),
    function(first_year) {
      cohort_data <- forest_data[treated_first_fair_year == first_year]
      cohort_file <- file.path(
        forest_cohort_dir,
        paste0("synthdid_att_first_fair_", first_year, ".png")
      )
      cohort_plot <- build_forest_plot(
        cohort_data,
        paste0("50 km radius synthetic-DiD: first exposure in ", first_year)
      )
      cohort_height <- min(30, max(6.8, 2 + 0.22 *
        uniqueN(cohort_data$treated_lgd_id)))
      ggsave(cohort_file, cohort_plot, width = 12, height = cohort_height,
             dpi = 180, limitsize = FALSE)
      data.table(
        treated_first_fair_year = first_year,
        treated_lgds = uniqueN(cohort_data$treated_lgd_id),
        figure_file = cohort_file
      )
    }
  ))
  fwrite(
    forest_manifest,
    file.path(forest_cohort_dir, "forest_figure_manifest.csv"),
    na = ""
  )
  forest_output_label <- forest_cohort_dir
}

qc <- data.table(
  metric = c(
    "treatment_mode", "treatment_radius_km", "candidate_control_status",
    "matching_specification", "cohort_lead_years", "requested_models",
    "eligible_models",
    "successful_models", "skipped_models", "skipped_no_pre_period_models",
    "skipped_insufficient_pre_period_models",
    "infeasible_zero_preperiod_models",
    "estimation_failed_models", "att_rows", "dynamic_rows",
    "control_weight_rows", "time_weight_rows", "fit_path_rows",
    "event_window", "skipped_outside_balanced_window_models",
    "placebo_replications_requested_per_model",
    "minimum_successful_dynamic_placebos", "maximum_successful_dynamic_placebos",
    "models_below_dynamic_placebo_requirement", "models_missing_att_se",
    "maximum_control_weight_sum_error", "maximum_time_weight_sum_error",
    "minimum_pre_periods_successful", "maximum_pre_periods_successful",
    "minimum_post_periods_successful", "maximum_post_periods_successful",
    "base_seed"
  ),
  value = as.character(c(
    treatment_mode,
    if (treatment_mode == "radius") treatment_radius_km else NA_real_,
    candidate_control_status,
    matching_specification, cohort_lead_years, expected_requested_models,
    expected_eligible_models,
    successful_models, skipped_models, skipped_no_pre_models,
    skipped_insufficient_pre_models, infeasible_models, failed_models,
    nrow(att), nrow(dynamic), nrow(control_weights),
    nrow(time_weights), nrow(fit_paths),
    event_window_id, skipped_outside_window_models, placebo_replications,
    min(status[model_status == "estimated", dynamic_placebos_successful]),
    max(status[model_status == "estimated", dynamic_placebos_successful]),
    status[model_status == "estimated" & !dynamic_inference_complete, .N],
    status[model_status == "estimated" & !att_se_available, .N],
    max(abs(weight_qc$weight_sum - 1)),
    max(abs(time_weight_qc$weight_sum - 1)),
    min(att$n_pre_periods), max(att$n_pre_periods),
    min(att$n_post_periods), max(att$n_post_periods), base_seed
  ))
)

fwrite(status, status_file, na = "")
fwrite(att, att_file, na = "")
fwrite(dynamic, dynamic_file, na = "")
fwrite(placebo_att, placebo_att_file, na = "")
fwrite(placebo_dynamic, placebo_dynamic_file, na = "")
fwrite(placebo_status, placebo_status_file, na = "")
fwrite(control_weights, control_weights_file, na = "")
fwrite(time_weights, time_weights_file, na = "")
fwrite(fit_paths, fit_paths_file, na = "")
saveRDS(model_objects, models_file)
fwrite(qc, qc_file, na = "")

message("LGD-specific Wikipedia synthetic-DiD estimation complete.")
message("  Treatment mode: ", treatment_mode,
        if (treatment_mode == "radius") paste0(" (", radius_tag, ")") else "")
message("  Cohort lead: ", cohort_lead_years, " years")
message("  Requested models: ", nrow(status))
message("  Skipped models: ", skipped_models)
message("  Estimated models: ", successful_models)
message("  Infeasible zero-preperiod models: ", infeasible_models)
message("  ATT output: ", att_file)
message("  Model status: ", status_file)
message("  Forest figure output: ", forest_output_label)
message("  QC: ", qc_file)
