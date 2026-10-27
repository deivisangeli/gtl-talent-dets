###############################################################################
# Pooled augmented synthetic-control event studies for fixed-1911 UK LGDs.
#
# Four host-treatment models are estimated with augsynth::multisynth:
#   - standard and alternative decade definitions;
#   - contemporaneous treatment and a 20-year cohort lead.
#
# The fixed-1851 HC1-HC5, SC1-SC8, and children-per-teacher fields enter as
# native multisynth auxiliary balance covariates. The estimator uses its
# built-in unit/time fixed-effects outcome augmentation and native staggered
# donor eligibility. Existing 30-unit donor pools are deliberately not used.
#
# Run from the repository root:
#   Rscript analysis/world_fairs/augsynth/estimate_ukds_1911_lgd_wikipedia_multisynth.R
#
# Optional overrides:
#   AUGSYNTH_BOOTSTRAP_REPLICATIONS=1000
#   AUGSYNTH_BASE_SEED=20260819
###############################################################################

rm(list = ls()); gc()

required_augsynth_sha <- "7a90ea48877fae7925a72cb50bc03a315bc7c042"
install_command <- paste0(
  "remotes::install_github('ebenmichael/augsynth@",
  required_augsynth_sha,
  "', dependencies=TRUE, upgrade='never')"
)
if (!requireNamespace("augsynth", quietly = TRUE)) {
  stop("Package 'augsynth' is required. Install the pinned version with:\n  ",
       install_command)
}
augsynth_description <- utils::packageDescription("augsynth")
installed_augsynth_sha <- augsynth_description[["RemoteSha"]]
if (is.null(installed_augsynth_sha) ||
    !identical(tolower(installed_augsynth_sha),
               tolower(required_augsynth_sha))) {
  stop("The installed augsynth build is not the pinned pipeline version. ",
       "Install it with:\n  ", install_command)
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
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
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/", mustWork = TRUE
  )
}
source(file.path(repo_root, "paths.R"))
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
missing_inputs <- panel_files[!file.exists(panel_files)]
if (length(missing_inputs)) {
  stop("Missing multisynth input panels:\n", paste(missing_inputs, collapse = "\n"))
}

results_dir <- file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", "augsynth",
  "ukds_1911_lgd_wikipedia_multisynth"
)
figure_dir <- file.path(results_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

bootstrap_replications <- suppressWarnings(as.integer(Sys.getenv(
  "AUGSYNTH_BOOTSTRAP_REPLICATIONS", unset = "1000"
)))
base_seed <- suppressWarnings(as.integer(Sys.getenv(
  "AUGSYNTH_BASE_SEED", unset = "20260819"
)))
if (is.na(bootstrap_replications) || bootstrap_replications < 20L) {
  stop("AUGSYNTH_BOOTSTRAP_REPLICATIONS must be at least 20.")
}
if (is.na(base_seed) || base_seed <= 0L) {
  stop("AUGSYNTH_BASE_SEED must be a positive integer.")
}

baseline_variables <- c(
  paste0("baseline_hc", 1:5, "_pct"),
  paste0("baseline_sc", 1:8, "_pct"),
  "baseline_children_per_teacher"
)
expected_lgds <- 1834L
expected_usable_lgds <- 1801L
expected_never_host_usable <- 1786L
minimum_coverage <- 0.95
tolerance <- 1e-6
weight_sum_tolerance <- 1e-3

model_design <- data.table(
  model_id = c(
    "host_lead0_standard", "host_lead0_alternative",
    "host_lead20_standard", "host_lead20_alternative"
  ),
  cohort_lead_years = c(0L, 0L, 20L, 20L),
  decade_definition = c("standard", "alternative", "standard", "alternative"),
  n_lags = 2L,
  # multisynth indexes leads as 1 = event time 0, so +40/+30 require 5/4.
  n_leads = c(5L, 5L, 4L, 4L),
  expected_treated_lgds = c(10L, 10L, 7L, 8L),
  expected_sample_lgds = c(1796L, 1796L, 1793L, 1794L)
)

output_files <- list(
  panel = file.path(results_dir, "multisynth_analysis_panel.csv"),
  manifest = file.path(results_dir, "multisynth_model_manifest.csv"),
  status = file.path(results_dir, "multisynth_model_status.csv"),
  pooled_att = file.path(results_dir, "multisynth_pooled_att.csv"),
  dynamic = file.path(results_dir, "multisynth_pooled_dynamic_effects.csv"),
  unit_effects = file.path(results_dir, "multisynth_treated_lgd_effects.csv"),
  weights = file.path(results_dir, "multisynth_donor_weights.csv"),
  outcome_balance = file.path(results_dir, "multisynth_preoutcome_balance.csv"),
  covariate_balance = file.path(results_dir, "multisynth_covariate_balance.csv"),
  qc = file.path(results_dir, "multisynth_qc.csv"),
  models = file.path(results_dir, "multisynth_models.rds"),
  dynamic_figure = file.path(figure_dir, "multisynth_pooled_event_studies.png"),
  weight_figure = file.path(figure_dir, "multisynth_top_donor_weights.png")
)

###############################################################################
# Helpers
###############################################################################

coerce_logical <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) == "true"
}

standard_decade <- function(year) {
  as.integer(floor(as.numeric(year) / 10) * 10)
}

alternative_decade <- function(year) {
  year <- as.integer(year)
  standard_decade(year) + ifelse(year %% 10L >= 7L, 10L, 0L)
}

expected_event_times <- function(lead_years) {
  if (lead_years == 0L) return(seq(-20L, 40L, by = 10L))
  if (lead_years == 20L) return(seq(-20L, 30L, by = 10L))
  stop("Unsupported cohort lead.")
}

model_formula <- stats::as.formula(paste(
  "outcome_value ~ treatment |",
  paste(baseline_variables, collapse = " + ")
))

build_model_panel <- function(panel, design_row) {
  definition <- design_row$decade_definition
  lead_years <- design_row$cohort_lead_years
  event_times <- expected_event_times(lead_years)

  treated <- unique(panel[
    treatment_status == "treated_post1870",
    .(analysis_lgd_id, treated_lgd_name = population_lgd_name,
      treated_lgd_type = population_lgd_type, first_fair_year)
  ])
  treated[, effective_cohort_year := first_fair_year - lead_years]
  treated[, treatment_decade := if (definition == "standard") {
    standard_decade(effective_cohort_year)
  } else {
    alternative_decade(effective_cohort_year)
  }]
  treated[, event_window_supported := vapply(
    treatment_decade,
    function(g) all((g + event_times) %in% unique(panel$decade)),
    logical(1L)
  )]
  treated[, model_eligible := event_window_supported]
  treated[, exclusion_reason := fifelse(
    model_eligible, "eligible",
    "insufficient_panel_support_for_declared_event_window"
  )]
  if (lead_years == 0L) {
    treated[tolower(treated_lgd_name) == "lambeth", `:=`(
      model_eligible = FALSE,
      exclusion_reason = "lead0_lambeth_excluded_by_design"
    )]
  }

  active <- treated[model_eligible == TRUE]
  if (nrow(active) != design_row$expected_treated_lgds) {
    stop("Unexpected treated count for ", design_row$model_id, ": ",
         nrow(active), " rather than ", design_row$expected_treated_lgds, ".")
  }

  sample <- panel[
    baseline_covariates_usable == TRUE &
      (treatment_status == "never_host" | analysis_lgd_id %in% active$analysis_lgd_id)
  ]
  sample <- merge(
    sample,
    active[, .(analysis_lgd_id, effective_cohort_year, treatment_decade)],
    by = "analysis_lgd_id", all.x = TRUE, sort = FALSE
  )
  sample[, treatment := as.integer(
    !is.na(treatment_decade) & decade >= treatment_decade
  )]
  sample[, outcome_value := 1e5 * n_inventors /
           (population * n_years_in_bin)]
  sample[, `:=`(
    model_id = design_row$model_id,
    cohort_lead_years = lead_years,
    native_donor_design = TRUE
  )]
  setorder(sample, analysis_lgd_id, decade)

  unit_check <- sample[, .(
    periods = .N,
    baseline_profiles = uniqueN(do.call(paste, c(.SD, sep = "|"))),
    treatment_reversals = sum(diff(treatment) < 0L),
    outcome_complete = all(is.finite(outcome_value))
  ), by = analysis_lgd_id, .SDcols = baseline_variables]
  if (nrow(sample) != design_row$expected_sample_lgds * 12L ||
      uniqueN(sample$analysis_lgd_id) != design_row$expected_sample_lgds ||
      uniqueN(sample[treatment_status == "never_host", analysis_lgd_id]) !=
        expected_never_host_usable ||
      any(unit_check$periods != 12L) ||
      any(unit_check$baseline_profiles != 1L) ||
      any(unit_check$treatment_reversals != 0L) ||
      any(!unit_check$outcome_complete) ||
      anyNA(sample[, ..baseline_variables]) ||
      any(sample$treatment_status == "pretreated_by1870")) {
    stop("The analysis panel failed validation for ", design_row$model_id, ".")
  }

  treated[, `:=`(
    model_id = design_row$model_id,
    cohort_lead_years = lead_years,
    decade_definition = definition,
    event_time_min = min(event_times),
    event_time_max = max(event_times)
  )]
  list(panel = sample, timing = treated, active = active)
}

extract_summary_outputs <- function(summary_object, design_row) {
  effects <- as.data.table(summary_object$att)
  required <- c(
    "Time", "Level", "Estimate", "Std.Error", "lower_bound", "upper_bound"
  )
  if (!all(required %in% names(effects))) {
    stop("Unexpected summary.multisynth ATT schema for ", design_row$model_id, ".")
  }
  setnames(
    effects, required,
    c("event_time_periods", "effect_level", "estimate", "std_error",
      "conf_low", "conf_high")
  )
  effects[, `:=`(
    model_id = design_row$model_id,
    cohort_lead_years = design_row$cohort_lead_years,
    decade_definition = design_row$decade_definition,
    event_time = as.integer(event_time_periods * 10)
  )]
  pooled_att <- effects[effect_level == "Average" & is.na(event_time_periods)]
  dynamic <- effects[
    effect_level == "Average" & !is.na(event_time_periods) &
      event_time %in% expected_event_times(design_row$cohort_lead_years)
  ]
  unit_effects <- effects[effect_level != "Average"]
  setnames(unit_effects, "effect_level", "treated_lgd_id")
  if (nrow(pooled_att) != 1L ||
      !setequal(dynamic$event_time,
                expected_event_times(design_row$cohort_lead_years)) ||
      any(!is.finite(dynamic$estimate)) || any(!is.finite(dynamic$std_error)) ||
      any(!is.finite(pooled_att$estimate)) ||
      any(!is.finite(pooled_att$std_error))) {
    message("Unexpected pooled effects for ", design_row$model_id, ":")
    print(effects[effect_level == "Average"])
    stop("Incomplete pooled multisynth effects for ", design_row$model_id, ".")
  }
  list(pooled_att = pooled_att, dynamic = dynamic, unit_effects = unit_effects)
}

extract_donor_weights <- function(fit, sample, design_row) {
  weights <- as.matrix(fit$weights)
  treated_units <- as.character(fit$data$units[is.finite(fit$data$trt)])
  all_units <- as.character(fit$data$units)
  if (nrow(weights) != length(all_units) ||
      ncol(weights) != length(treated_units)) {
    stop("Unexpected weight matrix dimensions for ", design_row$model_id, ".")
  }
  rownames(weights) <- all_units
  colnames(weights) <- treated_units
  if (max(abs(colSums(weights) - 1)) > weight_sum_tolerance ||
      min(weights) < -tolerance) {
    message("Weight diagnostics for ", design_row$model_id,
            ": max sum error=", max(abs(colSums(weights) - 1)),
            "; min weight=", min(weights))
    stop("Invalid multisynth donor weights for ", design_row$model_id, ".")
  }

  unit_reference <- unique(sample[, .(
    analysis_lgd_id, population_lgd_name, population_lgd_type,
    treatment_status, treatment_decade
  )])
  out <- rbindlist(lapply(seq_along(treated_units), function(j) {
    data.table(
      model_id = design_row$model_id,
      cohort_lead_years = design_row$cohort_lead_years,
      decade_definition = design_row$decade_definition,
      treated_lgd_id = treated_units[[j]],
      donor_lgd_id = all_units,
      eligible_native_donor = as.logical(fit$donors[[j]]),
      donor_weight = as.numeric(weights[, j])
    )
  }))
  out <- merge(
    out,
    unit_reference,
    by.x = "donor_lgd_id", by.y = "analysis_lgd_id",
    all.x = TRUE, sort = FALSE
  )
  setnames(out, c("population_lgd_name", "population_lgd_type",
                  "treatment_status", "treatment_decade"),
           c("donor_lgd_name", "donor_lgd_type", "donor_treatment_status",
             "donor_treatment_decade"))
  if (any(out[eligible_native_donor == FALSE, donor_weight] > tolerance) ||
      anyNA(out$donor_lgd_name)) {
    stop("An ineligible or unidentified donor received weight for ",
         design_row$model_id, ".")
  }
  setorder(out, treated_lgd_id, -donor_weight, donor_lgd_id)
  out
}

matrix_to_balance <- function(matrix, variables, balance_type, fit, design_row) {
  matrix <- as.matrix(matrix)
  treated_units <- as.character(fit$data$units[is.finite(fit$data$trt)])
  if (ncol(matrix) != length(treated_units) + 1L) {
    stop("Unexpected balance matrix columns for ", design_row$model_id, ".")
  }
  colnames(matrix) <- c("Average", treated_units)
  if (length(variables) != nrow(matrix)) {
    stop("Unexpected balance matrix rows for ", design_row$model_id, ".")
  }
  out <- as.data.table(as.table(matrix))
  setnames(out, names(out)[1:3],
           c("variable", "effect_level", "imbalance"))
  out[, `:=`(
    model_id = design_row$model_id,
    cohort_lead_years = design_row$cohort_lead_years,
    decade_definition = design_row$decade_definition,
    balance_type = balance_type
  )]
  out[]
}

###############################################################################
# Read and validate input panels
###############################################################################

message("Reading fixed-1911 LGD decade panels...")
panels <- lapply(names(panel_files), function(definition) {
  panel <- fread(panel_files[[definition]], na.strings = c("", "NA"))
  required <- c(
    "analysis_lgd_id", "population_lgd_name", "population_lgd_type",
    "decade", "n_years_in_bin", "population", "n_inventors",
    "first_fair_year", "treatment_status", "baseline_covariates_usable",
    "baseline_covariate_population_coverage", baseline_variables
  )
  if (!all(required %in% names(panel))) {
    stop(definition, " panel is missing multisynth inputs.")
  }
  panel[, baseline_covariates_usable := coerce_logical(
    baseline_covariates_usable
  )]
  for (variable in c(
      "population", "n_inventors", "baseline_covariate_population_coverage",
      baseline_variables)) {
    set(panel, j = variable,
        value = suppressWarnings(as.numeric(panel[[variable]])))
  }
  if (nrow(panel) != expected_lgds * 12L ||
      uniqueN(panel$analysis_lgd_id) != expected_lgds ||
      panel[, uniqueN(decade)] != 12L ||
      uniqueN(panel[baseline_covariates_usable == TRUE, analysis_lgd_id]) !=
        expected_usable_lgds ||
      uniqueN(panel[baseline_covariates_usable == FALSE, analysis_lgd_id]) !=
        expected_lgds - expected_usable_lgds ||
      any(panel[baseline_covariates_usable == TRUE,
                baseline_covariate_population_coverage] + tolerance <
            minimum_coverage)) {
    stop(definition, " panel failed baseline/sample validation.")
  }
  panel
})
names(panels) <- names(panel_files)

###############################################################################
# Build panels, fit four models, and extract artifacts
###############################################################################

analysis_panels <- list()
timing_manifests <- list()
model_status <- list()
pooled_att_results <- list()
dynamic_results <- list()
unit_effect_results <- list()
weight_results <- list()
outcome_balance_results <- list()
covariate_balance_results <- list()
models <- list()

RNGkind("Mersenne-Twister", "Inversion", "Rejection")

for (model_index in seq_len(nrow(model_design))) {
  design_row <- model_design[model_index]
  model_id <- design_row$model_id
  message("Building ", model_id, " (", model_index, "/", nrow(model_design), ")...")
  built <- build_model_panel(
    panels[[design_row$decade_definition]], design_row
  )
  sample <- built$panel
  analysis_panels[[model_id]] <- sample
  timing_manifests[[model_id]] <- built$timing

  message("Estimating ", model_id, " with native staggered donors...")
  fit <- augsynth::multisynth(
    model_formula,
    unit = analysis_lgd_id,
    time = decade,
    data = as.data.frame(sample),
    n_leads = design_row$n_leads,
    n_lags = design_row$n_lags,
    nu = NULL,
    lambda = 0,
    V = NULL,
    fixedeff = TRUE,
    n_factors = 0,
    scm = TRUE,
    time_cohort = FALSE,
    eps_abs = 1e-6,
    eps_rel = 1e-6,
    verbose = FALSE
  )
  if (is.null(fit$data$Z) || ncol(fit$data$Z) != length(baseline_variables) ||
      !setequal(colnames(fit$data$Z), baseline_variables)) {
    stop("The fitted model does not contain all baseline controls for ", model_id, ".")
  }

  set.seed(base_seed + model_index - 1L)
  fit_summary <- summary(
    fit,
    inf_type = "bootstrap",
    n_boot = bootstrap_replications,
    alpha = 0.05
  )
  effects <- extract_summary_outputs(fit_summary, design_row)
  weights <- extract_donor_weights(fit, sample, design_row)

  outcome_matrix <- tail(fit$imbalance, design_row$n_lags)
  outcome_balance <- matrix_to_balance(
    outcome_matrix,
    paste0("event_time_", seq(-design_row$n_lags, -1L) * 10L),
    "pre_treatment_outcome", fit, design_row
  )
  covariate_balance <- matrix_to_balance(
    fit$imbalance_aux, rownames(fit$imbalance_aux),
    "fixed_1851_auxiliary_covariate", fit, design_row
  )

  model_status[[model_id]] <- data.table(
    model_id = model_id,
    model_status = "estimated",
    augsynth_version = as.character(augsynth_description[["Version"]]),
    augsynth_sha = installed_augsynth_sha,
    cohort_lead_years = design_row$cohort_lead_years,
    decade_definition = design_row$decade_definition,
    outcome = "discovery_science_births_per_100k_population_year",
    treated_lgds = uniqueN(sample[treatment_status == "treated_post1870",
                                 analysis_lgd_id]),
    sample_lgds = uniqueN(sample$analysis_lgd_id),
    never_host_lgds = uniqueN(sample[treatment_status == "never_host",
                                    analysis_lgd_id]),
    panel_rows = nrow(sample),
    n_lags = design_row$n_lags,
    n_leads = design_row$n_leads,
    event_time_min = min(expected_event_times(design_row$cohort_lead_years)),
    event_time_max = max(expected_event_times(design_row$cohort_lead_years)),
    bootstrap_replications = bootstrap_replications,
    bootstrap_seed = base_seed + model_index - 1L,
    nu = fit$nu,
    lambda = fit$lambda,
    global_l2 = fit$global_l2,
    scaled_global_l2 = fit$scaled_global_l2,
    individual_l2 = fit$ind_l2,
    scaled_individual_l2 = fit$scaled_ind_l2,
    global_auxiliary_l2 = fit$global_l2_aux,
    individual_auxiliary_l2 = fit$ind_l2_aux,
    positive_weight_links = weights[donor_weight > tolerance, .N],
    maximum_weight_sum_error = weights[, abs(sum(donor_weight) - 1),
                                       by = treated_lgd_id][, max(V1)]
  )
  pooled_att_results[[model_id]] <- effects$pooled_att
  dynamic_results[[model_id]] <- effects$dynamic
  unit_effect_results[[model_id]] <- effects$unit_effects
  weight_results[[model_id]] <- weights
  outcome_balance_results[[model_id]] <- outcome_balance
  covariate_balance_results[[model_id]] <- covariate_balance
  models[[model_id]] <- list(fit = fit, summary = fit_summary)
}

analysis_panel <- rbindlist(analysis_panels, fill = TRUE, use.names = TRUE)
timing_manifest <- rbindlist(timing_manifests, fill = TRUE, use.names = TRUE)
status <- rbindlist(model_status, fill = TRUE, use.names = TRUE)
pooled_att <- rbindlist(pooled_att_results, fill = TRUE, use.names = TRUE)
dynamic <- rbindlist(dynamic_results, fill = TRUE, use.names = TRUE)
unit_effects <- rbindlist(unit_effect_results, fill = TRUE, use.names = TRUE)
donor_weights <- rbindlist(weight_results, fill = TRUE, use.names = TRUE)
outcome_balance <- rbindlist(
  outcome_balance_results, fill = TRUE, use.names = TRUE
)
covariate_balance <- rbindlist(
  covariate_balance_results, fill = TRUE, use.names = TRUE
)

setorder(analysis_panel, cohort_lead_years, decade_definition,
         analysis_lgd_id, decade)
setorder(timing_manifest, cohort_lead_years, decade_definition,
         model_eligible, treatment_decade, analysis_lgd_id)
setorder(status, cohort_lead_years, decade_definition)
setorder(pooled_att, cohort_lead_years, decade_definition)
setorder(dynamic, cohort_lead_years, decade_definition, event_time)
setorder(unit_effects, cohort_lead_years, decade_definition,
         treated_lgd_id, event_time_periods)
setorder(donor_weights, cohort_lead_years, decade_definition,
         treated_lgd_id, -donor_weight, donor_lgd_id)

###############################################################################
# Final QC and outputs
###############################################################################

expected_dynamic_rows <- sum(vapply(
  model_design$cohort_lead_years,
  function(lead) length(expected_event_times(lead)), integer(1L)
))
status_check <- merge(
  model_design[, .(model_id, expected_treated_lgds, expected_sample_lgds)],
  status[, .(model_id, treated_lgds, sample_lgds)],
  by = "model_id", all = TRUE, sort = FALSE
)
if (nrow(status) != 4L || any(status$model_status != "estimated") ||
    nrow(pooled_att) != 4L || nrow(dynamic) != expected_dynamic_rows ||
    nrow(status_check) != 4L || anyNA(status_check) ||
    any(status_check$treated_lgds != status_check$expected_treated_lgds) ||
    any(status_check$sample_lgds != status_check$expected_sample_lgds) ||
    any(status$never_host_lgds != expected_never_host_usable) ||
    any(status$maximum_weight_sum_error > weight_sum_tolerance) ||
    any(!is.finite(status$nu)) ||
    any(!is.finite(unlist(pooled_att[, .(estimate, std_error, conf_low, conf_high)]))) ||
    any(!is.finite(unlist(dynamic[, .(estimate, std_error, conf_low, conf_high)])))) {
  message("Final validation diagnostics:")
  print(status)
  print(pooled_att[, .(model_id, estimate, std_error, conf_low, conf_high)])
  print(dynamic[, .(rows = .N, finite = all(is.finite(c(
    estimate, std_error, conf_low, conf_high
  )))), by = model_id])
  stop("Final pooled multisynth validation failed.")
}

qc <- rbindlist(list(
  status[, .(
    metric = c(
      "treated_lgds", "sample_lgds", "never_host_lgds", "panel_rows",
      "dynamic_event_times", "bootstrap_replications",
      "maximum_weight_sum_error", "global_l2", "scaled_global_l2",
      "global_auxiliary_l2"
    ),
    value = as.character(c(
      treated_lgds, sample_lgds, never_host_lgds, panel_rows,
      dynamic[model_id == .BY$model_id, .N], bootstrap_replications,
      maximum_weight_sum_error, global_l2, scaled_global_l2,
      global_auxiliary_l2
    ))
  ), by = model_id],
  data.table(
    model_id = "all_models",
    metric = c(
      "models_estimated", "input_lgds", "baseline_usable_lgds",
      "baseline_unusable_lgds", "baseline_controls",
      "augsynth_version", "augsynth_sha"
    ),
    value = as.character(c(
      nrow(status), expected_lgds, expected_usable_lgds,
      expected_lgds - expected_usable_lgds, length(baseline_variables),
      augsynth_description[["Version"]], installed_augsynth_sha
    ))
  )
), fill = TRUE)

fwrite(analysis_panel, output_files$panel, na = "")
fwrite(timing_manifest, output_files$manifest, na = "")
fwrite(status, output_files$status, na = "")
fwrite(pooled_att, output_files$pooled_att, na = "")
fwrite(dynamic, output_files$dynamic, na = "")
fwrite(unit_effects, output_files$unit_effects, na = "")
fwrite(donor_weights, output_files$weights, na = "")
fwrite(outcome_balance, output_files$outcome_balance, na = "")
fwrite(covariate_balance, output_files$covariate_balance, na = "")
fwrite(qc, output_files$qc, na = "")
saveRDS(models, output_files$models, compress = "xz")

dynamic_plot <- ggplot(
  dynamic,
  aes(x = event_time / 10, y = estimate, ymin = conf_low, ymax = conf_high)
) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey55") +
  geom_ribbon(alpha = 0.18, fill = "#2C7FB8") +
  geom_line(color = "#08589E", linewidth = 0.7) +
  geom_point(color = "#08589E", size = 1.8) +
  facet_grid(cohort_lead_years ~ decade_definition, scales = "free_x") +
  scale_x_continuous(breaks = -2:4) +
  labs(
    x = "Event time (decades)",
    y = "Discovery/Science births per 100,000 population-year",
    title = "Pooled augmented synthetic-control event studies",
    subtitle = "Native multisynth donors; 95% wild-bootstrap intervals"
  ) +
  theme_minimal(base_size = 11)
ggsave(output_files$dynamic_figure, dynamic_plot,
       width = 10, height = 7, dpi = 300)

pooled_physical_weights <- donor_weights[, .(
  average_weight_across_treated = mean(donor_weight),
  treated_models_receiving_positive_weight = sum(donor_weight > tolerance)
), by = .(model_id, cohort_lead_years, decade_definition,
          donor_lgd_id, donor_lgd_name)]
top_weights <- pooled_physical_weights[
  order(model_id, -average_weight_across_treated), head(.SD, 15L),
  by = model_id
]
top_weights[, donor_label := paste0(donor_lgd_name, " [", donor_lgd_id, "]")]
weight_plot <- ggplot(
  top_weights,
  aes(x = average_weight_across_treated,
      y = reorder(donor_label, average_weight_across_treated))
) +
  geom_col(fill = "#238B45") +
  facet_wrap(~ model_id, scales = "free_y", ncol = 2) +
  labs(
    x = "Average weight across treated LGDs",
    y = NULL,
    title = "Largest pooled physical-donor weights"
  ) +
  theme_minimal(base_size = 10)
ggsave(output_files$weight_figure, weight_plot,
       width = 12, height = 9, dpi = 300)

message("Pooled multisynth analysis complete.")
message("  Package: augsynth ", augsynth_description[["Version"]],
        " @ ", substr(installed_augsynth_sha, 1L, 8L))
message("  Bootstrap replications: ", bootstrap_replications)
message("  Model estimates:")
print(pooled_att[, .(
  model_id, estimate, std_error, conf_low, conf_high
)])
message("  Results: ", results_dir)
