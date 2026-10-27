###############################################################################
# Estimate pooled event-time synthetic-DiD models for the fixed-1911 LGDs.
#
# Four host-treatment specifications are estimated separately:
#   - standard and alternative decade definitions;
#   - contemporaneous treatment and a 20-year cohort lead.
#
# Each treated LGD and its 30 selected donors form a stack. Calendar decades
# are aligned to event times -20, -10, 0, and +10, producing one simultaneous-
# adoption synthdid matrix per specification. Treated LGDs receive equal
# weight. Inference uses within-stack block placebos.
#
# Run from the repository root:
#   Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_pooled_synthdid.R
#
# Optional overrides:
#   SYNTHDID_PLACEBO_REPLICATIONS=200
#   SYNTHDID_BASE_SEED=20260819
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
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
donor_base_dir <- file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", "synthdid",
  "ukds_1911_lgd_wikipedia_donor_pools"
)
results_dir <- file.path(donor_base_dir, "pooled_event_time")
figure_dir <- file.path(results_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

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
lead_roots <- c(
  `0` = donor_base_dir,
  `20` = file.path(donor_base_dir, "cohort_lead_20y")
)
required_inputs <- c(
  panel_files,
  unlist(lapply(lead_roots, function(root) file.path(
    root, c(
      "donor_pool_pairs.csv", "donor_pool_summary.csv",
      "cohort_timing_status.csv"
    )
  )))
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs)) {
  stop("Missing pooled-synthdid inputs:\n", paste(missing_inputs, collapse = "\n"))
}

placebo_replications <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_PLACEBO_REPLICATIONS", unset = "200"
)))
base_seed <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_BASE_SEED", unset = "20260819"
)))
if (is.na(placebo_replications) || placebo_replications < 20L) {
  stop("SYNTHDID_PLACEBO_REPLICATIONS must be an integer of at least 20.")
}
if (is.na(base_seed) || base_seed <= 0L) {
  stop("SYNTHDID_BASE_SEED must be a positive integer.")
}

event_times_for_lead <- function(lead_years) {
  if (lead_years == 0L) {
    return(seq(-20L, 40L, by = 10L))
  }
  if (lead_years == 20L) {
    return(seq(-20L, 30L, by = 10L))
  }
  stop("Unsupported cohort lead: ", lead_years, ".")
}
expected_donors <- 30L
expected_lgds <- 1834L
minimum_successful_placebos <- ceiling(0.80 * placebo_replications)
tolerance <- 1e-8
outcome_name <- "discovery_science_births_per_100k_population_year"
expected_matching_specification <- paste0(
  "equal_z_euclidean_log1p_pooled_pre_treatment_wikipedia_rate_",
  "plus_log_baseline_population_density"
)

expected_design <- data.table(
  cohort_lead_years = c(0L, 0L, 20L, 20L),
  decade_definition = c("standard", "alternative", "standard", "alternative"),
  expected_treated_stacks = c(10L, 10L, 7L, 8L),
  event_time_min = c(-20L, -20L, -20L, -20L),
  event_time_max = c(40L, 40L, 30L, 30L),
  expected_pre_periods = c(2L, 2L, 2L, 2L),
  expected_post_periods = c(5L, 5L, 4L, 4L)
)
expected_design[, `:=`(
  model_id = paste0(
    "host_lead", cohort_lead_years, "_", decade_definition
  ),
  expected_periods = expected_pre_periods + expected_post_periods,
  expected_control_stack_units = expected_treated_stacks * expected_donors,
  expected_panel_rows =
    expected_treated_stacks * (expected_donors + 1L) *
      (expected_pre_periods + expected_post_periods)
)]

output_files <- list(
  panel = file.path(results_dir, "pooled_event_time_panel.csv"),
  manifest = file.path(results_dir, "pooled_treated_stack_manifest.csv"),
  status = file.path(results_dir, "pooled_synthdid_model_status.csv"),
  att = file.path(results_dir, "pooled_synthdid_att.csv"),
  dynamic = file.path(results_dir, "pooled_synthdid_dynamic_effects.csv"),
  placebo_att = file.path(results_dir, "pooled_synthdid_placebo_att.csv"),
  placebo_dynamic = file.path(
    results_dir, "pooled_synthdid_placebo_dynamic.csv"
  ),
  placebo_status = file.path(
    results_dir, "pooled_synthdid_placebo_status.csv"
  ),
  placebo_assignments = file.path(
    results_dir, "pooled_synthdid_placebo_assignments.csv"
  ),
  control_weights_stack = file.path(
    results_dir, "pooled_synthdid_control_weights_stack.csv"
  ),
  control_weights_physical = file.path(
    results_dir, "pooled_synthdid_control_weights_physical_lgd.csv"
  ),
  time_weights = file.path(results_dir, "pooled_synthdid_time_weights.csv"),
  fit_paths = file.path(results_dir, "pooled_synthdid_fit_paths.csv"),
  models = file.path(results_dir, "pooled_synthdid_models.rds"),
  qc = file.path(results_dir, "pooled_synthdid_qc.csv"),
  forest = file.path(figure_dir, "pooled_synthdid_att_forest.png"),
  dynamic_figure = file.path(
    figure_dir, "pooled_synthdid_dynamic_effects.png"
  ),
  fit_figure = file.path(figure_dir, "pooled_synthdid_fit_paths.png")
)

###############################################################################
# Helpers
###############################################################################

coerce_logical <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) == "true"
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

build_stack <- function(
    model_id, lead_years, definition, timing_row, donor_rows, panel,
    model_event_times) {
  treated_id <- as.character(timing_row$treated_lgd_id)
  treatment_decade <- as.integer(timing_row$treatment_decade)
  stack_id <- paste(model_id, treated_id, sep = "|")

  donor_rows <- donor_rows[order(donor_rank)]
  if (nrow(donor_rows) != expected_donors ||
      anyDuplicated(donor_rows$donor_lgd_id) ||
      !identical(as.integer(donor_rows$donor_rank), seq_len(expected_donors)) ||
      any(donor_rows$donor_treatment_status != "never_host") ||
      any(donor_rows$distance_from_first_fair_venue_km + tolerance < 50) ||
      any(donor_rows$matching_specification !=
            expected_matching_specification)) {
    stop("Invalid donor pool for pooled stack ", stack_id, ".")
  }

  units <- rbind(
    data.table(
      physical_lgd_id = treated_id,
      unit_name = as.character(timing_row$treated_lgd_name),
      unit_type = as.character(timing_row$treated_lgd_type),
      role = "treated",
      donor_rank = NA_integer_,
      match_distance = NA_real_,
      distance_from_first_fair_venue_km = 0,
      donor_population_ratio = 1,
      donor_pre_treatment_scientist_rate_per_100k_year = NA_real_,
      donor_baseline_population = NA_real_,
      donor_baseline_population_density_per_km2 = NA_real_
    ),
    donor_rows[, .(
      physical_lgd_id = as.character(donor_lgd_id),
      unit_name = donor_lgd_name,
      unit_type = donor_lgd_type,
      role = "control",
      donor_rank = as.integer(donor_rank),
      match_distance = as.numeric(match_distance),
      distance_from_first_fair_venue_km =
        as.numeric(distance_from_first_fair_venue_km),
      donor_population_ratio = as.numeric(population_ratio),
      donor_pre_treatment_scientist_rate_per_100k_year = as.numeric(
        donor_pre_treatment_scientist_rate_per_100k_year
      ),
      donor_baseline_population = as.numeric(donor_baseline_population),
      donor_baseline_population_density_per_km2 = as.numeric(
        donor_baseline_population_density_per_km2
      )
    )],
    fill = TRUE,
    use.names = TRUE
  )
  units[, `:=`(
    model_id = model_id,
    cohort_lead_years = lead_years,
    decade_definition = definition,
    matching_specification = expected_matching_specification,
    stack_id = stack_id,
    stack_unit_id = paste(stack_id, role, physical_lgd_id, sep = "|"),
    treated_lgd_id = treated_id,
    treated_lgd_name = as.character(timing_row$treated_lgd_name),
    treated_lgd_type = as.character(timing_row$treated_lgd_type),
    treated_first_fair_year = as.integer(timing_row$treated_first_fair_year),
    effective_cohort_year = as.integer(timing_row$effective_cohort_year),
    treatment_decade = treatment_decade
  )]

  grid <- units[rep(seq_len(.N), each = length(model_event_times))]
  grid[, event_time := rep(model_event_times, times = nrow(units))]
  grid[, calendar_decade := treatment_decade + event_time]
  panel_values <- panel[, .(
    physical_lgd_id = analysis_lgd_id,
    calendar_decade = decade,
    n_inventors,
    population,
    n_years_in_bin,
    outcome_value
  )]
  grid <- merge(
    grid, panel_values,
    by = c("physical_lgd_id", "calendar_decade"),
    all.x = TRUE,
    sort = FALSE
  )
  grid[, treatment := as.integer(role == "treated" & event_time >= 0L)]
  setorder(grid, role, donor_rank, stack_unit_id, event_time)

  if (nrow(grid) != (expected_donors + 1L) * length(model_event_times) ||
      uniqueN(grid$stack_unit_id) != expected_donors + 1L ||
      anyDuplicated(grid[, .(stack_unit_id, event_time)]) ||
      any(!is.finite(grid$outcome_value))) {
    stop("Incomplete pooled stack ", stack_id, ".")
  }
  grid
}

###############################################################################
# Read panels and construct the stacked event-time data
###############################################################################

message("Reading donor pools and fixed-LGD decade panels...")
panels <- lapply(panel_files, function(path) {
  x <- fread(path, na.strings = c("", "NA"))
  x[, `:=`(
    analysis_lgd_id = as.character(analysis_lgd_id),
    decade = as.integer(decade),
    n_inventors = as.numeric(n_inventors),
    population = as.numeric(population),
    n_years_in_bin = as.integer(n_years_in_bin)
  )]
  x[, outcome_value :=
    1e5 * n_inventors / (population * n_years_in_bin)]
  if (nrow(x) != expected_lgds * 12L ||
      anyDuplicated(x[, .(analysis_lgd_id, decade)]) ||
      any(!is.finite(x$outcome_value))) {
    stop("Invalid fixed-LGD decade panel: ", path)
  }
  x
})
names(panels) <- names(panel_files)

stack_panels <- list()
stack_manifests <- list()

for (lead_name in names(lead_roots)) {
  lead_years <- as.integer(lead_name)
  root <- lead_roots[[lead_name]]
  donor_pairs <- fread(
    file.path(root, "donor_pool_pairs.csv"), na.strings = c("", "NA")
  )
  donor_summary <- fread(
    file.path(root, "donor_pool_summary.csv"), na.strings = c("", "NA")
  )
  timing <- fread(
    file.path(root, "cohort_timing_status.csv"), na.strings = c("", "NA")
  )
  timing[, model_eligible := coerce_logical(model_eligible)]

  if (any(donor_pairs$treatment_mode != "host") ||
      any(donor_summary$treatment_mode != "host") ||
      any(timing$treatment_mode != "host") ||
      any(as.integer(timing$cohort_lead_years) != lead_years) ||
      !setequal(timing$decade_definition, names(panels)) ||
      !identical(
        unique(donor_summary$matching_specification),
        expected_matching_specification
      )) {
    stop("Donor/timing configuration mismatch for lead ", lead_years, ".")
  }

  for (definition in names(panels)) {
    model_id <- paste0("host_lead", lead_years, "_", definition)
    model_id_value <- model_id
    model_event_times <- event_times_for_lead(lead_years)
    timing_model <- timing[
      decade_definition == definition & model_eligible == TRUE
    ][order(treatment_decade, treated_lgd_id)]
    if (lead_years == 0L) {
      timing_model <- timing_model[tolower(treated_lgd_name) != "lambeth"]
    }
    expected_stacks <- expected_design[
      model_id == model_id_value, expected_treated_stacks
    ]
    if (nrow(timing_model) != expected_stacks) {
      stop("Unexpected treated-stack count for ", model_id, ".")
    }

    for (treated_index in seq_len(nrow(timing_model))) {
      timing_row <- timing_model[treated_index]
      treated_id <- as.character(timing_row$treated_lgd_id)
      donor_rows <- donor_pairs[
        decade_definition == definition & treated_lgd_id == treated_id
      ]
      stack_id <- paste(model_id, treated_id, sep = "|")
      stack_panels[[stack_id]] <- build_stack(
        model_id, lead_years, definition, timing_row,
        donor_rows, panels[[definition]], model_event_times
      )
      stack_manifests[[stack_id]] <- data.table(
        model_id = model_id,
        treatment_mode = "host",
        cohort_lead_years = lead_years,
        decade_definition = definition,
        matching_specification = expected_matching_specification,
        stack_id = stack_id,
        treated_lgd_id = treated_id,
        treated_lgd_name = as.character(timing_row$treated_lgd_name),
        treated_lgd_type = as.character(timing_row$treated_lgd_type),
        treated_first_fair_year = as.integer(
          timing_row$treated_first_fair_year
        ),
        effective_cohort_year = as.integer(timing_row$effective_cohort_year),
        treatment_decade = as.integer(timing_row$treatment_decade),
        event_time_min = min(model_event_times),
        event_time_max = max(model_event_times),
        pre_periods = sum(model_event_times < 0L),
        post_periods = sum(model_event_times >= 0L),
        selected_donors = nrow(donor_rows)
      )
    }
  }
}

stack_panel <- rbindlist(stack_panels, fill = TRUE, use.names = TRUE)
stack_manifest <- rbindlist(stack_manifests, fill = TRUE, use.names = TRUE)
setorder(stack_panel, cohort_lead_years, decade_definition, stack_id,
         role, donor_rank, stack_unit_id, event_time)
setorder(stack_manifest, cohort_lead_years, decade_definition,
         treatment_decade, treated_lgd_id)

if (any(stack_manifest[
      cohort_lead_years == 0L, tolower(treated_lgd_name) == "lambeth"
    ]) ||
    stack_manifest[
      cohort_lead_years == 20L & tolower(treated_lgd_name) == "lambeth",
      uniqueN(decade_definition)
    ] != 2L) {
  stop("Lambeth exclusion/inclusion does not match the declared design.")
}

panel_identity_error <- stack_panel[, max(abs(
  outcome_value - 1e5 * n_inventors / (population * n_years_in_bin)
))]
if (!is.finite(panel_identity_error) || panel_identity_error > tolerance ||
    anyDuplicated(stack_panel[, .(model_id, stack_unit_id, event_time)]) ||
    any(stack_panel[role == "control", treatment] != 0L) ||
    any(stack_panel[
      role == "treated", treatment != as.integer(event_time >= 0L)
    ])) {
  stop("Pooled event-time panel validation failed.")
}

panel_dimensions <- stack_panel[, .(
  treated_stacks = uniqueN(stack_id),
  control_stack_units = uniqueN(stack_unit_id[role == "control"]),
  treated_stack_units = uniqueN(stack_unit_id[role == "treated"]),
  panel_rows = .N,
  physical_control_lgds = uniqueN(physical_lgd_id[role == "control"])
), by = .(model_id, cohort_lead_years, decade_definition)]
panel_dimensions <- merge(
  expected_design, panel_dimensions,
  by = c("model_id", "cohort_lead_years", "decade_definition"),
  all.x = TRUE,
  sort = FALSE
)
if (any(panel_dimensions$treated_stacks !=
          panel_dimensions$expected_treated_stacks) ||
    any(panel_dimensions$treated_stack_units !=
          panel_dimensions$expected_treated_stacks) ||
    any(panel_dimensions$control_stack_units !=
          panel_dimensions$expected_control_stack_units) ||
    any(panel_dimensions$panel_rows !=
          panel_dimensions$expected_panel_rows)) {
  stop("Pooled panel dimensions differ from the declared design.")
}

###############################################################################
# Estimate the four pooled models and within-stack block placebos
###############################################################################

model_specs <- unique(stack_manifest[, .(
  model_id, cohort_lead_years, decade_definition, matching_specification
)])
model_specs[, definition_order := match(
  decade_definition, c("standard", "alternative")
)]
setorder(model_specs, cohort_lead_years, definition_order)
model_specs[, model_number := seq_len(.N)]

status_results <- list()
att_results <- list()
dynamic_results <- list()
placebo_att_results <- list()
placebo_dynamic_results <- list()
placebo_status_results <- list()
placebo_assignment_results <- list()
control_weight_results <- list()
physical_weight_results <- list()
time_weight_results <- list()
fit_path_results <- list()
model_objects <- list()

for (model_index in seq_len(nrow(model_specs))) {
  meta <- model_specs[model_index]
  model_id <- meta$model_id
  model_id_value <- model_id
  message("Estimating pooled model ", model_id, " (", model_index, "/",
          nrow(model_specs), ")...")
  sample_panel <- stack_panel[model_id == model_id_value]
  sample_manifest <- stack_manifest[model_id == model_id_value]
  expected_stacks <- nrow(sample_manifest)
  design_row <- expected_design[model_id == model_id_value]
  model_event_times <- event_times_for_lead(meta$cohort_lead_years)

  setup <- synthdid::panel.matrices(
    as.data.frame(sample_panel[, .(
      stack_unit_id, event_time, outcome_value, treatment
    )]),
    unit = "stack_unit_id", time = "event_time", outcome = "outcome_value",
    treatment = "treatment", treated.last = TRUE
  )
  matrix_event_times <- as.integer(colnames(setup$Y))
  expected_n0 <- expected_stacks * expected_donors
  expected_n1 <- expected_stacks
  if (setup$N0 != expected_n0 || nrow(setup$Y) - setup$N0 != expected_n1 ||
      setup$T0 != design_row$expected_pre_periods ||
      ncol(setup$Y) - setup$T0 != design_row$expected_post_periods ||
      !identical(matrix_event_times, model_event_times)) {
    stop("Unexpected pooled synthdid matrix layout for ", model_id, ".")
  }

  pre_controls <- setup$Y[
    seq_len(setup$N0), seq_len(setup$T0), drop = FALSE
  ]
  raw_preperiod_noise <- sd(apply(pre_controls, 1L, diff))
  if (!is.finite(raw_preperiod_noise) || raw_preperiod_noise <= tolerance) {
    stop("Pooled controls have no informative pre-period variation for ",
         model_id, ".")
  }

  fit <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
  att <- as.numeric(fit)
  observed_curve <- as.numeric(synthdid::synthdid_effect_curve(fit))
  post_event_times <- matrix_event_times[
    (setup$T0 + 1L):length(matrix_event_times)
  ]
  if (!is.finite(att) || length(observed_curve) != length(post_event_times) ||
      any(!is.finite(observed_curve))) {
    stop("Invalid pooled point estimate for ", model_id, ".")
  }

  control_units <- unique(sample_panel[role == "control", .(
    stack_id, stack_unit_id, physical_lgd_id, unit_name, donor_rank
  )])
  if (control_units[, any(.N != expected_donors), by = stack_id]$V1 |> any()) {
    stop("A pooled placebo block does not contain 30 donors for ", model_id, ".")
  }

  model_placebo_att <- list()
  model_placebo_dynamic <- list()
  model_placebo_status <- list()
  model_placebo_assignments <- list()
  for (rep_id in seq_len(placebo_replications)) {
    rep_seed <- base_seed + model_index * 100000L + rep_id
    set.seed(rep_seed)
    chosen <- control_units[, .SD[sample.int(.N, 1L)], by = stack_id]
    chosen[, `:=`(
      model_id = model_id,
      placebo_replication = rep_id,
      placebo_seed = rep_seed
    )]
    model_placebo_assignments[[rep_id]] <- chosen

    placebo_panel <- copy(sample_panel[role == "control", .(
      stack_id, stack_unit_id, event_time, outcome_value
    )])
    placebo_panel[, treatment := as.integer(
      stack_unit_id %in% chosen$stack_unit_id & event_time >= 0L
    )]
    placebo_setup <- tryCatch(
      synthdid::panel.matrices(
        as.data.frame(placebo_panel[, .(
          stack_unit_id, event_time, outcome_value, treatment
        )]),
        unit = "stack_unit_id", time = "event_time",
        outcome = "outcome_value", treatment = "treatment",
        treated.last = TRUE
      ),
      error = function(e) e
    )
    if (inherits(placebo_setup, "error") ||
        placebo_setup$N0 != expected_stacks * (expected_donors - 1L) ||
        nrow(placebo_setup$Y) - placebo_setup$N0 != expected_stacks ||
        placebo_setup$T0 != setup$T0) {
      setup_error <- if (inherits(placebo_setup, "error")) {
        conditionMessage(placebo_setup)
      } else {
        "Block-placebo matrix dimensions are invalid."
      }
      model_placebo_status[[rep_id]] <- data.table(
        model_id = model_id,
        placebo_replication = rep_id,
        placebo_seed = rep_seed,
        successful = FALSE,
        error = setup_error
      )
      next
    }

    placebo_fit <- tryCatch(
      synthdid::synthdid_estimate(
        placebo_setup$Y, placebo_setup$N0, placebo_setup$T0
      ),
      error = function(e) e
    )
    if (inherits(placebo_fit, "error")) {
      model_placebo_status[[rep_id]] <- data.table(
        model_id = model_id,
        placebo_replication = rep_id,
        placebo_seed = rep_seed,
        successful = FALSE,
        error = conditionMessage(placebo_fit)
      )
      next
    }
    placebo_curve <- as.numeric(
      synthdid::synthdid_effect_curve(placebo_fit)
    )
    if (length(placebo_curve) != length(post_event_times) ||
        any(!is.finite(placebo_curve))) {
      model_placebo_status[[rep_id]] <- data.table(
        model_id = model_id,
        placebo_replication = rep_id,
        placebo_seed = rep_seed,
        successful = FALSE,
        error = "Block-placebo effect-curve length or values are invalid."
      )
      next
    }

    model_placebo_status[[rep_id]] <- data.table(
      model_id = model_id,
      placebo_replication = rep_id,
      placebo_seed = rep_seed,
      successful = TRUE,
      error = NA_character_
    )
    model_placebo_att[[rep_id]] <- data.table(
      model_id = model_id,
      cohort_lead_years = meta$cohort_lead_years,
      decade_definition = meta$decade_definition,
      placebo_replication = rep_id,
      placebo_seed = rep_seed,
      placebo_att = as.numeric(placebo_fit)
    )
    model_placebo_dynamic[[rep_id]] <- data.table(
      model_id = model_id,
      cohort_lead_years = meta$cohort_lead_years,
      decade_definition = meta$decade_definition,
      placebo_replication = rep_id,
      placebo_seed = rep_seed,
      event_time = post_event_times,
      event_time_decades = post_event_times / 10,
      placebo_effect = placebo_curve
    )
  }

  model_placebo_att <- rbindlist(model_placebo_att, fill = TRUE)
  model_placebo_dynamic <- rbindlist(model_placebo_dynamic, fill = TRUE)
  model_placebo_status <- rbindlist(model_placebo_status, fill = TRUE)
  model_placebo_assignments <- rbindlist(
    model_placebo_assignments, fill = TRUE
  )
  successful_placebos <- model_placebo_status[successful == TRUE, .N]
  if (successful_placebos < minimum_successful_placebos) {
    stop("Only ", successful_placebos, " successful block placebos for ",
         model_id, ".")
  }

  att_se <- sd(model_placebo_att$placebo_att)
  empirical_p <- (
    1 + sum(abs(model_placebo_att$placebo_att) >= abs(att))
  ) / (1 + successful_placebos)
  dynamic_inference <- model_placebo_dynamic[, .(
    se = sd(placebo_effect),
    successful_placebos = uniqueN(placebo_replication),
    empirical_p_value = (
      1 + sum(abs(placebo_effect) >= abs(observed_curve[
        match(event_time, post_event_times)
      ]))
    ) / (1 + .N)
  ), by = .(event_time, event_time_decades)]

  dynamic <- data.table(
    model_id = model_id,
    treatment_mode = "host",
    cohort_lead_years = meta$cohort_lead_years,
    decade_definition = meta$decade_definition,
    matching_specification = meta$matching_specification,
    outcome = outcome_name,
    event_time = post_event_times,
    event_time_decades = post_event_times / 10,
    effect = observed_curve
  )
  dynamic <- merge(
    dynamic, dynamic_inference,
    by = c("event_time", "event_time_decades"),
    all.x = TRUE, sort = FALSE
  )
  dynamic[, `:=`(
    ci_low = effect - 1.96 * se,
    ci_high = effect + 1.96 * se,
    se_method = "within_stack_block_placebo_sd"
  )]
  setorder(dynamic, event_time)

  omega <- extract_weights(fit, "omega", "stack_unit_id")
  omega <- merge(
    unique(sample_panel[role == "control", .(
      stack_unit_id, stack_id, physical_lgd_id, unit_name, unit_type,
      treated_lgd_id, treated_lgd_name, donor_rank, match_distance,
      distance_from_first_fair_venue_km, donor_population_ratio
    )]),
    omega,
    by = "stack_unit_id",
    all.x = TRUE,
    sort = FALSE
  )
  omega[is.na(weight), weight := 0]
  omega[, `:=`(
    model_id = model_id,
    cohort_lead_years = meta$cohort_lead_years,
    decade_definition = meta$decade_definition,
    matching_specification = meta$matching_specification,
    weight_type = "omega"
  )]
  setorder(omega, -weight, stack_unit_id)
  if (abs(sum(omega$weight) - 1) > tolerance) {
    stop("Pooled control weights do not sum to one for ", model_id, ".")
  }

  physical_omega <- omega[, .(
    weight = sum(weight),
    stack_copies = .N,
    positive_weight_stack_copies = sum(weight > tolerance),
    maximum_stack_weight = max(weight),
    treated_stacks_represented = uniqueN(treated_lgd_id)
  ), by = .(
    model_id, cohort_lead_years, decade_definition,
    matching_specification, physical_lgd_id, unit_name, unit_type
  )]
  setorder(physical_omega, -weight, physical_lgd_id)

  lambda <- extract_weights(fit, "lambda", "event_time")
  lambda[, event_time := as.integer(event_time)]
  lambda <- merge(
    data.table(event_time = model_event_times[model_event_times < 0L]),
    lambda,
    by = "event_time", all.x = TRUE, sort = FALSE
  )
  lambda[is.na(weight), weight := 0]
  lambda[, `:=`(
    model_id = model_id,
    cohort_lead_years = meta$cohort_lead_years,
    decade_definition = meta$decade_definition,
    matching_specification = meta$matching_specification,
    weight_type = "lambda"
  )]
  setorder(lambda, event_time)
  if (abs(sum(lambda$weight) - 1) > tolerance) {
    stop("Pooled time weights do not sum to one for ", model_id, ".")
  }

  control_matrix <- setup$Y[seq_len(setup$N0), , drop = FALSE]
  treated_matrix <- setup$Y[
    (setup$N0 + 1L):nrow(setup$Y), , drop = FALSE
  ]
  omega_lookup <- setNames(omega$weight, omega$stack_unit_id)
  ordered_omega <- omega_lookup[rownames(control_matrix)]
  if (anyNA(ordered_omega)) {
    stop("Cannot align pooled control weights for ", model_id, ".")
  }
  treated_average_path <- colMeans(treated_matrix)
  synthetic_path <- as.numeric(crossprod(ordered_omega, control_matrix))
  fit_path <- data.table(
    model_id = model_id,
    treatment_mode = "host",
    cohort_lead_years = meta$cohort_lead_years,
    decade_definition = meta$decade_definition,
    matching_specification = meta$matching_specification,
    outcome = outcome_name,
    event_time = matrix_event_times,
    event_time_decades = matrix_event_times / 10,
    period = ifelse(matrix_event_times < 0L, "pre", "post"),
    treated_average_outcome = treated_average_path,
    synthetic_outcome = synthetic_path,
    treated_minus_synthetic = treated_average_path - synthetic_path
  )
  pre_rmspe <- fit_path[period == "pre", sqrt(mean(
    treated_minus_synthetic^2
  ))]

  status_results[[model_id]] <- data.table(
    model_id = model_id,
    treatment_mode = "host",
    cohort_lead_years = meta$cohort_lead_years,
    decade_definition = meta$decade_definition,
    matching_specification = meta$matching_specification,
    outcome = outcome_name,
    model_status = "estimated",
    error = NA_character_,
    treated_weighting = "equal_lgd",
    event_time_min = min(model_event_times),
    event_time_max = max(model_event_times),
    n_control_stack_units = setup$N0,
    n_treated_lgds = nrow(setup$Y) - setup$N0,
    n_pre_periods = setup$T0,
    n_post_periods = ncol(setup$Y) - setup$T0,
    physical_control_lgds = uniqueN(omega$physical_lgd_id),
    raw_preperiod_noise_level = raw_preperiod_noise,
    placebo_replications_requested = placebo_replications,
    placebo_replications_successful = successful_placebos,
    minimum_successful_placebos_required = minimum_successful_placebos,
    model_seed = base_seed + model_index * 100000L
  )
  att_results[[model_id]] <- data.table(
    model_id = model_id,
    treatment_mode = "host",
    cohort_lead_years = meta$cohort_lead_years,
    decade_definition = meta$decade_definition,
    matching_specification = meta$matching_specification,
    outcome = outcome_name,
    treated_weighting = "equal_lgd",
    event_time_min = min(model_event_times),
    event_time_max = max(model_event_times),
    estimate = att,
    se = att_se,
    ci_low = att - 1.96 * att_se,
    ci_high = att + 1.96 * att_se,
    empirical_p_value = empirical_p,
    se_method = "within_stack_block_placebo_sd",
    ci_method = "normal_1.96_block_placebo_sd",
    placebo_replications = successful_placebos,
    n_control_stack_units = setup$N0,
    n_treated_lgds = nrow(setup$Y) - setup$N0,
    n_pre_periods = setup$T0,
    n_post_periods = ncol(setup$Y) - setup$T0,
    pre_rmspe = pre_rmspe
  )

  dynamic_results[[model_id]] <- dynamic
  placebo_att_results[[model_id]] <- model_placebo_att
  placebo_dynamic_results[[model_id]] <- model_placebo_dynamic
  placebo_status_results[[model_id]] <- model_placebo_status
  placebo_assignment_results[[model_id]] <- model_placebo_assignments
  control_weight_results[[model_id]] <- omega
  physical_weight_results[[model_id]] <- physical_omega
  time_weight_results[[model_id]] <- lambda
  fit_path_results[[model_id]] <- fit_path
  model_objects[[model_id]] <- list(
    estimate = fit,
    setup = setup,
    metadata = as.list(meta),
    event_times = model_event_times,
    outcome = outcome_name,
    treated_weighting = "equal_lgd",
    inference = "within_stack_block_placebo"
  )
}

###############################################################################
# Assemble, validate, and write outputs
###############################################################################

status <- rbindlist(status_results, fill = TRUE)
att <- rbindlist(att_results, fill = TRUE)
dynamic <- rbindlist(dynamic_results, fill = TRUE)
placebo_att <- rbindlist(placebo_att_results, fill = TRUE)
placebo_dynamic <- rbindlist(placebo_dynamic_results, fill = TRUE)
placebo_status <- rbindlist(placebo_status_results, fill = TRUE)
placebo_assignments <- rbindlist(placebo_assignment_results, fill = TRUE)
control_weights <- rbindlist(control_weight_results, fill = TRUE)
physical_weights <- rbindlist(physical_weight_results, fill = TRUE)
time_weights <- rbindlist(time_weight_results, fill = TRUE)
fit_paths <- rbindlist(fit_path_results, fill = TRUE)

setorder(status, cohort_lead_years, decade_definition)
setorder(att, cohort_lead_years, decade_definition)
setorder(dynamic, cohort_lead_years, decade_definition, event_time)
setorder(placebo_att, model_id, placebo_replication)
setorder(placebo_dynamic, model_id, placebo_replication, event_time)
setorder(placebo_status, model_id, placebo_replication)
setorder(placebo_assignments, model_id, placebo_replication, stack_id)
setorder(control_weights, model_id, -weight, stack_unit_id)
setorder(physical_weights, model_id, -weight, physical_lgd_id)
setorder(time_weights, model_id, event_time)
setorder(fit_paths, model_id, event_time)

control_weight_qc <- control_weights[, .(weight_sum = sum(weight)), by = model_id]
physical_weight_qc <- physical_weights[, .(weight_sum = sum(weight)), by = model_id]
time_weight_qc <- time_weights[, .(weight_sum = sum(weight)), by = model_id]
successful_placebo_qc <- placebo_status[, .(
  successful_placebos = sum(successful)
), by = model_id]
assignment_qc <- placebo_assignments[, .(
  stacks_assigned = uniqueN(stack_id),
  duplicate_stack_assignments = anyDuplicated(stack_id)
), by = .(model_id, placebo_replication)]

if (nrow(status) != 4L || any(status$model_status != "estimated") ||
    nrow(att) != 4L || any(!is.finite(att$estimate)) ||
    any(!is.finite(att$se)) ||
    nrow(dynamic) != sum(expected_design$expected_post_periods) ||
    nrow(fit_paths) != sum(expected_design$expected_periods) ||
    nrow(time_weights) != sum(expected_design$expected_pre_periods) ||
    nrow(control_weights) != sum(expected_design$expected_control_stack_units) ||
    any(abs(control_weight_qc$weight_sum - 1) > tolerance) ||
    any(abs(physical_weight_qc$weight_sum - 1) > tolerance) ||
    any(abs(time_weight_qc$weight_sum - 1) > tolerance) ||
    any(successful_placebo_qc$successful_placebos <
          minimum_successful_placebos) ||
    any(assignment_qc$duplicate_stack_assignments != 0L) ||
    any(assignment_qc$stacks_assigned != expected_design[
      match(assignment_qc$model_id, model_id), expected_treated_stacks
    ])) {
  stop("Final pooled synthetic-DiD validation failed.")
}

qc <- merge(
  panel_dimensions,
  successful_placebo_qc,
  by = "model_id", all.x = TRUE, sort = FALSE
)
qc <- merge(
  qc,
  control_weight_qc[, .(
    model_id, control_weight_sum_error = abs(weight_sum - 1)
  )],
  by = "model_id", all.x = TRUE, sort = FALSE
)
qc <- merge(
  qc,
  physical_weight_qc[, .(
    model_id, physical_weight_sum_error = abs(weight_sum - 1)
  )],
  by = "model_id", all.x = TRUE, sort = FALSE
)
qc <- merge(
  qc,
  time_weight_qc[, .(
    model_id, time_weight_sum_error = abs(weight_sum - 1)
  )],
  by = "model_id", all.x = TRUE, sort = FALSE
)
qc[, `:=`(
  matching_specification = expected_matching_specification,
  n_pre_periods = expected_pre_periods,
  n_post_periods = expected_post_periods,
  maximum_outcome_identity_error = panel_identity_error
)]
setcolorder(qc, c(
  "model_id", "cohort_lead_years", "decade_definition",
  "matching_specification", "treated_stacks", "control_stack_units",
  "physical_control_lgds", "panel_rows", "n_pre_periods",
  "n_post_periods", "successful_placebos", "control_weight_sum_error",
  "physical_weight_sum_error", "time_weight_sum_error",
  "maximum_outcome_identity_error",
  setdiff(names(qc), c(
    "model_id", "cohort_lead_years", "decade_definition",
    "matching_specification", "treated_stacks", "control_stack_units",
    "physical_control_lgds", "panel_rows", "n_pre_periods",
    "n_post_periods", "successful_placebos", "control_weight_sum_error",
    "physical_weight_sum_error", "time_weight_sum_error",
    "maximum_outcome_identity_error"
  ))
))
setorder(qc, cohort_lead_years, decade_definition)

fwrite(stack_panel, output_files$panel, na = "")
fwrite(stack_manifest, output_files$manifest, na = "")
fwrite(status, output_files$status, na = "")
fwrite(att, output_files$att, na = "")
fwrite(dynamic, output_files$dynamic, na = "")
fwrite(placebo_att, output_files$placebo_att, na = "")
fwrite(placebo_dynamic, output_files$placebo_dynamic, na = "")
fwrite(placebo_status, output_files$placebo_status, na = "")
fwrite(placebo_assignments, output_files$placebo_assignments, na = "")
fwrite(control_weights, output_files$control_weights_stack, na = "")
fwrite(physical_weights, output_files$control_weights_physical, na = "")
fwrite(time_weights, output_files$time_weights, na = "")
fwrite(fit_paths, output_files$fit_paths, na = "")
saveRDS(model_objects, output_files$models)
fwrite(qc, output_files$qc, na = "")

###############################################################################
# Figures
###############################################################################

att_plot <- copy(att)
att_plot[, specification := factor(
  paste0("Lead ", cohort_lead_years, " — ", decade_definition),
  levels = rev(c(
    "Lead 0 — standard", "Lead 0 — alternative",
    "Lead 20 — standard", "Lead 20 — alternative"
  ))
)]
forest_plot <- ggplot(att_plot, aes(y = specification, x = estimate)) +
  geom_vline(xintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high), width = 0.18,
    orientation = "y", color = "#1b6ca8"
  ) +
  geom_point(color = "#1b6ca8", size = 2.2) +
  labs(
    title = "Pooled event-time synthetic-DiD estimates",
    subtitle = "Equal-weight treated LGDs; 95% block-placebo intervals",
    x = "Effect per 100,000 population per year", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title.position = "plot")
ggsave(output_files$forest, forest_plot, width = 8.5, height = 4.8, dpi = 200)

dynamic_plot_data <- copy(dynamic)
dynamic_plot_data[, specification := paste0(
  "Lead ", cohort_lead_years, " — ", decade_definition
)]
dynamic_plot <- ggplot(
  dynamic_plot_data, aes(x = event_time_decades, y = effect)
) +
  geom_hline(yintercept = 0, color = "grey55") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "#1b6ca8", alpha = 0.18) +
  geom_line(color = "#1b6ca8", linewidth = 0.8) +
  geom_point(color = "#1b6ca8", size = 1.8) +
  facet_wrap(~ specification, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = 0:4) +
  labs(
    title = "Pooled post-treatment effect curves",
    subtitle = "Within-stack block-placebo intervals",
    x = "Decades since treatment", y = "Effect per 100,000 per year"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title.position = "plot")
ggsave(
  output_files$dynamic_figure, dynamic_plot,
  width = 9.5, height = 7, dpi = 200
)

fit_plot_data <- melt(
  fit_paths,
  id.vars = c("model_id", "cohort_lead_years", "decade_definition",
              "event_time_decades"),
  measure.vars = c("treated_average_outcome", "synthetic_outcome"),
  variable.name = "series", value.name = "outcome_value"
)
fit_plot_data[, `:=`(
  specification = paste0(
    "Lead ", cohort_lead_years, " — ", decade_definition
  ),
  series = factor(
    series,
    levels = c("treated_average_outcome", "synthetic_outcome"),
    labels = c("Treated-LGD average", "Synthetic control")
  )
)]
fit_plot <- ggplot(
  fit_plot_data,
  aes(x = event_time_decades, y = outcome_value,
      color = series, linetype = series)
) +
  geom_vline(xintercept = 0, color = "grey55", linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.7) +
  facet_wrap(~ specification, ncol = 2, scales = "free_y") +
  scale_color_manual(values = c(
    "Treated-LGD average" = "#1b6ca8",
    "Synthetic control" = "#c44e52"
  )) +
  labs(
    title = "Pooled treated and synthetic paths",
    x = "Decades relative to treatment",
    y = "Annual Discovery/Science births per 100,000",
    color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", plot.title.position = "plot")
ggsave(
  output_files$fit_figure, fit_plot,
  width = 9.5, height = 7, dpi = 200
)

message("Pooled event-time synthetic-DiD estimation complete.")
message("  Models estimated: ", nrow(att))
message("  Stacked panel: ", output_files$panel)
message("  ATT results: ", output_files$att)
message("  QC: ", output_files$qc)
print(att[, .(
  model_id, n_treated_lgds, estimate, se, ci_low, ci_high,
  empirical_p_value, placebo_replications
)])
