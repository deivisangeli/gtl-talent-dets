###############################################################################
# Pool the treated-LGD synthetic-DiD case studies for the fixed-1911 England
# and Wales world-fairs design, following
# docs/generic_pooled_synthetic_control_plan.md (Dube and Zipperer 2015;
# Abadie, Diamond and Hainmueller 2010).
#
# One synthetic control per treated event is estimated on that event's own
# donor pool, and the event-level estimates are pooled afterwards. This is a
# different estimator from estimate_ukds_1911_lgd_wikipedia_pooled_synthdid.R,
# which stacks every treated LGD into a single simultaneous-adoption matrix.
#
# Pipeline (plan sections in brackets):
#   1  refit each treated event and form the gap path g_et                 [3]
#   2  pre-treatment RMSPE and a scale-normalized NRMSPE                   [4]
#   3  event effect tau_e = mean post gap relative to final pre gap        [6]
#   4  standardized effect S_e = tau_e / RMSPE_pre and R_e = post/pre      [7,8]
#   5  in-space placebos over every donor in the pool                      [9]
#   6  placebo p-values, fit-restricted placebo sets, percentile ranks     [10,11]
#   7  equal-weighted pooled effect and dynamic effect path                [12.1]
#   8  mean percentile rank r-bar and mean standardized effect S-bar       [12.2]
#   9  donor-sharing diagnostics N_j, C_j and the overlap matrix O_ee'     [14]
#  10  joint randomization inference preserving donor dependence           [15,16,17]
#  11  progressive trimming, leave-one-event-out, leave-shared-donor-out,
#      and placebo-in-time robustness                                      [18]
#  12  the event-time path recomputed at every trim stage, so the dynamics can
#      be checked for robustness rather than only the single pooled average
#
# The progressive-trimming rule is reproduced inside every randomization draw,
# so each trim step carries its own joint p-value rather than only the first
# two. The baseline reported statistic remains the full treated-event sample:
# the trimming path is a diagnostic showing the result at every cutoff, not a
# screen used to choose one.
#
# Event inclusion uses PRE-treatment information only. The baseline retains
# every event that the per-event stage estimated; progressive trimming on the
# normalized pre-treatment fit is reported alongside rather than as a screen.
#
# Some treated LGDs have an identically zero pre-treatment outcome path. Their
# raw pre-treatment RMSPE is near zero, so the standardized effect
# tau_e / RMSPE_pre_e explodes, which is exactly the scale problem plan
# section 4 warns about. Two things follow. First, the normalized fit measure
# divides by the mean absolute PRE-treatment outcome of the whole event panel
# (treated unit plus its donors) rather than of the treated unit alone, so it
# stays defined and uses no post-treatment information. Second, an
# "informative pre-treatment path" variant that drops events whose treated
# pre-period outcome is identically zero is reported alongside the baseline,
# with the same screen applied inside the randomization draws. The
# pre-specified pooled statistic is the mean percentile rank, which is bounded
# and therefore unaffected by the standardized-effect blow-up.
#
# The mean percentile rank does not have a null expectation of exactly 0.5. An
# observed rank divides by J+1 = 31 while a placebo rank divides by 30, so
# under exchangeability both centre near 16/31 = 0.516. Two-sided
# randomization p-values are therefore centred on the empirical null mean of
# the draws rather than on 0.5, and the one-sided p-value is reported as well
# because it does not depend on the centring at all.
#
# Unlike the stacked pooled design, no common event window is required, so
# Lambeth is retained under the zero cohort lead.
#
# ESTIMAND. Event-time paths use the final pre-treatment period as the omitted
# reference. If g_et is the treated-minus-synthetic gap and t0 is the final pre
# period, the reported path is g_et - g_e,t0 and the event effect is its mean
# over the post-treatment periods. Under the preferred balanced window t0 is
# event time zero, so the plotted effect is exactly zero there and treatment
# begins at +10. The synthdid package ATT and its lambda-weighted pre-period
# offset are retained as diagnostics, but they are not the reported estimand.
#
# Run from the repository root, after the donor pools and the per-event models:
#   Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_pooled_dube_zipperer.R
#
# Under the balanced_4pre_3post event window each event contributes exactly
# four pre and three post decade bins, with the fair decade as the last pre
# period and treatment starting at T+10. tau_e is then the mean
# reference-normalized gap over the same three horizons for every event, and
# the dynamic effect path has a constant
# event count at all seven event times, which the full-window run does not.
#
# The stage does not need the per-event models to exist. It applies the same
# zero-pre-period-variance feasibility gate itself and derives the event list
# from the donor pools, so a window that has no per-event run can still be
# pooled. When the per-event outputs are present for the same window, the refit
# is reconciled against them.
#
# Optional overrides:
#   SYNTHDID_MATCHING_SPEC=pooled_rate_density
#   SYNTHDID_EVENT_WINDOW=full
#   SYNTHDID_DZ_RANDOMIZATION_DRAWS=1000
#   SYNTHDID_DZ_INFLUENTIAL_DONORS=5
#   SYNTHDID_BASE_SEED=20260819
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
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/", mustWork = TRUE
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
donor_base_dir <- file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", "synthdid",
  "ukds_1911_lgd_wikipedia_donor_pools"
)

matching_spec_id <- wf_matching_spec_id()
expected_matching_specification <- wf_matching_spec_string(matching_spec_id)
matching_donor_dir <- wf_matching_spec_dir(donor_base_dir, matching_spec_id)

event_window_id <- wf_event_window_id(matching_spec_id)
event_window <- wf_event_window_spec(event_window_id)
results_dir <- wf_window_output_dir(
  matching_donor_dir, "pooled_dz", matching_spec_id, event_window_id
)
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

cohort_leads <- c(0L, 20L)
lead_dirs <- vapply(cohort_leads, function(lead) {
  if (lead == 0L) matching_donor_dir else file.path(
    matching_donor_dir, paste0("cohort_lead_", lead, "y")
  )
}, character(1L))
names(lead_dirs) <- as.character(cohort_leads)

randomization_draws <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_DZ_RANDOMIZATION_DRAWS", unset = "1000"
)))
influential_donor_count <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_DZ_INFLUENTIAL_DONORS", unset = "5"
)))
base_seed <- suppressWarnings(as.integer(Sys.getenv(
  "SYNTHDID_BASE_SEED", unset = "20260819"
)))
if (is.na(randomization_draws) || randomization_draws < 50L)
  stop("SYNTHDID_DZ_RANDOMIZATION_DRAWS must be an integer of at least 50.")
if (is.na(influential_donor_count) || influential_donor_count < 1L)
  stop("SYNTHDID_DZ_INFLUENTIAL_DONORS must be a positive integer.")
if (is.na(base_seed)) stop("SYNTHDID_BASE_SEED must be an integer.")

outcome_name <- "discovery_science_births_per_100k_population_year"
tolerance <- 1e-10
expected_donors <- 30L
placebo_fit_cutoffs <- c(2, 5, 10, 20)
placebo_in_time_shift <- 20L
pooled_statistic <- "mean_placebo_percentile_rank"

required_inputs <- c(panel_files, unlist(lapply(lead_dirs, function(d) c(
  file.path(d, "donor_pool_pairs.csv"),
  file.path(d, "cohort_timing_status.csv")
))))
missing <- required_inputs[!file.exists(required_inputs)]
if (length(missing)) {
  stop("Missing inputs. Run the donor-pool stage first:\n",
       paste(missing, collapse = "\n"))
}

# The per-event stage is optional. When its outputs exist for this window the
# refit is reconciled against them; otherwise the reconciliation is skipped and
# recorded as such.
stage2_files <- unlist(lapply(lead_dirs, function(d) c(
  file.path(d, "models_wikipedia_rate", "synthdid_model_status.csv"),
  file.path(d, "models_wikipedia_rate", "synthdid_att.csv")
)))
stage2_available <- all(file.exists(stage2_files))
reconcile_with_stage2 <- stage2_available &&
  identical(event_window_id, wf_matching_spec_native_window(matching_spec_id))

###############################################################################
# Outputs
###############################################################################

event_effects_file <- file.path(results_dir, "dz_event_effects.csv")
feasibility_file <- file.path(results_dir, "dz_event_feasibility.csv")
placebo_events_file <- file.path(results_dir, "dz_placebo_events.csv")
placebo_paths_file <- file.path(results_dir, "dz_placebo_gap_paths.csv")
placebo_restrict_file <- file.path(
  results_dir, "dz_placebo_fit_restrictions.csv"
)
pooled_att_file <- file.path(results_dir, "dz_pooled_att.csv")
pooled_dynamic_file <- file.path(
  results_dir, "dz_pooled_dynamic_effects.csv"
)
pooled_dynamic_trim_file <- file.path(
  results_dir, "dz_pooled_dynamic_by_trim.csv"
)
event_paths_file <- file.path(results_dir, "dz_event_gap_paths.csv")
event_horizon_file <- file.path(results_dir, "dz_event_horizon_effects.csv")
trimming_file <- file.path(results_dir, "dz_progressive_trimming.csv")
loeo_file <- file.path(results_dir, "dz_leave_one_event_out.csv")
leave_donor_file <- file.path(results_dir, "dz_leave_donor_out.csv")
placebo_time_file <- file.path(results_dir, "dz_placebo_in_time.csv")
donor_usage_file <- file.path(results_dir, "dz_donor_usage.csv")
donor_overlap_file <- file.path(results_dir, "dz_donor_overlap_matrix.csv")
randomization_file <- file.path(results_dir, "dz_randomization_draws.csv")
table1_file <- file.path(results_dir, "dz_table1_events.csv")
table2_file <- file.path(results_dir, "dz_table2_fit_weights.csv")
table3_file <- file.path(results_dir, "dz_table3_event_effects.csv")
table4_file <- file.path(results_dir, "dz_table4_pooled_robustness.csv")
decisions_file <- file.path(results_dir, "dz_pre_analysis_decisions.csv")
qc_file <- file.path(results_dir, "dz_qc.csv")
models_file <- file.path(results_dir, "dz_models.rds")

###############################################################################
# Helpers
###############################################################################

# Fit one synthetic DiD and return the treated-minus-synthetic gap path.
# Rows 1..N0 of Y are controls and the last row is the treated unit, matching
# the layout produced by synthdid::panel.matrices(treated.last = TRUE).
fit_gap_path <- function(Y, N0, T0) {
  fit <- tryCatch(
    synthdid::synthdid_estimate(Y, N0, T0), error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(ok = FALSE, error = conditionMessage(fit)))
  }
  weights <- attr(fit, "weights")
  omega <- as.numeric(weights$omega)
  if (length(omega) != N0 || any(!is.finite(omega)) ||
      abs(sum(omega) - 1) > 1e-8) {
    return(list(ok = FALSE, error = "Donor weights do not sum to one."))
  }
  controls <- Y[seq_len(N0), , drop = FALSE]
  synthetic <- as.numeric(crossprod(omega, controls))
  treated <- as.numeric(Y[nrow(Y), ])
  gap <- treated - synthetic
  pre <- seq_len(T0)
  post <- (T0 + 1L):length(gap)
  rmspe_pre <- sqrt(mean(gap[pre]^2))
  mean_abs_pre <- mean(abs(treated[pre]))
  # Retain synthdid's lambda-weighted level adjustment as a diagnostic, but use
  # the final pre-treatment gap as the event-study reference period.
  lambda <- as.numeric(weights$lambda)
  lambda_pre_gap <- if (length(lambda) == T0) sum(lambda * gap[pre]) else
    mean(gap[pre])
  gap_lambda_adjusted <- gap - lambda_pre_gap
  reference_pre_gap <- gap[[T0]]
  gap_adjusted <- gap - reference_pre_gap
  att <- as.numeric(fit)
  tau_reference_normalized <- mean(gap_adjusted[post])
  # Panel-wide pre-treatment scale: strictly pre-treatment, and non-zero even
  # when the treated unit itself never records an outcome before treatment.
  mean_abs_pre_panel <- mean(abs(Y[, pre, drop = FALSE]))
  list(
    ok = TRUE,
    fit = fit,
    omega = omega,
    lambda = lambda,
    lambda_pre_gap = lambda_pre_gap,
    reference_pre_gap = reference_pre_gap,
    treated_path = treated,
    synthetic_path = synthetic,
    gap = gap,
    gap_adjusted = gap_adjusted,
    gap_lambda_adjusted = gap_lambda_adjusted,
    pre_index = pre,
    post_index = post,
    att_synthdid = att,
    mean_post_gap = mean(gap[post]),
    mean_pre_gap = mean(gap[pre]),
    # Primary event effect: mean post gap relative to the final pre-period gap.
    tau = tau_reference_normalized,
    rmspe_pre = rmspe_pre,
    rmspe_post = sqrt(mean(gap[post]^2)),
    mean_abs_pre = mean_abs_pre,
    mean_abs_pre_panel = mean_abs_pre_panel,
    pre_all_zero = is.finite(mean_abs_pre) && mean_abs_pre <= 0,
    nrmspe_pre = if (is.finite(mean_abs_pre_panel) && mean_abs_pre_panel > 0)
      rmspe_pre / mean_abs_pre_panel else NA_real_,
    nrmspe_pre_treated_scale = if (is.finite(mean_abs_pre) && mean_abs_pre > 0)
      rmspe_pre / mean_abs_pre else NA_real_,
    standardized = if (is.finite(rmspe_pre) && rmspe_pre > 0)
      tau_reference_normalized / rmspe_pre else NA_real_,
    post_pre_ratio = if (is.finite(rmspe_pre) && rmspe_pre > 0)
      sqrt(mean(gap[post]^2)) / rmspe_pre else NA_real_
  )
}

# Build the synthdid panel exactly as the per-event stage does, so the solver
# sees the same row order and reproduces the same numbers to machine precision.
# treatment_decade is the fair decade T. Under the balanced window the sample is
# the seven bins from T-30 to T+30 and treatment turns on at T+10, so the fair
# decade is the last pre-treatment period. Pass `decades` to override the
# window explicitly, as the placebo-in-time test does.
build_setup <- function(panel, donor_ids, treated_id, treatment_decade,
                        decades = NULL, treatment_start = NULL,
                        window = event_window) {
  unit_ids <- c(donor_ids, treated_id)
  sample <- panel[analysis_lgd_id %in% unit_ids, .(
    unit_id = analysis_lgd_id, decade, outcome_value
  )]
  if (is.null(decades)) decades <- wf_event_window_decades(
    treatment_decade, window
  )
  if (is.null(treatment_start)) treatment_start <-
    wf_event_window_treatment_start(treatment_decade, window)
  if (!is.null(decades)) {
    if (!all(decades %in% sample$decade))
      stop("The event window is not fully observed for treated unit ",
           treated_id, ".")
    sample <- sample[decade %in% decades]
  }
  sample[, treatment := as.integer(
    unit_id == treated_id & decade >= treatment_start
  )]
  if (anyNA(sample) || any(!is.finite(sample$outcome_value)))
    stop("Incomplete outcome panel for treated unit ", treated_id, ".")
  setup <- synthdid::panel.matrices(
    as.data.frame(sample[, .(unit_id, decade, outcome_value, treatment)]),
    unit = "unit_id", time = "decade", outcome = "outcome_value",
    treatment = "treatment", treated.last = TRUE
  )
  if (setup$N0 != length(donor_ids) || nrow(setup$Y) - setup$N0 != 1L)
    stop("Unexpected matrix layout for treated unit ", treated_id, ".")
  setup
}

# Means after progressively dropping the worst-fitting entries. Input must
# already be ordered best-fitting first; element k+1 is the mean with the k
# worst dropped.
cumulative_trim_means <- function(x) {
  n <- length(x)
  if (!n) return(numeric(0))
  vapply(seq_len(n), function(keep) mean(x[seq_len(keep)]), numeric(1L))[n:1L]
}

# Column-wise version of the same idea for an events-by-horizon matrix: rows
# must already be ordered best-fitting first, and element k+1 of the result is
# the column mean with the k worst-fitting rows dropped.
cumulative_trim_colmeans <- function(m) {
  n <- nrow(m)
  if (!n) return(list())
  out <- lapply(seq_len(n), function(keep)
    colMeans(m[seq_len(keep), , drop = FALSE], na.rm = TRUE))
  out[n:1L]
}

# Largest absolute null deviation accepted by the finite-sample plus-one
# randomization test at level alpha. This makes zero-in-CI agree exactly with
# the corresponding two-sided randomization decision, including the discrete
# boundary induced by a finite number of draws.
randomization_critical_value <- function(x, centre, alpha = 0.05) {
  deviations <- sort(abs(x[is.finite(x)] - centre), decreasing = TRUE)
  n <- length(deviations)
  if (!n || !is.finite(centre)) return(NA_real_)
  tail_count <- ceiling(alpha * (n + 1L) - 1L)
  tail_count <- min(n, max(1L, tail_count))
  deviations[[tail_count]]
}

# Two-sided placebo p-value and directional percentile rank (plan 9 and 11).
placebo_p_value <- function(observed, placebos) {
  placebos <- placebos[is.finite(placebos)]
  (1 + sum(abs(placebos) >= abs(observed))) / (length(placebos) + 1)
}
percentile_rank <- function(observed, placebos) {
  placebos <- placebos[is.finite(placebos)]
  (1 + sum(placebos <= observed)) / (length(placebos) + 1)
}

###############################################################################
# Read inputs
###############################################################################

message("Matching specification: ", matching_spec_id)
message("Reading fixed-LGD decade panels...")
panels <- lapply(panel_files, function(f) {
  x <- fread(f, na.strings = c("", "NA"))
  x[, `:=`(
    analysis_lgd_id = as.character(analysis_lgd_id),
    decade = as.integer(decade),
    n_inventors = as.integer(n_inventors),
    population = as.numeric(population),
    n_years_in_bin = as.integer(n_years_in_bin)
  )]
  x[, outcome_value := 1e5 * n_inventors / (population * n_years_in_bin)]
  if (any(!is.finite(x$outcome_value)) || any(x$n_years_in_bin <= 0L))
    stop("Invalid annualized Wikipedia outcome in a decade panel.")
  x[]
})
names(panels) <- names(panel_files)

design <- CJ(cohort_lead_years = cohort_leads,
             decade_definition = c("standard", "alternative"),
             sorted = FALSE)
design[, model_id := paste0("lead", cohort_lead_years, "_", decade_definition)]
setorder(design, cohort_lead_years, decade_definition)

donor_pairs <- rbindlist(lapply(cohort_leads, function(lead) {
  x <- fread(file.path(lead_dirs[[as.character(lead)]], "donor_pool_pairs.csv"),
             na.strings = c("", "NA"))
  x[, `:=`(
    cohort_lead_years = lead,
    treated_lgd_id = as.character(treated_lgd_id),
    donor_lgd_id = as.character(donor_lgd_id)
  )]
  x[]
}), fill = TRUE, use.names = TRUE)
if (!all(donor_pairs$matching_specification == expected_matching_specification))
  stop("Donor pools do not carry the requested matching specification.")

timing <- rbindlist(lapply(cohort_leads, function(lead) {
  x <- fread(file.path(lead_dirs[[as.character(lead)]],
                       "cohort_timing_status.csv"), na.strings = c("", "NA"))
  x[, `:=`(cohort_lead_years = lead,
           treated_lgd_id = as.character(treated_lgd_id))]
  x[]
}), fill = TRUE, use.names = TRUE)

stage2_att <- if (reconcile_with_stage2) rbindlist(lapply(cohort_leads,
  function(lead) {
    x <- fread(file.path(lead_dirs[[as.character(lead)]],
                         "models_wikipedia_rate", "synthdid_att.csv"),
               na.strings = c("", "NA"))
    x[, `:=`(cohort_lead_years = lead,
             treated_lgd_id = as.character(treated_lgd_id))]
    x[]
  }), fill = TRUE, use.names = TRUE) else NULL

event_manifest <- timing[model_eligible == TRUE, .(
  cohort_lead_years, decade_definition, treated_lgd_id, treated_lgd_name,
  treated_lgd_type, treated_first_fair_year, effective_cohort_year,
  treatment_decade, available_pre_periods
)]
# The balanced window trims the treated-decade range; re-apply the gate here so
# the pooled stage never depends on the builder having used the same window.
event_manifest <- event_manifest[
  wf_event_window_admits(treatment_decade, event_window)
]
event_manifest <- merge(
  event_manifest, design, by = c("cohort_lead_years", "decade_definition"),
  all.x = TRUE, sort = FALSE
)
if (!nrow(event_manifest)) stop("No timing-eligible events were found.")
setorder(event_manifest, model_id, treatment_decade, treated_lgd_id)
message("Timing-eligible events: ", nrow(event_manifest))
print(event_manifest[, .N, by = model_id])
message("Reconciling against the per-event stage: ", reconcile_with_stage2)

###############################################################################
# Stage 1-6: per-event synthetic controls and in-space placebos
###############################################################################

event_rows <- list()
event_path_rows <- list()
placebo_rows <- list()
placebo_path_rows <- list()
weight_rows <- list()
model_objects <- list()

feasibility_rows <- list()

for (row_index in seq_len(nrow(event_manifest))) {
  event <- event_manifest[row_index]
  panel <- panels[[event$decade_definition]]
  event_key <- paste(event$model_id, event$treated_lgd_id, sep = "|")

  donors <- donor_pairs[
    cohort_lead_years == event$cohort_lead_years &
      decade_definition == event$decade_definition &
      treated_lgd_id == event$treated_lgd_id
  ][order(donor_rank)]
  if (nrow(donors) != expected_donors)
    stop("Expected ", expected_donors, " donors for ", event_key, ".")

  setup <- build_setup(
    panel, donors$donor_lgd_id, event$treated_lgd_id, event$treatment_decade
  )
  Y <- setup$Y
  time_periods <- as.integer(colnames(Y))
  N0 <- setup$N0
  T0 <- setup$T0
  treatment_start <- wf_event_window_treatment_start(
    event$treatment_decade, event_window
  )
  if (N0 != expected_donors || T0 != sum(time_periods < treatment_start))
    stop("Unexpected matrix layout for ", event_key, ".")
  if (event_window$balanced &&
      (T0 != event_window$n_pre_periods ||
       length(time_periods) - T0 != event_window$n_post_periods))
    stop("The balanced window must give ", event_window$n_pre_periods,
         " pre and ", event_window$n_post_periods, " post periods for ",
         event_key, ".")

  # Same feasibility gate the per-event stage applies, reproduced here so this
  # stage does not depend on that stage having been run.
  pre_all <- Y[, seq_len(T0), drop = FALSE]
  pre_controls <- Y[seq_len(N0), seq_len(T0), drop = FALSE]
  raw_noise <- if (T0 > 1L) sd(apply(pre_controls, 1L, diff)) else
    sd(as.vector(pre_controls))
  pre_distinct <- uniqueN(as.vector(pre_all))
  feasible <- is.finite(raw_noise) && raw_noise > tolerance && pre_distinct > 1L
  feasibility_rows[[event_key]] <- data.table(
    model_id = event$model_id,
    treated_lgd_id = event$treated_lgd_id,
    treated_lgd_name = event$treated_lgd_name,
    treatment_decade = event$treatment_decade,
    treatment_start_decade = treatment_start,
    n_pre_periods = T0,
    n_post_periods = length(time_periods) - T0,
    raw_preperiod_noise_level = raw_noise,
    preperiod_distinct_values = pre_distinct,
    feasible = feasible,
    status = if (feasible) "estimated" else
      "infeasible_zero_preperiod_variation"
  )
  if (!feasible) {
    message("  SKIP ", event_key,
            ": no informative pre-treatment outcome variation.")
    next
  }

  observed <- fit_gap_path(Y, N0, T0)
  if (!observed$ok)
    stop("Could not refit the treated event ", event_key, ": ", observed$error)

  # When the per-event stage was run on this same window, require agreement so
  # the pooled stage cannot silently drift from it. A balanced-window run has no
  # full-window counterpart to compare against, so the check is skipped and the
  # mode is recorded in the QC table.
  reconciliation_error <- NA_real_
  if (reconcile_with_stage2) {
    reference <- stage2_att[
      cohort_lead_years == event$cohort_lead_years &
        decade_definition == event$decade_definition &
        treated_lgd_id == event$treated_lgd_id
    ]
    if (nrow(reference) != 1L)
      stop("Missing per-event ATT reference for ", event_key, ".")
    att_error <- abs(observed$att_synthdid - reference$estimate)
    tau_error <- abs(
      observed$mean_post_gap - reference$post_mean_treated_minus_synthetic
    )
    rmspe_error <- abs(observed$rmspe_pre - reference$pre_rmspe)
    reconciliation_error <- max(att_error, tau_error, rmspe_error)
    if (reconciliation_error > 1e-6) {
      stop("Refit does not reproduce the per-event stage for ", event_key,
           ": ATT error ", signif(att_error, 3),
           ", tau error ", signif(tau_error, 3),
           ", pre-RMSPE error ", signif(rmspe_error, 3), ".")
    }
  }

  event_time <- time_periods - event$treatment_decade
  reference_event_time <- event_time[[T0]]
  event_rows[[event_key]] <- data.table(
    model_id = event$model_id,
    cohort_lead_years = event$cohort_lead_years,
    decade_definition = event$decade_definition,
    matching_specification = expected_matching_specification,
    event_window = event_window_id,
    outcome = outcome_name,
    treated_lgd_id = event$treated_lgd_id,
    treated_lgd_name = event$treated_lgd_name,
    treated_lgd_type = event$treated_lgd_type,
    treated_first_fair_year = event$treated_first_fair_year,
    effective_cohort_year = event$effective_cohort_year,
    treatment_decade = event$treatment_decade,
    treatment_start_decade = treatment_start,
    reference_event_time = reference_event_time,
    n_donors = N0,
    n_pre_periods = T0,
    n_post_periods = length(time_periods) - T0,
    post_horizon_decades = length(observed$post_index),
    stage2_reconciliation_error = reconciliation_error,
    tau_event_effect = observed$tau,
    att_synthdid = observed$att_synthdid,
    mean_post_gap = observed$mean_post_gap,
    mean_pre_gap = observed$mean_pre_gap,
    lambda_weighted_pre_gap = observed$lambda_pre_gap,
    reference_pre_gap = observed$reference_pre_gap,
    rmspe_pre = observed$rmspe_pre,
    mean_abs_pre_outcome_treated = observed$mean_abs_pre,
    mean_abs_pre_outcome_panel = observed$mean_abs_pre_panel,
    pre_treatment_outcome_all_zero = observed$pre_all_zero,
    nrmspe_pre = observed$nrmspe_pre,
    nrmspe_pre_treated_scale = observed$nrmspe_pre_treated_scale,
    rmspe_post = observed$rmspe_post,
    post_pre_rmspe_ratio = observed$post_pre_ratio,
    standardized_effect = observed$standardized
  )
  event_path_rows[[event_key]] <- data.table(
    model_id = event$model_id,
    treated_lgd_id = event$treated_lgd_id,
    treated_lgd_name = event$treated_lgd_name,
    decade = time_periods,
    event_time = event_time,
    reference_event_time = reference_event_time,
    period = fifelse(time_periods < treatment_start, "pre", "post"),
    treated_outcome = observed$treated_path,
    synthetic_outcome = observed$synthetic_path,
    gap = observed$gap,
    gap_adjusted = observed$gap_adjusted,
    gap_lambda_adjusted = observed$gap_lambda_adjusted
  )
  weight_rows[[event_key]] <- data.table(
    model_id = event$model_id,
    treated_lgd_id = event$treated_lgd_id,
    treated_lgd_name = event$treated_lgd_name,
    donor_lgd_id = rownames(Y)[seq_len(N0)],
    donor_rank = donors$donor_rank,
    donor_lgd_name = donors$donor_lgd_name,
    match_distance = donors$match_distance,
    weight = observed$omega
  )
  model_objects[[event_key]] <- observed$fit

  # In-space placebos: every donor takes a turn as the treated unit, is removed
  # from its own pool, and the real treated unit is dropped (plan section 9).
  placebo_stats <- vector("list", N0)
  placebo_paths <- vector("list", N0)
  for (j in seq_len(N0)) {
    Y_placebo <- rbind(
      Y[setdiff(seq_len(N0), j), , drop = FALSE],
      Y[j, , drop = FALSE]
    )
    result <- fit_gap_path(Y_placebo, N0 - 1L, T0)
    if (!result$ok) {
      placebo_stats[[j]] <- data.table(
        placebo_lgd_id = rownames(Y)[j], successful = FALSE,
        error = result$error, tau_event_effect = NA_real_,
        att_synthdid = NA_real_, mean_post_gap = NA_real_,
        mean_pre_gap = NA_real_, reference_pre_gap = NA_real_,
        rmspe_pre = NA_real_,
        mean_abs_pre_outcome = NA_real_, nrmspe_pre = NA_real_,
        placebo_pre_all_zero = NA, rmspe_post = NA_real_,
        post_pre_rmspe_ratio = NA_real_, standardized_effect = NA_real_
      )
      next
    }
    placebo_stats[[j]] <- data.table(
      placebo_lgd_id = rownames(Y)[j], successful = TRUE, error = NA_character_,
      tau_event_effect = result$tau, att_synthdid = result$att_synthdid,
      mean_post_gap = result$mean_post_gap,
      mean_pre_gap = result$mean_pre_gap,
      reference_pre_gap = result$reference_pre_gap,
      rmspe_pre = result$rmspe_pre,
      mean_abs_pre_outcome = result$mean_abs_pre,
      nrmspe_pre = result$nrmspe_pre,
      placebo_pre_all_zero = result$pre_all_zero,
      rmspe_post = result$rmspe_post,
      post_pre_rmspe_ratio = result$post_pre_ratio,
      standardized_effect = result$standardized
    )
    placebo_paths[[j]] <- data.table(
      model_id = event$model_id,
      treated_lgd_id = event$treated_lgd_id,
      placebo_lgd_id = rownames(Y)[j],
      decade = time_periods,
      event_time = event_time,
      reference_event_time = reference_event_time,
      period = fifelse(time_periods < treatment_start, "pre", "post"),
      gap = result$gap,
      gap_adjusted = result$gap_adjusted,
      gap_lambda_adjusted = result$gap_lambda_adjusted
    )
  }
  placebo_table <- rbindlist(placebo_stats, fill = TRUE, use.names = TRUE)
  placebo_table[, `:=`(
    model_id = event$model_id,
    cohort_lead_years = event$cohort_lead_years,
    decade_definition = event$decade_definition,
    treated_lgd_id = event$treated_lgd_id,
    treated_lgd_name = event$treated_lgd_name,
    treatment_decade = event$treatment_decade,
    donor_rank = donors$donor_rank,
    treated_rmspe_pre = observed$rmspe_pre,
    mspe_pre_ratio_to_treated = (rmspe_pre^2) / (observed$rmspe_pre^2)
  )]
  placebo_rows[[event_key]] <- placebo_table
  placebo_path_rows[[event_key]] <- rbindlist(
    placebo_paths[!vapply(placebo_paths, is.null, logical(1L))],
    fill = TRUE, use.names = TRUE
  )
  message("  ", event$model_id, " / ", event$treated_lgd_name,
          ": ATT=", signif(observed$tau, 4),
          " post-gap=", signif(observed$mean_post_gap, 4),
          " pre-RMSPE=", signif(observed$rmspe_pre, 4),
          " placebos=", placebo_table[successful == TRUE, .N], "/", N0)
}

feasibility <- rbindlist(feasibility_rows, fill = TRUE, use.names = TRUE)
event_effects <- rbindlist(event_rows, fill = TRUE, use.names = TRUE)
if (!nrow(event_effects))
  stop("No treated event survived the feasibility gate.")
event_gap_paths <- rbindlist(event_path_rows, fill = TRUE, use.names = TRUE)
placebo_events <- rbindlist(placebo_rows, fill = TRUE, use.names = TRUE)
placebo_gap_paths <- rbindlist(
  placebo_path_rows, fill = TRUE, use.names = TRUE
)
donor_weights <- rbindlist(weight_rows, fill = TRUE, use.names = TRUE)

###############################################################################
# Placebo p-values, percentile ranks, and fit-restricted placebo sets
###############################################################################

event_keys <- c("model_id", "treated_lgd_id")

rank_stats <- function(cutoff) {
  usable <- placebo_events[successful == TRUE &
                             is.finite(standardized_effect)]
  if (is.finite(cutoff))
    usable <- usable[mspe_pre_ratio_to_treated < cutoff]
  stats <- merge(
    event_effects[, .(model_id, treated_lgd_id, standardized_effect)],
    usable[, .(placebo_values = list(standardized_effect),
               n_placebos = .N), by = event_keys],
    by = event_keys, all.x = TRUE, sort = FALSE
  )
  stats[, `:=`(
    placebo_fit_cutoff = cutoff,
    placebo_p_value = mapply(
      function(o, p) if (is.null(p)) NA_real_ else placebo_p_value(o, p),
      standardized_effect, placebo_values
    ),
    percentile_rank = mapply(
      function(o, p) if (is.null(p)) NA_real_ else percentile_rank(o, p),
      standardized_effect, placebo_values
    )
  )]
  stats[, placebo_values := NULL]
  stats[]
}

baseline_ranks <- rank_stats(Inf)
event_effects <- merge(
  event_effects,
  baseline_ranks[, .(model_id, treated_lgd_id, n_placebos,
                     placebo_p_value, percentile_rank)],
  by = event_keys, all.x = TRUE, sort = FALSE
)
if (any(!is.finite(event_effects$percentile_rank)))
  stop("Every event must have a placebo percentile rank.")

placebo_restrictions <- rbindlist(
  lapply(c(Inf, placebo_fit_cutoffs), rank_stats),
  fill = TRUE, use.names = TRUE
)

###############################################################################
# Per-horizon statistics: the scalar apparatus applied at each event time
###############################################################################

# Each unit's adjusted gap is standardized by its OWN pre-treatment RMSPE,
# exactly as the scalar S_e is. A placebo carries RMSPE_pre_ej rather than the
# treated unit's, so the division changes the ordering and cannot be skipped.
observed_horizon <- merge(
  event_gap_paths[, .(model_id, treated_lgd_id, treated_lgd_name, event_time,
                      reference_event_time, period, gap, gap_adjusted)],
  event_effects[, .(model_id, treated_lgd_id, rmspe_pre)],
  by = event_keys, sort = FALSE
)
observed_horizon[, standardized_effect := fifelse(
  is.finite(rmspe_pre) & rmspe_pre > 0, gap_adjusted / rmspe_pre, NA_real_
)]
observed_horizon[event_time == reference_event_time,
                 standardized_effect := NA_real_]

placebo_horizon <- merge(
  placebo_gap_paths[, .(model_id, treated_lgd_id, placebo_lgd_id, event_time,
                        reference_event_time, gap_adjusted)],
  placebo_events[successful == TRUE,
                 .(model_id, treated_lgd_id, placebo_lgd_id, rmspe_pre)],
  by = c("model_id", "treated_lgd_id", "placebo_lgd_id"), sort = FALSE
)
placebo_horizon[, standardized_effect := fifelse(
  is.finite(rmspe_pre) & rmspe_pre > 0, gap_adjusted / rmspe_pre, NA_real_
)]
placebo_horizon[event_time == reference_event_time,
                standardized_effect := NA_real_]

# Percentile rank of the treated unit within its own placebos, horizon by
# horizon, using the same (1 + #{<=}) / (J + 1) convention as the scalar.
horizon_rank_rows <- merge(
  observed_horizon[, .(model_id, treated_lgd_id, event_time,
                       observed_s = standardized_effect)],
  placebo_horizon[is.finite(standardized_effect),
                  .(placebo_s = list(standardized_effect), n_placebos = .N),
                  by = .(model_id, treated_lgd_id, event_time)],
  by = c("model_id", "treated_lgd_id", "event_time"),
  all.x = TRUE, sort = FALSE
)
horizon_rank_rows[, percentile_rank := mapply(
  function(o, p) if (is.null(p) || !is.finite(o)) NA_real_ else
    percentile_rank(o, p),
  observed_s, placebo_s
)]
horizon_rank_rows[, placebo_s := NULL]

event_horizon_effects <- merge(
  observed_horizon,
  horizon_rank_rows[, .(model_id, treated_lgd_id, event_time,
                        percentile_rank, n_placebos)],
  by = c("model_id", "treated_lgd_id", "event_time"),
  all.x = TRUE, sort = FALSE
)
setorder(event_horizon_effects, model_id, treated_lgd_id, event_time)
if (event_horizon_effects[
      event_time != reference_event_time & !is.finite(percentile_rank), .N])
  stop("Every non-reference event-horizon cell must have a placebo rank.")

###############################################################################
# Pooled magnitudes, pooled evidence, and progressive trimming
###############################################################################

# Equal weighting across treated events (plan 12.1): every treated LGD counts
# once regardless of its population, so large hosts cannot dominate.
pool_events <- function(effects) {
  effects[, .(
    n_events = .N,
    pooled_att = mean(tau_event_effect),
    pooled_att_sd_across_events = if (.N > 1L)
      sd(tau_event_effect) else NA_real_,
    pooled_att_synthdid = mean(att_synthdid),
    mean_percentile_rank = mean(percentile_rank),
    mean_standardized_effect = mean(standardized_effect),
    median_standardized_effect = median(standardized_effect),
    median_percentile_rank = median(percentile_rank),
    mean_rmspe_pre = mean(rmspe_pre),
    mean_nrmspe_pre = mean(nrmspe_pre),
    events_with_zero_pre_path = sum(pre_treatment_outcome_all_zero)
  ), by = model_id]
}

pooled_base <- pool_events(event_effects)
informative_events <- event_effects[pre_treatment_outcome_all_zero == FALSE]
pooled_informative <- pool_events(informative_events)
setnames(
  pooled_informative,
  setdiff(names(pooled_informative), "model_id"),
  paste0(setdiff(names(pooled_informative), "model_id"), "_informative_pre")
)

pooled_dynamic <- event_horizon_effects[, .(
  n_events = .N,
  reference_event_time = unique(reference_event_time),
  pooled_effect = mean(gap_adjusted),
  effect_sd_across_events = if (.N > 1L) sd(gap_adjusted) else NA_real_,
  pooled_raw_gap = mean(gap),
  # The evidence half of plan section 12: bounded and scale-free, so a single
  # outlying event cannot move it the way it moves the ATT.
  mean_percentile_rank = if (all(!is.finite(percentile_rank))) NA_real_ else
    mean(percentile_rank, na.rm = TRUE)
), by = .(model_id, event_time)]
pooled_dynamic[, `:=`(
  event_time_decades = event_time / 10L,
  # Treatment starts at the window offset, which is +10 decades under the
  # balanced window because the fair decade is the last pre-treatment period.
  period = fifelse(event_time < event_window$treatment_offset, "pre", "post")
)]
setorder(pooled_dynamic, model_id, event_time)

# Progressive trimming on the normalized PRE-treatment fit only (plan 5B).
trimming <- rbindlist(lapply(unique(event_effects$model_id), function(mid) {
  sub <- event_effects[model_id == mid][order(nrmspe_pre)]
  rbindlist(lapply(seq_len(max(1L, nrow(sub) - 1L)) - 1L, function(drop) {
    kept <- sub[seq_len(nrow(sub) - drop)]
    data.table(
      model_id = mid,
      worst_fitting_events_dropped = drop,
      dropped_event_names = if (drop > 0L) paste(
        sub[(nrow(sub) - drop + 1L):nrow(sub), treated_lgd_name],
        collapse = "; "
      ) else "",
      n_events = nrow(kept),
      max_nrmspe_pre_retained = max(kept$nrmspe_pre),
      pooled_att = mean(kept$tau_event_effect),
      mean_percentile_rank = mean(kept$percentile_rank),
      mean_standardized_effect = mean(kept$standardized_effect)
    )
  }), fill = TRUE, use.names = TRUE)
}), fill = TRUE, use.names = TRUE)

# The event-time path at every trim stage. Crossing the trimming sweep with the
# dynamic effects shows whether the shape of the response survives dropping the
# worst-fitting events, not merely whether the single pooled average does. The
# band is the dispersion of the underlying event gaps, not an inferential
# interval; inference on the trimming sweep lives in `trimming`.
trim_membership <- rbindlist(lapply(unique(event_effects$model_id),
  function(mid) {
    sub <- event_effects[model_id == mid][order(nrmspe_pre)]
    rbindlist(lapply(seq_len(max(1L, nrow(sub) - 1L)) - 1L, function(drop) {
      kept <- sub[seq_len(nrow(sub) - drop)]
      data.table(
        model_id = mid,
        worst_fitting_events_dropped = drop,
        treated_lgd_id = kept$treated_lgd_id
      )
    }), fill = TRUE, use.names = TRUE)
  }), fill = TRUE, use.names = TRUE)

pooled_dynamic_by_trim <- merge(
  trim_membership,
  event_horizon_effects[, .(model_id, treated_lgd_id, event_time,
                            gap_adjusted, percentile_rank)],
  by = c("model_id", "treated_lgd_id"), allow.cartesian = TRUE, sort = FALSE
)[, .(
  n_events = .N,
  reference_event_time = unique(reference_event_time),
  pooled_effect = mean(gap_adjusted),
  effect_sd_across_events = if (.N > 1L) sd(gap_adjusted) else NA_real_,
  mean_percentile_rank = if (all(!is.finite(percentile_rank))) NA_real_ else
    mean(percentile_rank, na.rm = TRUE)
), by = .(model_id, worst_fitting_events_dropped, event_time)]
pooled_dynamic_by_trim[, `:=`(
  event_time_decades = event_time / 10L,
  effect_lower = pooled_effect - fifelse(
    is.na(effect_sd_across_events), 0, effect_sd_across_events
  ),
  effect_upper = pooled_effect + fifelse(
    is.na(effect_sd_across_events), 0, effect_sd_across_events
  )
)]
pooled_dynamic_by_trim <- merge(
  pooled_dynamic_by_trim,
  trimming[, .(model_id, worst_fitting_events_dropped, dropped_event_names,
               max_nrmspe_pre_retained)],
  by = c("model_id", "worst_fitting_events_dropped"), all.x = TRUE, sort = FALSE
)
setorder(pooled_dynamic_by_trim, model_id, worst_fitting_events_dropped,
         event_time)
# Under the balanced window every stage must cover the same event times, so a
# ragged panel would mean the merge lost rows. Under the full window the grid is
# legitimately ragged, because events differ in how many pre and post bins they
# have and dropping one changes which event times are covered at all; that
# raggedness is recorded rather than treated as an error, and is precisely what
# the balanced window removes.
trim_time_counts <- pooled_dynamic_by_trim[, uniqueN(event_time),
                                           by = .(model_id,
                                                  worst_fitting_events_dropped)]
trim_grid_is_common <- uniqueN(trim_time_counts$V1) == 1L
if (event_window$balanced && !trim_grid_is_common)
  stop("Trim-stage event studies do not share a common event-time grid.")

# Leave-one-event-out (plan 18D).
loeo <- rbindlist(lapply(seq_len(nrow(event_effects)), function(i) {
  row <- event_effects[i]
  rest <- event_effects[model_id == row$model_id &
                          treated_lgd_id != row$treated_lgd_id]
  if (!nrow(rest)) return(NULL)
  data.table(
    model_id = row$model_id,
    excluded_lgd_id = row$treated_lgd_id,
    excluded_lgd_name = row$treated_lgd_name,
    n_events = nrow(rest),
    pooled_att = mean(rest$tau_event_effect),
    mean_percentile_rank = mean(rest$percentile_rank),
    mean_standardized_effect = mean(rest$standardized_effect)
  )
}), fill = TRUE, use.names = TRUE)

###############################################################################
# Donor-sharing diagnostics (plan 14)
###############################################################################

donor_usage <- donor_weights[, .(
  synthetic_controls_using_donor = sum(weight > tolerance),
  cumulative_weight = sum(weight),
  maximum_weight = max(weight),
  events_in_pool = .N
), by = .(model_id, donor_lgd_id, donor_lgd_name)]
setorder(donor_usage, model_id, -cumulative_weight)

donor_overlap <- rbindlist(lapply(unique(donor_weights$model_id), function(mid) {
  sub <- donor_weights[model_id == mid]
  ids <- sort(unique(sub$treated_lgd_id))
  if (length(ids) < 2L) return(NULL)
  wide <- dcast(sub, donor_lgd_id ~ treated_lgd_id, value.var = "weight",
                fill = 0)
  matrix_w <- as.matrix(wide[, -1L, with = FALSE])
  pairs <- CJ(a = ids, b = ids)[a < b]
  names_lookup <- unique(sub[, .(treated_lgd_id, treated_lgd_name)])
  pairs[, weighted_overlap := mapply(function(x, y)
    sum(pmin(matrix_w[, x], matrix_w[, y])), a, b)]
  pairs[, shared_positive_donors := mapply(function(x, y)
    sum(matrix_w[, x] > tolerance & matrix_w[, y] > tolerance), a, b)]
  pairs <- merge(pairs, names_lookup, by.x = "a", by.y = "treated_lgd_id",
                 sort = FALSE)
  setnames(pairs, "treated_lgd_name", "treated_lgd_name_a")
  pairs <- merge(pairs, names_lookup, by.x = "b", by.y = "treated_lgd_id",
                 sort = FALSE)
  setnames(pairs, "treated_lgd_name", "treated_lgd_name_b")
  pairs[, model_id := mid]
  setnames(pairs, c("a", "b"), c("treated_lgd_id_a", "treated_lgd_id_b"))
  pairs[]
}), fill = TRUE, use.names = TRUE)

###############################################################################
# Joint randomization inference (plan 15, 16, 17)
###############################################################################

# For every draw, unit identities are permuted jointly rather than one event at
# a time: a single permutation of the pooled donor universe is walked and each
# event takes the first still-unused unit that is eligible for it. This keeps
# treatment timing, donor eligibility, donor-pool overlap and the repeated use
# of the same physical LGD across events intact, and prevents one LGD from
# being placebo-treated for two events in the same draw.
#
# A drawn unit's statistics come from the in-space placebo already fitted for
# that event: that placebo was estimated with the unit treated, the real
# treated LGD removed and the other 29 donors as controls, which is exactly
# the configuration the draw requires. Ranks are recomputed within the
# remaining placebos of the same event.

randomization_rows <- list()
pooled_randomization <- list()
trim_inference <- list()
horizon_inference <- list()
joint_horizon_p <- list()

for (mid in unique(event_effects$model_id)) {
  model_events <- event_effects[model_id == mid]
  model_placebos <- placebo_events[model_id == mid & successful == TRUE &
                                     is.finite(standardized_effect)]
  event_ids <- model_events$treated_lgd_id
  pools <- split(model_placebos$placebo_lgd_id, model_placebos$treated_lgd_id)
  stat_lookup <- split(model_placebos, model_placebos$treated_lgd_id)
  universe <- sort(unique(model_placebos$placebo_lgd_id))

  # Per-event placebo lookups on the horizon grid: rows are placebo units,
  # columns are event times. A draw pulls a chosen placebo's whole path by name
  # in one indexing step. The rank matrix holds each placebo's rank among the
  # OTHER placebos of the same event at that horizon, which is the same
  # convention as the scalar: rank(ties = "max") / J equals
  # (1 + #{k != j : S_k <= S_j}) / ((J - 1) + 1).
  horizon_grid <- sort(unique(event_horizon_effects[model_id == mid,
                                                    event_time]))
  horizon_labels <- as.character(horizon_grid)
  wide_by_event <- function(value_column) {
    lapply(setNames(event_ids, event_ids), function(eid) {
      sub <- placebo_horizon[model_id == mid & treated_lgd_id == eid]
      w <- dcast(sub, placebo_lgd_id ~ event_time, value.var = value_column)
      m <- as.matrix(w[, -1L, with = FALSE])
      rownames(m) <- w$placebo_lgd_id
      missing_cols <- setdiff(horizon_labels, colnames(m))
      if (length(missing_cols)) {
        pad <- matrix(NA_real_, nrow(m), length(missing_cols),
                      dimnames = list(rownames(m), missing_cols))
        m <- cbind(m, pad)
      }
      m[, horizon_labels, drop = FALSE]
    })
  }
  placebo_gap_by_event <- wide_by_event("gap_adjusted")
  placebo_s_by_event <- wide_by_event("standardized_effect")
  # ties.method = "max" is what the plan's 1(S^P <= S) indicator implies:
  # rank_max(j) = #{k : S_k <= S_j}, so a placebo's rank is rank_max / J, the
  # same quantity the scalar computes as (1 + #{k != j : S_k <= S_j}) / J.
  # Ties therefore inflate ranks, and ties are common at early horizons where
  # many donors record no births at all. That pushes the rank null above the
  # exchangeable value of 16/31, which is why every p-value is centred on the
  # empirical null rather than on a theoretical constant.
  placebo_rank_by_event <- lapply(placebo_s_by_event, function(m) {
    out <- apply(m, 2L, function(v) {
      usable <- sum(is.finite(v))
      if (!usable) return(rep(NA_real_, length(v)))
      rank(v, ties.method = "max", na.last = "keep") / usable
    })
    # A horizon an event does not cover is an all-NA column, and the branch
    # above returns an unnamed vector for it, which makes apply() drop the
    # placebo rownames. That only arises on the ragged full-window grid, so the
    # dimnames are restored explicitly rather than relied upon.
    if (is.null(dim(out)))
      out <- matrix(out, nrow = nrow(m), ncol = ncol(m))
    dimnames(out) <- dimnames(m)
    out
  })

  # Draw-by-stage-by-horizon accumulators. Stage index s corresponds to
  # dropping s - 1 of the worst-fitting events.
  n_stage_max <- length(event_ids)
  draw_horizon_att <- array(
    NA_real_, dim = c(randomization_draws, n_stage_max, length(horizon_grid))
  )
  draw_horizon_rank <- array(
    NA_real_, dim = c(randomization_draws, n_stage_max, length(horizon_grid))
  )

  observed_stat <- model_events[, mean(percentile_rank)]
  observed_att <- model_events[, mean(tau_event_effect)]
  observed_sbar <- model_events[, mean(standardized_effect)]
  reference_time <- unique(model_events$reference_event_time)
  if (length(reference_time) != 1L || !is.finite(reference_time))
    stop("Each model must have one finite final-pre reference event time.")

  set.seed(base_seed + which(unique(event_effects$model_id) == mid) * 7919L)
  draws <- vector("list", randomization_draws)
  for (b in seq_len(randomization_draws)) {
    permutation <- sample(universe)
    position <- setNames(seq_along(permutation), permutation)
    used <- character(0)
    order_events <- sample(event_ids)
    assigned <- character(0)
    assigned_events <- character(0)
    for (eid in order_events) {
      eligible_units <- setdiff(pools[[eid]], used)
      if (!length(eligible_units)) next
      pick <- eligible_units[which.min(position[eligible_units])]
      used <- c(used, pick)
      assigned <- c(assigned, pick)
      assigned_events <- c(assigned_events, eid)
    }
    if (!length(assigned_events)) next
    ranks <- numeric(length(assigned_events))
    taus <- numeric(length(assigned_events))
    svals <- numeric(length(assigned_events))
    nfit <- numeric(length(assigned_events))
    informative <- logical(length(assigned_events))
    for (k in seq_along(assigned_events)) {
      stats <- stat_lookup[[assigned_events[k]]]
      self <- stats$placebo_lgd_id == assigned[k]
      own <- stats$standardized_effect[self]
      others <- stats$standardized_effect[!self]
      ranks[k] <- (1 + sum(others <= own)) / (length(others) + 1)
      taus[k] <- stats$tau_event_effect[self]
      svals[k] <- own
      nfit[k] <- stats$nrmspe_pre[self]
      informative[k] <- isFALSE(stats$placebo_pre_all_zero[self])
    }
    order_fit <- order(nfit)

    # The same draw, now as an events-by-horizon path. Rows are reordered
    # best-fitting first so the cumulative column means reproduce the observed
    # trimming sweep stage for stage.
    att_paths <- t(vapply(seq_along(assigned_events), function(k)
      placebo_gap_by_event[[assigned_events[k]]][assigned[k], ],
      numeric(length(horizon_grid))))
    rank_paths <- t(vapply(seq_along(assigned_events), function(k)
      placebo_rank_by_event[[assigned_events[k]]][assigned[k], ],
      numeric(length(horizon_grid))))
    att_stages <- cumulative_trim_colmeans(
      att_paths[order_fit, , drop = FALSE]
    )
    rank_stages <- cumulative_trim_colmeans(
      rank_paths[order_fit, , drop = FALSE]
    )
    for (s in seq_along(att_stages)) {
      draw_horizon_att[b, s, ] <- att_stages[[s]]
      draw_horizon_rank[b, s, ] <- rank_stages[[s]]
    }
    draws[[b]] <- data.table(
      model_id = mid,
      draw = b,
      n_assigned_events = length(assigned_events),
      mean_percentile_rank = mean(ranks),
      pooled_att = mean(taus),
      mean_standardized_effect = mean(svals),
      # The zero-pre-path screen is reproduced inside the draw (plan 17).
      n_informative_events = sum(informative),
      mean_percentile_rank_informative = if (any(informative))
        mean(ranks[informative]) else NA_real_,
      pooled_att_informative = if (any(informative))
        mean(taus[informative]) else NA_real_,
      # The pre-fit trimming rule is reproduced inside the draw (plan 17), at
      # every step of the path rather than only the first two.
      trim_ranks = list(cumulative_trim_means(ranks[order_fit])),
      trim_atts = list(cumulative_trim_means(taus[order_fit]))
    )
  }
  # Pointwise test-inversion CIs for effects plus null bands and p-values for
  # ranks. The omitted final-pre period has a fixed zero effect and no interval
  # or rank statistic.
  observed_stage_paths <- pooled_dynamic_by_trim[model_id == mid]
  horizon_inference[[mid]] <- rbindlist(lapply(
    sort(unique(observed_stage_paths$worst_fitting_events_dropped)),
    function(drop_k) {
      s <- drop_k + 1L
      if (s > n_stage_max) return(NULL)
      rbindlist(lapply(seq_along(horizon_grid), function(h) {
        va <- draw_horizon_att[, s, h]
        vr <- draw_horizon_rank[, s, h]
        ua <- is.finite(va)
        ur <- is.finite(vr)
        ca <- if (any(ua)) mean(va[ua]) else NA_real_
        cr <- if (any(ur)) mean(vr[ur]) else NA_real_
        is_reference <- horizon_grid[h] == reference_time
        critical_95 <- if (!is_reference && any(ua))
          randomization_critical_value(va[ua], ca, alpha = 0.05) else NA_real_
        obs <- observed_stage_paths[
          worst_fitting_events_dropped == drop_k &
            event_time == horizon_grid[h]
        ]
        data.table(
          model_id = mid,
          worst_fitting_events_dropped = drop_k,
          event_time = horizon_grid[h],
          reference_event_time = reference_time,
          omitted_reference_period = is_reference,
          randomization_draws_used = sum(ua),
          null_mean = ca,
          null_lower = if (any(ua))
            as.numeric(quantile(va[ua], 0.025, names = FALSE)) else NA_real_,
          null_upper = if (any(ua))
            as.numeric(quantile(va[ua], 0.975, names = FALSE)) else NA_real_,
          randomization_critical_95 = critical_95,
          effect_ci_lower = if (!is_reference && nrow(obs) == 1L)
            obs$pooled_effect - ca - critical_95 else NA_real_,
          effect_ci_upper = if (!is_reference && nrow(obs) == 1L)
            obs$pooled_effect - ca + critical_95 else NA_real_,
          confidence_level = if (!is_reference) 0.95 else NA_real_,
          confidence_method = if (!is_reference)
            "finite_sample_joint_randomization_test_inversion" else
            "omitted_reference_period",
          randomization_p = if (!is_reference && any(ua) && nrow(obs) == 1L) (1 + sum(
            abs(va[ua] - ca) >= abs(obs$pooled_effect - ca)
          )) / (sum(ua) + 1) else NA_real_,
          rank_null_mean = cr,
          rank_null_lower = if (any(ur))
            as.numeric(quantile(vr[ur], 0.025, names = FALSE)) else NA_real_,
          rank_null_upper = if (any(ur))
            as.numeric(quantile(vr[ur], 0.975, names = FALSE)) else NA_real_,
          rank_randomization_p = if (!is_reference && any(ur) && nrow(obs) == 1L) (1 + sum(
            abs(vr[ur] - cr) >= abs(obs$mean_percentile_rank - cr)
          )) / (sum(ur) + 1) else NA_real_
        )
      }), fill = TRUE, use.names = TRUE)
    }), fill = TRUE, use.names = TRUE)

  # Multiplicity guard: seven horizons are not seven independent tests, so a
  # single joint statistic over the post-treatment horizons is reported too.
  post_index <- which(horizon_grid >= event_window$treatment_offset)
  joint_p <- function(arr, observed_by_horizon) {
    centres <- vapply(post_index, function(h) {
      v <- arr[, 1L, h]; if (any(is.finite(v))) mean(v[is.finite(v)]) else
        NA_real_
    }, numeric(1L))
    draw_max <- apply(arr[, 1L, post_index, drop = FALSE], 1L, function(row)
      max(abs(row - centres), na.rm = TRUE))
    obs_max <- max(abs(observed_by_horizon - centres), na.rm = TRUE)
    usable <- is.finite(draw_max)
    if (!any(usable) || !is.finite(obs_max)) return(NA_real_)
    (1 + sum(draw_max[usable] >= obs_max)) / (sum(usable) + 1)
  }
  observed_post <- pooled_dynamic[model_id == mid][
    order(event_time)
  ][event_time %in% horizon_grid[post_index]]
  joint_horizon_p[[mid]] <- data.table(
    model_id = mid,
    randomization_p_any_post_horizon =
      joint_p(draw_horizon_att, observed_post$pooled_effect),
    randomization_p_any_post_horizon_rank =
      joint_p(draw_horizon_rank, observed_post$mean_percentile_rank)
  )

  draw_table <- rbindlist(draws[!vapply(draws, is.null, logical(1L))],
                          fill = TRUE, use.names = TRUE)
  if (!nrow(draw_table))
    stop("Joint randomization produced no usable draws for ", mid, ".")
  randomization_rows[[mid]] <- draw_table

  # Centre the two-sided test on the empirical null rather than on 0.5.
  null_centre <- mean(draw_table$mean_percentile_rank, na.rm = TRUE)
  null_centre_att <- mean(draw_table$pooled_att, na.rm = TRUE)
  null_centre_informative <- mean(
    draw_table$mean_percentile_rank_informative, na.rm = TRUE
  )
  centred <- function(x, centre = null_centre) abs(x - centre)

  # Joint p-value at every trim step. Draw b contributes its own trimmed mean,
  # so the screen is reproduced inside the randomization exactly as it is in
  # the observed data.
  observed_trim <- trimming[model_id == mid][
    order(worst_fitting_events_dropped)
  ]
  trim_inference[[mid]] <- rbindlist(lapply(
    seq_len(nrow(observed_trim)), function(step) {
      drop_k <- observed_trim$worst_fitting_events_dropped[step]
      draw_rank <- vapply(draw_table$trim_ranks, function(v)
        if (length(v) > drop_k) v[[drop_k + 1L]] else NA_real_, numeric(1L))
      draw_att <- vapply(draw_table$trim_atts, function(v)
        if (length(v) > drop_k) v[[drop_k + 1L]] else NA_real_, numeric(1L))
      usable <- is.finite(draw_rank)
      usable_att <- is.finite(draw_att)
      centre_k <- if (any(usable)) mean(draw_rank[usable]) else NA_real_
      centre_att_k <- if (any(usable_att)) mean(draw_att[usable_att]) else
        NA_real_
      data.table(
        model_id = mid,
        worst_fitting_events_dropped = drop_k,
        randomization_draws_used = sum(usable),
        randomization_null_mean = centre_k,
        randomization_null_mean_att = centre_att_k,
        randomization_p_rank = if (any(usable)) (1 + sum(
          abs(draw_rank[usable] - centre_k) >=
            abs(observed_trim$mean_percentile_rank[step] - centre_k)
        )) / (sum(usable) + 1) else NA_real_,
        randomization_p_att = if (any(usable_att)) (1 + sum(
          abs(draw_att[usable_att] - centre_att_k) >=
            abs(observed_trim$pooled_att[step] - centre_att_k)
        )) / (sum(usable_att) + 1) else NA_real_
      )
    }), fill = TRUE, use.names = TRUE)
  informative_observed <- pooled_informative[
    model_id == mid, mean_percentile_rank_informative_pre
  ]
  pooled_randomization[[mid]] <- data.table(
    model_id = mid,
    pooled_statistic = pooled_statistic,
    observed_mean_percentile_rank = observed_stat,
    randomization_draws = nrow(draw_table),
    randomization_mean = mean(draw_table$mean_percentile_rank),
    randomization_sd = sd(draw_table$mean_percentile_rank),
    randomization_centre_used = null_centre,
    randomization_mean_att = mean(draw_table$pooled_att),
    randomization_p_two_sided = (1 + sum(
      centred(draw_table$mean_percentile_rank) >= centred(observed_stat)
    )) / (nrow(draw_table) + 1),
    randomization_p_one_sided_positive = (1 + sum(
      draw_table$mean_percentile_rank >= observed_stat
    )) / (nrow(draw_table) + 1),
    randomization_p_att_two_sided = (1 + sum(
      abs(draw_table$pooled_att - null_centre_att) >=
        abs(observed_att - null_centre_att)
    )) / (nrow(draw_table) + 1),
    randomization_p_sbar_two_sided = (1 + sum(
      abs(draw_table$mean_standardized_effect) >= abs(observed_sbar)
    )) / (nrow(draw_table) + 1),
    randomization_p_trim1 = trim_inference[[mid]][
      worst_fitting_events_dropped == 1L, randomization_p_rank
    ][1L],
    randomization_p_trim2 = trim_inference[[mid]][
      worst_fitting_events_dropped == 2L, randomization_p_rank
    ][1L],
    randomization_p_informative_pre = if (length(informative_observed) == 1L &&
        is.finite(informative_observed) &&
        any(is.finite(draw_table$mean_percentile_rank_informative))) (1 + sum(
      centred(draw_table$mean_percentile_rank_informative,
              null_centre_informative) >=
        centred(informative_observed, null_centre_informative), na.rm = TRUE
    )) / (sum(is.finite(draw_table$mean_percentile_rank_informative)) + 1)
      else NA_real_,
    mean_assigned_events = mean(draw_table$n_assigned_events),
    incomplete_assignment_draws = draw_table[
      n_assigned_events < nrow(model_events), .N
    ]
  )
}

horizon_inference_table <- rbindlist(
  horizon_inference, fill = TRUE, use.names = TRUE
)
# Every trim stage carries a band; stage 0 is the baseline event study, so the
# same rows also furnish the main figure's inference.
pooled_dynamic_by_trim <- merge(
  pooled_dynamic_by_trim, horizon_inference_table,
  by = c("model_id", "worst_fitting_events_dropped", "event_time",
         "reference_event_time"),
  all.x = TRUE, sort = FALSE
)
setorder(pooled_dynamic_by_trim, model_id, worst_fitting_events_dropped,
         event_time)
pooled_dynamic <- merge(
  pooled_dynamic,
  horizon_inference_table[worst_fitting_events_dropped == 0L][
    , worst_fitting_events_dropped := NULL
  ],
  by = c("model_id", "event_time", "reference_event_time"),
  all.x = TRUE, sort = FALSE
)
setorder(pooled_dynamic, model_id, event_time)

trim_inference_table <- rbindlist(
  trim_inference, fill = TRUE, use.names = TRUE
)
trimming <- merge(
  trimming, trim_inference_table,
  by = c("model_id", "worst_fitting_events_dropped"),
  all.x = TRUE, sort = FALSE
)
setorder(trimming, model_id, worst_fitting_events_dropped)
randomization_draws_table <- rbindlist(
  randomization_rows, fill = TRUE, use.names = TRUE
)
# The per-draw trimming paths are list columns used only to build the inference
# above; drop them so the draw file stays a flat CSV.
randomization_draws_table[, c("trim_ranks", "trim_atts") := NULL]
pooled_inference <- rbindlist(
  pooled_randomization, fill = TRUE, use.names = TRUE
)
pooled_att <- merge(pooled_base, rbindlist(
  joint_horizon_p, fill = TRUE, use.names = TRUE
), by = "model_id", all.x = TRUE, sort = FALSE)
pooled_att <- merge(pooled_att, pooled_informative, by = "model_id",
                    all.x = TRUE, sort = FALSE)
pooled_att <- merge(pooled_att, pooled_inference, by = "model_id",
                    all.x = TRUE, sort = FALSE)
pooled_att <- merge(
  design[, .(model_id, cohort_lead_years, decade_definition)], pooled_att,
  by = "model_id", all.y = TRUE, sort = FALSE
)
pooled_att[, `:=`(
  matching_specification = expected_matching_specification,
  outcome = outcome_name,
  treated_weighting = "equal_treated_lgd",
  effect_estimand = "mean_post_gap_minus_final_pre_period_gap",
  event_screen = "retain_all_estimated_events"
)]
setorder(pooled_att, cohort_lead_years, decade_definition)

###############################################################################
# Leave-shared-donor-out (plan 18E)
###############################################################################

message("Re-estimating without the most influential shared donors...")
influential <- donor_usage[, head(.SD, influential_donor_count),
                           by = model_id]
leave_donor_rows <- list()
for (i in seq_len(nrow(influential))) {
  target <- influential[i]
  affected <- event_effects[model_id == target$model_id]
  refits <- list()
  for (k in seq_len(nrow(affected))) {
    ev <- affected[k]
    src <- event_manifest[model_id == ev$model_id &
                            treated_lgd_id == ev$treated_lgd_id]
    donors <- donor_pairs[
      cohort_lead_years == src$cohort_lead_years &
        decade_definition == src$decade_definition &
        treated_lgd_id == src$treated_lgd_id
    ][order(donor_rank)]
    keep <- donors$donor_lgd_id[donors$donor_lgd_id != target$donor_lgd_id]
    if (length(keep) == nrow(donors)) {
      refits[[k]] <- data.table(
        treated_lgd_id = ev$treated_lgd_id,
        tau_event_effect = ev$tau_event_effect,
        standardized_effect = ev$standardized_effect,
        donor_removed = FALSE
      )
      next
    }
    panel <- panels[[src$decade_definition]]
    setup <- build_setup(panel, keep, src$treated_lgd_id,
                         src$treatment_decade)
    result <- fit_gap_path(setup$Y, setup$N0, setup$T0)
    refits[[k]] <- data.table(
      treated_lgd_id = ev$treated_lgd_id,
      tau_event_effect = if (result$ok) result$tau else NA_real_,
      standardized_effect = if (result$ok) result$standardized else NA_real_,
      donor_removed = TRUE
    )
  }
  refit_table <- rbindlist(refits, fill = TRUE, use.names = TRUE)
  leave_donor_rows[[i]] <- data.table(
    model_id = target$model_id,
    excluded_donor_lgd_id = target$donor_lgd_id,
    excluded_donor_lgd_name = target$donor_lgd_name,
    excluded_donor_cumulative_weight = target$cumulative_weight,
    excluded_donor_synthetic_controls =
      target$synthetic_controls_using_donor,
    events_refitted = refit_table[donor_removed == TRUE, .N],
    n_events = refit_table[is.finite(tau_event_effect), .N],
    pooled_att = refit_table[, mean(tau_event_effect, na.rm = TRUE)],
    mean_standardized_effect = refit_table[
      , mean(standardized_effect, na.rm = TRUE)
    ],
    baseline_pooled_att = pooled_base[model_id == target$model_id, pooled_att]
  )
}
leave_donor_out <- rbindlist(leave_donor_rows, fill = TRUE, use.names = TRUE)

###############################################################################
# Placebo-in-time (plan 18H)
###############################################################################

message("Running placebo-in-time tests...")
placebo_time_rows <- list()
for (i in seq_len(nrow(event_effects))) {
  ev <- event_effects[i]
  src <- event_manifest[model_id == ev$model_id &
                          treated_lgd_id == ev$treated_lgd_id]
  panel <- panels[[src$decade_definition]]
  real_start <- wf_event_window_treatment_start(
    src$treatment_decade, event_window
  )
  fake_start <- real_start - placebo_in_time_shift
  # Only genuinely pre-treatment bins are usable, and under a balanced window
  # only those inside the window. Shifting treatment back inevitably shortens
  # both sides, so the reduced geometry is recorded on every row rather than
  # presented as the balanced design.
  decades <- if (event_window$balanced)
    wf_event_window_decades(src$treatment_decade, event_window) else
    sort(unique(panel$decade))
  usable <- sort(decades[decades < real_start])
  n_pre <- sum(usable < fake_start)
  n_post <- sum(usable >= fake_start)
  window_note <- if (event_window$balanced) paste0(
    "reduced from the balanced ", event_window$n_pre_periods, "/",
    event_window$n_post_periods, " window to ", n_pre, "/", n_post
  ) else "full pre-treatment window"
  if (n_pre < 2L || n_post < 1L) {
    placebo_time_rows[[i]] <- data.table(
      model_id = ev$model_id, treated_lgd_id = ev$treated_lgd_id,
      treated_lgd_name = ev$treated_lgd_name,
      placebo_treatment_decade = fake_start,
      real_treatment_start_decade = real_start,
      shift_years = placebo_in_time_shift,
      status = "insufficient_pre_treatment_window",
      n_pre_periods = n_pre, n_post_periods = n_post,
      window_note = window_note,
      tau_event_effect = NA_real_, rmspe_pre = NA_real_,
      standardized_effect = NA_real_
    )
    next
  }
  donors <- donor_pairs[
    cohort_lead_years == src$cohort_lead_years &
      decade_definition == src$decade_definition &
      treated_lgd_id == src$treated_lgd_id
  ][order(donor_rank)]
  setup <- build_setup(panel, donors$donor_lgd_id, src$treated_lgd_id,
                       fake_start, decades = usable,
                       treatment_start = fake_start)
  if (setup$T0 != n_pre)
    stop("Unexpected placebo-in-time layout for ", ev$treated_lgd_name, ".")
  result <- fit_gap_path(setup$Y, setup$N0, setup$T0)
  placebo_time_rows[[i]] <- data.table(
    model_id = ev$model_id, treated_lgd_id = ev$treated_lgd_id,
    treated_lgd_name = ev$treated_lgd_name,
    placebo_treatment_decade = fake_start,
    real_treatment_start_decade = real_start,
    shift_years = placebo_in_time_shift,
    status = if (result$ok) "estimated" else "failed",
    n_pre_periods = n_pre, n_post_periods = n_post,
    window_note = window_note,
    tau_event_effect = if (result$ok) result$tau else NA_real_,
    rmspe_pre = if (result$ok) result$rmspe_pre else NA_real_,
    standardized_effect = if (result$ok) result$standardized else NA_real_
  )
}
placebo_in_time <- rbindlist(placebo_time_rows, fill = TRUE, use.names = TRUE)
placebo_in_time_pooled <- placebo_in_time[status == "estimated", .(
  n_events = .N,
  pooled_att = mean(tau_event_effect),
  mean_standardized_effect = mean(standardized_effect, na.rm = TRUE)
), by = model_id]

###############################################################################
# Presentation tables (plan 19)
###############################################################################

table1 <- event_effects[, .(
  model_id, cohort_lead_years, decade_definition, treated_lgd_name,
  treated_lgd_type, treated_first_fair_year, effective_cohort_year,
  treatment_decade, n_donors, n_pre_periods, n_post_periods,
  rmspe_pre = round(rmspe_pre, 5), nrmspe_pre = round(nrmspe_pre, 4),
  pre_treatment_outcome_all_zero,
  included_in_baseline = TRUE,
  included_in_informative_pre = !pre_treatment_outcome_all_zero
)]
setorder(table1, model_id, treatment_decade)

table2 <- donor_weights[weight > tolerance][order(model_id, treated_lgd_name,
                                                  -weight)]
table2 <- table2[, .SD[seq_len(min(.N, 10L))],
                 by = .(model_id, treated_lgd_name)]
table2 <- merge(
  table2,
  event_effects[, .(model_id, treated_lgd_id, rmspe_pre, nrmspe_pre)],
  by = event_keys, all.x = TRUE, sort = FALSE
)
setorder(table2, model_id, treated_lgd_name, -weight)

table3 <- event_effects[, .(
  model_id, treated_lgd_name, treatment_decade,
  rmspe_pre = round(rmspe_pre, 5),
  tau_event_effect = round(tau_event_effect, 5),
  att_synthdid = round(att_synthdid, 5),
  standardized_effect = round(standardized_effect, 4),
  post_pre_rmspe_ratio = round(post_pre_rmspe_ratio, 4),
  percentile_rank = round(percentile_rank, 4),
  placebo_p_value = round(placebo_p_value, 4),
  n_placebos
)]
setorder(table3, model_id, treatment_decade)

table4 <- rbindlist(list(
  pooled_att[, .(
    model_id, specification = "baseline_all_events", n_events,
    pooled_att, mean_percentile_rank, mean_standardized_effect,
    p_value = randomization_p_two_sided
  )],
  pooled_att[, .(
    model_id, specification = "informative_pre_treatment_path",
    n_events = n_events_informative_pre,
    pooled_att = pooled_att_informative_pre,
    mean_percentile_rank = mean_percentile_rank_informative_pre,
    mean_standardized_effect = mean_standardized_effect_informative_pre,
    p_value = randomization_p_informative_pre
  )],
  trimming[worst_fitting_events_dropped > 0L, .(
    model_id,
    specification = paste0("trim_worst_", worst_fitting_events_dropped),
    n_events, pooled_att, mean_percentile_rank, mean_standardized_effect,
    p_value = randomization_p_rank
  )],
  loeo[, .(
    model_id, specification = paste0("leave_out_", excluded_lgd_name),
    n_events, pooled_att, mean_percentile_rank, mean_standardized_effect,
    p_value = NA_real_
  )],
  leave_donor_out[, .(
    model_id,
    specification = paste0("drop_donor_", excluded_donor_lgd_name),
    n_events, pooled_att, mean_percentile_rank = NA_real_,
    mean_standardized_effect, p_value = NA_real_
  )],
  placebo_in_time_pooled[, .(
    model_id, specification = "placebo_in_time_minus_20y", n_events,
    pooled_att, mean_percentile_rank = NA_real_, mean_standardized_effect,
    p_value = NA_real_
  )]
), fill = TRUE, use.names = TRUE)
setorder(table4, model_id, specification)

# The window geometry is described from the active window rather than restated,
# so this record cannot drift from the run it belongs to. The observed period
# ranges are read from the estimated events for the same reason.
pre_range <- range(event_effects$n_pre_periods)
post_range <- range(event_effects$n_post_periods)
describe_range <- function(r) if (r[[1L]] == r[[2L]]) as.character(r[[1L]]) else
  paste0(r[[1L]], " to ", r[[2L]])
offsets_text <- function(offsets) paste(
  ifelse(offsets > 0L, paste0("+", offsets), as.character(offsets)),
  collapse = ", "
)
treatment_start_text <- if (event_window$balanced) paste0(
  "Decade bin containing first_fair_year minus the cohort lead sets the fair ",
  "decade T; leads of 0 and 20 years are both reported. Under this window the ",
  "fair decade is the LAST PRE-TREATMENT period, so the treatment indicator ",
  "turns on at T+", event_window$treatment_offset, "."
) else paste0(
  "Decade bin containing first_fair_year minus the cohort lead; leads of 0 ",
  "and 20 years are both reported. The treatment indicator turns on at that ",
  "decade."
)
pre_window_text <- if (event_window$balanced) paste0(
  event_window$n_pre_periods, " decade bins at offsets ",
  offsets_text(event_window$pre_offsets), " decades from the fair decade, the ",
  "last of which is the fair decade itself"
) else paste0(
  "All decade bins strictly before the treatment decade (",
  describe_range(pre_range), " bins across events)"
)
post_window_text <- if (event_window$balanced) paste0(
  event_window$n_post_periods, " decade bins at offsets ",
  offsets_text(event_window$post_offsets), " decades from the fair decade"
) else paste0(
  "All decade bins from the treatment decade onward (",
  describe_range(post_range), " bins across events)"
)
event_window_text <- if (event_window$balanced) paste0(
  event_window$id, ": a fixed ", event_window$n_pre_periods, "-pre by ",
  event_window$n_post_periods, "-post window, which requires the fair decade ",
  "to lie in [", event_window$min_treatment_decade, ", ",
  event_window$max_treatment_decade, "] and gives every event the same ",
  "event-time grid"
) else paste0(
  event_window$id, ": every decade bin in the panel, so the number of pre and ",
  "post periods varies across events and the pooled event-time grid is ragged"
)

decisions <- data.table(
  decision = c(
    "TREATED EVENTS", "TREATMENT DATES", "OUTCOME", "UNIT OF OBSERVATION",
    "EVENT WINDOW", "PRE-TREATMENT WINDOW", "POST-TREATMENT WINDOW",
    "DONOR ELIGIBILITY RULE", "SPILLOVER EXCLUSION RULE",
    "SYNTHETIC-CONTROL ESTIMATOR", "PREDICTOR SET",
    "MODEL/TUNING SELECTION RULE", "PRE-FIT METRIC", "PRE-FIT SELECTION RULE",
    "EVENT-SPECIFIC EFFECT ESTIMAND", "EVENT-SPECIFIC TEST STATISTIC",
    "PER-HORIZON ESTIMAND", "PER-HORIZON TEST STATISTIC",
    "MULTIPLICITY GUARD",
    "PLACEBO ELIGIBILITY RULE", "PLACEBO FIT RESTRICTION",
    "POOLED EFFECT WEIGHTING", "POOLED INFERENCE STATISTIC",
    "JOINT RANDOMIZATION PROCEDURE", "NUMBER OF RANDOMIZATION DRAWS",
    "ROBUSTNESS SPECIFICATIONS"
  ),
  value = c(
    paste0("Fixed-1911 England and Wales LGDs whose first matched world's ",
           "fair falls strictly after 1870 and whose per-event synthdid model ",
           "was estimated (", nrow(event_effects), " event-by-definition ",
           "models across ", uniqueN(event_effects$model_id),
           " specifications)"),
    treatment_start_text,
    paste0("Annualized Wikipedia Discovery/Science births per 100,000 ",
           "population-year: 1e5 * n_inventors / (population * ",
           "n_years_in_bin)"),
    "Fixed-1911 local government district, decade bins 1850-1960",
    event_window_text,
    pre_window_text,
    post_window_text,
    paste0("Never-host LGDs only, at least 50 km from the treated LGD's ",
           "first-fair venue, with valid matching covariates; the 30 nearest ",
           "on the matching distance form the pool"),
    "50 km minimum polygon-to-venue distance around the treated venue",
    "Synthetic difference-in-differences (synthdid), unit and time weights",
    expected_matching_specification,
    paste0("No tuning; synthdid default regularization, donor pools fixed ",
           "before estimation"),
    "Pre-treatment RMSPE, and NRMSPE normalized by mean absolute pre-outcome",
    paste0("Baseline retains every estimated event; progressive trimming on ",
           "NRMSPE reported as robustness rather than used as a screen"),
    paste0("Mean post-treatment treated-minus-synthetic gap less the final ",
           "pre-treatment gap. Under the balanced window that reference is ",
           "event time 0; under the full window it is the last decade before ",
           "treatment. The package synthdid ATT and lambda-weighted pre-gap ",
           "are retained as diagnostics."),
    paste0("S_e = tau_e / RMSPE_pre_e. A standardized effect, not an ",
           "elasticity: the denominator is the pre-treatment root mean squared ",
           "prediction error, a fit measure rather than a standard error, so ",
           "S_e has no t reference distribution and is read only against its ",
           "placebo distribution."),
    paste0("ATT_h: the equal-weighted mean across events of the gap at event ",
           "time h less that event's final-pre-period gap. The reference ",
           "period is zero by construction and averaging the post horizons ",
           "reproduces the pooled ATT exactly."),
    paste0("S_{e,h} = adjusted gap at h divided by that unit's OWN ",
           "RMSPE_pre, and r_h the mean across events of the per-horizon ",
           "placebo percentile rank. A placebo carries RMSPE_pre_ej rather ",
           "than the treated unit's, so the division changes the ordering."),
    paste0("Per-horizon p-values are uncorrected. The multiplicity-aware ",
           "statistic is a joint any-post-treatment-horizon test comparing the ",
           "observed max |Q_h - c| across post horizons with the same ",
           "statistic in every draw, reported on both the effect and rank ",
           "scales."),
    paste0("Every donor in the event's own 30-LGD pool takes a turn as ",
           "placebo-treated, removed from its own pool, with the real treated ",
           "LGD dropped (29 controls)"),
    paste0("Full placebo set for the baseline; MSPE_pre ratio cutoffs of ",
           paste(placebo_fit_cutoffs, collapse = ", "), " reported alongside"),
    "Equal weight per treated LGD",
    pooled_statistic,
    paste0("Joint permutation of the pooled donor universe; each event takes ",
           "the first unused eligible unit, preserving timing, eligibility, ",
           "pool overlap and repeated units; pre-fit trimming reproduced ",
           "inside each draw"),
    as.character(randomization_draws),
    paste0("Progressive pre-fit trimming, leave-one-event-out, ",
           "leave-shared-donor-out for the top ", influential_donor_count,
           " donors by cumulative weight, placebo-in-time at minus ",
           placebo_in_time_shift, " years, placebo-fit restrictions")
  )
)

###############################################################################
# Figures (plan 19)
###############################################################################

message("Writing figures...")
theme_dz <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

for (mid in unique(event_effects$model_id)) {
  mdir <- file.path(figure_dir, mid)
  dir.create(mdir, recursive = TRUE, showWarnings = FALSE)
  paths <- event_gap_paths[model_id == mid]
  effects <- event_effects[model_id == mid]

  long_paths <- melt(
    paths, id.vars = c("treated_lgd_name", "event_time"),
    measure.vars = c("treated_outcome", "synthetic_outcome"),
    variable.name = "series", value.name = "value"
  )
  long_paths[, series := factor(
    series, levels = c("treated_outcome", "synthetic_outcome"),
    labels = c("Treated", "Synthetic")
  )]
  fig1 <- ggplot(long_paths, aes(event_time, value, linetype = series)) +
    geom_vline(xintercept = event_window$treatment_offset, colour = "grey60",
               linewidth = 0.3) +
    geom_line(linewidth = 0.5) +
    facet_wrap(~ treated_lgd_name, scales = "free_y") +
    labs(x = "Event time (years)",
         y = "Discovery/Science births per 100k population-year",
         linetype = NULL,
         title = paste0("Treated and synthetic trajectories: ", mid)) +
    theme_dz + theme(legend.position = "bottom")
  ggsave(file.path(mdir, "dz_fig1_paths.png"), fig1,
         width = 9, height = 6, dpi = 200)

  fig2 <- ggplot(paths, aes(event_time, gap, group = treated_lgd_name)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_vline(xintercept = event_window$treatment_offset, colour = "grey60",
               linewidth = 0.3) +
    geom_line(alpha = 0.7, linewidth = 0.4) +
    labs(x = "Event time (years)", y = "Treated minus synthetic",
         title = paste0("Event-specific gaps: ", mid)) +
    theme_dz
  ggsave(file.path(mdir, "dz_fig2_gaps.png"), fig2,
         width = 7, height = 4.5, dpi = 200)

  # MAIN REPORTED FIGURE. Two stacked panels keep the plan's separation of
  # pooled magnitude (section 12.1, top) from pooled evidence (section 12.2,
  # bottom). Both ribbons are joint-randomization NULL bands, that is where the
  # statistic would lie under no effect, and not confidence intervals. Long
  # format with facet_grid avoids taking on a plotting dependency.
  dyn <- pooled_dynamic[model_id == mid]
  horizon_grid_plot <- sort(unique(dyn$event_time))
  if (length(horizon_grid_plot) > 12L)
    horizon_grid_plot <- pretty(horizon_grid_plot, n = 8L)
  panel_effect <- "Pooled effect (births per 100k pop-year)"
  panel_rank <- "Mean placebo percentile rank"
  dyn_long <- rbindlist(list(
    dyn[, .(event_time, panel = panel_effect, value = pooled_effect,
            lower = effect_ci_lower, upper = effect_ci_upper,
            band = "95% randomization CI", p = randomization_p, n_events)],
    dyn[, .(event_time, panel = panel_rank, value = mean_percentile_rank,
            lower = rank_null_lower, upper = rank_null_upper,
            band = "95% rank null envelope", p = rank_randomization_p,
            n_events)]
  ), use.names = TRUE)
  dyn_long[, panel := factor(panel, levels = c(panel_effect, panel_rank))]
  reference_lines <- data.table(
    panel = factor(c(panel_effect, panel_rank),
                   levels = c(panel_effect, panel_rank)),
    yintercept = c(0, 16 / 31)
  )
  flagged <- dyn_long[is.finite(p) & p < 0.05]
  att_row <- pooled_att[model_id == mid]
  fig3 <- ggplot(dyn_long, aes(event_time, value)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = band), alpha = 0.55,
                na.rm = TRUE) +
    geom_hline(data = reference_lines, aes(yintercept = yintercept),
               colour = "grey55", linewidth = 0.3) +
    geom_vline(xintercept = event_window$treatment_offset, colour = "grey55",
               linewidth = 0.3, linetype = "dashed") +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.6) +
    {if (nrow(flagged)) geom_point(data = flagged, colour = "firebrick",
                                   size = 2.6) else NULL} +
    facet_grid(panel ~ ., scales = "free_y", switch = "y") +
    scale_fill_manual(values = c(
      "95% randomization CI" = "grey72",
      "95% rank null envelope" = "#9ecae1"
    ), name = NULL) +
    labs(
      x = "Event time (years, relative to the fair decade)", y = NULL,
      title = paste0("Pooled event study: ", mid),
      subtitle = paste0(
        "Pooled ATT ", signif(att_row$pooled_att, 3),
        ", rank p = ", signif(att_row$randomization_p_two_sided, 3),
        "; any-post-horizon p = ",
        signif(att_row$randomization_p_any_post_horizon, 3), " (effects), ",
        signif(att_row$randomization_p_any_post_horizon_rank, 3), " (ranks)",
        "\nTop panel: pooled magnitude (plan 12.1). Bottom: rank evidence",
        " (plan 12.2), null centre ", signif(16 / 31, 3), ".",
        "\nTop: pointwise 95% confidence intervals from joint-randomization ",
        "test inversion. Bottom: 95% rank null envelopes.",
        "\nEvent time 0 is the omitted final pre-period; treatment starts at ",
        event_window$treatment_offset, ".",
        "\nRed marks non-reference horizons with p < 0.05."
      )
    ) +
    scale_x_continuous(breaks = horizon_grid_plot) +
    theme_dz +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 90),
          legend.position = "bottom",
          plot.subtitle = element_text(size = 7.5, lineheight = 1.15))
  ggsave(file.path(mdir, "dz_fig3_pooled_event_time.png"), fig3,
         width = 7.5, height = 6.5, dpi = 200)

  pl <- placebo_events[model_id == mid & successful == TRUE &
                         is.finite(standardized_effect)]
  fig4 <- ggplot(pl, aes(treated_lgd_name, standardized_effect)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_jitter(width = 0.15, alpha = 0.4, size = 0.8, colour = "grey40") +
    geom_point(data = effects,
               aes(treated_lgd_name, standardized_effect),
               colour = "firebrick", size = 2.2) +
    coord_flip() +
    labs(x = NULL, y = "Standardized effect (tau / pre-RMSPE)",
         title = paste0("Observed versus placebo standardized effects: ", mid),
         subtitle = "Red points are the treated events") +
    theme_dz
  ggsave(file.path(mdir, "dz_fig4_placebo.png"), fig4,
         width = 7, height = 5, dpi = 200)

  tr <- trimming[model_id == mid]
  fig5 <- ggplot(tr, aes(worst_fitting_events_dropped, pooled_att)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_line(linewidth = 0.5) + geom_point(size = 1.8) +
    scale_x_continuous(breaks = tr$worst_fitting_events_dropped) +
    labs(x = "Worst pre-fitting events dropped", y = "Pooled ATT",
         title = paste0("Progressive pre-fit trimming: ", mid)) +
    theme_dz
  ggsave(file.path(mdir, "dz_fig5_trimming.png"), fig5,
         width = 6.5, height = 4, dpi = 200)

  dyn_trim <- pooled_dynamic_by_trim[model_id == mid]
  dyn_trim[, panel := factor(
    worst_fitting_events_dropped,
    levels = sort(unique(worst_fitting_events_dropped)),
    labels = paste0("drop ", sort(unique(worst_fitting_events_dropped)),
                    " (n=", n_events[match(
                      sort(unique(worst_fitting_events_dropped)),
                      worst_fitting_events_dropped
                    )], ")")
  )]
  fig7 <- ggplot(dyn_trim, aes(event_time, pooled_effect)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_vline(xintercept = event_window$treatment_offset, colour = "grey60",
               linewidth = 0.3) +
    geom_ribbon(aes(ymin = effect_ci_lower, ymax = effect_ci_upper),
                fill = "grey82", alpha = 0.7) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1) +
    facet_wrap(~ panel) +
    labs(x = "Event time (years)",
         y = "Equal-weighted pooled effect (final pre-period omitted)",
         title = paste0("Pooled event study by trim stage: ", mid),
         subtitle = paste0("Event time 0 is the omitted final pre-period; ",
                           "bands are pointwise 95% joint-randomization ",
                           "confidence intervals. Worst pre-fitting events ",
                           "are dropped first.")) +
    theme_dz
  ggsave(file.path(mdir, "dz_fig7_trim_event_studies.png"), fig7,
         width = 9, height = 6, dpi = 200)

  rd <- randomization_draws_table[model_id == mid]
  obs <- pooled_att[model_id == mid, observed_mean_percentile_rank]
  fig6 <- ggplot(rd, aes(mean_percentile_rank)) +
    geom_histogram(bins = 30, fill = "grey75", colour = "white") +
    geom_vline(xintercept = obs, colour = "firebrick", linewidth = 0.7) +
    geom_vline(xintercept = pooled_att[model_id == mid,
                                       randomization_centre_used],
               colour = "grey40", linetype = 2, linewidth = 0.4) +
    labs(x = "Mean placebo percentile rank", y = "Joint randomization draws",
         title = paste0("Pooled randomization distribution: ", mid),
         subtitle = paste0("Observed = ", signif(obs, 4),
                           "; two-sided p = ",
                           signif(pooled_att[model_id == mid,
                                             randomization_p_two_sided], 3))) +
    theme_dz
  ggsave(file.path(mdir, "dz_fig6_randomization.png"), fig6,
         width = 6.5, height = 4, dpi = 200)
}

###############################################################################
# QC and write
###############################################################################

qc <- rbindlist(lapply(unique(event_effects$model_id), function(mid) {
  ev <- event_effects[model_id == mid]
  pl <- placebo_events[model_id == mid]
  rd <- randomization_draws_table[model_id == mid]
  fs <- feasibility[model_id == mid]
  data.table(
    model_id = mid,
    matching_specification_id = matching_spec_id,
    event_window = event_window_id,
    reconciled_with_stage2 = reconcile_with_stage2,
    timing_eligible_events = nrow(fs),
    infeasible_events = fs[feasible == FALSE, .N],
    n_pre_periods = paste(sort(unique(ev$n_pre_periods)), collapse = "/"),
    n_post_periods = paste(sort(unique(ev$n_post_periods)), collapse = "/"),
    events_pooled = nrow(ev),
    placebo_fits_requested = nrow(pl),
    placebo_fits_successful = pl[successful == TRUE, .N],
    placebo_fits_per_event_min = pl[successful == TRUE, .N,
                                    by = treated_lgd_id][, min(N)],
    placebo_fits_per_event_max = pl[successful == TRUE, .N,
                                    by = treated_lgd_id][, max(N)],
    donor_weight_sum_error = donor_weights[model_id == mid, .(
      s = sum(weight)), by = treated_lgd_id][, max(abs(s - 1))],
    events_with_zero_pre_path = sum(ev$pre_treatment_outcome_all_zero),
    zero_pre_path_event_names = paste(
      ev[pre_treatment_outcome_all_zero == TRUE, treated_lgd_name],
      collapse = "; "
    ),
    nrmspe_pre_missing = sum(!is.finite(ev$nrmspe_pre)),
    min_percentile_rank = min(ev$percentile_rank),
    max_percentile_rank = max(ev$percentile_rank),
    mean_percentile_rank = mean(ev$percentile_rank),
    randomization_draws_used = nrow(rd),
    randomization_null_mean = mean(rd$mean_percentile_rank),
    randomization_null_mean_att = mean(rd$pooled_att),
    randomization_null_theoretical_centre = 16 / 31,
    randomization_null_centring_error =
      abs(mean(rd$mean_percentile_rank) - 16 / 31),
    incomplete_assignment_draws = rd[
      n_assigned_events < nrow(ev), .N
    ],
    gap_identity_error = max(abs(
      event_gap_paths[model_id == mid,
                      treated_outcome - synthetic_outcome - gap]
    )),
    # Averaging the offset-adjusted path over the post periods must reproduce
    # the event effect exactly.
    tau_recomputation_error = {
      recomputed <- event_gap_paths[model_id == mid & period == "post", .(
        tau = mean(gap_adjusted)), by = treated_lgd_id]
      merged <- merge(recomputed,
                      ev[, .(treated_lgd_id, tau_event_effect)],
                      by = "treated_lgd_id")
      max(abs(merged$tau - merged$tau_event_effect))
    },
    placebo_in_time_estimated = placebo_in_time[
      model_id == mid & status == "estimated", .N
    ],
    # Averaging the pooled adjusted event study over its post periods must
    # reproduce the pooled ATT, which only holds if the period labelling
    # follows the treatment start rather than event time zero.
    pooled_path_vs_pooled_att = abs(
      pooled_dynamic[model_id == mid & period == "post", mean(pooled_effect)] -
        mean(ev$tau_event_effect)
    ),
    # The per-horizon layer must reduce to the scalar apparatus: averaging
    # S_{e,h} over an event's post horizons divides the same adjusted path by
    # the same RMSPE_pre_e, so it must reproduce the scalar S_e exactly. This
    # is the strongest single check that the horizon wiring is correct.
    horizon_to_scalar_s_error = {
      recomputed <- event_horizon_effects[
        model_id == mid & period == "post",
        .(s_h = mean(standardized_effect)), by = treated_lgd_id
      ]
      cmp <- merge(recomputed, ev[, .(treated_lgd_id, standardized_effect)],
                   by = "treated_lgd_id")
      if (nrow(cmp) != nrow(ev)) NA_real_ else
        max(abs(cmp$s_h - cmp$standardized_effect))
    },
    reference_path_error = event_gap_paths[
      model_id == mid & event_time == reference_event_time,
      max(abs(gap_adjusted))
    ],
    reference_interval_rows = pooled_dynamic[
      model_id == mid & event_time == reference_event_time,
      sum(is.finite(effect_ci_lower) | is.finite(effect_ci_upper))
    ],
    horizon_bands_missing = pooled_dynamic[
      model_id == mid & event_time != reference_event_time,
      sum(!is.finite(effect_ci_lower) | !is.finite(effect_ci_upper) |
            !is.finite(rank_null_lower) | !is.finite(rank_null_upper))
    ],
    horizon_p_out_of_range = pooled_dynamic[
      model_id == mid & event_time != reference_event_time, sum(
      !is.finite(randomization_p) | randomization_p <= 0 |
        randomization_p > 1 | !is.finite(rank_randomization_p) |
        rank_randomization_p <= 0 | rank_randomization_p > 1
    )],
    ci_zero_decision_mismatch = pooled_dynamic[
      model_id == mid & event_time != reference_event_time, sum(
        ((effect_ci_lower <= 0 & effect_ci_upper >= 0) !=
           (randomization_p >= 0.05))
      )
    ],
    horizon_draws_min = pooled_dynamic_by_trim[
      model_id == mid, min(randomization_draws_used)
    ],
    horizon_rank_null_centring_error_max = pooled_dynamic[
      model_id == mid & event_time != reference_event_time,
      max(abs(rank_null_mean - 16 / 31))
    ],
    horizon_att_null_mean_range = pooled_dynamic[model_id == mid, paste0(
      signif(min(null_mean), 3), " to ", signif(max(null_mean), 3)
    )],
    horizon_stage0_band_matches_baseline = {
      base <- pooled_dynamic[model_id == mid, .(event_time, effect_ci_lower,
                                                effect_ci_upper)]
      stage0 <- pooled_dynamic_by_trim[
        model_id == mid & worst_fitting_events_dropped == 0L,
        .(event_time, t_lower = effect_ci_lower,
          t_upper = effect_ci_upper)
      ]
      cmp <- merge(base, stage0, by = "event_time")
      cmp <- cmp[is.finite(effect_ci_lower)]
      if (!nrow(cmp)) NA_real_ else
        max(abs(cmp$effect_ci_lower - cmp$t_lower),
            abs(cmp$effect_ci_upper - cmp$t_upper))
    },
    # Pre-treatment horizons that look significant are a pre-trend warning, or
    # a sign the ATT scale is unreliable for that specification, rather than an
    # effect. Recorded so they cannot pass unnoticed.
    pre_horizons_p_below_05 = pooled_dynamic[
      model_id == mid & event_time < event_window$treatment_offset,
      sum(is.finite(randomization_p) & randomization_p < 0.05)
    ],
    post_horizons_p_below_05 = pooled_dynamic[
      model_id == mid & event_time >= event_window$treatment_offset,
      sum(is.finite(randomization_p) & randomization_p < 0.05)
    ],
    pre_horizons_rank_p_below_05 = pooled_dynamic[
      model_id == mid & event_time < event_window$treatment_offset,
      sum(is.finite(rank_randomization_p) & rank_randomization_p < 0.05)
    ],
    post_horizons_rank_p_below_05 = pooled_dynamic[
      model_id == mid & event_time >= event_window$treatment_offset,
      sum(is.finite(rank_randomization_p) & rank_randomization_p < 0.05)
    ],
    placebo_rank_ties_mean_earliest_horizon = {
      h0 <- placebo_horizon[model_id == mid, min(event_time)]
      placebo_horizon[model_id == mid & event_time == h0, .(
        ties = .N - uniqueN(round(standardized_effect, 12))
      ), by = treated_lgd_id][, mean(ties)]
    },
    post_horizons_outside_null_band = pooled_dynamic[
      model_id == mid & event_time >= event_window$treatment_offset,
      sum(pooled_effect < null_lower | pooled_effect > null_upper)
    ],
    post_horizons_rank_outside_null_band = pooled_dynamic[
      model_id == mid & event_time >= event_window$treatment_offset,
      sum(mean_percentile_rank < rank_null_lower |
            mean_percentile_rank > rank_null_upper)
    ],
    trim_event_time_grid_common = trim_grid_is_common,
    trim_event_times_min = trim_time_counts[model_id == mid, min(V1)],
    trim_event_times_max = trim_time_counts[model_id == mid, max(V1)],
    trim_stages_with_event_study = pooled_dynamic_by_trim[
      model_id == mid, uniqueN(worst_fitting_events_dropped)
    ],
    trim_stage0_matches_baseline_dynamic = {
      base <- pooled_dynamic[model_id == mid, .(event_time, pooled_effect)]
      stage0 <- pooled_dynamic_by_trim[
        model_id == mid & worst_fitting_events_dropped == 0L,
        .(event_time, trim_effect = pooled_effect)
      ]
      cmp <- merge(base, stage0, by = "event_time")
      if (nrow(cmp) != nrow(base)) NA_real_ else
        max(abs(cmp$pooled_effect - cmp$trim_effect))
    },
    influential_donors_tested = leave_donor_out[model_id == mid, .N]
  )
}), fill = TRUE, use.names = TRUE)

if (any(qc$gap_identity_error > tolerance))
  stop("Gap paths do not equal treated minus synthetic.")
if (any(qc$reference_path_error > tolerance) ||
    any(qc$reference_interval_rows > 0L))
  stop("The final pre-period must be zero with no confidence interval.")
if (any(qc$tau_recomputation_error > 1e-8))
  stop("Event effects do not equal the mean reference-normalized post gap.")
if (any(qc$donor_weight_sum_error > 1e-8))
  stop("Donor weights do not sum to one for every event.")
if (any(qc$nrmspe_pre_missing > 0L))
  stop("Normalized pre-treatment fit must be defined for every event.")
if (any(!is.finite(qc$horizon_to_scalar_s_error)) ||
    any(qc$horizon_to_scalar_s_error > 1e-8))
  stop("Per-horizon standardized effects do not reduce to the scalar S_e.")
if (any(qc$horizon_bands_missing > 0L))
  stop("Every non-reference horizon must carry effect CIs and rank bands.")
if (any(qc$horizon_p_out_of_range > 0L))
  stop("Per-horizon p-values must lie in (0, 1].")
if (any(qc$ci_zero_decision_mismatch > 0L))
  stop("CI inclusion of zero must match the inverted randomization test.")
if (any(!is.finite(qc$horizon_stage0_band_matches_baseline)) ||
    any(qc$horizon_stage0_band_matches_baseline > tolerance))
  stop("The untrimmed stage band must equal the baseline event-study band.")
# The exchangeable value 16/31 is only an approximation for the rank null. Two
# features of the design push it up: ties among placebo statistics, which the
# plan's <= indicator counts in full, and the joint assignment, which is
# deliberately non-uniform so that donor sharing is preserved (plan 16). The
# observed per-horizon nulls sit around 0.51 to 0.63, correlating 0.74 with the
# per-horizon tie count. This is recorded rather than asserted, and it is
# harmless because every p-value is centred on its own empirical null. A wide
# bound still catches genuine breakage.
if (any(qc$horizon_rank_null_centring_error_max > 0.25))
  stop("Per-horizon rank nulls are implausibly far from 16/31; ",
       "the joint assignment or the rank matrix is likely broken.")
if (event_window$balanced && any(qc$horizon_draws_min < randomization_draws))
  stop("Every horizon-by-stage cell must use all randomization draws.")
if (any(!is.finite(qc$trim_stage0_matches_baseline_dynamic)) ||
    any(qc$trim_stage0_matches_baseline_dynamic > tolerance))
  stop("The untrimmed trim stage must reproduce the baseline event study.")
# This identity holds only when every event contributes the same post periods,
# which the balanced window guarantees. Under the full window the pooled path
# averages a different number of events at each event time, so its post-period
# mean legitimately differs from the mean of the event effects; the discrepancy
# is reported in the QC table instead of enforced.
if (event_window$balanced &&
    (any(!is.finite(qc$pooled_path_vs_pooled_att)) ||
     any(qc$pooled_path_vs_pooled_att > 1e-8)))
  stop("The pooled event study must integrate to the pooled ATT.")
if (any(qc$placebo_fits_per_event_min < expected_donors - 1L))
  message("NOTE: at least one event has failed placebo fits; see dz_qc.csv.")

fwrite(feasibility, feasibility_file, na = "")
fwrite(event_effects, event_effects_file, na = "")
fwrite(event_gap_paths, event_paths_file, na = "")
fwrite(event_horizon_effects, event_horizon_file, na = "")
fwrite(placebo_events, placebo_events_file, na = "")
fwrite(placebo_gap_paths, placebo_paths_file, na = "")
fwrite(placebo_restrictions, placebo_restrict_file, na = "")
fwrite(pooled_att, pooled_att_file, na = "")
fwrite(pooled_dynamic, pooled_dynamic_file, na = "")
fwrite(pooled_dynamic_by_trim, pooled_dynamic_trim_file, na = "")
fwrite(trimming, trimming_file, na = "")
fwrite(loeo, loeo_file, na = "")
fwrite(leave_donor_out, leave_donor_file, na = "")
fwrite(placebo_in_time, placebo_time_file, na = "")
fwrite(donor_usage, donor_usage_file, na = "")
fwrite(donor_overlap, donor_overlap_file, na = "")
fwrite(randomization_draws_table, randomization_file, na = "")
fwrite(table1, table1_file, na = "")
fwrite(table2, table2_file, na = "")
fwrite(table3, table3_file, na = "")
fwrite(table4, table4_file, na = "")
fwrite(decisions, decisions_file, na = "")
fwrite(qc, qc_file, na = "")
saveRDS(model_objects, models_file)

message("Dube-Zipperer pooled synthetic-control analysis complete.")
message("  Matching specification: ", matching_spec_id)
message("  Event window: ", event_window_id)
message("  Reconciled with the per-event stage: ", reconcile_with_stage2)
message("  Events pooled: ", nrow(event_effects), " of ",
        nrow(feasibility), " timing-eligible")
if (feasibility[feasible == FALSE, .N]) {
  message("  Infeasible (no pre-period variation): ",
          paste(feasibility[feasible == FALSE, paste0(model_id, "/",
                treated_lgd_name)], collapse = ", "))
}
message("  Placebo fits: ", placebo_events[successful == TRUE, .N], "/",
        nrow(placebo_events))
message("  Randomization draws: ", randomization_draws)
print(pooled_att[, .(
  model_id, n_events, pooled_att = round(pooled_att, 4),
  mean_rank = round(mean_percentile_rank, 4),
  s_bar = round(mean_standardized_effect, 3),
  s_med = round(median_standardized_effect, 3),
  p_rank = round(randomization_p_two_sided, 4),
  zero_pre = events_with_zero_pre_path,
  n_inf = n_events_informative_pre,
  att_inf = round(pooled_att_informative_pre, 4),
  rbar_inf = round(mean_percentile_rank_informative_pre, 4),
  p_inf = round(randomization_p_informative_pre, 4)
)])
message("  Per-horizon joint tests (any post-treatment horizon):")
print(pooled_att[, .(
  model_id,
  p_any_horizon_effect = round(randomization_p_any_post_horizon, 4),
  p_any_horizon_rank = round(randomization_p_any_post_horizon_rank, 4)
)])
print(qc[, .(model_id,
  s_reduction_err = signif(horizon_to_scalar_s_error, 2),
  pre_p05_att = pre_horizons_p_below_05,
  post_p05_att = post_horizons_p_below_05,
  pre_p05_rank = pre_horizons_rank_p_below_05,
  post_p05_rank = post_horizons_rank_p_below_05,
  rank_null_dev = round(horizon_rank_null_centring_error_max, 3),
  ties_h0 = round(placebo_rank_ties_mean_earliest_horizon, 1))])
if (any(qc$pre_horizons_p_below_05 > 0L)) {
  message("NOTE: some specifications show significant PRE-treatment horizons ",
          "on the effect scale. Read that as a pre-trend or scale warning for ",
          "those specifications, not as an effect; compare the rank panel.")
}
message("  Outputs: ", results_dir)
