# Annual county-level C&S DiD, schooling exposure ages 4-9.
suppressPackageStartupMessages({ library(data.table); library(did); library(ggplot2) })
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(arg)) {
  repo_root <- normalizePath(file.path(dirname(sub("^--file=", "", arg[1])), "../.."), winslash = "/")
} else repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/")
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "analysis/boll_weevil/csdid_helpers.R"))
config <- bw_config()
output_dir <- file.path(TALENT_DETS_DATA_DIR, "results/boll_weevil/amws_county_csdid")
dir.create(file.path(output_dir, "models"), recursive = TRUE, showWarnings = FALSE)
save_csv <- function(x, name) fwrite(x, file.path(output_dir, paste0(name, ".csv")), na = "NA")
warnings_log <- data.table(stage = character(), message = character())
capture_warnings <- function(expr, stage) withCallingHandlers(expr, warning = function(w) {
  warnings_log <<- rbind(warnings_log, data.table(stage = stage, message = conditionMessage(w)))
  fwrite(unique(warnings_log), file.path(output_dir, "estimation_warnings.csv"))
  invokeRestart("muffleWarning")
})
message("Reading and validating the balanced county panel.")
sample <- bw_read_sample(TALENT_DETS_DATA_DIR, config)
save_csv(sample$audit, "sample_inclusion_audit")
save_csv(sample$targets, "treated_counties")
save_csv(sample$units, "estimation_counties")
save_csv(sample$panel, "estimation_panel")
support <- bw_comparisons(sample$panel, sample$weights, config)
save_csv(support, "comparison_support")
save_csv(sample$weights, "target_cohort_weights")
state_composition <- sample$units[, .(counties = .N), by = .(role, statename)]
save_csv(state_composition, "state_composition")
set.seed(config$seed)
message("Fitting C&S: ", nrow(sample$units), " counties, ", nrow(sample$panel), " annual observations.")
fit <- capture_warnings(did::att_gt(
  yname = "model_y", tname = "year", idname = "id", gname = "g",
  data = as.data.frame(sample$panel[, .(id, year, g, model_y)]),
  xformla = ~1, est_method = "dr", panel = TRUE, allow_unbalanced_panel = FALSE,
  control_group = "notyettreated", base_period = "universal", anticipation = 0,
  clustervars = "id", bstrap = TRUE, biters = config$biters, cband = FALSE,
  alp = 0.05, faster_mode = TRUE), "att_gt")
bw_require(fit$n == 450L, "did changed the county sample unexpectedly.")
bw_require(fit$DIDparams$panel && !fit$DIDparams$allow_unbalanced_panel &&
  fit$DIDparams$anticipation == 0L && fit$DIDparams$base_period == "universal" &&
  fit$DIDparams$control_group == "notyettreated" && fit$DIDparams$idname == "id" &&
  length(setdiff(fit$DIDparams$clustervars, "id")) == 0L,
  "Fitted configuration differs from planned balanced, county-clustered design.")
saveRDS(list(fit = fit, config = config, input_md5 = sample$input_md5),
        file.path(output_dir, "models/att_gt.rds"), compress = "gzip")
message("Aggregating target ages, retaining joint covariance and county weights.")
# Give aggregation its own reproducible stream: upstream bootstrap consumption
# must not change the reported confidence bands when fit internals change.
set.seed(config$seed)
results <- capture_warnings(bw_aggregate(fit, sample, config), "aggte")
direct <- merge(results$cells[, .(g, year, age_at_infestation, att)],
                support[, .(g, year, control_counties)], by = c("g", "year"))
direct[, manual_att := mapply(function(gg, tt) bw_manual_did(sample$panel, gg, tt), g, year)]
direct[, absolute_error := abs(att - manual_att)]
save_csv(direct, "direct_did_validation")
bw_require(max(direct$absolute_error) < 1e-9, "C&S and direct unconditional DiD disagree.")
save_csv(results$cells, "group_time_coefficients")
save_csv(results$dynamic, "event_study_coefficients")
save_csv(results$pooled, "pooled_age_window_coefficients")
save_csv(results$weights, "aggregation_weights")
save_csv(results$dynamic_validation, "dynamic_weight_validation")
save_csv(results$pooled_validation, "pooled_weight_and_influence_validation")

dynamic_cov <- crossprod(results$dynamic_influence) / fit$n^2
dimnames(dynamic_cov) <- rep(list(paste0("age_", results$dynamic$age_at_infestation)), 2)
pooled_cov <- crossprod(results$pooled_influence) / fit$n^2
save_csv(as.data.table(dynamic_cov, keep.rownames = "age"), "event_study_analytic_covariance")
save_csv(as.data.table(pooled_cov, keep.rownames = "age_window"), "pooled_analytic_covariance")
saveRDS(list(dynamic = results$dynamic_object, pooled = results$pooled_objects,
             dynamic_influence = results$dynamic_influence, pooled_influence = results$pooled_influence,
             influence_ids = rownames(fit$inffunc), config = config, input_md5 = sample$input_md5),
        file.path(output_dir, "models/aggregations_and_influence.rds"), compress = "gzip")
pre <- copy(results$dynamic[age_at_infestation >= 20L])
pre[, simultaneous_band_excludes_zero := simultaneous_low > 0 | simultaneous_high < 0]
save_csv(pre, "pre_exposure_diagnostics")
raw <- sample$panel[role == "target" & age_at_infestation %in% c(4:9, 19L)]
raw[, age_window := fifelse(age_at_infestation == 19L, "19_reference", "4-9")]
descriptives <- raw[, .(county_age_observations = .N, scientists = sum(n_amws),
  scientists_early_editions = sum(n_amws_1906_1955_dedup), scientists_1986 = sum(n_amws_1986),
  estimated_births = sum(county_births_estimate_year), mean_county_rate = mean(model_y),
  zero_scientist_share = mean(n_amws == 0)), by = age_window]
save_csv(descriptives, "outcome_descriptives")
summary <- data.table(outcome = config$outcome, counties = nrow(sample$units),
  county_years = nrow(sample$panel), target_counties = nrow(sample$targets),
  later_infested_counties = sum(sample$units$role == "later_infested"),
  target_states = uniqueN(sample$targets$state_id), target_groups = nrow(sample$weights),
  first_birth_year = min(config$years), last_birth_year = max(config$years),
  first_target_infestation = min(sample$targets$infestation_year),
  last_target_infestation = max(sample$targets$infestation_year),
  minimum_control_counties = min(support$control_counties),
  minimum_control_states = min(support$control_states), reference_age = config$reference_age,
  target_min_age = 4L, target_max_age = 9L, age_4_9_county_cohorts = 6L * nrow(sample$targets),
  bootstrap_iterations = config$biters, seed = config$seed,
  simultaneous_critical_value = results$dynamic_object$crit.val.egt,
  pre_exposure_ages_excluding_zero_simultaneously = sum(pre$simultaneous_band_excludes_zero))
save_csv(summary, "sample_summary")
code_paths <- file.path(repo_root, c("paths.R", "analysis/boll_weevil/analysis_boll_weevil_amws_csdid.R",
                                    "analysis/boll_weevil/csdid_helpers.R"))
manifest_paths <- c(sample$paths, code_paths)
save_csv(data.table(path = unname(normalizePath(manifest_paths, winslash = "/")),
                    md5 = unname(tools::md5sum(manifest_paths))), "input_and_code_manifest")
bw_require(identical(unname(sample$input_md5), unname(tools::md5sum(sample$paths))),
             "An input changed during estimation.")
validation <- data.table(
  check = c("balanced_counties", "balanced_rows", "targets", "minimum_controls", "stored_rate_error",
    "direct_did_max_error", "dynamic_weight_max_error", "pooled_weight_max_error", "pooled_influence_max_error",
    "age19_reference_att", "age19_reference_se", "simultaneous_band_provided"),
  observed = c(fit$n, nrow(sample$panel), nrow(sample$targets), min(support$control_counties),
    sample$outcome_max_error, max(direct$absolute_error), max(results$dynamic_validation$absolute_error),
    max(results$pooled_validation$absolute_error), max(results$pooled_validation$max_influence_error),
    results$dynamic[age_at_infestation == 19L, att], results$dynamic[age_at_infestation == 19L, se],
    as.integer(results$dynamic_object$DIDparams$cband)),
  expected = c(450, 22950, 86, 33, 0, 0, 0, 0, 0, 0, 0, 1),
  tolerance = c(rep(0, 4), rep(1e-8, 7), 0))
validation[, passed := abs(observed - expected) <= tolerance]
save_csv(validation, "validation_checks")
bw_require(all(validation$passed), "Output validation failed.")
save_csv(if (nrow(warnings_log)) unique(warnings_log) else data.table(stage = "all", message = "none"),
         "estimation_warnings")
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))
bw_plot(results, output_dir)

pooled_text <- vapply(seq_len(nrow(results$pooled)), function(i) {
  r <- results$pooled[i]
  sprintf("| %s | %.4f | %.4f | [%.4f, %.4f] | %d |", r$age_window, r$att, r$se,
          r$conf_low, r$conf_high, r$county_age_observations)
}, character(1L))
diagnostic_text <- if (any(pre$simultaneous_band_excludes_zero)) {
  paste0("The simultaneous bands exclude zero for pre-exposure ages ",
    paste(pre[simultaneous_band_excludes_zero == TRUE, sort(age_at_infestation)], collapse = ", "),
    ". This is evidence against a flat pre-exposure profile under the design; causal interpretation requires caution.")
} else {
  "No age 20-40 estimate excludes zero using the simultaneous bands. This does not establish parallel trends or rule out meaningful violations."
}
if (any(grepl("Not returning pre-test Wald statistic", warnings_log$message, fixed = TRUE))) {
  diagnostic_text <- paste(diagnostic_text,
    "The package's omnibus pre-treatment Wald statistic is unavailable because its covariance matrix is singular; it is not reported as a passed pretrend test.")
}
reference_raw <- descriptives[age_window == "19_reference"]
target_raw <- descriptives[age_window == "4-9"]
readme_path <- normalizePath(file.path(repo_root, "analysis/boll_weevil/README.md"), winslash = "/", mustWork = FALSE)
writeLines(c(
  "# Boll weevil exposure and AMWS scientist generation", "",
  "## Main estimates", "",
  "Effects are AMWS scientists born per 1,000 estimated births, relative to the cohort aged 19 at infestation.",
  "Positive estimates indicate an increase; intervals use county-clustered multiplier-bootstrap standard errors.", "",
  "| Age at infestation | ATT | Bootstrap SE | 95% confidence interval | County-age observations |",
  "| --- | ---: | ---: | --- | ---: |", pooled_text, "",
  "Confidence intervals that include zero do not establish a zero effect. The primary 4-9 window averages the six annual cohorts equally within each equally weighted county.", "",
  sprintf("The numerator is sparse: %d scientist births across %d age-19 reference county-cohorts, and %d across %d target age-4-9 county-cohorts. Mean county rates are %.4f and %.4f per 1,000 estimated births, respectively. These raw means are descriptive, not the DiD estimate. The wide intervals do not support a precise-null interpretation.",
    reference_raw$scientists, reference_raw$county_age_observations, target_raw$scientists,
    target_raw$county_age_observations, reference_raw$mean_county_rate, target_raw$mean_county_rate), "",
  "## Sample and estimator", "",
  "The balanced 1853-1903 panel has 450 counties and 22,950 county-birth-year observations. The estimand covers 86 counties infested in 1893-1907: 59 in Texas, 21 in Louisiana, five in Arkansas, and one in Mississippi. Another 364 later-infested counties are available as controls; they stop serving as controls when their own birth cohorts become exposed.",
  "Infestation is dated by the Baker midpoint. First potentially exposed birth cohort g = infestation year - 18. Thus ages 4-9 correspond to event times +9 through +14, and the reference birth cohort g-1 was aged 19 at infestation. Ages 10-18 are potentially affected, not untreated controls. This reference differs from Baker's pooled ages 19-30.",
  "Callaway-Sant'Anna att_gt uses not-yet-treated comparisons, a universal base, no additional anticipation, an intercept-only specification, and a balanced panel. Target group-time effects are aggregated with weights proportional to target-county counts; the same 86 counties contribute at every displayed age. All three pooled windows retain joint influence-function covariance, including group-share weight uncertainty.",
  "The tightest comparison uses 33 controls (31 in North Carolina and two in Tennessee). This limited geographic support and concentration of targets in Texas constrain generalization. Results do not describe all 681 Baker-rule-eligible Southern counties.", "",
  "## Pre-exposure diagnostics", "", diagnostic_text, "",
  "![Event study](event_study_age_at_infestation.png)", "",
  "The ribbon is a 95% simultaneous band across all 37 reported ages, including pre-exposure ages. Age 19 is normalized to zero. The diagnostic is not the unrestricted age-cohort pretrend regression used by Baker.", "",
  "## Measurement and interpretation", "",
  "County of birth proxies schooling location: childhood migration creates exposure misclassification. Nominal GEOID verification does not harmonize historical boundaries. Requiring common annual coverage selects older counties and may affect external validity.",
  "The outcome uses the existing combined county-geocoded AMWS panel, preserving its inclusion rules. Per the user's information, previously listed 1986 entries generally lack county information and therefore do not enter the county numerator again; this analysis performs no new person deduplication. Edition components are reconciled and reported in outcome_descriptives.csv.",
  "County births are estimates based on county population and the national birth rate, not observed local births. Zero scientist counts are retained; invalid denominators are excluded. Cotton intensity is not a sample requirement or covariate. County-clustered inference accommodates dependence across birth cohorts within counties but does not additionally account for dependence across different counties.",
  "The effect is reduced form for early infestation and includes all channels, including child labor, income, health, and migration. Interpreting it causally requires parallel untreated cohort trends and the age-19 comparison cohort to be unaffected in scientist generation; that assumption is distinct from Baker's assumption for completed schooling.", "",
  "## Reproduction and audit", "",
  sprintf("Seed: %d, reset independently at estimation and aggregation; multiplier draws: %d. See [analysis README](%s).", config$seed, config$biters, readme_path),
  "The CSV exports include group-time and age-window coefficients, sample exclusions, control support, weights, raw outcome summaries, and validation. All target cells were checked against direct unconditional DiD; annual and pooled weights were independently reconstructed. Analytic covariance CSVs use joint influence functions; reported confidence intervals use multiplier-bootstrap standard errors, so the two variance calculations need not be numerically identical.",
  "Raw att_gt and aggregation/influence objects are saved under models/. Input/code hashes and session information identify the run. Warnings, including very small treatment groups, are retained in estimation_warnings.csv."
), file.path(output_dir, "results_summary.md"))
message("Complete. Results: ", output_dir)
print(results$pooled[, .(age_window, att, se, conf_low, conf_high)])
