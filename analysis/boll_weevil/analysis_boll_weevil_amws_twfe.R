suppressPackageStartupMessages({library(data.table); library(fixest); library(ggplot2)})
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", arg[1])), "../.."), winslash = "/")
} else normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/")
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "analysis/boll_weevil/twfe_helpers.R"))
output_dir <- file.path(TALENT_DETS_DATA_DIR, "results/boll_weevil/amws_county_twfe")
dir.create(file.path(output_dir, "models"), recursive = TRUE, showWarnings = FALSE)
save_csv <- function(x, name) fwrite(x, file.path(output_dir, paste0(name, ".csv")), na = "NA")
cs_dir <- file.path(TALENT_DETS_DATA_DIR, "results/boll_weevil/amws_county_csdid")
input_paths <- c(
  treatment = file.path(TALENT_DETS_DATA_DIR, "output/boll_weevil/boll_weevil_counties_1920.csv"),
  panel = file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv"),
  cs_targets = file.path(cs_dir, "treated_counties.csv"),
  cs_estimates = file.path(cs_dir, "pooled_age_window_coefficients.csv"))
bwfe_require(all(file.exists(input_paths)), "Missing treatment, AMWS panel, or completed CS DID reference outputs.")
input_hashes <- tools::md5sum(input_paths)
treatment <- fread(input_paths["treatment"], colClasses = c(GEOID = "character"))
panel <- fread(input_paths["panel"], select = c("GEOID", "year", "n_amws", "n_amws_1906_1955_dedup",
  "n_amws_1986", "county_births_estimate_year", "amws_per_1000_births"), colClasses = c(GEOID = "character"))
cs_targets <- fread(input_paths["cs_targets"], colClasses = c(GEOID = "character"))
bwfe_require(nrow(cs_targets) == 86L && !anyDuplicated(cs_targets$GEOID), "CS DID target sample changed.")
bwfe_require(!anyDuplicated(panel[, .(GEOID, year)]), "Duplicate county-birth-year panel key.")
bwfe_require(all(panel$n_amws == panel$n_amws_1906_1955_dedup + panel$n_amws_1986),
             "AMWS numerator components disagree.")
eligible <- treatment[eligible_baker_2018_rules == TRUE & geoid_verified == TRUE & !is.na(GEOID)]
eligible <- eligible[, .(GEOID, source_county_id, countyname, statename, infestation_year = as.integer(bw_average_year))]
bwfe_require(!anyDuplicated(eligible$GEOID) && !anyNA(eligible$infestation_year), "Invalid treatment county identifiers/dates.")
data <- merge(panel, eligible, by = "GEOID", sort = FALSE)
data[, age_at_infestation := infestation_year - year]
data <- data[age_at_infestation %in% 4:30]
data[, valid := is.finite(amws_per_1000_births) & is.finite(county_births_estimate_year) &
  county_births_estimate_year > 0 & is.finite(n_amws) & n_amws >= 0 & n_amws == floor(n_amws)]
counts <- data[, .(observed_age_years = .N, valid_age_years = sum(valid)), by = GEOID]
audit <- merge(treatment, counts, by = "GEOID", all.x = TRUE, sort = FALSE)
audit[is.na(valid_age_years), `:=`(observed_age_years = 0L, valid_age_years = 0L)]
audit[, `:=`(in_cs_target_sample = GEOID %in% cs_targets$GEOID,
              included_broader_fe = eligible_baker_2018_rules & geoid_verified & !is.na(GEOID) & valid_age_years == 27L)]
audit[, inclusion_reason := fcase(!eligible_baker_2018_rules, "excluded_baker_rules",
  !geoid_verified | is.na(GEOID), "excluded_unverified_geoid",
  valid_age_years != 27L, "excluded_incomplete_age4_to30_window", default = "included_broader_fe")]
save_csv(audit, "sample_inclusion_audit")
full_ids <- audit[included_broader_fe == TRUE, GEOID]
bwfe_require(length(full_ids) == 590L && all(cs_targets$GEOID %in% full_ids), "Balanced age-window sample changed.")
data <- data[GEOID %in% full_ids & valid == TRUE]
data[, `:=`(id = as.integer(GEOID), model_y = amws_per_1000_births)]
data[, valid := NULL]
bins <- bwfe_exposure_bins(data$age_at_infestation)
data <- cbind(data, bins)
setorder(data, id, year)
bwfe_require(max(abs(data$model_y - 1000 * data$n_amws / data$county_births_estimate_year)) < 1e-9,
             "Outcome rates disagree with original numerator and denominator.")
formula <- model_y ~ exposure_4_6 + exposure_7_9 + exposure_10_12 + exposure_13_15 + exposure_16_18 | id + year
specs <- list(matched_86 = data[GEOID %in% cs_targets$GEOID], broader_590 = data)
coefficient_rows <- list(); validation_rows <- list(); summary_rows <- list(); warning_rows <- list()
for (spec in names(specs)) {
  d <- specs[[spec]]
  n_counties <- uniqueN(d$id)
  message("Estimating ", spec, ": ", n_counties, " counties; ", nrow(d), " county-birth-years.")
  bwfe_require(all(d[, .N, by = id]$N == 27L), "Not every county contributes all ages 4-30.")
  captured <- character()
  fit <- withCallingHandlers(feols(formula, data = d, vcov = ~id, fixef.tol = 1e-10,
    ssc = ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, G.df = "min", t.df = "min")),
    warning = function(w) {captured <<- c(captured, conditionMessage(w)); invokeRestart("muffleWarning")})
  bwfe_require(nobs(fit) == nrow(d) && length(coef(fit)) == 5L, "Regression dropped observations or age indicators.")
  V <- vcov(fit, attr = TRUE)
  df <- attr(V, "df.t")
  bwfe_require(length(df) == 1L && df == n_counties - 1L, "Unexpected clustered t degrees of freedom.")
  estimates <- bwfe_age_contrasts(coef(fit), V, df)
  estimates[, `:=`(specification = spec, counties = n_counties, observations = nrow(d),
                   first_birth_year = min(d$year), last_birth_year = max(d$year))]
  estimates[, county_age_observations := n_counties * fifelse(age_window == "4-9", 6L, 3L)]
  coefficient_rows[[spec]] <- estimates
  within <- bwfe_fwl(d$model_y, as.matrix(d[, names(bins), with = FALSE]), d$id, d$year)
  unadjusted <- vcov(fit, vcov = ~id, ssc = ssc(K.adj = FALSE, G.adj = FALSE))
  coefficient_error <- max(abs(coef(fit) - within$beta[names(coef(fit))]))
  covariance_error <- max(abs(unadjusted - within$covariance_unadjusted[rownames(unadjusted), colnames(unadjusted)]))
  residual_error <- max(abs(resid(fit) - within$residual))
  validation_rows[[spec]] <- data.table(specification = spec, independent_fwl_iterations = within$iterations,
    coefficient_max_error = coefficient_error, unadjusted_cluster_covariance_max_error = covariance_error,
    residual_max_error = residual_error, demeaned_max_county_mean = within$max_county_mean,
    demeaned_max_birth_year_mean = within$max_year_mean,
    passed = coefficient_error < 1e-8 & covariance_error < 1e-8 & residual_error < 1e-8)
  print(validation_rows[[spec]])
  bwfe_require(validation_rows[[spec]]$passed, "Independent coefficient/covariance verification failed.")
  summary_rows[[spec]] <- data.table(specification = spec, counties = n_counties, observations = nrow(d),
    states = uniqueN(substr(d$GEOID, 1, 2)), first_birth_year = min(d$year), last_birth_year = max(d$year),
    first_infestation = min(d$infestation_year), last_infestation = max(d$infestation_year),
    reference_ages = "19-30", reference_county_cohorts = sum(d$age_at_infestation %in% 19:30),
    age4_to9_county_cohorts = sum(d$age_at_infestation %in% 4:9),
    reference_scientists = sum(d[age_at_infestation %in% 19:30, n_amws]),
    age4_to9_scientists = sum(d[age_at_infestation %in% 4:9, n_amws]),
    cluster_t_df = df, adjusted_coefficient_count = attr(V, "df.K"), r_squared = as.numeric(fitstat(fit, "r2")$r2))
  warning_rows[[spec]] <- data.table(specification = spec, message = if (length(captured)) unique(captured) else "none")
  save_csv(d, paste0("estimation_panel_", spec))
  save_csv(unique(d[, .(GEOID, id, source_county_id, countyname, statename, infestation_year)]), paste0("counties_", spec))
  save_csv(unique(d[, .(id, statename)])[, .(counties = .N), by = statename], paste0("state_composition_", spec))
  save_csv(d[, .(county_age_observations = .N, scientists = sum(n_amws),
    mean_county_rate = mean(model_y), estimated_births = sum(county_births_estimate_year)),
    by = .(age_at_infestation)], paste0("descriptives_", spec))
  # Export numeric covariance values without fixest's attached call/SSC metadata.
  V_plain <- matrix(as.numeric(V), nrow = nrow(V), dimnames = dimnames(V))
  save_csv(as.data.table(V_plain, keep.rownames = "term"), paste0("coefficient_covariance_", spec))
  saveRDS(list(fit = fit, adjusted_covariance = V, contrasts = estimates,
               independent_fwl = within, input_hashes = input_hashes),
          file.path(output_dir, "models", paste0(spec, ".rds")), compress = "gzip")
}
coefficients <- rbindlist(coefficient_rows)
save_csv(coefficients, "age_window_coefficients")
save_csv(rbindlist(validation_rows), "independent_fwl_validation")
summaries <- rbindlist(summary_rows)
save_csv(summaries, "sample_summary")
save_csv(rbindlist(warning_rows), "estimation_warnings")
cs <- fread(input_paths["cs_estimates"])[, .(age_window, estimate = att, se, conf_low, conf_high,
  specification = "csdid_86_reference_age19", counties = target_counties, reference_ages = "19")]
comparison <- rbindlist(list(cs, coefficients[age_window %in% c("4-6", "7-9", "4-9"),
  .(age_window, estimate, se, conf_low, conf_high, specification, counties, reference_ages)]), use.names = TRUE)
save_csv(comparison, "comparison_with_csdid")
plot_data <- copy(coefficients[age_window %in% c("4-6", "7-9", "4-9")])
plot_data[, specification := factor(specification, levels = c("matched_86", "broader_590"),
  labels = c("Original 86 target counties", "Broader 590 eligible counties"))]
plot_data[, age_window := factor(age_window, levels = c("4-9", "7-9", "4-6"))]
figure <- ggplot(plot_data, aes(age_window, estimate, colour = specification)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.15, position = position_dodge(width = 0.4)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.4)) + coord_flip() +
  scale_colour_manual(values = c("#245B78", "#BA642F")) +
  labs(title = "Boll weevil: county and birth-year fixed effects",
    subtitle = "Baker-style exposure-age bins; pooled reference ages 19-30",
    x = "Age at infestation", y = "Effect on AMWS scientists born per 1,000 estimated births", colour = NULL,
    caption = "Equal observation weights; complete ages 4-30 within every county.\n95% t intervals from county-clustered covariance; ages 4-9 use the joint covariance of the two three-year bins.") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.caption = element_text(hjust = 0))
ggsave(file.path(output_dir, "pooled_age_windows_twfe.png"), figure, width = 10.5, height = 6, dpi = 220, bg = "white")

format_rows <- function(spec) vapply(c("4-6", "7-9", "4-9"), function(age) {
  r <- coefficients[specification == spec & age_window == age]
  sprintf("| %s | %.4f | %.4f | [%.4f, %.4f] |", age, r$estimate, r$se, r$conf_low, r$conf_high)
}, character(1))
matched_summary <- summaries[specification == "matched_86"]
matched_single_age_reference <- specs$matched_86[age_at_infestation == 19L, sum(n_amws)]
writeLines(c(
  "# Boll weevil: county and birth-year fixed-effects alternative", "",
  "## Specification", "",
  "The dependent variable is AMWS scientists born per 1,000 estimated births. OLS includes county and birth-year fixed effects plus indicators for age-at-infestation bins 4-6, 7-9, 10-12, 13-15, and 16-18. Ages 19-30 form the pooled omitted category. All counties contribute the complete ages 4-30 window, with equal observation weights. Standard errors cluster by county, use fixest's nonnested-FE small-sample adjustment, and use t critical values with counties minus one degrees of freedom.",
  "The pooled 4-9 coefficient is half the sum of the 4-6 and 7-9 coefficients. Its variance uses their full joint covariance, not an average of standard errors. No family-background, cotton-intensity, population, or other controls are added.", "",
  "## Original 86 target counties", "",
  sprintf("This specification keeps the original CS DID target counties: 59 Texas, 21 Louisiana, five Arkansas, and one Mississippi. It has %d county-birth-year observations, birth cohorts %d-%d. Its %d older reference county-cohorts contain %d scientist births, compared with %d in the prior single-age reference. The six target ages contain %d scientist births across %d county-cohorts.",
    matched_summary$observations, matched_summary$first_birth_year, matched_summary$last_birth_year,
    matched_summary$reference_county_cohorts, matched_summary$reference_scientists, matched_single_age_reference,
    matched_summary$age4_to9_scientists, matched_summary$age4_to9_county_cohorts), "",
  "| Age window | Estimate | Clustered SE | 95% CI |", "| --- | ---: | ---: | --- |", format_rows("matched_86"), "",
  "## Broader eligible sample", "",
  "This additional specification uses 590 Baker-rule-eligible counties with complete ages 4-30 coverage: 15,930 observations, birth years 1863-1918, and infestation years 1893-1922. County counts by state are exported. Its broader sample and later cohorts change the population represented; differences from the 86-county result cannot be attributed to estimator choice alone.", "",
  "| Age window | Estimate | Clustered SE | 95% CI |", "| --- | ---: | ---: | --- |", format_rows("broader_590"), "",
  sprintf("![Fixed-effects estimates](%s)", normalizePath(file.path(output_dir, "pooled_age_windows_twfe.png"), winslash = "/")), "",
  "## Relation to CS DID and Baker", "",
  "The county and birth-year fixed effects and age bins follow Baker's main schooling specification, adapted to county-aggregated scientist rates. The outcome and birth-county schooling proxy remain different, and Baker's individual-level childhood controls and race-specific male samples are not reconstructed.",
  "The previous CS DID uses a single age-19 reference and only not-yet-exposed comparison counties. This OLS regression pools ages 19-30 and exploits comparisons across counties and birth cohorts without imposing the same not-yet-treated restriction. Already-exposed cohorts can contribute to regression comparisons. Under heterogeneous effects, its coefficients need not equal the CS DID target ATT, even on the same target counties. The 86-county specification changes reference category and comparison structure as well as estimator.",
  "The broader model is an additional sample sensitivity, not an isolated estimator comparison. TWFE is not labelled a replication of Baker's schooling result, nor automatically treated as causally preferable to CS DID. Interpreting coefficients causally requires the older cohorts to be unaffected in scientist generation and untreated cohort trends to be comparable conditional on fixed effects.", "",
  "## Validation and limitations", "",
  "An independent alternating-projection FWL implementation reproduces all five coefficients, all residuals, and the unadjusted county-clustered covariance matrix for both samples. Input and CS DID files are checksummed and verified unchanged. Saved models, covariance matrices, exact sample panels, sample audits, and the side-by-side coefficient export accompany this note.",
  "County births are estimates using national birth rates and county population. County of birth proxies schooling location, so migration can misclassify exposure. Nominal GEOIDs do not harmonize historical boundaries. The scientist counts preserve the existing combined AMWS inclusion rules and edition components. Sparse outcomes and wide intervals warrant care; an interval covering zero is not proof of no effect."
), file.path(output_dir, "results_summary.md"))
code_paths <- file.path(repo_root, c("paths.R", "analysis/boll_weevil/analysis_boll_weevil_amws_twfe.R",
                                    "analysis/boll_weevil/twfe_helpers.R"))
all_paths <- c(input_paths, code_paths)
save_csv(data.table(path = unname(normalizePath(all_paths, winslash = "/")),
                    md5 = unname(tools::md5sum(all_paths))), "input_and_code_manifest")
bwfe_require(identical(unname(input_hashes), unname(tools::md5sum(input_paths))), "An input or CS DID output changed.")
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))
message("Complete: ", output_dir)
print(coefficients[age_window %in% c("4-6", "7-9", "4-9"),
                   .(specification, age_window, estimate, se, conf_low, conf_high)])
