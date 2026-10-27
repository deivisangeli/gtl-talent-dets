# C&S DiD with pair-balanced comparisons within the exact FE590 county roster.
suppressPackageStartupMessages({library(data.table); library(did); library(fixest); library(ggplot2)})
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(arg)) normalizePath(file.path(dirname(sub("^--file=", "", arg[1])), "../.."), winslash = "/") else
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/")
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "analysis/boll_weevil/paired_csdid_helpers.R"))
source(file.path(repo_root, "analysis/boll_weevil/twfe_helpers.R"))
output_dir <- file.path(TALENT_DETS_DATA_DIR, "results/boll_weevil/amws_county_csdid_fe_counties")
cs_dir <- file.path(TALENT_DETS_DATA_DIR, "results/boll_weevil/amws_county_csdid")
fe_dir <- file.path(TALENT_DETS_DATA_DIR, "results/boll_weevil/amws_county_twfe")
dir.create(file.path(output_dir, "models"), recursive = TRUE, showWarnings = FALSE)
save_csv <- function(x, name) fwrite(x, file.path(output_dir, paste0(name, ".csv")), na = "NA")
input_paths <- c(roster = file.path(fe_dir, "counties_broader_590.csv"),
  treatment = file.path(TALENT_DETS_DATA_DIR, "output/boll_weevil/boll_weevil_counties_1920.csv"),
  panel = file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv"),
  old_cs = file.path(cs_dir, "pooled_age_window_coefficients.csv"),
  old_fe = file.path(fe_dir, "age_window_coefficients.csv"))
bwp_assert(all(file.exists(input_paths)), "Missing inputs or completed FE/CS reference results.")
input_hashes <- tools::md5sum(input_paths)
protected_paths <- sort(c(list.files(cs_dir, recursive = TRUE, full.names = TRUE),
                          list.files(fe_dir, recursive = TRUE, full.names = TRUE)))
protected_hashes <- tools::md5sum(protected_paths)
roster <- fread(input_paths["roster"], colClasses = c(GEOID = "character"))
treatment <- fread(input_paths["treatment"], colClasses = c(GEOID = "character"))
bwp_assert(nrow(roster) == 590L && !anyDuplicated(roster$id) && !anyDuplicated(roster$GEOID),
           "The broader FE roster is not exactly 590 unique counties.")
check <- treatment[match(roster$GEOID, GEOID)]
bwp_assert(all(check$eligible_baker_2018_rules & check$geoid_verified) &&
  identical(as.integer(check$bw_average_year), as.integer(roster$infestation_year)),
  "FE roster disagrees with source treatment eligibility or dates.")
setorder(roster, id)
roster[, `:=`(g = infestation_year - 18L, state_id = as.integer(substr(GEOID, 1, 2)),
              target = infestation_year <= 1907L)]
bwp_assert(sum(roster$target) == 133L, "The supported target roster changed.")
groups <- sort(unique(roster[target == TRUE, infestation_year]))
ages <- 30:4
years <- seq.int(min(groups) - max(ages), max(groups) - min(ages))
panel <- fread(input_paths["panel"], colClasses = c(GEOID = "character"),
  select = c("GEOID", "year", "n_amws", "n_amws_1906_1955_dedup", "n_amws_1986",
             "county_births_estimate_year", "amws_per_1000_births"))
bwp_assert(!anyDuplicated(panel[, .(GEOID, year)]), "Duplicate county-birth-year key.")
panel <- panel[GEOID %in% roster$GEOID & year %in% years]
bwp_assert(all(panel$n_amws == panel$n_amws_1906_1955_dedup + panel$n_amws_1986),
           "AMWS edition components do not reconcile.")
panel[, valid := is.finite(amws_per_1000_births) & is.finite(county_births_estimate_year) &
  county_births_estimate_year > 0 & is.finite(n_amws) & n_amws >= 0 & n_amws == floor(n_amws)]
availability <- panel[, .(observed_years = .N, valid_years = sum(valid),
                          first_valid_birth_year = min(year[valid]), last_valid_birth_year = max(year[valid])), by = GEOID]
panel <- merge(panel[valid == TRUE], roster, by = "GEOID", sort = FALSE)
panel[, `:=`(model_y = amws_per_1000_births, age_at_infestation = infestation_year - year)]
panel[, valid := NULL]
rate_error <- max(abs(panel$model_y - 1000 * panel$n_amws / panel$county_births_estimate_year))
bwp_assert(rate_error < 1e-9, "Stored rates disagree with input numerator and denominator.")
wide <- dcast(panel, id ~ year, value.var = "model_y", fill = NA_real_)
bwp_assert(setequal(wide$id, roster$id), "A FE roster county has no valid calendar-panel observations.")
outcomes <- as.matrix(wide[match(roster$id, id), -1L, with = FALSE])
rownames(outcomes) <- as.character(roster$id)
save_csv(panel, "available_county_birth_year_panel")
save_csv(availability, "calendar_coverage")

grid <- CJ(infestation_year = groups, age_at_infestation = ages)
grid[, comparison_id := .I]
influence <- matrix(0, nrow(roster), nrow(grid),
  dimnames = list(as.character(roster$id), as.character(grid$comparison_id)))
rows <- list(); memberships <- list(); warnings <- list()
fit_roles <- integer(nrow(roster))
for (G in groups) {
  group_fits <- list()
  for (k in which(grid$infestation_year == G)) {
    age <- grid$age_at_infestation[k]
    pair <- bwp_pair(roster, outcomes, G, age)
    influence[, k] <- pair$influence
    if (age != 19L) fit_roles <- fit_roles + as.integer(pair$selected)
    rows[[k]] <- data.table(comparison_id = k, infestation_year = G, g = G - 18L,
      age_at_infestation = age, event_time = 18L - age, base_year = G - 19L,
      year = G - age, estimate = pair$estimate, direct_did = pair$manual,
      direct_did_error = abs(pair$estimate - pair$manual),
      maximum_cell_influence_error = max(abs(pair$influence - pair$direct_if)),
      treated_counties = sum(pair$treated), control_counties = sum(pair$donor),
      control_states = uniqueN(roster$state_id[pair$donor]), paired_counties = sum(pair$selected),
      orientation_sign = pair$orientation_sign, status = ifelse(age == 19L, "reference", "estimated"))
    member <- roster[pair$selected, .(id, GEOID)]
    member[, `:=`(comparison_id = k, infestation_year = G, age_at_infestation = age,
                   role = ifelse(id %in% roster$id[pair$treated], "target", "paired_control"))]
    memberships[[k]] <- member
    if (length(pair$warnings)) warnings[[length(warnings) + 1L]] <-
      data.table(stage = "two_period_att_gt", infestation_year = G, age_at_infestation = age,
                 message = pair$warnings)
    group_fits[[as.character(age)]] <- pair$fit
  }
  saveRDS(list(infestation_year = G, fits_by_age = group_fits, input_hashes = input_hashes),
          file.path(output_dir, "models", paste0("group_", G, ".rds")), compress = "gzip")
  message("Completed infestation group ", G, " (", match(G, groups), "/", length(groups), ").")
}
cells <- rbindlist(rows)
bwp_assert(nrow(cells) == 405L && min(cells$control_counties) == 35L, "Target comparison support changed.")
roster[, `:=`(nonreference_comparisons = fit_roles, contributes = fit_roles > 0L)]
roster[, role := fifelse(target, "supported_target", fifelse(contributes, "later_infested_control", "not_contributing"))]
roster[, noncontribution_reason := fifelse(contributes, "",
  "no paired observations at any supported target baseline and outcome year")]
roster <- merge(roster, availability, by = "GEOID", all.x = TRUE, sort = FALSE)
setorder(roster, id)
bwp_assert(sum(roster$contributes) == 588L && sum(!roster$contributes) == 2L, "Contributing county count changed.")
save_csv(roster, "county_roster_and_roles")
save_csv(roster[contributes == FALSE], "noncontributing_counties")
save_csv(roster[target == TRUE], "treated_counties")
save_csv(roster[, .(counties = .N), by = .(role, statename)], "state_composition")
save_csv(cells, "group_age_coefficients_and_validation")
save_csv(cells[, .(comparison_id, infestation_year, g, age_at_infestation, base_year, year,
                   treated_counties, control_counties, paired_counties, control_states, status)], "comparison_support")
save_csv(rbindlist(memberships), "comparison_membership")

message("Aggregating 133 targets and applying 1,000 shared county multipliers.")
agg <- bwp_aggregate(cells, influence, roster, groups)
boot <- bwp_bootstrap(agg$annual_influence, agg$pooled_influence)
# A compact, exact record of the shared two-point multiplier draws lets an
# independent implementation reconstruct every interval without reading RDS.
high_draws <- matrix(as.integer(boot$multipliers > 0), nrow = boot$biters,
                     dimnames = list(NULL, as.character(roster$id)))
fwrite(as.data.table(high_draws), file.path(output_dir, "county_mammen_high_draws.csv.gz"))
annual <- agg$annual
annual[, `:=`(se = unname(boot$annual_se), reference_age = 19L, roster_counties = 590L,
               contributing_counties = 588L, simultaneous_critical_value = boot$critical)]
annual[, `:=`(conf_low = estimate - qnorm(0.975) * se, conf_high = estimate + qnorm(0.975) * se,
  simultaneous_low = estimate - boot$critical * se, simultaneous_high = estimate + boot$critical * se,
  status = ifelse(age_at_infestation == 19L, "reference", "estimated"))]
pooled <- agg$pooled
pooled[, `:=`(se = unname(boot$pooled_se), bootstrap_iterations = boot$biters, seed = boot$seed,
               roster_counties = 590L, contributing_counties = 588L)]
pooled[, `:=`(conf_low = estimate - qnorm(0.975) * se, conf_high = estimate + qnorm(0.975) * se)]
save_csv(annual, "event_study_coefficients")
save_csv(pooled, "pooled_age_window_coefficients")
save_csv(agg$group_weights, "target_cohort_weights")
save_csv(agg$validation, "pooled_weight_and_influence_validation")
weight_export <- merge(cells[, .(comparison_id, infestation_year, age_at_infestation)],
  agg$group_weights, by = "infestation_year", sort = FALSE)
for (w in list(c(4L,6L), c(7L,9L), c(4L,9L))) {
  column <- paste0("pooled_weight_", w[1], "_", w[2])
  weight_export[, (column) := ifelse(age_at_infestation %in% seq(w[1],w[2]), weight / (w[2]-w[1]+1L), 0)]
}
save_csv(weight_export, "aggregation_weights")
names_if <- c(paste0("age_", annual$age_at_infestation), paste0("window_", pooled$age_window))
dimnames(boot$covariance) <- list(names_if, names_if)
save_csv(as.data.table(boot$covariance, keep.rownames = "effect"), "joint_analytic_covariance")
saveRDS(list(county_ids = roster$id, cells = cells, cell_influence = influence,
  annual_influence = agg$annual_influence, pooled_influence = agg$pooled_influence,
  group_weights = agg$group_weights, bootstrap = boot, input_hashes = input_hashes),
  file.path(output_dir, "models/aggregation_influence_and_bootstrap.rds"), compress = "gzip")
pre <- copy(annual[age_at_infestation >= 20L])
pre[, simultaneous_band_excludes_zero := simultaneous_low > 0 | simultaneous_high < 0]
save_csv(pre, "pre_exposure_diagnostics")
raw <- panel[target == TRUE & age_at_infestation %in% c(4:9,19L),
  .(county_age_observations = .N, scientists = sum(n_amws), early_editions = sum(n_amws_1906_1955_dedup),
    edition_1986 = sum(n_amws_1986), estimated_births = sum(county_births_estimate_year),
    mean_county_rate = mean(model_y)), by = .(age_window = ifelse(age_at_infestation == 19L, "19_reference", "4-9"))]
save_csv(raw, "outcome_descriptives")

message("Fitting the supporting fixed-effects regression on the same 133 targets.")
fe_data <- copy(panel[target == TRUE & age_at_infestation %in% 4:30])
bwp_assert(nrow(fe_data) == 3591L && all(fe_data[, .N, by = id]$N == 27L), "Incomplete FE133 age window.")
fe_data <- cbind(fe_data, bwfe_exposure_bins(fe_data$age_at_infestation))
fe <- feols(model_y ~ exposure_4_6 + exposure_7_9 + exposure_10_12 + exposure_13_15 + exposure_16_18 | id + year,
  data = fe_data, vcov = ~id, fixef.tol = 1e-10,
  ssc = ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, G.df = "min", t.df = "min"))
fe_vcov <- vcov(fe, attr = TRUE)
fe_results <- bwfe_age_contrasts(coef(fe), fe_vcov, attr(fe_vcov, "df.t"))
fe_results[, `:=`(counties = 133L, observations = nrow(fe_data), specification = "FE133_reference19_30")]
fe_validation <- bwfe_fwl(fe_data$model_y, as.matrix(fe_data[, names(coef(fe)), with = FALSE]), fe_data$id, fe_data$year)
fe_unadjusted <- vcov(fe, vcov = ~id, ssc = ssc(K.adj = FALSE, G.adj = FALSE))
fe_checks <- data.table(coefficient_error = max(abs(coef(fe) - fe_validation$beta[names(coef(fe))])),
  unadjusted_covariance_error = max(abs(fe_unadjusted - fe_validation$covariance_unadjusted)),
  residual_error = max(abs(resid(fe) - fe_validation$residual)))
bwp_assert(max(unlist(fe_checks)) < 1e-8, "Supporting FE independent validation failed.")
save_csv(fe_data, "supporting_fe133_panel")
save_csv(fe_results, "supporting_fe133_coefficients")
save_csv(fe_checks, "supporting_fe133_validation")
fe_vcov_plain <- matrix(as.numeric(fe_vcov), nrow = nrow(fe_vcov), dimnames = dimnames(fe_vcov))
save_csv(as.data.table(fe_vcov_plain, keep.rownames = "term"), "supporting_fe133_covariance")
saveRDS(list(fit = fe, covariance = fe_vcov, independent_fwl = fe_validation, input_hashes = input_hashes),
        file.path(output_dir, "models/supporting_fe133.rds"), compress = "gzip")
old_cs <- fread(input_paths["old_cs"])[, .(age_window, estimate = att, se, conf_low, conf_high,
  specification = "original_CS86", target_counties = target_counties, reference_ages = "19")]
old_fe <- fread(input_paths["old_fe"])[specification == "broader_590" & age_window %in% c("4-6","7-9","4-9"),
  .(age_window, estimate, se, conf_low, conf_high, specification = "existing_FE590",
    target_counties = counties, reference_ages)]
comparison <- rbindlist(list(old_cs, old_fe,
  pooled[, .(age_window, estimate, se, conf_low, conf_high, specification = "paired_CS133_FE590_roster",
             target_counties, reference_ages = "19")],
  fe_results[age_window %in% c("4-6","7-9","4-9"), .(age_window, estimate, se, conf_low, conf_high,
    specification, target_counties = counties, reference_ages)]), use.names = TRUE)
save_csv(comparison, "comparison_with_prior_CS_and_FE")

covariance_check <- max(abs(boot$draws[, ncol(agg$annual_influence) + 3L] -
  rowMeans(boot$draws[, match(as.character(4:9), colnames(agg$annual_influence)), drop = FALSE])))
validation <- data.table(check = c("FE_roster_counties", "contributing_counties", "supported_targets",
  "group_age_cells", "minimum_paired_controls", "stored_rate_error", "direct_did_error",
  "closed_form_influence_error", "pooled_influence_error", "shared_bootstrap_average_error",
  "empirical_simultaneous_coverage_at_least_95pct", "FE_coefficient_error", "FE_covariance_error"),
  observed = c(nrow(roster), sum(roster$contributes), sum(roster$target), nrow(cells), min(cells$control_counties),
    rate_error, max(cells$direct_did_error), max(cells$maximum_cell_influence_error),
    max(agg$validation$maximum_influence_error), covariance_check,
    as.integer(boot$empirical_simultaneous_coverage >= 0.95), fe_checks$coefficient_error, fe_checks$unadjusted_covariance_error),
  expected = c(590,588,133,405,35,0,0,0,0,0,1,0,0),
  tolerance = c(rep(0,5),1e-8,1e-8,1e-7,1e-8,1e-8,0,1e-8,1e-8))
validation[, passed := abs(observed - expected) <= tolerance]
save_csv(validation, "validation_checks")
bwp_assert(all(validation$passed), "Final numerical validation failed.")
summary <- data.table(roster_counties = 590L, contributing_counties = 588L,
  supported_target_counties = 133L, supported_target_groups = length(groups),
  first_target_infestation = min(groups), last_target_infestation = max(groups),
  target_states = uniqueN(roster[target == TRUE, state_id]), first_available_birth_year = min(years),
  last_available_birth_year = max(years), observed_calendar_rows = nrow(panel),
  age4_to9_target_county_cohorts = 798L, minimum_paired_controls = min(cells$control_counties),
  min_age = 4L, max_age = 30L, reference_age = 19L, bootstrap_iterations = boot$biters, seed = boot$seed,
  raw_simultaneous_critical = boot$raw_critical, used_simultaneous_critical = boot$critical,
  empirical_simultaneous_coverage = boot$empirical_simultaneous_coverage)
save_csv(summary, "sample_summary")
save_csv(if (length(warnings)) rbindlist(warnings) else data.table(stage = "all", message = "none"), "estimation_warnings")

event_figure <- ggplot(annual, aes(age_at_infestation, estimate)) +
  annotate("rect", xmin = 4, xmax = 9, ymin = -Inf, ymax = Inf, fill = "#DCEEF7", alpha = .8) +
  geom_hline(yintercept = 0, colour = "grey45") + geom_vline(xintercept = 19, linetype = "dashed", colour = "grey45") +
  geom_ribbon(aes(ymin = simultaneous_low, ymax = simultaneous_high), fill = "#245B78", alpha = .18) +
  geom_line(colour = "#245B78", linewidth = .6) + geom_point(colour = "#245B78", size = 1.7) +
  scale_x_reverse(breaks = c(30,25,20,19,15,10,7,4)) +
  labs(title = "Boll weevil: CS DID within the FE county roster",
    subtitle = "590 counties on roster; 588 contribute; effects cover 133 early-infested target counties",
    x = "Age at infestation (birth county proxies schooling location)",
    y = "Effect on AMWS scientists born\nper 1,000 estimated births",
    caption = "Pair-balanced comparisons; reference age 19. Shading: ages 4-9. Ribbon: 95% simultaneous bands.\n1,000 shared county-level Mammen multiplier draws; equal target-county and annual-age weights.") +
  theme_minimal(base_size = 12) + theme(plot.caption = element_text(hjust = 0), panel.grid.minor = element_blank())
ggsave(file.path(output_dir,"event_study_age_at_infestation.png"), event_figure,
       width = 11, height = 6.5, dpi = 220, bg = "white")
forest_data <- copy(comparison[specification %in% c("paired_CS133_FE590_roster","FE133_reference19_30")])
forest_data[, specification := factor(specification,
  levels = c("paired_CS133_FE590_roster","FE133_reference19_30"),
  labels = c("Paired CS DID: reference age 19", "County/year FE: reference ages 19-30"))]
forest_data[, age_window := factor(age_window, levels = c("4-9","7-9","4-6"))]
forest <- ggplot(forest_data, aes(age_window,estimate,colour=specification)) +
  geom_hline(yintercept=0,colour="grey50",linetype="dashed") +
  geom_errorbar(aes(ymin=conf_low,ymax=conf_high),width=.15,position=position_dodge(width=.4)) +
  geom_point(size=2.5,position=position_dodge(width=.4)) + coord_flip() +
  scale_colour_manual(values=c("#245B78","#BA642F")) +
  labs(title="Same 133 target counties: CS DID and fixed effects",
    subtitle="CS comparisons are drawn from the 590-county FE roster",
    x="Age at infestation", y="Effect on AMWS scientists born per 1,000 estimated births",colour=NULL,
    caption="95% pointwise intervals. Reference cohorts and comparison structures still differ between estimators.") +
  theme_minimal(base_size=12) + theme(legend.position="bottom",plot.caption=element_text(hjust=0))
ggsave(file.path(output_dir,"pooled_comparison_same133.png"),forest,width=11,height=6,dpi=220,bg="white")

report_rows <- vapply(c("4-6","7-9","4-9"),function(w) {
  c <- pooled[age_window==w]; f <- fe_results[age_window==w]
  sprintf("| %s | %.4f [%.4f, %.4f] | %.4f [%.4f, %.4f] |",w,c$estimate,c$conf_low,c$conf_high,
          f$estimate,f$conf_low,f$conf_high)
},character(1))
noncontributors <- paste(roster[contributes==FALSE,paste0(countyname,", ",statename," (",GEOID,")")],collapse="; ")
raw_ref <- raw[age_window=="19_reference"];raw_target <- raw[age_window=="4-9"]
context_rows <- vapply(c("original_CS86","existing_FE590"),function(spec) {
  z<-comparison[specification==spec & age_window=="4-9"]
  sprintf("- %s: %.4f [%.4f, %.4f]; %d target counties; reference ages %s.",spec,z$estimate,z$conf_low,z$conf_high,
          z$target_counties,z$reference_ages)
},character(1))
writeLines(c(
  "# CS DID within the 590-county FE roster", "", "## Estimates for the supported target counties", "",
  "Effects are AMWS scientists born per 1,000 estimated births. Brackets are 95% confidence intervals.", "",
  "| Age at infestation | Paired CS DID, 133 targets | County/year FE, same 133 targets |",
  "| --- | --- | --- |", report_rows, "",
  "The county roster exactly matches the broader FE model, but a full ages-4-9 CS effect cannot be identified for all 590 counties using only later-infested controls. The CS estimates cover 133 counties with midpoint infestation in 1893-1907. The other counties are candidates for comparison while their own cohorts remain unexposed.", "",
  "## County coverage and identification", "",
  sprintf("All 590 counties remain in the roster; 588 contribute to at least one nonreference comparison. The two non-contributing counties are %s. Their observations start too late for the required target baselines; their influence rows are zero rather than treating them as observed or inventing earlier outcomes.",noncontributors),
  "Each comparison requires the same counties to have observed outcomes in both periods. Controls must have first exposed birth cohort strictly after both periods. There are at least 35 paired controls per requested comparison. No new counties or never-infested counties were added. Missing observations are never imputed as zero.",
  "Let G be midpoint infestation year and b birth year. The first exposed birth cohort is g=G-18, age at infestation is G-b, and event time is b-g. The reference is b=G-19 (age 19). Ages 4-9 are event times +9 to +14. Ages 10-18 can be affected and cannot supply untreated comparisons. Ages 20-30 provide pre-exposure diagnostics. This diagnostic window is shorter than the original age-40 window so all 133 targets contribute at every reported age.",
  "The original AMWS panel supplies additional older birth cohorts needed for controls. Reusing only the FE regression's own ages-4-30 observations would leave no valid age-four comparison pairs. Nominal county identifiers do not harmonize historical boundaries, and birth county proxies schooling location.", "",
  "## Estimator and inference", "",
  "Every nonreference cell uses public did::att_gt on a two-period, intercept-only balanced panel. Eligibility is determined from actual infestation timing before fitting. The pair's controls are coded zero only for that comparison; this does not label them historically never treated. Pre-reference comparisons reverse both the returned estimate and its influence function so all cells measure outcome-year minus age-19-reference changes.",
  "Each cell's county influence function is scaled by 590 divided by its paired sample size and aligned to the common roster. Annual effects weight the 133 target counties equally and include estimated group-share weight terms. Pooled effects weight each annual age equally. The same 1,000 county-level Mammen multipliers (seed 20261025) are applied jointly to annual and pooled influence functions. Standard errors use the bootstrap interquartile scale, as in did; pooled intervals use the normal 1.96 critical value. Annual bands use the joint supremum statistic across 26 nonreference ages, conservatively floored at 1.96 if necessary. The reference age has exactly zero estimate and variance.",
  "The supporting fixed-effects regression uses only the same 133 target counties, all ages 4-30, county and birth-year fixed effects, Baker's five three-year exposure bins, and omitted ages 19-30. Its t intervals use county-clustered, small-sample-adjusted covariance. Thus target geography is aligned, but its reference and comparison structure still differ from CS DID.", "",
  "Causal interpretation still requires comparable untreated cohort trends for retained target and control counties and an unaffected age-19 comparison cohort for scientist generation. Pairing and matching the county roster do not establish those assumptions, and pair selection can affect the comparison population.", "",
  sprintf("The age-19 reference contains %d scientist births across %d county-cohorts; ages 4-9 contain %d across %d county-cohorts. County births are estimated denominators, not observed local births. Wide intervals or failure to reject do not establish a zero effect.",raw_ref$scientists,raw_ref$county_age_observations,raw_target$scientists,raw_target$county_age_observations), "",
  sprintf("![Event study](%s)",normalizePath(file.path(output_dir,"event_study_age_at_infestation.png"),winslash="/")), "",
  sprintf("![Same-target comparison](%s)",normalizePath(file.path(output_dir,"pooled_comparison_same133.png"),winslash="/")), "",
  "## Prior results for context", "",context_rows, "",
  "These have different target populations and/or reference cohorts; neither is an identical-population estimate of the new 133-county effect. No causal conclusion follows from their numerical differences alone.", "",
  "## Validation and files", "",
  "All 405 group-age cells, including normalized references, are verified against direct paired DiD. Embedded public influence functions match closed-form paired influences; pooled weights and group-share terms are independently reconstructed. Shared bootstrap draws preserve covariance. The supporting FE regression is checked against independent two-way FWL coefficients, residuals, and clustered covariance. Small-group and structural two-period Wald-test warnings are recorded, not reported as passed pretrend tests.",
  "The output directory includes exact roster roles, non-contributors, observed calendar data, every comparison's membership/support, group-age and pooled coefficients, explicit weights, joint analytic covariance, and saved per-group did fits plus aggregation/influence/bootstrap objects. Input and code hashes, prior-result protection checks, and session information identify the run. The bootstrap covariance and analytic influence covariance are both retained; their finite-simulation values need not coincide."
),file.path(output_dir,"results_summary.md"))
code_paths <- file.path(repo_root,c("paths.R","analysis/boll_weevil/paired_csdid_helpers.R",
  "analysis/boll_weevil/analysis_boll_weevil_amws_csdid_fe_counties.R","analysis/boll_weevil/twfe_helpers.R"))
manifest_paths <- c(input_paths,code_paths)
save_csv(data.table(path=unname(normalizePath(manifest_paths,winslash="/")),
  md5=unname(tools::md5sum(manifest_paths))),"input_and_code_manifest")
prior_check <- data.table(path=unname(normalizePath(protected_paths,winslash="/")),
  md5_before=unname(protected_hashes),md5_after=unname(tools::md5sum(protected_paths)))
prior_check[,unchanged:=md5_before==md5_after]
save_csv(prior_check,"prior_results_protection")
bwp_assert(all(prior_check$unchanged) && identical(unname(input_hashes),unname(tools::md5sum(input_paths))),
           "A source input or prior result changed.")
writeLines(capture.output(sessionInfo()),file.path(output_dir,"session_info.txt"))
message("Complete. Results: ",output_dir)
print(pooled[,.(age_window,estimate,se,conf_low,conf_high)])
print(fe_results[age_window %in% c("4-6","7-9","4-9"),.(age_window,estimate,se,conf_low,conf_high)])
