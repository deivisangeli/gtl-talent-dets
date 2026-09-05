# Meaningful estimator checks: known anticipatory effects, ragged donor data,
# target-only aggregation, omitted support, and pre-exposure trend fitting.
suppressPackageStartupMessages({library(data.table); library(did)})
source("analysis/mandatory_schooling/annual_csdid_helpers.R")

set.seed(742L)
units <- data.table(id = 1:48, g = rep(c(1852L, 1874L, 1900L, 1918L), each = 12L))
units[, state_id := rep(1:12, each = 4L)]
units[, state_abbr := paste0("S", state_id)]
units[, effect := fifelse(g == 1852L, 2, fifelse(g == 1874L, 4, 10))]
panel <- merge(CJ(id = units$id, year = 1800:1960), units, by = "id")
panel[, event_time := year - g]
panel[, model_y := (year - 1800) * 0.01 + effect * (event_time >= -14)]
# These gaps cannot change the known means because control outcomes are equal
# within a year; they exercise the package's unbalanced-data inference path.
panel <- panel[!(id > 24 & id %% 3 == 0 & year < 1850 & year %% 2 == 0)]
weights <- units[g <= 1880, .(n_counties = .N, n_jurisdictions = uniqueN(state_id)), by = g]
weights[, weight := n_counties / sum(n_counties)]
grid <- -20:20
support <- annual_support(panel, weights, grid)
stopifnot(all(support$status %in% c("supported", "reference")),
          support[g == 1874 & event_time == 20, minimum_control_law_year] == 1909,
          support[g == 1874 & event_time == 20, control_year_states] == 3L)
fitted <- fit_annual_csdid(panel, biters = 199L)
estimated <- aggregate_annual_targets(fitted$fit, weights, grid, biters = 199L)
stopifnot(max(abs(estimated$dynamic$att - ifelse(grid >= -14, 3, 0))) < 1e-8,
          estimated$dynamic[event_time == -15, att] == 0,
          estimated$dynamic[event_time == 0, se] > 0,
          all(estimated$cells$g %in% c(1852, 1874)))

# Removing one target effect must not silently reweight the curve to the other
# target cohort, even though the donor cohorts have estimable effects.
missing <- fitted$fit
missing$att[missing$group == 1852 & missing$t == 1852] <- NA_real_
omitted <- aggregate_annual_targets(missing, weights, grid, biters = 199L)
stopifnot(is.na(omitted$dynamic[event_time == 0, att]),
          omitted$dynamic[event_time == 0, status] == "incomplete_cohort_support")

# Deliberately alter only potentially exposed/post-law outcomes: slopes must
# stay fixed because the trend fit is restricted to e <= -15.
panel[, n_amws := model_y]
a <- annual_detrend(panel, "n_amws", 20)$trends
panel[event_time >= -14, n_amws := n_amws + 1e6]
b <- annual_detrend(panel, "n_amws", 20)$trends
stopifnot(isTRUE(all.equal(a, b)), all(a$fitting_years == 6L),
          all(a$fitting_max_event == -15L))
cat("Annual C&S estimator, support, missing-cell, and detrending checks passed.\n")
