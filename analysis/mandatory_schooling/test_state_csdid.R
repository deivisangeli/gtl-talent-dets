# State-level C&S checks: county-to-state aggregation (annual and five-year),
# the state treatment merge, and the shared estimator recovering a known effect
# with the state as both unit and cluster. Run from the repo root.
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
source("prep/amws/period5_bins.R")
source("analysis/mandatory_schooling/annual_csdid_helpers.R")
source("analysis/mandatory_schooling/csdid_figure_helpers.R")
source("analysis/mandatory_schooling/period5_csdid_helpers.R")
source("analysis/mandatory_schooling/state_csdid_helpers.R")

# --- Aggregation: sums, recomputed rates, and dropped county-years ----------
county <- data.table(
  GEOID = c("01001", "01003", "01005", "02001", "01001", "01003"),
  year = c(1850L, 1850L, 1850L, 1850L, 1851L, 1851L),
  population = c(1000, 3000, 0, 500, 1100, 3100),
  county_births_estimate_year = c(40, 120, 0, 20, 44, 124),
  n_amws_1906_1955_dedup = c(1L, 2L, 5L, 0L, 0L, 1L),
  n_amws_1986 = c(0L, 1L, 0L, 1L, 2L, 0L))
county[, n_amws := n_amws_1906_1955_dedup + n_amws_1986]
agg <- aggregate_county_panel_to_state(county)
s <- agg$panel
stopifnot(nrow(s) == 3L,
          s[state_fips == "01" & year == 1850L, n_counties] == 2L,
          s[state_fips == "01" & year == 1850L, population] == 4000,
          s[state_fips == "01" & year == 1850L, n_amws] == 4L,
          # A ratio of sums, not a mean of county rates (which would be 1.25).
          isTRUE(all.equal(s[state_fips == "01" & year == 1850L, amws_per_1000_pop], 1)),
          isTRUE(all.equal(s[state_fips == "01" & year == 1850L, amws_per_1000_births], 25)),
          isTRUE(all.equal(s$log1p_n_amws, log1p(s$n_amws))),
          # The zero-population county is dropped and its five births reported.
          agg$dropped$dropped_rows == 1L, agg$dropped$dropped_amws_births == 5,
          all(s$n_years == 1L))

# --- Five-year bins: totals reconcile once weighted by n_years -------------
bins <- copy(county)[, `:=`(period = 1850L, n_years = 5L)][, year := NULL]
bins <- bins[, .(population = mean(population), county_births_estimate_year =
                   mean(county_births_estimate_year), n_amws_1906_1955_dedup =
                   sum(n_amws_1906_1955_dedup) / 5, n_amws_1986 = sum(n_amws_1986) / 5,
                 n_years = 5L), by = .(GEOID, period)]
bins[, n_amws := n_amws_1906_1955_dedup + n_amws_1986]
agg5 <- aggregate_county_panel_to_state(bins, "period")
stopifnot(abs(sum(agg5$panel$n_amws * agg5$panel$n_years) +
                sum(agg5$dropped$dropped_amws_births) - sum(county$n_amws)) < 1e-9,
          all(agg5$panel$n_years == 5L), "year" %in% names(agg5$panel))

# Inconsistent bin widths inside a state-period must not be summed.
bad <- copy(bins)[GEOID == "01003", n_years := 4L]
stopifnot(inherits(try(aggregate_county_panel_to_state(bad, "period"), silent = TRUE),
                   "try-error"))

# --- Treatment merge: id is the state, a missing law is excluded -----------
laws <- data.table(state_fips = c("01", "02"), state_abbr = c("AA", "BB"),
                   state = c("Aland", "Bland"), compulsory_law_year = c(1852L, NA))
attached <- state_attach_treatment(copy(s), laws)
stopifnot(all(attached$id == attached$state_id), all(attached$GEOID == attached$state_fips),
          attached[state_fips == "01", all(g == 1852L & event_time == year - 1852L)],
          attached[state_fips == "02", is.na(g)], all(attached$valid_outcomes))

# --- End to end: 36 states, known effect from e >= -14 ----------------------
set.seed(923L)
units <- data.table(id = 1:36, g = rep(c(1852L, 1874L, 1900L, 1918L), each = 9L))
units[, `:=`(state_id = id, GEOID = sprintf("%02d", id), state_fips = sprintf("%02d", id),
             state_abbr = paste0("S", id), state = paste0("State ", id))]
units[, effect := fifelse(g == 1852L, 2, fifelse(g == 1874L, 4, 10))]
panel <- merge(CJ(id = units$id, year = 1800:1960), units, by = "id")
panel[, `:=`(event_time = year - g, n_years = 1L, n_amws = 1,
             valid_outcomes = TRUE)]
panel[, model_y := (year - 1800) * 0.01 + effect * (event_time >= -14)]
# Donor gaps exercise the unbalanced path without moving the known means.
panel <- panel[!(g > 1880L & id %% 3 == 0 & year < 1850 & year %% 2 == 0)]
profile <- list(pre = 20L, post = 20L, counties = NA_integer_, jurisdictions = 18L)
sample <- build_annual_sample(panel, profile, check_expected = FALSE)
stopifnot(nrow(sample$targets) == 18L, setequal(sample$weights$g, c(1852L, 1874L)),
          all(sample$weights$weight == 0.5),
          identical(sort(unique(sample$audit$role)), c("later_adopter", "target")))
support <- annual_support(sample$panel, sample$weights, sample$grid)
stopifnot(all(support$status %in% c("supported", "reference")))
fitted <- fit_annual_csdid(sample$panel, biters = 199L)
stopifnot(identical(as.integer(fitted$fit$DIDparams$cluster_vector),
                    as.integer(rownames(fitted$fit$inffunc))))
estimated <- aggregate_annual_targets(fitted$fit, sample$weights, sample$grid, biters = 199L)
stopifnot(max(abs(estimated$dynamic$att - ifelse(sample$grid >= -14, 3, 0))) < 1e-8,
          estimated$dynamic[event_time == -15, att == 0 & se == 0])
for (gg in c(1852L, 1874L)) for (ee in c(-20L, -14L, 0L, 20L)) {
  stopifnot(abs(manual_annual_did(sample$panel, gg, ee) -
                  estimated$cells[g == gg & event_time == ee, att]) < 1e-8)
}

# --- Ragged window: a late cohort outruns the donor pool ------------------
# Targets 1852/1874/1895 (effects 2/4/6), donors 1910/1920. Nothing after
# 1920 - 15 = 1905 has a donor, so the 1895 cohort is identified only to +10.
units_r <- data.table(id = 1:40, g = rep(c(1852L, 1874L, 1895L, 1910L, 1920L), each = 8L))
units_r[, `:=`(state_id = id, GEOID = sprintf("%02d", id), state_fips = sprintf("%02d", id),
               state_abbr = paste0("S", id), state = paste0("State ", id))]
units_r[, effect := fcase(g == 1852L, 2, g == 1874L, 4, g == 1895L, 6, default = 10)]
panel_r <- merge(CJ(id = units_r$id, year = 1800:1960), units_r, by = "id")
panel_r[, `:=`(event_time = year - g, n_years = 1L, n_amws = 1, valid_outcomes = TRUE)]
panel_r[, model_y := (year - 1800) * 0.01 + effect * (event_time >= -14)]
last <- state_last_comparison(panel_r)
stopifnot(last == 1905L)
# With a single last-adopting state the 1896-1905 periods have one donor, which
# gives no standard error: the horizon falls back to the last period with two.
lone <- panel_r[!(g == 1920L & id > 33L)]
stopifnot(state_donor_counts(lone)[year == 1905L, donor_states] == 1L,
          state_last_comparison(lone) == 1895L)
profile_r <- list(pre = 20L, post = 20L, counties = NA_integer_, jurisdictions = 24L,
                  target_max = last)
sample_r <- build_annual_sample(panel_r, profile_r, check_expected = FALSE)
stopifnot(nrow(sample_r$targets) == 24L, setequal(sample_r$weights$g, c(1852L, 1874L, 1895L)))
support_r <- state_support(sample_r$panel, sample_r$weights, sample_r$grid, 14L, 1L, last, TRUE)
stopifnot(all(support_r$status %in% state_allowed_support),
          identical(support_r[status == "beyond_donor_horizon", sort(unique(event_time))], 11:20),
          all(support_r[status == "beyond_donor_horizon", g] == 1895L))
# Without the ragged relabel the same cells are unsupported.
stopifnot(any(!state_support(sample_r$panel, sample_r$weights, sample_r$grid, 14L, 1L, last,
                             FALSE)$status %in% c("supported", "reference")))
fit_r <- fit_annual_csdid(sample_r$panel, biters = 199L)$fit
est_r <- aggregate_annual_targets(fit_r, sample_r$weights, sample_r$grid, biters = 199L,
                                  ragged = TRUE)
cells_r <- state_mark_beyond(est_r$cells, last)
expected_r <- fcase(sample_r$grid < -14L, 0, sample_r$grid <= 10L, 4, default = 3)
stopifnot(max(abs(est_r$dynamic$att - expected_r)) < 1e-8,
          all(est_r$dynamic[event_time <= 10L, n_cohorts] == 3L),
          all(est_r$dynamic[event_time > 10L, n_cohorts] == 2L),
          all(est_r$dynamic[event_time > 10L, n_units] == 16L),
          identical(cells_r[status == "beyond_donor_horizon", sort(unique(event_time))], 11:20),
          grepl("24 states at e = 0 and 16 at e = \\+20", state_ragged_note(est_r$dynamic, 20L, last)))
# The balanced default still refuses to pool an event time a cohort misses.
est_b <- aggregate_annual_targets(fit_r, sample_r$weights, sample_r$grid, biters = 199L)
stopifnot(all(est_b$dynamic[event_time > 10L, status] == "incomplete_cohort_support"),
          !"n_cohorts" %in% names(est_b$dynamic))

# --- Figures: the state header renders and the county default still does ---
counts <- csdid_figure_counts(sample$targets, sample$audit, sample$weights)
coefs <- copy(estimated$dynamic)[, `:=`(outcome_name = "n_amws", specification_label = "Levels")]
out <- tempfile("state_fig"); dir.create(out)
plot_csdid_results(coefs, out, 20L, counts, outcomes = c(n_amws = "AMWS"), unit = "state")
plot_csdid_results(coefs, out, 20L, counts, outcomes = c(n_amws = "AMWS"))
stopifnot(all(file.exists(file.path(out, c("pooled_n_amws.png", "pooled_n_amws.pdf")))))
unlink(out, recursive = TRUE)
cat("State C&S aggregation, treatment, estimator and figure checks passed.\n")
