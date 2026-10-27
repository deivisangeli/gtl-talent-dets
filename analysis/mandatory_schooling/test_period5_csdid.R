# Five-year-bin checks: the bin rules themselves, annual-average aggregation,
# the cadence mapping onto did's base period and donor eligibility, and the
# guarantee that step = 1 still reproduces the annual design exactly.
suppressPackageStartupMessages({library(data.table); library(did)})
source("prep/amws/period5_bins.R")
source("analysis/mandatory_schooling/annual_csdid_helpers.R")
source("analysis/mandatory_schooling/csdid_figure_helpers.R")
source("analysis/mandatory_schooling/period5_csdid_helpers.R")

# --- 1. Bin rules ------------------------------------------------------------
stopifnot(
  all(period5_bin(1850:1854, "standard") == 1850L),
  all(period5_bin(1855:1859, "standard") == 1855L),
  all(period5_bin(c(1850L, 1851L, 1852L), "alt") == 1850L),
  all(period5_bin(c(1853L, 1854L, 1855L, 1856L, 1857L), "alt") == 1855L),
  is.na(period5_bin(NA_integer_, "standard")),
  is.na(period5_bin(NA_integer_, "alt")))
# `alt` bins are centred on their label; `standard` bins lead it.
stopifnot(all(range(which(period5_bin(1800:1900, "alt") == 1850L)) ==
              which(1800:1900 %in% c(1848L, 1852L))),
          all(range(which(period5_bin(1800:1900, "standard") == 1850L)) ==
              which(1800:1900 %in% c(1850L, 1854L))))
# The conventions differ on exactly the years ending 3, 4, 8 and 9.
differ <- 1800:1960
stopifnot(identical(sort(unique(differ[period5_bin(differ, "standard") !=
                                       period5_bin(differ, "alt")] %% 10L)),
                    c(3L, 4L, 8L, 9L)))
stopifnot(identical(period5_suffix("standard"), ""), identical(period5_suffix("alt"), "_alt"))

# --- 2. Annual averages preserve totals --------------------------------------
# Aggregation divides bin sums by the contributing years, so the original total
# is recovered only after reweighting each bin by n_years.
set.seed(20260916L)
raw <- data.table(GEOID = "01001", year = 1800:1959, n = rpois(160L, 3))
for (convention in period5_conventions) {
  binned <- raw[, .(n_years = .N, n_mean = sum(n) / .N),
                by = .(period = period5_bin(year, convention))]
  stopifnot(abs(sum(binned$n_mean * binned$n_years) - sum(raw$n)) < 1e-9)
  complete <- binned[n_years == 5L]
  if (convention == "standard") {
    stopifnot(nrow(complete) == nrow(binned))
  } else {
    # Only the two edge bins are partial under the centred convention.
    stopifnot(nrow(binned) - nrow(complete) == 2L,
              all(binned[n_years < 5L, n_years] < 5L))
  }
}

# --- 3. Cadence: reference snaps to the grid; step 1 is unchanged ------------
annual <- csdid_cadence()
five <- period5_cadence()
stopifnot(annual$ref == -15L, five$ref == -15L, five$step == 5L,
          five$anticipation == annual$anticipation,
          csdid_ref_event(14L, 1L) == -15L, csdid_ref_event(14L, 5L) == -15L,
          csdid_ref_event(14L, 10L) == -20L)
stopifnot(identical(csdid_profile_specifications(period5_profiles$pre20), "levels"),
          identical(csdid_profile_specifications(period5_profiles$pre40),
                    c("levels", "detrended")),
          csdid_specification_count(period5_profiles$pre20) == 6L,
          csdid_specification_count(period5_profiles$pre40) == 12L)

# --- 3b. Horizons and the 1905 comparison ceiling ----------------------------
# post20 is already published, so its directory suffix and seed base are pinned
# here: renumbering either would silently orphan or overwrite those results.
stopifnot(period5_cadence("post20")$dir_suffix == "_post20_ref15",
          period5_cadence("post20")$seed_base == 20260916L,
          period5_cadence("post30")$dir_suffix == "_post30_ref15",
          period5_cadence("post30")$seed_base != period5_cadence("post20")$seed_base,
          period5_horizons$post20$post == 20L, period5_horizons$post20$target_max == 1880L,
          period5_horizons$post30$post == 30L, period5_horizons$post30$target_max == 1875L)
# 1905 is the last comparison bin: every state had a law by 1920 and a donor
# must start at least 15 years later, so a cohort g reaches at most 1905 - g.
# Every horizon must respect that bound; post30 is the case where it binds.
for (hz in names(period5_horizons)) {
  h <- period5_horizons[[hz]]
  stopifnot(h$target_max <= PERIOD5_LAST_COMPARISON_BIN - h$post)
}
stopifnot(period5_horizons$post30$target_max ==
            PERIOD5_LAST_COMPARISON_BIN - period5_horizons$post30$post)
for (cv in period5_conventions) for (hz in names(period5_horizons)) {
  pf <- period5_profiles_for(cv, hz)
  stopifnot(pf$pre20$post == period5_horizons[[hz]]$post,
            pf$pre40$target_max == period5_horizons[[hz]]$target_max,
            identical(csdid_profile_specifications(pf$pre20), "levels"),
            csdid_specification_count(pf$pre40) == 12L)
}

# --- 3c. The horizon cap turns a late cohort into a donor, not an exclusion --
cap_units <- data.table(id = 1:40, g = rep(c(1850L, 1875L, 1880L, 1910L), each = 10L))
cap_units[, `:=`(state_id = rep(1:20, each = 2L), state_abbr = paste0("C", rep(1:20, each = 2L)),
                 state = paste0("C", rep(1:20, each = 2L)), state_fips = sprintf("%02d", rep(1:20, each = 2L)),
                 GEOID = sprintf("%05d", 1:40))]
cap_panel <- merge(CJ(id = cap_units$id, year = seq(1800L, 1955L, by = 5L)), cap_units, by = "id")
cap_panel[, `:=`(event_time = year - g, valid_outcomes = TRUE, n_amws = 1)]
role_of <- function(target_max) {
  pr <- list(pre = 20L, post = 20L, specifications = "levels",
             counties = NA_integer_, jurisdictions = NA_integer_)
  if (!is.null(target_max)) pr$target_max <- target_max
  s <- build_annual_sample(cap_panel, pr, anticipation = five$anticipation, step = five$step)
  list(roles = s$audit[, .(role = role[1L]), by = g][order(g)],
       donors = sort(unique(s$panel[target == FALSE, g])))
}
default_cap <- role_of(NULL)
capped <- role_of(1875L)
# Absent field: unchanged behaviour, 1880 is still a target cohort.
stopifnot(default_cap$roles[g == 1880L, role] == "target",
          !(1880L %in% default_cap$donors))
# Capped: 1880 is reclassified as an eligible donor, NOT dropped, and the
# surviving target cohorts are unchanged.
stopifnot(capped$roles[g == 1880L, role] == "later_adopter",
          1880L %in% capped$donors,
          capped$roles[g %in% c(1850L, 1875L), all(role == "target")],
          identical(default_cap$roles[g %in% c(1850L, 1875L), role],
                    capped$roles[g %in% c(1850L, 1875L), role]))

# --- 4. Estimator on a synthetic five-year panel ------------------------------
# Law years are deliberately off the grid so the binning of g is exercised.
units <- data.table(id = 1:48, law_year = rep(c(1851L, 1874L, 1901L, 1918L), each = 12L))
units[, state_id := rep(1:12, each = 4L)]
units[, state_abbr := paste0("S", state_id)]
units[, g := period5_bin(law_year, "standard")]
stopifnot(identical(sort(unique(units$g)), c(1850L, 1870L, 1900L, 1915L)))
units[, effect := fifelse(g == 1850L, 2, fifelse(g == 1870L, 4, 10))]
panel <- merge(CJ(id = units$id, year = seq(1800L, 1960L, by = 5L)), units, by = "id")
panel[, event_time := year - g]
# A constant jump from the first potentially exposed bin onwards. A base period
# placed anywhere inside the exposed window would difference the jump away and
# drive the post-treatment estimates to zero, which the assertion below catches.
panel[, model_y := (year - 1800) * 0.01 + effect * (event_time >= five$ref + five$step)]
# Ragged donor coverage exercises the unbalanced-panel path without changing
# any mean, because control outcomes are equal within a bin.
panel <- panel[!(id > 24 & id %% 3 == 0 & year < 1850 & (year %/% 5L) %% 2L == 0L)]

weights <- units[g <= 1880, .(n_counties = .N, n_jurisdictions = uniqueN(state_id)), by = g]
setorder(weights, g)
weights[, weight := n_counties / sum(n_counties)]
grid <- seq.int(-20L, 20L, by = five$step)
support <- annual_support(panel, weights, grid, five$anticipation, five$step)
stopifnot(all(support$status %in% c("supported", "reference")))
# A donor law bin must start at least 15 years after the comparison bin, which
# is the annual rule expressed on this grid.
stopifnot(support[g == 1870 & event_time == 20, minimum_control_law_year] == 1905L,
          support[g == 1870 & event_time == -20, minimum_control_law_year] == 1870L)

fitted <- fit_annual_csdid(panel, five$anticipation, biters = 199L)
estimated <- aggregate_annual_targets(fitted$fit, weights, grid, five$anticipation,
                                      biters = 199L, step = five$step)
expected <- 3 * (grid >= five$ref + five$step)
stopifnot(max(abs(estimated$dynamic$att - expected)) < 1e-8,
          estimated$dynamic[event_time == five$ref, att] == 0,
          estimated$dynamic[event_time == 0, se] > 0,
          all(estimated$cells$g %in% c(1850, 1870)))

# The independent 2x2 DiD must reproduce did cell by cell; this is what pins
# the base-period and donor-eligibility mapping to the five-year grid.
manual <- CJ(g = weights$g, event_time = grid)
manual[, manual_att := mapply(function(gg, ee)
  manual_annual_did(panel, gg, ee, five$anticipation, five$step), g, event_time)]
manual <- merge(manual, estimated$cells[, .(g, event_time, att)], by = c("g", "event_time"))
stopifnot(max(abs(manual$att - manual$manual_att)) < 1e-8)

# --- 5. Base-period placement, isolated --------------------------------------
# One treated cohort with a pure pre-trend, and donors late enough never to be
# targets themselves, so no treated unit ever serves as another's control. The
# ATT is then zero at exactly one event time: whichever bin did chose as its
# universal base period. This is what caught anticipation being read as a count
# of periods rather than of years.
iso_units <- data.table(id = 1:20, g = rep(c(1870L, 1950L), each = 10L))
iso_units[, `:=`(state_id = rep(1:10, each = 2L), state_abbr = paste0("T", rep(1:10, each = 2L)))]
iso <- merge(CJ(id = iso_units$id, year = seq(1800L, 1940L, by = 5L)), iso_units, by = "id")
iso[, event_time := year - g]
iso[, model_y := 0.01 * (year - 1800) + (g == 1870L) * 0.05 * event_time]
iso_fit <- fit_annual_csdid(iso, five$anticipation, biters = 199L)$fit
iso_cells <- data.table(g = iso_fit$group, e = iso_fit$t - iso_fit$group, att = iso_fit$att)
stopifnot(identical(iso_cells[g == 1870 & abs(att) < 1e-12, e], as.numeric(five$ref)))
# The annual cadence must still land on -15 under the same construction.
iso_annual <- merge(CJ(id = iso_units$id, year = 1800:1940), iso_units, by = "id")
iso_annual[, event_time := year - g]
iso_annual[, model_y := 0.01 * (year - 1800) + (g == 1870L) * 0.05 * event_time]
iso_annual_fit <- fit_annual_csdid(iso_annual, annual$anticipation, biters = 199L)$fit
iso_annual_cells <- data.table(g = iso_annual_fit$group,
                               e = iso_annual_fit$t - iso_annual_fit$group,
                               att = iso_annual_fit$att)
stopifnot(identical(iso_annual_cells[g == 1870 & abs(att) < 1e-12, e], as.numeric(annual$ref)))

# --- 6. Detrending window on the five-year grid ------------------------------
panel[, n_amws := model_y]
a <- annual_detrend(panel, "n_amws", 40L, five$anticipation, five$step)$trends
panel[event_time > five$ref, n_amws := n_amws + 1e6]
b <- annual_detrend(panel, "n_amws", 40L, five$anticipation, five$step)$trends
stopifnot(isTRUE(all.equal(a, b)), all(a$fitting_years == 6L),
          all(a$fitting_max_event == five$ref))
# pre20 leaves two unexposed bins, which the three-point guard must reject.
stopifnot(inherits(try(annual_detrend(panel, "n_amws", 20L, five$anticipation, five$step),
                       silent = TRUE), "try-error"))

cat("Five-year bin rules, annual averaging, cadence mapping, horizon cap,",
    "estimator and detrending checks passed.\n")
