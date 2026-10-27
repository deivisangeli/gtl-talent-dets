# Region x birth-year C&S checks: the donor restriction IS the C&S region x
# time estimator, the ragged controlled aggregation reduces to the balanced one
# and matches did's own ragged aggregation, the donor horizon counts states,
# and a Southern target is refused. Run from the repo root.
suppressPackageStartupMessages({library(data.table); library(did)})
source("analysis/mandatory_schooling/annual_csdid_helpers.R")
source("analysis/mandatory_schooling/annual_csdid_population_controls.R")
source("analysis/mandatory_schooling/region_csdid_helpers.R")

stopifnot(length(CLS_SOUTH) == 11L, !"DC" %in% CLS_SOUTH, !"MD" %in% CLS_SOUTH)

# 12 states x 3 counties. Targets (1852, 1874) are non-Southern; both donor
# cohorts mix regions, so every comparison has donors from both.
states <- data.table(state_id = 1:12, g = rep(c(1852L, 1874L, 1900L, 1918L), each = 3L),
                     state_abbr = c("MA", "VT", "NH", "NY", "NJ", "CT",
                                    "IA", "AL", "GA", "MO", "MS", "TX"))
u <- states[rep(seq_len(.N), each = 3L)]
u[, `:=`(id = seq_len(.N), x = log(100 + seq_len(.N) * 23))]
u[, `:=`(GEOID = sprintf("%05d", id), state_fips = sprintf("%02d", state_id), state = state_abbr,
         south = as.integer(state_abbr %in% CLS_SOUTH),
         effect = fifelse(g == 1852L, 2, fifelse(g == 1874L, 4, 0)))]
p <- merge(CJ(id = u$id, year = 1800:1960), u, by = "id")
# Region-specific year shocks: a pooled comparison is biased, a within-region
# one is not.
p[, `:=`(event_time = year - g, valid_outcomes = TRUE, population_source = "nhgis",
         population = exp(x + .002 * (year - 1800)))]
p[, model_y := (year - 1800) * .01 + south * sin(year / 4) * 3 +
    effect * (event_time >= -14L) + (id %% 5) * cos(year / 9 + id) * .01]
profile <- list(pre = 20L, post = 20L, counties = NA_integer_)
full <- build_annual_sample(p, profile, check_expected = FALSE)
s <- region_restrict_sample(full)
stopifnot(nrow(s$targets) == 18L, !any(s$panel[target == FALSE, south == 1L]),
          s$audit[role == "excluded_other_region", .N] == 12L,
          s$audit[role == "later_adopter", .N] == 6L)

# (a) Region x year equivalence: C&S with a saturated South dummy on the FULL
# pool equals unconditional C&S on the non-Southern pool.
cells_of <- function(fit) {
  keep <- fit$group %in% s$weights$g & (fit$t - fit$group) %in% s$grid
  data.table(g = fit$group[keep], t = fit$t[keep], att = fit$att[keep])
}
region_fe <- fit_annual_csdid(copy(p)[year <= 1903], xformla = ~south, est_method = "reg",
                              bstrap = FALSE)$fit
restricted <- fit_annual_csdid(copy(s$panel)[year <= 1903], bstrap = FALSE)$fit
pooled_fit <- fit_annual_csdid(copy(p)[year <= 1903], bstrap = FALSE)$fit
a <- merge(cells_of(region_fe), cells_of(restricted), by = c("g", "t"))
b <- merge(cells_of(pooled_fit), cells_of(restricted), by = c("g", "t"))
stopifnot(nrow(a) == 2L * length(s$grid), max(abs(a$att.x - a$att.y)) < 1e-8,
          max(abs(b$att.x - b$att.y)) > 0.1)
# The restricted pool recovers the effect despite the Southern shocks.
truth <- merge(cells_of(restricted), u[, .(effect = effect[1L]), by = g], by = "g")
truth[, expected := effect * (t - g >= -14L)]
stopifnot(max(abs(truth$att - truth$expected)) < 0.05)

# (b) With every cell finite the ragged controlled aggregation is the balanced
# one, bootstrap draws included.
p[, covariate := x]
cp <- copy(s$panel)[, covariate := x]
w <- s$weights
fits <- lapply(w$g, function(gg) {
  f <- fit_annual_csdid(cp[g >= gg], xformla = ~covariate, est_method = "reg", bstrap = FALSE)
  compact_annual_cohort_fit(f$fit, gg, s$grid, f$warnings)
})
units <- unique(s$panel[, .(id, g, state_id)]); setorder(units, id)
balanced <- aggregate_annual_controlled(fits, units, w, s$grid, seed = 77L, biters = 199L)
ragged <- region_aggregate_controlled(fits, units, w, s$grid, seed = 77L, biters = 199L)
cols <- c("event_time", "att", "se", "status", "conf_low", "conf_high")
stopifnot(isTRUE(all.equal(as.data.frame(balanced$dynamic[, ..cols]),
                           as.data.frame(ragged$dynamic[, ..cols]), tolerance = 1e-12)),
          isTRUE(all.equal(as.data.frame(balanced$cells), as.data.frame(ragged$cells),
                           tolerance = 1e-12)),
          max(abs(balanced$influence - ragged$influence)) < 1e-12,
          all(ragged$dynamic$n_cohorts == 2L), all(ragged$dynamic$n_units == 18L))

# (c) Ragged: cut at a horizon that the 1874 cohort reaches only to +10. Points
# renormalise over the identified cohorts, and the whole influence function
# matches did's own ragged aggregation of a single joint fit.
horizon <- 1884L
cut <- copy(cp)[year <= horizon]
split <- lapply(w$g, function(gg) {
  f <- fit_annual_csdid(cut[g >= gg], xformla = ~covariate, est_method = "reg", bstrap = FALSE)
  region_compact_cohort_fit(f$fit, gg, s$grid, f$warnings, horizon)
})
stopifnot(identical(is.finite(split[[2]]$att), 1874L + s$grid <= horizon),
          all(split[[2]]$inffunc[, 1874L + s$grid > horizon] == 0))
rag <- region_aggregate_controlled(split, units, w, s$grid, seed = 77L, biters = 199L)
manual <- vapply(s$grid, function(e) {
  a <- vapply(split, function(f) f$att[match(e, f$event_time)], numeric(1L))
  sum((a * w$n_counties)[is.finite(a)]) / sum(w$n_counties[is.finite(a)])
}, numeric(1L))
stopifnot(max(abs(rag$dynamic$att - manual)) < 1e-12,
          all(rag$dynamic[event_time > 10L, n_cohorts == 1L & n_units == 9L]),
          all(rag$dynamic[event_time <= 10L, n_cohorts == 2L]),
          all(is.finite(rag$dynamic$se)), rag$dynamic[event_time == 20L, se] > 0,
          identical(is.finite(rag$cells$att), rag$cells$year <= horizon))
joint_fit <- fit_annual_csdid(cut, xformla = ~covariate, est_method = "reg", bstrap = FALSE)$fit
joint <- aggregate_annual_targets(joint_fit, w, s$grid, biters = 199L, ragged = TRUE,
                                  max_year = horizon)
did_if <- joint$aggregation$inf.function$dynamic.inf.func.e
stopifnot(max(abs(joint$dynamic$att - rag$dynamic$att)) < 1e-8,
          max(abs(rag$influence[match(as.integer(rownames(joint_fit$inffunc)), units$id), ] -
                    did_if)) < 1e-8)

# (d) The donor horizon counts states: one state's many counties are not a pool.
d <- data.table(year = rep(1890:1891, each = 5L), state_id = c(1L, 1L, 1L, 2L, 3L, 1L, 1L, 1L, 2L, 3L),
                g = c(1905L, 1905L, 1905L, 1907L, 1880L, 1905L, 1905L, 1905L, 1905L, 1880L))
# 1890: donors (g > 1904) are states 1 and 2; 1891: g > 1905 is none; so 1890.
stopifnot(REGION_MIN_DONOR_STATES == 3L,
          region_last_comparison(d, min_states = 2L) == 1890L,
          identical(region_donor_counts(d)$donor_states, c(2L, 0L)),
          # Two donor states are below the default: no period qualifies.
          inherits(try(region_last_comparison(d), silent = TRUE), "try-error"))
thin <- data.table(year = 1890L, state_id = c(1L, 1L, 1L, 1L), g = 1910L)
stopifnot(inherits(try(region_last_comparison(thin, min_states = 2L), silent = TRUE), "try-error"))

# Why two donor states are not enough: one treated cluster plus two donor
# clusters makes the multiplier distribution {-2c, 0, 0, +2c}.
cl2 <- c(1L, 1L, 2L, 3L); inf2 <- matrix(c(.5, -.5, 1, -1), ncol = 1L)
cl3 <- c(1L, 1L, 2L, 3L, 4L); inf3 <- matrix(c(.5, -.5, 1, -.4, -.6), ncol = 1L)
boot_se <- function(inf, cl, seed) {
  set.seed(seed)
  did:::mboot(inf, list(idname = "id", tname = "year", clustervars = "state_id", biters = 999L,
    alp = .05, panel = FALSE, allow_unbalanced_panel = TRUE, true_repeated_cross_sections = FALSE,
    cluster_vector = cl), return_V = FALSE)$se
}
se2 <- vapply(1:40, function(k) boot_se(inf2, cl2, k), numeric(1L))
se3 <- vapply(1:40, function(k) boot_se(inf3, cl3, k), numeric(1L))
stopifnot(uniqueN(round(se2[is.finite(se2)], 6)) + any(!is.finite(se2)) >= 2L,
          all(is.finite(se3)))

# (e) A Southern target would need a stratified aggregation; refuse it.
bad <- copy(full)
bad$targets <- rbind(bad$targets, bad$targets[1L][, state_abbr := "AL"])
stopifnot(inherits(try(region_restrict_sample(bad), silent = TRUE), "try-error"))

cat("Region x birth-year C&S checks passed: region-FE equivalence, balanced reduction,",
    "ragged aggregation vs did, state-counted horizon, Southern-target guard.\n")
