# Known conditional trends, singleton treatment cohort, ragged donors, and joint IFs.
suppressPackageStartupMessages({library(data.table); library(did)})
source("analysis/mandatory_schooling/annual_csdid_helpers.R")
source("analysis/mandatory_schooling/annual_csdid_population_controls.R")

u <- data.table(id = 1:37, g = c(rep(c(1852L, 1874L, 1900L, 1918L), each = 9L), 1864L))
u[, `:=`(state_id = c(rep(1:12, each = 3L), 13L),
          GEOID = sprintf("%05d", id), x = log(100 + id * 23))]
u[, `:=`(state_fips = sprintf("%02d", state_id), state_abbr = paste0("S", state_id),
          target = g <= 1880L, effect = fifelse(g == 1852L, 2, fifelse(g == 1864L, 7, 4)))]
p <- merge(CJ(id = u$id, year = 1800:1960), u, by = "id")
p[, `:=`(event_time = year - g, population = exp(x + .002 * (year - 1800)),
          population_source = "nhgis")]
p[, model_y := (year - 1800) * x * .02 + effect * (event_time >= -14L)]
p <- p[!(id > 18 & id != 37 & id %% 3 == 0 & year < 1850 & year %% 2 == 0)]
w <- u[target == TRUE, .(n_counties = .N, n_jurisdictions = uniqueN(state_id)), by = g]
setorder(w, g); w[, weight := n_counties / sum(n_counties)]
s <- list(panel = p, weights = w, grid = -20:20)
a <- annual_covariate_audit(p, s, 20, "baseline_log_population")
cagr <- annual_covariate_audit(p, s, 40, "preperiod_population_cagr")
cagr20 <- annual_covariate_audit(p, s, 20, "preperiod_population_cagr")
stopifnot(max(abs(cagr[eligible == TRUE, covariate] - 100 * expm1(.002))) < 1e-10,
          max(abs(cagr20[eligible == TRUE, covariate] - 100 * expm1(.002))) < 1e-10,
          all(cagr20$cagr_years == 5L),
          all(cagr$cagr_years == 25L), all(a$baseline_year == a$focal_g - 15L),
          all(cagr$cagr_start_year == cagr$focal_g - 40L))
# A missing endpoint excludes a donor only from affected focal-cohort fits.
bad <- copy(p); bad[id == 20 & year == 1837, population := NA_real_]
bad_a <- annual_covariate_audit(bad, s, 20, "baseline_log_population")
stopifnot(!bad_a[id == 20 & focal_g == 1852, eligible],
          bad_a[id == 20 & focal_g == 1874, eligible])
fits <- lapply(w$g, function(gg) {
  cp <- annual_control_panel(p, a, gg)
  stopifnot(cp[, all(vapply(split(covariate, id), function(x) uniqueN(x) == 1L, logical(1L)))])
  f <- fit_annual_csdid(cp, xformla = ~covariate, est_method = "reg", bstrap = FALSE)
  z <- compact_annual_cohort_fit(f$fit, gg, s$grid, f$warnings)
  stopifnot(identical(z$metadata$xformla, "~covariate"))
  expected <- u[g == gg, effect[1L]] * (s$grid >= -14L)
  stopifnot(max(abs(z$att - expected)) < 1e-8,
    abs(z$att[match(0, s$grid)] - manual_annual_reg_did(cp, gg, 0)) < 1e-8)
  z
})
units <- u[, .(id, g, state_id)]
agg <- aggregate_annual_controlled(fits, units, w, s$grid, seed = 141L, biters = 199L)
expected <- sum(w$weight * c(2, 7, 4)) * (s$grid >= -14L)
stopifnot(max(abs(agg$dynamic$att - expected)) < 1e-8,
          agg$dynamic[event_time == 0, se] > 0,
          max(abs(colMeans(agg$influence))) < 1e-8)
# Zero cohort-estimation IFs isolate the nonzero cohort-share contribution.
zero_fits <- lapply(fits, function(f) {f$inffunc[,] <- 0; f})
share <- aggregate_annual_controlled(zero_fits, units, w, s$grid, 141L, 199L)
stopifnot(share$dynamic[event_time == 0, se] > 0)
# Invariance to fitted sample IF-row order and to common-universe ordering.
shuffled <- lapply(fits, function(f) {
  idx <- rev(seq_len(f$n)); f$ids <- f$ids[idx]; f$clusters <- f$clusters[idx]
  f$inffunc <- f$inffunc[idx, , drop = FALSE]; f
})
other <- aggregate_annual_controlled(shuffled, units[.N:1], w, s$grid, 141L, 199L)
stopifnot(isTRUE(all.equal(as.data.frame(agg$dynamic), as.data.frame(other$dynamic), tolerance = 1e-10)))
# A missing target effect must invalidate that pooled year, not change weights.
missing <- fits; missing[[1]]$att[match(0, s$grid)] <- NA_real_
omitted <- aggregate_annual_controlled(missing, units, w, s$grid, 141L, 199L)
stopifnot(is.na(omitted$dynamic[event_time == 0, att]),
          omitted$dynamic[event_time == 0, status] == "incomplete_cohort_support")

# Compare the entire influence function against a single att_gt + aggte fit
# when cohort-specific covariates differ only by an intercept shift. Add noise
# so both the estimation and cohort-share parts of the IF are nonzero.
noisy <- copy(p)
noisy[, `:=`(covariate = x, model_y = model_y + (id %% 7) * cos(year / 7 + id))]
full <- fit_annual_csdid(noisy, xformla = ~covariate, est_method = "reg", bstrap = FALSE)$fit
split_fits <- lapply(w$g, function(gg) {
  f <- fit_annual_csdid(noisy[g >= gg], xformla = ~covariate, est_method = "reg", bstrap = FALSE)$fit
  z <- compact_annual_cohort_fit(f, gg, s$grid, character())
  cols <- match(paste(gg, gg + s$grid), paste(full$group, full$t))
  padded <- matrix(0, full$n, length(s$grid))
  padded[match(z$ids, as.integer(rownames(full$inffunc))), ] <- z$inffunc * full$n / z$n
  stopifnot(max(abs(padded - as.matrix(full$inffunc[, cols]))) < 1e-8)
  z
})
joint <- aggregate_annual_controlled(split_fits, units, w, s$grid, 141L, 199L)
standard <- aggregate_annual_targets(full, w, s$grid, biters = 199L)
standard_if <- standard$aggregation$inf.function$dynamic.inf.func.e
stopifnot(max(abs(joint$dynamic$att - standard$dynamic$att)) < 1e-8,
  max(abs(joint$influence[match(as.integer(rownames(full$inffunc)), units$id), ] - standard_if)) < 1e-8)
cat("Population covariates, conditional C&S, singleton cohort, joint clustering, and missing-cell checks passed.\n")
