# Synthetic checks by default; --integration verifies saved production outputs.
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
test_root <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]),
  winslash = "/")) else file.path(getwd(), "analysis/mandatory_schooling")
source(file.path(test_root, "estimate_wwii_service_scientist_1960_iv.R"))
near <- function(a, b, tolerance = 1e-8) stopifnot(isTRUE(all.equal(
  as.numeric(a), as.numeric(b), tolerance = tolerance)))

# Known scientists, unknown/non-occupation codes, blanks, cutoff, and service rules.
fixture <- data.table(YEAR = 1960L, SAMPLE = 196002L, SEX = 1L, BPL = 1L,
  RACE = 1L, PERWT = 20, BIRTHYR = c(1927L, rep(1928L, 11)),
  BIRTHQTR = c(4L, rep(1L, 10), 9L), AGE = 32L, QAGE = 0L,
  OCC1950 = c(ws_scientist_codes, 997L, 999L, NA_integer_, 595L, 7L),
  EMPSTATD = 10L, CLASSWKRD = 22L, VETWWII = c(2L, 1L, 2L, rep(1L, 9)),
  VETKOREA = c(1L, 2L, 2L, rep(1L, 9)), VETSTAT = 2L)
derived <- ws_derive(fixture)
stopifnot(identical(derived$scientist, c(rep(1L, 7), 0L, 0L, NA_integer_, 0L, 1L)))
near(derived$x[1:2], c(0, 1)); near(derived$z[1:2], c(1, 0))
selected <- ws_select(derived, ws_specs()[spec == "primary"])
stopifnot(!any(selected$korean_only), any(selected$veteran == 1 & selected$korea == 1),
  nrow(selected) == 8L, !any(selected$active_military), !anyNA(selected$scientist),
  all(selected$BIRTHQTR %in% 1:4))
near(sum(selected$weight), 20 * nrow(selected))
additional <- fixture[rep(1L, 5L)]
additional[, `:=`(OCC1950 = 7L, EMPSTATD = c(13L, 14L, 15L, 10L, 10L),
  CLASSWKRD = c(22L, 22L, 22L, 26L, 22L), VETWWII = c(2L, 2L, 2L, 2L, 0L))]
stopifnot(!any(ws_derive(additional)$eligible))

# Independently calculated individual weighted 2SLS and cluster covariance.
set.seed(19601928)
people <- CJ(BIRTHYR = 1925:1930, BIRTHQTR = 1:4, person = 1:60)
people[, `:=`(YEAR = 1960L, SAMPLE = 196002L, SEX = 1L, BPL = (person %% 5L) + 1L,
  RACE = (person %% 2L) + 1L, AGE = 1960L - BIRTHYR - as.integer(BIRTHQTR > 1),
  PERWT = 1 + (person %% 4L), EMPSTATD = 10L, CLASSWKRD = 22L,
  QAGE = 0L, VETKOREA = 1L, VETSTAT = 2L)]
people[, x_fixture := 4 * (BIRTHYR - 1927) + BIRTHQTR - 4]
people[, VETWWII := 1L + as.integer(runif(.N) < .3 + .4 * (x_fixture <= 0) - .006 * x_fixture)]
people[, OCC1950 := ifelse(runif(.N) < .06 + .15 * (VETWWII == 2) + .002 * x_fixture, 7L, 100L)]
people <- ws_derive(people)
first <- ws_fit(people, outcome = "veteran")
reduced <- ws_fit(people)
iv <- ws_fit(people, iv = TRUE)
near(coef(iv)["fit_veteran"], coef(reduced)["z"] / coef(first)["z"])
Z <- model.matrix(~z + left_trend + right_trend, people)
X <- model.matrix(~veteran + left_trend + right_trend, people)
WZ <- Z * people$weight
Xhat <- Z %*% solve(crossprod(Z, WZ), crossprod(Z, X * people$weight))
direct <- solve(crossprod(Xhat, Xhat * people$weight),
  crossprod(Xhat, people$scientist * people$weight))
near(coef(iv)["fit_veteran"], direct["veteran", ])
# IV residuals must use actual endogenous regressors, not fitted treatment.
residual <- people$scientist - as.numeric(X %*% direct)
scores <- rowsum(Xhat * (people$weight * residual), people$cohort)
bread <- solve(crossprod(Xhat, Xhat * people$weight))
G <- uniqueN(people$cohort); N <- nrow(people); K <- ncol(X)
direct_vcov <- bread %*% crossprod(scores) %*% bread * G / (G - 1) * (N - 1) / (N - K)
near(vcov(iv)["fit_veteran", "fit_veteran"], direct_vcov["veteran", "veteran"])
reported <- ws_coef(iv, "fit_veteran", "fixture", "iv", people)
near(reported$p_value, 2 * pt(-abs(coef(iv)["fit_veteran"] / se(iv)["fit_veteran"]), G - 1))
flat <- copy(people); flat[, weight := 20]
unweighted <- copy(flat); unweighted[, weight := 1]
near(coef(ws_fit(flat, iv = TRUE)), coef(ws_fit(unweighted, iv = TRUE)))
near(vcov(ws_fit(flat, iv = TRUE)), vcov(ws_fit(unweighted, iv = TRUE)))
for (controls in c("quarter", "birthplace_race")) {
  fs <- ws_fit(people, controls, "veteran"); rf <- ws_fit(people, controls)
  near(coef(ws_fit(people, controls, iv = TRUE))["fit_veteran"], coef(rf)["z"] / coef(fs)["z"])
}

# AR p-values checked against direct transformed-outcome regressions.
ar <- ws_ar(people, first, reduced)
for (beta in seq(-.5, .5, length.out = 11)) {
  transformed <- copy(people); transformed[, adjusted := scientist - beta * veteran]
  model <- ws_fit(transformed, outcome = "adjusted")
  direct_p <- pf((coef(model)["z"] / se(model)["z"])^2, 1, G - 1, lower.tail = FALSE)
  near(ws_ar_p(beta, ar$components), direct_p)
}
near(ws_quadratic_set(1, 0, -1)$intervals$lower, -1)
near(ws_quadratic_set(1, 0, -1)$intervals$upper, 1)
tails <- ws_quadratic_set(-1, 0, 1)
stopifnot(tails$type == "disjoint_unbounded")
near(tails$intervals$lower, c(-Inf, 1)); near(tails$intervals$upper, c(-1, Inf))
stopifnot(ws_quadratic_set(1, 0, 1)$type == "empty",
  ws_quadratic_set(-1, 0, -1)$type == "all_real",
  ws_quadratic_set(0, 0, 0)$type == "all_real",
  ws_quadratic_set(0, 0, 1)$type == "empty",
  ws_quadratic_set(1, -2, 1)$type == "point")
near(ws_quadratic_set(0, 2, -4)$intervals$upper, 2)
near(ws_quadratic_set(0, -2, 4)$intervals$lower, 2)
grid <- seq(-5, 5, length.out = 501)
membership <- vapply(grid, function(b) any(b >= ar$intervals$lower & b <= ar$intervals$upper), logical(1))
stopifnot(identical(membership, ws_ar_p(grid, ar$components) >= .05))
cohort_fixture <- ws_cohorts(people, "fixture")
stopifnot(nrow(cohort_fixture) == 24L, sum(cohort_fixture$observations) == nrow(people))
near(sum(cohort_fixture$weighted_population), sum(people$weight))
message("WWII 1960 IV synthetic checks passed")

if ("--integration" %in% commandArgs(TRUE)) {
  paths <- ws_paths()
  estimates <- fread(file.path(paths$out, "estimates.csv"))
  samples <- fread(file.path(paths$out, "sample_summaries.csv"))
  exclusions <- fread(file.path(paths$out, "sample_exclusions.csv"))
  diagnostics <- fread(file.path(paths$out, "model_diagnostics.csv"))
  cohorts <- fread(file.path(paths$out, "cohort_summaries.csv"))
  manifest <- fread(file.path(paths$out, "input_manifest.csv"))
  components <- fread(file.path(paths$out, "anderson_rubin_components.csv"))
  intervals <- fread(file.path(paths$out, "anderson_rubin_intervals.csv"))
  stopifnot(nrow(estimates) == 30L, nrow(samples) == 10L,
    samples[spec == "primary", observations] == 245883L,
    samples[spec == "primary", scientist_count] == 1189L,
    samples[spec == "primary", cohort_clusters] == 24L,
    samples[spec == "primary", min_weight] == 20,
    samples[spec == "primary", max_weight] == 20,
    exclusions[stage == "exclude_korean_only", excluded_at_stage] == 75364L,
    cohorts[sample == "primary", sum(dual_service_count)] == 15211L,
    cohorts[sample == "primary", sum(korean_only_count)] == 0L,
    all(abs(diagnostics$iv_minus_wald) < 1e-8), all(is.finite(estimates$p_value)))
  stopifnot(identical(unname(tools::md5sum(manifest$path)), manifest$md5))
  primary_models <- readRDS(file.path(paths$out, "models/primary.rds"))
  near(ws_ar_p(0, components), estimates[spec == "primary" & model == "reduced_form", p_value])
  near(ws_ar_p(coef(primary_models$iv_2sls)["fit_veteran"], components), 1)
  if (nrow(intervals)) for (bound in c(intervals$lower, intervals$upper)) {
    if (is.finite(bound)) near(ws_ar_p(bound, components), .05)
  }
  grid <- seq(-1, 1, length.out = 1001)
  membership <- vapply(grid, function(b) any(b >= intervals$lower & b <= intervals$upper), logical(1))
  stopifnot(identical(membership, ws_ar_p(grid, components) >= .05))
  plot_paths <- file.path(paths$out, "figures", c("cohort_cutoff.png", "cohort_cutoff.pdf",
    "korean_service_and_retention.png", "korean_service_and_retention.pdf"))
  stopifnot(all(file.exists(plot_paths)), all(file.info(plot_paths)$size > 1000))
  message("WWII 1960 IV production checks passed")
}
