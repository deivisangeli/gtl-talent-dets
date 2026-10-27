# Baker-style exposure-age bins with county and birth-year fixed effects.
bwfe_require <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

bwfe_exposure_bins <- function(age) {
  bwfe_require(all(!is.na(age) & age %in% 4:30), "FE sample must contain ages 4-30 only.")
  data.table::data.table(
    exposure_4_6 = as.integer(age %in% 4:6),
    exposure_7_9 = as.integer(age %in% 7:9),
    exposure_10_12 = as.integer(age %in% 10:12),
    exposure_13_15 = as.integer(age %in% 13:15),
    exposure_16_18 = as.integer(age %in% 16:18)
  )
}

bwfe_age_contrasts <- function(beta, covariance, degrees_freedom) {
  terms <- c("exposure_4_6", "exposure_7_9", "exposure_10_12", "exposure_13_15", "exposure_16_18")
  bwfe_require(all(terms %in% names(beta)), "An exposure-age coefficient was dropped or is missing.")
  beta <- beta[terms]
  covariance <- covariance[terms, terms, drop = FALSE]
  L <- rbind(diag(5), c(0.5, 0.5, 0, 0, 0))
  effect <- as.numeric(L %*% beta)
  variance <- diag(L %*% covariance %*% t(L))
  bwfe_require(all(variance >= -1e-12), "Negative contrast variance.")
  se <- sqrt(pmax(0, variance))
  critical <- stats::qt(0.975, degrees_freedom)
  data.table::data.table(age_window = c("4-6", "7-9", "10-12", "13-15", "16-18", "4-9"),
    estimate = effect, se = se, conf_low = effect - critical * se,
    conf_high = effect + critical * se, df = degrees_freedom, critical_value = critical,
    reference_ages = "19-30", covariance_method = "county_clustered_small_sample_adjusted")
}

# Independent FWL implementation: alternately remove county and birth-year
# means. This does not use fixest's within transformation or its covariance code.
bwfe_fwl <- function(y, X, county, birth_year, tolerance = 1e-12, max_iterations = 2000L) {
  county <- as.integer(factor(county))
  birth_year <- as.integer(factor(birth_year))
  nc <- tabulate(county)
  ny <- tabulate(birth_year)
  values <- cbind(y = y, X)
  for (iteration in seq_len(max_iterations)) {
    old <- values
    values <- values - (rowsum(values, county, reorder = TRUE) / nc)[county, , drop = FALSE]
    values <- values - (rowsum(values, birth_year, reorder = TRUE) / ny)[birth_year, , drop = FALSE]
    if (max(abs(values - old)) <= tolerance) break
  }
  bwfe_require(iteration < max_iterations, "Independent FWL demeaning did not converge.")
  Xr <- values[, -1L, drop = FALSE]
  yr <- values[, 1L]
  bread <- solve(crossprod(Xr))
  beta <- as.numeric(bread %*% crossprod(Xr, yr))
  names(beta) <- colnames(X)
  residual <- as.numeric(yr - Xr %*% beta)
  score <- rowsum(Xr * residual, county, reorder = TRUE)
  covariance <- bread %*% crossprod(score) %*% bread
  dimnames(covariance) <- list(colnames(X), colnames(X))
  list(beta = beta, residual = residual, covariance_unadjusted = covariance,
       iterations = iteration, max_county_mean = max(abs(rowsum(values, county) / nc)),
       max_year_mean = max(abs(rowsum(values, birth_year) / ny)))
}
