# Annual exposure-age indicators and prespecified contrasts for the FE590 sample.
bwa_require <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

bwa_indicators <- function(age) {
  bwa_require(all(!is.na(age) & age %in% 4:30), "Exposure ages must be integers in 4-30.")
  x <- vapply(4:18, function(a) as.integer(age == a), integer(length(age)))
  colnames(x) <- paste0("age_", 4:18)
  data.table::as.data.table(x)
}

bwa_contrast_matrix <- function() {
  windows <- list(`4-9` = 4:9, `10-13` = 10:13, `14-18` = 14:18,
                  `13-18` = 13:18, `16-18` = 16:18)
  L <- matrix(0, 6, 15, dimnames = list(c(names(windows), "14-18_minus_4-9"), paste0("age_", 4:18)))
  for (w in names(windows)) L[w, paste0("age_", windows[[w]])] <- 1 / length(windows[[w]])
  L["14-18_minus_4-9", ] <- L["14-18", ] - L["4-9", ]
  L
}

bwa_inference <- function(beta, covariance, degrees_freedom = 589L) {
  L <- bwa_contrast_matrix()
  terms <- colnames(L)
  bwa_require(all(terms %in% names(beta)), "An annual-age coefficient is missing.")
  beta <- beta[terms]
  covariance <- covariance[terms, terms, drop = FALSE]
  theta <- as.numeric(L %*% beta)
  contrast_covariance <- L %*% covariance %*% t(L)
  variance <- diag(contrast_covariance)
  bwa_require(all(variance >= -1e-12), "Negative contrast variance.")
  se <- sqrt(pmax(0, variance))
  statistic <- theta / se
  statistic[se == 0 & abs(theta) < 1e-12] <- 0
  p_value <- 2 * stats::pt(-abs(statistic), df = degrees_freedom)
  critical <- stats::qt(.975, degrees_freedom)
  table <- data.table::data.table(contrast = rownames(L),
    type = c(rep("age_window", 5), "difference"), primary = rownames(L) == "14-18",
    estimate = theta, se = se, t_statistic = statistic, df = degrees_freedom,
    conf_low = theta - critical * se, conf_high = theta + critical * se,
    p_value = p_value, p_holm = stats::p.adjust(p_value, method = "holm"),
    multiplicity_family_size = 6L, reference_ages = "19-30", critical_value = critical)
  list(table = table, covariance = contrast_covariance, L = L)
}
