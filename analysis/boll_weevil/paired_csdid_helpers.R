# Comparison-specific balanced C&S cells, embedded in a common county roster.
# Sourcing this file has no estimation or filesystem side effects.
bwp_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

bwp_pair <- function(roster, outcomes, infestation, age) {
  N <- nrow(roster)
  target_year <- infestation - age
  base_year <- infestation - 19L
  candidates <- roster$infestation_year != infestation &
    roster$g > max(base_year, target_year)
  treated <- roster$infestation_year == infestation
  ybase <- outcomes[, match(base_year, as.integer(colnames(outcomes)))]
  ytarget <- outcomes[, match(target_year, as.integer(colnames(outcomes)))]
  present <- is.finite(ybase) & is.finite(ytarget)
  bwp_assert(all(present[treated]), "A target county is missing a required age/reference observation.")
  selected <- present & (treated | candidates)
  donor <- selected & !treated
  nt <- sum(treated)
  nc <- sum(donor)
  bwp_assert(nt > 0L && nc > 0L, "No treated or paired-control support.")
  delta <- ytarget - ybase
  mt <- mean(delta[treated])
  mc <- mean(delta[donor])
  manual <- mt - mc
  direct_if <- numeric(N)
  direct_if[treated] <- N / nt * (delta[treated] - mt)
  direct_if[donor] <- -N / nc * (delta[donor] - mc)
  early <- min(target_year, base_year)
  late <- max(target_year, base_year)
  sign <- if (target_year < base_year) -1L else 1L
  member <- which(selected)
  pair_data <- data.table::data.table(id = rep(roster$id[member], each = 2L),
    period = rep(1:2, length(member)),
    treat_period = rep(ifelse(treated[member], 2L, 0L), each = 2L),
    model_y = as.vector(t(outcomes[member, match(c(early, late), as.integer(colnames(outcomes))), drop = FALSE])))
  # Reference pairs require no fit: their desired change and influence are zero.
  if (age == 19L) return(list(estimate = 0, influence = direct_if, manual = 0,
    direct_if = direct_if, selected = selected, treated = treated, donor = donor,
    fit = NULL, warnings = character(), orientation_sign = 0L, pair_data = NULL))
  warnings <- character()
  fit <- withCallingHandlers(did::att_gt(yname = "model_y", tname = "period", idname = "id",
    gname = "treat_period", data = as.data.frame(pair_data), panel = TRUE,
    allow_unbalanced_panel = FALSE, control_group = "nevertreated", base_period = "universal",
    xformla = ~1, est_method = "dr", anticipation = 0, bstrap = FALSE, cband = FALSE,
    faster_mode = TRUE, print_details = FALSE), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  column <- which(fit$t == 2L)
  bwp_assert(length(column) == 1L && fit$n == length(member), "Unexpected two-period att_gt output.")
  ids <- as.integer(rownames(fit$inffunc))
  bwp_assert(!anyNA(ids) && setequal(ids, roster$id[member]), "County influence-function IDs are misaligned.")
  influence <- numeric(N)
  influence[match(ids, roster$id)] <- sign * as.numeric(fit$inffunc[, column]) * N / fit$n
  estimate <- sign * fit$att[column]
  bwp_assert(abs(estimate - manual) < 1e-9, "Public att_gt disagrees with direct paired DiD.")
  bwp_assert(max(abs(influence - direct_if)) < 1e-7,
             "Embedded public att_gt influence disagrees with closed-form paired influence.")
  list(estimate = estimate, influence = influence, manual = manual, direct_if = direct_if,
       selected = selected, treated = treated, donor = donor, fit = fit, warnings = unique(warnings),
       orientation_sign = sign, pair_data = pair_data)
}

bwp_aggregate <- function(cells, cell_influence, roster, target_groups) {
  N <- nrow(roster)
  target <- roster$infestation_year %in% target_groups
  M <- sum(target)
  group_n <- vapply(target_groups, function(G) sum(roster$infestation_year == G), integer(1L))
  weights <- group_n / M
  membership <- outer(roster$infestation_year, target_groups, `==`) * 1
  ages <- sort(unique(cells$age_at_infestation), decreasing = TRUE)
  influence <- matrix(0, N, length(ages), dimnames = list(as.character(roster$id), as.character(ages)))
  annual <- vector("list", length(ages))
  for (j in seq_along(ages)) {
    index <- match(paste(target_groups, ages[j]), paste(cells$infestation_year, cells$age_at_infestation))
    bwp_assert(!anyNA(index), "An annual target group is missing.")
    theta <- cells$estimate[index]
    average <- sum(weights * theta)
    influence[, j] <- as.numeric(cell_influence[, index, drop = FALSE] %*% weights) +
      as.numeric(membership %*% (theta - average)) * N / M
    annual[[j]] <- data.table::data.table(age_at_infestation = ages[j], event_time = 18L - ages[j],
      estimate = average, target_counties = M, target_groups = length(target_groups))
  }
  windows <- list(`4-6` = 4:6, `7-9` = 7:9, `4-9` = 4:9)
  pooled <- list(); pooled_influence <- matrix(0, N, length(windows),
    dimnames = list(as.character(roster$id), names(windows)))
  validation <- list()
  for (j in seq_along(windows)) {
    age <- windows[[j]]
    index <- which(cells$age_at_infestation %in% age)
    cell_weights <- weights[match(cells$infestation_year[index], target_groups)] / length(age)
    average <- sum(cells$estimate[index] * cell_weights)
    pooled_influence[, j] <- rowMeans(influence[, match(as.character(age), colnames(influence)), drop = FALSE])
    # Independent group-level construction verifies the group-share derivative.
    group_mean <- vapply(target_groups, function(G)
      mean(cells[infestation_year == G & age_at_infestation %in% age, estimate]), numeric(1L))
    direct <- as.numeric(cell_influence[, index, drop = FALSE] %*% cell_weights) +
      as.numeric(membership %*% (group_mean - average)) * N / M
    validation[[j]] <- data.table::data.table(age_window = names(windows)[j],
      estimate = average, manual_annual_average = mean(vapply(annual, function(z)
        if (z$age_at_infestation %in% age) z$estimate else NA_real_, numeric(1L)), na.rm = TRUE),
      maximum_influence_error = max(abs(direct - pooled_influence[, j])))
    pooled[[j]] <- data.table::data.table(age_window = names(windows)[j], estimate = average,
      target_counties = M, county_age_observations = M * length(age), reference_age = 19L)
  }
  validation <- data.table::rbindlist(validation)
  bwp_assert(max(abs(validation$estimate - validation$manual_annual_average)) < 1e-10 &&
    max(validation$maximum_influence_error) < 1e-8, "Pooled weighting or influence validation failed.")
  list(annual = data.table::rbindlist(annual), annual_influence = influence,
       pooled = data.table::rbindlist(pooled), pooled_influence = pooled_influence,
       validation = validation,
       group_weights = data.table::data.table(infestation_year = target_groups, n_counties = group_n, weight = weights))
}

bwp_bootstrap <- function(annual_if, pooled_if, seed = 20261025L, biters = 1000L) {
  N <- nrow(annual_if)
  bwp_assert(nrow(pooled_if) == N, "Influence rows differ across annual and pooled effects.")
  set.seed(seed)
  low <- (1 - sqrt(5)) / 2
  high <- (1 + sqrt(5)) / 2
  probability_low <- (sqrt(5) + 1) / (2 * sqrt(5))
  multipliers <- matrix(ifelse(runif(biters * N) < probability_low, low, high), biters, N)
  influence <- cbind(annual_if, pooled_if)
  draws <- multipliers %*% influence / N
  se <- apply(draws, 2L, function(x) diff(quantile(x, c(0.25, 0.75), type = 1)) /
                (qnorm(0.75) - qnorm(0.25)))
  nonzero <- colSums(abs(annual_if)) > 1e-10
  bwp_assert(all(is.finite(se)) && all(se[seq_len(ncol(annual_if))][nonzero] > 0),
             "Degenerate bootstrap scale for a nonzero annual influence function.")
  standardized <- abs(sweep(draws[, which(nonzero), drop = FALSE], 2L, se[which(nonzero)], "/"))
  supremum <- apply(standardized, 1L, max)
  raw_critical <- as.numeric(quantile(supremum, 0.95, type = 1))
  # Conservatively floor the joint critical value at the pointwise threshold.
  critical <- max(qnorm(0.975), raw_critical)
  covariance <- crossprod(influence) / N^2
  list(annual_se = se[seq_len(ncol(annual_if))], pooled_se = tail(se, ncol(pooled_if)),
       critical = critical, raw_critical = raw_critical,
       empirical_simultaneous_coverage = mean(supremum <= critical),
       draws = draws, multipliers = multipliers, covariance = covariance,
       seed = seed, biters = biters, iqr_se = se)
}
