# Matching and fitting primitives shared by observed events and local placebos.
ms_rich_features <- c("manufacturing_intensity", "agricultural_intensity",
                      "illiteracy", "foreign_born_share")
ms_base_features <- c("outcome_bin1", "outcome_bin2", "outcome_bin3",
                     "log_population", "log_population_growth", "log_density")
ms_attribute_features <- c("log_population", "log_population_growth", "log_density", ms_rich_features)

ms_matching_config <- function(value = Sys.getenv("MS_SYNTH_MATCHING_SET", "default")) {
  stopifnot(value %in% c("default", "attributes_only"))
  list(specifications = if (value == "default") c("original", "richer") else "attributes_only",
       directory = if (value == "default") "synth_matching" else "synth_matching_attributes_only")
}

ms_complete <- function(features, columns) {
  features[, Reduce(`&`, lapply(.SD, is.finite)), .SDcols = columns]
}

ms_covariate_vintages <- function(manifest, g) {
  data.table::rbindlist(lapply(ms_rich_features, function(v) {
    yy <- manifest[variable == v & available == TRUE & year <= g - 15L, year]
    data.table::data.table(variable = v, census_year = if (length(yy)) max(yy) else NA_integer_)
  }))
}

ms_cohort_features <- function(panel, targets, covariates, manifest, area, g, pre, outcome) {
  gg <- g
  years <- seq.int(gg - pre, gg + 20L)
  clean <- seq.int(gg - pre, gg - 15L)
  window <- panel[year %in% years & valid_outcomes == TRUE]
  units <- window[, .(rows = .N, g = g[1], state_id = state_id[1],
                     state_abbr = state_abbr[1], GEOID = GEOID[1]), by = id]
  units[, is_target := id %in% targets[g == gg, id]]
  units[, donor_eligible := rows == length(years) & g > gg + 34L]
  units <- units[(is_target & rows == length(years)) | donor_eligible]
  data <- window[id %in% units$id]
  # Three nonoverlapping bins use every clean pre-exposure year, preserving
  # comparable feature weights between the six- and 26-year fitting windows.
  bins <- split(clean, cut(seq_along(clean), breaks = 3, labels = FALSE))
  for (i in seq_along(bins)) {
    value <- data[year %in% bins[[i]], .(value = mean(get(outcome))), by = id]
    if (outcome != "log1p_n_amws") value[, value := log1p(value)]
    units[value, on = "id", (paste0("outcome_bin", i)) := i.value]
  }
  p0 <- data[year == gg - pre, .(id, p_start = population)]
  pb <- data[year == gg - 15L, .(id, p_base = population)]
  units <- merge(merge(units, p0, by = "id"), pb, by = "id")
  units <- merge(units, area, by = "GEOID", all.x = TRUE, sort = FALSE)
  units[, `:=`(log_population = log(p_base),
    log_population_growth = log(p_base / p_start) / (pre - 15L),
    log_density = log(p_base / land_km2))]
  vintages <- ms_covariate_vintages(manifest, gg)
  for (v in ms_rich_features) {
    yy <- vintages[variable == v, census_year]
    units[, (v) := NA_real_]
    if (!is.na(yy)) {
      cv <- covariates[year == yy, .(GEOID, value = get(v))]
      stopifnot(!anyDuplicated(cv$GEOID))
      units[cv, on = "GEOID", (v) := i.value]
    }
    units[, (paste0(v, "_year")) := yy]
  }
  units[, base_complete := Reduce(`&`, lapply(.SD, is.finite)), .SDcols = ms_base_features]
  units[, rich_complete := base_complete & Reduce(`&`, lapply(.SD, is.finite)), .SDcols = ms_rich_features]
  # Outcome availability still defines the balanced estimation panel; AMWS lag
  # values do not enter attribute completeness, distances, or support checks.
  units[, attributes_complete := ms_complete(units, ms_attribute_features)]
  units[, missing_covariates := apply(as.matrix(.SD), 1L, function(z)
    paste(ms_rich_features[!is.finite(z)], collapse = ";")), .SDcols = ms_rich_features]
  wide <- data.table::dcast(data, id ~ year, value.var = outcome)
  Y <- as.matrix(wide[, -1L]); rownames(Y) <- as.character(wide$id)
  list(features = units, panel = data, vintages = vintages, years = years, clean = clean, Y = Y)
}

ms_match <- function(target_id, features, candidate_ids, columns, k = 30L) {
  target <- features[id == target_id]
  stopifnot(nrow(target) == 1L, !anyDuplicated(features$id))
  donor <- data.table::copy(features[id %in% candidate_ids & id != target_id & state_id != target$state_id])
  donor <- donor[donor[, Reduce(`&`, lapply(.SD, is.finite)), .SDcols = columns]]
  empty <- function(status) list(ok = FALSE, status = status, selected = data.table::data.table(),
                                balance = data.table::data.table())
  tx <- as.numeric(target[, ..columns])
  if (any(!is.finite(tx))) return(empty("missing_target_features"))
  if (nrow(donor) < k) return(empty("insufficient_donors"))
  dx <- as.matrix(donor[, ..columns])
  # setorder below mutates columns by reference: preserve the matrix row ids.
  original_ids <- data.table::copy(donor$id)
  mu <- colMeans(dx); sigma <- apply(dx, 2L, stats::sd)
  constant <- !is.finite(sigma) | sigma <= 1e-12
  unsupported <- constant & abs(tx - mu) > 1e-10 * pmax(1, abs(mu))
  if (any(unsupported)) return(empty(paste0("constant_donor_mismatch:", paste(columns[unsupported], collapse = ";"))))
  z <- matrix(0, nrow(dx), ncol(dx))
  if (any(!constant)) z[, !constant] <- sweep(sweep(dx[, !constant, drop = FALSE],
    2L, tx[!constant], "-"), 2L, sigma[!constant], "/")
  donor[, distance := sqrt(rowSums(z^2))]
  data.table::setorder(donor, distance, id)
  selected <- donor[seq_len(k), .(id, state_id, distance)]
  sel <- dx[match(selected$id, original_ids), , drop = FALSE]
  balance <- data.table::data.table(variable = columns, treated = tx,
    eligible_mean = mu, eligible_sd = sigma, selected_mean = colMeans(sel),
    eligible_min = apply(dx, 2L, min), eligible_max = apply(dx, 2L, max), constant = constant)
  balance[, outside_range := treated < eligible_min | treated > eligible_max]
  balance[, selected_std_gap := ifelse(constant, 0, (treated - selected_mean) / eligible_sd)]
  list(ok = TRUE, status = "matched", selected = selected, balance = balance)
}

ms_fit <- function(cohort, target_id, selected, outcome) {
  ids <- c(selected$id, target_id)
  if (!is.null(cohort$Y)) {
    Y <- cohort$Y[as.character(ids), , drop = FALSE]
  } else {
    values <- data.table::dcast(cohort$panel[id %in% ids], id ~ year, value.var = outcome)
    Y <- as.matrix(values[match(ids, values$id), -1L])
  }
  stopifnot(ncol(Y) == length(cohort$years), all(is.finite(Y)))
  n0 <- nrow(Y) - 1L; t0 <- length(cohort$clean)
  noise <- stats::sd(as.numeric(Y[, 2:t0, drop = FALSE] - Y[, 1:(t0 - 1L), drop = FALSE]))
  floor <- 1e-8 * max(1, mean(abs(Y[, seq_len(t0), drop = FALSE])))
  noise <- max(noise, floor)
  fit <- synthdid::synthdid_estimate(Y, n0, t0, noise.level = noise)
  omega <- as.numeric(attr(fit, "weights")$omega)
  stopifnot(all(is.finite(omega)), all(omega >= -1e-10), abs(sum(omega) - 1) < 1e-8)
  treated <- as.numeric(Y[nrow(Y), ])
  synthetic <- as.numeric(crossprod(omega, Y[seq_len(n0), , drop = FALSE]))
  gap <- treated - synthetic; adjusted <- gap - gap[t0]
  pre_rmspe <- sqrt(mean(gap[seq_len(t0)]^2))
  post_index <- which(cohort$years >= max(cohort$clean) + 15L)
  post_effect <- mean(adjusted[post_index])
  list(path = data.table::data.table(year = cohort$years, event_time = cohort$years - max(cohort$clean) - 15L,
    treated = treated, synthetic = synthetic, gap = gap, gap_adjusted = adjusted),
    weights = data.table::data.table(id = selected$id, weight = omega),
    summary = data.table::data.table(pre_rmspe = pre_rmspe, noise_floor = floor,
      zero_pre_fit = pre_rmspe <= floor, effect_e0_e20 = post_effect,
      standardized_effect = if (pre_rmspe > floor) post_effect / pre_rmspe else NA_real_,
      synthdid_att_exposure_m14_p20 = as.numeric(fit)))
}

ms_weighted_balance <- function(balance, features, weights) {
  b <- data.table::copy(balance)
  x <- features[match(weights$id, id)]
  b[, synthetic := vapply(variable, function(v) sum(x[[v]] * weights$weight), numeric(1))]
  b[, synthetic_std_gap := ifelse(constant, 0, (treated - synthetic) / eligible_sd)]
  b
}
