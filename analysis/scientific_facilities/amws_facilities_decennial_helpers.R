# Complete ten-year outcomes and explicit C&S 2x2 comparisons. Source the
# annual helpers first for shared spatial assignment, labels, and annotations.
sfd_settings <- function() {
  s <- sfa_settings()
  s$first_year <- 1860L; s$pre <- 30L; s$balanced_post <- 10L; s$reference <- -10L
  s
}

sfd_figure_name <- function(outcome, timing, control_group, balance) {
  paste0("ES10_", outcome, "__", ifelse(timing == "standard_decade", "std", "shift"),
    "__", ifelse(control_group == "nevertreated", "never", "notyet"),
    "__", ifelse(balance == "balanced", "bal", "unbal"), ".png")
}

sfd_decade <- function(year, timing) {
  stopifnot(timing %in% c("standard_decade", "alternative_decade"))
  floor(year / 10) * 10 + if (timing == "alternative_decade") 10 * (year %% 10 >= 7) else 0
}

sfd_build_panel <- function(p) {
  p <- data.table::copy(p)
  p[, decade := floor(year / 10) * 10]
  blocks <- p[, {
    complete <- .N == 10L && identical(sort(year), as.integer(decade[1] + 0:9))
    count_ok <- complete && all(is.finite(n_amws) & n_amws >= 0)
    pop_ok <- complete && all(is.finite(population) & population > 0)
    birth_ok <- complete && all(is.finite(county_births_estimate_year) & county_births_estimate_year > 0)
    .(annual_rows = .N, count_complete = count_ok, population_complete = pop_ok,
      births_complete = birth_ok,
      n_amws = if (count_ok) sum(n_amws) else NA_real_,
      population = if (pop_ok) mean(population) else NA_real_,
      county_births_estimate = if (birth_ok) sum(county_births_estimate_year) else NA_real_)
  }, by = .(GEOID, decade)]
  blocks[, `:=`(log1p_n_amws = log1p(n_amws),
    amws_per_100k = 100000 * n_amws / population,
    amws_per_1000_births = 1000 * n_amws / county_births_estimate)]
  audit <- data.table::rbindlist(lapply(names(sfa_outcomes), function(o) {
    blocks[, .(GEOID, decade, annual_rows, outcome = o, valid = is.finite(get(o)),
                count_complete, population_complete, births_complete)]
  }))
  list(panel = blocks, audit = audit)
}

sfd_model_data <- function(blocks, assignment, outcome, timing) {
  p <- merge(blocks, assignment[, .(GEOID, first_exposure_year, role)], by = "GEOID")
  p <- p[is.finite(get(outcome))]
  p[, .(id = as.integer(GEOID), GEOID, year = as.integer(decade),
    g = as.integer(sfd_decade(first_exposure_year, timing)), role, first_exposure_year,
    y = get(outcome))]
}

sfd_select <- function(d, gg, yy, bb, control_group, paired = FALSE) {
  target <- d$role == "target" & !is.na(d$g) & d$g == gg
  boundary <- if (control_group == "nevertreated") 1949L else max(yy, bb) + 9L
  donor <- !target & (is.na(d$first_exposure_year) | d$first_exposure_year > boundary)
  b <- data.table::copy(d[year == bb & (target | donor)])
  t <- data.table::copy(d[year == yy & (target | donor)])
  if (paired) {
    ids <- intersect(b$id, t$id); b <- b[id %in% ids]; t <- t[id %in% ids]
  }
  for (x in list(b, t)) x[, participant_role := ifelse(role == "target" & g == gg, "treated", "control")]
  list(b = b, t = t, boundary = boundary)
}

sfd_cell_fit <- function(selected, control_group) {
  b <- data.table::copy(selected$b); t <- data.table::copy(selected$t)
  b[, period := 1L]; t[, period := 2L]
  sample <- data.table::rbindlist(list(b, t))
  sample[, treatment := ifelse(participant_role == "treated", 2L, 0L)]
  warnings <- character()
  model <- withCallingHandlers(suppressMessages(did::att_gt(
    yname = "y", tname = "period", idname = "id", gname = "treatment",
    data = as.data.frame(sample), xformla = ~1, control_group = control_group,
    est_method = "dr", base_period = "universal", anticipation = 0L,
    panel = TRUE, allow_unbalanced_panel = TRUE, bstrap = FALSE, cband = FALSE,
    clustervars = "id", alp = .05)), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  pos <- which(model$group == 2L & model$t == 2L)
  if (length(pos) != 1L || !is.finite(model$att[pos]) ||
      any(!is.finite(model$inffunc[, pos]))) stop("Non-estimable 2x2 ATT or influence function")
  ids <- unique(model$DIDparams$data$id)
  if (length(ids) != nrow(model$inffunc) || length(ids) != model$n) stop("Unknown IF row alignment")
  list(att = model$att[pos], contribution = as.numeric(model$inffunc[, pos]) / model$n,
       ids = ids, paired = isTRUE(model$DIDparams$panel), warnings = unique(warnings))
}

sfd_estimate <- function(d, control_group, settings, estimate = TRUE) {
  targets <- sort(unique(d[role == "target", g]))
  grid <- seq(-settings$pre, floor(settings$last_year / 10) * 10 - min(targets), by = 10)
  ids <- sort(unique(d$id))
  rows <- list(); influences <- list(); participants <- list(); baselines <- list(); warnings <- list()
  k <- 0L
  for (gg in targets) for (ee in grid) {
    k <- k + 1L; yy <- gg + ee; bb <- gg - 10L
    sel <- sfd_select(d, gg, yy, bb, control_group)
    status <- if (yy < settings$first_year || yy + 9L > settings$last_year || bb < settings$first_year)
      "outside_calendar_window" else if (!nrow(sel$b[participant_role == "treated"]) ||
        !nrow(sel$t[participant_role == "treated"])) "missing_treated_observations" else if (
        !nrow(sel$b[participant_role == "control"]) || !nrow(sel$t[participant_role == "control"]))
        "missing_unexposed_controls" else "available"
    att <- NA_real_; influence <- rep(NA_real_, length(ids)); error <- NA_character_
    if (status == "available" && estimate) {
      fitted <- if (ee == -10L) list(att = 0, contribution = 0,
        ids = ids, paired = FALSE, warnings = character()) else
        tryCatch(sfd_cell_fit(sel, control_group), error = function(e) e)
      if (inherits(fitted, "error")) {
        status <- "failed_estimation"; error <- conditionMessage(fitted)
      } else {
        att <- fitted$att; influence <- numeric(length(ids))
        influence[match(fitted$ids, ids)] <- fitted$contribution
        if (fitted$paired) sel <- sfd_select(d, gg, yy, bb, control_group, paired = TRUE)
        if (length(fitted$warnings)) warnings[[k]] <- data.table::data.table(
          g = gg, event_time = ee, warning = fitted$warnings)
        u <- unique(data.table::rbindlist(list(sel$b, sel$t)), by = "id")
        participants[[k]] <- u[, .(id, GEOID, participant_role,
          first_exposure_year, g = gg, event_time = ee)]
        baselines[[k]] <- sel$b[, .(id, GEOID, base_year = bb, y, g = gg, event_time = ee)]
        status <- if (ee == -10L) "reference" else "estimated"
      }
    }
    controls <- data.table::rbindlist(list(sel$b, sel$t))[participant_role == "control"]
    if (nrow(controls) && any(!is.na(controls$first_exposure_year) &
                              controls$first_exposure_year <= sel$boundary)) stop("Exposed control admitted")
    rows[[k]] <- data.table::data.table(g = gg, year = yy, block_end_year = yy + 9L,
      event_time = ee, base_year = bb, base_end_year = bb + 9L,
      treated_base_counties = nrow(sel$b[participant_role == "treated"]),
      treated_year_counties = nrow(sel$t[participant_role == "treated"]),
      treated_paired_counties = length(intersect(sel$b[participant_role == "treated", id],
                                               sel$t[participant_role == "treated", id])),
      control_base_counties = nrow(sel$b[participant_role == "control"]),
      control_year_counties = nrow(sel$t[participant_role == "control"]),
      control_paired_counties = length(intersect(sel$b[participant_role == "control", id],
                                               sel$t[participant_role == "control", id])),
      control_exposure_boundary = sel$boundary,
      earliest_control_exposure = if (all(is.na(controls$first_exposure_year))) NA_real_ else
        min(controls$first_exposure_year, na.rm = TRUE),
      att = att, estimable = is.finite(att), status = status, error = error)
    influences[[k]] <- influence
  }
  list(cells = data.table::rbindlist(rows), contribution = do.call(cbind, influences),
       ids = ids, participants = data.table::rbindlist(participants),
       baselines = data.table::rbindlist(baselines), warnings = data.table::rbindlist(warnings), grid = grid)
}

# Each IF column is already divided by its fit's county count. Add the
# derivative of the cohort-size weights, then use common county multipliers.
sfd_weighted <- function(fit, d, positions, multipliers) {
  c <- fit$cells[positions]
  units <- unique(d[role == "target", .(id, g)])
  sizes <- units[, .(n_counties = .N), by = g]
  n <- sizes$n_counties[match(c$g, sizes$g)]
  weights <- n / sum(n)
  att <- sum(c$att * weights)
  contribution <- as.numeric(fit$contribution[, positions, drop = FALSE] %*% weights)
  for (gg in unique(c$g)) {
    delta <- sum(c$att[c$g == gg] - att) / sum(n)
    ii <- match(units[g == gg, id], fit$ids)
    contribution[ii] <- contribution[ii] + delta
  }
  draws <- as.numeric(multipliers %*% contribution)
  se <- stats::sd(draws)
  data.table::data.table(estimate = att, manual_att = weighted.mean(c$att, n),
    se = se, ci_low = att - qnorm(.975) * se, ci_high = att + qnorm(.975) * se,
    p_value = if (se > 0) 2 * pnorm(-abs(att / se)) else ifelse(att == 0, 1, 0),
    n_cohorts = data.table::uniqueN(c$g), n_cohort_time_cells = nrow(c),
    cohort_years = paste(sort(unique(c$g)), collapse = ";"),
    cohort_counties = sum(sizes[g %in% c$g, n_counties]),
    treated_year_counties = sum(c$treated_year_counties),
    treated_paired_counties = sum(c$treated_paired_counties),
    min_control_year_counties = min(c$control_year_counties),
    min_control_paired_counties = min(c$control_paired_counties))
}

sfd_aggregate <- function(fit, d, balance, settings) {
  cells <- data.table::copy(fit$cells)
  balanced_grid <- seq(-10L, settings$balanced_post, by = 10L)
  balanced <- sfa_balanced_cohorts(cells, balanced_grid)
  cohorts <- if (balance == "balanced") balanced else unique(cells[estimable == TRUE, g])
  if (!length(cohorts)) stop("No estimable cohorts for ", balance)
  grid <- if (balance == "balanced") balanced_grid else fit$grid
  selected <- which(cells$g %in% cohorts & cells$event_time %in% grid & cells$estimable)
  set.seed(settings$seed + ifelse(balance == "balanced", 1L, 2L))
  multipliers <- matrix(rnorm(settings$biters * length(fit$ids)), nrow = settings$biters)
  dynamic <- data.table::rbindlist(lapply(grid, function(ee) {
    positions <- selected[cells$event_time[selected] == ee]
    if (!length(positions)) return(data.table::data.table(event_time = ee, status = "not_estimable"))
    row <- sfd_weighted(fit, d, positions, multipliers)
    row[, `:=`(event_time = ee, status = if (ee == -10L) "reference" else "estimated")]
    row
  }), fill = TRUE)
  if (balance == "balanced" && (!all(is.finite(dynamic$estimate)) ||
                                  any(dynamic$n_cohorts != length(cohorts)))) stop("Balanced membership changed")
  if (any(abs(dynamic$estimate - dynamic$manual_att) > 1e-10, na.rm = TRUE)) stop("Weight mismatch")
  horizons <- unique(c(max(grid), settings$balanced_post))
  summaries <- data.table::rbindlist(lapply(horizons, function(h) {
    positions <- selected[cells$event_time[selected] >= 0 & cells$event_time[selected] <= h]
    row <- sfd_weighted(fit, d, positions, multipliers)
    row[, `:=`(post_min = 0L, post_max = h, summary_weighting = "cohort_size_weighted_group_time_cells")]
    row
  }))
  keys <- cells[selected, .(g, event_time)]
  people <- fit$participants[keys, on = c("g", "event_time"), nomatch = 0]
  bases <- fit$baselines[keys, on = c("g", "event_time"), nomatch = 0]
  compact <- people[, .(n_comparison_cells = .N), by = .(id, GEOID, participant_role)]
  annotation <- sfa_annotation(compact, bases[, .(id, GEOID, base_year, y)])
  chosen <- cells[selected]
  sizes <- unique(d[role == "target", .(id, g)])[, .(n_counties = .N), by = g]
  chosen <- merge(chosen, sizes, by = "g")
  chosen[, cohort_weight := n_counties / sum(n_counties), by = event_time]
  # For cell intervals, include both fit uncertainty and common bootstrap draws.
  chosen[, se := vapply(seq_len(.N), function(j) {
    original <- which(cells$g == g[j] & cells$event_time == event_time[j])
    stats::sd(as.numeric(multipliers %*% fit$contribution[, original]))
  }, numeric(1))]
  chosen[, `:=`(ci_low = att - qnorm(.975) * se, ci_high = att + qnorm(.975) * se)]
  list(dynamic = dynamic, cells = chosen, summaries = summaries, cohorts = cohorts,
       annotation = annotation, donor_availability = people[participant_role == "control"])
}

sfd_timing_audit <- function(assignment) {
  data.table::rbindlist(lapply(c("standard_decade", "alternative_decade"), function(timing) {
    a <- data.table::copy(assignment[role == "target"])
    a[, `:=`(timing = timing, assigned_decade = sfd_decade(first_exposure_year, timing))]
    a[, `:=`(reference_decade = assigned_decade - 10L,
      exposed_years_opening_block = pmax(0, pmin(10, assigned_decade + 10 - first_exposure_year)),
      exposed_years_reference_block = pmax(0, pmin(10, assigned_decade - first_exposure_year)))]
    a
  }))
}
