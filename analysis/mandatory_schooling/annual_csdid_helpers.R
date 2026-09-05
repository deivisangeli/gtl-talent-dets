# Annual compulsory-schooling C&S helpers. Keep historical law years unchanged.
# The 14-year allowance is an exposure assumption for birth-cohort outcomes.

annual_outcomes <- c(
  amws_per_1000_pop = "AMWS scientists born per 1,000 population",
  n_amws = "AMWS scientists born",
  log1p_n_amws = "log(1 + AMWS scientists born)",
  amws_per_1000_births = "AMWS scientists born per 1,000 estimated births"
)

annual_profiles <- list(
  pre20 = list(pre = 20L, post = 20L, counties = 322L, jurisdictions = 12L),
  pre40 = list(pre = 40L, post = 20L, counties = 195L, jurisdictions = 9L)
)

read_annual_inputs <- function(data_root) {
  panel_path <- file.path(data_root, "output", "us_panel_county_amws_combined_year.csv")
  laws_path <- file.path(data_root, "input", "compulsory_schooling_laws.csv")
  panel <- data.table::fread(panel_path, colClasses = c(GEOID = "character"))
  laws <- data.table::fread(laws_path, colClasses = c(state_fips = "character"))
  required <- c("GEOID", "year", "population", "population_source",
                "county_births_estimate_year", "n_amws_1906_1955_dedup",
                "n_amws_1986", names(annual_outcomes))
  stopifnot(all(required %in% names(panel)),
            all(c("state_fips", "state_abbr", "state", "compulsory_law_year") %in% names(laws)))
  panel <- panel[, ..required]
  panel[, GEOID := sprintf("%05d", as.integer(GEOID))]
  laws[, state_fips := sprintf("%02d", as.integer(state_fips))]
  stopifnot(!anyDuplicated(panel[, .(GEOID, year)]), !anyDuplicated(laws$state_fips))
  stopifnot(all(panel$n_amws == panel$n_amws_1906_1955_dedup + panel$n_amws_1986),
            all(panel$n_amws >= 0), sum(panel$n_amws) == 112291,
            sum(panel$n_amws_1906_1955_dedup) == 49587,
            sum(panel$n_amws_1986) == 62704,
            all(panel$population_source %in% c("nhgis", "manual", "merged_nyc")))
  panel[, state_fips := substr(GEOID, 1L, 2L)]
  panel <- merge(panel, laws[, .(state_fips, state_abbr, state,
                                g = as.integer(compulsory_law_year))],
                 by = "state_fips", all.x = TRUE, sort = FALSE)
  finite_vars <- c("population", "county_births_estimate_year", names(annual_outcomes))
  panel[, valid_outcomes := Reduce(`&`, lapply(.SD, is.finite)) &
          population > 0 & county_births_estimate_year > 0, .SDcols = finite_vars]
  panel[, `:=`(id = as.integer(GEOID), state_id = as.integer(state_fips),
               event_time = year - g)]
  data.table::setorder(panel, id, year)
  list(panel = panel, paths = c(panel = panel_path, laws = laws_path),
       input_totals = panel[, .(rows = .N, counties = data.table::uniqueN(id),
                               amws_total = sum(n_amws))])
}

build_annual_sample <- function(panel, profile, check_expected = TRUE, anticipation = 14L) {
  grid <- seq.int(-profile$pre, profile$post)
  audit <- panel[, .(
    state_fips = state_fips[1L], state_abbr = state_abbr[1L], state = state[1L],
    g = g[1L], valid_years = sum(valid_outcomes),
    unexposed_years = sum(valid_outcomes & !is.na(event_time) & event_time <= -anticipation - 1L),
    required_years_observed = sum(valid_outcomes & event_time %in% grid),
    missing_event_times = paste(setdiff(grid, event_time[valid_outcomes]), collapse = ";")
  ), by = .(id, GEOID)]
  audit[, target_candidate := !is.na(g) & g >= 1850L & g <= 1880L]
  audit[, balanced_target := target_candidate & required_years_observed == length(grid)]
  audit[, role := data.table::fcase(
    is.na(g), "excluded_missing_law_date",
    balanced_target, "target",
    !is.na(g) & g > 1880L & unexposed_years == 0L, "excluded_no_unexposed_observations",
    !is.na(g) & g > 1880L & valid_years > 0L, "later_adopter",
    target_candidate, "excluded_incomplete_target_window",
    default = "excluded_outside_design"
  )]
  ids <- audit[role %in% c("target", "later_adopter"), id]
  sample <- data.table::copy(panel[id %in% ids & valid_outcomes & year >= 1800L & year <= 1960L])
  sample[, target := id %in% audit[balanced_target == TRUE, id]]
  target_units <- unique(sample[target == TRUE, .(id, GEOID, state_id, state_fips, state_abbr, state, g)])
  if (check_expected) {
    stopifnot(nrow(target_units) == profile$counties,
              data.table::uniqueN(target_units$state_id) == profile$jurisdictions)
  }
  stopifnot(all(sample$g > 0L), !anyNA(sample$g),
            !anyDuplicated(sample[, .(id, year)]))
  window <- sample[target & event_time %in% grid]
  stopifnot(nrow(window) == nrow(target_units) * length(grid))
  weights <- target_units[, .(n_counties = .N,
                              n_jurisdictions = data.table::uniqueN(state_id)), by = g]
  weights[, weight := n_counties / sum(n_counties)]
  data.table::setorder(weights, g)
  list(panel = sample, audit = audit, targets = target_units, weights = weights,
       grid = grid, window = window)
}

annual_detrend <- function(panel, outcome, pre, anticipation = 14L) {
  result <- data.table::copy(panel)
  trend <- result[event_time >= -pre & event_time <= -anticipation - 1L, {
    if (data.table::uniqueN(year) < 3L) stop("Fewer than three unexposed trend-fitting years for cohort ", .BY$g)
    fit <- stats::lm(get(outcome) ~ event_time)
    .(intercept = unname(coef(fit)[1L]), slope = unname(coef(fit)[2L]),
      fitting_rows = .N, fitting_years = data.table::uniqueN(year),
      fitting_min_event = min(event_time), fitting_max_event = max(event_time))
  }, by = g]
  stopifnot(setequal(trend$g, unique(result$g)), all(is.finite(trend$intercept)),
            all(is.finite(trend$slope)), all(trend$fitting_max_event <= -anticipation - 1L))
  result[trend, on = "g", model_y := get(outcome) - i.intercept - i.slope * event_time]
  list(panel = result, trends = trend)
}

# Both observed-period counts and paired counts are reported. did's unbalanced
# panel path uses the available observations in each period; it does not impose
# an additional complete-calendar-panel restriction on the donors.
annual_support <- function(panel, weights, grid, anticipation = 14L) {
  years <- split(panel, panel$year)
  rows <- vector("list", nrow(weights) * length(grid))
  k <- 0L
  for (gg in weights$g) for (ee in grid) {
    k <- k + 1L
    base <- gg - anticipation - 1L
    tt <- gg + ee
    threshold <- max(base, tt) + anticipation
    b <- years[[as.character(base)]]
    t <- years[[as.character(tt)]]
    tb <- b[g == gg]
    tp <- t[g == gg]
    cb <- b[g != gg & g > threshold]
    cp <- t[g != gg & g > threshold]
    paired <- intersect(cb$id, cp$id)
    rows[[k]] <- data.table::data.table(
      g = gg, year = tt, event_time = ee, base_year = base,
      minimum_control_law_year = threshold + 1L,
      treated_base_counties = nrow(tb), treated_year_counties = nrow(tp),
      treated_paired_counties = length(intersect(tb$id, tp$id)),
      treated_states = data.table::uniqueN(tp$state_id),
      control_base_counties = nrow(cb), control_year_counties = nrow(cp),
      control_paired_counties = length(paired),
      control_base_states = data.table::uniqueN(cb$state_id),
      control_year_states = data.table::uniqueN(cp$state_id),
      control_paired_states = data.table::uniqueN(cp[id %in% paired, state_id]),
      control_year_state_abbr = paste(sort(unique(cp$state_abbr)), collapse = ";"),
      status = if (ee == -anticipation - 1L) "reference" else if (
        !nrow(cb) || !nrow(cp)) "missing_unexposed_controls" else if (
        nrow(tb) != weights[g == gg, n_counties] ||
        nrow(tp) != weights[g == gg, n_counties]) "missing_target_observations" else "supported"
    )
  }
  data.table::rbindlist(rows)
}

fit_annual_csdid <- function(panel, anticipation = 14L, biters = 1000L) {
  model_data <- as.data.frame(panel[, .(id, year, g, state_id, model_y)])
  captured <- character()
  fit <- withCallingHandlers(did::att_gt(
    yname = "model_y", tname = "year", idname = "id", gname = "g",
    data = model_data, xformla = ~1, est_method = "dr",
    panel = TRUE, allow_unbalanced_panel = TRUE,
    control_group = "notyettreated", anticipation = anticipation,
    base_period = "universal", clustervars = "state_id",
    bstrap = TRUE, biters = biters, cband = FALSE, alp = 0.05,
    faster_mode = TRUE
  ), warning = function(w) {
    captured <<- c(captured, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  influence_ids <- as.integer(rownames(fit$inffunc))
  units <- unique(panel[, .(id, state_id)])
  expected_clusters <- units$state_id[match(influence_ids, units$id)]
  stopifnot(length(influence_ids) == fit$n, !anyNA(expected_clusters),
            identical(as.integer(fit$DIDparams$cluster_vector), expected_clusters),
            identical(fit$DIDparams$cluster_vector_var, "state_id"))
  list(fit = fit, warnings = unique(captured))
}

# aggte has no public target-cohort argument. Mask non-target ATT cells in a
# COPY of the fitted object, preserving the full sample, joint influence matrix,
# and DIDparams. aggte(na.rm=TRUE) then aggregates just the requested cells and
# includes uncertainty in cohort shares. We verify its weights against unique
# county counts; this guards against observation-count weighting in ragged panels.
aggregate_annual_targets <- function(fit, weights, grid, anticipation = 14L, biters = 1000L) {
  selected <- fit$group %in% weights$g & (fit$t - fit$group) %in% grid
  cells <- data.table::data.table(g = fit$group[selected], year = fit$t[selected],
    event_time = fit$t[selected] - fit$group[selected], att = fit$att[selected],
    se = fit$se[selected])
  all_cells <- data.table::CJ(g = weights$g, event_time = grid)
  cells <- merge(all_cells, cells, by = c("g", "event_time"), all.x = TRUE)
  cells[is.na(year), year := g + event_time]
  # A zero influence function has exactly zero variance, although did reports
  # tiny/zero standard errors as NA. Do not confuse this with missing support.
  positions <- which(selected)
  zero_variance <- vapply(positions, function(j) all(abs(fit$inffunc[, j]) < 1e-12), logical(1L))
  zero_keys <- paste(fit$group[positions[zero_variance]], fit$t[positions[zero_variance]])
  cells[paste(g, year) %in% zero_keys & is.finite(att), se := 0]
  cells[, status := ifelse(is.finite(att), "estimated", "not_estimated")]
  ref <- -anticipation - 1L
  stopifnot(all(cells[event_time == ref, is.finite(att) & abs(att) < 1e-10]))
  cells[event_time == ref, `:=`(att = 0, se = 0, status = "reference")]
  complete_events <- cells[, .(complete = all(is.finite(att))), by = event_time][complete == TRUE, event_time]
  target_fit <- fit
  target_fit$att[!selected | !(fit$t - fit$group) %in% complete_events] <- NA_real_
  agg <- did::aggte(target_fit, type = "dynamic", min_e = min(grid), max_e = max(grid),
                    na.rm = TRUE, bstrap = TRUE, biters = biters, cband = FALSE,
                    clustervars = "state_id", alp = 0.05)
  dynamic <- data.table::data.table(event_time = as.integer(agg$egt),
                                    att = agg$att.egt, se = agg$se.egt)
  influence <- agg$inf.function$dynamic.inf.func.e
  zero_dynamic <- apply(as.matrix(influence), 2L, function(x) all(abs(x) < 1e-12))
  dynamic[zero_dynamic & is.finite(att), se := 0]
  dynamic <- merge(data.table::data.table(event_time = grid), dynamic,
                   by = "event_time", all.x = TRUE)
  dynamic[, status := ifelse(is.finite(att), "estimated", "incomplete_cohort_support")]
  dynamic[event_time == ref, `:=`(att = 0, se = 0, status = "reference")]
  weighted <- merge(cells, weights[, .(g, weight)], by = "g")
  expected <- weighted[, .(manual_att = if (all(is.finite(att))) sum(att * weight) else NA_real_),
                       by = event_time]
  check <- merge(dynamic, expected, by = "event_time")
  stopifnot(all(abs(check$att - check$manual_att) < 1e-8, na.rm = TRUE),
            identical(is.finite(check$att), is.finite(check$manual_att)))
  for (obj in c("cells", "dynamic")) {
    d <- get(obj)
    d[, `:=`(conf_low = att - qnorm(0.975) * se, conf_high = att + qnorm(0.975) * se)]
    if (any(is.finite(d$att) & !is.finite(d$se))) stop("Missing inference for supported ", obj)
  }
  list(dynamic = dynamic, cells = cells, aggregation = agg,
       weighted_validation = check, selected_positions = positions)
}

manual_annual_did <- function(panel, g_value, event, anticipation = 14L) {
  base <- g_value - anticipation - 1L
  tt <- g_value + event
  threshold <- max(base, tt) + anticipation
  means <- vapply(c(base, tt), function(yr) {
    d <- panel[year == yr]
    mean(d[g == g_value, model_y]) - mean(d[g != g_value & g > threshold, model_y])
  }, numeric(1L))
  means[2L] - means[1L]
}
