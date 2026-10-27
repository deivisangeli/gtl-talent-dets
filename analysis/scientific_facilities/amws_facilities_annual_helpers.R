# Annual AMWS facilities: exact-year exposure, donor eligibility, and target-only
# aggregation. No estimator is loaded here, so preflight checks remain usable
# when an R dependency cannot load.
sfa_outcomes <- c(
  n_amws = "AMWS scientists born",
  log1p_n_amws = "log(1 + AMWS scientists born)",
  amws_per_100k = "AMWS scientists per 100,000 population",
  amws_per_1000_births = "AMWS scientists per 1,000 estimated births",
  population = "Population (persons)"
)

sfa_settings <- function() list(
  first_year = 1866L, last_year = 1949L, target_cutoff = 1943L,
  pre = 10L, balanced_post = 7L, radius_m = 50000,
  reference = -1L, biters = 1000L, seed = 20261002L
)

sfa_classify <- function(first_exposure_year, last_year = 1949L,
                         target_cutoff = 1943L) {
  data.table::data.table(
    g = ifelse(is.na(first_exposure_year) | first_exposure_year > last_year,
               0L, as.integer(first_exposure_year)),
    role = ifelse(is.na(first_exposure_year) | first_exposure_year > last_year,
                  "unexposed_through_1949",
                  ifelse(first_exposure_year <= target_cutoff,
                         "target", "donor_only"))
  )
}

sfa_controls <- function(g, target_g, year, base_year, control_group) {
  stopifnot(control_group %in% c("nevertreated", "notyettreated"))
  if (control_group == "nevertreated") return(g == 0L)
  g == 0L | (g != target_g & g > max(year, base_year))
}

sfa_read_panel <- function(path, settings) {
  columns <- c("GEOID", "year", "population", "county_births_estimate_year",
               "n_amws_1906_1955_dedup", "n_amws_1986", names(sfa_outcomes))
  p <- data.table::fread(path, select = unique(columns),
                        colClasses = c(GEOID = "character"))
  p[, GEOID := sprintf("%05d", as.integer(GEOID))]
  p[, year := as.integer(year)]
  if (anyNA(p[, .(GEOID, year)]) || anyDuplicated(p[, .(GEOID, year)])) {
    stop("Missing or duplicate county-year keys")
  }
  if (any(!is.finite(p$n_amws)) || any(p$n_amws < 0) ||
      any(p$n_amws != p$n_amws_1906_1955_dedup + p$n_amws_1986)) {
    stop("AMWS counts are invalid or edition components do not reconcile")
  }
  if (any(abs(p$log1p_n_amws - log1p(p$n_amws)) > 1e-8, na.rm = TRUE)) {
    stop("Stored log counts do not reconcile")
  }
  for (outcome in c("amws_per_100k", "amws_per_1000_births")) {
    denominator <- if (outcome == "amws_per_100k") p$population else p$county_births_estimate_year
    multiplier <- if (outcome == "amws_per_100k") 100000 else 1000
    usable <- is.finite(denominator) & denominator > 0 & is.finite(p[[outcome]])
    if (any(abs(p[[outcome]][usable] - multiplier * p$n_amws[usable] /
                denominator[usable]) > 1e-8)) stop("Stored annual rate does not reconcile: ", outcome)
  }
  p[year >= settings$first_year & year <= settings$last_year]
}

sfa_exposure <- function(facility_path, county_path, settings) {
  f <- data.table::fread(facility_path, sep = ";", encoding = "Latin-1")
  f[, year := as.integer(year)]
  f[, valid_location := is.finite(lon) & is.finite(lat) &
      lon >= -180 & lon <= 180 & lat >= -90 & lat <= 90]
  f[, eligible_target := !is.na(year) & year <= settings$target_cutoff & valid_location]
  usable <- f[!is.na(year) & valid_location]
  if (!nrow(usable)) stop("No facilities with usable dates and coordinates")
  counties <- sf::st_read(county_path, quiet = TRUE)
  counties <- counties[as.integer(substr(counties$GEOID, 1, 2)) <= 56,
                       c("GEOID", "NAME")]
  counties <- sf::st_transform(counties, 3857)
  buffers <- sf::st_buffer(sf::st_transform(
    sf::st_as_sf(as.data.frame(usable), coords = c("lon", "lat"), crs = 4326),
    3857), dist = settings$radius_m)
  links <- data.table::as.data.table(sf::st_drop_geometry(sf::st_join(
    counties, buffers[, c("facility", "year")], join = sf::st_intersects)))
  links <- links[!is.na(year)]
  first <- links[, .(first_exposure_year = min(year),
                     first_facilities = paste(sort(unique(facility[year == min(year)])),
                                              collapse = ";")), by = GEOID]
  assignment <- merge(data.table::as.data.table(sf::st_drop_geometry(counties)),
                      first, by = "GEOID", all.x = TRUE)
  assignment <- cbind(assignment, sfa_classify(assignment$first_exposure_year,
                                              settings$last_year, settings$target_cutoff))
  list(facilities = f, links = links, assignment = assignment)
}

sfa_model_data <- function(panel, outcome) {
  valid <- is.finite(panel[[outcome]])
  if (outcome == "population") valid <- valid & panel$population > 0
  if (outcome == "amws_per_100k") valid <- valid & is.finite(panel$population) & panel$population > 0
  if (outcome == "amws_per_1000_births") valid <- valid &
    is.finite(panel$county_births_estimate_year) & panel$county_births_estimate_year > 0
  d <- panel[valid, .(id = as.integer(GEOID), GEOID, year, g, y = get(outcome))]
  if (anyDuplicated(d[, .(id, year)])) stop("Duplicate model keys")
  list(data = d, exclusions = panel[!valid, .(GEOID, year, g, outcome = outcome,
                                             reason = "missing_outcome_or_invalid_denominator")])
}

sfa_support <- function(d, target_cohorts, grid, control_group, settings) {
  periods <- split(d, d$year)
  empty <- d[0]
  rows <- vector("list", length(target_cohorts) * length(grid))
  k <- 0L
  for (gg in target_cohorts) for (ee in grid) {
    k <- k + 1L
    base_year <- gg + settings$reference
    comparison_year <- gg + ee
    b <- periods[[as.character(base_year)]]
    t <- periods[[as.character(comparison_year)]]
    if (is.null(b)) b <- empty
    if (is.null(t)) t <- empty
    tb <- b[g == gg, id]
    tp <- t[g == gg, id]
    # Use a scalar with a distinct name: inside b[], `year` is the reference
    # observations' column and must not shadow the intended comparison year.
    cb <- b[sfa_controls(g, gg, comparison_year, base_year, control_group), id]
    cp <- t[sfa_controls(g, gg, comparison_year, base_year, control_group), id]
    boundary <- if (control_group == "nevertreated") settings$last_year else max(comparison_year, base_year)
    donor_dates <- c(b[id %in% cb & g > 0L, g], t[id %in% cp & g > 0L, g])
    if (length(donor_dates) && any(donor_dates <= boundary)) stop("Exposed annual donor admitted")
    status <- if (comparison_year < settings$first_year || comparison_year > settings$last_year) {
      "outside_calendar_window"
    } else if (!length(tb) || !length(tp)) {
      "missing_treated_observations"
    } else if (!length(cb) || !length(cp)) {
      "missing_unexposed_controls"
    } else if (ee == settings$reference) "reference" else "available"
    rows[[k]] <- data.table::data.table(
      g = gg, year = comparison_year, event_time = ee, base_year = base_year,
      treated_base_counties = length(tb), treated_year_counties = length(tp),
      treated_paired_counties = length(intersect(tb, tp)),
      control_base_counties = length(cb), control_year_counties = length(cp),
      control_paired_counties = length(intersect(cb, cp)),
      control_exposure_boundary = boundary,
      earliest_control_exposure = if (length(donor_dates)) min(donor_dates) else NA_integer_,
      status = status
    )
  }
  data.table::rbindlist(rows)
}

sfa_fit <- function(d, control_group, settings) {
  warnings <- character()
  set.seed(settings$seed)
  model <- withCallingHandlers(did::att_gt(
    yname = "y", tname = "year", idname = "id", gname = "g",
    data = as.data.frame(d), xformla = ~1, panel = TRUE,
    allow_unbalanced_panel = TRUE, control_group = control_group,
    est_method = "dr", anticipation = 0L, base_period = "universal",
    bstrap = TRUE, biters = settings$biters, cband = FALSE,
    clustervars = "id", alp = 0.05
  ), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(model = model, warnings = unique(warnings))
}

sfa_cells <- function(model, targets, grid, support) {
  cells <- data.table::data.table(g = as.integer(model$group), year = as.integer(model$t),
                                att = model$att, se = model$se)
  cells[, event_time := year - g]
  cells <- cells[g %in% targets & event_time %in% grid]
  if (anyDuplicated(cells[, .(g, event_time)])) stop("Duplicate ATT cells")
  # did may mark exactly zero variance as NA; a genuinely zero influence
  # column distinguishes it from absent inference.
  positions <- which(model$group %in% targets & (model$t - model$group) %in% grid)
  zero <- vapply(positions, function(j) all(is.finite(model$inffunc[, j])) &&
                   all(abs(model$inffunc[, j]) < 1e-12), logical(1L))
  zero_keys <- paste(model$group[positions[zero]], model$t[positions[zero]])
  cells[paste(g, year) %in% zero_keys & is.finite(att), se := 0]
  cells <- merge(support, cells[, .(g, event_time, att, se)],
                 by = c("g", "event_time"), all.x = TRUE)
  cells[, estimable := status %in% c("available", "reference") & is.finite(att) & is.finite(se)]
  if (any(abs(cells[event_time == -1L & estimable == TRUE, att]) > 1e-10)) stop("Invalid reference")
  cells[event_time == -1L & estimable == TRUE, `:=`(att = 0, se = 0)]
  cells[, `:=`(ci_low = att - qnorm(0.975) * se,
               ci_high = att + qnorm(0.975) * se)]
  cells
}

sfa_balanced_cohorts <- function(cells, grid = -10:7) {
  cells[event_time %in% grid,
        .(complete = .N == length(grid) && all(estimable) && setequal(event_time, grid)),
        by = g][complete == TRUE, g]
}

sfa_mask <- function(model, cells, cohorts, grid) {
  keys <- cells[g %in% cohorts & event_time %in% grid & estimable == TRUE,
                paste(g, year)]
  selected <- paste(model$group, model$t) %in% keys
  masked <- model
  masked$att[!selected] <- NA_real_
  list(model = masked, selected = selected)
}

sfa_manual_dynamic <- function(cells, weights, cohorts, grid) {
  selected <- merge(cells[g %in% cohorts & event_time %in% grid & estimable == TRUE],
                    weights, by = "g")
  selected[, cohort_weight := n_counties / sum(n_counties), by = event_time]
  pooled <- selected[, .(
    manual_att = sum(att * cohort_weight), n_cohorts = .N,
    cohort_years = paste(sort(g), collapse = ";"),
    cohort_counties = sum(n_counties),
    treated_year_counties = sum(treated_year_counties),
    treated_paired_counties = sum(treated_paired_counties),
    min_control_year_counties = min(control_year_counties),
    min_control_paired_counties = min(control_paired_counties)
  ), by = event_time]
  list(pooled = pooled, cells = selected)
}

sfa_aggregate <- function(model, cells, weights, balance, settings) {
  grid <- if (balance == "balanced") -settings$pre:settings$balanced_post else
    seq.int(-settings$pre, settings$last_year - min(weights$g))
  cohorts <- if (balance == "balanced") sfa_balanced_cohorts(cells, grid) else weights$g
  if (!length(cohorts)) stop("No cohorts with the requested support for ", balance)
  mask <- sfa_mask(model, cells, cohorts, grid)
  set.seed(settings$seed + ifelse(balance == "balanced", 1L, 2L))
  # Mask a copy of ATT cells, retaining the full joint influence functions and
  # DIDparams so did accounts for both covariance and estimated cohort shares.
  agg <- did::aggte(mask$model, type = "dynamic", min_e = min(grid), max_e = max(grid),
                    na.rm = TRUE, bstrap = TRUE, biters = settings$biters,
                    cband = FALSE, clustervars = "id", alp = 0.05)
  dynamic <- data.table::data.table(event_time = as.integer(agg$egt),
                                    estimate = agg$att.egt, se = agg$se.egt)
  inf <- as.matrix(agg$inf.function$dynamic.inf.func.e)
  zero <- apply(inf, 2, function(x) all(is.finite(x)) && all(abs(x) < 1e-12))
  dynamic[zero & is.finite(estimate), se := 0]
  manual <- sfa_manual_dynamic(cells, weights, cohorts, grid)
  dynamic <- merge(data.table::data.table(event_time = grid), dynamic,
                   by = "event_time", all.x = TRUE)
  dynamic <- merge(dynamic, manual$pooled, by = "event_time", all.x = TRUE)
  if (!identical(is.finite(dynamic$estimate), is.finite(dynamic$manual_att)) ||
      any(abs(dynamic$estimate - dynamic$manual_att) > 1e-8, na.rm = TRUE)) {
    stop("did aggregation does not match unique-county cohort-size weighting")
  }
  if (any(is.finite(dynamic$estimate) & !is.finite(dynamic$se))) stop("Missing dynamic inference")
  if (balance == "balanced" &&
      (!all(is.finite(dynamic$estimate)) || any(dynamic$n_cohorts != length(cohorts)))) {
    stop("Balanced curve changed cohort composition or lost an event year")
  }
  dynamic[, `:=`(ci_low = estimate - qnorm(0.975) * se,
                 ci_high = estimate + qnorm(0.975) * se,
                 status = ifelse(is.finite(estimate), "estimated", "not_estimable"))]
  dynamic[event_time == settings$reference & is.finite(estimate), status := "reference"]
  # Summary parameters weight group-time cells by cohort size, matching the
  # former simple ATT while explicitly masking every out-of-window cell.
  horizons <- unique(c(max(grid), settings$balanced_post))
  summaries <- lapply(horizons, function(horizon) {
    post <- sfa_mask(model, cells, cohorts, seq.int(0L, horizon))
    simple <- did::aggte(post$model, type = "simple", min_e = 0, max_e = horizon,
                         na.rm = TRUE, bstrap = TRUE, biters = settings$biters,
                         cband = FALSE, clustervars = "id", alp = 0.05)
    selected <- merge(cells[g %in% cohorts & event_time >= 0 &
                              event_time <= horizon & estimable == TRUE], weights, by = "g")
    expected <- weighted.mean(selected$att, selected$n_counties)
    if (abs(simple$overall.att - expected) > 1e-8) stop("Post-summary weighting mismatch")
    se <- simple$overall.se
    if (!is.finite(se) && all(abs(simple$inf.function$simple.att) < 1e-12)) se <- 0
    if (!is.finite(se)) stop("Missing post-treatment inference")
    data.table::data.table(post_min = 0L, post_max = horizon,
      summary_weighting = "cohort_size_weighted_group_time_cells",
      estimate = simple$overall.att, se = se,
      ci_low = simple$overall.att - qnorm(0.975) * se,
      ci_high = simple$overall.att + qnorm(0.975) * se,
      p_value = if (se > 0) 2 * pnorm(-abs(simple$overall.att / se)) else
        ifelse(simple$overall.att == 0, 1, 0),
      n_cohort_time_cells = nrow(selected), n_cohorts = data.table::uniqueN(selected$g))
  })
  list(dynamic = dynamic, cells = manual$cells,
       summaries = data.table::rbindlist(summaries), cohorts = cohorts)
}

sfa_annotation <- function(participants, baseline) {
  baseline <- unique(baseline, by = c("id", "base_year"))
  if (!nrow(baseline) || any(!is.finite(baseline$y))) stop("Invalid baseline annotation data")
  summary <- data.table::data.table(
    n_treated_counties = data.table::uniqueN(participants[participant_role == "treated", id]),
    n_control_counties = data.table::uniqueN(participants[participant_role == "control", id]),
    baseline_mean = mean(baseline$y), baseline_observations = nrow(baseline),
    baseline_counties = data.table::uniqueN(baseline$id),
    reference_periods = paste(sort(unique(baseline$base_year)), collapse = ";"),
    baseline_weighting = "equal_weight_unique_county_reference_observations")
  list(summary = summary, baseline = baseline, participants = participants)
}

# Reconstruct actual estimation observations, including paired selection when
# did retained a balanced panel. Compact county-role exports avoid repeating
# the same donor for every event-time cell.
sfa_annual_annotation <- function(d, cells, control_group, paired = FALSE) {
  periods <- split(d, d$year)
  people <- list(); baselines <- list()
  for (j in seq_len(nrow(cells))) {
    gg <- cells$g[j]; yy <- cells$year[j]; bb <- cells$base_year[j]
    b <- data.table::copy(periods[[as.character(bb)]])
    t <- data.table::copy(periods[[as.character(yy)]])
    b <- b[g == gg | sfa_controls(g, gg, yy, bb, control_group)]
    t <- t[g == gg | sfa_controls(g, gg, yy, bb, control_group)]
    if (paired) {
      ids <- intersect(b$id, t$id); b <- b[id %in% ids]; t <- t[id %in% ids]
    }
    u <- unique(data.table::rbindlist(list(b, t)), by = "id")
    people[[j]] <- u[, .(id, GEOID,
      participant_role = ifelse(g == gg, "treated", "control"),
      g = gg, event_time = cells$event_time[j])]
    baselines[[j]] <- b[, .(id, GEOID, base_year = bb, y)]
  }
  participants <- data.table::rbindlist(people)[,
    .(n_comparison_cells = .N), by = .(id, GEOID, participant_role)]
  sfa_annotation(participants, data.table::rbindlist(baselines))
}

sfa_plot <- function(dynamic, outcome, control_group, balance, annotation = NULL,
                     frequency = "Annual", timing = NULL, reference = -1L,
                     outcome_label = sfa_outcomes[[outcome]], target_cutoff = 1943L,
                     last_year = 1949L) {
  label <- if (control_group == "nevertreated") "Never-treated controls" else "Not-yet-treated controls"
  p <- ggplot2::ggplot(dynamic, ggplot2::aes(event_time, estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_low, ymax = ci_high,
                                     color = event_time >= 0), width = 0.2, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(color = event_time >= 0), size = 1.5, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("FALSE" = "#B25445", "TRUE" = "#267B8C"),
                                labels = c("Pre", "Post"), name = NULL) +
    ggplot2::labs(title = paste(frequency, "AMWS facilities:", balance),
      subtitle = paste(label, if (!is.null(timing)) paste("|", timing) else "",
                       paste0("| Events before ", target_cutoff + 1L,
                              "; outcomes through ", last_year)),
      x = "Birth year relative to assigned first county exposure", y = outcome_label,
      caption = paste0("Reference: ", reference, ". Pointwise 95% intervals; county-clustered inference.\n",
        "County counts are distinct contributors across displayed comparisons; roles can overlap.\n",
        "Baseline pools unique county-reference observations. Population is annually interpolated.",
        if (identical(timing, "alternative_decade"))
          " Shifted references may include exposed years." else "")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom", plot.caption = ggplot2::element_text(hjust = 0))
  if (!is.null(annotation)) {
    fmt <- function(x, digits = 0L) formatC(x, format = "f", digits = digits, big.mark = ",")
    box <- paste0("Treated counties: ", fmt(annotation$n_treated_counties),
      "\nControl counties: ", fmt(annotation$n_control_counties),
      "\nBaseline mean (pooled): ", fmt(annotation$baseline_mean,
        if (outcome == "population") 1L else 3L))
    yr <- range(c(0, dynamic$ci_low, dynamic$ci_high), na.rm = TRUE)
    span <- max(diff(yr), 1e-6)
    p <- p + ggplot2::annotate("label", x = Inf, y = Inf, label = box,
      hjust = 1.03, vjust = 1.05, size = 3.4, fill = "#F4F5F6", color = "#222222",
      linewidth = 0.25) + ggplot2::coord_cartesian(
        ylim = c(yr[1] - .06 * span, yr[2] + .45 * span))
  }
  p
}
