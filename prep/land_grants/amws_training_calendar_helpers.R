# Calendar-decade balance only; existing release helpers are immutable.
training_calendar_stamp <- "amws_training_50km_calendar_no_reuse_20261014"
training_calendar_grid <- seq(1830L, 1950L, 10L)
training_calendar_config <- function(repo, root) {
  x <- no_reuse_config(repo, root)
  x$no_reuse_output <- x$output; x$no_reuse_results <- x$results
  x$output <- file.path(root, "output", "land_grants", training_calendar_stamp)
  x$results <- file.path(root, "results", "land_grants", "event_studies", training_calendar_stamp)
  x$local <- file.path(Sys.getenv("AMWS_CHECKPOINT_ROOT", unset = file.path(Sys.getenv("USERPROFILE"), ".codex", "amws_checkpoints")), training_calendar_stamp)
  x$companion <- file.path(root, "results", "land_grants", "event_studies", "amws_county_pairs_calendar_balanced_m50_p70", "calendar_appendix_20261005")
  x
}
training_calendar_audit <- function(panel, outcomes, grid = training_calendar_grid) {
  p <- copy(as.data.table(panel))
  training_require(!anyDuplicated(p[, .(GEOID, decade)]), "Duplicate calendar county period")
  all_ids <- unique(p$GEOID)
  p <- p[decade %in% grid]
  p[, usable := is.finite(county_births_estimate) & county_births_estimate > 0]
  for (name in outcomes) p[, usable := usable & is.finite(get(name)) & get(name) >= 0]
  z <- p[, .(observed_decades = .N, usable_decades = sum(usable),
    missing_or_invalid_decades = paste(setdiff(grid, decade[usable]), collapse = ";")), by = GEOID]
  z <- merge(data.table(GEOID = all_ids), z, by = "GEOID", all.x = TRUE)
  z[is.na(observed_decades), `:=`(observed_decades = 0L, usable_decades = 0L, missing_or_invalid_decades = paste(grid, collapse = ";"))]
  z[, included := observed_decades == length(grid) & usable_decades == length(grid)]
  z
}
training_calendar_events <- function(members, site_meta) {
  active <- merge(unique(members[, .(site_id)]), site_meta[, .(site_id, event_id, sample_role)], by = "site_id")
  valid <- active[, .(has_treated = any(sample_role == "treated"), has_control = any(sample_role == "runner_up")), by = event_id]
  active[event_id %in% valid[has_treated & has_control, event_id]]
}
training_calendar_covariance <- function(att) {
  V <- as.matrix(att$V_analytical)
  analytic_sd <- sqrt(pmax(diag(V), 0) / att$n)
  ii <- which(att$group > att$t & is.finite(analytic_sd) & analytic_sd > sqrt(.Machine$double.eps) * 10)
  preV <- V[ii, ii, drop = FALSE]
  rc <- if (length(preV) && all(is.finite(preV))) rcond(preV) else NA_real_
  available <- !is.null(att$W) && !is.null(att$Wpval) && length(att$Wpval) == 1L && is.finite(att$Wpval)
  list(summary = data.table(pre_coefficients = length(ii), covariance_rank = if (length(preV) && all(is.finite(preV))) qr(preV, tol = 1e-10)$rank else 0L,
    reciprocal_condition_number = rc, singular_at_package_threshold = is.finite(rc) && rc <= .Machine$double.eps,
    joint_test_available = available, wald_statistic = if (available) as.numeric(att$W) else NA_real_,
    p_value = if (available) as.numeric(att$Wpval) else NA_real_, rank_tolerance = 1e-10),
    coefficients = data.table(coefficient_index = ii, cohort = att$group[ii], calendar_decade = att$t[ii], estimate = att$att[ii], analytic_se = analytic_sd[ii]),
    covariance = preV)
}
training_calendar_support <- function(att, data_es, grid = seq(-50L, 70L, 10L)) {
  cells <- data.table(cohort = att$group, calendar_decade = att$t, estimate = att$att, se = att$se)
  design <- CJ(cohort = sort(unique(data_es$g[data_es$g > 0])), event_time = grid)
  design[, calendar_decade := cohort + event_time]
  design <- merge(design, cells, by = c("cohort", "calendar_decade"), all.x = TRUE)
  design[, status := fcase(!calendar_decade %in% training_calendar_grid, "outside_calendar_window",
    calendar_decade == cohort - 10L & is.finite(estimate), "universal_base_reference",
    is.finite(estimate), "estimated", default = "unavailable_estimate")]
  design[, supported := status %in% c("estimated", "universal_base_reference")]
  support <- rbindlist(lapply(grid, function(e) {
    gs <- design[event_time == e & supported == TRUE, cohort]
    units <- unique(as.data.table(data_es)[g > 0 & g %in% gs, .(stack_unit_num, event_id, g)])
    data.table(event_time = e, n_cohorts = length(gs), cohort_years = paste(sort(gs), collapse = ";"),
      n_events = uniqueN(units$event_id), n_treated_sites = uniqueN(units$stack_unit_num),
      reference_e0_treated_sites = uniqueN(data_es$stack_unit_num[data_es$g > 0]),
      support_definition = "Underlying dynamic ATT before common e=0 subtraction")
  }))
  list(cells = design, support = support)
}
training_calendar_tests <- function() {
  x <- CJ(GEOID = c("a", "b", "c"), decade = seq(1830L, 1850L, 10L))
  x[, `:=`(n = 0L, county_births_estimate = 10)]
  x <- x[!(GEOID == "b" & decade == 1840L)]; x[GEOID == "c" & decade == 1850L, county_births_estimate := 0]
  y <- training_calendar_audit(x, "n", seq(1830L, 1850L, 10L))
  stopifnot(identical(y[included == TRUE, GEOID], "a"))
  sites <- data.table(site_id = c("t1", "c1", "t2", "c2"), event_id = c(1L,1L,2L,2L), sample_role = c("treated","runner_up","treated","runner_up"))
  stopifnot(setequal(training_calendar_events(data.table(site_id = c("t1","c1","c2")), sites)$site_id, c("t1","c1")))
  toy <- list(V_analytical = matrix(c(1,1,0,1,1,0,0,0,1),3,3), n = 10L,
    group = rep(1880L,3), t = c(1830L,1840L,1900L), att = c(1,1,2), W = NULL, Wpval = NULL)
  d <- training_calendar_covariance(toy)$summary
  stopifnot(d$pre_coefficients == 2L, d$covariance_rank == 1L, !d$joint_test_available)
  toy$se <- rep(1,3)
  z <- training_calendar_support(toy, data.table(g = 1880L, stack_unit_num = 1L, event_id = 1L))
  stopifnot(z$support[event_time == -50L, n_events] == 1L,
    z$cells[event_time == 70L, status] == "unavailable_estimate")
  cat("Calendar tests passed: complete calendar grid, genuine zeros, missing/invalid periods, unsupported event removal, variable event support and singular-test diagnostics.\n")
  invisible(TRUE)
}
