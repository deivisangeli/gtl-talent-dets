###############################################################################
# Region x birth-year C&S helpers for the compulsory-schooling event studies.
#
# In C&S, region x time effects are parallel trends conditional on region: each
# ATT(g,t) is computed within region cells and averaged over the treated. The
# regions are the CLS South (their region 3) and everything else. Every target
# of the 1850-1880 window is non-Southern -- DC included, which CLS code in the
# Northeast -- so the conditional estimator reduces to comparing the targets
# with NON-SOUTHERN not-yet-treated donors only:
#   * unadjusted, that is exactly region x year fixed effects;
#   * with a population covariate, the outcome regression is fitted within the
#     non-Southern pool, so its slope is region-specific (fully interacted).
#
# Non-Southern donors run out long before Southern ones: a donor must adopt at
# least 15 years after the comparison year, and outside the South the last laws
# are IA/MD (1902), MO (1905), DE (1907) and OK (1908). Late target cohorts are
# therefore identified only to e = last_comparison - g, and the pooled curve is
# RAGGED: each point averages the cohorts identified there.
#
# The published annual and five-year helpers are reused unchanged.
###############################################################################

# CLS region 3. Their coding puts MD and DC in the Northeast, KY and WV in the
# Midwest and OK in the West, so this is narrower than a census South (see
# describe_cls_policy_environment.R).
CLS_SOUTH <- c("AL", "AR", "FL", "GA", "LA", "MS", "NC", "SC", "TN", "TX", "VA")

# Three donor states is the least the state-clustered bootstrap can use. With
# two, a target cohort that is a single state (DC, VT, MA) has an influence
# function summing to zero inside its own cluster, and the two donor clusters'
# sums are exact negatives of each other; the multiplier draws then take only
# the values {-2c, 0, 0, +2c}, so the interquartile SE is 0, 2c or 4c by the
# luck of the draw. It happens at 1890, where only DE and MO remain.
REGION_MIN_DONOR_STATES <- 3L

REGION_LABEL <- "Region x birth-year effects: CLS South / non-South"

region_of <- function(state_abbr) {
  data.table::fifelse(state_abbr %in% CLS_SOUTH, "south", "non_south")
}

# Keep the targets and the donors in the targets' region. Southern donors stay
# in the audit, relabelled, so the control-pool counts report the pool actually
# used and the exclusion stays visible.
region_restrict_sample <- function(sample) {
  target_regions <- unique(region_of(sample$targets$state_abbr))
  if (length(target_regions) != 1L) {
    stop("Targets span several regions (", paste(target_regions, collapse = ", "),
         "); a stratified aggregation would be needed.")
  }
  if (target_regions != "non_south") stop("Region design expects non-Southern targets only.")
  s <- sample
  s$panel <- data.table::copy(sample$panel)[target == TRUE | region_of(state_abbr) == target_regions]
  s$audit <- data.table::copy(sample$audit)
  s$audit[, region := region_of(state_abbr)]
  s$audit[role == "later_adopter" & region != target_regions, role := "excluded_other_region"]
  s$region <- target_regions
  stopifnot(all(s$panel[target == FALSE, !state_abbr %in% CLS_SOUTH]),
            nrow(s$panel[target == TRUE]) == nrow(sample$panel[target == TRUE]),
            setequal(unique(s$panel[target == FALSE, id]),
                     s$audit[role == "later_adopter", id]))
  s
}

region_assignment <- function(audit) {
  audit[!is.na(g), .(counties = .N, g = g[1L], region = region_of(state_abbr[1L]),
                     roles = paste(sort(unique(role)), collapse = "/")),
        by = .(state_fips, state_abbr)][order(g, state_abbr)]
}

# Donor STATES observed in each period of the restricted panel: states with a
# valid row whose law comes more than `anticipation` years later. The county
# analogue of state_donor_counts(); counting counties would let one state's
# many counties pass for a donor pool.
region_donor_counts <- function(panel, anticipation = 14L) {
  periods <- sort(unique(panel$year))
  data.table::data.table(year = periods, donor_states = vapply(periods, function(t)
    data.table::uniqueN(panel[year == t & g > t + anticipation, state_id]), integer(1L)))
}

region_last_comparison <- function(panel, anticipation = 14L,
                                   min_states = REGION_MIN_DONOR_STATES) {
  donors <- region_donor_counts(panel, anticipation)
  supported <- donors[donor_states >= min_states, year]
  if (!length(supported)) stop("No period has ", min_states, " donor states.")
  last <- max(supported)
  # Donor availability only shrinks with calendar time; a gap would mean a
  # supported cell after an unsupported one, which a single cutoff cannot hold.
  stopifnot(all(donors[year <= last, donor_states] >= min_states))
  last
}

# annual_support flags every cell without donors. Cells after the donor horizon
# are relabelled; any OTHER unsupported or thin cell still stops the run.
region_support <- function(panel, weights, grid, anticipation, step, last_comparison) {
  support <- annual_support(panel, weights, grid, anticipation, step)
  support[year > last_comparison & status != "reference", status := "beyond_region_donor_horizon"]
  support
}

region_allowed_support <- c("supported", "reference", "beyond_region_donor_horizon")

region_check_support <- function(support, path) {
  bad <- support[!status %in% region_allowed_support |
                   (status == "supported" & (control_year_states < REGION_MIN_DONOR_STATES |
                                               control_base_states < REGION_MIN_DONOR_STATES))]
  if (nrow(bad)) stop("Unsupported region comparison inside the donor horizon; see ", path)
  invisible(TRUE)
}

region_mark_beyond <- function(cells, last_comparison) {
  cells[year > last_comparison, status := "beyond_region_donor_horizon"]
  stopifnot(all(is.na(cells[status == "beyond_region_donor_horizon", att])),
            all(is.finite(cells[status != "beyond_region_donor_horizon", att])),
            all(is.finite(cells[status != "beyond_region_donor_horizon", se])))
  cells
}

# compact_annual_cohort_fit, except that cells did never produced (the panel is
# cut at the donor horizon) or that lie past `max_year` are NA with zero IF.
region_compact_cohort_fit <- function(fit, gg, grid, warnings, max_year) {
  positions <- match(paste(gg, gg + grid), paste(fit$group, fit$t))
  keep <- !is.na(positions) & gg + grid <= max_year
  inffunc <- matrix(0, fit$n, length(grid))
  inffunc[, keep] <- as.matrix(fit$inffunc[, positions[keep], drop = FALSE])
  att <- rep(NA_real_, length(grid))
  att[keep] <- fit$att[positions[keep]]
  inffunc[, !is.finite(att)] <- 0
  dp <- fit$DIDparams
  list(g = gg, event_time = grid, att = att, inffunc = inffunc, n = fit$n,
    ids = as.integer(rownames(fit$inffunc)), clusters = as.integer(dp$cluster_vector),
    warnings = warnings, max_year = max_year,
    metadata = list(est_method = dp$est_method, xformla = paste(deparse(dp$xformla), collapse = " "),
      anticipation = dp$anticipation, base_period = dp$base_period,
      control_group = dp$control_group, allow_unbalanced_panel = dp$allow_unbalanced_panel,
      processed_rows = nrow(dp$data), min_year = min(dp$data$year), max_year = max(dp$data$year),
      did_version = as.character(packageVersion("did"))))
}

# Ragged generalisation of aggregate_annual_controlled. At each event time the
# cohort weights are renormalised over the cohorts with a finite effect, and the
# cohort-share delta-method term uses that event time's own target share. With
# every cell finite it reduces exactly to the balanced aggregation, draws
# included (test_region_csdid.R checks this).
region_aggregate_controlled <- function(fits, units, weights, grid, seed, biters = 1000L,
                                        ref = -15L) {
  stopifnot(!anyDuplicated(units$id), !anyDuplicated(weights$g),
            setequal(vapply(fits, function(f) f$g, integer(1L)), weights$g),
            sum(weights$n_counties) == sum(units$g %in% weights$g))
  n <- nrow(units); k <- length(grid)
  joint <- matrix(0, n, nrow(weights) * k)
  effects <- matrix(NA_real_, nrow(weights), k)
  cells <- vector("list", nrow(weights))
  for (j in seq_len(nrow(weights))) {
    gg <- weights$g[j]
    f <- fits[[which(vapply(fits, function(z) z$g == gg, logical(1L)))]]
    ids <- match(f$ids, units$id)
    stopifnot(!anyNA(ids), !anyDuplicated(ids), length(ids) == f$n,
      identical(f$clusters, as.integer(units$state_id[ids])),
      identical(as.integer(f$event_time), as.integer(grid)), nrow(f$inffunc) == f$n)
    columns <- (j - 1L) * k + seq_len(k)
    joint[ids, columns] <- f$inffunc * n / f$n
    effects[j, ] <- f$att
    cells[[j]] <- data.table(g = gg, event_time = grid, year = gg + grid, att = f$att)
  }
  finite_cell <- is.finite(effects)
  counts <- matrix(weights$n_counties, nrow(weights), k)
  counts[!finite_cell] <- 0
  n_units <- colSums(counts)
  w_e <- sweep(counts, 2L, pmax(n_units, 1), "/")
  pooled <- colSums(w_e * ifelse(finite_cell, effects, 0))
  pooled[n_units == 0] <- NA_real_
  pooled_if <- matrix(0, n, k)
  for (j in seq_len(nrow(weights))) {
    columns <- (j - 1L) * k + seq_len(k)
    pooled_if <- pooled_if + sweep(joint[, columns, drop = FALSE], 2L, w_e[j, ], "*")
    in_cohort <- units$g == weights$g[j]
    p_target <- n_units / n
    shift <- ifelse(finite_cell[j, ] & n_units > 0, (effects[j, ] - pooled) / p_target, 0)
    pooled_if[in_cohort, ] <- sweep(pooled_if[in_cohort, , drop = FALSE], 2L, shift, "+")
  }
  all_if <- cbind(joint, pooled_if)
  all_att <- c(as.vector(t(effects)), pooled)
  finite <- is.finite(all_att) & apply(all_if, 2L, function(x) all(is.finite(x)))
  se <- rep(NA_real_, length(all_att))
  zero <- finite & vapply(seq_along(all_att), function(j) all(abs(all_if[, j]) < 1e-12), logical(1L))
  se[zero] <- 0
  nonzero <- which(finite & !zero)
  set.seed(seed)
  if (length(nonzero)) {
    params <- list(idname = "id", tname = "year", clustervars = "state_id",
      biters = biters, alp = 0.05, panel = FALSE, allow_unbalanced_panel = TRUE,
      true_repeated_cross_sections = FALSE, cluster_vector = units$state_id)
    se[nonzero] <- did:::mboot(all_if[, nonzero, drop = FALSE], params, return_V = FALSE)$se
  }
  if (any(finite & !is.finite(se))) stop("Missing bootstrap inference for a finite controlled effect.")
  cells <- rbindlist(cells)
  cells[, se := head(se, nrow(cells))]
  dynamic <- data.table(event_time = grid, att = pooled, se = tail(se, k),
                        n_cohorts = as.integer(colSums(finite_cell)), n_units = as.integer(n_units))
  for (d in list(cells, dynamic)) {
    d[, status := fifelse(is.finite(att), "estimated", "incomplete_cohort_support")]
    d[event_time == ref & is.finite(att), `:=`(att = 0, se = 0, status = "reference")]
    d[, `:=`(conf_low = att - qnorm(.975) * se, conf_high = att + qnorm(.975) * se)]
  }
  list(cells = cells, dynamic = dynamic, influence = pooled_if,
       units = units, seed = seed, biters = biters,
       weight_influence_included = TRUE, ragged = TRUE)
}

# Caption line describing the donor restriction and the curve's composition.
region_ragged_note <- function(dynamic, post, last_comparison, unit_word = "counties") {
  at <- function(e) {
    v <- dynamic[event_time == e, n_units]
    if (length(v)) v[1L] else NA_integer_
  }
  sprintf(paste("Region x birth year: Southern (CLS region 3) donors excluded. Fewer than %d",
                "non-Southern donor states after %d, so the pooled estimate is ragged:",
                "%s target %s at e = 0, %s at e = +%d."),
          REGION_MIN_DONOR_STATES, last_comparison,
          format(at(0L), big.mark = ","), unit_word, format(at(post), big.mark = ","), post)
}
