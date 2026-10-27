# County birth-cohort C&S DiD. No file writes or estimation occur on sourcing.
bw_config <- function() list(
  years = 1853:1903, ages = 40:4, target_max_infestation = 1907L,
  exposure_max_age = 18L, reference_age = 19L,
  seed = 20261025L, biters = 1000L, outcome = "amws_per_1000_births"
)

bw_require <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

bw_coordinates <- function(infestation_year, birth_year, max_age = 18L) {
  data.table::data.table(g = as.integer(infestation_year - max_age),
    age_at_infestation = as.integer(infestation_year - birth_year),
    event_time = as.integer(birth_year - (infestation_year - max_age)),
    base_year = as.integer(infestation_year - max_age - 1L))
}

bw_read_sample <- function(data_root, config = bw_config()) {
  paths <- c(treatment = file.path(data_root, "output/boll_weevil/boll_weevil_counties_1920.csv"),
             panel = file.path(data_root, "output/us_panel_county_amws_combined_year.csv"))
  bw_require(all(file.exists(paths)), "Missing treatment or AMWS annual panel input.")
  treatment <- data.table::fread(paths["treatment"], colClasses = c(GEOID = "character"))
  columns <- c("GEOID", "year", "n_amws", "n_amws_1906_1955_dedup", "n_amws_1986",
               "county_births_estimate_year", "amws_per_1000_births", "population", "population_source")
  panel <- data.table::fread(paths["panel"], select = columns, colClasses = c(GEOID = "character"))
  bw_require(!anyDuplicated(panel[, .(GEOID, year)]), "Duplicate county-birth-year key.")
  bw_require(!anyDuplicated(treatment$source_county_id), "Duplicate treatment county key.")
  bw_require(all(treatment$county_geography_year == 1920L), "Treatment geography is not 1920.")
  panel <- panel[year %in% config$years]
  panel[, valid := is.finite(amws_per_1000_births) & is.finite(county_births_estimate_year) &
    county_births_estimate_year > 0 & is.finite(n_amws) & n_amws >= 0 & n_amws == floor(n_amws)]
  bw_require(all(panel$n_amws == panel$n_amws_1906_1955_dedup + panel$n_amws_1986),
             "Combined AMWS numerator does not reconcile to edition components.")
  errors <- panel[valid == TRUE, abs(amws_per_1000_births - 1000 * n_amws / county_births_estimate_year)]
  bw_require(all(errors < 1e-9), "Stored scientist rates disagree with numerator/denominator.")
  coverage <- panel[, .(observed_years = .N, valid_years = sum(valid)), by = GEOID]
  audit <- merge(treatment, coverage, by = "GEOID", all.x = TRUE, sort = FALSE)
  audit[is.na(observed_years), `:=`(observed_years = 0L, valid_years = 0L)]
  audit[, `:=`(infestation_year = as.integer(bw_average_year),
               g = as.integer(bw_average_year - config$exposure_max_age))]
  audit[, role := data.table::fcase(
    !eligible_baker_2018_rules, "excluded_baker_rules",
    is.na(GEOID) | !geoid_verified, "excluded_unverified_geoid",
    is.na(infestation_year), "excluded_missing_infestation_date",
    valid_years != length(config$years), "excluded_incomplete_common_panel",
    infestation_year <= config$target_max_infestation, "target",
    default = "later_infested"
  )]
  units <- audit[role %in% c("target", "later_infested"),
    .(GEOID, id = as.integer(GEOID), source_county_id, countyname, statename,
      infestation_year, g, role)]
  units[, state_id := as.integer(substr(GEOID, 1L, 2L))]
  data.table::setorder(units, id)
  panel <- merge(panel[valid == TRUE], units, by = "GEOID", sort = FALSE)
  panel[, `:=`(model_y = amws_per_1000_births,
               age_at_infestation = infestation_year - year, event_time = year - g)]
  panel[, valid := NULL]
  data.table::setorder(panel, id, year)
  targets <- units[role == "target"]
  weights <- targets[, .(n_counties = .N), by = .(g, infestation_year)]
  weights[, weight := n_counties / nrow(targets)]
  data.table::setorder(weights, g)
  bw_require(nrow(units) == 450L && nrow(targets) == 86L && nrow(panel) == 22950L,
             "Preflight sample counts changed; review sample_inclusion_audit before estimating.")
  bw_require(all(panel[, .N, by = id]$N == 51L), "Estimation panel is not balanced.")
  bw_require(!anyNA(panel[, .(id, g, model_y)]) && all(units$g > 0), "Invalid estimation fields.")
  list(panel = panel, units = units, targets = targets, audit = audit, weights = weights,
       paths = paths, input_md5 = tools::md5sum(paths), outcome_max_error = max(errors))
}

bw_comparisons <- function(panel, weights, config = bw_config()) {
  units <- unique(panel[, .(id, g, state_id)])
  rows <- data.table::CJ(g = weights$g, age_at_infestation = config$ages)
  rows[, `:=`(infestation_year = g + config$exposure_max_age,
               event_time = config$exposure_max_age - age_at_infestation, base_year = g - 1L)]
  rows[, year := g + event_time]
  rows[, treated_counties := vapply(g, function(gg) sum(units$g == gg), integer(1L))]
  rows[, control_counties := mapply(function(gg, bb, tt)
    sum(units$g != gg & units$g > max(bb, tt)), g, base_year, year)]
  rows[, control_states := mapply(function(gg, bb, tt)
    data.table::uniqueN(units[g != gg & g > max(bb, tt), state_id]), g, base_year, year)]
  rows[, `:=`(treated_base_counties = treated_counties, treated_year_counties = treated_counties,
               control_base_counties = control_counties, control_year_counties = control_counties,
               control_paired_counties = control_counties,
               minimum_control_infestation_year = pmax(base_year, year) + config$exposure_max_age + 1L,
               status = ifelse(age_at_infestation == config$reference_age, "reference", "supported"))]
  bw_require(all(rows$year %in% config$years) && all(rows$base_year %in% config$years),
             "Target comparisons extend beyond common calendar window.")
  bw_require(min(rows$control_counties) == 33L, "Expected minimum control support changed.")
  rows
}

bw_manual_did <- function(panel, g_value, year_value) {
  base <- g_value - 1L
  units <- unique(panel[, .(id, g)])
  treated <- units[g == g_value, id]
  controls <- units[g != g_value & g > max(base, year_value), id]
  bw_require(length(treated) > 0L && length(controls) > 0L, "Unsupported direct DiD comparison.")
  difference <- function(ids) mean(panel[id %in% ids & year == year_value, model_y]) -
    mean(panel[id %in% ids & year == base, model_y])
  difference(treated) - difference(controls)
}

# Used to verify that averaging annual influence functions retains their full
# covariance, including did's estimated group-share weight contributions.
bw_average_influence <- function(influence, ages, selected_ages) {
  index <- match(selected_ages, ages)
  bw_require(!anyNA(index), "Missing annual influence function for requested age.")
  rowMeans(as.matrix(influence)[, index, drop = FALSE])
}

bw_aggregate <- function(fit, sample, config = bw_config()) {
  grid <- config$exposure_max_age - config$ages
  selected <- fit$group %in% sample$weights$g & (fit$t - fit$group) %in% grid
  positions <- which(selected)
  cells <- data.table::data.table(g = fit$group[positions], year = fit$t[positions],
    event_time = fit$t[positions] - fit$group[positions], att = fit$att[positions], se = fit$se[positions])
  cells[, `:=`(infestation_year = g + config$exposure_max_age,
               age_at_infestation = config$exposure_max_age - event_time)]
  zero <- vapply(positions, function(j) all(abs(fit$inffunc[, j]) < 1e-12), logical(1L))
  cells[zero & is.finite(att), se := 0]
  cells <- merge(cells, sample$weights[, .(g, n_counties, weight)], by = "g")
  bw_require(nrow(cells) == nrow(sample$weights) * length(grid) && all(is.finite(cells$att)),
             "Target group-time estimates are missing.")
  bw_require(!anyDuplicated(cells[, .(g, year)]), "Duplicate target ATT cell.")
  reference <- cells[age_at_infestation == config$reference_age]
  bw_require(all(abs(reference$att) < 1e-12), "Universal base is not age 19.")
  masked <- fit
  masked$att[!selected] <- NA_real_
  dynamic <- did::aggte(masked, type = "dynamic", min_e = min(grid), max_e = max(grid),
    na.rm = TRUE, bstrap = TRUE, biters = config$biters, cband = TRUE,
    clustervars = "id", alp = 0.05)
  bw_require(isTRUE(dynamic$DIDparams$cband) && is.finite(dynamic$crit.val.egt),
             "did could not provide genuine simultaneous bands; inspect warnings.")
  influence <- as.matrix(dynamic$inf.function$dynamic.inf.func.e)
  dyn <- data.table::data.table(event_time = as.integer(dynamic$egt), att = dynamic$att.egt,
                                se = dynamic$se.egt)
  zero_dynamic <- apply(influence, 2L, function(x) all(abs(x) < 1e-12))
  dyn[zero_dynamic & is.finite(att), se := 0]
  dyn[, `:=`(age_at_infestation = config$exposure_max_age - event_time,
              target_counties = nrow(sample$targets), target_groups = nrow(sample$weights),
              pointwise_critical_value = qnorm(0.975), simultaneous_critical_value = dynamic$crit.val.egt)]
  dyn[, `:=`(pointwise_low = att - qnorm(0.975) * se, pointwise_high = att + qnorm(0.975) * se,
              simultaneous_low = att - simultaneous_critical_value * se,
              simultaneous_high = att + simultaneous_critical_value * se,
              status = ifelse(age_at_infestation == config$reference_age, "reference", "estimated"))]
  expected <- cells[, .(manual_att = sum(att * weight)), by = age_at_infestation]
  dynamic_validation <- merge(dyn[, .(age_at_infestation, att)], expected, by = "age_at_infestation")
  dynamic_validation[, absolute_error := abs(att - manual_att)]
  bw_require(max(dynamic_validation$absolute_error) < 1e-9, "Dynamic county weighting disagrees with manual average.")
  bw_require(nrow(dyn) == length(config$ages) && all(is.finite(dyn$se)), "Missing dynamic inference.")

  ranges <- list(`4-6` = 4:6, `7-9` = 7:9, `4-9` = 4:9)
  pooled_objects <- list(); pooled_rows <- list(); pooled_if <- list(); pooled_validation <- list()
  for (label in names(ranges)) {
    ages <- ranges[[label]]
    use <- selected & (config$exposure_max_age - (fit$t - fit$group)) %in% ages
    part <- fit
    part$att[!use] <- NA_real_
    agg <- did::aggte(part, type = "simple", na.rm = TRUE, bstrap = TRUE,
      biters = config$biters, cband = FALSE, clustervars = "id", alp = 0.05)
    inf <- as.numeric(agg$inf.function$simple.att)
    manual <- cells[age_at_infestation %in% ages, sum(att * weight / length(ages))]
    expected_if <- bw_average_influence(influence, dyn$age_at_infestation, ages)
    max_if_error <- max(abs(inf - expected_if))
    bw_require(abs(agg$overall.att - manual) < 1e-9 && max_if_error < 1e-8,
               "Pooled ATT or joint influence function disagrees with equal-county/equal-age average.")
    se <- as.numeric(agg$overall.se)
    if (all(abs(inf) < 1e-12)) se <- 0
    bw_require(is.finite(se), "Missing pooled inference.")
    pooled_rows[[label]] <- data.table::data.table(age_window = label, min_age = min(ages),
      max_age = max(ages), att = agg$overall.att, se = se,
      conf_low = agg$overall.att - qnorm(0.975) * se,
      conf_high = agg$overall.att + qnorm(0.975) * se,
      target_counties = nrow(sample$targets), county_age_observations = nrow(sample$targets) * length(ages),
      target_groups = nrow(sample$weights), reference_age = config$reference_age,
      bootstrap_iterations = config$biters, seed = config$seed)
    pooled_validation[[label]] <- data.table::data.table(age_window = label, att = agg$overall.att,
      manual_att = manual, absolute_error = abs(agg$overall.att - manual), max_influence_error = max_if_error)
    pooled_if[[label]] <- inf
    pooled_objects[[label]] <- agg
  }
  weight_export <- data.table::copy(cells[, .(g, infestation_year, age_at_infestation, event_time,
                                             n_counties, dynamic_weight = weight)])
  for (label in names(ranges)) weight_export[, (paste0("pooled_weight_age_", gsub("-", "_", label))) :=
    ifelse(age_at_infestation %in% ranges[[label]], dynamic_weight / length(ranges[[label]]), 0)]
  list(cells = cells, dynamic = dyn, pooled = data.table::rbindlist(pooled_rows),
       dynamic_validation = dynamic_validation, pooled_validation = data.table::rbindlist(pooled_validation),
       weights = weight_export, dynamic_object = dynamic, pooled_objects = pooled_objects,
       dynamic_influence = influence, pooled_influence = do.call(cbind, pooled_if))
}

bw_plot <- function(results, output_dir) {
  figure <- ggplot2::ggplot(results$dynamic, ggplot2::aes(age_at_infestation, att)) +
    ggplot2::annotate("rect", xmin = 4, xmax = 9, ymin = -Inf, ymax = Inf, fill = "#DCEEF7", alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 19, linetype = "dashed", colour = "grey45") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = simultaneous_low, ymax = simultaneous_high),
                        fill = "#245B78", alpha = 0.17) +
    ggplot2::geom_line(colour = "#245B78", linewidth = 0.6) +
    ggplot2::geom_point(colour = "#245B78", size = 1.7) +
    ggplot2::scale_x_reverse(breaks = c(40, 35, 30, 25, 19, 15, 10, 7, 4)) +
    ggplot2::labs(title = "Boll weevil exposure and scientist generation",
      subtitle = "86 early-infested counties; 364 later-infested counties available as controls",
      x = "Age when the boll weevil arrived (county of birth proxies schooling location)",
      y = "Effect on AMWS scientists born\nper 1,000 estimated births",
      caption = "Reference: age 19. Shading highlights ages 4-9. Ribbon: 95% simultaneous bands across reported ages.\nBalanced annual birth-cohort panel, 1853-1903; county-clustered multiplier bootstrap, 1,000 draws.") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                    plot.caption = ggplot2::element_text(hjust = 0),
                    plot.title = ggplot2::element_text(face = "bold"))
  ggplot2::ggsave(file.path(output_dir, "event_study_age_at_infestation.png"), figure,
                  width = 11, height = 6.5, dpi = 220, bg = "white")
  pooled <- data.table::copy(results$pooled)
  pooled[, age_window := factor(age_window, levels = c("4-9", "7-9", "4-6"))]
  forest <- ggplot2::ggplot(pooled, ggplot2::aes(age_window, att)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey45", linetype = "dashed") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf_low, ymax = conf_high), width = 0.15, colour = "#245B78") +
    ggplot2::geom_point(size = 2.8, colour = "#245B78") + ggplot2::coord_flip() +
    ggplot2::labs(title = "Effects for children aged 4-9 at infestation",
      x = "Age window", y = "Effect on AMWS scientists born per 1,000 estimated births",
      caption = "Equal county and equal annual-cohort weights; 95% confidence intervals; reference age 19.") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
  ggplot2::ggsave(file.path(output_dir, "pooled_age_windows.png"), forest,
                  width = 9, height = 4.5, dpi = 220, bg = "white")
}
