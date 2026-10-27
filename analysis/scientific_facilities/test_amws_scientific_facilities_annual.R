# Meaningful checks for future-treated donor contamination, unavailable years,
# changing cohort composition, missing observations, and known treatment effects.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
repo_root <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(data.table))
source(file.path(repo_root, "analysis/scientific_facilities/amws_facilities_annual_helpers.R"))
settings <- sfa_settings()
settings$biters <- 199L

dates <- c(1876L, 1943L, 1944L, 1949L, 1950L, NA_integer_)
classified <- sfa_classify(dates)
stopifnot(identical(classified$g, c(1876L, 1943L, 1944L, 1949L, 0L, 0L)),
          identical(classified$role, c("target", "target", "donor_only", "donor_only",
                                       "unexposed_through_1949", "unexposed_through_1949")),
          settings$last_year - min(classified[g > 0 & role == "target", g]) == 73L)

# A donor exposed in 1945 is usable for 1944 but not 1945. For a pre-period,
# it must also be unexposed at the universal reference year.
stopifnot(identical(sfa_controls(c(0L, 1943L, 1944L, 1945L), 1943L, 1944L, 1942L,
                                 "notyettreated"), c(TRUE, FALSE, FALSE, TRUE)),
          identical(sfa_controls(c(0L, 1943L, 1944L, 1945L), 1943L, 1944L, 1942L,
                                 "nevertreated"), c(TRUE, FALSE, FALSE, FALSE)),
          !sfa_controls(1905L, 1908L, 1898L, 1907L, "notyettreated"))

units <- data.table(id = 1:60, g = rep(c(1876L, 1908L, 1943L, 1945L, 0L), each = 12L))
units[, effect := fifelse(g == 1876L, 2, fifelse(g == 1908L, 4,
                                  fifelse(g == 1943L, 8, 1000)))]
d <- merge(CJ(id = units$id, year = 1866:1949), units, by = "id")
d[, y := (year - 1866) / 100 + effect * (g > 0 & year >= g)]
# One county is absent at one pre-year; other counties preserve cohort support.
d <- d[!(id == 1 & year == 1870)]
targets <- c(1876L, 1908L, 1943L)
grid <- -10:73
never <- sfa_support(d, targets, grid, "nevertreated", settings)
notyet <- sfa_support(d, targets, grid, "notyettreated", settings)
stopifnot(never[g == 1943 & event_time == 7, status] == "outside_calendar_window",
          never[g == 1876 & event_time == 73, year] == 1949L,
          never[g == 1876 & event_time == -6, treated_year_counties] == 11L,
          never[g == 1876 & event_time == -6, treated_paired_counties] == 11L,
          all(never$status[never$year > 1949] == "outside_calendar_window"),
          never[g == 1943 & event_time == 0, control_year_counties] == 12L,
          notyet[g == 1943 & event_time == 0, control_year_counties] == 24L,
          notyet[g == 1943 & event_time == 2, control_year_counties] == 12L)

# Pretend ATT(g,t) values with known post effects exercise target selection and
# aggregation arithmetic independently of the estimator dependency.
fake <- list(group = rep(c(targets, 1945L), each = 84L),
             t = rep(1866:1949, times = 4L))
fake$att <- ifelse(fake$t >= fake$group,
                  c(`1876` = 2, `1908` = 4, `1943` = 8, `1945` = 1000)[as.character(fake$group)], 0)
fake$se <- rep(0.1, length(fake$att))
fake$inffunc <- matrix(0.1, nrow = 60L, ncol = length(fake$att))
cells <- sfa_cells(fake, targets, grid, never)
stopifnot(setequal(sfa_balanced_cohorts(cells), c(1876L, 1908L)))
weights <- unique(d[g %in% targets, .(id, g)])[, .(n_counties = .N), by = g]
manual <- sfa_manual_dynamic(cells, weights, targets, grid)$pooled
stopifnot(abs(manual[event_time == 0, manual_att] - 14 / 3) < 1e-12,
          manual[event_time == 7, manual_att] == 3,
          manual[event_time == 73, manual_att] == 2,
          manual[event_time == 0, n_cohorts] == 3L,
          manual[event_time == 7, n_cohorts] == 2L,
          manual[event_time == 73, n_cohorts] == 1L,
          manual[event_time == -1, manual_att] == 0)
masked <- sfa_mask(fake, cells, targets, grid)
stopifnot(all(is.na(masked$model$att[fake$group == 1945L])),
          all(is.na(masked$model$att[fake$t > 1949L])),
          identical(fake$att, ifelse(fake$t >= fake$group,
            c(`1876` = 2, `1908` = 4, `1943` = 8, `1945` = 1000)[as.character(fake$group)], 0)))
missing <- copy(cells)
missing[g == 1908L & event_time == -10L, `:=`(estimable = FALSE, att = NA_real_)]
stopifnot(identical(sfa_balanced_cohorts(missing), 1876L))
bad_ref <- fake
bad_ref$att[bad_ref$t - bad_ref$group == -1L] <- 3
stopifnot(inherits(try(sfa_cells(bad_ref, targets, grid, never), silent = TRUE), "try-error"))

# Outcome zero is observed; a missing value or invalid denominator is excluded
# without creating artificial zero scientists or a complete county panel.
p <- data.table(GEOID = c("01001", "01001", "01003"), year = 1930:1932,
                 g = c(1931L, 1931L, 0L), n_amws = c(0, NA, 2),
                 population = c(100, 100, 0), county_births_estimate_year = c(10, 10, 0),
                 amws_per_100k = c(0, NA, Inf), amws_per_1000_births = c(0, NA, Inf))
counts <- sfa_model_data(p, "n_amws")
rates <- sfa_model_data(p, "amws_per_1000_births")
stopifnot(nrow(counts$data) == 2L, counts$data[year == 1930, y] == 0,
          nrow(rates$data) == 1L, nrow(rates$exclusions) == 2L)
pop <- sfa_model_data(p, "population")
stopifnot(nrow(pop$data) == 2L, all(pop$data$y > 0))
# A county used as a donor repeatedly at the same reference contributes once
# to the pooled baseline, but remains represented at a different reference.
people <- data.table(id = c(1L, 2L), participant_role = c("treated", "control"))
base <- data.table(id = c(1L, 2L, 2L, 2L), base_year = c(1900L, 1900L, 1900L, 1910L),
                   y = c(2, 4, 4, 9))
a <- sfa_annotation(people, base)
stopifnot(a$summary$baseline_observations == 3L,
          a$summary$baseline_mean == 5, a$summary$n_control_counties == 1L)
annotation_cells <- cells[g == 1943 & event_time %in% c(-1L, 0L, 2L) & estimable == TRUE]
d[, GEOID := sprintf("%05d", id)]
a <- sfa_annual_annotation(d, annotation_cells, "notyettreated")
stopifnot(a$summary$n_treated_counties == 12L, a$summary$n_control_counties == 24L,
          !anyDuplicated(a$baseline[, .(id, base_year)]))
cat("Dependency-free timing, donor, support, balance, weighting and missing-outcome tests passed.\n")

load_error <- tryCatch({suppressPackageStartupMessages(library(did)); NA_character_},
                       error = function(e) conditionMessage(e))
if (!is.na(load_error)) {
  cat("Estimator integration tests NOT RUN: ", load_error, "\n", sep = "")
} else {
  for (control in c("nevertreated", "notyettreated")) {
    fitted <- sfa_fit(d, control, settings)$model
    c <- sfa_cells(fitted, targets, grid, if (control == "nevertreated") never else notyet)
    for (balance in c("unbalanced", "balanced")) {
      result <- sfa_aggregate(fitted, c, weights, balance, settings)
      stopifnot(max(abs(result$dynamic$estimate - result$dynamic$manual_att), na.rm = TRUE) < 1e-8,
                max(abs(result$cells$att - ifelse(result$cells$event_time >= 0,
                  c(`1876` = 2, `1908` = 4, `1943` = 8)[as.character(result$cells$g)], 0)),
                  na.rm = TRUE) < 1e-8)
      if (balance == "balanced") stopifnot(setequal(result$cohorts, c(1876L, 1908L)),
                                            all(result$dynamic$n_cohorts == 2L))
    }
  }
  cat("Estimator integration tests passed for both control groups and both balance versions.\n")
  # The 1919 cutoff is exact: 1919 can reach +30 in 1949; 1920 is donor-only.
  # A missing county at +30 must not remove its cohort if other units contribute.
  early_settings <- settings; early_settings$target_cutoff <- 1919L; early_settings$balanced_post <- 30L
  early_class <- sfa_classify(c(1919L, 1920L, 1949L, 1950L), target_cutoff = 1919L)
  stopifnot(identical(early_class$role, c("target", "donor_only", "donor_only", "unexposed_through_1949")))
  early_units <- data.table(id = 1:60, g = rep(c(1876L, 1908L, 1919L, 1920L, 0L), each = 12L))
  early_units[, effect := rep(c(2, 4, 8, 1000, 0), each = 12L)]
  early_d <- merge(CJ(id = early_units$id, year = 1866:1949), early_units, by = "id")
  early_d[, `:=`(GEOID = sprintf("%05d", id),
    y = (year - 1866) / 100 + effect * (g > 0 & year >= g))]
  early_d <- early_d[!(id == 13 & year == 1938)]
  early_targets <- c(1876L, 1908L, 1919L)
  early_weights <- early_units[g %in% early_targets, .(n_counties = .N), by = g]
  for (control in c("nevertreated", "notyettreated")) {
    audit <- sfa_support(early_d, early_targets, grid, control, early_settings)
    stopifnot(audit[g == 1919 & event_time == 30, year] == 1949L,
      all(audit$earliest_control_exposure > audit$control_exposure_boundary, na.rm = TRUE),
      audit[g == 1908 & event_time == 30, treated_year_counties] == 11L)
    fit <- sfa_fit(early_d, control, early_settings)$model
    early_cells <- sfa_cells(fit, early_targets, grid, audit)
    for (balance in c("balanced", "unbalanced")) {
      result <- sfa_aggregate(fit, early_cells, early_weights, balance, early_settings)
      stopifnot(!any(result$cells$g == 1920L),
        all(result$cells$year <= 1949L),
        max(abs(result$cells$att - ifelse(result$cells$event_time >= 0,
          c(`1876` = 2, `1908` = 4, `1919` = 8)[as.character(result$cells$g)], 0))) < 1e-8,
        all(result$dynamic[event_time == -1L, estimate] == 0))
      if (balance == "balanced") stopifnot(setequal(result$cohorts, early_targets),
        all(result$dynamic$n_cohorts == 3L),
        identical(result$dynamic$event_time, -10:30), result$summaries$post_max == 30L)
      else stopifnot(max(result$dynamic$event_time) == 73L,
        setequal(result$summaries$post_max, c(30L, 73L)))
      annotation <- sfa_annual_annotation(early_d, result$cells, control,
        paired = isTRUE(fit$DIDparams$panel))
      stopifnot(!anyDuplicated(annotation$baseline[, .(id, base_year)]),
        annotation$summary$baseline_mean == mean(annotation$baseline$y),
        annotation$summary$n_treated_counties == 36L)
    }
  }
  cat("Pre-1920 cutoff, +30 fixed cohorts, +73 unbalanced support and donor-only effect tests passed.\n")
}
