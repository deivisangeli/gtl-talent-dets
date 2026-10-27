# Synthetic checks; no Dropbox data required.
source("analysis/mandatory_schooling/estimate_linked_1940_ba_1950_scientist_iv.R",
       local = TRUE)
stopifnot(identical(ba_completion(c(0L, 15L, 16L, 17L, NA_integer_)),
                    c(0L, 0L, 1L, 1L, NA_integer_)))
stopifnot(identical(ba_completion(school_years(c(180L, 190L, 200L, 230L, 0L, 999L))),
                    c(0L, 1L, 1L, 1L, NA_integer_, NA_integer_)))

people <- expand.grid(bpl = 1:12, yob = 1885:1894, person = 1:30)
people$region <- (people$bpl - 1L) %/% 3L + 1L
people$required <- (people$bpl * 3L + people$yob * 2L) %% 10L
people$law_bin <- law_bin(people$required)
people$school_years <- pmin(17L, 12L + people$required %/% 3L +
                            (people$person * 3L + people$bpl) %% 5L)
people$ba_completed <- ba_completion(people$school_years)
people$scientist <- as.integer((people$person + 5L * people$ba_completed +
                                people$bpl + people$yob) %% 11L == 0L)
people$region_yob <- paste(people$region, people$yob, sep = "_")
people$state_cohort <- paste(people$bpl, people$yob, sep = "_")
for (term in law_terms) people[[term]] <- as.integer(people$law_bin == sub("^z_", "", term))
cells <- aggregate(rep(1L, nrow(people)),
                    people[c("bpl", "yob", "region", "law_bin", "school_years", "scientist")], sum)
names(cells)[names(cells) == "x"] <- "n"
fit <- estimate_models(cells, "fixture", endogenous = "ba_completed")
years_fit <- estimate_models(cells, "fixture")
z <- paste(law_terms, collapse = " + ")
formulas <- list(first = as.formula(paste0("ba_completed ~ ", z, " | bpl + region_yob")),
                 reduced = as.formula(paste0("scientist ~ ", z, " | bpl + region_yob")),
                 iv = as.formula(paste0("scientist ~ 1 | bpl + region_yob | ba_completed ~ ", z)),
                 ols = scientist ~ ba_completed | bpl + region_yob)
for (cluster in c("cell", "state")) {
  cluster_formula <- if (cluster == "cell") ~state_cohort else ~bpl
  for (model in names(formulas)) {
    expanded <- fixest::feols(formulas[[model]], data = people, cluster = cluster_formula,
                              ssc = fixest::ssc(K.adj = FALSE), notes = FALSE)
    grouped <- fit$models[[paste(model, cluster, sep = "_")]]
    stopifnot(isTRUE(all.equal(unname(coef(expanded)), unname(coef(grouped)), tolerance = 1e-9)),
              isTRUE(all.equal(unname(vcov(expanded)), unname(vcov(grouped)), tolerance = 1e-8)))
  }
}
stopifnot(isTRUE(all.equal(fit$tables$reduced_form, years_fit$tables$reduced_form, tolerance = 1e-9)),
          all(fit$tables$diagnostics$observations == nrow(people)),
          all(fit$tables$diagnostics$scientist_count == sum(people$scientist)),
          identical(fit$tables$diagnostics$anderson_rubin_zero_p,
                    years_fit$tables$diagnostics$anderson_rubin_zero_p),
          "effect_per_100k_per_ba_completed" %in% names(fit$tables$estimates),
          !"effect_per_100k_per_school_year" %in% names(fit$tables$estimates),
          "effect_per_100k_per_school_year" %in% names(years_fit$tables$estimates))
stopifnot(isTRUE(all.equal(fit$tables$estimates$effect_per_100k_per_ba_completed,
                           1e5 * fit$tables$estimates$estimate)))
fails <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")
stopifnot(fails(estimate_models(cells, "fixture", "missing_column")),
          fails(estimate_models(transform(cells, school_years = 15L), "fixture", "ba_completed")))
ar <- ar_confidence_set(cells, "fixture", limit = 1, grid_points = 21L, endogenous = "ba_completed")
stopifnot(isTRUE(all.equal(ar$grid$p_value[ar$grid$beta == 0],
  fit$tables$diagnostics$anderson_rubin_zero_p[fit$tables$diagnostics$cluster == "state"],
  tolerance = 1e-9)))

grid <- seq(-1, 1, length.out = 101L)
disjoint_p <- function(b) 0.1 * exp(-((abs(b) - 0.55) / 0.1)^2)
disjoint <- ar_intervals(grid, disjoint_p(grid), disjoint_p, "fixture")
stopifnot(nrow(disjoint) == 2L, disjoint$upper[[1L]] < 0, disjoint$lower[[2L]] > 0,
          max(abs(disjoint_p(c(disjoint$lower, disjoint$upper)) - 0.05)) < 1e-8)
disjoint$lower_boundary_reached <- is.na(disjoint$lower)
disjoint$upper_boundary_reached <- is.na(disjoint$upper)
stopifnot(grepl("\\cup", ba_ar_text(disjoint), fixed = TRUE))
boundary_p <- function(b) 0.05 + 0.03 * b^2 - 0.003
boundary <- ar_intervals(grid, boundary_p(grid), boundary_p, "fixture")
boundary$lower_boundary_reached <- is.na(boundary$lower)
boundary$upper_boundary_reached <- is.na(boundary$upper)
stopifnot(nrow(boundary) == 2L, boundary$lower_boundary_reached[[1L]],
          boundary$upper_boundary_reached[[2L]],
          grepl("lower search boundary", ba_ar_text(boundary), fixed = TRUE),
          grepl("upper search boundary", ba_ar_text(boundary), fixed = TRUE))
empty <- ar_intervals(grid, rep(0.01, length(grid)), function(b) 0.01, "fixture")
stopifnot(nrow(empty) == 0L, grepl("No values accepted", ba_ar_text(empty), fixed = TRUE))
all_accepted <- ar_intervals(grid, rep(0.2, length(grid)), function(b) 0.2, "fixture")
stopifnot(nrow(all_accepted) == 1L, is.na(all_accepted$lower), is.na(all_accepted$upper))
stopifnot(fails(ar_intervals(grid, rep(NA_real_, length(grid)), function(b) NA_real_, "fixture")))
compare_ba_tables(fit$tables$reduced_form, years_fit$tables$reduced_form,
                  c("sample", "model", "cluster", "term"), c("estimate", "se", "p_value"))
bad_rf <- years_fit$tables$reduced_form
bad_rf$estimate[[1L]] <- bad_rf$estimate[[1L]] + 0.01
stopifnot(fails(compare_ba_tables(fit$tables$reduced_form, bad_rf,
                                 c("sample", "model", "cluster", "term"), "estimate")))
message("BA-completion IV: threshold, grouped inference, invariance, AR, and rendering tests passed")
