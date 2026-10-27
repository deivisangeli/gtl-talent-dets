suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(script)) normalizePath(file.path(dirname(script), "../.."), winslash = "/") else Sys.getenv("GTL_REPO", getwd())
source(file.path(repo_root, "prep/mandatory_schooling/matching_covariate_helpers.R"))
source(file.path(repo_root, "analysis/mandatory_schooling/annual_synth_matching_helpers.R"))

# Missing components must not become observed zeros; invalid universes fail closed.
x <- data.table(a = c(0, 1, NA, 2), b = c(0, NA, NA, -1))
stopifnot(identical(matching_strict_sum(x, c("a", "b")), c(0, NA_real_, NA_real_, NA_real_)))
stopifnot(identical(matching_ratio(c(0,1,2,-1), c(2,0,1,2), TRUE), c(0,NA_real_,NA_real_,NA_real_)))
m <- CJ(variable = ms_rich_features, year = c(1850L,1860L))[, available := TRUE]
m[variable == "illiteracy" & year == 1860, available := FALSE]
stopifnot(all(is.na(ms_covariate_vintages(m, 1864)$census_year)),
          all(ms_covariate_vintages(m, 1867)$census_year == 1850),
          ms_covariate_vintages(m, 1875)[variable == "illiteracy", census_year] == 1850,
          ms_covariate_vintages(m, 1875)[variable == "manufacturing_intensity", census_year] == 1860)

# Deterministic ties, same-state exclusion, donor-only scaling, and balance.
f <- data.table(id = 1:8, state_id = c(1,1,2,2,3,3,4,4), a = c(0,0,-1,1,-2,2,3,4), b = 1)
mt <- ms_match(1, f, 2:8, c("a","b"), k=3)
stopifnot(mt$ok, identical(mt$selected$id, c(3L,4L,5L)),
          mt$balance[variable == "a", selected_mean] == -2/3,
          isTRUE(all.equal(mt$balance[variable == "a", eligible_sd], sd(f[id > 2, a]))))
shuffled <- ms_match(1, f[8:1], 2:8, c("a","b"), k=3)
stopifnot(identical(mt$selected$id, shuffled$selected$id),
          isTRUE(all.equal(mt$balance, shuffled$balance, check.attributes = FALSE)))
bad <- copy(f); bad[id == 1, b := 2]
stopifnot(grepl("constant_donor_mismatch", ms_match(1,bad,2:8,c("a","b"),k=3)$status))
bad[id == 1, a := NA_real_]
stopifnot(ms_match(1,bad,2:8,c("a","b"),k=3)$status == "missing_target_features")
stopifnot(ms_match(1,f,2:4,c("a","b"),k=3)$status == "insufficient_donors")
pm <- ms_match(3,f,2:8,c("a","b"),k=3)
stopifnot(pm$ok, !any(pm$selected$state_id == 2))
wb <- ms_weighted_balance(mt$balance, f, data.table(id=c(3L,4L,5L),weight=c(.2,.3,.5)))
stopifnot(abs(wb[variable == "a", synthetic] + .9) < 1e-12)

# A deliberately simple fit: flat zero preperiod, known positive treatment.
cohort <- list(years = 1830:1870, clean = 1830:1835)
cohort$panel <- CJ(id=1:4,year=cohort$years)
cohort$panel[, n_amws := as.numeric(id == 4 & year >= 1850)]
fit <- ms_fit(cohort,4,data.table(id=1:3),'n_amws')
stopifnot(fit$summary$zero_pre_fit, is.na(fit$summary$standardized_effect),
          abs(fit$summary$effect_e0_e20 - 1) < 1e-8,
          fit$path[event_time == -15, gap_adjusted] == 0,
          abs(sum(fit$weights$weight) - 1) < 1e-8)
cat("Matching construction, timing, missingness, support, rematching and synthetic-fit checks passed.\n")

# Lag removal, missingness and constant-support failures cannot affect the
# attributes-only specification, including completeness and placebo matching.
a <- copy(f)
for (v in ms_attribute_features) a[, (v) := f$a]
for (v in paste0("outcome_bin", 1:3)) a[, (v) := as.numeric(id == 1)]
am <- ms_match(1, a, 2:8, ms_attribute_features, k=3)
stopifnot(am$ok, all(ms_complete(a, ms_attribute_features)),
  grepl("constant_donor_mismatch", ms_match(1, a, 2:8, c(ms_attribute_features, "outcome_bin1"), k=3)$status))
for (v in paste0("outcome_bin", 1:3)) a[, (v) := c(NA, Inf, rep(-999, 6))]
stopifnot(identical(am$selected, ms_match(1, a, 2:8, ms_attribute_features, k=3)$selected),
  all(ms_complete(a, ms_attribute_features)))
a[, (paste0("outcome_bin", 1:3)) := NULL]
stopifnot(identical(am$selected, ms_match(1, a, 2:8, ms_attribute_features, k=3)$selected),
  all(ms_complete(a, ms_attribute_features)),
  identical(ms_matching_config()$specifications, c("original", "richer")),
  ms_matching_config("attributes_only")$directory == "synth_matching_attributes_only")
cat("Attributes-only lag independence and separate namespace checks passed.\n")
