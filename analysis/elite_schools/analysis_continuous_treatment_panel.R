###############################################################################
# Continuous-treatment panel regression: outcome on cumulative high-access
# seats in the county.
#
# Treatment_i,t = sum over high-access schools s in county i of
#                 capacity_s(t - founding_s)
#   where capacity_s(0) = 0, ramps linearly to year10_seats over the first
#   10 years, then to year20_seats by year 20, then to year30_seats by year 30,
#   then flat thereafter. (Schools missing a timepoint use linear interp/extrap
#   between available points, or the most recent observed value.)
#
# Spec: feols(outcome ~ seats_in_county | GEOID + year, cluster = ~ GEOID)
# Outcomes: n_amws, amws_per_1000_pop, n_stem, stem_per_1000_pop, population
# Panel:    all US counties 1850-1920, contaminated counties dropped.
# Treated: any county with at least one high-access school (incl. pre-1860)
#          gets a positive seats series; never-treated counties have seats = 0.
###############################################################################
suppressPackageStartupMessages({
  library(data.table); library(fixest); library(ggplot2)
})
args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repo_root <- if (basename(cwd) == "analysis") dirname(cwd) else if (basename(dirname(cwd)) == "analysis") dirname(dirname(cwd)) else cwd
}
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "analysis", "elite_schools", "amws_timing_helpers.R"))
timing <- elite_timing_config()

# ---- Load school list + enrollment v2 --------------------------------------
s <- fread(file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"))
e <- fread(file.path(SCHOOLS_OUTPUT, "elite_high_schools_enrollment_v2.tsv"))

hi <- s[crit_high_access_strict == "yes",
        .(school, state_abbr, county_geoid, founding_year_used)]
hi <- merge(hi, e[, .(school, state_abbr,
                       year10_seats, year20_seats, year30_seats)],
            by = c("school","state_abbr"), all.x = TRUE)
hi[, GEOID := sprintf("%05d", as.integer(county_geoid))]
cat("high-access schools matched to enrollment:", nrow(hi), "\n")
print(hi[, .(school, GEOID, founding_year_used,
             year10_seats, year20_seats, year30_seats)])

# ---- Build school-level capacity by year ----------------------------------
# For each school s and each year t, capacity_s(t).
cap_for_school <- function(t, treatment_start, y10, y20, y30) {
  age <- t - treatment_start
  out <- rep(0, length(age))
  # piecewise linear
  out[age <= 0] <- 0
  # 0..10: ramp from 0 to y10
  k <- age > 0 & age <= 10
  if (!is.na(y10)) out[k] <- (age[k] / 10) * y10
  # 10..20: y10 -> y20 (if y20 missing, use y10)
  k <- age > 10 & age <= 20
  if (!is.na(y10) && !is.na(y20)) out[k] <- y10 + ((age[k] - 10) / 10) * (y20 - y10)
  else if (!is.na(y10))           out[k] <- y10
  # 20..30: y20 -> y30
  k <- age > 20 & age <= 30
  if (!is.na(y20) && !is.na(y30)) out[k] <- y20 + ((age[k] - 20) / 10) * (y30 - y20)
  else if (!is.na(y20))           out[k] <- y20
  else if (!is.na(y10))           out[k] <- y10
  # 30+: flat at last observed
  k <- age > 30
  last <- if (!is.na(y30)) y30 else if (!is.na(y20)) y20 else if (!is.na(y10)) y10 else 0
  out[k] <- last
  out
}

years <- 1840:1930
school_caps <- rbindlist(lapply(seq_len(nrow(hi)), function(i) {
  r <- hi[i]
  treatment_start <- elite_event_year(r$founding_year_used, timing)
  data.table(GEOID = r$GEOID, school = r$school, year = years,
             cap = cap_for_school(years, treatment_start,
                                  r$year10_seats, r$year20_seats, r$year30_seats))
}))
# Sum across schools per county-year
cnty_caps <- school_caps[, .(seats = sum(cap)), by = .(GEOID, year)]

# ---- Load yearly panel + merge in seats ------------------------------------
p <- fread(file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv"))
p[, GEOID := sprintf("%05d", as.integer(GEOID))]
p <- p[year >= 1850 & year <= 1920]

# Drop contaminated counties? For continuous treatment we keep them in but
# their seats series gets correctly accumulated. The pre-1860 BCC/Boston Latin/
# etc. just give them positive seats from earlier years.
# Caller can comment out contamination filter to include / exclude.

panel <- merge(p, cnty_caps, by = c("GEOID","year"), all.x = TRUE)
panel[is.na(seats), seats := 0]

cat("\n--- Panel summary ---\n")
cat("rows:", nrow(panel), " counties:", uniqueN(panel$GEOID), "\n")
cat("counties with seats > 0 at any year:", uniqueN(panel[seats > 0]$GEOID), "\n")
cat("seats summary:\n"); print(summary(panel$seats))

# ---- Run regressions ------------------------------------------------------
OUTCOMES <- c("n_amws", "amws_per_1000_pop", "amws_per_1000_births",
              "n_stem", "stem_per_1000_pop", "stem_per_1000_births",
              "population")

out_dir <- elite_results_dir(
  TALENT_DETS_DATA_DIR, "continuous_treatment_panel", timing
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

estimates <- list()
for (oc in OUTCOMES) {
  d <- panel[is.finite(get(oc))]
  fit <- tryCatch(
    feols(as.formula(sprintf("%s ~ seats | GEOID + year", oc)),
          data = d, cluster = ~GEOID),
    error = function(e) e)
  if (inherits(fit, "error")) {
    cat("FAIL", oc, ":", conditionMessage(fit), "\n"); next
  }
  ct <- as.data.table(summary(fit)$coeftable, keep.rownames = "term")
  ct[, outcome := oc][, mean_y := mean(d[[oc]])][, n := nrow(d)]
  estimates[[oc]] <- ct
  cat(sprintf("%-25s beta=%.4e  se=%.4e  t=%.2f  meanY=%.3f  N=%d\n",
              oc, ct$Estimate, ct$`Std. Error`,
              ct$`t value`, mean(d[[oc]]), nrow(d)))
}
all_est <- rbindlist(estimates, fill = TRUE)
# Per-1000-seat scaled effect for interpretation
all_est[, beta_per_1000_seats := Estimate * 1000]
all_est[, `:=`(timing_mode = timing$mode, school_age = timing$school_age)]
fwrite(all_est, file.path(out_dir, "continuous_treatment_estimates.csv"))
cat("\nwrote", file.path(out_dir, "continuous_treatment_estimates.csv"), "\n")
