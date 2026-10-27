# Does a county's pre-law schooling predict its schooling once the law applies?
#
# One point per county in the 13 states whose first compulsory law passed in
# 1907-1920: x is the mean among the 5 cohorts just too old to be covered, y the
# mean among the first 5 covered cohorts (the "ramp" contrast, window k = 5).
# Simple OLS of y on x, plus a men-weighted fit and one with state fixed effects,
# all with heteroskedasticity-robust standard errors.
#
# Reading the slope: sampling noise in x pulls an OLS slope toward 0, so the
# script also reports the split-half reliability of x and the slope divided by it.
# Pre-law LEVELS are reliable (0.85-0.97 on the 2026-09-22 build); it is county
# before/after DIFFERENCES that are not. For ever_attended the 100% ceiling adds
# mechanical mean reversion. For meets_min much of the spread is between states,
# because law_min runs from 4 to 9 years -- read the state_fe row.
#
# Input:  output/mandatory_schooling/attendance_cohort/county_exposure_contrast.csv
#         output/mandatory_schooling/attendance_cohort/students.parquet  (reliability)
# Output: results/mandatory_schooling/attendance_cohort/
#           pre_post_<outcome>_scatter.png, pre_post_regressions.csv
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/plot_attendance_pre_post_law.R

suppressPackageStartupMessages({
  library("dplyr")
  library("readr")
  library("ggplot2")
  library("fixest")
  library("DBI")
  library("duckdb")
})

repo_root <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  } else {
    Sys.getenv("GTL_REPO", unset = getwd())
  }
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not locate the repo root (no paths.R found)")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
})

data_root <- local({
  env <- new.env()
  suppressWarnings(try(sys.source(file.path(repo_root, "paths.R"), envir = env), silent = TRUE))
  candidates <- c(
    get0("TALENT_DETS_DATA_DIR", envir = env, ifnotfound = ""),
    Sys.getenv("DET_DIR"),
    file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox/gtl_talent_dets")
  )
  hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
  if (!length(hit)) stop("Could not locate the Dropbox data root. Set TALENT_DETS_DATA_DIR or DET_DIR.")
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
})

IN_DIR <- file.path(data_root, "output/mandatory_schooling/attendance_cohort")
OUT_DIR <- file.path(data_root, "results/mandatory_schooling/attendance_cohort")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

WINDOW_K <- 5L
EXPOSED_DEF <- "ramp"

OUTCOMES <- list(
  ever_attended = list(label = "Share ever attended school", pct = TRUE),
  meets_min     = list(label = "Share completing the law's minimum years", pct = TRUE),
  years_hs      = list(label = "Years of schooling (capped at 12)", pct = FALSE)
)

contrast <- read_csv(file.path(IN_DIR, "county_exposure_contrast.csv"), show_col_types = FALSE) %>%
  filter(window_k == WINDOW_K, exposed_def == EXPOSED_DEF, !below_min_cell) %>%
  mutate(n_men = n_unexp + n_exp)

# --------------------------------------------------------------------------- #
# Split-half reliability of the pre-law county mean
# --------------------------------------------------------------------------- #

con <- dbConnect(duckdb::duckdb())
halves <- dbGetQuery(con, sprintf("
  SELECT county_geoid, half, count(*) AS n,
         avg(ever_attended) AS ever_attended, avg(meets_min) AS meets_min, avg(years_hs) AS years_hs
  FROM read_parquet('%s')
  WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL AND in_law_state
    AND rel_cohort BETWEEN %d AND -1
  GROUP BY county_geoid, half",
  gsub("\\\\", "/", file.path(IN_DIR, "students.parquet")), -WINDOW_K))
dbDisconnect(con, shutdown = TRUE)

reliability_of <- function(outcome) {
  w <- inner_join(
    halves %>% filter(half == 0) %>% select(county_geoid, h0 = all_of(outcome)),
    halves %>% filter(half == 1) %>% select(county_geoid, h1 = all_of(outcome)),
    by = "county_geoid") %>%
    semi_join(contrast, by = "county_geoid")
  r <- cor(w$h0, w$h1)
  2 * r / (1 + r)   # Spearman-Brown: reliability of the full-sample mean
}

# --------------------------------------------------------------------------- #
# Regressions and plots
# --------------------------------------------------------------------------- #

results <- list()

for (o in names(OUTCOMES)) {
  d <- contrast %>%
    transmute(county_geoid, statename, n_men,
              x = .data[[paste0("unexp_", o)]], y = .data[[paste0("exp_", o)]]) %>%
    filter(!is.na(x), !is.na(y))

  fits <- list(
    ols          = feols(y ~ x, data = d, vcov = "hetero"),
    weighted     = feols(y ~ x, data = d, weights = ~n_men, vcov = "hetero"),
    state_fe     = feols(y ~ x | statename, data = d, vcov = "hetero")
  )
  rel <- reliability_of(o)

  results[[o]] <- bind_rows(lapply(names(fits), function(s) {
    f <- fits[[s]]
    data.frame(
      outcome = o, specification = s, n_counties = nobs(f),
      intercept = if ("(Intercept)" %in% names(coef(f))) unname(coef(f)["(Intercept)"]) else NA_real_,
      slope = unname(coef(f)["x"]), slope_se = unname(se(f)["x"]),
      r2 = unname(r2(f, "r2")), within_r2 = if (s == "state_fe") unname(r2(f, "wr2")) else NA_real_,
      reliability_x = rel, slope_reliability_corrected = unname(coef(f)["x"]) / rel,
      mean_x = mean(d$x), mean_y = mean(d$y))
  }))

  ols <- results[[o]] %>% filter(specification == "ols")
  fmt <- if (OUTCOMES[[o]]$pct) scales::label_percent(accuracy = 1) else scales::label_number(accuracy = 0.1)
  lims <- range(c(d$x, d$y))

  p <- ggplot(d, aes(x, y)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
    geom_point(aes(size = n_men, colour = statename), alpha = 0.55, stroke = 0) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "black", fill = "grey70", linewidth = 0.8) +
    annotate("label", x = lims[1], y = lims[2], hjust = 0, vjust = 1, size = 3.4,
             fill = "white",
             label = sprintf("y = %.3f + %.3f x   (robust SE %.3f)\nR² = %.3f   N = %d counties\nreliability of x = %.2f  ->  corrected slope %.2f",
                             ols$intercept, ols$slope, ols$slope_se, ols$r2, ols$n_counties,
                             ols$reliability_x, ols$slope_reliability_corrected)) +
    scale_x_continuous(labels = fmt) +
    scale_y_continuous(labels = fmt) +
    scale_size_area(max_size = 5, name = "Men") +
    # Limits on the coordinate system, not the scales, so no county or band is dropped.
    coord_equal(xlim = lims, ylim = lims) +
    labs(x = sprintf("Before the law: %d cohorts just too old to be covered", WINDOW_K),
         y = sprintf("After the law: first %d covered cohorts", WINDOW_K),
         colour = "State",
         title = OUTCOMES[[o]]$label,
         subtitle = "Counties in the 13 states with a first compulsory law in 1907-1920; white US-born non-migrant men born 1890-1920.\nDashed line: 45 degrees. Solid line: OLS fit with 95% band.") +
    theme_minimal(base_size = 11) +
    theme(plot.subtitle = element_text(size = 8.5, colour = "grey30"),
          legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 8))

  ggsave(file.path(OUT_DIR, sprintf("pre_post_%s_scatter.png", o)), p,
         width = 9, height = 7.5, dpi = 200, bg = "white")
}

results <- bind_rows(results)
write_csv(results, file.path(OUT_DIR, "pre_post_regressions.csv"))

# The OLS line passes through the means, so its fitted value at mean(x) must be
# mean(y). A cheap check that x and y were not swapped or misaligned.
stopifnot(all(with(filter(results, specification == "ols"),
                   abs(intercept + slope * mean_x - mean_y) < 1e-9)))

options(width = 160)
print(as.data.frame(results %>% mutate(across(where(is.numeric), ~ signif(.x, 4)))), row.names = FALSE)
message("Wrote: ", OUT_DIR)
