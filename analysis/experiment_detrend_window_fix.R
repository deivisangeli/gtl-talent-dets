###############################################################################
# Diagnostic experiment: does symmetrizing the pre-period window between
# treated and control counties fix the wild pre-period swing in the trend
# spec for stem_per_1000_pop in Spec A?
#
# Hypothesis: the current detrend_county fits never-treated controls' linear
# trends on the FULL panel (g == 0 makes is_pre = TRUE everywhere) while
# fitting treated counties' trends only on their pre-treatment decades. At
# early calendar decades when controls' rates are far from their long-run
# linear extrapolation, this produces large negative control residuals and
# small treated residuals, so the cohort x event-time DiD picks up a giant
# spurious positive lead.
#
# Test: define an alternative detrend that uses a UNIFORM pre-treatment
# window for everyone (decade < min(g_treated)). If the trend spec stops
# diverging wildly from the plain spec at long pre-period leads, the
# asymmetric-window diagnosis is right.
###############################################################################

rm(list = ls())

source("etwfe_high_access_helpers.R")
suppressPackageStartupMessages(library("ggplot2"))

window_years <- 60L
event_grid   <- seq(-window_years, window_years, by = 10L)

target_spec_env <- Sys.getenv("TARGET_SPEC", unset = "C")
results_dir <- file.path(
  "results", "elite_school_event_studies",
  paste0("experiment_detrend_window_fix_spec_", target_spec_env)
)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Load panel and treatment table (mirroring the production script)
###############################################################################

panel <- read_csv("../prep/output/us_panel_county_stem_1800.csv",
                  show_col_types = FALSE) %>%
  mutate(
    GEOID  = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade = as.integer(decade)
  )

us_births_raw <- read_csv(
  "../prep/input/new_births_total_number_estimated.csv",
  show_col_types = FALSE
) %>%
  filter(geo == "usa")

us_births_decade <- us_births_raw %>%
  select(-geo, -name) %>%
  pivot_longer(everything(), names_to = "year", values_to = "us_births_year") %>%
  mutate(
    year   = as.integer(year),
    decade = (year %/% 10L) * 10L
  ) %>%
  group_by(decade) %>%
  summarise(us_births_in_decade = sum(us_births_year, na.rm = TRUE),
            .groups = "drop")

us_pop_decade <- panel %>%
  group_by(decade) %>%
  summarise(us_pop_decade = sum(population, na.rm = TRUE), .groups = "drop")

panel <- panel %>%
  left_join(us_births_decade, by = "decade") %>%
  left_join(us_pop_decade,    by = "decade") %>%
  mutate(
    stem_per_1000_pop = if_else(
      population > 0,
      1000 * n_stem / population,
      NA_real_
    )
  )

schools <- read_csv("../prep/output/elite_high_schools_core_1800_1930.csv",
                    show_col_types = FALSE) %>%
  mutate(
    county_geoid       = str_pad(as.character(county_geoid), width = 5, side = "left", pad = "0"),
    founding_year_used = as.integer(founding_year_used),
    g_full             = first_full_exposure_decade(founding_year_used, 14L)
  )

county_treatment <- schools %>%
  group_by(county_geoid) %>%
  summarise(
    first_exposure_year     = min(founding_year_used, na.rm = TRUE),
    first_exposure_access   = if_else(
      any(poor_access_historical[founding_year_used == first_exposure_year] == "high"),
      "high", "low"
    ),
    g_any                   = min(g_full, na.rm = TRUE),
    .groups                 = "drop"
  )

target_spec <- Sys.getenv("TARGET_SPEC", unset = "C")
spec_panel <- if (target_spec == "A") {
  build_panel_spec_A(panel, county_treatment)
} else if (target_spec == "B") {
  build_panel_spec_B(panel, county_treatment)
} else {
  build_panel_spec_C(panel, county_treatment)
}
spec_panel <- spec_panel %>% filter(!is.na(stem_per_1000_pop))

cat("Spec ", target_spec, " panel rows: ", nrow(spec_panel), "\n", sep = "")
cat("Treated counties: ", n_distinct(spec_panel$GEOID[spec_panel$g > 0]), "\n", sep = "")
cat("Never-treated counties: ", n_distinct(spec_panel$GEOID[spec_panel$g == 0]), "\n", sep = "")
cat("Min treated cohort g: ", min(spec_panel$g[spec_panel$g > 0]), "\n", sep = "")

###############################################################################
# Detrending variants
###############################################################################

# Variant 1: production (asymmetric window).
# Already implemented as detrend_county() in helpers.

# Variant 2: uniform window. All units (treated and control) detrend on
# decades strictly before the earliest treated cohort.
detrend_county_uniform <- function(data, outcome_var, min_pre_obs = 4L) {
  treated_g <- data$g[data$g > 0L]
  if (length(treated_g) == 0L) stop("no treated units")
  cutoff <- min(treated_g)

  fit_one <- function(df) {
    y_orig <- df[[outcome_var]]
    pre_idx <- which(df$decade < cutoff & !is.na(y_orig))
    if (length(pre_idx) < min_pre_obs) {
      df$y_resid   <- y_orig
      df$detrended <- FALSE
      return(df)
    }
    fit  <- lm(y_orig[pre_idx] ~ df$decade[pre_idx])
    pred <- coef(fit)[1] + coef(fit)[2] * df$decade
    df$y_resid   <- y_orig - pred
    df$detrended <- TRUE
    df
  }
  data %>% group_split(GEOID_num) %>% map_dfr(fit_one)
}

# Variant 5: detrend each unit on its OWN pre-period (decade < g) for
# treated, and for never-treated controls use only the decades before
# their cohort-equivalent g (which we don't have). Skipping; instead try
# variant 2b: drop pre-period restriction altogether and use decade < g
# always; for g==0 controls, define cutoff as the minimum treated cohort g.
detrend_county_per_unit_pre <- function(data, outcome_var, min_pre_obs = 4L) {
  treated_g <- data$g[data$g > 0L]
  if (length(treated_g) == 0L) stop("no treated units")
  default_cutoff <- min(treated_g)

  fit_one <- function(df) {
    y_orig <- df[[outcome_var]]
    cutoff <- if (df$g[1] > 0L) df$g[1] else default_cutoff
    pre_idx <- which(df$decade < cutoff & !is.na(y_orig))
    if (length(pre_idx) < min_pre_obs) {
      df$y_resid   <- y_orig
      df$detrended <- FALSE
      return(df)
    }
    fit  <- lm(y_orig[pre_idx] ~ df$decade[pre_idx])
    pred <- coef(fit)[1] + coef(fit)[2] * df$decade
    df$y_resid   <- y_orig - pred
    df$detrended <- TRUE
    df
  }
  data %>% group_split(GEOID_num) %>% map_dfr(fit_one)
}

# Variant 3: detrend treated only. Controls keep raw outcome.
detrend_county_treated_only <- function(data, outcome_var, min_pre_obs = 4L) {
  fit_one <- function(df) {
    y_orig <- df[[outcome_var]]
    if (df$g[1] == 0L) {
      df$y_resid   <- y_orig
      df$detrended <- FALSE
      return(df)
    }
    pre_idx <- which(df$decade < df$g[1] & !is.na(y_orig))
    if (length(pre_idx) < min_pre_obs) {
      df$y_resid   <- y_orig
      df$detrended <- FALSE
      return(df)
    }
    fit  <- lm(y_orig[pre_idx] ~ df$decade[pre_idx])
    pred <- coef(fit)[1] + coef(fit)[2] * df$decade
    df$y_resid   <- y_orig - pred
    df$detrended <- TRUE
    df
  }
  data %>% group_split(GEOID_num) %>% map_dfr(fit_one)
}

###############################################################################
# Estimate
###############################################################################

# Apply min_pre_obs filter once, consistently
filtered <- filter_min_pre_obs(spec_panel, "stem_per_1000_pop", 4L)

cat("After min_pre filter rows:", nrow(filtered), "\n")
cat("Treated counties used:",
    n_distinct(filtered$GEOID[filtered$g > 0]), "\n")

run_one <- function(df, outcome_var) {
  run_sa_event(df, outcome_var, window_years, event_grid, ref_event = -10L)
}

dyn_plain <- run_one(filtered, "stem_per_1000_pop") %>%
  mutate(spec = "1. Plain ETWFE (no detrend)")

dyn_current <- run_one(
  detrend_county(filtered, "stem_per_1000_pop", 4L),
  "y_resid"
) %>%
  mutate(spec = "2. Current detrend (asymmetric: controls fit on full panel)")

dyn_uniform <- run_one(
  detrend_county_uniform(filtered, "stem_per_1000_pop", 4L),
  "y_resid"
) %>%
  mutate(spec = "3. Uniform-window detrend (everyone fit on decade < min(g_treated))")

dyn_treated_only <- run_one(
  detrend_county_treated_only(filtered, "stem_per_1000_pop", 4L),
  "y_resid"
) %>%
  mutate(spec = "4. Treated-only detrend (controls keep raw)")

dyn_all <- bind_rows(dyn_plain, dyn_current, dyn_uniform, dyn_treated_only) %>%
  mutate(
    conf_low  = att - 1.96 * se,
    conf_high = att + 1.96 * se
  )

write_csv(dyn_all, file.path(results_dir, "experiment_dyn.csv"))

###############################################################################
# Plot side by side
###############################################################################

p <- ggplot(dyn_all, aes(event_time, att, color = spec)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70", linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ spec, ncol = 2, scales = "fixed") +
  scale_x_continuous(breaks = event_grid) +
  labs(
    x = "Years relative to first fully exposed birth decade",
    y = "ATT, STEM births per 1,000 population"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave(file.path(results_dir, "experiment_detrend_window_fix.png"),
       p, width = 12, height = 8, dpi = 200)

cat("\n=== Pre-period coefficient comparison (event time -60 to -20) ===\n")
print(
  dyn_all %>%
    filter(event_time %in% c(-60, -50, -40, -30, -20, -10, 0, 10, 20)) %>%
    select(spec, event_time, att) %>%
    pivot_wider(names_from = event_time, values_from = att) %>%
    mutate(across(-spec, \(x) round(x, 3)))
)

cat("\nResults saved to: ", results_dir, "\n", sep = "")
