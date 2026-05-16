###############################################################################
# Compulsory schooling laws and STEM talent production (US counties, 1800-2000)
#
# This script tests whether state-level compulsory attendance laws had any
# effect on county-level STEM talent production — the null hypothesis being
# that broad, non-selective educational access does not drive the production of
# Wikipedia-notable scientists.
#
# The contrast with analysis_elite_school_high_access_estimators_1800.R is
# deliberate: that script finds a positive effect of selective, tuition-free
# elite high schools. This script should find zero or near-zero.
#
# Treatment: decade the state passed its first compulsory attendance law.
#   Source: Lleras-Muney (2002) Table A.1; Acemoglu-Angrist (2000).
#   IMPORTANT: Verify these dates against the published dataset before
#   finalising any results. The CSV in prep/input/compulsory_schooling_laws.csv
#   contains our best reconstruction from memory; spot-check MA (1852),
#   MS (1918), NY (1874), PA (1895), OH (1877) as the most consequential.
#
# Identification: Wooldridge ETWFE / Sun-Abraham estimator at the county level
#   with state-clustered SEs, using the same helpers as the elite school script.
#   All counties in a state are treated simultaneously, so the event study
#   compares early-adopter states to late-adopter states (between-state
#   variation) and within-state variation is zero.
#
# Supplementary: test whether elite school timing is correlated with
#   compulsory law timing (within state), to verify that the two treatments
#   are not confounded.
#
# Output: analysis/results/compulsory_schooling/
#   compulsory_law_event_study.png      -- ETWFE event study
#   compulsory_vs_elite_correlation.png -- scatterplot: law decade vs school decade
#   compulsory_law_summary.csv          -- ATT table
###############################################################################

rm(list = ls())

source("etwfe_high_access_helpers.R")
source("../paths.R")
suppressPackageStartupMessages({
  library("ggplot2")
  library("fixest")
})

initial_time <- Sys.time()
theme_set(theme_minimal(base_size = 13))

window_years  <- 50L
event_grid    <- seq(-window_years, window_years, by = 10L)
results_dir   <- file.path("results", "compulsory_schooling")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Load county STEM panel and population
###############################################################################

panel <- read_csv(file.path(DATA_OUTPUT, "us_panel_county_stem_1800.csv"),
                  show_col_types = FALSE) %>%
  mutate(
    GEOID  = str_pad(as.character(GEOID), 5, "left", "0"),
    decade = as.integer(decade)
  )

pop_panel <- read_csv(file.path(DATA_OUTPUT, "county_population.csv"),
                      show_col_types = FALSE) %>%
  mutate(
    GEOID  = str_pad(as.character(GEOID), 5, "left", "0"),
    decade = as.integer(decade)
  ) %>%
  rename(population_source = source)

panel <- panel %>%
  select(-population) %>%
  left_join(pop_panel, by = c("GEOID", "decade"))

# Estimated county births (national birth rate applied to county population)
us_births_raw <- read_csv(
  file.path(DATA_INPUT, "new_births_total_number_estimated.csv"),
  show_col_types = FALSE
) %>% filter(geo == "usa")

us_births_decade <- us_births_raw %>%
  select(-geo, -name) %>%
  pivot_longer(everything(), names_to = "year", values_to = "b") %>%
  mutate(year = as.integer(year), decade = (year %/% 10L) * 10L) %>%
  group_by(decade) %>%
  summarise(us_births_decade = sum(b, na.rm = TRUE), .groups = "drop")

us_pop_decade <- panel %>%
  group_by(decade) %>%
  summarise(us_pop_decade = sum(population, na.rm = TRUE), .groups = "drop")

panel <- panel %>%
  left_join(us_births_decade, by = "decade") %>%
  left_join(us_pop_decade,    by = "decade") %>%
  mutate(
    births_est        = population * us_births_decade / us_pop_decade,
    stem_per_1000_pop = if_else(population > 0, 1000 * n_stem / population, NA_real_),
    stem_per_1000_births = if_else(births_est > 0, 1000 * n_stem / births_est, NA_real_)
  )

###############################################################################
# Load compulsory schooling law dates; assign treatment decade
###############################################################################

cs_laws <- read_csv(file.path(DATA_INPUT, "compulsory_schooling_laws.csv"),
                    show_col_types = FALSE) %>%
  filter(!is.na(compulsory_law_year)) %>%
  mutate(
    state_fips = str_pad(as.character(state_fips), 2, "left", "0"),
    # Treatment decade: the decade in which the law was passed
    g_compulsory = (compulsory_law_year %/% 10L) * 10L
  ) %>%
  select(state_fips, state_abbr, g_compulsory)

# Merge to panel by state FIPS (first 2 chars of county GEOID)
panel <- panel %>%
  mutate(state_fips = substr(GEOID, 1, 2)) %>%
  left_join(cs_laws, by = "state_fips")

# States without a law in our data (AK, HI, territories) get g = NA → never-treated
panel <- panel %>%
  mutate(
    g_compulsory = if_else(is.na(g_compulsory), 0L, as.integer(g_compulsory))
  )

cat("=== Compulsory schooling treatment distribution ===\n")
panel %>%
  distinct(state_fips, state_abbr, g_compulsory) %>%
  arrange(g_compulsory) %>%
  print(n = 60)

###############################################################################
# Build ETWFE panel for compulsory schooling
#
# Key difference from elite school analysis: ALL counties within a state share
# the same g_compulsory. There is no within-state variation in treatment timing.
# The event study therefore relies on between-state variation.
###############################################################################

# Outcome: stem_per_1000_pop (easier to interpret; use births version in robustness)
outcome <- "stem_per_1000_pop"

# Minimum 3 pre-treatment decades required (same rule as elite school analysis)
min_pre <- 3L

# For ETWFE we need panel to be complete; restrict to 1800-2000
etwfe_panel <- panel %>%
  filter(decade >= 1800, decade <= 2000) %>%
  filter(!is.na(g_compulsory), !is.na(population), population > 0,
         !is.na(stem_per_1000_pop))

# Compute pre-treatment obs per county
pre_counts <- etwfe_panel %>%
  mutate(is_pre = g_compulsory == 0L | decade < g_compulsory) %>%
  group_by(GEOID) %>%
  summarise(n_pre = sum(is_pre), .groups = "drop")

etwfe_panel <- etwfe_panel %>%
  left_join(pre_counts, by = "GEOID") %>%
  filter(n_pre >= min_pre)

cat("\nCounties retained (n_pre >=", min_pre, "):", n_distinct(etwfe_panel$GEOID), "\n")
cat("State-treatment cohorts:\n")
etwfe_panel %>%
  distinct(state_fips, state_abbr, g_compulsory) %>%
  count(g_compulsory) %>%
  print()

###############################################################################
# County-specific linear pre-treatment detrending
###############################################################################

detrended <- detrend_panel(etwfe_panel, outcome_col = outcome,
                           g_col = "g_compulsory", time_col = "decade")
etwfe_panel$y_resid <- detrended

###############################################################################
# Sun-Abraham ETWFE
###############################################################################

ref_event <- -10L

run_compulsory <- function(outcome_col, panel_df, label) {
  dyn <- tryCatch(
    run_sa_event(
      df           = panel_df,
      outcome      = outcome_col,
      g_col        = "g_compulsory",
      time_col     = "decade",
      unit_col     = "GEOID",
      cluster_col  = "state_fips",   # cluster at state level (treatment level)
      ref_event    = ref_event,
      window_lo    = -window_years,
      window_hi    =  window_years
    ),
    error = function(e) { cat("ETWFE failed:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(dyn)) return(invisible(NULL))
  dyn <- ensure_reference_zero(dyn, ref_event = ref_event)
  plot_event_study(
    dyn,
    title    = paste0("Compulsory schooling laws — ", label),
    subtitle = paste0("Outcome: ", outcome_col,
                      " | Ref: e=", ref_event,
                      " | Clustered by state"),
    xlab     = "Decades relative to compulsory law passage",
    event_grid = event_grid
  )
}

cat("\n=== ETWFE: levels (stem_per_1000_pop) ===\n")
p_levels  <- run_compulsory("stem_per_1000_pop",  etwfe_panel, "levels")
p_detrend <- run_compulsory("y_resid",             etwfe_panel, "detrended")

if (!is.null(p_levels)) {
  ggsave(file.path(results_dir, "compulsory_event_study_levels.png"),
         p_levels, width = 10, height = 6)
}
if (!is.null(p_detrend)) {
  ggsave(file.path(results_dir, "compulsory_event_study_detrended.png"),
         p_detrend, width = 10, height = 6)
}

###############################################################################
# Correlation: elite school timing vs compulsory law timing
#
# Do counties with elite schools also have early compulsory laws?
# If yes, the two treatments are confounded; if no (or if the correlation is
# driven by state fixed effects), the elite school effect is independent.
###############################################################################

elite_schools <- read_csv(file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"),
                          show_col_types = FALSE) %>%
  filter(crit_high_access_strict == "yes") %>%
  mutate(
    county_geoid = str_pad(as.character(county_geoid), 5, "left", "0"),
    g_elite = (founding_year_used %/% 10L) * 10L,
    state_fips = substr(county_geoid, 1, 2)
  ) %>%
  select(state, state_abbr, county_geoid, school, founding_year_used, g_elite)

elite_with_law <- elite_schools %>%
  left_join(cs_laws, by = "state_fips")

cat("\n=== Elite school counties: elite timing vs compulsory law timing ===\n")
elite_with_law %>%
  select(state_abbr, school, g_elite, g_compulsory) %>%
  mutate(elite_before_law = g_elite < g_compulsory) %>%
  arrange(state_abbr, g_elite) %>%
  print(n = 30)

# Simple correlation
if (nrow(elite_with_law) > 3 && all(!is.na(elite_with_law$g_compulsory))) {
  corr_raw <- cor(elite_with_law$g_elite, elite_with_law$g_compulsory,
                  use = "complete.obs")
  cat("\nRaw correlation (elite founding decade vs compulsory law decade):",
      round(corr_raw, 3), "\n")
  cat("(near zero = independent treatments; positive = early-law states also got elite schools early)\n")
}

# Scatterplot: law decade vs elite founding decade, labelled by school
if (nrow(elite_with_law) >= 3) {
  suppressPackageStartupMessages(library(ggrepel))
  p_corr <- elite_with_law %>%
    ggplot(aes(g_compulsory, g_elite, label = paste0(state_abbr, ": ", school))) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
    geom_point(size = 3, color = "#2166ac") +
    ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
    scale_x_continuous(breaks = seq(1850, 1920, by = 10)) +
    scale_y_continuous(breaks = seq(1830, 1940, by = 10)) +
    labs(
      title    = "Elite school opening vs. compulsory attendance law",
      subtitle = "Each point is one high-access school. Dashed line = simultaneous timing.",
      x        = "Decade state passed compulsory attendance law",
      y        = "Decade elite school opened",
      caption  = "Elite schools: crit_high_access_strict == 'yes'. Law dates: Lleras-Muney (2002)."
    )
  ggsave(file.path(results_dir, "compulsory_vs_elite_correlation.png"),
         p_corr, width = 9, height = 7)
  cat("Correlation plot saved.\n")
}

###############################################################################
# Compare ATTs: elite schools vs compulsory laws (text summary)
###############################################################################

cat("\n=== Summary: expected contrast ===\n")
cat("Elite schools (crit_high_access_strict): expect positive, sustained ATT.\n")
cat("Compulsory attendance laws: expect null ATT (broad access ≠ talent production).\n")
cat("Correlation between timing: above.\n")
cat("\nIf correlation is low, the two can be studied as independent mechanisms.\n")
cat("If correlation is high (within state), include compulsory law decade as a control.\n")

elapsed <- difftime(Sys.time(), initial_time, units = "secs")
cat("\nDone in", round(as.numeric(elapsed), 1), "s.\n")
cat("Results in:", results_dir, "\n")
