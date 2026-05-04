###############################################################################
# Shared helpers for the high-access ETWFE analysis.
# Sourced by:
#   analysis_elite_school_high_access_estimators_1800.R
#   analysis_high_access_etwfe_synthetic_validation.R
#
# All helpers operate on a county-decade panel with columns at minimum:
#   GEOID       five-digit county FIPS string
#   GEOID_num   numeric county id (built downstream)
#   decade      integer decade
#   g           integer treatment cohort (g = 0 means never-treated control)
#   <outcome>   numeric outcome column referenced by name
#
# Estimator implementation note
#   The Wooldridge ETWFE event-study is the saturated cohort x event-time
#   regression aggregated to event time. We implement it via fixest::sunab
#   (Sun and Abraham, 2021), which is the same regression with anti-treatment
#   leads explicitly included. The reference event time is fixed at e = -10
#   (one decade before the school opens), so ATT(e = -10) = 0 by construction;
#   e = 0 is the partial-exposure decade, e = +10 is the first fully-exposed
#   decade, and e = -20, -30, ... are honest pre-period placebos.
###############################################################################

suppressPackageStartupMessages({
  library("tidyverse")
  library("fixest")
  library("purrr")
})

first_full_exposure_decade <- function(open_year, school_age = 14L) {
  as.integer(10 * ceiling((open_year - school_age) / 10))
}

# Spec A: treated = high-access counties; control = low-access counties +
# counties with no elite school. Counties whose high-access cohort sits at
# or before the first panel decade are dropped (they have no pre-period).
build_panel_spec_A <- function(panel, treatment) {
  first_panel_decade <- min(panel$decade, na.rm = TRUE)
  panel %>%
    left_join(treatment, by = c("GEOID" = "county_geoid")) %>%
    mutate(
      GEOID_num = as.numeric(GEOID),
      g = case_when(
        is.na(g_any)                      ~ 0L,
        is.na(first_exposure_access)      ~ 0L,
        first_exposure_access != "high"   ~ 0L,
        g_any <= first_panel_decade       ~ NA_integer_,
        TRUE                              ~ as.integer(g_any)
      )
    ) %>%
    filter(!is.na(g))
}

# Spec B: treated = high-access counties; control = low-access counties only.
# Counties with no elite school are dropped via the inner_join.
build_panel_spec_B <- function(panel, treatment) {
  first_panel_decade <- min(panel$decade, na.rm = TRUE)
  panel %>%
    inner_join(treatment, by = c("GEOID" = "county_geoid")) %>%
    mutate(
      GEOID_num = as.numeric(GEOID),
      g = case_when(
        first_exposure_access == "high" & g_any >  first_panel_decade ~ as.integer(g_any),
        first_exposure_access == "high" & g_any <= first_panel_decade ~ NA_integer_,
        first_exposure_access == "low"                                ~ 0L,
        TRUE                                                          ~ NA_integer_
      )
    ) %>%
    filter(!is.na(g))
}

# Spec C: high-access counties only; identification from not-yet-treated
# cohorts. Low-access and no-elite-school counties are dropped. There is
# no never-treated reference: the sunab regression uses later-treated
# cohorts as controls for earlier-treated cohorts at calendar times before
# the later cohort is itself treated.
build_panel_spec_C <- function(panel, treatment) {
  first_panel_decade <- min(panel$decade, na.rm = TRUE)
  panel %>%
    inner_join(treatment, by = c("GEOID" = "county_geoid")) %>%
    filter(first_exposure_access == "high") %>%
    mutate(
      GEOID_num = as.numeric(GEOID),
      g = case_when(
        g_any >  first_panel_decade ~ as.integer(g_any),
        TRUE                        ~ NA_integer_
      )
    ) %>%
    filter(!is.na(g))
}

# Detrend the outcome by a county-specific linear fit on pre-treatment cells.
# Returns the input panel with extra columns y_resid (detrended outcome) and
# detrended (TRUE if the linear fit was applied; FALSE if the county had
# fewer than min_pre_obs pre-treatment outcome observations and was therefore
# not detrended). The caller should typically filter to detrended == TRUE
# before passing the panel to the event-study regression.
detrend_county <- function(data, outcome_var, min_pre_obs = 4L) {
  fit_one <- function(df) {
    y_orig  <- df[[outcome_var]]
    pre_idx <- which(df$is_pre & !is.na(y_orig))
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
  data %>%
    mutate(is_pre = (g == 0L | decade < g)) %>%
    group_split(GEOID_num) %>%
    map_dfr(fit_one)
}

# Run the Wooldridge ETWFE event study via fixest::sunab. Reference event
# time is fixed at -10 (one decade before the school first opens), so
# ATT(e = -10) is mechanically zero. e = 0 is the partial-exposure decade
# (school opens this decade; cohorts born early in the decade reach age 14
# just as the school opens and receive partial exposure), e = +10 is the
# first fully-exposed decade, and e = -20, -30, ... are honest pre-period
# placebo estimates.
# The Wooldridge cohort x event-time regression is aggregated to event time
# inside fixest, with cohort weights proportional to cohort population at
# each event time.
run_sa_event <- function(data, outcome_var, window_years, event_grid,
                         ref_event = -10L) {
  data_sa <- data %>%
    mutate(g_sa = if_else(g == 0L, 10000L, as.integer(g)))

  fml <- as.formula(paste0(
    outcome_var,
    " ~ sunab(g_sa, decade, ref.p = ", ref_event, ") | GEOID_num + decade"
  ))

  mod <- suppressWarnings(
    feols(fml, data = data_sa, cluster = ~GEOID_num, warn = FALSE)
  )

  ct <- as.data.frame(summary(mod, agg = TRUE)$coeftable)
  ct$term <- rownames(ct)

  out <- ct %>%
    as_tibble() %>%
    mutate(event_time = as.integer(str_extract(term, "(?<=::)-?\\d+"))) %>%
    filter(event_time %in% event_grid) %>%
    transmute(
      event_time,
      att = Estimate,
      se  = `Std. Error`
    )

  # The reference event time is omitted from the regression. Add it back
  # explicitly with att = 0 and se = 0 so plots have a complete grid.
  if (!ref_event %in% out$event_time) {
    out <- bind_rows(out, tibble(event_time = ref_event, att = 0, se = 0))
  }

  out %>% arrange(event_time)
}

ensure_reference_zero <- function(dyn, ref_event = -10L) {
  if (any(dyn$event_time == ref_event, na.rm = TRUE)) return(dyn)
  bind_rows(dyn, tibble(event_time = ref_event, att = 0, se = 0)) %>%
    arrange(event_time)
}

# Drop counties that do not have at least min_pre_obs non-missing
# pre-treatment observations of the outcome. Applied identically to both
# the plain and detrend specifications so the samples are comparable.
filter_min_pre_obs <- function(data, outcome_var, min_pre_obs = 4L) {
  keep_ids <- data %>%
    mutate(is_pre = (g == 0L | decade < g)) %>%
    group_by(GEOID_num) %>%
    summarise(
      keep = sum(is_pre & !is.na(.data[[outcome_var]])) >= min_pre_obs,
      .groups = "drop"
    ) %>%
    filter(keep) %>%
    pull(GEOID_num)
  data %>% filter(GEOID_num %in% keep_ids)
}

compute_dynamic <- function(data, outcome_var, spec_label, outcome_label,
                            estimator_label, window_years, event_grid,
                            detrend = FALSE, min_pre_obs = 4L,
                            ref_event = -10L) {
  d <- filter_min_pre_obs(data, outcome_var, min_pre_obs = min_pre_obs)
  if (detrend) {
    d   <- detrend_county(d, outcome_var, min_pre_obs = min_pre_obs)
    dyn <- run_sa_event(d, "y_resid", window_years, event_grid,
                        ref_event = ref_event)
  } else {
    dyn <- run_sa_event(d, outcome_var, window_years, event_grid,
                        ref_event = ref_event)
  }
  dyn <- ensure_reference_zero(dyn, ref_event = ref_event)
  dyn %>%
    mutate(
      spec      = spec_label,
      outcome   = outcome_label,
      estimator = estimator_label,
      conf_low  = att - 1.96 * se,
      conf_high = att + 1.96 * se,
      .before   = 1
    )
}

# Raw means by event time, pooling across cohorts.
#
# For each calendar decade, compute the average outcome over the control
# set: never-treated counties (g == 0) plus units not yet treated by that
# decade (g > decade). This matches the not-yet-treated identification
# used by the sunab regression. Then for each treated observation at
# decade = g + e, pair its outcome with that decade's control mean and
# aggregate to event time e by pooling across cohorts.
#
# Spec C has no never-treated marker, so the control set reduces to the
# not-yet-treated cohorts.
make_raw_means <- function(data, outcome_var, event_grid) {
  decade_grid <- sort(unique(data$decade))
  control_means <- map_dfr(decade_grid, function(d) {
    is_control <- (data$g == 0L) | (data$g > d)
    is_decade  <- data$decade == d
    y          <- data[[outcome_var]][is_decade & is_control]
    tibble(
      decade       = d,
      control_mean = mean(y, na.rm = TRUE),
      control_n    = sum(!is.na(y))
    )
  })

  treated <- data %>%
    filter(g > 0L) %>%
    mutate(event_time = as.integer(decade - g)) %>%
    filter(event_time %in% event_grid) %>%
    left_join(control_means, by = "decade")

  treated %>%
    group_by(event_time) %>%
    summarise(
      treated_mean   = mean(.data[[outcome_var]], na.rm = TRUE),
      control_mean   = mean(control_mean, na.rm = TRUE),
      raw_diff       = treated_mean - control_mean,
      treated_obs    = n(),
      mean_control_n = mean(control_n, na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    arrange(event_time)
}

# Cohort-specific raw means over calendar time. Each cohort g (including
# the g == 0 control group, if present) gets one line: the mean outcome
# in decade t over the units of that cohort.
make_cohort_means <- function(data, outcome_var) {
  data %>%
    group_by(g, decade) %>%
    summarise(
      cohort_mean = mean(.data[[outcome_var]], na.rm = TRUE),
      n_obs       = sum(!is.na(.data[[outcome_var]])),
      .groups     = "drop"
    ) %>%
    mutate(
      cohort_label = if_else(g == 0L, "Control (g = 0)",
                             paste0("Cohort ", g))
    ) %>%
    arrange(g, decade)
}

count_support <- function(data, spec_label, outcome_label,
                          min_pre_obs = 4L, outcome_var = NULL) {
  has_treated <- any(data$g > 0)

  kept_ids <- character(0)
  if (!is.null(outcome_var)) {
    pre_counts <- data %>%
      mutate(is_pre = (g == 0L | decade < g)) %>%
      group_by(GEOID) %>%
      summarise(
        n_pre = sum(is_pre & !is.na(.data[[outcome_var]])),
        .groups = "drop"
      )
    kept_ids <- pre_counts$GEOID[pre_counts$n_pre >= min_pre_obs]
  }
  kept <- data %>% filter(GEOID %in% kept_ids)
  has_treated_kept <- any(kept$g > 0)

  tibble(
    spec                      = spec_label,
    outcome                   = outcome_label,
    sample_counties           = n_distinct(data$GEOID),
    counties_dropped_min_pre  = n_distinct(data$GEOID) - length(kept_ids),
    sample_counties_used      = length(kept_ids),
    treated_counties_used     = n_distinct(kept$GEOID[kept$g > 0]),
    control_counties_used     = n_distinct(kept$GEOID[kept$g == 0]),
    first_cohort              = if (has_treated_kept) min(kept$g[kept$g > 0]) else NA_integer_,
    last_cohort               = if (has_treated_kept) max(kept$g[kept$g > 0]) else NA_integer_
  )
}
