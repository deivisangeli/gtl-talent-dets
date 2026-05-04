###############################################################################
# Project: Determinants of Talent Production via Elite High Schools
# Goal:    Wooldridge ETWFE event studies for high-access elite schools.
#
# Two specifications (defined by the control group):
#   A. High-access counties vs all other counties
#      Control = low-access counties + counties with no elite school.
#      Treated = counties whose first elite-school exposure is high access.
#   B. High-access counties vs low-access counties
#      Drop counties with no elite school.
#      Treated = high-access ever-treated counties.
#      Control = low-access ever-treated counties.
#
# Two estimators per specification:
#   1. Plain Wooldridge ETWFE (etwfe::etwfe + emfx, type = "event").
#      Wooldridge-Mundlak equivalent of Sun-Abraham, with ATT(e = 0)
#      mechanically zero by the omitted-reference convention.
#   2. ETWFE with county-specific linear pre-treatment trends.
#      Step 1: for each county fit y_it = a_i + b_i * decade by OLS using
#              only pre-treatment cells (decade < g for treated counties;
#              all decades for control counties with g = 0).
#      Step 2: subtract the predicted linear path from y to obtain
#              y_resid = y - (a_i + b_i * decade).
#      Step 3: run plain ETWFE on y_resid.
#      Counties with fewer than 3 pre-treatment outcome observations are
#      not detrended; their original y is passed through unchanged.
#      Note: standard errors do not propagate first-step uncertainty.
#
# Outcomes:
#   - n_stem                STEM births (count)
#   - any_stem_pct          Any STEM birth (pp)
#   - stem_per_1000_pop     STEM births per 1,000 county population
#   - stem_per_1000_births  STEM births per 1,000 estimated county births
#   - population            County population (level)
#
# The "estimated county births" denominator is built by scaling the country-
# level US annual birth series (prep/input/new_births_total_number_estimated.csv)
# to county level via the county's share of the US decennial population. We
# do not have direct historical county-level birth counts spanning 1800-2000,
# so this is a national-rate proxy: it captures temporal variation in US
# fertility but not cross-county variation in birth rates.
#
# Outputs (analysis/results/elite_school_event_studies/<results_subdir>/):
#   high_access_county_treatment_core.csv
#   high_access_etwfe_dynamic.csv
#   high_access_etwfe_summary.csv
#   high_access_etwfe_support.csv
#   high_access_etwfe_<spec>_<outcome>.png   (one figure per spec x outcome)
#   high_access_etwfe_notes.txt
###############################################################################

rm(list = ls())

source("etwfe_high_access_helpers.R")
suppressPackageStartupMessages(library("ggplot2"))

initial_time <- Sys.time()
theme_set(theme_minimal(base_size = 13))

window_years <- 50L
event_grid   <- seq(-window_years, window_years, by = 10L)

school_age <- as.integer(Sys.getenv("ELITE_SCHOOL_AGE", unset = "14"))

drop_cohorts_str <- Sys.getenv("ELITE_DROP_COHORTS", unset = "")
drop_cohorts <- if (nzchar(drop_cohorts_str)) {
  as.integer(strsplit(drop_cohorts_str, "[,\\s]+", perl = TRUE)[[1]])
} else {
  integer(0)
}

state_abbr_to_fips <- c(
  AL="01", AK="02", AZ="04", AR="05", CA="06", CO="08", CT="09", DE="10",
  DC="11", FL="12", GA="13", HI="15", ID="16", IL="17", IN="18", IA="19",
  KS="20", KY="21", LA="22", ME="23", MD="24", MA="25", MI="26", MN="27",
  MS="28", MO="29", MT="30", NE="31", NV="32", NH="33", NJ="34", NM="35",
  NY="36", NC="37", ND="38", OH="39", OK="40", OR="41", PA="42", RI="44",
  SC="45", SD="46", TN="47", TX="48", UT="49", VT="50", VA="51", WA="53",
  WV="54", WI="55", WY="56"
)

merge_nyc <- toupper(Sys.getenv("ELITE_MERGE_NYC", unset = "FALSE")) %in% c("TRUE","T","1","YES")

drop_states_str <- Sys.getenv("ELITE_DROP_STATES", unset = "")
drop_states <- if (nzchar(drop_states_str)) {
  toupper(strsplit(drop_states_str, "[,\\s]+", perl = TRUE)[[1]])
} else {
  character(0)
}
drop_state_fips <- if (length(drop_states) > 0) {
  unname(ifelse(drop_states %in% names(state_abbr_to_fips),
                state_abbr_to_fips[drop_states],
                drop_states))
} else {
  character(0)
}

results_root <- file.path("results", "elite_school_event_studies")
default_results_subdir <- if (school_age == 14L) {
  "high_access_etwfe_county_only"
} else {
  paste0("high_access_etwfe_county_only_age", school_age)
}
if (length(drop_cohorts) > 0) {
  default_results_subdir <- paste0(
    default_results_subdir, "_drop_",
    paste(drop_cohorts, collapse = "_")
  )
}
if (length(drop_states) > 0) {
  default_results_subdir <- paste0(
    default_results_subdir, "_drop_states_",
    paste(drop_states, collapse = "_")
  )
}
if (merge_nyc) {
  default_results_subdir <- paste0(default_results_subdir, "_merge_nyc")
}
results_subdir <- Sys.getenv("ELITE_RESULTS_SUBDIR", unset = default_results_subdir)
results_dir    <- file.path(results_root, results_subdir)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Load panel and school list
###############################################################################

panel <- read_csv("../prep/output/us_panel_county_stem_1800.csv",
                  show_col_types = FALSE) %>%
  mutate(
    GEOID  = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade = as.integer(decade)
  )

# Replace population with the unified county-decade population panel built
# in prep/build_county_population.R. That script prefers NHGIS Census
# (1850-2020) where available and falls back to HYDE (1800-2000) for
# 1800-1840 and for counties not yet in NHGIS for a given decade. HYDE
# alone is known to undercount urban density during rapid urban growth
# (Manhattan 1850-1910 by 13-17x; Brooklyn 1860-1940 by 2-3x; SF Gold
# Rush 1850 by 200x), so the unified panel is materially more accurate
# for high-access urban counties. The source for each cell is recorded
# in the `population_source` column.
pop_panel <- read_csv("../prep/output/county_population.csv",
                      show_col_types = FALSE) %>%
  mutate(
    GEOID  = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade = as.integer(decade)
  ) %>%
  rename(population_source = source)

panel <- panel %>%
  select(-population) %>%
  left_join(pop_panel, by = c("GEOID", "decade"))

if (length(drop_state_fips) > 0) {
  panel <- panel %>%
    filter(!(substr(GEOID, 1, 2) %in% drop_state_fips))
}

###############################################################################
# Build estimated county births from country-level US annual births
#   For each decade, county_births = county_pop * (US_total_births_in_decade
#   / US_total_pop_in_decade). This applies the national US birth rate to
#   each county's population. Captures temporal variation in fertility but
#   no cross-county variation in birth rates.
###############################################################################

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
    us_birth_rate_decade           = us_births_in_decade / us_pop_decade,
    county_births_estimate_decade  = population * us_birth_rate_decade,
    stem_per_1000_pop = if_else(
      population > 0,
      1000 * n_stem / population,
      NA_real_
    ),
    stem_per_1000_births = if_else(
      !is.na(county_births_estimate_decade) & county_births_estimate_decade > 0,
      1000 * n_stem / county_births_estimate_decade,
      NA_real_
    )
  )

schools <- read_csv("../prep/output/elite_high_schools_core_1800_1930.csv",
                    show_col_types = FALSE) %>%
  mutate(
    county_geoid       = str_pad(as.character(county_geoid), width = 5, side = "left", pad = "0"),
    founding_year_used = as.integer(founding_year_used),
    g_full             = first_full_exposure_decade(founding_year_used, school_age)
  )

# Optional: merge the 5 NYC boroughs (Bronx, Kings, NY, Queens, Richmond) into
# one synthetic county. Substantively NYC is one labor market and one elite-
# educational ecosystem; the staggered borough-school timing (Hunter 1869,
# Stuyvesant 1904, Brooklyn Tech 1922, Bronx Sci 1938) does not isolate clean
# treatment-introduction effects because Brooklyn and the Bronx already had
# substantial pre-existing infrastructure tied to Manhattan. Merging treats
# NYC as a single unit treated at its earliest high-access school (Hunter HS
# 1869 -> g = 1860).
if (merge_nyc) {
  nyc_boroughs <- c("36005","36047","36061","36081","36085")
  nyc_synthetic_geoid <- "36000"

  panel_nyc <- panel %>%
    filter(GEOID %in% nyc_boroughs) %>%
    group_by(decade) %>%
    summarise(
      GEOID                          = nyc_synthetic_geoid,
      n_inventors                    = sum(n_inventors, na.rm = TRUE),
      n_stem                         = sum(n_stem, na.rm = TRUE),
      any_stem                       = as.integer(any(any_stem == 1L, na.rm = TRUE)),
      any_stem_pct                   = 100 * as.integer(any(any_stem == 1L, na.rm = TRUE)),
      population                     = sum(population, na.rm = TRUE),
      county_births_estimate_decade  = sum(county_births_estimate_decade, na.rm = TRUE),
      lon_county                     = mean(lon_county, na.rm = TRUE),
      lat_county                     = mean(lat_county, na.rm = TRUE),
      population_source              = "merged_nyc",
      .groups                        = "drop"
    ) %>%
    mutate(
      inv_per_100k         = if_else(population > 0, 1e5 * n_inventors / population, NA_real_),
      stem_per_100k        = if_else(population > 0, 1e5 * n_stem / population,      NA_real_),
      log1p_n_inventors    = log1p(n_inventors),
      log1p_n_stem         = log1p(n_stem),
      stem_per_1000_pop    = if_else(population > 0, 1000 * n_stem / population, NA_real_),
      stem_per_1000_births = if_else(
        !is.na(county_births_estimate_decade) & county_births_estimate_decade > 0,
        1000 * n_stem / county_births_estimate_decade,
        NA_real_
      )
    )

  panel <- bind_rows(
    panel %>% filter(!GEOID %in% nyc_boroughs),
    panel_nyc
  )

  # Reassign the 5 boroughs' schools to the merged NYC GEOID.
  schools <- schools %>%
    mutate(
      county_geoid = if_else(county_geoid %in% nyc_boroughs,
                             nyc_synthetic_geoid, county_geoid),
      county_name  = if_else(county_geoid == nyc_synthetic_geoid,
                             "New York City (merged)", county_name),
      state_abbr   = if_else(county_geoid == nyc_synthetic_geoid,
                             "NY", state_abbr)
    )

  cat("\nELITE_MERGE_NYC: merged 5 NYC boroughs into GEOID ",
      nyc_synthetic_geoid, "\n", sep = "")
}

###############################################################################
# County-level treatment table
###############################################################################

# Treatment definition: a county is "high-access treated" if it EVER had a
# high-access elite school during the panel. Treatment year is the founding
# year of the first high-access school there, regardless of whether earlier
# low-access elite schools existed in the same county. This is what we want
# substantively: e.g., New York County (Manhattan) had Brearley (low, 1884)
# before Stuyvesant (high, 1904), but the relevant treatment for the
# accessible-elite-education hypothesis is Stuyvesant, not Brearley.
county_high_access <- schools %>%
  group_by(county_geoid) %>%
  summarise(
    county_name     = first(county_name),
    state_abbr      = first(state_abbr),
    n_schools       = n(),
    has_high_access = any(crit_high_access_strict == "yes"),
    .groups         = "drop"
  )

county_first_exposure <- schools %>%
  inner_join(county_high_access %>% select(county_geoid, has_high_access),
             by = "county_geoid") %>%
  mutate(
    is_relevant_for_g = if_else(
      has_high_access,
      crit_high_access_strict == "yes",
      TRUE
    )
  ) %>%
  filter(is_relevant_for_g) %>%
  group_by(county_geoid) %>%
  summarise(
    first_exposure_year    = min(founding_year_used, na.rm = TRUE),
    first_exposure_schools = paste(
      sort(unique(school[founding_year_used == first_exposure_year])),
      collapse = "; "
    ),
    .groups                = "drop"
  )

county_treatment <- county_high_access %>%
  left_join(county_first_exposure, by = "county_geoid") %>%
  mutate(
    first_exposure_access = if_else(has_high_access, "high", "low"),
    g_any                 = first_full_exposure_decade(first_exposure_year, school_age)
  )

write_csv(county_treatment,
          file.path(results_dir, "high_access_county_treatment_core.csv"))

###############################################################################
# Estimate
###############################################################################

specs <- list(
  list(label   = "High access vs all other counties",
       short   = "vs_all_other_counties",
       builder = build_panel_spec_A),
  list(label   = "High access vs low-access counties only",
       short   = "vs_low_access_only",
       builder = build_panel_spec_B),
  list(label   = "High access only (not-yet-treated controls)",
       short   = "vs_notyet_high_access",
       builder = build_panel_spec_C)
)

outcomes <- list(
  list(var = "n_stem",               label = "STEM births (count)",                 short = "stem_count"),
  list(var = "any_stem_pct",         label = "Any STEM birth (pp)",                 short = "any_stem_pct"),
  list(var = "stem_per_1000_pop",    label = "STEM births per 1,000 population",    short = "stem_per_pop"),
  list(var = "stem_per_1000_births", label = "STEM births per 1,000 est. births",   short = "stem_per_birth"),
  list(var = "population",           label = "County population",                   short = "pop")
)

estimator_colors <- c(
  "Wooldridge ETWFE"                            = "#023047",
  "Wooldridge ETWFE + county pre-period trend"  = "#bc4749"
)

methodology_notes <- function() {
  cat("Panel decades: ", min(panel$decade), " to ", max(panel$decade), "\n", sep = "")
  cat("School age assumption: ", school_age, "\n", sep = "")
  cat("School file: ../prep/output/elite_high_schools_core_1800_1930.csv\n\n")

  cat("Sample restriction (applies to BOTH estimators)\n")
  cat("- Counties with fewer than 4 non-missing pre-treatment outcome\n")
  cat("  observations are dropped before any regression. The trend\n")
  cat("  specification needs at least 4 pre-period cells to identify a\n")
  cat("  county-specific slope with 2 residual degrees of freedom; we apply\n")
  cat("  the same sample restriction to the plain estimator so the two lines\n")
  cat("  in each figure are directly comparable.\n\n")

  cat("Estimators\n")
  cat("- Wooldridge ETWFE:\n")
  cat("    Implemented as fixest::sunab(g, decade, ref.p = -10) with county\n")
  cat("    and decade fixed effects. Sun and Abraham's saturated cohort x\n")
  cat("    event-time regression is the same model as Wooldridge's ETWFE\n")
  cat("    with anti-treatment leads. The reference event time is fixed at\n")
  cat("    e = -10 (one decade before the school opens), so ATT(e = -10) =\n")
  cat("    0 by construction. e = 0 is the partial-exposure decade (school\n")
  cat("    opens this decade; cohorts born early in the decade reach age 14\n")
  cat("    just as the school opens, so they receive partial exposure).\n")
  cat("    e = +10 is the first fully-exposed decade. e = -20, -30, ... are\n")
  cat("    honest pre-period placebo estimates of the pre-trend.\n")
  cat("    Standard errors are clustered at the county level.\n\n")
  cat("- Wooldridge ETWFE + county pre-period trend:\n")
  cat("    Step 1 fits y_it = a_i + b_i * decade by OLS for each county on\n")
  cat("    pre-treatment cells only (decade < g for treated counties; all\n")
  cat("    decades for control counties with g = 0).\n")
  cat("    Step 2 subtracts the fitted path from y to obtain y_resid.\n")
  cat("    Step 3 feeds y_resid into the same sunab regression as the plain\n")
  cat("    estimator above.\n")
  cat("    Caveat: standard errors do not propagate first-step uncertainty.\n\n")
}

for (spec in specs) {
  spec_dir <- file.path(results_dir, spec$short)
  dir.create(spec_dir, recursive = TRUE, showWarnings = FALSE)

  spec_panel <- spec$builder(panel, county_treatment)
  if (length(drop_cohorts) > 0) {
    spec_panel <- spec_panel %>% filter(!(g %in% drop_cohorts))
  }

  spec_dynamic_list      <- list()
  spec_support_list      <- list()
  spec_raw_means_list    <- list()
  spec_cohort_means_list <- list()

  for (outcome in outcomes) {
    out_panel <- spec_panel %>% filter(!is.na(.data[[outcome$var]]))
    if (nrow(out_panel) == 0) next

    spec_support_list[[outcome$short]] <- count_support(
      out_panel, spec$label, outcome$label,
      min_pre_obs = 4L, outcome_var = outcome$var
    )

    plain <- tryCatch(
      compute_dynamic(out_panel, outcome$var, spec$label, outcome$label,
                      "Wooldridge ETWFE",
                      window_years, event_grid, detrend = FALSE),
      error = function(e) {
        message("Skipping plain ETWFE for ", spec$short, " / ",
                outcome$short, ": ", conditionMessage(e))
        NULL
      }
    )
    trend <- tryCatch(
      compute_dynamic(out_panel, outcome$var, spec$label, outcome$label,
                      "Wooldridge ETWFE + county pre-period trend",
                      window_years, event_grid, detrend = TRUE),
      error = function(e) {
        message("Skipping trend ETWFE for ", spec$short, " / ",
                outcome$short, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(plain)) {
      spec_dynamic_list[[paste(outcome$short, "plain", sep = "|")]] <- plain
    }
    if (!is.null(trend)) {
      spec_dynamic_list[[paste(outcome$short, "trend", sep = "|")]] <- trend
    }
    if (is.null(plain) && is.null(trend)) next

    # Raw means use the same >=4 pre-obs sample as the regressions so the
    # raw and adjusted plots are comparable.
    rm_panel <- filter_min_pre_obs(out_panel, outcome$var, min_pre_obs = 4L)
    rm <- make_raw_means(rm_panel, outcome$var, event_grid)
    spec_raw_means_list[[outcome$short]] <- rm %>%
      mutate(outcome = outcome$label, .before = 1)

    cm <- make_cohort_means(rm_panel, outcome$var)
    spec_cohort_means_list[[outcome$short]] <- cm %>%
      mutate(outcome = outcome$label, .before = 1)
  }

  spec_dynamic      <- bind_rows(spec_dynamic_list)
  spec_support      <- bind_rows(spec_support_list)
  spec_raw_means    <- bind_rows(spec_raw_means_list)
  spec_cohort_means <- bind_rows(spec_cohort_means_list)

  spec_summary <- spec_dynamic %>%
    group_by(outcome, estimator) %>%
    summarise(
      lead_m60      = att[event_time == -60][1],
      se_m60        = se [event_time == -60][1],
      lead_m40      = att[event_time == -40][1],
      se_m40        = se [event_time == -40][1],
      lead_m20      = att[event_time == -20][1],
      se_m20        = se [event_time == -20][1],
      post_0        = att[event_time ==   0][1],
      se_0          = se [event_time ==   0][1],
      post_avg_0_60 = mean(att[event_time >= 0 & event_time <= window_years], na.rm = TRUE),
      .groups       = "drop"
    )

  write_csv(spec_dynamic,      file.path(spec_dir, "dynamic.csv"))
  write_csv(spec_support,      file.path(spec_dir, "support.csv"))
  write_csv(spec_summary,      file.path(spec_dir, "summary.csv"))
  write_csv(spec_raw_means,    file.path(spec_dir, "raw_means.csv"))
  write_csv(spec_cohort_means, file.path(spec_dir, "cohort_means.csv"))

  for (outcome in outcomes) {
    if (nrow(spec_dynamic) == 0) next
    plot_df <- spec_dynamic %>% filter(outcome == !!outcome$label)
    if (nrow(plot_df) == 0) next

    plot_df_jitter <- plot_df %>%
      mutate(
        x_jitter = event_time + if_else(
          estimator == "Wooldridge ETWFE", -0.9, 0.9
        )
      )

    support_row <- spec_support %>% filter(outcome == !!outcome$label)
    n_treated   <- if (nrow(support_row) > 0) support_row$treated_counties_used[1] else NA_integer_
    n_control   <- if (nrow(support_row) > 0) support_row$control_counties_used[1] else NA_integer_
    n_total     <- if (nrow(support_row) > 0) support_row$sample_counties_used[1] else NA_integer_
    n_decades   <- length(unique(panel$decade))
    sample_text <- paste0(
      "Treated counties: ", n_treated,
      "  |  Control counties: ", n_control,
      "  |  Total counties: ", n_total,
      "  |  County-decade obs: ", format(n_total * n_decades, big.mark = ",")
    )

    p <- plot_df_jitter %>%
      ggplot(aes(x = event_time, y = att,
                 color = estimator, fill = estimator)) +
      geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
      geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4,
                 linetype = "dashed") +
      geom_line(linewidth = 0.9) +
      geom_errorbar(aes(x = x_jitter, ymin = conf_low, ymax = conf_high),
                    width = 1.6, linewidth = 0.5) +
      geom_point(aes(x = x_jitter), size = 1.6) +
      scale_x_continuous(breaks = event_grid) +
      scale_color_manual(values = estimator_colors) +
      scale_fill_manual(values  = estimator_colors) +
      labs(
        x     = "Years relative to school-opening decade (e=0 is partial exposure)",
        y     = paste0("ATT, ", outcome$label),
        color = NULL, fill = NULL,
        caption = sample_text
      ) +
      theme(
        legend.position = "bottom",
        plot.caption    = element_text(hjust = 0.5, size = 9, color = "gray30")
      )

    ggsave(
      file.path(spec_dir, paste0("es_", outcome$short, ".png")),
      p, width = 9, height = 5.25, dpi = 300
    )

    rm_df <- spec_raw_means %>% filter(outcome == !!outcome$label)
    if (nrow(rm_df) == 0) next

    control_label <- if (spec$short == "vs_notyet_high_access") {
      "Not-yet-treated cohorts"
    } else {
      "Control"
    }

    rm_long <- rm_df %>%
      select(event_time, treated_mean, control_mean) %>%
      pivot_longer(c(treated_mean, control_mean),
                   names_to = "series", values_to = "mean_value") %>%
      mutate(series = if_else(series == "treated_mean",
                              "Treated (high access)",
                              control_label))

    p_rm <- rm_long %>%
      ggplot(aes(x = event_time, y = mean_value,
                 color = series, linetype = series)) +
      geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4,
                 linetype = "dashed") +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.6) +
      scale_x_continuous(breaks = event_grid) +
      scale_color_manual(values = setNames(
        c("#bc4749", "#023047"),
        c("Treated (high access)", control_label)
      )) +
      scale_linetype_manual(values = setNames(
        c("solid", "dashed"),
        c("Treated (high access)", control_label)
      )) +
      labs(
        x        = "Years relative to school-opening decade (e=0 is partial exposure)",
        y        = paste0("Raw mean, ", outcome$label),
        color    = NULL,
        linetype = NULL
      ) +
      theme(legend.position = "bottom")

    ggsave(
      file.path(spec_dir, paste0("raw_means_", outcome$short, ".png")),
      p_rm, width = 9, height = 5.25, dpi = 300
    )

    cm_df <- spec_cohort_means %>% filter(outcome == !!outcome$label)
    if (nrow(cm_df) == 0) next

    treated_cohorts <- cm_df %>% filter(g > 0L) %>%
      mutate(cohort_factor = factor(g))
    control_cohort  <- cm_df %>% filter(g == 0L)

    p_cm <- ggplot() +
      geom_line(data = treated_cohorts,
                aes(x = decade, y = cohort_mean,
                    color = cohort_factor, group = cohort_factor),
                linewidth = 0.7) +
      geom_point(data = treated_cohorts,
                 aes(x = decade, y = cohort_mean, color = cohort_factor),
                 size = 1.3) +
      {if (nrow(control_cohort) > 0)
        list(
          geom_line(data = control_cohort,
                    aes(x = decade, y = cohort_mean),
                    color = "black", linetype = "dashed", linewidth = 0.7),
          geom_point(data = control_cohort,
                     aes(x = decade, y = cohort_mean),
                     color = "black", size = 1.3)
        )
       else NULL} +
      scale_color_viridis_d(option = "plasma", end = 0.9,
                            name = "Treated cohort (g)") +
      scale_x_continuous(breaks = seq(1800, 2000, by = 20)) +
      labs(
        x = "Decade (calendar time)",
        y = paste0("Mean, ", outcome$label),
        caption = if (nrow(control_cohort) > 0)
                    "Black dashed line: controls with g = 0."
                  else NULL
      ) +
      theme(legend.position = "right")

    ggsave(
      file.path(spec_dir, paste0("cohort_means_", outcome$short, ".png")),
      p_cm, width = 11, height = 5.5, dpi = 300
    )
  }

  sink(file.path(spec_dir, "notes.txt"))
  cat("=== ", spec$label, " ===\n\n", sep = "")
  if (length(drop_cohorts) > 0) {
    cat("Cohorts dropped from this run: ",
        paste(drop_cohorts, collapse = ", "), "\n", sep = "")
  }
  if (length(drop_states) > 0) {
    cat("States dropped from the panel: ",
        paste(drop_states, collapse = ", "), "\n", sep = "")
  }
  if (length(drop_cohorts) > 0 || length(drop_states) > 0) cat("\n")
  cat("Treated:  high-access counties (g = first fully exposed decade).\n")
  if (spec$short == "vs_all_other_counties") {
    cat("Control:  low-access counties + counties with no elite school\n")
    cat("          (pooled into the never-treated reference, g = 0).\n\n")
  } else if (spec$short == "vs_low_access_only") {
    cat("Control:  low-access counties (g = 0).\n")
    cat("          Counties with no elite school are excluded from this spec.\n\n")
  } else {
    cat("Control:  not-yet-treated high-access cohorts.\n")
    cat("          Low-access and no-elite-school counties are excluded.\n")
    cat("          Identification is purely from staggered timing within\n")
    cat("          the high-access group: at calendar time t, units with\n")
    cat("          g > t serve as controls. There is no never-treated\n")
    cat("          reference, so the latest cohort's post-treatment effects\n")
    cat("          are not identified.\n\n")
  }
  methodology_notes()
  cat("=== Support ===\n");  print(spec_support)
  cat("\n=== Summary ===\n"); print(spec_summary)
  sink()
}

elapsed <- difftime(Sys.time(), initial_time, units = "mins")
cat("Done. Per-spec results in subfolders of ", results_dir,
    ". Runtime ", round(as.numeric(elapsed), 2), " minutes.\n", sep = "")
