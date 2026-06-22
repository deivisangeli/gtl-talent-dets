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
# Three estimators per specification:
#   1. Plain Wooldridge ETWFE via fixest::sunab. Wooldridge-Mundlak
#      equivalent of Sun-Abraham, ATT(e = -10) = 0 by convention.
#   2. Callaway-Sant'Anna staggered DID (did::att_gt + aggte), with
#      control_group = "nevertreated" when never-treated units exist and
#      "notyettreated" otherwise. With never-treated controls and no
#      covariates this is algebraically equivalent to (1) up to weighting
#      of the cohort x event-time cells; differences typically reflect
#      different finite-sample weights.
#   3. CS DID with each county's log 1820 population as a time-invariant
#      covariate (xformla = ~ log_pop_1820, est_method = "reg"). Adjusts
#      for cross-sectional differences in initial size that might predict
#      both treatment timing and STEM-talent production.
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
# Outputs (Dropbox results/elite_schools/elite_school_event_studies/<results_subdir>/):
#   high_access_county_treatment_core.csv
#   high_access_etwfe_dynamic.csv
#   high_access_etwfe_summary.csv
#   high_access_etwfe_support.csv
#   high_access_etwfe_<spec>_<outcome>.png   (one figure per spec x outcome)
#   high_access_etwfe_notes.txt
###############################################################################

rm(list = ls())

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repo_root <- if (basename(cwd) == "analysis") dirname(cwd) else if (basename(dirname(cwd)) == "analysis") dirname(dirname(cwd)) else cwd
}
source(file.path(repo_root, "analysis", "elite_schools", "etwfe_high_access_helpers.R"))
source(file.path(repo_root, "paths.R"))
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

results_root <- file.path(TALENT_DETS_DATA_DIR, "results", "elite_schools", "elite_school_event_studies")
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
#
# Panel selection: ELITE_MERGE_NYC=TRUE picks the NYC-merged variant in which
# the five boroughs are collapsed into the synthetic GEOID "36000". Both
# panels are built upstream by prep/cleaning_county_1800.R and ship with
# population, county_births_estimate_decade, stem_per_1000_pop, and
# stem_per_1000_births already computed; this script only consumes them.
###############################################################################

panel_path <- if (merge_nyc) {
  file.path(DATA_OUTPUT, "us_panel_county_stem_1800_nyc_merged.csv")
} else {
  file.path(DATA_OUTPUT, "us_panel_county_stem_1800.csv")
}

panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  mutate(
    GEOID  = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    decade = as.integer(decade)
  )

if (length(drop_state_fips) > 0) {
  panel <- panel %>%
    filter(!(substr(GEOID, 1, 2) %in% drop_state_fips))
}

# Drop every cell whose population comes from HYDE gridded interpolation.
# HYDE is unreliable for county-decade pre-organization periods (Western
# counties not yet established, Bronx pre-1914 etc.) and we agreed that
# no analysis cell should rely on it. Cells with source in {"nhgis",
# "manual", "merged_nyc"} are retained. This makes the panel mildly
# unbalanced for counties whose early decades were HYDE-filled.
n_before <- nrow(panel)
n_geoid_before <- n_distinct(panel$GEOID)
panel <- panel %>% filter(population_source != "hyde")
cat("\nDropping HYDE-sourced rows: ", n_before - nrow(panel),
    " cells (", n_geoid_before - n_distinct(panel$GEOID),
    " counties lose all rows).\n", sep = "")

# Time-invariant covariate for CS DID: each county's 1820 population
# (logged for scale stability). 1820 is chosen because it is in the
# pre-treatment window for every retained treated cohort (g >= 1830) and
# captures cross-sectional differences in initial size that may correlate
# with both treatment timing and STEM-talent production. Counties with
# missing 1820 population (e.g., not yet established in 1820) drop out
# of the CS DID sample.
pop_1820 <- panel %>%
  filter(decade == 1820L) %>%
  transmute(GEOID, pop_1820 = population, log_pop_1820 = log1p(population))
panel <- panel %>% left_join(pop_1820, by = "GEOID")

schools <- read_csv(file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"),
                    show_col_types = FALSE) %>%
  mutate(
    county_geoid       = str_pad(as.character(county_geoid), width = 5, side = "left", pad = "0"),
    founding_year_used = as.integer(founding_year_used),
    g_full             = first_full_exposure_decade(founding_year_used, school_age)
  )

###############################################################################
# Exclude counties contaminated by a pre-1800 high-access school.
#
# A county is contaminated when it contains a school that passes every
# high-access criterion except crit_in_frame — i.e., a pre-1800 tuition-free
# selective secondary school. Such counties are always-treated for our
# treatment and have no usable pre-treatment window.
#
# Under the corrected logic ONLY public tuition-free schools contaminate.
# Private tuition schools (Phillips Andover, Collegiate, Trinity, etc.) do NOT
# contaminate their counties because they fail crit_tuition_free_historical and
# therefore do not represent the same treatment. Currently: Suffolk County MA
# (Boston Latin, founded 1635) is the only contaminated county.
###############################################################################

contaminated_geoids <- schools %>%
  filter(contaminates_county == "yes") %>%
  pull(county_geoid) %>%
  unique()

if (length(contaminated_geoids) > 0) {
  cat("\nExcluding", length(contaminated_geoids),
      "county/counties always-treated by pre-1800 high-access school:\n")
  schools %>%
    filter(contaminates_county == "yes") %>%
    select(school, county_geoid, county_name, state_abbr, founding_year_used) %>%
    print()
  panel   <- panel   %>% filter(!GEOID %in% contaminated_geoids)
  schools <- schools %>% filter(!county_geoid %in% contaminated_geoids)
}

# Counties whose pre-treatment period is not a meaningful US administrative
# unit are excluded outright:
#   - Bronx (36005) is excluded from the boroughs-separate spec (kept in
#     merge_nyc, absorbed into synthetic NYC). Pre-1873 the area was rural
#     southern Westchester (no separate enumeration); 1874-1898 it was
#     annexed to New York County; 1898-1914 it was the Bronx subdivision
#     of NYC; 1914 it became Bronx County. NHGIS has no pre-1920 row, so
#     any standalone pre-trend would rely on HYDE gridded values.
#   - San Francisco (06075) is excluded everywhere. The area was Mexican
#     Alta California until 1848 and the US county was created in 1850;
#     pre-1850 HYDE cells estimate ~100 people in the footprint, which
#     are not a meaningful pre-treatment baseline for a US elite school.
#     Lowell HS (1856) was the only high-access school in SF, so dropping
#     SF removes the g=1850 cohort entirely.
counties_without_pre_period <- c("06075")   # SF — always excluded
if (!merge_nyc) {
  counties_without_pre_period <- c(counties_without_pre_period, "36005")  # Bronx
  cat("\nExcluding Bronx (36005) from boroughs-separate spec; pre-1914 not a meaningful unit.\n")
}
cat("Excluding San Francisco (06075) from all specs; pre-1850 not a US unit.\n")
panel   <- panel   %>% filter(!GEOID %in% counties_without_pre_period)
schools <- schools %>% filter(!county_geoid %in% counties_without_pre_period)

# When the NYC-merged panel is in use, reassign the 5 boroughs' schools to
# the synthetic GEOID "36000" so the treatment join below picks them up.
# (The panel rows for the merged unit are produced upstream by
# prep/cleaning_county_1800.R; only the schools file needs remapping here.)
if (merge_nyc) {
  nyc_boroughs        <- c("36005", "36047", "36061", "36081", "36085")
  nyc_synthetic_geoid <- "36000"

  schools <- schools %>%
    mutate(
      county_geoid = if_else(county_geoid %in% nyc_boroughs,
                             nyc_synthetic_geoid, county_geoid),
      county_name  = if_else(county_geoid == nyc_synthetic_geoid,
                             "New York City (merged)", county_name),
      state_abbr   = if_else(county_geoid == nyc_synthetic_geoid,
                             "NY", state_abbr)
    )

  cat("\nELITE_MERGE_NYC: schools in 5 NYC boroughs reassigned to GEOID ",
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
  # ---- Hard-STEM subset of Discovery/Science ----
  list(var = "n_stem",                   label = "STEM births (count)",                       short = "stem_count"),
  list(var = "any_stem_pct",             label = "Any STEM birth (pp)",                       short = "any_stem_pct"),
  list(var = "stem_per_1000_pop",        label = "STEM births per 1,000 population",          short = "stem_per_pop"),
  list(var = "stem_per_1000_births",     label = "STEM births per 1,000 est. births",         short = "stem_per_birth"),
  # ---- All Discovery/Science (all scientific) ----
  list(var = "n_inventors",              label = "Scientific births (count)",                 short = "allsci_count"),
  list(var = "any_allsci_pct",           label = "Any scientific birth (pp)",                 short = "any_allsci_pct"),
  list(var = "allsci_per_1000_pop",      label = "Scientific births per 1,000 population",    short = "allsci_per_pop"),
  list(var = "allsci_per_1000_births",   label = "Scientific births per 1,000 est. births",   short = "allsci_per_birth"),
  # ---- All Wikipedia notable births ----
  list(var = "n_all_wiki",               label = "Wikipedia births (count)",                  short = "allwiki_count"),
  list(var = "any_all_wiki_pct",         label = "Any Wikipedia birth (pp)",                  short = "any_allwiki_pct"),
  list(var = "all_wiki_per_1000_pop",    label = "Wikipedia births per 1,000 population",     short = "allwiki_per_pop"),
  list(var = "all_wiki_per_1000_births", label = "Wikipedia births per 1,000 est. births",    short = "allwiki_per_birth"),
  # ---- STEM share of all Wikipedia births in the county-decade ----
  list(var = "stem_over_allwiki_pct",    label = "STEM share of Wikipedia births (pp)",       short = "stem_over_allwiki"),
  # ---- Auxiliary level outcome ----
  list(var = "population",               label = "County population",                         short = "pop")
)

estimator_colors <- c(
  "Callaway-Sant'Anna"                          = "#e9a200",
  "Callaway-Sant'Anna + log 1820 pop"           = "#2a9d8f"
)

methodology_notes <- function() {
  cat("Panel decades: ", min(panel$decade), " to ", max(panel$decade), "\n", sep = "")
  cat("School age assumption: ", school_age, "\n", sep = "")
  cat("School file: ", file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"), "\n\n")

  cat("Sample restriction\n")
  cat("- Counties with fewer than 3 non-missing pre-treatment outcome\n")
  cat("  observations are dropped before any regression.\n\n")

  cat("Estimators\n")
  cat("- Wooldridge ETWFE: fixest::sunab(g, decade, ref.p = -10) with\n")
  cat("  county and decade fixed effects. Cluster-robust SEs at county.\n")
  cat("- Callaway-Sant'Anna: did::att_gt + aggte, control_group =\n")
  cat("  'nevertreated' when available else 'notyettreated', universal\n")
  cat("  base period e = -10.\n")
  cat("- CS DID + log 1820 pop: same as above with xformla =\n")
  cat("  ~ log_pop_1820 and est_method = 'reg'. Outcome regression on\n")
  cat("  the never-treated controls projects the counterfactual; we use\n")
  cat("  regression rather than dr because the propensity-score step is\n")
  cat("  singular when treated cohorts are tiny.\n\n")
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
      min_pre_obs = 3L, outcome_var = outcome$var
    )

    plain <- NULL   # Wooldridge ETWFE temporarily disabled per user request

    csdid_plain <- tryCatch(
      compute_dynamic(out_panel, outcome$var, spec$label, outcome$label,
                      "Callaway-Sant'Anna",
                      window_years, event_grid,
                      engine = "csdid",
                      covariates = NULL),
      error = function(e) {
        message("Skipping CS DID (no controls) for ", spec$short, " / ",
                outcome$short, ": ", conditionMessage(e))
        NULL
      }
    )
    csdid_pop <- tryCatch(
      compute_dynamic(out_panel, outcome$var, spec$label, outcome$label,
                      "Callaway-Sant'Anna + log 1820 pop",
                      window_years, event_grid,
                      engine = "csdid",
                      covariates = ~ log_pop_1820),
      error = function(e) {
        message("Skipping CS DID + 1820 pop for ", spec$short, " / ",
                outcome$short, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(plain)) {
      spec_dynamic_list[[paste(outcome$short, "plain", sep = "|")]] <- plain
    }
    if (!is.null(csdid_plain)) {
      spec_dynamic_list[[paste(outcome$short, "csdid_plain", sep = "|")]] <- csdid_plain
    }
    if (!is.null(csdid_pop)) {
      spec_dynamic_list[[paste(outcome$short, "csdid_pop1820", sep = "|")]] <- csdid_pop
    }
    if (is.null(plain) && is.null(csdid_plain) && is.null(csdid_pop)) next

    # Raw means use the same >=3 pre-obs sample as the regressions so the
    # raw and adjusted plots are comparable.
    rm_panel <- filter_min_pre_obs(out_panel, outcome$var, min_pre_obs = 3L)
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

  make_es_plot <- function(plot_df, jitter_anchor, outcome_label, sample_text) {
    estimators_in_plot <- unique(plot_df$estimator)
    n_est <- length(estimators_in_plot)
    # Spread error-bar markers symmetrically around the integer event time
    # so overlapping CIs are readable. With 1 estimator no offset is needed,
    # with k > 1 estimators the offsets are evenly spaced on [-w, +w] with
    # the anchor pinned to the leftmost slot.
    width <- 1.6
    if (n_est <= 1) {
      offsets <- setNames(0, estimators_in_plot)
    } else {
      ordered <- c(jitter_anchor, setdiff(estimators_in_plot, jitter_anchor))
      offsets <- setNames(seq(-width, width, length.out = n_est), ordered)
    }
    plot_df_jitter <- plot_df %>%
      mutate(x_jitter = event_time + offsets[estimator])
    plot_df_jitter %>%
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
        y     = paste0("ATT, ", outcome_label),
        color = NULL, fill = NULL,
        caption = sample_text
      ) +
      theme(
        legend.position = "bottom",
        plot.caption    = element_text(hjust = 0.5, size = 9, color = "gray30")
      )
  }

  for (outcome in outcomes) {
    if (nrow(spec_dynamic) == 0) next
    plot_df <- spec_dynamic %>% filter(outcome == !!outcome$label)
    if (nrow(plot_df) == 0) next

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

    # Single figure per outcome: vanilla CS DID and CS DID with log 1820
    # pop as a covariate. Wooldridge ETWFE temporarily disabled.
    if (nrow(plot_df) > 0) {
      ggsave(
        file.path(spec_dir, paste0("es_", outcome$short, ".png")),
        make_es_plot(plot_df, "Callaway-Sant'Anna", outcome$label, sample_text),
        width = 9, height = 5.25, dpi = 300
      )
    }

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
