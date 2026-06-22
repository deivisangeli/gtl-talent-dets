###############################################################################
# Year-level companion to analysis_elite_school_high_access_estimators_1800.R.
#
# Same three specs (vs_all_other / vs_low_access / vs_notyet_high_access) and
# four estimators (plain ETWFE, cohort-detrend ETWFE, vanilla CS DID, CS DID +
# log 1820 pop). Difference: the panel is one row per county-year (not
# county-decade), the cohort g is the first fully exposed birth YEAR
# (founding_year - school_age), the reference period is e = -1, and the event
# window is +/- 20 years around the school's first fully exposed birth cohort.
#
# Implementation note: the shared helpers in etwfe_high_access_helpers.R hard-
# code the time variable name `decade`. Rather than re-parameterise them, this
# script renames the year-level time variable to `decade` in-memory so the
# helpers work unchanged.
#
# Outputs land in Dropbox results/elite_schools/elite_school_event_studies_year/<subdir>/
# so the year-level run does not overwrite the decade-level figures.
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

window_pre   <- 10L
window_post  <- 30L
window_years <- max(window_pre, window_post)
event_grid   <- seq(-window_pre, window_post, by = 1L)
ref_event    <- -1L

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

results_root <- file.path(TALENT_DETS_DATA_DIR, "results", "elite_schools", "elite_school_event_studies_year")
default_results_subdir <- if (school_age == 14L) {
  "high_access_etwfe_year_only"
} else {
  paste0("high_access_etwfe_year_only_age", school_age)
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
# Load panel
###############################################################################

panel_path <- if (merge_nyc) {
  file.path(DATA_OUTPUT, "us_panel_county_stem_year_1800_nyc_merged.csv")
} else {
  file.path(DATA_OUTPUT, "us_panel_county_stem_year_1800.csv")
}

panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  mutate(
    GEOID  = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    year   = as.integer(year)
  ) %>%
  rename(decade = year)   # helpers expect a column named "decade"

if (length(drop_state_fips) > 0) {
  panel <- panel %>%
    filter(!(substr(GEOID, 1, 2) %in% drop_state_fips))
}

# Drop every year whose population came from a HYDE-sourced decennial knot.
# Year-level cells inherit their floor-decennial source (1825 from the 1820
# knot, 1849 from 1840, etc.), so this excludes the same county-period
# combinations as the decade-level filter.
n_before <- nrow(panel)
n_geoid_before <- n_distinct(panel$GEOID)
panel <- panel %>% filter(population_source != "hyde")
cat("\nDropping HYDE-sourced rows: ", n_before - nrow(panel),
    " cells (", n_geoid_before - n_distinct(panel$GEOID),
    " counties lose all rows).\n", sep = "")

# Year-level CS DID covariate: each county's 1820 population (logged).
pop_1820 <- panel %>%
  filter(decade == 1820L) %>%
  transmute(GEOID, pop_1820 = population, log_pop_1820 = log1p(population))
panel <- panel %>% left_join(pop_1820, by = "GEOID")

###############################################################################
# Schools and treatment timing (year level)
###############################################################################

schools <- read_csv(file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"),
                    show_col_types = FALSE) %>%
  mutate(
    county_geoid       = str_pad(as.character(county_geoid), width = 5, side = "left", pad = "0"),
    founding_year_used = as.integer(founding_year_used),
    g_full_year        = founding_year_used - school_age   # first fully exposed birth year
  )

contaminated_geoids <- schools %>%
  filter(contaminates_county == "yes") %>%
  pull(county_geoid) %>% unique()
if (length(contaminated_geoids) > 0) {
  panel   <- panel   %>% filter(!GEOID %in% contaminated_geoids)
  schools <- schools %>% filter(!county_geoid %in% contaminated_geoids)
}

# Same exclusions as the decade analysis: SF always (pre-1850 not a US
# unit), Bronx in the boroughs-separate spec only (pre-1914 not a
# standalone county; absorbed via merge_nyc).
counties_without_pre_period <- c("06075")
if (!merge_nyc) {
  counties_without_pre_period <- c(counties_without_pre_period, "36005")
}
panel   <- panel   %>% filter(!GEOID %in% counties_without_pre_period)
schools <- schools %>% filter(!county_geoid %in% counties_without_pre_period)

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
}

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
    g_any                 = first_exposure_year - school_age
  )

write_csv(county_treatment,
          file.path(results_dir, "high_access_county_treatment_core.csv"))

###############################################################################
# Estimate
###############################################################################

# Year-level only runs spec B (high-access vs low-access counties). Specs A
# and C are skipped here for performance: spec A pulls in ~3,000 control
# counties x 200 years and the fixest sunab regression on that grid takes
# hours; spec C with only 8 treated counties and no never-treated controls
# is too thin to be informative at year resolution. Spec B has ~84 counties
# x 200 years (~17k rows), runs in seconds for ETWFE and minutes for CS DID,
# and is the headline comparison anyway. Override with ELITE_YEAR_ALL_SPECS
# to attempt all three.
csdid_specs <- c("vs_low_access_only")

run_all_specs <- toupper(Sys.getenv("ELITE_YEAR_ALL_SPECS", unset = "FALSE")) %in%
                  c("TRUE","T","1","YES")
specs <- if (run_all_specs) {
  list(
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
} else {
  list(
    list(label   = "High access vs low-access counties only",
         short   = "vs_low_access_only",
         builder = build_panel_spec_B)
  )
}

outcomes <- list(
  list(var = "n_stem",                   label = "STEM births (count)",                       short = "stem_count"),
  list(var = "any_stem_pct",             label = "Any STEM birth (pp)",                       short = "any_stem_pct"),
  list(var = "stem_per_1000_pop",        label = "STEM births per 1,000 population",          short = "stem_per_pop"),
  list(var = "stem_per_1000_births",     label = "STEM births per 1,000 est. births",         short = "stem_per_birth"),
  list(var = "n_inventors",              label = "Scientific births (count)",                 short = "allsci_count"),
  list(var = "any_allsci_pct",           label = "Any scientific birth (pp)",                 short = "any_allsci_pct"),
  list(var = "allsci_per_1000_pop",      label = "Scientific births per 1,000 population",    short = "allsci_per_pop"),
  list(var = "allsci_per_1000_births",   label = "Scientific births per 1,000 est. births",   short = "allsci_per_birth"),
  list(var = "n_all_wiki",               label = "Wikipedia births (count)",                  short = "allwiki_count"),
  list(var = "any_all_wiki_pct",         label = "Any Wikipedia birth (pp)",                  short = "any_allwiki_pct"),
  list(var = "all_wiki_per_1000_pop",    label = "Wikipedia births per 1,000 population",     short = "allwiki_per_pop"),
  list(var = "all_wiki_per_1000_births", label = "Wikipedia births per 1,000 est. births",    short = "allwiki_per_birth"),
  list(var = "stem_over_allwiki_pct",    label = "STEM share of Wikipedia births (pp)",       short = "stem_over_allwiki"),
  list(var = "population",               label = "County population",                         short = "pop")
)

estimator_colors <- c(
  "Callaway-Sant'Anna"                          = "#e9a200",
  "Callaway-Sant'Anna + log 1820 pop"           = "#2a9d8f"
)

methodology_notes <- function() {
  cat("Panel years: ", min(panel$decade), " to ", max(panel$decade), "\n", sep = "")
  cat("School age assumption: ", school_age, "\n", sep = "")
  cat("Reference event time: e = ", ref_event, " (one year before fully-exposed cohort)\n", sep = "")
  cat("Event window: ", -window_pre, " to +", window_post,
      " years around g = founding_year - school_age\n\n", sep = "")
  cat("Year-level analysis: outcomes are per-county per-birth-year. Population\n")
  cat("is linearly interpolated between decennial Census knots; the births\n")
  cat("denominator uses annual US-births-per-decade-population scaled by\n")
  cat("county population.\n")
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
      min_pre_obs = 5L, outcome_var = outcome$var
    )

    plain <- NULL   # Wooldridge ETWFE temporarily disabled per user request
    if (spec$short %in% csdid_specs) {
      csdid_plain <- tryCatch(
        compute_dynamic(out_panel, outcome$var, spec$label, outcome$label,
                        "Callaway-Sant'Anna",
                        window_years, event_grid,
                        engine = "csdid", covariates = NULL,
                        ref_event = ref_event, min_pre_obs = 5L),
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
                        engine = "csdid", covariates = ~ log_pop_1820,
                        ref_event = ref_event, min_pre_obs = 5L),
        error = function(e) {
          message("Skipping CS DID + 1820 pop for ", spec$short, " / ",
                  outcome$short, ": ", conditionMessage(e))
          NULL
        }
      )
    } else {
      csdid_plain <- NULL
      csdid_pop   <- NULL
    }

    if (!is.null(plain))       spec_dynamic_list[[paste(outcome$short, "plain",         sep = "|")]] <- plain
    if (!is.null(csdid_plain)) spec_dynamic_list[[paste(outcome$short, "csdid_plain",   sep = "|")]] <- csdid_plain
    if (!is.null(csdid_pop))   spec_dynamic_list[[paste(outcome$short, "csdid_pop1820", sep = "|")]] <- csdid_pop
    if (is.null(plain) && is.null(csdid_plain) && is.null(csdid_pop)) next

    rm_panel <- filter_min_pre_obs(out_panel, outcome$var, min_pre_obs = 5L)
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
      lead_m10                = att[event_time == -10][1],
      se_m10                  = se [event_time == -10][1],
      lead_m5                 = att[event_time ==  -5][1],
      se_m5                   = se [event_time ==  -5][1],
      post_0                  = att[event_time ==   0][1],
      se_0                    = se [event_time ==   0][1],
      post_p10                = att[event_time ==  10][1],
      se_p10                  = se [event_time ==  10][1],
      post_p20                = att[event_time ==  20][1],
      se_p20                  = se [event_time ==  20][1],
      post_p30                = att[event_time ==  30][1],
      se_p30                  = se [event_time ==  30][1],
      post_avg_0_30           = mean(att[event_time >= 0 & event_time <= 30],
                                     na.rm = TRUE),
      .groups                 = "drop"
    )

  write_csv(spec_dynamic,      file.path(spec_dir, "dynamic.csv"))
  write_csv(spec_support,      file.path(spec_dir, "support.csv"))
  write_csv(spec_summary,      file.path(spec_dir, "summary.csv"))
  write_csv(spec_raw_means,    file.path(spec_dir, "raw_means.csv"))
  write_csv(spec_cohort_means, file.path(spec_dir, "cohort_means.csv"))

  make_es_plot <- function(plot_df, jitter_anchor, outcome_label, sample_text) {
    estimators_in_plot <- unique(plot_df$estimator)
    n_est <- length(estimators_in_plot)
    width <- 0.3   # narrower jitter at year level (events are 1 unit apart)
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
                    width = 0.5, linewidth = 0.4) +
      geom_point(aes(x = x_jitter), size = 1.4) +
      scale_x_continuous(breaks = seq(-window_pre, window_post, by = 5L)) +
      scale_color_manual(values = estimator_colors) +
      scale_fill_manual(values  = estimator_colors) +
      labs(
        x     = "Years relative to first fully-exposed birth cohort (e=0 first cohort with full access)",
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
    n_years     <- length(unique(panel$decade))
    sample_text <- paste0(
      "Treated counties: ", n_treated,
      "  |  Control counties: ", n_control,
      "  |  Total counties: ", n_total,
      "  |  County-year obs: ", format(n_total * n_years, big.mark = ",")
    )

    if (nrow(plot_df) > 0) {
      ggsave(
        file.path(spec_dir, paste0("es_", outcome$short, ".png")),
        make_es_plot(plot_df, "Callaway-Sant'Anna", outcome$label, sample_text),
        width = 9, height = 5.25, dpi = 300
      )
    }
  }

  sink(file.path(spec_dir, "notes.txt"))
  cat("=== ", spec$label, "  (year-level) ===\n\n", sep = "")
  if (length(drop_states) > 0) {
    cat("States dropped from the panel: ",
        paste(drop_states, collapse = ", "), "\n\n", sep = "")
  }
  cat("Treated:  high-access counties (g = founding_year - school_age).\n")
  if (spec$short == "vs_all_other_counties") {
    cat("Control:  low-access counties + counties with no elite school\n")
    cat("          (pooled into the never-treated reference, g = 0).\n\n")
  } else if (spec$short == "vs_low_access_only") {
    cat("Control:  low-access counties (g = 0). Counties with no elite school\n")
    cat("          are excluded.\n\n")
  } else {
    cat("Control:  not-yet-treated high-access cohorts.\n\n")
  }
  methodology_notes()
  cat("\n=== Support ===\n");  print(spec_support)
  cat("\n=== Summary ===\n"); print(spec_summary)
  sink()
}

elapsed <- difftime(Sys.time(), initial_time, units = "mins")
cat("Done. Per-spec results in subfolders of ", results_dir,
    ". Runtime ", round(as.numeric(elapsed), 2), " minutes.\n", sep = "")
