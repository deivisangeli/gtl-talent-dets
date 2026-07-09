###############################################################################
# Project: GTL Talent Determinants
# Goal: AMWS county event studies using scientific-facility treatment timing
#
# Mirrors the county-level scientific-facilities event studies:
# - 50km facility buffers intersected with 2020 county polygons
# - first exposed facility decade defines g
# - did::att_gt with never-treated controls, DR, universal base period
# - only facilities opened up to 1920 are treated in this restricted spec
# - balanced county-decade panel over 1860-1950
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(did)
  library(sf)
  library(tigris)
})

initial_time <- Sys.time()
options(tigris_use_cache = TRUE, timeout = 1000)

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

results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results",
  "scientific_facilities",
  "event_studies",
  "amws_scientific_facilities"
)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

control_group <- "nevertreated"
est_method <- "dr"
treatment_year_cutoff <- 1920
max_treatment_cohort <- 1920
pre_event_window <- 30
post_event_window <- 30
analysis_min_decade <- 1860
analysis_max_decade <- 1950
analysis_decades <- seq(analysis_min_decade, analysis_max_decade, by = 10)
min_supported_cohort <- analysis_min_decade + pre_event_window
max_supported_cohort <- analysis_max_decade - post_event_window

###############################################################################
# Helpers
###############################################################################

as_geoid <- function(x) {
  str_pad(as.character(as.integer(x)), width = 5, side = "left", pad = "0")
}

mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

value_or <- function(x, default) {
  if (is.null(x)) default else x
}

sanitize_filename <- function(x) {
  str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

restrict_treatment_cohort <- function(g) {
  case_when(
    is.na(g) ~ 0,
    g == 0 ~ 0,
    g > max_treatment_cohort ~ NA_real_,
    g < min_supported_cohort ~ NA_real_,
    g > max_supported_cohort ~ NA_real_,
    TRUE ~ as.numeric(g)
  )
}

count_supported_facility_events <- function(facilities_df, counties_poly,
                                            panel_geoids, timing, radius_m) {
  cohort_col <- if (timing == "standard_decade") {
    "decade_std"
  } else {
    "decade_shift"
  }

  fac_buf <- facilities_df %>%
    filter(
      .data[[cohort_col]] <= max_treatment_cohort,
      .data[[cohort_col]] >= min_supported_cohort,
      .data[[cohort_col]] <= max_supported_cohort
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(3857) %>%
    st_buffer(dist = radius_m) %>%
    mutate(fac_idx = row_number()) %>%
    select(fac_idx)

  if (nrow(fac_buf) == 0) {
    return(0L)
  }

  st_join(counties_poly, fac_buf, join = st_intersects) %>%
    st_drop_geometry() %>%
    filter(!is.na(fac_idx), GEOID %in% panel_geoids) %>%
    distinct(fac_idx) %>%
    nrow()
}

format_sample_annotation <- function(model) {
  paste(
    paste0("Events: ", model$n_events),
    paste0("Treated: ", model$n_treated_units),
    paste0("Controls: ", model$n_control_units),
    sep = "\n"
  )
}

extract_dynamic_att <- function(es, outcome, facility_set, timing) {
  tibble(
    outcome = outcome,
    facility_set = facility_set,
    timing = timing,
    control_group = control_group,
    est_method = est_method,
    max_treatment_cohort = max_treatment_cohort,
    analysis_min_decade = analysis_min_decade,
    analysis_max_decade = analysis_max_decade,
    max_post_event_time = post_event_window,
    event_time = es$egt,
    estimate = es$att.egt,
    se = es$se.egt,
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se
  )
}

extract_simple_att <- function(ag, outcome, facility_set, timing) {
  tibble(
    outcome = outcome,
    facility_set = facility_set,
    timing = timing,
    control_group = control_group,
    est_method = est_method,
    max_treatment_cohort = max_treatment_cohort,
    analysis_min_decade = analysis_min_decade,
    analysis_max_decade = analysis_max_decade,
    max_post_event_time = post_event_window,
    estimate = ag$overall.att,
    se = ag$overall.se,
    p_value = 2 * (1 - pnorm(abs(estimate / se))),
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se
  )
}

plot_y_limits <- function(plot_obj, padding = 0.12) {
  built <- ggplot_build(plot_obj)
  y_values <- built$data %>%
    map(~ {
      y_cols <- intersect(c("y", "ymin", "ymax", "yend"), names(.x))
      if (length(y_cols) == 0) {
        return(numeric())
      }
      unlist(.x[y_cols], use.names = FALSE)
    }) %>%
    unlist(use.names = FALSE)

  y_values <- y_values[is.finite(y_values)]

  if (length(y_values) == 0) {
    return(c(-1, 1))
  }

  y_range <- range(c(y_values, 0), na.rm = TRUE)
  y_span <- diff(y_range)

  if (!is.finite(y_span) || y_span == 0) {
    y_span <- max(abs(y_range), 1)
  }

  y_range + c(-padding, padding) * y_span
}

plot_dynamic_event_study <- function(es, outcome, facility_set, timing,
                                     sample_annotation) {
  plot_base <- did::ggdid(es) +
    annotate(
      "label",
      x = Inf,
      y = Inf,
      label = sample_annotation,
      hjust = 1.05,
      vjust = 1.05,
      size = 3,
      linewidth = 0.2,
      alpha = 0.9
    ) +
    labs(
      x = "Relative Time",
      y = "Effect",
      title = str_wrap(
        paste(
          "AMWS event study - scientific facilities -",
          facility_set,
          timing,
          outcome
        ),
        width = 72
      )
    )

  plot_scaled <- suppressMessages(
    plot_base +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
  )

  plot_scaled +
    coord_cartesian(ylim = plot_y_limits(plot_base), clip = "off")
}

prepare_facilities <- function(facilities_df) {
  facilities_df %>%
    filter(
      !is.na(year),
      year <= treatment_year_cutoff,
      !is.na(lon),
      !is.na(lat)
    ) %>%
    mutate(
      decade_std = floor(year / 10) * 10,
      decade_shift = ifelse(
        year %% 10 >= 7,
        floor(year / 10) * 10 + 10,
        floor(year / 10) * 10
      )
    )
}

build_facility_treatment <- function(facilities_df, counties_poly, radius_m) {
  fac_buf <- facilities_df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(3857) %>%
    st_buffer(dist = radius_m) %>%
    mutate(fac_idx = row_number()) %>%
    select(fac_idx, decade_std, decade_shift)

  st_join(counties_poly, fac_buf, join = st_intersects) %>%
    st_drop_geometry() %>%
    filter(!is.na(fac_idx)) %>%
    group_by(GEOID) %>%
    summarise(
      g_std = min(decade_std),
      g_shift = min(decade_shift),
      .groups = "drop"
    )
}

make_analysis_panel <- function(panel, treatment) {
  panel %>%
    left_join(treatment, by = "GEOID") %>%
    mutate(
      g_std = replace_na(g_std, 0),
      g_shift = replace_na(g_shift, 0),
      g_std = restrict_treatment_cohort(g_std),
      g_shift = restrict_treatment_cohort(g_shift),
      GEOID_num = as.numeric(GEOID)
    )
}

run_event_study <- function(data, outcome, facility_set, timing, gname,
                            n_facility_events,
                            min_event_time = -70, max_event_time = 40,
                            cores = 4) {
  data_es <- data %>%
    select(GEOID_num, GEOID, decade, all_of(gname), all_of(outcome)) %>%
    rename(g = all_of(gname), y = all_of(outcome)) %>%
    filter(!is.na(g), !is.na(y), is.finite(y))

  n_events <- n_distinct(data_es$GEOID_num[data_es$g > 0])

  if (n_distinct(data_es$g[data_es$g > 0]) == 0) {
    return(list(
      ok = FALSE,
      outcome = outcome,
        facility_set = facility_set,
        timing = timing,
        n_events = n_events,
        n_facility_events = n_facility_events,
        error = "No treated cohorts in estimation sample."
      ))
  }

  if (n_distinct(data_es$y, na.rm = TRUE) < 2) {
    return(list(
      ok = FALSE,
      outcome = outcome,
        facility_set = facility_set,
        timing = timing,
        n_events = n_events,
        n_facility_events = n_facility_events,
        error = "Outcome has insufficient variation."
      ))
  }

  tryCatch(
    {
      out <- did::att_gt(
        yname = "y",
        tname = "decade",
        idname = "GEOID_num",
        gname = "g",
        data = data_es,
        control_group = control_group,
        est_method = est_method,
        base_period = "universal",
        cores = cores
      )

      es <- did::aggte(
        out,
        type = "dynamic",
        na.rm = TRUE,
        min_e = min_event_time,
        max_e = max_event_time
      )

      simple <- did::aggte(
        out,
        type = "simple",
        na.rm = TRUE,
        min_e = 0,
        max_e = max_event_time
      )

      list(
        ok = TRUE,
        outcome = outcome,
        facility_set = facility_set,
        timing = timing,
        out = out,
        es = es,
        simple = simple,
        n_events = n_events,
        n_facility_events = n_facility_events,
        n_rows = nrow(data_es),
        n_units = n_distinct(data_es$GEOID_num),
        n_treated_units = n_distinct(data_es$GEOID_num[data_es$g > 0]),
        n_control_units = n_distinct(data_es$GEOID_num[data_es$g == 0]),
        n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
        min_decade = min(data_es$decade, na.rm = TRUE),
        max_decade = max(data_es$decade, na.rm = TRUE),
        error = NA_character_
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        outcome = outcome,
        facility_set = facility_set,
        timing = timing,
        n_events = n_events,
        n_facility_events = n_facility_events,
        n_rows = nrow(data_es),
        n_units = n_distinct(data_es$GEOID_num),
        n_treated_units = n_distinct(data_es$GEOID_num[data_es$g > 0]),
        n_control_units = n_distinct(data_es$GEOID_num[data_es$g == 0]),
        n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
        min_decade = min(data_es$decade, na.rm = TRUE),
        max_decade = max(data_es$decade, na.rm = TRUE),
        error = conditionMessage(e)
      )
    }
  )
}

###############################################################################
# Load AMWS panel and aggregate to county-decade
###############################################################################

panel_year <- read_csv(
  file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    GEOID = as_geoid(GEOID),
    year = as.integer(year),
    decade = floor(year / 10) * 10
  )

panel <- panel_year %>%
  group_by(GEOID, decade) %>%
  summarise(
    n_amws = sum(replace_na(n_amws, 0), na.rm = TRUE),
    population = mean_or_na(population),
    county_births_estimate = sum(
      replace_na(county_births_estimate_year, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    amws_per_1000_births = if_else(
      county_births_estimate > 0,
      1000 * n_amws / county_births_estimate,
      NA_real_
    ),
    amws_per_100k = if_else(
      population > 0,
      100000 * n_amws / population,
      NA_real_
    ),
    log1p_n_amws = log1p(n_amws)
  )

complete_counties <- panel %>%
  filter(decade %in% analysis_decades) %>%
  distinct(GEOID, decade) %>%
  count(GEOID, name = "n_decades") %>%
  filter(n_decades == length(analysis_decades)) %>%
  select(GEOID)

panel <- panel %>%
  semi_join(complete_counties, by = "GEOID") %>%
  filter(decade %in% analysis_decades)

panel_geoids <- unique(panel$GEOID)

###############################################################################
# Build scientific-facility treatment assignment
###############################################################################

facilities <- read_delim(
  file.path(DATA_OUTPUT, "facilities_us.csv"),
  delim = ";",
  locale = locale(decimal_mark = ".", grouping_mark = ""),
  show_col_types = FALSE
) %>%
  prepare_facilities()

facilities_alt <- read_delim(
  file.path(DATA_OUTPUT, "facilities_us_alt.csv"),
  delim = ";",
  locale = locale(decimal_mark = ".", grouping_mark = ""),
  show_col_types = FALSE
) %>%
  prepare_facilities()

radius_m <- 50000

counties_poly <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_transform(3857) %>%
  select(GEOID, geometry) %>%
  filter(as.integer(substr(GEOID, 1, 2)) <= 56)

treatment_full <- build_facility_treatment(facilities, counties_poly, radius_m)
treatment_selected <- build_facility_treatment(
  facilities_alt,
  counties_poly,
  radius_m
)

panel_full <- make_analysis_panel(panel, treatment_full)
panel_selected <- make_analysis_panel(panel, treatment_selected)

write_csv(
  treatment_full %>%
    mutate(
      facility_set = "full_facilities",
      g_std_unrestricted = g_std,
      g_shift_unrestricted = g_shift,
      g_std = restrict_treatment_cohort(g_std),
      g_shift = restrict_treatment_cohort(g_shift)
    ),
  file.path(results_dir, "treatment_assignment_full_facilities.csv")
)
write_csv(
  treatment_selected %>%
    mutate(
      facility_set = "selected_facilities",
      g_std_unrestricted = g_std,
      g_shift_unrestricted = g_shift,
      g_std = restrict_treatment_cohort(g_std),
      g_shift = restrict_treatment_cohort(g_shift)
    ),
  file.path(results_dir, "treatment_assignment_selected_facilities.csv")
)

###############################################################################
# Event studies
###############################################################################

outcomes <- c(
  "amws_per_100k",
  "amws_per_1000_births",
  "n_amws",
  "log1p_n_amws"
)

specs <- list(
  list(
    facility_set = "full_facilities",
    timing = "standard_decade",
    gname = "g_std",
    data = panel_full,
    n_facility_events = count_supported_facility_events(
      facilities,
      counties_poly,
      panel_geoids,
      "standard_decade",
      radius_m
    )
  ),
  list(
    facility_set = "full_facilities",
    timing = "alternative_decade",
    gname = "g_shift",
    data = panel_full,
    n_facility_events = count_supported_facility_events(
      facilities,
      counties_poly,
      panel_geoids,
      "alternative_decade",
      radius_m
    )
  ),
  list(
    facility_set = "selected_facilities",
    timing = "standard_decade",
    gname = "g_std",
    data = panel_selected,
    n_facility_events = count_supported_facility_events(
      facilities_alt,
      counties_poly,
      panel_geoids,
      "standard_decade",
      radius_m
    )
  ),
  list(
    facility_set = "selected_facilities",
    timing = "alternative_decade",
    gname = "g_shift",
    data = panel_selected,
    n_facility_events = count_supported_facility_events(
      facilities_alt,
      counties_poly,
      panel_geoids,
      "alternative_decade",
      radius_m
    )
  )
)

model_results <- list()

for (spec in specs) {
  for (outcome in outcomes) {
    key <- paste(outcome, spec$facility_set, spec$timing, sep = "__")
    message(
      "Running AMWS scientific-facilities event study: ",
      outcome,
      " | ",
      spec$facility_set,
      " | ",
      spec$timing
    )

    model_results[[key]] <- run_event_study(
      data = spec$data,
      outcome = outcome,
      facility_set = spec$facility_set,
      timing = spec$timing,
      gname = spec$gname,
      n_facility_events = spec$n_facility_events,
      min_event_time = -pre_event_window,
      max_event_time = post_event_window,
      cores = 4
    )
  }
}

successful_models <- keep(model_results, "ok")

model_status <- imap_dfr(
  model_results,
  ~ tibble(
    outcome = .x$outcome,
    facility_set = .x$facility_set,
    timing = .x$timing,
    control_group = control_group,
    est_method = est_method,
    max_treatment_cohort = max_treatment_cohort,
    treatment_year_cutoff = treatment_year_cutoff,
    analysis_min_decade = analysis_min_decade,
    analysis_max_decade = analysis_max_decade,
    min_supported_cohort = min_supported_cohort,
    max_supported_cohort = max_supported_cohort,
    max_post_event_time = post_event_window,
    ok = .x$ok,
    n_events = value_or(.x$n_events, NA_integer_),
    n_facility_events = value_or(.x$n_facility_events, NA_integer_),
    n_rows = value_or(.x$n_rows, NA_integer_),
    n_units = value_or(.x$n_units, NA_integer_),
    n_treated_units = value_or(.x$n_treated_units, NA_integer_),
    n_control_units = value_or(.x$n_control_units, NA_integer_),
    n_treated_cohorts = value_or(.x$n_treated_cohorts, NA_integer_),
    min_decade = value_or(.x$min_decade, NA_real_),
    max_decade = value_or(.x$max_decade, NA_real_),
    error = value_or(.x$error, NA_character_)
  )
)

if (length(successful_models) == 0) {
  write_csv(model_status, file.path(results_dir, "model_status.csv"))
  stop("No AMWS scientific-facilities event-study model ran successfully.")
}

dynamic_att <- imap_dfr(
  successful_models,
  ~ extract_dynamic_att(.x$es, .x$outcome, .x$facility_set, .x$timing)
)

simple_att <- imap_dfr(
  successful_models,
  ~ extract_simple_att(.x$simple, .x$outcome, .x$facility_set, .x$timing)
)

event_distribution <- bind_rows(
  panel_full %>%
    distinct(GEOID, g_std, g_shift) %>%
    count(g_std, name = "n_counties") %>%
    mutate(
      facility_set = "full_facilities",
      timing = "standard_decade",
      cohort = if_else(is.na(g_std), "unsupported", as.character(g_std))
    ) %>%
    select(facility_set, timing, cohort, n_counties),
  panel_full %>%
    distinct(GEOID, g_std, g_shift) %>%
    count(g_shift, name = "n_counties") %>%
    mutate(
      facility_set = "full_facilities",
      timing = "alternative_decade",
      cohort = if_else(is.na(g_shift), "unsupported", as.character(g_shift))
    ) %>%
    select(facility_set, timing, cohort, n_counties),
  panel_selected %>%
    distinct(GEOID, g_std, g_shift) %>%
    count(g_std, name = "n_counties") %>%
    mutate(
      facility_set = "selected_facilities",
      timing = "standard_decade",
      cohort = if_else(is.na(g_std), "unsupported", as.character(g_std))
    ) %>%
    select(facility_set, timing, cohort, n_counties),
  panel_selected %>%
    distinct(GEOID, g_std, g_shift) %>%
    count(g_shift, name = "n_counties") %>%
    mutate(
      facility_set = "selected_facilities",
      timing = "alternative_decade",
      cohort = if_else(is.na(g_shift), "unsupported", as.character(g_shift))
    ) %>%
    select(facility_set, timing, cohort, n_counties)
)

sample_summary <- bind_rows(
  panel_full %>%
    summarise(
      facility_set = "full_facilities",
      n_rows = n(),
      n_counties = n_distinct(GEOID),
      min_decade = min(decade, na.rm = TRUE),
      max_decade = max(decade, na.rm = TRUE),
      total_amws = sum(n_amws, na.rm = TRUE),
      n_events_std = n_distinct(GEOID[!is.na(g_std) & g_std > 0]),
      n_events_shift = n_distinct(GEOID[!is.na(g_shift) & g_shift > 0]),
      n_facility_events_std = count_supported_facility_events(
        facilities,
        counties_poly,
        panel_geoids,
        "standard_decade",
        radius_m
      ),
      n_facility_events_shift = count_supported_facility_events(
        facilities,
        counties_poly,
        panel_geoids,
        "alternative_decade",
        radius_m
      ),
      n_treated_std = n_distinct(GEOID[!is.na(g_std) & g_std > 0]),
      n_treated_shift = n_distinct(GEOID[!is.na(g_shift) & g_shift > 0]),
      n_controls_std = n_distinct(GEOID[!is.na(g_std) & g_std == 0]),
      n_controls_shift = n_distinct(GEOID[!is.na(g_shift) & g_shift == 0]),
      n_unsupported_std = n_distinct(GEOID[is.na(g_std)]),
      n_unsupported_shift = n_distinct(GEOID[is.na(g_shift)]),
      control_group = control_group,
      est_method = est_method,
      treatment_year_cutoff = treatment_year_cutoff,
      max_treatment_cohort = max_treatment_cohort,
      analysis_min_decade = analysis_min_decade,
      analysis_max_decade = analysis_max_decade,
      min_supported_cohort = min_supported_cohort,
      max_supported_cohort = max_supported_cohort,
      balanced_panel = TRUE,
      max_post_event_time = post_event_window
    ),
  panel_selected %>%
    summarise(
      facility_set = "selected_facilities",
      n_rows = n(),
      n_counties = n_distinct(GEOID),
      min_decade = min(decade, na.rm = TRUE),
      max_decade = max(decade, na.rm = TRUE),
      total_amws = sum(n_amws, na.rm = TRUE),
      n_events_std = n_distinct(GEOID[!is.na(g_std) & g_std > 0]),
      n_events_shift = n_distinct(GEOID[!is.na(g_shift) & g_shift > 0]),
      n_facility_events_std = count_supported_facility_events(
        facilities_alt,
        counties_poly,
        panel_geoids,
        "standard_decade",
        radius_m
      ),
      n_facility_events_shift = count_supported_facility_events(
        facilities_alt,
        counties_poly,
        panel_geoids,
        "alternative_decade",
        radius_m
      ),
      n_treated_std = n_distinct(GEOID[!is.na(g_std) & g_std > 0]),
      n_treated_shift = n_distinct(GEOID[!is.na(g_shift) & g_shift > 0]),
      n_controls_std = n_distinct(GEOID[!is.na(g_std) & g_std == 0]),
      n_controls_shift = n_distinct(GEOID[!is.na(g_shift) & g_shift == 0]),
      n_unsupported_std = n_distinct(GEOID[is.na(g_std)]),
      n_unsupported_shift = n_distinct(GEOID[is.na(g_shift)]),
      control_group = control_group,
      est_method = est_method,
      treatment_year_cutoff = treatment_year_cutoff,
      max_treatment_cohort = max_treatment_cohort,
      analysis_min_decade = analysis_min_decade,
      analysis_max_decade = analysis_max_decade,
      min_supported_cohort = min_supported_cohort,
      max_supported_cohort = max_supported_cohort,
      balanced_panel = TRUE,
      max_post_event_time = post_event_window
    )
)

write_csv(dynamic_att, file.path(results_dir, "dynamic_att.csv"))
write_csv(simple_att, file.path(results_dir, "simple_att_summary.csv"))
write_csv(model_status, file.path(results_dir, "model_status.csv"))
write_csv(event_distribution, file.path(results_dir, "event_distribution.csv"))
write_csv(sample_summary, file.path(results_dir, "sample_summary.csv"))

for (model in successful_models) {
  plot_es <- plot_dynamic_event_study(
    model$es,
    model$outcome,
    model$facility_set,
    model$timing,
    format_sample_annotation(model)
  )

  ggsave(
    file.path(
      results_dir,
      paste0(
        "ES_amws_scientific_facilities_",
        sanitize_filename(model$outcome),
        "_",
        sanitize_filename(model$facility_set),
        "_",
        sanitize_filename(model$timing),
        ".png"
      )
    ),
    plot_es,
    width = 8,
    height = 6,
    dpi = 300
  )
}

notes <- c(
  "AMWS scientific-facilities event studies",
  "",
  paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("DATA_OUTPUT: ", DATA_OUTPUT),
  paste0("AMWS panel: ", file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")),
  paste0("Radius: ", radius_m / 1000, "km"),
  paste0("Facility-year cutoff: year <= ", treatment_year_cutoff, "."),
  "Treatment: first decade in which a 2020 county polygon intersects a scientific-facility buffer.",
  "Timing variables: g_std = floor(year / 10) * 10; g_shift shifts years ending in 7-9 to next decade.",
  paste0("Estimator: did::att_gt, control_group = ", control_group, ", est_method = ", est_method, ", base_period = universal."),
  paste0("Panel handling: balanced county-decade panel over ", analysis_min_decade, "-", analysis_max_decade, "."),
  paste0("Restricted treatment cohorts: ", min_supported_cohort, " <= g <= ", max_supported_cohort, "; unsupported cohorts are excluded from the estimation sample."),
  "Figure annotation: Events counts treated county events; n_facility_events is reported separately in model_status and sample_summary.",
  "Simple ATT summary restricted to event times 0 to +30.",
  paste0("Dynamic event-time window: -", pre_event_window, " to +", post_event_window, "."),
  paste0("Outcomes: ", paste(outcomes, collapse = ", ")),
  paste0("Successful models: ", sum(model_status$ok), " / ", nrow(model_status)),
  paste0(
    "Elapsed minutes: ",
    round(difftime(Sys.time(), initial_time, units = "mins"), 1)
  )
)

writeLines(notes, file.path(results_dir, "notes.txt"))

message("Saved AMWS scientific-facilities outputs in: ", results_dir)
message(
  "Done. Elapsed: ",
  round(difftime(Sys.time(), initial_time, units = "mins"), 1),
  " min"
)
