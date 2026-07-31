###############################################################################
# Project: GTL Talent Determinants
# Goal: UK historical urban-unit synthetic DiD estimates using distance to the
#       first Crystal Palace world's fair venue.
#
# Treatment:
#   - Exposure to The Great Exhibition of 1851 at Crystal Palace, London,
#     within distance bins:
#       0-2, 2-4, 4-6, 6-8, 8-10, 10-12, 12-14, 14-16, 16-18, 18-20 km.
#   - Controls are at least 50 km from the venue and matched on 1801 population
#     density and occupation shares.
#   - Units first exposed before 1840 are always-treated and excluded.
#   - Units first exposed after 1910 are future-treated and excluded.
#   - US counties are excluded from the panel and control group.
#   - Greater London is included as an outcome unit using the Nomis/ONS 1921
#     boundary definition selected by >=50% overlap with 1911 Greater London.
#
# Run from analysis/ or repo root:
#   Rscript analysis/world_fairs/synthdid/worlds_fairs_venue_distance_crystal_palace_1851_synthdid_0_20km_uk_only.R
# Law-Robson-only population robustness:
#   SYNTHDID_POPULATION_SAMPLE=law_robson_only Rscript analysis/world_fairs/synthdid/worlds_fairs_venue_distance_crystal_palace_1851_synthdid_0_20km_uk_only.R
# Recalculate 1801 matching density from observed Law-Robson population:
#   SYNTHDID_POPULATION_SAMPLE=law_robson_only SYNTHDID_DENSITY_SOURCE=observed_population Rscript analysis/world_fairs/synthdid/worlds_fairs_venue_distance_crystal_palace_1851_synthdid_0_20km_uk_only.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  if (!requireNamespace("synthdid", quietly = TRUE)) {
    stop(
      "Package 'synthdid' is required. Install it with ",
      "remotes::install_github('synth-inference/synthdid') or the local project convention."
    )
  }
  library(synthdid)
  library(sf)
})

initial_time <- Sys.time()
options(timeout = 1000)
sf_use_s2(FALSE)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
  if (basename(repo_root) == "synthdid" &&
      basename(dirname(repo_root)) == "world_fairs" &&
      basename(dirname(dirname(repo_root))) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, "..", "..", ".."), winslash = "/", mustWork = TRUE)
  }
  if (basename(repo_root) == "world_fairs" && basename(dirname(repo_root)) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, "..", ".."), winslash = "/", mustWork = TRUE)
  }
  if (basename(repo_root) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, ".."), winslash = "/", mustWork = TRUE)
  }
}

source(file.path(repo_root, "paths.R"))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(user_data_dir, winslash = "/", mustWork = TRUE)
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

###############################################################################
# Paths and constants
###############################################################################

data_processed <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed")
donor_min_distance_km <- as.numeric(Sys.getenv(
  "SYNTHDID_DONOR_MIN_DISTANCE_KM", unset = "50"
))
donor_density_ratio <- as.numeric(Sys.getenv(
  "SYNTHDID_DONOR_DENSITY_RATIO", unset = "2"
))
donor_occupation_caliper_pp <- as.numeric(Sys.getenv(
  "SYNTHDID_DONOR_OCCUPATION_CALIPER_PP", unset = "10"
))
donor_match_mode <- tolower(Sys.getenv(
  "SYNTHDID_DONOR_MATCH_MODE", unset = "demographic"
))
min_density_area_coverage <- as.numeric(Sys.getenv(
  "SYNTHDID_MIN_DENSITY_AREA_COVERAGE", unset = "0.95"
))
population_sample <- tolower(Sys.getenv(
  "SYNTHDID_POPULATION_SAMPLE", unset = "all"
))
density_source <- tolower(Sys.getenv(
  "SYNTHDID_DENSITY_SOURCE", unset = "swing"
))
donor_audit_only <- tolower(Sys.getenv(
  "SYNTHDID_DONOR_AUDIT_ONLY", unset = "false"
)) %in% c("1", "true", "yes", "y")
anticipation_decades <- as.integer(Sys.getenv(
  "SYNTHDID_ANTICIPATION_DECADES", unset = "0"
))
if (!is.finite(donor_min_distance_km) || donor_min_distance_km <= 20 ||
    !is.finite(donor_density_ratio) || donor_density_ratio <= 1 ||
    !is.finite(donor_occupation_caliper_pp) ||
      donor_occupation_caliper_pp < 0 ||
    !donor_match_mode %in% c("demographic", "density_only") ||
    !population_sample %in% c("all", "law_robson_only") ||
    !density_source %in% c("swing", "observed_population") ||
    (density_source == "observed_population" &&
      population_sample != "law_robson_only") ||
    !is.finite(min_density_area_coverage) ||
      min_density_area_coverage < 0 || min_density_area_coverage > 1 ||
    is.na(anticipation_decades) || anticipation_decades < 0L) {
  stop("Invalid restricted-donor configuration.")
}
anticipation_years <- 10L * anticipation_decades
analysis_end_decade <- 1930L
analysis_year_max <- analysis_end_decade + 9L
expected_treated_units <- 42L
expected_donor_units <- 101L
expected_donor_units_occ6_5 <- 43L
expected_donor_units_density_only_ge30 <- 472L
expected_law_robson_treated_units <- 4L
expected_law_robson_donor_units <- 28L
expected_law_robson_donor_units_density_only_ge30 <- 94L
expected_law_robson_observed_density_donor_units <- 34L
expected_law_robson_observed_density_donor_units_density_only_ge30 <- 120L
result_spec_tag <- paste0(
  "ge", format(donor_min_distance_km, trim = TRUE, scientific = FALSE),
  "km_density", format(donor_density_ratio, trim = TRUE, scientific = FALSE),
  "x_",
  if (donor_match_mode == "demographic") {
    paste0(
      "occ", format(
        donor_occupation_caliper_pp, trim = TRUE, scientific = FALSE
      ), "pp"
    )
  } else {
    "density_only"
  },
  "_cov", format(100 * min_density_area_coverage, trim = TRUE, scientific = FALSE),
  if (population_sample == "law_robson_only" &&
      density_source == "observed_population") {
    "_lawobsdens1801"
  } else if (population_sample == "law_robson_only") {
    "_lawrobson_only"
  } else {
    ""
  },
  "_pop1801_1800_", analysis_end_decade,
  if (anticipation_decades > 0L) {
    paste0("_anticipation", anticipation_decades, "decades")
  } else {
    ""
  }
)
results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results",
  "worlds_fair",
  "synthdid",
  paste0(
    "cp1851_sdid_0_20km_",
    result_spec_tag
  )
)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
if (!donor_audit_only) {
  audit_only_file <- file.path(results_dir, "audit_only_notes.txt")
  if (file.exists(audit_only_file)) unlink(audit_only_file)
}

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
panel_file <- file.path(
  data_processed,
  "uk_historical_urban_units_us_county_inventor_panel_1800_1960_nhgis_us.csv"
)
fairs_file <- file.path(
  DATA_INPUT,
  "worlds_fairs",
  "worlds_fairs_1790_1960_with_visits_venues.csv"
)
boundary_gpkg <- file.path(
  gbr_dir,
  "raw",
  "historical_boundaries",
  "uk_historical_districts_1921_1961.gpkg"
)
greater_london_crosswalk_file <- file.path(
  gbr_dir,
  "raw",
  "arcgis_english_admin_boundaries_1911",
  "greater_london_1911_to_nomis_1921_crosswalk.csv"
)

required_files <- c(panel_file, fairs_file, boundary_gpkg, greater_london_crosswalk_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

greater_london_id <- "GBR_HIST_URBAN_GREATER_LONDON"
target_types <- c("Urban District", "Municipal Borough", "County Borough")
crystal_palace_1851_fair_id <- 23L
crystal_palace_1851_latitude <- 51.50241
crystal_palace_1851_longitude <- -0.17049
crystal_palace_1851_coordinate_source <- "Geograph/Read the Plaque - Great Exhibition site marker, Hyde Park"
crystal_palace_1851_coordinate_note <- paste(
  "Manual override for the 1851 Hyde Park Crystal Palace site.",
  "The consolidated fair file geocodes the name Crystal Palace to the later Sydenham site;",
  "this specification uses the Hyde Park Great Exhibition site marker instead."
)
max_treatment_distance_km <- 20
max_treatment_distance_m <- max_treatment_distance_km * 1000
aggregate_bin_label <- "0-20"
zoomed_display_y_limits <- c(-10, 10)
bin_breaks <- c(-1e-9, seq(2, max_treatment_distance_km, by = 2))
bin_labels <- paste(seq(0, max_treatment_distance_km - 2, by = 2),
                    seq(2, max_treatment_distance_km, by = 2),
                    sep = "-")
bin_dirs <- c(
  "0-2" = "bin_0_2km",
  "2-4" = "bin_2_4km",
  "4-6" = "bin_4_6km",
  "6-8" = "bin_6_8km",
  "8-10" = "bin_8_10km",
  "10-12" = "bin_10_12km",
  "12-14" = "bin_12_14km",
  "14-16" = "bin_14_16km",
  "16-18" = "bin_16_18km",
  "18-20" = "bin_18_20km",
  "0-20" = "bin_0_20km"
)
analysis_bin_labels <- c(bin_labels, aggregate_bin_label)
selected_bin_env <- Sys.getenv("SYNTHDID_BINS", unset = aggregate_bin_label)
selected_analysis_bin_labels <- if (selected_bin_env == "") {
  aggregate_bin_label
} else {
  str_split(selected_bin_env, ",", simplify = FALSE)[[1]] %>%
    str_trim() %>%
    discard(~ .x == "")
}
bad_selected_bins <- setdiff(selected_analysis_bin_labels, analysis_bin_labels)
if (length(bad_selected_bins) > 0L) {
  stop("Unknown SYNTHDID_BINS: ", paste(bad_selected_bins, collapse = ", "))
}
if (!identical(selected_analysis_bin_labels, aggregate_bin_label)) {
  stop("This restricted-donor specification must run only SYNTHDID_BINS=0-20.")
}
selected_bins_active <- !identical(selected_analysis_bin_labels, analysis_bin_labels)
aggregate_suffix <- if (selected_bins_active) "_selected_bins" else "_all_bins"
classification_year_min <- 1790L
classification_year_max <- 1961L
treated_event_year_min <- 1840L
treated_event_year_max <- 1910L
panel_year_min <- 1800L
panel_year_max <- analysis_year_max
control_group_name <- paste0(
  "restricted_ge", format(donor_min_distance_km, trim = TRUE), "km_",
  donor_match_mode, "_match"
)
compute_placebo_se <- tolower(Sys.getenv("SYNTHDID_PLACEBO_SE", unset = "true")) %in%
  c("1", "true", "yes", "y")
synthdid_se_replications <- as.integer(Sys.getenv("SYNTHDID_SE_REPLICATIONS", unset = "200"))
if (is.na(synthdid_se_replications) || synthdid_se_replications <= 0L) {
  stop("SYNTHDID_SE_REPLICATIONS must be a positive integer.")
}
curve_placebo_replications <- as.integer(Sys.getenv(
  "SYNTHDID_CURVE_PLACEBO_REPLICATIONS",
  unset = as.character(synthdid_se_replications)
))
if (is.na(curve_placebo_replications) || curve_placebo_replications < 0L) {
  stop("SYNTHDID_CURVE_PLACEBO_REPLICATIONS must be a non-negative integer.")
}
plot_package_placebo_ci <- tolower(Sys.getenv("SYNTHDID_PLOT_PLACEBO_CI", unset = "false")) %in%
  c("1", "true", "yes", "y")

outcomes <- c(
  "inventors_per_100k_pop",
  "stem_per_100k_pop",
  "n_inventors",
  "log1p_n_inventors",
  "n_stem",
  "log1p_n_stem",
  "population",
  "log_population"
)

###############################################################################
# Helpers
###############################################################################

value_or <- function(x, default) {
  if (is.null(x)) default else x
}

mean_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0L) return(NA_character_)
  as.character(x[[1L]])
}

first_nonmissing_numeric <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  x[[1L]]
}

standard_decade <- function(year) {
  as.integer(floor(year / 10) * 10)
}

sanitize_filename <- function(x) {
  str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

pad_geoid <- function(x) {
  x_chr <- as.character(x)
  x_chr <- if_else(
    is.na(x_chr) | x_chr == "",
    NA_character_,
    str_pad(str_replace(x_chr, "\\.0$", ""), 5, pad = "0")
  )
  x_chr
}

extract_synthdid_att <- function(model) {
  estimate <- as.numeric(model$estimate)
  se <- suppressWarnings(as.numeric(model$se))
  tibble(
    outcome = model$outcome,
    distance_bin_km = model$distance_bin_km,
    estimator = "synthdid",
    se_method = model$se_method,
    se_replications = model$se_replications,
    treatment_decade = model$treatment_decade,
    actual_event_decade = model$actual_event_decade,
    anticipation_decades = anticipation_decades,
    estimate = estimate,
    se = se,
    p_value = if_else(is.finite(se) & se > 0, 2 * (1 - pnorm(abs(estimate / se))), NA_real_),
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    n0 = model$n0,
    n1 = model$n1,
    t0 = model$t0,
    t1 = model$t1,
    min_decade = model$min_decade,
    max_decade = model$max_decade
  )
}

format_att_annotation <- function(model) {
  estimate <- as.numeric(model$estimate)
  se <- suppressWarnings(as.numeric(model$se))

  if (is.finite(se) && se > 0) {
    ci_low <- estimate - 1.96 * se
    ci_high <- estimate + 1.96 * se
    paste(
      paste0("ATT: ", signif(estimate, 4)),
      paste0("SE: ", signif(se, 4)),
      paste0("95% CI: [", signif(ci_low, 4), ", ", signif(ci_high, 4), "]"),
      paste0("SE method: ", model$se_method, " (", model$se_replications, " reps)"),
      sep = "\n"
    )
  } else {
    paste(
      paste0("ATT: ", signif(estimate, 4)),
      "SE: not computed",
      sep = "\n"
    )
  }
}

dynamic_y_limits <- function(effect_curve) {
  y_values <- effect_curve %>%
    select(any_of(c("effect", "ci_low", "ci_high"))) %>%
    unlist(use.names = FALSE)
  y_values <- y_values[is.finite(y_values)]
  max_abs <- max(abs(y_values), na.rm = TRUE)
  if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
  c(-1.1 * max_abs, 1.1 * max_abs)
}

n_distinct_nonmissing <- function(x) {
  length(unique(x[!is.na(x) & x != ""]))
}

summarise_effective_sample <- function(data_es, outcome, bin_label) {
  event_id <- coalesce(
    as.character(data_es$first_parent_fair_id),
    as.character(data_es$first_fair_id)
  )

  tibble(
    outcome = outcome,
    distance_bin_km = bin_label,
    n_events = n_distinct_nonmissing(event_id[data_es$g > 0]),
    n_treated_gbr = n_distinct(data_es$unit_id[data_es$g > 0 & data_es$iso3 == "GBR"]),
    n_treated_usa = n_distinct(data_es$unit_id[data_es$g > 0 & data_es$iso3 == "USA"]),
    n_control_gbr = n_distinct(data_es$unit_id[data_es$g == 0 & data_es$iso3 == "GBR"]),
    n_control_usa = n_distinct(data_es$unit_id[data_es$g == 0 & data_es$iso3 == "USA"])
  )
}

format_sample_annotation <- function(sample_summary) {
  paste(
    paste0("Events: ", sample_summary$n_events),
    paste0("Treated UK: ", sample_summary$n_treated_gbr),
    paste0("Treated US: ", sample_summary$n_treated_usa),
    paste0("Control UK: ", sample_summary$n_control_gbr),
    paste0("Control US: ", sample_summary$n_control_usa),
    sep = "\n"
  )
}

extract_effect_curve <- function(model) {
  curve <- as.numeric(synthdid::synthdid_effect_curve(model$estimate_obj))
  post_periods <- model$time_periods[(model$t0 + 1L):length(model$time_periods)]
  if (length(curve) != length(post_periods)) {
    post_periods <- seq_len(length(curve)) + model$t0
  }

  observed <- tibble(
    outcome = model$outcome,
    distance_bin_km = model$distance_bin_km,
    estimator = "synthdid",
    decade = as.integer(post_periods),
    event_time = as.integer(post_periods - model$actual_event_decade),
    effect = curve
  )

  if (is.null(model$placebo_effect_curve) || nrow(model$placebo_effect_curve) == 0L) {
    return(observed %>% mutate(se = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }

  placebo_se <- model$placebo_effect_curve %>%
    group_by(event_time) %>%
    summarise(se = sd(placebo_effect, na.rm = TRUE), .groups = "drop")

  observed %>%
    left_join(placebo_se, by = "event_time") %>%
    mutate(
      ci_low = effect - 1.96 * se,
      ci_high = effect + 1.96 * se
    )
}

extract_synthdid_controls <- function(model, weight_type = c("omega", "lambda")) {
  weight_type <- match.arg(weight_type)
  out <- tryCatch(
    synthdid::synthdid_controls(
      model$estimate_obj,
      mass = 1,
      weight.type = weight_type
    ),
    error = function(e) tibble(error = conditionMessage(e))
  )

  id_col <- if (weight_type == "omega") "control_unit_id" else "pre_period"

  as.data.frame(out) %>%
    rownames_to_column(id_col) %>%
    as_tibble() %>%
    rename(weight = 2) %>%
    mutate(
      outcome = model$outcome,
      distance_bin_km = model$distance_bin_km,
      estimator = "synthdid",
      weight_type = weight_type,
      .before = 1
    )
}

plot_effect_curve <- function(effect_curve, outcome, bin_label, y_limits, sample_annotation,
                              att_annotation) {
  annotation <- paste(sample_annotation, att_annotation, sep = "\n\n")
  anticipation_layer <- if (anticipation_years > 0L) {
    geom_vline(
      xintercept = -anticipation_years,
      linetype = "dashed",
      color = "#d95f02"
    )
  } else {
    NULL
  }

  ggplot(effect_curve, aes(x = event_time, y = effect)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
    anticipation_layer +
    geom_vline(xintercept = 0, linetype = "dotted", color = "#333333") +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.18, fill = "#1f78b4", na.rm = TRUE) +
    geom_line(linewidth = 0.7, color = "#1f78b4") +
    geom_point(size = 2, color = "#1f78b4") +
    annotate(
      "label",
      x = Inf,
      y = Inf,
      label = annotation,
      hjust = 1.05,
      vjust = 1.05,
      size = 3,
      label.size = 0.2,
      alpha = 0.9
    ) +
    labs(
      x = "Relative time (years)",
      y = "Effect curve",
      title = str_wrap(
        paste(
          "World's fairs UK venue-distance synthdid effect curve",
          paste0("bin ", bin_label, " km"),
          outcome
        ),
        width = 72
      ),
      caption = paste(
        "Orange dashed line marks anticipated treatment onset;",
        "black dotted line marks the actual fair decade.",
        "Bands are pointwise 95% intervals from placebo curves where available."
      )
    ) +
    theme_minimal(base_size = 12) +
    coord_cartesian(ylim = y_limits)
}

plot_synthdid_fit <- function(model) {
  se_method <- if (isTRUE(plot_package_placebo_ci) && model$se_method == "placebo") {
    "placebo"
  } else {
    "none"
  }
  treatment_decade <- as.integer(model$treatment_decade)
  actual_event_decade <- as.integer(model$actual_event_decade)

  plot(model$estimate_obj, se.method = se_method) +
    annotate(
      "rect",
      xmin = treatment_decade,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "#1f78b4",
      alpha = 0.04
    ) +
    geom_vline(
      xintercept = treatment_decade,
      linewidth = 0.6,
      linetype = "dashed",
      color = "#333333"
    ) +
    geom_vline(
      xintercept = actual_event_decade,
      linewidth = 0.6,
      linetype = "dotted",
      color = "#d95f02"
    ) +
    annotate(
      "text",
      x = treatment_decade - 20,
      y = Inf,
      label = "Pre-treatment",
      hjust = 1,
      vjust = 2.1,
      size = 3.2,
      color = "#333333"
    ) +
    annotate(
      "text",
      x = treatment_decade + 20,
      y = Inf,
      label = "Post-treatment",
      hjust = 0,
      vjust = 2.1,
      size = 3.2,
      color = "#333333"
    ) +
    annotate(
      "label",
      x = Inf,
      y = Inf,
      label = format_att_annotation(model),
      hjust = 1.05,
      vjust = 1.05,
      size = 3,
      label.size = 0.2,
      alpha = 0.9
    ) +
    labs(
      title = str_wrap(
        paste(
          "World's fairs UK venue-distance synthetic DiD",
          paste0("bin ", model$distance_bin_km, " km"),
          model$outcome
        ),
        width = 72
      ),
      subtitle = paste0(
        "Anticipated treatment onset: ", treatment_decade,
        "; actual fair decade: ", actual_event_decade
      ),
      caption = paste(
        "Dashed line marks anticipated treatment onset; dotted line marks the actual fair.",
        "The lower panel shows pre-treatment time weights, not outcome levels."
      )
    )
}

plot_att_by_bin <- function(att_data, outcome) {
  plot_data <- att_data %>%
    filter(outcome == !!outcome) %>%
    mutate(distance_bin_km = factor(distance_bin_km, levels = analysis_bin_labels))

  ggplot(plot_data, aes(x = distance_bin_km, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high),
      width = 0.15,
      na.rm = TRUE,
      color = "#2f4f4f"
    ) +
    geom_point(size = 2.4, color = "#1f78b4") +
    labs(
      x = "Distance bin (km)",
      y = "Synthetic DiD ATT",
      title = str_wrap(
        paste("Crystal Palace 1851 synthetic DiD ATT by distance bin", outcome),
        width = 76
      ),
      caption = "Bars show 95% CIs from placebo SEs where available."
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}

check_balanced_panel <- function(data, id_col, time_col) {
  id_col <- rlang::ensym(id_col)
  time_col <- rlang::ensym(time_col)
  n_times <- data %>% summarise(n = n_distinct(!!time_col)) %>% pull(n)
  counts <- data %>% count(!!id_col, name = "n_periods")
  list(
    is_balanced = all(counts$n_periods == n_times),
    n_units = nrow(counts),
    n_periods = n_times,
    min_periods_per_unit = min(counts$n_periods, na.rm = TRUE),
    max_periods_per_unit = max(counts$n_periods, na.rm = TRUE)
  )
}

build_uk_target_geometries <- function() {
  districts_1921 <- st_read(boundary_gpkg, layer = "districts_1921", quiet = TRUE) %>%
    st_transform(27700) %>%
    st_make_valid()

  greater_london_boundary_ids <- read_csv(
    greater_london_crosswalk_file,
    show_col_types = FALSE,
    col_types = cols(.default = col_guess(), nomis_1921_id = col_character())
  ) %>%
    filter(in_greater_london_1911_main == TRUE) %>%
    pull(nomis_1921_id) %>%
    unique()

  if (length(greater_london_boundary_ids) == 0L) {
    stop("No Greater London 1911 main units found in crosswalk.")
  }

  greater_london_components <- districts_1921[
    districts_1921$boundary_id %in% greater_london_boundary_ids,
  ]

  missing_london_ids <- setdiff(greater_london_boundary_ids, greater_london_components$boundary_id)
  if (length(missing_london_ids) > 0L) {
    stop(
      "Greater London crosswalk ids missing from districts_1921:\n",
      paste(missing_london_ids, collapse = "\n")
    )
  }

  greater_london_sf <- st_sf(
    unit_id = greater_london_id,
    target_unit_id = greater_london_id,
    target_unit_name = "Greater London",
    target_area_type = "Greater London",
    target_boundary_id = greater_london_id,
    geo_country_iso3 = "GBR",
    GEOID = NA_character_,
    geometry = st_sfc(st_union(greater_london_components), crs = 27700)
  )

  base_targets <- districts_1921[districts_1921$boundary_type %in% target_types, ]
  base_targets <- base_targets[!(base_targets$boundary_id %in% greater_london_boundary_ids), ]

  base_targets_sf <- st_sf(
    unit_id = paste0("GBR_HIST_URBAN_", base_targets$boundary_id),
    target_unit_id = paste0("GBR_HIST_URBAN_", base_targets$boundary_id),
    target_unit_name = base_targets$boundary_name,
    target_area_type = base_targets$boundary_type,
    target_boundary_id = base_targets$boundary_id,
    geo_country_iso3 = "GBR",
    GEOID = NA_character_,
    geometry = st_geometry(base_targets)
  )

  bind_rows(base_targets_sf, greater_london_sf) %>%
    st_make_valid()
}

build_us_target_geometries <- function(panel_year) {
  us_units <- panel_year %>%
    filter(iso3 == "USA") %>%
    distinct(unit_id, GEOID, place_name) %>%
    mutate(GEOID = pad_geoid(GEOID)) %>%
    filter(!is.na(GEOID))

  counties_poly <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
    st_transform(5070) %>%
    select(GEOID, NAMELSAD, STATEFP, geometry) %>%
    filter(as.integer(STATEFP) <= 56) %>%
    mutate(GEOID = as.character(GEOID)) %>%
    inner_join(us_units, by = "GEOID")

  missing_us <- anti_join(us_units, st_drop_geometry(counties_poly), by = "GEOID")
  if (nrow(missing_us) > 0L) {
    stop(
      "US panel counties without tigris geometry:\n",
      paste(head(missing_us$GEOID, 20), collapse = ", ")
    )
  }

  counties_poly %>%
    transmute(
      unit_id,
      target_unit_id = unit_id,
      target_unit_name = NAMELSAD,
      target_area_type = "US County",
      target_boundary_id = GEOID,
      geo_country_iso3 = "USA",
      GEOID,
      geometry
    ) %>%
    st_make_valid()
}

load_conservative_venues <- function() {
  fairs <- fread(fairs_file, na.strings = c("", "NA")) %>%
    as_tibble()

  if (!"parent_fair_id" %in% names(fairs)) fairs$parent_fair_id <- fairs$fair_id
  if (!"venue_seq" %in% names(fairs)) fairs$venue_seq <- 1L

  fairs <- fairs %>%
    mutate(
      fair_id = as.integer(fair_id),
      parent_fair_id = as.integer(parent_fair_id),
      venue_seq = as.integer(venue_seq),
      year_start = as.integer(year_start),
      host_matched_country_iso3 = as.character(host_matched_country_iso3),
      venue_longitude = as.numeric(venue_longitude),
      venue_latitude = as.numeric(venue_latitude),
      venue_coordinates_note = as.character(venue_coordinates_note)
    )

  venue_audit <- fairs %>%
    filter(
      year_start >= classification_year_min,
      year_start <= classification_year_max,
      host_matched_country_iso3 == "GBR"
    ) %>%
    mutate(
      has_venue_coordinates = !is.na(venue_longitude) & !is.na(venue_latitude),
      excluded_no_venue_coordinates = !has_venue_coordinates,
      excluded_low_quality_venue_coordinates =
        has_venue_coordinates &
          str_detect(
            coalesce(venue_coordinates_note, ""),
            fixed("automated geocoding returned no reliable coordinate")
          ),
      venue_used_conservative =
        has_venue_coordinates & !excluded_low_quality_venue_coordinates
    )

  venue_audit <- venue_audit %>%
    filter(fair_id == crystal_palace_1851_fair_id) %>%
    mutate(
      venue_longitude = crystal_palace_1851_longitude,
      venue_latitude = crystal_palace_1851_latitude,
      venue_coordinates_source_title = crystal_palace_1851_coordinate_source,
      venue_coordinates_note = crystal_palace_1851_coordinate_note,
      has_venue_coordinates = TRUE,
      excluded_no_venue_coordinates = FALSE,
      excluded_low_quality_venue_coordinates = FALSE,
      venue_used_conservative = TRUE
    )

  if (nrow(venue_audit) != 1L) {
    stop(
      "Expected exactly one Crystal Palace 1851 venue row for fair_id ",
      crystal_palace_1851_fair_id,
      "; found ",
      nrow(venue_audit)
    )
  }
  if (!isTRUE(venue_audit$venue_used_conservative[[1L]])) {
    stop("Crystal Palace 1851 venue row is not usable under conservative coordinate rules.")
  }

  venues <- venue_audit %>%
    filter(venue_used_conservative) %>%
    select(
      fair_id,
      parent_fair_id,
      venue_seq,
      year_start,
      City,
      Country,
      Fair_name,
      host_matched_country_iso3,
      host_matched_name,
      host_admin1_name,
      venue,
      venue_longitude,
      venue_latitude,
      venue_coordinates_source_title,
      venue_coordinates_note
    )

  list(venues = venues, audit = venue_audit)
}

build_distance_exposure_one_country <- function(targets_sf, venues_country) {
  target_dt <- as_tibble(st_drop_geometry(targets_sf)) %>%
    select(
      unit_id,
      target_unit_id,
      target_unit_name,
      target_area_type,
      target_boundary_id,
      geo_country_iso3,
      GEOID
    )

  if (nrow(venues_country) == 0L) {
    return(list(
      distance_audit = tibble(),
      first_exposure = tibble(),
      never_units = target_dt %>% mutate(exposure_status = "never_treated"),
      always_units = tibble(),
      future_units = tibble()
    ))
  }

  venue_points <- venues_country %>%
    st_as_sf(coords = c("venue_longitude", "venue_latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(st_crs(targets_sf))

  distance_matrix <- matrix(
    as.numeric(st_distance(targets_sf, venue_points)),
    nrow = nrow(targets_sf),
    ncol = nrow(venues_country)
  )

  hit_index <- which(distance_matrix <= max_treatment_distance_m, arr.ind = TRUE)
  if (nrow(hit_index) == 0L) {
    return(list(
      distance_audit = tibble(),
      first_exposure = tibble(),
      never_units = target_dt %>% mutate(exposure_status = "never_treated"),
      always_units = tibble(),
      future_units = tibble()
    ))
  }

  distance_audit <- tibble(
    target_row = hit_index[, 1],
    venue_row = hit_index[, 2],
    distance_km = distance_matrix[hit_index] / 1000
  ) %>%
    bind_cols(target_dt[.$target_row, ]) %>%
    bind_cols(venues_country[.$venue_row, ]) %>%
    mutate(
      distance_bin_km = cut(
        distance_km,
        breaks = bin_breaks,
        labels = bin_labels,
        include.lowest = TRUE,
        right = TRUE
      )
    ) %>%
    arrange(geo_country_iso3, unit_id, year_start, distance_km, fair_id)

  first_exposure <- distance_audit %>%
    group_by(unit_id) %>%
    slice(1L) %>%
    ungroup() %>%
    transmute(
      unit_id,
      target_unit_id,
      target_unit_name,
      target_area_type,
      target_boundary_id,
      geo_country_iso3,
      GEOID,
      first_exposure_year = year_start,
      first_exposure_decade = standard_decade(first_exposure_year),
      distance_bin_km = as.character(distance_bin_km),
      first_distance_km = distance_km,
      first_fair_id = fair_id,
      first_parent_fair_id = parent_fair_id,
      first_venue_seq = venue_seq,
      first_fair_name = Fair_name,
      first_fair_city = City,
      first_fair_country = Country,
      first_fair_venue = venue,
      exposure_status = case_when(
        first_exposure_year < treated_event_year_min ~ "always_treated_pre_1840",
        first_exposure_year >= treated_event_year_min &
          first_exposure_year <= treated_event_year_max ~ "treated",
        first_exposure_year > treated_event_year_max &
          first_exposure_year <= classification_year_max ~ "future_treated_after_1910",
        TRUE ~ "outside_classification_window"
      )
    )

  exposed_units <- first_exposure %>% distinct(unit_id)
  never_units <- target_dt %>%
    anti_join(exposed_units, by = "unit_id") %>%
    mutate(exposure_status = "never_treated")

  list(
    distance_audit = distance_audit,
    first_exposure = first_exposure,
    never_units = never_units,
    always_units = first_exposure %>% filter(exposure_status == "always_treated_pre_1840"),
    future_units = first_exposure %>% filter(exposure_status == "future_treated_after_1910")
  )
}

build_distance_exposure <- function(uk_targets, venues) {
  uk_exposure <- build_distance_exposure_one_country(
    uk_targets,
    venues %>% filter(host_matched_country_iso3 == "GBR")
  )

  list(
    distance_audit = uk_exposure$distance_audit,
    first_exposure = uk_exposure$first_exposure,
    never_units = uk_exposure$never_units,
    always_units = uk_exposure$always_units,
    future_units = uk_exposure$future_units
  )
}

run_synthdid <- function(data, outcome, bin_label) {
  data_es <- data %>%
    select(
      unit_num,
      unit_id,
      iso3,
      decade,
      g,
      actual_event_decade,
      first_parent_fair_id,
      first_fair_id,
      all_of(outcome)
    ) %>%
    rename(y = all_of(outcome)) %>%
    mutate(
      unit_num = as.integer(unit_num),
      unit_id = as.character(unit_id),
      decade = as.integer(decade),
      g = as.integer(g),
      actual_event_decade = as.integer(actual_event_decade),
      y = as.numeric(y)
    )

  all_periods <- sort(unique(data_es$decade))
  complete_units <- data_es %>%
    group_by(unit_num) %>%
    summarise(
      n_periods = n_distinct(decade),
      n_valid_y = sum(!is.na(y) & is.finite(y)),
      .groups = "drop"
    ) %>%
    filter(n_periods == length(all_periods), n_valid_y == length(all_periods)) %>%
    pull(unit_num)

  data_es <- data_es %>%
    filter(unit_num %in% complete_units, !is.na(y), is.finite(y))

  effective_sample_summary <- summarise_effective_sample(data_es, outcome, bin_label)
  fail_result <- function(error) {
    list(
      ok = FALSE,
      outcome = outcome,
      distance_bin_km = bin_label,
      n_rows = nrow(data_es),
      n_units = n_distinct(data_es$unit_num),
      n_treated_units = n_distinct(data_es$unit_num[data_es$g > 0]),
      n_control_units = n_distinct(data_es$unit_num[data_es$g == 0]),
      n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
      effective_sample_summary = effective_sample_summary,
      n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - length(complete_units),
      min_decade = if (nrow(data_es) > 0L) min(data_es$decade, na.rm = TRUE) else NA_real_,
      max_decade = if (nrow(data_es) > 0L) max(data_es$decade, na.rm = TRUE) else NA_real_,
      error = error
    )
  }

  if (n_distinct(data_es$g[data_es$g > 0]) == 0L) {
    return(fail_result("No treated cohorts in estimation sample."))
  }

  if (n_distinct(data_es$g[data_es$g > 0]) != 1L) {
    return(fail_result(
      "synthdid requires simultaneous adoption; estimation sample has more than one treated cohort."
    ))
  }

  if (n_distinct(data_es$unit_num[data_es$g == 0]) == 0L) {
    return(fail_result("No never-treated control units in estimation sample."))
  }

  if (n_distinct(data_es$y, na.rm = TRUE) < 2L) {
    return(fail_result("Outcome has insufficient variation."))
  }

  tryCatch(
    {
      treatment_decade <- unique(data_es$g[data_es$g > 0])[[1L]]
      actual_event_decades <- unique(
        data_es$actual_event_decade[
          data_es$g > 0 & !is.na(data_es$actual_event_decade)
        ]
      )
      if (length(actual_event_decades) != 1L) {
        stop("Expected exactly one actual event decade in the treated sample.")
      }
      actual_event_decade <- actual_event_decades[[1L]]
      if (actual_event_decade - treatment_decade != anticipation_years) {
        stop("Treatment onset does not match the configured anticipation window.")
      }
      data_sdid <- data_es %>%
        mutate(
          treatment = as.integer(g > 0 & decade >= treatment_decade)
        ) %>%
        arrange(treatment, unit_id, decade)

      setup <- synthdid::panel.matrices(
        data_sdid %>%
          select(unit_id, decade, y, treatment) %>%
          as.data.frame(),
        unit = "unit_id",
        time = "decade",
        outcome = "y",
        treatment = "treatment",
        treated.last = TRUE
      )

      if (setup$T0 != sum(sort(unique(data_sdid$decade)) < treatment_decade)) {
        stop("Unexpected synthdid pre-period count for treatment decade ", treatment_decade, ".")
      }
      if (setup$N0 != n_distinct(data_sdid$unit_id[data_sdid$g == 0])) {
        stop("Unexpected synthdid control-unit count.")
      }

      estimate_obj <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
      se_result <- if (compute_placebo_se) {
        tryCatch(
          list(
            se = sqrt(as.numeric(vcov(
              estimate_obj,
              method = "placebo",
              replications = synthdid_se_replications
            ))),
            error = NA_character_
          ),
          error = function(e) list(
            se = NA_real_,
            error = conditionMessage(e)
          )
        )
      } else {
        list(se = NA_real_, error = NA_character_)
      }
      se <- se_result$se
      se_method <- case_when(
        !compute_placebo_se ~ "not_computed",
        is.finite(se) ~ "placebo",
        TRUE ~ "placebo_failed"
      )

      time_periods <- sort(unique(data_sdid$decade))
      post_periods <- time_periods[(setup$T0 + 1L):length(time_periods)]
      n1 <- nrow(setup$Y) - setup$N0
      placebo_effect_curve <- tibble()
      if (curve_placebo_replications > 0L && setup$N0 > n1) {
        control_index <- seq_len(setup$N0)
        y_control <- setup$Y[control_index, , drop = FALSE]
        placebo_effect_curve <- map_dfr(seq_len(curve_placebo_replications), function(rep_id) {
          set.seed(20260703L + rep_id)
          placebo_treated <- sample(control_index, size = n1, replace = FALSE)
          placebo_controls <- setdiff(control_index, placebo_treated)
          y_placebo <- rbind(
            y_control[placebo_controls, , drop = FALSE],
            y_control[placebo_treated, , drop = FALSE]
          )
          est_placebo <- tryCatch(
            synthdid::synthdid_estimate(y_placebo, length(placebo_controls), setup$T0),
            error = function(e) NULL
          )
          if (is.null(est_placebo)) return(tibble())

          placebo_effect <- as.numeric(synthdid::synthdid_effect_curve(est_placebo))
          if (length(placebo_effect) != length(post_periods)) return(tibble())

          tibble(
            outcome = outcome,
            distance_bin_km = bin_label,
            placebo_rep = rep_id,
            decade = as.integer(post_periods),
            event_time = as.integer(post_periods - actual_event_decade),
            placebo_effect = placebo_effect
          )
        })
      }

      list(
        ok = TRUE,
        outcome = outcome,
        distance_bin_km = bin_label,
        estimate_obj = estimate_obj,
        setup = setup,
        estimate = as.numeric(estimate_obj),
        se = se,
        se_method = se_method,
        se_error = se_result$error,
        se_replications = if (compute_placebo_se) synthdid_se_replications else NA_integer_,
        treatment_decade = treatment_decade,
        actual_event_decade = actual_event_decade,
        time_periods = time_periods,
        placebo_effect_curve = placebo_effect_curve,
        n0 = setup$N0,
        n1 = nrow(setup$Y) - setup$N0,
        t0 = setup$T0,
        t1 = ncol(setup$Y) - setup$T0,
        n_rows = nrow(data_es),
        n_units = n_distinct(data_es$unit_num),
        n_treated_units = n_distinct(data_es$unit_num[data_es$g > 0]),
        n_control_units = n_distinct(data_es$unit_num[data_es$g == 0]),
        n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
        effective_sample_summary = effective_sample_summary,
        n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - length(complete_units),
        min_decade = min(data_es$decade, na.rm = TRUE),
        max_decade = max(data_es$decade, na.rm = TRUE),
        error = NA_character_
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        outcome = outcome,
        distance_bin_km = bin_label,
        n_rows = nrow(data_es),
        n_units = n_distinct(data_es$unit_num),
        n_treated_units = n_distinct(data_es$unit_num[data_es$g > 0]),
        n_control_units = n_distinct(data_es$unit_num[data_es$g == 0]),
        n_treated_cohorts = n_distinct(data_es$g[data_es$g > 0]),
        effective_sample_summary = effective_sample_summary,
        n_units_dropped_for_incomplete_outcome = n_distinct(data$unit_num) - length(complete_units),
        min_decade = min(data_es$decade, na.rm = TRUE),
        max_decade = max(data_es$decade, na.rm = TRUE),
        error = conditionMessage(e)
      )
    }
  )
}

###############################################################################
# Load panel and treatment geography
###############################################################################

message("Reading UK panel...")
panel_year <- fread(panel_file, na.strings = c("", "NA")) %>%
  as_tibble() %>%
  mutate(
    unit_id = as.character(unit_id),
    GEOID = pad_geoid(GEOID),
    target_unit_id = as.character(target_unit_id),
    target_area_type = as.character(target_area_type),
    target_boundary_id = as.character(target_boundary_id),
    year = as.integer(year)
  ) %>%
  filter(
    iso3 == "GBR",
    year >= panel_year_min,
    year <= panel_year_max
  )

population_sample_eligibility_audit <- panel_year %>%
  mutate(
    population_decade = standard_decade(year),
    law_robson_observed_1801 =
      year == 1801L &
      is.finite(population_original) &
      str_detect(
        coalesce(population_source, ""),
        fixed("Law-Robson-Bennett Urban Population Database")
      )
  ) %>%
  group_by(unit_id, target_unit_id, iso3) %>%
  summarise(
    has_law_robson_population_1801 = any(law_robson_observed_1801),
    observed_population_1801 = first_nonmissing_numeric(
      population_original[law_robson_observed_1801]
    ),
    used_swing_population_1800_1939 = any(
      coalesce(population_swing_used, FALSE)
    ),
    n_valid_population_decades_1800_1930 = n_distinct(
      population_decade[
        population_decade >= panel_year_min &
          population_decade <= analysis_end_decade &
          is.finite(population) & population > 0
      ]
    ),
    min_valid_population_decade = if (
      any(is.finite(population) & population > 0)
    ) min(population_decade[is.finite(population) & population > 0]) else NA_integer_,
    max_valid_population_decade = if (
      any(is.finite(population) & population > 0)
    ) max(population_decade[is.finite(population) & population > 0]) else NA_integer_,
    .groups = "drop"
  ) %>%
  mutate(
    balanced_population_1800_1930 =
      n_valid_population_decades_1800_1930 == 14L &
      min_valid_population_decade == panel_year_min &
      max_valid_population_decade == analysis_end_decade,
    law_robson_only_eligible =
      has_law_robson_population_1801 &
      !used_swing_population_1800_1939 &
      balanced_population_1800_1930,
    selected_by_population_sample = if (
      population_sample == "law_robson_only"
    ) law_robson_only_eligible else TRUE,
    population_sample_exclusion_reason = case_when(
      selected_by_population_sample ~ "selected",
      !has_law_robson_population_1801 ~ "no_law_robson_population_1801",
      used_swing_population_1800_1939 ~ "uses_swing_population_1800_1939",
      !balanced_population_1800_1930 ~ "incomplete_population_1800_1930",
      TRUE ~ "not_selected"
    )
  ) %>%
  arrange(desc(selected_by_population_sample), unit_id)

eligible_units <- panel_year %>%
  filter(year >= treated_event_year_min, year <= panel_year_max) %>%
  group_by(unit_id, iso3) %>%
  summarise(has_any_population = any(!is.na(population)), .groups = "drop") %>%
  filter(has_any_population) %>%
  inner_join(
    population_sample_eligibility_audit %>%
      select(unit_id, iso3, selected_by_population_sample),
    by = c("unit_id", "iso3")
  ) %>%
  filter(selected_by_population_sample)

panel_year <- panel_year %>%
  semi_join(eligible_units, by = c("unit_id", "iso3"))

message("Building UK historical urban-unit geometries...")
uk_targets <- build_uk_target_geometries() %>%
  semi_join(eligible_units %>% filter(iso3 == "GBR"), by = "unit_id") %>%
  mutate(target_area_km2 = as.numeric(st_area(geometry)) / 1e6)

###############################################################################
# Build venue-distance treatment assignment
###############################################################################

message("Building conservative venue-distance treatment assignment...")
venue_data <- load_conservative_venues()
venues <- venue_data$venues
venue_quality_audit <- venue_data$audit
exposure <- build_distance_exposure(uk_targets, venues)

first_exposure <- exposure$first_exposure
never_units <- exposure$never_units
always_units <- exposure$always_units
future_units <- exposure$future_units
distance_audit <- exposure$distance_audit

message("Building the restricted donor design...")
demographic_columns <- c(
  "population_implied_1801", "population_density_1801",
  "population_density_area_coverage_1801", "agri_share_1801",
  "trade_share_1801", "other_share_1801",
  "occupation_share_coverage_1801"
)
missing_demographic_columns <- setdiff(demographic_columns, names(panel_year))
if (length(missing_demographic_columns) > 0L) {
  stop(
    "Panel is missing required 1801 demographic fields: ",
    paste(missing_demographic_columns, collapse = ", ")
  )
}

unit_demographics <- panel_year %>%
  group_by(unit_id, target_unit_id) %>%
  summarise(
    across(all_of(demographic_columns), first_nonmissing_numeric),
    .groups = "drop"
  )

venue_point <- venues %>%
  st_as_sf(
    coords = c("venue_longitude", "venue_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(st_crs(uk_targets))
if (nrow(venue_point) != 1L) {
  stop("Restricted Crystal Palace design requires exactly one venue point.")
}

distance_to_venue_km <- as.numeric(st_distance(uk_targets, venue_point)) / 1000
unit_distance_demographics <- uk_targets %>%
  mutate(distance_to_venue_km = distance_to_venue_km) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  left_join(unit_demographics, by = c("unit_id", "target_unit_id")) %>%
  left_join(
    population_sample_eligibility_audit %>%
      select(
        unit_id, target_unit_id, has_law_robson_population_1801,
        observed_population_1801,
        used_swing_population_1800_1939,
        balanced_population_1800_1930,
        law_robson_only_eligible,
        selected_by_population_sample,
        population_sample_exclusion_reason
      ),
    by = c("unit_id", "target_unit_id")
  ) %>%
  mutate(
    population_density_swing_1801 = population_density_1801,
    population_density_observed_1801 = if_else(
      is.finite(observed_population_1801) & observed_population_1801 > 0 &
        is.finite(target_area_km2) & target_area_km2 > 0,
      observed_population_1801 / target_area_km2,
      NA_real_
    ),
    population_density_1801 = if (
      density_source == "observed_population"
    ) population_density_observed_1801 else population_density_swing_1801,
    density_population_1801 = if (
      density_source == "observed_population"
    ) observed_population_1801 else population_implied_1801,
    density_source = density_source
  )

treated_initial_ids <- first_exposure %>%
  filter(
    exposure_status == "treated",
    distance_bin_km %in% bin_labels,
    first_exposure_year >= treated_event_year_min,
    first_exposure_year <= treated_event_year_max
  ) %>%
  pull(unit_id)

treated_demographic_audit <- unit_distance_demographics %>%
  filter(unit_id %in% treated_initial_ids) %>%
  mutate(
    density_data_complete = if_all(
      all_of(c("density_population_1801", "population_density_1801")),
      ~ is.finite(.x)
    ) & density_population_1801 > 0 & population_density_1801 > 0,
    occupation_data_complete = if_all(
      all_of(c("agri_share_1801", "trade_share_1801", "other_share_1801")),
      ~ is.finite(.x)
    ),
    demographics_complete = density_data_complete & occupation_data_complete,
    match_data_complete = density_data_complete &
      (donor_match_mode == "density_only" | occupation_data_complete),
    density_coverage_ok = density_data_complete & coalesce(
      population_density_area_coverage_1801 >= min_density_area_coverage,
      FALSE
    ),
    selected_treated = match_data_complete & density_coverage_ok,
    exclusion_reason = case_when(
      !density_data_complete ~ "missing_1801_density_data",
      donor_match_mode == "demographic" & !occupation_data_complete ~
        "missing_1801_occupation_data",
      !density_coverage_ok ~ "density_area_coverage_below_threshold",
      TRUE ~ "selected"
    )
  ) %>%
  arrange(distance_to_venue_km, unit_id)

selected_treated_ids <- treated_demographic_audit %>%
  filter(selected_treated) %>%
  pull(unit_id)
treated_reference <- treated_demographic_audit %>%
  filter(selected_treated) %>%
  summarise(
    donor_match_mode = donor_match_mode,
    density_source = first(density_source),
    n_treated_units = n(),
    mean_population_density_1801 = mean(population_density_1801),
    mean_agri_share_1801 = mean_or_na(agri_share_1801),
    mean_trade_share_1801 = mean_or_na(trade_share_1801),
    mean_other_share_1801 = mean_or_na(other_share_1801),
    log_density_caliper = log(donor_density_ratio),
    occupation_caliper_pp = donor_occupation_caliper_pp,
    min_density_area_coverage = min_density_area_coverage
  )
if (nrow(treated_reference) != 1L ||
    !is.finite(treated_reference$mean_population_density_1801)) {
  stop("Could not construct the treated demographic reference.")
}

occupation_caliper <- donor_occupation_caliper_pp / 100
donor_pool_eligibility_audit <- unit_distance_demographics %>%
  mutate(
    distance_ok = distance_to_venue_km >= donor_min_distance_km,
    density_data_complete = if_all(
      all_of(c("density_population_1801", "population_density_1801")),
      ~ is.finite(.x)
    ) & density_population_1801 > 0 & population_density_1801 > 0,
    occupation_data_complete = if_all(
      all_of(c("agri_share_1801", "trade_share_1801", "other_share_1801")),
      ~ is.finite(.x)
    ),
    demographics_complete = density_data_complete & occupation_data_complete,
    match_data_complete = density_data_complete &
      (donor_match_mode == "density_only" | occupation_data_complete),
    density_coverage_ok = density_data_complete & coalesce(
      population_density_area_coverage_1801 >= min_density_area_coverage,
      FALSE
    ),
    log_density_difference = abs(
      log(population_density_1801) -
        log(treated_reference$mean_population_density_1801)
    ),
    density_caliper_ok = density_coverage_ok & coalesce(
      log_density_difference <= log(donor_density_ratio), FALSE
    ),
    agri_share_difference = abs(
      agri_share_1801 - treated_reference$mean_agri_share_1801
    ),
    trade_share_difference = abs(
      trade_share_1801 - treated_reference$mean_trade_share_1801
    ),
    other_share_difference = abs(
      other_share_1801 - treated_reference$mean_other_share_1801
    ),
    occupation_calipers_ok = density_caliper_ok & occupation_data_complete & coalesce(
      agri_share_difference <= occupation_caliper &
        trade_share_difference <= occupation_caliper &
        other_share_difference <= occupation_caliper,
      FALSE
    ),
    selected_donor = distance_ok & if (donor_match_mode == "density_only") {
      density_caliper_ok
    } else {
      occupation_calipers_ok
    },
    exclusion_reason = case_when(
      !distance_ok ~ "distance_below_donor_minimum",
      !density_data_complete ~ "missing_1801_density_data",
      donor_match_mode == "demographic" & !occupation_data_complete ~
        "missing_1801_occupation_data",
      !density_coverage_ok ~ "density_area_coverage_below_threshold",
      !density_caliper_ok ~ "outside_log_density_caliper",
      donor_match_mode == "demographic" & !occupation_calipers_ok ~
        "outside_occupation_share_caliper",
      TRUE ~ "selected"
    )
  ) %>%
  arrange(desc(selected_donor), distance_to_venue_km, unit_id)

selected_donor_units <- donor_pool_eligibility_audit %>%
  filter(selected_donor)
donor_pool_filter_counts <- bind_rows(
  tibble(
    stage = c(
      "eligible_urban_units", "distance_ge_minimum",
      "complete_1801_density_data", "density_area_coverage_ge_minimum",
      "within_log_density_caliper"
    ),
    n_units = c(
      nrow(donor_pool_eligibility_audit),
      sum(donor_pool_eligibility_audit$distance_ok),
      sum(
        donor_pool_eligibility_audit$distance_ok &
          donor_pool_eligibility_audit$density_data_complete
      ),
      sum(
        donor_pool_eligibility_audit$distance_ok &
          donor_pool_eligibility_audit$density_coverage_ok
      ),
      sum(
        donor_pool_eligibility_audit$distance_ok &
          donor_pool_eligibility_audit$density_caliper_ok
      )
    )
  ),
  if (donor_match_mode == "demographic") {
    tibble(
      stage = "within_all_occupation_calipers",
      n_units = nrow(selected_donor_units)
    )
  } else {
    tibble()
  }
)

default_restricted_spec <- donor_match_mode == "demographic" &&
  population_sample == "all" &&
  isTRUE(all.equal(donor_min_distance_km, 50)) &&
  isTRUE(all.equal(donor_density_ratio, 2)) &&
  isTRUE(all.equal(donor_occupation_caliper_pp, 10)) &&
  isTRUE(all.equal(min_density_area_coverage, 0.95))
if (default_restricted_spec &&
    (length(selected_treated_ids) != expected_treated_units ||
     nrow(selected_donor_units) != expected_donor_units)) {
  stop(
    "Restricted-design count mismatch: expected ", expected_treated_units,
    " treated and ", expected_donor_units, " donors; found ",
    length(selected_treated_ids), " and ", nrow(selected_donor_units), "."
  )
}
occ6_5_restricted_spec <- donor_match_mode == "demographic" &&
  population_sample == "all" &&
  isTRUE(all.equal(donor_min_distance_km, 50)) &&
  isTRUE(all.equal(donor_density_ratio, 2)) &&
  isTRUE(all.equal(donor_occupation_caliper_pp, 6.5)) &&
  isTRUE(all.equal(min_density_area_coverage, 0.95))
if (occ6_5_restricted_spec &&
    (length(selected_treated_ids) != expected_treated_units ||
     nrow(selected_donor_units) != expected_donor_units_occ6_5)) {
  stop(
    "6.5 pp restricted-design count mismatch: expected ",
    expected_treated_units, " treated and ", expected_donor_units_occ6_5,
    " donors; found ", length(selected_treated_ids), " and ",
    nrow(selected_donor_units), "."
  )
}
density_only_ge30_spec <- donor_match_mode == "density_only" &&
  population_sample == "all" &&
  isTRUE(all.equal(donor_min_distance_km, 30)) &&
  isTRUE(all.equal(donor_density_ratio, 2)) &&
  isTRUE(all.equal(min_density_area_coverage, 0.95))
if (density_only_ge30_spec &&
    (length(selected_treated_ids) != expected_treated_units ||
     nrow(selected_donor_units) != expected_donor_units_density_only_ge30)) {
  stop(
    "Density-only >=30 km count mismatch: expected ",
    expected_treated_units, " treated and ",
    expected_donor_units_density_only_ge30, " donors; found ",
    length(selected_treated_ids), " and ", nrow(selected_donor_units), "."
  )
}

law_robson_default_restricted_spec <-
  population_sample == "law_robson_only" &&
  density_source == "swing" &&
  donor_match_mode == "demographic" &&
  isTRUE(all.equal(donor_min_distance_km, 50)) &&
  isTRUE(all.equal(donor_density_ratio, 2)) &&
  isTRUE(all.equal(donor_occupation_caliper_pp, 10)) &&
  isTRUE(all.equal(min_density_area_coverage, 0.95))
if (law_robson_default_restricted_spec &&
    (length(selected_treated_ids) != expected_law_robson_treated_units ||
     nrow(selected_donor_units) != expected_law_robson_donor_units)) {
  stop(
    "Law-Robson restricted-design count mismatch: expected ",
    expected_law_robson_treated_units, " treated and ",
    expected_law_robson_donor_units, " donors; found ",
    length(selected_treated_ids), " and ", nrow(selected_donor_units), "."
  )
}

law_robson_density_only_ge30_spec <-
  population_sample == "law_robson_only" &&
  density_source == "swing" &&
  donor_match_mode == "density_only" &&
  isTRUE(all.equal(donor_min_distance_km, 30)) &&
  isTRUE(all.equal(donor_density_ratio, 2)) &&
  isTRUE(all.equal(min_density_area_coverage, 0.95))
if (law_robson_density_only_ge30_spec &&
    (length(selected_treated_ids) != expected_law_robson_treated_units ||
     nrow(selected_donor_units) !=
       expected_law_robson_donor_units_density_only_ge30)) {
  stop(
    "Law-Robson density-only >=30 km count mismatch: expected ",
    expected_law_robson_treated_units, " treated and ",
    expected_law_robson_donor_units_density_only_ge30,
    " donors; found ", length(selected_treated_ids), " and ",
    nrow(selected_donor_units), "."
  )
}

law_robson_observed_density_default_spec <-
  population_sample == "law_robson_only" &&
  density_source == "observed_population" &&
  donor_match_mode == "demographic" &&
  isTRUE(all.equal(donor_min_distance_km, 50)) &&
  isTRUE(all.equal(donor_density_ratio, 2)) &&
  isTRUE(all.equal(donor_occupation_caliper_pp, 10)) &&
  isTRUE(all.equal(min_density_area_coverage, 0.95))
if (law_robson_observed_density_default_spec &&
    (length(selected_treated_ids) != expected_law_robson_treated_units ||
     nrow(selected_donor_units) !=
       expected_law_robson_observed_density_donor_units)) {
  stop(
    "Observed-density Law-Robson design count mismatch: expected ",
    expected_law_robson_treated_units, " treated and ",
    expected_law_robson_observed_density_donor_units,
    " donors; found ", length(selected_treated_ids), " and ",
    nrow(selected_donor_units), "."
  )
}

law_robson_observed_density_ge30_spec <-
  population_sample == "law_robson_only" &&
  density_source == "observed_population" &&
  donor_match_mode == "density_only" &&
  isTRUE(all.equal(donor_min_distance_km, 30)) &&
  isTRUE(all.equal(donor_density_ratio, 2)) &&
  isTRUE(all.equal(min_density_area_coverage, 0.95))
if (law_robson_observed_density_ge30_spec &&
    (length(selected_treated_ids) != expected_law_robson_treated_units ||
     nrow(selected_donor_units) !=
       expected_law_robson_observed_density_donor_units_density_only_ge30)) {
  stop(
    "Observed-density Law-Robson density-only >=30 km count mismatch: expected ",
    expected_law_robson_treated_units, " treated and ",
    expected_law_robson_observed_density_donor_units_density_only_ge30,
    " donors; found ", length(selected_treated_ids), " and ",
    nrow(selected_donor_units), "."
  )
}

treated_units_selected <- first_exposure %>%
  filter(unit_id %in% selected_treated_ids) %>%
  transmute(
    unit_id,
    target_unit_id,
    geo_country_iso3,
    event_year = first_exposure_year,
    actual_event_decade = first_exposure_decade,
    g = first_exposure_decade - anticipation_years,
    distance_bin_km = aggregate_bin_label,
    source_distance_bin_km = distance_bin_km,
    first_distance_km,
    first_fair_id = as.character(first_fair_id),
    first_parent_fair_id = as.character(first_parent_fair_id),
    first_venue_seq,
    first_fair_name,
    first_fair_city,
    first_fair_country,
    first_fair_venue
  )
controls_selected <- selected_donor_units %>%
  transmute(
    unit_id,
    target_unit_id,
    geo_country_iso3,
    g = 0L,
    actual_event_decade = NA_integer_,
    first_parent_fair_id = NA_character_,
    first_fair_id = NA_character_
  )
analysis_units_selected <- bind_rows(
  treated_units_selected %>%
    select(
      unit_id, target_unit_id, geo_country_iso3, g, actual_event_decade,
      first_parent_fair_id, first_fair_id
    ),
  controls_selected
)

message(
  if (population_sample == "law_robson_only") {
    "Using the Law-Robson-only canonical population sample..."
  } else {
    "Using canonical population with observed and Swing knots from prep panel..."
  }
)
canonical_population_columns <- c(
  "population", "population_original", "population_knot",
  "population_swing_implied", "population_swing_used",
  "population_swing_geometry_coverage", "population_swing_density_coverage",
  "population_swing_growth_outlier", "population_swing_exclusion_reason",
  "population_source", "population_interp_status"
)
missing_population_columns <- setdiff(
  canonical_population_columns, names(panel_year)
)
if (length(missing_population_columns) > 0L) {
  stop(
    "The final panel predates the Swing population integration. Missing: ",
    paste(missing_population_columns, collapse = ", ")
  )
}
invalid_swing_knots <- panel_year %>%
  filter(
    coalesce(population_swing_used, FALSE) &
      (
        !is.na(population_original) |
          !is.finite(population_swing_implied) |
          coalesce(population_swing_growth_outlier, FALSE) |
          coalesce(population_swing_geometry_coverage, 0) < 0.95 |
          coalesce(population_swing_density_coverage, 0) < 0.95
      )
  )
if (nrow(invalid_swing_knots) > 0L) {
  stop("The canonical panel contains invalid or overriding Swing knots.")
}

write_csv(
  treated_reference,
  file.path(results_dir, "treated_demographic_reference.csv")
)
write_csv(
  population_sample_eligibility_audit,
  file.path(results_dir, "population_sample_eligibility_audit.csv")
)
write_csv(
  treated_demographic_audit,
  file.path(results_dir, "treated_demographic_eligibility_audit.csv")
)
write_csv(
  donor_pool_filter_counts,
  file.path(results_dir, "donor_pool_filter_counts.csv")
)
write_csv(
  donor_pool_eligibility_audit,
  file.path(results_dir, "donor_pool_eligibility_audit.csv")
)
write_csv(
  selected_donor_units,
  file.path(results_dir, "selected_donor_units.csv")
)
write_csv(
  panel_year %>%
    semi_join(analysis_units_selected, by = c("unit_id", "target_unit_id")) %>%
    select(
      unit_id, target_unit_id, place_name, year,
      population_original, population_knot, population,
      population_implied_1801, population_swing_implied,
      population_swing_used, population_swing_geometry_coverage,
      population_swing_density_coverage, population_swing_growth_outlier,
      population_swing_exclusion_reason, population_source,
      population_interp_status
    ),
  file.path(results_dir, "population_swing_knot_audit.csv")
)

write_csv(venue_quality_audit, file.path(results_dir, "venue_quality_audit.csv"))
write_csv(distance_audit, file.path(results_dir, "venue_distance_match_audit_all_bins.csv"))
write_csv(first_exposure, file.path(results_dir, "first_exposure_all_bins.csv"))
write_csv(never_units, file.path(results_dir, "never_treated_units.csv"))
write_csv(always_units, file.path(results_dir, "always_treated_pre_1840_units.csv"))
write_csv(future_units, file.path(results_dir, "future_treated_after_1910_units.csv"))

###############################################################################
# Aggregate annual outcome panel to decades
###############################################################################

message("Aggregating annual panel to decades...")
panel_decade_base <- panel_year %>%
  mutate(decade = standard_decade(year)) %>%
  group_by(
    unit_type,
    unit_id,
    GEOID,
    lau_id,
    city_geonameid,
    target_unit_id,
    target_area_type,
    target_boundary_id,
    place_name,
    place_name_ascii,
    country,
    iso3,
    lat,
    lon,
    decade
  ) %>%
  summarise(
    n_inventors = sum(n_inventors, na.rm = TRUE),
    n_stem = sum(n_stem, na.rm = TRUE),
    n_nonstem = sum(n_nonstem, na.rm = TRUE),
    population = mean_or_na(population),
    population_original = mean_or_na(population_original),
    population_knot = mean_or_na(population_knot),
    population_implied_1801 = first_nonmissing_numeric(population_implied_1801),
    population_swing_used = any(population_swing_used),
    population_swing_growth_outlier = any(
      coalesce(population_swing_growth_outlier, FALSE)
    ),
    population_density_1801 = first_nonmissing_numeric(population_density_1801),
    population_density_area_coverage_1801 = first_nonmissing_numeric(
      population_density_area_coverage_1801
    ),
    agri_share_1801 = first_nonmissing_numeric(agri_share_1801),
    trade_share_1801 = first_nonmissing_numeric(trade_share_1801),
    other_share_1801 = first_nonmissing_numeric(other_share_1801),
    source_panel = first_nonmissing(source_panel),
    .groups = "drop"
  ) %>%
  mutate(
    any_inventor = as.integer(n_inventors > 0),
    any_stem = as.integer(n_stem > 0),
    log1p_n_inventors = log1p(n_inventors),
    log1p_n_stem = log1p(n_stem),
    log_population = if_else(population > 0, log(population), NA_real_),
    inventors_per_100k_pop = if_else(
      population > 0,
      100000 * n_inventors / population,
      NA_real_
    ),
    stem_per_100k_pop = if_else(
      population > 0,
      100000 * n_stem / population,
      NA_real_
    )
  )

population_balance_audit <- panel_decade_base %>%
  semi_join(analysis_units_selected, by = c("unit_id", "target_unit_id")) %>%
  left_join(
    analysis_units_selected %>% select(unit_id, target_unit_id, g),
    by = c("unit_id", "target_unit_id")
  ) %>%
  group_by(unit_id, target_unit_id, g) %>%
  summarise(
    n_decades = n_distinct(decade),
    n_valid_population_decades = sum(is.finite(population) & population > 0),
    min_decade = min(decade),
    max_decade = max(decade),
    used_swing_population = any(population_swing_used),
    .groups = "drop"
  ) %>%
  mutate(
    treatment_status = if_else(g > 0, "treated", "donor"),
    balanced_population =
      n_decades == 14L & n_valid_population_decades == 14L &
      min_decade == panel_year_min & max_decade == analysis_end_decade
  ) %>%
  arrange(desc(treatment_status), unit_id)
write_csv(
  population_balance_audit,
  file.path(results_dir, "population_balance_audit_1800_1930.csv")
)
if (nrow(population_balance_audit) !=
      length(selected_treated_ids) + nrow(selected_donor_units) ||
    any(!population_balance_audit$balanced_population)) {
  stop("The restricted 1800-1930 population panel is not balanced as expected.")
}

if (donor_audit_only) {
  audit_notes <- c(
    "Crystal Palace 1851 restricted-donor audit completed; models not run.",
    paste0("Donor matching mode: ", donor_match_mode),
    paste0("Population sample: ", population_sample),
    paste0("Density source: ", density_source),
    paste0("Treated units: ", length(selected_treated_ids)),
    paste0("Selected donor units: ", nrow(selected_donor_units)),
    paste0("Panel decades: ", panel_year_min, "-", analysis_end_decade),
    paste0("Anticipation decades: ", anticipation_decades),
    paste0("Treatment onset decade: ", 1850L - anticipation_years),
    paste0("Results directory: ", results_dir)
  )
  writeLines(audit_notes, file.path(results_dir, "audit_only_notes.txt"))
  message(paste(audit_notes, collapse = "\n"))
  quit(save = "no", status = 0L)
}

###############################################################################
# Run synthetic DiD estimates by distance bin
###############################################################################

root_sample_summary <- list()
root_country_summary <- list()
root_synthdid_att <- list()
root_synthdid_effect_curve <- list()
root_synthdid_control_weights <- list()
root_synthdid_time_weights <- list()

for (bin_label in selected_analysis_bin_labels) {
  message("Running bin ", bin_label, " km...")
  bin_dir <- file.path(results_dir, bin_dirs[[bin_label]])
  dir.create(bin_dir, recursive = TRUE, showWarnings = FALSE)

  treated_units <- treated_units_selected
  controls <- controls_selected
  analysis_units <- analysis_units_selected

  panel_decade <- panel_decade_base %>%
    semi_join(analysis_units, by = c("unit_id", "target_unit_id")) %>%
    left_join(analysis_units, by = c("unit_id", "target_unit_id")) %>%
    mutate(unit_num = as.integer(factor(unit_id)))

  balance <- check_balanced_panel(panel_decade, unit_id, decade)

  event_distribution <- bind_rows(
    panel_decade %>%
      distinct(unit_id, iso3, g) %>%
      count(iso3, g, name = "n_units") %>%
      mutate(distance_bin_km = bin_label, cohort = as.character(g)) %>%
      select(distance_bin_km, iso3, cohort, n_units),
    always_units %>%
      count(geo_country_iso3, name = "n_units") %>%
      transmute(
        distance_bin_km = bin_label,
        iso3 = geo_country_iso3,
        cohort = "always_treated_pre_1840_excluded",
        n_units
      ),
    future_units %>%
      count(geo_country_iso3, name = "n_units") %>%
      transmute(
        distance_bin_km = bin_label,
        iso3 = geo_country_iso3,
        cohort = "future_treated_after_1910_excluded",
        n_units
      )
  )

  sample_summary <- panel_decade %>%
    summarise(
      distance_bin_km = bin_label,
      n_rows = n(),
      n_units = n_distinct(unit_id),
      n_periods = n_distinct(decade),
      min_decade = min(decade, na.rm = TRUE),
      max_decade = max(decade, na.rm = TRUE),
      is_balanced = balance$is_balanced,
      min_periods_per_unit = balance$min_periods_per_unit,
      max_periods_per_unit = balance$max_periods_per_unit,
      n_treated_units = n_distinct(unit_id[g > 0]),
      n_control_units = n_distinct(unit_id[g == 0]),
      n_always_treated_excluded = nrow(always_units),
      n_future_treated_after_1910_excluded = nrow(future_units),
      missing_inventors_per_100k_pop = sum(is.na(inventors_per_100k_pop)),
      missing_stem_per_100k_pop = sum(is.na(stem_per_100k_pop)),
      total_inventors = sum(n_inventors, na.rm = TRUE),
      total_stem = sum(n_stem, na.rm = TRUE),
      .groups = "drop"
    )

  country_summary <- panel_decade %>%
    group_by(iso3) %>%
    summarise(
      distance_bin_km = bin_label,
      n_rows = n(),
      n_units = n_distinct(unit_id),
      n_treated_units = n_distinct(unit_id[g > 0]),
      n_control_units = n_distinct(unit_id[g == 0]),
      total_inventors = sum(n_inventors, na.rm = TRUE),
      total_stem = sum(n_stem, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    relocate(distance_bin_km)

  model_results <- list()
  for (outcome in outcomes) {
    message("  outcome: ", outcome)
    model_results[[outcome]] <- run_synthdid(
      data = panel_decade,
      outcome = outcome,
      bin_label = bin_label
    )
  }

  successful_models <- keep(model_results, "ok")

  model_status <- imap_dfr(
    model_results,
    ~ tibble(
      outcome = .x$outcome,
      distance_bin_km = .x$distance_bin_km,
      control_group = control_group_name,
      ok = .x$ok,
      n_rows = value_or(.x$n_rows, NA_integer_),
      n_units = value_or(.x$n_units, NA_integer_),
      n_treated_units = value_or(.x$n_treated_units, NA_integer_),
      n_control_units = value_or(.x$n_control_units, NA_integer_),
      n_treated_cohorts = value_or(.x$n_treated_cohorts, NA_integer_),
      n0 = value_or(.x$n0, NA_integer_),
      n1 = value_or(.x$n1, NA_integer_),
      t0 = value_or(.x$t0, NA_integer_),
      t1 = value_or(.x$t1, NA_integer_),
      treatment_decade = value_or(.x$treatment_decade, NA_integer_),
      actual_event_decade = value_or(.x$actual_event_decade, NA_integer_),
      anticipation_decades = anticipation_decades,
      se_method = value_or(.x$se_method, NA_character_),
      se_replications = value_or(.x$se_replications, NA_integer_),
      se_error = value_or(.x$se_error, NA_character_),
      n_events = value_or(.x$effective_sample_summary$n_events, NA_integer_),
      n_treated_gbr = value_or(.x$effective_sample_summary$n_treated_gbr, NA_integer_),
      n_treated_usa = value_or(.x$effective_sample_summary$n_treated_usa, NA_integer_),
      n_control_gbr = value_or(.x$effective_sample_summary$n_control_gbr, NA_integer_),
      n_control_usa = value_or(.x$effective_sample_summary$n_control_usa, NA_integer_),
      n_units_dropped_for_incomplete_outcome = value_or(
        .x$n_units_dropped_for_incomplete_outcome,
        NA_integer_
      ),
      min_decade = value_or(.x$min_decade, NA_real_),
      max_decade = value_or(.x$max_decade, NA_real_),
      error = value_or(.x$error, NA_character_)
    )
  )

  if (length(successful_models) > 0L) {
    synthdid_att <- imap_dfr(
      successful_models,
      ~ extract_synthdid_att(.x)
    )
    synthdid_effect_curve <- imap_dfr(
      successful_models,
      ~ extract_effect_curve(.x)
    )
    synthdid_control_weights <- imap_dfr(
      successful_models,
      ~ extract_synthdid_controls(.x, weight_type = "omega")
    )
    synthdid_time_weights <- imap_dfr(
      successful_models,
      ~ extract_synthdid_controls(.x, weight_type = "lambda")
    )
  } else {
    synthdid_att <- tibble()
    synthdid_effect_curve <- tibble()
    synthdid_control_weights <- tibble()
    synthdid_time_weights <- tibble()
  }

  effective_sample_summary <- imap_dfr(
    model_results,
    ~ .x$effective_sample_summary
  )
  distance_audit_bin <- if (bin_label == aggregate_bin_label) {
    distance_audit %>%
      filter(distance_bin_km %in% bin_labels) %>%
      mutate(analysis_distance_bin_km = bin_label)
  } else {
    distance_audit %>%
      filter(distance_bin_km == bin_label) %>%
      mutate(analysis_distance_bin_km = bin_label)
  }

  write_csv(treated_units, file.path(bin_dir, "treatment_assignment.csv"))
  write_csv(
    distance_audit_bin,
    file.path(bin_dir, "venue_distance_match_audit.csv")
  )
  write_csv(event_distribution, file.path(bin_dir, "event_distribution.csv"))
  write_csv(sample_summary, file.path(bin_dir, "sample_summary.csv"))
  write_csv(country_summary, file.path(bin_dir, "sample_summary_by_country.csv"))
  write_csv(effective_sample_summary, file.path(bin_dir, "effective_sample_summary_by_outcome.csv"))
  write_csv(model_status, file.path(bin_dir, "model_status.csv"))
  write_csv(synthdid_att, file.path(bin_dir, "synthdid_att.csv"))
  write_csv(synthdid_effect_curve, file.path(bin_dir, "synthdid_effect_curve.csv"))
  write_csv(synthdid_control_weights, file.path(bin_dir, "synthdid_control_weights.csv"))
  write_csv(synthdid_time_weights, file.path(bin_dir, "synthdid_time_weights.csv"))

  if (nrow(synthdid_effect_curve) > 0L) {
    y_limits_by_outcome <- synthdid_effect_curve %>%
      group_by(outcome) %>%
      summarise(y_limits = list(dynamic_y_limits(pick(everything()))), .groups = "drop")

    for (model in successful_models) {
      y_limits <- y_limits_by_outcome %>%
        filter(outcome == model$outcome) %>%
        pull(y_limits) %>%
        pluck(1)

      model_effect_curve <- synthdid_effect_curve %>%
        filter(outcome == model$outcome, distance_bin_km == model$distance_bin_km)

      plot_curve <- plot_effect_curve(
        model_effect_curve,
        model$outcome,
        model$distance_bin_km,
        y_limits,
        format_sample_annotation(model$effective_sample_summary),
        format_att_annotation(model)
      )

      ggsave(
        file.path(
          bin_dir,
          paste0("SDID_effect_curve_", sanitize_filename(model$outcome), "_bin_", sanitize_filename(bin_label), "km.png")
        ),
        plot_curve,
        width = 8,
        height = 6,
        dpi = 300
      )
      ggsave(
        file.path(
          bin_dir,
          paste0("SDID_event_study_", sanitize_filename(model$outcome), "_", sanitize_filename(bin_label), ".png")
        ),
        plot_curve,
        width = 8,
        height = 6,
        dpi = 300
      )

      plot_fit <- plot_synthdid_fit(model)
      ggsave(
        file.path(
          bin_dir,
          paste0("SDID_fit_", sanitize_filename(model$outcome), "_bin_", sanitize_filename(bin_label), "km.png")
        ),
        plot_fit,
        width = 8,
        height = 6,
        dpi = 300
      )

      if (bin_label == aggregate_bin_label && model$outcome == "stem_per_100k_pop") {
        plot_curve_zoomed <- plot_effect_curve(
          model_effect_curve,
          model$outcome,
          model$distance_bin_km,
          zoomed_display_y_limits,
          format_sample_annotation(model$effective_sample_summary),
          format_att_annotation(model)
        ) +
          labs(caption = "Zoomed y-axis for display only; confidence intervals outside the range are clipped.")

        ggsave(
          file.path(
            bin_dir,
            paste0("SDID_effect_curve_", sanitize_filename(model$outcome), "_bin_", sanitize_filename(bin_label), "km_zoomed.png")
          ),
          plot_curve_zoomed,
          width = 8,
          height = 6,
          dpi = 300
        )
      }
    }
  }

  notes <- c(
    "World's fairs UK venue-distance synthetic DiD: Crystal Palace 1851 only",
    "",
    paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Distance bin: ", bin_label, " km"),
    paste0("Panel: ", panel_file),
    paste0("Fairs: ", fairs_file),
    paste0("Selected fair_id: ", crystal_palace_1851_fair_id),
    paste0(
      "Selected venue coordinates: ",
      crystal_palace_1851_latitude,
      ", ",
      crystal_palace_1851_longitude
    ),
    paste0("Selected venue coordinate source: ", crystal_palace_1851_coordinate_source),
    paste0("Control group: ", control_group_name),
    paste0("Donor matching mode: ", donor_match_mode, "."),
    paste0("Population sample: ", population_sample, "."),
    paste0("Density source: ", density_source, "."),
    paste0("Analysis decades: ", panel_year_min, "-", analysis_end_decade, "."),
    paste0("Anticipation: ", anticipation_decades, " decades."),
    paste0("Minimum donor distance: ", donor_min_distance_km, " km."),
    paste0("Density ratio caliper: ", donor_density_ratio, "."),
    if (donor_match_mode == "demographic") {
      paste0("Occupation-share caliper: ", donor_occupation_caliper_pp, " pp.")
    } else {
      "Occupation-share calipers: not applied."
    },
    paste0("Minimum 1801 density-area coverage: ", min_density_area_coverage, "."),
    if (population_sample == "law_robson_only") {
      paste(
        "Population sample requires observed Law-Robson population in 1801,",
        "a balanced 1800-1930 population panel, and no Swing population knot",
        "during 1800-1939. Interpolation between non-Swing knots remains allowed."
      )
    } else {
      paste(
        "Missing early population uses accepted 1801-1831 Swing knots from",
        "the prep panel and interpolation between canonical knots; observed",
        "population is never overwritten."
      )
    },
    if (density_source == "observed_population") {
      paste(
        "The 1801 matching density is observed Law-Robson population divided",
        "by the full 1921 target-unit area used for treatment assignment."
      )
    } else {
      "The 1801 matching density is derived from the Swing cross-section."
    },
    "The 1801 occupation-share matching covariates remain derived from Swing.",
    "Greater London included using Nomis/ONS 1921 districts selected by >=50% overlap with 1911 Greater London.",
    "The Crystal Palace 1851 venue is used to classify exposure for Greater London and nearby units.",
    "Distance is polygon-to-venue; venues inside polygons have distance 0.",
    "Venue coordinates use conservative quality filter.",
    paste0("Maximum exposure distance: ", max_treatment_distance_km, " km."),
    if (bin_label == aggregate_bin_label) {
      paste0(
        "A zoomed display-only figure is saved for stem_per_100k_pop with y-axis limits ",
        paste(zoomed_display_y_limits, collapse = " to "),
        "; CSV estimates and confidence intervals are unchanged."
      )
    } else {
      NULL
    },
    "Always-treated units are units first exposed before 1840.",
    "Future-treated units are units first exposed after 1910 and before or during 1961.",
    "Estimator: synthdid::synthdid_estimate.",
    paste0(
      "Standard errors: ",
      if (compute_placebo_se) {
        paste0("vcov(method = 'placebo', replications = ", synthdid_se_replications, ")")
      } else {
        "not computed; set SYNTHDID_PLACEBO_SE=TRUE to enable placebo SEs"
      },
      "."
    ),
    paste0(
      "Package plot CI arrows: ",
      if (plot_package_placebo_ci) {
        "enabled via SYNTHDID_PLOT_PLACEBO_CI=TRUE"
      } else {
        "disabled by default to avoid recomputing placebo vcov inside plot()"
      },
      "."
    ),
    "Dynamic CSV/figures use synthdid_effect_curve for post-treatment decades.",
    paste0("Treated units: ", sample_summary$n_treated_units),
    paste0("Restricted donor units: ", sample_summary$n_control_units),
    paste0("Always-treated units excluded: ", nrow(always_units)),
    paste0("Future-treated units after 1910 excluded: ", nrow(future_units)),
    paste0("Successful models: ", sum(model_status$ok), " / ", nrow(model_status))
  )
  writeLines(notes, file.path(bin_dir, "notes.txt"))

  root_sample_summary[[bin_label]] <- sample_summary
  root_country_summary[[bin_label]] <- country_summary
  root_synthdid_att[[bin_label]] <- synthdid_att
  root_synthdid_effect_curve[[bin_label]] <- synthdid_effect_curve
  root_synthdid_control_weights[[bin_label]] <- synthdid_control_weights
  root_synthdid_time_weights[[bin_label]] <- synthdid_time_weights
}

all_sample_summary <- bind_rows(root_sample_summary)
write_csv(all_sample_summary, file.path(results_dir, paste0("sample_summary", aggregate_suffix, ".csv")))

all_country_summary <- bind_rows(root_country_summary)
write_csv(all_country_summary, file.path(results_dir, paste0("sample_summary_by_country", aggregate_suffix, ".csv")))

all_event_distribution <- map_dfr(selected_analysis_bin_labels, function(bin_label) {
  bin_file <- file.path(results_dir, bin_dirs[[bin_label]], "event_distribution.csv")
  read_csv(bin_file, show_col_types = FALSE)
})
write_csv(all_event_distribution, file.path(results_dir, paste0("event_distribution", aggregate_suffix, ".csv")))

all_synthdid_att <- bind_rows(root_synthdid_att)
write_csv(all_synthdid_att, file.path(results_dir, paste0("synthdid_att", aggregate_suffix, ".csv")))

if (nrow(all_synthdid_att) > 0L) {
  for (outcome in unique(all_synthdid_att$outcome)) {
    plot_att <- plot_att_by_bin(all_synthdid_att, outcome)
    ggsave(
      file.path(
        results_dir,
        paste0("SDID_att_by_bin_", sanitize_filename(outcome), aggregate_suffix, ".png")
      ),
      plot_att,
      width = 9,
      height = 5.5,
      dpi = 300
    )
  }
}

all_synthdid_effect_curve <- bind_rows(root_synthdid_effect_curve)
write_csv(all_synthdid_effect_curve, file.path(results_dir, paste0("synthdid_effect_curve", aggregate_suffix, ".csv")))

all_synthdid_control_weights <- bind_rows(root_synthdid_control_weights)
write_csv(all_synthdid_control_weights, file.path(results_dir, paste0("synthdid_control_weights", aggregate_suffix, ".csv")))

all_synthdid_time_weights <- bind_rows(root_synthdid_time_weights)
write_csv(all_synthdid_time_weights, file.path(results_dir, paste0("synthdid_time_weights", aggregate_suffix, ".csv")))

root_notes <- c(
  "World's fairs UK historical urban-unit venue-distance synthetic DiD: Crystal Palace 1851 only",
  "",
  paste0("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("TALENT_DETS_DATA_DIR: ", TALENT_DETS_DATA_DIR),
  paste0("Panel: ", panel_file),
  paste0("Fairs: ", fairs_file),
  paste0("Selected fair_id: ", crystal_palace_1851_fair_id),
  paste0(
    "Selected venue coordinates: ",
    crystal_palace_1851_latitude,
    ", ",
    crystal_palace_1851_longitude
  ),
  paste0("Selected venue coordinate source: ", crystal_palace_1851_coordinate_source),
  paste0("Results directory: ", results_dir),
  paste0("Donor matching mode: ", donor_match_mode, "."),
  paste0("Population sample: ", population_sample, "."),
  paste0("Density source: ", density_source, "."),
  paste0("Distance specification: cumulative ", aggregate_bin_label, " km."),
  paste0("Analysis decades: ", panel_year_min, "-", analysis_end_decade, "."),
  paste0("Anticipation: ", anticipation_decades, " decades."),
  paste0("Anticipated treatment onset decade: ", 1850L - anticipation_years, "."),
  "Actual fair event-time zero remains 1850.",
  paste0("Exposure classification window: ", classification_year_min, "-", classification_year_max, "."),
  paste0("Included treated event window: ", treated_event_year_min, "-", treated_event_year_max, "."),
  paste0("Donors are at least ", donor_min_distance_km, " km from the venue."),
  paste0(
    "Donor density must be within a factor of ", donor_density_ratio,
    " of the treated mean."
  ),
  if (donor_match_mode == "demographic") {
    paste0(
      "Each donor occupation share must be within ",
      donor_occupation_caliper_pp, " pp of the treated mean."
    )
  } else {
    "Occupation-share calipers are not applied."
  },
  paste0(
    "Treated and donor density-area coverage must be at least ",
    min_density_area_coverage, "."
  ),
  if (population_sample == "law_robson_only") {
    paste(
      "Population sample requires observed Law-Robson population in 1801,",
      "a balanced 1800-1930 population panel, and no Swing population knot",
      "during 1800-1939. Interpolation between non-Swing knots remains allowed."
    )
  } else {
    paste(
      "Missing early population uses accepted 1801-1831 Swing knots from",
      "the prep panel and interpolation between canonical knots; observed",
      "population is never overwritten."
    )
  },
  if (density_source == "observed_population") {
    paste(
      "The 1801 matching density is observed Law-Robson population divided",
      "by the full 1921 target-unit area used for treatment assignment."
    )
  } else {
    "The 1801 matching density is derived from the Swing cross-section."
  },
  "The 1801 occupation-share matching covariates remain derived from Swing.",
  paste0("Selected treated units: ", length(selected_treated_ids), "."),
  paste0("Selected donor units: ", nrow(selected_donor_units), "."),
  "Greater London is included using Nomis/ONS 1921 districts selected by >=50% overlap with 1911 Greater London.",
  "US counties are excluded from the panel and control group.",
  "Venue coordinates with low-quality automated geocoding notes are excluded.",
  "Estimator: synthdid::synthdid_estimate.",
  paste0(
    "Standard errors: ",
    if (compute_placebo_se) {
      paste0("vcov(method = 'placebo', replications = ", synthdid_se_replications, ")")
    } else {
      "not computed; set SYNTHDID_PLACEBO_SE=TRUE to enable placebo SEs"
    },
    "."
  ),
  paste0(
    "Package plot CI arrows: ",
    if (plot_package_placebo_ci) {
      "enabled via SYNTHDID_PLOT_PLACEBO_CI=TRUE"
    } else {
      "disabled by default to avoid recomputing placebo vcov inside plot()"
    },
    "."
  ),
  "Dynamic outputs: synthdid_effect_curve for post-treatment decades.",
  paste0("Elapsed minutes: ", round(difftime(Sys.time(), initial_time, units = "mins"), 1))
)
writeLines(root_notes, file.path(results_dir, "notes.txt"))

message("Saved results in: ", results_dir)
message(
  "Done. Elapsed: ",
  round(difftime(Sys.time(), initial_time, units = "mins"), 1),
  " min"
)
