###############################################################################
# USA-only AMWS world's-fairs event studies estimated separately by cohort.
#
# Reuses the validated treated and balanced never-treated samples from:
#   WORLD_FAIRS_PROFILE=robust_m30_pop_m10_balanced_oldest
#
# Run from the repository root or analysis/:
#   Rscript analysis/world_fairs/worlds_fairs_us_amws_event_studies_per_cohort.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(did)
})

initial_time <- Sys.time()

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
  if (basename(repo_root) == "world_fairs" && basename(dirname(repo_root)) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, "..", ".."), winslash = "/")
  }
  if (basename(repo_root) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, ".."), winslash = "/")
  }
}

source(file.path(repo_root, "paths.R"))
source(file.path(
  repo_root,
  "analysis",
  "world_fairs",
  "worlds_fairs_us_amws_helpers.R"
))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(user_data_dir, winslash = "/", mustWork = TRUE)
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

profile_name <- Sys.getenv(
  "WORLD_FAIRS_PROFILE",
  unset = "robust_m30_pop_m10_balanced_oldest"
)
profile <- world_fairs_us_amws_profile(profile_name)
balance_controls_calendar <- isTRUE(value_or(
  profile$balance_controls_calendar,
  value_or(profile$balance_controls_oldest, FALSE)
))
if (!isTRUE(profile$population_control) || !balance_controls_calendar) {
  stop(
    paste0(
      "Per-cohort analysis requires calendar-balanced controls and the ",
      "log-population profile; received: "
    ),
    profile$name
  )
}

cores <- suppressWarnings(as.integer(Sys.getenv("WORLD_FAIRS_CORES", unset = "4")))
if (is.na(cores) || cores < 1L) cores <- 1L
treatment_cohort_shift <- as.integer(value_or(profile$treatment_cohort_shift, 0L))
treatment_timing <- as.character(value_or(
  profile$treatment_timing,
  "standard_decade"
))
if (!treatment_timing %in% c("standard_decade", "alternative_decade")) {
  stop("Unsupported treatment timing: ", treatment_timing)
}
single_fair_event_window <- isTRUE(value_or(profile$single_fair_event_window, FALSE))
plot_estimator_label <- value_or(
  profile$plot_estimator_label,
  "CSDID + log population, ref e=-10"
)

panel_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results",
  "worlds_fair",
  profile$results_subdir
)
per_cohort_dir <- file.path(results_dir, "per_cohort")
dir.create(per_cohort_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(
  panel_file,
  file.path(results_dir, "all_sample_summary.csv"),
  file.path(results_dir, "all_treated_counts_by_cohort.csv"),
  file.path(results_dir, "run_validation.csv")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required inputs:\n", paste(missing_files, collapse = "\n"))
}

source_validation <- read_csv(
  file.path(results_dir, "run_validation.csv"),
  show_col_types = FALSE
)
if (!all(source_validation$passed)) {
  stop("The aggregate balanced specification has failed validation checks.")
}

event_time_min <- as.integer(profile$event_time_min)
reference_event_time <- -10L
outcomes <- c(
  "n_amws",
  "log1p_n_amws",
  "amws_per_100k",
  "amws_per_1000_births",
  "population",
  "county_births_estimate"
)
outcome_labels <- c(
  n_amws = "AMWS scientists born",
  log1p_n_amws = "log(1 + AMWS scientists born)",
  amws_per_100k = "AMWS scientists born per 100k population",
  amws_per_1000_births = "AMWS scientists born per 1,000 births",
  population = "County population",
  county_births_estimate = "County births estimate"
)
outcome_file_labels <- c(
  n_amws = "n_amws",
  log1p_n_amws = "log1p_n_amws",
  amws_per_100k = "amws_per_100k",
  population = "population",
  # Short stems avoid the Windows MAX_PATH limit in nested distance-bin folders.
  county_births_estimate = "county_births",
  amws_per_1000_births = if (
    treatment_cohort_shift == 0L
  ) "amws_per_1000_births" else "amws_1k_births"
)
support_vars <- unique(c(outcomes, "population"))

message("Reading and aggregating the consolidated AMWS panel...")
panel_year <- fread(panel_file, na.strings = c("", "NA")) %>% as_tibble()
required_panel_cols <- c(
  "GEOID", "year", "population", "county_births_estimate_year",
  "n_amws_1906_1955_dedup", "n_amws_1986", "n_amws"
)
missing_panel_cols <- setdiff(required_panel_cols, names(panel_year))
if (length(missing_panel_cols) > 0L) {
  stop("AMWS panel is missing columns: ", paste(missing_panel_cols, collapse = ", "))
}

panel_decade <- panel_year %>%
  transmute(
    GEOID = pad_geoid(GEOID),
    year = as.integer(year),
    population = as.numeric(population),
    county_births_estimate_year = as.numeric(county_births_estimate_year),
    n_amws_1906_1955_dedup = as.numeric(n_amws_1906_1955_dedup),
    n_amws_1986 = as.numeric(n_amws_1986),
    n_amws = as.numeric(n_amws)
  ) %>%
  filter(year >= 1800L, year <= 1960L, !is.na(GEOID)) %>%
  # Bin outcomes on the same decade grid as the treatment cohort (see the main
  # event-studies script). Standard profiles -> standard_decade(); g_shift
  # profiles -> alternative_decade() (birth years ending 7-9 shift forward).
  mutate(decade = event_decade(year, treatment_timing)) %>%
  group_by(GEOID, decade) %>%
  summarise(
    n_amws_1906_1955_dedup = sum(n_amws_1906_1955_dedup, na.rm = TRUE),
    n_amws_1986 = sum(n_amws_1986, na.rm = TRUE),
    n_amws = sum(n_amws, na.rm = TRUE),
    population = mean_or_na(population),
    county_births_estimate = sum_or_na(county_births_estimate_year),
    .groups = "drop"
  ) %>%
  mutate(
    unit_id = paste0("US_COUNTY_", GEOID),
    log1p_n_amws = log1p(n_amws),
    amws_per_100k = if_else(population > 0, 1e5 * n_amws / population, NA_real_),
    amws_per_1000_births = if_else(
      county_births_estimate > 0,
      1000 * n_amws / county_births_estimate,
      NA_real_
    )
  )

panel_decade_max <- max(panel_decade$decade)

sample_keys <- read_csv(
  file.path(results_dir, "all_sample_summary.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    treatment_spec,
    distance_bin_km = as.character(distance_bin_km),
    expected_treated = as.integer(n_treated_eligible),
    expected_controls = as.integer(n_never_treated_controls)
  )

expected_treated_by_cohort <- read_csv(
  file.path(results_dir, "all_treated_counts_by_cohort.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    treatment_spec,
    distance_bin_km = as.character(distance_bin_km),
    cohort = as.integer(g),
    expected_treated_cohort = as.integer(n_treated_eligible)
  ) %>%
  filter(expected_treated_cohort > 0L)

run_one_cohort <- function(data, outcome, spec_label, bin_label, cohort,
                           cohort_event_times, cores) {
  data_es <- data %>%
    transmute(
      unit_num = as.numeric(.data$unit_num),
      GEOID = .data$GEOID,
      decade = as.numeric(.data$decade),
      g = as.numeric(.data$g),
      y = as.numeric(.data[[outcome]]),
      x = log(as.numeric(.data$population))
    ) %>%
    filter(is.finite(y), is.finite(x))

  n_treated <- n_distinct(data_es$unit_num[data_es$g > 0])
  n_control <- n_distinct(data_es$unit_num[data_es$g == 0])
  n_expected_rows <- (n_treated + n_control) * length(cohort_event_times)
  base_result <- list(
    treatment_spec = spec_label,
    distance_bin_km = bin_label,
    outcome = outcome,
    cohort = cohort,
    fair_cohort = cohort - treatment_cohort_shift,
    event_time_min = min(cohort_event_times),
    event_time_max = max(cohort_event_times),
    n_expected_event_times = length(cohort_event_times),
    n_rows = nrow(data_es),
    n_expected_rows = n_expected_rows,
    n_treated_units = n_treated,
    n_control_units = n_control,
    panel_balanced = nrow(data_es) == n_expected_rows,
    baseline_population_complete = all(
      is.finite(data_es$x[data_es$decade == cohort - 10L])
    )
  )

  fail <- function(error, warnings = character()) {
    c(base_result, list(
      ok = FALSE,
      error = error,
      warnings = paste(unique(warnings), collapse = " | ")
    ))
  }
  if (n_treated == 0L) return(fail("No treated units in cohort."))
  if (n_control == 0L) return(fail("No never-treated controls."))
  if (!base_result$panel_balanced) return(fail("Cohort subpanel is not fully balanced."))
  if (!base_result$baseline_population_complete) {
    return(fail("Missing baseline log population at g-10."))
  }
  if (n_distinct(data_es$y) < 2L) return(fail("Outcome has insufficient variation."))

  captured_warnings <- character()
  tryCatch(
    {
      fit <- withCallingHandlers(
        {
          att <- did::att_gt(
            yname = "y",
            tname = "decade",
            idname = "unit_num",
            gname = "g",
            xformla = ~ x,
            data = data_es,
            panel = TRUE,
            allow_unbalanced_panel = FALSE,
            control_group = "nevertreated",
            est_method = "reg",
            base_period = "universal",
            bstrap = FALSE,
            cband = FALSE,
            cores = cores
          )
          dynamic <- did::aggte(
            att,
            type = "dynamic",
            min_e = min(cohort_event_times),
            max_e = max(cohort_event_times),
            na.rm = TRUE,
            bstrap = FALSE,
            cband = FALSE
          )
          list(att = att, dynamic = dynamic)
        },
        warning = function(w) {
          captured_warnings <<- c(captured_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )

      dynamic_rows <- tibble(
        treatment_spec = spec_label,
        distance_bin_km = bin_label,
        outcome = outcome,
        cohort = cohort,
        fair_cohort = cohort - treatment_cohort_shift,
        event_time = fit$dynamic$egt,
        estimate = fit$dynamic$att.egt,
        se = fit$dynamic$se.egt,
        ci_low = estimate - 1.96 * se,
        ci_high = estimate + 1.96 * se,
        n_treated_units = n_treated,
        n_control_units = n_control
      )

      c(base_result, list(
        ok = TRUE,
        error = NA_character_,
        warnings = paste(unique(captured_warnings), collapse = " | "),
        dynamic = dynamic_rows
      ))
    },
    error = function(e) fail(conditionMessage(e), captured_warnings)
  )
}

status_rows <- list()
dynamic_rows <- list()
result_index <- 0L

for (key_index in seq_len(nrow(sample_keys))) {
  spec_label <- sample_keys$treatment_spec[[key_index]]
  bin_label <- sample_keys$distance_bin_km[[key_index]]
  expected_controls <- sample_keys$expected_controls[[key_index]]
  source_dir <- world_fairs_output_dir(results_dir, spec_label, bin_label)
  output_dir <- world_fairs_output_dir(per_cohort_dir, spec_label, bin_label)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  treated_file <- file.path(source_dir, "treatment_assignment.csv")
  controls_file <- file.path(source_dir, "balanced_never_treated_controls.csv")
  if (!all(file.exists(c(treated_file, controls_file)))) {
    stop("Missing validated sample files in: ", source_dir)
  }

  treated <- read_csv(treated_file, show_col_types = FALSE) %>%
    transmute(
      unit_id,
      GEOID = pad_geoid(GEOID),
      fair_cohort = as.integer(first_exposure_decade),
      g = as.integer(g)
    )
  controls <- read_csv(controls_file, show_col_types = FALSE) %>%
    transmute(unit_id, GEOID = pad_geoid(GEOID), fair_cohort = NA_integer_, g = 0L)

  if (n_distinct(controls$unit_id) != expected_controls) {
    stop("Control count differs from validated sample in: ", source_dir)
  }

  cohorts <- sort(unique(treated$g))
  for (cohort in cohorts) {
    cohort_event_times <- seq(
      event_time_min,
      panel_decade_max - cohort,
      10L
    )
    required_decades <- cohort + cohort_event_times
    analysis_units <- bind_rows(
      treated %>% filter(g == cohort),
      controls
    ) %>%
      distinct(unit_id, .keep_all = TRUE)

    panel_cohort <- panel_decade %>%
      filter(decade %in% required_decades) %>%
      inner_join(analysis_units, by = c("unit_id", "GEOID")) %>%
      arrange(unit_id, decade) %>%
      mutate(unit_num = as.integer(factor(unit_id)))

    for (outcome in outcomes) {
      result_index <- result_index + 1L
      message(
        "Running per-cohort CSDID ", result_index, " | ",
        spec_label, " | ", ifelse(is.na(bin_label), "hosted", bin_label),
        " | ", outcome, " | g = ", cohort
      )
      result <- run_one_cohort(
        panel_cohort,
        outcome,
        spec_label,
        bin_label,
        cohort,
        cohort_event_times,
        cores
      )

      status_rows[[result_index]] <- tibble(
        treatment_spec = result$treatment_spec,
        distance_bin_km = result$distance_bin_km,
        outcome = result$outcome,
        cohort = result$cohort,
        fair_cohort = result$fair_cohort,
        event_time_min = result$event_time_min,
        event_time_max = result$event_time_max,
        n_expected_event_times = result$n_expected_event_times,
        ok = result$ok,
        n_rows = result$n_rows,
        n_expected_rows = result$n_expected_rows,
        n_treated_units = result$n_treated_units,
        n_control_units = result$n_control_units,
        panel_balanced = result$panel_balanced,
        baseline_population_complete = result$baseline_population_complete,
        warnings = result$warnings,
        error = result$error
      )
      if (isTRUE(result$ok)) dynamic_rows[[result_index]] <- result$dynamic
    }
  }
}

model_status <- bind_rows(status_rows)
dynamic_att <- bind_rows(dynamic_rows)
write_csv(model_status, file.path(per_cohort_dir, "all_model_status_per_cohort.csv"))
write_csv(dynamic_att, file.path(per_cohort_dir, "all_dynamic_att_per_cohort.csv"))

warning_summary <- model_status %>%
  filter(!is.na(warnings), nzchar(warnings)) %>%
  count(warnings, sort = TRUE, name = "n_models")
write_csv(warning_summary, file.path(per_cohort_dir, "warning_summary.csv"))

rendered_files <- character()
calendar_axis_min <- min(expected_treated_by_cohort$cohort) + event_time_min
calendar_axis_max <- panel_decade_max
calendar_axis_breaks <- seq(calendar_axis_min, calendar_axis_max, 20L)

for (key_index in seq_len(nrow(sample_keys))) {
  spec_label <- sample_keys$treatment_spec[[key_index]]
  bin_label <- sample_keys$distance_bin_km[[key_index]]
  output_dir <- world_fairs_output_dir(per_cohort_dir, spec_label, bin_label)
  local_status <- model_status %>%
    filter(
      treatment_spec == spec_label,
      if (is.na(bin_label)) is.na(distance_bin_km) else distance_bin_km == bin_label
    )
  local_dynamic <- dynamic_att %>%
    filter(
      treatment_spec == spec_label,
      if (is.na(bin_label)) is.na(distance_bin_km) else distance_bin_km == bin_label
    )
  write_csv(local_status, file.path(output_dir, "model_status_per_cohort.csv"))
  write_csv(local_dynamic, file.path(output_dir, "dynamic_att_per_cohort.csv"))

  for (outcome in outcomes) {
    plot_data <- local_dynamic %>%
      filter(.data$outcome == .env$outcome) %>%
      group_by(cohort) %>%
      mutate(cohort_event_time_max = max(event_time)) %>%
      ungroup() %>%
      mutate(
        decade_calendar = cohort + event_time,
        period = factor(
          if_else(event_time < 0, "Pre", "Post"),
          levels = c("Pre", "Post")
        ),
        cohort_label = if (treatment_cohort_shift == 0L) {
          paste0(
            "g = ", cohort,
            " (n_t=", n_treated_units,
            ", n_c=", n_control_units,
            ", e_max=", cohort_event_time_max, ")"
          )
        } else {
          paste0(
            "g_birth = ", cohort,
            " (fair g=", fair_cohort,
            ", n_t=", n_treated_units,
            ", n_c=", n_control_units,
            ", e_max=", cohort_event_time_max, ")"
          )
        }
      )
    if (nrow(plot_data) == 0L) next

    n_cohorts <- n_distinct(plot_data$cohort)
    cohort_label_levels <- plot_data %>%
      distinct(cohort, cohort_label) %>%
      arrange(cohort) %>%
      pull(cohort_label)
    plot_data <- plot_data %>%
      mutate(cohort_label = factor(cohort_label, levels = cohort_label_levels))
    cohort_lines <- plot_data %>%
      distinct(cohort_label, cohort)
    plot_y_limits <- plot_data %>%
      summarise(
        ymin = min(ci_low, na.rm = TRUE),
        ymax = max(ci_high, na.rm = TRUE)
      )
    treatment_label <- world_fairs_treatment_label(spec_label, bin_label)

    plot <- ggplot(
      plot_data,
      aes(x = decade_calendar, y = estimate, ymin = ci_low, ymax = ci_high)
    ) +
      geom_vline(
        data = cohort_lines,
        aes(xintercept = cohort),
        linetype = "dotted",
        colour = "gray60"
      ) +
      geom_point(aes(colour = period), size = 1.2, na.rm = TRUE) +
      geom_errorbar(aes(colour = period), width = 1.5, na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~ cohort_label, nrow = 1, scales = "fixed") +
      scale_x_continuous(
        breaks = calendar_axis_breaks
      ) +
      scale_color_manual(
        drop = FALSE,
        values = c(Pre = "#e87d72", Post = "#56bcc2"),
        breaks = c("Pre", "Post"),
        labels = c("Pre", "Post")
      ) +
      coord_cartesian(
        xlim = c(calendar_axis_min, calendar_axis_max),
        ylim = c(plot_y_limits$ymin, plot_y_limits$ymax)
      ) +
      labs(
        x = "Birth decade",
        y = "Effect",
        colour = NULL,
        title = str_wrap(
          paste0(
            outcome_labels[[outcome]],
            " — World's fairs, ", treatment_label,
            " (per-cohort ", plot_estimator_label, ", calendar axis)"
          ),
          90
        )
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(color = "darkgray", face = "bold", size = 11),
        axis.title = element_text(color = "darkgray", face = "bold", size = 11),
        strip.text = element_text(color = "darkgray", face = "bold", size = 9),
        strip.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom"
      )

    output_file <- file.path(
      output_dir,
      paste0("ES_percohort_", outcome_file_labels[[outcome]], ".png")
    )
    ggsave(
      output_file,
      plot,
      width = 3 * n_cohorts + 2,
      height = 4.5,
      dpi = 300
    )
    rendered_files <- c(rendered_files, output_file)
  }
}

expected_grid <- tidyr::crossing(
  expected_treated_by_cohort,
  outcome = outcomes
) %>%
  mutate(
    expected_event_time_min = event_time_min,
    expected_event_time_max = panel_decade_max - cohort,
    expected_event_times =
      (expected_event_time_max - expected_event_time_min) / 10L + 1L
  )
expected_models <- nrow(expected_grid)
expected_dynamic_rows <- sum(expected_grid$expected_event_times)
dynamic_grid <- dynamic_att %>%
  group_by(treatment_spec, distance_bin_km, outcome, cohort) %>%
  summarise(
    n_event_times = n(),
    observed_event_time_min = min(event_time),
    observed_event_time_max = max(event_time),
    .groups = "drop"
  ) %>%
  left_join(
    expected_grid %>%
      select(
        treatment_spec, distance_bin_km, outcome, cohort,
        expected_event_times, expected_event_time_min,
        expected_event_time_max
      ),
    by = c("treatment_spec", "distance_bin_km", "outcome", "cohort")
  ) %>%
  mutate(
    grid_complete =
      n_event_times == expected_event_times &
      observed_event_time_min == expected_event_time_min &
      observed_event_time_max == expected_event_time_max
  )
reference_rows <- dynamic_att %>% filter(event_time == reference_event_time)

status_validation <- model_status %>%
  left_join(
    expected_treated_by_cohort,
    by = c("treatment_spec", "distance_bin_km", "cohort")
  ) %>%
  left_join(
    sample_keys %>% select(treatment_spec, distance_bin_km, expected_controls),
    by = c("treatment_spec", "distance_bin_km")
  ) %>%
  mutate(
    treated_count_ok = n_treated_units == expected_treated_cohort,
    control_count_ok = n_control_units == expected_controls
  )
write_csv(status_validation, file.path(per_cohort_dir, "sample_validation.csv"))

run_validation <- tibble(
  check = c(
    "source_aggregate_validated",
    "expected_model_count",
    "all_models_successful",
    "expected_dynamic_row_count",
    "dynamic_grid_complete",
    "reference_period_normalized",
    "treated_counts_match",
    "fixed_control_counts_match",
    "cohort_panels_balanced",
    "baseline_population_complete",
    "expected_figure_count"
  ),
  passed = c(
    all(source_validation$passed),
    nrow(model_status) == expected_models,
    nrow(model_status) == expected_models && all(model_status$ok),
    nrow(dynamic_att) == expected_dynamic_rows,
    nrow(dynamic_grid) == expected_models &&
      all(dynamic_grid$grid_complete),
    nrow(reference_rows) == expected_models &&
      all(abs(reference_rows$estimate) < .Machine$double.eps^0.5),
    all(status_validation$treated_count_ok),
    all(status_validation$control_count_ok),
    all(model_status$panel_balanced),
    all(model_status$baseline_population_complete),
    length(rendered_files) == nrow(sample_keys) * length(outcomes) &&
      all(file.exists(rendered_files))
  ),
  detail = c(
    paste(sum(source_validation$passed), "of", nrow(source_validation)),
    paste(nrow(model_status), "of", expected_models),
    paste(sum(model_status$ok), "of", nrow(model_status)),
    paste(nrow(dynamic_att), "of", expected_dynamic_rows),
    paste(
      nrow(dynamic_grid),
      "model grids ending between",
      min(dynamic_grid$observed_event_time_max), "and",
      max(dynamic_grid$observed_event_time_max)
    ),
    paste(nrow(reference_rows), "normalized rows"),
    paste(sum(status_validation$treated_count_ok), "of", nrow(status_validation)),
    paste(sum(status_validation$control_count_ok), "of", nrow(status_validation)),
    paste(sum(model_status$panel_balanced), "of", nrow(model_status)),
    paste(sum(model_status$baseline_population_complete), "of", nrow(model_status)),
    paste(length(rendered_files), "of", nrow(sample_keys) * length(outcomes))
  )
)
write_csv(run_validation, file.path(per_cohort_dir, "run_validation.csv"))

elapsed <- difftime(Sys.time(), initial_time, units = "mins")
notes <- c(
  "USA-only AMWS world's-fairs event studies estimated separately by cohort.",
  paste0("Source profile: ", profile$name),
  paste0(
    "Event-time window by cohort: [", event_time_min,
    ", 1960-g], ending at the last available panel decade."
  ),
  paste0("Reference period: e = ", reference_event_time, "."),
  paste0("Treatment cohort shift from fair decade: ", treatment_cohort_shift, "."),
  paste0("Single-fair requirement within event window: ", single_fair_event_window, "."),
  paste0("Controls balanced over full calendar support: ", balance_controls_calendar, "."),
  paste0(
    "Control sample source profile: ",
    value_or(profile$control_sample_source_profile, profile$name), "."
  ),
  "Each model contains treated units from one cohort and the validated fixed",
  "never-treated pool for that treatment specification.",
  "Covariate: log county population, evaluated at g-10 by the universal base.",
  "Estimator: did::att_gt outcome regression; analytical pointwise 95% CIs.",
  paste0(
    "Figures use a common calendar axis [", calendar_axis_min, ", ",
    calendar_axis_max, "] and fixed y scales across cohorts within each figure."
  ),
  "Dotted vertical lines mark each treatment cohort; facet headers report e_max.",
  paste0("Models successful: ", sum(model_status$ok), " / ", nrow(model_status), "."),
  paste0("Figures rendered: ", length(rendered_files), "."),
  paste0("Elapsed minutes: ", round(as.numeric(elapsed), 2), ".")
)
writeLines(notes, file.path(per_cohort_dir, "notes.txt"))

if (!all(run_validation$passed)) {
  stop(
    "Per-cohort validation failed; see ",
    file.path(per_cohort_dir, "run_validation.csv")
  )
}

message("Completed per-cohort world's-fairs event studies in: ", per_cohort_dir)
