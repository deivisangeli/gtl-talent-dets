###############################################################################
# Export the US AMWS synthetic-DiD raw fits and pooled Dube-Zipperer figures to
# the standalone LaTeX figure file. This script reads existing estimates; it
# does not re-estimate models or alter result files in Dropbox.
#
# Run from any working directory:
#   Rscript analysis/world_fairs/export_us_amws_synthdid_project_artifacts.R
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) == 0L) {
  stop("Run this file with Rscript so the repository root can be resolved.")
}
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

source(file.path(repo_root, "paths.R"))
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (!dir.exists(user_data_dir)) {
    stop(
      "Could not find the Dropbox data root. Set TALENT_DETS_DATA_DIR; tried: ",
      TALENT_DETS_DATA_DIR, " and ", user_data_dir
    )
  }
  TALENT_DETS_DATA_DIR <- normalizePath(
    user_data_dir,
    winslash = "/",
    mustWork = TRUE
  )
}

project_dir <- file.path(repo_root, "project")
project_figure_dir <- file.path(project_dir, "figures")
project_table_dir <- file.path(project_dir, "tables")
project_chapter_dir <- file.path(project_dir, "chapters")
dir.create(project_figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(project_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(project_chapter_dir, recursive = TRUE, showWarnings = FALSE)

results_root <- file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", "synthdid"
)

specifications <- tribble(
  ~specification, ~source_subdir, ~expected_variant, ~expected_pre_min, ~expected_pre_max,
  "fixed", "us_county_amws_figure5_lead20_e0_nonpooled", "fixed_m30", 3L, 3L,
  "maxpre", "us_county_amws_figure5_lead20_e0_nonpooled_max_pre", "max_pre", 3L, 9L
) %>%
  mutate(model_subdir = file.path(results_root, source_subdir, "models_amws_per_1000_births"))

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

validate_specification <- function(spec_row) {
  model_dir <- spec_row$model_subdir[[1L]]
  required_files <- c(
    "synthdid_event_effects.csv",
    "synthdid_fit_paths.csv",
    "synthdid_model_status.csv"
  )
  required_paths <- file.path(model_dir, required_files)
  missing_paths <- required_paths[!file.exists(required_paths)]
  assert_true(
    length(missing_paths) == 0L,
    paste("Missing required result files:", paste(missing_paths, collapse = "; "))
  )

  effects <- read_csv(required_paths[[1L]], show_col_types = FALSE)
  fit_paths <- read_csv(required_paths[[2L]], show_col_types = FALSE)
  status <- read_csv(required_paths[[3L]], show_col_types = FALSE)
  removed_inference_columns <- c("placebo_percentile_rank", "placebo_p_value")
  assert_true(
    !any(removed_inference_columns %in% names(effects)),
    "US event outputs still contain removed rank/p-value columns."
  )

  assert_true(nrow(effects) == 12L, "Expected 12 event-effect rows.")
  assert_true(n_distinct(effects$model_id) == 12L, "Expected 12 unique models.")
  assert_true(
    identical(sort(unique(effects$decade_definition)), c("alternative", "standard")),
    "Expected standard and alternative decade definitions."
  )
  assert_true(
    all(count(effects, decade_definition)$n == 6L),
    "Expected six treated events under each decade definition."
  )
  assert_true(
    all(effects$reference_event_time == -10L) &&
      all(effects$treatment_start_event_time == 0L) &&
      all(effects$fair_event_time == 20L),
    "Corrected timing metadata must be reference=-10, treatment=0, fair=+20."
  )
  assert_true(
    all(effects$variant == spec_row$expected_variant[[1L]]),
    "Unexpected model variant."
  )
  assert_true(
    all(effects$n_control_units == 30L) && all(effects$n_treated_units == 1L),
    "Each non-pooled model must contain one treated unit and 30 donors."
  )
  assert_true(
    all(effects$n_post_periods == 6L),
    "Expected six treated horizons, e=0 through e=+50."
  )
  assert_true(
    min(effects$n_pre_periods) == spec_row$expected_pre_min[[1L]] &&
      max(effects$n_pre_periods) == spec_row$expected_pre_max[[1L]],
    "Unexpected pre-period range."
  )
  assert_true(
    nrow(status) == 12L && all(status$model_status == "estimated"),
    "All 12 models must have estimated status."
  )
  assert_true(
    all(status$n_control_units == 30L),
    "Model status does not confirm 30 donors."
  )

  required_fit_columns <- c(
    "model_id", "decade_definition", "treated_county_name", "fair_year",
    "event_time", "treated_outcome", "synthetic_outcome"
  )
  assert_true(
    all(required_fit_columns %in% names(fit_paths)),
    "Synthetic-DiD fit paths have an unexpected schema."
  )
  fit_geometry <- fit_paths %>%
    count(model_id, name = "observed_periods") %>%
    left_join(
      effects %>% transmute(
        model_id, expected_periods = n_pre_periods + n_post_periods
      ),
      by = "model_id"
    )
  assert_true(
    nrow(fit_geometry) == 12L &&
      all(fit_geometry$observed_periods == fit_geometry$expected_periods),
    "Synthetic-DiD fit paths do not match the expected model windows."
  )
  assert_true(
    !anyDuplicated(fit_paths[c("model_id", "event_time")]) &&
      all(is.finite(fit_paths$treated_outcome)) &&
      all(is.finite(fit_paths$synthetic_outcome)),
    "Synthetic-DiD fit paths contain duplicates or non-finite outcomes."
  )

  list(effects = effects, fit_paths = fit_paths)
}

validated <- map(seq_len(nrow(specifications)), function(i) {
  validate_specification(specifications[i, ])
})
names(validated) <- specifications$specification

obsolete_figure_names <- c(
  "WF_US_synthdid_fixed_standard_event_paths.png",
  "WF_US_synthdid_fixed_alternative_event_paths.png",
  "WF_US_synthdid_fixed_event_effects.png",
  "WF_US_synthdid_maxpre_standard_event_paths.png",
  "WF_US_synthdid_maxpre_alternative_event_paths.png",
  "WF_US_synthdid_maxpre_event_effects.png"
)
unlink(file.path(project_figure_dir, obsolete_figure_names))

global_y_max <- max(map_dbl(validated, function(x) {
  max(c(x$fit_paths$treated_outcome, x$fit_paths$synthetic_outcome))
}))
global_y_limits <- c(0, global_y_max * 1.04)

render_fit_figure <- function(spec_name, decade_definition) {
  result <- validated[[spec_name]]
  effects_plot <- result$effects %>%
    filter(.data$decade_definition == !!decade_definition) %>%
    arrange(fair_year, treated_county_name) %>%
    mutate(
      facet_label = paste0(
        treated_county_name, " (", fair_year, ")\n",
        "Pre-RMSPE = ", sprintf("%.4f", pre_rmspe)
      ),
      facet_order = row_number()
    )
  facet_levels <- effects_plot$facet_label

  fit_long <- result$fit_paths %>%
    filter(.data$decade_definition == !!decade_definition) %>%
    inner_join(
      effects_plot %>% select(model_id, facet_label, facet_order),
      by = "model_id"
    ) %>%
    pivot_longer(
      c(treated_outcome, synthetic_outcome),
      names_to = "series", values_to = "outcome_value"
    ) %>%
    mutate(
      facet_label = factor(facet_label, levels = facet_levels),
      series = factor(
        series,
        levels = c("treated_outcome", "synthetic_outcome"),
        labels = c("Treated county", "Synthetic control")
      )
    )

  spec_label <- if (identical(spec_name, "fixed")) {
    "Fixed window"
  } else {
    "Maximum pre-periods"
  }
  decade_label <- str_to_sentence(decade_definition)
  x_limits <- if (identical(spec_name, "fixed")) c(-30, 50) else c(-90, 50)
  x_breaks <- seq(x_limits[[1L]], x_limits[[2L]], by = 20)

  fit_plot <- ggplot(
    fit_long,
    aes(
      x = event_time, y = outcome_value, colour = series,
      linetype = series, shape = series, group = series
    )
  ) +
    geom_vline(
      xintercept = -10, colour = "grey35", linewidth = 0.35,
      linetype = "longdash"
    ) +
    geom_vline(
      xintercept = 0, colour = "#b2182b", linewidth = 0.4,
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = 20, colour = "#2166ac", linewidth = 0.4,
      linetype = "dotted"
    ) +
    geom_line(linewidth = 0.65, na.rm = TRUE) +
    geom_point(size = 1.35, stroke = 0.25, na.rm = TRUE) +
    facet_wrap(~facet_label, ncol = 2, drop = FALSE) +
    scale_colour_manual(values = c(
      "Treated county" = "black", "Synthetic control" = "#377eb8"
    )) +
    scale_linetype_manual(values = c(
      "Treated county" = "solid", "Synthetic control" = "longdash"
    )) +
    scale_shape_manual(values = c(
      "Treated county" = 16, "Synthetic control" = 17
    )) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    coord_cartesian(ylim = global_y_limits) +
    labs(
      title = "US world's fairs: treated and synthetic-control trajectories",
      subtitle = paste0(spec_label, "; ", str_to_lower(decade_label), " decades"),
      x = "Event time relative to lead-20 cohort",
      y = "AMWS scientists per 1,000 county births",
      colour = NULL, linetype = NULL, shape = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 8.5),
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.title.position = "plot",
      plot.margin = margin(10, 12, 10, 12)
    )

  output_name <- paste0(
    "WF_US_synthdid_", spec_name, "_", decade_definition, "_fit.png"
  )
  ggsave(
    file.path(project_figure_dir, output_name),
    fit_plot, width = 10.5, height = 9, dpi = 220
  )
  output_name
}

fit_figure_names <- crossing(
  specification = c("fixed", "maxpre"),
  decade_definition = c("standard", "alternative")
) %>%
  mutate(file_name = map2_chr(
    specification, decade_definition, render_fit_figure
  ))

pooled_dir <- file.path(
  results_root, "us_county_amws_figure5_lead20_e0_nonpooled", "pooled_dz"
)
pooled_files <- c(
  dynamic = file.path(pooled_dir, "dz_pooled_dynamic_effects.csv"),
  dynamic_trim = file.path(pooled_dir, "dz_pooled_dynamic_by_trim.csv"),
  pooled_att = file.path(pooled_dir, "dz_pooled_att.csv"),
  event_effects = file.path(pooled_dir, "dz_event_effects.csv"),
  qc = file.path(pooled_dir, "dz_qc.csv")
)
missing_pooled <- pooled_files[!file.exists(pooled_files)]
assert_true(
  length(missing_pooled) == 0L,
  paste("Missing pooled DZ outputs:", paste(missing_pooled, collapse = "; "))
)

pooled_qc <- read_csv(pooled_files[["qc"]], show_col_types = FALSE)
assert_true(nrow(pooled_qc) > 0L && all(pooled_qc$passed),
            "The pooled DZ analysis did not pass validation.")
pooled_dynamic <- read_csv(pooled_files[["dynamic"]], show_col_types = FALSE)
pooled_dynamic_trim <- read_csv(
  pooled_files[["dynamic_trim"]], show_col_types = FALSE
)
pooled_att <- read_csv(pooled_files[["pooled_att"]], show_col_types = FALSE)
pooled_events <- read_csv(
  pooled_files[["event_effects"]], show_col_types = FALSE
)
assert_true(
  nrow(pooled_dynamic) == 18L && all(pooled_dynamic$n_events == 6L) &&
    all(pooled_dynamic$reference_event_time == -10L),
  "Unexpected full-sample pooled event-study output."
)
assert_true(
  nrow(pooled_dynamic_trim) == 90L &&
    setequal(unique(pooled_dynamic_trim$n_events), 2:6) &&
    all(pooled_dynamic_trim$reference_event_time == -10L),
  "Unexpected progressive-trimming event-study output."
)
assert_true(
  nrow(pooled_att) == 2L && all(pooled_att$n_events == 6L) &&
    all(pooled_att$randomization_draws == 1000L),
  "Expected two six-event pooled estimates with 1,000 randomization draws."
)

definition_levels <- c("standard", "alternative")
definition_labels <- c(standard = "Standard decades",
                       alternative = "Alternative decades")
paper_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(8, 12, 8, 18)
  )

pooled_dynamic <- pooled_dynamic %>%
  mutate(
    decade_definition = factor(
      decade_definition, levels = definition_levels,
      labels = unname(definition_labels[definition_levels])
    )
  )
effect_panel <- pooled_dynamic %>%
  transmute(
    decade_definition, event_time,
    panel = "Reference-normalized pooled effect",
    estimate = pooled_effect, lower = effect_ci_lower,
    upper = effect_ci_upper, p_value = randomization_p,
    reference = 0, band_type = "95% effect confidence interval"
  )
rank_panel <- pooled_dynamic %>%
  transmute(
    decade_definition, event_time,
    panel = "Mean placebo percentile rank",
    estimate = mean_percentile_rank, lower = rank_null_lower,
    upper = rank_null_upper, p_value = rank_randomization_p,
    reference = 16 / 31, band_type = "95% rank null envelope"
  )
plot_dynamic <- bind_rows(effect_panel, rank_panel) %>%
  mutate(
    panel = factor(
      panel,
      levels = c("Reference-normalized pooled effect",
                 "Mean placebo percentile rank")
    ),
    significant = if_else(is.finite(p_value), p_value < 0.05, FALSE),
    interval_segment = case_when(
      event_time == -10L ~ "reference",
      event_time < 0L ~ "pre",
      TRUE ~ "post"
    )
  )

pooled_plot <- ggplot(
  plot_dynamic,
  aes(x = event_time, y = estimate,
      group = interaction(panel, decade_definition))
) +
  geom_ribbon(
    data = filter(plot_dynamic, is.finite(lower), is.finite(upper)),
    aes(ymin = lower, ymax = upper, fill = band_type,
        group = interaction(panel, decade_definition, interval_segment)),
    alpha = 0.72, colour = NA
  ) +
  geom_hline(aes(yintercept = reference), colour = "grey45", linewidth = 0.35) +
  geom_vline(xintercept = -10, linetype = "longdash", colour = "grey45") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#b2182b") +
  geom_vline(xintercept = 20, linetype = "dotted", colour = "#2166ac") +
  geom_line(linewidth = 0.55, na.rm = TRUE) +
  geom_point(aes(colour = significant), size = 2.1, na.rm = TRUE) +
  facet_grid(panel ~ decade_definition, scales = "free_y") +
  scale_fill_manual(
    name = "Band",
    values = c(
      `95% effect confidence interval` = "grey76",
      `95% rank null envelope` = "#b7d4e8"
    )
  ) +
  scale_colour_manual(
    name = "Horizon test",
    values = c(`FALSE` = "black", `TRUE` = "#b22222"),
    labels = c(`FALSE` = "p >= 0.05", `TRUE` = "p < 0.05")
  ) +
  scale_x_continuous(breaks = seq(-30, 50, 10)) +
  labs(
    title = "US world's fairs: pooled synthetic-DiD event study",
    subtitle = paste0(
      "Reference e=-10 (grey long-dash); treatment e=0 (red dash); ",
      "fair e=+20 (blue dot)"
    ),
    x = "Event time (years relative to the lead-20 treatment decade)",
    y = NULL
  ) +
  paper_theme

pooled_figure_name <- "WF_US_synthdid_fixed_pooled_event_study.png"
ggsave(
  file.path(project_figure_dir, pooled_figure_name), pooled_plot,
  width = 11.5, height = 8.2, units = "in", dpi = 300, bg = "white"
)

render_trim_event_study <- function(decade_definition) {
  d <- pooled_dynamic_trim %>%
    filter(.data$decade_definition == !!decade_definition) %>%
    arrange(worst_fitting_events_dropped, event_time)
  stage_info <- d %>%
    distinct(worst_fitting_events_dropped, n_events) %>%
    arrange(worst_fitting_events_dropped) %>%
    mutate(stage_label = sprintf(
      "Drop %d (n = %d)", worst_fitting_events_dropped, n_events
    ))
  d <- d %>%
    left_join(stage_info, by = c(
      "worst_fitting_events_dropped", "n_events"
    )) %>%
    mutate(
      panel = factor(stage_label, levels = stage_info$stage_label),
      significant = if_else(
        is.finite(randomization_p), randomization_p < 0.05, FALSE
      ),
      interval_segment = case_when(
        event_time == -10L ~ "reference",
        event_time < 0L ~ "pre",
        TRUE ~ "post"
      )
    )
  decade_label <- definition_labels[[decade_definition]]
  p <- ggplot(d, aes(event_time, pooled_effect, group = panel)) +
    geom_ribbon(
      data = filter(d, is.finite(effect_ci_lower), is.finite(effect_ci_upper)),
      aes(ymin = effect_ci_lower, ymax = effect_ci_upper,
          group = interaction(panel, interval_segment)),
      fill = "grey76", alpha = 0.72, colour = NA
    ) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = -10, linetype = "longdash", colour = "grey45") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "#b2182b") +
    geom_vline(xintercept = 20, linetype = "dotted", colour = "#2166ac") +
    geom_line(linewidth = 0.55) +
    geom_point(aes(colour = significant), size = 2) +
    facet_wrap(~panel, ncol = 2, scales = "free_y") +
    scale_colour_manual(
      name = "Horizon test",
      values = c(`FALSE` = "black", `TRUE` = "#b22222"),
      labels = c(`FALSE` = "p >= 0.05", `TRUE` = "p < 0.05")
    ) +
    scale_x_continuous(breaks = seq(-30, 50, 10)) +
    labs(
      title = paste0(
        "US pooled event studies by pre-fit trimming stage: ",
        str_to_lower(decade_label)
      ),
      subtitle = paste0(
        "Grey ribbons: 95% inverted CIs; reference e=-10, treatment e=0, ",
        "fair e=+20"
      ),
      x = "Event time (years relative to the lead-20 treatment decade)",
      y = "Reference-normalized pooled effect"
    ) +
    paper_theme
  output_name <- paste0(
    "WF_US_synthdid_fixed_", decade_definition,
    "_trim_event_studies.png"
  )
  ggsave(
    file.path(project_figure_dir, output_name), p,
    width = 11.5, height = 10.2, units = "in", dpi = 300, bg = "white"
  )
  output_name
}

trim_figure_names <- set_names(
  map_chr(definition_levels, render_trim_event_study), definition_levels
)

figure_block <- function(file_name, caption, label, notes) {
  c(
    "\\begin{figure}[H]",
    "\\centering",
    paste0(
      "\\includegraphics[width=\\textwidth,height=0.67\\textheight,keepaspectratio]",
      "{figures/", file_name, "}"
    ),
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\caption*{\\footnotesize Notes: ", notes, "}"),
    "\\end{figure}",
    ""
  )
}

fit_notes_fixed <- paste0(
  "Each panel compares one treated county with its synthetic control using the ",
  "raw outcome, AMWS scientists born per 1,000 county births. Each model uses a ",
  "30-county donor pool and three pre-treatment decades; panel headers report ",
  "pre-treatment RMSPE. The grey long-dashed line marks $e=-10$, the reference ",
  "and final untreated period; the red dashed line marks $e=0$, when treatment ",
  "begins; and the blue dotted line marks $e=+20$, when the fair occurs."
)
fit_notes_maxpre <- paste0(
  "Each panel compares one treated county with its synthetic control using the ",
  "raw outcome, AMWS scientists born per 1,000 county births. Each model uses a ",
  "30-county donor pool and the maximum admissible pre-treatment history, ranging ",
  "from three to nine decades; panel headers report pre-treatment RMSPE. The grey ",
  "long-dashed line marks $e=-10$, the reference and final untreated period; the ",
  "red dashed line marks $e=0$, when treatment begins; and the blue dotted line ",
  "marks $e=+20$, when the fair occurs."
)

pooled_result_note <- pooled_att %>%
  mutate(
    definition_label = recode(
      decade_definition, standard = "standard", alternative = "alternative"
    ),
    result = sprintf(
      "%s decades: pooled effect $%.3f$, mean rank $%.3f$, joint rank $p=%.3f$",
      definition_label, pooled_att, mean_percentile_rank,
      randomization_p_rank_two_sided
    )
  ) %>%
  arrange(match(decade_definition, definition_levels)) %>%
  pull(result) %>%
  str_c(collapse = "; ")

pooled_notes <- paste0(
  "Fixed-window Dube--Zipperer pooling of six treated fairs under each decade ",
  "definition. Each observed case uses 30 donors and $e\\in[-30,+50]$. The top ",
  "panels report the equal-weighted mean treated-minus-synthetic gap after ",
  "subtracting the final untreated gap at $e=-10$; grey ribbons are pointwise ",
  "95\\% confidence intervals obtained by inverting the two-sided joint ",
  "randomization test. The bottom panels report the mean event-specific placebo ",
  "percentile rank; blue ribbons are 2.5th--97.5th-percentile randomization-null ",
  "envelopes, not confidence intervals. No interval or rank is reported at the ",
  "reference period. Red points have uncorrected horizon-specific $p<0.05$. ",
  "For each fair, the original 30 donors define the placebo-treated identities, ",
  "but every placebo receives a newly selected 30-county control pool from the ",
  "full event-eligible never-host universe using the same five pre-treatment ",
  "matching variables and 50-km fair-venue exclusion. The 1,000 joint draws ",
  "preserve timing and prohibit duplicate placebo-treated counties within a draw. ",
  "Exact zero pre-RMSPE placebo cases are retained at their signed extreme rank; ",
  "a numerical noise floor is used only to recover otherwise unidentified ",
  "synthetic weights. ", str_to_sentence(pooled_result_note), "."
)

trim_order <- pooled_events %>%
  group_by(decade_definition) %>%
  arrange(desc(nrmspe_pre), .by_group = TRUE) %>%
  summarise(
    removal_order = str_c(treated_county_name[1:4], collapse = ", "),
    .groups = "drop"
  )

trim_notes <- function(decade_definition) {
  order_text <- trim_order %>%
    filter(.data$decade_definition == !!decade_definition) %>%
    pull(removal_order)
  paste0(
    "Fixed $e\\in[-30,+50]$ pooled synthetic-DiD specification using ",
    decade_definition, " decades. Panels show the event-study path after ",
    "progressively removing zero through four treated fairs, retaining six ",
    "through two fairs. At every stage the event with the largest normalized ",
    "pre-treatment RMSPE is removed, using pre-treatment information only; the ",
    "same fit-based selection rule is repeated inside each of 1,000 joint ",
    "randomization draws. The removal order is ", order_text, ". Event time ",
    "$-10$ is the reference final untreated period; treatment begins at $e=0$ ",
    "and the fair occurs at $e=+20$. Grey ribbons are pointwise 95\\% confidence ",
    "intervals obtained by inverting the two-sided joint-randomization test; no ",
    "interval is reported at the reference period. Each assigned placebo uses ",
    "its independently rebuilt 30-county donor pool."
  )
}

chapter_tex <- c(
  "% Generated by analysis/world_fairs/export_us_amws_synthdid_project_artifacts.R",
  "% Included by project/synth_dids.tex and the compatibility wrapper.",
  "",
  "\\chapter{World Fairs}",
  "",
  "\\section{Raw fits}",
  "",
  figure_block(
    "WF_US_synthdid_fixed_standard_fit.png",
    "US world's fairs: treated and synthetic-control trajectories, fixed window, standard decades",
    "fig:wf_us_synthdid_fixed_standard_fit",
    fit_notes_fixed
  ),
  figure_block(
    "WF_US_synthdid_fixed_alternative_fit.png",
    "US world's fairs: treated and synthetic-control trajectories, fixed window, alternative decades",
    "fig:wf_us_synthdid_fixed_alternative_fit",
    fit_notes_fixed
  ),
  figure_block(
    "WF_US_synthdid_maxpre_standard_fit.png",
    "US world's fairs: treated and synthetic-control trajectories, maximum pre-periods, standard decades",
    "fig:wf_us_synthdid_maxpre_standard_fit",
    fit_notes_maxpre
  ),
  figure_block(
    "WF_US_synthdid_maxpre_alternative_fit.png",
    "US world's fairs: treated and synthetic-control trajectories, maximum pre-periods, alternative decades",
    "fig:wf_us_synthdid_maxpre_alternative_fit",
    fit_notes_maxpre
  ),
  "\\clearpage",
  "\\section{Pooled regression}",
  "",
  figure_block(
    pooled_figure_name,
    "US world's fairs: pooled synthetic-DiD event study",
    "fig:wf_us_synthdid_fixed_pooled_event_study",
    pooled_notes
  ),
  "\\clearpage",
  "\\section{Progressive pre-fit trimming}",
  "",
  figure_block(
    trim_figure_names[["standard"]],
    "US world's fairs: pooled event studies by pre-fit trimming stage, standard decades",
    "fig:wf_us_synthdid_fixed_standard_trimming",
    trim_notes("standard")
  ),
  figure_block(
    trim_figure_names[["alternative"]],
    "US world's fairs: pooled event studies by pre-fit trimming stage, alternative decades",
    "fig:wf_us_synthdid_fixed_alternative_trimming",
    trim_notes("alternative")
  )
)
writeLines(
  chapter_tex,
  file.path(project_chapter_dir, "us_synthdid_world_fairs.tex"),
  useBytes = TRUE
)

compatibility_tex <- c(
  "% Generated by analysis/world_fairs/export_us_amws_synthdid_project_artifacts.R",
  "% Compatibility entrypoint for the World Fairs chapter only.",
  "\\documentclass[12pt,oneside]{report}",
  "\\usepackage{graphicx}",
  "\\usepackage[a4paper,margin=2cm]{geometry}",
  "\\usepackage{caption}",
  "\\usepackage{float}",
  "\\usepackage{titlesec}",
  "",
  "\\titleformat{\\chapter}[hang]",
  "  {\\normalfont\\LARGE\\bfseries}",
  "  {Chapter \\thechapter}{1em}{}",
  "\\titlespacing*{\\chapter}{0pt}{-30pt}{16pt}",
  "",
  "\\begin{document}",
  "\\input{chapters/us_synthdid_world_fairs}",
  "\\end{document}"
)
writeLines(
  compatibility_tex,
  file.path(project_dir, "wf_us_synthdid_figures.tex"),
  useBytes = TRUE
)

latex_escape <- function(x) {
  x %>%
    str_replace_all("\\\\", "\\\\textbackslash\\{\\}") %>%
    str_replace_all("([&%$#_{}])", "\\\\\\1")
}

format_effect <- function(x) {
  if_else(x < 0, paste0("$-", sprintf("%.3f", abs(x)), "$"), paste0("$", sprintf("%.3f", x), "$"))
}

make_table_rows <- function(effects) {
  effects %>%
    mutate(
      decade_order = match(decade_definition, c("standard", "alternative")),
      decade_label = recode(decade_definition, standard = "Standard", alternative = "Alternative"),
      fair_display = if_else(
        str_detect(fair_name, "\\([0-9]{4}\\)$"),
        fair_name,
        paste0(fair_name, " (", fair_year, ")")
      ),
      event_label = paste0(
        latex_escape(treated_county_name), ": ", latex_escape(fair_display)
      )
    ) %>%
    arrange(decade_order, fair_year, treated_county_name) %>%
    transmute(
      line = paste0(
        decade_label, " & ", event_label, " & ", treatment_cohort,
        " & ", n_pre_periods, " & ", format_effect(event_effect), " \\\\"
      )
    ) %>%
    pull(line)
}

table_block <- function(effects, caption, label, notes) {
  rows <- make_table_rows(effects)
  alternative_start <- which(str_starts(rows, "Alternative"))[[1L]]
  rows <- append(rows, "\\addlinespace", after = alternative_start - 1L)
  c(
    "\\begin{table}[p]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{3.5pt}",
    "\\begin{tabular}{>{\\raggedright\\arraybackslash}p{2.0cm}>{\\raggedright\\arraybackslash}p{8.0cm}rrr}",
    "\\toprule",
    "Decades & Treated event & Cohort & Pre & Effect \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    paste0("\\caption*{\\footnotesize Notes: ", notes, "}"),
    "\\end{table}",
    ""
  )
}

common_table_notes <- paste0(
  "The outcome is AMWS scientists born per 1,000 county births. Effect is the ",
  "mean treated-minus-synthetic gap over $e=0,+10,\\ldots,+50$, normalized to ",
  "the final untreated period $e=-10$. Treatment begins at $e=0$ and the fair ",
  "occurs at $e=+20$. Every model is non-pooled and uses one treated county, ",
  "30 donors, and 30 successful placebos; the placebos supply the null envelopes ",
  "shown in the accompanying figures."
)

table_tex <- c(
  "% Generated by analysis/world_fairs/export_us_amws_synthdid_project_artifacts.R",
  "% Include from project/main.tex with: \\input{tables/wf_us_synthdid_results}",
  "",
  table_block(
    validated$fixed$effects,
    "US world's fairs: non-pooled synthetic-DiD estimates, fixed window",
    "tab:wf_us_synthdid_fixed",
    paste0(common_table_notes, " All models use three pre-treatment decades.")
  ),
  table_block(
    validated$maxpre$effects,
    "US world's fairs: non-pooled synthetic-DiD estimates, maximum pre-periods",
    "tab:wf_us_synthdid_maxpre",
    paste0(
      common_table_notes,
      " Models use the maximum admissible pre-treatment history, ranging from three to nine decades."
    )
  )
)
writeLines(
  table_tex,
  file.path(project_table_dir, "wf_us_synthdid_results.tex"),
  useBytes = TRUE
)

message("Exported four raw-fit and three pooled US synthetic-DiD figures.")
message("Wrote project/chapters/us_synthdid_world_fairs.tex.")
message("Wrote the project/wf_us_synthdid_figures.tex compatibility wrapper.")
message("Wrote project/tables/wf_us_synthdid_results.tex.")
