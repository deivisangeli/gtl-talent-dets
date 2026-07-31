###############################################################################
# CSDID event studies per cohort, each cohort using its MAXIMUM available
# event-time window [-20, 1950 - g]. Balance is applied per cohort on calendar
# decades [g-20, 1950]. Each cohort estimated on its own subpanel (treated of
# that cohort + all never-treated runner_ups pooled). Facet by cohort with
# free x/y scales (each cohort has a different post-treatment horizon).
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(did)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "prep", "raw_paths.R"))

analysis_min_decade <- 1830L
analysis_max_decade <- 1950L
min_event_time <- -20L
reference_event_time <- 0L
outcomes <- c("n_amws", "amws_per_1000_births", "county_births_estimate")
control_var <- "births_baseline"

outcome_labels <- c(
  n_amws                 = "AMWS scientists born",
  amws_per_1000_births   = "AMWS scientists born per 1000 births",
  county_births_estimate = "County births estimate"
)
timing_labels <- c(
  standard_decade    = "standard decade",
  alternative_decade = "alternative decade"
)

results_subdir <- "amws_county_pairs_temporal_support_1830_1950_baseline_pop_per_cohort_max_e"
results_subdir_path <- function(...) {
  path <- results_file_path("land_grants", "event_studies", results_subdir, ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}

panel <- read_csv(
  output_file_path("land_grants", "amws_temporal_support_county_decade_1830_1950.csv"),
  show_col_types = FALSE
) %>%
  mutate(GEOID = str_pad(as.character(as.integer(GEOID)), 5, pad = "0")) %>%
  filter(between(decade, analysis_min_decade, analysis_max_decade))

# Alternative-decade outcome/denominator panel (births ending 7-9 shifted forward
# one decade), used only by the alternative_decade timing.
panel_alt <- read_csv(
  output_file_path("land_grants", "amws_temporal_support_county_decade_1830_1950_alt.csv"),
  show_col_types = FALSE
) %>%
  mutate(GEOID = str_pad(as.character(as.integer(GEOID)), 5, pad = "0")) %>%
  filter(between(decade, analysis_min_decade, analysis_max_decade))

units <- read_csv(
  output_file_path("land_grants", "andrews_event_county_units_1850_1920.csv"),
  show_col_types = FALSE
) %>%
  mutate(GEOID = str_pad(as.character(as.integer(GEOID)), 5, pad = "0"))

build_stack <- function(timing_var, timing_name, panel_src) {
  stack_units <- units %>%
    mutate(
      treatment_decade = .data[[timing_var]],
      control_decade = treatment_decade - 10L,
      g = if_else(sample_role == "treated", treatment_decade, 0L),
      timing = timing_name,
      stack_unit_id = paste(event_id, GEOID, sample_role, sep = "_"),
      stack_unit_num = dense_rank(stack_unit_id)
    )

  baseline <- stack_units %>%
    select(stack_unit_id, GEOID, control_decade) %>%
    left_join(
      panel_src %>% select(GEOID, control_decade = decade,
                       population_baseline = population,
                       baseline_population_source = population_source,
                       births_baseline = county_births_estimate),
      by = c("GEOID", "control_decade")
    )

  stack_units %>%
    left_join(baseline, by = c("stack_unit_id", "GEOID", "control_decade")) %>%
    inner_join(panel_src, by = "GEOID") %>%
    arrange(event_id, sample_role, GEOID, decade)
}

panel_by_timing <- list(
  standard_decade = build_stack("g_std", "standard_decade", panel),
  alternative_decade = build_stack("g_shift", "alternative_decade", panel_alt)
)

renormalize_dynamic_at <- function(dynamic, ref_e = 0L) {
  if_mat <- dynamic$inf.function$dynamic.inf.func.e
  egt <- dynamic$egt
  att <- dynamic$att.egt
  n <- nrow(if_mat)
  col_ref <- which(egt == ref_e)
  if (length(col_ref) != 1L) {
    stop("reference event_time = ", ref_e, " not present in dynamic$egt")
  }
  if_new <- sweep(if_mat, 1, if_mat[, col_ref], "-")
  att_new <- att - att[col_ref]
  se_new <- sqrt(colMeans(if_new^2) / n)
  tibble(
    event_time = egt,
    estimate = att_new,
    se = se_new,
    ci_low = att_new - 1.96 * se_new,
    ci_high = att_new + 1.96 * se_new
  )
}

run_csdid_one_cohort <- function(panel_full, cohort, outcome_col) {
  et_max <- analysis_max_decade - cohort
  needed_decades <- seq(cohort + min_event_time, cohort + et_max, 10L)

  sub <- panel_full %>%
    filter(g == cohort | g == 0)

  # per-cohort calendar balance on population support (consistent across outcomes):
  # keep stack_units with non-NA population at all needed calendar decades and
  # non-NA covariate (baseline births is unit-level, constant within stack_unit)
  sub <- sub %>%
    group_by(stack_unit_num) %>%
    mutate(
      .ok = all(needed_decades %in% decade[!is.na(population)]) &&
        all(!is.na(.data[[control_var]]))
    ) %>%
    ungroup() %>%
    filter(.ok) %>%
    select(-.ok)

  data_es <- sub %>%
    select(stack_unit_num, GEOID, decade, g, event_id, sample_role,
           all_of(outcome_col), all_of(control_var)) %>%
    rename(y = all_of(outcome_col), x = all_of(control_var)) %>%
    filter(!is.na(y), is.finite(y), !is.na(x), is.finite(x))

  if (!any(data_es$g > 0) || !any(data_es$g == 0)) return(NULL)

  tryCatch(
    suppressWarnings({
      att <- did::att_gt(
        yname = "y", tname = "decade", idname = "stack_unit_num", gname = "g",
        xformla = ~ x, data = data_es, control_group = "nevertreated",
        est_method = "reg", base_period = "universal",
        allow_unbalanced_panel = TRUE, cores = 1, bstrap = FALSE
      )
      dynamic <- did::aggte(
        att, type = "dynamic", na.rm = TRUE,
        min_e = min_event_time, max_e = et_max
      )
      renorm <- renormalize_dynamic_at(dynamic, ref_e = reference_event_time)
      list(
        renorm = renorm,
        n_treated = n_distinct(data_es$stack_unit_num[data_es$g > 0]),
        n_control = n_distinct(data_es$stack_unit_num[data_es$g == 0]),
        et_max = et_max
      )
    }),
    error = function(e) NULL
  )
}

drop_cohorts <- c(1850L)

results <- list()
for (timing_name in names(panel_by_timing)) {
  panel_t <- panel_by_timing[[timing_name]]
  cohorts <- sort(setdiff(unique(panel_t$g[panel_t$g > 0]), drop_cohorts))
  for (outcome_col in outcomes) {
    for (cohort in cohorts) {
      message("Running per-cohort (max-e) CSDID: ", timing_name, " | ",
              outcome_col, " | g = ", cohort, " (e_max = ",
              analysis_max_decade - cohort, ")")
      res <- run_csdid_one_cohort(panel_t, cohort, outcome_col)
      if (!is.null(res)) {
        results[[length(results) + 1L]] <- res$renorm %>%
          mutate(
            cohort = cohort,
            outcome = outcome_col,
            timing = timing_name,
            n_treated = res$n_treated,
            n_control = res$n_control,
            et_max = res$et_max
          )
      }
    }
  }
}

if (length(results) == 0L) stop("No cohort models succeeded.")

dynamic_att <- bind_rows(results) %>%
  mutate(decade_calendar = cohort + event_time)
write_csv(dynamic_att, results_subdir_path("dynamic_att_per_cohort.csv"), na = "")

status <- dynamic_att %>%
  distinct(outcome, timing, cohort, n_treated, n_control, et_max) %>%
  arrange(outcome, timing, cohort)
write_csv(status, results_subdir_path("status.csv"), na = "")

# Shared axis limits per outcome (identical for standard and alternative timing,
# and fixed across all cohort facets, so cohorts/timings are directly comparable)
lims <- dynamic_att %>%
  group_by(outcome) %>%
  summarise(
    xmin = min(decade_calendar), xmax = max(decade_calendar),
    ymin = min(ci_low, na.rm = TRUE), ymax = max(ci_high, na.rm = TRUE),
    .groups = "drop"
  )

for (o in outcomes) {
  lim_o <- lims[lims$outcome == o, ]
  for (t in names(panel_by_timing)) {
    df_plot <- dynamic_att %>%
      filter(outcome == o, timing == t) %>%
      mutate(
        post = factor(as.integer(event_time >= 0), levels = c(0, 1)),
        cohort_label = paste0("g = ", cohort,
                              " (n_t=", n_treated, ", n_c=", n_control,
                              ", e_max=", et_max, ")")
      )
    if (nrow(df_plot) == 0L) next
    n_cohorts <- n_distinct(df_plot$cohort)
    p <- ggplot(df_plot, aes(x = decade_calendar, y = estimate,
                             ymin = ci_low, ymax = ci_high)) +
      geom_vline(aes(xintercept = cohort), linetype = "dotted",
                 colour = "gray60") +
      geom_point(aes(colour = post), size = 1.2) +
      geom_errorbar(aes(colour = post), width = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~ cohort_label, nrow = 1, scales = "fixed") +
      scale_x_continuous(breaks = seq(1840, 1950, 20)) +
      scale_color_manual(
        drop = FALSE,
        values = c("#e87d72", "#56bcc2"),
        breaks = c(0, 1),
        labels = c("Pre", "Post")
      ) +
      coord_cartesian(
        xlim = c(lim_o$xmin, lim_o$xmax),
        ylim = c(lim_o$ymin, lim_o$ymax)
      ) +
      labs(
        x = "Birth decade", y = "Effect", color = NULL,
        title = str_wrap(
          paste0(outcome_labels[[o]],
                 " - Controlled by baseline births (per-cohort CSDID, ",
                 "calendar axis, ", timing_labels[[t]], ")"),
          80
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
    ggsave(
      results_subdir_path(paste0("ES_", o, "_", t, ".png")),
      p,
      width = 3 * n_cohorts + 2,
      height = 4.5,
      dpi = 300
    )
  }
}

notes <- c(
  "Per-cohort CSDID with each cohort using its MAXIMUM available event-time window.",
  "For cohort g the window is [-20, 1950 - g]: g=1850 -> +100, g=1910 -> +40.",
  "Balance is applied per cohort on calendar decades [g-20, 1950]:",
  "each stack_unit must have non-NA outcome at all those decades and non-NA covariate.",
  "Each cohort estimated on its own subpanel (treated of cohort g + never-treated runner_ups).",
  paste0("Dropped cohorts (too few treated to inform the aggregate): ",
         paste(drop_cohorts, collapse = ", ")),
  paste0("Reference: e = ", reference_event_time, ". Covariate: baseline estimated births."),
  paste0("Generated: ", Sys.Date()), "",
  "Cohorts included per (outcome, timing):",
  capture.output(print(status, n = Inf))
)
writeLines(notes, results_subdir_path("notes.txt"))

cat("wrote results to", results_subdir_path("."), "\n")
print(status, n = Inf)
