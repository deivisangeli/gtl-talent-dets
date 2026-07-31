###############################################################################
# CSDID per-event + agregacao (design analogo ao Andrews 2023 county-pair):
# Cada event roda com apenas seus proprios runner_ups como controle,
# entao ATTs sao agregados across events por inversa-variancia.
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(callr)
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
max_event_time_raw <- Sys.getenv("MAX_EVENT_TIME", unset = "90")
max_event_time <- suppressWarnings(as.integer(max_event_time_raw))
if (is.na(max_event_time) || max_event_time <= 0) {
  stop("Invalid MAX_EVENT_TIME: ", max_event_time_raw)
}
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

balance_mode_raw <- toupper(Sys.getenv("BALANCE_EVENT_TIME", unset = "FALSE"))
balance_mode <- switch(
  balance_mode_raw,
  "TRUE" = "full",
  "FULL" = "full",
  "PRE" = "pre",
  "none"
)
balance_event_time <- balance_mode != "none"

balanced_suffix <- switch(
  balance_mode,
  full = ", balanced event time",
  pre = ", balanced pre-treatment",
  ""
)

results_subdir <- "amws_county_pairs_temporal_support_1830_1950_baseline_pop_per_event"
if (balance_mode == "full") {
  results_subdir <- paste0(results_subdir, "_balanced_event_time")
} else if (balance_mode == "pre") {
  results_subdir <- paste0(results_subdir, "_balanced_pre_treatment")
}
if (max_event_time != 90L) {
  results_subdir <- paste0(results_subdir, "_max", max_event_time)
}
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
    ) %>%
    mutate(
      log_population_baseline = if_else(
        population_baseline > 0, log(population_baseline), NA_real_
      )
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

filter_balanced_event_time <- function(stack_panel,
                                       event_times = seq(min_event_time, max_event_time, 10L)) {
  stack_panel %>%
    mutate(.et = decade - treatment_decade) %>%
    group_by(stack_unit_num) %>%
    mutate(.has_full = all(event_times %in% .et[!is.na(population)])) %>%
    ungroup() %>%
    filter(.has_full) %>%
    group_by(event_id) %>%
    mutate(.has_treated = any(sample_role == "treated")) %>%
    ungroup() %>%
    filter(.has_treated) %>%
    select(-.et, -.has_full, -.has_treated)
}

balance_event_times <- switch(
  balance_mode,
  full = seq(min_event_time, max_event_time, 10L),
  pre = c(-20L, -10L),
  NULL
)
if (!is.null(balance_event_times)) {
  panel_by_timing <- lapply(
    panel_by_timing,
    filter_balanced_event_time,
    event_times = balance_event_times
  )
}

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

run_csdid_one_event <- function(sub, outcome_col) {
  data_es <- sub %>%
    select(stack_unit_num, GEOID, decade, g, event_id, sample_role,
           all_of(outcome_col), all_of(control_var)) %>%
    rename(y = all_of(outcome_col), x = all_of(control_var)) %>%
    filter(!is.na(y), is.finite(y), !is.na(x), is.finite(x))

  if (!any(data_es$g > 0) || !any(data_es$g == 0)) return(NULL)

  tryCatch(
    callr::r(
      function(data_es, min_event_time, max_event_time, reference_event_time) {
        suppressPackageStartupMessages({
          library(did)
          library(dplyr)
          library(tibble)
        })
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
              min_e = min_event_time, max_e = max_event_time
            )
            if_mat <- dynamic$inf.function$dynamic.inf.func.e
            egt <- dynamic$egt
            att_vec <- dynamic$att.egt
            n <- nrow(if_mat)
            col_ref <- which(egt == reference_event_time)
            if (length(col_ref) != 1L) return(NULL)
            if_new <- sweep(if_mat, 1, if_mat[, col_ref], "-")
            att_new <- att_vec - att_vec[col_ref]
            se_new <- sqrt(colMeans(if_new^2) / n)
            tibble::tibble(
              event_time = egt,
              estimate = att_new,
              se = se_new,
              ci_low = att_new - 1.96 * se_new,
              ci_high = att_new + 1.96 * se_new
            )
          }),
          error = function(e) NULL
        )
      },
      args = list(
        data_es = data_es,
        min_event_time = min_event_time,
        max_event_time = max_event_time,
        reference_event_time = reference_event_time
      ),
      show = FALSE,
      timeout = 60
    ),
    error = function(e) {
      message("  Subprocess failed: ", conditionMessage(e))
      NULL
    }
  )
}

per_event <- list()
for (timing_name in names(panel_by_timing)) {
  panel_t <- panel_by_timing[[timing_name]]
  for (outcome_col in outcomes) {
    for (eid in unique(panel_t$event_id)) {
      sub <- panel_t[panel_t$event_id == eid, ]
      message("Running per-event CSDID: ", timing_name, " | ", outcome_col, " | event ", eid)
      res <- run_csdid_one_event(sub, outcome_col)
      if (!is.null(res)) {
        per_event[[length(per_event) + 1L]] <- res %>%
          mutate(event_id = eid, outcome = outcome_col, timing = timing_name)
      }
    }
  }
}

if (length(per_event) == 0L) {
  stop("No per-event models succeeded.")
}

per_event_df <- bind_rows(per_event)

aggregate_across_events <- function(df) {
  df %>%
    filter(is.finite(estimate)) %>%
    group_by(outcome, timing, event_time) %>%
    summarise(
      mean_att = mean(estimate, na.rm = TRUE),
      sd_att = sd(estimate, na.rm = TRUE),
      n_events = n(),
      .groups = "drop"
    ) %>%
    mutate(
      estimate = mean_att,
      se = sd_att / sqrt(n_events),
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se
    ) %>%
    select(outcome, timing, event_time, estimate, se, ci_low, ci_high, n_events)
}

anchor <- per_event_df %>%
  filter(event_time == 0L) %>%
  distinct(outcome, timing) %>%
  mutate(event_time = 0L, estimate = 0, se = 0, ci_low = 0, ci_high = 0,
         n_events = per_event_df %>%
           filter(event_time == 0L) %>%
           count(outcome, timing) %>%
           right_join(distinct(., outcome, timing), by = c("outcome", "timing")) %>%
           pull(n))

dynamic_att <- per_event_df %>%
  filter(event_time != 0L) %>%
  aggregate_across_events() %>%
  bind_rows(
    per_event_df %>%
      filter(event_time == 0L) %>%
      group_by(outcome, timing, event_time) %>%
      summarise(estimate = 0, se = 0, ci_low = 0, ci_high = 0, n_events = n(),
                .groups = "drop")
  ) %>%
  arrange(outcome, timing, event_time)

write_csv(dynamic_att, results_subdir_path("dynamic_att.csv"), na = "")
write_csv(per_event_df, results_subdir_path("per_event_dynamic_att.csv"), na = "")

status <- per_event_df %>%
  count(outcome, timing, name = "n_events_included") %>%
  arrange(outcome, timing)
write_csv(status, results_subdir_path("status.csv"), na = "")

for (o in outcomes) {
  for (t in names(panel_by_timing)) {
    df_plot <- dynamic_att %>%
      filter(outcome == o, timing == t) %>%
      mutate(post = factor(as.integer(event_time >= 0), levels = c(0, 1)))
    if (nrow(df_plot) == 0L) next
    n_events_agg <- max(df_plot$n_events, na.rm = TRUE)

    p <- ggplot(df_plot, aes(x = event_time, y = estimate,
                             ymin = ci_low, ymax = ci_high)) +
      geom_point(aes(colour = post), size = 1.5) +
      geom_errorbar(aes(colour = post), width = 0.1) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_x_continuous(breaks = sort(unique(df_plot$event_time))) +
      scale_color_manual(
        drop = FALSE,
        values = c("#e87d72", "#56bcc2"),
        breaks = c(0, 1),
        labels = c("Pre", "Post")
      ) +
      labs(
        x = "Relative Time", y = "Effect", color = NULL,
        title = str_wrap(
          paste0(outcome_labels[[o]],
                 " - Controlled by baseline births (per-event CSDID, ref e=0",
                 balanced_suffix, ", ", timing_labels[[t]], ")"),
          72
        )
      ) +
      coord_cartesian(xlim = c(min_event_time, max_event_time)) +
      theme_classic() +
      theme(
        plot.title = element_text(color = "darkgray", face = "bold", size = 12),
        axis.title = element_text(color = "darkgray", face = "bold", size = 12),
        legend.position = "bottom"
      ) +
      annotate(
        "label", x = Inf, y = Inf, hjust = 1.05, vjust = 1.05,
        label = sprintf("Events aggregated: %d\n(mean, between-event SE)",
                        n_events_agg),
        size = 3, label.padding = unit(0.4, "lines"),
        label.r = unit(0, "lines")
      )
    ggsave(
      results_subdir_path(paste0("ES_", o, "_", t, ".png")),
      p, width = 8, height = 6, dpi = 300
    )
  }
}

notes <- c(
  "Per-event CS-DID with cross-event inverse-variance aggregation.",
  "Each event's runner_ups serve only as controls for their own treated county",
  "(no cross-event control pooling), analogous to Andrews (2023) county-pair design.",
  paste0("Balance mode: ", balance_mode, ". Event-time window: [", min_event_time,
         ", ", max_event_time, "]."),
  paste0("Reference: e = ", reference_event_time, ". Covariate: baseline estimated births."),
  paste0("Generated: ", Sys.Date()), "",
  "Events included per (outcome, timing):",
  capture.output(print(status, n = Inf))
)
writeLines(notes, results_subdir_path("notes.txt"))

cat("wrote results to", results_subdir_path("."), "\n")
print(status)
