###############################################################################
# CSDID event study for county_births_estimate (no covariate),
# using the exact same sample as Figures 1-2 of the paper
# (PRE balance filter + retained filter, matching script 03).
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
max_event_time <- 90L
reference_event_time <- 0L
outcome <- "county_births_estimate"
outcome_label <- "County births estimate"

timing_labels <- c(
  standard_decade    = "standard decade",
  alternative_decade = "alternative decade"
)

results_subdir <- "amws_county_pairs_temporal_support_1830_1950_births_no_covariate_balanced_pre_treatment"
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

units <- read_csv(
  output_file_path("land_grants", "andrews_event_county_units_1850_1920.csv"),
  show_col_types = FALSE
) %>%
  mutate(GEOID = str_pad(as.character(as.integer(GEOID)), 5, pad = "0"))

build_stack <- function(timing_var, timing_name) {
  units %>%
    mutate(
      treatment_decade = .data[[timing_var]],
      g = if_else(sample_role == "treated", treatment_decade, 0L),
      timing = timing_name,
      stack_unit_id = paste(event_id, GEOID, sample_role, sep = "_"),
      stack_unit_num = dense_rank(stack_unit_id)
    ) %>%
    inner_join(panel, by = "GEOID") %>%
    arrange(event_id, sample_role, GEOID, decade)
}

panel_by_timing <- list(
  standard_decade = build_stack("g_std", "standard_decade"),
  alternative_decade = build_stack("g_shift", "alternative_decade")
)

filter_balanced_event_time <- function(stack_panel,
                                       event_times = c(-20L, -10L)) {
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

panel_by_timing <- lapply(panel_by_timing, filter_balanced_event_time)

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

run_csdid_no_cov <- function(data, timing_name) {
  data_es <- data %>%
    select(stack_unit_num, GEOID, decade, g, event_id, sample_role, all_of(outcome)) %>%
    rename(y = all_of(outcome)) %>%
    filter(!is.na(y), is.finite(y))

  retained <- data_es %>%
    group_by(event_id) %>%
    summarise(
      has_treated = any(g > 0),
      has_control = any(g == 0),
      .groups = "drop"
    ) %>%
    filter(has_treated, has_control) %>%
    select(event_id)
  data_es <- semi_join(data_es, retained, by = "event_id")

  tryCatch({
    att <- did::att_gt(
      yname = "y", tname = "decade", idname = "stack_unit_num", gname = "g",
      data = data_es, control_group = "nevertreated", est_method = "dr",
      base_period = "universal", allow_unbalanced_panel = TRUE, cores = 1
    )
    dynamic <- did::aggte(
      att, type = "dynamic", na.rm = TRUE,
      min_e = min_event_time, max_e = max_event_time
    )
    dynamic_renorm <- renormalize_dynamic_at(dynamic, ref_e = reference_event_time)
    list(
      ok = TRUE, outcome = outcome, timing = timing_name, att = att,
      dynamic = dynamic, dynamic_renorm = dynamic_renorm,
      n_rows = nrow(data_es),
      n_units = n_distinct(data_es$stack_unit_num),
      n_treated_units = n_distinct(data_es$stack_unit_num[data_es$sample_role == "treated"]),
      n_control_units = n_distinct(data_es$stack_unit_num[data_es$sample_role == "runner_up"]),
      n_events = n_distinct(data_es$event_id),
      error = NA_character_
    )
  }, error = function(e) {
    list(ok = FALSE, outcome = outcome, timing = timing_name,
         n_rows = nrow(data_es), error = conditionMessage(e))
  })
}

models <- list()
for (timing_name in names(panel_by_timing)) {
  message("Running CSDID (no covariate): ", timing_name, " | ", outcome)
  models[[timing_name]] <- run_csdid_no_cov(panel_by_timing[[timing_name]], timing_name)
}

successful <- keep(models, ~ isTRUE(.x$ok))
if (length(successful) == 0L) stop("No model completed successfully.")

dynamic_att <- imap_dfr(successful, function(x, model_name) {
  x$dynamic_renorm %>%
    mutate(model = model_name, outcome = x$outcome, timing = x$timing) %>%
    select(model, outcome, timing, event_time, estimate, se, ci_low, ci_high)
})

status <- imap_dfr(models, function(x, model_name) {
  tibble(
    model = model_name, outcome = x$outcome, timing = x$timing, ok = x$ok,
    n_rows = x$n_rows,
    n_units = if (isTRUE(x$ok)) x$n_units else NA_integer_,
    n_treated_units = if (isTRUE(x$ok)) x$n_treated_units else NA_integer_,
    n_control_units = if (isTRUE(x$ok)) x$n_control_units else NA_integer_,
    n_events = if (isTRUE(x$ok)) x$n_events else NA_integer_,
    error = x$error
  )
})

write_csv(dynamic_att, results_subdir_path("dynamic_att.csv"), na = "")
write_csv(status, results_subdir_path("model_status.csv"), na = "")

for (x in successful) {
  df <- x$dynamic_renorm %>%
    mutate(post = factor(as.integer(event_time >= 0), levels = c(0, 1)))
  p <- ggplot(df, aes(x = event_time, y = estimate, ymin = ci_low, ymax = ci_high)) +
    geom_point(aes(colour = post), size = 1.5) +
    geom_errorbar(aes(colour = post), width = 0.1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(breaks = sort(unique(df$event_time))) +
    scale_color_manual(
      drop = FALSE,
      values = c("#e87d72", "#56bcc2"),
      breaks = c(0, 1),
      labels = c("Pre", "Post")
    ) +
    labs(
      x = "Relative Time", y = "Effect", color = NULL,
      title = str_wrap(
        paste0(
          outcome_label,
          " (CSDID, ref e=0, balanced pre-treatment, no covariate, ",
          timing_labels[[x$timing]], ")"
        ),
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
      label = sprintf("Events: %d\nTreated units: %d\nControl units: %d",
                      x$n_events, x$n_treated_units, x$n_control_units),
      size = 3, label.padding = unit(0.4, "lines"),
      label.r = unit(0, "lines")
    )
  ggsave(
    results_subdir_path(paste0("ES_", outcome, "_", x$timing, ".png")),
    p, width = 8, height = 6, dpi = 300
  )
}

notes <- c(
  "AMWS land-grants event study: county_births_estimate outcome, no covariate.",
  "Same sample as Figures 1-2 of paper (PRE balance filter + retained filter).",
  "Event years: 1850-1920. Relative window: -20 to +90. Reference: e = 0.",
  "Outcome: county_births_estimate = population x US annual birth rate (Gapminder), summed by decade.",
  paste0("Generated: ", Sys.Date()), "", "Model status:",
  capture.output(print(status, n = Inf))
)
writeLines(notes, results_subdir_path("notes.txt"))

cat("wrote results to", results_subdir_path("."), "\n")
print(status)
