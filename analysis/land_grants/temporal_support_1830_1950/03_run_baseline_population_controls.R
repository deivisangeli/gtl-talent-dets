###############################################################################
# AMWS CSDID event studies with baseline population controls.
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(did)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "prep", "raw_paths.R"))
# Sample construction is shared with 07_run_csdid_per_cohort_max_e.R so the
# per-cohort figures decompose exactly these events.
source(file.path(dirname(script_path), "_sample_helpers.R"))

min_event_time_raw <- Sys.getenv("MIN_EVENT_TIME", unset = "-20")
min_event_time <- suppressWarnings(as.integer(min_event_time_raw))
if (is.na(min_event_time) || min_event_time >= 0) {
  stop("Invalid MIN_EVENT_TIME (must be negative integer): ", min_event_time_raw)
}
max_event_time_raw <- Sys.getenv("MAX_EVENT_TIME", unset = "90")
max_event_time <- suppressWarnings(as.integer(max_event_time_raw))
if (is.na(max_event_time) || max_event_time <= 0) {
  stop("Invalid MAX_EVENT_TIME: ", max_event_time_raw)
}
reference_event_time <- 0L

cohorts_raw <- Sys.getenv("COHORTS", unset = "")
cohorts_filter <- if (nzchar(cohorts_raw)) {
  suppressWarnings(as.integer(unlist(strsplit(cohorts_raw, "[,\\s]+"))))
} else {
  NULL
}
if (!is.null(cohorts_filter) && any(is.na(cohorts_filter))) {
  stop("Invalid COHORTS: ", cohorts_raw)
}

outcomes <- c("n_amws", "log1p_n_amws", "amws_per_100k", "amws_per_1000_births",
              "county_births_estimate")
control_specs <- c(
  population_level = "population_baseline",
  population_log   = "log_population_baseline",
  births_level     = "births_baseline"
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
csdid_title_prefix <- paste0("CSDID, ref e=0", balanced_suffix)

outcome_labels <- c(
  n_amws                 = "AMWS scientists born",
  log1p_n_amws           = "log(1 + AMWS scientists born)",
  amws_per_100k          = "AMWS scientists born per 100k population",
  amws_per_1000_births   = "AMWS scientists born per 1000 births",
  county_births_estimate = "County births estimate"
)

control_labels <- c(
  population_level = "Controlled by baseline population",
  population_log   = "Controlled by log baseline population",
  births_level     = "Controlled by baseline births"
)

timing_labels <- c(
  standard_decade    = "standard decade",
  alternative_decade = "alternative decade"
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

results_subdir <- "amws_county_pairs_temporal_support_1830_1950_baseline_pop"
if (balance_mode == "full") {
  results_subdir <- paste0(results_subdir, "_balanced_event_time")
} else if (balance_mode == "pre") {
  results_subdir <- paste0(results_subdir, "_balanced_pre_treatment")
}
if (max_event_time != 90L) {
  results_subdir <- paste0(results_subdir, "_max", max_event_time)
}
if (min_event_time != -20L) {
  results_subdir <- paste0(results_subdir, "_min", abs(min_event_time))
}
if (!is.null(cohorts_filter)) {
  cohorts_tag <- paste(sort(cohorts_filter), collapse = "_")
  results_subdir <- paste0(results_subdir, "_cohorts_", cohorts_tag)
}
results_subdir_path <- function(...) {
  path <- results_file_path("land_grants", "event_studies", results_subdir, ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}

inputs <- read_temporal_support_inputs()

panel_by_timing <- list(
  standard_decade = build_stack(inputs$units, inputs$panel,
                                "g_std", "standard_decade"),
  alternative_decade = build_stack(inputs$units, inputs$panel_alt,
                                   "g_shift", "alternative_decade")
)

panel_by_timing <- lapply(panel_by_timing, filter_cohorts, cohorts_filter = cohorts_filter)

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

run_controlled <- function(data, outcome, control, spec, timing_name) {
  data_es <- prepare_event_study_sample(data, outcome, control)
  tryCatch({
    att <- did::att_gt(
      yname = "y", tname = "decade", idname = "stack_unit_num", gname = "g",
      xformla = ~ x, data = data_es, control_group = "nevertreated",
      est_method = "reg", base_period = "universal",
      allow_unbalanced_panel = TRUE, cores = 4
    )
    dynamic <- did::aggte(
      att, type = "dynamic", na.rm = TRUE,
      min_e = min_event_time, max_e = max_event_time
    )
    simple <- did::aggte(
      att, type = "simple", na.rm = TRUE, min_e = 0, max_e = max_event_time
    )
    dynamic_renorm <- renormalize_dynamic_at(dynamic, ref_e = reference_event_time)
    list(
      ok = TRUE, outcome = outcome, control = control, spec = spec,
      timing = timing_name, att = att, dynamic = dynamic,
      dynamic_renorm = dynamic_renorm, simple = simple,
      n_rows = nrow(data_es), n_units = n_distinct(data_es$stack_unit_num),
      n_treated_units = n_distinct(data_es$stack_unit_num[data_es$sample_role == "treated"]),
      n_control_units = n_distinct(data_es$stack_unit_num[data_es$sample_role == "runner_up"]),
      n_events = n_distinct(data_es$event_id), error = NA_character_
    )
  }, error = function(e) {
    list(
      ok = FALSE, outcome = outcome, control = control, spec = spec,
      timing = timing_name, n_rows = nrow(data_es),
      n_units = n_distinct(data_es$stack_unit_num),
      n_treated_units = n_distinct(data_es$stack_unit_num[data_es$sample_role == "treated"]),
      n_control_units = n_distinct(data_es$stack_unit_num[data_es$sample_role == "runner_up"]),
      n_events = n_distinct(data_es$event_id), error = conditionMessage(e)
    )
  })
}

models <- list()
for (timing_name in names(panel_by_timing)) {
  for (spec in names(control_specs)) {
    control <- unname(control_specs[[spec]])
    for (outcome in outcomes) {
      message("Running controlled CSDID: ", timing_name, " | ", spec, " | ", outcome)
      key <- paste(timing_name, spec, outcome, sep = "__")
      models[[key]] <- run_controlled(
        panel_by_timing[[timing_name]], outcome, control, spec, timing_name
      )
    }
  }
}

successful <- keep(models, ~ isTRUE(.x$ok))
if (length(successful) == 0L) stop("No controlled model completed successfully.")

dynamic_att <- imap_dfr(successful, function(x, model_name) {
  x$dynamic_renorm %>%
    mutate(model = model_name, outcome = x$outcome, timing = x$timing,
           spec = x$spec, control = x$control) %>%
    select(model, outcome, timing, spec, control,
           event_time, estimate, se, ci_low, ci_high)
})

simple_att <- imap_dfr(successful, function(x, model_name) {
  tibble(
    model = model_name, outcome = x$outcome, timing = x$timing,
    spec = x$spec, control = x$control,
    estimate = x$simple$overall.att, se = x$simple$overall.se,
    ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se
  )
})

status <- imap_dfr(models, function(x, model_name) {
  tibble(
    model = model_name, outcome = x$outcome, timing = x$timing,
    spec = x$spec, control = x$control, ok = x$ok,
    n_rows = x$n_rows, n_units = x$n_units, n_events = x$n_events,
    error = x$error
  )
})

baseline_audit <- map_dfr(names(panel_by_timing), function(timing_name) {
  panel_by_timing[[timing_name]] %>%
    distinct(stack_unit_num, event_id, sample_role, GEOID, control_decade,
             population_baseline, log_population_baseline,
             baseline_population_source) %>%
    mutate(timing = timing_name) %>%
    arrange(event_id, sample_role, GEOID)
})

write_csv(dynamic_att, results_subdir_path("baseline_pop_dynamic_att.csv"), na = "")
write_csv(simple_att, results_subdir_path("baseline_pop_simple_att.csv"), na = "")
write_csv(status, results_subdir_path("baseline_pop_model_status.csv"), na = "")
write_csv(baseline_audit, results_subdir_path("baseline_population_audit.csv"), na = "")

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
          outcome_labels[[x$outcome]],
          " — ", control_labels[[x$spec]],
          " (", csdid_title_prefix, ", ", timing_labels[[x$timing]], ")"
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
    results_subdir_path(paste0("ES_", x$outcome, "_", x$spec, "_", x$timing, ".png")),
    p, width = 8, height = 6, dpi = 300
  )
}

notes <- c(
  "AMWS land-grants event studies with baseline population controls.",
  "Population baseline is measured at treatment decade - 10.",
  "Population is NHGIS/manual observed or interpolated between valid NHGIS/manual knots.",
  "Relative window: -20 to +90; partial post-treatment support is allowed.",
  paste0("Dynamic ATT reference period: e = ", reference_event_time,
         " (renormalized via CSDID influence functions)."),
  "Simple ATT keeps default CS-DID reference (g-1) since it is a post-treatment average.",
  paste0("Event-time window: [", min_event_time, ", ", max_event_time,
         "] (max override via MAX_EVENT_TIME env var)."),
  paste0("Sample restriction (BALANCE_EVENT_TIME = ", balance_mode_raw,
         ", mode = ", balance_mode, "): ",
         switch(balance_mode,
                full = paste0("keep stack_units with non-NA population at all event_times in [",
                              min_event_time, ", ", max_event_time, "]."),
                pre = "keep stack_units with non-NA population at event_times -20 and -10.",
                "no filter (all events).")),
  paste0("Cohort restriction (COHORTS env var): ",
         if (is.null(cohorts_filter)) "all cohorts" else paste(cohorts_filter, collapse = ", ")),
  paste0("Generated: ", Sys.Date()), "", "Model status:",
  capture.output(print(status, n = Inf))
)
writeLines(notes, results_subdir_path("baseline_pop_notes.txt"))

cat("wrote controlled results to", results_subdir_path("."), "\n")
print(status, n = Inf)
