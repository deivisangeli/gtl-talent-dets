###############################################################################
# Main AMWS event studies: calendar panel 1830-1950, event years 1850-1920,
# relative window -20 to +90 with partial post-treatment support.
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(did)
  library(dplyr)
  library(fixest)
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
source(file.path(repo_root, "analysis", "land_grants", "amws_twfe_event_study_helpers.R"))

analysis_min_decade <- 1830L
analysis_max_decade <- 1950L
min_event_time <- -20L
max_event_time_raw <- Sys.getenv("MAX_EVENT_TIME", unset = "90")
max_event_time <- suppressWarnings(as.integer(max_event_time_raw))
if (is.na(max_event_time) || max_event_time <= 0) {
  stop("Invalid MAX_EVENT_TIME: ", max_event_time_raw)
}
reference_event_time <- 0L
outcomes <- c("n_amws", "log1p_n_amws", "amws_per_100k", "amws_per_1000_births", "population")

balance_mode_raw <- toupper(Sys.getenv("BALANCE_EVENT_TIME", unset = "FALSE"))
balance_mode <- switch(
  balance_mode_raw,
  "TRUE" = "full",
  "FULL" = "full",
  "PRE" = "pre",
  "none"
)
balance_event_time <- balance_mode != "none"

results_subdir <- "amws_county_pairs_temporal_support_1830_1950"
if (balance_mode == "full") {
  results_subdir <- paste0(results_subdir, "_balanced_event_time")
} else if (balance_mode == "pre") {
  results_subdir <- paste0(results_subdir, "_balanced_pre_treatment")
}
if (max_event_time != 90L) {
  results_subdir <- paste0(results_subdir, "_max", max_event_time)
}

balanced_suffix <- switch(
  balance_mode,
  full = ", balanced event time",
  pre = ", balanced pre-treatment",
  ""
)
csdid_title_prefix <- paste0("CSDID, ref e=0", balanced_suffix)
twfe_title_prefix <- paste0("TWFE", balanced_suffix)

outcome_labels <- c(
  n_amws               = "AMWS scientists born",
  log1p_n_amws         = "log(1 + AMWS scientists born)",
  amws_per_100k        = "AMWS scientists born per 100k population",
  amws_per_1000_births = "AMWS scientists born per 1000 births",
  population           = "County population"
)

timing_labels <- c(
  standard_decade    = "standard decade",
  alternative_decade = "alternative decade"
)
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

run_csdid <- function(data, outcome, timing_name) {
  data_es <- data %>%
    select(stack_unit_num, GEOID, decade, g, event_id, sample_role, all_of(outcome)) %>%
    rename(y = all_of(outcome)) %>%
    filter(!is.na(y), is.finite(y))

  tryCatch({
    att <- did::att_gt(
      yname = "y", tname = "decade", idname = "stack_unit_num", gname = "g",
      data = data_es, control_group = "nevertreated", est_method = "dr",
      base_period = "universal", allow_unbalanced_panel = TRUE, cores = 4
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
      ok = TRUE, outcome = outcome, timing = timing_name, att = att,
      dynamic = dynamic, dynamic_renorm = dynamic_renorm,
      simple = simple, n_rows = nrow(data_es),
      n_units = n_distinct(data_es$stack_unit_num),
      n_treated_units = n_distinct(data_es$stack_unit_num[data_es$sample_role == "treated"]),
      n_control_units = n_distinct(data_es$stack_unit_num[data_es$sample_role == "runner_up"]),
      n_events = n_distinct(data_es$event_id), error = NA_character_
    )
  }, error = function(e) {
    list(ok = FALSE, outcome = outcome, timing = timing_name,
         n_rows = nrow(data_es), n_units = n_distinct(data_es$stack_unit_num),
         n_treated_units = n_distinct(data_es$stack_unit_num[data_es$sample_role == "treated"]),
         n_control_units = n_distinct(data_es$stack_unit_num[data_es$sample_role == "runner_up"]),
         n_events = n_distinct(data_es$event_id), error = conditionMessage(e))
  })
}

models <- list()
for (timing_name in names(panel_by_timing)) {
  for (outcome in outcomes) {
    message("Running CSDID: ", timing_name, " | ", outcome)
    models[[paste(timing_name, outcome, sep = "__")]] <-
      run_csdid(panel_by_timing[[timing_name]], outcome, timing_name)
  }
}

successful <- keep(models, ~ isTRUE(.x$ok))
if (length(successful) == 0L) stop("No CSDID model completed successfully.")

dynamic_att <- imap_dfr(successful, function(x, model_name) {
  x$dynamic_renorm %>%
    mutate(model = model_name, outcome = x$outcome, timing = x$timing) %>%
    select(model, outcome, timing, event_time, estimate, se, ci_low, ci_high)
})

simple_att <- imap_dfr(successful, function(x, model_name) {
  tibble(
    model = model_name, outcome = x$outcome, timing = x$timing,
    estimate = x$simple$overall.att, se = x$simple$overall.se,
    ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se
  )
})

model_status <- imap_dfr(models, function(x, model_name) {
  tibble(
    model = model_name, outcome = x$outcome, timing = x$timing, ok = x$ok,
    n_rows = x$n_rows, n_units = x$n_units, n_events = x$n_events,
    error = x$error
  )
})

write_csv(dynamic_att, results_subdir_path("amws_temporal_support_dynamic_att.csv"), na = "")
write_csv(simple_att, results_subdir_path("amws_temporal_support_simple_att.csv"), na = "")
write_csv(model_status, results_subdir_path("amws_temporal_support_model_status.csv"), na = "")

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
    results_subdir_path(paste0("ES_", x$outcome, "_", x$timing, ".png")),
    p, width = 8, height = 6, dpi = 300
  )
}

twfe <- run_and_export_twfe_event_studies(
  panel_by_timing = panel_by_timing,
  outcomes = outcomes,
  min_event_time = min_event_time,
  max_event_time = max_event_time,
  reference_event_time = reference_event_time,
  results_subdir_path = results_subdir_path,
  title_prefix = twfe_title_prefix,
  outcome_labels = outcome_labels,
  timing_labels = timing_labels
)

support <- map_dfr(names(panel_by_timing), function(timing_name) {
  timing_var <- if (timing_name == "standard_decade") "g_std" else "g_shift"
  event_cohorts <- units %>%
    filter(sample_role == "treated") %>%
    distinct(event_id, treatment_decade = .data[[timing_var]])
  expand_grid(timing = timing_name, event_time = seq(min_event_time, max_event_time, 10L)) %>%
    rowwise() %>%
    mutate(
      n_events = sum(
        event_cohorts$treatment_decade + event_time >= analysis_min_decade &
          event_cohorts$treatment_decade + event_time <= analysis_max_decade
      )
    ) %>%
    ungroup()
})

sample_summary <- map_dfr(names(panel_by_timing), function(timing_name) {
  d <- panel_by_timing[[timing_name]]
  tibble(
    timing = timing_name,
    events = n_distinct(d$event_id),
    treated_units = n_distinct(d$stack_unit_num[d$sample_role == "treated"]),
    control_units = n_distinct(d$stack_unit_num[d$sample_role == "runner_up"]),
    stacked_units = n_distinct(d$stack_unit_num),
    count_outcome_rows = sum(!is.na(d$n_amws)),
    min_decade = min(d$decade), max_decade = max(d$decade)
  )
})

write_csv(support, results_subdir_path("amws_temporal_support_by_event_time.csv"), na = "")
write_csv(sample_summary, results_subdir_path("amws_temporal_support_sample_summary.csv"), na = "")

notes <- c(
  "AMWS land-grants event studies with calendar support 1830-1950.",
  "Event years: 1850-1920. Relative window: -20 to +90.",
  paste0("Dynamic ATT reference period: e = ", reference_event_time,
         " (TWFE via fixest ref=; CSDID via inf-function renormalization)."),
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
  "No complete relative-window requirement; later event times use earlier cohorts only.",
  paste0("Generated: ", Sys.Date()), "", "Sample summary:",
  capture.output(print(sample_summary)), "", "Event-time support:",
  capture.output(print(support, n = Inf)), "", "CSDID status:",
  capture.output(print(model_status, n = Inf)), "", "TWFE status:",
  capture.output(print(twfe$status, n = Inf))
)
writeLines(notes, results_subdir_path("amws_temporal_support_notes.txt"))

cat("wrote results to", results_subdir_path("."), "\n")
print(sample_summary)
print(support, n = Inf)
