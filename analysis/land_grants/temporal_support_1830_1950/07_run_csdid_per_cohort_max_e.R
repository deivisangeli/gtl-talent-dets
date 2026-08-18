###############################################################################
# CSDID event studies per cohort, estimated on EXACTLY the sample used by the
# pooled event studies in 03_run_baseline_population_controls.R.
#
# The sample (balance rule, cohort filter, event-retention rule) is built by the
# shared helpers in _sample_helpers.R and is then split by treatment cohort, so
# the treated counts in the cohort facets sum to the pooled "Events" count. The
# defaults reproduce the main-text pooled specification
# (BALANCE_EVENT_TIME = TRUE, e in [-20, +70]); the same env vars as script 03
# override them.
#
# Within that fixed sample each cohort is still followed as far as the panel
# supports it: et_max(g) is the largest event time at which every retained unit
# in the cohort subpanel still has an observation. Units are never dropped to
# extend the horizon. Facet by cohort on a relative-event-time axis, shared across
# facets and both timings, with every estimated relative year ticked.
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
source(file.path(dirname(script_path), "_sample_helpers.R"))

# Defaults are the main-text pooled specification, so a bare run of this script
# decomposes the figures produced by
#   BALANCE_EVENT_TIME=TRUE MAX_EVENT_TIME=70 Rscript 03_...R
min_event_time_raw <- Sys.getenv("MIN_EVENT_TIME", unset = "-20")
min_event_time <- suppressWarnings(as.integer(min_event_time_raw))
if (is.na(min_event_time) || min_event_time >= 0) {
  stop("Invalid MIN_EVENT_TIME (must be negative integer): ", min_event_time_raw)
}
max_event_time_raw <- Sys.getenv("MAX_EVENT_TIME", unset = "70")
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

balance_mode_raw <- toupper(Sys.getenv("BALANCE_EVENT_TIME", unset = "TRUE"))
balance_mode <- switch(
  balance_mode_raw,
  "TRUE" = "full",
  "FULL" = "full",
  "PRE" = "pre",
  "none"
)

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

# The unsuffixed directory holds the manuscript specification. Any override of
# the sample definition writes elsewhere so the two cannot be confused.
results_subdir <- "amws_county_pairs_temporal_support_1830_1950_baseline_pop_per_cohort_max_e"
if (balance_mode != "full") {
  results_subdir <- paste0(results_subdir, "_bal", tolower(balance_mode))
}
if (max_event_time != 70L) {
  results_subdir <- paste0(results_subdir, "_max", max_event_time)
}
if (min_event_time != -20L) {
  results_subdir <- paste0(results_subdir, "_min", abs(min_event_time))
}
if (!is.null(cohorts_filter)) {
  results_subdir <- paste0(results_subdir, "_cohorts_",
                           paste(sort(cohorts_filter), collapse = "_"))
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

# Longest horizon this cohort supports: every treated county of the cohort must
# still be observed, and at least one never-treated runner-up must be observed,
# at each event time from 0 up to the returned value. Controls inherited from
# other cohorts' events are allowed to be unbalanced at the extremes, exactly as
# in the pooled model (allow_unbalanced_panel = TRUE); no unit is ever dropped
# to extend the horizon.
supported_event_time_max <- function(data_sub, cohort) {
  n_treated <- n_distinct(data_sub$stack_unit_num[data_sub$g > 0])
  decades_ok <- data_sub %>%
    group_by(decade) %>%
    summarise(
      n_treated_present = n_distinct(stack_unit_num[g > 0]),
      n_control_present = n_distinct(stack_unit_num[g == 0]),
      .groups = "drop"
    ) %>%
    filter(n_treated_present == n_treated, n_control_present > 0L) %>%
    pull(decade)

  et <- NA_integer_
  for (e in seq(reference_event_time, max(data_sub$decade) - cohort, 10L)) {
    if (!((cohort + e) %in% decades_ok)) break
    et <- as.integer(e)
  }
  et
}

run_csdid_one_cohort <- function(data_fixed, cohort) {
  # Treated counties of this cohort plus every never-treated runner-up of the
  # retained events -- the same control pool the pooled model uses.
  data_es <- data_fixed %>% filter(g == cohort | g == 0)
  if (!any(data_es$g > 0) || !any(data_es$g == 0)) return(NULL)

  et_max <- supported_event_time_max(data_es, cohort)
  if (is.na(et_max)) return(NULL)

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
        event_ids = sort(unique(data_es$event_id[data_es$g > 0])),
        et_max = et_max
      )
    }),
    error = function(e) NULL
  )
}

results <- list()
event_audit <- list()
for (timing_name in names(panel_by_timing)) {
  for (outcome_col in outcomes) {
    # Identical to the pooled sample for this (timing, outcome) model.
    data_fixed <- prepare_event_study_sample(
      panel_by_timing[[timing_name]], outcome_col, control_var
    )
    pooled_events <- sort(unique(data_fixed$event_id))
    cohorts <- sort(unique(data_fixed$g[data_fixed$g > 0]))

    for (cohort in cohorts) {
      message("Running per-cohort CSDID: ", timing_name, " | ",
              outcome_col, " | g = ", cohort)
      res <- run_csdid_one_cohort(data_fixed, cohort)
      if (is.null(res)) {
        stop("Cohort model failed: ", timing_name, " | ", outcome_col,
             " | g = ", cohort,
             " — the facets would no longer sum to the pooled event count.")
      }
      results[[length(results) + 1L]] <- res$renorm %>%
        mutate(
          cohort = cohort,
          outcome = outcome_col,
          timing = timing_name,
          n_treated = res$n_treated,
          n_control = res$n_control,
          et_max = res$et_max
        )
      event_audit[[length(event_audit) + 1L]] <- tibble(
        timing = timing_name,
        outcome = outcome_col,
        cohort = cohort,
        n_treated = res$n_treated,
        n_control = res$n_control,
        et_max = res$et_max,
        event_ids = paste(res$event_ids, collapse = "|")
      )
    }

    # Hard guard: the per-cohort facets must decompose the pooled sample.
    facet_events <- event_audit %>%
      bind_rows() %>%
      filter(timing == timing_name, outcome == outcome_col) %>%
      pull(event_ids) %>%
      strsplit("\\|") %>%
      unlist() %>%
      as.integer() %>%
      sort()
    if (!identical(facet_events, as.integer(pooled_events))) {
      stop("Per-cohort events do not match the pooled sample for ",
           timing_name, " | ", outcome_col, ": ",
           length(facet_events), " vs ", length(pooled_events), ".")
    }
  }
}

if (length(results) == 0L) stop("No cohort models succeeded.")

dynamic_att <- bind_rows(results) %>%
  mutate(decade_calendar = cohort + event_time)
write_csv(dynamic_att, results_subdir_path("dynamic_att_per_cohort.csv"), na = "")

events_by_cohort <- bind_rows(event_audit) %>%
  arrange(timing, outcome, cohort)
write_csv(events_by_cohort, results_subdir_path("events_by_cohort.csv"), na = "")

status <- dynamic_att %>%
  distinct(outcome, timing, cohort, n_treated, n_control, et_max) %>%
  arrange(outcome, timing, cohort)
write_csv(status, results_subdir_path("status.csv"), na = "")

pooled_totals <- events_by_cohort %>%
  group_by(timing, outcome) %>%
  summarise(n_events = sum(n_treated), .groups = "drop")

# Shared axis limits per outcome (identical for standard and alternative timing,
# and fixed across all cohort facets, so cohorts/timings are directly comparable).
# The x axis is relative event time, so e = +50 sits at the same position in every
# facet; a cohort that ends at its own et_max simply leaves the right tail blank.
lims <- dynamic_att %>%
  group_by(outcome) %>%
  summarise(
    xmin = min(event_time), xmax = max(event_time),
    ymin = min(ci_low, na.rm = TRUE), ymax = max(ci_high, na.rm = TRUE),
    .groups = "drop"
  )

# Every estimated relative year gets a tick, on every facet of both timing rows.
# Taken over the whole result set rather than per timing, so the standard row
# (which stops at +90) still labels the +100 edge and both rows share one grid.
x_breaks <- sort(unique(dynamic_att$event_time))

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
    n_events_total <- pooled_totals %>%
      filter(timing == t, outcome == o) %>%
      pull(n_events)
    p <- ggplot(df_plot, aes(x = event_time, y = estimate,
                             ymin = ci_low, ymax = ci_high)) +
      geom_vline(xintercept = 0, linetype = "dotted", colour = "gray60") +
      geom_point(aes(colour = post), size = 1.2) +
      geom_errorbar(aes(colour = post), width = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~ cohort_label, nrow = 1, scales = "fixed") +
      scale_x_continuous(breaks = x_breaks) +
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
        x = "Relative Time", y = "Effect", color = NULL,
        title = str_wrap(
          paste0(outcome_labels[[o]],
                 " - Controlled by baseline births (per-cohort CSDID, ",
                 "relative time, ", timing_labels[[t]], ")"),
          80
        ),
        subtitle = paste0("Events: ", n_events_total,
                          " (same sample as the pooled event study)")
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(color = "darkgray", face = "bold", size = 11),
        plot.subtitle = element_text(color = "darkgray", size = 9),
        axis.title = element_text(color = "darkgray", face = "bold", size = 11),
        # 13 ticks per facet: shrink the labels so they do not overplot
        axis.text.x = element_text(size = 8),
        strip.text = element_text(color = "darkgray", face = "bold", size = 9),
        strip.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom"
      )
    ggsave(
      results_subdir_path(paste0("ES_", o, "_", t, ".png")),
      p,
      width = 3.5 * n_cohorts + 2,
      height = 4.5,
      dpi = 300
    )
  }
}

notes <- c(
  "Per-cohort CSDID estimated on exactly the pooled event-study sample.",
  paste0("Sample rules (shared with 03_run_baseline_population_controls.R): ",
         "BALANCE_EVENT_TIME = ", balance_mode_raw, " (mode = ", balance_mode, "), ",
         "event-time window [", min_event_time, ", ", max_event_time, "], ",
         "cohort restriction: ",
         if (is.null(cohorts_filter)) "all cohorts" else paste(cohorts_filter, collapse = ", "),
         "."),
  paste0("A stack unit is kept only if it has non-NA population at every event ",
         "time in the window relative to its own event; an event is kept only ",
         "if both its treated county and one of its own runner-ups survive."),
  paste0("Each cohort is then estimated on its own subpanel (treated of cohort ",
         "g + all never-treated runner-ups of the retained events)."),
  paste0("et_max(g) is the longest horizon every retained unit in the cohort ",
         "subpanel supports; no unit is dropped to extend it."),
  paste0("Reference: e = ", reference_event_time, ". Covariate: baseline estimated births."),
  "Treated counts across facets sum to the pooled event count, by construction",
  "and enforced by a hard check.",
  paste0("Generated: ", Sys.Date()), "",
  "Events per (timing, outcome):",
  capture.output(print(pooled_totals, n = Inf)), "",
  "Cohorts included per (outcome, timing):",
  capture.output(print(status, n = Inf))
)
writeLines(notes, results_subdir_path("notes.txt"))

cat("wrote results to", results_subdir_path("."), "\n")
print(pooled_totals, n = Inf)
print(status, n = Inf)
