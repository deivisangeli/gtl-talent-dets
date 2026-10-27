# Fixed-site catchment outcomes on the frozen manuscript CSDID observations.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo, "prep", "land_grants", "amws_training_helpers.R"))
suppressPackageStartupMessages({
  library(did)
  library(dplyr)
  library(readr)
  library(ggplot2)
})
source(file.path(repo, "prep", "land_grants", "amws_catchment_helpers.R"))
cfg <- catchment_config(repo, training_root())
if (dir.exists(cfg$results)) {
  training_check_release(cfg$results)
  cat("Event-study release already complete and verified:", cfg$results, "\n")
  quit(status = 0)
}
prep_manifest <- training_check_release(cfg$output)
started <- proc.time()[3]
stage <- file.path(cfg$local, "results_stage")
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(stage, "models"), showWarnings = FALSE)
Sys.setenv(DET_DIR = cfg$root)
source(file.path(repo, "prep", "raw_paths.R"))

prepared_panels <- c(standard_decade = file.path(cfg$output, "catchment_decade_standard_decade.csv"),
                     alternative_decade = file.path(cfg$output, "catchment_decade_alternative_decade.csv"))
sources <- c(cfg$units, prepared_panels, file.path(cfg$output, c("completion_manifest.json", "analysis_coverage_summary.csv")),
  file.path(cfg$previous_results, c("completion_manifest.json", "dynamic_att.csv", "frozen_sample_standard_decade.csv", "frozen_sample_alternative_decade.csv")),
  script, file.path(repo, "prep", "land_grants", c("amws_training_helpers.R", "amws_catchment_helpers.R")))
inputs <- training_hashes(unique(unname(sources)))
invisible(training_check_release(cfg$previous_results))
coverage <- fread(file.path(cfg$output, "analysis_coverage_summary.csv"))
catchment_tests()
outcomes <- training_rates
labels <- c(ever_nearby = "Ever within 50 km", never_nearby = "Never within 50 km", unknown = "Unknown training", total = "Updated total")
expected <- list(standard_decade = c(rows = 650L, units = 52L, events = 16L),
                 alternative_decade = c(rows = 834L, units = 62L, events = 20L))
models <- list()
membership <- list()
freeze_audit <- list()
sample_coverage <- list()
warnings_log <- list()
model_index <- 0L
baseline_comparison <- list()
for (timing in names(prepared_panels)) {
  frozen <- read_csv(file.path(cfg$previous_results, paste0("frozen_sample_", timing, ".csv")),
                     col_types = cols(GEOID = col_character()), show_col_types = FALSE)
  frozen_keys <- frozen %>% select(-x)
  panel <- read_csv(prepared_panels[[timing]], col_types = cols(GEOID = col_character()), show_col_types = FALSE)
  training_require(identical(as.data.frame(panel %>% select(all_of(names(frozen_keys)))), as.data.frame(frozen_keys)),
                   "Pooled panel changes frozen sample identifiers or ordering")
  e <- expected[[timing]]
  training_require(nrow(panel) == e['rows'] && n_distinct(panel$stack_unit_num) == e['units'] && n_distinct(panel$event_id) == e['events'],
                   "Frozen manuscript sample dimensions differ")
  training_require(identical(panel$county_only_births_baseline, frozen$x), "Original baseline audit differs")
  freeze_audit[[timing]] <- data.table(timing = timing, rows = nrow(panel), stack_units = n_distinct(panel$stack_unit_num),
    events = n_distinct(panel$event_id), event_ids = paste(sort(unique(panel$event_id)), collapse = "|"), key_comparison = "exact",
    baseline_control = "Pooled catchment estimated births at g-10")
  fwrite(as.data.table(frozen), file.path(stage, paste0("frozen_sample_", timing, ".csv")))
  baseline_comparison[[timing]] <- unique(as.data.table(panel)[, .(timing = timing, site_id, stack_unit_num, event_id, sample_role,
    county_only_births_baseline, catchment_births_baseline = births_baseline)])
  sample_coverage[[timing]] <- fread(file.path(cfg$output, "matching_coverage_by_edition_cohort_role.csv"))
  for (group in names(outcomes)) {
    outcome <- outcomes[[group]]
    data_es <- panel %>% select(stack_unit_num, GEOID, decade, g, event_id, sample_role, all_of(outcome), births_baseline) %>%
      rename(y = all_of(outcome), x = births_baseline)
    training_require(identical(as.data.frame(data_es %>% select(-y, -x)), as.data.frame(frozen_keys)),
                     paste("Outcome changes frozen sample:", timing, group))
    model_index <- model_index + 1L
    model_name <- paste(timing, group, sep = "__")
    seed <- 20261014L + model_index
    set.seed(seed)
    message("Running CSDID: ", model_name, " (", nrow(data_es), " observations)")
    warning_messages <- character()
    result <- tryCatch(withCallingHandlers({
      att <- did::att_gt(yname = "y", tname = "decade", idname = "stack_unit_num", gname = "g",
                         xformla = ~ x, data = data_es, control_group = "nevertreated", est_method = "reg",
                         base_period = "universal", allow_unbalanced_panel = TRUE, cores = 4)
      dynamic <- did::aggte(att, type = "dynamic", na.rm = TRUE, min_e = -20L, max_e = 70L)
      simple <- did::aggte(att, type = "simple", na.rm = TRUE, min_e = 0L, max_e = 70L)
      list(ok = TRUE, att = att, dynamic = dynamic, simple = simple, renormalized = training_renormalize(dynamic), error = NA_character_)
    }, warning = function(w) {
      warning_messages <<- c(warning_messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    result$model <- model_name
    result$timing <- timing
    result$group <- group
    result$outcome <- outcome
    result$seed <- seed
    result$n_rows <- nrow(data_es)
    result$n_units <- n_distinct(data_es$stack_unit_num)
    result$n_events <- n_distinct(data_es$event_id)
    result$warnings <- warning_messages
    models[[model_name]] <- result
    saveRDS(result, file.path(stage, "models", paste0(model_name, ".rds")))
    member <- as.data.table(data_es %>% select(-y))
    member[, `:=`(model = model_name, timing = timing, outcome = outcome)]
    membership[[model_name]] <- member
    if (length(warning_messages)) warnings_log[[model_name]] <- data.table(model = model_name, message = warning_messages)
  }
}

status <- rbindlist(lapply(models, function(m) data.table(model = m$model, timing = m$timing, group = m$group,
                    outcome = m$outcome, control = "births_baseline", ok = m$ok, n_rows = m$n_rows,
                    n_units = m$n_units, n_events = m$n_events, seed = m$seed,
                    warning_count = length(m$warnings), error = m$error)))
fwrite(status, file.path(stage, "model_status.csv"))
successful <- Filter(function(m) isTRUE(m$ok), models)
training_require(length(successful) > 0L, "No event study completed")
dynamic <- rbindlist(lapply(successful, function(m) {
  x <- copy(m$renormalized)
  x[, `:=`(model = m$model, timing = m$timing, group = m$group, outcome = m$outcome)]
  x
}))
simple <- rbindlist(lapply(successful, function(m) data.table(model = m$model, timing = m$timing, group = m$group,
                     outcome = m$outcome, estimate = m$simple$overall.att, se = m$simple$overall.se,
                     ci_low = m$simple$overall.att - 1.96 * m$simple$overall.se,
                     ci_high = m$simple$overall.att + 1.96 * m$simple$overall.se,
                     reference = "Default CSDID pre-treatment base; not re-normalized to e=0")))
training_require(all(is.finite(dynamic$estimate) & is.finite(dynamic$se) & dynamic$se >= 0 &
                       is.finite(dynamic$ci_low) & is.finite(dynamic$ci_high)), "Invalid dynamic inference output")
fwrite(status, file.path(stage, "model_status.csv"))
fwrite(dynamic, file.path(stage, "dynamic_att.csv"))
fwrite(simple, file.path(stage, "post_treatment_att.csv"))
fwrite(rbindlist(membership), file.path(stage, "estimation_sample_membership.csv"))
fwrite(rbindlist(freeze_audit), file.path(stage, "frozen_sample_audit.csv"))
fwrite(sample_coverage[[1]], file.path(stage, "main_sample_matching_coverage.csv"))
fwrite(rbindlist(baseline_comparison), file.path(stage, "county_vs_catchment_baseline.csv"))
warnings <- if (length(warnings_log)) rbindlist(warnings_log) else data.table(model = character(), message = character())
fwrite(warnings, file.path(stage, "warnings.csv"))

decomposition <- merge(
  dynamic[group != "total", .(components = .N, sum_component_effects = sum(estimate)), by = .(timing, event_time)],
  dynamic[group == "total", .(timing, event_time, benchmark_effect = estimate)],
  by = c("timing", "event_time"), all = TRUE)
decomposition[, difference := sum_component_effects - benchmark_effect]
decomposition[, passed := components == 3L & is.finite(difference) & abs(difference) <= 1e-8 * pmax(1, abs(benchmark_effect))]
fwrite(decomposition, file.path(stage, "effect_decomposition_check.csv"))
all_ok <- all(status$ok)
if (all_ok) training_require(all(decomposition$passed) && nrow(decomposition) > 0L, "CSDID effect decomposition failed")

group_plot <- dynamic[group != "total"]
group_plot[, group_label := factor(labels[group], levels = labels[training_groups])]
ylim <- max(abs(c(group_plot$ci_low, group_plot$ci_high)), na.rm = TRUE)
if (!is.finite(ylim) || ylim == 0) ylim <- 1
for (timing in names(prepared_panels)) {
  title_timing <- if (timing == "standard_decade") "Standard decade timing" else "Alternative decade timing"
  this_timing <- timing
  plot_data <- group_plot[timing == this_timing]
  training_require(uniqueN(plot_data$timing) == 1L && identical(unique(plot_data$timing), this_timing) &&
                     !anyDuplicated(plot_data[, .(group, event_time)]), "Figure contains mixed timings or duplicate effects")
  plot <- ggplot(plot_data, aes(x = event_time, y = estimate, ymin = ci_low, ymax = ci_high)) +
    geom_hline(yintercept = 0, colour = "grey65", linetype = "dashed", linewidth = 0.35) +
    geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.3) +
    geom_errorbar(width = 1.5, colour = "#28618C", linewidth = 0.5) +
    geom_line(colour = "#28618C", linewidth = 0.45) + geom_point(colour = "#28618C", size = 1.5) +
    facet_wrap(~ group_label, nrow = 1, drop = FALSE) +
    scale_x_continuous(breaks = seq(-20, 70, 10)) + coord_cartesian(xlim = c(-22, 72), ylim = c(-1, 1) * ylim * 1.08) +
    labs(title = "College allocation and training: 50 km county catchments",
         subtitle = paste0(title_timing, "; ", expected[[timing]]['events'], " allocation events; ", coverage$analysis_counties[coverage$timing == timing], " counties"),
         x = "Years relative to allocation decade", y = "Effect on scientists per 1,000 births",
         caption = paste("Fixed catchments; CSDID regression with pooled baseline births; reference e = 0. Pointwise 95% confidence intervals.",
                         "Training proximity uses birth-county centroids. OpenAlex reference locations; historical campuses unverified.", sep = "\n")) +
    theme_classic(base_size = 11) +
    theme(strip.background = element_rect(fill = "grey95", colour = NA), strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold"), plot.caption = element_text(hjust = 0, size = 9),
          axis.text.x = element_text(size = 9), panel.spacing = grid::unit(1.1, "lines"))
  ggsave(file.path(stage, paste0("training_outcomes_", timing, ".png")), plot, width = 13.2, height = 5.3, dpi = 300)
  ggsave(file.path(stage, paste0("training_outcomes_", timing, ".pdf")), plot, width = 13.2, height = 5.3, useDingbats = FALSE)
}
test_status <- system2(file.path(R.home("bin"), "Rscript.exe"),
                       c("--vanilla", shQuote(file.path(repo, "prep", "land_grants", "test_amws_training_50km.R"))))
training_require(test_status == 0L, "Training outcome regression checks failed")
validation <- list(passed = all_ok && all(decomposition$passed), models_requested = 8L,
                   successful_models = sum(status$ok), failed_models = sum(!status$ok),
                   exact_frozen_keys_and_events_verified = TRUE,
                   baseline_births_updated_to_catchment_totals = TRUE,
                   all_outcomes_have_identical_timing_specific_samples = TRUE,
                   figure_timings_separated_and_unique = TRUE,
                   decomposition_passed = all(decomposition$passed),
                   maximum_effect_decomposition_error = max(abs(decomposition$difference), na.rm = TRUE),
                   standard_errors_or_confidence_limits_summed = FALSE,
                   window = c(-20L, 70L), reference_event_time = 0L,
                   historical_campus_accuracy_verified = FALSE)
county <- fread(file.path(cfg$previous_results, "dynamic_att.csv"))
comparison <- merge(dynamic, county[, .(timing, group, event_time, county_only_estimate = estimate,
  county_only_se = se, county_only_ci_low = ci_low, county_only_ci_high = ci_high)], by = c("timing", "group", "event_time"), all.x = TRUE)
comparison[, change_in_estimate := estimate - county_only_estimate]
fwrite(comparison, file.path(stage, "county_only_vs_catchment_dynamic.csv"))
training_json(validation, file.path(stage, "validation_report.json"))
training_json(list(base_seed = 20261014L, model_seeds = as.list(setNames(status$seed, status$model)),
                   est_method = "reg", control_group = "nevertreated", base_period = "universal",
                   xformla = "~ pooled catchment births at g-10", allow_unbalanced_panel = TRUE, cores = 4L,
                   dynamic_reference = "e=0, manuscript influence-function renormalization",
                   simple_reference = "Default pre-treatment base, as in manuscript", session = capture.output(sessionInfo())),
              file.path(stage, "model_specification.json"))
writeLines(c("Training-location event studies using the current manuscript main specification.",
             "Standard: 16 events, 52 stack units, 650 rows. Alternative: 20 events, 62 stack units, 834 rows.",
             "Eight models: three category rates and an updated-total benchmark under each timing.",
             "All calendar observations retained by the manuscript sample are used; displayed dynamics span -20 to +70.",
             "Dynamic effects and IFs re-normalized to e=0; simple ATT retains the original pre-treatment base.",
             "Category effects sum to the updated total; their SEs and confidence limits are not added.",
             "One fixed county catchment per original site; counts and denominators are sums across counties.",
             "Within-event overlaps use nearest-site assignment with original-county priority. Unsupported additions excluded for every period.",
             "Cross-event county reuse is retained; inherited inference does not add a correction for this induced dependence.",
             "Counts reflect existing edition-specific inclusion rules, not new cross-edition person linkage.",
             "Reference locations and existing provisional IDs do not establish historical teaching campuses."),
           file.path(stage, "README.txt"))
dir.create(file.path(stage, "code_snapshot"), showWarnings = FALSE)
for (p in c(script, file.path(repo, "prep", "land_grants", "amws_catchment_helpers.R"), file.path(repo, "prep", "land_grants", c("amws_training_helpers.R", "test_amws_training_50km.R"))))
  file.copy(p, file.path(stage, "code_snapshot", basename(p)), overwrite = TRUE)
training_json(list(elapsed_seconds = unname(proc.time()[3] - started)), file.path(stage, "timing_report.json"))
training_publish(stage, cfg$results, inputs, list(status = if (all_ok) "complete" else "complete_with_estimation_failures", validation = validation))
cat("Published training event studies:", cfg$results, "\n")
print(status)
print(simple)
