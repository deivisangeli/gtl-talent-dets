###############################################################################
# Validate and consolidate a versioned elite-schools AMWS refresh.
###############################################################################
suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg)) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

tag <- trimws(Sys.getenv("ELITE_RESULTS_TAG", unset = "countyfix_amws1986_20260719"))
run_dir <- file.path(TALENT_DETS_DATA_DIR, "results", "elite_schools", "amws_refresh", tag)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

result_rel <- c(
  event_study_cs = file.path("event_study_yearly_1860_1910", "all_event_study_estimates.csv"),
  amws_wiki_sunab = file.path("elite_school_event_studies_year_amws", "event_study_estimates_threespec.csv"),
  synthetic_control = file.path("event_study_yearly_1860_1910", "sc", "all_sc_estimates.csv"),
  continuous_treatment = file.path("continuous_treatment_panel", "continuous_treatment_estimates.csv")
)
result_path <- function(relative_file, mode) {
  parts <- strsplit(relative_file, .Platform$file.sep, fixed = TRUE)[[1]]
  do.call(file.path, as.list(c(
    TALENT_DETS_DATA_DIR, "results", "elite_schools", parts[[1]], tag,
    paste0("timing_", mode), parts[-1]
  )))
}

output_manifest <- rbindlist(lapply(names(result_rel), function(result_name) {
  rbindlist(lapply(c("exposure14", "opening"), function(mode) {
    path <- result_path(result_rel[[result_name]], mode)
    data.table(
      analysis = result_name, timing_mode = mode, path = path,
      exists = file.exists(path), bytes = if (file.exists(path)) file.info(path)$size else NA_real_
    )
  }))
}))
if (output_manifest[, any(!exists | is.na(bytes) | bytes <= 0)]) {
  fwrite(output_manifest, file.path(run_dir, "output_manifest.csv"))
  stop("One or more expected analysis outputs are missing or empty")
}

expected_outcomes <- c(
  event_study_cs = 7L, amws_wiki_sunab = 4L,
  synthetic_control = 5L, continuous_treatment = 7L
)
comparison_checks <- list()
for (result_name in names(result_rel)) {
  combined <- rbindlist(lapply(c("exposure14", "opening"), function(mode) {
    out <- fread(result_path(result_rel[[result_name]], mode))
    if (!"timing_mode" %in% names(out)) out[, timing_mode := mode]
    out
  }), fill = TRUE)
  fwrite(combined, file.path(run_dir, paste0(result_name, "_timing_comparison.csv")))
  estimate_cols <- intersect(
    c(
      "estimate", "est", "std_error", "se", "Estimate", "Std. Error",
      "gap", "ci_low", "ci_high", "lo", "hi", "treated", "synth"
    ),
    names(combined)
  )
  comparison_checks[[result_name]] <- data.table(
    check = paste0("complete_finite_results_", result_name),
    passed = uniqueN(combined$timing_mode) == 2L &&
      uniqueN(combined$outcome) == expected_outcomes[[result_name]] &&
      all(vapply(estimate_cols, function(z) all(is.finite(combined[[z]])), logical(1))),
    detail = paste(nrow(combined), "rows;", uniqueN(combined$outcome), "outcomes")
  )
}

panel_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
panel <- fread(panel_file)
panel_summary <- data.table(
  metric = c(
    "panel_rows", "panel_counties", "panel_min_year", "panel_max_year",
    "n_amws_1906_1955_dedup", "n_amws_1986", "n_amws_total",
    "component_reconciliation_failures"
  ),
  value = c(
    nrow(panel), uniqueN(panel$GEOID), min(panel$year), max(panel$year),
    sum(panel$n_amws_1906_1955_dedup, na.rm = TRUE),
    sum(panel$n_amws_1986, na.rm = TRUE), sum(panel$n_amws, na.rm = TRUE),
    panel[, sum(n_amws != n_amws_1906_1955_dedup + n_amws_1986, na.rm = TRUE)]
  )
)
fwrite(panel_summary, file.path(run_dir, "panel_validation_summary.csv"))

national_file <- file.path(SCHOOLS_OUTPUT, "elite_high_schools_national_1800_1930.csv")
core_file <- file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv")
national <- fread(national_file, colClasses = "character")
core <- fread(core_file, colClasses = "character")
overrides_file <- file.path(SCHOOLS_INPUT, "elite_high_schools_founding_county_overrides.csv")
overrides <- fread(overrides_file, colClasses = "character")
setnames(overrides, "school_state_abbr", "state_abbr")

audit <- merge(
  overrides,
  national[, .(
    state_abbr, school, school_city, school_county_name, founding_state_abbr,
    founding_city, founding_county_name, founding_county_geoid, county_geoid,
    founding_geo_audit_status, founding_geo_core_action, include_in_core_sample
  )],
  by = c("state_abbr", "school"), all.x = TRUE, suffixes = c("_override", "_output")
)
audit[, validation_ok :=
  !is.na(founding_county_geoid) & county_geoid == founding_county_geoid &
  founding_geo_audit_status == audit_status & founding_geo_core_action == core_action &
  (apply_geography_override == "no" |
     (founding_county_geoid == expected_founding_county_geoid &
        founding_county_name_output == founding_county_name_override)) &
  (core_action != "exclude_from_core" | include_in_core_sample == "no")]
fwrite(audit, file.path(run_dir, "school_geography_changes.csv"))

core_keys <- paste(core$state_abbr, core$school, sep = "|")
excluded_keys <- paste(
  overrides[core_action == "exclude_from_core"]$state_abbr,
  overrides[core_action == "exclude_from_core"]$school,
  sep = "|"
)
high_access <- core[crit_high_access_strict == "yes"]

event_checks <- rbindlist(lapply(c("exposure14", "opening"), function(mode) {
  event_file <- file.path(
    TALENT_DETS_DATA_DIR, "results", "elite_schools",
    "event_study_yearly_1860_1910", tag, paste0("timing_", mode),
    "treatment_events.csv"
  )
  ev <- fread(event_file)
  expected_delta <- if (mode == "exposure14") 14L else 0L
  data.table(
    check = paste0("event_timing_", mode),
    passed = nrow(ev) == 3L && all(ev$opening_year - ev$event_year == expected_delta),
    detail = paste(ev$school, ev$opening_year, ev$event_year, sep = ":", collapse = "; ")
  )
}))

log_files <- list.files(file.path(run_dir, "logs"), pattern = "\\.log$", full.names = TRUE)
bad_log_pattern <- "(^|[^[:alpha:]])FAIL([^[:alpha:]]|$)|Execution halted|Execu.*interrompida|Error in"
log_checks <- rbindlist(lapply(log_files, function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  data.table(log_file = path, passed = !grepl(bad_log_pattern, txt, ignore.case = TRUE, perl = TRUE))
}))
command_manifest <- log_checks[, .(
  step = tools::file_path_sans_ext(basename(log_file)),
  status = ifelse(passed, 0L, 1L),
  log_file,
  log_bytes = file.info(log_file)$size,
  log_modified_at = format(file.info(log_file)$mtime, "%Y-%m-%d %H:%M:%S %Z")
)]

validation <- rbindlist(list(
  data.table(check = "all_expected_outputs_nonempty", passed = output_manifest[, all(exists & bytes > 0)], detail = paste(nrow(output_manifest), "files")),
  data.table(check = "amws_components_reconcile", passed = panel_summary[metric == "component_reconciliation_failures", value] == 0, detail = panel_summary[metric == "n_amws_total", as.character(value)]),
  data.table(check = "amws_1986_is_present", passed = panel_summary[metric == "n_amws_1986", value] > 0, detail = panel_summary[metric == "n_amws_1986", as.character(value)]),
  data.table(check = "all_15_geography_audits_match", passed = nrow(audit) == 15L && all(audit$validation_ok), detail = paste(sum(audit$validation_ok), "of", nrow(audit))),
  data.table(check = "ambiguous_schools_excluded_from_core", passed = !any(excluded_keys %in% core_keys), detail = paste(length(excluded_keys), "schools")),
  data.table(check = "high_access_definition_unchanged", passed = nrow(high_access) == 13L && uniqueN(high_access$county_geoid) == 9L, detail = paste(nrow(high_access), "schools in", uniqueN(high_access$county_geoid), "counties")),
  rbindlist(comparison_checks),
  event_checks,
  data.table(check = "analysis_logs_without_failure_markers", passed = all(log_checks$passed), detail = paste(sum(log_checks$passed), "of", nrow(log_checks), "logs"))
), fill = TRUE)

input_manifest <- data.table(
  input = c("founding_county_overrides", "amws_1986", "amws_early_dedup", "population_panel", "school_core_output", "amws_combined_output"),
  path = c(
    overrides_file,
    file.path(TALENT_DETS_DATA_DIR, "Data", "processed", "amws", "amws_ed86_final.csv"),
    file.path(AMWS_OUTPUT, "amws_combined_us_geocoded.csv"),
    file.path(DATA_OUTPUT, "us_panel_county_stem_year_1800.csv"),
    core_file, panel_file
  )
)
input_manifest[, `:=`(
  exists = file.exists(path),
  bytes = file.info(path)$size,
  modified_at = format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S %Z")
)]

fwrite(output_manifest, file.path(run_dir, "output_manifest.csv"))
fwrite(log_checks, file.path(run_dir, "log_validation.csv"))
fwrite(command_manifest, file.path(run_dir, "command_manifest.csv"))
fwrite(validation, file.path(run_dir, "validation_summary.csv"))
fwrite(input_manifest, file.path(run_dir, "input_manifest.csv"))
if (validation[, any(!passed)]) {
  stop("Refresh validation failed; see ", file.path(run_dir, "validation_summary.csv"))
}
cat("Refresh validation passed. Consolidated files in", run_dir, "\n")
