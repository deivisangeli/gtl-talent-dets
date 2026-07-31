###############################################################################
# Rebuild elite-school founding geography and the combined AMWS panel, then
# rerun the complete AMWS elite-school analysis under both timing conventions.
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
if (!nzchar(tag)) stop("ELITE_RESULTS_TAG cannot be empty for a refresh run")
Sys.setenv(
  TALENT_DETS_DATA_DIR = TALENT_DETS_DATA_DIR,
  GTL_REPO = repo_root,
  ELITE_RESULTS_TAG = tag
)
run_dir <- file.path(TALENT_DETS_DATA_DIR, "results", "elite_schools", "amws_refresh", tag)
log_dir <- file.path(run_dir, "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

school_file <- file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv")
panel_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
if (!file.exists(school_file)) stop("Missing pre-refresh school file: ", school_file)
if (!file.exists(panel_file)) stop("Missing pre-refresh AMWS panel: ", panel_file)

old_school <- fread(school_file)
old_panel <- fread(panel_file)
old_school[, county_geoid := sprintf("%05d", as.integer(county_geoid))]
old_low <- unique(old_school[
  access_model_historical_prelim == "private_tuition_dominant" &
    crit_secondary_school == "yes" & crit_in_frame_1800_1940 == "yes" &
    founding_year_used <= 1910,
  county_geoid
])

command_log <- list()
run_command <- function(label, command, command_args, extra_env = character()) {
  log_file <- file.path(log_dir, paste0(label, ".log"))
  if (length(extra_env)) {
    eq <- regexpr("=", extra_env, fixed = TRUE)
    if (any(eq < 1L)) stop("Malformed environment assignment: ", extra_env[eq < 1L])
    env_names <- substring(extra_env, 1L, eq - 1L)
    env_values <- substring(extra_env, eq + 1L)
    do.call(Sys.setenv, as.list(setNames(env_values, env_names)))
  }
  started <- Sys.time()
  status <- system2(
    command, shQuote(command_args), stdout = log_file, stderr = log_file,
    wait = TRUE
  )
  ended <- Sys.time()
  command_log[[length(command_log) + 1L]] <<- data.table(
    step = label,
    command = paste(c(command, command_args), collapse = " "),
    status = as.integer(status),
    started_at = format(started, "%Y-%m-%d %H:%M:%S %Z"),
    ended_at = format(ended, "%Y-%m-%d %H:%M:%S %Z"),
    elapsed_seconds = as.numeric(difftime(ended, started, units = "secs")),
    log_file = log_file
  )
  if (!identical(as.integer(status), 0L)) {
    fwrite(rbindlist(command_log), file.path(run_dir, "command_manifest.csv"))
    stop("Refresh step failed: ", label, ". See ", log_file)
  }
}

run_command(
  "01_build_elite_schools", "python",
  file.path(repo_root, "prep", "elite_schools", "build_elite_high_schools_national.py")
)
run_command(
  "02_dedup_amws_1906_1938_1955", "Rscript",
  file.path(repo_root, "prep", "amws", "dedup_amws_editions.R")
)
run_command(
  "03_build_amws_combined_panel", "Rscript",
  file.path(repo_root, "prep", "amws", "build_amws_combined_county_year.R")
)

analysis_scripts <- c(
  event_study_cs = "analysis_event_study_yearly_1860_1910.R",
  amws_wiki_sunab = "analysis_elite_school_year_amws_wiki.R",
  synthetic_control = "analysis_synthetic_control_yearly_1860_1910.R",
  continuous_treatment = "analysis_continuous_treatment_panel.R"
)
for (mode in c("exposure14", "opening")) {
  for (label in names(analysis_scripts)) {
    run_command(
      paste0("04_", mode, "_", label),
      "Rscript",
      file.path(repo_root, "analysis", "elite_schools", analysis_scripts[[label]]),
      extra_env = paste0("ELITE_TREATMENT_TIMING=", mode)
    )
  }
}

# Consolidate the two timing variants without touching any pre-existing result.
analysis_result_files <- c(
  event_study_cs = file.path("event_study_yearly_1860_1910", "all_event_study_estimates.csv"),
  amws_wiki_sunab = file.path("elite_school_event_studies_year_amws", "event_study_estimates_threespec.csv"),
  synthetic_control = file.path("event_study_yearly_1860_1910", "sc", "all_sc_estimates.csv"),
  continuous_treatment = file.path("continuous_treatment_panel", "continuous_treatment_estimates.csv")
)
for (result_name in names(analysis_result_files)) {
  timing_results <- lapply(c("exposure14", "opening"), function(mode) {
    relative_file <- analysis_result_files[[result_name]]
    parts <- strsplit(relative_file, .Platform$file.sep, fixed = TRUE)[[1]]
    result_file <- do.call(file.path, as.list(c(
      TALENT_DETS_DATA_DIR, "results", "elite_schools", parts[[1]], tag,
      paste0("timing_", mode), parts[-1]
    )))
    if (!file.exists(result_file)) stop("Expected analysis result is missing: ", result_file)
    out <- fread(result_file)
    if (!"timing_mode" %in% names(out)) out[, timing_mode := mode]
    out
  })
  fwrite(
    rbindlist(timing_results, fill = TRUE),
    file.path(run_dir, paste0(result_name, "_timing_comparison.csv"))
  )
}

new_school <- fread(school_file)
new_panel <- fread(panel_file)
new_school[, county_geoid := sprintf("%05d", as.integer(county_geoid))]
new_low <- unique(new_school[
  access_model_historical_prelim == "private_tuition_dominant" &
    crit_secondary_school == "yes" & crit_in_frame_1800_1940 == "yes" &
    founding_year_used <= 1910,
  county_geoid
])

panel_summary <- data.table(
  metric = c(
    "panel_rows", "panel_counties", "n_amws_1906_1955_dedup",
    "n_amws_1986", "n_amws_total", "low_access_control_counties"
  ),
  old = c(
    nrow(old_panel), uniqueN(old_panel$GEOID),
    sum(old_panel$n_amws_1906_1955_dedup, na.rm = TRUE),
    sum(old_panel$n_amws_1986, na.rm = TRUE), sum(old_panel$n_amws, na.rm = TRUE),
    length(old_low)
  ),
  new = c(
    nrow(new_panel), uniqueN(new_panel$GEOID),
    sum(new_panel$n_amws_1906_1955_dedup, na.rm = TRUE),
    sum(new_panel$n_amws_1986, na.rm = TRUE), sum(new_panel$n_amws, na.rm = TRUE),
    length(new_low)
  )
)
panel_summary[, change := new - old]
fwrite(panel_summary, file.path(run_dir, "old_vs_new_panel_summary.csv"))

school_keys <- c("state_abbr", "school")
school_geo <- merge(
  old_school[, .(state_abbr, school, old_county_geoid = county_geoid,
                 old_core = include_in_core_sample)],
  new_school[, .(state_abbr, school, new_county_geoid = county_geoid,
                 new_core = include_in_core_sample,
                 founding_geo_audit_status)],
  by = school_keys, all = TRUE
)
school_geo[, changed :=
  fcoalesce(old_county_geoid, "") != fcoalesce(new_county_geoid, "") |
  fcoalesce(old_core, "") != fcoalesce(new_core, "")]
fwrite(school_geo[changed == TRUE], file.path(run_dir, "school_geography_changes.csv"))

control_changes <- data.table(
  county_geoid = sort(unique(c(old_low, new_low)))
)[, `:=`(
  old_low_access_control = county_geoid %in% old_low,
  new_low_access_control = county_geoid %in% new_low
)][old_low_access_control != new_low_access_control]
fwrite(control_changes, file.path(run_dir, "low_access_control_changes.csv"))

input_manifest <- data.table(
  input = c(
    "founding_county_overrides", "amws_1986", "amws_early_dedup",
    "population_panel", "school_core_output", "amws_combined_output"
  ),
  path = c(
    file.path(SCHOOLS_INPUT, "elite_high_schools_founding_county_overrides.csv"),
    file.path(TALENT_DETS_DATA_DIR, "Data", "processed", "amws", "amws_ed86_final.csv"),
    file.path(AMWS_OUTPUT, "amws_combined_us_geocoded.csv"),
    file.path(DATA_OUTPUT, "us_panel_county_stem_year_1800.csv"),
    school_file, panel_file
  )
)
input_manifest[, `:=`(
  exists = file.exists(path),
  bytes = file.info(path)$size,
  modified_at = format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S %Z")
)]
fwrite(input_manifest, file.path(run_dir, "input_manifest.csv"))
run_command(
  "05_validate_refresh", "Rscript",
  file.path(repo_root, "analysis", "elite_schools", "validate_amws_1986_refresh.R")
)
fwrite(rbindlist(command_log), file.path(run_dir, "command_manifest.csv"))
cat("Refresh complete. Manifest and comparisons in", run_dir, "\n")
