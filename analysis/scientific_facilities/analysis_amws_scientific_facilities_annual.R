# Annual companion: pre-1944 target facilities, outcomes ending in 1949,
# ragged and fixed-opening-cohort curves under both C&S control groups.
# Run from any directory with TALENT_DETS_DATA_DIR set to the Dropbox root.
# --preflight-only writes data/support audits without requiring did to load.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "analysis/scientific_facilities/amws_facilities_annual_helpers.R"))
suppressPackageStartupMessages({library(data.table); library(sf); library(ggplot2)})

sfa_main <- function(settings = sfa_settings(), outcomes = names(sfa_outcomes), results_family = NULL) {
  if (!length(outcomes) || anyDuplicated(outcomes) || any(!outcomes %in% names(sfa_outcomes)))
    stop("Invalid annual outcome selection")
  settings$biters <- as.integer(Sys.getenv("FACILITIES_ANNUAL_BITERS", unset = "1000"))
  if (!is.finite(settings$biters) || settings$biters < 2) stop("Invalid bootstrap iteration count")
  tag <- Sys.getenv("FACILITIES_ANNUAL_RESULTS_TAG",
                   unset = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (!grepl("^[A-Za-z0-9_-]+$", tag)) stop("Results tag must contain letters, digits, underscores or hyphens")
  controls <- c("nevertreated", "notyettreated")
  balances <- c("unbalanced", "balanced")
  specifications <- CJ(outcome = outcomes, control_group = controls, balance = balances)
  specifications[, figure_file := paste0("ES_annual_", outcome, "__", control_group, "__", balance, ".png")]
  if (is.null(results_family)) results_family <- paste0(
    "amws_scientific_facilities_annual_pre", settings$target_cutoff + 1L, "_end", settings$last_year)
  out_dir <- file.path(TALENT_DETS_DATA_DIR, "results/scientific_facilities/event_studies",
                       results_family, tag)
  if (dir.exists(out_dir)) stop("Refusing to overwrite existing run: ", out_dir)
  panel_path <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
  facility_path <- file.path(DATA_OUTPUT, "facilities_us.csv")
  county_path <- file.path(TALENT_DETS_DATA_DIR, "raw/tigris/cb_2020_us_county_20m.shp")
  if (!file.exists(panel_path) || !file.exists(facility_path)) {
    stop("Missing annual inputs. Set TALENT_DETS_DATA_DIR to the Dropbox project root.")
  }
  if (!file.exists(county_path)) {
    if (!requireNamespace("tigris", quietly = TRUE)) stop("Missing cached counties and tigris")
    options(tigris_use_cache = TRUE, tigris_cache_dir = file.path(TALENT_DETS_DATA_DIR, "raw/tigris"))
    county_shape <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020)
    # Use a temporary copy for this run; leave the source/cache contents intact.
    county_path <- file.path(tempdir(), paste0("annual_facilities_counties_", tag, ".gpkg"))
    sf::st_write(county_shape, county_path, quiet = TRUE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  cat("Results directory:", out_dir, "\n")
  write_table <- function(x, name) fwrite(x, file.path(out_dir, name))
  manifest <- data.table(metric = c(names(settings), "outcomes", "expected_figures", "results_tag", "R_version", "status"),
                         value = c(as.character(unlist(settings)), paste(outcomes, collapse = ";"),
                                   as.character(nrow(specifications)), tag,
                                   R.version.string, "preflight_running"))
  update_status <- function(run_status) {
    manifest[metric == "status", value := run_status]
    write_table(manifest, "run_manifest.csv")
  }
  update_status("preflight_running")
  inputs <- c(panel_path, facility_path, county_path)
  if (grepl("\\.shp$", county_path)) inputs <- c(inputs, sub("\\.shp$", ".dbf", county_path),
                                                sub("\\.shp$", ".prj", county_path),
                                                sub("\\.shp$", ".shx", county_path))
  early_summary <- file.path(DATA_OUTPUT, "amws/amws_consolidated_county_year_summary.csv")
  final_1986 <- file.path(TALENT_DETS_DATA_DIR, "Data/processed/amws/amws_ed86_final.csv")
  inputs <- c(inputs, c(early_summary, final_1986)[file.exists(c(early_summary, final_1986))],
              script_path, file.path(repo_root, "analysis/scientific_facilities",
                c("analysis_amws_scientific_facilities_annual.R", "amws_facilities_annual_helpers.R")))
  inputs <- unique(inputs[file.exists(inputs)])
  input_info <- file.info(inputs)
  write_table(data.table(path = normalizePath(inputs, winslash = "/"),
                        bytes = input_info$size,
                        modified_at = format(input_info$mtime, "%Y-%m-%d %H:%M:%S %z"),
                        md5 = unname(tools::md5sum(inputs))), "input_manifest.csv")
  packages <- c("data.table", "sf", "ggplot2", "did", "DRDID", "fastglm", "bigmemory")
  write_table(data.table(package = packages, version = vapply(packages, function(p) {
    tryCatch(as.character(packageVersion(p)), error = function(e) NA_character_)
  }, character(1))), "package_manifest.csv")

  panel <- sfa_read_panel(panel_path, settings)
  exposure <- sfa_exposure(facility_path, county_path, settings)
  panel <- merge(panel, exposure$assignment[, .(GEOID, g, role, first_exposure_year)],
                 by = "GEOID", all.x = TRUE)
  unmatched <- panel[is.na(g), .(GEOID, year, reason = "county_absent_from_2020_geometry")]
  write_table(unmatched, "geography_exclusions.csv")
  panel <- panel[!is.na(g)]
  assignment <- copy(exposure$assignment)
  assignment[, observed_panel_years := 0L]
  availability <- panel[, .(observed_panel_years = .N), by = GEOID]
  assignment[availability, on = "GEOID", observed_panel_years := i.observed_panel_years]
  write_table(assignment, "treatment_assignment.csv")
  write_table(exposure$facilities, "facility_dates.csv")
  write_table(exposure$links, "county_facility_exposure_links.csv")
  write_table(panel[, .(counties = uniqueN(GEOID), n_amws = sum(n_amws),
                        n_amws_early = sum(n_amws_1906_1955_dedup),
                        n_amws_1986 = sum(n_amws_1986)), by = year][order(year)],
              "annual_outcome_coverage.csv")
  write_table(unique(panel[, .(GEOID, g, role)])[, .(n_counties = .N), by = .(g, role)][order(g)],
              "cohort_distribution.csv")
  targets <- sort(unique(panel[role == "target", g]))
  if (!length(targets)) stop("No target cohorts through ", settings$target_cutoff, " in the annual panel")
  grid <- seq.int(-settings$pre, settings$last_year - min(targets))
  write_table(specifications, "figure_manifest.csv")
  specifications[, `:=`(status = "pending", error = NA_character_)]
  model_data <- list()
  supports <- list()
  all_exclusions <- list()
  for (outcome in outcomes) {
    prepared <- sfa_model_data(panel, outcome)
    model_data[[outcome]] <- prepared$data
    all_exclusions[[outcome]] <- prepared$exclusions
    for (control in controls) {
      key <- paste(outcome, control, sep = "__")
      audit <- sfa_support(prepared$data, targets, grid, control, settings)
      audit[, `:=`(outcome = outcome, control_group = control)]
      supports[[key]] <- audit
    }
  }
  write_table(rbindlist(all_exclusions), "outcome_exclusions.csv")
  write_table(rbindlist(supports), "support_preflight.csv")
  notes <- c(
    "Annual AMWS facilities analysis. Exact-year exposure; no decade averaging.",
    paste0("Target events: year <= ", settings$target_cutoff, ". Outcome calendar: ",
      settings$first_year, "-", settings$last_year, ". Reference: ", settings$reference, "."),
    paste0("Unbalanced horizon derives from ", settings$last_year, " minus the earliest target year."),
    paste0("Balanced means fixed opening-year cohorts with estimable cells at all e=-", settings$pre,
      ",..., +", settings$balanced_post, "; counties may vary."),
    "Control exposure is constructed from the COMPLETE facility list, including all donor-only events.",
    paste0("Exposures in ", settings$target_cutoff + 1L, "-", settings$last_year,
      " are donor-only and never enter reported target aggregation."),
    "Not-yet-treated controls must remain unexposed in both the comparison and reference years.",
    "Facilities after 1949 are unexposed within the observed period.",
    "50km buffers in EPSG:3857 intersect 2020 counties, preserving the previous spatial definition.",
    paste0("Outcomes: ", paste(outcomes, collapse = ";"),
      "; two control settings; two balance settings. No gaps are filled with zero."),
    "Plot baselines pool unique county-reference observations from treated and eligible controls; counts are unique contributors across displayed comparisons.",
    "Pointwise 95% intervals; multiplier bootstrap and county clustering, DR with xformla=~1.",
    "Post summaries weight eligible ATT(g,t) cells by cohort county count; longer-followed cohorts contribute more cells.",
    "1986 records are not deduplicated against the early editions, as in the current source panel."
  )
  writeLines(notes, file.path(out_dir, "notes.txt"))
  if ("--preflight-only" %in% commandArgs(trailingOnly = TRUE)) {
    specifications[, status := "preflight_only"]
    write_table(specifications, "model_status.csv")
    update_status("preflight_complete")
    cat("Preflight complete; estimation was not requested.\n")
    return(invisible(out_dir))
  }
  dependency_error <- tryCatch({
    suppressPackageStartupMessages(library(did))
    NA_character_
  }, error = function(e) conditionMessage(e))
  write_table(data.table(package = "did", loaded = is.na(dependency_error),
                        error = dependency_error), "dependency_status.csv")
  if (!is.na(dependency_error)) {
    specifications[, `:=`(status = "blocked_dependency", error = dependency_error)]
    write_table(specifications, "model_status.csv")
    update_status("blocked_dependency")
    stop("Preflight saved, but did cannot load. No estimates or figures were produced. ", dependency_error)
  }

  all_dynamic <- list(); all_cells <- list(); all_summaries <- list()
  all_membership <- list(); all_warnings <- list(); all_checks <- list()
  all_support_estimation <- list()
  all_annotations <- list(); all_baselines <- list(); all_participants <- list()
  for (outcome in outcomes) {
    oi <- match(outcome, names(sfa_outcomes))
    outcome_name <- outcome
    d <- model_data[[outcome]]
    weights <- unique(d[g %in% targets, .(id, g)])[, .(n_counties = .N), by = g]
    for (ci in seq_along(controls)) {
      control <- controls[ci]
      key <- paste(outcome, control, sep = "__")
      settings$seed <- sfa_settings()$seed + oi * 100L + ci * 10L
      cat("Estimating", key, "\n")
      fitted <- tryCatch(sfa_fit(d, control, settings), error = function(e) e)
      if (inherits(fitted, "error")) {
        specifications[outcome == outcome_name & control_group == control,
                       `:=`(status = "failed", error = conditionMessage(fitted))]
        next
      }
      if (length(fitted$warnings)) all_warnings[[key]] <- data.table(
        outcome = outcome, control_group = control, warning = fitted$warnings)
      cells <- sfa_cells(fitted$model, weights$g, grid, supports[[key]])
      all_support_estimation[[key]] <- copy(cells)[, `:=`(outcome = outcome, control_group = control)]
      for (balance_mode in balances) {
        result_key <- paste(key, balance_mode, sep = "__")
        result <- tryCatch(sfa_aggregate(fitted$model, cells, weights, balance_mode, settings),
                           error = function(e) e)
        if (inherits(result, "error")) {
          specifications[outcome == outcome_name & control_group == control & balance == balance_mode,
                         `:=`(status = "failed", error = conditionMessage(result))]
          next
        }
        for (component in c("dynamic", "cells", "summaries")) {
          result[[component]][, `:=`(outcome = outcome, control_group = control, balance = balance_mode)]
        }
        all_dynamic[[result_key]] <- result$dynamic
        all_cells[[result_key]] <- result$cells
        all_summaries[[result_key]] <- result$summaries
        balanced_grid <- seq.int(-settings$pre, settings$balanced_post)
        membership <- cells[event_time %in% balanced_grid,
          .(full_window_estimable = all(estimable) && .N == length(balanced_grid)), by = g]
        membership[, `:=`(outcome = outcome, control_group = control, balance = balance_mode,
                           included = g %in% result$cohorts)]
        all_membership[[result_key]] <- membership
        all_checks[[result_key]] <- result$dynamic[, .(outcome, control_group, balance, event_time,
                                                      estimate, manual_att,
                                                      difference = estimate - manual_att)]
        annotation <- sfa_annual_annotation(d, result$cells, control,
                                            paired = isTRUE(fitted$model$DIDparams$panel))
        for (component in names(annotation)) annotation[[component]][,
          `:=`(outcome = outcome, control_group = control, balance = balance_mode)]
        all_annotations[[result_key]] <- annotation$summary
        all_baselines[[result_key]] <- annotation$baseline
        all_participants[[result_key]] <- annotation$participants
        ggsave(file.path(out_dir, paste0("ES_annual_", result_key, ".png")),
               sfa_plot(result$dynamic, outcome, control, balance_mode, annotation$summary,
                 reference = settings$reference, target_cutoff = settings$target_cutoff,
                 last_year = settings$last_year),
               width = 11, height = 6, dpi = 200, bg = "white")
        specifications[outcome == outcome_name & control_group == control & balance == balance_mode,
                       status := "estimated"]
      }
    }
  }
  exports <- list(dynamic_att = all_dynamic, cohort_time_att = all_cells,
                  post_att_summary = all_summaries, cohort_membership = all_membership,
                  model_warnings = all_warnings, aggregation_validation = all_checks,
                  support_estimation = all_support_estimation,
                  plot_annotation_summary = all_annotations,
                  plot_baseline_observations = all_baselines, county_contributions = all_participants)
  for (name in names(exports)) {
    if (length(exports[[name]])) write_table(rbindlist(exports[[name]], fill = TRUE), paste0(name, ".csv"))
  }
  write_table(specifications, "model_status.csv")
  complete <- all(specifications$status == "estimated") &&
    length(list.files(out_dir, pattern = "^ES_annual_.*\\.png$")) == nrow(specifications)
  update_status(if (complete) "complete" else "incomplete")
  if (!complete) stop("Some specifications failed; inspect model_status.csv in ", out_dir)
  cat("Completed all", nrow(specifications), "specifications and figures in", out_dir, "\n")
  invisible(out_dir)
}

if (sys.nframe() == 0L) sfa_main()
