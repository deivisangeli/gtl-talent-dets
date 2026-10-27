# Expanded complete-decade companion. The legacy decennial script is unchanged.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) normalizePath(sub("^--file=", "", file_arg[1]),
  winslash = "/", mustWork = TRUE) else NULL
repo_root <- if (!is.null(script_path)) normalizePath(file.path(dirname(script_path), "..", ".."),
  winslash = "/", mustWork = TRUE) else normalizePath(Sys.getenv("GTL_REPO", getwd()), winslash = "/")
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "analysis/scientific_facilities/amws_facilities_annual_helpers.R"))
source(file.path(repo_root, "analysis/scientific_facilities/amws_facilities_decennial_helpers.R"))
suppressPackageStartupMessages({library(data.table); library(sf); library(ggplot2)})

sfd_main <- function(settings = sfd_settings(), results_family = NULL) {
  settings$biters <- as.integer(Sys.getenv("FACILITIES_DECENNIAL_BITERS", "1000"))
  if (!is.finite(settings$biters) || settings$biters < 2) stop("Invalid bootstrap iterations")
  tag <- Sys.getenv("FACILITIES_DECENNIAL_RESULTS_TAG", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (!grepl("^[A-Za-z0-9_-]+$", tag)) stop("Invalid results tag")
  if (is.null(results_family)) results_family <- paste0(
    "amws_scientific_facilities_decennial_pre", settings$target_cutoff + 1L,
    "_end", settings$last_year)
  out_dir <- file.path(TALENT_DETS_DATA_DIR, "results/scientific_facilities/event_studies",
    results_family, tag)
  if (dir.exists(out_dir)) stop("Refusing to overwrite existing run: ", out_dir)
  panel_path <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
  facility_path <- file.path(DATA_OUTPUT, "facilities_us.csv")
  county_path <- file.path(TALENT_DETS_DATA_DIR, "raw/tigris/cb_2020_us_county_20m.shp")
  if (!file.exists(county_path)) {
    if (!requireNamespace("tigris", quietly = TRUE)) stop("Missing counties and tigris")
    options(tigris_use_cache = TRUE, tigris_cache_dir = file.path(TALENT_DETS_DATA_DIR, "raw/tigris"))
    county_path <- file.path(tempdir(), paste0("decennial_counties_", tag, ".gpkg"))
    sf::st_write(tigris::counties(cb = TRUE, resolution = "20m", year = 2020), county_path, quiet = TRUE)
  }
  if (!file.exists(panel_path) || !file.exists(facility_path)) stop("Missing AMWS/facility inputs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  cat("Results directory:", out_dir, "\n")
  write_table <- function(x, name) fwrite(x, file.path(out_dir, name))
  manifest <- data.table(metric = c(names(settings), "results_tag", "R_version", "status"),
    value = c(as.character(unlist(settings)), tag, R.version.string, "preflight_running"))
  update_status <- function(status) {
    manifest[metric == "status", value := status]; write_table(manifest, "run_manifest.csv")
  }
  update_status("preflight_running")
  inputs <- c(panel_path, facility_path, county_path, script_path,
    file.path(repo_root, "analysis/scientific_facilities/analysis_amws_scientific_facilities_decennial_expanded.R"),
    file.path(repo_root, "analysis/scientific_facilities",
      c("amws_facilities_annual_helpers.R", "amws_facilities_decennial_helpers.R")),
    file.path(AMWS_OUTPUT, "amws_consolidated_county_year_summary.csv"),
    file.path(TALENT_DETS_DATA_DIR, "Data/processed/amws/amws_ed86_final.csv"))
  if (grepl("\\.shp$", county_path)) inputs <- c(inputs,
    sub("\\.shp$", ".dbf", county_path), sub("\\.shp$", ".prj", county_path), sub("\\.shp$", ".shx", county_path))
  inputs <- unique(inputs[file.exists(inputs)])
  info <- file.info(inputs)
  write_table(data.table(path = normalizePath(inputs, winslash = "/"), bytes = info$size,
    modified_at = format(info$mtime, "%Y-%m-%d %H:%M:%S %z"), md5 = unname(tools::md5sum(inputs))), "input_manifest.csv")
  pkgs <- c("data.table", "sf", "ggplot2", "did", "DRDID", "fastglm", "bigmemory")
  write_table(data.table(package = pkgs, version = vapply(pkgs, function(p)
    tryCatch(as.character(packageVersion(p)), error = function(e) NA_character_), character(1))), "package_manifest.csv")
  annual <- sfa_read_panel(panel_path, settings)
  exposure <- sfa_exposure(facility_path, county_path, settings)
  built <- sfd_build_panel(annual)
  geography <- built$panel[!GEOID %in% exposure$assignment$GEOID]
  write_table(geography[, .(GEOID, decade)], "geography_exclusions.csv")
  write_table(built$audit, "block_completeness.csv")
  write_table(built$panel, "county_decade_outcomes.csv")
  assignment <- copy(exposure$assignment)
  assignment[, observed_panel_years := 0L]
  availability <- annual[, .(observed_panel_years = .N), by = GEOID]
  assignment[availability, on = "GEOID", observed_panel_years := i.observed_panel_years]
  write_table(assignment, "treatment_assignment.csv")
  write_table(exposure$facilities, "facility_dates.csv")
  write_table(exposure$links, "county_facility_exposure_links.csv")
  write_table(sfd_timing_audit(assignment), "timing_exposure_audit.csv")
  write_table(annual[, .(counties = uniqueN(GEOID), n_amws = sum(n_amws),
    n_amws_early = sum(n_amws_1906_1955_dedup), n_amws_1986 = sum(n_amws_1986)), by = year][order(year)],
    "annual_outcome_coverage.csv")
  controls <- c("nevertreated", "notyettreated")
  timings <- c("standard_decade", "alternative_decade")
  balances <- c("unbalanced", "balanced")
  specs <- CJ(outcome = names(sfa_outcomes), timing = timings, control_group = controls, balance = balances)
  specs[, figure_file := mapply(sfd_figure_name, outcome, timing, control_group, balance)]
  write_table(specs, "figure_manifest.csv")
  specs[, `:=`(status = "pending", error = NA_character_)]
  all_support <- list(); models <- list()
  for (o in names(sfa_outcomes)) for (timing in timings) {
    d <- sfd_model_data(built$panel, assignment, o, timing)
    models[[paste(o, timing, sep = "__")]] <- d
    for (control in controls) {
      fit <- sfd_estimate(d, control, settings, estimate = FALSE)
      fit$cells[, `:=`(outcome = o, timing = timing, control_group = control)]
      all_support[[paste(o, timing, control, sep = "__")]] <- fit$cells
    }
  }
  write_table(rbindlist(all_support), "support_preflight.csv")
  writeLines(c("Complete calendar decades 1860-1869 through 1940-1949; no partial blocks or gap filling.",
    "Scientist and birth counts are ten-year sums; population is the annual mean; rates and log counts are recomputed.",
    paste0("Targets have exact first exposure <=", settings$target_cutoff,
      "; full list and 50km EPSG:3857 buffers define exposure."),
    "Both legacy timing conventions; shifted references may contain exposed years, documented in timing_exposure_audit.csv.",
    "Never-treated means unexposed through 1949. Not-yet-treated donors are unexposed through both block ends.",
    "Each ATT is an explicit did::att_gt two-period DR comparison, intercept only; donor-only counties never enter targets.",
    "Influence functions are aligned by county; cohort-size weight derivatives and common 1000 multiplier draws preserve joint inference.",
    paste0("Balanced assigned-decade cohorts have estimable cells at every decade from -10 through +",
      settings$balanced_post, "; counties within cohorts can vary."),
    "County annotations count the union of contributors; baseline pools unique county-reference observations equally.",
    "Pointwise 95% intervals. Annual population is interpolated; edition 1986 is not cross-deduplicated with earlier editions."),
    file.path(out_dir, "notes.txt"))
  if ("--preflight-only" %in% commandArgs(trailingOnly = TRUE)) {
    specs[, status := "preflight_only"]; write_table(specs, "model_status.csv")
    update_status("preflight_complete"); return(invisible(out_dir))
  }
  dependency_error <- tryCatch({suppressPackageStartupMessages(library(did)); NA_character_},
    error = function(e) conditionMessage(e))
  write_table(data.table(package = "did", loaded = is.na(dependency_error), error = dependency_error), "dependency_status.csv")
  if (!is.na(dependency_error)) {
    specs[, `:=`(status = "blocked_dependency", error = dependency_error)]
    write_table(specs, "model_status.csv"); update_status("blocked_dependency")
    stop("did cannot load; no estimates or replacement estimator used. ", dependency_error)
  }
  exports <- list(dynamic_att = list(), cohort_time_att = list(), post_att_summary = list(),
    cohort_membership = list(), support_estimation = list(), plot_annotation_summary = list(),
    plot_baseline_observations = list(), county_contributions = list(), donor_availability = list(),
    model_warnings = list(), aggregation_validation = list())
  failed_cells <- FALSE
  for (oi in seq_along(sfa_outcomes)) for (ti in seq_along(timings)) for (ci in seq_along(controls)) {
    o <- names(sfa_outcomes)[oi]; timing <- timings[ti]; control <- controls[ci]
    key <- paste(o, timing, control, sep = "__")
    cat("Estimating", key, "\n")
    settings$seed <- sfd_settings()$seed + oi * 100L + ti * 20L + ci * 10L
    d <- models[[paste(o, timing, sep = "__")]]
    fit <- sfd_estimate(d, control, settings)
    failed_cells <- failed_cells || any(fit$cells$status == "failed_estimation")
    for (part in c("cells", "warnings")) if (nrow(fit[[part]])) fit[[part]][,
      `:=`(outcome = o, timing = timing, control_group = control)]
    exports$support_estimation[[key]] <- fit$cells
    if (nrow(fit$warnings)) exports$model_warnings[[key]] <- fit$warnings
    for (balance_mode in balances) {
      result <- tryCatch(sfd_aggregate(fit, d, balance_mode, settings), error = function(e) e)
      if (inherits(result, "error")) {
        specs[outcome == o & timing == timings[ti] & control_group == control & balance == balance_mode,
          `:=`(status = "failed", error = conditionMessage(result))]
        next
      }
      rkey <- paste(key, balance_mode, sep = "__")
      balanced_grid <- seq(-10L, settings$balanced_post, by = 10L)
      membership <- fit$cells[event_time %in% balanced_grid,
        .(full_window_estimable = .N == length(balanced_grid) && all(estimable)), by = g]
      membership[, included := g %in% result$cohorts]
      result_exports <- list(dynamic_att = result$dynamic, cohort_time_att = result$cells,
        post_att_summary = result$summaries, cohort_membership = membership,
        plot_annotation_summary = result$annotation$summary, plot_baseline_observations = result$annotation$baseline,
        county_contributions = result$annotation$participants, donor_availability = result$donor_availability,
        aggregation_validation = result$dynamic[, .(event_time, estimate, manual_att,
                                                     difference = estimate - manual_att)])
      for (name in names(result_exports)) {
        x <- result_exports[[name]]
        x[, `:=`(outcome = o, timing = timing, control_group = control, balance = balance_mode)]
        exports[[name]][[rkey]] <- x
      }
      outcome_label <- if (o == "population") "Mean population (persons)" else
        if (o == "n_amws") "AMWS scientists born per decade" else
          if (o == "amws_per_100k") "Decade AMWS births per 100,000 mean population" else sfa_outcomes[[o]]
      figure_path <- file.path(out_dir, sfd_figure_name(o, timing, control, balance_mode))
      ggsave(figure_path,
        sfa_plot(result$dynamic, o, control, balance_mode, result$annotation$summary,
          frequency = "Ten-year", timing = timing, reference = -10L, outcome_label = outcome_label,
          target_cutoff = settings$target_cutoff, last_year = settings$last_year),
        width = 11, height = 6, dpi = 200, bg = "white")
      if (!file.exists(figure_path) || file.info(figure_path)$size == 0) stop("Figure export failed: ", figure_path)
      specs[outcome == o & timing == timings[ti] & control_group == control & balance == balance_mode, status := "estimated"]
    }
  }
  for (name in names(exports)) if (length(exports[[name]])) write_table(rbindlist(exports[[name]], fill = TRUE), paste0(name, ".csv"))
  write_table(specs, "model_status.csv")
  complete <- !failed_cells && all(specs$status == "estimated") &&
    length(list.files(out_dir, pattern = "^ES10_.*\\.png$")) == nrow(specs)
  update_status(if (complete) "complete" else "incomplete")
  if (!complete) stop("Incomplete run; inspect model_status.csv and support_estimation.csv in ", out_dir)
  cat("Completed", nrow(specs), "specifications in", out_dir, "\n")
  invisible(out_dir)
}

if (sys.nframe() == 0L) sfd_main()
