###############################################################################
# Open the UK Law-Robson and Nomis population data for interactive inspection.
#
# Recommended use in RStudio, from any working directory:
#   source("prep/world_fairs_panel/07_open_uk_nomis_law_robson_data.R")
#
# Running with Rscript also works and prints summaries, but does not open View().
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

###############################################################################
# Paths
###############################################################################

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  candidate_roots <- c(
    Sys.getenv("GTL_REPO", unset = ""),
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  )
  candidate_roots <- candidate_roots[nzchar(candidate_roots)]
  path_hits <- candidate_roots[
    file.exists(file.path(candidate_roots, "paths.R"))
  ]
  if (length(path_hits) == 0L) {
    stop("Could not locate the repository root containing paths.R.")
  }
  repo_root <- normalizePath(path_hits[[1L]], winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "paths.R"))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users",
    Sys.info()[["user"]],
    "Globtalent Dropbox",
    "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(
      user_data_dir,
      winslash = "/",
      mustWork = TRUE
    )
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
DATA_PROCESSED <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed")
nomis_raw_dir <- file.path(gbr_dir, "raw", "nomis_historical_census")

files <- c(
  law_robson_raw = "law_robson_bennet_standard_csv.csv",
  law_robson_panel = "city_population_law_robson_bennett_1801_1911_geocoded.csv",
  nomis_panel = "city_population_nomis_1921_1961.csv",
  nomis_crosswalk = "law_robson_nomis_city_crosswalk.csv",
  nomis_audit = "law_robson_nomis_match_audit.csv",
  nomis_coverage = "nomis_city_population_coverage_summary.csv",
  combined_panel = "city_population_1801_1961_geocoded.csv",
  nomis_spatial_panel = "city_population_nomis_1921_1961_spatial.csv",
  nomis_spatial_crosswalk = "law_robson_nomis_spatial_crosswalk_1921_1961.csv",
  nomis_spatial_audit = "law_robson_nomis_spatial_match_audit_1921_1961.csv",
  nomis_spatial_coverage = "nomis_spatial_match_coverage_summary_1921_1961.csv",
  nomis_1911_1921_benchmark = "nomis_urban_units_1911_1921_benchmark.csv",
  nomis_1911_1921_quality_audit = "law_robson_nomis_1911_1921_quality_audit.csv",
  nomis_1911_1921_quality_summary = "law_robson_nomis_1911_1921_quality_summary.csv",
  nomis_1911_1921_quality_outliers = "law_robson_nomis_1911_1921_quality_outliers.csv",
  uk_lau_population_observed = "uk_lau_urban_population_census_1801_1961.csv",
  uk_lau_population_annual = "uk_lau_urban_population_census_1801_1961_annual.csv",
  uk_lau_allocation_audit = "uk_lau_urban_population_allocation_audit.csv",
  uk_lau_quality_summary = "uk_lau_urban_population_quality_summary.csv",
  uk_lau_transition_1911_1921 = "uk_lau_population_1911_1921_transition_audit.csv",
  uk_parish_feasibility = "uk_parish_population_feasibility_ukgeog.csv",
  uk_parish_nomis_lower_units = "uk_parish_population_nomis_lower_unit_counts.csv",
  ukgeog_available_boundary_levels = "ukgeog_available_boundary_levels.csv",
  uk_nomis_parish_population = "uk_nomis_parish_population_1921_1961.csv",
  uk_nomis_parish_related_units = "uk_nomis_parish_related_lower_units_1921_1961.csv",
  uk_nomis_parish_summary = "uk_nomis_parish_population_summary_1921_1961.csv"
)
file_paths <- setNames(file.path(gbr_dir, unname(files)), names(files))
inventor_panel_path <- file.path(
  DATA_PROCESSED, "uk_lau_inventor_panel_1801_1960_census_population.csv"
)
missing_files <- file_paths[!file.exists(file_paths)]
if (!file.exists(inventor_panel_path)) {
  missing_files <- c(missing_files, inventor_panel_path)
}
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

###############################################################################
# Main analysis objects
###############################################################################

law_robson_raw <- fread(file_paths[["law_robson_raw"]], na.strings = c("", "NA"))
law_robson_panel <- fread(
  file_paths[["law_robson_panel"]],
  na.strings = c("", "NA")
)
nomis_panel <- fread(file_paths[["nomis_panel"]], na.strings = c("", "NA"))
nomis_crosswalk <- fread(
  file_paths[["nomis_crosswalk"]],
  na.strings = c("", "NA")
)
nomis_audit <- fread(file_paths[["nomis_audit"]], na.strings = c("", "NA"))
nomis_coverage <- fread(
  file_paths[["nomis_coverage"]],
  na.strings = c("", "NA")
)
combined_panel <- fread(
  file_paths[["combined_panel"]],
  na.strings = c("", "NA")
)
nomis_spatial_panel <- fread(
  file_paths[["nomis_spatial_panel"]],
  na.strings = c("", "NA")
)
nomis_spatial_crosswalk <- fread(
  file_paths[["nomis_spatial_crosswalk"]],
  na.strings = c("", "NA")
)
nomis_spatial_audit <- fread(
  file_paths[["nomis_spatial_audit"]],
  na.strings = c("", "NA")
)
nomis_spatial_coverage <- fread(
  file_paths[["nomis_spatial_coverage"]],
  na.strings = c("", "NA")
)
nomis_1911_1921_benchmark <- fread(
  file_paths[["nomis_1911_1921_benchmark"]],
  na.strings = c("", "NA")
)
nomis_1911_1921_quality_audit <- fread(
  file_paths[["nomis_1911_1921_quality_audit"]],
  na.strings = c("", "NA")
)
nomis_1911_1921_quality_summary <- fread(
  file_paths[["nomis_1911_1921_quality_summary"]],
  na.strings = c("", "NA")
)
nomis_1911_1921_quality_outliers <- fread(
  file_paths[["nomis_1911_1921_quality_outliers"]],
  na.strings = c("", "NA")
)
uk_lau_population_observed <- fread(
  file_paths[["uk_lau_population_observed"]], na.strings = c("", "NA")
)
uk_lau_population_annual <- fread(
  file_paths[["uk_lau_population_annual"]], na.strings = c("", "NA")
)
uk_lau_allocation_audit <- fread(
  file_paths[["uk_lau_allocation_audit"]], na.strings = c("", "NA")
)
uk_lau_quality_summary <- fread(
  file_paths[["uk_lau_quality_summary"]], na.strings = c("", "NA")
)
uk_lau_transition_1911_1921 <- fread(
  file_paths[["uk_lau_transition_1911_1921"]], na.strings = c("", "NA")
)
uk_parish_feasibility <- fread(
  file_paths[["uk_parish_feasibility"]], na.strings = c("", "NA")
)
uk_parish_nomis_lower_units <- fread(
  file_paths[["uk_parish_nomis_lower_units"]], na.strings = c("", "NA")
)
ukgeog_available_boundary_levels <- fread(
  file_paths[["ukgeog_available_boundary_levels"]], na.strings = c("", "NA")
)
uk_nomis_parish_population <- fread(
  file_paths[["uk_nomis_parish_population"]], na.strings = c("", "NA")
)
uk_nomis_parish_related_units <- fread(
  file_paths[["uk_nomis_parish_related_units"]], na.strings = c("", "NA")
)
uk_nomis_parish_summary <- fread(
  file_paths[["uk_nomis_parish_summary"]], na.strings = c("", "NA")
)
uk_lau_inventor_panel <- fread(
  inventor_panel_path, na.strings = c("", "NA")
)

# Direct 1911-1921 comparison for cities observed in both sources.
comparison_1911_1921 <- merge(
  law_robson_panel[
    census_year == 1911L & population_available == TRUE,
    .(
      city_id,
      town_name,
      standard_name,
      historic_county,
      population_1911 = population
    )
  ],
  nomis_panel[
    census_year == 1921L & population_available == TRUE,
    .(
      city_id,
      population_1921 = population,
      source_area_id,
      source_area_name,
      source_area_type,
      source_county,
      match_method
    )
  ],
  by = "city_id"
)
comparison_1911_1921[, `:=`(
  population_change = population_1921 - population_1911,
  population_ratio = population_1921 / population_1911,
  population_change_pct = 100 * (population_1921 / population_1911 - 1)
)]
comparison_1911_1921 <- comparison_1911_1921[
  order(-abs(population_change_pct))
]

###############################################################################
# Original Nomis CR03 tables
###############################################################################

nomis_years <- c(1921L, 1931L, 1951L, 1961L)
nomis_raw <- setNames(lapply(nomis_years, function(year) {
  extracted_dir <- file.path(nomis_raw_dir, as.character(year), "extracted")
  candidates <- list.files(
    extracted_dir,
    pattern = sprintf("%s_cr03_values[.]csv$", year),
    recursive = TRUE,
    full.names = TRUE
  )
  candidates <- candidates[!grepl("__MACOSX", candidates, fixed = TRUE)]
  if (length(candidates) != 1L) {
    stop("Expected one extracted Nomis CR03 CSV for ", year, ".")
  }
  fread(candidates[[1L]], na.strings = c("", "NA", ".."))
}), as.character(nomis_years))

###############################################################################
# Console summary and RStudio Data Viewer
###############################################################################

cat("\nUK population data loaded into the following objects:\n")
object_summary <- data.table(
  object = c(
    "law_robson_raw",
    "law_robson_panel",
    "nomis_panel",
    "nomis_crosswalk",
    "nomis_audit",
    "nomis_coverage",
    "combined_panel",
    "nomis_spatial_panel",
    "nomis_spatial_crosswalk",
    "nomis_spatial_audit",
    "nomis_spatial_coverage",
    "nomis_1911_1921_benchmark",
    "nomis_1911_1921_quality_audit",
    "nomis_1911_1921_quality_summary",
    "nomis_1911_1921_quality_outliers",
    "uk_lau_population_observed",
    "uk_lau_population_annual",
    "uk_lau_allocation_audit",
    "uk_lau_quality_summary",
    "uk_lau_transition_1911_1921",
    "uk_parish_feasibility",
    "uk_parish_nomis_lower_units",
    "ukgeog_available_boundary_levels",
    "uk_nomis_parish_population",
    "uk_nomis_parish_related_units",
    "uk_nomis_parish_summary",
    "uk_lau_inventor_panel",
    "comparison_1911_1921",
    "nomis_raw"
  ),
  description = c(
    "Original wide Law-Robson table, 1801-1911",
    "Long, geocoded Law-Robson panel",
    "Matched Nomis city panel, 1921-1961",
    "Accepted city-to-Nomis-area matches",
    "All matched, ambiguous, and unmatched cases",
    "Coverage by census year",
    "Combined 1801-1961 city panel",
    "Spatially validated Nomis panel, 1921-1961",
    "Accepted spatial city-to-Nomis matches",
    "Spatial, textual, ambiguous, and unmatched audit",
    "Spatial match coverage by census year",
    "Nomis urban-unit benchmark reported for 1911 and 1921",
    "Law-Robson-to-Nomis transition quality audit",
    "Transition audit summary and decomposition",
    "High-priority transition audit cases",
    "Observed census urban population on GISCO 2019 LAUs",
    "Annual interpolated census urban population on GISCO 2019 LAUs",
    "Source-unit to LAU allocation audit",
    "LAU population coverage and conservation summary",
    "Direct LAU-level 1911-1921 transition audit",
    "Feasibility check for a ukgeog parish panel",
    "Nomis lower-level area counts by census year",
    "Boundary levels exposed by ukgeog metadata",
    "Observed Nomis primary parish population, 1921-1961",
    "Related parish/intersection units excluded from main parish table",
    "Summary of observed Nomis parish outputs",
    "Alternative England/Wales inventor panel with census population",
    "Cities observed in both 1911 and 1921",
    "Named list of original CR03 tables by year"
  ),
  rows = c(
    nrow(law_robson_raw),
    nrow(law_robson_panel),
    nrow(nomis_panel),
    nrow(nomis_crosswalk),
    nrow(nomis_audit),
    nrow(nomis_coverage),
    nrow(combined_panel),
    nrow(nomis_spatial_panel),
    nrow(nomis_spatial_crosswalk),
    nrow(nomis_spatial_audit),
    nrow(nomis_spatial_coverage),
    nrow(nomis_1911_1921_benchmark),
    nrow(nomis_1911_1921_quality_audit),
    nrow(nomis_1911_1921_quality_summary),
    nrow(nomis_1911_1921_quality_outliers),
    nrow(uk_lau_population_observed),
    nrow(uk_lau_population_annual),
    nrow(uk_lau_allocation_audit),
    nrow(uk_lau_quality_summary),
    nrow(uk_lau_transition_1911_1921),
    nrow(uk_parish_feasibility),
    nrow(uk_parish_nomis_lower_units),
    nrow(ukgeog_available_boundary_levels),
    nrow(uk_nomis_parish_population),
    nrow(uk_nomis_parish_related_units),
    nrow(uk_nomis_parish_summary),
    nrow(uk_lau_inventor_panel),
    nrow(comparison_1911_1921),
    sum(vapply(nomis_raw, nrow, integer(1L)))
  )
)
print(object_summary)

cat("\nNomis coverage:\n")
print(nomis_coverage)
cat("\nSpatial Nomis coverage:\n")
print(nomis_spatial_coverage)
cat("\nNomis 1911-1921 benchmark decomposition:\n")
print(nomis_1911_1921_quality_summary[section == "period_comparison"])

cat("\nExample commands:\n")
cat("  View(law_robson_raw)\n")
cat("  View(nomis_panel)\n")
cat("  View(nomis_spatial_panel)\n")
cat("  View(nomis_spatial_audit[spatial_match_accepted == FALSE])\n")
cat("  View(nomis_1911_1921_quality_audit)\n")
cat("  View(nomis_1911_1921_quality_outliers)\n")
cat("  View(uk_lau_population_observed)\n")
cat("  View(uk_lau_allocation_audit[boundary_link_needs_review == TRUE])\n")
cat("  View(uk_lau_transition_1911_1921)\n")
cat("  View(uk_parish_feasibility)\n")
cat("  View(uk_parish_nomis_lower_units)\n")
cat("  View(ukgeog_available_boundary_levels)\n")
cat("  View(uk_nomis_parish_population)\n")
cat("  View(uk_nomis_parish_summary)\n")
cat("  View(uk_lau_inventor_panel)\n")
cat("  View(comparison_1911_1921)\n")
cat("  View(nomis_audit[match_status != 'matched'])\n")
cat("  nomis_raw[['1921']][area == 'Manchester, City of']\n")

if (interactive()) {
  View(law_robson_raw, title = "Law-Robson raw population data, 1801-1911")
  View(nomis_panel, title = "Nomis matched city population data, 1921-1961")
}
