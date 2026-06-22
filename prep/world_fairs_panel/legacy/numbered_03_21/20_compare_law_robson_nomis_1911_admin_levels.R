###############################################################################
# Compare Law-Robson settlements with Nomis 1911 administrative units spatially.
#
# Purpose:
# - Assign 1911 Law-Robson settlement points to 1921 historical district
#   boundaries using point-in-polygon only.
# - Compare summed Law-Robson 1911 settlement population inside each boundary
#   with the Nomis 1911 benchmark population for that boundary.
# - Summarize fit by Nomis administrative type.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/20_compare_law_robson_nomis_1911_admin_levels.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
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
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
}

source(file.path(repo_root, "paths.R"))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(
      user_data_dir, winslash = "/", mustWork = TRUE
    )
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  }
}

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
boundary_gpkg_file <- file.path(
  gbr_dir,
  "raw",
  "historical_boundaries",
  "uk_historical_districts_1921_1961.gpkg"
)

law_file <- file.path(
  gbr_dir,
  "city_population_law_robson_bennett_1801_1911_geocoded.csv"
)
benchmark_file <- file.path(
  gbr_dir,
  "nomis_urban_units_1911_1921_benchmark.csv"
)

settlement_matches_file <- file.path(
  gbr_dir,
  "law_robson_nomis_1911_admin_level_settlement_matches.csv"
)
comparison_detail_file <- file.path(
  gbr_dir,
  "law_robson_nomis_1911_admin_level_comparison_detail.csv"
)
summary_file <- file.path(
  gbr_dir,
  "law_robson_nomis_1911_admin_level_summary.csv"
)
coverage_file <- file.path(
  gbr_dir,
  "law_robson_nomis_1911_admin_level_coverage.csv"
)
outliers_file <- file.path(
  gbr_dir,
  "law_robson_nomis_1911_admin_level_outliers.csv"
)

required_files <- c(law_file, benchmark_file, boundary_gpkg_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

###############################################################################
# Helpers
###############################################################################

safe_ratio <- function(numerator, denominator) {
  fifelse(is.na(denominator) | denominator == 0, NA_real_, numerator / denominator)
}

pct_diff <- function(numerator, denominator) {
  100 * (safe_ratio(numerator, denominator) - 1)
}

###############################################################################
# Load inputs
###############################################################################

message("Loading Law-Robson settlements...")
law <- fread(law_file)
law_1911_all <- law[census_year == 1911]
law_1911 <- law_1911_all[
  population_available == TRUE &
    !is.na(population) &
    !is.na(easting) &
    !is.na(northing)
]
law_1911[, law_row_id := .I]

message("Loading Nomis benchmark...")
benchmark <- fread(benchmark_file)
setnames(
  benchmark,
  old = c("source_area_id", "source_area_name", "source_area_type"),
  new = c("boundary_id", "nomis_area_name", "nomis_area_type")
)

message("Loading 1921 district boundaries...")
districts_sf <- st_read(
  boundary_gpkg_file,
  layer = "districts_1921",
  quiet = TRUE
)
districts_sf <- st_make_valid(districts_sf)
if (is.na(st_crs(districts_sf))) {
  st_crs(districts_sf) <- 27700
}
if (st_crs(districts_sf)$epsg != 27700) {
  districts_sf <- st_transform(districts_sf, 27700)
}

districts_dt <- as.data.table(st_drop_geometry(districts_sf))
districts_dt[, boundary_area_sqkm := as.numeric(st_area(districts_sf)) / 1e6]

message("Building Law-Robson settlement points...")
law_points_sf <- st_as_sf(
  law_1911,
  coords = c("easting", "northing"),
  crs = 27700,
  remove = FALSE
)

###############################################################################
# Spatial assignment
###############################################################################

message("Running point-in-polygon assignment...")
intersection_index <- st_intersects(law_points_sf, districts_sf, sparse = TRUE)

candidate_matches <- rbindlist(
  lapply(seq_along(intersection_index), function(i) {
    candidate_ids <- intersection_index[[i]]
    if (length(candidate_ids) == 0L) {
      return(data.table(law_row_id = i, boundary_candidate_row = NA_integer_))
    }
    data.table(law_row_id = i, boundary_candidate_row = candidate_ids)
  })
)

candidate_matches[, spatial_candidate_count := .N, by = law_row_id]

boundary_lookup <- districts_dt[
  ,
  .(
    boundary_candidate_row = .I,
    boundary_id,
    boundary_name,
    boundary_type,
    boundary_source,
    boundary_area_sqkm
  )
]

candidate_matches <- boundary_lookup[
  candidate_matches,
  on = "boundary_candidate_row"
]
candidate_matches <- benchmark[
  candidate_matches,
  on = "boundary_id"
]

candidate_matches[
  ,
  has_nomis_1911 := !is.na(nomis_population_1911)
]

candidate_matches[
  ,
  boundary_type_priority := fcase(
    boundary_type == "County Borough", 1L,
    boundary_type == "Metropolitan Borough", 2L,
    boundary_type == "County Corporate", 3L,
    boundary_type == "Municipal Borough", 4L,
    boundary_type == "Urban District", 5L,
    boundary_type == "Rural District", 6L,
    default = 7L
  )
]

setorder(
  candidate_matches,
  law_row_id,
  -has_nomis_1911,
  boundary_type_priority,
  boundary_area_sqkm,
  boundary_name
)

selected_matches <- candidate_matches[, .SD[1L], by = law_row_id]
selected_matches[
  ,
  spatial_match_status := fcase(
    is.na(boundary_id), "no_boundary_intersection",
    is.na(nomis_population_1911), "boundary_not_in_nomis_benchmark",
    spatial_candidate_count > 1L, "matched_nomis_1911_ambiguous_boundary_selected",
    default = "matched_nomis_1911"
  )
]
selected_matches[
  ,
  selected_boundary_rule := fcase(
    is.na(boundary_id), "no_boundary",
    spatial_candidate_count > 1L,
    "selected_candidate_with_nomis_then_admin_priority_then_smallest_area",
    default = "single_spatial_candidate"
  )
]

law_keep_cols <- c(
  "law_row_id",
  "city_id",
  "town_name",
  "standard_name",
  "historic_county",
  "census_year",
  "population",
  "population_available",
  "longitude",
  "latitude",
  "easting",
  "northing",
  "geocode_status",
  "geocode_method",
  "point_id",
  "settlement_point_name",
  "settlement_point_county"
)
law_keep_cols <- intersect(law_keep_cols, names(law_1911))

settlement_matches <- selected_matches[
  law_1911[, ..law_keep_cols],
  on = "law_row_id"
]
setnames(settlement_matches, "population", "law_robson_population_1911")

settlement_matches[
  ,
  `:=`(
    law_to_nomis_ratio = safe_ratio(
      law_robson_population_1911,
      nomis_population_1911
    ),
    law_to_nomis_pct_diff = pct_diff(
      law_robson_population_1911,
      nomis_population_1911
    )
  )
]
settlement_matches[, law_to_nomis_abs_pct_diff := abs(law_to_nomis_pct_diff)]

###############################################################################
# Boundary-level comparison
###############################################################################

matched_settlements <- settlement_matches[
  spatial_match_status %chin% c(
    "matched_nomis_1911",
    "matched_nomis_1911_ambiguous_boundary_selected"
  )
]

comparison_detail <- matched_settlements[
  ,
  .(
    law_robson_settlement_count = .N,
    law_robson_city_ids = paste(sort(unique(city_id)), collapse = ";"),
    law_robson_town_names = paste(sort(unique(town_name)), collapse = ";"),
    law_robson_standard_names = paste(sort(unique(standard_name)), collapse = ";"),
    law_robson_historic_counties = paste(
      sort(unique(historic_county)),
      collapse = ";"
    ),
    law_robson_population_1911 = sum(law_robson_population_1911, na.rm = TRUE),
    representative_longitude = mean(longitude, na.rm = TRUE),
    representative_latitude = mean(latitude, na.rm = TRUE),
    representative_easting = mean(easting, na.rm = TRUE),
    representative_northing = mean(northing, na.rm = TRUE),
    max_spatial_candidate_count = max(spatial_candidate_count, na.rm = TRUE)
  ),
  by = .(
    boundary_id,
    boundary_name,
    boundary_type,
    boundary_source,
    nomis_area_name,
    nomis_area_type,
    nomis_population_1911,
    nomis_population_1921,
    nomis_growth_ratio_1911_1921,
    nomis_growth_pct_1911_1921,
    nomis_log_growth_1911_1921,
    nomis_growth_outlier,
    boundary_area_sqkm
  )
]

comparison_detail[
  ,
  `:=`(
    law_to_nomis_ratio = safe_ratio(
      law_robson_population_1911,
      nomis_population_1911
    ),
    law_to_nomis_pct_diff = pct_diff(
      law_robson_population_1911,
      nomis_population_1911
    ),
    law_minus_nomis_population = law_robson_population_1911 -
      nomis_population_1911
  )
]
comparison_detail[, law_to_nomis_abs_pct_diff := abs(law_to_nomis_pct_diff)]
comparison_detail[
  ,
  multiple_law_robson_settlements_in_boundary := law_robson_settlement_count > 1L
]

setorder(
  comparison_detail,
  nomis_area_type,
  -law_to_nomis_abs_pct_diff,
  boundary_name
)

###############################################################################
# Summaries and outputs
###############################################################################

benchmark_summary <- benchmark[
  ,
  .(
    nomis_units_total = .N,
    nomis_population_1911_total = sum(nomis_population_1911, na.rm = TRUE)
  ),
  by = nomis_area_type
]

summary_by_type <- comparison_detail[
  ,
  .(
    nomis_units_with_law_robson = .N,
    law_robson_settlements = sum(law_robson_settlement_count),
    multi_settlement_nomis_units = sum(
      multiple_law_robson_settlements_in_boundary
    ),
    law_robson_population_1911_total = sum(
      law_robson_population_1911,
      na.rm = TRUE
    ),
    nomis_population_1911_total_matched_units = sum(
      nomis_population_1911,
      na.rm = TRUE
    ),
    total_law_to_nomis_ratio = safe_ratio(
      sum(law_robson_population_1911, na.rm = TRUE),
      sum(nomis_population_1911, na.rm = TRUE)
    ),
    median_unit_law_to_nomis_ratio = median(
      law_to_nomis_ratio,
      na.rm = TRUE
    ),
    median_abs_pct_diff = median(law_to_nomis_abs_pct_diff, na.rm = TRUE),
    p75_abs_pct_diff = as.numeric(
      quantile(law_to_nomis_abs_pct_diff, 0.75, na.rm = TRUE)
    ),
    p90_abs_pct_diff = as.numeric(
      quantile(law_to_nomis_abs_pct_diff, 0.90, na.rm = TRUE)
    ),
    share_units_within_10pct = mean(
      law_to_nomis_abs_pct_diff <= 10,
      na.rm = TRUE
    ),
    share_units_within_25pct = mean(
      law_to_nomis_abs_pct_diff <= 25,
      na.rm = TRUE
    ),
    outlier_units_gt25pct = sum(law_to_nomis_abs_pct_diff > 25, na.rm = TRUE),
    outlier_units_gt50pct = sum(law_to_nomis_abs_pct_diff > 50, na.rm = TRUE)
  ),
  by = nomis_area_type
]

summary_by_type <- summary_by_type[
  benchmark_summary,
  on = "nomis_area_type"
]
zero_if_missing_cols <- c(
  "nomis_units_with_law_robson",
  "law_robson_settlements",
  "multi_settlement_nomis_units",
  "nomis_population_1911_total_matched_units",
  "law_robson_population_1911_total",
  "outlier_units_gt25pct",
  "outlier_units_gt50pct"
)
for (col in zero_if_missing_cols) {
  set(
    summary_by_type,
    i = which(is.na(summary_by_type[[col]])),
    j = col,
    value = 0
  )
}
summary_by_type[
  ,
  `:=`(
    share_nomis_units_with_law_robson = safe_ratio(
      nomis_units_with_law_robson,
      nomis_units_total
    ),
    share_nomis_population_1911_in_matched_units = safe_ratio(
      nomis_population_1911_total_matched_units,
      nomis_population_1911_total
    )
  )
]

setcolorder(
  summary_by_type,
  c(
    "nomis_area_type",
    "nomis_units_total",
    "nomis_units_with_law_robson",
    "share_nomis_units_with_law_robson",
    "law_robson_settlements",
    "multi_settlement_nomis_units",
    "nomis_population_1911_total",
    "nomis_population_1911_total_matched_units",
    "share_nomis_population_1911_in_matched_units",
    "law_robson_population_1911_total",
    "total_law_to_nomis_ratio"
  )
)
setorder(summary_by_type, median_abs_pct_diff)

law_coverage <- data.table(
  coverage_group = c(
    "law_robson_1911_rows",
    "law_robson_1911_population_available",
    "law_robson_1911_population_and_coordinates",
    "law_robson_1911_spatially_matched_to_nomis_benchmark"
  ),
  n = c(
    nrow(law_1911_all),
    nrow(law_1911_all[population_available == TRUE & !is.na(population)]),
    nrow(law_1911),
    nrow(matched_settlements)
  )
)

status_coverage <- settlement_matches[
  ,
  .(
    n_law_robson_settlements = .N,
    law_robson_population_1911 = sum(law_robson_population_1911, na.rm = TRUE)
  ),
  by = spatial_match_status
]
status_coverage[, coverage_group := paste0("status_", spatial_match_status)]
status_coverage <- status_coverage[
  ,
  .(
    coverage_group,
    n = n_law_robson_settlements,
    law_robson_population_1911
  )
]

type_coverage <- settlement_matches[
  !is.na(boundary_type),
  .(
    n_law_robson_settlements = .N,
    law_robson_population_1911 = sum(law_robson_population_1911, na.rm = TRUE)
  ),
  by = boundary_type
]
type_coverage[, coverage_group := paste0("boundary_type_", boundary_type)]
type_coverage <- type_coverage[
  ,
  .(
    coverage_group,
    n = n_law_robson_settlements,
    law_robson_population_1911
  )
]

coverage <- rbindlist(
  list(
    law_coverage[, law_robson_population_1911 := NA_real_],
    status_coverage,
    type_coverage
  ),
  fill = TRUE
)

outliers <- comparison_detail[
  law_to_nomis_abs_pct_diff > 25 |
    multiple_law_robson_settlements_in_boundary == TRUE |
    max_spatial_candidate_count > 1L
]
setorder(outliers, -law_to_nomis_abs_pct_diff, boundary_name)

message("Writing outputs...")
fwrite(settlement_matches, settlement_matches_file)
fwrite(comparison_detail, comparison_detail_file)
fwrite(summary_by_type, summary_file)
fwrite(coverage, coverage_file)
fwrite(outliers, outliers_file)

message("Done.")
message("Settlement matches: ", settlement_matches_file)
message("Boundary-level comparison: ", comparison_detail_file)
message("Summary by administrative type: ", summary_file)
message("Coverage: ", coverage_file)
message("Outliers: ", outliers_file)

message("\nCoverage:")
print(coverage)

message("\nSummary by administrative type:")
print(summary_by_type)
