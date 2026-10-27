###############################################################################
# Build an area-weighted crosswalk between the UKDS 1911 registration-district
# and local-government-district boundaries for England and Wales.
#
# Inputs (discovered recursively below raw/ukds_polygons):
#   UKDA 9032: ew1911_regdistricts.shp
#   UKDA 9321: ew1911_lgdistricts.shp
#
# Outputs:
#   Data/processed/worlds_fairs/ukds_1911_boundary_crosswalk/
#   results/worlds_fair/ukds_1911_boundary_overlap/
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/03c_build_ukds_1911_registration_lgd_crosswalk.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(sf)
})

sf_use_s2(FALSE)

###############################################################################
# Paths and constants
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
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

ukds_root <- file.path(TALENT_DETS_DATA_DIR, "raw", "ukds_polygons")
processed_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "Data", "processed", "worlds_fairs", "ukds_1911_boundary_crosswalk"
)
figure_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results", "worlds_fair", "ukds_1911_boundary_overlap"
)

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

analysis_year <- 1911L
analysis_crs <- 27700L
relationship_threshold <- 0.95
coverage_lower <- 0.99
coverage_upper <- 1.01
expected_rd_features <- 637L
expected_lgd_features <- 1836L

crosswalk_file <- file.path(
  processed_dir, "ukds_1911_registration_lgd_area_crosswalk.csv"
)
rd_summary_file <- file.path(
  processed_dir, "ukds_1911_registration_overlap_summary.csv"
)
lgd_summary_file <- file.path(
  processed_dir, "ukds_1911_lgd_overlap_summary.csv"
)
qc_file <- file.path(
  processed_dir, "ukds_1911_boundary_overlap_qc.csv"
)
gpkg_file <- file.path(
  processed_dir, "ukds_1911_registration_lgd_overlap.gpkg"
)

find_one_file <- function(root, pattern, label) {
  hits <- list.files(
    root,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(hits) != 1L) {
    stop(
      "Expected exactly one ", label, " below ", root,
      "; found ", length(hits), ".\n",
      paste(hits, collapse = "\n")
    )
  }
  normalizePath(hits[[1L]], winslash = "/", mustWork = TRUE)
}

if (!dir.exists(ukds_root)) {
  stop("Missing UKDS polygon directory: ", ukds_root)
}

rd_file <- find_one_file(
  ukds_root, "^ew1911_regdistricts[.]shp$", "1911 registration-district shapefile"
)
lgd_file <- find_one_file(
  ukds_root, "^ew1911_lgdistricts[.]shp$", "1911 local-government-district shapefile"
)

###############################################################################
# Input validation and standardization
###############################################################################

required_columns <- c(
  "G_UNIT", "G_NAME", "UNITTYPE", "G_STATUS", "NATION", "G_YEAR",
  "IM_AUTH", "IM_NOTE", "NOTES"
)

read_boundary_layer <- function(path, prefix, expected_n) {
  x <- st_read(path, quiet = TRUE)
  missing_columns <- setdiff(required_columns, names(x))
  if (length(missing_columns) > 0L) {
    stop(
      basename(path), " is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (nrow(x) != expected_n) {
    stop(
      basename(path), " has ", nrow(x), " features; expected ", expected_n,
      " for the current UKDS release."
    )
  }
  years <- unique(as.integer(x$G_YEAR))
  if (length(years) != 1L || is.na(years) || years != analysis_year) {
    stop(
      basename(path), " must contain only ", analysis_year,
      " features; found: ", paste(years, collapse = ", ")
    )
  }
  if (is.na(st_crs(x))) {
    stop("Missing CRS in ", basename(path))
  }
  if (is.na(st_crs(x)$epsg) || st_crs(x)$epsg != analysis_crs) {
    stop(
      basename(path), " must use EPSG:", analysis_crs,
      "; found ", st_crs(x)$input
    )
  }

  x$source_row <- seq_len(nrow(x))
  x$geometry_valid_before <- st_is_valid(x)
  x$geometry_empty_before <- st_is_empty(x)
  x <- st_make_valid(x)
  x <- suppressWarnings(st_collection_extract(x, "POLYGON"))
  x <- suppressWarnings(st_cast(x, "MULTIPOLYGON"))

  if (any(!st_is_valid(x))) {
    stop("Geometry repair left invalid features in ", basename(path))
  }
  if (any(st_is_empty(x))) {
    stop("Geometry repair left empty features in ", basename(path))
  }

  official_id <- trimws(as.character(x$G_UNIT))
  missing_official_id <- is.na(official_id) | official_id == "" | official_id == "0"
  stable_id <- ifelse(
    missing_official_id,
    sprintf("%s_1911_ROW_%06d", prefix, x$source_row),
    paste0(prefix, "_GUNIT_", official_id)
  )
  if (anyDuplicated(stable_id)) {
    stop("Standardized IDs are not unique in ", basename(path))
  }

  x$official_g_unit <- official_id
  x$source_id_missing <- missing_official_id
  x$stable_id <- stable_id
  x$area_m2 <- as.numeric(st_area(x))
  if (any(!is.finite(x$area_m2) | x$area_m2 <= 0)) {
    stop("Non-positive or non-finite source areas in ", basename(path))
  }

  x
}

rd_raw <- read_boundary_layer(rd_file, "RD", expected_rd_features)
lgd_raw <- read_boundary_layer(lgd_file, "LGD", expected_lgd_features)

rd <- rd_raw[, c(
  "stable_id", "official_g_unit", "source_id_missing", "source_row",
  "G_NAME", "UNITTYPE", "G_STATUS", "NATION", "G_YEAR", "IM_AUTH",
  "IM_NOTE", "NOTES", "geometry_valid_before", "area_m2"
)]
names(rd)[names(rd) == "stable_id"] <- "rd_id"
names(rd)[names(rd) == "official_g_unit"] <- "rd_official_g_unit"
names(rd)[names(rd) == "source_id_missing"] <- "rd_source_id_missing"
names(rd)[names(rd) == "source_row"] <- "rd_source_row"
names(rd)[names(rd) == "G_NAME"] <- "rd_name"
names(rd)[names(rd) == "UNITTYPE"] <- "rd_unit_type"
names(rd)[names(rd) == "G_STATUS"] <- "rd_status"
names(rd)[names(rd) == "NATION"] <- "rd_nation"
names(rd)[names(rd) == "G_YEAR"] <- "rd_year"
names(rd)[names(rd) == "IM_AUTH"] <- "rd_im_auth"
names(rd)[names(rd) == "IM_NOTE"] <- "rd_im_note"
names(rd)[names(rd) == "NOTES"] <- "rd_notes"
names(rd)[names(rd) == "geometry_valid_before"] <- "rd_geometry_valid_before"
names(rd)[names(rd) == "area_m2"] <- "rd_area_m2"

lgd <- lgd_raw[, c(
  "stable_id", "official_g_unit", "source_id_missing", "source_row",
  "G_NAME", "UNITTYPE", "G_STATUS", "NATION", "G_YEAR", "IM_AUTH",
  "IM_NOTE", "NOTES", "geometry_valid_before", "area_m2"
)]
names(lgd)[names(lgd) == "stable_id"] <- "lgd_id"
names(lgd)[names(lgd) == "official_g_unit"] <- "lgd_official_g_unit"
names(lgd)[names(lgd) == "source_id_missing"] <- "lgd_source_id_missing"
names(lgd)[names(lgd) == "source_row"] <- "lgd_source_row"
names(lgd)[names(lgd) == "G_NAME"] <- "lgd_name"
names(lgd)[names(lgd) == "UNITTYPE"] <- "lgd_unit_type"
names(lgd)[names(lgd) == "G_STATUS"] <- "lgd_status"
names(lgd)[names(lgd) == "NATION"] <- "lgd_nation"
names(lgd)[names(lgd) == "G_YEAR"] <- "lgd_year"
names(lgd)[names(lgd) == "IM_AUTH"] <- "lgd_im_auth"
names(lgd)[names(lgd) == "IM_NOTE"] <- "lgd_im_note"
names(lgd)[names(lgd) == "NOTES"] <- "lgd_notes"
names(lgd)[names(lgd) == "geometry_valid_before"] <- "lgd_geometry_valid_before"
names(lgd)[names(lgd) == "area_m2"] <- "lgd_area_m2"

###############################################################################
# Area crosswalk
###############################################################################

message("Intersecting 1911 registration and local-government districts...")
intersections <- suppressWarnings(st_intersection(rd, lgd))
intersections$overlap_area_m2 <- as.numeric(st_area(intersections))
intersections <- intersections[
  is.finite(intersections$overlap_area_m2) & intersections$overlap_area_m2 > 0,
]

pair_dt <- as.data.table(st_drop_geometry(intersections))
pair_dt[, `:=`(
  rd_overlap_share = pmin(1, pmax(0, overlap_area_m2 / rd_area_m2)),
  lgd_overlap_share = pmin(1, pmax(0, overlap_area_m2 / lgd_area_m2))
)]

if (pair_dt[, anyDuplicated(paste(rd_id, lgd_id, sep = "||"))] > 0L) {
  stop("The overlay produced duplicate registration-district/LGD pairs.")
}
if (any(pair_dt$rd_overlap_share < 0 | pair_dt$rd_overlap_share > 1) ||
    any(pair_dt$lgd_overlap_share < 0 | pair_dt$lgd_overlap_share > 1)) {
  stop("Overlap shares fall outside [0, 1].")
}

setorder(pair_dt, rd_id, -rd_overlap_share, -overlap_area_m2, lgd_id)
pair_dt[, rd_match_rank := seq_len(.N), by = rd_id]
pair_dt[, is_primary_lgd_for_rd := rd_match_rank == 1L]

setorder(pair_dt, lgd_id, -lgd_overlap_share, -overlap_area_m2, rd_id)
pair_dt[, lgd_match_rank := seq_len(.N), by = lgd_id]
pair_dt[, is_primary_rd_for_lgd := lgd_match_rank == 1L]

pair_dt[, `:=`(
  rd_share_ge_0_1pct = rd_overlap_share >= 0.001,
  rd_share_ge_1pct = rd_overlap_share >= 0.01,
  rd_share_ge_5pct = rd_overlap_share >= 0.05,
  lgd_share_ge_0_1pct = lgd_overlap_share >= 0.001,
  lgd_share_ge_1pct = lgd_overlap_share >= 0.01,
  lgd_share_ge_5pct = lgd_overlap_share >= 0.05,
  sliver_both_lt_0_1pct = rd_overlap_share < 0.001 & lgd_overlap_share < 0.001,
  relationship = fcase(
    rd_overlap_share >= relationship_threshold &
      lgd_overlap_share >= relationship_threshold,
    "near_one_to_one",
    lgd_overlap_share >= relationship_threshold &
      rd_overlap_share < relationship_threshold,
    "lgd_nested_in_registration_district",
    rd_overlap_share >= relationship_threshold &
      lgd_overlap_share < relationship_threshold,
    "registration_district_nested_in_lgd",
    default = "boundary_crossing"
  )
)]

setorder(pair_dt, rd_id, rd_match_rank, lgd_id)

###############################################################################
# Unit summaries
###############################################################################

rd_base <- as.data.table(st_drop_geometry(rd))[, .(
  rd_id, rd_official_g_unit, rd_source_id_missing, rd_source_row,
  rd_name, rd_unit_type, rd_status, rd_nation, rd_year, rd_area_m2,
  rd_geometry_valid_before, rd_notes
)]
lgd_base <- as.data.table(st_drop_geometry(lgd))[, .(
  lgd_id, lgd_official_g_unit, lgd_source_id_missing, lgd_source_row,
  lgd_name, lgd_unit_type, lgd_status, lgd_nation, lgd_year, lgd_area_m2,
  lgd_geometry_valid_before, lgd_notes
)]

rd_matches <- pair_dt[, {
  normalized_weight <- overlap_area_m2 / sum(overlap_area_m2)
  primary <- .SD[rd_match_rank == 1L][1L]
  .(
    rd_total_overlap_area_m2 = sum(overlap_area_m2),
    rd_coverage_share = sum(overlap_area_m2) / first(rd_area_m2),
    rd_dominant_share = max(rd_overlap_share),
    rd_effective_lgd_count = 1 / sum(normalized_weight^2),
    rd_n_positive_lgd_matches = .N,
    rd_n_lgd_ge_0_1pct = sum(rd_share_ge_0_1pct),
    rd_n_lgd_ge_1pct = sum(rd_share_ge_1pct),
    rd_n_lgd_ge_5pct = sum(rd_share_ge_5pct),
    primary_lgd_id = primary$lgd_id,
    primary_lgd_name = primary$lgd_name,
    primary_lgd_status = primary$lgd_status,
    primary_relationship = primary$relationship
  )
}, by = rd_id]

lgd_matches <- pair_dt[, {
  normalized_weight <- overlap_area_m2 / sum(overlap_area_m2)
  primary <- .SD[lgd_match_rank == 1L][1L]
  .(
    lgd_total_overlap_area_m2 = sum(overlap_area_m2),
    lgd_coverage_share = sum(overlap_area_m2) / first(lgd_area_m2),
    lgd_dominant_share = max(lgd_overlap_share),
    lgd_effective_rd_count = 1 / sum(normalized_weight^2),
    lgd_n_positive_rd_matches = .N,
    lgd_n_rd_ge_0_1pct = sum(lgd_share_ge_0_1pct),
    lgd_n_rd_ge_1pct = sum(lgd_share_ge_1pct),
    lgd_n_rd_ge_5pct = sum(lgd_share_ge_5pct),
    primary_rd_id = primary$rd_id,
    primary_rd_name = primary$rd_name,
    primary_relationship = primary$relationship
  )
}, by = lgd_id]

rd_summary <- merge(rd_base, rd_matches, by = "rd_id", all.x = TRUE, sort = FALSE)
lgd_summary <- merge(lgd_base, lgd_matches, by = "lgd_id", all.x = TRUE, sort = FALSE)

rd_summary[is.na(rd_total_overlap_area_m2), `:=`(
  rd_total_overlap_area_m2 = 0,
  rd_coverage_share = 0,
  rd_dominant_share = 0,
  rd_effective_lgd_count = 0,
  rd_n_positive_lgd_matches = 0L,
  rd_n_lgd_ge_0_1pct = 0L,
  rd_n_lgd_ge_1pct = 0L,
  rd_n_lgd_ge_5pct = 0L
)]
lgd_summary[is.na(lgd_total_overlap_area_m2), `:=`(
  lgd_total_overlap_area_m2 = 0,
  lgd_coverage_share = 0,
  lgd_dominant_share = 0,
  lgd_effective_rd_count = 0,
  lgd_n_positive_rd_matches = 0L,
  lgd_n_rd_ge_0_1pct = 0L,
  lgd_n_rd_ge_1pct = 0L,
  lgd_n_rd_ge_5pct = 0L
)]

rd_summary[, `:=`(
  rd_uncovered_area_m2 = pmax(0, rd_area_m2 - rd_total_overlap_area_m2),
  rd_overlap_excess_area_m2 = pmax(0, rd_total_overlap_area_m2 - rd_area_m2),
  rd_coverage_flag = fcase(
    rd_coverage_share < coverage_lower, "below_99pct",
    rd_coverage_share > coverage_upper, "above_101pct",
    default = "within_99_101pct"
  )
)]
lgd_summary[, `:=`(
  lgd_uncovered_area_m2 = pmax(0, lgd_area_m2 - lgd_total_overlap_area_m2),
  lgd_overlap_excess_area_m2 = pmax(0, lgd_total_overlap_area_m2 - lgd_area_m2),
  lgd_coverage_flag = fcase(
    lgd_coverage_share < coverage_lower, "below_99pct",
    lgd_coverage_share > coverage_upper, "above_101pct",
    default = "within_99_101pct"
  ),
  lgd_crosses_registration_boundary = lgd_dominant_share < relationship_threshold
)]

setorder(rd_summary, rd_id)
setorder(lgd_summary, lgd_id)

if (nrow(rd_summary) != nrow(rd) || nrow(lgd_summary) != nrow(lgd)) {
  stop("Unit-summary row counts do not reconcile to the source layers.")
}

###############################################################################
# Global topology and QC
###############################################################################

message("Building topology diagnostics...")
rd_union <- st_make_valid(st_union(st_geometry(rd)))
lgd_union <- st_make_valid(st_union(st_geometry(lgd)))
rd_union_area_m2 <- as.numeric(st_area(rd_union))
lgd_union_area_m2 <- as.numeric(st_area(lgd_union))
common_area_m2 <- as.numeric(st_area(st_intersection(rd_union, lgd_union)))
rd_only_geometry <- suppressWarnings(st_difference(rd_union, lgd_union))
lgd_only_geometry <- suppressWarnings(st_difference(lgd_union, rd_union))

rd_only <- st_sf(
  diagnostic = "registration_not_covered_by_lgd",
  area_m2 = as.numeric(st_area(rd_only_geometry)),
  geometry = rd_only_geometry
)
lgd_only <- st_sf(
  diagnostic = "lgd_not_covered_by_registration",
  area_m2 = as.numeric(st_area(lgd_only_geometry)),
  geometry = lgd_only_geometry
)

qc <- rbindlist(list(
  data.table(section = "inputs", metric = c(
    "registration_file", "lgd_file", "analysis_year", "analysis_epsg"
  ), value = c(rd_file, lgd_file, analysis_year, analysis_crs)),
  data.table(section = "registration_districts", metric = c(
    "features", "missing_official_ids", "invalid_before_repair",
    "invalid_after_repair", "sum_feature_area_m2", "union_area_m2",
    "internal_overlap_excess_area_m2", "units_below_99pct_coverage",
    "units_above_101pct_coverage", "units_without_positive_intersection"
  ), value = as.character(c(
    nrow(rd), sum(rd$rd_source_id_missing), sum(!rd$rd_geometry_valid_before),
    sum(!st_is_valid(rd)), sum(rd$rd_area_m2), rd_union_area_m2,
    pmax(0, sum(rd$rd_area_m2) - rd_union_area_m2),
    sum(rd_summary$rd_coverage_share < coverage_lower),
    sum(rd_summary$rd_coverage_share > coverage_upper),
    sum(rd_summary$rd_n_positive_lgd_matches == 0L)
  ))),
  data.table(section = "local_government_districts", metric = c(
    "features", "missing_official_ids", "invalid_before_repair",
    "invalid_after_repair", "sum_feature_area_m2", "union_area_m2",
    "internal_overlap_excess_area_m2", "units_below_99pct_coverage",
    "units_above_101pct_coverage", "units_without_positive_intersection"
  ), value = as.character(c(
    nrow(lgd), sum(lgd$lgd_source_id_missing), sum(!lgd$lgd_geometry_valid_before),
    sum(!st_is_valid(lgd)), sum(lgd$lgd_area_m2), lgd_union_area_m2,
    pmax(0, sum(lgd$lgd_area_m2) - lgd_union_area_m2),
    sum(lgd_summary$lgd_coverage_share < coverage_lower),
    sum(lgd_summary$lgd_coverage_share > coverage_upper),
    sum(lgd_summary$lgd_n_positive_rd_matches == 0L)
  ))),
  data.table(section = "overlay", metric = c(
    "positive_area_pairs", "common_area_m2",
    "registration_union_covered_share", "lgd_union_covered_share",
    "registration_only_area_m2", "lgd_only_area_m2",
    "sliver_pairs_below_0_1pct_both_directions"
  ), value = as.character(c(
    nrow(pair_dt), common_area_m2,
    common_area_m2 / rd_union_area_m2,
    common_area_m2 / lgd_union_area_m2,
    as.numeric(st_area(rd_only_geometry)),
    as.numeric(st_area(lgd_only_geometry)),
    sum(pair_dt$sliver_both_lt_0_1pct)
  ))),
  pair_dt[, .N, by = relationship][
    , .(
      section = "relationship_counts",
      metric = relationship,
      value = as.character(N)
    )
  ]
), fill = TRUE)

qc[, status := "info"]
qc[section == "registration_districts" &
     metric == "invalid_after_repair" & value != "0", status := "fail"]
qc[section == "local_government_districts" &
     metric == "invalid_after_repair" & value != "0", status := "fail"]
qc[section == "registration_districts" &
     metric %chin% c("units_below_99pct_coverage", "units_above_101pct_coverage",
                     "units_without_positive_intersection") & value != "0",
   status := "review"]
qc[section == "local_government_districts" &
     metric %chin% c("units_below_99pct_coverage", "units_above_101pct_coverage",
                     "units_without_positive_intersection") & value != "0",
   status := "review"]

###############################################################################
# Write machine-readable outputs
###############################################################################

fwrite(pair_dt, crosswalk_file)
fwrite(rd_summary, rd_summary_file)
fwrite(lgd_summary, lgd_summary_file)
fwrite(qc, qc_file)

if (file.exists(gpkg_file)) invisible(file.remove(gpkg_file))

rd_export <- rd
rd_idx <- match(rd_export$rd_id, rd_summary$rd_id)
rd_export$rd_coverage_share <- rd_summary$rd_coverage_share[rd_idx]
rd_export$rd_dominant_share <- rd_summary$rd_dominant_share[rd_idx]
rd_export$rd_n_lgd_ge_1pct <- rd_summary$rd_n_lgd_ge_1pct[rd_idx]
rd_export$rd_coverage_flag <- rd_summary$rd_coverage_flag[rd_idx]

lgd_export <- lgd
lgd_idx <- match(lgd_export$lgd_id, lgd_summary$lgd_id)
lgd_export$lgd_coverage_share <- lgd_summary$lgd_coverage_share[lgd_idx]
lgd_export$lgd_dominant_share <- lgd_summary$lgd_dominant_share[lgd_idx]
lgd_export$lgd_n_rd_ge_1pct <- lgd_summary$lgd_n_rd_ge_1pct[lgd_idx]
lgd_export$lgd_crosses_registration_boundary <-
  lgd_summary$lgd_crosses_registration_boundary[lgd_idx]
lgd_export$lgd_coverage_flag <- lgd_summary$lgd_coverage_flag[lgd_idx]

pair_key <- paste(pair_dt$rd_id, pair_dt$lgd_id, sep = "||")
intersection_key <- paste(intersections$rd_id, intersections$lgd_id, sep = "||")
pair_idx <- match(intersection_key, pair_key)
intersections$rd_overlap_share <- pair_dt$rd_overlap_share[pair_idx]
intersections$lgd_overlap_share <- pair_dt$lgd_overlap_share[pair_idx]
intersections$rd_match_rank <- pair_dt$rd_match_rank[pair_idx]
intersections$lgd_match_rank <- pair_dt$lgd_match_rank[pair_idx]
intersections$relationship <- pair_dt$relationship[pair_idx]
intersections$sliver_both_lt_0_1pct <- pair_dt$sliver_both_lt_0_1pct[pair_idx]

st_write(rd_export, gpkg_file, layer = "registration_districts_1911", quiet = TRUE)
st_write(
  lgd_export, gpkg_file, layer = "local_government_districts_1911",
  quiet = TRUE, append = TRUE
)
st_write(
  intersections, gpkg_file, layer = "positive_area_intersections",
  quiet = TRUE, append = TRUE
)
st_write(
  rd_only, gpkg_file, layer = "registration_not_covered_by_lgd",
  quiet = TRUE, append = TRUE
)
st_write(
  lgd_only, gpkg_file, layer = "lgd_not_covered_by_registration",
  quiet = TRUE, append = TRUE
)

###############################################################################
# Diagnostic figures
###############################################################################

theme_set(theme_minimal(base_size = 11))

overlay_plot <- ggplot() +
  geom_sf(data = rd, fill = NA, color = "#4D4D4D", linewidth = 0.18) +
  geom_sf(data = lgd, fill = NA, color = "#2C7FB8", linewidth = 0.10, alpha = 0.8) +
  coord_sf(datum = NA) +
  labs(
    title = "UKDS 1911 administrative-boundary overlay",
    subtitle = "Registration districts (grey) and local-government districts (blue)",
    caption = "England and Wales; British National Grid (EPSG:27700)"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

ggsave(
  file.path(figure_dir, "ukds_1911_registration_lgd_boundary_overlay.png"),
  overlay_plot, width = 9, height = 11, dpi = 300, bg = "white"
)

rd_plot <- ggplot(rd_export) +
  geom_sf(aes(fill = rd_dominant_share), color = "grey75", linewidth = 0.08) +
  scale_fill_viridis_c(
    option = "C", limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%"),
    name = "Largest LGD\nshare of RD"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Registration-district fragmentation across 1911 LGDs",
    subtitle = "Lower values indicate that a registration district is split among more LGDs"
  ) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "right")

ggsave(
  file.path(figure_dir, "ukds_1911_registration_dominant_overlap.png"),
  rd_plot, width = 9, height = 11, dpi = 300, bg = "white"
)

lgd_export$boundary_relation <- ifelse(
  lgd_export$lgd_crosses_registration_boundary,
  "Crosses registration-district boundary",
  "At least 95% within one registration district"
)
lgd_plot <- ggplot(lgd_export) +
  geom_sf(aes(fill = boundary_relation), color = "grey75", linewidth = 0.08) +
  scale_fill_manual(
    values = c(
      "Crosses registration-district boundary" = "#D95F0E",
      "At least 95% within one registration district" = "#2C7FB8"
    ),
    name = NULL
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Local-government districts crossing registration boundaries",
    subtitle = "Classification uses the largest registration-district share of each LGD"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  )

ggsave(
  file.path(figure_dir, "ukds_1911_lgd_boundary_crossings.png"),
  lgd_plot, width = 9, height = 11, dpi = 300, bg = "white"
)

distribution_dt <- rbindlist(list(
  data.table(
    metric = "RD dominant LGD share (%)",
    value = 100 * rd_summary$rd_dominant_share
  ),
  data.table(
    metric = "LGD dominant RD share (%)",
    value = 100 * lgd_summary$lgd_dominant_share
  ),
  data.table(
    metric = "Material LGDs per RD (>=1% RD area)",
    value = rd_summary$rd_n_lgd_ge_1pct
  ),
  data.table(
    metric = "Material RDs per LGD (>=1% LGD area)",
    value = lgd_summary$lgd_n_rd_ge_1pct
  )
))

distribution_plot <- ggplot(distribution_dt, aes(x = value)) +
  geom_histogram(bins = 35, fill = "#2C7FB8", color = "white", linewidth = 0.2) +
  facet_wrap(~metric, scales = "free", ncol = 2) +
  labs(
    title = "UKDS 1911 boundary-overlap diagnostics",
    x = NULL,
    y = "Number of source units"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figure_dir, "ukds_1911_overlap_diagnostic_distributions.png"),
  distribution_plot, width = 11, height = 8, dpi = 300, bg = "white"
)

message("Completed UKDS 1911 registration/LGD boundary crosswalk.")
message("Crosswalk rows: ", format(nrow(pair_dt), big.mark = ","))
message("Registration districts: ", format(nrow(rd_summary), big.mark = ","))
message("Local-government districts: ", format(nrow(lgd_summary), big.mark = ","))
message("Processed outputs: ", processed_dir)
message("Diagnostic figures: ", figure_dir)
