###############################################################################
# Attach UKDS 1831 socioeconomic controls to an alternative copy of the
# canonical UK historical urban-unit panel.
#
# This is deliberately a post-processing step. It does not rebuild or alter the
# canonical population series, inventor outcomes, target units, or production
# output. A UKDS demographic record contributes its full counts to a target only
# when at least 60% of its source geometry lies inside that target.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/03b_attach_ukds_1831_socioeconomic_controls.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(stringdist)
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

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
data_processed <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed")
output_dir <- file.path(
  data_processed, "worlds_fairs", "alternative_ukds"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

base_panel_filename <-
  "uk_historical_urban_units_inventor_panel_1801_1960_census_population.csv"
base_panel_candidates <- c(
  file.path(data_processed, base_panel_filename),
  file.path(data_processed, "worlds_fairs", base_panel_filename)
)
base_panel_file <- base_panel_candidates[file.exists(base_panel_candidates)][1L]
if (is.na(base_panel_file)) base_panel_file <- base_panel_candidates[[1L]]

target_file <- file.path(
  gbr_dir, "uk_historical_urban_units_1921_target_units.csv"
)
boundary_gpkg <- file.path(
  gbr_dir, "raw", "historical_boundaries",
  "uk_historical_districts_1921_1961.gpkg"
)
greater_london_crosswalk_file <- file.path(
  gbr_dir, "raw", "arcgis_english_admin_boundaries_1911",
  "greater_london_1911_to_nomis_1921_crosswalk.csv"
)

ukds_polygon_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "ukds_polygons")
ukds_demographic_dir <- file.path(
  TALENT_DETS_DATA_DIR, "raw", "ukds_demographics"
)

find_one_file <- function(path, pattern, label) {
  hits <- list.files(
    path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(hits) != 1L) {
    stop(
      "Expected exactly one ", label, " under ", path,
      "; found ", length(hits), ".\n",
      paste(hits, collapse = "\n")
    )
  }
  hits[[1L]]
}

ukds_polygon_file <- find_one_file(
  ukds_polygon_dir,
  "^1851EngWalesParishandPlace[.]shp$",
  "UKDS 1851 parish/place shapefile"
)
ukds_demographic_file <- find_one_file(
  ukds_demographic_dir,
  "^engwal[.]tab$",
  "UKDS Study 4961 England/Wales table"
)

controls_file <- file.path(
  output_dir, "uk_historical_urban_units_ukds_1831_controls.csv"
)
enriched_panel_file <- file.path(
  output_dir,
  "uk_historical_urban_units_inventor_panel_1801_1960_ukds_1831_controls.csv"
)
crosswalk_file <- file.path(
  output_dir, "ukds_1831_demographic_polygon_crosswalk.csv"
)
assignment_audit_file <- file.path(
  output_dir, "ukds_1831_city_assignment_audit.csv"
)
qc_file <- file.path(output_dir, "ukds_1831_controls_qc.csv")

required_files <- c(
  base_panel_file, target_file, boundary_gpkg,
  greater_london_crosswalk_file, ukds_polygon_file,
  ukds_demographic_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

overlap_threshold <- 0.60
target_types <- c("Urban District", "Municipal Borough", "County Borough")
greater_london_id <- "GBR_HIST_URBAN_GREATER_LONDON"

###############################################################################
# Helpers
###############################################################################

normalize_name <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("\\b(SAINT|ST)\\b", " ", x)
  x <- gsub(
    "\\b(PART OF|PARISH|TOWNSHIP|HAMLET|CHAPELRY|PLACE)\\b",
    " ", x
  )
  x <- gsub("[^A-Z0-9]", "", x)
  x[is.na(x)] <- ""
  x
}

normalize_polygon_registration <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^0-9a-z]", "", x)
  x[x == ""] <- NA_character_
  x
}

normalize_demographic_registration <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  whole <- floor(x[ok] + 1e-9)
  tenth <- round((x[ok] - whole) * 10)
  suffix <- fifelse(tenth == 1L, "a", fifelse(tenth == 2L, "b", ""))
  out[ok] <- paste0(whole, suffix)
  out
}

place_type_group <- function(x) {
  raw <- toupper(trimws(as.character(x)))
  fcase(
    raw %chin% c("P", "PARISH", "V", "VEP"), "parish",
    raw %chin% c("T", "TO", "TY", "TOWNSHIP", "TOS", "TN"), "township",
    raw %chin% c("H", "HAMLET"), "hamlet",
    raw %chin% c("C", "CH", "CHAPELRY", "PC", "PCH", "EPCH"), "chapelry",
    raw %chin% c("E", "EP", "EX", "EX-PARA", "EPC"), "extra_parochial",
    default = NA_character_
  )
}

polygon_type_group <- function(x) {
  raw <- toupper(trimws(as.character(x)))
  fcase(
    raw == "P", "parish",
    raw %chin% c("T", "Y"), "township",
    raw == "H", "hamlet",
    raw %chin% c("C", "PC"), "chapelry",
    raw == "EP", "extra_parochial",
    default = NA_character_
  )
}

sum_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

safe_ratio <- function(num, den, scale = 1) {
  out <- rep(NA_real_, length(num))
  ok <- is.finite(num) & is.finite(den) & den > 0
  out[ok] <- scale * num[ok] / den[ok]
  out
}

collapse_ids <- function(x) {
  paste(sort(unique(as.character(x))), collapse = ";")
}

###############################################################################
# Reconstruct and validate the canonical target geometry
###############################################################################

cat("Reconstructing canonical 1921 urban-unit geometry...\n")

districts_1921 <- st_read(
  boundary_gpkg, layer = "districts_1921", quiet = TRUE
)
districts_1921 <- st_make_valid(st_transform(districts_1921, 27700))

london_crosswalk <- fread(greater_london_crosswalk_file)
required_london_cols <- c("nomis_1921_id", "in_greater_london_1911_main")
if (!all(required_london_cols %chin% names(london_crosswalk))) {
  stop("Greater London crosswalk is missing required columns.")
}
london_boundary_ids <- london_crosswalk[
  in_greater_london_1911_main == TRUE,
  unique(as.character(nomis_1921_id))
]
if (length(london_boundary_ids) == 0L) {
  stop("Greater London crosswalk selects no 1921 boundaries.")
}

base_targets <- districts_1921[
  districts_1921$boundary_type %in% target_types &
    !(as.character(districts_1921$boundary_id) %in% london_boundary_ids),
]
base_targets_sf <- st_sf(
  target_unit_id = paste0("GBR_HIST_URBAN_", base_targets$boundary_id),
  target_unit_name = as.character(base_targets$boundary_name),
  geometry = st_geometry(base_targets)
)

london_components <- districts_1921[
  as.character(districts_1921$boundary_id) %in% london_boundary_ids,
]
if (nrow(london_components) != length(london_boundary_ids)) {
  stop("Some selected Greater London boundaries are missing from the GeoPackage.")
}
london_sf <- st_sf(
  target_unit_id = greater_london_id,
  target_unit_name = "Greater London",
  geometry = st_sfc(st_union(london_components), crs = 27700)
)
targets_sf <- st_make_valid(rbind(base_targets_sf, london_sf))
targets_sf$target_area_m2 <- as.numeric(st_area(targets_sf))

if (any(!is.finite(targets_sf$target_area_m2) |
        targets_sf$target_area_m2 <= 0)) {
  stop("Canonical target geometry contains non-positive areas.")
}
if (anyDuplicated(targets_sf$target_unit_id) > 0L) {
  stop("Canonical target geometry contains duplicate target IDs.")
}

target_export <- fread(target_file)
base_panel <- fread(base_panel_file, na.strings = c("", "NA"))
base_panel_original <- copy(base_panel)
base_columns <- names(base_panel_original)

required_panel_cols <- c("unit_id", "year", "population", "n_inventors", "n_stem")
missing_panel_cols <- setdiff(required_panel_cols, names(base_panel))
if (length(missing_panel_cols) > 0L) {
  stop(
    "Canonical panel is missing required columns:\n",
    paste(missing_panel_cols, collapse = "\n")
  )
}

panel_units <- sort(unique(base_panel$unit_id))
target_csv_units <- sort(unique(target_export$target_unit_id))
geometry_units <- sort(targets_sf$target_unit_id)
if (!identical(panel_units, target_csv_units) ||
    !identical(panel_units, geometry_units)) {
  stop("Panel, target CSV, and reconstructed geometry do not contain identical units.")
}

###############################################################################
# Read and validate the UKDS polygon and demographic sources
###############################################################################

cat("Reading UKDS 1851 polygons and 1831 socioeconomic data...\n")

ukds_sf <- st_read(ukds_polygon_file, quiet = TRUE)
required_polygon_cols <- c("ID", "CEN1", "CEN2", "CEN3", "CEN", "PAR", "PLA", "CAT")
missing_polygon_cols <- setdiff(required_polygon_cols, names(ukds_sf))
if (length(missing_polygon_cols) > 0L) {
  stop(
    "UKDS polygon file is missing required columns:\n",
    paste(missing_polygon_cols, collapse = "\n")
  )
}

ukds_sf <- st_make_valid(st_transform(ukds_sf, 27700))
ukds_sf$poly_row <- seq_len(nrow(ukds_sf))
ukds_sf$ukds_polygon_id <- as.integer(ukds_sf$ID)
ukds_sf$poly_area_m2 <- as.numeric(st_area(ukds_sf))
if (anyDuplicated(ukds_sf$ukds_polygon_id) > 0L ||
    any(st_is_empty(ukds_sf)) ||
    any(!is.finite(ukds_sf$poly_area_m2) | ukds_sf$poly_area_m2 <= 0)) {
  stop("UKDS polygons must have unique IDs and positive, nonempty geometry.")
}

polygon_dt <- as.data.table(st_drop_geometry(ukds_sf))[, .(
  poly_row,
  ukds_polygon_id,
  CEN = as.character(CEN),
  cen1_key = normalize_polygon_registration(CEN1),
  cen2 = suppressWarnings(as.integer(CEN2)),
  cen3 = suppressWarnings(as.integer(CEN3)),
  parish_name = as.character(PAR),
  place_name = as.character(PLA),
  parish_norm = normalize_name(PAR),
  place_norm = normalize_name(PLA),
  polygon_type = as.character(CAT),
  polygon_type_group = polygon_type_group(CAT),
  poly_area_m2
)]

demo <- fread(
  ukds_demographic_file,
  sep = "\t",
  na.strings = c("", "NA")
)
required_demo_cols <- c(
  "RGNUM", "SUBDIST", "COUNTY", "PLACNAME", "TYPEPLAC", "URBAN", "CITY",
  "AREA1831", "INHAB31", "FAM1831", "BLD1831", "UNINH31",
  "FAMAGRI", "FAMTRADE", "FAMOTH", "MAL1831", "FEM1831", "TOT1831",
  "MAL20PL", "OCCEMP", "AGIOCC", "AGLABS", "MANUFAC", "RETAIL",
  "CAPSETC", "LABOUR", "OTHMAL", "MSV20PL", "MSVTUN20", "FEMSERV"
)
missing_demo_cols <- setdiff(required_demo_cols, names(demo))
if (length(missing_demo_cols) > 0L) {
  stop(
    "UKDS demographic table is missing required columns:\n",
    paste(missing_demo_cols, collapse = "\n")
  )
}

demo[, `:=`(
  record_id = .I,
  cen1_key = normalize_demographic_registration(RGNUM),
  cen2 = suppressWarnings(as.integer(SUBDIST)),
  place_norm = normalize_name(PLACNAME),
  demographic_type_group = place_type_group(TYPEPLAC),
  population_identity_ok = as.numeric(TOT1831) ==
    as.numeric(MAL1831) + as.numeric(FEM1831),
  family_identity_ok = as.numeric(FAM1831) ==
    as.numeric(FAMAGRI) + as.numeric(FAMTRADE) + as.numeric(FAMOTH)
)]

source_to_output <- c(
  AREA1831 = "ukds_area_raw_1831",
  INHAB31 = "inhabited_houses_1831",
  FAM1831 = "families_1831",
  BLD1831 = "houses_under_construction_1831",
  UNINH31 = "uninhabited_houses_1831",
  FAMAGRI = "families_agriculture_1831",
  FAMTRADE = "families_trade_manufacturing_1831",
  FAMOTH = "families_other_1831",
  MAL1831 = "males_1831",
  FEM1831 = "females_1831",
  TOT1831 = "population_ukds_1831",
  MAL20PL = "males_20_plus_1831",
  OCCEMP = "occupiers_employing_labour_1831",
  AGIOCC = "occupiers_not_employing_labour_1831",
  AGLABS = "agricultural_labourers_1831",
  MANUFAC = "manufacturing_workers_1831",
  RETAIL = "retail_handicraft_masters_1831",
  CAPSETC = "capitalists_professionals_1831",
  LABOUR = "general_labourers_1831",
  OTHMAL = "other_males_20_plus_1831",
  MSV20PL = "male_servants_20_plus_1831",
  MSVTUN20 = "male_servants_under_20_1831",
  FEMSERV = "female_servants_1831"
)

negative_values <- rbindlist(lapply(names(source_to_output), function(col) {
  data.table(variable = col, negative_values = sum(as.numeric(demo[[col]]) < 0, na.rm = TRUE))
}))
for (source_col in names(source_to_output)) {
  output_col <- unname(source_to_output[[source_col]])
  values <- suppressWarnings(as.numeric(demo[[source_col]]))
  values[!is.finite(values) | values < 0] <- NA_real_
  demo[, (output_col) := values]
}

###############################################################################
# Match demographic records to UKDS source polygons
###############################################################################

cat("Matching demographic records to UKDS source geographies...\n")

polygon_names <- unique(rbindlist(list(
  polygon_dt[nzchar(place_norm), .(
    cen1_key, cen2, CEN, candidate_norm = place_norm,
    polygon_type_group, candidate_field = "PLA"
  )],
  polygon_dt[nzchar(parish_norm), .(
    cen1_key, cen2, CEN, candidate_norm = parish_norm,
    polygon_type_group, candidate_field = "PAR"
  )]
)))

crosswalk <- demo[, .(
  record_id,
  registration_number_raw = RGNUM,
  registration_subdistrict_raw = SUBDIST,
  cen1_key,
  cen2,
  county = as.character(COUNTY),
  demographic_place_name = as.character(PLACNAME),
  demographic_place_type = as.character(TYPEPLAC),
  demographic_type_group,
  place_norm,
  urban_code = as.integer(URBAN),
  city_code = as.integer(CITY),
  population_ukds_1831,
  population_identity_ok,
  family_identity_ok
)]
crosswalk[, `:=`(
  match_tier = NA_character_,
  matched_cen = NA_character_,
  match_score = NA_real_,
  runner_up_score = NA_real_,
  score_margin = NA_real_
)]

build_exact_matches <- function(field_value, remaining_ids) {
  names_one <- polygon_names[
    candidate_field == field_value,
    .(cen1_key, cen2, CEN, candidate_norm)
  ]
  candidates <- merge(
    demo[
      record_id %in% remaining_ids & cen2 > 0L & nzchar(place_norm),
      .(record_id, cen1_key, cen2, candidate_norm = place_norm)
    ],
    names_one,
    by = c("cen1_key", "cen2", "candidate_norm"),
    allow.cartesian = TRUE
  )
  if (nrow(candidates) == 0L) return(data.table())
  candidates[, .(
    n_cen = uniqueN(CEN),
    matched_cen = if (uniqueN(CEN) == 1L) unique(CEN) else NA_character_
  ), by = record_id][n_cen == 1L]
}

all_record_ids <- demo$record_id
exact_pla <- build_exact_matches("PLA", all_record_ids)
if (nrow(exact_pla) > 0L) {
  crosswalk[exact_pla, on = "record_id", `:=`(
    match_tier = "exact_pla",
    matched_cen = i.matched_cen,
    match_score = 1,
    runner_up_score = NA_real_,
    score_margin = NA_real_
  )]
}

remaining_ids <- crosswalk[is.na(match_tier), record_id]
exact_par <- build_exact_matches("PAR", remaining_ids)
if (nrow(exact_par) > 0L) {
  crosswalk[exact_par, on = "record_id", `:=`(
    match_tier = "exact_par",
    matched_cen = i.matched_cen,
    match_score = 1,
    runner_up_score = NA_real_,
    score_margin = NA_real_
  )]
}

remaining <- demo[
  record_id %in% crosswalk[is.na(match_tier), record_id] &
    cen2 > 0L & nzchar(place_norm),
  .(record_id, cen1_key, cen2, place_norm, demographic_type_group)
]
fuzzy_candidates <- merge(
  remaining,
  polygon_names,
  by = c("cen1_key", "cen2"),
  allow.cartesian = TRUE
)
if (nrow(fuzzy_candidates) > 0L) {
  fuzzy_candidates <- fuzzy_candidates[
    is.na(demographic_type_group) |
      is.na(polygon_type_group) |
      demographic_type_group == polygon_type_group
  ]
  fuzzy_candidates[, score := stringdist::stringsim(
    place_norm, candidate_norm, method = "jw", p = 0.1
  )]
  fuzzy_candidates <- fuzzy_candidates[, .(
    score = max(score, na.rm = TRUE)
  ), by = .(record_id, CEN)]
  setorder(fuzzy_candidates, record_id, -score, CEN)
  fuzzy_best <- fuzzy_candidates[, .(
    matched_cen = CEN[[1L]],
    match_score = score[[1L]],
    runner_up_score = if (.N > 1L) score[[2L]] else NA_real_,
    score_margin = if (.N > 1L) score[[1L]] - score[[2L]] else Inf
  ), by = record_id]
  fuzzy_best <- fuzzy_best[
    match_score >= 0.94 & (is.na(score_margin) | score_margin >= 0.05)
  ]
  if (nrow(fuzzy_best) > 0L) {
    crosswalk[fuzzy_best, on = "record_id", `:=`(
      match_tier = "fuzzy_unique",
      matched_cen = i.matched_cen,
      match_score = i.match_score,
      runner_up_score = i.runner_up_score,
      score_margin = i.score_margin
    )]
  }
}

available_subdistricts <- unique(polygon_dt[!is.na(cen1_key) & !is.na(cen2), .(
  cen1_key, cen2
)])
subdistrict_fallback <- merge(
  crosswalk[is.na(match_tier) & cen2 > 0L, .(record_id, cen1_key, cen2)],
  available_subdistricts,
  by = c("cen1_key", "cen2")
)
if (nrow(subdistrict_fallback) > 0L) {
  crosswalk[subdistrict_fallback, on = "record_id", match_tier := "subdistrict_fallback"]
}

available_districts <- unique(polygon_dt[!is.na(cen1_key), .(cen1_key)])
district_fallback <- merge(
  crosswalk[is.na(match_tier), .(record_id, cen1_key)],
  available_districts,
  by = "cen1_key"
)
if (nrow(district_fallback) > 0L) {
  crosswalk[district_fallback, on = "record_id", match_tier := "district_fallback"]
}
crosswalk[is.na(match_tier), match_tier := "unmatched_no_source_geometry"]

# Expand each demographic record to the UKDS polygons defining its source area.
record_polygons <- rbindlist(list(
  merge(
    crosswalk[match_tier %chin% c("exact_pla", "exact_par", "fuzzy_unique"),
              .(record_id, matched_cen)],
    polygon_dt[, .(poly_row, ukds_polygon_id, CEN, poly_area_m2)],
    by.x = "matched_cen", by.y = "CEN", allow.cartesian = TRUE
  )[, .(record_id, poly_row, ukds_polygon_id, poly_area_m2)],
  merge(
    crosswalk[match_tier == "subdistrict_fallback", .(record_id, cen1_key, cen2)],
    polygon_dt[, .(poly_row, ukds_polygon_id, cen1_key, cen2, poly_area_m2)],
    by = c("cen1_key", "cen2"), allow.cartesian = TRUE
  )[, .(record_id, poly_row, ukds_polygon_id, poly_area_m2)],
  merge(
    crosswalk[match_tier == "district_fallback", .(record_id, cen1_key)],
    polygon_dt[, .(poly_row, ukds_polygon_id, cen1_key, poly_area_m2)],
    by = "cen1_key", allow.cartesian = TRUE
  )[, .(record_id, poly_row, ukds_polygon_id, poly_area_m2)]
), use.names = TRUE)
record_polygons <- unique(record_polygons, by = c("record_id", "poly_row"))

source_geometry_summary <- record_polygons[, .(
  source_polygon_count = uniqueN(poly_row),
  source_polygon_ids = collapse_ids(ukds_polygon_id),
  source_area_m2 = sum(poly_area_m2)
), by = record_id]
crosswalk[source_geometry_summary, on = "record_id", `:=`(
  source_polygon_count = i.source_polygon_count,
  source_polygon_ids = i.source_polygon_ids,
  source_area_m2 = i.source_area_m2
)]

###############################################################################
# Apply the 60% UKDS-source-area overlap rule
###############################################################################

cat("Intersecting UKDS source polygons with canonical city targets...\n")

polygon_city_sf <- suppressWarnings(st_intersection(
  ukds_sf[, "poly_row"],
  targets_sf[, "target_unit_id"]
))
polygon_city <- as.data.table(st_drop_geometry(polygon_city_sf))
polygon_city[, intersection_area_m2 := as.numeric(st_area(polygon_city_sf))]
polygon_city <- polygon_city[
  is.finite(intersection_area_m2) & intersection_area_m2 > 0,
  .(poly_row, target_unit_id, intersection_area_m2)
]

record_city_overlap <- merge(
  record_polygons[, .(record_id, poly_row)],
  polygon_city,
  by = "poly_row",
  allow.cartesian = TRUE
)[, .(
  intersection_area_m2 = sum(intersection_area_m2)
), by = .(record_id, target_unit_id)]
record_city_overlap <- merge(
  record_city_overlap,
  source_geometry_summary[, .(record_id, source_area_m2)],
  by = "record_id"
)
record_city_overlap[, overlap_share := intersection_area_m2 / source_area_m2]
setorder(record_city_overlap, record_id, -overlap_share, target_unit_id)

qualifying_counts <- record_city_overlap[
  overlap_share >= overlap_threshold,
  .N,
  by = record_id
]
if (qualifying_counts[N > 1L, .N] > 0L) {
  stop(
    "Some UKDS demographic records meet the 60% source-area threshold for ",
    "more than one city."
  )
}

best_overlap <- record_city_overlap[, .SD[1L], by = record_id]
assignment <- merge(
  crosswalk[, .(record_id, match_tier, source_area_m2)],
  best_overlap[, .(
    record_id, best_target_unit_id = target_unit_id,
    best_intersection_area_m2 = intersection_area_m2,
    best_overlap_share = overlap_share
  )],
  by = "record_id",
  all.x = TRUE,
  sort = FALSE
)
assignment[, assignment_status := fcase(
  match_tier == "unmatched_no_source_geometry" | is.na(source_area_m2),
  "unmatched_no_source_geometry",
  is.na(best_target_unit_id), "no_city_overlap",
  best_overlap_share < overlap_threshold, "below_60pct_overlap",
  default = "assigned_full_counts"
)]
assignment[, assigned_target_unit_id := fifelse(
  assignment_status == "assigned_full_counts",
  best_target_unit_id,
  NA_character_
)]

assignment_audit <- merge(
  crosswalk,
  assignment[, .(
    record_id, best_target_unit_id, best_intersection_area_m2,
    best_overlap_share, assignment_status, assigned_target_unit_id
  )],
  by = "record_id",
  all.x = TRUE,
  sort = FALSE
)

###############################################################################
# Aggregate full counts and derive city-level controls
###############################################################################

cat("Aggregating assigned 1831 socioeconomic records...\n")

assigned_demo <- merge(
  demo,
  assignment[assignment_status == "assigned_full_counts", .(
    record_id, assigned_target_unit_id, best_overlap_share, match_tier
  )],
  by = "record_id"
)

raw_output_cols <- unname(source_to_output)
unit_raw <- assigned_demo[, lapply(.SD, sum_or_na),
                          by = assigned_target_unit_id,
                          .SDcols = raw_output_cols]

unit_provenance <- assigned_demo[, {
  pop <- population_ukds_1831
  total_pop <- sum(pop, na.rm = TRUE)
  weighted_overlap <- if (is.finite(total_pop) && total_pop > 0) {
    weighted.mean(best_overlap_share, pop, na.rm = TRUE)
  } else {
    mean(best_overlap_share, na.rm = TRUE)
  }
  tier_population <- function(tier) sum_or_na(pop[match_tier == tier])
  list(
    ukds_1831_source_records = .N,
    ukds_1831_mean_overlap_share = mean(best_overlap_share),
    ukds_1831_min_overlap_share = min(best_overlap_share),
    ukds_1831_population_weighted_overlap_share = weighted_overlap,
    ukds_1831_exact_population = sum_or_na(
      pop[match_tier %chin% c("exact_pla", "exact_par")]
    ),
    ukds_1831_fuzzy_population = tier_population("fuzzy_unique"),
    ukds_1831_subdistrict_fallback_population =
      tier_population("subdistrict_fallback"),
    ukds_1831_district_fallback_population =
      tier_population("district_fallback"),
    ukds_1831_population_from_urban_flag = sum_or_na(
      pop[as.integer(URBAN) == 1L]
    ),
    ukds_1831_population_from_city_flag = sum_or_na(
      pop[as.integer(CITY) > 0L]
    ),
    ukds_1831_population_identity_failures = sum(
      population_identity_ok == FALSE, na.rm = TRUE
    ),
    ukds_1831_family_identity_failures = sum(
      family_identity_ok == FALSE, na.rm = TRUE
    )
  )
}, by = assigned_target_unit_id]

controls <- merge(
  data.table(
    unit_id = targets_sf$target_unit_id,
    place_name = targets_sf$target_unit_name
  ),
  unit_raw,
  by.x = "unit_id",
  by.y = "assigned_target_unit_id",
  all.x = TRUE,
  sort = FALSE
)
controls <- merge(
  controls,
  unit_provenance,
  by.x = "unit_id",
  by.y = "assigned_target_unit_id",
  all.x = TRUE,
  sort = FALSE
)
controls[is.na(ukds_1831_source_records), ukds_1831_source_records := 0L]
controls[, `:=`(
  ukds_1831_overlap_threshold = overlap_threshold,
  ukds_1831_overlap_denominator = "ukds_source_geometry_area",
  ukds_1831_allocation_method = "full_counts_if_source_overlap_ge_0.60",
  ukds_1831_source = "UKDS Study 4961: 1831 Census Database"
)]

controls[, housing_stock_1831 :=
           inhabited_houses_1831 + uninhabited_houses_1831 +
             houses_under_construction_1831]
controls[, `:=`(
  male_share_1831 = safe_ratio(males_1831, population_ukds_1831),
  female_share_1831 = safe_ratio(females_1831, population_ukds_1831),
  persons_per_inhabited_house_1831 = safe_ratio(
    population_ukds_1831, inhabited_houses_1831
  ),
  families_per_inhabited_house_1831 = safe_ratio(
    families_1831, inhabited_houses_1831
  ),
  uninhabited_housing_share_1831 = safe_ratio(
    uninhabited_houses_1831, housing_stock_1831
  ),
  housing_under_construction_share_1831 = safe_ratio(
    houses_under_construction_1831, housing_stock_1831
  ),
  agriculture_family_share_1831 = safe_ratio(
    families_agriculture_1831, families_1831
  ),
  trade_manufacturing_family_share_1831 = safe_ratio(
    families_trade_manufacturing_1831, families_1831
  ),
  other_family_share_1831 = safe_ratio(families_other_1831, families_1831),
  occupiers_employing_labour_share_1831 = safe_ratio(
    occupiers_employing_labour_1831, males_20_plus_1831
  ),
  occupiers_not_employing_labour_share_1831 = safe_ratio(
    occupiers_not_employing_labour_1831, males_20_plus_1831
  ),
  agricultural_labourer_share_1831 = safe_ratio(
    agricultural_labourers_1831, males_20_plus_1831
  ),
  manufacturing_worker_share_1831 = safe_ratio(
    manufacturing_workers_1831, males_20_plus_1831
  ),
  retail_handicraft_master_share_1831 = safe_ratio(
    retail_handicraft_masters_1831, males_20_plus_1831
  ),
  capitalist_professional_share_1831 = safe_ratio(
    capitalists_professionals_1831, males_20_plus_1831
  ),
  general_labourer_share_1831 = safe_ratio(
    general_labourers_1831, males_20_plus_1831
  ),
  other_male_20_plus_share_1831 = safe_ratio(
    other_males_20_plus_1831, males_20_plus_1831
  ),
  male_servant_20_plus_share_1831 = safe_ratio(
    male_servants_20_plus_1831, males_20_plus_1831
  ),
  servants_per_1000_population_1831 = safe_ratio(
    male_servants_20_plus_1831 + male_servants_under_20_1831 +
      female_servants_1831,
    population_ukds_1831,
    scale = 1000
  ),
  ukds_1831_exact_population_share = safe_ratio(
    ukds_1831_exact_population, population_ukds_1831
  ),
  ukds_1831_fuzzy_population_share = safe_ratio(
    ukds_1831_fuzzy_population, population_ukds_1831
  ),
  ukds_1831_subdistrict_fallback_population_share = safe_ratio(
    ukds_1831_subdistrict_fallback_population, population_ukds_1831
  ),
  ukds_1831_district_fallback_population_share = safe_ratio(
    ukds_1831_district_fallback_population, population_ukds_1831
  ),
  ukds_1831_urban_population_share = safe_ratio(
    ukds_1831_population_from_urban_flag, population_ukds_1831
  ),
  ukds_1831_city_flag_population_share = safe_ratio(
    ukds_1831_population_from_city_flag, population_ukds_1831
  )
)]

setorder(controls, place_name, unit_id)

###############################################################################
# Join to a copy of the canonical panel and validate invariance
###############################################################################

cat("Joining controls to an alternative copy of the canonical panel...\n")

control_join_cols <- setdiff(names(controls), "place_name")
base_panel[, .ukds_row_order__ := .I]
enriched_panel <- merge(
  base_panel,
  controls[, ..control_join_cols],
  by = "unit_id",
  all.x = TRUE,
  sort = FALSE
)
setorder(enriched_panel, .ukds_row_order__)
enriched_panel[, .ukds_row_order__ := NULL]
base_panel[, .ukds_row_order__ := NULL]

if (nrow(enriched_panel) != nrow(base_panel_original)) {
  stop("Enrichment changed the canonical panel row count.")
}
for (col in base_columns) {
  if (!identical(enriched_panel[[col]], base_panel_original[[col]])) {
    stop("Enrichment changed pre-existing panel column: ", col)
  }
}
if (enriched_panel[, anyDuplicated(paste(unit_id, year))] > 0L) {
  stop("Enriched panel contains duplicate unit-year keys.")
}

new_panel_cols <- setdiff(names(enriched_panel), base_columns)
static_check <- enriched_panel[, lapply(.SD, uniqueN),
                               by = unit_id,
                               .SDcols = new_panel_cols]
if (static_check[, any(unlist(.SD, use.names = FALSE) != 1L),
                 .SDcols = new_panel_cols]) {
  stop("One or more UKDS controls vary within unit across panel years.")
}

share_cols <- grep("(_share_1831|_population_share)$", names(controls), value = TRUE)
for (col in share_cols) {
  bad <- controls[
    !is.na(get(col)) & (get(col) < -1e-9 | get(col) > 1 + 1e-9),
    .N
  ]
  if (bad > 0L) stop("Derived share outside [0,1]: ", col)
}
if (assignment[assignment_status == "assigned_full_counts",
               any(best_overlap_share < overlap_threshold - 1e-12)]) {
  stop("Assigned record below the required 60% source-area overlap.")
}

###############################################################################
# QC and outputs
###############################################################################

qc <- rbindlist(list(
  data.table(metric = "base_panel_rows", value = nrow(base_panel_original)),
  data.table(metric = "enriched_panel_rows", value = nrow(enriched_panel)),
  data.table(metric = "target_units", value = length(panel_units)),
  data.table(metric = "target_units_with_assigned_ukds_records",
             value = controls[ukds_1831_source_records > 0L, .N]),
  data.table(metric = "target_units_without_assigned_ukds_records",
             value = controls[ukds_1831_source_records == 0L, .N]),
  data.table(metric = "ukds_demographic_records", value = nrow(demo)),
  assignment[, .(value = .N), by = .(metric = paste0(
    "assignment_status_", assignment_status
  ))],
  crosswalk[, .(value = .N), by = .(metric = paste0("match_tier_", match_tier))],
  data.table(
    metric = "ukds_population_1831_all_records",
    value = sum(demo$population_ukds_1831, na.rm = TRUE)
  ),
  data.table(
    metric = "ukds_population_1831_assigned",
    value = sum(assigned_demo$population_ukds_1831, na.rm = TRUE)
  ),
  data.table(
    metric = "source_population_identity_failures",
    value = sum(demo$population_identity_ok == FALSE, na.rm = TRUE)
  ),
  data.table(
    metric = "source_family_identity_failures",
    value = sum(demo$family_identity_ok == FALSE, na.rm = TRUE)
  ),
  negative_values[, .(
    metric = paste0("negative_source_values_", variable),
    value = negative_values
  )]
), use.names = TRUE, fill = TRUE)

setorder(crosswalk, record_id)
setorder(assignment_audit, record_id)

fwrite(controls, controls_file)
fwrite(enriched_panel, enriched_panel_file)
fwrite(crosswalk, crosswalk_file)
fwrite(assignment_audit, assignment_audit_file)
fwrite(qc, qc_file)

cat("\nCompleted UKDS 1831 socioeconomic enrichment.\n")
cat("Controls: ", controls_file, "\n", sep = "")
cat("Alternative panel: ", enriched_panel_file, "\n", sep = "")
cat("Crosswalk: ", crosswalk_file, "\n", sep = "")
cat("Assignment audit: ", assignment_audit_file, "\n", sep = "")
cat("QC: ", qc_file, "\n", sep = "")
cat("Assigned demographic records: ",
    assignment[assignment_status == "assigned_full_counts", .N],
    " / ", nrow(assignment), "\n", sep = "")
cat("Target units with assigned records: ",
    controls[ukds_1831_source_records > 0L, .N],
    " / ", nrow(controls), "\n", sep = "")
