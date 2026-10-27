###############################################################################
# Backcast census population to fixed 1911 local-government districts (LGDs)
# using the calibrated 1911 registration-district (RD) x LGD density surface.
#
# Directly reconstructed years: 1851, 1861, 1871, 1891, 1901.
# Interpolated year: 1881, linearly in levels between 1871 and 1891.
# Validation year: 1911, using the calibrated/adjusted RD margins.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/03e_build_ukds_historical_lgd_population_1851_1901.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
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
    winslash = "/", mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/", mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/", mustWork = TRUE
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
  }
}

ukds_polygon_root <- file.path(
  TALENT_DETS_DATA_DIR, "raw", "ukds_polygons", "UKDA-9032-xml", "xml"
)
rsd_population_root <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "raw", "world_fairs", "ukds_birth_data",
  "UKDA-8613-csv", "csv"
)
processed_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "Data", "processed", "worlds_fairs", "ukds_1911_boundary_crosswalk"
)

overlap_gpkg <- file.path(
  processed_dir, "ukds_1911_registration_lgd_overlap.gpkg"
)
population_crosswalk_file <- file.path(
  processed_dir, "ukds_1911_lgd_rd_population_crosswalk.csv"
)
calibration_audit_file <- file.path(
  processed_dir, "ukds_1911_lgd_calibrated_population_audit.csv"
)

backcast_file <- file.path(
  processed_dir, "ukds_1911_lgd_population_backcast_1851_1901.csv"
)
weights_file <- file.path(
  processed_dir, "ukds_historical_rd_to_1911_lgd_weights_1851_1901.csv"
)
match_audit_file <- file.path(
  processed_dir, "ukds_historical_rd_population_match_audit_1851_1901.csv"
)
qc_file <- file.path(
  processed_dir, "ukds_historical_lgd_population_backcast_qc.csv"
)
baseline_covariate_file <- file.path(
  processed_dir, "ukds_1911_lgd_baseline_covariates_1851.csv"
)
baseline_rd_audit_file <- file.path(
  processed_dir, "ukds_1851_rd_baseline_covariate_audit.csv"
)
baseline_covariate_qc_file <- file.path(
  processed_dir, "ukds_1911_lgd_baseline_covariates_1851_qc.csv"
)

direct_years <- c(1851L, 1861L, 1871L, 1891L, 1901L)
validation_year <- 1911L
baseline_year <- 1851L
baseline_minimum_coverage <- 0.95
analysis_crs <- 27700L
tolerance <- 1e-8
baseline_source_variables <- c(
  paste0("HC", 1:5), paste0("SC", 1:8), "C_TEACHER"
)
baseline_output_variables <- c(
  paste0("baseline_hc", 1:5, "_pct"),
  paste0("baseline_sc", 1:8, "_pct"),
  "baseline_children_per_teacher"
)

required_files <- c(
  overlap_gpkg, population_crosswalk_file, calibration_audit_file,
  file.path(rsd_population_root, paste0(c(direct_years, validation_year),
                                         "_rsd_data.csv")),
  file.path(
    ukds_polygon_root,
    paste0("ukds_ew", c(direct_years, validation_year), "_regdistricts"),
    paste0("ew", c(direct_years, validation_year), "_regdistricts.shp")
  )
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required inputs:\n", paste(missing_files, collapse = "\n"))
}

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Helpers and reviewed historical RD name mappings
###############################################################################

normalize_name <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

relative_error <- function(actual, target) {
  abs(actual - target) / pmax(abs(target), 1)
}

assert_unique <- function(x, columns, label) {
  if (anyDuplicated(x[, ..columns])) {
    stop(label, " is not unique by: ", paste(columns, collapse = ", "))
  }
}

# Each G_UNIT can have multiple candidate spellings because the population
# files themselves change spelling over time. Only a candidate present in the
# given year's RD population table is selected.
alias_candidates <- data.table(
  g_unit = c(
    10005681, 10139533, 10030195, 10066694, 10112394, 10139399,
    10167516, 10167516, 10174820, 10155265, 10155265, 10030110,
    10155058, 10155058, 10112138, 10194118, 10194118, 10187679,
    10167590, 10154923, 10030171, 10139211, 10029650, 10029879,
    10155149, 10178897, 10029569, 10087673, 10055805, 10174739,
    10111833, 10154730, 10029624, 10063292, 10139648, 10081749,
    10005280, 10112035, 10055714, 10174636, 10081804, 10081804,
    10044236,
    10063206, 10075427, 10063190, 10187825, 10055726, 10005220,
    10005218, 10210549, 10111900, 10055740, 10155022, 10174788,
    10005735, 10085690, 10043839, 10187758, 10484420, 10111973,
    10043815, 10099596
  ),
  population_rd_name = c(
    "CRICKHOWELL", "PONTYPOOL", "BRIDGEND", "NEWTOWN", "ABERAYRON",
    "LLANDILOFAWR", "MERTHYR TYDFIL", "MERTHYR-TYDFIL", "DOLGELLEY",
    "ABERYSTWYTH", "ABRYSTWITH", "ST COLUMB", "BISHOP'S STORTFORD",
    "BISHOP STORTFORD", "CRICKLADE", "SWINDON", "HIGHWORTH", "MITFORD",
    "ST FAITH'S", "KING'S LYNN", "BOSMERE", "UPTON ON SEVERN",
    "WINCHCOMB", "LEXDEN", "GRAVESEND", "MELKSHAM", "THAKEHAM",
    "FLEGG", "MUTFORD", "LODDON", "HUNGERFORD", "ST GILES",
    "WESTBURY", "LONDON CITY", "ST GEORGE HANOVER SQUARE",
    "ST OLAVE (BERMONDSEY)", "WAREHAM", "BANGOR", "WOLSTANTON",
    "STOKE UPON TRENT", "FFESTINIOG", "FESTINOG", "ALSTON",
    "SCILLY ISLANDS", "WEOBLY", "HOLLINGBOURN", "WORTHING",
    "ST JAMES WESTMINSTER", "ST SAVIOUR SOUTHWARK", "WESTMINSTER",
    "ST GEORGE SOUTHWARK", "NEWINGTON", "TUNBRIDGE", "AYSGARTH",
    "ASHBORNE", "GUISBROUGH", "TUNSTEAD", "BOURN", "BEDWELTY",
    "KIRK DEIGHTON", "SHIFFNAL", "WORKSHOP", "ALDERBURY"
  )
)
alias_candidates[, population_name_key := normalize_name(population_rd_name)]
alias_candidates <- unique(
  alias_candidates,
  by = c("g_unit", "population_name_key")
)

duplicate_name_overrides <- data.table(
  g_unit = c(
    10104129, 10201482, 10167530, 10043967,
    10139107, 10187564, 10084404, 10075439
  ),
  rd_regcnty = c(
    "MONMOUTHSHIRE", "SHROPSHIRE", "YORKSHIRE NORTH RIDING", "SURREY",
    "SOMERSET", "SHROPSHIRE", "SHROPSHIRE", "HAMPSHIRE"
  ),
  rd_regdist = c(
    "NEWPORT", "NEWPORT", "RICHMOND", "RICHMOND",
    "WELLINGTON", "WELLINGTON", "WHITCHURCH", "WHITCHURCH"
  )
)
duplicate_name_overrides[, rd_pop_key := paste(
  rd_regcnty, rd_regdist, sep = " | "
)]

build_historical_rd_match <- function(year) {
  boundary_file <- file.path(
    ukds_polygon_root, paste0("ukds_ew", year, "_regdistricts"),
    paste0("ew", year, "_regdistricts.shp")
  )
  population_file <- file.path(
    rsd_population_root, paste0(year, "_rsd_data.csv")
  )

  boundary_sf <- st_read(boundary_file, quiet = TRUE)
  boundary_sf <- st_transform(st_make_valid(boundary_sf), analysis_crs)
  boundary_sf$historical_rd_row <- seq_len(nrow(boundary_sf))
  boundary_sf$historical_rd_id <- sprintf(
    "RD_%d_ROW_%06d", year, boundary_sf$historical_rd_row
  )
  boundary <- as.data.table(st_drop_geometry(boundary_sf))[, .(
    historical_rd_id, historical_rd_row,
    g_unit = suppressWarnings(as.integer(G_UNIT)),
    boundary_rd_name = as.character(G_NAME)
  )]
  boundary[, boundary_name_key := normalize_name(boundary_rd_name)]

  rsd <- fread(
    population_file, na.strings = c("", "NA"),
    colClasses = list(character = c("REGCNTY", "REGDIST", "SUBDIST"))
  )
  population <- rsd[, .(
    historical_rd_population = sum(as.numeric(POP), na.rm = TRUE),
    n_registration_subdistrict_rows = .N
  ), by = .(
    rd_regcnty = trimws(REGCNTY),
    rd_regdist = trimws(REGDIST)
  )]
  population[, `:=`(
    population_name_key = normalize_name(rd_regdist),
    rd_pop_key = paste(rd_regcnty, rd_regdist, sep = " | ")
  )]
  assert_unique(population, "rd_pop_key", paste(year, "RD population"))

  # Duplicate place names are resolved first using registration county.
  duplicate_matches <- merge(
    boundary[g_unit %in% duplicate_name_overrides$g_unit],
    duplicate_name_overrides,
    by = "g_unit", all = FALSE, sort = FALSE
  )
  duplicate_matches <- duplicate_matches[rd_pop_key %in% population$rd_pop_key,
    .(historical_rd_id, rd_pop_key,
      match_method = "manual_duplicate_name")
  ]

  used_boundary <- duplicate_matches$historical_rd_id
  used_population <- duplicate_matches$rd_pop_key

  # Reviewed aliases are selected only when the candidate exists in that year.
  alias_matches <- merge(
    boundary[
      !historical_rd_id %in% used_boundary &
        g_unit %in% alias_candidates$g_unit
    ],
    alias_candidates,
    by = "g_unit", all = FALSE, allow.cartesian = TRUE, sort = FALSE
  )
  alias_matches <- merge(
    alias_matches,
    population[!rd_pop_key %in% used_population,
               .(rd_pop_key, population_name_key)],
    by = "population_name_key", all = FALSE, allow.cartesian = TRUE,
    sort = FALSE
  )[, .(
    historical_rd_id, rd_pop_key,
    match_method = "manual_name_alias"
  )]
  if (anyDuplicated(alias_matches$historical_rd_id) ||
      anyDuplicated(alias_matches$rd_pop_key)) {
    message("Conflicting reviewed aliases in ", year, ":")
    print(alias_matches[
      duplicated(historical_rd_id) | duplicated(historical_rd_id, fromLast = TRUE) |
        duplicated(rd_pop_key) | duplicated(rd_pop_key, fromLast = TRUE)
    ])
  }
  assert_unique(alias_matches, "historical_rd_id",
                paste(year, "historical alias boundary match"))
  assert_unique(alias_matches, "rd_pop_key",
                paste(year, "historical alias population match"))

  used_boundary <- c(used_boundary, alias_matches$historical_rd_id)
  used_population <- c(used_population, alias_matches$rd_pop_key)

  # Remaining names must be unique on both sides for an automatic exact match.
  boundary_remaining <- boundary[!historical_rd_id %in% used_boundary &
                                    boundary_name_key != ""]
  population_remaining <- population[!rd_pop_key %in% used_population]
  boundary_remaining[, boundary_name_count := .N, by = boundary_name_key]
  population_remaining[, population_name_count := .N,
                       by = population_name_key]
  exact_matches <- merge(
    boundary_remaining,
    population_remaining,
    by.x = "boundary_name_key", by.y = "population_name_key",
    all = FALSE, allow.cartesian = TRUE, sort = FALSE
  )[boundary_name_count == 1L & population_name_count == 1L,
    .(historical_rd_id, rd_pop_key, match_method = "exact_unique_name")
  ]

  matches <- rbindlist(
    list(duplicate_matches, alias_matches, exact_matches),
    use.names = TRUE
  )
  assert_unique(matches, "historical_rd_id",
                paste(year, "historical RD boundary match"))
  assert_unique(matches, "rd_pop_key",
                paste(year, "historical RD population match"))

  audit_boundary <- merge(
    boundary, matches, by = "historical_rd_id", all.x = TRUE, sort = FALSE
  )
  audit_boundary <- merge(
    audit_boundary, population,
    by = "rd_pop_key", all.x = TRUE, sort = FALSE
  )
  audit_boundary[, `:=`(
    year = year,
    audit_side = "boundary",
    match_status = fifelse(
      !is.na(historical_rd_population), "matched",
      fifelse(
        (is.na(g_unit) | g_unit == 0L) &
          (is.na(boundary_name_key) | boundary_name_key == ""),
        "excluded_common_land_no_population", "unmatched_boundary"
      )
    )
  )]

  unmatched_population <- population[!rd_pop_key %in% matches$rd_pop_key]
  if (nrow(unmatched_population) > 0L) {
    unmatched_population[, `:=`(
      year = year, audit_side = "population",
      match_status = "unmatched_population",
      match_method = NA_character_, historical_rd_id = NA_character_,
      historical_rd_row = NA_integer_, g_unit = NA_integer_,
      boundary_rd_name = NA_character_, boundary_name_key = NA_character_
    )]
  }
  audit <- rbindlist(
    list(audit_boundary, unmatched_population),
    fill = TRUE, use.names = TRUE
  )

  named_unmatched <- audit_boundary[match_status == "unmatched_boundary"]
  if (nrow(unmatched_population) > 0L || nrow(named_unmatched) > 0L) {
    message("Unmatched historical RDs in ", year, ":")
    if (nrow(named_unmatched) > 0L) print(named_unmatched[, .(
      g_unit, boundary_rd_name
    )])
    if (nrow(unmatched_population) > 0L) print(unmatched_population[, .(
      rd_regcnty, rd_regdist, historical_rd_population
    )])
    stop("Historical RD matching is incomplete for ", year, ".")
  }

  matched_lookup <- audit_boundary[match_status == "matched", .(
    historical_rd_id, historical_rd_row, g_unit, boundary_rd_name,
    rd_regcnty, rd_regdist, rd_pop_key, historical_rd_population,
    n_registration_subdistrict_rows, match_method
  )]
  matched_sf <- merge(
    boundary_sf, matched_lookup,
    by = c("historical_rd_id", "historical_rd_row"),
    all = FALSE, sort = FALSE
  )
  if (nrow(matched_sf) != nrow(population)) {
    stop(year, " does not have one matched polygon per RD population row.")
  }

  list(sf = matched_sf, audit = audit)
}

###############################################################################
# Build the calibrated 1911 population-density surface
###############################################################################

message("Reading the calibrated 1911 RD x LGD density surface...")
cell_geometry <- st_read(
  overlap_gpkg, layer = "positive_area_intersections", quiet = TRUE
)
cell_geometry <- st_transform(st_make_valid(cell_geometry), analysis_crs)

population_crosswalk <- fread(
  population_crosswalk_file, na.strings = c("", "NA")
)
cell_population <- population_crosswalk[
  included_in_primary_raking == TRUE,
  .(
    rd_id, lgd_id, analysis_lgd_id,
    calibrated_cell_population_1911 = estimated_overlap_pop_1911,
    adjusted_rd_population_1911,
    rd_official_g_unit
  )
]
assert_unique(cell_population, c("rd_id", "lgd_id"),
              "Calibrated 1911 source cells")

cell_surface <- merge(
  cell_geometry[, c("rd_id", "lgd_id")],
  cell_population,
  by = c("rd_id", "lgd_id"), all = FALSE, sort = FALSE
)
cell_surface$cell_area_m2 <- as.numeric(st_area(cell_surface))
cell_surface$calibrated_cell_density_1911 <-
  cell_surface$calibrated_cell_population_1911 / cell_surface$cell_area_m2
if (any(!is.finite(cell_surface$calibrated_cell_density_1911) |
        cell_surface$calibrated_cell_density_1911 <= 0)) {
  stop("The calibrated 1911 cell surface contains invalid densities.")
}

calibration_audit <- fread(
  calibration_audit_file, na.strings = c("", "NA")
)
lgd_reference <- calibration_audit[, .(
  analysis_lgd_id, population_adm_county, population_lgd_name,
  population_lgd_type, source_polygon_count, source_lgd_ids,
  observed_lgd_population_1911
)]
assert_unique(lgd_reference, "analysis_lgd_id", "1911 analytical LGDs")
if (nrow(lgd_reference) != 1834L) {
  stop("Expected 1,834 calibrated 1911 analytical LGDs.")
}

###############################################################################
# Adapt the 1911 surface to each historical RD system
###############################################################################

process_year <- function(year) {
  message("Processing historical RD system: ", year)
  matched <- build_historical_rd_match(year)
  historical_sf <- matched$sf
  historical_sf$historical_rd_area_m2 <- as.numeric(st_area(historical_sf))

  pieces <- suppressWarnings(st_intersection(
    historical_sf[, c(
      "historical_rd_id", "historical_rd_row", "g_unit", "boundary_rd_name",
      "rd_regcnty", "rd_regdist", "rd_pop_key", "historical_rd_population",
      "historical_rd_area_m2", "match_method"
    )],
    cell_surface[, c(
      "analysis_lgd_id", "calibrated_cell_density_1911"
    )]
  ))
  pieces$intersection_area_m2 <- as.numeric(st_area(pieces))
  pieces_dt <- as.data.table(st_drop_geometry(pieces))[
    intersection_area_m2 > 0 & calibrated_cell_density_1911 > 0
  ]
  pieces_dt[, seed_population_1911 :=
              intersection_area_m2 * calibrated_cell_density_1911]

  weights <- pieces_dt[, .(
    intersection_area_m2 = sum(intersection_area_m2),
    seed_population_1911 = sum(seed_population_1911)
  ), by = .(
    historical_rd_id, historical_rd_row, historical_rd_g_unit = g_unit,
    historical_rd_name = boundary_rd_name, rd_regcnty, rd_regdist,
    rd_pop_key, historical_rd_population, historical_rd_area_m2,
    population_match_method = match_method, analysis_lgd_id
  )]
  weights[, `:=`(
    covered_area_m2 = sum(intersection_area_m2),
    seed_population_total_1911 = sum(seed_population_1911)
  ), by = historical_rd_id]
  weights[, `:=`(
    historical_rd_surface_coverage_share =
      covered_area_m2 / historical_rd_area_m2,
    adapted_rd_to_lgd_weight =
      seed_population_1911 / seed_population_total_1911
  )]
  weights[, historical_rd_surface_coverage_flag := fifelse(
    historical_rd_surface_coverage_share >= 0.99,
    "within_99pct", "below_99pct"
  )]
  weights[, estimated_lgd_population_contribution :=
            historical_rd_population * adapted_rd_to_lgd_weight]
  weights[, year := year]

  weight_check <- weights[, .(
    weight_sum = sum(adapted_rd_to_lgd_weight),
    reconstructed_rd_population = sum(estimated_lgd_population_contribution),
    historical_rd_population = unique(historical_rd_population)
  ), by = historical_rd_id]
  if (nrow(weight_check) != nrow(historical_sf) ||
      max(abs(weight_check$weight_sum - 1)) >= tolerance ||
      max(relative_error(
        weight_check$reconstructed_rd_population,
        weight_check$historical_rd_population
      )) >= tolerance) {
    stop("Historical RD weights or population conservation failed for ", year)
  }

  lgd <- weights[, .(
    reconstructed_lgd_population =
      sum(estimated_lgd_population_contribution),
    n_contributing_historical_rds = .N,
    minimum_contributing_rd_surface_coverage =
      min(historical_rd_surface_coverage_share),
    population_weighted_rd_surface_coverage = weighted.mean(
      historical_rd_surface_coverage_share,
      estimated_lgd_population_contribution
    )
  ), by = analysis_lgd_id]
  lgd <- merge(
    lgd_reference[, setdiff(
      names(lgd_reference), "observed_lgd_population_1911"
    ), with = FALSE], lgd,
    by = "analysis_lgd_id", all.x = TRUE, sort = FALSE
  )
  lgd[, `:=`(
    year = year,
    population_method = "calibrated_1911_density_backcast",
    population_interpolated = FALSE
  )]

  list(
    panel = lgd,
    weights = weights,
    match_audit = matched$audit,
    max_weight_error = max(abs(weight_check$weight_sum - 1)),
    source_population_total = sum(weight_check$historical_rd_population),
    reconstructed_population_total =
      sum(weight_check$reconstructed_rd_population)
  )
}

year_results <- lapply(c(direct_years, validation_year), process_year)
names(year_results) <- as.character(c(direct_years, validation_year))

###############################################################################
# Validate the 1911 geometry using calibrated adjusted RD margins
###############################################################################

validation_weights <- copy(year_results[[as.character(validation_year)]]$weights)
adjusted_rd <- unique(cell_population[, .(
  historical_rd_g_unit = as.integer(rd_official_g_unit),
  adjusted_rd_population_1911
)])
assert_unique(adjusted_rd, "historical_rd_g_unit", "Adjusted 1911 RD margins")
validation_weights <- merge(
  validation_weights[, setdiff(
    names(validation_weights), "historical_rd_population"
  ), with = FALSE],
  adjusted_rd,
  by = "historical_rd_g_unit", all.x = TRUE, sort = FALSE
)
if (anyNA(validation_weights$adjusted_rd_population_1911)) {
  stop("A 1911 validation RD lacks its adjusted population margin.")
}
validation_weights[, validation_population_contribution :=
  adjusted_rd_population_1911 * adapted_rd_to_lgd_weight]
validation <- validation_weights[, .(
  reconstructed_lgd_population_1911 = sum(validation_population_contribution)
), by = analysis_lgd_id]
validation <- merge(
  validation, lgd_reference[, .(
    analysis_lgd_id, observed_lgd_population_1911
  )], by = "analysis_lgd_id", all = TRUE
)
validation[, relative_error := relative_error(
  reconstructed_lgd_population_1911, observed_lgd_population_1911
)]
if (anyNA(validation) || max(validation$relative_error) >= tolerance) {
  stop("The internal 1911 calibrated LGD validation failed.")
}

###############################################################################
# Assemble direct backcasts and interpolate 1881 in levels
###############################################################################

direct_panel <- rbindlist(
  lapply(as.character(direct_years), function(y) year_results[[y]]$panel),
  fill = TRUE, use.names = TRUE
)
endpoint_1871 <- direct_panel[year == 1871L, .(
  analysis_lgd_id, population_1871 = reconstructed_lgd_population
)]
endpoint_1891 <- direct_panel[year == 1891L, .(
  analysis_lgd_id, population_1891 = reconstructed_lgd_population
)]
interpolated_1881 <- merge(
  endpoint_1871, endpoint_1891,
  by = "analysis_lgd_id", all = TRUE, sort = FALSE
)
interpolated_1881 <- merge(
  lgd_reference[, setdiff(
    names(lgd_reference), "observed_lgd_population_1911"
  ), with = FALSE],
  interpolated_1881,
  by = "analysis_lgd_id", all.x = TRUE, sort = FALSE
)
interpolated_1881[, `:=`(
  reconstructed_lgd_population = (population_1871 + population_1891) / 2,
  n_contributing_historical_rds = NA_integer_,
  minimum_contributing_rd_surface_coverage = NA_real_,
  population_weighted_rd_surface_coverage = NA_real_,
  year = 1881L,
  population_method = "linear_interpolation_1871_1891",
  population_interpolated = TRUE
)]
interpolated_1881[, c("population_1871", "population_1891") := NULL]

backcast <- rbindlist(
  list(direct_panel, interpolated_1881),
  fill = TRUE, use.names = TRUE
)
setcolorder(backcast, c(
  "analysis_lgd_id", "population_adm_county", "population_lgd_name",
  "population_lgd_type", "source_polygon_count", "source_lgd_ids", "year",
  "reconstructed_lgd_population", "population_method",
  "population_interpolated", "n_contributing_historical_rds",
  "minimum_contributing_rd_surface_coverage",
  "population_weighted_rd_surface_coverage"
))
setorder(backcast, analysis_lgd_id, year)

expected_rows <- nrow(lgd_reference) * 6L
if (nrow(backcast) != expected_rows || anyNA(backcast$reconstructed_lgd_population)) {
  stop("The balanced six-year LGD backcast is incomplete.")
}
midpoint_check <- merge(
  backcast[year == 1881L, .(
    analysis_lgd_id, population_1881 = reconstructed_lgd_population
  )],
  merge(endpoint_1871, endpoint_1891, by = "analysis_lgd_id"),
  by = "analysis_lgd_id"
)
midpoint_check[, expected_1881 := (population_1871 + population_1891) / 2]
if (max(relative_error(
      midpoint_check$population_1881, midpoint_check$expected_1881
    )) >= tolerance) {
  stop("The 1881 linear interpolation check failed.")
}

weights_output <- rbindlist(
  lapply(as.character(direct_years), function(y) year_results[[y]]$weights),
  fill = TRUE, use.names = TRUE
)
setorder(weights_output, year, historical_rd_id,
         -estimated_lgd_population_contribution, analysis_lgd_id)

###############################################################################
# Build fixed-1851 baseline socioeconomic covariates
###############################################################################

message("Constructing fixed-1851 LGD baseline covariates...")
baseline_raw <- fread(
  file.path(rsd_population_root, paste0(baseline_year, "_rsd_data.csv")),
  na.strings = c("", "NA", "-"),
  colClasses = list(character = c("REGCNTY", "REGDIST", "SUBDIST"))
)
required_baseline_columns <- c(
  "REGCNTY", "REGDIST", "SUBDIST", "POP", baseline_source_variables
)
if (!all(required_baseline_columns %in% names(baseline_raw))) {
  stop("The 1851 RSD file is missing required baseline covariates.")
}
baseline_raw[, POP := suppressWarnings(as.numeric(POP))]
for (variable in baseline_source_variables) {
  set(
    baseline_raw, j = variable,
    value = suppressWarnings(as.numeric(baseline_raw[[variable]]))
  )
}
if (any(!is.finite(baseline_raw$POP) | baseline_raw$POP <= 0)) {
  stop("The 1851 RSD baseline source contains invalid population values.")
}
baseline_raw[, observed_baseline_fields := rowSums(
  is.finite(as.matrix(.SD))
), .SDcols = baseline_source_variables]
if (any(!baseline_raw$observed_baseline_fields %in%
          c(0L, length(baseline_source_variables)))) {
  stop("The baseline-variable missingness pattern differs across fields.")
}
baseline_raw[, baseline_covariates_observed :=
  observed_baseline_fields == length(baseline_source_variables)]

baseline_rd <- baseline_raw[, {
  observed <- baseline_covariates_observed
  total_population <- sum(POP)
  observed_population <- sum(POP[observed])
  values <- lapply(.SD, function(value) {
    if (observed_population <= 0) return(NA_real_)
    sum(value[observed] * POP[observed]) / observed_population
  })
  names(values) <- baseline_output_variables
  c(list(
    rd_subdistrict_rows = .N,
    rd_observed_subdistrict_rows = sum(observed),
    rd_total_population_1851 = total_population,
    rd_observed_covariate_population_1851 = observed_population,
    rd_covariate_population_coverage = observed_population / total_population
  ), values)
}, by = .(
  rd_regcnty = trimws(REGCNTY),
  rd_regdist = trimws(REGDIST)
), .SDcols = baseline_source_variables]
baseline_rd[, rd_pop_key := paste(rd_regcnty, rd_regdist, sep = " | ")]

baseline_weight_rd <- unique(weights_output[year == baseline_year, .(
  rd_pop_key, historical_rd_id, historical_rd_name,
  historical_rd_population
)])
assert_unique(baseline_weight_rd, "rd_pop_key", "1851 covariate RD weights")
baseline_rd <- merge(
  baseline_weight_rd, baseline_rd,
  by = "rd_pop_key", all.x = TRUE, sort = FALSE
)
if (nrow(baseline_rd) != 624L ||
    anyNA(baseline_rd[, .(
      rd_regcnty, rd_regdist, rd_total_population_1851,
      rd_observed_covariate_population_1851,
      rd_covariate_population_coverage
    )]) ||
    max(relative_error(
      baseline_rd$rd_total_population_1851,
      baseline_rd$historical_rd_population
    )) >= tolerance) {
  stop("The 1851 RD baseline aggregation failed its population checks.")
}

baseline_edges <- merge(
  weights_output[year == baseline_year, .(
    rd_pop_key, analysis_lgd_id, estimated_lgd_population_contribution
  )],
  baseline_rd[, c(
    "rd_pop_key", "rd_covariate_population_coverage",
    baseline_output_variables
  ), with = FALSE],
  by = "rd_pop_key", all.x = TRUE, sort = FALSE
)
if (anyNA(baseline_edges$rd_covariate_population_coverage)) {
  stop("An 1851 RD-to-LGD edge lacks baseline source coverage.")
}
baseline_edges[, effective_covariate_population_contribution :=
  estimated_lgd_population_contribution * rd_covariate_population_coverage]

baseline_lgd <- baseline_edges[, {
  contribution <- estimated_lgd_population_contribution
  effective_contribution <- effective_covariate_population_contribution
  total_population <- sum(contribution)
  observed_population <- sum(effective_contribution)
  values <- lapply(.SD, function(value) {
    valid <- is.finite(value) & effective_contribution > 0
    if (!any(valid)) return(NA_real_)
    sum(value[valid] * effective_contribution[valid]) /
      sum(effective_contribution[valid])
  })
  names(values) <- baseline_output_variables
  c(list(
    reconstructed_lgd_population_1851 = total_population,
    baseline_covariate_observed_population = observed_population,
    baseline_covariate_population_coverage =
      observed_population / total_population
  ), values)
}, by = analysis_lgd_id, .SDcols = baseline_output_variables]
baseline_lgd <- merge(
  lgd_reference, baseline_lgd,
  by = "analysis_lgd_id", all.x = TRUE, sort = FALSE
)
baseline_lgd[, `:=`(
  baseline_covariate_year = baseline_year,
  baseline_covariates_usable =
    baseline_covariate_population_coverage + tolerance >=
      baseline_minimum_coverage,
  baseline_covariate_method = paste0(
    "population_weighted_rsd_to_rd_then_",
    "historical_population_contribution_weighted_rd_to_lgd"
  )
)]
baseline_lgd[, baseline_covariate_coverage_flag := fifelse(
  baseline_covariates_usable,
  "usable_ge95pct", "insufficient_below95pct"
)]
for (variable in baseline_output_variables) {
  set(
    baseline_lgd,
    i = which(!baseline_lgd$baseline_covariates_usable),
    j = variable,
    value = NA_real_
  )
}
setcolorder(baseline_lgd, c(
  "analysis_lgd_id", "population_adm_county", "population_lgd_name",
  "population_lgd_type", "source_polygon_count", "source_lgd_ids",
  "baseline_covariate_year", baseline_output_variables,
  "baseline_covariate_population_coverage", "baseline_covariates_usable",
  "baseline_covariate_coverage_flag", "baseline_covariate_method",
  "baseline_covariate_observed_population",
  "reconstructed_lgd_population_1851", "observed_lgd_population_1911"
))
setorder(baseline_lgd, analysis_lgd_id)

usable_baseline <- baseline_lgd[baseline_covariates_usable == TRUE]
unusable_baseline <- baseline_lgd[baseline_covariates_usable == FALSE]
baseline_percentage_variables <- c(
  paste0("baseline_hc", 1:5, "_pct"),
  paste0("baseline_sc", 1:8, "_pct")
)
if (nrow(baseline_lgd) != 1834L ||
    anyDuplicated(baseline_lgd$analysis_lgd_id) ||
    nrow(unusable_baseline) != 33L ||
    anyNA(usable_baseline[, ..baseline_output_variables]) ||
    unusable_baseline[, any(rowSums(!is.na(.SD)) > 0),
                      .SDcols = baseline_output_variables] ||
    usable_baseline[, any(unlist(.SD) < 0 | unlist(.SD) > 100),
                    .SDcols = baseline_percentage_variables] ||
    usable_baseline[, any(!is.finite(baseline_children_per_teacher) |
                            baseline_children_per_teacher < 0)]) {
  stop("The fixed-1851 LGD baseline covariates failed validation.")
}

setcolorder(baseline_rd, c(
  "rd_pop_key", "historical_rd_id", "historical_rd_name",
  "rd_regcnty", "rd_regdist", "rd_subdistrict_rows",
  "rd_observed_subdistrict_rows", "rd_total_population_1851",
  "rd_observed_covariate_population_1851",
  "rd_covariate_population_coverage", baseline_output_variables,
  "historical_rd_population"
))
setorder(baseline_rd, rd_regcnty, rd_regdist)

baseline_covariate_qc <- data.table(
  metric = c(
    "source_subdistrict_rows", "source_registration_districts",
    "source_rows_missing_all_baseline_fields",
    "source_population_coverage", "registration_districts_zero_coverage",
    "registration_districts_partial_coverage", "output_lgd_rows",
    "usable_lgds_ge95pct", "unusable_lgds_below95pct",
    "minimum_lgd_population_coverage", "coverage_threshold"
  ),
  value = as.character(c(
    nrow(baseline_raw), nrow(baseline_rd),
    baseline_raw[baseline_covariates_observed == FALSE, .N],
    sum(baseline_rd$rd_observed_covariate_population_1851) /
      sum(baseline_rd$rd_total_population_1851),
    baseline_rd[rd_covariate_population_coverage == 0, .N],
    baseline_rd[rd_covariate_population_coverage > 0 &
                  rd_covariate_population_coverage < 1, .N],
    nrow(baseline_lgd), nrow(usable_baseline), nrow(unusable_baseline),
    min(baseline_lgd$baseline_covariate_population_coverage),
    baseline_minimum_coverage
  ))
)

match_audit <- rbindlist(
  lapply(as.character(c(direct_years, validation_year)),
         function(y) year_results[[y]]$match_audit),
  fill = TRUE, use.names = TRUE
)
setorder(match_audit, year, audit_side, historical_rd_row, rd_pop_key)

###############################################################################
# QC and outputs
###############################################################################

qc <- rbindlist(lapply(as.character(direct_years), function(y) {
  result <- year_results[[y]]
  data.table(
    year = as.integer(y),
    matched_historical_rds = result$match_audit[
      audit_side == "boundary" & match_status == "matched", .N
    ],
    excluded_nonpopulation_boundaries = result$match_audit[
      audit_side == "boundary" &
        match_status == "excluded_common_land_no_population", .N
    ],
    unmatched_population_rds = result$match_audit[
      match_status == "unmatched_population", .N
    ],
    rds_below_99pct_surface_coverage = uniqueN(
      result$weights[
        historical_rd_surface_coverage_flag == "below_99pct",
        historical_rd_id
      ]
    ),
    max_rd_weight_sum_error = result$max_weight_error,
    source_rd_population_total = result$source_population_total,
    reconstructed_lgd_population_total =
      result$reconstructed_population_total,
    population_conservation_relative_error = relative_error(
      result$reconstructed_population_total, result$source_population_total
    )
  )
}))
qc[, `:=`(
  lgd_rows = nrow(lgd_reference),
  minimum_rd_surface_coverage = vapply(
    as.character(year),
    function(y) min(
      year_results[[y]]$weights$historical_rd_surface_coverage_share
    ), numeric(1L)
  ),
  lgd_population_complete = TRUE
)]

liverpool_validation <- validation[
  analysis_lgd_id == calibration_audit[
    toupper(population_lgd_name) == "LIVERPOOL" &
      toupper(population_lgd_type) == "CB", analysis_lgd_id
  ]
]
if (nrow(liverpool_validation) != 1L ||
    abs(liverpool_validation$reconstructed_lgd_population_1911 - 747535) >=
      1e-6) {
  stop("Liverpool's 1911 calibrated validation does not equal 747,535.")
}
qc[, `:=`(
  output_lgd_year_rows = nrow(backcast),
  interpolated_1881_lgd_rows = backcast[
    year == 1881L & population_interpolated == TRUE, .N
  ],
  interpolated_1881_population_total = backcast[
    year == 1881L, sum(reconstructed_lgd_population)
  ],
  max_1881_midpoint_relative_error = max(relative_error(
    midpoint_check$population_1881, midpoint_check$expected_1881
  )),
  max_1911_validation_relative_error = max(validation$relative_error),
  liverpool_1911_validation_population =
    liverpool_validation$reconstructed_lgd_population_1911
)]

fwrite(backcast, backcast_file, na = "")
fwrite(weights_output, weights_file, na = "")
fwrite(match_audit, match_audit_file, na = "")
fwrite(qc, qc_file, na = "")
fwrite(baseline_lgd, baseline_covariate_file, na = "")
fwrite(baseline_rd, baseline_rd_audit_file, na = "")
fwrite(baseline_covariate_qc, baseline_covariate_qc_file, na = "")

message("Historical population backcast to fixed 1911 LGDs complete.")
message("  Direct years: ", paste(direct_years, collapse = ", "))
message("  Interpolated year: 1881")
message("  LGD-year rows: ", nrow(backcast))
message("  Maximum 1911 validation relative error: ",
        signif(max(validation$relative_error), 6))
message("  Liverpool fixed-1911-boundary population series:")
print(backcast[
  toupper(population_lgd_name) == "LIVERPOOL" &
    toupper(population_lgd_type) == "CB",
  .(year, reconstructed_lgd_population, population_method)
])
message("  Backcast: ", backcast_file)
message("  Historical weights: ", weights_file)
message("  Match audit: ", match_audit_file)
message("  QC: ", qc_file)
message("  Fixed-1851 baseline covariates: ", baseline_covariate_file)
message("  Baseline RD audit: ", baseline_rd_audit_file)
message("  Baseline covariate QC: ", baseline_covariate_qc_file)
