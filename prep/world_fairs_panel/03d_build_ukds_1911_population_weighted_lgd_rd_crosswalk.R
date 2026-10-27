###############################################################################
# Build a 1911 population-weighted crosswalk from local-government districts
# (LGDs) to registration districts (RDs) for England and Wales.
#
# Method:
#   1. Match 1911 RD and LGD population totals to the UKDS boundary units.
#   2. Aggregate the positive-area RD x LGD intersections from step 03c.
#   3. Reconcile RD totals to LGD totals within each connected overlap component.
#   4. Rake intersection-area seeds to both sets of population margins (IPF).
#      If structural zeros make the published margins locally incompatible,
#      finish on the exact LGD margin and audit the implied feasible RD margin.
#   5. Assign an LGD to its largest estimated-population RD when the share is
#      at least 95 percent. Run all-positive-edge and 1-percent sensitivities.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/03d_build_ukds_1911_population_weighted_lgd_rd_crosswalk.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(readxl)
  library(sf)
})

sf_use_s2(FALSE)

###############################################################################
# Paths and constants
###############################################################################

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]), winslash = "/", mustWork = TRUE
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
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

processed_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "Data", "processed", "worlds_fairs", "ukds_1911_boundary_crosswalk"
)
figure_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results", "worlds_fair", "ukds_1911_boundary_overlap"
)

area_file <- file.path(
  processed_dir, "ukds_1911_registration_lgd_area_crosswalk.csv"
)
gpkg_file <- file.path(
  processed_dir, "ukds_1911_registration_lgd_overlap.gpkg"
)
rd_population_file <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "raw", "world_fairs", "ukds_birth_data",
  "UKDA-8613-csv", "csv", "1911_rsd_data.csv"
)
lgd_population_file <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "raw", "world_fairs", "ukds_birth_data",
  "UKDA-9035-excel", "excel", "vital_ew_lgd_ann.xlsx"
)
fairs_file <- file.path(
  TALENT_DETS_DATA_DIR, "input", "worlds_fairs",
  "worlds_fairs_1790_1960_with_visits_venues.csv"
)

rd_audit_file <- file.path(
  processed_dir, "ukds_1911_rd_population_match_audit.csv"
)
lgd_audit_file <- file.path(
  processed_dir, "ukds_1911_lgd_population_match_audit.csv"
)
population_crosswalk_file <- file.path(
  processed_dir, "ukds_1911_lgd_rd_population_crosswalk.csv"
)
assignment_file <- file.path(
  processed_dir, "ukds_1911_lgd_population_assignment.csv"
)
calibration_audit_file <- file.path(
  processed_dir, "ukds_1911_lgd_calibrated_population_audit.csv"
)
raking_qc_file <- file.path(
  processed_dir, "ukds_1911_population_raking_qc.csv"
)
fair_assignment_file <- file.path(
  processed_dir, "ukds_1911_fair_host_population_assignments.csv"
)
assignment_map_file <- file.path(
  figure_dir, "ukds_1911_lgd_population_assignment_map.png"
)
share_scatter_file <- file.path(
  figure_dir, "ukds_1911_area_vs_population_dominant_share.png"
)

required_files <- c(
  area_file, gpkg_file, rd_population_file, lgd_population_file, fairs_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required inputs:\n", paste(missing_files, collapse = "\n"))
}

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

assignment_threshold <- 0.95
ipf_tolerance <- 1e-8
ipf_max_iterations <- 10000L
fair_parent_ids <- c(165L, 175L, 226L, 273L, 348L, 375L, 391L)

###############################################################################
# Helpers
###############################################################################

normalize_name <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

first_nonmissing <- function(x) {
  y <- x[!is.na(x) & trimws(as.character(x)) != ""]
  if (length(y) == 0L) NA_character_ else as.character(y[[1L]])
}

assert_unique <- function(x, columns, label) {
  if (anyDuplicated(x[, ..columns])) {
    stop(label, " is not unique by: ", paste(columns, collapse = ", "))
  }
}

relative_error <- function(actual, target) {
  abs(actual - target) / pmax(abs(target), 1)
}

connected_components <- function(edges) {
  rd_nodes <- paste0("R|", edges$rd_id)
  lgd_nodes <- paste0("L|", edges$analysis_lgd_id)
  nodes <- unique(c(rd_nodes, lgd_nodes))
  node_index <- setNames(seq_along(nodes), nodes)
  parent <- seq_along(nodes)
  rank <- integer(length(nodes))

  find_root <- function(i) {
    j <- i
    while (parent[[j]] != j) j <- parent[[j]]
    root <- j
    j <- i
    while (parent[[j]] != j) {
      next_j <- parent[[j]]
      parent[[j]] <<- root
      j <- next_j
    }
    root
  }

  union_nodes <- function(i, j) {
    ri <- find_root(i)
    rj <- find_root(j)
    if (ri == rj) return(invisible(NULL))
    if (rank[[ri]] < rank[[rj]]) {
      parent[[ri]] <<- rj
    } else if (rank[[ri]] > rank[[rj]]) {
      parent[[rj]] <<- ri
    } else {
      parent[[rj]] <<- ri
      rank[[ri]] <<- rank[[ri]] + 1L
    }
    invisible(NULL)
  }

  for (i in seq_len(nrow(edges))) {
    union_nodes(node_index[[rd_nodes[[i]]]], node_index[[lgd_nodes[[i]]]])
  }
  roots <- vapply(seq_along(nodes), find_root, integer(1L))
  root_levels <- sort(unique(roots))
  component <- match(roots, root_levels)
  component_lookup <- setNames(component, nodes)
  as.integer(component_lookup[rd_nodes])
}

run_ipf <- function(edges, spec_name) {
  x <- copy(edges)
  x[, component_id := connected_components(x)]

  lgd_component <- unique(x[, .(
    component_id, analysis_lgd_id, lgd_population_1911
  )])
  rd_component <- unique(x[, .(
    component_id, rd_id, rd_population_1911
  )])

  component_qc <- merge(
    lgd_component[, .(
      lgd_population_total = sum(lgd_population_1911),
      n_lgds = .N
    ), by = component_id],
    rd_component[, .(
      rd_population_total = sum(rd_population_1911),
      n_rds = .N
    ), by = component_id],
    by = "component_id", all = TRUE
  )
  component_qc[, rd_margin_scale := lgd_population_total / rd_population_total]
  if (any(!is.finite(component_qc$rd_margin_scale) |
          component_qc$rd_margin_scale <= 0)) {
    stop("Invalid RD population scale in ", spec_name)
  }

  x <- merge(
    x,
    component_qc[, .(component_id, rd_margin_scale)],
    by = "component_id", all.x = TRUE, sort = FALSE
  )
  x[, component_scaled_rd_population_1911 :=
      rd_population_1911 * rd_margin_scale]
  x[, estimated_overlap_pop_1911 := overlap_area_m2]

  # Integer indices and rowsum() make the iterative scaling much faster than
  # repeated data.table group-bys, especially for near-boundary solutions.
  lgd_levels <- unique(x$analysis_lgd_id)
  rd_levels <- unique(x$rd_id)
  lgd_index <- match(x$analysis_lgd_id, lgd_levels)
  rd_index <- match(x$rd_id, rd_levels)
  lgd_targets <- x$lgd_population_1911[
    match(lgd_levels, x$analysis_lgd_id)
  ]
  rd_targets <- x$component_scaled_rd_population_1911[
    match(rd_levels, x$rd_id)
  ]
  fitted <- x$estimated_overlap_pop_1911

  iterations <- 0L
  max_row_error <- Inf
  max_column_error <- Inf
  while (iterations < ipf_max_iterations) {
    iterations <- iterations + 1L

    row_sum <- as.numeric(rowsum(fitted, lgd_index, reorder = FALSE))
    fitted <- fitted * (lgd_targets / row_sum)[lgd_index]

    column_sum <- as.numeric(rowsum(fitted, rd_index, reorder = FALSE))
    fitted <- fitted * (rd_targets / column_sum)[rd_index]

    if (iterations == 1L || iterations %% 10L == 0L) {
      row_check <- as.numeric(rowsum(fitted, lgd_index, reorder = FALSE))
      column_check <- as.numeric(rowsum(fitted, rd_index, reorder = FALSE))
      max_row_error <- max(relative_error(row_check, lgd_targets))
      max_column_error <- max(relative_error(column_check, rd_targets))
      if (max(max_row_error, max_column_error) < ipf_tolerance) break
    }
  }

  exact_original_margin_convergence <-
    max(max_row_error, max_column_error) < ipf_tolerance
  if (!exact_original_margin_convergence) {
    row_diagnostic <- data.table(
      analysis_lgd_id = lgd_levels,
      target = lgd_targets,
      actual = as.numeric(rowsum(fitted, lgd_index, reorder = FALSE))
    )
    row_diagnostic[, relative_error := relative_error(actual, target)]
    row_diagnostic <- merge(
      row_diagnostic,
      unique(x[, .(analysis_lgd_id, population_lgd_name,
                   population_adm_county, component_id)]),
      by = "analysis_lgd_id", all.x = TRUE
    )
    setorder(row_diagnostic, -relative_error)
    message("Largest LGD margin errors for ", spec_name, ":\n",
            paste(capture.output(print(head(row_diagnostic, 10L))),
                  collapse = "\n"))
    message(
      "The component-scaled RD margins are structurally incompatible in ",
      spec_name, ". Using the alternating-projection RD margins after ",
      iterations, " alternating projections."
    )
  }

  # Whether or not the two original margin sets intersect, finish on the LGD
  # projection so every observed LGD population is preserved exactly. The
  # resulting RD column sums are the feasible local revisions to the initially
  # component-scaled RD targets. When exact convergence occurred, revisions are
  # only floating-point noise.
  final_row_sum <- as.numeric(rowsum(fitted, lgd_index, reorder = FALSE))
  fitted <- fitted * (lgd_targets / final_row_sum)[lgd_index]
  feasible_rd_targets <- as.numeric(rowsum(fitted, rd_index, reorder = FALSE))
  x[, estimated_overlap_pop_1911 := fitted]
  x[, adjusted_rd_population_1911 := feasible_rd_targets[rd_index]]
  x[, rd_feasibility_adjustment_ratio :=
      adjusted_rd_population_1911 / component_scaled_rd_population_1911]

  final_row_check <- as.numeric(rowsum(fitted, lgd_index, reorder = FALSE))
  final_column_check <- as.numeric(rowsum(fitted, rd_index, reorder = FALSE))
  final_max_row_error <- max(relative_error(final_row_check, lgd_targets))
  final_max_column_error <- max(
    relative_error(final_column_check, feasible_rd_targets)
  )
  component_revision <- unique(x[, .(
    component_id, rd_id, component_scaled_rd_population_1911,
    adjusted_rd_population_1911
  )])[, .(
    max_rd_feasibility_revision = max(relative_error(
      adjusted_rd_population_1911, component_scaled_rd_population_1911
    ))
  ), by = component_id]

  x[, estimated_share_of_lgd_population :=
      estimated_overlap_pop_1911 / lgd_population_1911]
  x[, estimated_share_of_adjusted_rd_population :=
      estimated_overlap_pop_1911 / adjusted_rd_population_1911]
  setorder(
    x, analysis_lgd_id, -estimated_share_of_lgd_population,
    -lgd_overlap_share, rd_id
  )
  x[, population_match_rank := seq_len(.N), by = analysis_lgd_id]
  x[, is_population_primary_rd := population_match_rank == 1L]
  setorder(x, rd_id, -estimated_share_of_adjusted_rd_population,
           -rd_overlap_share, analysis_lgd_id)
  x[, population_lgd_rank_within_rd := seq_len(.N), by = rd_id]

  assignment <- x[is_population_primary_rd == TRUE]
  assignment[, population_assignment_ge_95pct :=
               estimated_share_of_lgd_population >= assignment_threshold]
  assignment[, assigned_rd_id := fifelse(
    population_assignment_ge_95pct, rd_id, NA_character_
  )]
  assignment[, assigned_rd_name := fifelse(
    population_assignment_ge_95pct, rd_name, NA_character_
  )]
  assignment[, assigned_rd_regcnty := fifelse(
    population_assignment_ge_95pct, rd_regcnty, NA_character_
  )]

  component_qc <- merge(
    component_qc, component_revision,
    by = "component_id", all.x = TRUE, sort = FALSE
  )
  component_qc[, `:=`(
    spec = spec_name,
    iterations = iterations,
    exact_original_margin_convergence = exact_original_margin_convergence,
    pre_reconciliation_max_lgd_relative_error = max_row_error,
    pre_reconciliation_max_rd_relative_error = max_column_error,
    max_lgd_margin_relative_error = final_max_row_error,
    max_rd_margin_relative_error = final_max_column_error,
    rd_scale_outside_5pct = rd_margin_scale < 0.95 | rd_margin_scale > 1.05
  )]

  list(pairs = x, assignment = assignment, component_qc = component_qc)
}

###############################################################################
# Read the immutable area crosswalk and boundary layers
###############################################################################

area <- fread(area_file, na.strings = c("", "NA"))
if (nrow(area) != 5158L) {
  stop("Area crosswalk has ", nrow(area), " rows; expected 5,158.")
}

rd_sf <- st_read(gpkg_file, layer = "registration_districts_1911", quiet = TRUE)
lgd_sf <- st_read(
  gpkg_file, layer = "local_government_districts_1911", quiet = TRUE
)
rd_boundary <- as.data.table(st_drop_geometry(rd_sf))
lgd_boundary <- as.data.table(st_drop_geometry(lgd_sf))
assert_unique(rd_boundary, "rd_id", "RD boundary layer")
assert_unique(lgd_boundary, "lgd_id", "LGD boundary layer")

###############################################################################
# Match registration-district populations
###############################################################################

rd_population_raw <- fread(
  rd_population_file, na.strings = c("", "NA"),
  colClasses = list(character = c("REGCNTY", "REGDIST", "SUBDIST"))
)
required_rd_columns <- c("REGCNTY", "REGDIST", "POP")
if (!all(required_rd_columns %in% names(rd_population_raw))) {
  stop("RD population file is missing required columns.")
}
rd_population <- rd_population_raw[, .(
  rd_population_1911 = sum(as.numeric(POP), na.rm = TRUE),
  n_registration_subdistrict_rows = .N
), by = .(rd_regcnty = trimws(REGCNTY), rd_regdist = trimws(REGDIST))]
rd_population[, rd_pop_key := paste(rd_regcnty, rd_regdist, sep = " | ")]
rd_population[, rd_name_normalized := normalize_name(rd_regdist)]
assert_unique(rd_population, "rd_pop_key", "Aggregated RD population")

rd_reference <- unique(area[, .(
  rd_id, rd_official_g_unit, rd_source_id_missing, rd_source_row,
  rd_name, rd_unit_type, rd_status, rd_nation, rd_year, rd_area_m2
)])
assert_unique(rd_reference, "rd_id", "RD area reference")
rd_reference[, rd_name_normalized := normalize_name(rd_name)]

rd_alias <- data.table(
  rd_id = c(
    "RD_GUNIT_10005681", "RD_GUNIT_10139533", "RD_GUNIT_10030195",
    "RD_GUNIT_10066694", "RD_GUNIT_10112394", "RD_GUNIT_10139399",
    "RD_GUNIT_10167516", "RD_GUNIT_10174820", "RD_GUNIT_10155265",
    "RD_GUNIT_10030110", "RD_GUNIT_10155058", "RD_GUNIT_10112138",
    "RD_GUNIT_10194118", "RD_GUNIT_10187679", "RD_GUNIT_10167590",
    "RD_GUNIT_10154923", "RD_GUNIT_10030171", "RD_GUNIT_10139211",
    "RD_GUNIT_10029650", "RD_GUNIT_10029879", "RD_GUNIT_10155149",
    "RD_GUNIT_10178897", "RD_GUNIT_10029569", "RD_GUNIT_10087673",
    "RD_GUNIT_10055805", "RD_GUNIT_10174739", "RD_GUNIT_10111833",
    "RD_GUNIT_10154730", "RD_GUNIT_10029624", "RD_GUNIT_10063292",
    "RD_GUNIT_10139648", "RD_GUNIT_10081749", "RD_GUNIT_10005280",
    "RD_GUNIT_10112035", "RD_GUNIT_10055714", "RD_GUNIT_10174636",
    "RD_GUNIT_10081804", "RD_GUNIT_10044236"
  ),
  rd_regdist = c(
    "CRICKHOWELL", "PONTYPOOL", "BRIDGEND", "NEWTOWN", "ABERAYRON",
    "LLANDILOFAWR", "MERTHYR TYDFIL", "DOLGELLEY", "ABERYSTWYTH",
    "ST COLUMB", "BISHOP'S STORTFORD", "CRICKLADE", "SWINDON",
    "MITFORD", "ST FAITH'S", "KING'S LYNN", "BOSMERE",
    "UPTON ON SEVERN", "WINCHCOMB", "LEXDEN", "GRAVESEND", "MELKSHAM",
    "THAKEHAM", "FLEGG", "MUTFORD", "LODDON", "HUNGERFORD",
    "ST GILES", "WESTBURY", "LONDON CITY", "ST GEORGE HANOVER SQUARE",
    "ST OLAVE (BERMONDSEY)", "WAREHAM", "BANGOR", "WOLSTANTON",
    "STOKE UPON TRENT", "FFESTINIOG", "ALSTON"
  ),
  match_method = "manual_name_alias"
)

rd_duplicate <- data.table(
  rd_id = c(
    "RD_GUNIT_10104129", "RD_GUNIT_10201482", "RD_GUNIT_10167530",
    "RD_GUNIT_10043967", "RD_GUNIT_10139107", "RD_GUNIT_10187564",
    "RD_GUNIT_10084404", "RD_GUNIT_10075439"
  ),
  rd_regcnty = c(
    "MONMOUTHSHIRE", "SHROPSHIRE", "YORKSHIRE NORTH RIDING", "SURREY",
    "SOMERSET", "SHROPSHIRE", "SHROPSHIRE", "HAMPSHIRE"
  ),
  rd_regdist = c(
    "NEWPORT", "NEWPORT", "RICHMOND", "RICHMOND", "WELLINGTON",
    "WELLINGTON", "WHITCHURCH", "WHITCHURCH"
  ),
  match_method = "manual_duplicate_name"
)

rd_pop_name_counts <- rd_population[, .N, by = rd_name_normalized]
rd_boundary_name_counts <- rd_reference[rd_name_normalized != "", .N,
                                        by = rd_name_normalized]
rd_exact <- merge(
  rd_reference[rd_name_normalized != ""],
  rd_population,
  by = "rd_name_normalized", allow.cartesian = TRUE
)[rd_pop_name_counts, on = "rd_name_normalized"][
  rd_boundary_name_counts, on = "rd_name_normalized",
  nomatch = 0L
][N == 1L & i.N == 1L, .(rd_id, rd_pop_key, match_method = "exact_unique_name")]

rd_alias <- merge(
  rd_alias,
  rd_population[, .(rd_pop_key, rd_regdist)],
  by = "rd_regdist", all.x = TRUE, sort = FALSE
)[, .(rd_id, rd_pop_key, match_method)]
rd_duplicate[, rd_pop_key := paste(rd_regcnty, rd_regdist, sep = " | ")]
rd_match <- rbindlist(list(
  rd_exact,
  rd_alias,
  rd_duplicate[, .(rd_id, rd_pop_key, match_method)]
), use.names = TRUE)
assert_unique(rd_match, "rd_id", "RD boundary-population match")
assert_unique(rd_match, "rd_pop_key", "RD population-boundary match")

rd_audit_boundary <- merge(
  rd_reference, rd_match, by = "rd_id", all.x = TRUE, sort = FALSE
)
rd_audit_boundary <- merge(
  rd_audit_boundary, rd_population,
  by = "rd_pop_key", all.x = TRUE, sort = FALSE, suffixes = c("", "_pop")
)
rd_audit_boundary[, `:=`(
  audit_side = "boundary",
  match_status = fifelse(
    !is.na(rd_population_1911), "matched",
    fifelse(rd_source_id_missing, "excluded_common_land_no_population",
            "unmatched_boundary")
  )
)]

matched_rd_pop_keys <- unique(na.omit(rd_match$rd_pop_key))
rd_audit_pop_only <- rd_population[!rd_pop_key %in% matched_rd_pop_keys]
if (nrow(rd_audit_pop_only) > 0L) {
  rd_audit_pop_only[, `:=`(
    rd_id = NA_character_, rd_official_g_unit = NA_character_,
    rd_source_id_missing = NA, rd_source_row = NA_integer_,
    rd_name = NA_character_, rd_unit_type = NA_character_,
    rd_status = NA_character_, rd_nation = NA_character_, rd_year = 1911L,
    rd_area_m2 = NA_real_, match_method = NA_character_,
    audit_side = "population", match_status = "unmatched_population"
  )]
}
rd_audit <- rbindlist(
  list(rd_audit_boundary, rd_audit_pop_only), fill = TRUE, use.names = TRUE
)
setcolorder(rd_audit, c(
  "audit_side", "match_status", "match_method", "rd_id",
  "rd_official_g_unit", "rd_source_row", "rd_name", "rd_unit_type",
  "rd_regcnty", "rd_regdist", "rd_pop_key", "rd_population_1911",
  "n_registration_subdistrict_rows", "rd_area_m2"
))
setorder(rd_audit, audit_side, rd_source_row, rd_pop_key)

if (rd_audit_boundary[match_status == "matched", .N] != 635L ||
    rd_audit_boundary[match_status == "excluded_common_land_no_population", .N] != 2L ||
    nrow(rd_audit_pop_only) != 0L) {
  stop("RD matching did not reproduce 635 matched districts plus two commons.")
}

###############################################################################
# Match local-government-district populations
###############################################################################

lgd_population_raw <- as.data.table(read_excel(
  lgd_population_file, sheet = "vital_ew_lgd_ann", col_types = "text"
))
required_lgd_columns <- c(
  "year", "adm_cnty", "lg_dist", "lg_type", "part_of", "g_unit",
  "est_pop", "civ_pop"
)
if (!all(required_lgd_columns %in% names(lgd_population_raw))) {
  stop("LGD population workbook is missing required columns.")
}
lgd_population <- lgd_population_raw[
  year == "1911" & toupper(trimws(lg_type)) != "TOTAL",
  .(
    lgd_pop_key = trimws(g_unit),
    population_adm_county = trimws(adm_cnty),
    population_lgd_name = trimws(lg_dist),
    population_lgd_type = trimws(lg_type),
    population_part_of = trimws(part_of),
    lgd_population_1911 = as.numeric(est_pop),
    lgd_civil_population_1911 = as.numeric(civ_pop)
  )
]
assert_unique(lgd_population, "lgd_pop_key", "1911 LGD population")
if (nrow(lgd_population) != 1841L ||
    any(is.na(lgd_population$lgd_population_1911)) ||
    any(lgd_population$lgd_population_1911 <= 0)) {
  stop("Unexpected 1911 LGD population rows or values.")
}

lgd_reference <- unique(area[, .(
  lgd_id, lgd_official_g_unit, lgd_source_id_missing, lgd_source_row,
  lgd_name, lgd_unit_type, lgd_status, lgd_nation, lgd_year, lgd_area_m2
)])
assert_unique(lgd_reference, "lgd_id", "LGD area reference")

lgd_match <- lgd_reference[, .(
  lgd_id,
  lgd_pop_key = as.character(lgd_official_g_unit),
  match_method = "exact_official_g_unit"
)]
lgd_match[lgd_id == "LGD_GUNIT_10074022", `:=`(
  lgd_pop_key = "10062068", match_method = "manual_changed_g_unit"
)]
lgd_match[lgd_id == "LGD_GUNIT_10292992", `:=`(
  lgd_pop_key = "10412808", match_method = "manual_changed_g_unit"
)]
lgd_match[lgd_id == "LGD_GUNIT_10470018", `:=`(
  lgd_pop_key = "10027093", match_method = "manual_combined_spalding_rd"
)]
lgd_match[lgd_id == "LGD_1911_ROW_000284", `:=`(
  lgd_pop_key = NA_character_, match_method = NA_character_
)]
lgd_match[, analysis_lgd_id := fifelse(
  is.na(lgd_pop_key), NA_character_, paste0("LGD_POP_", lgd_pop_key)
)]

lgd_audit_boundary <- merge(
  lgd_reference, lgd_match, by = "lgd_id", all.x = TRUE, sort = FALSE
)
lgd_audit_boundary <- merge(
  lgd_audit_boundary, lgd_population,
  by = "lgd_pop_key", all.x = TRUE, sort = FALSE
)
lgd_audit_boundary[, source_polygons_for_population_unit := .N,
                   by = analysis_lgd_id]
lgd_audit_boundary[, `:=`(
  audit_side = "boundary",
  match_status = fifelse(
    !is.na(lgd_population_1911),
    fifelse(source_polygons_for_population_unit > 1L,
            "matched_combined_boundary_parts", "matched"),
    fifelse(lgd_source_id_missing, "excluded_common_land_no_population",
            "unmatched_boundary")
  )
)]

matched_lgd_pop_keys <- unique(na.omit(lgd_audit_boundary$lgd_pop_key))
lgd_audit_pop_only <- lgd_population[!lgd_pop_key %in% matched_lgd_pop_keys]
lgd_audit_pop_only[, `:=`(
  lgd_id = NA_character_, lgd_official_g_unit = NA_character_,
  lgd_source_id_missing = NA, lgd_source_row = NA_integer_,
  lgd_name = NA_character_, lgd_unit_type = NA_character_,
  lgd_status = NA_character_, lgd_nation = NA_character_, lgd_year = 1911L,
  lgd_area_m2 = NA_real_, match_method = NA_character_,
  analysis_lgd_id = paste0("LGD_POP_", lgd_pop_key),
  source_polygons_for_population_unit = 0L,
  audit_side = "population", match_status = "unmatched_population_no_polygon"
)]
lgd_audit <- rbindlist(
  list(lgd_audit_boundary, lgd_audit_pop_only), fill = TRUE, use.names = TRUE
)
setcolorder(lgd_audit, c(
  "audit_side", "match_status", "match_method", "lgd_id",
  "analysis_lgd_id", "lgd_official_g_unit", "lgd_source_row", "lgd_name",
  "lgd_unit_type", "lgd_pop_key", "population_adm_county",
  "population_lgd_name", "population_lgd_type", "population_part_of",
  "lgd_population_1911", "lgd_civil_population_1911",
  "source_polygons_for_population_unit", "lgd_area_m2"
))
setorder(lgd_audit, audit_side, lgd_source_row, lgd_pop_key)

if (lgd_audit_boundary[!is.na(lgd_population_1911), .N] != 1835L ||
    uniqueN(lgd_audit_boundary[!is.na(lgd_population_1911), analysis_lgd_id]) !=
      1834L ||
    lgd_audit_boundary[match_status == "excluded_common_land_no_population", .N] !=
      1L ||
    nrow(lgd_audit_pop_only) != 7L) {
  stop("LGD matching did not reproduce 1,834 statistical units and 7 exclusions.")
}

###############################################################################
# Build statistical overlap edges and run the three raking specifications
###############################################################################

lgd_analysis <- lgd_audit_boundary[!is.na(lgd_population_1911), .(
  analysis_lgd_area_m2 = sum(lgd_area_m2),
  lgd_population_1911 = unique(lgd_population_1911),
  lgd_civil_population_1911 = unique(lgd_civil_population_1911),
  population_adm_county = unique(population_adm_county),
  population_lgd_name = unique(population_lgd_name),
  population_lgd_type = unique(population_lgd_type),
  source_polygon_count = .N,
  source_lgd_ids = paste(sort(lgd_id), collapse = ";")
), by = analysis_lgd_id]
if (any(lengths(lgd_audit_boundary[!is.na(lgd_population_1911),
                                    .(list(unique(lgd_population_1911))),
                                    by = analysis_lgd_id]$V1) != 1L)) {
  stop("An analytical LGD maps to multiple population totals.")
}

rd_analysis <- rd_audit_boundary[match_status == "matched", .(
  rd_id, rd_name, rd_official_g_unit, rd_area_m2,
  rd_regcnty, rd_regdist, rd_pop_key, rd_population_1911
)]
assert_unique(rd_analysis, "rd_id", "Matched RD analysis units")

lgd_source_map <- lgd_audit_boundary[, .(lgd_id, analysis_lgd_id)]
stat_edges <- merge(
  area[, .(rd_id, lgd_id, overlap_area_m2)],
  lgd_source_map, by = "lgd_id", all.x = TRUE, sort = FALSE
)
stat_edges <- stat_edges[!is.na(analysis_lgd_id) & rd_id %in% rd_analysis$rd_id,
  .(overlap_area_m2 = sum(overlap_area_m2)),
  by = .(rd_id, analysis_lgd_id)
]
stat_edges <- merge(stat_edges, rd_analysis, by = "rd_id", all.x = TRUE,
                    sort = FALSE)
stat_edges <- merge(stat_edges, lgd_analysis, by = "analysis_lgd_id",
                    all.x = TRUE, sort = FALSE)
if (anyNA(stat_edges[, .(rd_population_1911, lgd_population_1911)])) {
  stop("Population margins are missing from matched statistical edges.")
}
stat_edges[, rd_overlap_share := overlap_area_m2 / rd_area_m2]
stat_edges[, lgd_overlap_share := overlap_area_m2 / analysis_lgd_area_m2]
setorder(stat_edges, rd_id, -rd_overlap_share, analysis_lgd_id)
stat_edges[, rd_match_rank := seq_len(.N), by = rd_id]
setorder(stat_edges, analysis_lgd_id, -lgd_overlap_share, rd_id)
stat_edges[, lgd_match_rank := seq_len(.N), by = analysis_lgd_id]

specifications <- list(
  all_positive = copy(stat_edges),
  primary_0_1pct = stat_edges[
    lgd_overlap_share >= 0.001 | lgd_match_rank == 1L | rd_match_rank == 1L
  ],
  sensitivity_1pct = stat_edges[
    lgd_overlap_share >= 0.01 | lgd_match_rank == 1L | rd_match_rank == 1L
  ]
)

for (spec_name in names(specifications)) {
  spec <- specifications[[spec_name]]
  if (uniqueN(spec$analysis_lgd_id) != nrow(lgd_analysis) ||
      uniqueN(spec$rd_id) != nrow(rd_analysis)) {
    stop("Specification ", spec_name, " drops a matched population margin.")
  }
}

message("Raking the primary and sensitivity overlap graphs...")
raking_results <- lapply(
  names(specifications),
  function(nm) {
    message("  Raking specification: ", nm)
    run_ipf(specifications[[nm]], nm)
  }
)
names(raking_results) <- names(specifications)

primary_pairs <- raking_results$primary_0_1pct$pairs
primary_assignment <- raking_results$primary_0_1pct$assignment
all_assignment <- raking_results$all_positive$assignment[, .(
  analysis_lgd_id,
  all_positive_primary_rd_id = rd_id,
  all_positive_primary_rd_name = rd_name,
  all_positive_dominant_population_share = estimated_share_of_lgd_population,
  all_positive_assignment_ge_95pct = population_assignment_ge_95pct
)]
sensitivity_assignment <- raking_results$sensitivity_1pct$assignment[, .(
  analysis_lgd_id,
  sensitivity_1pct_primary_rd_id = rd_id,
  sensitivity_1pct_primary_rd_name = rd_name,
  sensitivity_1pct_dominant_population_share = estimated_share_of_lgd_population,
  sensitivity_1pct_assignment_ge_95pct = population_assignment_ge_95pct
)]

###############################################################################
# Expand primary results back to all 5,158 source-boundary intersections
###############################################################################

source_crosswalk <- merge(
  area, lgd_source_map, by = "lgd_id", all.x = TRUE, sort = FALSE
)
source_crosswalk <- merge(
  source_crosswalk,
  stat_edges[, .(
    rd_id, analysis_lgd_id, statistical_pair_overlap_area_m2 = overlap_area_m2,
    statistical_rd_overlap_share = rd_overlap_share,
    statistical_lgd_overlap_share = lgd_overlap_share,
    statistical_rd_match_rank = rd_match_rank,
    statistical_lgd_match_rank = lgd_match_rank
  )],
  by = c("rd_id", "analysis_lgd_id"), all.x = TRUE, sort = FALSE
)
source_crosswalk <- merge(
  source_crosswalk,
  primary_pairs[, .(
    rd_id, analysis_lgd_id, component_id, rd_margin_scale,
    rd_population_1911, component_scaled_rd_population_1911,
    adjusted_rd_population_1911, rd_feasibility_adjustment_ratio,
    lgd_population_1911, estimated_overlap_pop_1911_statistical_pair =
      estimated_overlap_pop_1911,
    estimated_share_of_lgd_population_statistical_pair =
      estimated_share_of_lgd_population,
    estimated_share_of_adjusted_rd_population_statistical_pair =
      estimated_share_of_adjusted_rd_population,
    population_match_rank, population_lgd_rank_within_rd,
    is_population_primary_rd
  )],
  by = c("rd_id", "analysis_lgd_id"), all.x = TRUE, sort = FALSE
)
source_crosswalk[, included_in_primary_raking :=
                   !is.na(estimated_overlap_pop_1911_statistical_pair)]
source_crosswalk[, source_share_of_statistical_pair_area :=
                   overlap_area_m2 / statistical_pair_overlap_area_m2]
source_crosswalk[, estimated_overlap_pop_1911 :=
                   estimated_overlap_pop_1911_statistical_pair *
                     source_share_of_statistical_pair_area]
source_crosswalk[, estimated_share_of_lgd_population :=
                   estimated_overlap_pop_1911 / lgd_population_1911]
source_crosswalk[, estimated_share_of_adjusted_rd_population :=
                   estimated_overlap_pop_1911 / adjusted_rd_population_1911]
setorder(source_crosswalk, lgd_source_row, -overlap_area_m2, rd_id)

if (nrow(source_crosswalk) != nrow(area)) {
  stop("Population crosswalk no longer has the 5,158 source-area pairs.")
}

###############################################################################
# One-row-per-source-LGD assignments and the seven fair hosts
###############################################################################

area_assignment <- stat_edges[lgd_match_rank == 1L, .(
  analysis_lgd_id,
  area_primary_rd_id = rd_id,
  area_primary_rd_name = rd_name,
  area_dominant_share = lgd_overlap_share,
  area_assignment_ge_95pct = lgd_overlap_share >= assignment_threshold
)]

assignment_stat <- primary_assignment[, .(
  analysis_lgd_id,
  primary_component_id = component_id,
  lgd_population_1911,
  primary_rd_id = rd_id,
  primary_rd_name = rd_name,
  primary_rd_regcnty = rd_regcnty,
  dominant_population_share = estimated_share_of_lgd_population,
  population_assignment_ge_95pct,
  assigned_rd_id, assigned_rd_name, assigned_rd_regcnty,
  estimated_population_in_primary_rd = estimated_overlap_pop_1911
)]
assignment_stat <- merge(assignment_stat, area_assignment,
                         by = "analysis_lgd_id", all.x = TRUE, sort = FALSE)
assignment_stat <- merge(assignment_stat, all_assignment,
                         by = "analysis_lgd_id", all.x = TRUE, sort = FALSE)
assignment_stat <- merge(assignment_stat, sensitivity_assignment,
                         by = "analysis_lgd_id", all.x = TRUE, sort = FALSE)
assignment_stat[, `:=`(
  primary_vs_area_same_rd = primary_rd_id == area_primary_rd_id,
  primary_vs_all_positive_same_rd = primary_rd_id == all_positive_primary_rd_id,
  primary_vs_1pct_same_rd = primary_rd_id == sensitivity_1pct_primary_rd_id
)]

assignment <- merge(
  lgd_audit_boundary[, .(
    lgd_id, analysis_lgd_id, lgd_official_g_unit, lgd_source_row,
    lgd_name, lgd_unit_type, lgd_status, lgd_nation, lgd_area_m2,
    population_adm_county, population_lgd_name, population_lgd_type,
    lgd_population_1911, source_polygons_for_population_unit, match_status
  )],
  assignment_stat,
  by = c("analysis_lgd_id", "lgd_population_1911"), all.x = TRUE,
  sort = FALSE
)
setorder(assignment, lgd_source_row)

###############################################################################
# LGD calibration audit
###############################################################################

# The primary IPF treats published LGD populations as exact column margins.
# This audit makes that accounting identity explicit and records how each LGD
# total is distributed across its contributing RDs. It is not an out-of-sample
# comparison or an independently reconstructed population estimate.
primary_pairs_ordered <- copy(primary_pairs)
setorder(
  primary_pairs_ordered, analysis_lgd_id, population_match_rank, rd_id
)
calibration_audit <- primary_pairs_ordered[, .(
  primary_component_id = unique(component_id),
  observed_lgd_population_1911 = unique(lgd_population_1911),
  reconstructed_lgd_population_1911 = sum(estimated_overlap_pop_1911),
  n_contributing_rds = .N,
  dominant_rd_id = rd_id[[1L]],
  dominant_rd_name = rd_name[[1L]],
  dominant_rd_population = estimated_overlap_pop_1911[[1L]],
  dominant_rd_population_share = estimated_share_of_lgd_population[[1L]],
  contributing_rd_ids = paste(rd_id, collapse = ";"),
  contributing_rd_names = paste(rd_name, collapse = ";"),
  contributing_rd_populations = paste(
    paste0(rd_name, "=", sprintf("%.6f", estimated_overlap_pop_1911)),
    collapse = ";"
  ),
  contributing_rd_population_shares = paste(
    paste0(
      rd_name, "=", sprintf("%.12f", estimated_share_of_lgd_population)
    ),
    collapse = ";"
  ),
  component_rd_margin_scale = unique(rd_margin_scale),
  max_abs_rd_feasibility_adjustment = max(
    abs(rd_feasibility_adjustment_ratio - 1)
  )
), by = analysis_lgd_id]
calibration_audit[, `:=`(
  calibration_difference =
    reconstructed_lgd_population_1911 - observed_lgd_population_1911,
  calibration_relative_error = relative_error(
    reconstructed_lgd_population_1911, observed_lgd_population_1911
  )
)]

calibration_audit <- merge(
  calibration_audit,
  lgd_analysis[, .(
    analysis_lgd_id, population_adm_county, population_lgd_name,
    population_lgd_type, source_polygon_count, source_lgd_ids
  )],
  by = "analysis_lgd_id", all.x = TRUE, sort = FALSE
)

primary_pair_keys <- unique(primary_pairs[, .(rd_id, analysis_lgd_id)])
excluded_edges <- stat_edges[
  !primary_pair_keys, on = .(rd_id, analysis_lgd_id),
  .(
    excluded_positive_edge_count = .N,
    excluded_positive_overlap_area_m2 = sum(overlap_area_m2)
  ),
  by = analysis_lgd_id
]
calibration_audit <- merge(
  calibration_audit, excluded_edges,
  by = "analysis_lgd_id", all.x = TRUE, sort = FALSE
)
calibration_audit[is.na(excluded_positive_edge_count), `:=`(
  excluded_positive_edge_count = 0L,
  excluded_positive_overlap_area_m2 = 0
)]
calibration_audit[, calibration_status := fifelse(
  calibration_relative_error < ipf_tolerance,
  "exact_within_tolerance", "outside_tolerance"
)]
setcolorder(calibration_audit, c(
  "analysis_lgd_id", "population_adm_county", "population_lgd_name",
  "population_lgd_type", "source_polygon_count", "source_lgd_ids",
  "primary_component_id", "observed_lgd_population_1911",
  "reconstructed_lgd_population_1911", "calibration_difference",
  "calibration_relative_error", "calibration_status",
  "n_contributing_rds", "dominant_rd_id", "dominant_rd_name",
  "dominant_rd_population", "dominant_rd_population_share",
  "contributing_rd_ids", "contributing_rd_names",
  "contributing_rd_populations", "contributing_rd_population_shares",
  "component_rd_margin_scale", "max_abs_rd_feasibility_adjustment",
  "excluded_positive_edge_count", "excluded_positive_overlap_area_m2"
))
setorder(calibration_audit, population_adm_county, population_lgd_name)

liverpool_audit <- calibration_audit[
  toupper(population_lgd_name) == "LIVERPOOL" &
    toupper(population_lgd_type) == "CB"
]
if (nrow(liverpool_audit) != 1L ||
    liverpool_audit$observed_lgd_population_1911 != 747535) {
  stop("Expected exactly one Liverpool CB calibration row with population 747,535.")
}
liverpool_pairs <- primary_pairs_ordered[
  analysis_lgd_id == liverpool_audit$analysis_lgd_id
]
if (abs(sum(liverpool_pairs$estimated_share_of_lgd_population) - 1) >=
    ipf_tolerance) {
  stop("Liverpool calibrated RD contribution shares do not sum to one.")
}

fairs <- fread(fairs_file, na.strings = c("", "NA"))
fair_hosts <- fairs[parent_fair_id %in% fair_parent_ids]
if (nrow(fair_hosts) != length(fair_parent_ids) ||
    uniqueN(fair_hosts$parent_fair_id) != length(fair_parent_ids)) {
  stop("Expected one venue row for each of the seven selected fair events.")
}
fair_points <- st_as_sf(
  fair_hosts,
  coords = c("venue_longitude", "venue_latitude"),
  crs = 4326, remove = FALSE
)
fair_points <- st_transform(fair_points, st_crs(lgd_sf))
fair_matches <- st_intersects(fair_points, lgd_sf)
fair_match_count <- lengths(fair_matches)
if (any(fair_match_count != 1L)) {
  stop(
    "Fair-host point matching did not produce exactly one LGD for every fair: ",
    paste(fair_hosts$parent_fair_id, fair_match_count, sep = "=", collapse = ", ")
  )
}
fair_host_assignment <- as.data.table(st_drop_geometry(fair_points))
fair_host_assignment[, `:=`(
  lgd_id = lgd_sf$lgd_id[unlist(fair_matches)],
  point_match_count = fair_match_count
)]
fair_host_assignment <- merge(
  fair_host_assignment,
  assignment,
  by = "lgd_id", all.x = TRUE, sort = FALSE
)
setorder(fair_host_assignment, Year, parent_fair_id)

###############################################################################
# Quality-control table
###############################################################################

qc <- data.table(
  section = character(), spec = character(), metric = character(),
  value = character(), flag = character(), detail = character()
)
add_qc <- function(section, spec, metric, value, flag = "ok", detail = NA) {
  qc <<- rbind(
    qc,
    data.table(
      section = as.character(section), spec = as.character(spec),
      metric = as.character(metric), value = as.character(value),
      flag = as.character(flag), detail = as.character(detail)
    )
  )
}

add_qc("source", "all", "area_crosswalk_rows", nrow(area))
add_qc("source", "all", "rd_population_total", sum(rd_population$rd_population_1911))
add_qc("source", "all", "lgd_population_total", sum(lgd_population$lgd_population_1911))
add_qc("matching", "all", "matched_rd_boundary_units",
       rd_audit_boundary[match_status == "matched", .N])
add_qc("matching", "all", "rd_common_land_exclusions",
       rd_audit_boundary[match_status == "excluded_common_land_no_population", .N])
add_qc("matching", "all", "matched_lgd_source_polygons",
       lgd_audit_boundary[!is.na(lgd_population_1911), .N])
add_qc("matching", "all", "matched_lgd_statistical_units", nrow(lgd_analysis))
add_qc("matching", "all", "lgd_population_rows_without_polygon",
       nrow(lgd_audit_pop_only), "review",
       paste(lgd_audit_pop_only$population_lgd_name, collapse = "; "))
add_qc("matching", "all", "combined_spalding_source_polygons", 2L, "review",
       "Boundary IDs 10027093 and 10470018 share population unit 10027093.")
add_qc(
  "calibration", "primary_0_1pct", "max_lgd_calibration_relative_error",
  signif(max(calibration_audit$calibration_relative_error), 12)
)
add_qc(
  "calibration", "primary_0_1pct", "lgds_outside_calibration_tolerance",
  calibration_audit[calibration_status == "outside_tolerance", .N]
)
add_qc(
  "calibration", "primary_0_1pct", "liverpool_reconstructed_population",
  signif(liverpool_audit$reconstructed_lgd_population_1911, 12),
  "ok", "Observed LGD population is an exact IPF calibration margin."
)

for (spec_name in names(raking_results)) {
  result <- raking_results[[spec_name]]
  add_qc("raking", spec_name, "included_statistical_edges", nrow(result$pairs))
  add_qc("raking", spec_name, "connected_components",
         nrow(result$component_qc))
  add_qc("raking", spec_name, "iterations",
         unique(result$component_qc$iterations))
  add_qc("raking", spec_name, "max_lgd_margin_relative_error",
         signif(max(result$component_qc$max_lgd_margin_relative_error), 12))
  add_qc("raking", spec_name, "max_rd_margin_relative_error",
         signif(max(result$component_qc$max_rd_margin_relative_error), 12))
  add_qc(
    "raking", spec_name, "exact_component_scaled_rd_margins_feasible",
    all(result$component_qc$exact_original_margin_convergence),
    fifelse(all(result$component_qc$exact_original_margin_convergence),
            "ok", "review")
  )
  add_qc("raking", spec_name, "max_rd_feasibility_revision",
         signif(max(result$component_qc$max_rd_feasibility_revision), 12),
         fifelse(max(result$component_qc$max_rd_feasibility_revision) < 0.05,
                 "ok", "review"))
  add_qc("assignment", spec_name, "lgds_assigned_ge_95pct",
         result$assignment[population_assignment_ge_95pct == TRUE, .N])

  component_rows <- result$component_qc[, .(
    section = "component",
    spec = spec_name,
    metric = paste0("component_", component_id, "_rd_margin_scale"),
    value = as.character(signif(rd_margin_scale, 12)),
    flag = fifelse(rd_scale_outside_5pct, "review", "ok"),
    detail = paste0(
      "n_lgds=", n_lgds, "; n_rds=", n_rds,
      "; lgd_total=", signif(lgd_population_total, 12),
      "; rd_total=", signif(rd_population_total, 12),
      "; max_rd_feasibility_revision=",
      signif(max_rd_feasibility_revision, 6)
    )
  )]
  qc <- rbind(qc, component_rows, fill = TRUE)
}
add_qc("fair_hosts", "primary_0_1pct", "selected_fair_events",
       nrow(fair_host_assignment))
add_qc("fair_hosts", "primary_0_1pct", "fair_events_in_assigned_lgd",
       fair_host_assignment[population_assignment_ge_95pct == TRUE, .N])

###############################################################################
# Write outputs
###############################################################################

fwrite(rd_audit, rd_audit_file, na = "")
fwrite(lgd_audit, lgd_audit_file, na = "")
fwrite(source_crosswalk, population_crosswalk_file, na = "")
fwrite(assignment, assignment_file, na = "")
fwrite(calibration_audit, calibration_audit_file, na = "")
fwrite(qc, raking_qc_file, na = "")
fwrite(fair_host_assignment, fair_assignment_file, na = "")

###############################################################################
# Figures
###############################################################################

assignment_plot_data <- merge(
  lgd_sf,
  assignment[, .(
    lgd_id, population_assignment_ge_95pct, assigned_rd_name,
    dominant_population_share
  )],
  by = "lgd_id", all.x = TRUE, sort = FALSE
)
assignment_plot_data$assignment_status <- ifelse(
  assignment_plot_data$population_assignment_ge_95pct %in% TRUE,
  "Assigned (>=95%)",
  ifelse(is.na(assignment_plot_data$dominant_population_share),
         "No 1911 population match", "Not assigned (<95%)")
)
assignment_plot_data$assignment_status <- factor(
  assignment_plot_data$assignment_status,
  levels = c("Assigned (>=95%)", "Not assigned (<95%)",
             "No 1911 population match")
)

fair_label_points <- fair_points[fair_points$parent_fair_id != 391L, ]
fair_label_points$label <- paste0(
  fair_label_points$City, " (", fair_label_points$Year, ")"
)
fair_label_points$label[
  fair_label_points$parent_fair_id == 175L
] <- "Newcastle (1887, 1929)"

assignment_map <- ggplot() +
  geom_sf(
    data = assignment_plot_data,
    aes(fill = assignment_status), color = NA, linewidth = 0
  ) +
  geom_sf(data = rd_sf, fill = NA, color = "grey35", linewidth = 0.08) +
  geom_sf(data = fair_label_points, color = "#7A0019", size = 1.5) +
  geom_sf_text(
    data = fair_label_points, aes(label = label),
    color = "#7A0019", size = 2.5, nudge_y = 10000, check_overlap = TRUE
  ) +
  scale_fill_manual(
    values = c(
      "Assigned (>=95%)" = "#5AAE61",
      "Not assigned (<95%)" = "#D95F5F",
      "No 1911 population match" = "grey80"
    ), drop = FALSE
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "1911 LGDs assigned to registration districts by population",
    subtitle = paste0(
      "Area-seeded IPF; assignment requires >=95% of estimated LGD population"
    ),
    fill = NULL,
    caption = "Thin lines show 1911 registration-district boundaries."
  ) +
  theme_void(base_size = 10) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  assignment_map_file, assignment_map,
  width = 8.5, height = 10, dpi = 300, bg = "white"
)

scatter_data <- assignment_stat
scatter_data[, assigned := population_assignment_ge_95pct]
fair_analysis_ids <- unique(fair_host_assignment$analysis_lgd_id)
scatter_data[, fair_host := analysis_lgd_id %in% fair_analysis_ids]

share_scatter <- ggplot(
  scatter_data,
  aes(x = area_dominant_share, y = dominant_population_share)
) +
  geom_hline(yintercept = assignment_threshold, color = "grey55",
             linewidth = 0.35, linetype = 2) +
  geom_vline(xintercept = assignment_threshold, color = "grey55",
             linewidth = 0.35, linetype = 2) +
  geom_abline(slope = 1, intercept = 0, color = "grey75", linewidth = 0.35) +
  geom_point(
    aes(color = assigned), alpha = 0.55, size = 1.25
  ) +
  geom_point(
    data = scatter_data[fair_host == TRUE],
    shape = 21, fill = "#FFD92F", color = "black", size = 3, stroke = 0.6
  ) +
  scale_color_manual(
    values = c(`TRUE` = "#238B45", `FALSE` = "#CB181D"),
    labels = c(`TRUE` = "Assigned (>=95%)", `FALSE` = "Not assigned (<95%)")
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), oob = scales::squish) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), oob = scales::squish) +
  labs(
    title = "Dominant registration district: area share vs population share",
    subtitle = "Yellow rings identify LGDs containing the seven selected fair events",
    x = "Largest share of LGD area",
    y = "Largest estimated share of LGD population",
    color = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  share_scatter_file, share_scatter,
  width = 8, height = 6.5, dpi = 300, bg = "white"
)

###############################################################################
# Final validation and console summary
###############################################################################

primary_lgd_check <- primary_pairs[, .(
  actual = sum(estimated_overlap_pop_1911),
  target = unique(lgd_population_1911)
), by = analysis_lgd_id]
primary_rd_check <- primary_pairs[, .(
  actual = sum(estimated_overlap_pop_1911),
  target = unique(adjusted_rd_population_1911)
), by = rd_id]
if (max(relative_error(primary_lgd_check$actual, primary_lgd_check$target)) >=
      ipf_tolerance ||
    max(relative_error(primary_rd_check$actual, primary_rd_check$target)) >=
      ipf_tolerance) {
  stop("Primary raking margins fail the final tolerance check.")
}
if (any(calibration_audit$calibration_status != "exact_within_tolerance")) {
  stop("One or more LGD calibration margins fail the audit tolerance check.")
}
if (source_crosswalk[
      included_in_primary_raking == FALSE &
        !is.na(estimated_overlap_pop_1911), .N
    ] > 0L) {
  stop("Excluded positive-area edges unexpectedly carry calibrated population.")
}

message("Population-weighted 1911 LGD-RD crosswalk complete.")
message("  Matched statistical LGDs: ", nrow(lgd_analysis))
message(
  "  Primary assignments >=95%: ",
  primary_assignment[population_assignment_ge_95pct == TRUE, .N]
)
message(
  "  Fair events in assigned LGDs: ",
  fair_host_assignment[population_assignment_ge_95pct == TRUE, .N],
  " / ", nrow(fair_host_assignment)
)
message("  Crosswalk: ", population_crosswalk_file)
message("  Assignments: ", assignment_file)
message("  Calibration audit: ", calibration_audit_file)
message("  Fair-host audit: ", fair_assignment_file)
message("  Liverpool CB calibrated RD contributions:")
for (i in seq_len(nrow(liverpool_pairs))) {
  message(
    "    ", liverpool_pairs$rd_name[[i]], ": ",
    format(round(liverpool_pairs$estimated_overlap_pop_1911[[i]]),
           big.mark = ",", scientific = FALSE),
    " (", sprintf("%.3f%%",
                   100 * liverpool_pairs$estimated_share_of_lgd_population[[i]]),
    ")"
  )
}
message(
  "    Total: ",
  format(round(liverpool_audit$reconstructed_lgd_population_1911),
         big.mark = ",", scientific = FALSE),
  " = observed Liverpool CB population (calibration identity)"
)
