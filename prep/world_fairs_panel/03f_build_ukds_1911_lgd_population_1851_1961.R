###############################################################################
# Extend the fixed-1911 LGD population reconstruction through 1961 and build
# annual population and estimated-birth denominators for 1851--1961.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/03f_build_ukds_1911_lgd_population_1851_1961.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(sf)
})

sf_use_s2(FALSE)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]),
                              winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                             winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  candidate <- file.path("C:/Users", Sys.info()[["user"]],
                         "Globtalent Dropbox", "gtl_talent_dets")
  if (dir.exists(candidate)) TALENT_DETS_DATA_DIR <- candidate
}

processed_dir <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "processed", "worlds_fairs",
  "ukds_1911_boundary_crosswalk"
)
polygon_root <- file.path(
  TALENT_DETS_DATA_DIR, "raw", "ukds_polygons", "UKDA-9321-SHP", "shp"
)
lgd_vital_file <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "raw", "world_fairs", "ukds_birth_data",
  "UKDA-9035-excel", "excel", "vital_ew_lgd_ann.xlsx"
)
rd_vital_file <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "raw", "world_fairs", "ukds_birth_data",
  "UKDA-4570-excel", "excel", "vital_ew_reg_ann.xlsx"
)
overlap_gpkg <- file.path(processed_dir, "ukds_1911_registration_lgd_overlap.gpkg")
crosswalk_file <- file.path(processed_dir, "ukds_1911_lgd_rd_population_crosswalk.csv")
calibration_file <- file.path(processed_dir, "ukds_1911_lgd_calibrated_population_audit.csv")
match_1911_file <- file.path(processed_dir, "ukds_1911_lgd_population_match_audit.csv")
backcast_file <- file.path(processed_dir, "ukds_1911_lgd_population_backcast_1851_1901.csv")

forward_file <- file.path(processed_dir, "ukds_1911_lgd_population_forward_1921_1961.csv")
weights_file <- file.path(processed_dir, "ukds_later_lgd_to_1911_lgd_weights_1921_1961.csv")
match_audit_file <- file.path(processed_dir, "ukds_later_lgd_population_match_audit_1921_1961.csv")
knots_file <- file.path(processed_dir, "ukds_1911_lgd_population_knots_1851_1961.csv")
annual_file <- file.path(processed_dir, "ukds_1911_lgd_population_annual_1851_1961.csv")
births_file <- file.path(processed_dir, "ukds_ew_national_births_1851_1961.csv")
geometry_file <- file.path(processed_dir, "ukds_1911_lgd_analysis_geometries.gpkg")
qc_file <- file.path(processed_dir, "ukds_1911_lgd_population_1851_1961_qc.csv")

later_years <- c(1921L, 1931L, 1951L, 1961L)
knot_years <- c(1851L, 1861L, 1871L, 1881L, 1891L, 1901L,
                1911L, later_years)
analysis_crs <- 27700L
tolerance <- 1e-8

later_shapes <- file.path(
  polygon_root, paste0("ukds_ew", later_years, "_lgdistricts2"),
  paste0("ew", later_years, "_lgdistricts.shp")
)
required <- c(overlap_gpkg, crosswalk_file, calibration_file, match_1911_file,
              backcast_file, lgd_vital_file, rd_vital_file, later_shapes)
missing <- required[!file.exists(required)]
if (length(missing)) stop("Missing inputs:\n", paste(missing, collapse = "\n"))
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

relative_error <- function(actual, target) {
  abs(actual - target) / pmax(abs(target), 1)
}
assert_unique <- function(x, columns, label) {
  if (anyDuplicated(x[, ..columns]))
    stop(label, " is not unique by ", paste(columns, collapse = ", "))
}

message("Reading the calibrated 1911 density cells and target LGDs...")
cells <- st_read(overlap_gpkg, layer = "positive_area_intersections", quiet = TRUE)
cells <- st_transform(st_make_valid(cells), analysis_crs)
cw <- fread(crosswalk_file, na.strings = c("", "NA"))[
  included_in_primary_raking == TRUE,
  .(rd_id, lgd_id, analysis_lgd_id,
    cell_population_1911 = estimated_overlap_pop_1911)
]
assert_unique(cw, c("rd_id", "lgd_id"), "Calibrated cells")
cells <- merge(cells[, c("rd_id", "lgd_id")], cw,
               by = c("rd_id", "lgd_id"), all = FALSE, sort = FALSE)
cells$cell_area_m2 <- as.numeric(st_area(cells))
cells$cell_density_1911 <- cells$cell_population_1911 / cells$cell_area_m2
if (any(!is.finite(cells$cell_density_1911) | cells$cell_density_1911 <= 0))
  stop("Invalid calibrated cell densities.")

reference <- fread(calibration_file, na.strings = c("", "NA"))[, .(
  analysis_lgd_id, population_adm_county, population_lgd_name,
  population_lgd_type, source_polygon_count, source_lgd_ids,
  population = observed_lgd_population_1911
)]
assert_unique(reference, "analysis_lgd_id", "Target LGD reference")
if (nrow(reference) != 1834L) stop("Expected 1,834 analytical LGDs.")

boundary_1911 <- st_read(overlap_gpkg, layer = "local_government_districts_1911",
                         quiet = TRUE)
boundary_1911 <- st_transform(st_make_valid(boundary_1911), analysis_crs)
audit_1911 <- fread(match_1911_file, na.strings = c("", "NA"))[
  audit_side == "boundary" &
    match_status %in% c("matched", "matched_combined_boundary_parts"),
  .(lgd_id, analysis_lgd_id)
]
audit_1911 <- unique(audit_1911)
target <- merge(boundary_1911[, "lgd_id"], audit_1911,
                by = "lgd_id", all = FALSE, sort = FALSE)
target <- aggregate(target[, "analysis_lgd_id"],
                    by = list(analysis_lgd_id = target$analysis_lgd_id),
                    FUN = function(x) x[1L])
target <- merge(target, as.data.frame(reference[, .(
  analysis_lgd_id, population_adm_county, population_lgd_name,
  population_lgd_type
)]), by = "analysis_lgd_id", all.x = TRUE, sort = FALSE)
target <- st_make_valid(target)
if (nrow(target) != 1834L || anyNA(target$population_lgd_name))
  stop("Fixed 1911 target geometry is incomplete.")
if (file.exists(geometry_file)) file.remove(geometry_file)
st_write(target, geometry_file, layer = "fixed_1911_lgds", quiet = TRUE)

message("Reading LGD vital-statistics workbook...")
vital_lgd <- as.data.table(read_excel(lgd_vital_file, sheet = "vital_ew_lgd_ann"))
vital_lgd[, `:=`(year = as.integer(year), g_unit = as.integer(g_unit),
                 est_pop = as.numeric(est_pop), births = as.numeric(births))]

# The 1921 Morecambe polygon retains an obsolete G_UNIT. UKDS links its
# population series under 10062068. G_UNIT zero is common land and is excluded.
gunit_overrides <- data.table(
  year = 1921L, shape_g_unit = 10074022L, population_g_unit = 10062068L,
  override_reason = "Morecambe obsolete boundary identifier"
)

process_later_year <- function(yr) {
  message("Reconstructing fixed-1911 LGD population from ", yr, " LGDs...")
  shape_file <- file.path(polygon_root, paste0("ukds_ew", yr, "_lgdistricts2"),
                          paste0("ew", yr, "_lgdistricts.shp"))
  source <- st_read(shape_file, quiet = TRUE)
  source <- st_transform(st_make_valid(source), analysis_crs)
  source$shape_row <- seq_len(nrow(source))
  source$shape_g_unit <- suppressWarnings(as.integer(source$G_UNIT))
  source$population_g_unit <- source$shape_g_unit
  override <- gunit_overrides[year == yr]
  if (nrow(override)) {
    source$population_g_unit[
      source$shape_g_unit %in% override$shape_g_unit
    ] <- override$population_g_unit[
      match(source$shape_g_unit[source$shape_g_unit %in% override$shape_g_unit],
            override$shape_g_unit)
    ]
  }

  pop <- vital_lgd[year == yr & !is.na(g_unit) & g_unit != 10000000L,
    .(source_population = sum(est_pop, na.rm = TRUE),
      workbook_rows = .N,
      workbook_lgd_names = paste(sort(unique(lg_dist)), collapse = " | "),
      workbook_lgd_types = paste(sort(unique(lg_type)), collapse = " | ")),
    by = .(population_g_unit = g_unit)
  ]
  source_attr <- as.data.table(st_drop_geometry(source))[, .(
    shape_row, shape_g_unit, population_g_unit,
    boundary_lgd_name = as.character(G_NAME),
    boundary_lgd_type = as.character(G_STATUS)
  )]
  source_attr <- merge(source_attr, pop, by = "population_g_unit",
                       all.x = TRUE, sort = FALSE)
  source_attr[, `:=`(
    year = yr,
    match_status = fifelse(
      !is.na(source_population), "matched",
      fifelse(is.na(shape_g_unit) | shape_g_unit == 0L,
              "excluded_common_land_no_population", "unmatched_boundary")
    ),
    match_method = fifelse(
      shape_g_unit != population_g_unit, "manual_g_unit_override",
      "exact_g_unit"
    )
  )]
  unmatched <- source_attr[match_status == "unmatched_boundary"]
  if (nrow(unmatched)) {
    print(unmatched[, .(shape_g_unit, boundary_lgd_name, boundary_lgd_type)])
    stop("Unmatched populated ", yr, " boundary units.")
  }
  matched_rows <- source_attr[match_status == "matched", shape_row]
  source_matched <- source[source$shape_row %in% matched_rows, ]
  source_matched <- merge(source_matched,
    as.data.frame(source_attr[match_status == "matched", .(
      shape_row, source_population
    )]), by = "shape_row", all.x = TRUE, sort = FALSE)

  # Dissolve repeated boundary pieces and repeated workbook divisions to the
  # population G_UNIT before computing allocation weights.
  source_dissolved <- aggregate(
    source_matched[, c("population_g_unit", "source_population")],
    by = list(population_g_unit = source_matched$population_g_unit),
    FUN = function(x) x[1L]
  )
  source_dissolved <- st_make_valid(source_dissolved)
  source_dissolved$source_area_m2 <- as.numeric(st_area(source_dissolved))

  pieces <- suppressWarnings(st_intersection(
    source_dissolved[, c("population_g_unit", "source_population",
                         "source_area_m2")],
    cells[, c("analysis_lgd_id", "cell_density_1911")]
  ))
  pieces$intersection_area_m2 <- as.numeric(st_area(pieces))
  pdt <- as.data.table(st_drop_geometry(pieces))[
    intersection_area_m2 > 0 & cell_density_1911 > 0
  ]
  pdt[, seed_population_1911 := intersection_area_m2 * cell_density_1911]
  weights <- pdt[, .(
    intersection_area_m2 = sum(intersection_area_m2),
    seed_population_1911 = sum(seed_population_1911),
    source_population = unique(source_population),
    source_area_m2 = unique(source_area_m2)
  ), by = .(population_g_unit, analysis_lgd_id)]
  weights[, allocation_seed_method := "calibrated_1911_cell_density"]

  # A tiny institutional LGD can sit in a gap between the two 1911 source
  # systems (Nottingham Shire Hall in 1921 has population six). Preserve its
  # population with a transparent target-area fallback rather than dropping it.
  missing_seed_units <- setdiff(source_dissolved$population_g_unit,
                                weights$population_g_unit)
  if (length(missing_seed_units)) {
    fallback_pieces <- suppressWarnings(st_intersection(
      source_dissolved[source_dissolved$population_g_unit %in%
                         missing_seed_units,
                       c("population_g_unit", "source_population",
                         "source_area_m2")],
      target[, "analysis_lgd_id"]
    ))
    fallback_pieces$intersection_area_m2 <- as.numeric(st_area(fallback_pieces))
    fallback <- as.data.table(st_drop_geometry(fallback_pieces))[
      intersection_area_m2 > 0,
      .(intersection_area_m2 = sum(intersection_area_m2),
        seed_population_1911 = sum(intersection_area_m2),
        source_population = unique(source_population),
        source_area_m2 = unique(source_area_m2),
        allocation_seed_method = "target_area_fallback_no_density_cell"),
      by = .(population_g_unit, analysis_lgd_id)
    ]
    weights <- rbindlist(list(weights, fallback), fill = TRUE)
  }
  weights[, `:=`(
    covered_area_m2 = sum(intersection_area_m2),
    seed_total_1911 = sum(seed_population_1911)
  ), by = population_g_unit]
  weights[, `:=`(
    source_surface_coverage_share = covered_area_m2 / source_area_m2,
    later_lgd_to_1911_lgd_weight = seed_population_1911 / seed_total_1911
  )]
  weights[, population_contribution :=
            source_population * later_lgd_to_1911_lgd_weight]
  weights[, year := yr]

  check <- weights[, .(
    weight_sum = sum(later_lgd_to_1911_lgd_weight),
    reconstructed_source_population = sum(population_contribution),
    source_population = unique(source_population)
  ), by = population_g_unit]
  if (nrow(check) != nrow(source_dissolved) ||
      max(abs(check$weight_sum - 1)) > tolerance ||
      max(relative_error(check$reconstructed_source_population,
                         check$source_population)) > tolerance) {
    message("  source units: ", nrow(source_dissolved),
            "; units with positive calibrated overlap: ", nrow(check),
            "; max weight error: ", max(abs(check$weight_sum - 1)))
    missing_overlap <- setdiff(source_dissolved$population_g_unit,
                               check$population_g_unit)
    if (length(missing_overlap))
      message("  no-overlap G_UNITs: ", paste(missing_overlap, collapse = ", "))
    stop(yr, " allocation weights do not conserve population.")
  }

  panel <- weights[, .(
    population = sum(population_contribution),
    n_contributing_source_lgds = .N,
    minimum_source_surface_coverage = min(source_surface_coverage_share),
    population_weighted_surface_coverage = weighted.mean(
      source_surface_coverage_share, population_contribution)
  ), by = analysis_lgd_id]
  panel <- merge(reference[, setdiff(names(reference), "population"), with = FALSE],
                 panel, by = "analysis_lgd_id", all.x = TRUE, sort = FALSE)
  panel[, `:=`(year = yr,
               population_method = paste0("calibrated_1911_density_forward_", yr),
               population_interpolated = FALSE)]
  if (anyNA(panel$population)) stop(yr, " fixed-LGD population is incomplete.")

  pop_without_polygon <- pop[!population_g_unit %in% source_attr$population_g_unit]
  if (nrow(pop_without_polygon)) pop_without_polygon[, `:=`(
    year = yr, match_status = "unmatched_population",
    match_method = NA_character_, shape_row = NA_integer_,
    shape_g_unit = NA_integer_, boundary_lgd_name = NA_character_,
    boundary_lgd_type = NA_character_
  )]
  audit <- rbindlist(list(source_attr, pop_without_polygon), fill = TRUE)
  list(panel = panel, weights = weights, audit = audit,
       source_total = sum(check$source_population),
       reconstructed_total = sum(check$reconstructed_source_population),
       min_coverage = min(weights$source_surface_coverage_share),
       unmatched_population = nrow(pop_without_polygon))
}

later_results <- lapply(later_years, process_later_year)
names(later_results) <- as.character(later_years)
forward <- rbindlist(lapply(later_results, `[[`, "panel"), fill = TRUE)
weights <- rbindlist(lapply(later_results, `[[`, "weights"), fill = TRUE)
match_audit <- rbindlist(lapply(later_results, `[[`, "audit"), fill = TRUE)

message("Assembling census knots and annual linear interpolation...")
backcast <- fread(backcast_file, na.strings = c("", "NA"))[, .(
  analysis_lgd_id, population_adm_county, population_lgd_name,
  population_lgd_type, source_polygon_count, source_lgd_ids,
  year = as.integer(year),
  population = as.numeric(reconstructed_lgd_population),
  population_method, population_interpolated
)]
observed_1911 <- copy(reference)
observed_1911[, `:=`(year = 1911L,
  population_method = "observed_1911_lgd_calibration_target",
  population_interpolated = FALSE)]
forward_knots <- forward[, .(
  analysis_lgd_id, population_adm_county, population_lgd_name,
  population_lgd_type, source_polygon_count, source_lgd_ids, year,
  population, population_method, population_interpolated
)]
knots <- rbindlist(list(backcast, observed_1911, forward_knots), fill = TRUE)
setorder(knots, analysis_lgd_id, year)
if (nrow(knots) != 1834L * length(knot_years) ||
    anyNA(knots$population) || !setequal(unique(knots$year), knot_years))
  stop("Population knots are not a complete 1,834 by 11 panel.")

annual <- knots[, {
  years <- 1851L:1961L
  vals <- approx(year, population, xout = years, method = "linear",
                 rule = 1, ties = "ordered")$y
  data.table(year = years, population = vals,
             population_is_census_knot = years %in% year)
}, by = .(analysis_lgd_id, population_adm_county, population_lgd_name,
          population_lgd_type, source_polygon_count, source_lgd_ids)]
annual[, population_method := fifelse(
  population_is_census_knot, "census_knot", "linear_interpolation_between_knots"
)]
if (nrow(annual) != 1834L * 111L || anyNA(annual$population))
  stop("Annual population panel is incomplete.")

message("Building annual national-birth-rate denominators...")
vital_rd <- as.data.table(read_excel(rd_vital_file, sheet = "vital_ew_reg_ann"))
national_early <- vital_rd[
  row_type == "Nation" & year >= 1851 & year <= 1910,
  .(national_births = sum(as.numeric(birth_tot), na.rm = TRUE)),
  by = .(year = as.integer(year))
]
national_late <- vital_lgd[year >= 1911 & year <= 1961, {
  total_value <- births[lg_type == "TOTAL" & !is.na(births)]
  value <- if (length(total_value)) sum(total_value) else
    sum(births[lg_type != "TOTAL" | is.na(lg_type)], na.rm = TRUE)
  .(national_births = value,
    national_births_method = if (length(total_value))
      "published_national_total" else "sum_lgd_rows")
}, by = year]
national_early[, national_births_method := "published_national_total"]
national_births <- rbindlist(list(national_early, national_late), fill = TRUE)
national_pop <- annual[, .(reconstructed_national_population = sum(population)),
                       by = year]
national_births <- merge(national_pop, national_births, by = "year", all.x = TRUE)
national_births[, national_birth_rate :=
                  national_births / reconstructed_national_population]
if (nrow(national_births) != 111L || anyNA(national_births$national_births) ||
    any(national_births$national_births <= 0))
  stop("National births are incomplete for 1851--1961.")
annual <- merge(annual, national_births[, .(year, national_births,
  national_birth_rate)], by = "year", all.x = TRUE, sort = FALSE)
annual[, estimated_births := population * national_birth_rate]

birth_check <- annual[, .(allocated_births = sum(estimated_births)), by = year]
birth_check <- merge(birth_check, national_births[, .(year, national_births)],
                     by = "year")
if (max(relative_error(birth_check$allocated_births,
                       birth_check$national_births)) > tolerance)
  stop("Estimated LGD births do not sum to national births.")

qc <- rbindlist(lapply(later_years, function(year) {
  x <- later_results[[as.character(year)]]
  data.table(year = year, source_population_total = x$source_total,
    reconstructed_population_total = x$reconstructed_total,
    population_conservation_relative_error = relative_error(
      x$reconstructed_total, x$source_total),
    minimum_source_surface_coverage = x$min_coverage,
    unmatched_population_rows = x$unmatched_population,
    fixed_lgd_rows = x$panel[,.N])
}))
qc[, `:=`(population_knots_rows = nrow(knots), annual_panel_rows = nrow(annual),
          annual_min_year = min(annual$year), annual_max_year = max(annual$year),
          max_birth_allocation_relative_error = max(relative_error(
            birth_check$allocated_births, birth_check$national_births)))]

setorder(forward, analysis_lgd_id, year)
setorder(weights, year, population_g_unit, -population_contribution)
setorder(match_audit, year, match_status, shape_row)
setorder(annual, analysis_lgd_id, year)
fwrite(forward, forward_file, na = "")
fwrite(weights, weights_file, na = "")
fwrite(match_audit, match_audit_file, na = "")
fwrite(knots, knots_file, na = "")
fwrite(annual, annual_file, na = "")
fwrite(national_births, births_file, na = "")
fwrite(qc, qc_file, na = "")

message("Fixed-1911 LGD population/birth-denominator stage complete.")
message("  Knots: ", nrow(knots), "; annual rows: ", nrow(annual))
message("  Annual population: ", annual_file)
message("  Target geometry: ", geometry_file)
message("  QC: ", qc_file)
