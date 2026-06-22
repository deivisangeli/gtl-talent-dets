suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})

sf::sf_use_s2(FALSE)

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE), error = function(e) NA_character_)
if (is.na(script_path)) {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  script_path <- if (length(file_arg)) normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE) else NA_character_
}

repo_root <- Sys.getenv("GTL_REPO", unset = NA_character_)
if (is.na(repo_root) || repo_root == "") {
  repo_root <- if (!is.na(script_path)) normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE) else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "paths.R"))

message("Building Greater London 1911 parish crosswalk")

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
raw_dir <- file.path(gbr_dir, "raw")
arcgis_dir <- file.path(raw_dir, "arcgis_english_admin_boundaries_1911")
historical_dir <- file.path(raw_dir, "historical_boundaries")

bbce_file <- file.path(raw_dir, "london_1911_parishes_from_BBCE.csv")
parish_gpkg <- file.path(arcgis_dir, "english_parishes_1911.gpkg")
district_gpkg <- file.path(historical_dir, "uk_historical_districts_1921_1961.gpkg")
gisco_lau_gpkg <- file.path(TALENT_DETS_DATA_DIR, "raw", "gisco", "lau", "LAU_RG_01M_2019_4326.gpkg")

required_files <- c(bbce_file, parish_gpkg, district_gpkg, gisco_lau_gpkg)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing required input files:\n", paste(missing_files, collapse = "\n"))
}

normalize_name <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("BOR'GH", "BOROUGH", x, fixed = TRUE)
  x <- gsub("M'TYR", "MARTYR", x, fixed = TRUE)
  x <- gsub("S'THWARK", "SOUTHWARK", x, fixed = TRUE)
  x <- gsub("W'NDSW'TH B", "WANDSWORTH BOROUGH", x, fixed = TRUE)
  x <- gsub("CONVENT", "COVENT", x, fixed = TRUE)
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

safe_sum <- function(x) sum(as.numeric(x), na.rm = TRUE)

bbce_raw <- fread(bbce_file)
required_cols <- c("ParID", "REGCNTY", "AdminCnty", "REGDIST", "SUBDIST", "PARISH", "AREA", "POP")
if (!all(required_cols %in% names(bbce_raw))) {
  stop("BBCE file does not have expected columns: ", paste(setdiff(required_cols, names(bbce_raw)), collapse = ", "))
}

bbce_raw[, `:=`(
  POP = as.numeric(POP),
  AREA = as.numeric(AREA),
  bbce_parish_norm = normalize_name(PARISH)
)]

bbce_parishes <- bbce_raw[
  ,
  .(
    bbce_pop_1911 = safe_sum(POP),
    bbce_area_raw = safe_sum(AREA),
    bbce_rows = .N,
    regcnty = paste(sort(unique(REGCNTY)), collapse = "; "),
    admin_county = paste(sort(unique(AdminCnty)), collapse = "; "),
    regdist = paste(sort(unique(REGDIST)), collapse = "; "),
    subdist = paste(sort(unique(SUBDIST)), collapse = "; ")
  ),
  by = .(bbce_parish = PARISH, bbce_parish_norm)
]

parishes <- st_read(parish_gpkg, quiet = TRUE)
parishes <- st_transform(st_make_valid(parishes), 27700)

needed_parish_cols <- c("G_UNIT", "G_NAME")
if (!all(needed_parish_cols %in% names(parishes))) {
  stop("Parish GeoPackage does not have expected columns: ", paste(setdiff(needed_parish_cols, names(parishes)), collapse = ", "))
}

parishes$arcgis_g_unit <- as.character(parishes$G_UNIT)
parishes$arcgis_g_name <- as.character(parishes$G_NAME)
parishes$arcgis_name_norm <- normalize_name(parishes$arcgis_g_name)
parishes$arcgis_area_m2 <- as.numeric(st_area(parishes))

lau <- st_read(gisco_lau_gpkg, quiet = TRUE)
lau <- st_transform(st_make_valid(lau), 27700)
lau_names <- names(lau)
lau_id_col <- intersect(c("LAU_ID", "lau_id", "GISCO_ID"), lau_names)[1]
lau_name_col <- intersect(c("LAU_NAME", "lau_name", "NAME_LATN", "NAME"), lau_names)[1]
if (is.na(lau_id_col) || is.na(lau_name_col)) {
  stop("Could not identify LAU id/name columns in GISCO LAU file.")
}

lau_dt <- as.data.table(st_drop_geometry(lau))
greater_london_ids <- lau_dt[grepl("^E090000", get(lau_id_col)), get(lau_id_col)]
city_london_ids <- lau_dt[get(lau_id_col) == "E09000001", get(lau_id_col)]

if (!length(greater_london_ids) || !length(city_london_ids)) {
  stop("Could not find Greater London / City of London LAUs in GISCO LAU file.")
}

greater_london_ref <- st_union(lau[lau[[lau_id_col]] %in% greater_london_ids, ])
city_london_ref <- st_union(lau[lau[[lau_id_col]] %in% city_london_ids, ])

parish_dt <- as.data.table(st_drop_geometry(parishes))

alias_map <- data.table(
  bbce_parish_norm = normalize_name(c(
    "CHARTERHOUSE", "CHISWICK", "CHRISTCHURCH SOUTHWARK", "CROYDON",
    "EALING", "EDMONTON", "ELTHAM",
    "FURNIVAL'S INN", "GRAY'S INN", "HORNSEY", "LAMBETH", "LINCOLN'S INN",
    "LOW LEYTON", "MIDDLE TEMPLE", "PENGE (KENT)", "POPLAR BOR'GH", "RATCLIFF",
    "ST CLEMENT DANEST", "ST GEORGE THE M'TYR SOUTHWARK", "ST JAMES WESTMINSTER",
    "ST MARGARET AND ST JOHN", "ST NICHOLAS DEPTFORD", "ST PAUL CONVENT GARDEN",
    "ST PAUL DEPTFORD", "ST SAVIOUR S'THWARK", "W'NDSW'TH B", "WIMBLEDON"
  )),
  target_arcgis_norm = normalize_name(c(
    "CLERKENWELL", "CHISWICK ST NICHOLAS", "SOUTHWARK CHRISTCHURCH",
    "CROYDON ST JOHN THE BAPTIST",
    "EALING ST MARY", "EDMONTON ALL SAINTS", "ELTHAM ST JOHN THE BAPTIST", "FURNIVALS INN",
    "GRAYS INN", "HORNSEY ST MARY", "LAMBETH ST MARY", "LINCOLNS INN",
    "LEYTON", "INNER TEMPLE", "PENGE", "POPLAR BOROUGH", "RATCLIFFE",
    "ST CLEMENT DANES", "SOUTHWARK ST GEORGE THE MARTYR", "WESTMINSTER ST JAMES",
    "WESTMINSTER ST MARGARET", "DEPTFORD ST NICHOLAS", "ST PAUL COVENT GARDEN",
    "DEPTFORD ST PAUL", "SOUTHWARK ST SAVIOUR", "WANDSWORTH BOROUGH",
    "WIMBLEDON ST MARY"
  )),
  match_method = "manual_alias"
)

find_best_candidate <- function(target_norm, method) {
  cand_units <- parish_dt[arcgis_name_norm == target_norm, unique(arcgis_g_unit)]
  if (!length(cand_units)) {
    return(data.table(
      arcgis_g_unit = NA_character_,
      arcgis_g_name = NA_character_,
      candidate_count = 0L,
      match_method = method,
      match_note = paste0("No ArcGIS parish candidate for ", target_norm)
    ))
  }

  cand_sf <- parishes[parishes$arcgis_g_unit %in% cand_units, ]
  cand_points <- suppressWarnings(st_point_on_surface(cand_sf))
  cand_dist <- as.numeric(st_distance(cand_points, greater_london_ref))
  keep <- which.min(cand_dist)

  data.table(
    arcgis_g_unit = cand_sf$arcgis_g_unit[keep],
    arcgis_g_name = cand_sf$arcgis_g_name[keep],
    candidate_count = length(cand_units),
    match_method = if (length(cand_units) > 1L) paste0(method, "_nearest_greater_london") else method,
    match_note = if (length(cand_units) > 1L) paste0("Selected nearest Greater London candidate among ", length(cand_units), " ArcGIS parishes") else ""
  )
}

match_rows <- vector("list", nrow(bbce_parishes))
for (i in seq_len(nrow(bbce_parishes))) {
  src <- bbce_parishes[i]

  if (src$bbce_parish_norm == normalize_name("CITY OF LONDON")) {
    city_intersections <- suppressWarnings(st_intersection(
      parishes[, c("arcgis_g_unit", "arcgis_g_name", "arcgis_area_m2")],
      st_sf(city_proxy = "CITY_OF_LONDON_2019_LAU", geometry = city_london_ref)
    ))
    if (nrow(city_intersections) == 0L) {
      stop("City of London spatial fallback did not intersect any 1911 parishes.")
    }
    city_intersections$city_overlap_m2 <- as.numeric(st_area(city_intersections))
    city_units <- as.data.table(st_drop_geometry(city_intersections))[city_overlap_m2 > 1]
    city_units <- city_units[order(-city_overlap_m2)]
    city_units[, source_match_weight := city_overlap_m2 / sum(city_overlap_m2)]
    match_rows[[i]] <- cbind(
      src[, .(bbce_parish, bbce_parish_norm, bbce_pop_1911, bbce_area_raw, bbce_rows, regcnty, admin_county, regdist, subdist)],
      city_units[, .(
        arcgis_g_unit,
        arcgis_g_name,
        candidate_count = .N,
        match_method = "city_of_london_spatial_fallback",
        match_note = "Allocated City of London BBCE aggregate across 1911 parishes intersecting City of London LAU",
        source_match_weight,
        used_in_union = TRUE
      )]
    )
    next
  }

  if (src$bbce_parish_norm == normalize_name("LAND COMMON TO INNER AND MIDDLE TEMPLES")) {
    match_rows[[i]] <- data.table(
      bbce_parish = src$bbce_parish,
      bbce_parish_norm = src$bbce_parish_norm,
      bbce_pop_1911 = src$bbce_pop_1911,
      bbce_area_raw = src$bbce_area_raw,
      bbce_rows = src$bbce_rows,
      regcnty = src$regcnty,
      admin_county = src$admin_county,
      regdist = src$regdist,
      subdist = src$subdist,
      arcgis_g_unit = NA_character_,
      arcgis_g_name = NA_character_,
      candidate_count = 0L,
      match_method = "zero_population_no_geometry",
      match_note = "BBCE row has zero population and no separable ArcGIS geometry; City of London fallback covers the Temple area in the union",
      source_match_weight = 0,
      used_in_union = FALSE
    )
    next
  }

  alias_hit <- alias_map[bbce_parish_norm == src$bbce_parish_norm]
  target_norm <- if (nrow(alias_hit)) alias_hit$target_arcgis_norm[1] else src$bbce_parish_norm
  method <- if (nrow(alias_hit)) alias_hit$match_method[1] else "exact_normalized_name"

  best <- find_best_candidate(target_norm, method)
  best[, `:=`(source_match_weight = ifelse(is.na(arcgis_g_unit), 0, 1), used_in_union = !is.na(arcgis_g_unit))]
  match_rows[[i]] <- cbind(
    src[, .(bbce_parish, bbce_parish_norm, bbce_pop_1911, bbce_area_raw, bbce_rows, regcnty, admin_county, regdist, subdist)],
    best
  )
}

match_audit <- rbindlist(match_rows, fill = TRUE)
match_audit[, allocated_pop_1911 := bbce_pop_1911 * source_match_weight]
match_audit[, allocated_area_raw := bbce_area_raw * source_match_weight]

unresolved <- match_audit[is.na(arcgis_g_unit) & bbce_pop_1911 > 0]
if (nrow(unresolved)) {
  fwrite(unresolved, file.path(arcgis_dir, "greater_london_1911_unresolved_parishes.csv"))
  stop("Unresolved BBCE parishes with positive population. See greater_london_1911_unresolved_parishes.csv")
}

matched_units <- unique(na.omit(match_audit$arcgis_g_unit))
matched_parishes <- parishes[parishes$arcgis_g_unit %in% matched_units, ]

alloc_dt <- match_audit[!is.na(arcgis_g_unit), .(
  bbce_sources = paste(sort(unique(bbce_parish)), collapse = "; "),
  bbce_pop_1911_allocated = sum(allocated_pop_1911, na.rm = TRUE),
  bbce_area_raw_allocated = sum(allocated_area_raw, na.rm = TRUE),
  match_methods = paste(sort(unique(match_method)), collapse = "; "),
  n_bbce_sources = uniqueN(bbce_parish)
), by = arcgis_g_unit]

matched_parishes <- merge(matched_parishes, alloc_dt, by = "arcgis_g_unit", all.x = TRUE, sort = FALSE)
matched_parishes$greater_london_source <- "BBCE parish list matched to ArcGIS English parishes 1911"
matched_parishes$bbce_pop_1911_allocated[is.na(matched_parishes$bbce_pop_1911_allocated)] <- 0

greater_london_1911 <- st_sf(
  unit_id = "GREATER_LONDON_1911_PARISHES",
  unit_name = "Greater London 1911 reconstructed from BBCE parish list",
  source = "BBCE parish list + ArcGIS English parishes 1911",
  bbce_pop_1911 = sum(bbce_parishes$bbce_pop_1911, na.rm = TRUE),
  bbce_area_raw = sum(bbce_parishes$bbce_area_raw, na.rm = TRUE),
  matched_arcgis_parishes = length(matched_units),
  matched_bbce_parishes = uniqueN(bbce_parishes$bbce_parish),
  geometry = st_union(st_geometry(matched_parishes))
)
greater_london_1911$area_sqkm <- as.numeric(st_area(greater_london_1911)) / 1e6

districts <- st_read(district_gpkg, layer = "districts_1921", quiet = TRUE)
districts <- st_transform(st_make_valid(districts), 27700)

district_cols <- names(districts)
district_id_col <- intersect(c("boundary_id", "unit_id", "district_id", "area_code", "code", "GSS_CODE"), district_cols)[1]
district_name_col <- intersect(c("boundary_name", "unit_name", "district_name", "area_name", "name", "NAME"), district_cols)[1]
district_type_col <- intersect(c("boundary_type", "area_type", "unit_type", "district_type", "type"), district_cols)[1]

if (is.na(district_id_col) || is.na(district_name_col)) {
  stop("Could not identify district id/name columns in districts_1921 layer.")
}
if (is.na(district_type_col)) {
  districts$area_type <- NA_character_
  district_type_col <- "area_type"
}

districts$nomis_1921_id <- as.character(districts[[district_id_col]])
districts$nomis_1921_name <- as.character(districts[[district_name_col]])
districts$nomis_1921_type <- as.character(districts[[district_type_col]])
districts$nomis_1921_area_m2 <- as.numeric(st_area(districts))

gl_district_intersections <- suppressWarnings(st_intersection(
  districts[, c("nomis_1921_id", "nomis_1921_name", "nomis_1921_type", "nomis_1921_area_m2")],
  greater_london_1911[, c("unit_id")]
))

gl_district_intersections$gl_overlap_area_m2 <- as.numeric(st_area(gl_district_intersections))
gl_intersection_dt <- as.data.table(st_drop_geometry(gl_district_intersections))[
  ,
  .(
    nomis_1921_name = first(nomis_1921_name),
    nomis_1921_type = first(nomis_1921_type),
    nomis_1921_area_m2 = first(nomis_1921_area_m2),
    gl_overlap_area_m2 = sum(gl_overlap_area_m2, na.rm = TRUE)
  ),
  by = nomis_1921_id
]
gl_intersection_dt[, gl_overlap_share_boundary := gl_overlap_area_m2 / nomis_1921_area_m2]

parish_pop_for_intersection <- matched_parishes[, c("arcgis_g_unit", "arcgis_g_name", "bbce_pop_1911_allocated", "arcgis_area_m2")]
parish_district_intersections <- suppressWarnings(st_intersection(
  parish_pop_for_intersection,
  districts[, c("nomis_1921_id", "nomis_1921_name", "nomis_1921_type")]
))

parish_district_intersections$intersection_area_m2 <- as.numeric(st_area(parish_district_intersections))
parish_district_dt <- as.data.table(st_drop_geometry(parish_district_intersections))
parish_district_dt[, parish_area_in_district_share := fifelse(arcgis_area_m2 > 0, intersection_area_m2 / arcgis_area_m2, 0)]
parish_district_dt[, bbce_pop_1911_to_nomis_1921 := bbce_pop_1911_allocated * parish_area_in_district_share]
pop_alloc_dt <- parish_district_dt[
  ,
  .(
    bbce_pop_1911_allocated_to_boundary = sum(bbce_pop_1911_to_nomis_1921, na.rm = TRUE),
    parish_intersection_area_m2 = sum(intersection_area_m2, na.rm = TRUE),
    n_arcgis_parish_geometries = uniqueN(arcgis_g_unit),
    n_arcgis_parish_names = uniqueN(arcgis_g_name)
  ),
  by = .(nomis_1921_id, nomis_1921_name, nomis_1921_type)
]

parish_coverage_dt <- parish_district_dt[
  ,
  .(
    nomis_covered_area_m2 = sum(intersection_area_m2, na.rm = TRUE),
    bbce_pop_1911_allocated_to_nomis = sum(bbce_pop_1911_to_nomis_1921, na.rm = TRUE),
    n_nomis_1921_boundaries = uniqueN(nomis_1921_id),
    nomis_1921_boundaries = paste(sort(unique(nomis_1921_name)), collapse = "; ")
  ),
  by = .(arcgis_g_unit, arcgis_g_name)
]
parish_coverage_audit <- merge(
  as.data.table(st_drop_geometry(matched_parishes))[
    ,
    .(
      arcgis_g_unit,
      arcgis_g_name,
      bbce_sources,
      bbce_pop_1911_allocated,
      arcgis_area_m2
    )
  ],
  parish_coverage_dt,
  by = c("arcgis_g_unit", "arcgis_g_name"),
  all.x = TRUE
)
parish_coverage_audit[is.na(nomis_covered_area_m2), `:=`(
  nomis_covered_area_m2 = 0,
  bbce_pop_1911_allocated_to_nomis = 0,
  n_nomis_1921_boundaries = 0,
  nomis_1921_boundaries = ""
)]
parish_coverage_audit[, nomis_area_coverage_share := fifelse(arcgis_area_m2 > 0, nomis_covered_area_m2 / arcgis_area_m2, 0)]
parish_coverage_audit[, bbce_pop_1911_unallocated_to_nomis := bbce_pop_1911_allocated - bbce_pop_1911_allocated_to_nomis]
setorder(parish_coverage_audit, -bbce_pop_1911_unallocated_to_nomis)

crosswalk <- merge(
  gl_intersection_dt,
  pop_alloc_dt,
  by = c("nomis_1921_id", "nomis_1921_name", "nomis_1921_type"),
  all = TRUE
)
crosswalk[is.na(bbce_pop_1911_allocated_to_boundary), bbce_pop_1911_allocated_to_boundary := 0]
crosswalk[is.na(gl_overlap_area_m2), gl_overlap_area_m2 := 0]
crosswalk[is.na(gl_overlap_share_boundary), gl_overlap_share_boundary := 0]
crosswalk[, in_greater_london_1911_any := gl_overlap_area_m2 > 1000]
crosswalk[, in_greater_london_1911_main := gl_overlap_share_boundary >= 0.5]
setorder(crosswalk, -gl_overlap_share_boundary, nomis_1921_name)

summary_dt <- data.table(
  metric = c(
    "bbce_rows",
    "bbce_unique_parishes",
    "bbce_total_pop_1911",
    "bbce_total_area_raw",
    "matched_arcgis_parish_geometries",
    "greater_london_1911_area_sqkm",
    "nomis_1921_boundaries_intersecting_any",
    "nomis_1921_boundaries_intersecting_main",
    "allocated_pop_to_nomis_1921",
    "allocation_gap_pop",
    "unresolved_positive_pop_parishes"
  ),
  value = c(
    nrow(bbce_raw),
    uniqueN(bbce_parishes$bbce_parish),
    sum(bbce_parishes$bbce_pop_1911, na.rm = TRUE),
    sum(bbce_parishes$bbce_area_raw, na.rm = TRUE),
    length(matched_units),
    greater_london_1911$area_sqkm,
    sum(crosswalk$in_greater_london_1911_any, na.rm = TRUE),
    sum(crosswalk$in_greater_london_1911_main, na.rm = TRUE),
    sum(crosswalk$bbce_pop_1911_allocated_to_boundary, na.rm = TRUE),
    sum(bbce_parishes$bbce_pop_1911, na.rm = TRUE) - sum(crosswalk$bbce_pop_1911_allocated_to_boundary, na.rm = TRUE),
    nrow(unresolved)
  )
)

audit_file <- file.path(arcgis_dir, "greater_london_1911_parish_match_audit.csv")
coverage_file <- file.path(arcgis_dir, "greater_london_1911_parish_to_nomis_coverage_audit.csv")
crosswalk_file <- file.path(arcgis_dir, "greater_london_1911_to_nomis_1921_crosswalk.csv")
summary_file <- file.path(arcgis_dir, "greater_london_1911_to_nomis_1921_summary.csv")
gpkg_file <- file.path(arcgis_dir, "greater_london_1911_from_parishes.gpkg")

fwrite(match_audit, audit_file)
fwrite(parish_coverage_audit, coverage_file)
fwrite(crosswalk, crosswalk_file)
fwrite(summary_dt, summary_file)

if (file.exists(gpkg_file)) invisible(file.remove(gpkg_file))
st_write(greater_london_1911, gpkg_file, layer = "greater_london_1911", quiet = TRUE)
st_write(matched_parishes, gpkg_file, layer = "matched_parishes_1911", quiet = TRUE, append = TRUE)
st_write(gl_district_intersections, gpkg_file, layer = "nomis_1921_intersections", quiet = TRUE, append = TRUE)

message("Wrote: ", audit_file)
message("Wrote: ", coverage_file)
message("Wrote: ", crosswalk_file)
message("Wrote: ", summary_file)
message("Wrote: ", gpkg_file)
message("BBCE 1911 population: ", format(sum(bbce_parishes$bbce_pop_1911, na.rm = TRUE), big.mark = ",", scientific = FALSE))
message("Allocated to Nomis 1921 boundaries: ", format(sum(crosswalk$bbce_pop_1911_allocated_to_boundary, na.rm = TRUE), big.mark = ",", scientific = FALSE))
message("Nomis 1921 boundaries with any overlap: ", sum(crosswalk$in_greater_london_1911_any, na.rm = TRUE))
message("Nomis 1921 boundaries with >=50% overlap: ", sum(crosswalk$in_greater_london_1911_main, na.rm = TRUE))
