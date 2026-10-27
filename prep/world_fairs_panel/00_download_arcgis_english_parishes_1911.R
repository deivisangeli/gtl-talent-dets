###############################################################################
# Download and validate the ArcGIS English Parishes (1911) feature layer.
#
# The public service limits responses to 2,000 records, so features are fetched
# in ordered GeoJSON batches. The untouched batches and source metadata are
# retained beside a single GeoPackage used by the world-fairs pipeline.
#
# Run from any working directory:
#   Rscript prep/world_fairs_panel/00_download_arcgis_english_parishes_1911.R
#
# Re-download every batch and rebuild the GeoPackage:
#   Rscript prep/world_fairs_panel/00_download_arcgis_english_parishes_1911.R --force
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(sf)
})

sf_use_s2(FALSE)

###############################################################################
# Paths and source endpoints
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
  if (!dir.exists(user_data_dir)) {
    stop("Could not find TALENT_DETS_DATA_DIR or the current user's Dropbox data root.")
  }
  TALENT_DETS_DATA_DIR <- normalizePath(
    user_data_dir, winslash = "/", mustWork = TRUE
  )
  DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
}

output_dir <- file.path(
  DATA_INPUT, "worlds_fairs", "city_census", "GBR", "raw",
  "arcgis_english_admin_boundaries_1911"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

web_map_id <- "1de6d2bb5e794fa885c2e320c191ad17"
feature_item_id <- "65b7575623c94d109f7389177a97735c"
service_url <- paste0(
  "https://services8.arcgis.com/WN5coiM8DFAUrNQ2/arcgis/rest/services/",
  "English_Parishes_(1911)/FeatureServer/0"
)
source_item_url <- paste0(
  "https://www.arcgis.com/home/item.html?id=", web_map_id
)

metadata_urls <- c(
  web_map_item = paste0(
    "https://www.arcgis.com/sharing/rest/content/items/", web_map_id,
    "?f=pjson"
  ),
  web_map_data = paste0(
    "https://www.arcgis.com/sharing/rest/content/items/", web_map_id,
    "/data?f=pjson"
  ),
  feature_item = paste0(
    "https://www.arcgis.com/sharing/rest/content/items/", feature_item_id,
    "?f=pjson"
  ),
  feature_layer = paste0(service_url, "?f=pjson")
)
metadata_files <- file.path(
  output_dir,
  c(
    "english_parishes_1911_web_map_item_metadata.json",
    "english_parishes_1911_web_map_data.json",
    "english_parishes_1911_feature_item_metadata.json",
    "english_parishes_1911_layer_metadata.json"
  )
)
names(metadata_files) <- names(metadata_urls)

gpkg_file <- file.path(output_dir, "english_parishes_1911.gpkg")
download_manifest_file <- file.path(
  output_dir, "english_parishes_1911_download_metadata.csv"
)
qc_file <- file.path(output_dir, "english_parishes_1911_validation.csv")

force_download <- "--force" %in% commandArgs(trailingOnly = TRUE)
batch_size <- 2000L

###############################################################################
# Download helpers
###############################################################################

download_atomic <- function(url, destination, overwrite = FALSE) {
  if (file.exists(destination) && !overwrite) return(invisible("existing"))

  temporary_file <- tempfile(fileext = paste0(".", tools::file_ext(destination)))
  on.exit(unlink(temporary_file, force = TRUE), add = TRUE)

  last_error <- NULL
  for (attempt in seq_len(3L)) {
    result <- tryCatch({
      utils::download.file(
        url, temporary_file, mode = "wb", quiet = TRUE,
        method = "libcurl"
      )
      TRUE
    }, error = function(e) {
      last_error <<- e
      FALSE
    })
    if (isTRUE(result) && file.exists(temporary_file) &&
        file.info(temporary_file)$size > 0) break
    if (attempt < 3L) Sys.sleep(2^(attempt - 1L))
  }

  if (!file.exists(temporary_file) || file.info(temporary_file)$size <= 0) {
    stop("Download failed for ", url, ": ", conditionMessage(last_error))
  }
  if (!file.copy(temporary_file, destination, overwrite = TRUE)) {
    stop("Could not move the downloaded file to ", destination)
  }
  invisible("downloaded")
}

for (i in seq_along(metadata_urls)) {
  download_atomic(
    metadata_urls[[i]], metadata_files[[i]], overwrite = force_download
  )
  # Fail early if ArcGIS returned an HTML or error response.
  fromJSON(metadata_files[[i]], simplifyVector = FALSE)
}

layer_metadata <- fromJSON(metadata_files[["feature_layer"]])
if (!identical(layer_metadata$geometryType, "esriGeometryPolygon")) {
  stop("The ArcGIS layer is no longer a polygon feature layer.")
}

count_url <- paste0(
  service_url,
  "/query?where=1%3D1&returnCountOnly=true&f=json"
)
count_response <- fromJSON(count_url)
if (!is.null(count_response$error) || is.null(count_response$count)) {
  stop("Could not obtain the live ArcGIS feature count.")
}
expected_features <- as.integer(count_response$count)
if (expected_features <= 0L) stop("The live ArcGIS layer is empty.")

###############################################################################
# Download paginated GeoJSON and build the GeoPackage when needed
###############################################################################

n_batches <- as.integer(ceiling(expected_features / batch_size))
manifest <- vector("list", n_batches)
chunks <- vector("list", n_batches)

for (batch_index in seq_len(n_batches)) {
  offset <- as.integer((batch_index - 1L) * batch_size)
  expected_batch_features <- min(batch_size, expected_features - offset)
  chunk_file <- file.path(
    output_dir,
    sprintf("english_parishes_1911_chunk_%03d.geojson", batch_index - 1L)
  )
  query_url <- paste0(
    service_url,
    "/query?where=1%3D1&outFields=%2A&returnGeometry=true",
    "&f=geojson&orderByFields=FID",
    "&resultOffset=", offset,
    "&resultRecordCount=", batch_size,
    "&outSR=4326"
  )

  status <- download_atomic(query_url, chunk_file, overwrite = force_download)
  chunk <- st_read(chunk_file, quiet = TRUE, stringsAsFactors = FALSE)
  if (nrow(chunk) != expected_batch_features) {
    stop(
      "Batch ", batch_index - 1L, " has ", nrow(chunk),
      " features; expected ", expected_batch_features, "."
    )
  }

  chunks[[batch_index]] <- chunk
  manifest[[batch_index]] <- data.table(
    source_item_url = source_item_url,
    service_url = service_url,
    chunk_index = batch_index - 1L,
    result_offset = offset,
    requested_record_count = batch_size,
    expected_features = expected_batch_features,
    downloaded_features = nrow(chunk),
    file_name = basename(chunk_file),
    file_size_bytes = file.info(chunk_file)$size,
    file_status = status,
    file_modified_at = format(
      file.info(chunk_file)$mtime, "%Y-%m-%d %H:%M:%S %z"
    )
  )
}

parishes_raw <- do.call(rbind, chunks)
if (nrow(parishes_raw) != expected_features) {
  stop("Combined GeoJSON batches do not equal the live feature count.")
}

required_fields <- c(
  "FID", "G_UNIT", "G_YEAR", "G_NAME", "G_STATUS", "NATION", "UNITTYPE"
)
missing_fields <- setdiff(required_fields, names(parishes_raw))
if (length(missing_fields) > 0L) {
  stop("Downloaded layer is missing fields: ", paste(missing_fields, collapse = ", "))
}
if (anyDuplicated(parishes_raw$FID)) stop("Downloaded FID values are not unique.")

raw_validity <- st_is_valid(parishes_raw, NA_on_exception = TRUE)
invalid_before_repair <- sum(is.na(raw_validity) | !raw_validity)
parishes_clean <- if (invalid_before_repair > 0L) {
  st_make_valid(parishes_raw)
} else {
  parishes_raw
}
parishes_clean <- st_transform(parishes_clean, 27700)

# GeoPackage uses FID as an internal identifier. Rename the source field so it
# remains available as an ordinary, auditable attribute.
names(parishes_clean)[names(parishes_clean) == "FID"] <- "source_fid"

rebuild_gpkg <- force_download || !file.exists(gpkg_file)
if (!rebuild_gpkg) {
  existing <- tryCatch(st_read(gpkg_file, quiet = TRUE), error = function(e) NULL)
  rebuild_gpkg <- is.null(existing) || nrow(existing) != expected_features ||
    !"G_UNIT" %in% names(existing)
}

if (rebuild_gpkg) {
  temporary_gpkg <- tempfile(fileext = ".gpkg")
  on.exit(unlink(temporary_gpkg, force = TRUE), add = TRUE)
  st_write(
    parishes_clean, temporary_gpkg,
    layer = "english_parishes_1911", quiet = TRUE,
    delete_dsn = TRUE
  )
  if (!file.copy(temporary_gpkg, gpkg_file, overwrite = TRUE)) {
    stop("Could not install the rebuilt GeoPackage at ", gpkg_file)
  }
}

fwrite(rbindlist(manifest), download_manifest_file)

###############################################################################
# Dataset and Liverpool coverage validation
###############################################################################

parishes_stored <- st_read(gpkg_file, quiet = TRUE)
gpkg_crs_input <- st_crs(parishes_stored)$input
gpkg_epsg <- st_crs(parishes_stored)$epsg
parishes <- st_transform(st_make_valid(parishes_stored), 27700)
parish_validity <- st_is_valid(parishes, NA_on_exception = TRUE)

boundary_gpkg <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "processed", "worlds_fairs",
  "ukds_1911_boundary_crosswalk", "ukds_1911_registration_lgd_overlap.gpkg"
)

liverpool_intersecting_parishes <- NA_integer_
liverpool_cb_area_coverage_share <- NA_real_
if (file.exists(boundary_gpkg)) {
  lgds <- st_read(
    boundary_gpkg, layer = "local_government_districts_1911", quiet = TRUE
  )
  liverpool <- lgds[toupper(trimws(lgds$lgd_name)) == "LIVERPOOL", ]
  if (nrow(liverpool) != 1L) {
    stop("Expected exactly one Liverpool LGD polygon in the 1911 boundary file.")
  }
  liverpool <- st_transform(st_make_valid(liverpool), 27700)
  hit <- lengths(st_intersects(parishes, liverpool)) > 0L
  liverpool_intersecting_parishes <- sum(hit)
  parish_cover <- st_union(parishes[hit, ])
  covered <- suppressWarnings(st_intersection(parish_cover, st_union(liverpool)))
  liverpool_cb_area_coverage_share <- as.numeric(
    st_area(covered) / st_area(st_union(liverpool))
  )
}

g_unit <- trimws(as.character(parishes$G_UNIT))
g_unit_present <- !is.na(g_unit) & g_unit != "" & g_unit != "0"
source_fid_unique <- if ("source_fid" %in% names(parishes)) {
  !anyDuplicated(parishes$source_fid)
} else {
  # Older valid copies may have used the ArcGIS FID as the GeoPackage row id.
  !anyDuplicated(parishes_raw$FID)
}

qc <- data.table(
  dataset = "English Parishes (1911)",
  source_item_url = source_item_url,
  service_url = service_url,
  feature_count_live = expected_features,
  feature_count_downloaded = nrow(parishes_raw),
  feature_count_gpkg = nrow(parishes),
  chunk_count = n_batches,
  unique_source_fid = source_fid_unique,
  unique_g_unit = !anyDuplicated(g_unit),
  missing_or_zero_g_unit = sum(!g_unit_present),
  duplicated_nonzero_g_unit_rows = sum(duplicated(g_unit[g_unit_present])),
  invalid_geometries_in_download = invalid_before_repair,
  invalid_geometries_in_gpkg = sum(is.na(parish_validity) | !parish_validity),
  gpkg_crs = gpkg_crs_input,
  gpkg_epsg = gpkg_epsg,
  liverpool_intersecting_parishes = liverpool_intersecting_parishes,
  liverpool_cb_area_coverage_share = liverpool_cb_area_coverage_share,
  source_attribution = paste(
    "Great Britain Historical GIS Project (2011).",
    "Great Britain Historical GIS. University of Portsmouth."
  ),
  validated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
)
fwrite(qc, qc_file)

if (qc$feature_count_gpkg != expected_features ||
    !qc$unique_source_fid || qc$invalid_geometries_in_gpkg != 0L) {
  stop("The downloaded parish layer failed one or more validation checks.")
}
if (!is.na(qc$liverpool_cb_area_coverage_share) &&
    qc$liverpool_cb_area_coverage_share < 0.999) {
  stop("The parish layer covers less than 99.9% of the 1911 Liverpool CB.")
}

message("English Parishes (1911) download is complete and valid.")
message("Features: ", format(expected_features, big.mark = ","))
message("GeoPackage: ", gpkg_file)
message("Stored CRS: ", gpkg_crs_input)
if (!is.na(liverpool_intersecting_parishes)) {
  message(
    "Liverpool-intersecting parishes: ", liverpool_intersecting_parishes,
    "; CB area coverage: ",
    sprintf("%.4f%%", 100 * liverpool_cb_area_coverage_share)
  )
}
