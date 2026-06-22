###############################################################################
# Build spatial Law-Robson-to-Nomis matches for 1921, 1931, 1951, and 1961.
#
# Sources:
# - ONS Open Geography Portal boundaries for 1921 and 1961
# - UK Data Service study 9321 historical district boundaries for all years
# - Law-Robson Settlement Points in British National Grid (EPSG:27700)
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/08_build_uk_nomis_spatial_matches_1921_1961.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(httr2)
  library(readxl)
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
nomis_raw_dir <- file.path(gbr_dir, "raw", "nomis_historical_census")
boundary_raw_dir <- file.path(gbr_dir, "raw", "historical_boundaries")
ukds_dir <- file.path(boundary_raw_dir, "ukds_9321")
ons_dir <- file.path(boundary_raw_dir, "ons")
dir.create(ukds_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ons_dir, recursive = TRUE, showWarnings = FALSE)

base_panel_file <- file.path(
  gbr_dir, "city_population_law_robson_bennett_1801_1911_geocoded.csv"
)
text_panel_file <- file.path(gbr_dir, "city_population_nomis_1921_1961.csv")
london_file <- file.path(gbr_dir, "raw", "population_1801_to_2021.xlsx")

spatial_panel_file <- file.path(
  gbr_dir, "city_population_nomis_1921_1961_spatial.csv"
)
spatial_crosswalk_file <- file.path(
  gbr_dir, "law_robson_nomis_spatial_crosswalk_1921_1961.csv"
)
spatial_audit_file <- file.path(
  gbr_dir, "law_robson_nomis_spatial_match_audit_1921_1961.csv"
)
spatial_summary_file <- file.path(
  gbr_dir, "nomis_spatial_match_coverage_summary_1921_1961.csv"
)
boundary_gpkg_file <- file.path(
  gbr_dir, "raw", "historical_boundaries", "uk_historical_districts_1921_1961.gpkg"
)
manifest_file <- file.path(boundary_raw_dir, "download_manifest.csv")

required_files <- c(base_panel_file, text_panel_file, london_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

###############################################################################
# Downloads
###############################################################################

force_download <- tolower(Sys.getenv("UK_BOUNDARY_FORCE_DOWNLOAD", "false")) %chin%
  c("1", "true", "yes")

download_file <- function(url, destination, force = FALSE) {
  if (!force && file.exists(destination) && file.info(destination)$size > 0L) {
    return("existing")
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(destination)) unlink(destination)
  request(url) |>
    req_retry(max_tries = 5L) |>
    req_timeout(300) |>
    req_perform(path = destination)
  if (!file.exists(destination) || file.info(destination)$size <= 0L) {
    if (file.exists(destination)) unlink(destination)
    stop("Downloaded file is empty: ", url)
  }
  "downloaded"
}

ukds_graphql_endpoint <- paste0(
  "https://ohlhy6cg7nhwtpuer664aeok2i.appsync-api.eu-west-2.amazonaws.com/",
  "graphql"
)
ukds_api_key <- "da2-dbqlla2y3jf2vaqev4lcrpiq4a"
ukds_object_key <- "Study_9321/9321shp_7c35b0e8ad52bb65d67e5ece1e696857.zip"

get_ukds_download_url <- function() {
  query <- paste0(
    "query GetFileUrl($BucketName: String!, $Key: String!) { ",
    "getFileUrl(BucketName: $BucketName, Key: $Key) }"
  )
  response <- request(ukds_graphql_endpoint) |>
    req_headers(`x-api-key` = ukds_api_key) |>
    req_body_json(list(
      query = query,
      variables = list(BucketName = "", Key = ukds_object_key)
    )) |>
    req_retry(max_tries = 5L) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  url <- response$data$getFileUrl
  if (is.null(url) || !nzchar(url)) stop("UK Data Service did not return a download URL.")
  url
}

arcgis_query_url <- function(service_url) {
  paste0(
    service_url,
    "/query?where=1%3D1&outFields=*&returnGeometry=true&outSR=27700&f=geojson"
  )
}

ons_1921_lad_service <- paste0(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
  "LAD_JUN_1921_EW_BGC/FeatureServer/0"
)
ons_1921_cb_service <- paste0(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
  "CB_JUN_1921_EW_BGC_V2/FeatureServer/0"
)
ons_1961_lad_service <- paste0(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
  "LAD_Dec_1961_in_England_and_Wales_BFC_Boundaries_2022/FeatureServer/0"
)

ukds_zip <- file.path(ukds_dir, "ukds_study_9321_boundaries.zip")
ons_1921_lad_file <- file.path(ons_dir, "ons_lad_1921.geojson")
ons_1921_cb_file <- file.path(ons_dir, "ons_county_boroughs_1921.geojson")
ons_1961_lad_file <- file.path(ons_dir, "ons_lad_1961.geojson")

cat("Downloading historical boundary files...\n")
download_records <- list()

ukds_status <- if (!force_download && file.exists(ukds_zip) &&
                   file.info(ukds_zip)$size > 0L) {
  "existing"
} else {
  download_file(get_ukds_download_url(), ukds_zip, force = TRUE)
}
download_records[[1L]] <- data.table(
  source = "UK Data Service study 9321",
  source_url = "https://datacatalogue.ukdataservice.ac.uk/studies/study/9321?id=9321",
  local_path = ukds_zip,
  download_status = ukds_status
)

ons_downloads <- list(
  list("ONS 1921 LAD", ons_1921_lad_service, ons_1921_lad_file),
  list("ONS 1921 County Borough", ons_1921_cb_service, ons_1921_cb_file),
  list("ONS 1961 LAD", ons_1961_lad_service, ons_1961_lad_file)
)
for (item in ons_downloads) {
  status <- download_file(arcgis_query_url(item[[2L]]), item[[3L]], force_download)
  download_records[[length(download_records) + 1L]] <- data.table(
    source = item[[1L]],
    source_url = item[[2L]],
    local_path = item[[3L]],
    download_status = status
  )
}

manifest <- rbindlist(download_records)
manifest[, `:=`(
  local_path = normalizePath(local_path, winslash = "/", mustWork = TRUE),
  bytes = file.info(local_path)$size,
  sha256 = vapply(local_path, digest, character(1L), algo = "sha256", file = TRUE),
  accessed_on = as.character(Sys.Date())
)]
fwrite(manifest, manifest_file)

ukds_extract_dir <- file.path(ukds_dir, "extracted")
if (force_download && dir.exists(ukds_extract_dir)) {
  unlink(ukds_extract_dir, recursive = TRUE)
}
if (!dir.exists(file.path(ukds_extract_dir, "UKDA-9321-SHP", "shp"))) {
  dir.create(ukds_extract_dir, recursive = TRUE, showWarnings = FALSE)
  unzip(ukds_zip, exdir = ukds_extract_dir)
}

###############################################################################
# Helpers
###############################################################################

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("[^A-Z0-9]+", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}

canonical_town <- function(x) {
  x <- normalize_text(x)
  x <- sub(" COUNTY CORPORATE$", "", x)
  x <- sub("^THE ", "", x)
  x <- sub("^CITY OF ", "", x)
  x <- sub(" CITY AND COUNTY OF$", "", x)
  x <- sub(" CITY AND COUNT OF$", "", x)
  x <- sub(" CITY OF$", "", x)
  x <- sub(" BOROUGH OF$", "", x)
  aliases <- c(
    "CAERNARVON" = "CARNARVON",
    "NEWCASTLE ON TYNE" = "NEWCASTLE UPON TYNE",
    "NANTYGLO AND BLAINA" = "NANT Y GLO AND BLAINA",
    "STOKE ON TRENT" = "STOKE UPON TRENT"
  )
  hit <- match(x, names(aliases))
  replace <- !is.na(hit)
  x[replace] <- unname(aliases[hit[replace]])
  x
}

canonical_county <- function(x) {
  x <- normalize_text(x)
  x <- sub("^COUNTY OF ", "", x)
  x <- gsub("[()]", "", x)
  x <- gsub(" PARTS OF ", " ", x, fixed = TRUE)
  x <- gsub("YORKSHIRE WEST RIDING", "YORKSHIRE WEST RIDING", x, fixed = TRUE)
  aliases <- c(
    "CAERNARVONSHIRE" = "CARNARVONSHIRE",
    "DEVONSHIRE" = "DEVON",
    "DORSETSHIRE" = "DORSET",
    "GLAMORGANSHIRE" = "GLAMORGAN",
    "GLOUCESTER" = "GLOUCESTERSHIRE",
    "RUTLANDSHIRE" = "RUTLAND",
    "SOMERSETSHIRE" = "SOMERSET",
    "SOUTHAMPTON" = "HAMPSHIRE",
    "WESTMORELAND" = "WESTMORLAND"
  )
  hit <- match(x, names(aliases))
  replace <- !is.na(hit)
  x[replace] <- unname(aliases[hit[replace]])
  x
}

urban_types <- c(
  "Urban District", "Municipal Borough", "County Borough",
  "Metropolitan Borough", "County Corporate", "London County Corporate"
)
district_types <- c(urban_types, "Rural District")

status_to_type <- c(
  "UD" = "Urban District",
  "MB" = "Municipal Borough",
  "CB" = "County Borough",
  "METB" = "Metropolitan Borough",
  "CC" = "County Corporate",
  "LCC" = "London County Corporate",
  "RD" = "Rural District"
)

population_columns <- c(
  "1921" = "2c3_0003", "1931" = "3c3_0003",
  "1951" = "5c3_0003", "1961" = "6c3_0003"
)

county_parent_priority <- c(
  "Administrative County (excluding County Boroughs)" = 1L,
  "Administrative County" = 2L,
  "Administrative County with any County Boroughs" = 3L,
  "Administrative County with associated County Boroughs" = 4L,
  "Ancient County" = 5L,
  "City and County of York" = 6L
)

derive_1951_cr03_id <- function(metadata_id, area_type) {
  prefix <- ifelse(area_type == "County Borough", "H06", "H07")
  paste0(prefix, substr(metadata_id, 1L, 1L), substr(metadata_id, 3L, nchar(metadata_id)))
}

###############################################################################
# Nomis district populations and parent counties
###############################################################################

read_nomis_districts <- function(year) {
  year_dir <- file.path(nomis_raw_dir, as.character(year))
  metadata_file <- file.path(year_dir, sprintf("%s_metadata.xlsx", year))
  values_candidates <- list.files(
    file.path(year_dir, "extracted"),
    pattern = sprintf("%s_cr03_values[.]csv$", year),
    recursive = TRUE,
    full.names = TRUE
  )
  values_candidates <- values_candidates[
    !grepl("__MACOSX", values_candidates, fixed = TRUE)
  ]
  if (length(values_candidates) != 1L || !file.exists(metadata_file)) {
    stop("Missing Nomis CR03 values or metadata for ", year)
  }

  values <- fread(values_candidates[[1L]], na.strings = c("", "NA", ".."))
  pop_column <- population_columns[[as.character(year)]]
  values <- values[area_type %chin% district_types, .(
    source_area_id = as.character(area_id),
    value_area_name = as.character(area),
    source_area_type = as.character(area_type),
    population = suppressWarnings(as.numeric(get(pop_column)))
  )]

  areas <- as.data.table(read_excel(
    metadata_file,
    sheet = sprintf("%s_areas", year),
    col_types = "text"
  ))
  relationships <- as.data.table(read_excel(
    metadata_file,
    sheet = sprintf("%s_area_relationships", year),
    col_types = "text"
  ))
  areas <- areas[area_type %chin% district_types]
  areas[, metadata_area_id := as.character(area_id)]
  areas[, source_area_id := if (year == 1951L) {
    derive_1951_cr03_id(metadata_area_id, area_type)
  } else {
    metadata_area_id
  }]

  parents <- relationships[
    area_type_1 %chin% names(county_parent_priority) &
      area_type_2 %chin% district_types,
    .(
      metadata_area_id = as.character(area_id_2),
      source_county = as.character(area_1),
      source_county_type = as.character(area_type_1),
      parent_priority = unname(county_parent_priority[area_type_1])
    )
  ]
  setorder(parents, metadata_area_id, parent_priority, source_county)
  parents <- parents[, .SD[1L], by = metadata_area_id]

  out <- merge(
    areas[, .(
      metadata_area_id,
      source_area_id,
      metadata_area_name = as.character(area),
      metadata_area_type = as.character(area_type)
    )],
    values,
    by = "source_area_id",
    all.x = TRUE,
    sort = FALSE
  )
  out <- merge(out, parents, by = "metadata_area_id", all.x = TRUE, sort = FALSE)
  missing_urban_population <- out[
    is.na(population) & metadata_area_type %chin% urban_types,
    .N
  ]
  if (missing_urban_population > 0L) {
    stop(
      "Failed to attach Nomis population to ", missing_urban_population,
      " urban districts for ", year
    )
  }
  out[, `:=`(
    census_year = year,
    source_area_name = metadata_area_name,
    source_name_normalized = canonical_town(metadata_area_name),
    source_county_normalized = canonical_county(source_county)
  )]
  out[, .(
    census_year,
    source_area_id,
    metadata_area_id,
    source_area_name,
    source_area_type,
    source_county,
    source_county_type,
    source_name_normalized,
    source_county_normalized,
    population
  )]
}

census_years <- c(1921L, 1931L, 1951L, 1961L)
nomis_districts <- rbindlist(lapply(census_years, read_nomis_districts))
if (nomis_districts[, anyDuplicated(paste(census_year, source_area_id))] > 0L) {
  stop("Duplicate Nomis source area IDs by census year.")
}

###############################################################################
# Boundary layers
###############################################################################

cat("Reading and standardizing boundary layers...\n")

ons_1921_lad <- st_read(ons_1921_lad_file, quiet = TRUE)
ons_1921_lad <- ons_1921_lad[, c(
  "LAD1921CD", "LAD1921NM", "LADT1921NM", attr(ons_1921_lad, "sf_column")
)]
names(ons_1921_lad)[1:3] <- c("boundary_id", "boundary_name", "boundary_type")

ons_1921_cb <- st_read(ons_1921_cb_file, quiet = TRUE)
ons_1921_cb <- ons_1921_cb[, c(
  "CTYCB1921C", "CTYCB1921N", attr(ons_1921_cb, "sf_column")
)]
names(ons_1921_cb)[1:2] <- c("boundary_id", "boundary_name")
ons_1921_cb$boundary_type <- "County Borough"
ons_1921_cb <- ons_1921_cb[, c(
  "boundary_id", "boundary_name", "boundary_type", attr(ons_1921_cb, "sf_column")
)]

boundaries_1921 <- rbind(ons_1921_lad, ons_1921_cb)
boundaries_1921$census_year <- 1921L
boundaries_1921$boundary_source <- "ONS Open Geography Portal"

ons_1961 <- st_read(ons_1961_lad_file, quiet = TRUE)
ons_1961 <- ons_1961[, c("LA61CD", "LA61NM", attr(ons_1961, "sf_column"))]
names(ons_1961)[1:2] <- c("boundary_id", "boundary_name")
ons_1961$census_year <- 1961L
ons_1961$boundary_type <- NA_character_
ons_1961$boundary_source <- "ONS Open Geography Portal"

ukds_shp_root <- file.path(ukds_extract_dir, "UKDA-9321-SHP", "shp")
read_ukds_boundary <- function(year) {
  path <- file.path(
    ukds_shp_root,
    sprintf("ukds_ew%s_lgdistricts2", year),
    sprintf("ew%s_lgdistricts.shp", year)
  )
  if (!file.exists(path)) stop("Missing UKDS boundary shapefile: ", path)
  x <- st_read(path, quiet = TRUE)
  x <- x[, c("G_UNIT", "G_NAME", "G_STATUS", attr(x, "sf_column"))]
  names(x)[1:3] <- c("gbhgis_unit_id", "boundary_name", "boundary_status")
  x$boundary_id <- paste0("GBHGIS_", x$gbhgis_unit_id)
  x$boundary_type <- unname(status_to_type[toupper(x$boundary_status)])
  x$census_year <- year
  x$boundary_source <- "UK Data Service study 9321 / GBHGIS"
  x[, c(
    "boundary_id", "gbhgis_unit_id", "boundary_name", "boundary_type",
    "census_year", "boundary_source", attr(x, "sf_column")
  )]
}

boundaries_1931 <- read_ukds_boundary(1931L)
boundaries_1951 <- read_ukds_boundary(1951L)

boundary_list <- list(
  `1921` = boundaries_1921,
  `1931` = boundaries_1931,
  `1951` = boundaries_1951,
  `1961` = ons_1961
)

for (year in names(boundary_list)) {
  x <- boundary_list[[year]]
  if (is.na(st_crs(x))) st_crs(x) <- 27700
  x <- st_transform(x, 27700)
  x <- st_make_valid(x)
  boundary_list[[year]] <- x
}

if (nrow(boundary_list[["1921"]]) != 1817L) {
  stop("Expected 1,817 combined ONS polygons for 1921.")
}
if (nrow(boundary_list[["1931"]]) != 1800L) {
  stop("Expected 1,800 UKDS polygons for 1931.")
}
if (nrow(boundary_list[["1951"]]) != 1472L) {
  stop("Expected 1,472 UKDS polygons for 1951.")
}
if (nrow(boundary_list[["1961"]]) != 1467L) {
  stop("Expected 1,467 ONS polygons for 1961.")
}

# Direct ONS IDs identify the Nomis area and therefore its administrative type.
for (year in c(1921L, 1961L)) {
  key <- as.character(year)
  x <- boundary_list[[key]]
  types <- nomis_districts[census_year == year, .(
    boundary_id = source_area_id,
    nomis_boundary_type = source_area_type
  )]
  x <- merge(x, types, by = "boundary_id", all.x = TRUE, sort = FALSE)
  replace_type <- is.na(x$boundary_type) | !nzchar(x$boundary_type)
  x$boundary_type[replace_type] <- x$nomis_boundary_type[replace_type]
  x$nomis_boundary_type <- NULL
  boundary_list[[key]] <- x
}

###############################################################################
# Law-Robson city points
###############################################################################

base_panel <- fread(base_panel_file, na.strings = c("", "NA"))
text_panel <- fread(text_panel_file, na.strings = c("", "NA"))

city_columns <- c(
  "city_id", "town_name", "standard_name", "historic_county",
  "easting", "northing", "longitude", "latitude",
  "geocode_status", "geocode_method"
)
cities <- unique(base_panel[, ..city_columns], by = "city_id")
if (nrow(cities) != 934L) stop("Expected 934 Law-Robson cities.")
cities[, `:=`(
  town_normalized = canonical_town(town_name),
  standard_name_normalized = canonical_town(standard_name),
  county_normalized = canonical_county(historic_county),
  is_composite = grepl("&", town_name, fixed = TRUE),
  is_london = standard_name == "LONDON"
)]

located_cities <- cities[
  geocode_status == "matched" & !is.na(easting) & !is.na(northing)
]
city_points <- st_as_sf(
  located_cities,
  coords = c("easting", "northing"),
  crs = 27700,
  remove = FALSE
)

###############################################################################
# Point-in-polygon and Nomis linking
###############################################################################

spatial_rows <- list()
for (year in census_years) {
  cat("Spatial matching for ", year, "...\n", sep = "")
  boundaries <- boundary_list[[as.character(year)]]
  joined <- st_join(
    city_points,
    boundaries,
    join = st_covered_by,
    left = TRUE
  )
  joined <- as.data.table(st_drop_geometry(joined))
  joined[, spatial_polygon_count := uniqueN(boundary_id[!is.na(boundary_id)]),
         by = city_id]
  joined <- joined[
    spatial_polygon_count != 1L | !is.na(boundary_id)
  ]
  joined <- unique(joined, by = c("city_id", "boundary_id"))
  joined[, census_year := year]

  missing_points <- cities[!city_id %in% joined$city_id]
  if (nrow(missing_points) > 0L) {
    joined <- rbindlist(list(
      joined,
      missing_points[, .(
        city_id, town_name, standard_name, historic_county,
        easting, northing, longitude, latitude,
        geocode_status, geocode_method,
        town_normalized, standard_name_normalized, county_normalized,
        is_composite, is_london,
        boundary_id = NA_character_,
        boundary_name = NA_character_,
        boundary_type = NA_character_,
        boundary_source = NA_character_,
        spatial_polygon_count = 0L,
        census_year = year
      )]
    ), use.names = TRUE, fill = TRUE)
  }

  if (year %in% c(1921L, 1961L)) {
    candidates <- merge(
      joined,
      nomis_districts[census_year == year],
      by.x = c("census_year", "boundary_id"),
      by.y = c("census_year", "source_area_id"),
      all.x = TRUE,
      sort = FALSE
    )
    setnames(candidates, "boundary_id", "source_area_id")
    candidates[, nomis_candidate_count := fifelse(
      !is.na(source_area_name), 1L, 0L
    )]
    candidates[, nomis_link_method := "official_boundary_code"]
  } else {
    joined[, `:=`(
      boundary_name_normalized = canonical_town(boundary_name),
      boundary_county_normalized = county_normalized
    )]
    source_year <- nomis_districts[census_year == year]
    candidates_all <- merge(
      joined[spatial_polygon_count == 1L & !is.na(boundary_id)],
      source_year,
      by.x = c("census_year", "boundary_name_normalized", "boundary_type"),
      by.y = c("census_year", "source_name_normalized", "source_area_type"),
      all.x = TRUE,
      allow.cartesian = TRUE,
      sort = FALSE
    )
    # The Nomis type is a join key and is therefore omitted from the merged
    # columns; retain the spatially verified type for downstream validation.
    candidates_all[, source_area_type := boundary_type]
    candidates_all[, county_agrees :=
      nzchar(county_normalized) & nzchar(source_county_normalized) &
      county_normalized == source_county_normalized]
    candidates_all[, nomis_candidate_count := uniqueN(
      source_area_id[!is.na(source_area_id)]
    ), by = .(city_id, boundary_id)]
    candidates_all[, county_candidate_count := uniqueN(
      source_area_id[county_agrees & !is.na(source_area_id)]
    ), by = .(city_id, boundary_id)]
    candidates_all[, selected :=
      nomis_candidate_count == 1L |
      (nomis_candidate_count > 1L & county_candidate_count == 1L & county_agrees)]
    selected <- candidates_all[selected == TRUE]
    selected[, nomis_link_method := fifelse(
      nomis_candidate_count == 1L,
      "spatial_polygon_unique_name_and_type",
      "spatial_polygon_name_type_and_county"
    )]
    selected <- unique(selected, by = c("city_id", "boundary_id", "source_area_id"))

    unresolved <- joined[
      !selected,
      on = .(city_id, boundary_id)
    ]
    unresolved <- merge(
      unresolved,
      candidates_all[, .(
        nomis_candidate_count = max(nomis_candidate_count, na.rm = TRUE),
        county_candidate_count = max(county_candidate_count, na.rm = TRUE)
      ), by = .(city_id, boundary_id)],
      by = c("city_id", "boundary_id"),
      all.x = TRUE,
      sort = FALSE
    )
    unresolved[!is.finite(nomis_candidate_count), nomis_candidate_count := 0L]
    unresolved[!is.finite(county_candidate_count), county_candidate_count := 0L]
    unresolved[, nomis_link_method := NA_character_]
    candidates <- rbindlist(list(selected, unresolved), use.names = TRUE, fill = TRUE)
  }

  spatial_rows[[as.character(year)]] <- candidates
}

spatial <- rbindlist(spatial_rows, use.names = TRUE, fill = TRUE)

###############################################################################
# Reviewed composite aggregations
###############################################################################

composite_components <- data.table(
  city_id = rep(c(64L, 957L, 1275L, 1279L, 1764L, 2244L, 2303L),
                times = c(2L, 2L, 2L, 2L, 2L, 2L, 2L)),
  component_name = c(
    "WINDSOR", "ETON",
    "BOURNEMOUTH", "POOLE",
    "LIVERPOOL", "BIRKENHEAD",
    "MANCHESTER", "SALFORD",
    "NEWCASTLE UPON TYNE", "GATESHEAD",
    "BRIGHTON", "HOVE",
    "BIRMINGHAM", "SMETHWICK"
  )
)
composite_components[, component_normalized := canonical_town(component_name)]

component_candidates <- merge(
  CJ(city_id = unique(composite_components$city_id), census_year = census_years),
  composite_components,
  by = "city_id",
  allow.cartesian = TRUE
)
component_candidates <- merge(
  component_candidates,
  nomis_districts[source_area_type %chin% urban_types],
  by.x = c("census_year", "component_normalized"),
  by.y = c("census_year", "source_name_normalized"),
  allow.cartesian = TRUE
)
component_candidates[, component_candidate_count := uniqueN(source_area_id),
                     by = .(city_id, census_year, component_name)]
component_selected <- component_candidates[component_candidate_count == 1L]
component_summary <- component_selected[, .(
  selected_component_count = uniqueN(component_name),
  component_source_area_ids = paste(sort(unique(source_area_id)), collapse = " | "),
  component_source_area_names = paste(sort(unique(source_area_name)), collapse = " | "),
  component_source_area_types = paste(sort(unique(source_area_type)), collapse = " | "),
  component_population = sum(population),
  component_counties = paste(sort(unique(source_county)), collapse = " | ")
), by = .(city_id, census_year)]
required_component_counts <- composite_components[, .(
  required_component_count = uniqueN(component_name)
), by = city_id]
component_summary <- merge(component_summary, required_component_counts, by = "city_id")
component_summary <- component_summary[
  selected_component_count == required_component_count
]

spatial <- merge(
  spatial,
  component_summary,
  by = c("city_id", "census_year"),
  all.x = TRUE,
  sort = FALSE
)

###############################################################################
# Match classification and textual comparison
###############################################################################

spatial[, composite_polygon_agrees :=
  is_composite & !is.na(boundary_name) &
  canonical_town(boundary_name) == town_normalized]

spatial[, preliminary_status := fcase(
  geocode_status != "matched", "missing_settlement_point",
  spatial_polygon_count == 0L, "outside_historical_boundaries",
  spatial_polygon_count > 1L, "ambiguous_spatial_polygons",
  is_london, "incompatible_london_geography",
  is_composite & !is.na(component_population), "matched_composite_aggregation",
  is_composite & composite_polygon_agrees & source_area_type %chin% urban_types,
    "matched_single_district_composite",
  is_composite, "composite_requires_aggregation",
  is.na(source_area_id), "nomis_link_unresolved",
  source_area_type == "Rural District", "inside_rural_district",
  source_area_type %chin% urban_types, "matched_spatial",
  default = "unsupported_area_type"
)]

spatial[, `:=`(
  final_source_area_ids = fifelse(
    preliminary_status == "matched_composite_aggregation",
    component_source_area_ids,
    source_area_id
  ),
  final_source_area_names = fifelse(
    preliminary_status == "matched_composite_aggregation",
    component_source_area_names,
    source_area_name
  ),
  final_source_area_types = fifelse(
    preliminary_status == "matched_composite_aggregation",
    component_source_area_types,
    source_area_type
  ),
  final_source_counties = fifelse(
    preliminary_status == "matched_composite_aggregation",
    component_counties,
    source_county
  ),
  spatial_population = fifelse(
    preliminary_status == "matched_composite_aggregation",
    component_population,
    population
  ),
  final_match_method = fifelse(
    preliminary_status == "matched_composite_aggregation",
    "reviewed_composite_aggregation",
    nomis_link_method
  )
)]

accepted_statuses <- c(
  "matched_spatial", "matched_single_district_composite",
  "matched_composite_aggregation"
)
spatial[, preliminary_accepted := preliminary_status %chin% accepted_statuses]

# Reusing one administrative population for multiple Law-Robson cities would
# duplicate population in city-level analyses, so these matches remain audited.
reuse <- spatial[preliminary_accepted == TRUE, .(
  source_reuse_count = uniqueN(city_id)
), by = .(census_year, final_source_area_ids)]
spatial <- merge(
  spatial,
  reuse,
  by = c("census_year", "final_source_area_ids"),
  all.x = TRUE,
  sort = FALSE
)
spatial[is.na(source_reuse_count), source_reuse_count := 0L]
spatial[, spatial_match_status := fifelse(
  preliminary_accepted & source_reuse_count > 1L,
  "ambiguous_reused_source_area",
  preliminary_status
)]
spatial[, spatial_match_accepted :=
  preliminary_accepted & source_reuse_count <= 1L]

text_compare <- text_panel[, .(
  city_id,
  census_year,
  text_population = population,
  text_source_area_id = source_area_id,
  text_source_area_name = source_area_name,
  text_source_area_type = source_area_type,
  text_match_method = match_method,
  text_match_status = match_status
)]
spatial <- merge(
  spatial,
  text_compare,
  by = c("city_id", "census_year"),
  all.x = TRUE,
  sort = FALSE
)
spatial[, spatial_text_agreement := fcase(
  spatial_match_accepted & final_match_method == "reviewed_composite_aggregation",
    NA_character_,
  spatial_match_accepted & !is.na(text_source_area_id) &
    final_source_area_ids == text_source_area_id,
    "same_source_area",
  spatial_match_accepted & !is.na(text_source_area_id),
    "different_source_area",
  spatial_match_accepted & is.na(text_source_area_id),
    "new_spatial_match",
  !spatial_match_accepted & !is.na(text_source_area_id),
    "text_only_match",
  default = "unmatched_both"
)]

# Keep exactly one result row per city and year. Multiple polygon cases are
# summarized rather than silently choosing one polygon.
spatial[, row_priority := fcase(
  spatial_match_accepted, 1L,
  spatial_match_status == "ambiguous_spatial_polygons", 2L,
  default = 3L
)]
setorder(spatial, city_id, census_year, row_priority, boundary_id)
spatial <- spatial[, .SD[1L], by = .(city_id, census_year)]

###############################################################################
# Outputs and validation
###############################################################################

audit <- spatial[, .(
  city_id,
  town_name,
  standard_name,
  historic_county,
  census_year,
  longitude,
  latitude,
  easting,
  northing,
  geocode_status,
  is_composite,
  boundary_id,
  boundary_name,
  boundary_type,
  boundary_source,
  spatial_polygon_count,
  nomis_candidate_count,
  final_source_area_ids,
  final_source_area_names,
  final_source_area_types,
  final_source_counties,
  spatial_population,
  final_match_method,
  source_reuse_count,
  spatial_match_status,
  spatial_match_accepted,
  text_population,
  text_source_area_id,
  text_source_area_name,
  text_source_area_type,
  text_match_method,
  text_match_status,
  spatial_text_agreement
)]
setorder(audit, census_year, city_id)

crosswalk <- audit[spatial_match_accepted == TRUE]
spatial_panel <- audit[, .(
  country_iso3 = "GBR",
  country_name = "United Kingdom",
  city_id,
  town_name,
  standard_name,
  historic_county,
  census_year,
  population = fifelse(spatial_match_accepted, spatial_population, NA_real_),
  population_available = spatial_match_accepted,
  longitude,
  latitude,
  easting,
  northing,
  source_area_id = final_source_area_ids,
  source_area_name = final_source_area_names,
  source_area_type = final_source_area_types,
  source_county = final_source_counties,
  population_source = "Nomis historical census CR03",
  population_match_method = final_match_method,
  population_match_status = spatial_match_status,
  boundary_id,
  boundary_name,
  boundary_type,
  boundary_source,
  spatial_text_agreement,
  coordinate_crs = "EPSG:4326"
)]
setorder(spatial_panel, city_id, census_year)

summary_table <- audit[, .(
  total_cities = uniqueN(city_id),
  geocoded_cities = sum(geocode_status == "matched"),
  cities_inside_one_polygon = sum(spatial_polygon_count == 1L),
  accepted_spatial_matches = sum(spatial_match_accepted),
  spatial_coverage_pct = 100 * mean(spatial_match_accepted),
  new_spatial_matches = sum(spatial_text_agreement == "new_spatial_match", na.rm = TRUE),
  same_as_text_match = sum(spatial_text_agreement == "same_source_area", na.rm = TRUE),
  different_from_text_match = sum(
    spatial_text_agreement == "different_source_area", na.rm = TRUE
  ),
  text_only_matches = sum(spatial_text_agreement == "text_only_match", na.rm = TRUE)
), by = census_year]
setorder(summary_table, census_year)

if (nrow(audit) != 934L * length(census_years)) {
  stop("Spatial audit does not contain 934 cities for every census year.")
}
if (audit[, anyDuplicated(paste(city_id, census_year))] > 0L) {
  stop("Duplicate city/year rows in spatial audit.")
}
if (crosswalk[spatial_match_accepted == TRUE, .N, by = .(
  census_year, final_source_area_ids
)][, any(N > 1L)]) {
  stop("Accepted source areas are reused across Law-Robson cities.")
}
if (spatial_panel[population_available == TRUE &
                  (is.na(population) | population < 0), .N] > 0L) {
  stop("Accepted spatial matches have missing or negative populations.")
}

cat("Writing spatial match outputs...\n")
fwrite(spatial_panel, spatial_panel_file)
fwrite(crosswalk, spatial_crosswalk_file)
fwrite(audit, spatial_audit_file)
fwrite(summary_table, spatial_summary_file)

if (file.exists(boundary_gpkg_file)) unlink(boundary_gpkg_file)
for (year in names(boundary_list)) {
  st_write(
    boundary_list[[year]],
    boundary_gpkg_file,
    layer = paste0("districts_", year),
    quiet = TRUE,
    append = file.exists(boundary_gpkg_file)
  )
}
st_write(
  city_points,
  boundary_gpkg_file,
  layer = "law_robson_settlement_points",
  quiet = TRUE,
  append = TRUE
)

cat("\nCompleted spatial Law-Robson-to-Nomis matching.\n")
print(summary_table)
cat("\nMatch statuses:\n")
print(audit[, .N, by = .(census_year, spatial_match_status)][order(census_year, -N)])
cat("Spatial panel:", spatial_panel_file, "\n")
cat("Crosswalk:", spatial_crosswalk_file, "\n")
cat("Audit:", spatial_audit_file, "\n")
cat("GeoPackage:", boundary_gpkg_file, "\n")
