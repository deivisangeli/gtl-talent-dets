# Rebuild the reviewed Table A1 station inventory and its geographic enrichment.
# Curated inputs and all data products live in Dropbox, never in the repository.
# No web requests, automatic geocoding, or opening-date imputation are performed.
# Run with TALENT_DETS_DATA_DIR explicitly set to the local Dropbox project root.

# Preserve source punctuation even when a shell supplies an unsupported Unix
# locale to Windows R (for example, LC_ALL=C.UTF-8).
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  utf8_locale <- Sys.setlocale("LC_CTYPE", "English_United States.utf8")
  if (!nzchar(utf8_locale)) stop("A UTF-8 locale is required to preserve the source transcription.")
}
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script <- normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- Sys.getenv("GTL_REPO", unset = getwd())
  while (!file.exists(file.path(repo_root, "paths.R"))) {
    parent <- dirname(repo_root)
    if (identical(parent, repo_root)) stop("Cannot find paths.R; set GTL_REPO.")
    repo_root <- parent
  }
}
source(file.path(repo_root, "paths.R"), encoding = "UTF-8")
for (pkg in c("sf", "data.table")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Install required R package: ", pkg)
}

root <- TALENT_DETS_DATA_DIR
intermediate <- file.path(root, "Data", "intermediary", "scientific_facilities")
destination <- file.path(root, "Data", "processed", "scientific_facilities")
inputs <- c(
  appendix = file.path(root, "Data/raw/scientific_facilities/2014226appendix.pdf"),
  table_a1 = file.path(intermediate, "kantor_whalley_2019_table_a1.csv"),
  geography = file.path(intermediate, "kantor_whalley_2019_geography_lookup.csv"),
  geonames_snapshot = file.path(intermediate, "kantor_whalley_2019_geonames_snapshot.tsv"),
  county_reference = file.path(root, "output/national_county2020.txt"),
  county_geometry = file.path(root, "raw/tigris/cb_2020_us_county_20m.shp"),
  amws_panel = file.path(root, "output/us_panel_county_amws_combined_year.csv")
)
missing <- inputs[!file.exists(inputs)]
if (length(missing)) stop("Missing inputs:\n", paste(missing, collapse = "\n"))
read_text_csv <- function(path) {
  read.csv(path, colClasses = "character", na.strings = "", check.names = FALSE,
           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
}
check <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
require_fields <- function(x, fields, label) {
  check(all(fields %in% names(x)), paste("Missing columns in", label, ":", paste(setdiff(fields, names(x)), collapse = ", ")))
}
normalize_county <- function(x) {
  tolower(trimws(sub(" County$", "", x)))
}

published <- read_text_csv(inputs[["table_a1"]])
geo <- read_text_csv(inputs[["geography"]])
require_fields(published, c("station_id", "source_row", "state_reported", "city_reported",
  "county_reported", "university_reported", "pre_hatch_open_date_reported", "station_type_reported",
  "source_pdf", "source_table", "source_pdf_page", "source_printed_page", "source_doi",
  "source_note", "source_footnote"), "Table A1")
require_fields(geo, c("station_id", "site_name", "expected_GEOID", "latitude", "longitude",
  "coordinate_kind", "coordinate_precision", "coordinate_method", "geonames_id",
  "coordinate_source_title", "coordinate_source_url", "coordinate_source_locator",
  "location_source_title", "location_source_url", "geography_note", "geography_review_status",
  "source_checked_date"), "geography lookup")
check(nrow(published) == 23L && !anyDuplicated(published$station_id), "Expected 23 unique source records.")
check(nrow(geo) == 23L && !anyDuplicated(geo$station_id), "Expected 23 unique geography records.")
check(setequal(published$station_id, geo$station_id), "Source and geography station IDs differ.")
published$source_row <- as.integer(published$source_row)
check(identical(sort(published$source_row), 1:23), "Source rows must be 1 through 23.")
published <- published[order(published$source_row), , drop = FALSE]
check(sum(published$station_type_reported == "Main") == 20L &&
        sum(published$station_type_reported == "Minor") == 3L, "Expected 20 main and 3 minor stations.")
check(length(unique(published$state_reported)) == 20L &&
        all(table(published$state_reported[published$station_type_reported == "Main"]) == 1L),
      "Expected one main station in each of 20 states.")
check(all(published$pre_hatch_open_date_reported == "None" |
          grepl("^[0-9]{4}$", published$pre_hatch_open_date_reported)), "Unexpected published date text.")
out <- cbind(published, geo[match(published$station_id, geo$station_id), setdiff(names(geo), "station_id"), drop = FALSE])
rownames(out) <- NULL
out$pre_hatch_open_year <- as.integer(replace(out$pre_hatch_open_date_reported,
  out$pre_hatch_open_date_reported == "None", NA_character_))
out$reported_year_after_hatch <- !is.na(out$pre_hatch_open_year) & out$pre_hatch_open_year > 1887L
out$latitude <- as.numeric(out$latitude)
out$longitude <- as.numeric(out$longitude)
check(all(is.finite(out$latitude) & is.finite(out$longitude)), "Every reviewed station must have a coordinate pair.")
check(all(out$latitude > 24 & out$latitude < 50 & out$longitude > -125 & out$longitude < -66),
      "Coordinate outside the contiguous US; check sign and latitude/longitude order.")
check(all(out$geography_review_status == "reviewed"), "Unreviewed geographic lookup.")
check(all(grepl("^https://", out$coordinate_source_url) & grepl("^https://", out$location_source_url)),
      "Every point needs coordinate and location evidence URLs.")
check(all(grepl("^(campus|station)_reference_point$", out$coordinate_kind)), "Invalid coordinate precision class.")

# Independently check all 21 numeric GeoNames points against the saved source rows.
gn <- read.delim(inputs[["geonames_snapshot"]], colClasses = "character", check.names = FALSE,
                 quote = "", fileEncoding = "UTF-8")
from_gn <- which(out$coordinate_method == "curated_geonames_facility")
ix <- match(out$geonames_id[from_gn], gn$geonameid)
check(!anyNA(ix) && !anyDuplicated(gn$geonameid), "Missing or duplicated GeoNames snapshot IDs.")
check(all(out$latitude[from_gn] == as.numeric(gn$latitude[ix])) &&
        all(out$longitude[from_gn] == as.numeric(gn$longitude[ix])), "Coordinate differs from GeoNames snapshot.")
check(all(out$state_reported[from_gn] == gn$admin1_code[ix]), "GeoNames facility state mismatch.")

# County IDs are matched by state and name, then checked with 2020 polygons.
# In particular, never use current GeoNames CT admin2 planning-region codes.
ref <- read.delim(inputs[["county_reference"]], sep = "|", colClasses = "character",
                  check.names = FALSE, fileEncoding = "UTF-8")
ref$GEOID <- paste0(ref$STATEFP, ref$COUNTYFP)
check(all(grepl("^[0-9]{5}$", ref$GEOID)) && !anyDuplicated(ref$GEOID), "Invalid 2020 county reference.")
key <- paste(ref$STATE, normalize_county(ref$COUNTYNAME), sep = "|")
reported_match <- match(paste(out$state_reported, normalize_county(out$county_reported), sep = "|"), key)
check(!anyNA(reported_match), "A published county could not be matched to the 2020 county list.")
out$county_reported_GEOID <- ref$GEOID[reported_match]
expected_match <- match(out$expected_GEOID, ref$GEOID)
check(!anyNA(expected_match), "Unknown expected 2020 county ID.")
check(all(ref$STATE[expected_match] == out$state_reported), "Expected county state mismatch.")

counties <- sf::st_read(inputs[["county_geometry"]], quiet = TRUE)
counties <- sf::st_transform(counties, 4326)
pts <- sf::st_as_sf(out, coords = c("longitude", "latitude"), crs = 4326)
hits <- sf::st_intersects(pts, counties)
check(all(lengths(hits) == 1L), "Station point must intersect exactly one county; review boundaries manually.")
spatial_geoid <- as.character(counties$GEOID[unlist(hits)])
check(all(spatial_geoid == out$expected_GEOID),
      paste("Spatial county differs from reviewed county:", paste(out$station_id[spatial_geoid != out$expected_GEOID], collapse = ", ")))
out$GEOID <- spatial_geoid
out$county_verified <- ref$COUNTYNAME[match(out$GEOID, ref$GEOID)]
out$county_geography_year <- 2020L
out$county_discrepancy <- out$county_reported_GEOID != out$GEOID
out$county_assignment_method <- "point_in_2020_census_county_polygon_and_reviewed_county_reference"
out$coordinate_crs <- "EPSG:4326"
out$historical_location_basis <- "Table A1 listed locality (1910 inventory); modern station/campus reference point"
check(identical(out$station_id[out$county_discrepancy], "kw2019_mo_mountain_grove"),
      "Unexpected set of published-versus-verified county discrepancies.")

panel_ids <- unique(data.table::fread(inputs[["amws_panel"]], select = "GEOID",
  colClasses = c(GEOID = "character"), showProgress = FALSE)$GEOID)
out$amws_panel_GEOID_present <- out$GEOID %in% panel_ids
check(all(out$amws_panel_GEOID_present), "A verified county ID is absent from the AMWS panel.")
check(identical(out$GEOID[1:2], c("09009", "09013")), "Connecticut must retain its pre-2022 county IDs.")
check(out$pre_hatch_open_date_reported[out$station_id == "kw2019_mo_mountain_grove"] == "1906", "Do not silently correct Mountain Grove's reported date.")
check(out$city_reported[out$station_id == "kw2019_oh_wooster"] == "Wooster" &&
        out$GEOID[out$station_id == "kw2019_oh_wooster"] == "39169", "Keep Ohio at the published Wooster site.")
check(all(is.na(out$pre_hatch_open_year[out$pre_hatch_open_date_reported == "None"])), "Published None must remain missing, not 1887.")
out$source_pdf_md5 <- unname(tools::md5sum(inputs[["appendix"]]))
out$expected_GEOID <- NULL

dictionary <- c(
  station_id = "Stable kw2019_<state>_<published_city> record ID; not an OpenAlex or institution ID.",
  source_row = "Row number 1-23 in Table A1; original publication order.",
  state_reported = "Published two-letter state, verbatim.",
  city_reported = "Published city, verbatim; Lafayette is not silently changed to West Lafayette.",
  county_reported = "Published county, verbatim; Mountain Grove remains Texas here.",
  university_reported = "Published university, verbatim, including Ohio footnote asterisk and Missouri dash.",
  pre_hatch_open_date_reported = "Published Pre-Hatch Station Open Date text, including literal None and 1906.",
  station_type_reported = "Published Main or Minor category; not inferred institutional importance.",
  source_pdf = "Project-root-relative immutable source PDF path.",
  source_table = "Published source table identifier, A1.",
  source_pdf_page = "One-based PDF page, 35.",
  source_printed_page = "Printed appendix page, 71.",
  source_doi = "Paper DOI, 10.1086/701035.",
  source_note = "Table A1 bibliographic source note (line wrapping normalized).",
  source_footnote = "Ohio relocation footnote transcribed from A1; blank otherwise. Preserved as a source claim, not verified movement history.",
  site_name = "Curated station/campus identity represented by the point.",
  location_source_title = "Title identifying institutional or historical evidence linking the station to this campus.",
  location_source_url = "Evidence URL for campus/station identity, distinct from numeric-coordinate provenance.",
  geography_note = "Manual review note, historical qualifications, campus choices, and discrepancies.",
  geography_review_status = "reviewed: locality and campus/station identity reviewed; does not mean survey-grade historical precision.",
  source_checked_date = "ISO date of geographic research/curation; fixed in lookup rather than replaced on rebuild.",
  latitude = "WGS84 latitude in decimal degrees; modern station/campus proxy for the published location.",
  longitude = "WGS84 longitude in decimal degrees, west negative.",
  coordinate_kind = "campus_reference_point or station_reference_point; never a city/county centroid.",
  coordinate_precision = "Qualitative spatial/historical precision. Decimal places are not an accuracy guarantee.",
  coordinate_method = "curated_geonames_facility, published_institutional_gps, or published_research_site_gps.",
  geonames_id = "GeoNames facility record ID for local dump points; blank for the two published GPS sources.",
  coordinate_source_title = "Source of numeric latitude and longitude.",
  coordinate_source_url = "Coordinate source record or publication URL.",
  coordinate_source_locator = "Exact local record ID or publication page/section for the coordinate.",
  pre_hatch_open_year = "Integer parsed from reported date; missing for None. Not a researched treatment/opening year.",
  reported_year_after_hatch = "TRUE if the table reports a numeric year after 1887, highlighting its heading/date inconsistency.",
  county_reported_GEOID = "Five-character 2020 county code matching the published state/county name.",
  GEOID = "Verified five-character 2020 county FIPS of the station point; use this for AMWS county joins.",
  county_verified = "Official 2020 county name associated with GEOID.",
  county_geography_year = "2020 county definitions, including the former CT counties rather than new planning regions.",
  county_discrepancy = "TRUE where the published county and verified station county differ.",
  county_assignment_method = "Independent county polygon and county-name/reference validation; no nearest-county snapping.",
  coordinate_crs = "EPSG:4326; latitude/longitude axes are stored as explicitly named columns.",
  historical_location_basis = "Published 1910 locality represented by a modern campus/station reference; not a time-varying relocation panel.",
  amws_panel_GEOID_present = "TRUE if verified GEOID occurs in the existing combined annual AMWS county panel; does not assert complete year coverage.",
  source_pdf_md5 = "MD5 checksum of the immutable appendix used on this build."
)
check(setequal(names(dictionary), names(out)), "Field dictionary does not cover the output schema exactly.")
dictionary <- data.frame(field = names(out), storage = vapply(out, function(x) class(x)[1], character(1)),
  description = unname(dictionary[names(out)]), stringsAsFactors = FALSE)

validation <- data.frame(
  check = c("station_records", "states", "main_stations", "minor_stations", "missing_coordinates",
    "county_polygon_matches", "amws_panel_county_matches", "published_county_discrepancies", "reported_dates_none",
    "reported_dates_after_1887", "geonames_points_checked", "published_gps_points_checked", "visual_source_rows_reviewed"),
  result = c(nrow(out), length(unique(out$state_reported)), sum(out$station_type_reported == "Main"),
    sum(out$station_type_reported == "Minor"), sum(is.na(out$latitude) | is.na(out$longitude)),
    sum(spatial_geoid == out$GEOID), sum(out$amws_panel_GEOID_present), sum(out$county_discrepancy),
    sum(out$pre_hatch_open_date_reported == "None"), sum(out$reported_year_after_hatch), length(from_gn),
    sum(out$coordinate_method != "curated_geonames_facility"), 23L),
  note = c("Required 23", "Required 20", "Required 20; one per state", "Required 3", "Required zero",
    "2020 Census cartographic boundary polygons; exactly one polygon per point", "GEOID membership only",
    "Mountain Grove: published Texas, verified Wright", "No dates imputed", "Mountain Grove 1906 retained",
    "Numeric points checked against saved GeoNames source rows", "Geneva Cornell GPS and Mountain Grove research-site GPS",
    "Manual visual review of rendered PDF page 35 / printed page 71 on 2026-10-20; not an automated PDF comparison")
)
sidecars <- paste0(sub("\\.shp$", "", inputs[["county_geometry"]]), c(".shx", ".dbf", ".prj", ".cpg"))
manifest_paths <- c(unname(inputs), sidecars[file.exists(sidecars)])
manifest <- data.frame(path = substring(gsub("\\\\", "/", manifest_paths), nchar(gsub("\\\\", "/", root)) + 2L),
  md5 = unname(tools::md5sum(manifest_paths)), stringsAsFactors = FALSE)

readme <- c(
  "# Kantor-Whalley (2019): Table A1 experiment stations", "",
  "23 records in publication order: 20 main stations (one in each state) and 3 minor stations.",
  "Source: appendix Table A1, PDF page 35 / printed page 71, DOI 10.1086/701035.",
  "Original PDF: Data/raw/scientific_facilities/2014226appendix.pdf. Its checksum is recorded in the data and input manifest.", "",
  "## Files and rebuilding", "",
  "- kantor_whalley_2019_stations.csv: complete joined station inventory.",
  "- kantor_whalley_2019_stations_dictionary.csv: every field's type and interpretation.",
  "- kantor_whalley_2019_stations_validation.csv: build checks and manual-review declaration.",
  "- kantor_whalley_2019_stations_input_manifest.csv: checksums of inputs, including county geometry sidecars.",
  "Curated transcription, geography lookup, and 21-row GeoNames snapshot are in Data/intermediary/scientific_facilities/.",
  "Run prep/scientific_facilities/build_kantor_whalley_2019_stations.R with TALENT_DETS_DATA_DIR set to the Dropbox project root.",
  "The builder sources paths.R, requires sf and data.table, works from any working directory, and performs no web requests.", "",
  "## Coordinates and county joins", "",
  "These are sourced modern campus/station reference points for the localities in the 1910 inventory, not surveyed historical building or plot coordinates.",
  "21 points come from manually selected facility records in input/geonames_US.txt, preserved in a small source snapshot. GeoNames is credited under CC BY 4.0: https://www.geonames.org/about.html .",
  "Geneva uses Cornell Cooperative Extension's Food Research Laboratory GPS, VegEdge 15(2), p. 10; Mountain Grove uses an orchard coordinate published in HortScience 59(6), 787-793 (2024), DOI 10.21273/HORTSCI17772-24.",
  "Institutional histories and facilities descriptions identify the campus; coordinate_source_* documents the separate numeric point evidence. Decimal precision is not historical accuracy.",
  "The site_name, coordinate_precision, and geography_note columns identify each approximation. No city or county centroid is used.",
  "Use GEOID for joins: it is the station point's verified 2020 county FIPS. Read it as a five-character string to retain leading zeros.",
  "County checks use raw/tigris/cb_2020_us_county_20m.shp and output/national_county2020.txt; generalized polygons are sufficient for these reviewed interior points, not parcel boundaries.",
  "All 23 verified county IDs occur in output/us_panel_county_amws_combined_year.csv. This checks membership, not historical boundary harmonization or complete annual coverage.",
  "Connecticut uses New Haven 09009 and Tolland 09013, not newer planning-region county-equivalents.", "",
  "## Source preservation and historical qualifications", "",
  "- Mountain Grove: county_reported remains Texas (29215); the station point and Missouri State facilities evidence locate it in Wright (GEOID 29229). The modern orchard is a station-scale proxy, not a historical headquarters coordinate.",
  "- Ohio: university_reported retains its asterisk, the 1882 table entry remains, and source_footnote reproduces the appendix's 1870/1892/1948 relocation narrative. Those dates are source claims, not independently established movement dates. This static dataset uses Wooster throughout.",
  "- New Hampshire: the point represents Durham as published; the institutional history records a move from Hanover in 1893. It is not necessarily the station's initial location.",
  "- New Haven: the actual Huntington Street station site is used, occupied since 1882, rather than central Yale. The table's Yale affiliation remains verbatim.",
  "- Purdue: published Lafayette remains, while the campus point is in West Lafayette within Tippecanoe County.",
  "- Nebraska and Minnesota: use the agricultural East Campus and St. Paul campus, respectively, rather than downtown Lincoln or Minneapolis.",
  "- Rutgers: College Farm/Cook campus is a proxy for the agricultural estate; contemporaneous laboratory work also occupied downtown New Jersey Hall. It is not a comprehensive map of station buildings.",
  "- Geneva and Ithaca are separate stations and remain separate records.",
  "The published date heading is Pre-Hatch Station Open Date, but Mountain Grove's entry is 1906. Preserve it and flag reported_year_after_hatch; do not treat it as a corrected founding date.",
  "Literal None is preserved in the source field and parsed as missing in pre_hatch_open_year. No missing date becomes 1887.",
  "This inventory is not a nationally complete station census, a staggered-opening treatment panel, or a replication of the authors' county-seat-based distance measures.",
  "Runner-up counties, crop trials, opening-date research, time-varying relocations, and analysis changes are outside this release.", "",
  "## Field dictionary", "",
  "See kantor_whalley_2019_stations_dictionary.csv for definitions of all columns. TRUE/FALSE fields are logical; blank derived values are missing."
)

dir.create(destination, recursive = TRUE, showWarnings = FALSE)
save_csv <- function(x, name) write.csv(x, file.path(destination, name), row.names = FALSE, na = "", fileEncoding = "UTF-8")
save_csv(out, "kantor_whalley_2019_stations.csv")
save_csv(dictionary, "kantor_whalley_2019_stations_dictionary.csv")
save_csv(validation, "kantor_whalley_2019_stations_validation.csv")
save_csv(manifest, "kantor_whalley_2019_stations_input_manifest.csv")
writeLines(readme, file.path(destination, "kantor_whalley_2019_stations_README.md"), useBytes = TRUE)
message("Built ", nrow(out), " stations; ", sum(out$county_discrepancy),
        " published county discrepancy; all coordinates and AMWS county IDs validated.\n", destination)
