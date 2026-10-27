# Rebuild the reviewed June 30, 1910 inventory, audit, geography, and allocation
# pool summaries. Research is curated in Dropbox inputs; this builder is offline.
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
}
args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
if (length(script_arg)) {
  script <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script), "../.."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- Sys.getenv("GTL_REPO", unset = getwd())
  while (!file.exists(file.path(repo_root, "paths.R"))) {
    parent <- dirname(repo_root)
    if (identical(parent, repo_root)) stop("Cannot locate paths.R; set GTL_REPO.")
    repo_root <- parent
  }
}
source(file.path(repo_root, "paths.R"), encoding = "UTF-8")
for (pkg in c("data.table", "sf")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Install required package: ", pkg)
}
root <- TALENT_DETS_DATA_DIR
src <- file.path(root, "Data/intermediary/scientific_facilities/usda_1910")
dest <- file.path(root, "Data/processed/scientific_facilities")
dir.create(dest, showWarnings = FALSE, recursive = TRUE)
prefix <- "usda_1910_university_agricultural_stations"
inputs <- c(
  builder = file.path(repo_root, "prep/scientific_facilities/build_usda_1910_stations.R"),
  shared_paths = file.path(repo_root, "paths.R"),
  candidates = file.path(src, "station_candidates_curated.csv"),
  geography = file.path(src, "station_geography_curated.csv"),
  coverage = file.path(src, "jurisdiction_coverage_curated.csv"),
  university_crosswalk = file.path(src, "university_allocation_crosswalk_curated.csv"),
  pool_aliases = file.path(src, "allocation_pool_name_aliases.csv"),
  selected_geonames = file.path(src, "geonames_selected_facilities.csv"),
  source_catalog = file.path(src, "source_catalog.csv"),
  raw_source_manifest = file.path(src, "raw_source_manifest.csv"),
  report_context = file.path(src, "candidate_national_report_context.csv"),
  report = file.path(root, "Data/raw/scientific_facilities/usda_1910/annualreportoes1910unit.pdf"),
  county_reference = file.path(root, "output/national_county2020.txt"),
  county_geometry = file.path(root, "raw/tigris/cb_2020_us_county_20m.shp"),
  amws_panel = file.path(root, "output/us_panel_county_amws_combined_year.csv"),
  allocation = file.path(root, "Data/raw/land_grants/College_Control_Towns.csv"),
  curated_pool = file.path(root, "output/land_grants/andrews_event_county_units_1850_1920.csv"),
  main_membership = file.path(root, "results/land_grants/event_studies/amws_county_pairs_temporal_support_1830_1950_baseline_pop_per_cohort_max_e/events_by_cohort.csv")
)
geometry_base <- sub("[.]shp$", "", inputs[["county_geometry"]])
sidecars <- paste0(geometry_base, c(".dbf", ".shx", ".prj"))
names(sidecars) <- paste0("county_geometry_", c("dbf", "shx", "prj"))
inputs <- c(inputs, sidecars)
if (any(!file.exists(inputs))) stop("Missing inputs:\n", paste(inputs[!file.exists(inputs)], collapse = "\n"))
read_csv <- function(path) read.csv(path, colClasses = "character", na.strings = "", check.names = FALSE,
  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
save_csv <- function(x, suffix = "") {
  p <- file.path(dest, paste0(prefix, suffix, ".csv"))
  write.csv(x, p, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(p)
}
checks <- list()
check <- function(ok, label) {
  checks[[length(checks) + 1L]] <<- data.frame(check = label, passed = isTRUE(ok))
  if (!isTRUE(ok)) stop(label, call. = FALSE)
}
require_fields <- function(x, fields, label) check(all(fields %in% names(x)), paste("Required fields:", label))
text <- function(x) ifelse(is.na(x), "", x)
candidate <- read_csv(inputs[["candidates"]])
geo <- read_csv(inputs[["geography"]])
coverage <- read_csv(inputs[["coverage"]])
xwalk <- read_csv(inputs[["university_crosswalk"]])
aliases <- read_csv(inputs[["pool_aliases"]])
source_catalog <- read_csv(inputs[["source_catalog"]])
require_fields(candidate, c("station_id", "state", "station_type", "parent_station_id", "university_id",
  "creation_date", "legal_establishment_date", "first_operation_date", "eligibility_status",
  "founding_affiliation_status", "affiliation_1910_status", "operating_1910", "snapshot_date"), "candidates")
check(!anyDuplicated(candidate$station_id), "Unique stable candidate identities")
check(all(candidate$eligibility_status %in% c("confirmed", "excluded", "unresolved")), "Valid eligibility classifications")
check(all(candidate$snapshot_date == "1910-06-30"), "Common June 30, 1910 reference date")
check(all(text(candidate$parent_station_id) == "" | candidate$parent_station_id %in% candidate$station_id), "All branch parent IDs resolve")
check(all(text(candidate$parent_station_id) != candidate$station_id), "No station is its own parent")
check(!anyDuplicated(geo$station_id) && setequal(candidate$station_id, geo$station_id), "One geography record per candidate")
check(!anyDuplicated(xwalk$university_id), "University crosswalk is unique")
confirmed <- candidate$eligibility_status == "confirmed"
check(all(candidate$founding_affiliation_status[confirmed] == "confirmed" &
          candidate$affiliation_1910_status[confirmed] == "confirmed" &
          candidate$operating_1910[confirmed] == "confirmed"), "Confirmed inventory meets all three evidence gates")
check(all(nzchar(text(candidate$university_id[confirmed]))), "Every included station has an institutional identity")
check(all(grepl("^https://", candidate$source_url)), "Candidate primary-source URLs present")
check(all(c(candidate$source_url, na.omit(candidate$history_source_url)) %in% source_catalog$source_url), "All historical research source URLs resolve to the curated catalog")
check(all(!is.na(candidate$source_pdf_pages)), "Candidate report page provenance present")
page_numbers <- as.integer(unlist(strsplit(candidate$source_pdf_pages, ";", fixed = TRUE)))
check(all(!is.na(page_numbers) & page_numbers >= 1L & page_numbers <= 582L), "All report PDF locators are valid one-based pages")

# Partial dates remain strings with their actual precision. Bounds are used
# only to detect impossible ordering, never to fill a missing source date.
date_bound <- function(x, upper = FALSE) {
  ans <- rep(as.Date(NA), length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i]) || !nzchar(x[i])) next
    if (grepl("^[0-9]{4}$", x[i])) {
      ans[i] <- as.Date(paste0(x[i], if (upper) "-12-31" else "-01-01"))
    } else if (grepl("^[0-9]{4}-[0-9]{2}$", x[i])) {
      first <- as.Date(paste0(x[i], "-01"))
      ans[i] <- if (upper) seq(first, by = "month", length.out = 2L)[2L] - 1 else first
    } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x[i])) {
      ans[i] <- as.Date(x[i])
    } else stop("Invalid partial date: ", x[i])
  }
  ans
}
for (field in c("creation_date", "legal_establishment_date", "first_operation_date")) {
  lo <- date_bound(candidate[[field]])
  check(all(is.na(lo[confirmed]) | lo[confirmed] <= as.Date("1910-06-30")), paste(field, "not after snapshot for included rows"))
}
legal_lo <- date_bound(candidate$legal_establishment_date)
op_hi <- date_bound(candidate$first_operation_date, TRUE)
check(all(is.na(legal_lo) | is.na(op_hi) | legal_lo <= op_hi), "Legal establishment does not follow known operation interval")
candidate$creation_year <- suppressWarnings(as.integer(substr(candidate$creation_date, 1L, 4L)))
candidate$legal_establishment_year <- suppressWarnings(as.integer(substr(candidate$legal_establishment_date, 1L, 4L)))
candidate$first_operation_year <- suppressWarnings(as.integer(substr(candidate$first_operation_date, 1L, 4L)))
candidate$first_operation_known_by_snapshot <- candidate$operating_1910 == "confirmed"

audit <- cbind(candidate, geo[match(candidate$station_id, geo$station_id), setdiff(names(geo), "station_id"), drop = FALSE])
audit$latitude <- as.numeric(audit$latitude)
audit$longitude <- as.numeric(audit$longitude)
audit$coordinate_precision_m <- as.integer(audit$coordinate_precision_m)
has_point <- !is.na(audit$latitude)
check(identical(is.na(audit$latitude), is.na(audit$longitude)), "Coordinates are paired or both missing")
check(all(audit$latitude[has_point] >= 18 & audit$latitude[has_point] <= 72 &
          audit$longitude[has_point] >= -180 & audit$longitude[has_point] <= -65), "Coordinates in US WGS84 range")
check(all(grepl("^reviewed_approximate_", audit$geography_status[has_point])), "Every numeric point is explicitly reviewed and approximate")
check(all(!is.na(audit$coordinate_precision_m[has_point]) & audit$coordinate_precision_m[has_point] > 0), "Coordinate precision present")
gn <- read_csv(inputs[["selected_geonames"]])
gix <- match(audit$geonameid[has_point], gn$geonameid)
check(!anyNA(gix), "Every point resolves to a saved GeoNames facility record")
check(all(audit$latitude[has_point] == as.numeric(gn$latitude[gix])) &&
          all(audit$longitude[has_point] == as.numeric(gn$longitude[gix])), "Points agree with saved facility source")
check(all(audit$state[has_point] == gn$admin1_code[gix]), "Facility-source states agree")

ref <- read.delim(inputs[["county_reference"]], sep = "|", colClasses = "character", check.names = FALSE,
  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
norm_county <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  tolower(gsub("[^a-zA-Z0-9]", "", sub(" (County|Parish|Borough|Census Area|Municipality|city)$", "", x)))
}
ref$GEOID <- paste0(ref$STATEFP, ref$COUNTYFP)
key <- paste(ref$STATE, norm_county(ref$COUNTYNAME))
county_ix <- match(paste(audit$state, norm_county(audit$county_2020_name_curated)), key)
has_county <- !is.na(audit$county_2020_name_curated)
check(!anyNA(county_ix[has_county]), "All curated county names resolve within their state in 2020 Census roster")
audit$GEOID <- ref$GEOID[county_ix]
audit$county_2020_name <- ref$COUNTYNAME[county_ix]
check(all(is.na(audit$GEOID) | grepl("^[0-9]{5}$", audit$GEOID)), "County GEOIDs retain leading zeros and five characters")
audit$county_polygon_GEOID <- NA_character_
if (any(has_point)) {
  counties <- sf::st_read(inputs[["county_geometry"]], quiet = TRUE)
  points <- sf::st_as_sf(audit[has_point, ], coords = c("longitude", "latitude"), crs = 4326)
  hits <- sf::st_intersects(sf::st_transform(points, sf::st_crs(counties)), counties)
  check(all(lengths(hits) == 1L), "Every reviewed point intersects exactly one 2020 county polygon")
  audit$county_polygon_GEOID[has_point] <- as.character(counties$GEOID[unlist(hits)])
  check(all(audit$GEOID[has_point] == audit$county_polygon_GEOID[has_point]), "Point-in-polygon verifies all coordinate county assignments")
}
audit$county_geography_year <- 2020L
audit$county_spatial_check <- ifelse(has_point, "passed", ifelse(has_county, "county_identity_only_no_point", "unresolved"))
amws <- unique(data.table::fread(inputs[["amws_panel"]], select = "GEOID", colClasses = "character", showProgress = FALSE)$GEOID)
audit$county_in_amws_panel <- ifelse(is.na(audit$GEOID), NA, audit$GEOID %in% amws)
check(all(audit$GEOID[!is.na(audit$GEOID)] %in% ref$GEOID), "County IDs compatible with 2020 reference; AMWS availability separately flagged")

# Institution identity matches, independent of station location. A branch in
# Davis, for example, belonged to the Berkeley university in 1910.
allocation <- read_csv(inputs[["allocation"]])
require_fields(allocation, c("College", "Treatment", "Year_Founded"), "allocation roster")
selected <- allocation[allocation$Treatment == "1", , drop = FALSE]
roster <- selected[!duplicated(selected$College), c("College", "State", "Year_Founded"), drop = FALSE]
names(roster) <- c("allocation_college", "allocation_state", "allocation_founded_year_reported")
check(nrow(roster) == 195L, "Full allocation roster has 195 distinct institutions")
check(all(na.omit(xwalk$allocation_college) %in% roster$allocation_college), "Reviewed identity matches exist in allocation roster")
check(!anyDuplicated(na.omit(xwalk$allocation_college)), "No allocation college receives multiple historical university identities")
roster$university_id <- xwalk$university_id[match(roster$allocation_college, xwalk$allocation_college)]
station_list <- function(uid, status) {
  if (is.na(uid)) return("")
  paste(audit$station_id[audit$university_id == uid & !is.na(audit$university_id) & audit$eligibility_status == status], collapse = "|")
}
for (status in c("confirmed", "unresolved", "excluded")) {
  roster[[paste0(status, "_station_ids")]] <- vapply(roster$university_id, station_list, character(1), status = status)
}
roster$confirmed_station_count <- vapply(strsplit(roster$confirmed_station_ids, "|", fixed = TRUE), function(x) sum(nzchar(x)), integer(1))
roster$with_qualifying_station_in_1910 <- roster$confirmed_station_count > 0L
roster$institution_founded_after_1910_reported <- as.numeric(roster$allocation_founded_year_reported) > 1910
roster$institution_founded_in_1910_precision_warning <- as.numeric(roster$allocation_founded_year_reported) == 1910
roster$historical_branch_site_held_by_other_university_in_1910 <- roster$allocation_college == "University of California Davis"
roster$station_assessment <- ifelse(roster$with_qualifying_station_in_1910, "confirmed_qualifying_station",
  ifelse(nzchar(roster$unresolved_station_ids), "candidate_unresolved",
    ifelse(roster$institution_founded_after_1910_reported, "institution_founded_after_1910_reported",
      ifelse(nzchar(roster$excluded_station_ids), "excluded_candidates_only_absence_not_established", "no_confirmed_match_absence_not_established"))))
roster$match_notes <- xwalk$match_notes[match(roster$university_id, xwalk$university_id)]
roster$match_notes[roster$allocation_college == "University of California Davis"] <- "University Farm at Davis is included, but its founding/1910 university is UC Berkeley. Do not count it twice as an independent modern campus."
roster$match_notes[is.na(roster$match_notes)] <- "No confirmed inventory identity match; do not interpret as historical absence."
check(!any(roster$with_qualifying_station_in_1910 & roster$institution_founded_after_1910_reported, na.rm = TRUE), "No confirmed university match is founded after snapshot in allocation roster")
audit$allocation_college <- xwalk$allocation_college[match(audit$university_id, xwalk$university_id)]
audit$allocation_identity_match_status <- xwalk$match_status[match(audit$university_id, xwalk$university_id)]

curated <- read_csv(inputs[["curated_pool"]])
treated <- curated[curated$sample_role == "treated", , drop = FALSE]
check(nrow(treated) == 57L && !anyDuplicated(treated$college), "Curated pool has 57 unique treated institutions")
ix <- match(treated$college, aliases$pool_college)
treated$allocation_college <- ifelse(is.na(ix), treated$college, aliases$allocation_college[ix])
check(all(treated$allocation_college %in% roster$allocation_college), "Every curated institution resolves by exact label or reviewed alias")
check(!anyDuplicated(treated$allocation_college), "Curated aliases do not duplicate institutions")
membership <- read_csv(inputs[["main_membership"]])
pool_ids <- function(timing) {
  z <- membership[membership$outcome == "amws_per_1000_births" & membership$timing == timing, "event_ids"]
  unique(unlist(strsplit(z, "|", fixed = TRUE)))
}
std_ids <- pool_ids("standard_decade")
shift_ids <- pool_ids("alternative_decade")
check(length(std_ids) == 16L && length(shift_ids) == 20L, "Saved main samples contain 16 standard and 20 shifted events")
check(all(c(std_ids, shift_ids) %in% treated$event_id), "All main-sample event IDs resolve to curated treated roster")
roster$in_full_roster <- TRUE
roster$in_curated_pool <- roster$allocation_college %in% treated$allocation_college
roster$in_standard_main_sample <- roster$allocation_college %in% treated$allocation_college[treated$event_id %in% std_ids]
roster$in_shifted_main_sample <- roster$allocation_college %in% treated$allocation_college[treated$event_id %in% shift_ids]
roster$curated_event_id <- treated$event_id[match(roster$allocation_college, treated$allocation_college)]
roster$interpretation <- "FALSE means not confirmed by this inventory; it is not a verified historical zero. Counts use university identity in 1910, not modern branch-city campus identity."
pool_fields <- c(full_roster = "in_full_roster", curated_pool = "in_curated_pool", standard_main_sample = "in_standard_main_sample", shifted_main_sample = "in_shifted_main_sample")
summary_rows <- lapply(names(pool_fields), function(pool) {
  z <- roster[roster[[pool_fields[[pool]]]], , drop = FALSE]
  n <- sum(z$with_qualifying_station_in_1910)
  data.frame(pool = pool, institutions = nrow(z), institutions_with_qualifying_station_in_1910 = n,
    share_with_qualifying_station_in_1910 = n / nrow(z), percent_with_qualifying_station_in_1910 = 100 * n / nrow(z),
    institutions_with_unresolved_candidates_without_confirmed_station = sum(z$station_assessment == "candidate_unresolved"),
    institutions_founded_after_1910_reported = sum(z$institution_founded_after_1910_reported, na.rm = TRUE),
    numerator_definition = "Institutions with a qualifying station in 1910 (confirmed only; count each university once)",
    limitation = "Confirmed-match share/lower bound, not completed historical prevalence. Unresolved candidates and omitted-branch coverage gaps remain.", stringsAsFactors = FALSE)
})
summary_table <- do.call(rbind, summary_rows)
check(identical(summary_table$institutions, c(195L, 57L, 16L, 20L)), "All four denominators match agreed pools")

check(nrow(coverage) == 51L && !anyDuplicated(coverage$state), "All 50 states and DC have one coverage check")
check(setequal(coverage$state, c(state.abb, "DC")), "Coverage jurisdiction universe is complete")
check(all(audit$state %in% coverage$state), "All candidates lie in reviewed jurisdiction universe")
for (status in c("confirmed", "unresolved", "excluded")) {
  coverage[[paste0(status, "_candidate_count")]] <- vapply(coverage$state, function(st) sum(audit$state == st & audit$eligibility_status == status), integer(1))
}
coverage$confirmed_university_count <- vapply(coverage$state, function(st) length(unique(na.omit(audit$university_id[audit$state == st & audit$eligibility_status == "confirmed"]))), integer(1))
inventory <- audit[audit$eligibility_status == "confirmed", , drop = FALSE]
check(!anyDuplicated(paste(inventory$state, inventory$station_name, inventory$city_1910)), "No duplicate confirmed station/name/site identities")
inventory <- inventory[order(inventory$state, inventory$station_type, inventory$station_id), ]
audit <- audit[order(audit$state, audit$station_id), ]
save_csv(inventory)
save_csv(audit, "_candidate_audit")
save_csv(coverage, "_jurisdiction_coverage")
save_csv(roster, "_allocation_matches")
save_csv(summary_table, "_allocation_pool_shares")
save_csv(source_catalog, "_source_catalog")
save_csv(audit[audit$eligibility_status == "unresolved", ], "_unresolved_candidates")
save_csv(audit[audit$eligibility_status == "confirmed" & is.na(audit$latitude), ], "_unresolved_geography")

manifest <- data.frame(input = names(inputs), path = unname(inputs), bytes = file.info(inputs)$size,
  md5 = unname(tools::md5sum(inputs)), stringsAsFactors = FALSE)
save_csv(manifest, "_input_manifest")
save_csv(do.call(rbind, checks), "_validation")

# Field dictionary contains every inventory/audit column. The source-date and
# geography semantics are explicit; no blank is silently turned into zero.
definitions <- c(
  station_id = "Stable candidate/station identity; renaming or relocation retains identity.",
  candidate_record_kind = "station_candidate, facility_screen, or network_gap. Network gaps and broad facility screens are audit leads, not invented confirmed stations.",
  state = "Present-day two-letter jurisdiction containing the 1910 site.",
  station_name = "Reviewed historical station name, not necessarily verbatim USDA spelling.",
  aliases = "Other names for the same identity, separated by semicolons.",
  station_type = "main or branch; Tuskegee is a state-authorized branch with its own founding institution.",
  parent_station_id = "Parent station/network identity; may itself fail strict inclusion while a separately founded branch qualifies.",
  city_1910 = "Historical locality; is not a geocoded city-centroid location.",
  university_id = "Stable historical university identity used for matching; modern branch campuses not automatically separate.",
  founding_university = "Institution/organization associated at foundation; excluded candidates may name independent founder.",
  university_1910 = "Affiliation at reference date; independent organizations explicitly identified.",
  eligibility_status = "confirmed, excluded, unresolved. Confirmed inventory filters this field only.",
  eligibility_reason = "Evidence-based inclusion/exclusion reason or remaining question.",
  creation_date = "Preferred sourced creation/organization date at actual precision YYYY, YYYY-MM, YYYY-MM-DD. See kind and conflict notes; never a default 1887.",
  creation_date_kind = "Meaning of creation_date: organization, legislative_authorization, custody_for_experiment_farm, estate_experimental_use, or unknown.",
  creation_date_precision = "day/month/year/unknown; dates remain partial strings.",
  legal_establishment_date = "Separately evidenced legal authorization/establishment date; blank when unverified.",
  first_operation_date = "Separately sourced first operation date; blank may coexist with positive evidence of operation by 1910.",
  original_organization_reported = "Original organization date transcribed from USDA Table 11, normalized to actual ISO precision; blank cells remain blank.",
  hatch_organization_reported = "Organization under Hatch Act in USDA Table 11; NOT automatically legal or first operating date.",
  operating_1910 = "confirmed, unresolved, closed_before_snapshot, after_snapshot, or not_qualifying.",
  founding_affiliation_status = "confirmed/no/unresolved for association with the eligible university from foundation.",
  affiliation_1910_status = "confirmed/no/unresolved for continued same-institution association at snapshot.",
  date_conflict = "yes if competing organization/date claims retained; blank otherwise.",
  date_notes = "Differences among legal, organizational, operating and source dates; conflicts not overwritten.",
  source_pdf_pages = "One-based PDF page numbers in saved 582-page USDA 1910 scan, separated by semicolons.",
  source_printed_pages = "Printed Table 11 date page (300/302/304/306) when applicable; inspection locator is PDF-page field.",
  source_url = "Primary national report reference.", history_source_url = "Institutional/contemporary historical evidence URL; national report supplements it.",
  evidence_notes = "Paraphrased factual basis, identity decisions and caveats; no inferred absence from silence.",
  snapshot_date = "Fixed 1910-06-30; report published 1911 concerns fiscal year ended 1910.",
  source_checked_date = "Research review date, not source publication date.",
  creation_year = "Year extracted from known creation_date, not imputed.", legal_establishment_year = "Year of separately sourced legal date, not imputed.",
  first_operation_year = "Year of separately sourced operation date, not imputed.",
  first_operation_known_by_snapshot = "Positive operation evidence in fiscal-year report; does not manufacture an exact first-operation date.",
  site_1910 = "Historical campus/station locality identified in source.",
  latitude = "WGS84 latitude of reviewed historic-campus/station reference point; blank when unresolved.",
  longitude = "WGS84 longitude; never a city/county centroid substitute.",
  coordinate_datum = "WGS84 (EPSG:4326).", coordinate_kind = "historic_campus_reference, historic_station_reference or unresolved.",
  coordinate_precision_m = "Approximate campus/station positional precision in metres (1000 or 500); NOT survey accuracy.",
  geonameid = "Manually selected GeoNames facility identifier; checked against saved source snapshot.",
  coordinate_source_url = "URL supporting facility reference point; historical-location source is separate.",
  historical_location_source_url = "Evidence locating the site in 1910, rather than just modern station address.",
  historical_location_source_locator = "National report page locator; see history URL for site continuity/relocation.",
  geography_status = "reviewed_approximate_historic_campus, reviewed_approximate_historic_station, or unresolved_1910_point.",
  county_2020_name_curated = "Reviewed target 2020 county name; no county imputation from parent university.",
  county_verification_method = "Historical-site locality plus 2020 county roster, with independent polygon check when point available.",
  geography_discrepancy = "yes for source county error; historical_county_boundary_change for valid historical/2020 difference; no otherwise.",
  geography_notes = "Location/precision, original-property continuity, relocation and county-boundary caveats.",
  geography_review_date = "Individual geography review date.", GEOID = "Five-character 2020 county FIPS, stored as text; original CT counties retained.",
  county_2020_name = "Official name in saved 2020 Census county roster.", county_polygon_GEOID = "Independent 2020 polygon intersection result, blank without point.",
  county_geography_year = "2020.", county_spatial_check = "passed, county_identity_only_no_point or unresolved.",
  county_in_amws_panel = "Whether GEOID appears in existing AMWS combined county-year panel; blank if county unresolved. FALSE does not change station eligibility.",
  allocation_college = "Exact institution label in full allocation roster; blank if no institution match.",
  allocation_identity_match_status = "reviewed_identity or not_in_allocation_roster; no fuzzy matching."
)
dictionary <- data.frame(field = names(audit), storage_type = vapply(audit, function(x) class(x)[1L], character(1)),
  definition = unname(definitions[names(audit)]), stringsAsFactors = FALSE)
check(!anyNA(dictionary$definition), "Every delivered station/audit field has a dictionary definition")
save_csv(dictionary, "_field_dictionary")
companion_objects <- list(allocation_matches = roster, allocation_pool_shares = summary_table,
  jurisdiction_coverage = coverage, input_manifest = manifest, source_catalog = source_catalog)
companion_definitions <- c(
  allocation_college = "Exact unique College label in Treatment=1 allocation source.",
  allocation_state = "Allocation source State label, preserved.",
  allocation_founded_year_reported = "Allocation Year_Founded, not independently corrected/researched in this task.",
  university_id = "Reviewed stable historical identity; missing means no curated institution match.",
  confirmed_station_ids = "Pipe-separated confirmed station IDs; empty is not verified absence.",
  unresolved_station_ids = "Pipe-separated unresolved candidate IDs mapped to this institution.",
  excluded_station_ids = "Pipe-separated excluded candidate IDs mapped to this institution.",
  confirmed_station_count = "Number of confirmed station identities; university numerator counts only once.",
  with_qualifying_station_in_1910 = "TRUE only with at least one confirmed qualifying station. FALSE is not a verified historical zero.",
  institution_founded_after_1910_reported = "Allocation founding year >1910; reported-source flag, not a new date determination.",
  institution_founded_in_1910_precision_warning = "Founding year is 1910; exact date relative to June 30 is unknown.",
  historical_branch_site_held_by_other_university_in_1910 = "University Farm at Davis was held by Berkeley in 1910; flag modern Davis allocation row.",
  station_assessment = "Confirmed qualifying, candidate unresolved, post-1910-founded, excluded-only, or no confirmed match; latter two are not absence determinations.",
  match_notes = "Reviewed identity/renaming/branch-parent decision or unconfirmed-match limitation.",
  in_full_roster = "Membership in 195 distinct Treatment=1 College institutions.",
  in_curated_pool = "Membership in saved 57-institution curated treated pool.",
  in_standard_main_sample = "Membership in saved 16-event standard_decade AMWS births main sample.",
  in_shifted_main_sample = "Membership in saved 20-event alternative_decade AMWS births main sample.",
  curated_event_id = "Event ID in saved curated treated roster; blank outside curated pool.",
  interpretation = "Warning that non-confirmation is not established absence, and historical university identity governs attribution.",
  pool = "One of full_roster, curated_pool, standard_main_sample, shifted_main_sample.",
  institutions = "Number of distinct institutions in the specified pool; denominator includes post-1910 founded institutions.",
  institutions_with_qualifying_station_in_1910 = "Number with one or more confirmed qualifying stations at snapshot; count university once.",
  share_with_qualifying_station_in_1910 = "Confirmed university numerator / all pool institutions; lower bound given coverage gaps.",
  percent_with_qualifying_station_in_1910 = "100 times confirmed share; explicitly qualifying station in 1910.",
  institutions_with_unresolved_candidates_without_confirmed_station = "Pool institutions with unresolved mapped candidates but no confirmed positive.",
  institutions_founded_after_1910_reported = "Count of pool institutions whose allocation founding year exceeds 1910.",
  numerator_definition = "Positive-only numerator definition, not a claim of exhaustive national coverage.",
  limitation = "Remaining candidate/coverage limits affecting percentage interpretation.",
  state = "Current two-letter jurisdiction abbreviation.", jurisdiction = "Current state/DC name; Alaska and Hawaii included despite 1910 territorial status.",
  reference_date = "1910-06-30.", national_report_reviewed = "yes indicates national inspection/directory screening.",
  institutional_sources_reviewed = "yes if institutional history/archive source screened; partial where only national sources or federal source screen available. Does not certify complete institutional-network enumeration.",
  national_source_url = "Primary 1910 USDA source.", institutional_source_urls = "Semicolon-separated institutional/primary source URLs used in screening.",
  branch_reconciliation_status = "partial until contemporary network completeness is established.", coverage_complete = "no means exhaustive branch/institution absence has not been certified.",
  coverage_notes = "Jurisdiction-specific unresolved branch network, identity or source gaps.", absence_inference_allowed = "no: missing confirmed inventory row is not evidence of absence.",
  confirmed_candidate_count = "Number of confirmed station IDs in jurisdiction.", unresolved_candidate_count = "Number of unresolved candidate IDs in jurisdiction.",
  excluded_candidate_count = "Number of excluded candidate/screening IDs in jurisdiction.", confirmed_university_count = "Unique university identities among confirmed stations in jurisdiction.",
  input = "Builder input role.", path = "Absolute file path used in this run.", bytes = "Input byte count at rebuild.", md5 = "MD5 content checksum for reproducibility, not a security guarantee.",
  source_id = "Stable URL-derived source identifier in curated catalog.",
  source_url = "Primary historical research reference URL.", source_title = "Source title where recovered; otherwise descriptive reference label.",
  local_primary_file = "Project-relative complete primary scan, if locally saved; blank for a web-only reference.",
  local_primary_sha256 = "SHA256 of locally saved complete primary scan, if available.",
  discovery_capture_files = "Pipe-separated discovery text captures under raw/usda_1910; these are not complete primary-source scans.",
  used_by_station_ids = "Pipe-separated candidate IDs citing this source.", evidence_role = "National enumeration/inspection or history/affiliation/date/location evidence.",
  capture_status = "Complete local primary scan versus referenced web primary source; no assertion every referenced page is locally archived.",
  review_date = "Research reference review date."
)
companion_dictionary <- do.call(rbind, lapply(names(companion_objects), function(nm) {
  x <- companion_objects[[nm]]
  data.frame(file_suffix = nm, field = names(x), storage_type = vapply(x, function(z) class(z)[1L], character(1)),
    definition = unname(companion_definitions[names(x)]), stringsAsFactors = FALSE)
}))
check(!anyNA(companion_dictionary$definition), "Every allocation/coverage/manifest field has a dictionary definition")
save_csv(companion_dictionary, "_companion_field_dictionary")
save_csv(do.call(rbind, checks), "_validation")

readme <- c(
  "# University-associated agricultural experiment stations: June 30, 1910", "",
  sprintf("This release contains %d confirmed station identities at %d universities, plus an audit of %d candidate/screening records (%d unresolved, %d excluded). Audit records include unidentified network leads and facility screens, marked in candidate_record_kind; they are not all established station identities. It is a documented confirmed inventory, not a certified exhaustive national census.",
    nrow(inventory), length(unique(inventory$university_id)), nrow(audit), sum(audit$eligibility_status == "unresolved"), sum(audit$eligibility_status == "excluded")), "",
  "Source: USDA, Annual Report of the Office of Experiment Stations for the year ended June 30, 1910 (published September 14, 1911), saved unchanged in Data/raw/scientific_facilities/usda_1910/annualreportoes1910unit.pdf. The scan has 582 PDF pages. Table 11 date columns (printed pp.300,302,304,306; PDF pp.328,330,332,334) were visually reviewed. State inspections and overview pp.62-66 supply additional branches.", "",
  "## Inclusion and identity", "",
  "Confirm only stations operating by June 30, 1910, associated with a university/college from foundation and still associated with that same institution at snapshot. Institutional renaming/merger continuity is allowed (Florida Agricultural College into University of Florida); acquisition by a different institution is not. Branches get separate stable IDs; renaming does not create a new station. Farms or laboratories qualify only where the evidence identifies an experimental-station branch, not merely teaching or temporary trials.", "",
  "The university need not have held the word university in its 1910 name. Tuskegee's separately staffed state-authorized branch counts as Tuskegee. Rutgers has two separately organized stations but one university. The merged Amherst state/Hatch stations count once in 1910. A parent station's acquisition history does not automatically disqualify a later branch founded from the outset by the receiving university.", "",
  "Exclude independent federal/state/planters stations subsequently acquired by universities, extension-only/demonstration-only offices, teaching-only farms, temporary plots and stations closed before cutoff. Geneva, Mountain Grove, the Louisiana Sugar Station and Alaska's federal stations fail founding affiliation. Connecticut's Wesleyan-founded station and Ohio's OSU-founded station fail continuing same-institution association. North Carolina's 1877 station was transferred from UNC to NC A&M; its later university label is not backdated.", "",
  "## Dates and conflicts", "",
  "Creation, legal establishment, first operation and the original/Hatch organization columns are separate. YYYY and YYYY-MM retain actual source precision. Blank never becomes 1887 or zero. Contemporary reports proving that research was underway establish an upper bound of the snapshot for first operation, not an invented exact opening date. Conflicting institutional and USDA dates remain in date_conflict/date_notes (including Arizona, Arkansas, Purdue, Nevada, West Virginia, South Haven and Edgeley). Research reported after the fiscal-year close is not sufficient evidence of operation by June 30.", "",
  "## Geography", "",
  sprintf("%d of the %d confirmed stations have reviewed approximate reference points, and %d have a reviewed county identity. All other coordinates remain missing. Points use historic campus/station reference locations, not city/county centroids. Precision is 500 m at station scale or 1 km at campus scale; not a surveyed 1910 plot.",
    sum(!is.na(inventory$latitude)), nrow(inventory), sum(!is.na(inventory$GEOID))), "",
  "The builder verifies each facility point against saved GeoNames source rows and 2020 Census county polygons. County-only rows use historical-site locality plus the 2020 county roster; county_spatial_check distinguishes these from polygon-verified points. CT uses the 2020 original counties, not new planning regions. Wright County corrects Mountain Grove's published Texas County. Spooner is Washburn County, despite USDA's Washington County. Moccasin and Gooding use modern counties created after the snapshot, explicitly flagged as boundary changes. Highmoor's original cross-county farm remains unresolved.", "",
  "1910 relocations matter: LSU was at its old northern Baton Rouge campus; the Rice Station was west of Crowley, not modern Rayne; Riverside was at Rubidoux, not the post-1917 campus; early Williston and other relocated branches require historical parcel work. Modern facilities with similar names are not substituted.", "",
  "## Coverage limitations", "",
  "Every one of the 50 present-day states and DC has a coverage row. All national-report state sections have been screened; institutional sources are recorded. Branch reconciliation is PARTIAL and coverage_complete is NO: additional contemporary university reports/catalogs are still needed to certify omitted-branch completeness, including non-land-grant universities. See the jurisdiction coverage file for specific remaining networks (Arkansas unnamed substations, Washington's second branch, Virginia's remaining tobacco sites, and others). Thus no missing institution is assigned a verified historical zero. Unresolved candidates and geography are separately exported for follow-up.", "",
  "The confirmed inventory is usable as positive matches. It does not measure later station acquisition/establishment, and should not be used as an exhaustive binary absence treatment until the coverage gaps are closed.", "",
  "## Allocation pools and interpretation", "",
  "Full roster: 195 unique College labels among Treatment=1 rows of College_Control_Towns.csv (not experiment IDs). Curated pool: 57 treated institutions in andrews_event_county_units_1850_1920.csv. Standard and shifted main samples: union of event_ids in saved events_by_cohort.csv for amws_per_1000_births and standard_decade/alternative_decade, respectively (16 and 20 institutions). These are the previously agreed saved sample pools, not a re-estimation of current analysis scripts.", "",
  "Match via the reviewed historical-university crosswalk and six explicit spelling/name aliases. Count every institution once regardless of branch count. The University Farm at Davis qualifies under its 1910 parent UC Berkeley; the modern Davis record is flagged separately and receives no duplicate positive. Nebraska-Lincoln and Tuskegee are not in this allocation roster; do not substitute other in-state institutions.", "",
  "All percentages below mean institutions WITH A QUALIFYING STATION IN 1910, confirmed only. They are confirmed-match lower bounds given unresolved candidates and coverage gaps. FALSE in with_qualifying_station_in_1910 means no confirmed match, not verified absence. Universities founded after 1910 are separately flagged using allocation Year_Founded; those reported years are not independently re-researched here.", "",
  "| Pool | Institutions with a qualifying station in 1910 | Institutions | Share |", "|---|---:|---:|---:|",
  vapply(seq_len(nrow(summary_table)), function(i) sprintf("| %s | %d | %d | %.2f%% |", summary_table$pool[i], summary_table$institutions_with_qualifying_station_in_1910[i], summary_table$institutions[i], summary_table$percent_with_qualifying_station_in_1910[i]), character(1)), "",
  "## Files and rebuilding", "",
  "Main CSV = confirmed inventory. Companion suffixes: _candidate_audit, _unresolved_candidates, _unresolved_geography, _jurisdiction_coverage, _field_dictionary, _companion_field_dictionary, _allocation_matches, _allocation_pool_shares, _source_catalog, _validation, _input_manifest. Curated research inputs, candidate-specific national-report context, raw-source manifest and facility-source snapshots are in Data/intermediary/scientific_facilities/usda_1910/. The source catalog distinguishes the complete local primary scan from web references and discovery captures. Discovery text captures are not substituted for original primary-source documents. The existing Kantor-derived files are preserved.", "",
  "Run from any working directory in PowerShell:", "", "```powershell",
  "$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'",
  "& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' 'C:/Users/megaj/repos/gtl_talent_dets/prep/scientific_facilities/build_usda_1910_stations.R'", "```", "",
  "Requires sf and data.table plus the local input files enumerated in the manifest. No network calls, automatic geocoding, fuzzy university matching, date imputation or analysis changes occur during rebuilding. The input manifest records MD5 and bytes. Source PDFs are read-only."
)
writeLines(readme, file.path(dest, paste0(prefix, "_README.md")), useBytes = TRUE)
cat(sprintf("Built %d confirmed stations (%d universities), %d audited candidates; %d checks passed.\n", nrow(inventory), length(unique(inventory$university_id)), nrow(audit), length(checks)))
print(summary_table[, 1:5], row.names = FALSE)
