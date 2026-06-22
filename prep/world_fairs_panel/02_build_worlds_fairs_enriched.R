###############################################################################
# Build the enriched world's fairs file, 1790-1960.
#
# This is the active enrichment entrypoint. It does not perform new internet
# searches. It combines the scraped Wikipedia list with previously extracted
# host geocodes, visits, venues, and venue coordinates.
#
# Inputs:
#   input/worlds_fairs_wikipedia.xlsx
#   input/worlds_fairs_wikipedia_geocoded.xlsx
#   input/worlds_fairs/worlds_fairs_visits_1790_1910_with_venues_exhaustive.csv
#   input/worlds_fairs/worlds_fairs_additions_1911_1960_agent_batch{1..4}.csv
#
# Outputs:
#   input/worlds_fairs/worlds_fairs_1790_1960_with_visits_venues.csv
#   input/worlds_fairs/worlds_fairs_1790_1960_with_visits_venues.xlsx
#   input/worlds_fairs/worlds_fairs_1790_1960_with_visits_venues_summary.txt
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/02_build_worlds_fairs_enriched.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
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
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

worlds_fairs_dir <- file.path(DATA_INPUT, "worlds_fairs")
dir.create(worlds_fairs_dir, recursive = TRUE, showWarnings = FALSE)

scrape_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia.xlsx")
geocoded_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia_geocoded.xlsx")
old_enriched_file <- file.path(
  worlds_fairs_dir,
  "worlds_fairs_visits_1790_1910_with_venues_exhaustive.csv"
)
batch_paths <- file.path(
  worlds_fairs_dir,
  sprintf("worlds_fairs_additions_1911_1960_agent_batch%d.csv", 1:4)
)

out_csv <- file.path(
  worlds_fairs_dir,
  "worlds_fairs_1790_1960_with_visits_venues.csv"
)
out_xlsx <- file.path(
  worlds_fairs_dir,
  "worlds_fairs_1790_1960_with_visits_venues.xlsx"
)
out_summary <- file.path(
  worlds_fairs_dir,
  "worlds_fairs_1790_1960_with_visits_venues_summary.txt"
)
out_excluded_non_realized <- file.path(
  worlds_fairs_dir,
  "worlds_fairs_1790_1960_excluded_non_realized.csv"
)
venue_research_file <- file.path(
  worlds_fairs_dir,
  "worlds_fairs_missing_venue_coordinate_research_2026_06_18.csv"
)

required_files <- c(scrape_file, geocoded_file, old_enriched_file, batch_paths)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required input files:\n", paste(missing_files, collapse = "\n"))
}

###############################################################################
# Helpers
###############################################################################

parse_first_year <- function(x) {
  suppressWarnings(as.integer(str_extract(as.character(x), "[0-9]{4}")))
}

normalize_key_text <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

non_realized_reason <- function(text) {
  case_when(
    str_detect(text, "cancelled|canceled") ~ "cancelled",
    str_detect(text, "never held") ~ "never_held",
    str_detect(text, "not held") ~ "not_held",
    str_detect(text, "postponed") ~ "postponed",
    str_detect(text, "planned") ~ "planned_only",
    str_detect(text, "^intended\\b|\\bintended\\b") ~ "intended_only",
    TRUE ~ NA_character_
  )
}

as_integer_checked <- function(x, field_name) {
  x_chr <- na_if(str_squish(as.character(x)), "")
  out <- suppressWarnings(as.integer(x_chr))
  invalid <- !is.na(x_chr) & is.na(out)
  if (any(invalid)) {
    stop(
      "Non-integer values in ", field_name, ": ",
      paste(unique(x_chr[invalid]), collapse = ", ")
    )
  }
  out
}

as_numeric_checked <- function(x, field_name) {
  x_chr <- na_if(str_squish(as.character(x)), "")
  out <- suppressWarnings(as.numeric(x_chr))
  invalid <- !is.na(x_chr) & is.na(out)
  if (any(invalid)) {
    stop(
      "Non-numeric values in ", field_name, ": ",
      paste(unique(x_chr[invalid]), collapse = ", ")
    )
  }
  out
}

ensure_columns <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  for (col in missing_cols) {
    data[[col]] <- NA_character_
  }
  data
}

make_fair_key <- function(data) {
  data %>%
    mutate(
      key_year = as.integer(year_start),
      key_city = normalize_key_text(City),
      key_fair = normalize_key_text(Fair_name)
    )
}

reviewed_non_realized_reason <- function(source_period, source_row_id) {
  case_when(
    source_period == "1790_1910" & source_row_id %in% c("84", "141", "208", "233", "274", "276") ~
      "curated_pre_1911_never_held",
    source_period == "1911_1960" & source_row_id == "365" ~
      "researched_not_held",
    source_period == "1911_1960" & source_row_id == "416" ~
      "opened_in_1939_not_1935",
    source_period == "1911_1960" & source_row_id == "431" ~
      "researched_planned_only_not_held",
    TRUE ~ NA_character_
  )
}

###############################################################################
# Scrape and geocodes
###############################################################################

cat("Reading scrape and host geocodes...\n")

scrape_required_cols <- c("Year", "City", "Country", "Fair_name", "Fair_observation")

scrape <- read_xlsx(scrape_file) %>%
  ensure_columns(scrape_required_cols) %>%
  select(all_of(scrape_required_cols)) %>%
  mutate(
    scrape_row_id = row_number(),
    year_start = parse_first_year(Year),
    search_text = str_to_lower(str_squish(paste(
      coalesce(Year, ""),
      coalesce(City, ""),
      coalesce(Country, ""),
      coalesce(Fair_name, ""),
      coalesce(Fair_observation, "")
    ))),
    exclusion_reason = non_realized_reason(search_text),
    excluded_non_realized = !is.na(exclusion_reason)
  )

geocode_required_cols <- c(
  "Year", "City", "Country", "Fair_name", "Fair_observation", "year_start",
  "lat", "lon", "geonameid", "matched_name", "matched_country_iso2",
  "matched_country_iso3", "admin1_code", "admin1_name", "match_source",
  "match_score", "needs_review"
)

geocoded <- read_xlsx(geocoded_file) %>%
  ensure_columns(geocode_required_cols) %>%
  select(all_of(geocode_required_cols)) %>%
  mutate(
    year_start = as.integer(year_start),
    host_lat = as.numeric(lat),
    host_lon = as.numeric(lon),
    host_geonameid = suppressWarnings(as.integer(geonameid)),
    host_match_score = suppressWarnings(as.numeric(match_score)),
    host_needs_review = as.logical(needs_review)
  ) %>%
  make_fair_key() %>%
  transmute(
    key_year,
    key_city,
    key_fair,
    geocoded_country = Country,
    geocoded_fair_observation = Fair_observation,
    host_lat,
    host_lon,
    host_geonameid,
    host_matched_name = matched_name,
    host_matched_country_iso2 = matched_country_iso2,
    host_matched_country_iso3 = matched_country_iso3,
    host_admin1_code = admin1_code,
    host_admin1_name = admin1_name,
    host_match_source = match_source,
    host_match_score,
    host_needs_review
  )

duplicate_geocode_keys <- geocoded %>%
  count(key_year, key_city, key_fair, name = "n") %>%
  filter(n > 1L)
if (nrow(duplicate_geocode_keys) > 0L) {
  stop(
    "Duplicate geocode keys found:\n",
    paste(capture.output(print(duplicate_geocode_keys, n = Inf)), collapse = "\n")
  )
}

###############################################################################
# 1911-1960 researched additions from scrape and existing batches
###############################################################################

cat("Building 1911-1960 researched additions...\n")

candidate_window <- scrape %>%
  filter(!is.na(year_start), year_start >= 1911L, year_start <= 1960L)

additions <- candidate_window %>%
  filter(!excluded_non_realized) %>%
  transmute(
    scrape_row_id,
    row_id = scrape_row_id,
    Year,
    year_start,
    City,
    Country,
    Fair_name,
    Fair_observation
  ) %>%
  arrange(year_start, Country, City, Fair_name)

excluded <- candidate_window %>%
  filter(excluded_non_realized)

if (nrow(candidate_window) != 152L) {
  stop("Expected 152 scraped candidate rows in 1911-1960; found ", nrow(candidate_window))
}
if (nrow(excluded) != 7L) {
  stop("Expected 7 non-realized rows to exclude; found ", nrow(excluded))
}
if (nrow(additions) != 145L) {
  stop("Expected 145 additions after exclusions; found ", nrow(additions))
}

research_required_cols <- c(
  "row_id", "Fair_name", "City", "Year",
  "venue", "venue_source_title", "venue_source_url", "venue_note",
  "venue_latitude", "venue_longitude",
  "venue_coordinates_source_title", "venue_coordinates_source_url",
  "venue_coordinates_note", "visits", "visits_measure", "source_tier",
  "confidence", "source_title", "source_url", "source_note", "search_status"
)

read_research_batch <- function(path) {
  x <- read_csv(
    path,
    show_col_types = FALSE,
    na = c("", "NA", "N/A", "na"),
    col_types = cols(.default = col_character())
  )
  missing_cols <- setdiff(research_required_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop("Missing columns in ", basename(path), ": ", paste(missing_cols, collapse = ", "))
  }
  x %>%
    select(all_of(research_required_cols)) %>%
    mutate(batch_file = basename(path))
}

researched <- bind_rows(lapply(batch_paths, read_research_batch)) %>%
  mutate(
    row_id = as.integer(row_id),
    Year = as.integer(Year),
    visits = as_integer_checked(visits, "visits"),
    venue_latitude = as_numeric_checked(venue_latitude, "venue_latitude"),
    venue_longitude = as_numeric_checked(venue_longitude, "venue_longitude"),
    source_tier = str_replace(source_tier, "^tier(\\d+)$", "tier_\\1"),
    across(where(is.character), ~na_if(str_squish(.x), ""))
  )

expected_ids <- additions$row_id
found_ids <- researched$row_id
duplicate_ids <- found_ids[duplicated(found_ids)]
missing_ids <- setdiff(expected_ids, found_ids)
extra_ids <- setdiff(found_ids, expected_ids)

if (length(duplicate_ids) > 0L) {
  stop("Duplicate row_id values in researched batches: ", paste(unique(duplicate_ids), collapse = ", "))
}
if (length(missing_ids) > 0L) {
  stop("Missing row_id values in researched batches: ", paste(missing_ids, collapse = ", "))
}
if (length(extra_ids) > 0L) {
  stop("Unexpected row_id values in researched batches: ", paste(extra_ids, collapse = ", "))
}

allowed_status <- c("found", "conflicting_sources", "ambiguous_match", "not_found")
allowed_confidence <- c("high", "medium", "low")

invalid_status <- researched %>%
  filter(!search_status %in% allowed_status | is.na(search_status)) %>%
  distinct(row_id, search_status)
invalid_confidence <- researched %>%
  filter(!confidence %in% allowed_confidence | is.na(confidence)) %>%
  distinct(row_id, confidence)

if (nrow(invalid_status) > 0L) {
  stop("Invalid search_status values:\n", paste(capture.output(print(invalid_status)), collapse = "\n"))
}
if (nrow(invalid_confidence) > 0L) {
  stop("Invalid confidence values:\n", paste(capture.output(print(invalid_confidence)), collapse = "\n"))
}

new_enriched <- additions %>%
  left_join(
    researched %>% select(-batch_file, -Fair_name, -City, -Year),
    by = "row_id"
  )

###############################################################################
# 1790-1910 curated enrichment and final consolidation
###############################################################################

cat("Combining 1790-1910 and 1911-1960 enriched records...\n")

final_cols_without_host <- c(
  "fair_id", "source_period", "source_row_id", "scrape_row_id",
  "Year", "year_start", "City", "Country", "Fair_name", "Fair_observation",
  "visits", "visits_measure", "source_tier", "confidence", "source_title",
  "source_url", "source_note", "search_status", "source_status",
  "venue", "venue_source_title", "venue_source_url", "venue_note",
  "venue_latitude", "venue_longitude",
  "venue_coordinates_source_title", "venue_coordinates_source_url",
  "venue_coordinates_note", "venue_search_status"
)

venue_level_cols <- c(
  "parent_fair_id", "venue_seq", "n_venues_for_fair",
  "fair_event_weight", "venue_row_type"
)

host_cols <- c(
  "host_lat", "host_lon", "host_geonameid", "host_matched_name",
  "host_matched_country_iso2", "host_matched_country_iso3",
  "host_admin1_code", "host_admin1_name", "host_match_source",
  "host_match_score", "host_needs_review", "host_geocode_override"
)

final_cols_base <- append(final_cols_without_host, host_cols, after = 10L)
final_cols <- append(final_cols_base, venue_level_cols, after = 1L)

venue_update_cols <- c(
  "venue", "venue_source_title", "venue_source_url", "venue_note",
  "venue_latitude", "venue_longitude",
  "venue_coordinates_source_title", "venue_coordinates_source_url",
  "venue_coordinates_note", "venue_search_status"
)

as_numeric_or_na <- function(x) {
  suppressWarnings(as.numeric(na_if(str_squish(as.character(x)), "")))
}

apply_venue_research_updates <- function(combined) {
  if (!file.exists(venue_research_file)) {
    warning("Missing venue research file; skipping venue-coordinate overrides: ", venue_research_file)
    return(combined)
  }

  updates <- read_csv(
    venue_research_file,
    show_col_types = FALSE,
    col_types = cols(.default = col_character()),
    na = c("", "NA", "N/A", "na")
  ) %>%
    mutate(
      fair_id = as.integer(fair_id),
      venue_latitude = as_numeric_or_na(venue_latitude),
      venue_longitude = as_numeric_or_na(venue_longitude)
    ) %>%
    select(any_of(c("fair_id", venue_update_cols)))

  missing_ids <- setdiff(updates$fair_id, combined$fair_id)
  if (length(missing_ids) > 0L) {
    stop("Venue research fair_id values not found in combined file: ", paste(missing_ids, collapse = ", "))
  }

  combined %>%
    rows_update(updates, by = "fair_id", unmatched = "ignore")
}

make_multisite_venue_rows <- function() {
  tribble(
    ~parent_fair_id, ~venue_seq, ~venue, ~venue_latitude, ~venue_longitude, ~venue_coordinates_source_title, ~venue_coordinates_source_url, ~venue_coordinates_note,
    305L, 1L, "Statue of Liberty", 40.689249, -74.044500, "Wikidata/OpenStreetMap - Statue of Liberty", "https://www.openstreetmap.org/search?query=Statue%20of%20Liberty", "Fixed illuminated site documented for the Hudson-Fulton Celebration; coordinate is the monument point.",
    305L, 2L, "Grant's Tomb", 40.813376, -73.963087, "Wikidata/OpenStreetMap - General Grant National Memorial", "https://www.openstreetmap.org/search?query=Grant%27s%20Tomb%20New%20York", "Fixed illuminated site documented for the Hudson-Fulton Celebration; coordinate is the memorial point.",
    305L, 3L, "Soldiers' and Sailors' Monument, Riverside Park", 40.793241, -73.972376, "Wikidata/OpenStreetMap - Soldiers' and Sailors' Monument New York", "https://www.openstreetmap.org/search?query=Soldiers%20and%20Sailors%20Monument%20Riverside%20Park", "Fixed illuminated site documented for the Hudson-Fulton Celebration; coordinate is the monument point.",
    305L, 4L, "Washington Square Arch", 40.731113, -73.997333, "Wikidata/OpenStreetMap - Washington Square Arch", "https://www.openstreetmap.org/search?query=Washington%20Square%20Arch%20New%20York", "Fixed illuminated site documented for the Hudson-Fulton Celebration; coordinate is the arch point.",
    305L, 5L, "Brooklyn Institute of Arts and Sciences / Brooklyn Museum", 40.671206, -73.963630, "Wikidata/OpenStreetMap - Brooklyn Museum", "https://www.openstreetmap.org/search?query=Brooklyn%20Museum", "Fixed museum site documented for the Hudson-Fulton Celebration; coordinate is Brooklyn Museum.",
    305L, 6L, "Metropolitan Museum of Art", 40.779437, -73.963244, "Wikidata/OpenStreetMap - Metropolitan Museum of Art", "https://www.openstreetmap.org/search?query=Metropolitan%20Museum%20of%20Art", "Fixed exhibition site documented for the Hudson-Fulton Celebration; coordinate is the museum.",
    307L, 1L, "San Francisco Ferry Building / Pier 2 waterfront", 37.795490, -122.393700, "Wikidata/OpenStreetMap - San Francisco Ferry Building", "https://www.openstreetmap.org/search?query=San%20Francisco%20Ferry%20Building", "Fixed waterfront arrival site reported for the Portola Festival; coordinate uses the Ferry Building as the fixed landmark.",
    307L, 2L, "Union Square", 37.787994, -122.407437, "Wikidata/OpenStreetMap - Union Square San Francisco", "https://www.openstreetmap.org/search?query=Union%20Square%20San%20Francisco", "Fixed civic site reported for the Portola Festival; coordinate is Union Square.",
    315L, 1L, "Union Square", 37.787994, -122.407437, "Wikidata/OpenStreetMap - Union Square San Francisco", "https://www.openstreetmap.org/search?query=Union%20Square%20San%20Francisco", "Fixed fireworks site reported for the Admission Day Festival; coordinate is Union Square.",
    315L, 2L, "Auditorium at Page and Fillmore", 37.772600, -122.430300, "OpenStreetMap geocode - Page Street and Fillmore Street, San Francisco", "https://www.openstreetmap.org/search?query=Page%20Street%20and%20Fillmore%20Street%20San%20Francisco", "Fixed auditorium location from contemporary reports; coordinate approximates the Page and Fillmore intersection.",
    350L, 1L, "Fort McHenry", 39.263300, -76.579400, "Wikidata/OpenStreetMap - Fort McHenry", "https://www.openstreetmap.org/search?query=Fort%20McHenry%20Baltimore", "Fixed official ceremony site in the Star-Spangled Banner Centennial programme.",
    350L, 2L, "Druid Hill Park", 39.318500, -76.638300, "Wikidata/OpenStreetMap - Druid Hill Park", "https://www.openstreetmap.org/search?query=Druid%20Hill%20Park%20Baltimore", "Fixed public celebration site in the Star-Spangled Banner Centennial programme.",
    350L, 3L, "Peabody Art Galleries / Peabody Institute", 39.297100, -76.615700, "Wikidata/OpenStreetMap - Peabody Institute", "https://www.openstreetmap.org/search?query=Peabody%20Institute%20Baltimore", "Fixed gallery site in the Star-Spangled Banner Centennial programme; coordinate uses Peabody Institute.",
    350L, 4L, "Ferry Bar waterfront", 39.254700, -76.622700, "OpenStreetMap geocode - Ferry Bar Park Baltimore", "https://www.openstreetmap.org/search?query=Ferry%20Bar%20Baltimore", "Fixed waterfront site in the Star-Spangled Banner Centennial programme; coordinate approximates the Ferry Bar waterfront.",
    381L, 1L, "Civic Center Plaza / San Francisco City Hall", 37.779300, -122.419300, "Wikidata/OpenStreetMap - San Francisco City Hall", "https://www.openstreetmap.org/search?query=San%20Francisco%20City%20Hall", "Fixed decorative centerpiece site from OpenSFHistory; coordinate is Civic Center/City Hall.",
    381L, 2L, "Civic Auditorium", 37.778457, -122.417369, "Wikidata/OpenStreetMap - Bill Graham Civic Auditorium", "https://www.openstreetmap.org/search?query=Bill%20Graham%20Civic%20Auditorium", "Fixed ball/concert site from OpenSFHistory; coordinate is the Civic Auditorium.",
    381L, 3L, "Embarcadero Joy Zone", 37.795500, -122.393700, "OpenStreetMap geocode - San Francisco Ferry Building/Embarcadero", "https://www.openstreetmap.org/search?query=San%20Francisco%20Embarcadero%20Ferry%20Building", "Fixed waterfront amusement zone from OpenSFHistory; coordinate approximates the Embarcadero/Ferry Building area.",
    381L, 4L, "Golden Gate Park", 37.769400, -122.486200, "Wikidata/OpenStreetMap - Golden Gate Park", "https://www.openstreetmap.org/search?query=Golden%20Gate%20Park", "Fixed concert site from OpenSFHistory; coordinate is the park centroid.",
    381L, 5L, "Union Square", 37.787994, -122.407437, "Wikidata/OpenStreetMap - Union Square San Francisco", "https://www.openstreetmap.org/search?query=Union%20Square%20San%20Francisco", "Fixed concert site from OpenSFHistory; coordinate is Union Square.",
    381L, 6L, "Tanforan Racetrack", 37.636300, -122.417500, "Wikidata/OpenStreetMap - The Shops at Tanforan / former Tanforan Racetrack", "https://www.openstreetmap.org/search?query=Tanforan%20Racetrack%20San%20Bruno", "Fixed auto racing site from OpenSFHistory; coordinate approximates the former Tanforan racetrack site.",
    381L, 7L, "Fairmont Hotel", 37.792400, -122.410000, "Wikidata/OpenStreetMap - Fairmont San Francisco", "https://www.openstreetmap.org/search?query=Fairmont%20Hotel%20San%20Francisco", "Fixed banquet site from OpenSFHistory; coordinate is the Fairmont Hotel.",
    381L, 8L, "Fleishhacker Pool", 37.733200, -122.503200, "Wikidata/OpenStreetMap - Fleishhacker Pool", "https://www.openstreetmap.org/search?query=Fleishhacker%20Pool%20San%20Francisco", "Fixed swimming competition site from OpenSFHistory; coordinate approximates the historic pool site."
  )
}

expand_multisite_venues <- function(combined) {
  multisite_rows <- make_multisite_venue_rows()
  multisite_parents <- unique(multisite_rows$parent_fair_id)
  parent_rows <- combined %>%
    filter(fair_id %in% multisite_parents)

  missing_parents <- setdiff(multisite_parents, parent_rows$fair_id)
  if (length(missing_parents) > 0L) {
    stop("Missing multi-site parent fair_id values: ", paste(missing_parents, collapse = ", "))
  }

  parent_counts <- multisite_rows %>%
    count(parent_fair_id, name = "n_venues_for_fair")

  expanded <- parent_rows %>%
    select(-any_of(venue_level_cols)) %>%
    rename(parent_fair_id = fair_id) %>%
    select(-all_of(venue_update_cols)) %>%
    left_join(multisite_rows, by = "parent_fair_id") %>%
    left_join(parent_counts, by = "parent_fair_id") %>%
    mutate(
      fair_id = parent_fair_id * 100L + venue_seq,
      fair_event_weight = 1 / n_venues_for_fair,
      venue_row_type = "multisite_fixed_venue",
      venue_source_title = case_when(
        parent_fair_id == 305L ~ "Official program, Hudson-Fulton Celebration; New York Heritage; Metropolitan Museum references",
        parent_fair_id == 307L ~ "Portola Festival - FoundSF; The Portola Festival October 19-23, 1909",
        parent_fair_id == 315L ~ "Sterling Furniture - Carnival Glass Worldwide; San Francisco Story/OpenSFHistory",
        parent_fair_id == 350L ~ "National Star-Spangled Banner Centennial official programme; Maryland State Archives",
        parent_fair_id == 381L ~ "Diamond Jubilee: A Closer Look - OpenSFHistory",
        TRUE ~ NA_character_
      ),
      venue_source_url = case_when(
        parent_fair_id == 305L ~ "https://www.nypl.org/research/research-catalog/bib/pb9923179383506421",
        parent_fair_id == 307L ~ "https://www.foundsf.org/Portola_Festival",
        parent_fair_id == 315L ~ "https://www.carnivalglassworldwide.com/sterling-furniture.html",
        parent_fair_id == 350L ~ "https://archive.org/download/nationalstarspan02nati/nationalstarspan02nati.pdf",
        parent_fair_id == 381L ~ "https://www.opensfhistory.org/osfhcrucible/2020/09/13/diamond-jubilee-a-closer-look/",
        TRUE ~ NA_character_
      ),
      venue_note = paste0(
        "One fixed venue extracted from a multi-site event; parent_fair_id=",
        parent_fair_id,
        ". Route-only and river/pageant-only sites are not represented as venue rows."
      ),
      venue_search_status = "found"
    ) %>%
    select(all_of(final_cols))

  single_site <- combined %>%
    filter(!fair_id %in% multisite_parents) %>%
    mutate(
      parent_fair_id = fair_id,
      venue_seq = 1L,
      n_venues_for_fair = 1L,
      fair_event_weight = 1,
      venue_row_type = case_when(
        str_detect(coalesce(venue, ""), regex("^Multiple\\b", ignore_case = TRUE)) ~ "unresolved_multisite",
        TRUE ~ "single_site"
      )
    ) %>%
    select(all_of(final_cols))

  bind_rows(single_site, expanded) %>%
    arrange(year_start, Country, City, Fair_name, parent_fair_id, venue_seq)
}

old_raw <- read_csv(
  old_enriched_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A", "na")
)

old <- old_raw %>%
  mutate(
    source_period = "1790_1910",
    source_row_id = as.character(row_id),
    scrape_row_id = NA_character_,
    year_start = parse_first_year(Year),
    Country = NA_character_,
    Fair_observation = NA_character_
  ) %>%
  ensure_columns(final_cols_without_host) %>%
  select(all_of(final_cols_without_host)) %>%
  mutate(across(everything(), as.character))

new <- new_enriched %>%
  mutate(
    source_period = "1911_1960",
    source_row_id = as.character(row_id),
    scrape_row_id = as.character(scrape_row_id),
    source_status = NA_character_,
    venue_search_status = NA_character_
  ) %>%
  ensure_columns(final_cols_without_host) %>%
  select(all_of(final_cols_without_host)) %>%
  mutate(across(everything(), as.character))

if (nrow(old) != 324L) {
  stop("Expected 324 rows in 1790-1910 input; found ", nrow(old))
}
if (nrow(new) != 145L) {
  stop("Expected 145 rows in 1911-1960 input; found ", nrow(new))
}

combined_pre_geocode <- bind_rows(old, new) %>%
  mutate(
    visits = as_integer_checked(visits, "visits"),
    venue_latitude = as_numeric_checked(venue_latitude, "venue_latitude"),
    venue_longitude = as_numeric_checked(venue_longitude, "venue_longitude"),
    year_start = as.integer(year_start),
    source_tier = str_replace(source_tier, "^tier(\\d+)$", "tier_\\1"),
    across(where(is.character), ~na_if(str_squish(.x), ""))
  ) %>%
  make_fair_key()

geocode_match_audit <- combined_pre_geocode %>%
  select(source_period, source_row_id, Year, City, Fair_name, key_year, key_city, key_fair) %>%
  left_join(
    geocoded %>% select(key_year, key_city, key_fair, host_lat, host_lon),
    by = c("key_year", "key_city", "key_fair")
  )

missing_geocodes <- geocode_match_audit %>%
  filter(is.na(host_lat) | is.na(host_lon))
if (nrow(missing_geocodes) > 0L) {
  stop(
    "Some consolidated fairs lack host geocodes:\n",
    paste(capture.output(print(missing_geocodes, n = Inf)), collapse = "\n")
  )
}

combined_all <- combined_pre_geocode %>%
  left_join(geocoded, by = c("key_year", "key_city", "key_fair")) %>%
  mutate(
    host_geocode_override = FALSE,
    # Reviewed fixes for known homonym errors in the stored host geocodes.
    host_geocode_override = if_else(
      (City == "Dublin" & year_start %in% c(1865L, 1874L, 1907L)) |
        (City == "Cork" & year_start %in% c(1883L, 1902L)) |
        (City == "Newcastle" & year_start == 1887L),
      TRUE,
      host_geocode_override
    ),
    host_lat = case_when(
      City == "Dublin" & year_start %in% c(1865L, 1874L, 1907L) ~ 53.3498,
      City == "Cork" & year_start %in% c(1883L, 1902L) ~ 51.8985,
      City == "Newcastle" & year_start == 1887L ~ 54.9783,
      TRUE ~ host_lat
    ),
    host_lon = case_when(
      City == "Dublin" & year_start %in% c(1865L, 1874L, 1907L) ~ -6.2603,
      City == "Cork" & year_start %in% c(1883L, 1902L) ~ -8.4756,
      City == "Newcastle" & year_start == 1887L ~ -1.6178,
      TRUE ~ host_lon
    ),
    host_matched_name = case_when(
      City == "Dublin" & year_start %in% c(1865L, 1874L, 1907L) ~ "Dublin",
      City == "Cork" & year_start %in% c(1883L, 1902L) ~ "Cork",
      City == "Newcastle" & year_start == 1887L ~ "Newcastle upon Tyne",
      TRUE ~ host_matched_name
    ),
    host_matched_country_iso2 = case_when(
      City == "Dublin" & year_start %in% c(1865L, 1874L, 1907L) ~ "IE",
      City == "Cork" & year_start %in% c(1883L, 1902L) ~ "IE",
      City == "Newcastle" & year_start == 1887L ~ "GB",
      TRUE ~ host_matched_country_iso2
    ),
    host_matched_country_iso3 = case_when(
      City == "Dublin" & year_start %in% c(1865L, 1874L, 1907L) ~ "IRL",
      City == "Cork" & year_start %in% c(1883L, 1902L) ~ "IRL",
      City == "Newcastle" & year_start == 1887L ~ "GBR",
      TRUE ~ host_matched_country_iso3
    ),
    host_admin1_name = case_when(
      City == "Dublin" & year_start %in% c(1865L, 1874L, 1907L) ~ "Leinster",
      City == "Cork" & year_start %in% c(1883L, 1902L) ~ "Munster",
      City == "Newcastle" & year_start == 1887L ~ "England",
      TRUE ~ host_admin1_name
    )
  ) %>%
  mutate(
    non_realized_exclusion_reason = reviewed_non_realized_reason(
      source_period,
      source_row_id
    )
  ) %>%
  select(all_of(setdiff(final_cols_base, "fair_id")), non_realized_exclusion_reason)

excluded_non_realized <- combined_all %>%
  filter(!is.na(non_realized_exclusion_reason)) %>%
  transmute(
    source_period,
    source_row_id,
    scrape_row_id,
    Year,
    year_start,
    City,
    Country,
    Fair_name,
    Fair_observation,
    source_note,
    venue,
    venue_note,
    exclusion_reason = non_realized_exclusion_reason
  ) %>%
  arrange(year_start, City, Fair_name)

combined <- combined_all %>%
  filter(is.na(non_realized_exclusion_reason)) %>%
  select(-non_realized_exclusion_reason) %>%
  arrange(year_start, Country, City, Fair_name, source_period, source_row_id) %>%
  mutate(fair_id = row_number()) %>%
  select(all_of(final_cols_base)) %>%
  apply_venue_research_updates() %>%
  expand_multisite_venues()

###############################################################################
# Validation and writes
###############################################################################

cat("Validating and writing outputs...\n")

if (nrow(excluded_non_realized) != 9L) {
  stop("Expected 9 non-realized exclusions; found ", nrow(excluded_non_realized))
}
if (nrow(combined) != 477L) {
  stop("Expected 477 venue-level rows after multi-site expansion; found ", nrow(combined))
}
if (anyDuplicated(combined$fair_id) > 0L) {
  stop("fair_id is not unique.")
}
if (any(is.na(combined$parent_fair_id))) {
  stop("Combined file contains missing parent_fair_id.")
}
if (any(is.na(combined$venue_seq))) {
  stop("Combined file contains missing venue_seq.")
}
if (any(is.na(combined$n_venues_for_fair))) {
  stop("Combined file contains missing n_venues_for_fair.")
}
if (any(is.na(combined$fair_event_weight))) {
  stop("Combined file contains missing fair_event_weight.")
}
bad_weight_sums <- combined %>%
  distinct(parent_fair_id, fair_id, .keep_all = TRUE) %>%
  group_by(parent_fair_id) %>%
  summarise(weight_sum = sum(fair_event_weight, na.rm = TRUE), .groups = "drop") %>%
  filter(abs(weight_sum - 1) > 1e-8)
if (nrow(bad_weight_sums) > 0L) {
  stop("Some parent_fair_id weights do not sum to 1:\n", paste(capture.output(print(bad_weight_sums)), collapse = "\n"))
}
if (any(is.na(combined$year_start))) {
  stop("Combined file contains missing year_start.")
}
if (any(is.na(combined$host_lat) | is.na(combined$host_lon))) {
  stop("Combined file contains missing host coordinates.")
}
if (any(
  combined$source_period == "1790_1910" &
    combined$source_row_id %in% c("84", "141", "208", "233", "274", "276")
)) {
  stop("A reviewed pre-1911 never-held fair remains in the final file.")
}
if (any(
  combined$source_period == "1911_1960" &
    combined$source_row_id %in% c("365", "416", "431")
)) {
  stop("A reviewed post-1911 non-realized fair remains in the final file.")
}

write_csv(combined, out_csv, na = "")
write_csv(excluded_non_realized, out_excluded_non_realized, na = "")

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(combined, out_xlsx)
} else {
  warning("Package 'writexl' is not installed; skipped XLSX export.")
}

summary_lines <- c(
  paste0("Scrape input: ", scrape_file),
  paste0("Host geocode input: ", geocoded_file),
  paste0("1790-1910 enrichment input: ", old_enriched_file),
  paste0("1911-1960 research batch inputs: ", paste(basename(batch_paths), collapse = ", ")),
  paste0("Output CSV: ", out_csv),
  paste0("Output XLSX: ", if (file.exists(out_xlsx)) out_xlsx else "not written"),
  paste0("Excluded non-realized audit CSV: ", out_excluded_non_realized),
  paste0("Venue-level rows: ", nrow(combined)),
  paste0("Distinct fairs: ", n_distinct(combined$parent_fair_id)),
  paste0("Columns: ", ncol(combined)),
  paste0("Excluded non-realized rows: ", nrow(excluded_non_realized)),
  paste0("Year range: ", min(combined$year_start), "-", max(combined$year_start)),
  "",
  "Rows by source_period:",
  capture.output(print(table(combined$source_period, useNA = "ifany"))),
  "",
  "Rows by venue_row_type:",
  capture.output(print(table(combined$venue_row_type, useNA = "ifany"))),
  "",
  paste0("Rows with host coordinates: ", sum(!is.na(combined$host_lat) & !is.na(combined$host_lon))),
  paste0("Rows with reviewed host geocode overrides: ", sum(combined$host_geocode_override)),
  paste0("Rows with visits: ", sum(!is.na(combined$visits))),
  paste0("Distinct fairs with visits: ", n_distinct(combined$parent_fair_id[!is.na(combined$visits)])),
  paste0("Rows with venue: ", sum(!is.na(combined$venue))),
  paste0(
    "Rows with venue coordinates: ",
    sum(!is.na(combined$venue_latitude) & !is.na(combined$venue_longitude))
  ),
  paste0(
    "Distinct fairs with venue coordinates: ",
    n_distinct(combined$parent_fair_id[!is.na(combined$venue_latitude) & !is.na(combined$venue_longitude)])
  ),
  "",
  "search_status counts:",
  capture.output(print(table(combined$search_status, useNA = "ifany"))),
  "",
  "venue_search_status counts:",
  capture.output(print(table(combined$venue_search_status, useNA = "ifany"))),
  "",
  "confidence counts:",
  capture.output(print(table(combined$confidence, useNA = "ifany")))
)

writeLines(summary_lines, out_summary)
message(paste(summary_lines, collapse = "\n"))
