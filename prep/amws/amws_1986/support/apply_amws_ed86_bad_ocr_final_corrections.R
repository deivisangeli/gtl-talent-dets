###############################################################################
# Apply the adjudicated AMWS Ed86 bad-OCR birth-location corrections only.
#
# This deliberately does not run the full all-country corrections pipeline. It
# writes and validates an audit candidate first. Set
# AMWS_ED86_BAD_OCR_COMMIT=TRUE to back up and replace the processed CSV.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(stringdist)
  library(stringr)
  library(tibble)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..",
                                       ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

local_dropbox <- file.path("C:/Users", Sys.info()[["user"]],
                           "Globtalent Dropbox", "gtl_talent_dets")
if (!dir.exists(TALENT_DETS_DATA_DIR) && dir.exists(local_dropbox)) {
  TALENT_DETS_DATA_DIR <- normalizePath(local_dropbox, winslash = "/")
  DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
}

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_flag <- function(name, default = FALSE) {
  value <- tolower(env_chr(name, ifelse(default, "true", "false")))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be a Boolean flag; received: ", value)
  }
  value %in% c("true", "1", "yes")
}

env_int <- function(name, default) {
  value <- env_chr(name, as.character(default))
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed)) stop(name, " must be an integer; received: ", value)
  parsed
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | str_to_upper(str_trim(x)) == "NA", "", x)
}

normalize_text <- function(x) {
  blank_na(x) |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

row_key <- function(data) {
  paste(data$doc_id, data$lineid, data$entry_instance, sep = "\r")
}

strip_punct <- function(x) {
  x <- gsub("\\.", "", x)
  x <- gsub("[[:punct:]]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

expand_abbrev <- function(x) {
  x <- gsub("\\bSt\\b\\.?", "Saint", x, ignore.case = TRUE)
  x <- gsub("\\bSte\\b\\.?", "Sainte", x, ignore.case = TRUE)
  x <- gsub("\\bMt\\b\\.?", "Mount", x, ignore.case = TRUE)
  gsub("\\bFt\\b\\.?", "Fort", x, ignore.case = TRUE)
}

strip_suffix <- function(x) {
  x <- gsub(
    "\\b(city|town|township|village|borough|cdp|zona urbana|comunidad|municipio)\\b",
    "", x,
            ignore.case = TRUE)
  trimws(gsub("\\s+", " ", x))
}

norm_place_key <- function(x) {
  tolower(trimws(strip_punct(strip_suffix(expand_abbrev(x)))))
}

state_to_fp <- c(
  AL = "01", AK = "02", AZ = "04", AR = "05", CA = "06", CO = "08",
  CT = "09", DE = "10", DC = "11", FL = "12", GA = "13", HI = "15",
  ID = "16", IL = "17", IN = "18", IA = "19", KS = "20", KY = "21",
  LA = "22", ME = "23", MD = "24", MA = "25", MI = "26", MN = "27",
  MS = "28", MO = "29", MT = "30", NE = "31", NV = "32", NH = "33",
  NJ = "34", NM = "35", NY = "36", NC = "37", ND = "38", OH = "39",
  OK = "40", OR = "41", PA = "42", RI = "44", SC = "45", SD = "46",
  TN = "47", TX = "48", UT = "49", VT = "50", VA = "51", WA = "53",
  WV = "54", WI = "55", WY = "56", PR = "72"
)

geocode_us_rows <- function(input, gaz_file, geon_file, county_shp) {
  gaz <- read_tsv(gaz_file, show_col_types = FALSE,
                  locale = locale(encoding = "UTF-8")) |>
    rename_with(trimws) |>
    transmute(state = USPS, name = NAME, lat = as.numeric(INTPTLAT),
              lon = as.numeric(INTPTLONG), key = norm_place_key(name))

  geon_cols <- c("geonameid", "name", "asciiname", "alternatenames",
                 "latitude", "longitude", "feature_class", "feature_code",
                 "country", "cc2", "admin1", "admin2", "admin3", "admin4",
                 "population", "elevation", "dem", "timezone",
                 "modification_date")
  geon <- read_tsv(geon_file, col_names = geon_cols, show_col_types = FALSE,
                   quote = "", locale = locale(encoding = "UTF-8")) |>
    filter(feature_class %in% c("P", "A")) |>
    transmute(state = admin1, name = asciiname, lat = as.numeric(latitude),
              lon = as.numeric(longitude), admin2_fips = admin2,
              population = suppressWarnings(as.integer(population)),
              key = norm_place_key(asciiname))

  pairs <- input |>
    distinct(city = birth_city, state = birth_state) |>
    mutate(key = norm_place_key(city))

  exact_gaz <- pairs |>
    inner_join(gaz |> select(key, state, lat, lon, matched_name = name),
               by = c("key", "state"), relationship = "many-to-many") |>
    group_by(city, state) |>
    slice(1L) |>
    ungroup() |>
    mutate(match_source = "gazetteer_exact", admin2_fips = "", jw = 0)

  remaining <- pairs |> anti_join(exact_gaz, by = c("city", "state"))
  exact_geon <- remaining |>
    inner_join(geon |> select(key, state, lat, lon, admin2_fips,
                              matched_name = name, population),
               by = c("key", "state"), relationship = "many-to-many") |>
    arrange(city, state, desc(population)) |>
    group_by(city, state) |>
    slice(1L) |>
    ungroup() |>
    mutate(match_source = "geonames_exact", jw = 0)

  remaining <- remaining |> anti_join(exact_geon, by = c("city", "state"))
  fuzzy_one <- function(key_in, state_in) {
    candidates <- bind_rows(
      gaz |> filter(state == state_in) |>
        transmute(key, matched_name = name, lat, lon,
                  match_source = "gazetteer_fuzzy", admin2_fips = "",
                  population = NA_integer_),
      geon |> filter(state == state_in) |>
        transmute(key, matched_name = name, lat, lon,
                  match_source = "geonames_fuzzy", admin2_fips, population)
    )
    if (!nrow(candidates)) return(NULL)
    distance <- stringdist(candidates$key, key_in, method = "jw", p = 0.1)
    i <- which.min(distance)
    if (!length(i) || distance[[i]] > 0.10) return(NULL)
    candidates[i, ] |> mutate(jw = distance[[i]])
  }

  fuzzy_rows <- vector("list", nrow(remaining))
  if (nrow(remaining)) {
    for (i in seq_len(nrow(remaining))) {
      hit <- fuzzy_one(remaining$key[[i]], remaining$state[[i]])
      if (!is.null(hit)) {
        fuzzy_rows[[i]] <- bind_cols(remaining[i, ], hit |> select(-key))
      }
    }
  }
  fuzzy <- bind_rows(fuzzy_rows)

  matched <- bind_rows(
    exact_gaz |> select(city, state, lat, lon, matched_name, match_source,
                        admin2_fips, jw),
    exact_geon |> select(city, state, lat, lon, matched_name, match_source,
                         admin2_fips, jw),
    if (nrow(fuzzy)) {
      fuzzy |> select(city, state, lat, lon, matched_name, match_source,
                      admin2_fips, jw)
    } else tibble()
  ) |>
    filter(!is.na(lat), !is.na(lon))

  counties <- st_read(county_shp, quiet = TRUE) |> st_transform(4326)
  points <- st_as_sf(matched, coords = c("lon", "lat"), crs = 4326,
                     remove = FALSE)
  spatial <- st_join(points, counties |> select(STATEFP, COUNTYFP, GEOID, NAME),
                     left = TRUE)
  matched$geoid <- blank_na(spatial$GEOID)
  matched$county_name <- blank_na(spatial$NAME)

  missing_geo <- which(!nzchar(matched$geoid))
  if (length(missing_geo)) {
    by_state <- split(missing_geo, matched$state[missing_geo])
    for (state in names(by_state)) {
      state_fp <- state_to_fp[[state]]
      if (is.null(state_fp) || is.na(state_fp)) next
      county_state <- counties[counties$STATEFP == state_fp, ]
      if (!nrow(county_state)) next
      nearest <- st_nearest_feature(points[by_state[[state]], ], county_state)
      matched$geoid[by_state[[state]]] <- as.character(county_state$GEOID[nearest])
      matched$county_name[by_state[[state]]] <-
        as.character(county_state$NAME[nearest])
    }
  }

  needs_fips <- !nzchar(matched$geoid) & nzchar(blank_na(matched$admin2_fips))
  county_lookup <- counties |>
    st_drop_geometry() |>
    transmute(state_fp = STATEFP, county_fp = COUNTYFP,
              geoid_fb = GEOID, county_fb = NAME)
  for (i in which(needs_fips)) {
    state_fp <- state_to_fp[[matched$state[[i]]]]
    if (is.null(state_fp) || is.na(state_fp)) next
    hit <- county_lookup |>
      filter(state_fp == !!state_fp, county_fp == matched$admin2_fips[[i]])
    if (nrow(hit) == 1L) {
      matched$geoid[[i]] <- hit$geoid_fb[[1]]
      matched$county_name[[i]] <- hit$county_fb[[1]]
    }
  }

  overrides <- tribble(
    ~city,           ~state, ~geoid, ~county_name,
    "New York",      "NY",   "36061", "New York",
    "Manhattan",     "NY",   "36061", "New York",
    "Bronx",         "NY",   "36005", "Bronx",
    "Brooklyn",      "NY",   "36047", "Kings",
    "Queens",        "NY",   "36081", "Queens",
    "Staten Island", "NY",   "36085", "Richmond",
    "Jersey City",   "NJ",   "34017", "Hudson",
    "Hoboken",       "NJ",   "34017", "Hudson",
    "San Francisco", "CA",   "06075", "San Francisco"
  )
  for (i in seq_len(nrow(overrides))) {
    hit <- matched$city == overrides$city[[i]] &
      matched$state == overrides$state[[i]]
    matched$geoid[hit] <- overrides$geoid[[i]]
    matched$county_name[hit] <- overrides$county_name[[i]]
  }

  matched |>
    mutate(geoid = ifelse(nzchar(geoid), str_pad(geoid, 5, pad = "0"), "")) |>
    select(birth_city = city, birth_state = state, lat, lon, geoid,
           county_name, matched_name, match_source, jw)
}

data_dir <- file.path(TALENT_DETS_DATA_DIR, "Data")
rollout_dir <- env_chr(
  "AMWS_ED86_BAD_OCR_ROLLOUT_DIR",
  file.path(data_dir, "intermediary", "amws",
            "manual_bad_ocr_birth_city_full_rollout_20260710")
)
audit_dir <- env_chr(
  "AMWS_ED86_BAD_OCR_APPLY_DIR",
  file.path(rollout_dir, "apply_20260711")
)
canonical_csv <- env_chr(
  "AMWS_ED86_BAD_OCR_CANONICAL_CSV",
  file.path(data_dir, "processed", "amws", "amws_ed86.csv")
)
corrections_csv <- env_chr(
  "AMWS_ED86_BAD_OCR_CORRECTIONS_CSV",
  file.path(rollout_dir, "amws_ed86_bad_ocr_all_corrections.csv")
)
candidate_csv <- file.path(audit_dir, "amws_ed86_bad_ocr_corrected_candidate.csv")
backup_csv <- file.path(audit_dir, "amws_ed86_before_bad_ocr_corrections_20260711.csv")
log_csv <- file.path(audit_dir, "amws_ed86_bad_ocr_application_log.csv")
summary_csv <- file.path(audit_dir, "amws_ed86_bad_ocr_application_summary.csv")
commit <- env_flag("AMWS_ED86_BAD_OCR_COMMIT", FALSE)
expected_corrections_n <- env_int(
  "AMWS_ED86_BAD_OCR_EXPECTED_CORRECTIONS_N", 1342L
)
expected_to_apply_n <- env_int("AMWS_ED86_BAD_OCR_EXPECTED_TO_APPLY_N", 947L)
expected_unclear_n <- env_int("AMWS_ED86_BAD_OCR_EXPECTED_UNCLEAR_N", 394L)
expected_no_change_n <- env_int("AMWS_ED86_BAD_OCR_EXPECTED_NO_CHANGE_N", 1L)
expected_regeo_n <- env_int("AMWS_ED86_BAD_OCR_EXPECTED_REGEO_N", 780L)

gaz_file <- file.path(DATA_INPUT, "2024_Gaz_place_national.txt")
geon_file <- file.path(DATA_INPUT, "geonames_US.txt")
county_shp <- file.path(Sys.getenv("LOCALAPPDATA"), "tigris", "tigris",
                        "Cache", "cb_2020_us_county_20m.shp")
required_files <- c(canonical_csv, corrections_csv, gaz_file, geon_file,
                    county_shp)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing required files: ", paste(missing_files, collapse = ", "))
}

csv_types <- cols(.default = col_character())
source_hash <- unname(tools::md5sum(canonical_csv))
source <- read_csv(canonical_csv, col_types = csv_types,
                   show_col_types = FALSE, progress = FALSE) |>
  mutate(across(everything(), blank_na))
corrections <- read_csv(corrections_csv, col_types = csv_types,
                        show_col_types = FALSE, progress = FALSE) |>
  mutate(across(everything(), blank_na))

source_cols <- c(
  "doc_id", "lineid", "entry_instance", "birth_city", "birth_state",
  "birth_country", "is_us_birth", "is_us_geocoded", "geo_lat", "geo_lon",
  "geo_geoid", "geo_county_name", "geo_matched_name", "geocoding_status"
)
correction_cols <- c(
  "doc_id", "lineid", "entry_instance", "birth_city_old", "birth_state_old",
  "birth_country_old", "birth_city_new", "birth_state_new",
  "birth_country_new", "manual_action", "manual_confidence"
)
if (length(setdiff(source_cols, names(source)))) {
  stop("Canonical CSV is missing required columns: ",
       paste(setdiff(source_cols, names(source)), collapse = ", "))
}
if (length(setdiff(correction_cols, names(corrections)))) {
  stop("Corrections CSV is missing required columns: ",
       paste(setdiff(correction_cols, names(corrections)), collapse = ", "))
}
if (nrow(source) != 94809L || anyDuplicated(row_key(source))) {
  stop("Canonical row count or key uniqueness does not match expectations.")
}
if ((expected_corrections_n > 0L &&
     nrow(corrections) != expected_corrections_n) ||
    anyDuplicated(row_key(corrections))) {
  stop("Corrections row count or key uniqueness does not match expectations.")
}

to_apply <- corrections |>
  filter(manual_action == "correct",
         manual_confidence %in% c("high", "medium"))
if (expected_to_apply_n > 0L && nrow(to_apply) != expected_to_apply_n) {
  stop("Expected ", expected_to_apply_n,
       " applicable corrections; found ", nrow(to_apply), ".")
}
if ((expected_unclear_n > 0L &&
     sum(corrections$manual_action == "review_unclear") != expected_unclear_n) ||
    (expected_no_change_n > 0L &&
     sum(corrections$manual_action == "no_change") != expected_no_change_n)) {
  stop("The final action distribution does not match the adjudicated rollout.")
}

idx <- match(row_key(to_apply), row_key(source))
if (anyNA(idx) || anyDuplicated(idx)) {
  stop("Applicable corrections do not match canonical keys one-to-one.")
}
old_matches <-
  source$birth_city[idx] == to_apply$birth_city_old &
  source$birth_state[idx] == to_apply$birth_state_old &
  source$birth_country[idx] == to_apply$birth_country_old
if (!all(old_matches)) {
  stop("Canonical old location values differ for ", sum(!old_matches),
       " applicable rows; no output was written.")
}

suspicious_pattern <- "[^\\p{L}\\s.,'’‘()&/\\-]"
if (any(!nzchar(normalize_text(to_apply$birth_city_new))) ||
    any(!nzchar(normalize_text(to_apply$birth_country_new))) ||
    any(str_detect(to_apply$birth_city_new, suspicious_pattern))) {
  stop("Applicable corrected cities/countries are blank or cities retain suspicious characters.")
}
if (any(nzchar(to_apply$birth_state_new) &
        !str_detect(to_apply$birth_state_new, "^[A-Z]{2}$"))) {
  stop("Applicable state/province values are malformed.")
}
us_codes <- names(state_to_fp)
canada_codes <- c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON",
                  "PE", "QC", "SK", "YT")
if (any(to_apply$birth_state_new %in% us_codes &
        to_apply$birth_country_new != "USA") ||
    any(to_apply$birth_state_new %in% canada_codes &
        to_apply$birth_country_new != "Canada")) {
  stop("State/province and country values are inconsistent.")
}
if (any(to_apply$birth_country_new %in% c("USA", "Canada") &
        !nzchar(to_apply$birth_state_new))) {
  stop("Applicable USA/Canada corrections require a state/province code.")
}

candidate <- source
old_location <- source[idx, c("birth_city", "birth_state", "birth_country",
                              "is_us_birth", "is_us_geocoded", "geo_lat",
                              "geo_lon", "geo_geoid", "geo_county_name",
                              "geo_matched_name", "geocoding_status")]
candidate$birth_city[idx] <- to_apply$birth_city_new
candidate$birth_state[idx] <- to_apply$birth_state_new
candidate$birth_country[idx] <- to_apply$birth_country_new

geo_cols <- c("geo_lat", "geo_lon", "geo_geoid", "geo_county_name",
              "geo_matched_name")
for (column in geo_cols) candidate[[column]][idx] <- ""
candidate$is_us_birth[idx] <- ifelse(candidate$birth_country[idx] == "USA",
                                     "TRUE", "FALSE")
candidate$is_us_geocoded[idx] <- "FALSE"
candidate$geocoding_status[idx] <- case_when(
  !nzchar(candidate$birth_country[idx]) ~ "missing_country",
  candidate$birth_country[idx] != "USA" ~ "not_usa",
  !nzchar(candidate$birth_city[idx]) | !nzchar(candidate$birth_state[idx]) ~
    "missing_city_or_state",
  TRUE ~ "no_geocoder_match"
)

regeo_idx <- idx[
  candidate$birth_country[idx] == "USA" &
    nzchar(candidate$birth_city[idx]) & nzchar(candidate$birth_state[idx])
]
if (expected_regeo_n > 0L && length(regeo_idx) != expected_regeo_n) {
  stop("Expected ", expected_regeo_n,
       " corrected US geocoding candidates; found ",
       length(regeo_idx), ".")
}
geo_matches <- geocode_us_rows(candidate[regeo_idx, ], gaz_file, geon_file,
                               county_shp)
geo_lookup <- paste(geo_matches$birth_city, geo_matches$birth_state, sep = "\r")
candidate_lookup <- paste(candidate$birth_city[regeo_idx],
                          candidate$birth_state[regeo_idx], sep = "\r")
geo_idx <- match(candidate_lookup, geo_lookup)
matched <- !is.na(geo_idx) & nzchar(blank_na(geo_matches$geoid[geo_idx]))
matched_rows <- regeo_idx[matched]
matched_geo <- geo_matches[geo_idx[matched], ]
candidate$geo_lat[matched_rows] <- as.character(matched_geo$lat)
candidate$geo_lon[matched_rows] <- as.character(matched_geo$lon)
candidate$geo_geoid[matched_rows] <- matched_geo$geoid
candidate$geo_county_name[matched_rows] <- matched_geo$county_name
candidate$geo_matched_name[matched_rows] <- matched_geo$matched_name
candidate$is_us_geocoded[matched_rows] <- "TRUE"
candidate$geocoding_status[matched_rows] <- "geocoded"

if (nrow(candidate) != nrow(source) || !identical(names(candidate), names(source)) ||
    !identical(row_key(candidate), row_key(source))) {
  stop("Candidate did not preserve row count, schema, or key order.")
}
changed_rows <- which(
  candidate$birth_city != source$birth_city |
    candidate$birth_state != source$birth_state |
    candidate$birth_country != source$birth_country
)
if (!identical(changed_rows, sort(idx))) {
  stop("Candidate location changes are not limited to the 947 approved rows.")
}
if (any(candidate$is_us_geocoded == "TRUE" &
        (candidate$birth_country != "USA" | !nzchar(candidate$geo_geoid)))) {
  stop("Candidate contains inconsistent US-geocoding flags.")
}
if (any(candidate$birth_country == "USA" & candidate$is_us_birth != "TRUE") ||
    any(candidate$birth_country != "USA" & candidate$is_us_birth != "FALSE")) {
  stop("Candidate contains inconsistent US-birth flags.")
}

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
write_excel_csv(candidate, candidate_csv, na = "")
candidate_hash <- unname(tools::md5sum(candidate_csv))

application_log <- bind_cols(
  to_apply |>
    select(doc_id, lineid, entry_instance, name_raw, manual_action,
           manual_confidence, final_source, location_inference_basis,
           location_inference_note, manual_note),
  old_location |> rename_with(~ paste0("old_", .x)),
  candidate[idx, c("birth_city", "birth_state", "birth_country",
                   "is_us_birth", "is_us_geocoded", "geo_lat", "geo_lon",
                   "geo_geoid", "geo_county_name", "geo_matched_name",
                   "geocoding_status")] |>
    rename_with(~ paste0("new_", .x))
)
write_excel_csv(application_log, log_csv, na = "")

committed <- FALSE
backup_hash <- ""
final_hash <- source_hash
if (commit) {
  if (file.exists(backup_csv)) {
    stop("Backup already exists; refusing to overwrite: ", backup_csv)
  }
  if (unname(tools::md5sum(canonical_csv)) != source_hash) {
    stop("Canonical CSV changed during processing; refusing to commit.")
  }
  if (!file.copy(canonical_csv, backup_csv, overwrite = FALSE)) {
    stop("Could not create backup: ", backup_csv)
  }
  backup_hash <- unname(tools::md5sum(backup_csv))
  if (backup_hash != source_hash) stop("Backup hash does not match source hash.")
  if (!file.copy(candidate_csv, canonical_csv, overwrite = TRUE)) {
    stop("Could not replace canonical CSV after backup.")
  }
  final_hash <- unname(tools::md5sum(canonical_csv))
  if (final_hash != candidate_hash) {
    stop("Final canonical hash does not match the validated candidate.")
  }
  final_check <- read_csv(canonical_csv, col_types = csv_types,
                          show_col_types = FALSE, progress = FALSE) |>
    mutate(across(everything(), blank_na))
  if (!identical(final_check, candidate)) {
    stop("Final canonical contents do not match the validated candidate.")
  }
  committed <- TRUE
}

summary <- tribble(
  ~metric, ~value,
  "commit_requested", as.character(commit),
  "committed", as.character(committed),
  "canonical_csv", canonical_csv,
  "corrections_csv", corrections_csv,
  "candidate_csv", candidate_csv,
  "backup_csv", ifelse(committed, backup_csv, ""),
  "application_log_csv", log_csv,
  "source_rows", as.character(nrow(source)),
  "candidate_rows", as.character(nrow(candidate)),
  "reviewed_rows", as.character(nrow(corrections)),
  "applied_rows", as.character(length(idx)),
  "review_unclear_untouched", "394",
  "no_change_untouched", "1",
  "birth_city_changed", as.character(sum(candidate$birth_city[idx] !=
                                            source$birth_city[idx])),
  "birth_state_changed", as.character(sum(candidate$birth_state[idx] !=
                                             source$birth_state[idx])),
  "birth_country_changed", as.character(sum(candidate$birth_country[idx] !=
                                               source$birth_country[idx])),
  "us_regeocode_candidates", as.character(length(regeo_idx)),
  "us_regeocoded", as.character(length(matched_rows)),
  "us_no_geocoder_match", as.character(length(regeo_idx) - length(matched_rows)),
  "source_md5", source_hash,
  "backup_md5", backup_hash,
  "candidate_md5", candidate_hash,
  "final_md5", final_hash
)
write_excel_csv(summary, summary_csv, na = "")

cat("Validated candidate:", candidate_csv, "\n")
cat("Applied rows:", length(idx), "\n")
cat("US re-geocoded:", length(matched_rows), "of", length(regeo_idx), "\n")
cat("Committed:", committed, "\n")
cat("Summary:", summary_csv, "\n")
