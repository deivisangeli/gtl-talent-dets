###############################################################################
# Geocode AMWS Ed16 expanded corrected US birthplaces to 2020 county GEOIDs.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv
#
# Writes:
#   output/amws/regex_all_docs/
#     amws_ed16_us_geocoded.csv
#     amws_ed16_us_unmatched.csv
#     amws_ed16_us_geocoded_suspects.csv
#     amws_ed16_us_geocoded_qc100.csv
#     amws_ed16_us_geocode_summary.csv
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringdist)
  library(sf)
  library(tibble)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."),
                             winslash = "/", mustWork = TRUE)
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
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
}

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x), "", x)
}

normalize_text <- function(x) {
  blank_na(x) |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

as_bool <- function(x) {
  x <- tolower(normalize_text(x))
  x %in% c("true", "t", "1", "yes")
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
  x <- gsub("\\bFt\\b\\.?", "Fort", x, ignore.case = TRUE)
  x
}

strip_suffix <- function(x) {
  x <- gsub("\\b(city|town|township|village|borough|cdp)\\b", "", x,
            ignore.case = TRUE)
  trimws(gsub("\\s+", " ", x))
}

norm_place_key <- function(x) {
  x <- expand_abbrev(x)
  x <- strip_suffix(x)
  x <- strip_punct(x)
  tolower(trimws(x))
}

norm_for_check <- function(s) {
  s <- tolower(ifelse(is.na(s), "", s))
  s <- gsub("[^a-z ]", " ", s)
  trimws(gsub("\\s+", " ", s))
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

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_GEOCODE_OUTPUT_DIR", default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input_file <- env_chr(
  "AMWS_ED16_GEOCODE_INPUT_FILE",
  file.path(output_dir,
            "amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv")
)
input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

gaz_file <- file.path(DATA_INPUT, "2024_Gaz_place_national.txt")
geon_file <- file.path(DATA_INPUT, "geonames_US.txt")
county_shp <- file.path(Sys.getenv("LOCALAPPDATA"), "tigris", "tigris",
                        "Cache", "cb_2020_us_county_20m.shp")
stopifnot(file.exists(gaz_file), file.exists(geon_file), file.exists(county_shp))

geocoded_csv <- file.path(output_dir, "amws_ed16_us_geocoded.csv")
unmatched_csv <- file.path(output_dir, "amws_ed16_us_unmatched.csv")
suspects_csv <- file.path(output_dir, "amws_ed16_us_geocoded_suspects.csv")
qc_csv <- file.path(output_dir, "amws_ed16_us_geocoded_qc100.csv")
summary_csv <- file.path(output_dir, "amws_ed16_us_geocode_summary.csv")

csv_text_cols <- cols(.default = col_character())
input <- read_csv(input_file, col_types = csv_text_cols,
                  show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c(
  "doc_id", "lineid", "source_lineid", "entry_instance",
  "raw_text_adjusted", "birth_place", "birth_date", "birth_year",
  "birth_city", "birth_state", "birth_country",
  "birth_location_format_problem", "field"
)
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input missing required columns: ", paste(missing_cols, collapse = ", "))
}

if (n_distinct(paste(input$doc_id, input$lineid)) != nrow(input)) {
  stop("Input has duplicated doc_id + lineid.")
}

input <- input |>
  mutate(
    birth_location_format_problem_bool =
      as_bool(birth_location_format_problem),
    geocode_candidate = birth_country == "USA" &
      nzchar(normalize_text(birth_city)) &
      nzchar(normalize_text(birth_state)) &
      !birth_location_format_problem_bool,
    unmatched_reason = case_when(
      birth_country != "USA" ~ "not_usa",
      birth_location_format_problem_bool ~ "format_problem",
      !nzchar(normalize_text(birth_city)) |
        !nzchar(normalize_text(birth_state)) ~ "missing_city_or_state",
      TRUE ~ ""
    )
  )

us_all <- input |> filter(birth_country == "USA")
eligible <- input |>
  filter(geocode_candidate) |>
  transmute(
    doc_id, lineid, source_lineid, entry_instance,
    raw_text_adjusted,
    raw_text = if ("raw_text" %in% names(input)) raw_text else "",
    birth_place, birth_date, birth_year,
    birth_city, birth_state, birth_country, field,
    birth_location_format_problem,
    birth_location_format_problem_reason =
      if ("birth_location_format_problem_reason" %in% names(input)) {
        birth_location_format_problem_reason
      } else {
        ""
      },
    city = birth_city,
    state = birth_state
  )

cat("Input rows:", nrow(input), "\n")
cat("USA rows:", nrow(us_all), "\n")
cat("Eligible USA rows:", nrow(eligible), "\n")

gaz <- read_tsv(gaz_file, show_col_types = FALSE,
                locale = locale(encoding = "UTF-8")) |>
  rename_with(~ trimws(.x)) |>
  transmute(
    state = USPS,
    geoid_place = GEOID,
    name = NAME,
    lat = INTPTLAT,
    lon = INTPTLONG
  ) |>
  mutate(key = norm_place_key(name))

geon_cols <- c("geonameid", "name", "asciiname", "alternatenames", "latitude",
               "longitude", "feature_class", "feature_code", "country", "cc2",
               "admin1", "admin2", "admin3", "admin4", "population",
               "elevation", "dem", "timezone", "modification_date")
geon <- read_tsv(geon_file, col_names = geon_cols, show_col_types = FALSE,
                 quote = "", locale = locale(encoding = "UTF-8")) |>
  filter(feature_class %in% c("P", "A")) |>
  transmute(
    state = admin1,
    name = asciiname,
    lat = latitude,
    lon = longitude,
    admin2_fips = admin2,
    population = as.integer(population),
    key = norm_place_key(asciiname)
  )

pairs <- eligible |>
  distinct(city, state) |>
  mutate(key = norm_place_key(city))
cat("Distinct eligible city/state pairs:", nrow(pairs), "\n")

m1 <- pairs |>
  inner_join(gaz |> select(key, state, lat, lon, geoid_place,
                           gaz_name = name),
             by = c("key", "state"), relationship = "many-to-many") |>
  group_by(city, state) |>
  slice(1L) |>
  ungroup() |>
  mutate(match_source = "gazetteer_exact",
         admin2_fips = NA_character_,
         matched_name = gaz_name,
         jw = 0)
cat("Step 1 gazetteer exact pairs:", nrow(m1), "\n")

remain <- pairs |> anti_join(m1, by = c("city", "state"))
m2 <- remain |>
  inner_join(geon |> select(key, state, lat, lon, admin2_fips,
                            geon_name = name, population),
             by = c("key", "state"), relationship = "many-to-many") |>
  arrange(city, state, desc(population)) |>
  group_by(city, state) |>
  slice(1L) |>
  ungroup() |>
  mutate(match_source = "geonames_exact",
         geoid_place = NA_character_,
         matched_name = geon_name,
         jw = 0)
cat("Step 2 geonames exact pairs:", nrow(m2), "\n")

remain <- remain |> anti_join(m2, by = c("city", "state"))
fuzzy_one <- function(key_in, state_in) {
  cands <- bind_rows(
    gaz |> filter(state == state_in) |>
      transmute(key, name, lat, lon, src = "gazetteer_fuzzy",
                admin2_fips = NA_character_, population = NA_integer_),
    geon |> filter(state == state_in) |>
      transmute(key, name, lat, lon, src = "geonames_fuzzy",
                admin2_fips, population)
  )
  if (!nrow(cands)) return(NULL)
  dist <- stringdist::stringdist(key_in, cands$key, method = "jw", p = 0.1)
  i <- which.min(dist)
  if (!length(i) || dist[[i]] > 0.10) return(NULL)
  cands[i, ] |> mutate(jw = dist[[i]])
}

if (nrow(remain) > 0L) {
  fuzzy_rows <- vector("list", nrow(remain))
  for (i in seq_len(nrow(remain))) {
    row <- remain[i, ]
    res <- fuzzy_one(row$key, row$state)
    if (!is.null(res)) {
      fuzzy_rows[[i]] <- bind_cols(row, res |> select(-key))
    }
  }
  m3 <- bind_rows(fuzzy_rows) |>
    rename(match_source = src, matched_name = name) |>
    mutate(geoid_place = NA_character_)
} else {
  m3 <- tibble()
}
cat("Step 3 fuzzy pairs:", nrow(m3), "\n")

matched <- bind_rows(
  m1 |> transmute(city, state, key, lat, lon, geoid_place, admin2_fips,
                  matched_name, match_source, jw),
  m2 |> transmute(city, state, key, lat, lon, geoid_place, admin2_fips,
                  matched_name, match_source, jw),
  if (nrow(m3)) {
    m3 |> transmute(city, state, key, lat, lon, geoid_place, admin2_fips,
                    matched_name, match_source, jw)
  } else {
    tibble()
  }
) |>
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) |>
  filter(!is.na(lat), !is.na(lon))

cat("Matched city/state pairs before county join:", nrow(matched), "\n")

counties <- st_read(county_shp, quiet = TRUE) |> st_transform(4326)
pts <- st_as_sf(matched, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
sj <- st_join(pts, counties |> select(STATEFP, COUNTYFP, GEOID, NAME),
              left = TRUE)
matched$geoid <- as.character(sj$GEOID)
matched$county_name <- as.character(sj$NAME)

snap_idx <- which(is.na(matched$geoid) | !nzchar(matched$geoid))
if (length(snap_idx)) {
  by_state <- split(snap_idx, matched$state[snap_idx])
  for (st in names(by_state)) {
    st_fp <- state_to_fp[st]
    if (is.na(st_fp)) next
    county_state <- counties[counties$STATEFP == st_fp, ]
    if (!nrow(county_state)) next
    nearest <- sf::st_nearest_feature(pts[by_state[[st]], ], county_state)
    matched$geoid[by_state[[st]]] <- as.character(county_state$GEOID[nearest])
    matched$county_name[by_state[[st]]] <-
      as.character(county_state$NAME[nearest])
  }
}

needs_fb <- (is.na(matched$geoid) | !nzchar(matched$geoid)) &
  !is.na(matched$admin2_fips) &
  nzchar(matched$admin2_fips)
county_lookup <- counties |>
  st_drop_geometry() |>
  transmute(state_fp = STATEFP, county_fp = COUNTYFP,
            geoid_fb = GEOID, name_fb = NAME)
for (i in which(needs_fb)) {
  st_fp <- state_to_fp[matched$state[[i]]]
  if (is.na(st_fp)) next
  hit <- county_lookup |>
    filter(state_fp == st_fp, county_fp == matched$admin2_fips[[i]])
  if (nrow(hit) == 1L) {
    matched$geoid[[i]] <- as.character(hit$geoid_fb[[1]])
    matched$county_name[[i]] <- as.character(hit$name_fb[[1]])
  }
}

overrides <- tribble(
  ~city,            ~state, ~geoid_new, ~county_new,
  "New York",       "NY",   "36061",    "New York",
  "Manhattan",      "NY",   "36061",    "New York",
  "Bronx",          "NY",   "36005",    "Bronx",
  "Brooklyn",       "NY",   "36047",    "Kings",
  "Queens",         "NY",   "36081",    "Queens",
  "Staten Island",  "NY",   "36085",    "Richmond",
  "Jersey City",    "NJ",   "34017",    "Hudson",
  "Hoboken",        "NJ",   "34017",    "Hudson",
  "San Francisco",  "CA",   "06075",    "San Francisco"
)
for (i in seq_len(nrow(overrides))) {
  hit <- matched$city == overrides$city[[i]] &
    matched$state == overrides$state[[i]]
  if (any(hit)) {
    matched$geoid[hit] <- overrides$geoid_new[[i]]
    matched$county_name[hit] <- overrides$county_new[[i]]
  }
}

matched <- matched |>
  mutate(geoid = ifelse(nzchar(geoid), str_pad(geoid, 5, pad = "0"), ""))

result <- eligible |>
  left_join(matched |>
              select(city, state, lat, lon, geoid, county_name,
                     matched_name, match_source, jw),
            by = c("city", "state")) |>
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    has_geoid = !is.na(geoid) & nzchar(geoid),
    birth_place_check = norm_for_check(birth_place),
    city_check = norm_for_check(birth_city),
    city_substr_hit = mapply(function(city_norm, place_norm) {
      if (!nzchar(city_norm)) return(FALSE)
      grepl(paste0("\\b", gsub("\\s+", "\\\\s+", city_norm), "\\b"),
            place_norm)
    }, city_check, birth_place_check, USE.NAMES = FALSE),
    cleaning_jw = stringdist::stringdist(
      city_check,
      substr(birth_place_check, 1, 25),
      method = "jw",
      p = 0.1
    ),
    geocoding_suspect = has_geoid & !city_substr_hit & cleaning_jw > 0.30
  )

geocoded <- result |>
  filter(has_geoid) |>
  select(-has_geoid, -birth_place_check, -city_check, -city_substr_hit)

eligible_unmatched <- result |>
  filter(!has_geoid) |>
  transmute(
    doc_id, lineid, source_lineid, entry_instance,
    raw_text_adjusted, birth_place, birth_date, birth_year,
    birth_city, birth_state, birth_country, field,
    unmatched_reason = "no_geocoder_match"
  )

ineligible_unmatched <- input |>
  filter(birth_country == "USA", !geocode_candidate) |>
  transmute(
    doc_id, lineid, source_lineid, entry_instance,
    raw_text_adjusted, birth_place, birth_date, birth_year,
    birth_city, birth_state, birth_country, field,
    unmatched_reason
  )

unmatched <- bind_rows(eligible_unmatched, ineligible_unmatched) |>
  arrange(doc_id, suppressWarnings(as.integer(lineid)))

suspects <- geocoded |>
  filter(geocoding_suspect) |>
  arrange(desc(cleaning_jw)) |>
  select(doc_id, lineid, raw_text_adjusted, birth_place, birth_city,
         birth_state, matched_name, county_name, geoid, cleaning_jw,
         match_source)

set.seed(1)
qc <- geocoded |>
  slice_sample(n = min(100L, nrow(geocoded))) |>
  select(doc_id, lineid, raw_text_adjusted, birth_place, birth_city,
         birth_state, birth_year, matched_name, county_name, geoid,
         lat, lon, match_source, jw, cleaning_jw)

summary <- bind_rows(
  tibble(metric = "input_file", value = input_file),
  tibble(metric = "input_rows", value = as.character(nrow(input))),
  tibble(metric = "input_unique_doc_lineid",
         value = as.character(n_distinct(paste(input$doc_id, input$lineid)))),
  tibble(metric = "usa_rows", value = as.character(nrow(us_all))),
  tibble(metric = "eligible_usa_rows", value = as.character(nrow(eligible))),
  tibble(metric = "geocoded_rows", value = as.character(nrow(geocoded))),
  tibble(metric = "unmatched_rows", value = as.character(nrow(unmatched))),
  tibble(metric = "suspect_rows", value = as.character(nrow(suspects))),
  unmatched |>
    count(unmatched_reason, name = "value") |>
    transmute(metric = paste0("unmatched_reason:", unmatched_reason),
              value = as.character(value)),
  geocoded |>
    count(match_source, name = "value") |>
    transmute(metric = paste0("match_source:", match_source),
              value = as.character(value))
)

write_excel_csv(geocoded, geocoded_csv, na = "")
write_excel_csv(unmatched, unmatched_csv, na = "")
write_excel_csv(suspects, suspects_csv, na = "")
write_excel_csv(qc, qc_csv, na = "")
write_excel_csv(summary, summary_csv, na = "")

cat("Geocoded rows:", nrow(geocoded), "\n")
cat("Unmatched rows:", nrow(unmatched), "\n")
cat("Suspects:", nrow(suspects), "\n")
cat("Wrote geocoded:", geocoded_csv, "\n")
cat("Wrote unmatched:", unmatched_csv, "\n")
cat("Wrote suspects:", suspects_csv, "\n")
cat("Wrote QC:", qc_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
