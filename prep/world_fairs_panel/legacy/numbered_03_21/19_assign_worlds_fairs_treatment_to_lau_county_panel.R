###############################################################################
# Assign world's-fair host treatments to the UK LAU + US county panel.
#
# Treatment is based on the geocoded host city of each fair, not on venue
# coordinates. European/UK fairs are assigned to GISCO LAUs; US fairs are assigned
# to TIGER/Line county polygons.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/19_assign_worlds_fairs_treatment_to_lau_county_panel.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(readxl)
  library(sf)
  library(stringr)
  library(tidyr)
  library(tigris)
})

options(tigris_use_cache = TRUE)

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

DATA_PROCESSED <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed")
fairs_dir <- file.path(DATA_INPUT, "worlds_fairs")

panel_file <- file.path(
  DATA_PROCESSED,
  "uk_lau_us_county_inventor_panel_1800_1960_nhgis_us.csv"
)
fairs_file <- file.path(
  fairs_dir,
  "worlds_fairs_1790_1960_with_visits_venues.csv"
)
fairs_geo_file <- file.path(
  DATA_INPUT,
  "worlds_fairs_wikipedia_geocoded.xlsx"
)
uk_lau_boundaries_file <- file.path(
  TALENT_DETS_DATA_DIR,
  "raw", "gisco", "lau", "LAU_RG_01M_2019_4326.gpkg"
)

out_panel <- file.path(
  DATA_PROCESSED,
  "uk_lau_us_county_inventor_panel_1800_1960_nhgis_us_worlds_fairs_treatment.csv"
)
out_assignment <- file.path(
  DATA_PROCESSED,
  "worlds_fairs_lau_county_treatment_assignment_1790_1960.csv"
)
out_audit <- file.path(
  DATA_PROCESSED,
  "worlds_fairs_lau_county_treatment_match_audit_1790_1960.csv"
)
out_summary <- file.path(
  DATA_PROCESSED,
  "worlds_fairs_lau_county_treatment_summary_1790_1960.csv"
)

required_files <- c(panel_file, fairs_file, fairs_geo_file, uk_lau_boundaries_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

###############################################################################
# Helpers
###############################################################################

normalize_key_text <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

standard_decade <- function(year) {
  if_else(is.na(year), NA_integer_, as.integer(floor(year / 10) * 10))
}

alt_decade <- function(year) {
  base <- standard_decade(year)
  if_else(!is.na(year) & year %% 10 >= 7, base + 10L, base)
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    NA
  } else {
    x[[1L]]
  }
}

###############################################################################
# Read panel and fairs
###############################################################################

cat("Reading combined LAU + US county panel...\n")
panel <- read_csv(panel_file, show_col_types = FALSE) %>%
  mutate(
    unit_type = as.character(unit_type),
    unit_id = as.character(unit_id),
    GEOID = if_else(
      is.na(GEOID),
      NA_character_,
      str_pad(as.character(as.integer(GEOID)), 5, pad = "0")
    ),
    lau_id = as.character(lau_id),
    year = as.integer(year)
  )

cat("Reading consolidated world's fairs and host geocodes...\n")
fairs <- read_csv(fairs_file, show_col_types = FALSE) %>%
  mutate(
    fair_id = as.integer(fair_id),
    year_start = as.integer(year_start),
    key_year = year_start,
    key_city = normalize_key_text(City),
    key_fair = normalize_key_text(Fair_name)
  )

fairs_geo <- read_xlsx(fairs_geo_file) %>%
  mutate(
    year_start = as.integer(year_start),
    key_year = year_start,
    key_city = normalize_key_text(City),
    key_fair = normalize_key_text(Fair_name),
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    geonameid = suppressWarnings(as.integer(geonameid)),
    matched_country_iso3 = as.character(matched_country_iso3)
  ) %>%
  select(
    key_year,
    key_city,
    key_fair,
    host_lat = lat,
    host_lon = lon,
    geonameid,
    matched_name,
    matched_country_iso2,
    matched_country_iso3,
    admin1_code,
    admin1_name,
    match_source,
    match_score,
    needs_review
  )

duplicate_geo_keys <- fairs_geo %>%
  count(key_year, key_city, key_fair, name = "n") %>%
  filter(n > 1L)
if (nrow(duplicate_geo_keys) > 0L) {
  stop("Duplicate fair geocode keys found: ", nrow(duplicate_geo_keys))
}

fairs_host <- fairs %>%
  left_join(fairs_geo, by = c("key_year", "key_city", "key_fair")) %>%
  mutate(
    fair_row_id = row_number(),
    event_year = year_start,
    # Clear inherited geocoding errors in the host-city file. These are city
    # names that were sent to UK homonyms but refer to the named fair host city.
    host_lat = case_when(
      City == "Dublin" & event_year %in% c(1865L, 1874L, 1907L) ~ 53.3498,
      City == "Cork" & event_year %in% c(1883L, 1902L) ~ 51.8985,
      City == "Newcastle" & event_year == 1887L ~ 54.9783,
      TRUE ~ host_lat
    ),
    host_lon = case_when(
      City == "Dublin" & event_year %in% c(1865L, 1874L, 1907L) ~ -6.2603,
      City == "Cork" & event_year %in% c(1883L, 1902L) ~ -8.4756,
      City == "Newcastle" & event_year == 1887L ~ -1.6178,
      TRUE ~ host_lon
    ),
    matched_name = case_when(
      City == "Dublin" & event_year %in% c(1865L, 1874L, 1907L) ~ "Dublin",
      City == "Cork" & event_year %in% c(1883L, 1902L) ~ "Cork",
      City == "Newcastle" & event_year == 1887L ~ "Newcastle upon Tyne",
      TRUE ~ matched_name
    ),
    matched_country_iso2 = case_when(
      City == "Dublin" & event_year %in% c(1865L, 1874L, 1907L) ~ "IE",
      City == "Cork" & event_year %in% c(1883L, 1902L) ~ "IE",
      City == "Newcastle" & event_year == 1887L ~ "GB",
      TRUE ~ matched_country_iso2
    ),
    matched_country_iso3 = case_when(
      City == "Dublin" & event_year %in% c(1865L, 1874L, 1907L) ~ "IRL",
      City == "Cork" & event_year %in% c(1883L, 1902L) ~ "IRL",
      City == "Newcastle" & event_year == 1887L ~ "GBR",
      TRUE ~ matched_country_iso3
    ),
    admin1_name = case_when(
      City == "Dublin" & event_year %in% c(1865L, 1874L, 1907L) ~ "Leinster",
      City == "Cork" & event_year %in% c(1883L, 1902L) ~ "Munster",
      City == "Newcastle" & event_year == 1887L ~ "England",
      TRUE ~ admin1_name
    ),
    decade_standard = standard_decade(event_year),
    decade_alt = alt_decade(event_year),
    has_host_coords = !is.na(host_lon) & !is.na(host_lat)
  )

if (any(is.na(fairs_host$host_lon) | is.na(fairs_host$host_lat))) {
  missing_geo <- fairs_host %>%
    filter(is.na(host_lon) | is.na(host_lat)) %>%
    select(fair_id, Year, City, Fair_name)
  stop(
    "Some consolidated fairs lack host geocodes:\n",
    paste(capture.output(print(missing_geo, n = Inf)), collapse = "\n")
  )
}

###############################################################################
# Build spatial matches
###############################################################################

cat("Assigning UK host points to GISCO 2019 LAUs...\n")
europe_lau_units <- panel %>%
  filter(unit_type == "europe_lau") %>%
  distinct(
    lau_id,
    europe_unit_id = unit_id,
    europe_place_name = place_name,
    europe_country = country,
    europe_iso3 = iso3
  ) %>%
  filter(!is.na(lau_id))

uk_lau_poly <- st_read(uk_lau_boundaries_file, quiet = TRUE) %>%
  st_transform(4326) %>%
  filter(CNTR_CODE == "UK", substr(LAU_ID, 1L, 1L) %in% c("E", "W")) %>%
  select(LAU_ID, CNTR_CODE, LAU_NAME)

europe_fairs_spatial <- fairs_host %>%
  filter(
    matched_country_iso3 == "GBR",
    has_host_coords
  ) %>%
  st_as_sf(coords = c("host_lon", "host_lat"), crs = 4326, remove = FALSE) %>%
  st_join(uk_lau_poly, join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  select(
    fair_row_id,
    fair_lau_id = LAU_ID,
    fair_lau_iso2 = CNTR_CODE,
    fair_lau_name = LAU_NAME
  ) %>%
  left_join(europe_lau_units, by = c("fair_lau_id" = "lau_id"))

cat("Assigning US host points to TIGER counties...\n")
us_panel_units <- panel %>%
  filter(unit_type == "us_county") %>%
  distinct(
    GEOID,
    us_unit_id = unit_id,
    us_place_name = place_name
  ) %>%
  filter(!is.na(GEOID))

counties_poly <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  st_transform(4326) %>%
  select(GEOID, NAMELSAD, STATEFP, geometry) %>%
  filter(as.integer(STATEFP) <= 56) %>%
  mutate(GEOID = as.character(GEOID))

us_fairs_spatial <- fairs_host %>%
  filter(
    matched_country_iso3 == "USA",
    has_host_coords
  ) %>%
  st_as_sf(coords = c("host_lon", "host_lat"), crs = 4326, remove = FALSE) %>%
  st_join(counties_poly, join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  select(
    fair_row_id,
    fair_us_geoid = GEOID,
    fair_us_county_name = NAMELSAD,
    fair_us_statefp = STATEFP
  ) %>%
  left_join(us_panel_units, by = c("fair_us_geoid" = "GEOID"))

###############################################################################
# Audit, assignment, and panel
###############################################################################

cat("Building treatment assignment and panel columns...\n")
fair_match_audit <- fairs_host %>%
  left_join(europe_fairs_spatial, by = "fair_row_id") %>%
  left_join(us_fairs_spatial, by = "fair_row_id") %>%
  mutate(
    assigned_unit_type = case_when(
      matched_country_iso3 == "USA" & !is.na(us_unit_id) ~ "us_county",
      matched_country_iso3 != "USA" & !is.na(europe_unit_id) ~ "europe_lau",
      TRUE ~ NA_character_
    ),
    assigned_unit_id = case_when(
      assigned_unit_type == "us_county" ~ us_unit_id,
      assigned_unit_type == "europe_lau" ~ europe_unit_id,
      TRUE ~ NA_character_
    ),
    assigned_lau_id = if_else(assigned_unit_type == "europe_lau", fair_lau_id, NA_character_),
    assigned_GEOID = if_else(assigned_unit_type == "us_county", fair_us_geoid, NA_character_),
    match_status = case_when(
      is.na(event_year) ~ "missing_event_year",
      !has_host_coords ~ "missing_host_coordinates",
      matched_country_iso3 == "USA" & is.na(fair_us_geoid) ~ "usa_no_county_spatial_match",
      matched_country_iso3 == "USA" & is.na(us_unit_id) ~ "usa_county_not_in_panel",
      matched_country_iso3 == "GBR" & is.na(fair_lau_id) ~ "gbr_no_lau_spatial_match",
      matched_country_iso3 == "GBR" & is.na(europe_unit_id) ~ "gbr_lau_not_in_panel",
      matched_country_iso3 != "USA" & matched_country_iso3 != "GBR" ~ "non_gbr_not_in_lau_panel",
      !is.na(assigned_unit_id) ~ "matched_to_panel",
      TRUE ~ "unmatched"
    )
  ) %>%
  select(
    fair_id,
    source_period,
    source_row_id,
    Year,
    City,
    Country,
    Fair_name,
    event_year,
    decade_standard,
    decade_alt,
    host_lat,
    host_lon,
    geonameid,
    matched_name,
    matched_country_iso3,
    matched_country_iso2,
    admin1_code,
    admin1_name,
    assigned_unit_type,
    assigned_unit_id,
    assigned_lau_id,
    assigned_GEOID,
    fair_lau_iso2,
    fair_lau_name,
    fair_us_county_name,
    fair_us_statefp,
    europe_place_name,
    europe_country,
    europe_iso3,
    us_place_name,
    match_status
  )

fair_unit_decades <- fair_match_audit %>%
  filter(match_status == "matched_to_panel") %>%
  transmute(
    unit_type = assigned_unit_type,
    unit_id = assigned_unit_id,
    fair_id,
    fair_name = Fair_name,
    fair_city = City,
    fair_country = Country,
    event_year,
    decade_standard,
    decade_alt
  )

decade_flags_standard <- fair_unit_decades %>%
  group_by(unit_type, unit_id, year = decade_standard) %>%
  summarise(
    hosted_fair_standard = 1L,
    n_fairs_standard_decade = n(),
    fair_ids_standard_decade = paste(sort(unique(fair_id)), collapse = ";"),
    fair_names_standard_decade = paste(unique(fair_name), collapse = " | "),
    .groups = "drop"
  )

decade_flags_alt <- fair_unit_decades %>%
  group_by(unit_type, unit_id, year = decade_alt) %>%
  summarise(
    hosted_fair_alt = 1L,
    n_fairs_alt_decade = n(),
    fair_ids_alt_decade = paste(sort(unique(fair_id)), collapse = ";"),
    fair_names_alt_decade = paste(unique(fair_name), collapse = " | "),
    .groups = "drop"
  )

treatment_assignment <- fair_unit_decades %>%
  arrange(unit_type, unit_id, event_year, fair_id) %>%
  group_by(unit_type, unit_id) %>%
  summarise(
    first_fair_year = first(event_year),
    first_fair_name = first(fair_name),
    first_fair_city = first(fair_city),
    first_fair_country = first_nonmissing(fair_country),
    g_standard = standard_decade(first_fair_year),
    g_alt = alt_decade(first_fair_year),
    n_fairs_1790_1960 = n(),
    fair_years_1790_1960 = paste(sort(unique(event_year)), collapse = ";"),
    fair_ids_1790_1960 = paste(sort(unique(fair_id)), collapse = ";"),
    fair_names_1790_1960 = paste(unique(fair_name), collapse = " | "),
    .groups = "drop"
  )

panel_out <- panel %>%
  left_join(treatment_assignment, by = c("unit_type", "unit_id")) %>%
  left_join(decade_flags_standard, by = c("unit_type", "unit_id", "year")) %>%
  left_join(decade_flags_alt, by = c("unit_type", "unit_id", "year")) %>%
  mutate(
    first_fair_year = replace_na(first_fair_year, 0L),
    g_standard = replace_na(g_standard, 0L),
    g_alt = replace_na(g_alt, 0L),
    n_fairs_1790_1960 = replace_na(n_fairs_1790_1960, 0L),
    hosted_fair_standard = replace_na(hosted_fair_standard, 0L),
    hosted_fair_alt = replace_na(hosted_fair_alt, 0L),
    n_fairs_standard_decade = replace_na(n_fairs_standard_decade, 0L),
    n_fairs_alt_decade = replace_na(n_fairs_alt_decade, 0L)
  )

###############################################################################
# Validation and write
###############################################################################

if (nrow(panel_out) != nrow(panel)) {
  stop("Panel row count changed after treatment joins.")
}
if (n_distinct(panel_out$unit_id) != n_distinct(panel$unit_id)) {
  stop("Panel unit count changed after treatment joins.")
}
if (any(duplicated(panel_out[c("unit_id", "year")]))) {
  stop("Duplicate unit_id-year rows found in output panel.")
}

summary_rows <- bind_rows(
  tibble(
    section = "input",
    metric = "panel_rows",
    value = nrow(panel)
  ),
  tibble(
    section = "input",
    metric = "panel_units",
    value = n_distinct(panel$unit_id)
  ),
  tibble(
    section = "input",
    metric = "fairs_consolidated_1790_1960",
    value = nrow(fairs_host)
  ),
  tibble(
    section = "input",
    metric = "fairs_with_host_coordinates",
    value = sum(fairs_host$has_host_coords)
  ),
  fair_match_audit %>%
    count(match_status, name = "value") %>%
    transmute(section = "match_status", metric = match_status, value),
  treatment_assignment %>%
    count(unit_type, name = "value") %>%
    transmute(section = "treated_units", metric = unit_type, value),
  panel_out %>%
    distinct(unit_type, unit_id, g_standard) %>%
    count(unit_type, g_standard, name = "value") %>%
    transmute(
      section = "standard_cohort_units",
      metric = paste(unit_type, g_standard, sep = "_"),
      value
    ),
  panel_out %>%
    distinct(unit_type, unit_id, g_alt) %>%
    count(unit_type, g_alt, name = "value") %>%
    transmute(
      section = "alt_cohort_units",
      metric = paste(unit_type, g_alt, sep = "_"),
      value
    )
)

fwrite(as.data.table(panel_out), out_panel)
fwrite(as.data.table(treatment_assignment), out_assignment)
fwrite(as.data.table(fair_match_audit), out_audit)
fwrite(as.data.table(summary_rows), out_summary)

cat("\nCompleted world's-fair treatment assignment.\n")
cat("Panel: ", out_panel, "\n", sep = "")
cat("Assignment: ", out_assignment, "\n", sep = "")
cat("Audit: ", out_audit, "\n", sep = "")
cat("Summary: ", out_summary, "\n", sep = "")
