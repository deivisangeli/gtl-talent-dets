###############################################################################
# Geocode Wikipedia world's fairs with local GeoNames data.
#
# Inputs:
#   DATA_INPUT/worlds_fairs_wikipedia.xlsx
#
# Outputs:
#   DATA_INPUT/worlds_fairs_wikipedia_geocoded.xlsx
#   DATA_INPUT/worlds_fairs_wikipedia_geocoded_unmatched.xlsx
#   DATA_INPUT/worlds_fairs_wikipedia_geocoded_qc.xlsx
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/fair_enrichment/02_geocode_worlds_fairs.R
###############################################################################

suppressPackageStartupMessages({
  library(countrycode)
  library(data.table)
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringdist)
  library(stringr)
  library(tibble)
  library(writexl)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", "..", ".."),
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
options(timeout = 2000)

###############################################################################
# Paths and downloads
###############################################################################

geonames_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "geonames")
dir.create(geonames_dir, recursive = TRUE, showWarnings = FALSE)

all_countries_file <- file.path(geonames_dir, "allCountries.txt")
admin1_file <- file.path(geonames_dir, "admin1CodesASCII.txt")
country_info_file <- file.path(geonames_dir, "countryInfo.txt")

download_if_missing <- function(url, dest, unzip_to = NULL) {
  if (file.exists(dest)) {
    return(invisible(dest))
  }

  zf <- tempfile(fileext = ".zip")
  message("Downloading ", url)
  download.file(url, zf, mode = "wb")

  if (is.null(unzip_to)) {
    file.copy(zf, dest, overwrite = TRUE)
  } else {
    utils::unzip(zf, exdir = unzip_to)
  }

  unlink(zf)
  invisible(dest)
}

if (!file.exists(all_countries_file)) {
  download_if_missing(
    "https://download.geonames.org/export/dump/allCountries.zip",
    all_countries_file,
    unzip_to = geonames_dir
  )
}

if (!file.exists(admin1_file)) {
  download.file(
    "https://download.geonames.org/export/dump/admin1CodesASCII.txt",
    admin1_file,
    mode = "wb"
  )
}

if (!file.exists(country_info_file)) {
  download.file(
    "https://download.geonames.org/export/dump/countryInfo.txt",
    country_info_file,
    mode = "wb"
  )
}

in_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia.xlsx")
out_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia_geocoded.xlsx")
unmatched_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia_geocoded_unmatched.xlsx")
qc_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia_geocoded_qc.xlsx")

###############################################################################
# Helpers
###############################################################################

norm_text <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(ifelse(is.na(x), "", x))
  x <- str_replace_all(x, "&", " and ")
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  str_squish(x)
}

year_start <- function(x) {
  as.integer(str_extract(x, "^[0-9]{4}"))
}

hyde_floor_decade <- function(y) {
  d <- floor(y / 10) * 10
  ifelse(d < 1800 | d > 2000, NA_integer_, as.integer(d))
}

hyde_nearest_decade <- function(y) {
  d <- round(y / 10) * 10
  ifelse(d < 1800 | d > 2000, NA_integer_, as.integer(d))
}

write_xlsx_replace <- function(x, path) {
  tmp <- file.path(dirname(path), paste0(".", tools::file_path_sans_ext(basename(path)), "_tmp.xlsx"))
  if (file.exists(tmp)) {
    unlink(tmp)
  }
  writexl::write_xlsx(x, tmp)
  if (file.exists(path)) {
    unlink(path)
  }
  if (!file.rename(tmp, path)) {
    stop("Could not replace output file: ", path)
  }
}

###############################################################################
# Load GeoNames reference data
###############################################################################

message("Reading GeoNames countries...")
country_info <- fread(
  country_info_file,
  sep = "\t",
  skip = "#ISO",
  header = TRUE,
  fill = TRUE,
  quote = "",
  encoding = "UTF-8"
) %>%
  as_tibble() %>%
  transmute(
    iso2 = `#ISO`,
    iso3 = ISO3,
    country_name = Country,
    continent = Continent
  )

message("Reading GeoNames admin-1...")
admin1 <- fread(
  admin1_file,
  sep = "\t",
  header = FALSE,
  fill = TRUE,
  quote = "",
  col.names = c("admin1_full", "admin1_name", "admin1_ascii", "admin1_geonameid"),
  encoding = "UTF-8"
) %>%
  as_tibble() %>%
  mutate(
    matched_country_iso2 = str_extract(admin1_full, "^[A-Z]{2}"),
    admin1_code = str_replace(admin1_full, "^[A-Z]{2}\\.", "")
  ) %>%
  select(matched_country_iso2, admin1_code, admin1_name, admin1_ascii)

message("Reading GeoNames allCountries. This can take a few minutes...")
geon_cols <- c(
  "geonameid", "name", "asciiname", "alternatenames", "latitude",
  "longitude", "feature_class", "feature_code", "country_code", "cc2",
  "admin1_code", "admin2_code", "admin3_code", "admin4_code", "population",
  "elevation", "dem", "timezone", "modification_date"
)

geon <- fread(
  all_countries_file,
  sep = "\t",
  header = FALSE,
  quote = "",
  fill = TRUE,
  select = c(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 15),
  col.names = geon_cols[c(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 15)],
  encoding = "UTF-8",
  showProgress = TRUE
) %>%
  as_tibble() %>%
  filter(feature_class %in% c("P", "A")) %>%
  mutate(
    key_name = norm_text(name),
    key_ascii = norm_text(asciiname),
    population = as.numeric(population)
  )

geon <- geon %>%
  left_join(country_info, by = c("country_code" = "iso2")) %>%
  left_join(admin1, by = c("country_code" = "matched_country_iso2", "admin1_code" = "admin1_code"))

###############################################################################
# Load fair data and country lookup
###############################################################################

fairs <- read_xlsx(in_file) %>%
  mutate(
    row_id = row_number(),
    year_start = year_start(Year),
    hyde_decade_floor = hyde_floor_decade(year_start),
    hyde_decade_nearest = hyde_nearest_decade(year_start)
  )

if (!"Fair_observation" %in% names(fairs)) {
  fairs <- fairs %>% mutate(Fair_observation = NA_character_)
}

custom_country_iso2 <- c(
  "Austria-Hungary" = NA,
  "Bavaria" = "DE",
  "Bohemia" = "CZ",
  "California United States" = "US",
  "Cape Colony" = "ZA",
  "Cape Colony (now South Africa)" = "ZA",
  "Cape of Good Hope" = "ZA",
  "Ceylon" = "LK",
  "Dutch East Indies" = "ID",
  "French Indochina" = NA,
  "Mandatory Palestine" = "IL",
  "New South Wales" = "AU",
  "Ohio" = "US",
  "Ottoman Empire" = NA,
  "Piedmont-Sardinia" = "IT",
  "Prussia" = "DE",
  "Queensland" = "AU",
  "Russian Empire" = "RU",
  "Saint Helena" = "SH",
  "South Australia" = "AU",
  "Soviet Union" = NA,
  "Tasmania" = "AU",
  "Türkiye" = "TR",
  "Two Sicilies" = "IT",
  "Victoria" = "AU",
  "West Germany" = "DE",
  "Western Australia" = "AU"
)

city_country_overrides <- tribble(
  ~City, ~Country, ~lookup_iso2,
  "Prague", "Austria-Hungary", "CZ",
  "Kiel", "Austria-Hungary", "DE",
  "Szeged", "Austria-Hungary", "HU",
  "Vienna", "Austria-Hungary", "AT",
  "Budapest", "Austria-Hungary", "HU",
  "Zagreb", "Austria-Hungary", "HR",
  "Brno", "Austria-Hungary", "CZ",
  "Hanoi", "French Indochina", "VN",
  "Saigon", "French Indochina", "VN",
  "Jerusalem", "Mandatory Palestine", "IL",
  "Istanbul", "Ottoman Empire", "TR",
  "Moscow", "Soviet Union", "RU",
  "Leningrad", "Soviet Union", "RU",
  "Kiev", "Soviet Union", "UA",
  "Riga", "Soviet Union", "LV"
)

city_lookup_overrides <- tribble(
  ~City, ~Country, ~lookup_city, ~lookup_iso2_alias,
  "1925 Buenos Aires", "Argentina", "Buenos Aires", "AR",
  "Taipei. Taiwan", NA_character_, "Taipei", "TW",
  "Ciudad Trujillo (Santo Domingo)", "Dominican Republic", "Santo Domingo", "DO",
  "three expositions (1 was cancelled) celebrating 500 years sinceChristopher Columbusreached the Americas Seville", "Spain", "Seville", "ES",
  "Metro Manila", "Philippines", "Manila", "PH"
)

pairs <- fairs %>%
  distinct(City, Country) %>%
  mutate(
    pair_id = row_number(),
    country_iso2_auto = countrycode(Country, "country.name", "iso2c", warn = FALSE),
    country_iso2_custom = unname(custom_country_iso2[Country]),
    lookup_iso2 = coalesce(country_iso2_custom, country_iso2_auto)
  ) %>%
  left_join(city_country_overrides, by = c("City", "Country"), suffix = c("", "_override")) %>%
  left_join(city_lookup_overrides, by = c("City", "Country")) %>%
  mutate(
    lookup_city = coalesce(lookup_city, City),
    city_key = norm_text(lookup_city),
    lookup_iso2 = coalesce(lookup_iso2_alias, lookup_iso2_override, lookup_iso2),
    lookup_source = case_when(
      !is.na(lookup_city) & lookup_city != City ~ "city_lookup_override",
      !is.na(lookup_iso2_alias) ~ "city_lookup_override",
      !is.na(lookup_iso2_override) ~ "city_country_override",
      !is.na(country_iso2_custom) ~ "historical_country_override",
      !is.na(country_iso2_auto) ~ "countrycode",
      TRUE ~ "missing_country_lookup"
    )
  ) %>%
  select(-lookup_iso2_override)

fairs_with_pairs <- fairs %>%
  left_join(
    pairs %>% select(City, Country, pair_id),
    by = c("City", "Country")
  )

###############################################################################
# Matching
###############################################################################

candidate_rank <- function(feature_class, feature_code, population) {
  case_when(
    feature_code == "PPLC" ~ 1L,
    feature_code %in% c("PPLA", "PPLA2", "PPLA3", "PPLA4") ~ 2L,
    feature_class == "P" ~ 3L,
    feature_class == "A" ~ 4L,
    TRUE ~ 5L
  ) * 1e10 - coalesce(as.numeric(population), 0)
}

select_best <- function(cands) {
  cands %>%
    mutate(rank_value = candidate_rank(feature_class, feature_code, population)) %>%
    arrange(rank_value) %>%
    slice(1) %>%
    select(-rank_value)
}

match_one <- function(city, country, city_key, lookup_iso2) {
  if (!is.na(lookup_iso2)) {
    cands <- geon %>%
      filter(country_code == lookup_iso2, key_name == city_key | key_ascii == city_key)
    if (nrow(cands) > 0) {
      return(select_best(cands) %>% mutate(match_source = "exact_city_country", match_score = 0))
    }

    country_cands <- geon %>% filter(country_code == lookup_iso2)
    if (nrow(country_cands) > 0) {
      dist_name <- stringdist(city_key, country_cands$key_name, method = "jw", p = 0.1)
      dist_ascii <- stringdist(city_key, country_cands$key_ascii, method = "jw", p = 0.1)
      d <- pmin(dist_name, dist_ascii)
      best <- which.min(d)
      if (length(best) > 0 && is.finite(d[best]) && d[best] <= 0.10) {
        return(country_cands[best, ] %>% mutate(match_source = "fuzzy_city_country", match_score = d[best]))
      }
    }
  }

  global_cands <- geon %>%
    filter(key_name == city_key | key_ascii == city_key)

  if (nrow(global_cands) == 1) {
    return(global_cands %>% mutate(match_source = "exact_city_global_unique", match_score = 0))
  }

  if (nrow(global_cands) > 1) {
    return(select_best(global_cands) %>% mutate(match_source = "exact_city_global_ambiguous_best_population", match_score = 0))
  }

  tibble()
}

message("Matching ", nrow(pairs), " distinct city-country pairs...")
matches <- vector("list", nrow(pairs))
for (i in seq_len(nrow(pairs))) {
  p <- pairs[i, ]
  matches[[i]] <- match_one(p$City, p$Country, p$city_key, p$lookup_iso2)
  if (nrow(matches[[i]]) > 0) {
    matches[[i]] <- matches[[i]] %>% mutate(pair_id = p$pair_id)
  }
}

matched_pairs <- bind_rows(matches)

geocoded_pairs <- pairs %>%
  left_join(matched_pairs, by = "pair_id") %>%
  mutate(
    matched_country_iso2 = country_code,
    matched_country_iso3 = iso3,
    lon = as.numeric(longitude),
    lat = as.numeric(latitude),
    population_geonames = population,
    needs_review = is.na(geonameid) |
      match_source != "exact_city_country" |
      lookup_source != "countrycode" |
      is.na(lookup_iso2)
  ) %>%
  select(
    pair_id, City, Country, lookup_city, lookup_iso2, lookup_source,
    lat, lon, geonameid, matched_name = name,
    matched_country_iso2, matched_country_iso3,
    admin1_code, admin1_name, feature_class, feature_code,
    population_geonames, match_source, match_score, needs_review
  )

###############################################################################
# Join back to fairs and write outputs
###############################################################################

out <- fairs_with_pairs %>%
  left_join(
    geocoded_pairs %>% select(-City, -Country),
    by = "pair_id"
  ) %>%
  select(
    Year, City, Country, Fair_name, Fair_observation,
    year_start, hyde_decade_floor, hyde_decade_nearest,
    lat, lon, geonameid, matched_name,
    matched_country_iso2, matched_country_iso3,
    admin1_code, admin1_name,
    feature_class, feature_code, population_geonames,
    match_source, match_score, needs_review,
    lookup_city, lookup_iso2, lookup_source
  )

unmatched <- out %>%
  filter(is.na(geonameid))

qc <- out %>%
  filter(needs_review | City %in% c(
    "London", "Paris", "Philadelphia", "Prague", "Vienna",
    "Chicago", "Melbourne", "Buenos Aires", "Tokyo", "Osaka"
  )) %>%
  arrange(desc(needs_review), Country, City, Year)

write_xlsx_replace(out, out_file)
write_xlsx_replace(unmatched, unmatched_file)
write_xlsx_replace(qc, qc_file)

###############################################################################
# Validation
###############################################################################

if (nrow(out) != nrow(fairs)) {
  stop("Output row count changed: ", nrow(out), " vs ", nrow(fairs))
}

coverage <- out %>%
  summarise(
    rows = n(),
    matched_rows = sum(!is.na(geonameid)),
    unmatched_rows = sum(is.na(geonameid)),
    matched_pct = round(100 * matched_rows / rows, 2),
    needs_review_rows = sum(needs_review, na.rm = TRUE)
  )

coverage_by_country <- out %>%
  group_by(Country) %>%
  summarise(
    rows = n(),
    matched_rows = sum(!is.na(geonameid)),
    unmatched_rows = sum(is.na(geonameid)),
    needs_review_rows = sum(needs_review, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(unmatched_rows), desc(needs_review_rows), Country)

cat("\n=== World's fairs geocoding coverage ===\n")
print(coverage)

cat("\n=== Countries with unmatched rows ===\n")
print(coverage_by_country %>% filter(unmatched_rows > 0), n = 50)

cat("\n=== Match source mix ===\n")
print(out %>% count(match_source, name = "rows") %>% arrange(desc(rows)))

cat("\nWrote:\n")
cat("- ", out_file, "\n", sep = "")
cat("- ", unmatched_file, "\n", sep = "")
cat("- ", qc_file, "\n", sep = "")
