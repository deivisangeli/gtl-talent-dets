###############################################################################
# Build a city-year panel of Discovery/Science births in the US and Europe.
#
# The cross-verified database contains birth coordinates, but not city names.
# This script assigns each person to the nearest GeoNames populated place within
# the same birth country, then aggregates counts by city and birth year.
#
# Outputs:
#   output/discovery_science_city_year_us_europe.csv
#   output/discovery_science_city_year_us_europe.xlsx
#   output/discovery_science_city_year_us_europe_unmatched.csv
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(FNN)
  library(readr)
  library(rnaturalearth)
  library(sf)
  library(stringr)
  library(writexl)
})

initial_time <- Sys.time()
sf::sf_use_s2(FALSE)

repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = NA), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "prep", "stem_labels.R"))
source(file.path(repo_root, "paths.R"))

###############################################################################
# Paths
###############################################################################

raw_file <- file.path(DATA_INPUT, "cross-verified-database.csv")
geonames_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "geonames")
all_countries_file <- file.path(geonames_dir, "allCountries.txt")
country_info_file <- file.path(geonames_dir, "countryInfo.txt")
admin1_file <- file.path(geonames_dir, "admin1CodesASCII.txt")

out_file <- file.path(DATA_OUTPUT, "discovery_science_city_year_us_europe.csv")
out_xlsx <- file.path(DATA_OUTPUT, "discovery_science_city_year_us_europe.xlsx")
unmatched_file <- file.path(DATA_OUTPUT, "discovery_science_city_year_us_europe_unmatched.csv")

stopifnot(file.exists(raw_file))
stopifnot(file.exists(all_countries_file))
stopifnot(file.exists(country_info_file))
stopifnot(file.exists(admin1_file))

###############################################################################
# Helpers
###############################################################################

fix_iso_a3 <- function(iso_a3, country_name) {
  case_when(
    iso_a3 == "-99" & grepl("France",      country_name, ignore.case = TRUE) ~ "FRA",
    iso_a3 == "-99" & grepl("Norway",      country_name, ignore.case = TRUE) ~ "NOR",
    iso_a3 == "-99" & grepl("Denmark",     country_name, ignore.case = TRUE) ~ "DNK",
    iso_a3 == "-99" & grepl("Netherlands", country_name, ignore.case = TRUE) ~ "NLD",
    TRUE ~ iso_a3
  )
}

haversine_km <- function(lon1, lat1, lon2, lat2) {
  r <- 6371.0088
  to_rad <- pi / 180
  p1 <- lat1 * to_rad
  p2 <- lat2 * to_rad
  dp <- (lat2 - lat1) * to_rad
  dl <- (lon2 - lon1) * to_rad
  a <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

assign_nearest_city <- function(people_dt, geon_dt, max_distance_km = 50) {
  countries <- sort(unique(people_dt$iso2))
  matched <- vector("list", length(countries))
  unmatched <- vector("list", length(countries))

  for (i in seq_along(countries)) {
    cc <- countries[i]
    p <- people_dt[iso2 == cc]
    g <- geon_dt[country_code == cc]

    if (nrow(g) == 0) {
      p[, unmatched_reason := "no_geonames_country_candidates"]
      unmatched[[i]] <- p
      next
    }

    nn <- FNN::get.knnx(
      data = as.matrix(g[, .(longitude, latitude)]),
      query = as.matrix(p[, .(bplo1, bpla1)]),
      k = 1
    )
    idx <- as.integer(nn$nn.index[, 1])
    m <- cbind(p, g[idx])
    m[, distance_to_city_km := haversine_km(bplo1, bpla1, longitude, latitude)]
    m[, needs_review := distance_to_city_km > 10]

    ok <- m[distance_to_city_km <= max_distance_km]
    bad <- m[distance_to_city_km > max_distance_km]
    if (nrow(bad) > 0) {
      bad[, unmatched_reason := "nearest_city_over_50km"]
    }

    matched[[i]] <- ok
    unmatched[[i]] <- bad
  }

  list(
    matched = rbindlist(matched, use.names = TRUE, fill = TRUE),
    unmatched = rbindlist(unmatched, use.names = TRUE, fill = TRUE)
  )
}

###############################################################################
# Country metadata
###############################################################################

cat("Reading GeoNames country metadata...\n")
country_info <- fread(
  country_info_file,
  sep = "\t",
  skip = "#ISO",
  header = TRUE,
  fill = TRUE,
  quote = "",
  encoding = "UTF-8"
)[, .(
  iso2 = `#ISO`,
  iso3 = ISO3,
  geonames_country = Country,
  continent = Continent
)]

admin1 <- fread(
  admin1_file,
  sep = "\t",
  header = FALSE,
  fill = TRUE,
  quote = "",
  col.names = c("admin1_full", "admin1_name", "admin1_ascii", "admin1_geonameid"),
  encoding = "UTF-8"
)
admin1[, iso2 := str_extract(admin1_full, "^[A-Z]{2}")]
admin1[, admin1_code := str_replace(admin1_full, "^[A-Z]{2}\\.", "")]
admin1 <- admin1[, .(iso2, admin1_code, admin1_name, admin1_ascii)]

###############################################################################
# Load and classify Discovery/Science people
###############################################################################

cat("Reading cross-verified Discovery/Science people...\n")
raw <- fread(
  raw_file,
  select = c(
    "wikidata_code", "name", "birth", "death", "bplo1", "bpla1",
    "citizenship_1_b", "level1_main_occ", "level2_main_occ",
    "level3_main_occ", "level3_all_occ"
  ),
  showProgress = TRUE
)

people <- raw[
  level1_main_occ == "Discovery/Science" &
    !is.na(birth) & !is.na(bplo1) & !is.na(bpla1)
]
people <- as_tibble(people) %>%
  add_stem_dummy() %>%
  select(
    wikidata_code, name, birth, death, bplo1, bpla1, citizenship_1_b,
    level2_main_occ, level3_main_occ, level3_all_occ, level3_occ, stem
  )

cat("Discovery/Science with birth coordinates:", nrow(people), "\n")

###############################################################################
# Assign birth country from coordinates
###############################################################################

cat("Assigning birth countries from coordinates...\n")
world <- ne_countries(returnclass = "sf") %>%
  select(iso_a3, country_name = name, geometry)

points <- st_as_sf(people, coords = c("bplo1", "bpla1"), crs = 4326, remove = FALSE)
points_country <- st_join(points, world, join = st_within, left = TRUE) %>%
  mutate(iso3 = fix_iso_a3(iso_a3, country_name)) %>%
  st_drop_geometry() %>%
  as.data.table()

points_country <- merge(
  points_country,
  country_info,
  by = "iso3",
  all.x = TRUE,
  sort = FALSE
)

target_people <- points_country[
  iso3 == "USA" | continent == "EU"
]
target_people <- target_people[
  !is.na(iso2) & !is.na(bplo1) & !is.na(bpla1)
]

cat("US or Europe Discovery/Science with coordinates:", nrow(target_people), "\n")
cat("  USA:", nrow(target_people[iso3 == "USA"]), "\n")
cat("  Europe:", nrow(target_people[continent == "EU"]), "\n")

target_iso2 <- sort(unique(target_people$iso2))

###############################################################################
# Load GeoNames populated places for target countries
###############################################################################

cat("Reading GeoNames populated places for target countries...\n")
geon_cols <- c(
  "geonameid", "city", "city_ascii", "alternatenames", "latitude",
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
)

geon <- geon[
  feature_class == "P" &
    feature_code %in% c("PPL", "PPLA", "PPLA2", "PPLA3", "PPLA4", "PPLC", "PPLG", "PPLS") &
    country_code %in% target_iso2 &
    !is.na(latitude) & !is.na(longitude)
]
geon[, population := as.numeric(population)]
geon <- merge(
  geon,
  admin1,
  by.x = c("country_code", "admin1_code"),
  by.y = c("iso2", "admin1_code"),
  all.x = TRUE,
  sort = FALSE
)

cat("GeoNames populated-place candidates:", nrow(geon), "\n")

###############################################################################
# Nearest populated place within country
###############################################################################

cat("Assigning nearest populated place within country...\n")
target_people <- target_people[, .(
  wikidata_code, name, birth, death, bplo1, bpla1, citizenship_1_b,
  level2_main_occ, level3_main_occ, level3_all_occ, level3_occ, stem,
  birth_country = country_name, iso2, iso3, continent, geonames_country
)]

assigned <- assign_nearest_city(target_people, geon, max_distance_km = 50)
matched <- assigned$matched
unmatched <- assigned$unmatched

cat("Matched to city:", nrow(matched), "\n")
cat("Unmatched:", nrow(unmatched), "\n")
cat("Needs review (>10km):", nrow(matched[needs_review == TRUE]), "\n")

###############################################################################
# Aggregate city-year
###############################################################################

city_year <- matched[, .(
  n_scientists = .N,
  n_stem = sum(stem == 1L, na.rm = TRUE),
  n_nonstem = sum(stem != 1L | is.na(stem), na.rm = TRUE),
  mean_distance_to_city_km = mean(distance_to_city_km, na.rm = TRUE),
  max_distance_to_city_km = max(distance_to_city_km, na.rm = TRUE),
  n_needs_review = sum(needs_review == TRUE, na.rm = TRUE)
), by = .(
  city_geonameid = geonameid,
  city,
  city_ascii,
  country = geonames_country,
  iso2,
  iso3,
  continent,
  admin1_code,
  admin1_name,
  lat_city = latitude,
  lon_city = longitude,
  year = birth
)]

setorder(city_year, iso3, city_ascii, year)

###############################################################################
# QC and export
###############################################################################

cat("\n=== QC ===\n")
cat("Aggregated rows:", nrow(city_year), "\n")
cat("Unique cities:", uniqueN(city_year$city_geonameid), "\n")
cat("Total scientists in aggregate:", sum(city_year$n_scientists), "\n")
cat("Total matched micro rows:", nrow(matched), "\n")
cat("All rows are USA or Europe:",
    all(city_year$iso3 == "USA" | city_year$continent == "EU"), "\n")

stopifnot(nrow(city_year) > 0)
stopifnot(!any(is.na(city_year$city_geonameid)))
stopifnot(!any(is.na(city_year$iso3)))
stopifnot(!any(is.na(city_year$year)))
stopifnot(!any(is.na(city_year$lat_city)))
stopifnot(!any(is.na(city_year$lon_city)))
stopifnot(sum(city_year$n_scientists) == nrow(matched))
stopifnot(all(city_year$iso3 == "USA" | city_year$continent == "EU"))

cat("\nTop cities by total Discovery/Science births:\n")
print(
  city_year[, .(n_scientists = sum(n_scientists), n_stem = sum(n_stem)),
            by = .(city, country, iso3)][order(-n_scientists)][1:20]
)

cat("\nSpot checks:\n")
spot <- city_year[
  city_ascii %in% c(
    "New York City", "Boston", "Philadelphia", "London", "Paris",
    "Berlin", "Vienna", "Rome"
  )
][, .(n_scientists = sum(n_scientists), n_stem = sum(n_stem)),
  by = .(city_ascii, country, iso3)][order(city_ascii)]
print(spot)

fwrite(city_year, out_file)
writexl::write_xlsx(as.data.frame(city_year), out_xlsx)
fwrite(unmatched, unmatched_file)

cat("\nWrote:\n")
cat("  ", out_file, "\n", sep = "")
cat("  ", out_xlsx, "\n", sep = "")
cat("  ", unmatched_file, "\n", sep = "")

final_time <- Sys.time() - initial_time
cat("\nRan in", round(as.numeric(final_time, units = "mins"), 2), "minutes.\n")
