###############################################################################
# Build annual city population series for European scientist cities, 1800-1960.
#
# Source cities: DATA_OUTPUT/discovery_science_city_year_us_europe.csv
# Population source: Stadester non-metro adjusted JSONs downloaded from Zenodo.
#
# Outputs:
#   output/europe_city_population_stadester_1800_1960.csv
#   output/europe_city_population_stadester_1800_1960_matched.csv
#   output/europe_city_population_stadester_matches.csv
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(FNN)
  library(jsonlite)
  library(stringi)
})

initial_time <- Sys.time()

repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = NA), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "paths.R"))

###############################################################################
# Paths and constants
###############################################################################

city_file <- file.path(DATA_OUTPUT, "discovery_science_city_year_us_europe.csv")
stadester_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "stadester", "non_metro_adjusted_json")
stadester_file <- file.path(stadester_dir, "stadester.json")
stadester_ghsl_file <- file.path(stadester_dir, "stadester_ghsl.json")

out_full <- file.path(DATA_OUTPUT, "europe_city_population_stadester_1800_1960.csv")
out_matched <- file.path(DATA_OUTPUT, "europe_city_population_stadester_1800_1960_matched.csv")
out_matches <- file.path(DATA_OUTPUT, "europe_city_population_stadester_matches.csv")

years_keep <- 1800L:1960L
max_nearest_km <- 10

stopifnot(file.exists(city_file))
stopifnot(file.exists(stadester_file))
stopifnot(file.exists(stadester_ghsl_file))

###############################################################################
# Helpers
###############################################################################

norm_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

harmonize_country <- function(x) {
  x <- as.character(x)
  fcase(
    x == "Czechia", "Czech Republic",
    x == "The Netherlands", "Netherlands",
    x == "Bosnia and Herzegovina", "Bosnia and Herzegovina",
    x == "North Macedonia", "North Macedonia",
    default = x
  )
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371.0088
  to_rad <- pi / 180
  p1 <- lat1 * to_rad
  p2 <- lat2 * to_rad
  dp <- (lat2 - lat1) * to_rad
  dl <- (lon2 - lon1) * to_rad
  a <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

extract_country_from_ghsl_key <- function(key) {
  key <- sub("^ghsl-", "", key)
  sub("^.*-", "", key)
}

json_city_rows <- function(path, source_name) {
  cat("Reading ", basename(path), "...\n", sep = "")
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  keys <- names(raw)
  out <- vector("list", length(raw))

  for (i in seq_along(raw)) {
    v <- raw[[i]]
    pop <- v$population
    if (is.null(pop) || length(pop) == 0) next

    pop_years <- suppressWarnings(as.integer(names(pop)))
    keep <- !is.na(pop_years) & pop_years %in% years_keep
    if (!any(keep)) next

    coords <- v$coords
    if (is.null(coords) || length(coords) < 2 || any(is.na(coords[1:2]))) next

    country <- v$country
    if (is.null(country) || !nzchar(country)) {
      country <- extract_country_from_ghsl_key(keys[i])
    }

    other_names <- v$other_names
    if (is.null(other_names)) other_names <- character(0)
    all_names <- unique(c(v$name, other_names))
    all_names <- all_names[!is.na(all_names) & nzchar(all_names)]

    out[[i]] <- data.table(
      stadester_source = source_name,
      stadester_key = keys[i],
      stadester_name = as.character(v$name),
      stadester_country = as.character(country),
      stadester_country_harmonized = harmonize_country(country),
      stadester_lat = as.numeric(coords[1]),
      stadester_lon = as.numeric(coords[2]),
      stadester_type = as.character(ifelse(is.null(v$type), NA_character_, v$type)),
      stadester_alt_name = all_names
    )
  }

  dt <- rbindlist(out, use.names = TRUE, fill = TRUE)
  dt[, stadester_alt_name_norm := norm_text(stadester_alt_name)]
  dt[, stadester_name_norm := norm_text(stadester_name)]
  dt
}

json_population_long <- function(path, source_name, keep_keys) {
  cat("Reading population series from ", basename(path), "...\n", sep = "")
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  keep_keys <- intersect(keep_keys, names(raw))
  out <- vector("list", length(keep_keys))

  for (i in seq_along(keep_keys)) {
    key <- keep_keys[i]
    pop <- raw[[key]]$population
    if (is.null(pop) || length(pop) == 0) next

    y <- suppressWarnings(as.integer(names(pop)))
    val <- suppressWarnings(as.numeric(unlist(pop, use.names = FALSE)))
    keep <- !is.na(y) & y %in% years_keep
    if (!any(keep)) next

    out[[i]] <- data.table(
      stadester_source = source_name,
      stadester_key = key,
      year = y[keep],
      city_population_stadester = val[keep]
    )
  }

  rbindlist(out, use.names = TRUE, fill = TRUE)
}

dedupe_stadester_cities <- function(dt) {
  # Name rows are repeated for aliases. Keep a city-level table for spatial match.
  unique(dt[, .(
    stadester_source, stadester_key, stadester_name, stadester_country,
    stadester_country_harmonized, stadester_lat, stadester_lon, stadester_type
  )])
}

match_stadester <- function(cities, stad_names, stad_city, source_name, already_matched) {
  remaining <- cities[!city_geonameid %in% already_matched]
  if (nrow(remaining) == 0 || nrow(stad_city) == 0) {
    return(data.table())
  }

  cat("Matching ", nrow(remaining), " cities against ", source_name, "...\n", sep = "")

  # 1. Exact name + country, using aliases when present.
  exact_candidates <- merge(
    remaining[, .(
      city_geonameid, city_norm, city_ascii_norm, country_harmonized,
      lat_city, lon_city
    )],
    stad_names[, .(
      stadester_source, stadester_key, stadester_name, stadester_country,
      stadester_country_harmonized, stadester_lat, stadester_lon, stadester_type,
      stadester_alt_name, stadester_alt_name_norm
    )],
    by.x = c("country_harmonized", "city_norm"),
    by.y = c("stadester_country_harmonized", "stadester_alt_name_norm"),
    allow.cartesian = TRUE
  )

  exact_candidates_ascii <- merge(
    remaining[, .(
      city_geonameid, city_norm = city_ascii_norm, city_ascii_norm,
      country_harmonized, lat_city, lon_city
    )],
    stad_names[, .(
      stadester_source, stadester_key, stadester_name, stadester_country,
      stadester_country_harmonized, stadester_lat, stadester_lon, stadester_type,
      stadester_alt_name, stadester_alt_name_norm
    )],
    by.x = c("country_harmonized", "city_norm"),
    by.y = c("stadester_country_harmonized", "stadester_alt_name_norm"),
    allow.cartesian = TRUE
  )

  exact_candidates <- rbindlist(
    list(exact_candidates, exact_candidates_ascii),
    use.names = TRUE,
    fill = TRUE
  )
  if (nrow(exact_candidates) > 0) {
    exact_candidates[, stadester_country_harmonized := country_harmonized]
  }

  exact_matches <- data.table()
  if (nrow(exact_candidates) > 0) {
    exact_candidates[, match_distance_km := haversine_km(
      lat_city, lon_city, stadester_lat, stadester_lon
    )]
    setorder(exact_candidates, city_geonameid, match_distance_km)
    exact_matches <- exact_candidates[, .SD[1], by = city_geonameid]
    exact_matches <- exact_matches[match_distance_km <= max_nearest_km]
    exact_matches[, stadester_name_norm_check := norm_text(stadester_name)]
    exact_matches[, `:=`(
      match_status = "matched",
      match_method = "exact_name_country",
      match_needs_review = match_distance_km > 5 |
        (stadester_name_norm_check != city_norm &
           stadester_name_norm_check != city_ascii_norm)
    )]
    exact_matches[, stadester_name_norm_check := NULL]
  }

  matched_ids <- exact_matches$city_geonameid

  # 2. Nearest within country for remaining cities, thresholded at 10km.
  remaining2 <- remaining[!city_geonameid %in% matched_ids]
  nearest_matches <- vector("list", 0)
  if (nrow(remaining2) > 0) {
    countries <- intersect(unique(remaining2$country_harmonized),
                           unique(stad_city$stadester_country_harmonized))
    nearest_matches <- vector("list", length(countries))
    for (i in seq_along(countries)) {
      cc <- countries[i]
      p <- remaining2[country_harmonized == cc]
      s <- stad_city[stadester_country_harmonized == cc]
      if (nrow(p) == 0 || nrow(s) == 0) next

      k <- min(10L, nrow(s))
      nn <- FNN::get.knnx(
        data = as.matrix(s[, .(stadester_lon, stadester_lat)]),
        query = as.matrix(p[, .(lon_city, lat_city)]),
        k = k
      )

      out <- vector("list", nrow(p))
      for (j in seq_len(nrow(p))) {
        idx <- nn$nn.index[j, ]
        cands <- s[idx]
        cands[, match_distance_km := haversine_km(
          p$lat_city[j], p$lon_city[j], stadester_lat, stadester_lon
        )]
        setorder(cands, match_distance_km)
        best <- cands[1]
        if (best$match_distance_km <= max_nearest_km) {
          out[[j]] <- cbind(
            p[j, .(city_geonameid, city_norm, city_ascii_norm, country_harmonized,
                   lat_city, lon_city)],
            best
          )
        }
      }
      nearest_matches[[i]] <- rbindlist(out, use.names = TRUE, fill = TRUE)
    }
  }
  nearest_matches <- rbindlist(nearest_matches, use.names = TRUE, fill = TRUE)
  if (nrow(nearest_matches) > 0) {
    nearest_matches[, `:=`(
      stadester_alt_name = NA_character_,
      stadester_alt_name_norm = NA_character_,
      match_status = "matched",
      match_method = "nearest_country_10km",
      match_needs_review = match_distance_km > 5
    )]
  }

  keep_cols <- c(
    "city_geonameid", "stadester_source", "stadester_key", "stadester_name",
    "stadester_country", "stadester_country_harmonized", "stadester_lat",
    "stadester_lon", "stadester_type", "match_status", "match_method",
    "match_distance_km", "match_needs_review"
  )

  for (col in setdiff(keep_cols, names(exact_matches))) {
    exact_matches[, (col) := NA]
  }
  for (col in setdiff(keep_cols, names(nearest_matches))) {
    nearest_matches[, (col) := NA]
  }

  rbindlist(
    list(exact_matches[, ..keep_cols], nearest_matches[, ..keep_cols]),
    use.names = TRUE,
    fill = TRUE
  )
}

###############################################################################
# Load scientist city universe
###############################################################################

cat("Reading European scientist city universe...\n")
cities <- fread(city_file)
cities <- unique(cities[iso3 != "USA", .(
  city_geonameid, city, city_ascii, country, iso3, lat_city, lon_city
)])
cities[, country_harmonized := harmonize_country(country)]
cities[, city_norm := norm_text(city)]
cities[, city_ascii_norm := norm_text(city_ascii)]

cat("European cities:", nrow(cities), "\n")
cat("Countries:", uniqueN(cities$iso3), "\n")

###############################################################################
# Load and match Stadester sources
###############################################################################

stad_names <- json_city_rows(stadester_file, "stadester")
stad_city <- dedupe_stadester_cities(stad_names)

matches_primary <- match_stadester(
  cities = cities,
  stad_names = stad_names,
  stad_city = stad_city,
  source_name = "stadester",
  already_matched = integer(0)
)

ghsl_names <- json_city_rows(stadester_ghsl_file, "stadester_ghsl")
ghsl_city <- dedupe_stadester_cities(ghsl_names)

matches_ghsl <- match_stadester(
  cities = cities,
  stad_names = ghsl_names,
  stad_city = ghsl_city,
  source_name = "stadester_ghsl",
  already_matched = matches_primary$city_geonameid
)

matches <- rbindlist(list(matches_primary, matches_ghsl), use.names = TRUE, fill = TRUE)
setorder(matches, city_geonameid, stadester_source)
matches <- matches[, .SD[1], by = city_geonameid]

matches_full <- merge(cities, matches, by = "city_geonameid", all.x = TRUE, sort = FALSE)
matches_full[is.na(match_status), `:=`(
  match_status = "unmatched",
  match_method = "unmatched",
  match_needs_review = NA
)]

fwrite(matches_full, out_matches)

###############################################################################
# Build population long table only for matched keys
###############################################################################

matched_keys_primary <- unique(matches_full[
  match_status == "matched" & stadester_source == "stadester",
  stadester_key
])
matched_keys_ghsl <- unique(matches_full[
  match_status == "matched" & stadester_source == "stadester_ghsl",
  stadester_key
])

pop_primary <- json_population_long(stadester_file, "stadester", matched_keys_primary)
pop_ghsl <- json_population_long(stadester_ghsl_file, "stadester_ghsl", matched_keys_ghsl)
pop_long <- rbindlist(list(pop_primary, pop_ghsl), use.names = TRUE, fill = TRUE)

###############################################################################
# Expand full panel and merge population
###############################################################################

cat("Building full city-year panel...\n")
panel <- CJ(city_geonameid = cities$city_geonameid, year = years_keep, unique = TRUE)
panel <- merge(panel, matches_full, by = "city_geonameid", all.x = TRUE, sort = FALSE)
panel <- merge(
  panel,
  pop_long,
  by = c("stadester_source", "stadester_key", "year"),
  all.x = TRUE,
  sort = FALSE
)

setorder(panel, city_geonameid, year)
panel[, city_population_stadester_interp := {
  observed <- !is.na(city_population_stadester)
  if (sum(observed) < 2) {
    city_population_stadester
  } else {
    approx(
      x = year[observed],
      y = city_population_stadester[observed],
      xout = year,
      rule = 1
    )$y
  }
}, by = city_geonameid]
panel[, city_population_stadester_interp_status := fcase(
  !is.na(city_population_stadester), "observed",
  is.na(city_population_stadester) & !is.na(city_population_stadester_interp), "interpolated",
  default = "missing"
)]

out_cols <- c(
  "city_geonameid", "city", "city_ascii", "country", "iso3", "lat_city",
  "lon_city", "year", "city_population_stadester",
  "city_population_stadester_interp",
  "city_population_stadester_interp_status", "match_status", "match_method",
  "stadester_source", "stadester_key", "stadester_name", "stadester_country",
  "stadester_lat", "stadester_lon", "match_distance_km", "match_needs_review"
)
panel <- panel[, ..out_cols]
setorder(panel, iso3, city_ascii, year)

panel_matched <- panel[!is.na(city_population_stadester_interp)]

fwrite(panel, out_full)
fwrite(panel_matched, out_matched)

###############################################################################
# QC
###############################################################################

expected_rows <- nrow(cities) * length(years_keep)
cat("\n=== QC ===\n")
cat("Expected full panel rows:", expected_rows, "\n")
cat("Actual full panel rows:", nrow(panel), "\n")
cat("Matched-only rows:", nrow(panel_matched), "\n")
cat("Cities matched:", uniqueN(matches_full[match_status == "matched"]$city_geonameid), "\n")
cat("Cities unmatched:", uniqueN(matches_full[match_status != "matched"]$city_geonameid), "\n")
cat("City-years with population:", sum(!is.na(panel$city_population_stadester)), "\n")
cat(
  "City-years with interpolated population:",
  sum(panel$city_population_stadester_interp_status == "interpolated"),
  "\n"
)
cat(
  "City-years remaining missing after interpolation:",
  sum(is.na(panel$city_population_stadester_interp)),
  "\n"
)
cat("Year range:", min(panel$year), "-", max(panel$year), "\n")

stopifnot(nrow(panel) == expected_rows)
stopifnot(identical(sort(unique(panel$year)), years_keep))
stopifnot(all(!is.na(panel_matched$city_population_stadester_interp)))
stopifnot(all(
  panel[city_population_stadester_interp_status == "observed",
        city_population_stadester_interp == city_population_stadester]
))
obs_bounds <- panel[
  !is.na(city_population_stadester),
  .(first_obs_year = min(year), last_obs_year = max(year)),
  by = city_geonameid
]
interp_bounds <- panel[
  !is.na(city_population_stadester_interp),
  .(first_interp_year = min(year), last_interp_year = max(year)),
  by = city_geonameid
]
bounds_check <- merge(interp_bounds, obs_bounds, by = "city_geonameid")
stopifnot(all(bounds_check$first_interp_year >= bounds_check$first_obs_year))
stopifnot(all(bounds_check$last_interp_year <= bounds_check$last_obs_year))

cat("\nMatch method distribution:\n")
print(matches_full[, .N, by = .(match_status, match_method, stadester_source)][order(-N)])

cat("\nCoverage by country (top 25 by source cities):\n")
coverage <- matches_full[, .(
  source_cities = .N,
  matched_cities = sum(match_status == "matched"),
  matched_share = round(100 * mean(match_status == "matched"), 2)
), by = .(iso3, country)][order(-source_cities)]
print(coverage[1:min(25, nrow(coverage))])

cat("\nPotentially risky matches (distance 5-10km or exact names far from coordinates):\n")
risky <- matches_full[
  match_status == "matched" &
    !is.na(match_distance_km) &
    match_distance_km > 5,
  .(city, country, stadester_name, stadester_country, stadester_source,
    match_method, match_distance_km)
][order(-match_distance_km)]
print(risky[1:min(20, nrow(risky))])

cat("\nSpot checks:\n")
spots <- c(
  "London", "Paris", "Berlin", "Vienna", "Rome", "Madrid", "Stockholm",
  "Moscow", "Amsterdam", "Zurich"
)
spot <- panel[
  city_ascii %in% spots & year %in% c(1800L, 1850L, 1900L, 1950L, 1960L),
  .(city, country, year, city_population_stadester,
    city_population_stadester_interp, city_population_stadester_interp_status,
    stadester_name, stadester_country, match_method, stadester_source,
    match_distance_km)
][order(city, year)]
print(spot)

cat("\nWrote:\n")
cat("  ", out_full, "\n", sep = "")
cat("  ", out_matched, "\n", sep = "")
cat("  ", out_matches, "\n", sep = "")

final_time <- Sys.time() - initial_time
cat("\nRan in", round(as.numeric(final_time, units = "mins"), 2), "minutes.\n")
