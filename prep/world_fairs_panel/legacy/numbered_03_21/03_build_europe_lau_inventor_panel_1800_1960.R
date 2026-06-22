###############################################################################
# Build European LAU-year inventor/scientist panel using GISCO boundaries.
#
# This replaces the Europe person-to-city nearest-neighbor assignment with
# point-in-polygon assignment to modern GISCO LAU polygons.
#
# Inputs:
#   input/cross-verified-database.csv
#   raw/stadester/non_metro_adjusted_json/stadester.json
#   raw/stadester/non_metro_adjusted_json/stadester_ghsl.json
#
# Outputs:
#   output/discovery_science_lau_year_europe.csv
#   output/discovery_science_lau_year_europe_unmatched.csv
#   output/europe_lau_boundaries_2024.gpkg
#   output/europe_lau_population_stadester_1700_1960.csv
#   output/europe_lau_population_stadester_matches.csv
#   output/europe_lau_inventor_panel_1700_1960.csv
#   output/europe_lau_inventor_panel_1700_1960_balanced_rates.csv
#   output/europe_lau_inventor_panel_1700_1960_qc.csv
#
# Run from prep/world_fairs_panel/:
#   Rscript 03_build_europe_lau_inventor_panel_1800_1960.R
###############################################################################

rm(list = ls());gc()

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(giscoR)
  library(jsonlite)
  library(readr)
  library(sf)
  library(stringr)
  library(stringi)
})

initial_time <- Sys.time()
sf::sf_use_s2(FALSE)

repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = NA), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "prep", "stem_labels.R"))
source(file.path(repo_root, "paths.R"))

###############################################################################
# Paths and constants
###############################################################################

raw_file <- file.path(DATA_INPUT, "cross-verified-database.csv")
stadester_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "stadester", "non_metro_adjusted_json")
stadester_file <- file.path(stadester_dir, "stadester.json")
stadester_ghsl_file <- file.path(stadester_dir, "stadester_ghsl.json")
gisco_cache_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "gisco")

out_lau_gpkg <- file.path(DATA_OUTPUT, "europe_lau_boundaries_2024.gpkg")
out_sci_lau <- file.path(DATA_OUTPUT, "discovery_science_lau_year_europe.csv")
out_sci_lau_unmatched <- file.path(DATA_OUTPUT, "discovery_science_lau_year_europe_unmatched.csv")
out_pop_lau <- file.path(DATA_OUTPUT, "europe_lau_population_stadester_1700_1960.csv")
out_pop_matches <- file.path(DATA_OUTPUT, "europe_lau_population_stadester_matches.csv")
out_panel <- file.path(DATA_OUTPUT, "europe_lau_inventor_panel_1700_1960.csv")
out_panel_balanced_rates <- file.path(DATA_OUTPUT, "europe_lau_inventor_panel_1700_1960_balanced_rates.csv")
out_qc <- file.path(DATA_OUTPUT, "europe_lau_inventor_panel_1700_1960_qc.csv")

years_keep <- 1700L:1960L
gisco_year <- 2024L

dir.create(gisco_cache_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(raw_file))
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

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

first_nonmissing_char <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) NA_character_ else x[1]
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

    out[[i]] <- data.table(
      stadester_source = source_name,
      stadester_key = keys[i],
      stadester_name = as.character(v$name),
      stadester_country = as.character(country),
      stadester_lat = as.numeric(coords[1]),
      stadester_lon = as.numeric(coords[2])
    )
  }

  rbindlist(out, use.names = TRUE, fill = TRUE)
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

interp_no_extrapolate <- function(year, population, years_out) {
  ok <- !is.na(year) & !is.na(population)
  year <- as.integer(year[ok])
  population <- as.numeric(population[ok])

  if (length(unique(year)) == 0) {
    return(rep(NA_real_, length(years_out)))
  }

  by_year <- data.table(year = year, population = population)[
    , .(population = sum(population, na.rm = TRUE)), by = year
  ]
  setorder(by_year, year)

  if (nrow(by_year) == 1) {
    out <- rep(NA_real_, length(years_out))
    out[years_out == by_year$year] <- by_year$population
    return(out)
  }

  approx(
    x = by_year$year,
    y = by_year$population,
    xout = years_out,
    rule = 1,
    ties = "ordered"
  )$y
}

###############################################################################
# GISCO countries and LAU boundaries
###############################################################################

cat("Downloading/loading GISCO Europe countries...\n")
countries_sf <- giscoR::gisco_get_countries(
  year = gisco_year,
  resolution = 20,
  spatialtype = "RG",
  region = "Europe",
  cache_dir = gisco_cache_dir,
  update_cache = FALSE
) %>%
  select(CNTR_ID, CNTR_NAME, NAME_ENGL, ISO3_CODE, geometry) %>%
  st_make_valid()

europe_iso2 <- sort(unique(countries_sf$CNTR_ID))

cat("Downloading/loading GISCO LAU polygons...\n")
lau_sf <- giscoR::gisco_get_lau(
  year = gisco_year,
  country = europe_iso2,
  cache_dir = gisco_cache_dir,
  update_cache = FALSE,
  verbose = TRUE
) %>%
  select(GISCO_ID, CNTR_CODE, LAU_NAME, POP_2024, POP_DENS_2024, AREA_KM2, YEAR, geometry) %>%
  st_make_valid()

lau_sf <- lau_sf %>%
  inner_join(
    countries_sf %>% st_drop_geometry() %>% select(CNTR_ID, country = NAME_ENGL, iso3 = ISO3_CODE),
    by = c("CNTR_CODE" = "CNTR_ID")
  )

lau_centroids <- lau_sf %>%
  st_point_on_surface() %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(lon = X, lat = Y)

lau_meta <- lau_sf %>%
  st_drop_geometry() %>%
  bind_cols(lau_centroids) %>%
  transmute(
    lau_id = GISCO_ID,
    iso2 = CNTR_CODE,
    iso3,
    country,
    lau_name = LAU_NAME,
    lau_pop_2024 = POP_2024,
    lau_area_km2 = AREA_KM2,
    lat_lau = lat,
    lon_lau = lon
  )

if (file.exists(out_lau_gpkg)) {
  unlink(out_lau_gpkg)
}
st_write(lau_sf, out_lau_gpkg, quiet = TRUE)

cat("GISCO Europe countries:", nrow(countries_sf), "\n")
cat("GISCO LAU polygons:", nrow(lau_sf), "\n")

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
    !is.na(birth) & birth %in% years_keep &
    !is.na(bplo1) & !is.na(bpla1)
]

people <- as_tibble(people) %>%
  add_stem_dummy() %>%
  select(
    wikidata_code, name, birth, death, bplo1, bpla1, citizenship_1_b,
    level2_main_occ, level3_main_occ, level3_all_occ, level3_occ, stem
  )

cat("Discovery/Science with birth coordinates, 1700-1960:", nrow(people), "\n")

people_points <- st_as_sf(people, coords = c("bplo1", "bpla1"), crs = 4326, remove = FALSE)

cat("Assigning birth countries from GISCO country polygons...\n")
people_europe <- st_join(
  people_points,
  countries_sf %>% select(CNTR_ID, birth_country = NAME_ENGL, birth_iso3 = ISO3_CODE),
  join = st_within,
  left = TRUE
) %>%
  filter(!is.na(CNTR_ID))

cat("European Discovery/Science with GISCO country match:", nrow(people_europe), "\n")

cat("Assigning European births to GISCO LAU polygons...\n")
people_lau_join <- st_join(
  people_europe,
  lau_sf %>% select(GISCO_ID, CNTR_CODE, LAU_NAME),
  join = st_within,
  left = TRUE
) %>%
  st_drop_geometry() %>%
  as.data.table()

people_lau <- people_lau_join[
  !is.na(GISCO_ID),
  .SD[1],
  by = .(wikidata_code, birth)
]

people_unmatched <- people_lau_join[
  is.na(GISCO_ID),
  .(
    wikidata_code, name, birth, death, bplo1, bpla1, citizenship_1_b,
    level2_main_occ, level3_main_occ, level3_all_occ, level3_occ, stem,
    birth_country, birth_iso3, unmatched_reason = "no_lau_polygon_containing_birth_point"
  )
]

cat("European people assigned to LAU:", nrow(people_lau), "\n")
cat("European people unmatched to LAU:", nrow(people_unmatched), "\n")

sci_lau_year <- people_lau[, .(
  n_inventors = .N,
  n_stem = sum(stem == 1, na.rm = TRUE),
  n_nonstem = sum(stem != 1 | is.na(stem), na.rm = TRUE)
), by = .(
  lau_id = GISCO_ID,
  iso2 = CNTR_CODE,
  lau_name = LAU_NAME,
  year = as.integer(birth)
)]

sci_lau_year <- sci_lau_year %>%
  left_join(lau_meta %>% select(lau_id, iso3, country, lat_lau, lon_lau), by = "lau_id") %>%
  as.data.table()

setorder(sci_lau_year, iso3, lau_name, year)
fwrite(sci_lau_year, out_sci_lau)
fwrite(people_unmatched, out_sci_lau_unmatched)

###############################################################################
# Assign Stadester population points to LAU polygons
###############################################################################

cat("Reading and deduplicating Stadester city points...\n")
stad_points <- rbindlist(
  list(
    json_city_rows(stadester_file, "stadester"),
    json_city_rows(stadester_ghsl_file, "stadester_ghsl")
  ),
  use.names = TRUE,
  fill = TRUE
)

stad_points <- stad_points[
  !is.na(stadester_lat) & !is.na(stadester_lon)
]
stad_points[, source_priority := fifelse(stadester_source == "stadester", 1L, 2L)]
stad_points[, dedupe_key := paste(
  norm_text(stadester_name),
  norm_text(stadester_country),
  round(stadester_lat, 3),
  round(stadester_lon, 3),
  sep = "|"
)]
setorder(stad_points, dedupe_key, source_priority)
stad_points <- stad_points[, .SD[1], by = dedupe_key]

stad_sf <- st_as_sf(
  stad_points,
  coords = c("stadester_lon", "stadester_lat"),
  crs = 4326,
  remove = FALSE
)

cat("Assigning Stadester population points to LAU polygons...\n")
stad_lau <- st_join(
  stad_sf,
  lau_sf %>% select(GISCO_ID, CNTR_CODE, LAU_NAME),
  join = st_within,
  left = TRUE
) %>%
  st_drop_geometry() %>%
  as.data.table()

stad_lau_matched <- stad_lau[!is.na(GISCO_ID)]
stad_lau_matches <- stad_lau[, .(
  stadester_source,
  stadester_key,
  stadester_name,
  stadester_country,
  stadester_lat,
  stadester_lon,
  lau_id = GISCO_ID,
  lau_iso2 = CNTR_CODE,
  lau_name = LAU_NAME,
  match_status = fifelse(is.na(GISCO_ID), "unmatched_no_lau_polygon", "matched_lau_polygon"),
  match_method = fifelse(is.na(GISCO_ID), NA_character_, "stadester_point_within_lau_polygon")
)]
fwrite(stad_lau_matches, out_pop_matches)

cat("Stadester city points:", nrow(stad_lau), "\n")
cat("Stadester city points assigned to LAU:", nrow(stad_lau_matched), "\n")

pop_primary <- json_population_long(
  stadester_file,
  "stadester",
  stad_lau_matched[stadester_source == "stadester"]$stadester_key
)
pop_ghsl <- json_population_long(
  stadester_ghsl_file,
  "stadester_ghsl",
  stad_lau_matched[stadester_source == "stadester_ghsl"]$stadester_key
)
pop_long <- rbindlist(list(pop_primary, pop_ghsl), use.names = TRUE, fill = TRUE)

pop_long <- merge(
  pop_long,
  stad_lau_matched[, .(stadester_source, stadester_key, lau_id = GISCO_ID)],
  by = c("stadester_source", "stadester_key"),
  all.x = FALSE,
  all.y = FALSE
)

pop_lau_observed <- pop_long[, .(
  population_stadester_lau = sum(city_population_stadester, na.rm = TRUE),
  n_stadester_cities_in_lau = uniqueN(stadester_key)
), by = .(lau_id, year)]

lau_skeleton <- data.table(
  lau_id = rep(unique(pop_lau_observed$lau_id), each = length(years_keep)),
  year = rep(years_keep, times = uniqueN(pop_lau_observed$lau_id))
)

pop_lau <- merge(lau_skeleton, pop_lau_observed, by = c("lau_id", "year"), all.x = TRUE)
pop_lau[, population_stadester_lau_interp := interp_no_extrapolate(
  year = year[!is.na(population_stadester_lau)],
  population = population_stadester_lau[!is.na(population_stadester_lau)],
  years_out = year
), by = lau_id]

pop_lau[, population_interp_status := fcase(
  !is.na(population_stadester_lau), "observed_stadester_year",
  is.na(population_stadester_lau) & !is.na(population_stadester_lau_interp),
  "linear_interpolation_between_observed_years",
  default = "missing_no_extrapolation"
)]

pop_lau <- pop_lau %>%
  left_join(lau_meta, by = "lau_id") %>%
  as.data.table()

setorder(pop_lau, iso3, lau_name, year)
fwrite(pop_lau, out_pop_lau)

###############################################################################
# Build Europe LAU inventor panel
###############################################################################

cat("Building Europe LAU inventor panel...\n")
sci_agg <- sci_lau_year[, .(
  n_inventors = sum(n_inventors, na.rm = TRUE),
  n_stem = sum(n_stem, na.rm = TRUE),
  n_nonstem = sum(n_nonstem, na.rm = TRUE)
), by = .(lau_id, year)]

panel_raw <- merge(pop_lau, sci_agg, by = c("lau_id", "year"), all.x = TRUE)
panel_raw[, `:=`(
  n_inventors = fifelse(is.na(n_inventors), 0L, as.integer(n_inventors)),
  n_stem = fifelse(is.na(n_stem), 0L, as.integer(n_stem)),
  n_nonstem = fifelse(is.na(n_nonstem), 0L, as.integer(n_nonstem))
)]

panel_raw[, `:=`(
  any_inventor = as.integer(n_inventors > 0),
  any_stem = as.integer(n_stem > 0),
  log1p_n_inventors = log1p(n_inventors),
  log1p_n_stem = log1p(n_stem),
  inventors_per_100k_pop = fifelse(
    !is.na(population_stadester_lau_interp) & population_stadester_lau_interp > 0,
    1e5 * n_inventors / population_stadester_lau_interp,
    NA_real_
  ),
  stem_per_100k_pop = fifelse(
    !is.na(population_stadester_lau_interp) & population_stadester_lau_interp > 0,
    1e5 * n_stem / population_stadester_lau_interp,
    NA_real_
  ),
  inventors_per_1000_pop = fifelse(
    !is.na(population_stadester_lau_interp) & population_stadester_lau_interp > 0,
    1000 * n_inventors / population_stadester_lau_interp,
    NA_real_
  ),
  stem_per_1000_pop = fifelse(
    !is.na(population_stadester_lau_interp) & population_stadester_lau_interp > 0,
    1000 * n_stem / population_stadester_lau_interp,
    NA_real_
  )
)]

panel <- panel_raw[, .(
  unit_type = "europe_lau",
  unit_id = paste0("GISCO_LAU_", lau_id),
  GEOID = NA_character_,
  lau_id,
  city_geonameid = NA_integer_,
  place_name = lau_name,
  place_name_ascii = lau_name,
  country,
  iso3,
  lat = lat_lau,
  lon = lon_lau,
  year,
  n_inventors,
  n_stem,
  n_nonstem,
  any_inventor,
  any_stem,
  log1p_n_inventors,
  log1p_n_stem,
  population = population_stadester_lau_interp,
  population_original = population_stadester_lau,
  population_source = "stadester_lau",
  population_interp_status,
  inventors_per_100k_pop,
  stem_per_100k_pop,
  inventors_per_1000_pop,
  stem_per_1000_pop,
  match_status = "matched",
  match_method = "point_within_lau_polygon",
  match_distance_km = NA_real_,
  match_needs_review = FALSE,
  source_panel = "europe_lau_inventor_panel_1700_1960"
)]

setorder(panel, unit_type, iso3, place_name_ascii, year)
fwrite(panel, out_panel)

rate_complete_laus <- panel[, .(
  n_years = .N,
  n_nonmissing_rate = sum(!is.na(inventors_per_100k_pop)),
  n_nonmissing_population = sum(!is.na(population))
), by = lau_id][
  n_years == length(years_keep) &
    n_nonmissing_rate == length(years_keep) &
    n_nonmissing_population == length(years_keep),
  lau_id
]

panel_balanced_rates <- panel[lau_id %in% rate_complete_laus]
setorder(panel_balanced_rates, unit_type, iso3, place_name_ascii, year)
fwrite(panel_balanced_rates, out_panel_balanced_rates)

###############################################################################
# QC
###############################################################################

qc <- rbindlist(list(
  data.table(
    panel = "europe_lau_full",
    rows = nrow(panel),
    units = uniqueN(panel$unit_id),
    years_min = min(panel$year),
    years_max = max(panel$year),
    balanced = all(panel[, .N, by = unit_id]$N == length(years_keep)),
    duplicate_unit_year = nrow(panel[, .N, by = .(unit_id, year)][N > 1]),
    missing_population = sum(is.na(panel$population)),
    missing_rate = sum(is.na(panel$inventors_per_100k_pop)),
    total_inventors = sum(panel$n_inventors, na.rm = TRUE),
    total_stem = sum(panel$n_stem, na.rm = TRUE)
  ),
  data.table(
    panel = "europe_lau_balanced_rates",
    rows = nrow(panel_balanced_rates),
    units = uniqueN(panel_balanced_rates$unit_id),
    years_min = min(panel_balanced_rates$year),
    years_max = max(panel_balanced_rates$year),
    balanced = all(panel_balanced_rates[, .N, by = unit_id]$N == length(years_keep)),
    duplicate_unit_year = nrow(panel_balanced_rates[, .N, by = .(unit_id, year)][N > 1]),
    missing_population = sum(is.na(panel_balanced_rates$population)),
    missing_rate = sum(is.na(panel_balanced_rates$inventors_per_100k_pop)),
    total_inventors = sum(panel_balanced_rates$n_inventors, na.rm = TRUE),
    total_stem = sum(panel_balanced_rates$n_stem, na.rm = TRUE)
  )
), use.names = TRUE, fill = TRUE)

qc_extra <- rbindlist(list(
  data.table(section = "people", metric = "europe_people_country_match", value = nrow(people_europe)),
  data.table(section = "people", metric = "europe_people_lau_match", value = nrow(people_lau)),
  data.table(section = "people", metric = "europe_people_lau_unmatched", value = nrow(people_unmatched)),
  data.table(section = "stadester", metric = "stadester_points", value = nrow(stad_lau)),
  data.table(section = "stadester", metric = "stadester_points_lau_match", value = nrow(stad_lau_matched)),
  data.table(section = "gisco", metric = "europe_country_polygons", value = nrow(countries_sf)),
  data.table(section = "gisco", metric = "europe_lau_polygons", value = nrow(lau_sf))
), use.names = TRUE, fill = TRUE)

fwrite(qc, out_qc)
fwrite(qc_extra, file.path(DATA_OUTPUT, "europe_lau_inventor_panel_1700_1960_qc_extra.csv"))

cat("\n=== Europe LAU inventor panel complete ===\n")
print(qc)
cat("\nAdditional QC:\n")
print(qc_extra)
cat(
  "Done. Elapsed minutes:",
  round(difftime(Sys.time(), initial_time, units = "mins"), 1),
  "\n"
)
