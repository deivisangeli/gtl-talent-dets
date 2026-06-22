# geocode_amws_1955_us.R
#
# Geocode US birthplaces from the cleaned AMWS 1955 file to 2020 county GEOIDs.
#
# Strategy (layered, free, local):
#   1. Exact match (norm city, state) against US Census Gazetteer 2024 Places.
#   2. Fuzzy match (jw dist < 0.10) against same Gazetteer Places.
#   3. Fall back to GeoNames US dump (feature_class P, populated place / admin).
#   4. Spatial join lat/lon to cb_2020_us_county_20m to assign 2020 GEOID.
#
# Inputs:
#   - prep/output/amws_1955_cleaned.csv (built by aggregate_amws_1955.R)
#   - prep/input/2024_Gaz_place_national.txt (Census Gazetteer; auto-downloaded)
#   - prep/input/geonames_US.txt (GeoNames US; auto-downloaded)
#   - tigris cache: cb_2020_us_county_20m.shp
#
# Outputs:
#   - prep/output/amws_1955_us_geocoded.csv (per-lineid resolved rows)
#   - prep/output/amws_1955_us_unmatched.csv (rows that need OSM/manual)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(stringdist)
  library(sf)
})
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

in_dir  <- DATA_INPUT
out_dir <- AMWS_OUTPUT
dir.create(in_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

gaz_file  <- file.path(in_dir, "2024_Gaz_place_national.txt")
geon_file <- file.path(in_dir, "geonames_US.txt")
county_shp <- file.path(Sys.getenv("LOCALAPPDATA"),
                        "tigris", "tigris", "Cache",
                        "cb_2020_us_county_20m.shp")

# ---- downloads ----
if (!file.exists(gaz_file)) {
  url <- "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2024_Gazetteer/2024_Gaz_place_national.zip"
  zf  <- tempfile(fileext = ".zip")
  cat("downloading Census Gazetteer Places...\n")
  download.file(url, zf, mode = "wb", quiet = TRUE)
  utils::unzip(zf, exdir = in_dir)
  unlink(zf)
}
if (!file.exists(geon_file)) {
  url <- "https://download.geonames.org/export/dump/US.zip"
  zf  <- tempfile(fileext = ".zip")
  cat("downloading GeoNames US...\n")
  download.file(url, zf, mode = "wb", quiet = TRUE)
  tmpd <- tempfile(); dir.create(tmpd)
  utils::unzip(zf, exdir = tmpd)
  file.rename(file.path(tmpd, "US.txt"), geon_file)
  unlink(zf); unlink(tmpd, recursive = TRUE)
}
stopifnot(file.exists(county_shp))

# ---- load gazetteer ----
gaz <- read_tsv(gaz_file, show_col_types = FALSE,
                locale = locale(encoding = "UTF-8")) |>
  rename_with(~ trimws(.x)) |>
  transmute(state = USPS,
            geoid_place = GEOID,
            name        = NAME,
            lsad        = LSAD,
            lat         = INTPTLAT,
            lon         = INTPTLONG)
cat("gazetteer places:", nrow(gaz), "\n")

# ---- load geonames (only populated places + admin) ----
geon_cols <- c("geonameid","name","asciiname","alternatenames","latitude",
               "longitude","feature_class","feature_code","country","cc2",
               "admin1","admin2","admin3","admin4","population","elevation",
               "dem","timezone","modification_date")
geon <- read_tsv(geon_file, col_names = geon_cols, show_col_types = FALSE,
                 quote = "", locale = locale(encoding = "UTF-8")) |>
  filter(feature_class %in% c("P","A")) |>
  transmute(state = admin1,
            name  = asciiname,
            altnames = alternatenames,
            lat = latitude, lon = longitude,
            admin2_fips = admin2,
            population = as.integer(population),
            feature_class)
cat("geonames US (P+A):", nrow(geon), "\n")

# ---- normalize ----
strip_punct <- function(x) {
  x <- gsub("\\.", "", x)
  x <- gsub("[[:punct:]]", " ", x)
  trimws(gsub("\\s+", " ", x))
}
expand <- function(x) {
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
norm <- function(x) {
  x <- expand(x)
  x <- strip_suffix(x)
  x <- strip_punct(x)
  tolower(trimws(x))
}

gaz$key  <- norm(gaz$name)
geon$key <- norm(geon$name)

# ---- load cleaned AMWS US rows ----
cleaned <- read_csv(file.path(out_dir, "amws_1955_cleaned.csv"),
                    show_col_types = FALSE)
us <- cleaned |>
  filter(country == "USA", nzchar(city), nzchar(state)) |>
  select(lineid, birthplace_orig, city, state, flag)
cat("US rows with non-empty city+state:", nrow(us), "\n")

pairs <- us |> distinct(city, state) |> mutate(key = norm(city))
cat("distinct (city, state) pairs:", nrow(pairs), "\n")

# ---- 1. exact gazetteer ----
m1 <- pairs |>
  inner_join(gaz |> select(key, state, lat, lon, geoid_place, gaz_name = name),
             by = c("key","state")) |>
  group_by(city, state) |>
  slice(1) |>
  ungroup() |>
  mutate(match_source = "gazetteer_exact")
cat("step 1 (gazetteer exact):", nrow(m1), "\n")

# ---- 2. exact geonames (P/A) ----
remain <- pairs |> anti_join(m1, by = c("city","state"))
m2 <- remain |>
  inner_join(geon |> select(key, state, lat, lon, admin2_fips, geon_name = name,
                            population, feature_class),
             by = c("key","state")) |>
  arrange(city, state, desc(population)) |>
  group_by(city, state) |>
  slice(1) |>
  ungroup() |>
  mutate(match_source = "geonames_exact")
cat("step 2 (geonames exact):", nrow(m2), "\n")

# ---- 3. fuzzy gazetteer + geonames within same state ----
remain <- remain |> anti_join(m2, by = c("city","state"))

fuzzy_one <- function(key, state_in) {
  cands_gaz  <- gaz  |> filter(state == state_in)
  cands_geon <- geon |> filter(state == state_in)
  cands <- bind_rows(
    cands_gaz  |> transmute(key, name, lat, lon, src = "gazetteer_fuzzy",
                            admin2_fips = NA_character_, population = NA_integer_),
    cands_geon |> transmute(key, name, lat, lon, src = "geonames_fuzzy",
                            admin2_fips, population)
  )
  if (nrow(cands) == 0) return(NULL)
  d <- stringdist::stringdist(key, cands$key, method = "jw", p = 0.1)
  i <- which.min(d)
  if (length(i) == 0 || d[i] > 0.10) return(NULL)
  cands[i, ] |> mutate(jw = d[i])
}

if (nrow(remain) > 0) {
  fuzz <- vector("list", nrow(remain))
  for (i in seq_len(nrow(remain))) {
    r <- remain[i, ]
    res <- fuzzy_one(r$key, r$state)
    if (!is.null(res)) {
      fuzz[[i]] <- bind_cols(r, res |> select(-key))
    }
  }
  m3 <- bind_rows(fuzz) |>
    rename(match_source = src) |>
    rename(matched_name = name)
} else {
  m3 <- tibble()
}
cat("step 3 (fuzzy):", nrow(m3), "\n")

# ---- combine matches ----
matched <- bind_rows(
  m1 |> transmute(city, state, key, lat, lon, geoid_place,
                  admin2_fips = NA_character_,
                  matched_name = gaz_name, match_source, jw = 0),
  m2 |> transmute(city, state, key, lat, lon,
                  geoid_place = NA_character_,
                  admin2_fips, matched_name = geon_name, match_source, jw = 0),
  if (nrow(m3)) m3 |> transmute(city, state, key, lat, lon,
                                geoid_place = NA_character_,
                                admin2_fips, matched_name, match_source, jw)
  else tibble()
)

matched$lat <- as.numeric(matched$lat)
matched$lon <- as.numeric(matched$lon)
matched <- matched |> filter(!is.na(lat), !is.na(lon))

# ---- 4. spatial join lat/lon → 2020 county GEOID ----
cat("loading county shapefile...\n")
counties <- st_read(county_shp, quiet = TRUE) |>
  st_transform(4326)

pts <- st_as_sf(matched, coords = c("lon","lat"), crs = 4326, remove = FALSE)
sj  <- st_join(pts, counties |> select(STATEFP, COUNTYFP, GEOID, NAME),
               left = TRUE)
matched$geoid       <- sj$GEOID
matched$county_name <- sj$NAME

# Coastal places (Gazetteer centroids sometimes land in water due to 20m
# coastline simplification). For points that fell outside all county polygons,
# snap to nearest county within the same state.
state_to_fp <- c(
  AL="01",AK="02",AZ="04",AR="05",CA="06",CO="08",CT="09",DE="10",DC="11",
  FL="12",GA="13",HI="15",ID="16",IL="17",IN="18",IA="19",KS="20",KY="21",
  LA="22",ME="23",MD="24",MA="25",MI="26",MN="27",MS="28",MO="29",MT="30",
  NE="31",NV="32",NH="33",NJ="34",NM="35",NY="36",NC="37",ND="38",OH="39",
  OK="40",OR="41",PA="42",RI="44",SC="45",SD="46",TN="47",TX="48",UT="49",
  VT="50",VA="51",WA="53",WV="54",WI="55",WY="56",PR="72")

snap_idx <- which(is.na(matched$geoid))
if (length(snap_idx) > 0) {
  cat("snapping", length(snap_idx), "points to nearest in-state county...\n")
  by_st <- split(snap_idx, matched$state[snap_idx])
  for (st in names(by_st)) {
    st_fp <- state_to_fp[st]
    if (is.na(st_fp)) next
    cs <- counties[counties$STATEFP == st_fp, ]
    if (nrow(cs) == 0) next
    nf <- sf::st_nearest_feature(pts[by_st[[st]], ], cs)
    matched$geoid[by_st[[st]]]       <- cs$GEOID[nf]
    matched$county_name[by_st[[st]]] <- cs$NAME[nf]
  }
}

# fall back to admin2_fips for geonames matches that fell outside any polygon
needs_fb <- is.na(matched$geoid) & !is.na(matched$admin2_fips) &
            nzchar(matched$admin2_fips)
st_lk <- counties |>
  st_drop_geometry() |>
  transmute(state_fp = STATEFP, county_fp = COUNTYFP,
            geoid_fb = GEOID, name_fb = NAME)
state_to_fp <- c(
  AL="01",AK="02",AZ="04",AR="05",CA="06",CO="08",CT="09",DE="10",DC="11",
  FL="12",GA="13",HI="15",ID="16",IL="17",IN="18",IA="19",KS="20",KY="21",
  LA="22",ME="23",MD="24",MA="25",MI="26",MN="27",MS="28",MO="29",MT="30",
  NE="31",NV="32",NH="33",NJ="34",NM="35",NY="36",NC="37",ND="38",OH="39",
  OK="40",OR="41",PA="42",RI="44",SC="45",SD="46",TN="47",TX="48",UT="49",
  VT="50",VA="51",WA="53",WV="54",WI="55",WY="56",PR="72")
fb_idx <- which(needs_fb)
for (i in fb_idx) {
  st_fp <- state_to_fp[matched$state[i]]
  if (is.na(st_fp)) next
  hit <- st_lk |> filter(state_fp == st_fp,
                         county_fp == matched$admin2_fips[i])
  if (nrow(hit) == 1) {
    matched$geoid[i]       <- hit$geoid_fb
    matched$county_name[i] <- hit$name_fb
  }
}

cat("matched with GEOID:", sum(!is.na(matched$geoid)), "\n")

# ---- 5. known-centroid overrides ----
# Census Gazetteer place centroids occasionally fall in the wrong county for
# multi-county places or right across a county line. Hand-curated fixes:
overrides <- tribble(
  ~city,          ~state, ~geoid_new, ~county_new,           ~note,
  "New York",     "NY",   "36061",    "New York",            "NYC default to Manhattan; explicit boroughs handled separately",
  "Manhattan",    "NY",   "36061",    "New York",            "borough → New York County",
  "Bronx",        "NY",   "36005",    "Bronx",               "borough → Bronx County",
  "Brooklyn",     "NY",   "36047",    "Kings",               "borough → Kings County",
  "Queens",       "NY",   "36081",    "Queens",              "borough → Queens County",
  "Staten Island","NY",   "36085",    "Richmond",            "borough → Richmond County",
  "Jersey City",  "NJ",   "34017",    "Hudson",              "centroid lands in Bergen; correct is Hudson",
  "Hoboken",      "NJ",   "34017",    "Hudson",              "Hudson County",
  "San Francisco","CA",   "06075",    "San Francisco",       "Gazetteer centroid offshore (Farallons) snaps to Marin",
)

for (i in seq_len(nrow(overrides))) {
  hit <- matched$city == overrides$city[i] & matched$state == overrides$state[i]
  if (any(hit)) {
    matched$geoid[hit]       <- overrides$geoid_new[i]
    matched$county_name[hit] <- overrides$county_new[i]
  }
}

# ---- 6. Hawaii island disambiguation ----
# Several Hawaiian placenames exist on multiple islands. Look for the island
# name in birthplace_orig to override the centroid match.
hi_islands <- c(
  Kauai     = "15007",
  Oahu      = "15003",
  Maui      = "15009",
  Molokai   = "15009",
  Lanai     = "15009",
  Hawaii    = "15001",
  "Big Island" = "15001"
)
hi_county_names <- c("15007"="Kauai","15003"="Honolulu","15009"="Maui","15001"="Hawaii")
hi_rows <- matched$state == "HI"
if (any(hi_rows)) {
  # we need birthplace_orig for these — re-attach via city/state for those pairs
  hi_pairs <- matched[hi_rows, c("city","state")] |>
    left_join(us |> distinct(city, state, birthplace_orig),
              by = c("city","state"), multiple = "first")
  for (i in seq_len(nrow(hi_pairs))) {
    orig <- hi_pairs$birthplace_orig[i]
    if (is.na(orig)) next
    for (isl in names(hi_islands)) {
      if (grepl(paste0("\\b", isl, "\\b"), orig, ignore.case = TRUE)) {
        ix <- which(hi_rows)[i]
        matched$geoid[ix]       <- hi_islands[[isl]]
        matched$county_name[ix] <- hi_county_names[[ hi_islands[[isl]] ]]
        break
      }
    }
  }
}

# ---- 7. sanity-check agent cleaning: cleaned city vs original first token ----
# Flags entries where the cleaned (city, state) bears no resemblance to the
# leading 30 chars of birthplace_orig (likely upstream Haiku errors).
# cleaning_jw is computed per-row (in result section below), not per (city,state)
norm_for_check <- function(s) {
  s <- tolower(ifelse(is.na(s), "", s))
  s <- gsub("[^a-z ]", " ", s)
  trimws(gsub("\\s+", " ", s))
}

# ---- expand back to per-row ----
result <- us |>
  left_join(matched |>
              select(city, state, lat, lon, geoid, county_name,
                     matched_name, match_source, jw),
            by = c("city","state"))

# per-row suspect detection: compare cleaned (city, state) to this lineid's own original
result_norm_orig  <- norm_for_check(result$birthplace_orig)
result_norm_city  <- norm_for_check(result$city)
result_substr_hit <- mapply(function(c, o) {
  if (!nzchar(c)) return(FALSE)
  grepl(paste0("\\b", gsub("\\s+", "\\\\s+", c), "\\b"), o)
}, result_norm_city, result_norm_orig, USE.NAMES = FALSE)
result$cleaning_jw <- stringdist::stringdist(
  result_norm_city, substr(result_norm_orig, 1, 25), method = "jw", p = 0.1)
result$agent_cleaning_suspect <- !result_substr_hit & result$cleaning_jw > 0.30

geocoded  <- result |> filter(!is.na(geoid))
unmatched <- result |> filter(is.na(geoid)) |>
  select(lineid, birthplace_orig, city, state, flag)

# Per-row suspects (matched but cleaned city doesn't track the original)
suspects <- result |>
  filter(!is.na(geoid), agent_cleaning_suspect) |>
  arrange(desc(cleaning_jw)) |>
  select(lineid, birthplace_orig, city, state, matched_name,
         county_name, geoid, cleaning_jw, match_source)
write_csv(suspects, file.path(out_dir, "amws_1955_us_geocoded_suspects.csv"))
cat("per-row suspects (cleaning_jw>0.30 AND no substring hit):", nrow(suspects), "\n")

write_csv(geocoded,  file.path(out_dir, "amws_1955_us_geocoded.csv"))
write_csv(unmatched, file.path(out_dir, "amws_1955_us_unmatched.csv"))

cat("\n=== output ===\n")
cat("US rows total:      ", nrow(us), "\n")
cat("geocoded:           ", nrow(geocoded), "\n")
cat("unmatched:          ", nrow(unmatched), "\n")
cat("\n=== match_source mix ===\n")
print(table(geocoded$match_source))

# ---- QC: 100 random matched rows ----
set.seed(1)
qc <- geocoded |> slice_sample(n = 100)
qc_path <- file.path(out_dir, "amws_1955_us_geocoded_qc100.csv")
write_csv(qc |> select(lineid, birthplace_orig, city, state, matched_name,
                       county_name, geoid, match_source, jw), qc_path)
cat("\nQC sample written:", qc_path, "\n")
