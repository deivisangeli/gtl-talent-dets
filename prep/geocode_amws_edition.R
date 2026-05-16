###############################################################################
# Generic AMWS edition geocoder. Takes an edition tag via env AMWS_EDITION
# (1906 or 1938) and points at the corresponding cleaned file.
#
# Pipeline:
#   1. Filter to country == "USA" with non-empty city+state.
#   2. Stage A: exact (lower(city), state) match against Census Gazetteer 2024
#      Places. Result: gazetteer_exact + 2024 place GEOID + lat/lon.
#   3. Stage B: exact match against GeoNames-US (feature_class P/A) with admin1
#      = state. Result: geonames_exact + admin2 FIPS.
#   4. Stage C: fuzzy within-state (Jaro-Winkler <= 0.10) against the union of
#      Gazetteer + GeoNames-US. Result: *_fuzzy + JW score.
#   5. Spatial join lat/lon -> 2020 county GEOID, snap stray coastal hits.
#   6. Hand-curated multi-county-place overrides (NYC, Jersey City, SF, ...).
#   7. Suspect flag: cleaning_jw > 0.30 AND city not a substring of orig.
#
# Outputs (where ED = edition tag):
#   output/amws_<ED>_us_geocoded.csv
#   output/amws_<ED>_us_unmatched.csv
#   output/amws_<ED>_us_geocoded_suspects.csv
#   output/amws_<ED>_us_geocoded_qc100.csv
###############################################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(stringdist)
  library(sf)
})

ED <- Sys.getenv("AMWS_EDITION", unset = "1906")
stopifnot(ED %in% c("1906", "1938", "1955"))
cat("=== geocoding AMWS edition", ED, "===\n")

source("../paths.R")
in_dir   <- DATA_INPUT
out_dir  <- AMWS_OUTPUT

gaz_file  <- file.path(DATA_INPUT, "2024_Gaz_place_national.txt")
geon_file <- file.path(DATA_INPUT, "geonames_US.txt")
county_shp <- file.path(Sys.getenv("LOCALAPPDATA"),
                        "tigris", "tigris", "Cache",
                        "cb_2020_us_county_20m.shp")
stopifnot(file.exists(gaz_file), file.exists(geon_file), file.exists(county_shp))

cleaned_file <- Sys.getenv("AMWS_CLEANED_FILE",
                           unset = file.path(out_dir,
                                             sprintf("amws_%s_cleaned.csv", ED)))
# Optional suffix on output filenames (e.g. "_final" after manual fix).
SUF <- Sys.getenv("AMWS_OUT_SUFFIX", unset = "")

# ---- load gazetteer ----
gaz <- read_tsv(gaz_file, show_col_types = FALSE,
                locale = locale(encoding = "UTF-8")) |>
  rename_with(~ trimws(.x)) |>
  transmute(state = USPS,
            geoid_place = GEOID,
            name        = NAME,
            lat         = INTPTLAT,
            lon         = INTPTLONG)

# ---- load geonames (P + A) ----
geon_cols <- c("geonameid","name","asciiname","alternatenames","latitude",
               "longitude","feature_class","feature_code","country","cc2",
               "admin1","admin2","admin3","admin4","population","elevation",
               "dem","timezone","modification_date")
geon <- read_tsv(geon_file, col_names = geon_cols, show_col_types = FALSE,
                 quote = "", locale = locale(encoding = "UTF-8")) |>
  filter(feature_class %in% c("P", "A")) |>
  transmute(state = admin1,
            name = asciiname,
            lat = latitude, lon = longitude,
            admin2_fips = admin2,
            population = as.integer(population))

# ---- normalize ----
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
strip_punct <- function(x) {
  x <- gsub("\\.", "", x)
  x <- gsub("[[:punct:]]", " ", x)
  trimws(gsub("\\s+", " ", x))
}
norm <- function(x) tolower(trimws(strip_punct(strip_suffix(expand(x)))))

gaz$key  <- norm(gaz$name)
geon$key <- norm(geon$name)

# ---- load cleaned AMWS rows ----
cleaned <- read_csv(cleaned_file, show_col_types = FALSE)
us <- cleaned |>
  filter(country == "USA", nzchar(city), nzchar(state)) |>
  select(lineid, birthplace_orig, city, state, flag)
cat("US rows total:", nrow(us), "\n")

pairs <- us |> distinct(city, state) |> mutate(key = norm(city))
cat("distinct (city, state) pairs:", nrow(pairs), "\n")

# ---- 1. exact gazetteer ----
m1 <- pairs |>
  inner_join(gaz |> select(key, state, lat, lon, geoid_place, gaz_name = name),
             by = c("key","state")) |>
  group_by(city, state) |> slice(1) |> ungroup() |>
  mutate(match_source = "gazetteer_exact")
cat("step 1 (gazetteer exact):", nrow(m1), "\n")

# ---- 2. exact geonames ----
remain <- pairs |> anti_join(m1, by = c("city","state"))
m2 <- remain |>
  inner_join(geon |> select(key, state, lat, lon, admin2_fips,
                            geon_name = name, population),
             by = c("key","state")) |>
  arrange(city, state, desc(population)) |>
  group_by(city, state) |> slice(1) |> ungroup() |>
  mutate(match_source = "geonames_exact")
cat("step 2 (geonames exact):", nrow(m2), "\n")

# ---- 3. fuzzy ----
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
    if (!is.null(res)) fuzz[[i]] <- bind_cols(r, res |> select(-key))
  }
  m3 <- bind_rows(fuzz) |>
    rename(match_source = src, matched_name = name)
} else {
  m3 <- tibble()
}
cat("step 3 (fuzzy):", nrow(m3), "\n")

# ---- combine ----
matched <- bind_rows(
  m1 |> transmute(city, state, key, lat, lon, geoid_place,
                  admin2_fips = NA_character_,
                  matched_name = gaz_name, match_source, jw = 0),
  m2 |> transmute(city, state, key, lat, lon,
                  geoid_place = NA_character_, admin2_fips,
                  matched_name = geon_name, match_source, jw = 0),
  if (nrow(m3)) m3 |> transmute(city, state, key, lat, lon,
                                geoid_place = NA_character_,
                                admin2_fips, matched_name, match_source, jw)
  else tibble()
)
matched$lat <- as.numeric(matched$lat)
matched$lon <- as.numeric(matched$lon)
matched <- matched |> filter(!is.na(lat), !is.na(lon))

# ---- 4. spatial join → 2020 GEOID, with coastal snap ----
counties <- st_read(county_shp, quiet = TRUE) |> st_transform(4326)
pts <- st_as_sf(matched, coords = c("lon","lat"), crs = 4326, remove = FALSE)
sj  <- st_join(pts, counties |> select(STATEFP, COUNTYFP, GEOID, NAME),
               left = TRUE)
matched$geoid       <- sj$GEOID
matched$county_name <- sj$NAME

state_to_fp <- c(
  AL="01",AK="02",AZ="04",AR="05",CA="06",CO="08",CT="09",DE="10",DC="11",
  FL="12",GA="13",HI="15",ID="16",IL="17",IN="18",IA="19",KS="20",KY="21",
  LA="22",ME="23",MD="24",MA="25",MI="26",MN="27",MS="28",MO="29",MT="30",
  NE="31",NV="32",NH="33",NJ="34",NM="35",NY="36",NC="37",ND="38",OH="39",
  OK="40",OR="41",PA="42",RI="44",SC="45",SD="46",TN="47",TX="48",UT="49",
  VT="50",VA="51",WA="53",WV="54",WI="55",WY="56",PR="72")

snap_idx <- which(is.na(matched$geoid))
if (length(snap_idx) > 0) {
  by_st <- split(snap_idx, matched$state[snap_idx])
  for (st in names(by_st)) {
    st_fp <- state_to_fp[st]; if (is.na(st_fp)) next
    cs <- counties[counties$STATEFP == st_fp, ]
    if (nrow(cs) == 0) next
    nf <- sf::st_nearest_feature(pts[by_st[[st]], ], cs)
    matched$geoid[by_st[[st]]]       <- cs$GEOID[nf]
    matched$county_name[by_st[[st]]] <- cs$NAME[nf]
  }
}

# ---- 5. hand-curated overrides (NYC, JC, SF) ----
overrides <- tibble::tribble(
  ~city,           ~state, ~geoid_new, ~county_new,
  "New York",      "NY",   "36061",    "New York",
  "Manhattan",     "NY",   "36061",    "New York",
  "Bronx",         "NY",   "36005",    "Bronx",
  "Brooklyn",      "NY",   "36047",    "Kings",
  "Queens",        "NY",   "36081",    "Queens",
  "Staten Island", "NY",   "36085",    "Richmond",
  "Jersey City",   "NJ",   "34017",    "Hudson",
  "Hoboken",       "NJ",   "34017",    "Hudson",
  "San Francisco", "CA",   "06075",    "San Francisco"
)
for (i in seq_len(nrow(overrides))) {
  hit <- matched$city == overrides$city[i] & matched$state == overrides$state[i]
  if (any(hit)) {
    matched$geoid[hit]       <- overrides$geoid_new[i]
    matched$county_name[hit] <- overrides$county_new[i]
  }
}

# ---- 6. expand back to per-row + suspect detection ----
result <- us |>
  left_join(matched |>
              select(city, state, lat, lon, geoid, county_name,
                     matched_name, match_source, jw),
            by = c("city","state"))

norm_for_check <- function(s) {
  s <- tolower(ifelse(is.na(s), "", s))
  s <- gsub("[^a-z ]", " ", s)
  trimws(gsub("\\s+", " ", s))
}
result_norm_orig <- norm_for_check(result$birthplace_orig)
result_norm_city <- norm_for_check(result$city)
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
suspects <- result |>
  filter(!is.na(geoid), agent_cleaning_suspect) |>
  arrange(desc(cleaning_jw)) |>
  select(lineid, birthplace_orig, city, state, matched_name,
         county_name, geoid, cleaning_jw, match_source)

write_csv(geocoded,  file.path(out_dir, sprintf("amws_%s_us_geocoded%s.csv", ED, SUF)))
write_csv(unmatched, file.path(out_dir, sprintf("amws_%s_us_unmatched%s.csv", ED, SUF)))
write_csv(suspects,  file.path(out_dir, sprintf("amws_%s_us_geocoded_suspects%s.csv", ED, SUF)))

cat("\n=== output (edition", ED, ") ===\n")
cat("US rows total:    ", nrow(us), "\n")
cat("geocoded:         ", nrow(geocoded), "\n")
cat("unmatched:        ", nrow(unmatched), "\n")
cat("suspects:         ", nrow(suspects), "\n")
cat("\n=== match_source mix ===\n")
print(table(geocoded$match_source))

set.seed(1)
qc <- geocoded |> slice_sample(n = min(100, nrow(geocoded)))
write_csv(qc |> select(lineid, birthplace_orig, city, state, matched_name,
                       county_name, geoid, match_source, jw),
          file.path(out_dir, sprintf("amws_%s_us_geocoded_qc100%s.csv", ED, SUF)))
