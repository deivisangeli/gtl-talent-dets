# aggregate_manual_fix.R
#
# Combine all manual-fix output JSONLs into a single CSV, merge into the cleaned
# data as overrides (city/state/country), then re-run the geocoder on the
# corrected dataset and produce a final per-row file plus accuracy stats.

suppressPackageStartupMessages({
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(stringr)
  library(stringdist)
  library(sf)
})

source("../paths.R")
out_dir    <- AMWS_OUTPUT
batch_dir  <- file.path(out_dir, "manual_fix")
fix_in     <- file.path(batch_dir, "in")
fix_out    <- file.path(batch_dir, "out")
gaz_file   <- file.path(DATA_INPUT, "2024_Gaz_place_national.txt")
geon_file  <- file.path(DATA_INPUT, "geonames_US.txt")
county_shp <- file.path(Sys.getenv("LOCALAPPDATA"),
                        "tigris", "tigris", "Cache", "cb_2020_us_county_20m.shp")

# ---- combine fix outputs ----
files <- sort(list.files(fix_out, pattern = "^\\d{5}\\.jsonl$", full.names = TRUE))
in_files <- sort(list.files(fix_in,  pattern = "^\\d{5}\\.jsonl$", full.names = TRUE))
cat("input batches: ",  length(in_files),  "\n")
cat("output batches:", length(files),     "\n")

read_jsonl <- function(f) {
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  lines <- iconv(lines, from = "UTF-8", to = "UTF-8", sub = "?")
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) return(NULL)
  lines <- unlist(strsplit(paste(lines, collapse = "\n"),
                           "(?<=\\})\\s*(?=\\{)", perl = TRUE))
  lines <- lines[nzchar(trimws(lines))]
  do.call(rbind, lapply(lines, function(l) {
    j <- tryCatch(fromJSON(l), error = function(e) NULL)
    if (is.null(j)) return(NULL)
    data.frame(
      lineid     = as.integer(j$lineid %||% NA),
      city_fix   = as.character(j$city    %||% ""),
      state_fix  = as.character(j$state   %||% ""),
      country_fix= as.character(j$country %||% ""),
      confidence = as.character(j$confidence %||% ""),
      notes      = as.character(j$notes %||% ""),
      stringsAsFactors = FALSE
    )
  }))
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

fixes <- do.call(rbind, lapply(files, read_jsonl))
cat("rows of fixes:", nrow(fixes), "\n")
write_csv(fixes, file.path(out_dir, "manual_fix_results.csv"))

cat("\n=== confidence distribution ===\n")
print(table(fixes$confidence, useNA = "ifany"))
cat("\n=== country distribution ===\n")
print(table(fixes$country_fix, useNA = "ifany"))
cat("\n=== non-US rows ===\n")
print(fixes |> filter(country_fix != "USA", country_fix != "") |> head(20))

# ---- merge with cleaned data ----
cleaned <- read_csv(file.path(out_dir, "amws_1955_cleaned.csv"),
                    show_col_types = FALSE)
us_rows_before <- sum(cleaned$country == "USA")
cleaned_fixed <- cleaned |>
  left_join(fixes, by = "lineid") |>
  mutate(
    city    = ifelse(!is.na(city_fix),    city_fix,    city),
    state   = ifelse(!is.na(state_fix),   state_fix,   state),
    country = ifelse(!is.na(country_fix), country_fix, country)
  )
us_rows_after <- sum(cleaned_fixed$country == "USA")
cat("\nUS rows before/after fix overrides:", us_rows_before, "/", us_rows_after, "\n")

write_csv(cleaned_fixed |> select(-city_fix, -state_fix, -country_fix, -confidence, -notes),
          file.path(out_dir, "amws_1955_cleaned_corrected.csv"))

# ---- re-geocode the US-corrected rows ----
# Re-uses the gazetteer + geonames + spatial join logic from geocode_amws_1955_us.R
strip_punct  <- function(x) trimws(gsub("\\s+"," ",gsub("[[:punct:]]"," ",gsub("\\.","",x))))
expand_abbr  <- function(x) {
  x <- gsub("\\bSt\\b\\.?",  "Saint", x, ignore.case = TRUE)
  x <- gsub("\\bSte\\b\\.?", "Sainte",x, ignore.case = TRUE)
  x <- gsub("\\bMt\\b\\.?",  "Mount", x, ignore.case = TRUE)
  x <- gsub("\\bFt\\b\\.?",  "Fort",  x, ignore.case = TRUE)
  x
}
strip_suffix <- function(x) trimws(gsub("\\s+"," ",
  gsub("\\b(city|town|township|village|borough|cdp)\\b","",x,ignore.case=TRUE)))
norm <- function(x) tolower(trimws(strip_punct(strip_suffix(expand_abbr(x)))))

gaz <- read_tsv(gaz_file, show_col_types = FALSE,
                locale = locale(encoding = "UTF-8")) |>
  transmute(state = USPS, geoid_place = GEOID, name = NAME,
            lat = INTPTLAT, lon = INTPTLONG)
gaz$key <- norm(gaz$name)

geon_cols <- c("geonameid","name","asciiname","alternatenames","latitude",
               "longitude","feature_class","feature_code","country","cc2",
               "admin1","admin2","admin3","admin4","population","elevation",
               "dem","timezone","modification_date")
geon <- read_tsv(geon_file, col_names = geon_cols, show_col_types = FALSE,
                 quote = "", locale = locale(encoding = "UTF-8")) |>
  filter(feature_class %in% c("P","A")) |>
  transmute(state = admin1, name = asciiname,
            lat = latitude, lon = longitude,
            admin2_fips = admin2, population = as.integer(population))
geon$key <- norm(geon$name)

counties <- st_read(county_shp, quiet = TRUE) |> st_transform(4326)
state_to_fp <- c(
  AL="01",AK="02",AZ="04",AR="05",CA="06",CO="08",CT="09",DE="10",DC="11",
  FL="12",GA="13",HI="15",ID="16",IL="17",IN="18",IA="19",KS="20",KY="21",
  LA="22",ME="23",MD="24",MA="25",MI="26",MN="27",MS="28",MO="29",MT="30",
  NE="31",NV="32",NH="33",NJ="34",NM="35",NY="36",NC="37",ND="38",OH="39",
  OK="40",OR="41",PA="42",RI="44",SC="45",SD="46",TN="47",TX="48",UT="49",
  VT="50",VA="51",WA="53",WV="54",WI="55",WY="56",PR="72")

us <- cleaned_fixed |> filter(country == "USA", nzchar(city), nzchar(state)) |>
  select(lineid, birthplace_orig, city, state, flag)
cat("US rows w/ city+state after fix:", nrow(us), "\n")

pairs <- us |> distinct(city, state) |> mutate(key = norm(city))

# Exact gazetteer
m1 <- pairs |>
  inner_join(gaz |> select(key, state, lat, lon, geoid_place, gaz_name = name),
             by = c("key","state"), relationship = "many-to-many") |>
  group_by(city, state) |> slice(1) |> ungroup() |>
  mutate(match_source = "gazetteer_exact",
         admin2_fips = NA_character_, matched_name = gaz_name, jw = 0)

remain <- pairs |> anti_join(m1, by = c("city","state"))
m2 <- remain |>
  inner_join(geon |> select(key, state, lat, lon, admin2_fips, geon_name = name, population),
             by = c("key","state"), relationship = "many-to-many") |>
  arrange(city, state, desc(population)) |>
  group_by(city, state) |> slice(1) |> ungroup() |>
  mutate(match_source = "geonames_exact",
         geoid_place = NA_character_, matched_name = geon_name, jw = 0)

remain <- remain |> anti_join(m2, by = c("city","state"))
fuzzy_one <- function(key_in, state_in) {
  cands <- bind_rows(
    gaz  |> filter(state == state_in) |>
      transmute(key, name, lat, lon, src = "gazetteer_fuzzy",
                admin2_fips = NA_character_, population = NA_integer_),
    geon |> filter(state == state_in) |>
      transmute(key, name, lat, lon, src = "geonames_fuzzy", admin2_fips, population)
  )
  if (nrow(cands) == 0) return(NULL)
  d <- stringdist::stringdist(key_in, cands$key, method = "jw", p = 0.1)
  i <- which.min(d)
  if (length(i) == 0 || d[i] > 0.10) return(NULL)
  cands[i, ] |> mutate(jw = d[i])
}
m3 <- if (nrow(remain) > 0) {
  rs <- vector("list", nrow(remain))
  for (i in seq_len(nrow(remain))) {
    rr <- remain[i,]
    res <- fuzzy_one(rr$key, rr$state)
    if (!is.null(res)) rs[[i]] <- bind_cols(rr, res |> select(-key))
  }
  bind_rows(rs) |> rename(match_source = src, matched_name = name) |>
    mutate(geoid_place = NA_character_)
} else tibble()

matched <- bind_rows(
  m1 |> transmute(city, state, key, lat, lon, geoid_place,
                  admin2_fips, matched_name, match_source, jw),
  m2 |> transmute(city, state, key, lat, lon, geoid_place,
                  admin2_fips, matched_name, match_source, jw),
  if (nrow(m3)) m3 |> transmute(city, state, key, lat, lon, geoid_place,
                                admin2_fips, matched_name, match_source, jw)
  else tibble()
)
matched$lat <- as.numeric(matched$lat); matched$lon <- as.numeric(matched$lon)
matched <- matched |> filter(!is.na(lat), !is.na(lon))

pts <- st_as_sf(matched, coords = c("lon","lat"), crs = 4326, remove = FALSE)
sj  <- st_join(pts, counties |> select(STATEFP, COUNTYFP, GEOID, NAME), left = TRUE)
matched$geoid <- sj$GEOID; matched$county_name <- sj$NAME

# nearest-state snap for offshore centroids
snap_idx <- which(is.na(matched$geoid))
if (length(snap_idx) > 0) {
  by_st <- split(snap_idx, matched$state[snap_idx])
  for (st in names(by_st)) {
    st_fp <- state_to_fp[st]; if (is.na(st_fp)) next
    cs <- counties[counties$STATEFP == st_fp, ]; if (nrow(cs) == 0) next
    nf <- sf::st_nearest_feature(pts[by_st[[st]], ], cs)
    matched$geoid[by_st[[st]]]       <- cs$GEOID[nf]
    matched$county_name[by_st[[st]]] <- cs$NAME[nf]
  }
}
# admin2 FIPS fallback (for geonames matches that fell outside polygons)
needs_fb <- is.na(matched$geoid) & !is.na(matched$admin2_fips) & nzchar(matched$admin2_fips)
st_lk <- counties |> st_drop_geometry() |>
  transmute(state_fp = STATEFP, county_fp = COUNTYFP, geoid_fb = GEOID, name_fb = NAME)
for (i in which(needs_fb)) {
  st_fp <- state_to_fp[matched$state[i]]
  if (is.na(st_fp)) next
  hit <- st_lk |> filter(state_fp == st_fp, county_fp == matched$admin2_fips[i])
  if (nrow(hit) == 1) {
    matched$geoid[i]       <- hit$geoid_fb
    matched$county_name[i] <- hit$name_fb
  }
}

# overrides (NYC etc)
overrides <- tribble(
  ~city, ~state, ~geoid_new, ~county_new,
  "New York",     "NY", "36061", "New York",
  "Manhattan",    "NY", "36061", "New York",
  "Bronx",        "NY", "36005", "Bronx",
  "Brooklyn",     "NY", "36047", "Kings",
  "Queens",       "NY", "36081", "Queens",
  "Staten Island","NY", "36085", "Richmond",
  "Jersey City",  "NJ", "34017", "Hudson",
  "Hoboken",      "NJ", "34017", "Hudson",
  "San Francisco","CA", "06075", "San Francisco",
)
for (i in seq_len(nrow(overrides))) {
  hit <- matched$city == overrides$city[i] & matched$state == overrides$state[i]
  if (any(hit)) {
    matched$geoid[hit]       <- overrides$geoid_new[i]
    matched$county_name[hit] <- overrides$county_new[i]
  }
}

result <- us |> left_join(matched |> select(city, state, lat, lon, geoid,
                                            county_name, matched_name,
                                            match_source, jw),
                          by = c("city","state"))
result <- result |> left_join(fixes |> select(lineid, confidence, notes),
                              by = "lineid")
final_geocoded <- result |> filter(!is.na(geoid))
still_unmatched <- result |> filter(is.na(geoid)) |>
  select(lineid, birthplace_orig, city, state, flag, confidence, notes)

write_csv(final_geocoded,
          file.path(out_dir, "amws_1955_us_geocoded_final.csv"))
write_csv(still_unmatched,
          file.path(out_dir, "amws_1955_us_still_unmatched.csv"))

cat("\n=== FINAL ===\n")
cat("US rows total:                ", nrow(us), "\n")
cat("geocoded (after manual fix):  ", nrow(final_geocoded), "\n")
cat("still unmatched:              ", nrow(still_unmatched), "\n")
cat("\nmatch_source mix:\n"); print(table(final_geocoded$match_source))
cat("\nconfidence mix (manual-fixed rows):\n")
print(table(final_geocoded$confidence, useNA = "ifany"))
