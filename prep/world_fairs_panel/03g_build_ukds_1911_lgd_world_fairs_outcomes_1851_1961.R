###############################################################################
# Build the fixed-1911 LGD annual and decade panels used by the UK world-fairs
# synthetic-DiD analyses. Outcomes: Wikipedia Discovery/Science births and
# AMWS births. Treatment: each LGD's first matched fair in the full fair file.
#
# Run from the repository root after step 03f:
#   Rscript prep/world_fairs_panel/03g_build_ukds_1911_lgd_world_fairs_outcomes_1851_1961.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(sf)
  library(stringi)
})

sf_use_s2(FALSE)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]),
                              winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                             winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  candidate <- file.path("C:/Users", Sys.info()[["user"]],
                         "Globtalent Dropbox", "gtl_talent_dets")
  if (dir.exists(candidate)) {
    TALENT_DETS_DATA_DIR <- candidate
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
    AMWS_INPUT <- file.path(DATA_INPUT, "amws")
    AMWS_OUTPUT <- file.path(DATA_OUTPUT, "amws")
  }
}

processed_dir <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "processed", "worlds_fairs",
  "ukds_1911_boundary_crosswalk"
)
population_file <- file.path(processed_dir,
  "ukds_1911_lgd_population_annual_1851_1961.csv")
baseline_file <- file.path(processed_dir,
  "ukds_1911_lgd_baseline_covariates_1851.csv")
geometry_file <- file.path(processed_dir,
  "ukds_1911_lgd_analysis_geometries.gpkg")
wiki_file <- file.path(DATA_INPUT, "cross-verified-database.csv")
fairs_file <- file.path(DATA_INPUT, "worlds_fairs",
  "worlds_fairs_1790_1960_with_visits_venues.csv")
geonames_file <- file.path(TALENT_DETS_DATA_DIR, "raw", "geonames",
                           "allCountries.txt")
geonames_cache <- file.path(processed_dir, "ukds_amws_geonames_ew_places.csv")

amws_1906_file <- file.path(AMWS_OUTPUT, "amws_1906_cleaned_corrected.csv")
amws_1938_file <- file.path(AMWS_OUTPUT, "amws_1938_cleaned_corrected.csv")
amws_1955_file <- file.path(AMWS_OUTPUT, "amws_1955_split_corrected.csv")
amws_1906_raw <- file.path(AMWS_INPUT, "amws_1906.xlsx")
amws_1986_file <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed",
                            "amws", "amws_ed86_final.csv")

annual_output <- file.path(processed_dir,
  "ukds_1911_lgd_world_fairs_panel_annual_1851_1961.csv")
standard_output <- file.path(processed_dir,
  "ukds_1911_lgd_world_fairs_panel_decade_standard_1851_1961.csv")
alternative_output <- file.path(processed_dir,
  "ukds_1911_lgd_world_fairs_panel_decade_alternative_1851_1961.csv")
wiki_audit_file <- file.path(processed_dir, "ukds_1911_lgd_wikipedia_match_audit.csv")
amws_audit_file <- file.path(processed_dir, "ukds_1911_lgd_amws_match_audit.csv")
amws_dedup_file <- file.path(processed_dir, "ukds_1911_lgd_amws_early_dedup_audit.csv")
amws_see_previous_file <- file.path(processed_dir,
  "ukds_1911_lgd_amws_1986_see_previous_excluded.csv")
fair_audit_file <- file.path(processed_dir, "ukds_1911_lgd_fair_treatment_audit.csv")
qc_file <- file.path(processed_dir, "ukds_1911_lgd_world_fairs_panel_qc.csv")

required <- c(population_file, baseline_file, geometry_file, wiki_file, fairs_file,
              geonames_file, amws_1906_file, amws_1938_file,
              amws_1955_file, amws_1906_raw, amws_1986_file)
missing <- required[!file.exists(required)]
if (length(missing)) stop("Missing inputs:\n", paste(missing, collapse = "\n"))

normalize_text <- function(x) {
  x <- stri_trans_general(as.character(x), "Latin-ASCII")
  x <- toupper(trimws(x))
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("[[:space:]]+", " ", x))
}
has_value <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & x != "" & !toupper(x) %in% c("NA", "N/A", "NULL")
}
standard_decade <- function(year) as.integer(floor(as.numeric(year) / 10) * 10)
alternative_decade <- function(year) {
  year <- as.integer(year)
  standard_decade(year) + ifelse(!is.na(year) & year %% 10L >= 7L, 10L, 0L)
}
first_pos <- function(pattern, text) {
  as.integer(regexpr(pattern, as.character(text), ignore.case = TRUE, perl = TRUE))
}
is_see_previous_contaminated <- function(text) {
  text <- as.character(text); text[is.na(text)] <- ""
  see_pos <- first_pos("\\bsee\\s+prev(?:ious)?(?:\\s+edition)?\\b", text)
  birth_pos <- first_pos("(^|[[:space:],.;])b[[:space:]]+", text)
  see_pos > 0L & (birth_pos < 0L | birth_pos > see_pos)
}

message("Reading fixed LGD panel and geometry...")
panel <- fread(population_file, na.strings = c("", "NA"))
panel[, `:=`(year = as.integer(year), population = as.numeric(population),
             estimated_births = as.numeric(estimated_births))]
baseline_variables <- c(
  paste0("baseline_hc", 1:5, "_pct"),
  paste0("baseline_sc", 1:8, "_pct"),
  "baseline_children_per_teacher"
)
baseline_metadata <- c(
  "baseline_covariate_year", "baseline_covariate_population_coverage",
  "baseline_covariates_usable", "baseline_covariate_coverage_flag",
  "baseline_covariate_method"
)
baseline_columns <- c("analysis_lgd_id", baseline_variables, baseline_metadata)
baseline <- fread(baseline_file, na.strings = c("", "NA"))
if (!all(baseline_columns %in% names(baseline)))
  stop("The fixed-1851 baseline file is missing required columns.")
baseline <- baseline[, ..baseline_columns]
baseline[, baseline_covariate_year := as.integer(baseline_covariate_year)]
baseline[, baseline_covariates_usable := as.logical(baseline_covariates_usable)]
for (variable in c(baseline_variables,
                   "baseline_covariate_population_coverage")) {
  set(baseline, j = variable,
      value = suppressWarnings(as.numeric(baseline[[variable]])))
}
if (nrow(baseline) != 1834L || anyDuplicated(baseline$analysis_lgd_id) ||
    anyNA(baseline$baseline_covariates_usable) ||
    !identical(unique(baseline$baseline_covariate_year), 1851L) ||
    baseline[baseline_covariates_usable == TRUE, .N] != 1801L ||
    baseline[baseline_covariates_usable == FALSE, .N] != 33L ||
    baseline[baseline_covariates_usable == TRUE,
             anyNA(.SD), .SDcols = baseline_variables] ||
    baseline[baseline_covariates_usable == FALSE,
             any(rowSums(!is.na(.SD)) > 0), .SDcols = baseline_variables]) {
  stop("The fixed-1851 baseline file failed its input checks.")
}
targets <- st_read(geometry_file, layer = "fixed_1911_lgds", quiet = TRUE)
targets <- st_transform(st_make_valid(targets), 27700)
if (nrow(targets) != 1834L || nrow(panel) != 1834L * 111L)
  stop("Unexpected fixed-LGD geometry or annual-panel dimensions.")

assign_points <- function(dt, lon_col, lat_col, id_col) {
  valid <- dt[is.finite(get(lon_col)) & is.finite(get(lat_col))]
  if (!nrow(valid)) return(data.table())
  pts <- st_as_sf(valid, coords = c(lon_col, lat_col), crs = 4326,
                  remove = FALSE)
  pts <- st_transform(pts, st_crs(targets))
  joined <- suppressWarnings(st_join(
    pts, targets[, c("analysis_lgd_id", "population_lgd_name",
                     "population_lgd_type")],
    join = st_intersects, left = TRUE
  ))
  out <- as.data.table(st_drop_geometry(joined))
  setorderv(out, c(id_col, "analysis_lgd_id"))
  out[, spatial_match_count := sum(!is.na(analysis_lgd_id)), by = id_col]
  out
}

###############################################################################
# Wikipedia outcomes
###############################################################################

message("Assigning Wikipedia Discovery/Science births...")
wiki_cols <- c("wikidata_code", "birth", "level1_main_occ",
               "level3_main_occ", "level3_all_occ", "bplo1", "bpla1")
wiki <- fread(wiki_file, select = wiki_cols, na.strings = c("", "NA"))
wiki[, birth := suppressWarnings(as.integer(birth))]
wiki <- wiki[level1_main_occ == "Discovery/Science" &
               birth >= 1851L & birth <= 1961L &
               is.finite(bplo1) & is.finite(bpla1)]
source(file.path(repo_root, "prep", "stem_labels.R"))
wiki <- as.data.table(add_stem_dummy(as.data.frame(wiki)))
wiki_match <- assign_points(wiki, "bplo1", "bpla1", "wikidata_code")
wiki_match[, match_status := fifelse(
  spatial_match_count == 1L, "matched_unique_lgd",
  fifelse(spatial_match_count > 1L, "ambiguous_boundary_overlap",
          "outside_fixed_ew_lgds"))]
wiki_valid <- wiki_match[match_status == "matched_unique_lgd"]
wiki_year <- wiki_valid[, .(
  n_inventors = .N,
  n_all_wiki = .N,
  n_stem = sum(stem == 1L, na.rm = TRUE),
  n_nonstem = sum(stem != 1L | is.na(stem))
), by = .(analysis_lgd_id, year = birth)]
fwrite(wiki_match, wiki_audit_file, na = "")

###############################################################################
# Exact-only AMWS England/Wales place matching
###############################################################################

if (!file.exists(geonames_cache)) {
  message("Building England/Wales populated-place cache from allCountries.txt...")
  gn_names <- c("geonameid", "name", "asciiname", "alternatenames",
                "latitude", "longitude", "feature_class", "feature_code",
                "country_code", "cc2", "admin1_code", "admin2_code",
                "admin3_code", "admin4_code", "population", "elevation",
                "dem", "timezone", "modification_date")
  gn <- fread(geonames_file, sep = "\t", header = FALSE,
              select = c(1L,2L,3L,4L,5L,6L,7L,8L,9L,11L,15L),
              col.names = gn_names[c(1L,2L,3L,4L,5L,6L,7L,8L,9L,11L,15L)],
              quote = "", showProgress = TRUE)
  gn <- gn[country_code == "GB" & feature_class == "P" &
             admin1_code %in% c("ENG", "WLS")]
  gn[, `:=`(geonameid = as.integer(geonameid), latitude = as.numeric(latitude),
             longitude = as.numeric(longitude), population = as.numeric(population),
             country_scope = fifelse(admin1_code == "ENG", "ENGLAND", "WALES"))]
  fwrite(gn, geonames_cache, na = "")
  rm(gn); gc()
}
gn <- fread(geonames_cache, na.strings = c("", "NA"))

# One row per exact primary/ascii/alternate spelling and GeoNames place.
primary_names <- rbindlist(list(
  gn[, .(geonameid, place_name = name, latitude, longitude, feature_code,
         population, country_scope, name_source = "primary")],
  gn[, .(geonameid, place_name = asciiname, latitude, longitude, feature_code,
         population, country_scope, name_source = "ascii")]
))
alt_names <- gn[has_value(alternatenames), .(
  place_name = unlist(strsplit(alternatenames, ",", fixed = TRUE))),
  by = .(geonameid, latitude, longitude, feature_code, population, country_scope)]
alt_names[, name_source := "alternate"]
place_names <- rbindlist(list(primary_names, alt_names), fill = TRUE)
place_names[, place_key := normalize_text(place_name)]
place_names <- unique(place_names[place_key != ""],
                      by = c("geonameid", "place_key", "country_scope"))

broad_place_keys <- normalize_text(c(
  "London", "England", "Wales", "Great Britain", "Britain", "United Kingdom",
  "Essex", "Kent", "Surrey", "Sussex", "Devon", "Devonshire", "Cornwall",
  "Somerset", "Lancashire", "Yorkshire", "Glamorgan", "Glamorganshire",
  "Monmouthshire", "Nottinghamshire", "Derbyshire", "Hampshire",
  "North Wales", "South Wales"
))

extract_first_place <- function(x) {
  x <- trimws(as.character(x))
  trimws(sub(",.*$", "", x))
}
country_scope_place <- function(x) {
  key <- normalize_text(x)
  fifelse(grepl("WALES", key) &
            !grepl("NEW SOUTH WALES|NORTH WALES PA|LAKE WALES", key),
          "WALES",
    fifelse(grepl("(^| )ENGLAND($| )", key) | key == "ENG", "ENGLAND",
      fifelse(grepl("UNITED KINGDOM|GREAT BRITAIN|(^| )(GT|GR) BRIT($| )",
                    key) | key %in% c("BRITAIN", "BRIT"),
              "EW", NA_character_)))
}
country_scope_label <- function(x) {
  key <- normalize_text(x)
  fifelse(grepl("^(S |SOUTH |NORTH )?WALES($| )", key), "WALES",
    fifelse(key %in% c("ENGLAND", "ENGLAND ENG", "ENG"), "ENGLAND",
      fifelse(key %in% c("UNITED KINGDOM", "GREAT BRITAIN", "GT BRIT",
                         "GR BRIT", "BRITAIN", "BRIT"), "EW", NA_character_)))
}

# 1906 names are recovered from the raw workbook, matching the established
# early-edition dedup pipeline.
raw06 <- as.data.table(read_excel(amws_1906_raw, sheet = 1, col_names = FALSE))
setnames(raw06, paste0("c", seq_len(ncol(raw06))))
names06 <- raw06[, .(lineid = suppressWarnings(as.integer(c1)), amsname = c2)]
names06[, `:=`(last = trimws(sub(",.*", "", amsname)),
               first = trimws(sub("^[^,]*,\\s*", "", amsname)))]

e06 <- fread(amws_1906_file, na.strings = c("", "NA"))
e06 <- merge(e06, names06[, .(lineid, last, first)], by = "lineid", all.x = TRUE)
e06[, `:=`(edition = 1906L, record_id = paste0("1906_", lineid),
  place_candidate = extract_first_place(city),
  source_country_scope = country_scope_label(country))]

e38 <- fread(amws_1938_file, na.strings = c("", "NA"))
e38[, `:=`(last = trimws(sub(",.*", "", AMSname)),
  first = trimws(sub("^[^,]*,\\s*", "", AMSname)), edition = 1938L,
  record_id = paste0("1938_", lineid),
  place_candidate = extract_first_place(city),
  source_country_scope = country_scope_label(country))]

e55 <- fread(amws_1955_file, na.strings = c("", "NA"))
e55[, source_country_scope := country_scope_place(place_raw)]
e55[, `:=`(edition = 1955L, record_id = paste0("1955_", lineid),
  place_candidate = extract_first_place(place_raw))]

early_cols <- c("edition", "record_id", "lineid", "last", "first",
                "birth_year", "place_candidate", "source_country_scope")
early <- rbindlist(list(e06[, ..early_cols], e38[, ..early_cols],
                        e55[, ..early_cols]), fill = TRUE)
early[, birth_year := suppressWarnings(as.integer(birth_year))]
early <- early[!is.na(source_country_scope) & birth_year >= 1851L &
                 birth_year <= 1960L]
early[, `:=`(norm_last = normalize_text(last),
             first_initial = substr(normalize_text(first), 1L, 1L))]
setorder(early, edition, record_id)
seen <- new.env(hash = TRUE, parent = emptyenv())
kept <- logical(nrow(early)); duplicate_of_key <- rep(NA_character_, nrow(early))
for (i in seq_len(nrow(early))) {
  if (!has_value(early$norm_last[i]) || !has_value(early$first_initial[i]) ||
      is.na(early$birth_year[i])) { kept[i] <- TRUE; next }
  keys <- paste(early$norm_last[i], early$first_initial[i],
                early$birth_year[i] + c(-1L, 0L, 1L),
                early$source_country_scope[i], sep = "|")
  hit <- keys[vapply(keys, exists, logical(1), envir = seen, inherits = FALSE)]
  if (length(hit)) {
    kept[i] <- FALSE; duplicate_of_key[i] <- hit[[1L]]
  } else {
    kept[i] <- TRUE
    assign(keys[[2L]], early$record_id[i], envir = seen)
  }
}
early[, `:=`(kept_after_early_dedup = kept,
             duplicate_of_key = duplicate_of_key)]
fwrite(early, amws_dedup_file, na = "")

ed86 <- fread(amws_1986_file, na.strings = c("", "NA"))
ed86[, see_previous_contaminated :=
       is_see_previous_contaminated(raw_text_adjusted)]
ed86[, `:=`(edition = 1986L, record_id = paste0("1986_", entry_id),
  lineid = entry_id, last = trimws(sub(",.*", "", name_raw)),
  first = trimws(sub("^[^,]*,\\s*", "", name_raw)),
  place_candidate = extract_first_place(birth_city),
  source_country_scope = country_scope_label(birth_country),
  birth_year = suppressWarnings(as.integer(birth_year)))]
ed86_excluded <- ed86[!is.na(source_country_scope) &
                        see_previous_contaminated == TRUE]
fwrite(ed86_excluded, amws_see_previous_file, na = "")
ed86 <- ed86[!is.na(source_country_scope) &
               see_previous_contaminated != TRUE &
               birth_year >= 1851L & birth_year <= 1960L]

amws_candidates <- rbindlist(list(
  early[kept_after_early_dedup == TRUE, .(
    edition, record_id, lineid, last, first, birth_year, place_candidate,
    source_country_scope, early_dedup_kept = TRUE)],
  ed86[, .(edition, record_id, lineid, last, first, birth_year,
           place_candidate, source_country_scope,
           early_dedup_kept = NA)]
), fill = TRUE)
amws_candidates[, place_key := normalize_text(place_candidate)]
amws_candidates[, broad_or_missing_place :=
  is.na(place_key) | place_key == "" | place_key %in% broad_place_keys]

scope_names <- rbindlist(list(
  place_names[country_scope == "ENGLAND"],
  place_names[country_scope == "WALES"],
  copy(place_names)[, country_scope := "EW"]
))
scope_names <- unique(scope_names,
  by = c("geonameid", "place_key", "country_scope"))
geo_candidates <- merge(
  amws_candidates[broad_or_missing_place == FALSE], scope_names,
  by.x = c("place_key", "source_country_scope"),
  by.y = c("place_key", "country_scope"),
  all.x = TRUE, allow.cartesian = TRUE, sort = FALSE
)
geo_candidates[, geonames_candidate_count := uniqueN(geonameid[!is.na(geonameid)]),
               by = record_id]
unique_geo <- geo_candidates[geonames_candidate_count == 1L & !is.na(geonameid)]
unique_geo <- unique(unique_geo, by = "record_id")
amws_points <- assign_points(unique_geo, "longitude", "latitude", "record_id")
amws_points[, amws_match_status := fifelse(
  spatial_match_count == 1L, "matched_unique_place_and_lgd",
  fifelse(spatial_match_count > 1L, "ambiguous_lgd_boundary",
          "geonames_place_outside_fixed_ew_lgds"))]

amws_audit <- merge(amws_candidates,
  unique(geo_candidates[, .(record_id, geonames_candidate_count)]),
  by = "record_id", all.x = TRUE, sort = FALSE)
amws_audit <- merge(amws_audit,
  amws_points[, .(record_id, geonameid, latitude, longitude,
                  analysis_lgd_id, population_lgd_name, population_lgd_type,
                  spatial_match_count, amws_match_status)],
  by = "record_id", all.x = TRUE, sort = FALSE)
amws_audit[broad_or_missing_place == TRUE, amws_match_status :=
  "excluded_broad_or_missing_place"]
amws_audit[broad_or_missing_place == FALSE &
             (is.na(geonames_candidate_count) | geonames_candidate_count == 0L),
           amws_match_status := "no_exact_geonames_match"]
amws_audit[broad_or_missing_place == FALSE & geonames_candidate_count > 1L,
           amws_match_status := "ambiguous_exact_geonames_match"]
fwrite(amws_audit, amws_audit_file, na = "")

amws_valid <- amws_audit[amws_match_status == "matched_unique_place_and_lgd"]
amws_year <- amws_valid[, .(
  n_amws_1906_1955_dedup = sum(edition %in% c(1906L, 1938L, 1955L)),
  n_amws_1986 = sum(edition == 1986L)
), by = .(analysis_lgd_id, year = birth_year)]

###############################################################################
# Fair treatment: own first fair, with reviewed Crystal Palace coordinate
###############################################################################

message("Assigning full fair history to fixed LGDs...")
fairs <- fread(fairs_file, na.strings = c("", "NA"))
fairs[, event_year := suppressWarnings(as.integer(year_start))]
fairs <- fairs[host_matched_country_iso3 == "GBR" & event_year <= 1961L &
  is.finite(venue_longitude) & is.finite(venue_latitude) &
  (is.na(venue_coordinates_note) |
    !grepl("automated geocoding returned no reliable coordinate",
           venue_coordinates_note, fixed = TRUE))]
fairs[parent_fair_id == 23L, `:=`(
  venue_longitude = -0.17049, venue_latitude = 51.50241,
  venue_coordinates_note = paste0(
    fifelse(is.na(venue_coordinates_note), "", venue_coordinates_note),
    " | reviewed override: Crystal Palace 1851 Hyde Park"))]
fairs[, fair_venue_row_id := .I]
fair_match <- assign_points(fairs, "venue_longitude", "venue_latitude",
                            "fair_venue_row_id")
fair_match[, fair_match_status := fifelse(
  spatial_match_count == 1L, "matched_unique_lgd",
  fifelse(spatial_match_count > 1L, "ambiguous_boundary_overlap",
          "outside_fixed_ew_lgds"))]
fair_valid <- fair_match[fair_match_status == "matched_unique_lgd"]
fair_events <- unique(fair_valid[, .(
  analysis_lgd_id, parent_fair_id, event_year, Fair_name, City
)], by = c("analysis_lgd_id", "parent_fair_id"))
first_fair <- fair_events[, .(
  first_fair_year = min(event_year), n_hosted_fairs_through_1961 = .N,
  hosted_fair_ids = paste(sort(unique(parent_fair_id)), collapse = ";")
), by = analysis_lgd_id]
treatment <- unique(panel[, .(
  analysis_lgd_id, population_adm_county, population_lgd_name,
  population_lgd_type
)])
treatment <- merge(treatment, first_fair, by = "analysis_lgd_id", all.x = TRUE)
treatment[, treatment_status := fifelse(
  is.na(first_fair_year), "never_host",
  fifelse(first_fair_year > 1870L, "treated_post1870", "pretreated_by1870"))]
treatment[, `:=`(
  synthdid_eligible = treatment_status != "pretreated_by1870",
  synthdid_g = fifelse(treatment_status == "treated_post1870", first_fair_year,
                       fifelse(treatment_status == "never_host", 0L, NA_integer_)),
  g_standard = fifelse(treatment_status == "treated_post1870",
                       standard_decade(first_fair_year),
                       fifelse(treatment_status == "never_host", 0L, NA_integer_)),
  g_alternative = fifelse(treatment_status == "treated_post1870",
                          alternative_decade(first_fair_year),
                          fifelse(treatment_status == "never_host", 0L, NA_integer_))
)]
fair_audit <- merge(fair_match, treatment[, .(analysis_lgd_id,
  first_fair_year, treatment_status)], by = "analysis_lgd_id", all.x = TRUE)
fwrite(fair_audit, fair_audit_file, na = "")

###############################################################################
# Annual panel and derived outcomes
###############################################################################

message("Assembling annual outcomes...")
panel <- merge(panel, wiki_year, by = c("analysis_lgd_id", "year"), all.x = TRUE)
panel <- merge(panel, amws_year, by = c("analysis_lgd_id", "year"), all.x = TRUE)
panel <- merge(panel, treatment, by = c("analysis_lgd_id",
  "population_adm_county", "population_lgd_name", "population_lgd_type"),
  all.x = TRUE, sort = FALSE)
wiki_count_vars <- c("n_inventors", "n_all_wiki", "n_stem", "n_nonstem")
for (v in wiki_count_vars) set(panel, which(is.na(panel[[v]])), v, 0L)
panel[, amws_coverage_available := year <= 1960L]
for (v in c("n_amws_1906_1955_dedup", "n_amws_1986")) {
  set(panel, which(is.na(panel[[v]]) & panel$amws_coverage_available), v, 0L)
  panel[amws_coverage_available == FALSE, (v) := NA_integer_]
}
panel[, n_amws := n_amws_1906_1955_dedup + n_amws_1986]
panel[, `:=`(
  any_inventor = as.integer(n_inventors > 0),
  any_stem = as.integer(n_stem > 0),
  log1p_n_inventors = log1p(n_inventors),
  log1p_n_all_wiki = log1p(n_all_wiki),
  log1p_n_stem = log1p(n_stem),
  log_population = log(population),
  inventors_per_100k = 1e5 * n_inventors / population,
  stem_per_100k = 1e5 * n_stem / population,
  nonstem_per_100k = 1e5 * n_nonstem / population,
  inventors_per_1000_births = 1000 * n_inventors / estimated_births,
  stem_per_1000_births = 1000 * n_stem / estimated_births,
  nonstem_per_1000_births = 1000 * n_nonstem / estimated_births,
  log1p_n_amws = fifelse(amws_coverage_available, log1p(n_amws), NA_real_),
  amws_per_1000_pop = fifelse(amws_coverage_available,
                              1000 * n_amws / population, NA_real_),
  amws_per_100k = fifelse(amws_coverage_available,
                          1e5 * n_amws / population, NA_real_),
  amws_per_1000_births = fifelse(amws_coverage_available,
                                 1000 * n_amws / estimated_births, NA_real_)
)]
annual_original_columns <- names(panel)
panel <- merge(panel, baseline, by = "analysis_lgd_id", all.x = TRUE,
               sort = FALSE)
setcolorder(panel, c(annual_original_columns,
                     setdiff(baseline_columns, "analysis_lgd_id")))
setorder(panel, analysis_lgd_id, year)
if (nrow(panel) != 1834L * 111L || anyNA(panel$treatment_status) ||
    panel[baseline_covariates_usable == TRUE,
          anyNA(.SD), .SDcols = baseline_variables] ||
    panel[baseline_covariates_usable == FALSE,
          any(rowSums(!is.na(.SD)) > 0), .SDcols = baseline_variables])
  stop("Annual outcome panel is incomplete.")
fwrite(panel, annual_output, na = "")

###############################################################################
# Standard and alternative decade panels
###############################################################################

aggregate_decades <- function(x, definition = c("standard", "alternative")) {
  definition <- match.arg(definition)
  x <- copy(x)
  x[, decade := if (definition == "standard") standard_decade(year) else
                    alternative_decade(year)]
  out <- x[, .(
    n_years_in_bin = .N,
    first_calendar_year = min(year), last_calendar_year = max(year),
    population = mean(population),
    estimated_births = sum(estimated_births),
    n_inventors = sum(n_inventors), n_all_wiki = sum(n_all_wiki),
    n_stem = sum(n_stem), n_nonstem = sum(n_nonstem),
    n_amws_1906_1955_dedup = sum(n_amws_1906_1955_dedup, na.rm = TRUE),
    n_amws_1986 = sum(n_amws_1986, na.rm = TRUE),
    n_amws_years_covered = sum(amws_coverage_available),
    population_is_census_knot_years = sum(population_is_census_knot)
  ), by = .(analysis_lgd_id, population_adm_county, population_lgd_name,
            population_lgd_type, source_polygon_count, source_lgd_ids,
            decade, first_fair_year, n_hosted_fairs_through_1961,
            hosted_fair_ids, treatment_status, synthdid_eligible,
            synthdid_g, g_standard, g_alternative)]
  out[, n_amws := n_amws_1906_1955_dedup + n_amws_1986]
  out[, `:=`(
    decade_definition = definition,
    complete_10_year_bin = n_years_in_bin == 10L,
    amws_complete_bin = n_amws_years_covered == n_years_in_bin,
    any_inventor = as.integer(n_inventors > 0),
    any_stem = as.integer(n_stem > 0),
    log1p_n_inventors = log1p(n_inventors),
    log1p_n_all_wiki = log1p(n_all_wiki),
    log1p_n_stem = log1p(n_stem),
    log_population = log(population),
    inventors_per_100k = 1e5 * n_inventors / population,
    stem_per_100k = 1e5 * n_stem / population,
    nonstem_per_100k = 1e5 * n_nonstem / population,
    inventors_per_1000_births = 1000 * n_inventors / estimated_births,
    stem_per_1000_births = 1000 * n_stem / estimated_births,
    nonstem_per_1000_births = 1000 * n_nonstem / estimated_births,
    log1p_n_amws = fifelse(n_amws_years_covered > 0, log1p(n_amws), NA_real_),
    amws_per_1000_pop = fifelse(n_amws_years_covered > 0,
                                1000 * n_amws / population, NA_real_),
    amws_per_100k = fifelse(n_amws_years_covered > 0,
                            1e5 * n_amws / population, NA_real_),
    amws_per_1000_births = fifelse(n_amws_years_covered > 0,
                                   1000 * n_amws / estimated_births, NA_real_),
    synthdid_g_decade = if (definition == "standard") g_standard else
                          g_alternative
  )]
  decade_original_columns <- names(out)
  out <- merge(out, baseline, by = "analysis_lgd_id", all.x = TRUE,
               sort = FALSE)
  setcolorder(out, c(decade_original_columns,
                     setdiff(baseline_columns, "analysis_lgd_id")))
  setorder(out, analysis_lgd_id, decade)
  out
}

standard <- aggregate_decades(panel, "standard")
alternative <- aggregate_decades(panel, "alternative")
if (nrow(standard) != 1834L * 12L || nrow(alternative) != 1834L * 12L ||
    !setequal(unique(standard$decade), seq(1850L, 1960L, 10L)) ||
    !setequal(unique(alternative$decade), seq(1850L, 1960L, 10L)) ||
    standard[baseline_covariates_usable == TRUE,
             anyNA(.SD), .SDcols = baseline_variables] ||
    alternative[baseline_covariates_usable == TRUE,
                anyNA(.SD), .SDcols = baseline_variables])
  stop("Decade panels are not balanced 1,834 by 12 grids.")
fwrite(standard, standard_output, na = "")
fwrite(alternative, alternative_output, na = "")

status_counts <- treatment[, .N, by = treatment_status]
qc <- data.table(
  metric = c(
    "annual_rows", "annual_lgds", "annual_years", "standard_decade_rows",
    "alternative_decade_rows", "wikipedia_science_points_candidate",
    "wikipedia_science_points_matched", "wikipedia_births_in_panel",
    "amws_candidates_after_early_dedup_and_1986_filter",
    "amws_exact_place_and_lgd_matches", "amws_early_births_in_panel",
    "amws_1986_births_in_panel", "amws_total_births_in_panel",
    "fair_venue_rows_matched", "fair_host_lgds", "treated_post1870_lgds",
    "pretreated_by1870_lgds", "never_host_lgds",
    "amws_1986_see_previous_excluded", "baseline_covariate_year",
    "baseline_lgds_usable_ge95pct", "baseline_lgds_unusable_below95pct"
  ),
  value = as.character(c(
    nrow(panel), uniqueN(panel$analysis_lgd_id), uniqueN(panel$year),
    nrow(standard), nrow(alternative), nrow(wiki), nrow(wiki_valid),
    sum(panel$n_inventors), nrow(amws_candidates), nrow(amws_valid),
    sum(panel$n_amws_1906_1955_dedup, na.rm = TRUE),
    sum(panel$n_amws_1986, na.rm = TRUE), sum(panel$n_amws, na.rm = TRUE),
    nrow(fair_valid), uniqueN(fair_events$analysis_lgd_id),
    status_counts[treatment_status == "treated_post1870", N],
    status_counts[treatment_status == "pretreated_by1870", N],
    status_counts[treatment_status == "never_host", N], nrow(ed86_excluded),
    1851L, baseline[baseline_covariates_usable == TRUE, .N],
    baseline[baseline_covariates_usable == FALSE, .N]
  ))
)
fwrite(qc, qc_file, na = "")

message("Fixed-1911 LGD world-fairs outcomes stage complete.")
message("  Treatment counts:")
print(status_counts)
message("  Annual: ", annual_output)
message("  Standard decades: ", standard_output)
message("  Alternative decades: ", alternative_output)
message("  QC: ", qc_file)
