###############################################################################
# Download and normalize historical local census population data for France
# and the United Kingdom, 1790-1960.
#
# Run from the repository root or from prep/world_fairs_panel/:
#   Rscript prep/world_fairs_panel/04_download_france_uk_city_census.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

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
    "C:/Users",
    Sys.info()[["user"]],
    "Globtalent Dropbox",
    "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(
      user_data_dir,
      winslash = "/",
      mustWork = TRUE
    )
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  }
}

if (!dir.exists(DATA_INPUT)) {
  stop("Dropbox input directory not found: ", DATA_INPUT)
}

root_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census")
fra_dir <- file.path(root_dir, "FRA")
gbr_dir <- file.path(root_dir, "GBR")
fra_raw_dir <- file.path(fra_dir, "raw")
gbr_raw_dir <- file.path(gbr_dir, "raw")
gbr_pop_past_dir <- file.path(gbr_raw_dir, "populations_past")

for (path in c(root_dir, fra_dir, gbr_dir, fra_raw_dir, gbr_raw_dir, gbr_pop_past_dir)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

fra_url <- paste0(
  "https://www.insee.fr/fr/statistiques/fichier/3698339/",
  "base-pop-historiques-1876-2023.xlsx"
)
fra_file <- file.path(fra_raw_dir, "base-pop-historiques-1876-2023.xlsx")

pop_past_zip_url <- paste0(
  "https://api.repository.cam.ac.uk/server/api/core/bitstreams/",
  "7bccd25c-bfde-468f-b23b-2e11e6326bca/content"
)
pop_past_readme_url <- paste0(
  "https://api.repository.cam.ac.uk/server/api/core/bitstreams/",
  "ac5a6dd2-1c2b-4ff3-8d23-90ff1939a70d/content"
)
pop_past_guide_url <- paste0(
  "https://api.repository.cam.ac.uk/server/api/core/bitstreams/",
  "3e50c0bf-dbb7-4a31-b782-11f490e9fe5a/content"
)
pop_past_zip <- file.path(gbr_raw_dir, "PopulationsPast_data.zip")
pop_past_readme <- file.path(gbr_raw_dir, "PopulationsPastData_readme.txt")
pop_past_guide <- file.path(gbr_raw_dir, "PopulationsPastData_userguide.pdf")

london_url <- paste0(
  "https://data.london.gov.uk/download/expjm/",
  "2c7867e5-3682-4fdd-8b9d-c63e289b92a6/",
  "population%201801%20to%202021.xlsx"
)
london_file <- file.path(gbr_raw_dir, "population_1801_to_2021.xlsx")

fra_out <- file.path(fra_dir, "city_census_population_1876_1954.csv")
gbr_out <- file.path(gbr_dir, "city_census_population_1801_1951.csv")
stale_gbr_out <- file.path(gbr_dir, "city_census_population_1801_1911.csv")
combined_out <- file.path(root_dir, "city_census_population_1790_1960.csv")
crosswalk_out <- file.path(root_dir, "fair_city_census_crosswalk.csv")
sources_out <- file.path(root_dir, "sources.csv")
validation_out <- file.path(root_dir, "validation_summary.csv")
audit_out <- file.path(root_dir, "city_census_population_1790_1960_audit.xlsx")

###############################################################################
# Helpers
###############################################################################

download_if_needed <- function(url, destination) {
  if (file.exists(destination) && file.info(destination)$size > 0) {
    cat("Using existing file:", destination, "\n")
    return(invisible(destination))
  }

  cat("Downloading:", url, "\n")
  temporary <- paste0(destination, ".download")
  on.exit(unlink(temporary), add = TRUE)
  utils::download.file(url, temporary, mode = "wb", quiet = FALSE)
  if (!file.exists(temporary) || file.info(temporary)$size == 0) {
    stop("Downloaded file is empty: ", url)
  }
  if (!file.rename(temporary, destination)) {
    stop("Could not move completed download to: ", destination)
  }
  invisible(destination)
}

normalize_text <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z0-9]+", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}

checksum_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}

empty_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

###############################################################################
# Download original files
###############################################################################

download_if_needed(fra_url, fra_file)
download_if_needed(pop_past_zip_url, pop_past_zip)
download_if_needed(pop_past_readme_url, pop_past_readme)
download_if_needed(pop_past_guide_url, pop_past_guide)
download_if_needed(london_url, london_file)

expected_pop_past_files <- sprintf(
  "PopulationsPast_census_data_%s.csv",
  c(1851L, 1861L, 1871L, 1881L, 1891L, 1901L, 1911L)
)
if (!all(file.exists(file.path(gbr_pop_past_dir, expected_pop_past_files)))) {
  cat("Extracting Populations Past archive...\n")
  utils::unzip(pop_past_zip, exdir = gbr_pop_past_dir, overwrite = TRUE)
}

###############################################################################
# France: INSEE historical municipal populations
###############################################################################

cat("Normalizing INSEE historical commune populations...\n")
fra_wide <- as.data.table(readxl::read_excel(
  fra_file,
  sheet = "pop_1876_2023",
  skip = 5
))

fra_population_cols <- grep("^PTOT[0-9]{4}$", names(fra_wide), value = TRUE)
fra_years <- as.integer(sub("^PTOT", "", fra_population_cols))
fra_population_cols <- fra_population_cols[fra_years >= 1790L & fra_years <= 1960L]

if (length(fra_population_cols) == 0L) {
  stop("No INSEE total-population columns were found for 1790-1960.")
}

fra_long <- melt(
  fra_wide,
  id.vars = c("CODGEO", "REG", "DEP", "LIBGEO"),
  measure.vars = fra_population_cols,
  variable.name = "source_variable",
  value.name = "population",
  variable.factor = FALSE
)
fra_long[, `:=`(
  country_iso3 = "FRA",
  country_name = "France",
  census_year = as.integer(sub("^PTOT", "", source_variable)),
  unit_id = as.character(CODGEO),
  unit_name = as.character(LIBGEO),
  unit_type = fifelse(grepl("^751(0[1-9]|1[0-9]|20)$", CODGEO),
                      "municipal_arrondissement", "commune"),
  parent_unit_id = as.character(DEP),
  parent_unit_name = NA_character_,
  region_id = as.character(REG),
  population = as.numeric(population),
  population_concept = "total population including floating population",
  geography_reference = "2025 commune geography",
  source_agency = "INSEE",
  source_id = "insee_historical_commune_population_1876_2023",
  source_title = "Historique des populations communales - Recensements de la population 1876-2023",
  source_url = fra_url,
  raw_file = file.path("FRA", "raw", basename(fra_file)),
  boundary_note = "INSEE supplies a stable geography as of 2025; Paris is reported by municipal arrondissement."
)]

fra_normalized <- fra_long[!is.na(population), .(
  country_iso3, country_name, census_year, unit_id, unit_name, unit_type,
  parent_unit_id, parent_unit_name, region_id, population,
  population_concept, geography_reference, source_agency, source_id,
  source_title, source_url, raw_file, boundary_note
)]
setorder(fra_normalized, census_year, unit_id)

###############################################################################
# United Kingdom: Populations Past plus official London series
###############################################################################

cat("Normalizing Populations Past census files...\n")
uk_years <- c(1851L, 1861L, 1871L, 1881L, 1891L, 1901L, 1911L)
uk_parts <- lapply(uk_years, function(year) {
  path <- file.path(
    gbr_pop_past_dir,
    sprintf("PopulationsPast_census_data_%s.csv", year)
  )
  x <- fread(path, na.strings = c("", "NA"), encoding = "UTF-8")
  id_col <- sprintf("CEN_%s", year)
  required <- c(id_col, "REGCNTY", "REGDIST", "SUBDIST", "POP")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop("Missing columns in ", basename(path), ": ", paste(missing, collapse = ", "))
  }

  x[, subdistrict_clean := empty_to_na(SUBDIST)]
  x[, .(
    country_iso3 = "GBR",
    country_name = "United Kingdom",
    census_year = year,
    unit_id = as.character(get(id_col)),
    unit_name = fifelse(!is.na(subdistrict_clean), subdistrict_clean, as.character(REGDIST)),
    unit_type = fifelse(!is.na(subdistrict_clean), "registration_subdistrict", "registration_district"),
    parent_unit_id = NA_character_,
    parent_unit_name = fifelse(!is.na(subdistrict_clean), as.character(REGDIST), as.character(REGCNTY)),
    region_id = as.character(REGCNTY),
    population = as.numeric(POP),
    population_concept = "census population (POP variable)",
    geography_reference = "census-year RSD geography for England/Wales; consistent registration districts for Scotland",
    source_agency = "University of Cambridge, Populations Past",
    source_id = "populations_past_2025",
    source_title = "Populations Past Data: Demographic and Socio-economic Data for Registration Sub-districts of England and Wales and Registration Districts of Scotland",
    source_url = "https://doi.org/10.17863/CAM.116164",
    raw_file = file.path("GBR", "raw", "populations_past", basename(path)),
    boundary_note = fifelse(
      year == 1911L,
      "Scotland is not included in the 1911 source file.",
      "Registration subdistrict boundaries may change between census years."
    )
  )]
})
uk_pop_past <- rbindlist(uk_parts, use.names = TRUE)
uk_pop_past <- uk_pop_past[!is.na(population)]

cat("Normalizing official London historical census series...\n")
london_wide <- as.data.table(readxl::read_excel(london_file, sheet = "data"))
london_year_cols <- names(london_wide)[grepl("^[0-9]{4}$", names(london_wide))]
london_census_years <- c(seq(1801L, 1931L, by = 10L), 1951L)
london_year_cols <- london_year_cols[as.integer(london_year_cols) %in% london_census_years]

london_long <- melt(
  london_wide,
  id.vars = "area",
  measure.vars = london_year_cols,
  variable.name = "census_year",
  value.name = "population",
  variable.factor = FALSE
)
london_long[, `:=`(
  country_iso3 = "GBR",
  country_name = "United Kingdom",
  census_year = as.integer(census_year),
  unit_id = paste0("LONDON_DATASTORE_", normalize_text(area)),
  unit_name = as.character(area),
  unit_type = fifelse(area %chin% c("Greater London", "Central London", "Rest of Inner London", "Outer London"),
                      "london_aggregate", "london_borough"),
  parent_unit_id = NA_character_,
  parent_unit_name = fifelse(area == "Greater London", NA_character_, "Greater London"),
  region_id = "LONDON",
  population = as.numeric(population),
  population_concept = "persons present",
  geography_reference = "current London borough and Greater London definitions used by source",
  source_agency = "Office for National Statistics / London Datastore",
  source_id = "london_historical_census_population",
  source_title = "Historical Census Population",
  source_url = "https://data.london.gov.uk/dataset/historical-census-population-expjm",
  raw_file = file.path("GBR", "raw", basename(london_file)),
  boundary_note = "Historical census counts reconstructed for London areas published in the source workbook."
)]

london_normalized <- london_long[!is.na(population), .(
  country_iso3, country_name, census_year, unit_id, unit_name, unit_type,
  parent_unit_id, parent_unit_name, region_id, population,
  population_concept, geography_reference, source_agency, source_id,
  source_title, source_url, raw_file, boundary_note
)]

gbr_normalized <- rbindlist(
  list(uk_pop_past, london_normalized),
  use.names = TRUE,
  fill = TRUE
)
setorder(gbr_normalized, source_id, census_year, unit_name)

###############################################################################
# Source manifest
###############################################################################

sources <- data.table(
  country_iso3 = c("FRA", "GBR", "GBR", "GBR", "GBR"),
  source_priority = c("official", "official", "academic_census_derived", "official_reference", "official_census_transcription"),
  source_agency = c(
    "INSEE",
    "Office for National Statistics / London Datastore",
    "University of Cambridge, Populations Past",
    "Office for National Statistics",
    "UK Data Service / Great Britain Historical GIS"
  ),
  source_title = c(
    "Historique des populations communales - Recensements de la population 1876-2023",
    "Historical Census Population",
    "Populations Past Data",
    "Census data 1801 to 1991",
    "Great Britain Historical Database: Parish-Level Population Statistics, 1801-1951"
  ),
  source_url = c(
    fra_url,
    "https://data.london.gov.uk/dataset/historical-census-population-expjm",
    "https://doi.org/10.17863/CAM.116164",
    "https://www.ons.gov.uk/census/2011census/2011censusdata/censusdata18011991",
    "https://doi.org/10.5255/UKDA-SN-4560-1"
  ),
  coverage = c(
    "France communes, 1876-2023; normalized through 1954",
    "London areas, 1801-2021; normalized through 1951",
    "England/Wales RSDs 1851-1911 and Scotland RDs 1851-1901",
    "Historical census reports and access guidance",
    "Great Britain parish-level census data, 1801-1951"
  ),
  local_file = c(
    file.path("FRA", "raw", basename(fra_file)),
    file.path("GBR", "raw", basename(london_file)),
    file.path("GBR", "raw", basename(pop_past_zip)),
    NA_character_,
    NA_character_
  ),
  access_status = c(
    "downloaded",
    "downloaded",
    "downloaded",
    "reference_only",
    "unavailable_for_download_as_of_access_date"
  ),
  access_date = as.character(Sys.Date()),
  md5 = c(
    checksum_or_na(fra_file),
    checksum_or_na(london_file),
    checksum_or_na(pop_past_zip),
    NA_character_,
    NA_character_
  ),
  notes = c(
    "Official INSEE workbook; historical values are supplied on 2025 commune geography.",
    "Official ONS-derived London series; 1961 is excluded because the requested endpoint is 1960.",
    "Academic structured dataset derived mainly from enhanced census microdata and official reports.",
    "ONS points users to Histpop and Vision of Britain for historical local census reports.",
    "The catalogue states that the structured data are currently unavailable for download."
  )
)

###############################################################################
# Fair-city crosswalk and deferred Ireland records
###############################################################################

fair_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia_geocoded.xlsx")
if (!file.exists(fair_file)) {
  stop("Fair geography file not found: ", fair_file)
}

fair_geo <- as.data.table(readxl::read_excel(fair_file))
fair_cities <- unique(fair_geo[Year >= 1790L & Year <= 1910L, .(
  fair_city = as.character(City),
  fair_country = as.character(Country),
  matched_country_iso3 = as.character(matched_country_iso3)
)])
fair_cities[, fair_city_normalized := normalize_text(fair_city)]

fra_units <- unique(fra_normalized[, .(
  candidate_id = unit_id,
  candidate_name = unit_name,
  candidate_type = unit_type,
  candidate_normalized = normalize_text(unit_name)
)])

uk_units <- unique(gbr_normalized[, .(
  candidate_id = unit_id,
  candidate_name = unit_name,
  candidate_type = unit_type,
  candidate_parent = parent_unit_name,
  candidate_normalized = normalize_text(unit_name),
  parent_normalized = normalize_text(parent_unit_name)
)])

uk_alias <- c(
  "NEWCASTLE" = "NEWCASTLE UPON TYNE",
  "SOUTH KENSINGTON" = "KENSINGTON",
  "KILBURN" = "KILBURN"
)

crosswalk_rows <- lapply(seq_len(nrow(fair_cities)), function(i) {
  fair <- fair_cities[i]
  city_norm <- fair$fair_city_normalized

  if (city_norm %chin% c("DUBLIN", "CORK")) {
    return(data.table(
      fair_city = fair$fair_city,
      fair_country = fair$fair_country,
      fair_country_iso3 = "IRL",
      census_country_iso3 = "IRL",
      match_status = "deferred_ireland",
      match_method = "country_scope_decision",
      candidate_count = 0L,
      census_unit_candidates = NA_character_,
      match_note = "Ireland was explicitly deferred from this implementation round."
    ))
  }

  if (fair$matched_country_iso3 %chin% "FRA" || fair$fair_country %chin% "France") {
    candidates <- if (city_norm == "PARIS") {
      fra_units[grepl("^751(0[1-9]|1[0-9]|20)$", candidate_id)]
    } else {
      fra_units[candidate_normalized == city_norm]
    }
    return(data.table(
      fair_city = fair$fair_city,
      fair_country = fair$fair_country,
      fair_country_iso3 = "FRA",
      census_country_iso3 = "FRA",
      match_status = fifelse(
        nrow(candidates) > 0L,
        fifelse(city_norm == "PARIS", "matched_aggregate_required", "matched_exact_name"),
        "unmatched"
      ),
      match_method = fifelse(city_norm == "PARIS", "aggregate_municipal_arrondissements", "normalized_exact_commune_name"),
      candidate_count = nrow(candidates),
      census_unit_candidates = if (nrow(candidates) > 0L) {
        paste(sprintf("%s [%s]", candidates$candidate_name, candidates$candidate_id), collapse = " | ")
      } else NA_character_,
      match_note = if (city_norm == "PARIS") {
        "INSEE reports Paris by municipal arrondissement; aggregate arrondissements before city-level use."
      } else NA_character_
    ))
  }

  if (fair$matched_country_iso3 %chin% "GBR" || fair$fair_country %chin% "United Kingdom") {
    lookup <- if (city_norm %in% names(uk_alias)) unname(uk_alias[[city_norm]]) else city_norm
    candidates <- uk_units[
      candidate_normalized == lookup |
        parent_normalized == lookup |
        startsWith(candidate_normalized, paste0(lookup, " ")) |
        (lookup == "LONDON" & candidate_normalized == "GREATER LONDON")
    ]
    candidates <- unique(candidates, by = c("candidate_id", "candidate_name", "candidate_type"))
    return(data.table(
      fair_city = fair$fair_city,
      fair_country = fair$fair_country,
      fair_country_iso3 = "GBR",
      census_country_iso3 = "GBR",
      match_status = fifelse(nrow(candidates) > 0L, "matched_candidates", "unmatched"),
      match_method = fifelse(lookup == city_norm, "normalized_unit_or_parent_name", "manual_alias_then_unit_or_parent_name"),
      candidate_count = nrow(candidates),
      census_unit_candidates = if (nrow(candidates) > 0L) {
        paste(unique(sprintf("%s [%s]", candidates$candidate_name, candidates$candidate_type)), collapse = " | ")
      } else NA_character_,
      match_note = fifelse(
        city_norm == "LONDON",
        "Prefer the official Greater London series for a city-level London total.",
        fifelse(city_norm %chin% c("SOUTH KENSINGTON", "KILBURN"),
                "Sub-city fair location; do not treat the matched unit as a whole-city total without review.",
                NA_character_)
      )
    ))
  }

  NULL
})

crosswalk <- rbindlist(crosswalk_rows, use.names = TRUE, fill = TRUE)
setorder(crosswalk, census_country_iso3, fair_city, fair_country)

###############################################################################
# Validation and output
###############################################################################

combined <- rbindlist(list(fra_normalized, gbr_normalized), use.names = TRUE)
combined <- combined[census_year >= 1790L & census_year <= 1960L]
setorder(combined, country_iso3, source_id, census_year, unit_id)

duplicate_rows <- combined[, .N, by = .(
  country_iso3, source_id, census_year, unit_id, population_concept
)][N > 1L]

validation_population_rows <- combined[, .(
    value = .N,
    detail = sprintf("%s-%s", min(census_year), max(census_year))
  ), by = country_iso3]
validation_population_rows[, metric := "population_rows"]

validation_distinct_units <- combined[, .(
    value = uniqueN(unit_id),
    detail = paste(sort(unique(census_year)), collapse = ",")
  ), by = country_iso3]
validation_distinct_units[, metric := "distinct_units"]

validation <- rbindlist(list(
  validation_population_rows,
  validation_distinct_units,
  data.table(
    country_iso3 = "ALL",
    metric = c("duplicate_keys", "invalid_population", "deferred_ireland_crosswalk_rows", "unmatched_crosswalk_rows"),
    value = c(
      nrow(duplicate_rows),
      combined[is.na(population) | population < 0, .N],
      crosswalk[match_status == "deferred_ireland", .N],
      crosswalk[match_status == "unmatched", .N]
    ),
    detail = c(
      "key: country/source/year/unit/concept",
      "missing or population < 0; zero is retained as a valid census count",
      "Dublin and Cork records",
      "requires manual review"
    )
  )
), use.names = TRUE, fill = TRUE)

if (nrow(duplicate_rows) > 0L) {
  stop("Duplicate normalized source keys found; inspect before publishing output.")
}
if (combined[is.na(population) | population < 0, .N] > 0L) {
  stop("Missing or negative population values remain in normalized output.")
}

fwrite(fra_normalized, fra_out)
fwrite(gbr_normalized, gbr_out)
if (file.exists(stale_gbr_out)) unlink(stale_gbr_out)
fwrite(combined, combined_out)
fwrite(crosswalk, crosswalk_out)
fwrite(sources, sources_out)
fwrite(validation, validation_out)

cat("Writing XLSX audit workbook...\n")
writexl::write_xlsx(
  list(
    population = as.data.frame(combined),
    sources = as.data.frame(sources),
    fair_city_crosswalk = as.data.frame(crosswalk),
    validation = as.data.frame(validation)
  ),
  audit_out
)

cat("\nCompleted historical city census data build.\n")
cat("Combined CSV:", combined_out, "\n")
cat("Audit XLSX:", audit_out, "\n")
cat("Rows:", format(nrow(combined), big.mark = ","), "\n")
cat("France units:", uniqueN(fra_normalized$unit_id), "\n")
cat("UK units:", uniqueN(gbr_normalized$unit_id), "\n")
cat("Crosswalk unmatched:", crosswalk[match_status == "unmatched", .N], "\n")
cat("Ireland deferred rows:", crosswalk[match_status == "deferred_ireland", .N], "\n")
