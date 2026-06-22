###############################################################################
# Download the Nomis historical census CR03 tables and extend the existing
# Law-Robson-Bennett city panel for England and Wales through 1961.
#
# Run from the repository root or from prep/world_fairs_panel/:
#   Rscript prep/world_fairs_panel/06_download_build_uk_city_population_1921_1961.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(readxl)
})

###############################################################################
# Paths and source files
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

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
raw_dir <- file.path(gbr_dir, "raw", "nomis_historical_census")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

base_panel_file <- file.path(
  gbr_dir,
  "city_population_law_robson_bennett_1801_1911_geocoded.csv"
)
nomis_panel_file <- file.path(gbr_dir, "city_population_nomis_1921_1961.csv")
crosswalk_file <- file.path(gbr_dir, "law_robson_nomis_city_crosswalk.csv")
audit_file <- file.path(gbr_dir, "law_robson_nomis_match_audit.csv")
summary_file <- file.path(gbr_dir, "nomis_city_population_coverage_summary.csv")
combined_file <- file.path(gbr_dir, "city_population_1801_1961_geocoded.csv")
manifest_file <- file.path(raw_dir, "download_manifest.csv")

if (!file.exists(base_panel_file)) {
  stop(
    "Missing the 1801-1911 city panel. Run ",
    "05_build_uk_urban_population_comparison.R first: ",
    base_panel_file
  )
}

census_years <- c(1921L, 1931L, 1951L, 1961L)
source_files <- rbindlist(lapply(census_years, function(year) {
  data.table(
    census_year = year,
    file_type = c("cr03_zip", "metadata"),
    url = c(
      sprintf("https://www.nomisweb.co.uk/output/census/%s/cr03.zip", year),
      sprintf(
        "https://www.nomisweb.co.uk/output/census/%s/%s_metadata.xlsx",
        year,
        year
      )
    ),
    local_name = c("cr03.zip", sprintf("%s_metadata.xlsx", year))
  )
}))

force_download <- tolower(Sys.getenv("UK_CENSUS_FORCE_DOWNLOAD", "false")) %chin%
  c("1", "true", "yes")

download_source <- function(url, destination, force = FALSE) {
  if (!force && file.exists(destination) && file.info(destination)$size > 0L) {
    return("existing")
  }

  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(destination, ".download")
  if (file.exists(temporary)) unlink(temporary)

  status <- tryCatch({
    download.file(url, temporary, mode = "wb", method = "libcurl", quiet = FALSE)
    if (!file.exists(temporary) || file.info(temporary)$size <= 0L) {
      stop("Downloaded file is empty: ", url)
    }
    if (!file.rename(temporary, destination)) {
      stop("Could not move downloaded file to: ", destination)
    }
    "downloaded"
  }, error = function(error) {
    if (file.exists(temporary)) unlink(temporary)
    stop("Download failed for ", url, ": ", conditionMessage(error))
  })
  status
}

cat("Downloading Nomis historical census files...\n")
manifest <- source_files[, {
  year_dir <- file.path(raw_dir, as.character(census_year))
  destination <- file.path(year_dir, local_name)
  download_status <- download_source(url, destination, force_download)
  .(
    local_path = normalizePath(destination, winslash = "/", mustWork = TRUE),
    bytes = file.info(destination)$size,
    sha256 = digest(destination, algo = "sha256", file = TRUE),
    download_status = download_status,
    accessed_on = as.character(Sys.Date())
  )
}, by = .(census_year, file_type, url, local_name)]
fwrite(manifest, manifest_file)

###############################################################################
# Helpers and harmonization rules
###############################################################################

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z0-9]+", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}

canonical_town <- function(x) {
  x <- normalize_text(x)
  x <- sub(" COUNTY CORPORATE$", "", x)
  x <- sub("^THE ", "", x)
  x <- sub("^CITY OF ", "", x)
  x <- sub(" CITY AND COUNTY OF$", "", x)
  x <- sub(" CITY AND COUNT OF$", "", x)
  x <- sub(" CITY OF$", "", x)
  x <- sub(" BOROUGH OF$", "", x)
  x
}

canonical_county <- function(x) {
  x <- normalize_text(x)
  x <- sub("^COUNTY OF ", "", x)
  aliases <- c(
    "CAERNARVONSHIRE" = "CARNARVONSHIRE",
    "DEVONSHIRE" = "DEVON",
    "DORSETSHIRE" = "DORSET",
    "RUTLANDSHIRE" = "RUTLAND",
    "SOMERSETSHIRE" = "SOMERSET",
    "WESTMORELAND" = "WESTMORLAND"
  )
  hit <- match(x, names(aliases))
  replace <- !is.na(hit)
  x[replace] <- unname(aliases[hit[replace]])
  x
}

# Reviewed spelling and naming variants. Composite places are intentionally
# excluded because their administrative populations are not town populations.
town_aliases <- c(
  "BERWICK" = "BERWICK UPON TWEED",
  "BOLTON" = "GREAT BOLTON",
  "BURTON ON TRENT" = "BURTON UPON TRENT",
  "CAERNARVON" = "CARNARVON",
  "DALTON IN FURNESS" = "DALTON",
  "GRAYS THURROCK" = "GRAYS",
  "GUISEBOROUGH" = "GUISBOROUGH",
  "HARTLEPOOLS" = "HARTLEPOOL",
  "HAZEL GROVE" = "HAZELGROVE",
  "HEOLSTON" = "HELSTON",
  "HUCKNALL" = "HUCKNALL TORKARD",
  "KING S LYNN" = "KINGS LYNN",
  "LLANELLI" = "LLANELLY",
  "OTLY" = "OTLEY",
  "ROYAL LEAMINGTON SPA" = "LEAMINGTON",
  "ROYAL TUNBRIDGE WELLS" = "TUNBRIDGE WELLS",
  "SOWEBY" = "SOWERBY",
  "STOCKTON ON TEES" = "STOCKTON",
  "TONBRIDGE" = "TUNBRIDGE",
  "WHITLEY BAY" = "WHITLEY",
  "WHITTICK" = "WHITWICK"
)

apply_town_alias <- function(x) {
  hit <- match(x, names(town_aliases))
  replace <- !is.na(hit)
  x[replace] <- unname(town_aliases[hit[replace]])
  x
}

city_area_types <- c(
  "Urban District",
  "Municipal Borough",
  "County Borough",
  "Metropolitan Borough",
  "County Corporate",
  "London County Corporate"
)

county_parent_priority <- c(
  "Administrative County (excluding County Boroughs)" = 1L,
  "Administrative County" = 2L,
  "Administrative County with any County Boroughs" = 3L,
  "Administrative County with associated County Boroughs" = 4L,
  "Ancient County" = 5L,
  "City and County of York" = 6L
)

population_columns <- c(
  "1921" = "2c3_0003",
  "1931" = "3c3_0003",
  "1951" = "5c3_0003",
  "1961" = "6c3_0003"
)
male_columns <- c(
  "1921" = "2c3_0004",
  "1931" = "3c3_0004",
  "1951" = "5c3_0004",
  "1961" = "6c3_0004"
)
female_columns <- c(
  "1921" = "2c3_0005",
  "1931" = "3c3_0005",
  "1951" = "5c3_0005",
  "1961" = "6c3_0005"
)
expected_england_wales_population <- c(
  "1931" = 39952377,
  "1951" = 43757888,
  "1961" = 46104548
)

extract_cr03 <- function(year, zip_file, extraction_dir) {
  if (dir.exists(extraction_dir)) unlink(extraction_dir, recursive = TRUE)
  dir.create(extraction_dir, recursive = TRUE, showWarnings = FALSE)
  unzip(zip_file, exdir = extraction_dir)
  csv_files <- list.files(
    extraction_dir,
    pattern = sprintf("%s_cr03_values[.]csv$", year),
    recursive = TRUE,
    full.names = TRUE
  )
  csv_files <- csv_files[!grepl("__MACOSX", csv_files, fixed = TRUE)]
  if (length(csv_files) != 1L) {
    stop("Expected one CR03 values CSV for ", year, "; found ", length(csv_files))
  }
  csv_files[[1L]]
}

derive_1951_cr03_id <- function(metadata_id, area_type) {
  prefix <- ifelse(area_type == "County Borough", "H06", "H07")
  paste0(prefix, substr(metadata_id, 1L, 1L), substr(metadata_id, 3L, nchar(metadata_id)))
}

read_year_source <- function(year) {
  cat("Reading and validating ", year, " CR03...\n", sep = "")
  year_dir <- file.path(raw_dir, as.character(year))
  zip_file <- file.path(year_dir, "cr03.zip")
  metadata_file <- file.path(year_dir, sprintf("%s_metadata.xlsx", year))
  extraction_dir <- file.path(year_dir, "extracted")
  values_file <- extract_cr03(year, zip_file, extraction_dir)

  values <- fread(values_file, na.strings = c("", "NA", ".."))
  pop_column <- population_columns[[as.character(year)]]
  male_column <- male_columns[[as.character(year)]]
  female_column <- female_columns[[as.character(year)]]
  required_values <- c(
    "area_id", "area", "area_type_id", "area_type",
    pop_column, male_column, female_column
  )
  missing_values <- setdiff(required_values, names(values))
  if (length(missing_values) > 0L) {
    stop("Missing CR03 columns for ", year, ": ", paste(missing_values, collapse = ", "))
  }

  values[, population := suppressWarnings(as.numeric(get(pop_column)))]
  values[, male_population := suppressWarnings(as.numeric(get(male_column)))]
  values[, female_population := suppressWarnings(as.numeric(get(female_column)))]
  sex_check <- values[
    !is.na(population) & !is.na(male_population) & !is.na(female_population)
  ]
  if (sex_check[, any(population != male_population + female_population)]) {
    stop("CR03 total population does not equal male plus female population in ", year)
  }
  expected_total <- unname(expected_england_wales_population[as.character(year)])
  if (!is.na(expected_total)) {
    observed_total <- values[
      normalize_text(area) == "ENGLAND AND WALES",
      unique(population)
    ]
    observed_total <- observed_total[!is.na(observed_total)]
    if (length(observed_total) != 1L || observed_total != expected_total) {
      stop("England and Wales population total failed validation in ", year)
    }
  }

  areas <- as.data.table(read_excel(
    metadata_file,
    sheet = sprintf("%s_areas", year),
    col_types = "text"
  ))
  relationships <- as.data.table(read_excel(
    metadata_file,
    sheet = sprintf("%s_area_relationships", year),
    col_types = "text"
  ))
  required_areas <- c("area_id", "area", "area_type_id", "area_type")
  required_relationships <- c(
    "area_id_1", "area_1", "area_type_1",
    "area_id_2", "area_2", "area_type_2"
  )
  if (length(setdiff(required_areas, names(areas))) > 0L ||
      length(setdiff(required_relationships, names(relationships))) > 0L) {
    stop("Unexpected Nomis metadata schema for ", year)
  }

  districts <- areas[area_type %chin% city_area_types]
  districts[, metadata_area_id := as.character(area_id)]
  districts[, source_area_id := if (year == 1951L) {
    derive_1951_cr03_id(metadata_area_id, area_type)
  } else {
    metadata_area_id
  }]

  value_districts <- values[area_type %chin% city_area_types, .(
    source_area_id = as.character(area_id),
    value_area = as.character(area),
    value_area_type = as.character(area_type),
    population
  )]
  districts <- merge(
    districts,
    value_districts,
    by = "source_area_id",
    all.x = TRUE,
    sort = FALSE
  )
  if (districts[is.na(population), .N] > 0L) {
    stop(
      "Could not attach CR03 population to ",
      districts[is.na(population), .N],
      " urban areas in ", year
    )
  }
  if (districts[, any(area_type != value_area_type)]) {
    stop("Area types in the metadata and CR03 values disagree in ", year)
  }
  name_disagreements <- districts[
    normalize_text(area) != normalize_text(value_area),
    .N
  ]
  if (name_disagreements > 0L) {
    cat(
      "  Retaining metadata names for ", name_disagreements,
      " CR03 spelling/name variants.\n",
      sep = ""
    )
  }

  parent_candidates <- relationships[
    area_type_1 %chin% names(county_parent_priority) &
      area_type_2 %chin% city_area_types,
    .(
      metadata_area_id = as.character(area_id_2),
      source_county = as.character(area_1),
      source_county_type = as.character(area_type_1),
      parent_priority = unname(county_parent_priority[area_type_1])
    )
  ]
  setorder(parent_candidates, metadata_area_id, parent_priority, source_county)
  parent_candidates <- parent_candidates[, .SD[1L], by = metadata_area_id]

  districts <- merge(
    districts,
    parent_candidates,
    by = "metadata_area_id",
    all.x = TRUE,
    sort = FALSE
  )
  districts[, `:=`(
    census_year = year,
    source_area_name = as.character(area),
    source_area_type = as.character(area_type),
    source_town_match_name = canonical_town(area),
    source_county_match_name = canonical_county(source_county)
  )]
  districts[, .(
    census_year,
    source_area_id,
    metadata_area_id,
    source_area_name,
    source_area_type,
    source_county,
    source_county_type,
    source_town_match_name,
    source_county_match_name,
    population
  )]
}

nomis_sources <- rbindlist(lapply(census_years, function(year) {
  read_year_source(year)
}), use.names = TRUE)

if (nomis_sources[, anyDuplicated(paste(census_year, source_area_id))] > 0L) {
  stop("Duplicate census-year/source-area IDs in Nomis urban source data.")
}
if (nomis_sources[is.na(population) | population < 0, .N] > 0L) {
  stop("Missing or negative urban population values in Nomis CR03 data.")
}

###############################################################################
# Match the 934-city universe to census urban districts
###############################################################################

cat("Matching Nomis urban districts to the 934-city panel...\n")
base_panel <- fread(base_panel_file, na.strings = c("", "NA"))
required_base <- c(
  "city_id", "town_name", "standard_name", "historic_county",
  "longitude", "latitude", "geocode_status", "geocode_method"
)
missing_base <- setdiff(required_base, names(base_panel))
if (length(missing_base) > 0L) {
  stop("Missing base-panel columns: ", paste(missing_base, collapse = ", "))
}

cities <- unique(base_panel[, ..required_base], by = "city_id")
if (nrow(cities) != 934L || uniqueN(cities$city_id) != 934L) {
  stop("Expected 934 unique cities in the base panel; found ", nrow(cities))
}
cities[, `:=`(
  town_name_normalized = canonical_town(town_name),
  standard_name_normalized = canonical_town(standard_name),
  county_match_name = canonical_county(historic_county)
)]
cities[, town_match_name := apply_town_alias(town_name_normalized)]
cities[, standard_match_name := apply_town_alias(standard_name_normalized)]

city_years <- CJ(
  city_id = cities$city_id,
  census_year = census_years,
  unique = TRUE
)
city_years <- merge(city_years, cities, by = "city_id", all.x = TRUE, sort = FALSE)

city_names <- rbindlist(list(
  cities[, .(
    city_id,
    city_name_candidate = town_match_name,
    city_name_field = "town_name",
    city_town_match_name = town_match_name
  )],
  cities[, .(
    city_id,
    city_name_candidate = standard_match_name,
    city_name_field = "standard_name",
    city_town_match_name = town_match_name
  )]
))
city_names <- unique(city_names[nzchar(city_name_candidate)])

name_candidates <- merge(
  city_names,
  nomis_sources,
  by.x = "city_name_candidate",
  by.y = "source_town_match_name",
  allow.cartesian = TRUE
)
name_candidates <- merge(
  name_candidates,
  cities[, .(city_id, city_county_match_name = county_match_name)],
  by = "city_id",
  all.x = TRUE
)
name_candidates[, county_agrees :=
  nzchar(city_county_match_name) &
  nzchar(source_county_match_name) &
  city_county_match_name == source_county_match_name]
name_candidates <- unique(
  name_candidates,
  by = c("city_id", "census_year", "source_area_id")
)

candidate_stats <- name_candidates[, .(
  name_candidate_count = uniqueN(source_area_id),
  county_candidate_count = uniqueN(source_area_id[county_agrees])
), by = .(city_id, census_year)]

county_matches <- name_candidates[county_agrees == TRUE]
county_matches[, county_candidate_count := uniqueN(source_area_id),
               by = .(city_id, census_year)]
selected_county <- county_matches[county_candidate_count == 1L]
selected_county[, match_method := fifelse(
  city_name_field == "town_name" & city_name_candidate == city_town_match_name,
  "exact_name_and_county",
  "reviewed_alias_or_standard_name_and_county"
)]

already_selected <- unique(selected_county[, .(city_id, census_year)])
remaining_candidates <- name_candidates[
  !already_selected,
  on = .(city_id, census_year)
]
remaining_candidates[, source_name_count := uniqueN(source_area_id),
                     by = .(city_id, census_year)]
remaining_candidates[, city_name_count := uniqueN(city_id),
                     by = .(census_year, city_name_candidate)]
selected_unique_name <- remaining_candidates[
  source_name_count == 1L & city_name_count == 1L
]
selected_unique_name[, match_method := "unique_name_without_county_agreement"]

selected <- rbindlist(
  list(selected_county, selected_unique_name),
  use.names = TRUE,
  fill = TRUE
)
selected <- unique(selected, by = c("city_id", "census_year", "source_area_id"))
selected[, selected_count := uniqueN(source_area_id), by = .(city_id, census_year)]
selected <- selected[selected_count == 1L]
selected[, source_reuse_count := uniqueN(city_id),
         by = .(census_year, source_area_id)]

safe_selected <- selected[source_reuse_count == 1L]
crosswalk <- safe_selected[, .(
  city_id,
  census_year,
  source_area_id,
  metadata_area_id,
  source_area_name,
  source_area_type,
  source_county,
  source_county_type,
  match_method,
  population
)]
setorder(crosswalk, census_year, city_id)

audit <- merge(
  city_years,
  candidate_stats,
  by = c("city_id", "census_year"),
  all.x = TRUE,
  sort = FALSE
)
audit <- merge(
  audit,
  selected[, .(
    city_id,
    census_year,
    selected_source_area_id = source_area_id,
    selected_source_area_name = source_area_name,
    selected_source_area_type = source_area_type,
    selected_source_county = source_county,
    selected_match_method = match_method,
    source_reuse_count
  )],
  by = c("city_id", "census_year"),
  all.x = TRUE,
  sort = FALSE
)
audit[is.na(name_candidate_count), name_candidate_count := 0L]
audit[is.na(county_candidate_count), county_candidate_count := 0L]
audit[, match_status := fcase(
  !is.na(selected_source_area_id) & source_reuse_count == 1L, "matched",
  !is.na(selected_source_area_id) & source_reuse_count > 1L,
    "ambiguous_reused_source_area",
  county_candidate_count > 1L, "ambiguous_name_and_county",
  name_candidate_count > 1L, "ambiguous_name",
  name_candidate_count == 1L, "unmatched_county_conflict",
  default = "unmatched_name"
)]
setorder(audit, census_year, city_id)

###############################################################################
# Panels and coverage outputs
###############################################################################

nomis_panel <- merge(
  city_years,
  crosswalk,
  by = c("city_id", "census_year"),
  all.x = TRUE,
  sort = FALSE
)
nomis_panel <- merge(
  nomis_panel,
  audit[, .(city_id, census_year, match_status)],
  by = c("city_id", "census_year"),
  all.x = TRUE,
  sort = FALSE
)
nomis_panel[, `:=`(
  country_iso3 = "GBR",
  country_name = "United Kingdom",
  population_available = !is.na(population),
  population_source = "Nomis historical census CR03",
  population_geography = source_area_type,
  population_match_method = match_method,
  coordinate_crs = "EPSG:4326"
)]
setorder(nomis_panel, city_id, census_year)

if (nrow(nomis_panel) != 934L * length(census_years)) {
  stop("Unexpected number of rows in the 1921-1961 city panel.")
}
if (nomis_panel[, anyDuplicated(paste(city_id, census_year))] > 0L) {
  stop("Duplicate city/year rows in the Nomis city panel.")
}

coverage_summary <- nomis_panel[, .(
  total_cities = uniqueN(city_id),
  matched_cities = sum(population_available),
  unmatched_cities = sum(!population_available),
  coverage_pct = 100 * mean(population_available),
  urban_districts_used = uniqueN(source_area_id[population_available])
), by = census_year]
setorder(coverage_summary, census_year)

base_for_combined <- copy(base_panel)
base_for_combined[, `:=`(
  source_area_id = NA_character_,
  metadata_area_id = NA_character_,
  source_area_name = NA_character_,
  source_area_type = NA_character_,
  source_county = NA_character_,
  source_county_type = NA_character_,
  match_method = population_match_method,
  match_status = fifelse(population_available, "observed", "missing"),
  population_geography = "Law-Robson-Bennett urban settlement"
)]
combined <- rbindlist(
  list(base_for_combined, nomis_panel),
  use.names = TRUE,
  fill = TRUE
)
setorder(combined, city_id, census_year)
if (combined[, anyDuplicated(paste(city_id, census_year))] > 0L) {
  stop("Duplicate city/year rows in the combined 1801-1961 panel.")
}

london_reference_file <- file.path(gbr_dir, "raw", "population_1801_to_2021.xlsx")
if (file.exists(london_reference_file)) {
  london_reference <- as.data.table(read_excel(london_reference_file, sheet = "data"))
  london_reference <- melt(
    london_reference[area == "City of London"],
    id.vars = "area",
    measure.vars = as.character(census_years),
    variable.name = "census_year",
    value.name = "reference_population",
    variable.factor = FALSE
  )
  london_reference[, census_year := as.integer(census_year)]
  london_check <- merge(
    nomis_panel[town_name == "LONDON", .(census_year, population)],
    london_reference[, .(census_year, reference_population)],
    by = "census_year",
    all = TRUE
  )
  london_check[, absolute_difference := abs(population - reference_population)]
  if (london_check[
    is.na(population) | is.na(reference_population) | absolute_difference > 500,
    .N
  ] > 0L) {
    print(london_check)
    stop("City of London populations failed the existing ONS workbook check.")
  }
}

cat("Writing city panels and audit files...\n")
fwrite(nomis_panel, nomis_panel_file)
fwrite(crosswalk, crosswalk_file)
fwrite(audit, audit_file)
fwrite(coverage_summary, summary_file)
fwrite(combined, combined_file)

cat("\nCompleted Nomis city population extension.\n")
print(coverage_summary)
cat("Raw files and manifest:", raw_dir, "\n")
cat("Nomis panel:", nomis_panel_file, "\n")
cat("Crosswalk:", crosswalk_file, "\n")
cat("Audit:", audit_file, "\n")
cat("Combined panel:", combined_file, "\n")
