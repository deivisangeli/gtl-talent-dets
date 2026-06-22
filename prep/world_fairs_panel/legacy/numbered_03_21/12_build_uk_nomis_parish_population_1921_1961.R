###############################################################################
# Build the observed Nomis parish-level population table, 1921-1961.
#
# This is not a harmonized geographic panel. It extracts the primary parish
# units available in the Nomis historical census CR03 files. ukgeog is checked
# and recorded, but ukgeog 1.0.0 does not expose parish boundary polygons, so
# this script cannot geocode or harmonize parish geometries.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/12_build_uk_nomis_parish_population_1921_1961.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
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
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(
      user_data_dir, winslash = "/", mustWork = TRUE
    )
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  }
}

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
nomis_raw_dir <- file.path(gbr_dir, "raw", "nomis_historical_census")

parish_panel_file <- file.path(
  gbr_dir, "uk_nomis_parish_population_1921_1961.csv"
)
related_units_file <- file.path(
  gbr_dir, "uk_nomis_parish_related_lower_units_1921_1961.csv"
)
summary_file <- file.path(
  gbr_dir, "uk_nomis_parish_population_summary_1921_1961.csv"
)

###############################################################################
# Constants
###############################################################################

census_years <- c(1921L, 1931L, 1951L, 1961L)

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

primary_parish_types <- c("Civil Parish", "Parish")
related_parish_patterns <- paste(c(
  "parish common land",
  "partial intersection.*parish",
  "parish/new town",
  "newtown/parish",
  "ward/parish"
), collapse = "|")

###############################################################################
# Helpers
###############################################################################

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("[^A-Z0-9]+", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}

find_cr03_values <- function(year) {
  extracted_dir <- file.path(nomis_raw_dir, as.character(year), "extracted")
  candidates <- list.files(
    extracted_dir,
    pattern = sprintf("%s_cr03_values[.]csv$", year),
    recursive = TRUE,
    full.names = TRUE
  )
  candidates <- candidates[!grepl("__MACOSX", candidates, fixed = TRUE)]
  if (length(candidates) != 1L) {
    stop("Expected one extracted Nomis CR03 values CSV for ", year)
  }
  candidates[[1L]]
}

read_parish_year <- function(year) {
  values_file <- find_cr03_values(year)
  values <- fread(values_file, na.strings = c("", "NA", ".."))

  pop_col <- population_columns[[as.character(year)]]
  male_col <- male_columns[[as.character(year)]]
  female_col <- female_columns[[as.character(year)]]
  required <- c("area_id", "area", "area_type_id", "area_type",
                pop_col, male_col, female_col)
  missing <- setdiff(required, names(values))
  if (length(missing) > 0L) {
    stop("Missing CR03 columns for ", year, ": ", paste(missing, collapse = ", "))
  }

  values[, area_type_lower := tolower(area_type)]
  values[, is_primary_parish_unit := area_type %chin% primary_parish_types]
  values[, is_related_parish_unit := grepl(
    related_parish_patterns, area_type_lower, ignore.case = TRUE
  )]

  out <- values[
    is_primary_parish_unit == TRUE | is_related_parish_unit == TRUE,
    .(
      census_year = year,
      parish_panel_unit_id = paste(year, area_id, sep = "_"),
      area_id,
      parish_name = as.character(area),
      parish_name_norm = normalize_text(area),
      area_type_id,
      area_type = as.character(area_type),
      population = as.numeric(get(pop_col)),
      male_population = as.numeric(get(male_col)),
      female_population = as.numeric(get(female_col)),
      is_primary_parish_unit,
      is_related_parish_unit,
      source_file = normalizePath(values_file, winslash = "/", mustWork = TRUE),
      geometry_source = NA_character_,
      has_ukgeog_geometry = FALSE,
      geography_harmonized = FALSE
    )
  ]

  out[]
}

###############################################################################
# Build outputs
###############################################################################

ukgeog_installed <- requireNamespace("ukgeog", quietly = TRUE)
ukgeog_has_parish <- FALSE
ukgeog_levels <- NA_character_
if (ukgeog_installed) {
  ukgeog_env <- new.env(parent = emptyenv())
  utils::data("metadata", package = "ukgeog", envir = ukgeog_env)
  ukgeog_metadata <- as.data.table(ukgeog_env$metadata)
  ukgeog_levels <- paste(sort(unique(ukgeog_metadata$geog_short)), collapse = ", ")
  ukgeog_has_parish <- any(grepl(
    "\\bparish\\b|civil parish|\\bPAR\\b",
    paste(ukgeog_metadata, collapse = " "),
    ignore.case = TRUE
  ))
}

all_parish_units <- rbindlist(lapply(census_years, read_parish_year), fill = TRUE)

primary_parishes <- all_parish_units[is_primary_parish_unit == TRUE]
related_units <- all_parish_units[is_primary_parish_unit != TRUE]

fwrite(primary_parishes, parish_panel_file)
fwrite(related_units, related_units_file)

summary <- rbindlist(list(
  primary_parishes[, .(
    unit_class = "primary_parish_units",
    units = .N,
    total_population = sum(population, na.rm = TRUE),
    missing_population = sum(is.na(population)),
    ukgeog_installed = ukgeog_installed,
    ukgeog_has_parish_geometry = ukgeog_has_parish,
    ukgeog_levels = ukgeog_levels,
    geography_harmonized = FALSE
  ), by = census_year],
  related_units[, .(
    unit_class = "related_parish_units_not_in_main_panel",
    units = .N,
    total_population = sum(population, na.rm = TRUE),
    missing_population = sum(is.na(population)),
    ukgeog_installed = ukgeog_installed,
    ukgeog_has_parish_geometry = ukgeog_has_parish,
    ukgeog_levels = ukgeog_levels,
    geography_harmonized = FALSE
  ), by = census_year]
), fill = TRUE)

setorder(summary, census_year, unit_class)
fwrite(summary, summary_file)

cat("Wrote Nomis primary parish population table:\n  ",
    parish_panel_file, "\n", sep = "")
cat("Wrote related parish/intersection units:\n  ",
    related_units_file, "\n", sep = "")
cat("Wrote summary:\n  ", summary_file, "\n", sep = "")
cat("ukgeog installed: ", ukgeog_installed, "\n", sep = "")
cat("ukgeog has parish geometry: ", ukgeog_has_parish, "\n", sep = "")
