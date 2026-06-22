###############################################################################
# Check whether the UK urban-population panel can be rebuilt at parish level
# using ukgeog boundaries.
#
# This script deliberately does not overwrite the current LAU panel. It records
# whether ukgeog exposes parish polygons and summarizes the lower-level Nomis
# population units available for 1921, 1931, 1951, and 1961.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/11_check_ukgeog_parish_population_panel_feasibility.R
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

feasibility_file <- file.path(
  gbr_dir, "uk_parish_population_feasibility_ukgeog.csv"
)
nomis_lower_units_file <- file.path(
  gbr_dir, "uk_parish_population_nomis_lower_unit_counts.csv"
)
ukgeog_levels_file <- file.path(
  gbr_dir, "ukgeog_available_boundary_levels.csv"
)

###############################################################################
# ukgeog metadata
###############################################################################

read_ukgeog_metadata <- function() {
  if (requireNamespace("ukgeog", quietly = TRUE)) {
    env <- new.env(parent = emptyenv())
    tryCatch({
      utils::data("metadata", package = "ukgeog", envir = env)
      metadata <- as.data.table(env$metadata)
      metadata[, metadata_source := "installed_package"]
      metadata
    }, error = function(error) {
      data.table(
        geog_short = character(),
        geog = character(),
        boundary_type = character(),
        type = character(),
        tag = character(),
        month = character(),
        metadata_source = character()
      )
    })
  } else {
    data.table(
      geog_short = character(),
      geog = character(),
      boundary_type = character(),
      type = character(),
      tag = character(),
      month = character(),
      metadata_source = character()
    )
  }
}

read_ukgeog_source_metadata <- function() {
  source_dir <- Sys.getenv(
    "UKGEOG_SOURCE_DIR",
    unset = file.path(Sys.getenv("TEMP"), "ukgeog_src")
  )
  metadata_rda <- file.path(source_dir, "data", "metadata.rda")
  if (!file.exists(metadata_rda)) {
    return(data.table())
  }

  env <- new.env(parent = emptyenv())
  load(metadata_rda, envir = env)
  if (!exists("metadata", envir = env, inherits = FALSE)) {
    return(data.table())
  }

  metadata <- as.data.table(env$metadata)
  metadata[, metadata_source := normalizePath(
    metadata_rda, winslash = "/", mustWork = TRUE
  )]
  metadata
}

ukgeog_installed <- requireNamespace("ukgeog", quietly = TRUE)
ukgeog_metadata <- read_ukgeog_metadata()

if (nrow(ukgeog_metadata) == 0L) {
  ukgeog_metadata <- read_ukgeog_source_metadata()
}

if (nrow(ukgeog_metadata) > 0L) {
  fwrite(ukgeog_metadata, ukgeog_levels_file)
}

ukgeog_text <- if (nrow(ukgeog_metadata) > 0L) {
  paste(ukgeog_metadata$geog_short, ukgeog_metadata$geog, collapse = " | ")
} else {
  ""
}
ukgeog_has_parish <- grepl(
  "\\bparish\\b|\\bparishes\\b|civil parish",
  ukgeog_text,
  ignore.case = TRUE
)

###############################################################################
# Nomis lower-unit availability
###############################################################################

census_years_nomis <- c(1921L, 1931L, 1951L, 1961L)

classify_area_type <- function(area_type) {
  type <- tolower(area_type)
  fcase(
    grepl("enumeration district", type), "enumeration_district",
    grepl("civil parish|^parish$|parish common land|parish/", type),
      "parish_like",
    grepl("\\bward\\b|ward/", type), "ward_like",
    grepl("urban district|municipal borough|county borough|metropolitan borough",
          type),
      "urban_district_like",
    grepl("rural district", type), "rural_district_like",
    default = "other"
  )
}

read_nomis_area_counts <- function(year) {
  metadata_file <- file.path(
    nomis_raw_dir, as.character(year), sprintf("%s_metadata.xlsx", year)
  )
  if (!file.exists(metadata_file)) {
    return(data.table(
      census_year = year,
      area_type_id = NA_integer_,
      area_type = NA_character_,
      area_count = NA_integer_,
      lower_unit_class = "metadata_missing"
    ))
  }

  sheets <- excel_sheets(metadata_file)
  areas_sheet <- sheets[grepl("_areas$", sheets)]
  if (length(areas_sheet) != 1L) {
    stop("Could not identify the areas sheet in: ", metadata_file)
  }

  areas <- as.data.table(read_excel(metadata_file, sheet = areas_sheet))
  area_counts <- areas[, .(area_count = .N), by = .(area_type_id, area_type)]
  area_counts[, census_year := year]
  setcolorder(area_counts, c(
    "census_year", "area_type_id", "area_type", "area_count"
  ))
  area_counts[, lower_unit_class := classify_area_type(area_type)]
  area_counts[]
}

nomis_area_counts <- rbindlist(
  lapply(census_years_nomis, read_nomis_area_counts),
  fill = TRUE
)

nomis_lower_units <- nomis_area_counts[
  lower_unit_class %chin% c(
    "parish_like", "ward_like", "enumeration_district",
    "urban_district_like", "rural_district_like"
  )
][order(census_year, lower_unit_class, area_type)]

fwrite(nomis_lower_units, nomis_lower_units_file)

nomis_has_parish_population <- nomis_area_counts[
  lower_unit_class == "parish_like",
  any(area_count > 0, na.rm = TRUE),
  by = census_year
][, all(V1)]

###############################################################################
# Feasibility report
###############################################################################

status <- if (!ukgeog_installed) {
  "blocked_by_package_install"
} else if (!ukgeog_has_parish) {
  "blocked_by_geography"
} else {
  "candidate"
}

if (!ukgeog_has_parish) {
  status <- "blocked_by_geography"
}

feasibility <- data.table(
  check_name = c(
    "ukgeog_installed",
    "ukgeog_metadata_available",
    "ukgeog_has_parish_boundary",
    "nomis_has_parish_like_population_units",
    "recommended_status",
    "recommended_next_step"
  ),
  status = c(
    as.character(ukgeog_installed),
    as.character(nrow(ukgeog_metadata) > 0L),
    as.character(ukgeog_has_parish),
    as.character(isTRUE(nomis_has_parish_population)),
    status,
    "use_direct_parish_boundary_source"
  ),
  detail = c(
    if (ukgeog_installed) {
      "ukgeog is installed in this R library."
    } else {
      "ukgeog is not installed; remotes::install_github('l-hodge/ukgeog') failed locally because the package imports rgdal, which is unavailable in this R setup."
    },
    if (nrow(ukgeog_metadata) > 0L) {
      paste(sort(unique(ukgeog_metadata$geog_short)), collapse = ", ")
    } else {
      "No ukgeog metadata found. Set UKGEOG_SOURCE_DIR to a local clone to audit the package metadata without installing it."
    },
    if (ukgeog_has_parish) {
      "ukgeog metadata contains a parish boundary level."
    } else {
      "ukgeog metadata contains NAT, LAD, GOR, UTLA, OA, LSOA, MSOA, electoral geographies, and NUTS, but no parish boundary level."
    },
    paste(
      "Nomis metadata include parish-like units in years:",
      paste(nomis_area_counts[
        lower_unit_class == "parish_like" & area_count > 0,
        sort(unique(census_year))
      ], collapse = ", ")
    ),
    if (status == "candidate") {
      "A parish panel can be attempted with ukgeog."
    } else {
      "A parish panel cannot be built from ukgeog alone because the needed parish polygons are not exposed by the package metadata."
    },
    "Keep the current LAU/district-area-weighted panel, or add an external parish boundary source and join it to Nomis parish/ward/enumeration district population units."
  )
)

fwrite(feasibility, feasibility_file)

cat("Wrote feasibility report:\n  ", feasibility_file, "\n", sep = "")
cat("Wrote Nomis lower-unit counts:\n  ", nomis_lower_units_file, "\n", sep = "")
if (file.exists(ukgeog_levels_file)) {
  cat("Wrote ukgeog boundary levels:\n  ", ukgeog_levels_file, "\n", sep = "")
}
cat("Recommended status: ", status, "\n", sep = "")
