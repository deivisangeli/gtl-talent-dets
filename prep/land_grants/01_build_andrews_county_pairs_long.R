###############################################################################
# Project: GTL Talent Determinants
# Goal: Build Andrews college site-selection county-pairs workbook
#
# Input:
#   raw/andrews_2023_appendix_table_a1.xlsx
#
# Output:
#   raw/andrews_2023_county_pairs_long.xlsx
#
# The output path intentionally keeps the historical "county_pairs" filename
# because AMWS and land_grants analysis scripts already consume it.
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("dplyr")
  library("readr")
  library("readxl")
  library("stringr")
  library("tidyr")
  library("writexl")
})

###############################################################################
# Paths
###############################################################################

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repo_root <- if (basename(cwd) == "prep") {
    dirname(cwd)
  } else if (basename(dirname(cwd)) == "prep") {
    dirname(dirname(cwd))
  } else {
    cwd
  }
}

source(file.path(repo_root, "prep", "raw_paths.R"))

source_workbook <- raw_file_path("andrews_2023_appendix_table_a1.xlsx")
source_sheet <- "table_a1"
output_workbook <- raw_file_path("andrews_2023_county_pairs_long.xlsx")

gazetteer_url <- paste0(
  "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/",
  "2025_Gazetteer/2025_Gaz_counties_national.zip"
)
gazetteer_zip <- raw_file_path("2025_Gaz_counties_national.zip")
gazetteer_file <- raw_file_path("2025_Gaz_counties_national.txt")

###############################################################################
# Helpers
###############################################################################

normalize_county <- function(x) {
  x %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    tolower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("\\bexperiment station\\b", " ") %>%
    str_replace_all("\\bexperiment\\b", " ") %>%
    str_replace_all(
      "\\bcounty\\b|\\bparish\\b|\\bborough\\b|\\bcensus area\\b|\\bmunicipality\\b|\\bcity and borough\\b|\\bcity\\b",
      " "
    ) %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

ensure_county_gazetteer <- function() {
  if (file.exists(gazetteer_file)) {
    return(invisible(gazetteer_file))
  }

  message("Downloading Census county gazetteer: ", gazetteer_url)
  download.file(gazetteer_url, gazetteer_zip, mode = "wb", quiet = FALSE)
  utils::unzip(gazetteer_zip, files = basename(gazetteer_file), exdir = dirname(gazetteer_file), overwrite = TRUE)

  if (!file.exists(gazetteer_file)) {
    stop("County gazetteer download/unzip failed: ", gazetteer_file)
  }

  invisible(gazetteer_file)
}

state_lookup <- tibble(
  state = c(state.name, "District of Columbia"),
  state_abbr = c(state.abb, "DC")
)

###############################################################################
# Load source and gazetteer
###############################################################################

if (!file.exists(source_workbook)) {
  stop("Missing Andrews appendix workbook: ", source_workbook)
}

ensure_county_gazetteer()

appendix <- readxl::read_excel(source_workbook, sheet = source_sheet) %>%
  mutate(
    row_id = row_number(),
    across(c(college, county, state, runner_up_counties, college_type), as.character),
    experiment_year = as.integer(experiment_year)
  )

gazetteer <- readr::read_delim(gazetteer_file, delim = "|", show_col_types = FALSE, trim_ws = TRUE) %>%
  transmute(
    state_abbr = USPS,
    county_norm = normalize_county(NAME),
    lat = as.numeric(INTPTLAT),
    lon = as.numeric(INTPTLONG)
  )

###############################################################################
# Build long county-pairs table
###############################################################################

selected_coords <- appendix %>%
  distinct(row_id, selected_county = county, selected_state = state) %>%
  left_join(state_lookup, by = c("selected_state" = "state")) %>%
  mutate(selected_county_norm = normalize_county(selected_county)) %>%
  left_join(
    gazetteer,
    by = c("state_abbr", "selected_county_norm" = "county_norm")
  ) %>%
  transmute(
    row_id,
    selected_lat = lat,
    selected_lon = lon
  )

unmatched_selected <- selected_coords %>%
  filter(is.na(selected_lat) | is.na(selected_lon))

if (nrow(unmatched_selected) > 0) {
  stop("Some selected counties could not be matched to the Census county gazetteer.")
}

county_pairs_long <- appendix %>%
  mutate(runner_up_county = str_split(runner_up_counties, "\\s*,\\s*")) %>%
  unnest_longer(runner_up_county, indices_to = "runner_up_order") %>%
  mutate(
    runner_up_order = as.integer(runner_up_order),
    runner_up_county = str_squish(as.character(runner_up_county))
  ) %>%
  left_join(selected_coords, by = "row_id") %>%
  left_join(state_lookup, by = c("state" = "state")) %>%
  mutate(
    runner_up_state_assumed = state,
    runner_up_county_norm = normalize_county(runner_up_county)
  ) %>%
  left_join(
    gazetteer,
    by = c("state_abbr", "runner_up_county_norm" = "county_norm")
  ) %>%
  transmute(
    college,
    experiment_year,
    college_type,
    selected_county = county,
    selected_state = state,
    selected_lat,
    selected_lon,
    runner_up_order,
    runner_up_county,
    runner_up_state_assumed,
    runner_up_lat = lat,
    runner_up_lon = lon,
    runner_up_match_status = if_else(
      is.na(runner_up_lat) | is.na(runner_up_lon),
      "unmatched_not_in_state",
      "matched_same_state"
    )
  ) %>%
  arrange(experiment_year, college, runner_up_order)

source_sheet_tbl <- tibble(
  field = c(
    "source_workbook",
    "source_sheet",
    "output_workbook",
    "gazetteer_url",
    "gazetteer_file",
    "county_coordinate_definition",
    "runner_up_matching_rule",
    "extracted_on",
    "pair_rows",
    "selected_rows"
  ),
  value = c(
    source_workbook,
    source_sheet,
    output_workbook,
    gazetteer_url,
    basename(gazetteer_file),
    "Census representative point coordinates (INTPTLAT, INTPTLONG)",
    "Match runner-up county only within the row state; leave coordinates blank when not matched with high confidence",
    as.character(Sys.Date()),
    as.character(nrow(county_pairs_long)),
    as.character(nrow(appendix))
  )
)

###############################################################################
# Validate and export
###############################################################################

status_counts <- county_pairs_long %>%
  count(runner_up_match_status, name = "n")

if (nrow(appendix) != 63L) {
  warning("Expected 63 selected rows; found ", nrow(appendix), ".")
}

if (nrow(county_pairs_long) != 128L) {
  warning("Expected 128 county-pair rows; found ", nrow(county_pairs_long), ".")
}

writexl::write_xlsx(
  list(
    county_pairs_long = county_pairs_long,
    source = source_sheet_tbl
  ),
  output_workbook
)

message("Wrote: ", output_workbook)
message("Selected rows: ", nrow(appendix))
message("Pair rows: ", nrow(county_pairs_long))
message(
  "Runner-up match status: ",
  paste(status_counts$runner_up_match_status, status_counts$n, sep = "=", collapse = "; ")
)
