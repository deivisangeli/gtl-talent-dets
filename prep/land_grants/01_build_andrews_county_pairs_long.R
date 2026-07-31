###############################################################################
# Project: GTL Talent Determinants
# Goal: Build Andrews college site-selection county-pairs workbook
#
# Inputs:
#   raw/andrews_2023_appendix_table_a1.xlsx        (event list = published Table A1)
#   Data/raw/land_grants/College_Control_Towns.csv (Andrews 2023 replication package)
#
# Output:
#   raw/andrews_2023_county_pairs_long.xlsx
#
# Design (hybrid):
#   The set of experiments (winners) and the runner-up membership come from the
#   published Appendix Table A1 (63 high-quality experiments; each county appears
#   as a college OR a runner-up exactly once -- Andrews 2023, footnote 12). We do
#   NOT rebuild the sample from the replication file, which is a broader working
#   set (would yield 65+ in-window experiments and reintroduce counties Andrews
#   deliberately de-duplicated).
#
#   Geocoding is taken from the replication file's real town-level State/Latitude/
#   Longitude, replacing the previous "assume runner-up is in the college's state"
#   rule. This resolves genuinely cross-state runner-ups (e.g. Lincoln College's
#   Warrick County, Indiana) instead of dropping them.
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
replication_file <- file.path(require_det_dir(), "Data", "raw", "land_grants", "College_Control_Towns.csv")
output_workbook <- raw_file_path("andrews_2023_county_pairs_long.xlsx")

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

###############################################################################
# Load Table A1 (event list) and Andrews replication file (geocoding)
###############################################################################

if (!file.exists(source_workbook)) {
  stop("Missing Andrews appendix workbook: ", source_workbook)
}
if (!file.exists(replication_file)) {
  stop("Missing Andrews replication file: ", replication_file)
}

appendix <- readxl::read_excel(source_workbook, sheet = source_sheet) %>%
  mutate(
    row_id = row_number(),
    across(c(college, county, state, runner_up_counties, college_type), as.character),
    experiment_year = as.integer(experiment_year),
    selected_county_norm = normalize_county(county)
  )

replication <- readr::read_csv(replication_file, show_col_types = FALSE) %>%
  transmute(
    experiment = as.character(Experiment),
    county = as.character(County),
    state = as.character(State),
    county_norm = normalize_county(County),
    treatment = suppressWarnings(as.integer(Treatment)),
    year = suppressWarnings(as.integer(Year_Experiment)),
    lat = suppressWarnings(as.numeric(Latitude)),
    lon = suppressWarnings(as.numeric(Longitude))
  )

###############################################################################
# Match each Table A1 experiment (winner) to a replication experiment id
# Key = normalized county + state + experiment year (year disambiguates the
# many counties that host more than one experiment, e.g. El Paso/CO hosts both
# Colorado College 1874 and the US Air Force Academy 1954).
###############################################################################

winners <- replication %>%
  filter(treatment == 1L) %>%
  transmute(
    experiment,
    county_norm,
    state,
    experiment_year = year,
    selected_lat = lat,
    selected_lon = lon
  )

appendix_matched <- appendix %>%
  left_join(
    winners,
    by = c("selected_county_norm" = "county_norm", "state", "experiment_year")
  )

ambiguous_winner <- appendix_matched %>% count(row_id, name = "n") %>% filter(n > 1L)
if (nrow(ambiguous_winner) > 0) {
  stop("Ambiguous winner match (>1 replication experiment) for row_id: ",
       paste(ambiguous_winner$row_id, collapse = ", "))
}

unmatched_selected <- appendix_matched %>% filter(is.na(experiment))
if (nrow(unmatched_selected) > 0) {
  stop("Selected experiments not found in replication file: ",
       paste(sprintf("%s (%s, %s, %d)",
                     unmatched_selected$college, unmatched_selected$county,
                     unmatched_selected$state, unmatched_selected$experiment_year),
             collapse = "; "))
}

###############################################################################
# Runner-up geocoding: for each experiment, collapse loser towns to one row per
# county, taking the real state and the mean town coordinate.
###############################################################################

losers <- replication %>%
  filter(treatment == 0L) %>%
  group_by(experiment, county_norm) %>%
  summarise(
    runner_up_state = dplyr::first(state),
    runner_up_lat = mean(lat, na.rm = TRUE),
    runner_up_lon = mean(lon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    runner_up_lat = ifelse(is.nan(runner_up_lat), NA_real_, runner_up_lat),
    runner_up_lon = ifelse(is.nan(runner_up_lon), NA_real_, runner_up_lon)
  )

county_pairs_long <- appendix_matched %>%
  mutate(runner_up_county = str_split(runner_up_counties, "\\s*,\\s*")) %>%
  unnest_longer(runner_up_county, indices_to = "runner_up_order") %>%
  mutate(
    runner_up_order = as.integer(runner_up_order),
    runner_up_county = str_squish(as.character(runner_up_county)),
    runner_up_county_norm = normalize_county(runner_up_county)
  ) %>%
  left_join(
    losers,
    by = c("experiment", "runner_up_county_norm" = "county_norm")
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
    # Column name kept for backward compatibility; now holds the REAL state
    # from the Andrews replication file (no longer a same-state assumption).
    runner_up_state_assumed = dplyr::coalesce(runner_up_state, state),
    runner_up_lat,
    runner_up_lon,
    runner_up_match_status = dplyr::case_when(
      is.na(runner_up_state) ~ "unmatched",
      runner_up_state == state ~ "matched_same_state",
      TRUE ~ "matched_cross_state"
    )
  ) %>%
  arrange(experiment_year, college, runner_up_order)

###############################################################################
# Provenance sheet
###############################################################################

source_sheet_tbl <- tibble(
  field = c(
    "appendix_workbook",
    "appendix_sheet",
    "replication_file",
    "output_workbook",
    "coordinate_definition",
    "runner_up_matching_rule",
    "baseline_note",
    "extracted_on",
    "pair_rows",
    "selected_rows",
    "cross_state_runner_ups"
  ),
  value = c(
    source_workbook,
    source_sheet,
    replication_file,
    output_workbook,
    "Andrews replication town coordinates (Latitude/Longitude); county-level mean when a county hosts >1 town",
    "Event list anchored on published Table A1; runner-up real state and coordinates taken from Andrews replication College_Control_Towns.csv (no same-state assumption)",
    "Baseline = 63 published high-quality experiments; each county is a college OR a runner-up exactly once (Andrews 2023, footnote 12)",
    as.character(Sys.Date()),
    as.character(nrow(county_pairs_long)),
    as.character(nrow(appendix)),
    as.character(sum(county_pairs_long$runner_up_match_status == "matched_cross_state"))
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

n_unmatched <- sum(county_pairs_long$runner_up_match_status == "unmatched")
if (n_unmatched > 0L) {
  warning(n_unmatched, " runner-up(s) could not be geocoded from the replication file.")
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
