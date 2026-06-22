###############################################################################
# Build a consolidated world's fairs file with visits and venues, 1790-1960.
#
# Inputs:
#   input/worlds_fairs/worlds_fairs_visits_1790_1910_with_venues_exhaustive.csv
#   input/worlds_fairs/worlds_fairs_additions_1911_1960_with_visits_venues.csv
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/18_build_worlds_fairs_1790_1960_with_visits_venues.R
###############################################################################

args_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/"), error = function(e) NA_character_)
if (!is.na(args_file)) {
  script_path <- args_file
} else {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  script_path <- if (length(file_arg) == 1L) {
    normalizePath(sub("^--file=", "", file_arg), winslash = "/")
  } else {
    NA_character_
  }
}

if (!is.na(script_path)) {
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", "..", ".."),
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
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

fairs_dir <- file.path(DATA_INPUT, "worlds_fairs")

old_path <- file.path(
  fairs_dir,
  "worlds_fairs_visits_1790_1910_with_venues_exhaustive.csv"
)
new_path <- file.path(
  fairs_dir,
  "worlds_fairs_additions_1911_1960_with_visits_venues.csv"
)

out_csv <- file.path(
  fairs_dir,
  "worlds_fairs_1790_1960_with_visits_venues.csv"
)
out_xlsx <- file.path(
  fairs_dir,
  "worlds_fairs_1790_1960_with_visits_venues.xlsx"
)
out_summary <- file.path(
  fairs_dir,
  "worlds_fairs_1790_1960_with_visits_venues_summary.txt"
)

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("Missing input file: ", path)
  }
}

stop_if_missing(old_path)
stop_if_missing(new_path)

parse_first_year <- function(x) {
  suppressWarnings(as.integer(str_extract(as.character(x), "[0-9]{4}")))
}

as_integer_checked <- function(x, field_name) {
  x_chr <- na_if(str_squish(as.character(x)), "")
  out <- suppressWarnings(as.integer(x_chr))
  invalid <- !is.na(x_chr) & is.na(out)
  if (any(invalid)) {
    stop(
      "Non-integer values in ", field_name, ": ",
      paste(unique(x_chr[invalid]), collapse = ", ")
    )
  }
  out
}

as_numeric_checked <- function(x, field_name) {
  x_chr <- na_if(str_squish(as.character(x)), "")
  out <- suppressWarnings(as.numeric(x_chr))
  invalid <- !is.na(x_chr) & is.na(out)
  if (any(invalid)) {
    stop(
      "Non-numeric values in ", field_name, ": ",
      paste(unique(x_chr[invalid]), collapse = ", ")
    )
  }
  out
}

ensure_columns <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  for (col in missing_cols) {
    data[[col]] <- NA_character_
  }
  data
}

final_cols <- c(
  "fair_id", "source_period", "source_row_id", "scrape_row_id",
  "Year", "year_start", "City", "Country", "Fair_name", "Fair_observation",
  "visits", "visits_measure", "source_tier", "confidence", "source_title",
  "source_url", "source_note", "search_status", "source_status",
  "venue", "venue_source_title", "venue_source_url", "venue_note",
  "venue_latitude", "venue_longitude",
  "venue_coordinates_source_title", "venue_coordinates_source_url",
  "venue_coordinates_note", "venue_search_status"
)

old_raw <- read_csv(
  old_path,
  show_col_types = FALSE,
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A", "na")
)
new_raw <- read_csv(
  new_path,
  show_col_types = FALSE,
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A", "na")
)

old <- old_raw %>%
  mutate(
    source_period = "1790_1910",
    source_row_id = row_id,
    scrape_row_id = NA_character_,
    year_start = parse_first_year(Year),
    Country = NA_character_,
    Fair_observation = NA_character_
  ) %>%
  ensure_columns(final_cols) %>%
  select(all_of(final_cols))

new <- new_raw %>%
  mutate(
    source_period = "1911_1960",
    source_row_id = row_id,
    year_start = coalesce(
      suppressWarnings(as.integer(year_start)),
      parse_first_year(Year)
    ),
    source_status = NA_character_,
    venue_search_status = NA_character_
  ) %>%
  ensure_columns(final_cols) %>%
  select(all_of(final_cols))

validate_inputs <- function(old, new) {
  if (nrow(old) != 324L) {
    stop("Expected 324 rows in 1790-1910 input; found ", nrow(old))
  }
  if (nrow(new) != 145L) {
    stop("Expected 145 rows in 1911-1960 input; found ", nrow(new))
  }
  if (any(is.na(old$year_start))) {
    stop("Missing parsed year_start values in 1790-1910 input.")
  }
  if (any(is.na(new$year_start))) {
    stop("Missing parsed year_start values in 1911-1960 input.")
  }
  if (any(old$year_start < 1790L | old$year_start > 1910L)) {
    stop("1790-1910 input has years outside the expected window.")
  }
  if (any(new$year_start < 1911L | new$year_start > 1960L)) {
    stop("1911-1960 input has years outside the expected window.")
  }
}

validate_inputs(old, new)

combined <- bind_rows(old, new) %>%
  mutate(
    visits = as_integer_checked(visits, "visits"),
    venue_latitude = as_numeric_checked(venue_latitude, "venue_latitude"),
    venue_longitude = as_numeric_checked(venue_longitude, "venue_longitude"),
    year_start = as.integer(year_start),
    source_tier = str_replace(source_tier, "^tier(\\d+)$", "tier_\\1"),
    across(where(is.character), ~na_if(str_squish(.x), ""))
  ) %>%
  arrange(year_start, Country, City, Fair_name, source_period, source_row_id) %>%
  mutate(fair_id = row_number()) %>%
  select(all_of(final_cols))

if (nrow(combined) != 469L) {
  stop("Expected 469 combined rows; found ", nrow(combined))
}
if (anyDuplicated(combined$fair_id) > 0) {
  stop("fair_id is not unique.")
}
if (any(is.na(combined$year_start))) {
  stop("Combined file contains missing year_start.")
}

write_csv(combined, out_csv, na = "")

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(combined, out_xlsx)
} else {
  warning("Package 'writexl' is not installed; skipped XLSX export.")
}

summary_lines <- c(
  paste0("Old input: ", old_path),
  paste0("New input: ", new_path),
  paste0("Output CSV: ", out_csv),
  paste0("Output XLSX: ", if (file.exists(out_xlsx)) out_xlsx else "not written"),
  paste0("Rows: ", nrow(combined)),
  paste0("Columns: ", ncol(combined)),
  paste0("Year range: ", min(combined$year_start), "-", max(combined$year_start)),
  "",
  "Rows by source_period:",
  capture.output(print(table(combined$source_period, useNA = "ifany"))),
  "",
  paste0("Rows with visits: ", sum(!is.na(combined$visits))),
  paste0("Rows with venue: ", sum(!is.na(combined$venue))),
  paste0(
    "Rows with venue coordinates: ",
    sum(!is.na(combined$venue_latitude) & !is.na(combined$venue_longitude))
  ),
  "",
  "search_status counts:",
  capture.output(print(table(combined$search_status, useNA = "ifany"))),
  "",
  "venue_search_status counts:",
  capture.output(print(table(combined$venue_search_status, useNA = "ifany"))),
  "",
  "confidence counts:",
  capture.output(print(table(combined$confidence, useNA = "ifany")))
)

writeLines(summary_lines, out_summary)
message(paste(summary_lines, collapse = "\n"))
