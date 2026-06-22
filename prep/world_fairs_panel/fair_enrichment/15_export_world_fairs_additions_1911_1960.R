###############################################################################
# Export world's fairs additions from the scraped Wikipedia list, 1911-1960.
#
# This script creates a clean queue of realized fairs to add after the existing
# curated 1790-1910 world's fairs files.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/15_export_world_fairs_additions_1911_1960.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
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

fairs_dir <- file.path(DATA_INPUT, "worlds_fairs")
dir.create(fairs_dir, recursive = TRUE, showWarnings = FALSE)

scrape_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia.xlsx")
out_file <- file.path(
  fairs_dir, "worlds_fairs_additions_1911_1960_from_scrape.csv"
)
excluded_file <- file.path(
  fairs_dir, "worlds_fairs_additions_1911_1960_from_scrape_excluded.csv"
)

if (!file.exists(scrape_file)) {
  stop("Missing scraped world's fairs file: ", scrape_file)
}

###############################################################################
# Build additions file
###############################################################################

parse_first_year <- function(year_text) {
  suppressWarnings(as.integer(str_extract(as.character(year_text), "[0-9]{4}")))
}

non_realized_reason <- function(text) {
  case_when(
    str_detect(text, "cancelled|canceled") ~ "cancelled",
    str_detect(text, "never held") ~ "never_held",
    str_detect(text, "not held") ~ "not_held",
    str_detect(text, "postponed") ~ "postponed",
    str_detect(text, "planned") ~ "planned_only",
    str_detect(text, "^intended\\b|\\bintended\\b") ~ "intended_only",
    TRUE ~ NA_character_
  )
}

scrape <- read_xlsx(scrape_file) %>%
  mutate(
    scrape_row_id = row_number(),
    year_start = parse_first_year(Year),
    search_text = str_to_lower(str_squish(paste(
      coalesce(Year, ""),
      coalesce(City, ""),
      coalesce(Country, ""),
      coalesce(Fair_name, ""),
      coalesce(Fair_observation, "")
    ))),
    exclusion_reason = non_realized_reason(search_text),
    excluded_non_realized = !is.na(exclusion_reason)
  )

candidate_window <- scrape %>%
  filter(!is.na(year_start), year_start >= 1911L, year_start <= 1960L)

additions <- candidate_window %>%
  filter(!excluded_non_realized) %>%
  transmute(
    scrape_row_id,
    Year,
    year_start,
    City,
    Country,
    Fair_name,
    Fair_observation,
    visits = NA_real_,
    visits_measure = NA_character_,
    source_tier = NA_character_,
    confidence = NA_character_,
    source_title = NA_character_,
    source_url = NA_character_,
    source_note = NA_character_,
    venue = NA_character_,
    venue_latitude = NA_real_,
    venue_longitude = NA_real_
  ) %>%
  arrange(year_start, Country, City, Fair_name)

excluded <- candidate_window %>%
  filter(excluded_non_realized) %>%
  transmute(
    scrape_row_id,
    Year,
    year_start,
    City,
    Country,
    Fair_name,
    Fair_observation,
    exclusion_reason
  ) %>%
  arrange(year_start, Country, City, Fair_name)

###############################################################################
# Validation and writes
###############################################################################

if (nrow(candidate_window) != 152L) {
  stop("Expected 152 scraped candidate rows in 1911-1960; found ", nrow(candidate_window))
}
if (nrow(excluded) != 7L) {
  stop("Expected 7 non-realized rows to exclude; found ", nrow(excluded))
}
if (nrow(additions) != 145L) {
  stop("Expected 145 additions after exclusions; found ", nrow(additions))
}

forbidden_pattern <- regex(
  "cancelled|canceled|never held|not held|postponed|planned|\\bintended\\b",
  ignore_case = TRUE
)
additions_text <- paste(
  additions$Year,
  additions$City,
  additions$Country,
  additions$Fair_name,
  additions$Fair_observation
)
if (any(str_detect(additions_text, forbidden_pattern), na.rm = TRUE)) {
  stop("Non-realized wording remains in additions output.")
}

write_csv(additions, out_file)
write_csv(excluded, excluded_file)

cat("\nCompleted world's fairs additions export.\n")
cat("Scraped source: ", scrape_file, "\n", sep = "")
cat("Additions CSV: ", out_file, "\n", sep = "")
cat("Excluded audit CSV: ", excluded_file, "\n", sep = "")
cat("Candidate rows 1911-1960: ", nrow(candidate_window), "\n", sep = "")
cat("Additions rows: ", nrow(additions), "\n", sep = "")
cat("Excluded non-realized rows: ", nrow(excluded), "\n", sep = "")
cat("Additions year range: ", min(additions$year_start), "-", max(additions$year_start), "\n", sep = "")
cat("\nAdditions by decade:\n")
print(additions %>% count(decade = floor(year_start / 10) * 10, name = "n"))
