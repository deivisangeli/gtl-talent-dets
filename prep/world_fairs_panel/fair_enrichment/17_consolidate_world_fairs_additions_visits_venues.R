###############################################################################
# Consolidate researched visits and venues for post-1911 world's fair additions.
#
# Expected inputs:
#   input/worlds_fairs/worlds_fairs_additions_1911_1960_agent_batch{1..4}.csv
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/17_consolidate_world_fairs_additions_visits_venues.R
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

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

worlds_fairs_dir <- file.path(DATA_INPUT, "worlds_fairs")
additions_path <- file.path(
  worlds_fairs_dir,
  "worlds_fairs_additions_1911_1960_from_scrape.csv"
)

batch_paths <- file.path(
  worlds_fairs_dir,
  sprintf("worlds_fairs_additions_1911_1960_agent_batch%d.csv", 1:4)
)

missing_batches <- batch_paths[!file.exists(batch_paths)]
if (length(missing_batches) > 0) {
  stop("Missing researched batch files:\n", paste(missing_batches, collapse = "\n"))
}

required_cols <- c(
  "row_id", "Fair_name", "City", "Year",
  "venue", "venue_source_title", "venue_source_url", "venue_note",
  "venue_latitude", "venue_longitude",
  "venue_coordinates_source_title", "venue_coordinates_source_url",
  "venue_coordinates_note", "visits", "visits_measure", "source_tier",
  "confidence", "source_title", "source_url", "source_note", "search_status"
)

read_batch <- function(path) {
  x <- read_csv(
    path,
    show_col_types = FALSE,
    na = c("", "NA", "N/A", "na"),
    col_types = cols(.default = col_character())
  )
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop("Missing columns in ", basename(path), ": ", paste(missing_cols, collapse = ", "))
  }
  x %>%
    select(all_of(required_cols)) %>%
    mutate(batch_file = basename(path))
}

researched <- bind_rows(lapply(batch_paths, read_batch)) %>%
  mutate(
    row_id = as.integer(row_id),
    Year = as.integer(Year),
    visits = suppressWarnings(as.integer(visits)),
    venue_latitude = suppressWarnings(as.numeric(venue_latitude)),
    venue_longitude = suppressWarnings(as.numeric(venue_longitude)),
    source_tier = str_replace(source_tier, "^tier(\\d+)$", "tier_\\1"),
    across(where(is.character), ~na_if(str_squish(.x), ""))
  )

additions <- read_csv(additions_path, show_col_types = FALSE, na = c("", "NA", "N/A", "na")) %>%
  mutate(
    row_id = as.integer(scrape_row_id),
    Year_research = if ("year_start" %in% names(.)) {
      coalesce(
        as.integer(year_start),
        as.integer(str_extract(as.character(Year), "^\\d{4}"))
      )
    } else {
      as.integer(str_extract(as.character(Year), "^\\d{4}"))
    }
  )

expected_ids <- additions$row_id
found_ids <- researched$row_id

duplicate_ids <- found_ids[duplicated(found_ids)]
missing_ids <- setdiff(expected_ids, found_ids)
extra_ids <- setdiff(found_ids, expected_ids)

allowed_status <- c("found", "conflicting_sources", "ambiguous_match", "not_found")
allowed_confidence <- c("high", "medium", "low")

invalid_status <- researched %>%
  filter(!search_status %in% allowed_status | is.na(search_status)) %>%
  distinct(row_id, search_status)

invalid_confidence <- researched %>%
  filter(!confidence %in% allowed_confidence | is.na(confidence)) %>%
  distinct(row_id, confidence)

missing_doc <- researched %>%
  filter(
    (!is.na(venue) & (is.na(venue_source_title) | is.na(venue_source_url))) |
      ((!is.na(venue_latitude) | !is.na(venue_longitude)) &
         (is.na(venue_coordinates_source_title) | is.na(venue_coordinates_source_url))) |
      (!is.na(visits) & (is.na(source_title) | is.na(source_url)))
  ) %>%
  distinct(row_id)

if (length(duplicate_ids) > 0) {
  stop("Duplicate row_id values in researched batches: ", paste(unique(duplicate_ids), collapse = ", "))
}
if (length(missing_ids) > 0) {
  stop("Missing row_id values in researched batches: ", paste(missing_ids, collapse = ", "))
}
if (length(extra_ids) > 0) {
  stop("Unexpected row_id values in researched batches: ", paste(extra_ids, collapse = ", "))
}
if (nrow(invalid_status) > 0) {
  stop("Invalid search_status values:\n", paste(capture.output(print(invalid_status)), collapse = "\n"))
}
if (nrow(invalid_confidence) > 0) {
  stop("Invalid confidence values:\n", paste(capture.output(print(invalid_confidence)), collapse = "\n"))
}
if (nrow(missing_doc) > 0) {
  warning(
    "Rows with filled venue/coordinates/visits but incomplete source documentation: ",
    paste(missing_doc$row_id, collapse = ", ")
  )
}

original_research_overlap <- intersect(
  c(
    "visits", "visits_measure", "source_tier", "confidence", "source_title",
    "source_url", "source_note", "venue", "venue_latitude", "venue_longitude"
  ),
  names(additions)
)

final_panel <- additions %>%
  select(-all_of(original_research_overlap)) %>%
  left_join(
    researched %>% select(-batch_file, -Fair_name, -City, -Year),
    by = "row_id",
    suffix = c("", "_research")
  ) %>%
  relocate(
    row_id, .after = scrape_row_id
  )

out_csv <- file.path(
  worlds_fairs_dir,
  "worlds_fairs_additions_1911_1960_with_visits_venues.csv"
)
out_xlsx <- file.path(
  worlds_fairs_dir,
  "worlds_fairs_additions_1911_1960_with_visits_venues.xlsx"
)
out_summary <- file.path(
  worlds_fairs_dir,
  "worlds_fairs_additions_1911_1960_with_visits_venues_summary.txt"
)

write_csv(final_panel, out_csv, na = "")

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(final_panel, out_xlsx)
} else {
  warning("Package 'writexl' is not installed; skipped XLSX export.")
}

summary_lines <- c(
  paste0("Input rows: ", nrow(additions)),
  paste0("Researched rows: ", nrow(researched)),
  paste0("Rows with venue: ", sum(!is.na(researched$venue))),
  paste0("Rows with venue coordinates: ", sum(!is.na(researched$venue_latitude) & !is.na(researched$venue_longitude))),
  paste0("Rows with visits: ", sum(!is.na(researched$visits))),
  "",
  "Search status counts:",
  capture.output(print(table(researched$search_status, useNA = "ifany"))),
  "",
  "Confidence counts:",
  capture.output(print(table(researched$confidence, useNA = "ifany"))),
  "",
  paste0("CSV: ", out_csv),
  paste0("XLSX: ", if (file.exists(out_xlsx)) out_xlsx else "not written"),
  paste0("Summary: ", out_summary)
)

writeLines(summary_lines, out_summary)
message(paste(summary_lines, collapse = "\n"))
