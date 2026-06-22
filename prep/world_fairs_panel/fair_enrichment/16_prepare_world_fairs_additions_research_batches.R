###############################################################################
# Prepare manual research batches for post-1911 world's fairs visits and venues.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/16_prepare_world_fairs_additions_research_batches.R
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

if (!file.exists(additions_path)) {
  stop("Missing additions file: ", additions_path)
}

additions <- read_csv(additions_path, show_col_types = FALSE) %>%
  mutate(
    row_id = as.integer(scrape_row_id),
    Year = if ("year_start" %in% names(.)) {
      coalesce(
        as.integer(year_start),
        as.integer(str_extract(as.character(Year), "^\\d{4}"))
      )
    } else {
      as.integer(str_extract(as.character(Year), "^\\d{4}"))
    },
    source_url = if ("source_url" %in% names(.)) source_url else NA_character_
  ) %>%
  arrange(Year, row_id)

required_input <- c("row_id", "Fair_name", "City", "Year", "source_url")
missing_input <- setdiff(required_input, names(additions))
if (length(missing_input) > 0) {
  stop("Missing required columns: ", paste(missing_input, collapse = ", "))
}

research_input <- additions %>%
  select(all_of(required_input)) %>%
  mutate(
    Fair_name = str_squish(Fair_name),
    Fair_name = if_else(
      str_detect(Fair_name, fixed("Damascus")),
      "Damascus World Fair",
      Fair_name
    ),
    City = str_squish(City),
    source_url = na_if(str_squish(source_url), "")
  )

n_batches <- 4L
research_input <- research_input %>%
  mutate(batch = ((row_number() - 1L) %% n_batches) + 1L)

for (batch_id in seq_len(n_batches)) {
  batch_rows <- research_input %>%
    filter(batch == batch_id) %>%
    select(-batch)

  out_path <- file.path(
    worlds_fairs_dir,
    sprintf("worlds_fairs_additions_1911_1960_input_batch%d.csv", batch_id)
  )
  write_csv(batch_rows, out_path, na = "")
}

message(
  "Wrote ", nrow(research_input), " rows across ", n_batches,
  " research input batches in ", worlds_fairs_dir
)
