###############################################################################
# Project: Determinants of Inventors
# Author: Lucas Mattos
# Goal: Downloading and cleaning the Wikipedia Database
###############################################################################

# Clean the working environment
rm(list = ls())

# Packages
library("tidyverse")
library("ggplot2")
library("tidygeocoder")
library("sf")
library("rnaturalearth")
library("readr")
library("data.table")
library("dataverse")
library("R.utils")
library("geodata")

# Recording Initial Time
initial_time <- Sys.time()

# Parameters
options(timeout = 600)

###############################################################################
# Importing the Database from downloads
###############################################################################
# Directories
zip_dir <- "input/2016_beta_release/zip"
asc_dir <- "input/hyde_pop_asc"

dir.create(asc_dir, recursive = TRUE, showWarnings = FALSE)

# Function to extract files
extract_hyde_pop_asc <- function(start_year = 1860,
                                 end_year   = 2000,
                                 zip_dir,
                                 asc_dir) {

 years <- seq(start_year, end_year, by = 10)

 for (y in years) {

  zip_name <- paste0(y, "AD_pop.zip")
  zip_path <- file.path(zip_dir, zip_name)

  if (!file.exists(zip_path)) {
   message("Skipping (not found): ", zip_name)
   next
  }

  message("Extracting from: ", zip_name)

  zip_list <- unzip(zip_path, list = TRUE)

  asc_files <- zip_list$Name[grepl("\\.asc$", zip_list$Name)]

  if (length(asc_files) == 0) {
   message("  No ASC files found.")
   next
  }

  unzip(
   zipfile = zip_path,
   files   = asc_files,
   exdir   = asc_dir
  )

  # Optional: rename to avoid weird nested names
  extracted <- file.path(asc_dir, basename(asc_files))

  if (length(extracted) == 1) {
   file.rename(
    from = file.path(asc_dir, asc_files),
    to   = file.path(asc_dir, paste0(y, "AD_pop.asc"))
   )
  }
 }
}

extract_hyde_pop_asc(
 start_year = 1800,
 end_year   = 2000,
 zip_dir    = zip_dir,
 asc_dir    = asc_dir
)
