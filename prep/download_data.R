###############################################################################
# Project: Determinants of Talent Production
# Goal: Download raw input data if not already present
# Run this script before Cleaning_Data.R or Coding_HYDE.R
###############################################################################

options(timeout = 600)

input_dir <- "input"
dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)

###############################################################################
# Cross-Verified Database of Notable People (Laouan et al., SciencesPo)
# Source: https://data.sciencespo.fr/dataset.xhtml?persistentId=doi:10.7910/DVN/EEA236
###############################################################################
cv_path <- file.path(input_dir, "cross-verified-database.csv")

if (!file.exists(cv_path)) {
  message("Downloading cross-verified database...")
  url <- "https://data.sciencespo.fr/api/access/datafile/4432?format=original"
  temp <- tempfile(fileext = ".gz")
  download.file(url, temp, mode = "wb")
  message("Decompressing...")
  R.utils::gunzip(temp, destname = cv_path, remove = TRUE, overwrite = TRUE)
  message("Saved to: ", cv_path)
} else {
  message("cross-verified-database.csv already present, skipping download.")
}
