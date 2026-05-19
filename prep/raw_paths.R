# ---------------------------------------------------------------------------
# Project data paths
# ---------------------------------------------------------------------------

det_dir <- trimws(Sys.getenv("DET_DIR", unset = ""))
if (!nzchar(det_dir)) {
 stop("DET_DIR is not set. Set it in .Renviron to the Dropbox project folder.")
}

det_dir <- normalizePath(det_dir, winslash = "/", mustWork = TRUE)

require_det_dir <- function() {
 det_dir
}

ensure_dir <- function(path) {
 dir.create(path, recursive = TRUE, showWarnings = FALSE)
 path
}

data_path <- function(folder, ..., create_parent = FALSE) {
 path <- file.path(det_dir, folder, ...)
 if (isTRUE(create_parent)) {
  ensure_dir(dirname(path))
 }
 path
}

# ---------------------------------------------------------------------------
# Raw data paths
# ---------------------------------------------------------------------------

raw_dir <- function(...) {
 ensure_dir(file.path(det_dir, "raw", ...))
}

raw_file_path <- function(...) {
 data_path("raw", ..., create_parent = TRUE)
}

# ---------------------------------------------------------------------------
# Input helpers
# ---------------------------------------------------------------------------

manual_input_path <- function(...) {
 data_path("input", ...)
}

hyde_input_dir <- function() {
 data_path("input", "hyde_pop_asc")
}

geodata_cache_dir <- function() {
 raw_dir("geodata")
}

tigris_cache_dir <- function() {
 raw_dir("tigris")
}

# ---------------------------------------------------------------------------
# Download URLs & helpers
# ---------------------------------------------------------------------------

cross_verified_url <- "https://data.sciencespo.fr/api/access/datafile/4432?format=original"
new_births_url <- paste0(
 "https://raw.githubusercontent.com/open-numbers/",
 "ddf--gapminder--systema_globalis/master/",
 "countries-etc-datapoints/",
 "ddf--datapoints--new_births_total_number_estimated--by--geo--time.csv"
)

cross_verified_csv_path <- function() {
 raw_file_path("cross-verified-database.csv")
}

new_births_csv_path <- function() {
 raw_file_path("new_births_total_number_estimated.csv")
}

ensure_downloaded_file <- function(url, dest, mode = "wb") {
 if (!file.exists(dest)) {
  message("Downloading ", basename(dest), "...")
  download.file(url, dest, mode = mode)
  message("Saved to: ", dest)
 } else {
  message(basename(dest), " already present, skipping download.")
 }
 dest
}

ensure_wikipedia_csv <- function() {
 dest <- cross_verified_csv_path()
 if (!file.exists(dest)) {
  message("Downloading cross-verified database...")
  temp <- tempfile(fileext = ".gz")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  download.file(cross_verified_url, temp, mode = "wb")
  message("Decompressing...")
  R.utils::gunzip(temp, destname = dest, remove = FALSE, overwrite = TRUE)
  message("Saved to: ", dest)
 } else {
  message("cross-verified-database.csv already present, skipping download.")
 }
 dest
}

ensure_new_births_csv <- function() {
 ensure_downloaded_file(new_births_url, new_births_csv_path())
}

# ---------------------------------------------------------------------------
# Output helpers
# ---------------------------------------------------------------------------

output_file_path <- function(...) {
 data_path("output", ..., create_parent = TRUE)
}

results_file_path <- function(...) {
 data_path("results", ..., create_parent = TRUE)
}

save_csv <- function(x, ..., row.names = FALSE) {
 path <- output_file_path(...)
 write.csv(x, path, row.names = row.names)
 invisible(path)
}

save_plot <- function(..., plot, width, height, dpi = 300) {
 path <- output_file_path(...)
 ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi)
 invisible(path)
}
