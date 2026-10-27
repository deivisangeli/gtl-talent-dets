# Download a reproducible, separate NHGIS extract; never overwrite legacy extracts.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(script)) normalizePath(file.path(dirname(script), "../.."), winslash = "/") else Sys.getenv("GTL_REPO", getwd())
source(file.path(repo_root, "paths.R"))
suppressPackageStartupMessages(library(ipumsr))
root <- file.path(TALENT_DETS_DATA_DIR, "raw", "mandatory_schooling_matching")
dir.create(root, recursive = TRUE, showWarnings = FALSE)
key <- Sys.getenv("IPUMS_API_KEY")
if (!nzchar(key)) key <- Sys.getenv("IPUMS_KEY")
if (!nzchar(key)) stop("Set IPUMS_API_KEY or IPUMS_KEY to download NHGIS inputs.")
set_ipums_api_key(key, save = FALSE)
datasets <- c("1850_cPAX", "1860_cPAX", "1850_cAg", "1860_cAg")
metadata_path <- file.path(root, "metadata.rds")
if (!file.exists(metadata_path)) {
  metadata <- setNames(lapply(datasets, function(ds) {
    m <- get_metadata("nhgis", dataset = ds)
    m
  }), datasets)
  saveRDS(metadata, metadata_path)
}
metadata <- readRDS(metadata_path)
request_path <- file.path(root, "extract.rds")
if (!file.exists(request_path)) {
  request <- define_extract_agg("nhgis",
    description = "Mandatory schooling pre-exposure matching: 1850/1860 county covariates",
    datasets = lapply(datasets, function(ds) ds_spec(ds,
      data_tables = metadata[[ds]]$data_tables$name, geog_levels = "county")),
    data_format = "csv_header")
  request <- submit_extract(request)
  saveRDS(request, request_path)
}
request <- get_extract_info(readRDS(request_path))
saveRDS(request, request_path)
message("NHGIS extract ", request$number, ": ", request$status)
if (request$status == "completed") {
  zips <- list.files(root, "^nhgis.*_csv[.]zip$", full.names = TRUE)
  if (!length(zips)) {
    paths <- download_extract(request, download_dir = root, progress = FALSE)
    zips <- unname(paths["data"])
  }
  utils::unzip(zips[1], exdir = file.path(root, "csv"))
  message("Inputs ready: ", root)
} else {
  message("Re-run this script to check/download the existing request; no duplicate submission.")
}
cmf <- file.path(root, "cmf")
dir.create(cmf, showWarnings = FALSE)
for (name in c("1850_ci", "ci_data")) {
  zip <- file.path(cmf, paste0(name, ".zip"))
  if (!file.exists(zip)) curl::curl_download(paste0(
    "https://pub-9228e653a45040598db0c86ec1d93b3c.r2.dev/website_materials/", name, ".zip"),
    zip, handle = curl::new_handle(useragent = "Mozilla/5.0"))
  utils::unzip(zip, exdir = file.path(cmf, name))
}
