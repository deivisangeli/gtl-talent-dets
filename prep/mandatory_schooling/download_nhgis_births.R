# Dedicated, resumable NT001 occurrence-birth extracts. No legacy files are changed.
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
repo_root <- if (length(script)) normalizePath(file.path(dirname(script[1]), '../..'), winslash = '/') else Sys.getenv('GTL_REPO', getwd())
source(file.path(repo_root, 'paths.R'))
suppressPackageStartupMessages(library(ipumsr))
root <- file.path(TALENT_DETS_DATA_DIR, 'raw', 'nhgis_births')
dir.create(root, recursive = TRUE, showWarnings = FALSE)
key <- Sys.getenv('IPUMS_API_KEY')
if (!nzchar(key)) key <- Sys.getenv('IPUMS_KEY')
if (nzchar(key)) set_ipums_api_key(key, save = FALSE)
specification <- list(early = list(dataset = '1915_1941_cVS', years = 1915:1941),
                      late = list(dataset = '1939_1959_cVS', years = 1939:1950))
for (label in names(specification)) {
  spec <- specification[[label]]
  dest <- file.path(root, label)
  dir.create(dest, showWarnings = FALSE)
  manifest <- file.path(dest, 'specification.rds')
  if (file.exists(manifest) && !identical(readRDS(manifest), spec)) stop('Cached specification differs: ', dest)
  zip <- list.files(dest, '_csv[.]zip$', full.names = TRUE)
  if (length(zip)==1L && file.exists(manifest) && file.exists(file.path(dest,'metadata.rds')) &&
      file.exists(file.path(dest,'manifest.txt'))) {
    message('Reusing completed ', label, ' archive')
    utils::unzip(zip, exdir=file.path(dest,'csv'))
    next
  }
  if (!nzchar(key)) stop('Set IPUMS_API_KEY or IPUMS_KEY for uncached requests.')
  saveRDS(spec, manifest)
  meta_file <- file.path(dest, 'metadata.rds')
  if (!file.exists(meta_file)) saveRDS(get_metadata('nhgis', dataset = spec$dataset), meta_file)
  request_file <- file.path(dest, 'extract.rds')
  if (!file.exists(request_file)) {
    request <- define_extract_agg('nhgis', description = paste('AMWS monthly panel occurrence births', label),
      datasets = list(ds_spec(spec$dataset, data_tables = 'NT001',
        geog_levels = c('county', 'state'), years = as.character(spec$years))), data_format = 'csv_header')
    request <- submit_extract(request)
    saveRDS(request, request_file)
  }
  request <- get_extract_info(readRDS(request_file))
  saveRDS(request, request_file)
  message(label, ': extract ', request$number, ' ', request$status)
  if (request$status != 'completed') {
    request <- wait_for_extract(request, initial_delay_seconds = 15, max_delay_seconds = 45,
                                timeout_seconds = 7200, verbose = TRUE)
    saveRDS(request, request_file)
  }
  zip <- list.files(dest, '_csv[.]zip$', full.names = TRUE)
  if (!length(zip)) {
    downloaded <- download_extract(request, download_dir = dest, progress = FALSE)
    zip <- unname(downloaded['data'])
  }
  if (length(zip) != 1L || !file.exists(zip)) stop('Expected one extract archive: ', dest)
  utils::unzip(zip, exdir = file.path(dest, 'csv'))
  writeLines(c(paste('dataset:', spec$dataset), 'table: NT001', 'geographies: county, state',
    paste('years:', paste(spec$years, collapse = ',')), paste('extract:', request$number),
    paste('archive_md5:', unname(tools::md5sum(zip)))), file.path(dest, 'manifest.txt'))
}
message('NHGIS inputs ready: ', root)
