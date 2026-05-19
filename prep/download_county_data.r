######################################################################

### Downloading IPUMS/NHGIS county data using R

######################################################################
rm(list = ls());gc()
pacman::p_load(ipumsr, tidyverse, data.table, arrow)

set_ipums_api_key(Sys.getenv("IPUMS_KEY"), save = FALSE)

det_dir <- Sys.getenv("DET_DIR")
if (det_dir == "") {
 stop("DET_DIR is not set. Add it to your .Renviron before running this script.")
}

raw_dir <- file.path(det_dir, "raw")
nhgis_datasets <- get_metadata_catalog("nhgis", "datasets")

write_parquet_schema <- function(parquet_manifest, schema_file) {
 schema <- map_dfr(parquet_manifest$parquet_file, \(parquet_file) {
  data <- read_parquet(parquet_file)

  tibble(
   parquet_file = parquet_file,
   column_index = seq_along(data),
   column_name = names(data),
   r_class = map_chr(data, \(x) paste(class(x), collapse = ";"))
  )
 })

 write_csv(schema, schema_file)
 schema
}

download_nhgis_domain <- function(domain_label,
                                  output_subdir,
                                  requested_datasets,
                                  extract_description,
                                  requested_tables = NULL) {
 domain_dir <- file.path(raw_dir, output_subdir)
 parquet_dir <- file.path(domain_dir, "parquet")
 dir.create(domain_dir, recursive = TRUE, showWarnings = FALSE)
 dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

 ### Resolve dataset names against the NHGIS catalog. This fixes harmless
 ### case differences like 1870_CMfg -> 1870_cMfg.
 dataset_resolved <- map_chr(requested_datasets, \(x) {
  hit <- nhgis_datasets$name[tolower(nhgis_datasets$name) == tolower(x)]
  if (length(hit) == 0) {
   stop("Dataset not found in NHGIS catalog: ", x)
  }
  hit[1]
 })

 domain_meta <- set_names(
  map(dataset_resolved, \(x) get_metadata(collection = "nhgis", dataset = x)),
  dataset_resolved
 )

 domain_tables <- map(dataset_resolved, \(dataset_name) {
  if (!is.null(requested_tables) && dataset_name %in% names(requested_tables)) {
   tables <- requested_tables[[dataset_name]]
   missing_tables <- setdiff(tables, domain_meta[[dataset_name]]$data_tables$name)
   if (length(missing_tables) > 0) {
    stop(
     "Data table(s) not found for ", dataset_name, ": ",
     paste(missing_tables, collapse = ", ")
    )
   }
   tables
  } else {
   domain_meta[[dataset_name]]$data_tables$name
  }
 })

 domain_manifest <- tibble(
  requested = requested_datasets,
  dataset = dataset_resolved,
  geog_level = map_chr(domain_meta, \(x) {
   if ("county" %in% x$geog_levels$name) "county" else "state"
  }),
  n_tables_available = map_int(domain_meta, \(x) nrow(x$data_tables)),
  n_tables_requested = map_int(domain_tables, length),
  data_tables = map_chr(domain_tables, \(x) paste(x, collapse = ";"))
 )

 write_csv(
  domain_manifest,
  file.path(domain_dir, paste0("nhgis_", domain_label, "_manifest.csv"))
 )

 ### Build and download the NHGIS extract. If the zip already exists, reuse it
 ### and only regenerate parquet files.
 existing_zip <- list.files(
  domain_dir,
  pattern = "^nhgis[0-9]+_csv[.]zip$",
  full.names = TRUE
 )

 if (length(existing_zip) == 0) {
  domain_specs <- map2(domain_manifest$dataset, domain_tables, \(dataset_name, tables) {
   ds_spec(
    dataset_name,
    data_tables = tables,
    geog_levels = domain_manifest$geog_level[domain_manifest$dataset == dataset_name]
   )
  })

  domain_extract <- define_extract_agg(
   collection = "nhgis",
   description = extract_description,
   datasets = domain_specs,
   data_format = "csv_header"
  )

  submitted_extract <- submit_extract(domain_extract)
  saveRDS(
   submitted_extract,
   file.path(
    domain_dir,
    paste0("nhgis_", domain_label, "_extract_", submitted_extract$number, "_submitted.rds")
   )
  )

  ready_extract <- wait_for_extract(
   submitted_extract,
   initial_delay_seconds = 15,
   max_delay_seconds = 60,
   timeout_seconds = 7200,
   verbose = TRUE
  )

  saveRDS(
   ready_extract,
   file.path(
    domain_dir,
    paste0("nhgis_", domain_label, "_extract_", ready_extract$number, "_ready.rds")
   )
  )

  downloaded_paths <- download_extract(
   ready_extract,
   download_dir = domain_dir,
   overwrite = TRUE,
   progress = TRUE
  )

  zip_path <- unname(downloaded_paths["data"])
 } else {
  zip_path <- existing_zip[which.max(file.info(existing_zip)$mtime)]
  message("Using existing NHGIS extract zip: ", zip_path)
 }

 extract_root <- tools::file_path_sans_ext(basename(zip_path))
 extract_dir <- file.path(domain_dir, extract_root)
 utils::unzip(zip_path, exdir = domain_dir)

 csv_files <- list.files(
  extract_dir,
  pattern = "[.]csv$",
  full.names = TRUE
 )

 if (length(csv_files) == 0) {
  stop("No CSV files found after extracting: ", zip_path)
 }

 ### Read the NHGIS CSVs with ipumsr metadata handling and save as parquet.
 parquet_manifest <- map_dfr(csv_files, \(csv_file) {
  message("Reading ", basename(csv_file), " with ipumsr...")
  data <- read_ipums_agg(
   csv_file,
   remove_extra_header = TRUE,
   verbose = FALSE
  )

  parquet_file <- file.path(
   parquet_dir,
   paste0(tools::file_path_sans_ext(basename(csv_file)), ".parquet")
  )

  write_parquet(data, parquet_file)

  tibble(
   csv_file = csv_file,
   parquet_file = parquet_file,
   rows = nrow(data),
   cols = ncol(data)
  )
 })

 write_csv(
  parquet_manifest,
  file.path(parquet_dir, paste0("nhgis_", domain_label, "_parquet_manifest.csv"))
 )

 write_parquet_schema(
  parquet_manifest,
  file.path(parquet_dir, paste0("nhgis_", domain_label, "_parquet_schema.csv"))
 )

 message("Saved parquet files to: ", parquet_dir)

 invisible(parquet_manifest)
}

### Manufacture datasets
manuf_dataset <- c(
 "1840_cMfg",
 "1870_CMfg",
 "1880_sPbSMX",
 "1890_cPHAM",
 "1900_cPHAM",
 "1920_cPHAM"
)

download_nhgis_domain(
 domain_label = "manufacturing",
 output_subdir = "nhgis_manufacturing",
 requested_datasets = manuf_dataset,
 extract_description = "GTL manufacturing NHGIS datasets"
)

### Farming datasets
farming_dataset <- c(
 "1840_cAg",
 "1870_cAg",
 "1880_cAg",
 "1890_cAg",
 "1900_cAg",
 "1920_cAg"
)

download_nhgis_domain(
 domain_label = "farming",
 output_subdir = "nhgis_farming",
 requested_datasets = farming_dataset,
 extract_description = "GTL farming NHGIS datasets"
)

### Demographic datasets: total population, slavery, and literacy/illiteracy
demographics_tables <- list(
 "1790_cPop" = c("NT1", "NT18"),
 "1800_cPop" = c("NT1", "NT6"),
 "1810_cPop" = c("NT1", "NT6"),
 "1820_cPop" = c("NT1", "NT10"),
 "1830_cPop" = c("NT1", "NT12"),
 "1840_cPopX" = c("NT1", "NT16", "NT25"),
 "1850_cPAX" = c("NT1", "NT6", "NT32"),
 "1860_cPAX" = c("NT1", "NT16"),
 "1870_cPAX" = c("NT1", "NT13", "NT14"),
 "1880_cPAX" = c("NT1"),
 "1890_cPHAM" = c("NT1"),
 "1900_cPHAM" = c("NT1", "NT16"),
 "1910_cPHA" = c("NT1", "NT21", "NT22"),
 "1920_cPHAM" = c("NT1", "NT19", "NT20")
)

download_nhgis_domain(
 domain_label = "demographics",
 output_subdir = "nhgis_demographics",
 requested_datasets = names(demographics_tables),
 extract_description = "GTL demographic NHGIS datasets",
 requested_tables = demographics_tables
)
