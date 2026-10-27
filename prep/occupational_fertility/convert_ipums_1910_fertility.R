# Convert the IPUMS 1910 full-count fertility/occupation extract to
# state-partitioned Parquet.
#
# The source is IPUMS extract usa_00009, delivered as a gzipped CSV with a quoted
# header, together with its DDI. The DDI says "fixed length fields", but the
# column names in the CSV header are what this script checks against it.
# D:/usa_00003_ddi.xml is NOT this extract's codebook: it describes the
# 1900-1950 linked extract read by prep/mandatory_schooling/convert_ipums_linked_1950.R.
#
# IPUMS codes are kept raw. CHBORN and CHSURV are offset by one (00 = N/A,
# 01 = none, k+1 = k children); decoding happens in build_professor_fertility.R.
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/convert_ipums_1910_fertility.R
#
# Override the defaults with IPUMS_1910_FERTILITY_SOURCE, IPUMS_1910_FERTILITY_DDI
# and IPUMS_1910_FERTILITY_PARQUET_DIR. The output path must not already exist.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(xml2)
})

SOURCE <- Sys.getenv("IPUMS_1910_FERTILITY_SOURCE",
                     unset = "D:/ipums_1910_birth_occupation_fullcount.gz")
DDI <- Sys.getenv("IPUMS_1910_FERTILITY_DDI",
                  unset = "D:/ipums_1910_birth_occupation_ddi.xml")
OUTPUT <- Sys.getenv("IPUMS_1910_FERTILITY_PARQUET_DIR",
                     unset = "D:/ipums_1910_fullcount_fertility")
EXPECTED_VARS <- 32L
EXPECTED_SAMPLE <- 191004L

script_dir <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/"))
  else file.path(Sys.getenv("GTL_REPO", unset = getwd()), "prep/occupational_fertility")
})
source(file.path(script_dir, "ipums_convert_helpers.R"))

main <- function() {
  started <- Sys.time()
  stop_if(!file.exists(SOURCE), "Source CSV not found: ", SOURCE)
  stop_if(!file.exists(DDI), "DDI not found: ", DDI)
  source <- normalizePath(SOURCE, winslash = "/", mustWork = TRUE)
  ddi_path <- normalizePath(DDI, winslash = "/", mustWork = TRUE)
  output <- sql_path(OUTPUT)
  parent <- dirname(output)
  stop_if(!dir.exists(parent), "Output parent does not exist: ", parent)
  stop_if(file.exists(output) || dir.exists(output), "Output already exists: ", output)
  suffix <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())
  stage <- paste0(output, ".staging-", suffix)
  work <- paste0(output, ".work-", suffix)
  stop_if(dir.exists(stage) || dir.exists(work), "Unexpected staging/work path collision")

  header <- read_header(source)
  ddi <- read_ddi(ddi_path)
  check_ddi(ddi, header, EXPECTED_VARS)
  spec <- ddi$spec[match(header, ddi$spec$name), , drop = FALSE]
  say("DDI matched all ", length(header), " CSV fields; source ",
      format(file.info(source)$size / 1e9, digits = 4), " GB gzipped")
  dir.create(work, recursive = FALSE)
  dir.create(file.path(work, "spill"), recursive = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(work, "conversion.duckdb"))
  connected <- TRUE
  on.exit(if (connected) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads = 12")
  DBI::dbExecute(con, "SET memory_limit = '20GB'")
  DBI::dbExecute(con, paste0("SET temp_directory = ",
                             sql_string(con, file.path(work, "spill"))))
  DBI::dbExecute(con, "SET preserve_insertion_order = false")
  csv <- paste0("read_csv(", sql_string(con, source),
                ", all_varchar = true, header = true)")

  # Typed once into the work database, so the counts below and the Parquet
  # write read the same rows rather than decompressing the CSV three times.
  select_fields <- paste(mapply(cast_expression, list(con), spec$name,
                                spec$parquet_type, SIMPLIFY = TRUE), collapse = ", ")
  say("Loading and typing the CSV")
  DBI::dbExecute(con, paste0("CREATE TABLE persons AS SELECT ", select_fields,
                             " FROM ", csv, " s"))

  say("Checking sample, year and fertility-code invariants")
  checks <- DBI::dbGetQuery(con, paste(
    "SELECT COUNT(*) AS n,",
    "COUNT(*) FILTER (WHERE SAMPLE IS DISTINCT FROM", EXPECTED_SAMPLE, ") AS bad_sample,",
    "COUNT(*) FILTER (WHERE YEAR IS DISTINCT FROM 1910) AS bad_year,",
    "COUNT(*) FILTER (WHERE SEX = 1 AND (CHBORN <> 0 OR CHSURV <> 0)) AS male_fertility,",
    "COUNT(*) FILTER (WHERE (CHBORN = 0) <> (CHSURV = 0)) AS na_mismatch,",
    "COUNT(*) FILTER (WHERE CHBORN BETWEEN 1 AND 96 AND CHSURV > CHBORN) AS surv_gt_born,",
    "COUNT(*) FILTER (WHERE CHBORN BETWEEN 97 AND 99) AS chborn_unknown,",
    "COUNT(*) FILTER (WHERE HISTID IS NULL OR LENGTH(TRIM(HISTID)) = 0) AS blank_histid,",
    "COUNT(DISTINCT HISTID) AS distinct_histid",
    "FROM persons"))
  say("Rows: ", format(checks$n, big.mark = ",", scientific = FALSE))
  stop_if(checks$bad_sample != 0 || checks$bad_year != 0,
          "Rows outside the 1910 100% sample: ", checks$bad_sample)
  stop_if(checks$male_fertility != 0, "Men with non-N/A CHBORN/CHSURV: ",
          checks$male_fertility)
  stop_if(checks$distinct_histid != checks$n - checks$blank_histid,
          "HISTID is not unique")

  say("Writing state-partitioned Parquet")
  DBI::dbExecute(con, paste0(
    "COPY (SELECT * FROM persons) TO ", sql_string(con, stage),
    " (FORMAT PARQUET, COMPRESSION ZSTD, PARTITION_BY (STATEFIP), ",
    "FILENAME_PATTERN 'part_{i}')"))
  parquet <- paste0("read_parquet(",
                    sql_string(con, file.path(stage, "STATEFIP=*", "*.parquet")),
                    ", hive_partitioning = true)")
  observed_names <- names(DBI::dbGetQuery(con, paste0("SELECT * FROM ", parquet, " LIMIT 0")))
  stop_if(!setequal(header, observed_names),
          "Parquet schema does not hold exactly the source fields")

  say("Checking source and Parquet counts by state")
  counts <- merge(
    DBI::dbGetQuery(con, "SELECT STATEFIP, COUNT(*) AS n_source,
                            COUNT(*) FILTER (WHERE OCC1950 BETWEEN 12 AND 29) AS prof_source,
                            SUM(CHSURV) AS chsurv_source
                          FROM persons GROUP BY 1"),
    DBI::dbGetQuery(con, paste0("SELECT CAST(STATEFIP AS INTEGER) AS STATEFIP,
                                   COUNT(*) AS n_parquet,
                                   COUNT(*) FILTER (WHERE OCC1950 BETWEEN 12 AND 29) AS prof_parquet,
                                   SUM(CHSURV) AS chsurv_parquet
                                 FROM ", parquet, " GROUP BY 1")),
    by = "STATEFIP", all = TRUE)
  stop_if(anyNA(counts) ||
            any(counts$n_source != counts$n_parquet) ||
            any(counts$prof_source != counts$prof_parquet) ||
            any(counts$chsurv_source != counts$chsurv_parquet),
          "Source and Parquet disagree by state")
  counts <- counts[order(counts$STATEFIP), ]
  utils::write.csv(counts, file.path(stage, "validation_state.csv"), row.names = FALSE)

  say("Profiling coverage and writing the dictionary")
  coverage_fields <- coverage_expressions(con, spec)
  coverage <- DBI::dbGetQuery(con, paste0("SELECT ", paste(coverage_fields, collapse = ", "),
                                          " FROM persons"))
  write_dictionary(stage, ddi, header, coverage, as.numeric(checks$n), source, ddi_path,
                   title = "IPUMS 1910 full count (usa_00009)")

  source_info <- file.info(source)
  ddi_info <- file.info(ddi_path)
  manifest <- list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source = list(path = source, bytes = unname(source_info$size),
                  modified = format(source_info$mtime, tz = "UTC", usetz = TRUE)),
    ddi = list(path = ddi_path, bytes = unname(ddi_info$size),
               sha256 = digest::digest(file = ddi_path, algo = "sha256"),
               extract = "usa_00009"),
    duckdb_version = as.character(utils::packageVersion("duckdb")),
    parquet_compression = "ZSTD",
    partition = "STATEFIP",
    columns = header,
    codes_kept_raw = TRUE,
    rows = as.numeric(checks$n),
    rows_by_state = counts[, c("STATEFIP", "n_source", "prof_source")],
    diagnostics = list(
      chborn_chsurv_na_mismatch = as.numeric(checks$na_mismatch),
      chsurv_gt_chborn = as.numeric(checks$surv_gt_born),
      chborn_unknown_97_99 = as.numeric(checks$chborn_unknown),
      blank_histid = as.numeric(checks$blank_histid)),
    validation = list(sample_191004_only = TRUE, men_fertility_na = TRUE,
                      histid_unique = TRUE, state_counts_match = TRUE,
                      ddi_columns_and_labels_match = TRUE),
    elapsed_minutes = round(as.numeric(difftime(Sys.time(), started, units = "mins")), 2))
  jsonlite::write_json(manifest, file.path(stage, "_manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE, na = "null")

  DBI::dbDisconnect(con, shutdown = TRUE)
  connected <- FALSE
  rm(con)
  gc(verbose = FALSE)
  promote_staging(stage, output)
  unlink(work, recursive = TRUE, force = TRUE)
  say("Complete: ", format(manifest$rows, big.mark = ",", scientific = FALSE),
      " rows in ", nrow(counts), " state partitions at ", output,
      " (CHSURV > CHBORN: ", checks$surv_gt_born,
      "; CHBORN unknown: ", checks$chborn_unknown, ")")
}

main()
