# Convert the IPUMS 1850-1910 full-count fertility/occupation extract to
# year-partitioned Parquet.
#
# The source is IPUMS extract usa_00010: the six surviving 100% censuses (1850,
# 1860, 1870, 1880, 1900, 1910; the 1890 schedules burned in 1921), 304 M
# persons, as a 10 GB gzipped CSV with a quoted header. It holds the 32
# variables of the 1910 extract (usa_00009) plus OCC, the contemporary
# occupation code, which has no labels in the DDI.
#
# Variables a census did not ask are blank, not zero: MARST and DURMARR before
# 1880, CHBORN and CHSURV before 1900, OCC in 1900-1910. They become NULL.
# All other codes are kept raw; decoding happens in build_professor_nchild_decades.R.
#
# 304 M rows do not fit in memory as a table, so unlike the 1910 converter this
# one streams COPY straight from the CSV and runs every check on the Parquet
# afterwards, one year at a time.
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/convert_ipums_1850_1910_fertility.R
#
# Override the defaults with IPUMS_1850_1910_FERTILITY_SOURCE, _DDI and
# _PARQUET_DIR. IPUMS_1910_FERTILITY_PARQUET_DIR names the 1910 Parquet the
# 1910 partition is cross-checked against (skipped if it does not exist).
# IPUMS_1850_1910_EXPECT_ROWS=false drops the per-year row-count check, for
# fixtures. The output path must not already exist.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(xml2)
})

SOURCE <- Sys.getenv("IPUMS_1850_1910_FERTILITY_SOURCE",
                     unset = "D:/ipums_1850_1910_fullcount_fertility.gz")
DDI <- Sys.getenv("IPUMS_1850_1910_FERTILITY_DDI",
                  unset = "D:/ipums_1850_1910_fullcount_ddi.xml")
OUTPUT <- Sys.getenv("IPUMS_1850_1910_FERTILITY_PARQUET_DIR",
                     unset = "D:/ipums_1850_1910_fullcount_fertility")
PARQUET_1910 <- Sys.getenv("IPUMS_1910_FERTILITY_PARQUET_DIR",
                           unset = "D:/ipums_1910_fullcount_fertility")
EXPECT_ROWS <- !identical(tolower(Sys.getenv("IPUMS_1850_1910_EXPECT_ROWS")), "false")
EXPECTED_VARS <- 33L
# One 100% database per census year, with the person counts of the extract as
# delivered on 2026-10-23.
YEARS <- data.frame(
  YEAR = c(1850L, 1860L, 1870L, 1880L, 1900L, 1910L),
  SAMPLE = c(185002L, 186003L, 187003L, 188003L, 190004L, 191004L),
  expected_rows = c(19981435, 27451053, 38515556, 50140482, 75824712, 92043618))

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
  source_path <- normalizePath(SOURCE, winslash = "/", mustWork = TRUE)
  ddi_path <- normalizePath(DDI, winslash = "/", mustWork = TRUE)
  output <- sql_path(OUTPUT)
  parent <- dirname(output)
  stop_if(!dir.exists(parent), "Output parent does not exist: ", parent)
  stop_if(file.exists(output) || dir.exists(output), "Output already exists: ", output)
  suffix <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())
  stage <- paste0(output, ".staging-", suffix)
  work <- paste0(output, ".work-", suffix)
  stop_if(dir.exists(stage) || dir.exists(work), "Unexpected staging/work path collision")

  header <- read_header(source_path)
  ddi <- read_ddi(ddi_path)
  check_ddi(ddi, header, EXPECTED_VARS)
  stop_if(!"OCC" %in% header, "Expected the contemporary OCC column")
  spec <- ddi$spec[match(header, ddi$spec$name), , drop = FALSE]
  say("DDI matched all ", length(header), " CSV fields; source ",
      format(file.info(source_path)$size / 1e9, digits = 4), " GB gzipped")
  dir.create(work, recursive = FALSE)
  dir.create(file.path(work, "spill"), recursive = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb())
  connected <- TRUE
  on.exit(if (connected) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads = 12")
  DBI::dbExecute(con, "SET memory_limit = '20GB'")
  DBI::dbExecute(con, paste0("SET temp_directory = ",
                             sql_string(con, file.path(work, "spill"))))
  DBI::dbExecute(con, "SET preserve_insertion_order = false")
  csv <- paste0("read_csv(", sql_string(con, source_path),
                ", all_varchar = true, header = true)")
  select_fields <- paste(mapply(cast_expression, list(con), spec$name,
                                spec$parquet_type, SIMPLIFY = TRUE), collapse = ", ")

  say("Streaming the CSV to year-partitioned Parquet")
  written <- DBI::dbExecute(con, paste0(
    "COPY (SELECT ", select_fields, " FROM ", csv, " s) TO ", sql_string(con, stage),
    " (FORMAT PARQUET, COMPRESSION ZSTD, PARTITION_BY (YEAR), ",
    "FILENAME_PATTERN 'part_{i}')"))
  say("Rows written: ", format(written, big.mark = ",", scientific = FALSE))
  glob <- file.path(stage, "YEAR=*", "*.parquet")
  parquet <- paste0("read_parquet(", sql_string(con, glob), ", hive_partitioning = true)")
  observed_names <- names(DBI::dbGetQuery(con, paste0("SELECT * FROM ", parquet, " LIMIT 0")))
  stop_if(!setequal(header, observed_names),
          "Parquet schema does not hold exactly the source fields")

  years <- sort(as.integer(sub("^YEAR=", "", basename(list.dirs(stage, recursive = FALSE)))))
  stop_if(!length(years) || !all(years %in% YEARS$YEAR),
          "Unexpected census years in the extract: ", paste(years, collapse = ","))
  say("Checking each year: ", paste(years, collapse = ", "))
  per_year <- do.call(rbind, lapply(years, function(year) {
    one <- paste0("read_parquet(", sql_string(con, file.path(stage, paste0("YEAR=", year),
                                                              "*.parquet")), ")")
    row <- DBI::dbGetQuery(con, paste(
      "SELECT COUNT(*) AS n,",
      "COUNT(*) FILTER (WHERE SAMPLE IS DISTINCT FROM",
      YEARS$SAMPLE[YEARS$YEAR == year], ") AS bad_sample,",
      "COUNT(*) FILTER (WHERE SEX = 1 AND (CHBORN <> 0 OR CHSURV <> 0)) AS male_fertility,",
      "COUNT(*) FILTER (WHERE CHBORN BETWEEN 1 AND 96 AND CHSURV > CHBORN) AS surv_gt_born,",
      "COUNT(*) FILTER (WHERE NCHILD IS NULL) AS nchild_null,",
      "COUNT(MARST) AS marst_filled, COUNT(CHBORN) AS chborn_filled, COUNT(OCC) AS occ_filled,",
      "COUNT(*) FILTER (WHERE SPLOC > 0) AS sploc_pos,",
      "COUNT(*) FILTER (WHERE OCC1950 BETWEEN 12 AND 29) AS professors,",
      "COUNT(*) FILTER (WHERE OCC1950 = 10) AS pres_deans,",
      "SUM(NCHILD) AS sum_nchild,",
      "COUNT(*) FILTER (WHERE HISTID IS NULL OR LENGTH(TRIM(HISTID)) = 0) AS blank_histid,",
      "COUNT(DISTINCT HISTID) AS distinct_histid,",
      "COUNT(DISTINCT (SERIAL, PERNUM)) AS distinct_person_keys FROM", one))
    cbind(YEAR = year, row)
  }))
  stop_if(sum(per_year$n) != written, "Per-year Parquet rows do not add up to rows written")
  stop_if(any(per_year$bad_sample != 0), "Rows outside the year's 100% sample")
  stop_if(any(per_year$male_fertility != 0), "Men with non-N/A CHBORN/CHSURV")
  stop_if(any(per_year$nchild_null != 0), "NCHILD is blank for some persons")
  # The builds key persons on SERIAL x PERNUM, which must be unique. HISTID is
  # not: in 1870, 331 HISTIDs are each carried by two distinct person records
  # (different PERNUM, sometimes different households). They are recorded, not
  # treated as an error.
  stop_if(any(per_year$distinct_person_keys != per_year$n),
          "SERIAL x PERNUM is not unique within a year")
  per_year$histid_shared_rows <- per_year$n - per_year$blank_histid - per_year$distinct_histid
  if (EXPECT_ROWS) {
    stop_if(!identical(years, YEARS$YEAR) ||
              !identical(as.numeric(per_year$n), YEARS$expected_rows),
            "Per-year row counts differ from the extract as profiled: ",
            paste(per_year$YEAR, per_year$n, collapse = "; "))
  }
  utils::write.csv(per_year, file.path(stage, "validation_year.csv"), row.names = FALSE)

  check_1910 <- NULL
  glob_1910 <- file.path(PARQUET_1910, "STATEFIP=*", "*.parquet")
  if (length(Sys.glob(glob_1910)) && 1910L %in% per_year$YEAR) {
    say("Cross-checking 1910 against ", PARQUET_1910)
    check_1910 <- DBI::dbGetQuery(con, paste0(
      "SELECT COUNT(*) AS n, COUNT(*) FILTER (WHERE OCC1950 BETWEEN 12 AND 29) AS professors,",
      " SUM(NCHILD) AS sum_nchild, SUM(CHSURV) AS sum_chsurv FROM read_parquet(",
      sql_string(con, glob_1910), ", hive_partitioning = true)"))
    mine <- per_year[per_year$YEAR == 1910L, ]
    stop_if(check_1910$n != mine$n || check_1910$professors != mine$professors ||
              check_1910$sum_nchild != mine$sum_nchild,
            "The 1910 partition disagrees with the 1910 extract")
  }

  say("Profiling coverage and writing the dictionary")
  coverage <- DBI::dbGetQuery(con, paste0(
    "SELECT ", paste(coverage_expressions(con, spec), collapse = ", "), " FROM ", parquet))
  coverage_year <- DBI::dbGetQuery(con, paste0(
    "SELECT YEAR, COUNT(*) AS n, ", paste(coverage_expressions(con, spec[spec$name != "YEAR", ]),
                                          collapse = ", "),
    " FROM ", parquet, " GROUP BY YEAR ORDER BY YEAR"))
  utils::write.csv(coverage_year, file.path(stage, "coverage_by_year.csv"), row.names = FALSE)
  write_dictionary(stage, ddi, header, coverage, as.numeric(written), source_path, ddi_path,
                   title = "IPUMS 1850-1910 full counts (usa_00010)",
                   notes = c("Blank means not asked that year; see `coverage_by_year.csv`.",
                             "OCC is the contemporary code and carries no labels in the DDI."))

  source_info <- file.info(source_path)
  ddi_info <- file.info(ddi_path)
  manifest <- list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source = list(path = source_path, bytes = unname(source_info$size),
                  modified = format(source_info$mtime, tz = "UTC", usetz = TRUE)),
    ddi = list(path = ddi_path, bytes = unname(ddi_info$size),
               sha256 = digest::digest(file = ddi_path, algo = "sha256"),
               extract = "usa_00010"),
    duckdb_version = as.character(utils::packageVersion("duckdb")),
    parquet_compression = "ZSTD",
    partition = "YEAR",
    columns = header,
    codes_kept_raw = TRUE,
    rows = as.numeric(written),
    by_year = per_year,
    cross_check_1910 = if (is.null(check_1910)) "skipped" else check_1910,
    validation = list(samples_match_years = TRUE, men_fertility_na = TRUE,
                      nchild_never_blank = TRUE, person_key_unique_within_year = TRUE,
                      expected_rows_checked = EXPECT_ROWS,
                      ddi_columns_and_labels_match = TRUE),
    elapsed_minutes = round(as.numeric(difftime(Sys.time(), started, units = "mins")), 2))
  jsonlite::write_json(manifest, file.path(stage, "_manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE, na = "null", digits = NA)

  DBI::dbDisconnect(con, shutdown = TRUE)
  connected <- FALSE
  rm(con)
  gc(verbose = FALSE)
  promote_staging(stage, output)
  unlink(work, recursive = TRUE, force = TRUE)
  say("Complete: ", format(written, big.mark = ",", scientific = FALSE), " rows in ",
      nrow(per_year), " census years at ", output)
}

main()
