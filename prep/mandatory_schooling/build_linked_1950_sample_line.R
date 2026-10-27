# Enrich 1950 sample-line respondents with their 1950 and linked 1940 census rows.
# Run from any directory: Rscript prep/mandatory_schooling/build_linked_1950_sample_line.R

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

sample_source <- Sys.getenv(
  "IPUMS_1950_SAMPLE_LINE_SOURCE",
  unset = "C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets/Data/raw/mandatory_schooling/sample_line_linked_1940_1950_census.gz")
base_dir <- Sys.getenv("IPUMS_LINKED_1950_PARQUET_DIR",
                       unset = "D:/ipums_linked_1900_1950")
output_dir <- Sys.getenv("IPUMS_1950_SAMPLE_LINE_PARQUET_DIR",
                         unset = "D:/ipums_linked_1950_sample_line")
expected_1950 <- as.numeric(Sys.getenv("IPUMS_1950_SAMPLE_LINE_EXPECTED_COUNT",
                                         unset = "15337444"))

fail_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
sql_path <- function(path) gsub("\\\\", "/", path)
quote_str <- function(con, value) as.character(dbQuoteString(con, sql_path(value)))
say <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)
one <- function(con, query) dbGetQuery(con, query)[[1L]][[1L]]

shared_check <- paste(
  c("SERIAL", "GQ", "PERNUM", "VERSIONHIST", "HISTID", "HIK",
    paste0("LINK", seq(1900, 1950, 10)), "VERSIONHIK"),
  collapse = ",")
shared_mismatch <- function(p = "p", s = "s") {
  fields <- strsplit(shared_check, ",", fixed = TRUE)[[1L]]
  paste(sprintf("%s.%s IS DISTINCT FROM %s.%s", p, fields, s, fields),
        collapse = " OR ")
}

write_field_dictionary <- function(base, stage) {
  path <- file.path(base, "variable_dictionary.csv")
  fail_if(!file.exists(path), "Missing base variable dictionary: ", path)
  dictionary <- utils::read.csv(path, stringsAsFactors = FALSE,
                                check.names = FALSE)
  keep <- c("name", "label", "concept", "description", "parquet_type")
  fail_if(!all(keep %in% names(dictionary)), "Unexpected base dictionary schema")
  dictionary <- dictionary[, keep]
  additions <- data.frame(
    name = c("SLWT", "SLREC", "VETWWI", "VETWWII"),
    label = c("Sample-line weight", "Sample-line record indicator",
              "World War I veteran response", "World War II veteran response"),
    concept = rep("Sample-line source field", 4L),
    description = c(
      "Weight from this census year's sample-line record; NULL on a 1940 row absent from the sample-line source.",
      "Sample-line code from this census year's source row; NULL on a 1940 row absent from the source.",
      "Unmodified coded response from this census year's sample-line source; NULL if blank or absent.",
      "Unmodified coded response from this census year's sample-line source; NULL if blank or absent."),
    parquet_type = c("DECIMAL(12,2)", "INTEGER", "INTEGER", "INTEGER"),
    stringsAsFactors = FALSE)
  utils::write.csv(rbind(dictionary, additions),
                   file.path(stage, "field_dictionary.csv"), row.names = FALSE,
                   na = "")
}

main <- function() {
  started <- Sys.time()
  fail_if(!file.exists(sample_source), "Sample-line gzip not found: ", sample_source)
  fail_if(!is.finite(expected_1950) || expected_1950 < 1,
          "Expected 1950 count must be positive")
  glob <- file.path(base_dir, "birth_bpl=*", "*.parquet")
  fail_if(length(Sys.glob(glob)) == 0L, "Base Parquet not found: ", glob)
  fail_if(file.exists(output_dir) || dir.exists(output_dir),
          "Output already exists: ", output_dir)
  parent <- dirname(output_dir)
  fail_if(!dir.exists(parent), "Output parent does not exist: ", parent)
  fail_if(normalizePath(parent, winslash = "/", mustWork = TRUE) ==
            normalizePath(dirname(sample_source), winslash = "/", mustWork = TRUE),
          "Output must not be in the raw source directory")

  output <- sql_path(output_dir)
  suffix <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())
  stage <- paste0(output, ".staging-", suffix)
  work <- paste0(output, ".work-", suffix)
  dir.create(work)
  dir.create(file.path(work, "spill"))
  con <- dbConnect(duckdb(), dbdir = file.path(work, "build.duckdb"))
  connected <- TRUE
  on.exit(if (connected) dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dbExecute(con, "SET threads = 8")
  dbExecute(con, "SET memory_limit = '20GB'")
  dbExecute(con, paste0("SET temp_directory = ",
                        quote_str(con, file.path(work, "spill"))))
  dbExecute(con, "SET preserve_insertion_order = false")
  dbExecute(con, "SET partitioned_write_max_open_files = 2048")
  source_csv <- paste0("read_csv(", quote_str(con, sample_source),
                       ", all_varchar = true, header = true)")
  base_parquet <- paste0("read_parquet(", quote_str(con, glob),
                         ", hive_partitioning = true)")
  source_fields <- c("YEAR", "SERIAL", "GQ", "PERNUM", "SLWT", "SLREC",
                     "VERSIONHIST", "VETWWII", "VETWWI", "HISTID", "HIK",
                     paste0("LINK", seq(1900, 1950, 10)), "VERSIONHIK")
  actual_fields <- names(dbGetQuery(con, paste0("SELECT * FROM ", source_csv,
                                                 " LIMIT 0")))
  fail_if(!identical(actual_fields, source_fields),
          "Sample-line header does not match the expected 18 fields")

  say("Reading the sample-line source")
  numeric_fields <- c("YEAR", "SERIAL", "GQ", "PERNUM", "SLREC",
                      "VERSIONHIST", "VETWWII", "VETWWI",
                      paste0("LINK", seq(1900, 1950, 10)))
  casts <- vapply(source_fields, function(field) {
    if (field == "SLWT") return("CAST(NULLIF(TRIM(SLWT), '') AS DECIMAL(12,2)) AS SLWT")
    if (field %in% numeric_fields) {
      return(sprintf("CAST(NULLIF(TRIM(%s), '') AS INTEGER) AS %s", field, field))
    }
    sprintf("NULLIF(TRIM(%s), '') AS %s", field, field)
  }, character(1L))
  dbExecute(con, paste0("CREATE TABLE sample AS SELECT ",
                        paste(casts, collapse = ", "), " FROM ", source_csv))
  source_counts <- dbGetQuery(con, "SELECT YEAR, COUNT(*) AS n,
    COUNT(*) FILTER (WHERE SLREC IS DISTINCT FROM 2) AS invalid_slrec,
    COUNT(*) FILTER (WHERE HISTID IS NULL OR HIK IS NULL) AS missing_ids,
    COUNT(*) - COUNT(DISTINCT HISTID) AS duplicate_histid
    FROM sample GROUP BY YEAR ORDER BY YEAR")
  fail_if(!identical(source_counts$YEAR, c(1940L, 1950L)),
          "Sample source contains unexpected years")
  fail_if(any(source_counts$invalid_slrec != 0) ||
            any(source_counts$missing_ids != 0) ||
            any(source_counts$duplicate_histid != 0),
          "Sample source has invalid SLREC, missing IDs, or duplicate HISTIDs")
  n50 <- source_counts$n[source_counts$YEAR == 1950]
  fail_if(n50 != expected_1950, "1950 sample-line count differs from expected: ",
          n50, " vs ", expected_1950)
  fail_if(one(con, "SELECT COUNT(*) - COUNT(DISTINCT HIK) FROM sample
                     WHERE YEAR = 1950") != 0,
          "1950 sample-line HIK is not unique")

  say("Matching 1950 sample-line records to the base census by HISTID")
  dbExecute(con, paste0("CREATE TABLE rows50 AS SELECT p.*, s.SLWT, s.SLREC,
    s.VETWWI, s.VETWWII, (", shared_mismatch(), ") AS _source_mismatch
    FROM ", base_parquet, " p JOIN sample s
      ON p.YEAR = 1950 AND s.YEAR = 1950 AND p.HISTID = s.HISTID
    WHERE p.YEAR = 1950"))
  matched50 <- one(con, "SELECT COUNT(*) FROM rows50")
  fail_if(matched50 != n50, "1950 source/base match is incomplete or duplicated: ",
          matched50, " vs ", n50)
  fail_if(one(con, "SELECT COUNT(*) FROM rows50 WHERE _source_mismatch") != 0,
          "Shared fields disagree between 1950 sample source and base census")

  say("Finding reciprocal 1940 links")
  dbExecute(con, paste0("CREATE TABLE rows40 AS SELECT p.*, s.SLWT, s.SLREC,
    s.VETWWI, s.VETWWII,
    CASE WHEN s.HISTID IS NULL THEN FALSE ELSE (", shared_mismatch(),
    ") END AS _source_mismatch
    FROM ", base_parquet, " p
    JOIN (SELECT HIK FROM rows50 WHERE LINK1940 = 1) c ON p.HIK = c.HIK
    LEFT JOIN sample s ON s.YEAR = 1940 AND p.HISTID = s.HISTID
    WHERE p.YEAR = 1940 AND p.LINK1950 = 1"))
  linked40 <- one(con, "SELECT COUNT(*) FROM rows40")
  fail_if(one(con, "SELECT COUNT(*) - COUNT(DISTINCT HIK) FROM rows40") != 0,
          "1940 linked HIK is not one-to-one")
  fail_if(one(con, "SELECT COUNT(*) FROM rows40 WHERE _source_mismatch") != 0,
          "Shared fields disagree between 1940 sample source and base census")
  flagged50 <- one(con, "SELECT COUNT(*) FROM rows50 WHERE LINK1940 = 1")
  fail_if(linked40 > flagged50, "More 1940 links than eligible 1950 records")
  sample40_in_pairs <- one(con, "SELECT COUNT(*) FROM rows40 WHERE SLREC = 2")
  say("Matched ", linked40, " reciprocal 1940 rows; ",
      flagged50 - linked40, " flagged links have no valid 1940 row")

  say("Writing birthplace-partitioned Parquet")
  dbExecute(con, paste0("COPY (SELECT * EXCLUDE (_source_mismatch) FROM rows50
    UNION ALL SELECT * EXCLUDE (_source_mismatch) FROM rows40) TO ",
    quote_str(con, stage),
    " (FORMAT PARQUET, COMPRESSION ZSTD, PARTITION_BY (birth_bpl),
       FILENAME_PATTERN 'part_{i}')"))
  files <- list.files(stage, pattern = "\\.parquet$", recursive = TRUE,
                      full.names = TRUE)
  fail_if(length(files) == 0L, "No Parquet files were written")
  written <- paste0("read_parquet(",
                    quote_str(con, file.path(stage, "birth_bpl=*", "*.parquet")),
                    ", hive_partitioning = true)")
  output_counts <- dbGetQuery(con, paste0("SELECT YEAR, COUNT(*) AS n,
    COUNT(*) - COUNT(DISTINCT HISTID) AS duplicate_histid,
    COUNT(*) FILTER (WHERE scientist_occ1950) AS scientists,
    COUNT(*) FILTER (WHERE SLREC = 2) AS sample_line_rows
    FROM ", written, " GROUP BY YEAR ORDER BY YEAR"))
  fail_if(!identical(output_counts$YEAR, c(1940L, 1950L)) ||
            any(output_counts$n != c(linked40, n50)) ||
            any(output_counts$duplicate_histid != 0) ||
            output_counts$sample_line_rows[output_counts$YEAR == 1950] != n50 ||
            output_counts$sample_line_rows[output_counts$YEAR == 1940] != sample40_in_pairs,
          "Output row count, identity, or sample-line fields failed validation")
  base_profile <- dbGetQuery(con, "SELECT YEAR, BPL, birth_bpl,
    COUNT(*) AS n, COUNT(*) FILTER (WHERE scientist_occ1950) AS scientists
    FROM (SELECT * FROM rows50 UNION ALL SELECT * FROM rows40)
    GROUP BY ALL ORDER BY ALL")
  output_profile <- dbGetQuery(con, paste0("SELECT YEAR, BPL, birth_bpl,
    COUNT(*) AS n, COUNT(*) FILTER (WHERE scientist_occ1950) AS scientists
    FROM ", written, " GROUP BY ALL ORDER BY ALL"))
  fail_if(!identical(base_profile, output_profile),
          "Output birthplace or scientist counts differ from selected base rows")
  field_names <- names(dbGetQuery(con, paste0("SELECT * FROM ", written,
                                                " LIMIT 0")))
  expected_fields <- setdiff(names(dbGetQuery(con, "SELECT * FROM rows50 LIMIT 0")),
                             "_source_mismatch")
  fail_if(!setequal(field_names, expected_fields), "Output field schema changed")

  write_field_dictionary(base_dir, stage)
  utils::write.csv(source_counts, file.path(stage, "source_year_counts.csv"),
                   row.names = FALSE)
  utils::write.csv(output_profile, file.path(stage, "validation_year_bpl.csv"),
                   row.names = FALSE)
  info <- file.info(sample_source)
  manifest <- list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    sample_source = list(path = normalizePath(sample_source, winslash = "/"),
                         bytes = unname(info$size),
                         modified_utc = format(info$mtime, tz = "UTC", usetz = TRUE)),
    base_parquet = normalizePath(base_dir, winslash = "/"),
    output = output,
    selection = "1950 sample-line respondents; reciprocal 1940 HIK links when present",
    rows_by_year = output_counts,
    source_rows_by_year = source_counts,
    flagged_1940_links = flagged50,
    matched_1940_links = linked40,
    flagged_links_without_1940_row = flagged50 - linked40,
    rows_1940_with_sample_line_fields = sample40_in_pairs,
    parquet_files = length(files),
    validation = list(source_1950_rows_matched = TRUE,
                      shared_source_fields_agree = TRUE,
                      one_to_one_links = TRUE,
                      output_ids_unique = TRUE,
                      birthplace_and_scientist_counts_match = TRUE),
    elapsed_minutes = round(as.numeric(difftime(Sys.time(), started,
                                                units = "mins")), 2))
  jsonlite::write_json(manifest, file.path(stage, "_manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  dbDisconnect(con, shutdown = TRUE)
  connected <- FALSE
  rm(con)
  gc(verbose = FALSE)
  promoted <- FALSE
  for (attempt in seq_len(30L)) {
    fail_if(file.exists(output) || dir.exists(output),
            "Final output appeared during build: ", output)
    promoted <- isTRUE(suppressWarnings(file.rename(stage, output)))
    if (promoted) break
    Sys.sleep(2)
  }
  fail_if(!promoted, "Could not promote validated dataset: ", stage)
  unlink(work, recursive = TRUE, force = TRUE)
  say("Complete: ", linked40 + n50, " rows at ", output)
}

if (sys.nframe() == 0L) main()
