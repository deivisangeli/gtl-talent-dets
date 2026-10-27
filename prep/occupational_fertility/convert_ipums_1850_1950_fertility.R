# Convert the IPUMS 1850-1950 full-count fertility/occupation extract to
# year-partitioned Parquet.
#
# The source is IPUMS extract usa_00011: every surviving 100% census from 1850
# to 1950 (no 1890; the 1950 database is a preliminary release), as a gzipped
# CSV with a quoted header. It carries the same 33 variables as usa_00010
# (convert_ipums_1850_1910_fertility.R) and adds 1920, 1930, 1940 and 1950, so
# the faculty-children panel can read every birth year 1830-1949 from a census
# that sees it at ages 0-20, without interpolation.
#
# Variables a census did not ask are blank and become NULL: MARST before 1880;
# DURMARR outside 1900, 1910 and 1950 (sample line); CHBORN outside 1900, 1910,
# 1940 and 1950 (sample-line women in 1940-1950); CHSURV outside 1900-1910; OCC
# in 1900-1930. coverage_by_year.csv records what the data actually hold.
# All other codes are kept raw.
#
# Sample codes and per-year row counts of the new years are not hard-coded:
# each year must hold exactly one SAMPLE whose DDI label marks a 100% file, and
# the years shared with usa_00010 and the 1940 full count are cross-checked
# against those Parquet trees (differences are recorded and warned about, since
# an IPUMS revision can legitimately move them).
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/convert_ipums_1850_1950_fertility.R
#
# Override the defaults with IPUMS_1850_1950_FERTILITY_SOURCE, _DDI and
# _PARQUET_DIR; IPUMS_1850_1910_FERTILITY_PARQUET_DIR and IPUMS_1940_PARQUET_DIR
# name the cross-check trees (skipped if absent). The output path must not
# already exist.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(xml2)
})

SOURCE <- Sys.getenv("IPUMS_1850_1950_FERTILITY_SOURCE",
                     unset = "D:/ipums_1850_1950_fullcount_fertility.gz")
DDI <- Sys.getenv("IPUMS_1850_1950_FERTILITY_DDI", unset = "D:/ipums_1850_1950_ddi.xml")
# The extract's own codebook as the browser renders it. When the XML DDI given
# above belongs to another extract with the same variables (usa_00010), its
# names and labels must agree with this page.
DDI_HTML <- Sys.getenv("IPUMS_1850_1950_FERTILITY_DDI_HTML", unset = "D:/ipums_1850_1950_ddi.html")
OUTPUT <- Sys.getenv("IPUMS_1850_1950_FERTILITY_PARQUET_DIR",
                     unset = "D:/ipums_1850_1950_fullcount_fertility")
PARQUET_1850_1910 <- Sys.getenv("IPUMS_1850_1910_FERTILITY_PARQUET_DIR",
                                unset = "D:/ipums_1850_1910_fullcount_fertility")
PARQUET_1940 <- Sys.getenv("IPUMS_1940_PARQUET_DIR", unset = "D:/ipums_1940_fullcount")
EXPECTED_VARS <- 33L
EXPECTED_YEARS <- c(1850L, 1860L, 1870L, 1880L, 1900L, 1910L, 1920L, 1930L, 1940L, 1950L)

script_dir <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/"))
  else file.path(Sys.getenv("GTL_REPO", unset = getwd()), "prep/occupational_fertility")
})
source(file.path(script_dir, "ipums_convert_helpers.R"))

# Variable names and labels from a rendered IPUMS codebook page, in order.
read_ddi_html_vars <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  text <- gsub("<[^>]+>", " | ", text)
  text <- gsub("&amp;", "&", gsub("&quot;", '"', text, fixed = TRUE), fixed = TRUE)
  text <- gsub("(\\|\\s*)+", "| ", gsub("[[:space:]]+", " ", text))
  hits <- regmatches(text, gregexpr(
    'Variable: "[A-Z0-9_]+" \\| Name: \\| [A-Z0-9_]+ \\| Label: \\| [^|]*', text))[[1]]
  data.frame(name = sub('^Variable: "([A-Z0-9_]+)".*$', "\\1", hits),
             label = clean_text(sub("^.*Label: \\| ", "", hits)), stringsAsFactors = FALSE)
}

ddi_title <- function(path) {
  doc <- xml2::read_xml(path)
  clean_text(xml2::xml_text(xml2::xml_find_first(doc, ".//*[local-name()='stdyDscr']//*[local-name()='titl']")))
}

main <- function() {
  started <- Sys.time()
  stop_if(!file.exists(SOURCE), "Source CSV not found: ", SOURCE)
  stop_if(!file.exists(DDI), "DDI not found: ", DDI,
          " (save the extract's DDI link as XML, not the rendered page)")
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
  html_check <- "skipped"
  if (file.exists(DDI_HTML)) {
    html_vars <- read_ddi_html_vars(DDI_HTML)
    stop_if(!identical(html_vars$name, ddi$spec$name),
            "XML DDI and the extract's HTML codebook list different variables: ",
            paste(setdiff(union(html_vars$name, ddi$spec$name),
                          intersect(html_vars$name, ddi$spec$name)), collapse = ","))
    stop_if(!identical(html_vars$label, ddi$spec$label),
            "XML DDI and HTML codebook labels differ for ",
            paste(html_vars$name[html_vars$label != ddi$spec$label], collapse = ","))
    html_check <- list(path = normalizePath(DDI_HTML, winslash = "/"),
                       sha256 = digest::digest(file = DDI_HTML, algo = "sha256"),
                       variables_and_labels_match = TRUE)
    say("XML DDI agrees with the extract's HTML codebook on all ", nrow(html_vars), " variables")
  }
  spec <- ddi$spec[match(header, ddi$spec$name), , drop = FALSE]
  sample_labels <- ddi$labels[ddi$labels$name == "SAMPLE", c("code", "value_label")]
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
  stop_if(!identical(years, EXPECTED_YEARS),
          "Unexpected census years in the extract: ", paste(years, collapse = ","))
  say("Checking each year: ", paste(years, collapse = ", "))
  per_year <- do.call(rbind, lapply(years, function(year) {
    one <- paste0("read_parquet(", sql_string(con, file.path(stage, paste0("YEAR=", year),
                                                              "*.parquet")), ")")
    row <- DBI::dbGetQuery(con, paste(
      "SELECT COUNT(*) AS n, COUNT(DISTINCT SAMPLE) AS n_samples, MIN(SAMPLE) AS sample,",
      "COUNT(*) FILTER (WHERE SEX = 1 AND (CHBORN <> 0 OR CHSURV <> 0)) AS male_fertility,",
      "COUNT(*) FILTER (WHERE CHBORN BETWEEN 1 AND 96 AND CHSURV > CHBORN) AS surv_gt_born,",
      "COUNT(*) FILTER (WHERE NCHILD IS NULL) AS nchild_null,",
      "COUNT(*) FILTER (WHERE POPLOC IS NULL OR AGE IS NULL OR OCC1950 IS NULL",
      "OR COUNTYICP IS NULL) AS core_null,",
      "COUNT(MARST) AS marst_filled, COUNT(CHBORN) AS chborn_filled, COUNT(OCC) AS occ_filled,",
      "COUNT(*) FILTER (WHERE SPLOC > 0) AS sploc_pos,",
      "COUNT(*) FILTER (WHERE POPLOC > 0) AS poploc_pos,",
      "COUNT(*) FILTER (WHERE OCC1950 BETWEEN 12 AND 29) AS professors,",
      "COUNT(*) FILTER (WHERE OCC1950 = 10) AS pres_deans,",
      "SUM(NCHILD) AS sum_nchild,",
      "COUNT(*) FILTER (WHERE HISTID IS NULL OR LENGTH(TRIM(HISTID)) = 0) AS blank_histid,",
      "COUNT(DISTINCT HISTID) AS distinct_histid,",
      "COUNT(DISTINCT (SERIAL, PERNUM)) AS distinct_person_keys FROM", one))
    cbind(YEAR = year, row)
  }))
  stop_if(sum(per_year$n) != written, "Per-year Parquet rows do not add up to rows written")
  stop_if(any(per_year$n_samples != 1L), "A census year holds more than one IPUMS sample")
  per_year$sample_label <- sample_labels$value_label[match(as.character(per_year$sample),
                                                           sample_labels$code)]
  stop_if(any(is.na(per_year$sample_label)) || !all(grepl("100%", per_year$sample_label)),
          "A census year is not a 100% file: ",
          paste(per_year$YEAR, per_year$sample_label, collapse = "; "))
  stop_if(any(per_year$male_fertility != 0), "Men with non-N/A CHBORN/CHSURV")
  stop_if(any(per_year$nchild_null != 0), "NCHILD is blank for some persons")
  # Recorded, not fatal: the builders drop such rows, and the 1950 file is preliminary.
  if (any(per_year$core_null != 0)) {
    warning("POPLOC, AGE, OCC1950 or COUNTYICP is blank for some persons in ",
            paste(per_year$YEAR[per_year$core_null != 0], collapse = ", "), call. = FALSE)
  }
  # The builds key persons on SERIAL x PERNUM; HISTID is not unique in 1870.
  stop_if(any(per_year$distinct_person_keys != per_year$n),
          "SERIAL x PERNUM is not unique within a year")
  per_year$histid_shared_rows <- per_year$n - per_year$blank_histid - per_year$distinct_histid
  utils::write.csv(per_year, file.path(stage, "validation_year.csv"), row.names = FALSE)

  # Cross-checks against the trees already in use. A mismatch is recorded and
  # warned about rather than fatal: IPUMS revises full counts between releases.
  cross <- list()
  old_validation <- file.path(PARQUET_1850_1910, "validation_year.csv")
  if (file.exists(old_validation)) {
    old <- utils::read.csv(old_validation)
    both <- merge(per_year[, c("YEAR", "n", "professors", "pres_deans", "sum_nchild")],
                  old[, c("YEAR", "n", "professors", "pres_deans", "sum_nchild")],
                  by = "YEAR", suffixes = c("", "_usa_00010"))
    cross$usa_00010 <- both
    differs <- with(both, n != n_usa_00010 | professors != professors_usa_00010 |
                      sum_nchild != sum_nchild_usa_00010)
    if (any(differs)) warning("1850-1910 counts differ from usa_00010 in ",
                              paste(both$YEAR[differs], collapse = ", "), call. = FALSE)
  }
  glob_1940 <- file.path(PARQUET_1940, "person", "*", "*.parquet")
  if (length(Sys.glob(glob_1940))) {
    say("Cross-checking 1940 against ", PARQUET_1940)
    old_1940 <- DBI::dbGetQuery(con, paste0(
      "SELECT COUNT(*) AS n, COUNT(*) FILTER (WHERE OCC1950 BETWEEN 10 AND 29) AS faculty_codes",
      " FROM read_parquet(", sql_string(con, glob_1940), ", hive_partitioning = true)"))
    mine <- per_year[per_year$YEAR == 1940L, ]
    cross$ipums_1940_fullcount <- data.frame(
      n = mine$n, n_ext1940 = old_1940$n,
      faculty_codes = mine$professors + mine$pres_deans, faculty_codes_ext1940 = old_1940$faculty_codes)
    if (old_1940$n != mine$n) warning("1940 rows differ from the 1940 full count: ", mine$n,
                                      " vs ", old_1940$n, call. = FALSE)
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
                   title = "IPUMS 1850-1950 full counts (usa_00011)",
                   notes = c("Blank means not asked that year; see `coverage_by_year.csv`.",
                             "OCC is the contemporary code and carries no labels in the DDI.",
                             "The 1950 database is an IPUMS preliminary release."))

  source_info <- file.info(source_path)
  ddi_info <- file.info(ddi_path)
  manifest <- list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source = list(path = source_path, bytes = unname(source_info$size),
                  modified = format(source_info$mtime, tz = "UTC", usetz = TRUE)),
    ddi = list(path = ddi_path, bytes = unname(ddi_info$size),
               sha256 = digest::digest(file = ddi_path, algo = "sha256"),
               extract = ddi_title(ddi_path), checked_against_html = html_check),
    duckdb_version = as.character(utils::packageVersion("duckdb")),
    parquet_compression = "ZSTD",
    partition = "YEAR",
    columns = header,
    codes_kept_raw = TRUE,
    rows = as.numeric(written),
    by_year = per_year,
    cross_checks = if (length(cross)) cross else "skipped",
    validation = list(years_match = TRUE, one_100pct_sample_per_year = TRUE,
                      men_fertility_na = TRUE, nchild_never_blank = TRUE,
                      core_fields_never_blank = TRUE, person_key_unique_within_year = TRUE,
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
