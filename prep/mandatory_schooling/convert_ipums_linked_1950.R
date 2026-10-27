# Convert the IPUMS 1900-1950 linked extract to birthplace-partitioned Parquet.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/convert_ipums_linked_1950.R
#
# Override the defaults with IPUMS_LINKED_1950_SOURCE, IPUMS_LINKED_1950_DDI,
# and IPUMS_LINKED_1950_PARQUET_DIR. The output path must not already exist.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(xml2)
})

SOURCE <- Sys.getenv("IPUMS_LINKED_1950_SOURCE", unset = "D:/usa_00003.csv.gz")
DDI <- Sys.getenv("IPUMS_LINKED_1950_DDI", unset = "D:/usa_00003_ddi.xml")
OUTPUT <- Sys.getenv("IPUMS_LINKED_1950_PARQUET_DIR",
                     unset = "D:/ipums_linked_1900_1950")
SCI_CODES <- c(7L, 61L, 62L, 63L, 67L, 68L, 69L)
SCI_LABELS <- c("Chemists", "Agricultural scientists", "Biological scientists",
                "Geologists and geophysicists", "Mathematicians", "Physicists",
                "Misc. natural scientists")

say <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
sql_string <- function(con, value) as.character(DBI::dbQuoteString(con, value))
sql_ident <- function(con, value) as.character(DBI::dbQuoteIdentifier(con, value))
sql_path <- function(path) gsub("\\\\", "/", path)
clean_text <- function(value) gsub("[[:space:]]+", " ", trimws(value))

read_header <- function(path) {
  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  line <- readLines(connection, n = 1L, warn = FALSE)
  stop_if(length(line) != 1L, "CSV has no header: ", path)
  strsplit(gsub('"', "", line, fixed = TRUE), ",", fixed = TRUE)[[1L]]
}

read_ddi <- function(path) {
  doc <- xml2::read_xml(path)
  nodes <- xml2::xml_find_all(doc, ".//*[local-name()='dataDscr']/*[local-name()='var']")
  stop_if(length(nodes) == 0L, "DDI contains no variables: ", path)
  child_text <- function(node, tag) {
    child <- xml2::xml_find_first(node, paste0("./*[local-name()='", tag, "']"))
    if (inherits(child, "xml_missing")) return("")
    clean_text(xml2::xml_text(child))
  }
  spec <- do.call(rbind, lapply(seq_along(nodes), function(i) {
    node <- nodes[[i]]
    format <- xml2::xml_find_first(node, "./*[local-name()='varFormat']")
    location <- xml2::xml_find_first(node, "./*[local-name()='location']")
    data.frame(
      name = xml2::xml_attr(node, "name"),
      label = child_text(node, "labl"),
      concept = child_text(node, "concept"),
      description = child_text(node, "txt"),
      ddi_type = xml2::xml_attr(format, "type"),
      ddi_decimals = as.integer(xml2::xml_attr(node, "dcml")),
      ddi_width = as.integer(xml2::xml_attr(location, "width")),
      stringsAsFactors = FALSE)
  }))
  spec$parquet_type <- ifelse(spec$ddi_type == "character", "VARCHAR",
                              ifelse(spec$ddi_decimals == 0L, "INTEGER",
                                     ifelse(spec$ddi_decimals == 1L,
                                            "DECIMAL(6,1)", NA_character_)))
  stop_if(anyNA(spec$parquet_type), "DDI has an unsupported variable type or decimal count")

  labels <- do.call(rbind, lapply(seq_along(nodes), function(i) {
    categories <- xml2::xml_find_all(nodes[[i]], "./*[local-name()='catgry']")
    if (length(categories) == 0L) return(NULL)
    do.call(rbind, lapply(categories, function(category) {
      data.frame(
        name = spec$name[[i]],
        code = child_text(category, "catValu"),
        value_label = child_text(category, "labl"),
        stringsAsFactors = FALSE)
    }))
  }))
  list(spec = spec, labels = labels)
}

check_ddi <- function(ddi, header) {
  spec <- ddi$spec
  stop_if(length(header) != 60L || nrow(spec) != 60L,
          "Expected 60 CSV and DDI variables; found ", length(header), " and ", nrow(spec))
  stop_if(anyDuplicated(header) > 0L || anyDuplicated(spec$name) > 0L,
          "Duplicate CSV or DDI variable names")
  stop_if(!setequal(header, spec$name),
          "CSV and DDI variable names disagree: CSV-only=",
          paste(setdiff(header, spec$name), collapse = ","), "; DDI-only=",
          paste(setdiff(spec$name, header), collapse = ","))
  stop_if(sum(spec$parquet_type == "INTEGER") != 54L ||
            sum(spec$parquet_type == "DECIMAL(6,1)") != 3L ||
            sum(spec$parquet_type == "VARCHAR") != 3L,
          "The DDI type distribution is not the expected 54 integer / 3 decimal / 3 text")
  expected <- data.frame(code = sprintf("%03d", SCI_CODES),
                         value_label = SCI_LABELS, stringsAsFactors = FALSE)
  actual <- ddi$labels[ddi$labels$name == "OCC1950" &
                         ddi$labels$code %in% expected$code,
                       c("code", "value_label")]
  actual <- actual[match(expected$code, actual$code), , drop = FALSE]
  stop_if(nrow(actual) != nrow(expected) || anyNA(actual$code) ||
            !identical(actual$value_label, expected$value_label),
          "The seven scientist codes do not match the DDI labels")
}

cast_expression <- function(con, name, type) {
  field <- paste0("s.", sql_ident(con, name))
  if (type == "VARCHAR") return(paste0(field, " AS ", sql_ident(con, name)))
  paste0("CAST(NULLIF(TRIM(", field, "), '') AS ", type, ") AS ",
         sql_ident(con, name))
}

write_dictionary <- function(root, ddi, header, coverage, yearly, source, ddi_path) {
  spec <- ddi$spec[match(header, ddi$spec$name), , drop = FALSE]
  for (year in sort(unique(coverage$YEAR))) {
    row <- coverage[coverage$YEAR == year, , drop = FALSE]
    spec[[paste0("nonblank_", year)]] <- as.numeric(row[1L, spec$name])
    spec[[paste0("fill_pct_", year)]] <- round(
      100 * spec[[paste0("nonblank_", year)]] / as.numeric(row$n), 3)
  }
  derived <- spec[rep(1L, 2L), , drop = FALSE]
  derived[,] <- NA
  derived$name <- c("birth_bpl", "scientist_occ1950")
  derived$label <- c("1950-anchored birthplace partition",
                     "Scientist occupation, 1950 classification")
  derived$concept <- c("Derived geography", "Derived occupation")
  derived$description <- c(
    paste("1950 rows use their own BPL. Earlier rows use the BPL of their unique",
          "1950 HIK match. Earlier rows with no unique 1950 match use 'unknown'.",
          "The original row BPL is preserved."),
    paste("TRUE for OCC1950 codes 007, 061, 062, 063, 067, 068, or 069;",
          "FALSE for other nonblank codes; NULL for blank OCC1950."))
  derived$ddi_type <- "derived"
  derived$parquet_type <- c("VARCHAR", "BOOLEAN")
  spec <- rbind(spec, derived)
  utils::write.csv(spec, file.path(root, "variable_dictionary.csv"), row.names = FALSE,
                   na = "")
  extra_labels <- data.frame(name = c("birth_bpl", "scientist_occ1950",
                                      "scientist_occ1950"),
                             code = c("unknown", "TRUE", "FALSE"),
                             value_label = c("No unique 1950 HIK match for an earlier row",
                                             "OCC1950 is one of the seven scientist codes",
                                             "Other recorded OCC1950 code"),
                             stringsAsFactors = FALSE)
  utils::write.csv(rbind(ddi$labels, extra_labels),
                   file.path(root, "value_labels.csv"), row.names = FALSE, na = "")

  escape_md <- function(x) gsub("|", "\\\\|", x, fixed = TRUE)
  lines <- c(
    "# IPUMS linked 1900-1950 extract: variable dictionary", "",
    paste0("Source: `", source, "`"),
    paste0("DDI: `", ddi_path, "`"), "",
    "All variable labels, descriptions, classes, and categorical code labels come from the supplied DDI.",
    "The CSV already has decimal points for its three one-decimal fields; no implied-decimal scaling was applied.",
    "The complete category tables are in `value_labels.csv`; `variable_dictionary.csv` includes year-specific fill counts.",
    "", "## Rows by census year", "",
    "| Year | Rows |", "|---:|---:|")
  for (i in seq_len(nrow(yearly))) {
    lines <- c(lines, sprintf("| %d | %s |", yearly$YEAR[[i]],
                              format(yearly$n[[i]], big.mark = ",", scientific = FALSE)))
  }
  lines <- c(lines, "", "## Variables", "")
  for (i in seq_len(nrow(spec))) {
    item <- spec[i, , drop = FALSE]
    lines <- c(lines, paste0("### `", item$name, "` — ", escape_md(item$label)), "",
               paste0("Class: `", item$parquet_type, "`; group: ", item$concept, "."), "",
               item$description, "")
    if (item$name %in% header) {
      rates <- vapply(sort(unique(coverage$YEAR)), function(year) {
        value <- item[[paste0("fill_pct_", year)]]
        paste0(year, ": ", sprintf("%.3f", value), "%")
      }, character(1L))
      lines <- c(lines, paste("Nonblank by year:", paste(rates, collapse = "; "), "."), "")
      values <- ddi$labels[ddi$labels$name == item$name, , drop = FALSE]
      if (nrow(values) > 0L) {
        lines <- c(lines, sprintf("DDI category codes: %d; see `value_labels.csv`.",
                                  nrow(values)), "")
      }
    }
  }
  writeLines(lines, file.path(root, "dictionary.md"), useBytes = TRUE)
}

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
  stop_if(normalizePath(parent, winslash = "/", mustWork = TRUE) !=
            normalizePath(dirname(source), winslash = "/", mustWork = TRUE) &&
            normalizePath(parent, winslash = "/", mustWork = TRUE) !=
            normalizePath("D:/", winslash = "/", mustWork = TRUE),
          "Output must be under the intended D: data directory")
  suffix <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())
  stage <- paste0(output, ".staging-", suffix)
  work <- paste0(output, ".work-", suffix)
  stop_if(dir.exists(stage) || dir.exists(work), "Unexpected staging/work path collision")

  header <- read_header(source)
  ddi <- read_ddi(ddi_path)
  check_ddi(ddi, header)
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
  # The DDI has many birthplace codes. Keep their writers open to avoid
  # thousands of tiny files as rows from different states interleave.
  DBI::dbExecute(con, "SET partitioned_write_max_open_files = 2048")
  DBI::dbExecute(con, paste0("SET temp_directory = ",
                             sql_string(con, file.path(work, "spill"))))
  DBI::dbExecute(con, "SET preserve_insertion_order = false")
  csv <- paste0("read_csv(", sql_string(con, source),
                ", all_varchar = true, header = true)")
  bpl_cast <- "CAST(NULLIF(TRIM(s.BPL), '') AS INTEGER)"
  occ_cast <- "CAST(NULLIF(TRIM(s.OCC1950), '') AS INTEGER)"
  sci_test <- paste0(occ_cast, " IN (", paste(SCI_CODES, collapse = ","), ")")

  say("Building unique 1950 HIK-to-BPL lookup")
  anchor_sql <- paste0(
    "CREATE TABLE anchor AS WITH rows AS (",
    "SELECT NULLIF(TRIM(HIK), '') AS HIK, ",
    "CAST(NULLIF(TRIM(BPL), '') AS INTEGER) AS BPL ",
    "FROM ", csv, " WHERE YEAR = '1950') ",
    "SELECT HIK, MIN(BPL) AS BPL FROM rows WHERE HIK IS NOT NULL ",
    "GROUP BY HIK HAVING COUNT(*) = 1 AND COUNT(BPL) = 1")
  DBI::dbExecute(con, anchor_sql)
  anchor_n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM anchor")$n[[1L]]
  say("Unique 1950 anchors: ", format(anchor_n, big.mark = ",", scientific = FALSE))

  select_fields <- paste(mapply(cast_expression, list(con), spec$name,
                                spec$parquet_type, SIMPLIFY = TRUE), collapse = ", ")
  partition_rule <- paste0(
    "CASE WHEN CAST(s.YEAR AS INTEGER) = 1950 THEN ", bpl_cast,
    " ELSE a.BPL END")
  partition_value <- paste0("CASE WHEN (", partition_rule,
                            ") IS NULL THEN 'unknown' ELSE ",
                            "LPAD(CAST((", partition_rule,
                            ") AS VARCHAR), 3, '0') END")
  scientist_value <- paste0("CASE WHEN ", occ_cast,
                            " IS NULL THEN NULL ELSE ", sci_test, " END")
  copy_sql <- paste0(
    "COPY (SELECT ", select_fields, ", ", partition_value,
    " AS birth_bpl, ", scientist_value,
    " AS scientist_occ1950 FROM ", csv,
    " s LEFT JOIN anchor a ON NULLIF(TRIM(s.HIK), '') = a.HIK) TO ",
    sql_string(con, stage),
    " (FORMAT PARQUET, COMPRESSION ZSTD, PARTITION_BY (birth_bpl), ",
    "FILENAME_PATTERN 'part_{i}')")
  say("Writing birthplace-partitioned Parquet")
  DBI::dbExecute(con, copy_sql)
  files <- list.files(stage, pattern = "\\.parquet$", recursive = TRUE,
                      full.names = TRUE)
  stop_if(length(files) == 0L, "DuckDB wrote no Parquet files")
  parquet <- paste0("read_parquet(",
                    sql_string(con, file.path(stage, "birth_bpl=*", "*.parquet")),
                    ", hive_partitioning = true)")
  observed_names <- names(DBI::dbGetQuery(con,
                           paste0("SELECT * FROM ", parquet, " LIMIT 0")))
  stop_if(!setequal(c(header, "birth_bpl", "scientist_occ1950"),
                    setdiff(observed_names, "filename")),
          "Parquet schema does not have all source fields and the two derived fields")

  say("Checking source and Parquet counts by year and original BPL")
  source_counts <- DBI::dbGetQuery(con, paste0(
    "SELECT CAST(YEAR AS INTEGER) AS YEAR, ",
    "CAST(NULLIF(TRIM(BPL), '') AS INTEGER) AS BPL, COUNT(*) AS n, ",
    "COUNT(*) FILTER (WHERE CAST(NULLIF(TRIM(OCC1950), '') AS INTEGER) IN (",
    paste(SCI_CODES, collapse = ","), ")) AS scientist_n ",
    "FROM ", csv, " GROUP BY 1, 2 ORDER BY 1, 2"))
  output_counts <- DBI::dbGetQuery(con, paste0(
    "SELECT YEAR, BPL, COUNT(*) AS n, ",
    "COUNT(*) FILTER (WHERE scientist_occ1950) AS scientist_n ",
    "FROM ", parquet, " GROUP BY 1, 2 ORDER BY 1, 2"))
  counts <- merge(source_counts, output_counts, by = c("YEAR", "BPL"),
                  all = TRUE, suffixes = c("_source", "_parquet"))
  stop_if(anyNA(counts$n_source) || anyNA(counts$n_parquet) ||
            anyNA(counts$scientist_n_source) ||
            anyNA(counts$scientist_n_parquet) ||
            any(counts$n_source != counts$n_parquet) ||
            any(counts$scientist_n_source != counts$scientist_n_parquet),
          "Source and Parquet counts or scientist counts disagree by year/BPL")
  yearly <- aggregate(as.numeric(source_counts$n),
                      by = list(YEAR = source_counts$YEAR), FUN = sum)
  names(yearly)[[2L]] <- "n"
  stop_if(!identical(sort(yearly$YEAR), c(1900L, 1910L, 1920L,
                                         1930L, 1940L, 1950L)),
          "Unexpected census years in the extract")

  say("Checking every output row against the 1950 birthplace rule")
  check_sql <- paste0(
    "SELECT COUNT(*) AS n, ",
    "COUNT(*) FILTER (WHERE p.birth_bpl <> ",
    "CASE WHEN p.YEAR = 1950 THEN ",
    "CASE WHEN p.BPL IS NULL THEN 'unknown' ELSE LPAD(CAST(p.BPL AS VARCHAR), 3, '0') END ",
    "WHEN a.BPL IS NULL THEN 'unknown' ",
    "ELSE LPAD(CAST(a.BPL AS VARCHAR), 3, '0') END) AS partition_mismatch, ",
    "COUNT(*) FILTER (WHERE p.YEAR < 1950 AND a.BPL IS NULL) AS unmatched_pre1950, ",
    "COUNT(*) FILTER (WHERE p.YEAR = 1950 AND ",
    "(p.HIK IS NULL OR LENGTH(TRIM(p.HIK)) = 0)) AS blank_hik_1950 ",
    "FROM ", parquet, " p LEFT JOIN anchor a ON NULLIF(TRIM(p.HIK), '') = a.HIK")
  partition_check <- DBI::dbGetQuery(con, check_sql)
  stop_if(partition_check$partition_mismatch[[1L]] != 0 ||
            partition_check$n[[1L]] != sum(yearly$n),
          "Birthplace partition check failed")

  say("Profiling year-specific variable coverage and writing dictionary")
  coverage_fields <- vapply(seq_len(nrow(spec)), function(i) {
    field <- sql_ident(con, spec$name[[i]])
    expr <- if (spec$parquet_type[[i]] == "VARCHAR") {
      paste0("COUNT(*) FILTER (WHERE ", field,
             " IS NOT NULL AND LENGTH(TRIM(", field, ")) > 0)")
    } else paste0("COUNT(", field, ")")
    paste0(expr, " AS ", field)
  }, character(1L))
  coverage <- DBI::dbGetQuery(con, paste0(
    "SELECT YEAR, COUNT(*) AS n, ", paste(coverage_fields, collapse = ", "),
    " FROM ", parquet, " GROUP BY YEAR ORDER BY YEAR"))
  write_dictionary(stage, ddi, header, coverage, yearly, source, ddi_path)
  utils::write.csv(counts, file.path(stage, "validation_year_bpl.csv"),
                   row.names = FALSE)

  partition_counts <- DBI::dbGetQuery(con, paste0(
    "SELECT birth_bpl, COUNT(*) AS n FROM ", parquet,
    " GROUP BY birth_bpl ORDER BY birth_bpl"))
  source_info <- file.info(source)
  ddi_info <- file.info(ddi_path)
  manifest <- list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source = list(path = source, bytes = unname(source_info$size),
                  modified = format(source_info$mtime, tz = "UTC", usetz = TRUE)),
    ddi = list(path = ddi_path, bytes = unname(ddi_info$size),
               sha256 = digest::digest(file = ddi_path, algo = "sha256")),
    duckdb_version = as.character(utils::packageVersion("duckdb")),
    threads = 12L, memory_limit = "20GB", parquet_compression = "ZSTD",
    source_columns = header,
    output_columns = c(header, "birth_bpl", "scientist_occ1950"),
    schema_file = "variable_dictionary.csv",
    rows = as.numeric(partition_check$n[[1L]]),
    unique_1950_anchors = as.numeric(anchor_n),
    unmatched_pre1950_rows = as.numeric(partition_check$unmatched_pre1950[[1L]]),
    blank_hik_1950_rows = as.numeric(partition_check$blank_hik_1950[[1L]]),
    rows_by_year = yearly,
    rows_by_birth_bpl = partition_counts,
    scientist_codes = sprintf("%03d", SCI_CODES),
    validation = list(source_parquet_counts_match = TRUE,
                      scientist_counts_match = TRUE,
                      partition_mismatches = 0L,
                      ddi_columns_and_scientist_labels_match = TRUE),
    elapsed_minutes = round(as.numeric(difftime(Sys.time(), started,
                                                units = "mins")), 2))
  jsonlite::write_json(manifest, file.path(stage, "_manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE, na = "null")

  DBI::dbDisconnect(con, shutdown = TRUE)
  connected <- FALSE
  rm(con)
  gc(verbose = FALSE)
  # On Windows a just-closed DuckDB scan can briefly retain a file handle.
  promoted <- FALSE
  for (attempt in seq_len(30L)) {
    stop_if(file.exists(output) || dir.exists(output),
            "Final output appeared during conversion: ", output)
    promoted <- isTRUE(suppressWarnings(file.rename(stage, output)))
    if (promoted) break
    Sys.sleep(2)
  }
  stop_if(!promoted, "Could not promote verified staging directory after 60 seconds: ",
          stage, " -> ", output)
  unlink(work, recursive = TRUE, force = TRUE)
  say("Complete: ", format(manifest$rows, big.mark = ",", scientific = FALSE),
      " rows in ", length(partition_counts$birth_bpl), " birthplace partitions at ", output)
}

main()
