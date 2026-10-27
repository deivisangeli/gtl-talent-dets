# Shared pieces of the IPUMS full-count converters in this folder:
#   convert_ipums_1910_fertility.R       (usa_00009, 1910)
#   convert_ipums_1850_1910_fertility.R  (usa_00010, 1850-1910)
# Both read a gzipped CSV whose header must agree with the extract's DDI.

say <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
sql_string <- function(con, value) as.character(DBI::dbQuoteString(con, value))
sql_ident <- function(con, value) as.character(DBI::dbQuoteIdentifier(con, value))
sql_path <- function(path) gsub("\\\\", "/", path)
clean_text <- function(value) gsub("[[:space:]]+", " ", trimws(value))

# OCC1950 labels the builds rely on. A swapped codebook or a reclassified
# extract fails here rather than silently changing who counts as a professor.
OCC_LABELS <- c(
  "010" = "College presidents and deans",
  "012" = "Agricultural sciences-Professors and instructors",
  "013" = "Biological sciences-Professors and instructors",
  "014" = "Chemistry-Professors and instructors",
  "015" = "Economics-Professors and instructors",
  "016" = "Engineering-Professors and instructors",
  "017" = "Geology and geophysics-Professors and instructors",
  "018" = "Mathematics-Professors and instructors",
  "019" = "Medical Sciences-Professors and instructors",
  "023" = "Physics-Professors and instructors",
  "024" = "Psychology-Professors and instructors",
  "025" = "Statistics-Professors and instructors",
  "026" = "Natural science (nec)-Professors and instructors",
  "027" = "Social sciences (nec)-Professors and instructors",
  "028" = "Non-scientific subjects-Professors and instructors",
  "029" = "Subject not specified-Professors and instructors")
# The offset convention: if IPUMS ever changes it, the decode is wrong.
FERTILITY_LABELS <- list(
  CHBORN = c("00" = "N/A", "01" = "No children", "02" = "1 child",
             "97" = "Unknown", "98" = "Illegible", "99" = "Missing"),
  CHSURV = c("00" = "N/A", "01" = "No children surviving", "02" = "1 child"))

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
      description = child_text(node, "txt"),
      ddi_type = xml2::xml_attr(format, "type"),
      ddi_decimals = as.integer(xml2::xml_attr(node, "dcml")),
      ddi_width = as.integer(xml2::xml_attr(location, "width")),
      stringsAsFactors = FALSE)
  }))
  # The weights carry two implied decimals, but the CSV already writes the point.
  spec$parquet_type <- ifelse(spec$ddi_type == "character", "VARCHAR",
                              ifelse(spec$ddi_decimals == 0L,
                                     ifelse(spec$ddi_width > 9L, "BIGINT", "INTEGER"),
                                     "DOUBLE"))
  stop_if(anyNA(spec$parquet_type), "DDI has an unsupported variable type")

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

check_labels <- function(labels, variable, expected) {
  actual <- labels[labels$name == variable, c("code", "value_label")]
  hit <- actual$value_label[match(names(expected), actual$code)]
  stop_if(anyNA(hit) || !identical(unname(hit), unname(expected)),
          variable, " labels do not match the expected codes: ",
          paste(names(expected)[is.na(hit) | hit != expected], collapse = ","))
}

# Header and DDI hold the same `expected_vars` names, the types the builds rely
# on, and the professor and fertility labels.
check_ddi <- function(ddi, header, expected_vars) {
  spec <- ddi$spec
  stop_if(length(header) != expected_vars || nrow(spec) != expected_vars,
          "Expected ", expected_vars, " CSV and DDI variables; found ",
          length(header), " and ", nrow(spec))
  stop_if(anyDuplicated(header) > 0L || anyDuplicated(spec$name) > 0L,
          "Duplicate CSV or DDI variable names")
  stop_if(!setequal(header, spec$name),
          "CSV and DDI variable names disagree: CSV-only=",
          paste(setdiff(header, spec$name), collapse = ","), "; DDI-only=",
          paste(setdiff(spec$name, header), collapse = ","))
  stop_if(!identical(spec$parquet_type[spec$name == "HISTID"], "VARCHAR") ||
            !all(spec$parquet_type[spec$name %in% c("HHWT", "PERWT")] == "DOUBLE"),
          "Unexpected DDI types for HISTID or the weights")
  check_labels(ddi$labels, "OCC1950", OCC_LABELS)
  for (variable in names(FERTILITY_LABELS)) {
    check_labels(ddi$labels, variable, FERTILITY_LABELS[[variable]])
  }
}

cast_expression <- function(con, name, type) {
  field <- paste0("s.", sql_ident(con, name))
  if (type == "VARCHAR") return(paste0(field, " AS ", sql_ident(con, name)))
  paste0("CAST(NULLIF(TRIM(", field, "), '') AS ", type, ") AS ",
         sql_ident(con, name))
}

# Non-blank count per column, as SELECT expressions.
coverage_expressions <- function(con, spec) {
  vapply(seq_len(nrow(spec)), function(i) {
    field <- sql_ident(con, spec$name[[i]])
    expr <- if (spec$parquet_type[[i]] == "VARCHAR") {
      paste0("COUNT(*) FILTER (WHERE ", field, " IS NOT NULL AND LENGTH(TRIM(",
             field, ")) > 0)")
    } else paste0("COUNT(", field, ")")
    paste0(expr, " AS ", field)
  }, character(1L))
}

write_dictionary <- function(root, ddi, header, coverage, rows, source, ddi_path,
                             title, notes = character()) {
  spec <- ddi$spec[match(header, ddi$spec$name), , drop = FALSE]
  spec$nonblank <- as.numeric(coverage[1L, spec$name])
  spec$fill_pct <- round(100 * spec$nonblank / rows, 3)
  utils::write.csv(spec, file.path(root, "variable_dictionary.csv"),
                   row.names = FALSE, na = "")
  utils::write.csv(ddi$labels, file.path(root, "value_labels.csv"),
                   row.names = FALSE, na = "")

  escape_md <- function(x) gsub("|", "\\\\|", x, fixed = TRUE)
  lines <- c(
    paste0("# ", title, ": variable dictionary"), "",
    paste0("Source: `", source, "`"),
    paste0("DDI: `", ddi_path, "`"), "",
    "Labels, descriptions and code tables come from the DDI. Codes are kept raw.",
    "CHBORN and CHSURV are offset by one: 00 = N/A, 01 = none, k+1 = k children;",
    "CHBORN 97/98/99 are unknown/illegible/missing.",
    notes,
    "The full category tables are in `value_labels.csv`.", "",
    paste0("Rows: ", format(rows, big.mark = ",", scientific = FALSE)), "",
    "## Variables", "")
  for (i in seq_len(nrow(spec))) {
    item <- spec[i, , drop = FALSE]
    lines <- c(lines, paste0("### `", item$name, "` — ", escape_md(item$label)), "",
               paste0("Class: `", item$parquet_type, "`; nonblank ",
                      sprintf("%.3f", item$fill_pct), "%."), "",
               item$description, "")
  }
  writeLines(lines, file.path(root, "dictionary.md"), useBytes = TRUE)
}

# Rename a verified staging directory into place. On Windows a just-closed
# DuckDB scan can briefly retain a file handle.
promote_staging <- function(stage, output) {
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
}
