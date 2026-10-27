suppressPackageStartupMessages({library(data.table); library(jsonlite)})
invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(2L, 3L))
repo_root <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
stage <- normalizePath(args[2], winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "prep/amws/amws_1986/ocr_supplement_helpers.R"))
h <- load_amws_ocr_helpers(repo_root)
if (length(args) == 3L && args[3] == "patterns") {
  keys <- c("SAFE_EMBEDDED_PATTERNS", "NOISE_RX", "ENTRY_START_BIRTH",
    "ENTRY_START_SEE_PREVIOUS", "ENTRY_START_NO_COMMA_BIRTH", "ENTRY_START_NO_BIRTH_FIELD",
    "CONTINUATION_START", "DAMAGED_SEE_PREVIOUS", "DAMAGED_WITH_MONTH_AND_EDUC",
    "DAMAGED_WITH_MONTH", "DAMAGED_GLUE_BIRTH_MONTH", "DAMAGED_WITH_FIELD_AND_EDUC",
    "DAMAGED_POUND_EDUC", "DASH_PROF_DAMAGED_START", "ISOLATED_DAMAGED_NAME")
  write_json(mget(keys, h), file.path(stage, "segmentation_patterns.json"), auto_unbox = TRUE)
  quit(save = "no")
}
if (file.exists(file.path(stage, "segmented_entries.csv"))) {
  d <- as.data.frame(fread(file.path(stage, "segmented_entries.csv"), colClasses = "character", na.strings = NULL))
} else {
blocks <- fread(file.path(stage, "blocks.csv"), colClasses = "character", na.strings = NULL)
entries <- list(); assignments <- list(); current <- NULL
flush <- function() {
  if (is.null(current)) return()
  entries[[length(entries) + 1L]] <<- current
}
previous_doc <- ""; previous_printed <- NA_integer_
for (i in seq_len(nrow(blocks))) {
  row <- blocks[i]
  if (row$block_status != "body") next
  pieces <- h$split_safe_embedded_entries(h$normalize_text(row$text))
  for (piece_no in seq_along(pieces)) {
    text <- pieces[[piece_no]]
    # OCR commonly prints the separator after the surname as a period.
    adjusted <- sub("^([A-Z][A-Z'’ -]{1,65})\\.\\s+", "\\1, ", text, perl = TRUE)
    cls <- h$classify_piece(adjusted)
    # A separate source PDF joins only when it is the immediately adjacent range.
    if (row$source_file != previous_doc && nzchar(previous_doc) &&
        (strsplit(row$source_file, "_")[[1]][2] != strsplit(previous_doc, "_")[[1]][2] ||
         is.na(previous_printed) || as.integer(row$printed_page) != previous_printed + 1L)) {
      flush(); current <- NULL
    }
    starts <- cls$starts_entry[[1]]
    if (starts || is.null(current)) {
      flush()
      key <- paste0(sub("\\.pdf$", "", row$source_file), ":", row$pdf_page,
                    ":", row$block_index, ":", piece_no)
      current <- list(source_key = key, doc_id = paste0("amws", sub("\\.pdf$", "", row$source_file)),
                      source_file = row$source_file, pdf_page = row$pdf_page,
                      block_index = row$block_index, piece_no = piece_no,
                      segmentation_rule = if (starts) cls$rule[[1]] else "orphan_continuation",
                      raw_text_adjusted = adjusted, raw_text = text)
    } else {
      current$raw_text_adjusted <- paste(current$raw_text_adjusted, adjusted)
      current$raw_text <- paste(current$raw_text, text)
    }
    assignments[[length(assignments) + 1L]] <- list(block_key = row$block_key,
      piece_no = piece_no, source_key = current$source_key, text = text)
    previous_doc <- row$source_file; previous_printed <- as.integer(row$printed_page)
  }
  if (i %% 2000L == 0L) cat("Segmented blocks", i, "\n")
}
flush()
d <- as.data.frame(rbindlist(entries, fill = TRUE), stringsAsFactors = FALSE)
fwrite(rbindlist(assignments), file.path(stage, "block_assignments.csv"))
}
d$lineid <- as.character(ave(seq_len(nrow(d)), d$doc_id, FUN = seq_along))
d$entry_instance <- "1"
cat("Biographies/fragments:", nrow(d), "\n")
master <- d
unchanged <- NULL
previous_file <- file.path(stage, "entries.csv")
if (file.exists(previous_file)) {
  previous <- as.data.frame(fread(previous_file, colClasses = "character", na.strings = NULL))
  matches <- match(paste(d$source_key, d$raw_text_adjusted), paste(previous$source_key, previous$raw_text_adjusted))
  reusable <- !is.na(matches)
  # Field overrides deliberately invalidate the corresponding cached result.
  override_file <- file.path(stage, "manual_overrides.json")
  if (file.exists(override_file)) {
    overrides <- fromJSON(override_file, simplifyVector = FALSE)
    for (override in overrides) {
      if (override$manual_action != "correct") next
      row <- match(override$source_key, d$source_key)
      if (is.na(row) || is.na(matches[row])) next
      values <- override$values
      if (any(vapply(names(values), function(col) previous[[col]][matches[row]] != values[[col]], logical(1)))) reusable[row] <- FALSE
    }
  }
  unchanged <- previous[matches[reusable], , drop = FALSE]
  for (col in names(master)) unchanged[[col]] <- master[[col]][reusable]
  d <- d[!reusable, , drop = FALSE]
  cat("Refreshing", nrow(d), "entries; retaining", nrow(unchanged), "unchanged parsed/geocoded rows\n")
  if (!nrow(d)) {
    fwrite(unchanged, previous_file, na = "")
    quit(save = "no")
  }
}
workers <- as.integer(Sys.getenv("AMWS_OCR_PARSE_WORKERS", "4"))
cache_file <- file.path(stage, "parse_cache.csv")
cache_idx <- rep(NA_integer_, nrow(d))
cache_cols <- c("name_raw", "birth_place", "birth_date", "field", "regex_birth_rule_id", "regex_field_rule_id", "regex_parse_flag")
if (file.exists(cache_file)) {
  cache <- fread(cache_file, colClasses = "character", na.strings = NULL)
  cache_idx <- match(paste(d$source_key, d$raw_text_adjusted), paste(cache$source_key, cache$raw_text_adjusted))
}
needed <- which(is.na(cache_idx))
cat("Parsing", length(needed), "new/changed entries; reusing", sum(!is.na(cache_idx)), "cached parses\n")
parsed_rows <- as.data.frame(setNames(replicate(length(cache_cols), rep("", nrow(d)), simplify = FALSE), cache_cols))
if (any(!is.na(cache_idx))) parsed_rows[!is.na(cache_idx), ] <- as.data.frame(cache[cache_idx[!is.na(cache_idx)], ..cache_cols])
if (length(needed)) {
cluster <- parallel::makeCluster(workers)
parallel::clusterExport(cluster, "repo_root", envir = environment())
parallel::clusterEvalQ(cluster, {
  invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
  source(file.path(repo_root, "prep/amws/amws_1986/ocr_supplement_helpers.R"))
  parser <- load_amws_ocr_helpers(repo_root)
  NULL
})
parsed <- parallel::parLapply(cluster, split(d$raw_text_adjusted[needed], ceiling(seq_along(needed)/500L)), function(texts) {
  dplyr::bind_rows(lapply(texts, function(text) parser$parse_entry_regex(text)))
})
parallel::stopCluster(cluster)
parsed_rows[needed, ] <- as.data.frame(dplyr::bind_rows(parsed))
}
fwrite(cbind(d[c("source_key", "raw_text_adjusted")], parsed_rows), cache_file)
d <- cbind(d, parsed_rows)
cat("Parsed all entries\n")
cluster <- parallel::makeCluster(workers)
parallel::clusterExport(cluster, "repo_root", envir = environment())
parallel::clusterEvalQ(cluster, {
  invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
  source(file.path(repo_root, "prep/amws/amws_1986/ocr_supplement_helpers.R"))
  parser <- load_amws_ocr_helpers(repo_root)
  NULL
})
normalized <- parallel::parLapply(cluster, split(d, ceiling(seq_len(nrow(d))/500L)), function(chunk) {
  chunk <- as.data.frame(parser$add_birth_year_city(chunk))
  chunk$birth_year <- parser$blank_na(chunk$birth_year)
  components <- dplyr::bind_rows(lapply(chunk$birth_place, function(p) parser$parse_place_components(parser$trim_birth_place(p)$place)))
  chunk$birth_city <- components$birth_city; chunk$birth_state <- components$birth_state
  chunk$birth_country <- components$birth_country
  chunk
})
parallel::stopCluster(cluster)
d <- as.data.frame(rbindlist(normalized))
fwrite(d, file.path(stage, "normalized_checkpoint.csv"), na = "")
cat("Normalized birthplace components\n")
for (col in c("geo_lat", "geo_lon", "geo_geoid", "geo_county_name", "geo_matched_name",
              "geo_match_source", "geo_jw", "geo_cleaning_jw")) d[[col]] <- ""
d$birth_location_format_problem <- "FALSE"
corrections <- h$apply_regex_corrections(d)
for (col in c("birth_place", "birth_city", "birth_state", "birth_country", "birth_year")) {
  d[[col]][corrections$row_id] <- corrections[[paste0("new_", col)]]
}
fwrite(corrections, file.path(stage, "regex_corrections.csv"))
cat("Applied", nrow(corrections), "regex corrections\n")
# Manual decisions are source-keyed and survive deterministic reruns.
override_file <- file.path(stage, "manual_overrides.json")
if (file.exists(override_file)) {
  decisions <- fromJSON(override_file, simplifyVector = FALSE)
  stopifnot(!anyDuplicated(vapply(decisions, `[[`, "", "source_key")))
  for (decision in decisions) {
    idx <- match(decision$source_key, d$source_key)
    if (is.na(idx)) next
    if (!decision$manual_action %in% c("correct", "no_change", "review_unclear", "exclude")) stop("Invalid manual action")
    if (decision$manual_action == "correct") {
      stopifnot(decision$manual_confidence %in% c("high", "medium"), nzchar(decision$evidence))
      for (col in names(decision$values)) d[[col]][idx] <- decision$values[[col]]
    }
  }
}
fmt <- h$birth_location_format_reasons(d$birth_city, d$birth_state, d$birth_year)
d$birth_location_format_problem <- ifelse(nzchar(fmt), "TRUE", "FALSE")
d$birth_location_format_problem_reason <- fmt
cat("Geocoding new entries only...\n")
geo <- h$geocode_us_rows(d, file.path(DATA_INPUT, "2024_Gaz_place_national.txt"),
  file.path(DATA_INPUT, "geonames_US.txt"),
  file.path(Sys.getenv("LOCALAPPDATA"), "tigris/tigris/Cache/cb_2020_us_county_20m.shp"))
if (nrow(geo)) {
  idx <- match(paste(geo$doc_id, geo$lineid), paste(d$doc_id, d$lineid))
  good <- !is.na(geo$geoid) & nzchar(geo$geoid); idx <- idx[good]; geo <- geo[good, ]
  for (pair in list(c("geo_lat", "lat"), c("geo_lon", "lon"), c("geo_geoid", "geoid"),
                   c("geo_county_name", "county_name"), c("geo_matched_name", "matched_name"),
                   c("geo_match_source", "match_source"), c("geo_jw", "jw"), c("geo_cleaning_jw", "cleaning_jw"))) {
    d[[pair[1]]][idx] <- as.character(geo[[pair[2]]])
  }
}
d$is_us_birth <- ifelse(d$birth_country == "USA", "TRUE", "FALSE")
# Explicitly reviewed geographic overrides take precedence over computed matches.
if (file.exists(override_file)) for (decision in decisions) {
  idx <- match(decision$source_key, d$source_key)
  if (is.na(idx) || decision$manual_action != "correct") next
  for (col in intersect(names(decision$values), grep("^geo_", names(d), value=TRUE)))
    d[[col]][idx] <- decision$values[[col]]
}
d$is_us_geocoded <- ifelse(d$birth_country == "USA" & nzchar(d$geo_geoid), "TRUE", "FALSE")
d$geocoding_status <- ifelse(d$birth_country != "USA", "not_usa",
  ifelse(nzchar(d$geo_geoid), "geocoded", "no_geocoder_match"))
fwrite(d, file.path(stage, "geocoded_checkpoint.csv"), na = "")
d$review_reasons <- vapply(seq_len(nrow(d)), function(i) paste(c(
  if (!nzchar(d$name_raw[i]) || !grepl("^[A-Z][A-Z'’ .-]+, [A-Z]", d$name_raw[i])) "name_uncertain",
  if (grepl("damaged|orphan", d$segmentation_rule[i])) "segmentation_uncertain",
  if (!nzchar(d$birth_year[i]) && grepl("\\bb\\s", d$raw_text_adjusted[i])) "birth_year_missing",
  if (nzchar(fmt[i])) fmt[i],
  if (d$birth_country[i] == "USA" && !nzchar(d$geo_geoid[i])) "us_unmatched",
  if (grepl("fuzzy", d$geo_match_source[i])) "fuzzy_geocode",
  if (!nzchar(d$birth_country[i]) && nzchar(d$birth_place[i])) "country_unresolved"
), collapse = ";"), "")
fwrite(d, file.path(stage, "entries.csv"), na = "")
if (!is.null(unchanged)) {
  combined <- rbindlist(list(unchanged, d), use.names = TRUE, fill = TRUE)
  combined <- combined[match(master$source_key, combined$source_key)]
  fwrite(combined, file.path(stage, "entries.csv"), na = "")
}
cat("Wrote", nrow(d), "entries;", sum(nzchar(d$review_reasons)), "flagged\n")
