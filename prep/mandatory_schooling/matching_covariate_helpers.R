# Pure construction helpers: all inputs and outputs are passed explicitly.
matching_covariates <- c("manufacturing_intensity", "agricultural_intensity",
                        "illiteracy", "foreign_born_share")

matching_strict_sum <- function(x, cols) {
  stopifnot(length(cols) > 0, all(cols %in% names(x)))
  m <- as.matrix(x[, ..cols])
  z <- rowSums(m, na.rm = FALSE)
  z[apply(m, 1L, function(r) any(!is.finite(r) | r < 0))] <- NA_real_
  z
}

matching_ratio <- function(num, den, share = FALSE) {
  z <- num / den
  z[!is.finite(num) | !is.finite(den) | num < 0 | den <= 0 |
      !is.finite(z) | (share & num > den)] <- NA_real_
  z
}

matching_read_nhgis <- function(path) {
  x <- data.table::fread(path, colClasses = "character", encoding = "Latin-1")
  # csv_header includes a second, descriptive header row.
  x <- x[grepl("^G[0-9]+$", GISJOIN)]
  cols <- grep("^[A-Z0-9]{3}[0-9]{3}$", names(x), value = TRUE)
  x[, (cols) := lapply(.SD, function(v) suppressWarnings(as.numeric(v))), .SDcols = cols]
  x
}

build_matching_covariates <- function(data_root) {
  raw <- file.path(data_root, "raw", "mandatory_schooling_matching")
  files <- list.files(file.path(raw, "csv"), recursive = TRUE, full.names = TRUE)
  find_input <- function(pattern) {
    hit <- files[grepl(pattern, basename(files))]
    if (length(hit) != 1L) stop("Expected one downloaded input: ", pattern)
    hit
  }
  crosswalk_path <- file.path(data_root, "input", "nhgis0001_ts_nominal_county.csv")
  cw <- data.table::fread(crosswalk_path, colClasses = "character")
  cw <- unique(cw[, .(GISJOIN, GEOID = paste0(STATEFP, COUNTYFP))])
  cw <- cw[grepl("^[0-9]{5}$", GEOID)]
  if (anyDuplicated(cw$GISJOIN)) stop("Ambiguous GISJOIN crosswalk")
  panel <- list(); audit <- list(); manifest <- list(); cmf_audit <- list()
  for (yr in c(1850L, 1860L)) {
    path <- find_input(if (yr == 1850L) "ds10_1850_county[.]csv$" else "ds14_1860_county[.]csv$")
    x <- matching_read_nhgis(path)
    x <- merge(x, cw, by = "GISJOIN", all.x = TRUE, sort = FALSE)
    x[, duplicate_geoid := !is.na(GEOID) & (duplicated(GEOID) | duplicated(GEOID, fromLast = TRUE))]
    audit[[as.character(yr)]] <- x[, .(year = yr, GISJOIN, GEOID, STATE, COUNTY,
      geography_status = data.table::fcase(is.na(GEOID), "unmatched_nominal_crosswalk",
        duplicate_geoid, "ambiguous_nominal_crosswalk", default = "nominal_identifier_match"))]
    x <- x[!is.na(GEOID) & !duplicate_geoid]
    pop <- if (yr == 1850L) x$ADQ001 else x$AG3001
    acres <- if (yr == 1850L) x$AED001 else x$AG4001
    free <- if (yr == 1850L) matching_strict_sum(x, c("AE6001", "AE6002")) else x$AH7001
    foreign <- if (yr == 1850L) x$AEM002 else x$AH6002
    # 1850 reports adult literacy for the free population. Its published
    # adult age bins begin at 20. Exclude unknown ages, retaining their count
    # in the audit rather than treating them as adults or as children.
    adult_cols <- sprintf("AEL%03d", c(6:14, 21:29, 36:44, 51:59))
    adults <- if (yr == 1850L) matching_strict_sum(x, adult_cols) else rep(NA_real_, nrow(x))
    illit <- if (yr == 1850L) x$AEC001 else rep(NA_real_, nrow(x))
    unknown_age <- if (yr == 1850L) matching_strict_sum(x, sprintf("AEL%03d", c(15,30,45,60))) else rep(NA_real_, nrow(x))
    literacy <- matching_ratio(illit, adults, TRUE)
    # Exact age-universe support is required: no denominator approximation
    # for counties with unclassified ages in the free population.
    literacy[is.na(unknown_age) | unknown_age != 0] <- NA_real_
    p <- data.table::data.table(GEOID = x$GEOID, GISJOIN = x$GISJOIN, year = yr,
      state = x$STATE, county = x$COUNTY, population = pop, improved_acres = acres,
      free_population = free, foreign_born_persons = foreign,
      illiterate_free_adults = illit, free_adults_20plus = adults,
      free_population_unknown_age = unknown_age,
      agricultural_intensity = log1p(matching_ratio(acres, pop)),
      illiteracy = literacy, foreign_born_share = matching_ratio(foreign, free, TRUE),
      source_file = path, crosswalk_file = crosswalk_path,
      geography_method = "NHGIS nominal GISJOIN-FIPS; not constant-boundary harmonization")
    if (yr == 1850L) {
      mp <- file.path(raw, "cmf", "1850_ci", "mfg_ind_1850broad.csv")
      m <- data.table::fread(mp)
      cmf_audit[[as.character(yr)]] <- unique(m[, .(year = yr, source_fips = fips,
        geography_status = ifelse(is.finite(fips) & fips == floor(fips), "integer_source_fips", "excluded_historical_code"))])
      m <- m[is.finite(fips) & fips == floor(fips)]
      m <- m[ind_broadest != "not mfg"]
      m[, GEOID := sprintf("%05d", as.integer(fips))]
      m <- m[, .(manufacturing_output = if (all(is.finite(mfgout) & mfgout >= 0)) as.numeric(sum(mfgout)) else NA_real_), by = GEOID]
      construction <- "CMF 1850 broad industries excluding not mfg; source zero-imputes unreadable digits; GA/LA unavailable"
    } else {
      mp <- file.path(raw, "cmf", "ci_data", "mfg_ind_1860broad.csv")
      m <- data.table::fread(mp)
      cmf_audit[[as.character(yr)]] <- unique(m[, .(year = yr, source_fips = fips,
        geography_status = ifelse(is.finite(fips) & fips == floor(fips), "integer_source_fips", "excluded_historical_code"))])
      m <- m[is.finite(fips) & fips == floor(fips)]
      # Broad table contains manufacturing categories and an overall TOTAL.
      # Only explicitly manufacturing rows, or structural-zero broad rows,
      # enter the sum. TOTAL may include non-manufacturing activity.
      m <- m[industry != "TOTAL" & !is.na(fips)]
      if (any(is.na(m$not_mfg) & is.finite(m$mfgout) & m$mfgout != 0))
        stop("Unclassified nonzero 1860 industry output")
      m <- m[not_mfg == 0 | is.na(not_mfg)]
      m[, GEOID := sprintf("%05d", as.integer(fips))]
      if (anyDuplicated(m[, .(GEOID, industry)])) stop("Ambiguous CMF county-industry mapping")
      m <- m[, .(manufacturing_output = if (all(is.finite(mfgout) & mfgout >= 0)) as.numeric(sum(mfgout)) else NA_real_), by = GEOID]
      construction <- "CMF 1860 broad manufacturing categories; excludes TOTAL, mining and fisheries"
    }
    p <- merge(p, m, by = "GEOID", all.x = TRUE, sort = FALSE)
    p[, `:=`(manufacturing_intensity = log1p(matching_ratio(manufacturing_output, population)),
      manufacturing_source_file = mp, manufacturing_construction = construction)]
    panel[[as.character(yr)]] <- p
    manifest[[as.character(yr)]] <- data.table::data.table(year = yr,
      variable = matching_covariates,
      available = c(TRUE, TRUE, yr == 1850L, TRUE),
      numerator = c("CMF manufacturing output", if (yr == 1850L) "AED001" else "AG4001",
        if (yr == 1850L) "AEC001" else "unavailable", if (yr == 1850L) "AEM002" else "AH6002"),
      denominator = c(if (yr == 1850L) "ADQ001" else "AG3001", if (yr == 1850L) "ADQ001" else "AG3001",
        if (yr == 1850L) paste(adult_cols, collapse = "+") else "unavailable",
        if (yr == 1850L) "AE6001+AE6002" else "AH7001"),
      universe = c("Dollars per resident, same census year", "Improved acres per resident, same census year",
        "Free adults, published age bins 20+; require zero unknown ages", "Free population, all ages"),
      transform = c("log1p", "log1p", "share", "share"),
      source_file = c(mp, path, path, path))
  }
  out <- data.table::rbindlist(panel, fill = TRUE)
  stopifnot(!anyDuplicated(out[, .(GEOID, year)]))
  list(panel = out, geography_audit = data.table::rbindlist(audit),
       cmf_geography_audit = data.table::rbindlist(cmf_audit), manifest = data.table::rbindlist(manifest))
}
