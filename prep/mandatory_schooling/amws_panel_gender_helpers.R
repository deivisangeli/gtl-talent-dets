# Gender-specific outcomes use saved flags, never new name inference.
amws_gender_groups <- c('male', 'female', 'unclassified')
amws_gender_suffix <- function(g) paste0('_', g, '_80')
amws_gender_columns <- function(x) grep('_(male|female|unclassified)_80$|^gender_inference_threshold$', x, value = TRUE)
amws_gender_period_cols <- function() as.vector(outer(
  c('n_amws_1906_1955_dedup', 'n_amws_1986', 'n_amws'), amws_gender_suffix(amws_gender_groups), paste0))
amws_gender_annual_bases <- c('n_early_year', 'n_1986_year', 'n_early_assigned', 'n_1986_assigned',
                            'n_early_unassigned', 'n_1986_unassigned', 'n_unassigned_month')

amws_classify_flags <- function(male, female) {
  if (length(male) != length(female) || any(!is.na(male) & !male %in% c(0L, 1L)) ||
      any(!is.na(female) & !female %in% c(0L, 1L))) stop('Invalid saved gender flags')
  is_male <- !is.na(male) & male == 1L
  is_female <- !is.na(female) & female == 1L
  if (any(is_male & is_female)) stop('Conflicting male_80 and female_80 flags')
  fifelse(is_male, 'male', fifelse(is_female, 'female', 'unclassified'))
}

amws_load_gender <- function(record_file, checksum_file, sources) {
  # Base parsing correctly decodes literal quotation runs in saved OCR/name text.
  g <- as.data.table(read.csv(record_file, colClasses = 'character', stringsAsFactors = FALSE,
                             na.strings = '', check.names = FALSE, encoding = 'UTF-8'))
  required <- c('edition', 'source_row', 'source_record_id', 'GEOID', 'birth_year',
                'first_name', 'male_80', 'female_80', 'record_id', 'validated_gender')
  if (!all(required %in% names(g))) stop('Saved male_80 and female_80 flags are required; run add_amws_gender_threshold_flags.R')
  for (col in c('edition', 'source_row', 'birth_year', 'male_80', 'female_80')) {
    old <- g[[col]]; value <- suppressWarnings(as.integer(old))
    if (any(!is.na(old) & (is.na(value) | suppressWarnings(as.numeric(old)) != value))) stop('Invalid gender numeric field: ', col)
    set(g, j = col, value = value)
  }
  if (anyNA(g[, .(edition, source_row, birth_year, GEOID)]) ||
      anyDuplicated(g[, .(edition, source_row)])) stop('Invalid or ambiguous gender source-row keys')
  g[, gender_80 := amws_classify_flags(male_80, female_80)]
  g[, gender_reason_80 := fifelse(gender_80 != 'unclassified', 'saved_flag',
    fifelse(is.na(first_name) | !nzchar(first_name), 'unreadable',
      fifelse(is.na(male_80) | is.na(female_80), 'unmatched', 'ambiguous')))]
  correction <- g[!is.na(validated_gender)]
  if (nrow(correction) != 1L || correction$record_id != '1906:19:17' ||
      correction$male_80 != 1L || correction$female_80 != 0L || correction$gender_80 != 'male') {
    stop('Documented Adams correction is not encoded in saved flags')
  }
  manifest <- fread(checksum_file)
  current <- normalizePath(sources, winslash = '/', mustWork = TRUE)
  idx <- match(current, normalizePath(manifest$path, winslash = '/', mustWork = TRUE))
  if (anyNA(idx) || !identical(unname(tools::md5sum(sources)), manifest$md5_after[idx])) {
    stop('AMWS source files differ from the saved gender-inference sources; do not join by row position')
  }
  g
}

amws_attach_gender <- function(records, gender) {
  if (anyDuplicated(records[, .(edition, source_row)])) stop('Duplicate retained source-row keys')
  idx <- match(paste(records$edition, records$source_row, sep = ':'),
               paste(gender$edition, gender$source_row, sep = ':'))
  found <- !is.na(idx)
  j <- idx[found]
  if (any(records$GEOID[found] != gender$GEOID[j] | records$year[found] != gender$birth_year[j] |
          records$source_record_id[found] != gender$source_record_id[j])) {
    stop('Gender source-row match has conflicting identifier, county or birth year')
  }
  records <- copy(records)
  records[, `:=`(gender_80 = 'unclassified', gender_reason_80 = 'no_saved_inference',
                 male_80 = NA_integer_, female_80 = NA_integer_, gender_inference_threshold = 0.8)]
  records[which(found), `:=`(gender_80 = gender$gender_80[j], gender_reason_80 = gender$gender_reason_80[j],
                            male_80 = gender$male_80[j], female_80 = gender$female_80[j])]
  records
}

amws_add_gender_annual <- function(cy, records) {
  for (g in amws_gender_groups) {
    suffix <- amws_gender_suffix(g)
    z <- records[gender_80 == g, .(
      n_early_year = sum(source == '1906_1955_dedup'), n_1986_year = sum(source == '1986'),
      n_early_assigned = sum(source == '1906_1955_dedup' & !is.na(month)),
      n_1986_assigned = sum(source == '1986' & !is.na(month)),
      n_early_unassigned = sum(source == '1906_1955_dedup' & is.na(month)),
      n_1986_unassigned = sum(source == '1986' & is.na(month)), n_unassigned_month = sum(is.na(month))), by = .(GEOID, year)]
    cols <- paste0(amws_gender_annual_bases, suffix)
    setnames(z, amws_gender_annual_bases, cols)
    cy <- merge(cy, z, by = c('GEOID', 'year'), all.x = TRUE)
    for (col in cols) set(cy, which(is.na(cy[[col]])), col, 0L)
    early <- cy[[paste0('n_early_year', suffix)]]; late <- cy[[paste0('n_1986_year', suffix)]]
    ea <- cy[[paste0('n_early_assigned', suffix)]]; la <- cy[[paste0('n_1986_assigned', suffix)]]
    set(cy, j = paste0('birth_month_completeness', suffix), value = ifelse(early + late > 0, (ea + la) / (early + late), NA_real_))
    set(cy, j = paste0('birth_month_complete', suffix), value = cy[[paste0('n_unassigned_month', suffix)]] == 0L)
    set(cy, j = paste0('early_month_completeness', suffix), value = ifelse(early > 0, ea / early, NA_real_))
    set(cy, j = paste0('late_month_completeness', suffix), value = ifelse(late > 0, la / late, NA_real_))
  }
  cy[, gender_inference_threshold := 0.8]
  amws_validate_gender_annual(cy)
  cy
}

amws_validate_gender_annual <- function(cy) {
  required <- as.vector(outer(amws_gender_annual_bases, amws_gender_suffix(amws_gender_groups), paste0))
  if (!all(required %in% names(cy))) stop('Incomplete gender-specific annual schema')
  for (base in amws_gender_annual_bases) {
    summed <- Reduce('+', lapply(amws_gender_groups, function(g) cy[[paste0(base, amws_gender_suffix(g))]]))
    if (any(summed != cy[[base]])) stop('Gender annual totals do not reconcile: ', base)
  }
  for (g in amws_gender_groups) {
    s <- amws_gender_suffix(g)
    for (src in c('early', '1986')) {
      if (any(cy[[paste0('n_', src, '_assigned', s)]] + cy[[paste0('n_', src, '_unassigned', s)]] !=
              cy[[paste0('n_', src, '_year', s)]])) stop('Gender assigned/unassigned reconciliation failed')
    }
  }
  invisible(TRUE)
}

amws_gender_month_counts <- function(records) {
  keys <- unique(records[!is.na(month), .(GEOID, year, month)])
  for (g in amws_gender_groups) {
    z <- records[gender_80 == g & !is.na(month), .(
      n_amws_1906_1955_dedup = sum(source == '1906_1955_dedup'),
      n_amws_1986 = sum(source == '1986'), n_amws = .N), by = .(GEOID, year, month)]
    setnames(z, c('n_amws_1906_1955_dedup', 'n_amws_1986', 'n_amws'),
             paste0(c('n_amws_1906_1955_dedup', 'n_amws_1986', 'n_amws'), amws_gender_suffix(g)))
    keys <- merge(keys, z, by = c('GEOID', 'year', 'month'), all.x = TRUE)
  }
  for (col in amws_gender_period_cols()) set(keys, which(is.na(keys[[col]])), col, 0L)
  keys
}

amws_add_gender_rates <- function(x, denominator) {
  for (base in c('n_amws_1906_1955_dedup', 'n_amws_1986', 'n_amws')) {
    summed <- Reduce('+', lapply(amws_gender_groups, function(g) x[[paste0(base, amws_gender_suffix(g))]]))
    if (anyNA(summed) || any(summed != x[[base]])) stop('Gender period counts do not reconcile: ', base)
  }
  for (g in amws_gender_groups) {
    s <- amws_gender_suffix(g); n <- x[[paste0('n_amws', s)]]
    if (any(n != x[[paste0('n_amws_1906_1955_dedup', s)]] + x[[paste0('n_amws_1986', s)]])) {
      stop('Gender source components do not sum to period total')
    }
    set(x, j = paste0('amws_per_1000_births', s), value = monthly_rate(n, x[[denominator]]))
    set(x, j = paste0('amws_per_1000_pop', s), value = monthly_rate(n, x$population))
  }
  amws_validate_gender_rates(x, denominator)
  extra <- amws_gender_columns(names(x))
  setcolorder(x, c(setdiff(names(x), extra), extra))
  x
}

amws_validate_gender_rates <- function(x, denominator) {
  for (base in c('amws_per_1000_births', 'amws_per_1000_pop')) {
    d <- x[[if (base == 'amws_per_1000_births') denominator else 'population']]
    for (g in amws_gender_groups) {
      verify_monthly_rates(x[[paste0('n_amws', amws_gender_suffix(g))]], d,
                           x[[paste0(base, amws_gender_suffix(g))]])
    }
    summed <- Reduce('+', lapply(amws_gender_groups, function(g) x[[paste0(base, amws_gender_suffix(g))]]))
    if (!isTRUE(all.equal(summed, x[[base]], tolerance = 1e-10))) stop('Gender rates are not additive')
  }
  invisible(TRUE)
}

amws_gender_reconciliation <- function(period, cy, time_col) {
  cols <- amws_gender_period_cols()
  q <- period[, lapply(.SD, sum), by = .(GEOID, year), .SDcols = cols]
  a_cols <- c('GEOID', 'year', amws_gender_columns(names(cy)))
  q <- merge(q, cy[, ..a_cols], by = c('GEOID', 'year'), all.x = TRUE)
  for (g in amws_gender_groups) {
    s <- amws_gender_suffix(g)
    ok <- q[[paste0('n_amws_1906_1955_dedup', s)]] == q[[paste0('n_early_assigned', s)]] &
          q[[paste0('n_amws_1986', s)]] == q[[paste0('n_1986_assigned', s)]]
    if (any(!ok)) stop('Gender period/annual assignment reconciliation failed')
    set(q, j = paste0(time_col, '_reconciles', s), value = ok)
  }
  q
}

# Panel CSVs contain one physical line per row (no scientist OCR columns).
# Compare all legacy values in bounded chunks before replacing a panel.
amws_invariant_panel_columns <- function(header) {
  legacy <- setdiff(header, amws_gender_columns(header))
  if (identical(Sys.getenv('AMWS_PANEL_SOURCE_REFRESH'), 'TRUE')) {
    # An explicit source refresh changes late/combined counts and their rates,
    # but not county/time keys, early-edition counts, population or births.
    refreshed <- grepl('^n_amws($|_1986)|^n_1986_|^n_unassigned_month$|^amws_per_|^birth_month_complet|^late_month_complet', legacy)
    legacy <- legacy[!refreshed]
  }
  legacy
}
amws_verify_staged_panel <- function(stage, target, expected_rows) {
  if (identical(Sys.getenv('AMWS_PANEL_VALIDATION_ENGINE'), 'duckdb')) {
    return(amws_verify_staged_panel_duckdb(stage, target, expected_rows))
  }
  stage_header <- names(fread(stage, nrows = 0L))
  new_con <- file(stage, open = 'rt', encoding = 'UTF-8'); on.exit(close(new_con))
  stage_header_line <- readLines(new_con, n = 1L)
  old_con <- NULL
  if (file.exists(target)) {
    old_header <- names(fread(target, nrows = 0L))
    legacy <- amws_invariant_panel_columns(old_header)
    if (!all(old_header %in% stage_header)) stop('Staged panel dropped original columns')
    old_con <- file(target, open = 'rt', encoding = 'UTF-8'); on.exit(close(old_con), add = TRUE)
    old_header_line <- readLines(old_con, n = 1L)
  } else legacy <- stage_header
  rows <- 0L
  repeat {
    lines <- readLines(new_con, n = 20000L)
    if (!length(lines)) break
    current <- fread(text = paste(c(stage_header_line, lines), collapse = '\n'), header = TRUE,
                     select = legacy, colClasses = 'character', showProgress = FALSE)
    rows <- rows + nrow(current)
    if (!is.null(old_con)) {
      previous_lines <- readLines(old_con, n = length(lines))
      if (length(previous_lines) != length(lines)) stop('Staged panel changed original row count')
      previous <- fread(text = paste(c(old_header_line, previous_lines), collapse = '\n'), header = TRUE,
                        select = legacy, colClasses = 'character', showProgress = FALSE)
      if (!identical(as.list(current), as.list(previous))) stop('Staged panel changed original column values: ', basename(target))
    }
  }
  if (!is.null(old_con) && length(readLines(old_con, n = 1L))) stop('Staged panel truncated original rows')
  if (rows != expected_rows) stop('Incorrect staged row count')
  message('Preserved ', if (identical(Sys.getenv('AMWS_PANEL_SOURCE_REFRESH'), 'TRUE'))
    'protected keys, early counts and denominators: ' else 'all original non-gender columns: ',
    basename(target), ' (', rows, ' rows)')
  invisible(TRUE)
}

# Optional SQL verification avoids repeated R parsing of bounded CSV chunks.
amws_verify_staged_panel_duckdb <- function(stage, target, expected_rows) {
  if (!requireNamespace('duckdb', quietly = TRUE) || !requireNamespace('DBI', quietly = TRUE)) {
    stop('DuckDB and DBI are required for AMWS_PANEL_VALIDATION_ENGINE=duckdb')
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = tempfile(fileext = '.duckdb'))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "SET memory_limit='4GB'")
  DBI::dbExecute(con, 'SET threads=4')
  q <- function(x) as.character(DBI::dbQuoteString(con, normalizePath(x, winslash = '/')))
  ident <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  header <- names(fread(stage, nrows = 0L))
  old_header <- if (file.exists(target)) names(fread(target, nrows = 0L)) else header
  legacy <- amws_invariant_panel_columns(old_header)
  if (!all(old_header %in% header)) stop('Staged panel dropped original columns')
  cols <- paste(ident(legacy), collapse = ',')
  DBI::dbExecute(con, paste0('CREATE TABLE staged AS SELECT row_number() OVER () AS position, ', cols,
    ' FROM read_csv(', q(stage), ', all_varchar=true)'))
  if (DBI::dbGetQuery(con, 'SELECT count(*) AS n FROM staged')$n != expected_rows) stop('Incorrect staged row count')
  if (file.exists(target)) {
    DBI::dbExecute(con, paste0('CREATE TABLE previous AS SELECT row_number() OVER () AS position, ', cols,
      ' FROM read_csv(', q(target), ', all_varchar=true)'))
    predicate <- paste(paste0('s.', ident(legacy), ' IS DISTINCT FROM p.', ident(legacy)), collapse = ' OR ')
    bad <- DBI::dbGetQuery(con, paste0('SELECT count(*) AS n FROM staged s FULL OUTER JOIN previous p USING(position)',
      ' WHERE s.position IS NULL OR p.position IS NULL OR ', predicate))$n
    if (bad != 0) stop('Staged panel changed original column values or row order: ', basename(target))
  }
  message('DuckDB preserved ', if (identical(Sys.getenv('AMWS_PANEL_SOURCE_REFRESH'), 'TRUE'))
    'protected keys, early counts and denominators: ' else 'all original non-gender columns: ',
    basename(target), ' (', expected_rows, ' rows)')
  invisible(TRUE)
}

amws_install_staged <- function(stage_root, dest) {
  sources <- sort(list.files(stage_root, recursive = TRUE, full.names = TRUE))
  prefix <- paste0(normalizePath(stage_root, winslash = '/'), '/')
  targets <- file.path(dest, substring(normalizePath(sources, winslash = '/'), nchar(prefix) + 1L))
  backups <- rep(NA_character_, length(targets)); installed <- rep(FALSE, length(targets))
  complete <- FALSE
  on.exit({
    if (!complete) for (i in rev(seq_along(targets))) {
      if (installed[i]) unlink(targets[i])
      if (!is.na(backups[i]) && file.exists(backups[i])) file.rename(backups[i], targets[i])
    }
  })
  for (i in seq_along(targets)) {
    dir.create(dirname(targets[i]), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(targets[i])) {
      backups[i] <- tempfile('gender_panel_backup_', tmpdir = dirname(targets[i]), fileext = '.csv')
      if (!file.rename(targets[i], backups[i])) stop('Cannot back up existing output: ', targets[i])
    }
    if (!file.rename(sources[i], targets[i])) stop('Cannot install staged output: ', targets[i])
    installed[i] <- TRUE
  }
  complete <- TRUE
  unlink(backups[!is.na(backups)])
  # Only remove directories created for this build, after every file was moved.
  remaining <- list.files(stage_root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if (!length(remaining) && startsWith(normalizePath(stage_root, winslash = '/'),
      paste0(normalizePath(dest, winslash = '/'), '/'))) unlink(stage_root, recursive = TRUE)
  invisible(targets)
}
