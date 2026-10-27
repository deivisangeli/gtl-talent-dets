# Build per-census cells for the professor-fertility trend, 1850-1910, on NCHILD.
#
# NCHILD (own children of any age in the household, including step- and adopted
# children, capped at 9+) is the one fertility-related measure filled in every
# surviving census. CHBORN/CHSURV start in 1900 and MARST in 1880, so:
#   married_sp = SPLOC > 0 in every year (MARST is blank 1850-1870), and
#   own children aged 0-4 / 0-9 come from POPLOC (father) and MOMLOC (mother).
# Before 1880 the census did not record relationship to head; IPUMS imputes the
# family pointers, and with them NCHILD.
#
# Each census year is processed on its own to bound memory.
#
# Outputs (OCC_FERTILITY_DECADES_OUT_DIR, default
# <Dropbox>/output/occupational_fertility/decades):
#   county_cells/YEAR=*/part.parquet  men aged 20-69 grouped by
#       STATEFIP x COUNTYICP x 10-year age bin x occ_group x married_sp x in_household
#       (input to build_professor_county_nchild.R)
#   men_cells/YEAR=*/part.parquet   men aged 20-64 grouped by
#       occ_group x AGE x married_sp x in_household x us_born x race3 x urban x STATEFIP
#       with n, sums of NCHILD and NCHILD^2, and own children <5 / <10 (all, bio)
#   professors_by_year.csv          every person with OCC1950 010-029
#   couples_calibration_cells.csv   1900 and 1910: husband group x wife age, with
#       the wife's CHSURV/CHBORN and both spouses' NCHILD
#   professor_occ_codes.csv         contemporary OCC codes among professors
#   decade_diagnostics.csv          per-year counts and the NCHILD cross-check
#   occupation_groups.csv, build_manifest.json
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/build_professor_nchild_decades.R
# Input: IPUMS_1850_1910_FERTILITY_PARQUET_DIR (default D:/ipums_1850_1910_fullcount_fertility).

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

repo_root <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  } else {
    Sys.getenv("GTL_REPO", unset = getwd())
  }
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not locate the repo root (no paths.R found)")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
})

IN_DIR <- Sys.getenv("IPUMS_1850_1910_FERTILITY_PARQUET_DIR",
                     unset = "D:/ipums_1850_1910_fullcount_fertility")
OUT_DIR <- Sys.getenv("OCC_FERTILITY_DECADES_OUT_DIR", unset = "")
if (!nzchar(OUT_DIR)) {
  data_root <- local({
    env <- new.env()
    suppressWarnings(try(sys.source(file.path(repo_root, "paths.R"), envir = env), silent = TRUE))
    candidates <- c(
      get0("TALENT_DETS_DATA_DIR", envir = env, ifnotfound = ""),
      Sys.getenv("DET_DIR"),
      file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox/gtl_talent_dets")
    )
    hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
    if (!length(hit)) stop("Could not locate the Dropbox data root. Set TALENT_DETS_DATA_DIR or DET_DIR.")
    normalizePath(hit[1], winslash = "/", mustWork = TRUE)
  })
  OUT_DIR <- file.path(data_root, "output/occupational_fertility/decades")
}
THREADS <- as.integer(Sys.getenv("OCC_FERTILITY_THREADS", unset = "12"))

source(file.path(repo_root, "prep/occupational_fertility/occupation_groups.R"))

say <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
sql_path <- function(path) gsub("\\\\", "/", path)
q <- function(con, sql) DBI::dbGetQuery(con, sql)
x <- function(con, sql) invisible(DBI::dbExecute(con, sql))

# Decoded columns carry a dec_ prefix: DuckDB identifiers are case-insensitive,
# so a decoded `chborn` would collide with the raw CHBORN.
decode_sql <- function(year_glob) paste(
  "SELECT STATEFIP, COUNTYICP, SERIAL, PERNUM, SEX, AGE, GQ, OCC, OCC1950, MARST, SPLOC,",
  "POPLOC, MOMLOC, STEPPOP, NCHILD,",
  "CASE WHEN CHBORN BETWEEN 1 AND 96 THEN CHBORN - 1 END AS dec_chborn,",
  "CASE WHEN CHSURV >= 1 THEN CHSURV - 1 END AS dec_chsurv,",
  "(SPLOC > 0) AS married_sp,",
  "(GQ IN (1, 2)) AS in_household,",
  "CASE WHEN BPL BETWEEN 1 AND 99 THEN 1 WHEN BPL >= 100 THEN 0 END AS us_born,",
  "CASE WHEN RACE = 1 THEN 'white' WHEN RACE = 2 THEN 'black'",
  "WHEN RACE IS NULL THEN NULL ELSE 'other' END AS race3,",
  "CASE WHEN URBAN = 2 THEN 1 WHEN URBAN = 1 THEN 0 END AS urban,",
  group_case(), "AS occ_group",
  sprintf("FROM read_parquet('%s')", year_glob))

own_children_sql <- function(pointer, step_filter) paste(
  "SELECT SERIAL,", pointer, "AS PERNUM,",
  "COUNT(*) AS own_all,",
  "COUNT(*) FILTER (WHERE AGE <= 4) AS own_u5,",
  "COUNT(*) FILTER (WHERE AGE <= 9) AS own_u10,",
  "COUNT(*) FILTER (WHERE", step_filter, ") AS own_all_bio,",
  "COUNT(*) FILTER (WHERE AGE <= 4 AND", step_filter, ") AS own_u5_bio,",
  "COUNT(*) FILTER (WHERE AGE <= 9 AND", step_filter, ") AS own_u10_bio",
  "FROM p WHERE", pointer, "> 0 GROUP BY ALL")

build_year <- function(con, year, cells_dir, county_dir) {
  year_glob <- sql_path(file.path(IN_DIR, paste0("YEAR=", year), "*.parquet"))
  x(con, "DROP TABLE IF EXISTS p; DROP TABLE IF EXISTS kids_father;
          DROP TABLE IF EXISTS kids_mother; DROP TABLE IF EXISTS persons")
  x(con, paste("CREATE TABLE p AS", decode_sql(year_glob)))
  unmapped <- q(con, "SELECT COUNT(*) AS n FROM p WHERE occ_group IS NULL")$n
  stop_if(unmapped > 0, year, ": ", unmapped, " persons have an OCC1950 code outside every group")
  # STEPPOP describes the father, so only the father side has a biological variant.
  x(con, paste("CREATE TABLE kids_father AS", own_children_sql("POPLOC", "STEPPOP = 0")))
  x(con, paste("CREATE TABLE kids_mother AS", own_children_sql("MOMLOC", "TRUE")))
  # SERIAL is unique within a census, so the joins need no STATEFIP.
  x(con, paste(
    "CREATE TABLE persons AS SELECT p.*,",
    "COALESCE(CASE WHEN p.SEX = 1 THEN f.own_all ELSE m.own_all END, 0) AS own_all,",
    "COALESCE(CASE WHEN p.SEX = 1 THEN f.own_u5 ELSE m.own_u5 END, 0) AS own_u5,",
    "COALESCE(CASE WHEN p.SEX = 1 THEN f.own_u10 ELSE m.own_u10 END, 0) AS own_u10,",
    "COALESCE(CASE WHEN p.SEX = 1 THEN f.own_u5_bio ELSE m.own_u5_bio END, 0) AS own_u5_bio,",
    "COALESCE(CASE WHEN p.SEX = 1 THEN f.own_u10_bio ELSE m.own_u10_bio END, 0) AS own_u10_bio",
    "FROM p LEFT JOIN kids_father f ON f.SERIAL = p.SERIAL AND f.PERNUM = p.PERNUM",
    "LEFT JOIN kids_mother m ON m.SERIAL = p.SERIAL AND m.PERNUM = p.PERNUM"))

  diag <- q(con, paste(
    "SELECT COUNT(*) AS persons,",
    "COUNT(*) FILTER (WHERE SEX = 1 AND AGE BETWEEN 20 AND 64) AS men_20_64,",
    "COUNT(*) FILTER (WHERE SEX = 1 AND AGE BETWEEN 20 AND 69) AS men_20_69,",
    "COUNT(*) FILTER (WHERE LEAST(own_all, 9) <> NCHILD) AS nchild_mismatch,",
    "COUNT(*) FILTER (WHERE SEX = 1 AND LEAST(own_all, 9) <> NCHILD) AS nchild_mismatch_men,",
    "COUNT(*) FILTER (WHERE married_sp) AS married_sp,",
    "COUNT(MARST) AS marst_filled,",
    "COUNT(*) FILTER (WHERE occ_group = 'professor' AND SEX = 1) AS professors_male,",
    "COUNT(*) FILTER (WHERE occ_group = 'professor' AND SEX = 2) AS professors_female,",
    "COUNT(*) FILTER (WHERE occ_group = 'professor' AND SEX = 1 AND married_sp)",
    "  AS professors_male_married,",
    "COUNT(*) FILTER (WHERE occ_group = 'professor' AND SEX = 1 AND AGE BETWEEN 25 AND 54",
    "  AND in_household) AS professors_male_25_54_household,",
    "COUNT(*) FILTER (WHERE occ_group = 'pres_dean') AS pres_deans",
    "FROM persons"))
  recip <- q(con, paste(
    "SELECT COUNT(*) AS sploc_links, COUNT(*) FILTER (WHERE w.SPLOC = h.PERNUM) AS reciprocal",
    "FROM persons h JOIN persons w ON w.SERIAL = h.SERIAL AND w.PERNUM = h.SPLOC",
    "WHERE h.SPLOC > 0"))
  diag <- cbind(YEAR = year, diag, recip)

  year_dir <- file.path(cells_dir, paste0("YEAR=", year))
  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
  x(con, sprintf(paste(
    "COPY (SELECT %d AS YEAR, occ_group, AGE, married_sp, in_household, us_born, race3,",
    "urban, STATEFIP, COUNT(*) AS n, SUM(NCHILD) AS sum_nchild,",
    "SUM(NCHILD * NCHILD) AS sum_nchild_sq, SUM(own_u5) AS sum_own_u5,",
    "SUM(own_u10) AS sum_own_u10, SUM(own_u5_bio) AS sum_own_u5_bio,",
    "SUM(own_u10_bio) AS sum_own_u10_bio",
    "FROM persons WHERE SEX = 1 AND AGE BETWEEN 20 AND 64 GROUP BY ALL)",
    "TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)"), year,
    sql_path(file.path(year_dir, "part.parquet"))))
  cells_n <- q(con, sprintf("SELECT COUNT(*) AS rows, SUM(n) AS men FROM read_parquet('%s')",
                            sql_path(file.path(year_dir, "part.parquet"))))
  stop_if(cells_n$men != diag$men_20_64, year, ": cells hold ", cells_n$men,
          " men, the persons table ", diag$men_20_64)

  # County x 10-year age bin cells for build_professor_county_nchild.R. Counties
  # are kept as coded (COUNTYICP), historical codes included.
  county_year_dir <- file.path(county_dir, paste0("YEAR=", year))
  dir.create(county_year_dir, recursive = TRUE, showWarnings = FALSE)
  x(con, sprintf(paste(
    "COPY (SELECT %d AS YEAR, STATEFIP, COUNTYICP,",
    "CAST(10 * FLOOR(AGE / 10) AS INTEGER) AS age_bin_lo, occ_group, married_sp,",
    "in_household, COUNT(*) AS n, SUM(NCHILD) AS sum_nchild,",
    "SUM(NCHILD * NCHILD) AS sum_nchild_sq, SUM(own_u5) AS sum_own_u5,",
    "SUM(own_u10) AS sum_own_u10",
    "FROM persons WHERE SEX = 1 AND AGE BETWEEN 20 AND 69 GROUP BY ALL)",
    "TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)"), year,
    sql_path(file.path(county_year_dir, "part.parquet"))))
  county_n <- q(con, sprintf("SELECT COUNT(*) AS rows, SUM(n) AS men FROM read_parquet('%s')",
                             sql_path(file.path(county_year_dir, "part.parquet"))))
  stop_if(county_n$men != diag$men_20_69, year, ": county cells hold ", county_n$men,
          " men, the persons table ", diag$men_20_69)

  professors <- q(con, paste(
    "SELECT", year, "AS YEAR, STATEFIP, SERIAL, PERNUM, SEX, AGE, GQ, in_household, urban,",
    "us_born, race3, MARST, married_sp, OCC, OCC1950, occ_group, NCHILD,",
    "own_u5, own_u10, own_u5_bio, own_u10_bio, dec_chborn AS chborn, dec_chsurv AS chsurv",
    "FROM persons WHERE OCC1950 BETWEEN 10 AND 29 ORDER BY STATEFIP, SERIAL, PERNUM"))
  occ_codes <- q(con, paste(
    "SELECT", year, "AS YEAR, occ_group, SEX, OCC, COUNT(*) AS n FROM persons",
    "WHERE OCC1950 BETWEEN 10 AND 29 GROUP BY ALL ORDER BY occ_group, n DESC, SEX, OCC"))

  calibration <- NULL
  if (q(con, "SELECT COUNT(dec_chborn) AS n FROM persons")$n > 0) {
    calibration <- q(con, paste(
      "SELECT", year, "AS YEAR, h.occ_group,",
      "CAST(5 * FLOOR(w.AGE / 5) AS INTEGER) AS w_age5_lo,",
      "COUNT(*) AS n, COUNT(w.dec_chsurv) AS n_chsurv, SUM(w.dec_chsurv) AS sum_chsurv,",
      "COUNT(w.dec_chborn) AS n_chborn, SUM(w.dec_chborn) AS sum_chborn,",
      "SUM(h.NCHILD) AS sum_h_nchild, SUM(w.NCHILD) AS sum_w_nchild,",
      "SUM(h.own_u5) AS sum_h_own_u5",
      "FROM persons h JOIN persons w ON w.SERIAL = h.SERIAL AND w.PERNUM = h.SPLOC",
      "AND w.SPLOC = h.PERNUM AND w.SEX = 2",
      "WHERE h.SEX = 1 AND h.SPLOC > 0 AND w.AGE BETWEEN 15 AND 79 GROUP BY ALL",
      "ORDER BY h.occ_group, w_age5_lo"))
  }
  list(diag = diag, professors = professors, occ_codes = occ_codes,
       calibration = calibration, cells_rows = cells_n$rows, county_rows = county_n$rows)
}

main <- function() {
  started <- Sys.time()
  years <- sort(as.integer(sub("^YEAR=", "", basename(list.dirs(IN_DIR, recursive = FALSE)))))
  years <- years[!is.na(years)]
  stop_if(!length(years), "No YEAR=* partitions under ", IN_DIR)
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  cells_dir <- file.path(OUT_DIR, "men_cells")
  if (dir.exists(cells_dir)) unlink(cells_dir, recursive = TRUE)
  county_dir <- file.path(OUT_DIR, "county_cells")
  if (dir.exists(county_dir)) unlink(county_dir, recursive = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  x(con, paste0("SET threads = ", THREADS))
  x(con, "SET memory_limit = '20GB'")
  x(con, "SET preserve_insertion_order = false")
  spill <- file.path(tempdir(), "occ_fertility_decades_spill")
  dir.create(spill, showWarnings = FALSE)
  x(con, sprintf("SET temp_directory = '%s'", sql_path(spill)))

  results <- lapply(years, function(year) {
    say("Census ", year)
    out <- build_year(con, year, cells_dir, county_dir)
    say("  ", format(out$diag$persons, big.mark = ","), " persons; ",
        format(out$diag$men_20_64, big.mark = ","), " men 20-64 in ",
        format(out$cells_rows, big.mark = ","), " cells (", format(out$county_rows, big.mark = ","),
        " county cells); male professors ",
        out$diag$professors_male, "; NCHILD mismatches ", out$diag$nchild_mismatch)
    out
  })
  pick <- function(name) do.call(rbind, lapply(results, `[[`, name))
  diagnostics <- pick("diag")
  stop_if(any(diagnostics$reciprocal != diagnostics$sploc_links), "Non-reciprocal SPLOC links")

  utils::write.csv(diagnostics, file.path(OUT_DIR, "decade_diagnostics.csv"), row.names = FALSE)
  utils::write.csv(pick("professors"), file.path(OUT_DIR, "professors_by_year.csv"),
                   row.names = FALSE, na = "")
  utils::write.csv(pick("occ_codes"), file.path(OUT_DIR, "professor_occ_codes.csv"),
                   row.names = FALSE, na = "")
  calibration <- pick("calibration")
  if (!is.null(calibration)) {
    utils::write.csv(calibration, file.path(OUT_DIR, "couples_calibration_cells.csv"),
                     row.names = FALSE)
  }
  utils::write.csv(OCC_GROUPS, file.path(OUT_DIR, "occupation_groups.csv"), row.names = FALSE)

  manifest <- list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    input = sql_path(normalizePath(IN_DIR, winslash = "/")),
    years = years,
    diagnostics = diagnostics,
    married_definition = "SPLOC > 0 (MARST is blank before 1880)",
    elapsed_minutes = round(as.numeric(difftime(Sys.time(), started, units = "mins")), 2))
  jsonlite::write_json(manifest, file.path(OUT_DIR, "build_manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE, na = "null", digits = NA)
  say("Done: ", paste(years, collapse = ", "), " -> ", OUT_DIR)
}

main()
