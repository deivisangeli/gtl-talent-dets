# Census coverage of college faculty, and the factor that puts every census on
# one scale.
#
# OCC1950 professors (012-029) capture a different share of real college faculty
# in each census: about a quarter in 1870 and 1900, but two-fifths in 1880 and
# 1910 and half in 1940. For men the jumps are coverage, not composition (1910
# professors are more concentrated in college counties than 1900's, with the
# same age and marriage profile), so one factor per census rescales the counts.
#
#   coverage       = census men coded OCC1950 010-029 / NCES male professional staff
#   faculty_factor = 1 / coverage
#
# Multiplying a census's professor counts by its factor estimates all male college
# faculty: the scale on which a faculty member's children count whether or not
# the census coded him "professor". NCES (prep/occupational_fertility/
# nces_faculty_benchmark.csv, Table 26 of NCES 93-442) counts all professional
# staff, administrators included, which is why presidents and deans (010) join
# the census side, and why the faculty scale is an upper bound.
# 1850 and 1860 have no NCES year and borrow the 1870 factor (factor_imputed).
# NCES reports no professional staff for 1929-30, only instructional staff; the
# 1930 benchmark scales that by the nearest year with both, 1931-32
# (male professional / instructional), and is flagged nces_estimated.
#
# Census counts come from the 1850-1950 full counts (usa_00011) when that
# Parquet tree exists, so every census is read from one extract; otherwise from
# the 1850-1910 extract plus the 1940 full count.
#
# The factor rescales counts; it does not reclassify anyone, and it leaves any
# comparison made within one census unchanged.
#
# Inputs: IPUMS_1850_1950_FERTILITY_PARQUET_DIR, or else
#   IPUMS_1850_1910_FERTILITY_PARQUET_DIR and IPUMS_1940_PARQUET_DIR (as the builders)
# Output: OCC_COVERAGE_OUT (default <Dropbox>/output/occupational_fertility/professor_coverage_factors.csv)
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/build_professor_coverage.R

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(data.table)
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
data_root <- function() {
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
}
env_or <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default()
}
DIR_1850_1950 <- env_or("IPUMS_1850_1950_FERTILITY_PARQUET_DIR",
                        function() "D:/ipums_1850_1950_fullcount_fertility")
DIR_1850_1910 <- env_or("IPUMS_1850_1910_FERTILITY_PARQUET_DIR",
                        function() "D:/ipums_1850_1910_fullcount_fertility")
DIR_1940 <- env_or("IPUMS_1940_PARQUET_DIR", function() "D:/ipums_1940_fullcount")
BENCHMARK <- env_or("OCC_COVERAGE_BENCHMARK", function()
  file.path(repo_root, "prep/occupational_fertility/nces_faculty_benchmark.csv"))
OUT <- env_or("OCC_COVERAGE_OUT", function()
  file.path(data_root(), "output/occupational_fertility/professor_coverage_factors.csv"))
IMPUTE_FROM <- 1870L

sql_path <- function(path) gsub("\\\\", "/", path)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)

main <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  count_sql <- paste(
    "SELECT %s AS census, COUNT(*) FILTER (WHERE OCC1950 BETWEEN 12 AND 29) AS census_professors,",
    "COUNT(*) FILTER (WHERE OCC1950 = 10) AS census_pres_deans,",
    "COUNT(*) FILTER (WHERE OCC1950 BETWEEN 10 AND 29) AS census_count",
    "FROM read_parquet('%s'%s) WHERE SEX = 1")
  single <- length(Sys.glob(file.path(DIR_1850_1950, "YEAR=*"))) > 0L
  year_dir <- if (single) DIR_1850_1950 else DIR_1850_1910
  years <- sort(as.integer(sub("^YEAR=", "", basename(list.dirs(year_dir, recursive = FALSE)))))
  counts <- rbindlist(lapply(years[!is.na(years)], function(year) {
    as.data.table(DBI::dbGetQuery(con, sprintf(count_sql, year,
      sql_path(file.path(year_dir, paste0("YEAR=", year), "*.parquet")), "")))
  }))
  if (!single && dir.exists(file.path(DIR_1940, "person"))) {
    counts <- rbind(counts, as.data.table(DBI::dbGetQuery(con, sprintf(count_sql, 1940L,
      sql_path(file.path(DIR_1940, "person", "*", "*.parquet")), ", hive_partitioning = true"))))
  }
  message("Census counts from ", if (single) DIR_1850_1950 else paste(DIR_1850_1910, "+", DIR_1940))
  bench <- fread(BENCHMARK)
  if (!"instructional_staff" %in% names(bench)) bench[, instructional_staff := NA_real_]
  for (col in c("staff_total", "staff_male", "staff_female", "instructional_staff")) {
    set(bench, j = col, value = as.numeric(bench[[col]]))
  }
  # A benchmark year with instructional staff only: scale it by the male
  # professional / instructional ratio of the nearest year that reports both.
  bench[, `:=`(nces_estimated = FALSE, estimate_from = NA_character_)]
  both <- bench[!is.na(staff_male) & !is.na(instructional_staff)]
  start_year <- function(academic_year) as.integer(substr(academic_year, 1, 4))
  for (i in which(is.na(bench$staff_male) & !is.na(bench$instructional_staff))) {
    stop_if(!nrow(both), "No benchmark year reports both professional and instructional staff")
    near <- both[which.min(abs(start_year(academic_year) - start_year(bench$academic_year[i])))]
    set(bench, i, "staff_male", bench$instructional_staff[i] * near$staff_male / near$instructional_staff)
    set(bench, i, "nces_estimated", TRUE)
    set(bench, i, "estimate_from", near$academic_year)
  }
  bench <- bench[!is.na(census_year)]
  out <- merge(counts, bench[, .(census = census_year, academic_year, nces_male_staff = staff_male,
                                 nces_total_staff = staff_total, nces_estimated, estimate_from)],
               by = "census", all.x = TRUE)
  out[, `:=`(coverage = census_count / nces_male_staff, factor_imputed = is.na(nces_male_staff))]
  out[is.na(nces_estimated), nces_estimated := FALSE]
  stop_if(!IMPUTE_FROM %in% out[factor_imputed == FALSE, census],
          "No NCES benchmark for the imputation year ", IMPUTE_FROM)
  out[factor_imputed == TRUE, coverage := out[census == IMPUTE_FROM, coverage]]
  out[, faculty_factor := 1 / coverage]
  # Benchmark identity: rescaled census counts reproduce NCES in every benchmark year.
  check <- out[factor_imputed == FALSE, abs(census_count * faculty_factor - nces_male_staff)]
  stop_if(any(check > 1e-6), "Rescaled census counts do not reproduce the NCES totals")
  dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
  fwrite(out[order(census)], OUT)
  print(out[order(census), .(census, census_count, nces_male_staff, coverage = round(coverage, 3),
                             faculty_factor = round(faculty_factor, 2), factor_imputed,
                             nces_estimated)])
  message("Wrote ", OUT)
}

main()
