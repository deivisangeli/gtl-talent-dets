# County-level coverage of college faculty by the census, and the factor that
# rescales a county's coded professors to its faculty headcount.
#
# The national factor (build_professor_coverage.R) rescales every county by the
# same ratio, but the census's coding of professors swings county by county.
# This uses the county faculty panel built from the Education by the Numbers
# archive (Data/processed/faculty_data/county_faculty_panel_estimated_interpolated.csv:
# corrected headcounts, FTE converted with 1949 staff ratios, bounded gaps
# interpolated; fixed 2020 counties) to set a factor per county and census:
#
#   county_factor = faculty headcount / census professors   if headcount > census professors
#                 = 1                                        otherwise (never scaled down)
#
# census professors = persons of both sexes coded OCC1950 010-029 in the county
# (the headcount covers men and women alike). Panel year matched to the census:
# the academic year under way on census day where the panel labels academic
# years by their start (1909 for 1910, 1919 for 1920, 1929 for 1930, 1939 for
# 1940, 1949 for 1950); the 1850-1900 single-year reports use the same year.
# 1949-50 is land-grant coverage only. A missing headcount, a headcount at or
# below the census count, or a county with no coded professors (no children to
# scale) gets 1; `reason` says which.
#
# Inputs:  IPUMS_1850_1950_FERTILITY_PARQUET_DIR, OCC_COUNTY_FACULTY_PANEL
# Output:  OCC_COUNTY_COVERAGE_OUT (default <Dropbox>/output/occupational_fertility/
#          professor_county_coverage_factors.csv) and a _summary.csv by census
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/build_professor_county_coverage.R

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
PARQUET <- env_or("IPUMS_1850_1950_FERTILITY_PARQUET_DIR", function() "D:/ipums_1850_1950_fullcount_fertility")
PANEL <- env_or("OCC_COUNTY_FACULTY_PANEL", function()
  file.path(data_root(), "Data/processed/faculty_data/county_faculty_panel_estimated_interpolated.csv"))
OUT <- env_or("OCC_COUNTY_COVERAGE_OUT", function()
  file.path(data_root(), "output/occupational_fertility/professor_county_coverage_factors.csv"))
EVENT_UNITS <- env_or("OCC_CHANNEL_EVENT_UNITS", function()
  file.path(data_root(), "output/land_grants/andrews_event_county_units_1850_1920.csv"))
PANEL_YEAR <- c(`1850` = 1850L, `1860` = 1860L, `1870` = 1870L, `1880` = 1880L, `1900` = 1900L,
                `1910` = 1909L, `1920` = 1919L, `1930` = 1929L, `1940` = 1939L, `1950` = 1949L)

say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
sql_path <- function(path) gsub("\\\\", "/", path)

main <- function() {
  years <- sort(as.integer(sub("^YEAR=", "", basename(Sys.glob(file.path(PARQUET, "YEAR=*"))))))
  stop_if(!length(years), "No YEAR partitions under ", PARQUET)
  stop_if(!all(as.character(years) %in% names(PANEL_YEAR)), "No panel year for census ",
          paste(setdiff(years, as.integer(names(PANEL_YEAR))), collapse = ", "))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads = 12")
  census <- rbindlist(lapply(years, function(year) {
    glob <- sql_path(file.path(PARQUET, paste0("YEAR=", year), "*.parquet"))
    as.data.table(DBI::dbGetQuery(con, sprintf(paste(
      "SELECT STATEFIP * 1000 + COUNTYICP // 10 AS county_geoid, COUNT(*) AS census_professors,",
      "COUNT(*) FILTER (WHERE SEX = 1) AS census_professors_men",
      "FROM read_parquet('%s') WHERE COUNTYICP %% 10 = 0 AND OCC1950 BETWEEN 10 AND 29 GROUP BY ALL"),
      glob)))[, census := year]
  }))
  say(nrow(census), " county x census cells with coded professors")

  panel <- fread(PANEL, select = c("GEOID", "year", "faculty_headcount_annual", "faculty_headcount_status"),
                 colClasses = list(character = "GEOID"))
  stop_if(!nrow(panel), "Empty faculty panel: ", PANEL)
  panel <- panel[year %in% PANEL_YEAR & nzchar(GEOID)]
  panel[, `:=`(census = as.integer(names(PANEL_YEAR))[match(year, PANEL_YEAR)],
               county_geoid = as.integer(GEOID))]
  panel <- panel[, .(county_geoid, census, panel_year = year,
                     faculty_headcount = suppressWarnings(as.numeric(faculty_headcount_annual)),
                     faculty_headcount_status)]
  stop_if(panel[, anyDuplicated(paste(county_geoid, census))] > 0, "Duplicate county x census rows in the panel")

  out <- merge(census, panel, by = c("county_geoid", "census"), all = TRUE)
  out[is.na(census_professors), `:=`(census_professors = 0L, census_professors_men = 0L)]
  out[is.na(panel_year), panel_year := PANEL_YEAR[as.character(census)]]
  out[, reason := fcase(
    is.na(faculty_headcount) | faculty_headcount_status == "missing", "no_headcount",
    census_professors == 0L, "no_census_professors",
    faculty_headcount > census_professors, "scaled",
    default = "headcount_le_census")]
  out[, county_factor := fifelse(reason == "scaled", faculty_headcount / census_professors, 1)]
  out[, scaled := reason == "scaled"]
  out[, GEOID := sprintf("%05d", county_geoid)]
  out <- out[, .(GEOID, county_geoid, census, panel_year, census_professors, census_professors_men,
                 faculty_headcount, faculty_headcount_status, county_factor, scaled, reason)]
  setorder(out, county_geoid, census)

  events <- unique(as.integer(fread(EVENT_UNITS, colClasses = list(character = "GEOID"))$GEOID))
  summary <- out[county_geoid %in% events, .(
    event_counties_with_professors = sum(census_professors > 0),
    with_headcount = sum(reason != "no_headcount"), scaled = sum(scaled),
    headcount_le_census = sum(reason == "headcount_le_census"),
    no_census_professors = sum(reason == "no_census_professors"),
    median_factor_scaled = if (any(scaled)) median(county_factor[scaled]) else NA_real_,
    max_factor = max(county_factor),
    census_professors = sum(census_professors),
    scaled_professors = sum(census_professors * county_factor)), by = census][order(census)]
  dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
  fwrite(out, OUT)
  fwrite(summary, sub("\\.csv$", "_summary.csv", OUT))
  say("Wrote ", OUT)
  print(summary)
}

main()
