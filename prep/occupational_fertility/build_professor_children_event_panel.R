# Children of professors by county and birth decade, for the college-allocation
# event counties: the size of the "professors' children" channel.
#
# A college imports professors, and professors' children are unusually likely
# to become scientists. This panel counts, for each Andrews event county, the
# children born there in each decade whose father (POPLOC) was a professor, so
# the channel can be put on the same event-study footing as the AMWS outcome
# (analysis/land_grants/temporal_support_1830_1950/08_run_professor_channel.R).
#
# Counting rule. A child aged a at census C is counted at birth year C - a.
# Each birth year is read from the census that sees it youngest, at ages 0-20,
# so most years come from ages 0-9: closest to birth, most likely still at home
# and born locally. With the 1850-1950 full counts (usa_00011) every census
# but 1890 is held, so each birth year 1830-1949 comes from a census that sees
# it at ages 0-9, except the 1880s (from 1900, at ages 11-20); the last birth
# year is the year before the latest census. Without that extract the builder
# falls back to 1850-1910 plus the 1940 full count, where 1911-1919 is seen by
# no census at 0-20 and is interpolated linearly between 1910 and 1920 (flagged)
# and the window ends in 1939.
# These are surviving co-resident children, which is the right base for an
# outcome that needs the child to reach adulthood, but the count misses
# children living away from their father and includes some born before he moved.
#
# Harmonised measure: faculty_births = children of fathers coded 010-029 times
# the coverage factor of the census the year is read from
# (build_professor_coverage.R), an estimate of all male college faculty's
# children on one scale across censuses. Raw columns are kept unchanged.
#
# Years are binned into decades under both conventions of the AMWS panel:
# standard floor(year / 10) * 10, and alternative (years ending 7-9 move forward).
#
# Geography: census county_geoid (STATEFIP * 1000 + COUNTYICP / 10; NULL for
# historical codes) is joined to the events' 2020 GEOID. A county absent from a
# census gets NA, not 0.
#
# Inputs:
#   IPUMS_1850_1950_FERTILITY_PARQUET_DIR (default D:/ipums_1850_1950_fullcount_fertility), or else
#   IPUMS_1850_1910_FERTILITY_PARQUET_DIR (default D:/ipums_1850_1910_fullcount_fertility) and
#   IPUMS_1940_PARQUET_DIR                (default D:/ipums_1940_fullcount)
#   OCC_CHANNEL_EVENT_UNITS (default <Dropbox>/output/land_grants/andrews_event_county_units_1850_1920.csv)
# Output: OCC_CHANNEL_OUT_DIR (default <Dropbox>/output/occupational_fertility/college_channel)
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/build_professor_children_event_panel.R

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
EVENT_UNITS <- env_or("OCC_CHANNEL_EVENT_UNITS", function()
  file.path(data_root(), "output/land_grants/andrews_event_county_units_1850_1920.csv"))
OUT_DIR <- env_or("OCC_CHANNEL_OUT_DIR",
                  function() file.path(data_root(), "output/occupational_fertility/college_channel"))
COVERAGE <- env_or("OCC_COVERAGE_FACTORS", function()
  file.path(data_root(), "output/occupational_fertility/professor_coverage_factors.csv"))
# County factors (build_professor_county_coverage.R): faculty headcount / coded
# professors where the headcount is larger, else 1. Optional; without the file
# county_faculty_births is NA.
COUNTY_COVERAGE <- env_or("OCC_COUNTY_COVERAGE_FACTORS", function()
  file.path(data_root(), "output/occupational_fertility/professor_county_coverage_factors.csv"))
THREADS <- as.integer(Sys.getenv("OCC_FERTILITY_THREADS", unset = "12"))
MAX_AGE <- 20L
FIRST_YEAR <- 1830L
# Birth years no census sees at ages 0-20 may only be filled here, by
# interpolation between 1910 and 1920; any other gap is an error.
INTERP_WINDOW <- 1911:1919

say <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
sql_path <- function(path) gsub("\\\\", "/", path)
q <- function(con, sql) DBI::dbGetQuery(con, sql)
x <- function(con, sql) invisible(DBI::dbExecute(con, sql))

FATHER_GROUP <- paste(
  "CASE WHEN f.PERNUM IS NULL THEN 'no_father'",
  "WHEN f.OCC1950 BETWEEN 12 AND 29 THEN 'professor'",
  "WHEN f.OCC1950 = 10 THEN 'pres_dean' ELSE 'other' END")

# Children aged 0-20 by county, birth year, age and father's group, plus the
# professor stock, for one census. `src` must expose SERIAL, PERNUM, SEX, AGE,
# POPLOC, OCC1950, county_geoid and a `part` column that SERIAL is unique within.
count_census <- function(con, census_year, src) {
  x(con, "DROP TABLE IF EXISTS p")
  x(con, paste("CREATE TABLE p AS SELECT part, SERIAL, PERNUM, SEX, AGE, POPLOC, OCC1950,",
               "county_geoid FROM", src))
  kids <- q(con, paste(
    "SELECT c.county_geoid, ", census_year, " - c.AGE AS birth_year, c.AGE AS age,",
    FATHER_GROUP, "AS father_group, COUNT(*) AS n",
    # Equalities only: a non-equality term in this LEFT JOIN's ON clause (such
    # as POPLOC > 0) turns DuckDB's hash join into a blockwise nested loop that
    # runs for hours on a full census. POPLOC = 0 never matches a PERNUM anyway.
    "FROM p c LEFT JOIN p f ON f.part = c.part AND f.SERIAL = c.SERIAL",
    "AND f.PERNUM = c.POPLOC",
    "WHERE c.AGE BETWEEN 0 AND", MAX_AGE, "GROUP BY ALL"))
  # The same children nationally, every county included, for the benchmark that
  # turns a transmission rate into a share of all AMWS scientists.
  national <- q(con, paste(
    "SELECT ", census_year, " - c.AGE AS birth_year, c.AGE AS age,",
    FATHER_GROUP, "AS father_group, COUNT(*) AS n",
    "FROM p c LEFT JOIN p f ON f.part = c.part AND f.SERIAL = c.SERIAL",
    "AND f.PERNUM = c.POPLOC",
    "WHERE c.AGE BETWEEN 0 AND", MAX_AGE, "GROUP BY ALL"))
  stock <- q(con, paste(
    "SELECT county_geoid, COUNT(*) FILTER (WHERE OCC1950 BETWEEN 12 AND 29) AS n_prof,",
    "COUNT(*) FILTER (WHERE OCC1950 = 10) AS n_pres_dean, COUNT(*) AS n_men_20_69",
    "FROM p WHERE SEX = 1 AND AGE BETWEEN 20 AND 69 GROUP BY ALL"))
  present <- q(con, "SELECT DISTINCT county_geoid FROM p WHERE county_geoid IS NOT NULL")
  x(con, "DROP TABLE p")
  # data.table's := also works on a census with no child rows.
  lapply(list(kids = kids, stock = stock, present = present, national = national),
         function(d) as.data.table(d)[, census := census_year][])
}

main <- function() {
  started <- Sys.time()
  units <- fread(EVENT_UNITS, colClasses = list(character = "GEOID"))
  units[, GEOID := sprintf("%05d", as.integer(GEOID))]
  event_geoids <- unique(as.integer(units$GEOID))
  say(length(event_geoids), " event counties from ", basename(EVENT_UNITS))
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  # One factor per census (build_professor_coverage.R) puts every census on the
  # total-faculty scale. Applied by the census a birth year is read from.
  stop_if(!file.exists(COVERAGE), "No coverage factors at ", COVERAGE,
          "; run build_professor_coverage.R first")
  factors <- fread(COVERAGE)[, .(census, faculty_factor)]

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  x(con, paste0("SET threads = ", THREADS))
  x(con, "SET memory_limit = '20GB'")
  x(con, "SET preserve_insertion_order = false")

  pieces <- list()
  single <- length(Sys.glob(file.path(DIR_1850_1950, "YEAR=*"))) > 0L
  year_dir <- if (single) DIR_1850_1950 else DIR_1850_1910
  say("Reading censuses from ", year_dir, if (!single) paste(" and", DIR_1940))
  years <- sort(as.integer(sub("^YEAR=", "", basename(list.dirs(year_dir, recursive = FALSE)))))
  for (year in years[!is.na(years)]) {
    say("Census ", year)
    glob <- sql_path(file.path(year_dir, paste0("YEAR=", year), "*.parquet"))
    # SERIAL is unique within a census year.
    src <- sprintf(paste("(SELECT 0 AS part, SERIAL, PERNUM, SEX, AGE, POPLOC, OCC1950,",
                         "CASE WHEN COUNTYICP %% 10 = 0 THEN STATEFIP * 1000 + COUNTYICP // 10 END",
                         "AS county_geoid FROM read_parquet('%s'))"), glob)
    pieces[[as.character(year)]] <- count_census(con, year, src)
  }
  dir_1940_person <- file.path(DIR_1940, "person")
  if (single) {
    # 1940 is a partition of the single extract.
  } else if (dir.exists(dir_1940_person)) {
    say("Census 1940")
    glob <- sql_path(file.path(dir_1940_person, "*", "*.parquet"))
    src <- sprintf(paste("(SELECT CAST(statefip AS INTEGER) AS part, SERIAL, PERNUM, SEX, AGE,",
                         # county_geoid is a zero-padded string in the 1940 Parquet.
                         "POPLOC, OCC1950, TRY_CAST(county_geoid AS INTEGER) AS county_geoid",
                         "FROM read_parquet('%s', hive_partitioning = true))"),
                   glob)
    pieces[["1940"]] <- count_census(con, 1940L, src)
  } else {
    warning("No 1940 full count at ", DIR_1940, "; birth years 1920-1939 stay NA")
  }
  kids <- rbindlist(lapply(pieces, `[[`, "kids"))
  stock <- rbindlist(lapply(pieces, `[[`, "stock"))
  present <- rbindlist(lapply(pieces, `[[`, "present"))
  censuses <- sort(unique(present$census))
  stop_if(!all(censuses %in% factors$census), "No coverage factor for census ",
          paste(setdiff(censuses, factors$census), collapse = ", "))
  # The window ends the year before the latest census (its own year is only
  # partly enumerated); years no census sees at 0-20 must sit in INTERP_WINDOW.
  LAST_YEAR <- max(censuses) - 1L
  seen_by_census <- function(y) any(censuses - y >= 0L & censuses - y <= MAX_AGE)
  unseen <- Filter(Negate(seen_by_census), FIRST_YEAR:LAST_YEAR)
  stop_if(length(setdiff(unseen, INTERP_WINDOW)), "Birth years no census sees at ages 0-",
          MAX_AGE, ": ", paste(setdiff(unseen, INTERP_WINDOW), collapse = ", "))
  interp_years <- intersect(INTERP_WINDOW, unseen)
  say("Birth years ", FIRST_YEAR, "-", LAST_YEAR, "; interpolated: ",
      if (length(interp_years)) paste(range(interp_years), collapse = "-") else "none")

  # Per county x birth year: the census that saw the county and sees the year
  # youngest (age 0-20). A year with no such census stays NA; a county that was
  # enumerated but had no child of that year counts 0.
  grid <- CJ(county_geoid = event_geoids, birth_year = FIRST_YEAR:LAST_YEAR)
  candidates <- merge(grid, present[county_geoid %in% event_geoids], by = "county_geoid",
                      allow.cartesian = TRUE)[, age := census - birth_year][
                        age >= 0L & age <= MAX_AGE]
  chosen <- candidates[, .SD[which.min(age)], by = .(county_geoid, birth_year)]
  kids <- kids[!is.na(county_geoid) & county_geoid %in% event_geoids]
  wide <- dcast(kids, county_geoid + birth_year + census ~ father_group,
                value.var = "n", fun.aggregate = sum, fill = 0)
  for (col in c("professor", "pres_dean", "other", "no_father")) {
    if (!col %in% names(wide)) wide[, (col) := 0]
  }
  chosen <- merge(chosen, wide, by = c("county_geoid", "birth_year", "census"), all.x = TRUE)
  for (col in c("professor", "pres_dean", "other", "no_father")) {
    set(chosen, which(is.na(chosen[[col]])), col, 0)
  }
  chosen[, `:=`(prof_births = professor, prof_births_plus_deans = professor + pres_dean,
                father_births = professor + pres_dean + other,
                all_births = professor + pres_dean + other + no_father,
                source = fifelse(age <= 9L, "c0_9", "c10_20"))]

  # Complete grid; any 1911-1919 gap is interpolated linearly between 1910 and 1920.
  annual <- merge(grid, chosen[, .(county_geoid, birth_year, census, age, source, prof_births,
                                   prof_births_plus_deans, father_births, all_births)],
                  by = c("county_geoid", "birth_year"), all.x = TRUE)
  # Harmonised: children of fathers coded 010-029 (NCES counts all staff,
  # administrators included) times the factor of the census that read the year.
  annual <- merge(annual, factors, by = "census", all.x = TRUE)
  annual[, faculty_births := prof_births_plus_deans * faculty_factor]
  # County-scaled: the county's own factor for the census the year is read from;
  # a county-census with no factor row is not scaled (factor 1).
  if (file.exists(COUNTY_COVERAGE)) {
    county_factors <- fread(COUNTY_COVERAGE)[, .(county_geoid, census, county_factor)]
    annual <- merge(annual, county_factors, by = c("county_geoid", "census"), all.x = TRUE)
    annual[!is.na(census) & is.na(county_factor), county_factor := 1]
    annual[, county_faculty_births := prof_births_plus_deans * county_factor]
  } else {
    warning("No county coverage factors at ", COUNTY_COVERAGE, "; county_faculty_births is NA")
    annual[, `:=`(county_factor = NA_real_, county_faculty_births = NA_real_)]
  }
  setorder(annual, county_geoid, birth_year)
  # Interpolating faculty_births uses its harmonised 1910 and 1920 ends, which
  # carry different factors.
  value_cols <- c("prof_births", "prof_births_plus_deans", "father_births", "all_births",
                  "faculty_births", "county_faculty_births")
  annual[, interpolated := FALSE]
  if (length(interp_years)) {
    annual[, (value_cols) := lapply(.SD, function(v) {
      v0 <- v[birth_year == 1910L]; v1 <- v[birth_year == 1920L]
      if (length(v0) == 1L && length(v1) == 1L && !is.na(v0) && !is.na(v1)) {
        v[birth_year %in% interp_years] <- v0 + (v1 - v0) * (interp_years - 1910L) / 10
      }
      v
    }), by = county_geoid, .SDcols = value_cols]
    annual[birth_year %in% interp_years & !is.na(prof_births),
           `:=`(interpolated = TRUE, source = "interpolated")]
  }
  annual[is.na(source), source := "none"]

  bin <- function(year, alternative) {
    decade <- (year %/% 10L) * 10L
    if (alternative) decade + 10L * (year %% 10L >= 7L) else decade
  }
  decade_panel <- function(alternative) {
    a <- copy(annual)[, decade := bin(birth_year, alternative)]
    out <- a[, c(lapply(.SD, function(v) if (anyNA(v)) NA_real_ else sum(v)),
                 list(years_observed = sum(source %in% c("c0_9", "c10_20")),
                      years_interpolated = sum(interpolated),
                      years_c0_9 = sum(source == "c0_9"),
                      years_in_decade = .N)),
             by = .(county_geoid, decade), .SDcols = value_cols]
    # A decade is only complete if all ten of its years are in the window.
    out[years_in_decade < 10L, (value_cols) := NA_real_]
    out[, GEOID := sprintf("%05d", county_geoid)][, county_geoid := NULL]
    setcolorder(out, c("GEOID", "decade"))
    out[order(GEOID, decade)]
  }
  std <- decade_panel(FALSE)
  alt <- decade_panel(TRUE)

  stock <- stock[!is.na(county_geoid) & county_geoid %in% event_geoids]
  stock <- merge(stock, factors, by = "census")[, n_faculty := (n_prof + n_pres_dean) * faculty_factor]
  stock[, GEOID := sprintf("%05d", county_geoid)][, county_geoid := NULL]
  setcolorder(stock, c("GEOID", "census"))

  say("Writing to ", OUT_DIR)
  fwrite(annual[, GEOID := sprintf("%05d", county_geoid)][, county_geoid := NULL],
         file.path(OUT_DIR, "professor_children_county_year.csv"))
  fwrite(std, file.path(OUT_DIR, "professor_children_county_decade.csv"))
  fwrite(alt, file.path(OUT_DIR, "professor_children_county_decade_alt.csv"))
  fwrite(stock[order(GEOID, census)], file.path(OUT_DIR, "professor_stock_county_census.csv"))
  # National series: every birth year from the census that sees it youngest.
  nat <- rbindlist(lapply(pieces, `[[`, "national"))[birth_year >= FIRST_YEAR & birth_year <= LAST_YEAR]
  nat_best <- nat[, .(age = min(age)), by = birth_year]
  nat <- merge(nat, nat_best, by = c("birth_year", "age"))
  nat_year <- dcast(nat, birth_year + age + census ~ father_group, value.var = "n",
                    fun.aggregate = sum, fill = 0)
  for (col in c("professor", "pres_dean", "other", "no_father")) {
    if (!col %in% names(nat_year)) nat_year[, (col) := 0]
  }
  nat_year <- merge(data.table(birth_year = FIRST_YEAR:LAST_YEAR), nat_year, by = "birth_year", all.x = TRUE)
  # A year some census sees at ages 0-20 but with no child in it counts 0, as for counties.
  seen <- vapply(nat_year$birth_year, seen_by_census, logical(1))
  for (col in c("professor", "pres_dean", "other", "no_father")) {
    set(nat_year, which(seen & is.na(nat_year[[col]])), col, 0)
  }
  nat_year[, `:=`(prof_births = professor, prof_births_plus_deans = professor + pres_dean,
                  all_births = professor + pres_dean + other + no_father)]
  nat_year <- merge(nat_year, factors, by = "census", all.x = TRUE)
  nat_year[, faculty_births := (professor + pres_dean) * faculty_factor]
  setorder(nat_year, birth_year)
  for (col in c("prof_births", "prof_births_plus_deans", "all_births", "faculty_births")) {
    v0 <- nat_year[birth_year == 1910L][[col]]; v1 <- nat_year[birth_year == 1920L][[col]]
    if (length(interp_years) && length(v0) && length(v1) && !is.na(v0) && !is.na(v1)) {
      nat_year[birth_year %in% interp_years, (col) := v0 + (v1 - v0) * (birth_year - 1910L) / 10]
    }
  }
  nat_year[, interpolated := birth_year %in% interp_years]
  nat_decades <- rbindlist(lapply(c(FALSE, TRUE), function(alternative) {
    nat_year[, .(prof_births = if (.N == 10L && !anyNA(prof_births)) sum(prof_births) else NA_real_,
                 prof_births_plus_deans = if (.N == 10L && !anyNA(prof_births_plus_deans))
                   sum(prof_births_plus_deans) else NA_real_,
                 faculty_births = if (.N == 10L && !anyNA(faculty_births)) sum(faculty_births) else NA_real_,
                 all_births = if (.N == 10L && !anyNA(all_births)) sum(all_births) else NA_real_,
                 years_interpolated = sum(interpolated)),
             by = .(decade = bin(birth_year, alternative))][
      , timing := if (alternative) "alternative_decade" else "standard_decade"]
  }))
  fwrite(nat_year[, .(birth_year, census, age, faculty_factor, prof_births, prof_births_plus_deans,
                      faculty_births, all_births, interpolated)],
         file.path(OUT_DIR, "national_professor_children_year.csv"))
  fwrite(nat_decades[order(timing, decade)], file.path(OUT_DIR, "national_professor_children_decade.csv"))

  coverage <- std[, .(counties_with_data = sum(!is.na(prof_births)),
                      prof_births = sum(prof_births, na.rm = TRUE),
                      faculty_births = sum(faculty_births, na.rm = TRUE),
                      all_births = sum(all_births, na.rm = TRUE)), by = decade][order(decade)]
  fwrite(coverage, file.path(OUT_DIR, "coverage_by_decade.csv"))
  jsonlite::write_json(list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    censuses = censuses, event_counties = length(event_geoids),
    source = if (single) DIR_1850_1950 else c(DIR_1850_1910, DIR_1940),
    birth_years = c(FIRST_YEAR, LAST_YEAR), interpolated_years = interp_years,
    max_child_age = MAX_AGE, coverage = coverage,
    elapsed_minutes = round(as.numeric(difftime(Sys.time(), started, units = "mins")), 2)),
    file.path(OUT_DIR, "build_manifest.json"), pretty = TRUE, auto_unbox = TRUE, digits = NA)
  say("Done: ", nrow(std), " county x decade rows (standard), ", nrow(alt), " (alternative)")
}

main()
