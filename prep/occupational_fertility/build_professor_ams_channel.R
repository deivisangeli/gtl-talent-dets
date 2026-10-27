# Predicted faculty children per event county and birth decade: the census
# faculty stock by age times a birth schedule per professor, instead of
# co-resident children read from the census that sees each cohort youngest.
# Two schedules feed it:
#   AMS 1956   build_professor_birth_schedule.R (default; prefix "ams")
#   census     build_professor_census_fertility_schedule.R: wives' CHSURV
#              1900-1910 and CHBORN 1940 (OCC_BIRTH_SCHEDULE=..._census.csv,
#              OCC_CHANNEL_PREFIX=chsurv)
#   regional   build_professor_regional_fertility_schedule.R: own co-resident
#              children by region x urban/rural and census
#              (OCC_BIRTH_SCHEDULE=..._regional.csv, OCC_CHANNEL_PREFIX=regional)
#
#   forward(county, D)  = sum_a stock(county, D, a)      x births at ages a .. a+9
#   backward(county, D) = sum_a stock(county, D + 10, a) x births at ages a-10 .. a-1
#   <prefix>_births(county, D) = mean of the two where both censuses exist
#
# stock = men coded OCC1950 010-029 aged 15-90 in the county at a census;
# births per professor by age are the schedule's single-year rates
# (births_at_age_<rule> columns; the first is the headline, others are stored
# with a _<rule> suffix). A schedule with a `census` column gives each census
# its own rates; one without applies the same rates to every census. A schedule
# with a `group` column as well gives each county x census its group's rates,
# the group read from OCC_COUNTY_GROUPS (county_urban_class.csv); stock outside
# any group takes the schedule's "national" group. The
# forward view counts fathers present when the decade opens and misses faculty
# who arrive during it; the backward view counts fathers present when it closes
# (as the census-observed channel does). Their mean approximates the faculty
# present at the births. The alternative binning (years ending 7-6) shifts
# both windows by three years. Decades bracketed by one census only use that
# one (1880s: 1880 forward; 1890s: 1900 backward; 1950s: 1950 forward); the
# 1830s repeat the 1840s (1850 backward), flagged. A county no census
# enumerated is NA, not 0.
#
# Three versions, as for the census-observed channel: <prefix>_births (unscaled),
# <prefix>_faculty_births (x the census's national faculty_factor,
# build_professor_coverage.R) and <prefix>_county_faculty_births (x the
# county's own factor, build_professor_county_coverage.R; 1 where it has none).
#
# Inputs:
#   IPUMS_1850_1950_FERTILITY_PARQUET_DIR (default D:/ipums_1850_1950_fullcount_fertility)
#   OCC_BIRTH_SCHEDULE (default <OCC_CHANNEL_OUT_DIR>/professor_birth_schedule.csv)
#   OCC_CHANNEL_PREFIX (default ams)
#   OCC_COUNTY_GROUPS (default <OCC_CHANNEL_OUT_DIR>/county_urban_class.csv; group-keyed schedules only)
#   OCC_CHANNEL_EVENT_UNITS, OCC_COVERAGE_FACTORS, OCC_CHANNEL_OUT_DIR (as the children panel)
# Output (in OCC_CHANNEL_OUT_DIR):
#   <prefix>_children_county_decade.csv, <prefix>_children_county_decade_alt.csv,
#   <prefix>_national_decade.csv, <prefix>_vs_observed_decade.csv, <prefix>_channel_manifest.json
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/build_professor_ams_channel.R

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
EVENT_UNITS <- env_or("OCC_CHANNEL_EVENT_UNITS", function()
  file.path(data_root(), "output/land_grants/andrews_event_county_units_1850_1920.csv"))
OUT_DIR <- env_or("OCC_CHANNEL_OUT_DIR",
                  function() file.path(data_root(), "output/occupational_fertility/college_channel"))
COVERAGE <- env_or("OCC_COVERAGE_FACTORS", function()
  file.path(data_root(), "output/occupational_fertility/professor_coverage_factors.csv"))
SCHEDULE <- env_or("OCC_BIRTH_SCHEDULE", function() file.path(OUT_DIR, "professor_birth_schedule.csv"))
PREFIX <- env_or("OCC_CHANNEL_PREFIX", function() "ams")
# County factors (build_professor_county_coverage.R); optional, see main().
COUNTY_COVERAGE <- env_or("OCC_COUNTY_COVERAGE_FACTORS", function()
  file.path(data_root(), "output/occupational_fertility/professor_county_coverage_factors.csv"))
COUNTY_GROUPS <- env_or("OCC_COUNTY_GROUPS", function() file.path(OUT_DIR, "county_urban_class.csv"))
DECADES <- seq(1830L, 1950L, 10L)
WINDOWS <- list(std_fwd = c(0L, 9L), std_back = c(-10L, -1L), alt_fwd = c(-3L, 6L), alt_back = c(-13L, -4L))

say <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
sql_path <- function(path) gsub("\\\\", "/", path)

main <- function() {
  stop_if(!file.exists(SCHEDULE), "No birth schedule at ", SCHEDULE)
  schedule <- fread(SCHEDULE)
  rules <- sub("^births_at_age_", "", grep("^births_at_age_", names(schedule), value = TRUE))
  stop_if(!length(rules), "The schedule has no births_at_age_* column: ", SCHEDULE)
  by_census <- "census" %in% names(schedule)
  by_group <- "group" %in% names(schedule)
  stop_if(by_group && !by_census, "A group-keyed schedule needs a census column")
  say("Schedule ", basename(SCHEDULE), " (", if (by_group) "by census and group" else if (by_census) "by census" else "every census",
      "), rules: ", paste(rules, collapse = ", "), "; prefix ", PREFIX)
  factors <- fread(COVERAGE)[, .(census, faculty_factor)]
  units <- fread(EVENT_UNITS, colClasses = list(character = "GEOID"))
  event_geoids <- unique(as.integer(units$GEOID))
  years <- sort(as.integer(sub("^YEAR=", "", basename(Sys.glob(file.path(PARQUET, "YEAR=*"))))))
  stop_if(!length(years), "No YEAR partitions under ", PARQUET)
  stop_if(!all(years %in% factors$census), "No coverage factor for census ",
          paste(setdiff(years, factors$census), collapse = ", "))
  stop_if(by_census && !all(years %in% schedule$census), "The schedule has no rates for census ",
          paste(setdiff(years, schedule$census), collapse = ", "))

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads = 12")
  pieces <- lapply(years, function(year) {
    glob <- sql_path(file.path(PARQUET, paste0("YEAR=", year), "*.parquet"))
    src <- sprintf(paste("(SELECT SEX, AGE, OCC1950,",
                         "CASE WHEN COUNTYICP %% 10 = 0 THEN STATEFIP * 1000 + COUNTYICP // 10 END",
                         "AS county_geoid FROM read_parquet('%s'))"), glob)
    stock <- as.data.table(DBI::dbGetQuery(con, paste(
      "SELECT county_geoid, AGE AS age, COUNT(*) AS n FROM", src,
      "WHERE SEX = 1 AND OCC1950 BETWEEN 10 AND 29 AND AGE BETWEEN 15 AND 90 GROUP BY ALL")))
    present <- as.data.table(DBI::dbGetQuery(con, paste(
      "SELECT DISTINCT county_geoid FROM", src, "WHERE county_geoid IS NOT NULL")))
    say("Census ", year, ": ", stock[, sum(n)], " men coded 010-029")
    list(stock = stock[, census := year], present = present[, census := year])
  })
  stock <- rbindlist(lapply(pieces, `[[`, "stock"))
  present <- rbindlist(lapply(pieces, `[[`, "present"))

  # Window sums of the single-year rates: births at the father's ages a+lo to
  # a+hi per professor aged a at the census, per census when the schedule has one.
  window_table <- function(s) {
    out <- data.table(age = s$age)
    for (w in names(WINDOWS)) for (rule in rules) {
      lo <- WINDOWS[[w]][1]; hi <- WINDOWS[[w]][2]
      r <- s[[paste0("births_at_age_", rule)]]
      out[, (paste(w, rule, sep = "_")) := vapply(age, function(a) sum(r[s$age >= a + lo & s$age <= a + hi]), 0)]
    }
    out
  }
  if (by_group) {
    # Group-keyed rates: each county x census takes its group's rates
    # (county_urban_class.csv); stock without a group takes "national".
    groups <- fread(COUNTY_GROUPS)[, .(county_geoid, census, group)]
    stock <- merge(stock, groups, by = c("county_geoid", "census"), all.x = TRUE)
    stock[is.na(group), group := "national"]
    stop_if(!all(unique(stock$group) %in% schedule$group), "The schedule has no rates for group ",
            paste(setdiff(unique(stock$group), schedule$group), collapse = ", "))
    wtab <- rbindlist(lapply(split(schedule, by = c("census", "group")), function(s)
      window_table(s)[, `:=`(census = s$census[1], group = s$group[1])]))
    stock <- merge(stock, wtab, by = c("census", "group", "age"))
  } else if (by_census) {
    wtab <- rbindlist(lapply(split(schedule, by = "census"), function(s)
      window_table(s)[, census := s$census[1]]))
    stock <- merge(stock, wtab, by = c("census", "age"))
  } else {
    wtab <- window_table(schedule)
    stock <- merge(stock, wtab, by = "age")
  }
  wcols <- setdiff(names(wtab), c("age", "census", "group"))
  sum_by <- function(s) s[, c(lapply(.SD, function(w) sum(n * w)), list(stock = sum(n))),
                          by = .(county_geoid, census), .SDcols = wcols]
  county_census <- sum_by(stock[!is.na(county_geoid)])
  national_census <- sum_by(copy(stock)[, county_geoid := 0L])

  # Every enumerated event county gets a row per census, 0 where it had no faculty.
  grid <- present[county_geoid %in% event_geoids]
  county_census <- merge(grid, county_census, by = c("county_geoid", "census"), all.x = TRUE)
  for (col in c(wcols, "stock")) set(county_census, which(is.na(county_census[[col]])), col, 0)

  # Decade D is seen forward from the census that opens it (fathers present at
  # the start) and backward from the census that closes it (fathers present at
  # the end, births over the previous ten years). Where both exist the two are
  # averaged, approximating the faculty present when the children were born;
  # each is scaled by its own census factor first. The 1830s, which no census
  # held brackets, repeat the 1840s (flagged).
  births <- paste0(PREFIX, "_births"); faculty_births <- paste0(PREFIX, "_faculty_births")
  county_births <- paste0(PREFIX, "_county_faculty_births")
  # County-scaled version (headline rule): each census's prediction times the
  # county's own factor, 1 where it has none. Without the file the column is NA.
  county_factors <- if (file.exists(COUNTY_COVERAGE)) {
    fread(COUNTY_COVERAGE)[, .(county_geoid, census, county_factor)]
  } else {
    warning("No county coverage factors at ", COUNTY_COVERAGE, "; ", county_births, " is NA")
    NULL
  }
  to_decades <- function(cc, binning) {
    cc <- merge(cc, factors, by = "census")
    if (is.null(county_factors)) {
      cc[, county_factor := NA_real_]
    } else {
      cc <- merge(cc, county_factors, by = c("county_geoid", "census"), all.x = TRUE)
      cc[is.na(county_factor), county_factor := 1]
    }
    per_rule <- lapply(rules, function(rule) {
      ends <- rbind(
        cc[, .(county_geoid, decade = census, end = "start",
               value = get(paste(binning, "fwd", rule, sep = "_")), faculty_factor, county_factor)],
        cc[, .(county_geoid, decade = census - 10L, end = "end",
               value = get(paste(binning, "back", rule, sep = "_")), faculty_factor, county_factor)])
      ends[, .(raw = mean(value), scaled = mean(value * faculty_factor),
               county_scaled = mean(value * county_factor),
               source = if (.N == 2L) "start_and_end" else paste0(end, "_only")),
           by = .(county_geoid, decade)][, rule := rule]
    })
    long <- rbindlist(per_rule)
    wide <- dcast(long, county_geoid + decade ~ rule, value.var = c("raw", "scaled", "county_scaled", "source"))
    out <- wide[, .(county_geoid, decade, source = get(paste0("source_", rules[1])))]
    for (rule in rules) {
      suffix <- if (rule == rules[1]) "" else paste0("_", rule)
      out[, (paste0(births, suffix)) := wide[[paste0("raw_", rule)]]]
      out[, (paste0(faculty_births, suffix)) := wide[[paste0("scaled_", rule)]]]
    }
    out[, (county_births) := wide[[paste0("county_scaled_", rules[1])]]]
    back <- copy(out[decade == 1840L])[, `:=`(decade = 1830L, source = "repeat_1840s")]
    rbind(out[decade != 1830L], back)[decade %in% DECADES][order(county_geoid, decade)]
  }
  shape <- function(d) {
    d[, GEOID := sprintf("%05d", county_geoid)][, county_geoid := NULL]
    setcolorder(d, c("GEOID", "decade"))
    d[]
  }
  std <- shape(to_decades(county_census, "std"))
  alt <- shape(to_decades(county_census, "alt"))
  national_dec <- to_decades(national_census, "std")
  stop_if(std[, anyDuplicated(paste(GEOID, decade))] > 0, "Duplicate county-decade rows")

  # Plausibility: predicted against census-observed faculty children (010-029, both
  # unscaled and scaled), event counties and nation, standard decades.
  observed <- fread(file.path(OUT_DIR, "professor_children_county_decade.csv"),
                    colClasses = list(character = "GEOID"))
  cmp <- merge(std, observed[, .(GEOID, decade, observed_births = prof_births_plus_deans,
                                 observed_faculty_births = faculty_births)],
               by = c("GEOID", "decade"))
  cmp <- cmp[!is.na(observed_births)]
  by_decade <- cmp[, .(counties = .N, predicted = sum(get(births)), observed = sum(observed_births),
                       predicted_scaled = sum(get(faculty_births)),
                       observed_scaled = sum(observed_faculty_births),
                       county_corr = suppressWarnings(cor(get(births), observed_births))), by = decade]
  nat_obs <- fread(file.path(OUT_DIR, "national_professor_children_decade.csv"))[timing == "standard_decade"]
  nat <- national_dec[, .(decade, national_predicted = get(births),
                          national_predicted_scaled = get(faculty_births))]
  by_decade <- merge(by_decade, nat, by = "decade", all.x = TRUE)
  by_decade <- merge(by_decade, nat_obs[, .(decade, national_observed = prof_births_plus_deans,
                                            national_observed_scaled = faculty_births)],
                     by = "decade", all.x = TRUE)
  by_decade[, `:=`(ratio_event_counties = predicted / observed,
                   ratio_national = national_predicted / national_observed)]

  say("Writing to ", OUT_DIR)
  out_file <- function(name) file.path(OUT_DIR, paste0(PREFIX, "_", name))
  fwrite(std, out_file("children_county_decade.csv"))
  fwrite(alt, out_file("children_county_decade_alt.csv"))
  fwrite(national_dec[, county_geoid := NULL][order(decade)], out_file("national_decade.csv"))
  fwrite(by_decade[order(decade)], out_file("vs_observed_decade.csv"))
  jsonlite::write_json(list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), prefix = PREFIX, censuses = years,
    decades = DECADES, event_counties = length(event_geoids), schedule = SCHEDULE,
    schedule_by_census = by_census, schedule_by_group = by_group, rules = rules,
    pooled_correlation_with_observed = cmp[, cor(get(births), observed_births)],
    decade_rule = paste("mean of the forward view from the opening census and the backward view from the",
                        "closing census; one end only for the 1880s, 1890s and 1950s; 1830s repeat the 1840s")),
    out_file("channel_manifest.json"), pretty = TRUE, auto_unbox = TRUE, digits = NA)
  print(by_decade[order(decade), .(decade, counties, predicted = round(predicted), observed = round(observed),
                                   ratio_event_counties = round(ratio_event_counties, 2),
                                   ratio_national = round(ratio_national, 2),
                                   county_corr = round(county_corr, 2))])
}

main()
