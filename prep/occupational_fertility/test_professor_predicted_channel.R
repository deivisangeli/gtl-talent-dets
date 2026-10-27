# Synthetic-fixture checks for build_professor_ams_channel.R with a
# group-keyed birth schedule (the regional channel).
#
# Tiny 1900/1910 Parquet tree: county A (36001, urban group) and county B
# (36003, rural group) each hold one professor aged 40 in both censuses, and a
# third professor sits in a county with a historical ICPSR code (COUNTYICP 15),
# which has no county_geoid and must take the "national" group. Rates are flat
# by age: 0.1 (urban), 0.2 (rural), 0.15 (national). A ten-year window then
# holds 1.0, 2.0 and 1.5 births, worked out by hand below.
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/test_professor_predicted_channel.R

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
BUILD <- file.path(repo_root, "prep/occupational_fertility/build_professor_ams_channel.R")

passed <- 0L; failed <- 0L
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) {
    passed <<- passed + 1L
    cat("  ok   ", label, "\n")
  } else {
    failed <<- failed + 1L
    detail <- paste(format(detail), collapse = ",")
    cat("  FAIL ", label, if (nzchar(detail)) paste0(" -- ", detail), "\n")
  }
}

root <- file.path(tempdir(), paste0("prof_predicted_fixture_", Sys.getpid()))
dir.create(root, recursive = TRUE, showWarnings = FALSE)
pq_dir <- file.path(root, "pq"); out_dir <- file.path(root, "out")
dir.create(out_dir, showWarnings = FALSE)

person <- function(year, county, occ = 29L, age = 40L, sex = 1L)
  data.table(YEAR = year, STATEFIP = 36L, COUNTYICP = county, SEX = sex, AGE = age, OCC1950 = occ)
people <- rbindlist(lapply(c(1900L, 1910L), function(y) rbind(
  person(y, 10L), person(y, 30L), person(y, 15L),
  # Non-professors and a female professor: never stock.
  person(y, 10L, occ = 100L), person(y, 30L, sex = 2L))))
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "people", people)
invisible(DBI::dbExecute(con, sprintf("COPY people TO '%s' (FORMAT PARQUET, PARTITION_BY (YEAR))",
                                      gsub("\\\\", "/", pq_dir))))
DBI::dbDisconnect(con, shutdown = TRUE)

rates <- c(Northeast_urban = 0.1, Northeast_rural = 0.2, national = 0.15)
schedule <- CJ(census = c(1900L, 1910L), group = names(rates), age = 15:90)
schedule[, births_at_age_nchild := rates[group]]
schedule_path <- file.path(root, "schedule.csv"); fwrite(schedule, schedule_path)
groups_path <- file.path(root, "groups.csv")
fwrite(data.table(county_geoid = c(36001L, 36003L), census = rep(c(1900L, 1910L), each = 2),
                  group = c("Northeast_urban", "Northeast_rural")), groups_path)
units_path <- file.path(root, "units.csv")
fwrite(data.table(event_id = 1L, GEOID = c("36001", "36003"), sample_role = c("treated", "runner_up")),
       units_path)
coverage_path <- file.path(root, "coverage.csv")
fwrite(data.table(census = c(1900L, 1910L), faculty_factor = c(2, 3)), coverage_path)
county_coverage_path <- file.path(root, "county_coverage.csv")
fwrite(data.table(county_geoid = 36001L, census = 1910L, county_factor = 4), county_coverage_path)
# The builder compares with the census-observed channel; any values will do.
fwrite(CJ(GEOID = c("36001", "36003"), decade = c(1890L, 1900L, 1910L))[
  , `:=`(prof_births_plus_deans = 1, faculty_births = 2)], file.path(out_dir, "professor_children_county_decade.csv"))
fwrite(data.table(timing = "standard_decade", decade = c(1890L, 1900L, 1910L),
                  prof_births_plus_deans = 3, faculty_births = 6),
       file.path(out_dir, "national_professor_children_decade.csv"))

run_build <- function(...) {
  Sys.setenv(IPUMS_1850_1950_FERTILITY_PARQUET_DIR = pq_dir, OCC_CHANNEL_OUT_DIR = out_dir,
             OCC_CHANNEL_EVENT_UNITS = units_path, OCC_COVERAGE_FACTORS = coverage_path,
             OCC_COUNTY_COVERAGE_FACTORS = county_coverage_path, OCC_COUNTY_GROUPS = groups_path,
             OCC_BIRTH_SCHEDULE = schedule_path, OCC_CHANNEL_PREFIX = "regional", ...)
  suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(BUILD), stdout = TRUE, stderr = TRUE))
}
out <- run_build()
ok("build exits cleanly", is.null(attr(out, "status")), tail(out, 5))
std <- fread(file.path(out_dir, "regional_children_county_decade.csv"), colClasses = list(character = "GEOID"))
nat <- fread(file.path(out_dir, "regional_national_decade.csv"))
val <- function(geoid, dec, col = "regional_births") std[GEOID == geoid & decade == dec][[col]]

# 1900s: forward from 1900 (ages 40-49) and backward from 1910 (ages 30-39),
# ten years of a flat rate either way.
ok("urban county takes the urban rate (1.0 in the 1900s)", isTRUE(all.equal(val("36001", 1900L), 1.0)),
   val("36001", 1900L))
ok("rural county takes the rural rate (2.0 in the 1900s)", isTRUE(all.equal(val("36003", 1900L), 2.0)),
   val("36003", 1900L))
ok("same stock in two groups: births in the ratio of the group rates",
   isTRUE(all.equal(val("36003", 1890L) / val("36001", 1890L), 2)))
# Nation: 1.0 + 2.0 + 1.5 for the professor without a county code.
ok("stock without a county code takes the national rate (nation 4.5 in the 1900s)",
   isTRUE(all.equal(nat[decade == 1900L, regional_births], 4.5)), nat[decade == 1900L, regional_births])
# Scaled: forward x 2 (1900 factor), backward x 3 (1910 factor), averaged.
ok("national scaling: urban 1900s = (1 x 2 + 1 x 3) / 2 = 2.5",
   isTRUE(all.equal(val("36001", 1900L, "regional_faculty_births"), 2.5)))
# County: A has factor 4 in 1910 only; 1900 unscaled.
ok("county scaling: urban 1900s = (1 x 1 + 1 x 4) / 2 = 2.5, rural unscaled 2.0",
   isTRUE(all.equal(val("36001", 1900L, "regional_county_faculty_births"), 2.5)) &&
     isTRUE(all.equal(val("36003", 1900L, "regional_county_faculty_births"), 2.0)))
ok("one-ended decades: 1890s backward from 1900 only, 1910s forward from 1910 only",
   std[GEOID == "36001" & decade == 1890L, source] == "end_only" &&
     std[GEOID == "36001" & decade == 1910L, source] == "start_only")

# A group missing from the schedule is an error, not a silent zero.
fwrite(schedule[group != "Northeast_rural"], file.path(root, "schedule_missing.csv"))
out_missing <- run_build(OCC_BIRTH_SCHEDULE = file.path(root, "schedule_missing.csv"))
ok("a county group with no rates stops the build",
   !is.null(attr(out_missing, "status")) && any(grepl("no rates for group Northeast_rural", out_missing)))

unlink(root, recursive = TRUE)
cat(sprintf("\n%d passed, %d failed\n", passed, failed))
if (failed > 0L) quit(status = 1L)
