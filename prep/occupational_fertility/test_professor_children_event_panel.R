# Synthetic-fixture checks for build_professor_children_event_panel.R.
#
# Tiny 1880/1900/1910 and 1940 Parquet trees are written to a temporary
# directory and the build is run against them. Every expected value is worked
# out by hand in the comments. The cases are the ones the panel is most exposed
# to: reading a birth year from the census that sees it youngest, telling a
# county that was not enumerated (NA) from one with no births (0), the
# 1911-1919 interpolation, both decade binnings, and the 1940 join staying
# inside a state when SERIAL repeats across states.
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/test_professor_children_event_panel.R

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
BUILD <- file.path(repo_root, "prep/occupational_fertility/build_professor_children_event_panel.R")
COVERAGE <- file.path(repo_root, "prep/occupational_fertility/build_professor_coverage.R")

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

# --------------------------------------------------------------------------- #
# Fixture. County A = 36001 (COUNTYICP 10), county B = 36003 (COUNTYICP 30),
# B is first enumerated in 1900.
# --------------------------------------------------------------------------- #
p <- function(year, county, serial, pernum, sex, age, occ = 999L, poploc = 0L) {
  data.table(YEAR = year, STATEFIP = 36L, COUNTYICP = county, SERIAL = serial,
             PERNUM = pernum, SEX = sex, AGE = age, POPLOC = poploc, OCC1950 = occ)
}
early <- rbind(
  # 1880, A: professor with a newborn (birth year 1880).
  p(1880L, 10L, 1L, 1L, 1L, 40L, 29L), p(1880L, 10L, 1L, 2L, 1L, 0L, poploc = 1L),
  # 1900, A: professor with children aged 0, 5, 15 (1900, 1895, 1885); a farmer's
  # child aged 3 (1897); a child of 2 with no father present (1898).
  p(1900L, 10L, 2L, 1L, 1L, 45L, 29L),
  p(1900L, 10L, 2L, 2L, 2L, 0L, poploc = 1L), p(1900L, 10L, 2L, 3L, 1L, 5L, poploc = 1L),
  p(1900L, 10L, 2L, 4L, 2L, 15L, poploc = 1L),
  p(1900L, 10L, 3L, 1L, 1L, 35L, 100L), p(1900L, 10L, 3L, 2L, 1L, 3L, poploc = 1L),
  p(1900L, 10L, 4L, 1L, 2L, 2L),
  # 1900, B: a farmer's child aged 4 (1896).
  p(1900L, 30L, 5L, 1L, 1L, 30L, 100L), p(1900L, 30L, 5L, 2L, 1L, 4L, poploc = 1L),
  # 1910, A: professor with children aged 0, 8 and 10 (1910, 1902, 1900). The
  # 10-year-old must be ignored: 1900 already saw birth year 1900 at age 0.
  p(1910L, 10L, 6L, 1L, 1L, 50L, 29L),
  p(1910L, 10L, 6L, 2L, 1L, 0L, poploc = 1L), p(1910L, 10L, 6L, 3L, 2L, 8L, poploc = 1L),
  p(1910L, 10L, 6L, 4L, 2L, 10L, poploc = 1L),
  # 1910, B: a man with no children.
  p(1910L, 30L, 7L, 1L, 1L, 30L, 100L))
q40 <- function(state, county, serial, pernum, sex, age, occ = 999L, poploc = 0L) {
  data.table(statefip = state, SERIAL = serial, PERNUM = pernum, SEX = sex, AGE = age,
             POPLOC = poploc, OCC1950 = occ, county_geoid = county)
}
late <- rbind(
  # 1940, A: professor with two 20-year-olds (1920), one 5-year-old (1935) and a
  # newborn (1940, outside the window).
  q40(36L, 36001L, 1L, 1L, 1L, 45L, 29L),
  q40(36L, 36001L, 1L, 2L, 1L, 20L, poploc = 1L), q40(36L, 36001L, 1L, 3L, 2L, 20L, poploc = 1L),
  q40(36L, 36001L, 1L, 4L, 1L, 5L, poploc = 1L), q40(36L, 36001L, 1L, 5L, 1L, 0L, poploc = 1L),
  # 1940, A: SERIAL 9 is a farmer's household in state 36 but a professor's in
  # state 17. The 6-year-old (1934) belongs to the farmer.
  q40(36L, 36001L, 9L, 1L, 1L, 40L, 100L), q40(36L, 36001L, 9L, 2L, 1L, 6L, poploc = 1L),
  q40(17L, 17019L, 9L, 1L, 1L, 40L, 29L))

root <- file.path(tempdir(), paste0("prof_children_fixture_", Sys.getpid()))
dir.create(root, recursive = TRUE, showWarnings = FALSE)
dir_early <- file.path(root, "early")
dir_1940 <- file.path(root, "fc1940")
out_dir <- file.path(root, "out")
units_path <- file.path(root, "units.csv")
dir.create(dir_1940, showWarnings = FALSE)
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "early", early)
duckdb::duckdb_register(con, "late", late)
invisible(DBI::dbExecute(con, sprintf(
  "COPY early TO '%s' (FORMAT PARQUET, PARTITION_BY (YEAR))", gsub("\\\\", "/", dir_early))))
invisible(DBI::dbExecute(con, sprintf(
  "COPY late TO '%s' (FORMAT PARQUET, PARTITION_BY (statefip))",
  gsub("\\\\", "/", file.path(dir_1940, "person")))))
DBI::dbDisconnect(con, shutdown = TRUE)
fwrite(data.table(event_id = 1L, GEOID = c("36001", "36003"),
                  sample_role = c("treated", "runner_up")), units_path)
# Hand-set coverage factors: different in every census, so the tests can see
# which census a year was read from.
coverage_path <- file.path(root, "coverage.csv")
fwrite(data.table(census = c(1880L, 1900L, 1910L, 1940L), faculty_factor = c(2, 4, 3, 1.5)),
       coverage_path)

# County factors: A (36001) has a headcount above its coded professors in 1900
# (factor 2); 1910 has no row, so it is not scaled.
county_coverage_path <- file.path(root, "county_coverage.csv")
fwrite(data.table(county_geoid = 36001L, census = 1900L, county_factor = 2), county_coverage_path)
# The 1850-1950 tree is pointed at a path that does not exist, so this run
# exercises the 1850-1910 + 1940 fallback even where the real tree is present.
Sys.setenv(OCC_COUNTY_COVERAGE_FACTORS = county_coverage_path,
           IPUMS_1850_1950_FERTILITY_PARQUET_DIR = file.path(root, "no_1850_1950"),
           IPUMS_1850_1910_FERTILITY_PARQUET_DIR = dir_early, IPUMS_1940_PARQUET_DIR = dir_1940,
           OCC_CHANNEL_EVENT_UNITS = units_path, OCC_CHANNEL_OUT_DIR = out_dir,
           OCC_COVERAGE_FACTORS = coverage_path,
           OCC_FERTILITY_THREADS = "2")
out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(BUILD),
                                stdout = TRUE, stderr = TRUE))
ok("build exits cleanly", is.null(attr(out, "status")), tail(out, 5))

yr <- fread(file.path(out_dir, "professor_children_county_year.csv"),
            colClasses = list(character = "GEOID"))
std <- fread(file.path(out_dir, "professor_children_county_decade.csv"),
             colClasses = list(character = "GEOID"))
alt <- fread(file.path(out_dir, "professor_children_county_decade_alt.csv"),
             colClasses = list(character = "GEOID"))
stock <- fread(file.path(out_dir, "professor_stock_county_census.csv"),
               colClasses = list(character = "GEOID"))
y <- function(geoid, year, col = "prof_births") yr[GEOID == geoid & birth_year == year][[col]]
d <- function(tab, geoid, dec, col = "prof_births") tab[GEOID == geoid & decade == dec][[col]]

cat("\nBirth year from the youngest census\n")
ok("1880 newborn read from 1880", y("36001", 1880) == 1 && y("36001", 1880, "census") == 1880)
ok("1885 read from 1900 at age 15, flagged c10_20",
   y("36001", 1885) == 1 && y("36001", 1885, "source") == "c10_20")
ok("birth year 1900 read from 1900 at age 0, not 1910 at age 10",
   y("36001", 1900) == 1 && y("36001", 1900, "census") == 1900)
ok("farmer's child counts for fathers, not professors",
   y("36001", 1897) == 0 && y("36001", 1897, "father_births") == 1)
ok("child with no father counts only in all births",
   y("36001", 1898, "father_births") == 0 && y("36001", 1898, "all_births") == 1)
ok("enumerated county-year with no child is 0", y("36001", 1903) == 0)
ok("1940 children read at their age (1920 x2, 1935)", y("36001", 1920) == 2 && y("36001", 1935) == 1)
ok("1940 join stays inside the state when SERIAL repeats",
   y("36001", 1934) == 0 && y("36001", 1934, "father_births") == 1)

cat("\nNA versus 0, and the 1910s gap\n")
ok("county not yet enumerated is NA", is.na(y("36003", 1879)))
ok("enumerated later at age 20 is 0, not NA", y("36003", 1880) == 0)
ok("1915 interpolated between 1910 (1) and 1920 (2)",
   isTRUE(all.equal(y("36001", 1915), 1.5)) && y("36001", 1915, "source") == "interpolated")
ok("no interpolation without both ends (B absent in 1940)", is.na(y("36003", 1915)))

cat("\nDecade binning\n")
ok("standard 1880s = 1880 + 1885", d(std, "36001", 1880) == 2)
ok("standard 1900s = 1900 + 1902", d(std, "36001", 1900) == 2)
ok("standard 1910s = 1 + interpolated 1.1..1.9", isTRUE(all.equal(d(std, "36001", 1910), 14.5)))
ok("standard 1920s and 1930s", d(std, "36001", 1920) == 2 && d(std, "36001", 1930) == 1)
ok("alternative 1890 decade (1887-1896) holds 1895 only", d(alt, "36001", 1890) == 1)
ok("alternative 1900 decade (1897-1906) = 1900 + 1902", d(alt, "36001", 1900) == 2)
ok("decade with a missing year is NA", is.na(d(std, "36003", 1870)))
ok("incomplete edge decade is NA (alternative 1940: 1937-1946)",
   nrow(alt[GEOID == "36001" & decade == 1940]) == 0 || is.na(d(alt, "36001", 1940)))
ok("county B 1880s is 0, not NA", d(std, "36003", 1880) == 0)

cat("\nNational series\n")
nat <- fread(file.path(out_dir, "national_professor_children_year.csv"))
natd <- fread(file.path(out_dir, "national_professor_children_decade.csv"))
ok("national series uses the same youngest-census rule",
   nat[birth_year == 1900, prof_births] == 1 && nat[birth_year == 1920, prof_births] == 2 &&
     nat[birth_year == 1934, prof_births] == 0)
ok("national standard 1900s decade matches county A (only county with professors)",
   natd[timing == "standard_decade" & decade == 1900, prof_births] == 2)

cat("\nHarmonised faculty measure\n")
ok("factor applies by source census: 1900 read x4, 1902 (from 1910) x3",
   y("36001", 1900, "faculty_births") == 4 && y("36001", 1902, "faculty_births") == 3)
ok("one decade mixes factors: standard 1900s faculty = 4 + 3, raw stays 2",
   d(std, "36001", 1900, "faculty_births") == 7 && d(std, "36001", 1900) == 2)
ok("1915 interpolated between harmonised ends (1910: 1 x 3, 1920: 2 x 1.5)",
   isTRUE(all.equal(y("36001", 1915, "faculty_births"), 3)) &&
     isTRUE(all.equal(y("36001", 1915), 1.5)))
ok("1880 read with the 1880 factor", y("36001", 1880, "faculty_births") == 2)
ok("county factor applies: 1900 read from 1900 (factor 2) -> 1 x 2",
   y("36001", 1900, "county_faculty_births") == 2)
ok("no county factor row is not scaled: 1902 read from 1910 -> 1",
   y("36001", 1902, "county_faculty_births") == 1)
ok("national faculty series uses the same factors",
   nat[birth_year == 1900, faculty_births] == 4 && nat[birth_year == 1920, faculty_births] == 3)

cat("\nProfessor stock\n")
ok("stock rescaled: n_faculty = (professors + deans) x factor",
   stock[GEOID == "36001" & census == 1900, n_faculty] == 4 &&
     stock[GEOID == "36001" & census == 1940, n_faculty] == 1.5)
ok("one professor in A in each census, none in B",
   all(stock[GEOID == "36001", n_prof] == 1) && all(stock[GEOID == "36003", n_prof] == 0))
ok("stock covers 1880, 1900, 1910 and 1940", setequal(stock[GEOID == "36001", census],
                                                      c(1880, 1900, 1910, 1940)))
ok("non-event county dropped", nrow(stock[GEOID == "17019"]) == 0)

cat("\nCoverage factors\n")
# 1860 and 1870 censuses: 1870 has an NCES benchmark, 1860 borrows its factor.
cov_dir <- file.path(root, "cov_parquet")
cov_rows <- rbind(
  p(1860L, 10L, 1L, 1L, 1L, 40L, 29L), p(1860L, 10L, 2L, 1L, 1L, 40L, 100L),
  p(1870L, 10L, 1L, 1L, 1L, 40L, 29L), p(1870L, 10L, 2L, 1L, 1L, 50L, 10L),
  p(1870L, 10L, 3L, 1L, 2L, 30L, 29L))
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "cov_rows", cov_rows)
invisible(DBI::dbExecute(con, sprintf("COPY cov_rows TO '%s' (FORMAT PARQUET, PARTITION_BY (YEAR))",
                                      gsub("\\\\", "/", cov_dir))))
DBI::dbDisconnect(con, shutdown = TRUE)
bench_path <- file.path(root, "bench.csv")
fwrite(data.table(academic_year = "1869-70", census_year = 1870L, staff_total = 10L,
                  staff_male = 8L, staff_female = 2L, source = "fixture"), bench_path)
cov_out <- file.path(root, "factors.csv")
Sys.setenv(IPUMS_1850_1950_FERTILITY_PARQUET_DIR = file.path(root, "no_1850_1950"),
           IPUMS_1850_1910_FERTILITY_PARQUET_DIR = cov_dir,
           IPUMS_1940_PARQUET_DIR = file.path(root, "no_1940"),
           OCC_COVERAGE_BENCHMARK = bench_path, OCC_COVERAGE_OUT = cov_out)
out_cov <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(COVERAGE),
                                    stdout = TRUE, stderr = TRUE))
ok("coverage build exits cleanly", is.null(attr(out_cov, "status")), tail(out_cov, 5))
fac <- fread(cov_out)
# 1870: two men coded 010-029 (a professor and a dean; the woman is excluded)
# against 8 NCES men -> coverage 0.25, factor 4.
ok("coverage counts men coded 010-029 against NCES men",
   fac[census == 1870, census_count] == 2 && isTRUE(all.equal(fac[census == 1870, faculty_factor], 4)))
ok("1860 borrows the 1870 factor and is flagged",
   isTRUE(all.equal(fac[census == 1860, faculty_factor], 4)) && fac[census == 1860, factor_imputed])

cat("\nSingle 1850-1950 extract\n")
# Every census 1850-1950 in one YEAR-partitioned tree (as usa_00011). County A
# has a man in each census; a professor's child aged 5 in 1920 (birth year
# 1915) and aged 3 in 1950 (1947). Factors differ by census so the source shows.
single_rows <- rbind(
  rbindlist(lapply(c(1850L, 1860L, 1870L, 1880L, 1900L, 1910L, 1930L, 1940L), function(yr)
    p(yr, 10L, 1L, 1L, 1L, 40L, 100L))),
  p(1920L, 10L, 1L, 1L, 1L, 40L, 29L), p(1920L, 10L, 1L, 2L, 1L, 5L, poploc = 1L),
  p(1950L, 10L, 1L, 1L, 1L, 40L, 29L), p(1950L, 10L, 1L, 2L, 2L, 3L, poploc = 1L))
dir_single <- file.path(root, "single")
out_single <- file.path(root, "out_single")
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "single_rows", single_rows)
invisible(DBI::dbExecute(con, sprintf("COPY single_rows TO '%s' (FORMAT PARQUET, PARTITION_BY (YEAR))",
                                      gsub("\\\\", "/", dir_single))))
DBI::dbDisconnect(con, shutdown = TRUE)
coverage_single <- file.path(root, "coverage_single.csv")
fwrite(data.table(census = c(1850L, 1860L, 1870L, 1880L, 1900L, 1910L, 1920L, 1930L, 1940L, 1950L),
                  faculty_factor = c(1, 1, 1, 1, 1, 1, 5, 1, 1, 2)), coverage_single)
# The fallback trees still point at the first fixture: the single tree must win.
Sys.setenv(IPUMS_1850_1950_FERTILITY_PARQUET_DIR = dir_single,
           IPUMS_1850_1910_FERTILITY_PARQUET_DIR = dir_early, IPUMS_1940_PARQUET_DIR = dir_1940,
           OCC_CHANNEL_OUT_DIR = out_single, OCC_COVERAGE_FACTORS = coverage_single)
out_s <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(BUILD),
                                  stdout = TRUE, stderr = TRUE))
ok("single-extract build exits cleanly", is.null(attr(out_s, "status")), tail(out_s, 5))
yr_s <- fread(file.path(out_single, "professor_children_county_year.csv"),
              colClasses = list(character = "GEOID"))
std_s <- fread(file.path(out_single, "professor_children_county_decade.csv"),
               colClasses = list(character = "GEOID"))
stock_s <- fread(file.path(out_single, "professor_stock_county_census.csv"),
                 colClasses = list(character = "GEOID"))
man_s <- jsonlite::read_json(file.path(out_single, "build_manifest.json"), simplifyVector = TRUE)
ys <- function(year, col = "prof_births") yr_s[GEOID == "36001" & birth_year == year][[col]]
ok("1915 read from 1920 at age 5, not interpolated",
   ys(1915) == 1 && ys(1915, "census") == 1920 && ys(1915, "source") == "c0_9" && !ys(1915, "interpolated"))
ok("1915 harmonised with the 1920 factor", ys(1915, "faculty_births") == 5)
ok("1947 read from 1950 with its factor", ys(1947) == 1 && ys(1947, "faculty_births") == 2)
ok("window ends the year before the latest census, nothing interpolated",
   identical(as.integer(man_s$birth_years), c(1830L, 1949L)) && !length(man_s$interpolated_years) &&
     !any(yr_s$interpolated))
ok("standard 1940s decade is complete (1947 only)", std_s[GEOID == "36001" & decade == 1940, prof_births] == 1)
ok("fallback trees ignored: stock holds the single tree's ten censuses",
   setequal(stock_s[GEOID == "36001", census], c(1850, 1860, 1870, 1880, 1900, 1910, 1920, 1930, 1940, 1950)))

# Coverage from the single tree, with 1930 estimated from instructional staff:
# 1929-30 reports 10 instructional staff; 1931-32 has 6 male professional per
# 12 instructional, so 1930 male staff = 10 x 6/12 = 5. One 1930 professor ->
# coverage 0.2, factor 5. 1920: 2 of 8 -> factor 4. 1950: 2 of 4 -> factor 2.
cov_single <- rbind(
  p(1870L, 10L, 1L, 1L, 1L, 40L, 29L),
  p(1920L, 10L, 1L, 1L, 1L, 40L, 29L), p(1920L, 10L, 2L, 1L, 1L, 40L, 15L),
  p(1930L, 10L, 1L, 1L, 1L, 40L, 29L),
  p(1950L, 10L, 1L, 1L, 1L, 40L, 29L), p(1950L, 10L, 2L, 1L, 1L, 40L, 10L))
dir_cov_single <- file.path(root, "cov_single")
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "cov_single", cov_single)
invisible(DBI::dbExecute(con, sprintf("COPY cov_single TO '%s' (FORMAT PARQUET, PARTITION_BY (YEAR))",
                                      gsub("\\\\", "/", dir_cov_single))))
DBI::dbDisconnect(con, shutdown = TRUE)
bench_single <- file.path(root, "bench_single.csv")
fwrite(data.table(academic_year = c("1869-70", "1919-20", "1929-30", "1931-32", "1949-50"),
                  census_year = c(1870L, 1920L, 1930L, NA, 1950L),
                  staff_total = c(10, 10, NA, 8, 5), staff_male = c(8, 8, NA, 6, 4),
                  staff_female = c(2, 2, NA, 2, 1), instructional_staff = c(NA, NA, 10, 12, NA),
                  source = "fixture"), bench_single)
cov_out_single <- file.path(root, "factors_single.csv")
Sys.setenv(IPUMS_1850_1950_FERTILITY_PARQUET_DIR = dir_cov_single,
           OCC_COVERAGE_BENCHMARK = bench_single, OCC_COVERAGE_OUT = cov_out_single)
out_cs <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(COVERAGE),
                                   stdout = TRUE, stderr = TRUE))
ok("single-tree coverage build exits cleanly", is.null(attr(out_cs, "status")), tail(out_cs, 5))
fs <- fread(cov_out_single)
ok("1930 benchmark estimated from instructional staff (factor 5, flagged)",
   isTRUE(all.equal(fs[census == 1930, faculty_factor], 5)) && fs[census == 1930, nces_estimated] &&
     fs[census == 1930, estimate_from] == "1931-32")
ok("1920 and 1950 read from the single tree (factors 4 and 2)",
   isTRUE(all.equal(fs[census == 1920, faculty_factor], 4)) &&
     isTRUE(all.equal(fs[census == 1950, faculty_factor], 2)) && !fs[census == 1950, nces_estimated])

unlink(root, recursive = TRUE)
cat(sprintf("\n%d passed, %d failed\n", passed, failed))
if (failed > 0L) quit(status = 1L)
