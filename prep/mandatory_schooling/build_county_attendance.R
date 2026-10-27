# Build county-level compulsory-schooling ATTENDANCE measures.
#
# Why attendance rather than attainment. A compulsory-ATTENDANCE law operates on
# whether a child is in school, not on the years they eventually complete. If
# enforcement pulls marginal children into school who still leave at the exit
# age, completed attainment barely moves and an attainment-based bite measure
# reads that as no bite -- or, through composition, as worse. Attendance is the
# margin the statute actually names, and SCHOOL records it CONTEMPORANEOUSLY on
# the childhood census row rather than being inferred from a 1940 report.
#
# Three consequences, all improvements over build_county_bite.R:
#
#   1. No 1940 linkage is needed for the outcome, so the sample is every
#      school-age childhood row (~42 M) rather than the 15.5 M that also carried
#      a usable 1940 attainment record. The extract is still linked-cases-only,
#      so selection does not vanish -- it just stops compounding.
#   2. No yob convention. The cohort is census year minus age, and nothing here
#      joins the CLS cohort file; only the state x year law panel is used.
#   3. 1930 becomes usable. Its children are born 1912-1925, outside the CLS
#      cohort window, which does not matter when the outcome is contemporaneous.
#
# Two things attendance costs, both handled below:
#
#   * SCHOOL's reference period changes across censuses (~12 months in 1900,
#     ~7.5 in 1910, ~4 in 1920, ~6 in 1930), so rates are comparable WITHIN a
#     census year and NOT across them. Every output is keyed on census year and
#     nothing is pooled over it.
#   * Attendance runs from 12% at age 5 to 87% at age 11 and back to 24% at 18,
#     so a county whose compulsory window skews old looks non-compliant when it
#     is not. Hence the age-standardised band alongside the raw window rate.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/build_county_attendance.R
#
# Outputs (output/mandatory_schooling/county_attendance/):
#   attendance_children.parquet     one row per childhood observation
#   attendance_county_year.csv      county x census year            (PRIMARY)
#   attendance_county_age.parquet   county x census year x age
#   attendance_state_year.csv       state x census year, incl. the no-law benchmark
#   input_manifest.csv, validation_summary.csv

suppressPackageStartupMessages({
  library("DBI")
  library("duckdb")
  library("dplyr")
  library("readr")
})

# --------------------------------------------------------------------------- #
# Paths
# --------------------------------------------------------------------------- #

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

IN_DIR <- Sys.getenv("COUNTY_ATTENDANCE_IN_DIR", unset = "")
OUT_DIR <- Sys.getenv("COUNTY_ATTENDANCE_OUT_DIR",
                      unset = file.path(data_root, "output/mandatory_schooling/county_attendance"))
overridden <- nzchar(IN_DIR)
if (!overridden) IN_DIR <- file.path(data_root, "output/mandatory_schooling/county_bite/inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

inputs <- c(
  children = file.path(IN_DIR, "candidates_pre1940.parquet"),
  limits   = if (overridden) file.path(IN_DIR, "cls_state_age_limits_1880_1930.parquet")
             else file.path(data_root, "output/mandatory_schooling/cls",
                            "cls_state_age_limits_1880_1930.parquet"),
  pop      = if (overridden) file.path(IN_DIR, "county_population.csv")
             else file.path(data_root, "output/county_population.csv")
)
absent <- inputs[!file.exists(inputs)]
if (length(absent)) {
  stop("Missing input(s):\n  ", paste(absent, collapse = "\n  "),
       "\nRun prep/mandatory_schooling/extract_linked_bite_inputs.py first ",
       "(it must be the version that carries SCHOOL).")
}

# --------------------------------------------------------------------------- #
# Constants
# --------------------------------------------------------------------------- #

CENSUS_YEARS <- c(1900L, 1910L, 1920L, 1930L)

# Ages 10-13 sit inside the compulsory window for 1,762 of the 1,808 law
# state-years (entry never exceeds 10, and exit is 14 or above in all but 46), so
# they are the widest band that is legally required almost everywhere. Direct
# standardisation over them, with uniform weights, makes county rates comparable
# across states whose windows differ.
STD_AGES <- 10:13

# Ages either side of the exit age, for the discontinuity. Attendance falls with
# age throughout, so the jump is only interpretable against the no-law benchmark
# that attendance_state_year.csv carries.
DISC_HALFWIDTH <- 2L

MIN_CELL <- 25L
MIN_CELL_FOR_RANKING <- 200L

# School-age children cannot plausibly exceed this share of a county's
# population, so a higher value means the denominator is wrong for the cell.
COVERAGE_MAX <- 0.30

# --------------------------------------------------------------------------- #
# Validation accumulator
# --------------------------------------------------------------------------- #

checks <- list()
expect <- function(label, ok, detail = "") {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = label, validation_passed = isTRUE(ok), detail = as.character(detail),
    stringsAsFactors = FALSE)
  if (!isTRUE(ok)) message("  FAIL  ", label, if (nzchar(detail)) paste0("  -- ", detail) else "")
  invisible(ok)
}

q <- function(path) paste0("'", normalizePath(path, winslash = "/", mustWork = TRUE), "'")

con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=4")
dbExecute(con, sprintf("PRAGMA temp_directory='%s'",
                       gsub("\\\\", "/", file.path(tempdir(), "duckdb_spill"))))
dbExecute(con, "PRAGMA memory_limit='12GB'")

# --------------------------------------------------------------------------- #
# 1. The law, and a counterfactual window for state-years without one
# --------------------------------------------------------------------------- #

message("Reading the law panel ...")

dbExecute(con, sprintf("
CREATE TABLE law AS
SELECT statefip, year, law_in_force, entryage, exitage, effective_leaving_age
FROM read_parquet(%s)
WHERE year IN (%s)", q(inputs[["limits"]]), paste(CENSUS_YEARS, collapse = ",")))

# Where no law was in force, entryage and exitage are 0 -- the raw CLS
# convention for "provision absent", not a missing value. So a counterfactual
# window is needed to ask "who WOULD the law later have required", which is the
# pre-law baseline. The state's modal window over its law-in-force years is the
# natural stand-in; it is a state constant and so cannot drive within-state
# variation.
dbExecute(con, sprintf("
CREATE TABLE cf_window AS
SELECT statefip, mode(entryage) AS cf_entryage, mode(exitage) AS cf_exitage
FROM read_parquet(%s)
WHERE law_in_force
GROUP BY statefip", q(inputs[["limits"]])))

# --------------------------------------------------------------------------- #
# 2. The children
# --------------------------------------------------------------------------- #

message("Building the child-level table ...")

# SCHOOL: 1 = did not attend, 2 = attended, 8 = unknown (0.03% of 1900 rows),
# 0 = N/A, 9 = missing. Only 1 and 2 are informative.
# COUNTYICP is the FIPS code times ten for all but ~0.03% of rows, which end in
# 5 and get a NULL geoid rather than being merged into a neighbouring county.
dbExecute(con, sprintf("
CREATE TABLE kids AS
SELECT c.HIK,
       c.YEAR                       AS census_year,
       c.AGE                        AS age,
       c.YEAR - c.AGE               AS birth_year_approx,
       c.SEX, c.RACE, c.SCHOOL,
       c.STATEFIP                   AS statefip,
       c.COUNTYICP                  AS countyicp,
       (c.COUNTYICP %% 10 = 0)      AS countyicp_standard,
       CASE WHEN c.COUNTYICP %% 10 = 0
            THEN c.STATEFIP::INTEGER * 1000 + c.COUNTYICP / 10 END AS county_geoid,
       (c.SCHOOL = 2)               AS attended,
       (c.SCHOOL IN (1, 2))         AS attendance_known,
       coalesce(l.law_in_force, FALSE) AS law_in_force,
       l.entryage, l.exitage, l.effective_leaving_age,
       w.cf_entryage, w.cf_exitage,
       -- The window the law actually imposed that year, where there was one.
       (coalesce(l.law_in_force, FALSE)
          AND c.AGE >= l.entryage AND c.AGE < l.exitage)        AS in_window,
       -- Net of the exemptions: the age any statutory route out first opens.
       (coalesce(l.law_in_force, FALSE)
          AND c.AGE >= l.entryage AND c.AGE < l.effective_leaving_age)
                                                                AS in_window_effective,
       -- Just above the exit age, where attendance is voluntary: a within-county
       -- benchmark for the county's general schooling propensity.
       (coalesce(l.law_in_force, FALSE)
          AND c.AGE >= l.exitage AND c.AGE < l.exitage + %d)    AS above_window,
       -- The pre-law counterfactual window, for state-years with no law.
       (NOT coalesce(l.law_in_force, FALSE)
          AND c.AGE >= w.cf_entryage AND c.AGE < w.cf_exitage)  AS in_window_cf,
       (c.AGE BETWEEN %d AND %d)                                AS in_std_band
FROM read_parquet(%s) c
LEFT JOIN law l ON l.statefip = c.STATEFIP AND l.year = c.YEAR
LEFT JOIN cf_window w ON w.statefip = c.STATEFIP
WHERE c.YEAR IN (%s)",
  DISC_HALFWIDTH, min(STD_AGES), max(STD_AGES), q(inputs[["children"]]),
  paste(CENSUS_YEARS, collapse = ",")))

n_kids <- dbGetQuery(con, "SELECT count(*) n FROM kids")$n
message(sprintf("  %s childhood observations", format(n_kids, big.mark = ",")))

# --------------------------------------------------------------------------- #
# 3. County x census year x age, then the standardised rate
# --------------------------------------------------------------------------- #

message("Aggregating ...")

dbExecute(con, "
CREATE TABLE county_age AS
SELECT county_geoid, census_year, age,
       count(*)                                            AS n_children,
       sum(CASE WHEN attendance_known THEN 1 ELSE 0 END)   AS n_known,
       avg(CASE WHEN attended THEN 0.0 ELSE 1.0 END)       AS not_attending
FROM kids
WHERE county_geoid IS NOT NULL AND attendance_known
GROUP BY 1, 2, 3")

# Direct standardisation with uniform weights over STD_AGES: the mean of the
# age-specific rates, which is what makes two counties comparable when their
# children's ages are distributed differently. NULL where any age in the band is
# too thin to carry a rate.
dbExecute(con, sprintf("
CREATE TABLE county_std AS
SELECT county_geoid, census_year,
       CASE WHEN count(*) = %d AND min(n_children) >= 5
            THEN avg(not_attending) END AS not_attending_std_1013,
       sum(n_children)                  AS n_std_band
FROM county_age
WHERE age BETWEEN %d AND %d
GROUP BY 1, 2", length(STD_AGES), min(STD_AGES), max(STD_AGES)))

dbExecute(con, "
CREATE TABLE county_year AS
SELECT k.county_geoid, k.census_year,
       any_value(k.statefip)                                     AS statefip,
       count(*)                                                  AS n_children,
       -- The headline: among children the law required to attend, the share who
       -- did not. Uses the actual window where a law was in force and the
       -- counterfactual window where none was.
       sum(CASE WHEN k.in_window OR k.in_window_cf THEN 1 ELSE 0 END) AS n_required,
       avg(CASE WHEN k.in_window OR k.in_window_cf
                THEN CASE WHEN k.attended THEN 0.0 ELSE 1.0 END END) AS not_attending_required,
       avg(CASE WHEN k.in_window_effective
                THEN CASE WHEN k.attended THEN 0.0 ELSE 1.0 END END) AS not_attending_effective,
       avg(CASE WHEN k.above_window
                THEN CASE WHEN k.attended THEN 0.0 ELSE 1.0 END END) AS not_attending_above,
       sum(CASE WHEN k.above_window THEN 1 ELSE 0 END)           AS n_above,
       avg(CASE WHEN k.in_window OR k.in_window_cf THEN k.age END) AS mean_age_required,
       max(CASE WHEN k.in_window_cf THEN 1 ELSE 0 END)           AS window_is_cf,
       any_value(k.entryage)                                     AS entryage,
       any_value(k.exitage)                                      AS exitage
FROM kids k
WHERE k.county_geoid IS NOT NULL AND k.attendance_known
GROUP BY 1, 2")

# The exit-age discontinuity, within county and census year: same county, same
# reference period, children either side of the age at which attendance stops
# being compulsory.
dbExecute(con, sprintf("
CREATE TABLE county_disc AS
SELECT county_geoid, census_year,
       avg(CASE WHEN age >= exitage - %1$d AND age < exitage
                THEN CASE WHEN attended THEN 0.0 ELSE 1.0 END END) AS not_attending_below_exit,
       avg(CASE WHEN age >= exitage AND age < exitage + %1$d
                THEN CASE WHEN attended THEN 0.0 ELSE 1.0 END END) AS not_attending_above_exit,
       sum(CASE WHEN age >= exitage - %1$d AND age < exitage + %1$d THEN 1 ELSE 0 END)
                                                                   AS n_disc_window
FROM kids
WHERE county_geoid IS NOT NULL AND attendance_known AND law_in_force
GROUP BY 1, 2", DISC_HALFWIDTH))

county <- dbGetQuery(con, "
SELECT y.*, s.not_attending_std_1013, s.n_std_band,
       d.not_attending_below_exit, d.not_attending_above_exit, d.n_disc_window,
       d.not_attending_above_exit - d.not_attending_below_exit AS exit_discontinuity
FROM county_year y
LEFT JOIN county_std s USING (county_geoid, census_year)
LEFT JOIN county_disc d USING (county_geoid, census_year)")

# Coverage: linked school-age children against the county's population. School-age
# children are roughly a quarter of a population, and the extract holds only
# linked cases, so a plausible value is well under 0.30.
pop <- read_csv(inputs[["pop"]], show_col_types = FALSE) %>%
  transmute(county_geoid = suppressWarnings(as.integer(GEOID)),
            census_year = as.integer(decade),
            county_population = suppressWarnings(as.numeric(population))) %>%
  filter(!is.na(county_geoid), census_year %in% CENSUS_YEARS)

county <- county %>%
  left_join(pop, by = c("county_geoid", "census_year")) %>%
  mutate(coverage = ifelse(!is.na(county_population) & county_population > 0,
                           n_children / county_population, NA_real_),
         below_min_cell = n_required < MIN_CELL,
         coverage_implausible = !is.na(coverage) & coverage > COVERAGE_MAX,
         coverage_missing = is.na(coverage),
         se_required = sqrt(not_attending_required * (1 - not_attending_required) /
                              pmax(n_required, 1L)),
         cell_usable = !below_min_cell & !coverage_implausible & !coverage_missing) %>%
  arrange(county_geoid, census_year)

# State x census year, carrying the no-law benchmark the discontinuity needs.
state_year <- dbGetQuery(con, sprintf("
SELECT statefip, census_year, any_value(law_in_force) AS law_in_force,
       count(*) AS n_children,
       avg(CASE WHEN in_window OR in_window_cf
                THEN CASE WHEN attended THEN 0.0 ELSE 1.0 END END) AS not_attending_required,
       avg(CASE WHEN above_window
                THEN CASE WHEN attended THEN 0.0 ELSE 1.0 END END) AS not_attending_above,
       -- NOT coalesce(): the CLS panel stores 0, not NULL, for a provision that
       -- is absent, so coalesce would never fire and the no-law benchmark -- the
       -- thing the discontinuity is read against -- would be computed at age 0
       -- and come back empty.
       avg(CASE WHEN age >= (CASE WHEN law_in_force THEN exitage ELSE cf_exitage END) - %1$d
                 AND age <  (CASE WHEN law_in_force THEN exitage ELSE cf_exitage END)
                THEN CASE WHEN attended THEN 0.0 ELSE 1.0 END END) AS not_attending_below_exit,
       avg(CASE WHEN age >= (CASE WHEN law_in_force THEN exitage ELSE cf_exitage END)
                 AND age <  (CASE WHEN law_in_force THEN exitage ELSE cf_exitage END) + %1$d
                THEN CASE WHEN attended THEN 0.0 ELSE 1.0 END END) AS not_attending_above_exit
FROM kids
WHERE attendance_known
GROUP BY 1, 2
ORDER BY 1, 2", DISC_HALFWIDTH)) %>%
  mutate(exit_discontinuity = not_attending_above_exit - not_attending_below_exit)

county_age <- dbGetQuery(con, "SELECT * FROM county_age ORDER BY county_geoid, census_year, age")

# --------------------------------------------------------------------------- #
# 4. Validation
# --------------------------------------------------------------------------- #

message("Validating ...")

rates <- c(county$not_attending_required, county$not_attending_above,
           county$not_attending_std_1013)
expect("every rate is a proportion", all(is.na(rates) | (rates >= 0 & rates <= 1)),
       sum(!is.na(rates) & (rates < 0 | rates > 1)))
expect("census years are a subset of the four pre-1940 censuses",
       all(county$census_year %in% CENSUS_YEARS),
       paste(sort(unique(county$census_year)), collapse = ", "))
# All four only on the real extract; a fixture may carry one.
if (!overridden) {
  expect("all four pre-1940 censuses are present",
         setequal(unique(county$census_year), CENSUS_YEARS),
         paste(sort(unique(county$census_year)), collapse = ", "))
}

prof <- dbGetQuery(con, "
SELECT age, avg(CASE WHEN attended THEN 1.0 ELSE 0.0 END) att
FROM kids WHERE attendance_known AND census_year = 1900 GROUP BY 1 ORDER BY 1")
peak <- prof$age[which.max(prof$att)]
expect("the attendance profile peaks between ages 9 and 13", peak >= 9 && peak <= 13, peak)
expect("attendance at 18 is below attendance at 11",
       prof$att[prof$age == 18] < prof$att[prof$age == 11],
       sprintf("18: %.3f vs 11: %.3f", prof$att[prof$age == 18], prof$att[prof$age == 11]))

if (!overridden) {
  expect("no-law state-years exist as a discontinuity benchmark",
         any(!state_year$law_in_force),
         sum(!state_year$law_in_force))
  expect("required-age non-attendance is lower than above-window non-attendance",
         median(county$not_attending_required, na.rm = TRUE) <
           median(county$not_attending_above, na.rm = TRUE),
         sprintf("%.3f vs %.3f", median(county$not_attending_required, na.rm = TRUE),
                 median(county$not_attending_above, na.rm = TRUE)))
}

nonstd <- dbGetQuery(con, "
SELECT sum(CASE WHEN NOT countyicp_standard THEN 1 ELSE 0 END) n, count(*) total FROM kids")
expect("non-standard COUNTYICP stays a rounding error",
       nonstd$n / nonstd$total < 0.001,
       sprintf("%d of %d (%.3f%%)", nonstd$n, nonstd$total, 100 * nonstd$n / nonstd$total))

message(sprintf(paste0("  county x year: %d cells, %d usable (%d below the %d-child ",
                       "minimum, %d implausible coverage, %d no denominator)"),
                nrow(county), sum(county$cell_usable), sum(county$below_min_cell),
                MIN_CELL, sum(county$coverage_implausible), sum(county$coverage_missing)))

# --------------------------------------------------------------------------- #
# 5. Write
# --------------------------------------------------------------------------- #

message("Writing ...")
dbExecute(con, sprintf("COPY kids TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
                       gsub("\\\\", "/", file.path(OUT_DIR, "attendance_children.parquet"))))
write_csv(county, file.path(OUT_DIR, "attendance_county_year.csv"))
arrow::write_parquet(county, file.path(OUT_DIR, "attendance_county_year.parquet"))
arrow::write_parquet(county_age, file.path(OUT_DIR, "attendance_county_age.parquet"))
write_csv(state_year, file.path(OUT_DIR, "attendance_state_year.csv"))

write_csv(data.frame(input = names(inputs), path = unname(inputs),
                     md5 = vapply(unname(inputs),
                                  function(p) unname(tools::md5sum(p)), character(1)),
                     stringsAsFactors = FALSE),
          file.path(OUT_DIR, "input_manifest.csv"))
validation <- dplyr::bind_rows(checks)
write_csv(validation, file.path(OUT_DIR, "validation_summary.csv"))

message(sprintf("\nchildren %s | county x year rows %s | state x year rows %s",
                format(n_kids, big.mark = ","), format(nrow(county), big.mark = ","),
                format(nrow(state_year), big.mark = ",")))
message("Wrote: ", OUT_DIR)

if (!all(validation$validation_passed)) {
  stop(sum(!validation$validation_passed), " validation check(s) FAILED.")
}
message("All ", nrow(validation), " validation checks passed.")
