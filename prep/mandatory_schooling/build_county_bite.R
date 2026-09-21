# Build the county-level compulsory-schooling bite measures.
#
# Bite = how far a student's completed schooling fell short of what the law
# required of them. Required years come from the law of the state the student
# lived in AT SCHOOL AGE, recovered through the MLP linkage, and the measures are
# aggregated to the county they lived in then.
#
# Specification: prep/mandatory_schooling/county_bite_from_linked_census.md
# Inputs come from extract_linked_bite_inputs.py; run that first.
#
# Six requirement bases, because "required years" is not one number:
#
#   required_nominal        ca_years1          age limits only
#   required_nominal_cf     + plateau imputed  nominal, pre-treatment
#   required_effective      ca_years_combined  net of work-permit, early-dropout
#                                              and continuation exemptions
#   required_effective_cf   + plateau imputed  HEADLINE for cross-cohort work
#   required_fixed6/8       constants          robustness: shortfall_rate is the
#                                              county CDF at the threshold, and
#                                              county CDFs cross
#
# The counterfactual bases exist because ca_years == 0 means NO LAW BOUND THAT
# COHORT (231 of 1,372 sample cells). There shortfall_rate and gap_censored are
# zero by construction, not by finding, so the actual bases are reported only on
# law-bound students while the counterfactual bases -- which impute the standard
# the state would later set -- are reported on everyone. Bite measured during a
# law regime is contaminated: if the law worked, attainment rose and measured bite
# fell, making it an outcome. Measured against a future standard on pre-law
# cohorts it is a usable baseline.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/build_county_bite.R
#
# Outputs (output/mandatory_schooling/county_bite/):
#   bite_students.parquet        one row per HIK
#   bite_county_stratum.csv      county x assignment stratum x basis  (PRIMARY)
#   bite_county_cohort.parquet   county x birth cohort x basis
#   bite_state_cohort.csv        state x birth cohort x basis
#   bite_county_coverage.csv     linked students against county births
#   input_manifest.csv, validation_summary.csv

suppressPackageStartupMessages({
  library("DBI")
  library("duckdb")
  library("dplyr")
  library("tidyr")
  library("readr")
  library("arrow")
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

CLS_DIR <- file.path(data_root, "output/mandatory_schooling/cls")

# COUNTY_BITE_IN_DIR / _OUT_DIR let test_county_bite.R drive the whole script over
# a synthetic fixture. When IN_DIR is overridden every input is expected there
# under its conventional name, so the test controls the law panel too and can
# make a boundary cohort flip on the state's own window.
IN_DIR <- Sys.getenv("COUNTY_BITE_IN_DIR",
                     unset = file.path(data_root, "output/mandatory_schooling/county_bite/inputs"))
BITE_DIR <- Sys.getenv("COUNTY_BITE_OUT_DIR",
                       unset = file.path(data_root, "output/mandatory_schooling/county_bite"))
overridden <- nzchar(Sys.getenv("COUNTY_BITE_IN_DIR"))
dir.create(BITE_DIR, recursive = TRUE, showWarnings = FALSE)

inputs <- c(
  students   = file.path(IN_DIR, "students_1940.parquet"),
  candidates = file.path(IN_DIR, "candidates_pre1940.parquet"),
  limits     = if (overridden) file.path(IN_DIR, "cls_state_age_limits_1880_1930.parquet")
               else file.path(CLS_DIR, "cls_state_age_limits_1880_1930.parquet"),
  cohorts    = if (overridden) file.path(IN_DIR, "cls_cohort_requirements_1875_1912.parquet")
               else file.path(CLS_DIR, "cls_cohort_requirements_1875_1912.parquet"),
  births     = if (overridden) file.path(IN_DIR, "us_panel_county_stem_1800.csv")
               else file.path(data_root, "output/us_panel_county_stem_1800.csv")
)
absent <- inputs[!file.exists(inputs)]
if (length(absent)) {
  stop("Missing input(s):\n  ", paste(absent, collapse = "\n  "),
       "\nRun prep/mandatory_schooling/extract_linked_bite_inputs.py and ",
       "build_cls_schooling_laws.R first.")
}

# --------------------------------------------------------------------------- #
# Constants
# --------------------------------------------------------------------------- #

YOB_MIN <- 1885L
YOB_MAX <- 1912L
CANDIDATE_YEARS <- c(1900L, 1910L, 1920L, 1930L)

# Median window midpoint over all 1,808 law-in-force state-years (mean 11.48).
# Used only where no law was in force at a candidate year, so the rule still has
# a target age to score against.
FALLBACK_TARGET_AGE <- 11.5

MIN_CELL <- 25L

# The statutory windows span entry 6-10 to exit 12-18, so any age at which a law
# could have applied. Enforced on cand_age, not on the recorded age.
CAND_AGE_MIN <- 5L
CAND_AGE_MAX <- 18L

ACTUAL_BASES <- c("nominal", "effective")
COUNTERFACTUAL_BASES <- c("nominal_cf", "effective_cf", "fixed6", "fixed8")

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
# 33 M students joined to 42 M candidates: give DuckDB somewhere to spill so a
# large intermediate degrades to disk instead of failing.
dbExecute(con, sprintf("PRAGMA temp_directory='%s'",
                       gsub("\\\\", "/", file.path(tempdir(), "duckdb_spill"))))
dbExecute(con, "PRAGMA memory_limit='12GB'")

message("Building student-level panel ...")

# --------------------------------------------------------------------------- #
# 1. Students: cohort key and observed schooling
# --------------------------------------------------------------------------- #

# yob = 1940 - AGE - 1, not 1940 - AGE. Enumeration was 1 April, so most people
# had not had their birthday; this is the key CLS built the law measure on.
# yrs_sch is constructed from HIGRADED because IPUMS has no years-of-schooling
# variable. Same expression as describe_cls_policy_environment.R:236-238.
dbExecute(con, sprintf("
CREATE TABLE students AS
WITH raw AS (
  SELECT HIK, AGE, SEX, RACE, BPL, HIGRADED, EDUC,
         1940 - AGE - 1 AS yob,
         CAST(FLOOR(HIGRADED / 10.0) AS INTEGER) AS g
  FROM read_parquet(%s)
  WHERE HIGRADED <> 999
)
SELECT HIK, AGE, SEX, RACE, BPL, HIGRADED, EDUC, yob,
       CASE WHEN g <= 3 THEN 0
            WHEN g <= 20 THEN g - 3
            WHEN g <= 23 THEN 17 END AS observed_years
FROM raw
WHERE yob BETWEEN %d AND %d", q(inputs[["students"]]), YOB_MIN, YOB_MAX))

n_students_raw <- dbGetQuery(con, "SELECT count(*) n FROM students")$n

# --------------------------------------------------------------------------- #
# 2. Statutory windows, and the year-selection rule
# --------------------------------------------------------------------------- #

dbExecute(con, sprintf("
CREATE TABLE windows AS
SELECT statefip, year, (entryage + exitage) / 2.0 AS window_mid
FROM read_parquet(%s)
WHERE law_in_force AND year IN (%s)",
  q(inputs[["limits"]]), paste(CANDIDATE_YEARS, collapse = ",")))

# Every candidate is scored against ITS OWN observed state, because the window
# depends on the state and the state is only known per candidate year. Two
# passes, not one, and not circular.
dbExecute(con, sprintf("
CREATE TABLE scored AS
SELECT c.HIK,
       c.YEAR                       AS cand_year,
       c.AGE                        AS cand_age_observed,
       c.STATEFIP                   AS cand_statefip,
       c.COUNTYICP                  AS cand_countyicp,
       c.MOMLOC, c.POPLOC, c.GQ,
       s.yob,
       c.YEAR - s.yob               AS cand_age,
       (c.YEAR - s.yob) - c.AGE     AS age_consistency,
       w.window_mid,
       (w.window_mid IS NULL)       AS no_law_at_candidate,
       abs((c.YEAR - s.yob) - COALESCE(w.window_mid, %f)) AS selection_distance
FROM read_parquet(%s) c
JOIN students s ON s.HIK = c.HIK
LEFT JOIN windows w ON w.statefip = c.STATEFIP AND w.year = c.YEAR
-- The extractor pre-filters on the age RECORDED at the candidate census, which
-- is cheap. The authoritative window is on cand_age = YEAR - yob, the same basis
-- the treatment key uses, and the two disagree: the modal difference is +1
-- (57%%), which is what an internally consistent record gives, since
-- yob = 1940 - AGE - 1 sits a year below the earlier censuses' own arithmetic.
-- Records outside the window on this basis are age-inconsistent links; without
-- this they produce distances up to 23 years and a spurious 1930 stratum.
WHERE (c.YEAR - s.yob) BETWEEN %d AND %d",
  FALLBACK_TARGET_AGE, q(inputs[["candidates"]]), CAND_AGE_MIN, CAND_AGE_MAX))

# Ties break to the earlier year, which is closer to school entry.
dbExecute(con, "
CREATE TABLE assigned AS
SELECT * EXCLUDE (rn) FROM (
  SELECT *, row_number() OVER (
    PARTITION BY HIK ORDER BY selection_distance, cand_year) AS rn
  FROM scored
) WHERE rn = 1")

# --------------------------------------------------------------------------- #
# 3. Required years: residence-state law, and the plateaus
# --------------------------------------------------------------------------- #

dbExecute(con, sprintf("
CREATE TABLE cohorts AS
SELECT bpl, yob, statename, region, region_label,
       ca_years1, ca_years, ca_years_combined
FROM read_parquet(%s)
WHERE yob BETWEEN %d AND %d", q(inputs[["cohorts"]]), YOB_MIN, YOB_MAX))

# The plateau is the requirement once the law is fully phased in -- equivalently
# the first fully-exposed cohort's value. Taken over the sample cohorts only.
dbExecute(con, "
CREATE TABLE plateau AS
SELECT bpl,
       max(ca_years1)         AS plateau_nominal,
       max(ca_years_combined) AS plateau_effective
FROM cohorts GROUP BY bpl")

# COUNTYICP is the FIPS county code TIMES 10 and is state-dependent, so the state
# is not optional. GEOID = STATEFIP*1000 + COUNTYICP/10.
dbExecute(con, "
CREATE TABLE bite_students AS
SELECT a.HIK, a.yob,
       -- FLOOR, not a bare cast: DuckDB ROUNDS on DOUBLE -> INTEGER, so
       -- (1889 / 10)::INTEGER * 10 would give 1890 rather than 1880.
       CAST(FLOOR(a.yob / 10.0) AS INTEGER) * 10 AS cohort_decade,
       s.AGE, s.SEX, s.RACE, s.BPL, s.HIGRADED, s.observed_years,
       a.cand_year AS assignment_stratum,
       a.cand_year, a.cand_age, a.cand_age_observed, a.age_consistency,
       a.selection_distance,
       a.no_law_at_candidate, a.MOMLOC, a.POPLOC, a.GQ,
       a.cand_statefip, a.cand_countyicp,
       -- COUNTYICP is the FIPS code times ten for all but ~0.03% of rows:
       -- 5,111 end in 5 (Oregon 605, Virginia 1875, Tennessee 875, South Dakota
       -- 1055). Flooring those would silently merge them into a neighbouring
       -- county, so they get a NULL geoid and a flag instead, and drop out of the
       -- county grains while still counting in the state ones.
       (a.cand_countyicp % 10 = 0) AS countyicp_standard,
       CASE WHEN a.cand_countyicp % 10 = 0
            THEN a.cand_statefip::INTEGER * 1000 + a.cand_countyicp / 10
       END AS county_geoid,
       r.statename AS resid_statename, r.region_label AS resid_region_label,
       r.ca_years1          AS required_nominal,
       r.ca_years_combined  AS required_effective,
       r.ca_years           AS required_effective_regular,
       b.ca_years_combined  AS required_effective_bpl,
       CASE WHEN r.ca_years1 > 0 THEN r.ca_years1 ELSE p.plateau_nominal END
         AS required_nominal_cf,
       CASE WHEN r.ca_years1 > 0 THEN r.ca_years_combined ELSE p.plateau_effective END
         AS required_effective_cf,
       6 AS required_fixed6,
       8 AS required_fixed8,
       (r.ca_years1 > 0)  AS law_bound_cohort,
       (r.ca_years1 = 0)  AS required_is_imputed
FROM assigned a
JOIN students s ON s.HIK = a.HIK
JOIN cohorts  r ON r.bpl = a.cand_statefip AND r.yob = a.yob
JOIN plateau  p ON p.bpl = a.cand_statefip
LEFT JOIN cohorts b ON b.bpl = s.BPL AND b.yob = a.yob")

n_bite <- dbGetQuery(con, "SELECT count(*) n FROM bite_students")$n
message(sprintf("  students with attainment: %s | assigned a county: %s",
                format(n_students_raw, big.mark = ","), format(n_bite, big.mark = ",")))

# --------------------------------------------------------------------------- #
# 4. Per-county linkage coverage
# --------------------------------------------------------------------------- #

# The threat to a within-state ranking is that linkage is not random across
# counties. Dividing linked students by an independent count of the county's
# children makes that visible instead of implicit.
dbExecute(con, sprintf("
CREATE TABLE births AS
SELECT GEOID, CAST(decade AS INTEGER) AS decade,
       -- TRY_CAST, not CAST: the column carries 294 literal 'NA' strings, which
       -- makes read_csv_auto type it VARCHAR. Those become NULL births and so
       -- NULL coverage, which is the honest answer for them.
       TRY_CAST(county_births_estimate_decade AS DOUBLE) AS births
FROM read_csv_auto(%s)
WHERE CAST(decade AS INTEGER) BETWEEN 1880 AND 1910", q(inputs[["births"]])))

dbExecute(con, "
CREATE TABLE coverage AS
SELECT d.county_geoid, d.cohort_decade, d.n_students, b.births,
       CASE WHEN b.births > 0 THEN d.n_students / b.births END AS coverage
FROM (SELECT county_geoid, cohort_decade, count(*)::DOUBLE AS n_students
      FROM bite_students WHERE county_geoid IS NOT NULL GROUP BY 1, 2) d
LEFT JOIN births b
  -- Compare as integers on both sides. read_csv_auto types GEOID VARCHAR on the
  -- real file (it holds leading zeros and some non-numeric rows) but BIGINT on a
  -- fixture where every value looks like a number, so a zero-padded string join
  -- silently matches nothing in one of the two cases.
  ON TRY_CAST(b.GEOID AS INTEGER) = d.county_geoid
 AND b.decade = d.cohort_decade")

dbExecute(con, "
CREATE TABLE bite_students_cov AS
SELECT s.*, c.coverage
FROM bite_students s
LEFT JOIN coverage c
  ON c.county_geoid = s.county_geoid AND c.cohort_decade = s.cohort_decade")

# --------------------------------------------------------------------------- #
# 5. Aggregate: four statistics per basis, three grains, two samples
# --------------------------------------------------------------------------- #

# Wide in SQL then long in R. Computing six bases in long form would sextuple a
# 30 M row table for no gain; the aggregates are small enough to reshape here.
basis_sql <- function(basis) {
  col <- paste0("required_", basis)
  sprintf(paste(
    "avg(%1$s)                                                   AS mean_required__%2$s",
    "avg(CASE WHEN observed_years < %1$s THEN 1.0 ELSE 0.0 END) AS shortfall_rate__%2$s",
    "avg(LEAST(observed_years, %1$s) - %1$s)                    AS gap_censored__%2$s",
    "avg(observed_years - %1$s)                                 AS gap_mean__%2$s",
    "quantile_cont(observed_years - %1$s, 0.10)                  AS gap_p10__%2$s",
    "quantile_cont(observed_years - %1$s, 0.25)                  AS gap_p25__%2$s",
    "quantile_cont(observed_years - %1$s, 0.50)                  AS gap_p50__%2$s",
    sep = ",\n       "), col, basis)
}

DIAGNOSTICS <- paste(
  "count(*)                         AS n_students",
  "avg(selection_distance)          AS mean_selection_distance",
  "avg(CASE WHEN no_law_at_candidate THEN 1.0 ELSE 0.0 END) AS share_no_law_at_candidate",
  "avg(CASE WHEN required_is_imputed THEN 1.0 ELSE 0.0 END) AS share_imputed",
  "avg(cand_age)                    AS mean_cand_age",
  "avg(coverage)                    AS mean_coverage",
  "avg(observed_years)              AS mean_observed_years",
  sep = ",\n       ")

aggregate_grain <- function(keys, bases, sample_label, where = "TRUE") {
  sql <- sprintf("SELECT %s,\n       %s,\n       %s\nFROM bite_students_cov\nWHERE %s\nGROUP BY %s",
                 paste(keys, collapse = ", "), DIAGNOSTICS,
                 paste(vapply(bases, basis_sql, character(1)), collapse = ",\n       "),
                 where, paste(seq_along(keys), collapse = ", "))
  wide <- dbGetQuery(con, sql)
  wide %>%
    pivot_longer(cols = matches("__"), names_to = c("statistic", "basis"),
                 names_sep = "__", values_to = "value") %>%
    pivot_wider(names_from = "statistic", values_from = "value") %>%
    mutate(sample = sample_label,
           shortfall_se = sqrt(shortfall_rate * (1 - shortfall_rate) / n_students),
           below_min_cell = n_students < MIN_CELL) %>%
    relocate(sample, basis, .after = last_col())
}

build_table <- function(keys) {
  # A NULL county_geoid is a non-standard COUNTYICP, which has no FIPS county to
  # aggregate to; it still belongs in the state grains.
  gate <- if ("county_geoid" %in% keys) "county_geoid IS NOT NULL" else "TRUE"
  bind_rows(
    aggregate_grain(keys, ACTUAL_BASES, "law_bound",
                    paste(gate, "AND law_bound_cohort")),
    aggregate_grain(keys, COUNTERFACTUAL_BASES, "all_students", gate)
  ) %>% arrange(across(all_of(keys)), basis)
}

message("Aggregating ...")
county_stratum <- build_table(c("county_geoid", "assignment_stratum"))
county_cohort  <- build_table(c("county_geoid", "yob"))
state_cohort   <- build_table(c("cand_statefip", "yob"))
coverage_tbl   <- dbGetQuery(con, "SELECT * FROM coverage ORDER BY county_geoid, cohort_decade")

# --------------------------------------------------------------------------- #
# 6. Validation
# --------------------------------------------------------------------------- #

message("Validating ...")

one_stratum <- dbGetQuery(con, "
SELECT count(*) n FROM (
  SELECT HIK FROM bite_students GROUP BY HIK HAVING count(*) > 1)")$n
expect("every student has exactly one assignment", one_stratum == 0, one_stratum)

strata <- dbGetQuery(con, "
SELECT assignment_stratum, min(yob) lo, max(yob) hi, count(*) n
FROM bite_students GROUP BY 1 ORDER BY 1")
expect("assignment strata are a subset of the candidate census years",
       all(strata$assignment_stratum %in% CANDIDATE_YEARS),
       paste(strata$assignment_stratum, collapse = ", "))

dist_range <- dbGetQuery(con, "
SELECT assignment_stratum, min(selection_distance) lo, max(selection_distance) hi
FROM bite_students GROUP BY 1 ORDER BY 1")
# cand_age is capped at 5-18 and window midpoints span 9.5-13.5, so no chosen
# candidate can sit further than |18 - 9.5| = 8.5 from its target age.
expect("selection_distance never exceeds 8.5",
       all(dist_range$hi <= 8.5 + 1e-9),
       paste(sprintf("%d: %.1f-%.1f", dist_range$assignment_stratum,
                     dist_range$lo, dist_range$hi), collapse = "; "))

viol <- dbGetQuery(con, "
SELECT sum(CASE WHEN required_effective > required_nominal THEN 1 ELSE 0 END) actual,
       sum(CASE WHEN required_effective_cf > required_nominal_cf THEN 1 ELSE 0 END) cf
FROM bite_students")
expect("required_effective <= required_nominal (actual)", viol$actual == 0, viol$actual)
expect("required_effective_cf <= required_nominal_cf", viol$cf == 0, viol$cf)

# Structural: where no law bound, the shortfall measures can only be zero.
zero_bad <- county_stratum %>%
  filter(basis %in% ACTUAL_BASES, n_students > 0) %>%
  summarise(bad = sum(shortfall_rate < 0 | shortfall_rate > 1, na.rm = TRUE)) %>%
  pull(bad)
expect("shortfall_rate is a proportion on law-bound cells", zero_bad == 0, zero_bad)

imputed <- dbGetQuery(con, "
SELECT count(DISTINCT cand_statefip) states, count(DISTINCT (cand_statefip, yob)) cells
FROM bite_students WHERE required_is_imputed")
if (!overridden) {
  expect("imputation touches 17 states", imputed$states == 17, imputed$states)
  expect("imputation touches at most 231 state-cohort cells", imputed$cells <= 231, imputed$cells)
}

plateau_diff <- dbGetQuery(con, "
SELECT sum(CASE WHEN plateau_nominal <> plateau_effective THEN 1 ELSE 0 END) n
FROM plateau p
WHERE p.bpl IN (SELECT DISTINCT cand_statefip FROM bite_students WHERE required_is_imputed)")$n
if (!overridden) {
  expect("the two plateaus differ in 10 of the 17 imputation states",
         plateau_diff == 10, plateau_diff)
}

geoid <- dbGetQuery(con, "
SELECT sum(CASE WHEN county_geoid IS NOT NULL AND county_geoid < 1000 THEN 1 ELSE 0 END) malformed,
       sum(CASE WHEN NOT countyicp_standard THEN 1 ELSE 0 END) nonstandard,
       count(*) total FROM bite_students")
expect("every non-NULL county_geoid is well formed", geoid$malformed == 0, geoid$malformed)
expect("non-standard COUNTYICP codes are a rounding error, not a pattern",
       geoid$nonstandard / geoid$total < 0.001,
       sprintf("%d of %d (%.3f%%)", geoid$nonstandard, geoid$total,
               100 * geoid$nonstandard / geoid$total))

# The 1930 stratum is a single cohort (1912) observed at 18, which is at or past
# every statutory exit age. It is a legitimate assignment -- 18 is inside the
# candidate window -- but it is the worst-assigned group in the file and is
# isolated by selection_distance >= 5. Recorded so a change in its shape surfaces.
late <- dbGetQuery(con, "
SELECT count(*) n, count(DISTINCT yob) cohorts, min(cand_age) age_lo,
       min(selection_distance) dist_lo
FROM bite_students WHERE assignment_stratum = 1930")
if (!overridden && late$n > 0) {
  expect("the 1930 stratum is one cohort observed at 18",
         late$cohorts == 1 && late$age_lo == 18,
         sprintf("%d students, %d cohort(s), min age %d", late$n, late$cohorts, late$age_lo))
  expect("the 1930 stratum is under 2% of students",
         late$n / n_bite < 0.02, sprintf("%.2f%%", 100 * late$n / n_bite))
}

age_fit <- dbGetQuery(con, "
SELECT age_consistency AS diff, count(*) n FROM bite_students GROUP BY 1")
modal <- age_fit$diff[which.max(age_fit$n)]
# A property of the real linkage, not of a fixture: yob = 1940 - AGE - 1 sits a
# year below the earlier censuses' own arithmetic, so +1 is the consistent case.
if (!overridden) {
  expect("the modal age disagreement is +1, as the yob convention implies",
         modal == 1, modal)
}

# --------------------------------------------------------------------------- #
# 7. Write
# --------------------------------------------------------------------------- #

message("Writing ...")
students_out <- file.path(BITE_DIR, "bite_students.parquet")
dbExecute(con, sprintf("COPY bite_students_cov TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
                       gsub("\\\\", "/", students_out)))

write_csv(county_stratum, file.path(BITE_DIR, "bite_county_stratum.csv"))
write_parquet(county_stratum, file.path(BITE_DIR, "bite_county_stratum.parquet"))
write_parquet(county_cohort, file.path(BITE_DIR, "bite_county_cohort.parquet"))
write_csv(state_cohort, file.path(BITE_DIR, "bite_state_cohort.csv"))
write_csv(coverage_tbl, file.path(BITE_DIR, "bite_county_coverage.csv"))

manifest <- data.frame(
  input = names(inputs), path = unname(inputs),
  md5 = vapply(unname(inputs), function(p) unname(tools::md5sum(p)), character(1)),
  stringsAsFactors = FALSE)
write_csv(manifest, file.path(BITE_DIR, "input_manifest.csv"))

validation <- dplyr::bind_rows(checks)
write_csv(validation, file.path(BITE_DIR, "validation_summary.csv"))

message(sprintf("\nstudents %s | county x stratum rows %s | state x cohort rows %s",
                format(n_bite, big.mark = ","), format(nrow(county_stratum), big.mark = ","),
                format(nrow(state_cohort), big.mark = ",")))
message("Wrote: ", BITE_DIR)

if (!all(validation$validation_passed)) {
  stop(sum(!validation$validation_passed), " validation check(s) FAILED.")
}
message("All ", nrow(validation), " validation checks passed.")
