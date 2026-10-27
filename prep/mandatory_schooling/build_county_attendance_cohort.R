# Build county x birth-cohort schooling measures around the first compulsory-
# schooling laws passed 1907-1920.
#
# For each of the 13 states whose first durable law took effect in 1907-1920, and
# for white, US-born, non-migrant men born 1890-1920, this compares the schooling
# of cohorts too old to be covered by the law with cohorts it covered, county by
# county. Specification: prep/mandatory_schooling/county_attendance_cohort.md.
# Inputs come from extract_linked_attendance_cohort_inputs.py; run that first.
#
# Definitions fixed by the design:
#
#   exposure      unexposed if age at T >= exitage + 1, i.e. yob <= T - exitage - 1;
#                 exposed otherwise. An 8-14 law passed in 1917 leaves the 1902
#                 cohort (15) unexposed and covers 1903 (14).
#   county        the man's EARLIEST linked COUNTYICP, used only if he was 18 or
#                 younger in that record. Otherwise he is unplaced -- counted in the
#                 funnel, never assigned.
#   law_min       exitage - entryage of the law as passed, held FIXED across every
#                 cohort in the state. The cohort's own requirement is 0 before the
#                 law and ramps up after it, so a gap against it would measure the
#                 yardstick changing (C7 in county_bite_from_linked_census.md).
#   outcomes      ever_attended = HIGRADED >= 040 (completed 1st grade);
#                 meets_min = years >= law_min; gap = years - law_min;
#                 years_hs = min(years, 12), the high-school cap.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/build_county_attendance_cohort.R
#
# Outputs (output/mandatory_schooling/attendance_cohort/):
#   law_events.csv                 the 13 law states and the pre-1907 benchmark states
#   students.parquet               one row per HIK, every flag
#   county_cohort.parquet / .csv   county x yob, headline sample, law states
#   county_exposure_contrast.csv   county x window x exposed definition, exposed - unexposed
#   state_cohort.csv               state x yob, law states and benchmark states
#   sample_funnel.csv, signal_diagnostics.csv, input_manifest.csv, validation_summary.csv

suppressPackageStartupMessages({
  library("DBI")
  library("duckdb")
  library("dplyr")
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

# ATT_COHORT_IN_DIR / _OUT_DIR let test_county_attendance_cohort.R drive the whole
# script over a synthetic fixture. When IN_DIR is overridden the law panel is
# expected there too, so the test controls the law dates.
IN_DIR <- Sys.getenv("ATT_COHORT_IN_DIR",
                     unset = file.path(data_root, "output/mandatory_schooling/attendance_cohort/inputs"))
OUT_DIR <- Sys.getenv("ATT_COHORT_OUT_DIR",
                      unset = file.path(data_root, "output/mandatory_schooling/attendance_cohort"))
overridden <- nzchar(Sys.getenv("ATT_COHORT_IN_DIR"))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

inputs <- c(
  men       = file.path(IN_DIR, "men_1940.parquet"),
  childhood = file.path(IN_DIR, "childhood_pre1940.parquet"),
  limits    = if (overridden) file.path(IN_DIR, "cls_state_age_limits_1880_1930.parquet")
              else file.path(data_root, "output/mandatory_schooling/cls/cls_state_age_limits_1880_1930.parquet")
)
absent <- inputs[!file.exists(inputs)]
if (length(absent)) {
  stop("Missing input(s):\n  ", paste(absent, collapse = "\n  "),
       "\nRun prep/mandatory_schooling/extract_linked_attendance_cohort_inputs.py and ",
       "build_cls_schooling_laws.R first.")
}

# --------------------------------------------------------------------------- #
# Constants
# --------------------------------------------------------------------------- #

YOB_MIN <- 1890L
YOB_MAX <- 1920L
LAW_T_MIN <- 1907L
LAW_T_MAX <- 1920L

# The earliest record locates the county of schooling only if the man was still
# a child in it.
PLACE_AGE_MAX <- 18L

# HIGRADED 040 = completed 1st grade; 010 = none; 011-032 nursery/kindergarten,
# which only 17 of 21.6 M white men report.
EVER_ATTENDED_CODE <- 40L
HS_YEARS <- 12L

MIN_CELL <- 25L
WINDOWS <- c(3L, 5L)

# Planning run, 2026-09-22 (scratch extract, same restrictions): county x cohort
# cells with n >= 25, all states. Reproducing them guards the yob, HIGRADED and
# placement conventions before anything county-level is read.
REF_EVER_CELL_MEAN <- 0.9888
REF_EVER_SIGNAL_RATIO <- 1.46

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
invisible(dbExecute(con, "PRAGMA threads=4"))
invisible(dbExecute(con, sprintf("PRAGMA temp_directory='%s'",
                                 gsub("\\\\", "/", file.path(tempdir(), "duckdb_spill")))))
invisible(dbExecute(con, "PRAGMA memory_limit='12GB'"))
x <- function(sql) invisible(dbExecute(con, sql))
g <- function(sql) dbGetQuery(con, sql)

# --------------------------------------------------------------------------- #
# 1. Law table
# --------------------------------------------------------------------------- #

message("Building law table ...")

limits <- read_parquet(inputs[["limits"]])

# T is the first year of the state's LAST law spell: Arizona's 1880-1882 spell was
# repealed, and "first year law_in_force" would date it 19 years early.
first_law <- limits %>%
  filter(law_in_force) %>%
  group_by(statefip) %>%
  filter(law_spell_id == max(law_spell_id)) %>%
  arrange(year, .by_group = TRUE) %>%
  slice_head(n = 1L) %>%
  ungroup() %>%
  transmute(statefip = as.integer(statefip), statename, T = as.integer(year),
            entryage = as.integer(entryage), exitage = as.integer(exitage),
            effective_leaving_age = as.integer(effective_leaving_age),
            left_censored = T == min(limits$year))

law_events <- first_law %>%
  mutate(
    role = case_when(T >= LAW_T_MIN & T <= LAW_T_MAX ~ "law_state",
                     T < LAW_T_MIN ~ "benchmark",
                     TRUE ~ "later_law"),
    law_min = exitage - entryage,
    law_min_effective = effective_leaving_age - entryage,
    last_unexposed_yob = T - exitage - 1L,
    first_exposed_yob = T - exitage,
    first_full_exposure_yob = T - entryage,
    n_unexposed_cohorts_in_window = pmax(0L, pmin(last_unexposed_yob, YOB_MAX) - YOB_MIN + 1L),
    n_exposed_cohorts_in_window = pmax(0L, YOB_MAX - pmax(first_exposed_yob, YOB_MIN) + 1L)
  ) %>%
  arrange(role, T, statename)

law_states <- filter(law_events, role == "law_state")
benchmark <- filter(law_events, role == "benchmark")

expect("every law state passed its first law in 1907-1920",
       all(law_states$T >= LAW_T_MIN & law_states$T <= LAW_T_MAX) && nrow(law_states) > 0,
       paste(law_states$statename, law_states$T, collapse = ", "))
if (!overridden) {
  expect("13 law states, as counted when the design was fixed",
         nrow(law_states) == 13L, nrow(law_states))
}
expect("every law state has both unexposed and exposed cohorts in 1890-1920",
       all(law_states$n_unexposed_cohorts_in_window > 0 & law_states$n_exposed_cohorts_in_window > 0))
expect("every benchmark state's law covers all of 1890-1920 already",
       all(benchmark$last_unexposed_yob < YOB_MIN),
       paste(benchmark$statename[benchmark$last_unexposed_yob >= YOB_MIN], collapse = ", "))
expect("law_min is positive everywhere", all(law_states$law_min > 0))
expect("the effective minimum never exceeds the nominal one",
       all(law_states$law_min_effective <= law_states$law_min))

duckdb::duckdb_register(con, "law_events_df", law_events)
x("CREATE TABLE laws AS SELECT * FROM law_events_df")

# --------------------------------------------------------------------------- #
# 2. Earliest record per HIK
# --------------------------------------------------------------------------- #

message("Locating each man's earliest record ...")

# The childhood table holds pre-1940 rows at AGE 0-18 only, so its earliest row is
# the man's earliest record whenever that record was taken at 18 or younger -- ages
# only rise across censuses. A man first linked as an adult has no row here and is
# unplaced, which is the rule.
x(sprintf("
CREATE TABLE earliest AS
SELECT HIK,
       min(YEAR)                   AS yr0,
       arg_min(AGE, YEAR)          AS age0,
       arg_min(STATEFIP, YEAR)     AS st0,
       arg_min(COUNTYICP, YEAR)    AS cty0,
       count(*)                    AS n_childhood_records
FROM read_parquet(%s)
GROUP BY HIK", q(inputs[["childhood"]])))

dup <- g("SELECT count(*) n FROM (SELECT HIK, YEAR FROM read_parquet(%s) GROUP BY ALL HAVING count(*) > 1)" |>
           sprintf(q(inputs[["childhood"]])))$n
expect("HIK appears at most once per census year in the childhood rows", dup == 0L, dup)

# --------------------------------------------------------------------------- #
# 3. Person-level panel
# --------------------------------------------------------------------------- #

message("Building person-level panel ...")

# yob = 1940 - AGE - 1: enumeration was 1 April, so most people had not yet had
# their birthday. years is CLS's construction from HIGRADED, as in
# build_county_bite.R and describe_cls_policy_environment.R.
x(sprintf("
CREATE TABLE students AS
WITH m AS (
  SELECT *, 1940 - CAST(AGE AS INTEGER) - 1 AS yob, CAST(FLOOR(HIGRADED / 10.0) AS INTEGER) AS g
  FROM read_parquet(%s)
),
p AS (
  SELECT m.HIK, m.AGE, m.yob, m.BPL, m.STATEFIP AS statefip_1940, m.COUNTYICP AS countyicp_1940,
         m.MIGRATE5, m.MIGPLAC5, m.GQ, m.SCHOOL, m.HIGRADED, m.EDUC,
         m.HIGRADED NOT IN (0, 999) AS has_attainment,
         CASE WHEN m.HIGRADED IN (0, 999) THEN NULL
              WHEN m.g <= 3 THEN 0 WHEN m.g <= 20 THEN m.g - 3 WHEN m.g <= 23 THEN 17 END AS years,
         e.yr0, e.age0, e.st0, e.cty0, e.n_childhood_records,
         coalesce(e.age0 <= %d, FALSE) AS placed,
         e.cty0 %% 10 = 0 AS countyicp_standard
  FROM m LEFT JOIN earliest e USING (HIK)
  WHERE m.yob BETWEEN %d AND %d
)
SELECT p.*,
       -- STATEFIP arrives as INT16 and would overflow at 47 * 1000
       CASE WHEN placed AND countyicp_standard THEN CAST(st0 AS INTEGER) * 1000 + cty0 // 10 END AS county_geoid,
       coalesce(BPL = statefip_1940 AND MIGRATE5 IN (1, 2) AND st0 = BPL, FALSE) AS nonmig,
       BPL = statefip_1940 AS nonmig_loose,
       least(years, %d) AS years_hs,
       CASE WHEN has_attainment THEN (HIGRADED >= %d)::INTEGER END AS ever_attended,
       CASE WHEN has_attainment THEN (years >= %d)::INTEGER END AS hs_complete,
       l.role, l.statename AS school_statename, l.T, l.entryage, l.exitage,
       l.law_min, l.law_min_effective, l.first_full_exposure_yob,
       coalesce(l.role = 'law_state', FALSE) AS in_law_state,
       yob - (l.T - l.exitage) AS rel_cohort,
       CASE WHEN l.role = 'law_state' THEN yob > l.T - l.exitage - 1 END AS exposed,
       CASE WHEN l.role = 'law_state' THEN yob >= l.T - l.entryage END AS full_exposure,
       -- ages in the CLS window [entryage, exitage) reached in or after T
       CASE WHEN l.role = 'law_state'
            THEN greatest(0, l.exitage - greatest(l.entryage, l.T - yob)) END AS years_exposed
FROM p LEFT JOIN laws l ON p.st0 = l.statefip",
  q(inputs[["men"]]), PLACE_AGE_MAX, YOB_MIN, YOB_MAX, HS_YEARS, EVER_ATTENDED_CODE, HS_YEARS))

x("ALTER TABLE students ADD COLUMN meets_min INTEGER")
x("ALTER TABLE students ADD COLUMN meets_min_effective INTEGER")
x("UPDATE students SET
     meets_min = CASE WHEN has_attainment AND in_law_state THEN (years >= law_min)::INTEGER END,
     meets_min_effective = CASE WHEN has_attainment AND in_law_state
                                THEN (years >= law_min_effective)::INTEGER END")
x("CREATE VIEW students_out AS SELECT *,
     CASE WHEN in_law_state THEN years_hs - law_min END AS gap_hs,
     CASE WHEN in_law_state THEN years - law_min END AS gap,
     CASE WHEN in_law_state THEN years_hs - law_min_effective END AS gap_hs_effective,
     -- random but reproducible halves for the split-half reliability
     hash(HIK) % 2 AS half
   FROM students")

# The headline analysis sample.
x("CREATE VIEW sample AS SELECT * FROM students_out
   WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL")

n_men <- g("SELECT count(*) n FROM students")$n
expect("no man outside yob 1890-1920", g(sprintf(
  "SELECT count(*) n FROM students WHERE yob NOT BETWEEN %d AND %d", YOB_MIN, YOB_MAX))$n == 0)
expect("every placed man's earliest record is at 18 or younger",
       g(sprintf("SELECT count(*) n FROM students WHERE placed AND age0 > %d", PLACE_AGE_MAX))$n == 0)
expect("exposure follows the leaving age + 1 rule in every law state",
       g("SELECT count(*) n FROM students WHERE in_law_state AND
            exposed <> ((T - yob) <= exitage)")$n == 0)
expect("rel_cohort is -1 at the last unexposed cohort and 0 at the first exposed",
       g("SELECT count(*) n FROM students WHERE in_law_state AND exposed <> (rel_cohort >= 0)")$n == 0)
expect("years_exposed is 0 for every unexposed man and never exceeds law_min",
       g("SELECT count(*) n FROM students WHERE in_law_state AND
            ((NOT exposed AND years_exposed <> 0) OR years_exposed > law_min)")$n == 0)
expect("fully exposed men face the whole requirement",
       g("SELECT count(*) n FROM students WHERE in_law_state AND full_exposure AND years_exposed <> law_min")$n == 0)
expect("county_geoid is NULL wherever COUNTYICP does not end in 0",
       g("SELECT count(*) n FROM students WHERE county_geoid IS NOT NULL AND NOT countyicp_standard")$n == 0)
expect("years_hs never exceeds 12", g("SELECT max(years_hs) m FROM students")$m <= HS_YEARS)
expect("the gap uses one fixed law_min per state across exposed and unexposed cohorts",
       g("SELECT count(*) n FROM (SELECT st0 FROM students WHERE in_law_state
            GROUP BY st0 HAVING count(DISTINCT law_min) > 1)")$n == 0)
expect("a non-migrant's school state is his birth state and his 1940 state",
       g("SELECT count(*) n FROM students WHERE nonmig AND (st0 <> BPL OR statefip_1940 <> BPL)")$n == 0)

# --------------------------------------------------------------------------- #
# 4. County x cohort
# --------------------------------------------------------------------------- #

message("Aggregating county x cohort ...")

# Binomial SE for shares, sd / sqrt(n) for means.
county_cohort <- g("
SELECT county_geoid, st0 AS statefip, school_statename AS statename, yob,
       any_value(T) AS T, any_value(exitage) AS exitage, any_value(law_min) AS law_min,
       any_value(law_min_effective) AS law_min_effective,
       any_value(rel_cohort) AS rel_cohort, bool_and(exposed) AS exposed,
       bool_and(full_exposure) AS full_exposure, any_value(years_exposed) AS years_exposed,
       count(*) AS n,
       avg(ever_attended)       AS share_ever_attended,
       avg(meets_min)           AS share_meets_min,
       avg(meets_min_effective) AS share_meets_min_effective,
       avg(hs_complete)         AS share_hs_complete,
       avg(gap_hs)              AS mean_gap_hs,
       avg(gap)                 AS mean_gap,
       avg(gap_hs_effective)    AS mean_gap_hs_effective,
       avg(years_hs)            AS mean_years_hs,
       sqrt(avg(ever_attended) * (1 - avg(ever_attended)) / count(*)) AS se_ever_attended,
       sqrt(avg(meets_min) * (1 - avg(meets_min)) / count(*))         AS se_meets_min,
       stddev_samp(gap_hs) / sqrt(count(*))                           AS se_gap_hs,
       stddev_samp(years_hs) / sqrt(count(*))                         AS se_years_hs,
       avg((yr0 = 1900)::INTEGER) AS share_placed_from_1900,
       avg((yr0 = 1910)::INTEGER) AS share_placed_from_1910,
       avg((yr0 = 1920)::INTEGER) AS share_placed_from_1920,
       avg((yr0 = 1930)::INTEGER) AS share_placed_from_1930,
       avg(age0) AS mean_age_at_placement
FROM sample
WHERE in_law_state
GROUP BY county_geoid, st0, school_statename, yob
ORDER BY county_geoid, yob") %>%
  mutate(below_min_cell = n < MIN_CELL)

expect("county x cohort cells carry one exposure status each",
       !anyNA(county_cohort$exposed))
expect("gap_hs equals mean_years_hs minus law_min in every cell",
       isTRUE(all(abs(county_cohort$mean_gap_hs - (county_cohort$mean_years_hs - county_cohort$law_min)) < 1e-9)))

# --------------------------------------------------------------------------- #
# 5. County before/after contrast
# --------------------------------------------------------------------------- #

message("Contrasting exposed and unexposed cohorts ...")

# Two exposed definitions:
#   ramp  the first k cohorts past the boundary (rel_cohort 0 .. k-1) -- the
#         user's rule; they are only partly covered.
#   full  the first k fully exposed cohorts (yob >= T - entryage).
# The unexposed side is always rel_cohort -k .. -1.
outcomes <- c(ever_attended = "binary", meets_min = "binary", meets_min_effective = "binary",
              hs_complete = "binary", gap_hs = "mean", gap = "mean", years_hs = "mean")

contrast_sql <- function(k, def, where = "TRUE", by = "county_geoid, st0, school_statename") {
  exposed_cond <- if (def == "ramp") {
    sprintf("rel_cohort BETWEEN 0 AND %d", k - 1L)
  } else {
    sprintf("yob BETWEEN first_full_exposure_yob AND first_full_exposure_yob + %d", k - 1L)
  }
  side <- sprintf("CASE WHEN rel_cohort BETWEEN %d AND -1 THEN 'u' WHEN %s THEN 'e' END", -k, exposed_cond)
  terms <- unlist(lapply(names(outcomes), function(o) c(
    sprintf("avg(%1$s) FILTER (WHERE side = 'e') - avg(%1$s) FILTER (WHERE side = 'u') AS diff_%1$s", o),
    sprintf("avg(%1$s) FILTER (WHERE side = 'u') AS unexp_%1$s", o),
    sprintf("avg(%1$s) FILTER (WHERE side = 'e') AS exp_%1$s", o),
    sprintf("sqrt(var_samp(%1$s) FILTER (WHERE side = 'e') / count(%1$s) FILTER (WHERE side = 'e') +
                  var_samp(%1$s) FILTER (WHERE side = 'u') / count(%1$s) FILTER (WHERE side = 'u')) AS se_%1$s", o))))
  sprintf("
    SELECT %s, %d AS window_k, '%s' AS exposed_def,
           count(*) FILTER (WHERE side = 'u') AS n_unexp,
           count(*) FILTER (WHERE side = 'e') AS n_exp,
           count(DISTINCT yob) FILTER (WHERE side = 'u') AS n_cohorts_unexp,
           count(DISTINCT yob) FILTER (WHERE side = 'e') AS n_cohorts_exp,
           %s
    FROM (SELECT *, %s AS side FROM sample WHERE in_law_state AND %s) s
    WHERE side IS NOT NULL
    GROUP BY %s", by, k, def, paste(terms, collapse = ",\n           "), side, where, by)
}

grid <- expand.grid(k = WINDOWS, def = c("ramp", "full"), stringsAsFactors = FALSE)
contrast <- bind_rows(lapply(seq_len(nrow(grid)), function(i) g(contrast_sql(grid$k[i], grid$def[i])))) %>%
  rename(statefip = st0, statename = school_statename) %>%
  left_join(select(law_states, statefip, last_unexposed_yob), by = "statefip") %>%
  # A property of the state's boundary, not of the county: the 1890 start leaves
  # fewer than k unexposed cohorts to average over.
  mutate(below_min_cell = is.na(n_unexp) | is.na(n_exp) | n_unexp < MIN_CELL | n_exp < MIN_CELL,
         short_pre_window = last_unexposed_yob - YOB_MIN + 1L < window_k) %>%
  select(-last_unexposed_yob) %>%
  arrange(window_k, exposed_def, county_geoid)

expect("no county contrast uses an unexposed cohort on the exposed side",
       g("SELECT count(*) n FROM sample WHERE in_law_state AND rel_cohort BETWEEN 0 AND 4 AND NOT exposed")$n == 0)
expect("no county draws more unexposed cohorts than its window allows",
       all(contrast$n_cohorts_unexp <= contrast$window_k & contrast$n_cohorts_exp <= contrast$window_k,
           na.rm = TRUE))

# --------------------------------------------------------------------------- #
# 6. State x cohort, with the pre-1907 benchmark
# --------------------------------------------------------------------------- #

message("Aggregating state x cohort ...")

state_cohort <- bind_rows(
  g("SELECT st0 AS statefip, school_statename AS statename, role, yob,
            any_value(rel_cohort) AS rel_cohort, bool_and(exposed) AS exposed,
            count(*) AS n, avg(ever_attended) share_ever_attended, avg(meets_min) share_meets_min,
            avg(hs_complete) share_hs_complete, avg(gap_hs) mean_gap_hs, avg(years_hs) mean_years_hs
     FROM sample WHERE role IN ('law_state', 'benchmark') GROUP BY ALL"),
  # Every benchmark cohort is already covered, so this series is the secular
  # trend the law states have to be read against. law_min is not defined for it.
  g("SELECT NULL::INTEGER AS statefip, 'BENCHMARK (first law before 1907)' AS statename,
            'benchmark_pooled' AS role, yob, count(*) AS n,
            avg(ever_attended) share_ever_attended, avg(hs_complete) share_hs_complete,
            avg(years_hs) mean_years_hs
     FROM sample WHERE role = 'benchmark' GROUP BY yob")
) %>% arrange(role, statename, yob)

# --------------------------------------------------------------------------- #
# 7. Diagnostics
# --------------------------------------------------------------------------- #

message("Diagnostics ...")

funnel <- g("
SELECT yob,
       count(*) AS white_usborn_men,
       count(*) FILTER (WHERE has_attainment) AS with_attainment,
       count(*) FILTER (WHERE has_attainment AND placed) AS placed,
       count(*) FILTER (WHERE has_attainment AND placed AND nonmig) AS placed_nonmig,
       count(*) FILTER (WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL) AS with_county,
       count(*) FILTER (WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL
                          AND in_law_state) AS in_law_state,
       avg(ever_attended) FILTER (WHERE placed AND nonmig AND county_geoid IS NOT NULL) AS ever_attended_sample,
       avg(ever_attended) FILTER (WHERE NOT placed) AS ever_attended_unplaced,
       avg(years_hs) FILTER (WHERE placed AND nonmig AND county_geoid IS NOT NULL) AS years_hs_sample,
       avg(years_hs) FILTER (WHERE NOT placed) AS years_hs_unplaced
FROM students GROUP BY yob ORDER BY yob") %>%
  mutate(share_placed = placed / with_attainment,
         share_placed_nonmig = placed_nonmig / with_attainment)

# Signal: within state x cohort, is the spread of county values larger than
# sampling noise alone would produce? Ratio 1 means the county dimension carries
# nothing. Cells of n >= MIN_CELL.
signal_sql <- function(outcome, type, scope_where, scope) {
  v <- if (type == "binary") "y * (1 - y) / n" else "s2 / n"
  sprintf("
    WITH cc AS (SELECT county_geoid, st0, yob, count(*) n, avg(%1$s) y, var_samp(%1$s) s2
                FROM sample WHERE %2$s AND %1$s IS NOT NULL GROUP BY ALL HAVING count(*) >= %3$d),
         d AS (SELECT *, y - avg(y) OVER (PARTITION BY st0, yob) dev, %4$s v FROM cc)
    SELECT '%1$s' AS outcome, '%5$s' AS scope, count(*) AS cells, avg(y) AS mean_cell,
           avg((y IN (0, 1))::INTEGER) AS share_cells_at_bound,
           stddev_samp(dev) AS sd_within, sqrt(avg(v)) AS noise_floor,
           stddev_samp(dev) / sqrt(avg(v)) AS signal_ratio
    FROM d", outcome, scope_where, MIN_CELL, v, scope)
}
signal <- bind_rows(
  lapply(names(outcomes), function(o) g(signal_sql(o, outcomes[[o]], "in_law_state", "law_states"))),
  lapply(c("ever_attended", "hs_complete", "years_hs"),
         function(o) g(signal_sql(o, outcomes[[o]], "TRUE", "all_states")))
)

# Split-half reliability of the county contrast: the same county's diff computed
# on two random halves of its men. Spearman-Brown gives the full-sample value.
split <- bind_rows(lapply(0:1, function(h) g(contrast_sql(5L, "ramp", sprintf("half = %d", h))) %>%
                            mutate(half = h))) %>%
  filter(n_unexp >= MIN_CELL, n_exp >= MIN_CELL)
reliability <- bind_rows(lapply(c("ever_attended", "meets_min", "gap_hs", "years_hs"), function(o) {
  col <- paste0("diff_", o)
  w <- inner_join(
    split %>% filter(half == 0L) %>% select(county_geoid, h0 = all_of(col)),
    split %>% filter(half == 1L) %>% select(county_geoid, h1 = all_of(col)),
    by = "county_geoid") %>%
    filter(!is.na(h0), !is.na(h1))
  r <- if (nrow(w) > 2) suppressWarnings(cor(w$h0, w$h1)) else NA_real_
  data.frame(outcome = o, scope = "law_states", split_half_counties = nrow(w),
             split_half_r = r, split_half_r_spearman_brown = 2 * r / (1 + r))
}))
signal <- left_join(signal, reliability, by = c("outcome", "scope"))

if (!overridden) {
  ref <- filter(signal, outcome == "ever_attended", scope == "all_states")
  expect("ever-attended cell mean reproduces the planning run",
         abs(ref$mean_cell - REF_EVER_CELL_MEAN) < 0.001, round(ref$mean_cell, 4))
  expect("ever-attended signal ratio reproduces the planning run",
         abs(ref$signal_ratio - REF_EVER_SIGNAL_RATIO) < 0.02, round(ref$signal_ratio, 3))
}

# --------------------------------------------------------------------------- #
# 8. Write
# --------------------------------------------------------------------------- #

message("Writing ...")

write_csv(law_events, file.path(OUT_DIR, "law_events.csv"))
x(sprintf("COPY (SELECT * FROM students_out) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
          gsub("\\\\", "/", file.path(OUT_DIR, "students.parquet"))))
write_parquet(county_cohort, file.path(OUT_DIR, "county_cohort.parquet"))
write_csv(county_cohort, file.path(OUT_DIR, "county_cohort.csv"))
write_csv(contrast, file.path(OUT_DIR, "county_exposure_contrast.csv"))
write_csv(state_cohort, file.path(OUT_DIR, "state_cohort.csv"))
write_csv(funnel, file.path(OUT_DIR, "sample_funnel.csv"))
write_csv(signal, file.path(OUT_DIR, "signal_diagnostics.csv"))

manifest <- data.frame(
  input = names(inputs), path = unname(inputs),
  md5 = vapply(unname(inputs), function(p) unname(tools::md5sum(p)), character(1)),
  stringsAsFactors = FALSE)
write_csv(manifest, file.path(OUT_DIR, "input_manifest.csv"))

validation <- bind_rows(checks)
write_csv(validation, file.path(OUT_DIR, "validation_summary.csv"))

message(sprintf("\nmen %s | analysis sample %s | county x cohort cells %s | county contrasts %s",
                format(n_men, big.mark = ","),
                format(g("SELECT count(*) n FROM sample WHERE in_law_state")$n, big.mark = ","),
                format(nrow(county_cohort), big.mark = ","),
                format(nrow(contrast), big.mark = ",")))
message("Wrote: ", OUT_DIR)

if (!all(validation$validation_passed)) {
  stop(sum(!validation$validation_passed), " validation check(s) FAILED.")
}
message("All ", nrow(validation), " validation checks passed.")
