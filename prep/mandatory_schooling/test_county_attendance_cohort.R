# Checks for build_county_attendance_cohort.R.
#
# Builds a synthetic fixture whose every answer is known by hand, runs the real
# build script over it through ATT_COHORT_IN_DIR, and reads the outputs back.
# Each block names the failure mode it rules out.
#
# Three states:
#   1 Alpha   first law 1917, entry 8 exit 14 (law_min 6), effective leaving age 13.
#             Last unexposed cohort 1902 (15 in 1917), first exposed 1903 (14),
#             first fully exposed 1909 (8).
#   2 Beta    law in force from 1880 -- a benchmark state, every cohort covered.
#   3 Gamma   a spell 1880-1882, repealed, then a durable law from 1912, entry 8
#             exit 16. T must be 1912, not 1880.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/test_county_attendance_cohort.R

suppressPackageStartupMessages({
  library("arrow")
  library("dplyr")
  library("readr")
})

FAILURES <- 0

check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr()), error = function(e) {
    message("    error: ", conditionMessage(e)); FALSE
  })
  if (!ok) FAILURES <<- FAILURES + 1L
  cat(sprintf("  %s  %s\n", if (ok) "PASS" else "FAIL", label))
}

near <- function(a, b, tol = 1e-8) length(a) == length(b) && all(!is.na(a)) && all(abs(a - b) < tol)

repo_root <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  } else {
    Sys.getenv("GTL_REPO", unset = getwd())
  }
  while (!file.exists(file.path(candidate, "paths.R"))) candidate <- dirname(candidate)
  normalizePath(candidate, winslash = "/")
})

tmp <- file.path(tempdir(), paste0("att_cohort_fixture_", Sys.getpid()))
in_dir <- file.path(tmp, "in")
out_dir <- file.path(tmp, "out")
dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------------------------------- #
# Fixture: law panel
# --------------------------------------------------------------------------- #

years <- 1880:1930
panel <- function(fip, name, in_force, spell, entry, exit, eff) {
  data.frame(statefip = fip, statename = name, year = years,
             law_in_force = in_force, law_spell_id = ifelse(in_force, spell, NA_integer_),
             entryage = ifelse(in_force, entry, 0L), exitage = ifelse(in_force, exit, 0L),
             effective_leaving_age = ifelse(in_force, eff, NA_integer_))
}
limits <- bind_rows(
  panel(1L, "Alpha", years >= 1917, 1L, 8L, 14L, 13L),
  panel(2L, "Beta", rep(TRUE, length(years)), 1L, 7L, 14L, 14L),
  panel(3L, "Gamma", years <= 1882 | years >= 1912,
        ifelse(years <= 1882, 1L, 2L), 8L, 16L, 16L)
)
write_parquet(limits, file.path(in_dir, "cls_state_age_limits_1880_1930.parquet"))

# --------------------------------------------------------------------------- #
# Fixture: men and their childhood records
# --------------------------------------------------------------------------- #

# HIGRADED: 010 none (0 yrs), 070 4th grade (4), 090 6th (6), 110 8th (8),
# 150 12th (12), 190 4th year of college (16), 999 N/A.
man <- function(hik, yob, higraded, bpl = 1L, st40 = bpl, migrate5 = 1L) {
  data.frame(HIK = hik, AGE = 1940L - yob - 1L, BPL = bpl, HIGRADED = higraded, EDUC = 2L,
             SCHOOL = 1L, STATEFIP = st40, COUNTYICP = 310L, MIGRATE5 = migrate5,
             MIGPLAC5 = 0L, GQ = 1L)
}
kid <- function(hik, year, yob, st = 1L, cty = 310L) {
  data.frame(HIK = hik, YEAR = year, AGE = year - yob, SEX = 1L, RACE = 1L,
             STATEFIP = st, COUNTYICP = cty)
}

men <- bind_rows(
  man("A", 1902L, 110L),              # unexposed, 8 years
  man("B", 1903L, 70L),               # first exposed cohort, 4 years
  man("C", 1901L, 10L),               # unexposed, never attended
  man("D", 1904L, 150L),              # exposed, 12 years
  man("E", 1910L, 190L),              # fully exposed, 16 years -> capped at 12
  man("F", 1895L, 110L),              # no childhood record -> unplaced
  man("G", 1892L, 90L),               # earliest record at exactly 18 -> placed
  man("H", 1905L, 110L, st40 = 2L),   # lives in another state in 1940
  man("I", 1905L, 110L, migrate5 = 3L),  # moved between states since 1935
  man("J", 1905L, 999L),              # no attainment
  man("K", 1905L, 110L),              # childhood COUNTYICP ends in 5
  man("L", 1905L, 110L),              # grew up in another state
  man("M", 1895L, 110L, bpl = 3L),    # Gamma, last unexposed (1912 - 16 - 1)
  man("N", 1896L, 110L, bpl = 3L),    # Gamma, first exposed
  man("P", 1900L, 110L, bpl = 2L),    # benchmark state
  man("Q", 1889L, 110L),              # yob 1889 -> outside the window
  man("R", 1921L, 110L)               # yob 1921 -> outside the window
)
childhood <- bind_rows(
  kid("A", 1910L, 1902L), kid("A", 1920L, 1902L, cty = 330L),  # earliest is 1910
  kid("B", 1910L, 1903L), kid("C", 1910L, 1901L), kid("D", 1910L, 1904L),
  kid("E", 1920L, 1910L), kid("G", 1910L, 1892L),
  kid("H", 1910L, 1905L), kid("I", 1910L, 1905L), kid("J", 1910L, 1905L),
  kid("K", 1910L, 1905L, cty = 315L), kid("L", 1910L, 1905L, st = 2L),
  kid("M", 1900L, 1895L, st = 3L), kid("N", 1900L, 1896L, st = 3L),
  kid("P", 1910L, 1900L, st = 2L), kid("Q", 1900L, 1889L), kid("R", 1930L, 1921L),
  kid("nobody", 1900L, 1890L)          # a childhood row with no 1940 man
)
write_parquet(men, file.path(in_dir, "men_1940.parquet"))
write_parquet(childhood, file.path(in_dir, "childhood_pre1940.parquet"))

# --------------------------------------------------------------------------- #
# Run the real script
# --------------------------------------------------------------------------- #

cat("\nRunning build_county_attendance_cohort.R over the fixture ...\n")
# system2(env=) prepends VAR=value on Windows instead of setting the
# environment, so set them here and let the child inherit.
Sys.setenv(ATT_COHORT_IN_DIR = in_dir, ATT_COHORT_OUT_DIR = out_dir)
on.exit(Sys.unsetenv(c("ATT_COHORT_IN_DIR", "ATT_COHORT_OUT_DIR")), add = TRUE)
status <- system2("Rscript",
                  shQuote(file.path(repo_root, "prep/mandatory_schooling/build_county_attendance_cohort.R")),
                  stdout = TRUE, stderr = TRUE)
if (!is.null(attr(status, "status")) && attr(status, "status") != 0) {
  cat(paste(status, collapse = "\n"), "\n")
  stop("build_county_attendance_cohort.R exited non-zero on the fixture")
}

s <- read_parquet(file.path(out_dir, "students.parquet")) %>% arrange(HIK)
laws <- read_csv(file.path(out_dir, "law_events.csv"), show_col_types = FALSE)
cc <- read_csv(file.path(out_dir, "county_cohort.csv"), show_col_types = FALSE)
ct <- read_csv(file.path(out_dir, "county_exposure_contrast.csv"), show_col_types = FALSE)
st <- read_csv(file.path(out_dir, "state_cohort.csv"), show_col_types = FALSE)
funnel <- read_csv(file.path(out_dir, "sample_funnel.csv"), show_col_types = FALSE)
validation <- read_csv(file.path(out_dir, "validation_summary.csv"), show_col_types = FALSE)

v <- function(hik, column) s[[column]][s$HIK == hik]
row_ct <- function(k, def) filter(ct, county_geoid == 1031L, window_k == k, exposed_def == def)

cat("\nLaw table\n")
check("Alpha and Gamma are law states, Beta is a benchmark",
      function() identical(sort(laws$statename[laws$role == "law_state"]), c("Alpha", "Gamma")) &&
        laws$role[laws$statename == "Beta"] == "benchmark")
check("Gamma's T is its durable 1912 law, not the repealed 1880 spell",
      function() laws$T[laws$statename == "Gamma"] == 1912L)
check("Alpha: law_min 6, effective 5, last unexposed 1902, first full exposure 1909",
      function() with(filter(laws, statename == "Alpha"),
                      law_min == 6L && law_min_effective == 5L &&
                        last_unexposed_yob == 1902L && first_full_exposure_yob == 1909L))

cat("\nSample window and attainment\n")
check("yob 1889 and 1921 are outside the window", function() !any(c("Q", "R") %in% s$HIK))
check("HIGRADED 999 has no attainment and no outcome",
      function() !v("J", "has_attainment") && is.na(v("J", "ever_attended")))
check("HIGRADED 010 is never attended, 0 years",
      function() v("C", "ever_attended") == 0L && v("C", "years") == 0L)
check("16 years is capped at 12 in years_hs but not in years",
      function() v("E", "years_hs") == 12L && v("E", "years") == 16L)

cat("\nExposure\n")
check("8-14 law of 1917: 1902 (aged 15) is unexposed, 1903 (aged 14) exposed",
      function() !v("A", "exposed") && v("B", "exposed"))
check("rel_cohort is -1 at the last unexposed cohort and 0 at the first exposed",
      function() v("A", "rel_cohort") == -1L && v("B", "rel_cohort") == 0L)
check("the 1903 cohort is exposed but had no year of its window left (14 at T)",
      function() v("B", "years_exposed") == 0L && v("D", "years_exposed") == 1L)
check("a fully exposed cohort faces the whole requirement",
      function() v("E", "full_exposure") && v("E", "years_exposed") == 6L)
check("Gamma: 1895 (17 in 1912) unexposed, 1896 (16) exposed",
      function() !v("M", "exposed") && v("N", "exposed"))

cat("\nCounty of schooling\n")
check("the earliest record gives the county: 1910 over 1920",
      function() v("A", "yr0") == 1910L && v("A", "county_geoid") == 1031L)
check("a man with no childhood record is unplaced",
      function() !v("F", "placed") && is.na(v("F", "county_geoid")))
check("an earliest record at exactly 18 places the man",
      function() v("G", "placed") && v("G", "age0") == 18L)
check("COUNTYICP ending in 5 gives a NULL county_geoid",
      function() v("K", "placed") && is.na(v("K", "county_geoid")))

cat("\nNon-migrants\n")
check("living in another state in 1940 fails both rules",
      function() !v("H", "nonmig") && !v("H", "nonmig_loose"))
check("a 1935 interstate move fails the strict rule only",
      function() !v("I", "nonmig") && v("I", "nonmig_loose"))
check("growing up outside the birth state fails the strict rule",
      function() !v("L", "nonmig"))
check("the stayers pass", function() all(sapply(c("A", "B", "C", "D", "E", "G"), v, "nonmig")))

cat("\nOutcomes against the fixed minimum\n")
check("gap uses law_min 6 for both unexposed and exposed men",
      function() v("A", "gap_hs") == 2L && v("B", "gap_hs") == -2L && v("C", "gap_hs") == -6L)
check("meets_min: 8 and 6 years clear it, 4 does not",
      function() v("A", "meets_min") == 1L && v("G", "meets_min") == 1L && v("B", "meets_min") == 0L)
check("the uncapped gap keeps college years", function() v("E", "gap") == 10L && v("E", "gap_hs") == 6L)
check("meets_min_effective uses the effective minimum of 5",
      function() v("B", "meets_min_effective") == 0L && v("G", "meets_min_effective") == 1L)

cat("\nCounty x cohort\n")
check("only the analysis sample reaches the county table (A B C D E G in 1031)",
      function() sum(cc$n[cc$county_geoid == 1031L]) == 6L)
check("benchmark men never enter the county table", function() !(2L %in% cc$statefip))
check("the 1902 cell: n 1, ever attended 1, gap 2",
      function() with(filter(cc, county_geoid == 1031L, yob == 1902L),
                      n == 1L && share_ever_attended == 1 && mean_gap_hs == 2))
check("cells under 25 men are flagged, not dropped", function() all(cc$below_min_cell))

cat("\nCounty contrast\n")
check("k=3 ramp: unexposed A C, exposed B D",
      function() with(row_ct(3L, "ramp"), n_unexp == 2L && n_exp == 2L))
check("k=3 ramp: ever attended rises 0.5 (1 vs 0.5)",
      function() near(row_ct(3L, "ramp")$diff_ever_attended, 0.5))
check("k=3 ramp: years_hs and gap_hs both rise 4 (8 vs 4)",
      function() near(row_ct(3L, "ramp")$diff_years_hs, 4) && near(row_ct(3L, "ramp")$diff_gap_hs, 4))
check("k=3 ramp: meets_min does not move (0.5 vs 0.5)",
      function() near(row_ct(3L, "ramp")$diff_meets_min, 0))
check("k=3 full: the exposed side is E alone, years_hs up 8",
      function() with(row_ct(3L, "full"), n_exp == 1L && near(diff_years_hs, 8)))
check("tiny contrasts are flagged below_min_cell", function() all(ct$below_min_cell))

cat("\nState table and funnel\n")
check("the pooled benchmark series exists", function() any(st$role == "benchmark_pooled"))
check("the funnel counts every man in the window",
      function() sum(funnel$white_usborn_men) == nrow(s))
check("the builder's own validation passed", function() all(validation$validation_passed))

cat(sprintf("\n%s\n", if (FAILURES == 0) "All checks passed." else paste(FAILURES, "check(s) FAILED.")))
quit(status = if (FAILURES == 0) 0 else 1)
