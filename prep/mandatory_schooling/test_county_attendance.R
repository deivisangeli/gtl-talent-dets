# Checks for build_county_attendance.R.
#
# Builds a synthetic fixture of children whose every rate is computable by hand,
# runs the real build script over it through COUNTY_ATTENDANCE_IN_DIR, and reads
# the outputs back. Each block names the failure mode it rules out.
#
# The fixture makes age standardisation do real work. Counties 01031 and 01033
# have IDENTICAL age-specific non-attendance -- 0.25 at ages 10 and 11, 0.75 at
# 12 and 13 -- but opposite age composition, so their RAW required-age rates
# differ (0.500 against 0.375) while their standardised rates must both be 0.500.
# A build that forgot to standardise would separate them.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/test_county_attendance.R

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

near <- function(a, b, tol = 1e-8) length(a) == 1 && !is.na(a) && abs(a - b) < tol

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

tmp <- if (nzchar(Sys.getenv("KEEP_FIXTURE"))) {
  Sys.getenv("KEEP_FIXTURE")
} else {
  file.path(tempdir(), paste0("att_fixture_", Sys.getpid()))
}
in_dir <- file.path(tmp, "in")
out_dir <- file.path(tmp, "out")
dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------------------------------- #
# Fixture
# --------------------------------------------------------------------------- #

# State 1: law throughout. Entry 8, exit 14, effective leaving age 12 -- so the
#          required window is ages 8-13, the effective window 8-11, and the
#          above-window benchmark ages 14-15.
# State 2: NO law in 1900, then entry 7 exit 16, so its counterfactual window --
#          the modal law-in-force window -- is ages 7-15.
limits <- bind_rows(
  tibble::tibble(statefip = 1L, year = c(1900L, 1910L, 1920L, 1930L),
                 law_in_force = TRUE, entryage = 8L, exitage = 14L,
                 effective_leaving_age = 12L),
  tibble::tibble(statefip = 2L, year = 1900L, law_in_force = FALSE,
                 entryage = 0L, exitage = 0L, effective_leaving_age = 0L),
  tibble::tibble(statefip = 2L, year = c(1910L, 1920L, 1930L), law_in_force = TRUE,
                 entryage = 7L, exitage = 16L, effective_leaving_age = 15L)
)
write_parquet(limits, file.path(in_dir, "cls_state_age_limits_1880_1930.parquet"))

# One row per child. SCHOOL: 1 = did not attend, 2 = attended, 8 = unknown.
kid <- function(age, statefip, countyicp, n, n_absent, n_unknown = 0L) {
  tibble::tibble(
    HIK = paste0("k", statefip, "-", countyicp, "-", age, "-", seq_len(n + n_unknown)),
    YEAR = 1900L, AGE = age, SEX = 1L, RACE = 1L,
    SCHOOL = c(rep(1L, n_absent), rep(2L, n - n_absent), rep(8L, n_unknown)),
    STATEFIP = statefip, COUNTYICP = countyicp,
    MOMLOC = 1L, POPLOC = 2L, GQ = 1L)
}

children <- bind_rows(
  # 01031: flat composition. Required ages 10-13 -> 32 children, 16 absent -> 0.500.
  kid(10L, 1L, 310L,  8L, 2L),   # 0.25
  kid(11L, 1L, 310L,  8L, 2L),   # 0.25
  kid(12L, 1L, 310L,  8L, 6L),   # 0.75
  kid(13L, 1L, 310L,  8L, 6L),   # 0.75   -> standardised 0.500
  kid(14L, 1L, 310L,  4L, 3L),   # above the exit age
  kid(15L, 1L, 310L,  4L, 1L),
  kid(18L, 1L, 310L,  2L, 2L),   # keeps an age-18 anchor in the profile
  kid(17L, 1L, 310L,  0L, 0L, n_unknown = 2L),   # two SCHOOL == 8, excluded
  # 01033: same age-specific rates, opposite composition -> raw 0.375, std 0.500.
  # Every age needs at least 5 children or the standardised rate is withheld.
  kid(10L, 1L, 330L, 24L,  6L),
  kid(11L, 1L, 330L, 24L,  6L),
  kid(12L, 1L, 330L,  8L,  6L),
  kid(13L, 1L, 330L,  8L,  6L),
  # 01035: a tiny population denominator, so coverage is implausible.
  kid(11L, 1L, 350L, 30L, 6L),
  # 02005: no law, so the counterfactual window 7-15 binds, and ages 16-17 give
  # the no-law discontinuity benchmark the real build needs.
  kid(10L, 2L,  50L,  8L, 2L),
  kid(11L, 2L,  50L,  8L, 2L),
  kid(12L, 2L,  50L,  8L, 2L),
  kid(13L, 2L,  50L,  8L, 2L),   # standardised 0.250
  kid(14L, 2L,  50L,  4L, 2L),
  kid(15L, 2L,  50L,  4L, 2L),   # required = ages 10-15 -> 40 children, 12 absent
  kid(16L, 2L,  50L,  4L, 3L),
  kid(17L, 2L,  50L,  4L, 3L)
)
write_parquet(children, file.path(in_dir, "candidates_pre1940.parquet"))

population <- tibble::tibble(
  GEOID = c("01031", "01033", "01035", "02005"),
  decade = 1900L,
  population = c(420, 640, 10, 480),
  source = "fixture")
write_csv(population, file.path(in_dir, "county_population.csv"))

# --------------------------------------------------------------------------- #
# Run the real script
# --------------------------------------------------------------------------- #

cat("\nRunning build_county_attendance.R over the fixture ...\n")
Sys.setenv(COUNTY_ATTENDANCE_IN_DIR = in_dir, COUNTY_ATTENDANCE_OUT_DIR = out_dir)
on.exit(Sys.unsetenv(c("COUNTY_ATTENDANCE_IN_DIR", "COUNTY_ATTENDANCE_OUT_DIR")), add = TRUE)
status <- system2("Rscript",
                  c(shQuote(file.path(repo_root,
                                      "prep/mandatory_schooling/build_county_attendance.R"))),
                  stdout = TRUE, stderr = TRUE)
if (!is.null(attr(status, "status")) && attr(status, "status") != 0) {
  cat(paste(status, collapse = "\n"), "\n")
  stop("build_county_attendance.R exited non-zero on the fixture")
}

cy <- read_csv(file.path(out_dir, "attendance_county_year.csv"), show_col_types = FALSE)
ca <- read_parquet(file.path(out_dir, "attendance_county_age.parquet"))
sy <- read_csv(file.path(out_dir, "attendance_state_year.csv"), show_col_types = FALSE)
kids <- read_parquet(file.path(out_dir, "attendance_children.parquet"))
validation <- read_csv(file.path(out_dir, "validation_summary.csv"), show_col_types = FALSE)

g <- function(geoid, field) cy[[field]][cy$county_geoid == geoid]
age_rate <- function(geoid, a) ca$not_attending[ca$county_geoid == geoid & ca$age == a]
st <- function(fip, field) sy[[field]][sy$statefip == fip]

cat("\nAttendance coding\n")
check("SCHOOL == 8 is counted as neither attending nor absent", function()
  sum(kids$attendance_known) == nrow(kids) - 2L)
check("SCHOOL == 2 is attendance and 1 is not", function()
  all(kids$attended[kids$SCHOOL == 2]) && !any(kids$attended[kids$SCHOOL == 1]))
check("an all-unknown age has no cell at all", function()
  length(age_rate(1031, 17)) == 0L)

cat("\nThe compulsory window\n")
check("the window is the state's own entry and exit ages", function()
  all(kids$in_window[kids$statefip == 1 & kids$age %in% 10:13]) &&
    !any(kids$in_window[kids$statefip == 1 & kids$age %in% c(14, 15, 18)]))
check("the effective window stops at the leaving age, not the exit age", function()
  all(kids$in_window_effective[kids$statefip == 1 & kids$age %in% 10:11]) &&
    !any(kids$in_window_effective[kids$statefip == 1 & kids$age %in% 12:13]))
check("above-window is the two ages past the exit age", function()
  all(kids$above_window[kids$statefip == 1 & kids$age %in% 14:15]) &&
    !any(kids$above_window[kids$statefip == 1 & kids$age == 18]))
check("a state with no law gets the counterfactual window instead", function()
  all(kids$in_window_cf[kids$statefip == 2 & kids$age %in% 10:15]) &&
    !any(kids$in_window[kids$statefip == 2]))
check("the counterfactual window is the modal law-in-force window", function()
  all(kids$cf_entryage[kids$statefip == 2] == 7) &&
    all(kids$cf_exitage[kids$statefip == 2] == 16))
check("ages past the counterfactual exit are outside it", function()
  !any(kids$in_window_cf[kids$statefip == 2 & kids$age %in% 16:17]))
check("the cell records which window it used", function()
  g(2005, "window_is_cf") == 1 && g(1031, "window_is_cf") == 0)

cat("\nRates\n")
check("age-specific non-attendance is computed per age", function()
  near(age_rate(1031, 10), 0.25) && near(age_rate(1031, 12), 0.75))
check("required-age non-attendance is 16 of 32", function()
  near(g(1031, "not_attending_required"), 0.5))
check("the effective window gives 4 of 16", function()
  near(g(1031, "not_attending_effective"), 0.25))
check("above-window non-attendance is 4 of 8", function()
  near(g(1031, "not_attending_above"), 0.5))
check("a no-law county's required rate uses the counterfactual window", function()
  near(g(2005, "not_attending_required"), 12 / 40))
check("above-window and effective are undefined without a law", function()
  is.na(g(2005, "not_attending_above")) && is.na(g(2005, "not_attending_effective")))

cat("\nAge standardisation\n")
check("the standardised rate is the mean of the age-specific rates", function()
  near(g(1031, "not_attending_std_1013"), 0.5))
check("identical age-specific rates standardise to the same value", function()
  near(g(1033, "not_attending_std_1013"), 0.5))
check("... even though the RAW rates differ, which is the point", function()
  near(g(1033, "not_attending_required"), 24 / 64) &&
    !near(g(1033, "not_attending_required"), g(1031, "not_attending_required")))
check("the standardised rate is NULL where the band is incomplete", function()
  is.na(g(1035, "not_attending_std_1013")))

cat("\nExit-age discontinuity\n")
check("below-exit is the two ages before the exit age", function()
  near(g(1031, "not_attending_below_exit"), 12 / 16))
check("above-exit is the two ages from the exit age", function()
  near(g(1031, "not_attending_above_exit"), 0.5))
check("the discontinuity is the difference", function()
  near(g(1031, "exit_discontinuity"), 0.5 - 12 / 16))
check("a county with no law has no discontinuity", function()
  is.na(g(2005, "exit_discontinuity")))
check("the state table carries a no-law benchmark", function()
  isFALSE(st(2, "law_in_force")) && near(st(2, "exit_discontinuity"), 0.75 - 0.5))
check("the benchmark uses the counterfactual exit age, not zero", function()
  near(st(2, "not_attending_below_exit"), 0.5) &&
    near(st(2, "not_attending_above_exit"), 0.75))

cat("\nCoverage and usability\n")
check("coverage is children over county population", function()
  near(g(1031, "coverage"), 42 / 420))
check("a tiny denominator is flagged implausible", function()
  isTRUE(g(1035, "coverage_implausible")) && !isTRUE(g(1031, "coverage_implausible")))
check("an implausible-coverage cell is not usable", function() !isTRUE(g(1035, "cell_usable")))
check("a plausible cell above the minimum is usable", function() isTRUE(g(1031, "cell_usable")))
check("the county key is STATEFIP*1000 + COUNTYICP/10", function()
  setequal(cy$county_geoid, c(1031, 1033, 1035, 2005)))

cat("\nThe script's own validation\n")
check("every validation check the script ran passed", function() all(validation$validation_passed))

if (!nzchar(Sys.getenv("KEEP_FIXTURE"))) unlink(tmp, recursive = TRUE)
cat(sprintf("\n%s\n", if (FAILURES == 0) "All checks passed."
            else sprintf("%d check(s) FAILED.", FAILURES)))
quit(status = if (FAILURES == 0) 0 else 1)
