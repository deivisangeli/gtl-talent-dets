# Checks for build_county_bite.R.
#
# Builds a synthetic fixture of seven students whose every answer is known by
# hand, runs the real build script over it through COUNTY_BITE_IN_DIR, and reads
# the outputs back. Each block names the failure mode it rules out.
#
# The fixture is designed so the year-selection rule has to do real work: two
# students share birth cohort 1894 and both have candidates in 1900 (age 6) and
# 1910 (age 16), but they live in different states. State 1's window midpoint is
# 11, which ties, so the rule takes the earlier year. State 2 has no law in 1900
# -- so the fallback target 11.5 applies -- and a midpoint of 12.5 in 1910, which
# picks 1910. Same cohort, same ages, opposite assignment.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/test_county_bite.R

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

near <- function(a, b, tol = 1e-8) length(a) == length(b) && all(abs(a - b) < tol)

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
  file.path(tempdir(), paste0("bite_fixture_", Sys.getpid()))
}
in_dir <- file.path(tmp, "in")
out_dir <- file.path(tmp, "out")
dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------------------------------- #
# Fixture
# --------------------------------------------------------------------------- #

# State 1: law throughout, entry 8 exit 14 -> midpoint 11.
# State 2: NO law in 1900, then entry 7 exit 18 -> midpoint 12.5.
limits <- bind_rows(
  tibble::tibble(statefip = 1L, year = c(1900L, 1910L, 1920L, 1930L),
                 law_in_force = TRUE, entryage = 8L, exitage = 14L),
  tibble::tibble(statefip = 2L, year = 1900L,
                 law_in_force = FALSE, entryage = 0L, exitage = 0L),
  tibble::tibble(statefip = 2L, year = c(1910L, 1920L, 1930L),
                 law_in_force = TRUE, entryage = 7L, exitage = 18L),
  # A high FIPS code, because 48 * 1000 overflows the INT16 that STATEFIP
  # arrives as.
  tibble::tibble(statefip = 48L, year = c(1900L, 1910L, 1920L, 1930L),
                 law_in_force = TRUE, entryage = 8L, exitage = 14L)
)
write_parquet(limits, file.path(in_dir, "cls_state_age_limits_1880_1930.parquet"))

# State 1 always bound: nominal 6, effective 5.
# State 2 unexposed through cohort 1900, then nominal 4, effective 3.
#   -> plateau_nominal = 4, plateau_effective = 3.
yobs <- 1885:1912
cohorts <- bind_rows(
  tibble::tibble(bpl = 1L, yob = yobs, statename = "Testland", region = 1L,
                 region_label = "Northeast", ca_years1 = 6L, ca_years = 5L,
                 ca_years_combined = 5L),
  tibble::tibble(bpl = 2L, yob = yobs, statename = "Fixtureland", region = 2L,
                 region_label = "Midwest",
                 ca_years1 = ifelse(yobs <= 1900L, 0L, 4L),
                 ca_years = ifelse(yobs <= 1900L, 0L, 3L),
                 ca_years_combined = ifelse(yobs <= 1900L, 0L, 3L)),
  tibble::tibble(bpl = 48L, yob = yobs, statename = "Bigfipsia", region = 3L,
                 region_label = "South", ca_years1 = 6L, ca_years = 5L,
                 ca_years_combined = 5L)
)
write_parquet(cohorts, file.path(in_dir, "cls_cohort_requirements_1875_1912.parquet"))

# HIGRADED -> observed_years: 110->8, 30->0, 230->17, 70->4, 60->3, 999 dropped.
students <- tibble::tibble(
  HIK      = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  # yob 1894,1894,1890,1890,1905,1905,1905,1889,1895. H and I exist to catch a
  # rounding cast: DuckDB rounds DOUBLE -> INTEGER, so a bare (yob/10)::INTEGER
  # puts 1889 in the 1890s and 1895 in the 1900s.
  AGE      = c(45L, 45L, 49L, 49L, 34L, 34L, 34L, 50L, 44L, 45L),
  SEX      = 1L, RACE = 1L,
  BPL      = c(1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 48L),
  HIGRADED = c(110L, 110L, 30L, 230L, 70L, 999L, 60L, 110L, 110L, 110L),
  EDUC     = 6L
)
write_parquet(students, file.path(in_dir, "students_1940.parquet"))

candidates <- tibble::tibble(
  HIK       = c("A", "A", "B", "B", "C", "D", "E", "E", "F", "G", "H", "I", "J"),
  YEAR      = c(1900L, 1910L, 1900L, 1910L, 1900L, 1900L, 1910L, 1920L, 1920L,
                1920L, 1900L, 1910L, 1900L),
  AGE       = c(6L, 16L, 6L, 16L, 10L, 10L, 5L, 15L, 15L, 15L, 11L, 15L, 6L),
  STATEFIP  = c(1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 48L),
  COUNTYICP = c(310L, 310L, 50L, 50L, 50L, 50L, 310L, 310L, 310L, 310L, 310L,
                310L, 10L),
  MOMLOC    = 1L, POPLOC = 2L, GQ = 1L
)
write_parquet(candidates, file.path(in_dir, "candidates_pre1940.parquet"))

# cohort_decade = (yob %/% 10) * 10, so 1894 and 1890 -> 1890, 1905 -> 1900.
# The real column carries literal "NA" strings, which makes DuckDB type the whole
# column VARCHAR -- so the fixture carries one too.
births <- tibble::tibble(
  GEOID = c("01031", "01031", "01031", "02005", "48001"),
  decade = c(1880L, 1890L, 1900L, 1890L, 1890L),
  county_births_estimate_decade = c("400", "100", "200", "50", "NA")
)
write_csv(births, file.path(in_dir, "us_panel_county_stem_1800.csv"))

# --------------------------------------------------------------------------- #
# Run the real script
# --------------------------------------------------------------------------- #

cat("\nRunning build_county_bite.R over the fixture ...\n")
# system2(env=) prepends VAR=value to the command line on Windows instead of
# setting the environment, so set them here and let the child inherit.
Sys.setenv(COUNTY_BITE_IN_DIR = in_dir, COUNTY_BITE_OUT_DIR = out_dir)
on.exit(Sys.unsetenv(c("COUNTY_BITE_IN_DIR", "COUNTY_BITE_OUT_DIR")), add = TRUE)
status <- system2("Rscript",
                  c(shQuote(file.path(repo_root, "prep/mandatory_schooling/build_county_bite.R"))),
                  stdout = TRUE, stderr = TRUE)
failed <- !is.null(attr(status, "status")) && attr(status, "status") != 0
if (failed) {
  cat(paste(status, collapse = "\n"), "\n")
  stop("build_county_bite.R exited non-zero on the fixture")
}

pupils <- read_parquet(file.path(out_dir, "bite_students.parquet")) %>% arrange(HIK)
stratum <- read_csv(file.path(out_dir, "bite_county_stratum.csv"), show_col_types = FALSE)
coverage <- read_csv(file.path(out_dir, "bite_county_coverage.csv"), show_col_types = FALSE)
validation <- read_csv(file.path(out_dir, "validation_summary.csv"), show_col_types = FALSE)

g <- function(hik, column) pupils[[column]][pupils$HIK == hik]

cat("\nSample and cohort key\n")
check("HIGRADED == 999 is dropped", function() !("F" %in% pupils$HIK))
check("nine students survive", function() nrow(pupils) == 9L)
check("yob = 1940 - AGE - 1", function() near(g("A", "yob"), 1894) && near(g("C", "yob"), 1890))
check("observed_years from HIGRADED", function()
  near(c(g("A", "observed_years"), g("C", "observed_years"), g("D", "observed_years"),
         g("E", "observed_years"), g("G", "observed_years")), c(8, 0, 17, 4, 3)))
check("cohort_decade is the birth decade", function()
  near(c(g("A", "cohort_decade"), g("E", "cohort_decade")), c(1890, 1900)))
check("cohort_decade floors rather than rounds (1889 -> 1880, 1895 -> 1890)", function()
  near(c(g("H", "cohort_decade"), g("I", "cohort_decade")), c(1880, 1890)))

cat("\nYear-selection rule\n")
check("a tie on the window midpoint breaks to the earlier year (A -> 1900)",
      function() g("A", "assignment_stratum") == 1900)
check("the state's own window flips the same cohort (B -> 1910)",
      function() g("B", "assignment_stratum") == 1910)
check("A and B share a cohort but not an assignment", function()
  g("A", "yob") == g("B", "yob") && g("A", "assignment_stratum") != g("B", "assignment_stratum"))
check("a later candidate nearer the midpoint wins (E -> 1920)",
      function() g("E", "assignment_stratum") == 1920)
check("no law at the candidate year is flagged (C)", function() isTRUE(g("C", "no_law_at_candidate")))
check("a law at the candidate year is not flagged (A)", function() !isTRUE(g("A", "no_law_at_candidate")))
check("selection_distance uses the fallback target for B's 1900 row", function()
  near(g("B", "selection_distance"), 3.5))
check("selection_distance for the chosen tie is 5", function()
  near(g("A", "selection_distance"), 5))

cat("\nCounty key\n")
check("county_geoid = STATEFIP*1000 + COUNTYICP/10", function()
  near(c(g("A", "county_geoid"), g("B", "county_geoid")), c(1031, 2005)))
check("a high state FIPS does not overflow the INT16 it arrives as", function()
  near(g("J", "county_geoid"), 48001))

cat("\nRequirements and the plateau imputation\n")
check("state 1 cohorts are law-bound", function()
  isTRUE(g("A", "law_bound_cohort")) && !isTRUE(g("A", "required_is_imputed")))
check("state 2 pre-law cohorts are not law-bound", function()
  all(!c(g("B", "law_bound_cohort"), g("C", "law_bound_cohort"))))
check("actual requirement is zero where no law bound", function()
  near(c(g("B", "required_nominal"), g("B", "required_effective")), c(0, 0)))
check("required_nominal_cf imputes the nominal plateau (4)", function()
  near(g("B", "required_nominal_cf"), 4))
check("required_effective_cf imputes the effective plateau (3)", function()
  near(g("B", "required_effective_cf"), 3))
check("the two plateaus differ, so both must be imputed separately", function()
  g("B", "required_nominal_cf") != g("B", "required_effective_cf"))
check("law-bound students keep their actual requirement in the cf columns", function()
  near(c(g("A", "required_nominal_cf"), g("A", "required_effective_cf")), c(6, 5)))
check("required_effective <= required_nominal everywhere", function()
  all(pupils$required_effective <= pupils$required_nominal))
check("birth-state requirement is carried separately", function()
  near(g("B", "required_effective_bpl"), 0) && near(g("A", "required_effective_bpl"), 5))

cat("\nMeasures\n")
cell <- function(geoid, stratum_year, basis_name) {
  stratum[stratum$county_geoid == geoid & stratum$assignment_stratum == stratum_year &
            stratum$basis == basis_name, ]
}
# (1031, 1920) holds E (observed 4) and G (observed 3), both required 6 nominal / 5 effective.
check("shortfall_rate = 1 where both students fall short", function()
  near(cell(1031, 1920, "nominal")$shortfall_rate, 1))
check("gap_mean = -2.5 on the nominal basis", function()
  near(cell(1031, 1920, "nominal")$gap_mean, -2.5))
check("gap_censored = -2.5 when nobody exceeds the minimum", function()
  near(cell(1031, 1920, "nominal")$gap_censored, -2.5))
check("gap_mean = -1.5 on the effective basis", function()
  near(cell(1031, 1920, "effective")$gap_mean, -1.5))
# (2005, 1900) holds C (observed 0) and D (observed 17), both imputed 4 / 3.
check("shortfall_rate = 0.5 with one of two below the imputed standard", function()
  near(cell(2005, 1900, "nominal_cf")$shortfall_rate, 0.5))
check("gap_mean = 4.5 on nominal_cf", function()
  near(cell(2005, 1900, "nominal_cf")$gap_mean, 4.5))
check("gap_censored = -2 ignores schooling beyond the minimum", function()
  near(cell(2005, 1900, "nominal_cf")$gap_censored, -2))
check("gap_censored = -1.5 on effective_cf", function()
  near(cell(2005, 1900, "effective_cf")$gap_censored, -1.5))
check("fixed6 uses a constant threshold", function()
  near(cell(2005, 1900, "fixed6")$gap_mean, 2.5))
check("binomial standard error is attached", function()
  near(cell(2005, 1900, "nominal_cf")$shortfall_se, sqrt(0.5 * 0.5 / 2)))
check("below_min_cell flags cells under 25 students", function()
  all(stratum$below_min_cell))

cat("\nPartitioning\n")
check("actual bases appear only on the law_bound sample", function()
  all(stratum$sample[stratum$basis %in% c("nominal", "effective")] == "law_bound"))
check("counterfactual bases appear only on the all_students sample", function()
  all(stratum$sample[grepl("_cf$|^fixed", stratum$basis)] == "all_students"))
check("unexposed students are absent from the law_bound cells", function()
  nrow(cell(2005, 1900, "nominal")) == 0)
check("unexposed students are present in the counterfactual cells", function()
  cell(2005, 1900, "nominal_cf")$n_students == 2)

cat("\nCoverage\n")
check("coverage = linked students / county births", function()
  near(coverage$coverage[coverage$county_geoid == 2005 & coverage$cohort_decade == 1890], 3 / 50))
check("coverage is computed per county-decade", function()
  near(coverage$coverage[coverage$county_geoid == 1031 & coverage$cohort_decade == 1900], 2 / 200))
check("the floored birth decade reaches the right coverage cell", function()
  near(coverage$coverage[coverage$county_geoid == 1031 & coverage$cohort_decade == 1880], 1 / 400))
check("an NA births figure yields NULL coverage, not an error or a zero", function()
  is.na(coverage$coverage[coverage$county_geoid == 48001]))

cat("\nThe script's own validation\n")
check("every validation check the script ran passed", function() all(validation$validation_passed))

if (!nzchar(Sys.getenv("KEEP_FIXTURE"))) unlink(tmp, recursive = TRUE) else cat("fixture kept at:", tmp, "
")
cat(sprintf("\n%s\n", if (FAILURES == 0) "All checks passed."
            else sprintf("%d check(s) FAILED.", FAILURES)))
quit(status = if (FAILURES == 0) 0 else 1)
