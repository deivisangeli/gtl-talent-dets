# Synthetic-fixture checks for build_county_attendance_bite.R.
#
# Runs without the 776 MB attendance parquet or the Dropbox tree: every input is
# written into a temporary directory and the build is invoked against it through
# COUNTY_ATTENDANCE_BITE_IN_DIR / _OUT_DIR.
#
# The fixtures are built so that each answer is known by hand, which is the
# point -- a test that only checks the build runs would not catch the two errors
# this measure is most exposed to: reading the raw entryage/exitage columns
# instead of the counterfactual window, and letting a thin single age through
# into a standardised mean.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/test_county_attendance_bite.R

suppressPackageStartupMessages({
  library("arrow")
  library("dplyr")
  library("readr")
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
BUILD <- file.path(repo_root, "prep/mandatory_schooling/build_county_attendance_bite.R")

passed <- 0L; failed <- 0L
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) {
    passed <<- passed + 1L
    cat("  ok    ", label, "\n", sep = "")
  } else {
    failed <<- failed + 1L
    cat("  FAIL  ", label, if (nzchar(detail)) paste0("  -- ", detail) else "", "\n", sep = "")
  }
}
near <- function(a, b, tol = 1e-8) !is.na(a) && !is.na(b) && abs(a - b) < tol

BAND <- 8:14

# --------------------------------------------------------------------------- #
# Fixture construction
# --------------------------------------------------------------------------- #

# Two states. State 1 adopts in 1917, so 1900 and 1910 are both pre-law. State 2
# adopts in 1890, so it is NOT a late adopter and every one of its rows must be
# dropped by the build.
make_laws <- function() {
  tibble(
    state = c("Alpha", "Beta"),
    state_abbr = c("AL", "BE"),
    state_fips = c("01", "02"),
    compulsory_law_year = c(1917L, 1890L),
    notes = "",
    source = "Lingwall (2014), Table 6.2",
    session_law_reference = c("1915 at 534", "1889 at 1")
  )
}

# county_age carries one row per county x year x age. `not_attending` is the
# age-specific rate; the build's standardised band is the UNWEIGHTED mean over
# ages 8-14, so a county whose children skew old must come out the same as one
# whose children skew young when the age-specific rates match.
make_county_age <- function() {
  rows <- list()
  add <- function(geoid, year, rates, n) {
    rows[[length(rows) + 1L]] <<- tibble(
      county_geoid = geoid, census_year = year, age = 5:18,
      n_children = n, n_known = n,
      not_attending = rates)
  }
  # A flat profile makes the expected band mean exactly the flat value.
  flat <- function(v) rep(v, 14)
  # County 1001: flat 0.20, young-skewed counts.
  add(1001, 1900L, flat(0.20), c(rep(400L, 9), rep(100L, 5)))
  # County 1002: flat 0.20, old-skewed counts. Same standardised answer.
  add(1002, 1900L, flat(0.20), c(rep(100L, 9), rep(400L, 5)))
  # County 1003: one thin age INSIDE the band (age 10 has 3 children) -> NULL.
  r <- flat(0.20); n <- rep(300L, 14); n[which(5:18 == 10L)] <- 3L
  add(1003, 1900L, r, n)
  # County 1004: thin age OUTSIDE the band (age 5) -> still computed.
  n2 <- rep(300L, 14); n2[which(5:18 == 5L)] <- 2L
  add(1004, 1900L, flat(0.20), n2)
  # County 1005: a realistic U profile. Band mean is computed by hand below.
  u <- c(0.83, 0.51, 0.30, 0.24, 0.20, 0.07, 0.07, 0.09, 0.12, 0.23, 0.38, 0.53, 0.67, 0.78)
  add(1005, 1900L, u, rep(500L, 14))
  # County 1001 again in 1910, still pre-law, with a lower profile. The build
  # takes the LATEST pre-law year, so this is the row that must win.
  add(1001, 1910L, flat(0.10), rep(400L, 14))
  # State 2 county, law in force since 1890 -> must be excluded entirely.
  add(2001, 1900L, flat(0.50), rep(500L, 14))
  bind_rows(rows)
}

make_county_year <- function(county_age) {
  county_age %>%
    group_by(county_geoid, census_year) %>%
    summarise(n_children = sum(n_children),
              n_required = sum(n_known[age >= 8 & age < 15]),
              not_attending_required = mean(not_attending[age >= 8 & age < 15]),
              not_attending_std_1013 = mean(not_attending[age >= 10 & age <= 13]),
              n_std_band = sum(n_known[age >= 10 & age <= 13]),
              .groups = "drop") %>%
    mutate(statefip = as.integer(substr(sprintf("%05d", county_geoid), 1, 2)),
           # Pre-law rows carry the CLS "provision absent" convention. A build
           # that reads these as the window will get a degenerate answer.
           window_is_cf = if_else(statefip == 1L, 1L, 0L),
           entryage = if_else(statefip == 1L, 0L, 7L),
           exitage = if_else(statefip == 1L, 0L, 14L),
           county_population = 20000,
           coverage = n_children / county_population,
           below_min_cell = n_required < 25L,
           coverage_implausible = coverage > 0.30,
           coverage_missing = is.na(coverage),
           cell_usable = !below_min_cell & !coverage_implausible & !coverage_missing)
}

fixture_dir <- file.path(tempdir(), paste0("cab_fixture_", as.integer(runif(1, 1, 1e6))))
out_dir <- file.path(fixture_dir, "out")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

county_age <- make_county_age()
county_year <- make_county_year(county_age)
write_parquet(county_age, file.path(fixture_dir, "attendance_county_age.parquet"))
write_csv(county_year, file.path(fixture_dir, "attendance_county_year.csv"))
write_csv(make_laws(), file.path(fixture_dir, "compulsory_schooling_laws.csv"))

cat("Fixture at ", fixture_dir, "\n", sep = "")

# --------------------------------------------------------------------------- #
# Run the build against the fixture
# --------------------------------------------------------------------------- #

# system2()'s `env` argument is a no-op on Windows, so the child would silently
# run against the real Dropbox tree. Set the variables in this process instead
# and let the child inherit them.
old_env <- Sys.getenv(c("COUNTY_ATTENDANCE_BITE_IN_DIR", "COUNTY_ATTENDANCE_BITE_OUT_DIR"),
                      unset = NA_character_)
Sys.setenv(COUNTY_ATTENDANCE_BITE_IN_DIR = fixture_dir,
           COUNTY_ATTENDANCE_BITE_OUT_DIR = out_dir)
on.exit({
  restore <- old_env[!is.na(old_env)]
  if (length(restore)) do.call(Sys.setenv, as.list(restore))
  drop <- names(old_env)[is.na(old_env)]
  if (length(drop)) Sys.unsetenv(drop)
}, add = TRUE)

log_path <- file.path(fixture_dir, "build_log.txt")
status <- system2("Rscript", c(shQuote(BUILD)), stdout = log_path, stderr = log_path)

cat("\nChecks:\n")
ok("the build exits cleanly on the fixture", status == 0L, paste("exit", status))
if (status != 0L && file.exists(log_path)) {
  cat("\n--- build log (tail) ---\n")
  writeLines(tail(readLines(log_path, warn = FALSE), 25))
  cat("--- end ---\n\n")
}

bite_path <- file.path(out_dir, "bite_county.csv")
ok("bite_county.csv is written", file.exists(bite_path))
if (!file.exists(bite_path)) {
  cat(sprintf("\n%d passed, %d failed\n", passed, failed)); quit(status = 1L)
}
b <- read_csv(bite_path, show_col_types = FALSE)

# --- the state restriction ------------------------------------------------- #
ok("counties in a pre-1900-adopting state are excluded",
   !any(b$county_geoid == 2001), paste(b$county_geoid, collapse = ","))
ok("only the late-adopting state survives",
   setequal(b$state_abbr, "AL"), paste(sort(unique(b$state_abbr)), collapse = ","))

# --- the baseline year ----------------------------------------------------- #
c1001 <- b %>% filter(county_geoid == 1001)
ok("the LATEST pre-law census is taken as the baseline",
   nrow(c1001) == 1L && c1001$baseline_year == 1910L,
   paste(c1001$baseline_year, collapse = ","))
ok("the baseline is strictly before the law year",
   all(b$baseline_year < b$compulsory_law_year))
ok("one row per county", !anyDuplicated(b$county_geoid))

# --- standardisation ------------------------------------------------------- #
ok("the 1910 baseline rate is the flat profile value",
   near(c1001$bite_std_8_14, 0.10), format(c1001$bite_std_8_14))

c1002 <- b %>% filter(county_geoid == 1002)
# 1001's baseline is 1910, so compare 1002 against the 1900 column instead.
ok("age composition does not move the standardised rate",
   near(c1002$bite_std_8_14, 0.20) && near(c1001$bite_std_8_14_1900, 0.20),
   sprintf("old-skewed %.4f vs young-skewed %.4f",
           c1002$bite_std_8_14, c1001$bite_std_8_14_1900))

c1005 <- b %>% filter(county_geoid == 1005)
u <- c(0.83, 0.51, 0.30, 0.24, 0.20, 0.07, 0.07, 0.09, 0.12, 0.23, 0.38, 0.53, 0.67, 0.78)
expected_band <- mean(u[(5:18) %in% BAND])
expected_1013 <- mean(u[(5:18) %in% 10:13])
ok("the band mean matches a hand-computed U profile",
   near(c1005$bite_std_8_14, expected_band),
   sprintf("%.6f vs %.6f", c1005$bite_std_8_14, expected_band))
ok("the 8-14 band exceeds the 10-13 band on a U profile",
   c1005$bite_std_8_14 > c1005$bite_std_1013,
   sprintf("%.4f vs %.4f (10-13 expected %.4f)",
           c1005$bite_std_8_14, c1005$bite_std_1013, expected_1013))

# --- thin ages ------------------------------------------------------------- #
c1003 <- b %>% filter(county_geoid == 1003)
ok("a thin age INSIDE the band nulls the standardised rate",
   nrow(c1003) == 1L && is.na(c1003$bite_std_8_14), format(c1003$bite_std_8_14))
c1004 <- b %>% filter(county_geoid == 1004)
ok("a thin age OUTSIDE the band does not null it",
   nrow(c1004) == 1L && near(c1004$bite_std_8_14, 0.20), format(c1004$bite_std_8_14))
ok("a nulled band rate is not usable",
   nrow(c1003) == 1L && !c1003$bite_usable)

# --- the degenerate-window trap -------------------------------------------- #
# Every pre-law fixture row carries entryage = exitage = 0. If the build read
# those as the window rather than using the precomputed counterfactual rate,
# bite_cf_window would be empty or zero for every county.
ok("the pre-law window convention does not empty the cf rate",
   all(!is.na(b$bite_cf_window)) && all(b$bite_cf_window > 0),
   sprintf("%d NA, %d zero", sum(is.na(b$bite_cf_window)), sum(b$bite_cf_window == 0, na.rm = TRUE)))

# --- guards ---------------------------------------------------------------- #
ok("bite_thin flags counties below the regression floor",
   is.logical(b$bite_thin) && !any(is.na(b$bite_thin)))
ok("bite_usable implies a non-missing band rate",
   all(!b$bite_usable | !is.na(b$bite_std_8_14)))
ok("every rate is a proportion",
   all(is.na(c(b$bite_cf_window, b$bite_std_1013, b$bite_std_8_14)) |
         (c(b$bite_cf_window, b$bite_std_1013, b$bite_std_8_14) >= 0 &
            c(b$bite_cf_window, b$bite_std_1013, b$bite_std_8_14) <= 1)))

# --- the sidecars ---------------------------------------------------------- #
v_path <- file.path(out_dir, "validation_summary.csv")
ok("validation_summary.csv is written", file.exists(v_path))
if (file.exists(v_path)) {
  v <- read_csv(v_path, show_col_types = FALSE)
  ok("the fixture run records checks", nrow(v) > 0L, nrow(v))
}
ok("notes.txt is written", file.exists(file.path(out_dir, "notes.txt")))
ok("input_manifest.csv is written", file.exists(file.path(out_dir, "input_manifest.csv")))

unlink(fixture_dir, recursive = TRUE)

cat(sprintf("\n%d passed, %d failed\n", passed, failed))
if (failed > 0L) quit(status = 1L)
