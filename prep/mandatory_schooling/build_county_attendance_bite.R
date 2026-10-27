# Build the county compulsory-schooling BITE from attendance.
#
# The bite is the room the law had to move: among the children a state's law
# WOULD later require to attend, the share who were not attending BEFORE the law
# took effect. A county already at near-universal attendance had nothing to
# gain; a county far below had a great deal. Because compulsory-schooling law
# varies only at the state level, this pre-law shortfall is the only route to
# county-level treatment intensity, and it is what lets a design put state x
# period fixed effects in and still identify something.
#
# Why attendance rather than attainment. build_county_bite.R measures completed
# schooling against what the law required. That is a downstream proxy, blind to
# the enrolment margin the statute actually names: if enforcement draws in
# marginal children who still leave at the exit age, attainment barely moves and
# a shortfall measure reads that as no bite. Its own spec reports that the
# county before/after contrast has no signal -- SD of the change 0.0484 against
# a binomial noise floor of 0.0454, a ratio of 1.07.
#
# Most of this is already computed. build_county_attendance.R:262-267 forms
# not_attending_required over `in_window OR in_window_cf` -- the actual window
# where a law was in force, the state's modal law-in-force window where none
# was. Its comment at :162-167 names the second case as "the pre-law baseline".
# This script extracts that, adds a cross-state-comparable band, applies the
# usability guards, and measures whether the result is signal or noise.
#
# A TRAP, for anyone reading attendance_county_year.csv directly. On pre-law
# rows `entryage` and `exitage` are 0. That is the raw CLS convention for
# "provision absent" carried through by any_value(), NOT the window the rate was
# computed over. The rate used cf_entryage/cf_exitage. Reading those two columns
# and concluding the pre-law rate is degenerate is wrong.
#
# WHAT THIS MAY AND MAY NOT BE USED FOR
#
#   MAY: a cross-sectional treatment intensity for an outcome measured
#   elsewhere -- bite x post on amws_per_1000_births, say, with state x cohort
#   fixed effects and within-state county variation. That never differences
#   SCHOOL.
#
#   MAY NOT: the basis of a pre/post regression on attendance itself. SCHOOL's
#   reference period is ~12 months in 1900, ~7.5 in 1910, ~4 in 1920 (enumerated
#   1 January, mid-winter) and ~6 in 1930. The state series runs 0.211, 0.063,
#   0.095, 0.065 and is mostly that. A pre-law-versus-post-law attendance
#   contrast is by construction a cross-census difference, and the attendance
#   spec's rule -- nothing is pooled or differenced across census years -- is
#   not negotiable here.
#
#   CARRIES AN UNMEASURED SELECTION: the source is the MLP linked extract, whose
#   coverage has median 0.057 in 1900 and 0.078 in 1910 against a true
#   school-age share near 0.26. The bite rests on roughly a fifth to a third of
#   each county's children. county_attendance_from_fullcount.md specifies the
#   rebuild that removes this; it should happen before the bite carries causal
#   weight.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/build_county_attendance_bite.R
#
# Outputs (output/mandatory_schooling/county_attendance_bite/):
#   bite_county.csv                 one row per county              (PRIMARY)
#   bite_county.parquet             the same
#   bite_county_band_age.parquet    county x age, the 8-14 band's inputs
#   bite_signal_by_state.csv        the signal-to-noise diagnostic
#   input_manifest.csv, validation_summary.csv, notes.txt

suppressPackageStartupMessages({
  library("arrow")
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

IN_DIR <- Sys.getenv("COUNTY_ATTENDANCE_BITE_IN_DIR", unset = "")
OUT_DIR <- Sys.getenv("COUNTY_ATTENDANCE_BITE_OUT_DIR",
                      unset = file.path(data_root, "output/mandatory_schooling/county_attendance_bite"))
overridden <- nzchar(IN_DIR)
if (!overridden) IN_DIR <- file.path(data_root, "output/mandatory_schooling/county_attendance")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

inputs <- c(
  county_year = file.path(IN_DIR, "attendance_county_year.csv"),
  county_age  = file.path(IN_DIR, "attendance_county_age.parquet"),
  laws        = if (overridden) file.path(IN_DIR, "compulsory_schooling_laws.csv")
                else file.path(data_root, "input/compulsory_schooling_laws.csv")
)
absent <- inputs[!file.exists(inputs)]
if (length(absent)) {
  stop("Missing input(s):\n  ", paste(absent, collapse = "\n  "),
       "\nRun prep/mandatory_schooling/build_county_attendance.R first.")
}

# --------------------------------------------------------------------------- #
# Constants
# --------------------------------------------------------------------------- #

# Post-adoption windows run entry 7-8 with an effective leaving age of 14-15, so
# 8-14 is the statutorily apt band. It is also where the variation lives: in the
# 1900 profile non-attendance is 0.23-0.26 at age 8 and 0.23-0.25 at 14 against
# 0.07-0.10 at 10-13, so the existing 10-13 band -- chosen for comparability,
# and sitting on the attendance peak -- compresses the bite about threefold.
BAND_AGES <- 8:14

# A single age with almost no children makes the unweighted mean over the band
# meaningless, so the band rate is NULL if ANY age in it is this thin. Matches
# build_county_attendance.R's rule for the 10-13 band.
MIN_AGE_CELL <- 5L

# Carried from build_county_attendance.R so the two measures are guarded alike.
MIN_CELL <- 25L
MIN_CELL_FOR_RANKING <- 200L

# Attenuation floor. At n = 100 the standard error on a rate near 0.11 is about
# 0.03 against a cross-county SD near 0.075, so below this the bite is mostly
# measurement error when it enters a regression as a regressor.
MIN_CELL_FOR_REGRESSION <- 100L

CENSUS_YEARS <- c(1900L, 1910L, 1920L, 1930L)

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

# --------------------------------------------------------------------------- #
# 1. The law, and which census years are pre-law
# --------------------------------------------------------------------------- #

message("Reading the law panel ...")

laws <- read_csv(inputs[["laws"]], show_col_types = FALSE,
                 col_types = cols(state_fips = col_character())) %>%
  mutate(statefip = as.integer(state_fips),
         compulsory_law_year = suppressWarnings(as.integer(compulsory_law_year))) %>%
  select(statefip, state_abbr, compulsory_law_year)

# csdid_read_laws() greps the source string for Lingwall (2014), Table 6.2 --
# the pipeline's only guard against silently reverting to the pre-Lingwall
# vintage. Reproduce it here rather than reading the file unguarded.
laws_raw <- read_csv(inputs[["laws"]], show_col_types = FALSE)
expect("law file is the Lingwall (2014), Table 6.2 vintage",
       all(grepl("Lingwall \\(2014\\), Table 6.2",
                 laws_raw$source[!is.na(laws_raw$compulsory_law_year)])),
       sprintf("%d rows with a finite year", sum(!is.na(laws_raw$compulsory_law_year))))

late <- laws %>% filter(!is.na(compulsory_law_year), compulsory_law_year > 1900L)
EXPECTED_LATE <- c("IA", "MD", "MO", "DE", "OK", "NC", "TN", "LA",
                   "TX", "AL", "AR", "GA", "VA", "FL", "SC", "MS")
if (!overridden) {
  expect("the post-1900 adopters are the validated 16 states",
         nrow(late) == 16L && setequal(late$state_abbr, EXPECTED_LATE),
         paste(sort(late$state_abbr), collapse = " "))
}

# --------------------------------------------------------------------------- #
# 2. The pre-law cells
# --------------------------------------------------------------------------- #

message("Selecting pre-law county cells ...")

county_year <- read_csv(inputs[["county_year"]], show_col_types = FALSE) %>%
  inner_join(laws, by = "statefip") %>%
  filter(!is.na(compulsory_law_year), census_year < compulsory_law_year)

# window_is_cf is the build's own pre-law flag. It is derived independently of
# Lingwall -- from the CLS state x year panel -- so agreement between the two is
# a real check on the treatment timing, not a tautology.
disagree <- county_year %>% filter(window_is_cf != 1L)
expect("window_is_cf agrees with the Lingwall pre-law rule",
       nrow(disagree) / max(nrow(county_year), 1L) < 0.005,
       sprintf("%d of %d cells disagree", nrow(disagree), nrow(county_year)))
expect("every disagreeing cell was already unusable",
       !nrow(disagree) || all(!disagree$cell_usable),
       sprintf("%d disagreeing cells, %d usable", nrow(disagree), sum(disagree$cell_usable)))

# --------------------------------------------------------------------------- #
# 3. The 8-14 band, re-standardised from the county x age grain
# --------------------------------------------------------------------------- #

message("Standardising over ages ", min(BAND_AGES), "-", max(BAND_AGES), " ...")

band_age <- read_parquet(inputs[["county_age"]]) %>%
  filter(age %in% BAND_AGES) %>%
  semi_join(county_year, by = c("county_geoid", "census_year"))

# Direct standardisation with uniform weights: the unweighted mean of the
# age-specific rates, so a county whose children skew old cannot look
# non-compliant on composition alone.
band <- band_age %>%
  group_by(county_geoid, census_year) %>%
  summarise(n_band = sum(n_known),
            n_ages_present = n(),
            min_age_cell = min(n_known),
            bite_std_8_14 = if (n() == length(BAND_AGES) && min(n_known) >= MIN_AGE_CELL) {
              mean(not_attending)
            } else NA_real_,
            .groups = "drop")

# --------------------------------------------------------------------------- #
# 4. Assemble, one row per county
# --------------------------------------------------------------------------- #

message("Assembling ...")

cells <- county_year %>%
  left_join(band, by = c("county_geoid", "census_year")) %>%
  mutate(bite_cf_window = not_attending_required,
         bite_std_1013  = not_attending_std_1013)

pick <- function(df, which_year) {
  df %>%
    group_by(county_geoid) %>%
    filter(if (which_year == "latest") census_year == max(census_year)
           else census_year == min(census_year)) %>%
    ungroup()
}

latest <- pick(cells, "latest") %>%
  select(county_geoid, statefip, state_abbr, compulsory_law_year,
         baseline_year = census_year,
         bite_cf_window, bite_std_1013, bite_std_8_14,
         n_required, n_std_band, n_band, coverage,
         below_min_cell, coverage_implausible, coverage_missing, cell_usable)

first_1900 <- cells %>%
  filter(census_year == 1900L) %>%
  select(county_geoid,
         bite_cf_window_1900 = bite_cf_window,
         bite_std_1013_1900  = bite_std_1013,
         bite_std_8_14_1900  = bite_std_8_14,
         n_required_1900     = n_required,
         n_band_1900         = n_band,
         cell_usable_1900    = cell_usable)

bite_county <- latest %>%
  left_join(first_1900, by = "county_geoid") %>%
  mutate(n_pre_law_cells = NA_integer_) %>%
  select(-n_pre_law_cells)

pre_counts <- cells %>% count(county_geoid, name = "n_pre_law_cells")
bite_county <- bite_county %>%
  left_join(pre_counts, by = "county_geoid") %>%
  mutate(
    # se on the standardised band mean, treating the ages as independent
    # binomial cells with uniform weights.
    bite_se = sqrt(bite_std_8_14 * (1 - bite_std_8_14) / pmax(n_band, 1L)),
    bite_thin      = !is.na(n_band) & n_band < MIN_CELL_FOR_REGRESSION,
    bite_rankable  = !is.na(n_band) & n_band >= MIN_CELL_FOR_RANKING,
    bite_usable    = cell_usable & !is.na(bite_std_8_14) & !bite_thin
  ) %>%
  arrange(county_geoid)

# --------------------------------------------------------------------------- #
# 5. Signal or noise
# --------------------------------------------------------------------------- #

message("Measuring signal against the noise floor ...")

# The project's own test, the one the attainment bite fails at a ratio of 1.07:
# if the cross-county dispersion of the measure is no larger than the sampling
# noise in each county, the measure is not carrying county information.
signal <- function(df) {
  d <- df %>% filter(bite_usable, !is.na(bite_se))
  if (nrow(d) < 3L) return(tibble(n_counties = nrow(d), sd_bite = NA_real_,
                                  mean_se = NA_real_, signal_ratio = NA_real_))
  tibble(n_counties = nrow(d),
         sd_bite = sd(d$bite_std_8_14),
         mean_se = mean(d$bite_se),
         signal_ratio = sd(d$bite_std_8_14) / mean(d$bite_se))
}

signal_pooled <- signal(bite_county)
signal_by_state <- bite_county %>%
  group_by(state_abbr) %>%
  group_modify(~ signal(.x)) %>%
  ungroup() %>%
  arrange(signal_ratio)

# A within-state ratio is the one that matters, since a design with state fixed
# effects uses only within-state variation.
signal_within <- bite_county %>%
  filter(bite_usable, !is.na(bite_se)) %>%
  group_by(state_abbr) %>%
  mutate(demeaned = bite_std_8_14 - mean(bite_std_8_14)) %>%
  ungroup() %>%
  summarise(sd_within = sd(demeaned), mean_se = mean(bite_se)) %>%
  mutate(signal_ratio_within = sd_within / mean_se)

# Split-half reliability on disjoint ages: there is no child id in the county
# grain to split on, so the two halves are alternating ages within the band.
halves <- band_age %>%
  mutate(half = if_else(age %% 2L == 0L, "even", "odd")) %>%
  group_by(county_geoid, census_year, half) %>%
  summarise(rate = mean(not_attending), n = sum(n_known), min_cell = min(n_known),
            .groups = "drop") %>%
  filter(min_cell >= MIN_AGE_CELL) %>%
  tidyr::pivot_wider(id_cols = c(county_geoid, census_year),
                     names_from = half, values_from = c(rate, n)) %>%
  semi_join(bite_county %>% filter(bite_usable) %>%
              select(county_geoid, census_year = baseline_year),
            by = c("county_geoid", "census_year")) %>%
  filter(!is.na(rate_even), !is.na(rate_odd))
split_half_r <- if (nrow(halves) > 2L) cor(halves$rate_even, halves$rate_odd) else NA_real_

# --------------------------------------------------------------------------- #
# 6. Validation
# --------------------------------------------------------------------------- #

message("Validating ...")

rates <- c(bite_county$bite_cf_window, bite_county$bite_std_1013, bite_county$bite_std_8_14)
expect("every bite is a proportion", all(is.na(rates) | (rates >= 0 & rates <= 1)),
       sum(!is.na(rates) & (rates < 0 | rates > 1)))
expect("every baseline year is strictly pre-law",
       all(bite_county$baseline_year < bite_county$compulsory_law_year),
       sprintf("%d violations", sum(bite_county$baseline_year >= bite_county$compulsory_law_year)))
expect("baseline years are census years",
       all(bite_county$baseline_year %in% CENSUS_YEARS),
       paste(sort(unique(bite_county$baseline_year)), collapse = ", "))
expect("one row per county",
       !anyDuplicated(bite_county$county_geoid), sum(duplicated(bite_county$county_geoid)))
# Recorded, not enforced: baseline_year mixes two reference periods by
# construction, since the latest pre-law census is 1900 for the five states
# adopting 1902-08 and 1910 for the eleven adopting 1913 or later. A cross-state
# comparison on bite_std_8_14 therefore compares a ~12-month window against a
# ~7.5-month one. The bite_*_1900 columns are the single-reference-period
# alternative and exist for exactly this reason.
expect("the mixed-baseline hazard is recorded",
       TRUE,
       paste(sprintf("%d counties on %d", table(bite_county$baseline_year),
                     as.integer(names(table(bite_county$baseline_year)))), collapse = "; "))
expect("the band rate is NULL wherever an age in the band is thin",
       !any(!is.na(bite_county$bite_std_8_14) &
              (is.na(bite_county$n_band) | bite_county$n_band < MIN_AGE_CELL * length(BAND_AGES))),
       "")

if (!overridden) {
  expect("only the 16 post-1900 adopters are present",
         setequal(bite_county$state_abbr, EXPECTED_LATE),
         paste(sort(unique(bite_county$state_abbr)), collapse = " "))
  expect("counties with a usable pre-law cell match the surveyed 1,312",
         sum(!is.na(bite_county$bite_std_1013) & bite_county$n_std_band > 25) == 1312L,
         sum(!is.na(bite_county$bite_std_1013) & bite_county$n_std_band > 25))

  # A median RATIO is undefined here -- a handful of counties sit at exactly
  # zero on the 10-13 band, giving 0/0 -- so the median DIFFERENCE is the
  # reportable statistic, with the ratio taken only where the denominator is
  # positive.
  wider <- bite_county %>% filter(!is.na(bite_std_8_14), !is.na(bite_std_1013))
  positive <- wider %>% filter(bite_std_1013 > 0)
  expect("the 8-14 band exceeds the 10-13 band for the large majority of counties",
         mean(wider$bite_std_8_14 > wider$bite_std_1013) > 0.85,
         sprintf("%.1f%% of %d counties, median difference %+.4f, median ratio %.2f over %d with a positive 10-13 rate",
                 100 * mean(wider$bite_std_8_14 > wider$bite_std_1013), nrow(wider),
                 median(wider$bite_std_8_14 - wider$bite_std_1013),
                 median(positive$bite_std_8_14 / positive$bite_std_1013), nrow(positive)))
  # The widening is much larger on a 1900 baseline than a 1910 one, which is the
  # reference-period change showing up again: at 1900's ~12-month window the
  # entry and exit edges carry far more non-attendance than at 1910's ~7.5.
  by_base <- wider %>% group_by(baseline_year) %>%
    summarise(share = mean(bite_std_8_14 > bite_std_1013),
              md = median(bite_std_8_14 - bite_std_1013), .groups = "drop")
  expect("the widening holds within each baseline year",
         all(by_base$share > 0.85),
         paste(sprintf("%d: %.1f%%, median %+.4f", by_base$baseline_year,
                       100 * by_base$share, by_base$md), collapse = "; "))

  # The 1900 cells must reproduce the state-level unexposed figure computed
  # against attendance_state_year.csv, which is 0.2248 on the 10-13 band.
  chk <- cells %>% filter(census_year == 1900L, !is.na(bite_std_1013), n_std_band > 0)
  recon <- weighted.mean(chk$bite_std_1013, chk$n_std_band)
  expect("the 1900 cells reproduce the unexposed state figure of 0.2248",
         abs(recon - 0.2248) < 0.005, sprintf("%.4f", recon))
}

expect("the bite carries more dispersion than its own noise floor",
       !is.na(signal_pooled$signal_ratio) && signal_pooled$signal_ratio > 1.5,
       sprintf("pooled ratio %.2f (sd %.4f vs mean se %.4f)",
               signal_pooled$signal_ratio, signal_pooled$sd_bite, signal_pooled$mean_se))
expect("within-state dispersion also exceeds the noise floor",
       !is.na(signal_within$signal_ratio_within) && signal_within$signal_ratio_within > 1.5,
       sprintf("within ratio %.2f", signal_within$signal_ratio_within))
expect("the split-half halves agree",
       !is.na(split_half_r) && split_half_r > 0.5,
       sprintf("r = %.3f over %d counties", split_half_r, nrow(halves)))

message(sprintf("  %d counties, %d usable, %d rankable, %d thin",
                nrow(bite_county), sum(bite_county$bite_usable),
                sum(bite_county$bite_rankable), sum(bite_county$bite_thin)))

# --------------------------------------------------------------------------- #
# 7. Write
# --------------------------------------------------------------------------- #

message("Writing ...")

write_csv(bite_county, file.path(OUT_DIR, "bite_county.csv"))
write_parquet(bite_county, file.path(OUT_DIR, "bite_county.parquet"))
write_parquet(band_age, file.path(OUT_DIR, "bite_county_band_age.parquet"))
write_csv(signal_by_state, file.path(OUT_DIR, "bite_signal_by_state.csv"))

manifest <- tibble(input = names(inputs), path = unname(inputs),
                   bytes = file.size(unname(inputs)),
                   md5 = unname(tools::md5sum(unname(inputs))))
write_csv(manifest, file.path(OUT_DIR, "input_manifest.csv"))

validation <- do.call(rbind, checks)
write_csv(validation, file.path(OUT_DIR, "validation_summary.csv"))

writeLines(c(
  "County compulsory-schooling bite, from attendance.",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  "",
  "WHAT THIS IS",
  "  Among the children a state's law WOULD later require to attend, the share",
  "  not attending BEFORE the law took effect. One row per county, for the 16",
  "  states adopting after 1900. Pre-law by construction.",
  "",
  "WHICH COLUMN TO USE",
  sprintf("  bite_std_8_14  headline. Ages %d-%d, direct standardisation with uniform",
          min(BAND_AGES), max(BAND_AGES)),
  "                 weights. Comparable across states and spans the entry and",
  "                 exit edges where the law binds.",
  "  bite_std_1013  the existing 10-13 band. Comparable, but sits on the",
  "                 attendance peak and compresses the bite about threefold.",
  "  bite_cf_window each state's own eventual statutory window. Statutorily",
  "                 apt, NOT comparable across states.",
  "",
  "GUARDS -- these are flags, never filters, because a silently missing county",
  "looks like a county with no bite.",
  sprintf("  bite_usable    cell_usable AND a band rate AND n_band >= %d", MIN_CELL_FOR_REGRESSION),
  sprintf("  bite_rankable  n_band >= %d", MIN_CELL_FOR_RANKING),
  "  bite_thin      below the regression floor; attenuation is the concern",
  "",
  "SIGNAL",
  sprintf("  pooled      sd %.4f vs mean se %.4f, ratio %.2f",
          signal_pooled$sd_bite, signal_pooled$mean_se, signal_pooled$signal_ratio),
  sprintf("  within-state sd %.4f vs mean se %.4f, ratio %.2f",
          signal_within$sd_within, signal_within$mean_se, signal_within$signal_ratio_within),
  sprintf("  split-half r %.3f over %d counties", split_half_r, nrow(halves)),
  "  For comparison the attainment bite's county before/after contrast runs at",
  "  1.07, which is what nothing happening produces.",
  "",
  "DO NOT",
  "  Do not difference attendance across census years. SCHOOL's reference period",
  "  is ~12 months in 1900, ~7.5 in 1910, ~4 in 1920, ~6 in 1930. Use this as a",
  "  cross-sectional treatment intensity for an outcome measured elsewhere.",
  "  Do not compare bite_std_8_14 ACROSS states without checking baseline_year.",
  sprintf("  It is 1900 for %d counties and 1910 for %d, because the latest pre-law",
          sum(bite_county$baseline_year == 1900L), sum(bite_county$baseline_year == 1910L)),
  "  census differs by adoption date, so the two rest on different reference",
  "  periods. The bite_*_1900 columns hold one period for every state and are",
  "  the right choice for any cross-state comparison.",
  "  Do not read entryage/exitage from attendance_county_year.csv as the window",
  "  these rates were computed over; on pre-law rows they are 0, the CLS",
  "  'provision absent' convention, while the rate used cf_entryage/cf_exitage.",
  "",
  "SELECTION",
  "  The source is the MLP linked extract. Median coverage is 0.057 in 1900 and",
  "  0.078 in 1910 against a true school-age share near 0.26, so this rests on a",
  "  fifth to a third of each county's children. See",
  "  prep/mandatory_schooling/county_attendance_from_fullcount.md."
), file.path(OUT_DIR, "notes.txt"))

cat("\nBite by state (latest pre-law baseline, usable counties):\n\n")
print(as.data.frame(
  bite_county %>% filter(bite_usable) %>%
    group_by(state_abbr, compulsory_law_year, baseline_year) %>%
    summarise(counties = n(), bite = mean(bite_std_8_14), sd = sd(bite_std_8_14),
              .groups = "drop") %>%
    arrange(compulsory_law_year, state_abbr)), row.names = FALSE, digits = 3)

cat("\nSignal by state:\n")
print(as.data.frame(signal_by_state), row.names = FALSE, digits = 3)

cat("\nValidation:\n")
print(as.data.frame(validation), row.names = FALSE)

message("\nSaved to: ", OUT_DIR)

if (!all(validation$validation_passed)) {
  stop(sum(!validation$validation_passed), " validation check(s) FAILED.")
}
