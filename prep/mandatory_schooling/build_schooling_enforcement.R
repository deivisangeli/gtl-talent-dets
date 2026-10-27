###############################################################################
# Project: GTL Talent Determinants
# Goal: Read the county schooling series against compulsory-schooling law dates
#
# A compulsory attendance law on the books is not a law enforced. This script
# joins the county schooling panel to the Lingwall (2014) Table 6.2 law dates
# so attendance and literacy can be read as proxies for enforcement.
#
# School attendance is the direct first stage: the law mandates exactly that
# school-age children attend school, and attendance is measured alongside the
# 1850-1880 law dates rather than decades later. Literacy is the downstream,
# long-run outcome.
#
# This script constructs no new measure. Both series are built once in
# prep/build_county_demographics_panel.R and shared with the land-grants and
# inventor-determinants analyses; duplicating them here would fork them.
#
# Coverage is limited by what the census asked:
#   1850, 1890, 1920  school attendance  school_attendance_rate
#   1840-1930         direct literacy    literacy_rate, illiterate_share_*
#   1940-1950         attainment proxy   low_education_share_25plus and friends
#   1880              attendance, STATE level only, handled separately below
#   1860, 1900        no county school table in NHGIS at all
#
# Attendance universes differ by year - 1850 counts attendance at any
# institution, 1890 only common-school pupils, 1920 persons by age band - so
# the series is comparable only with that caveat. attendance_universe and
# attendance_age_span carry it per row.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/build_schooling_enforcement.R
#
# Inputs:
#   output/county_nhgis_demographics_panel.csv
#   input/compulsory_schooling_laws.csv
#   raw/nhgis_manufacturing/parquet/*ds24_1880_state.parquet
#
# Output (output/mandatory_schooling/enforcement/):
#   county_schooling_enforcement.csv  county-year, law year, years since law
#   state_schooling_by_year.csv       denominator-weighted state rates
#   state_attendance_1880.csv         1880 state attendance and daily intensity
#   coverage_by_year.csv              non-missing county counts per measure
###############################################################################

suppressPackageStartupMessages({
 library("tidyverse")
})

repo_root <- local({
 file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
 candidate <- if (length(file_arg)) {
  dirname(dirname(dirname(
   normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")
  )))
 } else {
  Sys.getenv("GTL_REPO", unset = getwd())
 }
 while (!file.exists(file.path(candidate, "paths.R"))) {
  parent <- dirname(candidate)
  if (parent == candidate) stop("Cannot resolve repository root; set GTL_REPO.")
  candidate <- parent
 }
 normalizePath(candidate, winslash = "/")
})

### paths.R still defaults to a coauthor's home directory, so fall back the
### same way analysis_compulsory_schooling_amws_annual_csdid.R does rather
### than introducing a fourth path convention.
data_root <- local({
 config <- new.env()
 sys.source(file.path(repo_root, "paths.R"), config)
 root <- config$TALENT_DETS_DATA_DIR
 if (!dir.exists(root) && !nzchar(Sys.getenv("TALENT_DETS_DATA_DIR"))) {
  root <- Sys.getenv("DET_DIR", unset = "")
 }
 if (!dir.exists(root)) {
  root <- file.path("C:/Users", Sys.info()[["user"]],
                    "Globtalent Dropbox", "gtl_talent_dets")
 }
 normalizePath(root, winslash = "/", mustWork = TRUE)
})

output_dir <- file.path(data_root, "output", "mandatory_schooling", "enforcement")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Inputs
###############################################################################

panel_path <- file.path(data_root, "output", "county_nhgis_demographics_panel.csv")
laws_path <- file.path(data_root, "input", "compulsory_schooling_laws.csv")

if (!file.exists(panel_path)) {
 stop("No county literacy panel at ", panel_path,
      ".\nRun prep/build_county_demographics_panel.R first.")
}

panel <- read_csv(panel_path, show_col_types = FALSE, guess_max = 1e6,
                  col_types = cols(GEOID = col_character()))

laws <- read_csv(laws_path, show_col_types = FALSE,
                 col_types = cols(state_fips = col_character())) %>%
 mutate(
  state_fips = sprintf("%02d", as.integer(state_fips)),
  compulsory_law_year = suppressWarnings(as.integer(compulsory_law_year))
 )

### Same integrity assumptions the annual analysis asserts, so a changed laws
### file is caught here rather than silently reshaping the join.
stopifnot(
 nrow(laws) == 51L,
 !anyDuplicated(laws$state_fips),
 sum(is.finite(laws$compulsory_law_year)) == 49L,
 all(is.na(laws$compulsory_law_year[laws$state_abbr %in% c("AK", "HI")])),
 !anyDuplicated(panel[, c("GEOID", "year")])
)

### Attendance first: it is the measure the law actually mandates.
attendance_measures <- c(
 "school_attendance_rate", "school_attendance_rate_white",
 "school_attendance_rate_nonwhite", "attendance_rate_7_13",
 "attendance_rate_14_15", "attendance_rate_16_17", "attendance_rate_18_20"
)

literacy_measures <- c(
 "literacy_rate", "illiterate_share_10plus",
 "illiterate_share_total_population", "low_education_share_25plus",
 "no_schooling_share_25plus", "elementary_complete_share_25plus",
 "median_school_years_25plus"
)

schooling_measures <- c(attendance_measures, literacy_measures)

missing_measures <- setdiff(schooling_measures, names(panel))

if (length(missing_measures) > 0) {
 stop("County schooling panel is missing: ",
      paste(missing_measures, collapse = ", "),
      "\nRebuild it with prep/build_county_demographics_panel.R.")
}

###############################################################################
# County panel against law dates
###############################################################################

county_enforcement <- panel %>%
 mutate(GEOID = sprintf("%05d", as.integer(GEOID)),
        state_fips = substr(GEOID, 1, 2)) %>%
 left_join(
  laws %>% select(state_fips, state_abbr, state_law = state,
                  compulsory_law_year),
  by = "state_fips"
 ) %>%
 filter(if_any(all_of(schooling_measures), \(x) !is.na(x))) %>%
 mutate(years_since_law = year - compulsory_law_year) %>%
 select(
  GEOID, state_fips, state_abbr, state_law, county_nhgis, year,
  compulsory_law_year, years_since_law, literacy_measure_type,
  attendance_universe, attendance_age_span,
  total_population, population_10plus, population_25plus_reported,
  school_attendance_persons, school_age_population,
  all_of(schooling_measures)
 ) %>%
 arrange(GEOID, year)

write_csv(county_enforcement, file.path(output_dir, "county_schooling_enforcement.csv"))

###############################################################################
# State rates
###############################################################################

### Weight by the denominator each measure is defined on. A plain mean of
### county rates would weight a village the same as a city.
weighted_rate <- function(value, weight) {
 keep <- !is.na(value) & !is.na(weight) & weight > 0
 if (!any(keep)) return(NA_real_)
 sum(value[keep] * weight[keep]) / sum(weight[keep])
}

state_schooling <- county_enforcement %>%
 group_by(state_fips, state_abbr, state_law, compulsory_law_year, year) %>%
 summarise(
  counties = n(),
  years_since_law = first(years_since_law),
  literacy_measure_type = first(literacy_measure_type),
  attendance_universe = first(attendance_universe),
  attendance_age_span = first(attendance_age_span),
  school_attendance_rate = weighted_rate(school_attendance_rate, school_age_population),
  school_attendance_rate_white = weighted_rate(school_attendance_rate_white, school_age_population),
  school_attendance_rate_nonwhite = weighted_rate(school_attendance_rate_nonwhite, school_age_population),
  attendance_rate_7_13 = weighted_rate(attendance_rate_7_13, school_age_population),
  attendance_rate_14_15 = weighted_rate(attendance_rate_14_15, school_age_population),
  attendance_rate_16_17 = weighted_rate(attendance_rate_16_17, school_age_population),
  attendance_rate_18_20 = weighted_rate(attendance_rate_18_20, school_age_population),
  literacy_rate = weighted_rate(literacy_rate, population_10plus),
  illiterate_share_10plus = weighted_rate(illiterate_share_10plus, population_10plus),
  illiterate_share_total_population = weighted_rate(
   illiterate_share_total_population, total_population
  ),
  low_education_share_25plus = weighted_rate(
   low_education_share_25plus, population_25plus_reported
  ),
  no_schooling_share_25plus = weighted_rate(
   no_schooling_share_25plus, population_25plus_reported
  ),
  elementary_complete_share_25plus = weighted_rate(
   elementary_complete_share_25plus, population_25plus_reported
  ),
  .groups = "drop"
 ) %>%
 arrange(state_fips, year)

write_csv(state_schooling, file.path(output_dir, "state_schooling_by_year.csv"))

###############################################################################
# Coverage
###############################################################################

coverage <- panel %>%
 group_by(year) %>%
 summarise(
  counties = n(),
  across(all_of(schooling_measures), \(x) sum(!is.na(x))),
  .groups = "drop"
 )

write_csv(coverage, file.path(output_dir, "coverage_by_year.csv"))

first_non_missing_chr <- function(x) {
 x <- x[!is.na(x)]
 if (length(x) == 0) NA_character_ else x[1]
}

### Every state with a law date should have schooling observations somewhere,
### otherwise the enforcement read is silently missing jurisdictions.
states_with_law <- laws %>% filter(is.finite(compulsory_law_year))

states_without_schooling <- setdiff(states_with_law$state_fips,
                                   unique(county_enforcement$state_fips))

cat("\n=== County literacy coverage by census year ===\n")
print(coverage, n = Inf, width = Inf)

cat("\n=== Census years by measure available ===\n")
county_enforcement %>%
 group_by(year) %>%
 summarise(
  counties = n(),
  attendance = sum(!is.na(school_attendance_rate)),
  attendance_universe = first_non_missing_chr(attendance_universe),
  literacy = sum(!is.na(literacy_rate) | !is.na(illiterate_share_total_population)),
  attainment = sum(!is.na(low_education_share_25plus)),
  .groups = "drop"
 ) %>%
 print(n = Inf, width = Inf)

cat("\nStates with a Lingwall law year: ", nrow(states_with_law), "\n", sep = "")
cat("Of those, with no schooling observation: ",
    length(states_without_schooling),
    if (length(states_without_schooling) > 0) {
     paste0(" (", paste(states_without_schooling, collapse = ", "), ")")
    } else {
     ""
    },
    "\n", sep = "")

message("\nSaved: ", file.path(output_dir, "county_schooling_enforcement.csv"))
message("Saved: ", file.path(output_dir, "state_schooling_by_year.csv"))
message("Saved: ", file.path(output_dir, "coverage_by_year.csv"))

###############################################################################
# 1880 state attendance
###############################################################################

### 1880 sits in the middle of the 1850-1880 law window, but NHGIS only
### publishes its school tables for states (1880_sPbSMX), so this cannot join
### the county panel. Compulsory-attendance laws are state-level anyway, so a
### state series is directly usable.
###
### daily_attendance_per_pupil is the sharpest of these: average daily
### attendance over pupils enrolled needs no population denominator and reads
### as enforcement intensity rather than enrollment reach.

state_1880_files <- list.files(
 file.path(data_root, "raw", "nhgis_manufacturing", "parquet"),
 pattern = "ds24_1880_state[.]parquet$", full.names = TRUE
)

if (length(state_1880_files) == 0) {
 warning("No 1880 state school file found; skipping state_attendance_1880.csv.")
} else {
 state_1880_file <- state_1880_files[which.max(file.info(state_1880_files)$mtime)]
 raw_1880 <- arrow::read_parquet(state_1880_file)

 ### The parquet keeps the descriptive csv_header row; drop it.
 raw_1880 <- raw_1880[str_detect(raw_1880$GISJOIN, "^G[0-9]+$"), ]

 numeric_1880 <- function(code) suppressWarnings(as.numeric(raw_1880[[code]]))

 ### NHGIS STATEA is a three-digit code whose first two digits are the state
 ### FIPS: Alabama 010 -> 01, Virginia 510 -> 51.
 attendance_1880 <- tibble(
  state_fips = substr(raw_1880$STATEA, 1, 2),
  state_nhgis = raw_1880$STATE,
  year = 1880L,
  pupils_attending = numeric_1880("AQQ001"),
  pupils_attending_white = numeric_1880("AQR001"),
  pupils_attending_colored = numeric_1880("AQR002"),
  pupils_attending_white_male = numeric_1880("AQS001"),
  pupils_attending_white_female = numeric_1880("AQS002"),
  pupils_attending_colored_male = numeric_1880("AQS003"),
  pupils_attending_colored_female = numeric_1880("AQS004"),
  average_daily_attendance = numeric_1880("AQT001"),
  average_daily_attendance_white = numeric_1880("AQU001"),
  average_daily_attendance_colored = numeric_1880("AQU002")
 ) %>%
  mutate(
   daily_attendance_per_pupil = if_else(
    !is.na(average_daily_attendance) & !is.na(pupils_attending) &
     pupils_attending > 0,
    average_daily_attendance / pupils_attending, NA_real_
   ),
   attendance_universe = "Pupils attending public school during the year",
   attendance_construction = paste(
    "1880_sPbSMX state tables. Pupils attending during the year (AQQ001,",
    "AQR, AQS) and average daily attendance (AQT, AQU). State level only:",
    "NHGIS publishes no 1880 county school table.",
    "daily_attendance_per_pupil is AQT001 / AQQ001."
   ),
   source_file = state_1880_file
  ) %>%
  left_join(
   laws %>% select(state_fips, state_abbr, state_law = state,
                   compulsory_law_year),
   by = "state_fips"
  ) %>%
  mutate(years_since_law = year - compulsory_law_year) %>%
  relocate(state_fips, state_abbr, state_law, state_nhgis, year,
           compulsory_law_year, years_since_law) %>%
  arrange(state_fips)

 ### A ratio above one would mean average daily attendance exceeded
 ### enrolment, which is not possible and would mean the wrong columns.
 stopifnot(
  all(is.na(attendance_1880$daily_attendance_per_pupil) |
       (attendance_1880$daily_attendance_per_pupil > 0 &
         attendance_1880$daily_attendance_per_pupil <= 1))
 )

 write_csv(attendance_1880, file.path(output_dir, "state_attendance_1880.csv"))

 cat("\n=== 1880 state attendance ===\n")
 cat("States: ", nrow(attendance_1880),
     " | matched to a law year: ", sum(!is.na(attendance_1880$compulsory_law_year)),
     "\n", sep = "")
 cat("National pupils attending: ",
     format(sum(attendance_1880$pupils_attending, na.rm = TRUE),
            big.mark = ",", scientific = FALSE),
     " | average daily attendance per pupil: ",
     round(sum(attendance_1880$average_daily_attendance, na.rm = TRUE) /
            sum(attendance_1880$pupils_attending, na.rm = TRUE), 3),
     "\n", sep = "")

 message("Saved: ", file.path(output_dir, "state_attendance_1880.csv"))
}

