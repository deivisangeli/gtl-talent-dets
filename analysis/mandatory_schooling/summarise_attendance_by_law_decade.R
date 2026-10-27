###############################################################################
# Project: GTL Talent Determinants
# Goal: Distribution of county school attendance by compulsory-law decade
#
# Do counties in states that adopted a compulsory-attendance law earlier show
# higher attendance? This tabulates the attendance rate by the decade the
# state's law took effect.
#
# READ THIS BEFORE READING THE TABLE. It is descriptive, not causal:
#
#   1. Law-adoption decade is almost perfectly collinear with region. Every
#      one of the ten states that adopted in the 1910s is Southern, while the
#      1870s and 1880s cohorts contain no Southern state at all. The raw
#      gradient is therefore the North-South gap at least as much as it is
#      law timing. attendance_by_law_decade_region.csv splits the same
#      statistics within region so this is visible rather than buried.
#   2. By 1920 every state had a law, so there is no untreated comparison
#      group. The variation across groups is in length of exposure, not in
#      treatment status.
#
# For a causal answer use the annual Callaway-Sant'Anna design with
# not-yet-treated controls, already built in this directory.
#
# The requested year was 1930, which does not exist in Dropbox: the county
# panel stops at 1920 and attendance is present only for 1850, 1890 and 1920.
# 1920 is the closest available year and the only one with an age-graded
# rate, so attendance_rate_7_13 covers exactly the compulsory-attendance
# ages. This script is parameterised by year and will emit the 1930 table
# unchanged once the NHGIS education extract lands.
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/summarise_attendance_by_law_decade.R
#
# MS_ATTENDANCE_YEARS selects years: a comma-separated list, or "all" to add
# 1850 and 1890. Those two have no age band and a different universe, so they
# are not in the default.
#
# Input:
#   output/mandatory_schooling/enforcement/county_schooling_enforcement.csv
#
# Output (results/mandatory_schooling/attendance_law_decade/):
#   attendance_by_law_decade.csv         year x measure x law decade
#   attendance_by_law_decade_region.csv  the same, split South / non-South
#   notes.txt                            caveats and provenance
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
### same way build_schooling_enforcement.R does rather than introducing a
### fourth path convention.
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

output_dir <- file.path(data_root, "results", "mandatory_schooling",
                        "attendance_law_decade")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Input
###############################################################################

input_path <- file.path(data_root, "output", "mandatory_schooling",
                        "enforcement", "county_schooling_enforcement.csv")

if (!file.exists(input_path)) {
 stop("No county schooling enforcement file at ", input_path,
      ".\nRun prep/mandatory_schooling/build_schooling_enforcement.R first.")
}

enforcement <- read_csv(input_path, show_col_types = FALSE, guess_max = 1e6,
                        col_types = cols(GEOID = col_character(),
                                         state_fips = col_character()))

### The Census South, plus DC. OK, WV, DE and MD are border/Southern cases
### that the Census assigns to the South; the list is recorded in notes.txt
### because it is a judgement call that moves the region split.
southern_states <- c("AL", "AR", "DC", "DE", "FL", "GA", "KY", "LA", "MD",
                     "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV")

### Primary first: 7-13 is the compulsory-attendance age band, so it is the
### measure of bite. The broad rate spans 7-20 and is context.
candidate_measures <- c(
 attendance_rate_7_13 = "Attending school, ages 7-13 (compulsory ages)",
 school_attendance_rate = "Attending school, full available age span",
 attendance_rate_14_15 = "Attending school, ages 14-15",
 attendance_rate_16_17 = "Attending school, ages 16-17",
 attendance_rate_18_20 = "Attending school, ages 18-20"
)

available_measures <- intersect(names(candidate_measures), names(enforcement))

### Counties with no law year (Alaska and Hawaii, which Lingwall Table 6.2
### excludes) are dropped rather than treated as never-adopters, matching the
### rule the annual C&S design uses.
base <- enforcement %>%
 filter(!is.na(compulsory_law_year)) %>%
 mutate(
  law_decade = (compulsory_law_year %/% 10L) * 10L,
  region = if_else(state_abbr %in% southern_states, "South", "Non-South")
 )

requested <- Sys.getenv("MS_ATTENDANCE_YEARS", unset = "")

years_with_attendance <- base %>%
 filter(if_any(all_of(available_measures), \(x) !is.na(x))) %>%
 distinct(year) %>%
 arrange(year) %>%
 pull(year)

selected_years <- if (identical(tolower(requested), "all")) {
 years_with_attendance
} else if (nzchar(requested)) {
 as.integer(str_trim(str_split_1(requested, ",")))
} else {
 ### 1930 is absent today and appears here automatically once the NHGIS
 ### education extract lands.
 c(1920L, 1930L)
}

selected_years <- intersect(selected_years, years_with_attendance)

if (length(selected_years) == 0) {
 stop("None of the requested years have attendance data. Available: ",
      paste(years_with_attendance, collapse = ", "),
      "\nSet MS_ATTENDANCE_YEARS to one of those, or 'all'.")
}

### "all" asks for whatever exists, so nothing can be missing.
missing_years <- if (identical(tolower(requested), "all")) {
 integer(0)
} else {
 setdiff(
  if (nzchar(requested)) {
   as.integer(str_trim(str_split_1(requested, ",")))
  } else {
   c(1920L, 1930L)
  },
  years_with_attendance
 )
}

if (length(missing_years) > 0) {
 message("No attendance data for: ", paste(missing_years, collapse = ", "),
         ". Available years: ", paste(years_with_attendance, collapse = ", "))
}

###############################################################################
# Statistics
###############################################################################

### Rates are stored as proportions; report percentages because that is how
### attendance is read. Everything is unweighted: the requested statistics
### describe the spread across counties, and the correct 7-13 denominator is
### not in the panel, so weighting by the 7-20 population would mis-weight.
first_non_missing <- function(x) {
 x <- x[!is.na(x)]
 if (length(x) == 0) NA_character_ else as.character(x[1])
}

attendance_stats <- function(x) {
 x <- 100 * x[!is.na(x)]
 tibble(
  counties = length(x),
  ### Only the age-banded measures restrict the numerator to the
  ### denominator's ages. The 1850 rate counts attendance at any age against
  ### persons 5-19, and the 1890 rate counts common-school pupils enrolled at
  ### any point in the year against persons 5-20, so both can exceed 100 in a
  ### county with a college or a wide catchment. Report that rather than
  ### hiding it: 1850 has 2 such counties, 1890 has 17, peaking at 186.
  counties_over_100 = sum(x > 100),
  mean = mean(x),
  sd = sd(x),
  min = min(x),
  p25 = unname(quantile(x, 0.25)),
  median = median(x),
  p75 = unname(quantile(x, 0.75)),
  max = max(x)
 )
}

summarise_measure <- function(data, year_value, measure, group_vars) {
 rows <- data %>%
  filter(year == year_value, !is.na(.data[[measure]]))

 if (nrow(rows) == 0) return(tibble())

 rows %>%
  group_by(across(all_of(group_vars))) %>%
  group_modify(\(g, key) {
   bind_cols(
    tibble(
     states = n_distinct(g$state_fips),
     southern_states = n_distinct(g$state_fips[g$region == "South"]),
     mean_years_since_law = mean(g$years_since_law, na.rm = TRUE)
    ),
    attendance_stats(g[[measure]])
   )
  }) %>%
  ungroup() %>%
  mutate(
   year = year_value,
   measure = measure,
   measure_label = unname(candidate_measures[measure]),
   universe = first_non_missing(rows$attendance_universe),
   .before = 1
  )
}

### An All row pooling every county, so the group rows can be read against the
### overall distribution rather than against each other alone.
build_table <- function(group_vars) {
 map_dfr(selected_years, \(year_value) {
  map_dfr(available_measures, \(measure) {
   bind_rows(
    summarise_measure(mutate(base, law_decade = NA_integer_),
                      year_value, measure, group_vars),
    summarise_measure(base, year_value, measure, group_vars)
   )
  })
 }) %>%
  mutate(law_decade_label = if_else(is.na(law_decade), "All",
                                    as.character(law_decade))) %>%
  relocate(law_decade_label, .after = law_decade) %>%
  arrange(year, match(measure, names(candidate_measures)),
          is.na(law_decade) == FALSE, law_decade)
}

main_table <- build_table("law_decade")
region_table <- build_table(c("law_decade", "region"))

###############################################################################
# Checks
###############################################################################

### A quantile out of order, or a mean outside the range, would mean the
### grouping or the rate construction is wrong.
stopifnot(
 all(main_table$p25 <= main_table$median + 1e-9),
 all(main_table$median <= main_table$p75 + 1e-9),
 all(main_table$min <= main_table$mean + 1e-9),
 all(main_table$mean <= main_table$max + 1e-9),
 all(main_table$min >= 0),
 all(main_table$southern_states <= main_table$states)
)

### The age-banded measures are matched numerator-to-denominator by
### construction, so a rate above 100 there would be a real error - most
### likely the A7N/A7M bands misaligned.
age_banded <- filter(main_table, measure != "school_attendance_rate")

stopifnot(nrow(age_banded) == 0 || all(age_banded$max <= 100))

over_100 <- sum(main_table$counties_over_100[is.na(main_table$law_decade)])

if (over_100 > 0) {
 message("Counties with a rate above 100%: ", over_100,
         " - unmatched numerator/denominator universes in 1850 and 1890; ",
         "see notes.txt.")
}

### The region split must partition the group exactly.
region_totals <- region_table %>%
 filter(!is.na(law_decade)) %>%
 group_by(year, measure, law_decade) %>%
 summarise(counties = sum(counties), .groups = "drop")

main_totals <- main_table %>%
 filter(!is.na(law_decade)) %>%
 select(year, measure, law_decade, counties)

reconciliation <- full_join(region_totals, main_totals,
                            by = c("year", "measure", "law_decade"),
                            suffix = c("_region", "_main"))

stopifnot(all(reconciliation$counties_region == reconciliation$counties_main))

###############################################################################
# Export
###############################################################################

round_stats <- function(x) {
 mutate(x, across(c(mean, sd, min, p25, median, p75, max, mean_years_since_law),
                  \(v) round(v, 1)))
}

write_csv(round_stats(main_table),
          file.path(output_dir, "attendance_by_law_decade.csv"))
write_csv(round_stats(region_table),
          file.path(output_dir, "attendance_by_law_decade_region.csv"))

notes <- c(
 "Attendance rate by compulsory-schooling-law decade",
 strrep("=", 60),
 "",
 paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
 paste0("Input:     ", input_path),
 paste0("Input md5: ", tools::md5sum(input_path)),
 paste0("Years:     ", paste(selected_years, collapse = ", ")),
 "",
 "THIS TABLE IS DESCRIPTIVE. IT IS NOT A CAUSAL ESTIMATE.",
 "",
 "1. Law-adoption decade is almost perfectly collinear with region. All ten",
 "   states adopting in the 1910s are Southern; the 1870s (13 states) and",
 "   1880s (10 states) cohorts contain none. The gradient across law decades",
 "   is therefore the North-South gap at least as much as it is law timing.",
 "   See attendance_by_law_decade_region.csv for the within-region split.",
 "",
 "2. By 1920 every state had a compulsory-attendance law, so there is no",
 "   untreated comparison group. Variation across groups is in length of",
 "   exposure, not in treatment status.",
 "",
 "3. For a causal answer, use the annual Callaway-Sant'Anna design with",
 "   not-yet-treated controls in analysis/mandatory_schooling/.",
 "",
 "Construction",
 strrep("-", 60),
 "Unit:      one county per observation, so sd/p25/p75 describe spread",
 "           across counties within a law-decade group. This mixes",
 "           within-state and between-state variation, since every county",
 "           in a state shares its law year.",
 "Grouping:  law_decade = (compulsory_law_year %/% 10) * 10, from Lingwall",
 "           (2014) Table 6.2. Alaska and Hawaii have no law date and are",
 "           excluded rather than treated as never-adopters.",
 "Weighting: none. The requested statistics are a distribution across",
 "           counties. The correct denominator for the 7-13 rate (A7M002)",
 "           is not exposed in the panel - only the aggregate 7-20",
 "           school_age_population - so weighting by it would mis-weight.",
 "Units:     all statistics are percentages.",
 "Over 100:  counties_over_100 counts counties whose rate exceeds 100.",
 "           Only the age-banded measures restrict the numerator to the",
 "           denominator's ages. The 1850 rate counts attendance at any age",
 "           against persons 5-19, and the 1890 rate counts common-school",
 "           pupils enrolled at any point in the year against persons 5-20,",
 "           so either can exceed 100 in a county with a college or a wide",
 "           catchment. 1850 has 2 such counties, 1890 has 17 (max 186).",
 "           The 1920 age-banded rates have none, which is a check that the",
 "           A7N attendance bands are aligned with their A7M denominators.",
 "",
 paste0("Southern states (Census South plus DC): ",
        paste(southern_states, collapse = ", ")),
 "",
 "Measures",
 strrep("-", 60),
 paste0("  ", available_measures, " - ",
        unname(candidate_measures[available_measures])),
 "",
 "Universes differ by year (1850 counts attendance at any institution,",
 "1890 only common-school pupils, 1920 persons within an age band), so the",
 "universe column must be read before comparing across years.",
 "",
 "1930 was the year originally requested. It is not in Dropbox: the county",
 "panel stops at 1920. Once the NHGIS education extract lands, re-running",
 "this script picks 1930 up with no code change."
)

writeLines(notes, file.path(output_dir, "notes.txt"))

###############################################################################
# Report
###############################################################################

for (year_value in selected_years) {
 for (measure in available_measures) {
  rows <- main_table %>% filter(year == year_value, measure == !!measure)
  if (nrow(rows) == 0) next

  cat("\n=== ", year_value, ": ", unname(candidate_measures[measure]),
      " (%) ===\n", sep = "")
  print(
   rows %>%
    transmute(law_decade = law_decade_label, counties, states,
              southern = southern_states,
              yrs_since_law = round(mean_years_since_law),
              mean = round(mean, 1), sd = round(sd, 1), min = round(min, 1),
              p25 = round(p25, 1), median = round(median, 1),
              p75 = round(p75, 1), max = round(max, 1)),
   n = Inf, width = Inf
  )
 }
}

cat("\nDescriptive only: law decade is nearly collinear with region",
    "(all ten 1910s adopters are Southern).\n")
cat("See attendance_by_law_decade_region.csv for the within-region split.\n")

message("\nSaved: ", file.path(output_dir, "attendance_by_law_decade.csv"))
message("Saved: ", file.path(output_dir, "attendance_by_law_decade_region.csv"))
message("Saved: ", file.path(output_dir, "notes.txt"))
