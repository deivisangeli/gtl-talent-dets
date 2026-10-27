###############################################################################
# Project: GTL Talent Determinants
# Goal: Build county-level NHGIS slavery, literacy and schooling panel
#
# Literacy coverage is limited by what the census actually asked. The
# decennial literacy question ("able to read and write") ran 1840-1930 and was
# dropped afterwards; from 1940 the census asked years of school completed
# instead. NHGIS has no county literacy table for 1860, 1880 or 1890. So:
#
#   1840-1930  direct literacy          -> literacy_rate
#   1940-1950  attainment proxy only    -> *_share_25plus, literacy_rate = NA
#
# The 1790-1920 variable codes below are hand-verified against the codebooks.
# The 1930-1950 codes are resolved from the codebooks at build time instead,
# because they are split by sex and vary by dataset; see
# prep/nhgis_education_helpers.R.
#
# Inputs:
#   raw/nhgis_demographics/parquet/  (prep/download_county_data.r)
#   raw/nhgis_education/             (prep/download_county_data.r)
#   input/nhgis0001_ts_nominal_county.csv  GISJOIN -> FIPS crosswalk
#
# School attendance is the direct first stage for compulsory-attendance-law
# enforcement. County coverage is 1850, 1890 and 1920 only: NHGIS has no
# county school table for 1860 or 1900, and 1880 is state-level only.
# Universes differ by year, so the series is comparable only with that caveat.
#
# Output:
#   output/county_nhgis_demographics_panel.csv / .parquet   keyed (GEOID, year)
#   output/county_nhgis_demographics_variable_map.csv
#   output/county_nhgis_education_code_resolution.csv
#   output/county_nhgis_demographics_unmatched_historical_counties.csv
###############################################################################

rm(list = ls())

library("tidyverse")
library("arrow")

source("raw_paths.R")
source("nhgis_education_helpers.R")

###############################################################################
# Paths
###############################################################################

demographics_parquet_dir <- raw_dir("nhgis_demographics", "parquet")
education_dir <- raw_dir("nhgis_education")
education_parquet_dir <- raw_dir("nhgis_education", "parquet")

###############################################################################
# Helpers
###############################################################################

find_latest_parquet <- function(directory, pattern) {
 files <- list.files(directory, pattern = pattern, full.names = TRUE)
 files <- files[!str_detect(basename(files), "_with_total")]

 if (length(files) == 0) {
  stop("No parquet file matching pattern '", pattern, "' in ", directory)
 }

 files[which.max(file.info(files)$mtime)]
}

as_numeric_col <- function(data, var) {
 if (is.na(var) || !var %in% names(data)) {
  return(rep(NA_real_, nrow(data)))
 }
 suppressWarnings(as.numeric(data[[var]]))
}

sum_numeric_cols <- function(data, vars) {
 vars <- vars[vars %in% names(data)]
 if (length(vars) == 0) {
  return(rep(NA_real_, nrow(data)))
 }
 rowSums(as.data.frame(map(data[vars], ~ suppressWarnings(as.numeric(.x)))),
         na.rm = TRUE)
}

first_non_missing <- function(x) {
 x <- x[!is.na(x)]
 if (length(x) == 0) NA else x[1]
}

###############################################################################
# GISJOIN -> current/nominal FIPS GEOID crosswalk
###############################################################################

county_crosswalk <- read_csv(
 manual_input_path("nhgis0001_ts_nominal_county.csv"),
 show_col_types = FALSE
) %>%
 transmute(
  GISJOIN,
  statefp = STATEFP,
  countyfp = COUNTYFP,
  GEOID = if_else(!is.na(STATEFP) & !is.na(COUNTYFP),
                  paste0(STATEFP, COUNTYFP),
                  NA_character_),
  state_crosswalk = STATE,
  county_crosswalk = COUNTY
 ) %>%
 distinct(GISJOIN, .keep_all = TRUE)

###############################################################################
# Variable map
###############################################################################

demographic_specs <- tribble(
 ~year, ~source_dataset, ~file_pattern, ~total_population_var, ~slave_vars, ~illiterate_vars, ~population_10plus_var, ~literacy_universe, ~literacy_construction,
 1790L, "1790_cPop", "ds1_1790_county[.]parquet$", "AAA001", list("AAI001"), list(character()), NA_character_, NA_character_, NA_character_,
 1800L, "1800_cPop", "ds2_1800_county[.]parquet$", "AAS001", list("AAY002"), list(character()), NA_character_, NA_character_, NA_character_,
 1810L, "1810_cPop", "ds3_1810_county[.]parquet$", "AA1001", list("AA7002"), list(character()), NA_character_, NA_character_, NA_character_,
 1820L, "1820_cPop", "ds4_1820_county[.]parquet$", "ABA001", list(c("ABB003", "ABB004")), list(character()), NA_character_, NA_character_, NA_character_,
 1830L, "1830_cPop", "ds5_1830_county[.]parquet$", "ABN001", list(c("ABO003", "ABO004")), list(character()), NA_character_, NA_character_, NA_character_,
 1840L, "1840_cPopX", "ds7_1840_county[.]parquet$", "ACD001", list("ACS003"), list("ACK001"), NA_character_, "White persons over 20", "Direct NHGIS count of white persons over 20 who cannot read and write; not comparable to later total-population literacy counts.",
 1850L, "1850_cPAX", "ds10_1850_county[.]parquet$", "ADQ001", list("AE6003"), list("AEC001"), NA_character_, "Adults", "Direct NHGIS aggregate number of adults who cannot read or write.",
 1860L, "1860_cPAX", "ds14_1860_county[.]parquet$", "AG3001", list("AHB001"), list(character()), NA_character_, NA_character_, NA_character_,
 1870L, "1870_cPAX", "ds17_1870_county[.]parquet$", "AJ3001", list(character()), list("AJ8001"), NA_character_, "Persons who cannot write", "Direct NHGIS count of persons who cannot write; AJ7001 is separately retained as cannot_read_persons.",
 1880L, "1880_cPAX", "ds23_1880_county[.]parquet$", "AOT001", list(character()), list(character()), NA_character_, NA_character_, NA_character_,
 1890L, "1890_cPHAM", "ds27_1890_county[.]parquet$", "AUM001", list(character()), list(character()), NA_character_, NA_character_, NA_character_,
 1900L, "1900_cPHAM", "ds31_1900_county[.]parquet$", "AYM001", list(character()), list(c("AYS001", "AYS002", "AYS003", "AYS004")), NA_character_, "Illiterate persons 10 years and over", "Constructed from race/nativity categories. Excludes AYS005 because it is a Negro subtotal already included in AYS004.",
 1910L, "1910_cPHA", "ds37_1910_county[.]parquet$", "A3Y001", list(character()), list("A38001"), "A37001", "Persons 10 years and over", "Literate persons equals population 10 years and over minus illiterate persons 10 years and over.",
 1920L, "1920_cPHAM", "ds43_1920_county[.]parquet$", "A7L001", list(character()), list("A7U001"), "A7S001", "Persons 10 years and over", "Literate persons equals population 10 years and over minus illiterate persons 10 years and over."
)

###############################################################################
# 1930-1950 education: codes resolved from the shipped codebooks
###############################################################################

education_specs <- tribble(
 ~year, ~measure_type, ~topic_pattern, ~measure_note,
 1930L, "direct_literacy", "illiter|cannot read|cannot write",
 "Last census with a literacy question. Illiterate persons 10 years and over over population 10 years and over.",
 1940L, "attainment_proxy", "school years completed|years of school|school completed|educational attainment",
 "No literacy question in 1940. Shares derived from years of school completed by persons 25 years and over.",
 1950L, "attainment_proxy", "school years completed|years of school|school completed|educational attainment",
 "No literacy question in 1950. Shares derived from years of school completed by persons 25 years and over."
)

find_education_file <- function(directory, pattern) {
 files <- list.files(directory, pattern = pattern, full.names = TRUE,
                     recursive = TRUE)

 if (length(files) == 0) {
  stop("No education file matching '", pattern, "' under ", directory,
       ". Run prep/download_county_data.r first.")
 }

 files[which.max(file.info(files)$mtime)]
}

education_codebook_entries <- function(year) {
 nhgis_codebook_entries(
  find_education_file(
   education_dir,
   paste0("ds[0-9]+_", year, "_county_codebook[.]txt$")
  )
 )
}

extract_education_panel <- function(spec, crosswalk) {
 parquet_file <- find_education_file(
  education_parquet_dir,
  paste0("ds[0-9]+_", spec$year, "_county[.]parquet$")
 )
 data <- read_parquet(parquet_file)
 entries <- education_codebook_entries(spec$year)

 total_population_var <- resolve_single_total(
  nhgis_select_tables(entries, "^total population$"),
  "total population", spec$year
 )
 total_population <- as_numeric_col(data, total_population_var$code)

 resolution <- tibble()
 illiterate_persons <- rep(NA_real_, nrow(data))
 population_10plus <- rep(NA_real_, nrow(data))
 literate_persons <- rep(NA_real_, nrow(data))
 illiterate_vars <- NA_character_
 population_10plus_var <- NA_character_
 bin_counts <- list()
 median_school_years <- rep(NA_real_, nrow(data))

 if (spec$measure_type == "direct_literacy") {
  numerator <- resolve_single_total(
   nhgis_select_tables(entries, spec$topic_pattern),
   "illiteracy", spec$year
  )

  ### The denominator is a separate table; exclude the illiteracy table
  ### itself so its own "10 years and over" wording cannot match. Anchored on
  ### a population table because the same extract now also carries school
  ### attendance, and a title like "Population 10 to 20 Years Attending
  ### School" would otherwise be a candidate.
  denominator <- resolve_single_total(
   nhgis_select_tables(entries, "^(total )?population 10 years") %>%
    filter(!table_index %in% numerator$table_index),
   "population 10 years and over", spec$year
  )

  illiterate_persons <- as_numeric_col(data, numerator$code)
  population_10plus <- as_numeric_col(data, denominator$code)
  literate_persons <- population_10plus - illiterate_persons
  illiterate_vars <- numerator$code
  population_10plus_var <- denominator$code

  resolution <- bind_rows(numerator, denominator) %>%
   mutate(year = spec$year, bin = c("illiterate", "population_10plus"),
          sex_split = FALSE)
 } else {
  bins <- resolve_school_year_bins(entries, spec)
  resolution <- bins

  bin_counts <- split(bins$code, bins$bin) %>%
   map(\(codes) sum_numeric_cols(data, codes))

  ### The published median is a rate, so it cannot be summed across sexes.
  ### Carry it only when the table gives a single unambiguous value.
  median_codes <- bins %>% filter(bin == "median")

  if (nrow(median_codes) == 1) {
   median_school_years <- as_numeric_col(data, median_codes$code)
  }
 }

 pick_bin <- function(name) {
  if (is.null(bin_counts[[name]])) rep(NA_real_, nrow(data)) else bin_counts[[name]]
 }
 sum_bins <- function(wanted) {
  present <- intersect(wanted, names(bin_counts))
  if (length(present) == 0) {
   rep(NA_real_, nrow(data))
  } else {
   reduce(map(present, pick_bin), `+`)
  }
 }

 population_25plus_reported <- sum_bins(nhgis_school_years_required_bins)
 low_education <- sum_bins(nhgis_school_years_low_bins)
 elementary_complete <- sum_bins(nhgis_school_years_elementary_complete_bins)

 share_of_reported <- function(x) {
  if_else(!is.na(population_25plus_reported) & population_25plus_reported > 0,
          x / population_25plus_reported, NA_real_)
 }

 panel <- data %>%
  transmute(
   GISJOIN,
   state_nhgis = STATE,
   county_nhgis = COUNTY,
   statea_nhgis = as.character(STATEA),
   countya_nhgis = as.character(COUNTYA),
   year = spec$year,
   total_population = total_population,
   ### Slavery ended in 1865; these years carry a structural zero, matching
   ### how extract_demographic_panel() treats 1870 onwards.
   total_slaves = 0,
   slave_share = if_else(total_population > 0, 0, NA_real_),
   illiterate_persons = illiterate_persons,
   illiterate_share_total_population = if_else(
    total_population > 0, illiterate_persons / total_population, NA_real_
   ),
   cannot_read_persons = NA_real_,
   cannot_read_share_total_population = NA_real_,
   population_10plus = population_10plus,
   literate_persons = literate_persons,
   illiterate_share_10plus = if_else(
    population_10plus > 0, illiterate_persons / population_10plus, NA_real_
   ),
   literacy_rate = if_else(population_10plus > 0,
                           literate_persons / population_10plus, NA_real_),
   literacy_universe = if (spec$measure_type == "direct_literacy") {
    "Persons 10 years and over"
   } else {
    NA_character_
   },
   literacy_construction = spec$measure_note,
   literacy_measure_type = spec$measure_type,
   population_25plus_reported = population_25plus_reported,
   no_schooling_persons_25plus = pick_bin("none"),
   low_education_persons_25plus = low_education,
   no_schooling_share_25plus = share_of_reported(pick_bin("none")),
   low_education_share_25plus = share_of_reported(low_education),
   elementary_complete_share_25plus = share_of_reported(elementary_complete),
   median_school_years_25plus = median_school_years,
   source_dataset = str_extract(basename(parquet_file), "ds[0-9]+"),
   total_population_var = total_population_var$code,
   slave_vars = NA_character_,
   illiterate_vars = illiterate_vars,
   population_10plus_var = population_10plus_var,
   source_file = parquet_file
  ) %>%
  left_join(crosswalk, by = "GISJOIN")

 list(panel = panel, resolution = mutate(resolution, year = spec$year))
}

###############################################################################
# 1850-1920 school attendance: codes hand-verified from the codebooks
###############################################################################

### Compulsory attendance laws mandate exactly this, so attendance is the
### direct first stage for enforcement, measured alongside the 1850-1880 law
### dates rather than decades later.
###
### These tables arrived with extracts downloaded for other domains - 1850
### with the mandatory-schooling matching extract, 1890 and 1920 with the
### manufacturing extract - so each spec carries its own source directory and
### no new extract is needed. The demographics extract requested only a
### handful of tables per year and contains none of them.
###
### Universes differ, so the series is comparable only with that caveat:
###   1850  persons attending any institution, including colleges/academies
###   1890  pupils on the rolls of common schools only
###   1920  persons attending school, by age band
###
### The headline rate is deliberately broad so the three years line up: 1920
### sums all of its age bands rather than using 7-13 alone. The bands are also
### kept separately, because 7-13 is the compulsory-attendance age.
###
### NHGIS has no county school table for 1860 (verified against the full
### 41-table metadata) or 1900 (all 55 tables downloaded, none school).

### 1850 Table 4 (AEL) is Race/Slave Status by Sex by Age in six blocks of
### fifteen: White M/F, Nonwhite Free M/F, Nonwhite Slave M/F. Within each
### block, positions 3, 4 and 5 are ages 5-9, 10-14 and 15-19.
ael_school_age <- function(blocks) {
 sprintf("AEL%03d", unlist(map(blocks, \(b) (b - 1L) * 15L + 3:5)))
}

white_5_19_1850 <- ael_school_age(1:2)
free_nonwhite_5_19_1850 <- ael_school_age(3:4)

### The 1850 attendance universe is "White and Free Colored Persons Attending
### School", so the denominator must be the free population 5-19: counting
### enslaved children, who were largely barred from schooling, would depress
### the rate in slave states for a reason that is slavery rather than weak
### schooling.
###
### The denominator is built from AEL rather than from AEO001, the published
### 5-19 total, because AEL decomposes by race and so the total and the
### race-specific rates share one universe. AEO001 is itself already
### free-population-only - it is 0.94 to 1.00 of the AEL free total, against
### 0.42 to 0.63 of the all-persons total in slave states - and is retained in
### school_age_population_published as an independent check.
attendance_specs <- list(
 list(
  year = 1850L,
  source_dir = file.path(raw_dir("mandatory_schooling_matching"), "csv"),
  file_pattern = "ds10_1850_county[.]csv$",
  reader = "csv",
  attending_vars = "AD7001",
  school_age_vars = c(white_5_19_1850, free_nonwhite_5_19_1850),
  published_school_age_var = "AEO001",
  white_attending_vars = "AD5001",
  white_school_age_vars = white_5_19_1850,
  nonwhite_attending_vars = "AD5002",
  nonwhite_school_age_vars = free_nonwhite_5_19_1850,
  bands = NULL,
  age_span = "5 to 19",
  universe = "Persons attending school, any institution; free population",
  construction = paste(
   "AD7001 aggregate persons attending school over free persons aged 5-19",
   "(AEL blocks White and Nonwhite: Free, bins 5-9, 10-14, 15-19).",
   "Enslaved children are excluded from the denominator to match the",
   "numerator universe; the published AEO001 total is free-only as well and",
   "is kept in school_age_population_published. Includes attendance at",
   "colleges and academies, so this is not a common-school rate."
  )
 ),
 list(
  year = 1890L,
  source_dir = raw_dir("nhgis_manufacturing", "parquet"),
  file_pattern = "ds27_1890_county[.]parquet$",
  reader = "parquet",
  attending_vars = sprintf("AVK%03d", 5:8),
  school_age_vars = sprintf("AUN%03d", 1:6),
  published_school_age_var = NA_character_,
  white_attending_vars = sprintf("AVK%03d", 5:6),
  white_school_age_vars = sprintf("AUN%03d", 1:4),
  nonwhite_attending_vars = sprintf("AVK%03d", 7:8),
  nonwhite_school_age_vars = sprintf("AUN%03d", 5:6),
  bands = NULL,
  age_span = "5 to 20",
  universe = "Pupils in common schools",
  construction = paste(
   "Pupils in common schools (AVK005-008, race by sex) over persons aged",
   "5-20 (AUN001-006, race/nativity by sex). Common schools only, so",
   "academies and colleges are excluded; not comparable to 1850 without",
   "noting the universe change."
  )
 ),
 list(
  year = 1920L,
  source_dir = raw_dir("nhgis_manufacturing", "parquet"),
  file_pattern = "ds43_1920_county[.]parquet$",
  reader = "parquet",
  attending_vars = sprintf("A7N%03d", 1:4),
  school_age_vars = sprintf("A7M%03d", 2:5),
  published_school_age_var = NA_character_,
  white_attending_vars = NA_character_,
  white_school_age_vars = NA_character_,
  nonwhite_attending_vars = NA_character_,
  nonwhite_school_age_vars = NA_character_,
  ### A7N has four bands starting at 7-13; A7M has five, the first of which
  ### is Under 7. Pair them by label, never by index: A7N00k goes with
  ### A7M00(k+1), and A7M001 has no attendance partner at all.
  bands = list(
   list(name = "7_13", attending = "A7N001", population = "A7M002"),
   list(name = "14_15", attending = "A7N002", population = "A7M003"),
   list(name = "16_17", attending = "A7N003", population = "A7M004"),
   list(name = "18_20", attending = "A7N004", population = "A7M005")
  ),
  age_span = "7 to 20",
  universe = "Persons attending school, by age",
  construction = paste(
   "Persons attending school aged 7-20 (A7N001-004) over persons aged 7-20",
   "(A7M002-005). Summed across all bands so the rate is comparable with the",
   "1850 and 1890 totals; attendance_rate_7_13 is the compulsory-attendance",
   "age band. Children under 7 (A7M001) have no attendance count and are",
   "excluded from both sides."
  )
 )
)

find_source_file <- function(directory, pattern) {
 files <- list.files(directory, pattern = pattern, full.names = TRUE,
                     recursive = TRUE)

 if (length(files) == 0) {
  stop("No school-attendance source matching '", pattern, "' under ", directory)
 }

 files[which.max(file.info(files)$mtime)]
}

extract_attendance_panel <- function(spec) {
 source_file <- find_source_file(spec$source_dir, spec$file_pattern)
 data <- if (spec$reader == "csv") {
  nhgis_read_county_csv(source_file)
 } else {
  read_parquet(source_file)
 }

 wanted <- unique(na.omit(unlist(spec[grepl("_vars$|_var$", names(spec))])))
 if (!is.null(spec$bands)) {
  wanted <- unique(c(wanted, map_chr(spec$bands, "attending"),
                     map_chr(spec$bands, "population")))
 }
 absent <- setdiff(wanted, names(data))

 if (length(absent) > 0) {
  stop("Attendance variables missing for ", spec$year, ": ",
       paste(absent, collapse = ", "), " in ", source_file)
 }

 ### Strict: a partially missing denominator would understate the population
 ### and inflate the rate, so propagate NA rather than dropping components.
 strict_sum <- function(vars) {
  vars <- na.omit(vars)
  if (length(vars) == 0) return(rep(NA_real_, nrow(data)))
  totals <- rowSums(as.matrix(as.data.frame(
   map(data[vars], \(x) suppressWarnings(as.numeric(x)))
  )))
  totals[!is.finite(totals)] <- NA_real_
  totals
 }

 rate <- function(numerator, denominator) {
  if_else(!is.na(numerator) & !is.na(denominator) &
           denominator > 0 & numerator >= 0,
          numerator / denominator, NA_real_)
 }

 attending <- strict_sum(spec$attending_vars)
 school_age <- strict_sum(spec$school_age_vars)

 panel <- tibble(
  GISJOIN = data$GISJOIN,
  year = spec$year,
  school_attendance_persons = attending,
  school_age_population = school_age,
  school_age_population_published = strict_sum(spec$published_school_age_var),
  school_attendance_rate = rate(attending, school_age),
  school_attendance_rate_white = rate(strict_sum(spec$white_attending_vars),
                                      strict_sum(spec$white_school_age_vars)),
  school_attendance_rate_nonwhite = rate(strict_sum(spec$nonwhite_attending_vars),
                                         strict_sum(spec$nonwhite_school_age_vars)),
  attendance_age_span = spec$age_span,
  attendance_universe = spec$universe,
  attendance_construction = spec$construction,
  attendance_source_file = source_file
 )

 for (band in spec$bands) {
  panel[[paste0("attendance_rate_", band$name)]] <-
   rate(strict_sum(band$attending), strict_sum(band$population))
 }

 panel
}

###############################################################################
# Build panel
###############################################################################

extract_demographic_panel <- function(spec, crosswalk) {
 parquet_file <- find_latest_parquet(demographics_parquet_dir, spec$file_pattern)
 data <- read_parquet(parquet_file)

 total_population <- as_numeric_col(data, spec$total_population_var)
 slave_vars <- unlist(spec$slave_vars, use.names = FALSE)
 illiterate_vars <- unlist(spec$illiterate_vars, use.names = FALSE)
 total_slaves <- sum_numeric_cols(data, slave_vars)
 illiterate_persons <- sum_numeric_cols(data, illiterate_vars)
 population_10plus <- as_numeric_col(data, spec$population_10plus_var)
 cannot_read_persons <- if (spec$year == 1870L) {
  as_numeric_col(data, "AJ7001")
 } else {
  rep(NA_real_, nrow(data))
 }

 if (spec$year >= 1870L && length(slave_vars) == 0) {
  total_slaves <- rep(0, nrow(data))
 }

 literate_persons <- if (!is.na(spec$population_10plus_var) &&
                         length(illiterate_vars) > 0) {
  population_10plus - illiterate_persons
 } else {
  rep(NA_real_, nrow(data))
 }

 data %>%
  transmute(
   GISJOIN,
   state_nhgis = STATE,
   county_nhgis = COUNTY,
   statea_nhgis = as.character(STATEA),
   countya_nhgis = as.character(COUNTYA),
   year = spec$year,
   total_population = total_population,
   total_slaves = total_slaves,
   slave_share = if_else(total_population > 0, total_slaves / total_population, NA_real_),
   illiterate_persons = illiterate_persons,
   illiterate_share_total_population = if_else(
    total_population > 0,
    illiterate_persons / total_population,
    NA_real_
   ),
   cannot_read_persons = cannot_read_persons,
   cannot_read_share_total_population = if_else(
    total_population > 0,
    cannot_read_persons / total_population,
    NA_real_
   ),
   population_10plus = population_10plus,
   literate_persons = literate_persons,
   illiterate_share_10plus = if_else(
    population_10plus > 0,
    illiterate_persons / population_10plus,
    NA_real_
   ),
   literacy_rate = if_else(population_10plus > 0,
                           literate_persons / population_10plus,
                           NA_real_),
   literacy_universe = spec$literacy_universe,
   literacy_construction = spec$literacy_construction,
   literacy_measure_type = if (length(illiterate_vars) > 0) {
    "direct_literacy"
   } else {
    NA_character_
   },
   source_dataset = spec$source_dataset,
   total_population_var = spec$total_population_var,
   slave_vars = paste(slave_vars, collapse = " + "),
   illiterate_vars = paste(illiterate_vars, collapse = " + "),
   population_10plus_var = spec$population_10plus_var,
   source_file = parquet_file
  ) %>%
  left_join(crosswalk, by = "GISJOIN")
}

demographics_all <- map_dfr(
 seq_len(nrow(demographic_specs)),
 \(i) extract_demographic_panel(demographic_specs[i, ], county_crosswalk)
)

### The 1930-1950 years need their own NHGIS extract, which needs an IPUMS API
### key. Coauthors who only want the historical panel can set
### SKIP_NHGIS_EDUCATION=true; the default is to fail loudly rather than write
### a panel that silently stops at 1920.
skip_education <- tolower(Sys.getenv("SKIP_NHGIS_EDUCATION")) %in%
 c("true", "1", "yes")

### raw_dir() creates the directory it returns, so dir.exists() is always
### TRUE here and cannot be the test. Look for actual parquet files.
education_extract_present <- length(
 list.files(education_parquet_dir, pattern = "[.]parquet$")
) > 0

if (!skip_education && !education_extract_present) {
 stop(
  "No NHGIS education extract at ", education_parquet_dir, ".\n",
  "Run prep/download_county_data.r (needs IPUMS_API_KEY or IPUMS_KEY), or\n",
  "set SKIP_NHGIS_EDUCATION=true to build the 1790-1920 panel only."
 )
}

education_all <- if (skip_education) {
 message("SKIP_NHGIS_EDUCATION set: building 1790-1920 only.")
 list()
} else {
 map(
  seq_len(nrow(education_specs)),
  \(i) extract_education_panel(education_specs[i, ], county_crosswalk)
 )
}

education_resolution <- map_dfr(education_all, "resolution")

### bind_rows fills the columns each half does not have: attainment shares are
### NA for 1790-1920, slavery counts are NA for 1930-1950.
demographics_all <- bind_rows(demographics_all, map_dfr(education_all, "panel"))

### Hold the output schema fixed whether or not the education years were
### built, so a skipped run produces the same columns rather than a narrower
### file that downstream code would have to special-case.
attainment_columns <- c(
 "population_25plus_reported", "no_schooling_persons_25plus",
 "low_education_persons_25plus", "no_schooling_share_25plus",
 "low_education_share_25plus", "elementary_complete_share_25plus",
 "median_school_years_25plus"
)

for (column in setdiff(attainment_columns, names(demographics_all))) {
 demographics_all[[column]] <- NA_real_
}

### School attendance joins onto rows that already exist, so it adds columns
### and never rows. The attendance tables come from a different extract but
### the same underlying NHGIS datasets (ds10/1850, ds27/1890, ds43/1920), so
### GISJOIN matches exactly; anything that fails to join is a real problem.
attendance_all <- map_dfr(attendance_specs, extract_attendance_panel)

stopifnot(!anyDuplicated(attendance_all[, c("GISJOIN", "year")]))

attendance_unjoined <- anti_join(
 attendance_all, demographics_all, by = c("GISJOIN", "year")
)

if (nrow(attendance_unjoined) > 0) {
 stop(nrow(attendance_unjoined), " attendance rows have no matching county ",
      "in the demographics panel; first: ",
      paste(head(attendance_unjoined$GISJOIN, 5), collapse = ", "))
}

rows_before_attendance <- nrow(demographics_all)

demographics_all <- left_join(demographics_all, attendance_all,
                              by = c("GISJOIN", "year"))

stopifnot(nrow(demographics_all) == rows_before_attendance)

demographics_unmatched <- demographics_all %>%
 filter(is.na(GEOID)) %>%
 arrange(year, state_nhgis, county_nhgis)

demographics_panel <- demographics_all %>%
 filter(!is.na(GEOID)) %>%
 arrange(GEOID, year) %>%
 group_by(GEOID, year) %>%
 summarise(
  state_nhgis = first_non_missing(state_nhgis),
  county_nhgis = first_non_missing(county_nhgis),
  total_population = first_non_missing(total_population),
  total_slaves = first_non_missing(total_slaves),
  slave_share = first_non_missing(slave_share),
  illiterate_persons = first_non_missing(illiterate_persons),
  illiterate_share_total_population = first_non_missing(illiterate_share_total_population),
  cannot_read_persons = first_non_missing(cannot_read_persons),
  cannot_read_share_total_population = first_non_missing(cannot_read_share_total_population),
  population_10plus = first_non_missing(population_10plus),
  literate_persons = first_non_missing(literate_persons),
  illiterate_share_10plus = first_non_missing(illiterate_share_10plus),
  literacy_rate = first_non_missing(literacy_rate),
  literacy_universe = first_non_missing(literacy_universe),
  literacy_construction = first_non_missing(literacy_construction),
  literacy_measure_type = first_non_missing(literacy_measure_type),
  population_25plus_reported = first_non_missing(population_25plus_reported),
  no_schooling_persons_25plus = first_non_missing(no_schooling_persons_25plus),
  low_education_persons_25plus = first_non_missing(low_education_persons_25plus),
  no_schooling_share_25plus = first_non_missing(no_schooling_share_25plus),
  low_education_share_25plus = first_non_missing(low_education_share_25plus),
  elementary_complete_share_25plus = first_non_missing(elementary_complete_share_25plus),
  median_school_years_25plus = first_non_missing(median_school_years_25plus),
  school_attendance_persons = first_non_missing(school_attendance_persons),
  school_age_population = first_non_missing(school_age_population),
  school_age_population_published = first_non_missing(school_age_population_published),
  school_attendance_rate = first_non_missing(school_attendance_rate),
  school_attendance_rate_white = first_non_missing(school_attendance_rate_white),
  school_attendance_rate_nonwhite = first_non_missing(school_attendance_rate_nonwhite),
  attendance_rate_7_13 = first_non_missing(attendance_rate_7_13),
  attendance_rate_14_15 = first_non_missing(attendance_rate_14_15),
  attendance_rate_16_17 = first_non_missing(attendance_rate_16_17),
  attendance_rate_18_20 = first_non_missing(attendance_rate_18_20),
  attendance_age_span = first_non_missing(attendance_age_span),
  attendance_universe = first_non_missing(attendance_universe),
  attendance_construction = first_non_missing(attendance_construction),
  source_dataset = first_non_missing(source_dataset),
  total_population_var = first_non_missing(total_population_var),
  slave_vars = first_non_missing(slave_vars),
  illiterate_vars = first_non_missing(illiterate_vars),
  population_10plus_var = first_non_missing(population_10plus_var),
  .groups = "drop"
 )

###############################################################################
# Export
###############################################################################

write_csv(demographics_panel, output_file_path("county_nhgis_demographics_panel.csv"))
write_parquet(demographics_panel, output_file_path("county_nhgis_demographics_panel.parquet"))

write_csv(
 demographics_unmatched,
 output_file_path("county_nhgis_demographics_unmatched_historical_counties.csv")
)

demographic_variable_map <- demographic_specs %>%
 ### Computed before the list columns are flattened below, otherwise this
 ### reads the collapsed string and every year looks like it has literacy.
 mutate(has_literacy = map_int(illiterate_vars, \(x) length(unlist(x))) > 0) %>%
 transmute(
  year,
  source_dataset,
  total_population_var,
  ### Each tribble cell is itself a list, so unwrap one level before
  ### collapsing; paste() on a list deparses it instead of joining it.
  slave_vars = map_chr(slave_vars, \(x) paste(unlist(x), collapse = " + ")),
  illiterate_vars = map_chr(illiterate_vars, \(x) paste(unlist(x), collapse = " + ")),
  population_10plus_var,
  literacy_universe,
  literacy_construction,
  literacy_measure_type = if_else(has_literacy, "direct_literacy", NA_character_)
 )

### The 1930-1950 half of the map is reconstructed from what was actually
### resolved, so the exported provenance reflects the codes used rather than
### the patterns that found them.
education_variable_map <- if (nrow(education_resolution) == 0) {
 demographic_variable_map[0, ]
} else {
 education_resolution %>%
 group_by(year) %>%
 summarise(
  source_dataset = first_non_missing(source_code),
  total_population_var = NA_character_,
  slave_vars = "",
  illiterate_vars = paste(code[bin == "illiterate"], collapse = " + "),
  population_10plus_var = first_non_missing(code[bin == "population_10plus"]),
  literacy_universe = first_non_missing(universe),
  literacy_construction = paste(sort(unique(table_description)), collapse = "; "),
  .groups = "drop"
 ) %>%
 left_join(select(education_specs, year, literacy_measure_type = measure_type),
           by = "year")
}

write_csv(
 bind_rows(demographic_variable_map, education_variable_map),
 output_file_path("county_nhgis_demographics_variable_map.csv")
)

### Every resolved 1930-1950 code with the description it was matched on, so
### a mis-mapped bin is visible without re-reading the codebooks.
write_csv(
 education_resolution,
 output_file_path("county_nhgis_education_code_resolution.csv")
)

coverage <- demographics_panel %>%
 group_by(year) %>%
 summarise(
  counties = n(),
  literacy_rate = sum(!is.na(literacy_rate)),
  illiterate_share_total_population = sum(!is.na(illiterate_share_total_population)),
  low_education_share_25plus = sum(!is.na(low_education_share_25plus)),
  school_attendance_rate = sum(!is.na(school_attendance_rate)),
  .groups = "drop"
 )

print(coverage, n = Inf)

write_csv(coverage, output_file_path("county_nhgis_demographics_coverage.csv"))

message("Saved demographics panel: ", output_file_path("county_nhgis_demographics_panel.csv"))
message("Saved unmatched audit: ", output_file_path("county_nhgis_demographics_unmatched_historical_counties.csv"))
message("Saved education code resolution: ", output_file_path("county_nhgis_education_code_resolution.csv"))
