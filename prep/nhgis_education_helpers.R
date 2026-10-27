###############################################################################
# Project: GTL Talent Determinants
# Goal: Resolve NHGIS literacy and educational-attainment variable codes
#
# The 1790-1920 demographics tables have hand-verified variable codes in
# build_county_demographics_panel.R. The 1930-1950 education tables do not:
# their codes vary by dataset and are usually split by sex, so they are
# resolved from the codebooks that ship with the extract and audited, rather
# than guessed.
#
# Pure functions only. All inputs and outputs are passed explicitly.
###############################################################################

### Parse the Data Dictionary of an NHGIS codebook into one row per variable,
### carrying its table context. Format (verified against the 1900 and 1920
### codebooks already in raw/nhgis_demographics):
###
###   Table 3:     Illiterate Persons 10 Years of Age and Over
###   Universe:    Illiterate Persons 10 Years and Over
###   Source code: NT20
###   NHGIS code:  A7U
###       A7U001:      Total
nhgis_codebook_entries <- function(codebook_path) {
 lines <- readr::read_lines(codebook_path)

 dictionary_start <- which(stringr::str_detect(lines, "^Data Dictionary\\s*$"))
 if (length(dictionary_start) != 1) {
  stop("Expected exactly one Data Dictionary section in ", codebook_path)
 }
 lines <- lines[seq(dictionary_start, length(lines))]

 table_description <- stringr::str_match(lines, "^Table\\s+[0-9]+:\\s+(.*?)\\s*$")[, 2]
 universe <- stringr::str_match(lines, "^Universe:\\s+(.*?)\\s*$")[, 2]
 source_code <- stringr::str_match(lines, "^Source code:\\s+(.*?)\\s*$")[, 2]
 variable <- stringr::str_match(lines, "^\\s{2,}([A-Z0-9]{3}[0-9]{3}):\\s+(.*?)\\s*$")

 ### Carry each header forward until the next table begins.
 carry <- function(x) {
  last <- NA_character_
  for (i in seq_along(x)) {
   if (!is.na(x[i])) last <- x[i] else x[i] <- last
  }
  x
 }

 tibble::tibble(
  table_index = cumsum(!is.na(table_description)),
  table_description = carry(table_description),
  universe = carry(universe),
  source_code = carry(source_code),
  code = variable[, 2],
  description = variable[, 3]
 ) |>
  dplyr::filter(!is.na(code), table_index > 0) |>
  dplyr::mutate(codebook_file = codebook_path) |>
  dplyr::select(table_index, source_code, table_description, universe,
                code, description, codebook_file)
}

### Read a county-level NHGIS `csv_header` extract.
###
### Needed because raw/mandatory_schooling_matching is the only NHGIS folder
### without a parquet mirror, so the 1850 attendance tables are reachable only
### as CSV. The second row of a csv_header file is a descriptive header, not
### data; drop it by keeping only real GISJOIN codes. Same idiom as
### prep/mandatory_schooling/matching_covariate_helpers.R, kept here so the
### shared builder does not source across pipelines.
nhgis_read_county_csv <- function(path) {
 x <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                      locale = readr::locale(encoding = "latin1"),
                      show_col_types = FALSE, progress = FALSE)
 x <- x[stringr::str_detect(x$GISJOIN, "^G[0-9]+$"), ]
 codes <- grep("^[A-Z0-9]{3}[0-9]{3}$", names(x), value = TRUE)
 for (code in codes) {
  x[[code]] <- suppressWarnings(as.numeric(x[[code]]))
 }
 x
}

### Select the codebook tables whose description or universe matches a topic.
nhgis_select_tables <- function(entries, pattern) {
 dplyr::filter(
  entries,
  stringr::str_detect(table_description, stringr::regex(pattern, ignore_case = TRUE)) |
   stringr::str_detect(dplyr::coalesce(universe, ""),
                       stringr::regex(pattern, ignore_case = TRUE))
 )
}

### Map a years-of-school-completed variable description onto a bin.
### Descriptions are hierarchical and usually sex-split, e.g.
### "Male >> Elementary: 1 to 4 years". Returns NA for anything unrecognised;
### callers must treat NA as a failure to resolve, never as a zero.
nhgis_school_years_bin <- function(description) {
 d <- tolower(description)
 has <- function(pattern) stringr::str_detect(d, pattern)

 dplyr::case_when(
  has("not reported|unknown|not stated") ~ "not_reported",
  has("median")                          ~ "median",
  has("no school|none")                  ~ "none",
  has("college") & has("4 or more|4 years or more|4 year") ~ "college_4plus",
  has("college")                         ~ "college_1_3",
  has("high school") & has("4 year")     ~ "high_school_4",
  has("high school")                     ~ "high_school_1_3",
  has("1 to 4|1-4")                      ~ "elementary_1_4",
  has("5 and 6|5 to 6|5-6")              ~ "elementary_5_6",
  has("[^0-9]7 year|[^0-9]7th")          ~ "elementary_7",
  has("[^0-9]8 year|[^0-9]8th")          ~ "elementary_8",
  TRUE                                   ~ NA_character_
 )
}

### The count bins that must all be present before an attainment rate is
### trusted. "median" is optional; "not_reported" is excluded from the
### denominator rather than required.
nhgis_school_years_required_bins <- c(
 "none", "elementary_1_4", "elementary_5_6", "elementary_7", "elementary_8",
 "high_school_1_3", "high_school_4", "college_1_3", "college_4plus"
)

### Bins counted as "less than 5 years of school completed", the Census
### Bureau's own functional-illiteracy convention for 1950-1970.
nhgis_school_years_low_bins <- c("none", "elementary_1_4")

### Bins counted as having completed elementary school (8 years or more).
nhgis_school_years_elementary_complete_bins <- c(
 "elementary_8", "high_school_1_3", "high_school_4",
 "college_1_3", "college_4plus"
)

### Prefer an explicit "Total" variable, accept a lone variable, and refuse
### anything ambiguous. Summing every variable in a table is not safe: the
### 1900 race/nativity literacy table carries a Negro subtotal already
### included in its own parent, and that class of bug is silent.
resolve_single_total <- function(candidates, label, year) {
 if (nrow(candidates) == 0) {
  stop("No ", label, " variable resolved for ", year,
       ". Inspect the codebook and widen the pattern in education_specs.")
 }

 totals <- candidates %>%
  filter(str_detect(description, regex("^total$", ignore_case = TRUE)))

 if (nrow(totals) == 1) return(totals)
 if (nrow(candidates) == 1) return(candidates)

 stop(
  "Ambiguous ", label, " variables for ", year, ":\n",
  paste0("  ", candidates$code, "  [", candidates$table_description, "] ",
         candidates$description, collapse = "\n"),
  "\nPick one explicitly rather than summing these."
 )
}

### Resolve the years-of-school-completed bins for one attainment year.
resolve_school_year_bins <- function(entries, spec) {
 tables <- nhgis_select_tables(entries, spec$topic_pattern)

 if (n_distinct(tables$table_index) != 1) {
  stop(
   "Expected exactly one years-of-school-completed table for ", spec$year,
   ", found ", n_distinct(tables$table_index), ":\n",
   paste0("  ", unique(tables$table_description), collapse = "\n")
  )
 }

 bins <- tables %>%
  mutate(
   bin = nhgis_school_years_bin(description),
   prefix = str_trim(str_extract(description, "^[^>:]+"))
  )

 unclassified <- bins %>% filter(is.na(bin))

 if (nrow(unclassified) > 0) {
  stop(
   "Unclassified years-of-school-completed variables for ", spec$year, ":\n",
   paste0("  ", unclassified$code, ": ", unclassified$description,
          collapse = "\n"),
   "\nExtend nhgis_school_years_bin() rather than dropping them."
  )
 }

 ### A table carrying both a parent total and Male/Female columns would double
 ### count every bin. Where a sex split exists, keep only the sexed counts.
 ### The parent is not always labelled "Total" - in the published layout it is
 ### simply unprefixed - so select the sexed rows rather than excluding a name.
 ###
 ### The median is exempt: it is a rate, cannot be summed across sexes, and an
 ### unprefixed median is exactly the overall value worth keeping.
 has_sex_split <- any(str_detect(bins$prefix, regex("^(male|female)$",
                                                    ignore_case = TRUE)))

 if (has_sex_split) {
  bins <- bins %>%
   filter(str_detect(prefix, regex("^(male|female)$", ignore_case = TRUE)) |
           bin == "median")
 }

 missing_bins <- setdiff(nhgis_school_years_required_bins, bins$bin)

 if (length(missing_bins) > 0) {
  stop(
   "Missing years-of-school-completed bins for ", spec$year, ": ",
   paste(missing_bins, collapse = ", "),
   "\nResolved: ", paste(sort(unique(bins$bin)), collapse = ", ")
  )
 }

 bins %>%
  mutate(year = spec$year, sex_split = has_sex_split) %>%
  select(year, table_index, source_code, table_description, universe,
         code, description, bin, sex_split, codebook_file)
}
