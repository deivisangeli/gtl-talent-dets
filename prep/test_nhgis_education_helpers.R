###############################################################################
# Project: GTL Talent Determinants
# Goal: Check the NHGIS education codebook parser and code resolvers
#
# The 1930-1950 variable codes are resolved from codebooks rather than
# hand-listed, so the resolution logic carries the risk that hand-verified
# codes would carry. These tests pin the parser against the real 1900/1920
# codebooks and the resolvers against synthetic codebooks covering the failure
# modes that would otherwise be silent: subtotals summed into their parent,
# a sex split double counted against its own total, and an unrecognised bin
# quietly treated as zero.
#
# Run from the repository root or from prep/:
#   Rscript prep/test_nhgis_education_helpers.R
###############################################################################

suppressPackageStartupMessages({
 library("tidyverse")
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
  if (parent == candidate) stop("Cannot resolve repository root; set GTL_REPO.")
  candidate <- parent
 }
 normalizePath(candidate, winslash = "/")
})

source(file.path(repo_root, "prep", "nhgis_education_helpers.R"))

failures <- 0L

check <- function(label, expr) {
 ok <- isTRUE(tryCatch(expr, error = function(e) {
  message("    error: ", conditionMessage(e))
  FALSE
 }))
 if (!ok) failures <<- failures + 1L
 cat(if (ok) "  PASS  " else "  FAIL  ", label, "\n", sep = "")
}

check_error <- function(label, expr, pattern) {
 caught <- tryCatch({
  force(expr)
  NULL
 }, error = function(e) conditionMessage(e))

 ok <- !is.null(caught) && str_detect(caught, regex(pattern, ignore_case = TRUE))
 if (!ok) {
  failures <<- failures + 1L
  message("    expected an error matching '", pattern, "', got: ",
          if (is.null(caught)) "no error" else caught)
 }
 cat(if (ok) "  PASS  " else "  FAIL  ", label, "\n", sep = "")
}

### Build a codebook file in the real NHGIS layout from a list of tables.
write_codebook <- function(tables) {
 path <- tempfile(fileext = ".txt")
 header <- c(
  strrep("-", 80), "Data Dictionary", strrep("-", 80), " ",
  "Context Fields ",
  "    GISJOIN:     GIS Join Match Code",
  "    STATE:       State Name",
  " "
 )
 body <- imap(tables, \(tbl, i) {
  c(
   paste0("Table ", i, ":     ", tbl$description),
   paste0("Universe:    ", tbl$universe),
   paste0("Source code: ", tbl$source_code),
   paste0("NHGIS code:  ", substr(tbl$codes[[1]]$code, 1, 3)),
   map_chr(tbl$codes, \(v) paste0("    ", v$code, ":      ", v$description)),
   " "
  )
 })
 write_lines(c(header, flatten_chr(body)), path)
 path
}

var <- function(code, description) list(code = code, description = description)

school_bins <- function(prefix) {
 labels <- c(
  "No school years completed", "Elementary: 1 to 4 years",
  "Elementary: 5 and 6 years", "Elementary: 7 years", "Elementary: 8 years",
  "High school: 1 to 3 years", "High school: 4 years",
  "College: 1 to 3 years", "College: 4 years or more",
  "School years not reported"
 )
 map(seq_along(labels), \(i) {
  n <- if (prefix == "") i else i + switch(prefix, "Male" = 0, "Female" = 20, 40)
  var(sprintf("B1A%03d", n),
      if (prefix == "") labels[i] else paste0(prefix, " >> ", labels[i]))
 })
}

###############################################################################
cat("\n-- parser against the real codebooks already in raw/ --\n")
###############################################################################

real_codebooks <- file.path(
 Sys.getenv("DET_DIR"), "raw", "nhgis_demographics", "nhgis0004_csv",
 c("nhgis0004_ds43_1920_county_codebook.txt",
   "nhgis0004_ds31_1900_county_codebook.txt")
)

if (all(file.exists(real_codebooks))) {
 entries_1920 <- nhgis_codebook_entries(real_codebooks[1])
 entries_1900 <- nhgis_codebook_entries(real_codebooks[2])

 check("1920 parses to 3 tables and 3 variables",
       nrow(entries_1920) == 3 && n_distinct(entries_1920$table_index) == 3)
 check("1920 illiteracy variable is A7U001",
       resolve_single_total(nhgis_select_tables(entries_1920, "illiter"),
                            "illiteracy", 1920)$code == "A7U001")
 check("1920 denominator excludes the illiteracy table",
       resolve_single_total(
        nhgis_select_tables(entries_1920, "10 years") %>%
         filter(!str_detect(table_description, regex("illiter", ignore_case = TRUE))),
        "population 10 plus", 1920
       )$code == "A7S001")
 check("context fields are not parsed as variables",
       !any(c("GISJOIN", "STATE") %in% entries_1920$code))
 check("1900 race table parses all five categories",
       sum(entries_1900$table_index == 2) == 5)
 check_error(
  "1900 race/nativity table is refused rather than summed",
  resolve_single_total(nhgis_select_tables(entries_1900, "illiter"),
                       "illiteracy", 1900),
  "ambiguous"
 )
} else {
 cat("  SKIP  real codebooks not found under DET_DIR\n")
}

###############################################################################
cat("\n-- 1930-style direct literacy --\n")
###############################################################################

cb_1930 <- write_codebook(list(
 list(description = "Total Population", universe = "Persons",
      source_code = "NT1", codes = list(var("BAA001", "Total"))),
 list(description = "Population 10 Years of Age and Over",
      universe = "Persons 10 Years and Over", source_code = "NT12",
      codes = list(var("BAB001", "Total"))),
 list(description = "Illiterate Persons 10 Years of Age and Over",
      universe = "Illiterate Persons 10 Years and Over", source_code = "NT13",
      codes = list(var("BAC001", "Total")))
))

entries_1930 <- nhgis_codebook_entries(cb_1930)

check("1930 total population resolves",
      resolve_single_total(nhgis_select_tables(entries_1930, "^total population$"),
                           "total population", 1930)$code == "BAA001")

numerator_1930 <- resolve_single_total(
 nhgis_select_tables(entries_1930, "illiter|cannot read|cannot write"),
 "illiteracy", 1930
)
check("1930 illiteracy resolves", numerator_1930$code == "BAC001")
check("1930 denominator excludes the illiteracy table",
      resolve_single_total(
       nhgis_select_tables(entries_1930, "10 years") %>%
        filter(!table_index %in% numerator_1930$table_index),
       "population 10 plus", 1930
      )$code == "BAB001")

###############################################################################
cat("\n-- 1950-style attainment, sex split --\n")
###############################################################################

spec_1950 <- tibble(
 year = 1950L,
 topic_pattern = "school years completed|years of school|school completed|educational attainment"
)

cb_1950 <- write_codebook(list(
 list(description = "Total Population", universe = "Persons",
      source_code = "NT1", codes = list(var("B0A001", "Total"))),
 list(description = "Years of School Completed by Sex",
      universe = "Persons 25 Years and Over", source_code = "NT30",
      codes = c(school_bins("Male"), school_bins("Female")))
))

bins_1950 <- resolve_school_year_bins(nhgis_codebook_entries(cb_1950), spec_1950)

check("every required bin resolves",
      all(nhgis_school_years_required_bins %in% bins_1950$bin))
check("sex split is detected", all(bins_1950$sex_split))
check("each count bin gets one code per sex",
      all(count(filter(bins_1950, bin %in% nhgis_school_years_required_bins),
                bin)$n == 2))
check("'not reported' is resolved but excluded from the required bins",
      "not_reported" %in% bins_1950$bin &&
       !"not_reported" %in% nhgis_school_years_required_bins)
check("no bin is left unclassified", !any(is.na(bins_1950$bin)))

### The bins that build the headline rates must not overlap.
check("low-education and elementary-complete bins are disjoint",
      length(intersect(nhgis_school_years_low_bins,
                       nhgis_school_years_elementary_complete_bins)) == 0)
check("low-education bins are 'no school' and '1 to 4 years' only",
      setequal(nhgis_school_years_low_bins, c("none", "elementary_1_4")))

###############################################################################
cat("\n-- double-count and missing-data guards --\n")
###############################################################################

cb_total_and_sex <- write_codebook(list(
 list(description = "Years of School Completed by Sex",
      universe = "Persons 25 Years and Over", source_code = "NT30",
      codes = c(school_bins(""), school_bins("Male"), school_bins("Female")))
))

bins_total_and_sex <- resolve_school_year_bins(
 nhgis_codebook_entries(cb_total_and_sex), spec_1950
)

check("a parent Total column is dropped when a sex split is present",
      all(count(filter(bins_total_and_sex,
                       bin %in% nhgis_school_years_required_bins), bin)$n == 2))

cb_unclassified <- write_codebook(list(
 list(description = "Years of School Completed",
      universe = "Persons 25 Years and Over", source_code = "NT30",
      codes = c(school_bins(""), list(var("B1A099", "Postgraduate wizardry"))))
))

check_error(
 "an unrecognised bin stops the build instead of counting as zero",
 resolve_school_year_bins(nhgis_codebook_entries(cb_unclassified), spec_1950),
 "unclassified"
)

cb_missing_bin <- write_codebook(list(
 list(description = "Years of School Completed",
      universe = "Persons 25 Years and Over", source_code = "NT30",
      codes = school_bins("")[1:4])
))

check_error(
 "a missing bin stops the build",
 resolve_school_year_bins(nhgis_codebook_entries(cb_missing_bin), spec_1950),
 "missing years-of-school-completed bins"
)

cb_two_tables <- write_codebook(list(
 list(description = "Years of School Completed, Male",
      universe = "Males 25 Years and Over", source_code = "NT30",
      codes = school_bins("")),
 list(description = "Years of School Completed, Female",
      universe = "Females 25 Years and Over", source_code = "NT31",
      codes = school_bins(""))
))

check_error(
 "two candidate attainment tables stop the build",
 resolve_school_year_bins(nhgis_codebook_entries(cb_two_tables), spec_1950),
 "exactly one"
)

check_error(
 "an absent topic stops the build",
 resolve_single_total(nhgis_select_tables(entries_1930, "no such topic"),
                      "illiteracy", 1930),
 "no illiteracy variable resolved"
)

###############################################################################
cat("\n-- bin classification of the published 1950 labels --\n")
###############################################################################

expected <- tribble(
 ~description, ~bin,
 "Male >> No school years completed", "none",
 "Male >> Elementary: 1 to 4 years", "elementary_1_4",
 "Male >> Elementary: 5 and 6 years", "elementary_5_6",
 "Male >> Elementary: 7 years", "elementary_7",
 "Male >> Elementary: 8 years", "elementary_8",
 "Male >> High school: 1 to 3 years", "high_school_1_3",
 "Male >> High school: 4 years", "high_school_4",
 "Male >> College: 1 to 3 years", "college_1_3",
 "Male >> College: 4 years or more", "college_4plus",
 "Male >> School years not reported", "not_reported",
 "Median school years completed", "median"
)

check("published 1950 labels classify as expected",
      identical(nhgis_school_years_bin(expected$description), expected$bin))

### "4 years" inside a college label must not fall through to high school.
check("'College: 4 years' is college, not high school",
      nhgis_school_years_bin("College: 4 years") == "college_4plus")
check("'not reported' wins over any digits in the label",
      nhgis_school_years_bin("7 years, not reported") == "not_reported")

###############################################################################
cat("\n-- 1850 AEL age-block arithmetic --\n")
###############################################################################

### Mirrors ael_school_age() in build_county_demographics_panel.R. Table 4 is
### six blocks of fifteen (White M/F, Nonwhite Free M/F, Nonwhite Slave M/F);
### positions 3, 4, 5 in each block are ages 5-9, 10-14, 15-19.
ael_school_age <- function(blocks) {
 sprintf("AEL%03d", unlist(lapply(blocks, \(b) (b - 1L) * 15L + 3:5)))
}

check("white 5-19 resolves to the documented codes",
      identical(ael_school_age(1:2),
                c("AEL003", "AEL004", "AEL005", "AEL018", "AEL019", "AEL020")))
check("free nonwhite 5-19 resolves to the documented codes",
      identical(ael_school_age(3:4),
                c("AEL033", "AEL034", "AEL035", "AEL048", "AEL049", "AEL050")))
check("enslaved blocks are the ones excluded from the denominator",
      identical(ael_school_age(5:6),
                c("AEL063", "AEL064", "AEL065", "AEL078", "AEL079", "AEL080")))
check("free and enslaved school-age codes never overlap",
      length(intersect(ael_school_age(1:4), ael_school_age(5:6))) == 0)

### Verified against the real codebook, which labels AEL003 "White >> Male >>
### 5 to 9 years of age" and AEL031 "Nonwhite: Free >> Male >> Under 1 year".
real_1850_codebook <- file.path(
 Sys.getenv("DET_DIR"), "raw", "mandatory_schooling_matching", "csv",
 "nhgis0005_csv", "nhgis0005_ds10_1850_county_codebook.txt"
)

if (file.exists(real_1850_codebook)) {
 entries_1850 <- nhgis_codebook_entries(real_1850_codebook)
 label <- function(code) entries_1850$description[entries_1850$code == code]

 check("AEL003 really is White Male 5 to 9",
       grepl("^White >> Male >> 5 to 9", label("AEL003")))
 check("AEL020 really is White Female 15 to 19",
       grepl("^White >> Female >> 15 to 19", label("AEL020")))
 check("AEL033 really is Nonwhite Free Male 5 to 9",
       grepl("^Nonwhite: Free >> Male >> 5 to 9", label("AEL033")))
 check("AEL063 really is an enslaved block, correctly excluded",
       grepl("^Nonwhite: Slave", label("AEL063")))
 check("every free school-age code is a 5-9, 10-14 or 15-19 bin",
       all(grepl("(5 to 9|10 to 14|15 to 19) years of age",
                 vapply(ael_school_age(1:4), label, character(1)))))
 check("no free school-age code belongs to an enslaved block",
       !any(grepl("Slave", vapply(ael_school_age(1:4), label, character(1)))))
 check("the attendance numerator AD7001 is the aggregate attending count",
       grepl("aggregate number of persons attending",
             label("AD7001"), ignore.case = TRUE))
} else {
 cat("  SKIP  1850 codebook not found under DET_DIR\n")
}

###############################################################################
cat("\n-- 1920 attendance/population band pairing --\n")
###############################################################################

### The hazard: A7N has four bands starting at 7-13 while A7M has five, the
### first being Under 7, so pairing by index would silently divide attendance
### for 7-13 by the population under 7 and report a rate well over 100%.
bands_1920 <- list(
 list(name = "7_13", attending = "A7N001", population = "A7M002"),
 list(name = "14_15", attending = "A7N002", population = "A7M003"),
 list(name = "16_17", attending = "A7N003", population = "A7M004"),
 list(name = "18_20", attending = "A7N004", population = "A7M005")
)

real_1920_codebook <- file.path(
 Sys.getenv("DET_DIR"), "raw", "nhgis_manufacturing", "nhgis0002_csv",
 "nhgis0002_ds43_1920_county_codebook.txt"
)

if (file.exists(real_1920_codebook)) {
 entries_1920 <- nhgis_codebook_entries(real_1920_codebook)
 label_1920 <- function(code) entries_1920$description[entries_1920$code == code]

 ### Both sides of every pair must name the same age range.
 age_of <- function(code) {
  sub("^([0-9]+ to [0-9]+).*$", "\\1", label_1920(code))
 }

 mismatched <- Filter(
  \(band) age_of(band$attending) != age_of(band$population), bands_1920
 )

 check("every band pairs attendance with its own age range",
       length(mismatched) == 0)
 check("the compulsory band is 7 to 13 on both sides",
       age_of("A7N001") == "7 to 13" && age_of("A7M002") == "7 to 13")
 check("A7M001 is the under-7 band that has no attendance partner",
       grepl("^Under 7", label_1920("A7M001")) &&
        !"A7M001" %in% vapply(bands_1920, \(b) b$population, character(1)))
 check("pairing by index would have been wrong",
       age_of("A7N001") != age_of("A7M001"))
 check("attendance bands are exactly A7N001 to A7N004",
       identical(sort(vapply(bands_1920, \(b) b$attending, character(1))),
                 sprintf("A7N%03d", 1:4)))
} else {
 cat("  SKIP  1920 codebook not found under DET_DIR\n")
}

###############################################################################
cat("\n-- attendance rate construction --\n")
###############################################################################

### A partially missing denominator must propagate NA, not silently sum the
### components that happen to be present and inflate the rate.
strict_sum <- function(frame, vars) {
 totals <- rowSums(as.matrix(frame[, vars, drop = FALSE]))
 totals[!is.finite(totals)] <- NA_real_
 totals
}

demo <- data.frame(a = c(10, 10, 10), b = c(5, NA, 5), c = c(5, 5, 0))

check("a complete row sums normally", strict_sum(demo, c("a", "b", "c"))[1] == 20)
check("a partially missing row becomes NA",
      is.na(strict_sum(demo, c("a", "b", "c"))[2]))

rate <- function(numerator, denominator) {
 ifelse(!is.na(numerator) & !is.na(denominator) &
         denominator > 0 & numerator >= 0,
        numerator / denominator, NA_real_)
}

check("a zero denominator gives NA, not Inf", is.na(rate(5, 0)))
check("a missing denominator gives NA", is.na(rate(5, NA)))
check("an ordinary rate is the plain ratio", rate(9, 10) == 0.9)

###############################################################################

cat("\n", if (failures == 0L) "All checks passed." else
    paste0(failures, " check(s) FAILED."), "\n", sep = "")

quit(status = if (failures == 0L) 0L else 1L)
