###############################################################################
# Project: GTL Talent Determinants
# Goal: Build county-level NHGIS slavery and literacy panel
###############################################################################

rm(list = ls())

library("tidyverse")
library("arrow")

source("raw_paths.R")

###############################################################################
# Paths
###############################################################################

demographics_parquet_dir <- raw_dir("nhgis_demographics", "parquet")

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
 transmute(
  year,
  source_dataset,
  total_population_var,
  slave_vars = map_chr(slave_vars, \(x) paste(x, collapse = " + ")),
  illiterate_vars = map_chr(illiterate_vars, \(x) paste(x, collapse = " + ")),
  population_10plus_var,
  literacy_universe,
  literacy_construction
 )

write_csv(
 demographic_variable_map,
 output_file_path("county_nhgis_demographics_variable_map.csv")
)

message("Saved demographics panel: ", output_file_path("county_nhgis_demographics_panel.csv"))
message("Saved unmatched audit: ", output_file_path("county_nhgis_demographics_unmatched_historical_counties.csv"))
