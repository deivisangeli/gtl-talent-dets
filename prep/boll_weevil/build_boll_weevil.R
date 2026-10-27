# Recover Baker, Blanchette and Eriksson's deposited county treatment data.
# Run: Rscript prep/boll_weevil/build_boll_weevil.R (from any working directory).
suppressPackageStartupMessages({
  library(haven)
  library(readr)
  library(dplyr)
  library(tidyr)
})

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_arg)) {
  repo_root <- normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), "../.."),
                             winslash = "/", mustWork = TRUE)
} else {
  repo_root <- Sys.getenv("GTL_REPO", unset = getwd())
}
source(file.path(repo_root, "paths.R"))
input_dir <- file.path(TALENT_DETS_DATA_DIR, "Data", "raw", "boll weevil")
output_dir <- file.path(DATA_OUTPUT, "boll_weevil")
reference_file <- file.path(DATA_INPUT, "nhgis0001_ts_nominal_county.csv")
source_names <- c("BollWeevilByCensusYear.dta", "1889AGCensusCottonData.dta", "rose_exp.dta")
source_files <- file.path(input_dir, source_names)
required_files <- c(source_files, reference_file)
if (!all(file.exists(required_files))) {
  stop("Missing input(s): ", paste(required_files[!file.exists(required_files)], collapse = ", "),
       ". Set TALENT_DETS_DATA_DIR to the Dropbox project root.")
}
input_hashes <- tools::md5sum(required_files)
bw <- read_dta(source_files[1])
cotton <- read_dta(source_files[2])
reference <- read_csv(reference_file, col_types = cols(.default = col_character()),
                      show_col_types = FALSE, progress = FALSE) %>%
  select(STATE, STATEFP, COUNTY, COUNTYFP) %>% distinct()

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
required_bw <- c("censusyear", "fipscode", "countyname", "statename", "bw_first_arrival_year",
                 "bw_final_through_year", "bw_full_retreat", "bw_part_retreat", "bw_average_year")
required_cotton <- c("fipscode", "acimp", "cottonac", "farmarea", "bd", "countyname", "statename",
                     "cga1890_1920", "cga1890_1920_nm")
assert(all(required_bw %in% names(bw)), "Unexpected boll weevil source schema.")
assert(all(required_cotton %in% names(cotton)), "Unexpected agricultural source schema.")
assert(!anyDuplicated(bw[c("censusyear", "fipscode")]), "Duplicate source county-vintage key.")
assert(!anyDuplicated(cotton$fipscode), "Duplicate agricultural county key.")
assert(all(is.na(bw$fipscode) | bw$fipscode == floor(bw$fipscode)), "Noninteger county code.")
assert(all(is.na(bw$fipscode) | (bw$fipscode >= 100000 & bw$fipscode < 200000)),
       "Unexpected source county-code convention.")

normalize_name <- function(x) {
  x <- toupper(x)
  x <- gsub("\\b(COUNTY|PARISH|BOROUGH|CITY|MUNICIPALITY|CENSUS AREA)\\b", "", x)
  x <- gsub("SAINT", "ST", x, fixed = TRUE)
  gsub("[^A-Z0-9]", "", x)
}
# Only these two source-name variants are accepted beyond case/punctuation/suffixes.
name_overrides <- tibble(
  candidate_GEOID = c("22095", "22113"),
  source_countyname = c("ST JOHN THE BAPTI", "VERMILLION"),
  reference_name_normalized = c("STJOHNTHEBAPTIST", "VERMILION"),
  name_match_note = c("Deposited name truncates ST JOHN THE BAPTIST",
                      "Deposited VERMILLION spelling corresponds to VERMILION")
)
reference <- reference %>%
  filter(!is.na(STATEFP), !is.na(COUNTYFP)) %>%
  mutate(candidate_GEOID = paste0(STATEFP, COUNTYFP),
         reference_state_normalized = normalize_name(STATE),
         reference_name_normalized = normalize_name(COUNTY))

normalized <- bw %>%
  rename(county_geography_year = censusyear, source_fipscode = fipscode) %>%
  mutate(source_county_id = if_else(is.na(source_fipscode), NA_character_,
                                   sprintf("%.0f", source_fipscode)),
         candidate_GEOID = if_else(is.na(source_fipscode), NA_character_,
                                   sprintf("%05d", as.integer(source_fipscode - 100000))),
         county_normalized = normalize_name(countyname),
         state_normalized = normalize_name(statename))
geo_keys <- normalized %>%
  distinct(source_county_id, candidate_GEOID, countyname, statename,
           county_normalized, state_normalized) %>%
  left_join(name_overrides, by = c("candidate_GEOID", "countyname" = "source_countyname")) %>%
  rename(override_name_normalized = reference_name_normalized) %>%
  left_join(reference, by = "candidate_GEOID", relationship = "many-to-many") %>%
  mutate(name_ok = county_normalized == reference_name_normalized |
           (!is.na(override_name_normalized) & override_name_normalized == reference_name_normalized),
         match_ok = !is.na(candidate_GEOID) & !is.na(STATE) &
           state_normalized == reference_state_normalized & name_ok) %>%
  group_by(source_county_id, candidate_GEOID, countyname, statename) %>%
  summarise(geoid_verified = any(match_ok, na.rm = TRUE),
            reference_county_names = paste(sort(unique(COUNTY[!is.na(COUNTY)])), collapse = " | "),
            name_match_note = paste(unique(name_match_note[!is.na(name_match_note)]), collapse = " | "),
            .groups = "drop") %>%
  mutate(GEOID = if_else(geoid_verified, candidate_GEOID, NA_character_),
         geoid_match_status = case_when(
           is.na(source_county_id) ~ "missing_source_id",
           geoid_verified & name_match_note != "" ~ "verified_explicit_name_variant",
           geoid_verified ~ "verified_code_state_county_name",
           reference_county_names == "" ~ "no_nominal_reference_code",
           TRUE ~ "reference_name_conflict"))
normalized <- normalized %>%
  select(-county_normalized, -state_normalized) %>%
  left_join(geo_keys %>% select(-candidate_GEOID),
            by = c("source_county_id", "countyname", "statename"), relationship = "many-to-one") %>%
  mutate(statefp = substr(GEOID, 1, 2), countyfp = substr(GEOID, 3, 5)) %>%
  arrange(county_geography_year, statename, countyname)

agriculture <- cotton %>%
  rename(source_fipscode = fipscode, cotton_source_countyname = countyname,
         cotton_source_statename = statename) %>%
  mutate(cotton_source_matched = TRUE)
counties <- normalized %>% filter(county_geography_year == 1920) %>%
  left_join(agriculture, by = "source_fipscode", relationship = "one-to-one") %>%
  mutate(cotton_source_matched = coalesce(cotton_source_matched, FALSE),
         cotton_source_names_agree = if_else(cotton_source_matched,
           countyname == cotton_source_countyname & statename == cotton_source_statename, NA),
         infestation_gap_years = bw_final_through_year - bw_first_arrival_year,
         midpoint_year_recomputed = ceiling((bw_first_arrival_year + bw_final_through_year) / 2),
         midpoint_matches_source = if_else(!is.na(bw_average_year),
                                            bw_average_year == midpoint_year_recomputed, NA),
         cotton_intensity_1889 = if_else(!is.na(cottonac) & !is.na(acimp) & acimp > 0 & cottonac >= 0,
                                        cottonac / acimp, NA_real_),
         cotton_intensity_available = !is.na(cotton_intensity_1889),
         has_complete_infestation_date = !is.na(bw_final_through_year),
         eligible_timing = has_complete_infestation_date & !is.na(bw_first_arrival_year) &
           bw_full_retreat == 0 & !is.na(infestation_gap_years) & infestation_gap_years <= 4,
         eligible_southern_states = statename != "MISSOURI",
         eligible_baker_2018_rules = eligible_timing & eligible_southern_states,
         eligible_baker_2018_rules_with_cotton = eligible_baker_2018_rules & cotton_intensity_available,
         timing_exclusion_reason = case_when(
           is.na(bw_first_arrival_year) ~ "no_recorded_arrival_date",
           !has_complete_infestation_date ~ "no_recorded_complete_infestation_date",
           bw_full_retreat == 1 ~ "full_retreat",
           infestation_gap_years > 4 ~ "arrival_to_completion_exceeds_four_years",
           TRUE ~ "eligible"))

make_indicator <- function(year, date) if_else(is.na(date), NA_integer_, as.integer(year >= date))
panel <- expand_grid(source_county_id = counties$source_county_id, year = 1892:1922) %>%
  left_join(counties %>% select(source_county_id, county_geography_year, GEOID, countyname, statename,
       bw_first_arrival_year, bw_final_through_year, bw_average_year, cotton_intensity_1889,
       eligible_timing, eligible_baker_2018_rules, eligible_baker_2018_rules_with_cotton),
       by = "source_county_id", relationship = "many-to-one") %>%
  mutate(event_time_first_arrival = year - bw_first_arrival_year,
         event_time_complete = year - bw_final_through_year,
         event_time_midpoint = year - bw_average_year,
         derived_post_first_arrival = make_indicator(year, bw_first_arrival_year),
         derived_post_complete = make_indicator(year, bw_final_through_year),
         derived_post_midpoint = make_indicator(year, bw_average_year),
         treatment_status_basis = "derived_absorbing_timing_indicator_not_observed_infestation")

full <- sum(counties$has_complete_infestation_date)
no_retreat <- sum(counties$has_complete_infestation_date & counties$bw_full_retreat == 0)
reconciliation <- tibble(
  stage = c("all_counties_1920", "complete_infestation_date", "excluded_full_retreat_among_complete",
            "complete_without_full_retreat", "excluded_gap_over_four_among_remaining",
            "eligible_timing", "eligible_timing_excluding_missouri", "eligible_with_cotton"),
  deposited_count = c(nrow(counties), full, full - no_retreat, no_retreat,
                     no_retreat - sum(counties$eligible_timing), sum(counties$eligible_timing),
                     sum(counties$eligible_baker_2018_rules),
                     sum(counties$eligible_baker_2018_rules_with_cotton)),
  working_paper_2018_count = c(NA, 844, 120, 724, 38, 686, NA, NA),
  comparison_note = c("Deposited universe differs from the working-paper Cotton Belt universe",
    "Working paper reports 844 fully infested counties; deposit contains 843 in 1920",
    "Directly reported exclusion count", "Calculated as 844 minus 120 in working paper",
    "Directly reported exclusion count", "Calculated as 844 minus 120 minus 38 in working paper",
    "Additional geographic restriction: exclude Missouri; do not force counts to match",
    "Four eligible counties have missing cotton intensity; linked-person sample not reconstructed")
) %>% mutate(difference_from_paper = deposited_count - working_paper_2018_count)

checks <- tibble(
  check = c("source_records", "1920_counties", "annual_panel_rows", "unique_county_keys",
    "unique_panel_keys", "all_source_midpoints_match", "dates_in_1892_1922", "ordered_dates",
    "agricultural_matches", "matched_agricultural_names_agree", "timing_eligible",
    "southern_timing_eligible", "southern_eligible_with_cotton", "cotton_intensity_in_zero_one",
    "missing_dates_produce_missing_indicators", "post_indicators_switch_at_date"),
  observed = as.character(c(nrow(bw), nrow(counties), nrow(panel),
    !anyDuplicated(counties$source_county_id), !anyDuplicated(panel[c("source_county_id", "year")]),
    all(bw$bw_average_year == ceiling((bw$bw_first_arrival_year + bw$bw_final_through_year) / 2), na.rm = TRUE),
    all(unlist(bw[c("bw_first_arrival_year", "bw_final_through_year", "bw_average_year")]) %in%
          c(1892:1922, NA)),
    all(counties$infestation_gap_years >= 0, na.rm = TRUE), sum(counties$cotton_source_matched),
    all(counties$cotton_source_names_agree[counties$cotton_source_matched]),
    sum(counties$eligible_timing), sum(counties$eligible_baker_2018_rules),
    sum(counties$eligible_baker_2018_rules_with_cotton),
    all(between(counties$cotton_intensity_1889, 0, 1), na.rm = TRUE),
    all(is.na(panel$derived_post_first_arrival) == is.na(panel$bw_first_arrival_year)) &
      all(is.na(panel$derived_post_complete) == is.na(panel$bw_final_through_year)) &
      all(is.na(panel$derived_post_midpoint) == is.na(panel$bw_average_year)),
    identical(make_indicator(c(1899, 1900, 1901, 1900), c(1900, 1900, 1900, NA)),
              c(0L, 1L, 1L, NA_integer_)))),
  expected = as.character(c(4204, 1083, 33573, TRUE, TRUE, TRUE, TRUE, TRUE, 1019, TRUE,
                             685, 681, 677, TRUE, TRUE, TRUE))
) %>% mutate(passed = observed == expected)
assert(all(checks$passed), paste("Validation failed:", paste(checks$check[!checks$passed], collapse = ", ")))
assert(identical(unname(input_hashes), unname(tools::md5sum(required_files))), "Input files changed during build.")

tables <- list(
  boll_weevil_counties_1920 = counties,
  boll_weevil_county_year_1892_1922 = panel,
  boll_weevil_source_all_geographies = normalized,
  boll_weevil_geographic_match_audit = normalized %>% select(county_geography_year, source_county_id,
    source_fipscode, countyname, statename, candidate_GEOID, GEOID, geoid_match_status,
    reference_county_names, name_match_note),
  boll_weevil_agricultural_match_audit = counties %>% select(source_county_id, GEOID, countyname, statename,
    cotton_source_matched, cotton_source_countyname, cotton_source_statename, cotton_source_names_agree,
    acimp, cottonac, farmarea, bd, cga1890_1920, cga1890_1920_nm, cotton_intensity_1889,
    cotton_intensity_available, eligible_baker_2018_rules),
  boll_weevil_sample_count_reconciliation = reconciliation,
  boll_weevil_validation_checks = checks
)

descriptions <- c(
  county_geography_year = "Census geography vintage; canonical table uses 1920 boundaries.",
  source_fipscode = "Original deposited numeric fipscode, unchanged; candidate GEOID = fipscode - 100000.",
  source_county_id = "Character representation of original source_fipscode; missing source codes remain missing.",
  countyname = "County name exactly as deposited in the infestation file.",
  statename = "State name exactly as deposited in the infestation file.",
  candidate_GEOID = "Five-digit source_fipscode minus 100000; candidate only, not a modern boundary crosswalk.",
  GEOID = "Candidate code verified against NHGIS state and county names; missing when unverified. Geography remains historical.",
  geoid_verified = "TRUE when the candidate code, state and county name match NHGIS (including documented variants).",
  reference_county_names = "Nominal NHGIS county names associated with the candidate code, separated by |.",
  name_match_note = "Explicit Louisiana name-variant justification, otherwise empty.",
  geoid_match_status = "Verified exact/normalized name, verified explicit variant, absent reference, conflict, or missing source ID.",
  statefp = "First two characters of verified GEOID; character string preserving leading zeros.",
  countyfp = "Last three characters of verified GEOID; character string preserving leading zeros.",
  bw_first_arrival_year = "Deposited first partial or full infestation year; missing means no recorded date in this file.",
  bw_final_through_year = "Deposited permanent full-infestation date through the source horizon of 1922.",
  bw_full_retreat = "Deposited binary full-retreat indicator, unchanged.",
  bw_part_retreat = "Deposited binary partial-retreat indicator, unchanged; not a main timing exclusion.",
  bw_average_year = "Deposited ceiling of average first-arrival and permanent-full-infestation years.",
  cotton_source_countyname = "County name from agricultural source, unchanged.",
  cotton_source_statename = "State name from agricultural source, unchanged.",
  cotton_source_matched = "Agricultural source record found by original source_fipscode.",
  cotton_source_names_agree = "Exact state and county names agree across sources; missing if unmatched.",
  acimp = "Deposited improved farm acres; 1889 agricultural baseline. Fractional source allocations preserved.",
  cottonac = "Deposited cotton acreage in 1889; missing is not zero.",
  farmarea = "Deposited total farm area (acres), labeled 1890; not used as cotton-intensity denominator.",
  bd = "Deposited BD field, unchanged. Values resemble dates; construction is undocumented in supplied files.",
  cga1890_1920 = "Deposited geography flag, unchanged; exact construction undocumented in supplied files.",
  cga1890_1920_nm = "Deposited geography flag, unchanged; exact construction undocumented in supplied files.",
  infestation_gap_years = "Permanent full-infestation year minus first-arrival year; missing if either is missing.",
  midpoint_year_recomputed = "ceiling((bw_first_arrival_year + bw_final_through_year)/2).",
  midpoint_matches_source = "Recomputed and deposited midpoint dates agree; missing if deposited midpoint missing.",
  cotton_intensity_1889 = "cottonac/acimp where acimp > 0 and cottonac >= 0; otherwise missing; never imputed.",
  cotton_intensity_available = "TRUE when a valid cotton-intensity ratio can be calculated.",
  has_complete_infestation_date = "Permanent full-infestation year recorded in supplied source.",
  eligible_timing = "Both dates recorded, no full retreat, and arrival-to-completion gap <= 4 years.",
  eligible_southern_states = "State is not Missouri; all other deposited states are in the paper's Southern-state list.",
  eligible_baker_2018_rules = "eligible_timing AND eligible_southern_states; rule-based flag, not recovered linked-person sample.",
  eligible_baker_2018_rules_with_cotton = "eligible_baker_2018_rules AND cotton_intensity_available.",
  timing_exclusion_reason = "Ordered reason: missing arrival, missing completion, full retreat, gap > 4, or eligible.",
  year = "Calendar year, 1892 through 1922 inclusive.",
  event_time_first_arrival = "year minus first-arrival year; missing if date missing.",
  event_time_complete = "year minus permanent full-infestation year; missing if date missing.",
  event_time_midpoint = "year minus deposited midpoint infestation year; missing if date missing.",
  derived_post_first_arrival = "1(year >= first arrival), otherwise 0; missing date yields missing. Absorbing, not observed status.",
  derived_post_complete = "1(year >= permanent full infestation), otherwise 0; missing date yields missing. Derived indicator.",
  derived_post_midpoint = "1(year >= midpoint infestation), otherwise 0; missing date yields missing. Derived indicator.",
  treatment_status_basis = "Explicit warning that these absorbing treatment indicators are not observed annual infestation status."
)
data_tables <- tables[1:5]
dictionary <- bind_rows(lapply(names(data_tables), function(name) {
  tab <- data_tables[[name]]
  tibble(dataset = paste0(name, ".csv"), variable = names(tab),
         storage_type = vapply(tab, function(x) class(x)[1], character(1)),
         definition = unname(descriptions[names(tab)]))
}))
assert(!anyNA(dictionary$definition), "Some output variables lack dictionary definitions.")
source_variable_map <- bind_rows(lapply(seq_len(2), function(i) {
  tab <- if (i == 1) bw else cotton
  tibble(source_file = source_names[i], source_variable = names(tab),
         source_label = vapply(tab, function(x) {
           label <- attr(x, "label"); if (is.null(label)) "" else label
         }, character(1)),
         output_variable = case_when(names(tab) == "fipscode" ~ "source_fipscode",
           names(tab) == "censusyear" ~ "county_geography_year",
           i == 2 & names(tab) == "countyname" ~ "cotton_source_countyname",
           i == 2 & names(tab) == "statename" ~ "cotton_source_statename", TRUE ~ names(tab)))
}))
missingness <- bind_rows(lapply(names(data_tables), function(name) {
  tab <- data_tables[[name]]
  tibble(dataset = paste0(name, ".csv"), variable = names(tab), rows = nrow(tab),
         missing = vapply(tab, function(x) sum(is.na(x)), integer(1)))
}))
manifest <- tibble(
  source_file = basename(required_files), absolute_path = normalizePath(required_files, winslash = "/"),
  bytes = as.numeric(file.info(required_files)$size), md5 = unname(input_hashes),
  modified_utc = format(file.info(required_files)$mtime, tz = "UTC", usetz = TRUE),
  role = c("infestation_timing", "baseline_agriculture", "not_used_rosenwald_exposure", "nominal_county_reference"),
  source = c(rep("User-supplied Baker et al. deposited replication file; archive DOI 10.3886/E115906V1", 3),
             "Existing project NHGIS nominal county time-series reference"),
  retrieval_date = NA_character_,
  retrieval_note = "Local input; original download date and archive integrity were not independently verified"
)
tables$boll_weevil_data_dictionary <- dictionary
tables$boll_weevil_source_variable_map <- source_variable_map
tables$boll_weevil_missingness_audit <- missingness
tables$boll_weevil_source_manifest <- manifest
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
for (name in names(tables)) write_csv(tables[[name]], file.path(output_dir, paste0(name, ".csv")), na = "NA")
message("Wrote ", length(tables), " CSVs to ", normalizePath(output_dir, winslash = "/"),
        "\nCounties: ", nrow(counties), "; county-years: ", nrow(panel),
        "; timing eligible: ", sum(counties$eligible_timing),
        "; Southern eligible: ", sum(counties$eligible_baker_2018_rules),
        "; Southern eligible with cotton: ", sum(counties$eligible_baker_2018_rules_with_cotton),
        "\nAll ", nrow(checks), " validation checks passed.")
