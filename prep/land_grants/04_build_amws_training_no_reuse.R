# Sensitivity release: remove shared counties everywhere, without rematching or reassigning.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
for (name in c("amws_training_helpers.R", "amws_catchment_helpers.R", "amws_catchment_no_reuse_helpers.R")) source(file.path(dirname(script), name))
cfg <- no_reuse_config(repo, training_root())
if (dir.exists(cfg$output)) {
  invisible(training_check_release(cfg$output)); cat("No-reuse preparation already complete and verified.\n"); quit(status = 0)
}
started <- proc.time()[3]
for (folder in c(cfg$previous_output, cfg$previous_results, cfg$base_output, cfg$base_results)) invisible(training_check_release(folder))
no_reuse_tests()
stage <- file.path(cfg$local, "prep_stage")
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
timings <- c("standard_decade", "alternative_decade")
files <- c("completion_manifest.json", "allocation_sites.csv", "geographic_catchment_membership.csv", "county_centroids.csv",
  "institution_coordinate_lookup.csv", "entry_classification_and_exclusions.csv", "county_year_training_and_denominators.csv",
  paste0("catchment_membership_", timings, ".csv"), paste0("county_decade_", timings, ".csv"),
  paste0("catchment_decade_", timings, ".csv"), paste0("analysis_entry_membership_", timings, ".csv"))
sources <- c(file.path(cfg$base_output, files), file.path(cfg$base_results, "completion_manifest.json"),
  file.path(cfg$previous_results, paste0("frozen_sample_", timings, ".csv")), script,
  file.path(dirname(script), c("amws_training_helpers.R", "amws_catchment_helpers.R", "amws_catchment_no_reuse_helpers.R")))
inputs <- training_hashes(sources)
read_base <- function(name) fread(file.path(cfg$base_output, name), colClasses = c(GEOID = "character"))
member_tables <- setNames(lapply(timings, function(t) read_base(paste0("catchment_membership_", t, ".csv"))), timings)
excluded_by_timing <- lapply(member_tables, function(m) m[supported == TRUE, .(events = uniqueN(event_id)), by = GEOID][events > 1L, GEOID])
excluded <- sort(unique(unlist(excluded_by_timing)))
expected <- c("13119", "17039", "17179", "18147", "18173", "21059", "21101")
training_require(identical(excluded, expected) && all(vapply(excluded_by_timing, function(x) setequal(x, expected), logical(1))), "Shared county inventory differs")
centroids <- read_base("county_centroids.csv")
county_exclusions <- centroids[GEOID %in% excluded]
county_exclusions[, state := c("13" = "Georgia", "17" = "Illinois", "18" = "Indiana", "21" = "Kentucky")[substr(GEOID, 1L, 2L)]]
county_exclusions[, rule := "Excluded from every catchment, including original home memberships; no reassignment"]
setorder(county_exclusions, GEOID)
fwrite(county_exclusions, file.path(stage, "excluded_counties.csv"))
geography <- read_base("geographic_catchment_membership.csv")
fwrite(geography[GEOID %in% excluded], file.path(stage, "removed_geographic_memberships.csv"))
geography <- geography[!GEOID %in% excluded]
fwrite(geography, file.path(stage, "geographic_catchment_membership.csv"))
fwrite(centroids[!GEOID %in% excluded], file.path(stage, "county_centroids.csv"))
sites <- read_base("allocation_sites.csv")
sites[, original_home_county_excluded := GEOID %in% excluded]
fwrite(sites, file.path(stage, "allocation_sites.csv"))
training_require(sum(sites$original_home_county_excluded) == 2L, "Original-county exclusion audit differs")
file.copy(file.path(cfg$base_output, "institution_coordinate_lookup.csv"), file.path(stage, "institution_coordinate_lookup.csv"), overwrite = TRUE)
audit <- read_base("entry_classification_and_exclusions.csv")
audit[, `:=`(excluded_for_cross_event_reuse = GEOID %in% excluded,
  sensitivity_geographic_eligible = exclusion_reason == "eligible" & !GEOID %in% excluded)]
fwrite(audit, file.path(stage, "entry_classification_and_exclusions.csv"))
annual <- read_base("county_year_training_and_denominators.csv")[!GEOID %in% excluded]
fwrite(annual, file.path(stage, "county_year_training_and_denominators.csv"))
coverage <- list(); matching <- list(); denominator_quality <- list(); removal_coverage <- list()
for (timing in timings) {
  members <- member_tables[[timing]]
  old_members <- members[supported == TRUE]
  members[, excluded_for_cross_event_reuse := GEOID %in% excluded]
  members[, included_in_sensitivity := supported & !excluded_for_cross_event_reuse]
  members[, sensitivity_reason := fcase(excluded_for_cross_event_reuse, "excluded_shared_county",
    !supported, "excluded_missing_frozen_denominator", default = "included_unchanged_assignment")]
  included <- no_reuse_filter(members, excluded)
  no_reuse_check_members(included, old_members$site_id)
  fwrite(members, file.path(stage, paste0("catchment_membership_", timing, ".csv")))
  frozen <- fread(file.path(cfg$previous_results, paste0("frozen_sample_", timing, ".csv")), colClasses = c(GEOID = "character"))
  old_panel <- read_base(paste0("catchment_decade_", timing, ".csv"))
  keys <- old_panel[, .(site_id, stack_unit_num, GEOID, decade, g, event_id, sample_role, county_only_births_baseline)]
  training_require(identical(as.data.frame(keys[, setdiff(names(frozen), "x"), with = FALSE]), as.data.frame(frozen[, !"x"])), "Input sample differs")
  counties <- read_base(paste0("county_decade_", timing, ".csv"))[!GEOID %in% excluded]
  fwrite(counties, file.path(stage, paste0("county_decade_", timing, ".csv")))
  rows <- merge(keys[, .(site_id, decade)], included[, .(site_id, GEOID)], by = "site_id", allow.cartesian = TRUE)
  rows <- merge(rows, counties, by = c("GEOID", "decade"), all.x = TRUE)
  training_require(all(is.finite(rows$county_births_estimate) & rows$county_births_estimate > 0), "Missing denominator after exclusion")
  pooled <- rows[, c(lapply(.SD, sum), list(population = sum(population), county_births_estimate = sum(county_births_estimate),
    member_count = .N, annual_years = sum(annual_years), annual_years_with_births = sum(annual_years_with_births))),
    by = .(site_id, decade), .SDcols = unname(training_counts)]
  for (group in names(training_counts)) pooled[, (training_rates[[group]]) := training_rate(get(training_counts[[group]]), county_births_estimate)]
  catchment_check_partition(pooled)
  pooled <- merge(keys, pooled, by = c("site_id", "decade"), all.x = TRUE)
  timing_col <- if (timing == "standard_decade") "g_std" else "g_shift"
  meta <- sites[, .(site_id, treatment_decade = get(timing_col))]
  baseline <- merge(meta, pooled[, .(site_id, decade, county_births_estimate)], by = "site_id", allow.cartesian = TRUE)
  baseline <- baseline[decade == treatment_decade - 10L, .(site_id, births_baseline = county_births_estimate)]
  training_require(!anyDuplicated(baseline$site_id), "Duplicate sensitivity baseline")
  pooled <- merge(pooled, baseline, by = "site_id", all.x = TRUE)
  pooled[, unit_geography := "fixed_site_catchment_excluding_shared_counties"]
  setorder(pooled, event_id, sample_role, GEOID, decade)
  ids <- setdiff(names(frozen), "x")
  training_require(identical(as.data.frame(pooled[, ids, with = FALSE]), as.data.frame(frozen[, ids, with = FALSE])) &&
    all(is.finite(pooled$births_baseline) & pooled$births_baseline > 0), "Frozen sample cannot be retained")
  fwrite(pooled, file.path(stage, paste0("catchment_decade_", timing, ".csv")))
  ay <- merge(included[, .(site_id, GEOID)], annual, by = "GEOID", allow.cartesian = TRUE)
  ay <- ay[, c(lapply(.SD, sum), list(population = if (all(is.finite(population))) sum(population) else NA_real_,
    estimated_births = if (all(is.finite(county_births_estimate_year))) sum(county_births_estimate_year) else NA_real_,
    member_count = .N, members_with_births = sum(is.finite(county_births_estimate_year)))), by = .(site_id, year), .SDcols = unname(training_counts)]
  fwrite(ay, file.path(stage, paste0("catchment_year_", timing, ".csv")))
  old_entries <- read_base(paste0("analysis_entry_membership_", timing, ".csv"))
  entries <- old_entries[!GEOID %in% excluded]
  training_require(!anyDuplicated(entries$source_key), "Entry still contributes to different experiments")
  fwrite(entries, file.path(stage, paste0("analysis_entry_membership_", timing, ".csv")))
  joined <- merge(pooled[, .(site_id, decade, n_amws_training_total)], entries[, .N, by = .(site_id, decade)], by = c("site_id", "decade"), all.x = TRUE)
  joined[is.na(N), N := 0L]
  training_require(all(joined$n_amws_training_total == joined$N), "Sensitivity total does not reconstruct from entries")
  coverage[[timing]] <- data.table(timing = timing, events = uniqueN(pooled$event_id), sites = uniqueN(pooled$site_id), rows = nrow(pooled),
    analysis_counties = uniqueN(included$GEOID), previous_analysis_counties = uniqueN(old_members$GEOID), excluded_counties = length(excluded),
    excluded_memberships = nrow(old_members) - nrow(included), retained_home_counties = uniqueN(included[home == TRUE, GEOID]),
    entries_unique = uniqueN(entries$source_key), previous_entries_unique = uniqueN(old_entries$source_key),
    removed_entries_unique = uniqueN(old_entries$source_key) - uniqueN(entries$source_key), stacked_entries = nrow(entries),
    cross_event_reused_counties = included[, .(events = uniqueN(event_id)), by = GEOID][events > 1L, .N])
  removal_coverage[[timing]] <- old_entries[GEOID %in% excluded, .(timing = timing, stacked_entries = .N, distinct_entries = uniqueN(source_key)),
    by = .(GEOID, edition, sample_role, training_class)]
  record <- merge(entries, meta, by = "site_id", all.x = TRUE)
  matching[[timing]] <- record[, .(timing = timing, N = .N, entries_with_unmatched_awards = sum(n_unmatched_awards > 0L),
    unmatched_awards = sum(n_unmatched_awards), partial_awards = sum(n_partial_awards)),
    by = .(edition, cohort = treatment_decade, sample_role, home, training_class, unknown_reason)]
  denominator_quality[[timing]] <- pooled[, .(timing = timing, site_id, decade, annual_years, annual_years_with_births)]
}
fwrite(rbindlist(coverage), file.path(stage, "analysis_coverage_summary.csv"))
fwrite(rbindlist(matching), file.path(stage, "matching_coverage_by_edition_cohort_role.csv"))
fwrite(rbindlist(denominator_quality), file.path(stage, "annual_denominator_completeness.csv"))
fwrite(rbindlist(removal_coverage), file.path(stage, "removed_entry_coverage.csv"))
validation <- list(passed = TRUE, excluded_counties = excluded, excluded_from_all_catchments_including_original_counties = TRUE,
  no_reassignment_or_denominator_refiltering = TRUE, no_cross_event_county_or_entry_reuse_in_either_sample = TRUE,
  all_original_site_and_observation_identifiers_preserved = TRUE, training_classifications_unchanged = TRUE,
  counts_denominators_rates_and_baselines_rebuilt = TRUE, historical_campus_accuracy_verified = FALSE)
training_json(validation, file.path(stage, "validation_report.json"))
writeLines(c("Sensitivity: exclude the seven counties reused in the published estimation catchments from every catchment, including home memberships.",
  "Exclusions: Franklin GA; De Witt and Tazewell IL; Spencer and Warrick IN; Daviess and Henderson KY.",
  "No counties reassigned, no support rules relaxed, no new institutions or classifications. All frozen sites and observations retained.",
  "Warrick and Daviess original home counties are excluded; their original site IDs remain labels for surviving surrounding-county catchments.",
  "Member and birth-county GEOIDs exclude the seven counties. Anchor GEOIDs in site and pooled identifiers can still name an excluded home county.",
  "Entry audit preserves original eligibility/classification fields and adds excluded_for_cross_event_reuse and sensitivity_geographic_eligible.",
  "County denominators and training classifications are inherited unchanged; site counts, rates and g-10 birth controls are recomputed.",
  "No reuse remains in either estimation sample. The full 57-event geographic pool can still have other overlap outside these samples.",
  "Legacy annual missingness and binning conventions persist; OpenAlex reference locations do not establish historical campuses."), file.path(stage, "DATA_DICTIONARY.txt"))
dir.create(file.path(stage, "code_snapshot"), showWarnings = FALSE)
for (p in c(script, file.path(dirname(script), c("amws_training_helpers.R", "amws_catchment_helpers.R", "amws_catchment_no_reuse_helpers.R"))))
  file.copy(p, file.path(stage, "code_snapshot", basename(p)), overwrite = TRUE)
training_json(list(elapsed_seconds = unname(proc.time()[3] - started)), file.path(stage, "timing_report.json"))
capture.output(sessionInfo(), file = file.path(stage, "sessionInfo.txt"))
invisible(training_check_release(cfg$base_output)); invisible(training_check_release(cfg$base_results))
training_publish(stage, cfg$output, inputs, list(status = "complete", validation = validation))
cat("Published no-reuse preparation:", cfg$output, "\n")
print(rbindlist(coverage))
