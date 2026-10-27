# Reproduce Appendix A calendar selection, then build common no-reuse catchments.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
helpers <- file.path(dirname(script), c("amws_training_helpers.R", "amws_catchment_helpers.R", "amws_catchment_no_reuse_helpers.R", "amws_training_calendar_helpers.R"))
for (name in helpers) source(name)
suppressPackageStartupMessages({library(dplyr); library(readr)})
cfg <- training_calendar_config(repo, training_root())
if (dir.exists(cfg$output)) {
  invisible(training_check_release(cfg$output)); cat("Calendar preparation already complete and verified.\n"); quit(status = 0)
}
started <- proc.time()[3]
for (folder in c(cfg$base_output, cfg$base_results, cfg$no_reuse_output, cfg$no_reuse_results)) invisible(training_check_release(folder))
training_calendar_tests()
stage <- file.path(cfg$local, "prep_stage")
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
timings <- names(cfg$panels)
base_files <- c("completion_manifest.json", "allocation_sites.csv", "geographic_catchment_membership.csv", "county_centroids.csv",
  "entry_classification_and_exclusions.csv", "county_year_training_and_denominators.csv", "institution_coordinate_lookup.csv",
  paste0("county_decade_", timings, ".csv"))
companion_files <- c("calendar_sample_audit.csv", "event_sample_audit.csv", "baseline_pop_model_status.csv", "event_time_support.csv")
source_files <- c(script, helpers, cfg$sample_helper, file.path(repo, "prep", "raw_paths.R"), cfg$units, cfg$panels,
  file.path(cfg$base_output, base_files), file.path(cfg$no_reuse_output, c("completion_manifest.json", "excluded_counties.csv")),
  file.path(cfg$companion, companion_files), file.path(repo, "analysis", "report_companion_calendar_appendices.R"))
inputs <- training_hashes(unique(unname(source_files)))
Sys.setenv(DET_DIR = cfg$root)
source(file.path(repo, "prep", "raw_paths.R")); source(cfg$sample_helper)
old <- read_temporal_support_inputs()
panels_old <- list(standard_decade = old$panel, alternative_decade = old$panel_alt)
anchor_audits <- lapply(panels_old, training_calendar_audit, outcomes = "amws_per_1000_births")
anchors <- Reduce(intersect, lapply(anchor_audits, function(a) a[included == TRUE, GEOID]))
training_require(length(anchors) == 69L, "Companion calendar-complete anchor count differs")
saved_anchor <- fread(file.path(cfg$companion, "calendar_sample_audit.csv"), colClasses = c(GEOID = "character"))
saved_events <- fread(file.path(cfg$companion, "event_sample_audit.csv"), colClasses = c(GEOID = "character"))
reproduced <- list(); baseline_site_sets <- list()
for (timing in timings) {
  this_timing <- timing
  training_require(setequal(anchors, saved_anchor[timing == this_timing & retained_in_both_timings == TRUE, GEOID]), "Saved companion anchor selection differs")
  p <- panels_old[[timing]] %>% filter(GEOID %in% anchors, decade %in% training_calendar_grid)
  stack <- build_stack(old$units, p, if (timing == "standard_decade") "g_std" else "g_shift", timing)
  sample <- prepare_event_study_sample(stack, "amws_per_1000_births", "births_baseline")
  training_require(nrow(sample) == 598L && n_distinct(sample$stack_unit_num) == 46L && n_distinct(sample$event_id) == 15L, "Companion initial sample differs")
  sample <- as.data.table(sample)
  sample[, site_id := paste(event_id, GEOID, sample_role, sep = "_")]
  wanted <- saved_events[timing == this_timing & calendar_county_retained == TRUE & event_retained == TRUE,
    paste(event_id, GEOID, sample_role, sep = "_")]
  training_require(setequal(sample$site_id, wanted), "Saved companion event/site membership differs")
  reproduced[[timing]] <- sample
  baseline_site_sets[[timing]] <- sort(unique(sample$site_id))
  anchor_audits[[timing]][, timing := timing]
  fwrite(sample, file.path(stage, paste0("companion_reproduced_sample_", timing, ".csv")))
}
training_require(identical(baseline_site_sets[[1]], baseline_site_sets[[2]]), "Companion site sets differ across timings")
fwrite(rbindlist(anchor_audits), file.path(stage, "anchor_calendar_completeness.csv"))
read_base <- function(name) fread(file.path(cfg$base_output, name), colClasses = c(GEOID = "character"))
sites <- read_base("allocation_sites.csv")
geography <- read_base("geographic_catchment_membership.csv")
county_panels <- setNames(lapply(timings, function(t) read_base(paste0("county_decade_", t, ".csv"))), timings)
member_audits <- lapply(county_panels, training_calendar_audit, outcomes = unname(training_counts))
common_complete <- Reduce(intersect, lapply(member_audits, function(a) a[included == TRUE, GEOID]))
prior_excluded <- fread(file.path(cfg$no_reuse_output, "excluded_counties.csv"), colClasses = c(GEOID = "character"))$GEOID
candidate <- geography[site_id %in% baseline_site_sets[[1]] & GEOID %in% common_complete & !GEOID %in% prior_excluded]
new_excluded <- candidate[, .(events = uniqueN(event_id)), by = GEOID][events > 1L, GEOID]
training_require(setequal(new_excluded, c("37049", "37079", "37107", "37147")), "Additional shared county inventory differs")
excluded <- sort(union(prior_excluded, new_excluded))
members <- candidate[!GEOID %in% excluded]
retained_sites <- training_calendar_events(members, sites)
baseline_events <- unique(reproduced[[1]]$event_id)
dropped_events <- setdiff(baseline_events, retained_sites$event_id)
training_require(setequal(dropped_events, 50L), "Expected only East Carolina event to become unsupported")
members <- members[site_id %in% retained_sites$site_id]
no_reuse_check_members(members, retained_sites$site_id)
training_require(nrow(retained_sites) == 43L && uniqueN(retained_sites$event_id) == 14L && uniqueN(members$GEOID) == 154L, "Final calendar preflight dimensions differ; estimation blocked")
membership_audit <- copy(geography)
membership_audit[, `:=`(initial_companion_site = site_id %in% baseline_site_sets[[1]],
  calendar_complete_both = GEOID %in% common_complete, excluded_shared_county = GEOID %in% excluded,
  event_retained = event_id %in% retained_sites$event_id, site_retained = site_id %in% retained_sites$site_id)]
membership_audit[, included := initial_companion_site & calendar_complete_both & !excluded_shared_county & site_retained]
membership_audit[, reason := fcase(excluded_shared_county, "frozen_shared_county_exclusion", !initial_companion_site, "outside_companion_initial_site_sample",
  !calendar_complete_both, "incomplete_calendar_denominator_or_outcome", !event_retained, "event_without_surviving_treated_and_control", default = "included_fixed_common_catchment")]
fwrite(membership_audit, file.path(stage, "catchment_membership_audit.csv"))
fwrite(members, file.path(stage, "common_analysis_catchment_membership.csv"))
fwrite(geography[!GEOID %in% excluded], file.path(stage, "geographic_catchment_membership.csv"))
for (timing in timings) {
  member_audits[[timing]][, `:=`(timing = timing, complete_both = GEOID %in% common_complete)]
}
fwrite(rbindlist(member_audits), file.path(stage, "member_calendar_completeness.csv"))
centroids <- read_base("county_centroids.csv")
excluded_info <- centroids[GEOID %in% excluded]
excluded_info[, exclusion_source := fifelse(GEOID %in% prior_excluded, "previous_seven_exclusions", "additional_calendar_sample_reuse")]
fwrite(excluded_info, file.path(stage, "excluded_counties.csv"))
fwrite(centroids[!GEOID %in% excluded], file.path(stage, "county_centroids.csv"))
fwrite(sites, file.path(stage, "allocation_sites.csv"))
event_audit <- sites[event_id %in% baseline_events]
event_audit[, `:=`(retained = event_id %in% retained_sites$event_id,
  reason = fifelse(event_id %in% dropped_events, "East_Carolina_treated_catchment_empty_after_frozen_exclusions", "retained_with_own_runner_up"))]
fwrite(event_audit, file.path(stage, "event_selection_and_drop_audit.csv"))
entry_audit <- read_base("entry_classification_and_exclusions.csv")
entry_audit[, `:=`(excluded_shared_county = GEOID %in% excluded,
  calendar_analysis_county = GEOID %in% members$GEOID)]
fwrite(entry_audit, file.path(stage, "entry_classification_and_exclusions.csv"))
eligible <- entry_audit[exclusion_reason == "eligible"]
annual <- read_base("county_year_training_and_denominators.csv")[!GEOID %in% excluded]
fwrite(annual, file.path(stage, "county_year_training_and_denominators.csv"))
coverage <- list(); matching <- list(); quality <- list(); annual_bin_audit <- list()
for (timing in timings) {
  initial <- reproduced[[timing]]
  frozen <- initial[site_id %in% retained_sites$site_id]
  frozen[, y := NULL]; setnames(frozen, "x", "county_only_births_baseline")
  fwrite(frozen, file.path(stage, paste0("frozen_calendar_sample_", timing, ".csv")))
  county_panel <- county_panels[[timing]][decade %in% training_calendar_grid & !GEOID %in% excluded]
  fwrite(county_panel, file.path(stage, paste0("county_decade_", timing, ".csv")))
  joined <- merge(members[, .(site_id, GEOID)], county_panel, by = "GEOID", allow.cartesian = TRUE)
  training_require(all(is.finite(joined$county_births_estimate) & joined$county_births_estimate > 0), "Incomplete included denominator")
  pooled <- joined[, c(lapply(.SD, sum), list(population = sum(population), county_births_estimate = sum(county_births_estimate),
    member_count = .N, annual_years = sum(annual_years), annual_years_with_births = sum(annual_years_with_births))),
    by = .(site_id, decade), .SDcols = unname(training_counts)]
  for (group in names(training_counts)) pooled[, (training_rates[[group]]) := training_rate(get(training_counts[[group]]), county_births_estimate)]
  catchment_check_partition(pooled)
  pooled <- merge(frozen, pooled, by = c("site_id", "decade"), all.x = TRUE)
  timing_col <- if (timing == "standard_decade") "g_std" else "g_shift"
  meta <- sites[, .(site_id, treatment_decade = get(timing_col))]
  baseline <- merge(meta, pooled[, .(site_id, decade, county_births_estimate)], by = "site_id", allow.cartesian = TRUE)
  baseline <- baseline[decade == treatment_decade - 10L, .(site_id, births_baseline = county_births_estimate)]
  pooled <- merge(pooled, baseline, by = "site_id", all.x = TRUE)
  pooled[, unit_geography := "fixed_calendar_complete_site_catchment"]
  setorder(pooled, event_id, sample_role, GEOID, decade)
  training_require(nrow(pooled) == 559L && all(pooled[, .N, by = site_id]$N == 13L) &&
    all(is.finite(pooled$births_baseline) & pooled$births_baseline > 0), "Final calendar observations or baseline differ")
  fwrite(pooled, file.path(stage, paste0("catchment_decade_", timing, ".csv")))
  fwrite(members, file.path(stage, paste0("catchment_membership_", timing, ".csv")))
  entries_all <- merge(eligible, members[, .(site_id, event_id, sample_role, GEOID, home)], by = "GEOID", allow.cartesian = TRUE)
  entries_all[, decade := training_decade(year, timing)]
  annual_bin_audit[[timing]] <- entries_all[, .(timing = timing, entries = .N), by = .(year, decade, retained = decade %in% training_calendar_grid)]
  entries <- entries_all[decade %in% training_calendar_grid]
  training_require(!anyDuplicated(entries$source_key), "Entry reused across calendar experiments")
  fwrite(entries, file.path(stage, paste0("analysis_entry_membership_", timing, ".csv")))
  ec <- merge(pooled[, .(site_id, decade, n_amws_training_total)], entries[, .N, by = .(site_id, decade)], by = c("site_id", "decade"), all.x = TRUE)
  ec[is.na(N), N := 0L]
  training_require(all(ec$n_amws_training_total == ec$N), "Entry-to-count reconstruction differs")
  record <- merge(entries, meta, by = "site_id", all.x = TRUE)
  matching[[timing]] <- record[, .(timing = timing, N = .N, entries_with_unmatched_awards = sum(n_unmatched_awards > 0L),
    unmatched_awards = sum(n_unmatched_awards), partial_awards = sum(n_partial_awards)),
    by = .(edition, cohort = treatment_decade, sample_role, home, training_class, unknown_reason)]
  quality[[timing]] <- pooled[, .(timing = timing, site_id, decade, annual_years, annual_years_with_births)]
  coverage[[timing]] <- data.table(timing = timing, events = uniqueN(pooled$event_id), sites = uniqueN(pooled$site_id), rows = nrow(pooled),
    analysis_counties = uniqueN(members$GEOID), entries_unique = uniqueN(entries$source_key),
    ever_nearby = entries[training_class == "ever_nearby", .N], never_nearby = entries[training_class == "never_nearby", .N],
    unknown = entries[training_class == "unknown", .N], cross_event_reused_counties = 0L,
    initial_companion_events = 15L, initial_companion_sites = 46L, initial_companion_observations = 598L)
  # Annual pooled counts retain all 1830-1959 source years; the bin audit explains exclusions from displayed calendar labels.
  ay <- merge(members[, .(site_id, GEOID)], annual, by = "GEOID", allow.cartesian = TRUE)
  ay <- ay[, c(lapply(.SD, sum), list(estimated_births = if (all(is.finite(county_births_estimate_year))) sum(county_births_estimate_year) else NA_real_,
    members_with_births = sum(is.finite(county_births_estimate_year)))), by = .(site_id, year), .SDcols = unname(training_counts)]
  fwrite(ay, file.path(stage, paste0("catchment_year_", timing, ".csv")))
}
fwrite(rbindlist(coverage), file.path(stage, "analysis_coverage_summary.csv"))
fwrite(rbindlist(matching), file.path(stage, "matching_coverage_by_edition_cohort_role.csv"))
fwrite(rbindlist(quality), file.path(stage, "annual_denominator_completeness.csv"))
fwrite(rbindlist(annual_bin_audit), file.path(stage, "birth_year_binning_audit.csv"))
validation <- list(passed = TRUE, reproduced_companion_anchor_count = 69L, reproduced_companion_events_units_rows = c(15L,46L,598L),
  final_events_units_rows_counties = c(14L,43L,559L,154L), excluded_counties = excluded, dropped_event = 50L,
  common_fixed_membership_both_timings = TRUE, calendar_decades_per_site = 13L, event_time_balance_filter_applied = FALSE,
  no_county_or_entry_reuse_in_estimation = TRUE, classifications_and_county_values_unchanged = TRUE,
  pooled_counts_rates_baselines_validated = TRUE, historical_campus_accuracy_verified = FALSE)
training_json(validation, file.path(stage, "validation_report.json"))
writeLines(c("Calendar-only balance, following report companion Appendix A. Initial 69 eligible anchor counties reproduce 15 events/46 sites/598 observations.",
  "Common member counties must have usable counts and positive births in all 13 decade labels 1830-1950 under both timings.",
  "Existing seven exclusions plus Craven, Greene, Lenoir and Pitt NC frozen before dropping event 50 (East Carolina University). No reassignment or restoration.",
  "Final 14 events/43 site units/559 observations/154 physical member counties per timing. No relative-time balance filter.",
  "Source birth years remain 1830-1959; alternative 1960 bin excluded. Inherited partial-annual denominator sums preserved and audited.",
  "Pooled rates are 1000*sum(counts)/sum(births); baseline control is summed births at g-10. GEOID in pooled tables is an anchor label.",
  "Training classifications and institution qualifications inherited unchanged; proximity is relative to birth-county centroid.",
  "OpenAlex reference locations do not verify historical campuses. Existing releases and Drive companion remain unchanged."), file.path(stage, "DATA_DICTIONARY.txt"))
dir.create(file.path(stage, "code_snapshot"), showWarnings = FALSE)
for (p in c(script, helpers, cfg$sample_helper)) file.copy(p, file.path(stage, "code_snapshot", basename(p)), overwrite = TRUE)
training_json(list(elapsed_seconds = unname(proc.time()[3] - started)), file.path(stage, "timing_report.json"))
capture.output(sessionInfo(), file = file.path(stage, "sessionInfo.txt"))
training_publish(stage, cfg$output, inputs, list(status = "complete", validation = validation))
cat("Published calendar preparation:", cfg$output, "\n"); print(rbindlist(coverage))
