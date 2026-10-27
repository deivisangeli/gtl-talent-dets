# Read-only independent validation of exclusion and rebuilt catchment outcomes.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", "..", ".."), winslash = "/", mustWork = TRUE)
for (name in c("amws_training_helpers.R", "amws_catchment_helpers.R", "amws_catchment_no_reuse_helpers.R")) source(file.path(repo, "prep", "land_grants", name))
cfg <- no_reuse_config(repo, training_root())
for (folder in c(cfg$previous_output, cfg$previous_results, cfg$base_output, cfg$base_results, cfg$output, cfg$results)) invisible(training_check_release(folder))
no_reuse_tests()
read_at <- function(folder, name) {
  path <- file.path(folder, name)
  classes <- if ("GEOID" %in% names(fread(path, nrows = 0L))) c(GEOID = "character") else NULL
  fread(path, colClasses = classes)
}
excluded <- read_at(cfg$output, "excluded_counties.csv")$GEOID
training_require(identical(sort(excluded), c("13119", "17039", "17179", "18147", "18173", "21059", "21101")), "Excluded counties differ")
geography <- read_at(cfg$output, "geographic_catchment_membership.csv")
old_geo <- read_at(cfg$base_output, "geographic_catchment_membership.csv")
training_require(uniqueN(geography$GEOID) == 615L && !any(geography$GEOID %in% excluded) &&
  identical(as.data.frame(geography), as.data.frame(old_geo[!GEOID %in% excluded])), "Geographic assignments changed beyond exclusions")
audit <- read_at(cfg$output, "entry_classification_and_exclusions.csv")
old_audit <- read_at(cfg$base_output, "entry_classification_and_exclusions.csv")
training_require(nrow(audit) == 136489L && identical(as.data.frame(audit[, names(old_audit), with = FALSE]), as.data.frame(old_audit)) &&
  all(audit$excluded_for_cross_event_reuse == (audit$GEOID %in% excluded)) &&
  all(audit$sensitivity_geographic_eligible == (audit$exclusion_reason == "eligible" & !audit$excluded_for_cross_event_reuse)), "Entry audit or original classifications changed")
model_members <- read_at(cfg$results, "estimation_sample_membership.csv")
sites <- read_at(cfg$output, "allocation_sites.csv")
training_require(sum(sites$original_home_county_excluded) == 2L, "Home-county exclusion differs")
for (timing in c("standard_decade", "alternative_decade")) {
  all_members <- read_at(cfg$output, paste0("catchment_membership_", timing, ".csv"))
  old_members <- read_at(cfg$base_output, paste0("catchment_membership_", timing, ".csv"))
  training_require(identical(as.data.frame(all_members[, names(old_members), with = FALSE]), as.data.frame(old_members)), "Prior support or assignments changed")
  included <- all_members[included_in_sensitivity == TRUE]
  expected_members <- old_members[supported == TRUE & !GEOID %in% excluded]
  training_require(identical(as.data.frame(included[, names(expected_members), with = FALSE]), as.data.frame(expected_members)), "Exclusion membership differs")
  no_reuse_check_members(included, old_members[supported == TRUE, site_id])
  training_require(!any(included$GEOID %in% excluded), "Shared county remains")
  old_entries <- read_at(cfg$base_output, paste0("analysis_entry_membership_", timing, ".csv"))
  entries <- read_at(cfg$output, paste0("analysis_entry_membership_", timing, ".csv"))
  training_require(!anyDuplicated(entries$source_key) && !any(entries$GEOID %in% excluded) &&
    isTRUE(all.equal(as.data.frame(entries), as.data.frame(old_entries[!GEOID %in% excluded]), tolerance = 1e-12)), "Entry removal or provenance differs")
  county_panel <- read_at(cfg$output, paste0("county_decade_", timing, ".csv"))
  old_county_panel <- read_at(cfg$base_output, paste0("county_decade_", timing, ".csv"))
  training_require(isTRUE(all.equal(as.data.frame(county_panel), as.data.frame(old_county_panel[!GEOID %in% excluded]), tolerance = 1e-12)), "County values changed")
  pooled <- read_at(cfg$output, paste0("catchment_decade_", timing, ".csv"))
  frozen <- read_at(cfg$previous_results, paste0("frozen_sample_", timing, ".csv"))
  ids <- setdiff(names(frozen), "x")
  training_require(identical(as.data.frame(pooled[, ids, with = FALSE]), as.data.frame(frozen[, ids, with = FALSE])), "Frozen sample changed")
  rows <- merge(pooled[, .(site_id, decade)], included[, .(site_id, GEOID)], by = "site_id", allow.cartesian = TRUE)
  rows <- merge(rows, county_panel, by = c("GEOID", "decade"), all.x = TRUE)
  rebuilt <- rows[, lapply(.SD, sum), by = .(site_id, decade), .SDcols = c(unname(training_counts), "county_births_estimate")]
  compared <- merge(pooled, rebuilt, by = c("site_id", "decade"), suffixes = c("", "_rebuilt"))
  for (name in c(unname(training_counts), "county_births_estimate")) training_require(isTRUE(all.equal(compared[[name]], compared[[paste0(name, "_rebuilt")]], tolerance = 1e-12)), paste("Pooled value differs:", name))
  catchment_check_partition(pooled)
  for (group in names(training_rates)) training_require(isTRUE(all.equal(pooled[[training_rates[[group]]]], training_rate(pooled[[training_counts[[group]]]], pooled$county_births_estimate), tolerance = 1e-12)), "Rate differs")
  counts <- merge(pooled[, .(site_id, decade, n_amws_training_total)], entries[, .N, by = .(site_id, decade)], by = c("site_id", "decade"), all.x = TRUE)
  counts[is.na(N), N := 0L]
  training_require(all(counts$N == counts$n_amws_training_total), "Entry-to-count reconstruction differs")
  timing_col <- if (timing == "standard_decade") "g_std" else "g_shift"
  baseline <- merge(sites[, .(site_id, baseline_decade = get(timing_col) - 10L)], pooled[, .(site_id, decade, births = county_births_estimate)], by = "site_id", allow.cartesian = TRUE)
  baseline <- baseline[decade == baseline_decade, .(site_id, births)]
  cb <- merge(pooled, baseline, by = "site_id", all.x = TRUE)
  training_require(all(cb$births_baseline == cb$births), "Sensitivity baseline differs")
}
status <- read_at(cfg$results, "model_status.csv")
training_require(nrow(status) == 8L && all(status$ok), "An estimation failed; inspect model_status.csv")
for (j in seq_len(nrow(status))) {
  m <- status[j]
  pooled <- read_at(cfg$output, paste0("catchment_decade_", m$timing, ".csv"))
  mm <- model_members[model == m$model]
  training_require(identical(as.data.frame(mm[, .(stack_unit_num, GEOID, decade, g, event_id, sample_role)]),
    as.data.frame(pooled[, .(stack_unit_num, GEOID, decade, g, event_id, sample_role)])) && all(mm$x == pooled$births_baseline), "Estimation membership differs")
}
effects <- read_at(cfg$results, "dynamic_att.csv")
training_require(nrow(effects) == 80L && all(effects[event_time == 0, estimate] == 0), "Dynamic support/reference differs")
check <- merge(effects[group != "total", .(components = sum(estimate)), by = .(timing, event_time)], effects[group == "total", .(timing, event_time, estimate)], by = c("timing", "event_time"))
training_require(nrow(check) == 20L && max(abs(check$components - check$estimate)) < 1e-8, "Decomposition fails")
cat("PASS: immutable prior releases; seven counties excluded everywhere (including two home counties); no estimation reuse; unchanged classifications and source rows; rebuilt denominators, rates, baselines; exact frozen samples; eight models; additive effects.\n")
