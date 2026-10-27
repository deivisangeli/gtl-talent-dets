# Independent read-only calendar selection, aggregation, support and diagnostic checks.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", "..", ".."), winslash = "/", mustWork = TRUE)
for (name in c("amws_training_helpers.R", "amws_catchment_helpers.R", "amws_catchment_no_reuse_helpers.R", "amws_training_calendar_helpers.R")) source(file.path(repo, "prep", "land_grants", name))
cfg <- training_calendar_config(repo, training_root())
for (folder in c(cfg$previous_output, cfg$previous_results, cfg$base_output, cfg$base_results, cfg$no_reuse_output, cfg$no_reuse_results, cfg$output, cfg$results)) invisible(training_check_release(folder))
training_calendar_tests()
read_at <- function(folder, name) {
  path <- file.path(folder, name)
  classes <- if ("GEOID" %in% names(fread(path, nrows = 0L))) c(GEOID = "character") else NULL
  fread(path, colClasses = classes)
}
members <- read_at(cfg$output, "common_analysis_catchment_membership.csv")
training_require(uniqueN(members$GEOID) == 154L && uniqueN(members$site_id) == 43L && uniqueN(members$event_id) == 14L &&
  all(members[, .(events = uniqueN(event_id)), by = GEOID]$events == 1L), "Final calendar membership differs")
excluded <- read_at(cfg$output, "excluded_counties.csv")$GEOID
training_require(setequal(excluded, c("13119","17039","17179","18147","18173","21059","21101","37049","37079","37107","37147")) &&
  !any(members$GEOID %in% excluded) && !50L %in% members$event_id, "Exclusions or East Carolina event removal differ")
geo <- read_at(cfg$base_output, "geographic_catchment_membership.csv")
newgeo <- read_at(cfg$output, "geographic_catchment_membership.csv")
training_require(identical(as.data.frame(newgeo), as.data.frame(geo[!GEOID %in% excluded])), "Geographic assignments changed beyond fixed exclusions")
assignment_check <- merge(members, geo, by = c("event_id", "GEOID"), suffixes = c("", "_original"), all.x = TRUE)
training_require(nrow(assignment_check) == nrow(members) && all(assignment_check$site_id == assignment_check$site_id_original), "County reassigned")
people <- read_at(cfg$output, "entry_classification_and_exclusions.csv")
old_people <- read_at(cfg$base_output, "entry_classification_and_exclusions.csv")
training_require(identical(as.data.frame(people[, names(old_people), with = FALSE]), as.data.frame(old_people)), "Original entry fields changed")
anchor_audit <- read_at(cfg$output, "anchor_calendar_completeness.csv")
member_audit <- read_at(cfg$output, "member_calendar_completeness.csv")
training_require(uniqueN(anchor_audit[included == TRUE, GEOID]) == 69L && all(anchor_audit[included == TRUE, observed_decades] == 13L), "Companion anchor calendar selection differs")
sites <- read_at(cfg$output, "allocation_sites.csv")
stable <- list()
for (timing in c("standard_decade", "alternative_decade")) {
  this_timing <- timing
  initial <- read_at(cfg$output, paste0("companion_reproduced_sample_", timing, ".csv"))
  training_require(nrow(initial) == 598L && uniqueN(initial$event_id) == 15L && uniqueN(initial$site_id) == 46L, "Initial companion reproduction differs")
  ma <- member_audit[timing == this_timing]
  training_require(all(members$GEOID %in% ma[included == TRUE & complete_both == TRUE, GEOID]), "Member lacks both-timing calendar support")
  cp <- read_at(cfg$output, paste0("county_decade_", timing, ".csv"))
  old_cp <- read_at(cfg$base_output, paste0("county_decade_", timing, ".csv"))
  training_require(isTRUE(all.equal(as.data.frame(cp), as.data.frame(old_cp[decade %in% training_calendar_grid & !GEOID %in% excluded]), tolerance = 1e-12)), "County denominator or counts changed")
  pooled <- read_at(cfg$output, paste0("catchment_decade_", timing, ".csv"))
  frozen <- read_at(cfg$output, paste0("frozen_calendar_sample_", timing, ".csv"))
  keys <- c("stack_unit_num", "GEOID", "decade", "g", "event_id", "sample_role", "site_id")
  training_require(nrow(pooled) == 559L && all(pooled[, .N, by = site_id]$N == 13L) &&
    identical(as.data.frame(pooled[, keys, with = FALSE]), as.data.frame(frozen[, keys, with = FALSE])), "Final observation keys or calendar balance differ")
  expected_keys <- initial[event_id != 50L, keys, with = FALSE]
  training_require(identical(as.data.frame(pooled[, keys, with = FALSE]), as.data.frame(expected_keys)), "Events or observations changed beyond declared event drop")
  stable[[timing]] <- pooled[, .(site_id, decade, event_id, sample_role)]
  rows <- merge(members[, .(site_id, GEOID)], cp, by = "GEOID", allow.cartesian = TRUE)
  sums <- rows[, lapply(.SD, sum), by = .(site_id, decade), .SDcols = c(unname(training_counts), "county_births_estimate")]
  check <- merge(pooled, sums, by = c("site_id", "decade"), suffixes = c("", "_sum"))
  for (col in c(unname(training_counts), "county_births_estimate")) training_require(isTRUE(all.equal(check[[col]], check[[paste0(col, "_sum")]], tolerance = 1e-12)), paste("Pooled sum differs:", col))
  catchment_check_partition(pooled)
  for (group in names(training_counts)) training_require(isTRUE(all.equal(pooled[[training_rates[[group]]]], training_rate(pooled[[training_counts[[group]]]], pooled$county_births_estimate), tolerance = 1e-12)), "Pooled rate differs")
  timing_col <- if (timing == "standard_decade") "g_std" else "g_shift"
  baseline <- merge(sites[, .(site_id, baseline_decade = get(timing_col) - 10L)], pooled[, .(site_id, decade, births = county_births_estimate)], by = "site_id", allow.cartesian = TRUE)
  baseline <- baseline[decade == baseline_decade, .(site_id, births)]
  cb <- merge(pooled, baseline, by = "site_id", all.x = TRUE)
  training_require(all(cb$births_baseline == cb$births), "Baseline is not g-10 catchment births")
  entries <- read_at(cfg$output, paste0("analysis_entry_membership_", timing, ".csv"))
  training_require(!anyDuplicated(entries$source_key) && !any(entries$GEOID %in% excluded) &&
    all(entries$decade %in% training_calendar_grid) && all(entries$decade == training_decade(entries$year, timing)), "Entry reuse or binning differs")
  counts <- merge(pooled[, .(site_id, decade, n_amws_training_total)], entries[, .N, by = .(site_id, decade)], by = c("site_id", "decade"), all.x = TRUE)
  counts[is.na(N), N := 0L]
  training_require(all(counts$n_amws_training_total == counts$N), "Counts do not reconstruct from source entries")
}
training_require(identical(as.data.frame(stable[[1]]), as.data.frame(stable[[2]])), "Observation membership differs across timings")
status <- read_at(cfg$results, "model_status.csv")
effects <- read_at(cfg$results, "dynamic_att.csv")
support <- read_at(cfg$results, "event_time_support.csv")
diagnostics <- read_at(cfg$results, "pretrend_test_diagnostics.csv")
membership <- read_at(cfg$results, "estimation_sample_membership.csv")
training_require(nrow(status) == 8L && all(status$ok) && nrow(effects) == 104L && nrow(support) == 104L && nrow(diagnostics) == 8L,
  "Model completion or event-time coverage differs")
training_require(all(effects[event_time == 0, estimate] == 0) && all(effects[event_time == 0, se] == 0), "Normalization differs")
for (j in seq_len(nrow(status))) {
  m <- status[j]
  model <- readRDS(file.path(cfg$results, "models", paste0(m$model, ".rds")))
  pooled <- read_at(cfg$output, paste0("catchment_decade_", m$timing, ".csv"))
  mm <- membership[model == m$model]
  training_require(identical(as.data.frame(mm[, .(stack_unit_num, GEOID, decade, g, event_id, sample_role)]),
    as.data.frame(pooled[, .(stack_unit_num, GEOID, decade, g, event_id, sample_role)])) && all(mm$x == pooled$births_baseline), "Model sample or covariate differs")
  ss <- support[model == m$model]
  training_require(setequal(ss$event_time, seq(-50L,70L,10L)), "Event-time support grid differs")
  for (e in ss$event_time) {
    gs <- unique(model$att$group[model$att$t - model$att$group == e & is.finite(model$att$att)])
    us <- unique(mm[g > 0 & g %in% gs, .(stack_unit_num, event_id)])
    training_require(ss[event_time == e, n_cohorts] == length(gs) && ss[event_time == e, n_treated_sites] == nrow(us) &&
      ss[event_time == e, n_events] == uniqueN(us$event_id) && ss[event_time == e, reference_e0_treated_sites] == 14L, "Native ATT support counts differ")
  }
  d <- diagnostics[model == m$model]
  available <- !is.null(model$att$W) && !is.null(model$att$Wpval)
  training_require(d$joint_test_available == available, "Pre-test availability differs from model object")
  saved <- readRDS(file.path(cfg$results, "models", paste0(m$model, "__pretest_covariance.rds")))
  ii <- saved$coefficients$coefficient_index
  V <- as.matrix(model$att$V_analytical)[ii, ii, drop = FALSE]
  training_require(all(model$att$t[ii] < model$att$group[ii]) && isTRUE(all.equal(V, saved$covariance)) &&
    d$covariance_rank == qr(V, tol = 1e-10)$rank, "Saved pre-test matrix or rank differs")
  if (available) training_require(d$p_value == model$att$Wpval, "Saved Wald p-value differs")
}
decomposition <- merge(effects[group != "total", .(parts = sum(estimate)), by = .(timing, event_time)],
  effects[group == "total", .(timing, event_time, estimate)], by = c("timing", "event_time"))
training_require(nrow(decomposition) == 26L && max(abs(decomposition$parts - decomposition$estimate)) < 1e-8, "Effect decomposition differs")
cat("PASS: immutable previous releases; companion 69/15/46/598 reproduction; eleven frozen exclusions and East Carolina event removal; common 154-county membership; 14 events/43 sites/559 rows per timing; pooled counts/rates/baselines; eight models; native event support; pre-test diagnostics; additive effects.\n")
