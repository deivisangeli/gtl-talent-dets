# Independently reconstruct geographic choices, fixed support, pooled outcomes and samples.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo, "prep", "land_grants", "amws_training_helpers.R"))
source(file.path(repo, "prep", "land_grants", "amws_catchment_helpers.R"))
cfg <- catchment_config(repo, training_root())
for (folder in c(cfg$previous_output, cfg$previous_results, cfg$output, cfg$results)) invisible(training_check_release(folder))
catchment_tests()
read_prep <- function(name) fread(file.path(cfg$output, name), colClasses = c(GEOID = "character"))
assigned <- read_prep("geographic_catchment_membership.csv")
candidates <- read_prep("catchment_candidates_and_decisions.csv")
reconstructed <- catchment_assign(candidates[, .(event_id, site_id, sample_role, anchor_GEOID, GEOID, distance_m, home)])
training_require(!anyDuplicated(assigned[, .(event_id, GEOID)]) && uniqueN(assigned$GEOID) == 622L &&
                   nrow(assigned[home == TRUE]) == 168L && identical(as.data.frame(reconstructed), as.data.frame(assigned)), "Geographic assignment differs")
training_require(all(assigned$home | assigned$distance_m <= training_radius_m), "Added county outside radius")
people <- read_prep("entry_classification_and_exclusions.csv")
training_require(nrow(people) == 136489L && !anyDuplicated(people$source_key), "Source audit coverage differs")
original <- fread(file.path(cfg$previous_output, "entry_classification_and_exclusions.csv"), colClasses = c(GEOID = "character"))
unchanged <- merge(original[exclusion_reason == "eligible", .(source_key, old_class = training_class)],
                   people[, .(source_key, training_class)], by = "source_key", all.x = TRUE)
training_require(nrow(unchanged) == 12360L && all(unchanged$old_class == unchanged$training_class), "Original classification changed")
for (timing in c("standard_decade", "alternative_decade")) {
  members <- read_prep(paste0("catchment_membership_", timing, ".csv"))
  support <- read_prep(paste0("denominator_support_rows_", timing, ".csv"))
  reconstructed_support <- catchment_supported(support)
  comparison <- merge(members, reconstructed_support, by = c("site_id", "GEOID", "home"), suffixes = c("", "_reconstructed"))
  training_require(nrow(comparison) == nrow(members) && all(comparison$supported == comparison$supported_reconstructed) &&
                     all(members[home == TRUE, supported]), "Fixed denominator support differs")
  included <- members[supported == TRUE]
  panel <- read_prep(paste0("catchment_decade_", timing, ".csv"))
  counties <- read_prep(paste0("county_decade_", timing, ".csv"))
  frozen <- fread(file.path(cfg$previous_results, paste0("frozen_sample_", timing, ".csv")), colClasses = c(GEOID = "character"))
  ids <- setdiff(names(frozen), "x")
  training_require(identical(panel[, ids, with = FALSE], frozen[, ids, with = FALSE]), "Frozen rows, events or unit IDs changed")
  training_require(!anyDuplicated(panel[, .(site_id, decade)]), "Duplicate pooled observation")
  rows <- merge(panel[, .(site_id, decade)], included[, .(site_id, GEOID)], by = "site_id", allow.cartesian = TRUE)
  rows <- merge(rows, counties, by = c("GEOID", "decade"), all.x = TRUE)
  rebuilt <- rows[, lapply(.SD, sum), by = .(site_id, decade), .SDcols = c(unname(training_counts), "county_births_estimate")]
  check <- merge(panel, rebuilt, by = c("site_id", "decade"), suffixes = c("", "_rebuilt"))
  for (name in c(unname(training_counts), "county_births_estimate")) training_require(isTRUE(all.equal(check[[name]], check[[paste0(name, "_rebuilt")]], tolerance = 1e-12)),
                                                                                  paste("Pooled reconstruction differs:", name))
  catchment_check_partition(panel)
  for (group in names(training_counts)) training_require(isTRUE(all.equal(panel[[training_rates[[group]]]], training_rate(panel[[training_counts[[group]]]], panel$county_births_estimate), tolerance = 1e-12)), "Pooled rate differs")
  sites <- read_prep("allocation_sites.csv")
  timing_col <- if (timing == "standard_decade") "g_std" else "g_shift"
  baseline <- merge(sites[, .(site_id, baseline_decade = get(timing_col) - 10L)], panel[, .(site_id, decade, births = county_births_estimate)], by = "site_id", allow.cartesian = TRUE)
  baseline <- baseline[baseline_decade == decade, .(site_id, births)]
  check_baseline <- merge(panel, baseline, by = "site_id", all.x = TRUE)
  training_require(all(check_baseline$births_baseline == check_baseline$births), "Pooled baseline is not g-10 births")
  entries <- read_prep(paste0("analysis_entry_membership_", timing, ".csv"))
  training_require(!anyDuplicated(entries[, .(event_id, source_key)]), "Scientist duplicated within event")
  counts <- entries[, .N, by = .(site_id, decade)]
  check_counts <- merge(panel, counts, by = c("site_id", "decade"), all.x = TRUE)
  check_counts[is.na(N), N := 0L]
  training_require(all(check_counts$n_amws_training_total == check_counts$N), "Pooled total does not reconstruct from entries")
  # Membership does not depend on the presence of scientists in any particular period.
  nr <- rows[, .N, by = .(site_id, decade)]
  training_require(all(nr[, .(constant = uniqueN(N) == 1L), by = site_id]$constant), "Catchment changes over time")
}
models <- fread(file.path(cfg$results, "model_status.csv"))
training_require(nrow(models) == 8L && all(models$ok), "An event study failed; inspect model_status.csv")
effects <- fread(file.path(cfg$results, "dynamic_att.csv"))
training_require(nrow(effects) == 80L && all(effects[event_time == 0, estimate] == 0), "Dynamic support or reference differs")
decomp <- merge(effects[group != "total", .(sum_effects = sum(estimate)), by = .(timing, event_time)],
                effects[group == "total", .(timing, event_time, estimate)], by = c("timing", "event_time"))
training_require(nrow(decomp) == 20L && max(abs(decomp$sum_effects - decomp$estimate)) < 1e-8, "Effect decomposition differs")
model_members <- fread(file.path(cfg$results, "estimation_sample_membership.csv"), colClasses = c(GEOID = "character"))
for (j in seq_len(nrow(models))) {
  m <- models[j]
  p <- read_prep(paste0("catchment_decade_", m$timing, ".csv"))
  x <- model_members[model == m$model]
  training_require(nrow(x) == nrow(p) && all(x$x == p$births_baseline) &&
                     identical(x[, .(stack_unit_num, GEOID, decade, g, event_id, sample_role)], p[, .(stack_unit_num, GEOID, decade, g, event_id, sample_role)]),
                   "Published model membership or baseline differs")
}
cat("PASS: prior and new release checksums; deterministic catchments; fixed denominator support; original classifications; pooled counts/rates/baselines; exact frozen samples; eight models and additive effects. No files modified.\n")
