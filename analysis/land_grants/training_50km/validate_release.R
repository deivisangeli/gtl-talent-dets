# Independent read-only checks of the published training outcomes and models.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo, "prep", "land_grants", "amws_training_helpers.R"))
cfg <- training_config(repo, training_root())
invisible(training_check_release(cfg$output))
invisible(training_check_release(cfg$results))

people <- fread(file.path(cfg$output, "entry_classification_and_exclusions.csv"), colClasses = c(GEOID = "character"))
p <- people[exclusion_reason == "eligible"]
a <- fread(file.path(cfg$output, "award_location_evidence.csv"))
i <- fread(file.path(cfg$output, "assigned_institution_distances.csv"), colClasses = c(GEOID = "character"))
c <- fread(file.path(cfg$output, "county_centroids.csv"), colClasses = c(GEOID = "character"))
d <- fread(file.path(cfg$output, "county_institution_distances.csv"), colClasses = c(GEOID = "character"))
training_require(nrow(people) == 136489L && !anyDuplicated(people$source_key) && nrow(p) == 12360L,
                 "Input coverage or unique eligibility differs")
training_require(all(p$training_class %in% training_groups) &&
                   identical(as.integer(table(factor(p$training_class, levels = training_groups))), c(3413L, 5275L, 3672L)),
                 "Classification coverage differs")
training_require(nrow(c) == 168L && !anyDuplicated(c$GEOID) && all(is.finite(c$latitude) & abs(c$latitude) <= 90 &
                   is.finite(c$longitude) & abs(c$longitude) <= 180), "Invalid centroid coverage or coordinates")
training_require(!anyDuplicated(d[, .(GEOID, openalex_id)]) && nrow(d) == 168L * 715L &&
                   all(is.finite(d$distance_m) & d$distance_m >= 0), "Invalid distance lookup")
joined <- merge(i, d, by = c("GEOID", "openalex_id"), suffixes = c("", "_lookup"), all.x = TRUE)
training_require(nrow(joined) == nrow(i) && !anyNA(joined$distance_m_lookup) &&
                   all(abs(joined$distance_m - joined$distance_m_lookup) < 1e-8), "Assigned distance lookup differs")
training_require(!anyDuplicated(a[, .(source_key, award_index)]) &&
                   all(a$complete == (a$fully_matched & !a$condition_conflict & a$n_ids > 0L & a$n_locations == a$n_ids)),
                 "Award completeness differs")
known_near <- unique(i[distance_m <= training_radius_m, source_key])
training_require(setequal(known_near, p[training_class == "ever_nearby", source_key]), "Ever-nearby assignment differs")
never <- p[training_class == "never_nearby"]
training_require(all(never$n_reported_awards > 0L & never$n_complete_awards == never$n_reported_awards &
                   never$n_conflicting_awards == 0L & never$min_distance_m > training_radius_m), "Never-nearby assignment differs")

for (timing in names(cfg$panels)) {
  name <- if (timing == "standard_decade") "county_decade_training.csv" else "county_decade_training_alt.csv"
  panel <- fread(file.path(cfg$output, name), colClasses = c(GEOID = "character"))
  legacy <- fread(cfg$panels[[timing]], colClasses = c(GEOID = "character"))
  training_require(nrow(panel) == nrow(legacy) && all(vapply(names(legacy), function(k) identical(panel[[k]], legacy[[k]]), logical(1))),
                   "Frozen original panel fields differ")
  x <- copy(p)
  x[, decade := training_decade(year, timing)]
  count <- x[, .(expected = .N), by = .(GEOID, decade)]
  compared <- merge(panel[, .(GEOID, decade, n_amws_training_total)], count, by = c("GEOID", "decade"), all.x = TRUE)
  compared[is.na(expected), expected := 0L]
  training_require(all(compared$n_amws_training_total == compared$expected) && sum(compared$expected) == nrow(p),
                   "Decade aggregation differs")
  training_require(all(rowSums(panel[, unname(training_counts[1:3]), with = FALSE]) == panel$n_amws_training_total),
                   "Decade count partition fails")
  for (group in names(training_counts)) training_require(isTRUE(all.equal(panel[[training_rates[[group]]]],
    training_rate(panel[[training_counts[[group]]]], panel$county_births_estimate), tolerance = 1e-12)), "Rate denominator differs")
}

status <- fread(file.path(cfg$results, "model_status.csv"))
coefficients <- fread(file.path(cfg$results, "dynamic_att.csv"))
members <- fread(file.path(cfg$results, "estimation_sample_membership.csv"), colClasses = c(GEOID = "character"))
training_require(nrow(status) == 8L && all(status$ok) && nrow(coefficients) == 80L, "Model completion differs")
training_require(all(is.finite(coefficients$estimate) & is.finite(coefficients$se) & coefficients$se >= 0) &&
                   all(coefficients[event_time == 0, estimate] == 0) && all(coefficients[event_time == 0, se] == 0),
                 "Coefficient inference/reference differs")
for (timing in unique(status$timing)) {
  this_timing <- timing
  frozen <- fread(file.path(cfg$results, paste0("frozen_sample_", timing, ".csv")), colClasses = c(GEOID = "character"))
  for (model_name in status[timing == this_timing, model]) {
    membership <- members[model == model_name, names(frozen), with = FALSE]
    training_require(identical(membership, frozen), "Published model membership differs")
    training_require(identical(sort(coefficients[model == model_name, event_time]), seq(-20L, 70L, 10L)), "Dynamic support differs")
  }
}
decomposition <- merge(coefficients[group != "total", .(component_sum = sum(estimate)), by = .(timing, event_time)],
                       coefficients[group == "total", .(timing, event_time, benchmark = estimate)], by = c("timing", "event_time"))
training_require(nrow(decomposition) == 20L && max(abs(decomposition$component_sum - decomposition$benchmark)) < 1e-8,
                 "Published coefficient partition differs")
cat("PASS: protected inputs and both published checksums; 136489 audited / 12360 eligible entries; distance joins;\n",
    "award completeness; classifications; frozen denominators and timing; eight models and exact sample membership;\n",
    "80 dynamic coefficients and additive decomposition. No published files modified.\n", sep = "")
