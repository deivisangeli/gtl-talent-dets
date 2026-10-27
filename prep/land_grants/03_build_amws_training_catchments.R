# Build fixed county catchments and training outcomes without modifying prior releases.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(dirname(script), "amws_training_helpers.R"))
source(file.path(dirname(script), "amws_catchment_helpers.R"))
suppressPackageStartupMessages({library(sf); library(readxl)})
cfg <- catchment_config(repo, training_root())
if (dir.exists(cfg$output)) {
  invisible(training_check_release(cfg$output))
  cat("Catchment preparation already complete and verified.\n")
  quit(status = 0)
}
started <- proc.time()[3]
stage <- file.path(cfg$local, "prep_stage")
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
invisible(training_check_release(cfg$previous_output))
invisible(training_check_release(cfg$previous_results))
frozen_files <- setNames(file.path(cfg$previous_results, paste0("frozen_sample_", c("standard_decade", "alternative_decade"), ".csv")),
                         c("standard_decade", "alternative_decade"))
source_files <- c(cfg$persons, cfg$person_manifest, cfg$units, cfg$panels, cfg$pairs, cfg$population, cfg$birth_rates,
  paste0(tools::file_path_sans_ext(cfg$shape), c(".shp", ".shx", ".dbf", ".prj", ".cpg")), frozen_files,
  file.path(cfg$previous_output, c("completion_manifest.json", "entry_classification_and_exclusions.csv")),
  file.path(cfg$previous_results, "completion_manifest.json"), script,
  file.path(dirname(script), c("amws_training_helpers.R", "amws_catchment_helpers.R")))
inputs <- training_hashes(unique(unname(source_files)))
catchment_tests()
pm <- jsonlite::read_json(cfg$person_manifest, simplifyVector = TRUE)
training_require(identical(training_hash(cfg$persons), pm$sha256) && pm$rows == 136489L, "Canonical source differs")
units <- fread(cfg$units, colClasses = c(GEOID = "character"))
pairs <- as.data.table(readxl::read_excel(cfg$pairs, sheet = "county_pairs_long"))
selected <- unique(pairs[, .(college, experiment_year, sample_role = "treated", county = selected_county, state = selected_state,
                              site_latitude = selected_lat, site_longitude = selected_lon)])
runner <- unique(pairs[runner_up_match_status %in% c("matched_same_state", "matched_cross_state"),
  .(college, experiment_year, sample_role = "runner_up", county = runner_up_county, state = runner_up_state_assumed,
    site_latitude = runner_up_lat, site_longitude = runner_up_lon)])
sites <- merge(units, rbindlist(list(selected, runner)), by = c("college", "experiment_year", "sample_role", "county", "state"), all.x = TRUE)
sites[, site_id := paste(event_id, GEOID, sample_role, sep = "_")]
training_require(nrow(sites) == 168L && uniqueN(sites$event_id) == 57L && !anyDuplicated(sites$site_id) && !anyDuplicated(sites$GEOID), "Frozen site mapping differs")
training_require(all(is.finite(sites$site_latitude) & abs(sites$site_latitude) <= 90 &
                       is.finite(sites$site_longitude) & abs(sites$site_longitude) <= 180), "Invalid site coordinate")
sf::sf_use_s2(TRUE)
county_geom <- sf::st_read(cfg$shape, quiet = TRUE)
# All allocation sites are in the contiguous US; remote states/territories cannot enter a 50 km catchment.
county_geom <- county_geom[!county_geom$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"), ]
points <- sf::st_transform(sf::st_centroid(sf::st_geometry(sf::st_transform(county_geom, 5070))), 4326)
xy <- sf::st_coordinates(points)
all_centroids <- data.table(GEOID = county_geom$GEOID, county_name = county_geom$NAME, longitude = xy[,1], latitude = xy[,2])
site_points <- sf::st_as_sf(as.data.frame(sites), coords = c("site_longitude", "site_latitude"), crs = 4326)
site_distance <- units::drop_units(sf::st_distance(points, site_points))
candidates <- rbindlist(lapply(seq_len(nrow(sites)), function(j) {
  ii <- union(which(site_distance[,j] <= training_radius_m), match(sites$GEOID[j], all_centroids$GEOID))
  data.table(event_id = sites$event_id[j], site_id = sites$site_id[j], sample_role = sites$sample_role[j],
    anchor_GEOID = sites$GEOID[j], GEOID = all_centroids$GEOID[ii], distance_m = as.numeric(site_distance[ii,j]),
    home = all_centroids$GEOID[ii] == sites$GEOID[j])
}))
assigned <- catchment_assign(candidates)
training_require(nrow(assigned[home == TRUE]) == 168L && uniqueN(assigned$GEOID) == 622L, "Catchment geographic coverage differs")
decisions <- merge(candidates, assigned[, .(event_id, GEOID, assigned_site_id = site_id)], by = c("event_id", "GEOID"), all.x = TRUE)
decisions[, selected := site_id == assigned_site_id]
decisions[, decision := fifelse(selected, fifelse(home, "original_county_priority", "nearest_site_or_stable_tie"), "assigned_to_other_site")]
fwrite(sites, file.path(stage, "allocation_sites.csv"))
fwrite(decisions, file.path(stage, "catchment_candidates_and_decisions.csv"))
fwrite(assigned, file.path(stage, "geographic_catchment_membership.csv"))
fwrite(assigned[, .(events = uniqueN(event_id), event_ids = paste(sort(unique(event_id)), collapse = "|")), by = GEOID],
       file.path(stage, "cross_event_county_reuse.csv"))
geoids <- sort(unique(assigned$GEOID))
centroids <- all_centroids[match(geoids, GEOID)]
cat("Classifying canonical AMWS entries across", length(geoids), "catchment counties.\n")

header <- names(fread(cfg$persons, nrows = 0L))
award_prefixes <- sub("award_index$", "", grep("^(ba_bs|ma|ms|phd)_award_[0-9]+_award_index$", header, value = TRUE))
needed <- c("source_key", "birth_year", "geoid", "geo_geoid", "kept", "is_us_birth", "birth_country", "raw_text_adjusted",
            "oa_award_count", "oa_matched_award_count", "education_status")
degree_fields <- grep("^(ba_bs|ma|ms|phd)_award_[0-9]+_(award_index|raw_institution|degree_label|year|year_raw|assignment_class|conditional_ids|flags|matching_flags|fully_matched|condition_conflict|oa_id_count|institution_[0-9]+_(name|oa_id|latitude|longitude))$", header, value = TRUE)
cat("Reading canonical AMWS input and applying existing eligibility rules.\n")
d <- fread(cfg$persons, select = unique(c(needed, degree_fields)), colClasses = "character", na.strings = NULL)
training_require(nrow(d) == 136489L && !anyDuplicated(d$source_key), "Unexpected source entries")
d[, `:=`(edition = as.integer(sub(":.*$", "", source_key)), year = suppressWarnings(as.integer(birth_year)))]
d[, GEOID := training_geoid(ifelse(edition == 1986L, geo_geoid, geoid))]
d[, see_previous_contaminated := edition == 1986L & training_contaminated(raw_text_adjusted)]
d[, exclusion_reason := fcase(
  is.na(year), "missing_or_invalid_birth_year",
  year < 1830L | year > 1959L, "outside_birth_year_window",
  edition != 1986L & !training_true(kept), "earlier_not_kept",
  edition == 1986L & (!training_true(is_us_birth) | birth_country != "USA"), "not_classified_us_birth",
  is.na(GEOID), "missing_birth_county",
  !GEOID %in% geoids, "birth_county_outside_event_pool",
  see_previous_contaminated, "see_previous_contamination",
  default = "eligible")]
eligible <- d[exclusion_reason == "eligible"]
training_require(nrow(eligible) > 12360L, "No additional eligible entries")

# One long row per reported earned award, plus separate assigned institution rows.
awards <- rbindlist(lapply(award_prefixes, function(p) {
  take <- which(training_has(eligible[[paste0(p, "award_index")]]))
  if (!length(take)) return(NULL)
  data.table(source_key = eligible$source_key[take], prefix = p,
             award_index = as.integer(eligible[[paste0(p, "award_index")]][take]),
             family = sub("_award_.*$", "", p), raw_institution = eligible[[paste0(p, "raw_institution")]][take],
             degree_label = eligible[[paste0(p, "degree_label")]][take], year = eligible[[paste0(p, "year")]][take],
             year_raw = eligible[[paste0(p, "year_raw")]][take],
             assignment_class = eligible[[paste0(p, "assignment_class")]][take],
             conditional_ids = eligible[[paste0(p, "conditional_ids")]][take],
             flags = eligible[[paste0(p, "flags")]][take], matching_flags = eligible[[paste0(p, "matching_flags")]][take],
             fully_matched = training_true(eligible[[paste0(p, "fully_matched")]][take]),
             condition_conflict = training_true(eligible[[paste0(p, "condition_conflict")]][take]),
             reported_id_count = as.integer(eligible[[paste0(p, "oa_id_count")]][take]))
}), fill = TRUE)
institutions <- rbindlist(lapply(award_prefixes, function(p) rbindlist(lapply(1:2, function(k) {
  q <- paste0(p, "institution_", k, "_")
  take <- which(training_has(eligible[[paste0(p, "award_index")]]) & training_has(eligible[[paste0(q, "oa_id")]]))
  if (!length(take)) return(NULL)
  data.table(source_key = eligible$source_key[take], award_index = as.integer(eligible[[paste0(p, "award_index")]][take]),
             institution_slot = k, openalex_id = eligible[[paste0(q, "oa_id")]][take],
             institution_name = eligible[[paste0(q, "name")]][take],
             latitude = as.numeric(eligible[[paste0(q, "latitude")]][take]),
             longitude = as.numeric(eligible[[paste0(q, "longitude")]][take]), GEOID = eligible$GEOID[take])
}), fill = TRUE)), fill = TRUE)
training_require(!anyDuplicated(institutions[, .(source_key, award_index, openalex_id)]), "Duplicate award institution")
training_require(all(is.finite(institutions$latitude) & abs(institutions$latitude) <= 90 &
                       is.finite(institutions$longitude) & abs(institutions$longitude) <= 180), "Invalid assigned institution coordinates")
reference <- unique(institutions[, .(openalex_id, institution_name, latitude, longitude)])
training_require(!anyDuplicated(reference$openalex_id), "Assigned ID has conflicting coordinates or names")
setorder(reference, openalex_id)

cat("Computing training distances for", nrow(reference), "institutions.\n")
county_points <- sf::st_as_sf(as.data.frame(centroids), coords = c("longitude", "latitude"), crs = 4326)
institution_points <- sf::st_as_sf(as.data.frame(reference), coords = c("longitude", "latitude"), crs = 4326)
matrix <- sf::st_distance(county_points, institution_points)
distance <- data.table(GEOID = rep(geoids, times = nrow(reference)),
                       openalex_id = rep(reference$openalex_id, each = length(geoids)),
                       distance_m = as.numeric(matrix))
training_require(all(is.finite(distance$distance_m) & distance$distance_m >= 0), "Invalid distance matrix")
institutions <- merge(institutions, distance, by = c("GEOID", "openalex_id"), all.x = TRUE, sort = FALSE)
training_require(!anyNA(institutions$distance_m), "Unmatched county/institution distance")
ia <- institutions[, .(n_ids = .N, n_locations = sum(is.finite(distance_m)), min_distance_m = min(distance_m)), by = .(source_key, award_index)]
awards <- merge(awards, ia, by = c("source_key", "award_index"), all.x = TRUE, sort = FALSE)
awards[is.na(n_ids), `:=`(n_ids = 0L, n_locations = 0L)]
training_require(all(awards$n_ids == awards$reported_id_count), "Wide and long award ID counts differ")
awards[, complete := fully_matched & !condition_conflict & n_ids > 0L & n_locations == n_ids]
summary <- awards[, .(n_reported_awards = .N, n_complete_awards = sum(complete), n_unmatched_awards = sum(n_ids == 0L),
                      n_partial_awards = sum(n_ids > 0L & !complete), n_conflicting_awards = sum(condition_conflict),
                      n_raw_institutions = sum(training_has(raw_institution))), by = source_key]
located <- institutions[, .(n_assigned_locations = .N, min_distance_m = min(distance_m)), by = source_key]
person <- eligible[, .(source_key, edition, GEOID, year, education_status, oa_award_count, oa_matched_award_count)]
person <- merge(person, summary, by = "source_key", all.x = TRUE, sort = FALSE)
person <- merge(person, located, by = "source_key", all.x = TRUE, sort = FALSE)
zero_cols <- c("n_reported_awards", "n_complete_awards", "n_unmatched_awards", "n_partial_awards", "n_conflicting_awards", "n_raw_institutions", "n_assigned_locations")
for (col in zero_cols) set(person, which(is.na(person[[col]])), col, 0L)
training_require(all(person$n_reported_awards == as.integer(person$oa_award_count)), "Reported award counts differ")
person[, training_class := fcase(
  is.finite(min_distance_m) & min_distance_m <= training_radius_m, "ever_nearby",
  n_reported_awards > 0L & n_complete_awards == n_reported_awards & n_conflicting_awards == 0L &
    is.finite(min_distance_m) & min_distance_m > training_radius_m, "never_nearby", default = "unknown")]
person[, unknown_reason := fcase(training_class != "unknown", "",
  n_reported_awards == 0L, "no_qualifying_award", n_raw_institutions == 0L, "no_institution_text",
  n_assigned_locations == 0L, "no_assigned_location", n_conflicting_awards > 0L, "matching_conflict",
  n_unmatched_awards > 0L, "unmatched_award", default = "partially_matched_award")]
# Independently apply the scalar classification helper to every eligible entry.
setindexv(awards, "source_key")
setindexv(institutions, "source_key")
for (i in seq_len(nrow(person))) {
  p <- person[i]
  a <- awards[source_key == p$source_key]
  distances <- institutions[source_key == p$source_key, distance_m]
  training_require(identical(training_classify(distances, nrow(a), a$complete, a$condition_conflict), p$training_class), "Classification reconstruction differs")
}
audit <- merge(d[, .(source_key, edition, year, GEOID, kept, is_us_birth, see_previous_contaminated, exclusion_reason)],
               person[, !c("edition", "year", "GEOID"), with = FALSE], by = "source_key", all.x = TRUE, sort = FALSE)
setorder(audit, edition, source_key)
fwrite(audit, file.path(stage, "entry_classification_and_exclusions.csv"))
fwrite(awards, file.path(stage, "award_location_evidence.csv"))
fwrite(institutions, file.path(stage, "assigned_institution_distances.csv"))
fwrite(centroids, file.path(stage, "county_centroids.csv"))
fwrite(reference, file.path(stage, "institution_coordinate_lookup.csv"))
fwrite(distance, file.path(stage, "county_institution_distances.csv"))



# Validate unchanged classifications for the original eligible county-only entries.
old_audit <- fread(file.path(cfg$previous_output, "entry_classification_and_exclusions.csv"), colClasses = c(GEOID = "character"))
original <- old_audit[exclusion_reason == "eligible", .(source_key, old_class = training_class, old_GEOID = GEOID, old_year = year)]
comparison <- merge(original, person[, .(source_key, training_class, GEOID, year)], by = "source_key", all.x = TRUE)
training_require(nrow(comparison) == 12360L && !anyNA(comparison$training_class) && all(comparison$old_class == comparison$training_class &
                   comparison$old_GEOID == comparison$GEOID & comparison$old_year == comparison$year), "Original classifications changed")
fwrite(comparison, file.path(stage, "original_classification_preservation.csv"))

annual_counts <- person[, .(n_amws_ever_trained_50km = sum(training_class == "ever_nearby"),
  n_amws_never_trained_50km = sum(training_class == "never_nearby"), n_amws_training_unknown = sum(training_class == "unknown"),
  n_amws_training_total = .N), by = .(GEOID, year)]
annual <- merge(CJ(GEOID = geoids, year = 1830:1959), annual_counts, by = c("GEOID", "year"), all.x = TRUE)
for (col in unname(training_counts)) set(annual, which(is.na(annual[[col]])), col, 0L)
knots <- fread(cfg$population, colClasses = c(GEOID = "character"))
knots[, GEOID := training_geoid(GEOID)]
knots <- knots[GEOID %in% geoids & source %in% c("nhgis", "manual") & is.finite(population) & population > 0]
training_require(!anyDuplicated(knots[, .(GEOID, decade)]), "Duplicate population knots")
pop <- rbindlist(lapply(geoids, function(gid) {
  k <- knots[GEOID == gid][order(decade)]; years <- 1830:1959
  v <- if (nrow(k) >= 2L) approx(k$decade, k$population, xout = years, rule = 1)$y else
    if (nrow(k) == 1L) ifelse(years == k$decade, k$population, NA_real_) else rep(NA_real_, length(years))
  data.table(GEOID = gid, year = years, population = v)
}))
rates <- fread(cfg$birth_rates, select = c("year", "us_birth_rate_year"))
rates <- rates[year %in% 1830:1959, .(us_birth_rate_year = unique(na.omit(us_birth_rate_year))), by = year]
training_require(nrow(rates) == 130L && !anyDuplicated(rates$year) && all(is.finite(rates$us_birth_rate_year) & rates$us_birth_rate_year > 0), "Invalid national birth rates")
annual <- merge(annual, pop, by = c("GEOID", "year"), all.x = TRUE)
annual <- merge(annual, rates, by = "year", all.x = TRUE)
annual[, county_births_estimate_year := population * us_birth_rate_year]
fwrite(annual, file.path(stage, "county_year_training_and_denominators.csv"))
quality <- list(); analysis_membership <- list(); count_coverage <- list(); coverage_records <- list()
for (timing in names(frozen_files)) {
  x <- copy(annual); x[, decade := training_decade(year, timing)]
  panel <- x[, c(lapply(.SD, sum), list(
    population = if (all(is.na(population))) NA_real_ else mean(population, na.rm = TRUE),
    county_births_estimate = if (all(is.na(county_births_estimate_year))) NA_real_ else sum(county_births_estimate_year, na.rm = TRUE),
    annual_years = .N, annual_years_with_births = sum(is.finite(county_births_estimate_year) & county_births_estimate_year > 0))),
    by = .(GEOID, decade), .SDcols = unname(training_counts)]
  legacy <- fread(cfg$panels[[timing]], colClasses = c(GEOID = "character"))
  check <- merge(panel[GEOID %in% units$GEOID], legacy[, .(GEOID, decade, original_births = county_births_estimate, original_population = population)],
                 by = c("GEOID", "decade"), all = TRUE)
  training_require(isTRUE(all.equal(check$county_births_estimate, check$original_births, tolerance = 1e-12)) &&
                     isTRUE(all.equal(check$population, check$original_population, tolerance = 1e-12)), "Rebuilt original denominator differs")
  # Reuse the published values exactly, rather than the numerically equivalent reconstruction.
  panel[legacy, on = .(GEOID, decade), `:=`(county_births_estimate = i.county_births_estimate, population = i.population)]
  frozen <- fread(frozen_files[[timing]], colClasses = c(GEOID = "character"))
  frozen[, site_id := paste(event_id, GEOID, sample_role, sep = "_")]
  active <- unique(frozen$site_id)
  members <- assigned[site_id %in% active]
  period_grid <- merge(frozen[, .(site_id, decade)], members[, .(site_id, GEOID, home)], by = "site_id", allow.cartesian = TRUE)
  required <- merge(period_grid, panel[, .(GEOID, decade, county_births_estimate)], by = c("GEOID", "decade"), all.x = TRUE)
  support <- catchment_supported(required)
  training_require(all(support[home == TRUE, supported]), "An original county lacks frozen denominator support")
  members <- merge(members, support, by = c("site_id", "GEOID", "home"), all.x = TRUE)
  members[, `:=`(timing = timing, inclusion_reason = fifelse(supported, "included_fixed_membership", "excluded_missing_frozen_denominator"))]
  fwrite(required, file.path(stage, paste0("denominator_support_rows_", timing, ".csv")))
  fwrite(members, file.path(stage, paste0("catchment_membership_", timing, ".csv")))
  included <- members[supported == TRUE]
  analysis_membership[[timing]] <- members
  # Restrict pooled panels to the original observation grid. Every member must be supported at every retained row.
  joined <- merge(period_grid, included[, .(site_id, GEOID)], by = c("site_id", "GEOID"))
  joined <- merge(joined, panel, by = c("GEOID", "decade"), all.x = TRUE)
  training_require(all(is.finite(joined$county_births_estimate) & joined$county_births_estimate > 0), "Incomplete pooled denominator")
  pooled <- joined[, c(lapply(.SD, sum), list(population = sum(population), county_births_estimate = sum(county_births_estimate),
    member_count = .N, annual_years = sum(annual_years), annual_years_with_births = sum(annual_years_with_births))),
    by = .(site_id, decade), .SDcols = unname(training_counts)]
  for (group in names(training_counts)) pooled[, (training_rates[[group]]) := training_rate(get(training_counts[[group]]), county_births_estimate)]
  catchment_check_partition(pooled)
  pooled <- merge(frozen, pooled, by = c("site_id", "decade"), all.x = TRUE)
  setnames(pooled, "x", "county_only_births_baseline")
  timing_var <- if (timing == "standard_decade") "g_std" else "g_shift"
  meta <- sites[, .(site_id, treatment_decade = get(timing_var))]
  baseline <- merge(meta, pooled[, .(site_id, decade, county_births_estimate)], by = "site_id", allow.cartesian = TRUE)
  baseline <- baseline[decade == treatment_decade - 10L, .(site_id, births_baseline = county_births_estimate)]
  training_require(!anyDuplicated(baseline$site_id), "Duplicate pooled baseline")
  pooled <- merge(pooled, baseline, by = "site_id", all.x = TRUE)
  pooled[, unit_geography := "fixed_site_county_catchment"]
  setorder(pooled, event_id, sample_role, GEOID, decade)
  training_require(nrow(pooled) == nrow(frozen) && all(is.finite(pooled$births_baseline) & pooled$births_baseline > 0), "Frozen rows or pooled baseline unavailable")
  fwrite(panel, file.path(stage, paste0("county_decade_", timing, ".csv")))
  fwrite(pooled, file.path(stage, paste0("catchment_decade_", timing, ".csv")))
  # Annual panels retain geographic membership; missing denominator years remain explicit.
  ay <- merge(included[, .(site_id, GEOID)], annual, by = "GEOID", allow.cartesian = TRUE)
  ay <- ay[, c(lapply(.SD, sum), list(population = if (all(is.finite(population))) sum(population) else NA_real_,
    estimated_births = if (all(is.finite(county_births_estimate_year))) sum(county_births_estimate_year) else NA_real_,
    member_count = .N, members_with_births = sum(is.finite(county_births_estimate_year)))), by = .(site_id, year), .SDcols = unname(training_counts)]
  fwrite(ay, file.path(stage, paste0("catchment_year_", timing, ".csv")))
  record <- merge(person, included[, .(site_id, event_id, sample_role, GEOID, home)], by = "GEOID", allow.cartesian = TRUE)
  record[, decade := training_decade(year, timing)]
  record <- merge(record, frozen[, .(site_id, decade)], by = c("site_id", "decade"))
  training_require(!anyDuplicated(record[, .(event_id, source_key)]), "Scientist counted twice within experiment")
  fwrite(record, file.path(stage, paste0("analysis_entry_membership_", timing, ".csv")))
  count_coverage[[timing]] <- data.table(timing = timing, events = uniqueN(pooled$event_id), sites = uniqueN(pooled$site_id), rows = nrow(pooled),
    geographic_counties_for_frozen_sites = uniqueN(members$GEOID), analysis_counties = uniqueN(included$GEOID),
    original_counties = uniqueN(included[home == TRUE, GEOID]), excluded_added_memberships = members[supported == FALSE, .N],
    entries_unique = uniqueN(record$source_key), original_entries_unique = uniqueN(record[home == TRUE, source_key]),
    added_entries_unique = uniqueN(record[home == FALSE, source_key]), entries_only_in_added_counties = uniqueN(record$source_key[!record$source_key %in% record[home == TRUE, source_key]]),
    stacked_entries = nrow(record), cross_event_reused_counties = included[, .(events = uniqueN(event_id)), by = GEOID][events > 1L, .N])
  record <- merge(record, meta, by = "site_id", all.x = TRUE)
  coverage_records[[timing]] <- record[, .(timing = timing, N = .N, entries_with_unmatched_awards = sum(n_unmatched_awards > 0L),
    unmatched_awards = sum(n_unmatched_awards), partial_awards = sum(n_partial_awards)),
    by = .(edition, cohort = treatment_decade, sample_role, home, training_class, unknown_reason)]
  quality[[timing]] <- pooled[, .(timing = timing, site_id, decade, annual_years, annual_years_with_births)]
}
fwrite(rbindlist(count_coverage), file.path(stage, "analysis_coverage_summary.csv"))
fwrite(rbindlist(coverage_records), file.path(stage, "matching_coverage_by_edition_cohort_role.csv"))
fwrite(rbindlist(quality), file.path(stage, "annual_denominator_completeness.csv"))
fwrite(person[, .N, by = .(edition, training_class, unknown_reason)], file.path(stage, "coverage_by_edition.csv"))
fwrite(audit[, .N, by = .(edition, exclusion_reason)], file.path(stage, "exclusion_counts.csv"))
coverage <- list(geographic_counties = length(geoids), added_geographic_counties = length(setdiff(geoids, units$GEOID)),
  original_sites = nrow(sites), events = uniqueN(sites$event_id), candidate_memberships = nrow(candidates), geographic_memberships = nrow(assigned),
  forced_original_counties = candidates[home == TRUE & distance_m > training_radius_m, .N], eligible_entries = nrow(person),
  groups = as.list(table(person$training_class)))
validation <- list(passed = TRUE, original_classifications_preserved = nrow(comparison), immutable_inputs = TRUE,
  original_denominators_preserved = TRUE, county_unique_within_experiment = TRUE, fixed_timing_specific_membership = TRUE,
  frozen_observations_preserved = TRUE, count_and_rate_partitions = TRUE, historical_campus_accuracy_verified = FALSE)
training_json(coverage, file.path(stage, "coverage_report.json"))
training_json(validation, file.path(stage, "validation_report.json"))
writeLines(c("Fixed 50 km county catchments around Andrews selected and runner-up sites.",
  "GEOID in county tables is birth/member county. GEOID in pooled panels is the original anchor county, not the full unit geography.",
  "site_id = original event_id / anchor GEOID / role. Original home county always retained; added counties use nearest-site assignment within event.",
  "Assignments precede denominator screening; excluded counties are not reassigned. Cross-event reuse is permitted and audited.",
  "Frozen estimation rows and site identifiers are retained; baseline births and outcome denominators are summed across supported members.",
  "Added members must have positive finite decade births in all frozen rows; membership fixed within timing. Original published denominators reused exactly.",
  "County decade births preserve legacy partial-annual sums; annual_denominator_completeness audits this. Annual pooled births require all members usable that year.",
  "Nearby training remains relative to the scientist's own birth-county centroid. BA/BS, MA, MS, PhD and existing assignment qualifications retained.",
  "No new cross-edition linkage or birthplace correction. OpenAlex reference locations and provisional IDs do not establish historical campuses."),
  file.path(stage, "DATA_DICTIONARY.txt"))
dir.create(file.path(stage, "code_snapshot"), showWarnings = FALSE)
for (p in c(script, file.path(dirname(script), c("amws_training_helpers.R", "amws_catchment_helpers.R")))) file.copy(p, file.path(stage, "code_snapshot", basename(p)), overwrite = TRUE)
capture.output(sessionInfo(), file = file.path(stage, "sessionInfo.txt"))
training_json(list(elapsed_seconds = unname(proc.time()[3] - started)), file.path(stage, "timing_report.json"))
invisible(training_check_release(cfg$previous_output)); invisible(training_check_release(cfg$previous_results))
training_publish(stage, cfg$output, inputs, list(status = "complete", coverage = coverage, validation = validation))
cat("Published catchment preparation:", cfg$output, "\n")
print(rbindlist(count_coverage))
