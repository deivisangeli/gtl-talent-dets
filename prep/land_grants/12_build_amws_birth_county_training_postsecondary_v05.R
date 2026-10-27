# Refresh six birth-county training outcomes from the v05 education release.
# Same rules as 11_build_amws_birth_county_training_postsecondary_v04.R. v05 only adds
# manual institution matches to awards v04 left unmatched, so relative to v04 no entry
# may leave ever or never, and every entry whose class changes has a manual award.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(dirname(script), "amws_training_helpers.R"))
suppressPackageStartupMessages(library(sf))
source(file.path(dirname(script), "amws_birth_county_training_helpers.R"))
birth_training_stamp <- "amws_birth_county_training_postsecondary_v05_20261020"
setDTthreads(4)
cfg <- training_config(repo, training_root())
previous <- file.path(cfg$root, "output/land_grants/amws_birth_county_training_postsecondary_v04_20261019")
education_release <- file.path(cfg$root, "output/amws/amws_education_postsecondary_v05_20261020")
award_changes <- file.path(education_release, c("removed_awards.csv", "changed_awards.csv"))
cfg$persons <- file.path(education_release, "amws_open_alex.csv")
cfg$person_manifest <- file.path(education_release, "amws_open_alex_manifest.json")
schema_path <- file.path(education_release, "schema.json")
schema <- jsonlite::read_json(schema_path, simplifyVector = TRUE)
family_pattern <- paste0("^(", paste(schema$families, collapse = "|"), ")_award_")
frozen_release <- file.path(cfg$root, "output/land_grants/amws_county_training_location_20261014")
cfg$output <- file.path(cfg$root, "output/land_grants", birth_training_stamp)
cfg$local <- file.path(Sys.getenv("AMWS_CHECKPOINT_ROOT", unset = file.path(Sys.getenv("USERPROFILE"), ".codex/amws_checkpoints")), birth_training_stamp)
frozen_paths <- file.path(frozen_release, paste0("frozen_", rep(c("relative", "calendar"), each = 2), "_", rep(names(cfg$panels), 2), ".csv"))
previous_classification <- file.path(previous, "entry_classification_and_exclusions.csv")
code_files <- c(script, file.path(dirname(script), c("amws_training_helpers.R", "amws_birth_county_training_helpers.R", "test_amws_birth_county_training.R")))
if (dir.exists(cfg$output)) {
  training_check_release(cfg$output)
  cat("Prepared release already complete and verified:", cfg$output, "\n")
  quit(status = 0)
}
started <- proc.time()[3]
stage <- file.path(cfg$local, "prep_stage")
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
shape_parts <- paste0(tools::file_path_sans_ext(cfg$shape), c(".shp", ".shx", ".dbf", ".prj", ".cpg"))
source_files <- c(cfg$persons, cfg$person_manifest, cfg$units, cfg$panels, shape_parts,
                  cfg$legacy_early, cfg$sample_helper, cfg$main_status, cfg$main_audit,
                  code_files, frozen_paths, previous_classification, schema_path, file.path(education_release, "completion_manifest.json"),
                  award_changes)
training_require(all(file.exists(source_files)), "A frozen input or code file is missing")
inputs <- training_hashes(unique(unname(source_files)))
pm <- jsonlite::read_json(cfg$person_manifest, simplifyVector = TRUE)
training_require(identical(training_hash(cfg$persons), pm$sha256) && pm$rows == 136489L, "Canonical AMWS input differs from manifest")
units <- fread(cfg$units, colClasses = c(GEOID = "character"))
units[, GEOID := training_geoid(GEOID)]
geoids <- sort(unique(units$GEOID))
training_require(uniqueN(units$event_id) == 57L && length(geoids) == 168L, "Frozen event pool differs")
header <- names(fread(cfg$persons, nrows = 0L))
award_prefixes <- sub("award_index$", "", grep(paste0(family_pattern, "[0-9]+_award_index$"), header, value = TRUE))
needed <- c("source_key", "birth_year", "geoid", "geo_geoid", "kept", "is_us_birth", "birth_country", "raw_text_adjusted",
            "oa_award_count", "oa_matched_award_count", "education_status", "manual_assigned_awards")
degree_fields <- grep(paste0(family_pattern, "[0-9]+_(award_index|raw_institution|degree_label|year|year_raw|assignment_class|conditional_ids|flags|matching_flags|fully_matched|condition_conflict|oa_id_count|institution_[0-9]+_(name|oa_id|latitude|longitude))$"), header, value = TRUE)
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
training_require(nrow(eligible) == 12360L, "Unexpected eligible AMWS count in event counties")

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
institutions <- rbindlist(lapply(award_prefixes, function(p) {
  slots <- sub(paste0("^", p, "institution_([0-9]+)_oa_id$"), "\\1", grep(paste0("^", p, "institution_[0-9]+_oa_id$"), header, value = TRUE))
  rbindlist(lapply(as.integer(slots), function(k) {
  q <- paste0(p, "institution_", k, "_")
  take <- which(training_has(eligible[[paste0(p, "award_index")]]) & training_has(eligible[[paste0(q, "oa_id")]]))
  if (!length(take)) return(NULL)
  data.table(source_key = eligible$source_key[take], award_index = as.integer(eligible[[paste0(p, "award_index")]][take]),
             institution_slot = k, openalex_id = eligible[[paste0(q, "oa_id")]][take],
             institution_name = eligible[[paste0(q, "name")]][take],
             latitude = as.numeric(eligible[[paste0(q, "latitude")]][take]),
             longitude = as.numeric(eligible[[paste0(q, "longitude")]][take]), GEOID = eligible$GEOID[take])
}), fill = TRUE)
}), fill = TRUE)
training_require(!anyDuplicated(institutions[, .(source_key, award_index, openalex_id)]), "Duplicate award institution")
training_require(all(is.finite(institutions$latitude) & abs(institutions$latitude) <= 90 &
                       is.finite(institutions$longitude) & abs(institutions$longitude) <= 180), "Invalid assigned institution coordinates")
reference <- unique(institutions[, .(openalex_id, institution_name, latitude, longitude)])
training_require(!anyDuplicated(reference$openalex_id), "Assigned ID has conflicting coordinates or names")
setorder(reference, openalex_id)

cat("Computing cached county centroids and great-circle distances for", nrow(reference), "institutions.\n")
sf::sf_use_s2(TRUE)
counties <- sf::st_read(cfg$shape, quiet = TRUE)
county_geom <- counties[match(geoids, counties$GEOID), ]
training_require(!anyNA(county_geom$GEOID), "Missing county geometry")
points <- sf::st_transform(sf::st_centroid(sf::st_geometry(sf::st_transform(county_geom, 5070))), 4326)
xy <- sf::st_coordinates(points)
centroids <- data.table(GEOID = geoids, county_name = county_geom$NAME, longitude = xy[, 1], latitude = xy[, 2])
county_points <- sf::st_as_sf(as.data.frame(centroids), coords = c("longitude", "latitude"), crs = 4326)
institution_points <- sf::st_as_sf(as.data.frame(reference), coords = c("longitude", "latitude"), crs = 4326)
matrix <- sf::st_distance(county_points, institution_points)
# Closed polygon membership: a boundary point qualifies for every touching county.
inside <- sf::st_intersects(institution_points, sf::st_transform(county_geom, 4326), model = "closed")
local_matrix <- matrix(FALSE, length(geoids), nrow(reference))
for (k in seq_along(inside)) local_matrix[inside[[k]], k] <- TRUE
distance <- data.table(GEOID = rep(geoids, times = nrow(reference)),
                       openalex_id = rep(reference$openalex_id, each = length(geoids)),
                       distance_m = as.numeric(matrix), same_county = as.vector(local_matrix))
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
located <- institutions[, .(n_assigned_locations = .N, min_distance_m = min(distance_m), ever_same_county = any(same_county)), by = source_key]
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
person[is.na(ever_same_county), ever_same_county := FALSE]
person[, class_50km := sub("_nearby$", "", training_class)]
person[, class_same_county := fcase(ever_same_county, "ever",
  n_reported_awards > 0L & n_complete_awards == n_reported_awards & n_conflicting_awards == 0L, "never",
  default = "unknown")]
for (geo in birth_training_geographies) person[, (paste0("unknown_reason_", geo)) :=
  fifelse(get(paste0("class_", geo)) != "unknown", "", fcase(
    n_reported_awards == 0L, "no_qualifying_award", n_raw_institutions == 0L, "no_institution_text",
    n_assigned_locations == 0L, "no_assigned_location", n_conflicting_awards > 0L, "matching_conflict",
    n_unmatched_awards > 0L, "unmatched_award", default = "partially_matched_award"))]
old <- fread(previous_classification, colClasses = c(GEOID = "character"))
old <- old[exclusion_reason == "eligible"]
comparison <- merge(person[, .(source_key, GEOID, year, class_same_county, class_50km)],
  old[, .(source_key, GEOID, year, class_same_county, class_50km)], by = "source_key", all = TRUE, suffixes = c("_new", "_old"))
training_require(nrow(comparison) == nrow(person) && !anyNA(comparison) &&
  all(comparison$GEOID_new == comparison$GEOID_old & comparison$year_new == comparison$year_old), "Birth-origin eligibility changed")
# v05 removes and re-parses nothing (its removed/changed award logs are empty) and
# never alters an award v04 fully matched. It only adds IDs to unmatched awards, so
# ever and never are absorbing: only unknown entries may move, and only through an
# award v05 assigned manually.
changed_keys <- unique(unlist(lapply(award_changes, function(p) fread(p, select = "source_key", colClasses = "character")$source_key)))
training_require(length(changed_keys) == 0L, "The v05 education release lists removed or changed awards")
manual_keys <- eligible[as.integer(manual_assigned_awards) > 0L, source_key]
moved <- list()
for (geo in birth_training_geographies) {
  old_class <- comparison[[paste0("class_", geo, "_old")]]
  new_class <- comparison[[paste0("class_", geo, "_new")]]
  training_require(!any(old_class %in% c("ever", "never") & new_class != old_class), "An ever or never entry changed class under v05")
  training_require(all(comparison$source_key[new_class != old_class] %in% manual_keys), "An entry changed class without a manual award")
  moved[[geo]] <- as.list(table(factor(new_class[old_class == "unknown" & new_class != "unknown"], levels = c("ever", "never"))))
}
comparison[, manual_award := source_key %in% manual_keys]
fwrite(comparison, file.path(stage, "classification_transitions.csv"))
transitions <- rbindlist(lapply(birth_training_geographies, function(geo)
  comparison[, .(geography = geo, entries = .N), by = .(old_class = get(paste0("class_",geo,"_old")), new_class = get(paste0("class_",geo,"_new")))]))
fwrite(transitions, file.path(stage, "classification_transition_summary.csv"))
# Independently apply the scalar classification helper to every eligible entry.
setindexv(awards, "source_key")
setindexv(institutions, "source_key")
for (i in seq_len(nrow(person))) {
  p <- person[i]
  a <- awards[source_key == p$source_key]
  distances <- institutions[source_key == p$source_key, distance_m]
  training_require(identical(training_classify(distances, nrow(a), a$complete, a$condition_conflict), p$training_class), "Classification reconstruction differs")
  for (geo in birth_training_geographies) {
    local <- if (geo == "50km") distances <= 50000 else institutions[source_key == p$source_key, same_county]
    training_require(identical(birth_training_class(local, nrow(a), a$complete, a$condition_conflict), p[[paste0("class_", geo)]]), "Birth-origin classification reconstruction differs")
  }
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

annual_counts <- person[, {
  values <- unlist(lapply(birth_training_geographies, function(geo)
    vapply(birth_training_groups, function(group) sum(get(paste0("class_", geo)) == group), integer(1))))
  as.list(setNames(c(values, .N), unname(birth_training_counts)))
}, by = .(GEOID, year)]
annual <- merge(CJ(GEOID = geoids, year = 1830:1959), annual_counts, by = c("GEOID", "year"), all.x = TRUE, sort = FALSE)
for (col in unname(birth_training_counts)) set(annual, which(is.na(annual[[col]])), col, 0L)
birth_training_partition(annual)
fwrite(annual, file.path(stage, "county_year_training_counts.csv"))
reconcile <- list()
for (timing in names(cfg$panels)) {
  base <- fread(cfg$panels[[timing]], colClasses = c(GEOID = "character"))
  base[, GEOID := training_geoid(GEOID)]
  original <- copy(base)
  tmp <- copy(annual)
  tmp[, decade := training_decade(year, timing)]
  counts <- tmp[, lapply(.SD, sum), by = .(GEOID, decade), .SDcols = unname(birth_training_counts)]
  base[, .original_order := .I]
  base <- merge(base, counts, by = c("GEOID", "decade"), all.x = TRUE, sort = FALSE)
  setorder(base, .original_order)
  base[, .original_order := NULL]
  training_require(all(vapply(names(original), function(k) identical(base[[k]], original[[k]]), logical(1))), "Frozen denominator or panel column changed")
  for (group in names(birth_training_counts)) base[, (birth_training_rates[[group]]) := training_rate(get(birth_training_counts[[group]]), county_births_estimate)]
  birth_training_partition(base)
  filename <- paste0("county_decade_", timing, ".csv")
  fwrite(base, file.path(stage, filename))
  readback <- fread(file.path(stage, filename), colClasses = c(GEOID = "character"))
  training_require(all(vapply(names(original), function(k) identical(readback[[k]], original[[k]]), logical(1))), "Written panel changed a frozen column")
  reconcile[[timing]] <- base[, .(timing = timing, GEOID, decade, legacy_n_amws = n_amws,
                                  updated_n_amws = n_amws_born_total, difference = n_amws_born_total - n_amws)]
}
reconciliation <- rbindlist(reconcile)
training_require(all(reconciliation[difference != 0, GEOID] == "17113") && all(reconciliation[difference != 0, difference] == -1L) &&
                   all(reconciliation[difference != 0, decade] == 1930L) && nrow(reconciliation[difference != 0]) == 2L,
                 "Unexpected canonical versus legacy count differences")
fwrite(reconciliation, file.path(stage, "legacy_count_reconciliation.csv"))
legacy <- fread(cfg$legacy_early, colClasses = "character", na.strings = NULL)
conflict <- legacy[as.integer(edition) == 1955L & as.integer(lineid) == 46642L, .(source = "legacy_variant", source_key = "1955:46642", birth_year, GEOID = training_geoid(geoid), city, state, kept)]
canonical <- d[source_key == "1955:46642", .(source = "canonical_selected", source_key, birth_year, GEOID, city = NA_character_, state = NA_character_, kept)]
fwrite(rbindlist(list(conflict, canonical)), file.path(stage, "geography_difference_1955_46642.csv"))
coverage_edition <- rbindlist(lapply(birth_training_geographies, function(geo)
  person[, .(geography = geo, entries = .N), by = .(edition, class = get(paste0("class_", geo)))]))
fwrite(coverage_edition, file.path(stage, "coverage_by_edition.csv"))
fwrite(person[, .N, by = .(edition, birth_decade = training_decade(year, "standard_decade"), class_same_county, class_50km, unknown_reason_same_county, unknown_reason_50km)],
       file.path(stage, "coverage_by_edition_cohort.csv"))
role_coverage <- merge(person, units, by = "GEOID", allow.cartesian = TRUE)
fwrite(role_coverage[, .N, by = .(event_id, college, sample_role, g_std, g_shift, edition, class_same_county, class_50km, unknown_reason_same_county, unknown_reason_50km)], file.path(stage, "coverage_by_event_role.csv"))
fwrite(audit[, .N, by = .(edition, exclusion_reason)], file.path(stage, "exclusion_counts.csv"))
file.copy(cfg$units, file.path(stage, basename(cfg$units)), overwrite = TRUE)

for (p in frozen_paths) file.copy(p, file.path(stage, basename(p)), overwrite = TRUE)
for (balance in c("relative", "calendar")) for (timing in names(cfg$panels)) {
  f <- fread(file.path(stage, paste0("frozen_", balance, "_", timing, ".csv")))
  training_require(identical(as.integer(c(uniqueN(f$event_id), uniqueN(f$stack_unit_num), nrow(f))), birth_training_expected(balance, timing)), "Frozen sample dimensions differ")
}
coverage <- list(source_entries = nrow(d), eligible_entries = nrow(person), physical_counties = length(geoids),
  candidate_events = uniqueN(units$event_id), qualifying_awards = nrow(awards), distinct_institutions = nrow(reference),
  groups = coverage_edition)
training_json(coverage, file.path(stage, "coverage_report.json"))
validation <- list(passed = TRUE, one_class_per_entry_per_geography = TRUE, ever_and_never_absorbing = TRUE, class_changes_have_manual_awards = TRUE,
  unknown_resolved = moved,
  count_and_rate_partitions = TRUE, original_panel_columns_unchanged = TRUE, polygon_boundary = "closed",
  centroid_crs = "EPSG:5070", distance_geometry = "S2 great-circle metres", radius_m_inclusive = 50000,
  historical_campus_accuracy_verified = FALSE)
training_json(validation, file.path(stage, "validation_report.json"))
writeLines(c("Birth-origin training outcomes: only scientists born in target county c contribute.",
  "Same-county: institution point inside the cached 2020 county polygon, boundary included.",
  "50km: institution at most 50000 S2 great-circle metres from birth county centroid; no pooled birth counties.",
  "Ever: any assigned local institution. Never: one or more earned awards, every award completely located without conflicts, all locations outside.",
  "Unknown: remaining entries, including absent education and incomplete matching; known local takes precedence.",
  "All earned postsecondary degrees and credentials extracted with multi_v04; board diplomas/certificates excluded as licences; uncertain credential levels remain unresolved; no graduation-year restriction.",
  "Institutions come from education release v05: v04 assignments are kept, and awards v04 left unmatched get the top-500 manual crosswalk ID or an award-level medical-campus override.",
  "Birth years 1830-1959, earlier kept and 1986 US-birth/see-previous rules retained; edition-specific source-key counting, no new deduplication.",
  "Every rate is 1000 times its count divided by unchanged own-county estimated cohort births. Invalid denominators give NA; valid zero counts give zero.",
  "Column names and units are listed in outcome_dictionary.csv. Unknown classification may differ between geographies.",
  "Historical campuses are unverified; reference coordinates and 2020 geometry are used.",
  "1955:46642 keeps canonical GEOID 42015 outside the event pool; legacy GEOID 17113 explains the one-entry discrepancy in decade 1930."), file.path(stage, "DATA_DICTIONARY.txt"))
fwrite(data.table(outcome = names(birth_training_counts), count_column = unname(birth_training_counts),
  rate_column = unname(birth_training_rates), unit = "scientists per 1000 estimated births in birth county"), file.path(stage, "outcome_dictionary.csv"))
dir.create(file.path(stage, "code_snapshot"), showWarnings = FALSE)
for (p in code_files) file.copy(p, file.path(stage, "code_snapshot", basename(p)), overwrite = TRUE)
capture.output(sessionInfo(), file = file.path(stage, "sessionInfo.txt"))
training_json(list(elapsed_seconds = unname(proc.time()[3] - started)), file.path(stage, "timing_report.json"))
training_publish(stage, cfg$output, inputs, list(status = "complete", version = birth_training_stamp, coverage = coverage, validation = validation))
cat("Prepared birth-county training release:", cfg$output, "\n")
print(coverage_edition)
