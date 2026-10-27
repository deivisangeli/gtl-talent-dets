# Build training proximity outcomes while preserving the frozen Andrews design.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(dirname(script), "amws_training_helpers.R"))
suppressPackageStartupMessages(library(sf))
cfg <- training_config(repo, training_root())
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
                  script, file.path(dirname(script), c("amws_training_helpers.R", "test_amws_training_50km.R")))
training_require(all(file.exists(source_files)), "A frozen input or code file is missing")
inputs <- training_hashes(unique(unname(source_files)))
pm <- jsonlite::read_json(cfg$person_manifest, simplifyVector = TRUE)
training_require(identical(training_hash(cfg$persons), pm$sha256) && pm$rows == 136489L, "Canonical AMWS input differs from manifest")
units <- fread(cfg$units, colClasses = c(GEOID = "character"))
units[, GEOID := training_geoid(GEOID)]
geoids <- sort(unique(units$GEOID))
training_require(uniqueN(units$event_id) == 57L && length(geoids) == 168L, "Frozen event pool differs")
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

annual_counts <- person[, .(n_amws_ever_trained_50km = sum(training_class == "ever_nearby"),
                            n_amws_never_trained_50km = sum(training_class == "never_nearby"),
                            n_amws_training_unknown = sum(training_class == "unknown"),
                            n_amws_training_total = .N), by = .(GEOID, year)]
annual <- merge(CJ(GEOID = geoids, year = 1830:1959), annual_counts, by = c("GEOID", "year"), all.x = TRUE, sort = FALSE)
for (col in unname(training_counts)) set(annual, which(is.na(annual[[col]])), col, 0L)
training_require(all(rowSums(annual[, unname(training_counts[1:3]), with = FALSE]) == annual[[training_counts['total']]]), "Annual classes do not partition total")
fwrite(annual, file.path(stage, "county_year_training_counts.csv"))
reconcile <- list()
for (timing in names(cfg$panels)) {
  base <- fread(cfg$panels[[timing]], colClasses = c(GEOID = "character"))
  base[, GEOID := training_geoid(GEOID)]
  original <- copy(base)
  tmp <- copy(annual)
  tmp[, decade := training_decade(year, timing)]
  counts <- tmp[, lapply(.SD, sum), by = .(GEOID, decade), .SDcols = unname(training_counts)]
  base[, .original_order := .I]
  base <- merge(base, counts, by = c("GEOID", "decade"), all.x = TRUE, sort = FALSE)
  setorder(base, .original_order)
  base[, .original_order := NULL]
  training_require(all(vapply(names(original), function(k) identical(base[[k]], original[[k]]), logical(1))), "Frozen denominator or panel column changed")
  for (group in names(training_counts)) base[, (training_rates[[group]]) := training_rate(get(training_counts[[group]]), county_births_estimate)]
  component_rates <- rowSums(as.matrix(base[, unname(training_rates[1:3]), with = FALSE]))
  training_require(isTRUE(all.equal(component_rates, base[[training_rates[['total']]]], tolerance = 1e-12, check.attributes = FALSE)), "Rate decomposition differs")
  filename <- if (timing == "standard_decade") "county_decade_training.csv" else "county_decade_training_alt.csv"
  fwrite(base, file.path(stage, filename))
  readback <- fread(file.path(stage, filename), colClasses = c(GEOID = "character"))
  training_require(all(vapply(names(original), function(k) identical(readback[[k]], original[[k]]), logical(1))), "Written panel changed a frozen column")
  reconcile[[timing]] <- base[, .(timing = timing, GEOID, decade, legacy_n_amws = n_amws,
                                  updated_n_amws = n_amws_training_total, difference = n_amws_training_total - n_amws)]
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
coverage_edition <- person[, .N, by = .(edition, training_class)]
fwrite(coverage_edition, file.path(stage, "coverage_by_edition.csv"))
fwrite(person[, .N, by = .(edition, birth_decade = training_decade(year, "standard_decade"), training_class, unknown_reason)],
       file.path(stage, "coverage_by_edition_cohort.csv"))
role_coverage <- merge(person, units, by = "GEOID", allow.cartesian = TRUE)
fwrite(role_coverage[, .N, by = .(event_id, college, sample_role, g_std, g_shift, edition, training_class, unknown_reason)], file.path(stage, "coverage_by_event_role.csv"))
fwrite(audit[, .N, by = .(edition, exclusion_reason)], file.path(stage, "exclusion_counts.csv"))
file.copy(cfg$units, file.path(stage, basename(cfg$units)), overwrite = TRUE)
source(file.path(dirname(script), "test_amws_training_50km.R"))
coverage <- list(source_entries = nrow(d), eligible_entries = nrow(person), physical_counties = length(geoids),
                 candidate_events = uniqueN(units$event_id), qualifying_awards = nrow(awards),
                 assigned_institution_references = nrow(institutions), distinct_institutions = nrow(reference),
                 groups = as.list(setNames(vapply(training_groups, function(g) sum(person$training_class == g), integer(1)), training_groups)),
                 unknown_reasons = as.list(table(person[training_class == "unknown", unknown_reason])))
training_json(coverage, file.path(stage, "coverage_report.json"))
validation <- list(passed = TRUE, all_source_entries_audited = nrow(audit), one_class_per_eligible_entry = TRUE,
                   annual_and_decade_count_partitions = TRUE, rate_partitions = TRUE,
                   frozen_columns_preserved_on_disk = TRUE, legacy_differences_identified = TRUE,
                   centroid_crs = "EPSG:5070", distance_geometry = "S2 great-circle metres", radius_m_inclusive = training_radius_m,
                   historical_campus_accuracy_verified = FALSE)
training_json(validation, file.path(stage, "validation_report.json"))
writeLines(c("Training-location outcomes for Andrews allocation events.",
  "One row per existing eligible edition-specific entry; existing earlier kept and see-previous rules preserved.",
  "Ever: any assigned institution <=50000 metres from geometric birth-county centroid.",
  "Never: at least one award, all reported awards fully located without conflicts, all distances >50000 metres.",
  "Unknown: all other entries, including incomplete matching; unconditional undated locations remain usable.",
  "BA/BS, MA, MS and PhD included. Institutions are current OpenAlex reference locations, not validated historical campuses.",
  "Counts partition updated total. Rates use unchanged timing-specific county births; invalid denominators remain NA.",
  "No geocoding corrections: 1955:46642 retains canonical GEOID 42015, versus legacy eligible GEOID 17113."), file.path(stage, "DATA_DICTIONARY.txt"))
dir.create(file.path(stage, "code_snapshot"), showWarnings = FALSE)
for (p in c(script, file.path(dirname(script), c("amws_training_helpers.R", "test_amws_training_50km.R")))) file.copy(p, file.path(stage, "code_snapshot", basename(p)), overwrite = TRUE)
capture.output(sessionInfo(), file = file.path(stage, "sessionInfo.txt"))
training_json(list(elapsed_seconds = unname(proc.time()[3] - started)), file.path(stage, "timing_report.json"))
training_publish(stage, cfg$output, inputs, list(status = "complete", version = training_stamp, coverage = coverage, validation = validation))
cat("Prepared training outcome release:", cfg$output, "\n")
print(coverage_edition)
