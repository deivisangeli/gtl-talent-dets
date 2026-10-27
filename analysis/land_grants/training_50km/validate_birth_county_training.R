# Reconstruct from canonical eligibility/award flags and independently calculated geography.
aa <- commandArgs(FALSE)
script <- normalizePath(sub("^--file=", "", aa[grepl("^--file=", aa)][1]), winslash = "/")
repo <- normalizePath(file.path(dirname(script), "../../.."), winslash = "/")
for (f in c("amws_training_helpers.R", "amws_birth_county_training_helpers.R")) source(file.path(repo, "prep/land_grants", f))
suppressPackageStartupMessages(library(sf))
setDTthreads(4)
root <- training_root()
cfg <- training_config(repo, root)
prep <- file.path(root, "output/land_grants", birth_training_stamp)
res <- file.path(root, "results/land_grants/event_studies", birth_training_stamp)
invisible(training_check_release(prep))
invisible(training_check_release(res))
person <- fread(file.path(prep, "entry_classification_and_exclusions.csv"), colClasses = c(GEOID = "character"))
p <- person[exclusion_reason == "eligible"]
stopifnot(nrow(person) == 136489L, !anyDuplicated(person$source_key), nrow(p) == 12360L)

# Read eligibility and every award's matching evidence directly from the canonical source.
header <- names(fread(cfg$persons, nrows = 0L))
prefixes <- sub("award_index$", "", grep("^(ba_bs|ma|ms|phd)_award_[0-9]+_award_index$", header, value = TRUE))
degree_cols <- grep("^(ba_bs|ma|ms|phd)_award_[0-9]+_(award_index|fully_matched|condition_conflict|oa_id_count|institution_[0-9]+_(oa_id|latitude|longitude))$", header, value = TRUE)
d <- fread(cfg$persons, select = c("source_key", "birth_year", "geoid", "geo_geoid", "kept", "is_us_birth", "birth_country", "raw_text_adjusted", degree_cols), colClasses = "character", na.strings = NULL)
d[, edition := as.integer(sub(":.*", "", source_key))]
d[, `:=`(year = suppressWarnings(as.integer(birth_year)), GEOID = training_geoid(ifelse(edition == 1986L, geo_geoid, geoid)))]
units <- fread(cfg$units, colClasses = c(GEOID = "character"))
birth_pos <- regexpr("(^|[[:space:],.;])b[[:space:]]+", d$raw_text_adjusted, ignore.case = TRUE, perl = TRUE)
see_pos <- regexpr("\\bsee\\s+prev(?:ious)?(?:\\s+edition)?\\b", d$raw_text_adjusted, ignore.case = TRUE, perl = TRUE)
contaminated <- !is.na(see_pos) & see_pos > 0 & (is.na(birth_pos) | birth_pos < 0 | birth_pos > see_pos)
eligible <- !is.na(d$year) & d$year >= 1830L & d$year <= 1959L &
  ifelse(d$edition == 1986L, training_true(d$is_us_birth) & d$birth_country == "USA" & !contaminated, training_true(d$kept)) &
  !is.na(d$GEOID) & d$GEOID %in% units$GEOID
eligible[is.na(eligible)] <- FALSE
d <- d[eligible]
setorder(d, source_key)
setorder(p, source_key)
stopifnot(identical(d$source_key, p$source_key), identical(d$GEOID, p$GEOID), identical(d$year, p$year))

award_rows <- rbindlist(lapply(prefixes, function(prefix) {
  rows <- which(training_has(d[[paste0(prefix, "award_index")]]))
  if (!length(rows)) return(NULL)
  data.table(source_key = d$source_key[rows], award_index = as.integer(d[[paste0(prefix, "award_index")]][rows]),
    complete = training_true(d[[paste0(prefix, "fully_matched")]][rows]),
    conflict = training_true(d[[paste0(prefix, "condition_conflict")]][rows]),
    n_ids = as.integer(d[[paste0(prefix, "oa_id_count")]][rows]))
}))
raw_locations <- rbindlist(lapply(prefixes, function(prefix) rbindlist(lapply(1:2, function(k) {
  q <- paste0(prefix, "institution_", k, "_")
  rows <- which(training_has(d[[paste0(prefix, "award_index")]]) & training_has(d[[paste0(q, "oa_id")]]))
  if (!length(rows)) return(NULL)
  data.table(source_key = d$source_key[rows], award_index = as.integer(d[[paste0(prefix, "award_index")]][rows]),
    openalex_id = d[[paste0(q, "oa_id")]][rows], latitude = as.numeric(d[[paste0(q, "latitude")]][rows]),
    longitude = as.numeric(d[[paste0(q, "longitude")]][rows]), GEOID = d$GEOID[rows])
}))))
ins <- fread(file.path(prep, "assigned_institution_distances.csv"), colClasses = c(GEOID = "character"))
keys <- c("source_key", "award_index", "openalex_id")
setorderv(raw_locations, keys)
setorderv(ins, keys)
stopifnot(isTRUE(all.equal(raw_locations[, names(raw_locations), with = FALSE], ins[, names(raw_locations), with = FALSE], tolerance = 1e-12)))
valid <- raw_locations[, .(located = sum(is.finite(latitude) & abs(latitude) <= 90 & is.finite(longitude) & abs(longitude) <= 180), assigned = .N), by = .(source_key, award_index)]
aw <- merge(award_rows, valid, by = c("source_key", "award_index"), all.x = TRUE)
aw[is.na(located), `:=`(located = 0L, assigned = 0L)]
stopifnot(all(aw$n_ids == aw$assigned), !anyDuplicated(aw[, .(source_key, award_index)]))
aw[, complete := complete & !conflict & n_ids > 0L & located == n_ids]
complete_keys <- aw[, .(complete = all(complete)), by = source_key][complete == TRUE, source_key]

# Reverse polygon/point predicate, separate from the builder's point intersections.
sf_use_s2(TRUE)
shape <- st_read(cfg$shape, quiet = TRUE)
gids <- sort(unique(units$GEOID))
counties <- st_transform(shape[match(gids, shape$GEOID), ], 4326)
refs <- unique(raw_locations[, .(openalex_id, latitude, longitude)])
setorder(refs, openalex_id)
pts <- st_as_sf(as.data.frame(refs), coords = c("longitude", "latitude"), crs = 4326)
covers <- st_covers(counties, pts, model = "closed")
covered_pairs <- rbindlist(lapply(seq_along(gids), function(j)
  data.table(GEOID = rep(gids[j], length(covers[[j]])), openalex_id = refs$openalex_id[covers[[j]]])))
inside_key <- paste(covered_pairs$GEOID, covered_pairs$openalex_id)
same <- paste(ins$GEOID, ins$openalex_id) %in% inside_key
stopifnot(identical(same, ins$same_county))
centroids <- st_transform(st_centroid(st_geometry(st_transform(counties, 5070))), 4326)
xy <- st_coordinates(centroids)
exported <- fread(file.path(prep, "county_centroids.csv"), colClasses = c(GEOID = "character"))
stopifnot(identical(exported$GEOID, gids), max(abs(xy[,1] - exported$longitude)) < 1e-12, max(abs(xy[,2] - exported$latitude)) < 1e-12)
individual_points <- st_as_sf(as.data.frame(ins), coords = c("longitude", "latitude"), crs = 4326)
metres <- as.numeric(st_distance(individual_points, centroids[match(ins$GEOID, gids)], by_element = TRUE))
stopifnot(all(is.finite(metres)), max(abs(metres - ins$distance_m)) < 1e-7)
for (geo in birth_training_geographies) {
  local <- if (geo == "same_county") same else metres <= 50000
  ever_keys <- unique(ins$source_key[local])
  expected <- ifelse(p$source_key %in% ever_keys, "ever", ifelse(p$source_key %in% complete_keys, "never", "unknown"))
  stopifnot(identical(expected, p[[paste0("class_", geo)]]))
}
old <- fread(file.path(cfg$output, "entry_classification_and_exclusions.csv"), colClasses = c(GEOID = "character"))[exclusion_reason == "eligible"]
setorder(old, source_key)
stopifnot(identical(old$source_key, p$source_key), identical(sub("_nearby$", "", old$training_class), p$class_50km))

# Reconstruct each count directly from the independently verified individual records.
check_counts <- function(panel, time_col, timing = NULL) {
  z <- copy(p)
  if (time_col == "decade") z[, decade := if (timing == "standard_decade") 10L * (year %/% 10L) else 10L * (year %/% 10L) + 10L * (year %% 10L >= 7L)]
  for (key in names(birth_training_counts)) {
    sample <- if (key == "total") z else z[get(paste0("class_", sub("__.*", "", key))) == sub(".*__", "", key)]
    expected <- sample[, .(expected = .N), by = c("GEOID", time_col)]
    q <- merge(panel[, c("GEOID", time_col, birth_training_counts[[key]]), with = FALSE], expected, by = c("GEOID", time_col), all.x = TRUE)
    q[is.na(expected), expected := 0L]
    stopifnot(nrow(q) == nrow(panel), all(q[[birth_training_counts[[key]]]] == q$expected))
    if (time_col == "decade") {
      wanted <- ifelse(is.finite(panel$county_births_estimate) & panel$county_births_estimate > 0,
        1000 * panel[[birth_training_counts[[key]]]] / panel$county_births_estimate, NA_real_)
      stopifnot(isTRUE(all.equal(panel[[birth_training_rates[[key]]]], wanted, tolerance = 1e-12)))
    }
  }
  birth_training_partition(panel)
}
annual <- fread(file.path(prep, "county_year_training_counts.csv"), colClasses = c(GEOID = "character"))
check_counts(annual, "year")
status <- fread(file.path(res, "model_status.csv"))
dynamic <- fread(file.path(res, "dynamic_att.csv"))
simple <- fread(file.path(res, "post_treatment_att.csv"))
members <- fread(file.path(res, "estimation_sample_membership.csv"), colClasses = c(GEOID = "character"))
stopifnot(nrow(status) == 28L, all(status$ok), all(is.finite(dynamic$estimate)), all(is.finite(dynamic$se) & dynamic$se >= 0))
for (timing in names(cfg$panels)) {
  panel <- fread(file.path(prep, paste0("county_decade_", timing, ".csv")), colClasses = c(GEOID = "character"))
  original <- fread(cfg$panels[[timing]], colClasses = c(GEOID = "character"))
  stopifnot(identical(panel[, names(original), with = FALSE], original))
  check_counts(panel, "decade", timing)
  for (balance in c("relative", "calendar")) {
    frozen <- fread(file.path(prep, paste0("frozen_", balance, "_", timing, ".csv")), colClasses = c(GEOID = "character"))
    preceding <- fread(file.path(root, "output/land_grants/amws_county_training_location_20261014", paste0("frozen_", balance, "_", timing, ".csv")), colClasses = c(GEOID = "character"))
    actual <- fread(file.path(res, paste0("analysis_panel_", balance, "_", timing, ".csv")), colClasses = c(GEOID = "character"))
    stopifnot(identical(frozen, preceding), identical(actual[, names(frozen), with = FALSE], frozen),
      identical(as.integer(c(uniqueN(frozen$event_id), uniqueN(frozen$stack_unit_num), nrow(frozen))), birth_training_expected(balance, timing)))
    check_counts(actual, "decade", timing)
    # Controls must remain own-county estimated births at the allocation cohort minus 10.
    base <- merge(unique(units[, .(GEOID, g0 = get(if (timing == "standard_decade") "g_std" else "g_shift"))]),
      panel[, .(GEOID, decade, county_births_estimate)], by = "GEOID", allow.cartesian = TRUE)[decade == g0 - 10L]
    bc <- merge(actual[, .(GEOID, x)], base[, .(GEOID, expected = county_births_estimate)], by = "GEOID", all.x = TRUE)
    stopifnot(isTRUE(all.equal(bc$x, bc$expected, tolerance = 1e-12)))
    models <- list()
    for (group in names(birth_training_rates)) {
      wanted <- paste(balance, timing, group, sep = "__")
      mm <- readRDS(file.path(res, "models", paste0(wanted, ".rds")))
      models[[group]] <- mm
      model_cols <- c("stack_unit_num", "GEOID", "decade", "g", "event_id", "sample_role", "x")
      expected <- members[model == wanted, model_cols, with = FALSE]
      stopifnot(identical(expected, frozen[, model_cols, with = FALSE]), isTRUE(mm$ok))
      params <- mm$att$DIDparams
      stopifnot(params$est_method == "reg", params$control_group == "nevertreated", params$base_period == "universal")
      # Check the actual data retained inside did, including the dependent variable.
      fitted <- as.data.table(params$data)[, .(stack_unit_num, decade, fitted_g = g, fitted_y = y, fitted_x = x)]
      check <- merge(actual[, .(stack_unit_num, decade, g, y = get(birth_training_rates[[group]]), x)],
        fitted, by = c("stack_unit_num", "decade"), all = TRUE)
      # did internally encodes never-treated g=0 as infinity.
      stopifnot(nrow(check) == nrow(actual), !anyNA(check), all(ifelse(check$g == 0, Inf, check$g) == check$fitted_g),
        max(abs(check$y - check$fitted_y)) < 1e-12, max(abs(check$x - check$fitted_x)) < 1e-10)
      pub <- dynamic[model == wanted]
      ref <- which(mm$dynamic$egt == 0L)
      shifted_if <- mm$dynamic$inf.function$dynamic.inf.func.e - mm$dynamic$inf.function$dynamic.inf.func.e[, ref]
      expected_se <- sqrt(colMeans(shifted_if^2) / nrow(shifted_if))
      expected_att <- mm$dynamic$att.egt - mm$dynamic$att.egt[ref]
      stopifnot(isTRUE(all.equal(pub$event_time, mm$dynamic$egt)), max(abs(pub$estimate - expected_att)) < 1e-10,
        max(abs(pub$se - expected_se)) < 1e-10, max(abs(pub$ci_low - (expected_att - 1.96 * expected_se))) < 1e-10,
        max(abs(pub$ci_high - (expected_att + 1.96 * expected_se))) < 1e-10)
      stopifnot(abs(simple[model == wanted, estimate] - mm$simple$overall.att) < 1e-10)
    }
    for (geo in birth_training_geographies) {
      parts <- models[paste(geo, birth_training_groups, sep = "__")]
      # Add estimates and influence functions; uncertainty itself is not additive.
      total <- models[['total']]
      stopifnot(max(abs(Reduce(`+`, lapply(parts, function(m) m$renormalized$estimate)) - total$renormalized$estimate)) < 1e-8,
        max(abs(Reduce(`+`, lapply(parts, function(m) m$dynamic$inf.function$dynamic.inf.func.e)) - total$dynamic$inf.function$dynamic.inf.func.e)) < 1e-7)
    }
  }
}
stopifnot(nrow(dynamic[event_time == 0]) == 28L, all(dynamic[event_time == 0, estimate] == 0), all(dynamic[event_time == 0, se] == 0))
for (balance in c("relative", "calendar")) for (timing in names(cfg$panels)) for (ext in c("png", "pdf")) {
  path <- file.path(res, paste0("birth_county_training_", balance, "_", timing, ".", ext))
  stopifnot(file.exists(path), file.info(path)$size > 1000L)
}
# Verify the prior birth-origin and destination releases still have their recorded checksums.
for (stamp in c("amws_training_50km_20261014", "amws_county_training_location_20261014")) {
  for (branch in c("output/land_grants", "results/land_grants/event_studies")) invisible(training_check_release(file.path(root, branch, stamp)))
}
report <- list(passed = TRUE, source_entries = nrow(person), eligible_entries = nrow(p),
  independently_validated_classifications = 2L * nrow(p), independent_source_awards_and_locations = TRUE,
  geographic_predicates_and_distances_recomputed = TRUE, annual_and_decade_counts_reconstructed = TRUE,
  own_county_birth_denominators_and_frozen_samples_preserved = TRUE, models = nrow(status),
  influence_functions_and_intervals_reconstructed = TRUE, previous_50km_reproduced = TRUE,
  actual_estimator_inputs_verified = TRUE,
  previous_releases_unchanged = TRUE, figures = 4L, validator_sha256 = training_hash(script),
  prep_manifest_sha256 = training_hash(file.path(prep, "completion_manifest.json")),
  results_manifest_sha256 = training_hash(file.path(res, "completion_manifest.json")))
audit_dir <- paste0(res, "_validation")
dir.create(audit_dir, showWarnings = FALSE)
training_json(report, file.path(audit_dir, "independent_validation.json"))
file.copy(script, file.path(audit_dir, basename(script)), overwrite = TRUE)
print(report)
