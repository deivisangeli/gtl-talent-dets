# New 100 km samples; published 50 km runners and inputs remain immutable.
args0 <- commandArgs(FALSE)
script <- normalizePath(sub("^--file=", "", args0[grepl("^--file=", args0)][1]), winslash = "/")
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
for (f in c("amws_training_helpers.R", "amws_catchment_helpers.R", "amws_catchment_no_reuse_helpers.R", "amws_training_calendar_helpers.R")) source(file.path(dirname(script), f))
catchment_tests(); no_reuse_tests(); training_calendar_tests()
training_radius_m <- 100000
training_counts <- gsub("50km", "100km", training_counts)
training_rates <- setNames(paste0(sub("^n_", "", training_counts), "_per_1000_births"), names(training_counts))
root <- training_root()
base <- file.path(root, "output/land_grants/amws_training_100km_geography_20261014")
invisible(training_check_release(base))
read_base <- function(n) fread(file.path(base, n), colClasses = c(GEOID = "character"))
geo <- read_base("geographic_catchment_membership.csv")
sites <- read_base("allocation_sites.csv")
entries <- read_base("entry_classification_and_exclusions.csv")[exclusion_reason == "eligible"]
annual <- read_base("county_year_training_and_denominators.csv")
timings <- c("standard_decade", "alternative_decade")
panels <- setNames(lapply(timings, function(t) read_base(paste0("county_decade_", t, ".csv"))), timings)
for (balance in c("relative", "calendar")) {
  started <- proc.time()[3]
  stamp <- paste0("amws_training_100km_", if (balance == "relative") "catchments" else "calendar", "_no_reuse_20261014")
  old_stamp <- sub("100km", "50km", stamp)
  old <- file.path(root, "output/land_grants", old_stamp)
  old_results <- file.path(root, "results/land_grants/event_studies", old_stamp)
  invisible(training_check_release(old)); invisible(training_check_release(old_results))
  output <- file.path(root, "output/land_grants", stamp)
  if (dir.exists(output)) { invisible(training_check_release(output)); next }
  stage <- file.path(Sys.getenv("USERPROFILE"), ".codex/amws_checkpoints", stamp, "prep_stage")
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  inputs <- training_hashes(c(script, file.path(base, list.files(base, recursive = TRUE)),
    file.path(old, "completion_manifest.json"), file.path(old_results, "completion_manifest.json"),
    file.path(old, "excluded_counties.csv"), file.path(old, paste0("catchment_decade_", timings, ".csv"))))
  prior <- fread(file.path(old, "excluded_counties.csv"), colClasses = c(GEOID = "character"))$GEOID
  frozen <- setNames(lapply(timings, function(t) {
    p <- fread(file.path(old, paste0("catchment_decade_", t, ".csv")), colClasses = c(GEOID = "character"))
    p[, .(site_id, stack_unit_num, GEOID, decade, g, event_id, sample_role, county_only_births_baseline)]
  }), timings)
  candidates <- list(); support_audits <- list()
  if (balance == "calendar") {
    audits <- lapply(panels, training_calendar_audit, outcomes = unname(training_counts))
    complete <- Reduce(intersect, lapply(audits, function(a) a[included == TRUE, GEOID]))
    for (t in timings) {
      support_audits[[t]] <- copy(audits[[t]])[, timing := t]
      candidates[[t]] <- geo[site_id %in% frozen[[t]]$site_id & GEOID %in% complete]
    }
    training_require(setequal(candidates[[1]]$site_id, candidates[[2]]$site_id), "Calendar candidate sites differ")
  } else {
    for (t in timings) {
      m <- geo[site_id %in% frozen[[t]]$site_id]
      baseline_grid <- merge(unique(frozen[[t]][, .(site_id)]), sites[, .(site_id, decade = get(if (t == "standard_decade") "g_std" else "g_shift") - 10L)], by = "site_id")
      grid <- unique(rbindlist(list(frozen[[t]][, .(site_id, decade)], baseline_grid)))
      r <- merge(grid, m[, .(site_id, GEOID, home)], by = "site_id", allow.cartesian = TRUE)
      r <- merge(r, panels[[t]][, .(GEOID, decade, county_births_estimate)], by = c("GEOID", "decade"), all.x = TRUE)
      a <- catchment_supported(r)
      support_audits[[t]] <- copy(a)[, timing := t]
      candidates[[t]] <- merge(m, a[supported == TRUE, .(site_id, GEOID)], by = c("site_id", "GEOID"))
    }
  }
  newly_shared <- lapply(candidates, function(m) m[!GEOID %in% prior, .(events = uniqueN(event_id)), by = GEOID][events > 1L, GEOID])
  excluded <- sort(unique(c(prior, unlist(newly_shared))))
  training_require(length(excluded) == if (balance == "relative") 99L else 39L, "Exclusion preflight differs; estimation blocked")
  ex <- data.table(GEOID = excluded, prior_exclusion = excluded %in% prior)
  ex[, `:=`(shared_standard = GEOID %in% newly_shared[[1]], shared_alternative = GEOID %in% newly_shared[[2]])]
  fwrite(ex, file.path(stage, "excluded_counties.csv"))
  fwrite(rbindlist(support_audits, fill = TRUE), file.path(stage, "denominator_support_audit.csv"))
  coverage <- list(); matching <- list(); membership_sets <- list(); selection <- list()
  for (t in timings) {
    included <- candidates[[t]][!GEOID %in% excluded]
    retained <- training_calendar_events(included, sites)
    included <- included[site_id %in% retained$site_id]
    no_reuse_check_members(included, retained$site_id)
    keys <- frozen[[t]][site_id %in% retained$site_id]
    expected <- if (balance == "calendar") c(14,43,559,368) else if (t == "standard_decade") c(16,51,637,399) else c(20,61,820,464)
    actual <- c(uniqueN(keys$event_id), uniqueN(keys$site_id), nrow(keys), uniqueN(included$GEOID))
    training_require(identical(as.integer(actual), as.integer(expected)), paste("Full denominator preflight differs:", balance, t, paste(actual, collapse = "/")))
    removed_sites <- setdiff(frozen[[t]]$site_id, keys$site_id)
    training_require(if (balance == "calendar") !length(removed_sites) else setequal(removed_sites, "7_17115_runner_up"), "Unexpected empty site")
    audit <- geo[site_id %in% frozen[[t]]$site_id]
    audit[, `:=`(timing = t, denominator_supported = paste(site_id,GEOID) %in% paste(candidates[[t]]$site_id,candidates[[t]]$GEOID), excluded_shared = GEOID %in% excluded, retained_site = site_id %in% keys$site_id)]
    audit[, included := denominator_supported & !excluded_shared & retained_site]
    fwrite(audit, file.path(stage, paste0("catchment_membership_audit_", t, ".csv")))
    fwrite(included, file.path(stage, paste0("catchment_membership_", t, ".csv")))
    membership_sets[[t]] <- included
    rows <- merge(keys[, .(site_id, decade)], included[, .(site_id, GEOID)], by = "site_id", allow.cartesian = TRUE)
    rows <- merge(rows, panels[[t]], by = c("GEOID", "decade"), all.x = TRUE)
    training_require(all(is.finite(rows$county_births_estimate) & rows$county_births_estimate > 0), "Nonpositive pooled denominator")
    p <- rows[, c(lapply(.SD, sum), list(population = sum(population), county_births_estimate = sum(county_births_estimate), member_count = .N,
      annual_years = sum(annual_years), annual_years_with_births = sum(annual_years_with_births))), by = .(site_id, decade), .SDcols = unname(training_counts)]
    for (group in names(training_counts)) p[, (training_rates[[group]]) := training_rate(get(training_counts[[group]]), county_births_estimate)]
    catchment_check_partition(p)
    p <- merge(keys, p, by = c("site_id", "decade"), all.x = TRUE)
    meta <- sites[, .(site_id, treatment_decade = get(if (t == "standard_decade") "g_std" else "g_shift"))]
    b <- merge(meta, p[, .(site_id, decade, county_births_estimate)], by = "site_id", allow.cartesian = TRUE)[decade == treatment_decade - 10L, .(site_id, births_baseline = county_births_estimate)]
    training_require(!anyDuplicated(b$site_id) && nrow(b) == uniqueN(keys$site_id), "Baseline coverage failed")
    p <- merge(p, b, by = "site_id", all.x = TRUE)
    setorder(p, event_id, sample_role, GEOID, decade); setorder(keys, event_id, sample_role, GEOID, decade)
    training_require(identical(p[, names(keys), with = FALSE], keys), "Frozen row order/keys differ")
    fwrite(p, file.path(stage, paste0("catchment_decade_", t, ".csv")))
    fwrite(keys, file.path(stage, paste0("frozen_sample_", t, ".csv")))
    ee <- merge(entries, included[, .(site_id, event_id, sample_role, GEOID, home)], by = "GEOID", allow.cartesian = TRUE)
    ee[, decade := training_decade(year, t)]
    ee <- merge(ee, keys[, .(site_id, decade)], by = c("site_id", "decade"))
    training_require(!anyDuplicated(ee$source_key), "AMWS entry reuse")
    counts <- merge(p[, .(site_id, decade, n_amws_training_total)], ee[, .N, by = .(site_id, decade)], by = c("site_id", "decade"), all.x = TRUE)
    counts[is.na(N), N := 0L]; training_require(all(counts$N == counts$n_amws_training_total), "Entry reconciliation failed")
    fwrite(ee, file.path(stage, paste0("analysis_entry_membership_", t, ".csv")))
    ec <- merge(ee, meta, by = "site_id")
    matching[[t]] <- ec[, .(timing = t, entries = .N, entries_with_unmatched_awards = sum(n_unmatched_awards > 0), unmatched_awards = sum(n_unmatched_awards), partial_awards = sum(n_partial_awards)), by = .(edition, cohort = treatment_decade, sample_role, home, training_class, unknown_reason)]
    coverage[[t]] <- data.table(timing = t, events = actual[1], sites = actual[2], rows = actual[3], analysis_counties = actual[4], excluded_counties = length(excluded), entries_unique = nrow(ee), ever_nearby = sum(ee$training_class == "ever_nearby"), never_nearby = sum(ee$training_class == "never_nearby"), unknown = sum(ee$training_class == "unknown"), cross_event_reused_counties = 0L)
    selection[[t]] <- unique(frozen[[t]][, .(site_id,event_id,sample_role)])[, `:=`(timing = t, retained = site_id %in% keys$site_id)]
    cat(balance, t, "verified", paste(actual, collapse = "/"), "\n")
  }
  if (balance == "calendar") training_require(identical(membership_sets[[1]], membership_sets[[2]]), "Calendar membership differs")
  fwrite(rbindlist(coverage), file.path(stage, "analysis_coverage_summary.csv"))
  fwrite(rbindlist(matching), file.path(stage, "matching_coverage_by_edition_cohort_role.csv"))
  fwrite(rbindlist(selection), file.path(stage, "event_site_selection_audit.csv"))
  validation <- list(passed = TRUE, balance = balance, radius_m = 100000, preflight_dimensions_verified = TRUE, exclusions_frozen_across_timings = TRUE,
    no_county_or_entry_reuse = TRUE, assignments_not_reallocated = TRUE, denominators_baselines_partitions_valid = TRUE,
    source_geography_release = base, historical_campus_accuracy_verified = FALSE)
  training_json(validation, file.path(stage, "validation_report.json"))
  training_json(list(elapsed_seconds = unname(proc.time()[3]-started)), file.path(stage,"timing_report.json"))
  training_publish(stage, output, inputs, list(status = "complete", validation = validation))
}
