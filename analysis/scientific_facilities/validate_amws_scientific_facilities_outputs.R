# Independent checks of saved artifacts against raw annual observations.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE)
if (length(args) != 3L) stop("Usage: Rscript validate_amws_scientific_facilities_outputs.R annual_dir decennial_dir_or_- annual_input_csv")
input <- fread(args[3], colClasses = c(GEOID = "character"))
input[, GEOID := sprintf("%05d", as.integer(GEOID))]
outcomes <- c("n_amws", "log1p_n_amws", "amws_per_100k", "amws_per_1000_births", "population")
for (frequency in c("annual", "decennial")) {
  root <- args[if (frequency == "annual") 1L else 2L]
  if (root == "-") next
  read <- function(name) {
    x <- fread(file.path(root, paste0(name, ".csv")))
    if ("GEOID" %in% names(x)) x[, GEOID := sprintf("%05d", as.integer(GEOID))]
    x
  }
  report <- list()
  check <- function(name, valid) {
    report[[name]] <<- data.table(check = name, passed = isTRUE(valid))
    if (!isTRUE(valid)) stop("Failed ", frequency, " artifact check: ", name)
  }
  manifest <- read("run_manifest")
  setting <- function(name) as.integer(manifest[metric == name, value])
  target_cutoff <- setting("target_cutoff")
  last_year <- setting("last_year")
  balanced_post <- setting("balanced_post")
  specs <- read("model_status")
  check("run_complete", manifest[metric == "status", value] == "complete")
  expected_count <- if ("expected_figures" %in% manifest$metric) setting("expected_figures") else
    uniqueN(specs$outcome) * 4L * if (frequency == "decennial") 2L else 1L
  check("all_specifications_estimated", nrow(specs) == expected_count)
  if ("outcomes" %in% manifest$metric) check("requested_outcomes_only",
    setequal(specs$outcome, strsplit(manifest[metric == "outcomes", value], ";", fixed = TRUE)[[1]]))
  check("no_failed_specifications", all(specs$status == "estimated"))
  figs <- list.files(root, pattern = "\\.png$", full.names = TRUE)
  check("all_figures_nonempty", length(figs) == nrow(specs) && all(file.info(figs)$size > 0))
  cfg <- c("outcome", "control_group", "balance", if (frequency == "decennial") "timing")
  dynamic <- read("dynamic_att"); cells <- read("cohort_time_att")
  summary <- read("post_att_summary"); annotation <- read("plot_annotation_summary")
  baseline <- read("plot_baseline_observations"); participants <- read("county_contributions")
  assignment <- read("treatment_assignment"); membership <- read("cohort_membership")
  check("annotation_for_every_figure", nrow(unique(annotation, by = cfg)) == nrow(specs))
  ref <- if (frequency == "annual") -1L else -10L
  check("reference_normalized", all(dynamic[event_time == ref, estimate] == 0))
  check("observation_cutoff", all(cells$year <= if (frequency == "annual") 1949L else 1940L))
  check("maximum_supported_horizon", max(dynamic$event_time) == if (frequency == "annual") 73L else 70L)
  check("baseline_deduplication", !anyDuplicated(baseline[, c(cfg, "id", "base_year"), with = FALSE]))
  calculated <- baseline[, .(baseline_mean_calculated = mean(y), n_calculated = .N), by = cfg]
  compared <- merge(annotation, calculated, by = cfg)
  check("pooled_baseline_means", all(abs(compared$baseline_mean - compared$baseline_mean_calculated) < 1e-8))
  check("baseline_observation_counts", all(compared$baseline_observations == compared$n_calculated))
  for (role in c("treated", "control")) {
    counts <- participants[participant_role == role, .(actual = uniqueN(id)), by = cfg]
    compared <- merge(annotation, counts, by = cfg)
    check(paste0(role, "_county_counts"), all(compared[[paste0("n_", role, "_counties")]] == compared$actual))
  }
  treated <- merge(participants[participant_role == "treated"], assignment[, .(GEOID, first_exposure_year)], by = "GEOID")
  check("donor_only_excluded_from_targets", all(treated$first_exposure_year <= target_cutoff))
  check("exact_target_assignment", all(assignment[role == "target", first_exposure_year] <= target_cutoff) &&
    all(assignment[role == "donor_only", first_exposure_year] > target_cutoff &
      assignment[role == "donor_only", first_exposure_year] <= last_year))
  never <- merge(participants[participant_role == "control" & control_group == "nevertreated"],
    assignment[, .(GEOID, first_exposure_year)], by = "GEOID")
  check("never_controls_unexposed", all(is.na(never$first_exposure_year) | never$first_exposure_year > 1949))
  pooled <- cells[, .(calculated = sum(att * cohort_weight)), by = c(cfg, "event_time")]
  compared <- merge(dynamic, pooled, by = c(cfg, "event_time"))
  check("dynamic_cohort_weights", all(abs(compared$estimate - compared$calculated) < 1e-8))
  for (j in seq_len(nrow(summary))) {
    row <- summary[j]
    part <- cells[row, on = cfg, nomatch = 0][event_time >= row$post_min & event_time <= row$post_max]
    check(paste0("post_summary_", j), abs(weighted.mean(part$att, part$n_counties) - row$estimate) < 1e-8)
  }
  included <- membership[balance == "balanced" & included == TRUE, .(count = .N), by = cfg]
  compared <- merge(dynamic[balance == "balanced"], included, by = cfg)
  check("fixed_balanced_cohorts", all(compared$n_cohorts == compared$count))
  expected_balanced_grid <- if (frequency == "annual") seq(-setting("pre"), balanced_post) else
    seq(-10L, balanced_post, by = 10L)
  check("balanced_window", setequal(dynamic[balance == "balanced", event_time], expected_balanced_grid))
  retained <- membership[balance == "balanced" & included == TRUE]
  check("balanced_cohorts_complete", all(retained$full_window_estimable))
  composition <- cells[balance == "balanced", .(cohorts = paste(sort(unique(g)), collapse = ";")),
    by = c(cfg, "event_time")]
  check("balanced_cohort_identities_fixed", all(composition[, .(sets = uniqueN(cohorts)), by = cfg]$sets == 1L))
  if (frequency == "annual") {
    check("balanced_1943_excluded", !any(membership[balance == "balanced" & included == TRUE, g] == 1943))
    source <- melt(input, id.vars = c("GEOID", "year"), measure.vars = outcomes,
      variable.name = "outcome", value.name = "source_y")
    setnames(source, "year", "base_year")
    if (file.exists(file.path(root, "support_estimation.csv"))) {
      support <- read("support_estimation")
      check("annual_future_donors_leave_on_exposure", all(is.na(support$earliest_control_exposure) |
        support$earliest_control_exposure > support$control_exposure_boundary))
      check("annual_exact_year_timing", all(cells$year == cells$g + cells$event_time) &&
        all(cells$base_year == cells$g - 1L))
      links <- read("county_facility_exposure_links")
      first <- links[, .(calculated_first = min(year)), by = GEOID]
      compared_dates <- merge(assignment, first, by = "GEOID", all.x = TRUE)
      check("earliest_exposure_from_full_list", all(
        is.na(compared_dates$first_exposure_year) == is.na(compared_dates$calculated_first)) &&
        all(compared_dates$first_exposure_year == compared_dates$calculated_first, na.rm = TRUE))
      check("annual_common_post_summaries", all(summary[post_max == balanced_post,
        .(n = .N), by = cfg]$n == 1L) && nrow(summary[post_max == balanced_post]) == nrow(specs))
      check("annual_full_horizon_summaries", all(summary[balance == "unbalanced" & post_max != balanced_post,
        post_max] == max(dynamic[balance == "unbalanced", event_time])))
      # Independently reconstruct donor availability from source outcomes and
      # exact full-list exposure dates, rather than using model cohort codes.
      obs <- merge(input[year >= setting("first_year") & year <= last_year],
        assignment[, .(GEOID, first_exposure_year)], by = "GEOID")
      for (o in unique(support$outcome)) {
        usable <- obs[is.finite(get(o))]
        if (o == "population" || o == "amws_per_100k") usable <- usable[is.finite(population) & population > 0]
        if (o == "amws_per_1000_births") usable <- usable[
          is.finite(county_births_estimate_year) & county_births_estimate_year > 0]
        periods <- split(usable, usable$year)
        actual <- support[outcome == o & year >= setting("first_year") & year <= last_year]
        correct <- vapply(seq_len(nrow(actual)), function(j) {
          row <- actual[j]
          boundary <- if (row$control_group == "nevertreated") last_year else max(row$year, row$base_year)
          b <- periods[[as.character(row$base_year)]]
          t <- periods[[as.character(row$year)]]
          eligible <- function(x) if (is.null(x)) character() else
            x[is.na(first_exposure_year) |
              (first_exposure_year != row$g & first_exposure_year > boundary), GEOID]
          cb <- eligible(b); ct <- eligible(t)
          length(cb) == row$control_base_counties && length(ct) == row$control_year_counties &&
            length(intersect(cb, ct)) == row$control_paired_counties
        }, logical(1))
        check(paste0("annual_donor_counts_from_source_", o), all(correct))
      }
    }
  } else {
    check("balanced_1940_excluded", !any(membership[balance == "balanced" & included == TRUE, g] == 1940))
    donors <- read("donor_availability")
    donors[, boundary := fifelse(control_group == "nevertreated", 1949,
      pmax(g - 10, g + event_time) + 9)]
    check("future_donors_leave_before_opening_block", all(is.na(donors$first_exposure_year) |
      donors$first_exposure_year > donors$boundary))
    raw <- input[year >= 1860 & year <= 1949]
    raw[, decade := year %/% 10 * 10]
    blocks <- raw[, .(rows = .N, count = sum(n_amws), pop = mean(population),
      births = sum(county_births_estimate_year)), by = .(GEOID, decade)]
    blocks[rows == 10L, `:=`(n_amws = count, population = pop, log1p_n_amws = log1p(count),
      amws_per_100k = 100000 * count / pop, amws_per_1000_births = 1000 * count / births)]
    saved <- read("county_decade_outcomes")
    compared <- merge(saved, blocks, by = c("GEOID", "decade"), suffixes = c("", "_calculated"))
    for (o in outcomes) {
      valid <- is.finite(compared[[o]])
      check(paste0("decade_reconciliation_", o), all(compared$rows[valid] == 10) &&
        all(abs(compared[[o]][valid] - compared[[paste0(o, "_calculated")]][valid]) < 1e-7))
    }
    check("incomplete_blocks_remain_missing", all(is.na(saved[annual_rows != 10L, n_amws])))
    source <- melt(blocks, id.vars = c("GEOID", "decade"), measure.vars = outcomes,
      variable.name = "outcome", value.name = "source_y")
    setnames(source, "decade", "base_year")
  }
  compared <- merge(baseline, source, by = c("GEOID", "base_year", "outcome"), all.x = TRUE)
  check("baseline_matches_source_outcomes", all(is.finite(compared$source_y)) &&
    all(abs(compared$y - compared$source_y) < 1e-7))
  fwrite(rbindlist(report), file.path(root, "validation_report.csv"))
  cat(frequency, ":", length(report), "saved-output checks passed.\n")
}
