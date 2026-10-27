# Catchment-specific rules; the frozen county-only helpers remain unchanged.
catchment_stamp <- "amws_training_50km_catchments_20261014"
catchment_config <- function(repo, root) {
  x <- training_config(repo, root)
  x$previous_output <- x$output
  x$previous_results <- x$results
  x$output <- file.path(root, "output", "land_grants", catchment_stamp)
  x$results <- file.path(root, "results", "land_grants", "event_studies", catchment_stamp)
  x$local <- file.path(Sys.getenv("AMWS_CHECKPOINT_ROOT", unset = file.path(Sys.getenv("USERPROFILE"), ".codex", "amws_checkpoints")), catchment_stamp)
  x$pairs <- file.path(root, "raw", "andrews_2023_county_pairs_long.xlsx")
  x$population <- file.path(root, "output", "county_population.csv")
  x$birth_rates <- file.path(root, "output", "us_panel_county_stem_year_1800.csv")
  x
}
catchment_assign <- function(candidates, radius = training_radius_m) {
  x <- copy(candidates)[home | (is.finite(distance_m) & distance_m <= radius)]
  training_require(all(is.finite(x$distance_m) & x$distance_m >= 0), "Invalid site distance")
  training_require(!anyDuplicated(x[, .(event_id, GEOID, site_id)]), "Duplicate candidate membership")
  training_require(!any(x[home == TRUE, .N, by = .(event_id, GEOID)]$N > 1L), "Multiple original sites own one county")
  setorderv(x, c("event_id", "GEOID", "home", "distance_m", "site_id"), c(1, 1, -1, 1, 1))
  x[, head(.SD, 1L), by = .(event_id, GEOID)]
}
catchment_supported <- function(rows) {
  rows[, .(supported = all(is.finite(county_births_estimate) & county_births_estimate > 0)), by = .(site_id, GEOID, home)]
}
catchment_check_partition <- function(d) {
  training_require(all(rowSums(d[, unname(training_counts[1:3]), with = FALSE]) == d$n_amws_training_total), "Count partition failed")
  sums <- rowSums(d[, unname(training_rates[1:3]), with = FALSE])
  training_require(isTRUE(all.equal(sums, d[[training_rates[['total']]]], tolerance = 1e-12, check.attributes = FALSE)), "Rate partition failed")
}
catchment_tests <- function() {
  x <- data.table(event_id = c(1L,1L,1L,1L,2L,2L), GEOID = c("a","a","b","b","a","c"),
    site_id = c("s1","s2","s1","s2","s3","s3"), home = c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE),
    distance_m = c(60000,1,50000,50000,50000,50000.001))
  y <- catchment_assign(x)
  stopifnot(y[event_id == 1L & GEOID == "a", site_id] == "s1", y[GEOID == "b", site_id] == "s1",
            nrow(y[GEOID == "a"]) == 2L, !"c" %in% y$GEOID, !anyDuplicated(y[, .(event_id,GEOID)]))
  x[site_id == "s2" & GEOID == "b", distance_m := 49999]
  stopifnot(catchment_assign(x)[GEOID == "b", site_id] == "s2")
  stopifnot(identical(catchment_assign(x), catchment_assign(x[nrow(x):1L])))
  support <- data.table(site_id = c("s1","s1","s1","s1"), GEOID = c("a","a","b","b"), home = c(TRUE,TRUE,FALSE,FALSE),
                        county_births_estimate = c(1,2,1,NA_real_))
  z <- catchment_supported(support)
  stopifnot(z[GEOID == "a", supported], !z[GEOID == "b", supported])
  # Ratio of sums is not an unweighted average of member-county rates.
  stopifnot(training_rate(sum(c(1,3)),sum(c(100,900))) == 4,
            mean(training_rate(c(1,3),c(100,900))) != 4)
  cat("Catchment tests passed: boundary, home priority, nearest site, ties, controls, cross-event reuse, deterministic partition, fixed support and pooled rates.\n")
  invisible(TRUE)
}
