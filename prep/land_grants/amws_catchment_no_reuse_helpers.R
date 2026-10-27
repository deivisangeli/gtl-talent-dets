# Exclude the seven counties reused in the published catchment estimation samples.
no_reuse_stamp <- "amws_training_50km_catchments_no_reuse_20261014"
no_reuse_config <- function(repo, root) {
  x <- catchment_config(repo, root)
  x$base_output <- x$output
  x$base_results <- x$results
  x$output <- file.path(root, "output", "land_grants", no_reuse_stamp)
  x$results <- file.path(root, "results", "land_grants", "event_studies", no_reuse_stamp)
  x$local <- file.path(Sys.getenv("AMWS_CHECKPOINT_ROOT", unset = file.path(Sys.getenv("USERPROFILE"), ".codex", "amws_checkpoints")), no_reuse_stamp)
  x
}
no_reuse_filter <- function(members, excluded) {
  members[supported == TRUE & !GEOID %in% excluded]
}
no_reuse_check_members <- function(included, original_sites) {
  training_require(setequal(included$site_id, original_sites), "County exclusions empty an original site; sample must not change silently")
  training_require(!anyDuplicated(included[, .(event_id, GEOID)]) &&
    all(included[, .(events = uniqueN(event_id)), by = GEOID]$events == 1L), "A county still contributes to multiple events")
}
no_reuse_tests <- function() {
  x <- data.table(site_id = c("a", "b", "a", "b", "a"), event_id = c(1L, 2L, 1L, 2L, 1L),
    GEOID = c("shared", "shared", "one", "two", "unsupported"), home = c(TRUE,FALSE,FALSE,TRUE,FALSE),
    supported = c(TRUE,TRUE,TRUE,TRUE,FALSE))
  y <- no_reuse_filter(x, "shared")
  stopifnot(setequal(y$GEOID, c("one", "two")), !"shared" %in% y$GEOID)
  no_reuse_check_members(y, c("a", "b"))
  failed <- tryCatch({no_reuse_check_members(y[site_id == "a"], c("a", "b")); FALSE}, error = function(e) TRUE)
  stopifnot(failed, identical(as.data.frame(no_reuse_filter(y, "shared")), as.data.frame(y)))
  cat("No-reuse tests passed: exclude home and added memberships; retain support filters; reject empty sites; idempotent exclusion.\n")
  invisible(TRUE)
}
