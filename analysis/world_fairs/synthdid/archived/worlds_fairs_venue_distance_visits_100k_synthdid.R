###############################################################################
# Project: GTL Talent Determinants
# Goal: Stacked synthetic DiD for world's-fairs venue-distance treatment,
#       restricted to fairs with at least 100,000 visits.
#
# Run from analysis/ or repo root:
#   Rscript analysis/world_fairs/synthdid/archived/worlds_fairs_venue_distance_visits_100k_synthdid.R
###############################################################################

rm(list = ls())

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
  if (basename(repo_root) == "archived" &&
      basename(dirname(repo_root)) == "synthdid" &&
      basename(dirname(dirname(repo_root))) == "world_fairs" &&
      basename(dirname(dirname(dirname(repo_root)))) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, "..", "..", "..", ".."), winslash = "/", mustWork = TRUE)
  }
  if (basename(repo_root) == "synthdid" &&
      basename(dirname(repo_root)) == "world_fairs" &&
      basename(dirname(dirname(repo_root))) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, "..", "..", ".."), winslash = "/", mustWork = TRUE)
  }
  if (basename(repo_root) == "world_fairs" && basename(dirname(repo_root)) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, "..", ".."), winslash = "/", mustWork = TRUE)
  }
  if (basename(repo_root) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, ".."), winslash = "/", mustWork = TRUE)
  }
}

source(file.path(repo_root, "analysis", "world_fairs", "synthdid", "worlds_fairs_synthdid_helpers.R"))

run_worlds_fairs_synthdid(
  spec_type = "venue_distance",
  visits_threshold = 100000,
  results_subdir = file.path(
    "archived",
    "worlds_fairs_uk_us_venue_distance_visits_100k_synthdid_with_london_events_1840_1910"
  ),
  repo_root = repo_root
)
