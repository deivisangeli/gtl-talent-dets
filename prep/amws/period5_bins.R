###############################################################################
# Five-year binning rules, shared by the prep builder and the analysis runner
# so the two cannot drift apart.
#
# These carry the decade conventions of the land-grants and world-fairs papers
# (analysis/land_grants/temporal_support_1830_1950/01_build_temporal_support_panel.R)
# over to five-year blocks. The rule is applied to the outcome, its denominator
# and the treatment year alike:
#
#   standard : 5 * floor(y / 5)                     bin B collects B .. B+4  (leading)
#   alt      : 5 * floor(y / 5) + 5 * (y %% 5 >= 3) bin B collects B-2 .. B+2 (centred)
###############################################################################

period5_conventions <- c("standard", "alt")

period5_bin <- function(year, convention = "standard") {
  stopifnot(length(convention) == 1L, convention %in% period5_conventions)
  y <- as.integer(year)
  bin <- 5L * (y %/% 5L)
  if (convention == "alt") bin <- bin + 5L * (y %% 5L >= 3L)
  bin
}

# File-name suffix for a convention: the standard panel keeps the plain name,
# matching how the land-grants builder names its two decade panels.
period5_suffix <- function(convention) {
  stopifnot(length(convention) == 1L, convention %in% period5_conventions)
  if (convention == "standard") "" else paste0("_", convention)
}
