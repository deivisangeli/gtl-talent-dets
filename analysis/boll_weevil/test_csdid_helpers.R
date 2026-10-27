suppressPackageStartupMessages(library(data.table))
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", arg[1])), "../.."), winslash = "/")
source(file.path(root, "analysis/boll_weevil/csdid_helpers.R"))

# An age-nine child was born nine years before infestation, not after it.
coords <- bw_coordinates(1900L, c(1860L, 1881L, 1882L, 1891L, 1896L))
stopifnot(all(coords$g == 1882L), all(coords$base_year == 1881L),
          identical(coords$age_at_infestation, c(40L, 19L, 18L, 9L, 4L)),
          identical(coords$event_time, c(-22L, -1L, 0L, 9L, 14L)))

# The intermediate adopter has a large jump. It must stop being a control
# after its own exposure onset, even though infestation itself is later.
toy <- CJ(id = 1:3, year = 1880:1896)
toy[, g := c(1882L, 1888L, 1904L)[id]]
toy[, model_y := 10 * id + 0.1 * (year - 1880) +
      fifelse(id == 1L & year >= g, 0.25, 0) +
      fifelse(id == 2L & year >= g, 50, 0)]
stopifnot(abs(bw_manual_did(toy, 1882L, 1880L)) < 1e-12,
          abs(bw_manual_did(toy, 1882L, 1881L)) < 1e-12,
          abs(bw_manual_did(toy, 1882L, 1885L) - 0.25) < 1e-12,
          abs(bw_manual_did(toy, 1882L, 1896L) - 0.25) < 1e-12)

# Averaging perfectly correlated annual estimates must not reduce variance by
# pretending that the annual estimates are independent.
influence <- cbind(c(-1, 1, -2, 2), c(-1, 1, -2, 2), c(-1, 1, -2, 2))
pooled <- bw_average_influence(influence, c(9L, 8L, 7L), 7:9)
stopifnot(identical(pooled, influence[, 1]),
          abs(sum(pooled^2) / 16 - 3 * sum(influence[, 1]^2) / (16 * 9)) > 0.1)
bad <- try(bw_average_influence(influence, 7:9, 4:6), silent = TRUE)
stopifnot(inherits(bad, "try-error"))
cat("PASS: birth-cohort alignment, untreated baseline, control contamination, and covariance-preserving aggregation.\n")
