suppressPackageStartupMessages(library(data.table))
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", arg[1])), "../.."), winslash = "/")
source(file.path(root, "analysis/boll_weevil/twfe_helpers.R"))
bins <- bwfe_exposure_bins(4:30)
stopifnot(all(rowSums(bins)[1:15] == 1), all(rowSums(bins)[16:27] == 0))
stopifnot(inherits(try(bwfe_exposure_bins(c(3L, 31L)), silent = TRUE), "try-error"))
beta <- setNames(c(1, 3, 0, 0, 0), names(bins))
V <- diag(5); dimnames(V) <- list(names(bins), names(bins)); V[1, 2] <- V[2, 1] <- 0.8
contrast <- bwfe_age_contrasts(beta, V, 85L)[age_window == "4-9"]
stopifnot(contrast$estimate == 2, abs(contrast$se - sqrt(0.9)) < 1e-12,
          contrast$reference_ages == "19-30")
# A ragged calendar panel with known county/year effects verifies independent
# two-way residualization; relative-age balance does not imply calendar balance.
toy <- CJ(id = 1:12, year = 1850:1870)[(id %% 3 != 0) | year >= 1855]
toy[, x := sin(id * year / 100)]
toy[, y := 2.5 * x + id * 0.2 + (year - 1850)^2 / 100]
fwl <- bwfe_fwl(toy$y, as.matrix(toy[, .(x)]), toy$id, toy$year)
stopifnot(abs(fwl$beta["x"] - 2.5) < 1e-9,
          max(abs(fwl$residual)) < 1e-9, max(abs(fwl$covariance_unadjusted)) < 1e-10)
cat("PASS: age-bin boundaries, pooled 19-30 reference, joint contrast covariance, and independent two-way FWL.\n")
