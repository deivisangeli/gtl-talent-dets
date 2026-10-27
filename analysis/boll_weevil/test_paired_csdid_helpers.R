suppressPackageStartupMessages({library(data.table); library(did)})
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", arg[1])), "../.."), winslash = "/")
source(file.path(root, "analysis/boll_weevil/paired_csdid_helpers.R"))
# Nonsequential county IDs, uneven availability, and an already-exposed
# county with a large jump test pairing, contamination, orientation and scaling.
roster <- data.table(id = seq(101L, 125L, by = 2L),
  infestation_year = c(1900L, 1900L, 1910L, rep(1922L, 10)))
roster[, g := infestation_year - 18L]
years <- 1870:1900
y <- outer(seq_len(nrow(roster)), years, function(i, t) i * 3 + (t - 1870) * 0.1)
dimnames(y) <- list(roster$id, years)
y[1:2, years >= 1882L] <- y[1:2, years >= 1882L] + 0.3
y[3, years >= 1892L] <- y[3, years >= 1892L] + 100
y[13, years == 1881L] <- NA_real_
y[1, years == 1880L] <- y[1, years == 1880L] + 0.2
post <- bwp_pair(roster, y, 1900L, 4L)
pre <- bwp_pair(roster, y, 1900L, 20L)
ref <- bwp_pair(roster, y, 1900L, 19L)
stopifnot(abs(post$estimate - 0.3) < 1e-10, !post$selected[3], !post$selected[13],
          max(abs(post$influence - post$direct_if)) < 1e-8,
          abs(pre$estimate - 0.1) < 1e-10, pre$orientation_sign == -1L,
          ref$estimate == 0, all(ref$influence == 0))
# Group-share uncertainty must be included even when cell influence is zero.
r <- data.table(id = 1:6, infestation_year = c(1900,1900,1905,1905,1922,1922))
c <- CJ(infestation_year = c(1900L,1905L), age_at_infestation = 4:9)
c[, estimate := ifelse(infestation_year == 1900L, 1, 3)]
a <- bwp_aggregate(c, matrix(0,6,12), r, c(1900L,1905L))
stopifnot(all(a$annual$estimate == 2), all(abs(a$annual_influence[1:2,] + 1.5) < 1e-12),
          all(abs(a$annual_influence[3:4,] - 1.5) < 1e-12), all(a$annual_influence[5:6,] == 0))
# Perfectly correlated annual estimates retain their full variance when pooled.
aa <- matrix(rep(c(-2,-1,0,0,1,2),3),6,3)
b <- bwp_bootstrap(aa, matrix(rowMeans(aa),6,1), seed = 17L, biters = 1000L)
stopifnot(max(abs(b$draws[,1] - b$draws[,4])) < 1e-12,
          abs(b$annual_se[1] - b$pooled_se[1]) < 1e-12,
          b$empirical_simultaneous_coverage >= 0.95)
cat("PASS: paired coverage, untreated controls, sign reversal, county-ID alignment, influence scaling, group-share terms, and shared bootstrap covariance.\n")
