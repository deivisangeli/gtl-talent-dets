args <- commandArgs(FALSE)
f <- sub("^--file=", "", args[grepl("^--file=", args)][1])
if (is.na(f)) f <- "prep/land_grants/test_amws_birth_county_training.R"
folder <- dirname(normalizePath(f, winslash = "/"))
source(file.path(folder, "amws_training_helpers.R"))
source(file.path(folder, "amws_birth_county_training_helpers.R"))
suppressPackageStartupMessages(library(sf))
stopifnot(birth_training_class(TRUE, 2L, c(TRUE, FALSE)) == "ever",
  birth_training_class(c(FALSE, TRUE), 2L, c(TRUE, TRUE)) == "ever",
  birth_training_class(FALSE, 1L, TRUE) == "never",
  birth_training_class(FALSE, 2L, c(TRUE, FALSE)) == "unknown",
  birth_training_class(logical(), 0L, logical()) == "unknown",
  birth_training_class(NA, 1L, TRUE) == "unknown",
  birth_training_class(FALSE, 1L, TRUE, TRUE) == "unknown",
  birth_training_class(c(FALSE, FALSE), 2L, c(TRUE, TRUE)) == "never",
  birth_training_class(c(50000, 50000.01) <= 50000, 2L, c(TRUE, TRUE)) == "ever",
  birth_training_class(50000.01 <= 50000, 1L, TRUE) == "never")
# Closed polygon boundaries and cross-border nearby training are different rules.
square <- st_sfc(st_polygon(list(rbind(c(0,0), c(100000,0), c(100000,100000), c(0,100000), c(0,0)))), crs = 5070)
pts <- st_sfc(st_point(c(0,50000)), st_point(c(100001,50000)), crs = 5070)
stopifnot(identical(lengths(st_intersects(pts, square)) > 0L, c(TRUE, FALSE)))
near_border <- st_sfc(st_point(c(101,50)), crs = 5070)
small <- st_sfc(st_polygon(list(rbind(c(0,0),c(100,0),c(100,100),c(0,100),c(0,0)))), crs = 5070)
stopifnot(lengths(st_intersects(near_border, small)) == 0L,
  as.numeric(st_distance(near_border, st_centroid(small))) < 50000)
# Graduation dates are deliberately absent from the classifier's inputs.
undated <- data.table(year = NA_integer_, local = TRUE)
stopifnot(birth_training_class(undated$local, 1L, TRUE) == "ever",
  identical(training_rate(c(0,1,1), c(100,NA,0)), c(0,NA_real_,NA_real_)))
cat("PASS: training classification, polygon boundaries, cross-border proximity, 50 km threshold, undated training and invalid denominators.\n")
