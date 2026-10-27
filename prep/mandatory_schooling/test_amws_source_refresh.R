# Source refresh may change late counts, while legacy keys/denominators remain fixed.
suppressPackageStartupMessages(library(data.table))
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", arg[1])), "../.."), winslash = "/")
source(file.path(root, "prep/mandatory_schooling/amws_panel_gender_helpers.R"))
folder <- tempfile("source_refresh_test_")
dir.create(folder)
old <- data.table(GEOID = c("01001", "01003"), year = 1925:1926,
  population = c(100,200), county_births_estimate_year = c(10,20),
  n_amws_1906_1955_dedup = c(1L,2L), n_amws_1986 = c(3L,4L), n_amws = c(4L,6L),
  amws_per_1000_births = c(400,300))
target <- file.path(folder,"old.csv"); stage <- file.path(folder,"stage.csv")
fwrite(old,target)
new <- copy(old); new[, `:=`(n_amws_1986 = n_amws_1986 + 1L,
                            n_amws = n_amws + 1L, amws_per_1000_births = c(500,350))]
fails <- function(expr) inherits(tryCatch(force(expr), error = identity),"error")
for (engine in c("chunked", "duckdb")) {
  Sys.setenv(AMWS_PANEL_VALIDATION_ENGINE = engine, AMWS_PANEL_SOURCE_REFRESH = "FALSE")
  fwrite(new,stage)
  stopifnot(fails(amws_verify_staged_panel(stage,target,2L)))
  Sys.setenv(AMWS_PANEL_SOURCE_REFRESH = "TRUE")
  amws_verify_staged_panel(stage,target,2L)
  for (field in c("GEOID","population","county_births_estimate_year","n_amws_1906_1955_dedup")) {
    bad <- copy(new)
    if (field == "GEOID") bad[1,GEOID := "02001"] else bad[1,(field) := get(field)+1]
    fwrite(bad,stage)
    stopifnot(fails(amws_verify_staged_panel(stage,target,2L)))
  }
}
unlink(folder,recursive=TRUE)
cat("Source-refresh checks passed for both validation engines.\n")
