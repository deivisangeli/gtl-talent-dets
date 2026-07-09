###############################################################################
# Aggregate AMWS 1955 births to a county-year panel (n_amws per geoid x year).
#
# Inputs
#   output/amws_1955_us_geocoded_final.csv  (lineid -> geoid)
#   output/amws_1955_split.csv             (lineid -> canonical birth_year)
#
# Output
#   output/amws_1955_county_year.csv  (GEOID, year, n_amws)
#
# Birth-year parsing is handled upstream by cleaning_amws_1955.R, including
# conservative year-only recoveries and implausible-year filtering.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

geo <- fread(file.path(AMWS_OUTPUT, "amws_1955_us_geocoded_final.csv"),
             select = c("lineid", "geoid"))
split_corrected <- file.path(AMWS_OUTPUT, "amws_1955_split_corrected.csv")
split_original <- file.path(AMWS_OUTPUT, "amws_1955_split.csv")
split_file <- if (file.exists(split_corrected)) split_corrected else split_original
cln <- fread(split_file,
             select = c("lineid", "birth_year"))

d <- merge(geo, cln, by = "lineid", all.x = TRUE)

cat(sprintf("Geocoded rows with birth_year: %d / %d\n",
            sum(!is.na(d$birth_year)), nrow(d)))

panel <- d[!is.na(birth_year), .N,
           by = .(GEOID = as.integer(geoid), year = birth_year)]
setnames(panel, "N", "n_amws")
setorder(panel, GEOID, year)

cat(sprintf("Aggregated %d county-year rows, %d counties, years %d-%d, total births %d\n",
            nrow(panel),
            uniqueN(panel$GEOID),
            min(panel$year), max(panel$year),
            sum(panel$n_amws)))

fwrite(panel, file.path(AMWS_OUTPUT, "amws_1955_county_year.csv"))
cat("wrote", file.path(AMWS_OUTPUT, "amws_1955_county_year.csv"), "\n")
