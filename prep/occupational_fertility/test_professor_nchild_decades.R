# Synthetic-fixture checks for convert_ipums_1850_1910_fertility.R and
# build_professor_nchild_decades.R.
#
# A two-census fixture (1850 and 1910) is written as a gzipped CSV with the real
# usa_00010 header, converted, and built, all inside a temporary directory. The
# DDI on D: is required (the converter checks the header against it), the 10 GB
# source is not. The cases are the ones the decade build is exposed to:
# 1850 has no MARST, so marriage must come from SPLOC; a blank OCC1950 must not
# drop a man; and step-children must leave the biological counts.
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/test_professor_nchild_decades.R

suppressPackageStartupMessages({
  library(arrow)
})

repo_root <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  } else {
    Sys.getenv("GTL_REPO", unset = getwd())
  }
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not locate the repo root (no paths.R found)")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
})
CONVERT <- file.path(repo_root, "prep/occupational_fertility/convert_ipums_1850_1910_fertility.R")
BUILD <- file.path(repo_root, "prep/occupational_fertility/build_professor_nchild_decades.R")
COUNTY <- file.path(repo_root, "prep/occupational_fertility/build_professor_county_nchild.R")
HELPERS <- file.path(repo_root, "analysis/occupational_fertility/fertility_helpers.R")
DDI <- Sys.getenv("IPUMS_1850_1910_FERTILITY_DDI", unset = "D:/ipums_1850_1910_fullcount_ddi.xml")
stopifnot(file.exists(DDI))

passed <- 0L; failed <- 0L
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) {
    passed <<- passed + 1L
    cat("  ok   ", label, "\n")
  } else {
    failed <<- failed + 1L
    detail <- paste(format(detail), collapse = ",")
    cat("  FAIL ", label, if (nzchar(detail)) paste0(" -- ", detail), "\n")
  }
}
run <- function(script) {
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
                                  stdout = TRUE, stderr = TRUE))
  code <- attr(out, "status")
  list(ok = is.null(code) || code == 0L, out = out)
}

# --------------------------------------------------------------------------- #
# Fixture
# --------------------------------------------------------------------------- #
columns <- c("YEAR", "SAMPLE", "SERIAL", "HHWT", "STATEICP", "STATEFIP", "COUNTYICP", "URBAN",
             "GQ", "PERNUM", "PERWT", "MOMLOC", "POPLOC", "STEPPOP", "SPLOC", "SPRULE_HIST",
             "NCHILD", "RELATE", "RELATED", "SEX", "AGE", "MARST", "DURMARR", "CHBORN",
             "CHSURV", "RACE", "RACED", "BPL", "BPLD", "OCC", "OCC1950", "VERSIONHIST", "HISTID")
# Blank ("") means not asked that year, as in the real extract.
person <- function(year, serial, pernum, sex, age, occ1950 = "999", occ = "", marst = "",
                   sploc = 0, nchild = 0, poploc = 0, momloc = 0, steppop = 0, gq = 1,
                   chborn = "", chsurv = "", county = 10) {
  data.frame(YEAR = year, SAMPLE = if (year == 1850) 185002 else 191004, SERIAL = serial,
             HHWT = "1.0", STATEICP = 13, STATEFIP = 36, COUNTYICP = county, URBAN = 2, GQ = gq,
             PERNUM = pernum, PERWT = "1.0", MOMLOC = momloc, POPLOC = poploc,
             STEPPOP = steppop, SPLOC = sploc, SPRULE_HIST = as.integer(sploc > 0),
             NCHILD = nchild, RELATE = 1, RELATED = 101, SEX = sex, AGE = age, MARST = marst,
             DURMARR = if (year == 1850) "" else "99", CHBORN = chborn, CHSURV = chsurv,
             RACE = 1, RACED = 100, BPL = 36, BPLD = 3600, OCC = occ, OCC1950 = occ1950,
             VERSIONHIST = 2, HISTID = sprintf("id-%d-%d-%d", year, serial, pernum),
             stringsAsFactors = FALSE)
}
rows <- rbind(
  # 1850: married professor (no MARST that year), a child of 3 and one of 12.
  person(1850, 1, 1, 1, 40, occ1950 = "029", occ = "54", sploc = 2, nchild = 2),
  person(1850, 1, 2, 2, 35, sploc = 1, nchild = 2),
  person(1850, 1, 3, 1, 3, poploc = 1, momloc = 2),
  person(1850, 1, 4, 2, 12, poploc = 1, momloc = 2),
  person(1850, 2, 1, 1, 30, occ1950 = "100", occ = "1"),
  # A man with a blank OCC1950 stays in, as no_occupation.
  person(1850, 3, 1, 1, 45, occ1950 = ""),
  # A historical county (code ending in 5, no modern FIPS) with a man of 29.
  person(1850, 4, 1, 1, 29, occ1950 = "100", county = 15),
  # 1910: professor whose only co-resident child is a stepchild of 8.
  person(1910, 1, 1, 1, 50, occ1950 = "029", marst = "1", sploc = 2, nchild = 1,
         chborn = "00", chsurv = "00"),
  person(1910, 1, 2, 2, 45, marst = "1", sploc = 1, nchild = 1, chborn = "04", chsurv = "03"),
  person(1910, 1, 3, 1, 8, poploc = 1, momloc = 2, steppop = 2, marst = "6",
         chborn = "00", chsurv = "00"),
  person(1910, 2, 1, 1, 35, occ1950 = "075", marst = "6", gq = 4, chborn = "00", chsurv = "00"),
  person(1910, 3, 1, 2, 28, occ1950 = "029", marst = "6", chborn = "00", chsurv = "00"),
  # A county with no professor: a man of 30 (bin 30-39) and one of 70 (out of range).
  person(1910, 4, 1, 1, 30, occ1950 = "100", marst = "6", chborn = "00", chsurv = "00",
         county = 30),
  person(1910, 5, 1, 1, 70, occ1950 = "100", marst = "6", chborn = "00", chsurv = "00",
         county = 30))
stopifnot(identical(names(rows), columns))

root <- file.path(tempdir(), paste0("occ_nchild_fixture_", Sys.getpid()))
dir.create(root, recursive = TRUE, showWarnings = FALSE)
source_path <- file.path(root, "fixture.csv.gz")
parquet_dir <- file.path(root, "parquet")
out_dir <- file.path(root, "out")
county_dir <- file.path(root, "county")
crosswalk <- file.path(root, "crosswalk.csv")
# The NHGIS crosswalk knows counties 10 and 30 but not the historical code 15.
utils::write.csv(data.frame(GISJOIN = c("G3600010", "G3600030"), STATE = "New York",
                            COUNTY = c("Albany County", "Bronx County")),
                 crosswalk, row.names = FALSE)
utils::write.csv(data.frame(census = c(1850L, 1910L), faculty_factor = c(4, 2.5)),
                 file.path(root, "coverage.csv"), row.names = FALSE)
connection <- gzfile(source_path, "wt")
utils::write.csv(rows, connection, row.names = FALSE, na = "")
close(connection)

# system2(env = ) is ignored on Windows; the child inherits Sys.setenv instead.
Sys.setenv(IPUMS_1850_1910_FERTILITY_SOURCE = source_path,
           IPUMS_1850_1910_FERTILITY_DDI = DDI,
           IPUMS_1850_1910_FERTILITY_PARQUET_DIR = parquet_dir,
           IPUMS_1910_FERTILITY_PARQUET_DIR = file.path(root, "no_1910_parquet"),
           IPUMS_1850_1910_EXPECT_ROWS = "false",
           OCC_FERTILITY_DECADES_OUT_DIR = out_dir,
           OCC_FERTILITY_COUNTY_OUT_DIR = county_dir,
           OCC_COVERAGE_FACTORS = file.path(root, "coverage.csv"),
           NHGIS_COUNTY_CROSSWALK = crosswalk,
           OCC_FERTILITY_THREADS = "2")

cat("\nConverter\n")
conv <- run(CONVERT)
ok("converter exits cleanly", conv$ok, tail(conv$out, 5))
manifest <- jsonlite::read_json(file.path(parquet_dir, "_manifest.json"), simplifyVector = TRUE)
ok("one partition per census year",
   setequal(basename(list.dirs(parquet_dir, recursive = FALSE)), c("YEAR=1850", "YEAR=1910")))
by_year <- manifest$by_year
ok("rows per year", identical(as.numeric(by_year$n), c(7, 7)), by_year$n)
ok("blank MARST and CHBORN stay NULL in 1850",
   by_year$marst_filled[1] == 0 && by_year$chborn_filled[1] == 0)
ok("1910 MARST and CHBORN are filled", by_year$marst_filled[2] == 7 && by_year$chborn_filled[2] == 7)
ok("professors counted per year", identical(as.numeric(by_year$professors), c(1, 2)))
ok("missing 1910 Parquet skips the cross-check", identical(manifest$cross_check_1910, "skipped"))

cat("\nBuilder\n")
build <- run(BUILD)
ok("builder exits cleanly", build$ok, tail(build$out, 5))
diag <- utils::read.csv(file.path(out_dir, "decade_diagnostics.csv"))
cells <- as.data.frame(arrow::open_dataset(file.path(out_dir, "men_cells")) |> dplyr::collect())
prof <- utils::read.csv(file.path(out_dir, "professors_by_year.csv"))
calib <- utils::read.csv(file.path(out_dir, "couples_calibration_cells.csv"))
ok("own-children counts agree with NCHILD in both years", all(diag$nchild_mismatch == 0))
ok("cells hold every man aged 20-64",
   identical(as.numeric(tapply(cells$n, cells$YEAR, sum)), c(4, 3)))
c1850 <- cells[cells$YEAR == 1850 & cells$occ_group == "professor", ]
ok("1850 professor is married through SPLOC despite blank MARST", isTRUE(c1850$married_sp))
ok("1850 professor: NCHILD 2, one child under 5, one under 10",
   c1850$sum_nchild == 2 && c1850$sum_own_u5 == 1 && c1850$sum_own_u10 == 1)
ok("blank OCC1950 is no_occupation, not dropped",
   any(cells$YEAR == 1850 & cells$occ_group == "no_occupation" & cells$AGE == 45))
c1910 <- cells[cells$YEAR == 1910 & cells$occ_group == "professor", ]
ok("1910 stepchild counts in NCHILD and own_u10 but not the biological variant",
   c1910$sum_nchild == 1 && c1910$sum_own_u10 == 1 && c1910$sum_own_u10_bio == 0)
ok("group-quarters physician is flagged out of households",
   isFALSE(cells$in_household[cells$YEAR == 1910 & cells$occ_group == "physician"]))
ok("professor file holds both sexes and both years", nrow(prof) == 3L &&
     setequal(prof$YEAR, c(1850, 1910)) && sum(prof$SEX == 2) == 1L)
ok("professor OCC carried through", prof$OCC[prof$YEAR == 1850] == 54)
ok("calibration cells only where CHBORN exists", identical(unique(calib$YEAR), 1910L))
cp <- calib[calib$occ_group == "professor", ]
ok("calibration decodes the wife's offset codes",
   cp$sum_chsurv == 2 && cp$sum_chborn == 3 && cp$sum_h_nchild == 1 && cp$w_age5_lo == 45)

cat("\nCounty x age-bin dataset\n")
cc <- as.data.frame(arrow::open_dataset(file.path(out_dir, "county_cells")) |> dplyr::collect())
ok("county cells hold every man aged 20-69",
   identical(as.numeric(tapply(cc$n, cc$YEAR, sum)), c(4, 3)))
shape <- run(COUNTY)
ok("county shaper exits cleanly", shape$ok, tail(shape$out, 5))
ca <- utils::read.csv(file.path(county_dir, "professor_nchild_county_age.csv"))
cy <- utils::read.csv(file.path(county_dir, "professor_nchild_county.csv"))
row <- function(year, county, bin) ca[ca$YEAR == year & ca$COUNTYICP == county & ca$age_bin == bin, ]
h15 <- row(1850, 15, "20-29")
ok("age 29 falls in 20-29", nrow(h15) == 1L && h15$n_allmen == 1)
ok("historical code keeps its GISJOIN, no county_geoid, flagged off the crosswalk",
   h15$GISJOIN == "G3600015" && is.na(h15$county_geoid) && !as.logical(h15$in_nhgis_crosswalk))
a10 <- row(1850, 10, "40-49")
ok("standard code gets GISJOIN, county_geoid and the crosswalk name",
   a10$GISJOIN == "G3600010" && a10$county_geoid == 36001 && a10$county_name == "Albany County")
ok("1850 professor counted in both universes (married via SPLOC)",
   a10$n_prof == 1 && a10$n_prof_married == 1 && a10$nchild_mean_prof == 2)
ok("bin ratio is professor mean over all-men mean", isTRUE(all.equal(a10$ratio_prof_to_allmen, 2)))
b30 <- row(1910, 30, "30-39")
ok("county with no professor appears with n_prof 0", nrow(b30) == 1L && b30$n_prof == 0 &&
     is.na(b30$nchild_mean_prof) && is.na(b30$ratio_prof_to_allmen))
ok("age 70 is out of range", all(ca$age_bin_lo <= 60) &&
     sum(ca$n_allmen[ca$YEAR == 1910 & ca$COUNTYICP == 30]) == 1)
g10 <- row(1910, 10, "30-39")
ok("group-quarters physician kept out of household counts",
   nrow(g10) == 1L && g10$n_prof_other == 0 && g10$n_allmen == 0)
c10 <- cy[cy$YEAR == 1850 & cy$COUNTYICP == 10, ]
# Observed 2 (one professor aged 40, NCHILD 2); expected 1 x the county's all-men
# mean at 40-49, (2 + 0) / 2 = 1.
ok("indirect ratio: observed over expected at county rates",
   c10$observed == 2 && c10$expected_local == 1 && isTRUE(all.equal(c10$ratio_local, 2)))
ok("county summary flags thin cells", isTRUE(as.logical(c10$low_n_prof)))
c1910 <- cy[cy$YEAR == 1910 & cy$COUNTYICP == 10, ]
# 1910 county 10: the male professor aged 50 (the physician is not 010-029) x 2.5.
ok("faculty-equivalent count = men coded 010-029 x census factor",
   c10$n_prof_deans_all == 1 && c10$faculty_factor == 4 && c10$n_faculty_equiv == 4 &&
     c1910$n_prof_deans_all == 1 && c1910$n_faculty_equiv == 2.5)
ok("unmatched county listed",
   any(utils::read.csv(file.path(county_dir, "county_codes_unmatched.csv"))$GISJOIN == "G3600015"))

cat("\nFixed standard across years\n")
source(HELPERS, local = TRUE)
# Identical age-specific rates in two years with opposite age mixes, both
# standardised to one pooled standard, must come out equal.
yr <- data.frame(group = rep(c("1850", "1910"), each = 2), stratum = rep(c("25-29", "50-54"), 2),
                 n = c(80, 20, 20, 80), total = c(80, 20, 20, 80) * rep(c(1, 3), 2))
pooled <- data.frame(stratum = c("25-29", "50-54"), weight = c(100, 100))
std <- direct_standardise(yr, pooled)
ok("same rates, different age mix -> equal on the pooled standard",
   isTRUE(all.equal(std$std_mean[1], std$std_mean[2])) && abs(std$crude_mean[1] - std$crude_mean[2]) > 1)

unlink(root, recursive = TRUE)
cat(sprintf("\n%d passed, %d failed\n", passed, failed))
if (failed > 0L) quit(status = 1L)
