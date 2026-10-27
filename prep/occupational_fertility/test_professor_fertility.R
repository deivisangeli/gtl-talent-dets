# Synthetic-fixture checks for build_professor_fertility.R and the
# standardisation helper in analysis/occupational_fertility/fertility_helpers.R.
#
# Runs without the 92 M-row Parquet: a handful of hand-built households is
# written as STATEFIP-partitioned Parquet in a temporary directory and the build
# is invoked against it through IPUMS_1910_FERTILITY_PARQUET_DIR /
# OCC_FERTILITY_OUT_DIR. Every expected value is known by hand. The cases are the
# ones this build is most exposed to: the off-by-one CHBORN/CHSURV coding, a
# married professor who boards rather than heads the household, and own
# children who are step-children.
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/test_professor_fertility.R

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
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
BUILD <- file.path(repo_root, "prep/occupational_fertility/build_professor_fertility.R")
HELPERS <- file.path(repo_root, "analysis/occupational_fertility/fertility_helpers.R")

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

# --------------------------------------------------------------------------- #
# Fixture
# --------------------------------------------------------------------------- #
# One row per person. Columns not listed take the defaults in `person()`.
person <- function(state, serial, pernum, sex, age, occ = 999L, marst = 6L,
                   sploc = 0L, durmarr = 99L, chborn = 0L, chsurv = 0L,
                   poploc = 0L, momloc = 0L, steppop = 0L, nchild = 0L,
                   relate = 1L, related = 101L, gq = 1L, bpl = 36L) {
  data.frame(STATEFIP = state, SERIAL = serial, PERNUM = pernum,
             HISTID = sprintf("h%d-%d-%d", state, serial, pernum),
             COUNTYICP = 10L, URBAN = 2L, GQ = gq, RELATE = relate, RELATED = related,
             SEX = sex, AGE = age, MARST = marst, DURMARR = durmarr, SPLOC = sploc,
             SPRULE_HIST = as.integer(sploc > 0), NCHILD = nchild, RACE = 1L, BPL = bpl,
             OCC1950 = occ, CHBORN = chborn, CHSURV = chsurv, POPLOC = poploc,
             MOMLOC = momloc, STEPPOP = steppop)
}
rows <- rbind(
  # HH1: professor head, wife with 3 born / 2 surviving, two own children and a
  # stepchild (STEPPOP 2). Raw codes are offset by one.
  person(1, 1, 1, 1, 45, occ = 29, marst = 1, sploc = 2, durmarr = 15, nchild = 3),
  person(1, 1, 2, 2, 40, marst = 1, sploc = 1, durmarr = 15, chborn = 4, chsurv = 3,
         relate = 2, related = 201, nchild = 3),
  person(1, 1, 3, 1, 3, poploc = 1, momloc = 2, relate = 3, related = 301),
  person(1, 1, 4, 2, 8, poploc = 1, momloc = 2, relate = 3, related = 301),
  person(1, 1, 5, 1, 12, poploc = 1, momloc = 2, steppop = 2, relate = 3, related = 301),
  # HH2: farmer head; the professor and his wife are boarders. Wife: no
  # children (code 01), DURMARR illegible (98).
  person(1, 2, 1, 1, 60, occ = 100),
  person(1, 2, 2, 1, 33, occ = 12, marst = 1, sploc = 3, durmarr = 98,
         relate = 12, related = 1202),
  person(1, 2, 3, 2, 30, marst = 1, sploc = 2, durmarr = 98, chborn = 1, chsurv = 1,
         relate = 12, related = 1202),
  # HH3: widowed professor with one co-resident child aged 6.
  person(2, 3, 1, 1, 50, occ = 29, marst = 5, nchild = 1),
  person(2, 3, 2, 2, 6, poploc = 1, relate = 3, related = 301),
  # HH4: never-married professor in group quarters.
  person(2, 4, 1, 1, 30, occ = 29, gq = 4, relate = 12, related = 1223),
  # HH5: physician couple, wife CHBORN illegible (98), CHSURV 5 -> 4.
  person(2, 5, 1, 1, 50, occ = 75, marst = 1, sploc = 2, durmarr = 20, bpl = 410),
  person(2, 5, 2, 2, 46, marst = 1, sploc = 1, durmarr = 20, chborn = 98, chsurv = 5,
         relate = 2, related = 201, bpl = 410),
  # HH6: lawyer couple where surviving exceeds born (flagged, not dropped).
  person(2, 6, 1, 1, 38, occ = 55, marst = 1, sploc = 2, durmarr = 10),
  person(2, 6, 2, 2, 35, marst = 1, sploc = 1, durmarr = 10, chborn = 3, chsurv = 4,
         relate = 2, related = 201),
  # HH7: single female professor; HH8: married man of 70 with no SPLOC.
  person(2, 7, 1, 2, 28, occ = 29),
  person(2, 8, 1, 1, 70, occ = 100, marst = 1)
)
rows[] <- lapply(rows, function(v) if (is.numeric(v)) as.integer(v) else v)

root <- file.path(tempdir(), paste0("occ_fertility_fixture_", Sys.getpid()))
in_dir <- file.path(root, "parquet")
out_dir <- file.path(root, "out")
dir.create(root, recursive = TRUE, showWarnings = FALSE)
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "fixture", rows)
invisible(DBI::dbExecute(con, sprintf(
  "COPY fixture TO '%s' (FORMAT PARQUET, PARTITION_BY (STATEFIP))",
  gsub("\\\\", "/", in_dir))))
DBI::dbDisconnect(con, shutdown = TRUE)

cat("\nBuild on the fixture\n")
# system2(env = ) is ignored on Windows; the child inherits Sys.setenv instead.
Sys.setenv(IPUMS_1910_FERTILITY_PARQUET_DIR = in_dir, OCC_FERTILITY_OUT_DIR = out_dir,
           OCC_FERTILITY_THREADS = "2")
status <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(BUILD),
                                   stdout = TRUE, stderr = TRUE))
code <- attr(status, "status")
ok("build exits cleanly", is.null(code) || code == 0L, paste(tail(status, 5), collapse = " | "))

couples <- as.data.frame(arrow::read_parquet(file.path(out_dir, "couples_1910.parquet")))
men <- as.data.frame(arrow::read_parquet(file.path(out_dir, "men_own_children_1910.parquet")))
female <- utils::read.csv(file.path(out_dir, "female_professors_1910.csv"))
diag <- utils::read.csv(file.path(out_dir, "linkage_diagnostics.csv"))
manifest <- jsonlite::read_json(file.path(out_dir, "build_manifest.json"))
couple <- function(state, serial) couples[couples$STATEFIP == state & couples$SERIAL == serial, ]
man <- function(state, serial, pernum) {
  men[men$STATEFIP == state & men$SERIAL == serial & men$PERNUM == pernum, ]
}

cat("\nWife-linked fertility (way 1)\n")
ok("four reciprocal couples", nrow(couples) == 4L, nrow(couples))
c1 <- couple(1, 1)
ok("offset decode: CHBORN 4 -> 3 born", identical(c1$chborn, 3L), c1$chborn)
ok("offset decode: CHSURV 3 -> 2 surviving", identical(c1$chsurv, 2L), c1$chsurv)
ok("wife's age at marriage is age - durmarr", identical(c1$w_age_at_marriage, 25L))
ok("professor head gets the professor group", identical(c1$occ_group, "professor"))
c2 <- couple(1, 2)
ok("boarder professor couple is linked", nrow(c2) == 1L && c2$h_related == 1202L)
ok("code 01 decodes to zero children", identical(c2$chborn, 0L) && identical(c2$chsurv, 0L))
ok("DURMARR 98 decodes to NA", is.na(c2$durmarr) && is.na(c2$w_age_at_marriage))
c5 <- couple(2, 5)
ok("CHBORN 98 decodes to NA, CHSURV 5 -> 4", is.na(c5$chborn) && identical(c5$chsurv, 4L))
ok("foreign-born flag from BPL", isFALSE(c5$h_us_born) && isTRUE(c1$h_us_born))
c6 <- couple(2, 6)
ok("surviving > born is flagged, not dropped", isTRUE(c6$flag_surv_gt_born))
ok("no flag where surviving <= born", isFALSE(c1$flag_surv_gt_born))
ok("widowed professor has no couple row", nrow(couple(2, 3)) == 0L)
ok("MARST=1 without SPLOC is counted, not linked",
   manifest$married_spouse_present_without_sploc == 1L && nrow(couple(2, 8)) == 0L)

cat("\nOwn children through POPLOC (way 2)\n")
m1 <- man(1, 1, 1)
ok("own children counted by POPLOC, all ages", identical(m1$own_children_all, 3L))
ok("under-5 and under-10 cut-offs", identical(m1$own_children_u5, 1L) &&
     identical(m1$own_children_u10, 2L))
ok("biological variant drops STEPPOP > 0", identical(m1$own_children_all_bio, 2L) &&
     identical(m1$own_children_u10_bio, 2L))
ok("couple row carries the husband's counts", identical(c1$h_own_children_all, 3L))
ok("wife's co-resident children counted by MOMLOC", identical(c1$w_own_children_all, 3L))
m3 <- man(2, 3, 1)
ok("widowed father still gets a direct count", identical(m3$own_children_u10, 1L))
m4 <- man(2, 4, 1)
ok("group-quarters man flagged out of households, zero children",
   isFALSE(m4$in_household) && identical(m4$own_children_all, 0L))
ok("men table is ages 20-64 only", nrow(man(2, 8, 1)) == 0L && all(men$AGE >= 20 & men$AGE <= 64))
ok("own_children_all agrees with NCHILD", manifest$nchild_mismatch_capped9 == 0L)

cat("\nFemale professors and diagnostics\n")
ok("female professor file holds the single woman", nrow(female) == 1L &&
     is.na(female$chsurv) && female$MARST == 6L)
prof <- diag[diag$occ_group == "professor", ]
ok("diagnostics: two linked professors, one not a head",
   sum(prof$linked_couples) == 2L && sum(prof$linked_not_head) == 1L)
ok("diagnostics: one professor in group quarters", sum(prof$group_quarters) == 1L)

cat("\nStandardisation helper\n")
source(HELPERS, local = TRUE)
# Same age-specific rates, opposite age mix: the standardised values must match
# and differ from the crude ones.
cells <- data.frame(group = rep(c("a", "b"), each = 2), stratum = rep(c("young", "old"), 2),
                    n = c(90, 10, 10, 90), total = c(90, 10, 10, 90) * rep(c(1, 5), 2))
standard <- data.frame(stratum = c("young", "old"), weight = c(0.5, 0.5))
std <- direct_standardise(cells, standard)
ok("identical rates, different age mix -> equal standardised means",
   isTRUE(all.equal(std$std_mean[std$group == "a"], std$std_mean[std$group == "b"])) &&
     isTRUE(all.equal(std$std_mean[1], 3)))
ok("crude means differ", abs(std$crude_mean[1] - std$crude_mean[2]) > 1)
own <- direct_standardise(cells[cells$group == "a", ],
                          data.frame(stratum = c("young", "old"), weight = c(90, 10)))
ok("a group standardised to its own mix equals its crude mean",
   isTRUE(all.equal(own$std_mean, own$crude_mean)))
miss <- direct_standardise(cells[cells$group == "a" & cells$stratum == "young", ], standard)
ok("a missing stratum yields NA, not a reweighted mean", is.na(miss$std_mean))

unlink(root, recursive = TRUE)
cat(sprintf("\n%d passed, %d failed\n", passed, failed))
if (failed > 0L) quit(status = 1L)
