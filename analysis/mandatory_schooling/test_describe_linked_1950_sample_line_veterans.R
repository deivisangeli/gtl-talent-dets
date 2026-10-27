# End-to-end fixture for cohort overlap, veteran eligibility, and weighted shares.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

source("analysis/mandatory_schooling/describe_linked_1950_sample_line_veterans.R",
       local = TRUE)
root <- file.path(tempdir(), "sample_line_veteran_fixture")
dir.create(root, showWarnings = FALSE)
rows <- data.frame(
  YEAR = c(rep(1950L, 7L), 1940L),
  BIRTHYR = c(1921L, 1925L, 1926L, 1915L, 1918L, 1923L, 1924L, 1921L),
  SEX = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L),
  SLREC = 2L,
  SLWT = c(2, 3, 5, 7, 11, 13, 17, 19),
  VETWWII = c(1L, 1L, 2L, 2L, 1L, 0L, 2L, 2L),
  HIGRADE = c(15L, 99L, 15L, 17L, 15L, 17L, 17L, 17L),
  HIGRADED = c(151L, 999L, 150L, 170L, 151L, 170L, 170L, 170L),
  scientist_occ1950 = c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
  birth_bpl = "001")
con <- dbConnect(duckdb())
dbWriteTable(con, "fixture", rows)
invisible(dbExecute(con, sprintf("COPY fixture TO '%s' (FORMAT PARQUET,
  PARTITION_BY (birth_bpl), FILENAME_PATTERN 'part_{i}')",
  gsub("\\\\", "/", root))))
dbDisconnect(con, shutdown = TRUE)

result <- summarize_veterans(root)
g <- result$groups
cell <- function(cohort, status) g[g$cohort == cohort &
                                   g$veteran_status == status, , drop = FALSE]
young_non <- cell("1921-1927", "Non-veteran")
young_vet <- cell("1921-1927", "Veteran")
all_non <- cell("1910-1927", "Non-veteran")
all_vet <- cell("1910-1927", "Veteran")

stopifnot(nrow(g) == 4L,
          sum(g$sample_n[g$cohort == "1921-1927"]) == 3L,
          sum(g$sample_n[g$cohort == "1910-1927"]) == 5L,
          young_non$weighted_n == 5,
          young_non$school_valid_weighted == 2,
          young_non$school_missing_weighted == 3,
          young_non$college_n == 1L,
          young_non$college_weighted == 2,
          young_non$college_share_pct == 100,
          young_non$scientist_weighted == 3,
          young_non$scientist_share_pct == 60,
          young_vet$weighted_n == 5,
          young_vet$college_weighted == 0,
          young_vet$scientist_share_pct == 100,
          all_non$weighted_n == 16,
          all_non$college_weighted == 13,
          all_non$scientist_weighted == 14,
          all_vet$weighted_n == 12,
          all_vet$college_weighted == 7,
          all_vet$scientist_weighted == 5,
          isTRUE(all.equal(all_vet$college_share_pct, 100 * 7 / 12)),
          isTRUE(all.equal(all_vet$scientist_share_pct, 100 * 5 / 12)))
d <- result$contrasts
stopifnot(d$college_veteran_minus_nonveteran_pp[d$cohort == "1921-1927"] == -100,
          d$scientist_veteran_minus_nonveteran_pp[d$cohort == "1921-1927"] == 40)
message("1950 sample-line veteran fixture passed")

# Paper cohorts are disjoint and include all four endpoints, with 1909/1928 out.
paper_rows <- rows[rep(1L, 7L), ]
paper_rows$BIRTHYR <- c(1910L, 1921L, 1922L, 1927L, 1909L, 1928L, 1911L)
paper_rows$VETWWII <- c(1L, 2L, 1L, 2L, 1L, 2L, 2L)
paper_rows$SLWT <- c(2, 3, 5, 7, 1001, 1002, 4)
paper_rows$HIGRADED <- c(151L, 999L, 150L, 151L, 151L, 151L, 151L)
paper_rows$scientist_occ1950 <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
paper_root <- file.path(tempdir(), "sample_line_paper_cohort_fixture")
con <- dbConnect(duckdb())
dbWriteTable(con, "fixture", paper_rows)
invisible(dbExecute(con, sprintf("COPY fixture TO '%s' (FORMAT PARQUET,
  PARTITION_BY (birth_bpl), FILENAME_PATTERN 'part_{i}')",
  gsub("\\\\", "/", paper_root))))
dbDisconnect(con, shutdown = TRUE)
paper <- summarize_veterans(paper_root, data.frame(
  cohort = c("1922-1927", "1910-1921"),
  first_year = c(1922L, 1910L), last_year = c(1927L, 1921L)))$groups
stopifnot(identical(paper$cohort, rep(c("1922-1927", "1910-1921"), each = 2)),
  identical(as.numeric(paper$sample_n), c(1, 1, 1, 2)),
  identical(paper$weighted_n, c(5, 7, 2, 7)),
  identical(paper$college_weighted, c(0, 7, 2, 4)),
  identical(paper$scientist_weighted, c(5, 0, 2, 4)),
  identical(paper$school_missing_weighted, c(0, 0, 0, 3)),
  paper$college_share_pct[4] == 100)
message("Paper-cohort boundary fixture passed")
