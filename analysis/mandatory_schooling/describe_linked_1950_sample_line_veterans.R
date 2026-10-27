# Weighted, descriptive WWII veteran comparisons in the 1950 sample line.
# From the repository root:
#   Rscript analysis/mandatory_schooling/describe_linked_1950_sample_line_veterans.R

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

sql_path <- function(path) gsub("\\\\", "/", path)
fail_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)

data_root <- function() {
  configured <- Sys.getenv("TALENT_DETS_DATA_DIR")
  candidates <- c(configured,
                  file.path("C:/Users", Sys.info()[["user"]],
                            "Globtalent Dropbox/gtl_talent_dets"))
  available <- candidates[nzchar(candidates) & dir.exists(candidates)]
  fail_if(!length(available), "Cannot locate Dropbox data root; set TALENT_DETS_DATA_DIR")
  normalizePath(available[[1L]], winslash = "/")
}

summarize_veterans <- function(parquet_dir, cohorts = data.frame(
    cohort = c("1921-1927", "1910-1927"),
    first_year = c(1921L, 1910L), last_year = c(1927L, 1927L))) {
  fail_if(!nrow(cohorts) || anyDuplicated(cohorts$cohort) > 0L ||
            anyNA(cohorts) || any(!nzchar(cohorts$cohort)) ||
            any(!is.finite(cohorts$first_year)) ||
            any(!is.finite(cohorts$last_year)) ||
            any(cohorts$first_year != as.integer(cohorts$first_year)) ||
            any(cohorts$last_year != as.integer(cohorts$last_year)) ||
            any(cohorts$first_year > cohorts$last_year),
          "Cohorts require unique labels and inclusive integer year bounds")
  cohorts$cohort_order <- seq_len(nrow(cohorts))
  glob <- file.path(parquet_dir, "birth_bpl=*", "*.parquet")
  fail_if(!length(Sys.glob(glob)), "No sample-line Parquet files at ", glob)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads = 8")
  DBI::dbExecute(con, "SET memory_limit = '20GB'")
  DBI::dbWriteTable(con, "cohorts", cohorts)
  parquet <- paste0("read_parquet(",
                    as.character(DBI::dbQuoteString(con, sql_path(glob))),
                    ", hive_partitioning = true)")
  query <- paste0("WITH eligible AS (
      SELECT BIRTHYR, VETWWII, CAST(SLWT AS DOUBLE) AS weight,
             HIGRADED, scientist_occ1950
      FROM ", parquet, "
      WHERE YEAR = 1950 AND SLREC = 2 AND SEX = 1
        AND VETWWII IN (1, 2) AND SLWT > 0
        AND BIRTHYR BETWEEN ", min(cohorts$first_year), " AND ",
        max(cohorts$last_year), "
    )
    SELECT c.cohort, e.VETWWII AS veteran_code,
           COUNT(*) AS sample_n,
           SUM(e.weight) AS weighted_n,
           COUNT(*) FILTER (WHERE e.HIGRADED IS NULL OR
             e.HIGRADED IN (0, 999)) AS school_missing_n,
           COALESCE(SUM(e.weight) FILTER (WHERE e.HIGRADED IS NULL OR
             e.HIGRADED IN (0, 999)), 0) AS school_missing_weighted,
           COUNT(*) FILTER (WHERE e.HIGRADED IS NOT NULL AND
             e.HIGRADED NOT IN (0, 999)) AS school_valid_n,
           COALESCE(SUM(e.weight) FILTER (WHERE e.HIGRADED IS NOT NULL AND
             e.HIGRADED NOT IN (0, 999)), 0) AS school_valid_weighted,
           COUNT(*) FILTER (WHERE e.HIGRADED BETWEEN 151 AND 230) AS college_n,
           COALESCE(SUM(e.weight) FILTER (WHERE e.HIGRADED BETWEEN 151 AND 230),
                    0) AS college_weighted,
           COUNT(*) FILTER (WHERE e.scientist_occ1950) AS scientist_n,
           COALESCE(SUM(e.weight) FILTER (WHERE e.scientist_occ1950),
                    0) AS scientist_weighted
    FROM eligible e JOIN cohorts c
      ON e.BIRTHYR BETWEEN c.first_year AND c.last_year
    GROUP BY c.cohort, e.VETWWII
    ORDER BY MIN(c.cohort_order), e.VETWWII")
  result <- DBI::dbGetQuery(con, query)
  fail_if(nrow(result) != 2L * nrow(cohorts) ||
            !identical(result$cohort, rep(cohorts$cohort, each = 2L)) ||
            !identical(result$veteran_code, rep(c(1L, 2L), nrow(cohorts))),
          "Expected non-veteran and veteran rows for every cohort")
  fail_if(any(!is.finite(result$weighted_n)) ||
            any(result$weighted_n <= 0) ||
            any(result$school_valid_weighted <= 0) ||
            any(result$college_weighted > result$school_valid_weighted) ||
            any(result$scientist_weighted > result$weighted_n) ||
            any(result$school_valid_n + result$school_missing_n != result$sample_n) ||
            any(abs(result$school_valid_weighted +
                      result$school_missing_weighted - result$weighted_n) > 1e-6),
          "Weighted counts or school denominators failed validation")
  result$veteran_status <- ifelse(result$veteran_code == 2L,
                                  "Veteran", "Non-veteran")
  result$college_share_pct <- 100 * result$college_weighted /
    result$school_valid_weighted
  result$scientist_share_pct <- 100 * result$scientist_weighted /
    result$weighted_n
  result <- result[, c("cohort", "veteran_status", "veteran_code",
                       "sample_n", "weighted_n", "school_valid_n",
                       "school_valid_weighted", "school_missing_n",
                       "school_missing_weighted", "college_n",
                       "college_weighted", "college_share_pct",
                       "scientist_n", "scientist_weighted",
                       "scientist_share_pct")]
  contrasts <- do.call(rbind, lapply(unique(result$cohort), function(cohort) {
    rows <- result[result$cohort == cohort, , drop = FALSE]
    veteran <- rows[rows$veteran_code == 2L, , drop = FALSE]
    nonveteran <- rows[rows$veteran_code == 1L, , drop = FALSE]
    data.frame(cohort = cohort,
               college_veteran_minus_nonveteran_pp =
                 veteran$college_share_pct - nonveteran$college_share_pct,
               scientist_veteran_minus_nonveteran_pp =
                 veteran$scientist_share_pct - nonveteran$scientist_share_pct)
  }))
  rownames(contrasts) <- NULL
  list(groups = result, contrasts = contrasts)
}

main <- function() {
  parquet_dir <- Sys.getenv("IPUMS_1950_SAMPLE_LINE_PARQUET_DIR",
                            unset = "D:/ipums_linked_1950_sample_line")
  output_dir <- file.path(data_root(), "results/mandatory_schooling",
                          "linked_1950_sample_line_veterans")
  summary <- summarize_veterans(parquet_dir)
  group_totals <- aggregate(sample_n ~ cohort, summary$groups, sum)
  expected <- c("1921-1927" = 866793, "1910-1927" = 2345225)
  if (identical(sql_path(normalizePath(parquet_dir, mustWork = TRUE)),
                "D:/ipums_linked_1950_sample_line")) {
    fail_if(any(group_totals$sample_n != unname(expected[group_totals$cohort])),
            "Production eligible sample sizes changed unexpectedly")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary$groups, file.path(output_dir, "group_summary.csv"),
                   row.names = FALSE)
  utils::write.csv(summary$contrasts,
                   file.path(output_dir, "share_differences.csv"), row.names = FALSE)
  notes <- c(
    "Weighted 1950 sample-line WWII veteran comparisons",
    paste("Input:", normalizePath(parquet_dir, winslash = "/")),
    paste("Run UTC:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    "Cohorts use inclusive BIRTHYR ranges 1921-1927 and 1910-1927; the latter includes the former.",
    "Universe: YEAR=1950, SLREC=2, SEX=1, positive SLWT, VETWWII=1 (No) or 2 (Yes). VETWWII=0 (N/A) is excluded.",
    "Numbers labeled weighted are sums of 1950 SLWT; raw sample counts are separate.",
    "College attendance: HIGRADED 151-230, including an unfinished first college year. Codes 0 and 999 and SQL NULL are missing; the college share uses respondents with valid schooling as its denominator.",
    "Scientist occupation: existing scientist_occ1950 flag for OCC1950 007, 061, 062, 063, 067, 068, or 069. Its share uses all eligible men in the veteran group as denominator.",
    "Share differences are veteran minus non-veteran percentage points. These are descriptive, with no adjustments or inference.",
    "The linked extract selects census records with available linkage. SLWT does not correct that selection; weighted numbers should not be interpreted as full national population totals."
  )
  writeLines(notes, file.path(output_dir, "notes.txt"))
  print(summary$groups[, c("cohort", "veteran_status", "sample_n", "weighted_n",
                           "college_weighted", "college_share_pct",
                           "scientist_weighted", "scientist_share_pct")],
        row.names = FALSE)
  print(summary$contrasts, row.names = FALSE)
  message("Results written to ", output_dir)
}

if (sys.nframe() == 0L) main()
