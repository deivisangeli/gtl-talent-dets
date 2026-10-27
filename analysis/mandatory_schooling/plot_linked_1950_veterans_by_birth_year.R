# WWII veteran counts across birth cohorts, observed in the 1950 sample line.
# Rscript analysis/mandatory_schooling/plot_linked_1950_veterans_by_birth_year.R
# Overrides: IPUMS_1950_SAMPLE_LINE_PARQUET_DIR and TALENT_DETS_DATA_DIR.
suppressPackageStartupMessages(library(ggplot2))

veteran_plot_repo_root <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  root <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]),
    winslash = "/")) else Sys.getenv("GTL_REPO", getwd())
  while (!file.exists(file.path(root, "paths.R"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("Cannot locate repository")
    root <- parent
  }
  root
}

main <- function() {
  source(file.path(veteran_plot_repo_root(),
    "analysis/mandatory_schooling/describe_linked_1950_sample_line_veterans.R"),
    local = TRUE)
  input <- Sys.getenv("IPUMS_1950_SAMPLE_LINE_PARQUET_DIR",
    "D:/ipums_linked_1950_sample_line")
  output <- file.path(data_root(), "results/mandatory_schooling",
    "linked_1950_sample_line_veterans")
  glob <- file.path(input, "birth_bpl=*", "*.parquet")
  fail_if(!length(Sys.glob(glob)), "No sample-line Parquet files at ", glob)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads=8")
  DBI::dbExecute(con, "SET memory_limit='20GB'")
  parquet <- as.character(DBI::dbQuoteString(con, sql_path(glob)))
  counts <- DBI::dbGetQuery(con, paste0(
    "SELECT BIRTHYR AS birth_year, COUNT(*) AS sample_veterans,
      SUM(CAST(SLWT AS DOUBLE)) AS weighted_veterans
     FROM read_parquet(", parquet, ", hive_partitioning=true)
     WHERE YEAR=1950 AND SLREC=2 AND SEX=1 AND SLWT>0 AND VETWWII=2
       AND BIRTHYR BETWEEN 1910 AND 1935
     GROUP BY BIRTHYR ORDER BY BIRTHYR"))
  # Preserve years with no eligible veterans as explicit zero-count cohorts.
  counts <- merge(data.frame(birth_year = 1910:1935), counts,
    by = "birth_year", all.x = TRUE, sort = TRUE)
  counts$sample_veterans[is.na(counts$sample_veterans)] <- 0
  counts$weighted_veterans[is.na(counts$weighted_veterans)] <- 0
  fail_if(!identical(as.integer(counts$birth_year), 1910:1935) ||
    anyDuplicated(counts$birth_year) > 0L || anyNA(counts) ||
    any(counts$sample_veterans < 0) ||
    any(!is.finite(counts$weighted_veterans)) || any(counts$weighted_veterans < 0),
    "Expected 26 consecutive birth cohorts with nonnegative finite veteran counts")
  if (identical(sql_path(normalizePath(input, mustWork = TRUE)),
      "D:/ipums_linked_1950_sample_line")) {
    stopifnot(sum(counts$sample_veterans[counts$birth_year <= 1930]) == 1360481,
      sum(counts$weighted_veterans[counts$birth_year <= 1930]) == 6802405,
      sum(counts$sample_veterans[counts$birth_year <= 1921]) == 717978,
      sum(counts$sample_veterans[counts$birth_year %in% 1922:1927]) == 546730)
    summary_path <- file.path(output, "paper_cohort_group_summary.csv")
    fail_if(!file.exists(summary_path), "Cannot reconcile saved descriptive table: ", summary_path)
    summary <- read.csv(summary_path, stringsAsFactors = FALSE)
    for (cohort in c("1910-1921", "1922-1927")) {
      bounds <- as.integer(strsplit(cohort, "-", fixed = TRUE)[[1]])
      years <- counts$birth_year >= bounds[1] & counts$birth_year <= bounds[2]
      row <- summary[summary$cohort == cohort & summary$veteran_code == 2, ]
      stopifnot(nrow(row) == 1L,
        sum(counts$sample_veterans[years]) == row$sample_n,
        sum(counts$weighted_veterans[years]) == row$weighted_n)
    }
    previous_path <- file.path(output, "veterans_by_birth_year_1910_1930.csv")
    if (file.exists(previous_path)) {
      previous <- read.csv(previous_path)
      stopifnot(isTRUE(all.equal(counts[counts$birth_year <= 1930, ],
        previous, check.attributes = FALSE)))
    }
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  stem <- file.path(output, "veterans_by_birth_year_1910_1935")
  write.csv(counts, paste0(stem, ".csv"), row.names = FALSE)
  saved <- read.csv(paste0(stem, ".csv"))
  stopifnot(isTRUE(all.equal(counts, saved, check.attributes = FALSE)))
  p <- ggplot(saved, aes(x = birth_year, y = weighted_veterans)) +
    geom_line(color = "#24618C", linewidth = .85) +
    geom_point(color = "#24618C", size = 2.3) +
    scale_x_continuous(breaks = 1910:1935, minor_breaks = NULL,
      labels = function(x) sprintf("%d", as.integer(x))) +
    scale_y_continuous(limits = c(0, NA), labels = scales::label_comma(),
      expand = expansion(mult = c(0, .08))) +
    labs(x = "Birth year", y = "Weighted number of WWII veterans",
      title = "WWII veterans in the 1950 sample line, by birth year",
      subtitle = "Male respondents born 1910-1935; counts weighted by 1950 SLWT",
      caption = paste(
        "Note: Male sample-line respondents in 1950 with VETWWII=2 and positive SLWT.",
        "Each point represents a birth cohort observed in 1950; lines connect annual counts without smoothing.",
        "Weighted counts represent the available linked extract; SLWT does not correct selection into census linkage.",
        sep = "\n")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), axis.text.x = element_text(size = 9),
      plot.caption = element_text(hjust = 0, size = 8.5, lineheight = 1.15,
        margin = margin(t = 12)), plot.margin = margin(12, 16, 12, 12))
  layers <- ggplot_build(p)$data
  stopifnot(identical(as.numeric(layers[[1]]$x), as.numeric(saved$birth_year)),
    identical(as.numeric(layers[[1]]$y), as.numeric(saved$weighted_veterans)),
    identical(as.numeric(layers[[2]]$x), as.numeric(saved$birth_year)),
    identical(as.numeric(layers[[2]]$y), as.numeric(saved$weighted_veterans)))
  ggsave(paste0(stem, ".png"), p, width = 10, height = 5.5, dpi = 180)
  ggsave(paste0(stem, ".pdf"), p, width = 10, height = 5.5)
  print(counts, row.names = FALSE)
  message("Validated 26 birth cohorts; total sample veterans = ",
    sum(counts$sample_veterans), "; weighted veterans = ", sum(counts$weighted_veterans))
  message("Figure and underlying CSV saved to ", stem)
}

if (sys.nframe() == 0L) main()
