# Weighted WWII veteran shares across birth cohorts observed in the 1950 census.
# Rscript analysis/mandatory_schooling/plot_linked_1950_veteran_share_by_birth_year.R
# Overrides: IPUMS_1950_SAMPLE_LINE_PARQUET_DIR and TALENT_DETS_DATA_DIR.
suppressPackageStartupMessages(library(ggplot2))

veteran_share_repo_root <- function() {
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
  source(file.path(veteran_share_repo_root(),
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
  shares <- DBI::dbGetQuery(con, paste0(
    "SELECT BIRTHYR AS birth_year, COUNT(*) AS sample_n,
      COUNT(*) FILTER (WHERE VETWWII=2) AS sample_veterans,
      COUNT(*) FILTER (WHERE VETWWII=1) AS sample_non_veterans,
      SUM(CAST(SLWT AS DOUBLE)) AS weighted_n,
      SUM(CASE WHEN VETWWII=2 THEN CAST(SLWT AS DOUBLE) ELSE 0 END) AS weighted_veterans,
      SUM(CASE WHEN VETWWII=1 THEN CAST(SLWT AS DOUBLE) ELSE 0 END) AS weighted_non_veterans
     FROM read_parquet(", parquet, ", hive_partitioning=true)
     WHERE YEAR=1950 AND SLREC=2 AND SEX=1 AND SLWT>0 AND VETWWII IN (1,2)
       AND BIRTHYR BETWEEN 1910 AND 1935
     GROUP BY BIRTHYR ORDER BY BIRTHYR"))
  fail_if(!identical(as.integer(shares$birth_year), 1910:1935) ||
    anyDuplicated(shares$birth_year) > 0L || anyNA(shares) ||
    any(shares$sample_n <= 0) || any(!is.finite(shares$weighted_n)) ||
    any(shares$weighted_n <= 0) || any(shares$sample_veterans < 0) ||
    any(shares$weighted_veterans < 0) ||
    any(shares$sample_veterans + shares$sample_non_veterans != shares$sample_n) ||
    any(abs(shares$weighted_veterans + shares$weighted_non_veterans - shares$weighted_n) > 1e-6),
    "Expected 26 consecutive birth cohorts with valid positive denominators")
  shares$veteran_share_pct <- 100 * shares$weighted_veterans / shares$weighted_n
  fail_if(any(!is.finite(shares$veteran_share_pct)) ||
    any(shares$veteran_share_pct < 0 | shares$veteran_share_pct > 100),
    "Veteran shares must be finite and between zero and 100 percent")
  if (identical(sql_path(normalizePath(input, mustWork = TRUE)),
      "D:/ipums_linked_1950_sample_line")) {
    prior_path <- file.path(output, "veterans_by_birth_year_1910_1935.csv")
    fail_if(!file.exists(prior_path), "Missing veteran count chart CSV: ", prior_path)
    prior <- read.csv(prior_path)
    stopifnot(identical(as.integer(prior$birth_year), 1910:1935),
      identical(as.numeric(shares$sample_veterans), as.numeric(prior$sample_veterans)),
      identical(as.numeric(shares$weighted_veterans), as.numeric(prior$weighted_veterans)),
      sum(shares$sample_veterans) == 1373034,
      sum(shares$weighted_veterans) == 6865170)
    reference <- c(`1922` = 76.20, `1928` = 51.88, `1929` = 21.09)
    stopifnot(all(round(shares$veteran_share_pct[
      match(as.integer(names(reference)), shares$birth_year)], 2) == unname(reference)))
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  stem <- file.path(output, "veteran_share_by_birth_year_1910_1935")
  write.csv(shares, paste0(stem, ".csv"), row.names = FALSE)
  saved <- read.csv(paste0(stem, ".csv"))
  stopifnot(isTRUE(all.equal(shares, saved, check.attributes = FALSE)))
  p <- ggplot(saved, aes(x = birth_year, y = veteran_share_pct)) +
    geom_line(color = "#24618C", linewidth = .85) +
    geom_point(color = "#24618C", size = 2.3) +
    scale_x_continuous(breaks = 1910:1935, minor_breaks = NULL,
      labels = function(x) sprintf("%d", as.integer(x))) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
      labels = scales::label_percent(scale = 1, accuracy = 1),
      expand = expansion(mult = c(0, 0))) +
    labs(x = "Birth year", y = "WWII veteran share (%)",
      title = "WWII veteran share in the 1950 sample line, by birth year",
      subtitle = "Male respondents born 1910-1935; shares weighted by 1950 SLWT",
      caption = paste(
        "Note: Male sample-line respondents observed in 1950, with positive SLWT and VETWWII=1 or 2.",
        "Share: sum(SLWT for veterans) / sum(SLWT for veterans and non-veterans); code 0 and missing status excluded.",
        "Points compare birth cohorts; lines connect annual shares without smoothing. SLWT does not correct linkage selection.",
        sep = "\n")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), axis.text.x = element_text(size = 9),
      plot.caption = element_text(hjust = 0, size = 8.5, lineheight = 1.15,
        margin = margin(t = 12)), plot.margin = margin(12, 16, 12, 12))
  layers <- ggplot_build(p)$data
  for (layer in layers) {
    stopifnot(identical(as.numeric(layer$x), as.numeric(saved$birth_year)),
      isTRUE(all.equal(as.numeric(layer$y), saved$veteran_share_pct)))
  }
  ggsave(paste0(stem, ".png"), p, width = 10, height = 5.5, dpi = 180)
  ggsave(paste0(stem, ".pdf"), p, width = 10, height = 5.5)
  print(shares[c("birth_year", "sample_n", "sample_veterans",
    "weighted_n", "weighted_veterans", "veteran_share_pct")], row.names = FALSE)
  message("Validated 26 annual veteran shares and reconciled the veteran count series.")
  message("Figure and underlying CSV saved to ", stem)
}

if (sys.nframe() == 0L) main()
