# Reuse saved regression estimates and export the paper-cohort descriptive table.
# Run from any directory; data and result paths follow the existing env overrides.
suppressPackageStartupMessages(library(ggplot2))
paper_repo_root <- function() {
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
paper_cohorts <- data.frame(cohort = c("1922-1927", "1910-1921"),
  first_year = c(1922L, 1910L), last_year = c(1927L, 1921L))

main <- function() {
  repo <- paper_repo_root()
  source(file.path(repo,
    "analysis/mandatory_schooling/describe_linked_1950_sample_line_veterans.R"),
    local = TRUE)
  result_root <- file.path(data_root(), "results/mandatory_schooling")
  regression_dir <- Sys.getenv("VETERAN_SCIENTIST_RESULTS_DIR",
    file.path(result_root, "linked_1950_sample_line_veteran_scientist"))
  descriptive_dir <- file.path(result_root, "linked_1950_sample_line_veterans")
  coefficients <- read.csv(file.path(regression_dir, "coefficients.csv"),
    stringsAsFactors = FALSE)
  plotdata <- coefficients[coefficients$cohort %in% paper_cohorts$cohort &
    coefficients$sensitivity == "Main" & coefficients$specification == 3 &
    coefficients$inference == "cluster", ]
  scopes <- c("All men", "US-W", "B", "NW", "SE")
  expected_keys <- as.vector(outer(paper_cohorts$cohort, scopes, paste, sep = ":"))
  keys <- paste(plotdata$cohort, plotdata$group, sep = ":")
  fail_if(nrow(plotdata) != 10L || anyDuplicated(keys) > 0L ||
    !setequal(keys, expected_keys) || any(plotdata$status != "Estimated") ||
    any(!is.finite(as.matrix(plotdata[c("estimate_pp", "ci_lower_pp", "ci_upper_pp")]))),
    "Expected ten estimated paper-cohort coefficients with finite intervals")
  fail_if(any(abs(plotdata$estimate_pp - 100 * plotdata$estimate) > 1e-10) ||
    any(abs(plotdata$ci_lower_pp - 100 * plotdata$ci_lower) > 1e-10) ||
    any(abs(plotdata$ci_upper_pp - 100 * plotdata$ci_upper) > 1e-10) ||
    any(plotdata$ci_lower_pp > plotdata$estimate_pp) ||
    any(plotdata$ci_upper_pp < plotdata$estimate_pp), "Invalid percentage-point intervals")
  plotdata$group <- factor(plotdata$group, levels = rev(scopes))
  plotdata$cohort <- factor(plotdata$cohort, levels = paper_cohorts$cohort)
  p <- ggplot(plotdata, aes(x = estimate_pp, y = group)) +
    geom_vline(xintercept = 0, color = "grey65", linetype = 2) +
    geom_segment(aes(x = ci_lower_pp, xend = ci_upper_pp, yend = group),
      color = "#24618C", linewidth = .7) + geom_point(color = "#24618C", size = 2.4) +
    facet_wrap(~cohort, ncol = 2, scales = "fixed") +
    labs(x = "Adjusted veteran difference (percentage points; 95% CI)", y = NULL,
      title = "WWII veteran status and scientist occupation in 1950",
      subtitle = "Full prewar controls; 1950 sample-line weights; county-clustered inference") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
  # Check the actual plotted points and intervals against the saved CSV values.
  layers <- ggplot_build(p)$data
  stopifnot(isTRUE(all.equal(layers[[2]]$x, plotdata$ci_lower_pp)),
    isTRUE(all.equal(layers[[2]]$xend, plotdata$ci_upper_pp)),
    isTRUE(all.equal(layers[[3]]$x, plotdata$estimate_pp)))
  ggsave(file.path(regression_dir, "coefficients_paper_cohorts.png"), p,
    width = 10, height = 3.8, dpi = 180)
  ggsave(file.path(regression_dir, "coefficients_paper_cohorts.pdf"), p,
    width = 10, height = 3.8)
  input <- Sys.getenv("IPUMS_1950_SAMPLE_LINE_PARQUET_DIR",
    "D:/ipums_linked_1950_sample_line")
  summary <- summarize_veterans(input, paper_cohorts)
  g <- summary$groups
  if (identical(sql_path(normalizePath(input, mustWork = TRUE)),
      "D:/ipums_linked_1950_sample_line")) {
    totals <- vapply(paper_cohorts$cohort,
      function(cohort) sum(g$sample_n[g$cohort == cohort]), numeric(1))
    stopifnot(identical(unname(totals), c(742139, 1603086)))
  }
  dir.create(descriptive_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(g, file.path(descriptive_dir, "paper_cohort_group_summary.csv"), row.names = FALSE)
  write.csv(summary$contrasts, file.path(descriptive_dir,
    "paper_cohort_share_differences.csv"), row.names = FALSE)
  writeLines(c("Paper cohorts: inclusive BIRTHYR 1922-1927 and 1910-1921; nonoverlapping.",
    "YEAR=1950; SLREC=2; SEX=1; positive SLWT; VETWWII=1 or 2.",
    "All eligible men; no prewar complete-case restriction or active-military exclusion.",
    "College: HIGRADED 151-230; denominator excludes 0, 999 and NULL attainment.",
    "Scientist: inherited seven-code scientist_occ1950; denominator is all eligible men.",
    "Weighted counts and shares use 1950 SLWT; no correction for linkage selection.",
    paste("Input:", normalizePath(input, winslash = "/")),
    paste("Run UTC:", format(Sys.time(), tz = "UTC", usetz = TRUE))),
    file.path(descriptive_dir, "paper_cohort_notes.txt"))
  number <- function(x, digits = 0L) formatC(x, format = "f", digits = digits, big.mark = ",")
  rows <- vapply(seq_len(nrow(g)), function(i) paste0(paste(c(
    gsub("-", "--", g$cohort[i], fixed = TRUE), g$veteran_status[i],
    number(g$sample_n[i]), number(g$weighted_n[i]), number(g$college_weighted[i]),
    number(g$college_share_pct[i], 2L), number(g$scientist_weighted[i]),
    number(g$scientist_share_pct[i], 3L)), collapse = " & "), " \\\\"), character(1))
  tex_path <- file.path(repo, "project/wwII_gi_bill.tex")
  tex <- readLines(tex_path, warn = FALSE)
  start <- which(tex == "\\midrule")
  end <- which(tex == "\\bottomrule")
  stopifnot(length(start) == 1L, length(end) == 1L, start < end)
  tex <- c(tex[seq_len(start)], rows[1:2], "\\addlinespace", rows[3:4],
    tex[seq.int(end, length(tex))])
  tex <- sub("The expanded 1910--1927 cohort includes the 1921--1927 cohort. Veterans have",
    "The 1922--1927 and 1910--1921 cohorts do not overlap. Veterans have", tex, fixed = TRUE)
  writeLines(tex, tex_path)
  # Reconcile every rendered table row with values read back from the saved CSV.
  saved <- read.csv(file.path(descriptive_dir, "paper_cohort_group_summary.csv"))
  stopifnot(isTRUE(all.equal(g, saved, check.attributes = FALSE)),
    identical(readLines(tex_path)[grep(" & (Non-veteran|Veteran) &", readLines(tex_path))], rows))
  print(g[c("cohort", "veteran_status", "sample_n", "weighted_n",
    "college_weighted", "college_share_pct", "scientist_weighted", "scientist_share_pct")],
    row.names = FALSE)
  message("Paper-cohort plot, summaries, and LaTeX table saved.")
}
if (sys.nframe() == 0L) main()
