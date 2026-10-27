# Observed WWII-veteran share, 1910Q1-1940Q4; no Korean-only exclusion.
# Run with Rscript from any working directory. Uses the IV script's path helpers.
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(arg) && sub("^--file=", "", arg[1]) != "-") {
  script_dir <- dirname(normalizePath(sub("^--file=", "", arg[1]), winslash = "/"))
} else {
  script_dir <- file.path(Sys.getenv("GTL_REPO", unset = getwd()), "analysis/mandatory_schooling")
}
source(file.path(script_dir, "estimate_wwii_service_scientist_1960_iv.R"))

plot_wwii_cohort_share <- function() {
  paths <- ws_paths()
  ws_fail(!file.exists(paths$csv) || !file.exists(paths$ddi), "Census CSV or DDI missing")
  dir.create(file.path(paths$out, "figures"), recursive = TRUE, showWarnings = FALSE)
  input <- c(csv = paths$csv, ddi = paths$ddi)
  input_hash <- tools::md5sum(input)
  # Repeated runs replace only files belonging to this plot.
  output_files <- file.path(paths$out, c("wwii_veteran_share_by_birth_quarter_1910_1940.csv",
    "wwii_veteran_share_by_birth_quarter_1910_1940_manifest.csv",
    "figures/wwii_veteran_share_by_birth_quarter_1910_1940.png",
    "figures/wwii_veteran_share_by_birth_quarter_1910_1940.pdf"))
  protected <- setdiff(list.files(paths$out, full.names = TRUE, recursive = TRUE), output_files)
  protected <- protected[!dir.exists(protected)]
  protected_hash <- tools::md5sum(protected)
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dbExecute(con, "SET threads=4")
  dbExecute(con, "SET memory_limit='4GB'")
  csv <- paste0("read_csv_auto(", dbQuoteString(con, gsub("\\\\", "/", paths$csv)),
    ",sample_size=30000)")
  schema <- dbGetQuery(con, paste0("DESCRIBE SELECT * FROM ", csv))
  ws_ddi(paths$ddi, schema$column_name)
  ws_say("Aggregating WWII-veteran shares for 124 birth-quarter cohorts")
  cohorts <- as.data.table(dbGetQuery(con, paste0(
    "SELECT BIRTHYR,BIRTHQTR,COUNT(*) AS observations,",
    "COUNT(*) FILTER(WHERE VETWWII=2) AS wwii_veterans,",
    "SUM(PERWT) AS weighted_population,",
    "SUM(CASE WHEN VETWWII=2 THEN PERWT ELSE 0 END) AS weighted_wwii_veterans ",
    "FROM ", csv, " WHERE YEAR=1960 AND SEX=1 ",
    "AND (BPL BETWEEN 1 AND 56 OR BPL IN (90,99)) ",
    "AND BIRTHYR BETWEEN 1910 AND 1940 AND BIRTHQTR BETWEEN 1 AND 4 ",
    "AND VETWWII IN (1,2) AND ISFINITE(PERWT) AND PERWT>0 ",
    "AND NOT COALESCE(OCC1950=595 OR EMPSTATD BETWEEN 13 AND 15 OR CLASSWKRD=26,FALSE) ",
    "GROUP BY BIRTHYR,BIRTHQTR ORDER BY BIRTHYR,BIRTHQTR")))
  cohorts[, `:=`(BIRTHYR = as.integer(BIRTHYR), BIRTHQTR = as.integer(BIRTHQTR))]
  cohorts[, `:=`(cohort = paste0(BIRTHYR, "Q", BIRTHQTR),
    quarter_index = 4L * (BIRTHYR - 1910L) + BIRTHQTR - 1L,
    birth_time = BIRTHYR + (BIRTHQTR - 1) / 4,
    wwii_share_pct = 100 * weighted_wwii_veterans / weighted_population)]
  ws_fail(nrow(cohorts) != 124L || anyDuplicated(cohorts$cohort) > 0L ||
    !identical(cohorts$quarter_index, 0:123), "Birth-quarter coverage is incomplete")
  ws_fail(any(!is.finite(cohorts$weighted_population) | cohorts$weighted_population <= 0) ||
    any(!is.finite(cohorts$wwii_share_pct) | cohorts$wwii_share_pct < 0 | cohorts$wwii_share_pct > 100),
    "Invalid cohort weights or veteran shares")
  ws_fail(sum(cohorts$observations) != 1570544,
    "Respondent total differs from inspected 1,570,544")
  ws_fail(abs(cohorts[cohort == "1927Q4", wwii_share_pct] - 57.3183842727692) > 1e-8 ||
    abs(cohorts[cohort == "1928Q1", wwii_share_pct] - 44.7797535600974) > 1e-8,
    "Adjacent-cutoff shares differ from inspected values")
  # x=0 in 1927Q4 and x=1 in 1928Q1; mark the boundary between their points.
  boundary <- 1927 + 3.5 / 4
  chart <- ggplot(cohorts, aes(birth_time, wwii_share_pct)) +
    geom_vline(xintercept = boundary, linetype = "dashed", colour = "#6B7280", linewidth = .55) +
    geom_line(colour = "#176B91", linewidth = .7) +
    geom_point(colour = "#176B91", size = 1.5) +
    annotate("text", x = boundary + .3, y = 96, label = "1927Q4 / 1928Q1", hjust = 0,
      colour = "#4B5563", size = 3.5) +
    scale_x_continuous(breaks = seq(1910, 1940, 5), labels = seq(1910, 1940, 5),
      limits = c(1910, 1940.75), expand = expansion(mult = c(.01, .01))) +
    scale_y_continuous(breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%"),
      limits = c(0, 100), expand = expansion(mult = c(.01, .01))) +
    labs(title = "WWII-veteran share by birth quarter, 1910-1940",
      subtitle = "US-born civilian men observed in the 1960 census; Korean-only veterans retained",
      x = "Birth year (each point represents one quarter)", y = "Share who served in WWII",
      caption = "Source: IPUMS 1960 5% census, usa_00007. Shares use person weights (PERWT).\n124 birth-quarter cohorts; 1,570,544 respondents. Dashed line marks the IV cohort boundary.") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "#4B5563", size = 10.5),
      plot.caption = element_text(hjust = 0, colour = "#4B5563", size = 9),
      plot.margin = margin(12, 16, 12, 12))
  fwrite(cohorts, output_files[1])
  ggsave(output_files[3], chart, width = 11, height = 5.5, dpi = 200, bg = "white")
  ggsave(output_files[4], chart, width = 11, height = 5.5, bg = "white")
  ws_fail(!identical(input_hash, tools::md5sum(input)), "Census inputs changed")
  ws_fail(!identical(protected_hash, tools::md5sum(protected)), "Existing result files changed")
  fwrite(data.table(input = names(input), path = unname(input), md5 = unname(input_hash),
    observations = sum(cohorts$observations), cohorts = nrow(cohorts),
    timestamp = format(Sys.time(), tz = "America/Sao_Paulo", usetz = TRUE)), output_files[2])
  ws_say("Saved plot: ", output_files[3])
  print(cohorts[cohort %in% c("1927Q4", "1928Q1"), .(cohort, observations, wwii_share_pct)])
  invisible(output_files)
}

if (sys.nframe() == 0L) plot_wwii_cohort_share()
