# County-level descriptive statistics for the schooling measures before and after
# the first compulsory law, linked sample, 13 states with a first law in 1907-1920.
#
# Periods, per county:
#   before          the last 5 unexposed cohorts (rel_cohort -5..-1)
#   after           the first 5 exposed cohorts (0..+4) -- only partly covered
#   fully exposed   the first 5 cohorts from the state's first fully exposed one
#                   (rel_cohort law_min .. law_min + 4)
# Counties need >= 25 men both before and after; the fully exposed statistics use
# those counties that also have >= 25 men fully exposed.
#
# Panel A  distribution across counties of each measure in each period
# Panel B  the county change (after - before, fully exposed - before): mean, median,
#          SD, share rising, share significant at 5% on its own sampling error, and
#          the split-half reliability of the change (Spearman-Brown)
# Panel C  sample sizes
#
# Input:  output/mandatory_schooling/attendance_cohort/students.parquet
# Output: results/mandatory_schooling/attendance_cohort/county_measures_pre_post.{csv,tex}
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/describe_county_measures_pre_post.R

suppressPackageStartupMessages({
  library("dplyr")
  library("tidyr")
  library("readr")
  library("DBI")
  library("duckdb")
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

data_root <- local({
  env <- new.env()
  suppressWarnings(try(sys.source(file.path(repo_root, "paths.R"), envir = env), silent = TRUE))
  candidates <- c(
    get0("TALENT_DETS_DATA_DIR", envir = env, ifnotfound = ""),
    Sys.getenv("DET_DIR"),
    file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox/gtl_talent_dets")
  )
  hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
  if (!length(hit)) stop("Could not locate the Dropbox data root. Set TALENT_DETS_DATA_DIR or DET_DIR.")
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
})

STUDENTS <- file.path(data_root, "output/mandatory_schooling/attendance_cohort/students.parquet")
OUT_DIR <- file.path(data_root, "results/mandatory_schooling/attendance_cohort")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

MIN_MEN <- 25L
PERIODS <- c(before = "Before (-5 to -1)", after = "After (0 to +4)", full = "Fully exposed (first 5)")
MEASURES <- c(ever_attended = "Ever attended (%)", meets_min = "Meets law's minimum (%)",
              years_hs = "Years of schooling, capped at 12", hs_complete = "Completed high school (%)",
              years = "Years of schooling, uncapped")
SHORT <- c(ever_attended = "Ever attended", meets_min = "Meets minimum", years_hs = "Years (cap 12)",
           hs_complete = "High school", years = "Years (uncapped)")
BINARY <- c(ever_attended = TRUE, meets_min = TRUE, years_hs = FALSE, hs_complete = TRUE, years = FALSE)

# --------------------------------------------------------------------------- #
# 1. County x period (x half) aggregates
# --------------------------------------------------------------------------- #

m_sql <- paste(sprintf("avg(%1$s) AS %1$s, var_samp(%1$s) AS var_%1$s", names(MEASURES)), collapse = ", ")
period_sql <- "CASE WHEN rel_cohort BETWEEN -5 AND -1 THEN 'before'
                    WHEN rel_cohort BETWEEN 0 AND 4 THEN 'after' END"
full_sql <- "CASE WHEN rel_cohort BETWEEN law_min AND law_min + 4 THEN 'full' END"
con <- dbConnect(duckdb::duckdb())
q <- function(period_expr, by_half) dbGetQuery(con, sprintf("
  SELECT county_geoid, any_value(school_statename) AS statename, %s AS period, %s count(*) AS n, %s
  FROM read_parquet('%s')
  WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL AND in_law_state
    AND %s IS NOT NULL
  GROUP BY ALL", period_expr, if (by_half) "half," else "", m_sql,
  gsub("\\\\", "/", STUDENTS), period_expr))
agg <- bind_rows(q(period_sql, FALSE), q(full_sql, FALSE))
agg_half <- q(period_sql, TRUE)
dbDisconnect(con, shutdown = TRUE)

keep <- agg %>% filter(period %in% c("before", "after")) %>% group_by(county_geoid) %>%
  summarise(ok = sum(n >= MIN_MEN) == 2L, .groups = "drop") %>% filter(ok) %>% pull(county_geoid)
cells <- agg %>% filter(county_geoid %in% keep, n >= MIN_MEN)

# --------------------------------------------------------------------------- #
# 2. Panel A: distribution across counties
# --------------------------------------------------------------------------- #

long <- cells %>%
  pivot_longer(all_of(names(MEASURES)), names_to = "measure", values_to = "value") %>%
  select(county_geoid, statename, period, n, measure, value)
panel_a <- long %>% group_by(measure, period) %>%
  summarise(counties = n(), mean = mean(value), mean_men_weighted = weighted.mean(value, n), sd = sd(value),
            p10 = quantile(value, 0.10), p25 = quantile(value, 0.25), median = median(value),
            p75 = quantile(value, 0.75), p90 = quantile(value, 0.90), .groups = "drop")

# --------------------------------------------------------------------------- #
# 3. Panel B: the county change
# --------------------------------------------------------------------------- #

wide <- cells %>% select(county_geoid, period, n, all_of(names(MEASURES)), starts_with("var_")) %>%
  pivot_wider(names_from = period, values_from = -c(county_geoid, period))
change_stats <- function(to) bind_rows(lapply(names(MEASURES), function(m) {
  d <- wide %>% transmute(
    delta = .data[[paste0(m, "_", to)]] - .data[[paste0(m, "_before")]],
    se = if (BINARY[[m]]) {
      p1 <- .data[[paste0(m, "_", to)]]; p0 <- .data[[paste0(m, "_before")]]
      sqrt(p1 * (1 - p1) / .data[[paste0("n_", to)]] + p0 * (1 - p0) / .data$n_before)
    } else {
      sqrt(.data[[paste0("var_", m, "_", to)]] / .data[[paste0("n_", to)]] +
             .data[[paste0("var_", m, "_before")]] / .data$n_before)
    }) %>% filter(!is.na(delta))
  # A county at exactly 0% or 100% on both sides has a zero binomial SE; count it as
  # not significant rather than dividing by zero.
  data.frame(measure = m, change = to, counties = nrow(d), mean_change = mean(d$delta),
             median_change = median(d$delta), sd_change = sd(d$delta),
             share_increase = mean(d$delta > 0), share_decrease = mean(d$delta < 0),
             share_significant = mean(d$se > 0 & abs(d$delta) > 1.96 * d$se),
             share_sig_increase = mean(d$se > 0 & d$delta > 1.96 * d$se))
}))
panel_b <- bind_rows(change_stats("after"), change_stats("full"))

# Split-half reliability of the after - before change, counties with >= 25 men on
# each side in each half.
rel <- agg_half %>% filter(county_geoid %in% keep) %>%
  select(county_geoid, half, period, n, all_of(names(MEASURES))) %>%
  pivot_wider(names_from = c(period, half), values_from = c(n, all_of(names(MEASURES))))
ok_half <- with(rel, n_before_0 >= MIN_MEN & n_after_0 >= MIN_MEN & n_before_1 >= MIN_MEN & n_after_1 >= MIN_MEN)
rel <- rel[which(ok_half), ]
reliability <- bind_rows(lapply(names(MEASURES), function(m) {
  d0 <- rel[[paste0(m, "_after_0")]] - rel[[paste0(m, "_before_0")]]
  d1 <- rel[[paste0(m, "_after_1")]] - rel[[paste0(m, "_before_1")]]
  r <- cor(d0, d1)
  data.frame(measure = m, change = "after", reliability_counties = nrow(rel),
             split_half_r = r, reliability_sb = 2 * r / (1 + r))
}))
panel_b <- left_join(panel_b, reliability, by = c("measure", "change"))

# --------------------------------------------------------------------------- #
# 4. Panel C: sample
# --------------------------------------------------------------------------- #

panel_c <- cells %>% group_by(period) %>%
  summarise(counties = n_distinct(county_geoid), states = n_distinct(statename), men = sum(n),
            median_men_per_county = median(n), .groups = "drop") %>%
  arrange(match(period, names(PERIODS)))

stopifnot(all(panel_a$p10 <= panel_a$median & panel_a$median <= panel_a$p90),
          all(between(panel_b$share_increase, 0, 1)), all(between(panel_b$share_significant, 0, 1)))

# --------------------------------------------------------------------------- #
# 5. Write
# --------------------------------------------------------------------------- #

tidy <- bind_rows(
  panel_a %>% pivot_longer(-c(measure, period), names_to = "statistic") %>% mutate(panel = "A"),
  panel_b %>% pivot_longer(-c(measure, change), names_to = "statistic") %>% rename(period = change) %>%
    mutate(period = paste0(period, "_minus_before"), panel = "B"),
  panel_c %>% pivot_longer(-period, names_to = "statistic") %>% mutate(measure = NA_character_, panel = "C")) %>%
  select(panel, measure, period, statistic, value)
write_csv(tidy, file.path(OUT_DIR, "county_measures_pre_post.csv"))

# Formatting: shares in percentage points, years to two decimals.
fmt <- function(x, m, pp = BINARY[[m]]) if (pp) sprintf("%.1f", 100 * x) else sprintf("%.2f", x)
fmt_signed <- function(x, m) if (BINARY[[m]]) sprintf("%+.2f", 100 * x) else sprintf("%+.3f", x)
tex_esc <- function(x) gsub("%", "\\\\%", gsub("_", "\\\\_", x))

rows_a <- unlist(lapply(names(MEASURES), function(m) {
  c(sprintf("\\multicolumn{9}{l}{\\textit{%s}} \\\\", tex_esc(MEASURES[[m]])),
    sapply(names(PERIODS), function(p) {
      r <- filter(panel_a, measure == m, period == p)
      sprintf("\\quad %s & %d & %s & %s & %s & %s & %s & %s & %s \\\\", PERIODS[[p]], r$counties,
              fmt(r$mean, m), fmt(r$mean_men_weighted, m), fmt(r$sd, m),
              fmt(r$p10, m), fmt(r$median, m), fmt(r$p90, m),
              fmt(r$p75 - r$p25, m))
    }))
}))
rows_b <- unlist(lapply(names(MEASURES), function(m) {
  sapply(c("after", "full"), function(ch) {
    r <- filter(panel_b, measure == m, change == ch)
    sprintf("%s & %s & %d & %s & %s & %s & %.0f & %.0f & %s \\\\",
            if (ch == "after") tex_esc(SHORT[[m]]) else "",
            if (ch == "after") "After" else "Fully exp.",
            r$counties, fmt_signed(r$mean_change, m), fmt_signed(r$median_change, m), fmt(r$sd_change, m),
            100 * r$share_increase, 100 * r$share_significant,
            if (is.na(r$reliability_sb)) "--" else sprintf("%.2f", r$reliability_sb))
  })
}))
rows_c <- sapply(seq_len(nrow(panel_c)), function(i) with(panel_c[i, ], sprintf(
  "%s & %d & %d & %s & %.0f \\\\", PERIODS[[period]], counties, states, format(men, big.mark = ","), median_men_per_county)))

tex <- c(
  "% Generated by analysis/mandatory_schooling/describe_county_measures_pre_post.R -- do not edit by hand.",
  "\\begin{table}[!htbp]\\centering\\footnotesize\\setlength{\\tabcolsep}{3.5pt}",
  "\\caption{County-level schooling before and after the first compulsory law}",
  "\\label{tab:county_measures_pre_post}",
  "\\begin{tabular}{lrrrrrrrr}",
  "\\toprule",
  "\\multicolumn{9}{l}{\\textbf{Panel A. Distribution across counties}} \\\\",
  "& Counties & Mean & Men-weighted & SD & p10 & Median & p90 & IQR \\\\",
  "\\midrule", rows_a,
  "\\midrule",
  "\\multicolumn{9}{l}{\\textbf{Panel B. County change}} \\\\",
  "Measure & vs.\\ before & Counties & Mean & Median & SD & \\% rising & \\% signif. & Reliab. \\\\",
  "\\midrule", rows_b,
  "\\midrule",
  "\\multicolumn{9}{l}{\\textbf{Panel C. Sample}} \\\\",
  "Period & Counties & States & Men & Median men/county & & & & \\\\",
  "\\midrule", paste0(sub(" \\\\\\\\$", "", rows_c), " & & & & \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.95\\linewidth}\\vspace{4pt}\\scriptsize",
  paste0("\\textit{Notes:} White US-born non-migrant men in the IPUMS MLP linked sample, 13 states whose first compulsory ",
         "law took effect in 1907--1920, placed at their earliest linked record (age $\\le$ 18). Counties with at least ",
         MIN_MEN, " men both before and after; fully exposed statistics cover those that also have ", MIN_MEN,
         " men fully exposed. Percentages and percentage-point changes for shares; years for schooling. ",
         "\\% signif.\\ is the share of counties whose change exceeds 1.96 times its own sampling error. ",
         "Reliability is the Spearman--Brown split-half reliability of the after $-$ before change ",
         "(random halves of each county's men)."),
  "\\end{minipage}",
  "\\end{table}")
writeLines(tex, file.path(OUT_DIR, "county_measures_pre_post.tex"), useBytes = TRUE)

# --------------------------------------------------------------------------- #
# Console
# --------------------------------------------------------------------------- #

options(width = 220)
cat("\nPanel A\n")
print(as.data.frame(panel_a %>% arrange(match(measure, names(MEASURES)), match(period, names(PERIODS))) %>%
  mutate(across(mean:p90, ~ ifelse(BINARY[measure], 100 * .x, .x)), across(where(is.double), ~ round(.x, 3)))),
  row.names = FALSE)
cat("\nPanel B\n")
print(as.data.frame(panel_b %>% arrange(match(measure, names(MEASURES)), change) %>%
  mutate(across(c(mean_change, median_change, sd_change), ~ ifelse(BINARY[measure], 100 * .x, .x)),
         across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)
cat("\nPanel C\n"); print(as.data.frame(panel_c), row.names = FALSE)
message("Wrote: ", OUT_DIR)
