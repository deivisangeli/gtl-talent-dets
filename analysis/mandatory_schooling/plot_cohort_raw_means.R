# Raw-mean discontinuity plots: the plain average over all men at each birth
# cohort relative to the exposure cutoff -- no fixed effects, no fitted lines, no
# demeaning, no minimum cell size.
#
# Same balanced window as plot_cohort_discontinuity.R: event times -5..+9 over the
# 11 law states with at least five unexposed cohorts after 1890 (all but Delaware
# and Oklahoma). Three views:
#
#   raw_means_pooled.png              the 11 states pooled, one panel per measure
#   raw_means_by_state_<measure>.png  one small multiple per state
#   raw_means_calendar.png            by calendar birth year, law states against the
#                                     states whose first law predates 1907
#
# Input:  output/mandatory_schooling/attendance_cohort/students.parquet
# Output: results/mandatory_schooling/attendance_cohort/raw_means*
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/plot_cohort_raw_means.R

suppressPackageStartupMessages({
  library("dplyr")
  library("tidyr")
  library("readr")
  library("ggplot2")
  library("patchwork")
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

K_MIN <- -5L
K_MAX <- 9L
DROP_STATES <- c("Delaware", "Oklahoma")

MEASURES <- c(ever_attended = "Share ever attended school",
              meets_min     = "Share completing the law's minimum years",
              years_hs      = "Years of schooling (capped at 12)",
              hs_complete   = "Share completing high school")
PCT <- c(ever_attended = TRUE, meets_min = TRUE, years_hs = FALSE, hs_complete = TRUE)

# --------------------------------------------------------------------------- #
# Sums by state x event time and by group x birth year; means and SEs follow.
# --------------------------------------------------------------------------- #

moments <- paste(unlist(lapply(names(MEASURES), function(m) sprintf(
  "count(%1$s) AS n_%1$s, sum(%1$s) AS s_%1$s, sum(%1$s * %1$s) AS ss_%1$s", m))), collapse = ",\n  ")
con <- dbConnect(duckdb::duckdb())
base <- sprintf("FROM read_parquet('%s') WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL",
                gsub("\\\\", "/", STUDENTS))
by_state <- dbGetQuery(con, sprintf("
  SELECT school_statename AS statename, any_value(T) AS T, any_value(T - exitage) AS first_exposed_yob,
         any_value(law_min) AS law_min, rel_cohort, %s
  %s AND role = 'law_state' AND rel_cohort BETWEEN %d AND %d
  GROUP BY school_statename, rel_cohort", moments, base, K_MIN, K_MAX)) %>%
  filter(!statename %in% DROP_STATES)
by_year <- dbGetQuery(con, sprintf("
  SELECT CASE WHEN role = 'law_state' THEN 'law' ELSE 'benchmark' END AS grp, yob, %s
  %s AND (role = 'benchmark' OR (role = 'law_state' AND school_statename NOT IN ('%s')))
  GROUP BY grp, yob", moments, base, paste(DROP_STATES, collapse = "','"))) %>%
  filter(yob >= 1891, yob <= 1915)
dbDisconnect(con, shutdown = TRUE)

to_long <- function(d, keys) d %>%
  pivot_longer(matches("^(n|s|ss)_"), names_to = c(".value", "measure"), names_pattern = "^(n|s|ss)_(.*)$") %>%
  group_by(across(all_of(c(keys, "measure")))) %>%
  summarise(n = sum(n), s = sum(s), ss = sum(ss), .groups = "drop") %>%
  mutate(mean = s / n, sd = sqrt(pmax(ss / n - mean^2, 0) * n / (n - 1)), se = sd / sqrt(n),
         lo = mean - 1.96 * se, hi = mean + 1.96 * se) %>%
  select(-s, -ss, -sd)

pooled <- to_long(by_state, "rel_cohort")
state <- to_long(by_state, c("statename", "rel_cohort")) %>%
  left_join(distinct(by_state, statename, T, first_exposed_yob, law_min), by = "statename")
calendar <- to_long(by_year, c("grp", "yob"))

stopifnot(n_distinct(state$statename) == 11L,
          all(count(distinct(state, statename, rel_cohort), rel_cohort)$n == 11L))

jump <- function(d) d %>% group_by(across(-c(rel_cohort, n, mean, se, lo, hi))) %>%
  summarise(jump = mean[rel_cohort == 0] - mean[rel_cohort == -1],
            jump_se = sqrt(se[rel_cohort == 0]^2 + se[rel_cohort == -1]^2), .groups = "drop")
pooled_jump <- jump(pooled)
state_jump <- jump(state)

full_range <- range(by_state$law_min)
cut_years <- range(by_state$first_exposed_yob)
# ever_attended lives within a couple of points of 100%, so it needs a decimal.
fmt_y <- function(m) if (!PCT[[m]]) scales::label_number(accuracy = 0.1) else
  scales::label_percent(accuracy = if (m == "ever_attended") 0.1 else 1)
fmt_d <- function(m, x) if (PCT[[m]]) sprintf("%+.1f pp", 100 * x) else sprintf("%+.2f", x)

theme_set(theme_minimal(base_size = 10) +
            theme(plot.title = element_text(size = 10, face = "bold"),
                  plot.subtitle = element_text(size = 8, colour = "grey30"),
                  panel.grid.minor = element_blank()))
event_layers <- function() list(
  annotate("rect", xmin = full_range[1] - 0.5, xmax = full_range[2] + 0.5, ymin = -Inf, ymax = Inf,
           fill = "steelblue", alpha = 0.07),
  geom_vline(xintercept = -0.5, linetype = "dashed", colour = "firebrick"),
  scale_x_continuous(breaks = K_MIN:K_MAX))

# --------------------------------------------------------------------------- #
# 1. Pooled
# --------------------------------------------------------------------------- #

panels <- lapply(names(MEASURES), function(m) {
  d <- filter(pooled, measure == m)
  j <- filter(pooled_jump, measure == m)
  ggplot(d, aes(rel_cohort, mean)) + event_layers() +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.6) +
    geom_line(colour = "grey30") + geom_point(size = 1.8) +
    geom_point(data = filter(d, rel_cohort %in% c(-1, 0)), colour = "firebrick", size = 2.4) +
    scale_y_continuous(labels = fmt_y(m)) +
    labs(x = "Birth cohort relative to cutoff", y = NULL, title = MEASURES[[m]],
         subtitle = sprintf("-1 -> 0: %s (SE %s)", fmt_d(m, j$jump), sub("^\\+", "", fmt_d(m, j$jump_se))))
})
fig <- wrap_plots(panels, ncol = 2) + plot_annotation(
  title = "Raw means around the cutoff, 11 states pooled",
  subtitle = paste0("All white US-born non-migrant men in the analysis sample, no fixed effects or weighting. ",
                    "Band: 95% CI. Red dashed line between the last unexposed (-1) and first exposed (0) cohort;\n",
                    "cohort 0 was at the leaving age when the law passed and owed no schooling. Shaded: first fully exposed cohort (+",
                    full_range[1], " to +", full_range[2], " by state)."),
  theme = theme(plot.title = element_text(size = 13, face = "bold"),
                plot.subtitle = element_text(size = 8.5, colour = "grey30")))
ggsave(file.path(OUT_DIR, "raw_means_pooled.png"), fig, width = 11, height = 8, dpi = 180, bg = "white")

# --------------------------------------------------------------------------- #
# 2. By state
# --------------------------------------------------------------------------- #

for (m in names(MEASURES)) {
  d <- filter(state, measure == m)
  labs_tbl <- filter(state_jump, measure == m) %>%
    mutate(strip = sprintf("%s (law %d, min %d yrs)\n-1 -> 0: %s", statename, T, law_min, fmt_d(m, jump)))
  d <- left_join(d, select(labs_tbl, statename, strip), by = "statename")
  p <- ggplot(d, aes(rel_cohort, mean)) + event_layers() +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.6) +
    geom_line(colour = "grey30") + geom_point(size = 1.2) +
    geom_point(data = filter(d, rel_cohort %in% c(-1, 0)), colour = "firebrick", size = 1.8) +
    facet_wrap(~ strip, ncol = 4, scales = "free_y") +
    scale_x_continuous(breaks = seq(K_MIN, K_MAX, 2)) +
    scale_y_continuous(labels = fmt_y(m)) +
    labs(x = "Birth cohort relative to cutoff", y = NULL,
         title = paste0(MEASURES[[m]], ": raw means by state"),
         subtitle = "Band: 95% CI. Red dashed line: cutoff between -1 and 0. Free y-axes.") +
    theme(strip.text = element_text(size = 8))
  ggsave(file.path(OUT_DIR, sprintf("raw_means_by_state_%s.png", m)), p, width = 13, height = 8.5, dpi = 170, bg = "white")
}

# --------------------------------------------------------------------------- #
# 3. Calendar birth year, law states against the benchmark
# --------------------------------------------------------------------------- #

# meets_min is undefined in benchmark states, which have no minimum of their own.
cal_panels <- lapply(names(MEASURES), function(m) {
  d <- filter(calendar, measure == m, !(m == "meets_min" & grp == "benchmark"))
  ggplot(d, aes(yob, mean, colour = grp, fill = grp)) +
    annotate("rect", xmin = cut_years[1] - 0.5, xmax = cut_years[2] + 0.5, ymin = -Inf, ymax = Inf,
             fill = "firebrick", alpha = 0.06) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, colour = NA) +
    geom_line() + geom_point(size = 1.2) +
    # Fixed limits so the meets_min panel (law states only) shares one legend.
    scale_colour_manual(values = c(law = "black", benchmark = "steelblue"), limits = c("law", "benchmark"),
                        labels = c(law = "11 law states", benchmark = "First law before 1907"), name = NULL) +
    scale_fill_manual(values = c(law = "grey50", benchmark = "steelblue"), limits = c("law", "benchmark"),
                      guide = "none") +
    scale_y_continuous(labels = fmt_y(m)) +
    labs(x = "Birth year", y = NULL, title = MEASURES[[m]]) +
    # One series only, so its legend would not merge with the others' when collected.
    if (m == "meets_min") guides(colour = "none") else NULL
})
fig_cal <- wrap_plots(cal_panels, ncol = 2, guides = "collect") + plot_annotation(
  title = "Raw means by calendar birth year",
  subtitle = sprintf("Shaded: the first exposed cohort across the 11 states (%d-%d). The benchmark has no law minimum, so meets_min is shown for law states only.",
                     cut_years[1], cut_years[2]),
  theme = theme(plot.title = element_text(size = 13, face = "bold"),
                plot.subtitle = element_text(size = 8.5, colour = "grey30"))) &
  theme(legend.position = "bottom")
ggsave(file.path(OUT_DIR, "raw_means_calendar.png"), fig_cal, width = 11, height = 8, dpi = 180, bg = "white")

# --------------------------------------------------------------------------- #
# Table and console
# --------------------------------------------------------------------------- #

write_csv(bind_rows(
  mutate(pooled, scope = "pooled_11_states"),
  mutate(select(state, -T, -first_exposed_yob, -law_min), scope = "state"),
  mutate(rename(calendar, group = grp), scope = "calendar")), file.path(OUT_DIR, "raw_means.csv"))

options(width = 200)
cat("\nPooled -1 -> 0 change\n")
print(as.data.frame(pooled_jump %>% left_join(
  pooled %>% filter(rel_cohort %in% c(-1, 0)) %>% select(measure, rel_cohort, mean, n) %>%
    pivot_wider(names_from = rel_cohort, values_from = c(mean, n)), by = "measure") %>%
    mutate(across(where(is.double), ~ signif(.x, 4)))), row.names = FALSE)
cat("\nBy state -1 -> 0 change\n")
print(as.data.frame(state_jump %>% mutate(z = round(jump / jump_se, 1), jump = round(jump, 4)) %>%
  select(statename, measure, jump, z) %>%
  pivot_wider(names_from = measure, values_from = c(jump, z))), row.names = FALSE)
cat(sprintf("\nMen in the pooled window: %s\n",
            format(sum(filter(pooled, measure == "ever_attended")$n), big.mark = ",")))
message("Wrote: ", OUT_DIR)
