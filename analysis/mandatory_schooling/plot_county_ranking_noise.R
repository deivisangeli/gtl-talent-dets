# Show graphically that the county ranking on the ever-attended increase is noise.
#
# The county increase is mean ever_attended over rel_cohort 0..4 minus -5..-1 (the
# classifier used in plot_discontinuity_by_attendance_gain.R), linked sample, 13
# treated states, counties with >= 25 men on each side in the sample used. Four
# panels:
#
#   1  half A vs half B: each county's increase measured on two random halves of
#      its men. A real ranking hugs the 45-degree line; this is a cloud.
#   2  the same for the pre-law LEVEL of ever-attended -- what a reliable county
#      ranking looks like, for contrast
#   3  caterpillar: every county's increase with its 95% CI against the median
#   4  observed spread of county increases against the spread produced if every
#      county had the same true increase (binomial simulation, each county at its
#      own pre-law rate and its own number of men, 200 draws)
#
# Input:  output/mandatory_schooling/attendance_cohort/students.parquet
# Output: results/mandatory_schooling/attendance_cohort/county_ranking_noise*
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/plot_county_ranking_noise.R

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

save_or_fail <- function(path, plot, ...) {
  tmp <- tempfile(fileext = ".png")
  withCallingHandlers(ggsave(tmp, plot, ...),
                      warning = function(w) stop("could not render ", path, ": ", conditionMessage(w)))
  for (attempt in 1:5) {
    if (isTRUE(file.copy(tmp, path, overwrite = TRUE))) return(invisible(path))
    Sys.sleep(2)
  }
  stop("could not copy the figure into ", path)
}

K_CLASS <- 5L
MIN_SIDE <- 25L
N_SIM <- 200L
set.seed(1940)

theme_set(theme_minimal(base_size = 10) +
            theme(plot.title = element_text(size = 10.5, face = "bold"),
                  plot.subtitle = element_text(size = 8.3, colour = "grey30"),
                  panel.grid.minor = element_blank()))

# --------------------------------------------------------------------------- #
# 1. County x side x half aggregates
# --------------------------------------------------------------------------- #

con <- dbConnect(duckdb::duckdb())
agg <- dbGetQuery(con, sprintf("
  SELECT county_geoid, any_value(school_statename) AS statename, half,
         CASE WHEN rel_cohort < 0 THEN 'pre' ELSE 'post' END AS side,
         count(*) AS n, sum(ever_attended) AS k
  FROM read_parquet('%s')
  WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL AND in_law_state
    AND rel_cohort BETWEEN %d AND %d
  GROUP BY county_geoid, half, side",
  gsub("\\\\", "/", STUDENTS), -K_CLASS, K_CLASS - 1L))
dbDisconnect(con, shutdown = TRUE)

county_stats <- function(d) d %>%
  group_by(county_geoid, statename, side) %>% summarise(n = sum(n), k = sum(k), .groups = "drop") %>%
  pivot_wider(names_from = side, values_from = c(n, k), values_fill = 0) %>%
  filter(n_pre >= MIN_SIDE, n_post >= MIN_SIDE) %>%
  mutate(p_pre = k_pre / n_pre, p_post = k_post / n_post, increase = p_post - p_pre,
         se = sqrt(p_post * (1 - p_post) / n_post + p_pre * (1 - p_pre) / n_pre))

full <- county_stats(agg)
halves <- lapply(0:1, function(h) county_stats(filter(agg, half == h)) %>%
                   mutate(group = if_else(increase > median(increase), "above", "below")))
both <- inner_join(halves[[1]], halves[[2]], by = c("county_geoid", "statename"), suffix = c("_a", "_b"))

r_inc <- cor(both$increase_a, both$increase_b)
r_lvl <- cor(both$p_pre_a, both$p_pre_b)
same_group <- mean(both$group_a == both$group_b)
med_a <- median(halves[[1]]$increase)
med_b <- median(halves[[2]]$increase)

# --------------------------------------------------------------------------- #
# 2. Panels 1-2: half A vs half B
# --------------------------------------------------------------------------- #

pp <- function(x) x * 100
lim_inc <- quantile(pp(c(both$increase_a, both$increase_b)), c(0.01, 0.99))
quads <- data.frame(xmin = c(pp(med_a), -Inf), xmax = c(Inf, pp(med_a)),
                    ymin = c(pp(med_b), -Inf), ymax = c(Inf, pp(med_b)))
p1 <- ggplot(both, aes(pp(increase_a), pp(increase_b))) +
  geom_rect(data = quads, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), inherit.aes = FALSE,
            fill = "seagreen", alpha = 0.07) +
  geom_vline(xintercept = pp(med_a), colour = "grey55") + geom_hline(yintercept = pp(med_b), colour = "grey55") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey30") +
  geom_point(alpha = 0.35, size = 1.1, colour = "#2471a3") +
  geom_smooth(method = "lm", formula = y ~ x, colour = "firebrick", fill = "grey75", linewidth = 0.8) +
  coord_equal(xlim = lim_inc, ylim = lim_inc) +
  annotate("label", x = lim_inc[1], y = lim_inc[2], hjust = 0, vjust = 1, size = 3, fill = "white",
           label = sprintf("r = %.2f\nSame group in both halves: %.0f%%\n(coin flip = 50%%)", r_inc, 100 * same_group)) +
  labs(x = "Increase measured on half A (pp)", y = "Increase measured on half B (pp)",
       title = "1. The ranking we want: ever-attended INCREASE",
       subtitle = "Each dot is a county. A real ranking would hug the dashed 45° line;\nshaded quadrants = both halves agree on above/below median.")

lim_lvl <- quantile(pp(c(both$p_pre_a, both$p_pre_b)), c(0.01, 1))
p2 <- ggplot(both, aes(pp(p_pre_a), pp(p_pre_b))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey30") +
  geom_point(alpha = 0.35, size = 1.1, colour = "#7d3c98") +
  geom_smooth(method = "lm", formula = y ~ x, colour = "firebrick", fill = "grey75", linewidth = 0.8) +
  coord_equal(xlim = lim_lvl, ylim = lim_lvl) +
  annotate("label", x = lim_lvl[1], y = lim_lvl[2], hjust = 0, vjust = 1, size = 3, fill = "white",
           label = sprintf("r = %.2f", r_lvl)) +
  labs(x = "Pre-law level, half A (%)", y = "Pre-law level, half B (%)",
       title = "2. For contrast: the pre-law LEVEL of ever-attended",
       subtitle = "Same counties, same halves. A reliable ranking lines up\nalong the diagonal.")

# --------------------------------------------------------------------------- #
# 3. Panel 3: caterpillar
# --------------------------------------------------------------------------- #

med_full <- median(full$increase)
cat_d <- full %>% arrange(increase) %>% mutate(rank = row_number(), lo = increase - 1.96 * se, hi = increase + 1.96 * se)
covers <- mean(cat_d$lo <= med_full & cat_d$hi >= med_full)
p3 <- ggplot(cat_d, aes(rank, pp(increase))) +
  geom_linerange(aes(ymin = pp(lo), ymax = pp(hi)), colour = "grey70", linewidth = 0.3) +
  geom_point(size = 0.6, colour = "#2471a3") +
  geom_hline(yintercept = pp(med_full), colour = "firebrick", linewidth = 0.7) +
  coord_cartesian(ylim = quantile(pp(c(cat_d$lo, cat_d$hi)), c(0.01, 0.99))) +
  labs(x = sprintf("Counties ranked by their increase (%d counties)", nrow(cat_d)),
       y = "Increase in ever-attended (pp), with 95% CI",
       title = "3. Every county's increase with its uncertainty",
       subtitle = sprintf("Red: the median county. %.0f%% of counties' intervals include the median,\nso they cannot be told apart from it.", 100 * covers))

# --------------------------------------------------------------------------- #
# 4. Panel 4: observed spread vs pure noise
# --------------------------------------------------------------------------- #

# Null: every county has the same true increase (the mean), its own pre-law rate
# and its own numbers of men; the only thing that varies is the binomial draw.
common <- mean(full$increase)
sim <- replicate(N_SIM, {
  pre <- rbinom(nrow(full), full$n_pre, full$p_pre) / full$n_pre
  post <- rbinom(nrow(full), full$n_post, pmin(1, pmax(0, full$p_pre + common))) / full$n_post
  post - pre
})
sd_obs <- sd(full$increase)
sd_null <- apply(sim, 2, sd)
null_draws <- data.frame(increase = as.vector(sim))
share_noise <- mean(sd_null)^2 / sd_obs^2
# Same bins for both, so the mass at exactly 0 (counties at 100% on both sides,
# where the binomial cannot move) is drawn identically in the two distributions.
xr <- quantile(pp(full$increase), c(0.005, 0.995))
brks <- seq(floor(xr[1]) - 0.25, ceiling(xr[2]) + 0.25, by = 0.5)
p4 <- ggplot() +
  geom_histogram(data = filter(full, pp(increase) > min(brks), pp(increase) < max(brks)),
                 aes(pp(increase), after_stat(density)), breaks = brks, fill = "#2471a3", alpha = 0.55) +
  geom_freqpoly(data = filter(null_draws, pp(increase) > min(brks), pp(increase) < max(brks)),
                aes(pp(increase), after_stat(density)), breaks = brks, colour = "firebrick", linewidth = 0.9) +
  coord_cartesian(xlim = xr) +
  labs(x = "County increase in ever-attended (pp)", y = "Density",
       title = "4. Observed spread vs the spread luck alone would produce",
       subtitle = sprintf("Blue: observed counties (SD %.2f pp). Red: if every county had the same true increase\n(SD %.2f pp, 95%% of draws %.2f-%.2f). Luck accounts for about %.0f%% of the variance.",
                          pp(sd_obs), pp(mean(sd_null)), pp(quantile(sd_null, 0.025)), pp(quantile(sd_null, 0.975)),
                          100 * share_noise))

# --------------------------------------------------------------------------- #
# 5. Write
# --------------------------------------------------------------------------- #

fig <- ((p1 | p2) / (p3 | p4)) + plot_annotation(
  title = "Is the county ranking on the ever-attended increase real? Mostly not.",
  subtitle = paste0("Linked sample, 13 states with a first compulsory law in 1907-1920; increase = share ever attended in the first 5 exposed ",
                    "cohorts minus the last 5 unexposed.\nCounties with >= ", MIN_SIDE, " men on each side (in each half for panels 1-2). ",
                    "Halves are random splits of each county's men."),
  theme = theme(plot.title = element_text(size = 13, face = "bold"),
                plot.subtitle = element_text(size = 8.5, colour = "grey30")))
save_or_fail(file.path(OUT_DIR, "county_ranking_noise.png"), fig, width = 13, height = 11.5, dpi = 170, bg = "white")
for (i in 1:4) save_or_fail(file.path(OUT_DIR, sprintf("county_ranking_noise_panel%d.png", i)),
                            list(p1, p2, p3, p4)[[i]], width = 6.5, height = 6, dpi = 170, bg = "white")

stats <- data.frame(
  counties_both_halves = nrow(both), counties_full = nrow(full),
  split_half_r_increase = r_inc, split_half_r_pre_level = r_lvl, same_group_share = same_group,
  share_ci_covering_median = covers, sd_observed = sd_obs, sd_null_mean = mean(sd_null),
  sd_null_p025 = unname(quantile(sd_null, 0.025)), sd_null_p975 = unname(quantile(sd_null, 0.975)),
  noise_share_of_variance = share_noise)
write_csv(stats, file.path(OUT_DIR, "county_ranking_noise_stats.csv"))
print(as.data.frame(stats %>% mutate(across(where(is.double), ~ round(.x, 4)))), row.names = FALSE)
message("Wrote: ", OUT_DIR)
