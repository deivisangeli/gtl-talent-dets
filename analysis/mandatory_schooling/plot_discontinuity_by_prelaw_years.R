# RD in birth cohort by where a county started: quartiles of its pre-law average
# years of schooling measured against the state's law minimum
# (pre_gap_hs = mean capped years in the 5 unexposed cohorts - law_min), linked
# sample, 13 states with a first compulsory law in 1907-1920.
#
# The pre-law average is built from the same unexposed cohorts that form the left
# side of the RD, so a county that is low by luck would "rise" by mean reversion.
# Hence cross-fitting, as in plot_discontinuity_by_attendance_gain.R: classify each
# county on one random half of its men, read its RD on the other half, swap, stack.
# A naive same-men version is kept only to show the mean reversion removed.
#
# Splits:
#   within_state  quartiles within each state (headline; law_min is a state
#                 constant, so this equals quartiles of raw pre-law years)
#   pooled        quartiles of the gap to the law minimum across all states
#   naive         within-state quartiles classified and measured on the same men
#
# Per quartile and measure: binned means of cells net of county FE (within fold),
# local linear fits (triangular, h = 5), rdrobust jump and kink with SEs clustered
# by county, and the plain after - before / fully exposed - before changes. The
# table adds the Q1 - Q4 gap and an inverse-variance linear trend across quartiles.
#
# Reading guard: lower-starting counties catching up can be the law binding where
# there was room, or ordinary convergence. The laws required at most 9 years, so a
# catch-up in meets_min / years but NOT in high-school completion points to the law;
# the same catch-up on everything points to convergence.
#
# Input:  output/mandatory_schooling/attendance_cohort/students.parquet
# Output: results/mandatory_schooling/attendance_cohort/discontinuity_by_prelaw_years*
#         results/mandatory_schooling/attendance_cohort/prelaw_years_gain_gradient.png
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/plot_discontinuity_by_prelaw_years.R

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


run_rd <- function(d, v, deriv, h, cluster = "county_geoid") {
  fit <- tryCatch(
    rdrobust::rdrobust(y = d[[v]], x = d$rel_cohort, c = RD_CUTOFF, p = 1, deriv = deriv,
                       kernel = "triangular", weights = d$n,
                       cluster = if (is.null(cluster)) NULL else d[[cluster]],
                       vce = "hc1", h = h, masspoints = "adjust"),
    error = function(e) e)
  n_cl <- if (is.null(cluster)) NA_integer_ else n_distinct(d[[cluster]])
  if (inherits(fit, "error")) {
    return(data.frame(estimate = NA_real_, se_conv = NA_real_, p_conv = NA_real_,
                      ci_lo = NA_real_, ci_hi = NA_real_, p_robust = NA_real_,
                      h = if (is.null(h)) NA_real_ else h, n_left = NA_real_, n_right = NA_real_,
                      clusters = n_cl, error = conditionMessage(fit)))
  }
  data.frame(estimate = unname(fit$coef[1, 1]),
             se_conv = unname(fit$se["Conventional", 1]), p_conv = unname(fit$pv["Conventional", 1]),
             ci_lo = unname(fit$ci["Robust", 1]), ci_hi = unname(fit$ci["Robust", 2]),
             p_robust = unname(fit$pv["Robust", 1]), h = unname(fit$bws["h", 1]),
             n_left = unname(fit$N_h[1]), n_right = unname(fit$N_h[2]),
             clusters = n_cl, error = NA_character_)
}

wmean_ci <- function(d, v) d %>% group_by(rel_cohort) %>%
  summarise(mean = weighted.mean(.data[[v]], n),
            se = sqrt(sum(n^2 * (.data[[v]] - weighted.mean(.data[[v]], n))^2)) / sum(n),
            .groups = "drop") %>%
  mutate(lo = mean - 1.96 * se, hi = mean + 1.96 * se)

# Local linear fits with a triangular kernel inside bandwidth h -- the fit
# rdrobust estimates, drawn on each side of the cutoff.
local_linear <- function(d, v, h, k_min, k_max) {
  side_fit <- function(side) {
    s <- if (side == "pre") filter(d, rel_cohort < RD_CUTOFF) else filter(d, rel_cohort > RD_CUTOFF)
    s <- mutate(s, k = pmax(0, 1 - abs(rel_cohort - RD_CUTOFF) / h)) %>% filter(k > 0)
    f <- lm(as.formula(paste(v, "~ rel_cohort")), data = s, weights = n * k)
    lo <- if (side == "pre") max(k_min, RD_CUTOFF - h) else RD_CUTOFF
    hi <- if (side == "pre") RD_CUTOFF else min(k_max, RD_CUTOFF + h)
    data.frame(rel_cohort = seq(lo, hi, length.out = 30), side = side) %>% mutate(fit = predict(f, .))
  }
  bind_rows(side_fit("pre"), side_fit("post"))
}

theme_set(theme_minimal(base_size = 10) +
            theme(plot.title = element_text(size = 10.5, face = "bold"),
                  plot.subtitle = element_text(size = 8, colour = "grey30"),
                  panel.grid.minor = element_blank()))

# Render to a local temp file, then copy into the Dropbox folder: writing in
# place fails intermittently ("agg could not write") while Dropbox holds the
# previous version of the file during sync.
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

# --------------------------------------------------------------------------- #

K_MIN <- -5L
K_MAX <- 9L
K_CLASS <- 5L
MIN_PRE_CLASS <- 25L     # pre-law men needed in the classifying half
MIN_CELL_FULL <- 25L
MIN_CELL_HALF <- 13L
H <- 5
RD_CUTOFF <- -0.5
N_Q <- 4L

MEASURES <- list(
  years_hs      = list(label = "Years of schooling (capped at 12)", pct = FALSE),
  meets_min     = list(label = "Share completing the law's minimum years", pct = TRUE),
  hs_complete   = list(label = "Share completing high school (placebo: never required)", pct = TRUE),
  ever_attended = list(label = "Share ever attended school", pct = TRUE)
)
Q_COLS <- c(Q1 = "#1b4f72", Q2 = "#2e86c1", Q3 = "#e59866", Q4 = "#a04000")
Q_LABELS <- c(Q1 = "Q1: least schooled", Q2 = "Q2", Q3 = "Q3", Q4 = "Q4: most schooled")

# --------------------------------------------------------------------------- #
# 1. Cells
# --------------------------------------------------------------------------- #

con <- dbConnect(duckdb::duckdb())
half_cells <- dbGetQuery(con, sprintf("
  SELECT county_geoid, st0 AS statefip, any_value(school_statename) AS statename, yob, half,
         any_value(rel_cohort) AS rel_cohort, any_value(law_min) AS law_min, count(*) AS n, %s
  FROM read_parquet('%s')
  WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL AND in_law_state
    AND rel_cohort BETWEEN %d AND %d
  GROUP BY county_geoid, st0, yob, half",
  paste(sprintf("avg(%1$s) AS %1$s", names(MEASURES)), collapse = ", "),
  gsub("\\\\", "/", STUDENTS), K_MIN, K_MAX))
dbDisconnect(con, shutdown = TRUE)
stopifnot(n_distinct(half_cells$statename) == 13L)

full_cells <- half_cells %>%
  group_by(county_geoid, statefip, statename, yob, rel_cohort, law_min) %>%
  summarise(across(all_of(names(MEASURES)), ~ weighted.mean(.x, n)), n = sum(n), .groups = "drop")

# --------------------------------------------------------------------------- #
# 2. Classification on pre-law years minus the law minimum
# --------------------------------------------------------------------------- #

pre_gap <- function(d) d %>% filter(rel_cohort >= -K_CLASS, rel_cohort <= -1) %>%
  group_by(county_geoid, statename, law_min) %>%
  summarise(pre_years_hs = weighted.mean(years_hs, n), n_pre = sum(n), .groups = "drop") %>%
  filter(n_pre >= MIN_PRE_CLASS) %>%
  mutate(pre_gap_hs = pre_years_hs - law_min)

quartile <- function(g, within_state) {
  g %>% group_by(across(if (within_state) "statename" else character())) %>%
    mutate(q = paste0("Q", ntile(pre_gap_hs, N_Q))) %>% ungroup() %>%
    select(county_geoid, pre_gap_hs, q)
}
gap_half <- lapply(0:1, function(h) pre_gap(filter(half_cells, half == h)))
gap_full <- pre_gap(full_cells)
labels <- list(
  within_state = lapply(gap_half, quartile, within_state = TRUE),
  pooled = lapply(gap_half, quartile, within_state = FALSE),
  naive = list(full = quartile(gap_full, TRUE))
)
# Within a state, law_min is constant: quartiles of the gap equal quartiles of raw years.
chk_raw <- gap_half[[1]] %>% group_by(statename) %>% mutate(q_raw = paste0("Q", ntile(pre_years_hs, N_Q))) %>%
  ungroup() %>% inner_join(labels$within_state[[1]], by = c("county_geoid", "pre_gap_hs"))
stopifnot(all(chk_raw$q == chk_raw$q_raw))

agree <- function(l) {
  j <- inner_join(l[[1]], l[[2]], by = "county_geoid", suffix = c("_h0", "_h1"))
  data.frame(counties_in_both = nrow(j), same_quartile = mean(j$q_h0 == j$q_h1),
             pre_gap_corr = cor(j$pre_gap_hs_h0, j$pre_gap_hs_h1))
}
agreement <- bind_rows(within_state = agree(labels$within_state), pooled = agree(labels$pooled), .id = "split")

# --------------------------------------------------------------------------- #
# 3. Outcome cells, net of county FE within fold
# --------------------------------------------------------------------------- #

net_county <- function(d) {
  for (m in names(MEASURES)) {
    grand <- weighted.mean(d[[m]], d$n)
    d <- d %>% group_by(county_geoid, fold) %>%
      mutate("fe_{m}" := .data[[m]] - weighted.mean(.data[[m]], n) + grand) %>% ungroup()
  }
  d
}
outcome_cells <- list(
  naive = full_cells %>% filter(n >= MIN_CELL_FULL) %>%
    inner_join(select(labels$naive$full, county_geoid, q), by = "county_geoid") %>%
    mutate(fold = 0L) %>% net_county())
for (s in c("within_state", "pooled")) {
  outcome_cells[[s]] <- bind_rows(lapply(0:1, function(f)
    half_cells %>% filter(half == f, n >= MIN_CELL_HALF) %>%
      # label from the OTHER half: the classifying men are never the outcome men
      inner_join(select(labels[[s]][[2 - f]], county_geoid, q), by = "county_geoid") %>%
      mutate(fold = f))) %>% net_county()
  for (f in 0:1) {
    got <- distinct(filter(outcome_cells[[s]], fold == f), county_geoid, q)
    stopifnot(!anyDuplicated(got$county_geoid),
              nrow(anti_join(got, labels[[s]][[2 - f]], by = c("county_geoid", "q"))) == 0)
  }
}

# --------------------------------------------------------------------------- #
# 4. Estimates per split x quartile x measure
# --------------------------------------------------------------------------- #

mean_change <- function(d, m, lo, hi) {
  # plain before/after on the county-FE outcome: cohorts lo..hi minus -5..-1
  v <- paste0("fe_", m)
  weighted.mean(d[[v]][d$rel_cohort >= lo & d$rel_cohort <= hi], d$n[d$rel_cohort >= lo & d$rel_cohort <= hi]) -
    weighted.mean(d[[v]][d$rel_cohort < 0], d$n[d$rel_cohort < 0])
}
est <- bind_rows(lapply(names(outcome_cells), function(s) {
  d <- outcome_cells[[s]]
  bind_rows(lapply(paste0("Q", 1:N_Q), function(g) {
    dg <- filter(d, q == g)
    bind_rows(lapply(names(MEASURES), function(m) bind_rows(
      lapply(c("jump", "kink"), function(e) cbind(
        split = s, q = g, measure = m, estimand = e, counties = n_distinct(dg$county_geoid), men = sum(dg$n),
        run_rd(dg, paste0("fe_", m), deriv = if (e == "jump") 0 else 1, h = H, cluster = "county_geoid"))),
      data.frame(split = s, q = g, measure = m, estimand = c("after_minus_before", "late_minus_before"),
                 counties = n_distinct(dg$county_geoid), men = sum(dg$n),
                 estimate = c(mean_change(dg, m, 0, 4), mean_change(dg, m, 5, 9)),
                 se_conv = NA_real_))))
  }))
}))
# Q1 - Q4 gap, and an inverse-variance linear trend across quartiles (per quartile step).
gaps <- est %>% filter(estimand %in% c("jump", "kink")) %>%
  group_by(split, measure, estimand) %>%
  summarise(q1_minus_q4 = estimate[q == "Q1"] - estimate[q == "Q4"],
            q1_minus_q4_se = sqrt(se_conv[q == "Q1"]^2 + se_conv[q == "Q4"]^2),
            trend_per_quartile = {
              w <- 1 / se_conv^2; x <- as.integer(sub("Q", "", q))
              sum(w * (x - weighted.mean(x, w)) * (estimate - weighted.mean(estimate, w))) /
                sum(w * (x - weighted.mean(x, w))^2)
            },
            trend_se = { w <- 1 / se_conv^2; x <- as.integer(sub("Q", "", q))
              sqrt(1 / sum(w * (x - weighted.mean(x, w))^2)) },
            .groups = "drop") %>%
  mutate(q1_minus_q4_t = q1_minus_q4 / q1_minus_q4_se, trend_t = trend_per_quartile / trend_se)
write_csv(est, file.path(OUT_DIR, "discontinuity_by_prelaw_years_rd.csv"))
write_csv(gaps, file.path(OUT_DIR, "discontinuity_by_prelaw_years_gaps.csv"))

# Quartile profiles (full-sample pre-law levels, labels from half 0 for the cross-fits)
profiles <- bind_rows(
  gap_full %>% inner_join(labels$naive$full, by = c("county_geoid", "pre_gap_hs")) %>% mutate(split = "naive"),
  gap_full %>% select(-pre_gap_hs) %>% inner_join(labels$within_state[[1]], by = "county_geoid") %>%
    mutate(split = "within_state (half 0 labels)"),
  gap_full %>% select(-pre_gap_hs) %>% inner_join(labels$pooled[[1]], by = "county_geoid") %>%
    mutate(split = "pooled (half 0 labels)")) %>%
  group_by(split, q) %>%
  summarise(counties = n(), pre_years_hs = weighted.mean(pre_years_hs, n_pre),
            gap_to_law_min = weighted.mean(pre_years_hs - law_min, n_pre),
            top_states = paste(head(names(sort(table(statename), decreasing = TRUE)), 3), collapse = ", "),
            .groups = "drop")
write_csv(profiles, file.path(OUT_DIR, "discontinuity_by_prelaw_years_groups.csv"))

# --------------------------------------------------------------------------- #
# 5. Figures
# --------------------------------------------------------------------------- #

split_titles <- c(
  within_state = "Cross-fitted, quartiles within state: classified on one half of each county's men, measured on the other",
  pooled = "Cross-fitted, pooled quartiles of pre-law years minus the law's minimum",
  naive = "NAIVE (same men classify and measure): contains mean reversion, shown for comparison only")
for (s in names(outcome_cells)) {
  d <- outcome_cells[[s]]
  panels <- lapply(names(MEASURES), function(m) {
    v <- paste0("fe_", m)
    yfmt <- if (!MEASURES[[m]]$pct) scales::label_number(accuracy = 0.1) else
      scales::label_percent(accuracy = if (m == "ever_attended") 0.1 else 1)
    fmt_d <- function(x) if (MEASURES[[m]]$pct) sprintf("%+.2f pp", 100 * x) else sprintf("%+.3f", x)
    means <- bind_rows(lapply(paste0("Q", 1:N_Q), function(g) mutate(wmean_ci(filter(d, q == g), v), q = g)))
    fits <- bind_rows(lapply(paste0("Q", 1:N_Q), function(g)
      mutate(local_linear(filter(d, q == g), v, H, K_MIN, K_MAX), q = g)))
    gk <- filter(gaps, split == s, measure == m, estimand == "kink")
    gj <- filter(gaps, split == s, measure == m, estimand == "jump")
    ggplot(means, aes(rel_cohort, mean, colour = q)) +
      geom_vline(xintercept = RD_CUTOFF, linetype = "dashed", colour = "grey40") +
      geom_point(size = 1.4, position = position_dodge(width = 0.45)) +
      geom_line(data = fits, aes(y = fit, group = interaction(q, side)), linewidth = 0.8) +
      scale_colour_manual(values = Q_COLS, labels = Q_LABELS, name = NULL) +
      scale_x_continuous(breaks = K_MIN:K_MAX) +
      scale_y_continuous(labels = yfmt) +
      labs(x = "Birth cohort relative to cutoff", y = NULL, title = MEASURES[[m]]$label,
           subtitle = sprintf("Q1 - Q4: kink %s (SE %s) per cohort; jump %s (SE %s)",
                              fmt_d(gk$q1_minus_q4), substring(fmt_d(gk$q1_minus_q4_se), 2),
                              fmt_d(gj$q1_minus_q4), substring(fmt_d(gj$q1_minus_q4_se), 2)))
  })
  fig <- wrap_plots(panels, ncol = 2, guides = "collect") +
    plot_annotation(
      title = split_titles[[s]],
      subtitle = paste0("Linked sample, 13 states with a first compulsory law in 1907-1920. Quartiles of the county's pre-law ",
                        "average years (capped at 12, cohorts -5..-1) minus the state's law minimum.\n",
                        "Cells net of county FE; local linear fits, triangular kernel, h = ", H,
                        "; rdrobust, conventional SEs clustered by county.",
                        if (s != "naive") sprintf(" The two halves agree on the quartile %.0f%% of the time (chance 25%%).",
                                                  100 * agreement$same_quartile[agreement$split == s]) else ""),
      theme = theme(plot.title = element_text(size = 12, face = "bold"),
                    plot.subtitle = element_text(size = 8.5, colour = "grey30"))) &
    theme(legend.position = "bottom")
  save_or_fail(file.path(OUT_DIR, sprintf("discontinuity_by_prelaw_years_%s.png", s)), fig,
               width = 12, height = 9, dpi = 170, bg = "white")
}

# Gradient: held-out change against pre-law gap, binned scatter.
grad <- bind_rows(lapply(0:1, function(f) {
  lab <- gap_half[[2 - f]] %>% select(county_geoid, statename, pre_gap_hs)
  half_cells %>% filter(half == f) %>%
    mutate(side = case_when(rel_cohort >= -K_CLASS & rel_cohort <= -1 ~ "pre",
                            rel_cohort >= 0 & rel_cohort <= K_CLASS - 1 ~ "post")) %>%
    filter(!is.na(side)) %>% group_by(county_geoid, side) %>%
    summarise(across(c(years_hs, meets_min, hs_complete), ~ weighted.mean(.x, n)), n = sum(n), .groups = "drop") %>%
    pivot_wider(names_from = side, values_from = c(years_hs, meets_min, hs_complete, n)) %>%
    filter(n_pre >= MIN_CELL_FULL, n_post >= MIN_CELL_FULL) %>%
    transmute(county_geoid, d_years_hs = years_hs_post - years_hs_pre, d_meets_min = meets_min_post - meets_min_pre,
              d_hs_complete = hs_complete_post - hs_complete_pre) %>%
    inner_join(lab, by = "county_geoid") %>% mutate(fold = f)
})) %>%
  group_by(statename) %>% mutate(pre_gap_within = pre_gap_hs - mean(pre_gap_hs)) %>% ungroup()
grad_long <- grad %>%
  pivot_longer(c(d_years_hs, d_meets_min, d_hs_complete), names_to = "outcome", values_to = "change") %>%
  mutate(outcome = recode(outcome, d_years_hs = "Years of schooling (capped)", d_meets_min = "Meets the minimum (share)",
                          d_hs_complete = "Completed high school (share, placebo)")) %>%
  pivot_longer(c(pre_gap_hs, pre_gap_within), names_to = "xvar", values_to = "x") %>%
  mutate(xvar = recode(xvar, pre_gap_hs = "Pooled: pre-law years minus law minimum",
                       pre_gap_within = "Within state: same, minus the state mean"))
bins <- grad_long %>% group_by(outcome, xvar) %>% mutate(bin = ntile(x, 20)) %>%
  group_by(outcome, xvar, bin) %>% summarise(x = mean(x), change = mean(change), .groups = "drop")
pg <- ggplot(bins, aes(x, change)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(size = 1.6) +
  geom_smooth(data = grad_long, method = "lm", formula = y ~ x, colour = "firebrick", fill = "grey75") +
  facet_grid(outcome ~ xvar, scales = "free") +
  labs(x = "Pre-law years of schooling minus the law's minimum (years; half A of the men)",
       y = "Change after - before (half B of the men)",
       title = "Do counties that started lower gain more after the law?",
       subtitle = "Binned scatter (20 bins) of each county's held-out change against its pre-law gap to the legal floor; OLS on all counties.") +
  theme_minimal(base_size = 10) + theme(plot.subtitle = element_text(size = 8.5, colour = "grey30"))
save_or_fail(file.path(OUT_DIR, "prelaw_years_gain_gradient.png"), pg, width = 10, height = 9, dpi = 170, bg = "white")

# --------------------------------------------------------------------------- #
# Console
# --------------------------------------------------------------------------- #

options(width = 220)
cat("\nAgreement between halves on the quartile\n"); print(as.data.frame(agreement), row.names = FALSE)
cat("\nQuartile profiles\n"); print(as.data.frame(profiles %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)
cat("\nEstimates by quartile (jump/kink: rdrobust, SE clustered by county; changes: after 0..4 and late 5..9 minus before)\n")
print(as.data.frame(est %>% transmute(split, q, measure, estimand,
  est = ifelse(measure == "years_hs", round(estimate, 3), round(100 * estimate, 2)),
  se = ifelse(measure == "years_hs", round(se_conv, 3), round(100 * se_conv, 2))) %>%
  pivot_wider(names_from = q, values_from = c(est, se))), row.names = FALSE)
cat("\nQ1 - Q4 gaps and linear trend across quartiles (shares in pp)\n")
print(as.data.frame(gaps %>% mutate(sc = ifelse(measure == "years_hs", 1, 100)) %>%
  transmute(split, measure, estimand, q1_minus_q4 = round(sc * q1_minus_q4, 3), se = round(sc * q1_minus_q4_se, 3),
            t = round(q1_minus_q4_t, 1), trend = round(sc * trend_per_quartile, 3), trend_t = round(trend_t, 1))),
  row.names = FALSE)
if (any(!is.na(est$error))) { cat("\nrdrobust failures\n"); print(distinct(filter(est, !is.na(error)), split, q, measure, estimand, error)) }
message("Wrote: ", OUT_DIR)
