# RD in birth cohort, split by each county's increase in ever-attended after the
# law: above- versus below-median counties, linked sample, 13 treated states.
#
# A county's own post-minus-pre change is mostly sampling noise (split-half
# reliability 0.20), so splitting on it and then reading the RD on the same men
# selects on the outcome: the "above" group jumps and the "below" group falls by
# construction, whatever the truth. Three splits are therefore produced:
#
#   naive                classify and measure on the same men -- shown only to
#                        expose how much of the gap is mechanical
#   crossfit             classify each county on one random half of its men
#                        (students.parquet `half`), read its RD on the other half,
#                        then swap and stack, so every man is an outcome once and
#                        never in his own county's classification
#   crossfit_within      as crossfit, but the median is taken within each state,
#                        so the split is not merely "which states jumped"
#
# The increase is mean ever_attended over rel_cohort 0..4 minus -5..-1 (the k = 5
# ramp contrast), counties with >= 25 men on each side in the classifying sample.
# Outcomes: the four measures, cells net of county FE (within fold), rdrobust jump
# and kink (cutoff -0.5, local linear, triangular kernel, h = 5, weighted by men,
# SEs clustered by county). Group differences use sqrt(se1^2 + se2^2); in the
# stacked cross-fit a county can sit in different groups in the two folds, so that
# SE is approximate.
#
# Input:  output/mandatory_schooling/attendance_cohort/students.parquet
#         output/mandatory_schooling/county_attendance_bite/bite_county.csv
# Output: results/mandatory_schooling/attendance_cohort/discontinuity_by_gain_*
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/plot_discontinuity_by_attendance_gain.R

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

BITE <- file.path(data_root, "output/mandatory_schooling/county_attendance_bite/bite_county.csv")

K_MIN <- -5L
K_MAX <- 9L
K_CLASS <- 5L            # classifier: rel_cohort 0..4 minus -5..-1
MIN_SIDE <- 25L          # men on each side of the cutoff needed to classify a county
MIN_CELL_FULL <- 25L     # outcome cells, naive (all men)
MIN_CELL_HALF <- 13L     # outcome cells, cross-fit (one half of the men)
H <- 5
RD_CUTOFF <- -0.5

MEASURES <- list(
  ever_attended = list(label = "Share ever attended school", pct = TRUE),
  meets_min     = list(label = "Share completing the law's minimum years", pct = TRUE),
  years_hs      = list(label = "Years of schooling (capped at 12)", pct = FALSE),
  hs_complete   = list(label = "Share completing high school", pct = TRUE)
)
GROUP_COLS <- c(above = "#c0392b", below = "#2471a3")

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
# 1. County x yob x half cells
# --------------------------------------------------------------------------- #

con <- dbConnect(duckdb::duckdb())
half_cells <- dbGetQuery(con, sprintf("
  SELECT county_geoid, st0 AS statefip, any_value(school_statename) AS statename, yob, half,
         any_value(rel_cohort) AS rel_cohort, count(*) AS n,
         %s
  FROM read_parquet('%s')
  WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL AND in_law_state
    AND rel_cohort BETWEEN %d AND %d
  GROUP BY county_geoid, st0, yob, half",
  paste(sprintf("avg(%1$s) AS %1$s", names(MEASURES)), collapse = ", "),
  gsub("\\\\", "/", STUDENTS), K_MIN, K_MAX))
dbDisconnect(con, shutdown = TRUE)
stopifnot(n_distinct(half_cells$statename) == 13L, all(half_cells$half %in% 0:1))

# Pool the halves back into full-sample cells (n-weighted means).
pool_halves <- function(d) d %>%
  group_by(county_geoid, statefip, statename, yob, rel_cohort) %>%
  summarise(across(all_of(names(MEASURES)), ~ weighted.mean(.x, n)), n = sum(n), .groups = "drop")
full_cells <- pool_halves(half_cells)

# --------------------------------------------------------------------------- #
# 2. Classification
# --------------------------------------------------------------------------- #

increase <- function(d) d %>%
  mutate(side = case_when(rel_cohort >= -K_CLASS & rel_cohort <= -1 ~ "pre",
                          rel_cohort >= 0 & rel_cohort <= K_CLASS - 1 ~ "post")) %>%
  filter(!is.na(side)) %>%
  group_by(county_geoid, statename, side) %>%
  summarise(ever = weighted.mean(ever_attended, n), n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = side, values_from = c(ever, n)) %>%
  filter(n_pre >= MIN_SIDE, n_post >= MIN_SIDE) %>%
  mutate(increase = ever_post - ever_pre)

classify <- function(inc, within_state) {
  inc %>% group_by(across(if (within_state) "statename" else character())) %>%
    mutate(group = if_else(increase > median(increase), "above", "below")) %>% ungroup() %>%
    select(county_geoid, increase, group)
}

inc_full <- increase(full_cells)
inc_half <- lapply(0:1, function(h) increase(filter(half_cells, half == h)))

labels <- list(
  naive = list(full = classify(inc_full, FALSE)),
  crossfit = lapply(inc_half, classify, within_state = FALSE),
  crossfit_within = lapply(inc_half, classify, within_state = TRUE)
)

# How often do the two halves put a county in the same group? 50% is a coin flip.
agree <- function(l) {
  j <- inner_join(l[[1]], l[[2]], by = "county_geoid", suffix = c("_h0", "_h1"))
  data.frame(counties_in_both = nrow(j), same_group = mean(j$group_h0 == j$group_h1),
             increase_corr = cor(j$increase_h0, j$increase_h1))
}
agreement <- bind_rows(crossfit = agree(labels$crossfit), crossfit_within = agree(labels$crossfit_within),
                       .id = "split")

# --------------------------------------------------------------------------- #
# 3. Outcome cells per split, net of county FE
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
    inner_join(select(labels$naive$full, county_geoid, group), by = "county_geoid") %>%
    mutate(fold = 0L) %>% net_county()
)
for (s in c("crossfit", "crossfit_within")) {
  outcome_cells[[s]] <- bind_rows(lapply(0:1, function(f) {
    half_cells %>% filter(half == f, n >= MIN_CELL_HALF) %>%
      # label from the OTHER half: no man is both classifier and outcome
      inner_join(select(labels[[s]][[2 - f]], county_geoid, group), by = "county_geoid") %>%
      mutate(fold = f)
  })) %>% net_county()
}
# Guard: in the cross-fit, every outcome cell's label came from the other half.
for (s in c("crossfit", "crossfit_within")) {
  oc <- outcome_cells[[s]]
  for (f in 0:1) {
    lab <- labels[[s]][[2 - f]]
    got <- distinct(filter(oc, fold == f), county_geoid, group)
    stopifnot(nrow(anti_join(got, lab, by = c("county_geoid", "group"))) == 0,
              !anyDuplicated(got$county_geoid))
  }
}

# --------------------------------------------------------------------------- #
# 4. RD per split x group x measure
# --------------------------------------------------------------------------- #

rd_rows <- bind_rows(lapply(names(outcome_cells), function(s) {
  d <- outcome_cells[[s]]
  bind_rows(lapply(c("above", "below"), function(g) bind_rows(lapply(names(MEASURES), function(m)
    bind_rows(lapply(c("jump", "kink"), function(e) cbind(
      split = s, group = g, measure = m, estimand = e,
      counties = n_distinct(d$county_geoid[d$group == g]), men = sum(d$n[d$group == g]),
      run_rd(filter(d, group == g), paste0("fe_", m), deriv = if (e == "jump") 0 else 1, h = H,
             cluster = "county_geoid"))))))))
}))
rd_diff <- rd_rows %>% select(split, group, measure, estimand, estimate, se_conv) %>%
  pivot_wider(names_from = group, values_from = c(estimate, se_conv)) %>%
  mutate(diff = estimate_above - estimate_below, diff_se = sqrt(se_conv_above^2 + se_conv_below^2),
         diff_t = diff / diff_se)
write_csv(left_join(rd_rows, select(rd_diff, split, measure, estimand, diff, diff_se, diff_t),
                    by = c("split", "measure", "estimand")),
          file.path(OUT_DIR, "discontinuity_by_gain_rd.csv"))

# --------------------------------------------------------------------------- #
# 5. Group profiles (full sample, pre-law window), by classification
# --------------------------------------------------------------------------- #

bite <- read_csv(BITE, show_col_types = FALSE) %>% filter(bite_usable) %>%
  select(county_geoid, bite_std_8_14)
pre_profile <- full_cells %>% filter(rel_cohort >= -K_CLASS, rel_cohort <= -1) %>%
  group_by(county_geoid, statename) %>%
  summarise(pre_ever = weighted.mean(ever_attended, n), pre_meets = weighted.mean(meets_min, n),
            pre_years = weighted.mean(years_hs, n), pre_n = sum(n), .groups = "drop") %>%
  left_join(bite, by = "county_geoid")
profile_of <- function(lab, split, basis) {
  pre_profile %>% inner_join(lab, by = "county_geoid") %>% group_by(group) %>%
    summarise(split = split, classified_on = basis, counties = n(), states = n_distinct(statename),
              median_increase_pp = 100 * median(increase),
              pre_ever_attended = weighted.mean(pre_ever, pre_n),
              pre_shortfall_below_min = 1 - weighted.mean(pre_meets, pre_n),
              pre_years_hs = weighted.mean(pre_years, pre_n),
              attendance_bite = mean(bite_std_8_14, na.rm = TRUE),
              top_states = paste(head(names(sort(table(statename), decreasing = TRUE)), 4), collapse = ", "),
              .groups = "drop")
}
profiles <- bind_rows(
  profile_of(labels$naive$full, "naive", "all men"),
  profile_of(labels$crossfit[[1]], "crossfit", "half 0"),
  profile_of(labels$crossfit[[2]], "crossfit", "half 1"),
  profile_of(labels$crossfit_within[[1]], "crossfit_within", "half 0"),
  profile_of(labels$crossfit_within[[2]], "crossfit_within", "half 1"))
write_csv(profiles, file.path(OUT_DIR, "discontinuity_by_gain_groups.csv"))
write_csv(agreement, file.path(OUT_DIR, "discontinuity_by_gain_agreement.csv"))

# --------------------------------------------------------------------------- #
# 6. Figures
# --------------------------------------------------------------------------- #

split_titles <- c(
  naive = "NAIVE split: counties classified and measured on the same men (mechanical -- selection on the outcome)",
  crossfit = "Cross-fitted split: classified on one half of each county's men, measured on the other half",
  crossfit_within = "Cross-fitted split, median within state: classified on one half, measured on the other")

for (s in names(outcome_cells)) {
  d <- outcome_cells[[s]]
  panels <- lapply(names(MEASURES), function(m) {
    v <- paste0("fe_", m)
    yfmt <- if (!MEASURES[[m]]$pct) scales::label_number(accuracy = 0.1) else
      scales::label_percent(accuracy = if (m == "ever_attended") 0.1 else 1)
    fmt_d <- function(x) if (MEASURES[[m]]$pct) sprintf("%+.2f pp", 100 * x) else sprintf("%+.3f", x)
    means <- bind_rows(lapply(c("above", "below"), function(g) mutate(wmean_ci(filter(d, group == g), v), group = g)))
    fits <- bind_rows(lapply(c("above", "below"), function(g)
      mutate(local_linear(filter(d, group == g), v, H, K_MIN, K_MAX), group = g)))
    r <- filter(rd_rows, split == s, measure == m, estimand == "jump")
    dd <- filter(rd_diff, split == s, measure == m, estimand == "jump")
    kk <- filter(rd_diff, split == s, measure == m, estimand == "kink")
    ggplot(means, aes(rel_cohort, mean, colour = group)) +
      geom_vline(xintercept = RD_CUTOFF, linetype = "dashed", colour = "grey40") +
      geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, alpha = 0.6,
                    position = position_dodge(width = 0.35)) +
      geom_point(size = 1.6, position = position_dodge(width = 0.35)) +
      geom_line(data = fits, aes(y = fit, group = interaction(group, side)), linewidth = 0.8) +
      scale_colour_manual(values = GROUP_COLS, labels = c(above = "Above-median increase", below = "Below-median increase"),
                          name = NULL) +
      scale_x_continuous(breaks = K_MIN:K_MAX) +
      scale_y_continuous(labels = yfmt) +
      labs(x = "Birth cohort relative to cutoff", y = NULL, title = MEASURES[[m]]$label,
           subtitle = sprintf("Jump: above %s, below %s; gap %s (SE %s)\nKink gap %s (SE %s) per cohort",
                              fmt_d(r$estimate[r$group == "above"]), fmt_d(r$estimate[r$group == "below"]),
                              fmt_d(dd$diff), substring(fmt_d(dd$diff_se), 2),
                              fmt_d(kk$diff), substring(fmt_d(kk$diff_se), 2)))
  })
  n_c <- n_distinct(d$county_geoid)
  fig <- wrap_plots(panels, ncol = 2, guides = "collect") +
    plot_annotation(
      title = split_titles[[s]],
      subtitle = paste0("13 states with a first compulsory law in 1907-1920, linked sample, ", n_c,
                        " counties. Group = county's ever-attended change, cohorts 0..4 minus -5..-1, split at the ",
                        if (s == "crossfit_within") "state" else "pooled", " median.\n",
                        "Cells net of county FE; local linear fits, triangular kernel, h = ", H,
                        "; rdrobust, conventional SEs clustered by county.",
                        if (s != "naive") sprintf(" The two halves put a county in the same group %.0f%% of the time.",
                                                  100 * agreement$same_group[agreement$split == s]) else ""),
      theme = theme(plot.title = element_text(size = 12.5, face = "bold"),
                    plot.subtitle = element_text(size = 8.5, colour = "grey30"))) &
    theme(legend.position = "bottom")
  save_or_fail(file.path(OUT_DIR, sprintf("discontinuity_by_gain_%s.png", s)), fig,
               width = 12, height = 9, dpi = 170, bg = "white")
}

# --------------------------------------------------------------------------- #
# Console
# --------------------------------------------------------------------------- #

options(width = 220)
cat("\nAgreement between the two halves' classifications\n")
print(as.data.frame(agreement %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)
cat("\nRD by group (jump and kink, conventional SE clustered by county) and above - below gap\n")
print(as.data.frame(rd_diff %>% transmute(split, measure, estimand,
  above = sprintf("%.4f (%.4f)", estimate_above, se_conv_above),
  below = sprintf("%.4f (%.4f)", estimate_below, se_conv_below),
  gap = sprintf("%.4f (%.4f)", diff, diff_se), gap_t = round(diff_t, 1))), row.names = FALSE)
cat("\nGroup profiles (pre-law cohorts -5..-1, all men)\n")
print(as.data.frame(profiles %>% mutate(across(where(is.double), ~ signif(.x, 3)))), row.names = FALSE)
if (any(!is.na(rd_rows$error))) { cat("\nrdrobust failures\n"); print(distinct(filter(rd_rows, !is.na(error)), split, group, measure, estimand, error)) }
message("Wrote: ", OUT_DIR)
