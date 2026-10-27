# Regression discontinuity in birth cohort, within the treated states.
#
# For the 13 states whose first compulsory law took effect in 1907-1920, compare
# the cohorts just too old to be covered with the cohorts just young enough. The
# running variable is rel_cohort = yob - (T - exitage): -1 is the last unexposed
# cohort, 0 the first exposed. No other states enter -- this is the discontinuity
# itself, not a difference-in-differences.
#
# Two panels per measure:
#
#   A  county x cohort cells as they are: men-weighted binned means with 95% CIs
#      and local linear fits either side of the cutoff (triangular kernel, h = 5)
#   B  the same after removing county fixed effects: each cell minus its county's
#      men-weighted mean over ALL its cohorts in the window, plus the grand mean
#
# Estimates (rdrobust; cutoff -0.5, local linear, triangular kernel, cells weighted
# by men, SEs clustered by county, masspoints adjusted because birth year is
# discrete): the JUMP (deriv 0) and the KINK, the change in slope (deriv 1).
# Fixed bandwidths h = 3, 5 (headline) and 10: with 15 birth years a data-driven
# bandwidth is not reliable, and with clustering it fails. The headline uses
# conventional clustered SEs: with so few mass points the robust bias correction
# (a local quadratic on 3-10 points) is unstable, and its interval sometimes
# excludes its own estimate; it is kept in discontinuity_rd.csv. Robustness rows drop
# the age-heaped birth years 1898-1899, restrict to the 11 states with five
# unexposed cohorts (all but Delaware and Oklahoma), and cluster by state.
#
# Two versions are written:
#   full        all 13 events over -5..+9 (h = 3, 5, 10); Delaware and Oklahoma
#               enter only from -3 and -2, so the states behind each point change
#   balanced4   only events with at least 4 unexposed and 4 exposed cohorts born
#               1890-1920 (11 states), shown over exactly -4..+3 so every event
#               time has the same states and each side the same number of
#               cohorts (h = 3, 4); files carry the suffix _balanced4. No cohort in
#               -4..+3 is fully exposed -- full exposure starts at +4 -- so its
#               right side is the partial-exposure ramp only.
#
# Reading note: cohort 0 was exactly at the leaving age when the law passed, so
# the law required nothing of it. An effect should show as the kink -- a change in
# slope as coverage builds, reaching full exposure at +4..+9 by state.
#
# Input:  output/mandatory_schooling/attendance_cohort/students.parquet
# Output: results/mandatory_schooling/attendance_cohort/discontinuity_*
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/plot_cohort_discontinuity.R

suppressPackageStartupMessages({
  library("dplyr")
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


MIN_CELL <- 25L
RD_CUTOFF <- -0.5
YOB_MIN <- 1890L
YOB_MAX <- 1920L
HEAPED_YOB <- 1898:1899   # the age-40/41 heap in the 1940 census

MEASURES <- list(
  ever_attended = list(label = "Share ever attended school", pct = TRUE),
  meets_min     = list(label = "Share completing the law's minimum years", pct = TRUE),
  years_hs      = list(label = "Years of schooling (capped at 12)", pct = FALSE),
  hs_complete   = list(label = "Share completing high school", pct = TRUE)
)

# Two versions. `full` keeps every 1907-1920 event over -5..+9, so Delaware and
# Oklahoma enter only from -3 and -2. `balanced4` keeps the events with at least
# four unexposed and four exposed cohorts inside 1890-1920 and shows exactly
# those, -4..+3, so every event time has the same states and each side the same
# number of cohorts.
VERSIONS <- list(
  full = list(k_min = -5L, k_max = 9L, min_pre = 1L, min_post = 1L,
              bandwidths = c(3, 5, 10), headline = 5, balanced_check = TRUE,
              data_driven = TRUE, band = TRUE, suffix = ""),
  balanced4 = list(k_min = -4L, k_max = 3L, min_pre = 4L, min_post = 4L,
                   bandwidths = c(3, 4), headline = 4, balanced_check = FALSE,
                   data_driven = FALSE, band = FALSE, suffix = "_balanced4")
)

# --------------------------------------------------------------------------- #
# 1. County x cohort cells, treated states only (widest window of any version)
# --------------------------------------------------------------------------- #

con <- dbConnect(duckdb::duckdb())
all_cells <- dbGetQuery(con, sprintf("
  SELECT county_geoid, st0 AS statefip, any_value(school_statename) AS statename, yob,
         any_value(rel_cohort) AS rel_cohort, any_value(law_min) AS law_min,
         any_value(T - exitage) AS first_exposed_yob, count(*) AS n,
         avg(ever_attended) AS ever_attended, avg(meets_min) AS meets_min,
         avg(years_hs) AS years_hs, avg(hs_complete) AS hs_complete
  FROM read_parquet('%s')
  WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL AND in_law_state
    AND rel_cohort BETWEEN %d AND %d
  GROUP BY county_geoid, st0, yob
  HAVING count(*) >= %d",
  gsub("\\\\", "/", STUDENTS), min(sapply(VERSIONS, `[[`, "k_min")),
  max(sapply(VERSIONS, `[[`, "k_max")), MIN_CELL))
dbDisconnect(con, shutdown = TRUE)
stopifnot(n_distinct(all_cells$statename) == 13L)

# Events by how many unexposed and exposed cohorts fall inside 1890-1920.
events <- all_cells %>% distinct(statename, first_exposed_yob, law_min) %>%
  mutate(n_unexposed = first_exposed_yob - YOB_MIN, n_exposed = YOB_MAX - first_exposed_yob + 1L)

# --------------------------------------------------------------------------- #
# 2. Helpers
# --------------------------------------------------------------------------- #

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
# 3. One version: sample, county FE, rdrobust, figures, tables
# --------------------------------------------------------------------------- #

run_version <- function(name, cfg) {
  keep <- events %>% filter(n_unexposed >= cfg$min_pre, n_exposed >= cfg$min_post)
  cells <- all_cells %>%
    filter(statename %in% keep$statename, rel_cohort >= cfg$k_min, rel_cohort <= cfg$k_max)
  n_states <- n_distinct(cells$statename)
  if (!cfg$band) {
    # Balanced: every event time carries every kept state.
    per_k <- cells %>% group_by(rel_cohort) %>% summarise(s = n_distinct(statename), .groups = "drop")
    stopifnot(nrow(per_k) == cfg$k_max - cfg$k_min + 1L, all(per_k$s == n_states))
  }
  full_range <- range(cells$law_min)   # first fully exposed cohort sits at +law_min

  # Net of county FE over all the county's cohorts in this window, plus the grand
  # mean so the level stays readable.
  for (m in names(MEASURES)) {
    grand <- weighted.mean(cells[[m]], cells$n)
    cells <- cells %>% group_by(county_geoid) %>%
      mutate("fe_{m}" := .data[[m]] - weighted.mean(.data[[m]], n) + grand) %>% ungroup()
  }
  chk <- cells %>% group_by(county_geoid) %>% summarise(d = weighted.mean(fe_years_hs, n), .groups = "drop")
  stopifnot(max(abs(chk$d - weighted.mean(cells$years_hs, cells$n))) < 1e-9)

  base <- "all"
  specs <- bind_rows(
    expand.grid(bandwidth = cfg$bandwidths, sample = base, cluster = "county", stringsAsFactors = FALSE),
    data.frame(bandwidth = cfg$headline, sample = "no_heaping", cluster = "county"),
    if (cfg$balanced_check) data.frame(bandwidth = cfg$headline, sample = "balanced_11", cluster = "county"),
    data.frame(bandwidth = cfg$headline, sample = base, cluster = "state"),
    # Data-driven bandwidth, without clustering, where it is computable.
    if (cfg$data_driven) data.frame(bandwidth = NA_real_, sample = base, cluster = "none")
  )
  grid <- tidyr::crossing(measure = names(MEASURES), outcome = c("raw", "county_fe"),
                          estimand = c("jump", "kink"), specs)
  short_pre <- events$statename[events$n_unexposed < 5L]
  rd_all <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    d <- switch(g$sample,
                all = cells,
                no_heaping = filter(cells, !yob %in% HEAPED_YOB),
                balanced_11 = filter(cells, !statename %in% short_pre))
    v <- if (g$outcome == "raw") g$measure else paste0("fe_", g$measure)
    h <- if (is.na(g$bandwidth)) NULL else g$bandwidth
    cl <- switch(g$cluster, county = "county_geoid", state = "statefip", none = NULL)
    cbind(version = name, as.data.frame(g),
          run_rd(d, v, deriv = if (g$estimand == "jump") 0 else 1, h = h, cluster = cl))
  }))
  write_csv(rd_all, file.path(OUT_DIR, sprintf("discontinuity_rd%s.csv", cfg$suffix)))

  headline <- function(m, outcome, estimand) {
    filter(rd_all, measure == m, outcome == !!outcome, estimand == !!estimand,
           bandwidth == cfg$headline, sample == base, cluster == "county")
  }
  cutoff_layers <- function() c(
    if (cfg$band) list(annotate("rect", xmin = full_range[1] - 0.5, xmax = full_range[2] + 0.5,
                                ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.07)),
    list(geom_vline(xintercept = RD_CUTOFF, linetype = "dashed", colour = "firebrick"),
         scale_x_continuous(breaks = cfg$k_min:cfg$k_max)))

  sample_note <- if (cfg$band) {
    sprintf("%d states with a first compulsory law in 1907-1920", n_states)
  } else {
    sprintf("%d states with a first compulsory law in 1913-1920 and at least %d unexposed and %d exposed cohorts born 1890-1920 (drops %s)",
            n_states, cfg$min_pre, cfg$min_post,
            paste(setdiff(events$statename, keep$statename), collapse = ", "))
  }
  exposure_note <- if (cfg$band) {
    sprintf("\nShaded: range across states of the first fully exposed cohort (+%d to +%d).", full_range[1], full_range[2])
  } else {
    sprintf("\nNo cohort in this window is fully exposed: full exposure starts at +%d to +%d by state, so the right side is the partial-exposure ramp.",
            full_range[1], full_range[2])
  }

  summary_rows <- list()
  for (m in names(MEASURES)) {
    yfmt <- if (!MEASURES[[m]]$pct) scales::label_number(accuracy = 0.1) else
      scales::label_percent(accuracy = if (m == "ever_attended") 0.1 else 1)
    fmt_d <- function(x) if (MEASURES[[m]]$pct) sprintf("%+.2f pp", 100 * x) else sprintf("%+.3f", x)
    fmt_est <- function(r) sprintf("%s (SE %s, p %.2f)", fmt_d(r$estimate), substring(fmt_d(r$se_conv), 2), r$p_conv)

    rd_panel <- function(v, outcome, title, scatter = FALSE) {
      jump <- headline(m, outcome, "jump")
      kink <- headline(m, outcome, "kink")
      means <- wmean_ci(cells, v)
      p <- ggplot(means, aes(rel_cohort, mean)) + cutoff_layers()
      if (scatter) {
        p <- p + geom_jitter(data = cells, aes(y = .data[[v]], size = n), width = 0.2, height = 0,
                             alpha = 0.05, stroke = 0, colour = "grey20") +
          scale_size_area(max_size = 2, guide = "none")
      }
      p + geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, colour = "grey40") +
        geom_line(data = local_linear(cells, v, cfg$headline, cfg$k_min, cfg$k_max),
                  aes(y = fit, group = side), colour = "firebrick", linewidth = 0.8) +
        geom_point(size = 1.8) +
        scale_y_continuous(labels = yfmt) +
        coord_cartesian(ylim = range(c(means$lo, means$hi)) + c(-1, 1) * 0.15 * diff(range(c(means$lo, means$hi)))) +
        labs(x = "Birth cohort relative to cutoff", y = NULL, title = title,
             subtitle = sprintf("Jump %s\nKink %s per cohort", fmt_est(jump), fmt_est(kink)))
    }

    fig <- (rd_panel(m, "raw", "A. County x cohort cells", scatter = TRUE) |
              rd_panel(paste0("fe_", m), "county_fe", "B. Net of county fixed effects")) +
      plot_annotation(
        title = MEASURES[[m]]$label,
        subtitle = paste0(sample_note, "; cells with >= ", MIN_CELL,
                          " white US-born non-migrant men.\nLocal linear fits, triangular kernel, h = ", cfg$headline,
                          "; conventional SEs clustered by county (robust bias-corrected intervals in discontinuity_rd",
                          cfg$suffix, ".csv).\nCutoff between the last unexposed (-1) and first exposed (0) cohort; cohort 0 owed no schooling.",
                          exposure_note),
        theme = theme(plot.title = element_text(size = 13, face = "bold"),
                      plot.subtitle = element_text(size = 8.5, colour = "grey30")))
    save_or_fail(file.path(OUT_DIR, sprintf("discontinuity_%s%s.png", m, cfg$suffix)), fig,
                 width = 11, height = 5.4, dpi = 180, bg = "white")

    summary_rows[[m]] <- data.frame(
      version = name, measure = m, states = n_states, cells = nrow(cells),
      counties = n_distinct(cells$county_geoid), window = sprintf("%d..%d", cfg$k_min, cfg$k_max),
      h = cfg$headline,
      jump_raw = headline(m, "raw", "jump")$estimate, jump_raw_se = headline(m, "raw", "jump")$se_conv,
      kink_raw = headline(m, "raw", "kink")$estimate, kink_raw_se = headline(m, "raw", "kink")$se_conv,
      jump_fe = headline(m, "county_fe", "jump")$estimate, jump_fe_se = headline(m, "county_fe", "jump")$se_conv,
      kink_fe = headline(m, "county_fe", "kink")$estimate, kink_fe_se = headline(m, "county_fe", "kink")$se_conv,
      jump_fe_p_robust = headline(m, "county_fe", "jump")$p_robust,
      kink_fe_p_robust = headline(m, "county_fe", "kink")$p_robust)
  }
  summary_all <- bind_rows(summary_rows)
  write_csv(summary_all, file.path(OUT_DIR, sprintf("discontinuity_summary%s.csv", cfg$suffix)))

  cat(sprintf("\n=== %s: %d cells in %d counties, %d states (%s), event times %d..%d ===\n",
              name, nrow(cells), n_distinct(cells$county_geoid), n_states,
              paste(sort(unique(cells$statename)), collapse = ", "), cfg$k_min, cfg$k_max))
  print(as.data.frame(summary_all %>% select(-version) %>% mutate(across(where(is.double), ~ signif(.x, 3)))),
        row.names = FALSE)
  if (any(!is.na(rd_all$error))) {
    cat("rdrobust failures:\n")
    print(distinct(filter(rd_all, !is.na(error)), sample, cluster, bandwidth, error))
  }
  invisible(rd_all)
}

options(width = 220)
rd_versions <- lapply(names(VERSIONS), function(v) run_version(v, VERSIONS[[v]]))

# The event study against the pre-1907 states is gone; its file would read as current.
stale <- file.path(OUT_DIR, "discontinuity_event_study.csv")
if (file.exists(stale)) file.remove(stale)
message("Wrote: ", OUT_DIR)
