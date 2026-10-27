# Regression discontinuity in birth cohort at the STATE level, within the treated
# states. The state-level companion to plot_cohort_discontinuity.R, which pools
# county x cohort cells; same design, running variable and versions.
#
# Two figures per measure and version:
#
#   discontinuity_state_<measure>             pooled state RD
#     A  state x cohort cells (every man in the state's analysis sample), one
#        point per state, men-weighted means per event time with 95% CIs, and
#        local linear fits either side of the cutoff (triangular kernel)
#     B  the same net of state fixed effects: each cell minus its state's
#        men-weighted mean over the window, plus the grand mean
#   discontinuity_state_by_state_<measure>    one RD per state, small multiples
#        state x cohort means with person-level 95% CIs and two-sided fits; the
#        strip gives that state's jump and kink, estimated by rdrobust on the
#        state's county x cohort cells (n >= 25) net of county FE, SEs clustered
#        by county -- a handful of state points could not give an SE
#
# Pooled estimates: rdrobust on the state cells (cutoff -0.5, local linear,
# triangular kernel, weighted by men, masspoints adjusted), heteroskedasticity-
# robust conventional SEs as the headline and a state-clustered check -- only
# 11-13 clusters, so read it as a check. Jump = deriv 0, kink = deriv 1.
#
# Versions as in plot_cohort_discontinuity.R: `full` (13 states, -5..+9, h 5) and
# `balanced4` (11 states with >= 4 unexposed and 4 exposed cohorts born
# 1890-1920, -4..+3, h 4; file suffix _balanced4).
#
# Two data sources, same design and code downstream of the cell tables:
#   linked     the MLP linked sample (students.parquet): white US-born men placed at
#              their earliest linked childhood record (age <= 18), non-migrant if
#              birth, childhood, 1935 and 1940 state all agree. Outputs
#              discontinuity_state_*.
#   fullcount  the complete 1940 census (IPUMS_1940_PARQUET_DIR, default
#              D:/ipums_1940_fullcount): every white US-born man, no linkage and so
#              no linkage selection, but no childhood record either -- non-migrant
#              means living in the birth state in 1940 and in the same state in 1935
#              (MIGRATE5 1 or 2; unknown 8/9 excluded). State = birth state; county
#              (for the per-state SEs) is the 1940 residence county. Outputs
#              discontinuity_state_fc_*, plus discontinuity_state_fc_funnel.csv.
#   When both run, discontinuity_state_source_comparison.csv sets the pooled
#   state-FE estimates side by side.
#
# Input:  output/mandatory_schooling/attendance_cohort/students.parquet
#         output/mandatory_schooling/attendance_cohort/law_events.csv
#         D:/ipums_1940_fullcount/person/statefip=*/*.parquet
# Output: results/mandatory_schooling/attendance_cohort/discontinuity_state_*
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/plot_cohort_discontinuity_state.R [--source=linked|fullcount|both]


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

MEASURES <- list(
  ever_attended = list(label = "Share ever attended school", pct = TRUE),
  meets_min     = list(label = "Share completing the law's minimum years", pct = TRUE),
  years_hs      = list(label = "Years of schooling (capped at 12)", pct = FALSE),
  hs_complete   = list(label = "Share completing high school", pct = TRUE)
)

VERSIONS <- list(
  full = list(k_min = -5L, k_max = 9L, min_pre = 1L, min_post = 1L,
              bandwidths = c(3, 5, 10), headline = 5, suffix = ""),
  balanced4 = list(k_min = -4L, k_max = 3L, min_pre = 4L, min_post = 4L,
                   bandwidths = c(3, 4), headline = 4, suffix = "_balanced4")
)

# --------------------------------------------------------------------------- #
# 1. State x cohort and county x cohort cells, treated states only
# --------------------------------------------------------------------------- #

# Which source(s) to run: --source=linked|fullcount|both (default both).
SOURCE_ARG <- sub("^--source=", "", grep("^--source=", commandArgs(TRUE), value = TRUE))
SOURCES <- if (!length(SOURCE_ARG) || SOURCE_ARG == "both") c("linked", "fullcount") else SOURCE_ARG
stopifnot(all(SOURCES %in% c("linked", "fullcount")))

FULLCOUNT_DIR <- Sys.getenv("IPUMS_1940_PARQUET_DIR", unset = "D:/ipums_1940_fullcount")
LAW_EVENTS <- file.path(data_root, "output/mandatory_schooling/attendance_cohort/law_events.csv")

SOURCE_INFO <- list(
  linked = list(prefix = "discontinuity_state",
                label = "IPUMS MLP linked sample",
                nonmig = "non-migrant: born, placed as a child (earliest linked record, age <= 18) and living in 1935 and 1940 in the same state"),
  fullcount = list(prefix = "discontinuity_state_fc",
                   label = "1940 full-count census",
                   nonmig = "non-migrant: living in the birth state in 1940 and in the same state in 1935 (MIGRATE5 same house or within state); no childhood record")
)

K_MIN_ALL <- min(sapply(VERSIONS, `[[`, "k_min"))
K_MAX_ALL <- max(sapply(VERSIONS, `[[`, "k_max"))
moments <- paste(sprintf("avg(%1$s) AS %1$s, var_samp(%1$s) AS var_%1$s", names(MEASURES)), collapse = ", ")
means_only <- paste(sprintf("avg(%1$s) AS %1$s", names(MEASURES)), collapse = ", ")

# Both sources return the same two tables with identical columns, so everything
# downstream is source-agnostic:
#   state_cells   state x yob: n, the four means and their variances
#   county_cells  county x yob with n >= MIN_CELL: n and the four means
# Each also returns a sample funnel.
build_cells <- function(source) {
  con <- dbConnect(duckdb::duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE))
  if (source == "linked") {
    base <- sprintf("(SELECT st0 AS statefip, school_statename AS statename, T, law_min, exitage,
                             yob, rel_cohort, county_geoid, %s
                      FROM read_parquet('%s')
                      WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL
                        AND in_law_state AND rel_cohort BETWEEN %d AND %d)",
                    paste(names(MEASURES), collapse = ", "), gsub("\\\\", "/", STUDENTS), K_MIN_ALL, K_MAX_ALL)
    funnel <- NULL
  } else {
    # The full count holds every 1940 person: no linkage, so no childhood record.
    # The state is the birth state, which non-migrants still live in (1935 and 1940).
    laws <- read_csv(LAW_EVENTS, show_col_types = FALSE) %>%
      transmute(statefip = as.integer(statefip), statename, T = as.integer(T), exitage = as.integer(exitage),
                law_min = as.integer(law_min), role)
    duckdb::duckdb_register(con, "laws", laws)
    person_glob <- sprintf("%s/person/*/*.parquet", gsub("\\\\", "/", FULLCOUNT_DIR))
    if (!length(Sys.glob(person_glob))) stop("No full-count Parquet found under ", FULLCOUNT_DIR,
                                             ". Set IPUMS_1940_PARQUET_DIR.")
    dbExecute(con, "PRAGMA threads=4")
    # yob = 1940 - AGE - 1 (enumeration 1 April); AGE 19-49 is yob 1890-1920.
    # HIGRADED 000/999 = N/A; years follow the CLS mapping used by every build.
    dbExecute(con, sprintf("
      CREATE TEMP TABLE men AS
      SELECT CAST(p.statefip AS INTEGER) AS res_state, p.BPL, p.MIGRATE5, p.HIGRADED,
             TRY_CAST(p.county_geoid AS INTEGER) AS county_geoid,
             1940 - CAST(p.AGE AS INTEGER) - 1 AS yob,
             CAST(FLOOR(p.HIGRADED / 10.0) AS INTEGER) AS g
      FROM read_parquet('%s', hive_partitioning = true) p
      WHERE p.SEX = 1 AND p.RACE = 1 AND p.BPL BETWEEN 1 AND 56 AND p.AGE BETWEEN 19 AND 49", person_glob))
    funnel <- dbGetQuery(con, "
      SELECT count(*) AS white_usborn_men_19_49,
             count(*) FILTER (WHERE HIGRADED NOT IN (0, 999)) AS with_attainment,
             count(*) FILTER (WHERE HIGRADED NOT IN (0, 999) AND BPL = res_state) AS in_birth_state_1940,
             count(*) FILTER (WHERE HIGRADED NOT IN (0, 999) AND BPL = res_state AND MIGRATE5 IN (8, 9)) AS migrate5_unknown,
             count(*) FILTER (WHERE HIGRADED NOT IN (0, 999) AND BPL = res_state AND MIGRATE5 IN (1, 2)) AS nonmig,
             count(*) FILTER (WHERE HIGRADED NOT IN (0, 999) AND BPL = res_state AND MIGRATE5 IN (1, 2)
                                AND BPL IN (SELECT statefip FROM laws WHERE role = 'law_state')) AS nonmig_in_law_state
      FROM men") %>%
      mutate(share_migrate5_unknown = migrate5_unknown / in_birth_state_1940)
    base <- sprintf("(SELECT m.BPL AS statefip, l.statename, l.T, l.law_min, l.exitage, m.yob,
                             m.yob - (l.T - l.exitage) AS rel_cohort, m.county_geoid,
                             (m.HIGRADED >= 40)::INTEGER AS ever_attended,
                             (y.years >= l.law_min)::INTEGER AS meets_min,
                             least(y.years, 12) AS years_hs,
                             (y.years >= 12)::INTEGER AS hs_complete
                      FROM men m
                      JOIN laws l ON l.statefip = m.BPL AND l.role = 'law_state'
                      CROSS JOIN LATERAL (SELECT CASE WHEN m.g <= 3 THEN 0 WHEN m.g <= 20 THEN m.g - 3
                                                      WHEN m.g <= 23 THEN 17 END AS years) y
                      WHERE m.HIGRADED NOT IN (0, 999) AND m.BPL = m.res_state AND m.MIGRATE5 IN (1, 2)
                        AND m.yob - (l.T - l.exitage) BETWEEN %d AND %d)", K_MIN_ALL, K_MAX_ALL)
  }
  state_cells <- dbGetQuery(con, sprintf("
    SELECT statefip, any_value(statename) AS statename, any_value(T) AS T, any_value(law_min) AS law_min,
           any_value(T - exitage) AS first_exposed_yob, yob, any_value(rel_cohort) AS rel_cohort,
           count(*) AS n, %s
    FROM %s b GROUP BY statefip, yob", moments, base))
  county_cells <- dbGetQuery(con, sprintf("
    SELECT county_geoid, statefip, any_value(statename) AS statename, yob,
           any_value(rel_cohort) AS rel_cohort, count(*) AS n, %s
    FROM %s b WHERE county_geoid IS NOT NULL
    GROUP BY county_geoid, statefip, yob HAVING count(*) >= %d", means_only, base, MIN_CELL))
  stopifnot(n_distinct(state_cells$statename) == 13L,
            all(state_cells$yob >= YOB_MIN & state_cells$yob <= YOB_MAX))
  list(state_cells = state_cells, county_cells = county_cells, funnel = funnel)
}

# --------------------------------------------------------------------------- #
# 2. Helpers
# --------------------------------------------------------------------------- #

run_rd <- function(d, v, deriv, h, cluster = NULL) {
  fit <- tryCatch(
    rdrobust::rdrobust(y = d[[v]], x = d$rel_cohort, c = RD_CUTOFF, p = 1, deriv = deriv,
                       kernel = "triangular", weights = d$n,
                       cluster = if (is.null(cluster)) NULL else d[[cluster]],
                       vce = "hc1", h = h, masspoints = "adjust"),
    error = function(e) e)
  n_cl <- if (is.null(cluster)) NA_integer_ else n_distinct(d[[cluster]])
  if (inherits(fit, "error")) {
    return(data.frame(estimate = NA_real_, se_conv = NA_real_, p_conv = NA_real_,
                      ci_lo = NA_real_, ci_hi = NA_real_, p_robust = NA_real_, h = h,
                      n_left = NA_real_, n_right = NA_real_, clusters = n_cl,
                      error = conditionMessage(fit)))
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

local_linear <- function(d, v, h, k_min, k_max) {
  side_fit <- function(side) {
    s <- if (side == "pre") filter(d, rel_cohort < RD_CUTOFF) else filter(d, rel_cohort > RD_CUTOFF)
    s <- mutate(s, k = pmax(0, 1 - abs(rel_cohort - RD_CUTOFF) / h)) %>% filter(k > 0)
    if (nrow(s) < 2) return(NULL)
    f <- lm(as.formula(paste(v, "~ rel_cohort")), data = s, weights = n * k)
    lo <- if (side == "pre") max(k_min, min(s$rel_cohort), RD_CUTOFF - h) else RD_CUTOFF
    hi <- if (side == "pre") RD_CUTOFF else min(k_max, RD_CUTOFF + h)
    data.frame(rel_cohort = seq(lo, hi, length.out = 30), side = side) %>% mutate(fit = predict(f, .))
  }
  bind_rows(side_fit("pre"), side_fit("post"))
}

net_of <- function(d, group, v) {
  grand <- weighted.mean(d[[v]], d$n)
  d %>% group_by(.data[[group]]) %>% mutate(.x = .data[[v]] - weighted.mean(.data[[v]], n) + grand) %>%
    ungroup() %>% pull(.x)
}

theme_set(theme_minimal(base_size = 10) +
            theme(plot.title = element_text(size = 10.5, face = "bold"),
                  plot.subtitle = element_text(size = 8, colour = "grey30"),
                  panel.grid.minor = element_blank()))

# Render to a local temp file, then copy into the Dropbox folder: writing in
# place fails intermittently while Dropbox holds the previous version.
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
# 3. One version
# --------------------------------------------------------------------------- #

run_version <- function(name, cfg, source, cells) {
  info <- SOURCE_INFO[[source]]
  events <- cells$state_cells %>% distinct(statename, T, law_min, first_exposed_yob) %>%
    mutate(n_unexposed = first_exposed_yob - YOB_MIN, n_exposed = YOB_MAX - first_exposed_yob + 1L)
  keep <- events %>% filter(n_unexposed >= cfg$min_pre, n_exposed >= cfg$min_post)
  in_window <- function(d) filter(d, statename %in% keep$statename,
                                  rel_cohort >= cfg$k_min, rel_cohort <= cfg$k_max)
  sc <- in_window(cells$state_cells)
  cc <- in_window(cells$county_cells)
  n_states <- n_distinct(sc$statename)
  if (name == "balanced4") {
    per_k <- count(sc, rel_cohort)
    stopifnot(nrow(per_k) == cfg$k_max - cfg$k_min + 1L, all(per_k$n == n_states))
  }
  for (m in names(MEASURES)) {
    sc[[paste0("fe_", m)]] <- net_of(sc, "statename", m)
    cc[[paste0("fe_", m)]] <- net_of(cc, "county_geoid", m)
  }
  chk <- sc %>% group_by(statename) %>% summarise(d = weighted.mean(fe_years_hs, n), .groups = "drop")
  stopifnot(max(abs(chk$d - weighted.mean(sc$years_hs, sc$n))) < 1e-9)

  # Pooled state RD
  grid <- tidyr::crossing(measure = names(MEASURES), outcome = c("raw", "state_fe"),
                          estimand = c("jump", "kink"), bandwidth = cfg$bandwidths,
                          cluster = c("none", "state")) %>%
    filter(cluster == "none" | bandwidth == cfg$headline)
  pooled <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    v <- if (g$outcome == "raw") g$measure else paste0("fe_", g$measure)
    cbind(source = source, version = name, as.data.frame(g),
          run_rd(sc, v, deriv = if (g$estimand == "jump") 0 else 1, h = g$bandwidth,
                 cluster = if (g$cluster == "state") "statefip" else NULL))
  }))
  write_csv(pooled, file.path(OUT_DIR, sprintf("%s_rd%s.csv", info$prefix, cfg$suffix)))

  # One RD per state, on its county cells net of county FE, SEs clustered by county
  by_state <- bind_rows(lapply(sort(unique(sc$statename)), function(s) {
    d <- filter(cc, statename == s)
    st <- filter(sc, statename == s)
    bind_rows(lapply(names(MEASURES), function(m) {
      raw_step <- st[[m]][st$rel_cohort == 0] - st[[m]][st$rel_cohort == -1]
      bind_rows(lapply(c("jump", "kink"), function(est) cbind(
        source = source, version = name, statename = s, measure = m, estimand = est,
        T = st$T[1], law_min = st$law_min[1], counties = n_distinct(d$county_geoid),
        raw_step_m1_to_0 = if (length(raw_step)) raw_step else NA_real_,
        run_rd(d, paste0("fe_", m), deriv = if (est == "jump") 0 else 1, h = cfg$headline,
               cluster = "county_geoid"))))
    }))
  }))
  write_csv(by_state, file.path(OUT_DIR, sprintf("%s_by_state_rd%s.csv", info$prefix, cfg$suffix)))

  head_row <- function(m, outcome, estimand) filter(pooled, measure == m, outcome == !!outcome,
    estimand == !!estimand, bandwidth == cfg$headline, cluster == "none")
  state_row <- function(m, outcome, estimand) filter(pooled, measure == m, outcome == !!outcome,
    estimand == !!estimand, bandwidth == cfg$headline, cluster == "state")
  cutoff_layers <- function(breaks = cfg$k_min:cfg$k_max) list(
    geom_vline(xintercept = RD_CUTOFF, linetype = "dashed", colour = "firebrick"),
    scale_x_continuous(breaks = breaks))
  sample_note <- sprintf("%d states with a first compulsory law in %d-%d%s", n_states, min(sc$T), max(sc$T),
                         if (name == "balanced4") " and at least 4 unexposed and 4 exposed cohorts born 1890-1920" else "")

  for (m in names(MEASURES)) {
    yfmt <- if (!MEASURES[[m]]$pct) scales::label_number(accuracy = 0.1) else
      scales::label_percent(accuracy = if (m == "ever_attended") 0.1 else 1)
    fmt_d <- function(x) if (is.na(x)) "NA" else if (MEASURES[[m]]$pct) sprintf("%+.2f pp", 100 * x) else sprintf("%+.3f", x)
    fmt_se <- function(x) substring(fmt_d(x), 2)
    fmt_est <- function(r, rs) sprintf("%s (SE %s; state-clustered SE %s)", fmt_d(r$estimate), fmt_se(r$se_conv),
                                       fmt_se(rs$se_conv))

    pooled_panel <- function(v, outcome, title, by_state_points) {
      means <- wmean_ci(sc, v)
      p <- ggplot(means, aes(rel_cohort, mean)) + cutoff_layers()
      if (by_state_points) {
        p <- p + geom_point(data = sc, aes(y = .data[[v]], colour = statename, size = n), alpha = 0.55,
                            position = position_jitter(width = 0.15, height = 0, seed = 1)) +
          scale_size_area(max_size = 3, guide = "none") +
          guides(colour = guide_legend(title = NULL, ncol = 1, override.aes = list(size = 2)))
      }
      p + geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, colour = "grey30") +
        geom_line(data = local_linear(sc, v, cfg$headline, cfg$k_min, cfg$k_max), aes(y = fit, group = side),
                  colour = "firebrick", linewidth = 0.9) +
        geom_point(size = 2) +
        scale_y_continuous(labels = yfmt) +
        labs(x = "Birth cohort relative to cutoff", y = NULL, title = title,
             subtitle = sprintf("Jump %s\nKink %s per cohort",
                                fmt_est(head_row(m, outcome, "jump"), state_row(m, outcome, "jump")),
                                fmt_est(head_row(m, outcome, "kink"), state_row(m, outcome, "kink"))))
    }
    fig <- (pooled_panel(m, "raw", "A. State x cohort cells", TRUE) |
              pooled_panel(paste0("fe_", m), "state_fe", "B. Net of state fixed effects", FALSE)) +
      plot_layout(guides = "collect") +
      plot_annotation(
        title = paste0(MEASURES[[m]]$label, ": state-level RD"),
        subtitle = paste0(info$label, ": ", sample_note, "; white US-born men born 1890-1920.\n",
                          info$nonmig, ".\n",
                          "Black: men-weighted mean across states with 95% CI. Local linear fits, triangular kernel, h = ",
                          cfg$headline, "; heteroskedasticity-robust SEs, state-clustered SEs (", n_states,
                          " clusters) as a check.\nCutoff between the last unexposed (-1) and first exposed (0) cohort; cohort 0 owed no schooling."),
        theme = theme(plot.title = element_text(size = 13, face = "bold"),
                      plot.subtitle = element_text(size = 8.5, colour = "grey30")))
    save_or_fail(file.path(OUT_DIR, sprintf("%s_%s%s.png", info$prefix, m, cfg$suffix)), fig,
                 width = 12, height = 5.6, dpi = 180, bg = "white")

    # Small multiples
    labs_tbl <- by_state %>% filter(measure == m) %>% select(statename, estimand, estimate, se_conv, T, law_min) %>%
      tidyr::pivot_wider(names_from = estimand, values_from = c(estimate, se_conv)) %>%
      mutate(strip = sprintf("%s (law %d, min %d yrs)\njump %s (%s), kink %s (%s)", statename, T, law_min,
                             sapply(estimate_jump, fmt_d), sapply(se_conv_jump, fmt_se),
                             sapply(estimate_kink, fmt_d), sapply(se_conv_kink, fmt_se)))
    sd_ <- sc %>% mutate(y = .data[[m]], se = sqrt(.data[[paste0("var_", m)]] / n),
                         lo = y - 1.96 * se, hi = y + 1.96 * se) %>%
      left_join(select(labs_tbl, statename, strip), by = "statename")
    fits <- bind_rows(lapply(unique(sd_$statename), function(s) {
      f <- local_linear(filter(sd_, statename == s), m, cfg$headline, cfg$k_min, cfg$k_max)
      if (is.null(f)) NULL else mutate(f, strip = sd_$strip[sd_$statename == s][1])
    }))
    p2 <- ggplot(sd_, aes(rel_cohort, y)) +
      cutoff_layers(breaks = seq(cfg$k_min, cfg$k_max, if (cfg$k_max - cfg$k_min > 8) 2 else 1)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.6) +
      geom_line(data = fits, aes(y = fit, group = side), colour = "firebrick", linewidth = 0.7) +
      geom_point(size = 1.3) +
      facet_wrap(~ strip, ncol = 4, scales = "free_y") +
      scale_y_continuous(labels = yfmt) +
      labs(x = "Birth cohort relative to cutoff", y = NULL,
           title = paste0(MEASURES[[m]]$label, ": RD by state (", info$label, ")"),
           subtitle = paste0("State x cohort means with 95% CIs (grey) and local linear fits (h = ", cfg$headline,
                             "). Jump and kink per state: rdrobust on that state's county x cohort cells (n >= ", MIN_CELL,
                             ") net of county FE,\nconventional SEs clustered by county in brackets. Free y-axes.")) +
      theme(strip.text = element_text(size = 7.5))
    save_or_fail(file.path(OUT_DIR, sprintf("%s_by_state_%s%s.png", info$prefix, m, cfg$suffix)), p2,
                 width = 13, height = if (n_states > 12) 11 else 8.8, dpi = 170, bg = "white")
  }

  cat(sprintf("\n=== %s | %s: %d state cells, %d states, event times %d..%d; %s men ===\n", source, name, nrow(sc), n_states,
              cfg$k_min, cfg$k_max, format(sum(sc$n), big.mark = ",")))
  print(as.data.frame(pooled %>% filter(bandwidth == cfg$headline) %>%
    transmute(measure, outcome, estimand, cluster, est = signif(estimate, 3), se = signif(se_conv, 3),
              t = round(estimate / se_conv, 1))), row.names = FALSE)
  cat("\nBy state (county cells net of county FE, clustered by county)\n")
  print(as.data.frame(by_state %>% transmute(statename, measure, estimand, est = signif(estimate, 3),
                                              t = round(estimate / se_conv, 1)) %>%
    tidyr::pivot_wider(names_from = c(measure, estimand), values_from = c(est, t))), row.names = FALSE)
  if (any(!is.na(c(pooled$error, by_state$error)))) {
    cat("\nrdrobust failures\n")
    print(distinct(bind_rows(select(filter(pooled, !is.na(error)), error),
                             select(filter(by_state, !is.na(error)), statename, measure, estimand, error))))
  }
  invisible(mutate(pooled, men = sum(sc$n), states = n_states))
}

options(width = 250)
pooled_all <- list()
for (src in SOURCES) {
  message("Building cells from the ", SOURCE_INFO[[src]]$label, " ...")
  cells <- build_cells(src)
  if (!is.null(cells$funnel)) {
    write_csv(cells$funnel, file.path(OUT_DIR, sprintf("%s_funnel.csv", SOURCE_INFO[[src]]$prefix)))
    cat("\nFull-count sample funnel\n"); print(as.data.frame(cells$funnel), row.names = FALSE)
  }
  for (v in names(VERSIONS)) pooled_all[[paste(src, v)]] <- run_version(v, VERSIONS[[v]], src, cells)
}

# Side by side: pooled state-FE jump and kink at the headline bandwidth, by source.
headline_h <- sapply(VERSIONS, `[[`, "headline")
comparison <- bind_rows(pooled_all) %>%
  filter(outcome == "state_fe", cluster == "none", bandwidth == headline_h[version]) %>%
  select(source, version, measure, estimand, estimate, se_conv, men, states) %>%
  arrange(version, measure, estimand, source)
if (length(SOURCES) == 2) {
  write_csv(comparison, file.path(OUT_DIR, "discontinuity_state_source_comparison.csv"))
  cat("\nLinked vs full count: pooled state-FE estimates at the headline bandwidth\n")
  print(as.data.frame(comparison %>%
    mutate(cell = sprintf("%.4f (%.4f)", estimate, se_conv)) %>%
    select(version, measure, estimand, source, cell) %>%
    tidyr::pivot_wider(names_from = source, values_from = cell)), row.names = FALSE)
}
message("Wrote: ", OUT_DIR)
