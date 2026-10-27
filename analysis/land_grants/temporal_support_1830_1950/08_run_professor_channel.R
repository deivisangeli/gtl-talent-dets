###############################################################################
# Could professors' children explain the college-allocation AMWS effect?
#
# A college imports professors, and professors' children are unusually likely
# to become scientists. This script puts that channel on the same footing as
# the AMWS result in project/main.tex: same events, same balance filter, same
# CS-DID estimator and covariate as 03_run_baseline_population_controls.R.
#
#   1. Reproduce the main.tex AMWS estimates on the original sample.
#   2. Event-study professors' children born in the county (prof_births, from
#      prep/occupational_fertility/build_professor_children_event_panel.R).
#   3. Break-even transmission rate p*(e) = ATT_amws(e) / ATT_prof(e): the share
#      of professors' children who would have to become AMWS scientists for the
#      channel to explain the whole effect. On a common sample the
#      regression-adjusted DiD is linear in the outcome, so
#      ATT(n_amws - p * prof_births) = ATT_amws - p * ATT_prof exactly, influence
#      functions included. That gives adjusted ATTs for any p and a Fieller
#      (test-inversion) interval for p* without re-estimating.
#   4. Assumption-free checks: per-event dose-response, dropping the largest
#      professor importers, and college type.
#
# Inputs: the temporal-support panels (via _sample_helpers.R) and
#   OCC_CHANNEL_OUT_DIR (default <DET_DIR>/output/occupational_fertility/college_channel).
# Output: <DET_DIR>/results/land_grants/professor_channel/
#
# Run from <repo>:
#   Rscript analysis/land_grants/temporal_support_1830_1950/08_run_professor_channel.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(did)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "prep", "raw_paths.R"))
source(file.path(dirname(script_path), "_sample_helpers.R"))

CHANNEL_DIR <- Sys.getenv("OCC_CHANNEL_OUT_DIR",
                          unset = output_file_path("occupational_fertility", "college_channel"))
out_path <- function(...) {
  path <- results_file_path("land_grants", "professor_channel", ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}
say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

REFERENCE_E <- 0L
Z <- 1.96  # as in 03, so intervals match main.tex exactly
P_GRID <- c(0, 0.005, 0.01, 0.02, 0.05, 0.10, 0.20)
# The two samples reported in main.tex: the main-text fully balanced window and
# the Appendix's less-restrictive one.
SPECS <- list(
  balanced_max70   = list(balance = TRUE,  min_e = -20L, max_e = 70L),
  unbalanced_max90 = list(balance = FALSE, min_e = -20L, max_e = 90L)
)
# main.tex, College Allocation, e = +50, fully balanced.
PAPER <- tibble(
  timing = c("standard_decade", "standard_decade", "alternative_decade", "alternative_decade"),
  outcome = c("n_amws", "amws_per_1000_births", "n_amws", "amws_per_1000_births"),
  paper_e50 = c(8.278, 0.659, 5.427, 0.533)
)
# Channel measures. "faculty" (the headline) rescales every census to estimated
# total male college faculty with build_professor_coverage.R's factors, so the
# censuses' different coverage of faculty does not move the event-time profile;
# "raw" (the unscaled version) is the same OCC1950 010-029 count without the
# factor, so the two channels differ only by the rescaling. "ams_faculty" and
# "ams_raw" replace the co-resident children by a prediction: the census faculty
# stock by age times the American Men of Science 1956 birth schedule
# (prep/occupational_fertility/build_professor_ams_channel.R), scaled and
# unscaled. "chsurv_faculty" and "chsurv_raw" are the same prediction with the
# fertility measured in the census from professors' wives (CHSURV 1900-1910,
# CHBORN 1940; build_professor_census_fertility_schedule.R). Each group has
# its own common sample: the predictions also cover the 1950s, which no census
# in hand observes. "county_faculty", "ams_county" and "chsurv_county" rescale
# the same three measures by each county's own factor instead of the national
# one (faculty headcount / coded professors where the headcount is larger, else
# 1; prep/occupational_fertility/build_professor_county_coverage.R).
PAIRS <- tibble(
  channel = rep(c("faculty", "raw", "ams_faculty", "ams_raw", "chsurv_faculty", "chsurv_raw",
                  "county_faculty", "ams_county", "chsurv_county"), each = 2),
  measure = rep(c("count", "per_1000_births"), 9),
  amws = rep(c("n_amws", "amws_per_1000_births"), 9),
  prof = c("faculty_births", "faculty_births_per_1000_births",
           "prof_births_plus_deans", "prof_births_plus_deans_per_1000_births",
           "ams_faculty_births", "ams_faculty_births_per_1000_births",
           "ams_births", "ams_births_per_1000_births",
           "chsurv_faculty_births", "chsurv_faculty_births_per_1000_births",
           "chsurv_births", "chsurv_births_per_1000_births",
           "county_faculty_births", "county_faculty_births_per_1000_births",
           "ams_county_faculty_births", "ams_county_faculty_births_per_1000_births",
           "chsurv_county_faculty_births", "chsurv_county_faculty_births_per_1000_births"),
  sample = rep(c("common", "common", "common_ams", "common_ams", "common_chsurv", "common_chsurv",
                 "common", "common_ams", "common_chsurv"), each = 2)
)
CENSUS_CHANNELS <- c("faculty", "raw")
CHANNEL_COL <- c(faculty = "faculty_births", raw = "prof_births_plus_deans")

# --------------------------------------------------------------------------- #
# Data
# --------------------------------------------------------------------------- #
inputs <- read_temporal_support_inputs()
read_channel <- function(name) {
  read_csv(file.path(CHANNEL_DIR, name), show_col_types = FALSE,
           col_types = cols(GEOID = col_character())) %>%
    mutate(GEOID = pad_geoid(GEOID), decade = as.integer(decade)) %>%
    select(GEOID, decade, prof_births, prof_births_plus_deans, faculty_births, county_faculty_births, father_births,
           all_births, years_interpolated)
}
# Predicted channels (build_professor_ams_channel.R): <prefix>_births and
# <prefix>_faculty_births per county x decade.
read_predicted <- function(prefix, alternative) {
  name <- paste0(prefix, "_children_county_decade", if (alternative) "_alt" else "", ".csv")
  read_csv(file.path(CHANNEL_DIR, name), show_col_types = FALSE,
           col_types = cols(GEOID = col_character())) %>%
    mutate(GEOID = pad_geoid(GEOID), decade = as.integer(decade)) %>%
    select(GEOID, decade, all_of(paste0(prefix, c("_births", "_faculty_births", "_county_faculty_births"))))
}
PREDICTED <- c("ams", "chsurv")
add_channel <- function(panel, channel, alternative) {
  out <- panel %>% left_join(channel, by = c("GEOID", "decade"))
  for (prefix in PREDICTED) {
    out <- out %>% left_join(read_predicted(prefix, alternative), by = c("GEOID", "decade"))
    for (col in paste0(prefix, c("_births", "_faculty_births", "_county_faculty_births"))) {
      out[[paste0(col, "_per_1000_births")]] <- if_else(out$county_births_estimate > 0,
                                                        1000 * out[[col]] / out$county_births_estimate,
                                                        NA_real_)
    }
  }
  out %>%
    mutate(
      prof_births_per_1000_births = if_else(county_births_estimate > 0,
                                            1000 * prof_births / county_births_estimate, NA_real_),
      prof_births_plus_deans_per_1000_births = if_else(
        county_births_estimate > 0, 1000 * prof_births_plus_deans / county_births_estimate, NA_real_),
      county_faculty_births_per_1000_births = if_else(
        county_births_estimate > 0, 1000 * county_faculty_births / county_births_estimate, NA_real_),
      faculty_births_per_1000_births = if_else(county_births_estimate > 0,
                                               1000 * faculty_births / county_births_estimate, NA_real_),
      coverage_all_births = if_else(county_births_estimate > 0,
                                    all_births / county_births_estimate, NA_real_)
    )
}
panel_std <- add_channel(inputs$panel, read_channel("professor_children_county_decade.csv"), FALSE)
panel_alt <- add_channel(inputs$panel_alt, read_channel("professor_children_county_decade_alt.csv"), TRUE)
stacks <- list(
  standard_decade = build_stack(inputs$units, panel_std, "g_std", "standard_decade"),
  alternative_decade = build_stack(inputs$units, panel_alt, "g_shift", "alternative_decade")
)
spec_stack <- function(stack, spec) {
  if (!spec$balance) return(stack)
  filter_balanced_event_time(stack, seq(spec$min_e, spec$max_e, 10L))
}

# --------------------------------------------------------------------------- #
# Estimator: 03's CS-DID, keeping the re-normalised influence functions.
# Bootstrap is off: it does not move point estimates, and every SE here is
# recomputed from the influence functions, as in 03.
# --------------------------------------------------------------------------- #
fit_es <- function(data, outcome, spec) {
  d <- prepare_event_study_sample(data, outcome, "births_baseline")
  att <- did::att_gt(
    yname = "y", tname = "decade", idname = "stack_unit_num", gname = "g",
    xformla = ~ x, data = d, control_group = "nevertreated",
    est_method = "reg", base_period = "universal",
    allow_unbalanced_panel = TRUE, bstrap = FALSE, cband = FALSE
  )
  dyn <- did::aggte(att, type = "dynamic", na.rm = TRUE, min_e = spec$min_e,
                    max_e = spec$max_e, bstrap = FALSE, cband = FALSE)
  if_mat <- dyn$inf.function$dynamic.inf.func.e
  ref <- which(dyn$egt == REFERENCE_E)
  stopifnot(length(ref) == 1L)
  if_new <- sweep(if_mat, 1, if_mat[, ref], "-")
  est <- dyn$att.egt - dyn$att.egt[ref]
  se <- sqrt(colMeans(if_new^2) / nrow(if_new))
  list(
    es = tibble(event_time = dyn$egt, estimate = est, se = se,
                ci_low = est - Z * se, ci_high = est + Z * se),
    inf = if_new, egt = dyn$egt, n = nrow(if_new), rows = nrow(d),
    n_events = n_distinct(d$event_id),
    n_treated = n_distinct(d$stack_unit_num[d$sample_role == "treated"]),
    n_control = n_distinct(d$stack_unit_num[d$sample_role == "runner_up"])
  )
}

# --------------------------------------------------------------------------- #
# 1-3. Reproduction, channel event studies, break-even p, adjusted ATTs
# --------------------------------------------------------------------------- #
es_rows <- list(); repro <- list(); breakeven <- list(); grid_rows <- list(); fits_keep <- list()
for (spec_name in names(SPECS)) {
  spec <- SPECS[[spec_name]]
  for (timing in names(stacks)) {
    stack <- spec_stack(stacks[[timing]], spec)
    say(spec_name, " | ", timing, ": reproduction on the original sample")
    for (outcome in c("n_amws", "amws_per_1000_births")) {
      f <- fit_es(stack, outcome, spec)
      es_rows[[length(es_rows) + 1L]] <- f$es %>%
        mutate(spec = spec_name, timing = timing, outcome = outcome, sample = "original",
               n_events = f$n_events, n_treated = f$n_treated, n_control = f$n_control)
      repro[[length(repro) + 1L]] <- tibble(spec = spec_name, timing = timing, outcome = outcome,
                                            e50 = f$es$estimate[f$es$event_time == 50L],
                                            n_events = f$n_events)
    }
    # Common samples: rows where every AMWS outcome and every channel outcome of
    # the group is observed, so the influence functions line up row for row.
    # The census channels share one; the AMS channels another.
    commons <- list(
      common = stack %>%
        filter(!is.na(n_amws), !is.na(prof_births), !is.na(amws_per_1000_births),
               !is.na(prof_births_per_1000_births), !is.na(prof_births_plus_deans),
               !is.na(prof_births_plus_deans_per_1000_births),
               !is.na(faculty_births), !is.na(faculty_births_per_1000_births),
               !is.na(county_faculty_births), !is.na(county_faculty_births_per_1000_births)),
      common_ams = stack %>%
        filter(!is.na(n_amws), !is.na(amws_per_1000_births), !is.na(ams_births),
               !is.na(ams_births_per_1000_births), !is.na(ams_faculty_births),
               !is.na(ams_faculty_births_per_1000_births), !is.na(ams_county_faculty_births),
               !is.na(ams_county_faculty_births_per_1000_births)),
      common_chsurv = stack %>%
        filter(!is.na(n_amws), !is.na(amws_per_1000_births), !is.na(chsurv_births),
               !is.na(chsurv_births_per_1000_births), !is.na(chsurv_faculty_births),
               !is.na(chsurv_faculty_births_per_1000_births), !is.na(chsurv_county_faculty_births),
               !is.na(chsurv_county_faculty_births_per_1000_births)))
    for (k in seq_len(nrow(PAIRS))) {
      pr <- PAIRS[k, ]
      common <- commons[[pr$sample]]
      say(spec_name, " | ", timing, " | ", pr$channel, " | ", pr$measure, ": ", pr$sample, " sample")
      fa <- fit_es(common, pr$amws, spec)
      fp <- fit_es(common, pr$prof, spec)
      stopifnot(identical(fa$egt, fp$egt), fa$n == fp$n)
      fits_keep[[paste(spec_name, timing, pr$channel, pr$measure, sep = "__")]] <- list(a = fa, p = fp)
      # The AMWS event study is the same within a sample group; record it once.
      keep <- if (pr$channel %in% c("faculty", "ams_faculty", "chsurv_faculty")) list(list(fa, pr$amws), list(fp, pr$prof)) else
        list(list(fp, pr$prof))
      for (f in keep) {
        es_rows[[length(es_rows) + 1L]] <- f[[1]]$es %>%
          mutate(spec = spec_name, timing = timing, outcome = f[[2]], sample = pr$sample,
                 n_events = f[[1]]$n_events, n_treated = f[[1]]$n_treated,
                 n_control = f[[1]]$n_control)
      }
      # Professors alone (012-029, no presidents or deans), kept as a record.
      if (pr$measure == "count" && pr$channel == "raw") {
        fd <- fit_es(common, "prof_births", spec)
        es_rows[[length(es_rows) + 1L]] <- fd$es %>%
          mutate(spec = spec_name, timing = timing, outcome = "prof_births",
                 sample = "common", n_events = fd$n_events, n_treated = fd$n_treated,
                 n_control = fd$n_control)
      }
      n <- fa$n
      vaa <- colMeans(fa$inf^2) / n
      vbb <- colMeans(fp$inf^2) / n
      vab <- colMeans(fa$inf * fp$inf) / n
      a <- fa$es$estimate; b <- fp$es$estimate
      # Fieller: p such that (a - p b)^2 <= Z^2 (vaa - 2 p vab + p^2 vbb).
      qa <- b^2 - Z^2 * vbb; qb <- -2 * (a * b - Z^2 * vab); qc <- a^2 - Z^2 * vaa
      disc <- qb^2 - 4 * qa * qc
      bounded <- qa > 0 & disc >= 0
      lo <- ifelse(bounded, (-qb - sqrt(pmax(disc, 0))) / (2 * qa), NA_real_)
      hi <- ifelse(bounded, (-qb + sqrt(pmax(disc, 0))) / (2 * qa), NA_real_)
      p_star <- ifelse(b != 0, a / b, NA_real_)
      se_delta <- ifelse(b != 0, sqrt(vaa / b^2 + a^2 * vbb / b^4 - 2 * a * vab / b^3), NA_real_)
      breakeven[[length(breakeven) + 1L]] <- tibble(
        spec = spec_name, timing = timing, channel = pr$channel, measure = pr$measure,
        event_time = fa$egt,
        att_amws = a, se_amws = sqrt(vaa), att_prof = b, se_prof = sqrt(vbb),
        corr = vab / sqrt(vaa * vbb), p_star = p_star, p_star_se_delta = se_delta,
        p_star_fieller_lo = lo, p_star_fieller_hi = hi,
        fieller_bounded = bounded)
      for (p in P_GRID) {
        adj <- a - p * b
        se_adj <- sqrt(pmax(vaa - 2 * p * vab + p^2 * vbb, 0))
        grid_rows[[length(grid_rows) + 1L]] <- tibble(
          spec = spec_name, timing = timing, channel = pr$channel, measure = pr$measure, p = p,
          event_time = fa$egt,
          att_adjusted = adj, se = se_adj, ci_low = adj - Z * se_adj, ci_high = adj + Z * se_adj,
          share_explained = ifelse(a != 0, p * b / a, NA_real_))
      }
    }
  }
}
event_studies <- bind_rows(es_rows)
reproduction <- bind_rows(repro) %>%
  left_join(PAPER, by = c("timing", "outcome")) %>%
  mutate(paper_e50 = if_else(spec == "balanced_max70", paper_e50, NA_real_),
         matches_paper = if_else(is.na(paper_e50), NA, abs(round(e50, 3) - paper_e50) < 5e-4))
breakeven_p <- bind_rows(breakeven)
adjusted <- bind_rows(grid_rows)

# Smallest p at which the e = +50 interval reaches zero.
p_zero <- map_dfr(names(fits_keep), function(key) {
  f <- fits_keep[[key]]
  j <- which(f$a$egt == 50L)
  if (!length(j)) return(NULL)
  n <- f$a$n
  vaa <- mean(f$a$inf[, j]^2) / n; vbb <- mean(f$p$inf[, j]^2) / n
  vab <- mean(f$a$inf[, j] * f$p$inf[, j]) / n
  a <- f$a$es$estimate[j]; b <- f$p$es$estimate[j]
  ps <- seq(0, 2, by = 0.0005)
  lower <- a - ps * b - Z * sqrt(pmax(vaa - 2 * ps * vab + ps^2 * vbb, 0))
  parts <- strsplit(key, "__")[[1]]
  tibble(spec = parts[1], timing = parts[2], channel = parts[3], measure = parts[4],
         att_amws_e50 = a, att_prof_e50 = b,
         p_ci_reaches_zero = if (any(lower <= 0)) ps[which(lower <= 0)[1]] else NA_real_)
})

# --------------------------------------------------------------------------- #
# Benchmarks for p: AMWS scientists per birth outside the treated counties
# --------------------------------------------------------------------------- #
base_rates <- map_dfr(names(stacks), function(timing) {
  st <- stacks[[timing]]
  bind_rows(
    st %>% filter(sample_role == "runner_up") %>%
      summarise(group = "runner-up counties, all decades",
                amws = sum(n_amws, na.rm = TRUE),
                births = sum(county_births_estimate[!is.na(n_amws)], na.rm = TRUE)),
    st %>% filter(sample_role == "runner_up", decade >= 1880, decade <= 1930) %>%
      summarise(group = "runner-up counties, birth decades 1880-1930",
                amws = sum(n_amws, na.rm = TRUE),
                births = sum(county_births_estimate[!is.na(n_amws)], na.rm = TRUE)),
    st %>% filter(sample_role == "treated", decade - treatment_decade <= -10) %>%
      summarise(group = "treated counties, pre-treatment",
                amws = sum(n_amws, na.rm = TRUE),
                births = sum(county_births_estimate[!is.na(n_amws)], na.rm = TRUE))
  ) %>% mutate(timing = timing, amws_per_birth = amws / births)
})
base_ref <- base_rates %>% filter(group == "runner-up counties, birth decades 1880-1930") %>%
  select(timing, base_rate = amws_per_birth)
breakeven_p <- breakeven_p %>% left_join(base_ref, by = "timing") %>%
  mutate(p_star_multiple_of_base = if_else(measure == "count", p_star / base_rate, NA_real_))

# --------------------------------------------------------------------------- #
# National benchmark: what share of ALL AMWS scientists born in a decade would
# have to be professors' children for p* to hold? share = p* x national
# professors' children / national AMWS births. AMWS counts use exactly the
# filters of 01_build_temporal_support_panel.R.
# --------------------------------------------------------------------------- #
has_value <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & x != "" & !toupper(x) %in% c("NA", "N/A")
}
kept_flag <- function(x) as.character(x) %in% c("TRUE", "True", "true", "1")
is_see_previous_contaminated <- function(text) {
  text <- as.character(text); text[is.na(text)] <- ""
  see_pos <- as.integer(regexpr("\\bsee\\s+prev(?:ious)?(?:\\s+edition)?\\b", text,
                                ignore.case = TRUE, perl = TRUE))
  birth_pos <- as.integer(regexpr("(^|[[:space:],.;])b[[:space:]]+", text,
                                  ignore.case = TRUE, perl = TRUE))
  see_pos > 0L & (birth_pos < 0L | birth_pos > see_pos)
}
early <- read_csv(output_file_path("amws", "amws_combined_us_geocoded.csv"),
                  col_select = c(kept, birth_year, geoid), col_types = cols(.default = col_character()),
                  show_col_types = FALSE) %>%
  filter(kept_flag(kept), has_value(birth_year), has_value(geoid)) %>%
  transmute(year = suppressWarnings(as.integer(birth_year)))
ed86 <- read_csv(file.path(require_det_dir(), "Data", "processed", "amws", "amws_ed86_final.csv"),
                 col_select = c(birth_year, birth_country, geo_geoid, raw_text_adjusted),
                 col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
  filter(birth_country == "USA", has_value(birth_year), has_value(geo_geoid),
         !is_see_previous_contaminated(raw_text_adjusted)) %>%
  transmute(year = suppressWarnings(as.integer(birth_year)))
amws_year <- bind_rows(early, ed86) %>% filter(between(year, 1830L, 1959L)) %>% count(year, name = "amws")
bin_decade <- function(year, alternative) {
  d <- (year %/% 10L) * 10L
  if (alternative) d + 10L * (year %% 10L >= 7L) else d
}
national_amws <- bind_rows(
  amws_year %>% mutate(timing = "standard_decade", decade = bin_decade(year, FALSE)),
  amws_year %>% mutate(timing = "alternative_decade", decade = bin_decade(year, TRUE))
) %>% group_by(timing, decade) %>% summarise(amws_national = sum(amws), .groups = "drop")
national <- read_csv(file.path(CHANNEL_DIR, "national_professor_children_decade.csv"),
                     show_col_types = FALSE) %>%
  select(timing, decade, prof_births_national = prof_births_plus_deans,
         faculty_births_national = faculty_births, all_births_national = all_births) %>%
  inner_join(national_amws, by = c("timing", "decade")) %>%
  mutate(prof_children_per_amws = prof_births_national / amws_national,
         faculty_children_per_amws = faculty_births_national / amws_national,
         amws_per_surviving_child = amws_national / all_births_national)
p50 <- breakeven_p %>%
  filter(measure == "count", event_time %in% c(40L, 50L, 60L), channel %in% CENSUS_CHANNELS) %>%
  select(spec, timing, channel, event_time, p_star, p_star_fieller_lo)
national_benchmark <- p50 %>%
  inner_join(national %>% filter(decade >= 1890, decade <= 1930), by = "timing",
             relationship = "many-to-many") %>%
  mutate(children_per_amws = if_else(channel == "faculty", faculty_children_per_amws,
                                     prof_children_per_amws),
         share_of_all_amws_needed = p_star * children_per_amws,
         share_needed_at_fieller_lo = p_star_fieller_lo * children_per_amws)
harmonised_vs_raw <- breakeven_p %>%
  filter(event_time %in% c(40L, 50L, 60L), channel %in% CENSUS_CHANNELS) %>%
  select(spec, timing, measure, event_time, channel, att_prof, p_star,
         p_star_fieller_lo, p_star_fieller_hi) %>%
  pivot_wider(names_from = channel, values_from = c(att_prof, p_star, p_star_fieller_lo,
                                                    p_star_fieller_hi)) %>%
  mutate(p_star_ratio_faculty_to_raw = p_star_faculty / p_star_raw)

# --------------------------------------------------------------------------- #
# Coverage: surviving co-resident children against estimated births
# --------------------------------------------------------------------------- #
coverage <- panel_std %>%
  filter(GEOID %in% inputs$units$GEOID, !is.na(all_births), county_births_estimate > 0) %>%
  group_by(decade) %>%
  summarise(counties = n(), all_births = sum(all_births),
            county_births_estimate = sum(county_births_estimate),
            ratio = all_births / county_births_estimate, .groups = "drop")

# --------------------------------------------------------------------------- #
# 4. Assumption-free checks
# --------------------------------------------------------------------------- #
say("Per-event dose-response")
POST <- c(20L, 30L, 40L, 50L); PRE <- c(-20L, -10L)
per_event <- map_dfr(names(stacks), function(timing) {
  st <- stacks[[timing]] %>%
    mutate(event_time = decade - treatment_decade) %>%
    filter(event_time %in% c(PRE, POST), !is.na(n_amws), !is.na(prof_births_plus_deans),
           !is.na(faculty_births))
  gap <- function(v, role) mean(v[role == "treated"]) - mean(v[role == "runner_up"])
  st %>%
    group_by(event_id, college, college_type, experiment_year, event_time) %>%
    summarise(
      # did_prof is the unscaled channel: 010-029 without the factor.
      d_amws = gap(n_amws, sample_role), d_prof = gap(prof_births_plus_deans, sample_role),
      d_faculty = gap(faculty_births, sample_role),
      births = mean(county_births_estimate[sample_role == "treated"]),
      .groups = "drop") %>%
    filter(is.finite(d_amws), is.finite(d_prof), is.finite(d_faculty)) %>%
    group_by(event_id, college, college_type, experiment_year) %>%
    filter(all(PRE %in% event_time), any(POST %in% event_time)) %>%
    summarise(
      did_amws = mean(d_amws[event_time %in% POST]) - mean(d_amws[event_time %in% PRE]),
      did_prof = mean(d_prof[event_time %in% POST]) - mean(d_prof[event_time %in% PRE]),
      did_faculty = mean(d_faculty[event_time %in% POST]) - mean(d_faculty[event_time %in% PRE]),
      post_periods = sum(event_time %in% POST), .groups = "drop") %>%
    mutate(timing = timing)
})
balanced_ids <- map(names(stacks), function(timing) {
  unique(spec_stack(stacks[[timing]], SPECS$balanced_max70)$event_id)
}) %>% set_names(names(stacks))
per_event <- per_event %>%
  mutate(in_balanced = map2_lgl(event_id, timing, ~ .x %in% balanced_ids[[.y]]))
dose <- map_dfr(c(faculty = "did_faculty", raw = "did_prof"), .id = "channel", function(x_col) {
  map_dfr(names(stacks), function(timing) {
    map_dfr(c(TRUE, FALSE), function(bal) {
      d <- per_event %>% filter(timing == !!timing, if (bal) in_balanced else TRUE)
      m <- lm(reformulate(x_col, "did_amws"), data = d)
      ct <- summary(m)$coefficients
      tibble(timing = timing, sample = if (bal) "balanced events" else "all events with data",
             events = nrow(d), mean_did_amws = mean(d$did_amws), mean_did_channel = mean(d[[x_col]]),
             intercept = ct[1, 1], intercept_se = ct[1, 2],
             slope = ct[2, 1], slope_se = ct[2, 2], r2 = summary(m)$r.squared,
             corr = cor(d$did_amws, d[[x_col]]))
    })
  })
})
by_type <- per_event %>%
  mutate(type = if_else(college_type == "Land grant", "Land grant", "Other")) %>%
  group_by(timing, type) %>%
  summarise(events = n(), mean_did_amws = mean(did_amws), mean_did_prof = mean(did_prof),
            mean_did_faculty = mean(did_faculty), .groups = "drop")

say("Dropping the five largest faculty importers")
drop_top <- map_dfr(names(stacks), function(timing) {
  top <- per_event %>% filter(timing == !!timing, in_balanced) %>%
    slice_max(did_faculty, n = 5, with_ties = FALSE)
  st <- spec_stack(stacks[[timing]], SPECS$balanced_max70) %>% filter(!event_id %in% top$event_id)
  map_dfr(c("n_amws", "amws_per_1000_births"), function(outcome) {
    f <- fit_es(st, outcome, SPECS$balanced_max70)
    f$es %>% filter(event_time %in% c(30L, 40L, 50L)) %>%
      mutate(timing = timing, outcome = outcome, n_events = f$n_events,
             dropped = paste(top$college, collapse = "; "))
  })
})

# Events behind each horizon of the fully balanced per-1,000 series: every event
# in the paper's sample, and those with faculty-birth data (the common sample).
# An event counts at e when its treated county and a runner-up both have a row.
event_counts <- map_dfr(names(stacks), function(timing) {
  st <- spec_stack(stacks[[timing]], SPECS$balanced_max70) %>%
    filter(!is.na(amws_per_1000_births), !is.na(births_baseline)) %>%
    mutate(event_time = decade - treatment_decade)
  count_events <- function(d) d %>%
    group_by(event_id, event_time) %>%
    summarise(ok = any(sample_role == "treated") & any(sample_role == "runner_up"), .groups = "drop") %>%
    filter(ok) %>% count(event_time, name = "events")
  full_join(count_events(st) %>% rename(events_original = events),
            count_events(filter(st, !is.na(faculty_births_per_1000_births))) %>%
              rename(events_common = events), by = "event_time") %>%
    full_join(count_events(filter(st, !is.na(ams_faculty_births_per_1000_births))) %>%
                rename(events_common_ams = events), by = "event_time") %>%
    full_join(count_events(filter(st, !is.na(chsurv_faculty_births_per_1000_births))) %>%
                rename(events_common_chsurv = events), by = "event_time") %>%
    mutate(timing = timing, .before = 1)
})

# --------------------------------------------------------------------------- #
# Write
# --------------------------------------------------------------------------- #
write_csv(event_counts, out_path("event_counts_by_horizon.csv"))
write_csv(reproduction, out_path("reproduction_main_tex.csv"))
write_csv(event_studies, out_path("channel_event_studies.csv"))
write_csv(breakeven_p, out_path("breakeven_p.csv"))
write_csv(adjusted, out_path("adjusted_att_grid.csv"))
write_csv(p_zero, out_path("p_where_e50_ci_reaches_zero.csv"))
write_csv(base_rates, out_path("base_rates.csv"))
write_csv(national, out_path("national_professor_children_vs_amws.csv"))
write_csv(national_benchmark, out_path("national_benchmark_share_needed.csv"))
write_csv(harmonised_vs_raw, out_path("harmonised_vs_raw.csv"))
write_csv(coverage, out_path("coverage_children_vs_births.csv"))
write_csv(per_event, out_path("per_event_did.csv"))
write_csv(dose, out_path("dose_response.csv"))
write_csv(by_type, out_path("college_type.csv"))
write_csv(drop_top, out_path("drop_top5_professor_importers.csv"))

# --------------------------------------------------------------------------- #
# Figures (reference palette, light mode). The headline outcome is AMWS
# scientists per 1,000 births, main.tex Figure 2; the count versions (Figure 1)
# are kept as secondary files. Panel A always shows the paper's own series
# (original sample). The channel and the adjusted series need the common sample,
# which loses events at horizons whose births no census in hand sees (after the
# panel's last birth year): those points are drawn hollow and lighter.
# --------------------------------------------------------------------------- #
SURFACE <- "#fcfcfb"; INK <- "#0b0b0b"; INK_2 <- "#52514e"; GRID <- "#e4e3df"
BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; AQUA <- "#1baf7a"
viz_theme <- theme_minimal(base_size = 10) +
  theme(plot.background = element_rect(fill = SURFACE, colour = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3),
        axis.text = element_text(colour = INK_2), axis.title = element_text(colour = INK_2),
        strip.text = element_text(colour = INK, face = "bold", hjust = 0),
        plot.title = element_text(colour = INK, face = "bold"),
        plot.subtitle = element_text(colour = INK_2), plot.caption = element_text(colour = INK_2))
timing_label <- c(standard_decade = "Standard decades", alternative_decade = "Alternative decades")
factors <- read_csv(output_file_path("occupational_fertility", "professor_coverage_factors.csv"),
                    show_col_types = FALSE)
factor_note <- sprintf(paste("Faculty children: children of OCC1950 010-029 fathers x census coverage",
                             "factor (%.2f-%.2f, vs NCES staff)."),
                       min(factors$faculty_factor), max(factors$faculty_factor))
# Four versions of every figure: the harmonised channel (scaled to total college
# faculty) keeps the original file names; the unscaled census count (010-029,
# no factor) adds "_unscaled"; the AMS-predicted channels add "_ams_scaled" and
# "_ams_unscaled".
ams_note <- paste0("Faculty children predicted: census faculty (010-029 men) by age x American Men of Science 1956\n",
                   "births per professor (born 1870-1905, 2.5-year spacing after marriage).")
county_note <- paste0("Scaled by county: faculty headcount (Education by the Numbers panel; 1909-10, 1919-20, 1929-30, ",
                      "1939-40, 1949-50 for the\n1910-1950 censuses) / coded professors where the headcount is larger, ",
                      "else not scaled.")
chsurv_note <- paste0("Faculty children predicted: census faculty (010-029 men) by age x births per professor from ",
                      "wives' CHSURV\n(1900-1910 censuses, for faculty to 1920) and CHBORN (1940 sample line, ",
                      "for faculty 1930-1950).")
CHANNELS <- list(
  faculty = list(suffix = "", note = factor_note, sample = "common",
                 per1000 = "faculty_births_per_1000_births", count = "faculty_births",
                 panel_b_per1000 = "B. Faculty children born per 1,000 births (harmonised)",
                 panel_b_count = "B. Children born to college faculty in the county (harmonised)"),
  raw = list(suffix = "_unscaled", sample = "common",
             note = "Faculty children: children of OCC1950 010-029 fathers as enumerated, no coverage rescaling.",
             per1000 = "prof_births_plus_deans_per_1000_births", count = "prof_births_plus_deans",
             panel_b_per1000 = "B. Faculty children born per 1,000 births (unscaled census count)",
             panel_b_count = "B. Children born to college faculty in the county (unscaled census count)"),
  ams_faculty = list(suffix = "_ams_scaled", sample = "common_ams",
                     note = paste(ams_note, "Scaled by the census coverage factor."),
                     per1000 = "ams_faculty_births_per_1000_births", count = "ams_faculty_births",
                     panel_b_per1000 = "B. Faculty children per 1,000 births (AMS 1956 fertility x census faculty, scaled)",
                     panel_b_count = "B. Faculty children in the county (AMS 1956 fertility x census faculty, scaled)"),
  ams_raw = list(suffix = "_ams_unscaled", sample = "common_ams",
                 note = paste(ams_note, "No coverage rescaling."),
                 per1000 = "ams_births_per_1000_births", count = "ams_births",
                 panel_b_per1000 = "B. Faculty children per 1,000 births (AMS 1956 fertility x census faculty, unscaled)",
                 panel_b_count = "B. Faculty children in the county (AMS 1956 fertility x census faculty, unscaled)"),
  chsurv_faculty = list(suffix = "_chsurv_scaled", sample = "common_chsurv",
                        note = paste(chsurv_note, "Scaled by the census coverage factor."),
                        per1000 = "chsurv_faculty_births_per_1000_births", count = "chsurv_faculty_births",
                        panel_b_per1000 = "B. Faculty children per 1,000 births (census fertility x census faculty, scaled)",
                        panel_b_count = "B. Faculty children in the county (census fertility x census faculty, scaled)"),
  chsurv_raw = list(suffix = "_chsurv_unscaled", sample = "common_chsurv",
                    note = paste(chsurv_note, "No coverage rescaling."),
                    per1000 = "chsurv_births_per_1000_births", count = "chsurv_births",
                    panel_b_per1000 = "B. Faculty children per 1,000 births (census fertility x census faculty, unscaled)",
                    panel_b_count = "B. Faculty children in the county (census fertility x census faculty, unscaled)"),
  county_faculty = list(suffix = "_county_scaled", sample = "common",
                        note = paste0("Faculty children: children of OCC1950 010-029 fathers as enumerated.\n", county_note),
                        per1000 = "county_faculty_births_per_1000_births", count = "county_faculty_births",
                        panel_b_per1000 = "B. Faculty children born per 1,000 births (scaled by county faculty headcount)",
                        panel_b_count = "B. Children born to college faculty in the county (scaled by county faculty headcount)"),
  ams_county = list(suffix = "_ams_county_scaled", sample = "common_ams",
                    note = paste0(ams_note, "\n", county_note),
                    per1000 = "ams_county_faculty_births_per_1000_births", count = "ams_county_faculty_births",
                    panel_b_per1000 = "B. Faculty children per 1,000 births (AMS fertility x census faculty, county-scaled)",
                    panel_b_count = "B. Faculty children in the county (AMS fertility x census faculty, county-scaled)"),
  chsurv_county = list(suffix = "_chsurv_county_scaled", sample = "common_chsurv",
                       note = paste0(chsurv_note, "\n", county_note),
                       per1000 = "chsurv_county_faculty_births_per_1000_births", count = "chsurv_county_faculty_births",
                       panel_b_per1000 = "B. Faculty children per 1,000 births (census fertility x census faculty, county-scaled)",
                       panel_b_count = "B. Faculty children in the county (census fertility x census faculty, county-scaled)"))
MEASURES <- list(
  per1000 = list(amws = "amws_per_1000_births", measure = "per_1000_births", unit = "per 1,000 births",
                 panel_a = "A. AMWS scientists born per 1,000 births (main.tex Figure 2)"),
  count = list(amws = "n_amws", measure = "count", unit = "per county-decade",
               panel_a = "A. AMWS scientists born in the county (main.tex Figure 1)"))

# Event times where the common sample departs from the paper's series: fewer events.
fewer_events <- function(timing, amws_outcome, sample = "common") {
  d <- event_studies %>%
    filter(spec == "balanced_max70", timing == !!timing, outcome == amws_outcome) %>%
    select(event_time, sample, estimate) %>%
    pivot_wider(names_from = sample, values_from = estimate)
  d$event_time[abs(d$original - d[[sample]]) > 1e-9]
}
# The faculty series ends at the last birth year the panel reads from a census.
last_birth_year <- jsonlite::read_json(file.path(CHANNEL_DIR, "build_manifest.json"),
                                       simplifyVector = TRUE)$birth_years[2]
fewer_note <- function(et) {
  if (!length(et)) return("")
  sprintf("Hollow, lighter points (e >= +%d): fewer events, since no census in hand sees births after %d.",
          min(et), last_birth_year)
}

for (ch in names(CHANNELS)) {
  cs <- CHANNELS[[ch]]
  for (m in names(MEASURES)) {
    ms <- MEASURES[[m]]
    panel_b <- cs[[paste0("panel_b_", m)]]
    for (timing in names(stacks)) {
      fewer <- fewer_events(timing, ms$amws, cs$sample)
      d <- bind_rows(
        event_studies %>% filter(spec == "balanced_max70", timing == !!timing,
                                 outcome == ms$amws, sample == "original") %>%
          mutate(panel = ms$panel_a, fewer = FALSE),
        event_studies %>% filter(spec == "balanced_max70", timing == !!timing,
                                 outcome == cs[[m]], sample == cs$sample) %>%
          mutate(panel = panel_b, fewer = event_time %in% fewer)
      ) %>% mutate(panel = factor(panel, levels = c(ms$panel_a, panel_b)))
      p <- ggplot(d, aes(event_time, estimate, colour = panel)) +
        geom_hline(yintercept = 0, colour = INK_2, linewidth = 0.4, linetype = "dashed") +
        geom_vline(xintercept = 0, colour = GRID, linewidth = 0.4) +
        geom_errorbar(aes(ymin = ci_low, ymax = ci_high, alpha = fewer), width = 2, linewidth = 0.5) +
        geom_line(linewidth = 0.7) +
        geom_point(aes(fill = fewer, alpha = fewer), size = 2.4, shape = 21, stroke = 0.8) +
        facet_wrap(~ panel, ncol = 1, scales = "free_y") +
        scale_colour_manual(values = c(BLUE, ORANGE), guide = "none") +
        scale_fill_manual(values = c(`FALSE` = SURFACE, `TRUE` = SURFACE), guide = "none") +
        scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.4), guide = "none") +
        scale_x_continuous(breaks = seq(-20, 70, 10)) +
        labs(title = paste0("College allocation: AMWS effect and the faculty-children channel (",
                            tolower(timing_label[[timing]]), ")"),
             subtitle = "CS-DID, fully balanced e in [-20, +70], baseline births covariate, normalised to e = 0; 95% CIs",
             x = "Decades relative to college allocation", y = paste("Effect", ms$unit),
             caption = paste(cs$note, fewer_note(fewer), sep = "\n")) +
        viz_theme
      ggsave(out_path(paste0("fig_channel_paths_", m, "_", timing, cs$suffix, ".png")), p,
             width = 8, height = 6.5, dpi = 200)
      # Title-free copy for the companion PDF, whose LaTeX caption and notes carry the text.
      if (m == "per1000" && timing == "standard_decade") {
        ggsave(out_path(paste0("fig_channel_paths_", m, "_", timing, cs$suffix, "_pdf.pdf")),
               p + labs(title = NULL, subtitle = NULL, caption = NULL), width = 8, height = 6)
      }
    }
  }

  # How much of Figure 2 survives removing faculty children at rate p.
  P_SHOW <- c(0, 0.02, 0.05)
  p_label <- c("p = 0 (Figure 2, common sample)", "p = 2%", "p = 5%")
  for (timing in names(stacks)) {
    fewer <- fewer_events(timing, "amws_per_1000_births", cs$sample)
    d <- adjusted %>%
      filter(spec == "balanced_max70", timing == !!timing, channel == ch,
             measure == "per_1000_births", p %in% P_SHOW) %>%
      mutate(series = factor(p_label[match(p, P_SHOW)], levels = p_label),
             fewer = event_time %in% fewer)
    # Short direct labels; the legend carries the full description.
    last <- d %>% group_by(series) %>% filter(event_time == max(event_time)) %>% ungroup() %>%
      mutate(short = sub(" \\(.*\\)$", "", as.character(series)))
    dodge <- position_dodge(width = 4)
    p <- ggplot(d, aes(event_time, att_adjusted, colour = series, group = series)) +
      geom_hline(yintercept = 0, colour = INK_2, linewidth = 0.4, linetype = "dashed") +
      geom_vline(xintercept = 0, colour = GRID, linewidth = 0.4) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high, alpha = fewer), width = 1.5,
                    linewidth = 0.5, position = dodge) +
      geom_line(linewidth = 0.6, position = dodge) +
      geom_point(aes(alpha = fewer), size = 2.2, shape = 21, fill = SURFACE, stroke = 0.8,
                 position = dodge) +
      # A surface-coloured box keeps the zero line from striking through a label.
      geom_label(data = last, aes(label = short), hjust = 0, nudge_x = 4, size = 3,
                 colour = INK, fill = SURFACE, label.size = 0, label.padding = unit(0.12, "lines"),
                 show.legend = FALSE) +
      scale_colour_manual(values = c(BLUE, ORANGE, AQUA), name = NULL) +
      scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.4), guide = "none") +
      scale_x_continuous(breaks = seq(-20, 70, 10), expand = expansion(add = c(3, 30))) +
      labs(title = paste0("Figure 2 after removing faculty children (", tolower(timing_label[[timing]]), ")"),
           subtitle = paste("AMWS per 1,000 births minus p x faculty children per 1,000 births;",
                            "exact on the common sample; 95% CIs"),
           x = "Decades relative to college allocation", y = "Effect per 1,000 births",
           caption = paste("p: share of faculty children assumed to become AMWS scientists.",
                           cs$note, fewer_note(fewer), sep = "\n")) +
      viz_theme + theme(legend.position = "top", legend.justification = "left")
    ggsave(out_path(paste0("fig_figure2_adjusted_", timing, cs$suffix, ".png")), p,
           width = 8, height = 5, dpi = 200)
    if (timing == "standard_decade") {
      ggsave(out_path(paste0("fig_figure2_adjusted_", timing, cs$suffix, "_pdf.pdf")),
             p + labs(title = NULL, subtitle = NULL, caption = NULL), width = 8, height = 4.6)
    }
  }
}

plot_breakeven <- function(channel, measure, file, unit) {
  # Before e = +40 the faculty-births ATT is too small for p* to be informative.
  d <- breakeven_p %>%
    filter(spec == "balanced_max70", channel == !!channel, measure == !!measure,
           event_time >= 40, fieller_bounded) %>%
    mutate(timing = factor(timing_label[timing], levels = timing_label))
  # Fit the axis to the intervals, capped at 60% where an interval is very wide.
  y_top <- min(0.6, max(d$p_star_fieller_hi) * 1.1)
  y_bottom <- min(0, min(d$p_star_fieller_lo))
  capped <- max(d$p_star_fieller_hi) > 0.6
  p <- ggplot(d, aes(event_time, p_star, colour = timing)) +
    geom_errorbar(aes(ymin = p_star_fieller_lo, ymax = p_star_fieller_hi), width = 1.5,
                  linewidth = 0.5, position = position_dodge(width = 4)) +
    geom_point(size = 2.4, shape = 21, fill = SURFACE, stroke = 0.8,
               position = position_dodge(width = 4)) +
    scale_colour_manual(values = c(BLUE, ORANGE), name = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = seq(40, 70, 10)) +
    coord_cartesian(ylim = c(y_bottom, y_top)) +
    labs(title = "Share of college faculty's children who would have to become AMWS scientists",
         subtitle = paste0("Break-even p* = ATT(AMWS) / ATT(faculty children), ", unit,
                           ", fully balanced sample; Fieller 95% intervals"),
         x = "Decades relative to college allocation", y = "Break-even transmission rate p*",
         caption = paste(paste("From e = +40, where faculty children rise clearly; unbounded intervals",
                               "(ATT not significantly positive) are omitted.",
                               if (capped) "Axis capped at 60%." else ""),
                         CHANNELS[[channel]]$note, sep = "\n")) +
    viz_theme + theme(legend.position = "top", legend.justification = "left")
  ggsave(out_path(paste0(file, ".png")), p, width = 8, height = 5, dpi = 200)
  if (measure == "per_1000_births") {
    ggsave(out_path(paste0(file, "_pdf.pdf")),
           p + labs(title = NULL, subtitle = NULL, caption = NULL), width = 8, height = 5)
  }
}
for (ch in names(CHANNELS)) {
  plot_breakeven(ch, "per_1000_births", paste0("fig_breakeven_p", CHANNELS[[ch]]$suffix),
                 "per 1,000 births")
  plot_breakeven(ch, "count", paste0("fig_breakeven_p_count", CHANNELS[[ch]]$suffix), "counts")
}

say("Done: ", results_file_path("land_grants", "professor_channel"))
print(reproduction)
print(p_zero)
