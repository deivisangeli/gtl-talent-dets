# Rank counties by how much room the first compulsory law had to move, measured
# BEFORE the law, and test whether that room predicts the post-law gain.
#
# A county's own before/after change carries almost no signal (split-half
# reliability ~0.07 for meets_min, see county_attendance_cohort.md), so this ranks
# on pre-law LEVELS instead, three ways:
#
#   shortfall   share of the 5 cohorts just too old to be covered who did NOT
#               complete the law's minimum years (1 - meets_min), 1940 attainment.
#               law_min runs 4-9 years across states, so the headline rank is
#               within state; shortfall_fixed6 (years < 6) is the cross-state version.
#   bite        pre-law non-attendance among the children the law would later
#               require, from the childhood census rows (county_attendance_bite).
#               bite_std_8_14; the cross-state list keeps the 11 states whose
#               baseline is 1910, so every county is read in the same census.
#   pre_gap_hs  average years of schooling (capped at 12) in the 5 unexposed cohorts
#               minus the state's law minimum: how far below or above the legal
#               floor the county stood. law_min is a state constant, so within-state
#               results equal those on raw years (checked); pooled results use the
#               gap as the cross-state yardstick. Raw pre_years_hs is also carried.
#
# The shortfall and the post-law change share the same pre-law men, so noise in
# that mean would produce a spurious negative slope. The regressions therefore
# take the shortfall from one random half of each county's men and the change from
# the other. The bite comes from different data and needs no split.
#
# Inputs:  output/mandatory_schooling/attendance_cohort/students.parquet
#          output/mandatory_schooling/county_attendance_bite/bite_county.csv
#          input/nhgis0005_ts_nominal_county.csv   (county names)
# Outputs: results/mandatory_schooling/attendance_cohort/prelaw_room_*
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/rank_county_prelaw_room.R

suppressPackageStartupMessages({
  library("dplyr")
  library("tidyr")
  library("readr")
  library("ggplot2")
  library("fixest")
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
BITE <- file.path(data_root, "output/mandatory_schooling/county_attendance_bite/bite_county.csv")
NAMES <- file.path(data_root, "input/nhgis0005_ts_nominal_county.csv")
OUT_DIR <- file.path(data_root, "results/mandatory_schooling/attendance_cohort")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

WINDOW_K <- 5L
MIN_PRE <- 25L          # estimable
MIN_RANK <- 200L        # rankable
FIXED_THRESHOLD <- 6L   # cross-state comparable shortfall

# --------------------------------------------------------------------------- #
# County names
# --------------------------------------------------------------------------- #

names_tbl <- read_csv(NAMES, col_select = c(STATEFP, COUNTYFP, COUNTY),
                      col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  slice(-1) %>%                                   # the NHGIS label row
  filter(!is.na(STATEFP), !is.na(COUNTYFP)) %>%
  mutate(county_geoid = as.integer(STATEFP) * 1000L + as.integer(COUNTYFP)) %>%
  distinct(county_geoid, .keep_all = TRUE) %>%
  select(county_geoid, county = COUNTY)

# --------------------------------------------------------------------------- #
# 1. County x half x side means from the person panel
# --------------------------------------------------------------------------- #

con <- dbConnect(duckdb::duckdb())
cells <- dbGetQuery(con, sprintf("
  SELECT county_geoid, st0 AS statefip, any_value(school_statename) AS statename,
         any_value(law_min) AS law_min, half,
         CASE WHEN rel_cohort < 0 THEN 'pre' ELSE 'post' END AS side,
         count(*) AS n,
         avg(meets_min) AS meets_min, avg(meets_min_effective) AS meets_min_effective,
         avg((years < %d)::INTEGER) AS below_fixed, avg(years_hs) AS years_hs,
         avg(gap_hs) AS gap_hs, avg(ever_attended) AS ever_attended,
         avg(hs_complete) AS hs_complete, avg(years) AS years
  FROM read_parquet('%s')
  WHERE has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL AND in_law_state
    AND rel_cohort BETWEEN %d AND %d
  GROUP BY county_geoid, st0, half, side",
  FIXED_THRESHOLD, gsub("\\\\", "/", STUDENTS), -WINDOW_K, WINDOW_K - 1L))
dbDisconnect(con, shutdown = TRUE)

pool <- function(d) d %>%
  summarise(across(c(meets_min, meets_min_effective, below_fixed, years_hs, gap_hs, ever_attended,
                     hs_complete, years),
                   ~ weighted.mean(.x, n)), n = sum(n), .groups = "drop")

full <- cells %>% group_by(county_geoid, statefip, statename, law_min, side) %>% pool()
by_half <- cells %>% group_by(county_geoid, half, side) %>% pool()

pre <- full %>% filter(side == "pre") %>%
  transmute(county_geoid, statefip, statename, law_min, n_pre = n,
            shortfall = 1 - meets_min, shortfall_effective = 1 - meets_min_effective,
            shortfall_fixed6 = below_fixed, mean_years_hs_pre = years_hs, mean_gap_hs_pre = gap_hs,
            ever_attended_pre = ever_attended, hs_complete_pre = hs_complete,
            # pre-law average years of schooling: capped at 12 and uncapped, and each
            # measured against the state's law minimum -- one fixed number per state, so
            # within-state comparisons are unchanged and cross-state ones are put on the
            # common footing of distance to the legal floor. pre_gap_hs is the headline.
            pre_years_hs = years_hs, pre_years = years,
            pre_gap_hs = years_hs - law_min, pre_gap = years - law_min)
post <- full %>% filter(side == "post") %>%
  transmute(county_geoid, n_post = n, meets_min_post = meets_min, years_hs_post = years_hs,
            ever_attended_post = ever_attended, hs_complete_post = hs_complete)

shortfall_tbl <- pre %>%
  left_join(post, by = "county_geoid") %>%
  mutate(change_meets_min = meets_min_post - (1 - shortfall),
         change_years_hs = years_hs_post - mean_years_hs_pre,
         change_ever_attended = ever_attended_post - ever_attended_pre,
         change_hs_complete = hs_complete_post - hs_complete_pre) %>%
  filter(n_pre >= MIN_PRE)

# Split-half reliability of the pre-law shortfall, on the rankable counties.
halves_pre <- by_half %>% filter(side == "pre") %>%
  select(county_geoid, half, n, meets_min, below_fixed, years_hs) %>%
  pivot_wider(names_from = half, values_from = c(n, meets_min, below_fixed, years_hs))
rel_pool <- halves_pre %>% semi_join(filter(shortfall_tbl, n_pre >= MIN_RANK), by = "county_geoid")
sb <- function(r) 2 * r / (1 + r)
r_short <- cor(rel_pool$meets_min_0, rel_pool$meets_min_1, use = "complete.obs")
r_fixed <- cor(rel_pool$below_fixed_0, rel_pool$below_fixed_1, use = "complete.obs")
# Within state, where the headline rank lives.
r_short_within <- rel_pool %>%
  left_join(select(shortfall_tbl, county_geoid, statename), by = "county_geoid") %>%
  group_by(statename) %>%
  mutate(a = meets_min_0 - mean(meets_min_0, na.rm = TRUE), b = meets_min_1 - mean(meets_min_1, na.rm = TRUE)) %>%
  ungroup() %>% summarise(r = cor(a, b, use = "complete.obs")) %>% pull(r)
r_years <- cor(rel_pool$years_hs_0, rel_pool$years_hs_1, use = "complete.obs")
# Pooled across states the gap to the law minimum is a different variable from raw
# years (law_min shifts whole states), so it gets its own reliability.
gap_rel <- rel_pool %>% left_join(select(shortfall_tbl, county_geoid, law_min), by = "county_geoid")
r_gap <- cor(gap_rel$years_hs_0 - gap_rel$law_min, gap_rel$years_hs_1 - gap_rel$law_min, use = "complete.obs")
r_years_within <- rel_pool %>%
  left_join(select(shortfall_tbl, county_geoid, statename), by = "county_geoid") %>%
  group_by(statename) %>%
  mutate(a = years_hs_0 - mean(years_hs_0, na.rm = TRUE), b = years_hs_1 - mean(years_hs_1, na.rm = TRUE)) %>%
  ungroup() %>% summarise(r = cor(a, b, use = "complete.obs")) %>% pull(r)

shortfall_tbl <- shortfall_tbl %>%
  mutate(rankable = n_pre >= MIN_RANK) %>%
  group_by(statename) %>%
  mutate(rank_within_state = ifelse(rankable, rank(-ifelse(rankable, shortfall, NA), na.last = "keep", ties.method = "min"), NA),
         n_rankable_in_state = sum(rankable),
         pctile_within_state = ifelse(rankable, 1 - (rank_within_state - 1) / pmax(n_rankable_in_state - 1, 1), NA),
         # fewest pre-law years = most room = rank 1
         years_rank_within_state = ifelse(rankable, rank(ifelse(rankable, pre_gap_hs, NA), na.last = "keep",
                                                         ties.method = "min"), NA)) %>%
  ungroup()

# --------------------------------------------------------------------------- #
# 2. Attendance bite, restricted to the 13 law states
# --------------------------------------------------------------------------- #

law_states <- distinct(shortfall_tbl, statefip, statename)
bite_tbl <- read_csv(BITE, show_col_types = FALSE) %>%
  filter(bite_usable, statefip %in% law_states$statefip) %>%
  select(county_geoid, statefip, baseline_year, bite_std_8_14, bite_std_8_14_1900,
         cell_usable_1900, bite_se, n_required, coverage) %>%
  left_join(law_states, by = "statefip") %>%
  group_by(statename) %>%
  mutate(bite_rank_within_state = rank(-bite_std_8_14, ties.method = "min"),
         bite_pctile_within_state = 1 - (bite_rank_within_state - 1) / pmax(n() - 1, 1)) %>%
  ungroup()

# --------------------------------------------------------------------------- #
# 3. Agreement
# --------------------------------------------------------------------------- #

both <- inner_join(shortfall_tbl, select(bite_tbl, -statefip, -statename), by = "county_geoid")
spearman <- function(a, b) suppressWarnings(cor(a, b, method = "spearman", use = "complete.obs"))
agree_state <- both %>% filter(rankable) %>% group_by(statename) %>%
  summarise(counties = n(), rho = spearman(shortfall, bite_std_8_14),
            rho_years_shortfall = spearman(pre_years_hs, shortfall),
            rho_years_bite = spearman(pre_years_hs, bite_std_8_14), .groups = "drop")
agreement <- tibble(
  comparison = c("shortfall vs bite_std_8_14, pooled", "shortfall_fixed6 vs bite_std_8_14_1900, pooled",
                 "shortfall vs bite_std_8_14, within state (median of states)"),
  counties = c(sum(both$rankable), sum(both$rankable & both$cell_usable_1900 %in% TRUE),
               sum(agree_state$counties)),
  spearman = c(with(filter(both, rankable), spearman(shortfall, bite_std_8_14)),
               with(filter(both, rankable, cell_usable_1900 %in% TRUE), spearman(shortfall_fixed6, bite_std_8_14_1900)),
               median(agree_state$rho[agree_state$counties >= 5], na.rm = TRUE)))
agreement <- bind_rows(agreement, tibble(
  comparison = c("pre_years_hs vs shortfall, pooled", "pre_years_hs vs shortfall, within state (median)",
                 "pre_years_hs vs bite_std_8_14, pooled", "pre_years_hs vs bite_std_8_14, within state (median)"),
  counties = c(sum(shortfall_tbl$rankable), sum(agree_state$counties), sum(both$rankable), sum(agree_state$counties)),
  spearman = c(with(filter(shortfall_tbl, rankable), spearman(pre_years_hs, shortfall)),
               median(agree_state$rho_years_shortfall[agree_state$counties >= 5], na.rm = TRUE),
               with(filter(both, rankable), spearman(pre_years_hs, bite_std_8_14)),
               median(agree_state$rho_years_bite[agree_state$counties >= 5], na.rm = TRUE))))

# --------------------------------------------------------------------------- #
# 4. Does room predict the gain?
# --------------------------------------------------------------------------- #

# Shortfall from half 0, change from half 1.
split <- by_half %>%
  select(county_geoid, half, side, n, meets_min, years_hs, ever_attended, hs_complete) %>%
  pivot_wider(names_from = c(side, half), values_from = c(n, meets_min, years_hs, ever_attended, hs_complete)) %>%
  transmute(county_geoid,
            n_pre_0, n_pre_1, n_post_1,
            shortfall_h0 = 1 - meets_min_pre_0,
            pre_years_hs_h0 = years_hs_pre_0,
            change_hs_complete_h1 = hs_complete_post_1 - hs_complete_pre_1,
            change_meets_min_h1 = meets_min_post_1 - meets_min_pre_1,
            change_years_hs_h1 = years_hs_post_1 - years_hs_pre_1,
            change_ever_attended_h1 = ever_attended_post_1 - ever_attended_pre_1) %>%
  inner_join(select(shortfall_tbl, county_geoid, statename, law_min), by = "county_geoid") %>%
  mutate(pre_gap_hs_h0 = pre_years_hs_h0 - law_min) %>%
  filter(n_pre_0 >= MIN_PRE, n_pre_1 >= MIN_PRE, n_post_1 >= MIN_PRE)

reg_data_bite <- both %>% filter(!is.na(n_post), n_post >= MIN_PRE)
fit_row <- function(f, measure, outcome, note) {
  data.frame(measure = measure, outcome = outcome, counties = nobs(f),
             slope = unname(coef(f)[1]), slope_se = unname(se(f)[1]),
             within_r2 = unname(r2(f, "wr2")), note = note)
}
regs <- bind_rows(
  lapply(c("meets_min", "years_hs", "ever_attended", "hs_complete"), function(o) bind_rows(
    fit_row(feols(as.formula(sprintf("change_%s_h1 ~ shortfall_h0 | statename", o)), split, vcov = "hetero"),
            "shortfall (half 0)", o, "change from the other half of the men"),
    fit_row(feols(as.formula(sprintf("change_%s ~ bite_std_8_14 | statename", o)), reg_data_bite, vcov = "hetero"),
            "bite_std_8_14", o, "full sample; bite from childhood census rows"),
    fit_row(feols(as.formula(sprintf("change_%s_h1 ~ pre_gap_hs_h0 | statename", o)), split, vcov = "hetero"),
            "pre-law years - law minimum (half 0)", o,
            "state FE; change from the other half of the men; slope per pre-law year"),
    # Pooled across states: here subtracting law_min matters, so this row is the
    # one that uses the gap to the legal floor as a cross-state yardstick.
    fit_row(feols(as.formula(sprintf("change_%s_h1 ~ pre_gap_hs_h0", o)), split, vcov = "hetero"),
            "pre-law years - law minimum (half 0), pooled", o,
            "no state FE; change from the other half of the men")
  ))
)
# law_min is a state constant, so with state FE the gap and raw years must give the
# same slope -- check it rather than assume it.
for (o in c("meets_min", "years_hs")) {
  b_raw <- coef(feols(as.formula(sprintf("change_%s_h1 ~ pre_years_hs_h0 | statename", o)), split))[1]
  b_gap <- coef(feols(as.formula(sprintf("change_%s_h1 ~ pre_gap_hs_h0 | statename", o)), split))[1]
  stopifnot(abs(b_raw - b_gap) < 1e-10)
}
# Half-sample reliability of the shortfall attenuates its slope; report the corrected value.
regs <- regs %>% mutate(slope_corrected = case_when(measure == "shortfall (half 0)" ~ slope / r_short,
                                                     measure == "pre-law years - law minimum (half 0), pooled" ~ slope / r_gap,
                                                     grepl("^pre-law years", measure) ~ slope / r_years_within,
                                                     TRUE ~ NA_real_))

# --------------------------------------------------------------------------- #
# 5. Tables and plots
# --------------------------------------------------------------------------- #

nice <- function(d) d %>% left_join(names_tbl, by = "county_geoid") %>%
  mutate(county = coalesce(county, as.character(county_geoid)))

county_out <- shortfall_tbl %>%
  left_join(select(bite_tbl, county_geoid, baseline_year, bite_std_8_14, bite_std_8_14_1900,
                   cell_usable_1900, bite_se, n_required, bite_rank_within_state, bite_pctile_within_state),
            by = "county_geoid") %>%
  nice() %>% relocate(county, .after = county_geoid)
write_csv(county_out, file.path(OUT_DIR, "prelaw_room_county.csv"))

top_short <- county_out %>% filter(rankable) %>% arrange(desc(shortfall_fixed6)) %>% head(20)
top_short_law <- county_out %>% filter(rankable) %>% arrange(desc(shortfall)) %>% head(20)
write_csv(bind_rows(mutate(top_short, list = "fixed 6-year threshold (cross-state comparable)"),
                    mutate(top_short_law, list = "own law minimum (mixes thresholds)")),
          file.path(OUT_DIR, "prelaw_room_top20_shortfall.csv"))

# Cross-state: 11 of the 13 states share the 1910 baseline (Delaware and Oklahoma
# use 1900), so the latest bite is comparable across them. The _1900 columns would
# sit 13-20 years before these laws, on a different SCHOOL reference period.
top_bite <- bite_tbl %>% filter(baseline_year == 1910L, n_required >= MIN_RANK) %>%
  arrange(desc(bite_std_8_14)) %>% head(20) %>% nice()
write_csv(top_bite, file.path(OUT_DIR, "prelaw_room_top20_bite.csv"))

reliability <- tibble(measure = c("shortfall (own minimum)", "shortfall_fixed6", "shortfall, within state",
                                  "pre-law years (capped)", "pre-law years, within state",
                                  "pre-law years - law minimum, pooled"),
                      half_r = c(r_short, r_fixed, r_short_within, r_years, r_years_within, r_gap),
                      spearman_brown = sb(c(r_short, r_fixed, r_short_within, r_years, r_years_within, r_gap)),
                      counties = nrow(rel_pool))
write_csv(bind_rows(mutate(reliability, table = "reliability"),
                    mutate(agreement, table = "agreement"),
                    mutate(agree_state, table = "agreement_by_state"),
                    mutate(regs, table = "regression")),
          file.path(OUT_DIR, "prelaw_room_regressions.csv"))

p1 <- ggplot(filter(both, rankable), aes(bite_std_8_14, shortfall)) +
  geom_point(aes(colour = statename, size = n_pre), alpha = 0.6, stroke = 0) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", fill = "grey70") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_size_area(max_size = 4, name = "Pre-law men") +
  labs(x = "Attendance bite: pre-law non-attendance, ages 8-14 (childhood census)",
       y = "Attainment shortfall: pre-law cohorts below the law's minimum (1940)",
       colour = "State", title = "Two measures of the room the law had to move",
       subtitle = sprintf("Counties with >= %d pre-law men. Spearman %.2f pooled, %.2f median within state.",
                          MIN_RANK, agreement$spearman[1], agreement$spearman[3])) +
  theme_minimal(base_size = 11) +
  theme(plot.subtitle = element_text(size = 9, colour = "grey30"))
ggsave(file.path(OUT_DIR, "prelaw_room_agreement.png"), p1, width = 9, height = 6.5, dpi = 200, bg = "white")

long <- bind_rows(
  split %>% transmute(statename, measure = "Attainment shortfall (half A of the men)", x = shortfall_h0,
                      `Meets the minimum` = change_meets_min_h1, `Years of schooling` = change_years_hs_h1,
                      `Ever attended` = change_ever_attended_h1),
  reg_data_bite %>% transmute(statename, measure = "Attendance bite (childhood census)", x = bite_std_8_14,
                              `Meets the minimum` = change_meets_min, `Years of schooling` = change_years_hs,
                              `Ever attended` = change_ever_attended),
  split %>% transmute(statename, measure = "Pre-law years minus law minimum (half A)", x = pre_gap_hs_h0,
                      `Meets the minimum` = change_meets_min_h1, `Years of schooling` = change_years_hs_h1,
                      `Ever attended` = change_ever_attended_h1)
) %>% mutate(measure = factor(measure, levels = unique(measure))) %>% pivot_longer(c(`Meets the minimum`, `Years of schooling`, `Ever attended`),
                   names_to = "outcome", values_to = "y")
p2 <- ggplot(long, aes(x, y)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(alpha = 0.25, size = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "firebrick", fill = "grey70") +
  facet_grid(outcome ~ measure, scales = "free") +
  labs(x = "Pre-law room (shortfall and bite: share, 0-1; years: gap to the law's minimum, in years)",
       y = "Post-law change (first 5 covered minus last 5 uncovered cohorts)",
       title = "Does pre-law room predict the post-law gain?",
       subtitle = "Pooled OLS line shown; the table reports slopes with state fixed effects. Shortfall and change use disjoint halves of each county's men.") +
  theme_minimal(base_size = 10) +
  theme(plot.subtitle = element_text(size = 8.5, colour = "grey30"))
ggsave(file.path(OUT_DIR, "prelaw_room_predicts_gain.png"), p2, width = 12, height = 8, dpi = 200, bg = "white")

# --------------------------------------------------------------------------- #
# Console summary
# --------------------------------------------------------------------------- #

options(width = 200)
cat(sprintf("\nShortfall counties: %d estimable (>= %d pre-law men), %d rankable (>= %d)\n",
            nrow(shortfall_tbl), MIN_PRE, sum(shortfall_tbl$rankable), MIN_RANK))
cat(sprintf("Bite counties in the law states: %d usable, %d with a 1900 baseline\n",
            nrow(bite_tbl), sum(bite_tbl$cell_usable_1900 %in% TRUE)))
cat("\nReliability\n"); print(as.data.frame(reliability %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
cat("\nAgreement\n"); print(as.data.frame(agreement %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
print(as.data.frame(agree_state %>% mutate(rho = round(rho, 2))), row.names = FALSE)
cat("\nRegressions (state FE, robust SE)\n")
print(as.data.frame(regs %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))), row.names = FALSE)
cat("\nTop 20 shortfall, fixed 6-year threshold\n")
print(as.data.frame(top_short %>% transmute(county, statename, law_min, n_pre,
  below6 = round(100 * shortfall_fixed6, 1), shortfall_own = round(100 * shortfall, 1),
  years_pre = round(mean_years_hs_pre, 2), rank_in_state = rank_within_state, of = n_rankable_in_state,
  bite = round(100 * bite_std_8_14, 1))), row.names = FALSE)
cat("\nTop 20 attendance bite, 1910 baseline, >= 200 required children\n")
print(as.data.frame(top_bite %>%
  left_join(select(shortfall_tbl, county_geoid, shortfall, shortfall_fixed6, n_pre), by = "county_geoid") %>%
  transmute(county, statename, bite = round(100 * bite_std_8_14, 1), n_required,
            rank_in_state = bite_rank_within_state, below6 = round(100 * shortfall_fixed6, 1),
            shortfall_own = round(100 * shortfall, 1), n_pre)), row.names = FALSE)
message("Wrote: ", OUT_DIR)
