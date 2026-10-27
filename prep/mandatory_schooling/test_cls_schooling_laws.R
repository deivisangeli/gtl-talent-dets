# Checks for build_cls_schooling_laws.R.
#
# Runs against the written outputs, not the .dta, so it catches both a bad
# conversion and a silently changed input. Each block names the failure mode it
# rules out. Values are the ones established when the CLS package was first
# examined; if the upstream data or the recode changes, these trip.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/test_cls_schooling_laws.R

suppressPackageStartupMessages({
  library("arrow")
  library("dplyr")
  library("readr")
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
  if (!length(hit)) stop("Could not locate the Dropbox data root.")
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
})

CLS_OUT <- file.path(data_root, "output/mandatory_schooling/cls")

failures <- 0L
check <- function(label, expr) {
  ok <- isTRUE(tryCatch(expr, error = function(e) {
    message("    error: ", conditionMessage(e)); FALSE
  }))
  if (!ok) failures <<- failures + 1L
  cat(if (ok) "  PASS  " else "  FAIL  ", label, "\n", sep = "")
}

limits  <- arrow::read_parquet(file.path(CLS_OUT, "cls_state_age_limits_1880_1930.parquet"))
cohorts <- arrow::read_parquet(file.path(CLS_OUT, "cls_cohort_requirements_1875_1912.parquet"))
onset   <- arrow::read_parquet(file.path(CLS_OUT, "cls_law_onset_by_state.parquet"))

# ---- shape --------------------------------------------------------------- #
# Rules out a truncated read or a dropped state.
cat("\nShape\n")
check("law panel is 2,499 rows", nrow(limits) == 2499L)
check("49 states", dplyr::n_distinct(limits$statefip) == 49L)
check("years 1880-1930", min(limits$year) == 1880L && max(limits$year) == 1930L)
check("every state has all 51 years", {
  per <- limits %>% count(statefip)
  all(per$n == 51L)
})
check("cohort file is 1,862 rows", nrow(cohorts) == 1862L)
check("cohort yob 1875-1912", min(cohorts$yob) == 1875L && max(cohorts$yob) == 1912L)
check("cohort file has 49 states", dplyr::n_distinct(cohorts$bpl) == 49L)

# ---- sentinels ----------------------------------------------------------- #
# Rules out the decode being applied destructively, or a sentinel slipping
# through as a literal year count (9910 would read as 9,910 years of school).
cat("\nSentinel decoding\n")
for (v in c("earlyyrs", "workyrs", "contyrs")) {
  dec <- limits[[paste0(v, "_decoded")]]
  # Upper bound is 12, not 10: Nebraska's early-dropout exemption and
  # California's continuation exemption both required 12 years (high-school
  # completion). The point of the check is that no sentinel leaks through
  # undecoded -- 9910 would otherwise read as 9,910 years of school.
  check(paste0(v, "_decoded is a plausible year count (1-12) or NA"),
        all(is.na(dec) | (dec >= 1L & dec <= 12L)))
  check(paste0(v, ": originals preserved (sentinels still present)"),
        any(limits[[v]] %in% c(99L, 996L, 997L, 998L, 9910L)))
  check(paste0(v, ": every sentinel row flagged non-'years'"), {
    sent <- limits[[v]] %in% c(99L, 996L, 997L, 998L, 9910L)
    all(limits[[paste0(v, "_code_type")]][sent] != "years")
  })
  check(paste0(v, ": zeros decode to NA, not 0"),
        all(is.na(dec[limits[[v]] == 0L])))
}
check("literacy requirement (99) decodes to 4", {
  i <- which(limits$earlyyrs == 99L)
  length(i) > 0 && all(limits$earlyyrs_decoded[i] == 4L)
})
check("999-style codes absent from raw age columns",
      all(limits$entryage <= 10L) && all(limits$exitage <= 18L))

# ---- 0 is not missing ----------------------------------------------------- #
# The single easiest way to get a wrong answer from this file.
cat("\nZero-means-no-law\n")
check("has_law agrees with entryage > 0", all(limits$has_law == (limits$entryage > 0L)))
check("no-law rows have NA effective leaving age",
      all(is.na(limits$effective_leaving_age[!limits$has_law])))
check("no-law rows have NA required window",
      all(is.na(limits$required_window_nominal[!limits$has_law])))
check("16 states have no law in 1880", sum(!limits$has_law[limits$year == 1880L]) == 33L)

# ---- exemption routes ----------------------------------------------------- #
# effective_leaving_age is the earliest age ANY statutory route out opens, with
# both the age floor and the schooling condition enforced for each route. Rules
# out the column silently reverting to min(exitage, workage), which ignored the
# early-dropout route entirely and granted the work-permit exit before the child
# could have accumulated workyrs.
cat("
Exemption routes
")
in_force <- limits %>% filter(law_in_force)
check("effective leaving age never exceeds the stated exit age",
      all(in_force$effective_leaving_age <= in_force$exitage))
check("effective leaving age never precedes the entry age",
      all(in_force$effective_leaving_age >= in_force$entryage))
check("effective leaving age is never missing while a law is in force",
      !any(is.na(in_force$effective_leaving_age)))
check("work_exit_age is NA exactly when there is no work permit",
      all(is.na(in_force$work_exit_age) == !in_force$has_workage))
check("early_exit_age is NA exactly when there is no early-dropout exemption",
      all(is.na(in_force$early_exit_age) == (in_force$earlyyrs == 0L)))
check("work_exit_age respects both the age floor and workyrs", {
  w <- in_force %>% filter(has_workage)
  all(w$work_exit_age >= w$workage) &&
    all(w$work_exit_age >= w$entryage + dplyr::coalesce(w$workyrs_decoded, 0L))
})
check("early_exit_age respects both the age floor and earlyyrs", {
  e <- in_force %>% filter(earlyyrs > 0L)
  all(e$early_exit_age >= e$earlyyrs_condition) &&
    all(e$early_exit_age >= e$entryage + e$earlyyrs_decoded)
})
check("binding_exemption names a route that attains the minimum", {
  b <- in_force
  ok <- dplyr::case_when(
    b$binding_exemption == "none" ~
      b$effective_leaving_age == b$exitage,
    b$binding_exemption == "work_permit" ~
      !is.na(b$work_exit_age) & b$work_exit_age == b$effective_leaving_age,
    b$binding_exemption == "early_dropout" ~
      !is.na(b$early_exit_age) & b$early_exit_age == b$effective_leaving_age,
    b$binding_exemption == "both" ~
      !is.na(b$work_exit_age) & !is.na(b$early_exit_age) &
        b$work_exit_age == b$effective_leaving_age &
        b$early_exit_age == b$effective_leaving_age
  )
  all(ok)
})
# The edge cases that dominate, not rarities: most early-dropout provisions have
# no age floor at all, so the route is governed by accumulated years alone.
check("925 early-dropout state-years have no age floor",
      sum(in_force$earlyyrs > 0L & in_force$earlyyrs_condition == 0L) == 925L)
check("92 work-permit state-years have no schooling condition",
      sum(in_force$has_workage & in_force$workyrs == 0L) == 92L)
check("an exemption governs in 989 of 1,808 law state-years",
      sum(in_force$binding_exemption != "none") == 989L)
check("New York 1930: workyrs pushes the exit from 14 to 15", {
  ny <- limits %>% filter(statename == "New York", year == 1930L)
  ny$work_exit_age == 15L && ny$effective_leaving_age == 15L &&
    ny$binding_exemption == "work_permit"
})
check("mean effective leaving age is 14.90 in 1930", {
  m <- mean(limits$effective_leaving_age[limits$year == 1930L], na.rm = TRUE)
  isTRUE(all.equal(round(m, 2), 14.90))
})

# ---- repeals -------------------------------------------------------------- #
# Rules out a monotone "treated from year g" coding smoothing over the gaps.
cat("\nRepeal gaps\n")
repealers <- onset %>% filter(has_repeal) %>% pull(statename) %>% sort()
check("exactly Arizona and Michigan have repeal gaps",
      identical(repealers, c("Arizona", "Michigan")))
check("Arizona's gap is 1883-1898", {
  az <- limits %>% filter(statename == "Arizona", !law_in_force) %>% pull(year)
  identical(range(az), c(1883L, 1898L)) && length(az) == 16L
})
check("law_spell_id is NA exactly when the law is not in force",
      all(is.na(limits$law_spell_id) == !limits$law_in_force))

# ---- the substantive findings --------------------------------------------- #
# These are the facts the data is being brought in for. If they move, the
# underlying coding changed and the README needs revisiting.
cat("\nPolicy findings\n")
net_change <- limits %>%
  filter(law_in_force) %>%
  arrange(statefip, year) %>%
  group_by(statefip) %>%
  summarise(entry_net = dplyr::last(entryage) - dplyr::first(entryage),
            exit_net  = dplyr::last(exitage)  - dplyr::first(exitage), .groups = "drop")
check("no state ever raised its entry age", sum(net_change$entry_net > 0L) == 0L)
check("24 states lowered their entry age", sum(net_change$entry_net < 0L) == 24L)
check("38 states raised their exit age", sum(net_change$exit_net > 0L) == 38L)
check("Washington is the only state to lower its exit age", {
  i <- net_change$statefip[net_change$exit_net < 0L]
  length(i) == 1L && limits$statename[match(i, limits$statefip)] == "Washington"
})
check("mean exit age 14.25 in 1880 and 16.20 in 1930", {
  m <- limits %>% filter(law_in_force, year %in% c(1880L, 1930L)) %>%
    group_by(year) %>% summarise(m = mean(exitage), .groups = "drop")
  isTRUE(all.equal(round(m$m, 2), c(14.25, 16.20)))
})
check("work permits open a gap in 59% of states by 1930", {
  s <- limits %>% filter(year == 1930L, law_in_force)
  round(100 * mean(s$has_workage & s$workage < s$exitage)) == 59
})
check("29 states have a continuation-school law by 1930",
      sum(limits$has_contage[limits$year == 1930L]) == 29L)
check("22 states ever require continuation years (ca_years4 > 0)",
      dplyr::n_distinct(cohorts$bpl[cohorts$ca_years4 > 0L]) == 22L)

# ---- cross-source --------------------------------------------------------- #
# Rules out silently swapping law vintages: this file and the project's Lingwall
# dates must still agree where both are informative.
cat("\nAgainst Lingwall Table 6.2\n")
check("15 states are left-censored by the 1880 panel start", sum(onset$left_censored) == 15L)
comparable <- onset %>% filter(!left_censored, !is.na(diff_vs_lingwall))
check("34 states comparable, 32 match exactly",
      nrow(comparable) == 34L && sum(comparable$diff_vs_lingwall == 0L) == 32L)
check("the two disagreements are Arizona and Washington", {
  d <- comparable %>% filter(diff_vs_lingwall != 0L) %>% arrange(statename)
  identical(d$statename, c("Arizona", "Washington")) &&
    identical(as.integer(d$diff_vs_lingwall), c(-19L, 8L))
})
lingwall <- readr::read_csv(file.path(data_root, "input/compulsory_schooling_laws.csv"),
                            show_col_types = FALSE)
check("16 states first adopt in 1900 or later (Lingwall dates)",
      sum(as.integer(lingwall$compulsory_law_year) >= 1900L, na.rm = TRUE) == 16L)

# ---- join integrity ------------------------------------------------------- #
# The whole point of the cohort file is that it joins to the 1940 full count on
# BPL x yob without any record linkage.
cat("\nJoin to the 1940 full count\n")
check("every bpl is a plausible state FIPS",
      all(cohorts$bpl >= 1L & cohorts$bpl <= 56L))
check("cohort file is unique on bpl x yob",
      nrow(cohorts) == nrow(dplyr::distinct(cohorts, bpl, yob)))
parquet_1940 <- Sys.getenv("IPUMS_1940_PARQUET_DIR", unset = "D:/ipums_1940_fullcount")
if (dir.exists(file.path(parquet_1940, "person"))) {
  bpl_1940 <- arrow::open_dataset(file.path(parquet_1940, "person")) %>%
    filter(BPL <= 56L) %>% distinct(BPL) %>% collect()
  check("every cohort-file state appears in the 1940 BPL domain",
        all(cohorts$bpl %in% bpl_1940$BPL))
} else {
  cat("  ....  1940 parquet not present; BPL-domain check skipped\n")
}

cat("\n", if (failures == 0L) "All checks passed." else paste0(failures, " check(s) FAILED."),
    "\n", sep = "")
quit(status = if (failures == 0L) 0L else 1L)
