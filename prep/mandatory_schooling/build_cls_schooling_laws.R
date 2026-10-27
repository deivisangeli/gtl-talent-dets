# Convert the Clay-Lingwall-Stephens compulsory-schooling law data to project format.
#
# Source: Clay, Lingwall and Stephens Jr (2021), "Laws, educational outcomes, and
# returns to schooling: evidence from the first wave of U.S. state compulsory
# attendance laws", Labour Economics 68, 101935. Replication package ICPSR 145122
# v1.1, shipped as replication_CLS.zip and extracted alongside it in Dropbox.
#
# The project otherwise codes compulsory schooling as one adoption date per state
# (input/compulsory_schooling_laws.csv, Lingwall 2014 Table 6.2). That is the
# extensive margin only. This adds the intensive margin: entry and exit ages, the
# work-permit and early-dropout exemptions that cut the required window short, and
# continuation-school requirements -- plus CLS's cohort-level years-of-required-
# attendance measure, keyed on bpl x yob so it joins to the 1940 full count.
#
# Three encoding traps are handled explicitly rather than silently, because each
# one silently produces a wrong answer:
#   1. 0 means "no law", not missing. Averaging the raw column averages in the
#      states that had no law.
#   2. earlyyrs / workyrs / contyrs carry sentinel codes: 99 means the law required
#      literacy rather than a year count, and 99X means "completion of primary
#      school" where X is the typical primary-course length. CLS's own do-file
#      overwrites these in place; we keep the originals and add decoded siblings,
#      because 99 -> 4 years is an assumption, not a measurement.
#   3. Arizona and Michigan repealed and later re-passed. A monotone "treated from
#      year g" coding gets Arizona wrong for 19 years.
#
# Run from any working directory:
#   Rscript prep/mandatory_schooling/build_cls_schooling_laws.R
#
# Inputs (Dropbox):
#   Data/raw/mandatory_schooling/ICPSR_145122-V1.1/state_age_limits_1880_1930_17oct2016.dta
#   Data/raw/mandatory_schooling/ICPSR_145122-V1.1/cohort_requirements_17oct2016.dta
#   input/compulsory_schooling_laws.csv                      (for the cross-check)
#
# Outputs (output/mandatory_schooling/cls/), each as .csv and .parquet:
#   cls_state_age_limits_1880_1930    state x year law panel, decoded
#   cls_cohort_requirements_1875_1912 bpl x yob required-years measures
#   cls_law_onset_by_state            first year in force, spells, diff vs Lingwall

suppressPackageStartupMessages({
  library("haven")
  library("arrow")
  library("dplyr")
  library("readr")
  library("tidyr")
})

# --------------------------------------------------------------------------- #
# Paths
# --------------------------------------------------------------------------- #

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

CLS_DIR    <- file.path(data_root, "Data/raw/mandatory_schooling/ICPSR_145122-V1.1")
LINGWALL   <- file.path(data_root, "input/compulsory_schooling_laws.csv")
OUT_DIR    <- file.path(data_root, "output/mandatory_schooling/cls")

if (!dir.exists(CLS_DIR)) {
  stop("CLS replication package not found at:\n  ", CLS_DIR,
       "\nUnzip replication_CLS.zip in Data/raw/mandatory_schooling/ first.")
}
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------------------------------- #
# Sentinel decoding
# --------------------------------------------------------------------------- #

# CLS Data Appendix: "The variables earlyyrs, workyrs, and contyrs are coded as
# '99' when the laws required literacy rather than a specific number of years of
# school. When these variables used 'completion of primary school' or similar
# language as the requirement, we code the law as '99X', where X is the length of
# a typical primary school course in the state."
SENTINEL <- c(`99` = 4L, `996` = 6L, `997` = 7L, `998` = 8L, `9910` = 10L)

decode_years <- function(x) {
  out <- as.integer(x)
  hit <- as.character(out) %in% names(SENTINEL)
  out[hit] <- unname(SENTINEL[as.character(out[hit])])
  out[!is.na(out) & out == 0L] <- NA_integer_   # 0 = provision absent, not "0 years"
  out
}

code_type <- function(x) {
  x <- as.integer(x)
  dplyr::case_when(
    is.na(x)    ~ NA_character_,
    x == 0L     ~ "none",
    x == 99L    ~ "literacy_requirement",
    x %in% c(996L, 997L, 998L, 9910L) ~ "primary_school_completion",
    TRUE        ~ "years"
  )
}

# Consecutive runs of law-in-force, so repeals survive as gaps.
spell_id <- function(in_force) {
  ifelse(in_force, cumsum(in_force & !dplyr::lag(in_force, default = FALSE)), NA_integer_)
}

write_both <- function(df, stem) {
  csv <- file.path(OUT_DIR, paste0(stem, ".csv"))
  pq  <- file.path(OUT_DIR, paste0(stem, ".parquet"))
  readr::write_csv(df, csv, na = "")
  arrow::write_parquet(df, pq)
  message("Saved: ", csv, "  (", nrow(df), " rows x ", ncol(df), " cols)")
  message("Saved: ", pq)
}

# --------------------------------------------------------------------------- #
# 1. State x year law panel
# --------------------------------------------------------------------------- #

raw_limits <- haven::read_dta(file.path(CLS_DIR, "state_age_limits_1880_1930_17oct2016.dta"))

limits <- raw_limits %>%
  mutate(
    across(c(statefip, year, entryage, exitage, earlyyrs, earlyyrs_condition,
             workage, workyrs, contage, contyrs), ~ as.integer(haven::zap_labels(.x))),
    statename = trimws(as.character(statename)),
    across(c(reference_agelimit, reference_courseofstudy),
           ~ dplyr::na_if(trimws(as.character(.x)), ""))
  ) %>%
  arrange(statefip, year) %>%
  group_by(statefip) %>%
  mutate(
    has_law      = entryage > 0L,
    law_in_force = has_law,
    law_spell_id = spell_id(law_in_force),
    has_workage  = workage > 0L,
    has_contage  = contage > 0L,
    # Sentinels preserved; decoded siblings added.
    earlyyrs_decoded  = decode_years(earlyyrs),
    workyrs_decoded   = decode_years(workyrs),
    contyrs_decoded   = decode_years(contyrs),
    earlyyrs_code_type = code_type(earlyyrs),
    workyrs_code_type  = code_type(workyrs),
    contyrs_code_type  = code_type(contyrs),
    # Nominal vs effective leaving age.
    #
    # exitage is the age at which ALL children could stop. Two statutory routes
    # let a child out earlier, and each needs BOTH an age floor and a schooling
    # condition -- hence max() within a route, min() across routes:
    #
    #   work permit   : age >= workage            AND schooling >= workyrs
    #   early dropout : age >= earlyyrs_condition AND schooling >= earlyyrs
    #
    # Accumulated schooling at age a is taken as a - entryage, i.e. continuous
    # attendance from the entry age. That is CLS's own idealisation: their
    # ca_years2 counter increments once per year while entryage <= age < exitage,
    # so a route opens at max(<age floor>, entryage + <years required>).
    #
    # Note this makes the decoded sentinels feed an AGE. Where a law required
    # literacy rather than a year count, CLS's 99 -> 4 stand-in now propagates
    # into effective_leaving_age; workyrs is coded 99 in 166 state-years.
    # earlyyrs_condition == 0 means no age floor (925 of 1,118 cases), and
    # workyrs == 0 means no schooling condition (92 of 608).
    work_exit_age = ifelse(
      has_law & has_workage,
      pmax(workage, entryage + dplyr::coalesce(workyrs_decoded, 0L)),
      NA_integer_
    ),
    early_exit_age = ifelse(
      has_law & earlyyrs > 0L,
      pmax(earlyyrs_condition, entryage + dplyr::coalesce(earlyyrs_decoded, 0L)),
      NA_integer_
    ),
    effective_leaving_age = ifelse(
      !has_law, NA_integer_,
      pmin(exitage, dplyr::coalesce(work_exit_age, .Machine$integer.max),
           dplyr::coalesce(early_exit_age, .Machine$integer.max))
    ),
    # Which provision actually governs. "none" means no exemption opened a route
    # before exitage, so the stated exit age binds.
    binding_exemption = dplyr::case_when(
      !has_law ~ NA_character_,
      !is.na(work_exit_age)  & work_exit_age  == effective_leaving_age &
        !is.na(early_exit_age) & early_exit_age == effective_leaving_age ~ "both",
      !is.na(work_exit_age)  & work_exit_age  == effective_leaving_age ~ "work_permit",
      !is.na(early_exit_age) & early_exit_age == effective_leaving_age ~ "early_dropout",
      TRUE ~ "none"
    ),
    required_window_nominal   = ifelse(has_law, exitage - entryage, NA_integer_),
    required_window_effective = ifelse(has_law, effective_leaving_age - entryage, NA_integer_)
  ) %>%
  ungroup() %>%
  select(
    statename, statefip, year,
    has_law, law_in_force, law_spell_id,
    entryage, exitage,
    earlyyrs, earlyyrs_decoded, earlyyrs_code_type, earlyyrs_condition,
    has_workage, workage, workyrs, workyrs_decoded, workyrs_code_type,
    has_contage, contage, contyrs, contyrs_decoded, contyrs_code_type,
    work_exit_age, early_exit_age, binding_exemption,
    effective_leaving_age, required_window_nominal, required_window_effective,
    reference_agelimit, reference_courseofstudy
  )

write_both(limits, "cls_state_age_limits_1880_1930")

# --------------------------------------------------------------------------- #
# 2. Cohort requirements (values carried through unchanged)
# --------------------------------------------------------------------------- #

# ca_years1          age limits only (entryage <= age < exitage), no exemptions
# ca_years2          ... minus years lost once the work-permit exemption binds
# ca_years3          ... minus years lost once the early-dropout exemption binds
# ca_years4          additional years of continuation school (workage <= age < contage)
# ca_years           min(ca_years2, ca_years3)  -- binding requirement, regular school
# ca_years5          ca_years2 + ca_years4
# ca_years_combined  min(ca_years3, ca_years5) -- headline, includes continuation
REGION_LABEL <- c(`1` = "Northeast", `2` = "Midwest", `3` = "South", `4` = "West")

cohorts <- haven::read_dta(file.path(CLS_DIR, "cohort_requirements_17oct2016.dta")) %>%
  mutate(
    across(c(bpl, region, yob, ca_years1, ca_years2, ca_years3, ca_years4,
             ca_years5, ca_years, ca_years_combined),
           ~ as.integer(haven::zap_labels(.x))),
    statename    = trimws(as.character(statename)),
    region_label = unname(REGION_LABEL[as.character(region)])
  ) %>%
  arrange(bpl, yob) %>%
  select(statename, bpl, region, region_label, yob,
         ca_years1, ca_years2, ca_years3, ca_years4, ca_years5,
         ca_years, ca_years_combined)

write_both(cohorts, "cls_cohort_requirements_1875_1912")

# --------------------------------------------------------------------------- #
# 3. Law onset by state, against the project's Lingwall dates
# --------------------------------------------------------------------------- #

# state_fips reads as character (the Alaska row carries an explicit NA law year),
# so coerce both sides before joining.
lingwall <- readr::read_csv(LINGWALL, show_col_types = FALSE) %>%
  transmute(statefip = as.integer(state_fips),
            lingwall_law_year = as.integer(compulsory_law_year))

spells <- limits %>%
  filter(law_in_force) %>%
  group_by(statename, statefip, law_spell_id) %>%
  summarise(spell_start = min(year), spell_end = max(year), .groups = "drop") %>%
  arrange(statefip, law_spell_id)

onset <- spells %>%
  group_by(statename, statefip) %>%
  summarise(
    cls_first_year_in_force = min(spell_start),
    n_spells                = n(),
    spells                  = paste0(spell_start, "-", spell_end, collapse = "; "),
    .groups = "drop"
  ) %>%
  left_join(lingwall, by = "statefip") %>%
  mutate(
    # CLS's panel starts in 1880, so a state whose law predates that shows 1880
    # as its first in-force year. That is censoring, not disagreement.
    left_censored = cls_first_year_in_force == 1880L &
      !is.na(lingwall_law_year) & lingwall_law_year < 1880L,
    has_repeal    = n_spells > 1L,
    diff_vs_lingwall = ifelse(left_censored, NA_integer_,
                              cls_first_year_in_force - lingwall_law_year)
  ) %>%
  arrange(statefip)

write_both(onset, "cls_law_onset_by_state")

# --------------------------------------------------------------------------- #
# Summary
# --------------------------------------------------------------------------- #

cat("\n=== state x year law panel ===\n")
cat("  rows:", nrow(limits), " states:", dplyr::n_distinct(limits$statefip),
    " years:", min(limits$year), "-", max(limits$year), "\n")
cat("  states with a law in force, by year:\n")
limits %>%
  filter(year %in% seq(1880, 1930, 10)) %>%
  group_by(year) %>%
  summarise(states = sum(law_in_force),
            mean_entry = round(mean(entryage[law_in_force]), 2),
            mean_exit = round(mean(exitage[law_in_force]), 2),
            mean_effective = round(mean(effective_leaving_age, na.rm = TRUE), 2),
            with_continuation = sum(has_contage), .groups = "drop") %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n=== sentinel codes decoded ===\n")
for (v in c("earlyyrs", "workyrs", "contyrs")) {
  tab <- table(limits[[paste0(v, "_code_type")]], useNA = "no")
  cat("  ", v, ": ", paste(names(tab), unname(tab), sep = "=", collapse = "  "), "\n", sep = "")
}

cat("\n=== repeals (states with more than one law spell) ===\n")
onset %>% filter(has_repeal) %>% select(statename, spells) %>% as.data.frame() %>%
  print(row.names = FALSE)

cat("\n=== vs Lingwall Table 6.2 ===\n")
cat("  left-censored by the 1880 panel start:", sum(onset$left_censored), "states\n")
cmp <- onset %>% filter(!left_censored, !is.na(diff_vs_lingwall))
cat("  comparable states:", nrow(cmp), " exact match:", sum(cmp$diff_vs_lingwall == 0L), "\n")
disagree <- cmp %>% filter(diff_vs_lingwall != 0L)
if (nrow(disagree)) {
  disagree %>% select(statename, lingwall_law_year, cls_first_year_in_force, diff_vs_lingwall) %>%
    as.data.frame() %>% print(row.names = FALSE)
}

cat("\n=== cohort requirements ===\n")
cat("  rows:", nrow(cohorts), " states:", dplyr::n_distinct(cohorts$bpl),
    " yob:", min(cohorts$yob), "-", max(cohorts$yob), "\n")
cohorts %>%
  mutate(decade = (yob %/% 10L) * 10L) %>%
  group_by(decade, region_label) %>%
  summarise(mean_combined = round(mean(ca_years_combined), 2), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = region_label, values_from = mean_combined) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

message("\nDone.")
