# Descriptive statistics for the compulsory-schooling policy environment.
#
# Built on the Clay-Lingwall-Stephens law data converted by
# prep/mandatory_schooling/build_cls_schooling_laws.R, plus the 1940 full count
# for the one panel that weights by people.
#
# This characterises the POLICY, not an estimation sample. The motivating point is
# that an adoption date alone is a poor summary of it: entry ages barely moved,
# exit ages rose sharply, work permits gave back about half of that rise, and
# continuation schooling arrived late and only in some states. Panels B, C and E
# are the evidence for that.
#
# Panel D doubles as an end-to-end regression guard. Person-weighting the
# requirement distribution over the CLS sample reproduces their published Table 1
# to within 0.1pp, so it fails loudly if the 1940 conversion, the yob convention
# or the HIGRADED recode ever drifts.
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/describe_cls_policy_environment.R
#
# Inputs:
#   output/mandatory_schooling/cls/cls_state_age_limits_1880_1930.parquet
#   output/mandatory_schooling/cls/cls_cohort_requirements_1875_1912.parquet
#   output/mandatory_schooling/cls/cls_law_onset_by_state.parquet
#   input/compulsory_schooling_laws.csv
#   D:/ipums_1940_fullcount/person            (Panel D only; IPUMS_1940_PARQUET_DIR)
#
# Output (results/mandatory_schooling/cls_policy_descriptives/):
#   panel_a_adoption_timing.csv ... panel_e_requirement_expression.csv
#   cls_policy_environment.tex, panel_a.tex ... panel_e.tex
#   notes.txt, session_info.txt, input_manifest.csv, validation_summary.csv

suppressPackageStartupMessages({
  library("arrow")
  library("dplyr")
  library("tidyr")
  library("readr")
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

CLS_OUT      <- file.path(data_root, "output/mandatory_schooling/cls")
LINGWALL     <- file.path(data_root, "input/compulsory_schooling_laws.csv")
PARQUET_1940 <- Sys.getenv("IPUMS_1940_PARQUET_DIR", unset = "D:/ipums_1940_fullcount")
OUT_DIR      <- file.path(data_root, "results/mandatory_schooling/cls_policy_descriptives")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

inputs <- c(
  limits   = file.path(CLS_OUT, "cls_state_age_limits_1880_1930.parquet"),
  cohorts  = file.path(CLS_OUT, "cls_cohort_requirements_1875_1912.parquet"),
  onset    = file.path(CLS_OUT, "cls_law_onset_by_state.parquet"),
  lingwall = LINGWALL
)
missing <- inputs[!file.exists(inputs)]
if (length(missing)) {
  stop("Missing input(s):\n  ", paste(missing, collapse = "\n  "),
       "\nRun prep/mandatory_schooling/build_cls_schooling_laws.R first.")
}

limits  <- arrow::read_parquet(inputs[["limits"]])
cohorts <- arrow::read_parquet(inputs[["cohorts"]])
onset   <- arrow::read_parquet(inputs[["onset"]])
lingwall <- readr::read_csv(inputs[["lingwall"]], show_col_types = FALSE) %>%
  transmute(statefip = as.integer(state_fips),
            statename = state,
            law_year = as.integer(compulsory_law_year)) %>%
  filter(!is.na(law_year))

# CLS region 3. Note their coding puts MD and DC in the Northeast, KY and WV in
# the Midwest and OK in the West, so this is narrower than a census South.
SOUTH <- cohorts %>% filter(region == 3L) %>% distinct(statename) %>% pull(statename)

checks <- list()
expect <- function(label, ok, detail = "") {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = label, validation_passed = isTRUE(ok), detail = detail,
    stringsAsFactors = FALSE
  )
  invisible(ok)
}

# --------------------------------------------------------------------------- #
# Panel A: adoption timing
# --------------------------------------------------------------------------- #

panel_a <- lingwall %>%
  mutate(decade = (law_year %/% 10L) * 10L,
         is_south = statename %in% SOUTH) %>%
  count(decade, wt = NULL, name = "states") %>%
  left_join(
    lingwall %>%
      mutate(decade = (law_year %/% 10L) * 10L, is_south = statename %in% SOUTH) %>%
      group_by(decade) %>% summarise(south = sum(is_south), .groups = "drop"),
    by = "decade"
  ) %>%
  arrange(decade) %>%
  mutate(cumulative = cumsum(states),
         cumulative_pct = round(100 * cumulative / sum(states), 1),
         decade_label = paste0(decade, "s"))

adoption_median <- stats::median(lingwall$law_year)
adoption_iqr    <- stats::quantile(lingwall$law_year, c(0.25, 0.75), type = 7)
late_adopters   <- lingwall %>% filter(law_year >= 1900L)

expect("Panel A: decade counts sum to 49", sum(panel_a$states) == 49L,
       paste("sum =", sum(panel_a$states)))
expect("Panel A: 16 states first adopt in 1900 or later", nrow(late_adopters) == 16L,
       paste("n =", nrow(late_adopters)))
expect("Panel A: 11 of the late adopters are CLS-coded South",
       sum(late_adopters$statename %in% SOUTH) == 11L)

# --------------------------------------------------------------------------- #
# Panel B: age limits and exemptions over time
# --------------------------------------------------------------------------- #

panel_b <- limits %>%
  filter(year %in% seq(1880L, 1930L, 10L)) %>%
  group_by(year) %>%
  summarise(
    states_with_law   = sum(law_in_force),
    mean_entry_age    = round(mean(entryage[law_in_force]), 2),
    mean_exit_age     = round(mean(exitage[law_in_force]), 2),
    mean_effective_leaving_age = round(mean(effective_leaving_age, na.rm = TRUE), 2),
    # Share of law-states where SOME exemption opens a route before the stated
    # exit age -- work permit or early dropout, each with its schooling condition
    # enforced. The old column counted only work permits, and counted them
    # without workyrs.
    pct_exemption_binds = round(
      100 * mean(binding_exemption[law_in_force] != "none"), 1),
    mean_window_nominal   = round(mean(required_window_nominal, na.rm = TRUE), 2),
    mean_window_effective = round(mean(required_window_effective, na.rm = TRUE), 2),
    states_with_continuation = sum(has_contage),
    .groups = "drop"
  )

expect("Panel B: 16 law-states in 1880, 49 in 1930",
       panel_b$states_with_law[panel_b$year == 1880L] == 16L &&
         panel_b$states_with_law[panel_b$year == 1930L] == 49L)
expect("Panel B: mean exit age 14.25 in 1880 and 16.20 in 1930",
       isTRUE(all.equal(panel_b$mean_exit_age[panel_b$year %in% c(1880L, 1930L)],
                        c(14.25, 16.20))))
expect("Panel B: effective window never exceeds nominal",
       all(panel_b$mean_window_effective <= panel_b$mean_window_nominal))
expect("Panel B: effective leaving age is 14.90 in 1930",
       isTRUE(all.equal(panel_b$mean_effective_leaving_age[panel_b$year == 1930L], 14.90)))
expect("Panel B: an exemption binds in 989 of 1,808 law state-years", {
  inf <- limits %>% filter(law_in_force)
  sum(inf$binding_exemption != "none") == 989L
})

# --------------------------------------------------------------------------- #
# Panel C: within-state change, restricted to years the law was in force
# --------------------------------------------------------------------------- #

spell_summary <- function(col) {
  limits %>%
    filter(law_in_force) %>%
    arrange(statefip, year) %>%
    group_by(statefip) %>%
    summarise(
      n_spells = sum(.data[[col]] != dplyr::lag(.data[[col]], default = .data[[col]][1])) + 1L,
      net = dplyr::last(.data[[col]]) - dplyr::first(.data[[col]]),
      .groups = "drop"
    )
}

panel_c <- bind_rows(
  spell_summary("entryage") %>% mutate(limit = "Entry age"),
  spell_summary("exitage")  %>% mutate(limit = "Exit age")
) %>%
  group_by(limit) %>%
  summarise(
    states           = n(),
    never_changed    = sum(n_spells == 1L),
    changed_once     = sum(n_spells == 2L),
    changed_2plus    = sum(n_spells >= 3L),
    net_raised       = sum(net > 0L),
    net_unchanged    = sum(net == 0L),
    net_lowered      = sum(net < 0L),
    .groups = "drop"
  ) %>%
  arrange(desc(limit))

expect("Panel C: no state ever raised its entry age",
       panel_c$net_raised[panel_c$limit == "Entry age"] == 0L)
expect("Panel C: 38 states raised their exit age",
       panel_c$net_raised[panel_c$limit == "Exit age"] == 38L)
expect("Panel C: every state accounted for in both rows", all(panel_c$states == 49L))

# --------------------------------------------------------------------------- #
# Panel D: required years facing the CLS cohorts, weighted by people
# --------------------------------------------------------------------------- #

# CLS estimation sample: native-born white men aged 27-54 in 1940, i.e. yob
# 1885-1912. yob = 1940 - age - 1 because enumeration was 1 April and most people
# had not yet had their birthday. yrs_sch is constructed from HIGRADED, which is
# what the census actually recorded; there is no years-of-schooling variable.
BIN_BREAKS <- c(-1, 0, 5, 6, 7, 8, 20)
BIN_LABELS <- c("0 (no law)", "1-5", "6", "7", "8", "9+")
CLS_TABLE1_PCT <- c(13.5, 9.2, 19.1, 16.9, 33.4, 7.9)   # published, All Men column
CLS_TABLE1_AGE <- 38.9

have_1940 <- dir.exists(file.path(PARQUET_1940, "person"))
if (have_1940) {
  cells <- arrow::open_dataset(file.path(PARQUET_1940, "person")) %>%
    filter(AGE >= 27L, AGE <= 54L, SEX == 1L, RACE == 1L, BPL <= 56L, HIGRADED != 999L) %>%
    mutate(yob = 1940L - AGE - 1L, g = HIGRADED %/% 10L) %>%
    mutate(yrs_sch = case_when(g <= 3L ~ 0L, g <= 20L ~ g - 3L, g <= 23L ~ 17L)) %>%
    group_by(BPL, yob) %>%
    summarise(men = n(), mean_age = mean(AGE), mean_yrs_sch = mean(yrs_sch), .groups = "drop") %>%
    collect() %>%
    inner_join(cohorts, by = c("BPL" = "bpl", "yob"))

  total_men <- sum(cells$men)

  panel_d <- cells %>%
    mutate(required_years = cut(ca_years_combined, BIN_BREAKS, labels = BIN_LABELS)) %>%
    group_by(required_years) %>%
    # weighted mean must come before men is collapsed: summarise() evaluates in
    # order and later expressions see the already-reduced value.
    summarise(mean_yrs_sch = round(stats::weighted.mean(mean_yrs_sch, men), 2),
              men = sum(men),
              .groups = "drop") %>%
    mutate(pct_of_men = round(100 * men / total_men, 1),
           cls_table1_pct = CLS_TABLE1_PCT,
           diff_vs_cls = round(pct_of_men - cls_table1_pct, 2)) %>%
    select(required_years, men, pct_of_men, cls_table1_pct, diff_vs_cls, mean_yrs_sch)

  panel_d_region <- cells %>%
    mutate(cohort_decade = (yob %/% 10L) * 10L) %>%
    group_by(cohort_decade, region_label) %>%
    summarise(
      ca_years1        = round(stats::weighted.mean(ca_years1, men), 2),
      ca_years         = round(stats::weighted.mean(ca_years, men), 2),
      ca_years_combined = round(stats::weighted.mean(ca_years_combined, men), 2),
      men = sum(men), .groups = "drop"
    ) %>%
    arrange(cohort_decade, region_label)

  mean_age_all <- stats::weighted.mean(cells$mean_age, cells$men)

  # The end-to-end guard. If the 1940 conversion, the yob convention or the
  # HIGRADED recode drifts, these move.
  expect("Panel D: every bin within 0.1pp of CLS Table 1",
         all(abs(panel_d$diff_vs_cls) <= 0.1),
         paste("max |diff| =", max(abs(panel_d$diff_vs_cls))))
  expect("Panel D: mean age within 0.1 of CLS 38.9",
         abs(mean_age_all - CLS_TABLE1_AGE) <= 0.1,
         paste("mean age =", round(mean_age_all, 2)))
  expect("Panel D: shares sum to 100", abs(sum(panel_d$pct_of_men) - 100) < 0.15)
  expect("Panel D: every man assigned to a bin", sum(panel_d$men) == total_men)
} else {
  message("NOTE: 1940 parquet not found at ", PARQUET_1940, " -- Panel D skipped.")
  panel_d <- tibble::tibble()
  panel_d_region <- tibble::tibble()
  total_men <- NA_integer_
  mean_age_all <- NA_real_
  expect("Panel D: 1940 parquet available", FALSE, "skipped, parquet absent")
}

# --------------------------------------------------------------------------- #
# Panel E: how the requirements were expressed
# --------------------------------------------------------------------------- #

in_force <- limits %>% filter(law_in_force)

panel_e <- lapply(c("earlyyrs", "workyrs", "contyrs"), function(v) {
  in_force %>%
    count(code_type = .data[[paste0(v, "_code_type")]], name = "state_years") %>%
    mutate(provision = v,
           pct_of_law_state_years = round(100 * state_years / nrow(in_force), 1))
}) %>%
  bind_rows() %>%
  select(provision, code_type, state_years, pct_of_law_state_years) %>%
  arrange(provision, code_type)

expect("Panel E: state-years reconcile to the in-force panel",
       all(tapply(panel_e$state_years, panel_e$provision, sum) == nrow(in_force)))
expect("Panel E: literacy-coded rows exist for all three provisions",
       length(unique(panel_e$provision[panel_e$code_type == "literacy_requirement"])) == 3L)

# --------------------------------------------------------------------------- #
# LaTeX
# --------------------------------------------------------------------------- #

SCRIPT_REL <- "analysis/mandatory_schooling/describe_cls_policy_environment.R"

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  gsub("~", "\\\\textasciitilde{}", x)
}

fmt <- function(x, digits = 2) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits, big.mark = ","))
}
fmt_int <- function(x) ifelse(is.na(x), "--", formatC(x, format = "d", big.mark = ","))

tabular <- function(caption, colspec, header, rows) {
  c(sprintf("%% %s", caption),
    sprintf("\\begin{tabular}{%s}", colspec),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}")
}

tex_a <- tabular(
  "Panel A: adoption of the first compulsory attendance law",
  "lrrrr",
  "Decade & States & Of which South & Cumulative & Cumulative \\%",
  sprintf("%s & %s & %s & %s & %s \\\\",
          latex_escape(panel_a$decade_label), fmt_int(panel_a$states),
          fmt_int(panel_a$south), fmt_int(panel_a$cumulative),
          fmt(panel_a$cumulative_pct, 1))
)

tex_b <- tabular(
  "Panel B: age limits, exemptions and continuation school, 1880-1930",
  "lrrrrrrrr",
  paste("Year & States & Entry & Exit & Effective & Exemption & Window &",
        "Window & Continuation \\\\ & with law & age & age & leaving age &",
        "binds \\% & (nominal) & (effective) & laws"),
  sprintf("%d & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
          panel_b$year, fmt_int(panel_b$states_with_law),
          fmt(panel_b$mean_entry_age), fmt(panel_b$mean_exit_age),
          fmt(panel_b$mean_effective_leaving_age), fmt(panel_b$pct_exemption_binds, 1),
          fmt(panel_b$mean_window_nominal), fmt(panel_b$mean_window_effective),
          fmt_int(panel_b$states_with_continuation))
)

tex_c <- tabular(
  "Panel C: within-state changes while a law was in force, 1880-1930",
  "lrrrrrr",
  paste("Limit & Never & Changed & Changed & Net & Net & Net \\\\",
        "& changed & once & 2+ times & raised & unchanged & lowered"),
  sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
          latex_escape(panel_c$limit), fmt_int(panel_c$never_changed),
          fmt_int(panel_c$changed_once), fmt_int(panel_c$changed_2plus),
          fmt_int(panel_c$net_raised), fmt_int(panel_c$net_unchanged),
          fmt_int(panel_c$net_lowered))
)

tex_d <- if (nrow(panel_d)) {
  tabular(
    "Panel D: required years of attendance facing the CLS cohorts",
    "lrrrr",
    "Required years & Men & \\% of men & CLS Table 1 \\% & Mean years of schooling",
    sprintf("%s & %s & %s & %s & %s \\\\",
            latex_escape(as.character(panel_d$required_years)), fmt_int(panel_d$men),
            fmt(panel_d$pct_of_men, 1), fmt(panel_d$cls_table1_pct, 1),
            fmt(panel_d$mean_yrs_sch))
  )
} else {
  "% Panel D skipped: 1940 parquet not available"
}

tex_e <- tabular(
  "Panel E: how the schooling requirement was expressed, law-state-years",
  "llrr",
  "Provision & Coded as & State-years & \\% of law state-years",
  sprintf("%s & %s & %s & %s \\\\",
          latex_escape(panel_e$provision), latex_escape(gsub("_", " ", panel_e$code_type)),
          fmt_int(panel_e$state_years), fmt(panel_e$pct_of_law_state_years, 1))
)

tex_header <- c(
  sprintf("%% Generated by %s", SCRIPT_REL),
  sprintf("%% %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "% Sources: Clay, Lingwall and Stephens Jr (2021), Labour Economics 68, 101935;",
  "%          Lingwall (2014) dissertation Table 6.2; IPUMS 1940 full count.",
  "% Include with: \\input{tables/cls_policy_environment}  (requires booktabs)",
  "% Do not edit by hand."
)

for (nm in c("a", "b", "c", "d", "e")) {
  body <- get(paste0("tex_", nm))
  writeLines(c(tex_header, "", body), file.path(OUT_DIR, sprintf("panel_%s.tex", nm)))
}
writeLines(
  c(tex_header, "", tex_a, "", "\\bigskip", "", tex_b, "", "\\bigskip", "",
    tex_c, "", "\\bigskip", "", tex_d, "", "\\bigskip", "", tex_e),
  file.path(OUT_DIR, "cls_policy_environment.tex")
)

# --------------------------------------------------------------------------- #
# CSV, sidecars
# --------------------------------------------------------------------------- #

readr::write_csv(panel_a, file.path(OUT_DIR, "panel_a_adoption_timing.csv"))
readr::write_csv(panel_b, file.path(OUT_DIR, "panel_b_age_limits_over_time.csv"))
readr::write_csv(panel_c, file.path(OUT_DIR, "panel_c_within_state_changes.csv"))
if (nrow(panel_d)) {
  readr::write_csv(panel_d, file.path(OUT_DIR, "panel_d_required_years.csv"))
  readr::write_csv(panel_d_region, file.path(OUT_DIR, "panel_d_required_years_by_region.csv"))
}
readr::write_csv(panel_e, file.path(OUT_DIR, "panel_e_requirement_expression.csv"))

validation <- bind_rows(checks)
readr::write_csv(validation, file.path(OUT_DIR, "validation_summary.csv"))

manifest <- data.frame(
  input = names(inputs),
  path = unname(inputs),
  md5 = unname(tools::md5sum(unname(inputs))),
  stringsAsFactors = FALSE
)
readr::write_csv(manifest, file.path(OUT_DIR, "input_manifest.csv"))

notes <- c(
  "Compulsory-schooling policy environment: descriptive statistics",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("Script:", SCRIPT_REL),
  "",
  "Sources:",
  "  Law coding: Clay, Lingwall and Stephens Jr (2021), 'Laws, educational outcomes,",
  "    and returns to schooling: evidence from the first wave of U.S. state compulsory",
  "    attendance laws', Labour Economics 68, 101935. Replication package ICPSR 145122 v1.1.",
  "  Adoption dates: Lingwall (2014), dissertation Table 6.2, pp. 148-149.",
  "  Person weights: IPUMS 1940 full count (EXT1940USCB), converted by",
  "    prep/mandatory_schooling/convert_ipums_1940_fullcount.py.",
  "",
  "Inputs:",
  paste0("  ", manifest$path),
  "Input md5:",
  paste0("  ", manifest$md5, "  ", basename(manifest$path)),
  "",
  "Definitions and caveats:",
  "  - Panels B, C and E are restricted to state-years with a law in force",
  "    (law_in_force). 0 in the raw CLS data means 'no law', not missing.",
  "  - 'Effective leaving age' is min(exitage, workage) where a work permit exists.",
  "    It is the most permissive reading: it assumes the child is employed and has",
  "    met workyrs. CLS's ca_years2 enforces workyrs properly, cohort by cohort.",
  "  - Panel C reports spells within years the law was in force, so Arizona's",
  "    1883-1898 repeal and Michigan's gap do not register as age changes.",
  "  - Panel D sample is CLS's: native-born white men aged 27-54 in 1940",
  "    (BPL <= 56, RACE == 1, SEX == 1, HIGRADED != 999), yob = 1940 - AGE - 1.",
  "    yrs_sch is constructed from HIGRADED; there is no years-of-schooling variable.",
  "  - Panel D compares against CLS Table 1 column 1 (All Men). Small differences are",
  "    expected: their N is 19,055,381 against ours here, and the IPUMS full count runs",
  "    ~0.2% above the published 1940 population.",
  "  - Panel E: 'literacy requirement' rows are laws that required literacy rather",
  "    than a year count. CLS recode these to 4 years when building ca_years*, which",
  "    is an assumption, not a measurement. This panel shows how much rests on it.",
  "  - CLS region 3 ('South') is 11 states and excludes MD, DC, KY, WV and OK.",
  "",
  paste("Adoption: median", adoption_median, "| IQR",
        adoption_iqr[1], "-", adoption_iqr[2],
        "| 16 states first adopt in 1900 or later, 11 of them CLS-coded South."),
  paste("Validation:", sum(validation$validation_passed), "of", nrow(validation), "checks passed.")
)
writeLines(notes, file.path(OUT_DIR, "notes.txt"))
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "session_info.txt"))

# --------------------------------------------------------------------------- #
# Console
# --------------------------------------------------------------------------- #

cat("\n=== Panel A: adoption timing ===\n")
print(as.data.frame(panel_a[, c("decade_label", "states", "south", "cumulative",
                                "cumulative_pct")]), row.names = FALSE)
cat("\n=== Panel B: age limits and exemptions ===\n")
print(as.data.frame(panel_b), row.names = FALSE)
cat("\n=== Panel C: within-state changes ===\n")
print(as.data.frame(panel_c), row.names = FALSE)
if (nrow(panel_d)) {
  cat("\n=== Panel D: required years, person-weighted (CLS sample) ===\n")
  print(as.data.frame(panel_d), row.names = FALSE)
  cat("  N =", format(total_men, big.mark = ","),
      " mean age =", round(mean_age_all, 2), "(CLS 38.9)\n")
  cat("\n=== Panel D (b): mean requirement by cohort decade and region ===\n")
  print(as.data.frame(panel_d_region), row.names = FALSE)
}
cat("\n=== Panel E: how requirements were expressed ===\n")
print(as.data.frame(panel_e), row.names = FALSE)

cat("\n=== validation ===\n")
print(as.data.frame(validation), row.names = FALSE)

message("\nSaved to: ", OUT_DIR)
if (!all(validation$validation_passed)) {
  stop(sum(!validation$validation_passed), " validation check(s) FAILED.")
}
