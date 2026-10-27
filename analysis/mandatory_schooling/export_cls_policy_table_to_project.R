# Export the compulsory-schooling law table to the paper.
#
# Reads the validated descriptives written by
# analysis/mandatory_schooling/describe_cls_policy_environment.R and composes a
# single paper-ready float in project/tables/.
#
# Why a script rather than a hand-written .tex: CLAUDE.md records how the
# mandatory-schooling figure volume went silently stale because figures were
# hand-copied and the rule "lived nowhere in code". A hand-written table in
# project/ would repeat exactly that. The guards below are the same ones
# export_mandatory_schooling_project_artifacts.R applies to figures.
#
# Two artifacts are written from the same guarded inputs:
#   - the paper fragment, carrying Panels B and C relettered A and B (the paper
#     should not inherit the internal numbering);
#   - a standalone statistics volume carrying all five panels, compiling on its
#     own the way project/mandatory_schooling.tex does for the figures. It is
#     not \input anywhere.
#
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/export_cls_policy_table_to_project.R
#
# Inputs (results/mandatory_schooling/cls_policy_descriptives/):
#   panel_b_age_limits_over_time.csv, panel_c_within_state_changes.csv
#   notes.txt, validation_summary.csv                      (guards)
# RD figures (results/mandatory_schooling/attendance_cohort/), unbalanced version:
#   discontinuity_<m>.png, discontinuity_state_<m>.png, discontinuity_state_fc_<m>.png
#   for m in ever_attended, meets_min, years_hs, hs_complete, guarded by
#   discontinuity_summary.csv, discontinuity_state_rd.csv, discontinuity_state_fc_rd.csv
#   discontinuity_by_prelaw_years_pooled.png, guarded by discontinuity_by_prelaw_years_gaps.csv
#
# Outputs:
#   project/tables/mandatory_schooling_laws.tex         2-panel paper fragment
#   project/mandatory_schooling_laws_stats.tex          standalone volume: 5 panels
#                                                       plus the RD figure section
#   project/figures/mandatory_schooling_rd_*.png        the 13 RD figures it includes
#   project/tables/mandatory_schooling_table_manifest.csv

suppressPackageStartupMessages({
  library("readr")
  library("dplyr")
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

SCRIPT_REL  <- "analysis/mandatory_schooling/export_cls_policy_table_to_project.R"
RUN_DIR     <- file.path(data_root, "results/mandatory_schooling/cls_policy_descriptives")
TABLE_DIR   <- file.path(repo_root, "project", "tables")
TABLE_FILE  <- file.path(TABLE_DIR, "mandatory_schooling_laws.tex")
STATS_FILE  <- file.path(repo_root, "project", "mandatory_schooling_laws_stats.tex")
MANIFEST    <- file.path(TABLE_DIR, "mandatory_schooling_table_manifest.csv")

if (!dir.exists(TABLE_DIR)) stop("project/tables not found at:\n  ", TABLE_DIR)

sources <- c(
  panel_a    = file.path(RUN_DIR, "panel_a_adoption_timing.csv"),
  panel_b    = file.path(RUN_DIR, "panel_b_age_limits_over_time.csv"),
  panel_c    = file.path(RUN_DIR, "panel_c_within_state_changes.csv"),
  panel_d    = file.path(RUN_DIR, "panel_d_required_years.csv"),
  panel_d_reg = file.path(RUN_DIR, "panel_d_required_years_by_region.csv"),
  panel_e    = file.path(RUN_DIR, "panel_e_requirement_expression.csv"),
  notes      = file.path(RUN_DIR, "notes.txt"),
  validation = file.path(RUN_DIR, "validation_summary.csv")
)

# --------------------------------------------------------------------------- #
# Guards -- refuse to publish rather than publish something stale or unvalidated
# --------------------------------------------------------------------------- #

missing <- sources[!file.exists(sources)]
if (length(missing)) {
  stop("Missing source(s):\n  ", paste(missing, collapse = "\n  "),
       "\nRun analysis/mandatory_schooling/describe_cls_policy_environment.R first.")
}

# Catches exporting a superseded law vintage: the pre-Lingwall coding is how the
# figure volume silently went stale. Match the citation itself, not a sentence.
if (!any(grepl("Lingwall \\(2014\\)", readLines(sources[["notes"]], warn = FALSE)))) {
  stop("Refusing to export: ", sources[["notes"]],
       " does not record Lingwall (2014) law dates.")
}

validation <- readr::read_csv(sources[["validation"]], show_col_types = FALSE)
if (!"validation_passed" %in% names(validation) ||
    !all(validation$validation_passed %in% TRUE)) {   # %in% so NA fails
  stop("Refusing to export: validation_passed is not all TRUE in\n  ",
       sources[["validation"]])
}

# --------------------------------------------------------------------------- #
# RD figures for the volume's last section -- the unbalanced ("full") version
# only: 13 states with a first law in 1907-1920, cohorts -5..+9. Three RD
# variants x four measures. Copied into project/figures under the
# mandatory_schooling_rd_ prefix, which export_mandatory_schooling_project_
# artifacts.R never prunes (it only touches mandatory_schooling_csdid*.pdf).
# --------------------------------------------------------------------------- #

RD_DIR <- file.path(data_root, "results/mandatory_schooling/attendance_cohort")
FIGURE_DIR <- file.path(repo_root, "project", "figures")
RD_MEASURES <- c(ever_attended = "Share ever attended school",
                 meets_min = "Share completing the law's minimum years",
                 years_hs = "Years of schooling (capped at 12)",
                 hs_complete = "Share completing high school")
RD_VERSIONS <- data.frame(
  version = c("county", "state_linked", "state_fullcount"),
  source_prefix = c("discontinuity_", "discontinuity_state_", "discontinuity_state_fc_"),
  estimates = c("discontinuity_summary.csv", "discontinuity_state_rd.csv", "discontinuity_state_fc_rd.csv"),
  stringsAsFactors = FALSE)
rd_spec <- merge(RD_VERSIONS, data.frame(measure = names(RD_MEASURES), stringsAsFactors = FALSE))
rd_spec$source <- file.path(RD_DIR, paste0(rd_spec$source_prefix, rd_spec$measure, ".png"))
rd_spec$destination <- sprintf("mandatory_schooling_rd_%s_%s.png", rd_spec$version, rd_spec$measure)
rd_spec <- rd_spec[order(match(rd_spec$version, RD_VERSIONS$version),
                         match(rd_spec$measure, names(RD_MEASURES))), ]

if (!dir.exists(FIGURE_DIR)) stop("project/figures not found at:\n  ", FIGURE_DIR)
missing_rd <- rd_spec$source[!file.exists(rd_spec$source)]
if (length(missing_rd)) {
  stop("Missing RD figure(s):\n  ", paste(missing_rd, collapse = "\n  "),
       "\nRun plot_cohort_discontinuity.R and plot_cohort_discontinuity_state.R first.")
}
# Each figure's estimates must exist and cover all four measures, or the figures
# are not the current four-measure run.
for (f in unique(rd_spec$estimates)) {
  path <- file.path(RD_DIR, f)
  if (!file.exists(path)) stop("Missing RD estimates: ", path)
  est_measures <- unique(readr::read_csv(path, show_col_types = FALSE)$measure)
  if (!all(names(RD_MEASURES) %in% est_measures)) {
    stop("Refusing to export: ", path, " lacks measure(s) ",
         paste(setdiff(names(RD_MEASURES), est_measures), collapse = ", "))
  }
}

# Heterogeneity figure: the cross-fitted RD by pooled quartiles of pre-law years
# minus the law minimum (plot_discontinuity_by_prelaw_years.R). Same prefix, so it
# must be in a spec to survive the pruning below.
het_spec <- data.frame(
  source = file.path(RD_DIR, "discontinuity_by_prelaw_years_pooled.png"),
  destination = "mandatory_schooling_rd_by_prelaw_years_pooled.png",
  estimates = "discontinuity_by_prelaw_years_gaps.csv",
  stringsAsFactors = FALSE)
if (!file.exists(het_spec$source)) {
  stop("Missing heterogeneity figure:\n  ", het_spec$source,
       "\nRun plot_discontinuity_by_prelaw_years.R first.")
}
het_est_path <- file.path(RD_DIR, het_spec$estimates)
if (!file.exists(het_est_path)) stop("Missing heterogeneity estimates: ", het_est_path)
het_est <- readr::read_csv(het_est_path, show_col_types = FALSE)
het_measures <- unique(het_est$measure[het_est$split == "pooled"])
if (!all(names(RD_MEASURES) %in% het_measures)) {
  stop("Refusing to export: ", het_est_path, " lacks pooled-split measure(s) ",
       paste(setdiff(names(RD_MEASURES), het_measures), collapse = ", "))
}

panel_a     <- readr::read_csv(sources[["panel_a"]], show_col_types = FALSE)
panel_b     <- readr::read_csv(sources[["panel_b"]], show_col_types = FALSE)
panel_c     <- readr::read_csv(sources[["panel_c"]], show_col_types = FALSE)
panel_d     <- readr::read_csv(sources[["panel_d"]], show_col_types = FALSE)
panel_d_reg <- readr::read_csv(sources[["panel_d_reg"]], show_col_types = FALSE)
panel_e     <- readr::read_csv(sources[["panel_e"]], show_col_types = FALSE)

# The specific numbers this table publishes. If the upstream run changes, fail
# rather than quietly ship different figures into the paper.
stopifnot(
  nrow(panel_b) == 6L,
  panel_b$states_with_law[panel_b$year == 1880L] == 16L,
  panel_b$states_with_law[panel_b$year == 1930L] == 49L,
  isTRUE(all.equal(panel_b$mean_exit_age[panel_b$year == 1930L], 16.20)),
  isTRUE(all.equal(panel_b$mean_effective_leaving_age[panel_b$year == 1930L], 14.90)),
  isTRUE(all.equal(panel_b$mean_window_effective[panel_b$year == 1930L], 7.59)),
  nrow(panel_c) == 2L,
  panel_c$net_raised[panel_c$limit == "Entry age"] == 0L,
  panel_c$net_raised[panel_c$limit == "Exit age"] == 38L,
  sum(panel_a$states) == 49L,
  # Panel D is the end-to-end guard: person-weighting the requirement over the
  # CLS sample must still reproduce their published Table 1.
  nrow(panel_d) == 6L,
  all(abs(panel_d$diff_vs_cls) <= 0.1),
  nrow(panel_e) > 0L
)

# --------------------------------------------------------------------------- #
# LaTeX
# --------------------------------------------------------------------------- #

fmt     <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")
fmt_int <- function(x) formatC(as.integer(x), format = "d", big.mark = ",")

rows_a <- sprintf("%d & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                  panel_b$year,
                  fmt_int(panel_b$states_with_law),
                  fmt(panel_b$mean_entry_age),
                  fmt(panel_b$mean_exit_age),
                  fmt(panel_b$mean_effective_leaving_age),
                  fmt(panel_b$pct_exemption_binds, 1),
                  fmt(panel_b$mean_window_nominal),
                  fmt(panel_b$mean_window_effective),
                  fmt_int(panel_b$states_with_continuation))

# Exit age first: it is the margin that moved.
panel_c <- panel_c[order(match(panel_c$limit, c("Exit age", "Entry age"))), ]
rows_b <- sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
                  panel_c$limit,
                  fmt_int(panel_c$never_changed),
                  fmt_int(panel_c$changed_once),
                  fmt_int(panel_c$changed_2plus),
                  fmt_int(panel_c$net_raised),
                  fmt_int(panel_c$net_unchanged),
                  fmt_int(panel_c$net_lowered))

notes <- paste0(
  "Compulsory attendance, child-labour and continuation-school laws for the 49 states ",
  "coded by Clay, Lingwall and Stephens (2021); adoption dates from Lingwall (2014, ",
  "Table 6.2). Both panels are restricted to state-years with a law in force, so the ",
  "Arizona (1883--1898) and Michigan repeal gaps do not register as changes in the age ",
  "limits. The \\emph{nominal} exit age is the age at which all children could stop ",
  "attending. The \\emph{effective} leaving age is the earliest age at which any ",
  "statutory route out opened: the child-labour work permit, available at ",
  "$\\max(\\textit{workage},\\, \\textit{entryage} + \\textit{workyrs})$, or the ",
  "early-dropout exemption, available at $\\max(\\textit{earlyyrs\\_condition},\\, ",
  "\\textit{entryage} + \\textit{earlyyrs})$, whichever came first, capped at the ",
  "nominal exit age. Both routes therefore require the child to be old enough and to ",
  "have completed the required schooling. Accumulated schooling at a given age is taken ",
  "as age minus entry age, i.e. continuous attendance from the entry age, following the ",
  "construction of the required-years measures in the source. `Exemption binds' is the ",
  "share of states with a law whose effective leaving age fell below the nominal exit ",
  "age. The required window is the leaving age less the entry age, on each definition."
)

tex <- c(
  paste0("% Generated by ", SCRIPT_REL),
  paste0("% ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "% Include from project/main.tex with: \\input{tables/mandatory_schooling_laws}",
  "% Do not edit by hand.",
  "",
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{State compulsory attendance laws, 1880--1930}",
  "\\label{tab:mandatory_schooling_laws}",
  "\\footnotesize",
  "\\setlength{\\tabcolsep}{4pt}",
  "",
  "\\textit{Panel A. Age limits, exemptions and continuation schooling}",
  "",
  "\\begin{tabular}{lrrrrrrrr}",
  "\\toprule",
  paste("Year & States & Entry & Exit & Effective & Exemption & Window &",
        "Window & Continuation \\\\"),
  paste("& with law & age & age & leaving age & binds \\% & (nominal) &",
        "(effective) & laws \\\\"),
  "\\midrule",
  rows_a,
  "\\bottomrule",
  "\\end{tabular}",
  "",
  # Panel separator. Deliberately not addlinespace, which is noalign-based
  # and only legal inside a tabular, not between two of them.
  "\\vspace{1.25em}",
  "",
  "\\textit{Panel B. Within-state changes while a law was in force}",
  "",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "Limit & Never & Changed & Changed & Net & Net & Net \\\\",
  "& changed & once & 2+ times & raised & unchanged & lowered \\\\",
  "\\midrule",
  rows_b,
  "\\bottomrule",
  "\\end{tabular}",
  "",
  paste0("\\caption*{\\footnotesize \\textit{Notes:} ", notes, "}"),
  "\\end{table}"
)

writeLines(tex, TABLE_FILE, useBytes = TRUE)

# --------------------------------------------------------------------------- #
# Standalone statistics volume -- all five panels, compiles on its own
# --------------------------------------------------------------------------- #

# Own preamble. project/mandatory_schooling.tex is the precedent for a
# self-contained volume, but it carries none of the packages a booktabs table
# needs and sits at 8mm margins, so this one is built from scratch at 2cm.
stats_preamble <- c(
  paste0("% Generated by ", SCRIPT_REL),
  paste0("% ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "% Standalone volume: compile directly, it is not \\input anywhere.",
  "%   pdflatex mandatory_schooling_laws_stats.tex   (run it twice: cross-references)",
  "% Do not edit by hand.",
  "",
  "\\documentclass[11pt]{article}",
  "\\usepackage[a4paper,margin=2cm]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{array}",
  "\\usepackage{caption}",
  "\\usepackage{float}",
  "\\usepackage{graphicx}",
  "\\setlength{\\parindent}{0pt}",
  "",
  "\\begin{document}",
  "",
  "\\begin{center}",
  "  {\\LARGE\\bfseries State Compulsory Attendance Laws, 1880--1930}\\\\[0.4em]",
  "  {\\large Descriptive statistics}\\\\[0.8em]",
  paste0("  {\\small Generated ", format(Sys.Date(), "%Y-%m-%d"), "}"),
  "\\end{center}",
  "",
  "\\vspace{0.5em}",
  "",
  "Law coding from Clay, Lingwall and Stephens (2021), \\textit{Labour Economics} 68,",
  "101935; adoption dates from Lingwall (2014, Table 6.2); person weights from the",
  "IPUMS 1940 complete count.",
  "",
  "\\vspace{0.5em}",
  "",
  "\\textbf{Three reading rules.} In the source coding a \\textbf{zero means no law},",
  "not a missing value, so every panel below is restricted to state-years with a law",
  "in force. The schooling-requirement variables carry \\textbf{sentinel codes}: 99",
  "where the statute required literacy rather than a year count, and 99$X$ where it",
  "required completion of a primary course of length $X$; the source recodes literacy",
  "to four years, which is an assumption rather than a measurement, and Table",
  "\\ref{tab:stats_expression} shows how much rests on it. Finally, \\textbf{Arizona}",
  "had no law between 1883 and 1898 and \\textbf{Michigan} has a one-year gap, so a",
  "monotone `treated from year $g$' coding would misstate Arizona by 19 years.",
  "",
  "\\vspace{1em}"
)

stats_table <- function(caption, label, colspec, header, rows, notes,
                        tabcolsep = "4pt", size = "\\footnotesize",
                        second = NULL) {
  c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    size,
    paste0("\\setlength{\\tabcolsep}{", tabcolsep, "}"),
    paste0("\\begin{tabular}{", colspec, "}"),
    "\\toprule",
    header,
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    if (!is.null(second)) c("", "\\vspace{1.25em}", "", second) else NULL,
    "",
    paste0("\\caption*{\\footnotesize \\textit{Notes:} ", notes, "}"),
    "\\end{table}",
    ""
  )
}

# ---- Table 1: adoption timing ---------------------------------------------
# paste0(decade, "s") rather than the decade_label column: readr guesses that
# column as a number and silently strips the trailing "s".
rows_t1 <- sprintf("%s & %s & %s & %s & %s \\\\",
                   paste0(as.character(panel_a$decade), "s"), fmt_int(panel_a$states),
                   fmt_int(panel_a$south), fmt_int(panel_a$cumulative),
                   fmt(panel_a$cumulative_pct, 1))
tab1 <- stats_table(
  "Adoption of the first compulsory attendance law",
  "tab:stats_adoption", "lrrrr",
  "Decade & States & Of which South & Cumulative & Cumulative \\% \\\\",
  rows_t1,
  paste0("Year in which each of the 49 jurisdictions first enacted a compulsory ",
         "attendance law, from Lingwall (2014, Table 6.2). Median 1889, ",
         "interquartile range 1875--1907. Adoption is bimodal: 23 states (47\\%) ",
         "adopt in the 1870s and 1880s, then 16 states first adopt in 1900 or ",
         "later, \\textbf{11 of which are the entire CLS-coded South}, clustered ",
         "in 1913--1920. `South' is the source's region 3 and comprises 11 states; ",
         "it places MD and DC in the Northeast, KY and WV in the Midwest and OK ",
         "in the West."),
  size = "\\small")

# ---- Table 2: age limits ---------------------------------------------------
rows_t2 <- sprintf("%d & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                   panel_b$year, fmt_int(panel_b$states_with_law),
                   fmt(panel_b$mean_entry_age), fmt(panel_b$mean_exit_age),
                   fmt(panel_b$mean_effective_leaving_age),
                   fmt(panel_b$pct_exemption_binds, 1),
                   fmt(panel_b$mean_window_nominal),
                   fmt(panel_b$mean_window_effective),
                   fmt_int(panel_b$states_with_continuation))
tab2 <- stats_table(
  "Age limits, exemptions and continuation schooling, 1880--1930",
  "tab:stats_agelimits", "lrrrrrrrr",
  paste("Year & States & Entry & Exit & Effective & Exemption & Window &",
        "Window & Continuation \\\\ & with law & age & age & leaving age &",
        "binds \\% & (nominal) & (effective) & laws \\\\"),
  rows_t2,
  paste0("Means across states with a law in force. The \\textit{nominal} exit age ",
         "is the age at which all children could stop attending. The ",
         "\\textit{effective} leaving age is the earliest age at which any ",
         "statutory route out opened: the child-labour work permit, at ",
         "$\\max(\\textit{workage},\\, \\textit{entryage}+\\textit{workyrs})$, or ",
         "the early-dropout exemption, at ",
         "$\\max(\\textit{earlyyrs\\_condition},\\, \\textit{entryage}+",
         "\\textit{earlyyrs})$, whichever came first, capped at the nominal exit ",
         "age. Both routes require the child to be old enough \\textit{and} to ",
         "have completed the required schooling; accumulated schooling at a given ",
         "age is taken as age minus entry age, i.e.\\ continuous attendance from ",
         "the entry age, following the construction of the required-years measures ",
         "in the source. `Exemption binds' is the share of law-states whose ",
         "effective leaving age fell below the nominal one. The nominal required ",
         "window widened by 2.59 years over the period; the effective window by ",
         "\\textbf{1.36}."))

# ---- Table 3: within-state changes ----------------------------------------
panel_c <- panel_c[order(match(panel_c$limit, c("Exit age", "Entry age"))), ]
rows_t3 <- sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
                   panel_c$limit, fmt_int(panel_c$never_changed),
                   fmt_int(panel_c$changed_once), fmt_int(panel_c$changed_2plus),
                   fmt_int(panel_c$net_raised), fmt_int(panel_c$net_unchanged),
                   fmt_int(panel_c$net_lowered))
tab3 <- stats_table(
  "Within-state changes while a law was in force, 1880--1930",
  "tab:stats_changes", "lrrrrrr",
  paste("Limit & Never & Changed & Changed & Net & Net & Net \\\\",
        "& changed & once & 2+ times & raised & unchanged & lowered \\\\"),
  rows_t3,
  paste0("Spells are counted only over years in which a law was in force, so the ",
         "Arizona and Michigan repeal gaps do not register as changes in the age ",
         "limits. The two margins behave entirely differently: \\textbf{no state ",
         "ever raised its entry age} and 24 lowered it, while 38 raised the exit ",
         "age and only one (Washington) lowered it. There were 59 separate upward ",
         "changes to the exit age, with a median year of 1911 --- a generation ",
         "after the median adoption year of 1889."),
  size = "\\small")

# ---- Table 4: required years (two tabulars in one float) -------------------
rows_t4 <- sprintf("%s & %s & %s & %s & %s \\\\",
                   panel_d$required_years, fmt_int(panel_d$men),
                   fmt(panel_d$pct_of_men, 1), fmt(panel_d$cls_table1_pct, 1),
                   fmt(panel_d$mean_yrs_sch))
reg <- panel_d_reg[order(panel_d_reg$cohort_decade, panel_d_reg$region_label), ]
rows_t4b <- sprintf("%d & %s & %s & %s & %s & %s \\\\",
                    reg$cohort_decade, reg$region_label,
                    fmt(reg$ca_years1), fmt(reg$ca_years),
                    fmt(reg$ca_years_combined), fmt_int(reg$men))
second_t4 <- c(
  "\\begin{tabular}{llrrrr}",
  "\\toprule",
  paste("Cohort & Region & Age limits & Binding & Incl.\\ continuation & Men \\\\",
        "decade & & only & requirement & schooling & \\\\"),
  "\\midrule",
  rows_t4b,
  "\\bottomrule",
  "\\end{tabular}")
tab4 <- stats_table(
  "Required years of attendance facing the 1885--1912 birth cohorts",
  "tab:stats_required", "lrrrr",
  "Required years & Men & \\% of men & Source Table 1 \\% & Mean years of schooling \\\\",
  rows_t4,
  paste0("Required years of attendance by state of birth and birth cohort, weighted ",
         "by people. Sample follows the source exactly: native-born white men aged ",
         "27--54 in the 1940 complete count, with year of birth computed as ",
         "$1940-\\textit{age}-1$ because enumeration was on 1 April and most had ",
         "not yet had their birthday. Years of schooling is constructed from ",
         "\\textit{higraded}; the census recorded highest grade completed, not ",
         "years. The `Source Table 1' column is the published distribution in Clay, ",
         "Lingwall and Stephens (2021, Table 1, column 1); \\textbf{every bin ",
         "matches to within 0.1 percentage points}, which makes this panel a check ",
         "on the whole chain from the raw census file through to the law merge. The ",
         "lower block reports the mean requirement under three definitions: age ",
         "limits alone, the binding requirement after exemptions, and the binding ",
         "requirement including continuation schooling."),
  second = second_t4)

# ---- Table 5: how the requirement was expressed ---------------------------
rows_t5 <- sprintf("%s & %s & %s & %s \\\\",
                   panel_e$provision, gsub("_", " ", panel_e$code_type),
                   fmt_int(panel_e$state_years),
                   fmt(panel_e$pct_of_law_state_years, 1))
tab5 <- stats_table(
  "How the schooling requirement was expressed",
  "tab:stats_expression", "llrr",
  "Provision & Coded as & State-years & \\% of law state-years \\\\",
  rows_t5,
  paste0("Counts of state-years with a law in force, by how each exemption's ",
         "schooling requirement was written. \\textit{earlyyrs} is the schooling ",
         "that released a child from the exit age, \\textit{workyrs} the schooling ",
         "required for the child-labour exemption, and \\textit{contyrs} the ",
         "schooling that released a working child from continuation school. ",
         "`None' means the provision was absent. This panel is where the literacy ",
         "recode becomes a magnitude: the source substitutes four years wherever a ",
         "statute demanded literacy rather than a year count, which affects ",
         "\\textbf{9.2\\% of \\textit{workyrs} state-years} and, because those ",
         "years now feed the effective leaving age in Table ",
         "\\ref{tab:stats_agelimits}, propagates into an age."),
  size = "\\small")

# --------------------------------------------------------------------------- #
# RD section. The text states the design only; the estimates live in the figure
# subtitles, so nothing here can go stale when the RD runs are refreshed.
# --------------------------------------------------------------------------- #

rd_notes <- c(
  county = paste0(
    "IPUMS MLP linked sample. Each man is placed in the county of his earliest linked record taken at ",
    "age 18 or younger; non-migrant means born, placed as a child, and living in 1935 and 1940 in the same ",
    "state. Units are county $\\times$ birth-cohort cells with at least 25 men. Panel A shows the cells as they ",
    "are; panel B removes county fixed effects (each cell minus its county's mean over all its cohorts in the ",
    "window). Standard errors are clustered by county."),
  state_linked = paste0(
    "The same linked sample and non-migrant rule, aggregated to state $\\times$ birth-cohort cells (every man, ",
    "no minimum cell size). Panel A shows one point per state; panel B removes state fixed effects. Pooled ",
    "estimates carry heteroskedasticity-robust standard errors, with state-clustered ones (13 clusters) as a check."),
  state_fullcount = paste0(
    "The IPUMS 1940 complete count: every white US-born man, with no linkage and hence no linkage selection, ",
    "but no childhood record either. Non-migrant means living in the birth state in 1940 and in the same state ",
    "in 1935 (\\textit{MIGRATE5}); the state is the birth state. Otherwise as the linked state-level version."))
rd_titles <- c(county = "County-level RD, linked sample",
               state_linked = "State-level RD, linked sample",
               state_fullcount = "State-level RD, 1940 complete count")

rd_section <- c(
  "\\clearpage",
  "\\section*{Regression discontinuity in birth cohort around the first law}",
  "",
  paste0("The 13 states whose first durable compulsory attendance law took effect in 1907--1920; ",
         "white US-born non-migrant men born 1890--1920, with attainment from the 1940 census. The running ",
         "variable is the birth cohort relative to the state's cutoff, $T - \\textit{exitage}$: $-1$ is the ",
         "last cohort too old to be covered and $0$ the first covered. Cohort 0 was exactly at the leaving ",
         "age when the law passed and owed no schooling, so an effect should appear as a change in slope ",
         "(the kink) as coverage builds over the following cohorts, more than as a jump at the cutoff. ",
         "Unbalanced window: event times $-5$ to $+9$ over all 13 states, so Delaware and Oklahoma enter ",
         "only from $-3$ and $-2$. Local linear fits either side of the cutoff with a triangular kernel ",
         "and bandwidth 5; jump and kink from \\textit{rdrobust}, conventional standard errors. The four ",
         "measures are ever attended school, completing the law's minimum years, years of schooling capped ",
         "at 12, and completing high school, which no law required and so serves as a placebo."),
  "")
for (v in RD_VERSIONS$version) {
  rd_section <- c(rd_section,
    paste0("\\subsection*{", rd_titles[[v]], "}"), "", rd_notes[[v]], "")
  for (i in which(rd_spec$version == v)) {
    m <- rd_spec$measure[i]
    rd_section <- c(rd_section,
      "\\begin{figure}[H]", "\\centering",
      paste0("\\includegraphics[width=\\textwidth]{figures/", rd_spec$destination[i], "}"),
      paste0("\\caption{", rd_titles[[v]], ": ", tolower(substr(RD_MEASURES[[m]], 1, 1)),
             substring(RD_MEASURES[[m]], 2), "}"),
      paste0("\\label{fig:rd_", v, "_", m, "}"),
      "\\end{figure}", "")
  }
}

rd_section <- c(rd_section,
  "\\subsection*{Heterogeneity by pre-law years of schooling (pooled quartiles, cross-fitted)}", "",
  paste0("Counties are grouped into quartiles, pooled across the 13 states, of their pre-law average years ",
         "of schooling (capped at 12, cohorts $-5$ to $-1$) minus the years their state's law required, so ",
         "counties are compared on their distance to their own legal floor. The linked sample, county ",
         "$\\times$ birth-cohort cells net of county fixed effects, and the same local linear fits and ",
         "\\textit{rdrobust} settings as above. Because the pre-law average is built from the same cohorts ",
         "that form the left side of the discontinuity, a county that is low by chance would appear to ",
         "catch up by mean reversion alone; each county is therefore classified on one random half of its ",
         "men and its discontinuity read on the other half, then the halves are swapped and stacked. ",
         "Completing high school, which no law required, is the placebo: a catch-up by the least-schooled ",
         "counties in the law's minimum but not in high school would point to the law, a catch-up in ",
         "everything to general convergence."),
  "",
  "\\begin{figure}[H]", "\\centering",
  paste0("\\includegraphics[width=\\textwidth]{figures/", het_spec$destination, "}"),
  "\\caption{RD by pooled quartile of pre-law years of schooling minus the law's minimum, cross-fitted}",
  "\\label{fig:rd_by_prelaw_years_pooled}",
  "\\end{figure}", "")

# Copy the figures in, then prune rd figures the specs no longer emit.
all_fig <- rbind(rd_spec[, c("source", "destination")], het_spec[, c("source", "destination")])
copied <- file.copy(all_fig$source, file.path(FIGURE_DIR, all_fig$destination), overwrite = TRUE)
if (!all(copied)) stop("Failed to copy:\n  ", paste(all_fig$destination[!copied], collapse = "\n  "))
stale_rd <- setdiff(list.files(FIGURE_DIR, pattern = "^mandatory_schooling_rd_.*[.]png$"), all_fig$destination)
if (length(stale_rd)) {
  unlink(file.path(FIGURE_DIR, stale_rd))
  message("Removed ", length(stale_rd), " superseded RD figure(s): ", paste(stale_rd, collapse = ", "))
}

stats_tex <- c(stats_preamble, "", tab1, tab2, tab3, tab4, tab5, rd_section, "\\end{document}")
writeLines(stats_tex, STATS_FILE, useBytes = TRUE)

# --------------------------------------------------------------------------- #
# Manifest -- so a stale table is detectable the way a stale figure already is
# --------------------------------------------------------------------------- #

dest_sources <- list(
  c("panel_b", "panel_c"),                                        # paper fragment
  c("panel_a", "panel_b", "panel_c", "panel_d", "panel_d_reg", "panel_e")
)
dests <- c(basename(TABLE_FILE), basename(STATS_FILE))
src  <- unname(sources[unlist(dest_sources)])
info <- file.info(src)
manifest <- data.frame(
  destination     = rep(dests, lengths(dest_sources)),
  source          = sub(paste0("^", data_root, "/"), "", src),
  source_md5      = unname(tools::md5sum(src)),
  source_modified = format(info$mtime, "%Y-%m-%d %H:%M:%S"),
  exported        = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  stringsAsFactors = FALSE
)
rd_info <- file.info(all_fig$source)
manifest <- rbind(manifest, data.frame(
  destination     = file.path("figures", all_fig$destination),
  source          = sub(paste0("^", data_root, "/"), "", all_fig$source),
  source_md5      = unname(tools::md5sum(all_fig$source)),
  source_modified = format(rd_info$mtime, "%Y-%m-%d %H:%M:%S"),
  exported        = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  stringsAsFactors = FALSE))
readr::write_csv(manifest, MANIFEST)

message("Wrote: ", TABLE_FILE)
message("Wrote: ", STATS_FILE)
message("Wrote: ", MANIFEST)
message("\nproject/main.tex is NOT modified. Add the table where it belongs with:")
message("  \\input{tables/mandatory_schooling_laws}")
