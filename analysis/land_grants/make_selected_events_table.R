###############################################################################
# Build a LaTeX longtable of the selected Andrews college site-selection
# experiments used in the paper (57 events, 1850-1920), styled after Andrews
# (2023) Appendix Table A1.
#
# Adds a "Specification" column marking whether each event enters the main-text
# fully-balanced specification (Main) or only the less-restrictive Appendix
# specifications (Appendix). Membership comes from the saved per-cohort audit
# of the manuscript's actual balanced sample, including baseline-birth support
# and timing-specific outcome binning. The row label uses standard timing.
#
# Input (Dropbox): event county units, saved per-cohort membership audit and
#                 pooled baseline-population model status.
# Output (Overleaf repo):   project/tables/andrews_selected_events.tex
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "prep", "raw_paths.R"))

state_abbr <- setNames(c(state.abb, "DC"), c(state.name, "District of Columbia"))

latex_escape <- function(x) {
  x %>%
    str_replace_all("\\\\", "\\\\textbackslash{}") %>%
    str_replace_all("&", "\\\\&") %>%
    str_replace_all("%", "\\\\%") %>%
    str_replace_all("#", "\\\\#") %>%
    str_replace_all("_", "\\\\_") %>%
    str_replace_all("\\$", "\\\\$")
}

###############################################################################
# Load the original experiment metadata
###############################################################################

units <- read_csv(
  output_file_path("land_grants", "andrews_event_county_units_1850_1920.csv"),
  show_col_types = FALSE
) %>%
  mutate(GEOID = str_pad(as.character(as.integer(GEOID)), 5, pad = "0"))

###############################################################################
# Read the estimator's audited balanced-sample membership for each timing.
###############################################################################

cohort_audit <- read_csv(results_file_path("land_grants", "event_studies",
  "amws_county_pairs_temporal_support_1830_1950_baseline_pop_per_cohort_max_e",
  "events_by_cohort.csv"), show_col_types = FALSE)
model_status <- read_csv(results_file_path("land_grants", "event_studies",
  "amws_county_pairs_temporal_support_1830_1950_baseline_pop_balanced_event_time_max70",
  "baseline_pop_model_status.csv"), show_col_types = FALSE)
saved_events <- function(timing_name) {
  audit <- cohort_audit %>% filter(timing == timing_name, outcome == "amws_per_1000_births")
  ids <- sort(unique(as.integer(unlist(strsplit(audit$event_ids, "|", fixed = TRUE)))))
  status <- model_status %>% filter(timing == timing_name, outcome == "amws_per_1000_births", spec == "births_level")
  stopifnot(nrow(status) == 1L, status$ok, length(ids) == status$n_events,
            sum(audit$n_treated) == length(ids), all(ids %in% units$event_id))
  ids
}
main_std <- saved_events("standard_decade")
main_alt <- saved_events("alternative_decade")

message("Balanced (Main) events -- standard: ", length(main_std),
        " | alternative: ", length(main_alt))
if (length(main_std) != 16L) {
  warning("Expected 16 Main events (standard timing); found ", length(main_std), ".")
}

###############################################################################
# Assemble one row per event
###############################################################################

treated <- units %>%
  filter(sample_role == "treated") %>%
  transmute(event_id, college, experiment_year, college_type,
            county,
            state = ifelse(is.na(state_abbr[state]), state, unname(state_abbr[state])))

runners <- units %>%
  filter(sample_role == "runner_up") %>%
  mutate(abbr = ifelse(is.na(state_abbr[state]), state, state_abbr[state]),
         label = paste0(county, " (", abbr, ")")) %>%
  arrange(event_id, county) %>%
  group_by(event_id) %>%
  summarise(runner_ups = paste(label, collapse = ", "), .groups = "drop")

tbl <- treated %>%
  left_join(runners, by = "event_id") %>%
  mutate(
    specification = ifelse(event_id %in% main_std, "Main", "Appendix"),
    runner_ups = coalesce(runner_ups, "")
  ) %>%
  arrange(specification != "Main", experiment_year, college) %>%
  mutate(across(c(college, county, state, college_type, runner_ups), latex_escape))

stopifnot(nrow(tbl) == 57L)

###############################################################################
# Emit longtable
###############################################################################

rows <- sprintf(
  "%s & %s & %s & %s & %d & %s & %s \\\\",
  tbl$college, tbl$county, tbl$state, tbl$runner_ups,
  tbl$experiment_year, tbl$college_type, tbl$specification
)
# Separate the Main-specification block (listed first) from the Appendix block.
n_main <- sum(tbl$specification == "Main")
if (n_main > 0L && n_main < nrow(tbl)) {
  rows <- c(rows[seq_len(n_main)], "\\midrule", rows[(n_main + 1L):nrow(tbl)])
}

header <- c(
  "% Auto-generated by analysis/land_grants/make_selected_events_table.R -- do not edit by hand.",
  "{\\footnotesize",
  "\\begin{longtable}{p{3.2cm} p{1.5cm} c p{4.6cm} c p{1.5cm} c}",
  "\\caption{Selected college site-selection experiments (Andrews 2023) used in the event studies}",
  "\\label{tab:andrews_selected}\\\\",
  "\\toprule",
  "College & County & State & Runner-up counties & Year & Type & Spec. \\\\",
  "\\midrule",
  "\\endfirsthead",
  "\\multicolumn{7}{l}{\\emph{Table \\ref{tab:andrews_selected} -- continued from previous page}}\\\\",
  "\\toprule",
  "College & County & State & Runner-up counties & Year & Type & Spec. \\\\",
  "\\midrule",
  "\\endhead",
  "\\midrule \\multicolumn{7}{r}{\\emph{Continued on next page}}\\\\",
  "\\endfoot",
  "\\bottomrule",
  paste0("\\multicolumn{7}{p{15cm}}{\\footnotesize \\emph{Notes:} The ", nrow(tbl),
         " quasi-random college site-selection experiments from Andrews (2023) used in the event ",
         "studies (experiment years 1850--1920). Winner (college) county and state, the runner-up ",
         "counties (with state abbreviations) that serve as never-treated controls, the experiment ",
         "year, and the college type follow Andrews (2023) Appendix Table A1. ``Spec.'' indicates ",
         "membership in the saved standard-decade fully-balanced specification (``Main'': ",
         length(main_std), " treated events) or the less-restrictive Appendix ",
         "specifications (``Appendix''). Alternative timing uses its own outcome and denominator ",
         "bins and retains ", length(main_alt), " treated events; the row labels refer to standard timing only. ",
         "Membership is read from the saved per-cohort sample audit, which includes the baseline-birth ",
         "covariate and balanced $e \\in [-20,+70]$ support. Main-specification events are a subset of the Appendix ",
         "sample and therefore also appear in the Appendix figures. Because the fully-balanced ",
         "specification requires complete NHGIS county population across the entire window ",
         "$e \\in [-20,+70]$ for the winner and at least one runner-up control, the Main sample is ",
         "restricted to experiments from 1864 onward. Events are listed by ",
         "specification (Main first) and then by experiment year; states are given as postal ",
         "abbreviations.}\\\\"),
  "\\endlastfoot"
)

out <- c(header, rows, "\\end{longtable}", "}")
out_path <- file.path(repo_root, "project", "tables", "andrews_selected_events.tex")
writeLines(out, out_path)

cat("wrote", out_path, "\n")
cat("rows:", nrow(tbl), " | Main:", sum(tbl$specification == "Main"),
    " | Appendix:", sum(tbl$specification == "Appendix"), "\n")
