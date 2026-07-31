###############################################################################
# Build a LaTeX longtable of the selected Andrews college site-selection
# experiments used in the paper (57 events, 1850-1920), styled after Andrews
# (2023) Appendix Table A1.
#
# Adds a "Specification" column marking whether each event enters the main-text
# fully-balanced specification (Main) or only the less-restrictive Appendix
# specifications (Appendix). "Main" reproduces the pipeline's balanced-event-time
# filter (standard-decade timing): treated county with non-missing population at
# every e in {-20,-10,...,70} and at least one runner-up surviving the same filter.
#
# Input  (Dropbox output/): andrews_event_county_units_1850_1920.csv,
#                           amws_temporal_support_county_decade_1830_1950.csv
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

balance_event_times <- seq(-20L, 70L, 10L)

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
# Load units and population panel
###############################################################################

units <- read_csv(
  output_file_path("land_grants", "andrews_event_county_units_1850_1920.csv"),
  show_col_types = FALSE
) %>%
  mutate(GEOID = str_pad(as.character(as.integer(GEOID)), 5, pad = "0"))

panel <- read_csv(
  output_file_path("land_grants", "amws_temporal_support_county_decade_1830_1950.csv"),
  show_col_types = FALSE
) %>%
  mutate(GEOID = str_pad(as.character(as.integer(GEOID)), 5, pad = "0")) %>%
  select(GEOID, decade, population)

###############################################################################
# Balanced-sample membership (per timing): a unit survives if it has non-missing
# population at every required decade g + e; an event is in the balanced sample
# if its treated unit survives and >= 1 runner-up survives.
###############################################################################

survives_unit <- function(geoid, g, timing_var) {
  need <- g + balance_event_times
  have <- panel$decade[panel$GEOID == geoid & !is.na(panel$population)]
  all(need %in% have)
}

balanced_events <- function(timing_col) {
  u <- units %>% mutate(g = .data[[timing_col]])
  u$surv <- mapply(function(geoid, g) survives_unit(geoid, g), u$GEOID, u$g)
  u %>%
    group_by(event_id) %>%
    summarise(
      treated_ok = any(surv[sample_role == "treated"]),
      ctrl_ok = any(surv[sample_role == "runner_up"]),
      .groups = "drop"
    ) %>%
    filter(treated_ok & ctrl_ok) %>%
    pull(event_id)
}

main_std <- balanced_events("g_std")
main_alt <- balanced_events("g_shift")

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
         "whether the event enters the main-text fully-balanced specification (``Main'': ",
         "non-missing county population throughout $e \\in [-20,+70]$; ", length(main_std),
         " events under the standard-decade timing, ", length(main_alt),
         " under the alternative-decade timing) or only the less-restrictive Appendix ",
         "specifications (``Appendix''). Main-specification events are a subset of the Appendix ",
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
