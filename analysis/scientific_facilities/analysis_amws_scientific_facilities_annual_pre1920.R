# Annual birth-normalized outcome: pre-1920 targets, fixed-cohort -10:+30
# balanced curves and -10:+73 supported unbalanced curves, both controls.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) normalizePath(sub("^--file=", "", file_arg[1]),
  winslash = "/", mustWork = TRUE) else NULL
repo_root <- if (!is.null(script_path)) normalizePath(file.path(dirname(script_path), "..", ".."),
  winslash = "/", mustWork = TRUE) else normalizePath(Sys.getenv("GTL_REPO", getwd()), winslash = "/")
source(file.path(repo_root, "analysis/scientific_facilities/analysis_amws_scientific_facilities_annual.R"))

settings <- sfa_settings()
settings$target_cutoff <- 1919L
settings$balanced_post <- 30L
sfa_main(settings, outcomes = "amws_per_1000_births")
