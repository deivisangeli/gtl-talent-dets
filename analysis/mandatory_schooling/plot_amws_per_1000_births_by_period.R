###############################################################################
# County maps of AMWS scientists born per 1,000 estimated births.
#
# Inclusive birth-year windows: 1800-1850 and 1920-1950.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(sf)
  library(tigris)
})
options(tigris_use_cache = TRUE)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[1L]), winslash = "/", mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/", mustWork = TRUE
  )
} else {
  candidate <- Sys.getenv("GTL_REPO", unset = getwd())
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (parent == candidate) stop("Cannot resolve repository root; set GTL_REPO.")
    candidate <- parent
  }
  repo_root <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "paths.R"))
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  local_data_root <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (!dir.exists(local_data_root)) {
    stop("Data root does not exist; set TALENT_DETS_DATA_DIR.")
  }
  TALENT_DETS_DATA_DIR <- normalizePath(
    local_data_root, winslash = "/", mustWork = TRUE
  )
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
}

source(file.path(
  repo_root, "analysis", "mandatory_schooling", "amws_county_map_helpers.R"
))

panel_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results", "mandatory_schooling", "amws_county_birth_maps"
)
output_stem <- file.path(
  results_dir,
  "amws_per_1000_births_by_county_1800_1850_1920_1950"
)
if (!file.exists(panel_file)) stop("Missing annual AMWS panel: ", panel_file)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

periods <- data.table(
  period = c("1800-1850", "1920-1950"),
  start_year = c(1800L, 1920L),
  end_year = c(1850L, 1950L),
  expected_amws = c(600, 59950),
  expected_estimated_births = c(25021765, 95859836),
  expected_counties = c(1534L, 3088L),
  expected_positive_counties = c(227L, 2720L)
)

panel <- amws_map_read_panel(panel_file)
period_rates <- amws_map_build_period_rates(panel, periods)
geometry <- amws_map_load_geometry()
figure_result <- amws_map_build_figure(
  period_rates = period_rates,
  periods = periods,
  geometry = geometry,
  title = "AMWS scientists born per 1,000 estimated births",
  subtitle = paste(
    "County rates for selected birth cohorts | Contiguous United States",
    "| Grey indicates unavailable denominator data"
  )
)
outputs <- amws_map_save_figure(figure_result$plot, output_stem)

cat("\nAMWS county-map validation summary:\n")
print(figure_result$summary)
cat("\nWrote:\n", paste(outputs, collapse = "\n"), "\n", sep = "")
