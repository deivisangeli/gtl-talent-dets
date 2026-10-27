###############################################################################
# County maps of AMWS scientists born per 1,000 estimated births for counties
# in states whose first compulsory-schooling law took effect after 1900.
#
# Inclusive birth-year windows: 1890-1900 and 1920-1930.
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
  DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
}

source(file.path(
  repo_root, "analysis", "mandatory_schooling", "annual_csdid_helpers.R"
))
source(file.path(
  repo_root, "analysis", "mandatory_schooling", "amws_county_map_helpers.R"
))

panel_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
laws_file <- file.path(DATA_INPUT, "compulsory_schooling_laws.csv")
results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results", "mandatory_schooling", "amws_county_birth_maps"
)
output_stem <- file.path(
  results_dir,
  "amws_per_1000_births_treated_after_1900_1890_1900_1920_1930"
)
if (!file.exists(panel_file)) stop("Missing annual AMWS panel: ", panel_file)
if (!file.exists(laws_file)) stop("Missing compulsory-schooling laws: ", laws_file)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

laws <- csdid_read_laws(laws_file)
late_treated <- laws[
  is.finite(compulsory_law_year) & compulsory_law_year > 1900L
]
expected_states <- c(
  "IA", "MD", "MO", "DE", "OK", "NC", "TN", "LA",
  "TX", "AL", "AR", "GA", "VA", "FL", "SC", "MS"
)
if (nrow(late_treated) != 16L ||
    !setequal(late_treated$state_abbr, expected_states)) {
  stop("The post-1900 treatment-state set differs from the validated 16 states.")
}
eligible_state_fips <- late_treated$state_fips

periods <- data.table(
  period = c("1890-1900", "1920-1930"),
  start_year = c(1890L, 1920L),
  end_year = c(1900L, 1930L),
  expected_amws = c(1947, 4921),
  expected_estimated_births = c(8652276, 9285555),
  expected_counties = c(1281L, 1281L),
  expected_positive_counties = c(612L, 898L)
)

panel <- amws_map_read_panel(panel_file)
first_period_rates <- amws_map_build_period_rates(
  panel, periods[1L], eligible_state_fips = eligible_state_fips
)
common_geoids <- first_period_rates$GEOID
if (data.table::uniqueN(common_geoids) != 1281L) {
  stop("The 1890-1900 denominator sample differs from 1,281 counties.")
}

eligible_panel <- data.table::copy(panel)
eligible_panel[, state_fips := substr(GEOID, 1L, 2L)]
second_period_geoids <- eligible_panel[
  state_fips %in% eligible_state_fips & year >= 1920L & year <= 1930L,
  .(estimated_births = sum(county_births_estimate_year)),
  by = GEOID
][is.finite(estimated_births) & estimated_births > 0, GEOID]
if (length(setdiff(common_geoids, second_period_geoids)) != 0L) {
  stop("A first-period denominator county lacks a 1920-1930 denominator.")
}
if (length(setdiff(second_period_geoids, common_geoids)) != 162L) {
  stop("Expected 162 second-period-only denominator counties.")
}

common_panel <- panel[GEOID %in% common_geoids]
period_rates <- amws_map_build_period_rates(
  common_panel, periods, eligible_state_fips = eligible_state_fips
)
if (any(!period_rates$state_fips %in% eligible_state_fips)) {
  stop("The map contains counties outside the post-1900 treatment states.")
}
common_support <- period_rates[, .(periods_observed = uniqueN(period)), by = GEOID]
if (nrow(common_support) != 1281L ||
    any(common_support$periods_observed != 2L)) {
  stop("The two panels do not use the same 1,281 county GEOIDs.")
}

geometry <- amws_map_load_geometry()
figure_result <- amws_map_build_figure(
  period_rates = period_rates,
  periods = periods,
  geometry = geometry,
  title = paste(
    "AMWS scientists born per 1,000 estimated births",
    "- states treated after 1900"
  ),
  subtitle = paste0(
    "Fixed common sample: 1,281 counties with an 1890-1900 denominator\n",
    "Light grey: excluded states; dark grey: eligible counties outside the common sample"
  ),
  eligible_state_fips = eligible_state_fips
)
outputs <- amws_map_save_figure(figure_result$plot, output_stem)

cat("\nPost-1900 treatment states:\n")
print(late_treated[
  order(compulsory_law_year, state_abbr),
  .(state_abbr, state, compulsory_law_year)
])
cat("\nAMWS late-treated county-map validation summary:\n")
print(figure_result$summary)
cat("\nWrote:\n", paste(outputs, collapse = "\n"), "\n", sep = "")
