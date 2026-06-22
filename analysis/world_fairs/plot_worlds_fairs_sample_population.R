###############################################################################
# Plot UK vs US population evolution for the baseline world's fairs event-study
# sample.
#
# Sample:
#   - Union of baseline treated units across venue-distance bins.
#   - Baseline never-treated controls.
#
# Population:
#   - Annual panel is collapsed to unit-decades using the same mean population
#     rule used in the event-study scripts.
#   - Country totals sum nonmissing unit-decade population. Unit counts with and
#     without population are exported alongside the totals.
#
# Run from the repository root:
#   Rscript analysis/world_fairs/plot_worlds_fairs_sample_population.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
  if (basename(repo_root) == "world_fairs" &&
      basename(dirname(repo_root)) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, "..", ".."), winslash = "/", mustWork = TRUE)
  }
  if (basename(repo_root) == "analysis") {
    repo_root <- normalizePath(file.path(repo_root, ".."), winslash = "/", mustWork = TRUE)
  }
}

source(file.path(repo_root, "paths.R"))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(user_data_dir, winslash = "/", mustWork = TRUE)
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

###############################################################################
# Paths and helpers
###############################################################################

data_processed <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed")
event_results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results",
  "worlds_fair",
  "worlds_fairs_uk_us_venue_distance_event_studies_no_london_events_1840_1910"
)
plot_results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results",
  "worlds_fair",
  "worlds_fairs_sample_population"
)
dir.create(plot_results_dir, recursive = TRUE, showWarnings = FALSE)

panel_file <- file.path(
  data_processed,
  "uk_historical_urban_units_us_county_inventor_panel_1800_1960_nhgis_us.csv"
)
never_file <- file.path(event_results_dir, "never_treated_units.csv")

required_files <- c(panel_file, never_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

standard_decade <- function(year) {
  as.integer(floor(year / 10) * 10)
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

###############################################################################
# Define baseline analysis sample
###############################################################################

bin_dirs <- list.dirs(event_results_dir, recursive = FALSE, full.names = TRUE)
bin_dirs <- bin_dirs[grepl("^bin_[0-9]+_[0-9]+km$", basename(bin_dirs))]
treatment_files <- file.path(bin_dirs, "treatment_assignment.csv")
treatment_files <- treatment_files[file.exists(treatment_files)]
if (length(treatment_files) == 0L) {
  stop("No treatment_assignment.csv files found in: ", event_results_dir)
}

treated_units <- rbindlist(
  lapply(treatment_files, function(file) {
    x <- fread(file, na.strings = c("", "NA"))
    x[, .(
      unit_id = as.character(unit_id),
      iso3 = as.character(geo_country_iso3),
      sample_status = "treated_baseline_any_bin"
    )]
  }),
  use.names = TRUE,
  fill = TRUE
)

never_units <- fread(never_file, na.strings = c("", "NA"))[, .(
  unit_id = as.character(unit_id),
  iso3 = as.character(geo_country_iso3),
  sample_status = "never_treated_baseline"
)]

sample_units <- rbindlist(
  list(treated_units, never_units),
  use.names = TRUE,
  fill = TRUE
)
sample_units[, sample_status := fifelse(
  any(sample_status == "treated_baseline_any_bin"),
  "treated_baseline_any_bin",
  "never_treated_baseline"
), by = .(unit_id, iso3)]
sample_units <- unique(sample_units[, .(unit_id, iso3, sample_status)])

###############################################################################
# Build population summaries
###############################################################################

panel <- fread(
  panel_file,
  select = c("unit_id", "iso3", "year", "population"),
  na.strings = c("", "NA")
)
panel[, `:=`(
  unit_id = as.character(unit_id),
  iso3 = as.character(iso3),
  year = as.integer(year),
  population = as.numeric(population)
)]

panel_sample <- merge(
  panel,
  sample_units,
  by = c("unit_id", "iso3"),
  all.y = TRUE,
  allow.cartesian = TRUE,
  sort = FALSE
)

panel_decade <- panel_sample[
  year >= 1800L & year <= 1960L,
  .(
    population = mean_or_na(population),
    sample_status = sample_status[1L]
  ),
  by = .(unit_id, iso3, decade = standard_decade(year))
]

population_decade <- panel_decade[, .(
  total_population = sum(population, na.rm = TRUE),
  mean_population = mean(population, na.rm = TRUE),
  median_population = median(population, na.rm = TRUE),
  units_in_sample = uniqueN(unit_id),
  units_with_population = sum(!is.na(population)),
  units_missing_population = sum(is.na(population)),
  treated_units = uniqueN(unit_id[sample_status == "treated_baseline_any_bin"]),
  never_treated_units = uniqueN(unit_id[sample_status == "never_treated_baseline"])
), by = .(iso3, decade)]
population_decade[, total_population_millions := total_population / 1e6]
setorder(population_decade, iso3, decade)

population_annual <- panel_sample[, .(
  total_population = sum(population, na.rm = TRUE),
  mean_population = mean(population, na.rm = TRUE),
  median_population = median(population, na.rm = TRUE),
  units_in_sample = uniqueN(unit_id),
  units_with_population = sum(!is.na(population)),
  units_missing_population = sum(is.na(population)),
  treated_units = uniqueN(unit_id[sample_status == "treated_baseline_any_bin"]),
  never_treated_units = uniqueN(unit_id[sample_status == "never_treated_baseline"])
), by = .(iso3, year)]
population_annual[, total_population_millions := total_population / 1e6]
setorder(population_annual, iso3, year)

###############################################################################
# Plot and write outputs
###############################################################################

plot_data <- population_decade[iso3 %chin% c("GBR", "USA")]
plot_data[, country_label := fifelse(iso3 == "GBR", "UK", "US")]

population_plot <- ggplot(
  plot_data,
  aes(x = decade, y = total_population_millions, color = country_label)
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("UK" = "#2C7FB8", "US" = "#D95F0E")) +
  scale_x_continuous(breaks = seq(1800, 1960, by = 20)) +
  scale_y_continuous(labels = scales::label_number(suffix = "m")) +
  labs(
    title = "Population in the baseline world's fairs event-study sample",
    subtitle = "Union of treated units across distance bins plus baseline never-treated controls",
    x = "Decade",
    y = "Total population in sample",
    color = NULL,
    caption = "Country totals sum nonmissing unit-decade population; unit counts with missing population are reported in the CSV."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

decade_csv <- file.path(
  plot_results_dir,
  "uk_us_sample_population_total_baseline.csv"
)
annual_csv <- file.path(
  plot_results_dir,
  "uk_us_sample_population_total_baseline_annual.csv"
)
plot_png <- file.path(
  plot_results_dir,
  "uk_us_sample_population_total_baseline.png"
)

fwrite(population_decade, decade_csv)
fwrite(population_annual, annual_csv)
ggsave(plot_png, population_plot, width = 10, height = 6, dpi = 300)

cat("Saved decade population CSV: ", decade_csv, "\n", sep = "")
cat("Saved annual population CSV: ", annual_csv, "\n", sep = "")
cat("Saved population plot: ", plot_png, "\n", sep = "")
cat("\nSample units:\n")
print(sample_units[, .N, by = .(iso3, sample_status)][order(iso3, sample_status)])
cat("\nDecade population summary:\n")
print(population_decade[, .(
  first_decade = min(decade, na.rm = TRUE),
  last_decade = max(decade, na.rm = TRUE),
  units_in_sample = max(units_in_sample, na.rm = TRUE),
  max_units_missing_population = max(units_missing_population, na.rm = TRUE)
), by = iso3][order(iso3)])
