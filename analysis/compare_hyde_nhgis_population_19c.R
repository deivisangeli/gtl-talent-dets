###############################################################################
# Compare HYDE and NHGIS county population, nineteenth-century Census years
#
# Run from analysis/:
#   Rscript compare_hyde_nhgis_population_19c.R
#
# Inputs:
#   DATA_OUTPUT/county_hyde_population.csv
#   DATA_INPUT/nhgis0005_ts_nominal_county.csv
#
# Outputs:
#   results/hyde_nhgis_population_19c/
###############################################################################

rm(list = ls())

source("../paths.R")

suppressPackageStartupMessages({
  library("tidyverse")
})

initial_time <- Sys.time()

results_dir <- file.path("results", "hyde_nhgis_population_19c")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

census_years_19c <- seq(1800L, 1890L, by = 10L)

as_geoid <- function(x) {
  str_pad(as.character(x), width = 5, side = "left", pad = "0")
}

require_file <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
  path
}

stop_if_duplicate_keys <- function(data, key_vars, data_name) {
  duplicates <- data %>%
    count(across(all_of(key_vars)), name = "n") %>%
    filter(n > 1)

  if (nrow(duplicates) > 0) {
    print(head(duplicates, 20))
    stop(data_name, " has duplicate keys: ", paste(key_vars, collapse = ", "),
         call. = FALSE)
  }
}

population_summary <- function(x) {
  tibble(
    n = nrow(x),
    total_nhgis = sum(x$population_nhgis, na.rm = TRUE),
    total_hyde = sum(x$hyde_population, na.rm = TRUE),
    total_ratio_hyde_over_nhgis = total_hyde / total_nhgis,
    pearson = cor(x$population_nhgis, x$hyde_population, method = "pearson"),
    spearman = cor(x$population_nhgis, x$hyde_population, method = "spearman"),
    ratio_mean = mean(x$ratio_hyde_over_nhgis, na.rm = TRUE),
    ratio_median = median(x$ratio_hyde_over_nhgis, na.rm = TRUE),
    ratio_p10 = quantile(x$ratio_hyde_over_nhgis, 0.10, na.rm = TRUE),
    ratio_p90 = quantile(x$ratio_hyde_over_nhgis, 0.90, na.rm = TRUE),
    median_abs_log_ratio = median(x$abs_log_ratio, na.rm = TRUE),
    mean_abs_log_ratio = mean(x$abs_log_ratio, na.rm = TRUE)
  )
}

###############################################################################
# Load HYDE county population
###############################################################################

hyde_path <- require_file(file.path(DATA_OUTPUT, "county_hyde_population.csv"))

hyde_raw <- read_csv(hyde_path, show_col_types = FALSE)

if ("year" %in% names(hyde_raw) && !"decade" %in% names(hyde_raw)) {
  hyde_raw <- hyde_raw %>% rename(decade = year)
}

required_hyde_vars <- c("GEOID", "decade", "hyde_population")
missing_hyde_vars <- setdiff(required_hyde_vars, names(hyde_raw))
if (length(missing_hyde_vars) > 0) {
  stop("HYDE file is missing required variable(s): ",
       paste(missing_hyde_vars, collapse = ", "), call. = FALSE)
}

hyde <- hyde_raw %>%
  transmute(
    GEOID = as_geoid(GEOID),
    decade = as.integer(decade),
    hyde_population = as.numeric(hyde_population)
  ) %>%
  filter(decade %in% census_years_19c)

stop_if_duplicate_keys(hyde, c("GEOID", "decade"), "HYDE population")

###############################################################################
# Load NHGIS raw county population
###############################################################################

nhgis_path <- require_file(file.path(DATA_INPUT, "nhgis0005_ts_nominal_county.csv"))

nhgis_raw <- read_csv(
  nhgis_path,
  show_col_types = FALSE,
  skip = 2,
  col_names = c("GISJOIN", "YEAR", "STATE", "STATEFP", "STATENH",
                "COUNTY", "COUNTYFP", "COUNTYNH", "NAME", "A00AA"),
  col_types = cols(
    GISJOIN = col_character(),
    YEAR = col_integer(),
    STATE = col_character(),
    STATEFP = col_character(),
    STATENH = col_character(),
    COUNTY = col_character(),
    COUNTYFP = col_character(),
    COUNTYNH = col_character(),
    NAME = col_character(),
    A00AA = col_double()
  )
)

nhgis <- nhgis_raw %>%
  filter(
    str_detect(STATEFP, "^\\d{2}$"),
    str_detect(COUNTYFP, "^\\d{3}$")
  ) %>%
  transmute(
    GEOID = paste0(
      str_pad(STATEFP, width = 2, side = "left", pad = "0"),
      str_pad(COUNTYFP, width = 3, side = "left", pad = "0")
    ),
    decade = as.integer(YEAR),
    state_nhgis = STATE,
    county_nhgis = COUNTY,
    name_nhgis = NAME,
    population_nhgis = as.numeric(A00AA)
  ) %>%
  filter(decade %in% census_years_19c)

stop_if_duplicate_keys(nhgis, c("GEOID", "decade"), "NHGIS population")

###############################################################################
# Compare overlapping positive county-year observations
###############################################################################

matched <- nhgis %>%
  inner_join(hyde, by = c("GEOID", "decade")) %>%
  filter(
    !is.na(population_nhgis),
    !is.na(hyde_population),
    population_nhgis > 0,
    hyde_population > 0
  ) %>%
  mutate(
    ratio_hyde_over_nhgis = hyde_population / population_nhgis,
    log_ratio = log(ratio_hyde_over_nhgis),
    abs_log_ratio = abs(log_ratio),
    pct_difference_hyde_vs_nhgis = 100 * (ratio_hyde_over_nhgis - 1)
  ) %>%
  arrange(decade, GEOID)

stop_if_duplicate_keys(matched, c("GEOID", "decade"), "Matched comparison")

observed_years <- sort(unique(matched$decade))
if (!identical(observed_years, census_years_19c)) {
  stop("Matched comparison does not cover exactly 1800-1890. Observed years: ",
       paste(observed_years, collapse = ", "), call. = FALSE)
}

overall_summary <- population_summary(matched) %>%
  mutate(decade = "all", .before = 1)

decade_summary <- matched %>%
  group_by(decade) %>%
  group_modify(~ population_summary(.x)) %>%
  ungroup() %>%
  mutate(decade = as.character(decade)) %>%
  bind_rows(overall_summary) %>%
  arrange(decade == "all", as.integer(if_else(decade == "all", NA_character_, decade)))

low_hyde_outliers <- matched %>%
  group_by(decade) %>%
  arrange(ratio_hyde_over_nhgis, .by_group = TRUE) %>%
  mutate(rank_within_decade = row_number()) %>%
  ungroup() %>%
  arrange(ratio_hyde_over_nhgis) %>%
  mutate(rank_panel = row_number()) %>%
  filter(rank_panel <= 100L | rank_within_decade <= 10L) %>%
  select(
    rank_panel, rank_within_decade, GEOID, decade, state_nhgis,
    county_nhgis, name_nhgis, population_nhgis, hyde_population,
    ratio_hyde_over_nhgis, pct_difference_hyde_vs_nhgis, log_ratio,
    abs_log_ratio
  )

high_hyde_outliers <- matched %>%
  group_by(decade) %>%
  arrange(desc(ratio_hyde_over_nhgis), .by_group = TRUE) %>%
  mutate(rank_within_decade = row_number()) %>%
  ungroup() %>%
  arrange(desc(ratio_hyde_over_nhgis)) %>%
  mutate(rank_panel = row_number()) %>%
  filter(rank_panel <= 100L | rank_within_decade <= 10L) %>%
  select(
    rank_panel, rank_within_decade, GEOID, decade, state_nhgis,
    county_nhgis, name_nhgis, population_nhgis, hyde_population,
    ratio_hyde_over_nhgis, pct_difference_hyde_vs_nhgis, log_ratio,
    abs_log_ratio
  )

###############################################################################
# Save tables
###############################################################################

write_csv(
  matched,
  file.path(results_dir, "hyde_nhgis_population_19c_matched.csv")
)

write_csv(
  decade_summary,
  file.path(results_dir, "hyde_nhgis_population_19c_decade_summary.csv")
)

write_csv(
  low_hyde_outliers,
  file.path(results_dir, "hyde_nhgis_population_19c_outliers_low_hyde.csv")
)

write_csv(
  high_hyde_outliers,
  file.path(results_dir, "hyde_nhgis_population_19c_outliers_high_hyde.csv")
)

###############################################################################
# Save plots
###############################################################################

theme_set(theme_minimal(base_size = 12))

ratio_plot <- ggplot(matched, aes(x = factor(decade), y = ratio_hyde_over_nhgis)) +
  geom_hline(yintercept = 1, linewidth = 0.35, color = "gray40") +
  geom_boxplot(outlier.alpha = 0.15, outlier.size = 0.6, fill = "#8fb9aa") +
  scale_y_log10(labels = scales::label_number(accuracy = 0.1)) +
  labs(
    x = "Census year",
    y = "HYDE / NHGIS population, log scale",
    title = "HYDE county population relative to NHGIS, 1800-1890"
  )

ggsave(
  filename = file.path(results_dir, "hyde_nhgis_population_19c_ratio_by_decade.png"),
  plot = ratio_plot,
  width = 8,
  height = 5,
  dpi = 300
)

totals_plot_data <- matched %>%
  group_by(decade) %>%
  summarise(
    NHGIS = sum(population_nhgis, na.rm = TRUE),
    HYDE = sum(hyde_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(NHGIS, HYDE), names_to = "source", values_to = "population")

totals_plot <- ggplot(totals_plot_data, aes(x = decade, y = population, color = source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = census_years_19c) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  scale_color_manual(values = c("NHGIS" = "#2a6f97", "HYDE" = "#c05a3b")) +
  labs(
    x = "Census year",
    y = "Population in matched counties",
    color = NULL,
    title = "Total matched-county population by source"
  )

ggsave(
  filename = file.path(results_dir, "hyde_nhgis_population_19c_totals_by_decade.png"),
  plot = totals_plot,
  width = 8,
  height = 5,
  dpi = 300
)

scatter_plot <- ggplot(matched, aes(x = population_nhgis, y = hyde_population)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.35, color = "gray40") +
  geom_point(alpha = 0.18, size = 0.7, color = "#3a6b75") +
  geom_text(
    data = matched %>%
      group_by(decade) %>%
      summarise(
        spearman = cor(population_nhgis, hyde_population, method = "spearman"),
        .groups = "drop"
      ),
    aes(
      x = Inf,
      y = Inf,
      label = paste0("Spearman = ", round(spearman, 2))
    ),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.25,
    size = 3.1,
    color = "gray20"
  ) +
  facet_wrap(~ decade, ncol = 5) +
  scale_x_log10(labels = scales::label_number()) +
  scale_y_log10(labels = scales::label_number()) +
  labs(
    x = "NHGIS population, log scale",
    y = "HYDE population, log scale",
    title = "County population comparison by Census year"
  )

ggsave(
  filename = file.path(results_dir, "hyde_nhgis_population_19c_scatter_log.png"),
  plot = scatter_plot,
  width = 11,
  height = 6.5,
  dpi = 300
)

###############################################################################
# Console summary
###############################################################################

cat("\n=== HYDE vs NHGIS county population, 1800-1890 ===\n")
cat("Matched county-year rows: ", nrow(matched), "\n", sep = "")
cat("Years: ", paste(observed_years, collapse = ", "), "\n", sep = "")
cat("Results directory: ", results_dir, "\n\n", sep = "")

print(
  decade_summary %>%
    select(decade, n, spearman, ratio_median, total_ratio_hyde_over_nhgis,
           median_abs_log_ratio),
  n = nrow(decade_summary)
)

elapsed <- difftime(Sys.time(), initial_time, units = "secs")
cat("\nDone in ", round(as.numeric(elapsed), 1), " seconds.\n", sep = "")
