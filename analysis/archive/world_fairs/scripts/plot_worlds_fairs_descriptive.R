###############################################################################
# Descriptive plots for world's fairs held between 1790 and 1910.
#
# Input:
#   DATA_INPUT/worlds_fairs_wikipedia.xlsx
#   DATA_INPUT/worlds_fairs/worlds_fairs_visits_1790_1910_with_venues_exhaustive.csv
#
# Run from analysis/:
#   Rscript plot_worlds_fairs_descriptive.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(stringr)
  library(tidyr)
})

source("../paths.R")

input_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia.xlsx")
visits_file <- file.path(
  DATA_INPUT,
  "worlds_fairs",
  "worlds_fairs_visits_1790_1910_with_venues_exhaustive.csv"
)
results_dir <- file.path("results", "worlds_fairs_descriptive")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

start_year <- 1790L
end_year <- 1910L

if (!file.exists(input_file)) {
  stop("World's fairs input file not found: ", input_file)
}

if (!file.exists(visits_file)) {
  stop("World's fairs visits file not found: ", visits_file)
}

fairs_raw <- read_xlsx(input_file)
required_columns <- c("Year", "City", "Country", "Fair_name")
missing_columns <- setdiff(required_columns, names(fairs_raw))

if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

if (!"Fair_observation" %in% names(fairs_raw)) {
  fairs_raw <- fairs_raw %>% mutate(Fair_observation = NA_character_)
}

fairs <- fairs_raw %>%
  mutate(
    year_start = suppressWarnings(
      as.integer(str_extract(as.character(Year), "^[0-9]{4}"))
    ),
    fair_text = str_to_lower(
      paste(coalesce(Fair_name, ""), coalesce(Fair_observation, ""))
    ),
    was_held = !str_detect(fair_text, "never held|cancelled")
  ) %>%
  filter(
    between(year_start, start_year, end_year),
    was_held
  ) %>%
  select(-fair_text, -was_held)

if (nrow(fairs) == 0) {
  stop("No held fairs found between ", start_year, " and ", end_year, ".")
}

if (any(!between(fairs$year_start, start_year, end_year))) {
  stop("The filtered sample contains years outside the requested range.")
}

if (any(str_detect(
  str_to_lower(paste(coalesce(fairs$Fair_name, ""), coalesce(fairs$Fair_observation, ""))),
  "never held|cancelled"
))) {
  stop("The filtered sample still contains a cancelled or never-held fair.")
}

location_counts <- fairs %>%
  filter(
    !is.na(Country), Country != "",
    !is.na(City), City != ""
  ) %>%
  count(Country, City, name = "n_fairs") %>%
  arrange(desc(n_fairs), Country, City) %>%
  mutate(country_city = paste(Country, City, sep = " - "))

if (nrow(location_counts) == 0) {
  stop("No valid country-city combinations found in the filtered sample.")
}

fair_count_distribution <- location_counts %>%
  count(n_fairs, name = "n_country_city_pairs") %>%
  arrange(n_fairs)

if (sum(fair_count_distribution$n_country_city_pairs) != nrow(location_counts)) {
  stop("Fair-count distribution does not match the number of country-city pairs.")
}

decade_counts <- fairs %>%
  mutate(decade = as.integer(floor(year_start / 10) * 10)) %>%
  count(decade, name = "n_fairs") %>%
  complete(
    decade = seq(
      as.integer(floor(start_year / 10) * 10),
      as.integer(floor(end_year / 10) * 10),
      by = 10L
    ),
    fill = list(n_fairs = 0L)
  ) %>%
  arrange(decade)

first_fair_decade_counts <- fairs %>%
  filter(
    !is.na(Country), Country != "",
    !is.na(City), City != ""
  ) %>%
  group_by(Country, City) %>%
  summarise(first_fair_year = min(year_start), .groups = "drop") %>%
  mutate(decade = as.integer(floor(first_fair_year / 10) * 10)) %>%
  count(decade, name = "n_country_city_pairs") %>%
  complete(
    decade = seq(
      as.integer(floor(start_year / 10) * 10),
      as.integer(floor(end_year / 10) * 10),
      by = 10L
    ),
    fill = list(n_country_city_pairs = 0L)
  ) %>%
  arrange(decade)

if (sum(first_fair_decade_counts$n_country_city_pairs) != nrow(location_counts)) {
  stop("First-fair decade counts do not match the number of country-city pairs.")
}

country_counts <- fairs %>%
  filter(!is.na(Country), Country != "") %>%
  count(Country, name = "n_fairs") %>%
  arrange(desc(n_fairs), Country)

top_10_locations <- location_counts %>%
  slice_head(n = 10) %>%
  mutate(country_city = factor(country_city, levels = rev(country_city)))

top_10_countries <- country_counts %>%
  slice_head(n = 10) %>%
  mutate(Country = factor(Country, levels = rev(Country)))

if (nrow(top_10_locations) != 10) {
  stop("Expected exactly 10 country-city combinations in the top-10 plot.")
}

if (nrow(top_10_countries) != 10) {
  stop("Expected exactly 10 countries in the top-10 countries plot.")
}

visits_raw <- readr::read_csv(visits_file, show_col_types = FALSE)
visits_required_columns <- c("visits", "search_status")
visits_missing_columns <- setdiff(visits_required_columns, names(visits_raw))

if (length(visits_missing_columns) > 0) {
  stop(
    "Missing required visits columns: ",
    paste(visits_missing_columns, collapse = ", ")
  )
}

fair_size_sample <- visits_raw %>%
  filter(
    !is.na(visits),
    visits > 0,
    search_status %in% c("found", "conflicting_sources")
  )

fair_size_status_counts <- fair_size_sample %>%
  count(search_status) %>%
  tidyr::complete(
    search_status = c("found", "conflicting_sources"),
    fill = list(n = 0L)
  )

expected_status_counts <- c(found = 113L, conflicting_sources = 20L)
observed_status_counts <- setNames(
  fair_size_status_counts$n,
  fair_size_status_counts$search_status
)

if (
  nrow(fair_size_sample) != 133L ||
    any(observed_status_counts[names(expected_status_counts)] != expected_status_counts)
) {
  stop("Unexpected fair-size sample composition.")
}

if (
  min(fair_size_sample$visits) != 10000 ||
    max(fair_size_sample$visits) != 50860801 ||
    median(fair_size_sample$visits) != 1156232
) {
  stop("Unexpected fair-size sample range or median.")
}

fair_size_statistics <- fair_size_sample %>%
  summarise(
    mean = mean(visits),
    q1 = quantile(visits, 0.25, names = FALSE),
    median = median(visits),
    q3 = quantile(visits, 0.75, names = FALSE)
  )

fair_size_statistics_label <- paste(
  paste0("Mean: ", scales::comma(round(fair_size_statistics$mean))),
  paste0("Median: ", scales::comma(round(fair_size_statistics$median))),
  paste0("Q1: ", scales::comma(round(fair_size_statistics$q1))),
  paste0("Q3: ", scales::comma(round(fair_size_statistics$q3))),
  sep = "\n"
)

build_histogram_counts <- function(values, breaks) {
  bin_id <- cut(
    values,
    breaks = breaks,
    include.lowest = TRUE,
    right = TRUE,
    labels = FALSE
  )

  tibble(
    bin_id = seq_len(length(breaks) - 1L),
    bin_left = breaks[-length(breaks)],
    bin_right = breaks[-1]
  ) %>%
    left_join(
      tibble(bin_id = bin_id) %>% count(bin_id, name = "n_fairs"),
      by = "bin_id"
    ) %>%
    mutate(
      n_fairs = replace_na(n_fairs, 0L),
      bin_mid = (bin_left + bin_right) / 2,
      bin_width = bin_right - bin_left
    )
}

linear_breaks <- seq(
  min(fair_size_sample$visits),
  max(fair_size_sample$visits),
  length.out = 21L
)
linear_size_distribution <- build_histogram_counts(
  fair_size_sample$visits,
  linear_breaks
) %>%
  mutate(
    bin_mid_millions = bin_mid / 1e6,
    bin_width_millions = bin_width / 1e6
  )

log_visits <- log10(fair_size_sample$visits)
log_breaks <- seq(min(log_visits), max(log_visits), length.out = 16L)
log_size_distribution <- build_histogram_counts(log_visits, log_breaks)

if (
  sum(linear_size_distribution$n_fairs) != nrow(fair_size_sample) ||
    sum(log_size_distribution$n_fairs) != nrow(fair_size_sample)
) {
  stop("Fair-size histogram counts do not match the analytical sample.")
}

plot_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title.position = "plot"
  )

count_breaks <- seq.int(1L, max(location_counts$n_fairs), by = 1L)

plot_count_histogram <- ggplot(
  fair_count_distribution,
  aes(x = n_fairs, y = n_country_city_pairs)
) +
  geom_col(
    width = 1,
    fill = "#2f7786",
    color = "white"
  ) +
  geom_text(aes(label = n_country_city_pairs), vjust = -0.4, size = 4) +
  scale_x_continuous(breaks = count_breaks) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Distribution of fairs across country-city pairs",
    subtitle = "Fairs held between 1790 and 1910",
    x = "Number of fairs",
    y = "Number of country-city pairs"
  ) +
  plot_theme

plot_fairs_by_decade <- ggplot(decade_counts, aes(x = decade, y = n_fairs)) +
  geom_col(fill = "#2f7786", width = 8) +
  geom_text(aes(label = n_fairs), vjust = -0.4, size = 4) +
  scale_x_continuous(breaks = decade_counts$decade) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Number of fairs by decade",
    subtitle = "Fairs held between 1790 and 1910",
    x = "Decade",
    y = "Number of fairs"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_first_fair_by_decade <- ggplot(
  first_fair_decade_counts,
  aes(x = decade, y = n_country_city_pairs)
) +
  geom_col(fill = "#2f7786", width = 8) +
  geom_text(aes(label = n_country_city_pairs), vjust = -0.4, size = 4) +
  scale_x_continuous(breaks = first_fair_decade_counts$decade) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Country-city pairs holding their first fair by decade",
    subtitle = "First fairs held between 1790 and 1910",
    x = "Decade",
    y = "Number of country-city pairs"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_top_10 <- ggplot(top_10_locations, aes(x = n_fairs, y = country_city)) +
  geom_col(fill = "#2f7786", width = 0.75) +
  geom_text(aes(label = n_fairs), hjust = -0.25, size = 4) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Top 10 country-city pairs by number of fairs",
    subtitle = "Fairs held between 1790 and 1910",
    x = "Number of fairs",
    y = NULL
  ) +
  plot_theme

plot_top_10_countries <- ggplot(top_10_countries, aes(x = n_fairs, y = Country)) +
  geom_col(fill = "#2f7786", width = 0.75) +
  geom_text(aes(label = n_fairs), hjust = -0.25, size = 4) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Top 10 countries by number of fairs",
    subtitle = "Fairs held between 1790 and 1910",
    x = "Number of fairs",
    y = NULL
  ) +
  plot_theme

plot_fair_size_linear <- ggplot(
  linear_size_distribution,
  aes(x = bin_mid_millions, y = n_fairs)
) +
  geom_col(
    aes(width = bin_width_millions * 0.95),
    fill = "#2f7786",
    color = "white"
  ) +
  geom_text(
    data = filter(linear_size_distribution, n_fairs > 0),
    aes(label = n_fairs),
    vjust = -0.4,
    size = 4
  ) +
  annotate(
    "label",
    x = Inf,
    y = Inf,
    label = fair_size_statistics_label,
    hjust = 1.05,
    vjust = 1.1,
    size = 4,
    linewidth = 0,
    fill = scales::alpha("white", 0.85)
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Distribution of fair size",
    subtitle = "133 fairs with attendance data, 1790-1910",
    x = "Reported visits (millions)",
    y = "Number of fairs"
  ) +
  plot_theme

log_axis_visits <- c(1e4, 1e5, 1e6, 1e7, 5e7)

plot_fair_size_log <- ggplot(
  log_size_distribution,
  aes(x = bin_mid, y = n_fairs)
) +
  geom_col(
    aes(width = bin_width * 0.95),
    fill = "#2f7786",
    color = "white"
  ) +
  geom_text(
    data = filter(log_size_distribution, n_fairs > 0),
    aes(label = n_fairs),
    vjust = -0.4,
    size = 4
  ) +
  annotate(
    "label",
    x = Inf,
    y = Inf,
    label = fair_size_statistics_label,
    hjust = 1.05,
    vjust = 1.1,
    size = 4,
    linewidth = 0,
    fill = scales::alpha("white", 0.85)
  ) +
  scale_x_continuous(
    breaks = log10(log_axis_visits),
    labels = scales::label_number(big.mark = ",", accuracy = 1)(log_axis_visits)
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Distribution of fair size (log scale)",
    subtitle = "133 fairs with attendance data, 1790-1910",
    x = "Reported visits (log scale)",
    y = "Number of fairs"
  ) +
  plot_theme

ggsave(
  file.path(results_dir, "worlds_fairs_count_histogram_1790_1910.png"),
  plot_count_histogram,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(results_dir, "worlds_fairs_by_decade_1790_1910.png"),
  plot_fairs_by_decade,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(
    results_dir,
    "worlds_fairs_country_city_first_fair_by_decade_1790_1910.png"
  ),
  plot_first_fair_by_decade,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(results_dir, "worlds_fairs_top_10_country_city_1790_1910.png"),
  plot_top_10,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(results_dir, "worlds_fairs_top_10_countries_1790_1910.png"),
  plot_top_10_countries,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(results_dir, "worlds_fairs_size_distribution_linear_1790_1910.png"),
  plot_fair_size_linear,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(results_dir, "worlds_fairs_size_distribution_log_1790_1910.png"),
  plot_fair_size_log,
  width = 9,
  height = 6,
  dpi = 300
)

message("Input rows: ", nrow(fairs_raw))
message("Held fairs, 1790-1910: ", nrow(fairs))
message("Country-city combinations: ", nrow(location_counts))
message("Fairs in size-distribution sample: ", nrow(fair_size_sample))
message("Saved plots in: ", results_dir)
