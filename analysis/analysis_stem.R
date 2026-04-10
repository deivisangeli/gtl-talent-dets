###############################################################################
# Project: Determinants of Talent Production
# Goal: STEM-focused analysis — mirrors analysis_main.R but uses STEM panel
#
# Inputs:
#   ../prep/output/data_final_stem.csv    (n_stem per country-year)
#   ../prep/output/data_final_allsci.csv  (n_allsci per country-year)
#
# Outputs (results/ with prefix stem_):
#   stem_plot_absolute.png
#   stem_plot_rate.png
#   stem_plot_share.png
#   stem_ukraine_absolute.png
#   stem_ukraine_rate.png
#   stem_ussr_rate.png
#   stem_descriptive_statistics.txt
#   stem_model_ussr.txt
#   stem_model_staggered.txt
###############################################################################

rm(list = ls())

library("tidyverse")
library("ggplot2")
library("stargazer")
library("fixest")
library("patchwork")
library("modelsummary")

initial_time <- Sys.time()

###############################################################################
# Read data
###############################################################################
stem_raw   <- read_csv("../prep/output/data_final_stem.csv",   show_col_types = FALSE)
allsci_raw <- read_csv("../prep/output/data_final_allsci.csv", show_col_types = FALSE)

###############################################################################
# Join STEM and all-science panels on country, iso3, year
###############################################################################
# Full set of country-years from either panel
panel_base <- full_join(
  stem_raw   %>% select(country, iso3, year, births,
                        n_stem, rate_per_1000_mort, rate_per_1000_deaths),
  allsci_raw %>% select(country, iso3, year,
                        n_allsci,
                        rate_per_1000_mort_all   = rate_per_1000_mort,
                        rate_per_1000_deaths_all = rate_per_1000_deaths),
  by = c("country", "iso3", "year")
)

###############################################################################
# Fill zeros for country-years with no observations
###############################################################################
data_full <- panel_base %>%
  group_by(country) %>%
  complete(
    year = 1800:2015,
    fill = list(
      n_stem                   = 0,
      n_allsci                 = 0,
      rate_per_1000_mort       = 0,
      rate_per_1000_deaths     = 0,
      rate_per_1000_mort_all   = 0,
      rate_per_1000_deaths_all = 0
    )
  ) %>%
  ungroup() %>%
  # STEM share (avoid division by zero)
  mutate(
    stem_share = if_else(n_allsci > 0, n_stem / n_allsci, NA_real_)
  )

###############################################################################
# Descriptive statistics
###############################################################################
summary(panel_base)
summary(data_full)

###############################################################################
# Countries of interest (same as analysis_main.R)
###############################################################################
countries_sel <- c(
  "United States of America", "France", "Germany",
  "United Kingdom", "Brazil", "Argentina", "South Africa"
)

country_colors <- c(
  "United States of America" = "#1B9E77",
  "France"                   = "#D95F02",
  "Germany"                  = "#7570B3",
  "United Kingdom"           = "#E7298A",
  "Brazil"                   = "#66A61E",
  "Argentina"                = "#E6AB02",
  "South Africa"             = "#1F78B4"
)

df_plot <- data_full %>% filter(country %in% countries_sel)

###############################################################################
# Plot 1: Absolute number of STEM scientists — selected countries
###############################################################################
plot_stem_abs <- ggplot(df_plot, aes(x = year, y = n_stem, color = country)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = country_colors) +
  labs(
    title    = "Absolute Number of STEM Scientists (1800-2015)",
    subtitle = "Selected countries",
    x        = "Year",
    y        = "Number of STEM scientists"
  ) +
  theme_bw() +
  theme(
    legend.title     = element_blank(),
    legend.position  = "bottom",
    legend.background = element_blank(),
    legend.key       = element_blank(),
    legend.spacing.y = unit(0, "mm"),
    plot.margin      = unit(c(5, 5, 5, 5), "mm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 11)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

###############################################################################
# Plot 2: STEM scientists per 1,000 birth cohort (mortality-based)
###############################################################################
plot_stem_rate <- ggplot(df_plot, aes(x = year, y = rate_per_1000_mort, color = country)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = country_colors) +
  labs(
    title    = "STEM Scientists per 1,000 Birth Cohort (1800-2015)",
    subtitle = "Selected countries (mortality-adjusted cohort denominator)",
    x        = "Year",
    y        = "STEM scientists per 1,000 births"
  ) +
  theme_bw() +
  theme(
    legend.title     = element_blank(),
    legend.position  = "bottom",
    legend.background = element_blank(),
    legend.key       = element_blank(),
    legend.spacing.y = unit(0, "mm"),
    plot.margin      = unit(c(5, 5, 5, 5), "mm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 11)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

###############################################################################
# Plot 3: STEM share of all Discovery/Science — selected countries
###############################################################################
plot_stem_share <- ggplot(
  df_plot %>% filter(!is.na(stem_share)),
  aes(x = year, y = stem_share, color = country)
) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = country_colors) +
  labs(
    title    = "STEM Share of Notable Scientists (1800-2015)",
    subtitle = "n_stem / n_allsci — selected countries",
    x        = "Year",
    y        = "STEM share"
  ) +
  theme_bw() +
  theme(
    legend.title     = element_blank(),
    legend.position  = "bottom",
    legend.background = element_blank(),
    legend.key       = element_blank(),
    legend.spacing.y = unit(0, "mm"),
    plot.margin      = unit(c(5, 5, 5, 5), "mm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 11)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

###############################################################################
# Ukraine STEM plots
###############################################################################
df_ukr <- data_full %>% filter(country == "Ukraine")

plot_stem_ukraine_abs <- ggplot(df_ukr, aes(x = year, y = n_stem)) +
  geom_line(color = "#1B9E77", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1, color = "#D95F02") +
  labs(
    title = "STEM Scientists in Ukraine (1800-2015)",
    x     = "Year",
    y     = "Number of STEM scientists"
  ) +
  theme_bw() +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.title       = element_text(face = "bold", size = 13),
    axis.title       = element_text(size = 11),
    plot.margin      = unit(c(5, 5, 5, 5), "mm")
  )

plot_stem_ukraine_rate <- ggplot(df_ukr, aes(x = year, y = rate_per_1000_mort)) +
  geom_line(color = "#1B9E77", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1, color = "#D95F02") +
  labs(
    title = "STEM Scientists per 1,000 Birth Cohort — Ukraine (1800-2015)",
    x     = "Year",
    y     = "STEM scientists per 1,000 births"
  ) +
  theme_bw() +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.title       = element_text(face = "bold", size = 13),
    axis.title       = element_text(size = 11),
    plot.margin      = unit(c(5, 5, 5, 5), "mm")
  )

###############################################################################
# USSR STEM scientists — rate comparison
###############################################################################
ussr_major <- c("Russia", "Ukraine", "Belarus", "Kazakhstan", "Georgia", "Uzbekistan")

plot_stem_ussr <- ggplot(
  data_full %>% filter(country %in% ussr_major),
  aes(x = year, y = rate_per_1000_mort, color = country)
) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(
    "Russia"     = "#D95F02",
    "Ukraine"    = "#1B9E77",
    "Belarus"    = "#7570B3",
    "Kazakhstan" = "#E7298A",
    "Georgia"    = "#66A61E",
    "Uzbekistan" = "#E6AB02"
  )) +
  labs(
    title    = "STEM Scientists per 1,000 Birth Cohort (1800-2015)",
    subtitle = "Major USSR countries",
    x        = "Year",
    y        = "STEM scientists per 1,000 births",
    color    = NULL
  ) +
  theme_bw() +
  theme(
    legend.position   = "bottom",
    legend.background = element_blank(),
    legend.key        = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "gray85"),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(size = 11),
    plot.margin       = unit(c(5, 5, 5, 5), "mm")
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

###############################################################################
# DiD regressions — STEM outcome
###############################################################################
ussr_countries <- c(
  "Russia", "Ukraine", "Belarus", "Armenia", "Azerbaijan", "Estonia",
  "Georgia", "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania",
  "Moldova", "Tajikistan", "Turkmenistan", "Uzbekistan"
)

# --- Time-varying USSR membership ---
data_reg <- data_full %>%
  mutate(
    ussr_member_timevarying = if_else(
      country %in% ussr_countries & year >= 1922 & year <= 1991, 1, 0
    )
  )

model_ussr_stem_abs <- feols(
  n_stem ~ ussr_member_timevarying | country + year,
  cluster = ~country,
  data    = data_reg
)

model_ussr_stem_rate <- feols(
  rate_per_1000_mort ~ ussr_member_timevarying | country + year,
  cluster = ~country,
  data    = data_reg
)

summary(model_ussr_stem_abs)
summary(model_ussr_stem_rate)

# --- Staggered DiD on USSR entry years (within USSR countries only) ---
ussr_entry_years <- tibble::tribble(
  ~country,       ~ussr_entry_year,
  "Russia",        1922,
  "Ukraine",       1922,
  "Belarus",       1922,
  "Georgia",       1922,
  "Armenia",       1922,
  "Azerbaijan",    1922,
  "Uzbekistan",    1924,
  "Turkmenistan",  1924,
  "Tajikistan",    1929,
  "Kazakhstan",    1936,
  "Kyrgyzstan",    1936,
  "Moldova",       1940,
  "Latvia",        1940,
  "Lithuania",     1940,
  "Estonia",       1940
)

data_staggered <- data_full %>%
  left_join(ussr_entry_years, by = "country") %>%
  mutate(
    entered_ussr = if_else(
      !is.na(ussr_entry_year) & year >= ussr_entry_year & year <= 1991, 1, 0
    ),
    rel_year = if_else(!is.na(ussr_entry_year), year - ussr_entry_year, NA_real_)
  )

data_staggered_ussr <- data_staggered %>%
  filter(country %in% ussr_countries) %>%
  mutate(post = if_else(year >= ussr_entry_year & year <= 1991, 1, 0))

model_staggered_abs <- feols(
  n_stem ~ post | country + year,
  cluster = ~country,
  data    = data_staggered_ussr
)

model_staggered_rate <- feols(
  rate_per_1000_mort ~ post | country + year,
  cluster = ~country,
  data    = data_staggered_ussr
)

summary(model_staggered_abs)
summary(model_staggered_rate)

# --- Event study: Ukraine vs Western Europe (STEM) ---
west_countries  <- c("France", "Germany", "United Kingdom", "Italy", "Spain")
east_ussr       <- c("Russia", "Belarus", "Kazakhstan", "Armenia", "Georgia")
east_non_ussr   <- c("Poland", "Romania", "Hungary", "Czechia", "Slovakia", "Bulgaria")

run_stem_event_plot <- function(control_countries, group_label,
                                outcome = "n_stem") {
  df <- data_full %>%
    filter(country %in% c("Ukraine", control_countries),
           year >= 1915, year <= 1945) %>%
    mutate(
      treated  = if_else(country == "Ukraine", 1, 0),
      rel_year = year - 1930
    )

  f  <- as.formula(paste0(outcome, " ~ i(rel_year, treated, ref = -1) | country + year"))
  es <- feols(f, cluster = ~country, data = df)

  p <- iplot(
    es,
    main = paste0("Event-Study (STEM): Ukraine vs ", group_label),
    xlab = "Years relative to 1930 (Holodomor cutoff)",
    ylab = paste0("Effect on ", outcome)
  ) +
    theme_bw() +
    theme(
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90")
    )

  return(p)
}

p_west_stem      <- run_stem_event_plot(west_countries,  "Western Europe")
p_east_ussr_stem <- run_stem_event_plot(east_ussr,       "Eastern (inside USSR)")
p_east_non_stem  <- run_stem_event_plot(east_non_ussr,   "Eastern (outside USSR)")

# Rate version
p_west_stem_rate      <- run_stem_event_plot(west_countries,  "Western Europe",        outcome = "rate_per_1000_mort")
p_east_ussr_stem_rate <- run_stem_event_plot(east_ussr,       "Eastern (inside USSR)", outcome = "rate_per_1000_mort")
p_east_non_stem_rate  <- run_stem_event_plot(east_non_ussr,   "Eastern (outside USSR)", outcome = "rate_per_1000_mort")

###############################################################################
# Descriptive statistics export
###############################################################################
data_toexport <- data_full %>%
  select(n_stem, n_allsci, stem_share, rate_per_1000_mort, rate_per_1000_deaths) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.data.frame()

stargazer(
  data_toexport,
  summary = TRUE,
  type    = "text",
  title   = "Descriptive Statistics — STEM Scientists Dataset (1800-2015)",
  summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
  covariate.labels = c(
    "STEM scientists (absolute)",
    "All Discovery/Science scientists (absolute)",
    "STEM share of Discovery/Science",
    "STEM scientists per 1,000 births (mortality-based cohort)",
    "STEM scientists per 1,000 births (deaths-based cohort)"
  ),
  out = "results/stem_descriptive_statistics.txt"
)

###############################################################################
# Save figures
###############################################################################
ggsave("results/stem_plot_absolute.png",   plot_stem_abs,          width = 9, height = 6, dpi = 300)
ggsave("results/stem_plot_rate.png",       plot_stem_rate,         width = 9, height = 6, dpi = 300)
ggsave("results/stem_plot_share.png",      plot_stem_share,        width = 9, height = 6, dpi = 300)
ggsave("results/stem_ukraine_absolute.png", plot_stem_ukraine_abs, width = 9, height = 6, dpi = 300)
ggsave("results/stem_ukraine_rate.png",    plot_stem_ukraine_rate, width = 9, height = 6, dpi = 300)
ggsave("results/stem_ussr_rate.png",       plot_stem_ussr,         width = 9, height = 6, dpi = 300)

# Event-study plots
ggsave("results/stem_es_west_abs.png",       p_west_stem,           width = 8, height = 5, dpi = 300)
ggsave("results/stem_es_east_ussr_abs.png",  p_east_ussr_stem,      width = 8, height = 5, dpi = 300)
ggsave("results/stem_es_east_non_abs.png",   p_east_non_stem,       width = 8, height = 5, dpi = 300)
ggsave("results/stem_es_west_rate.png",      p_west_stem_rate,      width = 8, height = 5, dpi = 300)
ggsave("results/stem_es_east_ussr_rate.png", p_east_ussr_stem_rate, width = 8, height = 5, dpi = 300)
ggsave("results/stem_es_east_non_rate.png",  p_east_non_stem_rate,  width = 8, height = 5, dpi = 300)

###############################################################################
# Model output
###############################################################################
sink("results/stem_model_ussr.txt")
cat("=== Time-varying USSR membership — STEM absolute ===\n")
summary(model_ussr_stem_abs)
cat("\n=== Time-varying USSR membership — STEM rate (per 1,000 births) ===\n")
summary(model_ussr_stem_rate)
sink()

sink("results/stem_model_staggered.txt")
cat("=== Staggered DiD (within-USSR) — STEM absolute ===\n")
summary(model_staggered_abs)
cat("\n=== Staggered DiD (within-USSR) — STEM rate (per 1,000 births) ===\n")
summary(model_staggered_rate)
sink()

cat("\nAll STEM figures and model outputs saved to results/ with prefix stem_\n")

final_time <- Sys.time() - initial_time
print(paste("This code ran in", round(final_time, 1), "seconds."))
