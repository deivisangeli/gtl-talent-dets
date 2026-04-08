###############################################################################
# Project: Determinants of Talent Production in USSR 
# Author: Lucas Mattos
# Goal: Creating Outputs for Analysis
###############################################################################

# Clean the working environment
rm(list = ls())

# Packages
library("tidyverse")
library("ggplot2")
library("stargazer")
library("fixest")
library("patchwork")
library("ggplotify")
library("modelsummary")

# Recording Initial Time
initial_time <- Sys.time()

###############################################################################
# Uploading databases
###############################################################################

data <- read_csv("Output/data_final.csv", show_col_types = FALSE)

# Notice there are a lot of NAs, which is good to seeing dexcriptive statistics
# however this NAs simply come from no obserrvations, so let's fill with 0 when
# there is no observations

# Filling with 0s
data_full <- data %>%
 group_by(country) %>%
 complete(year = 1800:2015,
          fill = list(
           n_inventors = 0,
           rate_per_1000_mort = 0,
           rate_per_1000_deaths = 0
          )) %>%
 ungroup()


###############################################################################
# First let's create descriptive statistics 
###############################################################################

summary(data)

summary(data_full)

###############################################################################
# Now let's focus on some charts 
###############################################################################
# Filter countries of interest
countries_sel <- c("United States of America", "France", "Germany", 
                   "United Kingdom", "Brazil", "Argentina", "South Africa")

df_plot <- data_full %>%
 filter(country %in% countries_sel)

###############################################################################
# Plot absolute numbers
plot_inventors_selected <- ggplot(df_plot, aes(x = year, y = n_inventors, color = country)) +
 geom_line(linewidth = 0.9) +
 scale_colour_manual(values = c(
  "United States of America" = "#1B9E77",  
  "France"                   = "#D95F02",  
  "Germany"                  = "#7570B3",  
  "United Kingdom"           = "#E7298A", 
  "Brazil"                   = "#66A61E",  
  "Argentina"                = "#E6AB02",  
  "South Africa"             = "#1F78B4"   
 )) +
 labs(
  title = "Absolute Number of Inventors (1800–2015)",
  subtitle = "Selected countries",
  x = "Year",
  y = "Number of Inventors"
 ) +
 theme_bw() +
 theme(
  legend.title = element_blank(),
  legend.position = "bottom",
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.spacing.y = unit(0, "mm"),
  plot.margin = unit(c(5, 5, 5, 5), "mm"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "gray85"),
  plot.title = element_text(face = "bold", size = 13),
  plot.subtitle = element_text(size = 11)
 ) +
 guides(color = guide_legend(nrow = 1, byrow = TRUE))

# Show the plot
plot_inventors_selected

###############################################################################
# Plotting the rate, with deaths
plot_rate_selected <- ggplot(df_plot, aes(x = year, y = rate_per_1000_deaths, color = country)) +
 geom_line(linewidth = 0.9) +
 scale_colour_manual(values = c(
  "United States of America" = "#1B9E77",
  "France"                   = "#D95F02",
  "Germany"                  = "#7570B3",
  "United Kingdom"           = "#E7298A",
  "Brazil"                   = "#66A61E",
  "Argentina"                = "#E6AB02",
  "South Africa"             = "#1F78B4"
 )) +
 labs(
  title = "Inventors per 1,000 Birth Cohort (1800–2015)",
  subtitle = "Selected countries (based on deaths-adjusted cohorts)",
  x = "Year",
  y = "Inventors per 1,000 births"
 ) +
 theme_bw() +
 theme(
  legend.title = element_blank(),
  legend.position = "bottom",
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.spacing.y = unit(0, "mm"),
  plot.margin = unit(c(5, 5, 5, 5), "mm"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "gray85"),
  plot.title = element_text(face = "bold", size = 13),
  plot.subtitle = element_text(size = 11)
 ) +
 guides(color = guide_legend(nrow = 1, byrow = TRUE))


# Show the plot
plot_rate_selected

###############################################################################
# Ukraine  
###############################################################################
# Now for Ukraine
df_plot <- data_full %>%
 filter(country == "Ukraine")

# Plot
plot_ukraine <- ggplot(df_plot, aes(x = year, y = n_inventors)) +
 geom_line(color = "#1B9E77", linewidth = 1) +
 geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1, color = "#D95F02") +
 labs(
  title = "Absolute Number of Inventors in Ukraine (1800–2015)",
  x = "Year",
  y = "Number of Inventors"
 ) +
 theme_bw() +
 theme(
  legend.position = "none",
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "gray90"),
  plot.title = element_text(face = "bold", size = 13),
  axis.title = element_text(size = 11),
  plot.margin = unit(c(5, 5, 5, 5), "mm")
 )

plot_ukraine

plot_ukraine_rate <- ggplot(df_plot, aes(x = year, y = rate_per_1000_deaths)) +
 geom_line(color = "#1B9E77", linewidth = 1) +
 geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1, color = "#D95F02") +
 labs(
  title = "Inventors per 1,000 Birth Cohort (1800–2015)",
  x = "Year",
  y = "Inventors per 1,000 births"
 ) +
 theme_bw() +
 theme(
  legend.position = "none",
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "gray90"),
  plot.title = element_text(face = "bold", size = 13),
  axis.title = element_text(size = 11),
  plot.margin = unit(c(5, 5, 5, 5), "mm")
 )

plot_ukraine_rate

# Zooming In near Holodomor
df_plot_sub <- df_plot %>%
 filter(country == "Ukraine", year >= 1920, year <= 1950)

plot_ukraine_rate_zoom <- ggplot(df_plot_sub, aes(x = year, y = rate_per_1000_deaths)) +
 geom_line(color = "#1B9E77", linewidth = 1) +
 geom_smooth(method = "loess", se = FALSE, span = 0.4, linewidth = 1, color = "#D95F02") +
 scale_x_continuous(
  breaks = seq(1920, 1950, by = 2), 
  limits = c(1920, 1950)
 ) +
 labs(
  title = "Inventors per 1,000 Birth Cohort in Ukraine (1920–1950)",
  x = "Year",
  y = "Inventors per 1,000 births"
 ) +
 theme_bw() +
 theme(
  legend.position = "none",
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "gray90"),
  plot.title = element_text(face = "bold", size = 13),
  plot.subtitle = element_text(size = 11),
  axis.title = element_text(size = 11),
  axis.text.x = element_text(angle = 45, hjust = 1),
  plot.margin = unit(c(5, 5, 5, 5), "mm")
 )

plot_ukraine_rate_zoom


# Now like an RDD
cutoff_year <- 1930
bw <- 10 

# Prep windowed data
ukr <- data_full %>%
 filter(country == "Ukraine") %>%
 mutate(
  running = year - cutoff_year,
  side = if_else(year < cutoff_year, "Pre", "Post")
 ) %>%
 filter(abs(running) <= bw)

# RDD-style plot
plot_rdd <- ggplot(ukr, aes(x = running, y = n_inventors)) +
 geom_point(aes(color = side), size = 2, alpha = 0.8) +
 geom_smooth(
  data = subset(ukr, running < 0),
  method = "lm", formula = y ~ x, se = FALSE, linewidth = 1
 ) +
 geom_smooth(
  data = subset(ukr, running >= 0),
  method = "lm", formula = y ~ x, se = FALSE, linewidth = 1
 ) +
 geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
 scale_x_continuous(breaks = seq(-bw, bw, by = 1), limits = c(-bw, bw)) +
 scale_color_manual(values = c("Pre" = "#1B9E77", "Post" = "#D95F02")) +
 labs(
  title = "RDD-style: Inventors in Ukraine around the Holodomor",
  subtitle = paste0("Cutoff = ", cutoff_year, " (window ±", bw, " years)"),
  x = "Years relative to 1930 (Holodomor cutoff)",
  y = "Inventors",
  color = NULL
 ) +
 theme_bw() +
 theme(
  legend.position = "bottom",
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "gray90"),
  plot.title = element_text(face = "bold", size = 13),
  plot.margin = unit(c(5, 5, 5, 5), "mm")
 ) +
 annotate(
  "label",
  x = -0.2,
  y = max(ukr$rate_per_1000_deaths, na.rm = TRUE),
  label = "Holodomor\n(1930)",
  hjust = 1, vjust = 0, size = 3
 )

plot_rdd

# Now we can do an event study
# Define groups
west_countries <- c("France", "Germany", "United Kingdom", "Italy", "Spain")
east_ussr <- c("Russia", "Belarus", "Kazakhstan", "Armenia", "Georgia")
east_non_ussr <- c("Poland", "Romania", "Hungary", "Czechia", "Slovakia", "Bulgaria")

# Function to return the event study plots
run_event_plot <- function(control_countries, group_label, outcome = "n_inventors") {
 df <- data_full %>%
  filter(country %in% c("Ukraine", control_countries), year >= 1915, year <= 1945 ) %>%
  mutate(
   treated  = if_else(country == "Ukraine", 1, 0),
   rel_year = year - 1930
  )
 
 # event-study with country & year FE
 f <- as.formula(paste0(outcome, " ~ i(rel_year, treated, ref = -1) | country + year"))
 es <- feols(f, cluster = ~country, data = df)
 
 # return the ggplot object only
 p <- iplot(
  es,
  main = paste0("Event-Study: Ukraine vs ", group_label),
  xlab = "Years relative to 1930 (Holodomor cutoff)",
  ylab = "Effect on inventors absolute number"
 ) +
  theme_bw() +
  theme(
   plot.title   = element_text(face = "bold"),
   panel.grid.minor = element_blank(),
   panel.grid.major = element_line(color = "gray90")
  )
 
 return(p)
}

# Plots
p_west        <- run_event_plot(west_countries, "Western Europe")
p_east_ussr   <- run_event_plot(east_ussr, "Eastern (inside USSR)")
p_east_non    <- run_event_plot(east_non_ussr, "Eastern (outside USSR)")

###############################################################################
# USSR  
###############################################################################
# USSR member countries
ussr_major <- c("Russia", "Ukraine", "Belarus", "Kazakhstan", "Georgia", "Uzbekistan")

df_plot_ussr_major <- data_full %>%
 filter(country %in% ussr_major)

plot_ussr_major <- ggplot(df_plot_ussr_major, aes(x = year, y = rate_per_1000_deaths, color = country)) +
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
  title = "Inventors per 1,000 Birth Cohort (1800–2015)",
  subtitle = "Major USSR countries",
  x = "Year",
  y = "Inventors per 1,000 births",
  color = NULL
 ) +
 theme_bw() +
 theme(
  legend.position = "bottom",
  legend.background = element_blank(),
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "gray85"),
  plot.title = element_text(face = "bold", size = 13),
  plot.subtitle = element_text(size = 11),
  plot.margin = unit(c(5, 5, 5, 5), "mm")
 ) +
 guides(color = guide_legend(nrow = 2, byrow = TRUE))

plot_ussr_major

# Now comparing with the West Countries
ussr_focus <- c("Russia", "Ukraine", "Belarus", "Georgia")
west_focus <- c("United States of America", "United Kingdom", "France", "Germany", "Italy", "Spain", "Japan", "Canada")

# Filter dataset
df_plot_compare <- data_full %>%
 filter(country %in% c(ussr_focus, west_focus)) %>%
 mutate(region = if_else(country %in% ussr_focus, "USSR", "West"))

# Plot USSR vs other countries
plot_ussr_vs_west_focus <- ggplot(df_plot_compare, aes(x = year, y = rate_per_1000_deaths,
                                                       group = country, color = region)) +
 geom_line(linewidth = 0.8, alpha = 0.9) +
 scale_color_manual(values = c("USSR" = "#D95F02", "West" = "#1B9E77")) +
 labs(
  title = "Inventors per 1,000 Birth Cohort (1800–2015)",
  subtitle = "USSR (Russia, Ukraine, Belarus, Georgia) vs. Western countries",
  x = "Year",
  y = "Inventors per 1,000 births",
  color = NULL
 ) +
 theme_bw() +
 theme(
  legend.position = "bottom",
  legend.background = element_blank(),
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "gray85"),
  plot.title = element_text(face = "bold", size = 13),
  plot.subtitle = element_text(size = 11),
  plot.margin = unit(c(5, 5, 5, 5), "mm")
 )

plot_ussr_vs_west_focus

# Now comparing with more comparable countries:
# Comparable Eastern Europe countries
east_non_ussr <- c("Poland", "Hungary", "Czechia", "Slovakia", "Bulgaria", "Serbia")

# Filter dataset
df_plot_compare_east <- data_full %>%
 filter(country %in% c(ussr_focus, east_non_ussr)) %>%
 mutate(region = if_else(country %in% ussr_focus, "USSR", "East (non-USSR)"))

# Plot USSR vs Eastern Europe
plot_ussr_vs_east_focus <- ggplot(df_plot_compare_east,
                                  aes(x = year, y = rate_per_1000_deaths,
                                      group = country, color = region)) +
 geom_line(linewidth = 0.8, alpha = 0.9) +
 scale_color_manual(values = c("USSR" = "#D95F02", "East (non-USSR)" = "#1B9E77")) +
 labs(
  title = "Inventors per 1,000 Birth Cohort (1800–2015)",
  subtitle = "USSR (Russia, Ukraine, Belarus, Georgia) vs. Poland, Hungary, Czechia, Slovakia, Bulgaria, Serbia",
  x = "Year",
  y = "Inventors per 1,000 births",
  color = NULL
 ) +
 theme_bw() +
 theme(
  legend.position = "bottom",
  legend.background = element_blank(),
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "gray85"),
  plot.title = element_text(face = "bold", size = 13),
  plot.subtitle = element_text(size = 11),
  plot.margin = unit(c(5, 5, 5, 5), "mm")
 )

# Show the plot
plot_ussr_vs_east_focus

# Now lets run a regression and see at least the sign of being in the USSR
ussr_countries <- c(
 "Russia", "Ukraine", "Belarus", "Armenia", "Azerbaijan", "Estonia",
 "Georgia", "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania",
 "Moldova", "Tajikistan", "Turkmenistan", "Uzbekistan"
)

data_reg <- data_full %>%
 mutate(
  ussr_member_timevarying = if_else(
   country %in% ussr_countries & year >= 1922 & year <= 1991, 1, 0
  )
 )

model_ussr_timevarying <- feols(
 rate_per_1000_deaths ~ ussr_member_timevarying | country + year,
 cluster = ~country,
 data = data_reg
)

summary(model_ussr_timevarying)

# Now we can think of a staggered model, leveraging on the fact that countries entered
# the USSR in different years
ussr_entry_years <- tibble::tribble(
 ~country,        ~ussr_entry_year,
 "Russia",         1922,
 "Ukraine",        1922,
 "Belarus",        1922,
 "Georgia",        1922,
 "Armenia",        1922,
 "Azerbaijan",     1922,
 "Uzbekistan",     1924,
 "Turkmenistan",   1924,
 "Tajikistan",     1929,
 "Kazakhstan",     1936,
 "Kyrgyzstan",     1936,
 "Moldova",        1940,
 "Latvia",         1940,
 "Lithuania",      1940,
 "Estonia",        1940)

data_staggered <- data_full %>%
 left_join(ussr_entry_years, by = "country") %>%
 mutate(
  entered_ussr = if_else(!is.na(ussr_entry_year) & year >= ussr_entry_year & year <= 1991, 1, 0),
  rel_year     = if_else(!is.na(ussr_entry_year), year - ussr_entry_year, NA_real_)
 )

data_staggered_ussr <- data_staggered %>%
 filter(country %in% ussr_countries)

# Define pre/post within those countries
data_staggered_ussr <- data_staggered_ussr %>%
 mutate(
  post = if_else(year >= ussr_entry_year & year <= 1991, 1, 0)
 )

# Run FE regression only on USSR countries
model_staggered <- feols(
 rate_per_1000_deaths ~ post | country + year,
 cluster = ~country,
 data = data_staggered_ussr
)

summary(model_staggered)

###############################################################################
# Exporting
###############################################################################

# Descriptive Statistics
datafull_toexport <- data_full %>%
 select(n_inventors, rate_per_1000_mort, rate_per_1000_deaths) %>%
 mutate(across(everything(), as.numeric)) %>%
 as.data.frame()

stargazer(datafull_toexport,
          summary = TRUE,
          type = "text",
          title = "Descriptive Statistics for Inventor Dataset (1800–2015)",
          summary.stat = c("n","mean","sd","min","p25","median","p75","max"),
          covariate.labels = c(
           "Absolute Number of Inventors",
           "Rate of Inventors (per 1,000 births, mortality-based cohort)",
           "Rate of Inventors (per 1,000 births, deaths-based cohort)"),
          out = "Results/descriptive_statistics_datafull.txt")

data_toexport <- data %>%
 select(n_inventors, rate_per_1000_mort, rate_per_1000_deaths) %>%
 mutate(across(everything(), as.numeric)) %>%
 as.data.frame()

stargazer(data_toexport,
          summary = TRUE,
          type = "text",
          title = "Descriptive Statistics for Inventor Dataset (1800–2015)",
          summary.stat = c("n","mean","sd","min","p25","median","p75","max"),
          covariate.labels = c(
           "Absolute Number of Inventors",
           "Rate of Inventors (per 1,000 births, mortality-based cohort)",
           "Rate of Inventors (per 1,000 births, deaths-based cohort)"),
          out = "Results/descriptive_statistics_data.txt")

# Sanity Check Plots:
## Absolute
ggsave(filename = "Results/plot_inventors_selected.png",
 plot = plot_inventors_selected,
 width = 9, height = 6, dpi = 300)

## Rate
ggsave(filename = "Results/plot_rate_selected.png",
 plot = plot_rate_selected,
 width = 9, height = 6, dpi = 300)

# Ukraine plots

ggsave(filename = "Results/ukr_inventors.png",
 plot = plot_ukraine,
 width = 9, height = 6, dpi = 300)

ggsave(filename = "Results/ukr_inventors_rate.png",
       plot = plot_ukraine_rate,
       width = 9, height = 6, dpi = 300)

ggsave(filename = "Results/ukr_inventors_rate_zoom.png",
       plot = plot_ukraine_rate_zoom,
       width = 9, height = 6, dpi = 300)

ggsave(filename = "Results/ukr_rdd.png",
       plot = plot_rdd,
       width = 9, height = 6, dpi = 300)

# USSR plots
ggsave(filename = "Results/ussr_countries.png",
       plot = plot_ussr_major,
       width = 9, height = 6, dpi = 300)

ggsave(filename = "Results/ussr_comp.png",
       plot = plot_ussr_vs_west_focus,
       width = 9, height = 6, dpi = 300)

ggsave(filename = "Results/ussr_comp_east.png",
       plot = plot_ussr_vs_east_focus,
       width = 9, height = 6, dpi = 300)

sink("Results\\model_staggered_summary.txt")
summary(model_staggered)
sink()

sink("Results\\model_ussr.txt")
summary(model_ussr_timevarying)
sink()

