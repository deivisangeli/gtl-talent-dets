###############################################################################
# Project: Determinants of Talent Production via Scientific Infrastructure
# Author: Lucas Mattos and Deivis Angeli (GTF)
# Goal: Event studies for scientific facilities — STEM scientists only
#
# Mirrors analysis_jan26.R but uses data_final_stem.csv (STEM-only counts)
# instead of data_final_new.csv (all Discovery/Science).
# Figures saved with stem_ prefix for direct comparison.
###############################################################################

rm(list = ls())

library("tidyverse")
library("ggplot2")
library("stargazer")
library("fixest")
library("patchwork")
library("ggplotify")
library("modelsummary")
library("did")
library("fixest")

initial_time <- Sys.time()

###############################################################################
# Load data
###############################################################################
data <- read_csv("../prep/output/data_final_stem.csv", show_col_types = FALSE) %>%
  rename(n_inventors = n_stem)   # align column name with analysis_jan26.R

facilities <- read_csv2("../prep/output/scientific_facilities.csv",
                        show_col_types = FALSE)

###############################################################################
# Adjust data
###############################################################################
data_full <- data %>%
  group_by(country, iso3) %>%
  complete(year = 1800:2015,
           fill = list(n_inventors = 0, rate_per_100k_births = 0, births = 0)) %>%
  ungroup()

rate_births_decade <- data_full %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(country, decade) %>%
  summarise(rate_per_100k_births = mean(rate_per_100k_births, na.rm = TRUE),
            .groups = "drop") %>%
  rename(year = decade)

rate_births_decade_alt <- data_full %>%
  mutate(decade_alt = floor((year - 5) / 10) * 10 + 5) %>%
  group_by(country, decade_alt) %>%
  summarise(rate_per_100k_births = mean(rate_per_100k_births, na.rm = TRUE),
            .groups = "drop") %>%
  rename(year = decade_alt)

###############################################################################
# Sanity check plot
###############################################################################
countries_plot <- c("Russia", "Armenia", "Ukraine", "Georgia",
                    "Belarus", "Moldova", "Kazakhstan")

plot_data <- rate_births_decade %>%
  filter(country %in% countries_plot, year >= 1800, year <= 2000)

p_sanity <- ggplot(plot_data, aes(x = year, y = rate_per_100k_births, color = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1800, 2000, 20)) +
  labs(title = "Notable STEM Scientists per 100,000 Births",
       subtitle = "Decade averages, 1800–2000",
       x = "Decade of Birth", y = "STEM scientists per 100,000 births",
       color = "Country") +
  theme_bw(base_size = 13)

###############################################################################
# Build event-study panels (same facility selection as analysis_jan26.R)
###############################################################################
facility_event_year <- facilities %>%
  filter(!is.na(year)) %>%
  mutate(keep = case_when(
    country %in% c("United States of America", "Russia") ~ FALSE,
    country == "Australia" & facility == "Parkes Observatory"      ~ TRUE,
    country == "Chile"     & facility == "Cerro-Tolo Observatory"  ~ TRUE,
    country == "Armenia"   & facility == "Byurakan Observatory"    ~ TRUE,
    TRUE ~ TRUE
  )) %>%
  filter(keep) %>%
  group_by(country) %>%
  summarise(event_year = min(year, na.rm = TRUE),
            facility   = first(facility), .groups = "drop") %>%
  mutate(event_decade     = floor(event_year / 10) * 10,
         event_decade_alt = floor((event_year - 5) / 10) * 10 + 5)

panel_es <- data_full %>%
  left_join(facility_event_year, by = "country") %>%
  mutate(event_year = if_else(is.na(event_year), 0L, event_year)) %>%
  filter(year > 1850) %>%
  mutate(country_id = as.numeric(factor(country))) %>%
  select(country_id, year, event_year, rate_per_100k_births)

panel_es_decade <- rate_births_decade %>%
  left_join(facility_event_year %>% select(country, event_decade), by = "country") %>%
  mutate(event_year = if_else(is.na(event_decade), 0L, event_decade),
         country_id = as.numeric(factor(country))) %>%
  select(country_id, country, year, event_year, rate_per_100k_births)

panel_es_decade_alt <- rate_births_decade_alt %>%
  left_join(facility_event_year %>% select(country, event_decade_alt), by = "country") %>%
  mutate(event_year = if_else(is.na(event_decade_alt), 0L, event_decade_alt),
         country_id = as.numeric(factor(country))) %>%
  select(country_id, country, year, event_year, rate_per_100k_births)

###############################################################################
# Event studies (Callaway-Sant'Anna)
###############################################################################
ESgraph <- function(data, type, window, control) {
  out <- att_gt(yname         = "rate_per_100k_births",
                tname         = "year",
                idname        = "country_id",
                gname         = "event_year",
                data          = data,
                control_group = control,
                est_method    = "dr",
                base_period   = "universal",
                cores         = 4)
  es <- aggte(out, type = type, na.rm = TRUE,
              min_e = -window, max_e = window)
  ggdid(es) + labs(x = "Relative Time", y = "Effect on STEM rate per 100k births")
}

p_es_yearly      <- ESgraph(panel_es,           "dynamic", 20, "notyettreated")
p_es_decade      <- ESgraph(panel_es_decade,    "dynamic", 80, "notyettreated")
p_es_decade_alt  <- ESgraph(panel_es_decade_alt,"dynamic", 80, "notyettreated")

# Cohort-level ATT
out_dec <- att_gt(yname = "rate_per_100k_births", tname = "year",
                  idname = "country_id", gname = "event_year",
                  data = panel_es_decade, control_group = "notyettreated",
                  est_method = "dr", base_period = "universal", cores = 4)
p_ggdid_dec <- ggdid(out_dec)

out_dec_alt <- att_gt(yname = "rate_per_100k_births", tname = "year",
                      idname = "country_id", gname = "event_year",
                      data = panel_es_decade_alt, control_group = "notyettreated",
                      est_method = "dr", base_period = "universal", cores = 4)
p_ggdid_dec_alt <- ggdid(out_dec_alt)

###############################################################################
# Save outputs
###############################################################################
ggsave("results/stem_ES_dynamic_annual.png",      p_es_yearly,     width=8, height=6, dpi=300)
ggsave("results/stem_ES_dynamic_decade.png",      p_es_decade,     width=8, height=6, dpi=300)
ggsave("results/stem_ES_dynamic_decade_alt.png",  p_es_decade_alt, width=8, height=6, dpi=300)
ggsave("results/stem_ggdid_decade.png",           p_ggdid_dec,     width=8, height=6, dpi=300)
ggsave("results/stem_ggdid_decade_alt.png",       p_ggdid_dec_alt, width=8, height=6, dpi=300)
ggsave("results/stem_sanity_check.png",           p_sanity,        width=8, height=6, dpi=300)

cat("All STEM big-projects figures saved to results/ with stem_ prefix.\n")
final_time <- Sys.time() - initial_time
print(paste("Ran in", round(final_time, 1), "seconds."))
