###############################################################################
# Project: Determinants of Talent Production via Scientific Infrastructure 
# Author: Lucas Mattos and Deivis Angeli (GTF)
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
library("did")
library("tidygeocoder")
library("sf")
library("rnaturalearth")
library("fixest")

# Recording Initial Time
initial_time <- Sys.time()

###############################################################################
# Uploading databases
###############################################################################
data <- read_csv("Output/data_final_new.csv", show_col_types = FALSE)
facilities <- read_csv2("Output/scientific_facilities.csv",show_col_types = FALSE)

###############################################################################
# Ajusting the data
###############################################################################
# Filling the data with observations for every year in every country
data_full <- data %>%
 group_by(country, iso3) %>%
 complete(year = 1800:2015,
          fill = list(n_inventors = 0,rate_per_100k_births = 0, births =0)) %>%
 ungroup()

# Creating a decade dataframe
rate_births_decade <- data_full %>%
 mutate(decade = floor(year / 10) * 10) %>%
 group_by(country, decade) %>%
 summarise(rate_per_100k_births = mean(rate_per_100k_births, na.rm = TRUE),.groups = "drop") %>%
 rename(year = decade)

# Now with an alternative decade
rate_births_decade_alt <- data_full %>%
 mutate(decade_alt = floor((year - 5) / 10) * 10 + 5) %>%
 group_by(country, decade_alt) %>%
 summarise(rate_per_100k_births = mean(rate_per_100k_births, na.rm = TRUE),.groups = "drop") %>%
 rename(year = decade_alt)


###############################################################################
# Sanity Check Plotting
###############################################################################
countries_plot <- c( "Russia", "Armenia", "Ukraine", "Georgia", "Belarus", "Moldova", "Kazakhstan")

plot_data <- rate_births_decade %>%
 filter(
  country %in% countries_plot,
  year >= 1800,
  year <= 2000)

ggplot(plot_data,
       aes(x = year,
           y = rate_per_100k_births,
           color = country)) +
 geom_line(linewidth = 1) +
 geom_point(size = 2) +
 scale_x_continuous(breaks = seq(1800, 2000, 20)) +
 labs(
  title = "Notable Scientists per 100,000 Births",
  subtitle = "Decade averages, 1800–2000",
  x = "Decade of Birth",
  y = "Scientists per 100,000 births",
  color = "Country"
 ) +
 theme_bw(base_size = 13)


###############################################################################
# Adjusting the database to do the Event-Study
###############################################################################
# First we adjust to have just one facility for each country
facility_event_year <- facilities %>%
 filter(!is.na(year)) %>%
 mutate(keep = case_when(country %in% c("United States of America", "Russia") ~ FALSE,
   country == "Australia" & facility == "Parkes Observatory" ~ TRUE,
   country == "Chile" & facility == "Cerro-Tolo Observatory" ~ TRUE,
   country == "Armenia" & facility == "Byurakan Observatory" ~ TRUE,
   TRUE ~ TRUE)) %>%
 filter(keep) %>%
 group_by(country) %>%
 summarise(event_year = min(year, na.rm = TRUE),
           facility = first(facility),.groups = "drop") %>%
 mutate(event_decade = floor(event_year / 10) * 10,
        event_decade_alt = floor((event_year - 5) / 10) * 10 + 5)

# Now joining both datasets
panel_es <- data_full %>%
 left_join(facility_event_year, by = "country") %>%
 mutate(event_year = if_else(is.na(event_year), 0L, event_year)) %>%
 filter(year > 1850) %>%
 # Making numerical id to have the att
 mutate(country_id = as.numeric(factor(country))) %>%
 select(country_id,year,event_year,rate_per_100k_births)

# Now making for the decade
panel_es_decade <- rate_births_decade %>%
 left_join(facility_event_year %>% select(country, event_decade),
  by = "country") %>%
 mutate(event_year = if_else(is.na(event_decade), 0L, event_decade),
        country_id = as.numeric(factor(country))) %>%
 select(country_id,country,year,event_year,rate_per_100k_births)

# Alternative
panel_es_decade_alt <- rate_births_decade %>%
 left_join(facility_event_year %>% select(country, event_decade_alt),
           by = "country") %>%
 mutate(event_year = if_else(is.na(event_decade_alt), 0L, event_decade_alt),
        country_id = as.numeric(factor(country))) %>%
 select(country_id,country,year,event_year,rate_per_100k_births)

###############################################################################
# Event Studies
###############################################################################
# Creating function
ESgraph<-function(data, type, window, control ){
 out <- att_gt(yname = "rate_per_100k_births",
               tname = "year",
               idname = "country_id",
               gname = "event_year",
               data = data,
               control_group = control,
               est_method = "dr",
               base_period="universal",
               cores=4)
 
 es <- aggte(out, type = type,
             na.rm= TRUE,
             min_e = -window,
             max_e = window)
 
 plot=ggdid(es) + labs(x="Relative Time", y="Effect")
 print(plot)
}

p_es_yearly <- ESgraph(data=panel_es, type = "dynamic", window = 20, control= "notyettreated")

p_es_decade <- ESgraph(data=panel_es_decade, type = "dynamic", window = 80, control= "notyettreated")

p_es_decade_alt <- ESgraph(data=panel_es_decade_alt, type = "dynamic", window = 80, control= "notyettreated")

# Chart per cohort
out_dec <- att_gt(yname = "rate_per_100k_births",
              tname = "year",
              idname = "country_id",
              gname = "event_year",
              data = panel_es_decade,
              control_group = "notyettreated",
              est_method = "dr",
              base_period="universal",
              cores=4)

p_ggdid_dec <- ggdid(out_dec)

out_dec_alt <- att_gt(yname = "rate_per_100k_births",
                  tname = "year",
                  idname = "country_id",
                  gname = "event_year",
                  data = panel_es_decade_alt,
                  control_group = "notyettreated",
                  est_method = "dr",
                  base_period="universal",
                  cores=4)

p_ggdid_dec_alt <- ggdid(out_dec_alt)

###############################################################################
# Saving Outputs
###############################################################################
ggsave(filename = "Results/ES_dynamic_annual.png",plot= p_es_yearly, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "Results/ES_dynamic_decade.png",plot = p_es_decade, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "Results/ES_dynamic_decade_alt.png", plot = p_es_decade_alt,
       width = 8, height = 6, dpi = 300)

ggsave(filename = "Results/ggdid_decade.png", plot = p_ggdid_dec, width = 8,
       height = 6, dpi = 300)

ggsave(filename = "Results/ggdid_decade_alt.png",plot = p_ggdid_dec_alt, width = 8,
       height = 6, dpi = 300)