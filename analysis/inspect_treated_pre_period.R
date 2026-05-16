suppressPackageStartupMessages(library(tidyverse))
source("../paths.R")

panel <- read_csv(file.path(DATA_OUTPUT, "us_panel_county_stem_1800.csv"), show_col_types=FALSE) %>%
  mutate(GEOID = str_pad(as.character(GEOID), 5, "left", "0"),
         decade = as.integer(decade))

pop <- read_csv(file.path(DATA_OUTPUT, "county_population.csv"), show_col_types=FALSE) %>%
  mutate(GEOID = str_pad(as.character(GEOID), 5, "left", "0"),
         decade = as.integer(decade))

panel <- panel %>% select(-population) %>%
  left_join(pop, by=c("GEOID","decade"))

us_births <- read_csv(file.path(DATA_INPUT, "new_births_total_number_estimated.csv"), show_col_types=FALSE) %>%
  filter(geo == "usa") %>% select(-geo, -name) %>%
  pivot_longer(everything(), names_to="year", values_to="b") %>%
  mutate(year=as.integer(year), decade=(year %/% 10)*10) %>%
  group_by(decade) %>% summarise(us_births_decade = sum(b, na.rm=TRUE), .groups="drop")

us_pop <- panel %>% group_by(decade) %>% summarise(us_pop_decade = sum(population, na.rm=TRUE), .groups="drop")

panel <- panel %>%
  left_join(us_births, by="decade") %>%
  left_join(us_pop, by="decade") %>%
  mutate(births_est = population * us_births_decade / us_pop_decade,
         stem_per_1000_pop = if_else(population > 0, 1000*n_stem/population, NA_real_),
         stem_per_1000_births = if_else(births_est > 0, 1000*n_stem/births_est, NA_real_))

cat("=== Kings County NY (Brooklyn) decadal STEM births ===\n")
panel %>%
  filter(GEOID == "36047", decade <= 1900) %>%
  select(decade, n_stem, population, source, births_est, stem_per_1000_pop, stem_per_1000_births) %>%
  print(n=20)
