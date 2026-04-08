###############################################################################
# Project: Determinants of Talent Production in USSR 
# Author: Lucas Mattos
# Goal: Cleaning the Wikipedia Database
###############################################################################

# Clean the working environment
rm(list = ls())

# Packages
library("tidyverse")
library("ggplot2")
library("tidygeocoder")
library("sf")
library("rnaturalearth")

# Recording Initial Time
initial_time <- Sys.time()

###############################################################################
# Cleaning the Wikipedia database
## Already downloaded database from: 
## https://data.sciencespo.fr/dataset.xhtml?persistentId=doi:10.21410/7E4/RDAG3O
## The file is on the input folder in the project.
###############################################################################

# Downloading the data
raw_data <- read.csv("Input\\cross-verified-database.csv")

# Let's check the 49 variables available
names(raw_data)

# We are interested in the year of birth of individuals, so if this is not in the database
# I'll just drop the observation. The same for bplo1 and bpla1
data_clean <- raw_data %>%
 drop_na(birth, bplo1, bpla1) 


# Testing
## When doing this, we lose about 600.000 which is above 27%, but let's dive in to see which kind
## of observation is being deleted leveraging on the fact that we have the variable birth_min
raw_data1 <- raw_data %>%
 filter(birth_min >= 1900)

data_clean1 <- raw_data1 %>%
 drop_na(birth, bplo1, bpla1) 

rm(raw_data1, data_clean1)
## Now we are using loosing fewer observation, so we can keep working droping NAs from the 
## original dataset

# Plotting
plot_a <- ggplot(
 data_clean %>% filter(birth >= 1500),
 aes(x = birth)
) +
 geom_histogram(binwidth = 10, fill = "#67C090", color = "white") +
 labs(
  title = "Distribution of Birth Years (Discovery/Science, from 1500)",
  x = "Year of Birth",
  y = "Number of Individuals"
 ) +
 theme_minimal(base_size = 14)

###############################################################################
# Selecting the variables I think will be useful and filtering for Discovery/Science
###############################################################################
# Let's check the levels
unique(data_clean$level1_main_occ)
unique(data_clean$level2_main_occ)
#unique(data_clean$level3_main_occ)

# Filtering for birth year and occupation and selecting variables we will use
data_clean <- data_clean %>% 
 filter(level1_main_occ == "Discovery/Science") %>%
 filter(birth>= 1800) %>%
 select(wikidata_code,birth, death,bplo1, bpla1, citizenship_1_b)

# I used 1800 because is the minimum we have in the births database we will use later on

###############################################################################
# Adding country from coordinates
###############################################################################
# Getting the countries
world <- ne_countries(returnclass = "sf")

points <- st_as_sf(data_clean, coords = c("bplo1", "bpla1"), crs = 4326, remove = FALSE)

sf::sf_use_s2(FALSE)
points_with_country <- st_join(points, world[, c("iso_a3", "name")], join = st_within, left = TRUE)
points_with_country <- points_with_country %>%
 mutate(
  iso_a3 = case_when(
   iso_a3 == "-99" & grepl("France", name, ignore.case = TRUE) ~ "FRA",
   iso_a3 == "-99" & grepl("Norway", name, ignore.case = TRUE) ~ "NOR",
   iso_a3 == "-99" & grepl("Denmark", name, ignore.case = TRUE) ~ "DNK",
   iso_a3 == "-99" & grepl("Netherlands", name, ignore.case = TRUE) ~ "NLD",
   TRUE ~ iso_a3
  )
 )
sf::sf_use_s2(TRUE)

data_clean_final <- points_with_country %>%
 mutate(country = name, iso3 = iso_a3) %>%
 st_drop_geometry() %>%
 select(wikidata_code, birth, death, citizenship_1_b, country, iso3)


# Alternative Way, using API, can be more precise but way slower
#data_final <- data_clean %>%
# reverse_geocode(lat = bpla1, long = bplo1, method = "arcgis", full_results = T,
#                 num_threads = parallel::detectCores() - 1) %>%
# mutate(birth_country = CountryCode) %>%
# select(wikidata_code,birth,death,citizenship_1_b, birth_country)

# We want numbers aggregate by country and year, so we will have to aggregate.
## I will maintain the long for now because  we will use other variables following
data_clean_final_agg <- data_clean_final %>%
 filter(!is.na(iso3), !is.na(birth)) %>%
 transmute(iso3, country, year = as.integer(birth)) %>%
 count(iso3, country, year, name = "n_inventors")

###############################################################################
# Downloading the database for newbirths and child mortality/deaths data
## Already downloaded database from: 
## https://www.gapminder.org/data/
## The files are on the input folder in the project.
###############################################################################

new_births      <- read.csv("Input\\new_births_total_number_estimated.csv")
child_mortality <- read.csv("Input\\child_mortality_0_5_year_olds_dying_per_1000_born.csv")
child_deaths    <- read.csv("Input\\number_of_child_deaths.csv")

# Creating a function to clean the names of th variables and turn from wide to long dataframe
# so we can do operation correctly for country and year variables
to_long <- function(df, value_name){
 df %>%
  mutate(iso3 = toupper(geo)) %>%       
  pivot_longer(
   cols = matches("^X?\\d{4}$"),
   names_to = "year",
   values_to = value_name,
   names_transform = list(year = ~ as.integer(sub("^X", "", .x)))
  ) %>%
  transmute(iso3, country = name, year, !!value_name := .data[[value_name]])
}

# Using the function and creating one demographics database
nb_long  <- to_long(new_births,"births")
cm_long  <- to_long(child_mortality,"mort_per_1000")
cd_long  <- to_long(child_deaths,"child_deaths")

# Joining and creating variables of living people
demo_long <- nb_long %>%
 left_join(cm_long, by = c("iso3", "year")) %>%
 left_join(cd_long, by = c("iso3", "year")) %>%
 mutate(
  cohort_from_mortality = if_else(!is.na(births) & !is.na(mort_per_1000),
                                  pmax(births - round(births * (mort_per_1000/1000)), 0), NA_real_),
  cohort_from_deaths    = if_else(!is.na(births) & !is.na(child_deaths),
                                  pmax(births - child_deaths, 0), NA_real_)
 )

###############################################################################
# Joining with the cleaned Wikipedia dataset
###############################################################################

# Joining the inventors dataframe with the demographics and already calculating
# the frequency of inventors in the population
final_agg <- data_clean_final_agg %>%
 left_join(demo_long %>% select(iso3, year, births, cohort_from_mortality, cohort_from_deaths),
           by = c("iso3", "year")) %>%
 mutate(
  rate_per_1000_mort   = if_else(coalesce(cohort_from_mortality, 0) > 0,
                                 1e3 * n_inventors / cohort_from_mortality, NA_real_),
  rate_per_1000_deaths = if_else(coalesce(cohort_from_deaths, 0) > 0,
                                 1e3 * n_inventors / cohort_from_deaths,   NA_real_),
  rate_per_100k_births = if_else(cohort_from_mortality > 0,1e5 * n_inventors / births, NA_real_)
 ) %>%
 select(country, iso3, year, births, n_inventors, rate_per_1000_mort, rate_per_1000_deaths, rate_per_100k_births)

###############################################################################
# Making 3 panels
###############################################################################
## Absolute
abs_wide <- final_agg %>%
 select(country, year, n_inventors) %>%
 pivot_wider(
  names_from = year,
  values_from = n_inventors,
  values_fill = list(n_inventors = 0)
 )

year_cols <- sort(as.integer(setdiff(names(abs_wide), "country")))
abs_wide <- abs_wide %>%
 select(country, all_of(as.character(year_cols))) %>%
 arrange(country)

# Rate using mortality
rate_m_wide <- final_agg %>%
 select(country, year, rate_per_1000_mort) %>%
 pivot_wider(
  names_from = year,
  values_from = rate_per_1000_mort,
  values_fill = list(rate_per_1000_mort = 0)
 )

year_cols <- sort(as.integer(setdiff(names(rate_m_wide), "country")))
rate_m_wide <- rate_m_wide %>%
 select(country, all_of(as.character(year_cols))) %>%
 arrange(country)

# Rate using deaths
rate_d_wide <- final_agg %>%
 select(country, year, rate_per_1000_deaths) %>%
 pivot_wider(
  names_from = year,
  values_from = rate_per_1000_deaths,
  values_fill = list(rate_per_1000_deaths = 0)
 )

year_cols <- sort(as.integer(setdiff(names(rate_d_wide), "country")))
rate_d_wide <- rate_d_wide %>%
 select(country, all_of(as.character(year_cols))) %>%
 arrange(country)

# Rate using births
rate_births_wide <- final_agg %>%
 select(country, year, rate_per_100k_births) %>%
 pivot_wider(
  names_from = year,
  values_from = rate_per_100k_births,
  values_fill = list(rate_per_100k_births = 0)
 )

year_cols <- sort(as.integer(setdiff(names(rate_births_wide), "country")))

rate_births_wide <- rate_births_wide %>%
 select(country, all_of(as.character(year_cols))) %>%
 arrange(country)





###############################################################################
# Exporting
###############################################################################
write.csv(final_agg, "Output\\data_final.csv", row.names = FALSE)
write.csv(abs_wide, "Output\\abs_wide.csv", row.names = FALSE)
write.csv(rate_m_wide, "Output\\rate_m_wide.csv", row.names = FALSE)
write.csv(rate_d_wide, "Output\\rate_d_wide.csv", row.names = FALSE)
###############################################################################
# Recording Time
###############################################################################
final_time <- Sys.time() - initial_time

print(paste("This code ran in", round(final_time, 1), "seconds."))
