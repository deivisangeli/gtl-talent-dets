###############################################################################
# Project: Determinants of Inventors
# Author: Lucas Mattos
# Goal: Downloading and cleaning the Wikipedia Database
###############################################################################

# Clean the working environment
rm(list = ls())

# Packages
library("tidyverse")
library("ggplot2")
library("tidygeocoder")
library("sf")
library("rnaturalearth")
library("readr")
library("data.table")
library("dataverse")
library("R.utils")
library("geodata")

# Recording Initial Time
initial_time <- Sys.time()

# Parameters
options(timeout = 600)

###############################################################################
# Importing the Database
###############################################################################
# Wikipedia
## Getting the URL for the SciencePo website with the database
url_wikipedia <- "https://data.sciencespo.fr/api/access/datafile/4432?format=original"
## Downloading a tempfile for the compressed folder
temp_file <- tempfile(fileext = ".gz")
download.file(url_wikipedia, temp_file, mode = "wb")
## Now reading the data, detecting the CSV format from the zip compression
raw_data <- fread(temp_file)


# New Births from GapMinder
url_new_births <- "https://raw.githubusercontent.com/open-numbers/ddf--gapminder--systema_globalis/master/countries-etc-datapoints/ddf--datapoints--new_births_total_number_estimated--by--geo--time.csv"
new_births_raw <- fread(url_new_births)

###############################################################################
# Cleaning the Database
###############################################################################
# We are interested in the year of birth of individuals, so if this is not in the database
# I'll just drop the observation. The same for bplo1 and bpla1
data_clean <- raw_data %>%
 drop_na(birth, bplo1, bpla1)

# Selecting the variables I think will be useful and filtering for Discovery/Science
# Let's check the levels
#unique(data_clean$level1_main_occ)
#unique(data_clean$level2_main_occ)
#unique(data_clean$level3_main_occ)

# Filtering for birth year and occupation and selecting variables we will use
## I used 1800 because is the minimum we have in the births database
data_clean <- data_clean %>%
 filter(level1_main_occ == "Discovery/Science") %>%
 filter(birth>= 1800) %>%
 select(wikidata_code,birth, death,bplo1, bpla1, citizenship_1_b)

###############################################################################
# Adding geography from the database
###############################################################################
world <- ne_countries(returnclass = "sf")

points <- st_as_sf(
 data_clean,
 coords = c("bplo1", "bpla1"),
 crs = 4326,
 remove = FALSE
)

sf::sf_use_s2(FALSE)
points_with_country <- st_join(
 points,
 world[, c("iso_a3", "name")],
 join = st_within,
 left = TRUE
)

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

iso3_list <- points_with_country %>%
 st_drop_geometry() %>%
 distinct(iso_a3) %>%
 filter(!is.na(iso_a3), iso_a3 != "-99") %>%
 pull(iso_a3)

adm1 <- map_dfr(iso3_list, function(cty) {
 message("Downloading admin-1 for ", cty)
 tryCatch(
  gadm(country = cty, level = 1, path = tempdir()) %>%
   st_as_sf() %>%
   select(
    iso3 = GID_0,
    region_id = GID_1,
    region_name = NAME_1
   ),
  error = function(e) NULL
 )
})

sf::sf_use_s2(FALSE)
points_with_region <- st_join(
 points_with_country,
 adm1,
 join = st_within,
 left = TRUE
)
sf::sf_use_s2(TRUE)
###############################################################################
# Joining with the dataset of births
###############################################################################
# Adjusting the births database
data_clean_final <- points_with_region %>%
 st_drop_geometry() %>%
 select(
  wikidata_code,
  birth,
  death,
  citizenship_1_b,
  country = name,
  iso3,
  region_id,
  region_name
 )


# Joining the inventors dataframe with the demographics and already calculating
# the frequency of inventors in the population
data_region_agg <- data_clean_final %>%
 filter(!is.na(region_id), !is.na(birth)) %>%
 transmute(
  iso3,
  region_id,
  region_name,
  year = as.integer(birth)
 ) %>%
 count(iso3, region_id, region_name, year, name = "n_inventors")


data_usa <- data_region_agg %>%
 filter(iso3 == "USA")



new_births <- new_births_raw %>%
 mutate(iso3 = toupper(geo)) %>%
 rename(
  births = new_births_total_number_estimated,
  year = time
 ) %>%
 select(-geo)

final_agg <- data_region_agg %>%
 left_join(new_births, by = c("iso3", "year")) %>%
 mutate(
  rate_per_100k_births = if_else(
   births > 0,
   1e5 * n_inventors / births,
   NA_real_
  )
 )
###############################################################################
# Exporting
###############################################################################
write.csv(final_agg, "output/data_final_new.csv", row.names = FALSE)
