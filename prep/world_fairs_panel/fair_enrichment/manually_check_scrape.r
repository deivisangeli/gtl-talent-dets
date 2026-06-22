#############################################################################################

###Manually check entries without number of visits

#############################################################################################

###Load packages
pacman::p_load(data.table, tidyverse, readr, readxl, writexl)

###Load paths
source(file.path(Sys.getenv("GTL_REPO"), "paths.R"))
fairs_dir <- file.path(TALENT_DETS_DATA_DIR, "input/worlds_fairs")

###Check files within the fairs directory
fairs <- read.csv(file.path(fairs_dir, "worlds_fairs_visits_1790_1910.csv"))



#############################################################################################

###Select a sample of 10 filled entries and perform a qaulity check on the sources

#############################################################################################
set.seed(1914)
sample_filled_visits <- filled_visits %>%
  sample_n(10)

#############################################################################################

###Select a sample of 10 missing entries and perform a quality check on the sources

############################################################################################
set.seed(1914)
sample_missing_visits <- missing_visits %>%
  sample_n(10)

###Input values based on the row id
setDT(fairs)  
fairs[row_id == 187, visits := 2.7e6]
fairs[row_id == 120, visits := 20e3]

##Update source_tier and source_status to "found"
fairs[row_id %in% c(187, 120), source_tier := "found"]
fairs[row_id %in% c(187, 120), source_status := "found"]

###Save new version of fairs
write_csv(fairs, file.path(fairs_dir, "worlds_fairs_visits_1790_1910.csv"))

###Select entries with missing values
missing_visits <- fairs %>%
  filter(is.na(visits))

###Select entries with filled values
filled_visits <- fairs %>%
  filter(!is.na(visits))

###Store an xlsx file with the missing values
write_xlsx(missing_visits, file.path(fairs_dir, "worlds_fairs_visits_1790_1910_missing_visits.xlsx"))



#############################################################################################

###Second iteration: Data with geocoded fair venues

#############################################################################################




