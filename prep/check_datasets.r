###################################################################################################################
### Check HYDE and Wikipedia data
###################################################################################################################

rm(list = ls());gc()

library("tidyverse")
library("data.table")
library("terra")
library("R.utils")

#######################################################################################

###Get directories

#######################################################################################

###Dropbox repo
det_dir <- Sys.getenv("DET_DIR")

###Hyde data
hyde_dir <- file.path(det_dir, "input/hyde_pop_asc")

##List files in hyde_dir
hyde_files <- list.files(hyde_dir, pattern = "asc", full.names = TRUE)
hyde_files

###Open one file to check
hyde_file <- hyde_files[1]
hyde_rast <- rast(hyde_file)
hyde_rast

ds <- as.data.table(hyde_rast, xy = T, na.rm = F)

ds %>% filter(!is.na(popc_1800AD))

summary(ds$popc_1800AD)
