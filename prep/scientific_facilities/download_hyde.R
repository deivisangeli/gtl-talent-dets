###############################################################################
# Project: Determinants of Talent Production
# Goal: Download HYDE 3.3 rasters via pastclim and export ASC files.
#        All exported rasters are stored under DET_DIR/input.
###############################################################################

rm(list = ls())

library("terra")
library("pastclim")

options(timeout = 600)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "prep", "raw_paths.R"))

###############################################################################
# 1. Configure pastclim data path
###############################################################################
cache_dir <- raw_dir("pastclim")
set_data_path(cache_dir, ask = FALSE)

###############################################################################
# 2. Download HYDE 3.3 baseline variables
###############################################################################
hyde_specs <- list(
 population = list(
  var_name    = "population",
  file_prefix = "popc",
  local_dir   = hyde_input_dir(),
  remote_dir  = hyde_input_dir(),
  copy_remote = FALSE
 ),
 cropland = list(
  var_name    = "cropland",
  file_prefix = "cropland",
  local_dir   = manual_input_path("hyde", "cropland"),
  remote_dir  = manual_input_path("hyde", "cropland"),
  copy_remote = FALSE
 ),
 grazeland = list(
  var_name    = "grazing_land",
  file_prefix = "grazeland",
  local_dir   = manual_input_path("hyde", "grazeland"),
  remote_dir  = manual_input_path("hyde", "grazeland"),
  copy_remote = FALSE
 )
)

message("Downloading HYDE 3.3 baseline data: population, cropland, and grazing_land...")
download_dataset(
 dataset       = "HYDE_3.3_baseline",
 bio_variables = vapply(hyde_specs, `[[`, character(1), "var_name")
)

###############################################################################
# 3. Extract decadal rasters (1800-2000) and save as ASC
###############################################################################
decades <- seq(1800, 2000, by = 10)
det_dir <- require_det_dir()

resolve_output_dir <- function(spec) {
 if (nzchar(det_dir) && !isTRUE(spec$copy_remote)) {
  return(spec$remote_dir)
 }
 spec$local_dir
}

export_hyde_variable <- function(spec_name, spec) {
 output_dir <- resolve_output_dir(spec)
 dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

 for (d in decades) {
  asc_path <- file.path(output_dir, paste0(spec$file_prefix, "_", d, "AD.asc"))

  if (file.exists(asc_path)) {
   message(spec_name, " ", d, "AD -- already exists, skipping.")
   next
  }

  message(spec_name, " ", d, "AD -- extracting...")
  r <- region_slice(
   time_ce       = d,
   bio_variables = spec$var_name,
   dataset       = "HYDE_3.3_baseline"
  )
  writeRaster(r, asc_path, filetype = "AAIGrid", overwrite = TRUE)
  message(
   spec_name, " ", d, "AD -- done (",
   round(file.size(asc_path) / 1e6, 1), " MB)"
  )
 }

 if (isTRUE(spec$copy_remote) && nzchar(det_dir)) {
  dir.create(spec$remote_dir, recursive = TRUE, showWarnings = FALSE)
  local_files <- list.files(
   spec$local_dir,
   pattern = paste0("^", spec$file_prefix, "_.*\\.asc$"),
   full.names = TRUE
  )

  for (f in local_files) {
   dest <- file.path(spec$remote_dir, basename(f))
   file.copy(f, dest, overwrite = TRUE)
  }

  message(
   "Copied ", length(local_files), " ",
   spec_name, " ASC files to Dropbox: ", spec$remote_dir
  )
 }
}

for (spec_name in names(hyde_specs)) {
 export_hyde_variable(spec_name, hyde_specs[[spec_name]])
}

###############################################################################
# 4. Verify
###############################################################################
for (spec_name in names(hyde_specs)) {
 spec <- hyde_specs[[spec_name]]
 check_dir <- resolve_output_dir(spec)
 asc_files <- list.files(
  check_dir,
  pattern = paste0("^", spec$file_prefix, "_.*\\.asc$")
 )
 message("\nDone. ", length(asc_files), " ", spec_name, " ASC files in ", check_dir, "/")
}
