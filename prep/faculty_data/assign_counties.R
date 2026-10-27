#!/usr/bin/env Rscript
# Spatially check local GeoNames populated-place candidates against the project's
# fixed 2020 county geometry. Input/output paths are supplied by the Python build.
suppressPackageStartupMessages({library(data.table); library(sf)})
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args) == 3L)
points <- fread(args[[1]], colClasses=list(character=c('geonameid', 'admin_county')))
counties <- st_read(args[[2]], quiet=TRUE)
sf_use_s2(FALSE)
geometry <- st_as_sf(points, coords=c('longitude','latitude'), crs=4326, remove=FALSE)
hits <- st_intersects(geometry, st_transform(counties,4326))
points[, polygon_GEOID := vapply(hits, function(ix) if(length(ix)==1L) as.character(counties$GEOID[ix]) else '', character(1))]
points[, polygon_matches := lengths(hits)]
fwrite(points,args[[3]],na='')
