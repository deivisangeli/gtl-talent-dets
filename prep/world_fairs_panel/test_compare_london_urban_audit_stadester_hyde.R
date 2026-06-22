###############################################################################
# Compare London Urban Audit population trajectories in Stadester rasters and
# HYDE, 1800-1960.
#
# Main comparison:
#   sum Stadester raster pixels and HYDE popc raster pixels within GISCO Urban
#   Audit 2018 London boundaries:
#     - GREATER_CITIES: UK001K2, London
#     - FUA: UK001L3, London
#
# Run from repo root or prep/world_fairs_panel/:
#   Rscript prep/world_fairs_panel/test_compare_london_urban_audit_stadester_hyde.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(giscoR)
  library(png)
  library(readxl)
  library(sf)
  library(terra)
})

sf::sf_use_s2(FALSE)

###############################################################################
# Paths
###############################################################################

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "paths.R"))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users",
    Sys.info()[["user"]],
    "Globtalent Dropbox",
    "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(user_data_dir, winslash = "/", mustWork = TRUE)
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

DATA_RESULTS <- file.path(TALENT_DETS_DATA_DIR, "results")

stadester_raw_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "stadester")
stadester_global_zip <- file.path(stadester_raw_dir, "stadester_population_rasters.zip")
stadester_metro_zip <- file.path(stadester_raw_dir, "metro_adjusted_rasters_and_json.zip")
stadester_metro_url <- "https://zenodo.org/api/records/17180328/files/metro_adjusted_rasters_and_json.zip/content"
stadester_metro_md5 <- "da2a093685514c706228e72719619266"
london_datastore_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "london_datastore")
london_census_url <- paste0(
  "https://data.london.gov.uk/download/expjm/",
  "2c7867e5-3682-4fdd-8b9d-c63e289b92a6/",
  "population%201801%20to%202021.xlsx"
)
london_census_file <- file.path(london_datastore_dir, "population_1801_to_2021.xlsx")
hyde_dir <- file.path(DATA_INPUT, "hyde_pop_asc")
gisco_cache_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "gisco")

out_csv <- file.path(DATA_OUTPUT, "world_fairs_london_urban_audit_stadester_hyde_1800_1960.csv")
out_geom_csv <- file.path(DATA_OUTPUT, "world_fairs_london_urban_audit_geometries_2018.csv")
out_census_sources_csv <- file.path(DATA_OUTPUT, "world_fairs_london_urban_audit_population_sources_1871_1961.csv")
out_census_comparison_csv <- file.path(DATA_OUTPUT, "world_fairs_london_urban_audit_census_comparison_1871_1961.csv")
out_fig <- file.path(DATA_RESULTS, "world_fairs", "london_urban_audit_stadester_hyde_population_1800_1960.png")
out_census_fig <- file.path(DATA_RESULTS, "world_fairs", "london_urban_audit_census_comparison_1871_1961.png")

dir.create(stadester_raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_OUTPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_fig), recursive = TRUE, showWarnings = FALSE)
dir.create(gisco_cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(london_datastore_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(london_census_file)) {
  download.file(london_census_url, london_census_file, mode = "wb", quiet = FALSE)
}

if (!file.exists(stadester_metro_zip)) {
  cat("Downloading Stadester metro-adjusted rasters and JSON...\n")
  download.file(stadester_metro_url, stadester_metro_zip, mode = "wb", quiet = FALSE)
}

actual_metro_md5 <- unname(tools::md5sum(stadester_metro_zip))
if (!identical(tolower(actual_metro_md5), stadester_metro_md5)) {
  stop(
    "Unexpected MD5 for Stadester metro-adjusted zip. Expected ",
    stadester_metro_md5,
    ", found ",
    actual_metro_md5
  )
}

stopifnot(file.exists(stadester_global_zip))
stopifnot(file.exists(stadester_metro_zip))
stopifnot(file.exists(london_census_file))
stopifnot(dir.exists(hyde_dir))

years_keep <- 1800L:1960L
raster_years_for_census <- seq(1800L, 1970L, by = 10L)
census_years_keep <- c(1871L, 1881L, 1891L, 1901L, 1911L, 1921L, 1931L, 1951L, 1961L)

###############################################################################
# Helpers
###############################################################################

interpolate_no_extrapolate <- function(year, population, years_out) {
  ok <- !is.na(year) & !is.na(population)
  year <- as.integer(year[ok])
  population <- as.numeric(population[ok])

  if (length(unique(year)) == 0) {
    return(rep(NA_real_, length(years_out)))
  }

  by_year <- data.table(year = year, population = population)[
    , .(population = sum(population, na.rm = TRUE)), by = year
  ]
  setorder(by_year, year)

  if (nrow(by_year) == 1) {
    out <- rep(NA_real_, length(years_out))
    out[years_out == by_year$year] <- by_year$population
    return(out)
  }

  approx(
    x = by_year$year,
    y = by_year$population,
    xout = years_out,
    rule = 1,
    ties = "ordered"
  )$y
}

population_status <- function(years_out, observed_years, interpolated_population, source_name) {
  fcase(
    years_out %in% observed_years, paste0("observed_", source_name, "_year"),
    !is.na(interpolated_population), paste0("linear_interpolation_between_", source_name, "_years"),
    default = paste0("missing_", source_name)
  )
}

read_stadester_rgba_raster <- function(zip_file, zip_entry) {
  entry_exists <- zip_entry %in% utils::unzip(zip_file, list = TRUE)$Name
  if (!entry_exists) {
    stop("Missing Stadester raster in zip: ", zip_entry)
  }

  extract_dir <- file.path(tempdir(), "stadester_rasters")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_file, files = zip_entry, exdir = extract_dir, overwrite = TRUE)
  png_file <- file.path(extract_dir, zip_entry)

  img <- png::readPNG(png_file)
  if (length(dim(img)) != 3L || dim(img)[3] != 4L) {
    stop("Expected RGBA PNG with 4 channels: ", png_file)
  }

  bytes <- round(img * 255)
  values <- 16777216 * bytes[, , 1] +
    65536 * bytes[, , 2] +
    256 * bytes[, , 3] +
    bytes[, , 4]

  terra::rast(
    values,
    extent = terra::ext(-180, 180, -90, 90),
    crs = "EPSG:4326"
  )
}

read_stadester_global_raster <- function(year) {
  zip_entry <- sprintf(
    "stadester_population_rasters/stadester_population_%s.png",
    year
  )
  read_stadester_rgba_raster(stadester_global_zip, zip_entry)
}

read_stadester_metro_base_raster <- function(year) {
  zip_entry <- sprintf(
    "stadester_base_rasters/stadester_base_%s.png",
    year
  )
  read_stadester_rgba_raster(stadester_metro_zip, zip_entry)
}

build_annual_series <- function(observed, source_label, source_id, source_key,
                                geometry_row, status_source) {
  out <- data.table(year = years_keep)
  out[, population := interpolate_no_extrapolate(
    year = observed$year,
    population = observed$population_observed,
    years_out = year
  )]
  out <- merge(out, observed, by = "year", all.x = TRUE)
  out[, `:=`(
    city = "London",
    source = source_label,
    source_id = source_id,
    source_key = source_key,
    urban_audit_level = geometry_row$urban_audit_level,
    urban_audit_code = geometry_row$urban_audit_code,
    urban_audit_name = geometry_row$urban_audit_name,
    area_km2 = geometry_row$area_km2,
    geometry_definition = geometry_row$geometry_definition,
    census_benchmark = geometry_row$census_benchmark,
    interpolation_status = population_status(
      year,
      observed$year,
      population,
      status_source
    )
  )]
  out[]
}

series_at_years <- function(observed, years_out, source_label, source_id,
                            source_key, geometry_row, status_source) {
  out <- data.table(year = years_out)
  out[, population := interpolate_no_extrapolate(
    year = observed$year,
    population = observed$population_observed,
    years_out = year
  )]
  out <- merge(out, observed[, .(year, population_observed)], by = "year", all.x = TRUE)
  out[, `:=`(
    city = "London",
    source = source_label,
    source_id = source_id,
    source_key = source_key,
    urban_audit_level = geometry_row$urban_audit_level,
    urban_audit_code = geometry_row$urban_audit_code,
    urban_audit_name = geometry_row$urban_audit_name,
    area_km2 = geometry_row$area_km2,
    geometry_definition = geometry_row$geometry_definition,
    census_benchmark = geometry_row$census_benchmark,
    interpolation_status = population_status(
      year,
      observed$year,
      population,
      status_source
    )
  )]
  out[]
}

extract_hyde_series <- function(geometry) {
  observed <- rbindlist(lapply(raster_years_for_census, function(y) {
    raster_file <- file.path(hyde_dir, sprintf("popc_%sAD.asc", y))
    if (!file.exists(raster_file)) {
      stop("Missing HYDE raster: ", raster_file)
    }

    r <- terra::rast(raster_file)
    r_crs <- terra::crs(r, proj = TRUE)
    geom <- geometry
    if (!is.na(r_crs) && nzchar(r_crs)) {
      geom <- st_transform(geom, r_crs)
    }

    data.table(
      year = y,
      population_observed = as.numeric(
        terra::extract(r, geom, fun = sum, exact = TRUE, na.rm = TRUE)[[2]]
      )
    )
  }))
  setorder(observed, year)
  observed
}

extract_stadester_series <- function(geometry, read_raster) {
  observed <- rbindlist(lapply(raster_years_for_census, function(y) {
    r <- read_raster(y)
    data.table(
      year = y,
      population_observed = as.numeric(
        terra::extract(r, geometry, fun = sum, exact = TRUE, na.rm = TRUE)[[2]]
      )
    )
  }))
  setorder(observed, year)
  observed
}

###############################################################################
# Urban Audit geometry and census
###############################################################################

cat("Loading GISCO Urban Audit 2018 London boundaries...\n")
urban_geometry_list <- lapply(
  list(
    list(level = "GREATER_CITIES", code = "UK001K2", census_benchmark = "greater_london_direct"),
    list(level = "FUA", code = "UK001L3", census_benchmark = "greater_london_not_directly_comparable")
  ),
  function(spec) {
    x <- giscoR::gisco_get_urban_audit(
      year = 2018,
      country = "UK",
      level = spec$level,
      cache_dir = gisco_cache_dir,
      update_cache = FALSE,
      verbose = FALSE
    ) |>
      st_make_valid()

    x <- x[x$URAU_CODE == spec$code & x$URAU_NAME == "London", ]
    if (nrow(x) != 1L) {
      stop("Expected one Urban Audit London row for ", spec$level, " / ", spec$code, ", found ", nrow(x))
    }

    x <- st_transform(x, 4326)
    x$urban_audit_level <- spec$level
    x$urban_audit_code <- x$URAU_CODE
    x$urban_audit_name <- x$URAU_NAME
    x$census_benchmark <- spec$census_benchmark
    x$area_km2 <- as.numeric(st_area(st_transform(x, 27700))) / 1e6
    x$geometry_definition <- paste0(
      "GISCO Urban Audit 2018 ",
      spec$level,
      " ",
      x$URAU_CODE,
      ": ",
      x$URAU_NAME
    )
    x
  }
)

urban_geometries_sf <- do.call(rbind, urban_geometry_list)
urban_geometries_dt <- as.data.table(st_drop_geometry(urban_geometries_sf))[
  , .(
    urban_audit_level,
    urban_audit_code,
    urban_audit_name,
    area_km2,
    geometry_definition,
    census_benchmark
  )
]
fwrite(urban_geometries_dt, out_geom_csv)

cat("Reading Greater London census population series...\n")
london_census_raw <- as.data.table(readxl::read_excel(london_census_file, sheet = "data"))
if (!("Greater London" %in% london_census_raw$area)) {
  stop("London census file is missing Greater London row.")
}

census_year_cols <- names(london_census_raw)[grepl("^[0-9]{4}$", names(london_census_raw))]
london_census_long <- melt(
  london_census_raw[area == "Greater London"],
  id.vars = "area",
  measure.vars = census_year_cols,
  variable.name = "year",
  value.name = "population"
)
london_census_long[, `:=`(
  year = as.integer(as.character(year)),
  population = as.numeric(population)
)]

london_census_observed <- london_census_long[
  year %in% census_years_keep,
  .(year, population)
]
setorder(london_census_observed, year)

if (!all(census_years_keep %in% london_census_observed$year)) {
  stop("Census years missing in Greater London row.")
}

###############################################################################
# Raster series
###############################################################################

cat("Extracting raster series for Urban Audit London boundaries...\n")
all_series <- list()
all_census_sources <- list()

for (i in seq_len(nrow(urban_geometries_sf))) {
  geom <- urban_geometries_sf[i, ]
  geom_row <- as.list(urban_geometries_dt[i])
  cat("  Geometry:", geom_row$urban_audit_level, geom_row$urban_audit_code, "\n")

  hyde_observed <- extract_hyde_series(geom)
  stadester_global_observed <- extract_stadester_series(geom, read_stadester_global_raster)
  stadester_metro_observed <- extract_stadester_series(geom, read_stadester_metro_base_raster)

  hyde <- build_annual_series(
    observed = hyde_observed,
    source_label = "HYDE raster",
    source_id = "hyde_raster",
    source_key = "HYDE popc decadal rasters",
    geometry_row = geom_row,
    status_source = "hyde"
  )

  stadester_global <- build_annual_series(
    observed = stadester_global_observed,
    source_label = "Stadester global raster",
    source_id = "stadester_global_raster",
    source_key = "stadester_population_rasters.zip",
    geometry_row = geom_row,
    status_source = "stadester_global_raster"
  )

  stadester_metro <- build_annual_series(
    observed = stadester_metro_observed,
    source_label = "Stadester metro-adjusted raster",
    source_id = "stadester_metro_adjusted_raster",
    source_key = "metro_adjusted_rasters_and_json.zip/stadester_base_rasters",
    geometry_row = geom_row,
    status_source = "stadester_metro_adjusted_raster"
  )

  all_series[[length(all_series) + 1L]] <- rbindlist(
    list(hyde, stadester_global, stadester_metro),
    use.names = TRUE
  )

  census_series <- data.table(
    city = "London",
    year = london_census_observed$year,
    source = "Census: Greater London",
    source_id = "census_greater_london",
    source_key = "London Datastore population-borough-1801-2021.xlsx",
    urban_audit_level = geom_row$urban_audit_level,
    urban_audit_code = geom_row$urban_audit_code,
    urban_audit_name = geom_row$urban_audit_name,
    area_km2 = geom_row$area_km2,
    geometry_definition = "Greater London row in London Datastore historical census table",
    census_benchmark = geom_row$census_benchmark,
    population = london_census_observed$population,
    population_observed = london_census_observed$population,
    interpolation_status = "observed_census_year"
  )

  hyde_census <- series_at_years(
    observed = hyde_observed,
    years_out = census_years_keep,
    source_label = "HYDE raster",
    source_id = "hyde_raster",
    source_key = "HYDE popc decadal rasters",
    geometry_row = geom_row,
    status_source = "hyde"
  )

  stadester_global_census <- series_at_years(
    observed = stadester_global_observed,
    years_out = census_years_keep,
    source_label = "Stadester global raster",
    source_id = "stadester_global_raster",
    source_key = "stadester_population_rasters.zip",
    geometry_row = geom_row,
    status_source = "stadester_global_raster"
  )

  stadester_metro_census <- series_at_years(
    observed = stadester_metro_observed,
    years_out = census_years_keep,
    source_label = "Stadester metro-adjusted raster",
    source_id = "stadester_metro_adjusted_raster",
    source_key = "metro_adjusted_rasters_and_json.zip/stadester_base_rasters",
    geometry_row = geom_row,
    status_source = "stadester_metro_adjusted_raster"
  )

  all_census_sources[[length(all_census_sources) + 1L]] <- rbindlist(
    list(census_series, hyde_census, stadester_global_census, stadester_metro_census),
    use.names = TRUE,
    fill = TRUE
  )
}

comparison <- rbindlist(all_series, use.names = TRUE, fill = TRUE)
setcolorder(
  comparison,
  c(
    "city", "year", "urban_audit_level", "urban_audit_code",
    "urban_audit_name", "area_km2", "source", "source_id",
    "population", "population_observed", "interpolation_status",
    "geometry_definition", "census_benchmark", "source_key"
  )
)
setorder(comparison, urban_audit_level, source_id, year)
fwrite(comparison, out_csv)

census_sources <- rbindlist(all_census_sources, use.names = TRUE, fill = TRUE)
setcolorder(
  census_sources,
  c(
    "city", "year", "urban_audit_level", "urban_audit_code",
    "urban_audit_name", "area_km2", "source", "source_id",
    "population", "population_observed", "interpolation_status",
    "geometry_definition", "census_benchmark", "source_key"
  )
)
setorder(census_sources, urban_audit_level, source_id, year)
fwrite(census_sources, out_census_sources_csv)

census_comparison <- dcast(
  census_sources,
  urban_audit_level + urban_audit_code + urban_audit_name + area_km2 + census_benchmark + year ~ source_id,
  value.var = "population"
)
setnames(
  census_comparison,
  old = c("census_greater_london", "hyde_raster", "stadester_global_raster", "stadester_metro_adjusted_raster"),
  new = c(
    "population_census_greater_london",
    "population_hyde_raster",
    "population_stadester_global_raster",
    "population_stadester_metro_adjusted_raster"
  )
)

census_comparison[, `:=`(
  direct_census_comparison = census_benchmark == "greater_london_direct",
  diff_hyde_minus_census = fifelse(
    census_benchmark == "greater_london_direct",
    population_hyde_raster - population_census_greater_london,
    NA_real_
  ),
  diff_stadester_global_raster_minus_census = fifelse(
    census_benchmark == "greater_london_direct",
    population_stadester_global_raster - population_census_greater_london,
    NA_real_
  ),
  diff_stadester_metro_adjusted_raster_minus_census = fifelse(
    census_benchmark == "greater_london_direct",
    population_stadester_metro_adjusted_raster - population_census_greater_london,
    NA_real_
  ),
  ratio_hyde_to_census = fifelse(
    census_benchmark == "greater_london_direct",
    population_hyde_raster / population_census_greater_london,
    NA_real_
  ),
  ratio_stadester_global_raster_to_census = fifelse(
    census_benchmark == "greater_london_direct",
    population_stadester_global_raster / population_census_greater_london,
    NA_real_
  ),
  ratio_stadester_metro_adjusted_raster_to_census = fifelse(
    census_benchmark == "greater_london_direct",
    population_stadester_metro_adjusted_raster / population_census_greater_london,
    NA_real_
  )
)]
setorder(census_comparison, urban_audit_level, year)
fwrite(census_comparison, out_census_comparison_csv)

###############################################################################
# Figures
###############################################################################

plot_dt <- copy(comparison)
plot_dt[, population_millions := population / 1e6]
plot_dt[, source_plot := fcase(
  source_id == "hyde_raster", "HYDE raster",
  source_id == "stadester_global_raster", "Stadester global raster",
  source_id == "stadester_metro_adjusted_raster", "Stadester metro-adjusted raster",
  default = source
)]
plot_dt[, geometry_plot := fcase(
  urban_audit_level == "GREATER_CITIES", "Urban Audit Greater City",
  urban_audit_level == "FUA", "Urban Audit FUA",
  default = urban_audit_level
)]

p <- ggplot(plot_dt, aes(x = year, y = population_millions, color = source_plot)) +
  geom_line(linewidth = 1) +
  geom_point(
    data = plot_dt[!is.na(population_observed)],
    aes(y = population_observed / 1e6),
    size = 1.0,
    alpha = 0.6
  ) +
  facet_wrap(~geometry_plot, scales = "free_y") +
  scale_color_manual(
    values = c(
      "HYDE raster" = "#D95F0E",
      "Stadester global raster" = "#2C7FB8",
      "Stadester metro-adjusted raster" = "#756BB1"
    )
  ) +
  labs(
    title = "London population, Urban Audit boundaries",
    subtitle = "Raster series sum pixels within GISCO Urban Audit 2018 boundaries; annual values are linearly interpolated.",
    x = NULL,
    y = "Population (millions)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    panel.grid.minor = element_blank()
  )

ggsave(out_fig, p, width = 11, height = 6.2, dpi = 300)

census_plot_dt <- copy(census_sources)
census_plot_dt[, population_millions := population / 1e6]
census_plot_dt[, source_plot := fcase(
  source_id == "census_greater_london", "Census: Greater London",
  source_id == "hyde_raster", "HYDE raster",
  source_id == "stadester_global_raster", "Stadester global raster",
  source_id == "stadester_metro_adjusted_raster", "Stadester metro-adjusted raster",
  default = source
)]
census_plot_dt[, geometry_plot := fcase(
  urban_audit_level == "GREATER_CITIES", "Urban Audit Greater City",
  urban_audit_level == "FUA", "Urban Audit FUA",
  default = urban_audit_level
)]

p_census <- ggplot(
  census_plot_dt,
  aes(x = year, y = population_millions, color = source_plot)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~geometry_plot, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Census: Greater London" = "#111111",
      "HYDE raster" = "#D95F0E",
      "Stadester global raster" = "#2C7FB8",
      "Stadester metro-adjusted raster" = "#756BB1"
    )
  ) +
  labs(
    title = "London Urban Audit rasters and Greater London census",
    subtitle = "Greater City is directly comparable to Greater London; FUA is larger and shown only as a reference.",
    x = NULL,
    y = "Population (millions)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    panel.grid.minor = element_blank()
  )

ggsave(out_census_fig, p_census, width = 11, height = 6.2, dpi = 300)

cat("\n=== London Urban Audit Stadester vs HYDE comparison complete ===\n")
cat("Rows written:", nrow(comparison), "\n")
cat("Urban Audit geometries:", nrow(urban_geometries_dt), "\n")
cat("Census comparison rows:", nrow(census_comparison), "\n")
cat("CSV:", out_csv, "\n")
cat("Geometry CSV:", out_geom_csv, "\n")
cat("Figure:", out_fig, "\n")
cat("Census source CSV:", out_census_sources_csv, "\n")
cat("Census comparison CSV:", out_census_comparison_csv, "\n")
cat("Census figure:", out_census_fig, "\n")
