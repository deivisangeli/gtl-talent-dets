###############################################################################
# Compare London population trajectories in Stadester metro-adjusted rasters
# and HYDE, 1800-1960.
#
# Main comparison:
#   sum Stadester metro-adjusted base raster pixels and HYDE popc raster pixels
#   within Greater London LAU 2019 boundaries.
#
# Benchmark:
#   Stadester metro-adjusted JSON key "London-United Kingdom".
#
# Run from repo root or prep/world_fairs_panel/:
#   Rscript prep/world_fairs_panel/test_compare_london_stadester_hyde.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(giscoR)
  library(jsonlite)
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
stadester_metro_zip <- file.path(stadester_raw_dir, "metro_adjusted_rasters_and_json.zip")
stadester_metro_url <- "https://zenodo.org/api/records/17180328/files/metro_adjusted_rasters_and_json.zip/content"
stadester_metro_md5 <- "da2a093685514c706228e72719619266"
stadester_key <- "London-United Kingdom"
stadester_source_label <- "Stadester metro-adjusted raster: Greater London LAU 2019"
stadester_source_id <- "stadester_metro_adjusted_raster"
stadester_json_source_label <- "Stadester metro-adjusted JSON: London-United Kingdom"
stadester_json_source_id <- "stadester_metro_adjusted_json"
stadester_raster_source_key <- "metro_adjusted_rasters_and_json.zip/stadester_base_rasters"
london_datastore_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "london_datastore")
london_census_url <- paste0(
  "https://data.london.gov.uk/download/expjm/",
  "2c7867e5-3682-4fdd-8b9d-c63e289b92a6/",
  "population%201801%20to%202021.xlsx"
)
london_census_file <- file.path(london_datastore_dir, "population_1801_to_2021.xlsx")
hyde_dir <- file.path(DATA_INPUT, "hyde_pop_asc")
gisco_cache_dir <- file.path(TALENT_DETS_DATA_DIR, "raw", "gisco")

out_csv <- file.path(DATA_OUTPUT, "world_fairs_london_stadester_metro_adjusted_hyde_1800_1960.csv")
out_lau_csv <- file.path(DATA_OUTPUT, "world_fairs_london_greater_london_lau_2019.csv")
out_census_sources_csv <- file.path(DATA_OUTPUT, "world_fairs_london_population_sources_metro_adjusted_1871_1961.csv")
out_census_comparison_csv <- file.path(DATA_OUTPUT, "world_fairs_london_population_census_comparison_metro_adjusted_1871_1961.csv")
out_fig <- file.path(DATA_RESULTS, "world_fairs", "london_stadester_metro_adjusted_hyde_population_1800_1960.png")
out_census_fig <- file.path(DATA_RESULTS, "world_fairs", "london_census_stadester_metro_adjusted_hyde_population_1871_1961.png")

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

stopifnot(file.exists(stadester_metro_zip))
stopifnot(file.exists(london_census_file))
stopifnot(dir.exists(hyde_dir))

years_keep <- 1800L:1960L
raster_years <- seq(1800L, 1960L, by = 10L)
census_years_keep <- c(1871L, 1881L, 1891L, 1901L, 1911L, 1921L, 1931L, 1951L, 1961L)
raster_years_for_census <- seq(1800L, 1970L, by = 10L)

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

read_stadester_metro_base_raster <- function(zip_file, year) {
  zip_entry <- sprintf(
    "stadester_base_rasters/stadester_base_%s.png",
    year
  )
  read_stadester_rgba_raster(zip_file, zip_entry)
}

read_stadester_metro_json <- function(zip_file, json_entry = "stadester.json") {
  entry_exists <- json_entry %in% utils::unzip(zip_file, list = TRUE)$Name
  if (!entry_exists) {
    stop("Missing Stadester JSON in zip: ", json_entry)
  }

  extract_dir <- file.path(tempdir(), "stadester_metro_json")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_file, files = json_entry, exdir = extract_dir, overwrite = TRUE)
  jsonlite::fromJSON(file.path(extract_dir, json_entry), simplifyVector = FALSE)
}

###############################################################################
# Stadester London series
###############################################################################

cat("Reading Stadester metro-adjusted London series...\n")
stadester_json <- read_stadester_metro_json(stadester_metro_zip)
stopifnot(stadester_key %in% names(stadester_json))

london_stadester <- stadester_json[[stadester_key]]
stadester_observed_all <- data.table(
  year = as.integer(names(london_stadester$population)),
  population_observed = as.numeric(unlist(london_stadester$population, use.names = FALSE))
)
stadester_observed <- stadester_observed_all[year %in% years_keep]
setorder(stadester_observed, year)

stadester <- data.table(year = years_keep)
stadester[, population := interpolate_no_extrapolate(
  year = stadester_observed$year,
  population = stadester_observed$population_observed,
  years_out = year
)]
stadester <- merge(stadester, stadester_observed, by = "year", all.x = TRUE)
stadester[, `:=`(
  city = "London",
  source = stadester_json_source_label,
  source_key = stadester_key,
  geometry_definition = "Stadester city/agglomeration definition",
  interpolation_status = population_status(
    year,
    stadester_observed$year,
    population,
    "stadester"
  )
)]

###############################################################################
# Greater London geometry
###############################################################################

cat("Loading GISCO LAU 2019 for UK...\n")
uk_lau_2019 <- giscoR::gisco_get_lau(
  year = 2019,
  country = "UK",
  cache_dir = gisco_cache_dir,
  update_cache = FALSE,
  verbose = FALSE
) |>
  st_make_valid()

greater_london_codes <- sprintf("E090000%02d", 1:33)
uk_lau_2019$lau_code_clean <- sub("^UK_", "", uk_lau_2019$GISCO_ID)
greater_london_lau <- uk_lau_2019[uk_lau_2019$lau_code_clean %in% greater_london_codes, ]

if (nrow(greater_london_lau) != 33L) {
  stop("Expected 33 Greater London LAUs in GISCO 2019, found ", nrow(greater_london_lau))
}

greater_london_lau_dt <- as.data.table(st_drop_geometry(greater_london_lau))
fwrite(greater_london_lau_dt, out_lau_csv)

cat("Reading London census population series...\n")
london_census_raw <- as.data.table(readxl::read_excel(london_census_file, sheet = "data"))
greater_london_names <- greater_london_lau_dt$LAU_NAME
census_missing_laus <- setdiff(greater_london_names, london_census_raw$area)
census_extra_laus <- setdiff(london_census_raw[!is.na(area)]$area, c(greater_london_names, "Central London", "Rest of Inner London", "Outer London", "Greater London"))

if (length(census_missing_laus) > 0) {
  stop("London census file is missing Greater London LAU names: ", paste(census_missing_laus, collapse = ", "))
}

if (length(census_extra_laus) > 0) {
  cat("Ignoring non-LAU census rows:", paste(census_extra_laus, collapse = ", "), "\n")
}

census_year_cols <- names(london_census_raw)[grepl("^[0-9]{4}$", names(london_census_raw))]
london_census_borough <- london_census_raw[area %in% greater_london_names]

london_census_long <- melt(
  london_census_borough,
  id.vars = "area",
  measure.vars = census_year_cols,
  variable.name = "year",
  value.name = "population_borough"
)
london_census_long[, `:=`(
  year = as.integer(as.character(year)),
  population_borough = as.numeric(population_borough)
)]

london_census_borough_sum <- london_census_long[
  year %in% census_years_keep,
  .(population = sum(population_borough, na.rm = TRUE)),
  by = year
]

london_census_aggregate_long <- melt(
  london_census_raw[area == "Greater London"],
  id.vars = "area",
  measure.vars = census_year_cols,
  variable.name = "year",
  value.name = "population"
)
london_census_aggregate_long[, `:=`(
  year = as.integer(as.character(year)),
  population = as.numeric(population)
)]
london_census_observed <- london_census_aggregate_long[
  year %in% census_years_keep,
  .(year, population)
]
setorder(london_census_observed, year)

if (!all(census_years_keep %in% london_census_observed$year)) {
  stop("Census years missing in official Greater London aggregate row.")
}

census_rounding_check <- merge(
  london_census_observed,
  london_census_borough_sum,
  by = "year",
  suffixes = c("_official", "_borough_sum")
)
census_rounding_check[, diff := population_borough_sum - population_official]
if (any(census_rounding_check$diff != 0, na.rm = TRUE)) {
  cat(
    "Using official Greater London census total; borough sums differ by rounding in some years.\n"
  )
}

greater_london_geom <- greater_london_lau |>
  st_transform(27700) |>
  st_union() |>
  st_as_sf()
greater_london_geom$name <- "Greater London LAU 2019"
greater_london_geom <- greater_london_geom |>
  st_transform(4326) |>
  st_make_valid()

###############################################################################
# HYDE Greater London raster series
###############################################################################

cat("Extracting HYDE population for Greater London...\n")
hyde_observed <- rbindlist(lapply(raster_years_for_census, function(y) {
  raster_file <- file.path(hyde_dir, sprintf("popc_%sAD.asc", y))
  if (!file.exists(raster_file)) {
    stop("Missing HYDE raster: ", raster_file)
  }

  r <- terra::rast(raster_file)
  r_crs <- terra::crs(r, proj = TRUE)
  geom <- greater_london_geom
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
setorder(hyde_observed, year)

hyde <- data.table(year = years_keep)
hyde[, population := interpolate_no_extrapolate(
  year = hyde_observed$year,
  population = hyde_observed$population_observed,
  years_out = year
)]
hyde <- merge(hyde, hyde_observed, by = "year", all.x = TRUE)
hyde[, `:=`(
  city = "London",
  source = "HYDE raster: Greater London LAU 2019",
  source_key = "HYDE popc decadal rasters",
  geometry_definition = "Union of GISCO 2019 UK LAUs E09000001-E09000033",
  interpolation_status = population_status(
    year,
    hyde_observed$year,
    population,
    "hyde"
  )
)]

###############################################################################
# Stadester Greater London raster series
###############################################################################

cat("Extracting Stadester metro-adjusted base raster for Greater London...\n")
stadester_raster_observed <- rbindlist(lapply(raster_years_for_census, function(y) {
  r <- read_stadester_metro_base_raster(stadester_metro_zip, y)

  data.table(
    year = y,
    population_observed = as.numeric(
      terra::extract(r, greater_london_geom, fun = sum, exact = TRUE, na.rm = TRUE)[[2]]
    )
  )
}))
setorder(stadester_raster_observed, year)

stadester_raster <- data.table(year = years_keep)
stadester_raster[, population := interpolate_no_extrapolate(
  year = stadester_raster_observed$year,
  population = stadester_raster_observed$population_observed,
  years_out = year
)]
stadester_raster <- merge(stadester_raster, stadester_raster_observed, by = "year", all.x = TRUE)
stadester_raster[, `:=`(
  city = "London",
  source = stadester_source_label,
  source_key = stadester_raster_source_key,
  geometry_definition = "Union of GISCO 2019 UK LAUs E09000001-E09000033",
  interpolation_status = population_status(
    year,
    stadester_raster_observed$year,
    population,
    "stadester_raster"
  )
)]

###############################################################################
# Save comparison and figure
###############################################################################

comparison <- rbindlist(list(stadester_raster, hyde, stadester), use.names = TRUE)
setcolorder(
  comparison,
  c(
    "city", "year", "source", "population", "population_observed",
    "interpolation_status", "geometry_definition", "source_key"
  )
)
setorder(comparison, source, year)
fwrite(comparison, out_csv)

plot_dt <- copy(comparison)
plot_dt[, population_millions := population / 1e6]
plot_dt[, source_plot := fcase(
  source == stadester_source_label, "Stadester metro-adjusted raster",
  source == "HYDE raster: Greater London LAU 2019", "HYDE raster",
  source == stadester_json_source_label, "Stadester metro-adjusted JSON benchmark",
  default = source
)]

p <- ggplot(plot_dt, aes(x = year, y = population_millions, color = source_plot)) +
  geom_line(linewidth = 1) +
  geom_point(
    data = plot_dt[!is.na(population_observed)],
    aes(y = population_observed / 1e6),
    size = 1.2,
    alpha = 0.65
  ) +
  scale_color_manual(
    values = c(
      "Stadester metro-adjusted raster" = "#2C7FB8",
      "HYDE raster" = "#D95F0E",
      "Stadester metro-adjusted JSON benchmark" = "#666666"
    )
  ) +
  labs(
    title = "London population, Stadester metro-adjusted vs HYDE",
    subtitle = "Stadester base raster and HYDE series sum pixels within Greater London LAU 2019; annual values are linearly interpolated.",
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

ggsave(out_fig, p, width = 10, height = 5.4, dpi = 300)

###############################################################################
# Census-year comparison
###############################################################################

series_at_years <- function(observed, years_out, source_label, source_id,
                            source_key, geometry_definition, status_source) {
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
    geometry_definition = geometry_definition,
    interpolation_status = population_status(
      year,
      observed$year,
      population,
      status_source
    )
  )]
  out[]
}

census_series <- data.table(
  city = "London",
  year = london_census_observed$year,
  source = "Census: Greater London boroughs",
  source_id = "census",
  population = london_census_observed$population,
  population_observed = london_census_observed$population,
  interpolation_status = "observed_census_year",
  geometry_definition = "Sum of 33 Greater London boroughs/LAUs in London Datastore historical census table",
  source_key = "London Datastore population-borough-1801-2021.xlsx"
)

hyde_census <- series_at_years(
  observed = hyde_observed,
  years_out = census_years_keep,
  source_label = "HYDE raster: Greater London LAU 2019",
  source_id = "hyde_raster",
  source_key = "HYDE popc decadal rasters",
  geometry_definition = "Union of GISCO 2019 UK LAUs E09000001-E09000033",
  status_source = "hyde"
)

stadester_raster_census <- series_at_years(
  observed = stadester_raster_observed,
  years_out = census_years_keep,
  source_label = stadester_source_label,
  source_id = stadester_source_id,
  source_key = stadester_raster_source_key,
  geometry_definition = "Union of GISCO 2019 UK LAUs E09000001-E09000033",
  status_source = "stadester_raster"
)

stadester_json_census <- series_at_years(
  observed = stadester_observed_all[
    year >= min(census_years_keep) & year <= max(census_years_keep)
  ],
  years_out = census_years_keep,
  source_label = stadester_json_source_label,
  source_id = stadester_json_source_id,
  source_key = stadester_key,
  geometry_definition = "Stadester city/agglomeration definition",
  status_source = "stadester_json"
)

census_sources <- rbindlist(
  list(census_series, hyde_census, stadester_raster_census, stadester_json_census),
  use.names = TRUE,
  fill = TRUE
)
setcolorder(
  census_sources,
  c(
    "city", "year", "source", "source_id", "population", "population_observed",
    "interpolation_status", "geometry_definition", "source_key"
  )
)
setorder(census_sources, source_id, year)
fwrite(census_sources, out_census_sources_csv)

census_comparison <- dcast(
  census_sources,
  year ~ source_id,
  value.var = "population"
)
setnames(
  census_comparison,
  old = c("census", "hyde_raster", stadester_source_id, stadester_json_source_id),
  new = c(
    "population_census",
    "population_hyde_raster",
    "population_stadester_metro_adjusted_raster",
    "population_stadester_metro_adjusted_json"
  )
)

census_comparison[, `:=`(
  diff_hyde_minus_census = population_hyde_raster - population_census,
  diff_stadester_metro_adjusted_raster_minus_census = population_stadester_metro_adjusted_raster - population_census,
  diff_stadester_metro_adjusted_json_minus_census = population_stadester_metro_adjusted_json - population_census,
  ratio_hyde_to_census = population_hyde_raster / population_census,
  ratio_stadester_metro_adjusted_raster_to_census = population_stadester_metro_adjusted_raster / population_census,
  ratio_stadester_metro_adjusted_json_to_census = population_stadester_metro_adjusted_json / population_census
)]
setorder(census_comparison, year)
fwrite(census_comparison, out_census_comparison_csv)

census_plot_dt <- copy(census_sources)
census_plot_dt[, population_millions := population / 1e6]
census_plot_dt[, source_plot := fcase(
  source_id == "census", "Census",
  source_id == "hyde_raster", "HYDE raster",
  source_id == stadester_source_id, "Stadester metro-adjusted raster",
  source_id == stadester_json_source_id, "Stadester metro-adjusted JSON benchmark",
  default = source
)]

p_census <- ggplot(
  census_plot_dt,
  aes(x = year, y = population_millions, color = source_plot)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "Census" = "#111111",
      "HYDE raster" = "#D95F0E",
      "Stadester metro-adjusted raster" = "#2C7FB8",
      "Stadester metro-adjusted JSON benchmark" = "#666666"
    )
  ) +
  labs(
    title = "London census population vs Stadester metro-adjusted and HYDE",
    subtitle = "Census uses Greater London; raster series sum pixels within Greater London LAU 2019.",
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

ggsave(out_census_fig, p_census, width = 10, height = 5.4, dpi = 300)

cat("\n=== London Stadester metro-adjusted vs HYDE comparison complete ===\n")
cat("Rows written:", nrow(comparison), "\n")
cat("Stadester raster observed years:", nrow(stadester_raster_observed), "\n")
cat("Stadester JSON observed years:", nrow(stadester_observed), "\n")
cat("HYDE observed years:", nrow(hyde_observed), "\n")
cat("Census comparison years:", length(census_years_keep), "\n")
cat("Greater London LAUs:", nrow(greater_london_lau), "\n")
cat("CSV:", out_csv, "\n")
cat("LAU definition CSV:", out_lau_csv, "\n")
cat("Figure:", out_fig, "\n")
cat("Census source CSV:", out_census_sources_csv, "\n")
cat("Census comparison CSV:", out_census_comparison_csv, "\n")
cat("Census figure:", out_census_fig, "\n")
