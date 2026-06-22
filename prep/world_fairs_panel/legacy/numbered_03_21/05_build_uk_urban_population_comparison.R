###############################################################################
# Build and geocode the Law-Robson-Bennett urban population panel, then compare
# it with Populations Past registration sub-district populations, 1851-1911.
#
# Run from the repository root or from prep/world_fairs_panel/:
#   Rscript prep/world_fairs_panel/05_build_uk_urban_population_comparison.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(sf)
})

###############################################################################
# Paths
###############################################################################

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
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
    TALENT_DETS_DATA_DIR <- normalizePath(
      user_data_dir,
      winslash = "/",
      mustWork = TRUE
    )
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  }
}

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
populations_past_dir <- file.path(gbr_dir, "raw", "populations_past")
results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results",
  "world_fairs",
  "uk_population_source_comparison"
)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

law_file <- file.path(gbr_dir, "law_robson_bennet_standard_csv.csv")
points_file <- file.path(gbr_dir, "SettlementPoints_csv.csv")

panel_out <- file.path(
  gbr_dir,
  "city_population_law_robson_bennett_1801_1911_geocoded.csv"
)
comparison_out <- file.path(
  gbr_dir,
  "law_robson_vs_populations_past_comparison.csv"
)
audit_out <- file.path(
  gbr_dir,
  "law_robson_vs_populations_past_match_audit.csv"
)
summary_out <- file.path(
  gbr_dir,
  "law_robson_vs_populations_past_summary.csv"
)

scatter_out <- file.path(results_dir, "law_robson_vs_populations_past_scatter.png")
ratio_out <- file.path(results_dir, "law_robson_vs_populations_past_ratio.png")
difference_out <- file.path(results_dir, "law_robson_vs_populations_past_difference.png")

required_files <- c(
  law_file,
  points_file,
  file.path(
    populations_past_dir,
    sprintf("PopulationsPast_census_data_%s.csv", seq(1851L, 1911L, by = 10L))
  )
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

###############################################################################
# Helpers and reviewed aliases
###############################################################################

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z0-9]+", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}

county_aliases <- c(
  "CAERNARVONSHIRE" = "CARNARVONSHIRE",
  "DEVONSHIRE" = "DEVON",
  "DORSETSHIRE" = "DORSET",
  "RUTLANDSHIRE" = "RUTLAND",
  "SOMERSETSHIRE" = "SOMERSET",
  "WESTMORELAND" = "WESTMORLAND"
)

apply_alias <- function(x, aliases) {
  hit <- match(x, names(aliases))
  replace <- !is.na(hit)
  x[replace] <- unname(aliases[hit[replace]])
  x
}

# These are spelling or naming variants where the Populations Past unit is a
# defensible counterpart. Composite settlements are deliberately excluded.
town_aliases <- c(
  "BERWICK" = "BERWICK UPON TWEED",
  "BOLTON" = "GREAT BOLTON",
  "BURTON ON TRENT" = "BURTON UPON TRENT",
  "CAERNARVON" = "CARNARVON",
  "DALTON IN FURNESS" = "DALTON",
  "GRAYS THURROCK" = "GRAYS",
  "GUISEBOROUGH" = "GUISBOROUGH",
  "HARTLEPOOLS" = "HARTLEPOOL",
  "HAZEL GROVE" = "HAZELGROVE",
  "HEOLSTON" = "HELSTON",
  "HUCKNALL" = "HUCKNALL TORKARD",
  "KING S LYNN" = "KINGS LYNN",
  "LLANELLI" = "LLANELLY",
  "OTLY" = "OTLEY",
  "ROYAL LEAMINGTON SPA" = "LEAMINGTON",
  "ROYAL TUNBRIDGE WELLS" = "TUNBRIDGE WELLS",
  "SOWEBY" = "SOWERBY",
  "STOCKTON ON TEES" = "STOCKTON",
  "TONBRIDGE" = "TUNBRIDGE",
  "WHITLEY BAY" = "WHITLEY",
  "WHITTICK" = "WHITWICK"
)

safe_correlation <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2L || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  cor(x[ok], y[ok])
}

###############################################################################
# Law-Robson-Bennett population panel
###############################################################################

cat("Reading Law-Robson-Bennett population data...\n")
law <- fread(law_file, na.strings = c("", "NA"), encoding = "UTF-8")

required_law <- c(
  "COUNTY", "TOWN", "STANDARD_NAME", "STANDARD NAME WITH COUNTY",
  as.character(seq(1801L, 1911L, by = 10L)), "alltownsdictID", "Newstandard"
)
missing_law <- setdiff(required_law, names(law))
if (length(missing_law) > 0L) {
  stop("Missing Law-Robson-Bennett columns: ", paste(missing_law, collapse = ", "))
}

law[, city_id := suppressWarnings(as.integer(alltownsdictID))]
law <- law[!is.na(city_id)]
if (nrow(law) != 934L || uniqueN(law$city_id) != 934L) {
  stop("Expected 934 unique Law-Robson-Bennett city IDs; found ",
       nrow(law), " rows and ", uniqueN(law$city_id), " IDs.")
}

law_cities <- law[, .(
  city_id,
  town_name = as.character(TOWN),
  standard_name = as.character(STANDARD_NAME),
  standard_name_with_county = as.character(`STANDARD NAME WITH COUNTY`),
  historic_county = as.character(COUNTY),
  source_geocode_key = as.character(Newstandard)
)]

law_cities[, `:=`(
  town_name_normalized = normalize_text(town_name),
  standard_name_normalized = normalize_text(standard_name),
  county_normalized = normalize_text(historic_county)
)]
law_cities[, county_match_name := apply_alias(county_normalized, county_aliases)]
law_cities[, town_match_name := apply_alias(town_name_normalized, town_aliases)]
law_cities[, population_match_method := fifelse(
  town_match_name == town_name_normalized,
  "exact_town_and_county",
  "reviewed_town_alias_and_county"
)]

population_years <- as.character(seq(1801L, 1911L, by = 10L))
law_wide <- law[, c("city_id", population_years), with = FALSE]
law_long <- melt(
  law_wide,
  id.vars = "city_id",
  measure.vars = population_years,
  variable.name = "census_year",
  value.name = "population",
  variable.factor = FALSE
)
law_long[, `:=`(
  census_year = as.integer(census_year),
  population = suppressWarnings(as.numeric(population))
)]

###############################################################################
# Settlement Points geocoding
###############################################################################

cat("Geocoding towns with Settlement Points...\n")
points <- fread(points_file, na.strings = c("", "NA"), encoding = "UTF-8")
required_points <- c("FID", "TOWN_NAME", "COUNTY", "NEWSTANDAR", "X_COORD", "Y_COORD")
missing_points <- setdiff(required_points, names(points))
if (length(missing_points) > 0L) {
  stop("Missing Settlement Points columns: ", paste(missing_points, collapse = ", "))
}

points[, `:=`(
  point_id = suppressWarnings(as.integer(FID)),
  point_key = as.character(NEWSTANDAR),
  easting = suppressWarnings(as.numeric(X_COORD)),
  northing = suppressWarnings(as.numeric(Y_COORD))
)]
points <- points[!is.na(point_id) & !is.na(easting) & !is.na(northing)]

law_cities[, geocode_lookup_key := source_geocode_key]
law_cities[standard_name == "COUNDON", geocode_lookup_key := "COUNDON.DURHAM"]
law_cities[standard_name == "COUNDON GRANGE", geocode_lookup_key := "COUNDON GRANGE.DURHAM"]
law_cities[standard_name == "MOUTAIN ASH",
           geocode_lookup_key := "MOUNTAIN ASH.GLOMORGANSHIRE"]
law_cities[standard_name == "NEWARK",
           geocode_lookup_key := "NEWARK-ON-TRENT.NOTTINGHAMSHIRE"]
law_cities[standard_name == "CRADLEY",
           geocode_lookup_key := "CRADLEY.WORCESTESHIRE"]

point_lookup <- points[, .(
  geocode_candidate_count = .N,
  point_id = if (.N == 1L) point_id[[1L]] else NA_integer_,
  settlement_point_name = if (.N == 1L) as.character(TOWN_NAME[[1L]]) else NA_character_,
  settlement_point_county = if (.N == 1L) as.character(COUNTY[[1L]]) else NA_character_,
  easting = if (.N == 1L) easting[[1L]] else NA_real_,
  northing = if (.N == 1L) northing[[1L]] else NA_real_
), by = .(geocode_lookup_key = point_key)]

law_cities <- merge(
  law_cities,
  point_lookup,
  by = "geocode_lookup_key",
  all.x = TRUE,
  sort = FALSE
)
law_cities[is.na(geocode_candidate_count), geocode_candidate_count := 0L]

shared_source_keys <- law_cities[, .N, by = source_geocode_key][N > 1L, source_geocode_key]
law_cities[, geocode_method := fcase(
  geocode_candidate_count == 0L, "unmatched",
  geocode_candidate_count > 1L, "ambiguous_settlement_point_key",
  geocode_lookup_key != source_geocode_key, "reviewed_key_correction",
  source_geocode_key %chin% shared_source_keys, "exact_shared_standard_key",
  default = "exact_standard_key"
)]
law_cities[, geocode_status := fifelse(
  geocode_candidate_count == 1L,
  "matched",
  fifelse(geocode_candidate_count > 1L, "ambiguous", "unmatched")
)]

matched_points <- law_cities[geocode_status == "matched"]
matched_sf <- st_as_sf(
  matched_points,
  coords = c("easting", "northing"),
  crs = 27700,
  remove = FALSE
)
matched_wgs84 <- st_transform(matched_sf, 4326)
coordinates_wgs84 <- st_coordinates(matched_wgs84)
matched_points[, `:=`(
  longitude = coordinates_wgs84[, "X"],
  latitude = coordinates_wgs84[, "Y"]
)]

law_cities[, `:=`(longitude = NA_real_, latitude = NA_real_)]
law_cities[matched_points, on = "city_id", `:=`(
  longitude = i.longitude,
  latitude = i.latitude
)]

invalid_coordinates <- law_cities[
  geocode_status == "matched" &
    (!between(longitude, -8.5, 2.5) | !between(latitude, 49.0, 56.5))
]
if (nrow(invalid_coordinates) > 0L) {
  stop("Settlement Points produced coordinates outside England and Wales for city IDs: ",
       paste(invalid_coordinates$city_id, collapse = ", "))
}

panel <- merge(law_long, law_cities, by = "city_id", all.x = TRUE, sort = FALSE)
panel[, `:=`(
  country_iso3 = "GBR",
  country_name = "United Kingdom",
  population_available = !is.na(population),
  population_source = "Law-Robson-Bennett Urban Population Database",
  coordinate_crs = "EPSG:4326",
  source_coordinate_crs = "EPSG:27700"
)]
setcolorder(panel, c(
  "country_iso3", "country_name", "city_id", "town_name", "standard_name",
  "historic_county", "census_year", "population", "population_available",
  "longitude", "latitude", "easting", "northing", "geocode_status",
  "geocode_method", "geocode_candidate_count", "point_id",
  "settlement_point_name", "settlement_point_county", "source_geocode_key",
  "geocode_lookup_key", "coordinate_crs", "source_coordinate_crs",
  "population_source", "standard_name_with_county", "town_name_normalized",
  "standard_name_normalized", "county_normalized", "county_match_name",
  "town_match_name", "population_match_method"
))
setorder(panel, city_id, census_year)

if (panel[, anyDuplicated(paste(city_id, census_year))] > 0L) {
  stop("Duplicate city_id/census_year rows in geocoded panel.")
}
if (panel[!is.na(population) & population < 0, .N] > 0L) {
  stop("Negative Law-Robson-Bennett population values found.")
}

###############################################################################
# Populations Past comparison
###############################################################################

cat("Reading Populations Past overlap years...\n")
overlap_years <- seq(1851L, 1911L, by = 10L)
pp_parts <- lapply(overlap_years, function(year) {
  path <- file.path(
    populations_past_dir,
    sprintf("PopulationsPast_census_data_%s.csv", year)
  )
  x <- fread(path, na.strings = c("", "NA"), encoding = "UTF-8")
  id_column <- sprintf("CEN_%s", year)
  required <- c(id_column, "REGCNTY", "REGDIST", "SUBDIST", "POP")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop("Missing columns in ", basename(path), ": ", paste(missing, collapse = ", "))
  }
  x <- x[!is.na(SUBDIST) & nzchar(trimws(as.character(SUBDIST)))]
  x[, .(
    census_year = year,
    pp_unit_id = as.character(get(id_column)),
    pp_subdistrict = as.character(SUBDIST),
    pp_registration_district = as.character(REGDIST),
    pp_registration_county = as.character(REGCNTY),
    pp_population = suppressWarnings(as.numeric(POP)),
    town_match_name = normalize_text(SUBDIST),
    county_match_name = normalize_text(REGCNTY)
  )]
})
pp <- rbindlist(pp_parts, use.names = TRUE)

pp_candidates <- pp[, .(
  pp_candidate_count = .N,
  pp_unit_id = if (.N == 1L) pp_unit_id[[1L]] else NA_character_,
  pp_subdistrict = if (.N == 1L) pp_subdistrict[[1L]] else NA_character_,
  pp_registration_district = if (.N == 1L) pp_registration_district[[1L]] else NA_character_,
  pp_registration_county = if (.N == 1L) pp_registration_county[[1L]] else NA_character_,
  pp_population = if (.N == 1L) pp_population[[1L]] else NA_real_
), by = .(census_year, town_match_name, county_match_name)]

law_overlap <- panel[
  census_year %in% overlap_years & population_available,
  .(
    city_id, town_name, standard_name, historic_county, census_year,
    law_robson_population = population, longitude, latitude,
    town_name_normalized, town_match_name, county_match_name,
    population_match_method
  )
]

audit <- merge(
  law_overlap,
  pp_candidates,
  by = c("census_year", "town_match_name", "county_match_name"),
  all.x = TRUE,
  sort = FALSE
)
audit[is.na(pp_candidate_count), pp_candidate_count := 0L]
audit[, match_status := fcase(
  pp_candidate_count == 1L, "matched",
  pp_candidate_count > 1L, "ambiguous_populations_past_candidates",
  default = "unmatched"
)]

audit[, pp_match_reuse_count := fifelse(
  match_status == "matched",
  .N,
  NA_integer_
), by = .(census_year, pp_unit_id)]
audit[match_status == "matched" & pp_match_reuse_count > 1L,
      match_status := "ambiguous_reused_populations_past_unit"]

comparison <- audit[match_status == "matched"]
comparison[, `:=`(
  population_difference = pp_population - law_robson_population,
  population_ratio_pp_to_law = pp_population / law_robson_population,
  population_pct_difference = 100 *
    (pp_population - law_robson_population) / law_robson_population,
  absolute_pct_difference = abs(100 *
    (pp_population - law_robson_population) / law_robson_population)
)]
setorder(comparison, census_year, city_id)
setorder(audit, census_year, city_id)

summary_by_year <- comparison[, .(
  law_cities_with_population = law_overlap[census_year == .BY$census_year, .N],
  matched_cities = .N,
  match_rate_pct = 100 * .N / law_overlap[census_year == .BY$census_year, .N],
  population_correlation = safe_correlation(law_robson_population, pp_population),
  median_ratio_pp_to_law = median(population_ratio_pp_to_law, na.rm = TRUE),
  median_absolute_pct_difference = median(absolute_pct_difference, na.rm = TRUE),
  exact_equal_count = sum(absolute_pct_difference < 1e-8, na.rm = TRUE),
  within_10_pct_count = sum(absolute_pct_difference <= 10, na.rm = TRUE),
  within_25_pct_count = sum(absolute_pct_difference <= 25, na.rm = TRUE)
), by = census_year]

overall_summary <- comparison[, .(
  census_year = NA_integer_,
  law_cities_with_population = nrow(law_overlap),
  matched_cities = .N,
  match_rate_pct = 100 * .N / nrow(law_overlap),
  population_correlation = safe_correlation(law_robson_population, pp_population),
  median_ratio_pp_to_law = median(population_ratio_pp_to_law, na.rm = TRUE),
  median_absolute_pct_difference = median(absolute_pct_difference, na.rm = TRUE),
  exact_equal_count = sum(absolute_pct_difference < 1e-8, na.rm = TRUE),
  within_10_pct_count = sum(absolute_pct_difference <= 10, na.rm = TRUE),
  within_25_pct_count = sum(absolute_pct_difference <= 25, na.rm = TRUE)
)]
summary_table <- rbindlist(list(summary_by_year, overall_summary), use.names = TRUE)
summary_table[, summary_scope := fifelse(is.na(census_year), "all_overlap_years", "census_year")]
setcolorder(summary_table, c("summary_scope", setdiff(names(summary_table), "summary_scope")))

###############################################################################
# Outputs and figures
###############################################################################

cat("Writing outputs...\n")
fwrite(panel, panel_out)
fwrite(comparison, comparison_out)
fwrite(audit, audit_out)
fwrite(summary_table, summary_out)

plot_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

p_scatter <- ggplot(
  comparison,
  aes(x = law_robson_population, y = pp_population)
) +
  geom_abline(slope = 1, intercept = 0, color = "grey45", linewidth = 0.5) +
  geom_point(alpha = 0.5, size = 1.2, color = "#2C6E9B") +
  scale_x_log10(labels = scales::label_number(big.mark = ",")) +
  scale_y_log10(labels = scales::label_number(big.mark = ",")) +
  facet_wrap(~ census_year) +
  labs(
    title = "Law-Robson-Bennett versus Populations Past",
    subtitle = "Matched town and registration sub-district populations",
    x = "Law-Robson-Bennett urban population (log scale)",
    y = "Populations Past sub-district population (log scale)"
  ) +
  plot_theme
ggsave(scatter_out, p_scatter, width = 11, height = 7.2, dpi = 300)

p_ratio <- ggplot(
  comparison,
  aes(x = factor(census_year), y = population_ratio_pp_to_law)
) +
  geom_hline(yintercept = 1, color = "grey45", linewidth = 0.5) +
  geom_boxplot(outlier.alpha = 0.18, fill = "#7FB3D5", width = 0.65) +
  scale_y_log10() +
  labs(
    title = "Ratio of Populations Past to urban population",
    subtitle = "Values above one indicate a larger registration sub-district geography",
    x = "Census year",
    y = "Population ratio (log scale)"
  ) +
  plot_theme
ggsave(ratio_out, p_ratio, width = 9.5, height = 5.8, dpi = 300)

difference_summary <- comparison[, .(
  median_pct_difference = median(population_pct_difference, na.rm = TRUE),
  p25_pct_difference = quantile(population_pct_difference, 0.25, na.rm = TRUE),
  p75_pct_difference = quantile(population_pct_difference, 0.75, na.rm = TRUE)
), by = census_year]

p_difference <- ggplot(
  difference_summary,
  aes(x = census_year, y = median_pct_difference)
) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.5) +
  geom_ribbon(
    aes(ymin = p25_pct_difference, ymax = p75_pct_difference),
    fill = "#A9CCE3",
    alpha = 0.55
  ) +
  geom_line(color = "#1F618D", linewidth = 0.8) +
  geom_point(color = "#1F618D", size = 2) +
  scale_x_continuous(breaks = overlap_years) +
  labs(
    title = "Percentage difference between population sources",
    subtitle = "Median and interquartile range: (Populations Past - urban) / urban",
    x = "Census year",
    y = "Population difference (%)"
  ) +
  plot_theme
ggsave(difference_out, p_difference, width = 9.5, height = 5.8, dpi = 300)

cat("\nCompleted UK urban population comparison.\n")
cat("Cities:", uniqueN(panel$city_id), "\n")
cat("City-year rows:", nrow(panel), "\n")
cat("Geocoded cities:", law_cities[geocode_status == "matched", .N], "\n")
cat("Unmatched geocodes:", law_cities[geocode_status == "unmatched", .N], "\n")
cat("Matched overlap observations:", nrow(comparison), "\n")
cat("Panel:", panel_out, "\n")
cat("Comparison:", comparison_out, "\n")
cat("Audit:", audit_out, "\n")
cat("Summary:", summary_out, "\n")
