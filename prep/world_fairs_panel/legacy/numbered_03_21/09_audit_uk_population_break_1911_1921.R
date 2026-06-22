###############################################################################
# Audit the 1911-1921 population transition between Law-Robson and Nomis.
#
# The 1921 Nomis CR03 table reports both 1911 and 1921 population for each
# listed administrative unit. This permits a decomposition of the observed
# Law-Robson-to-Nomis change into geography/source mismatch in 1911 and growth
# within the Nomis comparison geography from 1911 to 1921.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/09_audit_uk_population_break_1911_1921.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
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
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(
      user_data_dir, winslash = "/", mustWork = TRUE
    )
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  }
}

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
nomis_1921_dir <- file.path(
  gbr_dir, "raw", "nomis_historical_census", "1921", "extracted"
)
results_dir <- file.path(
  TALENT_DETS_DATA_DIR,
  "results",
  "world_fairs",
  "uk_population_1911_1921_audit"
)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

law_panel_file <- file.path(
  gbr_dir, "city_population_law_robson_bennett_1801_1911_geocoded.csv"
)
spatial_panel_file <- file.path(
  gbr_dir, "city_population_nomis_1921_1961_spatial.csv"
)
benchmark_file <- file.path(
  gbr_dir, "nomis_urban_units_1911_1921_benchmark.csv"
)
audit_file <- file.path(
  gbr_dir, "law_robson_nomis_1911_1921_quality_audit.csv"
)
summary_file <- file.path(
  gbr_dir, "law_robson_nomis_1911_1921_quality_summary.csv"
)
outliers_file <- file.path(
  gbr_dir, "law_robson_nomis_1911_1921_quality_outliers.csv"
)
source_scatter_file <- file.path(
  results_dir, "law_robson_vs_nomis_1911_scatter.png"
)
growth_plot_file <- file.path(
  results_dir, "nomis_growth_1911_1921_distribution.png"
)

cr03_candidates <- list.files(
  nomis_1921_dir,
  pattern = "1921_cr03_values[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)
cr03_candidates <- cr03_candidates[
  !grepl("__MACOSX", cr03_candidates, fixed = TRUE)
]

if (length(cr03_candidates) != 1L) {
  stop("Expected exactly one extracted 1921 Nomis CR03 values file.")
}
required_files <- c(law_panel_file, spatial_panel_file, cr03_candidates)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

###############################################################################
# Helpers
###############################################################################

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("[^A-Z0-9]+", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}

safe_weighted_ratio <- function(numerator, denominator) {
  keep <- !is.na(numerator) & !is.na(denominator) & denominator > 0
  if (!any(keep)) return(NA_real_)
  sum(numerator[keep]) / sum(denominator[keep])
}

safe_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  unname(quantile(x, probability, na.rm = TRUE))
}

urban_types <- c(
  "Urban District",
  "Municipal Borough",
  "County Borough",
  "Metropolitan Borough",
  "County Corporate",
  "London County Corporate"
)

###############################################################################
# Nomis benchmark units
###############################################################################

cat("Reading the 1921 CR03 comparison columns...\n")
cr03 <- fread(cr03_candidates[[1L]], na.strings = c("", "NA", ".."))
required_cr03_columns <- c(
  "area_id", "area", "area_type", "2c3_0002", "2c3_0003"
)
missing_cr03_columns <- setdiff(required_cr03_columns, names(cr03))
if (length(missing_cr03_columns) > 0L) {
  stop("Missing CR03 columns: ", paste(missing_cr03_columns, collapse = ", "))
}

benchmark <- cr03[area_type %chin% urban_types, .(
  source_area_id = as.character(area_id),
  source_area_name = as.character(area),
  source_area_type = as.character(area_type),
  nomis_population_1911 = as.numeric(get("2c3_0002")),
  nomis_population_1921 = as.numeric(get("2c3_0003"))
)]
benchmark[, `:=`(
  source_area_name_normalized = normalize_text(source_area_name),
  nomis_growth_ratio_1911_1921 = nomis_population_1921 / nomis_population_1911,
  nomis_growth_pct_1911_1921 = 100 *
    (nomis_population_1921 / nomis_population_1911 - 1),
  nomis_log_growth_1911_1921 = log(
    nomis_population_1921 / nomis_population_1911
  ),
  nomis_growth_outlier = nomis_population_1921 / nomis_population_1911 < 0.8 |
    nomis_population_1921 / nomis_population_1911 > 1.5
)]
setorder(benchmark, source_area_type, source_area_name, source_area_id)

if (nrow(benchmark) != 1154L) {
  stop("Expected 1,154 urban units in the 1921 CR03 benchmark.")
}
if (benchmark[, anyDuplicated(source_area_id)] > 0L) {
  stop("Duplicate urban source area IDs in the Nomis benchmark.")
}
if (benchmark[
  is.na(nomis_population_1911) | is.na(nomis_population_1921) |
    nomis_population_1911 <= 0 | nomis_population_1921 <= 0,
  .N
] > 0L) {
  stop("Missing or non-positive benchmark population values.")
}

###############################################################################
# Attach benchmark values to accepted spatial matches
###############################################################################

law_panel <- fread(law_panel_file, na.strings = c("", "NA"))
spatial_panel <- fread(spatial_panel_file, na.strings = c("", "NA"))

law_wide <- dcast(
  law_panel[
    census_year %chin% c(1901L, 1911L) & population_available == TRUE,
    .(city_id, town_name, standard_name, historic_county, census_year, population)
  ],
  city_id + town_name + standard_name + historic_county ~ census_year,
  value.var = "population"
)
setnames(law_wide, c("1901", "1911"), c(
  "law_robson_population_1901", "law_robson_population_1911"
))

spatial_1921 <- spatial_panel[
  census_year == 1921L & population_available == TRUE,
  .(
    city_id,
    spatial_population_1921 = population,
    source_area_id,
    source_area_name,
    source_area_type,
    source_county,
    population_match_method,
    population_match_status,
    boundary_id,
    boundary_name,
    boundary_type,
    spatial_text_agreement
  )
]
if (nrow(spatial_1921) != 754L) {
  stop("Expected 754 accepted spatial matches in 1921.")
}

# Expand composite source-area strings, attach each CR03 component, then sum
# both comparison years back to the Law-Robson city definition.
source_components <- spatial_1921[, .(
  component_source_area_id = trimws(unlist(strsplit(source_area_id, "\\|")))
), by = .(
  city_id,
  spatial_population_1921,
  source_area_id,
  source_area_name,
  source_area_type,
  source_county,
  population_match_method,
  population_match_status,
  boundary_id,
  boundary_name,
  boundary_type,
  spatial_text_agreement
)]
source_components <- merge(
  source_components,
  benchmark[, .(
    component_source_area_id = source_area_id,
    component_source_area_name = source_area_name,
    component_source_area_type = source_area_type,
    component_nomis_population_1911 = nomis_population_1911,
    component_nomis_population_1921 = nomis_population_1921
  )],
  by = "component_source_area_id",
  all.x = TRUE,
  sort = FALSE
)

city_benchmark <- source_components[, .(
  benchmark_component_count = .N,
  benchmark_components_found = sum(!is.na(component_nomis_population_1921)),
  benchmark_component_ids = paste(
    sort(unique(component_source_area_id)), collapse = " | "
  ),
  benchmark_component_names = paste(
    sort(unique(component_source_area_name)), collapse = " | "
  ),
  benchmark_component_types = paste(
    sort(unique(component_source_area_type)), collapse = " | "
  ),
  nomis_population_1911 = sum(component_nomis_population_1911),
  nomis_population_1921 = sum(component_nomis_population_1921)
), by = .(
  city_id,
  spatial_population_1921,
  source_area_id,
  source_area_name,
  source_area_type,
  source_county,
  population_match_method,
  population_match_status,
  boundary_id,
  boundary_name,
  boundary_type,
  spatial_text_agreement
)]

if (city_benchmark[
  benchmark_components_found != benchmark_component_count,
  .N
] > 0L) {
  stop("Failed to attach all Nomis benchmark components to accepted matches.")
}
if (city_benchmark[, anyDuplicated(city_id)] > 0L) {
  stop("Duplicate city IDs after aggregating Nomis benchmark components.")
}
if (city_benchmark[
  abs(nomis_population_1921 - spatial_population_1921) > 0.5,
  .N
] > 0L) {
  stop("Nomis benchmark 1921 values do not reproduce the spatial panel.")
}

audit <- merge(
  city_benchmark,
  law_wide,
  by = "city_id",
  all.x = TRUE,
  sort = FALSE
)
audit[, `:=`(
  town_name_normalized = normalize_text(town_name),
  source_area_name_normalized = normalize_text(source_area_name),
  geography_ratio_nomis_to_law_1911 =
    nomis_population_1911 / law_robson_population_1911,
  geography_difference_1911 =
    nomis_population_1911 - law_robson_population_1911,
  geography_difference_pct_1911 = 100 *
    (nomis_population_1911 / law_robson_population_1911 - 1),
  absolute_geography_difference_pct_1911 = abs(100 *
    (nomis_population_1911 / law_robson_population_1911 - 1)),
  nomis_growth_ratio_1911_1921 = nomis_population_1921 / nomis_population_1911,
  nomis_growth_pct_1911_1921 = 100 *
    (nomis_population_1921 / nomis_population_1911 - 1),
  observed_ratio_law_1911_to_nomis_1921 =
    nomis_population_1921 / law_robson_population_1911,
  observed_change_pct_law_1911_to_nomis_1921 = 100 *
    (nomis_population_1921 / law_robson_population_1911 - 1),
  law_growth_ratio_1901_1911 =
    law_robson_population_1911 / law_robson_population_1901,
  geography_log_component = log(
    nomis_population_1911 / law_robson_population_1911
  ),
  nomis_growth_log_component = log(
    nomis_population_1921 / nomis_population_1911
  ),
  observed_log_change = log(
    nomis_population_1921 / law_robson_population_1911
  )
)]
audit[, decomposition_identity_residual :=
  observed_ratio_law_1911_to_nomis_1921 -
    geography_ratio_nomis_to_law_1911 * nomis_growth_ratio_1911_1921]
audit[, log_decomposition_identity_residual :=
  observed_log_change - geography_log_component - nomis_growth_log_component]

audit[, geography_divergence_band := fcase(
  is.na(law_robson_population_1911), "missing_law_robson_1911",
  abs(geography_difference_pct_1911) <= 10, "within_10_pct",
  abs(geography_difference_pct_1911) <= 25, "10_to_25_pct",
  abs(geography_difference_pct_1911) <= 50, "25_to_50_pct",
  default = "over_50_pct"
)]
audit[, area_name_comparison := fcase(
  population_match_method == "reviewed_composite_aggregation", "composite_aggregation",
  source_area_name_normalized == town_name_normalized, "exact_town_name",
  source_area_name_normalized == normalize_text(standard_name), "exact_standard_name",
  mapply(
    function(town, area) nzchar(town) && nzchar(area) &&
      (grepl(town, area, fixed = TRUE) || grepl(area, town, fixed = TRUE)),
    town_name_normalized,
    source_area_name_normalized
  ), "partial_name_overlap",
  default = "different_name"
)]
audit[, nomis_growth_outlier :=
  nomis_growth_ratio_1911_1921 < 0.8 | nomis_growth_ratio_1911_1921 > 1.5]
audit[, review_priority := fcase(
  is.na(law_robson_population_1911), "missing_law_robson_1911",
  geography_divergence_band == "over_50_pct" & nomis_growth_outlier, "high_both",
  geography_divergence_band == "over_50_pct", "high_geography",
  nomis_growth_outlier, "high_growth",
  geography_divergence_band == "25_to_50_pct", "medium_geography",
  geography_divergence_band == "10_to_25_pct", "low_geography",
  default = "no_large_break"
)]
audit[, benchmark_interpretation := fcase(
  is.na(law_robson_population_1911),
    "Nomis benchmark available; Law-Robson 1911 missing",
  geography_divergence_band == "over_50_pct",
    "Large 1911 source/geography mismatch",
  geography_divergence_band == "25_to_50_pct",
    "Material 1911 source/geography mismatch",
  nomis_growth_outlier,
    "Comparable benchmark but unusual Nomis growth",
  default = "No large discontinuity detected"
)]

setcolorder(audit, c(
  "city_id", "town_name", "standard_name", "historic_county",
  "law_robson_population_1901", "law_robson_population_1911",
  "nomis_population_1911", "nomis_population_1921",
  "geography_ratio_nomis_to_law_1911", "geography_difference_1911",
  "geography_difference_pct_1911", "absolute_geography_difference_pct_1911",
  "nomis_growth_ratio_1911_1921",
  "nomis_growth_pct_1911_1921", "observed_ratio_law_1911_to_nomis_1921",
  "observed_change_pct_law_1911_to_nomis_1921",
  "law_growth_ratio_1901_1911", "geography_log_component",
  "nomis_growth_log_component", "observed_log_change",
  "decomposition_identity_residual", "log_decomposition_identity_residual",
  "geography_divergence_band", "nomis_growth_outlier",
  "area_name_comparison", "review_priority", "benchmark_interpretation",
  setdiff(names(audit), c(
    "city_id", "town_name", "standard_name", "historic_county",
    "law_robson_population_1901", "law_robson_population_1911",
    "nomis_population_1911", "nomis_population_1921",
                  "geography_ratio_nomis_to_law_1911", "geography_difference_1911",
                  "geography_difference_pct_1911",
                  "absolute_geography_difference_pct_1911",
                  "nomis_growth_ratio_1911_1921",
    "nomis_growth_pct_1911_1921", "observed_ratio_law_1911_to_nomis_1921",
    "observed_change_pct_law_1911_to_nomis_1921",
    "law_growth_ratio_1901_1911", "geography_log_component",
    "nomis_growth_log_component", "observed_log_change",
    "decomposition_identity_residual", "log_decomposition_identity_residual",
    "geography_divergence_band", "nomis_growth_outlier",
    "area_name_comparison", "review_priority", "benchmark_interpretation"
  ))
))
setorder(audit, -absolute_geography_difference_pct_1911, city_id)

###############################################################################
# Summary and review tables
###############################################################################

complete <- audit[!is.na(law_robson_population_1911)]
complete_1901 <- complete[!is.na(law_robson_population_1901)]

period_summary <- rbindlist(list(
  complete_1901[, .(
    section = "period_comparison",
    category = "Law-Robson 1901-1911",
    n = .N,
    share_pct = 100,
    median_ratio = median(law_growth_ratio_1901_1911),
    weighted_ratio = safe_weighted_ratio(
      law_robson_population_1911, law_robson_population_1901
    ),
    q05_ratio = safe_quantile(law_growth_ratio_1901_1911, 0.05),
    q95_ratio = safe_quantile(law_growth_ratio_1901_1911, 0.95)
  )],
  complete[, .(
    section = "period_comparison",
    category = "Observed Law-Robson 1911 to Nomis 1921",
    n = .N,
    share_pct = 100,
    median_ratio = median(observed_ratio_law_1911_to_nomis_1921),
    weighted_ratio = safe_weighted_ratio(
      nomis_population_1921, law_robson_population_1911
    ),
    q05_ratio = safe_quantile(observed_ratio_law_1911_to_nomis_1921, 0.05),
    q95_ratio = safe_quantile(observed_ratio_law_1911_to_nomis_1921, 0.95)
  )],
  complete[, .(
    section = "period_comparison",
    category = "Nomis benchmark 1911-1921",
    n = .N,
    share_pct = 100,
    median_ratio = median(nomis_growth_ratio_1911_1921),
    weighted_ratio = safe_weighted_ratio(
      nomis_population_1921, nomis_population_1911
    ),
    q05_ratio = safe_quantile(nomis_growth_ratio_1911_1921, 0.05),
    q95_ratio = safe_quantile(nomis_growth_ratio_1911_1921, 0.95)
  )],
  complete[, .(
    section = "period_comparison",
    category = "Nomis versus Law-Robson geography in 1911",
    n = .N,
    share_pct = 100,
    median_ratio = median(geography_ratio_nomis_to_law_1911),
    weighted_ratio = safe_weighted_ratio(
      nomis_population_1911, law_robson_population_1911
    ),
    q05_ratio = safe_quantile(geography_ratio_nomis_to_law_1911, 0.05),
    q95_ratio = safe_quantile(geography_ratio_nomis_to_law_1911, 0.95)
  )]
), use.names = TRUE)

band_summary <- audit[, .(
  n = .N,
  share_pct = 100 * .N / nrow(audit),
  median_ratio = median(geography_ratio_nomis_to_law_1911, na.rm = TRUE),
  weighted_ratio = safe_weighted_ratio(
    nomis_population_1911, law_robson_population_1911
  ),
  q05_ratio = safe_quantile(geography_ratio_nomis_to_law_1911, 0.05),
  q95_ratio = safe_quantile(geography_ratio_nomis_to_law_1911, 0.95)
), by = .(category = geography_divergence_band)]
band_summary[, section := "geography_divergence_band"]
setcolorder(band_summary, c("section", setdiff(names(band_summary), "section")))

priority_summary <- audit[, .(
  n = .N,
  share_pct = 100 * .N / nrow(audit),
  median_ratio = median(observed_ratio_law_1911_to_nomis_1921, na.rm = TRUE),
  weighted_ratio = safe_weighted_ratio(
    nomis_population_1921, law_robson_population_1911
  ),
  q05_ratio = safe_quantile(observed_ratio_law_1911_to_nomis_1921, 0.05),
  q95_ratio = safe_quantile(observed_ratio_law_1911_to_nomis_1921, 0.95)
), by = .(category = review_priority)]
priority_summary[, section := "review_priority"]
setcolorder(priority_summary, c("section", setdiff(names(priority_summary), "section")))

summary_table <- rbindlist(
  list(period_summary, band_summary, priority_summary),
  use.names = TRUE,
  fill = TRUE
)

outliers <- audit[
  review_priority %chin% c(
    "high_both", "high_geography", "high_growth", "missing_law_robson_1911"
  )
]
outliers[, review_priority_order := match(review_priority, c(
  "high_both", "high_geography", "high_growth", "missing_law_robson_1911"
))]
setorder(
  outliers,
  review_priority_order,
  -absolute_geography_difference_pct_1911,
  city_id
)
outliers[, review_priority_order := NULL]

###############################################################################
# Validation
###############################################################################

if (nrow(audit) != 754L || audit[, uniqueN(city_id)] != 754L) {
  stop("Quality audit must contain exactly 754 accepted city matches.")
}
if (audit[
  !is.na(law_robson_population_1911) &
    abs(decomposition_identity_residual) > 1e-10,
  .N
] > 0L) {
  stop("Multiplicative decomposition identity failed.")
}
if (audit[
  !is.na(law_robson_population_1911) &
    abs(log_decomposition_identity_residual) > 1e-10,
  .N
] > 0L) {
  stop("Log decomposition identity failed.")
}

review_cases <- c(
  "HARTLEPOOLS", "ESHER", "BROTTON", "BROSELEY", "CLECKHEATON"
)
missing_review_cases <- setdiff(review_cases, audit$town_name)
if (length(missing_review_cases) > 0L) {
  stop("Missing required review cases: ", paste(missing_review_cases, collapse = ", "))
}

###############################################################################
# Plots and outputs
###############################################################################

plot_data <- complete[
  law_robson_population_1911 > 0 & nomis_population_1911 > 0
]
source_scatter <- ggplot(
  plot_data,
  aes(x = law_robson_population_1911, y = nomis_population_1911)
) +
  geom_abline(slope = 1, intercept = 0, colour = "grey45", linewidth = 0.5) +
  geom_point(aes(colour = geography_divergence_band), alpha = 0.65, size = 1.6) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Law-Robson versus Nomis population in 1911",
    subtitle = "Nomis 1911 is the comparison column published in the 1921 CR03",
    x = "Law-Robson population, 1911 (log scale)",
    y = "Nomis comparison population, 1911 (log scale)",
    colour = "Absolute divergence"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

growth_plot <- ggplot(
  complete,
  aes(x = 100 * (nomis_growth_ratio_1911_1921 - 1))
) +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.5) +
  geom_histogram(binwidth = 5, fill = "#2C7FB8", colour = "white") +
  coord_cartesian(xlim = c(-50, 150)) +
  labs(
    title = "Population growth within Nomis comparison geography, 1911-1921",
    subtitle = "The plot is limited to -50% to +150%; all values remain in the audit CSV",
    x = "Nomis population change (%)",
    y = "Accepted Law-Robson city matches"
  ) +
  theme_minimal(base_size = 11)

cat("Writing benchmark audit outputs...\n")
fwrite(benchmark, benchmark_file)
fwrite(audit, audit_file)
fwrite(summary_table, summary_file)
fwrite(outliers, outliers_file)
ggsave(source_scatter_file, source_scatter, width = 8, height = 6, dpi = 300)
ggsave(growth_plot_file, growth_plot, width = 8, height = 6, dpi = 300)

cat("\nCompleted the Nomis 1911-1921 benchmark audit.\n")
cat("\nPeriod comparison:\n")
print(period_summary)
cat("\nGeography divergence bands:\n")
print(band_summary[order(category)])
cat("\nPriority review cases:\n")
print(audit[
  town_name %chin% review_cases,
  .(
    town_name,
    law_robson_population_1911,
    nomis_population_1911,
    nomis_population_1921,
    geography_ratio_nomis_to_law_1911,
    nomis_growth_ratio_1911_1921,
    observed_ratio_law_1911_to_nomis_1921,
    source_area_name,
    review_priority
  )
][order(town_name)])
cat("Benchmark units:", benchmark_file, "\n")
cat("City audit:", audit_file, "\n")
cat("Summary:", summary_file, "\n")
cat("Outliers:", outliers_file, "\n")
cat("Plots:", results_dir, "\n")
