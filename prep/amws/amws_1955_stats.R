# AMWS 1955 vs Wikipedia STEM — descriptive comparison
# - Missing-data table for AMWS cleaned
# - Births per year (AMWS vs Wikipedia US STEM)
# - County-level birth counts side-by-side map

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(sf)
  library(tigris)
  library(patchwork)
})
options(tigris_use_cache = TRUE)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

out_dir <- file.path(AMWS_OUTPUT, "amws_1955_stats")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load -------------------------------------------------------------------
amws_clean <- fread(file.path(AMWS_OUTPUT, "amws_1955_cleaned.csv"))
amws_geo   <- fread(file.path(AMWS_OUTPUT, "amws_1955_us_geocoded_final.csv"))
wiki       <- fread(file.path(DATA_OUTPUT, "crossverified_with_stem.csv"))
wiki_us_stem <- wiki[iso3 == "USA" & stem == 1L]

# ---- Year parsing -----------------------------------------------------------
parse_amws_year <- function(date_str) {
  yr <- suppressWarnings(as.integer(sub(".*?([0-9]{1,4})\\s*$", "\\1", date_str)))
  out <- rep(NA_integer_, length(date_str))
  k4    <- !is.na(yr) & yr >= 1000
  k1900 <- !is.na(yr) & yr >= 0  & yr <= 40
  k1800 <- !is.na(yr) & yr >= 41 & yr <= 99
  out[k4]    <- yr[k4]
  out[k1900] <- 1900L + yr[k1900]
  out[k1800] <- 1800L + yr[k1800]
  out
}
amws_clean[, birth_year := parse_amws_year(date)]

# Join birth year onto geocoded table via lineid
amws_geo <- merge(amws_geo, amws_clean[, .(lineid, birth_year, country)],
                  by = "lineid", all.x = TRUE)

# ---- 1. Missing data by column (cleaned table) ------------------------------
miss_tbl <- amws_clean[, lapply(.SD, function(x) {
  is_missing <- is.na(x) | (is.character(x) & (x == "" | x == "NA"))
  sum(is_missing)
})]
miss_tbl <- data.table(column = names(miss_tbl),
                       n_missing = as.integer(miss_tbl[1]),
                       pct_missing = round(100 * as.integer(miss_tbl[1]) / nrow(amws_clean), 2))
fwrite(miss_tbl, file.path(out_dir, "missing_by_column.csv"))
cat("\n--- Missing data by column (amws_1955_cleaned.csv, n =", nrow(amws_clean), ") ---\n")
print(miss_tbl)

# ---- 2. Births per year -----------------------------------------------------
amws_yr <- amws_clean[!is.na(birth_year), .(amws = .N), by = birth_year][order(birth_year)]
wiki_yr <- wiki_us_stem[!is.na(birth), .(wiki_stem = .N), by = .(birth_year = birth)][order(birth_year)]

per_yr <- merge(amws_yr, wiki_yr, by = "birth_year", all = TRUE)
per_yr[is.na(amws), amws := 0L]
per_yr[is.na(wiki_stem), wiki_stem := 0L]
fwrite(per_yr, file.path(out_dir, "births_per_year.csv"))

# Plot window: 1820-1940 covers both
yr_long <- melt(per_yr[birth_year >= 1820 & birth_year <= 1940],
                id.vars = "birth_year",
                variable.name = "source", value.name = "n")
yr_long[, source := factor(source,
                           levels = c("amws", "wiki_stem"),
                           labels = c("AMWS 1955 (all entries)",
                                      "Wikipedia US-born STEM"))]

p_yr <- ggplot(yr_long, aes(birth_year, n, color = source)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("#1f77b4", "#d62728")) +
  labs(x = "Birth year", y = "Count", color = NULL,
       title = "Birth-year distribution: AMWS 1955 vs Wikipedia US STEM",
       subtitle = sprintf("AMWS parsed years: %d / %d cleaned rows | Wikipedia US STEM: %d",
                          sum(!is.na(amws_clean$birth_year)),
                          nrow(amws_clean),
                          nrow(wiki_us_stem))) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "births_per_year.png"), p_yr,
       width = 9, height = 5, dpi = 150)

cat("\n--- Year totals ---\n")
cat("AMWS rows with parsed year:", sum(!is.na(amws_clean$birth_year)),
    "/", nrow(amws_clean), "\n")
cat("AMWS year range (1%-99%):",
    paste(quantile(amws_clean$birth_year, c(.01,.99), na.rm = TRUE), collapse = " - "), "\n")
cat("Wiki US STEM year range (1%-99%):",
    paste(quantile(wiki_us_stem$birth, c(.01,.99), na.rm = TRUE), collapse = " - "), "\n")

# ---- 3. County map ----------------------------------------------------------
# Common window: AMWS 5%-95% percentile (covers bulk without tails)
yr_lo <- as.integer(quantile(amws_geo$birth_year, 0.01, na.rm = TRUE))
yr_hi <- as.integer(quantile(amws_geo$birth_year, 0.99, na.rm = TRUE))
cat(sprintf("\nMap window: %d-%d (AMWS 1%%-99%% percentile)\n", yr_lo, yr_hi))

# AMWS counts per county: count US-flagged geocoded births in window
amws_geo[, geoid5 := sprintf("%05d", as.integer(geoid))]
amws_county <- amws_geo[!is.na(birth_year) & birth_year >= yr_lo & birth_year <= yr_hi,
                       .(amws_n = .N), by = geoid5]

# Wikipedia STEM per county: sum n_stem from county panel over same window (decades)
wiki_panel <- fread(file.path(DATA_OUTPUT, "us_panel_county_stem_1790.csv"))
dec_lo <- (yr_lo %/% 10) * 10
dec_hi <- (yr_hi %/% 10) * 10
wiki_county <- wiki_panel[decade >= dec_lo & decade <= dec_hi,
                          .(wiki_n = sum(n_stem, na.rm = TRUE)),
                          by = .(geoid5 = sprintf("%05d", GEOID))]

cat(sprintf("AMWS counties with >=1 birth: %d  (total births: %d)\n",
            nrow(amws_county[amws_n > 0]), sum(amws_county$amws_n)))
cat(sprintf("Wiki county-decades window: %d-%d  (total STEM: %d)\n",
            dec_lo, dec_hi, sum(wiki_county$wiki_n)))

# Counties shapefile (CONUS only)
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020,
                        progress_bar = FALSE) |>
  st_transform(5070)
counties_sf <- counties_sf[!counties_sf$STUSPS %in% c("AK","HI","PR","VI","GU","AS","MP"), ]
counties_sf$geoid5 <- counties_sf$GEOID

m <- merge(counties_sf, amws_county, by = "geoid5", all.x = TRUE)
m <- merge(m, wiki_county, by = "geoid5", all.x = TRUE)
m$amws_n[is.na(m$amws_n)] <- 0
m$wiki_n[is.na(m$wiki_n)] <- 0

state_sf <- tigris::states(cb = TRUE, resolution = "20m", year = 2020,
                           progress_bar = FALSE) |>
  st_transform(5070)
state_sf <- state_sf[!state_sf$STUSPS %in% c("AK","HI","PR","VI","GU","AS","MP"), ]

make_map <- function(sf_obj, var, title) {
  v <- sf_obj[[var]]
  vv <- ifelse(v > 0, log10(v), NA_real_)
  sf_obj$.fill <- vv
  ggplot() +
    geom_sf(data = sf_obj, aes(fill = .fill), color = NA) +
    geom_sf(data = state_sf, fill = NA, color = "grey20", linewidth = 0.2) +
    scale_fill_viridis_c(option = "magma", na.value = "grey92",
                        name = "log10(n)") +
    labs(title = title,
         subtitle = sprintf("births %d-%d  |  counties with >=1: %d  |  total: %d",
                            yr_lo, yr_hi,
                            sum(v > 0), sum(v))) +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
}

p_amws <- make_map(m, "amws_n", "AMWS 1955")
p_wiki <- make_map(m, "wiki_n", "Wikipedia STEM")
p_both <- p_amws + p_wiki + plot_layout(ncol = 1)
ggsave(file.path(out_dir, "county_map_amws_vs_wiki.png"), p_both,
       width = 11, height = 11, dpi = 150)

# Correlation
both <- as.data.table(m)[, .(geoid5, amws_n, wiki_n)]
cor_pearson <- cor(both$amws_n, both$wiki_n)
cor_log <- cor(log1p(both$amws_n), log1p(both$wiki_n))
cor_rank <- cor(both$amws_n, both$wiki_n, method = "spearman")
cat(sprintf("\nCounty-level correlation AMWS vs Wiki STEM:\n  pearson: %.3f\n  log1p:   %.3f\n  spearman:%.3f\n",
            cor_pearson, cor_log, cor_rank))
fwrite(both, file.path(out_dir, "county_counts.csv"))

cat("\nFigures + tables written to", out_dir, "\n")
