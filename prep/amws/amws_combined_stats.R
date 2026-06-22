###############################################################################
# Combined AMWS (1906 + 1938 + 1955, post-dedup) descriptive stats:
#  - Coverage summary by edition
#  - Births per year vs Wikipedia US STEM
#  - County-level map: AMWS combined vs Wikipedia STEM
###############################################################################
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(sf); library(tigris)
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

out_dir <- file.path(AMWS_OUTPUT, "amws_combined_stats")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

amws <- fread(file.path(AMWS_OUTPUT, "amws_combined_us_geocoded.csv"))
amws_kept <- amws[kept == TRUE]
wiki <- fread(file.path(DATA_OUTPUT, "crossverified_with_stem.csv"))
wiki_us_stem <- wiki[iso3 == "USA" & stem == 1L]

# ---- 1. Coverage table ------------------------------------------------------
cov <- amws[, .(
  pre_dedup     = .N,
  post_dedup    = sum(kept),
  with_year     = sum(!is.na(birth_year) & kept),
  with_state    = sum(!is.na(state) & state != "" & kept),
  with_geoid    = sum(!is.na(geoid) & kept)
), by = edition][order(edition)]
total <- amws[, .(edition = "TOTAL",
                  pre_dedup = .N,
                  post_dedup = sum(kept),
                  with_year = sum(!is.na(birth_year) & kept),
                  with_state = sum(!is.na(state) & state != "" & kept),
                  with_geoid = sum(!is.na(geoid) & kept))]
cov_out <- rbind(cov[, edition := as.character(edition)], total)
fwrite(cov_out, file.path(out_dir, "coverage_by_edition.csv"))
cat("Coverage:\n"); print(cov_out)

# ---- 2. Births per year vs Wikipedia ---------------------------------------
amws_yr <- amws_kept[!is.na(birth_year), .(amws = .N), by = birth_year]
wiki_yr <- wiki_us_stem[!is.na(birth), .(wiki_stem = .N), by = .(birth_year = birth)]
per_yr <- merge(amws_yr, wiki_yr, by = "birth_year", all = TRUE)
per_yr[is.na(amws), amws := 0L]; per_yr[is.na(wiki_stem), wiki_stem := 0L]
setorder(per_yr, birth_year)
fwrite(per_yr, file.path(out_dir, "births_per_year.csv"))

yr_long <- melt(per_yr[birth_year >= 1820 & birth_year <= 1940],
                id.vars = "birth_year",
                variable.name = "source", value.name = "n")
yr_long[, source := factor(source, levels = c("amws", "wiki_stem"),
                           labels = c("AMWS combined (post-dedup)",
                                      "Wikipedia US-born STEM"))]
p_yr <- ggplot(yr_long, aes(birth_year, n, color = source)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +   # Okabe-Ito
  labs(x = "Birth year", y = "Count", color = NULL,
       title = "Birth-year distribution: AMWS combined vs Wikipedia US STEM",
       subtitle = sprintf("AMWS post-dedup with parsed year: %d / %d  |  Wikipedia US STEM: %d",
                          sum(!is.na(amws_kept$birth_year)),
                          nrow(amws_kept),
                          nrow(wiki_us_stem))) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(out_dir, "births_per_year.png"), p_yr,
       width = 9, height = 5, dpi = 150)

# ---- 3. Births per year by edition (stacked) -------------------------------
amws_yr_ed <- amws_kept[!is.na(birth_year), .N, by = .(birth_year, edition)]
amws_yr_ed[, edition := factor(edition, levels = c(1906, 1938, 1955))]
p_ed <- ggplot(amws_yr_ed[birth_year >= 1820 & birth_year <= 1940],
               aes(birth_year, N, fill = edition)) +
  geom_col(width = 1) +
  # Okabe-Ito: blue, bluish green, vermillion
  scale_fill_manual(values = c("1906" = "#0072B2",
                               "1938" = "#009E73",
                               "1955" = "#D55E00")) +
  labs(x = "Birth year", y = "Count (post-dedup)", fill = "First edition",
       title = "AMWS combined births by year, colored by earliest edition",
       subtitle = "Each scientist counted once; assigned to their earliest edition") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(out_dir, "births_per_year_by_edition.png"), p_ed,
       width = 9, height = 5, dpi = 150)

# ---- 4. County map ---------------------------------------------------------
yr_lo <- as.integer(quantile(amws_kept$birth_year, 0.01, na.rm = TRUE))
yr_hi <- as.integer(quantile(amws_kept$birth_year, 0.99, na.rm = TRUE))
cat(sprintf("\nMap window: %d-%d (AMWS combined 1%%-99%% percentile)\n", yr_lo, yr_hi))

amws_kept[, geoid5 := sprintf("%05d", as.integer(geoid))]
amws_county <- amws_kept[!is.na(birth_year) & birth_year >= yr_lo & birth_year <= yr_hi,
                         .(amws_n = .N), by = geoid5]

wiki_panel <- fread(file.path(DATA_OUTPUT, "us_panel_county_stem_1790.csv"))
dec_lo <- (yr_lo %/% 10) * 10
dec_hi <- (yr_hi %/% 10) * 10
wiki_county <- wiki_panel[decade >= dec_lo & decade <= dec_hi,
                          .(wiki_n = sum(n_stem, na.rm = TRUE)),
                          by = .(geoid5 = sprintf("%05d", GEOID))]

cat(sprintf("AMWS combined counties with >=1 birth: %d  (total: %d)\n",
            nrow(amws_county[amws_n > 0]), sum(amws_county$amws_n)))
cat(sprintf("Wiki county-decades %d-%d (total STEM: %d)\n",
            dec_lo, dec_hi, sum(wiki_county$wiki_n)))

counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020,
                        progress_bar = FALSE) |> st_transform(5070)
counties_sf <- counties_sf[!counties_sf$STUSPS %in%
                             c("AK","HI","PR","VI","GU","AS","MP"), ]
counties_sf$geoid5 <- counties_sf$GEOID
m <- merge(counties_sf, amws_county, by = "geoid5", all.x = TRUE)
m <- merge(m, wiki_county, by = "geoid5", all.x = TRUE)
m$amws_n[is.na(m$amws_n)] <- 0
m$wiki_n[is.na(m$wiki_n)] <- 0

state_sf <- tigris::states(cb = TRUE, resolution = "20m", year = 2020,
                           progress_bar = FALSE) |> st_transform(5070)
state_sf <- state_sf[!state_sf$STUSPS %in%
                       c("AK","HI","PR","VI","GU","AS","MP"), ]

make_map <- function(sf_obj, var, title) {
  v <- sf_obj[[var]]
  sf_obj$.fill <- ifelse(v > 0, log10(v), NA_real_)
  ggplot() +
    geom_sf(data = sf_obj, aes(fill = .fill), color = NA) +
    geom_sf(data = state_sf, fill = NA, color = "grey20", linewidth = 0.2) +
    scale_fill_viridis_c(option = "magma", na.value = "grey92", name = "log10(n)") +
    labs(title = title,
         subtitle = sprintf("births %d-%d  |  counties with >=1: %d  |  total: %d",
                            yr_lo, yr_hi, sum(v > 0), sum(v))) +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
}

p_amws <- make_map(m, "amws_n", "AMWS combined (1906 + 1938 + 1955, post-dedup)")
p_wiki <- make_map(m, "wiki_n", "Wikipedia US-born STEM")
p_both <- p_amws + p_wiki + plot_layout(ncol = 1)
ggsave(file.path(out_dir, "county_map_amws_combined_vs_wiki.png"), p_both,
       width = 11, height = 11, dpi = 150)

both <- as.data.table(m)[, .(geoid5, amws_n, wiki_n)]
cat(sprintf("\nCounty-level correlation AMWS combined vs Wiki STEM:\n  pearson: %.3f\n  log1p:   %.3f\n  spearman:%.3f\n",
            cor(both$amws_n, both$wiki_n),
            cor(log1p(both$amws_n), log1p(both$wiki_n)),
            cor(both$amws_n, both$wiki_n, method = "spearman")))
fwrite(both, file.path(out_dir, "county_counts.csv"))

cat("\nFigures + tables written to", out_dir, "\n")
