suppressPackageStartupMessages({
  library(tidyverse); library(did); library(sf)
})
options(tigris_use_cache = TRUE, timeout = 1000)

data_full <- read_csv("../prep/output/us_panel_county_stem.csv", show_col_types = FALSE) %>%
  mutate(stem_per_100k = replace_na(stem_per_100k, 0))

facilities     <- read_delim("../prep/output/facilities_us.csv",     delim = ";",
                              locale = locale(decimal_mark = "."), show_col_types = FALSE)
facilities_alt <- read_delim("../prep/output/facilities_us_alt.csv", delim = ";",
                              locale = locale(decimal_mark = "."), show_col_types = FALSE)

counties_poly <- suppressMessages(
  tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
    st_transform(3857) %>% select(GEOID, geometry) %>%
    filter(as.integer(substr(GEOID, 1, 2)) <= 56)
)

prep_fac <- function(fac) fac %>% filter(!is.na(year)) %>%
  mutate(decade_std   = floor(year / 10) * 10,
         decade_shift = ifelse(year %% 10 >= 7, floor(year / 10) * 10 + 10, floor(year / 10) * 10))

facilities     <- prep_fac(facilities)
facilities_alt <- prep_fac(facilities_alt)

buf_treat <- function(fac_df) {
  buf <- fac_df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(3857) %>%
    st_buffer(50000) %>%
    mutate(fac_idx = row_number()) %>%
    select(fac_idx, decade_std, decade_shift)
  st_join(counties_poly, buf, join = st_intersects) %>%
    st_drop_geometry() %>%
    filter(!is.na(fac_idx)) %>%
    group_by(GEOID) %>%
    summarise(g_std = min(decade_std), g_shift = min(decade_shift), .groups = "drop")
}

make_panel <- function(ev) data_full %>%
  left_join(ev, by = "GEOID") %>%
  mutate(g_std   = ifelse(is.na(g_std),   0, g_std),
         GEOID   = as.numeric(GEOID))

panel     <- make_panel(buf_treat(facilities))
panel_alt <- make_panel(buf_treat(facilities_alt))

run_cs <- function(data, gname) suppressWarnings(
  att_gt(yname = "stem_per_100k", tname = "decade", idname = "GEOID", gname = gname,
         data = data, control_group = "notyettreated", est_method = "dr",
         base_period = "universal", cores = 4)
)

agg_res <- function(out, label) {
  ag <- suppressWarnings(aggte(out, type = "simple", na.rm = TRUE))
  pval <- 2 * (1 - pnorm(abs(ag$overall.att / ag$overall.se)))
  cat(sprintf("%-42s  ATT = %7.4f  SE = %6.4f  p = %5.3f\n",
              label, ag$overall.att, ag$overall.se, pval))
}

cat("=== Overall ATT (simple aggregation, STEM per 100k) ===\n")
agg_res(run_cs(panel,     "g_std"),   "Full facilities  | std decade")
agg_res(run_cs(panel,     "g_shift"), "Full facilities  | alt decade")
agg_res(run_cs(panel_alt, "g_std"),   "Alt  facilities  | std decade")
agg_res(run_cs(panel_alt, "g_shift"), "Alt  facilities  | alt decade")

cat("\n=== Fermilab: Weston vs. shortlisted counties ===\n")
weston <- data_full %>%
  filter(GEOID %in% c("17043", "26161", "36103", "08031", "55025", "06017")) %>%
  mutate(g_weston = ifelse(GEOID == "17043", 1970, 0),
         GEOID    = as.numeric(GEOID))
agg_res(run_cs(weston, "g_weston"), "Weston vs shortlisted")

# Also compare to all-sci outcome for context
cat("\n=== Same specs on all Discovery/Science (inv_per_100k) ===\n")
data_allsci <- read_csv("../prep/output/us_panel_county.csv", show_col_types = FALSE) %>%
  mutate(inv_per_100k = replace_na(inv_per_100k, 0))

run_cs_all <- function(data, gname) suppressWarnings(
  att_gt(yname = "inv_per_100k", tname = "decade", idname = "GEOID", gname = gname,
         data = data, control_group = "notyettreated", est_method = "dr",
         base_period = "universal", cores = 4)
)

agg_res2 <- function(out, label) {
  ag <- suppressWarnings(aggte(out, type = "simple", na.rm = TRUE))
  pval <- 2 * (1 - pnorm(abs(ag$overall.att / ag$overall.se)))
  cat(sprintf("%-42s  ATT = %7.4f  SE = %6.4f  p = %5.3f\n",
              label, ag$overall.att, ag$overall.se, pval))
}

panel_all     <- data_allsci %>% left_join(buf_treat(facilities),     by = "GEOID") %>%
  mutate(g_std = ifelse(is.na(g_std), 0, g_std), GEOID = as.numeric(GEOID))
panel_all_alt <- data_allsci %>% left_join(buf_treat(facilities_alt), by = "GEOID") %>%
  mutate(g_std = ifelse(is.na(g_std), 0, g_std), GEOID = as.numeric(GEOID))

agg_res2(run_cs_all(panel_all,     "g_std"),   "Full fac | std decade (all sci)")
agg_res2(run_cs_all(panel_all_alt, "g_std"),   "Alt  fac | std decade (all sci)")
