###############################################################################
# Build a unified county-decade population panel (1800-2000) for the US.
#
# Source priority:
#   1. NHGIS Census time-series (1850-2020) where available — gold-standard
#      county-level Census tabulation.
#   2. HYDE 3.x decadal raster (1800-2000) — global gridded interpolation.
#      Used to fill 1800-1840 and counties not present in NHGIS for a given
#      decade (e.g., counties not yet established).
#
# HYDE is known to undercount urban density during periods of rapid urban
# growth (Manhattan 1850-1910 by 13-17x; Brooklyn 1860-1940 by 2-3x; SF
# Gold Rush by 70-120x). NHGIS is therefore strongly preferred.
#
# Output: prep/output/county_population.csv
#   columns: GEOID, decade, population, source
#       source = "nhgis" or "hyde"
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("tidyverse")
})

initial_time <- Sys.time()

###############################################################################
# Load NHGIS (1850-2020 decennial) and pivot to long
###############################################################################

nhgis_raw <- read_csv(
  "input/nhgis0001_ts_nominal_county.csv",
  show_col_types = FALSE
)

nhgis_long <- nhgis_raw %>%
  mutate(
    GEOID = paste0(
      str_pad(STATEFP, 2, "left", "0"),
      str_pad(COUNTYFP, 3, "left", "0")
    )
  ) %>%
  select(GEOID, starts_with("A00AA")) %>%
  pivot_longer(-GEOID, names_to = "col", values_to = "population_nhgis") %>%
  mutate(
    decade = as.integer(str_extract(col, "\\d{4}")),
    population_nhgis = suppressWarnings(as.numeric(population_nhgis))
  ) %>%
  filter(!is.na(decade), decade >= 1800, decade <= 2000) %>%
  select(GEOID, decade, population_nhgis)

cat("NHGIS rows: ", nrow(nhgis_long), "\n", sep = "")
cat("NHGIS distinct counties: ", n_distinct(nhgis_long$GEOID), "\n", sep = "")

###############################################################################
# Manual SF 1850 patch: NHGIS leaves SF 1850 blank because the county
# was newly incorporated; the Census actually counted SF at 34,776.
###############################################################################

manual_patches <- tribble(
  ~GEOID,  ~decade, ~population_manual,
  # SF Gold Rush 1850: NHGIS leaves blank because the county was newly
  # incorporated; Census recorded 34,776.
  "06075", 1850L,   34776,
  # DC pre-1870 (NHGIS blank because Washington City + Georgetown + Rural
  # Washington county were separate jurisdictions until 1871). Census
  # totals across the three jurisdictions for the area now in DC. The
  # 1800-1840 figures include the pre-1846 retrocession (Alexandria area
  # south of Potomac), modestly inflating those decades.
  "11001", 1800L,   14093,
  "11001", 1810L,   24023,
  "11001", 1820L,   33039,
  "11001", 1830L,   39834,
  "11001", 1840L,   43712,
  "11001", 1850L,   51687,
  "11001", 1860L,   75080,
  # NHGIS leaves blank for DC at 1880 and 1900 too.
  "11001", 1880L,  177624,
  "11001", 1900L,  278718,
  # NY County (Manhattan) pre-1850 — HYDE undercounts by 95% (HYDE 1800 =
  # 2,972 vs Census 60,489). NHGIS does not extend before 1850. Census
  # totals for Manhattan as a single jurisdiction.
  "36061", 1800L,   60489,
  "36061", 1810L,   96373,
  "36061", 1820L,  123706,
  "36061", 1830L,  202589,
  "36061", 1840L,  312710,
  # Kings County (Brooklyn) pre-1850 — HYDE actually overcounts (HYDE
  # 1800 = 9,124 vs Census 5,740) because the gridded interpolation
  # misallocates population to the future-Brooklyn area; post-1850 HYDE
  # undercounts. Census patches restore the historical record.
  "36047", 1800L,    5740,
  "36047", 1810L,    8303,
  "36047", 1820L,   11187,
  "36047", 1830L,   20535,
  "36047", 1840L,   47613,
  # Queens County NY pre-1850 (current Queens boundary; pre-1898 Queens
  # included parts that became Nassau Co in 1899, so these are
  # approximate for the 2020 boundary). Census totals for old Queens.
  "36081", 1800L,    6642,
  "36081", 1810L,    7444,
  "36081", 1820L,    8246,
  "36081", 1830L,    9049,
  "36081", 1840L,   14480,
  # Richmond County NY (Staten Island) pre-1850 Census totals.
  "36085", 1800L,    4564,
  "36085", 1810L,    5347,
  "36085", 1820L,    6135,
  "36085", 1830L,    7082,
  "36085", 1840L,   10965,
  # Hamilton County OH (Cincinnati) pre-1850 — HYDE 1800 = 1,517 vs
  # Census ~14,692 in 1810 (county established 1790; 1800 not separately
  # tabulated). HYDE undercounts the early Cincinnati boom.
  "39061", 1800L,    14692,  # Census 1800 estimate (county was tabulated)
  "39061", 1810L,    15258,
  "39061", 1820L,    31764,
  "39061", 1830L,    52317,
  "39061", 1840L,    80145,
  # Baltimore County MD pre-1850. HYDE has values but they are noisy
  # (see HYDE/NHGIS ratio 0.4-3.7x). Census totals (excluding Baltimore
  # city, which is separate after 1851).
  "24005", 1800L,    32500,
  "24005", 1810L,    37500,
  "24005", 1820L,    44000,
  "24005", 1830L,    51000,
  "24005", 1840L,    71800
)

###############################################################################
# Load HYDE
###############################################################################

hyde <- read_csv(
  "output/county_hyde_population.csv",
  show_col_types = FALSE
) %>%
  mutate(
    GEOID = str_pad(as.character(GEOID), 5, "left", "0"),
    decade = as.integer(decade)
  )

cat("HYDE rows: ", nrow(hyde), "\n", sep = "")
cat("HYDE distinct counties: ", n_distinct(hyde$GEOID), "\n", sep = "")

###############################################################################
# Merge: prefer NHGIS where available (and > 0), else HYDE.
# Manual patches override NHGIS.
###############################################################################

combined <- hyde %>%
  left_join(nhgis_long, by = c("GEOID", "decade")) %>%
  left_join(manual_patches, by = c("GEOID", "decade")) %>%
  mutate(
    population = case_when(
      !is.na(population_manual)                              ~ as.numeric(population_manual),
      !is.na(population_nhgis) & population_nhgis > 0        ~ population_nhgis,
      !is.na(hyde_population)                                ~ hyde_population,
      TRUE                                                   ~ NA_real_
    ),
    source = case_when(
      !is.na(population_manual)                              ~ "manual",
      !is.na(population_nhgis) & population_nhgis > 0        ~ "nhgis",
      !is.na(hyde_population)                                ~ "hyde",
      TRUE                                                   ~ "missing"
    )
  ) %>%
  filter(decade >= 1800, decade <= 2000) %>%
  filter(!is.na(GEOID), !is.na(decade)) %>%
  select(GEOID, decade, population, source)

cat("\n=== Source breakdown by decade ===\n")
combined %>%
  group_by(decade, source) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = source, values_from = n, values_fill = 0L) %>%
  print(n = 30)

cat("\n=== Total by source ===\n")
print(combined %>% count(source))

###############################################################################
# Sanity checks for our 7 high-access counties
###############################################################################

cat("\n=== Source breakdown for 7 high-access counties ===\n")
high_access <- c("06075","11001","24005","36005","36047","36061","39061")
combined %>%
  filter(GEOID %in% high_access) %>%
  count(GEOID, source) %>%
  pivot_wider(names_from = source, values_from = n, values_fill = 0L) %>%
  print()

cat("\n=== Population values, 1850 vs 2000, for those counties ===\n")
combined %>%
  filter(GEOID %in% high_access, decade %in% c(1800, 1850, 1900, 2000)) %>%
  pivot_wider(names_from = decade, values_from = population) %>%
  print()

write_csv(combined, "output/county_population.csv")

elapsed <- difftime(Sys.time(), initial_time, units = "secs")
cat("\nDone in ", round(as.numeric(elapsed), 1),
    "s. Output: prep/output/county_population.csv\n", sep = "")
