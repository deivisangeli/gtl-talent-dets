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

source("../paths.R")

initial_time <- Sys.time()

###############################################################################
# Load NHGIS (1790-2020 decennial total population, time-series table A00,
# long format with one row per county-decade). The NHGIS extract uses
# nominal historical boundaries — i.e. each decade reflects the county as
# enumerated at that Census. Boundary mismatches with 2020 GEOIDs (e.g.
# pre-1851 Baltimore County included Baltimore City; pre-1899 Queens
# included Nassau) are corrected via manual_patches below.
###############################################################################

nhgis_raw <- read_csv(
  file.path(DATA_INPUT, "nhgis0005_ts_nominal_county.csv"),
  show_col_types = FALSE,
  skip = 2,
  col_names = c("GISJOIN", "YEAR", "STATE", "STATEFP", "STATENH",
                "COUNTY", "COUNTYFP", "COUNTYNH", "NAME", "A00AA")
)

nhgis_long <- nhgis_raw %>%
  mutate(
    GEOID = paste0(
      str_pad(STATEFP, 2, "left", "0"),
      str_pad(COUNTYFP, 3, "left", "0")
    ),
    decade = as.integer(YEAR),
    population_nhgis = suppressWarnings(as.numeric(A00AA))
  ) %>%
  filter(!is.na(decade), decade >= 1800, decade <= 2000) %>%
  select(GEOID, decade, population_nhgis)

cat("NHGIS rows: ", nrow(nhgis_long), "\n", sep = "")
cat("NHGIS distinct counties: ", n_distinct(nhgis_long$GEOID), "\n", sep = "")

###############################################################################
# Manual SF 1850 patch: NHGIS leaves SF 1850 blank because the county
# was newly incorporated; the Census actually counted SF at 34,776.
###############################################################################

# Manual patches retained ONLY where the NHGIS time-series fails to
# represent the 2020 county boundary used downstream. NHGIS now covers
# 1790-2020 directly for >99% of cells, so all patches that merely filled
# in HYDE-bad early decades have been removed.
#
# The remaining patches are corrections for documented historical
# boundary changes between the enumerated unit and the modern GEOID.
manual_patches <- tribble(
  ~GEOID,  ~decade, ~population_manual,

  # ---- DC 11001 ----
  # NHGIS has DC only from 1870 onward and gaps at 1880 + 1900. Pre-1871
  # the area was three separate jurisdictions (Washington City + Georgetown
  # + rural Washington County). We use Census totals across the three
  # jurisdictions for the area now in DC. The 1800-1840 figures include
  # the pre-1846 retrocession (Alexandria area south of the Potomac),
  # modestly inflating those decades.
  "11001", 1800L,   14093,
  "11001", 1810L,   24023,
  "11001", 1820L,   33039,
  "11001", 1830L,   39834,
  "11001", 1840L,   43712,
  "11001", 1850L,   51687,
  "11001", 1860L,   75080,
  "11001", 1880L,  177624,
  "11001", 1900L,  278718,

  # ---- SF 06075 ----
  # NHGIS has SF only from 1860 onward; the county was created in 1850.
  # Census 1850 enumerated 34,776.
  "06075", 1850L,   34776,

  # ---- Hamilton County OH 39061 ----
  # NHGIS missing 1790, 1800 (county was in the Northwest Territory and
  # was not separately enumerated in 1790; 1800 was the first US Census
  # for the area). Census 1800 = 14,692.
  "39061", 1800L,   14692,

  # ---- Baltimore City 24510 ----
  # Baltimore was part of Baltimore County until formal separation in
  # 1851. NHGIS has Baltimore City entries 1790-1830 and 1860+, but is
  # blank at 1840 and 1850. Census enumerations of Baltimore Town/City
  # as a distinct unit:
  "24510", 1840L,  102313,
  "24510", 1850L,  169054,

  # ---- Baltimore County 24005 ----
  # NHGIS Baltimore County figures pre-1851 (and at 1840, 1850, 1870)
  # alternate between including and excluding the city, producing wild
  # decade-to-decade swings (29k -> 134k -> 211k -> 54k -> 331k). For
  # a 2020-boundary BaltCo we want the rural area only (Baltimore City
  # is GEOID 24510). Census enumerations excluding the city:
  "24005", 1810L,    29255,  # NHGIS value is correct for this decade
  "24005", 1820L,    33463,  # NHGIS value is correct
  "24005", 1830L,    40250,  # NHGIS value is correct
  "24005", 1840L,    32066,  # 134,379 NHGIS includes town; subtract 102,313
  "24005", 1850L,    41592,  # 210,646 NHGIS includes town; subtract 169,054
  "24005", 1870L,    63387,  # 330,741 NHGIS appears to include town; subtract 267,354

  # ---- Queens County NY 36081 ----
  # Pre-1899 NHGIS Queens enumerations include the territory that became
  # Nassau County in 1899 (Hempstead, North Hempstead, Oyster Bay), which
  # roughly doubles the count vs the 2020 Queens-borough boundary. Census
  # for the 2020-Queens portion (Newtown, Flushing, Jamaica towns) only:
  "36081", 1800L,    6642,
  "36081", 1810L,    7444,
  "36081", 1820L,    8246,
  "36081", 1830L,    9049,
  "36081", 1840L,   14480,
  "36081", 1850L,   18593,
  "36081", 1860L,   32903,
  "36081", 1870L,   45468,
  "36081", 1880L,   56559,
  "36081", 1890L,   87050
)

###############################################################################
# Load HYDE
###############################################################################

hyde <- read_csv(
  file.path(DATA_OUTPUT, "county_hyde_population.csv"),
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
# Manual patches override everything.
#
# Build from a full skeleton (1790-2000) so manual patches can introduce 1790
# rows even for counties absent from HYDE (which starts at 1800).
###############################################################################

all_geoids <- unique(hyde$GEOID)
all_decades <- seq(1800L, 2000L, by = 10L)
skeleton <- expand_grid(GEOID = all_geoids, decade = all_decades)

combined <- skeleton %>%
  left_join(hyde,        by = c("GEOID", "decade")) %>%
  left_join(nhgis_long,  by = c("GEOID", "decade")) %>%
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

cat("\n=== Source breakdown for 9 high-access counties ===\n")
high_access <- c("06075","11001","24005","24510","36005","36047","36061","39061","42101")
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

write_csv(combined, file.path(DATA_OUTPUT, "county_population.csv"))

elapsed <- difftime(Sys.time(), initial_time, units = "secs")
cat("\nDone in ", round(as.numeric(elapsed), 1),
    "s. Output: prep/output/county_population.csv\n", sep = "")
