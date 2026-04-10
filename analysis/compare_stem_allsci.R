suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fixest))

stem   <- read_csv("../prep/output/data_final_stem.csv",   show_col_types=FALSE)
allsci <- read_csv("../prep/output/data_final_allsci.csv", show_col_types=FALSE)

# USSR membership (same as analysis_main.R)
ussr_countries <- c("Russia","Ukraine","Belarus","Moldova","Lithuania","Latvia",
                    "Estonia","Georgia","Armenia","Azerbaijan","Kazakhstan",
                    "Uzbekistan","Turkmenistan","Kyrgyzstan","Tajikistan")
ussr_start <- 1922; ussr_end <- 1991

panel <- full_join(
  stem   %>% select(country, iso3, year, n_stem,   rate_stem   = rate_per_1000_mort),
  allsci %>% select(country, iso3, year, n_allsci, rate_allsci = rate_per_1000_mort),
  by = c("country","iso3","year")
) %>%
  group_by(country) %>%
  complete(year = 1800:2015, fill = list(n_stem=0, n_allsci=0,
                                         rate_stem=0, rate_allsci=0)) %>%
  fill(country, iso3, .direction="downup") %>%
  ungroup() %>%
  mutate(
    ussr = as.integer(country %in% ussr_countries &
                      year >= ussr_start & year <= ussr_end),
    stem_share = if_else(n_allsci > 0, n_stem / n_allsci, NA_real_)
  )

panel <- panel %>%
  mutate(post_ussr = as.integer(country %in% ussr_countries & year >= ussr_start & year <= ussr_end))

run_twoway <- function(outcome, data = panel) {
  f <- as.formula(paste(outcome, "~ ussr | country + year"))
  feols(f, cluster = ~country, data = data) %>% coeftable()
}

run_did <- function(outcome, data = panel) {
  f <- as.formula(paste(outcome, "~ post_ussr | country + year"))
  feols(f, cluster = ~country, data = data) %>% coeftable()
}

cat("=== Two-way FE: USSR membership effect ===\n\n")
cat("Outcome: n_allsci (all Discovery/Science)\n")
print(run_twoway("n_allsci"))
cat("\nOutcome: n_stem (STEM only)\n")
print(run_twoway("n_stem"))
cat("\nOutcome: rate_allsci (all sci per 1000 mort-cohort)\n")
print(run_twoway("rate_allsci"))
cat("\nOutcome: rate_stem (STEM per 1000 mort-cohort)\n")
print(run_twoway("rate_stem"))
cat("\nOutcome: stem_share (STEM / all sci)\n")
print(run_twoway("stem_share"))


# ---- DiD on full panel ----
cat("\n=== DiD (full panel, USSR countries post-1922 vs rest) ===\n\n")
for (out in c("n_allsci","n_stem","rate_allsci","rate_stem","stem_share")) {
  cat("Outcome:", out, "\n")
  print(run_did(out))
  cat("\n")
}

# ---- STEM share trend over time for key country groups ----
cat("=== STEM share: mean by decade & country group ===\n")
panel %>%
  mutate(
    decade = floor(year/10)*10,
    group = case_when(
      country %in% ussr_countries & year >= 1922 & year <= 1991 ~ "USSR",
      country == "United States of America" ~ "USA",
      country %in% c("France","Germany","United Kingdom") ~ "W. Europe (FR/DE/UK)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group), decade >= 1850, decade <= 1980, n_allsci > 0) %>%
  group_by(group, decade) %>%
  summarise(stem_share = mean(stem_share, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=group, values_from=stem_share) %>%
  mutate(across(where(is.numeric), ~round(.,3))) %>%
  print(n=20)
