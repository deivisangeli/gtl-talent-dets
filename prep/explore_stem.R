suppressPackageStartupMessages(library(tidyverse))
data <- read_csv("input/cross-verified-database.csv", show_col_types=FALSE)

sci <- data %>% filter(level1_main_occ == "Discovery/Science")

cat("=== Total Discovery/Science ===\n")
cat("N =", nrow(sci), "\n\n")

cat("=== Is level3_main_occ already in English? Sample non-US entries ===\n")
sci %>%
  filter(citizenship_1_b != "US") %>%
  select(name, citizenship_1_b, level3_main_occ, level3_all_occ) %>%
  slice_sample(n = 20) %>%
  mutate(level3_all_occ = str_trunc(level3_all_occ, 100)) %>%
  print(n = 20, width = 200)

cat("\n=== All level3_main_occ values: top 80 ===\n")
sci %>%
  count(level3_main_occ, sort = TRUE) %>%
  slice_head(n = 80) %>%
  print(n = 80)

cat("\n=== level3_main_occ values that look non-English (contain accents/non-ASCII) ===\n")
sci %>%
  count(level3_main_occ, sort = TRUE) %>%
  filter(str_detect(level3_main_occ, "[^[:ascii:]]") |
         str_detect(level3_main_occ, "_(fr|de|es|it|pt|ru|pl|nl)$")) %>%
  print(n = 50)

cat("\n=== Sample of level3_all_occ for French/German scientists ===\n")
sci %>%
  filter(citizenship_1_b %in% c("FR", "DE")) %>%
  select(name, citizenship_1_b, level3_main_occ, level3_all_occ) %>%
  slice_head(n = 8) %>%
  mutate(level3_all_occ = str_trunc(level3_all_occ, 150)) %>%
  print(n = 8, width = 220)
