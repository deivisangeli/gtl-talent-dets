suppressPackageStartupMessages(library(tidyverse))
source("stem_labels.R")
data <- read_csv("input/cross-verified-database.csv", show_col_types=FALSE)
sci  <- data %>% filter(level1_main_occ == "Discovery/Science", birth >= 1800)
sci_stem <- add_stem_dummy(sci)

cat("=== STEM dummy coverage ===\n")
cat("Total Discovery/Science (birth>=1800):", nrow(sci_stem), "\n")
cat("STEM == 1:", sum(sci_stem$stem), sprintf("(%.1f%%)\n", 100*mean(sci_stem$stem)))
cat("STEM == 0:", sum(sci_stem$stem == 0), sprintf("(%.1f%%)\n\n", 100*mean(sci_stem$stem == 0)))

cat("=== STEM share by birth century ===\n")
sci_stem %>%
  mutate(century = paste0(floor(birth/100)*100, "s")) %>%
  filter(century %in% c("1800s","1900s")) %>%
  group_by(century) %>%
  summarise(n=n(), stem_n=sum(stem), stem_pct=round(100*mean(stem),1)) %>%
  print()

cat("\n=== Top 20 level3_occ among STEM==1 ===\n")
sci_stem %>% filter(stem==1) %>%
  count(level3_occ, sort=TRUE) %>% slice_head(n=20) %>% print()

cat("\n=== Suspicious non-STEM level3_occ (top 15, excluding obvious humanities/medicine) ===\n")
exclude_obv <- c("academic","professor","teacher","educator","scholar","lecturer",
                 "historian","philosopher","theologian","linguist","anthropologist",
                 "archaeologist","art_historian","physician","surgeon","nurse",
                 "dentist","psychiatrist","psychologist","economist","sociologist",
                 "political_scientist","jurist","lawyer","doctor","internist","medical")
sci_stem %>%
  filter(stem==0, !level3_occ %in% exclude_obv) %>%
  count(level3_occ, sort=TRUE) %>% slice_head(n=15) %>% print()
