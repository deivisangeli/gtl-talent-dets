suppressPackageStartupMessages(library(tidyverse))
data <- read_csv("../prep/input/cross-verified-database.csv", show_col_types=FALSE)
us <- data %>% filter(citizenship_1_b == "US", birth >= 1800, birth <= 1999)

# Check level3_all_occ for the top academics
cat("Top 20 academics — all occupations listed:\n")
us %>%
  filter(level1_main_occ == "Discovery/Science", level3_main_occ == "academic") %>%
  arrange(desc(wiki_readers_2015_2018)) %>%
  select(name, birth, level3_main_occ, level3_all_occ) %>%
  slice_head(n = 20) %>%
  print(n=20, width=200)
