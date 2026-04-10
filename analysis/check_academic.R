suppressPackageStartupMessages(library(tidyverse))
data <- read_csv("../prep/input/cross-verified-database.csv", show_col_types=FALSE)
us <- data %>%
  filter(citizenship_1_b == "US", birth >= 1800, birth <= 1999)

cat("Total 'academic' in Discovery/Science:\n")
us %>% filter(level1_main_occ == "Discovery/Science", level3_main_occ == "academic") %>% nrow() %>% cat("\n\n")

cat("Top 40 by Wikipedia views:\n")
us %>%
  filter(level1_main_occ == "Discovery/Science", level3_main_occ == "academic") %>%
  arrange(desc(wiki_readers_2015_2018)) %>%
  select(name, birth, wiki_readers_2015_2018) %>%
  slice_head(n = 40) %>%
  print(n=40, width=120)

# Also check what other fields are available for disambiguation
cat("\nColumn names with 'occ':\n")
data %>% select(contains("occ")) %>% names() %>% cat(sep="\n")
