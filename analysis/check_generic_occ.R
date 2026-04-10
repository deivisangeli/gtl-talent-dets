suppressPackageStartupMessages(library(tidyverse))
data <- read_csv("../prep/input/cross-verified-database.csv", show_col_types=FALSE)
us <- data %>% filter(citizenship_1_b == "US", birth >= 1800, birth <= 1999)

for (occ in c("professor", "teacher")) {
  cat("\n====", toupper(occ), "====\n")
  sub <- us %>%
    filter(level1_main_occ == "Discovery/Science", level3_main_occ == occ)
  cat("N =", nrow(sub), "\n\n")
  sub %>%
    arrange(desc(wiki_readers_2015_2018)) %>%
    select(name, birth, level3_all_occ) %>%
    slice_head(n = 15) %>%
    print(n=15, width=200)
}
