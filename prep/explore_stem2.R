suppressPackageStartupMessages(library(tidyverse))
data <- read_csv("input/cross-verified-database.csv", show_col_types=FALSE)
sci  <- data %>% filter(level1_main_occ == "Discovery/Science")

# All unique level3_main_occ values, sorted by count
all_occ <- sci %>% count(level3_main_occ, sort = TRUE)

cat("Total unique level3_main_occ values:", nrow(all_occ), "\n\n")

# Print ALL of them so we can classify exhaustively
print(all_occ, n = Inf)
