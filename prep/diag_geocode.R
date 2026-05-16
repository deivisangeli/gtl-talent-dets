suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
})
strip_punct <- function(x) trimws(gsub("\\s+"," ",gsub("[[:punct:]]"," ",gsub("\\.","",x))))
expand <- function(x) {
  x <- gsub("\\bSt\\b\\.?","Saint",x,ignore.case=TRUE)
  x <- gsub("\\bMt\\b\\.?","Mount",x,ignore.case=TRUE)
  x
}
strip_suffix <- function(x) trimws(gsub("\\s+"," ",
  gsub("\\b(city|town|township|village|borough|cdp)\\b","",x,ignore.case=TRUE)))
norm <- function(x) tolower(trimws(strip_punct(strip_suffix(expand(x)))))

gaz <- read_tsv("input/2024_Gaz_place_national.txt", show_col_types=FALSE)
gaz$key <- norm(gaz$NAME)
cat("=== SF in Gaz ===\n")
print(gaz |> filter(USPS=="CA", key=="san francisco") |> select(NAME, GEOID, key) |> as.data.frame())
cat("\n=== Portland ME in Gaz ===\n")
print(gaz |> filter(USPS=="ME", key=="portland") |> select(NAME, GEOID, key) |> as.data.frame())
cat("\n=== Winthrop MA in Gaz ===\n")
print(gaz |> filter(USPS=="MA", key=="winthrop") |> select(NAME, GEOID, key) |> as.data.frame())

us <- read_csv("output/amws_1955_us_unmatched.csv", show_col_types=FALSE)
cat("\n=== unmatched 'San Francisco, CA' rows ===\n")
print(us |> filter(city=="San Francisco", state=="CA") |> head(5) |> as.data.frame())
cat("\n=== unmatched 'Portland, ME' rows ===\n")
print(us |> filter(city=="Portland", state=="ME") |> head(5) |> as.data.frame())
