suppressPackageStartupMessages({ library(data.table); library(haven) })
df <- read_dta("input/amws_1938.dta")
for (col in names(df)[sapply(df, is.character)])
  df[[col]] <- iconv(df[[col]], from="latin1", to="UTF-8", sub="?")
d <- as.data.table(df)
setnames(d, c("lineid","page","last","address_legacy","field_legacy","blob","init","AMSname"))

cat("non-NA blob rows:", sum(!is.na(d$blob)), "\n")
cat("blob containing any month-like word (full or abbrev):",
    sum(grepl("\\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)",
              d$blob, ignore.case=TRUE)),"\n")
cat("blob containing month + day + year-like pattern (loose):",
    sum(grepl("\\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z\\.]*\\s+\\d{1,2}",
              d$blob, ignore.case=TRUE)),"\n")

cat("\n--- 20 random rows with NO month substring at all ---\n")
nomo <- d[!grepl("\\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)",
                 blob, ignore.case=TRUE) & !is.na(blob)]
cat("count:", nrow(nomo),"\n")
set.seed(1)
print(nomo[sample(.N, min(20,.N)), .(lineid, substr(blob, 1, 140))])
