suppressPackageStartupMessages({ library(data.table) })
d <- fread("output/amws_1938_cleaned.csv")
cat("Top 'country' values that are short / look like state fragments:\n")
short <- d[country != "USA" & country != "" & nchar(country) <= 4,
           .N, by = country][order(-N)]
print(head(short, 30))
cat("\nSample rows where country='N':\n")
print(d[country == "N"][1:5, .(birthplace_orig = substr(birthplace_orig,1,80),
                                city, state, country)])
cat("\nSample rows where country='Ohi':\n")
print(d[country == "Ohi"][1:5, .(birthplace_orig = substr(birthplace_orig,1,80),
                                  city, state, country)])
cat("\nSample rows where country='M':\n")
print(d[country == "M"][1:5, .(birthplace_orig = substr(birthplace_orig,1,80),
                                city, state, country)])
