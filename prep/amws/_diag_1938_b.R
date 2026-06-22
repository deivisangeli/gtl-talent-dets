suppressPackageStartupMessages({ library(data.table) })
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

d <- fread(file.path(AMWS_OUTPUT, "amws_1938_cleaned.csv"))
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
