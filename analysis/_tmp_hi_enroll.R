suppressPackageStartupMessages({library(jsonlite); library(data.table)})
files <- list.files("prep/output/enrollment_research/out", pattern="\\.jsonl$", full.names=TRUE)
rows <- lapply(files, function(f) {
  j <- tryCatch(fromJSON(paste(readLines(f, warn=FALSE), collapse="\n")),
                 error=function(e) NULL)
  if (is.null(j)) return(NULL)
  data.table(school=j$school, state=j$state_abbr,
             y10=j$year10_seats, y10y=j$year10_year_used,
             y20=j$year20_seats, y20y=j$year20_year_used,
             y30=j$year30_seats, y30y=j$year30_year_used,
             conf=j$confidence, notes=substr(j$notes,1,200))
})
d <- rbindlist(rows, fill=TRUE)
hi_names <- c("Lowell High School","Paul Laurence Dunbar High School",
              "Hunter College High School","Stuyvesant High School",
              "Regis High School","Brooklyn Technical High School",
              "Bronx High School of Science","Walnut Hills High School",
              "Central High School","Girls High School",
              "Western High School","Baltimore City College","McDonogh School")
cat("--- High-access schools (13 treated) ---\n")
print(d[school %in% hi_names, .(school, state, y10, y10y, y20, y20y, y30, y30y, conf)])
cat("\n--- Sample of notes (high-access) ---\n")
for (i in seq_len(nrow(d))) {
  if (d$school[i] %in% hi_names) {
    cat("\n", d$school[i], " (", d$conf[i], "):\n", d$notes[i], "\n", sep="")
  }
}
