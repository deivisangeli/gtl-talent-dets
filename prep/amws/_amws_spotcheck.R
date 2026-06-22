library(data.table)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

d <- fread(file.path(AMWS_OUTPUT, "amws_1955_cleaned.csv"))

# Parse trailing 1-4 digit number = year (after the final comma/space)
parse_amws_year <- function(s) {
  m <- regmatches(s, regexpr("[0-9]{1,4}\\s*$", s))
  out <- rep(NA_integer_, length(s))
  has <- regexpr("[0-9]{1,4}\\s*$", s) > 0
  if (any(has)) {
    yr <- suppressWarnings(as.integer(sub("\\s+$", "", m)))
    out_idx <- which(has)
    for (i in seq_along(out_idx)) {
      y <- yr[i]; idx <- out_idx[i]
      if (is.na(y)) next
      if (y >= 1000)               out[idx] <- y
      else if (y <= 40)            out[idx] <- 1900L + y
      else                         out[idx] <- 1800L + y
    }
  }
  out
}
d[, by := parse_amws_year(date)]
cat("parsed:", sum(!is.na(d$by)), "/", nrow(d), "\n")

has_month <- grepl("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec",
                   d$date, ignore.case = TRUE)
d[, has_month := has_month]

cat("\n--- has_month by birth-year bin ---\n")
brks <- c(-Inf, 1860, 1880, 1900, 1910, 1915, 1920, 1925, 1930, Inf)
d[, yrbin := cut(by, brks)]
print(d[!is.na(by), .(n = .N,
                      with_month = sum(has_month),
                      pct_month = round(100 * mean(has_month), 1)),
        by = yrbin][order(yrbin)])

cat("\n--- 25 random entries born >=1915 with full string ---\n")
set.seed(11)
s <- d[!is.na(by) & by >= 1915]
s <- s[sample(.N, min(25, .N))][order(-by)]
for (i in seq_len(nrow(s))) {
  cat(sprintf("%5d  by=%d  has_month=%s  date=[%s]\n  orig=%s\n",
              s$lineid[i], s$by[i], s$has_month[i], s$date[i],
              substr(s$birthplace_orig[i], 1, 180)))
}
