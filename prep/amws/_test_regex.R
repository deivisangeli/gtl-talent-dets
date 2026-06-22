test <- c("Easton, Pa, Jan. 15, 67. Lafayette Col",
          "Yorktown, N. Y. M.D, Womans Med",
          "Brooklyn, N. Y, April 2, 81",
          "Bear Lakes, Wis, March 24, 01. B.S")
MONTHS <- "(Jan|Feb|Mar|Apr|May|June?|July?|Aug|Sept?|Oct|Nov|Dec)"
DATE_RX <- paste0("\\b", MONTHS, "\\.?\\s+\\d{1,2}\\s*[,.]?\\s*(\\d{2,4})\\b")
cat("regex:", DATE_RX, "\n")
m <- regexpr(DATE_RX, test, ignore.case = TRUE, perl = TRUE)
cat("match positions:", m, "\n")
cat("match lengths:", attr(m, "match.length"), "\n")
for (i in seq_along(test)) {
  if (m[i] > 0) {
    cat(sprintf("[%d] match='%s'  prefix='%s'\n", i,
                substr(test[i], m[i], m[i] + attr(m, "match.length")[i] - 1),
                substr(test[i], 1, m[i] - 1)))
  } else {
    cat(sprintf("[%d] NO MATCH for: %s\n", i, test[i]))
  }
}
