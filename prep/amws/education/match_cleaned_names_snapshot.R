# Offline vectorized Damerau-Levenshtein scoring, preserving word order.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
suppressPackageStartupMessages({library(data.table); library(stringdist)})
d <- fread(file.path(args[1], "candidate_pairs.csv"), encoding="UTF-8",
           colClasses=c(query_id="character", id="character"))
stopifnot(!anyNA(d$query), !anyNA(d$candidate))
d[, distance := stringdist(query, candidate, method="dl", nthread=4L)]
d[, score := 100 * (1 - distance / pmax(nchar(query, type="chars"),
                                         nchar(candidate, type="chars"), 1L))]
setorder(d, query_id, id, -score, priority, alias, source)
best <- unique(d, by=c("query_id", "id"))
setorder(best, query_id, -score, priority, id)
best <- best[, head(.SD, 5L), by=query_id]
fwrite(best[, .(query_id, id, alias, source, score)],
       file.path(args[1], "candidate_scores.csv"), bom=TRUE)
cat("Scored", nrow(d), "alias pairs; retained", nrow(best), "distinct-ID candidates\n")
