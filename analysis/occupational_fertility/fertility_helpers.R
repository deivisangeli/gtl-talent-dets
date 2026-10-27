# Helpers for professor_fertility_1910.R.

# Direct standardisation of a mean.
#
# `cells` has one row per group x stratum with `n` (persons) and `total` (sum of
# the outcome). `standard` has one row per stratum with `weight` (any scale).
# A group missing any standard stratum gets NA: silently reweighting over the
# strata it has would compare it on a different standard.
direct_standardise <- function(cells, standard) {
  w <- standard$weight / sum(standard$weight)
  names(w) <- standard$stratum
  groups <- unique(cells$group)
  do.call(rbind, lapply(groups, function(g) {
    cg <- cells[cells$group == g, , drop = FALSE]
    rate <- (cg$total / cg$n)[match(names(w), cg$stratum)]
    data.frame(group = g, n = sum(cg$n), crude_mean = sum(cg$total) / sum(cg$n),
               std_mean = if (anyNA(rate)) NA_real_ else sum(w * rate),
               stringsAsFactors = FALSE)
  }))
}

# Group x stratum cells for several outcomes at once, from a data.frame.
standardise_outcomes <- function(data, outcomes, stratum, standard) {
  do.call(rbind, lapply(outcomes, function(outcome) {
    keep <- !is.na(data[[outcome]]) & !is.na(data[[stratum]])
    d <- data[keep, , drop = FALSE]
    cells <- stats::aggregate(list(total = d[[outcome]]),
                              by = list(group = d$group, stratum = d[[stratum]]), FUN = sum)
    cells$n <- stats::aggregate(list(n = d[[outcome]]),
                                by = list(group = d$group, stratum = d[[stratum]]),
                                FUN = length)$n
    out <- direct_standardise(cells, standard)
    out$outcome <- outcome
    out
  }))
}

five_year_bin <- function(x, lo, hi) {
  ifelse(x >= lo & x <= hi, paste0(lo + 5 * ((x - lo) %/% 5), "-",
                                    lo + 5 * ((x - lo) %/% 5) + 4), NA_character_)
}
