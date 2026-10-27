# Additive birth-origin outcomes; the older immutable release helpers stay unchanged.
birth_training_stamp <- "amws_birth_county_training_20261019"
birth_training_geographies <- c("same_county", "50km")
birth_training_groups <- c("ever", "never", "unknown")
birth_training_counts <- c(setNames(
  unlist(lapply(birth_training_geographies, function(g) c(paste0("n_amws_born_", c("ever", "never"), "_trained_", g), paste0("n_amws_born_training_unknown_", g)))),
  unlist(lapply(birth_training_geographies, function(g) paste(g, birth_training_groups, sep = "__")))),
  total = "n_amws_born_total")
birth_training_rates <- setNames(paste0(sub("^n_", "", birth_training_counts), "_per_1000_births"), names(birth_training_counts))

birth_training_class <- function(local, n_awards, complete, conflicts = rep(FALSE, n_awards)) {
  if (any(local %in% TRUE)) return("ever")
  if (n_awards > 0L && length(complete) == n_awards && all(complete %in% TRUE) &&
      length(conflicts) == n_awards && !any(conflicts %in% TRUE) &&
      length(local) > 0L && !anyNA(local)) return("never")
  "unknown"
}

birth_training_partition <- function(p) {
  for (geo in birth_training_geographies) {
    keys <- paste(geo, birth_training_groups, sep = "__")
    training_require(all(rowSums(p[, unname(birth_training_counts[keys]), with = FALSE]) == p$n_amws_born_total), "Count partition failed")
    if (all(unname(birth_training_rates) %in% names(p))) {
      training_require(isTRUE(all.equal(rowSums(p[, unname(birth_training_rates[keys]), with = FALSE]),
        p[[birth_training_rates[['total']]]], tolerance = 1e-12, check.attributes = FALSE)), "Rate partition failed")
    }
  }
}

birth_training_expected <- function(balance, timing) {
  if (balance == "calendar") c(15L, 46L, 598L)
  else if (timing == "standard_decade") c(16L, 52L, 650L) else c(20L, 62L, 834L)
}
