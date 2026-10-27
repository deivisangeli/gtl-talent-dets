# Synthetic validation; does not need project data or write outputs.
source("analysis/mandatory_schooling/estimate_linked_1940_schooling_1950_scientist_rd.R",
       local = TRUE)

# A law introduced in 1900 for ages 8-13 first covers the 1887 cohort for one year.
policy <- data.frame(year = 1890:1906, entryage = 0L, exitage = 0L,
                     earlyyrs = 0L, earlyyrs_condition = 0L, workage = 0L,
                     workyrs = 0L, contage = 0L, contyrs = 0L)
policy$entryage[policy$year >= 1900] <- 8L
policy$exitage[policy$year >= 1900] <- 14L
req <- calculate_cohort_requirements(policy, 1885:1889)
stopifnot(identical(req$ca_years_combined, 0:4 - c(0L, 1L, 1L, 1L, 1L)))

law_cells <- expand.grid(bpl = 1:49, yob = 1885:1912)
law_cells$statename <- paste0("State ", law_cells$bpl)
law_cells$ca_years_combined <- 0L
law_cells$ca_years_combined[law_cells$bpl == 1L & law_cells$yob >= 1900L] <-
  pmin(6L, law_cells$yob[law_cells$bpl == 1L & law_cells$yob >= 1900L] - 1899L)
law_cells$ca_years_combined[law_cells$bpl == 2L & law_cells$yob >= 1890L] <-
  pmin(6L, law_cells$yob[law_cells$bpl == 2L & law_cells$yob >= 1890L] - 1889L)
onset <- data.frame(statefip = 1:49, cls_first_year_in_force = 1910L,
                    has_repeal = FALSE)
onset$has_repeal[[2L]] <- TRUE
cutoffs <- first_law_cutoffs(law_cells, onset)
stopifnot(nrow(cutoffs) == 2L, identical(cutoffs$cutoff_yob,
                                        c(1890L, 1900L)),
          identical(cutoffs$bpl[cutoffs$is_first_law], 1L))

# A known schooling effect must survive collapsing identical individual records.
individual <- expand.grid(bpl = 1:8, k = -6:5, person = 1:8)
individual$yob <- 1900L + individual$k
individual$statename <- paste0("State ", individual$bpl)
individual$cutoff_yob <- 1900L
individual$post <- as.integer(individual$k >= 0L)
individual$x <- individual$k + 0.5
individual$ca_years_combined <- pmax(0L, individual$k + 1L)
individual$school_years <- 7 + individual$bpl / 5 +
  individual$k * (0.03 + individual$bpl / 1000) +
  0.4 * individual$post + individual$person / 100
individual$scientist <- 0.002 * individual$school_years +
  individual$bpl / 1000 + 0.0001 * individual$x
individual$n <- 1L
individual$kernel_weight <- 1 - abs(individual$x) / 6
states <- sort(unique(individual$bpl))
slopes <- unlist(lapply(states, function(s) paste0(c("pre_", "post_"), s)))
for (s in states) {
  individual[[paste0("pre_", s)]] <- individual$x *
    (individual$post == 0L & individual$bpl == s)
  individual[[paste0("post_", s)]] <- individual$x *
    (individual$post == 1L & individual$bpl == s)
}
attr(individual, "slopes") <- slopes
expanded_fit <- fit_local_models(individual)
stopifnot(abs(unname(coef(expanded_fit$fuzzy)[["fit_school_years"]]) - 0.002) < 1e-9)

group_keys <- c("bpl", "k", "yob", "statename", "cutoff_yob", "post", "x",
                "ca_years_combined", "school_years", "scientist", slopes)
grouped <- aggregate(individual$n, individual[group_keys], sum)
names(grouped)[ncol(grouped)] <- "n"
stopifnot("n" %in% names(grouped))
grouped$kernel_weight <- grouped$n * (1 - abs(grouped$x) / 6)
attr(grouped, "slopes") <- slopes
grouped_fit <- fit_local_models(grouped)
stopifnot(isTRUE(all.equal(unname(coef(grouped_fit$fuzzy)),
                           unname(coef(expanded_fit$fuzzy)), tolerance = 1e-9)))
message("linked 1940 schooling / 1950 scientist RD fixture passed")
