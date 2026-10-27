source("analysis/mandatory_schooling/estimate_linked_1940_ba_1950_scientist_rd_states.R", local = TRUE)
stopifnot(identical(ba_rd_completion(c(15L, 16L, 17L, NA_integer_)), c(0L, 1L, 1L, NA_integer_)))
people <- expand.grid(bpl = 1:8, k = -6:5, person = 1:20, copy = 1:2)
people$yob <- 1900L + people$k
people$statename <- paste0("State ", people$bpl)
people$cutoff_yob <- 1900L
people$event_year <- 1910L
people$event_kind <- "first law"
people$event_id <- factor(paste0("first_", people$bpl))
people$post <- as.integer(people$k >= 0L)
people$x <- people$k + 0.5
people$ca_years_combined <- pmax(0L, people$k + 1L)
people$ba_completed <- as.integer((people$person + people$bpl + people$k) %% 20L < 5L + 4L * people$post)
people$school_years <- ifelse(people$ba_completed == 1L, 16L + people$person %% 2L, 15L)
people$scientist <- as.integer((people$ba_completed == 1L & people$person %% 3L == 0L) |
                                people$person %% 17L == 0L)
people$n <- 1L
people$kernel_weight <- 1 - abs(people$x) / 6
slopes <- unlist(lapply(1:8, function(s) paste0(c("pre_", "post_"), s)))
for (s in 1:8) {
  people[[paste0("pre_", s)]] <- people$x * (people$post == 0L & people$bpl == s)
  people[[paste0("post_", s)]] <- people$x * (people$post == 1L & people$bpl == s)
}
attr(people, "slopes") <- slopes
keys <- c("bpl", "k", "yob", "statename", "cutoff_yob", "event_id", "event_year", "event_kind",
           "post", "x", "ca_years_combined", "school_years", "ba_completed", "scientist", slopes)
cells <- aggregate(people$n, people[keys], sum)
names(cells)[ncol(cells)] <- "n"
cells$kernel_weight <- cells$n * (1 - abs(cells$x) / 6)
attr(cells, "slopes") <- slopes
expanded <- fit_local_models(people, endogenous = "ba_completed")
grouped <- fit_local_models(cells, endogenous = "ba_completed")
for (name in names(expanded)) {
  stopifnot(isTRUE(all.equal(unname(coef(expanded[[name]])), unname(coef(grouped[[name]])), tolerance = 1e-9)),
            isTRUE(all.equal(unname(vcov(expanded[[name]])), unname(vcov(grouped[[name]])), tolerance = 1e-8)))
}
legacy <- fit_local_models(cells)
stopifnot(isTRUE(all.equal(summary(legacy$required)$coeftable, summary(grouped$required)$coeftable)),
          isTRUE(all.equal(summary(legacy$reduced)$coeftable, summary(grouped$reduced)$coeftable)))
ar <- ar_confidence_set_rd(cells, slopes, limit = 1, n_grid = 21L, endogenous = "ba_completed")
stopifnot(isTRUE(all.equal(unname(ar$grid$p_value[ar$grid$beta == 0]),
                           unname(summary(grouped$reduced)$coeftable["post", 4]), tolerance = 1e-9)))
new_points <- ms_state_grid_points(cells, "ca_years_combined", 6L, "ba_completed", ba_rd_label, 100)
old_points <- ms_state_grid_points(cells, "ca_years_combined", 6L)
stopifnot(nrow(new_points$events) == 8L, nrow(new_points$points) == 8L * 12L * 3L)
kept <- as.character(new_points$points$measure) != ba_rd_label
old_kept <- as.character(old_points$points$measure) != "Schooling (years)"
stopifnot(identical(new_points$points$value[kept], old_points$points$value[old_kept]))
for (i in which(new_points$points$measure == ba_rd_label)) {
  p <- new_points$points[i, ]
  d <- cells[as.character(cells$event_id) == p$event_id & cells$k == p$k, ]
  stopifnot(isTRUE(all.equal(p$value, 100 * sum(d$n * d$ba_completed) / sum(d$n))))
}
continuation_people <- crd_add_slopes(transform(people, required_increment = post, continuation_increment = post))
continuation_cells <- crd_add_slopes(transform(cells, required_increment = post, continuation_increment = post))
expanded_crd <- crd_fit(continuation_people, endogenous = "ba_completed")
grouped_crd <- crd_fit(continuation_cells, endogenous = "ba_completed")
for (name in names(expanded_crd)) {
  stopifnot(isTRUE(all.equal(unname(coef(expanded_crd[[name]])), unname(coef(grouped_crd[[name]])), tolerance = 1e-9)),
            isTRUE(all.equal(unname(vcov(expanded_crd[[name]])), unname(vcov(grouped_crd[[name]])), tolerance = 1e-8)))
}
crd_set <- crd_ar(continuation_cells, limit = 1, n_grid = 21L, endogenous = "ba_completed")
stopifnot(isTRUE(all.equal(unname(crd_set$grid$p_value[crd_set$grid$beta == 0]),
                           unname(summary(grouped_crd$reduced)$coeftable["post", 4]), tolerance = 1e-9)))
descriptive <- ba_rd_rows(crd_fit(continuation_cells, infer = FALSE, endogenous = "ba_completed"),
                           "continuation_only_4", infer = FALSE)
stopifnot(!any(descriptive$outcome == "fuzzy_rd"), all(is.na(descriptive$se)), all(is.na(descriptive$p_value)))
failure <- ba_rd_fit(transform(cells, ba_completed = 0L))
failure_rows <- ba_rd_rows(failure, "unidentified_fixture")
stopifnot(inherits(failure$reduced, "fixest"), inherits(failure$required, "fixest"),
          any(failure_rows$status == "unidentified"))
message("BA state RD checks passed: thresholds, grouped inference, AR zero, panel shares, legacy invariance and failure handling")
