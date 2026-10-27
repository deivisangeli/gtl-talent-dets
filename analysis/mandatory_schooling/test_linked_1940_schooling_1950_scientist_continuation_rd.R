# Validate the continuation-law event screen and stacked estimator.
source("analysis/mandatory_schooling/estimate_linked_1940_schooling_1950_scientist_continuation_rd.R",
       local = TRUE)

root <- rd_data_root()
cls <- file.path(root, "output/mandatory_schooling/cls")
iv <- file.path(root, "results/mandatory_schooling/linked_1940_schooling_1950_scientist_iv")
policy <- read.csv(file.path(cls, "cls_state_age_limits_1880_1930.csv"))
laws <- read.csv(file.path(cls, "cls_cohort_requirements_1875_1912.csv"))
onset <- read.csv(file.path(cls, "cls_law_onset_by_state.csv"))
cells <- read.csv(file.path(iv, "main_cells.csv"))
intro <- crd_first_introductions(policy)
audit <- audit_amendments(policy, laws, onset, cells)$inventory
audit$is_introduction <- audit$reform_year ==
  intro$introduction_year[match(audit$bpl, intro$bpl)]
audit$is_pure <- vapply(strsplit(audit$changed_fields, ";", fixed = TRUE),
                        function(x) all(x %in% c("contage", "contyrs")), logical(1L))
events <- audit[grepl("contage|contyrs", audit$changed_fields) & audit$eligible, ]
stopifnot(nrow(intro) == 29L, nrow(events) == 17L,
          length(unique(events$bpl)) == 16L,
          sum(events$is_introduction) == 14L, sum(events$is_pure) == 4L)
for (i in which(events$is_pure)) {
  req <- crd_event_requirements(events[i, ], policy)
  stopifnot(identical(req$required_increment, req$continuation_increment))
}
d <- crd_event_cells(events, policy, cells)
stopifnot(length(unique(d$event_id)) == 17L,
          all(table(unique(d[c("event_id", "k")])$event_id) == 6L),
          sum(d$n) == 1485537L,
          sum(d$n * d$scientist) == 2889L)

# Known effects survive event aggregation and event-specific local slopes.
fixture <- expand.grid(bpl = 1:8, k = -3:2, person = 1:5)
fixture$event_id <- factor(paste0("event_", fixture$bpl))
fixture$x <- fixture$k + 0.5
fixture$post <- as.integer(fixture$k >= 0L)
fixture$n <- 1L
fixture$kernel_weight <- 1 - abs(fixture$x) / 3
fixture$required_increment <- fixture$post
fixture$continuation_increment <- fixture$post
fixture$school_years <- 8 + fixture$bpl / 4 + fixture$x * (0.02 + fixture$bpl / 100) +
  0.4 * fixture$post + fixture$person / 100
fixture$scientist <- 0.002 * fixture$school_years +
  fixture$bpl / 1000 + 0.0001 * fixture$x
fixture <- crd_add_slopes(fixture)
fit <- crd_fit(fixture)
stopifnot(abs(unname(coef(fit$first)[["post"]]) - 0.4) < 1e-9,
          abs(unname(coef(fit$fuzzy)[["fit_school_years"]]) - 0.002) < 1e-9)
message("continuation-school RD audit and estimator checks passed")
