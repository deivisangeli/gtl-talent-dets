# Check the event-card inputs without writing plots or rerunning estimation.
source("analysis/mandatory_schooling/estimate_linked_1940_schooling_1950_scientist_continuation_rd.R",
       local = TRUE)
source("analysis/mandatory_schooling/cohort_rd_state_plot_helpers.R", local = TRUE)

root <- rd_data_root()
cls <- file.path(root, "output/mandatory_schooling/cls")
iv <- file.path(root, "results/mandatory_schooling/linked_1940_schooling_1950_scientist_iv")
results <- file.path(root, "results/mandatory_schooling")
cells <- read.csv(file.path(iv, "main_cells.csv"))
laws <- read.csv(file.path(cls, "cls_cohort_requirements_1875_1912.csv"))
onset <- read.csv(file.path(cls, "cls_law_onset_by_state.csv"))
policy <- read.csv(file.path(cls, "cls_state_age_limits_1880_1930.csv"))

first_events <- first_law_cutoffs(laws, onset)
first <- make_window(cells, laws, first_events, h = 6L)
first$event_id <- paste0("first_", first$bpl)
first$event_year <- first_events$cls_first_year_in_force[
  match(first$bpl, first_events$bpl)]
first$event_kind <- "first law"
f <- ms_state_grid_points(first, "ca_years_combined", 6L)
stopifnot(nrow(f$events) == 14L, nrow(f$points) == 14L * 12L * 3L,
          all(table(f$points$event_id) == 36L))

continuation_dir <- file.path(results,
  "linked_1940_schooling_1950_scientist_continuation_rd")
reforms <- read.csv(file.path(continuation_dir, "eligible_continuation_reforms.csv"))
later <- crd_event_cells(reforms, policy, cells)
later$event_year <- later$reform_year
later$event_kind <- ifelse(later$is_continuation_only,
                           "continuation only", "bundled")
c <- ms_state_grid_points(later, "required_increment", 3L)
stopifnot(nrow(c$events) == 17L, nrow(c$points) == 17L * 6L * 3L,
          all(table(c$points$event_id) == 18L),
          sum(c$events$statename == "Ohio") == 2L,
          sum(c$events$event_kind == "continuation only") == 4L)

for (sample in list(list(data = first, output = f, outcome = "ca_years_combined",
                         measure = "Required years"),
                    list(data = later, output = c, outcome = "scientist",
                         measure = "Scientists / 100k"))) {
  d <- sample$data
  event <- as.character(d$event_id[[1L]])
  k <- d$k[[1L]]
  x <- d[as.character(d$event_id) == event & d$k == k, ]
  point <- sample$output$points[
    sample$output$points$event_id == event &
      sample$output$points$k == k &
      sample$output$points$measure == sample$measure, ]
  scale <- if (sample$measure == "Scientists / 100k") 1e5 else 1
  stopifnot(nrow(point) == 1L, point$men == sum(x$n),
            isTRUE(all.equal(point$value,
                             sum(x$n * x[[sample$outcome]]) / sum(x$n) * scale)))
}
message("state RD plot data checks passed")
