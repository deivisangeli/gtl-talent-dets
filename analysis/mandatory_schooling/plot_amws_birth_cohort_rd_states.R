# State birth-cohort plots for the first-law and continuation-reform cutoffs.
# Run from anywhere: Rscript analysis/mandatory_schooling/plot_amws_birth_cohort_rd_states.R
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[[1L]])), "..", ".."),
                winslash = "/", mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
roots <- unique(c(Sys.getenv("TALENT_DETS_DATA_DIR"), TALENT_DETS_DATA_DIR,
                  file.path(Sys.getenv("USERPROFILE"), "Globtalent Dropbox",
                            "gtl_talent_dets")))
roots <- roots[nzchar(roots) & dir.exists(roots)]
if (!length(roots)) stop("Could not locate the Dropbox project data root")
data_root <- normalizePath(roots[[1L]], winslash = "/")
result_root <- file.path(data_root, "results", "mandatory_schooling")
output_dir <- file.path(result_root, "amws_birth_cohort_rd_states")
project_figures <- file.path(repo_root, "project", "figures")
stopifnot(dir.exists(project_figures))

first_file <- file.path(result_root, "linked_1940_schooling_1950_scientist_rd",
                        "first_law_cutoffs.csv")
reform_file <- file.path(result_root,
                         "linked_1940_schooling_1950_scientist_continuation_rd",
                         "eligible_continuation_reforms.csv")
panel_file <- file.path(data_root, "output", "us_panel_county_amws_combined_year.csv")
stopifnot(all(file.exists(c(first_file, reform_file, panel_file))))

first <- fread(first_file)[is_first_law == TRUE & pre_cohorts >= 6L &
                             post_cohorts >= 6L]
first[, `:=`(design = "first_law", event_id = paste0("first_", bpl),
             event_year = cls_first_year_in_force, event_kind = "first law")]
reforms <- fread(reform_file)[eligible == TRUE]
reforms[, `:=`(design = "continuation", event_id = paste(bpl, reform_year, sep = "_"),
               event_year = reform_year,
               event_kind = fifelse(is_continuation_only,
                                   "continuation only", "bundled"))]
events <- rbindlist(list(
  first[, .(design, event_id, bpl, statename, event_year, cutoff_yob, event_kind)],
  reforms[, .(design, event_id, bpl, statename, event_year, cutoff_yob, event_kind)]
))
stopifnot(nrow(first) == 14L, nrow(reforms) == 17L,
          !anyDuplicated(events[, .(design, event_id)]))

panel <- fread(panel_file,
               select = c("GEOID", "year", "n_amws", "county_births_estimate_year"),
               colClasses = c(GEOID = "character"))
stopifnot(!anyDuplicated(panel[, .(GEOID, year)]))
panel <- panel[year >= 1885L & year <= 1912L]
panel[, bpl := as.integer(substr(GEOID, 1L, 2L))]
panel <- panel[bpl %in% events$bpl]
state_year <- panel[, .(
  amws_births = sum(n_amws, na.rm = TRUE),
  estimated_births = sum(county_births_estimate_year, na.rm = TRUE),
  counties = .N,
  invalid_rows = sum(!is.finite(n_amws) | n_amws < 0 |
                       !is.finite(county_births_estimate_year) |
                       county_births_estimate_year <= 0)
), by = .(bpl, year)]
stopifnot(!anyDuplicated(state_year[, .(bpl, year)]))

audit_design <- function(event_data, h) {
  rbindlist(lapply(seq_len(nrow(event_data)), function(i) {
    event <- event_data[i]
    needed <- seq.int(event$cutoff_yob - h, event$cutoff_yob + h - 1L)
    observed <- state_year[bpl == event$bpl & year %in% needed]
    missing <- setdiff(needed, observed$year)
    data.table(design = event$design, event_id = event$event_id,
               statename = event$statename, event_year = event$event_year,
               cutoff_yob = event$cutoff_yob, required_years = 2L * h,
               observed_years = nrow(observed),
               missing_years = paste(missing, collapse = ";"),
               invalid_rows = sum(observed$invalid_rows),
               included = !length(missing) && all(observed$invalid_rows == 0L) &&
                 all(observed$estimated_births > 0))
  }))
}
audit <- rbindlist(list(audit_design(events[design == "first_law"], 6L),
                        audit_design(events[design == "continuation"], 3L)))
stopifnot(sum(audit$included & audit$design == "first_law") == 13L,
          sum(audit$included & audit$design == "continuation") == 15L,
          identical(sort(audit[design == "first_law" & !included, statename]),
                    "Oklahoma"),
          identical(sort(audit[design == "continuation" & !included, statename]),
                    c("New Mexico", "Utah")))

points_design <- function(event_data, h) {
  kept <- event_data[event_id %in% audit[included == TRUE, event_id]]
  rbindlist(lapply(seq_len(nrow(kept)), function(i) {
    event <- kept[i]
    x <- copy(state_year[bpl == event$bpl &
                           year >= event$cutoff_yob - h &
                           year < event$cutoff_yob + h])
    setorder(x, year)
    stopifnot(identical(x$year,
                        seq.int(event$cutoff_yob - h,
                                event$cutoff_yob + h - 1L)),
              all(x$estimated_births > 0), all(x$invalid_rows == 0L))
    x[, `:=`(design = event$design, event_id = event$event_id,
             statename = event$statename, event_year = event$event_year,
             cutoff_yob = event$cutoff_yob, event_kind = event$event_kind,
             k = year - event$cutoff_yob,
             amws_per_1000_births = 1000 * amws_births / estimated_births)]
    x[, .(design, event_id, bpl, statename, event_year, cutoff_yob, event_kind,
          year, k, counties, amws_births, estimated_births,
          amws_per_1000_births)]
  }))
}
points <- rbindlist(list(points_design(events[design == "first_law"], 6L),
                         points_design(events[design == "continuation"], 3L)))
stopifnot(nrow(points) == 13L * 12L + 15L * 6L,
          all(is.finite(points$amws_per_1000_births)),
          all(points$amws_per_1000_births >= 0),
          isTRUE(all.equal(points$amws_per_1000_births,
                           1000 * points$amws_births / points$estimated_births)))

save_grid <- function(design_name, h, expected_events, output, title) {
  x <- points[design == design_name]
  groups <- unique(x[, .(event_id, statename, event_year, cutoff_yob, event_kind)])
  setorder(groups, statename, event_year)
  stopifnot(nrow(groups) == expected_events)
  cards <- lapply(seq_len(nrow(groups)), function(i) {
    event <- groups[i]
    d <- copy(x[event_id == event$event_id])
    d[, post := k >= 0L]
    fitted <- rbindlist(lapply(split(d, d$post), function(side) {
      fit <- lm(amws_per_1000_births ~ k, data = side,
                weights = estimated_births)
      side[, amws_per_1000_births := as.numeric(predict(fit, newdata = side))]
      side
    }))
    setorder(fitted, k)
    subtitle <- if (design_name == "first_law") {
      sprintf("Law %s | first exposed %s", event$event_year, event$cutoff_yob)
    } else {
      sprintf("%s reform | cohort %s | %s", event$event_year,
              event$cutoff_yob, event$event_kind)
    }
    ggplot(d, aes(k, amws_per_1000_births)) +
      geom_vline(xintercept = -0.5, linetype = "dashed",
                 colour = "firebrick", linewidth = 0.4) +
      geom_line(data = fitted,
                aes(k, amws_per_1000_births, group = post),
                inherit.aes = FALSE, colour = "#d97720", linewidth = 0.75) +
      geom_point(colour = "#174a75", size = 1.9) +
      scale_x_continuous(breaks = if (h == 6L) seq(-6, 4, by = 2) else -3:2) +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
      labs(title = event$statename, subtitle = subtitle,
           x = "Cohorts from first exposure", y = "AMWS / 1,000 births") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold", size = 15),
            plot.subtitle = element_text(size = 10),
            panel.grid.minor = element_blank(),
            plot.margin = grid::unit(c(8, 10, 8, 10), "pt"))
  })
  figure <- wrap_plots(cards, ncol = 5L) +
    plot_annotation(title = title,
                    subtitle = paste(
                      "Raw AMWS births per 1,000 estimated state births;",
                      "orange lines are descriptive birth-weighted fits.",
                      "The dashed line marks the cohort cutoff. Vertical scales vary by event."),
                    theme = theme(plot.title = element_text(size = 23, face = "bold"),
                                  plot.subtitle = element_text(size = 12)))
  ggsave(output, figure, width = 25, height = 17,
         units = "in", dpi = 200, limitsize = FALSE, bg = "white")
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(audit, file.path(output_dir, "coverage_audit.csv"))
fwrite(points, file.path(output_dir, "state_cohort_points.csv"))
first_png <- file.path(output_dir, "first_law_amws_per_1000_births_by_state.png")
reform_png <- file.path(output_dir, "continuation_amws_per_1000_births_by_state.png")
save_grid("first_law", 6L, 13L, first_png,
          "First-law AMWS birth-cohort discontinuities by state")
save_grid("continuation", 3L, 15L, reform_png,
          "Continuation-reform AMWS discontinuities by state")
destinations <- c(
  file.path(project_figures, "mandatory_schooling_amws_first_law_by_state.png"),
  file.path(project_figures, "mandatory_schooling_amws_continuation_by_state.png")
)
sources <- c(first_png, reform_png)
stopifnot(all(file.copy(sources, destinations, overwrite = TRUE)),
          all(unname(tools::md5sum(sources)) ==
                unname(tools::md5sum(destinations))))
message("Wrote AMWS state RD figures and audits to ", output_dir)
