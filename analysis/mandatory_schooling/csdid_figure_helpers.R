###############################################################################
# Figure helpers shared by the annual and five-year compulsory-schooling C&S
# runners and by the population-controlled variants.
#
# Every default reproduces the annual figures exactly; the five-year runner
# passes a cadence with step = 5 and anticipation = 2 periods. The state-level
# runner passes unit = "state", which drops the county row from the summary box.
###############################################################################

csdid_figure_counts <- function(targets, audit, weights) {
  controls <- audit[role == "later_adopter"]
  stopifnot(!anyDuplicated(targets$id), !anyDuplicated(controls$id),
            setequal(targets$g, weights$g), sum(weights$n_counties) == nrow(targets))
  list(treated_counties = uniqueN(targets$id),
       treated_states = uniqueN(targets$state_fips),
       control_counties = uniqueN(controls$id),
       control_states = uniqueN(controls$state_fips),
       events = uniqueN(weights$g))
}

# A single shared header keeps the summary outside the panels, at a fixed
# physical size even on the tall per-cohort pages. No new plotting dependency.
csdid_figure_header <- function(plot, outcome_label, pre, counts, control_label = NULL,
                                cadence = csdid_cadence(), unit = "county") {
  stopifnot(unit %in% c("county", "state"))
  number <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  text <- function(label, x, y, size = 9, just = "left", bold = FALSE) {
    grid::textGrob(label, x = x, y = y, just = c(just, "centre"),
      gp = grid::gpar(fontsize = size, fontface = if (bold) "bold" else "plain",
                      col = "#333333"))
  }
  heading <- grid::grobTree(
    text(if (unit == "state") "Compulsory schooling event study (state level)"
         else "Compulsory schooling event study", 0, 0.84, 14),
    text(paste(strwrap(outcome_label, width = 95), collapse = "\n"), 0, 0.54, 10.5),
    text(paste0(cadence$label, " | ", pre, " pre-law years | Reference: ", cadence$ref),
         0, 0.29, 10),
    text(if (is.null(control_label)) "" else control_label, 0, 0.07, 9)
  )
  # With states as the unit the county row would repeat the state row.
  rows <- if (unit == "state") c(states = 0.56, events = 0.26) else
    c(counties = 0.63, states = 0.41, events = 0.19)
  box <- do.call(grid::grobTree, c(list(
    grid::rectGrob(gp = grid::gpar(fill = "white", col = "#c9cdd1", lwd = 0.7)),
    text("Treated", 0.64, 0.86, 8.5, "right", TRUE),
    text("Control pool", 0.96, 0.86, 8.5, "right", TRUE)),
    if (unit == "county") list(
      text("Counties", 0.04, rows[["counties"]], 8.5),
      text(number(counts$treated_counties), 0.64, rows[["counties"]], 9, "right"),
      text(number(counts$control_counties), 0.96, rows[["counties"]], 9, "right")),
    list(
      text("States/DC", 0.04, rows[["states"]], 8.5),
      text(number(counts$treated_states), 0.64, rows[["states"]], 9, "right"),
      text(number(counts$control_states), 0.96, rows[["states"]], 9, "right"),
      text("Events (cohorts)", 0.04, rows[["events"]], 8.5),
      text(number(counts$events), 0.64, rows[["events"]], 9, "right"),
      vp = grid::viewport(height = grid::unit(0.67, "in"), y = 0.55))))
  header <- gtable::gtable(
    widths = grid::unit.c(grid::unit(1, "null"), grid::unit(0.18, "in"), grid::unit(2.65, "in")),
    heights = grid::unit(0.82, "in")
  )
  header <- gtable::gtable_add_grob(header, heading, t = 1, l = 1, clip = "off")
  header <- gtable::gtable_add_grob(header, box, t = 1, l = 3, clip = "off")
  body <- ggplotGrob(plot)
  left <- min(body$layout$l[grepl("^panel", body$layout$name)])
  body <- gtable::gtable_add_rows(body, grid::unit(0.82, "in"), pos = 0)
  gtable::gtable_add_grob(body, header, t = 1, l = left, r = ncol(body) - 1L,
                         clip = "off", name = "annual-summary-header")
}

plot_csdid_results <- function(coefficients, output_dir, pre, counts, per_cohort = FALSE,
                               control_label = NULL, cadence = csdid_cadence(),
                               post = 20L, outcomes = annual_outcomes, unit = "county",
                               targets_text = "Targets: laws adopted in 1850-1880.",
                               note = NULL) {
  step <- cadence$step
  ref <- cadence$ref
  for (outcome in names(outcomes)) {
    d <- coefficients[outcome_name == outcome]
    d[, specification_label := factor(specification_label,
      levels = c("Levels", "Cohort-linear detrended"))]
    p <- ggplot(d, aes(event_time, att)) +
      annotate("rect", xmin = ref + step / 2, xmax = -step / 2, ymin = -Inf, ymax = Inf,
               fill = "#f0c674", alpha = 0.22) +
      geom_hline(yintercept = 0, color = "grey45", linetype = "dashed") +
      geom_vline(xintercept = c(ref, 0), color = "grey50", linetype = "dotted") +
      geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.35 * step,
                    color = "#315a7d", linewidth = 0.35, na.rm = TRUE) +
      geom_point(size = 1.15, color = "#173e60", na.rm = TRUE) +
      scale_x_continuous(breaks = sort(unique(c(seq(-pre, post, 10), ref))),
                         limits = c(-pre - 0.7 * step, post + 0.7 * step)) +
      labs(x = cadence$x_label, y = "Estimated effect",
           caption = paste0("Shaded: ", cadence$shade_label, ". ",
                            "95% pointwise intervals; ",
                            if (unit == "state") "state-level" else "state-clustered",
                            " multiplier bootstrap.\n",
                            targets_text, " Trends use only unexposed years; ",
                            "detrended inference is conditional on fitted trends.",
                            if (is.null(note)) "" else paste0("\n", note))) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(), plot.caption = element_text(hjust = 0),
            strip.text = element_text(face = "bold"))
    if (per_cohort) {
      p <- p + facet_grid(cohort_label ~ specification_label, scales = "free_y")
      height <- 2.05 * uniqueN(d$g) + 2
    } else {
      p <- p + facet_wrap(~specification_label, ncol = 2)
      height <- if (is.null(note)) 6 else 6.2
    }
    p <- csdid_figure_header(p, outcomes[[outcome]], pre, counts, control_label, cadence, unit)
    stem <- file.path(output_dir, paste0(if (per_cohort) "cohort_" else "pooled_", outcome))
    # Render outside Dropbox: the graphics device can otherwise fail when its
    # destination is momentarily held by the sync client. Copy only closed files.
    for (extension in c("png", "pdf")) {
      temporary <- tempfile(fileext = paste0(".", extension))
      withCallingHandlers(
        ggsave(temporary, p, width = 13, height = height, dpi = 160,
               limitsize = FALSE, bg = "white"),
        warning = function(w) stop(conditionMessage(w)))
      destination <- paste0(stem, ".", extension)
      copied <- FALSE
      for (attempt in 1:3) {
        copied <- file.copy(temporary, destination, overwrite = TRUE)
        if (copied) break
        Sys.sleep(0.5)
      }
      if (!copied) stop("Could not save figure: ", destination)
      unlink(temporary)
    }
  }
}

# Call-site compatibility for the annual runner and the controlled variants.
annual_figure_counts <- csdid_figure_counts
plot_annual_results <- function(coefficients, output_dir, pre, counts, per_cohort = FALSE,
                                control_label = NULL, cadence = csdid_cadence(), post = 20L) {
  plot_csdid_results(coefficients, output_dir, pre, counts, per_cohort, control_label,
                     cadence, post)
}
