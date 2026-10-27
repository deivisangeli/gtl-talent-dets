# Raw birth-cohort discontinuity cards shared by the first-law and reform RDs.

ms_state_grid_points <- function(d, required_col, h, schooling_col = "school_years",
                                 schooling_label = "Schooling (years)", schooling_scale = 1) {
  columns <- c("event_id", "statename", "event_year", "cutoff_yob",
               "event_kind", "k", "n", schooling_col, "scientist", required_col)
  stopifnot(all(columns %in% names(d)), h >= 2L, all(d$n > 0))
  events <- unique(d[c("event_id", "statename", "event_year",
                       "cutoff_yob", "event_kind")])
  stopifnot(!anyDuplicated(events$event_id),
            all(table(unique(d[c("event_id", "k")])$event_id) == 2L * h))
  events <- events[order(events$statename, events$event_year), ]
  rownames(events) <- NULL
  measures <- c("Required years" = required_col,
                setNames(schooling_col, schooling_label),
                "Scientists / 100k" = "scientist")
  points <- lapply(seq_len(nrow(events)), function(i) {
    event <- events[i, ]
    x <- d[as.character(d$event_id) == as.character(event$event_id), ]
    stopifnot(identical(sort(unique(x$k)), seq.int(-h, h - 1L)))
    do.call(rbind, lapply(names(measures), function(measure) {
      variable <- measures[[measure]]
      cell <- data.frame(k = x$k, n = x$n, numerator = x$n * x[[variable]])
      pooled <- aggregate(cbind(n, numerator) ~ k, data = cell, FUN = sum)
      data.frame(event_id = as.character(event$event_id),
                 measure = measure, k = pooled$k, men = pooled$n,
                 value = pooled$numerator / pooled$n *
                   if (measure == "Scientists / 100k") 1e5 else
                     if (measure == schooling_label) schooling_scale else 1)
    }))
  })
  points <- do.call(rbind, points)
  points$measure <- factor(points$measure, levels = names(measures))
  rownames(points) <- NULL
  list(events = events, points = points)
}

ms_save_state_grid <- function(d, required_col, h, output, title,
                               required_note, ncol, width, height, expected_events,
                               schooling_col = "school_years",
                               schooling_label = "Schooling (years)", schooling_scale = 1) {
  plot_data <- ms_state_grid_points(d, required_col, h, schooling_col,
                                    schooling_label, schooling_scale)
  events <- plot_data$events
  points <- plot_data$points
  stopifnot(nrow(events) == expected_events,
            nrow(points) == expected_events * 2L * h * 3L)
  cards <- lapply(seq_len(nrow(events)), function(i) {
    event <- events[i, ]
    x <- points[points$event_id == as.character(event$event_id), ]
    x$post <- x$k >= 0L
    fitted <- do.call(rbind, lapply(split(x, list(x$measure, x$post), drop = TRUE),
                                    function(side) {
      fit <- stats::lm(value ~ k, data = side, weights = men)
      side$value <- as.numeric(stats::predict(fit, newdata = side))
      side
    }))
    if (event$event_kind == "first law") {
      subtitle <- sprintf("Law %s | first exposed %s",
                          event$event_year, event$cutoff_yob)
    } else {
      subtitle <- sprintf("%s reform | cohort %s | %s",
                          event$event_year, event$cutoff_yob, event$event_kind)
    }
    ggplot2::ggplot(x, ggplot2::aes(k, value)) +
      ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed",
                          colour = "firebrick", linewidth = 0.35) +
      ggplot2::geom_line(data = fitted,
                         ggplot2::aes(k, value, group = post),
                         inherit.aes = FALSE, colour = "#d97720", linewidth = 0.65) +
      ggplot2::geom_point(colour = "#174a75", size = 1.6) +
      ggplot2::facet_wrap(ggplot2::vars(measure), ncol = 1, scales = "free_y") +
      ggplot2::scale_x_continuous(breaks = if (h == 6L) seq(-6, 4, by = 2) else
                                    seq.int(-h, h - 1L)) +
      ggplot2::labs(title = as.character(event$statename), subtitle = subtitle,
                    x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14),
                     plot.subtitle = ggplot2::element_text(size = 10),
                     strip.text = ggplot2::element_text(size = 10, face = "bold"),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.spacing = grid::unit(0.2, "lines"),
                     plot.margin = grid::unit(c(5, 8, 5, 8), "pt"))
  })
  figure <- patchwork::wrap_plots(cards, ncol = ncol) +
    patchwork::plot_annotation(
      title = title,
      subtitle = paste(required_note,
                       "Raw person-weighted cohort means; orange lines are descriptive fits.",
                       "The dashed line lies between the last unexposed and first exposed cohort.",
                       "Vertical scales vary by event."),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold"),
                             plot.subtitle = ggplot2::element_text(size = 12)))
  ggplot2::ggsave(output, figure, width = width, height = height,
                  units = "in", dpi = 200, limitsize = FALSE, bg = "white")
  invisible(plot_data)
}
