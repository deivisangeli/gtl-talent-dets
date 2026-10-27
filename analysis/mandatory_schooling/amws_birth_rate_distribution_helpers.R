# Shared machinery for the AMWS per-1,000-births county DISTRIBUTION figures.
#
# The rate itself is never computed here. It comes from
# amws_map_build_period_rates() in amws_county_map_helpers.R, the same
# ratio-of-sums aggregation the county maps use, so the histograms and the maps
# describe one measure on one universe.
#
# These helpers add only what a distribution needs on top of that: the
# cross-county summary, the thin-denominator flag, the faceted histogram and the
# Dropbox-safe save. The figure builder is generalised over a `group` column so
# one series (national) and two series (late vs early adopters) share a code
# path.

# Contiguous US plus DC. Excludes Alaska (02), Hawaii (15) and the territories,
# matching the CONUS universe of the existing county maps.
AMWS_DIST_CONUS_FIPS <- c(
  "01", "04", "05", "06", "08", "09", "10", "11", "12", "13",
  "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
  "26", "27", "28", "29", "30", "31", "32", "33", "34", "35",
  "36", "37", "38", "39", "40", "41", "42", "44", "45", "46",
  "47", "48", "49", "50", "51", "53", "54", "55", "56"
)

# Values above the cap are squished into the final bin so panels share a
# readable x-axis. The count above the cap is always reported, never dropped.
AMWS_DIST_RATE_CAP <- 2.0
AMWS_DIST_BIN_WIDTH <- 0.05

# A county-interval below this many estimated births carries a denominator too
# small for its rate to mean much: two scientists over 212 estimated births
# reads as 9.44 per 1,000. Flagged, never filtered - the same treatment the
# county bite measure gives `coverage_implausible` cells.
AMWS_DIST_MIN_BIRTHS <- 2000

### paths.R still defaults to a coauthor home directory, so fall back the same
### way the other mandatory-schooling scripts do rather than introducing a
### fourth path convention.
amws_dist_resolve_data_root <- function(repo_root) {
  config <- new.env()
  sys.source(file.path(repo_root, "paths.R"), config)
  candidates <- c(
    Sys.getenv("TALENT_DETS_DATA_DIR", unset = ""),
    config$TALENT_DETS_DATA_DIR,
    Sys.getenv("DET_DIR", unset = ""),
    file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets")
  )
  candidates <- candidates[nzchar(candidates)]
  hit <- candidates[dir.exists(candidates)]
  if (!length(hit)) stop("Cannot resolve the data root; set TALENT_DETS_DATA_DIR.")
  normalizePath(hit[1L], winslash = "/", mustWork = TRUE)
}

# Resolve the repository root from --file=, else walk up from GTL_REPO/getwd()
# until paths.R is found, so the callers run from any working directory.
amws_dist_resolve_repo_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1L]), winslash = "/"))
  } else {
    Sys.getenv("GTL_REPO", unset = getwd())
  }
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (parent == candidate) stop("Cannot resolve repository root; set GTL_REPO.")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
}

# The collected check ledger: every check runs, the results are written and
# printed, and only then does the caller fail. Returns the recorder and a
# reader over the accumulated rows.
amws_dist_check_ledger <- function() {
  checks <- list()
  expect <- function(label, ok, detail = "") {
    checks[[length(checks) + 1L]] <<- data.frame(
      check = label, validation_passed = isTRUE(ok), detail = detail,
      stringsAsFactors = FALSE
    )
    invisible(ok)
  }
  list(expect = expect, results = function() do.call(rbind, checks))
}

# Per-interval (and, when present, per-group) cross-county summary.
#
# pooled_rate_per_1000 is the births-weighted rate - total numerator over total
# denominator. county_mean is the unweighted mean across counties, which is what
# the histogram actually draws. They differ materially and both are reported.
amws_dist_summarise <- function(period_rates,
                                rate_cap = AMWS_DIST_RATE_CAP,
                                min_births = AMWS_DIST_MIN_BIRTHS) {
  by_cols <- intersect(c("group", "period"), names(period_rates))
  summary <- period_rates[, {
    rate <- amws_per_1000_births
    thin <- estimated_births < min_births
    .(
      n_counties             = .N,
      n_counties_positive    = sum(n_amws > 0),
      zero_share             = mean(n_amws == 0),
      total_amws             = sum(n_amws),
      total_estimated_births = sum(estimated_births),
      pooled_rate_per_1000   = 1000 * sum(n_amws) / sum(estimated_births),
      county_mean            = mean(rate),
      county_sd              = stats::sd(rate),
      county_median          = stats::median(rate),
      p25                    = unname(stats::quantile(rate, 0.25)),
      p75                    = unname(stats::quantile(rate, 0.75)),
      p90                    = unname(stats::quantile(rate, 0.90)),
      p99                    = unname(stats::quantile(rate, 0.99)),
      max                    = max(rate),
      n_above_cap            = sum(rate > rate_cap),
      n_thin_denominator     = sum(thin),
      n_above_cap_thin       = sum(rate > rate_cap & thin)
    )
  }, by = by_cols]
  summary[]
}

# The faceted histogram.
#
# `groups` is a spec table with columns group, label, colour and style
# ("fill" or "outline"). One row reproduces the single-series national figure;
# two rows overlay a filled series against an outlined reference series.
amws_dist_build_figure <- function(period_rates,
                                   period_summary,
                                   groups,
                                   period_levels,
                                   title,
                                   subtitle,
                                   caption,
                                   x_label,
                                   rate_cap = AMWS_DIST_RATE_CAP,
                                   bin_width = AMWS_DIST_BIN_WIDTH,
                                   annotation = NULL) {
  stopifnot(all(c("group", "label", "colour", "style") %in% names(groups)))
  stopifnot(all(groups$style %in% c("fill", "outline")))

  plot_data <- data.table::copy(period_rates)
  plot_data[, rate_plot := pmin(amws_per_1000_births, rate_cap)]
  plot_data[, period := factor(as.character(period), levels = period_levels)]
  plot_data[, group := factor(as.character(group), levels = groups$group)]

  group_labels <- stats::setNames(groups$label, groups$group)
  group_colours <- stats::setNames(groups$colour, groups$group)

  figure <- ggplot2::ggplot()

  # Outlined series are drawn first so the filled series reads on top of them.
  for (style in c("outline", "fill")) {
    spec <- groups[groups$style == style, , drop = FALSE]
    if (!nrow(spec)) next
    layer_data <- plot_data[group %in% spec$group]
    if (identical(style, "fill")) {
      figure <- figure + ggplot2::geom_histogram(
        data = layer_data,
        ggplot2::aes(x = rate_plot, fill = group),
        binwidth = bin_width, boundary = 0, position = "identity",
        colour = "white", linewidth = 0.15
      )
    } else {
      figure <- figure + ggplot2::geom_histogram(
        data = layer_data,
        ggplot2::aes(x = rate_plot, colour = group),
        binwidth = bin_width, boundary = 0, position = "identity",
        fill = NA, linewidth = 0.35
      )
    }
  }

  if (!is.null(annotation)) {
    annotation <- data.table::copy(annotation)
    annotation[, period := factor(as.character(period), levels = period_levels)]
    figure <- figure + ggplot2::geom_text(
      data = annotation,
      ggplot2::aes(x = Inf, y = Inf, label = label),
      inherit.aes = FALSE, hjust = 1.03, vjust = 1.3,
      size = 3.1, colour = "#333333", lineheight = 1.2
    )
  }

  # Each aesthetic keys only the series that actually uses it, or both scales
  # emit a full legend and the figure carries two clipped copies of it.
  fill_breaks <- groups$group[groups$style == "fill"]
  colour_breaks <- groups$group[groups$style == "outline"]

  figure <- figure +
    ggplot2::scale_fill_manual(
      values = group_colours, breaks = fill_breaks,
      labels = unname(group_labels[fill_breaks]), name = NULL, drop = FALSE
    )
  if (length(colour_breaks)) {
    figure <- figure + ggplot2::scale_colour_manual(
      values = group_colours, breaks = colour_breaks,
      labels = unname(group_labels[colour_breaks]), name = NULL, drop = FALSE
    )
  }

  figure +
    ggplot2::facet_wrap(~ period, ncol = 1) +
    ggplot2::scale_x_continuous(
      # No `limits`: rate_plot is already capped with pmin, and a hard limit
      # clips the boundary bin rather than showing it.
      breaks = seq(0, rate_cap, by = 0.25),
      labels = function(x) {
        ifelse(
          !is.na(x) & x >= rate_cap,
          paste0("\u2265", formatC(x, format = "f", digits = 2)),
          formatC(x, format = "f", digits = 2)
        )
      },
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      # pseudo-log, not log10: a plain log scale draws a count-of-1 bar at
      # height log10(1) = 0, i.e. invisible, which would erase the very tail
      # these figures exist to show. It also keeps an honest zero baseline.
      trans = scales::pseudo_log_trans(sigma = 1, base = 10),
      breaks = c(0, 1, 3, 10, 30, 100, 300, 1000, 3000),
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::labs(
      title = title, subtitle = subtitle, caption = caption,
      x = x_label, y = "Counties (pseudo-log scale)"
    ) +
    # A single filled series needs no legend at all; the title names it.
    ggplot2::guides(
      fill = if (nrow(groups) > 1L) ggplot2::guide_legend(order = 1) else "none",
      colour = if (length(colour_breaks)) {
        ggplot2::guide_legend(order = 2)
      } else {
        "none"
      }
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "top",
      legend.justification = "left",
      plot.caption = ggplot2::element_text(
        hjust = 0, colour = "#333333", lineheight = 1.2
      ),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "#333333")
    )
}

# Render outside Dropbox: the graphics device can otherwise fail when its
# destination is momentarily held by the sync client. Copy only closed files.
amws_dist_save_figure <- function(figure, output_stem, width = 10, height = 9) {
  for (extension in c("png", "pdf")) {
    temporary <- tempfile(fileext = paste0(".", extension))
    withCallingHandlers(
      ggplot2::ggsave(temporary, figure, width = width, height = height,
                      dpi = 300, limitsize = FALSE, bg = "white"),
      warning = function(w) stop(conditionMessage(w))
    )
    destination <- paste0(output_stem, ".", extension)
    copied <- FALSE
    for (attempt in 1:3) {
      copied <- file.copy(temporary, destination, overwrite = TRUE)
      if (copied) break
      Sys.sleep(0.5)
    }
    if (!copied) stop("Could not save figure: ", destination)
    unlink(temporary)
  }
  written <- paste0(output_stem, c(".png", ".pdf"))
  if (!all(file.exists(written)) || any(file.size(written) == 0)) {
    stop("Figure files missing or empty: ", paste(written, collapse = ", "))
  }
  written
}
