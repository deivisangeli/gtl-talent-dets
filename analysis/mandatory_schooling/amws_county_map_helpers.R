# Shared builders for mandatory-schooling AMWS county maps.

amws_map_read_panel <- function(panel_file) {
  required_columns <- c(
    "GEOID", "year", "n_amws", "county_births_estimate_year"
  )
  panel <- data.table::fread(
    panel_file, colClasses = c(GEOID = "character")
  )
  missing_columns <- setdiff(required_columns, names(panel))
  if (length(missing_columns)) {
    stop(
      "Annual AMWS panel is missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  panel <- panel[, ..required_columns]
  panel[, `:=`(
    GEOID = sprintf("%05d", as.integer(GEOID)),
    year = as.integer(year),
    n_amws = as.numeric(n_amws),
    county_births_estimate_year = as.numeric(county_births_estimate_year)
  )]

  if (anyNA(panel[, .(GEOID, year, n_amws)])) {
    stop("Missing GEOID, year, or AMWS count in the annual panel.")
  }
  if (anyDuplicated(panel[, .(GEOID, year)])) {
    stop("Duplicate GEOID-year rows in the annual AMWS panel.")
  }
  if (any(!is.finite(panel$n_amws)) || any(panel$n_amws < 0)) {
    stop("AMWS counts must be finite and nonnegative.")
  }
  if (abs(sum(panel$n_amws) - 112291) > 1e-8) {
    stop("Annual AMWS total differs from the validated total of 112,291.")
  }
  panel[]
}

amws_map_build_period_rates <- function(
    panel, periods, eligible_state_fips = NULL) {
  required_period_columns <- c("period", "start_year", "end_year")
  missing_period_columns <- setdiff(required_period_columns, names(periods))
  if (length(missing_period_columns)) {
    stop(
      "Period specification is missing columns: ",
      paste(missing_period_columns, collapse = ", ")
    )
  }
  if (any(periods$start_year > periods$end_year)) {
    stop("Every period must have start_year <= end_year.")
  }

  selected_panel <- data.table::copy(panel)
  selected_panel[, state_fips := substr(GEOID, 1L, 2L)]
  if (!is.null(eligible_state_fips)) {
    eligible_state_fips <- unique(as.character(eligible_state_fips))
    selected_panel <- selected_panel[state_fips %in% eligible_state_fips]
    if (!nrow(selected_panel)) stop("The eligibility restriction selected no counties.")
    if (any(!selected_panel$state_fips %in% eligible_state_fips)) {
      stop("The restricted panel contains an ineligible state.")
    }
  }

  period_rates <- data.table::rbindlist(lapply(seq_len(nrow(periods)), function(i) {
    spec <- periods[i]
    selected <- selected_panel[
      year >= spec$start_year & year <= spec$end_year
    ]
    if (!nrow(selected)) stop("No panel rows for period ", spec$period)
    if (any(
      !is.finite(selected$county_births_estimate_year) |
        selected$county_births_estimate_year <= 0
    )) {
      stop("Invalid estimated-birth denominator within period ", spec$period)
    }

    aggregated <- selected[, .(
      n_amws = sum(n_amws),
      estimated_births = sum(county_births_estimate_year)
    ), by = .(GEOID, state_fips)]
    aggregated[, `:=`(
      period = spec$period,
      amws_per_1000_births = 1000 * n_amws / estimated_births
    )]

    observed <- aggregated[, .(
      amws = sum(n_amws),
      estimated_births = sum(estimated_births),
      counties = .N,
      positive_counties = sum(n_amws > 0)
    )]
    expected_columns <- c(
      "expected_amws", "expected_estimated_births",
      "expected_counties", "expected_positive_counties"
    )
    if (all(expected_columns %in% names(spec))) {
      if (
        abs(observed$amws - spec$expected_amws) > 1e-8 ||
          abs(observed$estimated_births - spec$expected_estimated_births) > 0.5 ||
          observed$counties != spec$expected_counties ||
          observed$positive_counties != spec$expected_positive_counties
      ) {
        stop("Validated aggregate changed for period ", spec$period)
      }
    }
    aggregated[]
  }))

  period_rates[, period := factor(period, levels = periods$period)]
  data.table::setorder(period_rates, period, GEOID)
  period_rates[]
}

amws_map_load_geometry <- function() {
  excluded_areas <- c("AK", "HI", "PR", "VI", "GU", "AS", "MP")
  county_shapes <- tigris::counties(
    cb = TRUE, resolution = "20m", year = 2020, progress_bar = FALSE
  )
  county_shapes <- county_shapes[
    !county_shapes[["STUSPS"]] %in% excluded_areas,
  ]
  county_shapes <- sf::st_transform(county_shapes, 5070)
  county_shapes$GEOID <- as.character(county_shapes$GEOID)

  state_shapes <- tigris::states(
    cb = TRUE, resolution = "20m", year = 2020, progress_bar = FALSE
  )
  state_shapes <- state_shapes[
    !state_shapes[["STUSPS"]] %in% excluded_areas,
  ]
  state_shapes <- sf::st_transform(state_shapes, 5070)
  list(counties = county_shapes, states = state_shapes)
}

amws_map_build_figure <- function(
    period_rates, periods, geometry, title, subtitle,
    eligible_state_fips = NULL) {
  county_shapes <- geometry$counties
  state_shapes <- geometry$states

  unmatched <- period_rates[
    !GEOID %in% county_shapes$GEOID,
    .(period, GEOID, n_amws, estimated_births)
  ]
  if (nrow(unmatched)) {
    stop(
      "AMWS period data contain GEOIDs absent from CONUS county geometry:\n",
      paste(capture.output(print(unmatched)), collapse = "\n")
    )
  }

  period_summary <- period_rates[, .(
    amws_births = sum(n_amws),
    estimated_births = sum(estimated_births),
    pooled_rate = 1000 * sum(n_amws) / sum(estimated_births),
    counties_with_denominator = .N,
    counties_with_amws = sum(n_amws > 0)
  ), by = period]

  rate_max <- max(period_rates$amws_per_1000_births)
  legend_breaks <- c(0, 0.03, 0.1, 0.3, 1, 3, 6, 10)
  legend_breaks <- legend_breaks[legend_breaks <= rate_max]

  make_period_map <- function(period_value) {
    rates <- period_rates[period == period_value]
    summary_row <- period_summary[period == period_value]
    idx <- match(county_shapes$GEOID, rates$GEOID)
    map_data <- county_shapes
    map_data$amws_per_1000_births <- rates$amws_per_1000_births[idx]

    p <- ggplot2::ggplot()
    if (is.null(eligible_state_fips)) {
      p <- p + ggplot2::geom_sf(
        data = map_data,
        ggplot2::aes(fill = amws_per_1000_births),
        color = "white", linewidth = 0.025
      )
    } else {
      eligible_counties <- county_shapes[
        substr(county_shapes$GEOID, 1L, 2L) %in% eligible_state_fips,
      ]
      eligible_idx <- match(eligible_counties$GEOID, rates$GEOID)
      eligible_counties$amws_per_1000_births <-
        rates$amws_per_1000_births[eligible_idx]
      rate_counties <- eligible_counties[
        !is.na(eligible_counties$amws_per_1000_births),
      ]
      p <- p +
        ggplot2::geom_sf(
          data = county_shapes,
          fill = "grey94", color = "white", linewidth = 0.025
        ) +
        ggplot2::geom_sf(
          data = eligible_counties,
          fill = "grey70", color = "white", linewidth = 0.025
        ) +
        ggplot2::geom_sf(
          data = rate_counties,
          ggplot2::aes(fill = amws_per_1000_births),
          color = "white", linewidth = 0.025
        )
    }

    p +
      ggplot2::geom_sf(
        data = state_shapes,
        fill = NA, color = "grey25", linewidth = 0.24
      ) +
      ggplot2::scale_fill_viridis_c(
        option = "viridis",
        limits = c(0, rate_max),
        breaks = legend_breaks,
        labels = scales::label_number(accuracy = 0.01, trim = TRUE),
        trans = scales::pseudo_log_trans(sigma = 0.01, base = 10),
        na.value = "grey88",
        name = "AMWS scientists born per 1,000 estimated births",
        guide = ggplot2::guide_colourbar(
          title.position = "top", title.hjust = 0.5,
          barwidth = grid::unit(12, "cm"),
          barheight = grid::unit(0.45, "cm")
        )
      ) +
      ggplot2::coord_sf(datum = NA) +
      ggplot2::labs(
        title = period_value,
        subtitle = sprintf(
          "Pooled rate: %.3f | AMWS births: %s | Counties with denominator: %s",
          summary_row$pooled_rate,
          scales::comma(summary_row$amws_births),
          scales::comma(summary_row$counties_with_denominator)
        )
      ) +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(
          size = 9.5, margin = ggplot2::margin(b = 5)
        ),
        plot.margin = ggplot2::margin(5, 5, 5, 5)
      )
  }

  plots <- lapply(periods$period, make_period_map)
  combined <- patchwork::wrap_plots(plots, ncol = 2L, guides = "collect") +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 17),
        plot.subtitle = ggplot2::element_text(size = 11)
      )
    ) &
    ggplot2::theme(legend.position = "bottom")

  list(plot = combined, summary = period_summary)
}

amws_map_save_figure <- function(figure, output_stem) {
  png_file <- paste0(output_stem, ".png")
  pdf_file <- paste0(output_stem, ".pdf")
  ggplot2::ggsave(
    png_file, figure,
    width = 15, height = 7.5, units = "in", dpi = 300, bg = "white"
  )
  ggplot2::ggsave(
    pdf_file, figure,
    width = 15, height = 7.5, units = "in", bg = "white"
  )
  outputs <- c(png_file, pdf_file)
  if (!all(file.exists(outputs)) || any(file.info(outputs)$size <= 0)) {
    stop("One or more map outputs were not written correctly.")
  }
  outputs
}
