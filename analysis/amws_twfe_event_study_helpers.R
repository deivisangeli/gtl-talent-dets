###############################################################################
# TWFE helpers for AMWS county-pair event studies
###############################################################################

twfe_safe_filename <- function(x) {
 stringr::str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

run_twfe_event_study <- function(data, outcome, timing_name, fe_type,
                                 window = 70) {
 fe_rhs <- switch(
  fe_type,
  stack_county = "stack_unit_num + decade",
  geoid = "GEOID + decade",
  stop("Unknown FE type: ", fe_type)
 )

 data_es <- data %>%
  dplyr::select(
   stack_unit_num, GEOID, decade, treatment_decade, sample_role, event_id,
   dplyr::all_of(outcome)
  ) %>%
  dplyr::rename(y = dplyr::all_of(outcome)) %>%
  dplyr::mutate(
   event_time = decade - treatment_decade,
   treated_twfe = as.integer(sample_role == "treated")
  ) %>%
  dplyr::filter(
   !is.na(y),
   !is.na(event_time),
   event_time >= -window,
   event_time <= window
  )

 if (dplyr::n_distinct(data_es$treated_twfe) < 2) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   timing = timing_name,
   fe_type = fe_type,
   error = "Sample does not contain both treated and control observations."
  ))
 }

 if (!(-10 %in% unique(data_es$event_time[data_es$treated_twfe == 1]))) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   timing = timing_name,
   fe_type = fe_type,
   error = "Reference period event_time = -10 is absent for treated units."
  ))
 }

 if (dplyr::n_distinct(data_es$y, na.rm = TRUE) < 2) {
  return(list(
   ok = FALSE,
   outcome = outcome,
   timing = timing_name,
   fe_type = fe_type,
   error = "Outcome has insufficient variation."
  ))
 }

 tryCatch(
  {
   fml <- stats::as.formula(
    paste0("y ~ i(event_time, treated_twfe, ref = -10) | ", fe_rhs)
   )

   model <- fixest::feols(
    fml,
    data = data_es,
    cluster = ~event_id,
    warn = FALSE
   )

   list(
    ok = TRUE,
    outcome = outcome,
    timing = timing_name,
    fe_type = fe_type,
    model = model,
    n_rows = nrow(data_es),
    n_units = dplyr::n_distinct(data_es$stack_unit_num),
    n_counties = dplyr::n_distinct(data_es$GEOID),
    n_events = dplyr::n_distinct(data_es$event_id),
    n_treated_units =
     dplyr::n_distinct(data_es$stack_unit_num[data_es$treated_twfe == 1]),
    n_control_units =
     dplyr::n_distinct(data_es$stack_unit_num[data_es$treated_twfe == 0]),
    min_event_time = min(data_es$event_time, na.rm = TRUE),
    max_event_time = max(data_es$event_time, na.rm = TRUE),
    min_decade = min(data_es$decade, na.rm = TRUE),
    max_decade = max(data_es$decade, na.rm = TRUE),
    error = NA_character_
   )
  },
  error = function(e) {
   list(
    ok = FALSE,
    outcome = outcome,
    timing = timing_name,
    fe_type = fe_type,
    error = conditionMessage(e)
   )
  }
 )
}

extract_twfe_dynamic <- function(model_result) {
 model <- model_result$model
 coefs <- stats::coef(model)

 if (length(coefs) == 0) {
  return(tibble::tibble())
 }

 ct <- as.data.frame(fixest::coeftable(model))
 ct$term <- rownames(ct)

 se_col <- intersect(c("Std. Error", "Cluster s.e.", "S.E."), names(ct))[1]

 ct %>%
  tibble::as_tibble() %>%
  dplyr::filter(stringr::str_detect(term, "^event_time::")) %>%
  dplyr::mutate(
   event_time = as.integer(stringr::str_match(term, "^event_time::(-?\\d+)")[, 2]),
   outcome = model_result$outcome,
   timing = model_result$timing,
   fe_type = model_result$fe_type,
   estimate = Estimate,
   se = .data[[se_col]],
   ci_low = estimate - 1.96 * se,
   ci_high = estimate + 1.96 * se
  ) %>%
  dplyr::select(
   outcome, timing, fe_type, event_time, estimate, se, ci_low, ci_high, term
  ) %>%
  dplyr::arrange(outcome, timing, fe_type, event_time)
}

plot_twfe_dynamic <- function(dynamic_att, outcome, timing_name, fe_type,
                              title_prefix) {
 data_plot <- dynamic_att %>%
  dplyr::filter(
   outcome == !!outcome,
   timing == !!timing_name,
   fe_type == !!fe_type
  )

 y_values <- data_plot %>%
  dplyr::select(estimate, ci_low, ci_high) %>%
  unlist(use.names = FALSE)

 y_values <- y_values[is.finite(y_values)]
 max_abs <- max(abs(y_values), na.rm = TRUE)

 if (!is.finite(max_abs) || max_abs == 0) {
  max_abs <- 1
 }

 data_plot <- data_plot %>%
  dplyr::mutate(post = as.factor(as.integer(event_time >= 0)))

 ggplot2::ggplot(
  data_plot,
  ggplot2::aes(
   x = event_time,
   y = estimate,
   ymin = ci_low,
   ymax = ci_high
  )
 ) +
  ggplot2::geom_point(ggplot2::aes(colour = post), size = 1.5) +
  ggplot2::geom_errorbar(ggplot2::aes(colour = post), width = 0.1) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::scale_x_continuous(
   breaks = sort(unique(data_plot$event_time)),
   labels = as.character(sort(unique(data_plot$event_time)))
  ) +
  ggplot2::scale_y_continuous(
   limits = c(-1.1 * max_abs, 1.1 * max_abs)
  ) +
  ggplot2::scale_color_manual(
   drop = FALSE,
   values = c("#e87d72", "#56bcc2"),
   breaks = c(0, 1),
   labels = c("Pre", "Post")
  ) +
  ggplot2::labs(
   x = "Relative Time",
   y = "Effect",
   title = stringr::str_wrap(
    paste(title_prefix, "-", outcome, timing_name, fe_type),
    width = 72
   ),
   color = NULL
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
   plot.title = ggplot2::element_text(
    color = "darkgray",
    face = "bold",
    size = 12
   ),
   axis.title = ggplot2::element_text(
    color = "darkgray",
    face = "bold",
    size = 12
   ),
   strip.background = ggplot2::element_rect(fill = "white", color = "white"),
   strip.text = ggplot2::element_text(
    color = "darkgray",
    face = "bold",
    size = 12,
    hjust = 0
   ),
   legend.position = "bottom"
  )
}

run_and_export_twfe_event_studies <- function(panel_by_timing, outcomes, window,
                                              results_subdir_path,
                                              title_prefix) {
 fe_types <- c("stack_county", "geoid")
 twfe_results <- list()

 for (timing_name in names(panel_by_timing)) {
  panel_timing <- panel_by_timing[[timing_name]]

  for (outcome in outcomes) {
   for (fe_type in fe_types) {
    message("Running AMWS TWFE event study: ", outcome, " | ",
            timing_name, " | ", fe_type)

    twfe_results[[paste(outcome, timing_name, fe_type, sep = "__")]] <-
     run_twfe_event_study(
      data = panel_timing,
      outcome = outcome,
      timing_name = timing_name,
      fe_type = fe_type,
      window = window
     )
   }
  }
 }

 twfe_dynamic <- purrr::imap_dfr(
  purrr::keep(twfe_results, "ok"),
  ~ extract_twfe_dynamic(.x)
 )

 twfe_status <- purrr::imap_dfr(
  twfe_results,
  ~ tibble::tibble(
   model = .y,
   outcome = .x$outcome,
   timing = .x$timing,
   fe_type = .x$fe_type,
   ok = .x$ok,
   n_rows = ifelse(isTRUE(.x$ok), .x$n_rows, NA_integer_),
   n_units = ifelse(isTRUE(.x$ok), .x$n_units, NA_integer_),
   n_counties = ifelse(isTRUE(.x$ok), .x$n_counties, NA_integer_),
   n_events = ifelse(isTRUE(.x$ok), .x$n_events, NA_integer_),
   n_treated_units =
    ifelse(isTRUE(.x$ok), .x$n_treated_units, NA_integer_),
   n_control_units =
    ifelse(isTRUE(.x$ok), .x$n_control_units, NA_integer_),
   min_event_time = ifelse(isTRUE(.x$ok), .x$min_event_time, NA_real_),
   max_event_time = ifelse(isTRUE(.x$ok), .x$max_event_time, NA_real_),
   min_decade = ifelse(isTRUE(.x$ok), .x$min_decade, NA_real_),
   max_decade = ifelse(isTRUE(.x$ok), .x$max_decade, NA_real_),
   error = .x$error
  )
 )

 for (fe_type in fe_types) {
  readr::write_csv(
   twfe_dynamic %>% dplyr::filter(fe_type == !!fe_type),
   results_subdir_path(paste0("twfe_", fe_type, "_dynamic.csv")),
   na = ""
  )

  readr::write_csv(
   twfe_status %>% dplyr::filter(fe_type == !!fe_type),
   results_subdir_path(paste0("twfe_", fe_type, "_model_status.csv")),
   na = ""
  )
 }

 for (model_result in purrr::keep(twfe_results, "ok")) {
  model_att <- twfe_dynamic %>%
   dplyr::filter(
    outcome == model_result$outcome,
    timing == model_result$timing,
    fe_type == model_result$fe_type
   )

  if (nrow(model_att) == 0) {
   next
  }

  plot_twfe <- plot_twfe_dynamic(
   twfe_dynamic,
   model_result$outcome,
   model_result$timing,
   model_result$fe_type,
   title_prefix
  )

  ggplot2::ggsave(
   filename = results_subdir_path(
    paste0(
     "twfe_", model_result$fe_type, "_",
     twfe_safe_filename(model_result$outcome), "_",
     twfe_safe_filename(model_result$timing), ".png"
    )
   ),
   plot = plot_twfe,
   width = 8,
   height = 6,
   dpi = 300
  )
 }

 list(
  results = twfe_results,
  dynamic = twfe_dynamic,
  status = twfe_status
 )
}
