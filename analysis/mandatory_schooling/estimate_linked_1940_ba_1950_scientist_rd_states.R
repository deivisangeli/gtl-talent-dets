# BA-completion first-law and continuation RDs plus the two-page state-card deck.
# Rscript analysis/mandatory_schooling/estimate_linked_1940_ba_1950_scientist_rd_states.R
# Optional overrides: MS_BA_RD_RESULTS_ROOT, MS_BA_RD_PROJECT_DIR.
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[[1L]]))) else
  file.path(Sys.getenv("GTL_REPO", unset = getwd()), "analysis/mandatory_schooling")
source(file.path(script_dir, "estimate_linked_1940_schooling_1950_scientist_continuation_rd.R"),
       local = TRUE)
source(file.path(script_dir, "cohort_rd_state_plot_helpers.R"), local = TRUE)

ba_rd_completion <- function(years) as.integer(years >= 16L)
ba_rd_label <- "BA-equivalent completion (%)"

ba_rd_fit <- function(d, continuation = FALSE, infer = TRUE) {
  if (continuation) crd_fit(d, infer = infer, endogenous = "ba_completed", allow_unidentified = TRUE)
  else fit_local_models(d, endogenous = "ba_completed", allow_unidentified = TRUE)
}

ba_rd_coefficient <- function(fit, term, outcome, sample, infer = TRUE) {
  tab <- if (inherits(fit, "fixest")) summary(fit)$coeftable else NULL
  valid <- !is.null(tab) && term %in% rownames(tab) && is.finite(tab[term, 1L])
  msg <- if (valid) "" else if (inherits(fit, "error")) conditionMessage(fit) else
    "Coefficient absent or nonfinite; no identified estimate"
  estimate <- if (valid) unname(tab[term, 1L]) else NA_real_
  se <- if (valid && infer) unname(tab[term, 2L]) else NA_real_
  p <- if (valid && infer) unname(tab[term, 4L]) else NA_real_
  data.frame(sample = sample, outcome = outcome, term = term,
    estimate = estimate, se = se, p_value = p,
    effect_percentage_points = if (outcome == "ba_completion") 100 * estimate else NA_real_,
    se_percentage_points = if (outcome == "ba_completion") 100 * se else NA_real_,
    effect_per_100k = if (outcome %in% c("reduced_form", "fuzzy_rd")) 1e5 * estimate else NA_real_,
    se_per_100k = if (outcome %in% c("reduced_form", "fuzzy_rd")) 1e5 * se else NA_real_,
    status = if (!valid) "unidentified" else if (infer) "estimated" else "descriptive_only",
    message = msg)
}

ba_rd_rows <- function(fits, sample, infer = TRUE) {
  rows <- list(ba_rd_coefficient(fits$required, "post", "required_years", sample, infer),
    ba_rd_coefficient(fits$first, "post", "ba_completion", sample, infer),
    ba_rd_coefficient(fits$reduced, "post", "reduced_form", sample, infer))
  if (!is.null(fits$continuation)) rows <- append(rows, list(
    ba_rd_coefficient(fits$continuation, "post", "continuation_years", sample, infer)))
  if (infer) rows <- append(rows, list(
    ba_rd_coefficient(fits$fuzzy, "fit_ba_completed", "fuzzy_rd", sample)))
  do.call(rbind, rows)
}

ba_rd_diagnostics <- function(d, fits, sample, infer = TRUE) {
  fs <- ba_rd_coefficient(fits$first, "post", "ba_completion", sample, infer)
  rf <- ba_rd_coefficient(fits$reduced, "post", "reduced_form", sample, infer)
  data.frame(sample = sample, states = length(unique(d$bpl)),
    events = if ("event_id" %in% names(d)) length(unique(d$event_id)) else length(unique(d$bpl)),
    observations = sum(d$n), scientist_count = sum(d$n * d$scientist),
    ba_completions = sum(d$n * d$ba_completed), completion_share = sum(d$n * d$ba_completed) / sum(d$n),
    first_stage_F = if (is.finite(fs$se) && fs$se > 0) (fs$estimate / fs$se)^2 else NA_real_,
    first_stage_p = fs$p_value, anderson_rubin_zero_p = rf$p_value,
    cluster = if (infer) "birth_state" else "descriptive_only")
}

ba_rd_assert_close <- function(actual, expected, label) {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-8))) {
    stop(paste("Original RD mismatch:", label), call. = FALSE)
  }
}

ba_rd_validate_baseline <- function(rows, old, continuation = FALSE) {
  for (sample in unique(rows$sample)) {
    for (outcome in c("required_years", "reduced_form",
                       if (continuation) "continuation_years")) {
      new <- rows[rows$sample == sample & rows$outcome == outcome, ]
      previous <- if (continuation) old[old$sample == sample & old$outcome ==
        if (outcome == "reduced_form") "scientist" else outcome, ] else
        old[old$model == outcome, ]
      stopifnot(nrow(new) == 1L, nrow(previous) == 1L)
      fields <- if (sample == "continuation_only_4") "estimate" else c("estimate", "se", "p_value")
      for (field in fields) ba_rd_assert_close(new[[field]], previous[[field]],
                                               paste(sample, outcome, field))
    }
  }
}

ba_rd_write_ar <- function(d, fits, out_dir, continuation = FALSE) {
  ar <- tryCatch(if (continuation) crd_ar(d, limit = 1, n_grid = 2001L,
                                           endogenous = "ba_completed") else
    ar_confidence_set_rd(d, attr(d, "slopes"), limit = 1, n_grid = 2001L,
                          endogenous = "ba_completed"), error = identity)
  if (inherits(ar, "error")) {
    rd_write(data.frame(status = "failed", message = conditionMessage(ar)),
             file.path(out_dir, "anderson_rubin_status.csv"))
    return(invisible(NULL))
  }
  stopifnot(nrow(ar$grid) == 2001L, all(is.finite(ar$grid$p_value)))
  rf <- ba_rd_coefficient(fits$reduced, "post", "reduced_form", "primary")
  ba_rd_assert_close(ar$grid$p_value[ar$grid$beta == 0], rf$p_value, "AR zero")
  intervals <- ar$intervals
  if (!"lower" %in% names(intervals)) {
    intervals$lower <- intervals$lower_per_100k / 1e5
    intervals$upper <- intervals$upper_per_100k / 1e5
  }
  intervals$lower_boundary_reached <- is.na(intervals$lower)
  intervals$upper_boundary_reached <- is.na(intervals$upper)
  intervals$search_lower <- rep(-1, nrow(intervals))
  intervals$search_upper <- rep(1, nrow(intervals))
  intervals$confidence <- rep(0.95, nrow(intervals))
  intervals$cluster <- rep("birth_state", nrow(intervals))
  rd_write(ar$grid, file.path(out_dir, "anderson_rubin_grid.csv"))
  rd_write(intervals, file.path(out_dir, "anderson_rubin_intervals.csv"))
  rd_write(data.frame(status = "estimated", message = "AR zero matches reduced form; finite grid may miss narrow accepted regions"),
           file.path(out_dir, "anderson_rubin_status.csv"))
}

ba_rd_state_contrasts <- function(d, continuation = FALSE) {
  ids <- if (continuation) as.character(d$event_id) else as.character(d$bpl)
  do.call(rbind, lapply(unique(ids), function(id) {
    x <- d[ids == id, ]
    jump <- function(y) unname(coef(lm(as.formula(paste(y, "~post+x+post:x")),
                                        data = x, weights = kernel_weight))[["post"]])
    data.frame(event_id = id, bpl = x$bpl[[1L]], statename = x$statename[[1L]],
      cutoff_yob = x$cutoff_yob[[1L]], men = sum(x$n), scientists = sum(x$n * x$scientist),
      required_jump = jump(if (continuation) "required_increment" else "ca_years_combined"),
      ba_completion_jump_percentage_points = 100 * jump("ba_completed"),
      scientist_jump_per_100k = 1e5 * jump("scientist"),
      note = "Descriptive within-event contrasts; no event-specific causal ratio or inference")
  }))
}

ba_rd_first_sensitivity <- function(cells, laws, events, primary) {
  rows <- list()
  append_fit <- function(d, label, h) {
    fits <- ba_rd_fit(d)
    r <- ba_rd_rows(fits, label)
    r$window <- h; r$states <- length(unique(d$bpl)); r$men <- sum(d$n)
    r$scientists <- sum(d$n * d$scientist)
    rows[[length(rows) + 1L]] <<- r
  }
  for (h in 2:6) for (spec in c("fixed_14_states", "all_supported_states")) {
    ev <- events
    if (spec == "fixed_14_states") ev$is_first_law <- ev$is_first_law & ev$bpl %in% unique(primary$bpl)
    append_fit(make_window(cells, laws, ev, h), spec, h)
  }
  zero <- primary; zero$x <- zero$k
  for (s in sort(unique(zero$bpl))) {
    zero[[paste0("pre_", s)]] <- zero$x * (zero$post == 0L & zero$bpl == s)
    zero[[paste0("post_", s)]] <- zero$x * (zero$post == 1L & zero$bpl == s)
  }
  append_fit(zero, "cutoff_at_first_exposed_year", 6L)
  for (s in sort(unique(primary$bpl))) append_fit(primary[primary$bpl != s, ], paste0("drop_state_", s), 6L)
  do.call(rbind, rows)
}

ba_rd_write_deck <- function(project_dir) {
  reference <- file.path(rd_repo_root(), "project/mandatory_schooling_scientist_rd_states.tex")
  doc <- readLines(reference)
  stopifnot(sum(grepl("\\includegraphics", doc, fixed = TRUE)) == 2L,
            sum(doc == "\\clearpage") == 1L)
  doc <- gsub("mandatory_schooling_scientist_first_law_by_state.png",
               "mandatory_schooling_ba_scientist_first_law_by_state.png", doc, fixed = TRUE)
  doc <- gsub("mandatory_schooling_scientist_continuation_reforms_by_state.png",
               "mandatory_schooling_ba_scientist_continuation_reforms_by_state.png", doc, fixed = TRUE)
  doc <- gsub("completed schooling in 1940",
    "BA-equivalent completion in 1940 (16+ completed years, in percent; degree award not observed)",
    doc, fixed = TRUE)
  writeLines(doc, file.path(project_dir, "mandatory_schooling_ba_scientist_rd_states.tex"))
}

ba_rd_main <- function() {
  root <- rd_data_root(); repo <- rd_repo_root()
  results <- file.path(root, "results/mandatory_schooling")
  destination <- Sys.getenv("MS_BA_RD_RESULTS_ROOT", unset = results)
  project <- Sys.getenv("MS_BA_RD_PROJECT_DIR", unset = file.path(repo, "project"))
  first_dir <- file.path(destination, "linked_1940_ba_1950_scientist_rd")
  reform_dir <- file.path(destination, "linked_1940_ba_1950_scientist_continuation_rd")
  old_first <- file.path(results, "linked_1940_schooling_1950_scientist_rd")
  old_reform <- file.path(results, "linked_1940_schooling_1950_scientist_continuation_rd")
  cls <- file.path(root, "output/mandatory_schooling/cls")
  iv <- file.path(results, "linked_1940_schooling_1950_scientist_iv")
  files <- c(cells = file.path(iv, "main_cells.csv"), flow = file.path(iv, "sample_flow.csv"),
    laws = file.path(cls, "cls_cohort_requirements_1875_1912.csv"),
    onset = file.path(cls, "cls_law_onset_by_state.csv"),
    policy = file.path(cls, "cls_state_age_limits_1880_1930.csv"),
    first_events = file.path(old_first, "first_law_cutoffs.csv"),
    first_estimates = file.path(old_first, "primary_estimates.csv"),
    first_summary = file.path(old_first, "sample_summary.csv"),
    reforms = file.path(old_reform, "eligible_continuation_reforms.csv"),
    reform_estimates = file.path(old_reform, "pooled_estimates.csv"),
    reform_summary = file.path(old_reform, "sample_summary.csv"))
  stopifnot(all(file.exists(files)))
  input <- lapply(files, read.csv, stringsAsFactors = FALSE)
  cells <- input$cells
  stopifnot(!anyNA(cells), all(cells$school_years %in% 0:17), all(cells$scientist %in% 0:1),
    all(is.finite(cells$n) & cells$n > 0 & cells$n == floor(cells$n)),
    !anyDuplicated(cells[setdiff(names(cells), "n")]),
    sum(cells$n) == input$flow$n[input$flow$step == "Pairs with observed 1950 occupation"],
    sum(cells$n * cells$scientist) == input$flow$n[input$flow$step == "1950 scientist occupations"])
  cells$ba_completed <- ba_rd_completion(cells$school_years)
  events <- first_law_cutoffs(input$laws, input$onset)
  stopifnot(isTRUE(all.equal(events, input$first_events, check.attributes = FALSE)))
  first <- make_window(cells, input$laws, events, h = 6L)
  stopifnot(length(unique(first$bpl)) == 14L)
  ba_rd_assert_close(c(sum(first$n), sum(first$n * first$scientist)),
    c(input$first_summary$rd_men, input$first_summary$rd_scientists), "first-law counts")
  reforms <- input$reforms
  stopifnot(nrow(reforms) == 17L, length(unique(reforms$bpl)) == 16L,
    sum(reforms$is_introduction) == 14L, sum(reforms$is_continuation_only) == 4L,
    sum(reforms$statename == "Ohio") == 2L)
  reform <- crd_add_slopes(crd_event_cells(reforms, input$policy, cells))
  samples <- list(all_17 = reform,
    introductions_14 = crd_add_slopes(reform[reform$is_introduction, ]),
    continuation_only_4 = crd_add_slopes(reform[reform$is_continuation_only, ]))
  summary <- do.call(rbind, lapply(names(samples), function(name) {
    d <- samples[[name]]
    data.frame(sample = name, events = length(unique(d$event_id)), states = length(unique(d$bpl)),
      men = sum(d$n), scientists = sum(d$n * d$scientist), cohorts_per_side = 3L)
  }))
  stopifnot(isTRUE(all.equal(summary, input$reform_summary, check.attributes = FALSE)))
  dir.create(first_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(reform_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(project, "figures"), recursive = TRUE, showWarnings = FALSE)
  message("Estimating BA first-law RD and validating original reduced form...")
  first_fit <- ba_rd_fit(first)
  first_rows <- ba_rd_rows(first_fit, "first_law_14")
  ba_rd_validate_baseline(first_rows, input$first_estimates)
  rd_write(first_rows, file.path(first_dir, "primary_estimates.csv"))
  rd_write(ba_rd_diagnostics(first, first_fit, "first_law_14"), file.path(first_dir, "diagnostics.csv"))
  rd_write(input$first_summary, file.path(first_dir, "sample_summary.csv"))
  rd_write(events, file.path(first_dir, "first_law_cutoffs.csv"))
  message("Estimating BA continuation-reform RDs and validating original reduced forms...")
  fits <- setNames(lapply(names(samples), function(name) ba_rd_fit(samples[[name]],
    continuation = TRUE, infer = name != "continuation_only_4")), names(samples))
  reform_rows <- do.call(rbind, lapply(names(fits), function(name)
    ba_rd_rows(fits[[name]], name, infer = name != "continuation_only_4")))
  ba_rd_validate_baseline(reform_rows, input$reform_estimates, continuation = TRUE)
  rd_write(reform_rows, file.path(reform_dir, "pooled_estimates.csv"))
  rd_write(do.call(rbind, lapply(names(fits), function(name) ba_rd_diagnostics(samples[[name]],
    fits[[name]], name, infer = name != "continuation_only_4"))), file.path(reform_dir, "diagnostics.csv"))
  rd_write(summary, file.path(reform_dir, "sample_summary.csv"))
  rd_write(reforms, file.path(reform_dir, "eligible_continuation_reforms.csv"))
  message("Inverting 2,001 first-law state-clustered AR tests...")
  ba_rd_write_ar(first, first_fit, first_dir)
  message("Inverting 2,001 continuation-reform state-clustered AR tests...")
  ba_rd_write_ar(reform, fits$all_17, reform_dir, continuation = TRUE)
  message("Running BA window, cutoff, influence, placebo and Calonico checks...")
  rd_write(ba_rd_first_sensitivity(cells, input$laws, events, first),
           file.path(first_dir, "window_and_state_sensitivity.csv"))
  rd_write(ba_rd_state_contrasts(first), file.path(first_dir, "state_cutoff_diagnostics.csv"))
  rd_write(ba_rd_state_contrasts(reform, TRUE), file.path(reform_dir, "event_diagnostics.csv"))
  influence <- do.call(rbind, lapply(sort(unique(reform$bpl)), function(state) {
    d <- crd_add_slopes(reform[reform$bpl != state, setdiff(names(reform), attr(reform, "slopes"))])
    rows <- ba_rd_rows(ba_rd_fit(d, TRUE), paste0("drop_state_", state))
    rows$states <- length(unique(d$bpl)); rows$events <- length(unique(d$event_id)); rows
  }))
  rd_write(influence, file.path(reform_dir, "leave_one_state_out.csv"))
  placebo <- first[first$k < 0, ]; placebo$k <- placebo$k + 3L
  placebo$x <- placebo$k + 0.5; placebo$post <- as.integer(placebo$k >= 0)
  placebo$kernel_weight <- placebo$n * (1 - abs(placebo$x) / 3)
  for (s in unique(placebo$bpl)) {
    placebo[[paste0("pre_", s)]] <- placebo$x * (placebo$post == 0 & placebo$bpl == s)
    placebo[[paste0("post_", s)]] <- placebo$x * (placebo$post == 1 & placebo$bpl == s)
  }
  placebo_model <- function(y) rd_optional_fit(fixest::feols(
    rd_model_formula(y, attr(first, "slopes")), data = placebo,
    weights = ~kernel_weight, cluster = ~bpl, ssc = fixest::ssc(K.adj = FALSE),
    notes = FALSE, warn = FALSE), allow_unidentified = TRUE)
  placebo_fit <- list(first = placebo_model("ba_completed"), reduced = placebo_model("scientist"))
  rd_write(rbind(ba_rd_coefficient(placebo_fit$first, "post", "ba_completion", "prelaw_placebo"),
                 ba_rd_coefficient(placebo_fit$reduced, "post", "reduced_form", "prelaw_placebo")),
           file.path(first_dir, "prelaw_placebo.csv"))
  rd_write(calonico_check(first, h = 6, endogenous = "ba_completed"),
           file.path(first_dir, "calonico_fuzzy_sensitivity.csv"))
  rd_write(crd_calonico(reform, endogenous = "ba_completed"), file.path(reform_dir, "calonico_diagnostic.csv"))
  message("Rendering BA state cards with the existing layout...")
  first$event_id <- paste0("first_", first$bpl)
  first$event_year <- events$cls_first_year_in_force[match(first$bpl, events$bpl)]
  first$event_kind <- "first law"
  reform$event_year <- reform$reform_year
  reform$event_kind <- ifelse(reform$is_continuation_only, "continuation only", "bundled")
  figures <- c(first = file.path(first_dir, "first_law_by_state.png"),
                reform = file.path(reform_dir, "continuation_reforms_by_state.png"))
  first_grid <- ms_save_state_grid(first, "ca_years_combined", 6L, figures[["first"]],
    "First-law cohort discontinuities by state: BA completion",
    "Required years are total CLS years; 14 states, six cohorts per side.",
    5L, 25, 19.8, 14L, "ba_completed", ba_rd_label, 100)
  reform_grid <- ms_save_state_grid(reform, "required_increment", 3L, figures[["reform"]],
    "Continuation-field reform discontinuities by state: BA completion",
    "Required years are increments from the full reform; 17 cutoffs, three cohorts per side.",
    6L, 27, 19.8, 17L, "ba_completed", ba_rd_label, 100)
  for (item in list(list(d = first, grid = first_grid, required = "ca_years_combined", h = 6L, dir = first_dir),
                   list(d = reform, grid = reform_grid, required = "required_increment", h = 3L, dir = reform_dir))) {
    old_grid <- ms_state_grid_points(item$d, item$required, item$h)
    keep <- as.character(item$grid$points$measure) != ba_rd_label
    old_keep <- as.character(old_grid$points$measure) != "Schooling (years)"
    ba_rd_assert_close(item$grid$points$value[keep], old_grid$points$value[old_keep], "unchanged state-card panels")
    rd_write(item$grid$points, file.path(item$dir, "state_card_points.csv"))
    rd_write(item$grid$events, file.path(item$dir, "state_card_events.csv"))
  }
  stopifnot(file.copy(figures, file.path(project, "figures", c(
    "mandatory_schooling_ba_scientist_first_law_by_state.png",
    "mandatory_schooling_ba_scientist_continuation_reforms_by_state.png")), overwrite = TRUE))
  ba_rd_write_deck(project)
  saveRDS(first_fit, file.path(first_dir, "models.rds"))
  saveRDS(fits, file.path(reform_dir, "models.rds"))
  script_files <- file.path(repo, "analysis/mandatory_schooling", c(
    "estimate_linked_1940_ba_1950_scientist_rd_states.R",
    "estimate_linked_1940_schooling_1950_scientist_rd.R",
    "estimate_linked_1940_schooling_1950_scientist_continuation_rd.R", "cohort_rd_state_plot_helpers.R"))
  source_files <- c(unname(files), script_files, file.path(repo, "project/mandatory_schooling_scientist_rd_states.tex"))
  provenance <- data.frame(path = normalizePath(source_files, winslash = "/"),
                           md5 = unname(tools::md5sum(source_files)))
  for (dir in c(first_dir, reform_dir)) {
    rd_write(provenance, file.path(dir, "input_provenance.csv"))
    rd_write(data.frame(check = c("sample_counts", "event_ids_and_windows", "required_years_models",
      "scientist_reduced_forms", "unchanged_state_card_panels"), passed = TRUE),
      file.path(dir, "baseline_validation.csv"))
    writeLines(c("BA-equivalent completion = 1(school_years >= 16), including postgraduate schooling; no degree award observed.",
      "Same main linked 1940-1950 IV population, valid schooling retained; scientist occupation remains observed in 1950.",
      "Existing first-law or continuation cutoffs instrument completion; birth-state clustered inference, K.adj=FALSE.",
      "Local linear fits use midpoint k=-0.5, state/event intercepts and side-specific slopes; triangular person weights.",
      "BA jumps in probability and percentage-point units; fuzzy effects are scientists per 100,000 men for 0-to-1 completion.",
      "Four-state continuation-only estimates are descriptive, with no fuzzy ratio, SEs, p-values or intervals.",
      "Primary AR set: 2001 grid points over [-1,1] probability units, crossing tolerance 1e-8; all detected intervals retained.",
      "Boundary flags mean search truncation, not proven infinite endpoints; grid may miss accepted regions narrower than 0.001.",
      "Identification failures are recorded, retaining the required-years and scientist models and all state cards.",
      "Calonico calculations are fixed-bandwidth diagnostics; the score has only six or three distinct cohorts per side.",
      "Cards use raw person-weighted cohort means and separate descriptive linear fits, without kernel weights or causal ratio labels.",
      "RD validity requires smooth counterfactual cohorts, relevant first stages, exclusion and monotonicity.",
      "BA-specific exclusion requires changes at other schooling margins to have no separate scientist effect.",
      "Selected links are not representative of all men; birth year inferred as 1940-AGE-1 has measurement error near cutoffs.",
      paste("Built:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))), file.path(dir, "notes.txt"))
    writeLines(capture.output(sessionInfo()), file.path(dir, "session_info.txt"))
  }
  print(first_rows)
  print(reform_rows)
  message("Wrote BA RD results and ", file.path(project, "mandatory_schooling_ba_scientist_rd_states.tex"))
}

if (sys.nframe() == 0L) ba_rd_main()
