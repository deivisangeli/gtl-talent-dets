# Continuation-school reform cohort cutoffs on the exact linked IV population.
# Run from the repository root with Rscript. Outputs are in Dropbox results.

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else
  file.path(getwd(), "analysis/mandatory_schooling/estimate_linked_1940_schooling_1950_scientist_continuation_rd.R")
source(file.path(dirname(script_path),
                 "estimate_linked_1940_schooling_1950_scientist_rd.R"), local = TRUE)

crd_assert <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)

crd_first_introductions <- function(policy) {
  first <- lapply(split(policy, policy$statefip), function(p) {
    p <- p[order(p$year), ]
    hit <- which(p$contage > 0L)
    if (!length(hit)) return(NULL)
    data.frame(bpl = p$statefip[[1L]], statename = p$statename[[1L]],
               introduction_year = p$year[hit[[1L]]])
  })
  do.call(rbind, Filter(Negate(is.null), first))
}

crd_event_requirements <- function(event, policy) {
  p <- policy[policy$statefip == event$bpl, ]
  p <- p[order(p$year), ]
  i <- match(event$reform_year, p$year)
  crd_assert(!is.na(i) && i > 1L, "Reform year absent from policy history")
  changed <- which(vapply(seq_len(nrow(p)), function(j)
    j > 1L && any(as.numeric(p[j, policy_columns]) !=
                  as.numeric(p[j - 1L, policy_columns])), logical(1L)))
  crd_assert(i %in% changed, "Reform year has no policy-field change")
  later <- changed[changed > i]
  last <- if (length(later)) later[[1L]] - 1L else nrow(p)
  actual <- calculate_cohort_requirements(p)
  full_cf <- p
  full_cf[i:last, policy_columns] <-
    p[rep(i - 1L, last - i + 1L), policy_columns]
  cont_cf <- p
  cont_cf[i:last, c("contage", "contyrs")] <-
    p[rep(i - 1L, last - i + 1L), c("contage", "contyrs")]
  full <- calculate_cohort_requirements(full_cf)
  cont <- calculate_cohort_requirements(cont_cf)
  data.frame(yob = actual$yob,
             required_actual = actual$ca_years_combined,
             required_increment = actual$ca_years_combined - full$ca_years_combined,
             continuation_increment =
               actual$ca_years_combined - cont$ca_years_combined)
}

crd_event_cells <- function(events, policy, cells, h = 3L) {
  out <- lapply(seq_len(nrow(events)), function(i) {
    event <- events[i, ]
    req <- crd_event_requirements(event, policy)
    cut <- as.integer(event$cutoff_yob)
    req <- req[req$yob >= cut - h & req$yob < cut + h, ]
    crd_assert(nrow(req) == 2L * h &&
                 all(req$required_increment[req$yob < cut] == 0L) &&
                 all(req$required_increment[req$yob >= cut] > 0L),
               paste("Reform step changed in", event$statename, event$reform_year))
    d <- merge(cells[cells$bpl == event$bpl, ], req, by = "yob")
    d$event_id <- paste(event$bpl, event$reform_year, sep = "_")
    d$statename <- event$statename
    d$reform_year <- event$reform_year
    d$cutoff_yob <- cut
    d$is_introduction <- event$is_introduction
    d$is_continuation_only <- event$is_continuation_only
    d$k <- d$yob - cut
    d$x <- d$k + 0.5
    d$post <- as.integer(d$k >= 0L)
    d$kernel_weight <- d$n * (1 - abs(d$x) / h)
    crd_assert(all(d$kernel_weight > 0) &&
                 length(unique(d$k)) == 2L * h,
               paste("Incomplete IV sample window in", d$event_id[[1L]]))
    d
  })
  d <- do.call(rbind, out)
  crd_assert(!anyDuplicated(unique(d[c("bpl", "yob", "event_id")])[c("bpl", "yob")]),
             "An IV birth-state/cohort is assigned to multiple reform windows")
  d$event_id <- factor(d$event_id, levels = unique(d$event_id))
  d
}

crd_add_slopes <- function(d) {
  d$event_id <- droplevels(d$event_id)
  ids <- levels(d$event_id)
  slopes <- unlist(lapply(seq_along(ids), function(i) paste0(c("pre_e", "post_e"), i)))
  for (i in seq_along(ids)) {
    event <- d$event_id == ids[[i]]
    d[[paste0("pre_e", i)]] <- d$x * event * (d$post == 0L)
    d[[paste0("post_e", i)]] <- d$x * event * (d$post == 1L)
  }
  attr(d, "slopes") <- slopes
  d
}

crd_formula <- function(outcome, slopes, iv = FALSE, endogenous = "school_years") {
  rhs <- paste(slopes, collapse = " + ")
  if (iv) as.formula(paste0(outcome, " ~ ", rhs,
                           " | event_id | ", endogenous, " ~ post"))
  else as.formula(paste0(outcome, " ~ post + ", rhs, " | event_id"))
}

crd_fit <- function(d, infer = TRUE, endogenous = "school_years", allow_unidentified = FALSE) {
  slopes <- attr(d, "slopes")
  fit <- function(formula) fixest::feols(
    formula, data = d, weights = ~kernel_weight,
    cluster = ~bpl, ssc = fixest::ssc(K.adj = FALSE),
    notes = FALSE, warn = FALSE)
  list(required = fit(crd_formula("required_increment", slopes)),
       continuation = fit(crd_formula("continuation_increment", slopes)),
       first = rd_optional_fit(fit(crd_formula(endogenous, slopes)), allow_unidentified),
       reduced = fit(crd_formula("scientist", slopes)),
       fuzzy = if (infer) rd_optional_fit(
         fit(crd_formula("scientist", slopes, TRUE, endogenous)), allow_unidentified) else NULL)
}

crd_coefficient <- function(fit, term, outcome, sample) {
  tab <- summary(fit)$coeftable
  crd_assert(term %in% rownames(tab), paste("Missing term", term, outcome))
  data.frame(sample = sample, outcome = outcome,
             estimate = unname(tab[term, 1L]), se = unname(tab[term, 2L]),
             p_value = unname(tab[term, 4L]),
             per_100k = if (outcome %in% c("scientist", "fuzzy"))
               1e5 * unname(tab[term, 1L]) else NA_real_)
}

crd_ar <- function(d, limit = 0.05, n_grid = 301L, endogenous = "school_years") {
  formula <- crd_formula("ar_outcome", attr(d, "slopes"))
  p_at <- function(beta) {
    d$ar_outcome <- d$scientist - beta * d[[endogenous]]
    fit <- fixest::feols(formula, data = d, weights = ~kernel_weight,
                         cluster = ~bpl, ssc = fixest::ssc(K.adj = FALSE),
                         notes = FALSE, warn = FALSE)
    unname(summary(fit)$coeftable["post", 4L])
  }
  beta <- seq(-limit, limit, length.out = n_grid)
  p <- vapply(beta, p_at, numeric(1L))
  accepted <- p >= 0.05
  starts <- which(diff(c(FALSE, accepted)) == 1L)
  ends <- which(diff(c(accepted, FALSE)) == -1L)
  intervals <- if (length(starts)) do.call(rbind, lapply(seq_along(starts), function(i) {
    lo <- if (starts[[i]] == 1L) NA_real_ else
      stats::uniroot(function(b) p_at(b) - 0.05,
                     c(beta[starts[[i]] - 1L], beta[starts[[i]]]), tol = 1e-8)$root
    hi <- if (ends[[i]] == length(beta)) NA_real_ else
      stats::uniroot(function(b) p_at(b) - 0.05,
                     c(beta[ends[[i]]], beta[ends[[i]] + 1L]), tol = 1e-8)$root
    data.frame(lower_per_100k = 1e5 * lo, upper_per_100k = 1e5 * hi)
  })) else data.frame(lower_per_100k = numeric(), upper_per_100k = numeric())
  list(grid = data.frame(beta = beta, p_value = p, accepted = accepted),
       intervals = intervals)
}

crd_calonico <- function(d, endogenous = "school_years") {
  covs <- stats::model.matrix(~event_id - 1L, data = d)[, -1L, drop = FALSE]
  warnings <- character()
  fit <- tryCatch(withCallingHandlers(
    rdrobust::rdrobust(y = d$scientist, x = d$k, c = -0.5,
                      fuzzy = d[[endogenous]], p = 1, q = 2,
                      h = 3, b = 3, kernel = "triangular", weights = d$n,
                      cluster = d$bpl, covs = covs, vce = "hc1",
                      masspoints = "check"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) e)
  if (inherits(fit, "error")) return(data.frame(
    estimate_per_100k = NA_real_, robust_p = NA_real_,
    robust_lower_per_100k = NA_real_, robust_upper_per_100k = NA_real_,
    unique_cohorts_each_side = 3L,
    message = paste(c(warnings, conditionMessage(fit)), collapse = " | ")))
  data.frame(estimate_per_100k = 1e5 * unname(fit$coef[1L, 1L]),
             robust_p = unname(fit$pv["Robust", 1L]),
             robust_lower_per_100k = 1e5 * unname(fit$ci["Robust", 1L]),
             robust_upper_per_100k = 1e5 * unname(fit$ci["Robust", 2L]),
             unique_cohorts_each_side = 3L,
             message = paste(unique(warnings), collapse = " | "))
}

crd_plot_points <- function(d, fits, n_boot = 999L) {
  measures <- c(required = "required_increment", schooling = "school_years",
                scientist = "scientist")
  states <- sort(unique(d$bpl))
  set.seed(20260926)
  draws <- replicate(n_boot, sample(states, length(states), replace = TRUE),
                     simplify = FALSE)
  points <- fitted <- list()
  for (measure in names(measures)) {
    outcome <- measures[[measure]]
    sc <- d[c("event_id", "bpl", "k", "n", outcome)]
    sc$num <- sc$n * sc[[outcome]]
    sc <- aggregate(cbind(n, num) ~ event_id + bpl + k, data = sc, FUN = sum)
    sc$value <- sc$num / sc$n
    event_num <- tapply(sc$num, sc$event_id, sum)
    event_n <- tapply(sc$n, sc$event_id, sum)
    event_mean <- event_num / event_n
    grand <- sum(sc$num) / sum(sc$n)
    sc$adjusted <- sc$value - event_mean[as.character(sc$event_id)] + grand
    for (variant in c("adjusted", "raw")) {
      for (k in sort(unique(sc$k))) {
        sub <- sc[sc$k == k, ]
        val <- if (variant == "raw") sub$value else sub$adjusted
        w <- if (variant == "raw") sub$n else
          event_n[as.character(sub$event_id)]
        boot <- vapply(draws, function(ids) {
          count <- tabulate(match(ids, states), nbins = length(states))
          weighted.mean(val, w * count[match(sub$bpl, states)])
        }, numeric(1L))
        points[[length(points) + 1L]] <- data.frame(
          measure = measure, variant = variant, k = k,
          value = weighted.mean(val, w),
          lower = unname(stats::quantile(boot, 0.025)),
          upper = unname(stats::quantile(boot, 0.975)), men = sum(sub$n))
      }
    }
    events <- unique(d[c("event_id", "k")])
    pred <- vapply(seq_len(nrow(events)), function(i) {
      row <- d[d$event_id == events$event_id[[i]] & d$k == events$k[[i]], ][1L, , drop = FALSE]
      as.numeric(stats::predict(fits[[measure]], newdata = row))
    }, numeric(1L))
    events$predicted <- pred - event_mean[as.character(events$event_id)] + grand
    events$weight <- event_n[as.character(events$event_id)]
    fitted[[measure]] <- data.frame(
      measure = measure, k = sort(unique(events$k)),
      value = vapply(sort(unique(events$k)), function(k) {
        sub <- events[events$k == k, ]
        weighted.mean(sub$predicted, sub$weight)
      }, numeric(1L)))
  }
  list(points = do.call(rbind, points), fitted = do.call(rbind, fitted))
}

crd_save_plots <- function(plot_data, estimates, out_dir) {
  labels <- c(required = "Extra required schooling years",
              schooling = "Completed schooling in 1940",
              scientist = "Scientist occupations in 1950 per 100,000")
  outcomes <- c(required = "required_years", schooling = "schooling",
                scientist = "scientist")
  for (variant in c("adjusted", "raw")) {
    panels <- lapply(names(labels), function(measure) {
      d <- plot_data$points[plot_data$points$variant == variant &
                              plot_data$points$measure == measure, ]
      line <- plot_data$fitted[plot_data$fitted$measure == measure, ]
      scale <- if (measure == "scientist") 1e5 else 1
      d[, c("value", "lower", "upper")] <- d[, c("value", "lower", "upper")] * scale
      line$value <- line$value * scale
      est <- estimates[estimates$sample == "all_17" &
                         estimates$outcome == outcomes[[measure]], ]
      d$post <- as.integer(d$k >= 0L)
      line$post <- as.integer(line$k >= 0L)
      if (variant == "raw") {
        raw_fit <- stats::lm(value ~ post + k + post:k, data = d, weights = men)
        line$value <- as.numeric(stats::predict(raw_fit, newdata = line))
      }
      ggplot2::ggplot(d, ggplot2::aes(k, value)) +
        ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", colour = "firebrick") +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                              width = 0.12, colour = "grey55", linewidth = 0.35) +
        ggplot2::geom_point(size = 2, colour = "#174a75") +
        ggplot2::geom_line(data = line, ggplot2::aes(k, value, group = post),
                           inherit.aes = FALSE, colour = "#d97720", linewidth = 0.8) +
        ggplot2::scale_x_continuous(breaks = -3:2) +
        ggplot2::labs(title = labels[[measure]],
                      subtitle = sprintf("Pooled adjusted jump: %+.2f (state-clustered SE %.2f)",
                                         est$estimate[[1L]] * scale, est$se[[1L]] * scale),
                      x = "Birth cohort relative to reform cutoff", y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })
    fig <- patchwork::wrap_plots(panels, ncol = 1) +
      patchwork::plot_annotation(
        title = paste("Continuation-field reform cutoffs:", variant),
        subtitle = "17 clean reforms in 16 states; state-bootstrap pointwise 95% intervals")
    ggplot2::ggsave(file.path(out_dir, paste0("continuation_reforms_", variant, ".pdf")),
                    fig, width = 10, height = 12)
    ggplot2::ggsave(file.path(out_dir, paste0("continuation_reforms_", variant, ".png")),
                    fig, width = 10, height = 12, dpi = 180)
  }
}

crd_save_pure_plot <- function(d, out_dir) {
  labels <- c(required_increment = "Extra required years",
              school_years = "Completed schooling", scientist = "Scientists per 100,000")
  pieces <- lapply(names(labels), function(outcome) {
    x <- d[c("statename", "k", "n", outcome)]
    x$num <- x$n * x[[outcome]]
    x <- aggregate(cbind(n, num) ~ statename + k, data = x, FUN = sum)
    x$value <- x$num / x$n * if (outcome == "scientist") 1e5 else 1
    x$measure <- labels[[outcome]]
    x$post <- x$k >= 0L
    x
  })
  points <- do.call(rbind, pieces)
  points$measure <- factor(points$measure, levels = unname(labels))
  fig <- ggplot2::ggplot(points, ggplot2::aes(k, value)) +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", colour = "firebrick") +
    ggplot2::geom_line(ggplot2::aes(group = post), colour = "#d97720", linewidth = 0.8) +
    ggplot2::geom_point(colour = "#174a75", size = 2) +
    ggplot2::facet_grid(measure ~ statename, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = -3:2) +
    ggplot2::labs(title = "Continuation-only reform cutoffs: raw cohort means",
                  subtitle = "Delaware 1921, Iowa 1919, New Jersey 1919, Ohio 1922",
                  x = "Birth cohort relative to reform cutoff", y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  ggplot2::ggsave(file.path(out_dir, "continuation_only_raw.pdf"), fig,
                  width = 14, height = 9)
  ggplot2::ggsave(file.path(out_dir, "continuation_only_raw.png"), fig,
                  width = 14, height = 9, dpi = 180)
}

crd_main <- function() {
  root <- rd_data_root()
  iv_dir <- file.path(root, "results/mandatory_schooling/linked_1940_schooling_1950_scientist_iv")
  cls_dir <- file.path(root, "output/mandatory_schooling/cls")
  out_dir <- file.path(root, "results/mandatory_schooling/linked_1940_schooling_1950_scientist_continuation_rd")
  paths <- c(cells = file.path(iv_dir, "main_cells.csv"),
             flow = file.path(iv_dir, "sample_flow.csv"),
             diagnostics = file.path(iv_dir, "diagnostics.csv"),
             laws = file.path(cls_dir, "cls_cohort_requirements_1875_1912.csv"),
             onset = file.path(cls_dir, "cls_law_onset_by_state.csv"),
             policy = file.path(cls_dir, "cls_state_age_limits_1880_1930.csv"))
  crd_assert(all(file.exists(paths)), paste("Missing inputs:",
              paste(paths[!file.exists(paths)], collapse = ", ")))
  input <- lapply(paths, function(path) read.csv(path, stringsAsFactors = FALSE))
  iv_men <- input$flow$n[input$flow$step == "Pairs with observed 1950 occupation"]
  iv_scientists <- input$diagnostics$scientist_count[
    input$diagnostics$sample == "linked_1940_grade" &
      input$diagnostics$cluster == "state"]
  crd_assert(length(iv_men) == 1L && length(iv_scientists) == 1L &&
               sum(input$cells$n) == iv_men &&
               sum(input$cells$n * input$cells$scientist) == iv_scientists,
             "Analysis cells do not reconcile to the main linked IV sample")

  introductions <- crd_first_introductions(input$policy)
  crd_assert(nrow(introductions) == 29L, "Expected 29 continuation-school introductions")
  audit <- audit_amendments(input$policy, input$laws, input$onset, input$cells)
  inventory <- audit$inventory
  inventory$is_continuation_change <- grepl("contage|contyrs", inventory$changed_fields)
  year <- introductions$introduction_year[match(inventory$bpl, introductions$bpl)]
  inventory$is_introduction <- inventory$reform_year == year
  inventory$is_continuation_only <- vapply(strsplit(inventory$changed_fields, ";", fixed = TRUE),
                                            function(x) all(x %in% c("contage", "contyrs")),
                                            logical(1L))
  events <- inventory[inventory$is_continuation_change & inventory$eligible, ]
  crd_assert(nrow(events) == 17L && length(unique(events$bpl)) == 16L &&
               sum(events$is_introduction) == 14L &&
               sum(events$is_continuation_only) == 4L,
             "Continuation reform inventory differs from the audited design")
  events <- events[order(events$cutoff_yob, events$bpl), ]
  d <- crd_event_cells(events, input$policy, input$cells)
  d <- crd_add_slopes(d)
  pure <- crd_add_slopes(d[d$is_continuation_only, ])
  introductions_d <- crd_add_slopes(d[d$is_introduction, ])
  crd_assert(length(unique(pure$bpl)) == 4L, "Continuation-only event count changed")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  binding <- vapply(seq_len(nrow(events)), function(i) {
    event <- events[i, ]
    req <- crd_event_requirements(event, input$policy)
    req$continuation_increment[req$yob == event$cutoff_yob] > 0L
  }, logical(1L))
  events$continuation_binding_at_cutoff <- binding
  rd_write(introductions, file.path(out_dir, "first_continuation_introductions.csv"))
  rd_write(inventory, file.path(out_dir, "all_reform_inventory.csv"))
  rd_write(events, file.path(out_dir, "eligible_continuation_reforms.csv"))
  samples <- list(all_17 = d, introductions_14 = introductions_d,
                  continuation_only_4 = pure)
  summary_rows <- lapply(names(samples), function(name) {
    x <- samples[[name]]
    data.frame(sample = name, events = length(unique(x$event_id)),
               states = length(unique(x$bpl)), men = sum(x$n),
               scientists = sum(x$n * x$scientist),
               cohorts_per_side = 3L)
  })
  rd_write(do.call(rbind, summary_rows), file.path(out_dir, "sample_summary.csv"))
  fits <- lapply(samples, function(x) crd_fit(x, infer = length(unique(x$bpl)) > 4L))
  estimates <- do.call(rbind, lapply(names(samples), function(name) {
    fit <- fits[[name]]
    rows <- list(
      crd_coefficient(fit$required, "post", "required_years", name),
      crd_coefficient(fit$continuation, "post", "continuation_years", name),
      crd_coefficient(fit$first, "post", "schooling", name),
      crd_coefficient(fit$reduced, "post", "scientist", name))
    if (!is.null(fit$fuzzy))
      rows[[length(rows) + 1L]] <-
        crd_coefficient(fit$fuzzy, "fit_school_years", "fuzzy", name)
    do.call(rbind, rows)
  }))
  estimates$se[estimates$sample == "continuation_only_4"] <- NA_real_
  estimates$p_value[estimates$sample == "continuation_only_4"] <- NA_real_
  rd_write(estimates, file.path(out_dir, "pooled_estimates.csv"))

  ar <- crd_ar(d)
  rd_write(ar$grid, file.path(out_dir, "anderson_rubin_grid.csv"))
  rd_write(ar$intervals, file.path(out_dir, "anderson_rubin_intervals.csv"))
  rd_write(crd_calonico(d), file.path(out_dir, "calonico_diagnostic.csv"))

  per_event <- do.call(rbind, lapply(levels(d$event_id), function(id) {
    x <- d[d$event_id == id, ]
    per <- function(y) unname(stats::coef(stats::lm(
      stats::as.formula(paste(y, "~ post + x + post:x")), data = x,
      weights = kernel_weight))[["post"]])
    data.frame(event_id = id, statename = x$statename[[1L]],
               reform_year = x$reform_year[[1L]],
               is_continuation_only = x$is_continuation_only[[1L]],
               men = sum(x$n), scientists = sum(x$n * x$scientist),
               required_jump = per("required_increment"),
               schooling_jump = per("school_years"),
               scientist_jump_per_100k = 1e5 * per("scientist"))
  }))
  rd_write(per_event, file.path(out_dir, "event_diagnostics.csv"))
  influence <- do.call(rbind, lapply(sort(unique(d$bpl)), function(state) {
    x <- crd_add_slopes(d[d$bpl != state, setdiff(names(d), attr(d, "slopes"))])
    fit <- crd_fit(x)
    data.frame(dropped_state = state, events = length(unique(x$event_id)),
               states = length(unique(x$bpl)),
               schooling_jump = unname(coef(fit$first)[["post"]]),
               scientist_jump_per_100k = 1e5 * unname(coef(fit$reduced)[["post"]]),
               fuzzy_per_100k = 1e5 * unname(coef(fit$fuzzy)[["fit_school_years"]]))
  }))
  rd_write(influence, file.path(out_dir, "leave_one_state_out.csv"))

  plot_data <- crd_plot_points(d, list(required = fits$all_17$required,
                                        schooling = fits$all_17$first,
                                        scientist = fits$all_17$reduced))
  rd_write(plot_data$points, file.path(out_dir, "discontinuity_points.csv"))
  rd_write(plot_data$fitted, file.path(out_dir, "discontinuity_adjusted_fit.csv"))
  crd_save_plots(plot_data, estimates, out_dir)
  crd_save_pure_plot(pure, out_dir)
  source(file.path(rd_repo_root(), "analysis/mandatory_schooling/cohort_rd_state_plot_helpers.R"),
         local = TRUE)
  state_grid <- d
  state_grid$event_year <- state_grid$reform_year
  state_grid$event_kind <- ifelse(state_grid$is_continuation_only,
                                  "continuation only", "bundled")
  ms_save_state_grid(state_grid, required_col = "required_increment", h = 3L,
                     output = file.path(out_dir, "continuation_reforms_by_state.png"),
                     title = "Continuation-field reform discontinuities by state",
                     required_note = "Required years are increments from the full reform; 17 cutoffs, three cohorts per side.",
                     ncol = 6L, width = 27, height = 19.8, expected_events = 17L)
  saveRDS(fits, file.path(out_dir, "models.rds"))
  writeLines(c(
    "All analyses use the exact main linked 1940-schooling / 1950-occupation IV population.",
    "The 17-event sample has 14 first introductions and three later revisions in 16 states.",
    "The four continuation-only changes are Delaware 1921, Iowa 1919, New Jersey 1919, and Ohio 1922.",
    "The 17-event pooled jump is a full law-package effect; changed fields often include other rules.",
    "Each reform has three annual birth cohorts on either side; the cutoff is between -1 and 0.",
    "Required-year increments compare actual law to retaining all pre-reform fields until the next change.",
    "Continuation-specific increments retain only the old continuation fields under enacted other rules.",
    "Fits use event intercepts, event-specific slopes, triangular person weights, and state clustering.",
    "Four-state continuation-only estimates are descriptive; no ratio, p-value, or interval is reported.",
    "The Calonico calculation is diagnostic only: just three distinct birth cohorts occur per side.",
    "Plot intervals are pointwise bootstrap summaries from resampling whole states, not model intervals."
  ), file.path(out_dir, "notes.txt"))
  message("Wrote continuation RD results to ", out_dir)
  print(do.call(rbind, summary_rows))
  print(estimates)
}

if (sys.nframe() == 0L) crd_main()
