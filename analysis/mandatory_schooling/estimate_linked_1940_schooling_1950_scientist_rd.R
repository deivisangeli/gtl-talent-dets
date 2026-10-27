# Local birth-cohort cutoff analysis on the exact main linked-census IV sample.
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/estimate_linked_1940_schooling_1950_scientist_rd.R

suppressPackageStartupMessages({
  library(fixest)
  library(ggplot2)
  library(patchwork)
  library(rdrobust)
})

rd_fail <- function(condition, message) if (isTRUE(condition)) stop(message, call. = FALSE)

rd_repo_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(dirname(dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]),
                                          winslash = "/"))))
  } else Sys.getenv("GTL_REPO", unset = getwd())
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    rd_fail(identical(parent, candidate), "Could not find paths.R")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
}

rd_data_root <- function(root = rd_repo_root()) {
  env <- new.env(parent = baseenv())
  sys.source(file.path(root, "paths.R"), envir = env)
  candidates <- c(Sys.getenv("TALENT_DETS_DATA_DIR"),
                  get("TALENT_DETS_DATA_DIR", envir = env),
                  Sys.getenv("DET_DIR"),
                  file.path("C:/Users", Sys.info()[["user"]],
                            "Globtalent Dropbox/gtl_talent_dets"))
  hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
  rd_fail(!length(hit), "Could not locate Dropbox project root")
  normalizePath(hit[[1L]], winslash = "/")
}

rd_write <- function(x, path) utils::write.csv(x, path, row.names = FALSE, na = "")

first_law_cutoffs <- function(laws, onset, min_year = 1901L) {
  laws <- laws[laws$yob %in% 1885:1912, ]
  laws <- laws[order(laws$bpl, laws$yob), ]
  rd_fail(anyDuplicated(laws[c("bpl", "yob")]) > 0L || nrow(laws) != 49L * 28L,
          "Expected 49 states/jurisdictions x 28 unique cohorts")
  laws$previous <- ave(laws$ca_years_combined, laws$bpl,
                       FUN = function(x) c(NA_integer_, head(x, -1L)))
  candidates <- laws[!is.na(laws$previous) & laws$previous == 0L &
                       laws$ca_years_combined > 0L,
                     c("bpl", "statename", "yob", "ca_years_combined")]
  names(candidates)[names(candidates) == "yob"] <- "cutoff_yob"
  candidates <- merge(candidates, onset[, c("statefip", "cls_first_year_in_force",
                                              "has_repeal")],
                      by.x = "bpl", by.y = "statefip", all.x = TRUE,
                      sort = FALSE)
  rd_fail(anyNA(candidates$cls_first_year_in_force), "Missing first law dates")
  candidates$is_first_law <- candidates$cls_first_year_in_force >= min_year &
    !candidates$has_repeal
  candidates$pre_cohorts <- candidates$cutoff_yob - 1885L
  candidates$post_cohorts <- 1912L - candidates$cutoff_yob + 1L
  candidates[order(candidates$cutoff_yob, candidates$bpl), ]
}

make_window <- function(cells, laws, events, h = 6L) {
  keep <- events[events$is_first_law & events$pre_cohorts >= h &
                   events$post_cohorts >= h, c("bpl", "statename", "cutoff_yob")]
  rd_fail(!nrow(keep), "No first-law cutoffs cover the requested window")
  d <- merge(cells, keep, by = "bpl")
  d <- merge(d, laws[, c("bpl", "yob", "ca_years_combined")],
             by = c("bpl", "yob"), all.x = TRUE)
  rd_fail(anyNA(d$ca_years_combined), "Missing cohort requirement in analysis cells")
  d$k <- d$yob - d$cutoff_yob
  d <- d[d$k >= -h & d$k <= h - 1L, ]
  d$post <- as.integer(d$k >= 0L)
  d$x <- d$k + 0.5
  d$kernel_weight <- d$n * (1 - abs(d$x) / h)
  rd_fail(any(d$kernel_weight <= 0), "Nonpositive kernel weight")
  support <- unique(d[c("bpl", "k")])
  rd_fail(any(table(support$bpl) != 2L * h), "A retained state lacks a cohort")
  states <- sort(unique(d$bpl))
  slope_names <- unlist(lapply(states, function(s) paste0(c("pre_", "post_"), s)))
  for (s in states) {
    d[[paste0("pre_", s)]] <- d$x * (d$post == 0L & d$bpl == s)
    d[[paste0("post_", s)]] <- d$x * (d$post == 1L & d$bpl == s)
  }
  attr(d, "slopes") <- slope_names
  d
}

rd_model_formula <- function(outcome, slopes, iv = FALSE, endogenous = "school_years") {
  rhs <- paste(slopes, collapse = " + ")
  if (iv) as.formula(paste0(outcome, " ~ ", rhs,
                           " | bpl | ", endogenous, " ~ post"))
  else as.formula(paste0(outcome, " ~ post + ", rhs, " | bpl"))
}

rd_optional_fit <- function(expr, allow_unidentified = FALSE) {
  if (allow_unidentified) tryCatch(force(expr), error = identity) else force(expr)
}

fit_local_models <- function(d, endogenous = "school_years", allow_unidentified = FALSE) {
  slopes <- attr(d, "slopes")
  if (is.null(slopes)) slopes <- grep("^(pre|post)_[0-9]+$", names(d), value = TRUE)
  fit <- function(formula) fixest::feols(
    formula, data = d, weights = ~kernel_weight, cluster = ~bpl,
    ssc = fixest::ssc(K.adj = FALSE), notes = FALSE, warn = FALSE)
  list(required = fit(rd_model_formula("ca_years_combined", slopes)),
       first = rd_optional_fit(fit(rd_model_formula(endogenous, slopes)), allow_unidentified),
       reduced = fit(rd_model_formula("scientist", slopes)),
       fuzzy = rd_optional_fit(fit(rd_model_formula("scientist", slopes, iv = TRUE,
                                                    endogenous = endogenous)), allow_unidentified))
}

model_coefficient <- function(model, term, model_name) {
  tab <- summary(model)$coeftable
  rd_fail(!term %in% rownames(tab), paste("Missing coefficient", term, "in", model_name))
  data.frame(model = model_name, term = term, estimate = tab[term, 1L],
             se = tab[term, 2L], p_value = tab[term, 4L],
             effect_per_100k = if (model_name %in% c("reduced_form", "fuzzy_rd"))
               1e5 * tab[term, 1L] else NA_real_, row.names = NULL)
}

ar_confidence_set_rd <- function(d, slopes, limit = 0.05, n_grid = 501L,
                                  endogenous = "school_years") {
  formula <- rd_model_formula("ar_outcome", slopes)
  p_at <- function(beta) {
    d$ar_outcome <- d$scientist - beta * d[[endogenous]]
    fit <- fixest::feols(formula, data = d, weights = ~kernel_weight,
                        cluster = ~bpl, ssc = fixest::ssc(K.adj = FALSE),
                        notes = FALSE, warn = FALSE)
    summary(fit)$coeftable["post", 4L]
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
    data.frame(lower = lo, upper = hi, lower_per_100k = lo * 1e5,
               upper_per_100k = hi * 1e5)
  })) else data.frame(lower = numeric(), upper = numeric(),
                      lower_per_100k = numeric(), upper_per_100k = numeric())
  list(grid = data.frame(beta = beta, p_value = p, accepted = accepted),
       intervals = intervals)
}

calonico_check <- function(d, h = 6L, endogenous = "school_years") {
  state_dummies <- stats::model.matrix(~factor(bpl) - 1, data = d)[, -1L, drop = FALSE]
  warnings <- character()
  fit <- tryCatch(withCallingHandlers(
    rdrobust::rdrobust(y = d$scientist, x = d$k, c = -0.5,
                       fuzzy = d[[endogenous]], p = 1, q = 2,
                       h = h, b = h, kernel = "triangular", weights = d$n,
                       cluster = d$bpl, covs = state_dummies,
                       vce = "hc1", masspoints = "check"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) e)
  if (inherits(fit, "error")) return(data.frame(
    estimate = NA_real_, conventional_se = NA_real_, conventional_p = NA_real_,
    robust_ci_lower = NA_real_, robust_ci_upper = NA_real_,
    robust_p = NA_real_, unique_left = length(unique(d$k[d$k < 0L])),
    unique_right = length(unique(d$k[d$k >= 0L])),
    messages = paste(c(warnings, conditionMessage(fit)), collapse = " | ")))
  data.frame(estimate = unname(fit$coef[1L, 1L]),
             conventional_se = unname(fit$se["Conventional", 1L]),
             conventional_p = unname(fit$pv["Conventional", 1L]),
             robust_ci_lower = unname(fit$ci["Robust", 1L]),
             robust_ci_upper = unname(fit$ci["Robust", 2L]),
             robust_p = unname(fit$pv["Robust", 1L]),
             unique_left = length(unique(d$k[d$k < 0L])),
             unique_right = length(unique(d$k[d$k >= 0L])),
             messages = paste(unique(warnings), collapse = " | "))
}

# Reproduce the CLS Stata cohort loop before using it for amendment counterfactuals.
policy_columns <- c("entryage", "exitage", "earlyyrs", "earlyyrs_condition",
                    "workage", "workyrs", "contage", "contyrs")

decode_sentinel <- function(x) {
  map <- c(`99` = 4L, `996` = 6L, `997` = 7L, `998` = 8L, `9910` = 10L)
  hit <- as.character(x) %in% names(map)
  x[hit] <- unname(map[as.character(x[hit])])
  x
}

calculate_cohort_requirements <- function(policy, yobs = 1885:1912) {
  policy <- policy[order(policy$year), ]
  rd_fail(anyDuplicated(policy$year) > 0L, "Duplicate policy years")
  for (name in c("earlyyrs", "workyrs", "contyrs"))
    policy[[name]] <- decode_sentinel(policy[[name]])
  years <- integer(length(yobs))
  for (i in seq_along(yobs)) {
    ca1 <- ca2 <- ca3 <- ca4 <- 0L
    for (age in 5:17) {
      yr <- yobs[[i]] + age
      p <- policy[match(yr, policy$year), ]
      rd_fail(nrow(p) != 1L || anyNA(p[policy_columns]),
              paste("Missing law for calendar year", yr))
      exempt_work <- ca2 >= p$workyrs && age >= p$workage && p$workage > 0L
      exempt_early <- ca3 >= p$earlyyrs && age >= p$earlyyrs_condition &&
        p$earlyyrs > 0L
      exempt_cont <- (ca2 + ca4) >= p$contyrs && p$contyrs > 0L
      regular <- age >= p$entryage && age < p$exitage
      ca1 <- ca1 + as.integer(regular)
      ca2 <- ca2 + as.integer(regular && !exempt_work)
      ca3 <- ca3 + as.integer(regular && !exempt_early)
      ca4 <- ca4 + as.integer(age >= p$workage && age < p$contage &&
                                !exempt_cont && p$workage > 0L &&
                                !(regular && !exempt_work))
    }
    years[[i]] <- min(ca3, ca2 + ca4)
  }
  data.frame(yob = yobs, ca_years_combined = years)
}

audit_amendments <- function(policy, laws, onset, cells, min_side = 3L) {
  rd_fail(anyDuplicated(policy[c("statefip", "year")]) > 0L,
          "Duplicate state-year policy")
  validation <- do.call(rbind, lapply(split(policy, policy$statefip), function(p) {
    calc <- calculate_cohort_requirements(p)
    calc$bpl <- p$statefip[[1L]]
    calc
  }))
  reference <- merge(validation, laws[, c("bpl", "yob", "ca_years_combined")],
                     by = c("bpl", "yob"), suffixes = c("_recomputed", "_cls"))
  rd_fail(nrow(reference) != nrow(validation) ||
            any(reference$ca_years_combined_recomputed != reference$ca_years_combined_cls),
          "Recomputed CLS cohort requirements differ from the supplied data")

  output <- list()
  for (p in split(policy, policy$statefip)) {
    bpl <- p$statefip[[1L]]
    p <- p[order(p$year), ]
    onset_year <- onset$cls_first_year_in_force[match(bpl, onset$statefip)]
    if (is.na(onset_year)) next
    changed <- which(vapply(seq_len(nrow(p)), function(i) {
      i > 1L && any(as.numeric(p[i, policy_columns]) !=
                      as.numeric(p[i - 1L, policy_columns]))
    }, logical(1L)))
    changed <- changed[p$year[changed] > max(onset_year, 1900L)]
    if (!length(changed)) next
    actual <- calculate_cohort_requirements(p)
    for (j in seq_along(changed)) {
      i <- changed[[j]]
      next_i <- if (j < length(changed)) changed[[j + 1L]] - 1L else nrow(p)
      cf <- p
      cf[i:next_i, policy_columns] <- p[rep(i - 1L, next_i - i + 1L),
                                         policy_columns]
      delta <- actual$ca_years_combined -
        calculate_cohort_requirements(cf)$ca_years_combined
      candidates <- which(delta > 0L & c(TRUE, head(delta, -1L) <= 0L))
      cut <- if (length(candidates)) actual$yob[candidates[[1L]]] else NA_integer_
      pre <- if (is.na(cut)) integer() else delta[actual$yob %in%
                                                   (cut - min_side):(cut - 1L)]
      post <- if (is.na(cut)) integer() else delta[actual$yob %in%
                                                    cut:(cut + min_side - 1L)]
      output[[length(output) + 1L]] <- data.frame(
        bpl = bpl, statename = p$statename[[1L]], reform_year = p$year[[i]],
        changed_fields = paste(policy_columns[as.numeric(p[i, policy_columns]) !=
                                                 as.numeric(p[i - 1L, policy_columns])],
                               collapse = ";"),
        cutoff_yob = cut, positive_cohorts = sum(delta > 0L),
        negative_cohorts = sum(delta < 0L),
        pre_cohorts = if (is.na(cut)) NA_integer_ else cut - 1885L,
        post_cohorts = if (is.na(cut)) NA_integer_ else 1912L - cut + 1L,
        clean_step = length(pre) == min_side && length(post) == min_side &&
          all(pre == 0L) && all(post > 0L),
        delta_at_cutoff = if (is.na(cut)) NA_integer_ else
          delta[match(cut, actual$yob)])
    }
  }
  if (!length(output)) return(list(inventory = data.frame(), estimates = data.frame()))
  inventory <- do.call(rbind, output)
  inventory$overlapping_cutoff <- vapply(seq_len(nrow(inventory)), function(i) {
    if (is.na(inventory$cutoff_yob[[i]])) return(FALSE)
    any(inventory$bpl == inventory$bpl[[i]] &
          abs(inventory$cutoff_yob - inventory$cutoff_yob[[i]]) < 2L * min_side &
          seq_len(nrow(inventory)) != i, na.rm = TRUE)
  }, logical(1L))
  first_events <- first_law_cutoffs(laws, onset)
  first_cut <- first_events$cutoff_yob[match(inventory$bpl, first_events$bpl)]
  inventory$overlapping_first_law <- !is.na(first_cut) &
    !is.na(inventory$cutoff_yob) &
    abs(inventory$cutoff_yob - first_cut) < 2L * min_side
  inventory$eligible <- inventory$clean_step & !inventory$overlapping_cutoff &
    !inventory$overlapping_first_law &
    inventory$pre_cohorts >= min_side & inventory$post_cohorts >= min_side
  inventory$reason <- ifelse(inventory$eligible, "eligible_exploratory",
                             ifelse(is.na(inventory$cutoff_yob), "no_positive_cohort_step",
                                    ifelse(!inventory$clean_step, "no_clean_step",
                                           ifelse(inventory$overlapping_cutoff,
                                                  "overlapping_reform",
                                                  ifelse(inventory$overlapping_first_law,
                                                         "overlapping_first_law",
                                                         "sample_edge")))))
  estimates <- lapply(which(inventory$eligible), function(i) {
    event <- inventory[i, ]
    d <- cells[cells$bpl == event$bpl &
                 cells$yob >= event$cutoff_yob - min_side &
                 cells$yob < event$cutoff_yob + min_side, ]
    d$k <- d$yob - event$cutoff_yob
    d$x <- d$k + 0.5
    d$post <- as.integer(d$k >= 0L)
    d$w <- d$n * (1 - abs(d$x) / min_side)
    first <- stats::lm(school_years ~ post + x + post:x, data = d, weights = w)
    reduced <- stats::lm(scientist ~ post + x + post:x, data = d, weights = w)
    fs <- unname(stats::coef(first)[["post"]])
    rf <- unname(stats::coef(reduced)[["post"]])
    data.frame(bpl = event$bpl, reform_year = event$reform_year,
               cutoff_yob = event$cutoff_yob, men = sum(d$n),
               scientists = sum(d$n * d$scientist),
               first_stage = fs, reduced_form_per_100k = 1e5 * rf,
               note = "Descriptive within-state contrast; no causal ratio or interval")
  })
  list(inventory = inventory,
       estimates = if (length(estimates)) do.call(rbind, estimates) else data.frame())
}

build_plot_data <- function(d, models, n_boot = 999L) {
  outcomes <- list(required = "ca_years_combined", first = "school_years",
                   reduced = "scientist")
  events <- unique(d[c("bpl", "k", "x", "post")])
  states <- sort(unique(d$bpl))
  state_weights <- tapply(d$n, d$bpl, sum)
  set.seed(20260926)
  draws <- replicate(n_boot, sample(states, length(states), replace = TRUE),
                     simplify = FALSE)
  points <- list()
  lines <- list()
  for (name in names(outcomes)) {
    y <- outcomes[[name]]
    tmp <- d[c("bpl", "k", "n", y)]
    tmp$num <- tmp$n * tmp[[y]]
    sc <- aggregate(cbind(n, num) ~ bpl + k, data = tmp, FUN = sum)
    sc$value <- sc$num / sc$n
    state_mean <- tapply(sc$num, sc$bpl, sum) / tapply(sc$n, sc$bpl, sum)
    grand <- sum(sc$num) / sum(sc$n)
    sc$adjusted <- sc$value - state_mean[as.character(sc$bpl)] + grand
    for (variant in c("raw", "adjusted")) {
      for (k in sort(unique(sc$k))) {
        sub <- sc[sc$k == k, ]
        val <- if (variant == "raw") sub$value else sub$adjusted
        w <- if (variant == "raw") sub$n else state_weights[as.character(sub$bpl)]
        mean_value <- weighted.mean(val, w)
        boot <- vapply(draws, function(ids) {
          idx <- match(ids, sub$bpl)
          weighted.mean(val[idx], w[idx])
        }, numeric(1L))
        points[[length(points) + 1L]] <- data.frame(
          measure = name, variant = variant, k = k, x = k,
          value = mean_value, lower = unname(stats::quantile(boot, 0.025)),
          upper = unname(stats::quantile(boot, 0.975)),
          men = sum(sub$n))
      }
    }
    # Use the primary model for standardized fitted lines at fixed state weights.
    for (s in states) {
      for (k in sort(unique(events$k))) {
        row <- d[d$bpl == s & d$k == k, ][1L, , drop = FALSE]
        pred <- as.numeric(predict(models[[name]], newdata = row))
        lines[[length(lines) + 1L]] <- data.frame(
          measure = name, variant = "adjusted", bpl = s, k = k,
          predicted = pred - state_mean[as.character(s)] + grand,
          state_weight = state_weights[as.character(s)])
      }
    }
  }
  fitted <- do.call(rbind, lines)
  fitted <- do.call(rbind, lapply(split(fitted, list(fitted$measure, fitted$k),
                                         drop = TRUE), function(x) {
    data.frame(measure = x$measure[[1L]], variant = "adjusted", k = x$k[[1L]],
               value = weighted.mean(x$predicted, x$state_weight))
  }))
  list(points = do.call(rbind, points), fitted = fitted)
}

save_discontinuity_plots <- function(plot_data, estimates, out_dir) {
  labels <- c(required = "Required schooling years",
              first = "1940 schooling years",
              reduced = "1950 scientists per 100,000")
  for (variant in c("raw", "adjusted")) {
    panels <- lapply(names(labels), function(measure) {
      d <- plot_data$points[plot_data$points$variant == variant &
                              plot_data$points$measure == measure, ]
      line <- plot_data$fitted[plot_data$fitted$measure == measure, ]
      scale <- if (measure == "reduced") 1e5 else 1
      d[, c("value", "lower", "upper")] <- d[, c("value", "lower", "upper")] * scale
      line$value <- line$value * scale
      # The coefficient label uses the primary clustered model, including on raw plots.
      model_row <- estimates[estimates$model == c(required = "required_years",
                                                  first = "first_stage",
                                                  reduced = "reduced_form")[[measure]], ]
      jump <- model_row$estimate[[1L]] * scale
      jump_se <- model_row$se[[1L]] * scale
      p <- ggplot(d, aes(k, value)) +
        geom_vline(xintercept = -0.5, linetype = "dashed", colour = "firebrick") +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15,
                      colour = "grey55", linewidth = 0.35) +
        geom_point(size = 2, colour = "#174a75") +
        scale_x_continuous(breaks = seq(min(d$k), max(d$k), by = 1)) +
        labs(title = labels[[measure]],
             subtitle = sprintf("Model jump at midpoint: %+.2f (state-clustered SE %.2f)",
                                jump, jump_se),
             x = "Birth cohort relative to first exposure",
             y = NULL) + theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
      if (variant == "adjusted")
        p <- p + geom_line(data = line, aes(k, value, group = k >= 0),
                           inherit.aes = FALSE, colour = "#d97720", linewidth = 0.8)
      else {
        d$post <- as.integer(d$k >= 0L)
        raw_fit <- stats::lm(value ~ post + k + post:k, data = d, weights = men)
        line$post <- as.integer(line$k >= 0L)
        line$value <- as.numeric(predict(raw_fit, newdata = line))
        p <- p + geom_line(data = line, aes(k, value, group = k >= 0),
                           inherit.aes = FALSE, colour = "#d97720", linewidth = 0.8)
      }
      p
    })
    combined <- patchwork::wrap_plots(panels, ncol = 1) +
      patchwork::plot_annotation(title = paste("First-law cohort discontinuities:", variant),
                                 subtitle = "Same 14 states at each cohort; points show state-bootstrap 95% intervals")
    ggplot2::ggsave(file.path(out_dir, paste0("discontinuity_", variant, ".png")),
                    combined, width = 10, height = 12, dpi = 180)
    ggplot2::ggsave(file.path(out_dir, paste0("discontinuity_", variant, ".pdf")),
                    combined, width = 10, height = 12)
  }
}

main <- function() {
  root <- rd_data_root()
  iv_dir <- file.path(root, "results/mandatory_schooling/linked_1940_schooling_1950_scientist_iv")
  cls_dir <- file.path(root, "output/mandatory_schooling/cls")
  out_dir <- file.path(root, "results/mandatory_schooling/linked_1940_schooling_1950_scientist_rd")
  paths <- c(cells = file.path(iv_dir, "main_cells.csv"),
             iv_flow = file.path(iv_dir, "sample_flow.csv"),
             iv_diagnostics = file.path(iv_dir, "diagnostics.csv"),
             laws = file.path(cls_dir, "cls_cohort_requirements_1875_1912.csv"),
             onset = file.path(cls_dir, "cls_law_onset_by_state.csv"),
             policy = file.path(cls_dir, "cls_state_age_limits_1880_1930.csv"))
  rd_fail(any(!file.exists(paths)), paste("Missing inputs:", paste(paths[!file.exists(paths)],
                                                                  collapse = ", ")))
  cells <- read.csv(paths[["cells"]], stringsAsFactors = FALSE)
  iv_flow <- read.csv(paths[["iv_flow"]], stringsAsFactors = FALSE)
  iv_diagnostics <- read.csv(paths[["iv_diagnostics"]], stringsAsFactors = FALSE)
  laws <- read.csv(paths[["laws"]], stringsAsFactors = FALSE)
  onset <- read.csv(paths[["onset"]], stringsAsFactors = FALSE)
  policy <- read.csv(paths[["policy"]], stringsAsFactors = FALSE)
  iv_men <- iv_flow$n[iv_flow$step == "Pairs with observed 1950 occupation"]
  iv_scientists <- iv_diagnostics$scientist_count[
    iv_diagnostics$sample == "linked_1940_grade" &
      iv_diagnostics$cluster == "state"]
  rd_fail(length(iv_men) != 1L || length(iv_scientists) != 1L ||
            sum(cells$n) != iv_men ||
            sum(cells$n * cells$scientist) != iv_scientists,
          "IV main cells do not match the IV sample audit")
  events <- first_law_cutoffs(laws, onset)
  d <- make_window(cells, laws, events, h = 6L)
  rd_fail(length(unique(d$bpl)) != 14L,
          "Expected 14 first-law states with six cohorts on each side")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  rd_write(data.frame(iv_main_men = iv_men, iv_main_scientists = iv_scientists,
                      rd_states = length(unique(d$bpl)), rd_cohorts_per_side = 6L,
                      rd_men = sum(d$n), rd_scientists = sum(d$n * d$scientist)),
           file.path(out_dir, "sample_summary.csv"))
  rd_write(events, file.path(out_dir, "first_law_cutoffs.csv"))
  message("Estimating local first-law fuzzy RD...")
  models <- fit_local_models(d)
  estimates <- rbind(model_coefficient(models$required, "post", "required_years"),
                     model_coefficient(models$first, "post", "first_stage"),
                     model_coefficient(models$reduced, "post", "reduced_form"),
                     model_coefficient(models$fuzzy, "fit_school_years", "fuzzy_rd"))
  rd_write(estimates, file.path(out_dir, "primary_estimates.csv"))
  message("Inverting Anderson-Rubin tests...")
  ar <- ar_confidence_set_rd(d, attr(d, "slopes"))
  rd_write(ar$grid, file.path(out_dir, "anderson_rubin_grid.csv"))
  rd_write(ar$intervals, file.path(out_dir, "anderson_rubin_intervals.csv"))
  message("Running fixed-bandwidth Calonico sensitivity...")
  calonico <- calonico_check(d)
  rd_write(calonico, file.path(out_dir, "calonico_fuzzy_sensitivity.csv"))

  message("Auditing later statutory reforms...")
  amendments <- audit_amendments(policy, laws, onset, cells)
  rd_write(amendments$inventory, file.path(out_dir, "later_reform_inventory.csv"))
  rd_write(amendments$estimates, file.path(out_dir, "later_reform_exploratory.csv"))

  message("Estimating windows and state influence...")
  sensitivity <- list()
  for (h in 2:6) {
    for (sample_name in c("fixed_14_states", "all_supported_states")) {
      ev <- events
      if (sample_name == "fixed_14_states")
        ev$is_first_law <- ev$is_first_law & ev$bpl %in% unique(d$bpl)
      window <- make_window(cells, laws, ev, h = h)
      fits <- fit_local_models(window)
      row <- model_coefficient(fits$fuzzy, "fit_school_years", "fuzzy_rd")
      row$window <- h
      row$sample <- sample_name
      row$states <- length(unique(window$bpl))
      row$men <- sum(window$n)
      row$scientists <- sum(window$n * window$scientist)
      row$first_stage <- unname(coef(fits$first)[["post"]])
      row$reduced_form <- unname(coef(fits$reduced)[["post"]])
      sensitivity[[length(sensitivity) + 1L]] <- row
    }
  }
  # Keep the same observations and kernel weights, but evaluate the two fitted
  # cohort lines at the first exposed year instead of the between-year midpoint.
  cutoff_zero <- d
  cutoff_zero$x <- cutoff_zero$k
  for (s in sort(unique(cutoff_zero$bpl))) {
    cutoff_zero[[paste0("pre_", s)]] <- cutoff_zero$x *
      (cutoff_zero$post == 0L & cutoff_zero$bpl == s)
    cutoff_zero[[paste0("post_", s)]] <- cutoff_zero$x *
      (cutoff_zero$post == 1L & cutoff_zero$bpl == s)
  }
  zero_fit <- fit_local_models(cutoff_zero)
  zero_row <- model_coefficient(zero_fit$fuzzy, "fit_school_years", "fuzzy_rd")
  zero_row$window <- 6L
  zero_row$sample <- "cutoff_at_first_exposed_year"
  zero_row$states <- length(unique(cutoff_zero$bpl))
  zero_row$men <- sum(cutoff_zero$n)
  zero_row$scientists <- sum(cutoff_zero$n * cutoff_zero$scientist)
  zero_row$first_stage <- unname(coef(zero_fit$first)[["post"]])
  zero_row$reduced_form <- unname(coef(zero_fit$reduced)[["post"]])
  sensitivity[[length(sensitivity) + 1L]] <- zero_row
  for (s in sort(unique(d$bpl))) {
    drop <- d[d$bpl != s, ]
    fit <- fit_local_models(drop)
    row <- model_coefficient(fit$fuzzy, "fit_school_years", "fuzzy_rd")
    row$window <- 6L
    row$sample <- paste0("drop_state_", s)
    row$states <- length(unique(drop$bpl))
    row$men <- sum(drop$n)
    row$scientists <- sum(drop$n * drop$scientist)
    row$first_stage <- unname(coef(fit$first)[["post"]])
    row$reduced_form <- unname(coef(fit$reduced)[["post"]])
    sensitivity[[length(sensitivity) + 1L]] <- row
  }
  rd_write(do.call(rbind, sensitivity), file.path(out_dir, "window_and_state_sensitivity.csv"))
  per_state <- do.call(rbind, lapply(sort(unique(d$bpl)), function(s) {
    x <- d[d$bpl == s, ]
    first <- stats::lm(school_years ~ post + x + post:x, data = x,
                       weights = kernel_weight)
    reduced <- stats::lm(scientist ~ post + x + post:x, data = x,
                         weights = kernel_weight)
    data.frame(bpl = s, statename = x$statename[[1L]],
               men = sum(x$n), scientists = sum(x$n * x$scientist),
               first_stage = unname(coef(first)[["post"]]),
               reduced_form_per_100k = 1e5 * unname(coef(reduced)[["post"]]))
  }))
  rd_write(per_state, file.path(out_dir, "state_cutoff_diagnostics.csv"))

  # A false cutoff splitting the six genuinely unexposed cohorts into two groups.
  placebo <- d[d$k < 0L, ]
  placebo$k <- placebo$k + 3L
  placebo$x <- placebo$k + 0.5
  placebo$post <- as.integer(placebo$k >= 0L)
  placebo$kernel_weight <- placebo$n * (1 - abs(placebo$x) / 3)
  for (s in sort(unique(placebo$bpl))) {
    placebo[[paste0("pre_", s)]] <- placebo$x *
      (placebo$post == 0L & placebo$bpl == s)
    placebo[[paste0("post_", s)]] <- placebo$x *
      (placebo$post == 1L & placebo$bpl == s)
  }
  placebo_fit <- function(y) fixest::feols(
    rd_model_formula(y, attr(d, "slopes")), data = placebo,
    weights = ~kernel_weight, cluster = ~bpl,
    ssc = fixest::ssc(K.adj = FALSE), notes = FALSE, warn = FALSE)
  placebo_table <- rbind(model_coefficient(placebo_fit("school_years"), "post",
                                             "placebo_schooling"),
                           model_coefficient(placebo_fit("scientist"), "post",
                                             "placebo_scientist"))
  placebo_table$scientist_per_100k <- ifelse(placebo_table$model ==
                                               "placebo_scientist",
                                             1e5 * placebo_table$estimate, NA_real_)
  rd_write(placebo_table, file.path(out_dir, "prelaw_placebo.csv"))

  message("Rendering discontinuity plots...")
  plot_data <- build_plot_data(d, models)
  rd_write(plot_data$points, file.path(out_dir, "discontinuity_points.csv"))
  rd_write(plot_data$fitted, file.path(out_dir, "discontinuity_adjusted_fit.csv"))
  adjacent <- do.call(rbind, lapply(c("raw", "adjusted"), function(variant) {
    do.call(rbind, lapply(c("required", "first", "reduced"), function(measure) {
      p <- plot_data$points[plot_data$points$variant == variant &
                              plot_data$points$measure == measure, ]
      scale <- if (measure == "reduced") 1e5 else 1
      data.frame(variant = variant, measure = measure,
                 last_unexposed = p$value[p$k == -1L] * scale,
                 first_exposed = p$value[p$k == 0L] * scale,
                 adjacent_difference =
                   (p$value[p$k == 0L] - p$value[p$k == -1L]) * scale)
    }))
  }))
  rd_write(adjacent, file.path(out_dir, "adjacent_cohort_contrast.csv"))
  save_discontinuity_plots(plot_data, estimates, out_dir)
  source(file.path(rd_repo_root(), "analysis/mandatory_schooling/cohort_rd_state_plot_helpers.R"),
         local = TRUE)
  state_grid <- d
  state_grid$event_id <- paste0("first_", state_grid$bpl)
  state_grid$event_year <- events$cls_first_year_in_force[
    match(state_grid$bpl, events$bpl)]
  state_grid$event_kind <- "first law"
  rd_fail(anyNA(state_grid$event_year), "Missing first-law year for state grid")
  ms_save_state_grid(state_grid, required_col = "ca_years_combined", h = 6L,
                     output = file.path(out_dir, "first_law_by_state.png"),
                     title = "First-law cohort discontinuities by state",
                     required_note = "Required years are total CLS years; 14 states, six cohorts per side.",
                     ncol = 5L, width = 25, height = 19.8, expected_events = 14L)
  saveRDS(models, file.path(out_dir, "models.rds"))
  notes <- c(
    "Local fuzzy RD on the exact main IV linked population (not its quality or 1950-grade sensitivities).",
    "First law is defined by first year in force after 1900; cutoff is first positive CLS cohort requirement.",
    "Primary window: six annual birth cohorts on each side in the 14 states with complete support.",
    "The school-years coefficient is a local fuzzy-IV ratio; it uses the same laws as the broad IV.",
    "Primary model evaluates its jump halfway between the last unexposed and first exposed",
    "annual cohorts; cutoff_at_first_exposed_year holds weights fixed and changes only evaluation.",
    "Treatment changes from 0 to 1 required year at the cutoff and then generally ramps up.",
    "Models have state fixed effects, state-specific linear slopes on each side, a common jump,",
    "triangular weights times IV-cell population, and state-clustered standard errors.",
    "Anderson-Rubin set inverts the state-clustered post coefficient in Y-beta*schooling.",
    "Blank AR endpoint means the accepted set reaches the evaluated grid boundary.",
    "Calonico fuzzy RD is a diagnostic with fixed bandwidth 6, not the primary inference:",
    "the discrete running variable has only six mass points on each side.",
    "Raw plot lines are descriptive; adjusted lines come from the primary pooled model.",
    "Plot intervals bootstrap whole states (999 draws) and are descriptive pointwise intervals.",
    "Later reform estimates use three cohorts on each side and are exploratory point estimates only.",
    "Pre-law placebo splits the six unexposed cohorts into three and three.",
    "The linked extract and its 1950 occupation are selected; they do not represent the full census."
  )
  writeLines(notes, file.path(out_dir, "notes.txt"))
  message("Wrote ", out_dir)
  print(estimates)
  print(calonico)
}

if (sys.nframe() == 0L) main()
