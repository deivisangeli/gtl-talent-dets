# Standalone inline-table LaTeX renderer, called by the BA IV runner.
ba_sample_labels <- c(linked_1940_grade = "Main: 1940 schooling",
                      consistent_link_sensitivity = "Consistent links",
                      same_record_1950_grade = "Same-record 1950 schooling")
ba_fmt <- function(x, digits = 2L) formatC(x, format = "f", digits = digits, big.mark = ",")
ba_p_fmt <- function(p) {
  if (any(!is.finite(p) | p < 0 | p > 1)) stop("Invalid coefficient p-value", call. = FALSE)
  ifelse(p < 0.0001, "$<0.0001$", ba_fmt(p, 4L))
}
ba_inference_fields <- function(state, state_cohort, scale, digits) {
  if (!isTRUE(all.equal(state$estimate, state_cohort$estimate, tolerance = 1e-10))) {
    stop("Coefficient differs between clustering methods", call. = FALSE)
  }
  c(ba_fmt(scale * state$estimate, digits),
    ba_fmt(scale * state$se, digits), ba_p_fmt(state$p_value),
    ba_fmt(scale * state_cohort$se, digits), ba_p_fmt(state_cohort$p_value))
}
ba_coefficient_header <- function(prefix) {
  n <- length(prefix)
  paste0(paste(c(prefix, "\\multicolumn{2}{c}{Birth state}",
    "\\multicolumn{2}{c}{Birth state $\\times$ cohort}"), collapse = " & "),
    " \\\\ \\cmidrule(lr){", n + 1L, "-", n + 2L, "}\\cmidrule(lr){",
    n + 3L, "-", n + 4L, "}\n",
    paste(c(rep("", n), "SE", "$p$", "SE", "$p$"), collapse = " & "))
}
ba_row <- function(d, sample, cluster = "state", model = NULL, term = NULL) {
  keep <- d$sample == sample & d$cluster == cluster
  if (!is.null(model)) keep <- keep & d$model == model
  if (!is.null(term)) keep <- keep & d$term == term
  result <- d[keep, , drop = FALSE]
  if (nrow(result) != 1L) stop("Expected exactly one BA note result row", call. = FALSE)
  result
}

ba_ar_text <- function(intervals, scale = 1e5, digits = 1L) {
  if (!nrow(intervals)) return("No values accepted on the search grid.")
  pieces <- vapply(seq_len(nrow(intervals)), function(i) {
    lo <- if (intervals$lower_boundary_reached[[i]]) "\\text{lower search boundary}" else
      ba_fmt(scale * intervals$lower[[i]], digits)
    hi <- if (intervals$upper_boundary_reached[[i]]) "\\text{upper search boundary}" else
      ba_fmt(scale * intervals$upper[[i]], digits)
    paste0("$[", lo, ",\\ ", hi, "]$")
  }, character(1))
  paste(pieces, collapse = " $\\cup$ ")
}

ba_table <- function(caption, columns, header, rows, notes) {
  c("\\begin{center}\\small\\setlength{\\tabcolsep}{4pt}",
    paste0("\\textbf{", caption, "}\\par\\medskip"),
    paste0("\\begin{tabular}{", columns, "}\\toprule"),
    paste0(header, " \\\\ \\midrule"), paste0(rows, " \\\\"),
    "\\bottomrule\\end{tabular}\\end{center}",
    paste0("{\\footnotesize\\noindent\\textit{Notes:} ", notes, "\\par}\\medskip"))
}

write_ba_note <- function(t, baseline, source_dir, run_dir, project_dir) {
  main <- names(ba_sample_labels)[[1L]]
  iv <- ba_row(t$estimates, main, model = "iv_2sls")
  diag <- ba_row(t$diagnostics, main)
  effect <- 1e5 * iv$estimate
  ci <- 1e5 * (iv$estimate + c(-1, 1) * qt(0.975, diag$states - 1L) * iv$se)
  ar <- ba_ar_text(t$anderson_rubin_intervals)
  descriptive_rows <- vapply(names(ba_sample_labels), function(sample) {
    d <- t$completion_shares[t$completion_shares$sample == sample, ]
    paste(ba_sample_labels[[sample]], ba_fmt(d$observations, 0),
          paste0(ba_fmt(100 * d$completion_share), "\\%"),
          ba_fmt(d$scientist_count, 0), ba_fmt(d$scientist_rate_per_100k, 1), sep = " & ")
  }, character(1))
  fs_rows <- rf_rows <- character()
  for (term in law_terms) {
    label <- switch(term, z_1_5 = "1--5", z_6 = "6", z_7 = "7", z_8 = "8", z_9plus = "9+")
    fs <- ba_row(t$first_stage, main, term = term)
    fs_sc <- ba_row(t$first_stage, main, "state_cohort", term = term)
    rf <- ba_row(t$reduced_form, main, term = term)
    rf_sc <- ba_row(t$reduced_form, main, "state_cohort", term = term)
    fs_rows <- c(fs_rows, paste(c(label, ba_inference_fields(fs, fs_sc, 100, 3)), collapse = " & "))
    rf_rows <- c(rf_rows, paste(c(label, ba_inference_fields(rf, rf_sc, 1e5, 2)), collapse = " & "))
  }
  effect_rows <- diagnostic_rows <- character()
  for (sample in names(ba_sample_labels)) {
    for (model in c("ols", "iv_2sls")) {
      st <- ba_row(t$estimates, sample, model = model)
      sc <- ba_row(t$estimates, sample, "state_cohort", model)
      effect_rows <- c(effect_rows, paste(c(ba_sample_labels[[sample]],
        if (model == "ols") "OLS" else "2SLS",
        ba_inference_fields(st, sc, 1e5, 1)), collapse = " & "))
    }
    st <- ba_row(t$diagnostics, sample)
    sc <- ba_row(t$diagnostics, sample, "state_cohort")
    diagnostic_rows <- c(diagnostic_rows, paste(ba_sample_labels[[sample]],
      ba_fmt(st$first_stage_joint_F), ba_fmt(sc$first_stage_joint_F),
      ba_fmt(st$anderson_rubin_zero_p, 4), sep = " & "))
  }
  comparison_rows <- character()
  for (spec in c("years", "ba")) {
    d <- if (spec == "years") baseline else t
    for (model in c("ols", "iv_2sls")) {
      st <- ba_row(d$estimates, main, model = model)
      sc <- ba_row(d$estimates, main, "state_cohort", model)
      comparison_rows <- c(comparison_rows, paste(c(
        if (spec == "years") "Schooling (+1 year)" else "BA completion ($0\\to1$)",
        if (model == "ols") "OLS" else "2SLS",
        ba_inference_fields(st, sc, 1e5, 1)), collapse = " & "))
    }
  }
  doc <- c("\\documentclass[10pt]{article}",
    "\\usepackage[margin=0.8in]{geometry}", "\\PassOptionsToPackage{spaces,obeyspaces}{url}",
    "\\usepackage{amsmath,booktabs,hyperref}",
    "\\hypersetup{colorlinks=true,urlcolor=blue}",
    "\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{5pt}",
    "\\begin{document}",
    "\\begin{center}{\\Large\\bfseries Compulsory Schooling, BA-Equivalent Completion,",
    "and Scientist Occupations}\\par\\medskip",
    paste0("Linked 1940--1950 census IV analysis\\par Generated ", format(Sys.Date(), "%Y-%m-%d"),
           "\\end{center}"),
    "\\section*{Design and measurement}",
    "This analysis changes the endogenous treatment in the existing Clay--Lingwall--Stephens (CLS) law IV from completed schooling years to BA-equivalent completion. The outcome remains scientist occupation observed in the 1950 census.",
    "Define $D_i=1\\{S_i\\geq16\\}$, where $S_i$ is completed schooling in 1940. Postgraduate schooling is included. Everyone with valid schooling remains eligible, including people below the college threshold. The historical census records highest grade completed, not an awarded degree; see \\href{https://usa.ipums.org/usa-action/variables/191762}{IPUMS education documentation}.",
    "The main sample contains native-born white men aged 27--54 in 1940, with birth cohorts 1885--1912 assigned as $1940-\\mathrm{AGE}-1$, valid schooling, and a unique reciprocal HIK link to 1950. Scientist status uses the linked 1950 record's OCC1950 codes 007, 061, 062, 063, 067, 068, and 069. Other recorded occupation and nonoccupation codes are zero. Missing occupation flags are excluded.",
    "\\begin{align*}",
    "D_{isc}&=\\alpha_s^D+\\lambda_{r(s)c}^D+\\sum_b\\pi_b Z^b_{sc}+u_{isc},\\\\",
    "Y_{isc}&=\\alpha_s^Y+\\lambda_{r(s)c}^Y+\\beta\\widehat D_{isc}+\\varepsilon_{isc}.",
    "\\end{align*}",
    "The five instruments indicate 1--5, 6, 7, 8, and 9+ CLS combined required years, relative to zero. Every model includes birth-state and CLS-region $\\times$ birth-year fixed effects. OLS replaces $\\widehat D$ with observed $D$. Identical regressor/outcome cells are weighted by their person counts. Setting \\texttt{K.adj=FALSE} preserves clustered inference under this collapse. Both state and state $\\times$ cohort clustering are reported; the discussion emphasizes state clustering.",
    ba_table("Sample and completion shares", "lrrrr",
      "Sample & Men & BA-equivalent & Scientists & Rate / 100,000", descriptive_rows,
      "The consistent-link sample additionally requires the same birth state, male and white codes in 1950, and an age increase of 9--11 years. The same-record sensitivity uses 1950 schooling among those asked the sample-line question; it is unweighted because the extract omits SLWT."),
    "\\section*{First stage and reduced form}",
    ba_table("CLS first stage: BA-equivalent completion", "lrrrrr",
      ba_coefficient_header(c("Required years", "Effect (pp)")), fs_rows,
      "Coefficients and standard errors are in percentage points (pp). Zero required years is the reference. Each clustering group reports its own SE and two-sided coefficient-test p-value. P-values use four decimal places; values below 0.0001 are shown as $<0.0001$. Probability-unit coefficients are retained in the CSV results."),
    ba_table("CLS reduced form: scientist occupations", "lrrrrr",
      ba_coefficient_header(c("Required years", "Effect / 100,000")), rf_rows,
      "Each clustering group reports its own SE and two-sided coefficient-test p-value. The reduced form uses the same outcome, sample, instruments and fixed effects as the original schooling-years analysis and matches its saved coefficients and inference."),
    ba_table("Instrument strength and AR test of zero", "lrrr",
      "Sample & $F$: state & $F$: state $\\times$ cohort & AR $p(0)$: state", diagnostic_rows,
      "First-stage statistics are cluster-robust joint Wald $F$ tests of the five instruments. The AR test at zero is the corresponding joint reduced-form test and is unchanged by the endogenous-treatment substitution."),
    paste0("The main state-clustered first-stage joint $F$ is ",
      ba_fmt(diag$first_stage_joint_F),
      "; instrument strength is limited. The law-bin first stages also differ in direction. Neither joint significance nor the threshold substitution establishes a monotone BA-completion response to the laws."),
    "\\section*{BA-completion effects}",
    paste0("The main state-clustered 2SLS coefficient is ", ba_fmt(iv$estimate, 5),
      " probability units, or ", ba_fmt(effect, 1),
      " additional scientist occupations per 100,000 men for a change from noncompletion to completion. The conventional 95\\% interval is [", ba_fmt(ci[[1L]], 1),
      ", ", ba_fmt(ci[[2L]], 1), "]. This interval uses the state-clustered standard error and a $t_{",
      diag$states - 1L, "}$ critical value."),
    ba_table("BA-equivalent completion and scientist occupation", "llrrrrr",
      ba_coefficient_header(c("Sample", "Model", "Effect")), effect_rows,
      "Effects and SEs are scientist occupations per 100,000 men for a change from noncompletion to completion. Each clustering group reports its own SE and two-sided coefficient-test p-value. Multiply a probability-unit coefficient by 100,000 for this scale; p-values are not scaled. Both stages are linear models; conventional inference should be read alongside AR inference."),
    paste0("The main 95\\% state-clustered Anderson--Rubin confidence set detected on the grid, in the same per-100,000 effect units, is ", ar, "."),
    "AR inversion regresses $Y_i-\\beta D_i$ on the instruments and fixed effects and accepts values whose joint instrument-test $p$-value is at least 0.05. The search uses 2,001 equally spaced probability-unit values in $[-1,1]$, with crossings refined to tolerance $10^{-8}$. All detected intervals are retained. An endpoint labeled as a search boundary means truncation at $-100,000$ or $100,000$ in the displayed units, not a proven infinite endpoint. A grid can miss accepted regions narrower than its 0.001 probability-unit spacing.",
    "\\section*{Comparison with the original schooling specification}",
    ba_table("Main sample: distinct endogenous treatments", "llrrrrr",
      ba_coefficient_header(c("Treatment (change)", "Model", "Effect")), comparison_rows,
      "Effects and SEs are scientist occupations per 100,000 men. Each clustering group reports its own SE and two-sided coefficient-test p-value. Schooling measures one additional completed year; BA measures crossing the completion threshold. These are different estimands and their coefficient magnitudes are not directly comparable. The sample and reduced form are identical."),
    "\\section*{Interpretation and reproducibility}",
    "A causal IV interpretation requires relevance, exclusion, appropriate monotonicity, and comparable law timing after the fixed effects. Interpreting this specification specifically as a BA-completion effect additionally requires CLS-induced changes at other schooling levels to have no separate effect on scientist occupation. The threshold substitution alone does not establish that condition. With five instruments, a simple single-instrument complier interpretation is not automatic.",
    "\nThe linked sample is selected and does not represent all men in 1950. The outcome measures an occupation observed in 1950, rather than first entry into science or lifetime scientist production.",
    "\nNo census microdata were rescanned. All three saved cell samples were validated against the prior run. Re-estimated schooling-years results, scientist reduced forms, sample totals, scientist counts, and the AR test at zero passed comparison checks. CSVs retain probability-unit coefficients, explicit effect scales, completion shares, sample audits, input checksums, and the full AR grid; fitted models and the R session information are saved separately.",
    paste0("{\\footnotesize\\raggedright\\textbf{Saved-cell source:} \\path{", gsub("\\\\", "/", source_dir), "}\\par}"),
    paste0("{\\footnotesize\\raggedright\\textbf{BA results:} \\path{", gsub("\\\\", "/", run_dir), "}\\par}"),
    "\\end{document}")
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(project_dir, "mandatory_schooling_ba_scientist_iv.tex")
  writeLines(doc, path, useBytes = TRUE)
  note_inputs <- c(path, file.path(find_ba_repo(), "analysis/mandatory_schooling",
    "export_linked_1940_ba_1950_scientist_iv_to_project.R"),
    file.path(run_dir, paste0(c("first_stage", "reduced_form", "estimates", "diagnostics",
                                "completion_shares", "anderson_rubin_intervals"), ".csv")),
    file.path(source_dir, "estimates.csv"))
  utils::write.csv(data.frame(path = normalizePath(note_inputs, winslash = "/"),
                              md5 = unname(tools::md5sum(note_inputs))),
                   file.path(run_dir, "note_provenance.csv"), row.names = FALSE)
  message("Wrote standalone note: ", path)
  invisible(path)
}

# Re-render the note from completed results without re-estimating or rescanning.
if (sys.nframe() == 0L) {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  root <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[[1L]]))) else
    Sys.getenv("GTL_REPO", unset = getwd())
  while (!file.exists(file.path(root, "paths.R"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("Could not locate paths.R", call. = FALSE)
    root <- parent
  }
  source(file.path(root, "analysis/mandatory_schooling",
                   "estimate_linked_1940_ba_1950_scientist_iv.R"), local = TRUE)
  data_dir <- data_root(root)
  source_dir <- Sys.getenv("MS_BA_IV_SOURCE_DIR", unset = file.path(data_dir,
    "results/mandatory_schooling/linked_1940_schooling_1950_scientist_iv"))
  run_dir <- Sys.getenv("MS_BA_IV_RESULTS_DIR", unset = file.path(data_dir,
    "results/mandatory_schooling/linked_1940_ba_1950_scientist_iv"))
  project_dir <- Sys.getenv("MS_BA_IV_PROJECT_DIR", unset = file.path(root, "project"))
  table_names <- c("first_stage", "reduced_form", "estimates", "diagnostics",
                   "completion_shares", "anderson_rubin_intervals")
  t <- setNames(lapply(file.path(run_dir, paste0(table_names, ".csv")), utils::read.csv,
                       stringsAsFactors = FALSE), table_names)
  checks <- utils::read.csv(file.path(run_dir, "baseline_validation.csv"))
  fail_if(nrow(checks) != 4L || !all(checks$passed), "BA baseline checks did not pass")
  inputs <- read_ba_inputs(source_dir)
  write_ba_note(t, inputs$tables, source_dir, run_dir, project_dir)
}
