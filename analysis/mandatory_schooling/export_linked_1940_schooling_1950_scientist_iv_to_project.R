# Export the linked-census schooling IV results to a standalone project note.
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/export_linked_1940_schooling_1950_scientist_iv_to_project.R
# Override inputs/outputs for fixtures with MS_SCIENTIST_IV_RESULTS_DIR and
# MS_SCIENTIST_IV_PROJECT_DIR. Compile from project/ with pdflatex.

fail_if <- function(condition, message) if (isTRUE(condition)) stop(message, call. = FALSE)

repo_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(dirname(dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]),
                                          winslash = "/"))))
  } else {
    Sys.getenv("GTL_REPO", unset = getwd())
  }
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    fail_if(identical(parent, candidate), "Could not find paths.R")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
}

data_root <- function(root = repo_root()) {
  env <- new.env(parent = baseenv())
  sys.source(file.path(root, "paths.R"), envir = env)
  candidates <- c(Sys.getenv("TALENT_DETS_DATA_DIR"),
                  get("TALENT_DETS_DATA_DIR", envir = env),
                  Sys.getenv("DET_DIR"),
                  file.path("C:/Users", Sys.info()[["user"]],
                            "Globtalent Dropbox/gtl_talent_dets"))
  hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
  fail_if(!length(hit), "Could not locate Dropbox project root")
  normalizePath(hit[[1L]], winslash = "/")
}

required_columns <- list(
  sample_flow = c("step", "n"),
  main_cells = c("bpl", "yob", "region", "law_bin", "school_years",
                 "scientist", "n"),
  linkage_audit = c("matched_rows", "matched_people", "observed_occupation",
                    "scientists", "same_birthplace", "male_in_1950",
                    "white_in_1950", "age_change_9_to_11",
                    "scientist_code_mismatches"),
  law_bin_counts = c("law_bin", "men", "scientists"),
  first_stage = c("sample", "cluster", "term", "estimate", "se", "p_value"),
  reduced_form = c("sample", "cluster", "term", "estimate", "se", "p_value"),
  estimates = c("sample", "model", "cluster", "term", "estimate", "se",
                "effect_per_100k_per_school_year", "se_per_100k"),
  diagnostics = c("sample", "cluster", "observations", "scientist_count",
                  "scientist_rate_per_100k", "first_stage_joint_F",
                  "anderson_rubin_zero_p"),
  anderson_rubin_intervals = c("sample", "cluster", "confidence", "lower",
                                "upper", "lower_per_100k", "upper_per_100k")
)
samples <- c("linked_1940_grade", "consistent_link_sensitivity",
             "same_record_1950_grade")
clusters <- c("state_cohort", "state")
law_terms <- c("z_1_5", "z_6", "z_7", "z_8", "z_9plus")

read_inputs <- function(run_dir) {
  files <- setNames(file.path(run_dir, paste0(names(required_columns), ".csv")),
                    names(required_columns))
  files <- c(files, notes = file.path(run_dir, "notes.txt"))
  missing <- files[!file.exists(files)]
  fail_if(length(missing) > 0L,
          paste("Missing IV export input(s):", paste(missing, collapse = ", ")))
  tables <- lapply(names(required_columns), function(name) {
    table <- utils::read.csv(files[[name]], check.names = FALSE,
                             stringsAsFactors = FALSE)
    absent <- setdiff(required_columns[[name]], names(table))
    fail_if(length(absent) > 0L,
            paste(name, "is missing columns:", paste(absent, collapse = ", ")))
    table
  })
  names(tables) <- names(required_columns)
  tables$notes <- readLines(files[["notes"]], warn = FALSE)
  list(tables = tables, files = files)
}

select_one <- function(df, ..., label = "row") {
  filters <- list(...)
  for (name in names(filters)) df <- df[df[[name]] == filters[[name]], , drop = FALSE]
  fail_if(nrow(df) != 1L, paste("Expected exactly one", label, "row; got", nrow(df)))
  df
}

validate_inputs <- function(t) {
  flow <- t$sample_flow
  audit <- t$linkage_audit
  bins <- t$law_bin_counts
  cells <- t$main_cells
  fail_if(nrow(audit) != 1L, "Expected one linkage audit row")
  fail_if(anyDuplicated(flow$step) > 0L || anyNA(flow$n) || any(flow$n < 0),
          "Invalid sample flow")
  needed_steps <- c("CLS-eligible 1940 men", "Valid 1940 schooling",
                    "Valid grade and flagged 1950 link", "Unique 1940-1950 pairs",
                    "Pairs with observed 1950 occupation", "1950 scientist occupations")
  fail_if(!identical(flow$step, needed_steps), "Sample flow steps changed")
  fail_if(any(diff(flow$n) > 0), "Sample flow increases after a restriction")
  fail_if(audit$matched_rows != audit$matched_people ||
            audit$matched_people != flow$n[[4L]] ||
            audit$observed_occupation != flow$n[[5L]] ||
            audit$scientists != flow$n[[6L]] ||
            audit$scientist_code_mismatches != 0,
          "Linkage audit disagrees with sample flow or occupation codes")
  agreement <- unlist(audit[c("same_birthplace", "male_in_1950",
                              "white_in_1950", "age_change_9_to_11")],
                       use.names = FALSE)
  fail_if(anyNA(agreement) || any(agreement > audit$matched_people),
          "Invalid linkage agreement counts")
  fail_if(!setequal(bins$law_bin, c("0", "1_5", "6", "7", "8", "9plus")) ||
            anyDuplicated(bins$law_bin) > 0L ||
            sum(bins$men) != flow$n[[5L]] ||
            sum(bins$scientists) != flow$n[[6L]],
          "Law bins do not reconcile with the sample")
  fail_if(anyNA(cells[, required_columns$main_cells]) ||
            any(cells$n <= 0 | cells$n != round(cells$n)) ||
            any(!cells$school_years %in% 0:17) ||
            any(!cells$scientist %in% 0:1) ||
            any(!cells$law_bin %in% bins$law_bin),
          "Invalid main analysis cells")
  cell_counts <- aggregate(cbind(men = cells$n,
                                 scientists = cells$n * cells$scientist),
                           by = list(law_bin = cells$law_bin), FUN = sum)
  cell_counts <- cell_counts[match(bins$law_bin, cell_counts$law_bin), ]
  fail_if(anyNA(cell_counts$law_bin) ||
            any(cell_counts$men != bins$men) ||
            any(cell_counts$scientists != bins$scientists),
          "Main cells do not reconcile with law-bin counts")
  for (name in c("first_stage", "reduced_form")) {
    df <- t[[name]]
    fail_if(anyDuplicated(df[c("sample", "cluster", "term")]) > 0L ||
              anyNA(df[, c("estimate", "se", "p_value")]),
            paste("Invalid", name, "coefficient rows"))
    for (cluster in clusters) for (term in law_terms) {
      select_one(df, sample = samples[[1L]], cluster = cluster, term = term,
                 label = paste(name, cluster, term))
    }
    fail_if(any(df$se < 0 | df$p_value < 0 | df$p_value > 1),
            paste("Invalid", name, "standard errors or p-values"))
  }
  est <- t$estimates
  diag <- t$diagnostics
  fail_if(anyDuplicated(est[c("sample", "model", "cluster", "term")]) > 0L ||
            anyDuplicated(diag[c("sample", "cluster")]) > 0L,
          "Duplicate estimate or diagnostic rows")
  for (sample in samples) for (cluster in clusters) {
    d <- select_one(diag, sample = sample, cluster = cluster,
                    label = paste(sample, cluster, "diagnostics"))
    fail_if(anyNA(d[, c("observations", "scientist_count", "first_stage_joint_F",
                        "anderson_rubin_zero_p")]) ||
              d$observations <= 0 || d$scientist_count < 0 ||
              d$first_stage_joint_F < 0 ||
              d$anderson_rubin_zero_p < 0 || d$anderson_rubin_zero_p > 1,
            "Invalid model diagnostics")
    for (model in c("ols", "iv_2sls")) {
      row <- select_one(est, sample = sample, model = model, cluster = cluster,
                        label = paste(sample, model, cluster))
      fail_if(anyNA(row[, c("estimate", "se", "effect_per_100k_per_school_year",
                              "se_per_100k")]) || row$se < 0 ||
                abs(1e5 * row$estimate - row$effect_per_100k_per_school_year) > 1e-5 ||
                abs(1e5 * row$se - row$se_per_100k) > 1e-5,
              "Effect units do not reconcile with probability units")
    }
  }
  main_d <- select_one(diag, sample = samples[[1L]], cluster = "state")
  main_sc <- select_one(diag, sample = samples[[1L]], cluster = "state_cohort")
  fail_if(main_d$observations != flow$n[[5L]] ||
            main_d$scientist_count != flow$n[[6L]] ||
            main_sc$observations != main_d$observations ||
            main_sc$scientist_count != main_d$scientist_count,
          "Main diagnostics disagree with sample flow")
  for (name in c("first_stage", "reduced_form")) for (term in law_terms) {
    a <- select_one(t[[name]], sample = samples[[1L]], cluster = "state_cohort",
                    term = term)
    b <- select_one(t[[name]], sample = samples[[1L]], cluster = "state",
                    term = term)
    fail_if(abs(a$estimate - b$estimate) > 1e-10,
            paste(name, term, "coefficient differs across clustering"))
  }
  interval <- select_one(t$anderson_rubin_intervals, sample = samples[[1L]],
                         cluster = "state", label = "Anderson-Rubin interval")
  iv <- select_one(est, sample = samples[[1L]], model = "iv_2sls", cluster = "state")
  fail_if(anyNA(interval[, c("lower", "upper", "lower_per_100k", "upper_per_100k")]) ||
            abs(interval$confidence - 0.95) > 1e-8 ||
            interval$lower >= iv$estimate || interval$upper <= iv$estimate ||
            abs(1e5 * interval$lower - interval$lower_per_100k) > 1e-5 ||
            abs(1e5 * interval$upper - interval$upper_per_100k) > 1e-5,
          "Invalid or unbounded main Anderson-Rubin interval")
  fail_if(!any(grepl("1940 schooling -> 1950 scientist occupation", t$notes,
                     fixed = TRUE)) ||
            !any(grepl("The linked extract is selected", t$notes, fixed = TRUE)),
          "Run notes do not identify the source estimand and selection")
  invisible(TRUE)
}

fmt_count <- function(x) format(round(x), big.mark = ",", trim = TRUE,
                                scientific = FALSE)
fmt <- function(x, digits) sprintf(paste0("%.", digits, "f"), x)
significance_stars <- function(p) {
  mark <- if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
  if (nzchar(mark)) paste0("\\textsuperscript{", mark, "}") else ""
}
row_end <- " \\\\"

render_sample <- function(t) {
  flow <- t$sample_flow
  audit <- t$linkage_audit
  agreement <- c("Same recorded birth state" = audit$same_birthplace,
                 "Male code in 1950" = audit$male_in_1950,
                 "White code in 1950" = audit$white_in_1950,
                 "Age change of 9--11 years" = audit$age_change_9_to_11)
  flow_rows <- vapply(seq_len(nrow(flow)), function(i)
    paste0(flow$step[[i]], " & ", fmt_count(flow$n[[i]]), " &", row_end), character(1))
  audit_rows <- vapply(seq_along(agreement), function(i)
    paste0(names(agreement)[[i]], " & ", fmt_count(agreement[[i]]), " & ",
           fmt(100 * agreement[[i]] / audit$matched_people, 1), "\\%", row_end),
    character(1))
  c("% Generated from linked-census IV result CSVs. Do not edit by hand.",
    "\\begin{table}[H]", "\\centering",
    "\\caption{Linked 1940--1950 analysis sample and record agreement}",
    "\\label{tab:ms_scientist_iv_sample}", "\\small",
    "\\begin{tabular}{lrr}", "\\toprule",
    paste0("Sample restriction & Men &", row_end), "\\midrule",
    flow_rows, "\\addlinespace", "\\midrule",
    paste0("Linked-record check & Agreeing records & Share of pairs", row_end),
    "\\midrule", audit_rows, "\\bottomrule", "\\end{tabular}",
    "\\caption*{\\footnotesize \\textit{Notes:} Eligibility follows Clay, Lingwall and Stephens: native-born white men aged 27--54 in 1940, with birth cohorts 1885--1912 and a state coded in the CLS law file. The final analysis requires reported 1940 schooling and a unique 1950 HIK match. Agreement percentages use the unique matched pairs as denominator; disagreements are retained in the main analysis and examined through a stricter-link sensitivity.}",
    "\\end{table}")
}

render_descriptives <- function(t) {
  cells <- t$main_cells
  groups <- list(cells, cells[cells$law_bin == "0", , drop = FALSE],
                 cells[cells$law_bin != "0", , drop = FALSE])
  summarize <- function(d) {
    n <- sum(d$n)
    mean_school <- sum(d$n * d$school_years) / n
    sd_school <- sqrt(sum(d$n * (d$school_years - mean_school)^2) / (n - 1))
    c(men = fmt_count(n),
      schooling = paste0(fmt(mean_school, 2), " (", fmt(sd_school, 2), ")"),
      high_school = paste0(fmt(100 * sum(d$n[d$school_years >= 12]) / n, 2), "\\%"),
      ba_equivalent = paste0(fmt(100 * sum(d$n[d$school_years >= 16]) / n, 2), "\\%"),
      scientists = fmt_count(sum(d$n * d$scientist)),
      scientist_share = paste0(fmt(100 * sum(d$n * d$scientist) / n, 3), "\\%"))
  }
  values <- lapply(groups, summarize)
  labels <- c(men = "Men", schooling = "Completed schooling, mean (SD)",
              high_school = "High school or more (12+ years)",
              ba_equivalent = "BA-equivalent schooling (16+ years)",
              scientists = "Scientist occupations in 1950",
              scientist_share = "Scientist occupations in 1950, share")
  rows <- vapply(names(labels), function(name)
    paste0(labels[[name]], " & ", paste(vapply(values, `[[`, character(1), name),
                                       collapse = " & "), row_end), character(1))
  c("% Generated from linked-census IV result CSVs. Do not edit by hand.",
    "\\begin{table}[H]", "\\centering",
    "\\caption{Descriptive statistics by compulsory-schooling requirement}",
    "\\label{tab:ms_scientist_iv_descriptives}", "\\small",
    "\\begin{tabular}{lrrr}", "\\toprule",
    paste0(" & All linked men & 0 required years & 1+ required years", row_end),
    "\\midrule", rows, "\\bottomrule", "\\end{tabular}",
    "\\caption*{\\footnotesize \\textit{Notes:} Main linked sample only. Groups use the combined Clay--Lingwall--Stephens required-years measure used in the IV; zero means no years required for that birth-state cohort, not necessarily no related law on the books. Schooling is highest grade completed in 1940, recoded as years. High school and BA-equivalent schooling use thresholds of 12 and 16 completed years, respectively; a degree award is not observed. Scientist occupation is coded from the linked 1950 record. Descriptive group differences are not causal effects.}",
    "\\end{table}")
}

render_laws <- function(t) {
  labels <- c(z_1_5 = "1--5", z_6 = "6", z_7 = "7", z_8 = "8",
              z_9plus = "9 or more")
  rows <- unlist(lapply(law_terms, function(term) {
    fs <- select_one(t$first_stage, sample = samples[[1L]],
                     cluster = "state_cohort", term = term)
    fs_state <- select_one(t$first_stage, sample = samples[[1L]],
                           cluster = "state", term = term)
    rf <- select_one(t$reduced_form, sample = samples[[1L]],
                     cluster = "state_cohort", term = term)
    rf_state <- select_one(t$reduced_form, sample = samples[[1L]],
                           cluster = "state", term = term)
    c(paste0(labels[[term]], " & ", fmt(fs$estimate, 3),
             significance_stars(fs$p_value), " & ",
             fmt(1e5 * rf$estimate, 1),
             significance_stars(rf$p_value), row_end),
      paste0(" & (", fmt(fs$se, 3), ") [", fmt(fs_state$se, 3),
             "] & (", fmt(1e5 * rf$se, 1), ") [",
             fmt(1e5 * rf_state$se, 1), "]", row_end))
  }), use.names = FALSE)
  d <- select_one(t$diagnostics, sample = samples[[1L]], cluster = "state")
  d_sc <- select_one(t$diagnostics, sample = samples[[1L]], cluster = "state_cohort")
  c("% Generated from linked-census IV result CSVs. Do not edit by hand.",
    "\\begin{table}[H]", "\\centering",
    "\\caption{Compulsory-schooling requirements: first stage and reduced form}",
    "\\label{tab:ms_scientist_iv_laws}", "\\small",
    "\\begin{tabular}{lrr}", "\\toprule",
    paste0("Required years & Schooling in 1940 & Scientist in 1950", row_end),
    paste0(" & Years completed & Per 100{,}000 men", row_end),
    "\\midrule", rows, "\\midrule",
    paste0("Observations & ", fmt_count(d$observations), " & ",
           fmt_count(d$observations), row_end),
    paste0("Joint first-stage $F$, state $\\times$ cohort clustered & ",
           fmt(d_sc$first_stage_joint_F, 2), " & --", row_end),
    paste0("Joint first-stage $F$, state clustered & ",
           fmt(d$first_stage_joint_F, 2), " & --", row_end),
    "\\bottomrule", "\\end{tabular}",
    "\\caption*{\\footnotesize \\textit{Notes:} Main linked sample. Each coefficient compares a law-requirement bin with zero required years. Parentheses are standard errors clustered by birth-state $\\times$ birth cohort; brackets are standard errors clustered by birth state. Stars use birth-state $\\times$ cohort clustered $p$-values: * $p<0.10$, ** $p<0.05$, *** $p<0.01$. Both regressions include birth-state and CLS-region $\\times$ birth-cohort fixed effects. Reduced-form coefficients and standard errors are multiplied by 100{,}000. Anderson--Rubin inference uses birth-state clustering.}",
    "\\end{table}")
}

render_effects <- function(t) {
  labels <- c(linked_1940_grade = "Linked 1940 grade (main)",
              consistent_link_sensitivity = "Consistent HIK links",
              same_record_1950_grade = "1950 grade sample line")
  rows <- unlist(lapply(samples, function(sample) {
    ols_sc <- select_one(t$estimates, sample = sample, model = "ols",
                         cluster = "state_cohort")
    ols_st <- select_one(t$estimates, sample = sample, model = "ols",
                         cluster = "state")
    iv_sc <- select_one(t$estimates, sample = sample, model = "iv_2sls",
                        cluster = "state_cohort")
    iv_st <- select_one(t$estimates, sample = sample, model = "iv_2sls",
                        cluster = "state")
    d <- select_one(t$diagnostics, sample = sample, cluster = "state")
    d_sc <- select_one(t$diagnostics, sample = sample, cluster = "state_cohort")
    c(paste0(labels[[sample]], " & ",
             fmt(ols_st$effect_per_100k_per_school_year, 1), " & ",
             fmt(iv_st$effect_per_100k_per_school_year, 1), " & ",
             fmt_count(d$observations), " & ",
             fmt(d_sc$first_stage_joint_F, 2), " / ",
             fmt(d$first_stage_joint_F, 2), " & ",
             fmt(d$anderson_rubin_zero_p, 3), row_end),
      paste0(" & (", fmt(ols_sc$se_per_100k, 1), ") [",
             fmt(ols_st$se_per_100k, 1), "] & (",
             fmt(iv_sc$se_per_100k, 1), ") [",
             fmt(iv_st$se_per_100k, 1), "] & & &", row_end))
  }), use.names = FALSE)
  interval <- select_one(t$anderson_rubin_intervals, sample = samples[[1L]],
                         cluster = "state")
  c("% Generated from linked-census IV result CSVs. Do not edit by hand.",
    "\\begin{table}[H]", "\\centering",
    "\\caption{Completed schooling and 1950 scientist occupation}",
    "\\label{tab:ms_scientist_iv_effects}", "\\footnotesize",
    "\\setlength{\\tabcolsep}{5pt}",
    "\\begin{tabular}{lrrrrr}", "\\toprule",
    paste0("Sample & OLS & 2SLS & Men & \\shortstack{First-stage $F$\\\\state $\\times$ cohort / state} & AR $p(0)$", row_end),
    "\\midrule", rows, "\\bottomrule", "\\end{tabular}",
    paste0("\\caption*{\\footnotesize \\textit{Notes:} Effects are additional scientist occupations per 100{,}000 men per completed school year. Parentheses are birth-state $\\times$ cohort clustered standard errors; brackets are birth-state clustered standard errors. All models include birth-state and CLS-region $\\times$ cohort fixed effects. First-stage $F$ values use state $\\times$ cohort / state clustering, respectively. The Anderson--Rubin (AR) $p$ uses state clustering. The main-sample 95\\% AR confidence set is [",
           fmt(interval$lower_per_100k, 1), ", ",
           fmt(interval$upper_per_100k, 1),
           "]. Consistent HIK links agree on birth state, sex, race, and age change of 9--11 years. The 1950-grade sensitivity uses one census row and is unweighted because this extract lacks SLWT.}"),
    "\\end{table}")
}

render_document <- function(t) {
  d <- select_one(t$diagnostics, sample = samples[[1L]], cluster = "state")
  d_sc <- select_one(t$diagnostics, sample = samples[[1L]], cluster = "state_cohort")
  iv <- select_one(t$estimates, sample = samples[[1L]], model = "iv_2sls",
                   cluster = "state")
  interval <- select_one(t$anderson_rubin_intervals, sample = samples[[1L]],
                         cluster = "state")
  built <- grep("^Built: ", t$notes, value = TRUE)
  fail_if(length(built) != 1L || !grepl("^Built: [0-9]{4}-[0-9]{2}-[0-9]{2}", built),
          "Missing analysis build date in notes")
  run_date <- sub("^Built: ([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", built)
  c("% Generated by analysis/mandatory_schooling/export_linked_1940_schooling_1950_scientist_iv_to_project.R",
    "% Standalone results note; compile from project/ with pdflatex and bibtex.",
    "% Do not edit generated estimates by hand.",
    "\\documentclass[11pt]{article}",
    "\\usepackage[a4paper,margin=1in]{geometry}",
    "\\usepackage{amsmath}",
    "\\usepackage{booktabs}", "\\usepackage{caption}",
    "\\usepackage{float}", "\\usepackage{microtype}",
    "\\usepackage{setspace}",
    "\\usepackage[authoryear,round]{natbib}",
    "\\usepackage[colorlinks=true,citecolor=blue,linkcolor=black,urlcolor=blue]{hyperref}",
    "\\setlength{\\parindent}{0pt}",
    "\\setlength{\\parskip}{0.5em}",
    "\\begin{document}",
    "\\begin{center}",
    "{\\LARGE\\bfseries Compulsory Schooling and Scientist Occupations}\\\\[0.35em]",
    "{\\large Linked 1940--1950 Census IV results}\\\\[0.55em]",
    paste0("{\\small Analysis run ", run_date,
           "; tables exported ", format(Sys.Date(), "%Y-%m-%d"), "}"),
    "\\end{center}",
    "\\doublespacing",
    "\\section*{Question and design}",
    "This note asks whether additional completed schooling changes the probability of holding a scientist occupation in the 1950 Census. It adapts the first-wave compulsory-schooling-law design of \\citet{clay2021}: five indicators for required years of attendance instrument for completed schooling. The zero-requirement bin is the reference. Birth-state and CLS-region-by-birth-cohort fixed effects enter every model.",
    "The main sample is native-born white men aged 27--54 in 1940 whose 1940 record has a reported highest grade and links by HIK to a unique 1950 record. Birth cohort is $1940-\\mathrm{AGE}-1$ and law exposure uses the 1940 birthplace. Schooling is measured in 1940; the outcome is the 1950 row's occupation, coded as scientist for OCC1950 007, 061, 062, 063, 067, 068, or 069. It does not identify the year someone first became a scientist.",
    "\\section*{Empirical Methodology}",
    "Let $i$ index men, $s$ their birth state, $c$ their birth year, and $r(s)$ the Clay--Lingwall--Stephens region containing state $s$. Let $S_{isc}$ be completed years of schooling in 1940 and $Y_{isc}$ indicate a scientist occupation in 1950. Following \\citet{clay2021}, the first stage is",
    "\\begin{equation}",
    "S_{isc}=\\alpha_s^{S}+\\lambda_{r(s)c}^{S}+\\sum_{b\\in\\mathcal{B}}\\pi_b Z^b_{sc}+u_{isc},",
    "\\end{equation}",
    "where $Z^b_{sc}$ indicates that the combined compulsory-schooling requirement for birth-state--year cell $(s,c)$ falls in bin $b\\in\\mathcal{B}=\\{1\\text{--}5,6,7,8,9+\\}$; zero required years is the omitted category. The second stage is",
    "\\begin{equation}",
    "Y_{isc}=\\alpha_s^{Y}+\\lambda_{r(s)c}^{Y}+\\beta\\widehat{S}_{isc}+\\varepsilon_{isc}.",
    "\\end{equation}",
    "Both stages include birth-state and region-by-birth-year fixed effects, with coefficients allowed to differ across stages. Since $Y_{isc}$ is binary, $\\beta$ is a change in the probability of a scientist occupation per additional completed school year. Table~\\ref{tab:ms_scientist_iv_laws} reports the first stage and reduced form; Table~\\ref{tab:ms_scientist_iv_effects} multiplies the 2SLS estimate by 100{,}000 to express additional scientist occupations per 100{,}000 men.",
    "\\input{tables/mandatory_schooling_scientist_iv_sample.tex}",
    "\\begin{singlespace}",
    "\\input{tables/mandatory_schooling_scientist_iv_descriptives.tex}",
    "\\end{singlespace}",
    "\\section*{Law variation and the first stage}",
    "Table~\\ref{tab:ms_scientist_iv_laws} reports the law-bin associations with completed schooling and the reduced-form associations with scientist occupation. The first-stage coefficients rise across the higher requirement bins. We report joint first-stage tests with both state $\\times$ cohort and state clustering. The state-clustered test is modest, so the conventional 2SLS standard error is supplemented by Anderson--Rubin inference.",
    "\\begin{singlespace}",
    "\\input{tables/mandatory_schooling_scientist_iv_laws.tex}",
    "\\end{singlespace}",
    "\\section*{Schooling estimates and interpretation}",
    paste0("In the main linked sample, the 2SLS point estimate is ",
           fmt(iv$effect_per_100k_per_school_year, 1),
           " additional scientist occupations per 100{,}000 men per additional completed school year, against a sample prevalence of ",
           fmt(d$scientist_rate_per_100k, 1),
           " per 100{,}000. The first-stage $F$ is ",
           fmt(d_sc$first_stage_joint_F, 2), " with birth-state $\\times$ cohort clustering and ",
           fmt(d$first_stage_joint_F, 2), " with birth-state clustering",
           ". The weak-instrument-robust 95\\% Anderson--Rubin confidence set spans ",
           fmt(interval$lower_per_100k, 1), " to ",
           fmt(interval$upper_per_100k, 1),
           " per 100{,}000, indicating substantial uncertainty about magnitude."),
    "\\begin{singlespace}",
    "\\input{tables/mandatory_schooling_scientist_iv_effects.tex}",
    "\\end{singlespace}",
    "The linked sample is selected by successful cross-census matching and does not represent the full 1950 population. The separate 1950-grade analysis uses a sample-line variable without SLWT in this extract. A causal schooling interpretation also requires the laws to affect scientist occupation through individual schooling, conditional on the fixed effects; compulsory attendance, child-labor, and continuation-school provisions make that exclusion restriction substantive.",
    "\\clearpage",
    "\\begin{singlespace}",
    "\\bibliographystyle{plainnat}",
    "\\bibliography{references}",
    "\\end{singlespace}",
    "\\end{document}")
}

prepare_document <- function(t, path) {
  doc <- if (file.exists(path)) readLines(path, warn = FALSE) else render_document(t)
  anchor <- "\\input{tables/mandatory_schooling_scientist_iv_sample.tex}"
  addition <- "\\input{tables/mandatory_schooling_scientist_iv_descriptives.tex}"
  anchor_lines <- which(sub("^%[[:space:]]*", "", trimws(doc)) == anchor)
  fail_if(length(anchor_lines) != 1L || sum(doc == addition) > 1L,
          "Existing document has an unexpected sample or descriptive-table input")
  if (!any(doc == addition)) {
    doc <- append(doc, addition, after = anchor_lines)
  }
  date_lines <- grep("tables exported [0-9]{4}-[0-9]{2}-[0-9]{2}", doc)
  fail_if(length(date_lines) != 1L, "Existing document has no unique export date")
  doc[[date_lines]] <- sub("tables exported [0-9]{4}-[0-9]{2}-[0-9]{2}",
                           paste("tables exported", format(Sys.Date(), "%Y-%m-%d")),
                           doc[[date_lines]])
  doc
}

export_tables <- function(run_dir, project_dir) {
  inputs <- read_inputs(run_dir)
  validate_inputs(inputs$tables)
  table_dir <- file.path(project_dir, "tables")
  fail_if(!dir.exists(table_dir), paste("Missing project/tables:", table_dir))
  output <- c(sample = file.path(table_dir, "mandatory_schooling_scientist_iv_sample.tex"),
              descriptives = file.path(table_dir, "mandatory_schooling_scientist_iv_descriptives.tex"),
              laws = file.path(table_dir, "mandatory_schooling_scientist_iv_laws.tex"),
              effects = file.path(table_dir, "mandatory_schooling_scientist_iv_effects.tex"),
              document = file.path(project_dir, "mandatory_schooling_scientist_iv.tex"))
  rendered <- list(sample = render_sample(inputs$tables),
                   descriptives = render_descriptives(inputs$tables),
                   laws = render_laws(inputs$tables),
                   effects = render_effects(inputs$tables),
                   document = prepare_document(inputs$tables, output[["document"]]))
  for (name in names(output)) writeLines(rendered[[name]], output[[name]], useBytes = TRUE)
  dependency <- list(sample = c("sample_flow", "linkage_audit", "notes"),
                     descriptives = c("main_cells", "law_bin_counts", "notes"),
                     laws = c("first_stage", "reduced_form", "diagnostics", "notes"),
                     effects = c("estimates", "diagnostics", "anderson_rubin_intervals", "notes"),
                     document = names(inputs$files))
  manifest <- do.call(rbind, lapply(names(dependency), function(name) {
    source_files <- inputs$files[dependency[[name]]]
    data.frame(destination = basename(output[[name]]),
               source = normalizePath(source_files, winslash = "/"),
               source_md5 = unname(tools::md5sum(source_files)),
               source_modified = format(file.info(source_files)$mtime,
                                        "%Y-%m-%d %H:%M:%S %Z"),
               exported = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
               row.names = NULL)
  }))
  manifest_path <- file.path(table_dir, "mandatory_schooling_scientist_iv_manifest.csv")
  utils::write.csv(manifest, manifest_path, row.names = FALSE)
  c(output, manifest = manifest_path)
}

main <- function() {
  root <- repo_root()
  run_dir <- Sys.getenv("MS_SCIENTIST_IV_RESULTS_DIR",
                        unset = file.path(data_root(root), "results/mandatory_schooling",
                                          "linked_1940_schooling_1950_scientist_iv"))
  project_dir <- Sys.getenv("MS_SCIENTIST_IV_PROJECT_DIR",
                            unset = file.path(root, "project"))
  output <- export_tables(run_dir, project_dir)
  message("Wrote:\n", paste(output, collapse = "\n"))
}

if (sys.nframe() == 0L) main()
