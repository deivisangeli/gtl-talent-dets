# CLS instruments for BA-equivalent completion, using saved linked-census cells.
# Rscript analysis/mandatory_schooling/estimate_linked_1940_ba_1950_scientist_iv.R
# Overrides: MS_BA_IV_SOURCE_DIR, MS_BA_IV_RESULTS_DIR, MS_BA_IV_PROJECT_DIR.

find_ba_repo <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[[1L]]))) else
    Sys.getenv("GTL_REPO", unset = getwd())
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not find paths.R", call. = FALSE)
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
}
source(file.path(find_ba_repo(), "analysis/mandatory_schooling",
                 "estimate_linked_1940_schooling_1950_scientist_iv.R"), local = TRUE)
source(file.path(find_ba_repo(), "analysis/mandatory_schooling",
                 "export_linked_1940_ba_1950_scientist_iv_to_project.R"), local = TRUE)

ba_samples <- c("linked_1940_grade", "consistent_link_sensitivity", "same_record_1950_grade")
ba_cell_files <- c("main_cells", "quality_link_cells", "sensitivity_1950_grade_cells")

read_ba_inputs <- function(source_dir) {
  input_names <- c(ba_cell_files, "sample_flow", "linkage_audit", "law_bin_counts",
             "first_stage", "reduced_form", "estimates", "diagnostics")
  files <- setNames(file.path(source_dir, paste0(input_names, ".csv")), input_names)
  files <- c(files, notes = file.path(source_dir, "notes.txt"))
  fail_if(any(!file.exists(files)),
          paste("Missing source inputs:", paste(files[!file.exists(files)], collapse = ", ")))
  tables <- lapply(files[input_names], utils::read.csv, stringsAsFactors = FALSE, check.names = FALSE)
  audit_columns <- list(sample_flow = c("step", "n"),
    linkage_audit = c("matched_rows", "matched_people", "observed_occupation",
                      "scientists", "scientist_code_mismatches"),
    law_bin_counts = c("law_bin", "men", "scientists"))
  for (name in names(audit_columns)) {
    needed <- audit_columns[[name]]
    fail_if(!all(needed %in% names(tables[[name]])), paste("Missing audit columns:", name))
    fail_if(anyNA(tables[[name]][needed]), paste("Missing audit values:", name))
    for (column in setdiff(needed, c("step", "law_bin"))) {
      x <- tables[[name]][[column]]
      fail_if(!is.numeric(x) || any(!is.finite(x) | x < 0 | x != floor(x)),
              paste("Invalid audit counts:", name, column))
    }
  }
  for (name in ba_cell_files) {
    d <- tables[[name]]
    needed <- c("bpl", "yob", "region", "law_bin", "school_years", "scientist", "n")
    fail_if(!all(needed %in% names(d)), paste("Missing cell columns:", name))
    fail_if(anyNA(d[needed]) || any(!d$school_years %in% 0:17) ||
              any(!d$scientist %in% 0:1) || any(!is.finite(d$n) | d$n <= 0 | d$n != floor(d$n)) ||
              any(!d$law_bin %in% c("0", "1_5", "6", "7", "8", "9plus")) ||
              any(!d$yob %in% 1885:1912) || any(!d$bpl %in% 1:56) ||
              any(!d$region %in% 1:4), paste("Invalid regression cells:", name))
    fail_if(anyDuplicated(d[setdiff(needed, "n")]) > 0L, paste("Duplicate cells:", name))
    mapping <- unique(d[c("bpl", "yob", "region", "law_bin")])
    fail_if(anyDuplicated(mapping[c("bpl", "yob")]) > 0L || nrow(mapping) != 1372L ||
              length(unique(d$bpl)) != 49L, paste("Invalid state-cohort mapping:", name))
    fail_if(anyDuplicated(unique(d[c("bpl", "region")])$bpl) > 0L,
            paste("Inconsistent CLS regions:", name))
    tables[[name]] <- prepare_cells(d)
  }
  main <- tables$main_cells
  audit <- tables$linkage_audit
  flow <- tables$sample_flow
  fail_if(nrow(audit) != 1L || audit$matched_rows != audit$matched_people ||
            audit$scientist_code_mismatches != 0 ||
            sum(main$n) != audit$observed_occupation ||
            sum(main$n * main$scientist) != audit$scientists,
          "Main cells disagree with linkage audit")
  fail_if(anyDuplicated(flow$step) > 0L || anyNA(flow$n) ||
            any(diff(flow$n) > 0) ||
            !identical(as.numeric(tail(flow$n, 2)),
                       c(sum(main$n), sum(main$n * main$scientist))),
          "Main cells disagree with sample flow")
  by_bin <- aggregate(cbind(men = main$n, scientists = main$n * main$scientist),
                      list(law_bin = main$law_bin), sum)
  compare_ba_tables(by_bin, tables$law_bin_counts, "law_bin", c("men", "scientists"))
  for (i in 2:3) {
    other <- unique(tables[[ba_cell_files[[i]]]][c("bpl", "yob", "region", "law_bin")])
    compare_ba_tables(other, unique(main[c("bpl", "yob", "region", "law_bin")]),
                      c("bpl", "yob"), c("region", "law_bin"))
  }
  list(tables = tables, files = files)
}

compare_ba_tables <- function(actual, expected, keys, columns, tolerance = 1e-8) {
  fail_if(!all(c(keys, columns) %in% names(actual)) ||
            !all(c(keys, columns) %in% names(expected)), "Missing comparison columns")
  fail_if(anyDuplicated(actual[keys]) > 0L || anyDuplicated(expected[keys]) > 0L,
          "Duplicate comparison keys")
  joined <- merge(actual[c(keys, columns)], expected[c(keys, columns)],
                  by = keys, all = TRUE, suffixes = c("_new", "_old"))
  fail_if(nrow(joined) != nrow(actual) || nrow(actual) != nrow(expected),
          "Comparison row sets differ")
  for (column in columns) {
    fail_if(!isTRUE(all.equal(joined[[paste0(column, "_new")]],
                             joined[[paste0(column, "_old")]], tolerance = tolerance)),
            paste("Saved baseline mismatch:", column))
  }
  invisible(TRUE)
}

main <- function() {
  repo <- find_ba_repo()
  root <- data_root(repo)
  source_dir <- Sys.getenv("MS_BA_IV_SOURCE_DIR", unset = file.path(root,
    "results/mandatory_schooling/linked_1940_schooling_1950_scientist_iv"))
  out_dir <- Sys.getenv("MS_BA_IV_RESULTS_DIR", unset = file.path(root,
    "results/mandatory_schooling/linked_1940_ba_1950_scientist_iv"))
  project_dir <- Sys.getenv("MS_BA_IV_PROJECT_DIR", unset = file.path(repo, "project"))
  fail_if(tolower(normalizePath(source_dir, winslash = "/", mustWork = FALSE)) ==
            tolower(normalizePath(out_dir, winslash = "/", mustWork = FALSE)),
          "BA results must not overwrite schooling-years inputs")
  inputs <- read_ba_inputs(source_dir)
  fits <- vector("list", 3L)
  years_fits <- vector("list", 3L)
  for (i in seq_along(ba_samples)) {
    message("Estimating BA and checking saved schooling baseline: ", ba_samples[[i]])
    d <- inputs$tables[[ba_cell_files[[i]]]]
    fits[[i]] <- estimate_models(d, ba_samples[[i]], endogenous = "ba_completed")
    years_fits[[i]] <- estimate_models(d, ba_samples[[i]])
  }
  names(fits) <- names(years_fits) <- ba_samples
  combine <- function(fits, table) do.call(rbind, lapply(fits, function(f) f$tables[[table]]))
  tables <- setNames(lapply(names(fits[[1L]]$tables), function(n) combine(fits, n)),
                     names(fits[[1L]]$tables))
  result_keys <- c("sample", "model", "cluster", "term")
  for (name in c("first_stage", "reduced_form", "estimates")) {
    actual <- combine(years_fits, name)
    expected <- inputs$tables[[name]]
    compare_ba_tables(actual, expected, result_keys, setdiff(names(expected), result_keys))
  }
  compare_ba_tables(tables$reduced_form, inputs$tables$reduced_form, result_keys,
                    c("estimate", "se", "p_value"))
  stable <- c("observations", "state_cohort_cells", "states", "scientist_count",
              "scientist_rate_per_100k", "anderson_rubin_zero_F", "anderson_rubin_zero_p")
  compare_ba_tables(tables$diagnostics, inputs$tables$diagnostics,
                    c("sample", "cluster"), stable)
  compare_ba_tables(combine(years_fits, "diagnostics"), inputs$tables$diagnostics,
                    c("sample", "cluster"), setdiff(names(inputs$tables$diagnostics),
                                                      c("sample", "cluster")))
  fail_if(!all(tables$estimates$term %in% c("ba_completed", "fit_ba_completed")),
          "Unexpected or unidentified BA coefficient")
  fail_if(any(!is.finite(unlist(tables$diagnostics[setdiff(names(tables$diagnostics),
                                                        c("sample", "cluster"))]))),
          "Nonfinite diagnostics")
  tables$first_stage$estimate_percentage_points <- 100 * tables$first_stage$estimate
  tables$first_stage$se_percentage_points <- 100 * tables$first_stage$se
  tables$completion_shares <- do.call(rbind, lapply(seq_along(ba_samples), function(i) {
    d <- inputs$tables[[ba_cell_files[[i]]]]
    data.frame(sample = ba_samples[[i]], observations = sum(d$n),
               completions = sum(d$n * d$ba_completed),
               completion_share = sum(d$n * d$ba_completed) / sum(d$n),
               scientist_count = sum(d$n * d$scientist),
               scientist_rate_per_100k = 1e5 * sum(d$n * d$scientist) / sum(d$n))
  }))
  message("Baseline invariance checks passed. Inverting 2,001 state-clustered AR tests...")
  ar <- ar_confidence_set(inputs$tables$main_cells, ba_samples[[1L]],
                          limit = 1, grid_points = 2001L, endogenous = "ba_completed")
  zero_p <- ar$grid$p_value[ar$grid$beta == 0]
  fail_if(length(zero_p) != 1L || !isTRUE(all.equal(zero_p,
    tables$diagnostics$anderson_rubin_zero_p[tables$diagnostics$sample == ba_samples[[1L]] &
                                            tables$diagnostics$cluster == "state"],
    tolerance = 1e-8)), "AR zero differs from reduced-form test")
  ar$intervals$lower_boundary_reached <- is.na(ar$intervals$lower)
  ar$intervals$upper_boundary_reached <- is.na(ar$intervals$upper)
  ar$intervals$search_lower <- rep(-1, nrow(ar$intervals))
  ar$intervals$search_upper <- rep(1, nrow(ar$intervals))
  tables$anderson_rubin_grid <- ar$grid
  tables$anderson_rubin_intervals <- ar$intervals
  for (name in c("sample_flow", "linkage_audit", "law_bin_counts")) {
    tables[[name]] <- inputs$tables[[name]]
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (name in names(tables)) write_table(tables[[name]], file.path(out_dir, paste0(name, ".csv")))
  for (name in ba_cell_files) {
    write_table(inputs$tables[[name]], file.path(out_dir, paste0(name, ".csv")))
  }
  scripts <- file.path(repo, "analysis/mandatory_schooling", c(
    "estimate_linked_1940_schooling_1950_scientist_iv.R",
    "estimate_linked_1940_ba_1950_scientist_iv.R",
    "export_linked_1940_ba_1950_scientist_iv_to_project.R"))
  provenance_files <- c(unname(inputs$files), scripts)
  provenance <- data.frame(path = normalizePath(provenance_files, winslash = "/"),
                           bytes = file.info(provenance_files)$size,
                           modified = format(file.info(provenance_files)$mtime,
                                             "%Y-%m-%d %H:%M:%S %Z"),
                           md5 = unname(tools::md5sum(provenance_files)))
  write_table(provenance, file.path(out_dir, "input_provenance.csv"))
  saveRDS(lapply(fits, function(f) f$models), file.path(out_dir, "models.rds"))
  notes <- c("CLS law IV: 1940 BA-equivalent completion -> 1950 scientist occupation",
    paste("Built:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Saved-cell source:", normalizePath(source_dir, winslash = "/")),
    "No census microdata scanned; sample audits and schooling-years baseline validated.",
    "BA-equivalent completion = 1(school_years >= 16), including postgraduate schooling.",
    "Historical census schooling measures completed grades, not an awarded BA degree.",
    "Main: native-born white men, ages 27-54 in 1940, yob=1940-AGE-1, born 1885-1912.",
    "Scientist outcome: 1950 OCC1950 codes 007,061,062,063,067,068,069.",
    "Other recorded occupation/nonoccupation codes are zero; missing flags are excluded.",
    "CLS requirement bins 1-5,6,7,8,9+ instrument completion; zero omitted.",
    "Birth-state and CLS-region x birth-year fixed effects; count weights; K.adj=FALSE.",
    "Both state-cohort and state clustering reported; emphasize state clustering.",
    "First-stage effects are probabilities and percentage points.",
    "2SLS and OLS effects are probabilities and scientist occupations per 100,000 men",
    "for a change from noncompletion to completion; these are linear probability models.",
    "Consistent links: same birth state, male and white in 1950, age change 9-11 years.",
    "Same-record sensitivity uses 1950 schooling; unweighted because extract omits SLWT.",
    "Main 95% state-clustered AR set: 2,001 grid points over [-1,1], roots at tol=1e-8.",
    "All detected accepted intervals retained; boundary flags mean search truncation,",
    "not proven infinite endpoints. Grid inversion may miss regions narrower than 0.001.",
    "The reduced form, AR test at zero, sample counts and scientist totals match the baseline.",
    "Causal interpretation requires relevant instruments, valid exclusion, monotonicity,",
    "and comparable law timing conditional on fixed effects. BA-specific exclusion also",
    "requires CLS-induced changes at other schooling margins to have no separate effect.",
    "Selected census links are not representative of all men; outcome is observed scientist",
    "occupation in 1950, not first entry into science or lifetime scientist production.")
  writeLines(notes, file.path(out_dir, "notes.txt"))
  writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info.txt"))
  write_table(data.frame(check = c("saved_schooling_results", "reduced_form",
    "sample_and_scientist_counts", "AR_zero"), passed = TRUE),
    file.path(out_dir, "baseline_validation.csv"))
  write_ba_note(tables, inputs$tables, source_dir, out_dir, project_dir)
  message("Wrote BA results to ", out_dir)
  print(tables$estimates[tables$estimates$cluster == "state", ])
  print(tables$diagnostics[tables$diagnostics$cluster == "state", ])
  print(ar$intervals)
}

if (sys.nframe() == 0L) main()
