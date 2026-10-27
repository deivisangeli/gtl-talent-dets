# Small, self-contained exporter fixture. Run from the repository root.
source("analysis/mandatory_schooling/export_linked_1940_schooling_1950_scientist_iv_to_project.R",
       local = TRUE)

root <- file.path(tempdir(), "scientist_iv_tex_fixture")
run_dir <- file.path(root, "results")
project_dir <- file.path(root, "project")
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(project_dir, "tables"), recursive = TRUE,
           showWarnings = FALSE)

fixture <- list(
  sample_flow = data.frame(
    step = c("CLS-eligible 1940 men", "Valid 1940 schooling",
             "Valid grade and flagged 1950 link", "Unique 1940-1950 pairs",
             "Pairs with observed 1950 occupation", "1950 scientist occupations"),
    n = c(100, 95, 80, 80, 80, 2)),
  linkage_audit = data.frame(
    matched_rows = 80, matched_people = 80, observed_occupation = 80,
    scientists = 2, same_birthplace = 75, male_in_1950 = 79,
    white_in_1950 = 78, exact_ten_year_age_change = 65,
    age_change_9_to_11 = 72, scientist_code_mismatches = 0),
  law_bin_counts = data.frame(
    law_bin = c("0", "1_5", "6", "7", "8", "9plus"),
    men = c(10, 10, 15, 15, 20, 10),
    scientists = c(0, 0, 1, 0, 1, 0))
)

fixture$main_cells <- do.call(rbind, lapply(seq_len(nrow(fixture$law_bin_counts)), function(i) {
  bin <- fixture$law_bin_counts[i, ]
  schooling <- c(0L, 5L, 12L, 16L, 17L, 8L)[[i]]
  rows <- data.frame(bpl = 1L, yob = 1885L, region = 1L,
                     law_bin = bin$law_bin, school_years = schooling,
                     scientist = 0L, n = bin$men - bin$scientists)
  if (bin$scientists > 0L) {
    rows <- rbind(rows, transform(rows, scientist = 1L, n = bin$scientists))
  }
  rows
}))

coef_grid <- expand.grid(sample = "linked_1940_grade", cluster = clusters,
                         term = law_terms, stringsAsFactors = FALSE)
fixture$first_stage <- transform(coef_grid,
                                 estimate = match(term, law_terms) / 100,
                                 se = ifelse(cluster == "state", 0.02, 0.01),
                                 p_value = ifelse(cluster == "state", 0.9,
                                                  c(0.009, 0.049, 0.099, 0.10, 0.20)[match(term, law_terms)]))
fixture$reduced_form <- transform(coef_grid,
                                  estimate = match(term, law_terms) / 10000,
                                  se = ifelse(cluster == "state", 0.0002, 0.0001),
                                  p_value = ifelse(cluster == "state", 0.9,
                                                   c(0.20, 0.10, 0.099, 0.049, 0.009)[match(term, law_terms)]))
estimate_grid <- expand.grid(sample = samples, model = c("ols", "iv_2sls"),
                             cluster = clusters, stringsAsFactors = FALSE)
estimate_grid$term <- ifelse(estimate_grid$model == "ols", "school_years",
                           "fit_school_years")
estimate_grid$estimate <- ifelse(estimate_grid$model == "ols", 0.0005, 0.001)
estimate_grid$se <- ifelse(estimate_grid$cluster == "state", 0.0002, 0.0001)
estimate_grid$p_value <- 0.02
estimate_grid$effect_per_100k_per_school_year <- 1e5 * estimate_grid$estimate
estimate_grid$se_per_100k <- 1e5 * estimate_grid$se
fixture$estimates <- estimate_grid

diagnostic_grid <- expand.grid(sample = samples, cluster = clusters,
                               stringsAsFactors = FALSE)
diagnostic_grid$observations <- ifelse(diagnostic_grid$sample == samples[[1L]],
                                       80, 60)
diagnostic_grid$scientist_count <- 2
diagnostic_grid$scientist_rate_per_100k <- 1e5 * 2 / diagnostic_grid$observations
diagnostic_grid$first_stage_joint_F <- ifelse(diagnostic_grid$cluster == "state", 3.43, 26.23)
diagnostic_grid$first_stage_p <- 0.01
diagnostic_grid$anderson_rubin_zero_F <- 2.5
diagnostic_grid$anderson_rubin_zero_p <- 0.039
fixture$diagnostics <- diagnostic_grid
fixture$anderson_rubin_intervals <- data.frame(
  sample = "linked_1940_grade", cluster = "state", confidence = 0.95,
  lower = 0.0001, upper = 0.002, lower_per_100k = 10,
  upper_per_100k = 200)

for (name in names(fixture)) {
  utils::write.csv(fixture[[name]], file.path(run_dir, paste0(name, ".csv")),
                   row.names = FALSE)
}
writeLines(c("1940 schooling -> 1950 scientist occupation, CLS law IV",
             "Built: 2026-09-25 22:00:00 UTC",
             "The linked extract is selected; results do not represent the full census."),
           file.path(run_dir, "notes.txt"))

outputs <- export_tables(run_dir, project_dir)
stopifnot(all(file.exists(outputs)))
effects <- readLines(outputs[["effects"]], warn = FALSE)
laws <- readLines(outputs[["laws"]], warn = FALSE)
descriptives <- readLines(outputs[["descriptives"]], warn = FALSE)
document <- readLines(outputs[["document"]], warn = FALSE)
stopifnot(any(grepl("100.0", effects, fixed = TRUE)),
          any(grepl("[10.0, 200.0]", effects, fixed = TRUE)),
          any(grepl("26.23 / 3.43", effects, fixed = TRUE)),
          any(grepl("First-stage", effects, fixed = TRUE)),
          any(grepl("Per 100{,}000 men", laws, fixed = TRUE)),
          any(grepl("26.23", laws, fixed = TRUE)),
          any(grepl("3.43", laws, fixed = TRUE)),
          any(grepl("1--5 & 0.010\\textsuperscript{***} & 10.0", laws, fixed = TRUE)),
          any(grepl("6 & 0.020\\textsuperscript{**} & 20.0", laws, fixed = TRUE)),
          any(grepl("7 & 0.030\\textsuperscript{*} & 30.0\\textsuperscript{*}", laws, fixed = TRUE)),
          any(grepl("8 & 0.040 & 40.0\\textsuperscript{**}", laws, fixed = TRUE)),
          any(grepl("9 or more & 0.050 & 50.0\\textsuperscript{***}", laws, fixed = TRUE)),
          any(grepl("Stars use birth-state", laws, fixed = TRUE)),
          any(grepl("26.23 with birth-state", document, fixed = TRUE)),
          any(grepl("Analysis run 2026-09-25", document, fixed = TRUE)),
          any(grepl("\\usepackage[a4paper,margin=1in]{geometry}", document, fixed = TRUE)),
          any(document == "\\usepackage{amsmath}"),
          any(grepl("\\doublespacing", document, fixed = TRUE)),
          any(grepl("\\citet{clay2021}", document, fixed = TRUE)),
          sum(document == "\\section*{Empirical Methodology}") == 1L,
          any(grepl("S_{isc}=\\alpha_s^{S}+\\lambda_{r(s)c}^{S}+\\sum_{b\\in\\mathcal{B}}",
                    document, fixed = TRUE)),
          any(grepl("Y_{isc}=\\alpha_s^{Y}+\\lambda_{r(s)c}^{Y}+\\beta\\widehat{S}_{isc}",
                    document, fixed = TRUE)),
          which(document == "\\section*{Empirical Methodology}") <
            which(document == "\\input{tables/mandatory_schooling_scientist_iv_sample.tex}"),
          any(grepl("\\usepackage[authoryear,round]{natbib}", document, fixed = TRUE)),
          any(grepl("\\usepackage[colorlinks=true,citecolor=blue,linkcolor=black,urlcolor=blue]{hyperref}",
                    document, fixed = TRUE)),
          any(grepl("\\bibliography{references}", document, fixed = TRUE)),
          sum(document == "\\bibliographystyle{plainnat}") == 1L,
          document[[which(document == "\\bibliographystyle{plainnat}") - 2L]] == "\\clearpage",
          any(grepl("0 required years & 1+ required years", descriptives, fixed = TRUE)),
          any(grepl("Men & 80 & 10 & 70", descriptives, fixed = TRUE)),
          any(grepl("Completed schooling, mean (SD) & 11.12 (5.92)",
                    descriptives, fixed = TRUE)),
          any(grepl("High school or more (12+ years) & 62.50\\% & 0.00\\% & 71.43\\%",
                    descriptives, fixed = TRUE)),
          any(grepl("BA-equivalent schooling (16+ years) & 43.75\\%",
                    descriptives, fixed = TRUE)),
          any(grepl("Scientist occupations in 1950 & 2 & 0 & 2",
                    descriptives, fixed = TRUE)),
          sum(document == "\\input{tables/mandatory_schooling_scientist_iv_descriptives.tex}") == 1L)

# Re-exporting preserves narrative added to the standalone note.
manual_line <- "% Coauthor narrative must survive table refresh."
document <- append(document, manual_line, after = 3L)
document[document == "\\input{tables/mandatory_schooling_scientist_iv_sample.tex}"] <-
  "%\\input{tables/mandatory_schooling_scientist_iv_sample.tex}"
writeLines(document, outputs[["document"]])
refreshed <- export_tables(run_dir, project_dir)
document <- readLines(outputs[["document"]], warn = FALSE)
stopifnot(sum(document == manual_line) == 1L,
          sum(document == "%\\input{tables/mandatory_schooling_scientist_iv_sample.tex}") == 1L,
          sum(document == "\\input{tables/mandatory_schooling_scientist_iv_descriptives.tex}") == 1L)
manifest <- utils::read.csv(outputs[["manifest"]], stringsAsFactors = FALSE)
stopifnot(nrow(manifest) >= length(outputs),
          all(nzchar(manifest$source_md5)),
          all(file.exists(manifest$source)))

# A failed reconciliation must leave previously generated tables alone.
before <- tools::md5sum(outputs[c("sample", "descriptives", "laws", "effects", "document")])
fixture$law_bin_counts$men[[1L]] <- fixture$law_bin_counts$men[[1L]] + 1L
utils::write.csv(fixture$law_bin_counts,
                 file.path(run_dir, "law_bin_counts.csv"), row.names = FALSE)
failed <- try(export_tables(run_dir, project_dir), silent = TRUE)
stopifnot(inherits(failed, "try-error"),
          identical(unname(before),
                    unname(tools::md5sum(outputs[c("sample", "descriptives", "laws", "effects", "document")]))))
message("scientist IV LaTeX exporter fixture passed")
