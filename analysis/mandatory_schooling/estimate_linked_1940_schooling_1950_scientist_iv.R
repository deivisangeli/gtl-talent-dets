# Schooling and scientist occupation: CLS-law IV, 1940 schooling linked to 1950.
# Run from any working directory:
#   Rscript analysis/mandatory_schooling/estimate_linked_1940_schooling_1950_scientist_iv.R

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(arrow)
  library(fixest)
})

scientist_codes <- c(7L, 61L, 62L, 63L, 67L, 68L, 69L)
law_terms <- paste0("z_", c("1_5", "6", "7", "8", "9plus"))

fail_if <- function(condition, message) if (isTRUE(condition)) stop(message, call. = FALSE)
sql_path <- function(path) gsub("\\\\", "/", path)

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

school_years <- function(higraded) {
  g <- higraded %/% 10L
  ifelse(is.na(higraded) | higraded %in% c(0L, 999L), NA_integer_,
         ifelse(g <= 3L, 0L, ifelse(g <= 20L, g - 3L, 17L)))
}

ba_completion <- function(years) as.integer(years >= 16L)

law_bin <- function(required) {
  ifelse(required == 0L, "0",
         ifelse(required <= 5L, "1_5",
                ifelse(required >= 9L, "9plus", as.character(required))))
}

grade_sql <- function(column) sprintf(
  "CASE WHEN %1$s IN (0, 999) OR %1$s IS NULL THEN NULL
        WHEN FLOOR(%1$s / 10.0) <= 3 THEN 0
        WHEN FLOOR(%1$s / 10.0) <= 20 THEN CAST(FLOOR(%1$s / 10.0) - 3 AS INTEGER)
        ELSE 17 END", column)

law_sql <- function(column) sprintf(
  "CASE WHEN %1$s = 0 THEN '0' WHEN %1$s BETWEEN 1 AND 5 THEN '1_5'
        WHEN %1$s >= 9 THEN '9plus' ELSE CAST(%1$s AS VARCHAR) END", column)

prepare_cells <- function(cells) {
  fail_if(!nrow(cells), "No analysis cells")
  cells$bpl <- as.integer(cells$bpl)
  cells$yob <- as.integer(cells$yob)
  cells$region <- as.integer(cells$region)
  cells$school_years <- as.integer(cells$school_years)
  cells$ba_completed <- ba_completion(cells$school_years)
  cells$scientist <- as.integer(cells$scientist)
  cells$n <- as.numeric(cells$n)
  cells$region_yob <- paste(cells$region, cells$yob, sep = "_")
  cells$state_cohort <- paste(cells$bpl, cells$yob, sep = "_")
  for (term in law_terms) {
    cells[[term]] <- as.integer(cells$law_bin == sub("^z_", "", term))
  }
  fail_if(anyNA(cells[, c("bpl", "yob", "region", "school_years",
                            "scientist", "n", law_terms)]), "Missing analysis value")
  fail_if(any(!cells$scientist %in% 0:1) || any(cells$n <= 0),
          "Invalid scientist flag or cell size")
  cells
}

coef_rows <- function(model, model_name, sample_name, cluster_name, terms = NULL) {
  tab <- summary(model)$coeftable
  selected <- if (is.null(terms)) rownames(tab) else intersect(terms, rownames(tab))
  data.frame(sample = sample_name, model = model_name, cluster = cluster_name,
             term = selected, estimate = tab[selected, 1], se = tab[selected, 2],
             p_value = tab[selected, 4], row.names = NULL)
}

validate_endogenous <- function(d, endogenous) {
  fail_if(length(endogenous) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_]*$", endogenous) ||
            !endogenous %in% names(d), "Invalid endogenous variable")
  fail_if(!is.numeric(d[[endogenous]]) || any(!is.finite(d[[endogenous]])) ||
            length(unique(d[[endogenous]])) < 2L,
          "Endogenous variable must be finite and vary in the sample")
}

estimate_models <- function(cells, sample_name, endogenous = "school_years") {
  d <- prepare_cells(cells)
  validate_endogenous(d, endogenous)
  fail_if(length(unique(d$bpl)) < 2L || length(unique(d$yob)) < 2L,
          "Insufficient state or cohort variation")
  z <- paste(law_terms, collapse = " + ")
  first_formula <- as.formula(paste0(endogenous, " ~ ", z,
                                   " | bpl + region_yob"))
  reduced_formula <- as.formula(paste0("scientist ~ ", z,
                                     " | bpl + region_yob"))
  iv_formula <- as.formula(paste0("scientist ~ 1 | bpl + region_yob | ", endogenous, " ~ ", z))
  ols_formula <- as.formula(paste0("scientist ~ ", endogenous, " | bpl + region_yob"))
  fit <- function(formula, cluster) {
    fixest::feols(formula, data = d, weights = ~n, cluster = cluster,
                  ssc = fixest::ssc(K.adj = FALSE),
                  notes = FALSE, warn = FALSE)
  }
  first_cell <- fit(first_formula, ~state_cohort)
  first_state <- fit(first_formula, ~bpl)
  reduced_cell <- fit(reduced_formula, ~state_cohort)
  reduced_state <- fit(reduced_formula, ~bpl)
  iv_cell <- fit(iv_formula, ~state_cohort)
  iv_state <- fit(iv_formula, ~bpl)
  ols_cell <- fit(ols_formula, ~state_cohort)
  ols_state <- fit(ols_formula, ~bpl)
  first_wald_cell <- fixest::wald(first_cell, keep = "^z_", print = FALSE)
  first_wald_state <- fixest::wald(first_state, keep = "^z_", print = FALSE)
  # At beta=0 the Anderson-Rubin test is the joint test of the instruments
  # in the reduced form. It remains valid when the first stage is weak.
  ar_zero_cell <- fixest::wald(reduced_cell, keep = "^z_", print = FALSE)
  ar_zero_state <- fixest::wald(reduced_state, keep = "^z_", print = FALSE)
  tables <- list(
    first_stage = rbind(coef_rows(first_cell, "first_stage", sample_name, "state_cohort", law_terms),
                        coef_rows(first_state, "first_stage", sample_name, "state", law_terms)),
    reduced_form = rbind(coef_rows(reduced_cell, "reduced_form", sample_name, "state_cohort", law_terms),
                         coef_rows(reduced_state, "reduced_form", sample_name, "state", law_terms)),
    estimates = rbind(coef_rows(iv_cell, "iv_2sls", sample_name, "state_cohort"),
                      coef_rows(iv_state, "iv_2sls", sample_name, "state"),
                      coef_rows(ols_cell, "ols", sample_name, "state_cohort"),
                      coef_rows(ols_state, "ols", sample_name, "state")),
    diagnostics = data.frame(
      sample = sample_name, cluster = c("state_cohort", "state"),
      observations = sum(d$n), state_cohort_cells = length(unique(d$state_cohort)),
      states = length(unique(d$bpl)), scientist_count = sum(d$n * d$scientist),
      scientist_rate_per_100k = 1e5 * sum(d$n * d$scientist) / sum(d$n),
      first_stage_joint_F = c(first_wald_cell$stat, first_wald_state$stat),
      first_stage_p = c(first_wald_cell$p, first_wald_state$p),
      anderson_rubin_zero_F = c(ar_zero_cell$stat, ar_zero_state$stat),
      anderson_rubin_zero_p = c(ar_zero_cell$p, ar_zero_state$p))
  )
  effect_column <- if (endogenous == "school_years") "effect_per_100k_per_school_year" else
    paste0("effect_per_100k_per_", endogenous)
  tables$estimates[[effect_column]] <- 1e5 * tables$estimates$estimate
  tables$estimates$se_per_100k <- 1e5 * tables$estimates$se
  list(tables = tables,
       models = list(first_cell = first_cell, first_state = first_state,
                     reduced_cell = reduced_cell, reduced_state = reduced_state,
                     iv_cell = iv_cell, iv_state = iv_state,
                     ols_cell = ols_cell, ols_state = ols_state))
}

ar_intervals <- function(beta, p, ar_p, sample_name) {
  fail_if(length(beta) != length(p) || length(beta) < 2L ||
            any(!is.finite(c(beta, p))) || any(diff(beta) <= 0) ||
            any(p < 0 | p > 1), "Invalid Anderson-Rubin grid")
  accepted <- p >= 0.05
  starts <- which(diff(c(FALSE, accepted)) == 1L)
  ends <- which(diff(c(accepted, FALSE)) == -1L)
  if (length(starts)) do.call(rbind, lapply(seq_along(starts), function(i) {
    lo <- if (starts[[i]] == 1L) NA_real_ else
      stats::uniroot(function(b) ar_p(b) - 0.05,
                     c(beta[starts[[i]] - 1L], beta[starts[[i]]]), tol = 1e-8)$root
    hi <- if (ends[[i]] == length(beta)) NA_real_ else
      stats::uniroot(function(b) ar_p(b) - 0.05,
                     c(beta[ends[[i]]], beta[ends[[i]] + 1L]), tol = 1e-8)$root
    data.frame(sample = sample_name, cluster = "state", confidence = 0.95,
               lower = lo, upper = hi,
               lower_per_100k = 1e5 * lo, upper_per_100k = 1e5 * hi)
  })) else data.frame(sample = character(), cluster = character(), confidence = numeric(),
                      lower = numeric(), upper = numeric(), lower_per_100k = numeric(),
                      upper_per_100k = numeric())
}

ar_confidence_set <- function(cells, sample_name, limit = 0.05, grid_points = 501L,
                              endogenous = "school_years") {
  d <- prepare_cells(cells)
  validate_endogenous(d, endogenous)
  fail_if(!is.finite(limit) || limit <= 0 || grid_points < 3L,
          "Invalid Anderson-Rubin search limits")
  ar_p <- function(beta) {
    d$y_beta <- d$scientist - beta * d[[endogenous]]
    model <- fixest::feols(y_beta ~ z_1_5 + z_6 + z_7 + z_8 + z_9plus |
                            bpl + region_yob,
                          data = d, weights = ~n, cluster = ~bpl,
                          ssc = fixest::ssc(K.adj = FALSE),
                          notes = FALSE, warn = FALSE)
    fixest::wald(model, keep = "^z_", print = FALSE)$p
  }
  beta <- seq(-limit, limit, length.out = grid_points)
  p <- vapply(beta, ar_p, numeric(1))
  accepted <- p >= 0.05
  intervals <- ar_intervals(beta, p, ar_p, sample_name)
  list(grid = data.frame(sample = sample_name, cluster = "state", beta = beta,
                         p_value = p, accepted = accepted), intervals = intervals)
}

write_table <- function(df, path) utils::write.csv(df, path, row.names = FALSE, na = "")

main <- function() {
  root <- data_root()
  parquet_dir <- Sys.getenv("IPUMS_LINKED_1950_PARQUET_DIR",
                            unset = "D:/ipums_linked_1900_1950")
  parquet_glob <- sql_path(file.path(parquet_dir, "birth_bpl=*", "*.parquet"))
  law_file <- file.path(root, "output/mandatory_schooling/cls",
                        "cls_cohort_requirements_1875_1912.parquet")
  fail_if(!length(Sys.glob(parquet_glob)), paste("Missing Parquet:", parquet_glob))
  fail_if(!file.exists(law_file), paste("Missing CLS requirements:", law_file))
  laws <- as.data.frame(arrow::read_parquet(law_file))
  laws <- laws[laws$yob >= 1885 & laws$yob <= 1912,
               c("bpl", "yob", "region", "ca_years_combined")]
  fail_if(anyDuplicated(laws[c("bpl", "yob")]) > 0L || nrow(laws) != 1372L,
          "Expected 49 states x 28 unique CLS cohorts")
  out_dir <- file.path(root, "results/mandatory_schooling/linked_1940_schooling_1950_scientist_iv")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads = 8")
  DBI::dbExecute(con, "SET memory_limit = '20GB'")
  DBI::dbExecute(con, "PRAGMA disable_progress_bar")
  DBI::dbWriteTable(con, "laws", laws, temporary = TRUE)
  parquet <- as.character(DBI::dbQuoteString(con, parquet_glob))

  message("Selecting 1940 CLS cohorts...")
  DBI::dbExecute(con, sprintf("CREATE TEMP TABLE base40 AS
    SELECT p.HIK, p.BPL AS bpl, 1940 - p.AGE - 1 AS yob,
           p.AGE AS age40, p.HIGRADED, p.LINK1950,
           l.region, l.ca_years_combined AS required_years,
           %s AS school_years, %s AS law_bin
    FROM read_parquet(%s, hive_partitioning=true) p
    JOIN laws l ON p.BPL = l.bpl AND 1940 - p.AGE - 1 = l.yob
    WHERE p.YEAR = 1940 AND p.SEX = 1 AND p.RACE = 1
      AND p.AGE BETWEEN 27 AND 54",
    grade_sql("p.HIGRADED"), law_sql("l.ca_years_combined"), parquet))
  flow40 <- DBI::dbGetQuery(con, "SELECT count(*) AS cls_1940_men,
    count(*) FILTER (WHERE school_years IS NOT NULL) AS valid_schooling,
    count(*) FILTER (WHERE school_years IS NOT NULL AND LINK1950 = 1
                        AND HIK IS NOT NULL AND HIK <> '') AS flagged_1950_link
    FROM base40")
  DBI::dbExecute(con, "CREATE TEMP TABLE eligible40 AS SELECT * FROM base40
    WHERE school_years IS NOT NULL AND LINK1950 = 1
      AND HIK IS NOT NULL AND HIK <> ''")
  duplicate40 <- DBI::dbGetQuery(con, "SELECT count(*) - count(DISTINCT HIK) AS n FROM eligible40")$n
  fail_if(duplicate40 > 0, "Duplicate 1940 HIK in eligible sample")

  message("Joining 1950 occupation by HIK...")
  DBI::dbExecute(con, sprintf("CREATE TEMP TABLE pairs AS
    SELECT a.*, p.BPL AS bpl50, p.AGE AS age50, p.SEX AS sex50,
           p.RACE AS race50, p.HIGRADED AS higraded50, p.OCC1950 AS occ1950,
           CASE WHEN p.scientist_occ1950 THEN 1
                WHEN p.scientist_occ1950 = FALSE THEN 0 ELSE NULL END AS scientist
    FROM eligible40 a
    JOIN read_parquet(%s, hive_partitioning=true) p ON a.HIK = p.HIK
    WHERE p.YEAR = 1950 AND p.LINK1940 = 1", parquet))
  audit <- DBI::dbGetQuery(con, "SELECT count(*) AS matched_rows,
    count(DISTINCT HIK) AS matched_people,
    count(*) FILTER (WHERE scientist IS NOT NULL) AS observed_occupation,
    count(*) FILTER (WHERE scientist = 1) AS scientists,
    count(*) FILTER (WHERE bpl = bpl50) AS same_birthplace,
    count(*) FILTER (WHERE sex50 = 1) AS male_in_1950,
    count(*) FILTER (WHERE race50 = 1) AS white_in_1950,
    count(*) FILTER (WHERE age50 - age40 = 10) AS exact_ten_year_age_change,
    count(*) FILTER (WHERE age50 - age40 BETWEEN 9 AND 11) AS age_change_9_to_11,
    count(*) FILTER (WHERE scientist = 1 AND occ1950 NOT IN (7,61,62,63,67,68,69)
                        OR scientist = 0 AND occ1950 IN (7,61,62,63,67,68,69))
      AS scientist_code_mismatches
    FROM pairs")
  fail_if(audit$matched_rows != audit$matched_people,
          "1950 HIK match is not one-to-one")
  fail_if(audit$matched_rows > flow40$flagged_1950_link,
          "More joined people than flagged 1940 links")
  fail_if(audit$scientist_code_mismatches > 0,
          "Scientist flag disagrees with OCC1950")

  cell_query <- "SELECT bpl, yob, region, law_bin, school_years, scientist,
                       count(*) AS n FROM pairs WHERE scientist IS NOT NULL
                 GROUP BY bpl, yob, region, law_bin, school_years, scientist
                 ORDER BY bpl, yob, school_years, scientist"
  cells <- DBI::dbGetQuery(con, cell_query)
  write_table(cells, file.path(out_dir, "main_cells.csv"))
  law_counts <- DBI::dbGetQuery(con, "SELECT law_bin, count(*) AS men,
    count(*) FILTER (WHERE scientist = 1) AS scientists
    FROM pairs WHERE scientist IS NOT NULL GROUP BY law_bin ORDER BY law_bin")

  message("Estimating main IV and diagnostics...")
  main_fit <- estimate_models(cells, "linked_1940_grade")
  message("Inverting state-clustered Anderson-Rubin tests...")
  main_ar <- ar_confidence_set(cells, "linked_1940_grade")

  quality_cells <- DBI::dbGetQuery(con, paste0(
    "SELECT bpl, yob, region, law_bin, school_years, scientist, count(*) AS n
     FROM pairs WHERE scientist IS NOT NULL AND bpl = bpl50
       AND sex50 = 1 AND race50 = 1 AND age50 - age40 BETWEEN 9 AND 11
     GROUP BY ALL ORDER BY 1, 2, 5, 6"))
  write_table(quality_cells, file.path(out_dir, "quality_link_cells.csv"))
  quality_fit <- estimate_models(quality_cells, "consistent_link_sensitivity")

  message("Estimating 1950 sample-line sensitivity...")
  sensitivity <- DBI::dbGetQuery(con, sprintf("SELECT p.BPL AS bpl,
    1950 - p.AGE - 1 AS yob, l.region,
    %s AS law_bin, %s AS school_years,
    CASE WHEN p.scientist_occ1950 THEN 1
         WHEN p.scientist_occ1950 = FALSE THEN 0 ELSE NULL END AS scientist,
    count(*) AS n
    FROM read_parquet(%s, hive_partitioning=true) p
    JOIN laws l ON p.BPL = l.bpl AND 1950 - p.AGE - 1 = l.yob
    WHERE p.YEAR = 1950 AND p.SEX = 1 AND p.RACE = 1
      AND p.AGE BETWEEN 37 AND 64 AND p.HIGRADED NOT IN (0, 999)
      AND p.scientist_occ1950 IS NOT NULL
    GROUP BY ALL
    ORDER BY 1, 2, 5, 6",
    law_sql("l.ca_years_combined"), grade_sql("p.HIGRADED"), parquet))
  write_table(sensitivity, file.path(out_dir, "sensitivity_1950_grade_cells.csv"))
  sensitivity_fit <- estimate_models(sensitivity, "same_record_1950_grade")

  tables <- lapply(names(main_fit$tables), function(name) {
    rbind(main_fit$tables[[name]], quality_fit$tables[[name]],
          sensitivity_fit$tables[[name]])
  })
  names(tables) <- names(main_fit$tables)
  for (name in names(tables)) write_table(tables[[name]], file.path(out_dir, paste0(name, ".csv")))
  write_table(main_ar$grid, file.path(out_dir, "anderson_rubin_grid.csv"))
  write_table(main_ar$intervals, file.path(out_dir, "anderson_rubin_intervals.csv"))
  flow <- data.frame(step = c("CLS-eligible 1940 men", "Valid 1940 schooling",
                              "Valid grade and flagged 1950 link", "Unique 1940-1950 pairs",
                              "Pairs with observed 1950 occupation", "1950 scientist occupations"),
                     n = c(flow40$cls_1940_men, flow40$valid_schooling,
                           flow40$flagged_1950_link, audit$matched_people,
                           audit$observed_occupation, audit$scientists))
  write_table(flow, file.path(out_dir, "sample_flow.csv"))
  write_table(audit, file.path(out_dir, "linkage_audit.csv"))
  write_table(law_counts, file.path(out_dir, "law_bin_counts.csv"))
  saveRDS(list(main = main_fit$models, consistent_link = quality_fit$models,
               same_record_1950 = sensitivity_fit$models),
          file.path(out_dir, "models.rds"))
  notes <- c(
    "1940 schooling -> 1950 scientist occupation, CLS law IV",
    paste("Built:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Parquet source:", normalizePath(parquet_dir, winslash = "/")),
    paste("Law source:", normalizePath(law_file, winslash = "/")),
    "Main population: 1940 native-born white men, age 27-54, yob=1940-AGE-1,",
    "with reported HIGRADED and unique HIK link to a 1950 census record.",
    "Exposure uses 1940 BPL x yob; 1950 BPL and age disagreements are audited,",
    "not used to reassign law exposure. Outcome is 1950 census-row occupation,",
    "not first entry into science or ever being a scientist.",
    "Scientist codes: OCC1950 007,061,062,063,067,068,069.",
    "All other recorded occupations, including nonoccupational codes, enter as zero.",
    "Schooling uses CLS HIGRADED recode (0-17 years); 000 and 999 are excluded.",
    "Instruments: requirement bins 1-5,6,7,8,9+; zero is reference.",
    "Models include birth-state and CLS-region x birth-year fixed effects.",
    "Grouped cell counts preserve individual-level regression cross-products.",
    "K.adj=FALSE removes the finite-observation correction so clustered SEs",
    "are invariant to collapsing identical regressor/outcome records.",
    "The 1950 grade sensitivity uses the same 1950 census row for schooling and",
    "occupation and is restricted to those asked the sample-line grade question.",
    "That sensitivity is unweighted because this linked extract omits SLWT.",
    "The consistent-link sensitivity requires matching birth state, male and white",
    "codes in 1950, and an age change between nine and eleven years.",
    "Anderson-Rubin zero p is the reduced-form joint Wald test of all instruments.",
    "The main state-clustered 95% Anderson-Rubin confidence set inverts that test",
    "over beta=-0.05..0.05 probability units; blank endpoint means it reached",
    "the grid boundary, so it must not be read as a bounded confidence interval.",
    "The linked extract is selected; results do not represent the full 1950 census.",
    "IV interpretation additionally requires exclusion, monotonicity, and law timing",
    "comparability after the included fixed effects."
  )
  writeLines(notes, file.path(out_dir, "notes.txt"))
  message("Wrote ", out_dir)
  print(tables$estimates)
  print(tables$diagnostics)
}

if (sys.nframe() == 0L) main()
