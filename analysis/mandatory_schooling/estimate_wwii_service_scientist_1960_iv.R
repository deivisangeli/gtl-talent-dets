# Individual-level WWII service IV; run from any working directory.
# Source and DDI are read only. Only this analysis's result directory is written.
suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(data.table)
  library(fixest); library(xml2); library(ggplot2)
})

ws_scientist_codes <- c(7L, 61L, 62L, 63L, 67L, 68L, 69L)
ws_scientist_labels <- c("Chemists", "Agricultural scientists", "Biological scientists",
  "Geologists and geophysicists", "Mathematicians", "Physicists", "Misc. natural scientists")
ws_ssc <- function() fixest::ssc(K.adj = TRUE, K.fixef = "nonnested",
  G.adj = TRUE, G.df = "min", t.df = "min")
ws_fail <- function(condition, message) if (isTRUE(condition)) stop(message, call. = FALSE)
ws_say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

ws_repo_root <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  root <- if (length(arg) && sub("^--file=", "", arg[1]) != "-") dirname(normalizePath(sub("^--file=", "", arg[1]),
    winslash = "/")) else Sys.getenv("GTL_REPO", unset = getwd())
  while (!file.exists(file.path(root, "paths.R"))) {
    parent <- dirname(root)
    ws_fail(identical(parent, root), "Could not locate paths.R")
    root <- parent
  }
  normalizePath(root, winslash = "/")
}

ws_paths <- function() {
  repo <- ws_repo_root()
  env <- new.env(parent = baseenv())
  sys.source(file.path(repo, "paths.R"), envir = env)
  candidates <- c(Sys.getenv("TALENT_DETS_DATA_DIR"), env$TALENT_DETS_DATA_DIR,
    Sys.getenv("DET_DIR"), file.path("C:/Users", Sys.info()[["user"]],
      "Globtalent Dropbox/gtl_talent_dets"))
  candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]
  ws_fail(!length(candidates), "Set TALENT_DETS_DATA_DIR to the Dropbox project root")
  root <- normalizePath(candidates[1], winslash = "/")
  raw <- c(file.path(root, "Data/raw/mandatory_schooling"),
    file.path(root, "raw/mandatory_schooling"))
  hit <- raw[file.exists(file.path(raw, "usa_00007.csv.gz"))]
  default <- if (length(hit)) hit[1] else raw[1]
  list(repo = repo, root = root,
    csv = Sys.getenv("IPUMS_1960_SOURCE", file.path(default, "usa_00007.csv.gz")),
    ddi = Sys.getenv("IPUMS_1960_DDI", file.path(default, "usa_00007.xml")),
    out = Sys.getenv("WWII_1960_RESULTS_DIR", file.path(root,
      "results/mandatory_schooling/wwii_service_scientist_1960_iv")))
}

ws_ddi <- function(path, header) {
  doc <- xml2::read_xml(path)
  nodes <- xml2::xml_find_all(doc, ".//*[local-name()='dataDscr']/*[local-name()='var']")
  text <- function(node, child) xml2::xml_text(xml2::xml_find_first(node,
    paste0("./*[local-name()='", child, "']")))
  fields <- rbindlist(lapply(nodes, function(node) data.table(
    name = xml2::xml_attr(node, "name"), label = text(node, "labl"),
    decimals = as.integer(xml2::xml_attr(node, "dcml")), description = text(node, "txt"))))
  labels <- rbindlist(lapply(nodes, function(node) {
    categories <- xml2::xml_find_all(node, "./*[local-name()='catgry']")
    rbindlist(lapply(categories, function(cat) data.table(
      name = xml2::xml_attr(node, "name"), code = as.numeric(text(cat, "catValu")),
      label = text(cat, "labl"))))
  }))
  ws_fail(!setequal(header, fields$name) || anyDuplicated(header) > 0,
    "CSV header differs from DDI variables")
  actual <- labels[name == "OCC1950" & code %in% ws_scientist_codes]
  actual <- actual[match(ws_scientist_codes, code)]
  ws_fail(!identical(actual$label, ws_scientist_labels), "DDI scientist labels differ")
  checks <- list(SEX = c(1, 2), BIRTHQTR = 1:4, VETWWII = c(1, 2),
    VETKOREA = c(1, 2))
  for (field in names(checks)) ws_fail(!all(checks[[field]] %in% labels[name == field, code]),
    paste("Required DDI codes absent:", field))
  ws_fail(fields[name == "PERWT", decimals] != 2L, "Unexpected DDI PERWT format")
  list(fields = fields, labels = labels)
}

ws_derive <- function(d) {
  d <- as.data.table(copy(d))
  d[, us_born := !is.na(BPL) & (BPL %between% c(1, 56) | BPL %in% c(90, 99))]
  d[, active_military := OCC1950 %in% 595L | EMPSTATD %in% 13:15 | CLASSWKRD %in% 26L]
  d[, korean_only := !is.na(VETKOREA) & VETKOREA == 2L & VETWWII %in% 1L]
  d[, scientist := fifelse(is.na(OCC1950), NA_integer_, as.integer(OCC1950 %in% ws_scientist_codes))]
  d[, veteran := fifelse(VETWWII %in% 1:2, as.integer(VETWWII == 2L), NA_integer_)]
  d[, korea := fifelse(VETKOREA %in% 1:2, as.integer(VETKOREA == 2L), NA_integer_)]
  d[, x := 4L * (BIRTHYR - 1927L) + BIRTHQTR - 4L]
  d[, z := as.integer(x <= 0)]
  d[, `:=`(left_trend = x * z, right_trend = x * (1 - z),
    cohort = paste0(BIRTHYR, "Q", BIRTHQTR), weight = as.numeric(PERWT))]
  d[, eligible := YEAR %in% 1960 & SEX %in% 1L & !active_military &
    BIRTHYR %between% c(1923, 1930) & BIRTHQTR %in% 1:4 &
    VETWWII %in% 1:2 & is.finite(weight) & weight > 0 & !is.na(scientist)]
  d
}

ws_specs <- function() data.table(
  spec = c("primary", "unrestricted_us_born", "window_1923_1930", "window_1926_1929",
    "omit_1928", "all_birthplaces", "known_occupation", "unflagged_age",
    "quarter_effects", "birthplace_race_effects"),
  lo = c(1925L, 1925L, 1923L, 1926L, rep(1925L, 6)),
  hi = c(1930L, 1930L, 1930L, 1929L, rep(1930L, 6)),
  us_only = c(rep(TRUE, 5), FALSE, rep(TRUE, 4)),
  exclude_korean_only = c(TRUE, FALSE, rep(TRUE, 8)),
  omit_1928 = c(rep(FALSE, 4), TRUE, rep(FALSE, 5)),
  known_occupation = c(rep(FALSE, 6), TRUE, rep(FALSE, 3)),
  unflagged_age = c(rep(FALSE, 7), TRUE, FALSE, FALSE),
  controls = c(rep("none", 8), "quarter", "birthplace_race"))

ws_select <- function(d, spec) {
  keep <- d$eligible & d$BIRTHYR >= spec$lo & d$BIRTHYR <= spec$hi
  if (spec$us_only) keep <- keep & d$us_born
  if (spec$exclude_korean_only) keep <- keep & !d$korean_only
  if (spec$omit_1928) keep <- keep & d$BIRTHYR != 1928L
  if (spec$known_occupation) keep <- keep & !d$OCC1950 %in% c(979L, 997L)
  if (spec$unflagged_age) keep <- keep & d$QAGE %in% 0L
  if (spec$controls == "birthplace_race") keep <- keep & !is.na(d$BPL) & !is.na(d$RACE)
  d[which(keep)]
}

ws_fit <- function(d, controls = "none", outcome = "scientist", iv = FALSE) {
  rhs <- "left_trend + right_trend"
  if (controls == "quarter") rhs <- paste(rhs, "+ factor(BIRTHQTR)")
  fe <- if (controls == "birthplace_race") " | BPL + RACE" else ""
  if (iv) {
    formula <- paste0(outcome, " ~ ", rhs,
      if (nzchar(fe)) fe else " | 0", " | veteran ~ z")
  } else formula <- paste0(outcome, " ~ z + ", rhs, fe)
  model <- fixest::feols(as.formula(formula), data = d, weights = ~weight,
    cluster = ~cohort, ssc = ws_ssc(), notes = FALSE)
  term <- if (iv) "fit_veteran" else "z"
  ws_fail(nobs(model) != nrow(d) || !term %in% names(coef(model)) ||
    !is.finite(coef(model)[term]) || !is.finite(se(model)[term]),
    "Dropped observations, unidentified coefficient, or unavailable inference")
  ws_fail(!is.null(model$collin.var), paste("Collinear regressors:", paste(model$collin.var, collapse = ",")))
  model
}

ws_coef <- function(model, term, spec, model_name, d) {
  tab <- coeftable(model)[term, ]
  df <- uniqueN(d$cohort) - 1L
  critical <- qt(.975, df)
  row <- data.table(spec = spec, model = model_name, term = term,
    estimate = unname(tab[1]), std_error = unname(tab[2]),
    p_value = 2 * pt(-abs(tab[1] / tab[2]), df), df = df,
    lower = unname(tab[1] - critical * tab[2]), upper = unname(tab[1] + critical * tab[2]),
    observations = nrow(d), cohort_clusters = df + 1L,
    scientist_count = sum(d$scientist), weighted_population = sum(d$weight))
  for (field in c("estimate", "std_error", "lower", "upper")) {
    row[, (paste0(field, "_pp")) := 100 * get(field)]
    row[, (paste0(field, "_per_100k")) := 1e5 * get(field)]
  }
  row
}

# Solve A*b^2+B*b+C <= 0 over the entire real line, including disconnected sets.
ws_quadratic_set <- function(A, B, C, tolerance = 1e-12) {
  scale <- max(abs(c(A, B, C)))
  empty <- data.table(lower = numeric(), upper = numeric())
  answer <- function(type, lo = numeric(), hi = numeric())
    list(type = type, intervals = if (length(lo)) data.table(lower = lo, upper = hi) else empty)
  if (scale == 0) return(answer("all_real", -Inf, Inf))
  A <- A / scale; B <- B / scale; C <- C / scale
  if (abs(A) <= tolerance) {
    if (abs(B) <= tolerance) return(if (C <= tolerance) answer("all_real", -Inf, Inf) else answer("empty"))
    return(if (B > 0) answer("half_line", -Inf, -C / B) else answer("half_line", -C / B, Inf))
  }
  disc <- B^2 - 4 * A * C
  if (disc < -tolerance) return(if (A < 0) answer("all_real", -Inf, Inf) else answer("empty"))
  if (abs(disc) <= tolerance) return(if (A < 0) answer("all_real", -Inf, Inf) else
    answer("point", -B / (2 * A), -B / (2 * A)))
  q <- -.5 * (B + if (B >= 0) sqrt(disc) else -sqrt(disc))
  roots <- sort(c(q / A, C / q))
  if (A > 0) answer("bounded", roots[1], roots[2]) else
    answer("disjoint_unbounded", c(-Inf, roots[2]), c(roots[1], Inf))
}

ws_ar <- function(d, first, reduced) {
  both <- copy(d)
  both[, joint_outcome := scientist + veteran]
  sum_model <- ws_fit(both, outcome = "joint_outcome")
  ry <- unname(coef(reduced)["z"]); rv <- unname(coef(first)["z"])
  vy <- unname(vcov(reduced)["z", "z"]); vv <- unname(vcov(first)["z", "z"])
  cyv <- (unname(vcov(sum_model)["z", "z"]) - vy - vv) / 2
  ws_fail(abs(cyv) > sqrt(vy * vv) + 1e-10, "Invalid joint reduced-form covariance")
  df <- uniqueN(d$cohort) - 1L; critical <- qt(.975, df)^2
  A <- rv^2 - critical * vv; B <- -2 * ry * rv + 2 * critical * cyv
  C <- ry^2 - critical * vy
  set <- ws_quadratic_set(A, B, C)
  components <- data.table(reduced_jump = ry, first_jump = rv, variance_y = vy,
    variance_v = vv, covariance_y_v = cyv, critical_F = critical, df = df,
    quadratic_A = A, quadratic_B = B, quadratic_C = C, set_type = set$type)
  list(components = components, intervals = set$intervals, type = set$type)
}

ws_ar_p <- function(beta, components) {
  variance <- components$variance_y - 2 * beta * components$covariance_y_v + beta^2 * components$variance_v
  numerator <- (components$reduced_jump - beta * components$first_jump)^2
  ws_fail(any(variance < -1e-10), "Negative AR variance")
  statistic <- ifelse(variance > 0, numerator / variance, ifelse(numerator == 0, 0, Inf))
  pf(statistic, 1, components$df, lower.tail = FALSE)
}

ws_cohorts <- function(d, sample_name) d[, .(
  observations = .N, weighted_population = sum(weight),
  scientist_count = sum(scientist), wwii_count = sum(veteran),
  korean_count = sum(korea, na.rm = TRUE), korea_missing = sum(is.na(korea)),
  korean_only_count = sum(korean_only), dual_service_count = sum(veteran == 1 & korea == 1, na.rm = TRUE),
  unknown_occupation_count = sum(OCC1950 %in% c(979, 997)),
  scientist_rate = weighted.mean(scientist, weight), wwii_share = weighted.mean(veteran, weight),
  korean_share = weighted.mean(korea, weight, na.rm = TRUE)),
  by = .(BIRTHYR, BIRTHQTR, cohort, x, z, left_trend, right_trend)][,
    sample := sample_name][order(x)]

ws_plot <- function(cohorts, fitted, out) {
  # Separate construction keeps observed means and fitted values explicit.
  points <- rbindlist(lapply(c("wwii_share", "scientist_rate"), function(field) {
    p <- copy(cohorts[sample %in% c("primary", "unrestricted_us_born")])
    p[, `:=`(outcome = if (field == "wwii_share") "WWII service (%)" else "Scientist occupation per 100,000 men",
      value = get(field) * if (field == "wwii_share") 100 else 1e5)]; p
  }))
  lines <- copy(fitted)
  lines[, `:=`(outcome = ifelse(model == "first_stage", "WWII service (%)", "Scientist occupation per 100,000 men"),
    value = fitted * ifelse(model == "first_stage", 100, 1e5))]
  plot <- ggplot(points, aes(x, value)) + geom_vline(xintercept = .5, linetype = 2, colour = "grey45") +
    geom_point(size = 1.5) + geom_line(data = lines, aes(group = z), colour = "#176b91", linewidth = .8) +
    facet_grid(outcome ~ sample, scales = "free_y", labeller = labeller(
      sample = c(primary = "Korean-only veterans excluded",
        unrestricted_us_born = "Unrestricted US-born sample"),
      outcome = c("WWII service (%)" = "WWII service (%)",
        "Scientist occupation per 100,000 men" = "Scientists per 100,000 men"))) +
    scale_x_continuous(breaks = c(-11, -7, -3, 1, 5, 9), labels = paste0(1925:1930, "Q1")) +
    labs(x = "Birth cohort", y = NULL, title = "WWII service and scientist occupation in the 1960 census",
      caption = "Points: person-weighted cohort means. Lines: separate linear trends.\nDashed boundary: 1927Q4 / 1928Q1; the reported jump is evaluated at x = 0.") + theme_minimal(base_size = 11)
  ggplot2::ggsave(file.path(out, "figures/cohort_cutoff.png"), plot, width = 11, height = 7, dpi = 180)
  ggplot2::ggsave(file.path(out, "figures/cohort_cutoff.pdf"), plot, width = 11, height = 7)
  diagnostics <- copy(cohorts[sample == "unrestricted_us_born"])
  diagnostics[, retention := cohorts[sample == "primary", observations] / observations]
  diagnostics <- melt(diagnostics, id.vars = c("x", "cohort"),
    measure.vars = c("korean_share", "retention"), variable.name = "measure", value.name = "share")
  diagnostic_plot <- ggplot(diagnostics, aes(x, 100 * share)) + geom_line(colour = "#176b91") +
    geom_point(size = 1.5) + geom_vline(xintercept = .5, linetype = 2, colour = "grey45") +
    facet_wrap(~measure, scales = "free_y", labeller = labeller(measure = c(
      korean_share = "Korean War service", retention = "Sample retained after exclusion"))) +
    theme_minimal(base_size = 11) +
    labs(x = "Birth-quarter index (1927Q4 = 0)", y = "%", title = "Korean service and retention after the Korean-only exclusion")
  ggplot2::ggsave(file.path(out, "figures/korean_service_and_retention.png"), diagnostic_plot, width = 10, height = 4, dpi = 180)
  ggplot2::ggsave(file.path(out, "figures/korean_service_and_retention.pdf"), diagnostic_plot, width = 10, height = 4)
}

ws_main <- function() {
  paths <- ws_paths()
  ws_fail(!file.exists(paths$csv) || !file.exists(paths$ddi), "Census CSV or DDI missing")
  input <- c(csv = paths$csv, ddi = paths$ddi)
  hash_before <- tools::md5sum(input)
  ws_fail(dir.exists(paths$out), paste("Results already exist; choose a new WWII_1960_RESULTS_DIR:", paths$out))
  out <- paste0(paths$out, ".staging-", Sys.getpid())
  ws_fail(dir.exists(out), paste("Staging already exists:", out))
  dir.create(file.path(out, "models"), recursive = TRUE)
  dir.create(file.path(out, "figures"))
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dbExecute(con, "SET threads=4"); dbExecute(con, "SET memory_limit='4GB'")
  csv <- paste0("read_csv_auto(", dbQuoteString(con, gsub("\\\\", "/", paths$csv)), ",sample_size=30000)")
  schema <- dbGetQuery(con, paste0("DESCRIBE SELECT * FROM ", csv))
  ddi <- ws_ddi(paths$ddi, schema$column_name)
  fwrite(ddi$fields, file.path(out, "field_dictionary.csv"))
  fwrite(ddi$labels, file.path(out, "ddi_value_labels.csv"))
  ws_say("Auditing complete extract and respondent keys")
  full_audit <- as.data.table(dbGetQuery(con, paste0("SELECT YEAR,SAMPLE,COUNT(*) AS observations,",
    "COUNT(*)-COUNT(DISTINCT (SAMPLE,SERIAL,PERNUM)) AS duplicate_or_missing_keys,",
    "COUNT(*) FILTER(WHERE SAMPLE IS NULL OR SERIAL IS NULL OR PERNUM IS NULL) AS missing_keys,",
    "MIN(PERWT) AS min_weight,MAX(PERWT) AS max_weight FROM ", csv, " GROUP BY YEAR,SAMPLE")))
  ws_fail(nrow(full_audit) != 1 || full_audit$YEAR != 1960 || full_audit$SAMPLE != 196002 ||
    any(full_audit$duplicate_or_missing_keys != 0 | full_audit$missing_keys != 0), "Wrong sample/year or invalid person keys")
  fwrite(full_audit, file.path(out, "extract_audit.csv"))
  ws_say("Reading individual male cohorts 1923-1930")
  fields <- c("YEAR", "SAMPLE", "SERIAL", "PERNUM", "SEX", "BIRTHYR", "BIRTHQTR", "AGE",
    "BPL", "RACE", "PERWT", "OCC1950", "EMPSTATD", "CLASSWKRD", "VETWWII", "VETKOREA", "VETSTAT", "QAGE")
  d <- as.data.table(dbGetQuery(con, paste0("SELECT ", paste(fields, collapse = ","),
    " FROM ", csv, " WHERE SEX=1 AND BIRTHYR BETWEEN 1923 AND 1930")))
  for (field in setdiff(fields, "PERWT")) set(d, j = field, value = as.integer(d[[field]]))
  code_audit <- rbindlist(lapply(intersect(fields, ddi$labels$name), function(field) {
    counts <- d[, .(observations = .N), by = c(field)]
    setnames(counts, field, "code"); counts[, name := field]
    merge(counts, ddi$labels[name == field], by = c("name", "code"), all.x = TRUE)
  }))
  ws_fail(any(is.na(code_audit$label) & !is.na(code_audit$code)), "Observed code missing from DDI")
  fwrite(code_audit, file.path(out, "observed_code_audit.csv"))
  d <- ws_derive(d)
  age_audit <- d[, .(observations = .N), by = .(BIRTHQTR, QAGE,
    difference = BIRTHYR - (YEAR - AGE - as.integer(BIRTHQTR %in% 2:4)))]
  fwrite(age_audit, file.path(out, "birth_year_age_audit.csv"))
  stages <- list(male_cohorts_1923_1930 = rep(TRUE, nrow(d)),
    baseline_1925_1930 = d$BIRTHYR %between% c(1925, 1930))
  stages$us_born <- stages$baseline_1925_1930 & d$us_born
  stages$civilian <- stages$us_born & !d$active_military
  stages$valid_quarter <- stages$civilian & d$BIRTHQTR %in% 1:4
  stages$valid_service <- stages$valid_quarter & d$VETWWII %in% 1:2
  stages$valid_weight <- stages$valid_service & is.finite(d$weight) & d$weight > 0
  stages$observed_occupation <- stages$valid_weight & !is.na(d$scientist)
  stages$exclude_korean_only <- stages$observed_occupation & !d$korean_only
  exclusions <- rbindlist(lapply(names(stages), function(stage) {
    rows <- d[which(stages[[stage]])]
    data.table(stage = stage, retained = nrow(rows), weighted_population = sum(rows$weight),
      scientist_count = sum(rows$scientist, na.rm = TRUE))
  }))
  exclusions[, excluded_at_stage := shift(retained) - retained]
  fwrite(exclusions, file.path(out, "sample_exclusions.csv"))
  specs <- ws_specs(); fwrite(specs, file.path(out, "specifications.csv"))
  primary <- ws_select(d, specs[spec == "primary"])
  unrestricted <- ws_select(d, specs[spec == "unrestricted_us_born"])
  ws_fail(nrow(primary) != 245883 || sum(primary$scientist) != 1189 ||
    nrow(unrestricted) != 321247 || sum(unrestricted$scientist) != 1668 ||
    uniqueN(primary$cohort) != 24 || nrow(unrestricted) - nrow(primary) != 75364,
    "Production sample differs from agreed counts")
  ws_fail(any(primary$korean_only) || sum(primary$veteran == 1 & primary$korea == 1, na.rm = TRUE) != 15211,
    "Korean exclusion or dual-service retention differs")
  fwrite(d[eligible & us_born & BIRTHYR %between% c(1925, 1930),
    .(observations = .N, scientists = sum(scientist), weighted_population = sum(weight)),
    by = .(VETSTAT, VETWWII, VETKOREA)], file.path(out, "service_groups_before_exclusion.csv"))
  fwrite(primary[, .(observations = .N, scientists = sum(scientist)), by = .(OCC1950, EMPSTATD)],
    file.path(out, "primary_occupation_audit.csv"))
  estimates <- list(); diagnostics <- list(); summaries <- list(); cohorts <- list(); fitted <- list()
  primary_models <- NULL
  for (i in seq_len(nrow(specs))) {
    spec <- specs[i]; rows <- ws_select(d, spec)
    ws_say("Estimating ", spec$spec, ": ", nrow(rows), " individuals")
    first <- ws_fit(rows, spec$controls, "veteran")
    reduced <- ws_fit(rows, spec$controls)
    iv <- ws_fit(rows, spec$controls, iv = TRUE)
    models <- list(first_stage = first, reduced_form = reduced, iv_2sls = iv)
    saveRDS(models, file.path(out, "models", paste0(spec$spec, ".rds")))
    coefficient <- unname(coef(iv)["fit_veteran"])
    ratio <- unname(coef(reduced)["z"] / coef(first)["z"])
    ws_fail(abs(coefficient - ratio) > 1e-8 * max(1, abs(coefficient)), "2SLS/Wald ratio differs")
    estimates[[i]] <- rbindlist(list(ws_coef(first, "z", spec$spec, "first_stage", rows),
      ws_coef(reduced, "z", spec$spec, "reduced_form", rows),
      ws_coef(iv, "fit_veteran", spec$spec, "iv_2sls", rows)))
    F <- (coef(first)["z"] / se(first)["z"])^2
    diagnostics[[i]] <- data.table(spec = spec$spec, first_stage_F = unname(F),
      first_stage_p = pf(F, 1, uniqueN(rows$cohort) - 1, lower.tail = FALSE),
      wald_ratio = ratio, iv_minus_wald = coefficient - ratio)
    summaries[[i]] <- rows[, .(spec = spec$spec, observations = .N, scientist_count = sum(scientist),
      veteran_count = sum(veteran), weighted_population = sum(weight), cohort_clusters = uniqueN(cohort),
      min_weight = min(weight), max_weight = max(weight), flagged_age = sum(QAGE != 0, na.rm = TRUE),
      unknown_occupation = sum(OCC1950 %in% c(979, 997)), scientist_rate_per_100k = 1e5 * weighted.mean(scientist, weight))]
    summary <- ws_cohorts(rows, spec$spec); cohorts[[i]] <- summary
    if (spec$spec %in% c("primary", "unrestricted_us_born")) {
      fitted[[spec$spec]] <- rbindlist(lapply(c("first_stage", "reduced_form"), function(model) {
        data.table(sample = spec$spec, model = model, cohort = summary$cohort, x = summary$x,
          z = summary$z, fitted = as.numeric(predict(models[[model]], newdata = summary)))
      }))
    }
    if (spec$spec == "primary") primary_models <- models
  }
  estimates <- rbindlist(estimates); diagnostics <- rbindlist(diagnostics)
  summaries <- rbindlist(summaries); cohorts <- rbindlist(cohorts); fitted <- rbindlist(fitted)
  fwrite(estimates, file.path(out, "estimates.csv"))
  fwrite(diagnostics, file.path(out, "model_diagnostics.csv"))
  fwrite(summaries, file.path(out, "sample_summaries.csv"))
  fwrite(cohorts, file.path(out, "cohort_summaries.csv"))
  fwrite(fitted, file.path(out, "fitted_cohort_trends.csv"))
  ar <- ws_ar(primary, primary_models$first_stage, primary_models$reduced_form)
  fwrite(ar$components, file.path(out, "anderson_rubin_components.csv"))
  intervals <- copy(ar$intervals)
  intervals[, `:=`(lower_pp = 100 * lower, upper_pp = 100 * upper,
    lower_per_100k = 1e5 * lower, upper_per_100k = 1e5 * upper)]
  fwrite(intervals, file.path(out, "anderson_rubin_intervals.csv"))
  grid <- data.table(beta = seq(-.05, .05, length.out = 101))
  grid[, p_value := ws_ar_p(beta, ar$components)]
  grid[, accepted := p_value >= .05]
  fwrite(grid, file.path(out, "anderson_rubin_diagnostic_grid.csv"))
  korea <- ws_fit(unrestricted, outcome = "korea")
  saveRDS(korea, file.path(out, "models/korean_service_unrestricted.rds"))
  fwrite(ws_coef(korea, "z", "unrestricted_us_born", "korean_service", unrestricted),
    file.path(out, "korean_service_diagnostic.csv"))
  ws_say("Rendering figures and report")
  ws_plot(cohorts, fitted, out)
  main <- estimates[spec == "primary" & model == "iv_2sls"]
  benchmark <- estimates[spec == "unrestricted_us_born" & model == "iv_2sls"]
  fmt <- function(row) sprintf("%.3f pp (95%% CI %.3f to %.3f; p=%.4f)",
    row$estimate_pp, row$lower_pp, row$upper_pp, row$p_value)
  table <- c("| Sample | N | Scientists | First-stage jump (pp) | First-stage F | Reduced-form jump (pp) | IV effect (pp) | 95% IV interval (pp) |",
    "|---|---:|---:|---:|---:|---:|---:|---|")
  for (spec_name in specs$spec) {
    fs <- estimates[spec == spec_name & model == "first_stage"]
    rf <- estimates[spec == spec_name & model == "reduced_form"]
    iv <- estimates[spec == spec_name & model == "iv_2sls"]
    diagnostic <- diagnostics[spec == spec_name]
    table <- c(table, sprintf("| %s | %d | %d | %.3f | %.2f | %.4f | %.3f | [%.3f, %.3f] |",
      spec_name, iv$observations, iv$scientist_count, fs$estimate_pp, diagnostic$first_stage_F,
      rf$estimate_pp, iv$estimate_pp, iv$lower_pp, iv$upper_pp))
  }
  ar_text <- if (!nrow(intervals)) "Empty confidence set." else paste(
    sprintf("[%s, %s] pp", format(100 * intervals$lower, digits = 5),
      format(100 * intervals$upper, digits = 5)), collapse = " union ")
  note <- c("# WWII service and scientist occupation: 1960 census IV", "",
    paste0("Primary selected-sample IV: **", fmt(main), "**."),
    paste0("Unrestricted US-born benchmark: **", fmt(benchmark), "**."), "",
    "## Sample and measurement", "",
    "US-born civilian men, 1925-1930, valid birth quarter, observed occupation, VETWWII 1/2, positive PERWT.",
    "The Korean-only exclusion removes 75,364 men (479 scientists) from 321,247 men (1,668 scientists).",
    "The primary sample has 245,883 men and 1,189 scientists; 15,211 WWII-plus-Korea veterans remain.",
    "US birthplace codes: 1-56, 90, 99, matching the existing pipeline. Scientist OCC1950 codes: 7,61,62,63,67,68,69.",
    "Other recorded occupation codes, including unknown 997 and N/A 999, receive zero to reproduce the existing IV classifier. Blank occupation is missing; unknown codes are excluded in a sensitivity check.",
    "Students, unemployed men, and men outside the labor force remain. Current military personnel are excluded by OCC1950=595, EMPSTATD=13-15, or CLASSWKRD=26.",
    "Birth year uses the supplied quarter-refined IPUMS BIRTHYR, without an additional correction; discrepancies are saved in birth_year_age_audit.csv.",
    "All primary weights are PERWT=20. Weighted and unweighted coefficients coincide; weighted population is 4,917,660.",
    "No Vietnam-era filter is applicable to the 1960 outcome: IPUMS's August 1964-April 1975 era is later, and VETVIETN is unavailable in 1960. Earlier Vietnam service is not identifiable.", "",
    "## Specification and inference", "",
    "x=4*(BIRTHYR-1927)+BIRTHQTR-4; z=1{x<=0}. First stage and reduced form include intercept, z, x*z, x*(1-z). 2SLS instruments veteran with z and retains both trends.",
    "The reported jump is earlier-born minus later-born at x=0; adjacent bins are 0 and 1. The plot boundary is x=0.5; it does not change coefficient centering.",
    "Individual-level fits use PERWT, no baseline additional controls, and clustering by national birth-year x quarter. SSC: K.adj=TRUE, K.fixef=nonnested, G.adj=TRUE, G.df=min, t.df=min; confidence intervals use G-1 degrees of freedom.",
    "There are 24 primary cohort clusters. Conventional clustering does not address national cohort serial correlation or approximation bias from discrete linear trends.",
    paste0("Primary 95% Anderson-Rubin set (", ar$type, "): ", ar_text),
    "AR inversion solves the quadratic acceptance inequality over the whole real line. The saved grid is only a diagnostic and never truncates the confidence set.", "",
    "## Estimates and robustness", "", table, "",
    "Korean service discontinuity in the unrestricted sample is saved separately in korean_service_diagnostic.csv; Korean service and retention are plotted before filtering.", "",
    "## Interpretation", "",
    "The primary restriction conditions on subsequent Korean service, which may itself be affected by the instrument. It changes the comparison population and can compromise a causal IV interpretation. The unrestricted benchmark is essential for evaluating that sensitivity.",
    "A causal WWII-service interpretation requires smooth counterfactual outcomes, exclusion, a relevant first stage, and monotonicity. Effects bundle service with GI Bill and other veteran benefits; they do not isolate the education subsidy.",
    "The outcome is scientist occupation observed in 1960, not first entry, lifetime scientist status, or elite/notable scientist status. Census coverage also conditions on survival and residence in 1960.",
    "The extract does not permit the original college-entry-before-1952 robustness restriction.", "",
    "## Sources and reproducibility", "",
    paste0("CSV: ", paths$csv), paste0("DDI: ", paths$ddi),
    "Design: Lit/Compulsory Attendance/abramitzky_wwii_gi_bill_empirical_strategy.md.",
    "[Birth-year construction](https://usa.ipums.org/usa-action/variables/BIRTHYR); [WWII veteran universe](https://usa.ipums.org/usa-action/variables/VETWWII); [Vietnam-era definition](https://usa.ipums.org/usa-action/variables/VETVIETN).",
    "Input hashes, DDI labels, code and sample audits, fitted models, and session information accompany these results.")
  writeLines(note, file.path(out, "results_note.md"))
  hash_after <- tools::md5sum(input)
  ws_fail(!identical(hash_before, hash_after), "Source inputs changed during analysis")
  manifest <- data.table(input = names(input), path = unname(input), md5 = unname(hash_after),
    bytes = file.info(input)$size)
  fwrite(manifest, file.path(out, "input_manifest.csv"))
  implementation <- file.path(paths$repo, "analysis/mandatory_schooling", c(
    "estimate_wwii_service_scientist_1960_iv.R", "test_wwii_service_scientist_1960_iv.R",
    "wwii_service_scientist_1960_iv.md"))
  fwrite(data.table(path = implementation, md5 = unname(tools::md5sum(implementation))),
    file.path(out, "implementation_manifest.csv"))
  writeLines(c(capture.output(sessionInfo()), paste("Timestamp:",
    format(Sys.time(), tz = "America/Sao_Paulo", usetz = TRUE))), file.path(out, "session_info.txt"))
  ws_fail(!file.rename(out, paths$out), "Could not promote validated staging results")
  ws_say("Completed. Results: ", paths$out)
  print(estimates[spec %in% c("primary", "unrestricted_us_born"),
    .(spec, model, estimate_pp, lower_pp, upper_pp, p_value, observations)])
  invisible(paths$out)
}

if (sys.nframe() == 0L) ws_main()
