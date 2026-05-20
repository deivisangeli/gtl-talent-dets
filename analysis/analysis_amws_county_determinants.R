###############################################################################
# Project: GTL Talent Determinants
# Goal: Determinants of county-level AMWS outcomes using XIX controls
###############################################################################

rm(list = ls())

library("tidyverse")
library("broom")
library("sandwich")
library("lmtest")
library("sf")
library("tigris")
library("stargazer")

initial_time <- Sys.time()

###############################################################################
# Paths
###############################################################################

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]

if (length(file_arg) > 0) {
 script_path <- normalizePath(
  sub("^--file=", "", file_arg[[1]]),
  winslash = "/",
  mustWork = TRUE
 )
 repo_root <- dirname(dirname(script_path))
} else {
 cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
 repo_root <- if (basename(cwd) == "analysis") dirname(cwd) else cwd
}

source(file.path(repo_root, "prep", "raw_paths.R"))

options(tigris_use_cache = TRUE, tigris_cache_dir = tigris_cache_dir())

results_subdir <- "county_amws_determinants"

results_subdir_path <- function(...) {
 results_file_path(results_subdir, ...)
}

wrapper_results_file_path <- function(file_name) {
 results_file_path("county_amws_determinants_wrappers", file_name)
}

###############################################################################
# Helpers
###############################################################################

as_geoid <- function(x) {
 str_pad(as.character(as.integer(x)), width = 5, side = "left", pad = "0")
}

mean_na <- function(x) {
 if (all(is.na(x))) {
  return(NA_real_)
 }

 mean(x, na.rm = TRUE)
}

sanitize_filename <- function(x) {
 str_replace_all(x, "[^A-Za-z0-9]+", "_")
}

sanitize_outcome_name <- function(x) {
 x %>%
  str_remove("^mean_") %>%
  str_remove("_1900_1930$") %>%
  sanitize_filename()
}

tidy_lm_hc1 <- function(model_obj, model_name, outcome_name,
                        state_fixed_effects) {
 broom::tidy(
  lmtest::coeftest(model_obj, vcov. = sandwich::vcovHC(model_obj, type = "HC1"))
 ) %>%
  mutate(
   outcome = outcome_name,
   model = model_name,
   state_fixed_effects = state_fixed_effects,
   nobs = stats::nobs(model_obj),
   dependent_mean = mean(
    stats::model.response(stats::model.frame(model_obj)),
    na.rm = TRUE
   ),
   r_squared = summary(model_obj)$r.squared,
   adj_r_squared = summary(model_obj)$adj.r.squared,
   .before = 1
  )
}

drop_state_fe_terms <- function(model_results) {
 model_results %>%
  filter(!str_detect(term, "^factor\\(state_fips\\)"))
}

write_html_table <- function(data, path, title) {
 html <- c(
  "<!doctype html>",
  "<html>",
  "<head>",
  "<meta charset='utf-8'>",
  paste0("<title>", title, "</title>"),
  "<style>",
  "body{font-family:Arial,sans-serif;margin:24px;}",
  "table{border-collapse:collapse;font-size:13px;}",
  "th,td{border:1px solid #ddd;padding:6px 8px;text-align:right;}",
  "th:first-child,td:first-child{text-align:left;}",
  "th{background:#f5f5f5;}",
  "</style>",
  "</head>",
  "<body>",
  paste0("<h2>", title, "</h2>"),
  paste0(
   "<p>Heteroskedasticity-robust HC1 standard errors. Generated: ",
   Sys.time(),
   "</p>"
  ),
  "<table>",
  paste0("<tr>", paste0("<th>", names(data), "</th>", collapse = ""), "</tr>")
 )

 rows <- apply(data, 1, function(row) {
  paste0("<tr>", paste0("<td>", row, "</td>", collapse = ""), "</tr>")
 })

 html <- c(html, rows, "</table>", "</body>", "</html>")
 writeLines(html, path)
 invisible(path)
}

escape_latex <- function(x) {
 x %>%
  str_replace_all("\\\\", "\\\\textbackslash{}") %>%
  str_replace_all("([&_#%])", "\\\\\\1") %>%
  str_replace_all("\\$", "\\\\\\$") %>%
  str_replace_all("\\{", "\\\\{") %>%
  str_replace_all("\\}", "\\\\}") %>%
  str_replace_all("\\^", "\\\\textasciicircum{}") %>%
  str_replace_all("~", "\\\\textasciitilde{}")
}

write_latex_results_table <- function(data, path, title) {
 display_data <- data %>%
  mutate(across(where(is.character), escape_latex))

 header <- c(
  "\\begin{table}[!htbp] \\centering",
  paste0("  \\caption{", escape_latex(title), "}"),
  "\\scriptsize",
  "\\begin{tabular}{llllrrrrrr}",
  "\\hline\\hline",
  "Outcome & Model & State FE & Term & Estimate & SE & t & p & N & Adj. R2 \\\\",
  "\\hline"
 )

 rows <- pmap_chr(
  display_data %>%
   mutate(
    estimate = sprintf("%.4f", estimate),
    std.error = sprintf("%.4f", std.error),
    statistic = sprintf("%.3f", statistic),
    p.value = sprintf("%.3f", p.value),
    adj_r_squared = sprintf("%.3f", adj_r_squared)
   ) %>%
   select(outcome, model, state_fixed_effects, term, estimate, std.error,
          statistic, p.value, nobs, adj_r_squared),
  function(outcome, model, state_fixed_effects, term, estimate, std.error,
           statistic, p.value, nobs, adj_r_squared) {
   paste(
    outcome,
    model,
    state_fixed_effects,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    nobs,
    adj_r_squared,
    sep = " & "
   ) %>%
    paste0(" \\\\")
  }
 )

 footer <- c(
  "\\hline",
  "\\end{tabular}",
  "\\end{table}"
 )

 writeLines(c(header, rows, footer), path)
 invisible(path)
}

write_latex_wrapper <- function(table_path, wrapper_path) {
 dir.create(dirname(wrapper_path), recursive = TRUE, showWarnings = FALSE)
 table_dir <- normalizePath(dirname(table_path), winslash = "/", mustWork = TRUE)
 wrapper_dir <- normalizePath(dirname(wrapper_path), winslash = "/", mustWork = TRUE)
 table_file <- if (identical(table_dir, wrapper_dir)) {
  basename(table_path)
 } else if (identical(dirname(table_dir), dirname(wrapper_dir))) {
  file.path("..", basename(table_dir), basename(table_path))
 } else {
  file.path("..", basename(table_path))
 }
 table_file <- str_replace_all(table_file, fixed("\\"), "/")

 wrapper <- c(
  "\\documentclass{article}",
  "\\usepackage[margin=0.45in,landscape]{geometry}",
  "\\usepackage{booktabs}",
  "\\begin{document}",
  paste0("\\input{", table_file, "}"),
  "\\end{document}"
 )

 writeLines(wrapper, wrapper_path)
 invisible(wrapper_path)
}

compile_latex_wrapper <- function(wrapper_path) {
 pdflatex <- Sys.which("pdflatex")
 if (!nzchar(pdflatex)) {
  warning("pdflatex not found; skipping PDF compilation for ", wrapper_path)
  return(invisible(FALSE))
 }

 old_wd <- getwd()
 on.exit(setwd(old_wd), add = TRUE)
 setwd(dirname(wrapper_path))

 status <- system2(
  pdflatex,
  args = c("-interaction=nonstopmode", basename(wrapper_path)),
  stdout = TRUE,
  stderr = TRUE
 )

 if (!is.null(attr(status, "status")) && attr(status, "status") != 0) {
  warning("pdflatex returned non-zero status for ", wrapper_path)
 }

 wrapper_stem <- tools::file_path_sans_ext(basename(wrapper_path))
 intermediate_files <- file.path(
  dirname(wrapper_path),
  paste0(wrapper_stem, c(".aux", ".log", ".out"))
 )
 unlink(intermediate_files[file.exists(intermediate_files)])

 invisible(TRUE)
}

run_model_family <- function(data, outcome, control_sets,
                             state_fixed_effects = FALSE) {
 map(control_sets, function(controls) {
  rhs <- if (isTRUE(state_fixed_effects)) {
   c(controls, "factor(state_fips)")
  } else {
   controls
  }

  lm(
   stats::reformulate(rhs, response = outcome),
   data = data
  )
 })
}

summarise_missing_controls <- function(data, controls, spec_name) {
 data %>%
  summarise(across(all_of(controls), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "control", values_to = "missing_count") %>%
  mutate(
   spec = spec_name,
   total_counties = n_distinct(data$GEOID),
   missing_pct = 100 * missing_count / total_counties,
   .before = 1
  )
}

###############################################################################
# Load data
###############################################################################

county_covariates <- read_csv(
 output_file_path("county_tpe_covariates_clean.csv"),
 show_col_types = FALSE
) %>%
 mutate(GEOID = as_geoid(GEOID))

county_nhgis_demographics <- read_csv(
 output_file_path("county_nhgis_demographics_panel.csv"),
 show_col_types = FALSE
) %>%
 mutate(GEOID = as_geoid(GEOID))

amws_panel <- read_csv(
 output_file_path("us_panel_county_amws_combined_year.csv"),
 show_col_types = FALSE
) %>%
 mutate(
  GEOID = as_geoid(GEOID),
  year = as.integer(year)
 )

county_area <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 filter(as.integer(STATEFP) <= 56) %>%
 sf::st_drop_geometry() %>%
 transmute(
  GEOID = as_geoid(GEOID),
  county_area_km2 = as.numeric(ALAND) / 1e6
 )

###############################################################################
# Nineteenth-century controls
###############################################################################

covariate_vars <- c(
 "sex_ratio",
 "post_offices",
 "immigrant_share",
 "canal_access",
 "manufacturing_output_value_real_1900",
 "farming_output_value_real_1900",
 "cropland_km2",
 "grazeland_km2",
 "hyde_population"
)

covariates_19c <- county_covariates %>%
 filter(year >= 1800, year <= 1890) %>%
 group_by(GEOID) %>%
 summarise(
  frontier_years_1800_1890 = sum(frontier100kmL6 == 1, na.rm = TRUE) * 10,
  across(
   all_of(covariate_vars),
   mean_na,
   .names = "{.col}_mean_1800_1890"
  ),
  .groups = "drop"
 )

demographic_vars <- c(
 "slave_share",
 "illiterate_share_total_population"
)

demographics_19c <- county_nhgis_demographics %>%
 filter(year >= 1800, year <= 1890) %>%
 group_by(GEOID) %>%
 summarise(
  across(
   all_of(demographic_vars),
   mean_na,
   .names = "nhgis_{.col}_mean_1800_1890"
  ),
  .groups = "drop"
 )

###############################################################################
# AMWS outcomes, 1900-1930
###############################################################################

amws_mean_1900_1930 <- amws_panel %>%
 filter(year >= 1900, year <= 1930) %>%
 group_by(GEOID) %>%
 summarise(
  mean_amws_per_1000_births_1900_1930 =
   mean_na(amws_per_1000_births),
  mean_amws_per_100k_1900_1930 = mean_na(amws_per_100k),
  mean_log1p_n_amws_1900_1930 = mean_na(log1p_n_amws),
  total_n_amws_1900_1930 = sum(replace_na(n_amws, 0), na.rm = TRUE),
  .groups = "drop"
 )

analysis_data <- covariates_19c %>%
 left_join(demographics_19c, by = "GEOID") %>%
 left_join(amws_mean_1900_1930, by = "GEOID") %>%
 left_join(county_area, by = "GEOID") %>%
 mutate(
  state_fips = str_sub(GEOID, 1, 2),
  manufacturing_output_real_1900_million_mean_1800_1890 =
   manufacturing_output_value_real_1900_mean_1800_1890 / 1e6,
  farming_output_real_1900_million_mean_1800_1890 =
   farming_output_value_real_1900_mean_1800_1890 / 1e6,
  cropland_area_share_mean_1800_1890 = if_else(
   !is.na(county_area_km2) & county_area_km2 > 0,
   cropland_km2_mean_1800_1890 / county_area_km2,
   NA_real_
  ),
  grazeland_area_share_mean_1800_1890 = if_else(
   !is.na(county_area_km2) & county_area_km2 > 0,
   grazeland_km2_mean_1800_1890 / county_area_km2,
   NA_real_
  ),
  hyde_population_density_mean_1800_1890 = if_else(
   !is.na(county_area_km2) & county_area_km2 > 0,
   hyde_population_mean_1800_1890 / county_area_km2,
   NA_real_
  )
 ) %>%
 arrange(GEOID)

###############################################################################
# Model specifications
###############################################################################

outcomes <- c(
 "mean_amws_per_1000_births_1900_1930",
 "mean_amws_per_100k_1900_1930",
 "mean_log1p_n_amws_1900_1930"
)

outcome_labels <- c(
 mean_amws_per_1000_births_1900_1930 =
  "AMWS per 1,000 estimated births, mean 1900-1930",
 mean_amws_per_100k_1900_1930 =
  "AMWS per 100,000 population, mean 1900-1930",
 mean_log1p_n_amws_1900_1930 =
  "log(1 + AMWS count), mean 1900-1930"
)

control_sets <- list(
 baseline = c(
  "frontier_years_1800_1890",
  "cropland_area_share_mean_1800_1890",
  "grazeland_area_share_mean_1800_1890",
  "canal_access_mean_1800_1890"
 ),
 extended = c(
  "frontier_years_1800_1890",
  "cropland_area_share_mean_1800_1890",
  "grazeland_area_share_mean_1800_1890",
  "canal_access_mean_1800_1890",
  "sex_ratio_mean_1800_1890",
  "post_offices_mean_1800_1890",
  "manufacturing_output_real_1900_million_mean_1800_1890",
  "farming_output_real_1900_million_mean_1800_1890",
  "immigrant_share_mean_1800_1890",
  "nhgis_slave_share_mean_1800_1890"
 ),
 full = c(
  "frontier_years_1800_1890",
  "cropland_area_share_mean_1800_1890",
  "grazeland_area_share_mean_1800_1890",
  "canal_access_mean_1800_1890",
  "sex_ratio_mean_1800_1890",
  "post_offices_mean_1800_1890",
  "manufacturing_output_real_1900_million_mean_1800_1890",
  "farming_output_real_1900_million_mean_1800_1890",
  "immigrant_share_mean_1800_1890",
  "nhgis_slave_share_mean_1800_1890",
  "nhgis_illiterate_share_total_population_mean_1800_1890",
  "hyde_population_density_mean_1800_1890"
 )
)

term_labels <- c(
 "(Intercept)" = "Intercept",
 "frontier_years_1800_1890" = "Frontier years, XIX",
 "cropland_area_share_mean_1800_1890" = "Cropland share, mean XIX",
 "grazeland_area_share_mean_1800_1890" = "Grazeland share, mean XIX",
 "canal_access_mean_1800_1890" = "Canal access, mean XIX",
 "sex_ratio_mean_1800_1890" = "Sex ratio, mean XIX",
 "post_offices_mean_1800_1890" = "Post offices, mean XIX",
 "manufacturing_output_real_1900_million_mean_1800_1890" =
  "Manufacturing output (million 1900 dollars), mean XIX",
 "farming_output_real_1900_million_mean_1800_1890" =
  "Farming output (million 1900 dollars), mean XIX",
 "immigrant_share_mean_1800_1890" = "Immigrant share, mean XIX",
 "nhgis_slave_share_mean_1800_1890" = "Slave share, mean XIX",
 "nhgis_illiterate_share_total_population_mean_1800_1890" =
  "Illiterate share, mean XIX",
 "hyde_population_density_mean_1800_1890" =
  "HYDE population density, mean XIX"
)

covariate_labels_mean_1930 <- unname(term_labels[control_sets$full])

stargazer_outcome_labels <- c(
 mean_amws_per_1000_births_1900_1930 =
  "Mean AMWS per 1,000 estimated births, 1900-1930",
 mean_amws_per_100k_1900_1930 =
  "Mean AMWS per 100k, 1900-1930",
 mean_log1p_n_amws_1900_1930 =
  "Mean log(1 + AMWS count), 1900-1930"
)

###############################################################################
# Run models
###############################################################################

models_no_fe <- map(
 outcomes,
 \(outcome) run_model_family(
  analysis_data,
  outcome,
  control_sets,
  state_fixed_effects = FALSE
 )
) %>%
 set_names(outcomes)

models_state_fe <- map(
 outcomes,
 \(outcome) run_model_family(
  analysis_data,
  outcome,
  control_sets,
  state_fixed_effects = TRUE
 )
) %>%
 set_names(outcomes)

model_results_no_fe <- imap_dfr(models_no_fe, function(models, outcome) {
 imap_dfr(
  models,
  \(model_obj, model_name) tidy_lm_hc1(
   model_obj,
   model_name,
   outcome,
   "No"
  )
 )
})

model_results_state_fe <- imap_dfr(models_state_fe, function(models, outcome) {
 imap_dfr(
  models,
  \(model_obj, model_name) tidy_lm_hc1(
   model_obj,
   model_name,
   outcome,
   "Yes"
  )
 )
})

format_results <- function(results) {
 results %>%
  drop_state_fe_terms() %>%
  mutate(
   outcome_label = recode(outcome, !!!outcome_labels),
   term_label = recode(term, !!!term_labels, .default = term),
   across(
    c(estimate, std.error, statistic, p.value, dependent_mean,
      r_squared, adj_r_squared),
    ~ round(.x, 6)
   )
  ) %>%
  select(
   outcome,
   outcome_label,
   model,
   state_fixed_effects,
   term,
   term_label,
   estimate,
   std.error,
   statistic,
   p.value,
   nobs,
   dependent_mean,
   r_squared,
   adj_r_squared
  )
}

model_results_no_fe_out <- format_results(model_results_no_fe)
model_results_state_fe_out <- format_results(model_results_state_fe)
model_results_combined_out <- bind_rows(
 model_results_no_fe_out,
 model_results_state_fe_out
)

write_stargazer_combined_outcome <- function(outcome_name, out_path) {
 combined_models <- list(
  models_no_fe[[outcome_name]][["baseline"]],
  models_no_fe[[outcome_name]][["extended"]],
  models_no_fe[[outcome_name]][["full"]],
  models_state_fe[[outcome_name]][["baseline"]],
  models_state_fe[[outcome_name]][["extended"]],
  models_state_fe[[outcome_name]][["full"]]
 )

 combined_robust_se <- map(
  combined_models,
  \(model) sqrt(diag(sandwich::vcovHC(model, type = "HC1")))
 )

 combined_dependent_mean <- map_dbl(
  combined_models,
  \(model) mean(stats::model.response(stats::model.frame(model)), na.rm = TRUE)
 )

 e1 <- combined_models[[1]]
 e2 <- combined_models[[2]]
 e3 <- combined_models[[3]]
 e4 <- combined_models[[4]]
 e5 <- combined_models[[5]]
 e6 <- combined_models[[6]]

 stargazer::stargazer(
  e1,
  e2,
  e3,
  e4,
  e5,
  e6,
  type = "latex",
  out = out_path,
  se = combined_robust_se,
  title = "Determinants of County AMWS Levels",
  dep.var.labels = unname(stargazer_outcome_labels[[outcome_name]]),
  column.labels = c("Baseline", "Extended", "Full",
                    "Baseline", "Extended", "Full"),
  covariate.labels = covariate_labels_mean_1930,
  omit = "factor\\(state_fips\\)",
  omit.stat = c("f", "ser"),
  add.lines = list(
   c("State fixed effects", "No", "No", "No", "Yes", "Yes", "Yes"),
   c("Mean dependent variable", sprintf("%.3f", combined_dependent_mean))
  ),
  no.space = TRUE,
  font.size = "scriptsize",
  notes = "Heteroskedasticity-robust HC1 standard errors in parentheses.",
  notes.align = "l"
 )

 invisible(out_path)
}

sample_summary <- model_results_combined_out %>%
 distinct(outcome, outcome_label, model, state_fixed_effects, nobs,
          dependent_mean, r_squared, adj_r_squared) %>%
 arrange(outcome, state_fixed_effects, model)

missing_controls <- imap_dfr(control_sets, function(controls, spec_name) {
 summarise_missing_controls(analysis_data, controls, spec_name)
})

missing_outcomes <- analysis_data %>%
 summarise(across(all_of(outcomes), ~ sum(is.na(.x)))) %>%
 pivot_longer(everything(), names_to = "outcome", values_to = "missing_count") %>%
 mutate(
  outcome_label = recode(outcome, !!!outcome_labels),
  total_counties = n_distinct(analysis_data$GEOID),
  missing_pct = 100 * missing_count / total_counties
 )

outcome_summary <- analysis_data %>%
 select(all_of(outcomes)) %>%
 pivot_longer(everything(), names_to = "outcome", values_to = "value") %>%
 group_by(outcome) %>%
 summarise(
  n = sum(!is.na(value)),
  mean = mean(value, na.rm = TRUE),
  sd = sd(value, na.rm = TRUE),
  min = min(value, na.rm = TRUE),
  p25 = quantile(value, 0.25, na.rm = TRUE, names = FALSE),
  median = median(value, na.rm = TRUE),
  p75 = quantile(value, 0.75, na.rm = TRUE, names = FALSE),
  max = max(value, na.rm = TRUE),
  .groups = "drop"
 ) %>%
 mutate(outcome_label = recode(outcome, !!!outcome_labels), .after = outcome)

###############################################################################
# Export outputs
###############################################################################

write_csv(
 model_results_no_fe_out,
 results_subdir_path("county_amws_determinants_models_mean_1900_1930.csv"),
 na = ""
)

write_csv(
 model_results_state_fe_out,
 results_subdir_path(
  "county_amws_determinants_models_mean_1900_1930_state_fe.csv"
 ),
 na = ""
)

write_csv(
 model_results_combined_out,
 results_subdir_path(
  "county_amws_determinants_models_mean_1900_1930_combined.csv"
 ),
 na = ""
)

write_html_table(
 model_results_no_fe_out,
 results_subdir_path("county_amws_determinants_models_mean_1900_1930.html"),
 "County AMWS Determinants, Mean 1900-1930"
)

write_html_table(
 model_results_state_fe_out,
 results_subdir_path(
  "county_amws_determinants_models_mean_1900_1930_state_fe.html"
 ),
 "County AMWS Determinants with State Fixed Effects, Mean 1900-1930"
)

write_html_table(
 model_results_combined_out,
 results_subdir_path(
  "county_amws_determinants_models_mean_1900_1930_combined.html"
 ),
 "County AMWS Determinants, Combined State FE Specifications, Mean 1900-1930"
)

write_latex_results_table(
 model_results_no_fe_out,
 results_subdir_path("county_amws_determinants_models_mean_1900_1930.tex"),
 "County AMWS Determinants, Mean 1900-1930"
)

no_fe_wrapper_path <- wrapper_results_file_path(
 "county_amws_determinants_models_mean_1900_1930_wrapper.tex"
)
write_latex_wrapper(
 results_subdir_path("county_amws_determinants_models_mean_1900_1930.tex"),
 no_fe_wrapper_path
)
compile_latex_wrapper(no_fe_wrapper_path)

write_latex_results_table(
 model_results_state_fe_out,
 results_subdir_path(
  "county_amws_determinants_models_mean_1900_1930_state_fe.tex"
 ),
 "County AMWS Determinants with State Fixed Effects, Mean 1900-1930"
)

state_fe_wrapper_path <- wrapper_results_file_path(
 "county_amws_determinants_models_mean_1900_1930_state_fe_wrapper.tex"
)
write_latex_wrapper(
 results_subdir_path(
  "county_amws_determinants_models_mean_1900_1930_state_fe.tex"
 ),
 state_fe_wrapper_path
)
compile_latex_wrapper(state_fe_wrapper_path)

write_latex_results_table(
 model_results_combined_out,
 results_subdir_path(
  "county_amws_determinants_models_mean_1900_1930_combined.tex"
 ),
 "County AMWS Determinants, Combined State FE Specifications, Mean 1900-1930"
)

combined_wrapper_path <- wrapper_results_file_path(
 "county_amws_determinants_models_mean_1900_1930_combined_wrapper.tex"
)
write_latex_wrapper(
 results_subdir_path(
  "county_amws_determinants_models_mean_1900_1930_combined.tex"
 ),
 combined_wrapper_path
)
compile_latex_wrapper(combined_wrapper_path)

walk(outcomes, function(outcome_name) {
 outcome_safe <- sanitize_outcome_name(outcome_name)

 outcome_tex_file <- paste0(
  "county_amws_determinants_models_mean_1900_1930_combined_",
  outcome_safe,
  ".tex"
 )

 outcome_wrapper_file <- paste0(
  "county_amws_determinants_models_mean_1900_1930_combined_",
  outcome_safe,
  "_wrapper.tex"
 )

 outcome_tex_path <- results_subdir_path(outcome_tex_file)
 outcome_wrapper_path <- wrapper_results_file_path(outcome_wrapper_file)

 write_stargazer_combined_outcome(outcome_name, outcome_tex_path)

 write_latex_wrapper(outcome_tex_path, outcome_wrapper_path)
 compile_latex_wrapper(outcome_wrapper_path)
})

write_csv(
 sample_summary,
 results_subdir_path("county_amws_determinants_sample_summary.csv"),
 na = ""
)

write_csv(
 missing_controls,
 results_subdir_path("county_amws_determinants_missing_controls.csv"),
 na = ""
)

write_csv(
 missing_outcomes,
 results_subdir_path("county_amws_determinants_missing_outcomes.csv"),
 na = ""
)

write_csv(
 outcome_summary,
 results_subdir_path("county_amws_determinants_outcome_summary.csv"),
 na = ""
)

notes_lines <- c(
 "County AMWS determinant regressions",
 paste0("Generated on: ", Sys.Date()),
 paste0("AMWS panel file: ",
        output_file_path("us_panel_county_amws_combined_year.csv")),
 "Outcome window: 1900-1930 birth years.",
 "Estimation: OLS with HC1 heteroskedasticity-robust standard errors.",
 "Each model uses lm complete cases for its outcome and controls.",
 "",
 "Outcomes:",
 paste0("- ", names(outcome_labels), ": ", unname(outcome_labels)),
 "",
 "Control specifications:",
 imap_chr(
  control_sets,
  ~ paste0("- ", .y, ": ", paste(.x, collapse = ", "))
 ),
 "",
 paste0("Analytic counties: ", n_distinct(analysis_data$GEOID)),
 "",
 "Outcome missingness:",
 capture.output(print(missing_outcomes, n = Inf)),
 "",
 "Sample summary:",
 capture.output(print(sample_summary, n = Inf)),
 "",
 "Control missingness:",
 capture.output(print(missing_controls, n = Inf))
)

writeLines(
 notes_lines,
 con = results_subdir_path("county_amws_determinants_notes.txt")
)

message("Saved AMWS determinant regression outputs in: ",
        results_subdir_path("."))
message("Saved AMWS determinant regression wrappers in: ",
        dirname(wrapper_results_file_path("placeholder.tex")))
message("Done. Elapsed: ",
        round(difftime(Sys.time(), initial_time, units = "mins"), 1), " min")
