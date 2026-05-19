###############################################################################
# Project: GTL Talent Determinants
# Goal: Determinants of county-level inventor growth
###############################################################################

rm(list = ls())

library("tidyverse")
library("broom")
library("sandwich")
library("lmtest")
library("stargazer")

source("../prep/raw_paths.R")

options(tigris_use_cache = TRUE, tigris_cache_dir = tigris_cache_dir())

###############################################################################
# Helpers
###############################################################################

mean_na <- function(x) {
 if (all(is.na(x))) {
  NA_real_
 } else {
  mean(x, na.rm = TRUE)
 }
}

tidy_lm_hc1 <- function(model_obj, model_name) {
 broom::tidy(
  lmtest::coeftest(model_obj, vcov. = sandwich::vcovHC(model_obj, type = "HC1"))
 ) %>%
  mutate(
   model = model_name,
   nobs = stats::nobs(model_obj),
   r_squared = summary(model_obj)$r.squared,
   adj_r_squared = summary(model_obj)$adj.r.squared,
   .before = 1
  )
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

write_latex_table <- function(data, path, title) {
 display_data <- data %>%
  mutate(variable = escape_latex(variable))

 header <- c(
  "\\begin{table}[!htbp] \\centering",
  paste0("  \\caption{", escape_latex(title), "}"),
  "\\begin{tabular}{lrrrrrrrr}",
  "\\hline\\hline",
  "Variable & N & Mean & SD & Min & P25 & Median & P75 & Max \\\\",
  "\\hline"
 )

 rows <- pmap_chr(display_data, function(variable, n, mean, sd, min, p25, median, p75, max) {
  paste(
   variable,
   n,
   sprintf("%.3f", mean),
   sprintf("%.3f", sd),
   sprintf("%.3f", min),
   sprintf("%.3f", p25),
   sprintf("%.3f", median),
   sprintf("%.3f", p75),
   sprintf("%.3f", max),
   sep = " & "
  ) %>%
   paste0(" \\\\")
 })

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
 } else {
  file.path("..", basename(table_path))
 }
 table_file <- str_replace_all(table_file, fixed("\\"), "/")
 wrapper <- c(
  "\\documentclass{article}",
  "\\usepackage[margin=0.7in,landscape]{geometry}",
  "\\usepackage{booktabs}",
  "\\begin{document}",
  paste0("\\input{", table_file, "}"),
  "\\end{document}"
 )
 writeLines(wrapper, wrapper_path)
 invisible(wrapper_path)
}

wrapper_results_file_path <- function(file_name) {
 results_file_path("county_inventor_determinants_wrappers", file_name)
}

cleanup_wrapper_intermediates <- function() {
 wrapper_dir <- dirname(wrapper_results_file_path("placeholder.pdf"))
 unlink(
  list.files(
   wrapper_dir,
   pattern = "\\.(aux|log|tex|out)$",
   full.names = TRUE
  )
 )
 invisible(TRUE)
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
  paste0(wrapper_stem, c(".aux", ".log", ".tex", ".out"))
 )
 unlink(intermediate_files[file.exists(intermediate_files)])

 invisible(TRUE)
}

drop_state_fe_terms <- function(model_results) {
 model_results %>%
  filter(!str_detect(term, "^factor\\(state_fips\\)"))
}

###############################################################################
# Load data
###############################################################################

county_covariates <- read_csv(
 output_file_path("county_tpe_covariates_clean.csv"),
 show_col_types = FALSE
)

county_inventor_rates <- read_csv(
 output_file_path("county_inventor_rates_hyde.csv"),
 show_col_types = FALSE
)

county_nhgis_demographics <- read_csv(
 output_file_path("county_nhgis_demographics_panel.csv"),
 show_col_types = FALSE
)

county_area <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020) %>%
 filter(as.integer(STATEFP) <= 56) %>%
 sf::st_drop_geometry() %>%
 transmute(
  GEOID,
  county_area_km2 = as.numeric(ALAND) / 1e6
 )

###############################################################################
# Nineteenth-century covariate means
###############################################################################

covariate_vars <- c(
 "sex_ratio",
 "post_offices",
 "immigrant_share",
 "canal_access",
 "manufacturing_output_value",
 "farming_output_value",
 "manufacturing_output_value_real_1900",
 "farming_output_value_real_1900",
 "cropland_km2",
 "grazeland_km2",
 "hyde_population",
 "n_inventors"
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
 "total_population",
 "total_slaves",
 "slave_share",
 "illiterate_persons",
 "illiterate_share_total_population",
 "cannot_read_persons",
 "cannot_read_share_total_population"
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
# Nineteenth-century inventor-rate change
###############################################################################

inventor_change_19c <- county_inventor_rates %>%
 filter(year %in% c(1800, 1890)) %>%
 select(GEOID, year, inventors_per_100k_hyde) %>%
 pivot_wider(
  names_from = year,
  values_from = inventors_per_100k_hyde,
  names_glue = "inventors_per_100k_hyde_{year}"
 ) %>%
 mutate(
  delta_inventors_per_100k_hyde_1800_1890 =
   inventors_per_100k_hyde_1890 - inventors_per_100k_hyde_1800
 ) %>%
 select(GEOID, delta_inventors_per_100k_hyde_1800_1890)

###############################################################################
# Twentieth-century inventor growth outcome
###############################################################################

outcomes_20c <- county_inventor_rates %>%
 filter(year %in% c(1900, 1950, 2000)) %>%
 select(GEOID, year, n_inventors, hyde_population, inventors_per_100k_hyde) %>%
 pivot_wider(
  names_from = year,
  values_from = c(n_inventors, hyde_population, inventors_per_100k_hyde),
  names_glue = "{.value}_{year}"
 ) %>%
 mutate(
  delta_inventors_per_100k_1900_1950 =
   inventors_per_100k_hyde_1950 - inventors_per_100k_hyde_1900,
  delta_inventors_per_100k_1900_2000 =
   inventors_per_100k_hyde_2000 - inventors_per_100k_hyde_1900,
  delta_n_inventors_1900_1950 = n_inventors_1950 - n_inventors_1900,
  delta_n_inventors_1900_2000 = n_inventors_2000 - n_inventors_1900
 )

inventor_mean_1900_1960 <- county_inventor_rates %>%
 filter(year >= 1900, year <= 1960) %>%
 group_by(GEOID) %>%
 summarise(
  mean_inventors_per_100k_hyde_1900_1960 =
   mean_na(inventors_per_100k_hyde),
  .groups = "drop"
 )

analysis_data <- covariates_19c %>%
 left_join(demographics_19c, by = "GEOID") %>%
 left_join(inventor_change_19c, by = "GEOID") %>%
 left_join(outcomes_20c, by = "GEOID") %>%
 left_join(inventor_mean_1900_1960, by = "GEOID") %>%
 left_join(county_area, by = "GEOID") %>%
 mutate(
  state_fips = str_sub(str_pad(as.character(GEOID), 5, pad = "0"), 1, 2),
  manufacturing_output_million_mean_1800_1890 =
   manufacturing_output_value_mean_1800_1890 / 1e6,
 farming_output_million_mean_1800_1890 =
   farming_output_value_mean_1800_1890 / 1e6,
  manufacturing_output_real_1900_million_mean_1800_1890 =
   manufacturing_output_value_real_1900_mean_1800_1890 / 1e6,
  farming_output_real_1900_million_mean_1800_1890 =
   farming_output_value_real_1900_mean_1800_1890 / 1e6,
  hyde_population_thousand_mean_1800_1890 =
   hyde_population_mean_1800_1890 / 1e3,
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

write_csv(
 analysis_data,
 output_file_path("county_inventor_determinants_analysis.csv"),
 na = ""
)

###############################################################################
# Descriptive statistics for explanatory variables
###############################################################################

explanatory_variable_labels <- tribble(
 ~variable, ~label,
 "frontier_years_1800_1890", "Frontier years, XIX",
 "cropland_km2_mean_1800_1890", "Cropland km2, mean XIX",
 "grazeland_km2_mean_1800_1890", "Grazeland km2, mean XIX",
 "canal_access_mean_1800_1890", "Canal access, mean XIX",
 "sex_ratio_mean_1800_1890", "Sex ratio, mean XIX",
 "post_offices_mean_1800_1890", "Post offices, mean XIX",
 "manufacturing_output_real_1900_million_mean_1800_1890",
 "Manufacturing output (million 1900 dollars), mean XIX",
 "farming_output_real_1900_million_mean_1800_1890",
 "Farming output (million 1900 dollars), mean XIX",
 "immigrant_share_mean_1800_1890", "Immigrant share, mean XIX",
 "nhgis_slave_share_mean_1800_1890", "Slave share, mean XIX",
 "nhgis_illiterate_share_total_population_mean_1800_1890",
 "Illiterate share, mean XIX",
 "hyde_population_thousand_mean_1800_1890",
 "HYDE population (thousands), mean XIX",
 "delta_inventors_per_100k_hyde_1800_1890",
 "Change in inventors per 100k, 1800-1890"
)

explanatory_summary <- analysis_data %>%
 select(all_of(explanatory_variable_labels$variable)) %>%
 pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
 group_by(variable) %>%
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
 left_join(explanatory_variable_labels, by = "variable") %>%
 mutate(variable_order = match(variable, explanatory_variable_labels$variable)) %>%
 arrange(variable_order) %>%
 mutate(variable = label) %>%
 select(variable, n, mean, sd, min, p25, median, p75, max)

write_csv(
 explanatory_summary,
 results_file_path("county_inventor_explanatory_summary.csv"),
 na = ""
)

write_html_table(
 explanatory_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))),
 results_file_path("county_inventor_explanatory_summary.html"),
 "County Inventor Explanatory Variables"
)

write_latex_table(
 explanatory_summary,
 results_file_path("county_inventor_explanatory_summary.tex"),
 "County Inventor Explanatory Variables"
)

###############################################################################
# Models
###############################################################################

baseline_formula <- delta_inventors_per_100k_1900_2000 ~
 frontier_years_1800_1890 +
 cropland_km2_mean_1800_1890 +
 grazeland_km2_mean_1800_1890 +
 canal_access_mean_1800_1890

extended_formula <- update(
 baseline_formula,
 . ~ . +
 sex_ratio_mean_1800_1890 +
  post_offices_mean_1800_1890 +
  manufacturing_output_real_1900_million_mean_1800_1890 +
  farming_output_real_1900_million_mean_1800_1890 +
  immigrant_share_mean_1800_1890 +
  nhgis_slave_share_mean_1800_1890
)

full_formula <- update(
 extended_formula,
 . ~ . +
 nhgis_illiterate_share_total_population_mean_1800_1890 +
 hyde_population_thousand_mean_1800_1890 +
  delta_inventors_per_100k_hyde_1800_1890
)

models <- list(
 baseline = lm(baseline_formula, data = analysis_data),
 extended = lm(extended_formula, data = analysis_data),
 full = lm(full_formula, data = analysis_data)
)

model_results <- bind_rows(
 tidy_lm_hc1(models$baseline, "baseline"),
 tidy_lm_hc1(models$extended, "extended"),
 tidy_lm_hc1(models$full, "full")
)

model_results_out <- model_results %>%
 mutate(
  across(
   c(estimate, std.error, statistic, p.value, r_squared, adj_r_squared),
   ~ round(.x, 6)
  )
 )

robust_se <- map(
 models[c("baseline", "extended", "full")],
 \(model) sqrt(diag(sandwich::vcovHC(model, type = "HC1")))
)

dependent_mean <- map_dbl(
 models[c("baseline", "extended", "full")],
 \(model) mean(stats::model.response(stats::model.frame(model)), na.rm = TRUE)
)

model_tex_path <- results_file_path("county_inventor_determinants_models.tex")

stargazer::stargazer(
 models$baseline,
 models$extended,
 models$full,
 type = "latex",
 out = model_tex_path,
 se = robust_se,
 title = "Determinants of County Inventor Growth",
 dep.var.labels = "Change in inventors per 100k, 1900-2000",
 column.labels = c("Baseline", "Extended", "Full"),
 covariate.labels = c(
  "Frontier years, XIX",
  "Cropland km2, mean XIX",
  "Grazeland km2, mean XIX",
  "Canal access, mean XIX",
  "Sex ratio, mean XIX",
  "Post offices, mean XIX",
  "Manufacturing output (million 1900 dollars), mean XIX",
  "Farming output (million 1900 dollars), mean XIX",
  "Immigrant share, mean XIX",
  "Slave share, mean XIX",
  "Illiterate share, mean XIX",
  "HYDE population (thousands), mean XIX",
  "Change in inventors per 100k, 1800-1890"
 ),
 omit.stat = c("f", "ser"),
 add.lines = list(
  c("Mean dependent variable", sprintf("%.3f", dependent_mean))
 ),
 no.space = TRUE,
 notes = "Heteroskedasticity-robust HC1 standard errors in parentheses.",
 notes.align = "l"
)

write_csv(
 model_results_out,
 results_file_path("county_inventor_determinants_models.csv"),
 na = ""
)

write_html_table(
 model_results_out,
 results_file_path("county_inventor_determinants_models.html"),
 "County Inventor Growth Determinants"
)

###############################################################################
# Models with state fixed effects
###############################################################################

baseline_formula_state_fe <- update(
 baseline_formula,
 . ~ . + factor(state_fips)
)

extended_formula_state_fe <- update(
 extended_formula,
 . ~ . + factor(state_fips)
)

full_formula_state_fe <- update(
 full_formula,
 . ~ . + factor(state_fips)
)

models_state_fe <- list(
 baseline = lm(baseline_formula_state_fe, data = analysis_data),
 extended = lm(extended_formula_state_fe, data = analysis_data),
 full = lm(full_formula_state_fe, data = analysis_data)
)

model_results_state_fe <- bind_rows(
 tidy_lm_hc1(models_state_fe$baseline, "baseline"),
 tidy_lm_hc1(models_state_fe$extended, "extended"),
 tidy_lm_hc1(models_state_fe$full, "full")
)

model_results_state_fe_out <- model_results_state_fe %>%
 drop_state_fe_terms() %>%
 mutate(
  across(
   c(estimate, std.error, statistic, p.value, r_squared, adj_r_squared),
   ~ round(.x, 6)
  )
 )

robust_se_state_fe <- map(
 models_state_fe[c("baseline", "extended", "full")],
 \(model) sqrt(diag(sandwich::vcovHC(model, type = "HC1")))
)

dependent_mean_state_fe <- map_dbl(
 models_state_fe[c("baseline", "extended", "full")],
 \(model) mean(stats::model.response(stats::model.frame(model)), na.rm = TRUE)
)

covariate_labels <- c(
 "Frontier years, XIX",
 "Cropland km2, mean XIX",
 "Grazeland km2, mean XIX",
 "Canal access, mean XIX",
 "Sex ratio, mean XIX",
 "Post offices, mean XIX",
 "Manufacturing output (million 1900 dollars), mean XIX",
 "Farming output (million 1900 dollars), mean XIX",
 "Immigrant share, mean XIX",
 "Slave share, mean XIX",
 "Illiterate share, mean XIX",
 "HYDE population (thousands), mean XIX",
 "Change in inventors per 100k, 1800-1890"
)

state_fe_tex_path <- results_file_path(
 "county_inventor_determinants_models_state_fe.tex"
)

stargazer::stargazer(
 models_state_fe$baseline,
 models_state_fe$extended,
 models_state_fe$full,
 type = "latex",
 out = state_fe_tex_path,
 se = robust_se_state_fe,
 title = "Determinants of County Inventor Growth with State Fixed Effects",
 dep.var.labels = "Change in inventors per 100k, 1900-2000",
 column.labels = c("Baseline", "Extended", "Full"),
 covariate.labels = c(
  "Frontier years, XIX",
  "Cropland km2, mean XIX",
  "Grazeland km2, mean XIX",
  "Canal access, mean XIX",
  "Sex ratio, mean XIX",
  "Post offices, mean XIX",
  "Manufacturing output (million 1900 dollars), mean XIX",
  "Farming output (million 1900 dollars), mean XIX",
  "Immigrant share, mean XIX",
  "Slave share, mean XIX",
  "Illiterate share, mean XIX",
  "HYDE population (thousands), mean XIX",
  "Change in inventors per 100k, 1800-1890"
 ),
 omit = "factor\\(state_fips\\)",
 omit.stat = c("f", "ser"),
 add.lines = list(
  c("State fixed effects", "Yes", "Yes", "Yes"),
  c("Mean dependent variable", sprintf("%.3f", dependent_mean_state_fe))
 ),
 no.space = TRUE,
 notes = "Heteroskedasticity-robust HC1 standard errors in parentheses.",
 notes.align = "l"
)

write_csv(
 model_results_state_fe_out,
 results_file_path("county_inventor_determinants_models_state_fe.csv"),
 na = ""
)

write_html_table(
 model_results_state_fe_out,
 results_file_path("county_inventor_determinants_models_state_fe.html"),
 "County Inventor Growth Determinants with State Fixed Effects"
)

combined_models <- c(
 models[c("baseline", "extended", "full")],
 models_state_fe[c("baseline", "extended", "full")]
)
combined_robust_se <- c(robust_se, robust_se_state_fe)
combined_dependent_mean <- c(dependent_mean, dependent_mean_state_fe)

combined_results_out <- bind_rows(
 model_results %>% mutate(state_fixed_effects = "No"),
 model_results_state_fe %>%
  drop_state_fe_terms() %>%
  mutate(state_fixed_effects = "Yes")
) %>%
 mutate(
  across(
   c(estimate, std.error, statistic, p.value, r_squared, adj_r_squared),
   ~ round(.x, 6)
  )
 )

combined_tex_path <- results_file_path(
 "county_inventor_determinants_models_combined.tex"
)

c1 <- combined_models[[1]]
c2 <- combined_models[[2]]
c3 <- combined_models[[3]]
c4 <- combined_models[[4]]
c5 <- combined_models[[5]]
c6 <- combined_models[[6]]

stargazer::stargazer(
 c1,
 c2,
 c3,
 c4,
 c5,
 c6,
 type = "latex",
 out = combined_tex_path,
 se = combined_robust_se,
 title = "Determinants of County Inventor Growth",
 dep.var.labels = "Change in inventors per 100k, 1900-2000",
 column.labels = c("Baseline", "Extended", "Full", "Baseline", "Extended", "Full"),
 covariate.labels = covariate_labels,
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

write_csv(
 combined_results_out,
 results_file_path("county_inventor_determinants_models_combined.csv"),
 na = ""
)

write_html_table(
 combined_results_out,
 results_file_path("county_inventor_determinants_models_combined.html"),
 "County Inventor Growth Determinants, Combined State FE Specifications"
)

combined_wrapper_path <- wrapper_results_file_path(
 "county_inventor_determinants_models_combined_wrapper.tex"
)
write_latex_wrapper(combined_tex_path, combined_wrapper_path)
compile_latex_wrapper(combined_wrapper_path)

###############################################################################
# Alternative outcome: mean inventor rate, 1900-1960
###############################################################################

baseline_formula_mean_1960 <- mean_inventors_per_100k_hyde_1900_1960 ~
 frontier_years_1800_1890 +
 cropland_area_share_mean_1800_1890 +
 grazeland_area_share_mean_1800_1890 +
 canal_access_mean_1800_1890

extended_formula_mean_1960 <- update(
 baseline_formula_mean_1960,
 . ~ . +
 sex_ratio_mean_1800_1890 +
  post_offices_mean_1800_1890 +
  manufacturing_output_real_1900_million_mean_1800_1890 +
  farming_output_real_1900_million_mean_1800_1890 +
  immigrant_share_mean_1800_1890 +
  nhgis_slave_share_mean_1800_1890
)

full_formula_mean_1960 <- update(
 extended_formula_mean_1960,
 . ~ . +
 nhgis_illiterate_share_total_population_mean_1800_1890 +
 hyde_population_density_mean_1800_1890
)

models_mean_1960 <- list(
 baseline = lm(baseline_formula_mean_1960, data = analysis_data),
 extended = lm(extended_formula_mean_1960, data = analysis_data),
 full = lm(full_formula_mean_1960, data = analysis_data)
)

model_results_mean_1960 <- bind_rows(
 tidy_lm_hc1(models_mean_1960$baseline, "baseline"),
 tidy_lm_hc1(models_mean_1960$extended, "extended"),
 tidy_lm_hc1(models_mean_1960$full, "full")
)

model_results_mean_1960_out <- model_results_mean_1960 %>%
 mutate(
  across(
   c(estimate, std.error, statistic, p.value, r_squared, adj_r_squared),
   ~ round(.x, 6)
  )
 )

robust_se_mean_1960 <- map(
 models_mean_1960[c("baseline", "extended", "full")],
 \(model) sqrt(diag(sandwich::vcovHC(model, type = "HC1")))
)

dependent_mean_mean_1960 <- map_dbl(
 models_mean_1960[c("baseline", "extended", "full")],
 \(model) mean(stats::model.response(stats::model.frame(model)), na.rm = TRUE)
)

covariate_labels_mean_1960 <- c(
 "Frontier years, XIX",
 "Cropland share, mean XIX",
 "Grazeland share, mean XIX",
 "Canal access, mean XIX",
 "Sex ratio, mean XIX",
 "Post offices, mean XIX",
 "Manufacturing output (million 1900 dollars), mean XIX",
 "Farming output (million 1900 dollars), mean XIX",
 "Immigrant share, mean XIX",
 "Slave share, mean XIX",
 "Illiterate share, mean XIX",
 "HYDE population density, mean XIX"
)

mean_1960_tex_path <- results_file_path(
 "county_inventor_determinants_models_mean_1900_1960.tex"
)

stargazer::stargazer(
 models_mean_1960$baseline,
 models_mean_1960$extended,
 models_mean_1960$full,
 type = "latex",
 out = mean_1960_tex_path,
 se = robust_se_mean_1960,
 title = "Determinants of County Inventor Levels",
 dep.var.labels = "Mean inventors per 100k, 1900-1960",
 column.labels = c("Baseline", "Extended", "Full"),
 covariate.labels = covariate_labels_mean_1960,
 omit.stat = c("f", "ser"),
 add.lines = list(
  c("Mean dependent variable", sprintf("%.3f", dependent_mean_mean_1960))
 ),
 no.space = TRUE,
 notes = "Heteroskedasticity-robust HC1 standard errors in parentheses.",
 notes.align = "l"
)

write_csv(
 model_results_mean_1960_out,
 results_file_path("county_inventor_determinants_models_mean_1900_1960.csv"),
 na = ""
)

write_html_table(
 model_results_mean_1960_out,
 results_file_path("county_inventor_determinants_models_mean_1900_1960.html"),
 "County Inventor Level Determinants, 1900-1960"
)

baseline_formula_mean_1960_state_fe <- update(
 baseline_formula_mean_1960,
 . ~ . + factor(state_fips)
)

extended_formula_mean_1960_state_fe <- update(
 extended_formula_mean_1960,
 . ~ . + factor(state_fips)
)

full_formula_mean_1960_state_fe <- update(
 full_formula_mean_1960,
 . ~ . + factor(state_fips)
)

models_mean_1960_state_fe <- list(
 baseline = lm(baseline_formula_mean_1960_state_fe, data = analysis_data),
 extended = lm(extended_formula_mean_1960_state_fe, data = analysis_data),
 full = lm(full_formula_mean_1960_state_fe, data = analysis_data)
)

model_results_mean_1960_state_fe <- bind_rows(
 tidy_lm_hc1(models_mean_1960_state_fe$baseline, "baseline"),
 tidy_lm_hc1(models_mean_1960_state_fe$extended, "extended"),
 tidy_lm_hc1(models_mean_1960_state_fe$full, "full")
)

model_results_mean_1960_state_fe_out <- model_results_mean_1960_state_fe %>%
 drop_state_fe_terms() %>%
 mutate(
  across(
   c(estimate, std.error, statistic, p.value, r_squared, adj_r_squared),
   ~ round(.x, 6)
  )
 )

robust_se_mean_1960_state_fe <- map(
 models_mean_1960_state_fe[c("baseline", "extended", "full")],
 \(model) sqrt(diag(sandwich::vcovHC(model, type = "HC1")))
)

dependent_mean_mean_1960_state_fe <- map_dbl(
 models_mean_1960_state_fe[c("baseline", "extended", "full")],
 \(model) mean(stats::model.response(stats::model.frame(model)), na.rm = TRUE)
)

mean_1960_state_fe_tex_path <- results_file_path(
 "county_inventor_determinants_models_mean_1900_1960_state_fe.tex"
)

f1 <- models_mean_1960_state_fe$baseline
f2 <- models_mean_1960_state_fe$extended
f3 <- models_mean_1960_state_fe$full

stargazer::stargazer(
 f1,
 f2,
 f3,
 type = "latex",
 out = mean_1960_state_fe_tex_path,
 se = robust_se_mean_1960_state_fe,
 title = "Determinants of County Inventor Levels with State Fixed Effects",
 dep.var.labels = "Mean inventors per 100k, 1900-1960",
 column.labels = c("Baseline", "Extended", "Full"),
 covariate.labels = covariate_labels_mean_1960,
 omit = "factor\\(state_fips\\)",
 omit.stat = c("f", "ser"),
 add.lines = list(
  c("State fixed effects", "Yes", "Yes", "Yes"),
  c("Mean dependent variable", sprintf("%.3f", dependent_mean_mean_1960_state_fe))
 ),
 no.space = TRUE,
 notes = "Heteroskedasticity-robust HC1 standard errors in parentheses.",
 notes.align = "l"
)

write_csv(
 model_results_mean_1960_state_fe_out,
 results_file_path(
  "county_inventor_determinants_models_mean_1900_1960_state_fe.csv"
 ),
 na = ""
)

write_html_table(
 model_results_mean_1960_state_fe_out,
 results_file_path(
  "county_inventor_determinants_models_mean_1900_1960_state_fe.html"
 ),
 "County Inventor Level Determinants with State Fixed Effects, 1900-1960"
)

combined_models_mean_1960 <- c(
 models_mean_1960[c("baseline", "extended", "full")],
 models_mean_1960_state_fe[c("baseline", "extended", "full")]
)
combined_robust_se_mean_1960 <- c(
 robust_se_mean_1960,
 robust_se_mean_1960_state_fe
)
combined_dependent_mean_mean_1960 <- c(
 dependent_mean_mean_1960,
 dependent_mean_mean_1960_state_fe
)

combined_results_mean_1960_out <- bind_rows(
 model_results_mean_1960 %>% mutate(state_fixed_effects = "No"),
 model_results_mean_1960_state_fe %>%
  drop_state_fe_terms() %>%
  mutate(state_fixed_effects = "Yes")
) %>%
 mutate(
  across(
   c(estimate, std.error, statistic, p.value, r_squared, adj_r_squared),
   ~ round(.x, 6)
  )
 )

combined_mean_1960_tex_path <- results_file_path(
 "county_inventor_determinants_models_mean_1900_1960_combined.tex"
)

e1 <- combined_models_mean_1960[[1]]
e2 <- combined_models_mean_1960[[2]]
e3 <- combined_models_mean_1960[[3]]
e4 <- combined_models_mean_1960[[4]]
e5 <- combined_models_mean_1960[[5]]
e6 <- combined_models_mean_1960[[6]]

stargazer::stargazer(
 e1,
 e2,
 e3,
 e4,
 e5,
 e6,
 type = "latex",
 out = combined_mean_1960_tex_path,
 se = combined_robust_se_mean_1960,
 title = "Determinants of County Inventor Levels",
 dep.var.labels = "Mean inventors per 100k, 1900-1960",
 column.labels = c("Baseline", "Extended", "Full", "Baseline", "Extended", "Full"),
 covariate.labels = covariate_labels_mean_1960,
 omit = "factor\\(state_fips\\)",
 omit.stat = c("f", "ser"),
 add.lines = list(
  c("State fixed effects", "No", "No", "No", "Yes", "Yes", "Yes"),
  c("Mean dependent variable", sprintf("%.3f", combined_dependent_mean_mean_1960))
 ),
 no.space = TRUE,
 font.size = "scriptsize",
 notes = "Heteroskedasticity-robust HC1 standard errors in parentheses.",
 notes.align = "l"
)

write_csv(
 combined_results_mean_1960_out,
 results_file_path(
  "county_inventor_determinants_models_mean_1900_1960_combined.csv"
 ),
 na = ""
)

write_html_table(
 combined_results_mean_1960_out,
 results_file_path(
  "county_inventor_determinants_models_mean_1900_1960_combined.html"
 ),
 "County Inventor Level Determinants, Combined State FE Specifications, 1900-1960"
)

combined_mean_1960_wrapper_path <- wrapper_results_file_path(
 "county_inventor_determinants_models_mean_1900_1960_combined_wrapper.tex"
)
write_latex_wrapper(combined_mean_1960_tex_path, combined_mean_1960_wrapper_path)
compile_latex_wrapper(combined_mean_1960_wrapper_path)

###############################################################################
# Alternative sparse specifications: frontier and density
###############################################################################

alt_formula_frontier_mean_1960 <- mean_inventors_per_100k_hyde_1900_1960 ~
 frontier_years_1800_1890

alt_formula_density_mean_1960 <- mean_inventors_per_100k_hyde_1900_1960 ~
 hyde_population_density_mean_1800_1890

alt_formula_frontier_density_mean_1960 <-
 mean_inventors_per_100k_hyde_1900_1960 ~
 frontier_years_1800_1890 +
 hyde_population_density_mean_1800_1890

models_alt_mean_1960 <- list(
 frontier = lm(alt_formula_frontier_mean_1960, data = analysis_data),
 density = lm(alt_formula_density_mean_1960, data = analysis_data),
 frontier_density = lm(
  alt_formula_frontier_density_mean_1960,
  data = analysis_data
 )
)

model_results_alt_mean_1960 <- bind_rows(
 tidy_lm_hc1(models_alt_mean_1960$frontier, "frontier_only"),
 tidy_lm_hc1(models_alt_mean_1960$density, "density_only"),
 tidy_lm_hc1(
  models_alt_mean_1960$frontier_density,
  "frontier_and_density"
 )
)

model_results_alt_mean_1960_out <- model_results_alt_mean_1960 %>%
 mutate(
  across(
   c(estimate, std.error, statistic, p.value, r_squared, adj_r_squared),
   ~ round(.x, 6)
  )
 )

robust_se_alt_mean_1960 <- map(
 models_alt_mean_1960[c("frontier", "density", "frontier_density")],
 \(model) sqrt(diag(sandwich::vcovHC(model, type = "HC1")))
)

dependent_mean_alt_mean_1960 <- map_dbl(
 models_alt_mean_1960[c("frontier", "density", "frontier_density")],
 \(model) mean(stats::model.response(stats::model.frame(model)), na.rm = TRUE)
)

alt_mean_1960_tex_path <- results_file_path(
 "county_inventor_determinants_models_mean_1900_1960_alt_specs.tex"
)

g1 <- models_alt_mean_1960$frontier
g2 <- models_alt_mean_1960$density
g3 <- models_alt_mean_1960$frontier_density

stargazer::stargazer(
 g1,
 g2,
 g3,
 type = "latex",
 out = alt_mean_1960_tex_path,
 se = robust_se_alt_mean_1960,
 title = "Alternative Sparse Specifications for County Inventor Levels",
 dep.var.labels = "Mean inventors per 100k, 1900-1960",
 column.labels = c("Frontier only", "Density only", "Frontier + density"),
 covariate.labels = c(
  "Frontier years, XIX",
  "HYDE population density, mean XIX"
 ),
 omit.stat = c("f", "ser"),
 add.lines = list(
  c("Mean dependent variable", sprintf("%.3f", dependent_mean_alt_mean_1960))
 ),
 no.space = TRUE,
 notes = "Heteroskedasticity-robust HC1 standard errors in parentheses.",
 notes.align = "l"
)

write_csv(
 model_results_alt_mean_1960_out,
 results_file_path(
  "county_inventor_determinants_models_mean_1900_1960_alt_specs.csv"
 ),
 na = ""
)

write_html_table(
 model_results_alt_mean_1960_out,
 results_file_path(
  "county_inventor_determinants_models_mean_1900_1960_alt_specs.html"
 ),
 "Alternative Sparse Specifications for County Inventor Levels, 1900-1960"
)

alt_mean_1960_wrapper_path <- wrapper_results_file_path(
 "county_inventor_determinants_models_mean_1900_1960_alt_specs_wrapper.tex"
)
write_latex_wrapper(alt_mean_1960_tex_path, alt_mean_1960_wrapper_path)
compile_latex_wrapper(alt_mean_1960_wrapper_path)

###############################################################################
# Alternative outcome: inventor growth, 1900-1950
###############################################################################

baseline_formula_1950 <- update(
 baseline_formula,
 delta_inventors_per_100k_1900_1950 ~ .
)

extended_formula_1950 <- update(
 extended_formula,
 delta_inventors_per_100k_1900_1950 ~ .
)

full_formula_1950 <- update(
 full_formula,
 delta_inventors_per_100k_1900_1950 ~ .
)

models_1950 <- list(
 baseline = lm(baseline_formula_1950, data = analysis_data),
 extended = lm(extended_formula_1950, data = analysis_data),
 full = lm(full_formula_1950, data = analysis_data)
)

model_results_1950 <- bind_rows(
 tidy_lm_hc1(models_1950$baseline, "baseline"),
 tidy_lm_hc1(models_1950$extended, "extended"),
 tidy_lm_hc1(models_1950$full, "full")
)

model_results_1950_out <- model_results_1950 %>%
 mutate(
  across(
   c(estimate, std.error, statistic, p.value, r_squared, adj_r_squared),
   ~ round(.x, 6)
  )
 )

robust_se_1950 <- map(
 models_1950[c("baseline", "extended", "full")],
 \(model) sqrt(diag(sandwich::vcovHC(model, type = "HC1")))
)

dependent_mean_1950 <- map_dbl(
 models_1950[c("baseline", "extended", "full")],
 \(model) mean(stats::model.response(stats::model.frame(model)), na.rm = TRUE)
)

model_1950_tex_path <- results_file_path(
 "county_inventor_determinants_models_1900_1950.tex"
)

stargazer::stargazer(
 models_1950$baseline,
 models_1950$extended,
 models_1950$full,
 type = "latex",
 out = model_1950_tex_path,
 se = robust_se_1950,
 title = "Determinants of County Inventor Growth",
 dep.var.labels = "Change in inventors per 100k, 1900-1950",
 column.labels = c("Baseline", "Extended", "Full"),
 covariate.labels = c(
  "Frontier years, XIX",
  "Cropland km2, mean XIX",
  "Grazeland km2, mean XIX",
  "Canal access, mean XIX",
  "Sex ratio, mean XIX",
  "Post offices, mean XIX",
  "Manufacturing output (million 1900 dollars), mean XIX",
  "Farming output (million 1900 dollars), mean XIX",
  "Immigrant share, mean XIX",
  "Slave share, mean XIX",
  "Illiterate share, mean XIX",
  "HYDE population (thousands), mean XIX",
  "Change in inventors per 100k, 1800-1890"
 ),
 omit.stat = c("f", "ser"),
 add.lines = list(
  c("Mean dependent variable", sprintf("%.3f", dependent_mean_1950))
 ),
 no.space = TRUE,
 notes = "Heteroskedasticity-robust HC1 standard errors in parentheses.",
 notes.align = "l"
)

write_csv(
 model_results_1950_out,
 results_file_path("county_inventor_determinants_models_1900_1950.csv"),
 na = ""
)

write_html_table(
 model_results_1950_out,
 results_file_path("county_inventor_determinants_models_1900_1950.html"),
 "County Inventor Growth Determinants, 1900-1950"
)

###############################################################################
# Alternative outcome with state fixed effects
###############################################################################

baseline_formula_1950_state_fe <- update(
 baseline_formula_state_fe,
 delta_inventors_per_100k_1900_1950 ~ .
)

extended_formula_1950_state_fe <- update(
 extended_formula_state_fe,
 delta_inventors_per_100k_1900_1950 ~ .
)

full_formula_1950_state_fe <- update(
 full_formula_state_fe,
 delta_inventors_per_100k_1900_1950 ~ .
)

models_1950_state_fe <- list(
 baseline = lm(baseline_formula_1950_state_fe, data = analysis_data),
 extended = lm(extended_formula_1950_state_fe, data = analysis_data),
 full = lm(full_formula_1950_state_fe, data = analysis_data)
)

model_results_1950_state_fe <- bind_rows(
 tidy_lm_hc1(models_1950_state_fe$baseline, "baseline"),
 tidy_lm_hc1(models_1950_state_fe$extended, "extended"),
 tidy_lm_hc1(models_1950_state_fe$full, "full")
)

model_results_1950_state_fe_out <- model_results_1950_state_fe %>%
 drop_state_fe_terms() %>%
 mutate(
  across(
   c(estimate, std.error, statistic, p.value, r_squared, adj_r_squared),
   ~ round(.x, 6)
  )
 )

robust_se_1950_state_fe <- map(
 models_1950_state_fe[c("baseline", "extended", "full")],
 \(model) sqrt(diag(sandwich::vcovHC(model, type = "HC1")))
)

dependent_mean_1950_state_fe <- map_dbl(
 models_1950_state_fe[c("baseline", "extended", "full")],
 \(model) mean(stats::model.response(stats::model.frame(model)), na.rm = TRUE)
)

state_fe_1950_tex_path <- results_file_path(
 "county_inventor_determinants_models_1900_1950_state_fe.tex"
)

m1 <- models_1950_state_fe$baseline
m2 <- models_1950_state_fe$extended
m3 <- models_1950_state_fe$full

stargazer::stargazer(
 m1,
 m2,
 m3,
 type = "latex",
 out = state_fe_1950_tex_path,
 se = robust_se_1950_state_fe,
 title = "Determinants of County Inventor Growth with State Fixed Effects",
 dep.var.labels = "Change in inventors per 100k, 1900-1950",
 column.labels = c("Baseline", "Extended", "Full"),
 covariate.labels = c(
  "Frontier years, XIX",
  "Cropland km2, mean XIX",
  "Grazeland km2, mean XIX",
  "Canal access, mean XIX",
  "Sex ratio, mean XIX",
  "Post offices, mean XIX",
  "Manufacturing output (million 1900 dollars), mean XIX",
  "Farming output (million 1900 dollars), mean XIX",
  "Immigrant share, mean XIX",
  "Slave share, mean XIX",
  "Illiterate share, mean XIX",
  "HYDE population (thousands), mean XIX",
  "Change in inventors per 100k, 1800-1890"
 ),
 omit = "factor\\(state_fips\\)",
 omit.stat = c("f", "ser"),
 add.lines = list(
  c("State fixed effects", "Yes", "Yes", "Yes"),
  c("Mean dependent variable", sprintf("%.3f", dependent_mean_1950_state_fe))
 ),
 no.space = TRUE,
 notes = "Heteroskedasticity-robust HC1 standard errors in parentheses.",
 notes.align = "l"
)

write_csv(
 model_results_1950_state_fe_out,
 results_file_path("county_inventor_determinants_models_1900_1950_state_fe.csv"),
 na = ""
)

write_html_table(
 model_results_1950_state_fe_out,
 results_file_path("county_inventor_determinants_models_1900_1950_state_fe.html"),
 "County Inventor Growth Determinants with State Fixed Effects, 1900-1950"
)

combined_models_1950 <- c(
 models_1950[c("baseline", "extended", "full")],
 models_1950_state_fe[c("baseline", "extended", "full")]
)
combined_robust_se_1950 <- c(robust_se_1950, robust_se_1950_state_fe)
combined_dependent_mean_1950 <- c(
 dependent_mean_1950,
 dependent_mean_1950_state_fe
)

combined_results_1950_out <- bind_rows(
 model_results_1950 %>% mutate(state_fixed_effects = "No"),
 model_results_1950_state_fe %>%
  drop_state_fe_terms() %>%
  mutate(state_fixed_effects = "Yes")
) %>%
 mutate(
  across(
   c(estimate, std.error, statistic, p.value, r_squared, adj_r_squared),
   ~ round(.x, 6)
  )
 )

combined_1950_tex_path <- results_file_path(
 "county_inventor_determinants_models_1900_1950_combined.tex"
)

d1 <- combined_models_1950[[1]]
d2 <- combined_models_1950[[2]]
d3 <- combined_models_1950[[3]]
d4 <- combined_models_1950[[4]]
d5 <- combined_models_1950[[5]]
d6 <- combined_models_1950[[6]]

stargazer::stargazer(
 d1,
 d2,
 d3,
 d4,
 d5,
 d6,
 type = "latex",
 out = combined_1950_tex_path,
 se = combined_robust_se_1950,
 title = "Determinants of County Inventor Growth",
 dep.var.labels = "Change in inventors per 100k, 1900-1950",
 column.labels = c("Baseline", "Extended", "Full", "Baseline", "Extended", "Full"),
 covariate.labels = covariate_labels,
 omit = "factor\\(state_fips\\)",
 omit.stat = c("f", "ser"),
 add.lines = list(
  c("State fixed effects", "No", "No", "No", "Yes", "Yes", "Yes"),
  c("Mean dependent variable", sprintf("%.3f", combined_dependent_mean_1950))
 ),
 no.space = TRUE,
 font.size = "scriptsize",
 notes = "Heteroskedasticity-robust HC1 standard errors in parentheses.",
 notes.align = "l"
)

write_csv(
 combined_results_1950_out,
 results_file_path("county_inventor_determinants_models_1900_1950_combined.csv"),
 na = ""
)

write_html_table(
 combined_results_1950_out,
 results_file_path("county_inventor_determinants_models_1900_1950_combined.html"),
 "County Inventor Growth Determinants, Combined State FE Specifications, 1900-1950"
)

combined_1950_wrapper_path <- wrapper_results_file_path(
 "county_inventor_determinants_models_1900_1950_combined_wrapper.tex"
)
write_latex_wrapper(combined_1950_tex_path, combined_1950_wrapper_path)
compile_latex_wrapper(combined_1950_wrapper_path)

###############################################################################
# Figure
###############################################################################

scatter_plot <- ggplot(
 analysis_data,
 aes(
  x = frontier_years_1800_1890,
  y = delta_inventors_per_100k_1900_2000
 )
) +
 geom_point(alpha = 0.35, size = 1) +
 geom_smooth(method = "lm", se = TRUE, color = "steelblue", linewidth = 0.8) +
 labs(
  x = "Nineteenth-century frontier exposure (years)",
  y = "Change in inventors per 100k, 1900-2000",
  title = "Frontier Exposure and Inventor Growth"
 ) +
 theme_minimal(base_size = 12)

ggsave(
 results_file_path("county_inventor_growth_frontier_scatter.png"),
 plot = scatter_plot,
 width = 8,
 height = 5,
 dpi = 300
)

cleanup_wrapper_intermediates()

###############################################################################
# Console summary
###############################################################################

message("Saved analysis data: ", output_file_path("county_inventor_determinants_analysis.csv"))
message("Saved model CSV: ", results_file_path("county_inventor_determinants_models.csv"))
message("Saved model HTML: ", results_file_path("county_inventor_determinants_models.html"))
message("Saved model TeX: ", results_file_path("county_inventor_determinants_models.tex"))
message("Saved state FE model CSV: ", results_file_path("county_inventor_determinants_models_state_fe.csv"))
message("Saved state FE model HTML: ", results_file_path("county_inventor_determinants_models_state_fe.html"))
message("Saved state FE model TeX: ", results_file_path("county_inventor_determinants_models_state_fe.tex"))
message("Saved combined model CSV: ", results_file_path("county_inventor_determinants_models_combined.csv"))
message("Saved combined model HTML: ", results_file_path("county_inventor_determinants_models_combined.html"))
message("Saved combined model TeX: ", results_file_path("county_inventor_determinants_models_combined.tex"))
message("Saved combined model PDF: ", wrapper_results_file_path("county_inventor_determinants_models_combined_wrapper.pdf"))
message("Saved 1900-1960 mean model CSV: ", results_file_path("county_inventor_determinants_models_mean_1900_1960.csv"))
message("Saved 1900-1960 mean model HTML: ", results_file_path("county_inventor_determinants_models_mean_1900_1960.html"))
message("Saved 1900-1960 mean model TeX: ", results_file_path("county_inventor_determinants_models_mean_1900_1960.tex"))
message("Saved 1900-1960 mean state FE model CSV: ", results_file_path("county_inventor_determinants_models_mean_1900_1960_state_fe.csv"))
message("Saved 1900-1960 mean state FE model HTML: ", results_file_path("county_inventor_determinants_models_mean_1900_1960_state_fe.html"))
message("Saved 1900-1960 mean state FE model TeX: ", results_file_path("county_inventor_determinants_models_mean_1900_1960_state_fe.tex"))
message("Saved 1900-1960 mean combined model CSV: ", results_file_path("county_inventor_determinants_models_mean_1900_1960_combined.csv"))
message("Saved 1900-1960 mean combined model HTML: ", results_file_path("county_inventor_determinants_models_mean_1900_1960_combined.html"))
message("Saved 1900-1960 mean combined model TeX: ", results_file_path("county_inventor_determinants_models_mean_1900_1960_combined.tex"))
message("Saved 1900-1960 mean combined model PDF: ", wrapper_results_file_path("county_inventor_determinants_models_mean_1900_1960_combined_wrapper.pdf"))
message("Saved 1900-1960 mean alternative specs CSV: ", results_file_path("county_inventor_determinants_models_mean_1900_1960_alt_specs.csv"))
message("Saved 1900-1960 mean alternative specs HTML: ", results_file_path("county_inventor_determinants_models_mean_1900_1960_alt_specs.html"))
message("Saved 1900-1960 mean alternative specs TeX: ", results_file_path("county_inventor_determinants_models_mean_1900_1960_alt_specs.tex"))
message("Saved 1900-1960 mean alternative specs PDF: ", wrapper_results_file_path("county_inventor_determinants_models_mean_1900_1960_alt_specs_wrapper.pdf"))
message("Saved 1900-1950 model CSV: ", results_file_path("county_inventor_determinants_models_1900_1950.csv"))
message("Saved 1900-1950 model HTML: ", results_file_path("county_inventor_determinants_models_1900_1950.html"))
message("Saved 1900-1950 model TeX: ", results_file_path("county_inventor_determinants_models_1900_1950.tex"))
message("Saved 1900-1950 state FE model CSV: ", results_file_path("county_inventor_determinants_models_1900_1950_state_fe.csv"))
message("Saved 1900-1950 state FE model HTML: ", results_file_path("county_inventor_determinants_models_1900_1950_state_fe.html"))
message("Saved 1900-1950 state FE model TeX: ", results_file_path("county_inventor_determinants_models_1900_1950_state_fe.tex"))
message("Saved 1900-1950 combined model CSV: ", results_file_path("county_inventor_determinants_models_1900_1950_combined.csv"))
message("Saved 1900-1950 combined model HTML: ", results_file_path("county_inventor_determinants_models_1900_1950_combined.html"))
message("Saved 1900-1950 combined model TeX: ", results_file_path("county_inventor_determinants_models_1900_1950_combined.tex"))
message("Saved 1900-1950 combined model PDF: ", wrapper_results_file_path("county_inventor_determinants_models_1900_1950_combined_wrapper.pdf"))
message("Saved scatter plot: ", results_file_path("county_inventor_growth_frontier_scatter.png"))

message("Analytic counties: ", n_distinct(analysis_data$GEOID))
message(
 "Missing outcome: ",
 sum(is.na(analysis_data$delta_inventors_per_100k_1900_2000))
)
message(
 "Missing outcome, 1900-1950: ",
 sum(is.na(analysis_data$delta_inventors_per_100k_1900_1950))
)
message(
 "Missing outcome, mean 1900-1960: ",
 sum(is.na(analysis_data$mean_inventors_per_100k_hyde_1900_1960))
)

missing_summary <- analysis_data %>%
 summarise(
 across(
   matches("(_mean_1800_1890|frontier_years_1800_1890|delta_inventors_per_100k_hyde_1800_1890|mean_inventors_per_100k_hyde_1900_1960)$"),
   ~ mean(is.na(.x)) * 100
  )
 ) %>%
 pivot_longer(everything(), names_to = "variable", values_to = "missing_pct")

print(missing_summary, n = Inf)

model_nobs <- tibble(
 model = names(models),
 nobs = map_int(models, stats::nobs)
)

print(model_nobs)
