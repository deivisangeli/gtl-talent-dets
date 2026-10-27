# Finish a completely built monthly stage after validating current source hashes.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
repo_root <- Sys.getenv("GTL_REPO", getwd())
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "prep/mandatory_schooling/monthly_amws_helpers.R"))
source(file.path(repo_root, "prep/mandatory_schooling/amws_panel_gender_helpers.R"))
stage <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
dest <- normalizePath(file.path(DATA_OUTPUT, "mandatory_schooling"), winslash = "/")
stopifnot(startsWith(stage, paste0(dest, "/gender_monthly_stage_")))
audit <- file.path(stage, "monthly_gapminder_1800_1960_audits")
manifest <- fread(file.path(audit, "input_manifest.csv"))
stopifnot(identical(unname(tools::md5sum(manifest$file)), manifest$md5))
annual_name <- "county_gapminder_annual_births_1800_1960.csv"
monthly_name <- "county_amws_monthly_gapminder_1800_1960.csv"
annual <- fread(file.path(stage, annual_name))
amws_validate_gender_annual(annual)
stopifnot(nrow(annual) == 3092L * 161L,
          sum(annual$n_early_year + annual$n_1986_year) ==
            sum(annual$n_early_assigned + annual$n_1986_assigned + annual$n_unassigned_month))
reconcile <- fread(file.path(audit, "monthly_annual_reconciliation.csv"))
stopifnot(all(reconcile$early_month_sum == reconcile$early_assigned),
          all(reconcile$late_month_sum == reconcile$late_assigned),
          all(is.na(reconcile$annual_births) | abs(reconcile$monthly_birth_sum - reconcile$annual_births) < 1e-6))
Sys.setenv(AMWS_PANEL_SOURCE_REFRESH = "TRUE", AMWS_PANEL_VALIDATION_ENGINE = "duckdb")
amws_verify_staged_panel(file.path(stage, annual_name), file.path(dest, annual_name), nrow(annual))
amws_verify_staged_panel(file.path(stage, monthly_name), file.path(dest, monthly_name), nrow(annual) * 12L)
stopifnot(identical(unname(tools::md5sum(manifest$file)), manifest$md5))
amws_install_staged(stage, dest)
cat("Installed source-verified monthly refresh.\n")
