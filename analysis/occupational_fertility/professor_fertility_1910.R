# Fertility of US college professors in 1910, counted two ways.
#
#   Way 1, mother-reported: the wife's CHSURV (children surviving) and CHBORN
#   (ever born), linked through SPLOC. Counts children wherever they live.
#     (a) wives aged 40-49, near-completed fertility
#     (b) wife's age 20-49 in 5-year bins, directly standardised to all wives
#     (c) marriage duration 0-29 in 5-year bins, directly standardised to all
#         couples; professors married late, so (b) mixes later marriage with
#         lower marital fertility and (c) isolates the second
#   Way 2, father-reported: own children under 5 / under 10 in the household
#   (POPLOC), for every man aged 25-54 in a household, age-standardised.
#   Then the two are reconciled on the couples file, and female professors are
#   profiled separately.
#
# Inputs:  <Dropbox>/output/occupational_fertility/ (build_professor_fertility.R)
# Outputs: <Dropbox>/results/occupational_fertility/
#
# Run from any working directory:
#   Rscript analysis/occupational_fertility/professor_fertility_1910.R

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(fixest)
})

repo_root <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  } else {
    Sys.getenv("GTL_REPO", unset = getwd())
  }
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not locate the repo root (no paths.R found)")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
})
data_root <- local({
  env <- new.env()
  suppressWarnings(try(sys.source(file.path(repo_root, "paths.R"), envir = env), silent = TRUE))
  candidates <- c(
    get0("TALENT_DETS_DATA_DIR", envir = env, ifnotfound = ""),
    Sys.getenv("DET_DIR"),
    file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox/gtl_talent_dets")
  )
  hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
  if (!length(hit)) stop("Could not locate the Dropbox data root. Set TALENT_DETS_DATA_DIR or DET_DIR.")
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
})
source(file.path(repo_root, "analysis/occupational_fertility/fertility_helpers.R"))

IN_DIR <- Sys.getenv("OCC_FERTILITY_OUT_DIR",
                     unset = file.path(data_root, "output/occupational_fertility"))
OUT_DIR <- Sys.getenv("OCC_FERTILITY_RESULTS_DIR",
                      unset = file.path(data_root, "results/occupational_fertility"))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
setDTthreads(8L)

say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
write_out <- function(x, name) {
  fwrite(x, file.path(OUT_DIR, name))
  say("wrote ", name)
}

GROUP_ORDER <- c("professor", "professor_plus_deans", "pres_dean", "physician", "lawyer",
                 "clergy", "teacher", "engineer", "natural_scientist", "other_professional",
                 "all_professional", "farmer", "manager", "clerical", "sales", "craftsman",
                 "operative", "service", "farm_laborer", "laborer", "no_occupation", "all")
PROFESSIONAL <- c("professor", "pres_dean", "physician", "lawyer", "clergy", "teacher",
                  "engineer", "natural_scientist", "other_professional")

# Cell table (group x stratum: n, total) with the pooled groups added: every
# couple, professors with presidents/deans, and every professional code.
cells_for <- function(d, outcome, stratum) {
  d <- d[!is.na(get(outcome)) & !is.na(get(stratum))]
  cells <- d[, .(n = .N, total = sum(get(outcome))), by = .(group = occ_group, stratum = get(stratum))]
  pooled <- function(name, members) {
    cells[group %in% members, .(group = name, n = sum(n), total = sum(total)), by = stratum]
  }
  rbind(cells,
        pooled("professor_plus_deans", c("professor", "pres_dean")),
        pooled("all_professional", PROFESSIONAL),
        pooled("all", unique(cells$group)), use.names = TRUE)
}

standardise <- function(d, outcomes, stratum) {
  rbindlist(lapply(outcomes, function(outcome) {
    cells <- cells_for(d, outcome, stratum)
    standard <- cells[group == "all", .(stratum, weight = n)]
    out <- as.data.table(direct_standardise(as.data.frame(cells), as.data.frame(standard)))
    out[, outcome := outcome]
  }))
}

order_groups <- function(x) {
  x[, group := factor(group, levels = GROUP_ORDER)]
  setorderv(x, intersect(c("outcome", "group"), names(x)))
  x[, group := as.character(group)]
  x
}

# Ratios of each group's value to the two reference groups.
add_ratios <- function(x, value = "std_mean") {
  x[, ratio_to_all := get(value) / get(value)[group == "all"], by = outcome]
  x[, ratio_to_other_professional := get(value) / get(value)[group == "other_professional"],
    by = outcome]
  x
}

# --------------------------------------------------------------------------- #
# Way 1 — wife-linked CHSURV / CHBORN
# --------------------------------------------------------------------------- #
say("Reading couples")
couples <- as.data.table(read_parquet(
  file.path(IN_DIR, "couples_1910.parquet"),
  col_select = c("STATEFIP", "COUNTYICP", "URBAN", "occ_group", "h_age", "w_age",
                 "h_race", "h_us_born", "w_us_born", "chborn", "chsurv", "durmarr",
                 "w_age_at_marriage", "h_age_at_marriage", "h_own_children_all",
                 "h_own_children_u10", "w_own_children_all")))
say("Couples: ", format(nrow(couples), big.mark = ","))
couples[, `:=`(childless = as.integer(chborn == 0L),
               w_age5 = five_year_bin(w_age, 15L, 79L),
               dur5 = five_year_bin(durmarr, 0L, 29L),
               race3 = fifelse(h_race == 1L, "white", fifelse(h_race == 2L, "black", "other")),
               urban = as.integer(URBAN == 2L))]

outcomes1 <- c("chsurv", "chborn", "childless")

say("(a) wives aged 40-49")
a <- couples[w_age >= 40L & w_age <= 49L]
completed <- rbindlist(lapply(c(outcomes1, "w_age_at_marriage", "h_age_at_marriage"), function(outcome) {
  d <- a[!is.na(get(outcome))]
  by_group <- d[, .(n = .N, mean = mean(get(outcome)), sd = sd(get(outcome))), by = .(group = occ_group)]
  pool <- function(name, rows) d[rows, .(group = name, n = .N, mean = mean(get(outcome)),
                                         sd = sd(get(outcome)))]
  out <- rbind(by_group,
               pool("professor_plus_deans", d$occ_group %in% c("professor", "pres_dean")),
               pool("all_professional", d$occ_group %in% PROFESSIONAL),
               pool("all", rep(TRUE, nrow(d))))
  out[, `:=`(se = sd / sqrt(n), outcome = outcome)]
}))
completed <- add_ratios(order_groups(completed), "mean")
survival <- a[!is.na(chborn), .(n = .N, survival_ratio = sum(chsurv) / sum(chborn)), by = .(group = occ_group)]
survival <- rbind(survival,
                  a[!is.na(chborn) & occ_group %in% c("professor", "pres_dean"),
                    .(group = "professor_plus_deans", n = .N, survival_ratio = sum(chsurv) / sum(chborn))],
                  a[!is.na(chborn) & occ_group %in% PROFESSIONAL,
                    .(group = "all_professional", n = .N, survival_ratio = sum(chsurv) / sum(chborn))],
                  a[!is.na(chborn), .(group = "all", n = .N, survival_ratio = sum(chsurv) / sum(chborn))])
write_out(completed, "way1_a_wives_40_49.csv")
write_out(order_groups(survival), "way1_a_child_survival_ratio_40_49.csv")

say("(b) age-standardised, wives 20-49")
b <- couples[w_age >= 20L & w_age <= 49L]
age_std <- add_ratios(order_groups(standardise(b, outcomes1, "w_age5")))
write_out(age_std, "way1_b_age_standardised.csv")

say("(c) duration-standardised, marriage 0-29 years")
c_dur <- couples[!is.na(dur5) & w_age <= 49L]
dur_std <- add_ratios(order_groups(standardise(c_dur, outcomes1, "dur5")))
write_out(dur_std, "way1_c_duration_standardised.csv")

say("Poisson IRRs, wives 15-49 with known duration")
reg <- couples[w_age >= 15L & w_age <= 49L & !is.na(dur5)]
reg[, `:=`(professor = as.integer(occ_group == "professor"),
           pres_dean = as.integer(occ_group == "pres_dean"),
           county = paste(STATEFIP, COUNTYICP))]
irr_table <- function(models, term, sample, reference) {
  rbindlist(lapply(names(models), function(name) {
    ct <- coeftable(models[[name]])
    rows <- grep(term, rownames(ct))
    data.table(model = name, sample = sample, reference = reference,
               term = rownames(ct)[rows], coef = ct[rows, 1], se = ct[rows, 2],
               irr = exp(ct[rows, 1]), irr_lo = exp(ct[rows, 1] - 1.96 * ct[rows, 2]),
               irr_hi = exp(ct[rows, 1] + 1.96 * ct[rows, 2]), nobs = nobs(models[[name]]))
  }))
}
controls <- "h_us_born + w_us_born + i(race3, ref = 'white') + urban"
fit_all <- function(lhs, fe) fepois(as.formula(paste(lhs, "~ professor + pres_dean +", controls,
                                                       "|", fe)), data = reg, cluster = ~STATEFIP)
models_all <- list()
for (lhs in c("chsurv", "chborn")) {
  models_all[[paste0(lhs, "_state_fe")]] <- fit_all(lhs, "w_age5 + dur5 + STATEFIP")
  models_all[[paste0(lhs, "_county_fe")]] <- fit_all(lhs, "w_age5 + dur5 + county")
}
irr1 <- irr_table(models_all, "^professor$|^pres_dean$", "all couples", "all other married men")
prof_reg <- reg[occ_group %in% PROFESSIONAL]
models_prof <- list()
for (lhs in c("chsurv", "chborn")) {
  models_prof[[paste0(lhs, "_state_fe")]] <- fepois(
    as.formula(paste(lhs, "~ i(occ_group, ref = 'other_professional') +", controls,
                     "| w_age5 + dur5 + STATEFIP")), data = prof_reg, cluster = ~STATEFIP)
}
irr1 <- rbind(irr1, irr_table(models_prof, "^occ_group::", "professional couples",
                              "other_professional"))
write_out(irr1, "way1_poisson_irr.csv")
rm(reg, prof_reg, models_all, models_prof); invisible(gc(verbose = FALSE))

say("Reconciling the two ways on couples with wives 20-49")
recon <- couples[w_age >= 20L & w_age <= 49L]
recon[, group := fifelse(occ_group %in% c("professor", "pres_dean"), "professor_plus_deans",
                         fifelse(occ_group %in% PROFESSIONAL, "other_professional_codes", "non_professional"))]
reconciliation <- recon[!is.na(chsurv), .(
  n = .N,
  mean_chsurv = mean(chsurv),
  mean_husband_own_children = mean(h_own_children_all),
  mean_wife_own_children = mean(w_own_children_all),
  share_husband_above_chsurv = mean(h_own_children_all > chsurv),
  share_wife_above_chsurv = mean(w_own_children_all > chsurv)),
  by = .(group, w_age5)]
setorder(reconciliation, group, w_age5)
write_out(reconciliation, "reconciliation_coresident_vs_chsurv.csv")
rm(recon, a, b, c_dur); invisible(gc(verbose = FALSE))

# --------------------------------------------------------------------------- #
# Way 2 — father-reported own children (POPLOC)
# --------------------------------------------------------------------------- #
say("Reading men")
men <- as.data.table(read_parquet(
  file.path(IN_DIR, "men_own_children_1910.parquet"),
  col_select = c("STATEFIP", "URBAN", "in_household", "AGE", "MARST", "RACE", "us_born",
                 "occ_group", "own_children_u5", "own_children_u10",
                 "own_children_u5_bio", "own_children_u10_bio")))
men <- men[in_household == TRUE & AGE >= 25L & AGE <= 54L]
men[, `:=`(age5 = five_year_bin(AGE, 25L, 54L),
           race3 = fifelse(RACE == 1L, "white", fifelse(RACE == 2L, "black", "other")),
           urban = as.integer(URBAN == 2L),
           professor = as.integer(occ_group == "professor"),
           pres_dean = as.integer(occ_group == "pres_dean"))]
say("Men 25-54 in households: ", format(nrow(men), big.mark = ","))
outcomes2 <- c("own_children_u5", "own_children_u10", "own_children_u5_bio", "own_children_u10_bio")
way2 <- rbind(
  standardise(men, outcomes2, "age5")[, sample := "all men"],
  standardise(men[MARST == 1L], outcomes2, "age5")[, sample := "married, spouse present"])
way2 <- order_groups(way2)
way2[, ratio_to_all := std_mean / std_mean[group == "all"], by = .(outcome, sample)]
way2[, ratio_to_other_professional := std_mean / std_mean[group == "other_professional"],
     by = .(outcome, sample)]
setcolorder(way2, c("sample", "outcome"))
write_out(way2, "way2_own_children_age_standardised.csv")

say("Poisson IRRs, own children under 10")
irr2 <- list()
for (smp in c("all men", "married, spouse present")) {
  d <- if (smp == "all men") men else men[MARST == 1L]
  m <- fepois(as.formula(paste("own_children_u10 ~ professor + pres_dean + us_born +",
                               "i(race3, ref = 'white') + urban | age5 + STATEFIP")),
              data = d, cluster = ~STATEFIP)
  irr2[[smp]] <- irr_table(list(own_children_u10 = m), "^professor$|^pres_dean$", smp,
                           "all other men")
  mp <- fepois(as.formula(paste("own_children_u10 ~ i(occ_group, ref = 'other_professional') +",
                                "us_born + i(race3, ref = 'white') + urban | age5 + STATEFIP")),
               data = d[occ_group %in% PROFESSIONAL], cluster = ~STATEFIP)
  irr2[[paste(smp, "prof")]] <- irr_table(list(own_children_u10 = mp), "^occ_group::",
                                          paste(smp, "(professionals)"), "other_professional")
}
write_out(rbindlist(irr2), "way2_poisson_irr.csv")
rm(men); invisible(gc(verbose = FALSE))

# --------------------------------------------------------------------------- #
# Female professors
# --------------------------------------------------------------------------- #
say("Female professors")
fp <- fread(file.path(IN_DIR, "female_professors_1910.csv"))
fp <- fp[occ_group == "professor"]
fp[, age_band := cut(AGE, c(-Inf, 24, 34, 44, 54, Inf),
                     labels = c("<25", "25-34", "35-44", "45-54", "55+"))]
fp[, marst_label := c("married, spouse present", "married, spouse absent", "separated",
                      "divorced", "widowed", "never married")[MARST]]
female <- fp[, .(n = .N, n_fertility_known = sum(!is.na(chsurv)),
                 mean_chsurv = mean(chsurv, na.rm = TRUE), mean_chborn = mean(chborn, na.rm = TRUE)),
             by = .(marst_label, age_band)]
setorder(female, marst_label, age_band)
write_out(female, "female_professors_fertility.csv")
profile <- rbind(
  fp[, .(variable = "age_band", value = as.character(age_band))][, .N, by = .(variable, value)],
  fp[, .(variable = "GQ", value = as.character(GQ))][, .N, by = .(variable, value)],
  fp[, .(variable = "MARST", value = marst_label)][, .N, by = .(variable, value)],
  fp[, .(variable = "OCC1950", value = sprintf("%03d", OCC1950))][, .N, by = .(variable, value)],
  fp[, .(variable = "urban", value = as.character(URBAN))][, .N, by = .(variable, value)])
profile[, share := N / sum(N), by = variable]
write_out(profile, "female_professors_profile.csv")

# --------------------------------------------------------------------------- #
# Summary
# --------------------------------------------------------------------------- #
pick <- function(x, g, o, col = "std_mean") x[group == g & outcome == o][[col]]
fmt <- function(v, d = 2) formatC(v, format = "f", digits = d)
lines <- c(
  "# Professor fertility, 1910 full count", "",
  paste0("Couples: ", format(nrow(couples), big.mark = ","), "; professor couples: ",
         format(sum(couples$occ_group == "professor"), big.mark = ","), "."), "",
  "| Measure | Professors | All married men | Other professionals | Ratio to all |",
  "|---|---:|---:|---:|---:|")
row <- function(label, x, o, col = "std_mean") {
  sprintf("| %s | %s | %s | %s | %s |", label, fmt(pick(x, "professor", o, col)),
          fmt(pick(x, "all", o, col)), fmt(pick(x, "other_professional", o, col)),
          fmt(pick(x, "professor", o, col) / pick(x, "all", o, col)))
}
lines <- c(lines,
  row("(a) CHSURV, wives 40-49", completed, "chsurv", "mean"),
  row("(a) CHBORN, wives 40-49", completed, "chborn", "mean"),
  row("(a) childless, wives 40-49", completed, "childless", "mean"),
  row("(a) wife's age at marriage, 40-49", completed, "w_age_at_marriage", "mean"),
  row("(b) CHSURV, age-standardised 20-49", age_std, "chsurv"),
  row("(b) CHBORN, age-standardised 20-49", age_std, "chborn"),
  row("(c) CHSURV, duration-standardised", dur_std, "chsurv"),
  row("(c) CHBORN, duration-standardised", dur_std, "chborn"),
  row("Way 2: own children <10, men 25-54", way2[sample == "all men"], "own_children_u10"),
  row("Way 2: own children <10, married men", way2[sample == "married, spouse present"],
      "own_children_u10"),
  "",
  "Poisson IRRs, professor vs all other married men (state FE, clustered by state):", "")
for (i in seq_len(nrow(irr1[term == "professor"]))) {
  r <- irr1[term == "professor"][i]
  lines <- c(lines, sprintf("- %s: IRR %s [%s, %s], N = %s", r$model, fmt(r$irr, 3),
                            fmt(r$irr_lo, 3), fmt(r$irr_hi, 3), format(r$nobs, big.mark = ",")))
}
writeLines(lines, file.path(OUT_DIR, "summary.md"))
say("Done: ", OUT_DIR)
