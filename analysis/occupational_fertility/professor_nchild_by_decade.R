# Professor fertility by census, 1850-1910, on NCHILD (own children co-resident).
#
# Headline: mean NCHILD of male professors aged 25-54 in households, directly
# age-standardised (5-year bins) to ONE standard for every year, the age mix of
# all men aged 25-54 pooled over the six censuses, so a change across decades
# cannot come from a changing age mix. Compared with all men, men with an
# occupation and other professionals.
#
# Companions: married men only (SPLOC > 0; MARST is blank before 1880), own
# children under 5 (child-father ratio), biological variants, Poisson IRRs by
# year fitted on cells with an offset of log(n), the 1900/1910 calibration
# against the wife's CHSURV/CHBORN, and an audit of the professor definition.
#
# Caveats carried into summary.md: NCHILD counts co-resident children of any
# age, so it mixes fertility, child mortality and leaving home; before 1880 it
# rests on IPUMS-imputed relationships; 1890 is missing; and the professor code
# is not definitionally stable (1910 has 3.3x the professors of 1900).
#
# Inputs:  <Dropbox>/output/occupational_fertility/decades/ (build_professor_nchild_decades.R)
# Outputs: <Dropbox>/results/occupational_fertility/decades/
#
# Run from any working directory:
#   Rscript analysis/occupational_fertility/professor_nchild_by_decade.R

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(fixest)
  library(ggplot2)
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
source(file.path(repo_root, "prep/occupational_fertility/occupation_groups.R"))

IN_DIR <- Sys.getenv("OCC_FERTILITY_DECADES_OUT_DIR",
                     unset = file.path(data_root, "output/occupational_fertility/decades"))
OUT_DIR <- Sys.getenv("OCC_FERTILITY_DECADES_RESULTS_DIR",
                      unset = file.path(data_root, "results/occupational_fertility/decades"))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
setDTthreads(8L)

say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
write_out <- function(x, name) {
  fwrite(x, file.path(OUT_DIR, name))
  say("wrote ", name)
}
fmt <- function(v, d = 2) formatC(v, format = "f", digits = d)

GROUP_ORDER <- c("professor", "professor_plus_deans", "pres_dean", "physician", "lawyer",
                 "clergy", "teacher", "engineer", "natural_scientist", "other_professional",
                 "all_professional", "farmer", "manager", "clerical", "sales", "craftsman",
                 "operative", "service", "farm_laborer", "laborer", "no_occupation",
                 "all_occupied", "all")
POOLS <- list(professor_plus_deans = c("professor", "pres_dean"),
              all_professional = PROFESSIONAL,
              all_occupied = setdiff(OCC_GROUPS$group, "no_occupation"),
              all = OCC_GROUPS$group)
OUTCOMES <- c(nchild = "sum_nchild", own_u5 = "sum_own_u5", own_u10 = "sum_own_u10",
              own_u5_bio = "sum_own_u5_bio", own_u10_bio = "sum_own_u10_bio")

# --------------------------------------------------------------------------- #
# Cells
# --------------------------------------------------------------------------- #
say("Reading men's cells")
cells <- as.data.table(open_dataset(file.path(IN_DIR, "men_cells")) |> dplyr::collect())
cells[, YEAR := as.integer(YEAR)]
cells <- cells[in_household == TRUE & AGE >= 25L & AGE <= 54L]
cells[, age5 := paste0(25L + 5L * ((AGE - 25L) %/% 5L), "-", 29L + 5L * ((AGE - 25L) %/% 5L))]
years <- sort(unique(cells$YEAR))
say("Men 25-54 in households: ", format(sum(cells$n), big.mark = ","), " across ",
    paste(years, collapse = ", "))

# One fixed age standard for every year: all men 25-54 pooled over the censuses.
standard <- cells[, .(weight = sum(n)), by = age5][, weight := weight / sum(weight)]

# Year x sample x group x age5 sums, with the pooled groups added.
age_cells <- function(sample_filter) {
  d <- sample_filter(cells)
  base <- d[, c(list(n = sum(n), sum_nchild_sq = sum(sum_nchild_sq)),
                lapply(.SD, sum)), by = .(YEAR, group = occ_group, age5), .SDcols = OUTCOMES]
  pooled <- rbindlist(lapply(names(POOLS), function(name) {
    base[group %in% POOLS[[name]], c(list(n = sum(n), sum_nchild_sq = sum(sum_nchild_sq)),
                                     lapply(.SD, sum)), by = .(YEAR, age5), .SDcols = OUTCOMES][
      , group := name]
  }), use.names = TRUE)
  rbind(base, pooled, use.names = TRUE)
}

# Directly standardised mean per year x group, with an SE where the cells carry
# the sum of squares (NCHILD only). A group missing an age bin gets NA.
standardise_year <- function(ac, outcome) {
  col <- OUTCOMES[[outcome]]
  ac <- merge(ac, standard, by = "age5")
  ac[, rate := get(col) / n]
  out <- ac[, .(n = sum(n), crude = sum(get(col)) / sum(n),
                std = if (.N == nrow(standard)) sum(weight * rate) else NA_real_,
                std_se = if (outcome == "nchild" && .N == nrow(standard) && all(n > 1)) {
                  v <- (sum_nchild_sq / n - rate^2) * n / (n - 1)
                  sqrt(sum(weight^2 * v / n))
                } else NA_real_),
            by = .(YEAR, group)]
  out[, outcome := outcome]
}

# Ratios to the reference groups; the log-ratio SE treats the two means as
# independent (the professor share of "all" is under 0.1%).
add_ratios <- function(x) {
  for (ref in c("all", "all_occupied", "other_professional")) {
    x[, (paste0("ratio_to_", ref)) := std / std[group == ref], by = .(YEAR, outcome)]
    x[, (paste0("ratio_se_log_", ref)) := sqrt((std_se / std)^2 +
                                                 (std_se[group == ref] / std[group == ref])^2),
      by = .(YEAR, outcome)]
  }
  x[, group := factor(group, levels = GROUP_ORDER)]
  setorder(x, outcome, group, YEAR)
  x[, group := as.character(group)]
  x
}

say("Standardising")
samples <- list(`all men` = function(d) d,
                `married, spouse present` = function(d) d[married_sp == TRUE])
std_all <- rbindlist(lapply(names(samples), function(s) {
  ac <- age_cells(samples[[s]])
  out <- rbindlist(lapply(names(OUTCOMES), function(o) standardise_year(ac, o)))
  add_ratios(out)[, sample := s]
}))
setcolorder(std_all, c("sample", "outcome", "group", "YEAR"))
write_out(std_all[outcome == "nchild"], "nchild_age_standardised_by_year.csv")
write_out(std_all[outcome != "nchild"], "own_children_age_standardised_by_year.csv")

# Wide professor view, the table most readers want.
prof_wide <- dcast(std_all[group %in% c("professor", "all", "all_occupied", "other_professional")],
                   sample + outcome + YEAR ~ group, value.var = "std")
prof_wide <- merge(prof_wide, std_all[group == "professor",
                                      .(sample, outcome, YEAR, n_professors = n,
                                        ratio_to_all, ratio_to_all_occupied,
                                        ratio_to_other_professional)],
                   by = c("sample", "outcome", "YEAR"))
write_out(prof_wide, "professor_by_year_wide.csv")

# --------------------------------------------------------------------------- #
# Poisson IRRs by year, fitted on cells
# --------------------------------------------------------------------------- #
say("Poisson IRRs by year")
reg_cells <- cells[, c(list(n = sum(n)), lapply(.SD, sum)),
                   by = .(YEAR, occ_group, age5, married_sp, us_born, race3, urban, STATEFIP),
                   .SDcols = OUTCOMES]
reg_cells[, `:=`(professor = as.integer(occ_group == "professor"),
                 pres_dean = as.integer(occ_group == "pres_dean"),
                 us_born = fifelse(is.na(us_born), "unknown", as.character(us_born)),
                 race3 = fifelse(is.na(race3), "unknown", race3),
                 urban = fifelse(is.na(urban), "unknown", as.character(urban)),
                 YEAR = factor(YEAR))]
irr_rows <- function(model, label) {
  ct <- coeftable(model)
  rows <- grep("^YEAR::[0-9]+:(professor|pres_dean)$", rownames(ct))
  term <- rownames(ct)[rows]
  data.table(model = label, YEAR = as.integer(sub("^YEAR::([0-9]+):.*$", "\\1", term)),
             term = sub("^YEAR::[0-9]+:", "", term), coef = ct[rows, 1], se = ct[rows, 2],
             irr = exp(ct[rows, 1]), irr_lo = exp(ct[rows, 1] - 1.96 * ct[rows, 2]),
             irr_hi = exp(ct[rows, 1] + 1.96 * ct[rows, 2]))
}
fml <- function(lhs) as.formula(paste(
  lhs, "~ i(YEAR, professor) + i(YEAR, pres_dean) + i(us_born) + i(race3) + i(urban) |",
  "YEAR^age5 + YEAR^STATEFIP"))
irr <- list()
for (lhs in c("sum_nchild", "sum_own_u5")) {
  for (s in c("all men", "married, spouse present")) {
    d <- if (s == "all men") reg_cells else reg_cells[married_sp == TRUE]
    m_all <- fepois(fml(lhs), data = d, offset = ~log(n), cluster = ~STATEFIP)
    irr[[length(irr) + 1L]] <- irr_rows(m_all, lhs)[, `:=`(sample = s, reference = "all other men")]
    m_prof <- fepois(fml(lhs), data = d[occ_group %in% c("professor", "pres_dean", "other_professional")],
                     offset = ~log(n), cluster = ~STATEFIP)
    irr[[length(irr) + 1L]] <- irr_rows(m_prof, lhs)[, `:=`(sample = s, reference = "other_professional")]
  }
}
irr <- rbindlist(irr)
irr[, outcome := sub("^sum_", "", model)][, model := NULL]
setcolorder(irr, c("outcome", "sample", "reference", "term", "YEAR"))
setorder(irr, outcome, sample, reference, term, YEAR)
write_out(irr, "poisson_irr_by_year.csv")
rm(reg_cells); invisible(gc(verbose = FALSE))

# --------------------------------------------------------------------------- #
# Calibration: NCHILD against the wife's CHSURV/CHBORN, 1900 and 1910
# --------------------------------------------------------------------------- #
calibration <- NULL
calib_path <- file.path(IN_DIR, "couples_calibration_cells.csv")
if (file.exists(calib_path)) {
  say("Calibration against CHSURV/CHBORN")
  cc <- fread(calib_path)[w_age5_lo %in% c(40L, 45L)]
  pool_calib <- function(d, name) d[, .(group = name, n = sum(n), sum_chsurv = sum(sum_chsurv),
                                        n_chsurv = sum(n_chsurv), sum_chborn = sum(sum_chborn),
                                        n_chborn = sum(n_chborn), sum_h_nchild = sum(sum_h_nchild)),
                                    by = YEAR]
  calibration <- rbind(pool_calib(cc[occ_group == "professor"], "professor"),
                       pool_calib(cc[occ_group == "other_professional"], "other_professional"),
                       pool_calib(cc, "all"))
  calibration[, `:=`(mean_chsurv = sum_chsurv / n_chsurv, mean_chborn = sum_chborn / n_chborn,
                     mean_husband_nchild = sum_h_nchild / n)]
  for (v in c("mean_chsurv", "mean_chborn", "mean_husband_nchild")) {
    calibration[, (paste0(v, "_ratio_to_all")) := get(v) / get(v)[group == "all"], by = YEAR]
  }
  setorder(calibration, YEAR, group)
  write_out(calibration, "calibration_1900_1910.csv")
}

# --------------------------------------------------------------------------- #
# Professor definition audit
# --------------------------------------------------------------------------- #
say("Auditing the professor definition")
prof <- fread(file.path(IN_DIR, "professors_by_year.csv"))
audit <- prof[occ_group == "professor", .(
  n = .N, mean_age = mean(AGE), share_under_25 = mean(AGE < 25),
  share_married_sp = mean(married_sp), share_urban = mean(urban == 1, na.rm = TRUE),
  share_group_quarters = mean(GQ %in% c(3L, 4L)), mean_nchild = mean(NCHILD)),
  by = .(YEAR, sex = fifelse(SEX == 1L, "male", "female"))]
audit[, share_of_year := n / sum(n), by = YEAR]
setorder(audit, YEAR, sex)
write_out(audit, "professor_definition_audit.csv")
occ_codes <- fread(file.path(IN_DIR, "professor_occ_codes.csv"))
write_out(occ_codes[occ_group == "professor", .(n = sum(n)), by = .(YEAR, OCC)][
  order(YEAR, -n)][, .SD[1:min(.N, 5L)], by = YEAR], "professor_occ_codes_top.csv")

# --------------------------------------------------------------------------- #
# Figure: professor / reference ratio by census
# --------------------------------------------------------------------------- #
say("Drawing the figure")
fig <- std_all[sample == "all men" & outcome == "nchild" & group == "professor",
               .(YEAR, ratio_to_all, ratio_to_other_professional,
                 se_all = ratio_se_log_all, se_prof = ratio_se_log_other_professional)]
fig <- rbind(
  fig[, .(YEAR, series = "vs all men", ratio = ratio_to_all,
          lo = ratio_to_all * exp(-1.96 * se_all), hi = ratio_to_all * exp(1.96 * se_all))],
  fig[, .(YEAR, series = "vs other professionals", ratio = ratio_to_other_professional,
          lo = ratio_to_other_professional * exp(-1.96 * se_prof),
          hi = ratio_to_other_professional * exp(1.96 * se_prof))])
fig[, series := factor(series, levels = c("vs all men", "vs other professionals"))]
# Reference palette, light mode: categorical slots 1 and 2, recessive ink and grid.
SURFACE <- "#fcfcfb"; INK <- "#0b0b0b"; INK_2 <- "#52514e"; GRID <- "#e4e3df"
SERIES <- c("vs all men" = "#2a78d6", "vs other professionals" = "#eb6834")
last <- fig[YEAR == max(YEAR)]
p <- ggplot(fig, aes(YEAR, ratio, colour = series)) +
  geom_hline(yintercept = 1, colour = INK_2, linewidth = 0.4, linetype = "dashed") +
  annotate("rect", xmin = 1885, xmax = 1895, ymin = -Inf, ymax = Inf, fill = GRID, alpha = 0.5) +
  annotate("text", x = 1890, y = Inf, label = "1890 lost", vjust = 1.6, size = 3, colour = INK_2) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 1.4, linewidth = 0.5,
                position = position_dodge(width = 2.5)) +
  geom_line(linewidth = 0.7, position = position_dodge(width = 2.5)) +
  geom_point(size = 2.6, position = position_dodge(width = 2.5), stroke = 0.8,
             shape = 21, fill = SURFACE) +
  geom_text(data = last, aes(label = series), hjust = 0, nudge_x = 2.2, size = 3.2,
            colour = INK, show.legend = FALSE) +
  annotate("text", x = 1910, y = min(fig$lo, na.rm = TRUE), label = "professor code\nwidens in 1910",
           vjust = 1.1, size = 2.8, colour = INK_2, lineheight = 0.9) +
  scale_colour_manual(values = SERIES, name = NULL) +
  scale_x_continuous(breaks = years, expand = expansion(add = c(3, 20))) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.08))) +
  labs(title = "Professors' co-resident children relative to other men, 1850-1910",
       subtitle = paste("Ratio of mean NCHILD, male professors aged 25-54 in households,",
                        "age-standardised to one pooled standard; 95% CIs"),
       x = "Census year", y = "Ratio to reference group",
       caption = "IPUMS 100% databases. NCHILD counts own children of any age living in the household.") +
  theme_minimal(base_size = 10) +
  theme(plot.background = element_rect(fill = SURFACE, colour = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3),
        axis.text = element_text(colour = INK_2), axis.title = element_text(colour = INK_2),
        plot.title = element_text(colour = INK, face = "bold"),
        plot.subtitle = element_text(colour = INK_2), plot.caption = element_text(colour = INK_2),
        legend.position = "top", legend.justification = "left", legend.text = element_text(colour = INK))
ggsave(file.path(OUT_DIR, "figure_professor_nchild_ratio.png"), p, width = 8, height = 5, dpi = 200)
say("wrote figure_professor_nchild_ratio.png")

# --------------------------------------------------------------------------- #
# Summary
# --------------------------------------------------------------------------- #
pw <- prof_wide[sample == "all men" & outcome == "nchild"][order(YEAR)]
pm <- prof_wide[sample == "married, spouse present" & outcome == "nchild"][order(YEAR)]
pu <- prof_wide[sample == "all men" & outcome == "own_u5"][order(YEAR)]
ir <- irr[outcome == "nchild" & sample == "all men" & reference == "all other men" &
            term == "professor"][order(YEAR)]
lines <- c(
  "# Professor fertility by census, 1850-1910 (NCHILD)", "",
  "Male professors (OCC1950 012-029) aged 25-54 in households. Means are directly",
  "age-standardised to one standard for every year: all men 25-54 pooled over the censuses.", "",
  "| Year | Professors (N) | Professors | All men | Men with an occupation | Other professionals | Ratio to all | Ratio, married only | Ratio, own children <5 | Poisson IRR vs all other men [95% CI] |",
  "|---|---:|---:|---:|---:|---:|---:|---:|---:|---|")
for (i in seq_len(nrow(pw))) {
  y <- pw$YEAR[i]
  r <- ir[YEAR == y]
  lines <- c(lines, sprintf("| %d | %s | %s | %s | %s | %s | %s | %s | %s | %s [%s, %s] |", y,
    format(pw$n_professors[i], big.mark = ","), fmt(pw$professor[i]), fmt(pw$all[i]),
    fmt(pw$all_occupied[i]), fmt(pw$other_professional[i]), fmt(pw$ratio_to_all[i]),
    fmt(pm$ratio_to_all[pm$YEAR == y]), fmt(pu$ratio_to_all[pu$YEAR == y]),
    fmt(r$irr, 3), fmt(r$irr_lo, 3), fmt(r$irr_hi, 3)))
}
if (!is.null(calibration)) {
  lines <- c(lines, "", "Calibration, couples with wife aged 40-49 (professor / all):", "")
  for (y in unique(calibration$YEAR)) {
    cy <- calibration[YEAR == y & group == "professor"]
    lines <- c(lines, sprintf("- %d: husband's NCHILD %s, wife's CHSURV %s, wife's CHBORN %s",
                              y, fmt(cy$mean_husband_nchild_ratio_to_all),
                              fmt(cy$mean_chsurv_ratio_to_all), fmt(cy$mean_chborn_ratio_to_all)))
  }
}
lines <- c(lines, "", "Caveats:",
  "- NCHILD counts co-resident children of any age, so it mixes fertility, child mortality and the age children leave home; the under-5 column guards against the last.",
  "- Before 1880 the census did not record relationship to head; IPUMS imputes the family pointers, and with them NCHILD. Married = SPLOC > 0 in every year, since MARST is blank before 1880.",
  "- 1890 is missing (the schedules burned in 1921).",
  "- The professor code is not stable: 1910 holds 3.3x the professors of 1900 and a large female group. See professor_definition_audit.csv.")
writeLines(lines, file.path(OUT_DIR, "summary.md"))
say("Done: ", OUT_DIR)
