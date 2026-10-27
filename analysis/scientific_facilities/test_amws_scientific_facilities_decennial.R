args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
repo_root <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(data.table))
source(file.path(repo_root, "analysis/scientific_facilities/amws_facilities_annual_helpers.R"))
source(file.path(repo_root, "analysis/scientific_facilities/amws_facilities_decennial_helpers.R"))
s <- sfd_settings(); s$biters <- 499L
stopifnot(sfd_decade(1908, "standard_decade") == 1900,
          sfd_decade(1908, "alternative_decade") == 1910,
          sfd_decade(1876, "alternative_decade") == 1870)
u <- data.table(GEOID = sprintf("%05d", 1:60),
  first_exposure_year = rep(c(1876L, 1908L, 1942L, 1945L, NA_integer_), each = 12L))
u <- cbind(u, sfa_classify(u$first_exposure_year))
u[, effect := rep(c(2, 4, 8, 1000, 0), each = 12L)]
p <- merge(CJ(GEOID = u$GEOID, year = 1860:1949), u, by = "GEOID")
p[, `:=`(n_amws = 10 + (year - 1860) / 100 +
  effect * (!is.na(first_exposure_year) & year >= first_exposure_year),
  population = 100 + (year - 1860), county_births_estimate_year = 10)]
p <- p[!(GEOID == "00001" & year == 1878)]
b <- sfd_build_panel(p)
stopifnot(is.na(b$panel[GEOID == "00001" & decade == 1870, n_amws]),
  b$panel[GEOID == "00002" & decade == 1860, population] == 104.5,
  b$panel[GEOID == "00002" & decade == 1860, county_births_estimate] == 100,
  abs(b$panel[GEOID == "00002" & decade == 1860, n_amws] - 100.45) < 1e-10,
  all(is.na(b$panel[n_amws %in% NA_real_, log1p_n_amws])))
timing_audit <- sfd_timing_audit(u)
stopifnot(timing_audit[GEOID == "00013" & timing == "alternative_decade", exposed_years_reference_block] == 2L)
d <- sfd_model_data(b$panel, u, "n_amws", "alternative_decade")
sel <- sfd_select(d, 1910, 1910, 1900, "notyettreated")
stopifnot(!any(sel$t$first_exposure_year[sel$t$participant_role == "control"] <= 1919, na.rm = TRUE))
sel <- sfd_select(d, 1940, 1940, 1930, "notyettreated")
stopifnot(!any(sel$t$GEOID %in% u[role == "donor_only", GEOID]),
          all(sel$t[participant_role == "treated", first_exposure_year] == 1942))
sel <- sfd_select(d, 1910, 1930, 1900, "notyettreated")
stopifnot(any(sel$t$GEOID %in% u[role == "donor_only", GEOID]))
sel <- sfd_select(d, 1910, 1930, 1900, "nevertreated")
stopifnot(!any(sel$t$GEOID %in% u[role == "donor_only", GEOID]))
cat("Complete-block, rounding, partial-reference and donor-boundary tests passed.\n")
load_error <- tryCatch({suppressPackageStartupMessages(library(did)); NA_character_},
                      error = function(e) conditionMessage(e))
if (!is.na(load_error)) {
  cat("Estimator tests NOT RUN:", load_error, "\n")
} else {
  for (timing in c("standard_decade", "alternative_decade")) for (control in c("nevertreated", "notyettreated")) {
    d <- sfd_model_data(b$panel, u, "n_amws", timing)
    fit <- sfd_estimate(d, control, s)
    stopifnot(!any(fit$cells$status == "failed_estimation"),
              all(fit$cells[estimable == TRUE & event_time == -10, att] == 0),
              !1940 %in% sfa_balanced_cohorts(fit$cells, c(-10, 0, 10)))
    if (timing == "standard_decade") {
      stopifnot(abs(fit$cells[g == 1870 & event_time == 0, att] - 8) < 1e-8,
                abs(fit$cells[g == 1900 & event_time == 0, att] - 8) < 1e-8,
                abs(fit$cells[g == 1940 & event_time == 0, att] - 64) < 1e-8)
    } else stopifnot(abs(fit$cells[g == 1910 & event_time == 0, att] - 32) < 1e-8)
    for (balance in c("unbalanced", "balanced")) {
      a <- sfd_aggregate(fit, d, balance, s)
      stopifnot(max(abs(a$dynamic$estimate - a$dynamic$manual_att), na.rm = TRUE) < 1e-10,
        !anyDuplicated(a$annotation$baseline[, .(id, base_year)]),
        a$annotation$summary$baseline_mean == mean(a$annotation$baseline$y),
        all(a$donor_availability$first_exposure_year >
          if (control == "nevertreated") 1949 else
            pmax(a$donor_availability$g - 10, a$donor_availability$g + a$donor_availability$event_time) + 9,
          na.rm = TRUE))
      if (balance == "balanced") stopifnot(all(a$dynamic$n_cohorts == 2L), !1940 %in% a$cohorts)
    }
  }
  # Without donor-only collisions, standard timing matches ordinary att_gt.
  d <- sfd_model_data(b$panel, u, "n_amws", "standard_decade")
  d <- d[role != "donor_only"]
  d[is.na(g), g := 0L]
  for (control in c("nevertreated", "notyettreated")) {
    fit <- sfd_estimate(d, control, s)
    m <- suppressWarnings(suppressMessages(did::att_gt(yname = "y", tname = "year",
      idname = "id", gname = "g", data = as.data.frame(d), xformla = ~1,
      control_group = control, est_method = "dr", base_period = "universal",
      allow_unbalanced_panel = TRUE, bstrap = FALSE, cband = FALSE)))
    for (j in which(fit$cells$estimable)) {
      pos <- which(m$group == fit$cells$g[j] & m$t == fit$cells$year[j])
      stopifnot(length(pos) == 1L, abs(m$att[pos] - fit$cells$att[j]) < 1e-8)
      mapped <- numeric(length(fit$ids))
      mapped[match(unique(m$DIDparams$data$id), fit$ids)] <- as.numeric(m$inffunc[, pos])/m$n
      stopifnot(max(abs(mapped - fit$contribution[, j])) < 1e-8)
    }
    # Validate weight uncertainty and covariance against aggte's joint IF.
    agg <- suppressWarnings(did::aggte(m, type = "dynamic", min_e = -10, max_e = 10,
      balance_e = 10, bstrap = FALSE, cband = FALSE, na.rm = TRUE))
    a <- sfd_aggregate(fit, d, "balanced", s)
    stopifnot(max(abs(a$dynamic$estimate - agg$att.egt)) < 1e-8)
    set.seed(s$seed + 1L)
    multipliers <- matrix(rnorm(s$biters * length(fit$ids)), nrow = s$biters)
    expected_se <- apply(multipliers %*% (as.matrix(agg$inf.function$dynamic.inf.func.e) / m$n), 2, sd)
    stopifnot(max(abs(a$dynamic$se - expected_se)) < 1e-8)
  }
  # Pre-1920 cutoff is applied before decade rounding. The shifted 1917 cohort
  # becomes 1920 and is absent only from the +30 balanced aggregation, whereas
  # exact 1920 openings remain donor-only even when cohort labels coincide.
  early <- data.table(GEOID = sprintf("%05d", 101:160),
    first_exposure_year = rep(c(1876L, 1908L, 1917L, 1920L, NA_integer_), each = 12L))
  early <- cbind(early, sfa_classify(early$first_exposure_year, target_cutoff = 1919L))
  early[, effect := rep(c(2, 4, 8, 1000, 0), each = 12L)]
  raw <- merge(CJ(GEOID = early$GEOID, year = 1860:1949), early, by = "GEOID")
  raw[, `:=`(n_amws = 10 + (year - 1860) / 100 +
    effect * (!is.na(first_exposure_year) & year >= first_exposure_year),
    population = 100 + year - 1860, county_births_estimate_year = 10)]
  blocks <- sfd_build_panel(raw)$panel
  s_early <- s; s_early$target_cutoff <- 1919L; s_early$balanced_post <- 30L
  stopifnot(all(early[first_exposure_year == 1920, role] == "donor_only"))
  for (timing in c("standard_decade", "alternative_decade")) for (control in c("nevertreated", "notyettreated")) {
    d <- sfd_model_data(blocks, early, "n_amws", timing)
    fit <- sfd_estimate(d, control, s_early)
    stopifnot(!any(fit$cells$status == "failed_estimation"))
    bal <- sfd_aggregate(fit, d, "balanced", s_early)
    unbal <- sfd_aggregate(fit, d, "unbalanced", s_early)
    expected <- if (timing == "standard_decade") c(1870, 1900, 1910) else c(1870, 1910)
    stopifnot(setequal(bal$cohorts, expected),
      identical(bal$dynamic$event_time, seq(-10L, 30L, 10L)),
      all(bal$dynamic$n_cohorts == length(expected)),
      max(unbal$dynamic$event_time) == 70L,
      all(unbal$cells$block_end_year <= 1949L),
      all(unbal$summaries$post_max %in% c(30L, 70L)),
      !any(unbal$annotation$participants$id %in% early[role == "donor_only", as.integer(GEOID)] &
        unbal$annotation$participants$participant_role == "treated"))
    if (timing == "alternative_decade") stopifnot(1920 %in% unbal$cohorts)
    future <- sfd_select(d, min(expected), 1910, 1860, control)$t
    stopifnot(!any(future[participant_role == "control", first_exposure_year] <=
      if (control == "nevertreated") 1949 else 1919, na.rm = TRUE))
  }
  cat("Known-effect, cell IF, joint covariance, weighting, annotation and pre-1920/+30 tests passed for all timing/control versions.\n")
}
