# Synthetic checks for the linked-census CLS IV. No project data or outputs needed.
source("analysis/mandatory_schooling/estimate_linked_1940_schooling_1950_scientist_iv.R",
       local = TRUE)

stopifnot(identical(as.integer(school_years(c(10L, 30L, 40L, 110L,
                                              150L, 200L, 230L, 999L, 0L))),
                    c(0L, 0L, 1L, 8L, 12L, 17L, 17L, NA_integer_, NA_integer_)))
stopifnot(identical(law_bin(0:10),
                    c("0", rep("1_5", 5), "6", "7", "8", "9plus", "9plus")))

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
codes <- data.frame(higraded = c(10L, 30L, 40L, 110L, 150L,
                                  151L, 152L, 200L, 230L, 999L, 0L))
DBI::dbWriteTable(con, "codes", codes, temporary = TRUE)
sql_years <- DBI::dbGetQuery(con, paste("SELECT", grade_sql("higraded"),
                                        "AS school_years FROM codes"))$school_years
stopifnot(identical(as.integer(sql_years), as.integer(school_years(codes$higraded))))

set.seed(20260925)
people <- expand.grid(bpl = seq_len(12L), yob = 1885:1894,
                      person = seq_len(24L))
people$region <- (people$bpl - 1L) %/% 3L + 1L
people$required <- (people$bpl * 3L + people$yob * 2L) %% 10L
people$law_bin <- law_bin(people$required)
people$school_years <- pmin(17L, pmax(0L, 5L + people$required %/% 3L +
                                     (people$person * 3L + people$bpl) %% 7L))
people$scientist <- as.integer((people$person + people$school_years * 5L +
                                people$bpl + people$yob) %% 11L == 0L)
people$region_yob <- paste(people$region, people$yob, sep = "_")
people$state_cohort <- paste(people$bpl, people$yob, sep = "_")
for (term in law_terms) people[[term]] <- as.integer(
  people$law_bin == sub("^z_", "", term))

cells <- aggregate(rep(1L, nrow(people)),
                   people[c("bpl", "yob", "region", "law_bin",
                            "school_years", "scientist")], sum)
names(cells)[names(cells) == "x"] <- "n"
fit_grouped <- estimate_models(cells, "fixture")
z <- paste(law_terms, collapse = " + ")
individual_iv <- fixest::feols(
  as.formula(paste0("scientist ~ 1 | bpl + region_yob | school_years ~ ", z)),
  data = people, cluster = ~state_cohort, ssc = fixest::ssc(K.adj = FALSE))
individual_ols <- fixest::feols(scientist ~ school_years | bpl + region_yob,
                               data = people, cluster = ~state_cohort,
                               ssc = fixest::ssc(K.adj = FALSE))
stopifnot(isTRUE(all.equal(unname(coef(fit_grouped$models$iv_cell)),
                           unname(coef(individual_iv)), tolerance = 1e-9)))
stopifnot(isTRUE(all.equal(unname(coef(fit_grouped$models$ols_cell)),
                           unname(coef(individual_ols)), tolerance = 1e-9)))
stopifnot(isTRUE(all.equal(summary(fit_grouped$models$iv_cell)$coeftable[1, 2],
                           summary(individual_iv)$coeftable[1, 2], tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(summary(fit_grouped$models$ols_cell)$coeftable[1, 2],
                           summary(individual_ols)$coeftable[1, 2], tolerance = 1e-8)))
stopifnot(all(fit_grouped$tables$diagnostics$observations == nrow(people)))
ar <- ar_confidence_set(cells, "fixture", limit = 0.02, grid_points = 21L)
zero <- ar$grid$p_value[which.min(abs(ar$grid$beta))]
stopifnot(isTRUE(all.equal(zero,
  fit_grouped$tables$diagnostics$anderson_rubin_zero_p[
    fit_grouped$tables$diagnostics$cluster == "state"], tolerance = 1e-9)))
message("linked 1940 schooling / 1950 scientist IV fixture passed")
