# County x census dataset of male professors' NCHILD by 10-year age bin, 1850-1910.
#
# NCHILD (own children of any age in the household) proxies fertility; the
# census edition proxies the decade. One row per census year x county x age bin
# (20-29 ... 60-69), for every county with any man aged 20-69, so a county with
# no professor appears with n_prof = 0. Each row carries professors, presidents
# and deans, other professionals and all men, in two universes side by side:
#   (no suffix)  men in households (GQ 1/2), any marital status
#   _married     men with a wife in the household (SPLOC > 0; MARST is blank
#                before 1880, so SPLOC is the only marriage marker in every year)
# Professors in group quarters cannot be linked to children; they are counted
# in n_prof_gq and kept out of the means.
#
# Counties are contemporaneous (boundaries change across censuses). GISJOIN
# follows the NHGIS county code, historical counties included; county_geoid is
# the project's STATEFIP * 1000 + COUNTYICP / 10, NULL for codes not ending in 0.
#
# The county x census summary uses indirect standardisation, which stays stable
# on the 1-3 professors most counties have: observed professor NCHILD over what
# the same professors would have at their county's all-men rates by age bin.
#
# Inputs:
#   OCC_FERTILITY_DECADES_OUT_DIR/county_cells (build_professor_nchild_decades.R)
#   NHGIS_COUNTY_CROSSWALK (default <Dropbox>/input/nhgis0001_ts_nominal_county.csv)
# Output: OCC_FERTILITY_COUNTY_OUT_DIR
#   (default <Dropbox>/output/occupational_fertility/county_decade)
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/build_professor_county_nchild.R

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
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
data_root <- function() {
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
}
env_or <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default()
}
IN_DIR <- env_or("OCC_FERTILITY_DECADES_OUT_DIR",
                 function() file.path(data_root(), "output/occupational_fertility/decades"))
OUT_DIR <- env_or("OCC_FERTILITY_COUNTY_OUT_DIR",
                  function() file.path(data_root(), "output/occupational_fertility/county_decade"))
CROSSWALK <- env_or("NHGIS_COUNTY_CROSSWALK",
                    function() file.path(data_root(), "input/nhgis0001_ts_nominal_county.csv"))
COVERAGE <- env_or("OCC_COVERAGE_FACTORS", function()
  file.path(data_root(), "output/occupational_fertility/professor_coverage_factors.csv"))
LOW_N <- 5L

source(file.path(repo_root, "prep/occupational_fertility/occupation_groups.R"))
say <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)

KEYS <- c("YEAR", "STATEFIP", "COUNTYICP", "age_bin_lo")
GROUPS <- list(prof = "professor", pres_dean = "pres_dean",
               prof_other = setdiff(PROFESSIONAL, c("professor", "pres_dean")),
               allmen = OCC_GROUPS$group)

main <- function() {
  started <- Sys.time()
  cells_path <- file.path(IN_DIR, "county_cells")
  stop_if(!dir.exists(cells_path), "No county_cells under ", IN_DIR,
          "; run build_professor_nchild_decades.R first")
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  setDTthreads(8L)

  say("Reading county cells")
  cells <- as.data.table(open_dataset(cells_path) |> dplyr::collect())
  cells[, YEAR := as.integer(YEAR)]
  stop_if(any(cells$age_bin_lo < 20L | cells$age_bin_lo > 60L), "Age bin outside 20-69")
  say(format(sum(cells$n), big.mark = ","), " men aged 20-69 in ", format(nrow(cells), big.mark = ","),
      " cells, ", paste(sort(unique(cells$YEAR)), collapse = ", "))

  # Every county x bin with any man, including group quarters.
  grid <- unique(cells[, ..KEYS])
  sums <- function(d) d[, .(n = sum(n), nchild_sum = sum(sum_nchild),
                            nchild_sumsq = sum(sum_nchild_sq), own_u5_sum = sum(sum_own_u5)),
                        by = KEYS]
  out <- copy(grid)
  for (g in names(GROUPS)) {
    for (married in c(FALSE, TRUE)) {
      d <- cells[occ_group %in% GROUPS[[g]] & in_household == TRUE]
      if (married) d <- d[married_sp == TRUE]
      s <- sums(d)
      suffix <- paste0("_", g, if (married) "_married" else "")
      setnames(s, c("n", "nchild_sum", "nchild_sumsq", "own_u5_sum"),
               paste0(c("n", "nchild_sum", "nchild_sumsq", "own_u5_sum"), suffix))
      out <- merge(out, s, by = KEYS, all.x = TRUE)
    }
  }
  count_cols <- grep("^(n|nchild_sum|nchild_sumsq|own_u5_sum)_", names(out), value = TRUE)
  for (col in count_cols) set(out, which(is.na(out[[col]])), col, 0)
  gq <- cells[occ_group == "professor" & in_household == FALSE, .(n_prof_gq = sum(n)), by = KEYS]
  out <- merge(out, gq, by = KEYS, all.x = TRUE)
  out[is.na(n_prof_gq), n_prof_gq := 0]

  for (g in names(GROUPS)) for (m in c("", "_married")) {
    suffix <- paste0("_", g, m)
    out[, (paste0("nchild_mean", suffix)) :=
          fifelse(get(paste0("n", suffix)) > 0,
                  get(paste0("nchild_sum", suffix)) / get(paste0("n", suffix)), NA_real_)]
  }
  for (m in c("", "_married")) {
    out[, (paste0("ratio_prof_to_allmen", m)) :=
          get(paste0("nchild_mean_prof", m)) / get(paste0("nchild_mean_allmen", m))]
    out[, (paste0("low_n_prof", m)) := get(paste0("n_prof", m)) < LOW_N]
  }

  # Geography.
  out[, `:=`(GISJOIN = sprintf("G%02d0%04d", STATEFIP, COUNTYICP),
             county_geoid = fifelse(COUNTYICP %% 10L == 0L,
                                    as.integer(STATEFIP * 1000L + COUNTYICP %/% 10L), NA_integer_),
             age_bin = paste0(age_bin_lo, "-", age_bin_lo + 9L))]
  if (file.exists(CROSSWALK)) {
    xw <- unique(fread(CROSSWALK, select = c("GISJOIN", "STATE", "COUNTY"), encoding = "Latin-1"),
                 by = "GISJOIN")
    out <- merge(out, xw[, .(GISJOIN, state_name = STATE, county_name = COUNTY)],
                 by = "GISJOIN", all.x = TRUE)
    out[, in_nhgis_crosswalk := !is.na(county_name)]
  } else {
    warning("NHGIS crosswalk not found; county names left blank: ", CROSSWALK)
    out[, `:=`(state_name = NA_character_, county_name = NA_character_, in_nhgis_crosswalk = NA)]
  }

  front <- c("YEAR", "STATEFIP", "COUNTYICP", "GISJOIN", "county_geoid", "state_name",
             "county_name", "in_nhgis_crosswalk", "age_bin", "age_bin_lo")
  value_order <- unlist(lapply(c("", "_married"), function(m) c(
    paste0(c("n", "nchild_mean", "nchild_sum", "nchild_sumsq", "own_u5_sum"),
           rep(paste0("_", names(GROUPS), m), each = 5)),
    paste0(c("ratio_prof_to_allmen", "low_n_prof"), m))))
  setcolorder(out, c(front, "n_prof_gq", value_order))
  setorderv(out, c("YEAR", "STATEFIP", "COUNTYICP", "age_bin_lo"))

  # ---- County x census summary: indirect standardisation ------------------- #
  say("Indirect standardisation by county")
  national <- out[, .(nat_sum = sum(nchild_sum_allmen), nat_n = sum(n_allmen),
                      nat_sum_m = sum(nchild_sum_allmen_married), nat_n_m = sum(n_allmen_married)),
                  by = .(YEAR, age_bin_lo)]
  national[, `:=`(nat_rate = nat_sum / nat_n, nat_rate_married = nat_sum_m / nat_n_m)]
  z <- merge(out, national[, .(YEAR, age_bin_lo, nat_rate, nat_rate_married)],
             by = c("YEAR", "age_bin_lo"))
  summarise_universe <- function(m) {
    n_p <- paste0("n_prof", m); s_p <- paste0("nchild_sum_prof", m)
    n_a <- paste0("n_allmen", m); s_a <- paste0("nchild_sum_allmen", m)
    q_a <- paste0("nchild_sumsq_allmen", m)
    nat <- if (m == "") "nat_rate" else "nat_rate_married"
    z[, {
      rate_a <- fifelse(get(n_a) > 0, get(s_a) / get(n_a), NA_real_)
      # Within-bin variance of all men, assumed for the professors in the bin.
      var_a <- fifelse(get(n_a) > 1, (get(q_a) - get(s_a)^2 / get(n_a)) / (get(n_a) - 1), NA_real_)
      keep <- get(n_p) > 0
      obs <- sum(get(s_p))
      exp_local <- sum((get(n_p) * rate_a)[keep])
      exp_nat <- sum((get(n_p) * get(nat))[keep])
      var_obs <- sum((get(n_p) * var_a)[keep])
      list(n_prof = sum(get(n_p)), n_allmen = sum(get(n_a)), observed = obs,
           expected_local = exp_local, expected_national = exp_nat,
           ratio_local = if (sum(get(n_p)) > 0 && exp_local > 0) obs / exp_local else NA_real_,
           ratio_local_se = if (sum(get(n_p)) > 0 && exp_local > 0) sqrt(var_obs) / exp_local else NA_real_,
           ratio_national = if (sum(get(n_p)) > 0 && exp_nat > 0) obs / exp_nat else NA_real_)
    }, by = .(YEAR, STATEFIP, COUNTYICP, GISJOIN, county_geoid, state_name, county_name,
              in_nhgis_crosswalk)]
  }
  hh <- summarise_universe("")
  mar <- summarise_universe("_married")
  setnames(mar, c("n_prof", "n_allmen", "observed", "expected_local", "expected_national",
                  "ratio_local", "ratio_local_se", "ratio_national"),
           paste0(c("n_prof", "n_allmen", "observed", "expected_local", "expected_national",
                    "ratio_local", "ratio_local_se", "ratio_national"), "_married"))
  county <- merge(hh, mar, by = c("YEAR", "STATEFIP", "COUNTYICP", "GISJOIN", "county_geoid",
                                  "state_name", "county_name", "in_nhgis_crosswalk"))
  gq_county <- out[, .(n_prof_gq = sum(n_prof_gq)), by = .(YEAR, STATEFIP, COUNTYICP)]
  county <- merge(county, gq_county, by = c("YEAR", "STATEFIP", "COUNTYICP"))
  county[, low_n_prof := n_prof < LOW_N]
  # Counts on the total-faculty scale (build_professor_coverage.R): every man
  # coded 010-029, wherever he lives, times the census's coverage factor. The
  # NCHILD means and ratios above are within-census comparisons and need no factor.
  staff <- cells[occ_group %in% c("professor", "pres_dean"),
                 .(n_prof_deans_all = sum(n)), by = .(YEAR, STATEFIP, COUNTYICP)]
  county <- merge(county, staff, by = c("YEAR", "STATEFIP", "COUNTYICP"), all.x = TRUE)
  county[is.na(n_prof_deans_all), n_prof_deans_all := 0]
  if (file.exists(COVERAGE)) {
    factors <- fread(COVERAGE)[, .(YEAR = census, faculty_factor)]
    county <- merge(county, factors, by = "YEAR", all.x = TRUE)
    county[, n_faculty_equiv := n_prof_deans_all * faculty_factor]
  } else {
    warning("No coverage factors at ", COVERAGE, "; faculty_factor left blank")
    county[, `:=`(faculty_factor = NA_real_, n_faculty_equiv = NA_real_)]
  }
  setorderv(county, c("YEAR", "STATEFIP", "COUNTYICP"))

  unmatched <- out[in_nhgis_crosswalk == FALSE,
                   .(n_allmen_20_69 = sum(n_allmen), n_prof = sum(n_prof)),
                   by = .(YEAR, STATEFIP, COUNTYICP, GISJOIN)][order(YEAR, -n_allmen_20_69)]

  say("Writing outputs to ", OUT_DIR)
  fwrite(out, file.path(OUT_DIR, "professor_nchild_county_age.csv"))
  write_parquet(out, file.path(OUT_DIR, "professor_nchild_county_age.parquet"),
                compression = "zstd")
  fwrite(county, file.path(OUT_DIR, "professor_nchild_county.csv"))
  fwrite(unmatched, file.path(OUT_DIR, "county_codes_unmatched.csv"))

  by_year <- county[, .(counties = .N, counties_with_prof = sum(n_prof > 0),
                        counties_prof_ge_5 = sum(n_prof >= LOW_N), professors = sum(n_prof),
                        professors_gq = sum(n_prof_gq),
                        counties_not_in_crosswalk = sum(in_nhgis_crosswalk == FALSE, na.rm = TRUE)),
                    by = YEAR][order(YEAR)]
  fwrite(by_year, file.path(OUT_DIR, "coverage_by_year.csv"))
  write_dictionary(by_year)
  jsonlite::write_json(list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    input = normalizePath(cells_path, winslash = "/"), crosswalk = CROSSWALK,
    rows_county_age = nrow(out), rows_county = nrow(county), coverage = by_year,
    men_20_69 = sum(cells$n),
    elapsed_minutes = round(as.numeric(difftime(Sys.time(), started, units = "mins")), 2)),
    file.path(OUT_DIR, "build_manifest.json"), pretty = TRUE, auto_unbox = TRUE, digits = NA)
  say("Done: ", format(nrow(out), big.mark = ","), " county x age rows, ",
      format(nrow(county), big.mark = ","), " county x census rows")
}

write_dictionary <- function(by_year) {
  fmt_row <- function(r) sprintf("| %d | %s | %s | %s | %s | %s |", r$YEAR,
                                 format(r$counties, big.mark = ","),
                                 format(r$counties_with_prof, big.mark = ","),
                                 format(r$counties_prof_ge_5, big.mark = ","),
                                 format(r$professors, big.mark = ","),
                                 format(r$professors_gq, big.mark = ","))
  lines <- c(
    "# Professors' NCHILD by county, census and age bin, 1850-1910", "",
    "Built by `prep/occupational_fertility/build_professor_county_nchild.R` from the IPUMS",
    "100% databases (extract usa_00010). Men only. NCHILD (own children of any age living in",
    "the household, step-children included, capped at 9+) proxies fertility; the census",
    "edition (1850, 1860, 1870, 1880, 1900, 1910; 1890 burned) proxies the decade.", "",
    "## Coverage", "",
    "| Year | Counties | With a professor | With 5+ professors | Professors (households) | Professors in group quarters |",
    "|---|---:|---:|---:|---:|---:|",
    vapply(seq_len(nrow(by_year)), function(i) fmt_row(by_year[i]), character(1)), "",
    "## professor_nchild_county_age (one row per YEAR x county x age bin)", "",
    "| Column | Meaning |", "|---|---|",
    "| `YEAR` | Census edition, standing in for the decade |",
    "| `STATEFIP`, `COUNTYICP` | IPUMS state FIPS and ICPSR county code, as coded that year |",
    "| `GISJOIN` | NHGIS county identifier, `G` + 2-digit state + `0` + 4-digit county; links to that year's NHGIS boundary file |",
    "| `county_geoid` | `STATEFIP * 1000 + COUNTYICP / 10`; NULL for historical counties whose code does not end in 0 |",
    "| `state_name`, `county_name`, `in_nhgis_crosswalk` | From `input/nhgis0001_ts_nominal_county.csv`; FALSE rows are listed in `county_codes_unmatched.csv` |",
    "| `age_bin`, `age_bin_lo` | 10-year bin, 20-29 ... 60-69 |",
    "| `n_prof_gq` | Male professors in group quarters (college residences, institutions), counted but not used: they have no linkable children |",
    "| `n_<g>` | Men in households (GQ 1/2) in the group |",
    "| `nchild_mean_<g>`, `nchild_sum_<g>`, `nchild_sumsq_<g>` | Mean, sum and sum of squares of NCHILD; the mean is NA when n is 0 |",
    "| `own_u5_sum_<g>` | Own children aged 0-4 (POPLOC), for a child-father ratio |",
    "| `..._married` | The same for men with a wife in the household (SPLOC > 0) |",
    "| `ratio_prof_to_allmen[_married]` | `nchild_mean_prof / nchild_mean_allmen` in the same county and bin |",
    "| `low_n_prof[_married]` | Fewer than 5 professors in the cell |", "",
    "Groups `<g>`: `prof` (OCC1950 012-029), `pres_dean` (010), `prof_other` (other professional",
    "codes 000-099), `allmen` (every man, any occupation or none).", "",
    "## professor_nchild_county (one row per YEAR x county, ages 20-69)", "",
    "Indirect standardisation, stable on small counts:", "",
    "- `observed`: summed NCHILD of the county's professors.",
    "- `expected_local`: the same professors at their county's all-men mean NCHILD by age bin.",
    "- `expected_national`: the same at that census's national all-men rates.",
    "- `ratio_local = observed / expected_local`: professors against the men of their own county, at their own ages.",
    "- `ratio_local_se`: approximate, assuming the professors' within-bin variance equals all men's.",
    "- `_married` columns repeat this for married men; `low_n_prof` flags fewer than 5 professors.", "",
    "### Counts on the total-faculty scale", "",
    "- `n_prof_deans_all`: men aged 20-69 coded OCC1950 010-029, households and group quarters.",
    "- `faculty_factor`: 1 / census coverage of college faculty, from `build_professor_coverage.R`",
    "  against NCES male professional staff (NCES 93-442, Table 26): about 4.1 (1850-1870), 2.4 (1880),",
    "  4.2 (1900), 2.5 (1910). 1850 and 1860 borrow 1870's.",
    "- `n_faculty_equiv = n_prof_deans_all * faculty_factor`: estimated male college faculty, comparable",
    "  across censuses. It rescales counts; it does not reclassify anyone, and the NCHILD means and ratios",
    "  (within-census comparisons) need no factor.", "",
    "## Caveats", "",
    "- NCHILD counts co-resident children, so it mixes fertility, child mortality and the age children leave home. The 1900/1910 calibration in `results/occupational_fertility/decades/` shows it understates the professor gap in completed fertility by about 0.08-0.12.",
    "- Most counties with any professor have 1-3. Weight by `n_prof`, or pool, before reading a ratio.",
    "- Counties are contemporaneous: boundaries change between censuses and are not harmonised.",
    "- Before 1880 the census did not record relationship to head; IPUMS imputes the family pointers and NCHILD with them. Married = SPLOC > 0 in every year because MARST is blank before 1880.",
    "- The professor code widens in 1910 (3.3x the professors of 1900).")
  writeLines(lines, file.path(OUT_DIR, "README_county_decade.md"))
}

main()
