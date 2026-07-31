###############################################################################
# County-decade panel combining the consolidated AMWS editions with the
# canonical decennial Wikipedia/population panel, 1800-1960.
###############################################################################
suppressPackageStartupMessages(library(data.table))

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/", mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE
  )
}
source(file.path(repo_root, "paths.R"))

normalize_geoid <- function(x) sprintf("%05d", suppressWarnings(as.integer(x)))

annual_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
decennial_file <- file.path(DATA_OUTPUT, "us_panel_county_stem_1800.csv")
output_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_decade.csv")
summary_file <- file.path(
  AMWS_OUTPUT, "amws_consolidated_county_decade_summary.csv"
)

annual <- fread(annual_file)
required_annual <- c(
  "GEOID", "year", "n_amws_1906_1955_dedup", "n_amws_1986", "n_amws"
)
missing_annual <- setdiff(required_annual, names(annual))
if (length(missing_annual)) {
  stop("Missing annual AMWS columns: ", paste(missing_annual, collapse = ", "))
}
annual[, `:=`(
  GEOID = normalize_geoid(GEOID),
  year = as.integer(year),
  decade = as.integer(10L * floor(year / 10L))
)]
annual <- annual[year >= 1800L & year <= 1960L]

amws_decade <- annual[, .(
  n_amws_1906_1955_dedup = sum(n_amws_1906_1955_dedup, na.rm = TRUE),
  n_amws_1986 = sum(n_amws_1986, na.rm = TRUE),
  n_amws = sum(n_amws, na.rm = TRUE)
), by = .(GEOID, decade)]

if (amws_decade[, any(
  n_amws != n_amws_1906_1955_dedup + n_amws_1986
)]) {
  stop("AMWS components do not reconcile after decade aggregation")
}

panel <- fread(decennial_file)
required_panel <- c(
  "GEOID", "decade", "population", "population_source",
  "county_births_estimate_decade", "n_stem", "stem_per_1000_pop",
  "stem_per_1000_births"
)
missing_panel <- setdiff(required_panel, names(panel))
if (length(missing_panel)) {
  stop("Missing decennial panel columns: ", paste(missing_panel, collapse = ", "))
}
panel[, `:=`(
  GEOID = normalize_geoid(GEOID),
  decade = as.integer(decade)
)]
panel <- panel[
  decade >= 1800L & decade <= 1960L &
    population_source %in% c("nhgis", "manual", "merged_nyc")
]

panel_keys <- unique(panel[, .(GEOID, decade)])
unmatched <- amws_decade[!panel_keys, on = .(GEOID, decade)]
fallback_keys <- unmatched[n_amws > 0, .(GEOID, decade)]
fallback_births <- unmatched[n_amws > 0, sum(n_amws, na.rm = TRUE)]
if (nrow(fallback_keys)) {
  # Three Virginia independent-city GEOIDs occur in the annual NHGIS/manual
  # panel but not in the legacy decennial file. Build only those missing
  # county-decades from the same annual source rather than discarding AMWS
  # births or introducing HYDE population.
  fallback <- annual[fallback_keys, on = .(GEOID, decade), nomatch = 0L][
    order(GEOID, decade, year),
    .(
      population = {
        at_start <- population[year == decade]
        if (length(at_start) && any(is.finite(at_start))) {
          at_start[which(is.finite(at_start))[1L]]
        } else {
          population[which(is.finite(population))[1L]]
        }
      },
      population_source = {
        at_start <- population_source[year == decade]
        if (length(at_start)) at_start[1L] else population_source[1L]
      },
      county_births_estimate_decade = sum(
        county_births_estimate_year, na.rm = TRUE
      ),
      n_all_wiki = sum(n_all_wiki, na.rm = TRUE),
      n_inventors = sum(n_inventors, na.rm = TRUE),
      n_stem = sum(n_stem, na.rm = TRUE)
    ),
    by = .(GEOID, decade)
  ]
  fallback[, `:=`(
    stem_per_1000_pop = fifelse(
      population > 0, 1000 * n_stem / population, NA_real_
    ),
    stem_per_1000_births = fifelse(
      county_births_estimate_decade > 0,
      1000 * n_stem / county_births_estimate_decade,
      NA_real_
    )
  )]
  panel <- rbindlist(list(panel, fallback), fill = TRUE, use.names = TRUE)
}

out <- merge(panel, amws_decade, by = c("GEOID", "decade"), all.x = TRUE)
for (v in c("n_amws_1906_1955_dedup", "n_amws_1986", "n_amws")) {
  set(out, which(is.na(out[[v]])), v, 0L)
}
out[, amws_per_1000_pop := fifelse(
  is.finite(population) & population > 0,
  1000 * n_amws / population,
  NA_real_
)]
out[, amws_per_1000_births := fifelse(
  is.finite(county_births_estimate_decade) & county_births_estimate_decade > 0,
  1000 * n_amws / county_births_estimate_decade,
  NA_real_
)]
out[, log1p_n_amws := log1p(n_amws)]

expected_totals <- annual[, .(
  early = sum(n_amws_1906_1955_dedup, na.rm = TRUE),
  ed1986 = sum(n_amws_1986, na.rm = TRUE),
  total = sum(n_amws, na.rm = TRUE)
)]
actual_totals <- out[, .(
  early = sum(n_amws_1906_1955_dedup),
  ed1986 = sum(n_amws_1986),
  total = sum(n_amws)
)]
if (!identical(as.numeric(expected_totals), as.numeric(actual_totals))) {
  stop("Annual and decennial AMWS totals do not reconcile")
}

setorder(out, GEOID, decade)
fwrite(out, output_file)

summary_dt <- data.table(
  metric = c(
    "panel_rows", "panel_counties", "panel_min_decade", "panel_max_decade",
    "n_amws_1906_1955_dedup", "n_amws_1986", "n_amws_total",
    "component_reconciliation_failures", "annual_fallback_amws_births",
    "annual_fallback_rows",
    "hyde_rows"
  ),
  value = c(
    nrow(out), uniqueN(out$GEOID), min(out$decade), max(out$decade),
    actual_totals$early, actual_totals$ed1986, actual_totals$total,
    out[, sum(n_amws != n_amws_1906_1955_dedup + n_amws_1986)],
    fallback_births, nrow(fallback_keys),
    out[population_source == "hyde", .N]
  )
)
fwrite(summary_dt, summary_file)

cat("wrote", output_file, "\n")
cat("wrote", summary_file, "\n")
cat(
  "rows:", nrow(out), " counties:", uniqueN(out$GEOID),
  " decades:", min(out$decade), "-", max(out$decade),
  " AMWS births:", sum(out$n_amws), "\n"
)
