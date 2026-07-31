###############################################################################
# Yearly county-level panel of AMWS 1986 births.
#
# Builds on the existing Wikipedia yearly panel (us_panel_county_stem_year_1800)
# by merging in AMWS edition 16 / 1986 counts per county-year. Adds derived AMWS
# rates analogous to the combined AMWS county-year output.
#
# Outputs:
#   output/amws/amws_1986_county_year.csv
#   output/amws/amws_1986_county_year_summary.csv
#   output/amws/amws_1986_county_year_unmatched_to_population_panel.csv
#   output/us_panel_county_amws_1986_year.csv
###############################################################################
suppressPackageStartupMessages({
  library(data.table)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
out_root <- DATA_OUTPUT

ed16_file <- file.path(
  TALENT_DETS_DATA_DIR,
  "Data",
  "processed",
  "amws",
  "amws_ed86_final.csv"
)
panel_file <- file.path(out_root, "us_panel_county_stem_year_1800.csv")

if (!file.exists(ed16_file)) {
  stop("Missing AMWS 1986 geocoded file: ", ed16_file)
}
if (!file.exists(panel_file)) {
  stop("Missing yearly population panel: ", panel_file)
}

# ---- AMWS 1986: aggregate to county-year -----------------------------------
has_value <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & x != "" & !toupper(x) %in% c("NA", "N/A")
}

amws <- fread(ed16_file)

required_amws_cols <- c("birth_year", "birth_country", "geo_geoid")
missing_amws_cols <- setdiff(required_amws_cols, names(amws))
if (length(missing_amws_cols)) {
  stop("Missing required columns in AMWS 1986 file: ",
       paste(missing_amws_cols, collapse = ", "))
}

amws_valid <- amws[
  birth_country == "USA" &
    has_value(birth_year) &
    has_value(geo_geoid)
]
amws_valid[, year := suppressWarnings(as.integer(birth_year))]
amws_valid[, geoid_int := suppressWarnings(as.integer(geo_geoid))]
amws_valid <- amws_valid[!is.na(year) & !is.na(geoid_int)]
amws_valid <- amws_valid[year >= 1800 & year <= 1986]
amws_valid[, GEOID := sprintf("%05d", geoid_int)]

amws_cy <- amws_valid[, .(n_amws = .N), by = .(GEOID, year)]
setorder(amws_cy, GEOID, year)

amws_cy_file <- file.path(AMWS_OUTPUT, "amws_1986_county_year.csv")
fwrite(amws_cy, amws_cy_file)

# ---- Wikipedia yearly panel (already has GEOID-year, pop, county_births) ----
wiki <- fread(panel_file)
required_panel_cols <- c(
  "GEOID", "year", "population", "county_births_estimate_year", "n_all_wiki"
)
missing_panel_cols <- setdiff(required_panel_cols, names(wiki))
if (length(missing_panel_cols)) {
  stop("Missing required columns in yearly population panel: ",
       paste(missing_panel_cols, collapse = ", "))
}

wiki[, GEOID := sprintf("%05d", as.integer(GEOID))]
wiki[, year := as.integer(year)]

unmatched_amws <- amws_cy[!wiki[, .(GEOID, year)], on = .(GEOID, year)]
unmatched_file <- file.path(
  AMWS_OUTPUT,
  "amws_1986_county_year_unmatched_to_population_panel.csv"
)
fwrite(unmatched_amws, unmatched_file)

# ---- Merge AMWS into yearly panel ------------------------------------------
p <- merge(wiki, amws_cy, by = c("GEOID", "year"), all.x = TRUE)
p[is.na(n_amws), n_amws := 0L]

# ---- Derived AMWS outcomes -------------------------------------------------
p[, amws_per_1000_pop := ifelse(population > 0,
                                 1000 * n_amws / population,
                                 NA_real_)]
p[, amws_per_1000_births := ifelse(county_births_estimate_year > 0,
                                    1000 * n_amws / county_births_estimate_year,
                                    NA_real_)]
p[, amws_per_100k := ifelse(population > 0,
                             1e5 * n_amws / population,
                             NA_real_)]
p[, log1p_n_amws := log1p(n_amws)]
p[, amws_share_of_notable := ifelse((n_all_wiki + n_amws) > 0,
                                     n_amws / (n_all_wiki + n_amws),
                                     NA_real_)]

coverage_min_year <- min(amws_cy$year)
coverage_max_year <- max(amws_cy$year)
p_out <- p[year >= coverage_min_year & year <= coverage_max_year]
setorder(p_out, GEOID, year)

panel_out_file <- file.path(out_root, "us_panel_county_amws_1986_year.csv")
fwrite(p_out, panel_out_file)

summary_dt <- data.table(
  metric = c(
    "amws_1986_source_file",
    "input_rows",
    "rows_with_birth_year_and_geoid",
    "county_year_rows",
    "counties_with_amws_births",
    "min_birth_year",
    "max_birth_year",
    "total_amws_births_in_county_year",
    "unmatched_county_year_rows",
    "unmatched_amws_births",
    "panel_rows",
    "panel_counties",
    "panel_min_year",
    "panel_max_year",
    "total_amws_births_in_panel",
    "panel_rows_missing_population",
    "panel_rows_missing_births_denominator"
  ),
  value = as.character(c(
    basename(ed16_file),
    nrow(amws),
    nrow(amws_valid),
    nrow(amws_cy),
    uniqueN(amws_cy$GEOID),
    coverage_min_year,
    coverage_max_year,
    sum(amws_cy$n_amws),
    nrow(unmatched_amws),
    sum(unmatched_amws$n_amws),
    nrow(p_out),
    uniqueN(p_out$GEOID),
    min(p_out$year),
    max(p_out$year),
    sum(p_out$n_amws),
    p_out[is.na(population) | population <= 0, .N],
    p_out[is.na(county_births_estimate_year) | county_births_estimate_year <= 0, .N]
  ))
)
summary_file <- file.path(AMWS_OUTPUT, "amws_1986_county_year_summary.csv")
fwrite(summary_dt, summary_file)

cat("wrote", amws_cy_file, "\n")
cat("wrote", panel_out_file, "\n")
cat("wrote", summary_file, "\n")
cat("wrote", unmatched_file, "\n")
cat("rows:", nrow(p_out),
    "  counties:", uniqueN(p_out$GEOID),
    "  years:", min(p_out$year), "-", max(p_out$year),
    "  total AMWS births in panel:", sum(p_out$n_amws), "\n")
cat("unmatched AMWS county-years:", nrow(unmatched_amws),
    "  unmatched AMWS births:", sum(unmatched_amws$n_amws), "\n")
