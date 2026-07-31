###############################################################################
# Yearly county-level panel of consolidated AMWS US-born births, 1800-1960.
#
# Combines:
#   - 1906/1938/1955 deduplicated across editions
#   - 1986 edition, not deduplicated against earlier editions
#
# Builds on the existing yearly county panel (us_panel_county_stem_year_1800),
# which already contains annual interpolated population and birth denominators.
#
# Outputs:
#   output/us_panel_county_amws_combined_year.csv
#   output/amws/amws_1986_see_previous_excluded_from_panel.csv
#   output/amws/amws_consolidated_county_year_summary.csv
###############################################################################
suppressPackageStartupMessages({
  library(data.table)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
out_root <- DATA_OUTPUT

normalize_geoid <- function(x) {
  out <- suppressWarnings(as.integer(x))
  sprintf("%05d", out)
}

has_value <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & x != "" & !toupper(x) %in% c("NA", "N/A")
}

kept_flag <- function(x) {
  as.character(x) %in% c("TRUE", "True", "true", "1")
}

first_pos <- function(pattern, text) {
  pos <- regexpr(pattern, text, ignore.case = TRUE, perl = TRUE)
  as.integer(pos)
}

# A small number of 1986 rows still contain a "see previous edition" entry
# followed by a different person's "b ..." birth string. In those rows the
# parsed birth location/year belongs to the following entry, not to the
# "see previous" entry. Exclude rows where "see previous" appears before any
# visible birth marker.
is_see_previous_contaminated <- function(text) {
  text <- as.character(text)
  text[is.na(text)] <- ""
  see_pos <- first_pos("\\bsee\\s+prev(?:ious)?(?:\\s+edition)?\\b", text)
  birth_pos <- first_pos("(^|[[:space:],.;])b[[:space:]]+", text)
  see_pos > 0L & (birth_pos < 0L | birth_pos > see_pos)
}

# ---- 1906/1938/1955: already deduplicated across editions ------------------
amws_early <- fread(file.path(AMWS_OUTPUT, "amws_combined_us_geocoded.csv"))
required_early <- c("kept", "birth_year", "geoid")
missing_early <- setdiff(required_early, names(amws_early))
if (length(missing_early)) {
  stop("Missing required columns in AMWS 1906/1938/1955 file: ",
       paste(missing_early, collapse = ", "))
}

amws_early_valid <- amws_early[
  kept_flag(kept) & has_value(birth_year) & has_value(geoid)
]
amws_early_valid[, year := suppressWarnings(as.integer(birth_year))]
amws_early_valid[, GEOID := normalize_geoid(geoid)]
amws_early_valid <- amws_early_valid[
  !is.na(year) & year >= 1800L & year <= 1960L & !is.na(GEOID)
]

amws_early_cy <- amws_early_valid[
  ,
  .(n_amws_1906_1955_dedup = .N),
  by = .(GEOID, year)
]

# ---- 1986: include valid US-geocoded rows, no cross-edition dedup -----------
ed16_file <- file.path(
  TALENT_DETS_DATA_DIR,
  "Data",
  "processed",
  "amws",
  "amws_ed86_final.csv"
)
if (!file.exists(ed16_file)) stop("Missing AMWS 1986 source file: ", ed16_file)
amws_1986 <- fread(ed16_file)
required_1986 <- c("birth_year", "birth_country", "geo_geoid", "raw_text_adjusted")
missing_1986 <- setdiff(required_1986, names(amws_1986))
if (length(missing_1986)) {
  stop("Missing required columns in AMWS 1986 file: ",
       paste(missing_1986, collapse = ", "))
}

amws_1986[, see_previous_contaminated :=
            is_see_previous_contaminated(raw_text_adjusted)]
amws_1986[, year := suppressWarnings(as.integer(birth_year))]
amws_1986[, GEOID := normalize_geoid(geo_geoid)]

amws_1986_candidate <- amws_1986[
  birth_country == "USA" &
    has_value(birth_year) & has_value(geo_geoid) &
    !is.na(year) & year >= 1800L & year <= 1960L & !is.na(GEOID)
]

amws_1986_excluded <- amws_1986_candidate[see_previous_contaminated == TRUE]
excluded_file <- file.path(
  AMWS_OUTPUT,
  "amws_1986_see_previous_excluded_from_panel.csv"
)
fwrite(amws_1986_excluded, excluded_file)

amws_1986_valid <- amws_1986_candidate[see_previous_contaminated != TRUE]
amws_1986_cy <- amws_1986_valid[
  ,
  .(n_amws_1986 = .N),
  by = .(GEOID, year)
]

# ---- Annual population panel -----------------------------------------------
panel <- fread(file.path(out_root, "us_panel_county_stem_year_1800.csv"))
required_panel <- c(
  "GEOID", "year", "population", "population_source",
  "us_births_year", "us_pop_year", "us_birth_rate_year",
  "county_births_estimate_year", "n_all_wiki"
)
missing_panel <- setdiff(required_panel, names(panel))
if (length(missing_panel)) {
  stop("Missing required columns in yearly population panel: ",
       paste(missing_panel, collapse = ", "))
}

panel[, GEOID := normalize_geoid(GEOID)]
panel[, year := as.integer(year)]
panel <- panel[
  year >= 1800L & year <= 1960L &
    population_source %in% c("nhgis", "manual", "merged_nyc")
]

# ---- Merge AMWS counts ------------------------------------------------------
p_out <- merge(panel, amws_early_cy, by = c("GEOID", "year"), all.x = TRUE)
p_out <- merge(p_out, amws_1986_cy, by = c("GEOID", "year"), all.x = TRUE)
p_out[is.na(n_amws_1906_1955_dedup), n_amws_1906_1955_dedup := 0L]
p_out[is.na(n_amws_1986), n_amws_1986 := 0L]
p_out[, n_amws := n_amws_1906_1955_dedup + n_amws_1986]
if (p_out[, any(n_amws != n_amws_1906_1955_dedup + n_amws_1986)]) {
  stop("AMWS component counts do not reconcile")
}

# ---- Derived AMWS outcomes --------------------------------------------------
p_out[, amws_per_1000_pop := ifelse(
  population > 0,
  1000 * n_amws / population,
  NA_real_
)]
p_out[, amws_per_1000_births := ifelse(
  county_births_estimate_year > 0,
  1000 * n_amws / county_births_estimate_year,
  NA_real_
)]
p_out[, amws_per_100k := ifelse(
  population > 0,
  1e5 * n_amws / population,
  NA_real_
)]
p_out[, log1p_n_amws := log1p(n_amws)]
p_out[, amws_share_of_notable := ifelse(
  (n_all_wiki + n_amws) > 0,
  n_amws / (n_all_wiki + n_amws),
  NA_real_
)]

setorder(p_out, GEOID, year)

panel_file <- file.path(out_root, "us_panel_county_amws_combined_year.csv")
fwrite(p_out, panel_file)

summary_dt <- data.table(
  metric = c(
    "amws_1986_source_file",
    "amws_1986_source_mtime",
    "amws_early_dedup_scope",
    "amws_combination_rule",
    "panel_rows",
    "panel_counties",
    "panel_min_year",
    "panel_max_year",
    "panel_rows_with_hyde_population_source",
    "amws_1906_1955_dedup_valid_rows",
    "amws_1906_1955_dedup_births_in_panel",
    "amws_1986_candidate_rows",
    "amws_1986_excluded_see_previous_rows",
    "amws_1986_valid_rows",
    "amws_1986_births_in_panel",
    "total_amws_births_in_panel",
    "panel_rows_missing_population",
    "panel_rows_missing_births_denominator"
  ),
  value = as.character(c(
    basename(ed16_file),
    format(file.info(ed16_file)$mtime, "%Y-%m-%d %H:%M:%S %Z"),
    "1906+1938+1955 only; keep earliest matched appearance",
    "n_amws = n_amws_1906_1955_dedup + n_amws_1986; no cross-dedup with 1986",
    nrow(p_out),
    uniqueN(p_out$GEOID),
    min(p_out$year),
    max(p_out$year),
    p_out[population_source == "hyde", .N],
    nrow(amws_early_valid),
    sum(p_out$n_amws_1906_1955_dedup),
    nrow(amws_1986_candidate),
    nrow(amws_1986_excluded),
    nrow(amws_1986_valid),
    sum(p_out$n_amws_1986),
    sum(p_out$n_amws),
    p_out[is.na(population) | population <= 0, .N],
    p_out[is.na(county_births_estimate_year) |
            county_births_estimate_year <= 0, .N]
  ))
)
summary_file <- file.path(AMWS_OUTPUT, "amws_consolidated_county_year_summary.csv")
fwrite(summary_dt, summary_file)

cat("wrote", panel_file, "\n")
cat("wrote", excluded_file, "\n")
cat("wrote", summary_file, "\n")
cat("rows:", nrow(p_out),
    "  counties:", uniqueN(p_out$GEOID),
    "  years:", min(p_out$year), "-", max(p_out$year),
    "  total AMWS births in panel:", sum(p_out$n_amws), "\n")
cat("AMWS 1906/1938/1955 dedup births in panel:",
    sum(p_out$n_amws_1906_1955_dedup), "\n")
cat("AMWS 1986 valid births in panel:", sum(p_out$n_amws_1986), "\n")
cat("AMWS 1986 excluded see-previous rows:", nrow(amws_1986_excluded), "\n")
