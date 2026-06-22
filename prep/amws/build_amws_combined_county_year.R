###############################################################################
# Yearly county-level panel of AMWS combined post-dedup births.
#
# Builds on the existing Wikipedia yearly panel (us_panel_county_stem_year_1800)
# by merging in AMWS combined counts per county-year. Adds derived AMWS rates.
#
# Output: prep/output/us_panel_county_amws_combined_year.csv
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

# ---- AMWS combined: aggregate to county-year -------------------------------
amws <- fread(file.path(AMWS_OUTPUT, "amws_combined_us_geocoded.csv"))
amws <- amws[kept == TRUE & !is.na(birth_year) & !is.na(geoid)]
amws[, geoid5 := sprintf("%05d", as.integer(geoid))]
amws[, year   := as.integer(birth_year)]
amws_cy <- amws[, .(n_amws = .N), by = .(GEOID = geoid5, year)]

# ---- Wikipedia yearly panel (already has GEOID-year, pop, county_births) ---
wiki <- fread(file.path(out_root, "us_panel_county_stem_year_1800.csv"))
wiki[, GEOID := sprintf("%05d", as.integer(GEOID))]

# ---- Merge AMWS into yearly panel ------------------------------------------
p <- merge(wiki, amws_cy, by = c("GEOID", "year"), all.x = TRUE)
p[is.na(n_amws), n_amws := 0L]

# ---- Derived AMWS outcomes -------------------------------------------------
# Per 1000 county population
p[, amws_per_1000_pop := ifelse(population > 0, 1000 * n_amws / population, NA_real_)]
# Per 1000 estimated county births (national birth rate × pop)
p[, amws_per_1000_births := ifelse(county_births_estimate_year > 0,
                                    1000 * n_amws / county_births_estimate_year,
                                    NA_real_)]
# Per 100k pop, for parity with stem_per_100k
p[, amws_per_100k := ifelse(population > 0, 1e5 * n_amws / population, NA_real_)]
# log1p
p[, log1p_n_amws := log1p(n_amws)]
# Share of total notable (Wikipedia all-wiki + AMWS, deduped only crudely)
p[, amws_share_of_notable := ifelse((n_all_wiki + n_amws) > 0,
                                     n_amws / (n_all_wiki + n_amws),
                                     NA_real_)]

# Restrict to the AMWS coverage window 1840-1930
p_out <- p[year >= 1840 & year <= 1930]

fwrite(p_out, file.path(out_root, "us_panel_county_amws_combined_year.csv"))
cat("wrote", file.path(out_root, "us_panel_county_amws_combined_year.csv"), "\n")
cat("rows:", nrow(p_out),
    "  counties:", uniqueN(p_out$GEOID),
    "  years:", min(p_out$year), "-", max(p_out$year),
    "  total AMWS births in panel:", sum(p_out$n_amws), "\n")
