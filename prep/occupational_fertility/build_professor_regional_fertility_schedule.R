# Births per male professor by age, by Census region x urban/rural county and
# census, from professors' own co-resident children: the "middle ground"
# between one national schedule and each county's own realized children.
#
# Own-children method. In each census, a child aged k (0-9) linked by POPLOC to
# a professor aged a is a birth at the father's age b = a - k. Pooling the
# censuses of an era (1850-1880, 1900-1920, 1930-1950), the national age shape is
#   rate_era(b) = mean over k of children(a = b + k, k) / professors(a = b + k),
# smoothed by a 5-year moving average. Each census x group then gets a level by
# indirect standardisation: O = the group's children aged 0-9, E = the sum over
# its professors of the shape's expected children aged 0-9 given their age,
#   L = (O + 30 x L_census) / (E + 30),
# shrunk toward the census's national level L_census = O / E so that thin
# early-census cells borrow strength. Rates: births_at_age_nchild = L x rate_era.
#
# Groups: region (census_regions.R, from the state) x urban/rural, where a
# county is urban in a census if at least half its persons are URBAN = 2.
# Professors: men aged 20-69 coded OCC1950 010-029; counties with a historical
# ICPSR code (ending in 5) have no county_geoid and enter the national shape only.
# A group "national" carries each census's national level, for stock the
# channel builder cannot place in a group.
# Co-resident children miss children who died or left home and include step- and
# adopted children.
#
# Input:  IPUMS_1850_1950_FERTILITY_PARQUET_DIR (default D:/ipums_1850_1950_fullcount_fertility)
# Output: OCC_CHANNEL_OUT_DIR (default <Dropbox>/output/occupational_fertility/college_channel)
#   professor_birth_schedule_regional.csv   census x group x age rates
#   professor_fertility_group_levels.csv    O, E, raw and shrunk levels per census x group
#   professor_fertility_era_shape.csv       the national shape per era
#   county_urban_class.csv                  county x census urban share and class
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/build_professor_regional_fertility_schedule.R

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
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
source(file.path(repo_root, "prep/occupational_fertility/census_regions.R"))
PARQUET <- env_or("IPUMS_1850_1950_FERTILITY_PARQUET_DIR", function() "D:/ipums_1850_1950_fullcount_fertility")
OUT_DIR <- env_or("OCC_CHANNEL_OUT_DIR",
                  function() file.path(data_root(), "output/occupational_fertility/college_channel"))
ERAS <- list(`1850-1880` = c(1850L, 1860L, 1870L, 1880L), `1900-1920` = c(1900L, 1910L, 1920L),
             `1930-1950` = c(1930L, 1940L, 1950L))
PRIOR <- 30
URBAN_SHARE <- 0.5
AGES <- 15:90

say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
sql_path <- function(path) gsub("\\\\", "/", path)

# One census: professors and their children 0-9 by state, county and age, plus
# every county's urban share.
read_census <- function(con, year) {
  glob <- sql_path(file.path(PARQUET, paste0("YEAR=", year), "*.parquet"))
  DBI::dbExecute(con, "DROP TABLE IF EXISTS p")
  DBI::dbExecute(con, sprintf(paste(
    "CREATE TABLE p AS SELECT SERIAL, PERNUM, SEX, AGE, POPLOC, OCC1950, URBAN, STATEFIP,",
    "CASE WHEN COUNTYICP %% 10 = 0 THEN STATEFIP * 1000 + COUNTYICP // 10 END AS county_geoid",
    "FROM read_parquet('%s')"), glob))
  prof <- as.data.table(DBI::dbGetQuery(con, paste(
    "SELECT STATEFIP AS statefip, county_geoid, AGE AS age, COUNT(*) AS professors FROM p",
    "WHERE SEX = 1 AND OCC1950 BETWEEN 10 AND 29 AND AGE BETWEEN 20 AND 69 GROUP BY ALL")))
  # Equalities only in the join (a non-equality term makes DuckDB loop).
  kids <- as.data.table(DBI::dbGetQuery(con, paste(
    "SELECT f.STATEFIP AS statefip, f.county_geoid, f.AGE AS age, c.AGE AS k, COUNT(*) AS children",
    "FROM p c JOIN p f ON f.SERIAL = c.SERIAL AND f.PERNUM = c.POPLOC",
    "WHERE c.AGE BETWEEN 0 AND 9 AND f.SEX = 1 AND f.OCC1950 BETWEEN 10 AND 29",
    "AND f.AGE BETWEEN 20 AND 69 GROUP BY ALL")))
  urban <- as.data.table(DBI::dbGetQuery(con, paste(
    "SELECT county_geoid, STATEFIP AS statefip, AVG(CASE WHEN URBAN = 2 THEN 1.0 ELSE 0.0 END) AS urban_share",
    "FROM p WHERE county_geoid IS NOT NULL AND URBAN IN (1, 2) GROUP BY ALL")))
  DBI::dbExecute(con, "DROP TABLE p")
  say("Census ", year, ": ", sum(prof$professors), " professors, ", sum(kids$children), " children 0-9")
  list(prof = prof[, census := year], kids = kids[, census := year], urban = urban[, census := year])
}

main <- function() {
  years <- sort(as.integer(sub("^YEAR=", "", basename(Sys.glob(file.path(PARQUET, "YEAR=*"))))))
  stop_if(!length(years), "No YEAR partitions under ", PARQUET)
  stop_if(!setequal(years, unlist(ERAS)), "Censuses in the Parquet do not match the eras")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads = 12")
  DBI::dbExecute(con, "SET memory_limit = '20GB'")
  pieces <- lapply(years, function(y) read_census(con, y))
  prof <- rbindlist(lapply(pieces, `[[`, "prof"))
  kids <- rbindlist(lapply(pieces, `[[`, "kids"))
  urban <- rbindlist(lapply(pieces, `[[`, "urban"))
  era_of <- function(census) names(ERAS)[vapply(census, function(y) which(vapply(ERAS, `%in%`, NA, x = y)), 1L)]
  prof[, era := era_of(census)]; kids[, era := era_of(census)]

  # County groups: region x urban/rural, per census.
  urban[, `:=`(region = census_region(statefip), urban = urban_share >= URBAN_SHARE)]
  stop_if(urban[, anyNA(region)], "A state FIPS code has no Census region")
  urban[, group := paste(region, fifelse(urban, "urban", "rural"), sep = "_")]
  assign_group <- function(d) {
    d <- merge(d, urban[, .(county_geoid, census, group)], by = c("county_geoid", "census"), all.x = TRUE)
    d
  }

  # National age shape per era (all professors, with or without a county code).
  shape <- rbindlist(lapply(names(ERAS), function(e) {
    P <- prof[era == e, .(professors = sum(professors)), by = age]
    C <- kids[era == e, .(children = sum(children)), by = .(age, k)]
    cell <- merge(C, P, by = "age")[, rate := children / professors][, b := age - k]
    raw <- cell[, .(rate = mean(rate), terms = .N), by = b][order(b)]
    full <- data.table(b = AGES)[raw, on = "b", `:=`(rate = i.rate, terms = i.terms)]
    full[is.na(rate), rate := 0]
    # 5-year centred moving average.
    full[, rate_smooth := frollmean(rate, 5L, align = "center", fill = NA)]
    full[is.na(rate_smooth), rate_smooth := rate]
    full[, era := e]
  }))
  stop_if(any(shape$rate_smooth < 0), "Negative shape rates")

  # Expected children aged 0-9 for a professor aged a: sum of the shape at a - k.
  expected_0_9 <- shape[, {
    r <- setNames(rate_smooth, b)
    .(age = 20:69, expected = vapply(20:69, function(a) sum(r[as.character(a - 0:9)], na.rm = TRUE), 0))
  }, by = era]
  prof_g <- assign_group(merge(prof, expected_0_9, by = c("era", "age")))
  kids_g <- assign_group(kids)
  national <- merge(prof_g[, .(E = sum(professors * expected)), by = census],
                    kids_g[, .(O = sum(children)), by = census], by = "census")[, level_census := O / E]
  groups <- CJ(census = years, group = sort(unique(urban$group)))
  levels <- merge(prof_g[!is.na(group), .(professors = sum(professors), E = sum(professors * expected)),
                         by = .(census, group)],
                  kids_g[!is.na(group), .(O = sum(children)), by = .(census, group)],
                  by = c("census", "group"), all = TRUE)
  levels <- merge(groups, levels, by = c("census", "group"), all.x = TRUE)
  for (col in c("professors", "E", "O")) set(levels, which(is.na(levels[[col]])), col, 0)
  levels <- merge(levels, national[, .(census, level_census)], by = "census")
  levels[, `:=`(level_raw = fifelse(E > 0, O / E, NA_real_),
                level = (O + PRIOR * level_census) / (E + PRIOR), era = era_of(census))]

  with_national <- rbind(levels[, .(census, group, era, level)],
                         national[, .(census, group = "national", era = era_of(census), level = level_census)])
  schedule <- merge(with_national,
                    shape[, .(era, age = b, rate_smooth)], by = "era", allow.cartesian = TRUE)
  schedule[, births_at_age_nchild := level * rate_smooth]
  schedule <- schedule[, .(census, group, age, births_at_age_nchild)][order(census, group, age)]
  stop_if(any(schedule$births_at_age_nchild < 0), "Negative birth rates")

  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  fwrite(schedule, file.path(OUT_DIR, "professor_birth_schedule_regional.csv"))
  fwrite(levels[order(census, group)], file.path(OUT_DIR, "professor_fertility_group_levels.csv"))
  fwrite(shape[, .(era, age = b, rate, rate_smooth, terms)], file.path(OUT_DIR, "professor_fertility_era_shape.csv"))
  fwrite(urban[order(county_geoid, census), .(county_geoid, census, statefip, region, urban_share, urban, group)],
         file.path(OUT_DIR, "county_urban_class.csv"))
  lifetime <- shape[, .(lifetime_children = sum(rate_smooth)), by = era]
  jsonlite::write_json(list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), source = PARQUET, eras = ERAS,
    prior_weight = PRIOR, urban_share_threshold = URBAN_SHARE,
    lifetime_children_by_era_national_shape = lifetime, national_levels = national),
    file.path(OUT_DIR, "professor_birth_schedule_regional_manifest.json"),
    pretty = TRUE, auto_unbox = TRUE, digits = NA)
  print(lifetime)
  print(dcast(levels, group ~ census, value.var = "level", fun.aggregate = function(x) round(x, 2)))
}

main()
