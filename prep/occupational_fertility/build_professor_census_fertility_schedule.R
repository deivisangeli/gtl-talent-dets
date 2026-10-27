# Births per male professor by age, measured in the census from professors'
# wives: the census counterpart of the AMS 1956 schedule
# (build_professor_birth_schedule.R), for build_professor_ams_channel.R.
#
# Two eras, each applied to the faculty stock of the censuses it is closest to:
#   1900 + 1910  CHSURV (children surviving), asked of every ever-married woman
#                -> faculty enumerated 1850-1920
#   1940         CHBORN (children ever born), asked of sample-line ever-married
#                women only (5%; CHSURV was not asked) -> faculty 1930-1950
#
# Professors: men aged 20-69 coded OCC1950 010-029. A professor's children are
# his wife's count (SPLOC link, decoded value - 1; children by every husband).
# By 5-year husband-age bin,
#   m(a) = share ever married x mean count among couples whose wife answered,
# so unmarried professors count 0 and married men without a wife present take
# the couples' mean. Read as a synthetic cohort, m is cumulative children by
# age: it is made non-decreasing by a couple-weighted isotonic fit (declines at
# older ages come from child deaths, older cohorts or, in 1940, small bins),
# interpolated to single years (0 at age 15, flat after the last bin), and
# births_at_age(a) = m(a+1) - m(a) is the single-year rate the channel builder
# sums over windows.
#
# Input:  IPUMS_1850_1950_FERTILITY_PARQUET_DIR (default D:/ipums_1850_1950_fullcount_fertility)
# Output: OCC_CHANNEL_OUT_DIR (default <Dropbox>/output/occupational_fertility/college_channel)
#   professor_birth_schedule_census.csv, professor_birth_schedule_census_manifest.json
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/build_professor_census_fertility_schedule.R

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
PARQUET <- env_or("IPUMS_1850_1950_FERTILITY_PARQUET_DIR", function() "D:/ipums_1850_1950_fullcount_fertility")
OUT_DIR <- env_or("OCC_CHANNEL_OUT_DIR",
                  function() file.path(data_root(), "output/occupational_fertility/college_channel"))
ERAS <- list(
  chsurv_1900_1910 = list(years = c(1900L, 1910L), variable = "CHSURV",
                          censuses = c(1850L, 1860L, 1870L, 1880L, 1900L, 1910L, 1920L)),
  chborn_1940 = list(years = 1940L, variable = "CHBORN", censuses = c(1930L, 1940L, 1950L)))
AGES <- 15:90

# Weighted isotonic (non-decreasing) fit by pool-adjacent-violators. The 1940
# sample-line bins hold 24-281 couples, so m wobbles at older ages; clipping
# each dip to zero would bank the noise as births, a monotone fit does not.
isotonic <- function(y, w) {
  blocks <- lapply(seq_along(y), function(i) list(value = y[i], weight = w[i], size = 1L))
  i <- 1L
  while (i < length(blocks)) {
    if (blocks[[i]]$value > blocks[[i + 1L]]$value) {
      a <- blocks[[i]]; b <- blocks[[i + 1L]]
      blocks[[i]] <- list(value = (a$value * a$weight + b$value * b$weight) / (a$weight + b$weight),
                          weight = a$weight + b$weight, size = a$size + b$size)
      blocks[[i + 1L]] <- NULL
      i <- max(1L, i - 1L)
    } else {
      i <- i + 1L
    }
  }
  unlist(lapply(blocks, function(b) rep(b$value, b$size)))
}

say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
sql_path <- function(path) gsub("\\\\", "/", path)

# Professors by 5-year age bin: men, ever married, couples whose wife answered,
# and the sum of the wives' decoded counts (raw code - 1; 1-96 are answers).
bins_for <- function(con, year, variable) {
  glob <- sql_path(file.path(PARQUET, paste0("YEAR=", year), "*.parquet"))
  out <- as.data.table(DBI::dbGetQuery(con, sprintf(paste(
    "WITH p AS (SELECT SERIAL, PERNUM, SEX, AGE, MARST, SPLOC, OCC1950, %s AS kids",
    "FROM read_parquet('%s'))",
    "SELECT h.AGE // 5 * 5 AS age_bin, COUNT(*) AS professors,",
    "COUNT(*) FILTER (WHERE h.MARST BETWEEN 1 AND 5) AS ever_married,",
    "COUNT(*) FILTER (WHERE w.SEX = 2 AND w.kids BETWEEN 1 AND 96) AS couples_answered,",
    "SUM(CASE WHEN w.SEX = 2 AND w.kids BETWEEN 1 AND 96 THEN w.kids - 1 ELSE 0 END) AS children",
    "FROM p h LEFT JOIN p w ON w.SERIAL = h.SERIAL AND w.PERNUM = h.SPLOC",
    "WHERE h.SEX = 1 AND h.OCC1950 BETWEEN 10 AND 29 AND h.AGE BETWEEN 20 AND 69",
    "GROUP BY ALL ORDER BY 1"), variable, glob)))
  out[, year := year]
}

main <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads = 12")
  schedules <- list(); curves <- list()
  for (era in names(ERAS)) {
    e <- ERAS[[era]]
    raw <- rbindlist(lapply(e$years, function(y) bins_for(con, y, e$variable)))
    b <- raw[, lapply(.SD, sum), by = age_bin,
             .SDcols = c("professors", "ever_married", "couples_answered", "children")]
    stop_if(any(b$couples_answered == 0), era, ": an age bin has no couple with an answer")
    b[, `:=`(share_ever_married = ever_married / professors,
             children_per_couple = children / couples_answered)]
    b[, m := share_ever_married * children_per_couple]
    setorder(b, age_bin)
    b[, m_monotone := isotonic(m, couples_answered)]
    b[, midpoint := age_bin + 2.5]
    say(era, ": ", sum(b$professors), " professors, ", sum(b$couples_answered), " couples with an answer")
    # Cumulative children by single year: 0 at 15, linear between bin midpoints, flat after.
    m_single <- approx(c(15, b$midpoint), c(0, b$m_monotone), xout = AGES, rule = 2)$y
    rates <- pmax(c(diff(m_single), 0), 0)
    curves[[era]] <- b[, .(era = era, age_bin, professors, ever_married, couples_answered,
                           share_ever_married, children_per_couple, m, m_monotone)]
    schedules[[era]] <- CJ(census = e$censuses, age = AGES)[
      , births_at_age_census := rates[match(age, AGES)]][, era := era]
  }
  schedule <- rbindlist(schedules)
  curve <- rbindlist(curves)
  stop_if(any(schedule$births_at_age_census < 0), "Negative birth rates")
  lifetime <- schedule[census %in% c(1910L, 1940L), .(lifetime = sum(births_at_age_census)), by = era]

  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  fwrite(schedule[order(census, age)], file.path(OUT_DIR, "professor_birth_schedule_census.csv"))
  fwrite(curve, file.path(OUT_DIR, "professor_birth_schedule_census_curve.csv"))
  jsonlite::write_json(list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), source = PARQUET,
    eras = lapply(ERAS, function(e) list(years = e$years, variable = e$variable, applied_to = e$censuses)),
    lifetime_children_per_professor = lifetime,
    couples_answered = curve[, .(couples = sum(couples_answered)), by = era],
    rule = "births_at_age(a) = max(m(a+1) - m(a), 0); m = share ever married x mean wife's count by husband age"),
    file.path(OUT_DIR, "professor_birth_schedule_census_manifest.json"),
    pretty = TRUE, auto_unbox = TRUE, digits = NA)
  print(curve[, .(era, age_bin, professors, couples_answered, share_ever_married = round(share_ever_married, 3),
                  children_per_couple = round(children_per_couple, 2), m = round(m, 2))])
  print(lifetime)
}

main()
