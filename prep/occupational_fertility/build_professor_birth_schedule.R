# Births per male professor by age, from American Men of Science 1956: the
# fertility schedule that turns a census count of faculty into predicted
# faculty children (build_professor_ams_channel.R).
#
# Sample: male professors (ever held a faculty rank, mos1956_helpers.R) born
# 1870-1905, whose fertility was complete by the 1955-56 edition; childless and
# unmarried men included, since the census faculty stock includes them too.
#
# The directory gives the number of children and the year of marriage (first
# child year only for 124 men), so timing is assumed:
#   spaced     births at ages m+1, m+3.5, m+6, ... (m = age at marriage, 2.5
#              years apart); the headline.
#   uniform10  the same children spread evenly over the 10 years after marriage.
# Men with children but no usable marriage year get the median marriage age.
#
# f10(a) = expected births in the next ten years (ages a to a+10) per male
# professor aged a, for a = 15..90; f10_shift3 covers ages a-3 to a+7, for the
# alternative decade binning (years ending 7 to 6). births_at_age_* are the
# single-year rates (births at ages a to a+1 per professor), from which any
# window can be summed; over a lifetime they reproduce mean completed children.
#
# Output: OCC_CHANNEL_OUT_DIR (default <Dropbox>/output/occupational_fertility/college_channel)
#   professor_birth_schedule.csv, professor_birth_schedule_manifest.json
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/build_professor_birth_schedule.R

suppressPackageStartupMessages(library(data.table))

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
say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
source(file.path(repo_root, "prep/occupational_fertility/mos1956_helpers.R"))

OUT_DIR <- Sys.getenv("OCC_CHANNEL_OUT_DIR",
                      unset = file.path(data_root, "output/occupational_fertility/college_channel"))
COHORTS <- c(1870L, 1905L)
SPACING <- 2.5
AGES <- 15:90

main <- function() {
  d <- mos1956_load(data_root)$d
  prof <- d[gender == "male" & professor & between(birthyear, COHORTS[1], COHORTS[2])]
  prof[, marriage_age := yearofmarriage - birthyear]
  prof[!is.na(marriage_age) & (marriage_age < 15 | marriage_age > 70), marriage_age := NA_real_]
  median_marriage <- prof[!is.na(marriage_age), as.numeric(median(marriage_age))]
  imputed <- prof[children > 0 & is.na(marriage_age), .N]
  prof[children > 0 & is.na(marriage_age), marriage_age := median_marriage]
  n <- nrow(prof)
  say(n, " male professors born ", COHORTS[1], "-", COHORTS[2], "; median marriage age ",
      median_marriage, "; ", imputed, " fathers given the median")

  # One row per birth: spaced at fractional ages, uniform as ten tenth-shares.
  parents <- prof[children > 0]
  spaced <- parents[, .(age = marriage_age + 1 + SPACING * (seq_len(children) - 1)), by = unique_id][
    , weight := 1]
  uniform <- parents[, .(age = marriage_age + 0:9 + 0.5, weight = children / 10), by = unique_id]
  # f10: ages a to a+10, the standard decade that starts at the census.
  # f10_shift3: ages a-3 to a+7, the alternative decade (years 7-6) around it.
  f10 <- function(births, a, lag = 0) births[age >= a - lag & age < a - lag + 10, sum(weight)] / n
  schedule <- data.table(age = AGES)
  schedule[, `:=`(f10_spaced = vapply(age, function(a) f10(spaced, a), 0),
                  f10_uniform10 = vapply(age, function(a) f10(uniform, a), 0),
                  f10_shift3_spaced = vapply(age, function(a) f10(spaced, a, 3), 0),
                  f10_shift3_uniform10 = vapply(age, function(a) f10(uniform, a, 3), 0),
                  births_at_age_spaced = vapply(age, function(a) spaced[age >= a & age < a + 1, .N] / n, 0),
                  births_at_age_uniform10 = vapply(age, function(a)
                    uniform[age >= a & age < a + 1, sum(weight)] / n, 0))]

  mean_children <- prof[, mean(children)]
  lifetime <- c(spaced = spaced[, sum(weight)] / n, uniform10 = uniform[, sum(weight)] / n)
  stop_if(any(abs(lifetime - mean_children) > 1e-9),
          "Births assigned do not add up to mean completed children")
  outside <- spaced[age < min(AGES) | age >= max(AGES) + 1, .N]
  stop_if(outside > 0, outside, " births fall outside ages ", min(AGES), "-", max(AGES))
  stop_if(abs(schedule[, sum(births_at_age_spaced)] - mean_children) > 1e-9 ||
            abs(schedule[, sum(births_at_age_uniform10)] - mean_children) > 1e-9,
          "Single-year rates do not integrate to mean completed children")

  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  fwrite(schedule, file.path(OUT_DIR, "professor_birth_schedule.csv"))
  jsonlite::write_json(list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source = "American Men of Science 1956, Kim & Moser replication package (mos1956_helpers.R)",
    sample = sprintf("male professors born %d-%d (ever held a faculty rank)", COHORTS[1], COHORTS[2]),
    professors = n, fathers = nrow(parents), mean_children = mean_children,
    share_childless = prof[, mean(children == 0)], median_marriage_age = median_marriage,
    fathers_given_median_marriage_age = imputed, spacing_years = SPACING,
    peak_f10_spaced = schedule[which.max(f10_spaced), .(age, f10_spaced)],
    peak_f10_uniform10 = schedule[which.max(f10_uniform10), .(age, f10_uniform10)]),
    file.path(OUT_DIR, "professor_birth_schedule_manifest.json"), pretty = TRUE, auto_unbox = TRUE,
    digits = NA)
  say("mean children ", round(mean_children, 3), "; peak f10 (spaced) ",
      round(max(schedule$f10_spaced), 3), " at age ", schedule[which.max(f10_spaced), age])
  print(schedule[age %in% seq(20, 70, 5)])
}

main()
