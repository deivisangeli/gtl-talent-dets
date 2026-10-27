#!/usr/bin/env Rscript
# Validate the faculty-count release built by 01_build_faculty_counts.R.
# Hard checks stop the script; plausibility flags go to review_flags.csv.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), "..", "..", ".."), mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

release_id <- "faculty_counts_main20_20261022"
release_root <- file.path(DATA_OUTPUT, "land_grants", release_id)
read_release <- function(name) {
  read.csv(file.path(release_root, name), stringsAsFactors = FALSE, check.names = FALSE)
}
obs <- read_release("faculty_count_observations.csv")
panel <- read_release("faculty_count_panel.csv")
coverage <- read_release("coverage_summary.csv")
assignments <- read.csv(file.path(DATA_OUTPUT, "land_grants", "faculty_main_event_study_20",
                                  "college_batch_assignments.csv"), stringsAsFactors = FALSE)
legacy <- read.csv(file.path(DATA_OUTPUT, "land_grants", "faculty_longitudinal",
                             "legacy_opening_coverage.csv"), stringsAsFactors = FALSE)

main_event_ids <- c(7L, 8L, 11L, 14L, 15L, 16L, 17L, 19L, 20L, 23L,
                    25L, 26L, 31L, 32L, 33L, 36L, 39L, 40L, 42L, 43L)
families <- c("commissioner_report", "biennial_survey", "land_grant_statistics", "ace_auc",
              "other_directory", "catalog", "institutional_history", "other", "legacy_roster")

checks <- list()
check <- function(name, ok, detail = "") {
  checks[[length(checks) + 1L]] <<- data.frame(check = name, passed = isTRUE(ok), detail = detail)
}

found <- panel[panel$status != "not_found", ]
expected_n <- sum(sapply(assignments$opening_or_transition_year, function(o) {
  1L + length(seq(ceiling((o + 1) / 5) * 5, 1950, by = 5))
}))

check("scope is the 20 main-event-study colleges",
      setequal(unique(panel$event_id), main_event_ids) && setequal(assignments$event_id, main_event_ids))
check("full benchmark set per college", nrow(panel) == expected_n,
      sprintf("%d rows, expected %d", nrow(panel), expected_n))
check("one row per college x benchmark", !anyDuplicated(panel[, c("event_id", "benchmark_year")]))
check("first benchmark is the opening year",
      all(tapply(panel$benchmark_year, panel$event_id, min) ==
            assignments$opening_or_transition_year[match(sort(unique(panel$event_id)), assignments$event_id)]))
check("status values controlled",
      all(panel$status %in% c("observed", "substitute_within_2y", "not_found")))
check("covered cells have a positive integer count",
      all(!is.na(found$faculty_total) & found$faculty_total > 0 & found$faculty_total == round(found$faculty_total)))
check("not-found cells carry no count", all(is.na(panel$faculty_total[panel$status == "not_found"])))
check("covered cells cite a source", all(nzchar(found$source_url) & nzchar(found$source_family)))
check("substitutes within 2 years",
      all(abs(found$year_offset) <= 2) && all((found$year_offset == 0) == (found$status == "observed")))
check("panel picks trace to observations", all(found$observation_id %in% obs$observation_id))
check("source families controlled", all(obs$source_family %in% families),
      paste(setdiff(unique(obs$source_family), families), collapse = ", "))
check("confidence values controlled", all(obs$confidence %in% c("high", "medium", "low")))
check("staged observations cite a URL",
      all(nzchar(obs$source_url[obs$source_family != "legacy_roster"])))
check("split counts do not exceed the total",
      all(with(obs[!is.na(obs$faculty_total), ],
               (is.na(faculty_collegiate) | faculty_collegiate <= faculty_total) &
                 (is.na(faculty_preparatory) | faculty_preparatory <= faculty_total))))
check("coverage summary matches panel",
      sum(coverage$n_observed + coverage$n_substitute) == nrow(found) &&
        sum(coverage$n_benchmarks) == nrow(panel))

checks <- do.call(rbind, checks)

###############################################################################
# Plausibility flags for review (not failures)
###############################################################################

flags <- list()
for (id in unique(found$event_id)) {
  x <- found[found$event_id == id, ]
  x <- x[order(x$benchmark_year), ]
  if (nrow(x) < 2L) next
  ratio <- x$faculty_total[-1] / x$faculty_total[-nrow(x)]
  hit <- which(ratio > 2.5 | ratio < 0.5)
  if (length(hit)) {
    flags[[length(flags) + 1L]] <- data.frame(
      event_id = id, college = x$college[1], benchmark_year = x$benchmark_year[hit + 1L],
      flag = ifelse(ratio[hit] > 2.5, "jump_over_2.5x", "drop_over_50pct"),
      detail = sprintf("%d (%d, %s) -> %d (%d, %s)",
                       x$faculty_total[hit], x$benchmark_year[hit], x$source_family[hit],
                       x$faculty_total[hit + 1L], x$benchmark_year[hit + 1L], x$source_family[hit + 1L])
    )
  }
}
disagree <- found[!is.na(found$same_year_max) & found$same_year_max > 1.5 * found$same_year_min, ]
if (nrow(disagree)) {
  flags[[length(flags) + 1L]] <- data.frame(
    event_id = disagree$event_id, college = disagree$college, benchmark_year = disagree$benchmark_year,
    flag = "sources_disagree_same_year",
    detail = sprintf("chosen %d; same-year range %d-%d", disagree$faculty_total,
                     disagree$same_year_min, disagree$same_year_max)
  )
}
opening <- found[found$benchmark_year == found$opening_or_transition_year &
                   found$source_family != "legacy_roster", ]
opening$legacy <- legacy$found_roster_count[match(opening$event_id, legacy$event_id)]
off <- opening[!is.na(opening$legacy) &
                 abs(opening$faculty_total - opening$legacy) > 0.5 * pmax(opening$faculty_total, opening$legacy), ]
if (nrow(off)) {
  flags[[length(flags) + 1L]] <- data.frame(
    event_id = off$event_id, college = off$college, benchmark_year = off$benchmark_year,
    flag = "opening_vs_legacy_roster",
    detail = sprintf("panel %d (%s) vs legacy roster %d", off$faculty_total, off$source_family, off$legacy)
  )
}
low <- found[found$confidence == "low", ]
if (nrow(low)) {
  flags[[length(flags) + 1L]] <- data.frame(
    event_id = low$event_id, college = low$college, benchmark_year = low$benchmark_year,
    flag = "low_confidence_pick", detail = substr(low$count_definition, 1, 120)
  )
}
flags <- if (length(flags)) do.call(rbind, flags) else
  data.frame(event_id = integer(), college = character(), benchmark_year = integer(),
             flag = character(), detail = character())
flags <- flags[order(flags$event_id, flags$benchmark_year), ]

write.csv(checks, file.path(release_root, "validation.csv"), row.names = FALSE)
write.csv(flags, file.path(release_root, "review_flags.csv"), row.names = FALSE)

print(checks, row.names = FALSE)
cat(sprintf("\n%d review flags: %s\n", nrow(flags),
            paste(names(table(flags$flag)), table(flags$flag), sep = "=", collapse = ", ")))
if (!all(checks$passed)) stop("Faculty-count validation failed: ",
                              paste(checks$check[!checks$passed], collapse = "; "))
cat("All checks passed.\n")
