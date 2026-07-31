elite_timing_config <- function() {
  mode <- tolower(Sys.getenv("ELITE_TREATMENT_TIMING", unset = "exposure14"))
  allowed <- c("exposure14", "opening")
  if (!mode %in% allowed) {
    stop("ELITE_TREATMENT_TIMING must be one of: ", paste(allowed, collapse = ", "))
  }
  school_age <- as.integer(Sys.getenv("ELITE_SCHOOL_AGE", unset = "14"))
  if (is.na(school_age) || school_age < 0L || school_age > 25L) {
    stop("ELITE_SCHOOL_AGE must be an integer between 0 and 25")
  }
  list(
    mode = mode,
    school_age = school_age,
    label = if (mode == "exposure14") {
      paste0("first fully exposed birth cohort (opening - ", school_age, ")")
    } else {
      "school opening year"
    }
  )
}

elite_event_year <- function(opening_year, config) {
  opening_year <- as.integer(opening_year)
  if (config$mode == "exposure14") opening_year - config$school_age else opening_year
}

elite_results_dir <- function(data_root, analysis_name, config, ...) {
  tag <- trimws(Sys.getenv("ELITE_RESULTS_TAG", unset = ""))
  parts <- c(data_root, "results", "elite_schools", analysis_name)
  if (nzchar(tag)) parts <- c(parts, tag)
  parts <- c(parts, paste0("timing_", config$mode), list(...))
  do.call(file.path, as.list(parts))
}
