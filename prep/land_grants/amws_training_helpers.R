# Shared definitions for the additive AMWS training-location outcomes.
suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

training_stamp <- "amws_training_50km_20261014"
training_radius_m <- 50000
training_groups <- c("ever_nearby", "never_nearby", "unknown")
training_counts <- c(ever_nearby = "n_amws_ever_trained_50km",
                     never_nearby = "n_amws_never_trained_50km",
                     unknown = "n_amws_training_unknown", total = "n_amws_training_total")
training_rates <- setNames(paste0(sub("^n_", "", training_counts), "_per_1000_births"), names(training_counts))

training_require <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}
training_has <- function(x) !is.na(x) & nzchar(trimws(as.character(x))) &
  !toupper(trimws(as.character(x))) %in% c("NA", "N/A")
training_true <- function(x) !is.na(x) & tolower(trimws(as.character(x))) %in% c("true", "1")
training_geoid <- function(x) {
  y <- suppressWarnings(as.integer(x))
  ifelse(is.na(y), NA_character_, sprintf("%05d", y))
}
training_decade <- function(year, timing) {
  out <- (year %/% 10L) * 10L
  if (timing == "alternative_decade") out <- out + ifelse(year %% 10L >= 7L, 10L, 0L)
  out
}
training_contaminated <- function(text) {
  text[is.na(text)] <- ""
  see <- as.integer(regexpr("\\bsee\\s+prev(?:ious)?(?:\\s+edition)?\\b", text, ignore.case = TRUE, perl = TRUE))
  birth <- as.integer(regexpr("(^|[[:space:],.;])b[[:space:]]+", text, ignore.case = TRUE, perl = TRUE))
  see > 0L & (birth < 0L | birth > see)
}
training_classify <- function(distances, n_awards, complete, conflicts = rep(FALSE, n_awards), radius = training_radius_m) {
  training_require(!any(is.finite(distances) & distances < 0), "Negative training distance")
  if (any(is.finite(distances) & distances <= radius)) return("ever_nearby")
  if (n_awards > 0L && length(complete) == n_awards && all(complete %in% TRUE) &&
      length(conflicts) == n_awards && !any(conflicts %in% TRUE) &&
      length(distances) > 0L && all(is.finite(distances)) && all(distances > radius)) return("never_nearby")
  "unknown"
}
training_rate <- function(n, births) ifelse(is.finite(births) & births > 0, 1000 * n / births, NA_real_)
training_json <- function(value, path) jsonlite::write_json(value, path, pretty = TRUE, auto_unbox = TRUE, na = "null", digits = NA)
training_hash <- function(path) digest::digest(file = path, algo = "sha256")
training_hashes <- function(paths) setNames(lapply(paths, training_hash), paths)
training_verify <- function(hashes) {
  for (p in names(hashes)) training_require(identical(training_hash(p), hashes[[p]]), paste("Protected input changed:", p))
}
training_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
training_root <- function() {
  root <- Sys.getenv("TALENT_DETS_DATA_DIR", unset = Sys.getenv("DET_DIR", unset = ""))
  if (!nzchar(root)) root <- file.path(path.expand("~"), "Globtalent Dropbox", "gtl_talent_dets")
  normalizePath(root, winslash = "/", mustWork = TRUE)
}
training_config <- function(repo, root) {
  output <- file.path(root, "output", "land_grants")
  list(repo = repo, root = root,
       persons = file.path(root, "Data", "processed", "amws", "amws_open_alex.csv"),
       person_manifest = file.path(root, "Data", "processed", "amws", "amws_open_alex_manifest.json"),
       units = file.path(output, "andrews_event_county_units_1850_1920.csv"),
       panels = c(standard_decade = file.path(output, "amws_temporal_support_county_decade_1830_1950.csv"),
                  alternative_decade = file.path(output, "amws_temporal_support_county_decade_1830_1950_alt.csv")),
       shape = file.path(root, "raw", "tigris", "cb_2020_us_county_20m.shp"),
       legacy_early = file.path(root, "output", "amws", "amws_combined_us_geocoded.csv"),
       sample_helper = file.path(repo, "analysis", "land_grants", "temporal_support_1830_1950", "_sample_helpers.R"),
       main_status = file.path(root, "results", "land_grants", "event_studies",
                              "amws_county_pairs_temporal_support_1830_1950_baseline_pop_balanced_event_time_max70", "baseline_pop_model_status.csv"),
       main_audit = file.path(root, "results", "land_grants", "event_studies",
                             "amws_county_pairs_temporal_support_1830_1950_baseline_pop_per_cohort_max_e", "events_by_cohort.csv"),
       output = file.path(output, training_stamp),
       results = file.path(root, "results", "land_grants", "event_studies", training_stamp),
       local = file.path(Sys.getenv("AMWS_CHECKPOINT_ROOT", unset = file.path(path.expand("~"), ".codex", "amws_checkpoints")), training_stamp))
}
training_publish <- function(stage, destination, inputs, metadata) {
  training_verify(inputs)
  training_require(!dir.exists(destination), paste("Refusing to overwrite release:", destination))
  paths <- list.files(stage, full.names = TRUE, recursive = TRUE)
  paths <- paths[!dir.exists(paths)]
  relatives <- substring(paths, nchar(stage) + 2L)
  sums <- setNames(lapply(paths, training_hash), relatives)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_along(paths)) {
    target <- file.path(destination, relatives[i])
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    copied <- FALSE
    for (attempt in 1:5) {
      if (isTRUE(file.copy(paths[i], target, overwrite = FALSE))) { copied <- TRUE; break }
      Sys.sleep(min(attempt, 5))
    }
    training_require(copied && identical(training_hash(target), sums[[i]]), paste("Publication copy failed:", target))
  }
  training_verify(inputs)
  metadata$input_checksums <- inputs
  metadata$output_checksums <- sums
  metadata$published_at_utc <- training_now()
  metadata$copied_checksums_verified <- TRUE
  training_json(metadata, file.path(destination, "completion_manifest.json"))
  invisible(metadata)
}
training_check_release <- function(path) {
  marker <- file.path(path, "completion_manifest.json")
  training_require(file.exists(marker), paste("Missing release completion marker:", path))
  m <- jsonlite::read_json(marker, simplifyVector = FALSE)
  for (p in names(m$output_checksums)) training_require(identical(training_hash(file.path(path, p)), m$output_checksums[[p]]), paste("Published checksum differs:", p))
  training_verify(m$input_checksums)
  m
}

# Renormalization matches the manuscript runner's influence-function convention.
training_renormalize <- function(dynamic, ref = 0L) {
  influence <- dynamic$inf.function$dynamic.inf.func.e
  col <- which(dynamic$egt == ref)
  training_require(length(col) == 1L, "Reference event time absent")
  shifted <- sweep(influence, 1L, influence[, col], "-")
  estimates <- dynamic$att.egt - dynamic$att.egt[col]
  se <- sqrt(colMeans(shifted^2) / nrow(shifted))
  data.table(event_time = dynamic$egt, estimate = estimates, se = se,
             ci_low = estimates - 1.96 * se, ci_high = estimates + 1.96 * se)
}
