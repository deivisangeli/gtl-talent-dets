###############################################################################
# Copy the mandatory-schooling Callaway-Sant'Anna figures from the Dropbox
# results tree into project/figures, under the naming grammar the figure volume
# project/mandatory_schooling.tex expects.
#
# This exists because the volume's figures were originally hand-copied, which
# flattened the results directory structure into filenames by a rule that lived
# nowhere in code. Re-running the analysis then left the volume silently stale:
# its figures were still the pre-Lingwall vintage long after the Lingwall (2014)
# law dates became canonical. The Lingwall guard below is the specific check
# that would have caught it.
#
# The figures are copied, not re-plotted: unlike the world-fairs pipeline these
# carry a summary header box that the analysis itself renders
# (analysis/mandatory_schooling/csdid_figure_helpers.R).
#
# Run from anywhere:
#   Rscript analysis/mandatory_schooling/export_mandatory_schooling_project_artifacts.R
###############################################################################

suppressPackageStartupMessages(library(data.table))

export_repo_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    candidate <- normalizePath(sub("^--file=", "", file_arg[1L]), winslash = "/")
    if (basename(candidate) == "export_mandatory_schooling_project_artifacts.R") {
      return(normalizePath(file.path(dirname(candidate), "../.."), winslash = "/"))
    }
  }
  candidate <- Sys.getenv("GTL_REPO", unset = getwd())
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (parent == candidate) stop("Cannot resolve repository root; set GTL_REPO.")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
}

repo_root <- export_repo_root()

export_data_root <- function() {
  config <- new.env()
  sys.source(file.path(repo_root, "paths.R"), config)
  root <- config$TALENT_DETS_DATA_DIR
  if (!dir.exists(root) && !nzchar(Sys.getenv("TALENT_DETS_DATA_DIR"))) {
    root <- file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets")
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

results_root <- file.path(export_data_root(), "results", "mandatory_schooling")
project_figure_dir <- file.path(repo_root, "project", "figures")
tex_path <- file.path(repo_root, "project", "mandatory_schooling.tex")
stopifnot(dir.exists(results_root), file.exists(tex_path))
dir.create(project_figure_dir, recursive = TRUE, showWarnings = FALSE)

ANNUAL_DIR <- "compulsory_schooling_amws_annual_csdid"
PERIOD5_DIR <- "compulsory_schooling_amws_period5_csdid"
PERIOD5_HORIZON <- "post30_ref15"
# Region x birth-year versions of both designs (CLS South / non-South), from
# analysis_compulsory_schooling_amws_region_csdid.R. Every run directory there,
# uncontrolled ones included, carries its own validation_summary.csv.
REGION_DIR <- "compulsory_schooling_amws_region_csdid"

# "" is the uncontrolled run, which lives in the profile root rather than in a
# control subdirectory.
VARIANT_SUBDIR <- c(unadjusted = "",
                    baseline_log_population = "baseline_log_population",
                    preperiod_population_cagr = "preperiod_population_cagr")
PROFILES <- c("pre20", "pre40")
CONVENTIONS <- c("standard", "alt")
ANNUAL_OUTCOMES <- c("amws_per_1000_births", "amws_per_1000_pop", "n_amws",
                     "log1p_n_amws", "population", "county_births_estimate_year")
PERIOD5_OUTCOMES <- "amws_per_1000_births"

# Figures the volume references that this script does NOT manage. The synthetic
# DiD figures were hand-copied from the synth-matching and state-placebo trees,
# whose law-date vintage is unresolved: those trees carry no Lingwall
# attribution, and the committed PDFs no longer match the obvious current
# source. Recorded here so the gap is visible in code, not merely known.
UNMANAGED_FIGURES <- c(
  outer(c("amws_per_1000_births", "amws_per_1000_pop", "n_amws", "log1p_n_amws"),
        c("comparison", "state_placebos"),
        function(o, k) sprintf("mandatory_schooling_synthdid_pre40_%s_%s.pdf", o, k)))

join_path <- function(...) {
  parts <- c(...)
  paste(parts[nzchar(parts)], collapse = "/")
}

annual_spec <- CJ(profile = PROFILES, outcome = ANNUAL_OUTCOMES,
                  variant = names(VARIANT_SUBDIR), sorted = FALSE)
annual_spec[, run_dir := file.path(ANNUAL_DIR, paste0(profile, "_post20_a14"))]
annual_spec[, source := mapply(join_path, run_dir, VARIANT_SUBDIR[variant],
                               paste0("pooled_", outcome, ".pdf"))]
annual_spec[, validation := fifelse(
  variant == "unadjusted",
  file.path(ANNUAL_DIR, "validation_summary.csv"),
  mapply(join_path, run_dir, VARIANT_SUBDIR[variant], "validation_summary.csv"))]
annual_spec[, destination := sprintf("mandatory_schooling_csdid_%s_%s_%s.pdf",
                                     profile, outcome, variant)]
annual_spec[, kind := "annual"]

period5_spec <- CJ(convention = CONVENTIONS, profile = PROFILES,
                   outcome = PERIOD5_OUTCOMES, variant = names(VARIANT_SUBDIR),
                   sorted = FALSE)
period5_spec[, run_dir := file.path(PERIOD5_DIR, convention,
                                    paste0(profile, "_", PERIOD5_HORIZON))]
period5_spec[, source := mapply(join_path, run_dir, VARIANT_SUBDIR[variant],
                                paste0("pooled_", outcome, ".pdf"))]
period5_spec[, validation := fifelse(
  variant == "unadjusted",
  file.path(PERIOD5_DIR, convention, paste0("validation_summary_", PERIOD5_HORIZON, ".csv")),
  mapply(join_path, run_dir, VARIANT_SUBDIR[variant], "validation_summary.csv"))]
period5_spec[, destination := sprintf(
  "mandatory_schooling_csdid_5yr_%s_%s_post30_%s_%s.pdf",
  convention, profile, outcome, variant)]
period5_spec[, kind := "period5"]

region_annual_spec <- CJ(profile = PROFILES, outcome = PERIOD5_OUTCOMES,
                         variant = names(VARIANT_SUBDIR), sorted = FALSE)
region_annual_spec[, run_dir := file.path(REGION_DIR, "annual", paste0(profile, "_post20_a14"))]
region_period5_spec <- CJ(convention = CONVENTIONS, profile = PROFILES,
                          outcome = PERIOD5_OUTCOMES, variant = names(VARIANT_SUBDIR),
                          sorted = FALSE)
region_period5_spec[, run_dir := file.path(REGION_DIR, "period5", convention,
                                           paste0(profile, "_", PERIOD5_HORIZON))]
region_spec <- rbindlist(list(region_annual_spec, region_period5_spec), fill = TRUE)
region_spec[, source := mapply(join_path, run_dir, VARIANT_SUBDIR[variant],
                               paste0("pooled_", outcome, ".pdf"))]
region_spec[, validation := mapply(join_path, run_dir, VARIANT_SUBDIR[variant],
                                   "validation_summary.csv")]
region_spec[, destination := fifelse(
  is.na(convention),
  sprintf("mandatory_schooling_csdid_region_%s_%s_%s.pdf", profile, outcome, variant),
  sprintf("mandatory_schooling_csdid_region_5yr_%s_%s_post30_%s_%s.pdf",
          convention, profile, outcome, variant))]
region_spec[, kind := fifelse(is.na(convention), "region_annual", "region_period5")]

spec <- rbindlist(list(annual_spec, period5_spec, region_spec), fill = TRUE)
setcolorder(spec, c("kind", "destination", "source", "validation"))
stopifnot(!anyDuplicated(spec$destination), !anyDuplicated(spec$source))

###############################################################################
# Guards. Nothing is copied until all of these pass.
###############################################################################

missing_sources <- spec[!file.exists(file.path(results_root, source)), source]
if (length(missing_sources)) {
  stop("Missing source figures:\n  ", paste(missing_sources, collapse = "\n  "))
}

# The Lingwall guard. Both the uncontrolled and the controlled runs record the
# treatment-date source in notes.txt, in slightly different words, so match the
# citation itself rather than either sentence.
notes_dirs <- unique(dirname(file.path(results_root, spec$source)))
bad_notes <- character()
for (d in notes_dirs) {
  notes_file <- file.path(d, "notes.txt")
  if (!file.exists(notes_file)) {
    bad_notes <- c(bad_notes, paste0(d, " (no notes.txt)"))
    next
  }
  if (!any(grepl("Lingwall \\(2014\\)", readLines(notes_file, warn = FALSE)))) {
    bad_notes <- c(bad_notes, paste0(d, " (no Lingwall attribution)"))
  }
}
if (length(bad_notes)) {
  stop("Refusing to export: these runs do not record Lingwall (2014) law dates.\n",
       "This is the check that catches exporting a superseded vintage.\n  ",
       paste(bad_notes, collapse = "\n  "))
}

validation_files <- unique(file.path(results_root, spec$validation))
unvalidated <- character()
for (v in validation_files) {
  if (!file.exists(v)) {
    unvalidated <- c(unvalidated, paste0(v, " (missing)"))
    next
  }
  report <- fread(v)
  if (!"validation_passed" %in% names(report) || !all(report$validation_passed %in% TRUE)) {
    unvalidated <- c(unvalidated, paste0(v, " (validation_passed not all TRUE)"))
  }
}
if (length(unvalidated)) {
  stop("Refusing to export unvalidated runs:\n  ", paste(unvalidated, collapse = "\n  "))
}

# Cross-check against the volume: every figure it references must either be
# exported here or be declared unmanaged, and everything exported must be used.
tex <- readLines(tex_path, warn = FALSE)
referenced <- regmatches(tex, regexpr("(?<=\\\\figurepage\\{figures/)[^}]+", tex, perl = TRUE))
referenced <- referenced[nzchar(referenced)]
stopifnot(length(referenced) > 0L, !anyDuplicated(referenced))
managed_refs <- setdiff(referenced, UNMANAGED_FIGURES)
if (!setequal(managed_refs, spec$destination)) {
  stop("project/mandatory_schooling.tex and this script disagree.\n",
       "Referenced but not exported:\n  ",
       paste(setdiff(managed_refs, spec$destination), collapse = "\n  "), "\n",
       "Exported but not referenced:\n  ",
       paste(setdiff(spec$destination, managed_refs), collapse = "\n  "))
}

###############################################################################
# Copy, then record provenance.
###############################################################################

copied <- vapply(seq_len(nrow(spec)), function(i) {
  file.copy(file.path(results_root, spec$source[i]),
            file.path(project_figure_dir, spec$destination[i]), overwrite = TRUE)
}, logical(1L))
if (!all(copied)) {
  stop("Failed to copy:\n  ", paste(spec$destination[!copied], collapse = "\n  "))
}

info <- file.info(file.path(results_root, spec$source))
manifest <- data.table(
  destination = spec$destination,
  kind = spec$kind,
  source = spec$source,
  source_md5 = unname(tools::md5sum(file.path(results_root, spec$source))),
  source_modified = format(info$mtime, "%Y-%m-%d %H:%M:%S"),
  exported = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
setorder(manifest, kind, destination)
fwrite(manifest, file.path(project_figure_dir, "mandatory_schooling_figure_manifest.csv"))

# Remove superseded managed figures: anything matching the managed prefix that
# this script no longer emits and that the volume does not reference.
existing <- list.files(project_figure_dir, pattern = "^mandatory_schooling_csdid.*[.]pdf$")
obsolete <- setdiff(existing, c(spec$destination, UNMANAGED_FIGURES))
if (length(obsolete)) {
  unlink(file.path(project_figure_dir, obsolete))
  message("Removed ", length(obsolete), " superseded figure(s): ",
          paste(obsolete, collapse = ", "))
}

message("Exported ", nrow(spec), " figures to ", project_figure_dir)
message("  annual  : ", spec[kind == "annual", .N], " (",
        length(ANNUAL_OUTCOMES), " outcomes x ", length(PROFILES), " profiles x ",
        length(VARIANT_SUBDIR), " variants)")
message("  5-year  : ", spec[kind == "period5", .N], " (", PERIOD5_HORIZON, ", ",
        length(CONVENTIONS), " conventions x ", length(PROFILES), " profiles x ",
        length(VARIANT_SUBDIR), " variants)")
message("  region  : ", spec[kind %like% "^region", .N], " (CLS South / non-South; annual ",
        spec[kind == "region_annual", .N], ", 5-year ", spec[kind == "region_period5", .N], ")")
message("  unmanaged in the volume: ", length(UNMANAGED_FIGURES), " synthetic-DiD figures")
message("Manifest: ", file.path(project_figure_dir, "mandatory_schooling_figure_manifest.csv"))
