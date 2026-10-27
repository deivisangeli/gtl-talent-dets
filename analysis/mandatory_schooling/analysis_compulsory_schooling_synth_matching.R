# County-specific synthetic fits with original and richer donor matching.
# Local county placebos are diagnostics, not state-randomization inference.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(script)) normalizePath(file.path(dirname(script), "../.."), winslash = "/") else Sys.getenv("GTL_REPO", getwd())
source(file.path(repo_root, "paths.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
source(file.path(repo_root, "analysis/mandatory_schooling/annual_csdid_helpers.R"))
source(file.path(repo_root, "analysis/mandatory_schooling/annual_synth_matching_helpers.R"))
data.table::setDTthreads(1)
prep_root <- file.path(TALENT_DETS_DATA_DIR, "output/mandatory_schooling/matching")
config <- ms_matching_config()
out <- file.path(TALENT_DETS_DATA_DIR, "results/mandatory_schooling", config$directory)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
input <- read_annual_inputs(TALENT_DETS_DATA_DIR)
covariates <- fread(file.path(prep_root, "county_matching_covariates.csv"), colClasses = c(GEOID = "character"))
manifest <- fread(file.path(prep_root, "variable_manifest.csv"))
area_path <- file.path(TALENT_DETS_DATA_DIR, "raw/tigris/cb_2020_us_county_20m.shp")
# The 2025 gazetteer replaces Connecticut counties with planning regions.
# The existing 2020 geometry preserves the annual panel's county identifiers.
area <- as.data.table(sf::st_drop_geometry(sf::st_read(area_path, quiet = TRUE)))[,
  .(GEOID = as.character(GEOID), land_km2 = ALAND / 1e6)]
stopifnot(!anyDuplicated(area$GEOID))
profiles <- strsplit(Sys.getenv("MS_SYNTH_PROFILES", "pre20,pre40"), ",", fixed = TRUE)[[1]]
outcomes <- strsplit(Sys.getenv("MS_SYNTH_OUTCOMES", paste(names(annual_outcomes), collapse = ",")), ",", fixed = TRUE)[[1]]
stopifnot(all(profiles %in% names(annual_profiles)), all(outcomes %in% names(annual_outcomes)))
matching_only <- tolower(Sys.getenv("MS_MATCHING_ONLY", "false")) == "true"
n_placebos <- as.integer(Sys.getenv("MS_SYNTH_PLACEBOS", "30"))
stopifnot(n_placebos >= 0, n_placebos <= 30)
if (n_placebos != 30L && !matching_only) out <- file.path(out, paste0("pilot_placebos", n_placebos))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
signature_paths <- c(input$paths, sub("[.]shp$", ".dbf", area_path), file.path(prep_root, c("county_matching_covariates.csv", "variable_manifest.csv")),
  file.path(repo_root, "analysis/mandatory_schooling", c("annual_synth_matching_helpers.R", "analysis_compulsory_schooling_synth_matching.R")))
signature <- tools::md5sum(signature_paths)
fwrite(data.table(file = names(signature), md5 = unname(signature)), file.path(out, "input_manifest.csv"))

for (profile in profiles) {
  pre <- annual_profiles[[profile]]$pre
  sample <- build_annual_sample(input$panel, annual_profiles[[profile]])
  for (outcome in outcomes) {
    destination <- file.path(out, profile, outcome)
    dir.create(destination, recursive = TRUE, showWarnings = FALSE)
    cache <- file.path(destination, "events")
    dir.create(cache, showWarnings = FALSE)
    all_audit <- list(); all_status <- list(); all_paths <- list(); all_balance <- list()
    all_placebos <- list(); all_weights <- list(); all_vintages <- list(); features_audit <- list()
    for (gg in sort(unique(sample$targets$g))) {
      cohort <- ms_cohort_features(input$panel, sample$targets, covariates, manifest, area, gg, pre, outcome)
      features <- cohort$features
      vintages <- copy(cohort$vintages); vintages[, g := gg]
      all_vintages[[as.character(gg)]] <- vintages
      fa <- copy(features); fa[, focal_g := gg]; features_audit[[as.character(gg)]] <- fa
      for (specification in config$specifications) {
        placebo_cache <- new.env(parent = emptyenv())
        complete <- switch(specification, original = "base_complete", richer = "rich_complete", attributes_only = "attributes_complete")
        columns <- if (specification == "attributes_only") ms_attribute_features else
          c(ms_base_features, if (specification == "richer") ms_rich_features)
        donor_ids <- features[donor_eligible & get(complete), id]
        audit <- copy(features[, .(id, GEOID, state_id, state_abbr, is_target, donor_eligible,
          complete = get(complete), missing_covariates)])
        audit[, `:=`(g = gg, specification = specification,
          n_eligible_donors = length(donor_ids),
          n_eligible_donor_states = uniqueN(features[id %in% donor_ids, state_id]))]
        all_audit[[paste(gg, specification)]] <- audit
        for (target_id in features[is_target == TRUE, id]) {
          model_id <- paste(profile, outcome, specification, target_id, gg, sep = "_")
          target <- features[id == target_id]
          matched <- ms_match(target_id, features, donor_ids, columns)
          status <- data.table(model_id = model_id, id = target_id, GEOID = target$GEOID,
            state_id = target$state_id, state_abbr = target$state_abbr, g = gg,
            profile = profile, outcome = outcome, specification = specification,
            status = matched$status, n_eligible_donors = length(donor_ids),
            n_eligible_donor_states = uniqueN(features[id %in% donor_ids, state_id]))
          if (!matched$ok || matching_only) {
            all_status[[model_id]] <- status
            next
          }
          record_path <- file.path(cache, paste0(model_id, ".rds"))
          record <- if (file.exists(record_path)) readRDS(record_path) else NULL
          if (is.null(record) || !identical(record$signature, signature) || record$n_placebos != n_placebos) {
            actual <- tryCatch(ms_fit(cohort, target_id, matched$selected, outcome), error = function(e) e)
            if (inherits(actual, "error")) {
              status[, status := paste0("fit_error:", conditionMessage(actual))]
              all_status[[model_id]] <- status
              next
            }
            balance <- ms_weighted_balance(matched$balance, features, actual$weights)
            # Original matching still reports balance on the added variables
            # when observed, without using them to select donors.
            if (specification == "original") {
              extra <- rbindlist(lapply(ms_rich_features, function(v) {
                dv <- features[id %in% donor_ids, get(v)]
                sv <- features[match(matched$selected$id, id), get(v)]
                wv <- features[match(actual$weights$id, id), get(v)]
                tv <- target[[v]]; sdv <- sd(dv, na.rm = TRUE)
                data.table(variable = v, treated = tv, eligible_mean = mean(dv, na.rm = TRUE),
                  eligible_sd = sdv, selected_mean = mean(sv), synthetic = sum(wv * actual$weights$weight),
                  eligible_min = if (any(is.finite(dv))) min(dv, na.rm = TRUE) else NA_real_,
                  eligible_max = if (any(is.finite(dv))) max(dv, na.rm = TRUE) else NA_real_,
                  constant = is.finite(sdv) && sdv <= 1e-12,
                  selected_std_gap = if (is.finite(sdv) && sdv > 1e-12) (tv - mean(sv)) / sdv else NA_real_,
                  synthetic_std_gap = if (is.finite(sdv) && sdv > 1e-12) (tv - sum(wv * actual$weights$weight)) / sdv else NA_real_)
              }), fill = TRUE)
              extra[, outside_range := treated < eligible_min | treated > eligible_max]
              balance <- rbindlist(list(balance, extra), fill = TRUE)
            }
            placebo <- list(); placebo_records <- list()
            pseudo_ids <- head(matched$selected$id, n_placebos)
            for (pid in pseudo_ids) {
              cache_key <- as.character(pid)
              if (exists(cache_key, envir = placebo_cache, inherits = FALSE)) {
                cached <- get(cache_key, envir = placebo_cache)
                placebo[[cache_key]] <- cached$summary
                if (!is.null(cached$record)) placebo_records[[cache_key]] <- cached$record
                next
              }
              # Entire pseudo-treated state is excluded by ms_match, just as
              # for actual events. Original treated states are never donors.
              pm <- ms_match(pid, features, donor_ids, columns)
              ps <- data.table(placebo_id = pid, status = pm$status)
              if (pm$ok) {
                pf <- tryCatch(ms_fit(cohort, pid, pm$selected, outcome), error = function(e) e)
                if (!inherits(pf, "error")) {
                  ps <- cbind(ps, pf$summary); ps[, status := "estimated"]
                  placebo_records[[as.character(pid)]] <- list(path = pf$path, weights = pf$weights,
                    balance = ms_weighted_balance(pm$balance, features, pf$weights))
                } else ps[, status := paste0("fit_error:", conditionMessage(pf))]
              }
              placebo[[as.character(pid)]] <- ps
              assign(cache_key, list(summary = ps, record = placebo_records[[cache_key]]), envir = placebo_cache)
            }
            record <- list(signature = signature, n_placebos = n_placebos, actual = actual,
              balance = balance, placebo = rbindlist(placebo, fill = TRUE), placebo_records = placebo_records,
              selected = matched$selected, columns = columns, vintages = cohort$vintages,
              inference = "County placebo diagnostics only; state-level assignment is not randomized here.")
            saveRDS(record, record_path)
          }
          summary <- copy(record$actual$summary)
          pv <- record$placebo
          valid <- if (nrow(pv) && "standardized_effect" %in% names(pv)) pv[status == "estimated" & is.finite(standardized_effect)] else data.table()
          summary[, `:=`(n_placebos_requested = n_placebos, n_placebos_valid = nrow(valid),
            local_placebo_tail_rank = if (nrow(valid) && is.finite(standardized_effect))
              (1 + sum(abs(valid$standardized_effect) >= abs(standardized_effect))) / (1 + nrow(valid)) else NA_real_)]
          status[, status := "estimated"]
          all_status[[model_id]] <- cbind(status, summary)
          for (object in c("path", "weights")) {
            z <- copy(record$actual[[object]])
            z[, `:=`(model_id = model_id, target_id = target_id, g = gg, state_id = target$state_id,
              specification = specification)]
            if (object == "path") all_paths[[model_id]] <- z else all_weights[[model_id]] <- z
          }
          balance <- copy(record$balance); balance[, model_id := model_id]; all_balance[[model_id]] <- balance
          pv <- copy(pv)
          if (nrow(pv)) { pv[, model_id := model_id]; all_placebos[[model_id]] <- pv }
        }
      }
      message(profile, "/", outcome, "/g=", gg, ": completed matching", if (!matching_only) " and fits/placebos")
    }
    status <- rbindlist(all_status, fill = TRUE)
    audit <- rbindlist(all_audit, fill = TRUE)
    fwrite(status, file.path(destination, if (matching_only) "matching_status.csv" else "model_status.csv"))
    fwrite(audit, file.path(destination, "eligibility_audit.csv"))
    fwrite(rbindlist(features_audit), file.path(destination, "cohort_features.csv"))
    fwrite(rbindlist(all_vintages), file.path(destination, "covariate_vintages.csv"))
    counts <- status[, .(target_counties = .N, target_states = uniqueN(state_id),
      matched_counties = sum(status %in% c("matched", "estimated")),
      matched_states = uniqueN(state_id[status %in% c("matched", "estimated")])), by = specification]
    fwrite(counts, file.path(destination, "sample_summary.csv")); print(counts)
    if (matching_only || !length(all_paths)) next
    paths <- rbindlist(all_paths); balance <- rbindlist(all_balance, fill = TRUE)
    fwrite(paths, file.path(destination, "event_paths.csv"))
    fwrite(rbindlist(all_weights), file.path(destination, "donor_weights.csv"))
    fwrite(balance, file.path(destination, "covariate_balance.csv"))
    fwrite(rbindlist(all_placebos, fill = TRUE), file.path(destination, "placebo_diagnostics.csv"))
    common <- status[status == "estimated", .(specs = uniqueN(specification)), by = id][specs == 2L, id]
    pool_input <- if (identical(config$specifications, "attributes_only"))
      copy(paths)[, comparison := "attributes_only_full"] else rbindlist(list(
      copy(paths[specification == "original"])[, comparison := "original_full"],
      copy(paths[specification == "original" & target_id %in% common])[, comparison := "original_common"],
      copy(paths[specification == "richer" & target_id %in% common])[, comparison := "richer_common"]))
    pooled <- pool_input[, .(gap = mean(gap_adjusted), raw_gap = mean(gap),
      treated = mean(treated), synthetic = mean(synthetic),
      counties = uniqueN(target_id), states = uniqueN(state_id)), by = .(comparison, event_time)]
    pooled[, inference := "Descriptive county-weighted mean; no independent-county standard errors or pooled p-values"]
    fwrite(pooled, file.path(destination, "pooled_paths.csv"))
    utils::write.table(data.frame(note = c(
      "Law adoption is assigned at state level. Local placebo ranks are diagnostic, not randomization p-values.",
      "Pooled curves are descriptive. A joint state-level placebo design is required for pooled inference.",
      "No pre-fit trimming is applied. Zero-pre-RMSPE events have undefined standardized placebo statistics.",
      if (identical(config$specifications, "attributes_only"))
        "Attributes-only selects donors using seven county attributes. Pre-period AMWS still determines SDID weights. Run compare_annual_synth_attributes.R for the paired richer comparison." else
        "Original-common and richer-common use identical treated counties; original-full retains the original matched sample.")),
      file.path(destination, "inference_notes.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)
    grDevices::pdf(file.path(destination, "event_raw_fits.pdf"), width = 9, height = 5.5, onefile = TRUE)
    for (mid in unique(paths$model_id)) {
      z <- paths[model_id == mid]; meta <- status[model_id == mid]
      long <- melt(z, id.vars = "event_time", measure.vars = c("treated", "synthetic"), variable.name = "series")
      plot <- ggplot(long, aes(event_time, value, colour = series)) +
        annotate("rect", xmin = -14, xmax = 0, ymin = -Inf, ymax = Inf, fill = "grey70", alpha = .2) +
        geom_line(linewidth = .55) + geom_vline(xintercept = c(-15, 0), linetype = "dashed", colour = "grey50") +
        scale_colour_manual(values = c(treated = "#1B4965", synthetic = "#C45A32")) +
        scale_y_continuous(expand = expansion(mult = c(.05, .18))) +
        annotate("label", x = Inf, y = Inf, hjust = 1.02, vjust = 1.05, size = 2.8,
          label = paste0("Counties T/C: 1/30 | States T/C: 1/",
            uniqueN(input$panel[id %in% all_weights[[mid]]$id, state_id]), "\nEvents: 1")) +
        labs(title = paste(meta$GEOID, meta$state_abbr, "law", meta$g, "|", meta$specification),
          subtitle = paste(profile, "| grey band: potentially exposed birth cohorts before law"),
          x = "Birth year relative to law", y = annual_outcomes[[outcome]], colour = NULL,
          caption = "Local county placebos are diagnostics; treatment is assigned at state level.") + theme_minimal(base_size = 10)
      print(plot)
    }
    grDevices::dev.off()
    plot <- ggplot(pooled, aes(event_time, gap, colour = comparison)) + geom_hline(yintercept = 0, colour = "grey70") +
      geom_vline(xintercept = c(-15, 0), linetype = "dashed", colour = "grey60") + geom_line(linewidth = .7) +
      labs(title = paste("Mandatory schooling:", profile), subtitle = "Descriptive pooled gaps; reference = -15",
        x = "Birth year relative to law", y = annual_outcomes[[outcome]], colour = NULL) + theme_minimal(base_size = 10) +
      theme(legend.position = "bottom")
    ggsave(file.path(destination, "pooled_comparison.pdf"), plot, width = 9, height = 5.5)
  }
}
