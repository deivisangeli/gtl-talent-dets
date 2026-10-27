# Validate completed production artifacts, including every rematched placebo.
suppressPackageStartupMessages(library(data.table))
data.table::setDTthreads(1)
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(script)) normalizePath(file.path(dirname(script), "../.."), winslash = "/") else Sys.getenv("GTL_REPO", getwd())
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "analysis/mandatory_schooling/annual_csdid_helpers.R"))
source(file.path(repo_root, "analysis/mandatory_schooling/annual_synth_matching_helpers.R"))
config <- ms_matching_config()
attributes_only <- identical(config$specifications, "attributes_only")
root <- file.path(TALENT_DETS_DATA_DIR, "results/mandatory_schooling", config$directory)
stored_manifest <- fread(file.path(root, "input_manifest.csv"))
live_signature <- tools::md5sum(stored_manifest$file)
# Historical runs retain their recorded code version. Data must still agree;
# the newly generated specification must also match current runner/helper code.
required <- if (attributes_only) rep(TRUE, nrow(stored_manifest)) else !grepl("[.]R$", stored_manifest$file)
stopifnot(identical(unname(live_signature[required]), stored_manifest$md5[required]))
profiles <- strsplit(Sys.getenv("MS_SYNTH_PROFILES", "pre20,pre40"), ",", fixed = TRUE)[[1]]
outcomes <- strsplit(Sys.getenv("MS_SYNTH_OUTCOMES", paste(names(annual_outcomes), collapse = ",")), ",", fixed = TRUE)[[1]]
checks <- list()
donor_identity <- new.env(parent = emptyenv())
rematches <- new.env(parent = emptyenv())
raw_panel <- if (attributes_only) read_annual_inputs(TALENT_DETS_DATA_DIR)$panel else NULL
path_checks <- new.env(parent = emptyenv())
check_path <- function(path, weights, target_id, Y, years, g) {
  treated <- as.numeric(Y[as.character(target_id),])
  synthetic <- as.numeric(crossprod(weights$weight, Y[as.character(weights$id),,drop=FALSE]))
  gap <- treated-synthetic
  stopifnot(identical(as.integer(path$year),as.integer(years)),
    all(is.finite(as.matrix(path[,.(treated,synthetic,gap,gap_adjusted)]))),
    max(abs(path$treated-treated)) < 1e-10,
    max(abs(path$synthetic-synthetic)) < 1e-10,
    max(abs(path$gap-gap)) < 1e-10,
    max(abs(path$gap_adjusted-(gap-gap[years == g-15L]))) < 1e-10)
}
check_balance <- function(balance, features, weights) {
  x <- features[match(weights$id, id)]
  expected_mean <- vapply(balance$variable, function(v) mean(x[[v]]), numeric(1))
  expected_weighted <- vapply(balance$variable, function(v) sum(x[[v]] * weights$weight), numeric(1))
  stopifnot(isTRUE(all.equal(balance$selected_mean, unname(expected_mean), tolerance=1e-10)),
            isTRUE(all.equal(balance$synthetic, unname(expected_weighted), tolerance=1e-10)))
}
for (pr in profiles) for (oc in outcomes) {
  d <- file.path(root, pr, oc)
  s <- fread(file.path(d, "model_status.csv"))
  p <- fread(file.path(d, "event_paths.csv"))
  v <- fread(file.path(d, "covariate_vintages.csv"))
  f <- fread(file.path(d, "cohort_features.csv"))
  b <- fread(file.path(d, "covariate_balance.csv"))
  pooled <- fread(file.path(d, "pooled_paths.csv"))
  matrices <- new.env(parent=emptyenv())
  stopifnot(!anyDuplicated(s$model_id), !anyDuplicated(p[,.(model_id,year)]),
    nrow(s) == length(config$specifications) * annual_profiles[[pr]]$counties,
    all(v[!is.na(census_year), census_year <= g-15L]),
    all(s[status == "estimated", n_placebos_requested] == 30),
    all(s[status == "estimated", is.finite(pre_rmspe)]),
    !any(grepl("fit_error", s$status)),
    setequal(s[status == "estimated", model_id], unique(p$model_id)),
    all(p[event_time == -15, abs(gap_adjusted) < 1e-10]))
  for (mid in s[status == "estimated", model_id]) {
    meta <- s[model_id == mid]
    z <- readRDS(file.path(d, "events", paste0(mid, ".rds")))
    stopifnot(identical(stored_manifest$md5, unname(z$signature)),
      nrow(z$actual$path) == annual_profiles[[pr]]$pre + 21L,
      nrow(z$placebo) == 30L, !anyDuplicated(z$placebo$placebo_id),
      abs(sum(z$actual$weights$weight)-1) < 1e-8,
      all(z$actual$weights$weight >= -1e-10),
      all(z$actual$weights$id %/% 1000L != meta$state_id))
    cohort <- f[focal_g == meta$g]
    check_balance(z$balance, cohort, z$actual$weights)
    donors <- cohort[id %in% z$actual$weights$id]
    complete <- if (attributes_only) "attributes_complete" else "base_complete"
    stopifnot(nrow(donors) == 30L, all(donors$g > meta$g+34L), all(donors[[complete]]))
    if (attributes_only) {
      stopifnot(identical(z$columns, ms_attribute_features), cohort[id == meta$id, attributes_complete],
        all(is.finite(z$balance$synthetic)))
      candidates <- cohort[donor_eligible & attributes_complete, id]
      gkey <- as.character(meta$g)
      years <- seq.int(meta$g-annual_profiles[[pr]]$pre,meta$g+20L)
      if (!exists(gkey,matrices,inherits=FALSE)) {
        wide <- dcast(raw_panel[id %in% cohort$id & year %in% years],id~year,value.var=oc)
        Y <- as.matrix(wide[,-1L]); rownames(Y) <- as.character(wide$id)
        assign(gkey,Y,matrices)
      }
      Y <- get(gkey,matrices)
      check_path(z$actual$path,z$actual$weights,meta$id,Y,years,meta$g)
      stopifnot(abs(sqrt(mean(z$actual$path[event_time <= -15,gap^2]))-meta$pre_rmspe) < 1e-10,
        isTRUE(all.equal(as.data.frame(p[model_id == mid,names(z$actual$path),with=FALSE]),
          as.data.frame(z$actual$path),tolerance=1e-10,check.attributes=FALSE)),
        setequal(names(z$placebo_records),as.character(z$placebo[status == "estimated",placebo_id])),
        !any(grepl("fit_error",z$placebo$status)))
      expected_match <- ms_match(meta$id, cohort, candidates, ms_attribute_features)
      stopifnot(identical(expected_match$selected$id, z$selected$id),
        isTRUE(all.equal(expected_match$balance$selected_mean, z$balance$selected_mean)))
      key <- paste(pr, meta$id, meta$g)
      if (exists(key, donor_identity, inherits=FALSE)) {
        stopifnot(identical(get(key, donor_identity), z$selected$id))
      } else assign(key, z$selected$id, donor_identity)
    }
    if (meta$specification == "richer") stopifnot(all(donors$rich_complete),
      cohort[id == meta$id, rich_complete], all(ms_rich_features %in% z$columns),
      all(is.finite(z$balance$synthetic)))
    for (pid in names(z$placebo_records)) {
      w <- z$placebo_records[[pid]]$weights
      check_balance(z$placebo_records[[pid]]$balance, cohort, w)
      stopifnot(nrow(w) == 30L, !as.integer(pid) %in% w$id,
        all(w$id %/% 1000L != as.integer(pid) %/% 1000L),
        all(w$id %/% 1000L != meta$state_id), abs(sum(w$weight)-1) < 1e-8,
        all(cohort[id %in% w$id, g] > meta$g+34L))
      stopifnot(all(is.finite(w$weight)), all(w$weight >= -1e-10),
        nrow(z$placebo_records[[pid]]$path) == annual_profiles[[pr]]$pre + 21L,
        z$placebo_records[[pid]]$path[event_time == -15, gap_adjusted] == 0)
      if (attributes_only) {
        key <- paste(pr, meta$g, pid)
        if (!exists(key, rematches, inherits=FALSE)) {
          pm <- ms_match(as.integer(pid), cohort, candidates, ms_attribute_features)
          stopifnot(pm$ok)
          assign(key, pm$selected$id, rematches)
        }
        stopifnot(identical(w$id, get(key, rematches)), all(cohort[id %in% w$id, attributes_complete]),
          identical(z$placebo_records[[pid]]$balance$variable, ms_attribute_features))
        path_key <- paste(pr,oc,meta$g,pid)
        if (!exists(path_key,path_checks,inherits=FALSE)) {
          check_path(z$placebo_records[[pid]]$path,w,as.integer(pid),Y,years,meta$g)
          assign(path_key,TRUE,path_checks)
        }
      }
      if (meta$specification == "richer") stopifnot(all(cohort[id %in% w$id, rich_complete]))
    }
  }
  common <- s[status == "estimated", .(n = uniqueN(specification)), by = id][n == 2L,id]
  for (spec in if (attributes_only) character() else c("original","richer")) {
    expected <- p[specification == spec & target_id %in% common,
      .(expected = mean(gap_adjusted)), by = event_time]
    actual <- pooled[comparison == paste0(spec,"_common")]
    stopifnot(all(actual$counties == length(common)),
      max(abs(actual$gap - expected$expected[match(actual$event_time,expected$event_time)])) < 1e-10)
  }
  if (attributes_only) {
    expected <- p[, .(gap = mean(gap_adjusted)), by=event_time]
    actual <- pooled[comparison == "attributes_only_full"]
    stopifnot(all(actual$counties == sum(s$status == "estimated")),
      max(abs(actual$gap - expected$gap[match(actual$event_time,expected$event_time)])) < 1e-10,
      sum(s$status == "estimated") == if (pr == "pre20") 119L else 52L)
  }
  checks[[paste(pr,oc)]] <- data.table(profile=pr,outcome=oc,
    observed_fits=sum(s$status == "estimated"),
    matching_set=Sys.getenv("MS_SYNTH_MATCHING_SET", "default"),
    comparison_counties=s[specification != "original" & status == "estimated",.N],
    comparison_states=s[specification != "original" & status == "estimated",uniqueN(state_id)],
    requested_local_placebos=sum(s$n_placebos_requested,na.rm=TRUE),status="passed")
  message("Validated ",pr,"/",oc)
}
result <- rbindlist(checks)
fwrite(result,file.path(root,"validation.csv"))
print(result)
