# Compare a preserved richer run with attributes-only on identical targets.
# All writes are confined to the new specification's result directory.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
setDTthreads(1)
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(script)) normalizePath(file.path(dirname(script), "../.."), winslash="/") else Sys.getenv("GTL_REPO", getwd())
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "analysis/mandatory_schooling/annual_csdid_helpers.R"))
source(file.path(repo_root, "analysis/mandatory_schooling/annual_synth_matching_helpers.R"))
parent <- file.path(TALENT_DETS_DATA_DIR, "results/mandatory_schooling")
old_root <- file.path(parent, "synth_matching")
new_root <- file.path(parent, "synth_matching_attributes_only")
old_manifest <- fread(file.path(old_root, "input_manifest.csv"))
new_manifest <- fread(file.path(new_root, "input_manifest.csv"))
data_manifest <- merge(old_manifest[!grepl("[.]R$", file)], new_manifest[!grepl("[.]R$", file)], by="file")
stopifnot(nrow(data_manifest) == sum(!grepl("[.]R$", old_manifest$file)),
  all(data_manifest$md5.x == data_manifest$md5.y),
  identical(unname(tools::md5sum(data_manifest$file)), data_manifest$md5.x),
  identical(unname(tools::md5sum(new_manifest$file)), new_manifest$md5))
# Preserve the historical code hashes rather than relabeling the old run.
fwrite(old_manifest, file.path(new_root, "baseline_input_manifest.csv"))
comparison_source <- file.path(repo_root, "analysis/mandatory_schooling/compare_annual_synth_attributes.R")
fwrite(data.table(file=comparison_source, md5=unname(tools::md5sum(comparison_source))),
       file.path(new_root, "comparison_source_manifest.csv"))
input <- read_annual_inputs(TALENT_DETS_DATA_DIR)
profiles <- strsplit(Sys.getenv("MS_SYNTH_PROFILES", "pre20,pre40"), ",", fixed=TRUE)[[1]]
outcomes <- strsplit(Sys.getenv("MS_SYNTH_OUTCOMES", paste(names(annual_outcomes),collapse=",")), ",", fixed=TRUE)[[1]]
summaries <- list(); paired_summaries <- list(); checks <- list()

diagnose <- function(record, features, target_id, Y, clean_index, common_ids, specification) {
  columns <- ms_attribute_features
  target <- features[id == target_id]
  pool <- features[donor_eligible & attributes_complete & state_id != target$state_id]
  # Both selections are measured on this exact same donor pool and scale.
  X <- as.matrix(pool[, ..columns]); sigma <- apply(X, 2, sd)
  use <- is.finite(sigma) & sigma > 1e-12
  selected <- features[match(record$selected$id, id)]
  tx <- as.numeric(target[, ..columns])
  sx <- as.matrix(selected[, ..columns])
  z <- sweep(sweep(sx[, use, drop=FALSE], 2, tx[use], "-"), 2, sigma[use], "/")
  wx <- features[match(record$actual$weights$id, id), ..columns]
  weighted <- colSums(as.matrix(wx) * record$actual$weights$weight)
  donor_y <- Y[as.character(record$selected$id), clean_index, drop=FALSE]
  pool_y <- Y[as.character(pool$id), clean_index, drop=FALSE]
  treated_y <- as.numeric(Y[as.character(target_id), clean_index])
  positive <- treated_y > 0
  allzero <- colSums(donor_y > 0) == 0L
  path <- record$actual$path[event_time <= -15]
  data.table(id=target_id, specification=specification, common=target_id %in% common_ids,
    state_id=target$state_id, g=target$g, nonzero_pre=any(positive),
    pre_rmspe=record$actual$summary$pre_rmspe,
    treated_pre_mean=mean(path$treated), synthetic_pre_mean=mean(path$synthetic),
    attribute_distance=mean(sqrt(rowMeans(z^2))),
    selected_attribute_gap=sqrt(mean(((tx[use]-colMeans(sx)[use])/sigma[use])^2)),
    weighted_attribute_gap=sqrt(mean(((tx[use]-weighted[use])/sigma[use])^2)),
    active_donors=sum(rowSums(donor_y > 0) > 0),
    weighted_donors=sum(record$actual$weights$weight > 1e-8),
    effective_donors=1/sum(record$actual$weights$weight^2),
    zero_donor_years=sum(donor_y == 0), donor_years=length(donor_y),
    positive_treated_years=sum(positive), unsupported_positive_years=sum(positive & allzero),
    pool_unsupported_positive_years=sum(positive & colSums(pool_y > 0) == 0),
    n_placebos_requested=nrow(record$placebo), n_placebos_estimated=sum(record$placebo$status == "estimated"),
    n_placebos_finite_stat=sum(is.finite(record$placebo$standardized_effect)))
}

for (pr in profiles) for (oc in outcomes) {
  old <- file.path(old_root, pr, oc); new <- file.path(new_root, pr, oc)
  s0 <- fread(file.path(old, "model_status.csv"))[specification == "richer"]
  s1 <- fread(file.path(new, "model_status.csv"))
  common_ids <- intersect(s0[status == "estimated", id], s1[status == "estimated", id])
  stopifnot(length(common_ids) == if (pr == "pre20") 113L else 52L,
    all(s0[status == "estimated", id] %in% common_ids))
  f0 <- fread(file.path(old, "cohort_features.csv"))
  f1 <- fread(file.path(new, "cohort_features.csv"))
  p0 <- fread(file.path(old, "event_paths.csv"))[specification == "richer" & target_id %in% common_ids]
  p1 <- fread(file.path(new, "event_paths.csv"))
  pools <- rbindlist(list(copy(p0)[,comparison := "richer_common"],
    copy(p1[target_id %in% common_ids])[,comparison := "attributes_only_common"],
    copy(p1)[,comparison := "attributes_only_full"]))
  pooled <- pools[, .(gap=mean(gap_adjusted), raw_gap=mean(gap), treated=mean(treated), synthetic=mean(synthetic),
    counties=uniqueN(target_id), states=uniqueN(state_id)), by=.(comparison,event_time)]
  pooled[, inference := "Descriptive county-weighted mean; local county placebos are not state-randomization p-values"]
  stopifnot(all(pooled[event_time == -15, gap] == 0),
    all(pooled[comparison %in% c("richer_common","attributes_only_common"), counties] == length(common_ids)))
  setorder(pooled, comparison, event_time)
  fwrite(pooled, file.path(new, "pooled_richer_comparison.csv"))
  w0 <- fread(file.path(old, "donor_weights.csv"))[specification == "richer" & target_id %in% common_ids]
  w1 <- fread(file.path(new, "donor_weights.csv"))
  count_box <- paste0("Common counties T/C: ", length(common_ids), "/", uniqueN(w0$id), " R; ",
    uniqueN(w1[target_id %in% common_ids,id]), " A\nCommon states T/C: ", uniqueN(s1[id %in% common_ids,state_id]),
    "/", uniqueN(w0$id %/% 1000L), " R; ", uniqueN(w1[target_id %in% common_ids,id] %/% 1000L),
    " A\nFull A T/C: ", uniqueN(p1$target_id), "/", uniqueN(w1$id), " counties; ",
    uniqueN(s1[status == "estimated", state_id]), "/", uniqueN(w1$id %/% 1000L), " states",
    "\nEvents: ", length(common_ids), " common; ", uniqueN(p1$target_id), " A full")
  plot <- ggplot(pooled, aes(event_time,gap,colour=comparison)) +
    geom_hline(yintercept=0,colour="grey70") + geom_vline(xintercept=c(-15,0),linetype="dashed",colour="grey60") +
    geom_line(linewidth=.65) + annotate("label",x=Inf,y=Inf,hjust=1.02,vjust=1.05,label=count_box,size=2.5) +
    scale_y_continuous(expand=expansion(mult=c(.05,.28))) +
    scale_colour_manual(values=c(richer_common="#1B4965",attributes_only_common="#C45A32",attributes_only_full="#4C956C"),
      breaks=c("richer_common","attributes_only_common","attributes_only_full"),
      labels=c("Richer: common","Attributes: common","Attributes: full")) +
    labs(title=paste("Mandatory schooling:",pr), subtitle="Descriptive pooled gaps; reference = -15",
      x="Birth year relative to law",y=annual_outcomes[[oc]],colour=NULL,
      caption="R = richer, A = attributes only. County placebo ranks are diagnostic; law adoption is at state level.") +
    theme_minimal(base_size=10) + theme(legend.position="bottom")
  ggsave(file.path(new,"pooled_richer_comparison.pdf"),plot,width=9,height=5.5)

  diagnostics <- list(); overlap <- list()
  for (gg in sort(unique(s1[status == "estimated",g]))) {
    features <- f1[focal_g == gg]
    baseline_features <- f0[focal_g == gg]
    stopifnot(setequal(features[donor_eligible & attributes_complete,id],
      baseline_features[donor_eligible & rich_complete,id]))
    columns <- ms_attribute_features
    stopifnot(isTRUE(all.equal(features[order(id),..columns],baseline_features[order(id),..columns],check.attributes=FALSE)))
    years <- seq.int(gg-annual_profiles[[pr]]$pre,gg+20L)
    wide <- dcast(input$panel[id %in% features$id & year %in% years],id~year,value.var=oc)
    Y <- as.matrix(wide[,-1L]); rownames(Y) <- as.character(wide$id)
    clean <- which(years <= gg-15L)
    for (target_id in s1[status == "estimated" & g == gg,id]) {
      m1 <- s1[id == target_id,model_id]
      a <- readRDS(file.path(new,"events",paste0(m1,".rds")))
      stopifnot(identical(unname(a$signature),new_manifest$md5))
      diagnostics[[paste(target_id,"a")]] <- diagnose(a,features,target_id,Y,clean,common_ids,"attributes_only")
      if (target_id %in% common_ids) {
        m0 <- s0[id == target_id,model_id]
        b <- readRDS(file.path(old,"events",paste0(m0,".rds")))
        stopifnot(identical(unname(b$signature),old_manifest$md5))
        diagnostics[[paste(target_id,"r")]] <- diagnose(b,features,target_id,Y,clean,common_ids,"richer")
        overlap[[as.character(target_id)]] <- data.table(id=target_id,donors_shared=length(intersect(a$selected$id,b$selected$id)),
          donor_overlap=length(intersect(a$selected$id,b$selected$id))/30)
      }
    }
  }
  event <- rbindlist(diagnostics)
  fwrite(event,file.path(new,"comparison_event_diagnostics.csv"))
  paired <- merge(event[common & specification == "richer"],event[common & specification == "attributes_only"],
    by=c("id","state_id","g","nonzero_pre","common"),suffixes=c("_richer","_attributes"))
  paired <- merge(paired,rbindlist(overlap),by="id")
  paired[, rmspe_change := pre_rmspe_attributes-pre_rmspe_richer]
  paired[, fit_change := fifelse(rmspe_change < -1e-10,"improved",fifelse(rmspe_change > 1e-10,"worsened","unchanged"))]
  fwrite(paired,file.path(new,"comparison_paired_counties.csv"))
  recovered <- merge(s1[status == "estimated" & !id %in% common_ids,.(id,GEOID,state_abbr,g)],
    s0[,.(id,previous_status=status)],by="id",all.x=TRUE)
  fwrite(recovered,file.path(new,"newly_eligible_counties.csv"))
  for (group in c("all","nonzero_pre")) {
    e <- if (group == "all") event else event[nonzero_pre == TRUE]
    grouped <- rbindlist(list(copy(e[common == TRUE])[,sample := "common"],
      copy(e[specification == "attributes_only"])[,sample := "full"]))
    sm <- grouped[,.(counties=.N,states=uniqueN(state_id),mean_pre_rmspe=mean(pre_rmspe),median_pre_rmspe=median(pre_rmspe),
      mean_treated_pre=mean(treated_pre_mean),mean_synthetic_pre=mean(synthetic_pre_mean),
      mean_attribute_distance=mean(attribute_distance),mean_selected_attribute_gap=mean(selected_attribute_gap),
      mean_weighted_attribute_gap=mean(weighted_attribute_gap),median_active_donors=median(active_donors),
      mean_effective_donors=mean(effective_donors),zero_donor_year_share=sum(zero_donor_years)/sum(donor_years),
      positive_treated_years=sum(positive_treated_years),
      unsupported_positive_year_share=if (sum(positive_treated_years)>0) sum(unsupported_positive_years)/sum(positive_treated_years) else NA_real_,
      pool_unsupported_positive_year_share=if (sum(positive_treated_years)>0) sum(pool_unsupported_positive_years)/sum(positive_treated_years) else NA_real_),
      by=.(specification,sample)]
    sm[,`:=`(profile=pr,outcome=oc,subgroup=group)]
    summaries[[paste(pr,oc,group)]] <- sm
    pp <- if (group == "all") paired else paired[nonzero_pre == TRUE]
    ps <- pp[,.(counties=.N,improved=sum(fit_change == "improved"),worsened=sum(fit_change == "worsened"),
      unchanged=sum(fit_change == "unchanged"),mean_rmspe_change=mean(rmspe_change),
      median_rmspe_change=median(rmspe_change),mean_donor_overlap=mean(donor_overlap))]
    ps[,`:=`(profile=pr,outcome=oc,subgroup=group)]
    paired_summaries[[paste(pr,oc,group)]] <- ps
  }
  checks[[paste(pr,oc)]] <- data.table(profile=pr,outcome=oc,common_counties=length(common_ids),
    full_counties=uniqueN(p1$target_id),recovered_counties=nrow(recovered),status="passed")
  message("Compared ",pr,"/",oc)
}
fwrite(rbindlist(summaries),file.path(new_root,"comparison_summary.csv"))
fwrite(rbindlist(paired_summaries),file.path(new_root,"comparison_paired_summary.csv"))
fwrite(rbindlist(checks),file.path(new_root,"comparison_validation.csv"))
writeLines(c("Richer artifacts are read-only and retain their historical code hashes; underlying data hashes match the new run.",
  "Attribute distances: mean individual-donor RMS standardized distance across seven features, with eligible-donor SDs shared across specifications.",
  "Selected/weighted attribute gaps: RMS across seven standardized aggregate attribute imbalances, before/after SDID weights.",
  "Active donor: at least one positive clean-pre-period outcome. Zero donor-year shares count individual donor-year cells.",
  "Unsupported positive year: treated outcome is positive and every selected donor is zero; pool variant uses every eligible out-of-state donor.",
  "Common comparisons pair identical treated counties; full attributes-only includes newly eligible counties. Nonzero-pre is a diagnostic subgroup, not fit trimming.",
  "AMWS pre-outcomes remain in SDID unit/time weighting and regularization; only donor selection changes.",
  "Raw pre-RMSPE is unshifted. Pooled curves normalize gaps to -15 and are descriptive; local placebo ranks are not state-randomization p-values."),
  file.path(new_root,"comparison_notes.txt"))
print(rbindlist(checks))
report_table <- dcast(rbindlist(summaries)[sample == "common" & subgroup == "all"],
  profile + outcome ~ specification, value.var="mean_pre_rmspe")
report_rows <- vapply(seq_len(nrow(report_table)),function(i) {
  x <- report_table[i]
  sprintf("| %s | %s | %.6f | %.6f |",x$profile,x$outcome,x$richer,x$attributes_only)
},character(1))
writeLines(c("# Attributes-only donor selection: comparison with richer matching", "",
  "Only selection of the 30 donors changes. Pre-period AMWS remains in synthetic-DiD weighting and regularization.", "",
  "The attributes-only sample contains 119 counties in 10 states for pre20 and 52 counties in six states for pre40, for each of four outcomes. The six additional pre20 counties previously failed a constant AMWS donor-feature check.", "",
  "## Mean raw pre-period RMSPE on identical treated counties", "",
  "The paired samples contain 113 pre20 counties and 52 pre40 counties. Lower RMSPE indicates a closer raw outcome fit.", "",
  "| Window | Outcome | Richer | Attributes only |", "|---|---|---:|---:|", report_rows, "",
  "For AMWS per 1,000 births, seven-attribute distances improve modestly but mean pre-period RMSPE worsens in both windows. This alternative does not resolve the sparse-outcome support problem. See comparison_paired_summary.csv for improved/worsened counts and the nonzero-pre subgroup.", "",
  "## Files", "",
  "Each profile/outcome directory contains event_raw_fits.pdf (one page per county), event and placebo RDS records, donor weights, covariate balance, local placebo diagnostics, and pooled_richer_comparison.pdf. The latter separates richer-common, attributes-only-common, and attributes-only-full samples.", "",
  "comparison_summary.csv and comparison_event_diagnostics.csv record donor activity, zero donor-year cells, support for positive treated years, and attribute balance on shared eligible-pool scales. comparison_notes.txt defines every diagnostic.", "",
  "Local placebo ranks are diagnostics, not state-randomization p-values. Pooled curves are descriptive. No pre-fit trimming is applied.", "",
  "The preserved richer artifacts retain their historical code hashes; baseline_input_manifest.csv records that version. Underlying data hashes match the new run."),
  file.path(new_root,"comparison_report.md"))
