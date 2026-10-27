# Full eligibility accounting and cross-outcome donor-selection invariance.
suppressPackageStartupMessages(library(data.table));setDTthreads(1)
arg<-commandArgs(FALSE);script<-sub("^--file=","",arg[grepl("^--file=",arg)])
repo_root<-if(length(script)) normalizePath(file.path(dirname(script),"../.."),winslash="/") else Sys.getenv("GTL_REPO",getwd())
source(file.path(repo_root,"paths.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/annual_csdid_helpers.R"))
root<-file.path(TALENT_DETS_DATA_DIR,"results/mandatory_schooling/synth_state_placebos")
input<-read_annual_inputs(TALENT_DETS_DATA_DIR)
checks<-list()
for(pr in c("pre20","pre40")) {
  d<-file.path(root,pr);manifest<-fread(file.path(d,"preparation_manifest.csv"))
  actual<-unname(tools::md5sum(manifest$file))
  preparer<-grepl("/prepare_compulsory_schooling_state_placebos[.]R$",manifest$file)
  actual[preparer]<-unname(tools::md5sum(file.path(d,"preparation_source.R")))
  stopifnot(identical(actual,manifest$md5))
  observed<-fread(file.path(d,"observed_input_manifest.csv"))
  stopifnot(identical(unname(tools::md5sum(observed$file)),observed$md5))
  assignments<-fread(file.path(d,"assignments.csv"))
  f<-fread(file.path(TALENT_DETS_DATA_DIR,"results/mandatory_schooling/synth_matching_attributes_only",pr,"amws_per_1000_births/cohort_features.csv"))
  eligibility<-list()
  for(gg in sort(unique(assignments$g))) {
    years<-seq.int(gg-annual_profiles[[pr]]$pre,gg+20L)
    z<-input$panel[year %in% years,.(observed_years=.N,valid_years=sum(valid_outcomes),
      actual_law=g[1],state_id=state_id[1],state_abbr=state_abbr[1]),by=id]
    z[,`:=`(focal_g=gg,full_window=observed_years==length(years) & valid_years==length(years),
      law_eligible=actual_law>gg+34L,attributes_complete=FALSE,rich_complete=FALSE,missing_covariates="")]
    z[f[focal_g==gg],on="id",`:=`(attributes_complete=i.attributes_complete,rich_complete=i.rich_complete,missing_covariates=i.missing_covariates)]
    z[,eligibility_status:=fcase(!law_eligible,"actual_treatment_too_early",!full_window,"incomplete_valid_outcome_window",
      !attributes_complete,"missing_county_attributes",default="eligible_before_matching_support")]
    eligibility[[as.character(gg)]]<-z
  }
  fwrite(rbindlist(eligibility),file.path(d,"county_eligibility.csv"))
  contexts<-fread(file.path(d,"matching_contexts.csv"))[specification=="attributes_only"]
  fingerprint<-list()
  for(i in seq_len(nrow(contexts))) {
    meta<-contexts[i];z<-readRDS(file.path(d,"matching",paste0(meta$context_id,".rds")))
    stopifnot(identical(z$signature,c(setNames(manifest$md5,manifest$file),setNames(observed$md5,observed$file))))
    fingerprint[[meta$context_id]]<-data.table(g=meta$g,excluded_states=meta$excluded_states,outcome=meta$outcome,
      counties=nrow(z$status),donor_fingerprint=digest::digest(lapply(z$matches,function(m) m$selected$id),algo="xxhash64"))
  }
  hashes<-rbindlist(fingerprint)
  same<-hashes[,.(outcomes=uniqueN(outcome),selections=uniqueN(donor_fingerprint),sample_sizes=uniqueN(counties)),by=.(g,excluded_states)]
  stopifnot(all(same$outcomes==4L),all(same$selections==1L),all(same$sample_sizes==1L))
  fwrite(hashes,file.path(d,"attributes_donor_fingerprints.csv"))
  checks[[pr]]<-data.table(profile=pr,joint_assignments=uniqueN(assignments$draw_id),
    distinct_state_sets=uniqueN(assignments$excluded_states),cross_outcome_contexts=nrow(same),status="passed")
  message("Audited eligibility and cross-outcome donor identity: ",pr)
}
fwrite(rbindlist(checks),file.path(root,"design_validation.csv"));print(rbindlist(checks))
