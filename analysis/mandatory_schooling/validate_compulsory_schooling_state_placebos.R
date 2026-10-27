suppressPackageStartupMessages(library(data.table));setDTthreads(1)
arg<-commandArgs(FALSE);script<-sub("^--file=","",arg[grepl("^--file=",arg)])
repo_root<-if(length(script)) normalizePath(file.path(dirname(script),"../.."),winslash="/") else Sys.getenv("GTL_REPO",getwd())
source(file.path(repo_root,"paths.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/annual_csdid_helpers.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/annual_synth_matching_helpers.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/state_placebo_helpers.R"))
parent<-file.path(TALENT_DETS_DATA_DIR,"results/mandatory_schooling");root<-file.path(parent,"synth_state_placebos")
profiles<-strsplit(Sys.getenv("MS_STATE_PROFILES","pre20,pre40"),",",fixed=TRUE)[[1]]
outcomes<-strsplit(Sys.getenv("MS_STATE_OUTCOMES",paste(names(annual_outcomes),collapse=",")),",",fixed=TRUE)[[1]]
input<-read_annual_inputs(TALENT_DETS_DATA_DIR);results<-list()
for(pr in profiles) {
  assignments<-fread(file.path(root,pr,"assignments.csv"));slots<-fread(file.path(root,pr,"state_slots.csv"))
  coverage<-fread(file.path(root,pr,"candidate_state_coverage.csv"))
  design<-msp_design(slots,coverage);ds<-fread(file.path(root,pr,"design_summary.csv"))
  stopifnot(design$total==ds$structural_assignments,uniqueN(assignments$draw_id)==ds$accepted,
    !anyDuplicated(assignments[,.(draw_id,state_id)]),all(assignments[,.(n=uniqueN(placebo_state)),by=draw_id]$n==nrow(slots)))
  for(draw in unique(assignments$draw_id)) {
    a<-assignments[draw_id==draw];expected<-design$unrank(a$assignment_rank[1])
    stopifnot(uniqueN(a$assignment_rank)==1L,identical(a$placebo_state,expected$placebo_state),
      all(a$g==slots$g[match(a$state_id,slots$state_id)]))
  }
  stopifnot(uniqueN(assignments$assignment_rank)==ds$accepted)
  if(pr=="pre40") stopifnot(ds$checked==54L,ds$exhaustive)
  for(oc in outcomes) {
    dest<-file.path(root,pr,oc);fm<-fread(file.path(dest,"fit_manifest.csv"))
    live<-unname(tools::md5sum(fm$file))
    engine<-grepl("/estimate_compulsory_schooling_state_placebos[.]R$",fm$file)
    snapshot<-file.path(dest,"estimator_source.R")
    if(any(engine) && file.exists(snapshot)) live[engine]<-unname(tools::md5sum(snapshot))
    stopifnot(identical(live,fm$md5))
    f<-fread(file.path(parent,"synth_matching_attributes_only",pr,oc,"cohort_features.csv"))
    contexts<-fread(file.path(root,pr,"matching_contexts.csv"))[outcome==oc]
    state<-fread(file.path(dest,"placebo_state_paths.csv"))
    membership<-fread(file.path(dest,"state_county_membership.csv"))
    stopifnot(!anyDuplicated(state[,.(state_key,event_time)]),!anyDuplicated(membership))
    matrices<-new.env(parent=emptyenv());nfits<-0L
    for(ci in contexts$context_id) {
      meta<-contexts[context_id==ci];gg<-meta$g;cohort<-f[focal_g==gg]
      mpath<-file.path(root,pr,"matching",paste0(ci,".rds"));m<-readRDS(mpath)
      record<-readRDS(file.path(dest,"fits",paste0(ci,".rds")))
      stopifnot(identical(unname(record$signature),fm$md5),identical(record$matching_md5,unname(tools::md5sum(mpath))))
      eligible<-cohort[donor_eligible & ms_complete(cohort,m$columns) & !state_id %in% m$excluded,id]
      stopifnot(setequal(eligible,m$donor_ids),length(eligible)>=30L)
      # Compare the optimized shared-pool routine to the original matcher.
      for(tid in unique(c(head(m$status$id,2L),tail(m$status$id,2L)))) {
        direct<-ms_match(tid,cohort,eligible,m$columns)
        stopifnot(isTRUE(all.equal(direct,m$matches[[as.character(tid)]],check.attributes=FALSE)))
      }
      if(!exists(as.character(gg),matrices,inherits=FALSE)) {
        years<-seq.int(gg-annual_profiles[[pr]]$pre,gg+20L)
        wide<-dcast(input$panel[id %in% cohort$id & year %in% years],id~year,value.var=oc)
        Y<-as.matrix(wide[,-1L]);rownames(Y)<-as.character(wide$id)
        assign(as.character(gg),list(Y=Y,years=years),matrices)
      }
      yy<-get(as.character(gg),matrices)
      for(id in names(record$records)) {
        fit<-record$records[[id]];tid<-as.integer(id)
        stopifnot(identical(fit$weights$id,m$matches[[id]]$selected$id),
          all(fit$weights$id %in% eligible),!any(fit$weights$id %/% 1000L %in% m$excluded),
          cohort[id==tid,g]>gg+34L)
        msp_check_path(fit,yy$Y,tid,yy$years,gg);nfits<-nfits+1L
      }
      for(sk in unique(state[context_id==ci,state_key])) {
        ids<-membership[state_key==sk,id]
        paths<-rbindlist(lapply(ids,function(id) record$records[[as.character(id)]]$path))
        expected<-paths[,.(treated=mean(treated),synthetic=mean(synthetic),gap_adjusted=mean(gap_adjusted)),by=event_time]
        actual<-state[state_key==sk][order(event_time)];setorder(expected,event_time)
        stopifnot(nrow(actual)==annual_profiles[[pr]]$pre+21L,all(actual$counties==length(ids)),
          max(abs(actual$treated-expected$treated))<1e-10,max(abs(actual$synthetic-expected$synthetic))<1e-10,
          max(abs(actual$gap_adjusted-expected$gap_adjusted))<1e-10)
      }
    }
    # Independent reconstruction from the draw-to-state links and fixed weights.
    links<-fread(file.path(dest,"draw_state_links.csv"))
    stopifnot(!anyDuplicated(links[,.(draw_id,comparison,state_id)]))
    state_meta<-state[event_time==-15,.(state_key,meta_g=g,meta_ex=excluded_states,meta_ps=placebo_state,meta_comp=comparison)]
    binding<-merge(links,assignments[,.(draw_id,state_id,expected_g=g,expected_ps=placebo_state,expected_ex=excluded_states)],by=c("draw_id","state_id"))
    binding<-merge(binding,state_meta,by="state_key")
    stopifnot(nrow(binding)==nrow(links),all(binding$placebo_state==binding$expected_ps),
      all(binding$placebo_state==binding$meta_ps),all(binding$expected_g==binding$meta_g),
      all(binding$expected_ex==binding$meta_ex),all(binding$comparison==binding$meta_comp))
    members<-merge(membership,state_meta,by="state_key")
    paired<-dcast(members[meta_comp %in% c("richer_common","attributes_only_common")],
      meta_g+meta_ex+meta_ps+id~meta_comp,value.var="state_key",fun.aggregate=length)
    stopifnot(all(paired$richer_common==1L),all(paired$attributes_only_common==1L))
    common_ids<-members[meta_comp=="attributes_only_common",.(meta_g,meta_ex,meta_ps,id)]
    full_ids<-members[meta_comp=="attributes_only_full",.(meta_g,meta_ex,meta_ps,id)]
    stopifnot(nrow(merge(common_ids,full_ids,by=c("meta_g","meta_ex","meta_ps","id")))==nrow(common_ids))
    expected_weights<-ifelse(links$comparison=="attributes_only_full",slots$weight_full[match(links$state_id,slots$state_id)],slots$weight_common[match(links$state_id,slots$state_id)])
    stopifnot(max(abs(links$weight-expected_weights))<1e-10,
      all(abs(links[,.(total=sum(weight)),by=.(draw_id,comparison)]$total-1)<1e-10))
    linked<-merge(links,state[,.(state_key,event_time,gap_adjusted)],by="state_key",allow.cartesian=TRUE)
    expected<-linked[,.(expected=sum(weight*gap_adjusted)),by=.(draw_id,comparison,event_time)]
    pooled<-fread(file.path(dest,"pooled_placebo_paths.csv"))
    joined<-merge(expected,pooled,by=c("draw_id","comparison","event_time"))
    stopifnot(nrow(joined)==nrow(expected),nrow(joined)==nrow(pooled),max(abs(joined$gap_adjusted-joined$expected))<1e-10,
      all(pooled[event_time==-15,gap_adjusted]==0))
    stats<-fread(file.path(dest,"placebo_statistics.csv"));inf<-fread(file.path(dest,"inference_summary.csv"))
    expected_stats<-pooled[event_time>=0 & event_time<=20,.(expected=mean(gap_adjusted)),by=.(draw_id,comparison)]
    js<-merge(stats,expected_stats,by=c("draw_id","comparison"))
    stopifnot(nrow(js)==3L*ds$accepted,max(abs(js$statistic-js$expected))<1e-10,all(is.finite(js$statistic)))
    for(comp in inf$comparison) {
      row<-inf[comparison==comp];v<-stats[comparison==comp,statistic]
      stopifnot(row$assignments==ds$accepted,abs(row$state_placebo_tail_rank-(1+sum(abs(v)>=abs(row$observed_statistic)))/(length(v)+1))<1e-10)
    }
    results[[paste(pr,oc)]]<-data.table(profile=pr,outcome=oc,assignments=ds$accepted,contexts=nrow(contexts),county_context_fits=nfits,status="passed")
    fwrite(results[[paste(pr,oc)]],file.path(dest,"validation.csv"))
    message("Validated ",pr,"/",oc,": ",nfits," county/context fits")
    rm(matrices,linked,record);gc(FALSE)
  }
}
validation_files<-unlist(lapply(names(annual_profiles),function(pr) file.path(root,pr,names(annual_outcomes),"validation.csv")))
fwrite(rbindlist(lapply(validation_files[file.exists(validation_files)],fread)),file.path(root,"validation.csv"));print(rbindlist(results))
