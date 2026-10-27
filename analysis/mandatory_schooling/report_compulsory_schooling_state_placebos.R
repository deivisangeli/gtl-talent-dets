suppressPackageStartupMessages({library(data.table);library(ggplot2)});setDTthreads(1)
arg <- commandArgs(FALSE);script <- sub("^--file=","",arg[grepl("^--file=",arg)])
repo_root <- if(length(script)) normalizePath(file.path(dirname(script),"../.."),winslash="/") else Sys.getenv("GTL_REPO",getwd())
source(file.path(repo_root,"paths.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/annual_csdid_helpers.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/annual_synth_matching_helpers.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/state_placebo_helpers.R"))
parent <- file.path(TALENT_DETS_DATA_DIR,"results/mandatory_schooling");root <- file.path(parent,"synth_state_placebos")
profiles <- strsplit(Sys.getenv("MS_STATE_PROFILES","pre20,pre40"),",",fixed=TRUE)[[1]]
outcomes <- strsplit(Sys.getenv("MS_STATE_OUTCOMES",paste(names(annual_outcomes),collapse=",")),",",fixed=TRUE)[[1]]
comparisons <- c("richer_common","attributes_only_common","attributes_only_full")
all_summary <- list()
for(pr in profiles) for(oc in outcomes) {
  dest <- file.path(root,pr,oc)
  audit_parts <- list.files(dest,pattern="^county_fit_audit_g.*[.]csv$",full.names=TRUE)
  summary_parts <- list.files(dest,pattern="^estimation_summary_g.*[.]csv$",full.names=TRUE)
  if(length(audit_parts)) {
    audit <- rbindlist(lapply(audit_parts,fread),fill=TRUE)
    stopifnot(!anyDuplicated(audit[,.(context_id,id)]))
    fwrite(audit,file.path(dest,"county_fit_audit.csv"))
    sums <- rbindlist(lapply(summary_parts,fread))
    fwrite(sums[,.(profile=pr,outcome=oc,contexts=sum(contexts),new_unique_fits=sum(new_unique_fits),
      reused_in_memory=sum(reused_in_memory),county_context_fits=sum(county_context_fits),status="completed")],
      file.path(dest,"estimation_summary.csv"))
  }
  fm <- fread(file.path(dest,"fit_manifest.csv"))
  live <- unname(tools::md5sum(fm$file))
  engine <- grepl("/estimate_compulsory_schooling_state_placebos[.]R$",fm$file)
  snapshot <- file.path(dest,"estimator_source.R")
  if(any(engine) && file.exists(snapshot)) live[engine] <- unname(tools::md5sum(snapshot))
  stopifnot(identical(live,fm$md5))
  assignments <- fread(file.path(root,pr,"assignments.csv"))
  slots <- fread(file.path(root,pr,"state_slots.csv"))
  samples <- readRDS(file.path(root,pr,"observed_samples.rds"))[[oc]]
  contexts <- fread(file.path(root,pr,"matching_contexts.csv"))[outcome==oc]
  state_paths <- list(); county_audit <- list()
  # Build each distinct excluded-set/date/state fit once; draws link to it.
  for(ci in contexts[specification=="attributes_only",context_id]) {
    meta <- contexts[context_id==ci];gg<-meta$g;ex<-meta$excluded_states
    needed <- unique(assignments[g==gg & excluded_states==ex,placebo_state])
    context_ids <- c(richer=sub("_attributes_only$","_richer",ci),attributes_only=ci)
    fits <- lapply(context_ids,function(id) readRDS(file.path(dest,"fits",paste0(id,".rds"))))
    matches <- lapply(context_ids,function(id) readRDS(file.path(root,pr,"matching",paste0(id,".rds"))))
    for(j in seq_along(fits)) stopifnot(identical(unname(fits[[j]]$signature),fm$md5))
    common <- intersect(as.integer(names(fits$richer$records)),as.integer(names(fits$attributes_only$records)))
    for(ps in needed) for(comparison in comparisons) {
      spec <- if(comparison=="richer_common") "richer" else "attributes_only"
      ids <- as.integer(names(fits[[spec]]$records))
      ids <- ids[ids %/% 1000L==ps & (comparison=="attributes_only_full" | ids %in% common)]
      stopifnot(length(ids)>0)
      state_key <- paste(context_ids[[spec]],ps,comparison,sep="_")
      paths <- rbindlist(lapply(ids,function(tid) copy(fits[[spec]]$records[[as.character(tid)]]$path)[,id:=tid]))
      donor_ids <- unique(unlist(lapply(ids,function(tid) fits[[spec]]$records[[as.character(tid)]]$weights$id)))
      z <- paths[,.(treated=mean(treated),synthetic=mean(synthetic),gap=mean(gap),gap_adjusted=mean(gap_adjusted)),by=event_time]
      z[,`:=`(state_key=state_key,context_id=context_ids[[spec]],g=gg,excluded_states=ex,placebo_state=ps,
        comparison=comparison,counties=length(ids),control_counties=length(donor_ids),control_states=uniqueN(donor_ids %/% 1000L))]
      state_paths[[state_key]] <- z
      county_audit[[state_key]] <- data.table(state_key=state_key,id=ids)
    }
  }
  state <- rbindlist(state_paths);fwrite(state,file.path(dest,"placebo_state_paths.csv"))
  fwrite(rbindlist(county_audit),file.path(dest,"state_county_membership.csv"))
  lookup <- state[event_time==-15,.(g,excluded_states,placebo_state,comparison,state_key)]
  setkey(lookup,g,excluded_states,placebo_state,comparison)
  links <- list();pooled <- list()
  for(draw in unique(assignments$draw_id)) {
    allocation <- assignments[draw_id==draw]
    for(comparison in comparisons) {
      cmp <- comparison
      state_input <- list()
      for(si in seq_len(nrow(allocation))) {
        a <- allocation[si]
        sk <- lookup[.(a$g,a$excluded_states,a$placebo_state,cmp),state_key]
        stopifnot(length(sk)==1L)
        z <- copy(state_paths[[sk]]);z[,state_id:=a$state_id]
        state_input[[as.character(a$state_id)]] <- z
        links[[paste(draw,comparison,a$state_id)]] <- data.table(draw_id=draw,comparison=comparison,
          state_id=a$state_id,placebo_state=a$placebo_state,state_key=sk,
          weight=if(comparison=="attributes_only_full") a$weight_full else a$weight_common)
      }
      pp <- msp_pool(rbindlist(state_input),slots,if(comparison=="attributes_only_full") "weight_full" else "weight_common")
      pp[,`:=`(draw_id=draw,comparison=comparison)];pooled[[paste(draw,comparison)]]<-pp
    }
  }
  fwrite(rbindlist(links),file.path(dest,"draw_state_links.csv"))
  placebo <- rbindlist(pooled);fwrite(placebo,file.path(dest,"pooled_placebo_paths.csv"))
  observed <- list();count_boxes <- list()
  for(comparison in comparisons) {
    spec <- if(comparison=="richer_common") "richer" else "attributes_only"
    old <- file.path(parent,if(spec=="richer") "synth_matching" else "synth_matching_attributes_only",pr,oc)
    ids <- if(comparison=="attributes_only_full") samples$full else samples$common
    p <- fread(file.path(old,"event_paths.csv"))[specification==spec & target_id %in% ids]
    w <- fread(file.path(old,"donor_weights.csv"))[specification==spec & target_id %in% ids]
    sp <- p[,.(treated=mean(treated),synthetic=mean(synthetic),gap=mean(gap),gap_adjusted=mean(gap_adjusted)),by=.(state_id,event_time)]
    pp <- msp_pool(sp,slots,if(comparison=="attributes_only_full") "weight_full" else "weight_common")
    check <- p[,.(expected=mean(gap_adjusted)),by=event_time]
    stopifnot(max(abs(pp$gap_adjusted-check$expected[match(pp$event_time,check$event_time)]))<1e-10)
    pp[,comparison:=comparison];observed[[comparison]]<-pp
    count_boxes[[comparison]] <- data.table(comparison=comparison,label=paste0("Counties T/C: ",length(ids),"/",uniqueN(w$id),
      " | States T/C: ",nrow(slots),"/",uniqueN(w$id %/% 1000L),"\nState events: ",nrow(slots)," | Placebo draws: ",uniqueN(assignments$draw_id)))
  }
  observed <- rbindlist(observed);fwrite(observed,file.path(dest,"observed_pooled_paths.csv"))
  statistics <- placebo[event_time>=0 & event_time<=20,.(statistic=mean(gap_adjusted)),by=.(draw_id,comparison)]
  fwrite(statistics,file.path(dest,"placebo_statistics.csv"))
  obs <- observed[event_time>=0 & event_time<=20,.(observed_statistic=mean(gap_adjusted)),by=comparison]
  inference <- rbindlist(lapply(comparisons,function(comp) {
    values <- statistics[comparison==comp,statistic];value <- obs[comparison==comp,observed_statistic]
    data.table(profile=pr,outcome=oc,comparison=comp,observed_statistic=value,assignments=length(values),
      state_placebo_tail_rank=(1+sum(abs(values)>=abs(value)))/(length(values)+1),
      placebo_mean=mean(values),placebo_q025=as.numeric(quantile(values,.025)),placebo_q975=as.numeric(quantile(values,.975)))
  }))
  fwrite(inference,file.path(dest,"inference_summary.csv"));all_summary[[paste(pr,oc)]]<-inference
  envelope <- placebo[,.(lower=as.numeric(quantile(gap_adjusted,.025)),median=median(gap_adjusted),
    upper=as.numeric(quantile(gap_adjusted,.975))),by=.(comparison,event_time)]
  fwrite(envelope,file.path(dest,"pointwise_placebo_envelope.csv"))
  plot <- ggplot(envelope,aes(event_time)) + geom_ribbon(aes(ymin=lower,ymax=upper),fill="grey75",alpha=.55) +
    geom_line(aes(y=median),colour="grey45",linewidth=.35) +
    geom_line(data=observed,aes(y=gap_adjusted),colour="#1B4965",linewidth=.65) +
    geom_vline(xintercept=c(-15,0),linetype="dashed",colour="grey50") +
    geom_label(data=rbindlist(count_boxes),aes(x=Inf,y=Inf,label=label),inherit.aes=FALSE,hjust=1.02,vjust=1.05,size=2.6) +
    facet_wrap(~comparison,ncol=1,scales="free_y") + scale_y_continuous(expand=expansion(mult=c(.05,.3))) +
    labs(title=paste("Mandatory schooling: joint state placebos |",pr),x="Birth year relative to law",y=annual_outcomes[[oc]],
      caption="Blue: observed. Grey: median and pointwise 95% placebo envelope; not a confidence interval. Reference = -15.") + theme_minimal(base_size=10)
  ggsave(file.path(dest,"pooled_state_placebos.pdf"),plot,width=10,height=10)
  plot <- ggplot(statistics,aes(statistic)) + geom_histogram(bins=30,fill="grey70",colour="white") +
    geom_vline(data=obs,aes(xintercept=observed_statistic),colour="#1B4965",linewidth=.7) +
    facet_wrap(~comparison,ncol=1,scales="free") + theme_minimal(base_size=10) +
    labs(title=paste("State-placebo distribution |",pr),x=paste("Mean gap, event years 0-20 |",annual_outcomes[[oc]]),y="Assignments",
      caption="Blue: observed statistic. Inference is conditional on the specified state-assignment benchmark.")
  ggsave(file.path(dest,"state_placebo_distribution.pdf"),plot,width=10,height=9)
  # Representative state means cover every focal date and comparison. All
  # county paths are retained in fit records and all state means in the CSV.
  pages <- state[event_time==-15,.SD[1L],by=.(g,comparison)]
  grDevices::pdf(file.path(dest,"representative_placebo_state_fits.pdf"),width=9,height=5.5)
  for(sk in pages$state_key) {
    z <- state[state_key==sk];meta<-z[1]
    long <- melt(z,id.vars="event_time",measure.vars=c("treated","synthetic"),variable.name="series")
    plot <- ggplot(long,aes(event_time,value,colour=series)) + geom_line(linewidth=.6) +
      geom_vline(xintercept=c(-15,0),linetype="dashed",colour="grey55") +
      scale_colour_manual(values=c(treated="#1B4965",synthetic="#C45A32")) +
      scale_y_continuous(expand=expansion(mult=c(.05,.2))) +
      annotate("label",x=Inf,y=Inf,hjust=1.02,vjust=1.05,size=2.6,label=paste0("Counties T/C: ",meta$counties,"/",meta$control_counties,
        " | States T/C: 1/",meta$control_states,"\nState events: 1 | County fits: ",meta$counties)) +
      labs(title=paste("Placebo state",meta$placebo_state,"| law",meta$g,"|",meta$comparison),
        subtitle=paste(pr,"| Representative state average; all county fits retained in RDS"),x="Birth year relative to assigned law",y=annual_outcomes[[oc]],colour=NULL) + theme_minimal(base_size=10)
    print(plot)
  }
  grDevices::dev.off()
  fwrite(pages[,.(state_key,g,comparison,placebo_state)],file.path(dest,"representative_fit_pages.csv"))
  message("Reported ",pr,"/",oc)
}
summary_files <- unlist(lapply(names(annual_profiles),function(pr) file.path(root,pr,names(annual_outcomes),"inference_summary.csv")))
fwrite(rbindlist(lapply(summary_files[file.exists(summary_files)],fread)),file.path(root,"inference_summary.csv"))
writeLines(c("Joint placebo draws assign the observed law schedule to distinct states that remain actually untreated throughout the required exposure window.",
  "All assigned states are excluded from affected donor pools. Each draw rebuilds county selection and matching before fitting SDID with AMWS pre-outcomes.",
  "State means use the corresponding observed state's fixed county-share weight. Common comparisons use matched county intersections; attributes-only full uses all matched counties.",
  "Primary statistic: pooled reference-normalized gap averaged over event years 0-20, reference -15. The two-sided tail rank uses (1 + extreme draws)/(1 + draws).",
  "Zero-pre-RMSPE events remain in the primary statistic. No pre-fit trimming or post-outcome assignment rejection is applied.",
  "Tail ranks are conditional on this state-assignment benchmark, not experimental randomization p-values. Pointwise envelopes are placebo benchmarks, not confidence intervals.",
  "Pre40 has at most 54 structural assignments (21 distinct selected-state sets); Ohio is always replaced by Georgia and New Jersey by Louisiana. Matching-support checks may reduce this further.",
  "Observed county raw-fit books and the earlier local county diagnostics remain in the original richer/attributes result directories."),file.path(root,"inference_notes.txt"))
final_summary <- fread(file.path(root,"inference_summary.csv"))
rate_summary <- final_summary[outcome=="amws_per_1000_births"]
rows <- vapply(seq_len(nrow(rate_summary)),function(i) {
  x<-rate_summary[i]
  sprintf("| %s | %s | %d | %.5f | %.4f |",x$profile,x$comparison,x$assignments,x$observed_statistic,x$state_placebo_tail_rank)
},character(1))
writeLines(c("# Mandatory schooling: joint state-level placebo results", "",
  "Each draw assigns the observed law dates to distinct eligible states, removes all assigned states from the donor pools, rebuilds county matching, and fits county synthetic controls. State averages retain the corresponding observed states' county-share weights.", "",
  "Both richer and attributes-only matching retain pre-period AMWS for synthetic-DiD weighting. The paired observed samples contain 113 counties in 10 states for pre20 and 52 counties in six states for pre40. Attributes-only full contains 119 pre20 counties and the same 52 pre40 counties.", "",
  "## AMWS per 1,000 estimated births", "",
  "The statistic is the mean pooled reference-normalized gap over event years 0-20, reference -15. Tail ranks use joint state assignments and the plus-one correction.", "",
  "| Window | Comparison | Assignments | Observed mean gap | State-placebo tail rank |", "|---|---|---:|---:|---:|", rows, "",
  "## Interpretation and support", "",
  "These ranks are conditional on the state-assignment benchmark. Law adoption was not randomized, and the constrained control-state geography and weak raw pre-period fits limit causal interpretation. Pointwise placebo envelopes are not confidence intervals.", "",
  "Pre20 samples 999 unique assignments, spanning 71 selected-state sets. Pre40 exhausts 54 assignments spanning 21 sets; Georgia always replaces Ohio and Louisiana always replaces New Jersey. Matching support is checked jointly across all four outcomes before estimation. No post-treatment selection or additional pre-fit trimming is applied.", "",
  "## Outputs", "",
  "Each profile/outcome directory contains pooled_state_placebos.pdf, state_placebo_distribution.pdf, representative_placebo_state_fits.pdf, complete state paths and membership, pooled draws, statistics, county fit diagnostics, and resumable fit records. Matching records retain donor selections and balance; fit records retain weights and every county path.", "",
  "Profile-level assignments.csv, assignment_audit.csv, state_slots.csv, county_eligibility.csv, and candidate_state_coverage.csv document assignment and sample construction. Archived code and manifests preserve the actual source versions used.", "",
  "validation.csv records production reconstruction checks. design_validation.csv records provenance, full eligibility accounting, and identical attributes-only donor selections across all outcomes. inference_summary.csv contains all four outcomes."),
  file.path(root,"README.md"))
