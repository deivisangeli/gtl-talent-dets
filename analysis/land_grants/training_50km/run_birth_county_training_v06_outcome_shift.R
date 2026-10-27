# Shifted-decade definitions compared: outcome only shifted (treatment at g_std) against outcome and treatment shifted (v06 alternative_decade).
aa<-commandArgs(FALSE)
script<-normalizePath(sub("^--file=","",aa[grepl("^--file=",aa)][1]),winslash="/")
repo<-normalizePath(file.path(dirname(script),"../../.."),winslash="/")
for(f in c("amws_training_helpers.R","amws_training_calendar_helpers.R","amws_birth_county_training_helpers.R"))source(file.path(repo,"prep/land_grants",f))
birth_training_stamp <- "amws_birth_county_training_postsecondary_v06_20261021"
shift_stamp <- "amws_birth_county_training_v06_outcome_shift_20261022"
timing <- "outcome_only_shift"
suppressPackageStartupMessages(library(did))
suppressPackageStartupMessages(library(ggplot2))
setDTthreads(4)
root<-training_root();prep<-file.path(root,"output/land_grants",birth_training_stamp)
both<-file.path(root,"results/land_grants/event_studies",birth_training_stamp)
dest<-file.path(root,"results/land_grants/event_studies",shift_stamp)
if(dir.exists(dest)){invisible(training_check_release(dest));cat("Verified existing outcome-shift analysis release.\n");quit(status=0)}
invisible(training_check_release(prep));invisible(training_check_release(both))
started<-proc.time()[3]
stage<-file.path(Sys.getenv("AMWS_CHECKPOINT_ROOT",unset=file.path(Sys.getenv("USERPROFILE"),".codex/amws_checkpoints")),shift_stamp,"results_stage")
dir.create(file.path(stage,"models"),recursive=TRUE,showWarnings=FALSE)
sources<-c(script,file.path(repo,"prep/land_grants",c("amws_training_helpers.R","amws_training_calendar_helpers.R","amws_birth_county_training_helpers.R")),
  file.path(prep,c("andrews_event_county_units_1850_1920.csv","county_decade_alternative_decade.csv","frozen_relative_standard_decade.csv","frozen_calendar_standard_decade.csv")),
  file.path(both,c("dynamic_att.csv","post_treatment_att.csv","analysis_sample_summary.csv")))
inputs<-training_hashes(sources);signature<-digest::digest(inputs,algo="sha256")
models<-list();coverage<-list();membership<-list()
units<-fread(file.path(prep,"andrews_event_county_units_1850_1920.csv"),colClasses=c(GEOID="character"))
p<-fread(file.path(prep,"county_decade_alternative_decade.csv"),colClasses=c(GEOID="character"))
for(balance in c("relative","calendar")) {
  model_index<-0L;window<-if(balance=="relative")c(-20L,70L)else c(-50L,70L)
  # The standard-decade sample keeps treatment at g_std; its baseline x is replaced below.
  f<-fread(file.path(prep,paste0("frozen_",balance,"_standard_decade.csv")),colClasses=c(GEOID="character"))
  expected<-birth_training_expected(balance,"standard_decade")
  training_require(identical(as.integer(c(uniqueN(f$event_id),uniqueN(f$stack_unit_num),nrow(f))),expected),"Standard sample dimensions differ")
  meta<-unique(units[,.(GEOID,treatment_decade=g_std)])
  baseline<-merge(meta,p[,.(GEOID,decade,county_births_estimate)],by="GEOID",allow.cartesian=TRUE)[decade==treatment_decade-10L,.(GEOID,x_shift=county_births_estimate)]
  training_require(!anyDuplicated(baseline$GEOID),"Ambiguous shifted baseline")
  f[,row_order:=.I]
  panel<-merge(f[,!"x"],baseline,by="GEOID",all.x=TRUE,sort=FALSE)
  panel<-merge(panel,p,by=c("GEOID","decade"),all.x=TRUE,sort=FALSE)
  setorder(panel,row_order);panel[,row_order:=NULL];setnames(panel,"x_shift","x")
  training_require(identical(panel[,.(stack_unit_num,GEOID,decade,g,event_id,sample_role)],f[,.(stack_unit_num,GEOID,decade,g,event_id,sample_role)]),"Standard sample altered")
  training_require(all(is.finite(panel$x)&panel$x>0),"Shifted baseline births missing")
  check<-merge(panel[,c("GEOID","decade",unname(birth_training_rates)),with=FALSE],p[,c("GEOID","decade",unname(birth_training_rates)),with=FALSE],by=c("GEOID","decade"),suffixes=c("","_panel"))
  training_require(nrow(check)==nrow(panel)&&all(vapply(unname(birth_training_rates),function(k)identical(check[[k]],check[[paste0(k,"_panel")]]),logical(1))),"Outcomes differ from the shifted panel")
  training_calendar_grid<-sort(unique(panel$decade))
  fwrite(panel,file.path(stage,paste0("analysis_panel_",balance,"_",timing,".csv")))
  coverage[[balance]]<-data.table(balance=balance,timing=timing,events=expected[1],units=expected[2],observations=expected[3],counties=uniqueN(panel$GEOID),east_carolina=50L%in%panel$event_id)
  for(group in names(birth_training_rates)) {
    # Same seeds as the v06 standard-decade models, which share this sample.
    model_index<-model_index+1L;seed<-20261014L+model_index
    model_name<-paste(balance,timing,group,sep="__")
    data_es<-as.data.frame(panel[,.(stack_unit_num,GEOID,decade,g,event_id,sample_role,y=get(birth_training_rates[[group]]),x)])
    training_require(all(is.finite(data_es$y)&is.finite(data_es$x)&data_es$x>0),"Invalid estimation rows")
    path<-file.path(stage,"models",paste0(model_name,".rds"))
    m<-if(file.exists(path))readRDS(path)else NULL
    if(is.null(m)||!identical(m$input_signature,signature)) {
      set.seed(seed);ww<-character();tick<-proc.time()[3]
      cat("Running",model_name,"rows",nrow(data_es),"\n")
      m<-tryCatch(withCallingHandlers({
        att<-did::att_gt(yname="y",tname="decade",idname="stack_unit_num",gname="g",xformla=~x,data=data_es,control_group="nevertreated",est_method="reg",base_period="universal",allow_unbalanced_panel=TRUE,cores=4)
        dyn<-did::aggte(att,type="dynamic",na.rm=TRUE,min_e=window[1],max_e=window[2])
        simple<-did::aggte(att,type="simple",na.rm=TRUE,min_e=0L,max_e=70L)
        list(ok=TRUE,att=att,dynamic=dyn,simple=simple,renormalized=training_renormalize(dyn),error=NA_character_)
      },warning=function(w){ww<<-c(ww,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)list(ok=FALSE,error=conditionMessage(e)))
      m$warnings<-ww;m$elapsed_seconds<-unname(proc.time()[3]-tick);m$input_signature<-signature
      m$model<-model_name;m$balance<-balance;m$timing<-timing;m$group<-group;m$seed<-seed
      m$n_rows<-nrow(data_es);m$n_units<-uniqueN(data_es$stack_unit_num);m$n_events<-uniqueN(data_es$event_id)
      saveRDS(m,path)
    }
    models[[model_name]]<-m
    membership[[model_name]]<-as.data.table(data_es)[,y:=NULL][,`:=`(model=model_name,balance=balance,timing=timing,group=group)]
  }
}
status<-rbindlist(lapply(models,function(m)data.table(model=m$model,balance=m$balance,timing=m$timing,group=m$group,ok=m$ok,n_rows=m$n_rows,n_units=m$n_units,n_events=m$n_events,seed=m$seed,elapsed_seconds=m$elapsed_seconds,warning_count=length(m$warnings),error=m$error)))
fwrite(status,file.path(stage,"model_status.csv"))
fwrite(rbindlist(lapply(models,function(m)if(length(m$warnings))data.table(model=m$model,message=m$warnings))),file.path(stage,"warnings.csv"))
good<-Filter(function(m)isTRUE(m$ok),models)
training_require(length(good)>0,"All estimations failed; see local model status")
dyn<-rbindlist(lapply(good,function(m)copy(m$renormalized)[,`:=`(model=m$model,balance=m$balance,timing=m$timing,group=m$group)]))
training_require(all(is.finite(dyn$estimate)&is.finite(dyn$se)&dyn$se>=0),"Invalid dynamic inference")
fwrite(dyn,file.path(stage,"dynamic_att.csv"))
simple<-rbindlist(lapply(good,function(m)data.table(model=m$model,balance=m$balance,timing=m$timing,group=m$group,estimate=m$simple$overall.att,se=m$simple$overall.se,ci_low=m$simple$overall.att-1.96*m$simple$overall.se,ci_high=m$simple$overall.att+1.96*m$simple$overall.se,reference="Default pre-treatment base; not normalized to e=0")))
fwrite(simple,file.path(stage,"post_treatment_att.csv"))
fwrite(rbindlist(membership),file.path(stage,"estimation_sample_membership.csv"))
fwrite(rbindlist(coverage),file.path(stage,"analysis_sample_summary.csv"))
# Each spatial partition has the same county-born total benchmark.
components <- copy(dyn[group != "total"])
components[, geography := sub("__.*$", "", group)]
decomp <- merge(components[, .(components=.N, sum_components=sum(estimate)), by=.(balance,timing,geography,event_time)],
  dyn[group=="total",.(balance,timing,event_time,benchmark=estimate)], by=c("balance","timing","event_time"),all.x=TRUE)
decomp[,difference:=sum_components-benchmark]
decomp[,passed:=components==3L & is.finite(difference) & abs(difference)<=1e-8*pmax(1,abs(benchmark))]
fwrite(decomp,file.path(stage,"effect_decomposition_check.csv"))
simple_parts <- copy(simple[group != "total"])
simple_parts[, geography := sub("__.*$", "", group)]
simple_decomp <- merge(simple_parts[,.(components=.N,sum_components=sum(estimate)),by=.(balance,timing,geography)],
  simple[group=="total",.(balance,timing,benchmark=estimate)],by=c("balance","timing"),all.x=TRUE)
simple_decomp[,difference:=sum_components-benchmark]
simple_decomp[,passed:=components==3L & is.finite(difference) & abs(difference)<=1e-8*pmax(1,abs(benchmark))]
fwrite(simple_decomp,file.path(stage,"post_treatment_decomposition_check.csv"))
training_require(nrow(status)==14L && all(status$ok) && all(decomp$passed) && all(simple_decomp$passed),
  "Model validation failed; local outputs preserved, publication blocked")

# Side by side with the v06 shifted-decade models (outcome and treatment shifted).
labels <- c(alternative_decade="Outcome and treatment shifted", outcome_only_shift="Outcome only shifted")
both_dyn <- fread(file.path(both,"dynamic_att.csv"))[timing=="alternative_decade"]
both_simple <- fread(file.path(both,"post_treatment_att.csv"))[timing=="alternative_decade"]
both_sizes <- fread(file.path(both,"analysis_sample_summary.csv"))[timing=="alternative_decade"]
for(x in list(list(a=both_dyn,b=dyn,keys=c("balance","group","event_time"),name="shift_definition_comparison_dynamic.csv"),
              list(a=both_simple,b=simple,keys=c("balance","group"),name="shift_definition_comparison_post_treatment.csv"))) {
  cols <- c(x$keys,"estimate","se","ci_low","ci_high")
  cmp <- merge(x$a[,..cols],x$b[,..cols],by=x$keys,all=TRUE,suffixes=c("_both_shifted","_outcome_only"))
  cmp[,difference:=estimate_outcome_only-estimate_both_shifted]
  fwrite(cmp,file.path(stage,x$name))
}
sizes <- rbind(both_sizes[,.(balance,timing,events,units)],rbindlist(coverage)[,.(balance,timing,events,units)])
figure_data <- rbind(both_dyn[,.(balance,timing,group,event_time,estimate,ci_low,ci_high)],dyn[,.(balance,timing,group,event_time,estimate,ci_low,ci_high)])[group!="total"]
figure_data[, category := factor(sub("^.*__", "", group), levels=c("ever","never","unknown"),
  labels=c("Ever trained locally","Never trained locally","Training unknown"))]
figure_data[, geography_label := factor(sub("__.*$","",group),levels=c("same_county","50km"),
  labels=c("Inside birth county","Within 50 km of birth-county centroid"))]
figure_data[, definition := factor(labels[timing],levels=unname(labels))]
figure_data[, x_pos := event_time + ifelse(timing=="alternative_decade",-1.5,1.5)]
for(bal in c("relative","calendar")) {
  z <- figure_data[balance==bal]
  n <- sizes[balance==bal]
  size_note <- paste0(labels[n$timing],": ",n$events," events, ",n$units," units",collapse="; ")
  plot <- ggplot(z,aes(x_pos,estimate,colour=definition)) +
    geom_hline(yintercept=0,colour="grey60",linetype="dashed",linewidth=.3) +
    geom_vline(xintercept=0,colour="grey75",linewidth=.3) +
    geom_errorbar(aes(ymin=ci_low,ymax=ci_high),width=1.5,linewidth=.4) +
    geom_line(linewidth=.4) + geom_point(size=1.4) +
    scale_colour_manual(values=c("#28618C","#C2571A"),name=NULL) +
    facet_grid(geography_label~category) + scale_x_continuous(breaks=seq(-50,70,20)) +
    labs(title="College allocation and training of scientists born in the county",
      subtitle=paste(if(bal=="relative")"Relative-time-balanced sample" else "Calendar-balanced sample","two shifted-decade definitions",sep="; "),
      x="Years relative to allocation decade",y="Effect on scientists per 1,000 county births",
      caption=paste0("CSDID regression with baseline births (shifted decade before allocation); reference e = 0; pointwise 95% confidence intervals. Series offset by 1.5 years for legibility.\n",
        size_note,". Education release v06.")) +
    theme_classic(base_size=11) + theme(strip.background=element_rect(fill="grey95",colour=NA),
      strip.text=element_text(face="bold",size=10),plot.title=element_text(face="bold"),
      plot.caption=element_text(hjust=0,size=9),legend.position="top")
  for(ext in c("png","pdf")) ggsave(file.path(stage,paste0("decade_shift_comparison_",bal,".",ext)),plot,width=14,height=7.5,dpi=300)
}
validation <- list(passed=TRUE,successful_models=sum(status$ok),models_requested=14L,
  category_effects_sum_to_total=TRUE,maximum_decomposition_error=max(abs(decomp$difference)),
  maximum_post_treatment_decomposition_error=max(abs(simple_decomp$difference)),
  standard_samples_preserved=TRUE,outcomes_equal_shifted_panel=TRUE,comparison_figures=2L)
training_json(validation,file.path(stage,"validation_report.json"))
training_json(list(base_seed=20261014L,model_seeds=as.list(setNames(status$seed,status$model)),
  est_method="reg",control_group="nevertreated",base_period="universal",xformla="~ county births in the shifted decade g_std-10",
  treatment="g_std (standard decade of allocation)",outcome_decades="alternative_decade (years ending 7-9 moved to the next decade)",
  relative_window=c(-20L,70L),calendar_window=c(-50L,70L),reference_event_time=0L,pointwise_confidence=.95,
  inference="Preserved did defaults; dynamic estimates and influence functions shifted to e=0"),file.path(stage,"model_specification.json"))
dir.create(file.path(stage,"code_snapshot"),showWarnings=FALSE)
for(p in sources[seq_len(4)])file.copy(p,file.path(stage,"code_snapshot",basename(p)),overwrite=TRUE)
training_json(list(elapsed_seconds=unname(proc.time()[3]-started)),file.path(stage,"timing_report.json"))
capture.output(sessionInfo(),file=file.path(stage,"sessionInfo.txt"))
training_publish(stage,dest,inputs,list(status="complete",validation=validation))
print(status);print(validation)
