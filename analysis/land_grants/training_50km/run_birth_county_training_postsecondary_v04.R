# Birth-county training outcomes from the multi_v04 education release: six rates plus a shared total, four designs.
aa<-commandArgs(FALSE)
script<-normalizePath(sub("^--file=","",aa[grepl("^--file=",aa)][1]),winslash="/")
repo<-normalizePath(file.path(dirname(script),"../../.."),winslash="/")
for(f in c("amws_training_helpers.R","amws_training_calendar_helpers.R","amws_birth_county_training_helpers.R"))source(file.path(repo,"prep/land_grants",f))
birth_training_stamp <- "amws_birth_county_training_postsecondary_v04_20261019"
previous_stamp <- "amws_birth_county_training_postsecondary_20261019"
suppressPackageStartupMessages(library(did))
suppressPackageStartupMessages(library(ggplot2))
setDTthreads(4)
root<-training_root();prep<-file.path(root,"output/land_grants",birth_training_stamp)
dest<-file.path(root,"results/land_grants/event_studies",birth_training_stamp)
if(dir.exists(dest)){invisible(training_check_release(dest));cat("Verified existing birth-county training analysis release.\n");quit(status=0)}
invisible(training_check_release(prep))
started<-proc.time()[3]
stage<-file.path(Sys.getenv("AMWS_CHECKPOINT_ROOT",unset=file.path(Sys.getenv("USERPROFILE"),".codex/amws_checkpoints")),birth_training_stamp,"results_stage")
dir.create(file.path(stage,"models"),recursive=TRUE,showWarnings=FALSE)
sources<-c(script,file.path(repo,"prep/land_grants",c("amws_training_helpers.R","amws_training_calendar_helpers.R","amws_birth_county_training_helpers.R")),file.path(prep,list.files(prep,recursive=TRUE)),file.path(root,"results/land_grants/event_studies",previous_stamp,c("dynamic_att.csv","post_treatment_att.csv")))
inputs<-training_hashes(sources);signature<-digest::digest(inputs,algo="sha256")
models<-list();supports<-list();cells<-list();diagnostics<-list();precoeff<-list();membership<-list();coverage<-list()
units<-fread(file.path(prep,"andrews_event_county_units_1850_1920.csv"),colClasses=c(GEOID="character"))
for(balance in c("relative","calendar")) {
  model_index<-0L;window<-if(balance=="relative")c(-20L,70L)else c(-50L,70L)
  for(timing in c("standard_decade","alternative_decade")) {
    f<-fread(file.path(prep,paste0("frozen_",balance,"_",timing,".csv")),colClasses=c(GEOID="character"))
    p<-fread(file.path(prep,paste0("county_decade_",timing,".csv")),colClasses=c(GEOID="character"))
    f[,row_order:=.I]
    panel<-merge(f,p,by=c("GEOID","decade"),all.x=TRUE,sort=FALSE)
    setorder(panel,row_order);f[,row_order:=NULL];panel[,row_order:=NULL]
    training_require(identical(panel[,names(f),with=FALSE],f),"Frozen sample altered")
    expected<-birth_training_expected(balance,timing)
    training_require(identical(as.integer(c(uniqueN(panel$event_id),uniqueN(panel$stack_unit_num),nrow(panel))),expected),"Sample dimensions differ")
    if(balance=="calendar")training_require(50L%in%panel$event_id && all(panel[,.N,by=stack_unit_num]$N==13L),"Calendar/East Carolina sample differs")
    meta<-unique(units[,.(GEOID,treatment_decade=get(if(timing=="standard_decade")"g_std"else"g_shift"))])
    baseline<-merge(meta,p[,.(GEOID,decade,county_births_estimate)],by="GEOID",allow.cartesian=TRUE)[decade==treatment_decade-10L,.(GEOID,expected_baseline=county_births_estimate)]
    check<-merge(panel[,.(GEOID,x)],baseline,by="GEOID",all.x=TRUE)
    training_require(all(is.finite(check$x)&check$x>0)&&isTRUE(all.equal(check$x,check$expected_baseline,tolerance=1e-12)),"County baseline differs from frozen covariate")
    training_calendar_grid<-sort(unique(panel$decade))
    fwrite(panel,file.path(stage,paste0("analysis_panel_",balance,"_",timing,".csv")))
    coverage[[paste(balance,timing)]]<-data.table(balance=balance,timing=timing,events=expected[1],units=expected[2],observations=expected[3],counties=uniqueN(panel$GEOID),east_carolina=50L%in%panel$event_id)
    for(group in names(birth_training_rates)) {
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
      if(!isTRUE(m$ok))next
      diagnostic<-training_calendar_covariance(m$att)
      diagnostic$summary[,`:=`(model=model_name,balance=balance,timing=timing,group=group)]
      diagnostic$coefficients[,`:=`(model=model_name,balance=balance,timing=timing,group=group)]
      diagnostics[[model_name]]<-diagnostic$summary;precoeff[[model_name]]<-diagnostic$coefficients
      saveRDS(diagnostic,file.path(stage,"models",paste0(model_name,"__pretest_covariance.rds")))
      support<-training_calendar_support(m$att,data_es,seq(window[1],window[2],10L))
      support$support[,`:=`(model=model_name,balance=balance,timing=timing,group=group)]
      support$cells[,`:=`(model=model_name,balance=balance,timing=timing,group=group)]
      supports[[model_name]]<-support$support;cells[[model_name]]<-support$cells
    }
  }
}
status<-rbindlist(lapply(models,function(m)data.table(model=m$model,balance=m$balance,timing=m$timing,group=m$group,ok=m$ok,n_rows=m$n_rows,n_units=m$n_units,n_events=m$n_events,seed=m$seed,elapsed_seconds=m$elapsed_seconds,warning_count=length(m$warnings),error=m$error)))
fwrite(status,file.path(stage,"model_status.csv"))
warnings<-rbindlist(lapply(models,function(m)if(length(m$warnings))data.table(model=m$model,message=m$warnings)))
fwrite(warnings,file.path(stage,"warnings.csv"))
good<-Filter(function(m)isTRUE(m$ok),models)
training_require(length(good)>0,"All estimations failed; see local model status")
dyn<-rbindlist(lapply(good,function(m)copy(m$renormalized)[,`:=`(model=m$model,balance=m$balance,timing=m$timing,group=m$group)]))
training_require(all(is.finite(dyn$estimate)&is.finite(dyn$se)&dyn$se>=0),"Invalid dynamic inference")
fwrite(dyn,file.path(stage,"dynamic_att.csv"))
simple<-rbindlist(lapply(good,function(m)data.table(model=m$model,balance=m$balance,timing=m$timing,group=m$group,estimate=m$simple$overall.att,se=m$simple$overall.se,ci_low=m$simple$overall.att-1.96*m$simple$overall.se,ci_high=m$simple$overall.att+1.96*m$simple$overall.se,reference="Default pre-treatment base; not normalized to e=0")))
fwrite(simple,file.path(stage,"post_treatment_att.csv"))
fwrite(rbindlist(membership),file.path(stage,"estimation_sample_membership.csv"))
fwrite(rbindlist(supports),file.path(stage,"event_time_support.csv"))
fwrite(rbindlist(cells),file.path(stage,"cohort_event_time_support.csv"))
fwrite(rbindlist(diagnostics),file.path(stage,"pretrend_test_diagnostics.csv"))
fwrite(rbindlist(precoeff),file.path(stage,"pretrend_coefficients.csv"))
fwrite(rbindlist(coverage),file.path(stage,"analysis_sample_summary.csv"))
# Each spatial partition has the same county-born total benchmark.
components <- copy(dyn[group != "total"])
components[, geography := sub("__.*$", "", group)]
decomp <- merge(components[, .(components=.N, sum_components=sum(estimate)),
  by=.(balance,timing,geography,event_time)],
  dyn[group=="total",.(balance,timing,event_time,benchmark=estimate)],
  by=c("balance","timing","event_time"),all.x=TRUE)
decomp[,difference:=sum_components-benchmark]
decomp[,passed:=components==3L & is.finite(difference) & abs(difference)<=1e-8*pmax(1,abs(benchmark))]
fwrite(decomp,file.path(stage,"effect_decomposition_check.csv"))
# The same check applies to the overall ATT, whose reference is unchanged.
simple_parts <- copy(simple[group != "total"])
simple_parts[, geography := sub("__.*$", "", group)]
simple_decomp <- merge(simple_parts[,.(components=.N,sum_components=sum(estimate)),by=.(balance,timing,geography)],
  simple[group=="total",.(balance,timing,benchmark=estimate)],by=c("balance","timing"),all.x=TRUE)
simple_decomp[,difference:=sum_components-benchmark]
simple_decomp[,passed:=components==3L & is.finite(difference) & abs(difference)<=1e-8*pmax(1,abs(benchmark))]
fwrite(simple_decomp,file.path(stage,"post_treatment_decomposition_check.csv"))
training_require(nrow(status)==28L && all(status$ok) && all(decomp$passed) && all(simple_decomp$passed),
  "Model validation failed; local outputs preserved, publication blocked")

figure_data <- copy(components)
figure_data[, category := factor(sub("^.*__", "", group), levels=c("ever","never","unknown"),
  labels=c("Ever trained locally","Never trained locally","Training unknown"))]
figure_data[, geography_label := factor(geography,levels=c("same_county","50km"),
  labels=c("Inside birth county","Within 50 km of birth-county centroid"))]
for(bal in c("relative","calendar")) for(tim in c("standard_decade","alternative_decade")) {
  z <- figure_data[balance==bal & timing==tim]
  plot <- ggplot(z,aes(event_time,estimate)) +
    geom_hline(yintercept=0,colour="grey60",linetype="dashed",linewidth=.3) +
    geom_vline(xintercept=0,colour="grey75",linewidth=.3) +
    geom_errorbar(aes(ymin=ci_low,ymax=ci_high),width=1.5,colour="#28618C",linewidth=.4) +
    geom_line(colour="#28618C",linewidth=.4) + geom_point(colour="#28618C",size=1.4) +
    facet_grid(geography_label~category) + scale_x_continuous(breaks=seq(-50,70,20)) +
    labs(title="College allocation and training of scientists born in the county",
      subtitle=paste(if(bal=="relative")"Relative-time-balanced sample" else "Calendar-balanced sample",
        if(tim=="standard_decade")"standard decades" else "shifted decades",sep="; "),
      x="Years relative to allocation decade",y="Effect on scientists per 1,000 county births",
      caption="CSDID regression with baseline births; reference e = 0; pointwise 95% confidence intervals.\nAll earned postsecondary degrees and credentials (multi_v04 extraction); OpenAlex reference locations, historical campuses unverified.") +
    theme_classic(base_size=11) + theme(strip.background=element_rect(fill="grey95",colour=NA),
      strip.text=element_text(face="bold",size=10),plot.title=element_text(face="bold"),
      plot.caption=element_text(hjust=0,size=9))
  for(ext in c("png","pdf")) ggsave(file.path(stage,paste0("birth_county_training_",bal,"_",tim,".",ext)),
    plot,width=14,height=7.5,dpi=300)
}
# multi_v03-versus-v04 comparisons use unchanged county-born denominators and samples.
previous_results <- file.path(root,"results/land_grants/event_studies",previous_stamp)
for(name in c("dynamic_att.csv","post_treatment_att.csv")) {
  before <- fread(file.path(previous_results,name))
  keys <- c("model","balance","timing","group",if(name=="dynamic_att.csv")"event_time")
  after <- if(name=="dynamic_att.csv")dyn else simple
  compared <- merge(before,after,by=keys,all=TRUE,suffixes=c("_old","_v04"))
  training_require(nrow(compared)==nrow(after)&&!anyNA(compared$estimate_old)&&!anyNA(compared$estimate_v04),"Prior/v04 coefficient keys differ")
  compared[,estimate_change:=estimate_v04-estimate_old]
  training_require(all(abs(compared[group=="total",estimate_change])<1e-12),"County-born total effects changed")
  fwrite(compared,file.path(stage,paste0("v03_vs_v04_",name)))
}
validation <- list(passed=TRUE,successful_models=sum(status$ok),models_requested=28L,
  category_effects_sum_to_total=TRUE,maximum_decomposition_error=max(abs(decomp$difference)),
  maximum_post_treatment_decomposition_error=max(abs(simple_decomp$difference)),
  original_county_samples_and_baselines_preserved=TRUE,calendar_east_carolina_retained=TRUE,
  scientist_birth_county_required=TRUE,pooled_catchments_used=FALSE,comparison_figures=4L)
training_json(validation,file.path(stage,"validation_report.json"))
training_json(list(base_seed=20261014L,model_seeds=as.list(setNames(status$seed,status$model)),
  est_method="reg",control_group="nevertreated",base_period="universal",xformla="~ county births at g-10",
  relative_window=c(-20L,70L),calendar_window=c(-50L,70L),reference_event_time=0L,pointwise_confidence=.95,
  inference="Preserved did defaults; dynamic estimates and influence functions shifted to e=0",
  historical_campuses_verified=FALSE),file.path(stage,"model_specification.json"))
dir.create(file.path(stage,"code_snapshot"),showWarnings=FALSE)
for(p in sources[seq_len(4)])file.copy(p,file.path(stage,"code_snapshot",basename(p)),overwrite=TRUE)
training_json(list(elapsed_seconds=unname(proc.time()[3]-started)),file.path(stage,"timing_report.json"))
capture.output(sessionInfo(),file=file.path(stage,"sessionInfo.txt"))
training_publish(stage,dest,inputs,list(status="complete",validation=validation))
print(status);print(validation)
