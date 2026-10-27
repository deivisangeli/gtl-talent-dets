# Birth-county training outcomes (multi_v04) on the unbalanced event sample:
# no event-time or calendar balancing. A unit enters when it has positive births
# in the decade before allocation (its baseline covariate) and at least one
# decade from allocation on; an event needs its treated county and one runner-up.
# Six category rates plus the shared total, standard and shifted decades.
aa<-commandArgs(FALSE)
script<-normalizePath(sub("^--file=","",aa[grepl("^--file=",aa)][1]),winslash="/")
repo<-normalizePath(file.path(dirname(script),"../../.."),winslash="/")
for(f in c("amws_training_helpers.R","amws_training_calendar_helpers.R","amws_birth_county_training_helpers.R"))source(file.path(repo,"prep/land_grants",f))
birth_training_stamp <- "amws_birth_county_training_postsecondary_v04_unbalanced_20261019"
prep_stamp <- "amws_birth_county_training_postsecondary_v04_20261019"
suppressPackageStartupMessages(library(did))
suppressPackageStartupMessages(library(ggplot2))
setDTthreads(4)
window<-c(-30L,90L);post_window<-c(0L,70L)
expected<-list(standard_decade=c(37L,116L,1390L),alternative_decade=c(38L,118L,1528L))
root<-training_root();prep<-file.path(root,"output/land_grants",prep_stamp)
balanced<-file.path(root,"results/land_grants/event_studies",prep_stamp)
dest<-file.path(root,"results/land_grants/event_studies",birth_training_stamp)
if(dir.exists(dest)){invisible(training_check_release(dest));cat("Verified existing unbalanced birth-county training analysis release.\n");quit(status=0)}
invisible(training_check_release(prep));invisible(training_check_release(balanced))
started<-proc.time()[3]
stage<-file.path(Sys.getenv("AMWS_CHECKPOINT_ROOT",unset=file.path(Sys.getenv("USERPROFILE"),".codex/amws_checkpoints")),birth_training_stamp,"results_stage")
dir.create(file.path(stage,"models"),recursive=TRUE,showWarnings=FALSE)
sources<-c(script,file.path(repo,"prep/land_grants",c("amws_training_helpers.R","amws_training_calendar_helpers.R","amws_birth_county_training_helpers.R")),
  file.path(prep,c("andrews_event_county_units_1850_1920.csv",paste0("county_decade_",names(expected),".csv"),paste0("frozen_relative_",names(expected),".csv"),"completion_manifest.json")),
  file.path(balanced,c("dynamic_att.csv","post_treatment_att.csv","completion_manifest.json")))
inputs<-training_hashes(sources);signature<-digest::digest(inputs,algo="sha256")
units<-fread(file.path(prep,"andrews_event_county_units_1850_1920.csv"),colClasses=c(GEOID="character"))
training_require(uniqueN(units$event_id)==57L,"Event pool differs")
# Stack numbering follows build_stack() in temporal_support_1830_1950/_sample_helpers.R.
units[,stack_unit_id:=paste(event_id,GEOID,sample_role,sep="_")]
# dplyr::dense_rank orders in the C locale; a locale-aware sort() would renumber units.
ids<-sort(unique(units$stack_unit_id),method="radix");units[,stack_unit_num:=match(stack_unit_id,ids)]

models<-list();supports<-list();cells<-list();diagnostics<-list();membership<-list();samples<-list();excluded<-list()
model_index<-0L
for(timing in names(expected)) {
  p<-fread(file.path(prep,paste0("county_decade_",timing,".csv")),colClasses=c(GEOID="character"))
  rates<-unname(birth_training_rates)
  usable<-p[is.finite(county_births_estimate) & county_births_estimate>0]
  usable<-usable[Reduce(`&`,lapply(rates,function(r)is.finite(usable[[r]])))]
  s<-copy(units)[,`:=`(treatment_decade=get(if(timing=="standard_decade")"g_std"else"g_shift"))]
  s[,g:=fifelse(sample_role=="treated",treatment_decade,0L)]
  base<-usable[,.(GEOID,control_decade=decade,x=county_births_estimate)]
  s<-merge(s[,control_decade:=treatment_decade-10L],base,by=c("GEOID","control_decade"),all.x=TRUE)
  last<-usable[,.(last_decade=max(decade)),by=GEOID]
  s<-merge(s,last,by="GEOID",all.x=TRUE)
  s[,unit_ok:=is.finite(x) & x>0 & is.finite(last_decade) & last_decade>=treatment_decade]
  ev<-s[,.(treated_ok=any(sample_role=="treated" & unit_ok),runner_up_ok=any(sample_role!="treated" & unit_ok),
           college=college[sample_role=="treated"][1],treatment_decade=treatment_decade[1]),by=event_id]
  ev[,reason:=fcase(!treated_ok,"treated county has no births in the decade before allocation or no later decade",
                    !runner_up_ok,"no runner-up with births in the decade before allocation and a later decade",default="included")]
  excluded[[timing]]<-ev[,timing:=timing]
  keep<-s[unit_ok==TRUE & event_id %in% ev[reason=="included",event_id]]
  panel<-merge(keep[,.(stack_unit_num,GEOID,g,event_id,sample_role,x,site_id=stack_unit_id)],usable,by="GEOID",allow.cartesian=TRUE)
  setorder(panel,event_id,sample_role,GEOID,decade)
  training_require(identical(as.integer(c(uniqueN(panel$event_id),uniqueN(panel$stack_unit_num),nrow(panel))),expected[[timing]]),"Unbalanced sample dimensions differ")
  # Every unit of the event-time-balanced sample is here, with the same identifiers and baseline.
  frozen<-fread(file.path(prep,paste0("frozen_relative_",timing,".csv")),colClasses=c(GEOID="character"))
  overlap<-merge(unique(frozen[,.(stack_unit_num,GEOID,g,event_id,x)]),unique(panel[,.(stack_unit_num,GEOID,g,event_id,x_new=x)]),by=c("stack_unit_num","GEOID","g","event_id"),all.x=TRUE)
  training_require(!anyNA(overlap$x_new) && isTRUE(all.equal(overlap$x,overlap$x_new,tolerance=1e-12)),"Balanced units differ in the unbalanced sample")
  fwrite(panel,file.path(stage,paste0("analysis_panel_unbalanced_",timing,".csv")))
  samples[[timing]]<-data.table(timing=timing,events=uniqueN(panel$event_id),units=uniqueN(panel$stack_unit_num),observations=nrow(panel),
    counties=uniqueN(panel$GEOID),first_decade=min(panel$decade),last_decade=max(panel$decade))
  training_calendar_grid<-sort(unique(panel$decade))
  for(group in names(birth_training_rates)) {
    model_index<-model_index+1L;seed<-20261014L+model_index
    model_name<-paste("unbalanced",timing,group,sep="__")
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
        simple<-did::aggte(att,type="simple",na.rm=TRUE,min_e=post_window[1],max_e=post_window[2])
        list(ok=TRUE,att=att,dynamic=dyn,simple=simple,renormalized=training_renormalize(dyn),error=NA_character_)
      },warning=function(w){ww<<-c(ww,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)list(ok=FALSE,error=conditionMessage(e)))
      m$warnings<-ww;m$elapsed_seconds<-unname(proc.time()[3]-tick);m$input_signature<-signature
      m$model<-model_name;m$timing<-timing;m$group<-group;m$seed<-seed
      m$n_rows<-nrow(data_es);m$n_units<-uniqueN(data_es$stack_unit_num);m$n_events<-uniqueN(data_es$event_id)
      saveRDS(m,path)
    }
    models[[model_name]]<-m
    membership[[model_name]]<-as.data.table(data_es)[,y:=NULL][,`:=`(model=model_name,timing=timing,group=group)]
    if(!isTRUE(m$ok))next
    # The estimator saw exactly the saved panel, dependent variable included.
    fitted<-as.data.table(m$att$DIDparams$data)[,.(stack_unit_num,decade,fitted_g=g,fitted_y=y,fitted_x=x)]
    chk<-merge(as.data.table(data_es)[,.(stack_unit_num,decade,g,y,x)],fitted,by=c("stack_unit_num","decade"),all=TRUE)
    training_require(nrow(chk)==nrow(data_es)&&!anyNA(chk)&&all(ifelse(chk$g==0,Inf,chk$g)==chk$fitted_g)&&
      max(abs(chk$y-chk$fitted_y))<1e-12&&max(abs(chk$x-chk$fitted_x))<1e-10,"Estimator inputs differ from the saved panel")
    diagnostic<-training_calendar_covariance(m$att)
    diagnostic$summary[,`:=`(model=model_name,timing=timing,group=group)]
    diagnostics[[model_name]]<-diagnostic$summary
    support<-training_calendar_support(m$att,data_es,seq(window[1],window[2],10L))
    support$support[,`:=`(model=model_name,timing=timing,group=group)]
    support$cells[,`:=`(model=model_name,timing=timing,group=group)]
    supports[[model_name]]<-support$support;cells[[model_name]]<-support$cells
  }
}
status<-rbindlist(lapply(models,function(m)data.table(model=m$model,timing=m$timing,group=m$group,ok=m$ok,n_rows=m$n_rows,n_units=m$n_units,n_events=m$n_events,seed=m$seed,elapsed_seconds=m$elapsed_seconds,warning_count=length(m$warnings),error=m$error)))
fwrite(status,file.path(stage,"model_status.csv"))
warnings<-rbindlist(lapply(models,function(m)if(length(m$warnings))data.table(model=m$model,message=m$warnings)))
fwrite(warnings,file.path(stage,"warnings.csv"))
good<-Filter(function(m)isTRUE(m$ok),models)
training_require(length(good)>0,"All estimations failed; see local model status")
dyn<-rbindlist(lapply(good,function(m)copy(m$renormalized)[,`:=`(model=m$model,timing=m$timing,group=m$group)]))
training_require(all(is.finite(dyn$estimate)&is.finite(dyn$se)&dyn$se>=0),"Invalid dynamic inference")
support<-rbindlist(supports)
dyn<-merge(dyn,support[,.(model,event_time,n_events,n_treated_sites)],by=c("model","event_time"),all.x=TRUE)
setorder(dyn,timing,group,event_time)
fwrite(dyn,file.path(stage,"dynamic_att.csv"))
simple<-rbindlist(lapply(good,function(m)data.table(model=m$model,timing=m$timing,group=m$group,estimate=m$simple$overall.att,se=m$simple$overall.se,ci_low=m$simple$overall.att-1.96*m$simple$overall.se,ci_high=m$simple$overall.att+1.96*m$simple$overall.se,reference="Default pre-treatment base; not normalized to e=0; post window e=0..70")))
fwrite(simple,file.path(stage,"post_treatment_att.csv"))
fwrite(rbindlist(membership),file.path(stage,"estimation_sample_membership.csv"))
fwrite(support,file.path(stage,"event_time_support.csv"))
fwrite(rbindlist(cells),file.path(stage,"cohort_event_time_support.csv"))
fwrite(rbindlist(diagnostics),file.path(stage,"pretrend_test_diagnostics.csv"))
fwrite(rbindlist(samples),file.path(stage,"analysis_sample_summary.csv"))
ex<-rbindlist(excluded);fwrite(ex[reason=="included"],file.path(stage,"included_events.csv"));fwrite(ex[reason!="included"],file.path(stage,"excluded_events.csv"))
components<-copy(dyn[group!="total"]);components[,geography:=sub("__.*$","",group)]
decomp<-merge(components[,.(components=.N,sum_components=sum(estimate)),by=.(timing,geography,event_time)],
  dyn[group=="total",.(timing,event_time,benchmark=estimate)],by=c("timing","event_time"),all.x=TRUE)
decomp[,difference:=sum_components-benchmark][,passed:=components==3L&is.finite(difference)&abs(difference)<=1e-8*pmax(1,abs(benchmark))]
fwrite(decomp,file.path(stage,"effect_decomposition_check.csv"))
simple_parts<-copy(simple[group!="total"]);simple_parts[,geography:=sub("__.*$","",group)]
simple_decomp<-merge(simple_parts[,.(components=.N,sum_components=sum(estimate)),by=.(timing,geography)],
  simple[group=="total",.(timing,benchmark=estimate)],by="timing",all.x=TRUE)
simple_decomp[,difference:=sum_components-benchmark][,passed:=components==3L&is.finite(difference)&abs(difference)<=1e-8*pmax(1,abs(benchmark))]
fwrite(simple_decomp,file.path(stage,"post_treatment_decomposition_check.csv"))
training_require(nrow(status)==14L&&all(status$ok)&&all(decomp$passed)&&all(simple_decomp$passed),"Model validation failed; local outputs preserved, publication blocked")

# Side by side with the event-time-balanced v04 estimates of the same timing.
for(name in c("dynamic_att.csv","post_treatment_att.csv")) {
  before<-fread(file.path(balanced,name))[balance=="relative"]
  after<-if(name=="dynamic_att.csv")dyn else simple
  keys<-c("timing","group",if(name=="dynamic_att.csv")"event_time")
  compared<-merge(before[,c(keys,"estimate","se"),with=FALSE],after[,c(keys,"estimate","se"),with=FALSE],by=keys,all=TRUE,suffixes=c("_balanced","_unbalanced"))
  fwrite(compared,file.path(stage,paste0("unbalanced_vs_balanced_",name)))
}

figure_data<-copy(components)
figure_data[,category:=factor(sub("^.*__","",group),levels=c("ever","never","unknown"),labels=c("Ever trained locally","Never trained locally","Training unknown"))]
figure_data[,geography_label:=factor(geography,levels=c("same_county","50km"),labels=c("Inside birth county","Within 50 km of birth-county centroid"))]
for(tim in names(expected)) {
  z<-figure_data[timing==tim]
  lo<-min(z$ci_low);counts<-unique(z[,.(event_time,n_events,category,geography_label)])
  plot<-ggplot(z,aes(event_time,estimate))+
    geom_hline(yintercept=0,colour="grey60",linetype="dashed",linewidth=.3)+
    geom_vline(xintercept=0,colour="grey75",linewidth=.3)+
    geom_errorbar(aes(ymin=ci_low,ymax=ci_high),width=1.5,colour="#28618C",linewidth=.4)+
    geom_line(colour="#28618C",linewidth=.4)+geom_point(colour="#28618C",size=1.4)+
    geom_text(data=counts,aes(event_time,lo,label=n_events),inherit.aes=FALSE,size=2.3,colour="grey35",vjust=1.4)+
    facet_grid(geography_label~category)+scale_x_continuous(breaks=seq(window[1],window[2],20))+
    labs(title="College allocation and training of scientists born in the county",
      subtitle=paste("Unbalanced sample (no event-time or calendar balancing);",if(tim=="standard_decade")"standard decades" else "shifted decades"),
      x="Years relative to allocation decade",y="Effect on scientists per 1,000 county births",
      caption=paste0("CSDID regression with baseline births; reference e = 0; pointwise 95% confidence intervals. Grey numbers: events contributing at each event time.\n",
        "Late horizons rest on early allocations only. All earned postsecondary degrees and credentials (multi_v04 extraction); historical campuses unverified."))+
    theme_classic(base_size=11)+theme(strip.background=element_rect(fill="grey95",colour=NA),strip.text=element_text(face="bold",size=10),
      plot.title=element_text(face="bold"),plot.caption=element_text(hjust=0,size=9))
  for(ext in c("png","pdf"))ggsave(file.path(stage,paste0("birth_county_training_unbalanced_",tim,".",ext)),plot,width=14,height=7.5,dpi=300)
}
validation<-list(passed=TRUE,successful_models=sum(status$ok),models_requested=14L,
  sample=rbindlist(samples),excluded_events=ex[reason!="included",.N,by=.(timing,reason)],
  balanced_units_contained_with_same_identifiers_and_baseline=TRUE,estimator_inputs_verified=TRUE,
  category_effects_sum_to_total=TRUE,maximum_decomposition_error=max(abs(decomp$difference)),
  maximum_post_treatment_decomposition_error=max(abs(simple_decomp$difference)),
  event_time_balancing=FALSE,calendar_balancing=FALSE,dynamic_window=window,post_treatment_window=post_window)
training_json(validation,file.path(stage,"validation_report.json"))
training_json(list(base_seed=20261014L,model_seeds=as.list(setNames(status$seed,status$model)),
  est_method="reg",control_group="nevertreated",base_period="universal",xformla="~ county births at g-10",
  sample_rule="unit: births>0 at g-10 and a usable decade >= g; event: treated unit plus >=1 runner-up; no balancing",
  dynamic_window=window,post_treatment_window=post_window,reference_event_time=0L,pointwise_confidence=.95,
  inference="Preserved did defaults; dynamic estimates and influence functions shifted to e=0",
  historical_campuses_verified=FALSE),file.path(stage,"model_specification.json"))
dir.create(file.path(stage,"code_snapshot"),showWarnings=FALSE)
for(p in sources[seq_len(4)])file.copy(p,file.path(stage,"code_snapshot",basename(p)),overwrite=TRUE)
training_json(list(elapsed_seconds=unname(proc.time()[3]-started)),file.path(stage,"timing_report.json"))
capture.output(sessionInfo(),file=file.path(stage,"sessionInfo.txt"))
training_publish(stage,dest,inputs,list(status="complete",validation=validation))
print(status);print(rbindlist(samples));print(simple)
