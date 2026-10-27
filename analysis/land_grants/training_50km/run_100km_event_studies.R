# Sixteen new CSDID models; no changes to the 50 km releases.
aa <- commandArgs(FALSE)
script <- normalizePath(sub("^--file=", "", aa[grepl("^--file=", aa)][1]), winslash = "/")
repo <- normalizePath(file.path(dirname(script), "../../.."), winslash = "/")
for (f in c("amws_training_helpers.R", "amws_catchment_helpers.R", "amws_catchment_no_reuse_helpers.R", "amws_training_calendar_helpers.R")) source(file.path(repo,"prep/land_grants",f))
suppressPackageStartupMessages({library(did); library(dplyr)})
root <- training_root()
training_counts <- gsub("50km", "100km", training_counts)
training_rates <- setNames(paste0(sub("^n_", "", training_counts), "_per_1000_births"), names(training_counts))
for (balance in c("relative", "calendar")) {
  started <- proc.time()[3]
  stamp <- paste0("amws_training_100km_", if (balance == "relative") "catchments" else "calendar", "_no_reuse_20261014")
  prep <- file.path(root,"output/land_grants",stamp)
  dest <- file.path(root,"results/land_grants/event_studies",stamp)
  invisible(training_check_release(prep))
  if (dir.exists(dest)) { invisible(training_check_release(dest)); next }
  stage <- file.path(Sys.getenv("USERPROFILE"),".codex/amws_checkpoints",stamp,"results_stage")
  dir.create(file.path(stage,"models"), recursive = TRUE, showWarnings = FALSE)
  input_paths <- c(script, file.path(repo,"prep/land_grants",c("amws_training_helpers.R","amws_training_calendar_helpers.R")), file.path(prep,list.files(prep,recursive = TRUE)))
  inputs <- training_hashes(input_paths)
  signature <- digest::digest(inputs, algo = "sha256")
  window <- if (balance == "relative") c(-20L,70L) else c(-50L,70L)
  models <- list(); supports <- list(); cells <- list(); diagnostics <- list(); coefficients <- list(); membership <- list()
  model_index <- 0L
  for (timing in c("standard_decade","alternative_decade")) {
    p <- fread(file.path(prep,paste0("catchment_decade_",timing,".csv")),colClasses = c(GEOID="character"))
    frozen <- fread(file.path(prep,paste0("frozen_sample_",timing,".csv")),colClasses = c(GEOID="character"))
    training_require(identical(p[,names(frozen),with=FALSE],frozen),"Frozen sample keys changed")
    training_calendar_grid <- sort(unique(p$decade))
    fwrite(frozen,file.path(stage,paste0("frozen_sample_",timing,".csv")))
    for (group in names(training_rates)) {
      model_index <- model_index+1L
      model <- paste(timing,group,sep="__")
      seed <- 20261014L + model_index
      d <- as.data.frame(p[,.(stack_unit_num,GEOID,decade,g,event_id,sample_role,y=get(training_rates[[group]]),x=births_baseline)])
      training_require(all(is.finite(d$y)&is.finite(d$x)&d$x>0),"Invalid estimation input")
      path <- file.path(stage,"models",paste0(model,".rds"))
      m <- if (file.exists(path)) readRDS(path) else NULL
      if (is.null(m) || !identical(m$input_signature,signature)) {
        set.seed(seed); ww <- character(); tick <- proc.time()[3]
        cat("Running",balance,model,"rows",nrow(d),"\n")
        m <- tryCatch(withCallingHandlers({
          att <- did::att_gt(yname="y",tname="decade",idname="stack_unit_num",gname="g",xformla=~x,data=d,
            control_group="nevertreated",est_method="reg",base_period="universal",allow_unbalanced_panel=TRUE,cores=4)
          dyn <- did::aggte(att,type="dynamic",na.rm=TRUE,min_e=window[1],max_e=window[2])
          simple <- did::aggte(att,type="simple",na.rm=TRUE,min_e=0L,max_e=70L)
          list(ok=TRUE,att=att,dynamic=dyn,simple=simple,renormalized=training_renormalize(dyn),error=NA_character_)
        },warning=function(w){ww <<- c(ww,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)list(ok=FALSE,error=conditionMessage(e)))
        m$warnings <- ww; m$elapsed_seconds <- unname(proc.time()[3]-tick)
        m$input_signature <- signature; m$seed <- seed; m$model <- model; m$timing <- timing; m$group <- group
        m$n_rows <- nrow(d); m$n_units <- length(unique(d$stack_unit_num)); m$n_events <- length(unique(d$event_id))
        saveRDS(m,path)
      }
      models[[model]] <- m
      membership[[model]] <- as.data.table(d)[,y:=NULL][,`:=`(model=model,timing=timing,group=group)]
      if (!isTRUE(m$ok)) next
      diag <- training_calendar_covariance(m$att)
      diag$summary[,`:=`(model=model,timing=timing,group=group)]
      diag$coefficients[,`:=`(model=model,timing=timing,group=group)]
      diagnostics[[model]] <- diag$summary; coefficients[[model]] <- diag$coefficients
      saveRDS(diag,file.path(stage,"models",paste0(model,"__pretest_covariance.rds")))
      support <- training_calendar_support(m$att,d,seq(window[1],window[2],10L))
      support$support[,`:=`(model=model,timing=timing,group=group)]
      support$cells[,`:=`(model=model,timing=timing,group=group)]
      supports[[model]] <- support$support; cells[[model]] <- support$cells
    }
  }
  status <- rbindlist(lapply(models,function(m)data.table(model=m$model,timing=m$timing,group=m$group,ok=m$ok,n_rows=m$n_rows,n_units=m$n_units,n_events=m$n_events,seed=m$seed,elapsed_seconds=m$elapsed_seconds,warning_count=length(m$warnings),error=m$error)))
  fwrite(status,file.path(stage,"model_status.csv"))
  warnings <- rbindlist(lapply(models,function(m) if(length(m$warnings))data.table(model=m$model,message=m$warnings)))
  fwrite(warnings,file.path(stage,"warnings.csv"))
  good <- Filter(function(m)isTRUE(m$ok),models)
  training_require(length(good)>0,"Every estimator failed; model_status retained locally")
  dyn <- rbindlist(lapply(good,function(m)copy(m$renormalized)[,`:=`(model=m$model,timing=m$timing,group=m$group)]))
  training_require(all(is.finite(dyn$estimate)&is.finite(dyn$se)&dyn$se>=0),"Nonfinite dynamic estimates")
  fwrite(dyn,file.path(stage,"dynamic_att.csv"))
  simple <- rbindlist(lapply(good,function(m)data.table(model=m$model,timing=m$timing,group=m$group,estimate=m$simple$overall.att,se=m$simple$overall.se,ci_low=m$simple$overall.att-1.96*m$simple$overall.se,ci_high=m$simple$overall.att+1.96*m$simple$overall.se,reference="Default pre-treatment base; not normalized to e=0")))
  fwrite(simple,file.path(stage,"post_treatment_att.csv"))
  fwrite(rbindlist(membership),file.path(stage,"estimation_sample_membership.csv"))
  fwrite(rbindlist(supports),file.path(stage,"event_time_support.csv"))
  fwrite(rbindlist(cells),file.path(stage,"cohort_event_time_support.csv"))
  fwrite(rbindlist(diagnostics),file.path(stage,"pretrend_test_diagnostics.csv"))
  fwrite(rbindlist(coefficients),file.path(stage,"pretrend_coefficients.csv"))
  decomp <- merge(dyn[group!="total",.(components=.N,sum_component_effects=sum(estimate)),by=.(timing,event_time)],dyn[group=="total",.(timing,event_time,benchmark_effect=estimate)],by=c("timing","event_time"),all=TRUE)
  decomp[,difference:=sum_component_effects-benchmark_effect]
  decomp[,passed:=components==3L & is.finite(difference)&abs(difference)<=1e-8*pmax(1,abs(benchmark_effect))]
  fwrite(decomp,file.path(stage,"effect_decomposition_check.csv"))
  training_require(all(status$ok)&&all(decomp$passed),"Estimation/decomposition failure; status retained locally, publication blocked")
  old <- file.path(root,"results/land_grants/event_studies",sub("100km","50km",stamp),"dynamic_att.csv")
  change <- merge(fread(old),dyn,by=c("timing","group","event_time"),suffixes=c("_50km","_100km"),all=TRUE)
  fwrite(change,file.path(stage,"radius_dynamic_comparison.csv"))
  validation <- list(passed=TRUE,successful_models=sum(status$ok),models_requested=8L,exact_frozen_samples=TRUE,all_outcomes_same_sample=TRUE,effect_decomposition=TRUE,maximum_decomposition_error=max(abs(decomp$difference)),no_summed_standard_errors=TRUE)
  training_json(validation,file.path(stage,"validation_report.json"))
  training_json(list(radius_m=100000,balance=balance,base_seed=20261014,est_method="reg",control_group="nevertreated",base_period="universal",xformla="~ catchment births at g-10",window=window,reference_event_time=0,pointwise_confidence=.95,inference="Existing did defaults; influence-function e=0 normalization",historical_campuses_verified=FALSE),file.path(stage,"model_specification.json"))
  training_json(list(elapsed_seconds=unname(proc.time()[3]-started)),file.path(stage,"timing_report.json"))
  capture.output(sessionInfo(),file=file.path(stage,"sessionInfo.txt"))
  training_publish(stage,dest,inputs,list(status="complete",validation=validation))
  print(status)
}
