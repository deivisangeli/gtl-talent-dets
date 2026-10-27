# Estimate every county fit required by the prepared joint state assignments.
suppressPackageStartupMessages(library(data.table));setDTthreads(1)
arg <- commandArgs(FALSE); script <- sub("^--file=","",arg[grepl("^--file=",arg)])
repo_root <- if(length(script)) normalizePath(file.path(dirname(script),"../.."),winslash="/") else Sys.getenv("GTL_REPO",getwd())
source(file.path(repo_root,"paths.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/annual_csdid_helpers.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/annual_synth_matching_helpers.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/state_placebo_helpers.R"))
parent <- file.path(TALENT_DETS_DATA_DIR,"results/mandatory_schooling")
root <- file.path(parent,"synth_state_placebos")
input <- read_annual_inputs(TALENT_DETS_DATA_DIR)
profiles <- strsplit(Sys.getenv("MS_STATE_PROFILES","pre20,pre40"),",",fixed=TRUE)[[1]]
outcomes <- strsplit(Sys.getenv("MS_STATE_OUTCOMES",paste(names(annual_outcomes),collapse=",")),",",fixed=TRUE)[[1]]
stopifnot(all(profiles %in% names(annual_profiles)),all(outcomes %in% names(annual_outcomes)))
for(pr in profiles) for(oc in outcomes) {
  preparation_path <- file.path(root,pr,"preparation_manifest.csv")
  input_manifest <- fread(preparation_path)
  live <- unname(tools::md5sum(input_manifest$file))
  preparer <- grepl("/prepare_compulsory_schooling_state_placebos[.]R$",input_manifest$file)
  live[preparer] <- unname(tools::md5sum(file.path(root,pr,"preparation_source.R")))
  stopifnot(identical(live,input_manifest$md5))
  dest <- file.path(root,pr,oc);dir.create(file.path(dest,"fits"),recursive=TRUE,showWarnings=FALSE)
  assignments <- fread(file.path(root,pr,"assignments.csv"))
  contexts <- fread(file.path(root,pr,"matching_contexts.csv"))[outcome==oc]
  selected_cohorts <- Sys.getenv("MS_STATE_COHORTS","")
  suffix <- ""
  if(nzchar(selected_cohorts)) {
    cohort_subset <- sort(unique(as.integer(strsplit(selected_cohorts,",",fixed=TRUE)[[1]])))
    stopifnot(all(cohort_subset %in% contexts$g))
    contexts <- contexts[g %in% cohort_subset]
    suffix <- paste0("_g",paste(cohort_subset,collapse="-"))
  }
  setorder(contexts,g,excluded_states,specification)
  features <- fread(file.path(parent,"synth_matching_attributes_only",pr,oc,"cohort_features.csv"))
  observed_manifest <- fread(file.path(root,pr,"observed_input_manifest.csv"))
  stopifnot(identical(unname(tools::md5sum(observed_manifest$file)),observed_manifest$md5))
  engine <- file.path(repo_root,"analysis/mandatory_schooling/estimate_compulsory_schooling_state_placebos.R")
  snapshot <- file.path(dest,"estimator_source.R")
  if(!file.exists(snapshot) || !identical(unname(tools::md5sum(snapshot)),unname(tools::md5sum(engine))))
    stopifnot(file.copy(engine,snapshot,overwrite=TRUE))
  source_files <- c(preparation_path,file.path(root,pr,"observed_input_manifest.csv"),
    file.path(root,pr,"assignments.csv"),snapshot)
  signature <- tools::md5sum(source_files)
  fwrite(data.table(file=names(signature),md5=unname(signature)),file.path(dest,"fit_manifest.csv"))
  matrices <- new.env(parent=emptyenv()); fitted <- new.env(parent=emptyenv())
  audit <- list(); fit_count <- 0L; reused <- 0L
  for(ci in seq_len(nrow(contexts))) {
    row <- contexts[ci]; gg <- row$g; ex <- row$excluded_states; context_id <- row$context_id
    match_path <- file.path(root,pr,"matching",paste0(context_id,".rds"))
    m <- readRDS(match_path)
    expected_signature <- c(setNames(input_manifest$md5,input_manifest$file),
      setNames(observed_manifest$md5,observed_manifest$file))
    stopifnot(identical(m$signature,expected_signature))
    needed_states <- unique(assignments[g==gg & excluded_states==ex,placebo_state])
    targets <- m$status[state_id %in% needed_states]
    target_ids <- targets[status=="matched",id]
    path <- file.path(dest,"fits",paste0(context_id,".rds"))
    record <- if(file.exists(path)) tryCatch(readRDS(path),error=function(e) NULL) else NULL
    match_md5 <- unname(tools::md5sum(match_path))
    if(!exists(as.character(gg),matrices,inherits=FALSE)) {
      years <- seq.int(gg-annual_profiles[[pr]]$pre,gg+20L)
      ids <- features[focal_g==gg,id]
      wide <- dcast(input$panel[id %in% ids & year %in% years],id~year,value.var=oc)
      Y <- as.matrix(wide[,-1L]);rownames(Y)<-as.character(wide$id)
      assign(as.character(gg),list(Y=Y,years=years,clean=years[years<=gg-15L]),matrices)
    }
    cohort <- get(as.character(gg),matrices)
    if(is.null(record) || !identical(record$signature,signature) || !identical(record$matching_md5,match_md5)) {
      records <- list()
      for(id in target_ids) {
        selected <- m$matches[[as.character(id)]]$selected
        fit_key <- digest::digest(list(gg,id,selected$id),algo="xxhash64")
        if(exists(fit_key,fitted,inherits=FALSE)) {
          fit <- get(fit_key,fitted)
          stopifnot(identical(fit$weights$id,selected$id));reused <- reused+1L
        } else {
          # A numerical error aborts; no outcome-dependent rejection/redraw.
          fit <- ms_fit(cohort,id,selected,oc)
          assign(fit_key,fit,fitted);fit_count <- fit_count+1L
        }
        stopifnot(!any(fit$weights$id %/% 1000L %in% m$excluded))
        msp_check_path(fit,cohort$Y,id,cohort$years,gg)
        records[[as.character(id)]] <- fit
      }
      record <- list(signature=signature,matching_md5=match_md5,context_id=context_id,
        g=gg,excluded=m$excluded,needed_states=needed_states,records=records)
      saveRDS(record,path)
    }
    stopifnot(setequal(names(record$records),as.character(target_ids)))
    audit[[context_id]] <- rbindlist(lapply(targets$id,function(tid) {
      z <- copy(targets[id==tid])
      fit <- record$records[[as.character(tid)]]
      if(!is.null(fit)) z <- cbind(z,fit$summary)
      z[,`:=`(context_id=context_id,g=gg,specification=row$specification,excluded_states=ex)]
      z
    }),fill=TRUE)
    message(pr,"/",oc,": context ",ci,"/",nrow(contexts),"; fitted ",fit_count,"; reused ",reused)
  }
  fwrite(rbindlist(audit,fill=TRUE),file.path(dest,paste0("county_fit_audit",suffix,".csv")))
  fwrite(data.table(profile=pr,outcome=oc,contexts=nrow(contexts),new_unique_fits=fit_count,
    reused_in_memory=reused,county_context_fits=sum(vapply(audit,function(x) sum(x$status=="matched"),integer(1))),status="completed"),
    file.path(dest,paste0("estimation_summary",suffix,".csv")))
}
