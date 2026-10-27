suppressPackageStartupMessages(library(data.table));setDTthreads(1)
arg <- commandArgs(FALSE); script <- sub("^--file=","",arg[grepl("^--file=",arg)])
repo_root <- if(length(script)) normalizePath(file.path(dirname(script),"../.."),winslash="/") else Sys.getenv("GTL_REPO",getwd())
source(file.path(repo_root,"paths.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/annual_csdid_helpers.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/annual_synth_matching_helpers.R"))
source(file.path(repo_root,"analysis/mandatory_schooling/state_placebo_helpers.R"))
parent <- file.path(TALENT_DETS_DATA_DIR,"results/mandatory_schooling")
root <- file.path(parent,"synth_state_placebos")
dir.create(root,recursive=TRUE,showWarnings=FALSE)
profiles <- strsplit(Sys.getenv("MS_STATE_PROFILES","pre20,pre40"),",",fixed=TRUE)[[1]]
outcomes <- names(annual_outcomes)
requested <- as.integer(Sys.getenv("MS_STATE_DRAWS","999"))
seed <- as.integer(Sys.getenv("MS_STATE_SEED","20260906"))
stopifnot(requested>0,all(profiles %in% names(annual_profiles)))
if(requested != 999L || seed != 20260906L) stop("Production defaults are fixed: 999 draws, seed 20260906.")
base_manifest <- fread(file.path(parent,"synth_matching/input_manifest.csv"))
new_manifest <- fread(file.path(parent,"synth_matching_attributes_only/input_manifest.csv"))
data_files <- base_manifest[!grepl("[.]R$",file)]
stopifnot(all(data_files$md5 == new_manifest$md5[match(data_files$file,new_manifest$file)]),
  identical(unname(tools::md5sum(data_files$file)),data_files$md5))
source_files <- file.path(repo_root,"analysis/mandatory_schooling",c("annual_csdid_helpers.R","annual_synth_matching_helpers.R",
  "state_placebo_helpers.R","prepare_compulsory_schooling_state_placebos.R"))
signature <- tools::md5sum(c(data_files$file,source_files))
if(!file.exists(file.path(root,"input_manifest.csv")))
  fwrite(data.table(file=names(signature),md5=unname(signature)),file.path(root,"input_manifest.csv"))
fwrite(base_manifest,file.path(root,"observed_richer_manifest.csv"))
fwrite(new_manifest,file.path(root,"observed_attributes_manifest.csv"))
for(pr in profiles) {
  dest <- file.path(root,pr);dir.create(file.path(dest,"matching"),recursive=TRUE,showWarnings=FALSE)
  fwrite(data.table(file=names(signature),md5=unname(signature)),file.path(dest,"preparation_manifest.csv"))
  file.copy(file.path(repo_root,"analysis/mandatory_schooling/prepare_compulsory_schooling_state_placebos.R"),
    file.path(dest,"preparation_source.R"),overwrite=TRUE)
  features <- list(); observed <- list(); inputs <- list()
  for(oc in outcomes) {
    a <- file.path(parent,"synth_matching_attributes_only",pr,oc)
    b <- file.path(parent,"synth_matching",pr,oc)
    files <- c(file.path(a,c("cohort_features.csv","model_status.csv","event_paths.csv")),
      file.path(b,c("model_status.csv","event_paths.csv")))
    inputs[[oc]] <- data.table(file=files,md5=unname(tools::md5sum(files)))
    features[[oc]] <- fread(files[1])
    sa <- fread(files[2])[status=="estimated"]
    sr <- fread(files[4])[status=="estimated" & specification=="richer"]
    common <- intersect(sa$id,sr$id)
    st <- sa[,.(full_counties=.N),by=.(state_id,state_abbr,g)]
    st[sr[id %in% common,.(common_counties=.N),by=state_id],on="state_id",common_counties:=i.common_counties]
    st[,`:=`(weight_common=common_counties/sum(common_counties),weight_full=full_counties/sum(full_counties))]
    setorder(st,g,state_id)
    observed[[oc]] <- list(slots=st,common=common,full=sa$id)
    if(oc!=outcomes[1]) stopifnot(identical(observed[[outcomes[1]]]$slots,st))
  }
  audit_inputs <- rbindlist(inputs);fwrite(audit_inputs,file.path(dest,"observed_input_manifest.csv"))
  context_signature <- c(signature,setNames(audit_inputs$md5,audit_inputs$file))
  slots <- observed[[1]]$slots
  fwrite(slots,file.path(dest,"state_slots.csv"))
  counts <- features[[1]][donor_eligible & attributes_complete & focal_g %in% slots$g,
    .(counties=.N),by=.(g=focal_g,state_id,state_abbr)]
  design <- msp_design(slots,counts)
  stopifnot(design$total == if(pr=="pre20") 99792 else 54)
  fwrite(counts,file.path(dest,"candidate_state_coverage.csv"))
  set.seed(seed+match(pr,c("pre20","pre40"))-1L)
  ranks <- if(pr=="pre40") seq_len(design$total) else sample.int(design$total,design$total,replace=FALSE)
  desired <- min(requested,design$total)
  cache <- new.env(parent=emptyenv()); accepted <- list(); attempts <- list(); used <- list()
  worker_context <- function(job) {
    oc <- job$oc; gg <- job$gg; excluded <- job$excluded; spec <- job$spec
    key <- paste(oc,gg,paste(excluded,collapse="-"),spec,sep="_")
    path <- file.path(dest,"matching",paste0(key,".rds"))
    z <- if(file.exists(path)) tryCatch(readRDS(path),error=function(e) NULL) else NULL
    if(is.null(z) || !identical(z$signature,context_signature)) {
      z <- msp_context(features[[oc]][focal_g==gg],excluded,spec)
      z$signature <- context_signature; z$key <- key;z$g <- gg;z$outcome <- oc;z$specification <- spec
      saveRDS(z,path)
    }
    # Preparation only needs support status after the complete matching record
    # is saved. Keeping hundreds of thousands of balance tables in RAM causes
    # unnecessary memory pressure when exploring the pre20 assignment space.
    light <- z[c("key","g","outcome","specification","status")]
    light
  }
  workers <- as.integer(Sys.getenv("MS_STATE_PREP_WORKERS","4"))
  stopifnot(is.finite(workers),workers>=1L)
  cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(dest,"preparation_workers.log"))
  parallel::clusterExport(cluster,c("repo_root","features","context_signature","dest","worker_context"),envir=environment())
  parallel::clusterEvalQ(cluster,{
    suppressPackageStartupMessages(library(data.table));setDTthreads(1)
    source(file.path(repo_root,"analysis/mandatory_schooling/annual_synth_matching_helpers.R"))
    source(file.path(repo_root,"analysis/mandatory_schooling/state_placebo_helpers.R"));NULL
  })
  get_context <- function(oc,gg,excluded,spec) get(paste(oc,gg,paste(excluded,collapse="-"),spec,sep="_"),cache)
  for(rank in ranks) {
    allocation <- design$unrank(rank); excluded <- sort(allocation$placebo_state)
    jobs <- list()
    for(oc in outcomes) for(gg in unique(allocation$g)) for(spec in c("richer","attributes_only")) {
      key <- paste(oc,gg,paste(excluded,collapse="-"),spec,sep="_")
      if(!exists(key,cache,inherits=FALSE)) jobs[[key]] <- list(oc=oc,gg=gg,excluded=excluded,spec=spec)
    }
    if(length(jobs)) {
      values <- parallel::parLapply(cluster,jobs,worker_context)
      for(value in values) assign(value$key,value,cache)
    }
    ok <- TRUE; reasons <- character(); contexts <- list()
    for(oc in outcomes) for(gg in unique(allocation$g)) {
      zz <- lapply(c("richer","attributes_only"),function(spec) get_context(oc,gg,excluded,spec))
      common <- intersect(zz[[1]]$status[status=="matched",id],zz[[2]]$status[status=="matched",id])
      for(ps in allocation[g==gg,placebo_state]) {
        if(!any(zz[[1]]$status[id %in% common,state_id]==ps)) {
          ok <- FALSE; reasons <- c(reasons,paste(oc,gg,ps,"no_common_matched_county",sep=":"))
        }
      }
      for(z in zz) contexts[[z$key]] <- data.table(context_id=z$key,outcome=oc,g=gg,excluded_states=paste(excluded,collapse="-"),specification=z$specification)
    }
    attempts[[as.character(rank)]] <- data.table(rank=rank,admissible=ok,reason=paste(reasons,collapse=";"))
    if(ok) {
      allocation[,`:=`(draw_id=length(accepted)+1L,assignment_rank=rank)]
      accepted[[length(accepted)+1L]] <- allocation
      for(key in names(contexts)) used[[key]] <- contexts[[key]]
    }
    if(length(attempts) %% 10L == 0L) message(pr,": ",length(accepted)," accepted / ",length(attempts)," checked; ",length(ls(cache))," matching contexts")
    if(length(accepted)>=desired) break
  }
  parallel::stopCluster(cluster)
  stopifnot(length(accepted)>0)
  assignments <- rbindlist(accepted)
  fwrite(assignments,file.path(dest,"assignments.csv"))
  fwrite(rbindlist(attempts),file.path(dest,"assignment_audit.csv"))
  fwrite(rbindlist(used),file.path(dest,"matching_contexts.csv"))
  saveRDS(observed,file.path(dest,"observed_samples.rds"))
  fwrite(data.table(profile=pr,structural_assignments=design$total,requested=desired,
    checked=length(attempts),accepted=length(accepted),seed=seed+match(pr,c("pre20","pre40"))-1L,
    exhaustive=length(attempts)==design$total),file.path(dest,"design_summary.csv"))
  message(pr,": saved ",length(accepted)," assignments")
}
