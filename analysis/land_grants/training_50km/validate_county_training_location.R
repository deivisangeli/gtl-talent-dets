# Independent sparse-incidence reconstruction of every scientist/county category.
aa<-commandArgs(FALSE);script<-normalizePath(sub("^--file=","",aa[grepl("^--file=",aa)][1]),winslash="/")
repo<-normalizePath(file.path(dirname(script),"../../.."),winslash="/")
for(f in c("amws_training_helpers.R","amws_training_destination_helpers.R"))source(file.path(repo,"prep/land_grants",f))
suppressPackageStartupMessages(library(Matrix));setDTthreads(4)
root<-training_root();prep<-file.path(root,"output/land_grants",destination_stamp);res<-file.path(root,"results/land_grants/event_studies",destination_stamp)
invisible(training_check_release(prep));invisible(training_check_release(res));destination_tests()
person<-fread(file.path(prep,"eligible_scientist_training_evidence.csv"),colClasses=c(birth_GEOID="character"))
refs<-fread(file.path(prep,"institution_coordinate_lookup.csv"))
ins<-unique(fread(file.path(prep,"assigned_institution_references.csv"))[,.(source_key,openalex_id)])
distance<-fread(file.path(prep,"county_institution_distances.csv"),colClasses=c(GEOID="character"))
classes<-readRDS(file.path(prep,"scientist_county_classifications.rds"))
stopifnot(identical(classes$source_keys,person$source_key),!anyDuplicated(person$source_key),all(distance$distance_m>=0 & is.finite(distance$distance_m)))
A<-Matrix::sparseMatrix(i=match(ins$source_key,person$source_key),j=match(ins$openalex_id,refs$openalex_id),x=1L,dims=c(nrow(person),nrow(refs)))
annual<-fread(file.path(prep,"county_year_training_counts.csv"),colClasses=c(GEOID="character"))
all_tests<-list();count_checks<-list()
for(j in seq_along(classes$target_GEOIDs)) {
  gid<-classes$target_GEOIDs[j]
  dist<-distance[GEOID==gid][match(refs$openalex_id,openalex_id)]$distance_m
  stopifnot(length(dist)==nrow(refs),!anyNA(dist))
  for(radius in c(50L,100L)) {
    nearby<-as.numeric(A%*%as.integer(dist<=radius*1000))>0L
    expected<-rep(3L,nrow(person));expected[person$complete_training]<-2L;expected[nearby]<-1L
    observed<-classes$classifications[[as.character(radius)]][,j]
    stopifnot(identical(expected,observed))
    totals<-data.table(year=person$year,code=expected)[,.(e=sum(code==1L),n=sum(code==2L),u=sum(code==3L),total=.N),by=year]
    check<-merge(annual[GEOID==gid & radius_km==radius],totals,by="year",all.x=TRUE)
    check[is.na(total),`:=`(e=0L,n=0L,u=0L,total=0L)]
    stopifnot(all(check$e==check$n_amws_ever_trained_nearby),all(check$n==check$n_amws_never_trained_nearby),all(check$u==check$n_amws_training_unknown),all(check$total==check$n_amws_training_total))
  }
}
stopifnot(all(classes$classifications[['100']][classes$classifications[['50']]==1L]==1L))
cfg<-training_config(repo,root)
for(timing in names(cfg$panels)) {
  orig<-fread(cfg$panels[[timing]],colClasses=c(GEOID="character"))
  p50<-fread(file.path(prep,paste0("county_decade_50km_",timing,".csv")),colClasses=c(GEOID="character"))
  p100<-fread(file.path(prep,paste0("county_decade_100km_",timing,".csv")),colClasses=c(GEOID="character"))
  for(p in list(p50,p100)) {
    stopifnot(nrow(p)==nrow(orig),all(vapply(names(orig),function(n)isTRUE(all.equal(p[[n]],orig[[n]],tolerance=1e-12)),logical(1))))
    destination_partition(p)
    aa<-copy(annual[radius_km==p$radius_km[1]]);aa[,decade:=training_decade(year,timing)]
    ag<-aa[,lapply(.SD,sum),by=.(GEOID,decade),.SDcols=unname(destination_counts)]
    q<-merge(p,ag,by=c("GEOID","decade"),suffixes=c("","_reconstructed"))
    for(n in unname(destination_counts))stopifnot(all(q[[n]]==q[[paste0(n,"_reconstructed")]]))
  }
  stopifnot(identical(p50$n_amws_training_total,p100$n_amws_training_total),identical(p50$amws_training_total_per_1000_births,p100$amws_training_total_per_1000_births),all(p100$n_amws_ever_trained_nearby>=p50$n_amws_ever_trained_nearby),all(p100$n_amws_never_trained_nearby<=p50$n_amws_never_trained_nearby))
  for(balance in c("relative","calendar"))for(radius in c(50L,100L)) {
    p<-fread(file.path(res,paste0("analysis_panel_",balance,"_",radius,"km_",timing,".csv")),colClasses=c(GEOID="character"))
    f<-fread(file.path(prep,paste0("frozen_",balance,"_",timing,".csv")),colClasses=c(GEOID="character"))
    stopifnot(identical(p[,names(f),with=FALSE],f))
    if(balance=="calendar")stopifnot(all(p[,.N,by=stack_unit_num]$N==13),50L%in%p$event_id)
    for(group in names(destination_counts)) {
      wanted<-paste(balance,paste0(radius,"km"),timing,group,sep="__")
      model<-readRDS(file.path(res,"models",paste0(wanted,".rds")))
      reconstructed<-training_renormalize(model$dynamic)
      published<-fread(file.path(res,"dynamic_att.csv"))[model==wanted]
      stopifnot(isTRUE(model$ok),max(abs(reconstructed$estimate-published$estimate))<1e-10,max(abs(reconstructed$se-published$se))<1e-10)
    }
    all_tests[[paste(timing,balance,radius)]]<-data.table(timing=timing,balance=balance,radius_km=radius,events=uniqueN(p$event_id),units=uniqueN(p$stack_unit_num),observations=nrow(p),passed=TRUE)
  }
}
for(stem in c("amws_training_50km_20261014","amws_training_50km_calendar_no_reuse_20261014","amws_training_50km_catchments_no_reuse_20261014","amws_training_100km_catchments_no_reuse_20261014","amws_training_100km_calendar_no_reuse_20261014"))for(branch in c("output/land_grants","results/land_grants/event_studies"))invisible(training_check_release(file.path(root,branch,stem)))
dest<-file.path(root,"results/land_grants/companions",destination_stamp)
dir.create(dest,recursive=TRUE,showWarnings=FALSE)
fwrite(rbindlist(all_tests),file.path(dest,"independent_sample_validation.csv"))
validation<-list(passed=TRUE,eligible_entries=nrow(person),target_counties=length(classes$target_GEOIDs),independently_validated_classifications=nrow(person)*length(classes$target_GEOIDs)*2L,sparse_incidence_reconstruction=TRUE,annual_and_decade_reconstruction=TRUE,original_columns_denominators_and_samples_preserved=TRUE,monotonicity=TRUE,models_and_influence_functions_reconstructed=TRUE,previous_releases_verified_unchanged=TRUE)
training_json(validation,file.path(dest,"independent_validation.json"));print(validation)
