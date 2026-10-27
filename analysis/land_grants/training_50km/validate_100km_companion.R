# Independent reconstruction checks for the new radius analyses and frozen sources.
aa <- commandArgs(FALSE)
script <- normalizePath(sub("^--file=", "", aa[grepl("^--file=", aa)][1]), winslash = "/")
repo <- normalizePath(file.path(dirname(script),"../../.."),winslash="/")
for (f in c("amws_training_helpers.R","amws_catchment_helpers.R","amws_catchment_no_reuse_helpers.R","amws_training_calendar_helpers.R")) source(file.path(repo,"prep/land_grants",f))
root <- training_root(); records <- list()
base <- file.path(root,"output/land_grants/amws_training_100km_geography_20261014")
invisible(training_check_release(base))
new_i <- fread(file.path(base,"assigned_institution_distances.csv"),colClasses=c(GEOID="character"))
old_i <- fread(file.path(root,"output/land_grants/amws_training_50km_catchments_20261014/assigned_institution_distances.csv"),colClasses=c(GEOID="character"))
ii <- merge(old_i,new_i,by=c("source_key","award_index","openalex_id"),suffixes=c("_50","_100"),all.x=TRUE)
stopifnot(nrow(ii)==nrow(old_i),!anyNA(ii$GEOID_100),all(ii$GEOID_50==ii$GEOID_100),all(ii$institution_slot_50==ii$institution_slot_100),all(ii$institution_name_50==ii$institution_name_100),all(ii$latitude_50==ii$latitude_100),all(ii$longitude_50==ii$longitude_100),max(abs(ii$distance_m_50-ii$distance_m_100))<1e-5)
# Inclusive boundary, incomplete matches, multiple awards, conflicts and missing unconditional dates.
stopifnot(training_classify(100000,1L,TRUE,radius=100000)=="ever_nearby",
  training_classify(100000.001,1L,TRUE,radius=100000)=="never_nearby",
  training_classify(c(99999,200000),2L,c(TRUE,FALSE),radius=100000)=="ever_nearby",
  training_classify(200000,2L,c(TRUE,FALSE),radius=100000)=="unknown",
  training_classify(200000,1L,TRUE,TRUE,radius=100000)=="unknown",
  training_classify(numeric(),0L,logical(),radius=100000)=="unknown")
x <- data.table(event_id=1L,GEOID=c("a","a","b","b","c"),site_id=c("s2","s1","s2","s1","s1"),home=c(FALSE,FALSE,TRUE,FALSE,FALSE),distance_m=c(100000,100000,110000,1,100000.001))
z <- catchment_assign(x,radius=100000)
stopifnot(z[GEOID=="a",site_id]=="s1",z[GEOID=="b",site_id]=="s2",!"c"%in%z$GEOID,identical(as.data.frame(z),as.data.frame(catchment_assign(x[nrow(x):1],radius=100000))))
training_counts <- gsub("50km","100km",training_counts)
training_rates <- setNames(paste0(sub("^n_","",training_counts),"_per_1000_births"),names(training_counts))
for (b in c("relative","calendar")) {
  stem <- paste0("amws_training_100km_",if(b=="relative")"catchments"else"calendar","_no_reuse_20261014")
  prep <- file.path(root,"output/land_grants",stem);res <- file.path(root,"results/land_grants/event_studies",stem)
  invisible(training_check_release(prep));invisible(training_check_release(res))
  for (which in c("output/land_grants","results/land_grants/event_studies")) invisible(training_check_release(file.path(root,which,sub("100km","50km",stem))))
  excluded <- fread(file.path(prep,"excluded_counties.csv"),colClasses=c(GEOID="character"))$GEOID
  for(t in c("standard_decade","alternative_decade")) {
    p<-fread(file.path(prep,paste0("catchment_decade_",t,".csv")),colClasses=c(GEOID="character"))
    m<-fread(file.path(prep,paste0("catchment_membership_",t,".csv")),colClasses=c(GEOID="character"))
    cp<-fread(file.path(base,paste0("county_decade_",t,".csv")),colClasses=c(GEOID="character"))
    ee<-fread(file.path(prep,paste0("analysis_entry_membership_",t,".csv")),colClasses=c(GEOID="character"))
    stopifnot(!anyDuplicated(m$GEOID),!anyDuplicated(ee$source_key),!any(m$GEOID%in%excluded),!anyDuplicated(p[,.(site_id,decade)]))
    rows<-merge(p[,.(site_id,decade)],m[,.(site_id,GEOID)],by="site_id",allow.cartesian=TRUE)
    rows<-merge(rows,cp,by=c("GEOID","decade"),all.x=TRUE)
    sums<-rows[,c(lapply(.SD,sum),list(births=sum(county_births_estimate))),by=.(site_id,decade),.SDcols=unname(training_counts)]
    check<-merge(p,sums,by=c("site_id","decade"),suffixes=c("","_reconstructed"))
    stopifnot(isTRUE(all.equal(check$births,check$county_births_estimate,tolerance=1e-12)))
    for(n in unname(training_counts)) stopifnot(all(check[[n]]==check[[paste0(n,"_reconstructed")]]))
    catchment_check_partition(p)
    if(b=="calendar")stopifnot(all(p[, .N,by=site_id]$N==13),setequal(p$decade,seq(1830,1950,10)))
    for(g in names(training_rates)) {
      model<-readRDS(file.path(res,"models",paste0(t,"__",g,".rds")))
      shifted<-training_renormalize(model$dynamic)
      published<-fread(file.path(res,"dynamic_att.csv"))[timing==t&group==g]
      setorder(published,event_time);setorder(shifted,event_time)
      stopifnot(max(abs(published$estimate-shifted$estimate))<1e-12,max(abs(published$se-shifted$se))<1e-12)
      wanted_model <- paste0(t,"__",g)
      members<-fread(file.path(res,"estimation_sample_membership.csv"))[model==wanted_model]
      stopifnot(nrow(members)==nrow(p),all(members$x==p$births_baseline))
    }
    records[[paste(b,t)]]<-data.table(balance=b,timing=t,events=uniqueN(p$event_id),units=uniqueN(p$site_id),observations=nrow(p),counties=nrow(m),entries=nrow(ee),passed=TRUE)
  }
}
dest<-file.path(root,"results/land_grants/companions/amws_training_radius_companion_20261014")
dir.create(dest,recursive=TRUE,showWarnings=FALSE)
fwrite(rbindlist(records),file.path(dest,"independent_numerical_validation.csv"))
training_json(list(passed=TRUE,protected_50km_releases_verified=TRUE,assigned_institution_rows_preserved=nrow(old_i),coordinates_assignments_and_distances_preserved=TRUE,classification_radius_boundary_tests=TRUE,geographic_nearest_ties_home_tests=TRUE,independent_pooled_count_birth_rate_reconstruction=TRUE,model_influence_function_reconstruction=TRUE),file.path(dest,"independent_validation.json"))
print(rbindlist(records))
