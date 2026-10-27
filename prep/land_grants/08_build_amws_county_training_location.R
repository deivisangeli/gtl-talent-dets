# Build training proximity outcomes while preserving the frozen Andrews design.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(dirname(script), "amws_training_helpers.R"))
suppressPackageStartupMessages(library(sf))
cfg <- training_config(repo, training_root())
source(file.path(dirname(script), "amws_training_destination_helpers.R"))
cfg$output <- file.path(cfg$root,"output/land_grants",destination_stamp)
cfg$local <- file.path(Sys.getenv("USERPROFILE"),".codex/amws_checkpoints",destination_stamp)
setDTthreads(4)
destination_tests()
if (dir.exists(cfg$output)) {
  training_check_release(cfg$output)
  cat("Prepared release already complete and verified:", cfg$output, "\n")
  quit(status = 0)
}
started <- proc.time()[3]
stage <- file.path(cfg$local, "prep_stage")
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
shape_parts <- paste0(tools::file_path_sans_ext(cfg$shape), c(".shp", ".shx", ".dbf", ".prj", ".cpg"))
source_files <- c(cfg$persons, cfg$person_manifest, cfg$units, cfg$panels, shape_parts,
                  cfg$legacy_early, cfg$sample_helper, cfg$main_status, cfg$main_audit,
                  script, file.path(dirname(script), c("amws_training_helpers.R", "amws_training_destination_helpers.R")),
  file.path(cfg$results, paste0("frozen_sample_",names(cfg$panels),".csv")),
  file.path(cfg$root,"output/land_grants/amws_training_50km_calendar_no_reuse_20261014",paste0("companion_reproduced_sample_",names(cfg$panels),".csv")))
training_require(all(file.exists(source_files)), "A frozen input or code file is missing")
inputs <- training_hashes(unique(unname(source_files)))
pm <- jsonlite::read_json(cfg$person_manifest, simplifyVector = TRUE)
training_require(identical(training_hash(cfg$persons), pm$sha256) && pm$rows == 136489L, "Canonical AMWS input differs from manifest")
units <- fread(cfg$units, colClasses = c(GEOID = "character"))
units[, GEOID := training_geoid(GEOID)]
geoids <- sort(unique(units$GEOID))
training_require(uniqueN(units$event_id) == 57L && length(geoids) == 168L, "Frozen event pool differs")
header <- names(fread(cfg$persons, nrows = 0L))
award_prefixes <- sub("award_index$", "", grep("^(ba_bs|ma|ms|phd)_award_[0-9]+_award_index$", header, value = TRUE))
needed <- c("source_key", "birth_year", "geoid", "geo_geoid", "kept", "is_us_birth", "birth_country", "raw_text_adjusted",
            "oa_award_count", "oa_matched_award_count", "education_status")
degree_fields <- grep("^(ba_bs|ma|ms|phd)_award_[0-9]+_(award_index|raw_institution|degree_label|year|year_raw|assignment_class|conditional_ids|flags|matching_flags|fully_matched|condition_conflict|oa_id_count|institution_[0-9]+_(name|oa_id|latitude|longitude))$", header, value = TRUE)
cat("Reading canonical AMWS input and applying existing eligibility rules.\n")
d <- fread(cfg$persons, select = unique(c(needed, degree_fields)), colClasses = "character", na.strings = NULL)
training_require(nrow(d) == 136489L && !anyDuplicated(d$source_key), "Unexpected source entries")
d[, `:=`(edition = as.integer(sub(":.*$", "", source_key)), year = suppressWarnings(as.integer(birth_year)))]
d[, GEOID := training_geoid(ifelse(edition == 1986L, geo_geoid, geoid))]
d[, see_previous_contaminated := edition == 1986L & training_contaminated(raw_text_adjusted)]
d[, exclusion_reason := fcase(
  is.na(year), "missing_or_invalid_birth_year",
  year < 1830L | year > 1959L, "outside_birth_year_window",
  edition != 1986L & !training_true(kept), "earlier_not_kept",
  edition == 1986L & (!training_true(is_us_birth) | birth_country != "USA"), "not_classified_us_birth",
  
  
  see_previous_contaminated, "see_previous_contamination",
  default = "eligible")]
eligible <- d[exclusion_reason == "eligible"]
training_require(nrow(eligible) > 83007L, "Full eligible pool did not expand beyond catchments")

# One long row per reported earned award, plus separate assigned institution rows.
awards <- rbindlist(lapply(award_prefixes, function(p) {
  take <- which(training_has(eligible[[paste0(p, "award_index")]]))
  if (!length(take)) return(NULL)
  data.table(source_key = eligible$source_key[take], prefix = p,
             award_index = as.integer(eligible[[paste0(p, "award_index")]][take]),
             family = sub("_award_.*$", "", p), raw_institution = eligible[[paste0(p, "raw_institution")]][take],
             degree_label = eligible[[paste0(p, "degree_label")]][take], year = eligible[[paste0(p, "year")]][take],
             year_raw = eligible[[paste0(p, "year_raw")]][take],
             assignment_class = eligible[[paste0(p, "assignment_class")]][take],
             conditional_ids = eligible[[paste0(p, "conditional_ids")]][take],
             flags = eligible[[paste0(p, "flags")]][take], matching_flags = eligible[[paste0(p, "matching_flags")]][take],
             fully_matched = training_true(eligible[[paste0(p, "fully_matched")]][take]),
             condition_conflict = training_true(eligible[[paste0(p, "condition_conflict")]][take]),
             reported_id_count = as.integer(eligible[[paste0(p, "oa_id_count")]][take]))
}), fill = TRUE)
institutions <- rbindlist(lapply(award_prefixes, function(p) rbindlist(lapply(1:2, function(k) {
  q <- paste0(p, "institution_", k, "_")
  take <- which(training_has(eligible[[paste0(p, "award_index")]]) & training_has(eligible[[paste0(q, "oa_id")]]))
  if (!length(take)) return(NULL)
  data.table(source_key = eligible$source_key[take], award_index = as.integer(eligible[[paste0(p, "award_index")]][take]),
             institution_slot = k, openalex_id = eligible[[paste0(q, "oa_id")]][take],
             institution_name = eligible[[paste0(q, "name")]][take],
             latitude = as.numeric(eligible[[paste0(q, "latitude")]][take]),
             longitude = as.numeric(eligible[[paste0(q, "longitude")]][take]), GEOID = eligible$GEOID[take])
}), fill = TRUE)), fill = TRUE)
training_require(!anyDuplicated(institutions[, .(source_key, award_index, openalex_id)]), "Duplicate award institution")
training_require(all(is.finite(institutions$latitude) & abs(institutions$latitude) <= 90 &
                       is.finite(institutions$longitude) & abs(institutions$longitude) <= 180), "Invalid assigned institution coordinates")
reference <- unique(institutions[, .(openalex_id, institution_name, latitude, longitude)])
training_require(!anyDuplicated(reference$openalex_id), "Assigned ID has conflicting coordinates or names")
setorder(reference, openalex_id)

cat("Computing cached county centroids and great-circle distances for", nrow(reference), "institutions.\n")
sf::sf_use_s2(TRUE)
counties <- sf::st_read(cfg$shape, quiet = TRUE)
county_geom <- counties[match(geoids, counties$GEOID), ]
training_require(!anyNA(county_geom$GEOID), "Missing county geometry")
points <- sf::st_transform(sf::st_centroid(sf::st_geometry(sf::st_transform(county_geom, 5070))), 4326)
xy <- sf::st_coordinates(points)
centroids <- data.table(GEOID = geoids, county_name = county_geom$NAME, longitude = xy[, 1], latitude = xy[, 2])
county_points <- sf::st_as_sf(as.data.frame(centroids), coords = c("longitude", "latitude"), crs = 4326)
institution_points <- sf::st_as_sf(as.data.frame(reference), coords = c("longitude", "latitude"), crs = 4326)
matrix <- sf::st_distance(county_points, institution_points)
distance <- data.table(GEOID = rep(geoids, times = nrow(reference)),
                       openalex_id = rep(reference$openalex_id, each = length(geoids)),
                       distance_m = as.numeric(matrix))
training_require(all(is.finite(distance$distance_m) & distance$distance_m >= 0), "Invalid distance matrix")

# Completeness depends on the existing award assignments, not the target county.
ia <- institutions[,.(n_ids=.N,n_locations=.N),by=.(source_key,award_index)]
awards <- merge(awards,ia,by=c("source_key","award_index"),all.x=TRUE)
awards[is.na(n_ids),`:=`(n_ids=0L,n_locations=0L)]
training_require(all(awards$n_ids==awards$reported_id_count),"Award/ID count discrepancy")
awards[,complete:=fully_matched & !condition_conflict & n_ids>0L & n_locations==n_ids]
summary <- awards[,.(n_reported_awards=.N,n_complete_awards=sum(complete),n_unmatched_awards=sum(n_ids==0L),
  n_partial_awards=sum(n_ids>0L & !complete),n_conflicting_awards=sum(condition_conflict),n_raw_institutions=sum(training_has(raw_institution))),by=source_key]
person <- merge(eligible[,.(source_key,edition,year,birth_GEOID=GEOID,education_status,oa_award_count,oa_matched_award_count)],summary,by="source_key",all.x=TRUE)
for(n in c("n_reported_awards","n_complete_awards","n_unmatched_awards","n_partial_awards","n_conflicting_awards","n_raw_institutions"))set(person,which(is.na(person[[n]])),n,0L)
training_require(all(person$n_reported_awards==as.integer(person$oa_award_count)),"Reported award totals differ")
person[,complete_training:=n_reported_awards>0L & n_complete_awards==n_reported_awards & n_conflicting_awards==0L]
person[,default_unknown_reason:=fcase(n_reported_awards==0L,"no_qualifying_award",n_raw_institutions==0L,"no_institution_text",n_conflicting_awards>0L,"matching_conflict",n_unmatched_awards==n_reported_awards,"no_assigned_location",n_unmatched_awards>0L,"unmatched_award",default="partially_matched_award")]
setorder(person,source_key)
audit <- d[,.(source_key,edition,year,birth_GEOID=GEOID,kept,is_us_birth,see_previous_contaminated,exclusion_reason)]
fwrite(audit,file.path(stage,"source_eligibility_audit.csv"))
fwrite(audit[,.N,by=.(edition,exclusion_reason)],file.path(stage,"exclusion_counts.csv"))
fwrite(person,file.path(stage,"eligible_scientist_training_evidence.csv"))
fwrite(awards,file.path(stage,"award_location_evidence.csv"))
fwrite(institutions,file.path(stage,"assigned_institution_references.csv"))
fwrite(reference,file.path(stage,"institution_coordinate_lookup.csv"))
fwrite(centroids,file.path(stage,"county_centroids.csv"))
fwrite(distance,file.path(stage,"county_institution_distances.csv"))
fwrite(units,file.path(stage,"allocation_county_units.csv"))
# Build one vector of assigned reference indices per existing wide institution slot.
positions <- unique(institutions[,.(source_key,openalex_id)])
positions[,slot:=seq_len(.N),by=source_key]
slot_indices <- lapply(seq_len(max(positions$slot)),function(j){
  z<-positions[slot==j]; v<-rep(nrow(reference)+1L,nrow(person))
  v[match(z$source_key,person$source_key)]<-match(z$openalex_id,reference$openalex_id); v
})
class_matrices <- list(`50`=matrix(0L,nrow(person),length(geoids)),`100`=matrix(0L,nrow(person),length(geoids)))
annual_parts<-list();coverage_parts<-list();checkpoint<-file.path(cfg$local,"county_checkpoints")
dir.create(checkpoint,recursive=TRUE,showWarnings=FALSE)
signature<-digest::digest(inputs,algo="sha256")
for(j in seq_along(geoids)) {
  target<-geoids[j];path<-file.path(checkpoint,paste0(target,".rds"))
  saved<-if(file.exists(path))readRDS(path)else NULL
  if(is.null(saved)||!identical(saved$signature,signature)) {
    distances<-c(as.numeric(matrix[j,]),Inf)
    minimum<-Reduce(pmin,lapply(slot_indices,function(v)distances[v]))
    a<-destination_class(minimum,person$complete_training,50000)
    b<-destination_class(minimum,person$complete_training,100000)
    training_require(all(b[a==1L]==1L),"Radius monotonicity failed")
    saved<-list(signature=signature,GEOID=target,class50=a,class100=b)
    saveRDS(saved,path)
  }
  for(r in c(50L,100L)) {
    cc<-saved[[paste0("class",r)]]
    class_matrices[[as.character(r)]][,j]<-cc
    z<-person[,.(source_key,edition,year,birth_GEOID,complete_training,default_unknown_reason)]
    z[,class_code:=cc]
    annual_parts[[paste(target,r)]]<-z[,.(GEOID=target,radius_km=r,n_amws_ever_trained_nearby=sum(class_code==1L),n_amws_never_trained_nearby=sum(class_code==2L),n_amws_training_unknown=sum(class_code==3L),n_amws_training_total=.N),by=year]
    coverage_parts[[paste(target,r)]]<-z[,.(GEOID=target,radius_km=r,entries=.N,missing_birth_county=sum(is.na(birth_GEOID)),birth_outside_original_counties=sum(!is.na(birth_GEOID)&!birth_GEOID%in%geoids)),by=.(edition,birth_decade=training_decade(year,"standard_decade"),class_code,reason=ifelse(class_code==3L,default_unknown_reason,""))]
  }
  if(j%%30L==0L)cat("Classified target counties",j,"/",length(geoids),"\n")
}
training_require(all(class_matrices[['50']]%in%1:3)&all(class_matrices[['100']]%in%1:3),"Incomplete county/entry classifications")
saveRDS(list(source_keys=person$source_key,years=person$year,target_GEOIDs=geoids,class_codes=c(ever_nearby=1L,never_nearby=2L,unknown=3L),classifications=class_matrices),file.path(stage,"scientist_county_classifications.rds"))
annual<-merge(CJ(GEOID=geoids,year=1830:1959,radius_km=c(50L,100L)),rbindlist(annual_parts),by=c("GEOID","year","radius_km"),all.x=TRUE)
for(n in unname(destination_counts))set(annual,which(is.na(annual[[n]])),n,0L)
destination_partition(annual)
training_require(all(annual[,uniqueN(n_amws_training_total),by=.(year,radius_km)]$V1==1L),"Cohort pool differs across counties")
fwrite(annual,file.path(stage,"county_year_training_counts.csv"))
fwrite(person[,.N,by=.(edition,year)],file.path(stage,"full_pool_by_edition_birth_year.csv"))
fwrite(rbindlist(coverage_parts),file.path(stage,"classification_coverage_by_county_edition_cohort.csv"))
for(timing in names(cfg$panels)) {
  base<-fread(cfg$panels[[timing]],colClasses=c(GEOID="character"))
  original<-copy(base)
  for(r in c(50L,100L)) {
    tmp<-copy(annual[radius_km==r]);tmp[,decade:=training_decade(year,timing)]
    counts<-tmp[,lapply(.SD,sum),by=.(GEOID,decade),.SDcols=unname(destination_counts)]
    p<-merge(copy(base)[,original_order:=.I],counts,by=c("GEOID","decade"),all.x=TRUE,sort=FALSE)
    setorder(p,original_order);p[,original_order:=NULL]
    training_require(all(vapply(names(original),function(n)identical(p[[n]],original[[n]]),logical(1))),"Legacy panel column changed")
    p[,radius_km:=r]
    for(g in names(destination_counts))p[,(destination_rates[[g]]):=training_rate(get(destination_counts[[g]]),county_births_estimate)]
    destination_partition(p)
    fwrite(p,file.path(stage,paste0("county_decade_",r,"km_",timing,".csv")))
  }
  for(balance in c("relative","calendar")) {
    sfpath<-if(balance=="relative")file.path(cfg$results,paste0("frozen_sample_",timing,".csv"))else file.path(cfg$root,"output/land_grants/amws_training_50km_calendar_no_reuse_20261014",paste0("companion_reproduced_sample_",timing,".csv"))
    f<-fread(sfpath,colClasses=c(GEOID="character"))
    if("y"%in%names(f))f[,y:=NULL]
    if(!"site_id"%in%names(f))f[,site_id:=paste(event_id,GEOID,sample_role,sep="_")]
    expected<-if(balance=="calendar")c(15L,46L,598L)else if(timing=="standard_decade")c(16L,52L,650L)else c(20L,62L,834L)
    training_require(identical(as.integer(c(uniqueN(f$event_id),uniqueN(f$stack_unit_num),nrow(f))),expected),"Original sample dimensions differ")
    if(balance=="calendar")training_require(50L%in%f$event_id & all(f[,.N,by=stack_unit_num]$N==13L),"East Carolina/calendar coverage not restored")
    fwrite(f,file.path(stage,paste0("frozen_",balance,"_",timing,".csv")))
  }
}
validation<-list(passed=TRUE,eligible_entries=nrow(person),missing_birth_county_eligible=person[is.na(birth_GEOID),.N],outside_original_counties_eligible=person[!is.na(birth_GEOID)&!birth_GEOID%in%geoids,.N],counties=length(geoids),institutions=nrow(reference),awards=nrow(awards),all_county_entry_pairs_classified=TRUE,classification_monotonicity=TRUE,original_panel_columns_unchanged=TRUE,source_key_counting_preserved=TRUE,historical_campus_accuracy_verified=FALSE)
training_json(validation,file.path(stage,"validation_report.json"))
writeLines(c("One target county and birth cohort per observation; scientist birthplace never defines target membership.","Full eligible US-born sample; existing kept/year/see-previous rules retained. No new cross-edition linkage.","Scientist counted once per target county. Codes: ever nearby=1, never nearby=2, unknown=3.","scientist_county_classifications.rds contains source_keys, years, target_GEOIDs and integer matrices for each radius; matrix row/column order is explicit.","Ever: one assigned institution <=radius from target county centroid. Never: all reported awards complete conflict-free and all locations outside radius. Unknown: all remaining cases.","Unknown includes incomplete information; known nearby takes precedence. Graduation year is not the cohort time variable.","The same national eligible cohort is partitioned in every county, so totals are constant across target counties before normalization.","Rates =1000*category count/own-county births. Denominators and original panel columns are unchanged. No pooled catchments or shared-county exclusions.","County geometry: EPSG:5070 geometric centroids, transformed to longitude/latitude; distances S2 great-circle metres, inclusive boundary.","OpenAlex reference coordinates and existing provisional identities do not verify historical teaching campuses."),file.path(stage,"DATA_DICTIONARY.txt"))
dir.create(file.path(stage,"code_snapshot"),showWarnings=FALSE)
for(p in c(script,file.path(dirname(script),c("amws_training_helpers.R","amws_training_destination_helpers.R"))))file.copy(p,file.path(stage,"code_snapshot",basename(p)),overwrite=TRUE)
training_json(list(elapsed_seconds=unname(proc.time()[3]-started)),file.path(stage,"timing_report.json"))
training_publish(stage,cfg$output,inputs,list(status="complete",validation=validation))
print(validation)
