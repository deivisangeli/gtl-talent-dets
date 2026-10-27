# County-target training outcomes. Birthplace never determines the target county.
destination_stamp <- "amws_county_training_location_20261014"
destination_counts <- c(ever_nearby="n_amws_ever_trained_nearby", never_nearby="n_amws_never_trained_nearby",
  unknown="n_amws_training_unknown", total="n_amws_training_total")
destination_rates <- setNames(paste0(sub("^n_","",destination_counts),"_per_1000_births"),names(destination_counts))
destination_class <- function(minimum_distance, complete_training, radius_m) {
  ifelse(is.finite(minimum_distance)&minimum_distance<=radius_m,1L,ifelse(complete_training,2L,3L))
}
destination_partition <- function(x) {
  training_require(all(rowSums(x[,unname(destination_counts[1:3]),with=FALSE])==x[[destination_counts['total']]]),"Category count partition failed")
  if(all(unname(destination_rates)%in%names(x))) training_require(isTRUE(all.equal(rowSums(x[,unname(destination_rates[1:3]),with=FALSE]),x[[destination_rates['total']]],tolerance=1e-12,check.attributes=FALSE)),"Category rate partition failed")
}
destination_tests <- function() {
  stopifnot(identical(destination_class(c(50000,50000.001,Inf,49999,60000,100000),c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE),50000),c(1L,2L,3L,1L,3L,2L)))
  stopifnot(destination_class(100000,TRUE,100000)==1L,
    destination_class(100000.001,TRUE,100000)==2L,
    destination_class(min(c(120000,49999)),FALSE,50000)==1L)
  # Origin and graduation date are absent from the classifier by design.
  origin_fixture<-data.table(birth_county=c(NA,"99999","01001"),minimum_distance=c(10,10,10),complete=FALSE)
  stopifnot(all(destination_class(origin_fixture$minimum_distance,origin_fixture$complete,50000)==1L))
  cat("Destination tests passed: inclusive boundaries, multiple institutions, partial matching, no training, missing/distant birthplace and unconditional undated locations.\n")
}
