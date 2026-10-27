suppressPackageStartupMessages(library(data.table))
source("analysis/mandatory_schooling/annual_synth_matching_helpers.R")
source("analysis/mandatory_schooling/state_placebo_helpers.R")
slots <- data.table(state_id=c(10L,20L),g=c(1870L,1875L))
counts <- CJ(g=c(1870L,1875L),state_id=1:3)[,counties:=2L]
d <- msp_design(slots,counts,k=2L)
stopifnot(d$total==6)
allocations <- lapply(1:6,d$unrank)
stopifnot(uniqueN(vapply(allocations,function(z) paste(z$placebo_state,collapse=","),character(1)))==6,
  all(vapply(allocations,function(z) uniqueN(z$placebo_state)==2L,logical(1))))
set.seed(20260906); a <- sample.int(d$total,d$total)
set.seed(20260906); stopifnot(identical(a,sample.int(d$total,d$total)))
f <- data.table(id=1:80,state_id=rep(1:8,each=10),donor_eligible=TRUE)
for(v in ms_attribute_features) f[,(v):=id/100]
for(v in paste0("outcome_bin",1:3)) f[,(v):=0]
z <- msp_context(f,c(1L,2L),"attributes_only")
for(tid in z$status$id) {
  reference <- ms_match(tid,f,z$donor_ids,ms_attribute_features)
  stopifnot(isTRUE(all.equal(reference,z$matches[[as.character(tid)]],check.attributes=FALSE)))
}
stopifnot(length(z$donor_ids)==60L,all(z$status$status=="matched"),
  all(vapply(z$matches,function(m) !any(m$selected$state_id %in% c(1L,2L)),logical(1))))
f[id==1,outcome_bin1:=1]
zz <- msp_context(f,c(1L,2L),"richer")
stopifnot(grepl("constant_donor_mismatch",zz$status[id==1,status]),
  identical(z$matches[["1"]]$selected,msp_context(f,c(1L,2L),"attributes_only")$matches[["1"]]$selected))
paths <- CJ(state_id=c(10L,20L),event_time=c(-15L,0L))
paths[,`:=`(treated=state_id,synthetic=1,gap=state_id-1,gap_adjusted=ifelse(event_time==-15,0,state_id-1))]
slots[,weight:=c(.25,.75)]
pooled <- msp_pool(paths,slots,"weight")
stopifnot(pooled[event_time==0,gap_adjusted]==16.5,pooled[event_time==-15,gap_adjusted]==0)
cat("Joint assignment enumeration, reproducibility, all-state exclusion, matching support and fixed-weight pooling passed.\n")
