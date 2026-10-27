# Vectorized, installed stringdist scorer. All generated files stay in the new
# geolocation output directory; GTAllocation and education inputs are read-only.
args <- commandArgs(TRUE); stopifnot(length(args)==1L)
suppressPackageStartupMessages({library(data.table);library(stringdist)})
root <- args[1]
# Revision scoring uses the same scores and distinct-ID reduction in bounded
# CSV chunks. The original pilot path retains its original execution mode.
if (basename(root)=="revision_v2") {
  suppressPackageStartupMessages(library(readr))
  accumulated <- NULL
  scored <- 0L
  callback <- SideEffectChunkCallback$new(function(chunk,pos) {
    d <- as.data.table(chunk)
    stopifnot(!anyNA(d$query),!anyNA(d$candidate))
    d[,distance:=stringdist(query,candidate,method='dl',nthread=4L)]
    d[,distance_sorted:=stringdist(query_sorted,candidate_sorted,method='dl',nthread=4L)]
    d[,score:=100*(1-pmin(distance,distance_sorted)/pmax(nchar(query,type='chars'),nchar(candidate,type='chars'),1L))]
    setorder(d,name_id,id,-score,-automatic,alias,source)
    best <- unique(d[,.(name_id,id,alias,source,automatic,reviewed,score)],by=c('name_id','id'))
    if (!is.null(accumulated)) best <- rbindlist(list(accumulated,best))
    setorder(best,name_id,id,-score,-automatic,alias,source)
    best <- unique(best,by=c('name_id','id'))
    setorder(best,name_id,-score,-automatic,id)
    accumulated <<- best[,head(.SD,5),by=name_id]
    scored <<- scored+nrow(d)
  })
  invisible(read_csv_chunked(file.path(root,'candidate_pairs.csv'),callback,
    chunk_size=100000L,col_types=cols(.default=col_character(),automatic=col_logical(),reviewed=col_logical()),
    locale=locale(encoding='UTF-8'),progress=FALSE))
  fwrite(accumulated,file.path(root,'candidate_scores.csv'),bom=TRUE)
  cat('Scored',scored,'aliases; retained',nrow(accumulated),'distinct-entity candidates (bounded chunks)\n')
  quit(status=0L)
}
d <- fread(file.path(root,'candidate_pairs.csv'),encoding='UTF-8')
stopifnot(!anyNA(d$query),!anyNA(d$candidate))
d[,distance:=stringdist(query,candidate,method='dl',nthread=4L)]
d[,distance_sorted:=stringdist(query_sorted,candidate_sorted,method='dl',nthread=4L)]
d[,score:=100*(1-pmin(distance,distance_sorted)/pmax(nchar(query,type='chars'),nchar(candidate,type='chars'),1L))]
setorder(d,name_id,id,-score,-automatic,alias,source)
best <- unique(d[,.(name_id,id,alias,source,automatic,reviewed,score)],by=c('name_id','id'))
setorder(best,name_id,-score,-automatic,id)
best <- best[,head(.SD,5),by=name_id]
fwrite(best,file.path(root,'candidate_scores.csv'),bom=TRUE)
cat('Scored',nrow(d),'aliases; retained',nrow(best),'distinct-entity candidates\n')
