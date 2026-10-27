# Read original field columns; join by original lineid, never row position.
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(2,3))
root <- args[1]; out <- args[2]
suppressPackageStartupMessages({library(haven);library(readxl);library(readr);library(jsonlite)})
m <- fromJSON(file.path(out,"sample_manifest.json"))
people <- read_csv(file.path(root,"output/amws/amws_education_postsecondary_v06_20261021/amws_open_alex.csv"),
                   col_types=cols_only(source_key=col_character()),show_col_types=FALSE)
all_records <- list(); inventory <- list()
for(ed in c(1906,1938,1955)) {
  source <- file.path(root,"input/amws",paste0("amws_",ed,if(ed==1906) ".xlsx" else ".dta"))
  if(ed==1906) {
    d <- read_excel(source,col_names=FALSE,.name_repair="minimal")
    ids <- as.integer(d[[1]]); names <- d[[2]]; fields <- d[[4]]; column <- "workbook_column_4"
  } else {
    d <- read_dta(source); ids <- as.integer(d$lineid)
    names <- if(ed==1938) d$AMSname else paste(d$last,d$first,sep=", ")
    fields <- if(ed==1938) d$field_legacy else d$specialization
    column <- if(ed==1938) "field_legacy" else "specialization"
  }
  fields <- as.character(fields); fields[is.na(fields)] <- ""
  names <- as.character(names); names[is.na(names)] <- ""
  # Match the existing ingestion's lossless fallback for legacy Stata strings.
  decode <- function(x) { valid <- !is.na(iconv(x,from="UTF-8",to="UTF-8",sub=NA)); x[!valid] <- iconv(x[!valid],from="latin1",to="UTF-8",sub="byte"); x }
  fields <- decode(fields); names <- decode(names)
  keys <- paste0(ed,":",ids); stopifnot(!anyDuplicated(keys))
  retained <- people$source_key[startsWith(people$source_key,paste0(ed,":"))]
  selected <- m$sampled_keys_in_order[startsWith(m$sampled_keys_in_order,paste0(ed,":"))]
  loc <- match(retained,keys); stopifnot(!anyNA(loc))
  inventory[[as.character(ed)]] <- list(raw_rows=length(keys),raw_fields_nonblank=sum(nzchar(trimws(fields))),
    retained_rows=length(retained),retained_fields_nonblank=sum(nzchar(trimws(fields[loc]))),
    source_file=source,source_column=column,unique_source_keys=TRUE,retained_joins=length(loc))
  for(k in selected) {
    i <- match(k,keys); stopifnot(!is.na(i))
    all_records[[length(all_records)+1]] <- list(source_key=k,lineid=ids[i],name_raw=names[i],
      field_raw=fields[i],source_file=source,source_column=column)
  }
}
if(length(args)==3 && args[3]=="verify") {
  expected <- fromJSON(file.path(out,"early_source_records.json"),simplifyVector=FALSE)
  stopifnot(length(expected)==length(all_records))
  for(i in seq_along(expected)) for(k in names(expected[[i]])) {
    stopifnot(identical(as.character(expected[[i]][[k]]),as.character(all_records[[i]][[k]])))
  }
  expected_inventory <- fromJSON(file.path(out,"source_inventory.json"),simplifyVector=FALSE)
  for(ed in names(inventory)) for(k in names(inventory[[ed]])) {
    stopifnot(identical(as.character(expected_inventory[[ed]][[k]]),as.character(inventory[[ed]][[k]])))
  }
  cat("Original source reread verified: all 150 sampled fields and names; population inventory matches.\n")
} else {
  write_json(all_records,file.path(out,"early_source_records.json"),auto_unbox=TRUE,pretty=TRUE)
  write_json(inventory,file.path(out,"source_inventory.json"),auto_unbox=TRUE,pretty=TRUE)
  cat("150 sampled earlier-edition fields joined; all retained-source coverage checked.\n")
}
