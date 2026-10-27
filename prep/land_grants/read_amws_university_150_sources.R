# Preserve every source Stata field, including 1955 rest, for the selected IDs.
args <- commandArgs(TRUE)
suppressPackageStartupMessages({library(haven); library(readr); library(jsonlite)})
ids <- read_csv(args[2], col_types=cols(.default=col_character()))
decode <- function(x) {
 x <- as.character(x); x[is.na(x)] <- ""
 valid <- !is.na(iconv(x,from="UTF-8",to="UTF-8",sub=NA))
 x[!valid] <- iconv(x[!valid],from="latin1",to="UTF-8",sub="byte")
 x
}
out <- list()
for(ed in c(1938,1955)) {
 d <- read_dta(file.path(args[1],"input","amws",paste0("amws_",ed,".dta")))
 key <- paste0(ed,":",as.integer(d$lineid)); stopifnot(!anyDuplicated(key))
 take <- which(key %in% ids$source_key)
 z <- as.data.frame(lapply(d[take,],decode),stringsAsFactors=FALSE)
 out[[as.character(ed)]] <- data.frame(source_key=key[take],
  rest_text=if(ed==1955)z$rest else rep("",nrow(z)),
  original_fields_json=vapply(seq_len(nrow(z)),function(i)as.character(toJSON(as.list(z[i,]),auto_unbox=TRUE)),character(1)))
}
write_csv(do.call(rbind,out),args[3],na="")
