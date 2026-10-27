# Read only the sampled original Stata entries, retaining original text fields.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3)
suppressPackageStartupMessages({library(haven); library(readr)})
decode <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  valid <- !is.na(iconv(x, from="UTF-8", to="UTF-8", sub=NA))
  x[!valid] <- iconv(x[!valid], from="latin1", to="UTF-8", sub="byte")
  x
}
ids <- read_csv(args[2], col_types=cols(.default=col_character()))
out <- list()
for (ed in c(1938,1955)) {
  d <- read_dta(file.path(args[1],"input","amws",paste0("amws_",ed,".dta")))
  key <- paste0(ed,":",as.integer(d$lineid))
  stopifnot(!anyDuplicated(key))
  take <- which(key %in% ids$source_key)
  z <- d[take,]
  out[[as.character(ed)]] <- data.frame(source_key=key[take],
    raw_name=decode(if(ed==1938) z$AMSname else paste(z$last,z$first,sep=", ")),
    address_text=decode(if(ed==1938) z$address_legacy else z$address),
    field_text=decode(if(ed==1938) z$field_legacy else z$specialization),
    birthplace_text=decode(if(ed==1938) rep("",nrow(z)) else z$birthplace),
    biography_text=decode(if(ed==1938) z$a else z$original),
    source_columns=if(ed==1938) "AMSname; address_legacy; field_legacy; a" else "last; first; address; specialization; birthplace; original",
    encoding_note="UTF-8 strings retained; invalid UTF-8 decoded as Latin-1, without spelling or whitespace fixes")
}
write_csv(do.call(rbind,out),args[3],na="")
