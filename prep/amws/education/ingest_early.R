# Lossless source-file preservation; decoded text is kept with an encoding note.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2)
root <- args[1]; dest <- args[2]
dir.create(dest, recursive=TRUE, showWarnings=FALSE)
suppressPackageStartupMessages({library(haven); library(readxl); library(readr)})
decode <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  valid <- !is.na(iconv(x, from="UTF-8", to="UTF-8", sub=NA))
  out <- x
  out[!valid] <- iconv(x[!valid], from="latin1", to="UTF-8", sub="byte")
  list(text=out, note=ifelse(valid, "UTF-8 as read", "invalid UTF-8 decoded as latin1"))
}
for (ed in c(1906,1938,1955)) {
  path <- file.path(root,"input","amws",paste0("amws_",ed,if(ed==1906) ".xlsx" else ".dta"))
  if(ed==1906) {
    d <- read_excel(path, col_names=FALSE, .name_repair="minimal")
    lineid <- as.integer(d[[1]]); nm <- d[[2]]; tx <- d[[6]]
  } else {
    d <- read_dta(path); lineid <- as.integer(d$lineid)
    nm <- if(ed==1938) d$AMSname else paste(d$last,d$first,sep=", ")
    tx <- if(ed==1938) d$a else d$original
  }
  cl <- read_csv(file.path(root,"output","amws",if(ed==1955) "amws_1955_split_corrected.csv" else paste0("amws_",ed,"_cleaned_corrected.csv")), show_col_types=FALSE)
  stopifnot(!anyDuplicated(lineid), !anyDuplicated(cl$lineid))
  dd <- decode(tx); names <- decode(nm)
  out <- data.frame(edition=ed, source_key=paste(ed,lineid,sep=":"),lineid=lineid,
                    name_raw=names$text,birth_year=cl$birth_year[match(lineid,cl$lineid)],
                    raw_text=dd$text,raw_text_adjusted=dd$text,encoding_note=dd$note)
  write_csv(out,file.path(dest,paste0("source_",ed,".csv")),na="")
  cat(ed, nrow(out), "records;", sum(dd$note!="UTF-8 as read"), "encoding recoveries\n")
}
