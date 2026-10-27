# Apply the existing OCR digit/day/year helpers to complete date tokens only.
# Missing components are never inferred; malformed or incompatible dates remain flagged.
suppressPackageStartupMessages({library(data.table);library(stringr)})
invisible(Sys.setlocale("LC_CTYPE","English_United States.utf8"))
args <- commandArgs(TRUE);repo_root <- args[1];stage <- args[2]
source(file.path(repo_root,"prep/amws/amws_1986/ocr_supplement_helpers.R"))
h <- load_amws_ocr_helpers(repo_root)
d <- as.data.frame(fread(file.path(stage,"entries.csv"),colClasses="character",na.strings=NULL))
parts <- str_match(d$birth_date,"^([A-Za-z]+)\\.?\\s*([0-9OoIiLlSsZzBb|!]{1,2})\\s*[,.]\\s*([0-9OoIiLlSsZzBb|]{2,4})$")
months <- c(jan=31,feb=29,mar=31,apr=30,may=31,jun=30,jul=31,aug=31,sep=30,oct=31,nov=30,dec=31)
changes <- list()
has_ocr_digits <- grepl("[OoIiLlSsZzBb|!]",paste(parts[,3],parts[,4]))
for (i in which(!is.na(parts[,1]) & has_ocr_digits)) {
  month <- tolower(substr(parts[i,2],1,3))
  if (!month %in% names(months)) next
  day <- h$parse_day_token(parts[i,3]);year <- h$parse_year_token(parts[i,4])
  if (is.na(day)||!nzchar(year)||year!=d$birth_year[i]) next
  if (as.integer(day)>months[[month]]) next
  leap <- as.integer(year)%%4L==0L && (as.integer(year)%%100L!=0L || as.integer(year)%%400L==0L)
  if (month=="feb"&&as.integer(day)==29L&&!leap) next
  normalized <- paste0(parts[i,2]," ",day,", ",h$normalize_ocr_digits(parts[i,4]))
  if (normalized==d$birth_date[i]) next
  changes[[length(changes)+1L]] <- list(source_key=d$source_key[i],old_date=d$birth_date[i],new_date=normalized,
    rule="existing_parse_day_token_parse_year_token_and_normalize_ocr_digits")
  d$birth_date[i] <- normalized
}
fwrite(d,file.path(stage,"entries.csv"),na="")
ledger <- file.path(stage,"date_normalization.csv")
if (length(changes)) {
  combined <- rbindlist(c(if(file.exists(ledger)) list(fread(ledger,colClasses="character")) else list(),changes),fill=TRUE)
  combined <- unique(combined,by=c("source_key","old_date","new_date"))
  fwrite(combined,ledger)
}
cat("Normalized",length(changes),"complete OCR dates using existing helpers\n")
