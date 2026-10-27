# Audit adapter: definitions come exclusively from the frozen repository.
# Neither references nor identity alignments are inputs to this program.
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
repo_root <- normalizePath(args[1], winslash="/", mustWork=TRUE)
input <- args[2]; output <- args[3]
source(file.path(repo_root,"prep/amws/amws_1986/ocr_supplement_helpers.R"))
h <- load_amws_ocr_helpers(repo_root)
blocks <- fread(input, colClasses="character", na.strings=NULL)
entries <- list(); current <- NULL
flush <- function() {
  if (!is.null(current)) entries[[length(entries)+1L]] <<- current
}
for (i in seq_len(nrow(blocks))) {
  row <- blocks[i]
  if (row$block_status != "body") next
  pieces <- h$split_safe_embedded_entries(h$normalize_text(row$text))
  for (piece_no in seq_along(pieces)) {
    text <- pieces[[piece_no]]
    adjusted <- sub("^([A-Z][A-Z'’ -]{1,65})\\.\\s+", "\\1, ", text, perl=TRUE)
    cls <- h$classify_piece(adjusted)
    if (cls$starts_entry[[1]] || is.null(current)) {
      flush()
      current <- list(source_key=paste(row$method,row$source_file,row$pdf_page,row$block_index,piece_no,sep=":"),
        method=row$method,source_file=row$source_file,pdf_page=row$pdf_page,
        first_block=row$block_index,last_block=row$block_index,
        segmentation_rule=if(cls$starts_entry[[1]]) cls$rule[[1]] else "orphan_continuation",
        raw_text_adjusted=adjusted,raw_text=text)
    } else {
      current$raw_text_adjusted <- paste(current$raw_text_adjusted,adjusted)
      current$raw_text <- paste(current$raw_text,text)
      current$last_block <- row$block_index
    }
  }
}
flush()
d <- as.data.frame(rbindlist(entries,fill=TRUE),stringsAsFactors=FALSE)
d$doc_id <- paste0("amws",sub("\\.pdf$","",d$source_file))
d$lineid <- as.character(seq_len(nrow(d))); d$entry_instance <- "1"
cat("Segmented",nrow(d),"records\n")
parsed <- dplyr::bind_rows(lapply(d$raw_text_adjusted,h$parse_entry_regex))
d <- cbind(d,as.data.frame(parsed))
d <- as.data.frame(h$add_birth_year_city(d))
d$birth_year <- h$blank_na(d$birth_year)
components <- dplyr::bind_rows(lapply(d$birth_place,function(p) h$parse_place_components(h$trim_birth_place(p)$place)))
d$birth_city <- components$birth_city; d$birth_state <- components$birth_state
d$birth_country <- components$birth_country
d$birth_location_format_problem <- "FALSE"
corrections <- h$apply_regex_corrections(d)
for (col in c("birth_place","birth_city","birth_state","birth_country","birth_year")) {
  d[[col]][corrections$row_id] <- corrections[[paste0("new_",col)]]
}
fwrite(corrections,paste0(output,".corrections.csv"),na="")
fwrite(d,output,na="")
cat("Parsed",nrow(d),"records; general corrections",nrow(corrections),"\n")
