source("prep/mandatory_schooling/build_linked_1950_veteran_scientist.R")
con<-dbConnect(duckdb())
make_rows<-function(n,year) {
  x<-as.data.frame(setNames(rep(list(rep(0L,n)),length(vs_fields)),vs_fields))
  x$HISTID<-paste0("record",seq_len(n));x$HIK<-paste0("hik",seq_len(n));x$YEAR<-year
  x$VERSIONHIK<-"v1";x$VERSIONHIST<-1L;x$AGE<-30L;x$SEX<-1L;x$RACE<-1L;x$BPL<-13L
  x$SCHOOL<-1L;x$HIGRADED<-150L;x$HIGRADE<-15L;x$EDUC<-6L
  x$OCC1950<-200L;x$INCWAGE<-1000L;x$OCCSCORE<-25L
  x$STATEFIP<-6L;x$COUNTYICP<-10L;x$URBAN<-2L;x$FARM<-1L
  x$scientist_occ1950<-FALSE;x$LINK1940<-1L;x$LINK1950<-1L;x
}
e<-make_rows(12,1950L);e$HISTID<-paste0("E",letters[1:12]);e$HIK<-letters[1:12]
e$BIRTHYR<-c(1922L,1927L,1910L,1921L,1926L,1925L,1923L,1924L,1928L,1920L,1920L,1920L)
e$AGE<-1950L-e$BIRTHYR;e$SLWT<-c(2,3,5,7,11,13,17,19,23,29,0,31)
e$SLREC<-2L;e$VETWWII<-c(1L,2L,2L,1L,2L,2L,1L,2L,2L,0L,2L,2L)
e$EMPSTATD<-10L;e$CLASSWKRD<-22L;e$SEX[12]<-2L;e$OCC1950[8]<-595L
e$OCC1950[c(2,4,7)]<-61L;e$scientist_occ1950<-e$OCC1950 %in% c(7,61,62,63,67,68,69)
p<-make_rows(8,1940L);p$HISTID<-paste0("P",LETTERS[1:8]);p$HIK<-letters[1:8]
p$AGE<-1940L-e$BIRTHYR[1:8];p$SERIAL<-c(10,10,30,40,60,50,20,10)
p$PERNUM<-c(3,4,1,1,3,3,3,5);p$POPLOC<-c(2,2,0,0,2,9,2,2)
p$LINK1950[5]<-0L;p$INCWAGE[3]<-0L;p$OCC1950[3]<-61L;p$scientist_occ1950[3]<-TRUE
p$HIGRADED[2]<-151L
p$BPL[4]<-6L
f<-make_rows(2,1940L);f$HISTID<-c("dad10","dad20");f$HIK<-c("father10","father20")
f$SERIAL<-c(10L,20L);f$PERNUM<-2L;f$HIGRADED<-c(160L,190L);f$INCWAGE<-c(0L,999998L)
f$BPL<-c(99L,453L);f$OCC1950<-c(999L,970L);f$OCCSCORE<-c(0L,12L)
full<-data.frame(YEAR=1940L,HISTID=paste0("  ",tolower(p$HISTID)," "),SERIAL=p$SERIAL+1000L,
  AGE=p$AGE,RACE=p$RACE,BPL=p$BPL,FBPL=c(99,99,457,453,99,99,453,99))
full$AGE[3]<-full$AGE[3]+1L;full$BPL[1]<-14L
hh<-unique(data.frame(YEAR=1940L,SERIAL=full$SERIAL,OWNERSHP=1L))
dbWriteTable(con,"sl",rbind(e,transform(p,BIRTHYR=1940L-p$AGE,SLWT=NA_real_,SLREC=NA_integer_,VETWWII=NA_integer_,EMPSTATD=NA_integer_,CLASSWKRD=NA_integer_)))
dbWriteTable(con,"broad1940",rbind(p,f));dbWriteTable(con,"fullperson",full);dbWriteTable(con,"households",hh)
vs_prepare(con)
d<-dbGetQuery(con,"SELECT * FROM analysis_data ORDER BY respondent_id")
stopifnot(nrow(d)==8,anyDuplicated(d$respondent_id)==0,sum(d$linked_1940)==7,sum(d$father_available)==4,
  sum(d$regression_eligible)==4,all(d$regression_eligible[1:4]),!any(d$regression_eligible[5:8]),
  identical(d$father1940_HISTID[1:2],c("dad10","dad10")),all(d$home_owned_1940[1:4]==1),
  d$y1940_SERIAL[1]==10,d$fc1940_SERIAL[1]==1010,d$y1940_BPL[1]==13,d$fc1940_BPL[1]==14,
  d$own_log_wage[3]==0,d$own_zero_wage[3]==1,d$father_log_wage[1]==0,d$father_zero_wage[1]==1,
  is.na(d$father_log_wage[7]),is.na(d$father_zero_wage[7]),d$school_years_1940[2]==12,
  d$college_completed_1940[2]==0,d$father_school_years[1]==13,d$father_college_completed[1]==0,
  d$strict_link[1],!d$strict_link[4],
  d$paper_group[1]=="US-W",d$paper_group[3]=="SE",d$paper_group[4]=="NW",
  is.na(d$y1940_HISTID[5]),grepl("no_reciprocal",d$exclusion_reason[5]),grepl("no_usable_father",d$exclusion_reason[6]),
  grepl("missing_father_control",d$exclusion_reason[7]),grepl("active_military",d$exclusion_reason[8]),
  d$scientist_1950[2]==1,d$scientist_1940[3]==1,d$weight[1]==2)
counts<-vs_counts(con)
stopifnot(counts$eligible_1950[counts$cohort=="1921-1927"]==7,counts$eligible_1950[counts$cohort=="1910-1921"]==2)
grades<-dbGetQuery(con,paste0("SELECT ",vs_school_sql("grade")," AS years FROM (VALUES (0),(999),(10),(150),(151),(190),(200)) t(grade)"))$years
stopifnot(identical(grades,c(NA_integer_,NA_integer_,0L,12L,12L,16L,17L)))
plan<-dbGetQuery(con,"EXPLAIN SELECT e.HISTID,p.HISTID FROM eligible e LEFT JOIN prewar p ON CASE WHEN e.LINK1940=1 THEN e.HIK END=p.HIK")$explain_value
stopifnot(grepl("HASH_JOIN",plan),!grepl("BLOCKWISE_NL_JOIN",plan))
dbExecute(con,"INSERT INTO fathers SELECT * FROM fathers LIMIT 1")
stopifnot(inherits(try(vs_unique(con,"fathers",c("SERIAL","PERNUM")),silent=TRUE),"try-error"))
dbDisconnect(con,shutdown=TRUE)
cat("Veteran-scientist dataset fixture passed\n")
