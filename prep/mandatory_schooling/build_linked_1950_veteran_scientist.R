# One row per eligible 1950 sample-line man, with prewar and father controls.
suppressPackageStartupMessages({library(DBI); library(duckdb); library(jsonlite)})

vs_fail <- function(test, message) if (isTRUE(test)) stop(message, call. = FALSE)
vs_path <- function(x) gsub("\\\\", "/", x)
vs_data_root <- function() {
  candidates <- c(Sys.getenv("TALENT_DETS_DATA_DIR"), Sys.getenv("DET_DIR"),
    file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox/gtl_talent_dets"))
  hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
  vs_fail(!length(hit), "Set TALENT_DETS_DATA_DIR to the Dropbox project root")
  normalizePath(hit[[1]], winslash = "/")
}
vs_say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
vs_scalar <- function(con, sql) dbGetQuery(con, sql)[[1]][[1]]
vs_unique <- function(con, table, fields) {
  key <- if (length(fields) == 1) fields else paste0("(", paste(fields, collapse=","), ")")
  vs_fail(vs_scalar(con, paste0("SELECT COUNT(*)-COUNT(DISTINCT ",key,
    ") FROM ",table)) != 0, paste("Duplicate or missing key in",table))
}
vs_fields <- c("HISTID", "HIK", "SERIAL", "PERNUM", "POPLOC", "AGE", "SEX",
  "RACE", "BPL", "SCHOOL", "HIGRADE", "HIGRADED", "EDUC", "OCC1950",
  "INCWAGE", "OCCSCORE", "STATEFIP", "COUNTYICP", "URBAN", "FARM",
  "VERSIONHIST", "VERSIONHIK", "LINK1940", "LINK1950", "scientist_occ1950")
vs_cohorts <- data.frame(cohort=c("1922-1927","1910-1921","1921-1927","1910-1927"),
  lo=c(1922L,1910L,1921L,1910L), hi=c(1927L,1921L,1927L,1927L))
vs_common <- c("school_years_1940", "college_completed_1940", "school_1940",
  "scientist_1940", "urban_1940", "farm_1940", "home_owned_1940")
vs_labor <- c("log_wage", "zero_wage", "occscore", "zero_occscore", "white_collar", "unskilled")
vs_young_controls <- c("father_school_years", "father_college_completed", paste0("father_",vs_labor))
vs_old_controls <- paste0("own_",vs_labor)

vs_school_sql <- function(x) sprintf("CASE WHEN %1$s IS NULL OR %1$s IN (0,999) THEN NULL WHEN %1$s BETWEEN 10 AND 230 THEN CAST(LEAST(17,GREATEST(0,FLOOR(%1$s/10.0)-3)) AS INTEGER) ELSE NULL END",x)
vs_labor_sql <- function(prefix, output) {
  w <- paste0(prefix,"INCWAGE"); o <- paste0(prefix,"OCC1950"); s <- paste0(prefix,"OCCSCORE")
  c(sprintf("CASE WHEN %1$s BETWEEN 0 AND 999997 THEN CASE WHEN %1$s=0 THEN 0 ELSE LN(%1$s) END END AS %2$slog_wage",w,output),
    sprintf("CASE WHEN %s BETWEEN 0 AND 999997 THEN CAST(%s=0 AS INTEGER) END AS %szero_wage",w,w,output),
    sprintf("%s AS %soccscore",s,output),
    sprintf("CASE WHEN %s IS NOT NULL THEN CAST(%s=0 AS INTEGER) END AS %szero_occscore",s,s,output),
    sprintf("CASE WHEN %1$s IS NOT NULL AND %1$s NOT IN (979,997) THEN CAST(%1$s BETWEEN 0 AND 299 AS INTEGER) END AS %2$swhite_collar",o,output),
    sprintf("CASE WHEN %1$s IS NOT NULL AND %1$s NOT IN (979,997) THEN CAST(%1$s BETWEEN 900 AND 970 AS INTEGER) END AS %2$sunskilled",o,output))
}

# Inputs are SQL views sl, broad1940, fullperson, households. Fixtures use the same joins.
vs_prepare <- function(con) {
  dbExecute(con,"CREATE TEMP TABLE eligible AS SELECT * FROM sl WHERE YEAR=1950 AND SEX=1 AND SLREC=2 AND SLWT>0 AND VETWWII IN (1,2) AND BIRTHYR BETWEEN 1910 AND 1927")
  vs_unique(con,"eligible","HISTID")
  dbExecute(con,"CREATE TEMP TABLE prewar AS SELECT p.* FROM sl p JOIN eligible e ON p.HIK=e.HIK WHERE p.YEAR=1940 AND e.LINK1940=1 AND p.LINK1950=1 AND NULLIF(TRIM(e.HIK),'') IS NOT NULL")
  vs_unique(con,"prewar","HIK")
  dbExecute(con,"CREATE TEMP TABLE father_keys AS SELECT DISTINCT SERIAL,POPLOC FROM prewar WHERE POPLOC>0")
  vs_say("Looking up fathers in the broader linked extract")
  dbExecute(con,"CREATE TEMP TABLE fathers AS SELECT f.* FROM broad1940 f JOIN father_keys k ON f.SERIAL=k.SERIAL AND f.PERNUM=k.POPLOC WHERE f.YEAR=1940")
  vs_unique(con,"fathers",c("SERIAL","PERNUM"))
  dbExecute(con,"CREATE TEMP TABLE full_keys AS SELECT DISTINCT UPPER(TRIM(HISTID)) AS norm_histid FROM prewar")
  vs_say("Matching full-count person records by HISTID")
  dbExecute(con,"CREATE TEMP TABLE full_matches AS SELECT f.HISTID,f.SERIAL,f.AGE,f.RACE,f.BPL,f.FBPL,UPPER(TRIM(f.HISTID)) AS norm_histid FROM fullperson f JOIN full_keys k ON UPPER(TRIM(f.HISTID))=k.norm_histid WHERE f.YEAR=1940")
  vs_unique(con,"full_matches","norm_histid")
  vs_say("Joining full-count household ownership")
  dbExecute(con,"CREATE TEMP TABLE household_keys AS SELECT DISTINCT SERIAL FROM full_matches")
  dbExecute(con,"CREATE TEMP TABLE household_matches AS SELECT h.SERIAL,h.OWNERSHP FROM households h JOIN household_keys k ON h.SERIAL=k.SERIAL WHERE h.YEAR=1940")
  vs_unique(con,"household_matches","SERIAL")
  e_fields <- unique(c(vs_fields,"YEAR","BIRTHYR","SLWT","SLREC","VETWWII","EMPSTATD","CLASSWKRD"))
  aliases <- function(fields, alias, prefix) paste(sprintf("%s.%s AS %s%s",alias,fields,prefix,fields),collapse=",")
  vs_say("Assembling one row per 1950 respondent")
  # A left-only flag predicate in ON causes a quadratic BLOCKWISE_NL_JOIN in
  # DuckDB 1.4.4. Put the flag inside the equality key to retain a hash join.
  query <- paste0("CREATE TEMP TABLE raw_wide AS SELECT ",
    aliases(e_fields,"e","y1950_"),",",aliases(vs_fields,"p","y1940_"),",",
    aliases(vs_fields,"f","father1940_"),",",
    aliases(c("HISTID","SERIAL","AGE","RACE","BPL","FBPL"),"q","fc1940_"),
    ",h.OWNERSHP AS hh1940_OWNERSHP FROM eligible e LEFT JOIN prewar p ON CASE WHEN e.LINK1940=1 THEN e.HIK END=p.HIK LEFT JOIN fathers f ON p.SERIAL=f.SERIAL AND p.POPLOC=f.PERNUM LEFT JOIN full_matches q ON UPPER(TRIM(p.HISTID))=q.norm_histid LEFT JOIN household_matches h ON q.SERIAL=h.SERIAL")
  dbExecute(con,query)
  vs_unique(con,"raw_wide","y1950_HISTID")
  vs_fail(vs_scalar(con,"SELECT COUNT(*) FROM raw_wide") != vs_scalar(con,"SELECT COUNT(*) FROM eligible"),"Join changed the respondent count")
  vs_say("Deriving prewar controls and eligibility")
  derivations <- c(
    "y1950_HISTID AS respondent_id", "y1950_HIK AS hik", "y1950_BIRTHYR AS birthyr",
    "CAST(y1950_SLWT AS DOUBLE) AS weight", "CAST(y1950_VETWWII=2 AS INTEGER) AS veteran",
    "CAST(y1950_scientist_occ1950 AS INTEGER) AS scientist_1950",
    "CAST(y1940_scientist_occ1950 AS INTEGER) AS scientist_1940",
    "y1940_HISTID IS NOT NULL AS linked_1940", "father1940_HISTID IS NOT NULL AS father_available",
    "fc1940_HISTID IS NOT NULL AS fullcount_available", "y1950_BIRTHYR>=1922 AS young",
    "COALESCE(y1950_OCC1950=595 OR y1950_EMPSTATD BETWEEN 13 AND 15 OR y1950_CLASSWKRD=26,FALSE) AS active_military_1950",
    "COALESCE(y1940_RACE=y1950_RACE AND y1940_BPL=y1950_BPL AND 1940-y1940_AGE=1950-y1950_AGE,FALSE) AS strict_link",
    "CASE WHEN y1950_BPL BETWEEN 1 AND 56 OR y1950_BPL IN (90,99) THEN TRUE ELSE FALSE END AS us_born",
    paste0(vs_school_sql("y1940_HIGRADED")," AS school_years_1940"),
    paste0(vs_school_sql("father1940_HIGRADED")," AS father_school_years"),
    "CASE WHEN y1940_HIGRADED BETWEEN 10 AND 230 THEN CAST(y1940_HIGRADED>=190 AS INTEGER) END AS college_completed_1940",
    "CASE WHEN father1940_HIGRADED BETWEEN 10 AND 230 THEN CAST(father1940_HIGRADED>=190 AS INTEGER) END AS father_college_completed",
    "CASE WHEN y1940_SCHOOL IN (1,2) THEN CAST(y1940_SCHOOL=2 AS INTEGER) END AS school_1940",
    "CASE WHEN y1940_URBAN IN (1,2) THEN CAST(y1940_URBAN=2 AS INTEGER) END AS urban_1940",
    "CASE WHEN y1940_FARM IN (1,2) THEN CAST(y1940_FARM=2 AS INTEGER) END AS farm_1940",
    "CASE WHEN hh1940_OWNERSHP IN (1,2) THEN CAST(hh1940_OWNERSHP=1 AS INTEGER) END AS home_owned_1940",
    "CASE WHEN y1940_STATEFIP BETWEEN 1 AND 56 AND y1940_COUNTYICP>0 THEN CAST(y1940_STATEFIP AS VARCHAR)||':'||CAST(y1940_COUNTYICP AS VARCHAR) END AS county_1940",
    "CAST(y1950_BPL AS VARCHAR) AS birthplace",
    "CASE WHEN fc1940_FBPL BETWEEN 1 AND 56 OR fc1940_FBPL IN (90,99) THEN 'US' WHEN fc1940_FBPL BETWEEN 400 AND 429 OR fc1940_FBPL=453 THEN 'NW' WHEN fc1940_FBPL BETWEEN 430 AND 440 OR fc1940_FBPL BETWEEN 450 AND 452 OR fc1940_FBPL BETWEEN 454 AND 457 OR fc1940_FBPL BETWEEN 459 AND 465 THEN 'SE' WHEN fc1940_FBPL IS NULL OR fc1940_FBPL IN (0,999) THEN 'Unknown' ELSE 'Other' END AS father_origin",
    vs_labor_sql("y1940_","own_"), vs_labor_sql("father1940_","father_"))
  dbExecute(con,paste0("CREATE TEMP TABLE derived AS SELECT *,",paste(derivations,collapse=",")," FROM raw_wide"))
  dbExecute(con,"CREATE TEMP TABLE classified AS SELECT *, CAST(y1950_RACE AS VARCHAR)||':'||father_origin AS background, CASE WHEN us_born AND y1950_RACE=1 AND father_origin='US' THEN 'US-W' WHEN us_born AND y1950_RACE=2 AND father_origin='US' THEN 'B' WHEN us_born AND y1950_RACE=1 AND father_origin IN ('NW','SE') THEN father_origin ELSE 'Outside paper groups' END AS paper_group FROM derived")
  missing <- function(fields) paste(sprintf("%s IS NULL",fields),collapse=" OR ")
  reason <- c("CASE WHEN NOT linked_1940 THEN 'no_reciprocal_1940_link' END",
    "CASE WHEN active_military_1950 THEN 'active_military_1950' END",
    "CASE WHEN county_1940 IS NULL OR y1950_BPL IS NULL OR y1950_BPL=999 OR y1950_RACE IS NULL THEN 'missing_fixed_effect' END",
    paste0("CASE WHEN ",missing(vs_common)," THEN 'missing_common_prewar_control' END"),
    "CASE WHEN young AND (NOT father_available OR father1940_SEX IS DISTINCT FROM 1) THEN 'no_usable_father' END",
    paste0("CASE WHEN young AND (",missing(vs_young_controls),") THEN 'missing_father_control' END"),
    paste0("CASE WHEN NOT young AND (",missing(vs_old_controls),") THEN 'missing_own_labor_control' END"))
  dbExecute(con,paste0("CREATE TEMP TABLE annotated AS SELECT *,CONCAT_WS(';',",paste(reason,collapse=","),") AS exclusion_reason FROM classified"))
  dbExecute(con,"CREATE TEMP TABLE analysis_data AS SELECT *,exclusion_reason='' AS regression_eligible FROM annotated")
  vs_unique(con,"analysis_data","respondent_id")
  invisible(TRUE)
}

vs_counts <- function(con) {
  cohorts <- paste(sprintf("('%s',%s,%s)",vs_cohorts$cohort,vs_cohorts$lo,vs_cohorts$hi),collapse=",")
  dbGetQuery(con,paste0("WITH cohorts(cohort,lo,hi) AS (VALUES ",cohorts,
    ") SELECT c.cohort,COUNT(*) AS eligible_1950,COUNT(*) FILTER(WHERE linked_1940) AS linked_1940,COUNT(*) FILTER(WHERE y1940_POPLOC>0) AS father_pointer,COUNT(*) FILTER(WHERE father_available) AS father_available,COUNT(*) FILTER(WHERE fullcount_available) AS fullcount_available,COUNT(*) FILTER(WHERE regression_eligible) AS regression_eligible,COUNT(*) FILTER(WHERE regression_eligible AND strict_link) AS strict_regression_eligible FROM analysis_data a JOIN cohorts c ON a.birthyr BETWEEN c.lo AND c.hi GROUP BY 1 ORDER BY 1"))
}

vs_write_dictionary <- function(con, directory, sample_dir) {
  schema <- dbGetQuery(con,"DESCRIBE analysis_data")
  originals <- read.csv(file.path(sample_dir,"field_dictionary.csv"),stringsAsFactors=FALSE)
  describe <- function(n) {
    prefixes <- c("y1950_","y1940_","father1940_","fc1940_","hh1940_")
    prefix <- prefixes[vapply(prefixes,function(p) startsWith(n,p),logical(1))]
    if (length(prefix)) {
      field <- substring(n,nchar(prefix[[1]])+1)
      label <- originals$label[match(field,originals$name)]
      if (is.na(label)) label <- switch(field, FBPL="Reported father's birthplace",OWNERSHP="Home ownership",field)
      return(paste(prefix[[1]], label, "(raw source code; not overwritten across extracts)"))
    }
    descriptions <- c(respondent_id="1950 HISTID; unique analysis-row identifier",weight="1950 SLWT; never a 1940 weight",
      veteran="1 if VETWWII=2, 0 if VETWWII=1",scientist_1950="Inherited 1950 scientist flag: OCC1950 7,61,62,63,67,68,69",
      scientist_1940="Same scientist definition on respondent's linked 1940 row; NULL without link",
      young="BIRTHYR >= 1922; selects father rather than own labor controls",
      strict_link="Agreement in 1940/1950 race, BPL, and year minus AGE",
      home_owned_1940="Full-count household OWNERSHP: 1 owned, 0 rented, otherwise NULL",
      county_1940="Linked 1940 state FIPS and ICPSR county, separated by colon; not a FIPS county conversion",
      background="1950 race combined with father-origin category; includes Other and Unknown",
      paper_group="US-born US-W, B, NW, SE; other respondents retained for pooled analysis",
      father_origin="1940 reported FBPL: US=1-56,90,99; NW=400-429,453; SE=430-440,450-452,454-457,459-465; otherwise Other/Unknown",
      regression_eligible="Complete required prewar controls and valid geography; reciprocal link; civilian in 1950; usable father required only for young",
      exclusion_reason="Semicolon-separated applicable reasons; empty means eligible")
    if (n %in% names(descriptions)) return(unname(descriptions[n]))
    if (grepl("school_years",n)) return("Completed years: max(0,floor(HIGRADED/10)-3), capped at 17; 0/999/NULL missing")
    if (grepl("college_completed",n)) return("Completed 16+ years, HIGRADED >=190; unfinished first college year is not completion")
    if (grepl("log_wage",n)) return("log(INCWAGE) for positive valid wage; 0 for valid zero; NULL for missing/N/A 999998/999999")
    if (grepl("zero_wage",n)) return("1 for valid zero wage; 0 for positive wage; NULL for missing/N/A")
    if (grepl("white_collar",n)) return("OCC1950 0-299; 979/997 and NULL missing; non-occupation/N/A codes are 0")
    if (grepl("unskilled",n)) return("Nonfarm unskilled OCC1950 900-970; 979/997 and NULL missing")
    if (grepl("zero_occscore",n)) return("Indicator OCCSCORE=0; NULL if source score NULL")
    if (grepl("occscore",n)) return("Unmodified 1940 occupational income score")
    if (n %in% c("school_1940","urban_1940","farm_1940")) return("Binary recode of source 1/2; 1 denotes yes/urban/farm; other codes NULL")
    paste("Derived field",n,"; see builder and definitions.md")
  }
  dictionary <- data.frame(name=schema$column_name,type=schema$column_type,
    description=vapply(schema$column_name,describe,character(1)))
  write.csv(dictionary,file.path(directory,"field_dictionary.csv"),row.names=FALSE)
}

main <- function() {
  sample_dir <- Sys.getenv("IPUMS_1950_SAMPLE_LINE_PARQUET_DIR", "D:/ipums_linked_1950_sample_line")
  broad_dir <- Sys.getenv("IPUMS_LINKED_1950_PARQUET_DIR", "D:/ipums_linked_1900_1950")
  full_dir <- Sys.getenv("IPUMS_1940_PARQUET_DIR", "D:/ipums_1940_fullcount")
  output <- Sys.getenv("IPUMS_VETERAN_SCIENTIST_PARQUET_DIR", "D:/ipums_linked_1950_veteran_scientist")
  vs_fail(file.exists(output) || dir.exists(output),paste("Output already exists:",output))
  stage <- paste0(output,".staging-",format(Sys.time(),"%Y%m%d%H%M%S"),"-",Sys.getpid())
  dir.create(stage,recursive=TRUE)
  con <- dbConnect(duckdb())
  connected <- TRUE
  on.exit(if(connected) dbDisconnect(con,shutdown=TRUE),add=TRUE)
  dbExecute(con,"SET threads=8"); dbExecute(con,"SET memory_limit='14GB'")
  dbExecute(con,"SET preserve_insertion_order=false")
  reader <- function(root,parts) {
    glob <- vs_path(file.path(root,parts,"*.parquet"))
    vs_fail(!length(Sys.glob(glob)),paste("No Parquet files:",glob))
    paste0("read_parquet(",dbQuoteString(con,glob),",hive_partitioning=true)")
  }
  dbExecute(con,paste0("CREATE VIEW sl AS SELECT * FROM ",reader(sample_dir,"birth_bpl=*")))
  dbExecute(con,paste0("CREATE VIEW broad1940 AS SELECT YEAR,",paste(vs_fields,collapse=",")," FROM ",reader(broad_dir,"birth_bpl=*")," WHERE YEAR=1940"))
  dbExecute(con,paste0("CREATE VIEW fullperson AS SELECT YEAR,HISTID,SERIAL,AGE,RACE,BPL,FBPL FROM ",reader(file.path(full_dir,"person"),"statefip=*")))
  dbExecute(con,paste0("CREATE VIEW households AS SELECT YEAR,SERIAL,OWNERSHP FROM ",reader(file.path(full_dir,"household"),"statefip=*")))
  vs_say("Building eligible respondent and reciprocal prewar records")
  vs_prepare(con)
  counts <- vs_counts(con)
  print(counts)
  expected <- data.frame(cohort=c("1921-1927","1910-1927"),eligible_1950=c(866793,2345225),linked_1940=c(808226,2118285),father_available=c(679174,1124956))
  if (identical(vs_path(normalizePath(sample_dir)),"D:/ipums_linked_1950_sample_line")) {
    actual <- counts[match(expected$cohort,counts$cohort),names(expected)]
    vs_fail(!all(as.matrix(actual)==as.matrix(expected)),"Production cohort/link/father counts changed")
  }
  mismatches <- dbGetQuery(con,"SELECT COUNT(*) FILTER(WHERE fullcount_available) AS fullcount_matches,COUNT(*) FILTER(WHERE fullcount_available AND y1940_SERIAL<>fc1940_SERIAL) AS serial_differences,COUNT(*) FILTER(WHERE fullcount_available AND y1940_AGE IS DISTINCT FROM fc1940_AGE) AS age_differences,COUNT(*) FILTER(WHERE fullcount_available AND y1940_RACE IS DISTINCT FROM fc1940_RACE) AS race_differences,COUNT(*) FILTER(WHERE fullcount_available AND y1940_BPL IS DISTINCT FROM fc1940_BPL) AS birthplace_differences FROM analysis_data")
  vs_fail(vs_scalar(con,"SELECT COUNT(*) FROM analysis_data WHERE scientist_1950 IS DISTINCT FROM CAST(y1950_OCC1950 IN (7,61,62,63,67,68,69) AS INTEGER)")>0,"Inherited scientist flag differs from occupation codes")
  groups <- dbGetQuery(con,"SELECT birthyr,veteran,COUNT(*) AS sample_n,SUM(weight) AS weighted_n,SUM(scientist_1950) AS scientist_n,SUM(weight*scientist_1950) AS scientist_weighted FROM analysis_data GROUP BY 1,2 ORDER BY 1,2")
  old_summary <- file.path(vs_data_root(),"results/mandatory_schooling/linked_1950_sample_line_veterans/group_summary.csv")
  if (file.exists(old_summary)) {
    saved <- read.csv(old_summary)
    for (i in seq_len(nrow(saved))) {
      limits <- as.integer(strsplit(saved$cohort[i],"-",fixed=TRUE)[[1]])
      rows <- groups[groups$birthyr>=limits[1] & groups$birthyr<=limits[2] & groups$veteran==as.integer(saved$veteran_code[i]==2),]
      vs_fail(any(abs(c(sum(rows$sample_n),sum(rows$weighted_n),sum(rows$scientist_n),sum(rows$scientist_weighted))-unlist(saved[i,c("sample_n","weighted_n","scientist_n","scientist_weighted")]))>1e-6),"Raw scientist summaries do not reproduce previous analysis")
    }
  }
  vs_say("Writing and validating Parquet")
  dbExecute(con,paste0("COPY analysis_data TO ",dbQuoteString(con,vs_path(file.path(stage,"data.parquet")))," (FORMAT PARQUET,COMPRESSION ZSTD,ROW_GROUP_SIZE 100000)"))
  written <- paste0("read_parquet(",dbQuoteString(con,vs_path(file.path(stage,"data.parquet"))),")")
  vs_fail(vs_scalar(con,paste0("SELECT COUNT(*) FROM ",written)) != vs_scalar(con,"SELECT COUNT(*) FROM eligible"),"Written row count mismatch")
  vs_fail(vs_scalar(con,paste0("SELECT COUNT(*)-COUNT(DISTINCT respondent_id) FROM ",written)) != 0,"Written IDs not unique")
  write.csv(counts,file.path(stage,"validation_cohorts.csv"),row.names=FALSE)
  write.csv(mismatches,file.path(stage,"source_version_audit.csv"),row.names=FALSE)
  vs_write_dictionary(con,stage,sample_dir)
  manifest <- list(completed_utc=format(Sys.time(),tz="UTC",usetz=TRUE),sources=list(sample_line=sample_dir,broader_linked=broad_dir,fullcount=full_dir),
    output=output,rows=vs_scalar(con,"SELECT COUNT(*) FROM analysis_data"),cohort_counts=counts,source_version_audit=mismatches,
    weighting="1950 SLWT only; no correction for selection into linkage",joins="Reciprocal HIK to respondent; YEAR/SERIAL/POPLOC to linked father; normalized HISTID to full count; full-count SERIAL to household",
    validation="Unique IDs and join keys; unchanged row count; scientist flag/code agreement; known production counts; prior descriptive counts; Parquet read-back")
  write_json(manifest,file.path(stage,"_manifest.json"),pretty=TRUE,auto_unbox=TRUE)
  dbDisconnect(con,shutdown=TRUE)
  connected <- FALSE
  vs_fail(!file.rename(stage,output),paste("Validated staging promotion failed:",stage))
  vs_say("Dataset promoted to ",output)
}
if (sys.nframe()==0L) main()
