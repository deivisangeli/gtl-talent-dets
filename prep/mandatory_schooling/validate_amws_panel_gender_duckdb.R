# Validate installed period panels with SQL; optionally finish the quarterly cached rerun.
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value=TRUE))
repo_root <- if(length(script)) normalizePath(file.path(dirname(script[1]), '../..'), winslash='/') else Sys.getenv('GTL_REPO',getwd())
source(file.path(repo_root,'paths.R'))
source(file.path(repo_root,'prep/mandatory_schooling/monthly_amws_helpers.R'))
source(file.path(repo_root,'prep/mandatory_schooling/amws_panel_gender_helpers.R'))
stopifnot(requireNamespace('duckdb',quietly=TRUE),requireNamespace('DBI',quietly=TRUE))
dest <- file.path(DATA_OUTPUT,'mandatory_schooling')
con <- DBI::dbConnect(duckdb::duckdb(),dbdir=tempfile(fileext='.duckdb'))
DBI::dbExecute(con,"SET memory_limit='4GB'")
DBI::dbExecute(con,'SET threads=4')
q <- function(x) as.character(DBI::dbQuoteString(con,x))
id <- function(x) as.character(DBI::dbQuoteIdentifier(con,x))
exec <- function(sql) invisible(DBI::dbExecute(con,sql))
query <- function(sql) DBI::dbGetQuery(con,sql)
check <- function(label,sql) {
  if(query(sql)[[1]][1] != 0) stop('DuckDB validation failed: ',label)
  message('Passed: ',label)
}
annual_bases <- amws_gender_annual_bases
complete <- c('birth_month_completeness','birth_month_complete','early_month_completeness','late_month_completeness')
annual_metrics <- c(annual_bases,complete,as.vector(outer(c(annual_bases,complete),amws_gender_suffix(amws_gender_groups),paste0)),
                    'gender_inference_threshold')
period_metrics <- c('n_amws','n_amws_1906_1955_dedup','n_amws_1986',amws_gender_period_cols(),
  'amws_per_1000_births','amws_per_1000_pop',as.vector(outer(c('amws_per_1000_births','amws_per_1000_pop'),amws_gender_suffix(amws_gender_groups),paste0)))
load <- function(name,file,period=NULL) {
  header <- names(fread(file,nrows=0))
  cols <- c('GEOID','state_id','year','population',annual_metrics,
    intersect(c('annual_recorded_births','county_births_estimate_year'),header))
  if(!is.null(period)) cols <- c(cols,period,paste0(period,'_start'),
    if(period=='month') 'monthly_allocated_births' else 'quarterly_allocated_births',period_metrics)
  stopifnot(all(cols %in% header))
  types <- ifelse(cols %in% c('GEOID','state_id','month_start','quarter_start'),'VARCHAR',
    ifelse(grepl('^birth_month_complete(_|$)',cols),'BOOLEAN',
      ifelse(grepl('^n_|^year$|^month$|^quarter$',cols),'BIGINT','DOUBLE')))
  select <- paste(sprintf('CAST(%s AS %s) AS %s',id(cols),types,id(cols)),collapse=',')
  exec(paste0('CREATE TABLE ',name,' AS SELECT ',select,' FROM read_csv(',q(normalizePath(file,winslash='/')),',all_varchar=true)'))
}
annual_names <- c('county_nhgis_annual_births_1915_1950.csv','county_gapminder_annual_births_1915_1950.csv','county_gapminder_annual_births_1800_1960.csv')
panel_names <- c('county_amws_monthly_1915_1950.csv','county_amws_monthly_gapminder_1915_1950.csv',
 'county_amws_monthly_gapminder_1800_1960.csv','county_amws_quarterly_gapminder_1800_1960.csv')
audit_dirs <- file.path(dest,c('monthly_1915_1950_audits','monthly_gapminder_1915_1950_audits',
 'monthly_gapminder_1800_1960_audits','quarterly_gapminder_1800_1960_audits'))
outputs <- c(file.path(dest,c(annual_names,panel_names)),unlist(lapply(audit_dirs,function(d) sort(list.files(d,full.names=TRUE)))))
before <- tools::md5sum(outputs)
# Completed staged monthly reruns can be checked without reparsing their CSVs.
if('--verify-staged-rerun' %in% commandArgs(TRUE)) {
  stages <- list.dirs(dest,recursive=FALSE,full.names=TRUE)
  stages <- stages[grepl('gender_monthly_stage_',basename(stages))]
  verified <- FALSE
  for(stage in stages) {
    files <- sort(list.files(stage,recursive=TRUE,full.names=TRUE))
    if(!length(files) || !file.exists(file.path(stage,panel_names[3]))) next
    rel <- substring(normalizePath(files,winslash='/'),nchar(normalizePath(stage,winslash='/'))+2L)
    target <- file.path(dest,rel)
    if(length(files)<20L || !all(file.exists(target))) next
    stopifnot(identical(unname(tools::md5sum(files)),unname(tools::md5sum(target))))
    verified <- TRUE
  }
  stopifnot(verified)
  message('Passed: completed full-monthly cached rerun is byte-identical, including annual support and audits')
}
if('--rerun-quarterly' %in% commandArgs(TRUE)) {
  Sys.setenv(AMWS_PANEL_VALIDATION_ENGINE='duckdb')
  status <- system2(file.path(R.home('bin'),'Rscript.exe'),shQuote(file.path(repo_root,'prep/mandatory_schooling/build_amws_county_quarter.R')))
  stopifnot(status==0L,identical(before,tools::md5sum(outputs)))
  message('Passed: quarterly cached rebuild and all installed output hashes unchanged')
}
for(i in 1:3) {
  message('Loading annual support ',i,' into DuckDB...')
  load(paste0('a',i),file.path(dest,annual_names[i]))
}
for(i in 1:4) {
  message('Loading panel ',i,' into DuckDB...')
  period <- if(i==4) 'quarter' else 'month'; denom <- if(i==4) 'quarterly_allocated_births' else 'monthly_allocated_births'
  n <- c(1335744,1335744,5973744,1991248)[i]; times <- c(432,432,1932,644)[i]
  p <- paste0('p',i); a <- paste0('a',min(i,3))
  load(p,file.path(dest,panel_names[i]),period)
  check(paste0(p,' grid and boundaries'),paste0('SELECT count(*) FROM (SELECT GEOID,count(*) n FROM ',p,' GROUP BY GEOID) WHERE n<>',times))
  stopifnot(query(paste0('SELECT count(*) n,count(DISTINCT GEOID) counties FROM ',p))$n==n,
            query(paste0('SELECT count(DISTINCT GEOID) n FROM ',p))$n==3092)
  check(paste0(p,' unique period keys'),paste0('SELECT count(*) FROM (SELECT GEOID,year,',period,',count(*) n FROM ',p,' GROUP BY ALL HAVING n<>1)'))
  check(paste0(p,' valid dates'),paste0('SELECT count(*) FROM ',p,' WHERE ',period,' NOT BETWEEN 1 AND ',if(i==4)4 else 12,
    ' OR CAST(',period,'_start AS DATE) IS DISTINCT FROM make_date(CAST(year AS INTEGER),CAST(',if(i==4)'(quarter-1)*3+1' else 'month',' AS INTEGER),1)'))
  bad <- character()
  for(base in c('n_amws','n_amws_1906_1955_dedup','n_amws_1986',annual_bases)) {
    summands <- paste0(base,amws_gender_suffix(amws_gender_groups))
    bad <- c(bad,paste0('(',paste(id(summands),collapse='+'),') IS DISTINCT FROM ',id(base)))
  }
  for(g in amws_gender_groups) {
    s <- amws_gender_suffix(g)
    bad <- c(bad,paste0('n_amws',s,' IS DISTINCT FROM n_amws_1906_1955_dedup',s,'+n_amws_1986',s))
    for(src in c('early','1986')) bad <- c(bad,paste0('n_',src,'_year',s,' IS DISTINCT FROM n_',src,'_assigned',s,'+n_',src,'_unassigned',s))
  }
  bad <- c(bad,'gender_inference_threshold IS DISTINCT FROM 0.8')
  check(paste0(p,' exhaustive gender/source and annual assignment counts'),paste0('SELECT count(*) FROM ',p,' WHERE ',paste(bad,collapse=' OR ')))
  bad <- character()
  for(g in c('',amws_gender_suffix(amws_gender_groups))) for(rate in c('amws_per_1000_births','amws_per_1000_pop')) {
    d <- if(rate=='amws_per_1000_pop') 'population' else denom
    expected <- paste0('1000.0*n_amws',g,'/',d); actual <- paste0(rate,g)
    bad <- c(bad,paste0('CASE WHEN ',d,'>0 THEN ',actual,' IS NULL OR abs(',actual,'-(',expected,'))>1e-9*greatest(1,abs(',expected,')) ELSE ',actual,' IS NOT NULL END'))
  }
  check(paste0(p,' all total/gender rate formulas, zero and missing denominators'),paste0('SELECT count(*) FROM ',p,' WHERE ',paste(bad,collapse=' OR ')))
  repeated <- c('population',annual_metrics,if(i==1)'annual_recorded_births' else 'county_births_estimate_year')
  check(paste0(p,' annual support metadata'),paste0('SELECT count(*) FROM ',p,' p LEFT JOIN ',a,' a USING(GEOID,year) WHERE a.GEOID IS NULL OR ',
    paste(paste0('p.',id(repeated),' IS DISTINCT FROM a.',id(repeated)),collapse=' OR ')))
  countcols <- c('n_amws_1906_1955_dedup','n_amws_1986',amws_gender_period_cols())
  exec(paste0('CREATE TABLE totals',i,' AS SELECT GEOID,year,',paste(sprintf('sum(%s) AS %s',id(countcols),id(countcols)),collapse=','),
    ',sum(',denom,') AS births FROM ',p,' GROUP BY GEOID,year'))
  bad <- c('n_amws_1906_1955_dedup IS DISTINCT FROM n_early_assigned','n_amws_1986 IS DISTINCT FROM n_1986_assigned')
  for(g in amws_gender_groups) {
    s <- amws_gender_suffix(g)
    bad <- c(bad,paste0('n_amws_1906_1955_dedup',s,' IS DISTINCT FROM n_early_assigned',s),paste0('n_amws_1986',s,' IS DISTINCT FROM n_1986_assigned',s))
  }
  annual_d <- if(i==1)'annual_recorded_births' else 'county_births_estimate_year'
  bad <- c(bad,paste0('(births IS NULL)<>( ',annual_d,' IS NULL)'),paste0('abs(births-',annual_d,')>1e-9*greatest(1,abs(',annual_d,'))'))
  check(paste0(p,' annual reconciliation and allocation'),paste0('SELECT count(*) FROM totals',i,' JOIN ',a,' USING(GEOID,year) WHERE ',paste(bad,collapse=' OR ')))
}
common <- c('GEOID','year','month','population','monthly_allocated_births','county_births_estimate_year',annual_metrics,period_metrics)
check('1915-1950 subset equality',paste0('SELECT count(*) FROM ((SELECT ',paste(id(common),collapse=','),' FROM p2 EXCEPT ALL SELECT ',
 paste(id(common),collapse=','),' FROM p3 WHERE year BETWEEN 1915 AND 1950) UNION ALL (SELECT ',paste(id(common),collapse=','),
 ' FROM p3 WHERE year BETWEEN 1915 AND 1950 EXCEPT ALL SELECT ',paste(id(common),collapse=','),' FROM p2))'))
common <- c('GEOID','year','month',annual_metrics,c('n_amws','n_amws_1906_1955_dedup','n_amws_1986',amws_gender_period_cols()))
check('NHGIS/Gapminder scientist counts and completeness equality',paste0('SELECT count(*) FROM ((SELECT ',paste(id(common),collapse=','),
 ' FROM p1 EXCEPT ALL SELECT ',paste(id(common),collapse=','),' FROM p2) UNION ALL (SELECT ',paste(id(common),collapse=','),
 ' FROM p2 EXCEPT ALL SELECT ',paste(id(common),collapse=','),' FROM p1))'))
counts <- c('n_amws','n_amws_1906_1955_dedup','n_amws_1986',amws_gender_period_cols())
exec(paste0('CREATE TABLE expected_quarters AS SELECT GEOID,year,CAST(floor((month-1)/3)+1 AS BIGINT) AS "quarter",',
 paste(sprintf('sum(%s) AS %s',id(counts),id(counts)),collapse=','),',sum(monthly_allocated_births) births FROM p3 GROUP BY ALL'))
check('every quarterly source/gender count equals three-month sum',paste0('SELECT count(*) FROM expected_quarters e FULL JOIN p4 p USING(GEOID,year,quarter) WHERE e.GEOID IS NULL OR p.GEOID IS NULL OR ',
 paste(paste0('e.',id(counts),' IS DISTINCT FROM p.',id(counts)),collapse=' OR '),
 ' OR (e.births IS NULL)<>(p.quarterly_allocated_births IS NULL) OR abs(e.births-p.quarterly_allocated_births)>1e-9*greatest(1,abs(e.births))'))
totals <- query('SELECT sum(n_early_year_male_80+n_1986_year_male_80) male, sum(n_early_year_female_80+n_1986_year_female_80) female, sum(n_early_year_unclassified_80+n_1986_year_unclassified_80) unclassified FROM a3')
stopifnot(as.numeric(totals[1,])==c(94369,5480,14136))
no_saved <- fread(file.path(audit_dirs[3],'scientists_no_saved_gender_inference.csv'))
stopifnot(nrow(no_saved)==1694,uniqueN(no_saved[,.(GEOID,year)])==1119,uniqueN(no_saved$GEOID)==454,
 all(no_saved$gender_80=='unclassified'),identical(no_saved[,.N,by=edition][order(edition)]$N,c(187L,706L,580L,221L)))
nh <- fread(file.path(audit_dirs[1],'no_saved_inference_denominator_coverage.csv'))
stopifnot(sum(nh[annual_birth_denominator>0,records])==25)
for(d in audit_dirs) {
  manifest <- fread(file.path(d,'input_manifest.csv'))
  stopifnot(identical(unname(tools::md5sum(manifest$file)),manifest$md5))
}
stopifnot(identical(before,tools::md5sum(outputs)))
DBI::dbDisconnect(con,shutdown=TRUE)
message('DuckDB validation passed: all four panels, three annual supports, full-period gender totals, 1694 absent-inference records, 25 positive NHGIS cases, grids, formulas, reconciliation, quarterly sums and unchanged input/output hashes.')
