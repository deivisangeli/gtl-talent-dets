script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value=TRUE))
repo_root <- if (length(script)) normalizePath(file.path(dirname(script[1]), '../..'), winslash='/') else Sys.getenv('GTL_REPO', getwd())
source(file.path(repo_root, 'paths.R'))
source(file.path(repo_root, 'prep/mandatory_schooling/monthly_amws_helpers.R'))
source(file.path(repo_root, 'prep/mandatory_schooling/amws_panel_gender_helpers.R'))
source(file.path(repo_root, 'prep/mandatory_schooling/quarterly_amws_helpers.R'))
expect_failure <- function(expr) stopifnot(inherits(tryCatch(force(expr), error=identity), 'error'))
stopifnot(identical(amws_classify_flags(c(1L,0L,0L,NA,1L), c(0L,1L,0L,NA,0L)),
                    c('male','female','unclassified','unclassified','male')))
expect_failure(amws_classify_flags(1L,1L))
expect_failure(amws_classify_flags(2L,0L))
expect_failure(amws_classify_flags(.8,0L))
g <- data.table(edition=1955L, source_row=1:2, source_record_id='duplicate-id',
  GEOID=c('01001','01003'), birth_year=c(1900L,1901L), male_80=c(1L,0L), female_80=c(0L,1L),
  gender_80=c('male','female'), gender_reason_80='saved_flag')
r <- data.table(edition=c(1955L,1955L,1986L),source_row=c(2L,1L,9L),
  source_record_id=c('duplicate-id','duplicate-id','other'), GEOID=c('01003','01001','01001'), year=c(1901L,1900L,1900L))
j <- amws_attach_gender(r,g)
stopifnot(identical(j$gender_80,c('female','male','unclassified')),
          j$gender_reason_80[3]=='no_saved_inference')
bad <- copy(r); bad[1,year:=1900L]; expect_failure(amws_attach_gender(bad,g))
expect_failure(amws_attach_gender(rbind(r,r[1]),g))

records <- data.table(GEOID=c(rep('01001',5),'01003','01003'),year=1900L,
  source=c(rep('1906_1955_dedup',3),rep('1986',4)),month=c(1L,3L,NA,4L,NA,12L,NA),
  gender_80=c('male','female','male','female','unclassified','unclassified','male'))
cy <- data.table(GEOID=c('01001','01003'),year=1900L,population=c(1000,0),
  county_births_estimate_year=c(1200,0),n_early_year=c(3L,0L),n_1986_year=c(2L,2L),
  n_early_assigned=c(2L,0L),n_1986_assigned=c(1L,1L),n_early_unassigned=c(1L,0L),
  n_1986_unassigned=c(1L,1L),n_unassigned_month=c(2L,1L))
cy <- amws_add_gender_annual(cy,records)
stopifnot(cy[GEOID=='01001',birth_month_completeness_male_80]==.5,
  cy[GEOID=='01001',birth_month_completeness_female_80]==1,
  cy[GEOID=='01001',birth_month_completeness_unclassified_80]==0,
  is.na(cy[GEOID=='01003',birth_month_completeness_female_80]),
  cy[GEOID=='01003',birth_month_complete_female_80])
m <- cy[rep(seq_len(.N),each=12L)]
m[, `:=`(month=rep(1:12,times=nrow(cy)),monthly_allocated_births=county_births_estimate_year/12)]
base_counts <- records[!is.na(month),.(n_amws_1906_1955_dedup=sum(source=='1906_1955_dedup'),
  n_amws_1986=sum(source=='1986'),n_amws=.N),by=.(GEOID,year,month)]
base_counts <- merge(base_counts,amws_gender_month_counts(records),by=c('GEOID','year','month'))
m <- merge(m,base_counts,by=c('GEOID','year','month'),all.x=TRUE)
for(col in c('n_amws','n_amws_1986','n_amws_1906_1955_dedup',amws_gender_period_cols())) set(m,which(is.na(m[[col]])),col,0L)
m[, `:=`(amws_per_1000_births=monthly_rate(n_amws,monthly_allocated_births),
          amws_per_1000_pop=monthly_rate(n_amws,population))]
m <- amws_add_gender_rates(m,'monthly_allocated_births')
stopifnot(m[GEOID=='01001' & month==2,amws_per_1000_births_female_80]==0,
  all(is.na(m[GEOID=='01003',amws_per_1000_births_male_80])))
q <- amws_quarter_batch(cy,aggregate_amws_quarters(m))
invisible(amws_gender_reconciliation(m,cy,'monthly'))
invisible(amws_gender_reconciliation(q,cy,'quarterly'))
stopifnot(q[GEOID=='01001' & quarter==1,n_amws_male_80]==1L,
          q[GEOID=='01001' & quarter==1,n_amws_female_80]==1L)
for(d in c(NA_real_,0,-1)) {
  z <- copy(m); z[, `:=`(monthly_allocated_births=d,population=d,
    amws_per_1000_births=NA_real_,amws_per_1000_pop=NA_real_)]
  z <- amws_add_gender_rates(z,'monthly_allocated_births')
  stopifnot(all(is.na(z$amws_per_1000_births_male_80)),all(is.na(z$amws_per_1000_pop_female_80)))
}
old <- tempfile(fileext='.csv'); stage <- tempfile(fileext='.csv')
legacy <- data.table(GEOID=c('01001','01003'),year=1900L,n_amws=c(0L,1L),note=c('quote """ run','plain'))
fwrite(legacy,old); augmented <- copy(legacy); augmented[,n_amws_male_80:=c(0L,1L)]; fwrite(augmented,stage)
amws_verify_staged_panel(stage,old,2L)
augmented[1,n_amws:=1L]; fwrite(augmented,stage); expect_failure(amws_verify_staged_panel(stage,old,2L))
unlink(c(old,stage))
message('Saved-flag classification, stable-row linkage, annual/monthly/quarterly reconciliation, rates and staged-preservation tests passed.')

if ('--integration' %in% commandArgs(TRUE)) {
  dest <- file.path(DATA_OUTPUT,'mandatory_schooling')
  gender_dir <- file.path(AMWS_OUTPUT,'gender_inference')
  protected <- c(sort(list.files(gender_dir,recursive=TRUE,full.names=TRUE)),
    file.path(AMWS_OUTPUT,'amws_combined_us_geocoded.csv'),
    file.path(TALENT_DETS_DATA_DIR,'Data/processed/amws/amws_ed86_final.csv'),
    file.path(DATA_OUTPUT,c('us_panel_county_amws_combined_year.csv','us_panel_county_stem_year_1800.csv')),
    file.path(DATA_INPUT,c('new_births_total_number_estimated.csv','nhgis0005_ts_nominal_county.csv')))
  stopifnot(all(file.exists(protected)))
  before <- tools::md5sum(protected)
  run_all <- function() {
    previous <- getwd(); on.exit(setwd(previous)); setwd(tempdir())
    configs <- list(character(), '--births-source=gapminder', c('--births-source=gapminder','--years=all'))
    for(config in configs) {
      out <- system2(file.path(R.home('bin'),'Rscript.exe'),
        c(shQuote(file.path(repo_root,'prep/mandatory_schooling/build_amws_county_month.R')),config),stdout=TRUE,stderr=TRUE)
      if (!is.null(attr(out,'status')) && attr(out,'status')!=0L) stop(paste(out,collapse='\n'))
      message(paste(out,collapse='\n'))
    }
    out <- system2(file.path(R.home('bin'),'Rscript.exe'),
      shQuote(file.path(repo_root,'prep/mandatory_schooling/build_amws_county_quarter.R')),stdout=TRUE,stderr=TRUE)
    if (!is.null(attr(out,'status')) && attr(out,'status')!=0L) stop(paste(out,collapse='\n'))
    message(paste(out,collapse='\n'))
  }
  run_all()
  annual_names <- c('county_nhgis_annual_births_1915_1950.csv','county_gapminder_annual_births_1915_1950.csv',
                    'county_gapminder_annual_births_1800_1960.csv')
  panel_names <- c('county_amws_monthly_1915_1950.csv','county_amws_monthly_gapminder_1915_1950.csv',
    'county_amws_monthly_gapminder_1800_1960.csv','county_amws_quarterly_gapminder_1800_1960.csv')
  audit_dirs <- file.path(dest,c('monthly_1915_1950_audits','monthly_gapminder_1915_1950_audits',
    'monthly_gapminder_1800_1960_audits','quarterly_gapminder_1800_1960_audits'))
  outputs <- c(file.path(dest,c(annual_names,panel_names)),unlist(lapply(audit_dirs,function(d) sort(list.files(d,full.names=TRUE)))))
  first <- tools::md5sum(outputs)
  run_all()
  stopifnot(identical(first,tools::md5sum(outputs)),identical(before,tools::md5sum(protected)))
  annual <- lapply(file.path(dest,annual_names),function(f) fread(f,colClasses=c(GEOID='character',state_id='character')))
  for(a in annual) amws_validate_gender_annual(a)
  full <- annual[[3]]
  stopifnot(sum(full$n_early_year_male_80+full$n_1986_year_male_80)==94369L,
    sum(full$n_early_year_female_80+full$n_1986_year_female_80)==5480L,
    sum(full$n_early_year_unclassified_80+full$n_1986_year_unclassified_80)==14136L)
  subset <- full[year %between% c(1915L,1950L)]
  cols <- c('GEOID','year',amws_gender_columns(names(full)))
  stopifnot(identical(as.list(subset[,..cols]),as.list(annual[[2]][,..cols])),
            identical(as.list(annual[[1]][,..cols]),as.list(annual[[2]][,..cols])))
  no_saved <- fread(file.path(audit_dirs[3],'scientists_no_saved_gender_inference.csv'))
  stopifnot(nrow(no_saved)==1694L, uniqueN(no_saved[,.(GEOID,year)])==1119L, uniqueN(no_saved$GEOID)==454L,
    all(no_saved$gender_80=='unclassified'),
    identical(no_saved[, .N,by=edition][order(edition)]$N,c(187L,706L,580L,221L)))
  nh <- fread(file.path(audit_dirs[1],'no_saved_inference_denominator_coverage.csv'))
  stopifnot(sum(nh[annual_birth_denominator>0,records])==25L)
  for(i in seq_along(panel_names)) {
    is_quarter <- i==4L; expected_rows <- c(1335744L,1335744L,5973744L,1991248L)[i]
    time <- if(is_quarter) 'quarter' else 'month'
    denom <- if(is_quarter) 'quarterly_allocated_births' else 'monthly_allocated_births'
    metrics <- c('GEOID','year',time,'n_amws','n_amws_1906_1955_dedup','n_amws_1986','population',denom,
      'amws_per_1000_births','amws_per_1000_pop',amws_gender_period_cols(),
      as.vector(outer(c('amws_per_1000_births','amws_per_1000_pop'),amws_gender_suffix(amws_gender_groups),paste0)))
    p <- fread(file.path(dest,panel_names[i]),select=metrics,colClasses=c(GEOID='character'))
    stopifnot(nrow(p)==expected_rows,!anyDuplicated(p[,c('GEOID','year',time),with=FALSE]),
      all(p[,.N,by=GEOID]$N==expected_rows/3092L))
    amws_validate_gender_rates(p,denom)
    for(base in c('n_amws','n_amws_1906_1955_dedup','n_amws_1986')) {
      stopifnot(all(Reduce('+',lapply(amws_gender_groups,function(g) p[[paste0(base,amws_gender_suffix(g))]]))==p[[base]]))
    }
    if(i==3L) {
      p[,quarter:=amws_calendar_quarter(month)]
      wanted <- c('n_amws',amws_gender_period_cols())
      quarterly_expected <- p[,lapply(.SD,sum),by=.(GEOID,year,quarter),.SDcols=wanted]
      setorder(quarterly_expected,GEOID,year,quarter)
    }
    if(is_quarter) {
      wanted <- names(quarterly_expected)
      stopifnot(identical(as.list(p[,..wanted]),as.list(quarterly_expected)))
    }
    rm(p); invisible(gc())
  }
  message('Gender integration passed: all four panels and annual supports; original column preservation; exhaustive source/gender counts; rates; quarterly aggregation; 1694 missing-inference records and 25 positive NHGIS cases; deterministic reruns; inference and annual-analysis inputs unchanged.')
}
