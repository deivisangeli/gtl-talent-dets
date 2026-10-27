script <- sub('^--file=', '', grep('^--file=',commandArgs(FALSE),value=TRUE))
repo_root <- if(length(script)) normalizePath(file.path(dirname(script[1]),'../..'),winslash='/') else Sys.getenv('GTL_REPO',getwd())
source(file.path(repo_root,'paths.R'))
source(file.path(repo_root,'prep/mandatory_schooling/monthly_amws_helpers.R'))
cases <- data.table(text=c('Jan. 12, 29','February 29, 1924','Feb17,43','Oct, 14, 23',
  'Jan 12. 24','Feb 29, 1923','Jan 4, 1920','Oct 4, 21; B.S., May 3, 1942',
  'Ohio, 1921. A.B., May 3, 21','Cape May, N.J., March 2, 27',
  'Brooklyn, Dec. 17, Dec. 17, 16','May','Jan 2, 123','Jan 2, 19x','Jan 2, 1919'),
  year=c(1929,1924,1943,1923,1924,1923,1921,1921,1921,1927,1916,1921,1923,1919,NA),
  expected=c(1L,2L,2L,10L,1L,rep(NA_integer_,2),10L,NA_integer_,3L,rep(NA_integer_,5)))
p <- parse_amws_month(cases$text,cases$year,early=TRUE)
stopifnot(identical(p$month,cases$expected),p$month_status[6]=='invalid_birth_date',
          p$month_status[7]=='conflicting_birth_year',p$month_status[11]=='ambiguous_dates')
stopifnot(nrow(parse_amws_month(character(),integer()))==0L)
stopifnot(identical(monthly_geoid(c('1001','01001','','NA','0')),c('01001','01001',NA,NA,NA)))
stopifnot(identical(monthly_contaminated(c('See previous edition; b Ohio, 1920',
  'b Ohio, 1920; see previous edition','b Ohio, 1920',NA)),c(TRUE,FALSE,FALSE,FALSE)))
stopifnot(identical(monthly_rate(c(0,1,1,1),c(12,12,0,NA)),c(0,1000/12,NA,NA)))
b <- data.table(GISJOIN=c('A','B','C','D'),year=1930L,nhgis_dataset='1915_1941_cVS')
r <- data.table(GISJOIN=c('A','B','B','D'),GEOID=c('01001','01001','01003','01001'))
g <- match_birth_counties(b,r,c('01001','01003'))
stopifnot(g[GISJOIN=='B',county_match_status]=='ambiguous_historical_id',
  g[GISJOIN=='C',county_match_status]=='unmatched_historical_id',
  all(g[GISJOIN %in% c('A','D'),county_match_status]=='ambiguous_target_county'))
v <- data.table(nhgis_dataset=c('1915_1941_cVS','1939_1959_cVS'),year=1939L,
  annual_recorded_births=c(120,NA_real_))
stopifnot(nrow(choose_birth_vintage(v))==1L,is.na(choose_birth_vintage(v)$annual_recorded_births))
message('Monthly parsing, denominator and geography tests passed.')

if ('--integration' %in% commandArgs(TRUE)) {
  # Run from outside the repository, test all output hashes, preserve inputs.
  inputs <- c(file.path(DATA_OUTPUT,'us_panel_county_amws_combined_year.csv'),
    file.path(DATA_OUTPUT,'us_panel_county_stem_year_1800.csv'),
    file.path(AMWS_OUTPUT,'amws_combined_us_geocoded.csv'),
    file.path(TALENT_DETS_DATA_DIR,'Data/processed/amws/amws_ed86_final.csv'))
  before_inputs <- tools::md5sum(inputs)
  previous <- getwd(); setwd(tempdir())
  runner <- file.path(R.home('bin'),'Rscript.exe')
  run <- function(name) {
    out <- system2(runner,shQuote(file.path(repo_root,'prep/mandatory_schooling',name)),stdout=TRUE,stderr=TRUE)
    status <- attr(out,'status'); if (!is.null(status) && status!=0L) stop(paste(out,collapse='\n'))
    message(paste(out,collapse='\n'))
  }
  run('download_nhgis_births.R')
  run('build_amws_county_month.R')
  dest <- file.path(DATA_OUTPUT,'mandatory_schooling')
  files <- c(file.path(dest,c('county_amws_monthly_1915_1950.csv','county_nhgis_annual_births_1915_1950.csv')),
    sort(list.files(file.path(dest,'monthly_1915_1950_audits'),full.names=TRUE)))
  first <- tools::md5sum(files)
  run('build_amws_county_month.R')
  stopifnot(identical(first,tools::md5sum(files)),identical(before_inputs,tools::md5sum(inputs)))
  setwd(previous)
  message('Integration passed: all output files identical on cached rerun; existing annual panels and scientist sources unchanged.')
}
