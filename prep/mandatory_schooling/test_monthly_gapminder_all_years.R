script <- sub('^--file=', '', grep('^--file=',commandArgs(FALSE),value=TRUE))
repo_root <- if(length(script)) normalizePath(file.path(dirname(script[1]),'../..'),winslash='/') else Sys.getenv('GTL_REPO',getwd())
source(file.path(repo_root,'paths.R'))
source(file.path(repo_root,'prep/mandatory_schooling/monthly_amws_helpers.R'))
expect_failure <- function(expr) stopifnot(inherits(tryCatch(force(expr),error=identity),'error'))
raw <- as.data.table(c(list(geo='usa',name='United States'),
  as.list(setNames(rep(120,161),as.character(1800:1960)))))
fixture_file <- tempfile(fileext='.csv')
fwrite(raw,fixture_file)
fixture <- data.table(year=1800:1960,population=120,us_births_year=120,
                      us_pop_year=1200,us_birth_rate_year=0.1,county_births_estimate_year=12)
stopifnot(nrow(validate_gapminder_annual(fixture,fixture_file,1800:1960))==161L)
expect_failure(validate_gapminder_annual(fixture[year!=1801],fixture_file,1800:1960))
raw[, '1800' := NULL]; fwrite(raw,fixture_file)
expect_failure(validate_gapminder_annual(fixture,fixture_file,1800:1960))
unlink(fixture_file)
stopifnot(identical(monthly_rate(c(0,0,0,0,1),c(1,0,-1,NA,0.5)),c(0,NA,NA,NA,2000)))
dates <- parse_amws_month(c('Jan 2, 17','Feb 29, 00','February 29, 00','Dec 1, 1960'),c(1817,1800,1900,1960))
stopifnot(identical(dates$month,c(1L,NA_integer_,NA_integer_,12L)))
message('Full-period national coverage, missing/nonpositive rates and historical-date tests passed.')

if ('--integration' %in% commandArgs(TRUE)) {
  dest <- file.path(DATA_OUTPUT,'mandatory_schooling')
  protected <- c(file.path(dest,c('county_amws_monthly_1915_1950.csv','county_nhgis_annual_births_1915_1950.csv',
    'county_amws_monthly_gapminder_1915_1950.csv','county_gapminder_annual_births_1915_1950.csv')),
    sort(list.files(file.path(dest,'monthly_1915_1950_audits'),full.names=TRUE)),
    sort(list.files(file.path(dest,'monthly_gapminder_1915_1950_audits'),full.names=TRUE)),
    file.path(DATA_OUTPUT,c('us_panel_county_amws_combined_year.csv','us_panel_county_stem_year_1800.csv')),
    file.path(AMWS_OUTPUT,'amws_combined_us_geocoded.csv'),
    file.path(TALENT_DETS_DATA_DIR,'Data/processed/amws/amws_ed86_final.csv'),
    file.path(DATA_INPUT,'new_births_total_number_estimated.csv'))
  stopifnot(all(file.exists(protected)))
  before <- tools::md5sum(protected)
  outputs <- c(file.path(dest,c('county_amws_monthly_gapminder_1800_1960.csv','county_gapminder_annual_births_1800_1960.csv')),
    sort(list.files(file.path(dest,'monthly_gapminder_1800_1960_audits'),full.names=TRUE)))
  stopifnot(all(file.exists(outputs)))
  first <- tools::md5sum(outputs)
  previous <- getwd(); setwd(tempdir())
  out <- system2(file.path(R.home('bin'),'Rscript.exe'),
    c(shQuote(file.path(repo_root,'prep/mandatory_schooling/build_amws_county_month.R')),
      '--births-source=gapminder','--years=all'),stdout=TRUE,stderr=TRUE)
  setwd(previous)
  status <- attr(out,'status'); if (!is.null(status) && status!=0L) stop(paste(out,collapse='\n'))
  message(paste(out,collapse='\n'))
  stopifnot(identical(first,tools::md5sum(outputs)),identical(before,tools::md5sum(protected)))

  cov <- fread(file.path(dest,'monthly_gapminder_1800_1960_audits/coverage_year.csv'))
  stopifnot(identical(cov$year,1800:1960),all(cov$county_years==3092L),
    cov[year==1800,usable_denominators]==384L,cov[year==1960,usable_denominators]==3092L)
  state_cov <- fread(file.path(dest,'monthly_gapminder_1800_1960_audits/coverage_state_year.csv'))
  stopifnot(nrow(state_cov)==49L*161L)
  national <- fread(file.path(dest,'monthly_gapminder_1800_1960_audits/gapminder_national_birth_verification.csv'))
  stopifnot(nrow(national)==161L,all(national$gapminder_us_births==national$us_births_year))
  reconciliation <- fread(file.path(dest,'monthly_gapminder_1800_1960_audits/annual_scientist_reconciliation.csv'))
  stopifnot(all(reconciliation$early_reconciles),all(reconciliation$late_reconciles))
  metrics <- c('GEOID','year','month','n_amws','n_amws_1906_1955_dedup','n_amws_1986',
    'birth_month_completeness','birth_month_complete','early_month_completeness','late_month_completeness',
    'n_unassigned_month','population','county_births_estimate_year','monthly_allocated_births',
    'amws_per_1000_births','amws_per_1000_pop')
  full <- fread(file.path(dest,'county_amws_monthly_gapminder_1800_1960.csv'),select=metrics,
                colClasses=c(GEOID='character'))
  stopifnot(nrow(full)==5973744L,!anyDuplicated(full[,.(GEOID,year,month)]),
    all(full$month %in% 1:12),all(full[, .N,by=GEOID]$N==1932L))
  stopifnot(!is.unsorted(paste(full$GEOID,sprintf('%04d%02d',full$year,full$month))))
  verify_monthly_rates(full$n_amws,full$monthly_allocated_births,full$amws_per_1000_births)
  stopifnot(isTRUE(all.equal(full$monthly_allocated_births,full$county_births_estimate_year/12,tolerance=1e-10)))
  stopifnot(all(full[monthly_allocated_births>0 & n_amws==0,amws_per_1000_births]==0),
    all(is.na(full[is.na(monthly_allocated_births) | monthly_allocated_births<=0,amws_per_1000_births])))
  summed <- full[,.(annual=county_births_estimate_year[1],
    allocated=if(all(is.na(monthly_allocated_births))) NA_real_ else sum(monthly_allocated_births)),by=.(GEOID,year)]
  stopifnot(isTRUE(all.equal(summed$annual,summed$allocated,tolerance=1e-10)))
  overlap <- full[year %between% c(1915L,1950L)]
  rm(full,summed); gc()
  old <- fread(file.path(dest,'county_amws_monthly_gapminder_1915_1950.csv'),select=metrics,
               colClasses=c(GEOID='character'))
  stopifnot(identical(as.list(overlap),as.list(old)))
  # Explicitly compare all annual source completeness fields as well.
  cols <- c('GEOID','year','n_early_year','n_1986_year','n_early_assigned','n_1986_assigned',
    'n_early_unassigned','n_1986_unassigned','n_unassigned_month','birth_month_completeness',
    'birth_month_complete','early_month_completeness','late_month_completeness','annual_panel_present')
  a <- fread(file.path(dest,'county_gapminder_annual_births_1800_1960.csv'),select=cols)[year %between% c(1915L,1950L)]
  b <- fread(file.path(dest,'county_gapminder_annual_births_1915_1950.csv'),select=cols)
  stopifnot(identical(as.list(a),as.list(b)))
  message('Full-period integration passed: 5,973,744 unique rows; all 161 years; exact 1915-1950 overlap; rates and reconciliation; deterministic cached rerun; existing outputs and sources unchanged.')
}
