script <- sub('^--file=', '', grep('^--file=',commandArgs(FALSE),value=TRUE))
repo_root <- if(length(script)) normalizePath(file.path(dirname(script[1]),'../..'),winslash='/') else Sys.getenv('GTL_REPO',getwd())
source(file.path(repo_root,'paths.R'))
source(file.path(repo_root,'prep/mandatory_schooling/monthly_amws_helpers.R'))
expect_failure <- function(expr) stopifnot(inherits(tryCatch(force(expr),error=identity),'error'))
raw <- as.data.table(c(list(geo='usa',name='United States'),
  as.list(setNames(rep(120,36),as.character(1915:1950)))))
fixture_file <- tempfile(fileext='.csv')
fwrite(raw,fixture_file)
fixture <- CJ(GEOID=c('01001','01003','01005'),year=1915:1950)
fixture[, population := c(120,0,NA_real_)[match(GEOID,c('01001','01003','01005'))]]
fixture[, `:=`(us_births_year=120,us_pop_year=1200,us_birth_rate_year=0.1)]
fixture[, county_births_estimate_year := population*us_birth_rate_year]
national <- validate_gapminder_annual(fixture,fixture_file)
stopifnot(nrow(national)==36L,all(national$gapminder_us_births==120))
counts <- c(0,1,2,1,1)
births <- c(12,12,24,0,NA_real_)
rate <- monthly_rate(counts,births/12)
stopifnot(identical(rate,c(0,1000,1000,NA_real_,NA_real_)))
verify_monthly_rates(counts,births/12,rate)
expect_failure(verify_monthly_rates(counts,births/12,rep(0,5)))
bad <- copy(fixture); bad[1,us_births_year:=121]
expect_failure(validate_gapminder_annual(bad,fixture_file))
bad <- copy(fixture); bad[1,county_births_estimate_year:=99]
expect_failure(validate_gapminder_annual(bad,fixture_file))
bad <- copy(fixture); bad[,us_pop_year:=2400]
expect_failure(validate_gapminder_annual(bad,fixture_file))
raw[, '1915' := 121]; fwrite(raw,fixture_file)
expect_failure(validate_gapminder_annual(fixture,fixture_file))
unlink(fixture_file)
message('Gapminder denominator, source-consistency and missing/zero tests passed.')

if ('--integration' %in% commandArgs(TRUE)) {
  dest <- file.path(DATA_OUTPUT,'mandatory_schooling')
  # Preserve every existing NHGIS panel/audit and annual/source input by hash.
  protected <- c(file.path(dest,c('county_amws_monthly_1915_1950.csv','county_nhgis_annual_births_1915_1950.csv')),
    sort(list.files(file.path(dest,'monthly_1915_1950_audits'),full.names=TRUE)),
    file.path(DATA_OUTPUT,c('us_panel_county_amws_combined_year.csv','us_panel_county_stem_year_1800.csv')),
    file.path(AMWS_OUTPUT,'amws_combined_us_geocoded.csv'),
    file.path(TALENT_DETS_DATA_DIR,'Data/processed/amws/amws_ed86_final.csv'),
    file.path(DATA_INPUT,'new_births_total_number_estimated.csv'))
  if (any(!file.exists(protected))) stop('Integration requires existing NHGIS panel and annual/source inputs')
  before <- tools::md5sum(protected)
  runner <- file.path(R.home('bin'),'Rscript.exe')
  previous <- getwd(); setwd(tempdir())
  run <- function(source=NULL) {
    args <- c(shQuote(file.path(repo_root,'prep/mandatory_schooling/build_amws_county_month.R')),
      if (!is.null(source)) paste0('--births-source=',source))
    out <- system2(runner,args,stdout=TRUE,stderr=TRUE)
    status <- attr(out,'status'); if (!is.null(status) && status!=0L) stop(paste(out,collapse='\n'))
    message(paste(out,collapse='\n'))
  }
  run('gapminder')
  outputs <- c(file.path(dest,c('county_amws_monthly_gapminder_1915_1950.csv','county_gapminder_annual_births_1915_1950.csv')),
    sort(list.files(file.path(dest,'monthly_gapminder_1915_1950_audits'),full.names=TRUE)))
  first <- tools::md5sum(outputs)
  run('gapminder')
  stopifnot(identical(first,tools::md5sum(outputs)),identical(before,tools::md5sum(protected)))
  # Default mode remains equivalent to the pre-change NHGIS output, byte for byte.
  run()
  stopifnot(identical(before,tools::md5sum(protected)))
  setwd(previous)
  metrics <- c('GEOID','year','month','month_start','n_amws','n_amws_1906_1955_dedup','n_amws_1986',
    'n_early_year','n_1986_year','n_early_assigned','n_1986_assigned','n_unassigned_month',
    'n_early_unassigned','n_1986_unassigned','birth_month_completeness','birth_month_complete',
    'early_month_completeness','late_month_completeness','population','population_source','annual_panel_present')
  nhgis <- fread(file.path(dest,'county_amws_monthly_1915_1950.csv'),select=metrics,colClasses=c(GEOID='character'))
  gap <- fread(file.path(dest,'county_amws_monthly_gapminder_1915_1950.csv'),select=metrics,colClasses=c(GEOID='character'))
  stopifnot(identical(as.list(nhgis),as.list(gap)),nrow(gap)==1335744L,
    !anyDuplicated(gap[,.(GEOID,year,month)]),all(gap[, .N,by=GEOID]$N==432L))
  annual <- fread(file.path(dest,'county_gapminder_annual_births_1915_1950.csv'))
  cov <- fread(file.path(dest,'monthly_gapminder_1915_1950_audits/coverage_year.csv'))
  stopifnot(identical(cov$year,1915:1950),
    identical(cov$usable_denominators,c(rep(2894L,5),rep(3051L,10),rep(3084L,20),3088L)))
  m <- fread(file.path(dest,'county_amws_monthly_gapminder_1915_1950.csv'),
    select=c('GEOID','year','n_amws','county_births_estimate_year','monthly_allocated_births','amws_per_1000_births'))
  verify_monthly_rates(m$n_amws,m$monthly_allocated_births,m$amws_per_1000_births)
  stopifnot(isTRUE(all.equal(m$monthly_allocated_births,m$county_births_estimate_year/12,tolerance=1e-10)))
  summed <- m[,.(annual=county_births_estimate_year[1],
    monthly_sum=if(all(is.na(monthly_allocated_births))) NA_real_ else sum(monthly_allocated_births)),by=.(GEOID,year)]
  stopifnot(isTRUE(all.equal(summed$annual,summed$monthly_sum,tolerance=1e-10)))
  message('Integration passed: expected coverage; identical scientist counts/completeness; exact allocation and rates; deterministic cached outputs; existing NHGIS outputs, annual panels and sources unchanged.')
}
