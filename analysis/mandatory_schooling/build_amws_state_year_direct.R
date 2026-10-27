# Sourceable builder; directly runnable without estimating any regressions.
sdyr_build <- function(paths,repo,inventory,destination) {
  dir.create(destination,recursive=TRUE,showWarnings=FALSE)
  save <- function(d,name) fwrite(d,file.path(destination,name),na='')
  census_url <- paste0('https://www2.census.gov/programs-surveys/decennial/tables/1990/',
    'population-of-states-and-counties-us-1790-1990/population_partii.xls')
  raw_dir <- file.path(paths$root,'raw/mandatory_schooling_state_direct')
  dir.create(raw_dir,recursive=TRUE,showWarnings=FALSE)
  census_file <- file.path(raw_dir,'population_partii.xls')
  if (!file.exists(census_file)) download.file(census_url,census_file,mode='wb',quiet=TRUE)
  census <- sdyr_census(census_file,inventory)
  save(census$anchors,'state_population_census_anchors.csv')
  save(census$panel,'state_population_annual.csv')
  annual <- fread(paths$annual,select=c('year','us_births_year','us_pop_year','us_birth_rate_year'))
  national <- unique(annual);setorder(national,year)
  ysrd_require(!anyDuplicated(national$year)&&identical(national$year,1800:1960),
    'Require one existing national birth rate per year.')
  ysrd_require(all(is.finite(national$us_birth_rate_year)&national$us_birth_rate_year>0)&&
    isTRUE(all.equal(national$us_birth_rate_year,national$us_births_year/national$us_pop_year,tolerance=1e-10)),
    'Invalid existing national birth rates.')
  gapminder_file <- file.path(paths$root,'input/new_births_total_number_estimated.csv')
  usa <- fread(gapminder_file,header=TRUE)[geo=='usa']
  ysrd_require(nrow(usa)==1L&&all(as.character(national$year) %in% names(usa)),'Incomplete Gapminder USA row.')
  raw_births <- as.numeric(unlist(usa[,as.character(national$year),with=FALSE],use.names=FALSE))
  ysrd_require(identical(raw_births,as.numeric(national$us_births_year)),'Gapminder birth totals disagree with panel.')
  save(national,'national_birth_rate_inputs.csv')
  scientists <- sdyr_read_records(paths,inventory)
  save(scientists$audit,'scientist_record_dispositions.csv')
  save(scientists$additions,'state_only_addition_audit.csv')
  save(scientists$audit[,.(records=.N),by=.(edition,origin,reason,included)],'source_count_reconciliation.csv')
  save(scientists$records[state_county_disagreement==TRUE],'state_county_disagreements.csv')
  panel <- sdyr_panel(scientists$records,census$panel,national)
  save(panel,'state_amws_yearly_gapminder_1800_1960.csv')
  source <- scientists$records[,.(records=.N,first_birth=min(year),last_birth=max(year)),by=edition]
  save(source,'source_coverage_by_edition.csv')
  save(data.table(component=c('State census population','Boundary reconstruction documentation','Estimated births'),
    source=c(census_url,'https://www.census.gov/library/working-papers/2002/demo/POP-twps0056.html',gapminder_file),
    derivation=c('Forstall 1996, Part II; annual linear interpolation within observed census coverage.',
      'States and preceding territories in reconstructed 1990 boundaries to the extent possible; historical Census underenumeration remains.',
      'State population times existing national us_births_year/us_pop_year. Existing national population is county-panel-derived; not replaced with Census national totals.')),
    'population_and_birth_sources.csv')
  ysrd_require(sum(panel$n_amws)==nrow(scientists$records)&&all(panel$n_amws==panel$n_early+panel$n_1986),
    'Direct-state counts failed reconciliation.')
  observed <- merge(census$anchors[year %between% c(1800,1960)&!is.na(population),
    .(state_fips,year,anchor_population=population)],census$panel,by=c('state_fips','year'))
  ysrd_require(all(observed$anchor_population==observed$population),'Interpolation changed census anchors.')
  list(panel=panel,source=source,records=scientists$records,audit=scientists$audit,
    files=c(scientists$files,census_file,gapminder_file,paths$annual))
}

if (sys.nframe()==0L) {
  arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)
  here <- dirname(normalizePath(sub('^--file=','',arg[1]),winslash='/'))
  source(file.path(here,'yearly_state_rd_helpers.R'))
  source(file.path(here,'monthly_state_rd_helpers.R'))
  source(file.path(here,'../../prep/mandatory_schooling/monthly_amws_helpers.R'))
  source(file.path(here,'yearly_state_direct_helpers.R'))
  repo <- msrd_root();paths <- msrd_paths(repo)
  inv <- fread(file.path(paths$results,'cutoff_inventory.csv'),colClasses=c(state_fips='character'))
  destination <- file.path(paths$root,'output/mandatory_schooling/yearly_state_direct')
  built <- sdyr_build(paths,repo,inv,destination)
  message('Built direct-state annual panel: ',nrow(built$panel),' rows; ',nrow(built$records),' scientists.')
}
