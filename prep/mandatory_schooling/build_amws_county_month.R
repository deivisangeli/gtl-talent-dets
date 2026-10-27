# Independent monthly outcome panel. All generated data stay in Dropbox.
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value=TRUE))
repo_root <- if (length(script)) normalizePath(file.path(dirname(script[1]), '../..'), winslash='/') else Sys.getenv('GTL_REPO', getwd())
source(file.path(repo_root, 'paths.R'))
source(file.path(repo_root, 'prep/mandatory_schooling/monthly_amws_helpers.R'))
source(file.path(repo_root, 'prep/mandatory_schooling/amws_panel_gender_helpers.R'))
cli <- commandArgs(TRUE)
source_arg <- grep('^--births-source=', cli, value=TRUE)
year_arg <- grep('^--years=',cli,value=TRUE)
if (length(source_arg)>1L || length(year_arg)>1L ||
    any(!grepl('^(--births-source=(nhgis|gapminder)|--years=all)$',cli))) {
  stop('Usage: build_amws_county_month.R [--births-source=nhgis|gapminder] [--years=all]')
}
births_source <- if (length(source_arg)) sub('^--births-source=','',source_arg) else 'nhgis'
gapminder <- births_source == 'gapminder'
all_years <- length(year_arg)>0L
if (all_years && !gapminder) stop('--years=all requires --births-source=gapminder')
birth_column <- if (gapminder) 'county_births_estimate_year' else 'annual_recorded_births'
dest <- file.path(DATA_OUTPUT, 'mandatory_schooling')
dir.create(dest, recursive=TRUE, showWarnings=FALSE)
save_audit <- function(x, name) {
  order_cols <- intersect(c('GEOID','GISJOIN','year','source','record_id'), names(x))
  if (nrow(x) && length(order_cols)) setorderv(x, order_cols)
  fwrite(x, file.path(audit, paste0(name,'.csv')), na='')
}
annual_file <- file.path(DATA_OUTPUT, 'us_panel_county_amws_combined_year.csv')
early_file <- file.path(AMWS_OUTPUT, 'amws_combined_us_geocoded.csv')
late_file <- file.path(TALENT_DETS_DATA_DIR, 'Data/processed/amws/amws_ed86_final.csv')
reference_file <- file.path(DATA_INPUT, 'nhgis0005_ts_nominal_county.csv')
gapminder_file <- file.path(DATA_INPUT, 'new_births_total_number_estimated.csv')
gender_file <- file.path(AMWS_OUTPUT, 'gender_inference/amws_gender_records.csv')
gender_checksums <- file.path(AMWS_OUTPUT, 'gender_inference/amws_gender_source_checksums.csv')
gender <- amws_load_gender(gender_file, gender_checksums, c(early_file, late_file, annual_file))
gender_before <- tools::md5sum(c(gender_file, gender_checksums, early_file, late_file, annual_file))
annual <- fread(annual_file)
annual[, GEOID := monthly_geoid(GEOID)]
annual[, state_id := substr(GEOID,1,2)]
contiguous_states <- sprintf('%02d', c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,
  24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56))
annual <- annual[state_id %in% contiguous_states]
universe <- sort(unique(annual$GEOID))
years <- if (all_years) seq.int(min(annual$year),max(annual$year)) else 1915:1950
year_bounds <- range(years)
range_tag <- paste(year_bounds,collapse='_')
annual_name <- if (gapminder) paste0('county_gapminder_annual_births_',range_tag,'.csv') else 'county_nhgis_annual_births_1915_1950.csv'
monthly_name <- if (gapminder) paste0('county_amws_monthly_gapminder_',range_tag,'.csv') else 'county_amws_monthly_1915_1950.csv'
stage_root <- tempfile('gender_monthly_stage_', tmpdir = dest)
dir.create(stage_root)
audit <- file.path(stage_root, if (gapminder) paste0('monthly_gapminder_',range_tag,'_audits') else 'monthly_1915_1950_audits')
dir.create(audit, showWarnings=FALSE)
annual <- annual[year %in% years]
if (anyDuplicated(annual[,.(GEOID,year)])) stop('Duplicate annual panel keys')

reference <- fread(reference_file, colClasses='character')
reference <- reference[grepl('^[0-9]{4}$', YEAR) & grepl('^[0-9]{2}$',STATEFP) & grepl('^[0-9]{3}$',COUNTYFP)]
reference[, `:=`(GEOID=paste0(STATEFP,COUNTYFP), year=as.integer(YEAR))]
# Detect observed historical name/unit changes; this is a flag, not harmonization.
geo_bounds <- if (all_years) c(floor(year_bounds[1]/10)*10,ceiling(year_bounds[2]/10)*10) else c(1910L,1950L)
geo_flags <- reference[GEOID %in% universe, .(
  first_nominal_year=min(year),
  historical_name_variants=uniqueN(tolower(trimws(NAME[year %between% geo_bounds]))),
  historical_names=paste(sort(unique(NAME[year %between% geo_bounds])),collapse=' | ')), by=GEOID]
geo_flags[, `:=`(known_geography_change=historical_name_variants > 1L,
                late_nominal_coverage=first_nominal_year > year_bounds[1])]
geo_flags[, geography_caution := ifelse(known_geography_change,
  'nominal unit/name change observed; boundaries not harmonized', 'historical boundaries not harmonized or fully verified')]
known_changes <- data.table(GEOID=c('13121','51810','36005','36047','36061','36081','36085'),
  geography_note=c('Fulton absorbed Campbell and Milton in 1932; pre-merger births not reallocated',
    'Modern Virginia Beach includes Princess Anne County after 1963; no historical reallocation',
    rep('NYC citywide aggregate cannot supply borough-specific denominators',5)),
  geography_source=c('https://fultoncountyga.gov/Inside-Fulton-County/About-Fulton-County/History',
    'https://www.lva.virginia.gov/collections/ccmf/VA/VA229',rep('NHGIS downloaded NT001 county tables',5)))
if (gapminder) known_changes <- known_changes[!GEOID %in% c('36005','36047','36061','36081','36085')]
geo_flags <- merge(geo_flags,known_changes,by='GEOID',all.x=TRUE)
geo_flags[!is.na(geography_note), `:=`(known_geography_change=TRUE,geography_caution=geography_note)]
save_audit(geo_flags,'geography_change_flags')

if (!gapminder) {
root <- file.path(TALENT_DETS_DATA_DIR,'raw/nhgis_births')
raw_births <- rbindlist(lapply(c('early','late'), function(label) read_occurrence_births(root,label)))
if (anyDuplicated(raw_births[,.(nhgis_dataset,geography,GISJOIN,year)])) stop('Duplicate NHGIS keys')
crosswalk <- match_birth_counties(raw_births[geography=='county'], reference, universe)
save_audit(crosswalk,'nhgis_county_crosswalk')
save_audit(crosswalk[county_match_status != 'matched_nominal_id'],'unmatched_or_ambiguous_counties')
overlap <- merge(raw_births[nhgis_dataset=='1915_1941_cVS' & year>=1939L,
  .(GISJOIN,year,geography,early_births=annual_recorded_births)],
  raw_births[nhgis_dataset=='1939_1959_cVS' & year<=1941L,
  .(GISJOIN,year,geography,late_births=annual_recorded_births)],
  by=c('GISJOIN','year','geography'), all=TRUE)
overlap[, `:=`(difference=late_births-early_births,
  discrepancy=(!is.na(early_births) & !is.na(late_births) & early_births!=late_births) |
    (is.na(early_births) != is.na(late_births)))]
overlap[, geography_note := fifelse(GISJOIN %in% c('G3600610','G3609610'),
  '1939-1941 NYC aggregate: early G3600610 corresponds to late G3609610, not Manhattan', '')]
save_audit(overlap,'nhgis_overlap_1939_1941')
state_births <- choose_birth_vintage(raw_births[geography=='state'])
state_births[, state_id := substr(STATEA,1,2)]
save_audit(state_births,'nhgis_state_annual_births')
definitions <- data.table(year=c(1915L,1935L),published_live_births=c(776304,2155105))
definitions <- merge(definitions,state_births[state_id %in% contiguous_states,
  .(nhgis_births=sum(annual_recorded_births,na.rm=TRUE)),by=year],by='year',all.x=TRUE)
definitions[, source := 'Census Birth, Stillbirth, and Infant Mortality Statistics 1935, Table B, printed p.4 (PDF p.8); footnote: exclusive of stillbirths']
definitions[, source_url := 'https://www.cdc.gov/nchs/data/vsushistorical/birthstat_1935.pdf']
save_audit(definitions,'live_birth_definition_verification')
stopifnot(all(definitions$published_live_births==definitions$nhgis_births))
chosen <- choose_birth_vintage(crosswalk[county_match_status=='matched_nominal_id'])
save_audit(chosen,'nhgis_matched_county_annual_births')
} else {
  national <- validate_gapminder_annual(annual, gapminder_file, years)
  save_audit(national,'gapminder_national_birth_verification')
}

early <- fread(early_file)
early[, source_row := .I]
early[, `:=`(GEOID=monthly_geoid(geoid), year=as.integer(birth_year))]
early <- early[monthly_kept(kept) & !is.na(GEOID) & year %in% years]
early <- early[,.(GEOID,year,source='1906_1955_dedup',record_id=paste(edition,lineid,sep=':'),date_text=birthplace_orig,
                  edition,source_row,source_record_id=as.character(lineid))]
late <- fread(late_file)
late[, source_row := .I]
late[, `:=`(GEOID=monthly_geoid(geo_geoid), year=as.integer(birth_year),
            contaminated=monthly_contaminated(raw_text_adjusted))]
late <- late[birth_country=='USA' & !is.na(GEOID) & year %in% years]
save_audit(late[contaminated==TRUE,.(GEOID,year,entry_id,birth_date,raw_text_adjusted)],'1986_contamination_exclusions')
late <- late[contaminated==FALSE,.(GEOID,year,source='1986',record_id=as.character(entry_id),date_text=birth_date,
                                  edition=1986L,source_row,source_record_id=as.character(entry_id))]
records <- rbindlist(list(cbind(early,parse_amws_month(early$date_text,early$year,early=TRUE)),
                         cbind(late,parse_amws_month(late$date_text,late$year))))
records[, in_county_universe := GEOID %in% universe]
legacy_record_cols <- c('GEOID','year','source','record_id','date_text','month','month_status','birth_segment','in_county_universe')
save_audit(records[in_county_universe==FALSE, ..legacy_record_cols],'scientists_outside_county_universe')
records <- records[in_county_universe==TRUE]
records <- amws_attach_gender(records, gender)
save_audit(records[, ..legacy_record_cols],'scientist_birth_month_assignments')
save_audit(records[is.na(month), ..legacy_record_cols],'scientists_unassigned_month')
save_audit(records, 'scientist_gender_assignments')
save_audit(records[is.na(month)], 'scientists_unassigned_month_by_gender')
save_audit(records[gender_reason_80 == 'no_saved_inference'], 'scientists_no_saved_gender_inference')
save_audit(records[, .(records=.N, assigned_month=sum(!is.na(month)), unassigned_month=sum(is.na(month))),
                   by=.(edition,gender_80,gender_reason_80)], 'gender_classification_coverage')

cy <- CJ(GEOID=universe,year=years)
cy <- merge(cy,annual[,.(GEOID,year,population,population_source,
  n_early_existing=n_amws_1906_1955_dedup,n_1986_existing=n_amws_1986)],by=c('GEOID','year'),all.x=TRUE)
if (gapminder) {
  cy <- merge(cy,annual[,.(GEOID,year,county_births_estimate_year)],by=c('GEOID','year'),all.x=TRUE)
  cy <- merge(cy,national,by='year',all.x=TRUE)
}
cy[, annual_panel_present := !is.na(n_early_existing)]
totals <- records[,.(n_early_year=sum(source=='1906_1955_dedup'),n_1986_year=sum(source=='1986'),
  n_early_assigned=sum(source=='1906_1955_dedup' & !is.na(month)),
  n_1986_assigned=sum(source=='1986' & !is.na(month)),
  n_unassigned_month=sum(is.na(month))),by=.(GEOID,year)]
cy <- merge(cy,totals,by=c('GEOID','year'),all.x=TRUE)
count_cols <- c('n_early_year','n_1986_year','n_early_assigned','n_1986_assigned','n_unassigned_month')
for (col in count_cols) set(cy,which(is.na(cy[[col]])),col,0L)
cy[, `:=`(early_reconciles=!annual_panel_present | n_early_existing==n_early_year,
           late_reconciles=!annual_panel_present | n_1986_existing==n_1986_year)]
save_audit(cy,'annual_scientist_reconciliation')
if (any(!cy$early_reconciles | !cy$late_reconciles)) stop('Source counts differ from annual panel; inspect reconciliation audit')
cy[, `:=`(birth_month_completeness=ifelse(n_early_year+n_1986_year>0,
  (n_early_assigned+n_1986_assigned)/(n_early_year+n_1986_year),NA_real_),
  birth_month_complete=n_unassigned_month==0L)]
cy[, `:=`(n_early_unassigned=n_early_year-n_early_assigned,n_1986_unassigned=n_1986_year-n_1986_assigned,
  early_month_completeness=ifelse(n_early_year>0,n_early_assigned/n_early_year,NA_real_),
  late_month_completeness=ifelse(n_1986_year>0,n_1986_assigned/n_1986_year,NA_real_))]
if (!gapminder) {
cy <- merge(cy,chosen[,.(GEOID,year,annual_recorded_births,GISJOIN,county_match_status,
  nhgis_dataset,nhgis_table,nhgis_variable,nhgis_file)],by=c('GEOID','year'),all.x=TRUE)
cy[is.na(county_match_status),county_match_status:='no_matched_nhgis_record']
cy[, birth_denominator_status := fifelse(is.na(annual_recorded_births),'missing',
  fifelse(annual_recorded_births==0,'recorded_zero','observed_positive'))]
} else {
  cy[, `:=`(birth_denominator_source='gapminder_us_births_x_county_population_share',
    birth_denominator_status=fifelse(is.na(county_births_estimate_year),'missing',
      fifelse(county_births_estimate_year<=0,'nonpositive','estimated_positive')),
    gapminder_input_file=basename(gapminder_file),monthly_allocation='annual_estimate_divided_by_12')]
}
cy <- merge(cy,geo_flags,by='GEOID',all.x=TRUE)
cy[, `:=`(state_id=substr(GEOID,1,2),monthly_allocated_births=get(birth_column)/12)]
if (gapminder) cy[, county_births_estimate_month := monthly_allocated_births]
setorder(cy,GEOID,year)
cy <- amws_add_gender_annual(cy, records)
fwrite(cy,file.path(stage_root,annual_name),na='')
save_audit(cy[, c('GEOID','year',amws_gender_columns(names(cy))), with=FALSE], 'gender_annual_reconciliation')
no_inference <- records[gender_reason_80 == 'no_saved_inference', .(records=.N), by=.(GEOID,year)]
no_inference <- merge(no_inference, cy[,.(GEOID,year,annual_panel_present,population,
  annual_birth_denominator=get(birth_column))], by=c('GEOID','year'), all.x=TRUE)
save_audit(no_inference, 'no_saved_inference_denominator_coverage')

counts <- records[!is.na(month),.(n_amws_1906_1955_dedup=sum(source=='1906_1955_dedup'),
  n_amws_1986=sum(source=='1986')),by=.(GEOID,year,month)]
counts <- merge(counts, amws_gender_month_counts(records), by=c('GEOID','year','month'), all=TRUE)
county_batches <- split(universe,ceiling(seq_along(universe)/100L))
reconciliations <- vector('list',length(county_batches))
gender_reconciliations <- vector('list',length(county_batches))
total_rows <- 0L
total_scientists <- 0L
monthly_file <- file.path(stage_root,monthly_name)
for (batch_index in seq_along(county_batches)) {
county_ids <- county_batches[[batch_index]]
batch_cy <- cy[GEOID %in% county_ids]
monthly <- batch_cy[rep(seq_len(.N),each=12L)]
monthly[, month := rep(1:12,times=nrow(batch_cy))]
monthly <- merge(monthly,counts,by=c('GEOID','year','month'),all.x=TRUE)
for (col in c('n_amws_1906_1955_dedup','n_amws_1986',amws_gender_period_cols())) set(monthly,which(is.na(monthly[[col]])),col,0L)
monthly[, `:=`(n_amws=n_amws_1906_1955_dedup+n_amws_1986,
  month_start=as.IDate(sprintf('%04d-%02d-01',year,month)))]
monthly[, `:=`(amws_per_1000_births=monthly_rate(n_amws,monthly_allocated_births),
              amws_per_1000_pop=monthly_rate(n_amws,population))]
monthly[, c('n_early_existing','n_1986_existing','early_reconciles','late_reconciles') := NULL]
monthly <- amws_add_gender_rates(monthly, 'monthly_allocated_births')
setorder(monthly,GEOID,year,month)
months_per_county <- length(years)*12L
if (anyDuplicated(monthly[,.(GEOID,year,month)]) || nrow(monthly)!=months_per_county*length(county_ids) ||
    any(monthly[, .N,by=GEOID]$N !=months_per_county) ||
    !all(monthly$month %in% 1:12)) stop('Monthly grid validation failed')
recon <- monthly[,.(monthly_birth_sum=if(all(is.na(monthly_allocated_births))) NA_real_ else sum(monthly_allocated_births),
  annual_births=get(birth_column)[1],early_month_sum=sum(n_amws_1906_1955_dedup),
  early_assigned=n_early_assigned[1],late_month_sum=sum(n_amws_1986),late_assigned=n_1986_assigned[1]),by=.(GEOID,year)]
stopifnot(all(is.na(recon$annual_births) | abs(recon$monthly_birth_sum-recon$annual_births)<1e-7),
          all(recon$early_month_sum==recon$early_assigned),all(recon$late_month_sum==recon$late_assigned))
verify_monthly_rates(monthly$n_amws,monthly$monthly_allocated_births,monthly$amws_per_1000_births)
reconciliations[[batch_index]] <- recon
gender_reconciliations[[batch_index]] <- amws_gender_reconciliation(monthly, batch_cy, 'monthly')
total_rows <- total_rows+nrow(monthly)
total_scientists <- total_scientists+sum(monthly$n_amws)
fwrite(monthly,monthly_file,na='',append=batch_index>1L)
if (all_years && (batch_index %% 5L == 0L || batch_index==length(county_batches))) {
  message('Wrote counties ',min(batch_index*100L,length(universe)),'/',length(universe))
}
}
if (total_rows!=length(universe)*length(years)*12L || total_scientists!=sum(cy$n_early_assigned+cy$n_1986_assigned)) {
  stop('Full monthly grid/count totals failed')
}
recon <- rbindlist(reconciliations)
save_audit(recon,'monthly_annual_reconciliation')
save_audit(rbindlist(gender_reconciliations), 'gender_monthly_annual_reconciliation')
save_audit(cy[, .(scientists=sum(n_early_year+n_1986_year),
  male=sum(n_early_year_male_80+n_1986_year_male_80),
  female=sum(n_early_year_female_80+n_1986_year_female_80),
  unclassified=sum(n_early_year_unclassified_80+n_1986_year_unclassified_80)), by=.(state_id,year)],
  'gender_coverage_state_year')
coverage <- cy[,.(county_years=.N,observed_births=sum(!is.na(get(birth_column))),
  zero_births=sum(get(birth_column)==0,na.rm=TRUE),missing_births=sum(is.na(get(birth_column))),
  scientists=sum(n_early_year+n_1986_year),assigned_scientists=sum(n_early_assigned+n_1986_assigned),
  unassigned_scientists=sum(n_unassigned_month)),by=.(state_id,year)]
if (!gapminder) {
county_state <- cy[,.(matched_county_births=sum(annual_recorded_births,na.rm=TRUE),
                      matched_counties_observed=sum(!is.na(annual_recorded_births))),by=.(state_id,year)]
county_state <- merge(county_state,state_births[,.(state_id,year,state_recorded_births=annual_recorded_births)],
  by=c('state_id','year'),all.x=TRUE)
county_state[, state_minus_matched_counties := state_recorded_births-matched_county_births]
save_audit(county_state,'state_county_birth_coverage')
} else {
  setnames(coverage,'observed_births','observed_estimated_births')
  usable <- cy[,.(usable_denominators=sum(county_births_estimate_year>0,na.rm=TRUE)),by=.(state_id,year)]
  coverage <- merge(coverage,usable,by=c('state_id','year'))
  verify_monthly_rates(monthly$n_amws,monthly$monthly_allocated_births,monthly$amws_per_1000_births)
}
save_audit(coverage,'coverage_state_year')
save_audit(coverage[,lapply(.SD,sum),by=year,.SDcols=setdiff(names(coverage),c('state_id','year'))],'coverage_year')
inputs <- c(annual_file,early_file,late_file,reference_file,
  if (gapminder) gapminder_file else list.files(root,'_csv[.]zip$',recursive=TRUE,full.names=TRUE),
  gender_file,gender_checksums)
fwrite(data.table(file=normalizePath(inputs,winslash='/'),md5=unname(tools::md5sum(inputs))),file.path(audit,'input_manifest.csv'))
message('Validating staged outputs against every original panel column...')
amws_verify_staged_panel(file.path(stage_root,annual_name), file.path(dest,annual_name), nrow(cy))
amws_verify_staged_panel(monthly_file, file.path(dest,monthly_name), total_rows)
if (!identical(gender_before, tools::md5sum(c(gender_file,gender_checksums,early_file,late_file,annual_file)))) {
  stop('Gender or scientist input hashes changed during the build')
}
amws_install_staged(stage_root, dest)
message('Wrote ',total_rows,' rows; ',length(universe),' counties; ',total_scientists,
  ' assigned scientist births; ',sum(cy$n_unassigned_month),' unassigned; ',
  sum(!is.na(cy[[birth_column]]) & cy[[birth_column]]>0), ' usable ',births_source,' county-year birth denominators.')
