# Run from anywhere, with TALENT_DETS_DATA_DIR pointing to the local Dropbox root:
# Rscript analysis/mandatory_schooling/estimate_compulsory_schooling_amws_monthly_rd.R
arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)
here <- if (length(arg)) dirname(normalizePath(sub('^--file=','',arg[1]),winslash='/')) else
  file.path(Sys.getenv('GTL_REPO',unset=getwd()),'analysis/mandatory_schooling')
source(file.path(here,'monthly_state_rd_helpers.R'))
source(file.path(here,'monthly_state_rd_law_research.R'))
source(file.path(here,'monthly_state_rd_report.R'))
repo <- msrd_root(); paths <- msrd_paths(repo)
msrd_require(!length(commandArgs(TRUE)), 'This script takes no arguments; configure TALENT_DETS_DATA_DIR.')
inputs <- unlist(paths[c('monthly','annual','assignments','adoption','policy','onset')],use.names=FALSE)
msrd_require(all(file.exists(inputs)), 'Missing monthly panel, annual support, scientist assignments, or law inputs.')
dir.create(paths$results,recursive=TRUE,showWarnings=FALSE)
dir.create(paths$research,recursive=TRUE,showWarnings=FALSE)
start <- Sys.time()
write_out <- function(d,name) fwrite(d,file.path(paths$results,name),na='')

# Preserve input hashes and existing result-directory metadata. All writes below
# are explicitly confined to the new results/research directories. Avoid walking
# large online-only Dropbox result caches simply to inventory unrelated models.
old_results_root <- file.path(paths$root,'results/mandatory_schooling')
old_results <- list.files(old_results_root,full.names=TRUE,recursive=FALSE)
old_results <- old_results[normalizePath(old_results,winslash='/',mustWork=FALSE)!=paths$results]
old_info <- file.info(old_results)
old_metadata <- data.table(file=old_results,size=old_info$size,mtime=as.character(old_info$mtime))
protected_repo <- list.files(here,pattern='\\.(R|md)$',full.names=TRUE)
protected_repo <- protected_repo[!basename(protected_repo) %in% c(
  'monthly_state_rd_helpers.R','monthly_state_rd_law_research.R',
  'estimate_compulsory_schooling_amws_monthly_rd.R','test_monthly_state_rd.R',
  'monthly_state_rd_report.R','monthly_state_rd.md')]
protected <- unique(c(inputs,protected_repo,
  file.path(paths$root,c('output/amws/amws_combined_us_geocoded.csv',
                        'Data/processed/amws/amws_ed86_final.csv',
                        'output/us_panel_county_amws_combined_year.csv'))))
protected <- protected[file.exists(protected)]
message('Hashing protected source files...')
before <- data.table(file=protected,md5_before=unname(tools::md5sum(protected)))
write_out(before,'protected_input_manifest.csv')
write_out(old_metadata,'existing_results_manifest.csv')

if (!file.exists(paths$overrides)) fwrite(msrd_researched_laws(),paths$overrides,na='')
overrides <- fread(paths$overrides,colClasses=c(state_fips='character'))
research_before <- tools::md5sum(paths$overrides)
adoption <- fread(paths$adoption,colClasses=c(state_fips='character'))
policy <- fread(paths$policy)
onset <- fread(paths$onset)
inventory <- msrd_inventory(adoption,policy,onset,overrides)
fwrite(inventory,file.path(paths$research,'first_statewide_law_cutoff_inventory.csv'),na='')
write_out(inventory,'cutoff_inventory.csv')
policy_events <- msrd_policy_events(policy)
write_out(policy_events,'statutory_policy_changes.csv')

message('Reading seven columns of the full monthly panel...')
count_columns <- c('n_amws','n_amws_1906_1955_dedup','n_amws_1986')
m <- fread(paths$monthly,select=c('GEOID','year','month',count_columns,'monthly_allocated_births'),
           colClasses=c(GEOID='character'))
msrd_validate_panel(m)
msrd_require(nrow(m)==5973744L && uniqueN(m$GEOID)==3092L &&
               identical(sort(unique(m$year)),1800:1960), 'Unexpected full-period panel universe.')
m[, `:=`(state_fips=substr(GEOID,1L,2L),cohort_index=12L*year+month-1L)]

annual <- fread(paths$annual,select=c('GEOID','year','county_births_estimate_year',
  'n_early_year','n_1986_year','n_early_assigned','n_1986_assigned',
  'n_early_unassigned','n_1986_unassigned'),colClasses=c(GEOID='character'))
msrd_require(!anyDuplicated(annual[,.(GEOID,year)]), 'Duplicate annual support keys.')
annual[,state_fips:=substr(GEOID,1L,2L)]
reconciliation <- m[,.(monthly_births=sum(monthly_allocated_births),
  early=sum(n_amws_1906_1955_dedup),late=sum(n_amws_1986)),by=.(GEOID,year)]
reconciliation <- merge(reconciliation,annual,by=c('GEOID','year'),all=TRUE)
msrd_require(nrow(reconciliation)==nrow(annual), 'Monthly/annual key mismatch.')
msrd_require(all(reconciliation$early==reconciliation$n_early_assigned) &&
               all(reconciliation$late==reconciliation$n_1986_assigned), 'Monthly counts do not reconcile to assigned annual counts.')
good <- is.finite(reconciliation$county_births_estimate_year)
msrd_require(all(abs(reconciliation$monthly_births[good]-reconciliation$county_births_estimate_year[good]) <
                   1e-8*pmax(1,reconciliation$county_births_estimate_year[good])) &&
               all(is.na(reconciliation$monthly_births[!good])), 'Birth denominators do not reconcile.')
write_out(data.table(county_years=nrow(reconciliation),early_assigned=sum(reconciliation$early),
  ed1986_assigned=sum(reconciliation$late),assigned_total=sum(reconciliation$early+reconciliation$late),
  month_count_reconciliation=TRUE,denominator_reconciliation=TRUE),'panel_validation.csv')
rm(reconciliation);invisible(gc(verbose=FALSE))

assignments <- fread(paths$assignments,colClasses=c(GEOID='character'))
assignments[, `:=`(state_fips=substr(GEOID,1L,2L),
  edition=fifelse(source=='1986','1986',sub(':.*','',record_id)))]
edition <- assignments[,.(records=.N,assigned_month=sum(!is.na(month)),
  unassigned_month=sum(is.na(month)),assigned_share=mean(!is.na(month)),
  first_birth=min(year),last_birth=max(year)),by=edition]
setorder(edition,edition);write_out(edition,'birth_month_coverage_by_edition.csv')
coverage <- assignments[,.(records=.N,assigned_month=sum(!is.na(month)),
  unassigned_month=sum(is.na(month)),assigned_share=mean(!is.na(month))),by=.(state_fips,year,edition)]
write_out(coverage,'birth_month_coverage_state_year_edition.csv')

estimate_rows <- list();point_rows <- list();county_rows <- list();diagnostic_rows <- list()
event_rows <- list();overlap_rows <- list();plots <- list()
dir.create(file.path(paths$results,'states'),showWarnings=FALSE)
for (i in seq_len(nrow(inventory))) {
  event <- inventory[i]
  message(sprintf('[%02d/51] %s: %s',i,event$state,event$precision))
  if (!is.finite(event$cutoff_coordinate)) {
    event_rows[[length(event_rows)+1L]] <- data.table(state_fips=event$state_fips,state=event$state,
      precision=event$precision,status=event$research_status,retained_counties=0L,excluded_counties=0L)
    next
  }
  for (h in c(72L,36L,120L)) {
    win <- msrd_state_window(m,event,h)
    county_rows[[length(county_rows)+1L]] <- win$audit
    d <- win$data
    nearby <- policy_events[state_fips==event$state_fips & reform_year!=event$law_year &
      abs(12L*approximate_affected_cohort-event$cutoff_coordinate)<h]
    if (nrow(nearby)) {
      nearby[,`:=`(bandwidth_months=h,first_law_year=event$law_year)]
      overlap_rows[[length(overlap_rows)+1L]] <- nearby
    }
    for (source_column in count_columns) {
      lags <- if (h==72L) c(12L,6L,24L) else 12L
      for (lag in lags) {
        fit <- msrd_fit(d,source_column,h,lag)
        row <- fit$summary
        row[, `:=`(state_fips=event$state_fips,state=event$state,state_abbr=event$state_abbr,
          law_year=event$law_year,canonical_law_year=event$canonical_law_year,
          cutoff_date=event$cutoff_date,precision=event$precision,
          retained_counties=sum(win$audit$retained),excluded_counties=sum(!win$audit$retained),
          policy_overlap=nrow(nearby)>0L,earlier_partial_coverage=event$earlier_partial_coverage,
          historical_repeal=event$historical_repeal,source_coverage_boundary=FALSE)]
        # A source's observed first/last cohort or an edition turnover lies inside the window.
        source_editions <- if (source_column=='n_amws_1986') '1986' else if
          (source_column=='n_amws_1906_1955_dedup') c('1906','1938','1955') else edition$edition
        bounds <- edition[edition %in% source_editions]
        boundaries <- c(12L*bounds$first_birth,12L*(bounds$last_birth+1L))
        row[,source_coverage_boundary:=any(abs(boundaries-event$cutoff_coordinate)<h)]
        estimate_rows[[length(estimate_rows)+1L]] <- row
        if (h==72L && source_column=='n_amws' && lag==12L) {
          event_rows[[length(event_rows)+1L]] <- row[,.(state_fips,state,precision,status,retained_counties,excluded_counties)]
          if (nrow(d)) {
            point_rows[[length(point_rows)+1L]] <- d
            p <- msrd_plot_state(d,fit,event)
            ggsave(file.path(paths$results,'states',paste0(event$state_abbr,'.png')),p,
                    width=10,height=6,dpi=160,bg='white')
            plots[[event$state_abbr]] <- p
          }
        }
      }
    }
    if (nrow(d)) {
      cy <- annual[GEOID %in% win$retained_counties &
        year %in% d$year,.(early_records=sum(n_early_year),late_records=sum(n_1986_year),
          early_assigned=sum(n_early_assigned),late_assigned=sum(n_1986_assigned),
          early_unassigned=sum(n_early_unassigned),late_unassigned=sum(n_1986_unassigned)),by=year]
      cy[, `:=`(state_fips=event$state_fips,state=event$state,bandwidth_months=h,
        birth_month_completeness=fifelse(early_records+late_records>0,
          (early_assigned+late_assigned)/(early_records+late_records),NA_real_),
        early_share=fifelse(early_assigned+late_assigned>0,early_assigned/(early_assigned+late_assigned),NA_real_))]
      diagnostic_rows[[length(diagnostic_rows)+1L]] <- cy
    }
  }
}
estimates <- rbindlist(estimate_rows,fill=TRUE)
points <- rbindlist(point_rows,fill=TRUE)
counties <- rbindlist(county_rows,fill=TRUE)
diagnostics <- rbindlist(diagnostic_rows,fill=TRUE)
states <- rbindlist(event_rows,fill=TRUE)
overlaps <- rbindlist(overlap_rows,fill=TRUE)
msrd_require(nrow(states)==51L && !anyDuplicated(states$state_fips), 'Missing state dispositions.')
primary <- estimates[source=='combined' & bandwidth_months==72L & hac_lag==12L]
primary[,p_adjusted_bh:=NA_real_]
for (kind in c('verified_monthly','annual_approximation')) {
  idx <- which(primary$precision==kind & primary$status=='estimated')
  if (length(idx)) set(primary,idx,'p_adjusted_bh',p.adjust(primary$p_value[idx],method='BH'))
}
for (name in c('cutoff_date','precision','state')) msrd_require(!anyNA(primary[[name]]),'Missing primary metadata.')
write_out(estimates,'state_estimates_all_specifications.csv')
write_out(primary,'primary_state_estimates.csv')
write_out(primary[precision=='verified_monthly'],'primary_verified_monthly.csv')
write_out(primary[precision=='annual_approximation'],'primary_annual_approximations.csv')
write_out(points,'state_monthly_plot_points.csv')
write_out(counties,'county_window_coverage_audit.csv')
write_out(diagnostics,'window_source_completeness_diagnostics.csv')
write_out(states,'all_state_dispositions.csv')
write_out(overlaps,'overlapping_policy_changes.csv')

for (kind in c('verified_monthly','annual_approximation')) {
  d <- primary[precision==kind & status=='estimated']
  if (!nrow(d)) next
  d[,state_label:=factor(state,levels=state[order(estimate)])]
  p <- ggplot(d,aes(estimate,state_label)) + geom_vline(xintercept=0,linetype='dashed',colour='grey50') +
    geom_segment(aes(x=ci_low,xend=ci_high,yend=state_label),linewidth=.55) +
    geom_point(aes(colour=sparse),size=2.2) +
    scale_colour_manual(values=c('FALSE'='#174a75','TRUE'='#d97720'),labels=c('FALSE'='At least 20 scientists per side','TRUE'='Sparse (<20 on a side)')) +
    labs(title=paste('Monthly AMWS cohort discontinuities:',gsub('_',' ',kind)),
         subtitle='Six years per side; seasonal local linear fit; HAC 12; unadjusted 95% intervals',
         x='Jump in AMWS per 1,000 estimated births',y=NULL,colour=NULL,
         caption='Reduced-form cohort breaks. Legal exposure can ramp gradually; monthly birth seasonality is not in the denominator.') +
    theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),legend.position='bottom')
  ggsave(file.path(paths$results,paste0('state_comparison_',kind,'.png')),p,
          width=11,height=max(5,2+.23*nrow(d)),dpi=160,bg='white')
}
if (nrow(diagnostics)) {
  p <- ggplot(diagnostics[bandwidth_months==72L & is.finite(birth_month_completeness)],aes(year,birth_month_completeness)) +
    geom_line(colour='#174a75')+geom_point(size=.8)+facet_wrap(~state,scales='free_x',ncol=7) +
    scale_x_continuous(n.breaks=3,labels=function(x) format(round(x),trim=TRUE))+
    scale_y_continuous(limits=c(0,1))+labs(title='Birth-month assignment completeness in each primary state window',
      x='Birth year',y='Share with an assigned birth month',caption='Fixed retained counties; both sources; no redistribution of missing dates.') +
    theme_minimal(base_size=9)+theme(panel.grid.minor=element_blank())
  ggsave(file.path(paths$results,'state_month_completeness.png'),p,width=20,height=16,dpi=140,bg='white')
  p <- ggplot(diagnostics[bandwidth_months==72L & is.finite(early_share)],aes(year,early_share))+
    geom_line(colour='#174a75')+geom_point(size=.8)+facet_wrap(~state,scales='free_x',ncol=7)+
    scale_x_continuous(n.breaks=3,labels=function(x) format(round(x),trim=TRUE))+
    scale_y_continuous(limits=c(0,1))+labs(title='Early-edition share of assigned AMWS births',
      x='Birth year',y='1906/1938/1955 share',caption='The complement is 1986; sources have not been deduplicated against each other.')+
    theme_minimal(base_size=9)+theme(panel.grid.minor=element_blank())
  ggsave(file.path(paths$results,'state_source_composition.png'),p,width=20,height=16,dpi=140,bg='white')
}

# Document the later statewide reintroductions without replacing the earliest episodes.
supplement <- copy(inventory[state_fips %in% c('04','48','53')])
supplement[state_fips=='04',`:=`(law_year=1899L,entry_age=8L,exit_age=14L,precision='annual_approximation',
                                effective_date=NA_character_,cutoff_date='1886-01-01')]
supplement[state_fips=='48',`:=`(law_year=1916L,entry_age=8L,exit_age=14L,precision='verified_monthly',
                                effective_date='1916-09-01',cutoff_date='1902-09-01')]
supplement[state_fips=='53',`:=`(law_year=1886L,entry_age=policy[statefip==53 & year==1886,entryage],
                                exit_age=policy[statefip==53 & year==1886,exitage],precision='annual_approximation')]
supplement[state_fips=='53',cutoff_date:=sprintf('%04d-01-01',law_year-exit_age+1L)]
supplement[,cutoff_coordinate:=msrd_date_coordinate(cutoff_date)]
sup_rows <- list();sup_points <- list()
dir.create(file.path(paths$results,'reintroductions'),showWarnings=FALSE)
for (i in seq_len(nrow(supplement))) {
  event <- supplement[i]
  win <- msrd_state_window(m,event,72L); fit <- msrd_fit(win$data,'n_amws',72L,12L)
  row <- fit$summary
  row[,`:=`(state_fips=event$state_fips,state=event$state,law_year=event$law_year,
            cutoff_date=event$cutoff_date,precision=event$precision,event_role='later_statewide_reintroduction',
            timing_source='CLS Data Appendix pp.3,6; original adoption-year CLS ages; Texas 1915 Tex. Gen. Laws 92 specifies September 1 1916.')]
  sup_rows[[i]]<-row
  if (nrow(win$data)) {
    sup_points[[i]]<-win$data
    ggsave(file.path(paths$results,'reintroductions',paste0(event$state_abbr,'.png')),
      msrd_plot_state(win$data,fit,event),width=10,height=6,dpi=160,bg='white')
  }
}
write_out(rbindlist(sup_rows),'later_reintroduction_estimates.csv')
write_out(rbindlist(sup_points,fill=TRUE),'later_reintroduction_monthly_points.csv')

after <- unname(tools::md5sum(protected))
before[,`:=`(md5_after=after,unchanged=md5_before==after)]
msrd_require(all(before$unchanged), 'A protected input or existing analysis changed during execution.')
msrd_require(identical(unname(research_before),unname(tools::md5sum(paths$overrides))), 'Manual law overrides changed during execution.')
new_info <- file.info(old_results)
old_metadata[,unchanged:=size==new_info$size & mtime==as.character(new_info$mtime)]
msrd_require(all(old_metadata$unchanged), 'An existing result changed during execution.')
write_out(before,'protected_input_manifest.csv');write_out(old_metadata,'existing_results_manifest.csv')
write_out(data.table(check=c('monthly_grid','source_reconciliation','denominator_reconciliation',
  'all_state_dispositions','protected_input_hashes','existing_result_metadata','manual_research_preserved'),passed=TRUE),
  'validation_checks.csv')
write_out(data.table(states=51L,estimated_states=sum(primary$status=='estimated'),
  verified_monthly=sum(primary$precision=='verified_monthly' & primary$status=='estimated'),
  annual_approximations=sum(primary$precision=='annual_approximation' & primary$status=='estimated'),
  informative_early_states=sum(estimates[bandwidth_months==72 & hac_lag==12 & source=='early',status]=='estimated'),
  informative_1986_states=sum(estimates[bandwidth_months==72 & hac_lag==12 & source=='1986',status]=='estimated'),
  unadjusted_p_below_05=sum(primary$p_value<.05,na.rm=TRUE),bh_p_below_05=sum(primary$p_adjusted_bh<.05,na.rm=TRUE),
  elapsed_seconds=as.numeric(difftime(Sys.time(),start,units='secs'))),'run_summary.csv')
msrd_write_report(paths$results,primary,estimates,states,edition,inventory)
message('Finished: ',paths$results)
print(primary[,.(state,precision,status,estimate,se,p_value)])
