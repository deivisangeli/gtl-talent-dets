# Annual-outcome parallel to the monthly first-law RD, including known-year records
# without a birth month. Default: ten complete pre and ten complete post cohorts.
arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)
here <- dirname(normalizePath(sub('^--file=','',arg[1]),winslash='/'))
source(file.path(here,'yearly_state_rd_helpers.R'))
source(file.path(here,'monthly_state_rd_helpers.R'))
source(file.path(here,'render_yearly_state_rd_latex.R'))
repo <- msrd_root();paths <- msrd_paths(repo)
primary_years <- as.integer(Sys.getenv('ANNUAL_RD_YEARS_PER_SIDE',unset='10'))
ysrd_require(primary_years %in% c(6L,10L,20L),'Choose ANNUAL_RD_YEARS_PER_SIDE=6, 10 or 20.')
out_dir <- file.path(paths$root,'results/mandatory_schooling/compulsory_schooling_amws_yearly_rd')
dir.create(file.path(out_dir,'states'),recursive=TRUE,showWarnings=FALSE)
write_out <- function(d,name) fwrite(d,file.path(out_dir,name),na='')
inventory_path <- file.path(paths$results,'cutoff_inventory.csv')
existing <- c(paths$annual,paths$policy,list.files(paths$results,recursive=TRUE,full.names=TRUE))
existing <- existing[file.exists(existing) & !file.info(existing)$isdir]
before <- data.table(file=existing,md5_before=unname(tools::md5sum(existing)))
inventory <- fread(inventory_path,colClasses=c(state_fips='character'))
ysrd_require(nrow(inventory)==51L && !anyDuplicated(inventory$state_fips),'Expected all 51 jurisdictions.')
a <- fread(paths$annual,select=c('GEOID','year','county_births_estimate_year','n_early_year',
  'n_1986_year','n_early_assigned','n_1986_assigned'),colClasses=c(GEOID='character'))
setnames(a,c('county_births_estimate_year','n_early_year','n_1986_year'),c('births','n_early','n_1986'))
a[,`:=`(state_fips=substr(GEOID,1,2),n_amws=n_early+n_1986,n_assigned=n_early_assigned+n_1986_assigned)]
ysrd_require(all(grepl('^[0-9]{5}$',a$GEOID)) && !anyDuplicated(a[,.(GEOID,year)]),'Invalid annual county-year keys.')
ysrd_require(nrow(a)==497812L && uniqueN(a$GEOID)==3092L && identical(sort(unique(a$year)),1800:1960),
  'Unexpected annual panel universe.')
ysrd_require(all(is.finite(a$n_amws) & a$n_amws>=0 & a$n_amws==floor(a$n_amws)) &&
  all(a$n_assigned>=0 & a$n_assigned<=a$n_amws),'Invalid annual scientist counts.')
edition <- fread(file.path(paths$results,'birth_month_coverage_by_edition.csv'))
ysrd_require(sum(a$n_amws)==sum(edition$records) && sum(a$n_assigned)==sum(edition$assigned_month),
  'Annual source counts do not reconcile to the source records.')
support <- ysrd_support(a,inventory)
write_out(support$states,'available_years_by_state.csv')
write_out(support$counties,'available_years_by_county.csv')
write_out(inventory,'cutoff_inventory.csv')
policy_events <- msrd_policy_events(fread(paths$policy))
estimate_rows <- list(); point_rows <- list(); county_rows <- list();dispositions <- list();overlaps <- list()
for (i in seq_len(nrow(inventory))) {
  ev <- inventory[i]
  message(sprintf('[%02d/51] %s',i,ev$state))
  if (is.na(ev$cutoff_date) || ev$precision=='excluded') {
    dispositions[[i]] <- data.table(state_fips=ev$state_fips,state=ev$state,precision=ev$precision,
      status=if (ev$precision=='excluded') 'outside_annual_panel' else 'unresolved_cutoff',retained_counties=0L)
    next
  }
  bounds <- ysrd_event(ev)
  for (h in c(6L,10L,20L)) {
    win <- ysrd_window(a,ev,h)
    county_rows[[length(county_rows)+1L]] <- win$audit
    nearby <- policy_events[state_fips==ev$state_fips & reform_year!=ev$law_year &
      approximate_affected_cohort>=bounds$last_pre-h+1L & approximate_affected_cohort<=bounds$first_post+h-1L]
    if (nrow(nearby)) {nearby[,years_per_side:=h];overlaps[[length(overlaps)+1L]]<-nearby}
    for (column in c('n_amws','n_early','n_1986')) for (lag in if (h==primary_years) c(1L,0L,2L) else 1L) {
      fit <- ysrd_fit(win$data,column,lag)
      result <- fit$summary
      sources <- if (column=='n_1986') '1986' else if (column=='n_early') c('1906','1938','1955') else edition$edition
      bd <- edition[edition %in% sources]
      source_boundary <- any(c(bd$first_birth,bd$last_birth+1L)>bounds$last_pre-h+1L &
        c(bd$first_birth,bd$last_birth+1L)<bounds$first_post+h)
      result[,`:=`(state_fips=ev$state_fips,state=ev$state,state_abbr=ev$state_abbr,years_per_side=h,
        cutoff_date=as.character(ev$cutoff_date),precision=ev$precision,law_year=ev$law_year,
        mixed_year_omitted=bounds$mixed_year,retained_counties=sum(win$audit$retained),
        excluded_counties=sum(!win$audit$retained),policy_overlap=nrow(nearby)>0L,
        source_coverage_boundary=source_boundary,historical_repeal=ev$historical_repeal,
        earlier_partial_coverage=ev$earlier_partial_coverage)]
      estimate_rows[[length(estimate_rows)+1L]]<-result
      if (h==primary_years && column=='n_amws' && lag==1L) {
        dispositions[[i]]<-result[,.(state_fips,state,precision,status,retained_counties)]
        if (nrow(win$data)) {
          point_rows[[length(point_rows)+1L]]<-win$data
          ggsave(file.path(out_dir,'states',paste0(ev$state_abbr,'.png')),ysrd_plot(win$data,fit,ev),
            width=10,height=6,dpi=160,bg='white')
        }
      }
    }
  }
}
estimates <- rbindlist(estimate_rows,fill=TRUE)
primary <- estimates[source=='combined' & years_per_side==primary_years & hac_lag==1L]
primary[,p_adjusted_bh:=NA_real_]
for (kind in c('verified_monthly','annual_approximation')) {
  ids <- which(primary$precision==kind & primary$status=='estimated')
  if (length(ids)) set(primary,ids,'p_adjusted_bh',p.adjust(primary$p_value[ids],method='BH'))
}
coverage <- merge(rbindlist(dispositions,fill=TRUE),support$states,by=c('state_fips','state'),all.x=TRUE)
ysrd_require(nrow(coverage)==51L && !anyDuplicated(coverage$state_fips),'Missing annual state dispositions.')
points <- rbindlist(point_rows,fill=TRUE)
audit <- rbindlist(county_rows,fill=TRUE)
write_out(primary,'primary_state_estimates.csv')
write_out(estimates,'state_estimates_all_specifications.csv')
write_out(points,'state_yearly_plot_points.csv')
write_out(coverage,'all_state_dispositions.csv')
write_out(audit,'county_window_coverage_audit.csv')
write_out(rbindlist(overlaps,fill=TRUE),'overlapping_policy_changes.csv')
write_out(data.table(all_records=sum(a$n_amws),month_assigned=sum(a$n_assigned),
  additional_known_year_records=sum(a$n_amws-a$n_assigned)), 'source_count_reconciliation.csv')
for (kind in c('verified_monthly','annual_approximation')) {
  d <- primary[precision==kind & status=='estimated']
  if (!nrow(d)) next
  d[,state_label:=factor(state,levels=state[order(estimate)])]
  plot <- ggplot(d,aes(estimate,state_label))+geom_vline(xintercept=0,linetype='dashed',colour='grey50')+
    geom_segment(aes(x=ci_low,xend=ci_high,yend=state_label),linewidth=.55)+geom_point(aes(colour=sparse),size=2.2)+
    scale_colour_manual(values=c('FALSE'='#174a75','TRUE'='#d97720'),
      labels=c('FALSE'='At least 20 scientists per side','TRUE'='Sparse (<20 on a side)'))+
    labs(title=paste('Yearly AMWS cohort discontinuities:',gsub('_',' ',kind)),
      subtitle=sprintf('%s complete years per side; annual local linear fit; HAC 1; unadjusted 95%% intervals',primary_years),
      x='Jump in AMWS per 1,000 estimated births',y=NULL,colour=NULL,
      caption='Reduced-form cohort breaks. Within-year cutoffs omit the mixed birth year; known-year records need no birth month.')+
    theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),legend.position='bottom')
  ggsave(file.path(out_dir,paste0('state_comparison_',kind,'.png')),plot,
    width=11,height=max(5,2+.23*nrow(d)),dpi=160,bg='white')
}
ysrd_render_tex(out_dir,repo,primary,coverage,primary_years)
before[,`:=`(md5_after=unname(tools::md5sum(existing)))]
before[,unchanged:=md5_before==md5_after]
ysrd_require(all(before$unchanged),'An annual input or monthly result changed.')
write_out(before,'protected_input_manifest.csv')
write_out(data.table(check=c('annual_grid','source_reconciliation','all_51_dispositions',
  'annual_source_partition','mixed_year_exclusion','fixed_county_coverage','original_inputs_and_monthly_results'),passed=TRUE),
  'validation_checks.csv')
write_out(data.table(primary_years_per_side=primary_years,estimated_states=sum(primary$status=='estimated'),
  balanced_6=sum(support$states$max_balanced>=6,na.rm=TRUE),balanced_10=sum(support$states$max_balanced>=10,na.rm=TRUE),
  balanced_20=sum(support$states$max_balanced>=20,na.rm=TRUE)), 'run_summary.csv')
message('Created yearly RD figures and document: ',out_dir)
