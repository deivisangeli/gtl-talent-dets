# Direct-state annual first-law RDs; no monthly or prior yearly outputs rewritten.
arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)
here <- dirname(normalizePath(sub('^--file=','',arg[1]),winslash='/'))
source(file.path(here,'yearly_state_rd_helpers.R'))
source(file.path(here,'monthly_state_rd_helpers.R'))
source(file.path(here,'../../prep/mandatory_schooling/monthly_amws_helpers.R'))
source(file.path(here,'yearly_state_direct_helpers.R'))
source(file.path(here,'build_amws_state_year_direct.R'))
source(file.path(here,'render_yearly_state_direct_latex.R'))
repo <- msrd_root();paths <- msrd_paths(repo)
old_yearly <- file.path(paths$root,'results/mandatory_schooling/compulsory_schooling_amws_yearly_rd')
out <- file.path(paths$root,'results/mandatory_schooling/compulsory_schooling_amws_yearly_rd_state_direct')
panel_out <- file.path(paths$root,'output/mandatory_schooling/yearly_state_direct')
dir.create(file.path(out,'states'),recursive=TRUE,showWarnings=FALSE)
save <- function(d,name) fwrite(d,file.path(out,name),na='')
amws <- file.path(paths$root,'output/amws')
original_inputs <- c(paths$annual,paths$monthly,paths$assignments,paths$policy,paths$adoption,
  file.path(amws,c('amws_combined_us_geocoded.csv','amws_1906_us_unmatched_final.csv',
    'amws_1938_us_unmatched_final.csv','amws_1955_us_still_unmatched.csv','amws_1906_cleaned_corrected.csv',
    'amws_1938_cleaned_corrected.csv','amws_1955_split_corrected.csv')),
  file.path(paths$root,c('input/amws/amws_1906.xlsx','input/new_births_total_number_estimated.csv',
    'Data/processed/amws/amws_ed86_final.csv')),
  list.files(paths$results,recursive=TRUE,full.names=TRUE),list.files(old_yearly,recursive=TRUE,full.names=TRUE))
original_inputs <- unique(original_inputs[file.exists(original_inputs)&!file.info(original_inputs)$isdir])
message('Snapshotting original input and result checksums.')
before <- data.table(file=original_inputs,md5_before=unname(tools::md5sum(original_inputs)))
inventory <- fread(file.path(paths$results,'cutoff_inventory.csv'),colClasses=c(state_fips='character'))
ysrd_require(nrow(inventory)==51L&&!anyDuplicated(inventory$state_fips),'Expected 51 jurisdictions.')
built <- sdyr_build(paths,repo,inventory,panel_out)
a <- built$panel;support <- sdyr_support(a,inventory)
save(support,'available_years_by_state.csv');save(inventory,'cutoff_inventory.csv')
save(built$source,'source_coverage_by_edition.csv')
save(built$audit[,.(records=.N),by=.(edition,origin,reason,included)],'source_count_reconciliation.csv')
policy <- msrd_policy_events(fread(paths$policy))
estimates <- list();points <- list();coverage <- list();windows <- list();overlaps <- list()
for (i in seq_len(nrow(inventory))) {
  ev <- inventory[i];message(sprintf('[%02d/51] %s',i,ev$state))
  if (ev$precision=='excluded'||is.na(ev$cutoff_date)) {
    coverage[[i]] <- data.table(state_fips=ev$state_fips,state=ev$state,
      precision=ev$precision,status=if(ev$precision=='excluded') 'scope_excluded' else 'unresolved_cutoff')
    next
  }
  b <- ysrd_event(ev)
  for (h in c(6L,10L,20L)) {
    win <- sdyr_window(a,ev,h);windows[[length(windows)+1L]] <- win$audit
    nearby <- copy(policy[state_fips==ev$state_fips & reform_year!=ev$law_year &
      approximate_affected_cohort>=b$last_pre-h+1L & approximate_affected_cohort<=b$first_post+h-1L])
    if (nrow(nearby)) {nearby[,years_per_side:=h];overlaps[[length(overlaps)+1L]]<-nearby}
    for (column in c('n_amws','n_early','n_1986')) for (lag in if(h==10L) c(1L,0L,2L) else 1L) {
      fit <- ysrd_fit(win$data,column,lag);r <- fit$summary
      if (!nrow(win$data)) r[,status:=win$audit$reason]
      bd <- built$source[edition %in% if(column=='n_1986') 1986L else if(column=='n_early') c(1906L,1938L,1955L) else built$source$edition]
      boundary <- any(c(bd$first_birth,bd$last_birth+1L)>b$last_pre-h+1L &
        c(bd$first_birth,bd$last_birth+1L)<b$first_post+h)
      r[,`:=`(state_fips=ev$state_fips,state=ev$state,state_abbr=ev$state_abbr,
        years_per_side=h,cutoff_date=as.character(ev$cutoff_date),precision=ev$precision,
        law_year=ev$law_year,mixed_year_omitted=b$mixed_year,aggregation='direct_birth_state',
        source_coverage_boundary=boundary,policy_overlap=nrow(nearby)>0L,
        historical_repeal=ev$historical_repeal,earlier_partial_coverage=ev$earlier_partial_coverage,
        added_scientists_in_fit=if(nrow(win$data)) sum(win$data$n_added) else NA_integer_,
        failure_reason=if(nrow(win$data)) messages else paste0('Missing positive state denominators for years: ',win$audit$missing_year_list))]
      estimates[[length(estimates)+1L]] <- r
      if(h==10L && column=='n_amws' && lag==1L) {
        coverage[[i]] <- r[,.(state_fips,state,precision,status)]
        if(r$status=='estimated') {
          points[[length(points)+1L]] <- win$data
          line <- rbind(data.table(x=seq(min(win$data$x),0,length.out=150),post=0L),
            data.table(x=seq(0,max(win$data$x),length.out=150),post=1L))
          line[,fitted:=as.numeric(predict(fit$model,newdata=line))]
          p <- ggplot(win$data,aes(x,rate))+geom_vline(xintercept=0,linetype='dashed',colour='firebrick')+
            geom_point(size=2,colour='#174a75')+
            geom_line(data=line,aes(x,fitted,group=post),inherit.aes=FALSE,linewidth=.8,colour='#d97720')+
            labs(title=ev$state,subtitle=sprintf('Birth cutoff %s | 10 pre + 10 post years | Direct birth-state aggregation',ev$cutoff_date),
              x='Birth-cohort midpoint in years relative to eligibility cutoff',y='AMWS per 1,000 estimated births',
              caption=sprintf('Jump %.4f; 95%% CI [%.4f, %.4f] | Annual local linear fit; HAC 1',r$estimate,r$ci_low,r$ci_high))+
            theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank())
          ggsave(file.path(out,'states',paste0(ev$state_abbr,'.png')),p,width=10,height=6,dpi=160,bg='white')
        }
      }
    }
  }
}
estimates <- rbindlist(estimates,fill=TRUE)
primary <- estimates[source=='combined'&years_per_side==10L&hac_lag==1L]
primary[,p_adjusted_bh:=NA_real_]
for (kind in c('verified_monthly','annual_approximation')) {
  ids <- which(primary$precision==kind&primary$status=='estimated')
  if(length(ids)) set(primary,ids,'p_adjusted_bh',p.adjust(primary$p_value[ids],method='BH'))
}
coverage <- merge(rbindlist(coverage,fill=TRUE),support,by=c('state_fips','state'),all.x=TRUE)
points <- rbindlist(points,fill=TRUE);windows <- rbindlist(windows)
ysrd_require(nrow(coverage)==51L&&!anyDuplicated(coverage$state_fips),'Missing state dispositions.')
save(primary,'primary_state_estimates.csv');save(estimates,'state_estimates_all_specifications.csv')
save(points,'state_yearly_plot_points.csv');save(coverage,'all_state_dispositions.csv')
save(windows,'state_window_coverage_audit.csv');save(rbindlist(overlaps,fill=TRUE),'overlapping_policy_changes.csv')
for (kind in c('verified_monthly','annual_approximation')) {
  d <- copy(primary[precision==kind&status=='estimated'])
  save(primary[precision==kind],paste0('primary_',kind,'.csv'))
  ysrd_require(nrow(d)>0L,'No coefficient figure data for precision group.')
  d[,state_label:=factor(state,levels=state[order(estimate)])]
  p <- ggplot(d,aes(estimate,state_label))+geom_vline(xintercept=0,linetype='dashed',colour='grey50')+
    geom_segment(aes(x=ci_low,xend=ci_high,yend=state_label),linewidth=.55)+geom_point(aes(colour=sparse),size=2.2)+
    scale_colour_manual(values=c('FALSE'='#174a75','TRUE'='#d97720'),
      labels=c('FALSE'='At least 20 scientists per side','TRUE'='Sparse (<20 on a side)'))+
    labs(title=paste('Yearly AMWS cohort discontinuities:',gsub('_',' ',kind)),
      subtitle='Direct birth-state counts; 10 complete years per side; local linear fit; HAC 1; 95% intervals',
      x='Jump in AMWS per 1,000 estimated births',y=NULL,colour=NULL,
      caption='Reduced-form cohort breaks. Statewide population denominators; no county coverage selection.')+
    theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),legend.position='bottom')
  ggsave(file.path(out,paste0('state_comparison_',kind,'.png')),p,width=11,height=max(5,2+.23*nrow(d)),dpi=160,bg='white')
}
old <- fread(file.path(old_yearly,'primary_state_estimates.csv'),colClasses=c(state_fips='character'))
comparison <- merge(primary[,.(state_fips,state,direct_status=status,direct_estimate=estimate,
  direct_ci_low=ci_low,direct_ci_high=ci_high,direct_pre_scientists=pre_scientists,direct_post_scientists=post_scientists)],
  old[,.(state_fips,county_status=status,county_estimate=estimate,county_ci_low=ci_low,county_ci_high=ci_high,
    county_pre_scientists=pre_scientists,county_post_scientists=post_scientists,retained_counties)],by='state_fips',all=TRUE)
comparison[,`:=`(estimate_change=direct_estimate-county_estimate,
  pre_scientists_added=direct_pre_scientists-county_pre_scientists,
  post_scientists_added=direct_post_scientists-county_post_scientists)]
save(comparison,'comparison_with_county_restricted.csv')
cdata <- comparison[direct_status=='estimated'&county_status=='estimated']
p <- ggplot(cdata,aes(county_estimate,direct_estimate))+geom_abline(slope=1,intercept=0,colour='grey50',linetype='dashed')+
  geom_point(colour='#174a75')+geom_text(aes(label=state),check_overlap=TRUE,nudge_y=.002,size=3)+
  labs(title='Direct-state versus county-restricted yearly RD estimates',x='Previous county-restricted jump',
    y='Direct birth-state jump',caption='Both use 10 complete years per side. Changes reflect scientist inclusion and statewide population denominators.')+
  theme_minimal()
ggsave(file.path(out,'comparison_with_county_restricted.png'),p,width=10,height=7,dpi=160,bg='white')
existing_annual <- fread(paths$annual,select=c('GEOID','year','n_early_year','n_1986_year'),colClasses=c(GEOID='character'))
existing_annual[,state_fips:=substr(GEOID,1,2)]
old_counts <- existing_annual[,.(old_county_input_scientists=sum(n_early_year+n_1986_year)),by=.(state_fips,year)]
reconcile <- merge(a[,.(state_fips,year,direct_scientists=n_amws,n_added)],old_counts,by=c('state_fips','year'),all=TRUE)
reconcile[,difference:=direct_scientists-old_county_input_scientists]
save(reconcile,'comparison_with_full_county_input_counts.csv')
sdyr_render_tex(out,repo,primary,coverage,10L,nrow(built$records),sum(built$records$origin=='unmatched_early'))
before[,md5_after:=unname(tools::md5sum(original_inputs))]
before[,unchanged:=md5_before==md5_after]
save(before,'protected_input_manifest.csv')
ysrd_require(all(before$unchanged),'An original input or result changed.')
checks <- data.table(check=c('direct_source_reconciliation','unique_state_year_keys','leading_zero_state_codes',
  'source_partition','birth_denominator_formula','all_51_dispositions','20_primary_points_each',
  'mixed_year_omitted','no_denominator_extrapolation','original_inputs_and_results_unchanged'),
  passed=c(sum(a$n_amws)==nrow(built$records),!anyDuplicated(a[,.(state_fips,year)]),
    all(grepl('^[0-9]{2}$',a$state_fips)),all(a$n_amws==a$n_early+a$n_1986),
    isTRUE(all.equal(a$births,a$population*a$us_birth_rate_year)),nrow(coverage)==51L,
    all(points[,.N,by=state_fips]$N==20L),all(points$year!=as.integer(substr(points$cutoff_date,1,4))|
      substr(points$cutoff_date,6,10)=='01-01'),all(is.na(a[population_status=='outside_census_coverage',births])),all(before$unchanged)))
save(checks,'validation_checks.csv');ysrd_require(all(checks$passed),'Direct-state validation failed.')
save(data.table(primary_years_per_side=10L,estimated_states=sum(primary$status=='estimated'),
  balanced_6=sum(support$max_balanced>=6,na.rm=TRUE),balanced_10=sum(support$max_balanced>=10,na.rm=TRUE),
  balanced_20=sum(support$max_balanced>=20,na.rm=TRUE),source_records=nrow(built$records),
  state_only_additions=sum(built$records$origin=='unmatched_early')),'run_summary.csv')
fwrite(data.table(file=built$files,md5=unname(tools::md5sum(built$files))),file.path(out,'source_input_manifest.csv'))
message('Completed direct-state yearly results: ',out)
