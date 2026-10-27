# Generate a human-readable companion to the machine-readable audit outputs.
msrd_write_report <- function(directory,primary,estimates,states,edition,inventory) {
  fmt <- function(x,digits=4L) ifelse(is.finite(x),formatC(x,format='f',digits=digits),'—')
  fmt_p <- function(x) ifelse(is.finite(x) & x<.0001,'<0.0001',fmt(x))
  table <- function(d) c(paste0('| ',paste(names(d),collapse=' | '),' |'),
    paste0('| ',paste(rep('---',ncol(d)),collapse=' | '),' |'),
    vapply(seq_len(nrow(d)),function(i) paste0('| ',paste(unlist(d[i]),collapse=' | '),' |'),character(1L)))
  good <- primary[status=='estimated']
  verified <- good[precision=='verified_monthly']
  excluded <- states[status!='estimated']
  month_table <- edition[,.(Edition=edition,Retained=records,Assigned=assigned_month,
    `Assigned percent`=fmt(100*assigned_share,1L),`First birth`=first_birth,`Last birth`=last_birth)]
  exact <- verified[,.(State=state,`Birth cutoff`=cutoff_date,Jump=fmt(estimate),
    `95% interval`=paste0('[',fmt(ci_low),', ',fmt(ci_high),']'),`Raw p`=fmt_p(p_value))]
  sensitivity <- rbindlist(lapply(verified$state_fips,function(fips) {
    d <- estimates[state_fips==fips & source=='combined' & hac_lag==12L]
    d[order(bandwidth_months),.(State=state,`Years per side`=bandwidth_months/12,
      Jump=fmt(estimate),`Raw p`=fmt_p(p_value),Status=status)]
  }))
  rows <- good[order(precision,state),.(State=state,Precision=precision,`Birth cutoff`=cutoff_date,
    Jump=fmt(estimate),`95% interval`=paste0('[',fmt(ci_low),', ',fmt(ci_high),']'),
    `Raw p`=fmt_p(p_value),`BH p`=fmt_p(p_adjusted_bh),Counties=retained_counties,
    Sparse=sparse,`Policy overlap`=policy_overlap)]
  text <- c('# Monthly AMWS compulsory-schooling cohort discontinuities','',
    paste('Generated',format(Sys.time(),'%Y-%m-%d %H:%M %Z'), 'from the full 1800–1960 monthly panel.'),'',
    sprintf('The primary six-year window produced estimates for **%s jurisdictions: %s sourced month-specific cutoffs and %s annual approximations**. %s contiguous jurisdictions lack any county with positive birth denominators throughout the window. Alaska and Hawaii are outside the panel.',
      nrow(good),nrow(verified),sum(good$precision=='annual_approximation'),sum(excluded$status=='no_balanced_counties')),'',
    'These are **reduced-form cohort discontinuities**. A legal maximum-age boundary can produce gradually increasing exposure rather than a discrete schooling jump. AMWS supplies no schooling first stage. Birth state is a proxy for schooling state.','',
    '## Earlier editions carry birth months','',table(month_table),'',
    'Early dates were extracted from `birthplace_orig`; the 1986 edition uses `birth_date`. The early editions retain their existing cross-edition deduplication. The combined outcome adds 1986 without cross-deduplicating it against the early editions. Records without an assigned month are audited and excluded from monthly counts.','',
    sprintf('Early-edition-only outcomes are informative in %s primary windows; 1986-only outcomes are informative in %s. All-zero source outcomes have no estimate, interval or p-value.',
      sum(estimates[source=='early' & bandwidth_months==72L & hac_lag==12L,status]=='estimated'),
      sum(estimates[source=='1986' & bandwidth_months==72L & hac_lag==12L,status]=='estimated')),'',
    '## Sourced monthly timing','',table(exact),'',
    '“Verified monthly” means the original statutory implementation or first-school-year date and nominal age endpoints are sourced. It does not verify attendance or a common local enrollment decision. Michigan’s September 1857 month straddles its September 4 boundary and is omitted.','',
    '![Monthly timing estimates](state_comparison_verified_monthly.png)','',
    'Bandwidth sensitivity uses HAC 12 and rebalances the county set within each window:', '',table(sensitivity),'',
    'Michigan’s positive estimate is not robust to the three- or ten-year windows and rests on fewer than 20 scientists on each side. Georgia’s negative estimate weakens at ten years. Alabama also changes with bandwidth. Consult the county audit because bandwidth changes can change the retained counties.','',
    '## Primary state estimates','',
    'Units are scientists per 1,000 estimated births. Intervals are unadjusted 95% normal intervals. BH p-values adjust separately within the monthly-timing and annual-approximation families. No common pooled treatment effect is inferred from the mixed signs.','',
    table(rows),'',
    '![Annual approximations](state_comparison_annual_approximation.png)','',
    '## Exclusions','',table(excluded[,.(State=state,Reason=status)]),'',
    'The county-window audit records each excluded county, denominator failure and available scientist count. A missing or nonpositive denominator is not converted to a zero outcome. Failed and all-zero models remain in the specification export.','',
    '## Design and legal inventory','',
    '- Outcome: 1,000 times monthly scientist counts divided by aggregated estimated births. Annual birth estimates are divided by twelve; they do not supply observed monthly seasonality.',
    '- Fixed counties: require complete monthly coverage and a finite positive denominator throughout the window, including any omitted straddling month.',
    '- Regression: treatment indicator, separate linear slopes and calendar-month fixed effects; triangular kernel multiplied by estimated births.',
    '- Running variable: monthly cohort midpoint relative to the birth eligibility date. Verified within-month boundaries omit the straddling cohort month.',
    '- Inference: time-ordered Bartlett/Newey–West HAC, no prewhitening, finite-sample adjustment; 12 lags primary, 6 and 24 sensitivity. Actual calendar gaps are preserved.',
    '- Annual approximation: January 1 of effective law year minus exclusive leaving age plus one. An inclusive upper age is converted to the next birthday. Annual cutoffs are not asserted to be exact monthly eligibility thresholds.',
    '- Exemptions: contemporaneous CLS early-exit, employment and continuation codes are retained. Maryland and Missouri’s oldest nominal coverage is conditional on employment. Individual exemptions remain unobserved.',
    '- Earlier geographic coverage and repeals are flagged. Arizona 1875, Texas 1870 and Washington 1872 early episodes remain primary; their later reintroductions are exported separately.',
    '- Original-year CLS ages are used only where available. Left-censored 1880 entries never substitute for an earlier law. Original pre-1880 ages are sourced separately.',
    '- Research differences are explicit: New Jersey 1875 versus canonical 1874; Oregon effective 1890 versus enactment 1889; Washington’s early date and Texas’s 1870/1871 implementation remain approximate. Louisiana 1914 covered selected cities, so statewide 1916 is retained.','',
    'See [cutoff inventory](cutoff_inventory.csv) for the full statutory references, effective dates, endpoints, derivations, exemption codes, calendar assumptions and unresolved issues. The editable research overrides and matching inventory are also saved under Dropbox `input/mandatory_schooling_monthly_rd/`. Sources were consulted through CLS replication records, transcribed original statutes, historical government reports and a cross-check against the Gateway Policy Explorer source narratives; not every original statute was independently imaged.','',
    'Sources: [CLS replication data and appendix](https://doi.org/10.3886/E145122V1), [CLS 2012 Appendix A](https://www.nber.org/system/files/working_papers/w18477/w18477.pdf), [Gateway US source narratives](https://g2aging-media.s3.amazonaws.com/bilhdkmj/bywhymrq.pdf). Additional original-source links are state-specific in the inventory.','',
    '## Files and validation','',
    '- `primary_state_estimates.csv`, `primary_verified_monthly.csv`, `primary_annual_approximations.csv`: primary estimates and reasons.',
    '- `state_estimates_all_specifications.csv`: all three source outcomes, three bandwidths, and HAC sensitivities.',
    '- `state_monthly_plot_points.csv`, `states/*.png`: primary monthly points and individual state plots.',
    '- `county_window_coverage_audit.csv`, `all_state_dispositions.csv`: county exclusions and all 51 jurisdictions.',
    '- `birth_month_coverage_by_edition.csv`, `birth_month_coverage_state_year_edition.csv`, `window_source_completeness_diagnostics.csv`: assignment completeness and source composition.',
    '- `statutory_policy_changes.csv`, `overlapping_policy_changes.csv`: post-1880 age, work-exemption and continuation changes. Pre-1880 overlaps are not exhaustively reconstructed.',
    '- `later_reintroduction_estimates.csv`, `later_reintroduction_monthly_points.csv`, `reintroductions/*.png`: separately labeled later episodes.',
    '- `panel_validation.csv`, `validation_checks.csv`: exact reconciliation to assigned annual source counts and annual birth denominators.',
    '- `protected_input_manifest.csv`: before/after hashes of consumed inputs, source datasets and existing mandatory-schooling analysis files.',
    '- `existing_results_manifest.csv`: unchanged top-level pre-existing result-directory metadata; it is not a recursive content-hash audit.',
    '- `run_summary.csv`: estimation coverage and counts.','',
    'Synthetic tests cover inclusive/exclusive ages, year transitions, leap dates, midmonth boundaries, missing dates, left censoring, repeals, leading-zero GEOIDs, source partitions, fixed county coverage, zero versus missing data, recovery of a known discontinuity with seasonal patterns and separate slopes, and equivalence to `sandwich::NeweyWest` on contiguous months. Shared inputs are never rewritten; generated files stay within the new research and results directories.','')
  writeLines(text,file.path(directory,'results_summary.md'),useBytes=TRUE)
  invisible(text)
}
