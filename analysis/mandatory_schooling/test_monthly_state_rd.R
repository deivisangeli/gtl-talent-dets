# Synthetic validation; does not read or modify the project datasets.
arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)
here <- if (length(arg)) dirname(normalizePath(sub('^--file=','',arg[1]),winslash='/')) else
  file.path(Sys.getenv('GTL_REPO',unset=getwd()),'analysis/mandatory_schooling')
source(file.path(here,'monthly_state_rd_helpers.R'))
source(file.path(here,'monthly_state_rd_law_research.R'))
checks <- 0L
check <- function(ok) {stopifnot(isTRUE(ok));checks<<-checks+1L}
fails <- function(expr) inherits(tryCatch({force(expr);NULL},error=identity),'error')

check(msrd_date_coordinate('1900-01-01')==1900*12)
check(msrd_date_coordinate('1900-12-01')==1900*12+11)
check(msrd_date_coordinate('1901-01-01')-msrd_date_coordinate('1900-12-01')==1)
check(msrd_birth_cutoff('1917-10-01',16)=='1901-10-01')
check(msrd_birth_cutoff('1875-01-01',15,TRUE)=='1859-01-01')
check(msrd_birth_cutoff('1904-02-29',3)=='1901-02-28')
check(is.na(msrd_date_coordinate(NA_character_)))

adopt <- data.table(state_fips=sprintf('%02d',c(1:51)),state=paste0('S',1:51),
  state_abbr=paste0('S',1:51),compulsory_law_year=1900L,source='year evidence',session_law_reference='law')
policy <- data.table(statefip=1:51,year=1900L,law_in_force=TRUE,entryage=8L,exitage=14L,
                    reference_agelimit='contemporaneous law',earlyyrs=320L,
                    earlyyrs_condition=1L,workage=12L,workyrs=220L,
                    contage=16L,contyrs=420L,reference_courseofstudy='conditional exemption')
onset <- data.table(statefip=1:51,cls_first_year_in_force=1900L,left_censored=FALSE,has_repeal=FALSE,spells='1900-1930')
overrides <- data.table(state_fips='01',effective_date='1900-09-15',precision='verified_monthly',
                        timing_source='verified source',historical_repeal=TRUE)
inv <- msrd_inventory(adopt,policy,onset,overrides)
check(inv[state_fips=='01',cutoff_date]=='1886-09-15')
check(inv[state_fips=='03',cutoff_date]=='1887-01-01')
check(inv[state_fips=='01',historical_repeal])
check(inv[state_fips=='01',early_exit_years_code]==320L)
check(inv[state_fips=='01',work_certificate_age]==12L)
check(inv[state_fips=='01',continuation_age]==16L)
check(inv[state_fips=='01',exemption_reference]=='conditional exemption')
check(all(inv[state_fips %in% c('02','15'),precision]=='excluded'))
# An 1880 age rule cannot be used to assign a first-law age in 1870.
adopt[state_fips=='03',compulsory_law_year:=1870L]
policy[statefip==3,year:=1880L]
inv2 <- msrd_inventory(adopt,policy,onset,overrides)
check(inv2[state_fips=='03',precision]=='unresolved' && is.na(inv2[state_fips=='03',cutoff_date]))
check(fails(msrd_inventory(adopt,policy,onset,rbind(overrides,overrides))))
check(!anyDuplicated(msrd_researched_laws()$state_fips))
researched <- msrd_researched_laws()
check(researched[state_fips=='41',law_year]==1890L)
check(grepl('conditional',researched[state_fips=='29',exemption_notes]))

m <- CJ(GEOID=c('01001','01003','01005'),year=1879:1894,month=1:12)
m[,`:=`(state_fips='01',cohort_index=12L*year+month-1L,n_amws=3L,
          n_amws_1906_1955_dedup=2L,n_amws_1986=1L,monthly_allocated_births=1000)]
msrd_validate_panel(m)
bad <- copy(m);bad[1,GEOID:='1001']
check(fails(msrd_validate_panel(bad)))
bad <- copy(m);bad[1,n_amws:=5L]
check(fails(msrd_validate_panel(bad)))
check(fails(msrd_validate_panel(rbind(m,m[1]))))
event <- inv[state_fips=='01']
m[GEOID=='01003' & year==1886,monthly_allocated_births:=NA_real_]
m[GEOID=='01005' & year==1886,monthly_allocated_births:=0]
win <- msrd_state_window(m,event,72L)
check(identical(win$retained_counties,'01001'))
check(sum(win$audit$retained)==1L && sum(!win$audit$retained)==2L)
check(all(win$data$monthly_allocated_births==1000))
check(all(win$data$n_amws==3L))
check(all(win$data$combined_rate==win$data$early_rate+win$data$ed1986_rate))
check(nrow(win$data[straddles==TRUE])==1L && win$data[straddles==TRUE,month]==9L)
check(all(win$data[straddles==TRUE,year]==1886L))
check(is.na(m[GEOID=='01003' & year==1886,monthly_allocated_births][1]))
m[,n_amws:=0L];m[,`:=`(n_amws_1906_1955_dedup=0L,n_amws_1986=0L)]
zero <- msrd_state_window(m,event,72L)
zfit <- msrd_fit(zero$data,'n_amws',72L)
check(zfit$summary$status=='all_zero_uninformative' && is.na(zfit$summary$p_value))
check(all(zero$data$combined_rate==0))

# Recover known jump with two slopes and an arbitrary seasonal component.
d <- copy(win$data)
set.seed(914)
tau <- .45
d[,post:=as.integer(x>0)]
season <- c(-.04,.03,-.02,.01,.06,-.07,.04,.08,-.06,.01,.04,-.08)
d[,n_amws:=monthly_allocated_births/1000*(1+tau*post+.001*x+.002*post*x+season[month]+rnorm(.N,0,.004))]
fit <- msrd_fit(d,'n_amws',72L,12L)
check(fit$summary$status=='estimated')
check(abs(fit$summary$estimate-tau)<.015)
check(fit$summary$pre_months>=70 && fit$summary$post_months>=70)
check(is.finite(fit$summary$se) && fit$summary$se>0)
check(fit$summary$ci_low<fit$summary$estimate && fit$summary$ci_high>fit$summary$estimate)

# With no calendar gaps the custom HAC must equal sandwich::NeweyWest.
d <- copy(win$data); d[,straddles:=FALSE]
d[,n_amws:=monthly_allocated_births/1000*(1+tau*post+.001*x+.002*post*x+season[month]+rnorm(.N,0,.004))]
fit <- msrd_fit(d,'n_amws',72L,12L)
oracle <- sandwich::NeweyWest(fit$model,lag=12L,prewhite=FALSE,adjust=TRUE)
check(abs(fit$summary$se-sqrt(oracle['post','post']))<1e-10)

# Repeal and reinstatement survive the policy-change audit.
p <- data.table(statefip=1L,year=1900:1903,entryage=c(8L,0L,0L,8L),exitage=c(14L,0L,0L,14L),
  earlyyrs=0L,earlyyrs_condition=0L,workage=0L,workyrs=0L,contage=0L,contyrs=0L,reference_agelimit='source')
events <- msrd_policy_events(p)
check(identical(events$reform_year,c(1901L,1903L)))
check(all(events$approximate_affected_cohort==c(1888L,1890L)))
cat('Passed',checks,'monthly-state RD synthetic checks.\n')
