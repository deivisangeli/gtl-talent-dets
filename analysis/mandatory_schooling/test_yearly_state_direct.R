arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)
here <- dirname(normalizePath(sub('^--file=','',arg[1]),winslash='/'))
source(file.path(here,'yearly_state_rd_helpers.R'))
source(file.path(here,'yearly_state_direct_helpers.R'))
n <- 0L
check <- function(x) {stopifnot(isTRUE(x));n<<-n+1L}
inv <- data.table(state_fips=c('01','02','11','15'),state=c('Alabama','Alaska','District of Columbia','Hawaii'),
  state_abbr=c('AL','AK','DC','HI'))
check(identical(sdyr_state(c('AL','alabama','DC','unknown'),inv),c('01','01','11',NA_character_)))
base <- data.table(record_id=c('1906:1','1938:2'),edition=c(1906L,1938L),lineid=1:2,
  kept=c(TRUE,FALSE),norm_last='smith',finit='J',year=1900L,state_fips='01')
extra <- data.table(record_id=c('1938:2','1938:3','1955:4','1906:5','1938:6','1955:7','1955:8'),
  edition=c(1938L,1938L,1955L,1906L,1938L,1955L,1955L),lineid=2:8,
  norm_last=c('smith','smith','smith','jones','jones','brown',''),finit='J',
  year=c(1900L,1901L,1902L,1900L,1901L,NA_integer_,1900L),state_fips='01')
added <- sdyr_additions(base,extra)
check(added[record_id=='1938:2',reason]=='already_in_combined_file')
check(added[record_id=='1938:3',reason]=='duplicate_against_retained_records')
check(added[record_id=='1955:4',included])
check(added[record_id=='1906:5',included]&&!added[record_id=='1938:6',included])
check(added[record_id=='1955:7',reason]=='missing_or_outside_birth_year')
check(added[record_id=='1955:8',reason]=='added_missing_name_key')
event <- data.table(state_fips='01',state='Alabama',cutoff_date='1900-01-01',precision='annual_approximation')
a <- data.table(state_fips='01',state='Alabama',year=1870:1930,births=10000,population=100000,
  n_amws=8L,n_early=5L,n_1986=3L,n_added=1L)
w <- sdyr_window(a,event,10L)
check(nrow(w$data)==20L&&all(w$data$n_amws==8L)&&all(w$data$rate==.8))
check(sum(w$data$post==0)==10L&&sum(w$data$post==1)==10L&&all(w$data$kernel>0))
check(identical(w$data$state_fips,rep('01',20)))
mid <- copy(event);mid[,cutoff_date:='1900-09-04']
mw <- sdyr_window(a,mid,10L)
check(nrow(mw$data)==20L&&!1900L %in% mw$data$year&&mw$audit$mixed_year_omitted==1900L)
a[year==1900,births:=NA_real_]
check(sdyr_window(a,mid,10L)$audit$missing_years==1L&&nrow(sdyr_window(a,mid,10L)$data)==0L)
check(sdyr_support(a,mid)$max_balanced==0L)
a[year==1900,births:=10000]
s <- sdyr_support(a,event)
check(s$max_pre==30L&&s$max_post==31L&&s$max_balanced==30L)
set.seed(937)
d <- w$data
d[,n_amws:=births/1000*(1+.002*x+.4*post+.01*x*post+rnorm(.N,0,.002))]
fit <- ysrd_fit(d)
check(fit$summary$status=='estimated'&&abs(fit$summary$estimate-.4)<.015)
check(abs(fit$summary$se-sqrt(sandwich::NeweyWest(fit$model,lag=1,prewhite=FALSE,adjust=TRUE)['post','post']))<1e-10)
d[,n_amws:=0L]
check(ysrd_fit(d)$summary$status=='all_zero_uninformative')
p <- data.table(state_fips=c('01','01'),state='Alabama',year=c(1900L,1901L),population=c(1000,NA_real_))
r <- data.table(state_fips='01',year=1900L,edition=c(1906L,1986L),origin=c('existing_early','1986'))
national <- data.table(year=1900:1901,us_birth_rate_year=.02)
panel <- sdyr_panel(r,p,national)
check(panel[year==1900,n_amws]==2L&&panel[year==1900,births]==20)
check(panel[year==1901,n_amws]==0L&&is.na(panel[year==1901,rate]))
fixture <- as.data.frame(rbind(c(NA,as.character(seq(1790,1990,10)),'First census'),
  c('Alabama',rep('---',9),'100','200',rep('300',10),'1790')))
pop <- sdyr_census_table(fixture,inv[1],1879:1891)
check(is.na(pop$panel[year==1879,population])&&pop$panel[year==1880,population]==100)
check(pop$panel[year==1885,population]==150&&pop$panel[year==1890,population]==200)
check(pop$panel[year==1885,anchor_before]==1880L&&pop$panel[year==1885,anchor_after]==1890L)
cat('Passed',n,'direct-state yearly checks.\n')
