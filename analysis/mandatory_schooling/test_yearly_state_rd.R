arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)
here <- dirname(normalizePath(sub('^--file=','',arg[1]),winslash='/'))
source(file.path(here,'yearly_state_rd_helpers.R'))
n <- 0L
check <- function(x) {stopifnot(isTRUE(x));n<<-n+1L}
event <- data.table(state_fips='01',state='Test',cutoff_date='1900-01-01',precision='annual_approximation')
check(ysrd_event(event)$last_pre==1899L && ysrd_event(event)$first_post==1900L)
mid <- copy(event);mid[,cutoff_date:='1900-09-04']
check(ysrd_event(mid)$mixed_year==1900L && ysrd_event(mid)$first_post==1901L)
a <- CJ(GEOID=c('01001','01003','01005'),year=1870:1930)
a[,`:=`(state_fips='01',births=10000,n_early=3L,n_1986=2L,n_amws=5L,n_assigned=4L)]
a[GEOID=='01003' & year==1892,births:=NA_real_]
a[GEOID=='01005' & year==1900,births:=0]
w <- ysrd_window(a,mid,10L)
check(identical(w$audit[retained==TRUE,GEOID],'01001'))
check(nrow(w$data)==20L && all(w$data$year!=1900L))
check(all(w$data$counties==1L) && all(w$data$n_amws==5L))
check(all(w$data$rate==.5) && all(w$data$n_assigned==4L))
check(all(w$data$kernel>0) && sum(w$data$post==0)==10L && sum(w$data$post==1)==10L)
support <- ysrd_support(a,event)
check(support$states$max_balanced==30L)
check(support$states$max_pre==30L && support$states$max_post==31L)
w0 <- ysrd_window(a,event,10L)$data
set.seed(831)
tau <- .4
w0[,n_amws:=births/1000*(1+.002*x+tau*post+.01*post*x+rnorm(.N,0,.002))]
fit <- ysrd_fit(w0)
check(fit$summary$status=='estimated' && abs(fit$summary$estimate-tau)<.015)
oracle <- sandwich::NeweyWest(fit$model,lag=1,prewhite=FALSE,adjust=TRUE)
check(abs(fit$summary$se-sqrt(oracle['post','post']))<1e-10)
w0[,n_amws:=0]
z <- ysrd_fit(w0)
check(z$summary$status=='all_zero_uninformative' && is.na(z$summary$p_value))
check(grepl('^0',w$audit$GEOID[1L]))
cat('Passed',n,'annual RD synthetic checks.\n')
