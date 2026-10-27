# Annual first-law cohort discontinuities; no monthly-date imputation is needed.
suppressPackageStartupMessages({library(data.table);library(ggplot2);library(sandwich)})
ysrd_require <- function(ok,message) if (!isTRUE(ok)) stop(message,call.=FALSE)

ysrd_event <- function(event) {
  d <- as.character(event$cutoff_date)
  ysrd_require(length(d)==1L && !is.na(d),'Missing cutoff.')
  cy <- as.integer(substr(d,1,4))
  mixed <- substr(d,6,10)!='01-01'
  date <- as.Date(d)
  fraction <- as.numeric(date-as.Date(sprintf('%04d-01-01',cy))) /
    as.numeric(as.Date(sprintf('%04d-01-01',cy+1L))-as.Date(sprintf('%04d-01-01',cy)))
  list(cutoff=cy+fraction,last_pre=cy-1L,first_post=cy+as.integer(mixed),
       mixed_year=if (mixed) cy else NA_integer_)
}

ysrd_support <- function(a,inventory) {
  rows <- list(); county_rows <- list()
  for (i in seq_len(nrow(inventory))) {
    ev <- inventory[i]
    if (is.na(ev$cutoff_date) || ev$precision=='excluded') {
      rows[[i]] <- data.table(state_fips=ev$state_fips,state=ev$state,max_pre=NA_integer_,
        max_post=NA_integer_,max_balanced=NA_integer_)
      next
    }
    bounds <- ysrd_event(ev)
    d <- a[state_fips==ev$state_fips & is.finite(births) & births>0]
    ranges <- rbindlist(lapply(split(d,d$GEOID),function(s) {
      setorder(s,year)
      s[,spell:=cumsum(c(TRUE,diff(year)!=1L))]
      s[,.(first_year=min(year),last_year=max(year)),by=.(GEOID,spell)]
    }),fill=TRUE)
    if (nrow(ranges)) ranges <- ranges[first_year<=bounds$last_pre & last_year>=bounds$first_post]
    if (!nrow(ranges)) {
      rows[[i]] <- data.table(state_fips=ev$state_fips,state=ev$state,max_pre=0L,max_post=0L,max_balanced=0L)
      next
    }
    ranges[,`:=`(pre_years=bounds$last_pre-first_year+1L,post_years=last_year-bounds$first_post+1L,
      state_fips=ev$state_fips,state=ev$state)]
    ranges[,balanced_years:=pmin(pre_years,post_years)]
    county_rows[[i]] <- ranges
    rows[[i]] <- data.table(state_fips=ev$state_fips,state=ev$state,max_pre=max(ranges$pre_years),
      max_post=max(ranges$post_years),max_balanced=max(ranges$balanced_years))
  }
  list(states=rbindlist(rows),counties=rbindlist(county_rows,fill=TRUE))
}

ysrd_window <- function(a,event,years_per_side) {
  b <- ysrd_event(event)
  pre <- seq.int(b$last_pre-years_per_side+1L,b$last_pre)
  post <- seq.int(b$first_post,b$first_post+years_per_side-1L)
  spine <- seq.int(min(pre),max(post))
  selected <- a[state_fips==event$state_fips & year %in% spine]
  counties <- unique(a[state_fips==event$state_fips,GEOID])
  audit <- selected[,.(observed_years=uniqueN(year),invalid_denominators=sum(!is.finite(births)|births<=0),
    scientists_in_window=sum(n_amws),scientists_in_mixed_year=sum(n_amws[year %in% b$mixed_year])),by=GEOID]
  audit <- merge(data.table(GEOID=counties),audit,by='GEOID',all.x=TRUE)
  audit[is.na(observed_years),`:=`(observed_years=0L,invalid_denominators=0L,scientists_in_window=0L,scientists_in_mixed_year=0L)]
  audit[,`:=`(expected_years=length(spine),retained=observed_years==length(spine)&invalid_denominators==0L,
    state_fips=event$state_fips,state=event$state,years_per_side=years_per_side)]
  audit[,reason:=fifelse(retained,'retained',fifelse(observed_years!=expected_years,'incomplete_grid','missing_or_nonpositive_births'))]
  kept <- selected[GEOID %in% audit[retained==TRUE,GEOID]]
  if (!nrow(kept)) return(list(data=data.table(),audit=audit))
  d <- kept[,lapply(.SD,sum),by=year,.SDcols=c('births','n_amws','n_early','n_1986','n_assigned')]
  d[,`:=`(x=year+.5-b$cutoff,post=as.integer(year %in% post),mixed=year %in% b$mixed_year,
    state_fips=event$state_fips,state=event$state,years_per_side=years_per_side,
    cutoff_date=as.character(event$cutoff_date),precision=event$precision,counties=sum(audit$retained))]
  # For non-January cutoffs, omit the entire mixed birth year but keep denominator
  # coverage fixed through it. Each side retains the requested complete years.
  d <- d[mixed==FALSE]
  h_pre <- max(abs(d[post==0,x]))+.5
  h_post <- max(abs(d[post==1,x]))+.5
  d[,`:=`(kernel=1-abs(x)/fifelse(post==0,h_pre,h_post),
    bandwidth_pre=h_pre,bandwidth_post=h_post,rate=1000*n_amws/births)]
  setorder(d,year)
  ysrd_require(nrow(d)==2L*years_per_side && all(d$births>0 & d$kernel>0),'Invalid annual window.')
  ysrd_require(all(d$n_amws==d$n_early+d$n_1986),'Annual source mismatch.')
  list(data=d,audit=audit)
}

ysrd_fit <- function(d,count_column='n_amws',lag=1L) {
  out <- data.table(source=switch(count_column,n_amws='combined',n_early='early',n_1986='1986'),
    hac_lag=lag,status='not_estimated',estimate=NA_real_,se=NA_real_,ci_low=NA_real_,ci_high=NA_real_,
    p_value=NA_real_,pre_years=0L,post_years=0L,pre_scientists=0,post_scientists=0,sparse=TRUE,messages='')
  if (!nrow(d)) {out[,status:='no_balanced_counties'];return(list(summary=out,model=NULL))}
  out[,`:=`(pre_years=sum(d$post==0),post_years=sum(d$post==1),
    pre_scientists=sum(d[post==0,get(count_column)]),post_scientists=sum(d[post==1,get(count_column)]))]
  out[,sparse:=pre_scientists<20|post_scientists<20]
  if (out$pre_scientists+out$post_scientists==0) {
    out[,status:='all_zero_uninformative'];return(list(summary=out,model=NULL))
  }
  if (min(out$pre_years,out$post_years)<4L) {
    out[,status:='insufficient_year_support'];return(list(summary=out,model=NULL))
  }
  tryCatch({
    data <- copy(d);data[,`:=`(y=1000*get(count_column)/births,w=kernel*births)]
    model <- lm(y~post+x+post:x,data=data,weights=w)
    ysrd_require(model$rank==4L && all(is.finite(coef(model))),'Singular annual fit.')
    scores <- sandwich::estfun(model);middle <- crossprod(scores)
    if (lag>0L) for (ell in seq_len(lag)) {
      previous <- match(data$year-ell,data$year);ok <- which(!is.na(previous))
      if (length(ok)) {
        gamma <- crossprod(scores[ok,,drop=FALSE],scores[previous[ok],,drop=FALSE])
        middle <- middle+(1-ell/(lag+1))*(gamma+t(gamma))
      }
    }
    n <- nrow(scores);bread <- sandwich::bread(model)
    v <- bread%*%middle%*%bread/n^2*n/(n-model$rank)
    tau <- coef(model)[['post']];std <- sqrt(v['post','post'])
    ysrd_require(is.finite(std) && std>0,'Invalid annual HAC standard error.')
    out[,`:=`(status='estimated',estimate=tau,se=std,ci_low=tau-1.96*std,ci_high=tau+1.96*std,
      p_value=2*pnorm(-abs(tau/std)))]
    list(summary=out,model=model)
  },error=function(e) {out[,`:=`(status='model_failed',messages=conditionMessage(e))];list(summary=out,model=NULL)})
}

ysrd_plot <- function(d,fit,event) {
  line <- data.table()
  if (!is.null(fit$model)) {
    line <- rbind(data.table(x=seq(min(d$x),0,length.out=150),post=0L),
      data.table(x=seq(0,max(d$x),length.out=150),post=1L))
    line[,fitted:=as.numeric(predict(fit$model,newdata=line))]
  }
  p <- ggplot(d,aes(x,rate))+geom_vline(xintercept=0,linetype='dashed',colour='firebrick')+
    geom_point(size=2,colour='#174a75')
  if (nrow(line)) p <- p+geom_line(data=line,aes(x,fitted,group=post),inherit.aes=FALSE,linewidth=.8,colour='#d97720')
  s <- fit$summary
  p+labs(title=event$state,subtitle=sprintf('Birth cutoff %s | %s pre + %s post years | %s counties',
    as.character(event$cutoff_date),s$pre_years,s$post_years,unique(d$counties)),
    x='Birth-cohort midpoint in years relative to eligibility cutoff',y='AMWS per 1,000 estimated births',
    caption=if(s$status=='estimated') sprintf('Jump %.4f; 95%% CI [%.4f, %.4f] | Annual local linear fit; HAC 1',s$estimate,s$ci_low,s$ci_high) else s$status)+
    theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank())
}
