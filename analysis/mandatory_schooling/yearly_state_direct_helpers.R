# Direct birth-state annual panels. County coverage never selects scientists.
suppressPackageStartupMessages({library(data.table);library(readxl)})

sdyr_state <- function(x,inventory) {
  z <- toupper(trimws(as.character(x)))
  ans <- inventory$state_fips[match(z,toupper(inventory$state_abbr))]
  full <- inventory$state_fips[match(z,toupper(inventory$state))]
  ans[is.na(ans)] <- full[is.na(ans)]
  ans
}
sdyr_last <- function(x) gsub('[^a-z]','',tolower(trimws(iconv(x,to='ASCII//TRANSLIT',sub=''))))
sdyr_initial <- function(x) substr(gsub('[^A-Z]','',toupper(trimws(iconv(x,to='ASCII//TRANSLIT',sub='')))),1,1)

# Anchor all existing decisions, including duplicates already dropped upstream.
# New unmatched records cannot displace an existing retained record.
sdyr_additions <- function(base,extra) {
  extra <- copy(extra)
  extra[,reason:='candidate']
  extra[record_id %in% base$record_id,reason:='already_in_combined_file']
  extra[reason=='candidate' & (is.na(state_fips)|state_fips %in% c('02','15')),
        reason:='unresolved_or_excluded_state']
  extra[reason=='candidate' & (is.na(year)|!year %between% c(1800L,1960L)),reason:='missing_or_outside_birth_year']
  keys <- base[kept==TRUE & !is.na(state_fips) & !is.na(year) & !is.na(norm_last) & nzchar(norm_last),
               paste(norm_last,finit,year,state_fips,sep='|')]
  seen <- new.env(hash=TRUE,parent=emptyenv())
  for (key in keys) assign(key,TRUE,envir=seen)
  setorder(extra,edition,lineid)
  for (j in which(extra$reason=='candidate')) {
    z <- extra[j]
    if (is.na(z$norm_last)||!nzchar(z$norm_last)) {
      extra[j,reason:='added_missing_name_key'];next
    }
    near <- paste(z$norm_last,z$finit,z$year+c(-1L,0L,1L),z$state_fips,sep='|')
    if (any(vapply(near,exists,logical(1),envir=seen,inherits=FALSE))) {
      extra[j,reason:='duplicate_against_retained_records']
    } else {
      extra[j,reason:='added_state_only']
      assign(near[2],TRUE,envir=seen)
    }
  }
  extra[,included:=reason %in% c('added_state_only','added_missing_name_key')]
  extra
}

sdyr_read_records <- function(paths,inventory) {
  root <- paths$root; amws <- file.path(root,'output/amws')
  early_file <- file.path(amws,'amws_combined_us_geocoded.csv')
  late_file <- file.path(root,'Data/processed/amws/amws_ed86_final.csv')
  unmatched <- file.path(amws,c('amws_1906_us_unmatched_final.csv',
    'amws_1938_us_unmatched_final.csv','amws_1955_us_still_unmatched.csv'))
  canonical <- file.path(amws,c('amws_1906_cleaned_corrected.csv',
    'amws_1938_cleaned_corrected.csv','amws_1955_split_corrected.csv'))
  raw06_file <- file.path(root,'input/amws/amws_1906.xlsx')
  early <- fread(early_file,colClasses=c(geoid='character'))
  early[,`:=`(record_id=paste(edition,lineid,sep=':'),state_fips=sdyr_state(state,inventory),
    year=as.integer(birth_year),kept=monthly_kept(kept))]
  raw06 <- as.data.table(read_excel(raw06_file,sheet=1,col_names=FALSE,.name_repair='unique_quiet'))
  names06 <- data.table(lineid=suppressWarnings(as.integer(raw06[[1]])),name=as.character(raw06[[2]]))
  names06 <- names06[!is.na(lineid)]
  names06[,`:=`(last=trimws(sub(',.*','',name)),first=trimws(sub('^[^,]*,\\s*','',name)))]
  extra <- rbindlist(lapply(seq_along(unmatched),function(j) {
    ed <- c(1906L,1938L,1955L)[j]
    u <- fread(unmatched[j]);c <- fread(canonical[j])
    candidate_rows <- u[,.(unmatched_candidate_rows=.N),by=lineid]
    # The final 1955 file repeats identical identity/state candidates; these are
    # one scientist, not additional observations. Conflicting candidates fail.
    u <- unique(u[,.(lineid,state,birthplace_orig)])
    ysrd_require(!anyDuplicated(u$lineid)&&!anyDuplicated(c$lineid),'Conflicting unmatched/canonical record IDs.')
    u <- merge(u,candidate_rows,by='lineid')
    if (ed==1906L) {
      c <- merge(c[,.(lineid,year=as.integer(birth_year))],names06[,.(lineid,last,first)],by='lineid',all.x=TRUE)
    } else if (ed==1938L) {
      c <- c[,.(lineid,year=as.integer(birth_year),last=trimws(sub(',.*','',AMSname)),
        first=trimws(sub('^[^,]*,\\s*','',AMSname)))]
    } else c <- c[,.(lineid,year=as.integer(birth_year),last,first)]
    d <- merge(u,c,by='lineid',all.x=TRUE)
    d[,`:=`(edition=ed,state_fips=sdyr_state(state,inventory),record_id=paste(ed,lineid,sep=':'),
      norm_last=sdyr_last(last),finit=sdyr_initial(first))]
    d
  }),fill=TRUE)
  extra <- sdyr_additions(early,extra)
  early[,reason:=fifelse(!kept,'excluded_existing_dedup',fifelse(is.na(state_fips),'unresolved_state',
    fifelse(state_fips %in% c('02','15'),'excluded_Alaska_Hawaii',
    fifelse(is.na(year)|!year %between% c(1800L,1960L),'missing_or_outside_birth_year','retained_existing'))))]
  # Upstream kept flags remain untouched. A record attributed to multiple
  # birth states cannot be counted twice or assigned to an invented state.
  ambiguous <- early[reason=='retained_existing',.(states=uniqueN(state_fips)),by=record_id][states>1L,record_id]
  early[reason=='retained_existing'&record_id %in% ambiguous,reason:='conflicting_existing_birth_states']
  late <- fread(late_file,colClasses=c(geo_geoid='character'))
  late[,`:=`(state_fips=sdyr_state(birth_state,inventory),year=as.integer(birth_year),
    contaminated=monthly_contaminated(raw_text_adjusted))]
  late[,reason:=fifelse(is.na(birth_country)|birth_country!='USA','not_US_birth',
    fifelse(contaminated,'see_previous_contamination',fifelse(is.na(state_fips),'unresolved_state',
    fifelse(state_fips %in% c('02','15'),'excluded_Alaska_Hawaii',
    fifelse(is.na(year)|!year %between% c(1800L,1960L),'missing_or_outside_birth_year','retained_existing')))))]
  audit <- rbindlist(list(
    early[,.(record_id,edition,state_fips,year,GEOID=geoid,reason,origin='existing_early',original_kept=kept)],
    extra[,.(record_id,edition,state_fips,year,GEOID=NA_character_,reason,origin='unmatched_early',original_kept=NA)],
    late[,.(record_id=as.character(entry_id),edition=1986L,state_fips,year,GEOID=geo_geoid,reason,origin='1986',original_kept=NA)]))
  audit[,included:=reason %in% c('retained_existing','added_state_only','added_missing_name_key')]
  audit[,county_state:=substr(monthly_geoid(GEOID),1,2)]
  audit[,state_county_disagreement:=!is.na(county_state)&!is.na(state_fips)&county_state!=state_fips]
  records <- audit[included==TRUE]
  ysrd_require(!anyDuplicated(records[,.(edition,record_id)]),'Duplicate retained edition/record IDs.')
  list(records=records,audit=audit,additions=extra,
    files=c(early_file,late_file,unmatched,canonical,raw06_file))
}

sdyr_census <- function(file,inventory,years=1800:1960) {
  x <- as.data.frame(read_excel(file,sheet=1,col_names=FALSE,.name_repair='unique_quiet'))
  sdyr_census_table(x,inventory,years)
}

sdyr_census_table <- function(x,inventory,years=1800:1960) {
  header <- which(vapply(seq_len(nrow(x)),function(k)
    sum(as.character(unlist(x[k,])) %in% as.character(seq(1790L,1990L,10L)))>=20L,logical(1)))
  ysrd_require(length(header)==1L,'Cannot identify Census year header.')
  dates <- suppressWarnings(as.integer(unlist(x[header,])))
  cols <- which(dates %in% seq(1790L,1990L,10L))
  rows <- match(inventory$state,as.character(x[[1]]))
  ysrd_require(!anyNA(rows)&&length(cols)==21L,'Incomplete Census states or dates.')
  anchors <- rbindlist(lapply(seq_len(nrow(inventory)),function(j) {
    v <- as.character(unlist(x[rows[j],cols]));numeric <- suppressWarnings(as.numeric(v))
    ysrd_require(all(is.na(v)|v=='---'|is.finite(numeric)),'Unexpected Census sentinel.')
    data.table(state_fips=inventory$state_fips[j],state=inventory$state[j],year=dates[cols],population=numeric)
  }))
  setorder(anchors,state_fips,year)
  panel <- anchors[is.finite(population)&population>0,{
    vals <- approx(year,population,xout=years,method='linear',rule=1,ties='ordered')$y
    list(year=years,population=vals,
      anchor_before=vapply(years,function(y) if(any(year<=y)) max(year[year<=y]) else NA_integer_,integer(1)),
      anchor_after=vapply(years,function(y) if(any(year>=y)) min(year[year>=y]) else NA_integer_,integer(1)))
  },by=.(state_fips,state)]
  panel[,population_status:=fifelse(is.na(population),'outside_census_coverage',
    fifelse(anchor_before==anchor_after,'census_observation','linear_interpolation'))]
  list(anchors=anchors,panel=panel)
}

sdyr_panel <- function(records,population,national) {
  counts <- records[,.(n_early=sum(edition!=1986L),n_1986=sum(edition==1986L),
    n_added=sum(origin=='unmatched_early')),by=.(state_fips,year)]
  a <- merge(population[!state_fips %in% c('02','15')],national,by='year',all.x=TRUE)
  a <- merge(a,counts,by=c('state_fips','year'),all.x=TRUE)
  for (column in c('n_early','n_1986','n_added')) set(a,which(is.na(a[[column]])),column,0L)
  a[,`:=`(n_amws=n_early+n_1986,births=population*us_birth_rate_year)]
  a[,rate:=1000*n_amws/births]
  setorder(a,state_fips,year)
  ysrd_require(!anyDuplicated(a[,.(state_fips,year)])&&sum(a$n_amws)==nrow(records),'State source reconciliation failed.')
  a
}

sdyr_support <- function(a,inventory) {
  rbindlist(lapply(seq_len(nrow(inventory)),function(i) {
    ev <- inventory[i];d <- a[state_fips==ev$state_fips]
    if (ev$precision=='excluded'||is.na(ev$cutoff_date)) return(data.table(
      state_fips=ev$state_fips,state=ev$state,max_pre=NA_integer_,max_post=NA_integer_,max_balanced=NA_integer_))
    b <- ysrd_event(ev);valid <- d[is.finite(births)&births>0,year]
    pre <- 0L;post <- 0L
    while ((b$last_pre-pre) %in% valid) pre <- pre+1L
    while ((b$first_post+post) %in% valid) post <- post+1L
    if (!is.na(b$mixed_year)&&!b$mixed_year %in% valid) pre<-post<-0L
    data.table(state_fips=ev$state_fips,state=ev$state,max_pre=pre,max_post=post,
      max_balanced=min(pre,post),first_population_year=if(length(valid)) min(valid) else NA_integer_,
      last_population_year=if(length(valid)) max(valid) else NA_integer_)
  }),fill=TRUE)
}

sdyr_window <- function(a,event,h) {
  b <- ysrd_event(event);pre_years <- seq.int(b$last_pre-h+1L,b$last_pre)
  post_years <- seq.int(b$first_post,b$first_post+h-1L);spine <- seq.int(min(pre_years),max(post_years))
  d <- copy(a[state_fips==event$state_fips & year %in% spine])
  missing <- setdiff(spine,d[is.finite(births)&births>0,year])
  audit <- data.table(state_fips=event$state_fips,state=event$state,years_per_side=h,
    first_pre_year=min(pre_years),last_pre_year=max(pre_years),first_post_year=min(post_years),last_post_year=max(post_years),
    mixed_year_omitted=b$mixed_year,expected_years=length(spine),missing_years=length(missing),
    missing_year_list=paste(missing,collapse=';'),scientists_in_window=sum(d$n_amws),
    reason=if(length(missing)) 'incomplete_state_denominator_coverage' else 'supported')
  if (length(missing)) return(list(data=data.table(),audit=audit))
  d[,`:=`(x=year+.5-b$cutoff,post=as.integer(year %in% post_years))]
  d <- d[year %in% c(pre_years,post_years)]
  d[,`:=`(bandwidth_pre=max(abs(x[post==0]))+.5,bandwidth_post=max(abs(x[post==1]))+.5)]
  d[,`:=`(kernel=1-abs(x)/fifelse(post==0,bandwidth_pre,bandwidth_post),
    rate=1000*n_amws/births,cutoff_date=as.character(event$cutoff_date),precision=event$precision,years_per_side=h)]
  setorder(d,year)
  ysrd_require(nrow(d)==2*h&&all(d$kernel>0)&&all(d$births>0),'Invalid direct-state window.')
  list(data=d,audit=audit)
}
