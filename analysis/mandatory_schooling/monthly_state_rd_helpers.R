# Monthly AMWS reduced-form cohort discontinuities. No input datasets are rewritten.
suppressPackageStartupMessages({library(data.table); library(ggplot2); library(sandwich)})

msrd_require <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

msrd_root <- function() {
  arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
  candidate <- if (length(arg)) dirname(sub('^--file=', '', arg[1L])) else
    Sys.getenv('GTL_REPO', unset = getwd())
  while (!file.exists(file.path(candidate, 'paths.R'))) {
    parent <- dirname(candidate)
    msrd_require(parent != candidate, 'Cannot locate paths.R; set GTL_REPO.')
    candidate <- parent
  }
  normalizePath(candidate, winslash = '/', mustWork = TRUE)
}

msrd_paths <- function(repo) {
  e <- new.env(parent = baseenv())
  sys.source(file.path(repo, 'paths.R'), envir = e)
  msrd_require(dir.exists(e$TALENT_DETS_DATA_DIR),
               'Set TALENT_DETS_DATA_DIR to the local Dropbox project root.')
  root <- normalizePath(e$TALENT_DETS_DATA_DIR, winslash = '/')
  list(root = root,
       results = file.path(root, 'results/mandatory_schooling/compulsory_schooling_amws_monthly_rd'),
       research = file.path(root, 'input/mandatory_schooling_monthly_rd'),
       monthly = file.path(root, 'output/mandatory_schooling/county_amws_monthly_gapminder_1800_1960.csv'),
       annual = file.path(root, 'output/mandatory_schooling/county_gapminder_annual_births_1800_1960.csv'),
       assignments = file.path(root, 'output/mandatory_schooling/monthly_gapminder_1800_1960_audits/scientist_birth_month_assignments.csv'),
       adoption = file.path(root, 'input/compulsory_schooling_laws.csv'),
       policy = file.path(root, 'output/mandatory_schooling/cls/cls_state_age_limits_1880_1930.csv'),
       onset = file.path(root, 'output/mandatory_schooling/cls/cls_law_onset_by_state.csv'),
       overrides = file.path(root, 'input/mandatory_schooling_monthly_rd/law_research_overrides.csv'))
}

# Coordinate in calendar months: January 1 has coordinate 12*year.
# Month centers are half-integers. Exact within-month cutoffs use actual day counts.
msrd_date_coordinate <- function(date) {
  d <- as.Date(date)
  year <- as.integer(format(d, '%Y')); month <- as.integer(format(d, '%m'))
  day <- as.integer(format(d, '%d'))
  starts <- as.Date(sprintf('%04d-%02d-01', year, month),format='%Y-%m-%d')
  next_month <- as.Date(sprintf('%04d-%02d-01', year + (month == 12L), month %% 12L + 1L),format='%Y-%m-%d')
  12L * year + month - 1L + (day - 1L) / as.numeric(next_month - starts)
}

msrd_birth_cutoff <- function(effective_date, exit_age, upper_inclusive = FALSE) {
  d <- as.Date(effective_date)
  # Ages through A include the entire Ath year of life, so leaving birthday is A+1.
  age <- as.integer(exit_age) + as.integer(upper_inclusive)
  year <- as.integer(format(d, '%Y')) - age
  month_day <- format(d, '%m-%d')
  # Feb 29 anniversaries in a non-leap birth year fall on Feb 28.
  if (!is.na(d) && month_day == '02-29' &&
      !(year %% 4L == 0L && (year %% 100L != 0L || year %% 400L == 0L))) month_day <- '02-28'
  as.Date(sprintf('%04d-%s', year, month_day))
}

msrd_inventory <- function(adoption, policy, onset, overrides) {
  a <- copy(adoption)
  a[, state_fips := sprintf('%02d', as.integer(state_fips))]
  msrd_require(nrow(a) == 51L && !anyDuplicated(a$state_fips), 'Expected 51 unique jurisdictions.')
  p <- copy(policy); p[, state_fips := sprintf('%02d', statefip)]
  o <- copy(onset); o[, state_fips := sprintf('%02d', statefip)]
  inv <- merge(a[, .(state_fips, state, state_abbr, canonical_law_year = compulsory_law_year,
                     law_year_source = source, canonical_session_reference = session_law_reference)],
               o[, .(state_fips, cls_first_year_in_force, left_censored, has_repeal, spells)],
               by = 'state_fips', all.x = TRUE)
  inv[, `:=`(law_year = canonical_law_year, entry_age = NA_integer_, exit_age = NA_integer_,
              effective_date = NA_character_, precision = 'unresolved', upper_inclusive = FALSE,
              age_source = '', age_reference = '', timing_source = '', timing_reference = '',
              school_calendar = 'Not verified; no district calendar imputed.',
              research_notes = '', earlier_partial_coverage = FALSE,
              historical_repeal = FALSE, research_status = 'unresolved_original_age',
              early_exit_years_code = NA_integer_, early_exit_condition_code = NA_integer_,
              work_certificate_age = NA_integer_, work_schooling_years_code = NA_integer_,
              continuation_age = NA_integer_, continuation_years_code = NA_integer_,
              exemption_reference = '', exemption_notes =
                'Individual health, distance, competency and hardship exceptions may apply; nominal maximum coverage does not imply universal attendance.')]
  # Only contemporaneous adoption-year age rules are usable, never a later 1880 age.
  for (i in seq_len(nrow(inv))) {
    row <- inv[i]
    if (is.na(row$law_year)) next
    match <- p[state_fips == row$state_fips & year == row$law_year & law_in_force == TRUE]
    if (nrow(match) == 1L && match$entryage > 0L && match$exitage > match$entryage) {
      inv[i, `:=`(entry_age = match$entryage, exit_age = match$exitage,
                  precision = 'annual_approximation', research_status = 'age_rule_sourced',
                  age_source = 'https://doi.org/10.3886/E145122V1',
                  age_reference = match$reference_agelimit,
                  research_notes = 'Contemporaneous original-year CLS nominal ages; individual exceptions and school-calendar timing are not observed.')]
      mapping <- c(early_exit_years_code='earlyyrs',early_exit_condition_code='earlyyrs_condition',
                   work_certificate_age='workage',work_schooling_years_code='workyrs',
                   continuation_age='contage',continuation_years_code='contyrs',
                   exemption_reference='reference_courseofstudy')
      for (target in names(mapping)) {
        original <- mapping[[target]]
        if (original %in% names(match)) set(inv,i,target,match[[original]][1L])
      }
    }
  }
  msrd_require(!anyDuplicated(overrides$state_fips), 'Duplicate researched state overrides.')
  msrd_require(all(overrides$state_fips %in% inv$state_fips), 'Unknown state in law research.')
  for (i in seq_len(nrow(overrides))) {
    j <- match(overrides$state_fips[i], inv$state_fips)
    for (name in setdiff(names(overrides), 'state_fips')) {
      value <- overrides[[name]][i]
      if (!is.na(value) && !(is.character(value) && !nzchar(value))) set(inv, j, name, value)
    }
  }
  inv[, cutoff_date := NA_character_]
  for (i in seq_len(nrow(inv))) {
    r <- inv[i]
    if (r$state_fips %in% c('02', '15')) {
      inv[i, `:=`(precision = 'excluded', research_status = 'outside_monthly_panel')]
      next
    }
    if (is.na(r$exit_age) || is.na(r$entry_age) || is.na(r$law_year) || r$precision == 'unresolved') next
    upper <- r$exit_age + as.integer(r$upper_inclusive)
    if (r$precision == 'verified_monthly') {
      msrd_require(!is.na(r$effective_date) && nzchar(r$effective_date) && nzchar(r$timing_source),
                   paste('Missing verified timing evidence:', r$state))
      cutoff <- msrd_birth_cutoff(r$effective_date, r$exit_age, r$upper_inclusive)
    } else {
      # Under annual CLS coding, age is Y-yob; the last eligible age is exitage-1.
      cutoff <- as.Date(sprintf('%04d-01-01', r$law_year - upper + 1L))
    }
    inv[i, cutoff_date := as.character(cutoff)]
  }
  inv[, cutoff_coordinate := msrd_date_coordinate(cutoff_date)]
  inv[, derivation := fifelse(precision == 'verified_monthly',
    'Effective/first-school-year reference date minus exclusive nominal leaving age; younger birth dates eligible.',
    fifelse(precision == 'annual_approximation',
      'January 1 of law_year - exclusive nominal leaving age + 1; annual age convention, not a verified monthly threshold.',
      'No cutoff assigned.'))]
  inv[, `:=`(state_of_birth_proxy = TRUE,
              interpretation = 'Reduced-form cohort discontinuity; nominal legal age coverage, not a schooling IV or verified attendance first stage.')]
  setorder(inv, state_fips)
  inv
}

msrd_policy_events <- function(policy) {
  p <- copy(policy); setorder(p, statefip, year)
  columns <- c('entryage', 'exitage', 'earlyyrs', 'earlyyrs_condition', 'workage', 'workyrs', 'contage', 'contyrs')
  p[, state_fips := sprintf('%02d', statefip)]
  events <- rbindlist(lapply(split(p, p$state_fips), function(s) {
    out <- list()
    for (i in seq.int(2L, nrow(s))) {
      changed <- columns[vapply(columns, function(col) !identical(s[[col]][i], s[[col]][i-1L]), logical(1L))]
      if (!length(changed)) next
      oldest_age <- max(s$exitage[i], s$exitage[i-1L],
                        if (s$workage[i] > 0) s$contage[i] else 0,
                        if (s$workage[i-1L] > 0) s$contage[i-1L] else 0)
      out[[length(out)+1L]] <- data.table(state_fips = s$state_fips[i], reform_year = s$year[i],
        approximate_affected_cohort = s$year[i] - oldest_age + 1L,
        changed_fields = paste(changed, collapse = ';'), reference = s$reference_agelimit[i])
    }
    rbindlist(out)
  }), fill = TRUE)
  events
}

msrd_validate_panel <- function(m) {
  msrd_require(all(c('GEOID','year','month','n_amws','n_amws_1906_1955_dedup','n_amws_1986','monthly_allocated_births') %in% names(m)), 'Missing monthly columns.')
  msrd_require(all(grepl('^[0-9]{5}$', m$GEOID)) && !anyNA(m$GEOID), 'GEOIDs must remain five-character strings.')
  msrd_require(all(m$month %in% 1:12) && !anyNA(m$year), 'Invalid monthly dates.')
  msrd_require(!anyDuplicated(m[, .(GEOID,year,month)]), 'Duplicate county-month key.')
  for (name in c('n_amws','n_amws_1906_1955_dedup','n_amws_1986'))
    msrd_require(all(is.finite(m[[name]]) & m[[name]] >= 0 & m[[name]] == floor(m[[name]])), 'Invalid scientist counts.')
  msrd_require(all(m$n_amws == m$n_amws_1906_1955_dedup + m$n_amws_1986), 'Source count mismatch.')
  invisible(TRUE)
}

msrd_state_window <- function(m, event, h) {
  msrd_require(is.finite(event$cutoff_coordinate), 'Cutoff not assigned.')
  cutoff <- event$cutoff_coordinate
  low <- floor(cutoff - h); high <- ceiling(cutoff + h) - 1L
  spine <- data.table(cohort_index = seq.int(low, high))
  spine[, `:=`(x = cohort_index + 0.5 - cutoff,
                straddles = cohort_index < cutoff & cohort_index + 1 > cutoff)]
  spine <- spine[abs(x) < h]
  selected <- m[state_fips == event$state_fips & cohort_index %in% spine$cohort_index]
  all_counties <- sort(unique(m[state_fips == event$state_fips, GEOID]))
  audit <- selected[, .(observed_months = uniqueN(cohort_index),
    invalid_denominators = sum(!is.finite(monthly_allocated_births) | monthly_allocated_births <= 0),
    excluded_or_retained_scientists = sum(n_amws)), by = GEOID]
  audit <- merge(data.table(GEOID = all_counties), audit, by='GEOID', all.x=TRUE)
  audit[is.na(observed_months), `:=`(observed_months=0L, invalid_denominators=0L, excluded_or_retained_scientists=0)]
  audit[, `:=`(expected_months=nrow(spine), retained=observed_months == nrow(spine) & invalid_denominators == 0L)]
  audit[, reason := fifelse(retained, 'retained', fifelse(observed_months != expected_months, 'incomplete_grid', 'missing_or_nonpositive_birth_denominator'))]
  audit[, `:=`(state_fips=event$state_fips, state=event$state, bandwidth_months=h)]
  counts <- c('monthly_allocated_births','n_amws','n_amws_1906_1955_dedup','n_amws_1986')
  kept <- selected[GEOID %in% audit[retained==TRUE,GEOID]]
  if (!nrow(kept)) return(list(data=data.table(),audit=audit,retained_counties=character()))
  d <- kept[, lapply(.SD,sum), by=.(cohort_index,year,month),.SDcols=counts]
  d <- merge(d, spine, by='cohort_index', all.x=TRUE)
  d[, `:=`(post=as.integer(x > 0),
            birth_month_start=as.Date(sprintf('%04d-%02d-01',year,month)),
            state_fips=event$state_fips,state=event$state,bandwidth_months=h,
            cutoff_date=event$cutoff_date,precision=event$precision,
            counties=sum(audit$retained))]
  for (name in c('n_amws','n_amws_1906_1955_dedup','n_amws_1986')) {
    target <- switch(name,n_amws='combined_rate',n_amws_1906_1955_dedup='early_rate',n_amws_1986='ed1986_rate')
    d[, (target) := 1000 * get(name) / monthly_allocated_births]
  }
  setorder(d,cohort_index)
  msrd_require(nrow(d)==nrow(spine) && all(d$monthly_allocated_births>0), 'State window changed county coverage.')
  list(data=d,audit=audit,retained_counties=audit[retained==TRUE,GEOID])
}

msrd_fit <- function(d, count_column, h, lag=12L) {
  base <- data.table(source=switch(count_column,n_amws='combined',n_amws_1906_1955_dedup='early',n_amws_1986='1986'),
    bandwidth_months=h,hac_lag=lag,status='not_estimated',estimate=NA_real_,se=NA_real_,
    ci_low=NA_real_,ci_high=NA_real_,p_value=NA_real_,months=0L,pre_months=0L,post_months=0L,
    pre_scientists=0,post_scientists=0,pre_births=0,post_births=0,sparse=TRUE,messages='')
  if (!nrow(d)) {base[,status:='no_balanced_counties'];return(list(summary=base,model=NULL))}
  x <- copy(d[straddles==FALSE & abs(x)<h]); setorder(x,cohort_index)
  base[, `:=`(months=nrow(x),pre_months=sum(x$post==0),post_months=sum(x$post==1),
              pre_scientists=sum(x[post==0,get(count_column)]),
              post_scientists=sum(x[post==1,get(count_column)]),
              pre_births=sum(x[post==0,monthly_allocated_births]),post_births=sum(x[post==1,monthly_allocated_births]))]
  base[, sparse:= pre_scientists<20 | post_scientists<20]
  if (base$pre_scientists + base$post_scientists == 0) {
    base[,status:='all_zero_uninformative'];return(list(summary=base,model=NULL))
  }
  if (base$pre_months<24L || base$post_months<24L) {
    base[,status:='insufficient_month_support'];return(list(summary=base,model=NULL))
  }
  x[, `:=`(y=1000*get(count_column)/monthly_allocated_births,
            w=monthly_allocated_births*pmax(0,1-abs(x)/h))]
  warnings <- character()
  result <- tryCatch(withCallingHandlers({
    model <- lm(y ~ post + x + post:x + factor(month),data=x,weights=w)
    msrd_require(model$rank==length(coef(model)) && all(is.finite(coef(model))), 'Singular RD model.')
    # Calendar-aware Bartlett HAC: missing straddling months retain their actual lag.
    # This is Newey-West without prewhitening, finite-sample adjusted.
    scores <- sandwich::estfun(model)
    middle <- crossprod(scores)
    for (ell in seq_len(lag)) {
      prior <- match(x$cohort_index-ell,x$cohort_index)
      ok <- which(!is.na(prior))
      if (length(ok)) {
        gamma <- crossprod(scores[ok,,drop=FALSE],scores[prior[ok],,drop=FALSE])
        middle <- middle+(1-ell/(lag+1))*(gamma+t(gamma))
      }
    }
    n <- nrow(scores); k <- model$rank
    bread <- sandwich::bread(model)
    v <- bread %*% middle %*% bread / n^2 * n/(n-k)
    se_hat <- sqrt(v['post','post']); tau_hat <- coef(model)[['post']]
    msrd_require(is.finite(se_hat) && se_hat>0, 'Nonpositive or invalid HAC standard error.')
    base[, `:=`(status='estimated',estimate=tau_hat,se=se_hat,ci_low=tau_hat-1.96*se_hat,
                ci_high=tau_hat+1.96*se_hat,p_value=2*pnorm(-abs(tau_hat/se_hat)),
                messages=paste(unique(warnings),collapse=' | '))]
    list(summary=base,model=model)
  },warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')}),
  error=function(e){base[,`:=`(status='model_failed',messages=conditionMessage(e))];list(summary=base,model=NULL)})
  result
}

msrd_plot_state <- function(d, fit, event) {
  if (!nrow(d)) return(NULL)
  x <- copy(d[straddles==FALSE]); x[, y:=combined_rate]
  if (!is.null(fit$model)) {
    # Standardize fitted lines to the same twelve-month seasonal composition.
    grid <- CJ(x=seq(-71.5,71.5,length.out=288L),month=1:12)
    grid[,post:=as.integer(x>0)]
    grid[,fitted:=as.numeric(predict(fit$model,newdata=grid))]
    grid <- grid[,.(fitted=mean(fitted)),by=.(x,post)]
  } else grid <- data.table(x=numeric(),post=integer(),fitted=numeric())
  s <- fit$summary
  subtitle <- sprintf('Law %s | birth cutoff %s | %s | %s counties',
                      event$law_year,event$cutoff_date,gsub('_',' ',event$precision),unique(d$counties))
  label <- if (s$status=='estimated') sprintf('Jump %.4f; 95%% CI [%.4f, %.4f]',s$estimate,s$ci_low,s$ci_high) else s$status
  ggplot(x,aes(x,y)) + geom_vline(xintercept=0,linetype='dashed',colour='firebrick') +
    geom_point(colour='#174a75',alpha=.7,size=1.3) +
    geom_line(data=grid,aes(x,fitted,group=post),inherit.aes=FALSE,colour='#d97720',linewidth=.8) +
    labs(title=event$state,subtitle=subtitle,x='Birth month relative to eligibility cutoff',
         y='AMWS per 1,000 estimated births',caption=paste(label,'| Seasonal-standardized local linear fit; HAC 12.')) +
    theme_minimal(base_size=12) + theme(panel.grid.minor=element_blank())
}
