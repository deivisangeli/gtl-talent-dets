# Document renderer called by the annual estimator after figures are written.
sdyr_render_tex <- function(directory,repo,primary,coverage,years_per_side,source_records,added_records) {
  esc <- function(x) vapply(as.character(x),function(s) {
    chars <- strsplit(s,'',fixed=TRUE)[[1L]]
    map <- c('\\'='\\textbackslash{}','&'='\\&','%'='\\%','$'='\\$',
      '#'='\\#','_'='\\_','{'='\\{','}'='\\}','~'='\\textasciitilde{}','^'='\\textasciicircum{}')
    values <- map[chars];paste0(ifelse(is.na(values),chars,values),collapse='')
  },character(1L),USE.NAMES=FALSE)
  number <- function(x) formatC(x,format='f',digits=4L)
  precision <- function(x) ifelse(x=='verified_monthly','Sourced month-specific timing','Annual approximation')
  integer <- function(x) ifelse(is.na(x),'--',as.character(x))
  states <- copy(primary[status=='estimated']);setorder(states,state)
  cov <- copy(coverage);setorder(cov,state)
  n <- nrow(states); stem <- 'yearly_state_rd_figures'
  images <- c('state_comparison_verified_monthly.png','state_comparison_annual_approximation.png',
    paste0('states/',states$state_abbr,'.png'))
  ysrd_require(all(file.exists(file.path(directory,images))),'Missing annual figure assets.')
  scratch <- file.path(repo,'tmp/pdfs/yearly_state_rd_state_direct')
  dir.create(scratch,recursive=TRUE,showWarnings=FALSE)
  latex <- c('% Generated from annual outcome estimates; original monthly results are unchanged.',
    '\\documentclass[10pt,a4paper]{article}',
    '\\usepackage[left=15mm,right=15mm,top=16mm,bottom=17mm]{geometry}',
    '\\usepackage[T1]{fontenc}', '\\usepackage{lmodern}',
    '\\ifdefined\\pdfgentounicode\\input{glyphtounicode}\\pdfgentounicode=1\\fi',
    '\\usepackage{graphicx,caption,booktabs,longtable}',
    '\\usepackage[hidelinks]{hyperref}',
    '\\hypersetup{pdftitle={Direct-state yearly AMWS compulsory-schooling cohort discontinuities},pdfauthor={GTL Talent Determinants}}',
    '\\graphicspath{{./}{states/}}',
    '\\captionsetup{font=small,labelfont=bf,skip=4pt,hypcap=false}',
    '\\setlength{\\parindent}{0pt}', '\\setlength{\\parskip}{6pt}', '\\pagestyle{myheadings}',
    '\\begin{document}', '\\thispagestyle{plain}',
    '{\\LARGE\\bfseries Direct-state yearly AMWS schooling RDs\\par}',
    sprintf('{\\large %s complete pre-years and %s complete post-years\\par}',years_per_side,years_per_side),
    '\\vspace{8mm}', '\\section*{Outcome and specification}',
    'The outcome is annual AMWS scientist births per 1,000 estimated births. Scientists are counted directly by their recorded birth state; county denominator coverage never selects records. Combined counts preserve deduplication across the 1906, 1938 and 1955 editions; 1986 has not been deduplicated against them.',
    sprintf('All records with a valid state and birth year are included, even if the birth month or county is unknown. Across 1800--1960, the input contains %s scientist records, including %s additions from unmatched early-edition files. Additions are checked against retained records and one another; existing deduplication decisions remain fixed. Six candidates representing three existing record IDs with conflicting birth states are excluded as unresolved assignments.',format(source_records,big.mark=','),added_records),
    'State population uses the Census Bureau historical state table (Forstall, 1996, Part II), with reconstructed state boundaries and linear interpolation between census years. There is no extrapolation. Estimated state births equal state population times the existing national Gapminder birth rate. That rate retains its original county-panel-derived national population denominator.',
    sprintf('Each state requires positive statewide birth denominators throughout its window, including any omitted mixed-exposure year. The analysis selects %s complete birth years before the eligibility boundary and %s after it.',years_per_side,years_per_side),
    'For a sourced within-year cutoff, the entire straddling birth year is omitted. Thus Alabama omits 1901 and Michigan omits 1857. Georgia\'s January cutoff does not require an omission. Original annual-approximation cutoffs retain their January convention.',
    'The regression includes a treatment indicator and separate linear trends, using annual cohort midpoints relative to the original cutoff. Triangular weights multiply estimated annual births. Each side\'s kernel support ends half a year beyond its outermost selected midpoint; this accommodates an omitted mixed year while retaining the requested number of complete cohorts.',
    'There are no calendar-month fixed effects. Dots and orange fitted lines are on the same annual-rate scale. Intervals use calendar-aware Newey--West standard errors with one annual lag, no prewhitening, a finite-sample adjustment and normal 95\\% intervals. Six- and twenty-year windows and zero-/two-lag checks are exported separately.',
    '\\section*{How many years can be included?}',
    sprintf('The primary document estimates %s jurisdictions. Statewide denominators support ten complete years per side for %s jurisdictions and twenty for %s. The appendix accounts for all 51 jurisdictions; unsupported windows and uninformative outcomes are not plotted.',n,sum(cov$max_balanced>=10,na.rm=TRUE),sum(cov$max_balanced>=20,na.rm=TRUE)),
    'The appendix reports continuous pre, post and balanced spans. These are denominator-coverage limits, not recommended RD bandwidths or guarantees of adequate scientist counts. Very early territorial cutoffs can precede the first available population census.',
    '\\vfill',
    '{\\small Interpretation: reduced-form cohort discontinuities. Legal exposure may ramp gradually, birth state may differ from schooling state, and AMWS supplies no schooling first stage. Sparse counts, source turnover, individual exemptions and overlapping policy changes remain relevant.\\par}')
  prose <- !startsWith(latex,'\\')&!startsWith(latex,'{')&!startsWith(latex,'%')&nzchar(latex)
  latex[prose] <- paste0(latex[prose],'\\par')
  for (kind in c('verified_monthly','annual_approximation')) {
    count <- sum(states$precision==kind)
    title <- if(kind=='verified_monthly') 'sourced month-specific law timing' else 'annual cutoff approximations'
    image <- paste0('state_comparison_',kind,'.png')
    latex <- c(latex,'\\clearpage','\\markright{Yearly AMWS RDs: coefficient comparisons}',
      paste0('\\section*{Coefficients: ',title,'}'),'\\begin{center}',
      sprintf('\\includegraphics[width=\\linewidth,height=0.78\\textheight,keepaspectratio]{%s}',image),
      sprintf('\\captionof{figure}{Annual-outcome cohort discontinuities for %s jurisdictions with %s. Each fit uses %s complete birth years per side, annual local linear trends and HAC-1 inference. Intervals are unadjusted 95\\%% intervals. Orange denotes fewer than 20 scientists on at least one side.}',count,title,years_per_side),
      paste0('\\label{fig:yearly-rd-coefficients-',kind,'}'),'\\end{center}')
  }
  captions <- character(n)
  for (i in seq_len(n)) {
    d <- states[i]
    if (i%%2L==1L) latex <- c(latex,'\\clearpage','\\markright{Yearly AMWS RDs: state plots}')
    captions[i] <- sprintf('%s. Birth cutoff: %s; %s. %s pre / %s post years. Jump: %s; 95\\%% CI [%s, %s] per 1,000 estimated births.',
      esc(d$state),as.character(d$cutoff_date),tolower(precision(d$precision)),d$pre_years,d$post_years,
      number(d$estimate),number(d$ci_low),number(d$ci_high))
    latex <- c(latex,'\\begin{minipage}[t][0.475\\textheight][t]{\\textwidth}','\\centering',
      sprintf('\\includegraphics[width=\\linewidth,height=0.415\\textheight,keepaspectratio]{states/%s.png}',d$state_abbr),
      paste0('\\captionof{figure}{',captions[i],'}'),paste0('\\label{fig:yearly-rd-state-',d$state_abbr,'}'),
      '\\end{minipage}\\par')
    if (i%%2L==1L) latex <- c(latex,'\\vspace{0.02\\textheight}')
  }
  latex <- c(latex,'\\clearpage','\\markright{Yearly AMWS RDs: available years}',
    '\\section*{Appendix: annual coverage and available years}',
    sprintf('Maximum spans use continuous positive statewide denominator coverage around the cutoff. The primary requires %s complete years per side. Alaska and Hawaii retain the explicit scope exclusions. Very long spans may contain substantial source and policy changes.',years_per_side),
    '{\\footnotesize','\\setlength{\\tabcolsep}{4pt}','\\renewcommand{\\arraystretch}{1.05}',
    '\\begin{longtable}{@{}p{4.1cm}p{5.6cm}rrr@{}}',
    '\\toprule Jurisdiction & Primary status & Max pre & Max post & Balanced \\\\ \\midrule',
    '\\endfirsthead','\\toprule Jurisdiction & Primary status & Max pre & Max post & Balanced \\\\ \\midrule',
    '\\endhead','\\midrule \\multicolumn{5}{r}{Continued on the next page} \\\\',
    '\\endfoot','\\bottomrule','\\endlastfoot')
  for (i in seq_len(nrow(cov))) {
    d <- cov[i]
    status <- switch(d$status,estimated='Estimated',incomplete_state_denominator_coverage='No primary-window population coverage',
      scope_excluded='Excluded from scope',all_zero_uninformative='All-zero: uninformative',d$status)
    latex <- c(latex,sprintf('%s & %s & %s & %s & %s \\\\',esc(d$state),esc(status),
      integer(d$max_pre),integer(d$max_post),integer(d$max_balanced)))
  }
  latex <- c(latex,'\\end{longtable}','}',
    'Coverage: \\texttt{available\\_years\\_by\\_state.csv}. Captions: \\texttt{primary\\_state\\_estimates.csv}. Record exclusions, source reconciliation, population inputs and comparisons with county-restricted estimates are exported alongside this document.',
    'Population source: \\href{https://www2.census.gov/programs-surveys/decennial/tables/1990/population-of-states-and-counties-us-1790-1990/}{Census Bureau historical state population tables}. Historical Census coverage and underenumeration limitations remain; interpolation does not create independent annual population observations.',
    '\\end{document}')
  ysrd_require(sum(grepl('\\includegraphics[',latex,fixed=TRUE))==n+2L,'Wrong annual figure count.')
  for (image in images) ysrd_require(sum(grepl(paste0('{',image,'}'),latex,fixed=TRUE))==1L,'Duplicate or missing annual figure.')
  tex <- file.path(directory,paste0(stem,'.tex'))
  writeLines(latex,tex,useBytes=TRUE)
  compiler <- Sys.which('pdflatex')
  ysrd_require(nzchar(compiler),paste('Missing pdflatex; source preserved:',tex))
  oldwd <- getwd();on.exit(setwd(oldwd),add=TRUE);setwd(directory)
  args <- c('--disable-installer','-halt-on-error','-interaction=nonstopmode','-no-shell-escape',
    '-file-line-error',paste0('-output-directory=',shQuote(scratch)),shQuote(basename(tex)))
  for (pass in 1:2) {
    log <- system2(compiler,args,stdout=TRUE,stderr=TRUE)
    writeLines(log,file.path(scratch,paste0('compile_pass_',pass,'.txt')))
    ysrd_require(is.null(attr(log,'status')) || attr(log,'status')==0L,'Annual LaTeX compilation failed; inspect scratch logs.')
  }
  log <- readLines(file.path(scratch,paste0(stem,'.log')),warn=FALSE)
  ysrd_require(!any(grepl('Overfull|Undefined control sequence|LaTeX Error|undefined references',log)),
    'Annual document has overflow or unresolved LaTeX errors.')
  ysrd_require(file.copy(file.path(scratch,paste0(stem,'.pdf')),file.path(directory,paste0(stem,'.pdf')),overwrite=TRUE),
    'Could not save annual PDF.')
}

# Render existing direct-state estimates and PNGs without rerunning regressions.
if (sys.nframe()==0L) {
  arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)
  here <- dirname(normalizePath(sub('^--file=','',arg[1]),winslash='/'))
  source(file.path(here,'yearly_state_rd_helpers.R'))
  source(file.path(here,'monthly_state_rd_helpers.R'))
  repo <- msrd_root();paths <- msrd_paths(repo)
  directory <- file.path(paths$root,'results/mandatory_schooling/compulsory_schooling_amws_yearly_rd_state_direct')
  primary <- fread(file.path(directory,'primary_state_estimates.csv'),colClasses=c(state_fips='character'))
  coverage <- fread(file.path(directory,'all_state_dispositions.csv'),colClasses=c(state_fips='character'))
  summary <- fread(file.path(directory,'run_summary.csv'))
  sdyr_render_tex(directory,repo,primary,coverage,summary$primary_years_per_side,
    summary$source_records,summary$state_only_additions)
}
