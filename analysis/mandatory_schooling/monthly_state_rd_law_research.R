# Explicit source-reviewed first-law overrides. The generated inventory lives in Dropbox.
# Annual ages are nominal statutory ages, not observed attendance or an individual first stage.
msrd_researched_laws <- function() {
  gateway <- 'https://g2aging-media.s3.amazonaws.com/bilhdkmj/bywhymrq.pdf'
  nber <- 'https://www.nber.org/system/files/working_papers/w18477/w18477.pdf'
  cls <- 'https://doi.org/10.3886/E145122V1'
  # Sources inspected: Gateway Policy Explorer Series 2026, each named state's
  # School Attendance narrative; original statutes identified below. Gateway's
  # later-cohort enrollment-calendar assumptions are NOT imported into this design.
  early <- data.table(
    state_fips=c('06','09','11','20','23','25','26','32','33','34','36','39','50','53','55','56','04','48'),
    entry_age=c(8L,8L,8L,8L,9L,8L,8L,8L,8L,8L,8L,8L,8L,8L,7L,7L,8L,6L),
    exit_age=c(14L,14L,14L,14L,15L,14L,14L,14L,14L,14L,14L,14L,14L,16L,15L,16L,14L,18L),
    upper_inclusive=c(FALSE,FALSE,FALSE,FALSE,TRUE,rep(FALSE,13L)),
    age_reference=c(
      'Gateway PDF p.33; California 1874 ch.516',
      'Gateway PDF p.41; Connecticut 1872 ch.77 sec.1; CT OLR 94-R-0847',
      'Gateway PDF p.49; William B. Webb 1864 Public Schools sec.18',
      'Gateway PDF p.81; Kansas 1874 ch.123 sec.1',
      'Gateway PDF p.93; Maine 1875 ch.24 sec.1; ages 9 THROUGH 15',
      'CLS 2012 Appendix A printed p.49/PDF p.50; 1852 Mass. Acts 170',
      'CLS 2012 Appendix A printed pp.49-50/PDF pp.50-51; 1871 Mich. Acts 251',
      'Nevada 1873 ch.31 sec.1; official session statutes pp.89-90',
      'Gateway PDF p.134; New Hampshire 1871 ch.2 sec.1',
      'Gateway PDF p.138; New Jersey 1875 ch.443 sec.1; canonical year 1874 conflicts',
      'NZ parliamentary report 1886 E-12 p.82; 1874 NY law ages 8-14; retain Lingwall effective year 1875',
      'Gateway PDF p.156; Ohio 62nd General Assembly 1877 sec.1',
      'Gateway PDF p.201; Vermont 1867 Act 35 sec.1',
      'Gateway PDF p.210; Washington 1871 ch.6 sec.1; key date 1872',
      'Gateway PDF p.219; Wisconsin 1879 ch.121 sec.1; Thayer 74 Wis.48 (1889)',
      'Gateway PDF p.223; Wyoming 1873 ch.58 sec.37',
      'US Bureau of Education History of Public School Education in Arizona p.28; 1875 Ariz. Sess. Laws 40',
      'Texas Supreme Court Peay v Talbot 39 Tex.335 (1873); early school laws 1870/1871; CLS Data Appendix p.6'))
  early[, `:=`(age_source=gateway, precision='annual_approximation',
               research_status='age_rule_sourced',school_calendar='Original statewide enrollment reference date not verified; January is an annual approximation.',
               research_notes='Original-age narrative checked against cited law; later-age rules are not backcast.')]
  early[state_fips=='25',age_source:=nber]
  early[state_fips=='26', `:=`(age_source=nber,effective_date='1871-09-04',precision='verified_monthly',
    timing_source=nber,timing_reference='Appendix A PDF p.50: school year commencing first Monday of September 1871.',
    school_calendar='Original statute specifies first Monday of September 1871 (September 4).',
    historical_repeal=TRUE,research_notes='Nominal first-school-year eligibility boundary; individual exceptions remain. Michigan repealed in 1881 and reintroduced subsequently.')]
  early[state_fips=='09',age_source:='https://www.cga.ct.gov/PS94/rpt/olr/htm/94-R-0847.htm']
  early[state_fips=='09',earlier_partial_coverage:=TRUE]
  early[state_fips=='32',age_source:='https://www.leg.state.nv.us/division/Legal/LawLibrary/Statutes/06th1873/Stats187301.html']
  early[state_fips=='34', `:=`(law_year=1875L,research_notes='Gateway and the 1935 US Bureau attendance-law history identify 1875; Lingwall current CSV gives 1874. Use sourced 1875 approximation, flag one-year disagreement.')]
  early[state_fips=='36', `:=`(age_source='https://paperspast.natlib.govt.nz/parliamentary/AJHR1886-I.2.2.3.15',
    research_notes='Contemporary 1886 report reproduces original 1874 8-14 law. Keep Lingwall first statewide effective year 1875. Gateway begins at 1894 and cannot establish the original age rule.')]
  early[state_fips=='53', `:=`(law_year=1872L,historical_repeal=TRUE,
    research_notes='1871 territorial law; Gateway key date/2025 narrative gives effective year 1872, while 2026 narrative gives enactment 1871. Annual approximation only; repealed 1873. Canonical 1878 differs; CLS 1886 is a later statewide episode. Earlier 8-18 claims concern later laws.')]
  early[state_fips=='55',research_notes:='Original ch.121 covers 7-15; confirmed by contemporaneous Thayer judicial opinion. The later 1889 Bennett law covers 7-14 and is not backcast.']
  early[state_fips=='04', `:=`(law_year=1875L,historical_repeal=TRUE,
    age_source='https://en.wikisource.org/wiki/Page:History_of_Public_School_Education_in_Arizona.djvu/34',
    research_notes='Government Bureau of Education historical report identifies original 1875 ages 8-14 and 16 weeks. Repealed 1883; canonical 1899 is reintroduction. Year/month implementation remains approximate.')]
  early[state_fips=='48', `:=`(law_year=1870L,historical_repeal=TRUE,
    age_source='https://case-law.vlex.com/vid/peay-v-talbot-888251726',
    research_status='early_episode_year_uncertainty',
    research_notes='CLS cites an early 1870 law (1870 Tex. Gen. Laws 113), repealed in 1876. Contemporary court confirms scholastic ages 6-18 and four months; 1870 versus strengthened 1871 enforcement is uncertain. This is an annual early-episode approximation; also estimate the verified 1916 reintroduction separately.')]
  dated <- data.table(state_fips=c('01','13'),
    effective_date=c('1917-10-01','1917-01-01'),precision='verified_monthly',
    timing_source=cls,
    timing_reference=c('CLS Data Appendix p.3, footnote 2: 1915 Ala. Acts 534, effective October 1 1917.',
                       'CLS Data Appendix p.4, footnote 7: 1916 Ga. Laws 103, operative January 1 1917.'),
    school_calendar=c('Statute operative on and after October 1 1917; no common district school-opening date assumed.',
                     'Statute operative January 1 1917; no common district school-opening date assumed.'))
  partial <- data.table(state_fips=c('05','12','22','24','28','37','45','47','51'),
    earlier_partial_coverage=TRUE,
    research_notes=c(
      '1909 and 1911 laws exempted many counties; use statewide 1917. CLS Data Appendix p.4.',
      '1915 law opt-in; statewide 1919. CLS Data Appendix p.4.',
      '1910 law covered Orleans only; statewide 1916. CLS Data Appendix p.4.',
      'Baltimore city differs from non-Baltimore; CLS uses general non-city law. CLS Data Appendix p.4.',
      '1918 opt-in replaced by 1920 opt-out; some counties still opted out. CLS Data Appendix p.5; do not label universal realized enforcement.',
      'Selected counties 1901/1903 and opt-in 1907 predate statewide 1913. CLS Data Appendix p.5.',
      'District option 1915-1918 precedes statewide 1919. CLS Data Appendix p.5.',
      'County-specific population statutes from 1905 precede statewide 1913. CLS Data Appendix p.6.',
      '1908 opt-in weakly adopted; statewide 1918. CLS Data Appendix p.6.'))
  partial[state_fips=='22',research_notes:=paste(research_notes,
    'Gateway PDF p.89 calls 1914 the first law but describes coverage limited to cities above 25,000; this does not replace statewide 1916.')]
  partial[state_fips=='24',exemption_notes:=
    'Original 1902 law requires attendance at ages 8-12; ages 12-16 covered if not regularly employed. Use the oldest nominally covered age, not the universal attendance age. Gateway PDF p.97; CLS original-year codes.']
  conditional <- data.table(state_fips='29',exemption_notes=
    'Original 1905 law covers ages 8-14 generally and ages 14-16 if not employed; competency, distance and family support exemptions also apply. Oldest nominal coverage is conditional. Gateway PDF p.118; CLS original-year codes.')
  oregon <- data.table(state_fips='41',law_year=1890L,entry_age=8L,exit_age=14L,
    precision='annual_approximation',age_source=gateway,
    age_reference='Gateway PDF p.164; 1889 Oregon Acts p.111 sec.1',
    research_status='effective_year_sourced_month_unverified',
    research_notes='Original law passed in 1889; source narrative explicitly reports implementation in 1890. Use the effective-year annual approximation, preserving canonical 1889 for audit. Original within-year reference date remains unverified.',
    school_calendar='Effective year 1890; original school-opening date not verified.',
    exemption_notes='Original competency and distance (more than two miles) exemptions. Gateway PDF p.164.')
  # Merge explicit research records; default blank cells do not overwrite sourced fields.
  d <- rbindlist(list(early,dated,partial,conditional,oregon),fill=TRUE)
  result <- rbindlist(lapply(split(d,d$state_fips),function(s) {
    out <- s[1L]
    for (col in names(s)) {
      v <- s[[col]][!is.na(s[[col]])]
      if (is.character(v)) v <- v[nzchar(v)]
      if (length(v)) set(out,1L,col,tail(v,1L))
    }
    out
  }),fill=TRUE)
  setorder(result,state_fips)
  result
}
