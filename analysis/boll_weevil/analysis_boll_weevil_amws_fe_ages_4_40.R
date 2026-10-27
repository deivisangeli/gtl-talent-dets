# Annual ages 4-18 and 31-40, with pooled ages 19-30 as the reference.
suppressPackageStartupMessages({library(data.table); library(fixest); library(ggplot2)})
arg <- grep("^--file=", commandArgs(FALSE), value=TRUE)
repo_root <- if(length(arg)) normalizePath(file.path(dirname(sub("^--file=", "", arg[1])), "../.."), winslash="/") else
  normalizePath(Sys.getenv("GTL_REPO", unset=getwd()), winslash="/")
source(file.path(repo_root, "paths.R"))
source(file.path(repo_root, "analysis/boll_weevil/twfe_helpers.R"))
source(file.path(repo_root, "analysis/boll_weevil/fe_age_window_helpers.R"))
base_dir <- file.path(TALENT_DETS_DATA_DIR, "results/boll_weevil")
output_dir <- file.path(base_dir, "amws_county_fe_ages_4_40")
dir.create(file.path(output_dir, "models"), recursive=TRUE, showWarnings=FALSE)
save_csv <- function(x, name) fwrite(x, file.path(output_dir, paste0(name, ".csv")), na="NA")
save_matrix <- function(x, name) save_csv(as.data.table(matrix(as.numeric(x), nrow=nrow(x),
  dimnames=dimnames(x)), keep.rownames="term"), name)
inputs <- c(panel=file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv"),
  roster=file.path(base_dir, "amws_county_twfe/counties_broader_590.csv"),
  frozen_panel=file.path(base_dir, "amws_county_twfe/estimation_panel_broader_590.csv"),
  old_coefficients=file.path(base_dir, "amws_county_fe_age_windows/annual_age_coefficients.csv"),
  old_windows=file.path(base_dir, "amws_county_fe_age_windows/age_window_tests.csv"))
bwa_require(all(file.exists(inputs)), "Required prior FE results or AMWS panel missing.")
input_hashes <- tools::md5sum(inputs)
prior_dirs <- setdiff(list.dirs(base_dir, recursive=FALSE, full.names=TRUE), output_dir)
protected <- sort(unlist(lapply(prior_dirs, list.files, recursive=TRUE, full.names=TRUE), use.names=FALSE))
protected_hashes <- tools::md5sum(protected)
roster <- fread(inputs["roster"], colClasses=c(GEOID="character"))
frozen <- fread(inputs["frozen_panel"], colClasses=c(GEOID="character"))
columns <- c("GEOID", "year", "n_amws", "n_amws_1906_1955_dedup", "n_amws_1986",
  "county_births_estimate_year", "amws_per_1000_births")
p <- fread(inputs["panel"], select=columns, colClasses=c(GEOID="character"))
bwa_require(nrow(roster)==590L && !anyDuplicated(roster$GEOID) && !anyDuplicated(p[,.(GEOID,year)]),
  "Unexpected county roster or duplicate source key.")
source_overlap <- merge(frozen[,..columns], p, by=c("GEOID","year"), suffixes=c("_frozen","_source"))
value_columns <- setdiff(columns,c("GEOID","year"))
frozen_error <- max(vapply(value_columns,function(n) max(abs(source_overlap[[paste0(n,"_frozen")]]-
  source_overlap[[paste0(n,"_source")]])),numeric(1)))
bwa_require(nrow(source_overlap)==15930L && frozen_error<1e-12, "Source outcomes differ from frozen FE data.")
d0 <- merge(p,roster,by="GEOID",sort=FALSE)
d0[,age_at_infestation:=as.integer(infestation_year-year)]
d0 <- d0[age_at_infestation %in% 4:40]
d0[,valid:=is.finite(amws_per_1000_births) & is.finite(county_births_estimate_year) &
  county_births_estimate_year>0 & is.finite(n_amws) & n_amws>=0 & n_amws==floor(n_amws)]
missing <- CJ(GEOID=roster$GEOID,age_at_infestation=4:40)[
  !d0[valid==TRUE,.(GEOID,age_at_infestation)],on=.(GEOID,age_at_infestation)]
missing <- merge(missing,roster[,.(GEOID,countyname,statename,infestation_year)],by="GEOID")
missing[,required_birth_year:=infestation_year-age_at_infestation]
missing <- merge(missing,d0[,.(GEOID,age_at_infestation,valid)],by=c("GEOID","age_at_infestation"),all.x=TRUE)
missing[,reason:=fifelse(is.na(valid),"source_county_birth_year_absent","invalid_outcome_or_birth_denominator")]
audit <- merge(roster,missing[,.(missing_ages=paste(sort(age_at_infestation),collapse=","),
  missing_observations=.N),by=GEOID],by="GEOID",all.x=TRUE)
audit[is.na(missing_observations),`:=`(missing_observations=0L,missing_ages="")]
audit[,included:=missing_observations==0L]
save_csv(audit,"county_inclusion_audit");save_csv(audit[included==FALSE],"excluded_counties")
save_csv(missing,"missing_age_observations")
d <- d0[GEOID %in% audit[included==TRUE,GEOID] & valid==TRUE]
d[,`:=`(id=as.integer(GEOID),model_y=amws_per_1000_births,valid=NULL)]
setorder(d,id,year)
bwa_require(nrow(d)==20535L && uniqueN(d$id)==555L && all(d[,.N,by=id]$N==37L), "Extended sample dimensions changed.")
bwa_require(all(d$age_at_infestation==d$infestation_year-d$year) && !anyDuplicated(d[,.(id,year)]),
  "Invalid exposure ages or duplicate keys.")
bwa_require(all(d$n_amws==d$n_amws_1906_1955_dedup+d$n_amws_1986), "AMWS edition components disagree.")
outcome_error <- max(abs(d$model_y-1000*d$n_amws/d$county_births_estimate_year))
bwa_require(outcome_error<1e-9, "Outcome arithmetic failed.")

estimated_ages <- c(4:18,31:40)
terms <- paste0("age_",estimated_ages)
for(a in estimated_ages) d[,(paste0("age_",a)):=as.integer(age_at_infestation==a)]
X <- as.matrix(d[,..terms])
bwa_require(all(rowSums(X[d$age_at_infestation %in% 19:30,,drop=FALSE])==0) &&
  all(rowSums(X[!d$age_at_infestation %in% 19:30,,drop=FALSE])==1), "Reference or age boundaries incorrect.")
within_rank <- qr(demean(X,f=list(d$id,d$year),tol=1e-10))$rank
bwa_require(within_rank==25L,"Extended age design lacks full rank.")
adjustment <- ssc(K.adj=TRUE,K.fixef="nonnested",G.adj=TRUE,G.df="min",t.df="min")
warnings <- list()
capture <- function(expr,stage) withCallingHandlers(expr,warning=function(w){
  warnings[[length(warnings)+1L]] <<- data.table(stage=stage,message=conditionMessage(w))
  invokeRestart("muffleWarning")
})
formula <- as.formula(paste("model_y ~",paste(terms,collapse=" + "),"| id + year"))
message("Fitting ages 4-40 on 555 counties, with pooled reference ages 19-30.")
fit <- capture(feols(formula,data=d,vcov=~id,ssc=adjustment,fixef.tol=1e-10),"extended")
V <- vcov(fit,attr=TRUE)
df <- attr(V,"df.t");K <- attr(V,"df.K")
bwa_require(nobs(fit)==20535L && length(coef(fit))==25L && df==554L,"Fit dropped rows/ages or has wrong inference df.")
annual_table <- function(fit,V,ages) {
  n <- paste0("age_",ages); df <- attr(V,"df.t")
  z <- data.table(age_at_infestation=ages,term=n,estimate=as.numeric(coef(fit)[n]),
    se=sqrt(diag(V[n,n])),reference_ages="19-30",df=df)
  z[,`:=`(conf_low=estimate-qt(.975,df)*se,conf_high=estimate+qt(.975,df)*se,
    p_value=2*pt(-abs(estimate/se),df))];z
}
annual <- annual_table(fit,V,estimated_ages)
contrasts <- bwa_inference(coef(fit),V,df)
# Include explicit zero weights for all ten older-age coefficients.
L <- matrix(0,6,25,dimnames=list(rownames(contrasts$L),terms))
L[,colnames(contrasts$L)] <- contrasts$L
contrast_error <- max(abs(as.numeric(L %*% coef(fit)[terms])-contrasts$table$estimate))
contrast_variance_error <- max(abs(L %*% V[terms,terms] %*% t(L)-contrasts$covariance))
bwa_require(contrast_error<1e-10 && contrast_variance_error<1e-10,"Extended contrast weights/covariance failed.")

message("Refitting ages 4-30 on the same 555 counties.")
short <- d[age_at_infestation %in% 4:30]
short_terms <- paste0("age_",4:18)
short_fit <- capture(feols(as.formula(paste("model_y ~",paste(short_terms,collapse=" + "),"| id + year")),
  data=short,vcov=~id,ssc=adjustment,fixef.tol=1e-10),"same_counties_4_30")
short_V <- vcov(short_fit,attr=TRUE)
short_annual <- annual_table(short_fit,short_V,4:18)
short_contrasts <- bwa_inference(coef(short_fit),short_V,554L)
bwa_require(nobs(short_fit)==14985L && attr(short_V,"df.t")==554L,"Same-county benchmark dimensions failed.")
old_annual <- fread(inputs["old_coefficients"])
old_windows <- fread(inputs["old_windows"])
comparison <- rbindlist(list(
  cbind(data.table(specification="previous_590_ages_4_30",counties=590L,observations=15930L),old_windows),
  cbind(data.table(specification="same_555_ages_4_30",counties=555L,observations=14985L),short_contrasts$table),
  cbind(data.table(specification="extended_555_ages_4_40",counties=555L,observations=20535L),contrasts$table)),fill=TRUE)
annual_comparison <- rbindlist(list(
  cbind(data.table(specification="previous_590_ages_4_30"),old_annual),
  cbind(data.table(specification="same_555_ages_4_30"),short_annual),
  cbind(data.table(specification="extended_555_ages_4_40"),annual)),fill=TRUE)

message("Validating both fits using independent FWL coefficients and clustered covariance.")
check_fit <- function(fit,V,z,terms,stage) {
  independent <- bwfe_fwl(z$model_y,as.matrix(z[,..terms]),z$id,z$year)
  factor <- (nrow(z)-1)/(nrow(z)-attr(V,"df.K"))*555/554
  U <- vcov(fit,vcov=~id,ssc=ssc(K.adj=FALSE,G.adj=FALSE))
  errors <- c(coefficient=max(abs(coef(fit)[terms]-independent$beta[terms])),
    residual=max(abs(resid(fit)-independent$residual)),
    unadjusted_covariance=max(abs(U[terms,terms]-independent$covariance_unadjusted[terms,terms])),
    adjusted_covariance=max(abs(V[terms,terms]-factor*independent$covariance_unadjusted[terms,terms])))
  bwa_require(all(errors<1e-8),paste("Independent FWL validation failed:",stage))
  data.table(stage=stage,check=names(errors),observed=as.numeric(errors),expected=0,tolerance=1e-8)
}
validation <- rbindlist(list(
  data.table(stage="sample_and_design",check=c("counties","observations","age_indicators","within_rank",
    "cluster_t_df","reference_observations","primary_observations","age_min_count","age_max_count",
    "excluded_counties","frozen_source_error","outcome_error","contrast_error","contrast_covariance_error","holm_family_size"),
    observed=c(uniqueN(d$id),nrow(d),length(coef(fit)),within_rank,df,sum(d$age_at_infestation%in%19:30),
      sum(d$age_at_infestation%in%14:18),min(d[,.N,by=age_at_infestation]$N),max(d[,.N,by=age_at_infestation]$N),
      nrow(audit[included==FALSE]),frozen_error,outcome_error,contrast_error,contrast_variance_error,nrow(contrasts$table)),
    expected=c(555,20535,25,25,554,6660,2775,555,555,35,0,0,0,0,6),tolerance=c(rep(0,10),rep(1e-9,4),0)),
  check_fit(fit,V,d,terms,"extended_4_40"),check_fit(short_fit,short_V,short,short_terms,"benchmark_4_30")))
validation[,passed:=abs(observed-expected)<=tolerance]
bwa_require(all(validation$passed),"An extended-age validation failed.")
save_csv(validation,"validation_checks")
save_csv(annual,"annual_age_coefficients");save_csv(contrasts$table,"age_window_tests")
save_matrix(V,"annual_coefficient_covariance");save_matrix(contrasts$covariance,"contrast_covariance")
save_matrix(L,"contrast_weights");save_csv(comparison,"specification_comparison")
save_csv(annual_comparison,"annual_specification_comparison")
save_csv(short_annual,"benchmark_555_ages_4_30_coefficients")
save_matrix(short_V,"benchmark_555_ages_4_30_covariance")
save_csv(d,"estimation_panel");save_csv(roster[GEOID %in% d$GEOID],"counties")
save_csv(d[,.(observations=.N,scientists=sum(n_amws),estimated_births=sum(county_births_estimate_year),
  mean_county_rate=mean(model_y)),by=age_at_infestation][order(age_at_infestation)],"age_descriptives")
save_csv(data.table(counties=555L,observations=nrow(d),age_min=4L,age_max=40L,reference_ages="19-30",
  first_birth_year=min(d$year),last_birth_year=max(d$year),within_rank=within_rank,cluster_t_df=df,
  adjusted_parameter_count=K,finite_sample_covariance_factor=(nrow(d)-1)/(nrow(d)-K)*555/554,
  annual_interval="pointwise 95% t",weights="equal observations",holm_family_size=6L),"model_summary")
save_csv(if(length(warnings))rbindlist(warnings)else data.table(stage="all",message="none"),"estimation_warnings")
saveRDS(list(fit=fit,covariance=V,annual=annual,contrasts=contrasts,full_contrast_weights=L,
  benchmark_555=short_fit,benchmark_covariance=short_V,benchmark_contrasts=short_contrasts,
  input_hashes=input_hashes),file.path(output_dir,"models/annual_age_FE_and_contrasts.rds"),compress="gzip")

annual[,segment:=fifelse(age_at_infestation<=18,"school_age","older")]
profile <- ggplot(annual,aes(age_at_infestation,estimate)) +
  annotate("rect",xmin=18.5,xmax=30.5,ymin=-Inf,ymax=Inf,fill="#E8EAED",alpha=.75) +
  annotate("rect",xmin=13.5,xmax=18.5,ymin=-Inf,ymax=Inf,fill="#FAE5D5",alpha=.85) +
  geom_hline(yintercept=0,colour="grey50",linetype="dashed") +
  geom_errorbar(aes(ymin=conf_low,ymax=conf_high),width=.18,colour="#245B78") +
  geom_line(aes(group=segment),colour="#245B78",linewidth=.6) + geom_point(colour="#245B78",size=2) +
  annotate("text",x=24.5,y=Inf,label="Ages 19-30\nPooled reference\n(no separate estimates)",vjust=1.3,size=3.5,colour="#555555") +
  scale_x_reverse(breaks=40:4) +
  labs(title="Boll weevil: exposure age and scientist generation",
    subtitle="555 counties; county and birth-year fixed effects; reference ages 19-30",
    x="Age at infestation (birth county proxies schooling location)",
    y="Effect on AMWS scientists born\nper 1,000 estimated births",
    caption="Orange: ages 14-18. Grey: pooled reference ages 19-30. Bars: pointwise 95% confidence intervals.\nSeparate indicators for ages 4-18 and 31-40; equal observation weights; county-clustered t inference (554 df).") +
  theme_minimal(base_size=12) + theme(panel.grid.minor=element_blank(),axis.text.x=element_text(size=9),
    plot.caption=element_text(hjust=0))
ggsave(file.path(output_dir,"annual_age_profile.png"),profile,width=14,height=7,dpi=220,bg="white")

rows <- vapply(seq_len(nrow(contrasts$table)),function(i){z<-contrasts$table[i];sprintf(
  "| %s | %.4f | %.4f | [%.4f, %.4f] | %.4f | %.4f |",z$contrast,z$estimate,z$se,z$conf_low,z$conf_high,z$p_value,z$p_holm)},character(1))
comparison_rows <- vapply(seq_len(nrow(comparison[contrast%in%c("4-9","14-18")])),function(i){
  z<-comparison[contrast%in%c("4-9","14-18")][i]
  sprintf("| %s | %s | %.4f | [%.4f, %.4f] |",z$specification,z$contrast,z$estimate,z$conf_low,z$conf_high)},character(1))
writeLines(c("# Boll weevil FE profile: ages 4-40","",
  "The extended model uses 555 of the previous 590 counties, with 20,535 observations: every age 4-40 is valid for every included county. The 35 excluded counties have missing older county-birth-year observations, detailed in county_inclusion_audit.csv and missing_age_observations.csv. Birth years span 1853-1918.","",
  "## Specification and annual profile","",
  "OLS includes county and birth-year fixed effects, with 25 separate age indicators for ages 4-18 and 31-40. Ages 19-30 remain the pooled omitted category (6,660 observations). All observations receive equal weight. Inference clusters by county, uses the nonnested-FE small-sample adjustment, and uses 554 t degrees of freedom. No covariates, county trends, population weights, or new CS DID specifications are added.","",
  "![Annual exposure-age estimates](annual_age_profile.png)","",
  "Bars are pointwise 95% confidence intervals, not simultaneous bands. The grey ages-19-30 region is a pooled reference: there are no separately estimated effects or confidence intervals for those ages. Orange shading marks ages 14-18, the previously selected primary high-school window; age at arrival is not observed high-school attendance.","",
  "## Window averages and difference","",
  "| Age window / contrast | Estimate | Clustered SE | Pointwise 95% CI | p-value | Holm p-value |",
  "| --- | ---: | ---: | --- | ---: | ---: |",rows,"",
  "Each window gives equal weight to its annual-age coefficients. The full joint covariance determines all contrast variances, including overlapping windows and the high-school-minus-childhood difference. Holm adjustment covers these six tests together; annual-age coefficients are outside that family. Failure to reject does not establish absence of an effect.","",
  "## Separating county selection from the extended age window","",
  "| Specification | Window | Estimate | Pointwise 95% CI |","| --- | --- | ---: | --- |",comparison_rows,"",
  "The previous 590-county annual-age model is context. The ages-4-30 refit uses the same 555 counties as the extended model, with 14,985 observations and the same pooled ages-19-30 reference. Comparing the first two models changes county composition; comparing the latter two adds ages 31-40 and their separate indicators, which also changes the birth-year fixed-effect fit. These comparisons are descriptive and are not significance tests of differences between models.","",
  "## Validation and interpretation","",
  "The 25-indicator within design has full rank. Unique keys, complete age coverage, treatment-age arithmetic, edition components, reference coding, and rate arithmetic were verified. Source outcomes and denominators match the frozen previous FE panel. Independent FWL residualization verifies coefficients, residuals, and unadjusted and adjusted county-clustered covariance for both new fits. Contrast weights explicitly assign zero weight to older-age coefficients. Input and prior-result checksums are verified unchanged.","",
  "Older-age coefficients compare exposure at ages 31-40 with the pooled ages-19-30 reference. They do not establish that adult exposure leaves scientist generation unaffected. All included counties eventually experienced infestation. A causal interpretation still requires suitable county-specific cohort trends and an unaffected reference for scientist generation. County of birth proxies schooling location; historical boundaries, childhood migration, and estimated rather than observed county birth denominators remain limitations.","",
  "The folder contains annual coefficients, full joint covariance, six window tests and weights, both fitted models, the exact estimation panel and county roster, exclusion audits, specification comparisons, validation checks, warnings, checksums, and session information."),
  file.path(output_dir,"results_summary.md"))
code_paths <- file.path(repo_root,c("paths.R","analysis/boll_weevil/twfe_helpers.R",
  "analysis/boll_weevil/fe_age_window_helpers.R","analysis/boll_weevil/analysis_boll_weevil_amws_fe_ages_4_40.R"))
manifest_paths <- c(inputs,code_paths)
save_csv(data.table(path=unname(normalizePath(manifest_paths,winslash="/")),md5=unname(tools::md5sum(manifest_paths))),"input_and_code_manifest")
protection <- data.table(path=unname(normalizePath(protected,winslash="/")),md5_before=unname(protected_hashes),
  md5_after=unname(tools::md5sum(protected)))
protection[,unchanged:=md5_before==md5_after];save_csv(protection,"prior_results_protection")
bwa_require(all(protection$unchanged) && identical(unname(input_hashes),unname(tools::md5sum(inputs))),
  "Source input or prior result changed.")
writeLines(capture.output(sessionInfo()),file.path(output_dir,"session_info.txt"))
message("Complete: ",output_dir)
print(contrasts$table[,.(contrast,estimate,se,conf_low,conf_high,p_value,p_holm)])
