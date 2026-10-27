# Flexible exposure-age FE model on exactly the frozen broader FE590 sample.
suppressPackageStartupMessages({library(data.table);library(fixest);library(ggplot2)})
arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
repo_root <- if(length(arg)) normalizePath(file.path(dirname(sub("^--file=","",arg[1])),"../.."),winslash="/") else
  normalizePath(Sys.getenv("GTL_REPO",unset=getwd()),winslash="/")
source(file.path(repo_root,"paths.R"))
source(file.path(repo_root,"analysis/boll_weevil/twfe_helpers.R"))
source(file.path(repo_root,"analysis/boll_weevil/fe_age_window_helpers.R"))
base_dir <- file.path(TALENT_DETS_DATA_DIR,"results/boll_weevil")
legacy_dir <- file.path(base_dir,"amws_county_twfe")
output_dir <- file.path(base_dir,"amws_county_fe_age_windows")
dir.create(file.path(output_dir,"models"),recursive=TRUE,showWarnings=FALSE)
save_csv <- function(x,name) fwrite(x,file.path(output_dir,paste0(name,".csv")),na="NA")
save_matrix <- function(x,name) {
  values <- matrix(as.numeric(x),nrow=nrow(x),dimnames=dimnames(x))
  save_csv(as.data.table(values,keep.rownames="term"),name)
}
inputs <- c(panel=file.path(legacy_dir,"estimation_panel_broader_590.csv"),
  legacy_model=file.path(legacy_dir,"models/broader_590.rds"),
  legacy_counties=file.path(legacy_dir,"counties_broader_590.csv"))
bwa_require(all(file.exists(inputs)),"Run the broader FE590 analysis before the age-window extension.")
input_hashes <- tools::md5sum(inputs)
prior_dirs <- list.dirs(base_dir,recursive=FALSE,full.names=TRUE)
prior_dirs <- prior_dirs[normalizePath(prior_dirs,winslash="/") != normalizePath(output_dir,winslash="/")]
protected <- sort(unlist(lapply(prior_dirs,list.files,recursive=TRUE,full.names=TRUE),use.names=FALSE))
protected_hashes <- tools::md5sum(protected)
original <- fread(inputs["panel"],colClasses=c(GEOID="character"))
roster <- fread(inputs["legacy_counties"],colClasses=c(GEOID="character"))
legacy_saved <- readRDS(inputs["legacy_model"])
d <- copy(original)
bwa_require(nrow(d)==15930L && uniqueN(d$id)==590L && !anyDuplicated(d[,.(id,year)]),"Unexpected frozen sample dimensions.")
bwa_require(setequal(d$GEOID,roster$GEOID) && all(d[,.N,by=id]$N==27L),"The county roster or age coverage changed.")
bwa_require(all(d$age_at_infestation==d$infestation_year-d$year) &&
  setequal(d$age_at_infestation,4:30),"Invalid birth-cohort exposure alignment.")
bwa_require(all(is.finite(d$model_y)) && all(d$county_births_estimate_year>0) &&
  max(abs(d$model_y-1000*d$n_amws/d$county_births_estimate_year))<1e-9,"Invalid outcome or denominator.")
bwa_require(all(d$n_amws==d$n_amws_1906_1955_dedup+d$n_amws_1986),"AMWS edition components disagree.")
adjustment <- ssc(K.adj=TRUE,K.fixef="nonnested",G.adj=TRUE,G.df="min",t.df="min")
warnings <- list()
capture <- function(expr,stage) withCallingHandlers(expr,warning=function(w){
  warnings[[length(warnings)+1L]] <<- data.table(stage=stage,message=conditionMessage(w))
  invokeRestart("muffleWarning")
})
message("Reproducing the original five-bin FE590 benchmark.")
benchmark <- capture(feols(model_y~exposure_4_6+exposure_7_9+exposure_10_12+exposure_13_15+exposure_16_18|id+year,
  data=d,vcov=~id,ssc=adjustment,fixef.tol=1e-10),"benchmark")
old_beta <- coef(legacy_saved$fit)
old_covariance <- legacy_saved$adjusted_covariance
benchmark_error <- max(abs(coef(benchmark)-old_beta[names(coef(benchmark))]))
benchmark_covariance_error <- max(abs(vcov(benchmark)-old_covariance))
bwa_require(benchmark_error<1e-9 && benchmark_covariance_error<1e-9,"The original five-bin regression was not reproduced.")
save_csv(data.table(term=names(coef(benchmark)),estimate=as.numeric(coef(benchmark)),
  original_estimate=as.numeric(old_beta[names(coef(benchmark))]),
  absolute_error=abs(coef(benchmark)-old_beta[names(coef(benchmark))])),"benchmark_coefficients")
save_matrix(vcov(benchmark),"benchmark_covariance")

indicators <- bwa_indicators(d$age_at_infestation)
d <- cbind(d,indicators)
terms <- names(indicators)
bwa_require(identical(d[,names(original),with=FALSE],original),"Adding indicators changed a frozen input value or row.")
X <- as.matrix(d[,terms,with=FALSE])
within_rank <- qr(demean(X,f=list(d$id,d$year),tol=1e-10))$rank
bwa_require(within_rank==15L,"Annual-age indicators do not have full within rank.")
formula <- as.formula(paste("model_y ~",paste(terms,collapse=" + "),"| id + year"))
message("Estimating 15 individual exposure ages with county and birth-year fixed effects.")
fit <- capture(feols(formula,data=d,vcov=~id,ssc=adjustment,fixef.tol=1e-10),"annual_age_FE")
bwa_require(nobs(fit)==15930L && length(coef(fit))==15L,"The annual model dropped observations or coefficients.")
V <- vcov(fit,attr=TRUE)
df <- attr(V,"df.t")
bwa_require(df==589L,"Clustered t degrees of freedom differ from 589.")
annual <- data.table(age_at_infestation=4:18,term=terms,estimate=as.numeric(coef(fit)[terms]),
  se=sqrt(diag(V[terms,terms])),reference_ages="19-30",df=df)
annual[,`:=`(conf_low=estimate-qt(.975,df)*se,conf_high=estimate+qt(.975,df)*se)]
contrasts <- bwa_inference(coef(fit),V,df)
inference <- contrasts$table
L <- contrasts$L
save_csv(annual,"annual_age_coefficients")
save_csv(inference,"age_window_tests")
save_matrix(V,"annual_coefficient_covariance")
save_matrix(contrasts$covariance,"contrast_covariance")
save_csv(as.data.table(L,keep.rownames="contrast"),"contrast_weights")
save_csv(d,"estimation_panel")
save_csv(unique(d[,.(GEOID,id,source_county_id,countyname,statename,infestation_year)]),"counties")
save_csv(d[,.(observations=.N,scientists=sum(n_amws),estimated_births=sum(county_births_estimate_year),
  mean_county_rate=mean(model_y)),by=age_at_infestation],"age_descriptives")
windows <- list(`4-9`=4:9,`10-13`=10:13,`14-18`=14:18,`13-18`=13:18,`16-18`=16:18,
                `19-30_reference`=19:30)
window_descriptives <- rbindlist(lapply(names(windows),function(w) {
  z<-d[age_at_infestation %in% windows[[w]]]
  data.table(window=w,counties=uniqueN(z$id),annual_ages=length(windows[[w]]),observations=nrow(z),
    scientists=sum(z$n_amws),early_editions=sum(z$n_amws_1906_1955_dedup),edition_1986=sum(z$n_amws_1986),
    estimated_births=sum(z$county_births_estimate_year),mean_county_rate=mean(z$model_y))
}))
save_csv(window_descriptives,"window_descriptives")

message("Independently checking annual coefficients and county-clustered covariance by FWL.")
independent <- bwfe_fwl(d$model_y,X,d$id,d$year)
unadjusted <- vcov(fit,vcov=~id,ssc=ssc(K.adj=FALSE,G.adj=FALSE))
coefficient_error <- max(abs(coef(fit)[terms]-independent$beta[terms]))
covariance_error <- max(abs(unadjusted[terms,terms]-independent$covariance_unadjusted[terms,terms]))
residual_error <- max(abs(resid(fit)-independent$residual))
manual_theta <- as.numeric(L %*% independent$beta[terms])
manual_contrast_error <- max(abs(manual_theta-inference$estimate))
# Verify the exact finite-sample correction independently of fixest's covariance builder.
K <- attr(V,"df.K")
factor <- (nrow(d)-1)/(nrow(d)-K)*590/589
adjusted_covariance_error <- max(abs(V[terms,terms]-factor*independent$covariance_unadjusted[terms,terms]))
inference_error <- max(abs(diag(L %*% (factor*independent$covariance_unadjusted[terms,terms]) %*% t(L))-
  inference$se^2))
validation <- data.table(check=c("observations","counties","reference_observations","observations_per_annual_age",
  "primary_high_school_observations","within_design_rank","t_degrees_freedom","same_frozen_values",
  "benchmark_coefficient_error","benchmark_covariance_error","annual_coefficient_error",
  "unadjusted_cluster_covariance_error","adjusted_cluster_covariance_error","residual_error",
  "contrast_estimate_error","contrast_variance_error","holm_family_size"),
  observed=c(nrow(d),uniqueN(d$id),sum(d$age_at_infestation%in%19:30),min(d[,.N,by=age_at_infestation]$N),
    sum(d$age_at_infestation%in%14:18),within_rank,df,as.integer(identical(d[,names(original),with=FALSE],original)),
    benchmark_error,benchmark_covariance_error,coefficient_error,covariance_error,adjusted_covariance_error,
    residual_error,manual_contrast_error,inference_error,length(inference$p_value)),
  expected=c(15930,590,7080,590,2950,15,589,1,rep(0,8),6),
  tolerance=c(rep(0,8),rep(1e-8,8),0))
validation[,passed:=abs(observed-expected)<=tolerance]
save_csv(validation,"validation_checks")
bwa_require(all(validation$passed),"Age-window numerical validation failed.")
save_csv(data.table(iterations=independent$iterations,coefficient_error=coefficient_error,
  covariance_error=covariance_error,adjusted_covariance_error=adjusted_covariance_error,
  residual_error=residual_error,max_county_mean=independent$max_county_mean,
  max_birth_year_mean=independent$max_year_mean),"independent_fwl_validation")
summary <- data.table(counties=590L,observations=15930L,reference_ages="19-30",reference_observations=7080L,
  primary_window="14-18",primary_observations=2950L,annual_age_indicators=15L,within_rank=within_rank,
  first_birth_year=min(d$year),last_birth_year=max(d$year),cluster_t_df=df,
  adjusted_parameter_count=K,finite_sample_covariance_factor=factor,holm_family_size=6L,
  weights="equal county-age observations",covariance="county clustered, nonnested-FE small-sample adjustment")
save_csv(summary,"model_summary")
save_csv(if(length(warnings))rbindlist(warnings)else data.table(stage="all",message="none"),"estimation_warnings")
saveRDS(list(fit=fit,covariance=V,annual=annual,contrasts=contrasts,independent_fwl=independent,
  benchmark_refit=benchmark,summary=summary,input_hashes=input_hashes),
  file.path(output_dir,"models/annual_age_FE_and_contrasts.rds"),compress="gzip")

profile <- ggplot(annual,aes(age_at_infestation,estimate)) +
  annotate("rect",xmin=14,xmax=18,ymin=-Inf,ymax=Inf,fill="#FAE5D5",alpha=.75) +
  geom_hline(yintercept=0,colour="grey50",linetype="dashed") +
  geom_errorbar(aes(ymin=conf_low,ymax=conf_high),width=.12,colour="#245B78") +
  geom_line(colour="#245B78",linewidth=.6) + geom_point(colour="#245B78",size=2) +
  scale_x_reverse(breaks=18:4) +
  labs(title="Boll weevil: exposure age and scientist generation",
    subtitle="590 counties; county and birth-year fixed effects; reference ages 19-30",
    x="Age at infestation (birth county proxies schooling location)",
    y="Effect on AMWS scientists born\nper 1,000 estimated births",
    caption="Orange shading: primary high-school window, ages 14-18. Bars: pointwise 95% intervals.\nSeparate indicators for ages 4-18; equal observation weights; county-clustered t inference (589 df).") +
  theme_minimal(base_size=12) + theme(panel.grid.minor=element_blank(),plot.caption=element_text(hjust=0))
ggsave(file.path(output_dir,"annual_age_profile.png"),profile,width=11,height=6.5,dpi=220,bg="white")
labels <- c(`4-9`="Early childhood: 4-9",`10-13`="Intermediate: 10-13",`14-18`="High school: 14-18 (primary)",
  `13-18`="Adolescent sensitivity: 13-18",`16-18`="Later high school: 16-18",
  `14-18_minus_4-9`="Difference: high school minus childhood")
plot_data <- copy(inference)
plot_data[,label:=factor(labels[contrast],levels=rev(labels))]
forest <- ggplot(plot_data,aes(label,estimate,colour=primary)) +
  geom_hline(yintercept=0,colour="grey50",linetype="dashed") +
  geom_errorbar(aes(ymin=conf_low,ymax=conf_high),width=.15) +geom_point(size=2.7) +coord_flip() +
  scale_colour_manual(values=c(`FALSE`="#245B78",`TRUE`="#BA642F"),guide="none") +
  labs(title="Boll weevil: alternative exposure-age windows",
    subtitle="Same 590 counties and 15,930 observations in every contrast",
    x=NULL,y="Effect on AMWS scientists born per 1,000 estimated births",
    caption="Pointwise 95% county-clustered t intervals. Window effects average individual-age coefficients equally.\nReference: ages 19-30. Holm-adjusted p-values for this six-test family are provided in the results table.") +
  theme_minimal(base_size=12) +theme(plot.caption=element_text(hjust=0))
ggsave(file.path(output_dir,"age_window_comparison.png"),forest,width=12,height=7,dpi=220,bg="white")

primary <- inference[primary==TRUE]
primary_raw <- window_descriptives[window=="14-18"]
table_rows <- vapply(seq_len(nrow(inference)),function(i){
  z<-inference[i]
  sprintf("| %s | %.4f | %.4f | [%.4f, %.4f] | %.4f | %.4f |",z$contrast,z$estimate,z$se,
    z$conf_low,z$conf_high,z$p_value,z$p_holm)
},character(1))
writeLines(c(
  "# FE estimates for alternative boll weevil exposure-age windows", "",
  "## Primary high-school window", "",
  sprintf("For ages 14-18 at infestation, the estimated effect is %.4f AMWS scientists per 1,000 estimated births (95%% CI [%.4f, %.4f]; two-sided p=%.4f; six-test Holm-adjusted p=%.4f). This window was selected before estimation. It measures age at arrival, not observed high-school attendance.",
    primary$estimate,primary$conf_low,primary$conf_high,primary$p_value,primary$p_holm), "",
  "## All prespecified tests", "",
  "| Age window / contrast | Estimate | Clustered SE | Pointwise 95% CI | p-value | Holm p-value |",
  "| --- | ---: | ---: | --- | ---: | ---: |",table_rows,"",
  "The six-test family consists of five age-window effects and the high-school-minus-early-childhood contrast. Holm adjustment is applied to all six two-sided tests together; it does not turn the displayed pointwise intervals into simultaneous confidence intervals. Annual-age profile intervals are descriptive pointwise intervals and are not part of the six window tests. All specified windows are reported regardless of significance.", "",
  "## Sample and specification", "",
  sprintf("The sample is exactly the frozen broader FE sample: 590 counties and 15,930 county-birth-year observations, with all 27 exposure ages 4-30 observed in each county. Birth years span 1863-1918. The omitted ages 19-30 supply 7,080 reference observations. Neither county selection, row selection, AMWS outcome, denominator, nor reference category has changed. The primary high-school window contains %d observations and %d AMWS scientist births.",primary_raw$observations,primary_raw$scientists),
  "OLS includes county and birth-year fixed effects plus separate indicators for every age at infestation from 4 through 18. Each indicator is relative to the pooled ages-19-30 reference. The old model constrained each three-year bin to have one common coefficient; this model allows age effects to vary within those bins. Consequently, averages can differ from the older grouped-model coefficients even on identical observations.",
  "All observations receive equal weight. Each window averages its annual-age coefficients equally, and every county supplies the same number of observations per age. The high-school-minus-childhood contrast subtracts the ages-4-9 average from the ages-14-18 average. All standard errors use the full joint covariance, including covariance across overlapping windows; no standard errors are averaged.",
  "Inference uses county-clustered covariance with fixest's nonnested-FE finite-sample adjustment and t critical values with 589 degrees of freedom. No county trends, population weights, family-background controls, or other covariates are added. No new CS DID model is estimated.", "",
  sprintf("![Exposure-age profile](%s)",normalizePath(file.path(output_dir,"annual_age_profile.png"),winslash="/")),"",
  sprintf("![Window contrasts](%s)",normalizePath(file.path(output_dir,"age_window_comparison.png"),winslash="/")),"",
  "## Validation and interpretation", "",
  "The original five-bin regression and adjusted covariance were reproduced before fitting the new model. All 15 annual-age indicators have full within rank. Independent alternating-projection FWL calculations verify the new coefficients, residuals, unadjusted clustered covariance, finite-sample adjustment, and contrast variances. Helper tests verify age boundaries, the older reference, equal weights, covariance of overlapping windows, difference contrasts, and the six-test Holm adjustment. Frozen inputs and all prior FE/CS result files are checksummed and verified unchanged.",
  "These estimates use county of birth as a schooling-location proxy and estimated county births as the denominator. Historical boundaries and childhood migration remain measurement limitations. A causal interpretation requires comparable untreated cohort trends and an unaffected older comparison group for scientist generation. Confidence intervals including zero do not establish no effect; differences among age windows must be judged using their joint covariance rather than separate significance labels.","",
  "## Files", "",
  "annual_age_coefficients.csv and age_window_tests.csv report coefficients and tests. contrast_weights.csv defines every estimand. annual_coefficient_covariance.csv and contrast_covariance.csv retain full covariance. estimation_panel.csv, counties.csv, and descriptive tables identify the unchanged sample. benchmark files, validation_checks.csv, independent_fwl_validation.csv, saved models, provenance, warnings, and session information support reproduction."
),file.path(output_dir,"results_summary.md"))
code_paths <- file.path(repo_root,c("paths.R","analysis/boll_weevil/twfe_helpers.R",
  "analysis/boll_weevil/fe_age_window_helpers.R","analysis/boll_weevil/analysis_boll_weevil_amws_fe_age_windows.R"))
manifest_paths <- c(inputs,code_paths)
save_csv(data.table(path=unname(normalizePath(manifest_paths,winslash="/")),
  md5=unname(tools::md5sum(manifest_paths))),"input_and_code_manifest")
protection <- data.table(path=unname(normalizePath(protected,winslash="/")),
  md5_before=unname(protected_hashes),md5_after=unname(tools::md5sum(protected)))
protection[,unchanged:=md5_before==md5_after]
save_csv(protection,"prior_results_protection")
bwa_require(all(protection$unchanged) && identical(unname(input_hashes),unname(tools::md5sum(inputs))),
            "A frozen input or prior result changed.")
writeLines(capture.output(sessionInfo()),file.path(output_dir,"session_info.txt"))
message("Complete: ",output_dir)
print(inference[,.(contrast,estimate,se,conf_low,conf_high,p_value,p_holm)])
