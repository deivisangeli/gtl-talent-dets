source("analysis/mandatory_schooling/estimate_linked_1950_veteran_scientist.R")
set.seed(634)
n<-600L
d<-data.frame(birthyr=sample(1910:1927,n,TRUE),background=sample(c("1:US","2:US"),n,TRUE),birthplace=sample(c("13","6"),n,TRUE),county_1940=sample(paste0("6:",1:5),n,TRUE),veteran=rbinom(n,1,.6),scientist_1950=rbinom(n,1,.3),weight=sample(c(1,2,5,9),n,TRUE))
d$young<-d$birthyr>=1922
for(v in c(vs_common,vs_old_controls,vs_young_controls)) d[[v]]<-rnorm(n)
d<-vs_model_data(d)
controls<-c(vs_common,paste0("m_",c(vs_old_controls,vs_young_controls)))
for(spec in 1:3) {
  fit<-vs_fit(d,spec)
  fixed<-"factor(interaction(background,birthyr))"
  if(spec>=2) fixed<-paste(fixed,"factor(interaction(background,birthplace))","factor(interaction(background,county_1940))",sep=" + ")
  if(spec==3) fixed<-paste(fixed,paste0("background * (",paste(controls,collapse=" + "),")"),sep=" + ")
  reference<-lm(as.formula(paste("scientist_1950 ~ veteran +",fixed)),data=d,weights=weight)
  stopifnot(abs(coef(fit)["veteran"]-coef(reference)["veteran"])<1e-7)
  cluster<-vs_extract(fit,TRUE);hc1<-vs_extract(fit,FALSE)
  stopifnot(all(is.finite(cluster)),all(is.finite(hc1)),cluster["ci_lower"]<=cluster["estimate"],cluster["ci_upper"]>=cluster["estimate"])
  if(spec==3) {
    explicit<-feols(fit$vs_full_formula,data=d,weights=~weight,fixef.rm="none",fixef.tol=1e-9,notes=FALSE)
    # Check inference as well as coefficients against the direct interactions.
    stopifnot(max(abs(cluster-vs_extract(explicit,TRUE)))<1e-7,
      max(abs(hc1-vs_extract(explicit,FALSE)))<1e-7)
  }
}
x<-d[1:6,];x$background<-"1:US";x$birthyr<-1922L;x$veteran<-c(0,0,0,1,1,1);x$weight<-1:6;x$scientist_1950<-c(0,0,1,0,1,1)
fit<-vs_fit(x,1)
manual<-weighted.mean(x$scientist_1950[x$veteran==1],x$weight[x$veteran==1])-weighted.mean(x$scientist_1950[x$veteran==0],x$weight[x$veteran==0])
stopifnot(abs(coef(fit)["veteran"]-manual)<1e-10,abs(100*coef(fit)["veteran"]-23.3333333333)<1e-8,
  all(d$m_own_log_wage[d$young]==0),all(d$m_own_log_wage[!d$young]==d$own_log_wage[!d$young]),
  all(d$m_father_log_wage[d$young]==d$father_log_wage[d$young]),all(d$m_father_log_wage[!d$young]==0))
# Exercise file output and explicit unestimable groups through the real runner.
fixture<-d;fixture$respondent_id<-paste0("fixture",seq_len(n))
fixture$paper_group<-ifelse(fixture$background=="1:US","US-W","B")
fixture$scientist_1950[fixture$paper_group=="B"]<-0L
fixture$strict_link<-seq_len(n)%%2==0
fixture$linked_1940<-fixture$father_available<-fixture$fullcount_available<-fixture$regression_eligible<-TRUE
fixture$active_military_1950<-FALSE;fixture$exclusion_reason<-""
directory<-file.path(tempdir(),"veteran_scientist_analysis_fixture");dir.create(directory,showWarnings=FALSE)
arrow::write_parquet(fixture,file.path(directory,"data.parquet"))
Sys.setenv(IPUMS_VETERAN_SCIENTIST_PARQUET_DIR=directory,VETERAN_SCIENTIST_RESULTS_DIR=file.path(directory,"results"))
main()
out<-read.csv(file.path(directory,"results/coefficients.csv"))
stopifnot(all(out$status[out$group=="B" & out$cohort=="1922-1927"]=="No outcome variation"),
  all(is.na(out$estimate[out$status!="Estimated"])),any(out$status=="Estimated"),
  all(abs(out$estimate_pp[out$status=="Estimated"]-100*out$estimate[out$status=="Estimated"])<1e-10),
  file.exists(file.path(directory,"results/coefficients_full.png")))
cat("Weighted scientist regression fixtures passed\n")
