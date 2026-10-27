# Weighted linear probability models for WWII veteran status and scientist work.
suppressPackageStartupMessages({library(DBI);library(duckdb);library(fixest);library(ggplot2)})
vs_repo_root <- function() {
  arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
  root <- if(length(arg)) dirname(normalizePath(sub("^--file=","",arg[1]),winslash="/")) else Sys.getenv("GTL_REPO",getwd())
  while(!file.exists(file.path(root,"paths.R"))) {
    parent<-dirname(root); if(identical(parent,root)) stop("Cannot locate repository")
    root<-parent
  }
  root
}
source(file.path(vs_repo_root(),"prep/mandatory_schooling/build_linked_1950_veteran_scientist.R"))

vs_model_data <- function(d) {
  for(n in vs_old_controls) d[[paste0("m_",n)]] <- ifelse(!d$young,d[[n]],0)
  for(n in vs_young_controls) d[[paste0("m_",n)]] <- ifelse(d$young,d[[n]],0)
  d
}
vs_fit <- function(d,specification) {
  controls<-c(vs_common,paste0("m_",c(vs_old_controls,vs_young_controls)))
  varying<-controls[vapply(d[controls],function(x) length(unique(x))>1L,logical(1))]
  fixed<-"background^birthyr"
  if(specification>=2L) fixed<-paste(fixed,"background^birthplace","background^county_1940",sep=" + ")
  if(specification==3L && length(varying)) {
    # All nuisance terms vary by background, so their design is block diagonal.
    # Weighted Frisch-Waugh-Lovell within each background avoids the unstable
    # varying-slope iteration and a multi-gigabyte dense interaction matrix.
    ry<-rv<-numeric(nrow(d));nuisance_rank<-0L
    for(indices in split(seq_len(nrow(d)),d$background)) {
      block<-d[indices,,drop=FALSE]
      raw_matrix<-cbind(outcome=block$scientist_1950,treatment=block$veteran,as.matrix(block[varying]))
      dm<-fixest::demean(raw_matrix,f=list(factor(block$birthyr),factor(block$birthplace),factor(block$county_1940)),weights=block$weight,nthreads=8,iter=20000,tol=1e-9,notes=FALSE)
      root_weight<-sqrt(block$weight)
      wx<-dm[,-(1:2),drop=FALSE]*root_weight
      keep<-sqrt(colSums(wx^2)) > 1e-7*pmax(1,sqrt(colSums((raw_matrix[,-(1:2),drop=FALSE]*root_weight)^2)))
      wyv<-dm[,1:2,drop=FALSE]*root_weight
      if(any(keep)) {
        decomposition<-qr(wx[,keep,drop=FALSE],tol=1e-7)
        residuals<-qr.resid(decomposition,wyv)
        nuisance_rank<-nuisance_rank+decomposition$rank
      } else residuals<-wyv
      ry[indices]<-residuals[,1]/root_weight
      rv[indices]<-residuals[,2]/root_weight
    }
    vs_fail(sum(d$weight*rv^2)<1e-12,"No identifying veteran variation after prewar controls")
    d$scientist_1950<-ry;d$veteran<-rv
    formula<-as.formula(paste("scientist_1950 ~ veteran |",fixed))
    model<-feols(formula,data=d,weights=~weight,fixef.rm="none",fixef.tol=1e-9,fixef.iter=20000,notes=FALSE)
    model$vs_nuisance_rank<-nuisance_rank
    model$vs_full_formula<-as.formula(paste("scientist_1950 ~ veteran +",paste(sprintf("i(background,%s)",varying),collapse=" + "),"|",fixed))
    return(model)
  }
  formula<-as.formula(paste("scientist_1950 ~ veteran |",fixed))
  feols(formula,data=d,weights=~weight,fixef.rm="none",fixef.tol=1e-8,fixef.iter=20000,notes=FALSE)
}
vs_extract <- function(model,county=TRUE) {
  vc<-if(county) ~county_1940 else "hetero"
  tab<-coeftable(model,vcov=vc)
  vs_fail(!"veteran" %in% rownames(tab),"Veteran coefficient is unidentified after fixed effects")
  if(!is.null(model$vs_nuisance_rank)) {
    # The residual fit counts the fixed effects but not the projected controls.
    # Restore their rank in fixest's finite-sample covariance adjustment.
    k_base<-degrees_freedom(model,type="k",vcov=vc)
    k_full<-k_base+model$vs_nuisance_rank
    n<-nobs(model)
    vs_fail(n<=k_full,"No residual degrees of freedom after controls")
    se<-tab["veteran",2]*sqrt((n-k_base)/(n-k_full))
    df<-if(county)degrees_freedom(model,type="t",vcov=vc) else n-k_full
    beta<-tab["veteran",1]
    return(c(estimate=beta,se=se,p_value=2*pt(-abs(beta/se),df=df),ci_lower=beta-qt(.975,df=df)*se,ci_upper=beta+qt(.975,df=df)*se))
  }
  ci<-confint(model,parm="veteran",vcov=vc,level=.95)
  c(estimate=tab["veteran",1],se=tab["veteran",2],p_value=tab["veteran",4],ci_lower=ci[1,1],ci_upper=ci[1,2])
}

vs_definitions <- function() c(
  "WWII veteran status and scientist occupation: adjusted comparisons",
  "Universe: 1950 sample-line men, positive SLWT, VETWWII 1 or 2, BIRTHYR 1910-1927. Dataset retains all eligible rows.",
  "Outcome: inherited scientist flag, OCC1950 7,61,62,63,67,68,69. Treatment: VETWWII=2 versus 1.",
  "Paper cohorts: 1922-1927 and 1910-1921. Additional overlapping comparisons: 1921-1927 and 1910-1927.",
  "Weights: each respondent's 1950 SLWT only. No linkage-selection reweighting.",
  "Main regressions exclude current armed forces: OCC1950=595, EMPSTATD 13-15, or CLASSWKRD=26. Students, unemployed, and other men outside the labor force remain included.",
  "Complete cases require reciprocal 1940 link, valid county/background/birthplace and all required prewar controls. Younger men also require a male father record and valid father controls.",
  "Background: 1950 RACE crossed with 1940 reported father's origin, including Other/Unknown; all men remain eligible for pooled analysis irrespective of birthplace.",
  "US-born: BPL 1-56,90,99. Paper groups: US-W white with US-born father; B black with US-born father; NW/SE white with father in the corresponding European category. Each subgroup also requires US-born respondent.",
  "Father birthplace is 1940 full-count FBPL, not the paper's 1950 parental birthplace. NW=400-429,453; SE=430-440,450-452,454-457,459-465. Codes 458/499 and other ambiguous origins are outside NW/SE, retained in pooled analysis.",
  "Model 1: background x birth-year fixed effects. Model 2: additionally background x birthplace and background x 1940 county fixed effects. Model 3: additionally prewar controls, with slopes varying by background.",
  "Common controls: own completed schooling years, college completion (16+ years), school attendance, scientist status, urban/farm residence, and full-count household homeownership, all in 1940.",
  "Born 1910-1921: own log wage, zero wage, occupational income score, zero score, white-collar and nonfarm unskilled indicators. Born 1922-1927: father's schooling years, college completion and corresponding labor controls. Inactive age-specific terms are set to zero for modeling only; raw source missingness is retained.",
  "Completed years: max(0,floor(HIGRADED/10)-3), capped at 17. Codes 0/999/NULL are missing. College completion is HIGRADED>=190, unlike any college attendance (151+).",
  "Valid zero wages: log_wage=0 and zero_wage=1; positive wages use natural log. INCWAGE 999998/999999, negative or NULL are missing, not zero. OCC1950 979/997 are missing for occupation indicators; non-occupation and N/A codes remain non-white-collar/non-unskilled.",
  "All three models use the same complete-case rows. No automatic fixed-effect singleton removal. Strict sensitivity also requires 1940/1950 agreement in RACE, BPL and census year minus AGE.",
  "Full-control estimation uses weighted Frisch-Waugh-Lovell projection within each background, followed by a pooled residual regression. This is equivalent to explicitly interacting every prewar control with background; projected-control rank is restored in finite-sample SE and HC1 degrees-of-freedom adjustments.",
  "Primary inference: county-1940 clustered SE, two-sided 95% confidence interval, fixest default small-sample adjustments. HC1 heteroskedasticity-robust inference reported separately. Estimates, SE and intervals are also multiplied by 100 to give percentage points.",
  "Coefficient tables contain each overlapping cohort and subgroup, with no claim that these estimates are independent. Tests are unadjusted for multiple comparisons.",
  "Groups without outcome/treatment variation or fewer than two counties are marked unestimable. Fixed effects may also remove all identifying veteran variation; no estimate is fabricated.",
  "Interpretation: adjusted veteran-minus-non-veteran probability differences. Selection on unobservables remains possible; military service and GI Bill benefits are not separately identified. Weighted totals describe the extract, not a linkage-corrected national population."
)

main <- function() {
  input<-Sys.getenv("IPUMS_VETERAN_SCIENTIST_PARQUET_DIR","D:/ipums_linked_1950_veteran_scientist")
  output<-Sys.getenv("VETERAN_SCIENTIST_RESULTS_DIR",file.path(vs_data_root(),"results/mandatory_schooling/linked_1950_sample_line_veteran_scientist"))
  con<-dbConnect(duckdb());on.exit(dbDisconnect(con,shutdown=TRUE),add=TRUE)
  dbExecute(con,"SET threads=8")
  path<-dbQuoteString(con,vs_path(file.path(input,"data.parquet")))
  keep<-unique(c("respondent_id","birthyr","weight","veteran","scientist_1950","background","birthplace","county_1940","paper_group","young","strict_link","linked_1940","father_available","fullcount_available","active_military_1950","regression_eligible","exclusion_reason",vs_common,vs_old_controls,vs_young_controls))
  all<-dbGetQuery(con,paste0("SELECT ",paste(keep,collapse=",")," FROM read_parquet(",path,")"))
  vs_fail(anyDuplicated(all$respondent_id)>0 || any(!is.finite(all$weight)) || any(all$weight<=0),"Invalid IDs or weights")
  setFixest_nthreads(8)
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  writeLines(vs_definitions(),file.path(output,"model_definitions.txt"))
  coefficients<-list(); diagnostics<-list(); rates<-list(); selection<-list(); reasons<-list()
  scopes<-c("All men","US-W","B","NW","SE")
  specnames<-c("Birth year/background","Add birthplace/county","Full prewar controls")
  for(i in seq_len(nrow(vs_cohorts))) {
    cohort<-vs_cohorts$cohort[i]
    vs_say("Estimating cohort ",cohort)
    cohort_rows<-all$birthyr>=vs_cohorts$lo[i] & all$birthyr<=vs_cohorts$hi[i]
    for(scope in scopes) {
      raw<-all[cohort_rows & (scope=="All men" | all$paper_group==scope),,drop=FALSE]
      selection[[length(selection)+1L]]<-data.frame(cohort,group=scope,raw_n=nrow(raw),weighted_n=sum(raw$weight),linked_1940_n=sum(raw$linked_1940),father_available_n=sum(raw$father_available),fullcount_available_n=sum(raw$fullcount_available),active_military_n=sum(raw$active_military_1950),complete_case_n=sum(raw$regression_eligible),strict_complete_case_n=sum(raw$regression_eligible & raw$strict_link))
      for(status in 0:1) {
        rr<-raw[raw$veteran==status,]
        reason_table<-table(unlist(strsplit(rr$exclusion_reason[nzchar(rr$exclusion_reason)],";",fixed=TRUE)))
        if(length(reason_table)) reasons[[length(reasons)+1L]]<-data.frame(cohort,group=scope,veteran=status,reason=names(reason_table),n=as.integer(reason_table))
      }
      for(sensitivity in c("Main","Strict links")) {
        d<-raw[raw$regression_eligible & (sensitivity=="Main" | raw$strict_link),,drop=FALSE]
        d<-vs_model_data(d)
        for(sample_name in c("Full eligible sample","Regression sample")) for(status in 0:1) {
          rr<-if(sample_name=="Full eligible sample") raw[raw$veteran==status,] else d[d$veteran==status,]
          if(sample_name=="Full eligible sample" && sensitivity!="Main") next
          rates[[length(rates)+1L]]<-data.frame(cohort,group=scope,sensitivity,sample=sample_name,veteran=status,sample_n=nrow(rr),weighted_n=sum(rr$weight),scientist_n=sum(rr$scientist_1950),scientist_weighted=sum(rr$weight*rr$scientist_1950),scientist_share_pct=if(nrow(rr))100*sum(rr$weight*rr$scientist_1950)/sum(rr$weight) else NA_real_)
        }
        issue<-if(!nrow(d)) "No complete cases" else if(length(unique(d$veteran))<2) "No veteran variation" else if(length(unique(d$scientist_1950))<2) "No outcome variation" else if(length(unique(d$county_1940))<2) "Fewer than two counties" else ""
        for(spec in 1:3) {
          warning_text<-character()
          fitted<-if(nzchar(issue)) NULL else tryCatch(withCallingHandlers(vs_fit(d,spec),warning=function(w){warning_text<<-c(warning_text,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)e)
          error<-issue
          if(inherits(fitted,"error")) error<-conditionMessage(fitted)
          if(!is.null(fitted) && !inherits(fitted,"error") && !"veteran" %in% names(coef(fitted))) error<-"Veteran coefficient unidentified after fixed effects"
          if(!is.null(fitted) && !inherits(fitted,"error") && identical(fitted$convStatus,FALSE)) error<-"Fixed effects did not converge"
          estimates<-if(nzchar(error)) NULL else tryCatch(list(cluster=vs_extract(fitted,TRUE),hc1=vs_extract(fitted,FALSE)),error=function(e)e)
          if(inherits(estimates,"error")) {error<-conditionMessage(estimates);estimates<-NULL}
          base<-data.frame(cohort,group=scope,sensitivity,specification=spec,model=specnames[spec],status=if(nzchar(error))error else "Estimated",sample_n=nrow(d),weighted_n=sum(d$weight),scientist_n=sum(d$scientist_1950),counties=length(unique(d$county_1940)))
          for(inference in c("cluster","hc1")) {
            vals<-if(is.null(estimates)) setNames(rep(NA_real_,5),c("estimate","se","p_value","ci_lower","ci_upper")) else estimates[[inference]]
            row<-cbind(base,inference=inference,as.data.frame(as.list(vals)))
            for(n in c("estimate","se","ci_lower","ci_upper")) row[[paste0(n,"_pp")]]<-100*row[[n]]
            coefficients[[length(coefficients)+1L]]<-row
          }
          diagnostics[[length(diagnostics)+1L]]<-data.frame(cohort,group=scope,sensitivity,specification=spec,status=base$status,warnings=paste(unique(warning_text),collapse="; "),formula=if(is.null(fitted)||inherits(fitted,"error"))"" else paste(deparse(if(!is.null(fitted$vs_full_formula)) fitted$vs_full_formula else formula(fitted)),collapse=" "))
          rm(fitted,estimates);gc(FALSE)
        }
      }
    }
    write.csv(do.call(rbind,coefficients),file.path(output,"coefficients.csv"),row.names=FALSE)
    write.csv(do.call(rbind,diagnostics),file.path(output,"model_diagnostics.csv"),row.names=FALSE)
  }
  coefs<-do.call(rbind,coefficients)
  write.csv(do.call(rbind,rates),file.path(output,"scientist_rates.csv"),row.names=FALSE)
  write.csv(do.call(rbind,selection),file.path(output,"sample_selection.csv"),row.names=FALSE)
  write.csv(if(length(reasons))do.call(rbind,reasons) else data.frame(),file.path(output,"exclusion_reasons.csv"),row.names=FALSE)
  plotdata<-coefs[coefs$sensitivity=="Main" & coefs$specification==3 & coefs$inference=="cluster" & coefs$status=="Estimated",]
  plotdata$group<-factor(plotdata$group,levels=rev(scopes))
  plotdata$cohort<-factor(plotdata$cohort,levels=vs_cohorts$cohort)
  p<-ggplot(plotdata,aes(x=estimate_pp,y=group))+geom_vline(xintercept=0,color="grey65",linetype=2)+
    geom_segment(aes(x=ci_lower_pp,xend=ci_upper_pp,yend=group),color="#24618C",linewidth=.7)+geom_point(color="#24618C",size=2.4)+
    facet_wrap(~cohort,ncol=2)+labs(x="Adjusted veteran difference (percentage points; 95% CI)",y=NULL,title="WWII veteran status and scientist occupation in 1950",subtitle="Full prewar controls; 1950 sample-line weights; county-clustered inference")+
    theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())
  ggsave(file.path(output,"coefficients_full.png"),p,width=10,height=6.5,dpi=180)
  ggsave(file.path(output,"coefficients_full.pdf"),p,width=10,height=6.5)
  jsonlite::write_json(list(completed_utc=format(Sys.time(),tz="UTC",usetz=TRUE),input=input,rows=nrow(all),weight="1950 SLWT",models=nrow(coefs)/2,estimated=sum(coefs$status=="Estimated")/2,interpretation="Adjusted associations; not separate GI Bill effects",R_version=R.version.string,fixest_version=as.character(packageVersion("fixest"))),file.path(output,"_manifest.json"),pretty=TRUE,auto_unbox=TRUE)
  print(coefs[coefs$group=="All men" & coefs$sensitivity=="Main" & coefs$specification==3 & coefs$inference=="cluster",c("cohort","sample_n","estimate_pp","ci_lower_pp","ci_upper_pp","p_value","status")],row.names=FALSE)
  vs_say("Results saved to ",output)
}
if(sys.nframe()==0L) main()
