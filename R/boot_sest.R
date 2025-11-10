#' Counter Factual Model - summary
#'
#' A function to estimate the survival function based on parameter estimates -
#' used in ootstrapping CFM for CIs
#'
#' @param object an object of class 'psc'
#' @return A summary of a cfm object
boot_sest <- function(i,pscOb=pscOb,lam=lam,kn=kn,k=k,cov=cov,tm=tm,rest=rest,beta=beta){
  hc <- rest[i,1:length(pscOb$haz_co)]
  cc <- rest[i,-c(1:length(pscOb$haz_co))]
  se <- spline_surv_est(lam=lam,kn=kn,k=k,haz_co=hc,
                        cov_co=cc,cov=cov,tm=tm,beta=beta)
  se$S
}
