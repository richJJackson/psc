#' Estimates the linear predictor of a psc object
#'
#' @param CFM a model object of class 'glm' or 'flexsurvreg'
#' @param DC a cohort of data to match the CFM
#' @param resp detailing whether the linear predictor shoudl be returned on the natural or response level.  Defaults to the natural scale (resp=F)
#' @details A function which combines the data from the data cohort against the model parameters of the PSC
#' @export
linPred <- function(CFM,DC,resp=F,...){
  DC_clean <- dataComb(CFM, DC,)
  mt <- class(CFM)
  cov <- DC_clean$cov;cov
  cov_co <- DC_clean$model_extract$cov_co;cov_co
  lp <- cov %*% cov_co
  ret <- lp

  if(resp){
    if("glm"%in%mt){
      fam <- enrich(x$DC_clean$model_extract$family)
      ret <- fam$linkinv(lp)
    }

    if("flexsurvreg"%in%mt){
      mnlp <- mean(lp);mnlp
      ret <- surv_fpm(x,s=0.5)^exp(lp-mnlp);ret
    }

    return(ret)

  }
