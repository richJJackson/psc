#' Personalised Synthetic Controls - summary
#'
#' A generic function to provide a summary of a 'psc' object obtained from
#' pscfit.R
#'
#' @param object an object of class 'psc'
#' @param ... not used
#' @return A summary of a psc object obtained using pscSumm and a copy of the pscfit object
#' @export
#'
summary.psc <- function(pscOb,...){

  #### CFM details
  frm <- pscOb$formula;frm
  fam <- pscOb$family;fam
  mt <- pscOb$mod_class;mt

  ### Summary of the cfm
  if("glm"%in%pscOb$mod_class){
    cfm.summ <-cfmSumm.glm(pscOb)
  }

  if("flexsurvreg"%in%pscOb$mod_class){
    options(warn=-1) ## removing warning for unobtained med surv
    cfm.summ <- cfmSumm.flexsurvreg(pscOb)
    cfms.id <- lapply(cfm.summ[,c(2,4,5)],function(x) cfm.summ$time[min(which(x<0.5))])
    cfm.summ <-list(cfm.summ,"summ"=unlist(cfms.id))
    options(warn=1)
  }

  ### Outcome summary
  if("glm"%in%pscOb$mod_class){
    out.summ <- mean(pscOb$DC$Y)
    if(!is.null(pscOb$DC$trt)){
      out.summ <- tapply(pscOb$DC$Y,pscOb$DC$trt,mean)
    }
  }

  if("flexsurvreg"%in%pscOb$mod_class){
    sfit <- survfit(Surv(pscOb$DC$Y$time,pscOb$DC$Y$cen)~1)
    out.summ <- summary(sfit)$table[7:9];out.summ
    if(!is.null(pscOb$DC$trt)){
      sfit <- survfit(Surv(pscOb$DC$Y$time,pscOb$DC$Y$cen)~pscOb$DC$trt)
      out.summ <- summary(sfit)$table[,7:9]
    }
  }


  ### Model
  cat("Counterfactual Model (CFM): \n")

  if("glm"%in%mt){
    cat(
      paste("A model of class 'GLM'"," \n",sep=""),
      paste("Family: ",fam$family," \n",sep=""),
      paste("Link: ",fam$link," \n",sep=""))
  }

  if("flexsurvreg"%in%mt){
    cat(
      paste("A model of class 'flexsurvreg'"," \n",sep=""),
      paste("Fit with ",pscOb$k," internal knots","\n",sep=""))
  }

  cat("\n")
  cat("CFM Formula: \n")
  print(frm)
  cat("\n")

  ### CFM summary
  cat("CFM Summary: \n")
  cat("Expected response for the outcome under the CFM:")
  cat("\n")

  print.default(format(cfm.summ$summ,digits = max(3L, getOption("digits") - 3L)), print.gap = 2L,
                quote = FALSE)
  cat("\n")
  cat("Observed outcome from the Data Cohort:")
  cat("\n")
  print.default(format(as.matrix(out.summ),digits = max(3L, getOption("digits") - 3L)), print.gap = 2L,
                quote = FALSE)
  cat("\n")

  ### Fit summary
  cat("MCMC Fit: \n")
  cat("Posterior Distribution obtaine with fit summary:")
  cat("\n")
  pf <- as.matrix(pscOb$postFit)
  print.default(format(pf,digits = max(3L, getOption("digits") - 3L)), print.gap = 2L,
                quote = FALSE)
  cat("\n")

  ### Fit summary
  cat("Summary: \n")
  cat("Posterior Distribution for beta:")

  print(pscOb)

}
