#' Counter Factual Model - summary
#'
#' A generic function to provide a summary of a Counter factual model
#'
#' @param object an object of class 'psc'
#' @return A summary of a psc object obtained using pscSumm and a copy of the pscfit object
#' @examples
#' e4_data <- psc::e4_data
#' gemCFM <- psc::gemCFM
#' psc <- pscfit(gemCFM,e4_data)
#' cfmSumm.flexsurvreg(psc)
#' @export
cfmSumm <- function(pscOb){

  ### Glm summary
  if("glm" %in% pscOb$mod_class){
    cfm_summ <- cfmSumm.glm(pscOb,resp=T,bootCI = T,nboot=1000)
    mn.ob <- mean(pscOb$DC$Y)
  }

  ### Survival Summary
  if("flexsurvreg" %in% pscOb$mod_class){
    cfm_summ <- cfmSumm.flexsurvreg(pscOb,bootCI = T,nboot=1000)
  }

  ### Summary Text
  cat(paste(nrow(pscOb$DC$X),"observations selected from the data cohort for comparison"),"\n\n")

  cat("Average expected response: \n ")
  print.default(format(cfm_summ$summ, digits = max(3L, getOption("digits") - 3L)), print.gap = 2L,
                quote = FALSE)

  cat("\n Average observed response: \n")
  print.default(format(mn.ob, digits = max(3L, getOption("digits") - 3L)), print.gap = 2L,
                quote = FALSE)

  ret <- list("linpred"=cfm_summ$lp,"expResp"=cfm_summ$summ,"obResp"=mn.ob)
  return(ret)
}
