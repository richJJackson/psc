#' Personalised Synthetic Controls - summary
#'
#' @param x an object conting bothe the model and dataset created using 'dataComb()'
#' @return psc summary results including an estimate of the linear predictor
#' combing the data and the model, an estimate of patient level response and
#' summary statistics of the average responses for the sythenthic and observed
#' populations
#'
#' @export
pscSumm <- function(DC_clean){

  lp <- linPred(DC_clean,resp=F)
  resp <- linPred(DC_clean,resp=T)


  if("glm" %in% DC_clean$model.type){
    exp_resp <- mean(resp)
    ob_resp <- mean(unlist(DC_clean$out))
  }


  if("flexsurvreg" %in% DC_clean$model.type){
    s.ob <- Surv(DC_clean$out$time,DC_clean$out$cen)
    sfit <- survfit(s.ob~1)
    sfit.tab <- summary(sfit)$table

    exp_resp <- median(resp)
    ob_resp <- sfit.tab[7]
  }

  cat(paste(nrow(DC_clean$cov),"observations selected from the data cohort for comparison"),"\n")
  cat("CFM of type",DC_clean$model.type,"identified"," \n")
  cat(paste("linear predictor succesfully obtained with a median of ",round(median(lp),3)),"\n")
  cat(paste("Average expected response:",round(exp_resp,3)),"\n")
  cat(paste("Average observed response:",round(ob_resp,3)),"\n")

  ret <- list("linpred"=lp,"E(patResp)"=resp,"expResp"=exp_resp,"obResp"=ob_resp)
  return(ret)
}
