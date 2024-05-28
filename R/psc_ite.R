#' Wrapper function for individual treatment effects
#'
#' @param model a CFM
#' @param data a data cohort
#' @details
#'  A wrapper function that estimates individual treatment effects usinf pscfit
#' @export
psc_ite <- function(model,data){

  nr <- nrow(data)
  res <- NULL

  for(i in 1:nr){
    fit_i <- pscfit(model,data,id=i)
    co_i <- coef(fit_i)
    res <- rbind(res,co_i)
    }
  res
}




