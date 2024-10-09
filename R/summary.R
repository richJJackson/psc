#' Personalised Synthetic Controls - summary
#'
#' @param object an object of class 'psc'
#' @param ... not used
#' @return psc summary results'
#'
#' @export
summary.psc <- function(object,...){

  cat("Summary: \n \n")
  summ <-pscSumm(object$DC_clean)

  cat("\n")
  print(object)

}


