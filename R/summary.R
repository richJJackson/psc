#' Personalised Synthetic Controls - summary
#'
#' @param x an object of class 'psc'
#' @return psc summary results'
#'
#' @export
summary.psc <- function(x,...){

  cat("Summary: \n \n")
  summ <-pscSumm(x$DC_clean)

  cat("\n")
  print(x)

}


