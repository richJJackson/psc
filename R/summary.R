#' Personalised Synthetic Controls - summary
#'
#' @param x an object of class 'psc'
#' @return psc summary results'
#'
#' @export
summary.psc <- function(x){
  coef(x)
}

