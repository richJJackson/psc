#' Returns the coefficient estimate of a psc object.
#'
#' Returns basic measures of the posterior distribution obtained from the
#' psc object
#'
#' @param object a 'psc' object
#' @param ... not used
#' @return The summary of the posterior distribution for the efficacy parameter
#' in terms of the median and 95% HPD
#' @examples
#' e4_data <- psc::e4_data
#' gemCFM <- psc::gemCFM
#' psc <- pscfit(gemCFM,e4_data)
#' coef(psc)
#' @export
coef.psc <- function(pscOb, ...){
  co <- as.matrix(data.frame(pscOb$postEst));co
  rownames(co) <- rep("posterior",nrow(co))
  print.default(format(co,digits = max(3L, getOption("digits") - 3L)), print.gap = 2L,
                quote = FALSE)
}
