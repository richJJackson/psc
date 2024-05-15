#' Returns the coefficeint estimate of a psc object.
#'
#' @param x a 'psc' object
#' @return The summary of the posterior distribution for the efficacy parameter in terms of the median and 95% HPD
#' @export
### Utility functions for psc
coef.psc <- function(x, ...){
  y <- x$posterior$beta;y
  qu <- quantile(y,p=c(0.025,0.5,00.975))
  p <- sum(y<0)/length(y)

  res <- as.numeric(c(qu[2],qu[1],qu[3],p))
  names(res) <- c("median","lower","upper","Pr(x<0)")
  res
}
