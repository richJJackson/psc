#' Returns the coefficeint estimate of a psc object.
#'
#' @param x a 'psc' object
#' @return The summary of the posterior distribution for the efficacy parameter in terms of the median and 95% HPD
#' @export
coef.psc <- function(x, ...){

  cl.nm <- attributes(x$DC_clean$model_extract$sig)$dimnames[[1]]
  y <- x$posterior[,which(!names(x$posterior)%in%cl.nm)]

  lap <- lapply(y,quantile,na.rm=T,p=c(0.025,0.5,00.975))
  lap <- t(matrix(unlist(lap),3,ncol(y)))
  p <- colSums(y<0)/nrow(y)

  res <- data.frame(cbind(lap,p))
  names(res) <- c("median","lower","upper","Pr(x<0)")
  as.matrix(res)
}

