#' Wrapper for sub-group analysis
#' @param CFM CFM
#' @param DC DC
#' @param sg sg
#' @param cont_lev cont_lev
#' @param cont_cut cont_cut
#'
#' @export
pscsubWrap <- function(pscsub){
  tb <- pscsub$Table
  n <- rowSums(tb)
  e <- tb[,2]
  co <- lapply(pscsub$fit,coef.psc)
  co <- t(matrix(unlist(co),ncol=nrow(tb)))

  hr <- paste(round(exp(co[,1]),2)," (",round(exp(co[,2]),2),", ",round(exp(co[,3]),2),")",sep="")

  ret <- as.matrix(cbind(n,e,co,hr))
  colnames(ret) <- c("n","e","median","lower","upper","P(x<0)","hr")
  return(data.frame(ret))
}
