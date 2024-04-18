#' Fucntion for Plotting PSC objects
#' @param x an object of class 'psc'
#' @return a plot corresponding to the psc fit
#' @examples
#' psc.ob <- psc(model,data)
#' plot(psc.ob)

plot.psc <- function(x){
  med <- coef(x)[1];med
  s_fpm <- surv.fpm(x)
  s_fpm_2 <- surv.fpm(x,beta=med)

  plot(s_fpm$time,s_fpm$S,typ="l")
  lines(s_fpm_2$time,s_fpm_2$S,typ="l")
}





