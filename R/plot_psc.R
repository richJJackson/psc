## Fucntion for Plotting PSC objects
plot.psc <- function(x){

  med <- coef(x)[1]
  s_fpm <- surv.fpm(x)
  s_fpm_2 <- surv.fpm(x,beta=med)

  plot(s_fpm$time,s_fpm$S,typ="l")
  lines(s_fpm_2$time,s_fpm_2$S,typ="l")

}





