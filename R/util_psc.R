# util functions.

#### Functions
modp <- function(x){
  x*(sign(x)+1)/2
}


acc <-  function(old,new){
  ret <- FALSE
  r <- runif(1)
  e <-exp(old-new)
  try(if(e>r) ret <- TRUE)
  ret
}


surv_fpm <- function(x,beta=0){

  me <- x$DC_clean$model_extract
  dc <- x$DC_clean
  time <- dc$outcome$time
  logt <- log(time+1e-06)

  lam <- me$lam
  kn <- me$kn
  cov_co <- me$cov.co
  haz_co <- me$haz_co
  k <- me$k

  linPred <- lp_psc(x)
  adjLP <- mean(linPred) + beta

  z <- NULL
  ### basis functions
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)
  }

  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
  H<- H0*exp(adjLP)
  S <- exp(-H)

  ord <- order(time)
  ret <- list("time"=time[ord],"S"=S[ord])
  ret

}




lp_psc <- function(x){
  me <- x$DC_clean$model_extract;me
  dc <- x$DC_clean;dc
  lp <- dc$cov[,1:length(me$cov_co)]%*%me$cov_co
  c(lp)
}


